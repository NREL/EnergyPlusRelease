MODULE CrossVentMgr

! MODULE INFORMATION:
  !       AUTHOR         G. Carrilho da Graca
  !       DATE WRITTEN   October 2004
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Routines that implement the UCSD Cross Ventilation

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
USE DataHVACGlobals, ONLY: SysTimeElapsed
USE DataUCSDSharedData
USE DataAirflowNetwork
USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:

  ! DERIVED TYPE DEFINITIONS:
  ! na

 ! MODULE VARIABLE DECLARATIONS:
  REAL(r64)            :: HAT_J = 0.0d0     ! HAT_J Convection Coefficient times Area times Temperature for Jet subzone
  REAL(r64)            :: HA_J  = 0.0d0     ! HA_J  Convection Coefficient times Area for Jet subzone
  REAL(r64)            :: HAT_R = 0.0d0     ! HAT_R Convection Coefficient times Area times Temperature for Recirculation subzone
  REAL(r64)            :: HA_R  = 0.0d0     ! HA_J  Convection Coefficient times Area for Recirculation subzone
  REAL(r64),PARAMETER  :: Cjet1 = 1.6173d0  ! First correlation constant for the jet velocity
  REAL(r64),PARAMETER  :: Cjet2 = 0.1466d0  ! Second correlation constant for the jet velocity
  REAL(r64),PARAMETER  :: Crec1 = 0.8911d0  ! First correlation constant for the recirculation velocity
  REAL(r64),PARAMETER  :: Crec2 = 0.0393d0  ! Second correlation constant for the recirculation velocity
  REAL(r64),PARAMETER  :: CjetTemp = 0.8254d0  ! Correlation constant for the jet temperature rise
  REAL(r64),PARAMETER  :: CrecTemp = 1.2734d0  ! Correlation constant for the recirculation temperature rise
  REAL(r64),PARAMETER  :: CrecFlow1 = 0.4444d0  ! First correlation constant for the recirculation flow rate
  REAL(r64),PARAMETER  :: CrecFlow2 = 0.1751d0  ! Second correlation constant for the recirculation flow rate




  ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageUCSDCVModel
PRIVATE CalcUCSDCV
PRIVATE InitUCSDCV
PRIVATE HcUCSDCV
PRIVATE EvolveParaUCSDCV

CONTAINS

SUBROUTINE ManageUCSDCVModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   October 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   manage the UCSD Cross Ventilation model

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

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW:

    ! initialize Cross Ventilation model

    CALL InitUCSDCV(ZoneNum)

    ! perform Cross Ventilation model calculations
    CALL CalcUCSDCV(ZoneNum)

    RETURN

END SUBROUTINE ManageUCSDCVModel

!**************************************************************************************************

SUBROUTINE InitUCSDCV(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   October 2004
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
  USE DataRoomAirModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                     :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumOfZones))
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  ! Do the begin environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(ZoneNum)) THEN
    MyEnvrnFlag(ZoneNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ZoneNum) = .true.
  ENDIF

  RETURN

END SUBROUTINE InitUCSDCV

!**************************************************************************************************

SUBROUTINE HcUCSDCV(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   October 2004
          !       MODIFIED       8/2013 - Sam Brunswick
          !                      To improve convection coefficient calculation
          !       RE-ENGINEERED  -

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main subroutine for convection calculation in the UCSD Cross Ventilation model.
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
  USE ScheduleManager,                       ONLY: GetScheduleIndex  !, GetDayScheduleValues
  USE DataGlobals,                           ONLY:BeginEnvrnFlag


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN)                     :: ZoneNum             !

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                 :: Ctd                 ! DO loop counter for surfaces
  INTEGER                                 :: SurfNum             ! Surface number
  REAL(r64)                               :: Hjet
  REAL(r64)                               :: Hrec

  ! Initialize HAT and HA
  HAT_J    = 0.0d0
  HAT_R    = 0.0d0
  HA_J     = 0.0d0
  HA_R     = 0.0d0

  ! Is the air flow model for this zone set to UCSDCV Cross Ventilation?
  IF(IsZoneCV(ZoneNum)) THEN
    ! WALL Hc, HA and HAT calculation
    DO Ctd = PosZ_Wall((ZoneNum-1)*2 + 1),PosZ_Wall((ZoneNum-1)*2 + 2)
      SurfNum = APos_Wall(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
      HWall(Ctd)= CVHcIn(SurfNum)
      HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWall(Ctd) + HAT_R
      HA_R  = Surface(SurfNum)%Area*HWall(Ctd) + HA_R
    END DO  ! END WALL
    ! WINDOW Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Window((ZoneNum-1)*2 + 1),PosZ_Window((ZoneNum-1)*2 + 2)
      SurfNum = APos_Window(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      IF (Surface(SurfNum)%Tilt > 10.0d0 .AND. Surface(SurfNum)%Tilt < 170.0d0) THEN ! Window Wall
        TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
        HWindow(Ctd)= CVHcIn(SurfNum)
        HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_R
        HA_R = Surface(SurfNum)%Area*HWindow(Ctd) + HA_R
      ENDIF
      IF (Surface(SurfNum)%Tilt <= 10.0d0) THEN ! Window Ceiling
        TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Ujet)
        Hjet= CVHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
        Hrec= CVHcIn(SurfNum)
        HWindow(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
        HAT_R = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
        HA_R  = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
        HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
        HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
        TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)
      ENDIF
      IF (Surface(SurfNum)%Tilt >= 170.0d0) THEN ! Window Floor
        TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Ujet)
        Hjet= CVHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
        Hrec= CVHcIn(SurfNum)
        HWindow(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
        HAT_R = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
        HA_R  = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
        HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
        HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
        TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)
      ENDIF
      CVHcIn(SurfNum) = HWindow(Ctd)
    END DO ! END WINDOW
    ! DOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Door((ZoneNum-1)*2 + 1),PosZ_Door((ZoneNum-1)*2 + 2) ! DOOR
      SurfNum = APos_Door(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
      HDoor(Ctd)= CVHcIn(SurfNum)
      HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HDoor(Ctd) + HAT_R
      HA_R = Surface(SurfNum)%Area*HDoor(Ctd) + HA_R
    END DO ! END DOOR
    ! INTERNAL Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Internal((ZoneNum-1)*2 + 1),PosZ_Internal((ZoneNum-1)*2 + 2)
      SurfNum = APos_Internal(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
      HInternal(Ctd) = CVHcIn(SurfNum)
      HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HInternal(Ctd) + HAT_R
      HA_R  = Surface(SurfNum)%Area* HInternal(Ctd) + HA_R
    END DO  ! END INTERNAL

    ! CEILING Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Ceiling((ZoneNum-1)*2 + 1),PosZ_Ceiling((ZoneNum-1)*2 + 2)
      SurfNum = APos_Ceiling(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Ujet)
      Hjet= CVHcIn(SurfNum)
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
      Hrec= CVHcIn(SurfNum)
      HCeiling(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
      HAT_R = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
      HA_R  = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
      HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
      HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
      TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)
      CVHcIn(SurfNum) = HCeiling(Ctd)
    END DO  ! END CEILING
    ! FLOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Floor((ZoneNum-1)*2 + 1),PosZ_Floor((ZoneNum-1)*2 + 2)
      SurfNum = APos_Floor(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Ujet)
      Hjet= CVHcIn(SurfNum)
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Urec)
      Hrec= CVHcIn(SurfNum)
      HFloor(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
      HAT_R = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
      HA_R  = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
      HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
      HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
      TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)
      CVHcIn(SurfNum) = HFloor(Ctd)
    END DO  ! END FLOOR
  ENDIF

END SUBROUTINE HcUCSDCV

!**************************************************************************************************

SUBROUTINE EvolveParaUCSDCV(ZoneNum)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         G. Carrilho da Graca
        !       DATE WRITTEN   October 2004
        !       MODIFIED       8/2013 - Sam Brunswick
        !                      To incorporate an improved model
        !                      and add modeling of multiple jets
        !       RE-ENGINEERED  -

        ! PURPOSE OF THIS SUBROUTINE:
        ! Subroutine for parameter actualization in the UCSD Cross Ventilation model.
        !

        ! METHODOLOGY EMPLOYED:
        ! -
        ! -
        ! -
        ! -

        ! REFERENCES:
        ! -
        ! -

        ! USE STATEMENTS:
USE Psychrometrics
USE DataHeatBalFanSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN)                     :: ZoneNum             !

        ! SUBROUTINE PARAMETER DEFINITIONS:

REAL(r64),PARAMETER  :: MinUin = 0.2d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER           :: Ctd           ! counter
INTEGER           :: Ctd2          ! counter
INTEGER           :: OPtr          ! counter
REAL(r64), SAVE   :: Win           ! Inflow aperture width
REAL(r64), SAVE   :: Aroom         ! Room area cross section
REAL(r64), SAVE   :: Wroom         ! Room width
REAL(r64)         :: Uin           ! Inflow air velocity [m/s]
REAL(r64)         :: CosPhi        ! Angle (in degrees) between the wind and the outward normal of the dominant surface
REAL(r64)         :: SurfNorm      ! Outward normal of surface
REAL(r64)         :: SumToZone     ! Sum of velocities through
REAL(r64)         :: MaxFlux
INTEGER           :: MaxSurf
REAL(r64)         :: XX
REAL(r64)         :: YY
REAL(r64)         :: ZZ
REAL(r64)         :: XX_Wall
REAL(r64)         :: YY_Wall
REAL(r64)         :: ZZ_Wall
REAL(r64)         :: ActiveSurfNum
INTEGER           :: NSides                ! Number of sides in surface
INTEGER           :: CompNum       = 0     ! AirflowNetwork Component number
INTEGER           :: TypeNum       = 0     ! Airflownetwork Type Number within a component
INTEGER           :: NodeNum1      = 0     ! The first node number in an AirflowNetwork linkage data
INTEGER           :: NodeNum2      = 0     ! The Second node number in an AirflowNetwork linkage data


maxsurf=0
sumtozone=0.0d0
maxflux=0.0d0
RecInflowRatio(ZoneNum)=0.0d0

! Identify the dominant aperture:
MaxSurf=AirflowNetworkSurfaceUCSDCV(ZoneNum,1)
IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone == ZoneNum) THEN
  ! this is a direct airflow network aperture
  SumToZone=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,1))%VolFlow2
  MaxFlux=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,1))%VolFlow2
ELSE
  ! this is an indirect airflow network aperture
  SumToZone=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,1))%VolFlow
  MaxFlux=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,1))%VolFlow
END IF

DO Ctd2=2,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
  IF (Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,ctd2))%SurfNum)%Zone== ZoneNum) THEN
    IF (AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2))%VolFlow2 > MaxFlux) THEN
      MaxFlux=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2))%VolFlow2
      MaxSurf=AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2)
    END IF
    SumToZone=SumToZone+AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2))%VolFlow2
  ELSE
    IF (AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2))%VolFlow > MaxFlux) THEN
      MaxFlux=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2))%VolFlow
      MaxSurf=AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2)
    END IF
    SumToZone=SumToZone+AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd2))%VolFlow
  END IF
END DO

! Check if wind direction is within +/- 90 degrees of the outward normal of the dominant surface
SurfNorm=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Azimuth
CosPhi=COS((WindDir-SurfNorm)*DegToRadians)
IF (CosPhi <= 0) THEN
  AirModel(ZoneNum)%SimAirModel= .FALSE.
  CVJetRecFlows(ZoneNum,:)%Ujet=0.0d0
  CVJetRecFlows(ZoneNum,:)%Urec=0.0d0
  Urec(ZoneNum)=0.0d0
  Ujet(ZoneNum)=0.0d0
  Qrec(ZoneNum)=0.0d0
  IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond>0) THEN
    Tin(ZoneNum)=MAT(surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond)%zone)
  ELSEIF(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == Ground) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or. &
    Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
    OPtr=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OSCPtr
    OSC(OPtr)%OSCTempCalc = ( OSC(OPtr)%ZoneAirTempCoef*MAT(ZoneNum)      &
            +OSC(OPtr)%ExtDryBulbCoef*Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp &
            +OSC(OPtr)%ConstTempCoef*OSC(OPtr)%ConstTemp &
            +OSC(OPtr)%GroundTempCoef*GroundTemp         &
            +OSC(OPtr)%WindSpeedCoef                     &
            *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%WindSpeed &
            *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp )
    Tin(ZoneNum)  = OSC(OPtr)%OSCTempCalc
  ELSE
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  END IF
  RETURN
END IF

! Calculate the opening area for all apertures
DO Ctd=1,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
  CompNum = AirflowNetworkLinkageData(Ctd)%CompNum
  TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
  IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_DOP) THEN
     CVJetRecFlows(ZoneNum,Ctd)%Area=SurfParametersCVDV(Ctd)%Width*  &
         SurfParametersCVDV(Ctd)%Height*MultizoneSurfaceData(Ctd)%OpenFactor
  ELSE IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) THEN
    CVJetRecFlows(ZoneNum,Ctd)%Area=SurfParametersCVDV(Ctd)%Width*SurfParametersCVDV(Ctd)%Height
  ELSE
    CALL ShowSevereError('RoomAirModelCrossVent:EvolveParaUCSDCV: Illegal leakage component referenced '//  &
     'in the cross ventilation room air model')
    CALL ShowContinueError('Surface '//TRIM(AirflowNetworkLinkageData(Ctd)%Name)//' in zone '//TRIM(Zone(ZoneNum)%Name) &
                         //' uses leakage component '//TRIM(AirflowNetworkLinkageData(Ctd)%CompName))
    CALL ShowContinueError('Only leakage component types AirflowNetwork:MultiZone:Component:DetailedOpening and ')
    CALL ShowContinueError('AirflowNetwork:MultiZone:Surface:Crack can be used with the cross ventilation room air model')
    CALL ShowFatalError('Previous severe error causes program termination')
  END IF
END DO

! Calculate Droom, Wroom, Dstar
! Droom the distance between the average point of the base surface of the airflow network Surface (if the base surface
! is a Window or Door it looks for the second base surface).
! Dstar is Droom corrected for wind angle
Wroom=Zone(ZoneNum)%Volume/Zone(ZoneNum)%FloorArea
IF ((Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Sides == 3) .OR.  &
    (Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Sides == 4)) THEN
  XX=Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Centroid%X
  YY=Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Centroid%Y
  ZZ=Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Centroid%Z
ELSE
! If the surface has more than 4 vertex then average the vertex coordinates in X, Y and Z.
  NSides=Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Sides
  XX=SUM(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Vertex(1:NSides)%X) /  REAL(NSides,r64)
  YY=SUM(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Vertex(1:NSides)%Y) /  REAL(NSides,r64)
  ZZ=SUM(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%BaseSurf)%Vertex(1:NSides)%Z) /  REAL(NSides,r64)
END IF

DO Ctd=PosZ_Wall(2*ZoneNum-1),PosZ_Wall(2*ZoneNum)
  IF ((Surface(Apos_Wall(Ctd))%Sides==3) .OR.  (Surface(Apos_Wall(Ctd))%Sides==4)) THEN
    XX_Wall=Surface(Apos_Wall(Ctd))%Centroid%X
    YY_Wall=Surface(Apos_Wall(Ctd))%Centroid%Y
    ZZ_Wall=Surface(Apos_Wall(Ctd))%Centroid%Z
  ELSE
    NSides=Surface(Apos_Wall(Ctd))%Sides
    XX_Wall=SUM(Surface(Apos_Wall(Ctd))%Vertex(1:NSides)%X)/REAL(NSides,r64)
    YY_Wall=SUM(Surface(Apos_Wall(Ctd))%Vertex(1:NSides)%Y)/REAL(NSides,r64)
    ZZ_Wall=SUM(Surface(Apos_Wall(Ctd))%Vertex(1:NSides)%Z)/REAL(NSides,r64)
  END IF
  IF (SQRT((XX-XX_Wall)**2+(YY-YY_Wall)**2+(ZZ-ZZ_Wall)**2)>Droom(ZoneNum)) THEN
    Droom(ZoneNum)=SQRT((XX-XX_Wall)**2+(YY-YY_Wall)**2+(ZZ-ZZ_Wall)**2)
  END IF
  Dstar(ZoneNum)=MIN(Droom(ZoneNum)/CosPhi,SQRT(Wroom**2+Droom(ZoneNum)**2))
END DO


! Room area
Aroom=Zone(ZoneNum)%Volume/Droom(ZoneNum)

!Populate an array of inflow volume fluxes (Fin) for all apertures in the zone
!Calculate inflow velocity (%Uin) for each aperture in the zone
DO Ctd=1,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
  IF (Surface(MultizoneSurfaceData(Ctd)%SurfNum)%Zone == ZoneNum) THEN
  ! this is a direct airflow network aperture
    CVJetRecFlows(ZoneNum,Ctd)%Fin=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd))%VolFlow2
  ELSE
  ! this is an indirect airflow network aperture
    CVJetRecFlows(ZoneNum,Ctd)%Fin=AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Ctd))%VolFlow
  END IF
  IF (CVJetRecFlows(ZoneNum,Ctd)%Area /= 0) THEN
  CVJetRecFlows(ZoneNum,Ctd)%Uin=CVJetRecFlows(ZoneNum,Ctd)%Fin/CVJetRecFlows(ZoneNum,Ctd)%Area
  ELSE
  CVJetRecFlows(ZoneNum,Ctd)%Uin=0.0d0
  ENDIF
END DO

! Verify if Uin is higher than minimum for each aperture
! Create a flow flag for each aperture
! Calculate the total area of all active apertures
ActiveSurfNum=0.0d0
Ain(ZoneNum)=0.0d0
DO Ctd=1,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
  IF (CVJetRecFlows(ZoneNum,Ctd)%Uin <= MinUin) THEN
    CVJetRecFlows(ZoneNum,Ctd)%FlowFlag=0
  ELSE
    CVJetRecFlows(ZoneNum,Ctd)%FlowFlag=1
  END IF
  ActiveSurfNum = ActiveSurfNum + CVJetRecFlows(ZoneNum,Ctd)%FlowFlag
  Ain(ZoneNum)=Ain(ZoneNum)+CVJetRecFlows(ZoneNum,Ctd)%Area*CVJetRecFlows(ZoneNum,Ctd)%FlowFlag
END DO

! Verify if any of the apertures have minimum flow
IF (ActiveSurfNum == 0 ) THEN
  AirModel(ZoneNum)%SimAirModel= .FALSE.
  IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond>0) THEN
    Tin(ZoneNum)=MAT(surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond)%zone)
  ELSEIF(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == Ground) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or. &
           Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
    OPtr=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OSCPtr
    OSC(OPtr)%OSCTempCalc =( OSC(OPtr)%ZoneAirTempCoef*MAT(ZoneNum)      &
                    +OSC(OPtr)%ExtDryBulbCoef*Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp &
                    +OSC(OPtr)%ConstTempCoef*OSC(OPtr)%ConstTemp &
                    +OSC(OPtr)%GroundTempCoef*GroundTemp         &
                    +OSC(OPtr)%WindSpeedCoef                     &
                    *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%WindSpeed &
                    *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp )
    Tin(ZoneNum)  = OSC(OPtr)%OSCTempCalc
  ELSE
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  END IF
  Urec(ZoneNum)=0.0d0
  Ujet(ZoneNum)=0.0d0
  Qrec(ZoneNum)=0.0d0
  CVJetRecFlows(ZoneNum,:)%Ujet=0.0d0
  CVJetRecFlows(ZoneNum,:)%Urec=0.0d0
  RETURN
END IF

! Calculate Uin, the area weighted average velocity of all the active apertures in the zone
! Calculate Qtot, the total volumetric flow rate through all active openings in the zone
Uin=0.0d0

DO Ctd=1,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
  Uin=Uin+CVJetRecFlows(ZoneNum,Ctd)%Area*CVJetRecFlows(ZoneNum,Ctd)%Uin*CVJetRecFlows(ZoneNum,Ctd)%FlowFlag/Ain(ZoneNum)
END DO

!Verify if Uin is higher than minimum:
IF (Uin < MinUin) THEN
  AirModel(ZoneNum)%SimAirModel= .FALSE.
  Urec(ZoneNum)=0.0d0
  Ujet(ZoneNum)=0.0d0
  Qrec(ZoneNum)=0.0d0
  RecInflowRatio(ZoneNum)=0.0d0
  CVJetRecFlows(ZoneNum,:)%Ujet=0.0d0
  CVJetRecFlows(ZoneNum,:)%Urec=0.0d0
  IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond>0) THEN
    Tin(ZoneNum)=MAT(surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond)%zone)
  ELSEIF(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == Ground) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or. &
           Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
    OPtr=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OSCPtr
    OSC(OPtr)%OSCTempCalc = ( OSC(OPtr)%ZoneAirTempCoef*MAT(ZoneNum)      &
                    +OSC(OPtr)%ExtDryBulbCoef*Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp &
                    +OSC(OPtr)%ConstTempCoef*OSC(OPtr)%ConstTemp &
                    +OSC(OPtr)%GroundTempCoef*GroundTemp         &
                    +OSC(OPtr)%WindSpeedCoef                     &
                    *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%WindSpeed &
                    *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp )
    Tin(ZoneNum)  = OSC(OPtr)%OSCTempCalc

  ELSE
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  END IF
  RETURN
END IF

! Evaluate parameter that determines whether recirculations are present
DO Ctd=1,TotUCSDCV
    IF(ZoneNum == ZoneUCSDCV(Ctd)%ZonePtr) THEN
      IF (Ain(ZoneNum)/Aroom >1.0d0/2.0d0) THEN
        JetRecAreaRatio(ZoneNum)=1.0d0
      ELSE
        JetRecAreaRatio(ZoneNum)=Sqrt(Ain(ZoneNum)/Aroom)
      ENDIF
    ENDIF
ENDDO


AirModel(ZoneNum)%SimAirModel= .TRUE.
! Calculate jet and recirculation velocities for all active apertures
Ujet(ZoneNum)=0.0d0
Urec(ZoneNum)=0.0d0
Qrec(ZoneNum)=0.0d0
Qtot(ZoneNum)=0.0d0
CVJetRecFlows(ZoneNum,:)%Ujet=0.0d0
CVJetRecFlows(ZoneNum,:)%Urec=0.0d0
CVJetRecFlows(ZoneNum,:)%Qrec=0.0d0
DO Ctd=1,AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
  IF (CVJetRecFlows(ZoneNum,Ctd)%Uin /=0) THEN
    CVJetRecFlows(ZoneNum,Ctd)%Vjet = CVJetRecFlows(ZoneNum,Ctd)%Uin * SQRT(CVJetRecFlows(ZoneNum,Ctd)%Area) *   &
        6.3d0*LOG(Dstar(ZoneNum) / (6.0d0 * SQRT(CVJetRecFlows(ZoneNum,Ctd)%Area))) / Dstar(ZoneNum)
    CVJetRecFlows(ZoneNum,Ctd)%Yjet  = Cjet1 *   &
        SQRT(CVJetRecFlows(ZoneNum,Ctd)%Area/Aroom) * CVJetRecFlows(ZoneNum,Ctd)%Vjet / CVJetRecFlows(ZoneNum,Ctd)%Uin + Cjet2
    CVJetRecFlows(ZoneNum,Ctd)%Yrec  = Crec1 *   &
        SQRT(CVJetRecFlows(ZoneNum,Ctd)%Area/Aroom) * CVJetRecFlows(ZoneNum,Ctd)%Vjet / CVJetRecFlows(ZoneNum,Ctd)%Uin + Crec2
    CVJetRecFlows(ZoneNum,Ctd)%YQrec = CrecFlow1 *   &
        SQRT(CVJetRecFlows(ZoneNum,Ctd)%Area*Aroom) * CVJetRecFlows(ZoneNum,Ctd)%Vjet / CVJetRecFlows(ZoneNum,Ctd)%Uin + CrecFlow2
    CVJetRecFlows(ZoneNum,Ctd)%Ujet  = CVJetRecFlows(ZoneNum,Ctd)%FlowFlag * CVJetRecFlows(ZoneNum,Ctd)%Yjet /  &
        CVJetRecFlows(ZoneNum,Ctd)%Uin
    CVJetRecFlows(ZoneNum,Ctd)%Urec  = CVJetRecFlows(ZoneNum,Ctd)%FlowFlag * CVJetRecFlows(ZoneNum,Ctd)%Yrec /   &
        CVJetRecFlows(ZoneNum,Ctd)%Uin
    CVJetRecFlows(ZoneNum,Ctd)%Qrec  = CVJetRecFlows(ZoneNum,Ctd)%FlowFlag * CVJetRecFlows(ZoneNum,Ctd)%YQrec /   &
        CVJetRecFlows(ZoneNum,Ctd)%Uin
    Ujet(ZoneNum) = Ujet(ZoneNum) + CVJetRecFlows(ZoneNum,Ctd)%Area * CVJetRecFlows(ZoneNum,Ctd)%Ujet / Ain(ZoneNum)
    Urec(ZoneNum) = Urec(ZoneNum) + CVJetRecFlows(ZoneNum,Ctd)%Area * CVJetRecFlows(ZoneNum,Ctd)%Urec / Ain(ZoneNum)
    Qrec(ZoneNum) = Qrec(ZoneNum) + CVJetRecFlows(ZoneNum,Ctd)%Qrec
    Qtot(ZoneNum) = Qtot(ZoneNum)+CVJetRecFlows(ZoneNum,Ctd)%Fin*CVJetRecFlows(ZoneNum,Ctd)%FlowFlag
    IF (ActiveSurfNum > 1) THEN ! Recirculation flow for multiple surfaces is 90% of flow with one surface
      Urec(ZoneNum) = 0.9d0 * Urec(ZoneNum) + CVJetRecFlows(ZoneNum,Ctd)%Area * CVJetRecFlows(ZoneNum,Ctd)%Urec / Ain(ZoneNum)
    ELSE
      Urec(ZoneNum) = Urec(ZoneNum) + CVJetRecFlows(ZoneNum,Ctd)%Area * CVJetRecFlows(ZoneNum,Ctd)%Urec / Ain(ZoneNum)
    END IF
  ENDIF
END DO


! Ratio between recirculation flow rate and total inflow rate
IF (Qtot(ZoneNum) /=0) THEN
 RecInflowRatio(ZoneNum) = Qrec(ZoneNum)/Qtot(ZoneNum)
ELSE
  RecInflowRatio(ZoneNum) = 0.0d0
ENDIF

! Set Tin based on external conditions of the dominant aperture
IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond <= 0) THEN
  IF(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == Ground) THEN
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  ELSEIF  (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or. &
           Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
    OPtr=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OSCPtr
    OSC(OPtr)%OSCTempCalc = ( OSC(OPtr)%ZoneAirTempCoef*MAT(ZoneNum)      &
                    +OSC(OPtr)%ExtDryBulbCoef*Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp &
                    +OSC(OPtr)%ConstTempCoef*OSC(OPtr)%ConstTemp &
                    +OSC(OPtr)%GroundTempCoef*GroundTemp         &
                    +OSC(OPtr)%WindSpeedCoef                     &
                    *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%WindSpeed &
                    *Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp )
    Tin(ZoneNum)  = OSC(OPtr)%OSCTempCalc
  ELSE
    Tin(ZoneNum)=Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
  END IF
ELSE
  ! adiabatic surface
  IF (MultizoneSurfaceData(MaxSurf)%SurfNum == Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond) THEN
    NodeNum1 = AirflowNetworkLinkageData(MaxSurf)%NodeNums(1)
    NodeNum2 = AirflowNetworkLinkageData(MaxSurf)%NodeNums(2)
    IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone ==ZoneNum) THEN
      IF (AirflowNetworkNodeData(NodeNum1)%EplusZoneNum <= 0) THEN
        Tin(ZoneNum)= Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
      ELSEIF (AirModel(AirflowNetworkNodeData(NodeNum1)%EplusZoneNum)%AirModelType==RoomAirModel_UCSDCV) THEN
        Tin(ZoneNum)=RoomOutflowTemp(AirflowNetworkNodeData(NodeNum1)%EplusZoneNum)
      ELSE
        Tin(ZoneNum)=MAT(AirflowNetworkNodeData(NodeNum1)%EplusZoneNum)
      END IF

    ELSE

      IF (AirflowNetworkNodeData(NodeNum2)%EplusZoneNum <= 0) THEN
        Tin(ZoneNum)= Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
      ELSEIF (AirModel(AirflowNetworkNodeData(NodeNum2)%EplusZoneNum)%AirModelType==RoomAirModel_UCSDCV) THEN
        Tin(ZoneNum)=RoomOutflowTemp(AirflowNetworkNodeData(NodeNum2)%EplusZoneNum)
      ELSE
        Tin(ZoneNum)=MAT(AirflowNetworkNodeData(NodeNum2)%EplusZoneNum)
      END IF
    ENDIF
  ELSEIF ((Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone ==ZoneNum) .AND.   &
    (AirModel(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundcond)%Zone)%AirModelType==RoomAirModel_UCSDCV)) THEN
    Tin(ZoneNum)=RoomOutflowTemp(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundcond)%Zone)
  ELSEIF ((Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone /=ZoneNum) .AND.   &
    (AirModel(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone)%AirModelType==RoomAirModel_UCSDCV)) THEN
    Tin(ZoneNum)=RoomOutflowTemp(MultizoneSurfaceData(MaxSurf)%SurfNum)
  ELSE
    IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone ==ZoneNum) THEN
      Tin(ZoneNum)=MAT(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond)%Zone)
    ELSE
      Tin(ZoneNum)=MAT(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone)
    END IF
  END IF
END IF


END SUBROUTINE EvolveParaUCSDCV


!**************************************************************************************************

SUBROUTINE CalcUCSDCV(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   October 2004
          !       MODIFIED       8/2013 - Sam Brunswick
          !                      To incorporate improved temperature calculations
          !       RE-ENGINEERED  -

          ! PURPOSE OF THIS SUBROUTINE:
          ! Subroutine for cross ventilation modelling.
          !
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
  USE DataHVACGlobals,            ONLY: TimestepSys
  USE InternalHeatGains,          ONLY: SumAllInternalConvectionGains, SumAllReturnAirConvectionGains

  IMPLICIT         NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,                         INTENT(IN) :: ZoneNum   ! Which Zonenum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Sigma=10.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)     :: GainsFrac            ! Fraction of lower subzone internal gains that mix as opposed to forming plumes
  REAL(r64)     :: ConvGains            ! Total convective gains in the room
  REAL(r64)     :: ConvGainsJet         ! Total convective gains released in jet subzone
  REAL(r64)     :: ConvGainsRec         ! Total convective gains released in recirculation subzone
  REAL(r64)     :: MCP_Total            ! Total capacity rate into the zone - assumed to enter at low level
  REAL(r64)     :: ZTAveraged

  INTEGER       :: CTD
  REAL(r64)     :: VolOverAin
  REAL(r64)     :: MCpT_Total
  REAL(r64)     :: L
  REAL(r64)     :: ZoneMult   ! total zone multiplier
  REAL(r64)     :: RetAirConvGain

GainsFrac=0.0d0
ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

DO Ctd=1,TotUCSDCV
  IF(ZoneNum == ZoneUCSDCV(Ctd)%ZonePtr) THEN
    GainsFrac = GetCurrentScheduleValue(ZoneUCSDCV(Ctd)%SchedGainsPtr)
  ENDIF
ENDDO

CALL SumAllInternalConvectionGains(ZoneNum, ConvGains)
ConvGains =  ConvGains    &
             + SumConvHTRadSys(ZoneNum) &
             + SysDepZoneLoadsLagged(ZoneNum) + NonAirSystemResponse(ZoneNum)/ZoneMult

! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
! low or zero)
IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
  CALL SumAllReturnAirConvectionGains(ZoneNum, RetAirConvGain )
  ConvGains = ConvGains + RetAirConvGain
END IF

ConvGainsJet = ConvGains*GainsFrac
ConvGainsRec = ConvGains*(1.d0-GainsFrac)
MCP_Total = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MdotCPOA(ZoneNum)
MCpT_Total = MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + &
             MdotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp

IF (SimulateAirflowNetwork == AirflowNetworkControlMultizone) THEN
    MCP_Total = AirflowNetworkExchangeData(ZoneNum)%SumMCp+AirflowNetworkExchangeData(ZoneNum)%SumMMCp
    MCpT_Total = AirflowNetworkExchangeData(ZoneNum)%SumMCpT+AirflowNetworkExchangeData(ZoneNum)%SumMMCpT
END IF

CALL EvolveParaUCSDCV(ZoneNum)
L=Droom(ZoneNum)

IF (AirModel(ZoneNum)%SimAirModel) THEN
  !=============================== CROSS VENTILATION  Calculation ==============================================
  ZoneCVisMixing(ZoneNum)=0.0d0
  ZoneCVhasREC(ZoneNum)=1.0d0
  DO Ctd = 1,4
    CALL HcUCSDCV(ZoneNum)
    IF (JetRecAreaRatio(ZoneNum)/=1.0d0) THEN
      ZTREC(ZoneNum) =(ConvGainsRec*CrecTemp + CrecTemp*HAT_R + Tin(ZoneNum)*MCp_Total)/(CrecTemp*HA_R + MCp_Total)
    ENDIF
    ZTJET(ZoneNum) = (ConvGainsJet*CjetTemp + ConvGainsRec*CjetTemp +CjetTemp*HAT_J + CjetTemp*HAT_R + Tin(ZoneNum)*MCp_Total  &
       - CjetTemp*HA_R*ZTREC(ZoneNum)) / (CjetTemp*HA_J + MCp_Total)
    RoomOutflowTemp(ZoneNum) = (ConvGainsJet + ConvGainsRec + HAT_J + HAT_R +  Tin(ZoneNum)*MCp_Total -   &
       HA_J*ZTJET(ZoneNum) - HA_R*ZTREC(ZoneNum))/ MCp_Total
  END DO
  IF (JetRecAreaRatio(ZoneNum)==1.0d0) THEN
    ZoneCVhasREC(ZoneNum)=0.0d0
    ZTREC(ZoneNum)=RoomOutflowTemp(ZoneNum)
    ZTREC(ZoneNum)=ZTJET(ZoneNum)
    ZTREC(ZoneNum)=ZTJET(ZoneNum)
  ENDIF
  ! If temperature increase is above 1.5C then go to mixing
  IF (RoomOutflowTemp(ZoneNum) - Tin(ZoneNum) > 1.5d0) THEN
    ZoneCVisMixing(ZoneNum)=1.0d0
    ZoneCVhasREC(ZoneNum)=0.0d0
    AirModel(ZoneNum)%SimAirModel = .FALSE.
    Ujet(ZoneNum)=0.0d0
    Urec(ZoneNum)=0.0d0
    Qrec(ZoneNum)=0.0d0
    RecInflowRatio(ZoneNum)=0.0d0
    CVJetRecFlows%Ujet=0.0d0
    CVJetRecFlows%Urec=0.0d0
    DO Ctd = 1,3
      ZTAveraged=mat(zonenum)
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      CALL HcUCSDCV(ZoneNum)
      ZTAveraged=mat(zonenum)
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
    END DO
  END IF
ELSE
  !=============================== M I X E D  Calculation ======================================================
  ZoneCVisMixing(ZoneNum)=1.0d0
  ZoneCVhasREC(ZoneNum)=0.0d0
  Ujet(ZoneNum)=0.0d0
  Urec(ZoneNum)=0.0d0
  Qrec(ZoneNum)=0.0d0
  RecInflowRatio(ZoneNum)=0.0d0
  CVJetRecFlows%Ujet=0.0d0
  CVJetRecFlows%Urec=0.0d0
    DO Ctd = 1,3
      ZTAveraged=mat(zonenum)
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      CALL HcUCSDCV(ZoneNum)
      ZTAveraged=mat(zonenum)
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      RoomOutflowTemp(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
      ZTJET(ZoneNum) = ZTAveraged
      ZTREC(ZoneNum) = ZTAveraged
    END DO
END IF
!============================================================================================================

RETURN

END SUBROUTINE CalcUCSDCV

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

END MODULE CrossVentMgr
