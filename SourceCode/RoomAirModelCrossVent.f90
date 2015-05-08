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
  REAL(r64)  :: HAT_J =0.0 ! HAT_J Convection Coefficient times Area times Temperature for Jet subzone
  REAL(r64)  :: HA_J  =0.0 ! HA_J  Convection Coefficient times Area for Jet subzone
  REAL(r64)  :: HAT_R =0.0 ! HAT_R Convection Coefficient times Area times Temperature for Recirculation subzone
  REAL(r64)  :: HA_R  =0.0 ! HA_J  Convection Coefficient times Area for Recirculation subzone
  INTEGER, ALLOCATABLE, DIMENSION (:)  :: LastMaxSurfdt
  INTEGER                              :: LastMaxSurf=0
  INTEGER                              :: LastZone=0


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
  REAL(r64),PARAMETER  :: BaseDischargeCoef = 0.62d0

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
    ALLOCATE(LastMaxSurfdt(NumOfZones))
    LastMaxSurfdt = 0
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
    LastZone = 0
    LastMaxSurf = 0
  END IF

  ! Do the begin environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(ZoneNum)) THEN
    LastMaxSurfdt(ZoneNum) = 0
    LastMaxSurf = 0
    LastZone = 0
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
          !       MODIFIED       -
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
  HAT_J    = 0.0
  HAT_R    = 0.0
  HA_J     = 0.0
  HA_R     = 0.0

  ! Is the air flow model for this zone set to UCSDCV Displacement Ventilation?
  IF(IsZoneCV(ZoneNum)) THEN

    ! WALL Hc, HA and HAT calculation
    DO Ctd = PosZ_Wall((ZoneNum-1)*2 + 1),PosZ_Wall((ZoneNum-1)*2 + 2)
      SurfNum = APos_Wall(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp

      IF (SurfNum == 0) CYCLE

      TempEffBulkAir(SurfNum) = ZTrec(ZoneNum)%Med
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
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
          TempEffBulkAir(SurfNum) = ZTrec(ZoneNum)%Med
          CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
          HWindow(Ctd)= CVHcIn(SurfNum)
          HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_R
          HA_R = Surface(SurfNum)%Area*HWindow(Ctd) + HA_R
      ENDIF

      IF (Surface(SurfNum)%Tilt <= 10.0d0) THEN ! Window Ceiling
         TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)%In
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
         Hjet= CVHcIn(SurfNum)
         TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)%Med
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
         Hrec= CVHcIn(SurfNum)
         HWindow(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
         HAT_R = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
         HA_R  = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
         HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
         HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
         TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)%In+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)%Med
      ENDIF

      IF (Surface(SurfNum)%Tilt >= 170.0d0) THEN ! Window Floor
         TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)%In
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
         Hjet= CVHcIn(SurfNum)
         TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)%Med
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
         Hrec= CVHcIn(SurfNum)
         HWindow(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
         HAT_R = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
         HA_R  = Surface(SurfNum)%Area*(1.d0-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
         HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
         HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
         TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)%In+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)%Med
      ENDIF

      CVHcIn(SurfNum) = HWindow(Ctd)

    END DO ! END WINDOW

    ! DOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Door((ZoneNum-1)*2 + 1),PosZ_Door((ZoneNum-1)*2 + 2) ! DOOR
      SurfNum = APos_Door(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
        TempEffBulkAir(SurfNum) = ZTrec(ZoneNum)%Med
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
        HDoor(Ctd)= CVHcIn(SurfNum)
        HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HDoor(Ctd) + HAT_R
        HA_R = Surface(SurfNum)%Area*HDoor(Ctd) + HA_R
    ENDDO ! END DOOR


    ! INTERNAL Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Internal((ZoneNum-1)*2 + 1),PosZ_Internal((ZoneNum-1)*2 + 2)
      SurfNum = APos_Internal(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTrec(ZoneNum)%Med
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
      HInternal(Ctd) = CVHcIn(SurfNum)
      HAT_R = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HInternal(Ctd) + HAT_R
      HA_R  = Surface(SurfNum)%Area* HInternal(Ctd) + HA_R
    END DO  ! END INTERNAL

    ! CEILING Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Ceiling((ZoneNum-1)*2 + 1),PosZ_Ceiling((ZoneNum-1)*2 + 2)
      SurfNum = APos_Ceiling(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)%In
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
      Hjet= CVHcIn(SurfNum)
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)%Med
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
      Hrec= CVHcIn(SurfNum)
      HCeiling(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
      HAT_R = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
      HA_R  = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
      HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
      HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
      TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)%In+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)%Med
      CVHcIn(SurfNum) = HCeiling(Ctd)
    END DO  ! END CEILING

    ! FLOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Floor((ZoneNum-1)*2 + 1),PosZ_Floor((ZoneNum-1)*2 + 2)
      SurfNum = APos_Floor(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTJET(ZoneNum)%In
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
      Hjet= CVHcIn(SurfNum)
      TempEffBulkAir(SurfNum) = ZTREC(ZoneNum)%Med
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,CVHcIn,Uhc)
      Hrec= CVHcIn(SurfNum)
      HFloor(Ctd)= JetRecAreaRatio(ZoneNum)*Hjet+(1-JetRecAreaRatio(ZoneNum))*Hrec
      HAT_R = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*TempSurfIn(SurfNum)*Hrec + HAT_R
      HA_R  = Surface(SurfNum)%Area*(1-JetRecAreaRatio(ZoneNum))*Hrec + HA_R
      HAT_J = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*TempSurfIn(SurfNum)*Hjet + HAT_J
      HA_J  = Surface(SurfNum)%Area*JetRecAreaRatio(ZoneNum)*Hjet + HA_J
      TempEffBulkAir(SurfNum)=JetRecAreaRatio(ZoneNum)*ZTJET(ZoneNum)%In+(1-JetRecAreaRatio(ZoneNum))*ZTREC(ZoneNum)%Med
      CVHcIn(SurfNum) = HFloor(Ctd)

    END DO  ! END FLOOR


  ENDIF


END SUBROUTINE HcUCSDCV

!**************************************************************************************************

SUBROUTINE EvolveParaUCSDCV(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   October 2004
          !       MODIFIED       -
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
  REAL(r64),PARAMETER  :: VoltoAinRatio = 20.0d0
  REAL(r64),PARAMETER  :: Sigma = 10.0d0
  REAL(r64),PARAMETER  :: MinFluxRatio = 0.9d0
  REAL(r64),PARAMETER  :: MinUin = 0.2d0
  REAL(r64),PARAMETER  :: BaseDischargeCoef = 0.62d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER                                  :: Ctd           ! counter
INTEGER                                  :: Ctd2          ! counter
INTEGER                                  :: OPtr          ! counter
REAL(r64), SAVE                               :: Win           ! Inflow aperture width
REAL(r64), SAVE                               :: Wroom         ! Room width, relative to jet direction
REAL(r64), SAVE                               :: Aroom         ! Room area cross section
REAL(r64)                                :: Fin           ! Inflow air flux
REAL(r64)                                :: Uin           ! Inflow air velocity
REAL(r64)                                :: Fr            !
REAL(r64)                                :: Cl
REAL(r64)                                :: Cd
REAL(r64)                                :: SumToZone
REAL(r64)                                :: MaxFlux
INTEGER                                  :: MaxSurf
REAL(r64)                                :: XX
REAL(r64)                                :: YY
REAL(r64)                                :: ZZ
REAL(r64)                                :: XX_Wall
REAL(r64)                                :: YY_Wall
REAL(r64)                                :: ZZ_Wall
INTEGER                                  :: NSides        ! Number of sides in surface
INTEGER         :: CompNum = 0 ! AirflowNetwork Component number
INTEGER         :: TypeNum = 0 ! Airflownetwork Type Number within a component
INTEGER         :: NodeNum1 = 0 ! The first node number in an AirflowNetwork linkage data
INTEGER         :: NodeNum2 = 0 ! The Second node number in an AirflowNetwork linkage data
REAL(r64)       :: OpenFactor = 0    ! Opening factor for a detailed opening component
REAL(r64)       :: DischargeCoef = 0 ! Discharge coefficient for a detailed opening component

maxsurf=0
sumtozone=0.0
maxflux=0.0
Cd = 0.0

! Choose the dominant aperture:
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


! Check if dominant aperture has a high relative flux:
IF ((sumtozone==0) .or. ((MaxFlux/SumToZone)< MinFluxRatio)) THEN
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
  Urec(ZoneNum)=0
  Ujet(ZoneNum)=0
  RETURN
END IF


! This should be calculated once until the dominant aperture in the zone changes:
IF (((MaxSurf== LastMaxSurf) .AND. (ZoneNum==LastZone)) .or. LastMaxSurfdt(ZoneNum)==MaxSurf) THEN

  Fin=MaxFlux

  CompNum = AirflowNetworkLinkageData(MaxSurf)%CompNum
  TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
  IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_DOP) then
    DO Ctd=1,MultizoneCompDetOpeningData(TypeNum)%NumFac
      If (Ctd .eq. 1) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac1
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff1
      end if
      If (Ctd .eq. 2) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac2
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff2
      end if
      If (Ctd .eq. 3) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac3
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff3
      end if
      If (Ctd .eq. 4) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac4
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff4
      end if
     IF (OpenFactor == MultizoneSurfaceData(MaxSurf)%OpenFactor) THEN
        Cd = DischargeCoef
      END IF
    END DO
  ELSE If (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) then
    Cd = MultizoneSurfaceCrackData(TypeNum)%FlowCoef
  ELSE
    CALL ShowSevereError('RoomAirModelCrossVent:EvolveParaUCSDCV: Illegal leakage component referenced '//  &
       'in the cross ventilation room air model')
    CALL ShowContinueError('Surface '//TRIM(AirflowNetworkLinkageData(MaxSurf)%Name)//' in zone '//TRIM(Zone(ZoneNum)%Name) &
                           //' uses leakage component '//TRIM(AirflowNetworkLinkageData(MaxSurf)%CompName))
    CALL ShowContinueError('Only leakage component types AirflowNetwork:MultiZone:Component:DetailedOpening and ')
    CALL ShowContinueError('AirflowNetwork:MultiZone:Surface:Crack can be used with the cross ventilation room air model')
    CALL ShowFatalError('Previous severe error causes program termination')
  END IF
  IF (Cd <= 0.0 .OR. Cd >= 10) Cd = 0.6d0

  Uin=Fin/(Cd*Ain(ZoneNum))
  Aroom=Zone(ZoneNum)%Volume/Lroom(ZoneNum)

ELSE

  ! Dominant Aperture Ain, Win:
  CompNum = AirflowNetworkLinkageData(MaxSurf)%CompNum
  TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
  IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_DOP) then
    IF (MultizoneSurfaceData(MaxSurf)%OpenFactor /= 0.) THEN
      Ain(ZoneNum)=SurfParametersCVDV(MaxSurf)%Width*SurfParametersCVDV(MaxSurf)%Height*MultizoneSurfaceData(MaxSurf)%OpenFactor
      IF (SurfParametersCVDV(MaxSurf)%Width<SurfParametersCVDV(MaxSurf)%Height*MultizoneSurfaceData(MaxSurf)%OpenFactor) THEN
        Win=SurfParametersCVDV(MaxSurf)%Width
        IsWidth(ZoneNum)=1
      ELSE
        Win=SurfParametersCVDV(MaxSurf)%Height*MultizoneSurfaceData(MaxSurf)%OpenFactor
        IsWidth(ZoneNum)=0
      END IF
    ELSE
      Ain(ZoneNum)= MultizoneSurfaceCrackData(TypeNum)%FlowCoef / &
                     (BaseDischargeCoef*SQRT(2./PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))))
      Win=Ain(ZoneNum)/SurfParametersCVDV(MaxSurf)%Width
      IsWidth(ZoneNum)=1
    END IF
  ELSE IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) THEN
    Ain(ZoneNum)=SurfParametersCVDV(MaxSurf)%Width*SurfParametersCVDV(MaxSurf)%Height
    Win=SurfParametersCVDV(MaxSurf)%Width
    IsWidth(ZoneNum)=1
  ELSE
    CALL ShowSevereError('RoomAirModelCrossVent:EvolveParaUCSDCV: Illegal leakage component referenced '//  &
       'in the cross ventilation room air model')
    CALL ShowContinueError('Surface '//TRIM(AirflowNetworkLinkageData(MaxSurf)%Name)//' in zone '//TRIM(Zone(ZoneNum)%Name) &
                           //' uses leakage component '//TRIM(AirflowNetworkLinkageData(MaxSurf)%CompName))
    CALL ShowContinueError('Only leakage component types AirflowNetwork:MultiZone:Component:DetailedOpening and ')
    CALL ShowContinueError('AirflowNetwork:MultiZone:Surface:Crack can be used with the cross ventilation room air model')
    CALL ShowFatalError('Previous severe error causes program termination')
  END IF

  ! Lroom, Wroom, Tin
  ! Lroom is the distance between the average point of the base surface of the airflow network Surface (if the base surface
  ! is a Window or Door it looks for the second base surface).
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

 ! Calculate Lroom
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
    IF (SQRT((XX-XX_Wall)**2+(YY-YY_Wall)**2+(ZZ-ZZ_Wall)**2)>Lroom(ZoneNum)) THEN
     Lroom(ZoneNum)=SQRT((XX-XX_Wall)**2+(YY-YY_Wall)**2+(ZZ-ZZ_Wall)**2)
    END IF
  END DO

  IF (IsWidth(ZoneNum)==1) THEN
    Wroom =Zone(ZoneNum)%FloorArea/Lroom(ZoneNum)
  ELSE
    Wroom=Zone(ZoneNum)%Volume/Zone(ZoneNum)%FloorArea
  END IF
  Aroom=Zone(ZoneNum)%Volume/Lroom(ZoneNum)
  Fin=MaxFlux


  CompNum = AirflowNetworkLinkageData(MaxSurf)%CompNum
  TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
  IF (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_DOP) then
    DO Ctd=1,MultizoneCompDetOpeningData(TypeNum)%NumFac
      If (Ctd .eq. 1) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac1
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff1
      end if
      If (Ctd .eq. 2) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac2
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff2
      end if
      If (Ctd .eq. 3) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac3
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff3
      end if
      If (Ctd .eq. 4) then
         OpenFactor=MultizoneCompDetOpeningData(TypeNum)%OpenFac4
         DischargeCoef=MultizoneCompDetOpeningData(TypeNum)%DischCoeff4
      end if
      IF (OpenFactor == MultizoneSurfaceData(MaxSurf)%OpenFactor) THEN
        Cd = DischargeCoef
      END IF
    END DO
  ELSE
    If (AirflowNetworkCompData(CompNum)%CompTypeNum == CompTypeNum_SCR) then
      Cd = MultizoneSurfaceCrackData(TypeNum)%FlowCoef
    End If
  END IF

  IF (Cd <= 0.0 .OR. Cd >= 10) Cd = 0.6d0
  Uin=Fin/(Cd*Ain(ZoneNum))

END IF


LastMaxSurf= MaxSurf
LastMaxSurfdt(ZoneNum)= MaxSurf
LastZone=ZoneNum

!Verify if Uin is higher than minimum:
IF (Uin < MinUin) THEN
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
  Urec(ZoneNum)=0
  Ujet(ZoneNum)=0
  RETURN
END IF

! Evaluate parameter that determines wether recirculations are present
DO Ctd=1,TotUCSDCV
    IF(ZoneNum == ZoneUCSDCV(Ctd)%ZonePtr) THEN
      IF (Ain(ZoneNum)/Aroom >1.0d0/2.0d0) THEN
        JetRecAreaRatio(ZoneNum)=1.0
      ELSE
        JetRecAreaRatio(ZoneNum)=Sqrt(Ain(ZoneNum)/Aroom)
      ENDIF
      UPsOFo(ZoneNum) = 4.d0*Sqrt(Ain(ZoneNum))/(0.622d0*Ain(ZoneNum)*4*Sqrt(3.1416d0)*Sigma)
    ENDIF
ENDDO


!Verify if Volume to Input Area ratio is acceptable
IF (SQRT(Zone(ZoneNum)%Volume/Ain(ZoneNum)**(3.0d0/2.0d0)) >= VoltoAinRatio) THEN
  AirModel(ZoneNum)%SimAirModel= .FALSE.
IF (Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond>0) THEN
    Tin(ZoneNum)=MAT(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundCond)%zone)
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
  Urec(ZoneNum)=0
  Ujet(ZoneNum)=0
  RETURN
ELSE

!Calculate Cl
Cl = 2.0*Lroom(ZoneNum)/(Wroom-Win)

!Calculate Velocities
IF (Cl>=1.0/3.0 .AND. Cl<=4.0) THEN
  AirModel(ZoneNum)%SimAirModel= .TRUE.
  !Recirculation velocity
  Urec(ZoneNum)=0.298d0*sqrt(Lroom(ZoneNum)/(Aroom*Ain(ZoneNum)**(3.0d0/2.0d0)))*Fin
  !Jet velocity
  Ujet(ZoneNum)= 1.56d0*Fin/(sqrt(Aroom*Ain(Zonenum)))
  !Recirculation flux
  Fr= 0.147d0**sqrt(Lroom(ZoneNum)*Aroom/Ain(ZoneNum)**(3.0d0/2.0d0))*Fin
  !Velocity for forced hc calculation
  Uhc(ZoneNum)=0.115d0**(10.0d0/8.0d0)*((121.0d0+Lroom(ZoneNum)/sqrt(Ain(ZoneNum)))/Aroom/Ain(ZoneNum))**(0.5d0)*Uin
ELSEIF (Cl>4.0 .AND. Cl<= 11.0) THEN
  AirModel(ZoneNum)%SimAirModel= .TRUE.
  Urec(ZoneNum)=0.162d0*sqrt(Lroom(ZoneNum)/(Aroom*Ain(ZoneNum)**(3.0d0/2.0d0)))*Fin
  Ujet(ZoneNum)= 1.56d0*Fin/(sqrt(Aroom*Ain(Zonenum)))
  Fr= 0.077**sqrt(Lroom(ZoneNum)*Aroom/Ain(ZoneNum)**(3.0d0/2.0d0))*Fin
  Uhc(ZoneNum)=0.082d0**(10.0d0/8.0d0)*((121.0d0+Lroom(ZoneNum)/sqrt(Ain(ZoneNum)))/Aroom/Ain(ZoneNum))**(0.5d0)*Uin
ELSE
  AirModel(ZoneNum)%SimAirModel= .FALSE.
  Tin(ZoneNum)=MAT(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone)
  Urec(ZoneNum)=0
  Ujet(ZoneNum)=0
  RETURN
ENDIF

ENDIF

IF (JetRecAreaRatio(ZoneNum)==1.0) THEN
  Uhc(ZoneNum) = Ujet(ZoneNum)
END IF



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
        Tin(ZoneNum)=ZTJET(AirflowNetworkNodeData(NodeNum1)%EplusZoneNum)%OutRoom
      ELSE
        Tin(ZoneNum)=MAT(AirflowNetworkNodeData(NodeNum1)%EplusZoneNum)
      END IF

    ELSE

      IF (AirflowNetworkNodeData(NodeNum2)%EplusZoneNum <= 0) THEN
        Tin(ZoneNum)= Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutDryBulbTemp
      ELSEIF (AirModel(AirflowNetworkNodeData(NodeNum2)%EplusZoneNum)%AirModelType==RoomAirModel_UCSDCV) THEN
        Tin(ZoneNum)=ZTJET(AirflowNetworkNodeData(NodeNum2)%EplusZoneNum)%OutRoom
      ELSE
        Tin(ZoneNum)=MAT(AirflowNetworkNodeData(NodeNum2)%EplusZoneNum)
      END IF
    ENDIF
  ELSEIF ((Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone ==ZoneNum) .AND.   &
      (AirModel(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundcond)%Zone)%AirModelType==RoomAirModel_UCSDCV)) THEN
    Tin(ZoneNum)=ZTJET(Surface(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%ExtBoundcond)%Zone)%OutRoom
  ELSEIF ((Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone /=ZoneNum) .AND.   &
      (AirModel(Surface(MultizoneSurfaceData(MaxSurf)%SurfNum)%Zone)%AirModelType==RoomAirModel_UCSDCV)) THEN
    Tin(ZoneNum)=ZTJET(MultizoneSurfaceData(MaxSurf)%SurfNum)%OutRoom
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
          !       MODIFIED       -
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
!unused  REAL(r64)     :: TempDiffCritRep      ! Minimum temperature difference between mixed and occupied subzones for reporting
!unused  LOGICAL       :: MIXFLAG
!unused  INTEGER       :: ZoneNodeNum          ! index number of the zone node
!unused  INTEGER       :: NodeNum

  INTEGER       :: CTD
!unused  REAL(r64)     :: MinFlow
  REAL(r64)     :: VolOverAin
!unused  REAL(r64)     :: MTGAUX
!unused  REAL(r64)     :: CpAir
  REAL(r64)     :: MCpT_Total
  REAL(r64)     :: L
  REAL(r64)     :: ZoneMult   ! total zone multiplier

  GainsFrac=0.0
  ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

  DO Ctd=1,TotUCSDCV
    IF(ZoneNum == ZoneUCSDCV(Ctd)%ZonePtr) THEN
      GainsFrac = GetCurrentScheduleValue(ZoneUCSDCV(Ctd)%SchedGainsPtr)
    ENDIF
  ENDDO

  ConvGains = ZoneIntGain(ZoneNum)%QOCCON + ZoneIntGain(ZoneNum)%T_QLTCON &
                      + ZoneIntGain(ZoneNum)%QEECON + ZoneIntGain(ZoneNum)%QGECON + ZoneIntGain(ZoneNum)%QOECON &
                      + ZoneIntGain(ZoneNum)%QHWCON + ZoneIntGain(ZoneNum)%QSECON + ZoneIntGain(ZoneNum)%QBBCON &
                      + ZoneIntGain(ZoneNum)%QLTCON + ZoneIntGain(ZoneNum)%TDDPipeGain + SumConvHTRadSys(ZoneNum) &
                      + ZoneIntGain(ZoneNum)%WaterThermalTankGain + ZoneIntGain(ZoneNum)%QFCConv &
                      + ZoneIntGain(ZoneNum)%WaterUseSensibleGain + ZoneIntGain(ZoneNum)%QGenConv &
                      + ZoneIntGain(ZoneNum)%QInvertConv + ZoneIntGain(ZoneNum)%QElecStorConv &
                      + ZoneIntGain(ZoneNum)%PipeHTGain + RefrigCaseCredit(ZoneNum)%SenCaseCreditToZone &
                      + SysDepZoneLoadsLagged(ZoneNum) + NonAirSystemResponse(ZoneNum)/ZoneMult

  ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
  ! low or zero)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    ConvGains = ConvGains + RefrigCaseCredit(ZoneNum)%SenCaseCreditToHVAC + ZoneIntGain(ZoneNum)%QLTCRA +   &
       ZoneIntGain(ZoneNum)%T_QLTCRA
  END IF

  ConvGainsJet = ConvGains*GainsFrac
  ConvGainsRec = ConvGains*(1.d0-GainsFrac)

  MCP_Total = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MdotCPOA(ZoneNum)
  MCpT_Total = MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + &
               MdotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp
  if (SimulateAirflowNetwork == AirflowNetworkControlMultizone) then
    MCP_Total = AirflowNetworkExchangeData(ZoneNum)%SumMCp+AirflowNetworkExchangeData(ZoneNum)%SumMMCp
    MCpT_Total = AirflowNetworkExchangeData(ZoneNum)%SumMCpT+AirflowNetworkExchangeData(ZoneNum)%SumMMCpT
  end if


  CALL EvolveParaUCSDCV(ZoneNum)

  Rfr(zonenum)=0.147d0*Sqrt(Zone(ZoneNum)%Volume/Ain(ZoneNum)**1.5d0)
  L=Lroom(ZoneNum)
  VolOverAin=Sqrt(Zone(ZoneNum)%Volume/Ain(ZoneNum)**1.5d0)

  IF (AirModel(ZoneNum)%SimAirModel) THEN
    !=============================== CROSS VENTILATION  Calculation ==============================================
    ZoneCVisMixing(ZoneNum)=0.0
    ZoneCVhasREC(ZoneNum)=1
    DO Ctd = 1,4
      CALL HcUCSDCV(ZoneNum)

     IF (JetRecAreaRatio(ZoneNum)/=1.0) THEN

       ZTREC(ZoneNum)%In =((-0.294d0*MCp_Total*VolOverAin + HA_R)*(ConvGainsJet + 2.d0*MCp_Total*Tin(ZoneNum)) +   &
                         Exp((6.170068027210885d0*L* &
                         (1.d0 + 0.147d0*VolOverAin))/(Sqrt(Ain(ZoneNum))*VolOverAin*Sigma))*(0.294d0*ConvGainsJet*MCp_Total* &
                         VolOverAin + 4.d0*ConvGainsRec*MCp_Total*(1.d0 + 0.147d0*VolOverAin) - ConvGainsJet*HA_R + 4*MCp_Total* &
                         HAT_R + 0.588d0*MCp_Total*VolOverAin*HAT_R + 0.588d0*MCp_Total**2*VolOverAin* &
                         Tin(ZoneNum) - 2.d0*MCp_Total*HA_R*Tin(ZoneNum)))/(2.d0*MCp_Total*(-0.294d0*MCp_Total*VolOverAin + HA_R + &
                         Exp((6.170068027210885d0*L*(1.d0 + 0.147d0*VolOverAin))/(Sqrt(Ain(ZoneNum))*VolOverAin*Sigma))* &
                         (HA_R + 0.294d0*VolOverAin*(MCp_Total + HA_R))))

       ZTREC(ZoneNum)%Out=(4.d0*ConvGainsRec*MCp_Total - 0.294d0*ConvGainsJet*MCp_Total*VolOverAin - ConvGainsJet*HA_R +   &
                         4.d0*MCp_Total*HAT_R - 0.588d0*MCp_Total**2* &
                         VolOverAin*Tin(ZoneNum) - 2*MCp_Total*HA_R*Tin(ZoneNum) +   &
                         Exp((6.170068027210885d0*L*(1.d0 + 0.147d0*VolOverAin))/ &
                         (Sqrt(Ain(ZoneNum))*VolOverAin*Sigma))*(ConvGainsJet*(0.294d0*MCp_Total*VolOverAin + HA_R) +   &
                         0.588d0*MCp_Total* &
                         VolOverAin*(ConvGainsRec + HAT_R) + 2.d0*MCp_Total*(0.294d0*MCp_Total*VolOverAin + HA_R)*Tin(ZoneNum)))/  &
                         (2.d0*MCp_Total* &
                         (-0.294d0*MCp_Total*VolOverAin + HA_R + Exp((6.170068027210885d0*L*(1.d0 + 0.147d0*VolOverAin))/  &
                         (Sqrt(Ain(ZoneNum))* &
                         VolOverAin*Sigma))*(HA_R + 0.294d0*VolOverAin*(MCp_Total + HA_R))))
       ZTREC(ZoneNum)%Med= (ZTREC(ZoneNum)%In + ZTREC(ZoneNum)%Out)/2.d0
     ENDIF

       ZTJET(ZoneNum)%In = Tin(ZoneNum)+ConvGainsJet/(2* MCP_Total)


       ZTJET(ZoneNum)%Out=(-0.294d0*ConvGainsJet*MCp_Total*VolOverAin - 0.588d0*ConvGainsRec*MCp_Total*VolOverAin +   &
                           ConvGainsJet*HA_R + 0.294d0*ConvGainsJet* &
                        VolOverAin*HA_R - 0.588d0*MCp_Total*VolOverAin*HAT_R - 0.588d0*MCp_Total**2*VolOverAin*Tin(ZoneNum) &
                        + 2*MCp_Total*HA_R*Tin(ZoneNum) + 0.588d0*MCp_Total*VolOverAin*HA_R*Tin(ZoneNum) +   &
                        Exp((6.170068027210885d0*L*(1.d0 + 0.147d0* &
                        VolOverAin))/(Sqrt(Ain(ZoneNum))*VolOverAin*Sigma))*  &
                        (ConvGainsJet*(0.294d0*MCp_Total*VolOverAin + HA_R) + &
                        0.588d0*MCp_Total*VolOverAin*(ConvGainsRec + HAT_R) + 2.d0*MCp_Total*  &
                        (0.294d0*MCp_Total*VolOverAin + HA_R)*Tin(ZoneNum)))/(2.d0* &
                        MCp_Total*(-0.294d0*MCp_Total*VolOverAin + HA_R +   &
                        Exp((6.170068027210885*L*(1.d0 + 0.147d0*VolOverAin))/(Sqrt(Ain(ZoneNum))* &
                        VolOverAin*Sigma))*(HA_R + 0.294d0*VolOverAin*(MCp_Total + HA_R))))

       ZTJET(ZoneNum)%OutRoom=(ConvGainsJet*(-0.588d0*MCp_Total**2*VolOverAin + 0.294d0*MCp_Total*VolOverAin*HA_J +   &
                             2.d0*MCp_Total*HA_R + &
                             0.294d0*MCp_Total*VolOverAin*HA_R - HA_J*HA_R - 0.294d0*VolOverAin*HA_J*HA_R +   &
                             Exp((6.170068027210885d0* L*(1.d0 + 0.147d0*VolOverAin))/  &
                                (Sqrt(Ain(ZoneNum))*VolOverAin*Sigma))*(0.588d0*MCp_Total**2*VolOverAin - &
                             HA_J*HA_R + 2.d0*MCp_Total*(-0.147d0*VolOverAin*HA_J + HA_R + 0.147d0*VolOverAin*HA_R))) + &
                             2.d0*MCp_Total*(-0.294d0*ConvGainsRec*MCp_Total*VolOverAin + 0.294d0*ConvGainsRec*VolOverAin*HA_J -   &
                             0.294d0*MCp_Total* &
                             VolOverAin*HAT_J + HA_R*HAT_J - 0.294d0*MCp_Total*VolOverAin*HAT_R + 0.294d0* &
                             VolOverAin*HA_J*HAT_R - 0.294d0*MCp_Total**2*VolOverAin*Tin(ZoneNum) + 0.294d0*MCp_Total*VolOverAin* &
                             HA_J*Tin(ZoneNum) + MCp_Total*HA_R*Tin(ZoneNum) + 0.294d0*MCp_Total*VolOverAin*HA_R*Tin(ZoneNum) -  &
                             HA_J*HA_R*Tin(ZoneNum) - 0.294d0* &
                             VolOverAin*HA_J*HA_R*Tin(ZoneNum) + Exp((6.170068027210885d0*L*(1.d0 + 0.147d0*VolOverAin))/ &
                             (Sqrt(Ain(ZoneNum))* &
                             VolOverAin*Sigma))*(0.294d0*ConvGainsRec*VolOverAin*(MCp_Total - HA_J) +   &
                                0.294d0*MCp_Total*VolOverAin* &
                             HAT_J + HA_R*HAT_J + 0.294d0*VolOverAin*HA_R*HAT_J + 0.294d0*MCp_Total*VolOverAin* &
                             HAT_R - 0.294d0*VolOverAin*HA_J*HAT_R + (MCp_Total - HA_J)*(0.294d0*MCp_Total*VolOverAin + &
                             HA_R)*Tin(ZoneNum))))/(2.d0*MCp_Total**2*(-0.294d0*MCp_Total*VolOverAin + HA_R + &
                             Exp((6.170068027210885d0*L*(1.d0 + 0.147d0* &
                             VolOverAin))/(Sqrt(Ain(ZoneNum))*VolOverAin*Sigma))*(HA_R + 0.294d0*VolOverAin*(MCp_Total + HA_R))))


       ZTJET(ZoneNum)%Med= (ZTJET(ZoneNum)%In + ZTJET(ZoneNum)%Out)/2.d0

    END DO

    IF (JetRecAreaRatio(ZoneNum)==1.0) THEN
      ZoneCVhasREC(ZoneNum)=0
      ZTREC(ZoneNum)%In=ZTJET(ZoneNum)%Out
      ZTREC(ZoneNum)%Out=ZTJET(ZoneNum)%In
      ZTREC(ZoneNum)%Med=ZTJET(ZoneNum)%Med
    ENDIF

    ! If temperature increase is above 1.5C then go to mixing
    IF (ZTJET(ZoneNum)%OutRoom - Tin(ZoneNum) > 1.5d0) THEN
      ZoneCVisMixing(ZoneNum)=1.0
      ZoneCVhasREC(ZoneNum)=0
      AirModel(ZoneNum)%SimAirModel = .FALSE.
      Uhc(Zonenum)=0
      DO Ctd = 1,3
        ZTAveraged=mat(zonenum)
        ZTJET(ZoneNum)%OutRoom = ZTAveraged
        ZTJET(ZoneNum)%In = ZTAveraged
        ZTREC(ZoneNum)%In = ZTAveraged
        ZTJET(ZoneNum)%Out = ZTAveraged
        ZTREC(ZoneNum)%Out = ZTAveraged
        ZTJET(ZoneNum)%Med = ZTAveraged
        ZTREC(ZoneNum)%Med = ZTAveraged

        CALL HcUCSDCV(ZoneNum)

         ZTAveraged=mat(zonenum)
        ZTJET(ZoneNum)%OutRoom = ZTAveraged
        ZTJET(ZoneNum)%In = ZTAveraged
        ZTREC(ZoneNum)%In = ZTAveraged
        ZTJET(ZoneNum)%Out = ZTAveraged
        ZTREC(ZoneNum)%Out = ZTAveraged
        ZTJET(ZoneNum)%Med = ZTAveraged
        ZTREC(ZoneNum)%Med = ZTAveraged
       END DO
     END IF
  ELSE
  !=============================== M I X E D  Calculation ======================================================
  ZoneCVisMixing(ZoneNum)=1.0
  ZoneCVhasREC(ZoneNum)=0
  Uhc(Zonenum)=0

      DO Ctd = 1,3
        ZTAveraged=mat(zonenum)
        ZTJET(ZoneNum)%OutRoom = ZTAveraged
        ZTJET(ZoneNum)%In = ZTAveraged
        ZTREC(ZoneNum)%In = ZTAveraged
        ZTJET(ZoneNum)%Out = ZTAveraged
        ZTREC(ZoneNum)%Out = ZTAveraged
        ZTJET(ZoneNum)%Med = ZTAveraged
        ZTREC(ZoneNum)%Med = ZTAveraged

        CALL HcUCSDCV(ZoneNum)

        ZTAveraged=mat(zonenum)
        ZTJET(ZoneNum)%OutRoom = ZTAveraged
        ZTJET(ZoneNum)%In = ZTAveraged
        ZTREC(ZoneNum)%In = ZTAveraged
        ZTJET(ZoneNum)%Out = ZTAveraged
        ZTREC(ZoneNum)%Out = ZTAveraged
        ZTJET(ZoneNum)%Med = ZTAveraged
        ZTREC(ZoneNum)%Med = ZTAveraged
      END DO

  END IF
  !============================================================================================================

RETURN

END SUBROUTINE CalcUCSDCV

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

END MODULE CrossVentMgr
