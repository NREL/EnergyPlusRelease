#define EP_HBIRE_SEQ
#include "Timer.h"

MODULE HeatBalanceIntRadExchange
          ! Module containing the routines dealing with the interior radiant exchange
          ! between surfaces.

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Aug 2001, FW: recalculate ScriptF for a zone if window interior
          !                       shade/blind status is different from previous time step. This is
          !                       because ScriptF, which is used to calculate interior LW
          !                       exchange between surfaces, depends on inside surface emissivities,
          !                       which, for a window, depends on whether or not an interior
          !                       shade or blind is in place.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Part of the heat balance modularization/re-engineering.  Purpose of this
          ! module is to replace the MRT with RBAL method of modeling radiant exchange
          ! between interior surfaces.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology

          ! REFERENCES:
          ! ASHRAE Loads Toolkit "Script F" routines by Curt Pedersen
          ! Hottel, H.C., and A.F. Sarofim. "Radiative Transfer" (mainly chapter 3),
          !  McGraw-Hill, Inc., New York, 1967.

          ! OTHER NOTES: none

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalance
USE DataSurfaces
USE DataViewFactorInformation
USE DataInterfaces
USE DataTimings

IMPLICIT NONE   ! Enforce explicit typing of all variables

PRIVATE   ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS
character(len=*), PARAMETER :: fmtx='(A,I4,1x,A,1x,6f16.8)'
character(len=*), PARAMETER :: fmty='(A,1x,6f16.8)'

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: MaxNumOfZoneSurfaces  ! Max saved to get large enough space for user input view factors

          ! SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange
PUBLIC  CalcInteriorRadExchange
PRIVATE InitInteriorRadExchange
PRIVATE CalcApproximateViewFactors
PRIVATE FixViewFactors
PRIVATE CalcScriptF
PRIVATE CalcMatrixInverse

CONTAINS

SUBROUTINE CalcInteriorRadExchange(SurfaceTemp,SurfIterations,NetLWRadToSurf,ZoneToResimulate,calledfrom)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   September 2000
          !       MODIFIED       6/18/01, FCW: calculate IR on windows
          !                      Jan 2002, FCW: add blinds with movable slats
          !                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines the interior radiant exchange between surfaces using
          ! Hottel's ScriptF method for the grey interchange between surfaces
          ! in an enclosure.

          ! METHODOLOGY EMPLOYED:
          ! See reference

          ! REFERENCES:
          ! Hottel, H. C. and A. F. Sarofim, Radiative Transfer, Ch 3, McGraw Hill, 1967.

          ! USE STATEMENTS:
USE General, ONLY: InterpSlatAng        ! Function for slat angle interpolation
USE DataTimings
USE WindowEquivalentLayer,  ONLY: EQLWindowInsideEffectiveEmiss

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  REAL(r64), DIMENSION(:), INTENT(IN)  :: SurfaceTemp    ! Current surface temperatures
  INTEGER, INTENT(IN)               :: SurfIterations ! Number of iterations in calling subroutine
  REAL(r64), DIMENSION(:), INTENT(INOUT) :: NetLWRadToSurf ! Net long wavelength radiant exchange from other surfaces
  INTEGER, INTENT(IN), OPTIONAL          :: ZoneToResimulate  ! if passed in, then only calculate for this zone
  character(len=*), optional :: calledfrom

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: StefanBoltzmannConst = 5.6697d-8   ! Stefan-Boltzmann constant in W/(m2*K4)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: FirstTime = .TRUE.   ! Logical flag for one-time initializations
  INTEGER       :: RecSurfNum           ! Counter within DO loop (refers to main surface derived type index) RECEIVING SURFACE
  INTEGER       :: RecZoneSurfNum       ! DO loop counter for receiving surface within a zone (local derived type arrays)
  INTEGER       :: SendSurfNum          ! Counter within DO loop (refers to main surface derived type index) SENDING SURFACE

  INTEGER       :: SendZoneSurfNum      ! DO loop counter for sending surfaces within a zone (local derived type arrays)
  INTEGER       :: ZoneNum              ! DO loop counter for zones
  INTEGER       :: ConstrNumRec         ! Receiving surface construction number
  INTEGER       :: ConstrNumSend        ! Sending surface construction number
  REAL(r64)     :: RecSurfTemp          ! Receiving surface temperature (C)
  REAL(r64)     :: SendSurfTemp         ! Sending surface temperature (C)
  REAL(r64)     :: RecSurfEmiss         ! Inside surface emissivity
  INTEGER       :: ZoneSurfNum          ! Runs from 1 to number of surfaces in zone
  INTEGER       :: SurfNum              ! Surface number
  INTEGER       :: ConstrNum            ! Construction number
  LOGICAL       :: IntShadeOrBlindStatusChanged ! True if status of interior shade or blind on at least
                                                ! one window in a zone has changed from previous time step
  INTEGER       :: ShadeFlag            ! Window shading status current time step
  INTEGER       :: ShadeFlagPrev        ! Window shading status previous time step
  CHARACTER(len=158) :: tdstring

  !variables added as part of strategy to reduce calculation time - Glazer 2011-04-22
  REAL(r64)     :: SendSurfTempInKTo4th ! Sending surface temperature in K to 4th power
  REAL(r64)     :: RecSurfTempInKTo4th  ! Receiving surface temperature in K to 4th power
  REAL(r64),DIMENSION(:),ALLOCATABLE, SAVE :: SendSurfaceTempInKto4thPrecalc

          ! FLOW:

#ifdef EP_Detailed_Timings
                             CALL epStartTime('CalcInteriorRadExchange=')
#endif
  IF (FirstTime) THEN
    CALL InitInteriorRadExchange
#ifdef EP_HBIRE_SEQ
    ALLOCATE(SendSurfaceTempInKto4thPrecalc(MaxNumOfZoneSurfaces))
#else
    ALLOCATE(SendSurfaceTempInKto4thPrecalc(TotSurfaces))
#endif
    FirstTime = .FALSE.
     if (DeveloperFlag) then
       write(tdstring,*)' OMP turned off, HBIRE loop executed in serial'
       call DisplayString(trim(tdstring))
     endif
  END IF

if (kickoffsimulation .or. kickoffsizing) return

#ifdef EP_Count_Calls
if (.not. present(zonetoresimulate)) then
  NumIntRadExchange_Calls=NumIntRadExchange_Calls+1
else
  NumIntRadExchangeZ_Calls=NumIntRadExchangeZ_Calls+1
endif
if (calledfrom == 'Main') then
  NumIntRadExchangeMain_Calls=NumIntRadExchangeMain_Calls+1
elseif (calledfrom == 'Outside') then
  NumIntRadExchangeOSurf_Calls=NumIntRadExchangeOSurf_Calls+1
elseif (calledfrom == 'Inside') then
  NumIntRadExchangeISurf_Calls=NumIntRadExchangeISurf_Calls+1
endif
#endif

  ConstrNumRec=0
  IF (.NOT. PRESENT(ZoneToResimulate)) THEN
    NetLWRadToSurf = 0.0d0
    SurfaceWindow%IRfromParentZone = 0.0d0
  ENDIF

  DO ZoneNum = 1, NumOfZones

    IF (PRESENT(ZoneToResimulate)) THEN
      IF (ZoneNum /= ZoneToResimulate ) THEN
        CYCLE
      ELSE
        NetLWRadToSurf(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)                 = 0.d0
        SurfaceWindow(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%IRfromParentZone = 0.d0
      ENDIF
    ENDIF

    ! Calculate ScriptF if first time step in environment and surface heat-balance iterations not yet started;
    ! recalculate ScriptF if status of window interior shades or blinds has changed from
    ! previous time step. This recalculation is required since ScriptF depends on the inside
    ! emissivity of the inside surfaces, which, for windows, is (1) the emissivity of the
    ! inside face of the inside glass layer if there is no interior shade/blind, or (2) the effective
    ! emissivity of the shade/blind if the shade/blind is in place. (The "effective emissivity"
    ! in this case is (1) the shade/blind emissivity if the shade/blind IR transmittance is zero,
    ! or (2) a weighted average of the shade/blind emissivity and inside glass emissivity if the
    ! shade/blind IR transmittance is not zero (which is sometimes the case for a "shade" and
    ! usually the case for a blind). It assumed for switchable glazing that the inside surface
    ! emissivity does not change if the glazing is switched on or off.

    ! Determine if status of interior shade/blind on one or more windows in the zone has changed
    ! from previous time step.

    IF(SurfIterations == 0) THEN

      IntShadeOrBlindStatusChanged = .FALSE.

      IF(.NOT.BeginEnvrnFlag) THEN  ! Check for change in shade/blind status
        DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
          IF(IntShadeOrBlindStatusChanged) EXIT ! Need only check of one window's status has changed
          ConstrNum = Surface(SurfNum)%Construction
          IF(.NOT.Construct(ConstrNum)%TypeIsWindow) CYCLE
          ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
          ShadeFlagPrev = SurfaceWindow(SurfNum)%ExtIntShadePrevTS
          IF((ShadeFlagPrev /= IntShadeOn .AND. ShadeFlag == IntShadeOn).OR. &
             (ShadeFlagPrev /= IntBlindOn .AND. ShadeFlag == IntBlindOn).OR. &
             (ShadeFlagPrev == IntShadeOn .AND. ShadeFlag /= IntShadeOn).OR. &
             (ShadeFlagPrev == IntBlindOn .AND. ShadeFlag /= IntBlindOn))    &
            IntShadeOrBlindStatusChanged = .TRUE.
        END DO
      END IF

      IF(IntShadeOrBlindStatusChanged.OR.BeginEnvrnFlag) THEN  ! Calc inside surface emissivities for this time step
        DO ZoneSurfNum = 1,ZoneInfo(ZoneNum)%NumOfSurfaces
          SurfNum = ZoneInfo(ZoneNum)%SurfacePtr(ZoneSurfNum)
          ConstrNum = Surface(SurfNum)%Construction
          ZoneInfo(ZoneNum)%Emissivity(ZoneSurfNum) = Construct(ConstrNum)%InsideAbsorpThermal
          IF(Construct(ConstrNum)%TypeIsWindow.AND. &
             (SurfaceWindow(SurfNum)%ShadingFlag==IntShadeOn.OR.SurfaceWindow(SurfNum)%ShadingFlag==IntBlindOn)) THEN
            ZoneInfo(ZoneNum)%Emissivity(ZoneSurfNum) = &
                InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                   SurfaceWindow(SurfNum)%EffShBlindEmiss) + &
                InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                   SurfaceWindow(SurfNum)%EffGlassEmiss)
          END IF
        END DO

        CALL CalcScriptF(ZoneInfo(ZoneNum)%NumOfSurfaces, &
                         ZoneInfo(ZoneNum)%Area,          &
                         ZoneInfo(ZoneNum)%F,             &
                         ZoneInfo(ZoneNum)%Emissivity,    &
                         ZoneInfo(ZoneNum)%ScriptF)
        ! precalc - multiply by StefanBoltzmannConstant
        ZoneInfo(ZoneNum)%ScriptF=ZoneInfo(ZoneNum)%ScriptF*StefanBoltzmannConst
      END IF

    END IF  ! End of check if SurfIterations = 0

    ! precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
    DO SendZoneSurfNum = 1, ZoneInfo(ZoneNum)%NumOfSurfaces
      SendSurfNum = ZoneInfo(ZoneNum)%SurfacePtr(SendZoneSurfNum)
      ConstrNumSend = Surface(SendSurfNum)%Construction
      SendSurfTemp  = SurfaceTemp(SendSurfNum)
      IF(Construct(ConstrNumSend)%TypeIsWindow .AND.   &
         SurfaceWindow(SendSurfNum)%OriginalClass .NE. SurfaceClass_TDD_Diffuser .AND. &
         .NOT. Construct(ConstrNumSend)%WindowTypeEQL) THEN
        IF(SurfIterations == 0 .AND. SurfaceWindow(SendSurfNum)%ShadingFlag <= 0) THEN
          SendSurfTemp = SurfaceWindow(SendSurfNum)%ThetaFace(2*Construct(ConstrNumSend)%TotGlassLayers)-KelvinConv
        ELSE IF(SurfaceWindow(SendSurfNum)%ShadingFlag == IntShadeOn .OR.  &
          SurfaceWindow(SendSurfNum)%ShadingFlag == IntBlindOn) THEN
          SendSurfTemp = SurfaceWindow(SendSurfNum)%EffInsSurfTemp
       END IF
      ELSEIF (Construct(ConstrNumSend)%WindowTypeEQL) THEN
          SendSurfTemp = SurfaceWindow(SendSurfNum)%EffInsSurfTemp
      END IF
#ifdef EP_HBIRE_SEQ
      SendSurfaceTempInKto4thPrecalc(SendZoneSurfNum) = (SendSurfTemp+KelvinConv)**4
#else
      SendSurfaceTempInKto4thPrecalc(SendSurfNum) = (SendSurfTemp+KelvinConv)**4
#endif
    END DO

! these are the money do loops.
    DO RecZoneSurfNum = 1, ZoneInfo(ZoneNum)%NumOfSurfaces
      RecSurfNum = ZoneInfo(ZoneNum)%SurfacePtr(RecZoneSurfNum)
      ConstrNumRec = Surface(RecSurfNum)%Construction
      RecSurfTemp  = SurfaceTemp(RecSurfNum)
      RecSurfEmiss = Construct(ConstrNumRec)%InsideAbsorpThermal
      IF (Construct(ConstrNumRec)%TypeIsWindow .AND.   &
          SurfaceWindow(RecSurfNum)%OriginalClass .NE. SurfaceClass_TDD_Diffuser .AND. &
          .NOT. Construct(ConstrNumRec)%WindowTypeEQL ) THEN
        IF(SurfIterations == 0 .AND. SurfaceWindow(RecSurfNum)%ShadingFlag <= 0) THEN
            ! If the window is bare this TS and it is the first time through we use the previous TS glass
            ! temperature whether or not the window was shaded in the previous TS. If the window was shaded
            ! the previous time step this temperature is a better starting value than the shade temperature.
          RecSurfTemp = SurfaceWindow(RecSurfNum)%ThetaFace(2*Construct(ConstrNumRec)%TotGlassLayers)-KelvinConv
            ! For windows with an interior shade or blind an effective inside surface temp
            ! and emiss is used here that is a weighted combination of shade/blind and glass temp and emiss.
        ELSEIF (SurfaceWindow(RecSurfNum)%ShadingFlag==IntShadeOn .OR.      &
                SurfaceWindow(RecSurfNum)%ShadingFlag==IntBlindOn) THEN
          RecSurfTemp = SurfaceWindow(RecSurfNum)%EffInsSurfTemp
          RecSurfEmiss = &
              InterpSlatAng(SurfaceWindow(RecSurfNum)%SlatAngThisTS,SurfaceWindow(RecSurfNum)%MovableSlats, &
                 SurfaceWindow(RecSurfNum)%EffShBlindEmiss) + &
              InterpSlatAng(SurfaceWindow(RecSurfNum)%SlatAngThisTS,SurfaceWindow(RecSurfNum)%MovableSlats, &
                 SurfaceWindow(RecSurfNum)%EffGlassEmiss)
        END IF
      ELSEIF( Construct(ConstrNumRec)%WindowTypeEQL) THEN
        RecSurfEmiss = EQLWindowInsideEffectiveEmiss(ConstrNumRec)
        RecSurfTemp = SurfaceWindow(RecSurfNum)%EffInsSurfTemp
      END IF
      ! precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
      RecSurfTempInKTo4th = (RecSurfTemp+KelvinConv)**4
!      IF (ABS(RecSurfTempInKTo4th) > 1.d100) THEN
!        SendZoneSurfNum=1
!      ENDIF

      ! Calculate net long-wave radiation for opaque surfaces and incident
      ! long-wave radiation for windows.

      DO SendZoneSurfNum = 1, ZoneInfo(ZoneNum)%NumOfSurfaces
        SendSurfNum = ZoneInfo(ZoneNum)%SurfacePtr(SendZoneSurfNum)
!#ifdef EP_HBIRE_SEQ
!        SendSurfTempInKTo4th  = SendSurfaceTempInKto4thPrecalc(SendZoneSurfNum)
!#else
!        SendSurfTempInKTo4th  = SendSurfaceTempInKto4thPrecalc(SendSurfNum)
!#endif
        IF (RecZoneSurfNum /= SendZoneSurfNum) THEN
#ifdef EP_HBIRE_SEQ
          NetLWRadToSurf(RecSurfNum) = NetLWRadToSurf(RecSurfNum)   &
                                      +(ZoneInfo(ZoneNum)%ScriptF(RecZoneSurfNum,SendZoneSurfNum) &
                                      *(SendSurfaceTempInKto4thPrecalc(SendZoneSurfNum) - RecSurfTempInKTo4th))
#else
          NetLWRadToSurf(RecSurfNum) = NetLWRadToSurf(RecSurfNum)   &
                                      +(ZoneInfo(ZoneNum)%ScriptF(RecZoneSurfNum,SendZoneSurfNum) &
                                      *(SendSurfaceTempInKto4thPrecalc(SendSurfNum) - RecSurfTempInKTo4th))
#endif
        ENDIF
        IF(Construct(ConstrNumRec)%TypeIsWindow)  THEN    ! Window
           ! Calculate interior LW incident on window rather than net LW for use in window layer
           ! heat balance calculation.
#ifdef EP_HBIRE_SEQ
          SurfaceWindow(RecSurfNum)%IRfromParentZone = SurfaceWindow(RecSurfNum)%IRfromParentZone + &
                                      (ZoneInfo(ZoneNum)%ScriptF(RecZoneSurfNum,SendZoneSurfNum) &
                                      * SendSurfaceTempInKto4thPrecalc(SendZoneSurfNum)) / RecSurfEmiss
#else
          SurfaceWindow(RecSurfNum)%IRfromParentZone = SurfaceWindow(RecSurfNum)%IRfromParentZone + &
                                      (ZoneInfo(ZoneNum)%ScriptF(RecZoneSurfNum,SendZoneSurfNum) &
                                      * SendSurfaceTempInKto4thPrecalc(SendSurfNum)) / RecSurfEmiss
#endif
! Per BG -- this should never happened.  (CR6346,CR6550 caused this to be put in.  Now removed. LKL 1/2013)
!          IF (SurfaceWindow(RecSurfNum)%IRfromParentZone < 0.0) THEN
!            CALL ShowRecurringWarningErrorAtEnd('CalcInteriorRadExchange: Window_IRFromParentZone negative, Window="'// &
!                TRIM(Surface(RecSurfNum)%Name)//'"',  &
!                SurfaceWindow(RecSurfNum)%IRErrCount)
!            CALL ShowRecurringContinueErrorAtEnd('..occurs in Zone="'//TRIM(Surface(RecSurfNum)%ZoneName)//  &
!                '", reset to 0.0 for remaining calculations.',SurfaceWindow(RecSurfNum)%IRErrCountC)
!            SurfaceWindow(RecSurfNum)%IRfromParentZone=0.0
!          ENDIF
        ENDIF
      END DO
    END DO
  END DO

#ifdef EP_Detailed_Timings
                             CALL epStopTime('CalcInteriorRadExchange=')
#endif
  RETURN

END SUBROUTINE CalcInteriorRadExchange

SUBROUTINE InitInteriorRadExchange

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the various parameters for Hottel's ScriptF method for
          ! the grey interchange between surfaces in an enclosure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,GetObjectDefMaxArgs
  USE General, ONLY: RoundSigDigits,ScanForReports

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumOfZoneSurfaces  ! total number of surfaces in the zone.
  INTEGER :: SurfNum            ! Counter within DO loop (refers to main surface derived type index)
  INTEGER :: ZoneNum            ! DO loop counter for zones
  INTEGER :: ZoneSurfNum        ! DO loop counter for surfaces within a zone (refers to local derived type arrays)
  INTEGER :: Findex             !  index to print view factors
  INTEGER :: Vindex             !  index for vertices
  INTEGER :: NumZonesWithUserFbyS  !  Zones with user input,  used for flag here
  LOGICAL :: NoUserInputF       !  Logical flag signifying no input F's for zone
  LOGICAL,SAVE :: ViewFactorReport   !  Flag to output view factor report in eio file
  LOGICAL :: ErrorsFound=.false.
  REAL(r64) :: CheckValue1
  REAL(r64) :: CheckValue2
  REAL(r64) :: FinalCheckValue
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: SaveApproximateViewFactors   ! Save for View Factor reporting
  REAL(r64) :: RowSum
  REAL(r64) :: FixedRowSum
  INTEGER  :: NumIterations
  CHARACTER(len=MaxNameLength) :: Option1  ! view factor report option

          ! FLOW:

   ALLOCATE(ZoneInfo(NumOfZones))  ! Allocate the entire derived type

   CALL ScanForReports('ViewFactorInfo',ViewFactorReport,Option1=Option1)

   IF (ViewFactorReport) THEN  ! Print heading
     WRITE(OutputFileInits,'(A)')'! <Surface View Factor and Grey Interchange Information>'
     WRITE(OutputFileInits,'(A)')'! <View Factor - Zone Information>,Zone Name,Number of Surfaces'
     WRITE(OutputFileInits,'(A)')'! <View Factor - Surface Information>,Surface Name,Surface Class,Area {m2},Azimuth,'// &
             'Tilt,Thermal Emissivity,#Sides,Vertices'
     WRITE(OutputFileInits,'(A)')'! <View Factor / Grey Interchange Type>,Surface Name(s)'
     WRITE(OutputFileInits,'(A)')'! <View Factor>,Surface Name,Surface Class,Row Sum,View Factors for each Surface'
   END IF

   cCurrentModuleObject='ZoneProperty:UserViewFactors:bySurfaceName'
   NumZonesWithUserFbyS = GetNumObjectsFound(cCurrentModuleObject)


   MaxNumOfZoneSurfaces = 0
   DO ZoneNum = 1, NumOfZones

     IF (ZoneNum == 1) THEN
       IF (DisplayAdvancedReportVariables)   &
         WRITE(OutputFileInits,'(A)')'! <Surface View Factor Check Values>,Zone Name,Original Check Value,'//  &
            'Calculated Fixed Check Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence,'//  &
            'Used RowSum Convergence'
     ENDIF

     ZoneInfo(ZoneNum)%Name = Zone(ZoneNum)%Name

     NumOfZoneSurfaces =0
     DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
       IF (Surface(SurfNum)%HeatTransSurf) NumOfZoneSurfaces = NumOfZoneSurfaces + 1
     END DO
     ZoneInfo(ZoneNum)%NumOfSurfaces = NumOfZoneSurfaces
     MaxNumOfZoneSurfaces=MAX(MaxNumOfZoneSurfaces,NumOfZoneSurfaces)
     IF (ZoneInfo(ZoneNum)%NumOfSurfaces < 1) CALL ShowFatalError('No surfaces in a zone in InitInteriorRadExchange')

          ! Allocate the parts of the derived type
     ALLOCATE(ZoneInfo(ZoneNum)%F(ZoneInfo(ZoneNum)%NumOfSurfaces,ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%F=0.0d0
     ALLOCATE(ZoneInfo(ZoneNum)%ScriptF(ZoneInfo(ZoneNum)%NumOfSurfaces,ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%ScriptF=0.0d0
     ALLOCATE(ZoneInfo(ZoneNum)%Area(ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%Area=0.0d0
     ALLOCATE(ZoneInfo(ZoneNum)%Emissivity(ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%Emissivity=0.0d0
     ALLOCATE(ZoneInfo(ZoneNum)%Azimuth(ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%Azimuth=0.0d0
     ALLOCATE(ZoneInfo(ZoneNum)%Tilt(ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%Tilt=0.0d0
     ALLOCATE(ZoneInfo(ZoneNum)%SurfacePtr(ZoneInfo(ZoneNum)%NumOfSurfaces))
     ZoneInfo(ZoneNum)%SurfacePtr=0

          ! Initialize the surface pointer array
     ZoneSurfNum = 0
     DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
       IF (.NOT.Surface(SurfNum)%HeatTransSurf) CYCLE
       ZoneSurfNum = ZoneSurfNum + 1
       ZoneInfo(ZoneNum)%SurfacePtr(ZoneSurfNum) = SurfNum
     END DO
          ! Initialize the area and emissivity arrays
     DO ZoneSurfNum = 1, ZoneInfo(ZoneNum)%NumOfSurfaces
       SurfNum = ZoneInfo(ZoneNum)%SurfacePtr(ZoneSurfNum)

!************************************************
       IF (.not. Construct(Surface(SurfNum)%Construction)%TypeIsIRT) Then
            ZoneInfo(ZoneNum)%Area(ZoneSurfNum)       = Surface(SurfNum)%Area
        ELSE
           ! Double area for infrared transparent (IRT) surfaces
           ZoneInfo(ZoneNum)%Area(ZoneSurfNum)       = 2.0d0* Surface(SurfNum)%Area
        END IF
!***********************************************

       ZoneInfo(ZoneNum)%Emissivity(ZoneSurfNum) = Construct(Surface(SurfNum)%Construction)%InsideAbsorpThermal
       ZoneInfo(ZoneNum)%Azimuth(ZoneSurfNum)    = Surface(SurfNum)%Azimuth
       ZoneInfo(ZoneNum)%Tilt(ZoneSurfNum)       = Surface(SurfNum)%Tilt
     END DO

     IF (ZoneInfo(ZoneNum)%NumOfSurfaces == 1) THEN
          ! If there is only one surface in a zone, then there is no radiant exchange
       ZoneInfo(ZoneNum)%F       = 0.0d0
       ZoneInfo(ZoneNum)%ScriptF = 0.0d0
       IF (DisplayAdvancedReportVariables)   &
          WRITE(OutputFileInits,'(A)')'Surface View Factor Check Values,'//trim(Zone(ZoneNum)%Name)//  &
             ',0,0,0,-1,0,0'
       CYCLE ! Go to the next zone in the  ZoneNum DO loop
     END IF



    !  Get user supplied view factors if available in idf.

     NoUserInputF=.true.

     IF (NumZonesWithUserFbyS > 0) THEN

       CALL GetInputViewFactorsbyName(ZoneInfo(ZoneNum)%Name,       &   ! Obtains user input view factors from input file
                             ZoneInfo(ZoneNum)%NumOfSurfaces, &
                             ZoneInfo(ZoneNum)%F,             &
                             ZoneInfo(ZoneNum)%SurfacePtr,    &
                             NoUserInputF,                    &
                             ErrorsFound)
     ENDIF

     IF (NoUserInputF) Then

         ! Calculate the view factors and make sure they satisfy reciprocity
         CALL CalcApproximateViewFactors(ZoneInfo(ZoneNum)%NumOfSurfaces, &
                                    ZoneInfo(ZoneNum)%Area,          &
                                    ZoneInfo(ZoneNum)%Azimuth,       &
                                    ZoneInfo(ZoneNum)%Tilt,          &
                                    ZoneInfo(ZoneNum)%F,             &
                                    ZoneInfo(ZoneNum)%SurfacePtr)
     END IF

     IF (ViewFactorReport) THEN  ! Allocate and save user or approximate view factors for reporting.
       ALLOCATE(SaveApproximateViewFactors(ZoneInfo(ZoneNum)%NumOfSurfaces,ZoneInfo(ZoneNum)%NumOfSurfaces))
       SaveApproximateViewFactors=ZoneInfo(ZoneNum)%F
     ENDIF


     CALL FixViewFactors(ZoneInfo(ZoneNum)%NumOfSurfaces, &
                         ZoneInfo(ZoneNum)%Area,          &
                         ZoneInfo(ZoneNum)%F,ZoneNum,     &
                         CheckValue1,CheckValue2,FinalCheckValue,NumIterations,FixedRowSum)

          ! Calculate the script F factors
     CALL CalcScriptF(ZoneInfo(ZoneNum)%NumOfSurfaces, &
                      ZoneInfo(ZoneNum)%Area,          &
                      ZoneInfo(ZoneNum)%F,             &
                      ZoneInfo(ZoneNum)%Emissivity,    &
                      ZoneInfo(ZoneNum)%ScriptF)



     IF (ViewFactorReport) THEN  ! Write to SurfInfo File
       ! Zone Surface Information Output
       WRITE(OutputFileInits, '(A)') 'Surface View Factor - Zone Information,'//trim(ZoneInfo(ZoneNum)%Name)//  &
          ','//trim(RoundSigDigits(ZoneInfo(ZoneNum)%NumOfSurfaces))

       DO SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces
         WRITE(OutputFileInits,'(A,200(",",A))') 'Surface View Factor - Surface Information,'//  &
            trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name)//','//  &
             trim(cSurfaceClass(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Class)),  &
            trim(RoundSigDigits(ZoneInfo(ZoneNum)%Area(SurfNum),4))//','//  &
            trim(RoundSigDigits(ZoneInfo(ZoneNum)%Azimuth(SurfNum),4))//','//  &
            trim(RoundSigDigits(ZoneInfo(ZoneNum)%Tilt(SurfNum),4))//','//  &
            trim(RoundSigDigits(ZoneInfo(ZoneNum)%Emissivity(SurfNum),4))//','//  &
            trim(RoundSigDigits(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%sides)),  &
            (trim(RoundSigDigits(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%vertex(vindex)%X,4)),  &
             trim(RoundSigDigits(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%vertex(vindex)%Y,4)),  &
             trim(RoundSigDigits(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%vertex(vindex)%Z,4)),  &
                vindex=1,Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%sides)
       END DO

       WRITE(OutputFileInits,'(A,A,200(",",A))') 'Approximate or User Input ViewFactors',  &
           ',To Surface,Surface Class,RowSum',  &
          (trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name),SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces)

       DO Findex = 1 , ZoneInfo(ZoneNum)%NumOfSurfaces
         RowSum=SUM(SaveApproximateViewFactors(Findex,:))
         WRITE(OutputFileInits, '(A,",",A,",",A,200(",",A))')  &
           'View Factor',  &
           trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name),  &
           trim(cSurfaceClass(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Class)),  &
           trim(RoundSigDigits(RowSum,4)), &
           (trim(RoundSigDigits(SaveApproximateViewFactors(Findex,SurfNum),4)),SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces)
       END DO

     ENDIF

     IF (ViewFactorReport) THEN
       WRITE(OutputFileInits,'(A,A,200(",",A))') 'Final ViewFactors',  &
           ',To Surface,Surface Class,RowSum',  &
          (trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name),SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces)

       DO Findex = 1 , ZoneInfo(ZoneNum)%NumOfSurfaces
         RowSum=SUM(ZoneInfo(ZoneNum)%F(Findex,:))
         WRITE(OutputFileInits, '(A,",",A,",",A,200(",",A))')  &
           'View Factor',  &
           trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name),  &
           trim(cSurfaceClass(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Class)),  &
           trim(RoundSigDigits(RowSum,4)), &
           (trim(RoundSigDigits(ZoneInfo(ZoneNum)%F(Findex,SurfNum),4)),SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces)
       END DO

       IF (Option1 == 'IDF') THEN
         WRITE(OutputFileDebug,'(A)') '!======== original input factors ==========================='
         WRITE(OutputFileDebug,'(A)')  'ZoneProperty:UserViewFactors:bySurfaceName,'//trim(ZoneInfo(ZoneNum)%Name)//','
         DO SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces
           DO Findex=1,ZoneInfo(ZoneNum)%NumOfSurfaces
             IF (.not. (SurfNum == ZoneInfo(ZoneNum)%NumOfSurfaces .and. Findex == ZoneInfo(ZoneNum)%NumOfSurfaces) ) THEN
               WRITE(OutputFileDebug,'(A)') '  '//trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name)//','//  &
                  trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name)//','//  &
                  trim(RoundSigDigits(ZoneInfo(ZoneNum)%F(SurfNum,Findex),6))//','
             ELSE
               WRITE(OutputFileDebug,'(A)') '  '//trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name)//','//  &
                  trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name)//','//  &
                  trim(RoundSigDigits(ZoneInfo(ZoneNum)%F(SurfNum,Findex),6))//';'
             ENDIF
           ENDDO
         ENDDO
         WRITE(OutputFileDebug,'(A)') '!============= end of data ======================'

         WRITE(OutputFileDebug,'(A)') '!============ final view factors ======================='
         WRITE(OutputFileDebug,'(A)')  'ZoneProperty:UserViewFactors:bySurfaceName,'//trim(ZoneInfo(ZoneNum)%Name)//','
         DO SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces
           DO Findex=1,ZoneInfo(ZoneNum)%NumOfSurfaces
             IF (.not. (SurfNum == ZoneInfo(ZoneNum)%NumOfSurfaces .and. Findex == ZoneInfo(ZoneNum)%NumOfSurfaces) ) THEN
               WRITE(OutputFileDebug,'(A)') '  '//trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name)//','//  &
                  trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name)//','//  &
                  trim(RoundSigDigits(ZoneInfo(ZoneNum)%F(SurfNum,Findex),6))//','
             ELSE
               WRITE(OutputFileDebug,'(A)') '  '//trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name)//','//  &
                  trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name)//','//  &
                  trim(RoundSigDigits(ZoneInfo(ZoneNum)%F(SurfNum,Findex),6))//';'
             ENDIF
           ENDDO
         ENDDO
         WRITE(OutputFileDebug,'(A)') '!============= end of data ======================'
       END IF

     END IF

     IF (ViewFactorReport) THEN
       WRITE(OutputFileInits,'(A,A,200(",",A))') 'Script F Factors',  &
           ',X Surface',  &
          (trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(SurfNum))%Name),SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces)

       DO Findex = 1 , ZoneInfo(ZoneNum)%NumOfSurfaces
         WRITE(OutputFileInits, '(A,",",A,200(",",A))')  &
           'Script F Factor',  &
           trim(Surface(ZoneInfo(ZoneNum)%SurfacePtr(Findex))%Name),  &
           (trim(RoundSigDigits(ZoneInfo(ZoneNum)%ScriptF(Findex,SurfNum),4)),SurfNum=1,ZoneInfo(ZoneNum)%NumOfSurfaces)
       END DO

     END IF

     IF (ViewFactorReport) THEN  ! Deallocate saved approximate/user view factors
       DEALLOCATE(SaveApproximateViewFactors)
     ENDIF

     RowSum=0.0d0
     DO Findex = 1 , ZoneInfo(ZoneNum)%NumOfSurfaces
       RowSum=RowSum+SUM(ZoneInfo(ZoneNum)%F(Findex,:))
     END DO
     RowSum=ABS(RowSum-ZoneInfo(ZoneNum)%NumOfSurfaces)
     FixedRowSum=ABS(FixedRowSum-ZoneInfo(ZoneNum)%NumOfSurfaces)
     IF (DisplayAdvancedReportVariables)   &
        WRITE(OutputFileInits,'(A)')'Surface View Factor Check Values,'//trim(Zone(ZoneNum)%Name)//','//  &
          trim(RoundSigDigits(CheckValue1,6))//','//trim(RoundSigDigits(CheckValue2,6))//','//  &
          trim(RoundSigDigits(FinalCheckValue,6))//','//trim(RoundSigDigits(NumIterations))//','//  &
          trim(RoundSigDigits(FixedRowSum,6))//','//trim(RoundSigDigits(RowSum,6))

   END DO

   IF (ErrorsFound) THEN
     CALL ShowFatalError('InitInteriorRadExchange: Errors found during initialization of radiant exchange.  Program terminated.')
   ENDIF


  RETURN

END SUBROUTINE InitInteriorRadExchange

SUBROUTINE GetInputViewFactors(ZoneName,N,F,SPtr,NoUserInputF,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Curt Pedersen
          !       DATE WRITTEN   September 2005
          !       MODIFIED       Linda Lawrie;September 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the user view factor info.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts
USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,GetObjectItemNum
USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(Len=*), INTENT(IN)          :: ZoneName  !  Needed to check for user input view factors.
  INTEGER, INTENT (IN)                  :: N    ! NUMBER OF SURFACES
  REAL(r64), INTENT (OUT), DIMENSION(N,N) :: F    ! USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
  INTEGER, INTENT (IN),  DIMENSION(N)   :: SPtr ! pointer to actual surface number
  LOGICAL, INTENT (OUT)                 :: NoUserInputF ! Flag signifying no input F's for this
  LOGICAL, INTENT (INOUT)               :: ErrorsFound  ! True when errors are found in number of fields vs max args
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER   :: NumZonesWithUserF
  INTEGER   :: UserFZoneIndex
  INTEGER   :: NumAlphas
  INTEGER   :: NumNums
  INTEGER   :: IOStat
  INTEGER   :: index
  INTEGER :: inx1
  INTEGER :: inx2
!unused  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneSurfaceNames

  NoUserInputF = .true.
  UserFZoneIndex=GetObjectItemNum('ZoneProperty:UserViewFactors',ZoneName)

  IF (UserFZoneIndex > 0) THEN
    NoUserInputF = .false.

    CALL GetObjectItem('ZoneProperty:UserViewFactors',UserFZoneIndex,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT,  &
                AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IF (NumNums < 3*(N)**2) THEN
      CALL ShowSevereError('GetInputViewFactors: '//trim(cCurrentModuleObject)//  &
         '="'//trim(ZoneName)//'", not enough values.')
      CALL ShowContinueError('...Number of input values ['//  &
         TRIM(TrimSigDigits(NumNums))//'] is less than the required number=['//  &
         TRIM(TrimSigDigits(3*(N)**2))//'].')
      ErrorsFound=.true.
      NumNums=0
    ENDIF
    F = 0.0d0
    DO index = 1, NumNums,3
      inx1=rNumericArgs(index)
      inx2=rNumericArgs(index+1)
      F(inx1,inx2)=rNumericArgs(index+2)
    END DO
  END IF

END SUBROUTINE GetInputViewFactors


SUBROUTINE GetInputViewFactorsbyName(ZoneName,N,F,SPtr,NoUserInputF,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Curt Pedersen
          !       DATE WRITTEN   September 2005
          !       MODIFIED       Linda Lawrie;September 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the user view factor info.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts
USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,GetObjectItemNum,FindItemInList
USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(Len=*), INTENT(IN)          :: ZoneName  !  Needed to check for user input view factors.
  INTEGER, INTENT (IN)                  :: N    ! NUMBER OF SURFACES
  REAL(r64), INTENT (OUT), DIMENSION(N,N) :: F    ! USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
  INTEGER, INTENT (IN),  DIMENSION(N)   :: SPtr ! pointer to actual surface number
  LOGICAL, INTENT (OUT)                 :: NoUserInputF ! Flag signifying no input F's for this
  LOGICAL, INTENT (INOUT)               :: ErrorsFound  ! True when errors are found in number of fields vs max args
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: UserFZoneIndex
  INTEGER   :: NumAlphas
  INTEGER   :: NumNums
  INTEGER   :: IOStat
  INTEGER   :: index
  INTEGER   :: numinx1
  INTEGER   :: inx1
  INTEGER   :: inx2
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneSurfaceNames

  NoUserInputF = .true.
  UserFZoneIndex=GetObjectItemNum('ZoneProperty:UserViewFactors:bySurfaceName',ZoneName)

  IF (UserFZoneIndex > 0) THEN
    ALLOCATE(ZoneSurfaceNames(N))
    DO index=1,N
      ZoneSurfaceNames(index)=Surface(SPtr(index))%Name
    ENDDO
    NoUserInputF = .false.

    CALL GetObjectItem('ZoneProperty:UserViewFactors:bySurfaceName',UserFZoneIndex,cAlphaArgs,NumAlphas,  &
                rNumericArgs,NumNums,IOSTAT,  &
                AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IF (NumNums < N**2) THEN
      CALL ShowSevereError('GetInputViewFactors: '//trim(cCurrentModuleObject)//  &
         '="'//trim(ZoneName)//'", not enough values.')
      CALL ShowContinueError('...Number of input values ['//  &
         TRIM(TrimSigDigits(NumNums))//'] is less than the required number=['//  &
         TRIM(TrimSigDigits(N**2))//'].')
      ErrorsFound=.true.
      NumNums=0   ! cancel getting any coordinates
    ENDIF
    F = 0.0d0
    numinx1=0

    DO index = 2, NumAlphas,2
      inx1=FindItemInList(cAlphaArgs(index),ZoneSurfaceNames,N)
      inx2=FindItemInList(cAlphaArgs(index+1),ZoneSurfaceNames,N)
      IF (inx1 == 0) THEN
        CALL ShowSevereError('GetInputViewFactors: '//trim(cCurrentModuleObject)//  &
           '="'//trim(ZoneName)//'", invalid surface name.')
        CALL ShowContinueError('...Surface name="'//trim(cAlphaArgs(index))//'", not in this zone.')
        ErrorsFound=.true.
      ENDIF
      IF (inx2 == 0) THEN
        CALL ShowSevereError('GetInputViewFactors: '//trim(cCurrentModuleObject)//  &
           '="'//trim(ZoneName)//'", invalid surface name.')
        CALL ShowContinueError('...Surface name="'//trim(cAlphaArgs(index+2))//'", not in this zone.')
        ErrorsFound=.true.
      ENDIF
      numinx1=numinx1+1
      IF (inx1 > 0 .and. inx2 > 0) F(inx1,inx2)=rNumericArgs(numinx1)
    END DO
    DEALLOCATE(ZoneSurfaceNames)
  END IF

END SUBROUTINE GetInputViewFactorsbyName

SUBROUTINE CalcApproximateViewFactors(N,A,Azimuth,Tilt,F,SPtr)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Curt Pedersen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       March 2001 (RKS) to disallow surfaces facing the same direction to interact radiatively
          !                      May 2002 (COP) to include INTMASS, FLOOR, ROOF and CEILING.
          !       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine approximates view factors using an area weighting.
          ! This is improved by one degree by not allowing surfaces facing the same
          ! direction to "see" each other.

          ! METHODOLOGY EMPLOYED:
          ! Each surface sees some area of other surfaces within the zone.  The view
          ! factors from the surface to the other seen surfaces are defined by their
          ! area over the summed area of seen surfaces.  Surfaces facing the same angle
          ! are assumed to not be able to see each other.
          !  Modified May 2002 to cover poorly defined surface orientation.  Now all thermal masses, roofs and
          !  ceilings are "seen" by other surfaces. Floors are seen by all other surfaces, but
          !  not by other floors.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  INTEGER, INTENT (IN)                  :: N    ! NUMBER OF SURFACES
  REAL(r64), INTENT (IN),  DIMENSION(N)   :: A    ! AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
  REAL(r64), INTENT (IN),  DIMENSION(N)   :: Azimuth ! Facing angle of the surface (in degrees)
  REAL(r64), INTENT (IN),  DIMENSION(N)   :: Tilt ! Tilt angle of the surface (in degrees)
  REAL(r64), INTENT (OUT), DIMENSION(N,N) :: F    ! APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
  INTEGER, INTENT (IN),  DIMENSION(N)   :: SPtr ! pointer to REAL(r64) surface number (for error message)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SameAngleLimit = 10.0d0 ! If the difference in the azimuth angles are above this value (degrees),
                                          ! then the surfaces are assumed to be facing different directions.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: I, J       ! DO loop counters for surfaces in the zone
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneArea   ! Sum of the area of all zone surfaces seen

          ! FLOW:
          ! Calculate the sum of the areas seen by all zone surfaces
  ALLOCATE(ZoneArea(N))
  ZoneArea = 0.0d0
  DO I = 1, N
    DO J = 1, N
          ! Assumption is that a surface cannot see itself or any other surface
          ! that is facing the same direction (has the same azimuth)
          !  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
          !  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
          !
          !  Skip same surface
          !
      IF (I== J) cycle
         !
         !  Include INTMASS, FLOOR(for others), CEILING, ROOF  and different facing surfaces.
         !
      IF ( (Surface(SPtr(J))%Class == SurfaceClass_IntMass) .OR.     &  ! Everything sees internal mass surfaces
           (Surface(SPtr(J))%Class == SurfaceClass_Floor)  .OR.        &  ! Everything except other floors sees floors
           (Surface(SPtr(J))%Class == SurfaceClass_Roof .AND. Surface(SPtr(I))%Class == SurfaceClass_Floor)  .OR.  &
                       !  Roofs/ceilings always see floors
           ((ABS(Azimuth(I)-Azimuth(J)) > SameAngleLimit).OR. (ABS(Tilt(I)-Tilt(J)) > SameAngleLimit)  ) ) THEN

         ZoneArea(I) = ZoneArea(I) + A(J)

      END IF
    END DO
    IF (ZoneArea(I) <= 0.0d0) THEN
      CALL ShowWarningError('CalcApproximateViewFactors: Zero area for all other zone surfaces.')
      CALL ShowContinueError('Happens for Surface="'//TRIM(Surface(SPtr(I))%Name)//'" in Zone='//  &
                             TRIM(Zone(Surface(SPtr(I))%Zone)%Name))
    ENDIF
  END DO


          ! Set up the approximate view factors.  First these are initialized to all zero.
          ! This will clear out any junk leftover from whenever.  Then, for each zone
          ! surface, set the view factor from that surface to other surfaces as the
          ! area of the other surface divided by the sum of the area of all zone surfaces
          ! that the original surface can actually see (calculated above).  This will
          ! allow that the sum of all view factors from the original surface to all other
          ! surfaces will equal unity.  F(I,J)=0 if I=J or if the surfaces face the same
          ! direction.
          !
          !  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
          !  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
          !
          ! The second IF statement is intended to avoid a divide by zero if
          ! there are no other surfaces in the zone that can be seen.
  F = 0.0d0
  DO I = 1, N
    DO J = 1, N

         !  Skip same surface

       IF (I== J) cycle
         !
         !  Include INTMASS, FLOOR(for others), CEILING/ROOF  and different facing surfaces.
         !
       IF ( (Surface(SPtr(J))%Class == SurfaceClass_IntMass) .OR.   &
            (Surface(SPtr(J))%Class == SurfaceClass_Floor)  .OR.        &
            (Surface(SPtr(J))%Class == SurfaceClass_Roof)  .OR.        &
            ((ABS(Azimuth(I)-Azimuth(J)) > SameAngleLimit).OR. &
            (ABS(Tilt(I)-Tilt(J)) > SameAngleLimit)          ) ) THEN
         IF (ZoneArea(I) > 0.0d0) F(I,J) = A(J)/(ZoneArea(I))
       END IF




    END DO
  END DO

  DEALLOCATE(ZoneArea)

  RETURN

END SUBROUTINE CalcApproximateViewFactors


SUBROUTINE FixViewFactors(N,A,F,ZoneNum,OriginalCheckValue,FixedCheckValue,FinalCheckValue,NumIterations,RowSum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Curt Pedersen
          !       DATE WRITTEN   July 2000
          !       MODIFIED       September 2000 (RKS for EnergyPlus)
          !                      April 2005,COP added capability to handle a
          !                      surface larger than sum of all others (nonenclosure)
          !                      by using a Fii view factor for that surface. Process is
          !                      now much more robust and stable.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine fixes approximate view factors and enforces reciprocity
          ! and completeness.

          ! METHODOLOGY EMPLOYED:
          ! A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)
          ! Subroutine takes approximate view factors and enforces reciprocity by
          ! averaging AiFij and AjFji.  Then it determines a set of row coefficients
          ! which can be multipled by each AF product to force the sum of AiFij for
          ! each row to equal Ai, and applies them. Completeness is checked, and if
          ! not satisfied, the AF averaging and row modifications are repeated until
          ! completeness is within a preselected small deviation from 1.0
          ! The routine also checks the number of surfaces and if N<=3, just enforces reciprocity.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  INTEGER, INTENT (IN)                       :: N        ! NUMBER OF SURFACES
  REAL(r64), INTENT (IN),     DIMENSION(N)   :: A        ! AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
  REAL(r64), INTENT (INOUT),  DIMENSION(N,N) :: F        ! APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
  INTEGER, INTENT (IN)                       :: ZoneNum  ! Zone number being fixe
  REAL(r64), INTENT (INOUT) :: OriginalCheckValue        ! check of SUM(F) - N
  REAL(r64), INTENT (INOUT) :: FixedCheckValue           ! check after fixed of SUM(F) - N
  REAL(r64), INTENT (INOUT) :: FinalCheckValue           ! the one to go with
  INTEGER, INTENT (INOUT)   :: NumIterations             ! number of iterations to fixed
  REAL(r64), INTENT (INOUT) :: RowSum                    ! RowSum of Fixed

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: PrimaryConvergence   =0.001d0
  REAL(r64), PARAMETER :: DifferenceConvergence=0.00001d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AF             ! = (AREA * DIRECT VIEW FACTOR) MATRIX
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AFTranspose
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: AFAverage
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: FixedAF
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: FixedF         ! CORRECTED MATRIX OF VIEW FACTORS (N X N)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: FixedAFTranspose
  REAL(r64), ALLOCATABLE, DIMENSION(:)   :: RowCoefficient
  REAL(r64) :: LargestArea
  REAL(r64) :: ConvrgNew
  REAL(r64) :: ConvrgOld
  REAL(r64) :: Accelerator   !  RowCoefficient multipler to accelerate convergence
  REAL(r64) :: CheckConvergeTolerance  ! check value for actual warning

  LOGICAL :: Converged
  INTEGER :: I
  INTEGER :: J
  INTEGER :: LargestSurf=0

          ! FLOW:
  OriginalCheckValue=ABS(SUM(F)-N)

          !  Allocate and zero arrays
  ALLOCATE(AF(N,N))
  ALLOCATE(AFTranspose(N,N))
  ALLOCATE(AFAverage(N,N))
  ALLOCATE(FixedAF(N,N))
  ALLOCATE(FixedAFTranspose(N,N))

  AF          = 0.0d0
  AFTranspose = 0.0d0
  FixedAF     = 0.0d0
  Accelerator = 1.0d0
  ConvrgOld = 10.0d0
  LargestArea=MAXVAL(A)

  FixedAF=F   ! store for largest area check

!  Check for Strange Geometry
  IF (LargestArea > (SUM(A)-LargestArea)) THEN
       DO I=1,N
         IF (LargestArea /= A(I)) CYCLE
         LargestSurf=I
         EXIT
       END DO
       FixedAF(LargestSurf,LargestSurf)=MIN(0.9D0,1.2d0*LargestArea/SUM(A))  ! Give self view to big surface
  END IF

          !  Set up AF matrix.
  DO I=1,N
     DO J = 1,N
      AF(I,J)=FixedAF(I,J)*A(I)
    END DO
  END DO

          !  Enforce reciprocity by averaging AiFij and AjFji
  AFTranspose = TRANSPOSE(AF)
  AFAverage   = 0.5d0*(AF+AFTranspose)

  FixedAF=AFAverage  !Initialize Fixed Matrix

  DEALLOCATE(AF)
  DEALLOCATE(AFTranspose)
  DEALLOCATE(AFAverage)

  ALLOCATE(FixedF(N,N))
  ALLOCATE(RowCoefficient(N))
  FixedF      = 0.0d0
  RowCoefficient = 1.0d0

  NumIterations =0
  RowSum=0.0d0
    !  Check for physically unreasonable enclosures.

  If (N<=3 ) Then
     DO I=1,N
        DO J=1,N
           FixedF(i,j)=FixedAF(i,j)/A(i)
        END DO
      END DO

     CALL ShowWarningError('Surfaces in Zone="'//TRIM(Zone(ZoneNum)%Name)//'" do not define an enclosure.')
     CALL ShowContinueError('Number of surfaces <= 3, view factors are set to force reciprocity.')

     F=FixedF
     FixedCheckValue=ABS(SUM(FixedF)-N)
     FinalCheckValue=FixedCheckValue
     RowSum=0.0d0
     DO I = 1 , N
       RowSum=RowSum+SUM(FixedF(I,:))
     ENDDO
     Zone(ZoneNum)%EnforcedReciprocity=.true.
     DEALLOCATE(FixedAF)
     DEALLOCATE(FixedF)
     DEALLOCATE(FixedAFTranspose)
     DEALLOCATE(RowCoefficient)
     RETURN  ! Do not iterate, stop with reciprocity satisfied.

  END IF !  N <= 3 Case

!  Regular fix cases
  Converged = .false.
  DO WHILE ( .not. Converged )
    NumIterations = NumIterations + 1
    DO I=1,N
          ! Determine row coefficients which will enforce closure.
        IF (ABS(SUM(FixedAF(i,1:N))) > 1.0d-10) THEN
          RowCoefficient(i)=A(i)/SUM(FixedAF(i,1:N))
        ELSE
          RowCoefficient(i)=1.0d0
        ENDIF
        FixedAF(i,1:N)=FixedAF(i,1:N)*RowCoefficient(i)
    END DO

     !  Enforce reciprocity by averaging AiFij and AjFji
    FixedAFTranspose = TRANSPOSE(FixedAF)
    FixedAF=0.5d0*(FixedAFTranspose+FixedAF)

     !  Form FixedF matrix
    DO I=1,N
      DO J=1,N
        FixedF(i,j)=FixedAF(i,j)/A(i)
        IF (ABS(FixedF(i,j)) < 1.d-10) Then
          FixedF(i,j)=0.0d0
          FixedAF(i,j) = 0.0d0
        END IF
      END DO
    END DO

    ConvrgNew=ABS(SUM(FixedF)-N)
    IF (ABS(ConvrgOld-ConvrgNew) < DifferenceConvergence  .or. &
        ConvrgNew <= PrimaryConvergence) THEN   !  Change in sum of Fs must be small.
      Converged = .true.
    END IF
    ConvrgOld = ConvrgNew
    IF (NumIterations > 400 ) THEN  !  If everything goes bad,enforce reciprocity and go home.
      !  Enforce reciprocity by averaging AiFij and AjFji
      FixedAFTranspose = TRANSPOSE(FixedAF)
      FixedAF=0.5d0*(FixedAFTranspose+FixedAF)

       !  Form FixedF matrix
      DO I=1,N
        DO J=1,N
        FixedF(i,j)=FixedAF(i,j)/A(i)
        END DO
      END DO
      CheckConvergeTolerance=ABS(SUM(FixedF)-N)
      IF (CheckConvergeTolerance > .005d0) THEN
        CALL ShowWarningError('FixViewFactors: View factors not complete. Check for '//&
                   'bad surface descriptions or unenclosed zone="'//TRIM(Zone(ZoneNum)%Name)//'".')
        CALL ShowContinueError('Enforced reciprocity has tolerance (ideal is 0)=['//  &
                   TRIM(RoundSigDigits(CheckConvergeTolerance,6))//'], Row Sum (ideal is '//  &
                   trim(RoundSigDigits(N))//')=['//trim(RoundSigDigits(RowSum,2))//'].')
        CALL ShowContinueError('If zone is unusual, or tolerance is on the order of 0.001, view factors are probably OK.')
      ENDIF
      FixedCheckValue=ABS(SUM(FixedF)-N)
      FinalCheckValue=FixedCheckValue
      IF (ABS(FixedCheckValue) < ABS(OriginalCheckValue)) THEN
        F=FixedF
        FinalCheckValue=FixedCheckValue
      ENDIF
      RowSum=0.0d0
      DO I = 1,N
        RowSum=RowSum+SUM(FixedF(I,:))
      ENDDO
      DEALLOCATE(FixedAF)
      DEALLOCATE(FixedF)
      DEALLOCATE(FixedAFTranspose)
      DEALLOCATE(RowCoefficient)
      Return
    END IF
  END DO
  FixedCheckValue=ConvrgNew
  IF (FixedCheckValue < OriginalCheckValue) THEN
    F=FixedF
    FinalCheckValue=FixedCheckValue
  ELSE
    FinalCheckValue=OriginalCheckValue
    RowSum=0.0d0
    DO I = 1,N
      RowSum=RowSum+SUM(FixedF(I,:))
    ENDDO
    IF (ABS(RowSum-N) < PrimaryConvergence) THEN
      F=FixedF
      FinalCheckValue=FixedCheckValue
    ELSE
      CALL ShowWarningError('FixViewFactors: View factors not complete. Check for '//&
                'bad surface descriptions or unenclosed zone="'//TRIM(Zone(ZoneNum)%Name)//'".')
    ENDIF
  ENDIF

  DEALLOCATE(FixedAF)
  DEALLOCATE(FixedF)
  DEALLOCATE(FixedAFTranspose)
  DEALLOCATE(RowCoefficient)

  RETURN

END SUBROUTINE FixViewFactors


SUBROUTINE CalcScriptF(N,A,F,EMISS,ScriptF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Curt Pedersen
          !       DATE WRITTEN   1980
          !       MODIFIED       July 2000 (COP for the ASHRAE Loads Toolkit)
          !       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines Hottel's ScriptF coefficients which account for the total
          ! grey interchange between surfaces in an enclosure.

          ! METHODOLOGY EMPLOYED:
          ! See reference

          ! REFERENCES:
          ! Hottel, H. C. and A. F. Sarofim, Radiative Transfer, Ch 3, McGraw Hill, 1967.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  INTEGER, INTENT (IN)                  :: N         ! Number of surfaces
  REAL(r64), INTENT (IN),  DIMENSION(N)   :: A       ! AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
  REAL(r64), INTENT (IN),  DIMENSION(N,N) :: F       ! DIRECT VIEW FACTOR MATRIX (N X N)
                                                     ! --Must satisfy reciprocity and completeness:
                                                     !  A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)
  REAL(r64), INTENT (INOUT), DIMENSION(N) :: EMISS   ! VECTOR OF SURFACE EMISSIVITIES
  REAL(r64), INTENT (OUT), DIMENSION(N,N) :: ScriptF ! MATRIX OF SCRIPT F FACTORS (N X N)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: MaxEmissLimit = 0.99999d0        ! Limit the emissivity internally/avoid a divide by zero error

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: I, J  ! DO loop counters (for rows and columns of matrices)
  INTEGER :: K
  REAL(r64),  ALLOCATABLE, DIMENSION(:,:) :: AF           ! = (AREA * DIRECT VIEW FACTOR) MATRIX
  REAL(r64),  ALLOCATABLE, DIMENSION(:,:) :: Cinverse     ! Inverse of Cmatrix
  REAL(r64),  ALLOCATABLE, DIMENSION(:,:) :: Cmatrix      ! = (AF- EMISS/REFLECTANCE) MATRIX
  REAL(r64),  ALLOCATABLE, DIMENSION(:,:) :: ExciteMatrix ! EXCITATION VECTOR = A*EMISS/REFLECTANCE
  REAL(r64),  ALLOCATABLE, DIMENSION(:,:) :: Jmatrix      ! MATRIX OF PARTIAL RADIOSITIES

          ! FLOW:
          ! Allocate and zero arrays

#ifdef EP_Count_Calls
  NumCalcScriptF_Calls=NumCalcScriptF_Calls+1
#endif
  ALLOCATE(AF(N,N))
  ALLOCATE(Cinverse(N,N))
  ALLOCATE(Cmatrix(N,N))
  ALLOCATE(ExciteMatrix(N,N))

  AF           = 0.0d0
  Cmatrix      = 0.0d0
  Cinverse     = 0.0d0
  ExciteMatrix = 0.0d0
  ScriptF      = 0.0d0

          ! Set up AF matrix.
  DO I=1,N
    DO J = 1,N
      AF(I,J)=F(I,J)*A(I)
    END DO
  END DO

  Cmatrix = AF               !  Cmatrix is now same as AF

          ! Limit EMISS for any individual surface.  This is to avoid
          ! an obvious divide by zero error in the next section
  DO I=1,N
    IF (EMISS(I) > MaxEmissLimit) THEN
      EMISS(I) = MaxEmissLimit
      CALL ShowWarningError('A thermal emissivity above 0.99999 was detected. This is not allowed. Value was reset to 0.99999')
    END IF
  END DO

  DO I=1,N
    ExciteMatrix(I,I) = -A(I)*EMISS(I)/(1.d0-EMISS(I)) ! Set up matrix columns for partial radiosity calculation
    Cmatrix(I,I)      = AF(I,I) - A(I)/(1.d0-EMISS(I)) ! Coefficient matrix for partial radiosity calculation
  END DO

  DEALLOCATE(AF)

  CALL CalcMatrixInverse(Cmatrix,Cinverse)  ! SOLVE THE LINEAR SYSTEM

  DEALLOCATE(Cmatrix)

  ALLOCATE(Jmatrix(N,N))
!  Jmatrix      = 0.0
  Jmatrix = MATMUL(Cinverse,Excitematrix)   ! Jmatrix columns contain partial radiosities
!  DO i=1,N
!    DO j=1,N
!      DO k=1,N
!        Jmatrix(i,j) = Jmatrix(i,j) + Cinverse(i,k) * Excitematrix(k,j)
!      END DO
!    END DO
!  END DO

          ! Form Script F matrix
  DO I=1,N
    DO J=1,N
      IF (I == J) THEN
!        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=1
        ScriptF(I,J) = EMISS(I)/(1.d0-EMISS(I))*(Jmatrix(I,J)-EMISS(I))
      ELSE
!        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=0
        ScriptF(I,J) = EMISS(I)/(1.d0-EMISS(I))*(Jmatrix(I,J))
      END IF
    END DO
  END DO

  DEALLOCATE(Cinverse)
  DEALLOCATE(ExciteMatrix)
  DEALLOCATE(Jmatrix)

  RETURN

END SUBROUTINE CalcScriptF


SUBROUTINE CalcMatrixInverse(Matrix,InvMatrix)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jakob Asmundsson
          !       DATE WRITTEN   January 1999
          !       MODIFIED       September 2000 (RKS for EnergyPlus)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To find the inverse of Matrix, using partial pivoting.

          ! METHODOLOGY EMPLOYED:
          ! Inverse is found using partial pivoting and Gauss elimination

          ! REFERENCES:
          ! Any Linear Algebra book

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENTS:
  REAL(r64), DIMENSION(:,:), INTENT(INOUT) :: Matrix      ! Input Matrix
  REAL(r64), DIMENSION(:,:), INTENT(INOUT) :: InvMatrix   ! Inverse of Matrix

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: DimOfMatrix ! Matrix dimension
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: Identity    ! Identity matrix
  INTEGER,   ALLOCATABLE, DIMENSION(:)   :: p           ! Vector containing the
                                                               ! pivot order
  INTEGER         :: temp        ! Temporary variable
  REAL(r64)  :: mm          ! Multiplier
  REAL(r64)  :: pivot       ! Pivot value
  INTEGER         :: piv         ! Pivot index
  INTEGER         :: i           ! Loop counter
  INTEGER         :: j           ! Loop counter
  INTEGER         :: k           ! Loop counter

          ! FLOW:
  DimOfMatrix = SIZE(Matrix, DIM=1)
  ALLOCATE(Identity(DimOfMatrix,DimOfMatrix))
  ALLOCATE(p(DimOfMatrix))

  Identity  = 0.0d0
  InvMatrix = 0.0d0

  DO i = 1, DimOfMatrix
    Identity(i,i) = 1.0d0
  END DO

  p=(/ (I, I=1,DimOfMatrix) /)

  DO j = 1, DimOfMatrix-1
    pivot = ABS(Matrix(p(j),j))
    piv   = j
    temp  = p(j)
    DO i = j+1, DimOfMatrix
      IF (ABS(Matrix(p(i),j))>pivot) THEN
        pivot = ABS(Matrix(p(i),j))
        piv   = i
      END IF
    END DO

    p(j)   = p(piv)
    p(piv) = temp
    DO i = j+1, DimOfMatrix
      mm               = Matrix(p(i),j)/Matrix(p(j),j)
      Matrix(p(i),j)   = 0.0d0
      Identity(p(i),:) = Identity(p(i),:) - mm*Identity(p(j),:)
      DO k = j+1, DimOfMatrix
        Matrix(p(i),k) = Matrix(p(i),k) - mm*Matrix(p(j),k)
      END DO
    END DO
  END DO

  DO i = DimOfMatrix, 1, -1
    InvMatrix(i,:) = Identity(p(i),:)
    DO j = i+1, DimOfMatrix
      InvMatrix(i,:) = InvMatrix(i,:) - Matrix(p(i),j)*InvMatrix(j,:)
    END DO
    InvMatrix(i,:) = InvMatrix(i,:)/Matrix(p(i),i)
  END DO

  DEALLOCATE(p)
  DEALLOCATE(Identity)

  RETURN

END SUBROUTINE CalcMatrixInverse

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

END MODULE HeatBalanceIntRadExchange
