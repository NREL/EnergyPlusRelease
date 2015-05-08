MODULE HeatBalanceSurfaceManager

          ! Module containing the routines dealing with the Heat Balance of the surfaces

          ! MODULE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN
          !       MODIFIED       DJS (PSU Dec 2006) to add ecoroof
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms required to
          ! manage the simluation of the surface heat balance for the building.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! The heat balance method is outlined in the "TARP Reference Manual", NIST, NBSIR 83-2655, Feb 1983.
          ! The methods are also summarized in many BSO Theses and papers.

          ! OTHER NOTES:
          ! This module was created from IBLAST subroutines

          ! USE STATEMENTS:
          ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalFanSys
USE DataHeatBalance
USE DataHeatBalSurface
USE DataSurfaces
Use DataMoistureBalance, ONLY: TempOutsideAirFD,RhoVaporAirOut,RhoVaporAirIn,HConvExtFD,HMassConvExtFD,  &
                               HConvInFD,HMassConvInFD,RhoVaporSurfIn, &
                               HSkyFD,HGrndFD,HAirFD
USE DataInterfaces
!unused0909USE DataMoistureBalanceEMPD, ONLY: MoistEMPDNew, MoistEMPDFlux

          ! Use statements for access to subroutines in other modules
USE InputProcessor
USE ScheduleManager
USE SolarShading
USE DaylightingManager


IMPLICIT NONE   ! Enforce explicit typing of all variables

PRIVATE   !Make everything private except what is specifically Public

  ! MODULE PARAMETER DEFINITIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! MODULE VARIABLE DECLARATIONS:
  ! na

          ! Subroutine Specifications for the Heat Balance Module
          ! Driver Routines
PUBLIC  ManageSurfaceHeatBalance

          ! Initialization routines for module
PRIVATE AllocateSurfaceHeatBalArrays
PUBLIC  InitSurfaceHeatBalance
PRIVATE InitThermalAndFluxHistories
PRIVATE InitSolarHeatGains
PRIVATE InitIntSolarDistribution
PRIVATE ComputeIntThermalAbsorpFactors
PRIVATE ComputeIntSWAbsorpFactors
PRIVATE ComputeDifSolExcZonesWIZWindows
PRIVATE InitEMSControlledSurfaceProperties

          ! Algorithms for the module
          ! These are now external subroutines
!PUBLIC  CalcHeatBalanceOutsideSurf  ! The heat balance routines are now public because the
!PUBLIC  CalcHeatBalanceInsideSurf   ! radiant systems need access to them in order to simulate

          ! Record Keeping/Utility Routines for Module
PRIVATE UpdateFinalSurfaceHeatBalance
PRIVATE UpdateThermalHistories
PUBLIC  CalculateZoneMRT

          ! Reporting routines for module
PRIVATE ReportSurfaceHeatBalance

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE ManageSurfaceHeatBalance

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   January 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the heat surface balance method of calculating
          ! building thermal loads.  It is called from the HeatBalanceManager
          ! at the time step level.  This driver manages the calls to all of
          ! the other drivers and simulation algorithms.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE HeatBalanceAirManager,  ONLY : ManageAirHeatBalance
  USE ThermalComfort,         ONLY : ManageThermalComfort
  USE HeatBalFiniteDiffManager,   ONLY :  UpdateMoistureBalanceFD
  USE OutputReportTabular,    ONLY : GatherComponentLoadsSurface !for writing tabular compoonent loads output reports
  USE DataSystemVariables, ONLY: DeveloperFlag
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  logical, save :: firsttime=.true.
  INTEGER :: SurfNum
  INTEGER :: ConstrNum

          ! FLOW:
  if (firsttime) CALL DisplayString('Initializing Surfaces')
  CALL InitSurfaceHeatBalance  ! Initialize all heat balance related parameters

          ! Solve the zone heat balance 'Detailed' solution
          ! Call the outside and inside surface heat balances
  if (firsttime) CALL DisplayString('Calculate Outside Surface Heat Balance')
  CALL CalcHeatBalanceOutsideSurf
  if (firsttime) CALL DisplayString('Calculate Inside Surface Heat Balance')
  CALL CalcHeatBalanceInsideSurf

          ! The air heat balance must be called before the temperature history
          ! updates because there may be a radiant system in the building
  if (firsttime) CALL DisplayString('Calculate Air Heat Balance')
  CALL ManageAirHeatBalance

          ! IF NECESSARY, do one final "average" heat balance pass.  This is only
          ! necessary if a radiant system is present and it was actually on for
          ! part or all of the time step.
  CALL UpdateFinalSurfaceHeatBalance

          ! Before we leave the Surface Manager the thermal histories need to be updated
  IF((ANY(HeatTransferAlgosUsed == UseCTF)) .or. (ANY(HeatTransferAlgosUsed == UseEMPD))) THEN
    CALL UpdateThermalHistories  !Update the thermal histories
  END IF

  IF ( ANY(HeatTransferAlgosUsed == UseCondFD) ) THEN
    DO SurfNum =1, TotSurfaces
      IF(Surface(SurfNum)%Construction <= 0) CYCLE  ! Shading surface, not really a heat transfer surface
      ConstrNum=Surface(SurfNum)%Construction
      IF(Construct(ConstrNum)%TypeIsWindow) CYCLE  !  Windows simulated in Window module
      IF (Surface(SurfNum)%HeatTransferAlgorithm  /= HeatTransferModel_CondFD) CYCLE
      CALL UpdateMoistureBalanceFD(SurfNum)
    ENDDO
  ENDIF

  CALL ManageThermalComfort(InitializeOnly=.false.) ! "Record keeping" for the zone

  CALL ReportSurfaceHeatBalance
  IF (ZoneSizingCalc) CALL GatherComponentLoadsSurface

  firsttime=.false.

  RETURN

END SUBROUTINE ManageSurfaceHeatBalance


! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitSurfaceHeatBalance  ! Surface Heat Balance Initialization Manager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   January 1998
          !       MODIFIED       Nov. 1999, FCW,
          !                      Move ComputeIntThermalAbsorpFactors
          !                      so called every timestep
          !                      Jan 2004, RJH
          !                      Added calls to alternative daylighting analysis using DElight
          !                      All modifications demarked with RJH (Rob Hitchcock)
          !                      RJH, Jul 2004: add error handling for DElight calls
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for surface initializations within the
          ! heat balance.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger record keeping events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataDaylighting, ONLY: ZoneDaylight, NoDaylighting, DetailedDaylighting, DElightDaylighting,   &
                             mapResultsToReport, TotIllumMaps
  USE DataDaylightingDevices, ONLY: NumOfTDDPipes
  USE SolarShading
  USE ConvectionCoefficients, ONLY : InitInteriorConvectionCoeffs
  USE InternalHeatGains,      ONLY : ManageInternalHeatGains
  USE DataRoomAirModel,       ONLY : IsZoneDV,IsZoneCV,IsZoneUI
  USE HeatBalanceIntRadExchange,    ONLY : CalcInteriorRadExchange
  USE HeatBalFiniteDiffManager,  ONLY : InitHeatBalFiniteDiff
  USE DataSystemVariables, ONLY: GoodIOStatValue
  USE DataGlobals  , ONLY: AnyEnergyManagementSystemInModel
  ! RJH DElight Modification Begin
  USE DElightManagerF
  ! RJH DElight Modification End

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER     :: Eps = 1.d-10       ! Small number

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: ConstrNum ! Construction index
  INTEGER          :: NZ        ! DO loop counter for zones
  REAL(r64) :: QIC       ! Intermediate calculation variable
  REAL(r64) :: QOC       ! Intermediate calculation variable
  INTEGER          :: SurfNum   ! DO loop counter for surfaces
  INTEGER          :: Term      ! DO loop counter for conduction equation terms
  REAL(r64) :: TSC       ! Intermediate calculation variable
  INTEGER          :: ZoneNum   ! Counter for zone loop initialization

  ! RJH DElight Modification Begin
  REAL(r64) :: dPowerReducFac ! Return value Electric Lighting Power Reduction Factor for current Zone and Timestep
  REAL(r64) :: dHISKFFC ! double value for argument passing
  REAL(r64) :: dHISUNFFC ! double value for argument passing
  REAL(r64) :: dSOLCOS1 ! double value for argument passing
  REAL(r64) :: dSOLCOS2 ! double value for argument passing
  REAL(r64) :: dSOLCOS3 ! double value for argument passing
  REAL(r64) :: dLatitude ! double value for argument passing
  REAL(r64) :: dCloudFraction ! double value for argument passing
  INTEGER :: iErrorFlag  ! Error Flag for warning/errors returned from DElight
  INTEGER, EXTERNAL :: GetNewUnitNumber  ! External  function to "get" a unit number
  INTEGER :: iDElightErrorFile      ! Unit number for reading DElight Error File (eplusout.delightdfdmp)
  INTEGER :: iReadStatus            ! Error File Read Status
  CHARACTER(len=210) cErrorLine     ! Each DElight Error line can be up to 210 characters long
  CHARACTER(len=200) cErrorMsg      ! Each DElight Error Message can be up to 200 characters long
  LOGICAL :: bEndofErrFile          ! End of Error File flag
  INTEGER :: iDElightRefPt          ! Reference Point number for reading DElight Dump File (eplusout.delighteldmp)
  REAL(r64) :: dRefPtIllum          ! tmp var for reading RefPt illuminance
  ! RJH DElight Modification End
  logical, save :: firsttime=.true.
  INTEGER :: MapNum
  INTEGER :: iwriteStatus
  LOGICAL :: errFlag
  LOGICAL :: elOpened
!  LOGICAL :: ShadowingSurf

          ! FLOW:

  if (firsttime) CALL DisplayString('Initializing Outdoor environment for Surfaces')
  ! Initialize zone outdoor environmental variables
  ! Bulk Initialization for Temperatures & WindSpeed
  ! using the zone, modify the zone  Dry/Wet BulbTemps
  CALL SetOutBulbTempAt(NumOfZones, Zone(1:NumOfZones)%Centroid%Z,           &
       Zone(1:NumOfZones)%OutDryBulbTemp, Zone(1:NumOfZones)%OutWetBulbTemp, 'Zone')

  CALL SetWindSpeedAt(NumOfZones, Zone(1:NumOfZones)%Centroid%Z, Zone(1:NumOfZones)%WindSpeed, 'Zone')
!  DO ZoneNum = 1, NumOfZones
!    Zone(ZoneNum)%WindSpeed = WindSpeedAt(Zone(ZoneNum)%Centroid%Z)
!  END DO

  ! Initialize surface outdoor environmental variables
  ! Bulk Initialization for Temperatures & WindSpeed
  ! using the surface centroids, modify the surface Dry/Wet BulbTemps
  CALL SetOutBulbTempAt(TotSurfaces, Surface(1:TotSurfaces)%Centroid%Z, &
       Surface(1:TotSurfaces)%OutDryBulbTemp, Surface(1:TotSurfaces)%OutWetBulbTemp, 'Surface')

  CALL SetWindSpeedAt(TotSurfaces, Surface(1:TotSurfaces)%Centroid%Z, Surface(1:TotSurfaces)%WindSpeed, 'Surface')
!  DO SurfNum = 1, TotSurfaces
!    IF (Surface(SurfNum)%ExtWind) Surface(SurfNum)%WindSpeed = WindSpeedAt(Surface(SurfNum)%Centroid%Z)
!  END DO

  IF (AnyEnergyManagementSystemInModel) THEN
    DO SurfNum = 1, TotSurfaces
      IF (Surface(SurfNum)%OutDryBulbTempEMSOverrideOn) THEN
        Surface(SurfNum)%OutDryBulbTemp = Surface(SurfNum)%OutDryBulbTempEMSOverrideValue
      ENDIF
      IF (Surface(SurfNum)%OutWetBulbTempEMSOverrideOn) THEN
        Surface(SurfNum)%OutWetBulbTemp = Surface(SurfNum)%OutWetBulbTempEMSOverrideValue
      ENDIF
      IF (Surface(SurfNum)%WindSpeedEMSOverrideOn) THEN
        Surface(SurfNum)%WindSpeed = Surface(SurfNum)%WindSpeedEMSOverrideValue
      ENDIF
    ENDDO
  ENDIF

          ! Do the Begin Simulation initializations
  IF (BeginSimFlag) THEN
    CALL AllocateSurfaceHeatBalArrays ! Allocate the Module Arrays before any inits take place
    InterZoneWindow=ANY(Zone%HasInterZoneWindow)
    ALLOCATE (IsZoneDV(NumOfZones))
    IsZoneDV=.false.
    ALLOCATE (IsZoneCV(NumOfZones))
    IsZoneCV=.false.
    ALLOCATE (IsZoneUI(NumOfZones))
    IsZoneUI=.false.
  END IF

          ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag) THEN
    if (firsttime) CALL DisplayString('Initializing Temperature and Flux Histories')
    CALL InitThermalAndFluxHistories  ! Set initial temperature and flux histories
  ENDIF

  ! There are no daily initializations done in this portion of the surface heat balance

  ! There are no hourly initializations done in this portion of the surface heat balance
  IF (AnyEnergyManagementSystemInModel) THEN
    CALL InitEMSControlledConstructions
    CALL InitEMSControlledSurfaceProperties

  ENDIF

  ! Need to be called each timestep in order to check if surface points to new construction (EMS) and if does then
  ! complex fenestration needs to be initialized for additional states
  CALL TimestepInitComplexFenestration

  ! Calculate exterior-surface multipliers that account for anisotropy of
  ! sky radiance
  IF (SunIsUp .AND. DifSolarRad > 0.0d0) THEN
    CALL AnisoSkyViewFactors
  ELSE
    AnisoSkyMult = 0.0d0
  ENDIF

  ! Set shading flag for exterior windows (except flags related to daylighting) and
  ! window construction (unshaded or shaded) to be used in heat balance calculation
  if (firsttime) CALL DisplayString('Initializing Window Shading')
  CALL WindowShadingManager

  ! Calculate factors that are used to determine how much long-wave radiation from internal
  ! gains is absorbed by interior surfaces
  if (firsttime) CALL DisplayString('Computing Interior Absorption Factors')
  CALL ComputeIntThermalAbsorpFactors

  ! Calculate factors for diffuse solar absorbed by room surfaces and interior shades
  if (firsttime) CALL DisplayString('Computing Interior Diffuse Solar Absorption Factors')
  CALL ComputeIntSWAbsorpFactors

  ! Calculate factors for exchange of diffuse solar between zones through interzone windows
  if (firsttime) CALL DisplayString('Computing Interior Diffuse Solar Exchange through Interzone Windows')
  CALL ComputeDifSolExcZonesWIZWindows(NumOfZones)

  ! For daylit zones, calculate interior daylight illuminance at reference points and
  ! simulate lighting control system to get overhead electric lighting reduction
  ! factor due to daylighting.

  DO SurfNum = 1,TotSurfaces
    IF(Surface(SurfNum)%Class == SurfaceClass_Window .AND. Surface(SurfNum)%ExtSolar) THEN
      SurfaceWindow(SurfNum)%IllumFromWinAtRefPt1Rep   = 0.0d0
      SurfaceWindow(SurfNum)%IllumFromWinAtRefPt2Rep   = 0.0d0
      SurfaceWindow(SurfNum)%LumWinFromRefPt1Rep       = 0.0d0
      SurfaceWindow(SurfNum)%LumWinFromRefPt2Rep       = 0.0d0
    END IF
  END DO

  DO NZ=1,NumOfZones
    ! RJH DElight Modification Begin - Change Daylighting test to continue for Detailed AND DElight
    IF (ZoneDaylight(NZ)%DaylightType == NoDaylighting) CYCLE
    ! RJH DElight Modification End - Change Daylighting test to continue for Detailed AND DElight
    ZoneDaylight(NZ)%DaylIllumAtRefPt = 0.0d0
    ZoneDaylight(NZ)%GlareIndexAtRefPt = 0.0d0
    ZoneDaylight(NZ)%ZonePowerReductionFactor = 1.0d0
    ZoneDaylight(NZ)%InterReflIllFrIntWins = 0.0d0             ! inter-reflected illuminance from interior windows
    IF (ZoneDaylight(NZ)%TotalDaylRefPoints /= 0) THEN
      ZoneDaylight(NZ)%TimeExceedingGlareIndexSPAtRefPt = 0.0d0
      ZoneDaylight(NZ)%TimeExceedingDaylightIlluminanceSPAtRefPt = 0.0d0
    ENDIF

    IF(SunIsUp .AND. ZoneDaylight(NZ)%TotalDaylRefPoints /= 0) THEN
      if (firsttime) CALL DisplayString('Computing Interior Daylighting Illumination')
      CALL DayltgInteriorIllum(NZ)
      IF (.not. DoingSizing) CALL DayltgInteriorMapIllum(NZ)
    END IF


    IF (SunIsUp .AND. NumOfTDDPipes > 0 .and. NZ == 1) THEN
      if (firsttime) CALL DisplayString('Computing Interior Daylighting Illumination for TDD pipes')
      CALL DayltgInteriorTDDIllum
    END IF

    ! RJH DElight Modification Begin - Call to DElight electric lighting control subroutine
    ! Check if the sun is up and the current Thermal Zone hosts a Daylighting:DElight object
    IF(SunIsUp .AND. ZoneDaylight(NZ)%TotalDElightRefPts /= 0) THEN
        ! Call DElight interior illuminance and electric lighting control subroutine
        dPowerReducFac = 1.0d0
        dHISKFFC = HISKF*LUX2FC
        dHISUNFFC = HISUNF*LUX2FC
        dSOLCOS1 = SOLCOS(1)
        dSOLCOS2 = SOLCOS(2)
        dSOLCOS3 = SOLCOS(3)
        dLatitude = Latitude
        dCloudFraction = CloudFraction
        ! Init Error Flag to 0 (no Warnings or Errors)
        iErrorFlag = 0
        CALL DElightElecLtgCtrl(LEN_TRIM(Zone(NZ)%Name), TRIM(Zone(NZ)%Name), dLatitude, &
                                dHISKFFC, dHISUNFFC, dCloudFraction, &
                                dSOLCOS1, dSOLCOS2, dSOLCOS3, &
                                dPowerReducFac, iErrorFlag)
        ! Check Error Flag for Warnings or Errors returning from DElight
        ! RJH 2008-03-07: If no warnings/errors then read refpt illuminances for standard output reporting
        IF (iErrorFlag .NE. 0) THEN
            ! Open DElight Electric Lighting Error File for reading
            iDElightErrorFile=GetNewUnitNumber()
            ! RJH 2008-03-07: open file with READWRITE
            Open (unit=iDElightErrorFile, file='eplusout.delighteldmp', action='READWRITE', IOSTAT=iwriteStatus)
            IF (iwriteStatus == 0) THEN
              elOpened=.true.
            ELSE
              elOpened=.false.
            ENDIF
!            IF (iwriteStatus /= 0) THEN
!              CALL ShowFatalError('InitSurfaceHeatBalance: Could not open file "eplusout.delighteldmp" for output (readwrite).')
!            ENDIF
!            Open (unit=iDElightErrorFile, file='eplusout.delighteldmp', action='READ')

            ! Sequentially read lines in DElight Electric Lighting Error File
            ! and process them using standard EPlus warning/error handling calls
            bEndofErrFile=.false.
            ireadStatus=0
            DO WHILE (.not. bEndofErrFile .and. iwriteStatus==0 .and. ireadStatus==0)
                READ(iDElightErrorFile,'(A)',IOSTAT=iReadStatus) cErrorLine
                IF (iReadStatus < GoodIOStatValue) THEN
                    bEndofErrFile=.true.
                    CYCLE
                ENDIF
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

            ! Close DElight Error File and delete
            IF (elOpened) Close (unit=iDElightErrorFile, status='DELETE')
            ! If any DElight Error occurred then ShowFatalError to terminate
            IF (iErrorFlag .GT. 0) THEN
                CALL ShowFatalError("End of DElight Error Messages")
            ENDIF
        ELSE  ! RJH 2008-03-07: No errors
              ! extract reference point illuminance values from DElight Electric Lighting dump file for reporting
            ! Open DElight Electric Lighting Dump File for reading
            iDElightErrorFile=GetNewUnitNumber()
            Open (unit=iDElightErrorFile, file='eplusout.delighteldmp', action='READWRITE', IOSTAT=iwriteStatus)
!            IF (iwriteStatus /= 0) THEN
!              CALL ShowFatalError('InitSurfaceHeatBalance: Could not open file "eplusout.delighteldmp" for output (readwrite).')
!            ENDIF
            IF (iwriteStatus == 0) THEN
              elOpened=.true.
            ELSE
              elOpened=.false.
            ENDIF

            ! Sequentially read lines in DElight Electric Lighting Dump File
            ! and extract refpt illuminances for standard EPlus output handling
            bEndofErrFile=.false.
            iDElightRefPt = 0
            ireadStatus=0
            DO WHILE (.not. bEndofErrFile .and. iwriteStatus==0 .and. ireadStatus==0)
                READ(iDElightErrorFile, * ,IOSTAT=iReadStatus) dRefPtIllum
                IF (iReadStatus < GoodIOStatValue) THEN
                    bEndofErrFile=.true.
                    CYCLE
                ENDIF
                ! Increment refpt counter
                iDElightRefPt = iDElightRefPt + 1
                ! Assure refpt index does not exceed number of refpts in this zone
                IF (iDElightRefPt <= ZoneDaylight(NZ)%TotalDElightRefPts) THEN
                  ZoneDaylight(NZ)%DaylIllumAtRefPt(iDElightRefPt) = dRefPtIllum
                ENDIF
            ENDDO

            ! Close DElight Electric Lighting Dump File and delete
            IF (elOpened) Close (unit=iDElightErrorFile, status='DELETE')
        ENDIF
        ! Store the calculated total zone Power Reduction Factor due to DElight daylighting
        ! in the ZoneDaylight structure for later use
        ZoneDaylight(NZ)%ZonePowerReductionFactor = dPowerReducFac
    END IF
    ! RJH DElight Modification End - Call to DElight electric lighting control subroutine

  END DO

  errFlag=.false.
  DO SurfNum = 1, TotSurfaces
    IF(Surface(SurfNum)%Class /= SurfaceClass_Window) CYCLE
    SurfaceWindow(SurfNum)%FracTimeShadingDeviceOn = 0.0d0
    IF(SurfaceWindow(SurfNum)%ShadingFlag > 0) THEN
      SurfaceWindow(SurfNum)%FracTimeShadingDeviceOn = 1.0d0
    ELSE
      SurfaceWindow(SurfNum)%FracTimeShadingDeviceOn = 0.0d0
    END IF
  END DO

  CALL CalcInteriorRadExchange(TH(:,1,2),0,NetLWRadToSurf,calledfrom='Main')

  IF(AirflowWindows) CALL WindowGapAirflowControl

  ! The order of these initializations is important currently.  Over time we hope to
  !  take the appropriate parts of these inits to the other heat balance managers
  if (firsttime) CALL DisplayString('Initializing Solar Heat Gains')
  CALL InitSolarHeatGains
  IF(SunIsUp .AND. (BeamSolarRad+GndSolarRad+DifSolarRad > 0.0d0)) THEN
    DO NZ = 1,NumOfZones
      IF(ZoneDaylight(NZ)%TotalDaylRefPoints > 0) THEN
        IF(Zone(NZ)%HasInterZoneWindow) THEN
          CALL DayltgInterReflIllFrIntWins(NZ)
          CALL DayltgGlareWithIntWins(ZoneDaylight(NZ)%GlareIndexAtRefPt,NZ)
        END IF
        CALL DayltgElecLightingControl(NZ)
      END IF
    END DO
  ELSEIF (mapResultsToReport .and. TimeStep == NumOfTimeStepInHour) THEN
    DO MapNum = 1, TotIllumMaps
      CALL ReportIllumMap(MapNum)
    END DO
    mapResultsToReport=.false.
  END IF

  if (firsttime) CALL DisplayString('Initializing Internal Heat Gains')
  CALL ManageInternalHeatGains(InitOnly=.false.)
  if (firsttime) CALL DisplayString('Initializing Interior Solar Distribution')
  CALL InitIntSolarDistribution
  if (firsttime) CALL DisplayString('Initializing Interior Convection Coefficients')
  CALL InitInteriorConvectionCoeffs(TempSurfInTmp)

  IF (BeginSimFlag) THEN    ! Now's the time to report surfaces, if desired
!    if (firsttime) CALL DisplayString('Reporting Surfaces')
!    CALL ReportSurfaces
    if (firsttime) CALL DisplayString('Gathering Information for Predefined Reporting')
    CALL GatherForPredefinedReport
  ENDIF

          ! Initialize the temperature history terms for conduction through the surfaces
  IF (ANY(HeatTransferAlgosUsed == UseCondFD) ) THEN
    CALL InitHeatBalFiniteDiff
  ENDIF

  CTFConstOutPart  = 0.0d0
  CTFConstInPart   = 0.0d0
  CTFTsrcConstPart = 0.0d0
  DO SurfNum = 1, TotSurfaces   ! Loop through all surfaces...

    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE  ! Skip non-heat transfer surfaces
    IF (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_CTF .AND. &
        Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_EMPD) CYCLE
    IF(Surface(SurfNum)%Class == SurfaceClass_Window) CYCLE
       ! Outside surface temp of "normal" windows not needed in Window5 calculation approach
       ! Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

    ConstrNum = Surface(SurfNum)%Construction
    IF (Construct(ConstrNum)%NumCTFTerms > 1) THEN ! COMPUTE CONSTANT PORTION OF CONDUCTIVE FLUXES.

      QIC = 0.0D0
      QOC = 0.0D0
      TSC = 0.0D0
      DO Term = 1, Construct(ConstrNum)%NumCTFTerms

          ! Sign convention for the various terms in the following two equations
          ! is based on the form of the Conduction Transfer Function equation
          ! given by:
          ! Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old)
          ! Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old)
          ! In both equations, flux is positive from outside to inside.

        QIC = QIC + Construct(ConstrNum)%CTFCross(Term) *TH(SurfNum,Term+1,1) &
                  - Construct(ConstrNum)%CTFInside(Term)*TH(SurfNum,Term+1,2) &
                  + Construct(ConstrNum)%CTFFlux(Term)  *QH(SurfNum,Term+1,2)

        QOC = QOC + Construct(ConstrNum)%CTFOutside(Term)*TH(SurfNum,Term+1,1) &
                  - Construct(ConstrNum)%CTFCross(Term)  *TH(SurfNum,Term+1,2) &
                  + Construct(ConstrNum)%CTFFlux(Term)   *QH(SurfNum,Term+1,1)

        IF (Construct(ConstrNum)%SourceSinkPresent) THEN

          QIC = QIC + Construct(ConstrNum)%CTFSourceIn(Term) *QsrcHist(SurfNum,Term+1)

          QOC = QOC + Construct(ConstrNum)%CTFSourceOut(Term)*QsrcHist(SurfNum,Term+1)

          TSC = TSC + Construct(ConstrNum)%CTFTSourceOut(Term)*TH(SurfNum,Term+1,1)     &
                    + Construct(ConstrNum)%CTFTSourceIn(Term) *TH(SurfNum,Term+1,2)     &
                    + Construct(ConstrNum)%CTFTSourceQ(Term)  *QsrcHist(SurfNum,Term+1) &
                    + Construct(ConstrNum)%CTFFlux(Term)      *TsrcHist(SurfNum,Term+1)

        END IF


      END DO

      CTFConstOutPart(SurfNum)  = QOC
      CTFConstInPart(SurfNum)   = QIC
      CTFTsrcConstPart(SurfNum) = TSC

    ELSE    ! Number of CTF Terms = 1-->Resistance only constructions have no history terms.

      CTFConstOutPart(SurfNum)  = 0.0d0
      CTFConstInPart(SurfNum)   = 0.0d0
      CTFTsrcConstPart(SurfNum) = 0.0d0

    END IF

  END DO    ! ...end of surfaces DO loop for initializing temperature history terms for the surface heat balances

          ! Zero out all of the radiant system heat balance coefficient arrays
  RadSysTiHBConstCoef = 0.0d0
  RadSysTiHBToutCoef  = 0.0d0
  RadSysTiHBQsrcCoef  = 0.0d0
  RadSysToHBConstCoef = 0.0d0
  RadSysToHBTinCoef   = 0.0d0
  RadSysToHBQsrcCoef  = 0.0d0

  QRadSysSource = 0.0D0
  QPVSysSource  = 0.0d0
  QHTRadSysSurf = 0.0D0
  QHWBaseboardSurf = 0.0D0
  QSteamBaseboardSurf = 0.0D0
  QElecBaseboardSurf = 0.0D0

  IF (ZoneSizingCalc) CALL GatherComponentLoadsSurfAbsFact

  if (firsttime) CALL DisplayString('Completed Initializing Surface Heat Balance')
  firsttime=.false.
  RETURN

END SUBROUTINE InitSurfaceHeatBalance

SUBROUTINE GatherForPredefinedReport

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports the information for the predefined reports
          ! related to envelope components.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE OutputReportPredefined
  USE WindowManager, ONLY: CalcNominalWindowCond

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: surfName
  INTEGER                      :: curCons
  INTEGER                      :: zonePt
  REAL(r64)                    :: mult
  REAL(r64)                    :: curAzimuth
  REAL(r64)                    :: curTilt
  INTEGER                      :: iSurf
  REAL(r64)                    :: windowArea
  REAL(r64)                    :: frameWidth
  REAL(r64)                    :: frameArea
  REAL(r64)                    :: dividerArea
  !counts for object count report
  INTEGER, DIMENSION(20)       :: numSurfaces
  INTEGER, DIMENSION(20)       :: numExtSurfaces
  INTEGER                      :: frameDivNum
  LOGICAL                      :: isExterior

  ! the following variables are for the CalcNominalWindowCond call but only SHGCSummer is needed
  REAL(r64) :: nomCond
  REAL(r64) :: SHGCSummer
  REAL(r64) :: TransSolNorm
  REAL(r64) :: TransVisNorm
  REAL(r64) :: nomUfact
  INTEGER :: ErrFlag
  INTEGER :: curWSC
  !following variables are totals for fenestration table
  REAL(r64) :: windowAreaWMult = 0.0d0
  REAL(r64) :: fenTotArea = 0.0d0
  REAL(r64) :: fenTotAreaNorth = 0.0d0
  REAL(r64) :: fenTotAreaNonNorth = 0.0d0
  REAL(r64) :: ufactArea = 0.0d0
  REAL(r64) :: ufactAreaNorth = 0.0d0
  REAL(r64) :: ufactAreaNonNorth = 0.0d0
  REAL(r64) :: shgcArea = 0.0d0
  REAL(r64) :: shgcAreaNorth = 0.0d0
  REAL(r64) :: shgcAreaNonNorth   = 0.0d0
  REAL(r64) :: vistranArea = 0.0d0
  REAL(r64) :: vistranAreaNorth = 0.0d0
  REAL(r64) :: vistranAreaNonNorth   = 0.0d0
  REAL(r64) :: intFenTotArea = 0.0d0
  REAL(r64) :: intUfactArea = 0.0d0
  REAL(r64) :: intShgcArea = 0.0d0
  REAL(r64) :: intVistranArea = 0.0d0
  LOGICAL   :: isNorth

  numSurfaces=0
  numExtSurfaces=0

  DO iSurf = 1, TotSurfaces
    zonePt = Surface(iSurf)%Zone
    !only exterior surfaces including underground
    IF ((Surface(iSurf)%ExtBoundCond .EQ. ExternalEnvironment) .OR. (Surface(iSurf)%ExtBoundCond .EQ. Ground) &
     .OR. (Surface(iSurf)%ExtBoundCond .EQ. GroundFCfactorMethod)) THEN
      isExterior = .TRUE.
      SELECT CASE (Surface(iSurf)%Class)
        CASE (SurfaceClass_Wall,SurfaceClass_Floor,SurfaceClass_Roof)
          surfName = Surface(iSurf)%Name
          curCons = Surface(iSurf)%Construction
          CALL PreDefTableEntry(pdchOpCons,surfName,Construct(curCons)%Name)
          CALL PreDefTableEntry(pdchOpRefl,surfName,1 - Construct(curCons)%OutsideAbsorpSolar)
          CALL PreDefTableEntry(pdchOpUfactNoFilm,surfName,NominalU(Surface(iSurf)%Construction),3)
          mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier
          CALL PreDefTableEntry(pdchOpGrArea,surfName,Surface(iSurf)%GrossArea * mult)
          curAzimuth = Surface(iSurf)%Azimuth
          CALL PreDefTableEntry(pdchOpAzimuth,surfName,curAzimuth)
          curTilt = Surface(iSurf)%Tilt
          CALL PreDefTableEntry(pdchOpTilt,surfName,curTilt)
          IF ((curTilt >= 60.d0) .AND. (curTilt < 180.d0)) THEN
            IF ((curAzimuth >= 315.d0) .OR. (curAzimuth < 45.d0)) THEN
              CALL PreDefTableEntry(pdchOpDir,surfName,'N')
            ELSE IF ((curAzimuth >= 45.d0) .AND. (curAzimuth < 135.d0)) THEN
              CALL PreDefTableEntry(pdchOpDir,surfName,'E')
            ELSE IF ((curAzimuth >= 135.d0) .AND. (curAzimuth < 225.d0)) THEN
              CALL PreDefTableEntry(pdchOpDir,surfName,'S')
            ELSE IF ((curAzimuth >= 225.d0) .AND. (curAzimuth < 315.d0)) THEN
              CALL PreDefTableEntry(pdchOpDir,surfName,'W')
            ENDIF
          END IF
        CASE (SurfaceClass_Window,SurfaceClass_TDD_Dome)
          surfName = Surface(iSurf)%Name
          curCons = Surface(iSurf)%Construction
          CALL PreDefTableEntry(pdchFenCons,surfName,Construct(curCons)%Name)
          zonePt = Surface(iSurf)%Zone
          mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier * Surface(iSurf)%Multiplier
          !include the frame area if present
          windowArea = Surface(iSurf)%GrossArea
          frameArea = 0.0d0
          dividerArea = 0.0d0
          frameDivNum = Surface(iSurf)%FrameDivider
          IF  (frameDivNum /= 0) THEN
            frameWidth = FrameDivider(frameDivNum)%FrameWidth
            frameArea = (Surface(iSurf)%Height + 2.0d0*frameWidth)*(Surface(iSurf)%Width + 2.0d0*frameWidth) &
                        - (Surface(iSurf)%Height * Surface(iSurf)%Width)
            windowArea = windowArea + frameArea
            dividerArea = FrameDivider(frameDivNum)%DividerWidth * &
             (FrameDivider(frameDivNum)%HorDividers * Surface(iSurf)%Width + &
              FrameDivider(frameDivNum)%VertDividers * Surface(iSurf)%Height - &
              FrameDivider(frameDivNum)%HorDividers * FrameDivider(frameDivNum)%VertDividers * &
              FrameDivider(frameDivNum)%DividerWidth)
            CALL PreDefTableEntry(pdchFenFrameConductance,surfName,FrameDivider(frameDivNum)%FrameConductance,3)
            CALL PreDefTableEntry(pdchFenDividerConductance,surfName,FrameDivider(frameDivNum)%DividerConductance,3)
          END IF
          windowAreaWMult = windowArea * mult
          CALL PreDefTableEntry(pdChFenAreaOf1,surfName,windowArea)
          CALL PreDefTableEntry(pdchFenFrameAreaOf1,surfName,frameArea)
          CALL PreDefTableEntry(pdchFenDividerAreaOf1,surfName,dividerArea)
          CALL PreDefTableEntry(pdchFenGlassAreaOf1,surfName, windowArea - (frameArea + dividerArea))
          CALL PreDefTableEntry(pdchFenArea,surfName,windowAreaWMult)
          nomUfact = NominalU(Surface(iSurf)%Construction)
          CALL PreDefTableEntry(pdchFenUfact,surfName,nomUfact,3)
          !if the construction report is requested the SummerSHGC is already calculated
          IF (Construct(curCons)%SummerSHGC /= 0) THEN
            SHGCSummer = Construct(curCons)%SummerSHGC
            TransVisNorm = Construct(curCons)%VisTransNorm
          ELSE
            !must calculate Summer SHGC
            IF (.NOT. Construct(curCons)%WindowTypeEQL) THEN
              CALL CalcNominalWindowCond(curCons,2,nomCond,SHGCSummer,TransSolNorm,TransVisNorm,ErrFlag)
            ENDIF
          END IF
          CALL PreDefTableEntry(pdchFenSHGC,surfName,SHGCSummer,3)
          CALL PreDefTableEntry(pdchFenVisTr,surfName,TransVisNorm,3)
          CALL PreDefTableEntry(pdchFenParent,surfName,Surface(iSurf)%BaseSurfName)
          curAzimuth = Surface(iSurf)%Azimuth
          CALL PreDefTableEntry(pdchFenAzimuth,surfName,curAzimuth)
          isNorth = .FALSE.
          curTilt = Surface(iSurf)%Tilt
          CALL PreDefTableEntry(pdchFenTilt,surfName,curTilt)
          IF ((curTilt >= 60.d0) .AND. (curTilt < 180.d0)) THEN
            IF ((curAzimuth >= 315.d0) .OR. (curAzimuth < 45.d0)) THEN
              CALL PreDefTableEntry(pdchFenDir,surfName,'N')
              isNorth = .TRUE.
            ELSE IF ((curAzimuth >= 45.d0) .AND. (curAzimuth < 135.d0)) THEN
              CALL PreDefTableEntry(pdchFenDir,surfName,'E')
            ELSE IF ((curAzimuth >= 135.d0) .AND. (curAzimuth < 225.d0)) THEN
              CALL PreDefTableEntry(pdchFenDir,surfName,'S')
            ELSE IF ((curAzimuth >= 225.d0) .AND. (curAzimuth < 315.d0)) THEN
              CALL PreDefTableEntry(pdchFenDir,surfName,'W')
            ENDIF
          END IF
          curWSC = Surface(iSurf)%WindowShadingControlPtr
          !compute totals for area weighted averages
          fenTotArea = fenTotArea + windowAreaWMult
          ufactArea = ufactArea + nomUfact * windowAreaWMult
          shgcArea = shgcArea + SHGCSummer * windowAreaWMult
          vistranArea = vistranArea + TransVisNorm * windowAreaWMult
          IF (isNorth) THEN
            fenTotAreaNorth = fenTotAreaNorth + windowAreaWMult
            ufactAreaNorth = ufactAreaNorth + nomUfact * windowAreaWMult
            shgcAreaNorth = shgcAreaNorth + SHGCSummer * windowAreaWMult
            vistranAreaNorth = vistranAreaNorth + TransVisNorm * windowAreaWMult
          ELSE
            fenTotAreaNonNorth = fenTotAreaNonNorth + windowAreaWMult
            ufactAreaNonNorth = ufactAreaNonNorth + nomUfact * windowAreaWMult
            shgcAreaNonNorth = shgcAreaNonNorth + SHGCSummer * windowAreaWMult
            vistranAreaNonNorth = vistranAreaNonNorth + TransVisNorm * windowAreaWMult
          END IF
          ! shading
          IF (curWSC /= 0) THEN
            CALL PreDefTableEntry(pdchFenSwitchable,surfName,'Yes')
            !shading report
            CALL PreDefTableEntry(pdchWscName,surfName,WindowShadingControl(curWsc)%name)
            SELECT CASE (WindowShadingControl(curWsc)%ShadingType)
              CASE (WSC_ST_NoShade)
                CALL PreDefTableEntry(pdchWscShading,surfName,'No Shade')
              CASE (WSC_ST_InteriorShade)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Interior Shade')
              CASE (WSC_ST_SwitchableGlazing)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Switchable Glazing')
              CASE (WSC_ST_ExteriorShade)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Exterior Shade')
              CASE (WSC_ST_InteriorBlind)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Interior Blind')
              CASE (WSC_ST_ExteriorBlind)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Exterior Blind')
              CASE (WSC_ST_BetweenGlassShade)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Between Glass Shade')
              CASE (WSC_ST_BetweenGlassBlind)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Between Glass Blind')
              CASE (WSC_ST_ExteriorScreen)
                CALL PreDefTableEntry(pdchWscShading,surfName,'Exterior Screen')
            END SELECT
            SELECT CASE (WindowShadingControl(curWsc)%ShadingControlType)
              CASE (WSCT_AlwaysOn)
                CALL PreDefTableEntry(pdchWscControl,surfName,'AlwaysOn')
              CASE (WSCT_AlwaysOff)
                CALL PreDefTableEntry(pdchWscControl,surfName,'AlwaysOff')
              CASE (WSCT_OnIfScheduled)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfScheduleAllows')
              CASE (WSCT_HiSolar)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighSolarOnWindow')
              CASE (WSCT_HiHorzSolar)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighHorizontalSolar')
              CASE (WSCT_HiOutAirTemp)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighOutdoorAirTemperature')
              CASE (WSCT_HiZoneAirTemp)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighZoneAirTemperature')
              CASE (WSCT_HiZoneCooling)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighZoneCooling')
              CASE (WSCT_HiGlare)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighGlare')
              CASE (WSCT_MeetDaylIlumSetp)
                CALL PreDefTableEntry(pdchWscControl,surfName,'MeetDaylightIlluminanceSetpoint')
              CASE (WSCT_OnNightLoOutTemp_OffDay)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnNightIfLowOutdoorTempAndOffDay')
              CASE (WSCT_OnNightLoInTemp_OffDay)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnNightIfLowInsideTempAndOffDay')
              CASE (WSCT_OnNightIfHeating_OffDay)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnNightIfHeatingAndOffDay')
              CASE (WSCT_OnNightLoOutTemp_OnDayCooling)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnNightIfLowOutdoorTempAndOnDayIfCooling')
              CASE (WSCT_OnNightIfHeating_OnDayCooling)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnNightIfHeatingAndOnDayIfCooling')
              CASE (WSCT_OffNight_OnDay_HiSolarWindow)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OffNightAndOnDayIfCoolingAndHighSolarOnWindow')
              CASE (WSCT_OnNight_OnDay_HiSolarWindow)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnNightAndOnDayIfCoolingAndHighSolarOnWindow')
              CASE (WSCT_OnHiOutTemp_HiSolarWindow)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighOutdoorAirTempAndHighSolarOnWindow')
              CASE (WSCT_OnHiOutTemp_HiHorzSolar)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighOutdoorAirTempAndHighHorizontalSolar')
              CASE (WSCT_OnHiZoneTemp_HiSolarWindow)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighZoneAirTempAndHighSolarOnWindow')
              CASE (WSCT_OnHiZoneTemp_HiHorzSolar)
                CALL PreDefTableEntry(pdchWscControl,surfName,'OnIfHighZoneAirTempAndHighHorizontalSolar')
            END SELECT
            IF (WindowShadingControl(curWsc)%ShadedConstruction /= 0) THEN
              CALL PreDefTableEntry(pdchWscShadCons,surfName,Construct(WindowShadingControl(curWsc)%ShadedConstruction)%Name)
            END IF
            IF (WindowShadingControl(curWsc)%GlareControlIsActive) THEN
              CALL PreDefTableEntry(pdchWscGlare,surfName,'Yes')
            ELSE
              CALL PreDefTableEntry(pdchWscGlare,surfName,'No')
            END IF
          ELSE
            CALL PreDefTableEntry(pdchFenSwitchable,surfName,'No')
          END IF
       CASE (SurfaceClass_Door)
          surfName = Surface(iSurf)%Name
          curCons = Surface(iSurf)%Construction
          CALL PreDefTableEntry(pdchDrCons,surfName,Construct(curCons)%Name)
          CALL PreDefTableEntry(pdchDrUfactNoFilm,surfName,NominalU(Surface(iSurf)%Construction),3)
          mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier
          CALL PreDefTableEntry(pdchDrGrArea,surfName,Surface(iSurf)%GrossArea * mult)
          CALL PreDefTableEntry(pdchDrParent,surfName,Surface(iSurf)%BaseSurfName)

     END SELECT
    ELSE
      isExterior = .FALSE.
      !interior window report
      IF (Surface(iSurf)%Class .EQ. SurfaceClass_Window) THEN
        IF (Surface(iSurf)%Name(1:3) .NE. 'iz-') THEN !don't count created interzone surfaces that are mirrors of other surfaces
          surfName = Surface(iSurf)%Name
          curCons = Surface(iSurf)%Construction
          CALL PreDefTableEntry(pdchIntFenCons,surfName,Construct(curCons)%Name)
          zonePt = Surface(iSurf)%Zone
          mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier * Surface(iSurf)%Multiplier
          !include the frame area if present
          windowArea = Surface(iSurf)%GrossArea
          IF  (Surface(iSurf)%FrameDivider /= 0) THEN
            frameWidth = FrameDivider(Surface(iSurf)%FrameDivider)%FrameWidth
            frameArea = (Surface(iSurf)%Height + 2*frameWidth)*(Surface(iSurf)%Width + 2*frameWidth) &
                        - (Surface(iSurf)%Height * Surface(iSurf)%Width)
            windowArea = windowArea + frameArea
          END IF
          windowAreaWMult = windowArea * mult
          CALL PreDefTableEntry(pdchIntFenAreaOf1,surfName,windowArea)
          CALL PreDefTableEntry(pdchIntFenArea,surfName,windowAreaWMult)
          nomUfact = NominalU(Surface(iSurf)%Construction)
          CALL PreDefTableEntry(pdchIntFenUfact,surfName,nomUfact,3)
          !if the construction report is requested the SummerSHGC is already calculated
          IF (Construct(curCons)%SummerSHGC /= 0) THEN
            SHGCSummer = Construct(curCons)%SummerSHGC
            TransVisNorm = Construct(curCons)%VisTransNorm
          ELSE
            !must calculate Summer SHGC
            IF (.NOT. Construct(curCons)%WindowTypeEQL) THEN
                CALL CalcNominalWindowCond(curCons,2,nomCond,SHGCSummer,TransSolNorm,TransVisNorm,ErrFlag)
            ENDIF
          END IF
          CALL PreDefTableEntry(pdchIntFenSHGC,surfName,SHGCSummer,3)
          CALL PreDefTableEntry(pdchIntFenVisTr,surfName,TransVisNorm,3)
          CALL PreDefTableEntry(pdchIntFenParent,surfName,Surface(iSurf)%BaseSurfName)
          !compute totals for area weighted averages
          intFenTotArea = intFenTotArea + windowAreaWMult
          intUfactArea = intUfactArea + nomUfact * windowAreaWMult
          intShgcArea = intShgcArea + SHGCSummer * windowAreaWMult
          intVistranArea = intVistranArea + TransVisNorm * windowAreaWMult
        END IF
      END IF
    END IF
    ! do counts - use classification (note using numeric.  if class assignments change, this won't work)
    IF ((Surface(iSurf)%Class <= 20) .AND. (Surface(iSurf)%Class >= 1)) THEN
      numSurfaces(Surface(iSurf)%Class) = numSurfaces(Surface(iSurf)%Class) + 1
      IF (isExterior) THEN
        numExtSurfaces(Surface(iSurf)%Class) = numExtSurfaces(Surface(iSurf)%Class) + 1
      END IF
    END IF
  END DO
  ! total
  CALL PreDefTableEntry(pdchFenArea,"Total or Average",fenTotArea)
  IF (fenTotArea > 0.0d0) THEN
    CALL PreDefTableEntry(pdchFenUfact,"Total or Average",ufactArea / fenTotArea,3)
    CALL PreDefTableEntry(pdchFenSHGC,"Total or Average",shgcArea / fenTotArea,3)
    CALL PreDefTableEntry(pdchFenVisTr,"Total or Average",vistranArea / fenTotArea,3)
  ELSE
    CALL PreDefTableEntry(pdchFenUfact,"Total or Average","-")
    CALL PreDefTableEntry(pdchFenSHGC,"Total or Average","-")
    CALL PreDefTableEntry(pdchFenVisTr,"Total or Average","-")
  ENDIF
  ! north
  CALL PreDefTableEntry(pdchFenArea,"North Total or Average",fenTotAreaNorth)
  IF (fenTotAreaNorth > 0.0d0) THEN
    CALL PreDefTableEntry(pdchFenUfact,"North Total or Average",ufactAreaNorth / fenTotAreaNorth,3)
    CALL PreDefTableEntry(pdchFenSHGC,"North Total or Average",shgcAreaNorth / fenTotAreaNorth,3)
    CALL PreDefTableEntry(pdchFenVisTr,"North Total or Average",vistranAreaNorth / fenTotAreaNorth,3)
  ELSE
    CALL PreDefTableEntry(pdchFenUfact,"North Total or Average","-")
    CALL PreDefTableEntry(pdchFenSHGC,"North Total or Average","-")
    CALL PreDefTableEntry(pdchFenVisTr,"North Total or Average","-")
  ENDIF
  ! non-north
  CALL PreDefTableEntry(pdchFenArea,"Non-North Total or Average",fenTotAreaNonNorth)
  IF (fenTotAreaNonNorth > 0.0d0) THEN
    CALL PreDefTableEntry(pdchFenUfact,"Non-North Total or Average",ufactAreaNonNorth / fenTotAreaNonNorth,3)
    CALL PreDefTableEntry(pdchFenSHGC,"Non-North Total or Average",shgcAreaNonNorth / fenTotAreaNonNorth,3)
    CALL PreDefTableEntry(pdchFenVisTr,"Non-North Total or Average",vistranAreaNonNorth / fenTotAreaNonNorth,3)
  ELSE
    CALL PreDefTableEntry(pdchFenUfact,"Non-North Total or Average","-")
    CALL PreDefTableEntry(pdchFenSHGC,"Non-North Total or Average","-")
    CALL PreDefTableEntry(pdchFenVisTr,"Non-North Total or Average","-")
  ENDIF
  !interior fenestration totals
  CALL PreDefTableEntry(pdchIntFenArea,"Total or Average",intFenTotArea)
  IF (intFenTotArea > 0.0d0) THEN
    CALL PreDefTableEntry(pdchIntFenUfact,"Total or Average",intUfactArea / intFenTotArea,3)
    CALL PreDefTableEntry(pdchIntFenSHGC,"Total or Average",intShgcArea / intFenTotArea,3)
    CALL PreDefTableEntry(pdchIntFenVisTr,"Total or Average",intVistranArea / intFenTotArea,3)
  ELSE
    CALL PreDefTableEntry(pdchIntFenUfact,"Total or Average","-")
    CALL PreDefTableEntry(pdchIntFenSHGC,"Total or Average","-")
    CALL PreDefTableEntry(pdchIntFenVisTr,"Total or Average","-")
  ENDIF
  !counts
  CALL PreDefTableEntry(pdchSurfCntTot,'Wall',numSurfaces(SurfaceClass_Wall))
  CALL PreDefTableEntry(pdchSurfCntExt,'Wall',numExtSurfaces(SurfaceClass_Wall))
  CALL PreDefTableEntry(pdchSurfCntTot,'Floor',numSurfaces(SurfaceClass_Floor))
  CALL PreDefTableEntry(pdchSurfCntExt,'Floor',numExtSurfaces(SurfaceClass_Floor))
  CALL PreDefTableEntry(pdchSurfCntTot,'Roof',numSurfaces(SurfaceClass_Roof))
  CALL PreDefTableEntry(pdchSurfCntExt,'Roof',numExtSurfaces(SurfaceClass_Roof))
  CALL PreDefTableEntry(pdchSurfCntTot,'Internal Mass',numSurfaces(SurfaceClass_IntMass))
  CALL PreDefTableEntry(pdchSurfCntExt,'Internal Mass',numExtSurfaces(SurfaceClass_IntMass))
  CALL PreDefTableEntry(pdchSurfCntTot,'Building Detached Shading',numSurfaces(SurfaceClass_Detached_B))
  CALL PreDefTableEntry(pdchSurfCntExt,'Building Detached Shading',numExtSurfaces(SurfaceClass_Detached_B))
  CALL PreDefTableEntry(pdchSurfCntTot,'Fixed Detached Shading',numSurfaces(SurfaceClass_Detached_F))
  CALL PreDefTableEntry(pdchSurfCntExt,'Fixed Detached Shading',numExtSurfaces(SurfaceClass_Detached_F))
  CALL PreDefTableEntry(pdchSurfCntTot,'Window',numSurfaces(SurfaceClass_Window))
  CALL PreDefTableEntry(pdchSurfCntExt,'Window',numExtSurfaces(SurfaceClass_Window))
  CALL PreDefTableEntry(pdchSurfCntTot,'Door',numSurfaces(SurfaceClass_Door))
  CALL PreDefTableEntry(pdchSurfCntExt,'Door',numExtSurfaces(SurfaceClass_Door))
  CALL PreDefTableEntry(pdchSurfCntTot,'Glass Door',numSurfaces(SurfaceClass_GlassDoor))
  CALL PreDefTableEntry(pdchSurfCntExt,'Glass Door',numExtSurfaces(SurfaceClass_GlassDoor))
  CALL PreDefTableEntry(pdchSurfCntTot,'Shading',numSurfaces(SurfaceClass_Shading))
  CALL PreDefTableEntry(pdchSurfCntExt,'Shading',numExtSurfaces(SurfaceClass_Shading))
  CALL PreDefTableEntry(pdchSurfCntTot,'Overhang',numSurfaces(SurfaceClass_Overhang))
  CALL PreDefTableEntry(pdchSurfCntExt,'Overhang',numExtSurfaces(SurfaceClass_Overhang))
  CALL PreDefTableEntry(pdchSurfCntTot,'Fin',numSurfaces(SurfaceClass_Fin))
  CALL PreDefTableEntry(pdchSurfCntExt,'Fin',numExtSurfaces(SurfaceClass_Fin))
  CALL PreDefTableEntry(pdchSurfCntTot,'Tubular Daylighting Device Dome',numSurfaces(SurfaceClass_TDD_Dome))
  CALL PreDefTableEntry(pdchSurfCntExt,'Tubular Daylighting Device Dome',numExtSurfaces(SurfaceClass_TDD_Dome))
  CALL PreDefTableEntry(pdchSurfCntTot,'Tubular Daylighting Device Diffuser',numSurfaces(SurfaceClass_TDD_Diffuser))
  CALL PreDefTableEntry(pdchSurfCntExt,'Tubular Daylighting Device Diffuser',numExtSurfaces(SurfaceClass_TDD_Diffuser))
END SUBROUTINE GatherForPredefinedReport



SUBROUTINE AllocateSurfaceHeatBalArrays  ! Heat Balance Array Allocation done at BeginSim Flag

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger variable allocation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE DataRoomAirModel, ONLY: IsZoneDV,IsZoneCV,HVACMassFlow, ZoneDVMixedFlag

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop

          ! FLOW:

! Use the total number of surfaces to allocate variables to avoid a surface number limit
  ALLOCATE (CTFConstInPart(TotSurfaces))
  CTFConstInPart=0.0d0
  ALLOCATE (CTFConstOutPart(TotSurfaces))
  CTFConstOutPart=0.0d0
  ALLOCATE (CTFTsrcConstPart(TotSurfaces))
  CTFTsrcConstPart=0.0d0
  ALLOCATE (TempEffBulkAir(TotSurfaces))
  TempEffBulkAir=23.0d0
  ALLOCATE (HConvIn(TotSurfaces))
  HConvIn=0.0d0
  ALLOCATE (HcExtSurf(TotSurfaces))
  HcExtSurf=0.0d0
  ALLOCATE (HAirExtSurf(TotSurfaces))
  HAirExtSurf=0.0d0
  ALLOCATE (HSkyExtSurf(TotSurfaces))
  HSkyExtSurf=0.0d0
  ALLOCATE (HGrdExtSurf(TotSurfaces))
  HGrdExtSurf=0.0d0
  ALLOCATE (TempSurfIn(TotSurfaces))
  TempSurfIn=0.0d0
  ALLOCATE (TempSurfInTmp(TotSurfaces))
  TempSurfInTmp=0.0d0
  ALLOCATE (QRadSWOutAbs(TotSurfaces))
  QRadSWOutAbs=0.0d0
  ALLOCATE (QRadSWInAbs(TotSurfaces))
  QRadSWInAbs=0.0d0
  ALLOCATE (InitialDifSolInAbs(TotSurfaces))
  InitialDifSolInAbs=0.0d0
  ALLOCATE (InitialDifSolInTrans(TotSurfaces))
  InitialDifSolInTrans=0.0d0
  ALLOCATE (QRadSWwinAbs(TotSurfaces,CFSMAXNL+1))
  QRadSWwinAbs=0.0d0
  ALLOCATE (InitialDifSolwinAbs(TotSurfaces,CFSMAXNL))
  InitialDifSolwinAbs=0.0d0
  ALLOCATE (QRadSWOutMvIns(TotSurfaces))
  QRadSWOutMvIns=0.0d0
  ALLOCATE (QRadThermInAbs(TotSurfaces))
  QRadThermInAbs=0.0d0
  ALLOCATE (SUMH(TotSurfaces))
  SUMH=0

  ALLOCATE (TH(TotSurfaces,MaxCTFTerms,2))
  TH=0.0d0
  ALLOCATE (TempSurfOut(TotSurfaces))
  TempSurfOut = 0.d0
  ALLOCATE (TempSurfInRep(TotSurfaces))
  TempSurfInRep = 0.d0
  ALLOCATE (QConvInReport(TotSurfaces))
  QConvInReport = 0.d0
  ALLOCATE (QdotConvInRepPerArea(TotSurfaces))
  QdotConvInRepPerArea = 0.d0
  ALLOCATE (QdotConvInRep(TotSurfaces))
  QdotConvInRep = 0.d0

  ALLOCATE ( QRadNetSurfInReport(TotSurfaces))
  QRadNetSurfInReport   = 0.d0
  ALLOCATE ( QdotRadNetSurfInRep(TotSurfaces))
  QdotRadNetSurfInRep   = 0.d0
  ALLOCATE  ( QdotRadNetSurfInRepPerArea(TotSurfaces))
  QdotRadNetSurfInRepPerArea  = 0.d0

  ALLOCATE ( QRadSolarInReport(TotSurfaces))
  QRadSolarInReport   = 0.d0
  ALLOCATE ( QdotRadSolarInRep(TotSurfaces))
  QdotRadSolarInRep   = 0.d0
  ALLOCATE  ( QdotRadSolarInRepPerArea(TotSurfaces))
  QdotRadSolarInRepPerArea  = 0.d0

  ALLOCATE ( QRadLightsInReport(TotSurfaces))
  QRadLightsInReport   = 0.d0
  ALLOCATE ( QdotRadLightsInRep(TotSurfaces))
  QdotRadLightsInRep   = 0.d0
  ALLOCATE  ( QdotRadLightsInRepPerArea(TotSurfaces))
  QdotRadLightsInRepPerArea  = 0.d0

  ALLOCATE ( QRadIntGainsInReport(TotSurfaces))
  QRadIntGainsInReport   = 0.d0
  ALLOCATE ( QdotRadIntGainsInRep(TotSurfaces))
  QdotRadIntGainsInRep   = 0.d0
  ALLOCATE  ( QdotRadIntGainsInRepPerArea(TotSurfaces))
  QdotRadIntGainsInRepPerArea  = 0.d0

  ALLOCATE ( QRadHVACInReport(TotSurfaces))
  QRadHVACInReport   = 0.d0
  ALLOCATE ( QdotRadHVACInRep(TotSurfaces))
  QdotRadHVACInRep   = 0.d0
  ALLOCATE  ( QdotRadHVACInRepPerArea(TotSurfaces))
  QdotRadHVACInRepPerArea  = 0.d0

  ALLOCATE (QConvOutReport(TotSurfaces))
  QConvOutReport = 0.d0
  ALLOCATE (QdotConvOutRepPerArea(TotSurfaces))
  QdotConvOutRepPerArea = 0.d0
  ALLOCATE (QdotConvOutRep(TotSurfaces))
  QdotConvOutRep = 0.d0

  ALLOCATE (QdotRadOutRep(TotSurfaces))
  QdotRadOutRep = 0.d0
  ALLOCATE (QdotRadOutRepPerArea(TotSurfaces))
  QdotRadOutRepPerArea = 0.d0
  ALLOCATE (QRadOutReport(TotSurfaces))
  QRadOutReport = 0.d0


  ALLOCATE (OpaqSurfInsFaceConduction(TotSurfaces))
  OpaqSurfInsFaceConduction=0.d0
  ALLOCATE(OpaqSurfInsFaceConductionFlux(TotSurfaces))
  OpaqSurfInsFaceConductionFlux = 0.d0
  ALLOCATE (OpaqSurfInsFaceCondGainRep(TotSurfaces))
  OpaqSurfInsFaceCondGainRep=0.d0
  ALLOCATE (OpaqSurfInsFaceCondLossRep(TotSurfaces))
  OpaqSurfInsFaceCondLossRep=0.d0
  ALLOCATE (OpaqSurfInsFaceConductionEnergy(TotSurfaces))
  OpaqSurfInsFaceConductionEnergy = 0.d0

  ALLOCATE(SWOutAbsTotalReport(TotSurfaces))
  SWOutAbsTotalReport=0.0d0
  ALLOCATE(SWOutAbsEnergyReport(TotSurfaces))
  SWOutAbsEnergyReport=0.0d0

  ALLOCATE (OpaqSurfOutsideFaceConduction(TotSurfaces))
  OpaqSurfOutsideFaceConduction       = 0.d0
  ALLOCATE(OpaqSurfOutsideFaceConductionFlux(TotSurfaces))
  OpaqSurfOutsideFaceConductionFlux   = 0.d0
  ALLOCATE (OpaqSurfExtFaceCondGainRep(TotSurfaces))
  OpaqSurfExtFaceCondGainRep          = 0.d0
  ALLOCATE (OpaqSurfExtFaceCondLossRep(TotSurfaces))
  OpaqSurfExtFaceCondLossRep          = 0.d0
  ALLOCATE (OpaqSurfOutsideFaceConductionEnergy(TotSurfaces))
  OpaqSurfOutsideFaceConductionEnergy = 0.d0

  ALLOCATE (OpaqSurfAvgFaceCondGainRep(TotSurfaces))
  OpaqSurfAvgFaceCondGainRep      = 0.d0
  ALLOCATE (OpaqSurfAvgFaceCondLossRep(TotSurfaces))
  OpaqSurfAvgFaceCondLossRep      = 0.d0
  ALLOCATE (OpaqSurfAvgFaceConduction(TotSurfaces))
  OpaqSurfAvgFaceConduction       = 0.d0
  ALLOCATE ( OpaqSurfAvgFaceConductionFlux(TotSurfaces))
  OpaqSurfAvgFaceConductionFlux   = 0.d0
  ALLOCATE (OpaqSurfAvgFaceConductionEnergy(TotSurfaces))
  OpaqSurfAvgFaceConductionEnergy = 0.d0

  ALLOCATE ( OpaqSurfStorageGainRep(TotSurfaces))
  OpaqSurfStorageGainRep  = 0.d0
  ALLOCATE ( OpaqSurfStorageCondLossRep(TotSurfaces))
  OpaqSurfStorageCondLossRep  = 0.d0
  ALLOCATE ( OpaqSurfStorageConduction(TotSurfaces))
  OpaqSurfStorageConduction  = 0.d0
  ALLOCATE ( OpaqSurfStorageConductionFlux(TotSurfaces))
  OpaqSurfStorageConductionFlux  = 0.d0
  ALLOCATE ( OpaqSurfStorageConductionEnergy(TotSurfaces))
  OpaqSurfStorageConductionEnergy = 0.d0

  ALLOCATE (OpaqSurfInsFaceBeamSolAbsorbed(TotSurfaces))
  OpaqSurfInsFaceBeamSolAbsorbed=0.0d0
  ALLOCATE (TempSource(TotSurfaces))
  TempSource = 0.0d0
  ALLOCATE (QH(TotSurfaces,MaxCTFTerms,2))
  QH=0.0d0
  ALLOCATE (THM(TotSurfaces,MaxCTFTerms,2))
  THM=0.0d0
  ALLOCATE (QHM(TotSurfaces,MaxCTFTerms,2))
  QHM=0.0d0
  ALLOCATE (TsrcHist(TotSurfaces,MaxCTFTerms))
  TsrcHist = 0.0d0
  ALLOCATE (QsrcHist(TotSurfaces,MaxCTFTerms))
  QsrcHist = 0.0d0
  ALLOCATE (TsrcHistM(TotSurfaces,MaxCTFTerms))
  TsrcHistM = 0.0d0
  ALLOCATE (QsrcHistM(TotSurfaces,MaxCTFTerms))
  QsrcHistM = 0.0d0

  ALLOCATE (NetLWRadToSurf(TotSurfaces))
  NetLWRadToSurf = 0.0d0
  ALLOCATE (QRadSWLightsInAbs(TotSurfaces))
  QRadSWLightsInAbs = 0.d0

  ALLOCATE (RadSysTiHBConstCoef(TotSurfaces))
  RadSysTiHBConstCoef = 0.0d0
  ALLOCATE (RadSysTiHBToutCoef(TotSurfaces))
  RadSysTiHBToutCoef = 0.0d0
  ALLOCATE (RadSysTiHBQsrcCoef(TotSurfaces))
  RadSysTiHBQsrcCoef = 0.0d0
  ALLOCATE (RadSysToHBConstCoef(TotSurfaces))
  RadSysToHBConstCoef = 0.0d0
  ALLOCATE (RadSysToHBTinCoef(TotSurfaces))
  RadSysToHBTinCoef = 0.0d0
  ALLOCATE (RadSysToHBQsrcCoef(TotSurfaces))
  RadSysToHBQsrcCoef = 0.0d0
  ALLOCATE(QRadSysSource(TotSurfaces))
  QRadSysSource = 0.0D0
  ALLOCATE (TCondFDSourceNode(TotSurfaces))
  TcondFDSourceNode = 15.D0
  ALLOCATE(QHTRadSysSurf(TotSurfaces))
  QHTRadSysSurf = 0.0D0
  ALLOCATE(QHWBaseboardSurf(TotSurfaces))
  QHWBaseboardSurf = 0.0D0
  ALLOCATE(QSteamBaseboardSurf(TotSurfaces))
  QSteamBaseboardSurf = 0.0D0
  ALLOCATE(QElecBaseboardSurf(TotSurfaces))
  QElecBaseboardSurf = 0.0D0

  ! allocate term used as sink for PV electricity
  ALLOCATE(QPVSysSource(TotSurfaces))
  QPVSysSource = 0.0D0

!Allocate the moisture balance arrays
  ALLOCATE(TempOutsideAirFD(TotSurfaces))
  TempOutsideAirFD=0.0D0
  ALLOCATE(RhoVaporAirOut(TotSurfaces))
  RhoVaporAirOut=0.0d0
  ALLOCATE(RhoVaporSurfIn(TotSurfaces))
  RhoVaporSurfIn=0.0d0
  ALLOCATE(RhoVaporAirIn(TotSurfaces))
  RhoVaporAirIn=0.0d0
  ALLOCATE(HConvExtFD(TotSurfaces))
  HConvExtFD=0.0d0
  ALLOCATE(HMassConvExtFD(TotSurfaces))
  HMassConvExtFD=0.0d0
  ALLOCATE(HConvInFD(TotSurfaces))
  HConvInFD=0.0d0
  ALLOCATE(HMassConvInFD(TotSurfaces))
  HMassConvInFD=0.0d0
  ALLOCATE(HSkyFD(TotSurfaces))
  HSkyFD=0.0d0
  ALLOCATE(HGrndFD(TotSurfaces))
  HGrndFD=0.0d0
  ALLOCATE(HAirFD(TotSurfaces))
  HAirFD=0.0d0

  CALL DisplayString('Setting up Surface Reporting Variables')

  ! Setup surface report variables CurrentModuleObject='Opaque Surfaces'
  DO Loop=1,TotSurfaces
    IF (.not. Surface(Loop)%HeatTransSurf) CYCLE
    CALL SetupOutputVariable('Surface Inside Face Temperature [C]',TempSurfInRep(Loop),'Zone','State',Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Outside Face Temperature [C]',TempSurfOut(Loop),'Zone','State',Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Adjacent Air Temperature [C]',TempEffBulkAir(Loop),'Zone','State', &
                              Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Convection Heat Transfer Coefficient [W/m2-K]',HConvIn(Loop),'Zone', &
                              'State',Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Convection Heat Gain Rate [W]', QdotConvInRep(loop), 'Zone', 'State', &
                              Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Convection Heat Gain Rate per Area [W/m2]', QdotConvInRepPerArea(loop), &
                              'Zone', 'State',  Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Convection Heat Gain Energy [J]',QConvInReport(loop), 'Zone', 'Sum', &
                              Surface(Loop)%Name)

    CALL SetupOutputVariable('Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate [W]', QdotRadNetSurfInRep(loop), &
                              'Zone', 'State', Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Net Surface Thermal Radiation Heat Gain Rate per Area [W/m2]', &
                               QdotRadNetSurfInRepPerArea(loop), 'Zone', 'State', Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Net Surface Thermal Radiation Heat Gain Energy [J]',QRadNetSurfInReport(loop), &
                               'Zone', 'Sum', Surface(Loop)%Name)

    IF(Surface(Loop)%Class /= SurfaceClass_Window) THEN
      CALL SetupOutputVariable('Surface Inside Face Solar Radiation Heat Gain Rate [W]',  QdotRadSolarInRep(loop), 'Zone',  &
                                'State', Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Inside Face Solar Radiation Heat Gain Rate per Area [W/m2]',  &
                                 QdotRadSolarInRepPerArea(loop), 'Zone', 'State',   Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Inside Face Solar Radiation Heat Gain Energy [J]', QRadSolarInReport(loop), &
                                'Zone', 'Sum', Surface(Loop)%Name)

      CALL SetupOutputVariable('Surface Inside Face Lights Radiation Heat Gain Rate [W]',  QdotRadLightsInRep(loop), &
                                'Zone', 'State', Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Inside Face Lights Radiation Heat Gain Rate per Area [W/m2]',  &
                                QdotRadLightsInRepPerArea(loop), 'Zone', 'State',  Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Inside Face Lights Radiation Heat Gain Energy [J]', QRadLightsInReport(loop), &
                                'Zone', 'Sum', Surface(Loop)%Name)
    ENDIF

    CALL SetupOutputVariable('Surface Inside Face Internal Gains Radiation Heat Gain Rate [W]',  QdotRadIntGainsInRep(loop), &
                              'Zone', 'State', Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Internal Gains Radiation Heat Gain Rate per Area [W/m2]',  &
                              QdotRadIntGainsInRepPerArea(loop), 'Zone', 'State',  Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Internal Gains Radiation Heat Gain Energy [J]', QRadIntGainsInReport(loop), &
                              'Zone', 'Sum', Surface(Loop)%Name)

    CALL SetupOutputVariable('Surface Inside Face System Radiation Heat Gain Rate [W]',  QdotRadHVACInRep(loop), 'Zone', 'State',&
                                 Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face System Radiation Heat Gain Rate per Area [W/m2]',  &
                               QdotRadHVACInRepPerArea(loop), 'Zone', 'State',  Surface(Loop)%Name)
    CALL SetupOutputVariable('Surface Inside Face System Radiation Heat Gain Energy [J]', QRadHVACInReport(loop), 'Zone', 'Sum', &
                               Surface(Loop)%Name)

    IF (Surface(Loop)%ExtBoundCond == ExternalEnvironment .or. DisplayAdvancedReportVariables) THEN
      CALL SetupOutputVariable('Surface Outside Face Outdoor Air Drybulb Temperature [C]',Surface(Loop)%OutDryBulbTemp,&
                                'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Outdoor Air Wetbulb Temperature [C]',Surface(Loop)%OutWetBulbTemp,&
                                'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Outdoor Air Wind Speed [m/s]',Surface(Loop)%WindSpeed,&
                                'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Convection Heat Gain Rate [W]', QdotConvOutRep(loop), &
                                'Zone', 'State', Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Convection Heat Gain Rate per Area [W/m2]', QdotConvOutRepPerArea(loop),&
                                'Zone', 'State',   Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Convection Heat Gain Energy [J]',QConvOutReport(loop), 'Zone', 'Sum', &
                                                                                 Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Convection Heat Transfer Coefficient [W/m2-K]',HcExtSurf(Loop),&
                                'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Net Thermal Radiation Heat Gain Rate [W]', QdotRadOutRep(loop),   &
                                'Zone', 'State', Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Net Thermal Radiation Heat Gain Rate per Area [W/m2]',   &
         QdotRadOutRepPerArea(loop),  &
                                'Zone', 'State', Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Net Thermal Radiation Heat Gain Energy [J]',QRadOutReport(loop),   &
                                'Zone', 'Sum', Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Thermal Radiation to Air Heat Transfer Coefficient [W/m2-K]',  &
         HAirExtSurf(Loop),&
                                'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Thermal Radiation to Sky Heat Transfer Coefficient [W/m2-K]',  &
         HSkyExtSurf(Loop),&
                                'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Thermal Radiation to Ground Heat Transfer Coefficient [W/m2-K]',  &
         HGrdExtSurf(Loop),&
                                'Zone','State',Surface(Loop)%Name)
      IF(Surface(Loop)%Class /= SurfaceClass_Window) THEN
        CALL SetupOutputVariable('Surface Outside Face Solar Radiation Heat Gain Rate [W]',  &
                                  SWOutAbsTotalReport(Loop),'Zone','Average',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Solar Radiation Heat Gain Rate per Area [W/m2]',  &
                                  QRadSWOutAbs(Loop),'Zone','Average',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Solar Radiation Heat Gain Energy [J]', SWOutAbsEnergyReport(Loop),   &
                                   'Zone', 'Sum', Surface(Loop)%Name)
      ENDIF
    ENDIF
    IF(Surface(Loop)%Class==SurfaceClass_Floor .OR. Surface(Loop)%Class==SurfaceClass_Wall .OR.  &
       Surface(Loop)%Class==SurfaceClass_IntMass .or.  &
       Surface(Loop)%Class==SurfaceClass_Roof  .OR. Surface(Loop)%Class==SurfaceClass_Door) THEN
!      IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
        CALL SetupOutputVariable('Surface Inside Face Conduction Heat Transfer Rate [W]',OpaqSurfInsFaceConduction(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Inside Face Conduction Heat Gain Rate [W]',OpaqSurfInsFaceCondGainRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Inside Face Conduction Heat Loss Rate [W]',OpaqSurfInsFaceCondLossRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Inside Face Conduction Heat Transfer Rate per Area [W/m2]', &
                                  OpaqSurfInsFaceConductionFlux(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Inside Face Conduction Heat Transfer Energy [J]', &
                                   OpaqSurfInsFaceConductionEnergy(Loop), 'Zone', 'Sum', Surface(Loop)%Name)

        CALL SetupOutputVariable('Surface Outside Face Conduction Heat Transfer Rate [W]',OpaqSurfOutsideFaceConduction(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Conduction Heat Gain Rate [W]',OpaqSurfExtFaceCondGainRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Conduction Heat Loss Rate [W]',OpaqSurfExtFaceCondLossRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Conduction Heat Transfer Rate per Area [W/m2]',&
                                   OpaqSurfOutsideFaceConductionFlux(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Conduction Heat Transfer Energy [J]', &
                                   OpaqSurfOutsideFaceConductionEnergy(Loop), 'Zone', 'Sum', Surface(Loop)%Name)

        CALL SetupOutputVariable('Surface Average Face Conduction Heat Transfer Rate [W]',OpaqSurfAvgFaceConduction(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Average Face Conduction Heat Gain Rate [W]',OpaqSurfAvgFaceCondGainRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Average Face Conduction Heat Loss Rate [W]',OpaqSurfAvgFaceCondLossRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Average Face Conduction Heat Transfer Rate per Area [W/m2]', &
                                   OpaqSurfAvgFaceConductionFlux(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Average Face Conduction Heat Transfer Energy [J]', &
                                   OpaqSurfAvgFaceConductionEnergy(Loop), 'Zone', 'Sum', Surface(Loop)%Name)

        CALL SetupOutputVariable('Surface Heat Storage Rate [W]',OpaqSurfStorageConduction(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Heat Storage Gain Rate [W]',OpaqSurfStorageGainRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Heat Storage Loss Rate [W]',OpaqSurfStorageCondLossRep(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Heat Storage Rate per Area [W/m2]',OpaqSurfStorageConductionFlux(Loop), &
                                  'Zone','State',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Heat Storage Energy [J]', &
                                   OpaqSurfStorageConductionEnergy(Loop), 'Zone', 'Sum', Surface(Loop)%Name)

!      ENDIF
        !CurrentModuleObject='Opaque Surfaces'

      CALL SetupOutputVariable('Surface Inside Face Beam Solar Radiation Heat Gain Rate [W]',OpaqSurfInsFaceBeamSolAbsorbed(Loop), &
                        'Zone','State',Surface(Loop)%Name)
    END IF
    IF (Construct(Surface(Loop)%Construction)%SourceSinkPresent) &
      CALL SetupOutputVariable('Surface Internal Source Location Temperature [C]',  &
                               TempSource(Loop),'Zone','State',Surface(Loop)%Name)
    IF(Surface(Loop)%Class == SurfaceClass_Window) THEN  ! CurrentModuleObject='Windows'
      CALL SetupOutputVariable('Surface Shading Device Is On Time Fraction []',SurfaceWindow(Loop)%FracTimeShadingDeviceOn, &
                                          'Zone','Average',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Storm Window On Off Status []',SurfaceWindow(Loop)%StormWinFlag, &
                                          'Zone','State',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Window Blind Slat Angle [deg]',SurfaceWindow(Loop)%SlatAngThisTSDeg,  &
                               'Zone','State',Surface(Loop)%Name)
    END IF
!    IF (DisplayAdvancedReportVariables) THEN  !CurrentModuleObject='Opaque Surfaces(Advanced)'
      CALL SetupOutputVariable('Surface Inside Face Convection Classification Index [ ]',   &
                              Surface(Loop)%IntConvClassification, &
                              'Zone','Average',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Inside Face Convection Model Equation Index [ ]',   &
                               Surface(Loop)%IntConvHcModelEq, &
                              'Zone','Average',Surface(Loop)%Name)
      CALL SetupOutputVariable('Surface Inside Face Convection Reference Air Index [ ]',   &
                               Surface(Loop)%TairRef, &
                              'Zone','Average',Surface(Loop)%Name)
      IF (Surface(Loop)%ExtBoundCond == ExternalEnvironment) THEN
        CALL SetupOutputVariable('Surface Outside Face Convection Classification Index [ ]',   &
                              Surface(Loop)%OutConvClassification, &
                              'Zone','Average',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Forced Convection Model Equation Index [ ]',   &
                              Surface(Loop)%OutConvHfModelEq, &
                              'Zone','Average',Surface(Loop)%Name)
        CALL SetupOutputVariable('Surface Outside Face Natural Convection Model Equation Index [ ]',   &
                              Surface(Loop)%OutConvHnModelEq, &
                              'Zone','Average',Surface(Loop)%Name)
      ENDIF
!     ENDIF
    IF (DisplayAdvancedReportVariables) THEN
      CALL SetupOutputVariable('Surface Construction Index []', Surface(Loop)%Construction, &
                               'Zone','Average',Surface(Loop)%Name)
    ENDIF

  ENDDO

!unused  ALLOCATE (QBV(NumOfZones))
!unused  QBV=0.0
  ALLOCATE (QC(NumOfZones))
  QC=0.0d0
  ALLOCATE (QD(NumOfZones))
  QD=0.0d0
  ALLOCATE (QDforDaylight(NumOfZones))
  QDforDaylight=0.0d0
  ALLOCATE (QDV(NumOfZones))
  QDV=0.0d0
  ALLOCATE(QL(NumOfZones))
  QL=0.0d0

!UCSD
  ALLOCATE(MRT(NumOfZones))
  MRT=0.0d0

  ! Allocate Reporting Variables and set up tracking
  ALLOCATE(ZoneMRT(NumOfZones))
  ZoneMRT=0.0d0

  DO Loop=1,NumOfZones
      !CurrentModuleObject='Zone'
    CALL SetupOutputVariable('Zone Mean Radiant Temperature [C]',ZoneMRT(Loop),'Zone','State',Zone(Loop)%Name)
  END DO

  RETURN

END SUBROUTINE AllocateSurfaceHeatBalArrays


SUBROUTINE InitThermalAndFluxHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   March 1978
          !       MODIFIED       na
          !       RE-ENGINEERED  Feb98 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets the initial temperature and flux histories
          ! needed for a stable and reasonable heat balance solution starting
          ! point.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine assumes that the simulation is at steady state at
          ! the beginning and then begins to vary.  Thus, the temperatures, the
          ! fluxes. and their histories can all be set to the same value.  Some
          ! of the initializations depend on the surface characteristics.  This
          ! requires a DO loop to perform the proper calculation.

          ! REFERENCES:
          ! (I)BLAST legacy routine INITTH

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum            ! DO loop counter for surfaces
  INTEGER :: OSCMnum            ! DO loop counter for Other side conditions modeled (OSCM)
          ! FLOW:

          ! First do the "bulk" initializations of arrays sized to NumOfZones
  MRT        = 23.D0  ! module level array
  MAT        = 23.d0  ! DataHeatBalFanSys array
  ZT         = 23.d0
  ZTAV       = 23.D0
  XMAT       = 23.d0  ! DataHeatBalFanSys array
  XM2T       = 23.d0  ! DataHeatBalFanSys array
  XM3T       = 23.d0  ! DataHeatBalFanSys array
  XM4T       = 23.d0
  XMPT       = 23.d0
  DSXMAT     = 23.d0  ! DataHeatBalFanSys array
  DSXM2T     = 23.d0  ! DataHeatBalFanSys array
  DSXM3T     = 23.d0  ! DataHeatBalFanSys array
  DSXM4T     = 23.d0
  ZoneTMX    = 23.d0  ! DataHeatBalFanSys array
  ZoneTM2    = 23.d0  ! DataHeatBalFanSys array
  !Initialize the Zone Humidity Ratio here so that it is available for EMPD implementations
  ZoneAirHumRatAvg = OutHumRat
  ZoneAirHumRat=OutHumRat
  ZoneAirHumRatOld=OutHumRat
  SumHmAW= 0.0D0
  SumHmARa= 0.0D0
  SumHmARaW= 0.0D0

          ! "Bulk" initializations of arrays sized to TotSurfaces
  SUMH          = 0     ! module level array
  TempSurfIn    = 23.d0 ! module level array
  TempSurfInTmp = 23.d0 ! module level array
  HConvIn       = 3.076d0 ! module level array
  HcExtSurf      = 0.d0
  HAirExtSurf      = 0.d0
  HSkyExtSurf      = 0.d0
  HGrdExtSurf      = 0.d0
  TempSurfOut   = 0.d0
  TempSurfInRep = 0.d0
  QConvInReport = 0.d0
  QdotConvInRep = 0.d0
  QdotConvInRepPerArea = 0.d0
  QRadNetSurfInReport  = 0.d0
  QdotRadNetSurfInRep  = 0.d0
  QdotRadNetSurfInRepPerArea = 0.d0
  QRadSolarInReport = 0.d0
  QdotRadSolarInRep = 0.d0
  QdotRadSolarInRepPerArea = 0.d0
  QRadLightsInReport = 0.d0
  QdotRadLightsInRep = 0.d0
  QdotRadLightsInRepPerArea = 0.d0
  QRadIntGainsInReport = 0.d0
  QdotRadIntGainsInRep = 0.d0
  QdotRadIntGainsInRepPerArea = 0.d0
  QRadHVACInReport = 0.d0
  QdotRadHVACInRep = 0.d0
  QdotRadHVACInRepPerArea = 0.d0
  QConvOutReport = 0.d0
  QdotConvOutRep = 0.d0
  QdotConvOutRepPerArea = 0.d0
  QRadOutReport = 0.d0
  QdotRadOutRep = 0.d0
  QdotRadOutRepPerArea = 0.0d0
  OpaqSurfInsFaceConduction = 0.d0
  OpaqSurfInsFaceConductionFlux = 0.d0
  OpaqSurfInsFaceConductionEnergy = 0.d0
  OpaqSurfInsFaceBeamSolAbsorbed = 0.d0
  TempEffBulkAir=23.0d0
  TempTstatAir=23.0d0

          ! "Bulk" initializations of temperature arrays with dimensions (TotSurface,MaxCTFTerms,2)
  TH        = 23.D0 ! module level array
  THM       = 23.D0 ! module level array
  TsrcHist  = 23.D0
  TsrcHistM = 23.D0
  QH        = 0.0D0
  QHM       = 0.0D0
  QsrcHist  = 0.0D0
  QsrcHistM = 0.0D0
  CondFDRelaxFactor=CondFDRelaxFactorInput
          ! Initialize window frame and divider temperatures
  SurfaceWindow%FrameTempSurfIn = 23.d0
  SurfaceWindow%FrameTempSurfInOld = 23.d0
  SurfaceWindow%FrameTempSurfOut = 23.d0
  SurfaceWindow%DividerTempSurfIn = 23.d0
  SurfaceWindow%DividerTempSurfInOld = 23.d0
  SurfaceWindow%DividerTempSurfOut = 23.d0

          ! Initialize previous-timestep shading indicators
  SurfaceWindow%ExtIntShadePrevTS = 0
  SurfaceWindow%ShadingFlag = ShadeOff

! Perform other initializations that depend on the surface characteristics
  DO SurfNum = 1, TotSurfaces

    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE  ! Skip non-heat transfer surfaces

          ! Reset outside boundary conditions if necessary
    IF ((Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) .OR. (Surface(SurfNum)%ExtBoundCond == OtherSideCondModeledExt)) THEN

      THM(SurfNum,1:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1) = Surface(SurfNum)%OutDryBulbTemp
      TH(SurfNum,1:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1) = Surface(SurfNum)%OutDryBulbTemp

    ELSEIF (Surface(SurfNum)%ExtBoundCond == Ground) THEN

      THM(SurfNum,1:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1) = GroundTemp
      TH(SurfNum,1:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1)  = GroundTemp

    ELSEIF (Surface(SurfNum)%ExtBoundCond == GroundFCfactorMethod) THEN

      THM(SurfNum,1:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1) = GroundTempFC
      TH(SurfNum,1:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1)  = GroundTempFC

    END IF

    IF (Surface(SurfNum)%ExtCavityPresent) THEN
      ExtVentedCavity(Surface(SurfNum)%ExtCavNum)%TbaffleLast = 20.0d0
      ExtVentedCavity(Surface(SurfNum)%ExtCavNum)%TairLast    = 20.0d0
    ENDIF

          ! Initialize the flux histories
    QH(SurfNum,2:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1)  = &
      Construct(Surface(SurfNum)%Construction)%UValue*(TH(SurfNum,1,1)-TH(SurfNum,1,2))
    QH(SurfNum,2:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,2)  = QH(SurfNum,2,1)
    QHM(SurfNum,2:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,1) = QH(SurfNum,2,1)
    QHM(SurfNum,2:Construct(Surface(SurfNum)%Construction)%NumCTFTerms+1,2) = QH(SurfNum,2,1)

  END DO

  IF (TotOSCM > 1) THEN
    DO OSCMnum =1,TotOSCM
      OSCM(OSCMnum)%TConv     = 20.0d0
      OSCM(OSCMnum)%Hconv     = 4.0d0 !
      OSCM(OSCMnum)%TRad      = 20.0d0 !
      OSCM(OSCMnum)%Hrad      = 4.0d0 !
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE InitThermalAndFluxHistories

SUBROUTINE InitSolarHeatGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Anonymous
          !       DATE WRITTEN   July 1977
          !       MODIFIED       Mar99 (FW): handle movable interior shades and
          !                                  switchable glazing
          !                      Oct99 (FW): account for Window5 glass calculation approach
          !                      May01 (FW): handle interior and exterior blinds
          !                      Sep03 (FW): initialize SurfaceWindow%FrameQRadOutAbs
          !                      May06 (RR): handle exterior window screens
          !       RE-ENGINEERED  Feb98 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the arrays associated with solar heat
          ! gains for both individual surfaces and for zones.  As a result,
          ! this routine sets the following variable arrays:
          ! QBV(unused), QDV, QC, QD; QRadSWOutAbs and QRadSWInAbs (for opaque surfaces);
          ! QRadSWwinAbs (for windows)

          ! METHODOLOGY EMPLOYED:
          ! If the sun is down, all of the pertinent arrays are zeroed.  If the
          ! sun is up, various calculations are made.

          ! REFERENCES:
          ! (I)BLAST legacy routine QSUN

          ! USE STATEMENTS:
  USE SolarShading, ONLY: CalcInteriorSolarDistribution
  USE HeatBalanceMovableInsulation
  USE General, ONLY: POLYF, InterpSw, InterpBlind, InterpProfAng, InterpSlatAng, InterpProfSlatAng, BlindBeamBeamTrans
  USE DataDaylightingDevices
  USE DaylightingDevices, ONLY: FindTDDPipe, TransTDD
  USE DataWindowEquivalentLayer
  USE SolarShading, ONLY: SurfaceScheduledSolarInc, WindowScheduledSolarAbs

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: AbsExt             ! Absorptivity of outer most layer (or movable insulation if present)
  INTEGER   :: ConstrNum          ! Index for the Construct derived type
  INTEGER   :: ConstrNumSh        ! Shaded window construction
  REAL(r64) :: HMovInsul          ! Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
  INTEGER   :: RoughIndexMovInsul ! Roughness index of movable insulation
  INTEGER   :: SurfNum            ! DO loop counter for surfaces
  INTEGER   :: SurfNum2           ! TDD:DOME object number
  INTEGER   :: PipeNum            ! TDD pipe object number
  INTEGER   :: ShelfNum           ! Daylighting shelf object number
  INTEGER   :: InShelfSurf        ! Inside daylighting shelf surface number
  INTEGER   :: OutShelfSurf       ! Outside daylighting shelf surface number
  REAL(r64) :: ShelfSolarRad      ! Shelf diffuse solar radiation
  INTEGER   :: ShadeFlag          ! Window shading flag
  REAL(r64) :: SwitchFac          ! Switching factor for switchable glazing
  INTEGER   :: ZoneNum            ! DO loop counter for zones
  REAL(r64) :: BeamSolar          ! Local variable for BeamSolarRad
  REAL(r64) :: SkySolarInc        ! Sky diffuse solar incident on a surface
  REAL(r64) :: GndSolarInc        ! Ground diffuse solar incident on a surface
  INTEGER   :: TotGlassLay        ! Number of glass layers
  INTEGER   :: TotSolidLay        ! Number of solid layers in fenestration system (glass + shading)
  INTEGER   :: CurrentState       ! Current state for Complex Fenestration
  REAL(r64) :: AbsDiffWin(CFSMAXNL) ! Diffuse solar absorptance of glass layers
  REAL(r64) :: AbsDiffWinGnd(CFSMAXNL) ! Ground diffuse solar absorptance of glass layers
  REAL(r64) :: AbsDiffWinSky(CFSMAXNL) ! Sky diffuse solar absorptance of glass layers
  INTEGER   :: Lay                ! Layer number
  REAL(r64) :: DividerAbs         ! Window divider solar absorptance
  REAL(r64) :: DividerRefl        ! Window divider solar reflectance
  INTEGER   :: MatNumGl           ! Outer glass layer material number
  INTEGER   :: MatNumGlSh         ! Outer glass layer material number, switched construction
  REAL(r64) :: TransGl,ReflGl,AbsGl ! Outer glass solar transmittance, reflectance, absorptance
  REAL(r64) :: TransGlSh,ReflGlSh,AbsGlSh ! Outer glass solar trans, refl, absorptance if switched
  REAL(r64) :: TransDiffGl        ! Diffuse solar transmittance
  REAL(r64) :: TransDiffGlSh      ! Diffuse solar transmittance, switched construction
  INTEGER   :: FrDivNum           ! Frame/divider number
  REAL(r64) :: FrArea,DivArea     ! Frame, divider area (m2)
  REAL(r64) :: FrWidth, DivWidth  ! Frame, divider width (m)
  REAL(r64) :: FrProjOut, DivProjOut ! Frame, divider outside projection (m)
  REAL(r64) :: FrProjIn, DivProjIn ! Frame, divider outside projection (m)
  REAL(r64) :: PhiWin,ThWin       ! Altitude and azimuth angle of outward window normal (radians)
  REAL(r64) :: PhiSun,ThSun       ! Altitude and azimuth angle of sun (radians)
  REAL(r64) :: CosInc             ! Cosine of incidence angle of beam solar on glass
  REAL(r64) :: CosIncAngHorProj   ! Cosine of incidence angle of sun on horizontal faces of a frame or
                             !   divider projection
  REAL(r64) :: CosIncAngVertProj  ! Cosine of incidence angle of sun on vertical faces of a frame or
                             !   divider projection
  REAL(r64) :: FracSunLit         ! Fraction of window sunlit this time step
  REAL(r64) :: BeamFaceInc        ! Beam solar incident window plane this time step (W/m2)
  REAL(r64) :: DifSolarFaceInc    ! Diffuse solar incident on window plane this time step (W/m2)
  REAL(r64) :: FrIncSolarOut      ! Total solar incident on outside offrame including solar
                             !   on frame projection (W/m2)
  REAL(r64) :: FrIncSolarIn       ! Total solar incident on inside offrame including solar
                             !   on frame projection (W/m2)

  REAL(r64) :: DivIncSolarOutBm   ! Beam solar incident on outside of divider including beam on divider
                             !   projection (W/m2)
  REAL(r64) :: DivIncSolarOutDif  ! Diffuse solar incident on outside of divider including diffuse on divider
                             !   projection (W/m2)
  REAL(r64) :: DivIncSolarInBm    ! Beam solar incident on inside of divider including beam on divider
                             !   projection (W/m2)
  REAL(r64) :: DivIncSolarInDif   ! Diffuse solar incident on inside of divider including diffuse on divider
                             !   projection (W/m2)
  REAL(r64) :: BeamFrHorFaceInc   ! Beam solar on frame's horizontal projection faces (W/m2)
  REAL(r64) :: BeamFrVertFaceInc  ! Beam solar on frame's vertical projection faces (W/m2)
  REAL(r64) :: BeamDivHorFaceInc  ! Beam solar on divider's horizontal outside projection faces (W/m2)
  REAL(r64) :: BeamDivVertFaceInc ! Beam solar on divider's vertical outside projection faces (W/m2)
  REAL(r64) :: BeamDivHorFaceIncIn  ! Beam solar on divider's horizontal inside projection faces (W/m2)
  REAL(r64) :: BeamDivVertFaceIncIn ! Beam solar on divider's vertical inside projection faces (W/m2)
  INTEGER   :: BlNum              ! Blind number
  REAL(r64) :: ProfAng            ! Solar profile angle (rad)
  REAL(r64) :: SlatAng            ! Slat angle (rad)
  REAL(r64) :: TBlBmBm            ! Blind beam-beam solar transmittance
  REAL(r64) :: TBlBmDif           ! Blind diffuse-diffuse solar transmittance
  REAL(r64) :: ACosTlt            ! Absolute value of cosine of surface tilt angle
  REAL(r64) :: AbsDiffBlindGnd    ! System blind front ground diffuse solar absorptance at a particular slat angle
  REAL(r64) :: AbsDiffBlindSky    ! System blind front sky diffuse solar absorptance at a particular slat angle
  REAL(r64) :: AbsDiffGlassLayGnd ! System glass layer ground diffuse solar absorptance with blind on
  REAL(r64) :: AbsDiffGlassLaySky ! System glass layer sky diffuse solar absorptance with blind on
  INTEGER   :: OtherZoneNum       ! Adjacent zone number
  INTEGER   :: SurfSolAbs         ! Pointer to scheduled surface gains object for fenestration systems
  INTEGER   :: SurfSolIncPtr      ! Pointer to schedule surface gain object for interior side of the surface

          ! Always initialize the shortwave quantities

  QRadSWOutAbs = 0.0d0
  QRadSWInAbs  = 0.0d0
  QRadSWLightsInAbs = 0.d0
  QRadSWwinAbs = 0.0d0
  InitialDifSolInAbs = 0.0d0
  InitialDifSolInTrans = 0.0d0
  InitialDifSolwinAbs = 0.0d0
  InitialZoneDifSolReflW = 0.0d0
  QRadSWwinAbsTot = 0.0d0
  QRadSWwinAbsLayer = 0.0d0
  SWwinAbsTotalReport = 0.0d0
  InitialDifSolInAbsReport = 0.0d0
  InitialDifSolInTransReport = 0.0d0
  SWInAbsTotalReport = 0.0d0
  SWOutAbsTotalReport = 0.d0
  SWOutAbsEnergyReport = 0.d0
  QRadSWOutIncident = 0.0d0
  QRadSWOutIncidentBeam = 0.0d0
  BmIncInsSurfIntensRep = 0.0d0
  BmIncInsSurfAmountRep = 0.0d0
  IntBmIncInsSurfIntensRep = 0.0d0
  IntBmIncInsSurfAmountRep = 0.0d0
  QRadSWOutIncidentSkyDiffuse = 0.0d0
  QRadSWOutIncidentGndDiffuse = 0.0d0
  QRadSWOutIncBmToDiffReflGnd = 0.0d0
  QRadSWOutIncSkyDiffReflGnd = 0.0d0
  QRadSWOutIncBmToBmReflObs = 0.0d0
  QRadSWOutIncBmToDiffReflObs = 0.0d0
  QRadSWOutIncSkyDiffReflObs = 0.0d0
  CosIncidenceAngle = 0.0d0
  BSDFBeamDirectionRep = 0
  BSDFBeamThetaRep = 0.0d0
  BSDFBeamPhiRep = 0.0d0
  OpaqSurfInsFaceBeamSolAbsorbed = 0.0d0

  DO SurfNum = 1, TotSurfaces
     SurfaceWindow(SurfNum)%FrameQRadOutAbs = 0.0d0
     SurfaceWindow(SurfNum)%FrameQRadInAbs = 0.0d0
     SurfaceWindow(SurfNum)%DividerQRadOutAbs = 0.0d0
     SurfaceWindow(SurfNum)%DividerQRadInAbs = 0.0d0
     SurfaceWindow(SurfNum)%ExtBeamAbsByShade = 0.0d0
     SurfaceWindow(SurfNum)%ExtDiffAbsByShade = 0.0d0
     SurfaceWindow(SurfNum)%IntBeamAbsByShade = 0.0d0
     SurfaceWindow(SurfNum)%IntSWAbsByShade = 0.0d0
     SurfaceWindow(SurfNum)%InitialDifSolAbsByShade = 0.0d0
     SurfaceWindow(SurfNum)%IntLWAbsByShade = 0.0d0
     SurfaceWindow(SurfNum)%ConvHeatFlowNatural = 0.0d0
     SurfaceWindow(SurfNum)%ConvHeatGainToZoneAir = 0.0d0
     SurfaceWindow(SurfNum)%RetHeatGainToZoneAir = 0.0d0
     SurfaceWindow(SurfNum)%DividerConduction = 0.0d0
     SurfaceWindow(SurfNum)%BlTsolBmBm = 0.0d0
     SurfaceWindow(SurfNum)%BlTsolBmDif = 0.0d0
     SurfaceWindow(SurfNum)%BlTsolDifDif = 0.0d0
     SurfaceWindow(SurfNum)%BlGlSysTsolBmBm = 0.0d0
     SurfaceWindow(SurfNum)%BlGlSysTsolDifDif = 0.0d0
     SurfaceWindow(SurfNum)%ScTsolBmBm = 0.0d0
     SurfaceWindow(SurfNum)%ScTsolBmDif = 0.0d0
     SurfaceWindow(SurfNum)%ScTsolDifDif = 0.0d0
     SurfaceWindow(SurfNum)%ScGlSysTsolBmBm = 0.0d0
     SurfaceWindow(SurfNum)%ScGlSysTsolDifDif = 0.0d0
     SurfaceWindow(SurfNum)%GlTsolBmBm = 0.0d0
     SurfaceWindow(SurfNum)%GlTsolBmDif = 0.0d0
     SurfaceWindow(SurfNum)%GlTsolDifDif = 0.0d0
     SurfaceWindow(SurfNum)%BmSolTransThruIntWinRep = 0.0d0
     SurfaceWindow(SurfNum)%BmSolAbsdOutsReveal = 0.0d0
     SurfaceWindow(SurfNum)%BmSolRefldOutsRevealReport = 0.0d0
     SurfaceWindow(SurfNum)%BmSolAbsdInsReveal = 0.0d0
     SurfaceWindow(SurfNum)%BmSolRefldInsReveal = 0.0d0
     SurfaceWindow(SurfNum)%BmSolRefldInsRevealReport = 0.0d0
     SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing = 0.0d0
     SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing = 0.0d0
     SurfaceWindow(SurfNum)%InsRevealDiffIntoZone = 0.0d0
     SurfaceWindow(SurfNum)%OutsRevealDiffOntoFrame = 0.0d0
     SurfaceWindow(SurfNum)%InsRevealDiffOntoFrame = 0.0d0
     SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazingReport = 0.0d0
     SurfaceWindow(SurfNum)%InsRevealDiffIntoZoneReport = 0.0d0
     SurfaceWindow(SurfNum)%InsRevealDiffOntoFrameReport = 0.0d0
     SurfaceWindow(SurfNum)%BmSolAbsdInsRevealReport = 0.0d0
     SurfaceWindow(SurfNum)%BmSolTransThruIntWinRepEnergy = 0.0d0
     SurfaceWindow(SurfNum)%BmSolRefldOutsRevealRepEnergy = 0.0d0
     SurfaceWindow(SurfNum)%BmSolRefldInsRevealRepEnergy = 0.0d0
     SurfaceWindow(SurfNum)%ProfileAngHor = 0.0d0
     SurfaceWindow(SurfNum)%ProfileAngVert = 0.0d0
     SurfaceWindow(SurfNum)%SkySolarInc = 0.0d0
     SurfaceWindow(SurfNum)%GndSolarInc = 0.0d0
  end DO

  WinHeatGain = 0.0d0
  WinHeatGainRep = 0.0d0
  WinHeatLossRep = 0.0d0
  WinGainConvGlazToZoneRep        = 0.0D0
  WinGainIRGlazToZoneRep          = 0.0D0
  WinLossSWZoneToOutWinRep        = 0.0D0
  WinGainFrameDividerToZoneRep    = 0.0D0
  WinGainConvGlazShadGapToZoneRep = 0.0D0
  WinGainConvShadeToZoneRep       = 0.0D0
  WinGainIRShadeToZoneRep         = 0.0D0
  OtherConvGainInsideFaceToZoneRep= 0.0d0
  WinGapConvHtFlowRep = 0.0d0
  OpaqSurfInsFaceCondGainRep = 0.0d0
  OpaqSurfInsFaceCondLossRep = 0.0d0
  ZoneWinHeatGain = 0.0d0
  ZoneWinHeatGainRep = 0.0d0
  ZoneWinHeatLossRep = 0.0d0
  ZoneOpaqSurfInsFaceCond = 0.0d0
  ZoneOpaqSurfInsFaceCondGainRep = 0.0d0
  ZoneOpaqSurfInsFaceCondLossRep = 0.0d0
  ZoneOpaqSurfExtFaceCond        = 0.d0
  ZoneOpaqSurfExtFaceCondGainRep = 0.d0
  ZoneOpaqSurfExtFaceCondLossRep = 0.d0
  WinShadingAbsorbedSolar = 0.0d0
  WinSysSolTransmittance = 0.0d0
  WinSysSolReflectance = 0.0d0
  WinSysSolAbsorptance = 0.0d0
  IF (NumOfTDDPipes > 0) THEN
    TDDPipe%HeatGain = 0.0d0
    TDDPipe%HeatLoss = 0.0d0
  ENDIF
  BmIncInsSurfIntensRep = 0.0d0
  BmIncInsSurfAmountRep = 0.0d0
  IntBmIncInsSurfIntensRep = 0.0d0
  IntBmIncInsSurfAmountRep = 0.0d0
  !energy
  QRadSWwinAbsTotEnergy = 0.0d0
  BmIncInsSurfAmountRepEnergy = 0.0d0
  IntBmIncInsSurfAmountRepEnergy = 0.0d0
  WinHeatGainRepEnergy = 0.0d0
  WinHeatLossRepEnergy = 0.0d0
  WinGapConvHtFlowRepEnergy = 0.0d0
  ZoneWinHeatGainRepEnergy = 0.0d0
  ZoneWinHeatLossRepEnergy = 0.0d0
  ZnOpqSurfInsFaceCondGnRepEnrg = 0.0d0
  ZnOpqSurfInsFaceCondLsRepEnrg = 0.0d0
  ZnOpqSurfExtFaceCondGnRepEnrg = 0.d0
  ZnOpqSurfExtFaceCondLsRepEnrg = 0.d0
  WinShadingAbsorbedSolarEnergy = 0.0d0
  BmIncInsSurfAmountRepEnergy = 0.0d0
  IntBmIncInsSurfAmountRepEnergy = 0.0d0

  IF (.not. SunIsUp .or. (BeamSolarRad+GndSolarRad+DifSolarRad <= 0.0d0)) THEN  ! Sun is down

    QD  = 0.0d0
    QDforDaylight  = 0.0d0
    QC  = 0.0d0
    QDV = 0.0d0
!unused    QBV = 0.0
    ZoneTransSolar = 0.0d0
    ZoneBmSolFrExtWinsRep = 0.0d0
    ZoneBmSolFrIntWinsRep = 0.0d0
    ZoneDifSolFrExtWinsRep = 0.0d0
    ZoneDifSolFrIntWinsRep = 0.0d0
    WinTransSolar = 0.0d0
    WinBmSolar = 0.0d0
    WinBmBmSolar = 0.0d0
    WinBmDifSolar = 0.0d0

    WinDifSolar = 0.0d0
    WinDirSolTransAtIncAngle = 0.0d0
    !energy
    ZoneTransSolarEnergy = 0.0d0
    ZoneBmSolFrExtWinsRepEnergy = 0.0d0
    ZoneBmSolFrIntWinsRepEnergy = 0.0d0
    ZoneDifSolFrExtWinsRepEnergy = 0.0d0
    ZoneDifSolFrIntWinsRepEnergy = 0.0d0
    WinTransSolarEnergy = 0.0d0
    WinBmSolarEnergy = 0.0d0
    WinBmBmSolarEnergy = 0.0d0
    WinBmDifSolarEnergy = 0.0d0

    WinDifSolarEnergy = 0.0d0

    IF (NumOfTDDPipes > 0) THEN
      TDDPipe%TransSolBeam = 0.0d0
      TDDPipe%TransSolDiff = 0.0d0
      TDDPipe%TransVisBeam = 0.0d0
      TDDPipe%TransVisDiff = 0.0d0
      TDDPipe%TransmittedSolar = 0.0d0
    ENDIF

    IF(CalcSolRefl) THEN
      BmToBmReflFacObs = 0.0d0
      BmToDiffReflFacObs = 0.0d0
      BmToDiffReflFacGnd = 0.0d0
    END IF

  ELSE  ! Sun is up, calculate solar quantities

    DO SurfNum = 1,TotSurfaces
      SurfaceWindow(SurfNum)%SkySolarInc = DifSolarRad * AnisoSkyMult(SurfNum)
      SurfaceWindow(SurfNum)%GndSolarInc = GndSolarRad * Surface(SurfNum)%ViewFactorGround
      !For Complex Fenestrations:
      SurfaceWindow(SurfNum)%SkyGndSolarInc = SurfaceWindow(SurfNum)%GndSolarInc
      SurfaceWindow(SurfNum)%BmGndSolarInc =0.0d0
      !
      IF(CalcSolRefl) THEN

        !For Complex Fenestrations:
        SurfaceWindow(SurfNum)%SkyGndSolarInc = DifSolarRad * GndReflectance * ReflFacSkySolGnd(SurfNum)
        SurfaceWindow(SurfNum)%BmGndSolarInc =BeamSolarRad * SOLCOS(3) * GndReflectance * BmToDiffReflFacGnd(SurfNum)
        !
        BmToBmReflFacObs(SurfNum)   = (WeightNow * ReflFacBmToBmSolObs(SurfNum,HourOfDay) + &
                   WeightPreviousHour * ReflFacBmToBmSolObs(SurfNum,PreviousHour))
        BmToDiffReflFacObs(SurfNum) = (WeightNow * ReflFacBmToDiffSolObs(SurfNum,HourOfDay) + &
                   WeightPreviousHour * ReflFacBmToDiffSolObs(SurfNum,PreviousHour))
        BmToDiffReflFacGnd(SurfNum) = (WeightNow * ReflFacBmToDiffSolGnd(SurfNum,HourOfDay) + &
                   WeightPreviousHour * ReflFacBmToDiffSolGnd(SurfNum,PreviousHour))

        ! TH2 CR 9056
        SurfaceWindow(SurfNum)%SkySolarInc = SurfaceWindow(SurfNum)%SkySolarInc + &
                    BeamSolarRad * (BmToBmReflFacObs(SurfNum) + BmToDiffReflFacObs(SurfNum))  +  &
                    DifSolarRad * ReflFacSkySolObs(SurfNum)
        SurfaceWindow(SurfNum)%GndSolarInc = BeamSolarRad * SOLCOS(3) * GndReflectance * BmToDiffReflFacGnd(SurfNum) +  &
                    DifSolarRad * GndReflectance * ReflFacSkySolGnd(SurfNum)

      END IF
    END DO

    CALL CalcWindowProfileAngles

    IF(CalcWindowRevealReflection) CALL CalcBeamSolarOnWinRevealSurface

    CALL CalcInteriorSolarDistribution

    DO ZoneNum = 1, NumOfZones

      ! TH 3/24/2010 - QBV is not used!
!unused      QBV(ZoneNum) = (CBZone(ZoneNum) + DBZone(ZoneNum))*BeamSolarRad

      ! RJH 08/30/07 - QDV does not seem to ever be used. NOT USED!
      !QDV(ZoneNum) = DSZone(ZoneNum)*DifSolarRad &
      !                +DGZone(ZoneNum)*GndSolarRad

      ! Original QD calc used only for QSDifSol and daylighting calcs
      !QDforDaylight(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad  &
      !                          +DSZone(ZoneNum)*DifSolarRad  &
      !                          +DGZone(ZoneNum)*GndSolarRad

      ! TH 3/23/2010. CR 7869 and CR 7999. QDforDaylight in W
      !  Beam from interior windows (DBZoneIntWin) reflected from floor is counted in DayltgInterReflIllFrIntWins,
      !  DBZone needs to subtract this part since it is already counted in DBZone.
      !  Use InitialZoneDifSolReflW (Rob's previous work) as it better counts initial distribution of
      !   diffuse solar rather than using weighted area*absorptance
        QDforDaylight(ZoneNum) = (DBZone(ZoneNum) - DBZoneIntWin(ZoneNum)) * BeamSolarRad + DBZoneSSG(ZoneNum) &
                                 + InitialZoneDifSolReflW(ZoneNum)

      ! RJH 08/30/07 - Substitute InitialZoneDifSolReflW(ZoneNum) for DSZone and DGZone here
      ! to exclude diffuse solar now absorbed/transmitted in CalcWinTransDifSolInitialDistribution
      ! DBZone(ZoneNum) is Diffuse Solar from beam reflected from interior surfaces
      ! and transmitted through interior windows
      ! DBZone is a factor that when multiplied by BeamSolarRad [W/m2] gives Watts
      !QD(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad  &
      !                +DSZone(ZoneNum)*DifSolarRad  &
      !                +DGZone(ZoneNum)*GndSolarRad
      QD(ZoneNum)  = DBZone(ZoneNum)*BeamSolarRad + DBZoneSSG(ZoneNum)  &
                      + InitialZoneDifSolReflW(ZoneNum)
    END DO

          ! Flux of diffuse solar in each zone

    QSDifSol = 0.0d0
    DO ZoneNum = 1, NumOfZones
      QSDifSol(ZoneNum) = QDforDaylight(ZoneNum)
    END DO

    IF (InterZoneWindow) THEN
      DO ZoneNum = 1, NumOfZones
        IF (RecDifShortFromZ(ZoneNum)) THEN
          DO OtherZoneNum = 1, NumOfZones
            IF ((OtherZoneNum /= ZoneNum) .AND. (RecDifShortFromZ(OtherZoneNum))) THEN
              QSDifSol(ZoneNum) = QSDifSol(ZoneNum) + FractDifShortZtoZ(OtherZoneNum,ZoneNum) * QDforDaylight(OtherZoneNum)
            END IF
          END DO
        END IF
      END DO
    END IF

    DO ZoneNum = 1, NumOfZones
      QSDifSol(ZoneNum) = QSDifSol(ZoneNum) * FractDifShortZtoZ(ZoneNum,ZoneNum) * VMULT(ZoneNum)
    END DO

!    RJH - 09-12-07 commented out report varariable calcs here since they refer to old distribution method
!    DO SurfNum = 1, TotSurfaces
!      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
      !!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
!      IF (Surface(SurfNum)%Class == SurfaceClass_Shading) CYCLE
!      ZoneNum = Surface(SurfNum)%Zone
      ! Diffuse solar entering zone through exterior windows is assumed to be uniformly
      ! distributed on inside face of surfaces of zone
!      DifIncInsSurfIntensRep(SurfNum) = (DSZone(ZoneNum)*DifSolarRad + DGZone(ZoneNum)*GndSolarRad) /  &
!        Zone(ZoneNum)%TotalSurfArea
!      DifIncInsSurfAmountRep(SurfNum) = (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea) *  &
!        DifIncInsSurfIntensRep(SurfNum)
!      DifIncInsSurfAmountRepEnergy(SurfNum) = DifIncInsSurfAmountRep(SurfNum) * TimeStepZone * SecInHour
!    END DO

    DO SurfNum = 1, TotSurfaces
      IF (Surface(SurfNum)%HeatTransSurf) THEN
        ConstrNum = Surface(SurfNum)%Construction
        IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNum = Surface(SurfNum)%StormWinConstruction
      ELSE
        ConstrNum=0
      END IF
      ShelfNum = Surface(SurfNum)%Shelf
      IF (ShelfNum > 0) THEN
        InShelfSurf = Shelf(ShelfNum)%InSurf ! Inside daylighting shelf present if > 0
        OutShelfSurf = Shelf(ShelfNum)%OutSurf ! Outside daylighting shelf present if > 0
      ELSE
        InShelfSurf = 0
        OutShelfSurf = 0
      END IF

      IF (Surface(SurfNum)%ExtSolar .OR. SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN

        IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
          PipeNum = FindTDDPipe(SurfNum)
          SurfNum2 = TDDPipe(PipeNum)%Dome

          CosInc = CosIncAng(SurfNum2,HourOfDay,TimeStep)

          ! Reconstruct the beam, sky, and ground radiation transmittance of just the TDD:DOME and TDD pipe
          ! by dividing out diffuse solar transmittance of TDD:DIFFUSER
          BeamSolar = BeamSolarRad * TransTDD(PipeNum, CosInc, SolarBeam) &
            / Construct(ConstrNum)%TransDiff

          SkySolarInc = DifSolarRad * AnisoSkyMult(SurfNum2) * TransTDD(PipeNum, CosInc, SolarAniso)  &
            / Construct(ConstrNum)%TransDiff

          GndSolarInc = GndSolarRad * Surface(SurfNum2)%ViewFactorGround * TDDPipe(PipeNum)%TransSolIso &
            / Construct(ConstrNum)%TransDiff

        ELSE IF (OutShelfSurf > 0) THEN ! Outside daylighting shelf
          SurfNum2 = SurfNum

          CosInc = CosIncAng(SurfNum,HourOfDay,TimeStep)

          BeamSolar = BeamSolarRad
          SkySolarInc = DifSolarRad * AnisoSkyMult(SurfNum)

          ShelfSolarRad = (BeamSolarRad * SunlitFrac(OutShelfSurf,HourOfDay,TimeStep) &
            * CosIncAng(OutShelfSurf,HourOfDay,TimeStep) + DifSolarRad * AnisoSkyMult(OutShelfSurf)) &
            * Shelf(ShelfNum)%OutReflectSol

          ! Add all reflected solar from the outside shelf to the ground solar
          ! NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!
          GndSolarInc = GndSolarRad * Surface(SurfNum)%ViewFactorGround + ShelfSolarRad * Shelf(ShelfNum)%ViewFactor

        ELSE ! Regular surface
          SurfNum2 = SurfNum
          CosInc = CosIncAng(SurfNum,HourOfDay,TimeStep)
          BeamSolar = BeamSolarRad
          SkySolarInc = SurfaceWindow(SurfNum)%SkySolarInc
          GndSolarInc = SurfaceWindow(SurfNum)%GndSolarInc
        END IF

        ! Cosine of incidence angle and solar incident on outside of surface, for reporting
        CosIncidenceAngle(SurfNum) = CosInc

        ! Report variables for various incident solar quantities

        ! Incident direct (unreflected) beam
        QRadSWOutIncidentBeam(SurfNum) = BeamSolar * SunlitFrac(SurfNum2,HourOfDay,TimeStep) * CosInc ! NOTE: SurfNum2
        ! Incident (unreflected) diffuse solar from sky -- TDD_Diffuser calculated differently
        IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
          QRadSWOutIncidentSkyDiffuse(SurfNum) = SkySolarInc
        ELSE
          QRadSWOutIncidentSkyDiffuse(SurfNum) = DifSolarRad * AnisoSkyMult(SurfNum)
        END IF
        ! Incident diffuse solar from sky diffuse reflected from ground plus beam reflected from ground
        QRadSWOutIncidentGndDiffuse(SurfNum) = GndSolarInc
        ! Incident diffuse solar from beam-to-diffuse reflection from ground
        IF(CalcSolRefl) THEN
          QRadSWOutIncBmToDiffReflGnd(SurfNum) = BeamSolarRad * SOLCOS(3) * GndReflectance * &
                        BmToDiffReflFacGnd(SurfNum)
        ELSE
          QRadSWOutIncBmToDiffReflGnd(SurfNum) = BeamSolarRad * SOLCOS(3) * GndReflectance * &
                        Surface(SurfNum)%ViewFactorGround
        END IF
        ! Incident diffuse solar from sky diffuse reflection from ground
        IF(CalcSolRefl) THEN
          QRadSWOutIncSkyDiffReflGnd(SurfNum) = DifSolarRad * GndReflectance * ReflFacSkySolGnd(SurfNum)
        ELSE
          QRadSWOutIncSkyDiffReflGnd(SurfNum) = DifSolarRad * GndReflectance * Surface(SurfNum)%ViewFactorGround
        END IF
        ! Total incident solar. Beam and sky reflection from obstructions, if calculated, is included
        ! in SkySolarInc.
        ! QRadSWOutIncident(SurfNum) = QRadSWOutIncidentBeam(SurfNum) + SkySolarInc + GndSolarInc

        ! TH2 CR 9056
        QRadSWOutIncident(SurfNum) = QRadSWOutIncidentBeam(SurfNum) +  QRadSWOutIncidentSkyDiffuse(SurfNum) + &
                                     QRadSWOutIncBmToDiffReflGnd(SurfNum) + QRadSWOutIncSkyDiffReflGnd(SurfNum)

        IF(CalcSolRefl) THEN
          ! Incident beam solar from beam-to-beam (specular) reflection from obstructions
          QRadSWOutIncBmToBmReflObs(SurfNum) = BmToBmReflFacObs(SurfNum) * BeamSolarRad
          ! Incident diffuse solar from beam-to-diffuse reflection from obstructions
          QRadSWOutIncBmToDiffReflObs(SurfNum) = BmToDiffReflFacObs(SurfNum) * BeamSolarRad
          ! Incident diffuse solar from sky diffuse reflection from obstructions
          QRadSWOutIncSkyDiffReflObs(SurfNum) = DifSolarRad * ReflFacSkySolObs(SurfNum)
          ! TH2 CR 9056: Add reflections from obstructions to the total incident
          QRadSWOutIncident(SurfNum) =  QRadSWOutIncident(SurfNum) + QRadSWOutIncBmToBmReflObs(SurfNum) + &
                                        QRadSWOutIncBmToDiffReflObs(SurfNum) + QRadSWOutIncSkyDiffReflObs(SurfNum)
        END IF

        IF (Surface(SurfNum)%HeatTransSurf) THEN ! Exclude special shading surfaces which required QRadSWOut calculations above

          RoughIndexMovInsul = 0

          IF (Surface(SurfNum)%MaterialMovInsulExt > 0) &
            CALL EvalOutsideMovableInsulation(SurfNum,HMovInsul,RoughIndexMovInsul,AbsExt)

          IF (RoughIndexMovInsul <= 0) THEN   ! No movable insulation present

            IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN ! Opaque surface

              AbsExt = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar

            ELSE ! Exterior window

              IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel .AND. &
                  SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
                TotGlassLay = Construct(ConstrNum)%TotGlassLayers
                DO Lay = 1,TotGlassLay
                  AbsDiffWin(Lay) = Construct(ConstrNum)%AbsDiff(Lay)
                END DO

                ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag

                IF(ShadeFlag > 0) THEN          ! Shaded window
                  ConstrNumSh = Surface(SurfNum)%ShadedConstruction
                  IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction

                  IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. &
                    ShadeFlag==BGShadeOn  .OR. ShadeFlag==ExtScreenOn) THEN  ! Shade/screen on

                    DO Lay = 1,TotGlassLay
                      AbsDiffWin(Lay) = Construct(ConstrNumSh)%AbsDiff(Lay)
                    END DO
                    SurfaceWindow(SurfNum)%ExtDiffAbsByShade = Construct(ConstrNumSh)%AbsDiffShade * &
                      (SkySolarInc + GndSolarInc)
                  END IF

                  IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==BGBlindOn) THEN  ! Blind on
                    DO Lay = 1,TotGlassLay
                      AbsDiffWin(Lay) = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                        SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlAbsDiff(Lay,1:MaxSlatAngs))
                      AbsDiffWinGnd(Lay) = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                        SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlAbsDiffGnd(Lay,1:MaxSlatAngs))
                      AbsDiffWinSky(Lay) = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                        SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlAbsDiffSky(Lay,1:MaxSlatAngs))
                    END DO
                    SurfaceWindow(SurfNum)%ExtDiffAbsByShade = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                      SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%AbsDiffBlind) * &
                       (SkySolarInc + GndSolarInc)
                    IF(Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatOrientation==Horizontal) THEN
                      ACosTlt = ABS(Surface(SurfNum)%CosTilt)
                      AbsDiffBlindGnd = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                        SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%AbsDiffBlindGnd)
                      AbsDiffBlindSky = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                        SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%AbsDiffBlindSky)
                      SurfaceWindow(SurfNum)%ExtDiffAbsByShade = &
                        SkySolarInc*(0.5d0*ACosTlt*AbsDiffBlindGnd + (1.0d0-0.5d0*ACosTlt)*AbsDiffBlindSky) +  &
                        GndSolarInc*((1.0d0-0.5d0*ACosTlt)*AbsDiffBlindGnd + 0.5d0*ACosTlt*AbsDiffBlindSky)
                    END IF
                  END IF

                  ! Correct for shadowing of divider onto interior shading device (note that dividers are
                  ! not allowed in windows with between-glass shade/blind)

                  IF((ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) .AND. SurfaceWindow(SurfNum)%DividerArea > 0.0d0) &
                      SurfaceWindow(SurfNum)%ExtDiffAbsByShade = SurfaceWindow(SurfNum)%ExtDiffAbsByShade * &
                        SurfaceWindow(SurfNum)%GlazedFrac

                  IF(ShadeFlag == SwitchableGlazing) THEN       ! Switchable glazing
                    SwitchFac = SurfaceWindow(SurfNum)%SwitchingFactor
                    DO Lay = 1,TotGlassLay
                      AbsDiffWin(Lay) = InterpSw(SwitchFac, AbsDiffWin(Lay), Construct(ConstrNumSh)%AbsDiff(Lay))
                    END DO
                  END IF

                END IF ! End of check if window has shading device on

                QRadSWwinAbsTot(SurfNum) = 0.0d0
                DO Lay = 1,TotGlassLay
                  QRadSWwinAbs(SurfNum,Lay) = AbsDiffWin(Lay) * (SkySolarInc + GndSolarInc) &
                    + AWinSurf(SurfNum,Lay) * BeamSolar  ! AWinSurf is from InteriorSolarDistribution
                  IF(ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR.ShadeFlag==BGBlindOn) THEN
                     IF(Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatOrientation==Horizontal) THEN
                       AbsDiffGlassLayGnd = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                         SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlAbsDiffGnd(Lay,1:19))
                       AbsDiffGlassLaySky = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTs, &
                         SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlAbsDiffSky(Lay,1:19))
                       QRadSWwinAbs(SurfNum,Lay) = &
                        SkySolarInc*(0.5d0*ACosTlt*AbsDiffGlassLayGnd + (1.0d0-0.5d0*ACosTlt)*AbsDiffGlassLaySky) +  &
                        GndSolarInc*((1.0d0-0.5d0*ACosTlt)*AbsDiffGlassLayGnd + 0.5d0*ACosTlt*AbsDiffGlassLaySky) +  &
                        AWinSurf(SurfNum,Lay) * BeamSolar
                     END IF
                  END IF

                  ! Total solar absorbed in solid layer (W), for reporting
                  QRadSWwinAbsLayer(SurfNum,Lay) = QRadSWwinAbs(SurfNum,Lay) * Surface(SurfNum)%Area

                  ! Total solar absorbed in all glass layers (W), for reporting
                  QRadSWwinAbsTot(SurfNum) = QRadSWwinAbsTot(SurfNum) + QRadSWwinAbsLayer(SurfNum,Lay)
                END DO
                QRadSWwinAbsTotEnergy(SurfNum) = QRadSWwinAbsTot(SurfNum) * TimeStepZone * SecInHour

              ELSE IF (SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) THEN
                TotSolidLay = Construct(ConstrNum)%TotSolidLayers
                CurrentState = SurfaceWindow(SurfNum)%ComplexFen%CurrentState
                ! Examine for schedule surface gain
                SurfSolAbs = WindowScheduledSolarAbs(SurfNum, ConstrNum)

                DO Lay = 1, TotSolidLay
                  IF (SurfSolAbs /= 0) THEN
                    AWinSurf(SurfNum,Lay) = GetCurrentScheduleValue(FenLayAbsSSG(SurfSolAbs)%SchedPtrs(Lay))
                    !ABWin(Lay) = AWinSurf(SurfNum,Lay)
                    QRadSWwinAbs(SurfNum,Lay) = AWinSurf(SurfNum,Lay)
                  ELSE
                    ! Several notes about this equation.  First part is accounting for duffuse solar radiation for the ground and
                    ! from the sky.  Second item (AWinSurf(SurfNum,Lay) * BeamSolar) is accounting for absorbed solar radiation
                    ! originating from beam on exterior side.  Third item (AWinCFOverlap(SurfNum,Lay)) is accounting for
                    ! absorptances from beam hitting back of the window which passes through rest of exterior windows
                    QRadSWwinAbs(SurfNum,Lay) = SurfaceWindow(SurfNum)%ComplexFen%State(CurrentState)%WinSkyFtAbs(Lay) * &
                      & SkySolarInc + SurfaceWindow(SurfNum)%ComplexFen%State(CurrentState)%WinSkyGndAbs(Lay) * GndSolarInc &
                      & + AWinSurf(SurfNum,Lay) * BeamSolar &
                      & + AWinCFOverlap(SurfNum,Lay) * BeamSolar
                  END IF
                  ! Total solar absorbed in solid layer (W), for reporting
                  QRadSWwinAbsLayer(SurfNum,Lay) = QRadSWwinAbs(SurfNum,Lay) * Surface(SurfNum)%Area

                  ! Total solar absorbed in all glass layers (W), for reporting
                  QRadSWwinAbsTot(SurfNum) = QRadSWwinAbsTot(SurfNum) + QRadSWwinAbsLayer(SurfNum,Lay)
                END DO
                QRadSWwinAbsTotEnergy(SurfNum) = QRadSWwinAbsTot(SurfNum) * TimeStepZone * SecInHour

         ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag

              ELSE IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
                QRadSWwinAbsTot(SurfNum) = 0.0d0
                !EQLNum = Construct(Surface(SurfNum)%Construction)%EQLConsPtr
                TotSolidLay = CFS(Construct(Surface(SurfNum)%Construction)%EQLConsPtr)%NL
                DO Lay = 1, TotSolidLay
                  ! Absorbed window components include:
                  ! (1) beam solar radiation absorbed by all layers in the fenestration
                  ! (2) sky and ground reflected duffuse solar radiation absorbed by all layers
                  ! (3) diffuse short wave incident on the inside face of the fenestration.  The short wave internal sources
                  !     include light, ...
                  AbsDiffWin(Lay) = Construct(ConstrNum)%AbsDiffFrontEQL(Lay)
                  QRadSWwinAbs(SurfNum,Lay) = AWinSurf(SurfNum,Lay) * BeamSolar &
                                            + AbsDiffWin(Lay) * (SkySolarInc + GndSolarInc)

                  ! Total solar absorbed in solid layer (W), for reporting
                  QRadSWwinAbsLayer(SurfNum,Lay) = QRadSWwinAbs(SurfNum,Lay) * Surface(SurfNum)%Area

                  ! Total solar absorbed in all glass layers (W), for reporting
                  QRadSWwinAbsTot(SurfNum) = QRadSWwinAbsTot(SurfNum) + QRadSWwinAbsLayer(SurfNum,Lay)
                END DO
                QRadSWwinAbsTotEnergy(SurfNum) = QRadSWwinAbsTot(SurfNum) * TimeStepZone * SecInHour
              END IF ! IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel) THEN

              ! Solar absorbed by window frame and dividers
              FrDivNum = Surface(SurfNum)%FrameDivider
              FrArea = SurfaceWindow(SurfNum)%FrameArea
              IF (FrDivNum > 0) THEN
                FrWidth = FrameDivider(FrDivNum)%FrameWidth
                FrProjOut = FrameDivider(FrDivNum)%FrameProjectionOut
                FrProjIn = FrameDivider(FrDivNum)%FrameProjectionIn
                DivArea = SurfaceWindow(SurfNum)%DividerArea
                DivWidth = FrameDivider(FrDivNum)%DividerWidth
                DivProjOut = FrameDivider(FrDivNum)%DividerProjectionOut
                DivProjIn = FrameDivider(FrDivNum)%DividerProjectionIn
              ELSE
                FrWidth=0.0d0
                FrProjOut=0.0d0
                FrProjIn=0.0d0
                DivArea=0.0d0
                DivWidth=0.0d0
                DivProjOut=0.0d0
                DivProjIn=0.0d0
              ENDIF
              CosIncAngHorProj = 0.0d0
              CosIncAngVertProj = 0.0d0
              IF(FrArea > 0.0d0 .OR. DivArea > 0.0d0) THEN
                FracSunLit = SunLitFrac(SurfNum,HourOfDay,TimeStep)
                BeamFaceInc = BeamSolarRad * SunLitFrac(SurfNum,HourOfDay,TimeStep)*CosInc
                DifSolarFaceInc = SkySolarInc + GndSolarInc
              ELSE
                FracSunLit=0.0d0
              END IF
              IF(FracSunLit > 0.0d0) THEN
                IF((FrArea > 0.0d0 .AND. (FrProjOut > 0.0d0 .OR. FrProjIn > 0.0d0)) &
                  .OR. (DivArea > 0.0d0 .AND. (DivProjOut > 0.0d0 .OR. DivProjIn > 0.0d0))) THEN
                  ! Dot products used to calculate beam solar incident on faces of
                  ! frame and divider perpendicular to the glass surface.
                  ! Note that SOLCOS is the current timestep's solar direction cosines.
!                  PhiWin = ASIN(WALCOS(3,SurfNum))
                  PhiWin = ASIN(Surface(SurfNum)%OutNormVec(3))
                  ThWin  = ATAN2(Surface(SurfNum)%OutNormVec(2),Surface(SurfNum)%OutNormVec(1))
                  PhiSun = ASIN(SOLCOS(3))
                  ThSun = ATAN2(SOLCOS(2),SOLCOS(1))
                  CosIncAngHorProj = ABS(SIN(PhiWin)*COS(PhiSun)*COS(ThWin-ThSun) - COS(PhiWin)*Sin(PhiSun))
                  CosIncAngVertProj = ABS(COS(PhiWin)*COS(PhiSun)*SIN(Thwin-ThSun))
                END IF
              END IF

              ! Frame solar

              ! (A window shade or blind, if present, is assumed to not shade the frame, so no special
              ! treatment of frame solar needed if window has an exterior shade or blind.)
              IF(FrArea > 0.0d0) THEN
                FrIncSolarOut = BeamFaceInc
                FrIncSolarIn  = 0.0d0
                TransDiffGl   = 0.0d0
                IF(FrProjOut > 0.0d0 .OR. FrProjIn > 0.0d0) THEN
                  BeamFrHorFaceInc = BeamSolarRad * CosIncAngHorProj * &
                    (Surface(SurfNum)%Width - FrameDivider(FrDivNum)%VertDividers*DivWidth)*FracSunLit/FrArea
                  BeamFrVertFaceInc = BeamSolarRad * CosIncAngVertProj * &
                    (Surface(SurfNum)%Height - FrameDivider(FrDivNum)%HorDividers*DivWidth)*FracSunLit/FrArea
                  ! Beam solar on outside of frame
                  FrIncSolarOut = FrIncSolarOut + (BeamFrHorFaceInc + BeamFrVertFaceInc)*FrProjOut
                  IF(FrProjIn > 0.0d0) THEN
                    TransGl = POLYF(CosInc,Construct(ConstrNum)%TransSolBeamCoef(1:6))
                    TransDiffGl = Construct(ConstrNum)%TransDiff
                    IF(ShadeFlag == SwitchableGlazing) THEN   ! Switchable glazing
                      TransGlSh = POLYF(CosInc,Construct(ConstrNumSh)%TransSolBeamCoef(1:6))
                      TransGl = InterpSw(SwitchFac,TransGl,TransGlSh)
                      TransDiffGlSh = Construct(ConstrNumSh)%TransDiff
                      TransDiffGl = InterpSw(SwitchFac,TransDiffGl,TransDiffGlSh)
                    END IF
                    ! Beam solar on inside of frame
                    FrIncSolarIn = (BeamFrHorFaceInc + BeamFrVertFaceInc) * FrProjIn * TransGl
                  END IF
                END IF
                ! Beam plus diffuse solar on outside of frame
                FrIncSolarOut = FrIncSolarOut + DifSolarFaceInc*(1.0d0+0.5d0*SurfaceWindow(SurfNum)%ProjCorrFrOut)
                SurfaceWindow(SurfNum)%FrameQRadOutAbs = FrIncSolarOut*SurfaceWindow(SurfNum)%FrameSolAbsorp
                ! Add diffuse from beam reflected from window outside reveal surfaces
                SurfaceWindow(SurfNum)%FrameQRadOutAbs = SurfaceWindow(SurfNum)%FrameQRadOutAbs + BeamSolarRad * &
                  SurfaceWindow(SurfNum)%OutsRevealDiffOntoFrame * SurfaceWindow(SurfNum)%FrameSolAbsorp

                ! Beam plus diffuse solar on inside of frame
                FrIncSolarIn = FrIncSolarIn + DifSolarFaceInc * TransDiffGl * &
                  0.5d0 * SurfaceWindow(SurfNum)%ProjCorrFrIn
                SurfaceWindow(SurfNum)%FrameQRadInAbs = FrIncSolarIn*SurfaceWindow(SurfNum)%FrameSolAbsorp
                ! Add diffuse from beam reflected from window inside reveal surfaces
                SurfaceWindow(SurfNum)%FrameQRadInAbs = SurfaceWindow(SurfNum)%FrameQRadInAbs + BeamSolarRad * &
                  SurfaceWindow(SurfNum)%InsRevealDiffOntoFrame * SurfaceWindow(SurfNum)%FrameSolAbsorp

              END IF

              ! Divider solar

              ! (An exterior shade or blind, when in place, is assumed to completely cover the divider.
              ! Dividers are not allowed on windows with between-glass shade/blind so DivProjOut and
              ! DivProjIn will be zero in this case.)

              IF(DivArea > 0.0d0) THEN  ! Solar absorbed by window divider
                DividerAbs = SurfaceWindow(SurfNum)%DividerSolAbsorp
                IF(SurfaceWindow(SurfNum)%DividerType == Suspended) THEN
                  ! Suspended (between-glass) divider; account for effect glass on outside of divider
                  ! (note that outside and inside projection for this type of divider are both zero)
                  MatNumGL = Construct(ConstrNum)%LayerPoint(1)
                  TransGl = Material(MatNumGl)%Trans
                  ReflGl = Material(MatNumGl)%ReflectSolBeamFront
                  AbsGl = 1.0d0-TransGl-ReflGl
                  IF(ShadeFlag == SwitchableGlazing) THEN   ! Switchable glazing
                    MatNumGlSh = Construct(ConstrNumSh)%LayerPoint(1)
                    TransGlSh = Material(MatNumGlSh)%Trans
                    ReflGlSh = Material(MatNumGlSh)%ReflectSolBeamFront
                    AbsGlSh = 1.0d0-TransGlSh-ReflGlSh
                    TransGl = InterpSw(SwitchFac,TransGl,TransGlSh)
                    ReflGl = InterpSw(SwitchFac,ReflGl,ReflGlSh)
                    AbsGl = InterpSw(SwitchFac,AbsGl,AbsGlSh)
                  END IF
                  DividerRefl = 1.0d0-DividerAbs
                  DividerAbs = AbsGl + TransGl*(DividerAbs + DividerRefl*AbsGl)/(1.0d0-DividerRefl*ReflGl)
                END IF

                BeamDivHorFaceInc = 0.0d0
                BeamDivVertFaceInc = 0.0d0
                ! Beam incident on horizontal and vertical projection faces of divider if no exterior shading
                IF(DivProjOut > 0.0d0 .and. ShadeFlag /= ExtShadeOn .and.   &
                   ShadeFlag /= ExtBlindOn .and. ShadeFlag /= ExtScreenOn)THEN
                  BeamDivHorFaceInc = BeamSolarRad * CosIncAngHorProj * FrameDivider(FrDivNum)%HorDividers * &
                    DivProjOut * (Surface(SurfNum)%Width - FrameDivider(FrDivNum)%VertDividers*DivWidth) * &
                    FracSunLit / DivArea
                  BeamDivVertFaceInc = BeamSolarRad * CosIncAngVertProj * FrameDivider(FrDivNum)%VertDividers * &
                    DivProjOut * (Surface(SurfNum)%Height - FrameDivider(FrDivNum)%HorDividers*DivWidth) * &
                    FracSunLit / DivArea
                END IF
                DivIncSolarOutBm  = 0.0d0
                DivIncSolarOutDif = 0.0d0
                DivIncSolarInBm   = 0.0d0
                DivIncSolarInDif  = 0.0d0
                IF(ShadeFlag /= ExtShadeOn .and. ShadeFlag /= ExtBlindOn .AND. ShadeFlag /= BGShadeOn &
                       .AND. ShadeFlag /= BGBlindOn .and. ShadeFlag /= ExtScreenOn) THEN  ! No exterior or between-glass shading
                  DivIncSolarOutBm =  BeamFaceInc + BeamDivHorFaceInc + BeamDivVertFaceInc
                  DivIncSolarOutDif = DifSolarFaceInc*(1.0d0+SurfaceWindow(SurfNum)%ProjCorrDivOut)
                  IF(DivProjIn > 0.0d0) THEN
                    TransGl = POLYF(CosInc,Construct(ConstrNum)%TransSolBeamCoef(1:6))
                    TransDiffGl = Construct(ConstrNum)%TransDiff
                    IF(ShadeFlag == SwitchableGlazing) THEN   ! Switchable glazing
                      TransGlSh = POLYF(CosInc,Construct(ConstrNumSh)%TransSolBeamCoef(1:6))
                      TransGl = InterpSw(SwitchFac,TransGl,TransGlSh)
                      TransDiffGlSh = Construct(ConstrNumSh)%TransDiff
                      TransDiffGl = InterpSw(SwitchFac,TransDiffGl,TransDiffGlSh)
                    END IF
                    ! Beam plus diffuse solar on inside of divider
                    BeamDivHorFaceIncIn  = BeamSolarRad * CosIncAngHorProj * FrameDivider(FrDivNum)%HorDividers * &
                      DivProjIn * (Surface(SurfNum)%Width - FrameDivider(FrDivNum)%VertDividers*DivWidth) * &
                      FracSunLit / DivArea
                    BeamDivVertFaceIncIn = BeamSolarRad * CosIncAngVertProj * FrameDivider(FrDivNum)%VertDividers * &
                      DivProjIn * (Surface(SurfNum)%Height - FrameDivider(FrDivNum)%HorDividers*DivWidth) * &
                      FracSunLit / DivArea
                    DivIncSolarInBm  = TransGl*(BeamDivHorFaceIncIn + BeamDivVertFaceIncIn)
                    DivIncSolarInDif = TransDiffGl * DifSolarFaceInc * SurfaceWindow(SurfNum)%ProjCorrDivIn
                  END IF
                ELSE  ! Exterior shade, screen or blind present
                  DivIncSolarOutBm  = BeamFaceInc * (1.0d0+SurfaceWindow(SurfNum)%ProjCorrDivOut)
                  DivIncSolarOutDif = DifSolarFaceInc * (1.0d0+SurfaceWindow(SurfNum)%ProjCorrDivOut)
                  DivIncSolarInBm   = BeamFaceInc * SurfaceWindow(SurfNum)%ProjCorrDivIn * &
                                          Construct(ConstrNum)%TransDiff
                  DivIncSolarInDif  = DifSolarFaceInc * SurfaceWindow(SurfNum)%ProjCorrDivIn * &
                                          Construct(ConstrNum)%TransDiff
                END IF

                IF(ShadeFlag /= ExtShadeOn .and. ShadeFlag /= ExtBlindOn .and. ShadeFlag /= ExtScreenOn .and. &
                   ShadeFlag /= BGShadeOn .and. ShadeFlag /= BGBlindOn) THEN  ! No exterior or between-glass shade, screen or blind
                  SurfaceWindow(SurfNum)%DividerQRadOutAbs = DividerAbs*(DivIncSolarOutBm+DivIncSolarOutDif)
                  SurfaceWindow(SurfNum)%DividerQRadInAbs  = DividerAbs*(DivIncSolarInBm+DivIncSolarInDif)
                 ! Exterior shade, screen or blind
                ELSE IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN

                  IF(ShadeFlag == ExtBlindOn) THEN  ! Exterior blind
                    BlNum = SurfaceWindow(SurfNum)%BlindNumber
                    CALL ProfileAngle(SurfNum,SOLCOS,Blind(BlNum)%SlatOrientation,ProfAng)
                    SlatAng = SurfaceWindow(SurfNum)%SlatAngThisTS
                    TBlBmBm = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth,Blind(BlNum)%SlatSeparation, &
                                         Blind(BlNum)%SlatThickness)
                    TBlBmDif= InterpProfSlatAng(ProfAng,SlatAng,SurfaceWindow(SurfNum)%MovableSlats, &
                                Blind(BlNum)%SolFrontBeamDiffTrans)
                    SurfaceWindow(SurfNum)%DividerQRadOutAbs = DividerAbs * (DivIncSolarOutBm * &
                      (TBlBmBm+TBlBmDif) + DivIncSolarOutDif* &
                       InterpSlatAng(SlatAng,SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%SolFrontDiffDiffTrans))
                    SurfaceWindow(SurfNum)%DividerQRadInAbs = DividerAbs * (DivIncSolarInBm * &
                      (TBlBmBm+TBlBmDif) + DivIncSolarInDif* &
                       InterpSlatAng(SlatAng,SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%SolFrontDiffDiffTrans))

                  ELSE IF(ShadeFlag == ExtShadeOn) THEN  ! Exterior shade
                    SurfaceWindow(SurfNum)%DividerQRadOutAbs = DividerAbs * &
                      Material(Construct(ConstrNumSh)%LayerPoint(1))%Trans * &
                      (DivIncSolarOutBm + DivIncSolarOutDif)
                    SurfaceWindow(SurfNum)%DividerQRadInAbs = DividerAbs * &
                      Material(Construct(ConstrNumSh)%LayerPoint(1))%Trans * &
                      (DivIncSolarInBm + DivIncSolarInDif)
                  ELSE IF(ShadeFlag == ExtScreenOn) THEN  ! Exterior screen
                    SurfaceWindow(SurfNum)%DividerQRadOutAbs = DividerAbs * &
                      (SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmBmTrans + &
                       SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmDifTrans) * &
                      (DivIncSolarOutBm + DivIncSolarOutDif)
                    SurfaceWindow(SurfNum)%DividerQRadInAbs = DividerAbs * &
                      (SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmBmTrans + &
                       SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmDifTrans) * &
                      (DivIncSolarInBm + DivIncSolarInDif)
                  END IF
                END IF

              END IF

            END IF

          END IF ! RoughIndexMovInsul <= 0, no movable insulation

          IF (Surface(SurfNum)%HeatTransSurf .AND. Construct(ConstrNum)%TransDiff <= 0.0d0) THEN ! Opaque heat transfer surface
            QRadSWOutAbs(SurfNum) = AOSurf(SurfNum) * BeamSolarRad + AbsExt * (SkySolarInc + GndSolarInc)
            SWOutAbsTotalReport(SurfNum) = QRadSWOutAbs(SurfNum) * Surface(SurfNum)%Area
            SWOutAbsEnergyReport(SurfNum) = SWOutAbsTotalReport(SurfNum) * SecInHour * TimeStepZone
          ENDIF
        END IF ! Surface(SurfNum)%HeatTransSurf

      END IF ! Surface(SurfNum)%ExtSolar

      IF (Surface(SurfNum)%HeatTransSurf .and. ConstrNum > 0) THEN
        SurfSolIncPtr = SurfaceScheduledSolarInc(SurfNum, ConstrNum)
        IF (SurfSolIncPtr == 0) THEN
          IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN ! Opaque surface
            QRadSWInAbs(SurfNum) = QRadSWInAbs(SurfNum) + AISurf(SurfNum)*BeamSolarRad
            IF (InShelfSurf > 0) THEN ! Inside daylighting shelf
              ! Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
              OpaqSurfInsFaceBeamSolAbsorbed(SurfNum) = AISurf(SurfNum)*BeamSolarRad*(0.5*Surface(SurfNum)%Area)
            ELSE ! Regular surface
              OpaqSurfInsFaceBeamSolAbsorbed(SurfNum) = AISurf(SurfNum)*BeamSolarRad*Surface(SurfNum)%Area
            END IF
          ENDIF
        ELSE
          QRadSWInAbs(SurfNum) = QRadSWInAbs(SurfNum) + AISurf(SurfNum)
        END IF
      END IF

    END DO  !End of surface loop

  END IF  ! End of sun-up check

  RETURN

END SUBROUTINE InitSolarHeatGains


SUBROUTINE InitIntSolarDistribution

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Anonymous
          !       DATE WRITTEN   July 1977
          !       MODIFIED       Oct 1999 (FW) to handle movable shades
          !                      May 2000 (FW) to handle window frame and dividers
          !                      May 2001 (FW) to handle window blinds
          !                      Jan 2002 (FW) mods for between-glass shade/blind
          !                      May 2006 (RR) to handle exterior window screens
          !       RE-ENGINEERED  Mar98 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the arrays associated with solar heat
          ! gains for both individual surfaces and for zones.

          ! METHODOLOGY EMPLOYED:
          ! If the sun is down, all of the pertinent arrays are zeroed.  If the
          ! sun is up, various calculations are made.

          ! REFERENCES:
          ! (I)BLAST legacy routine QSUN

          ! USE STATEMENTS:
  USE General, ONLY: InterpSw, InterpSlatAng
  USE HeatBalanceMovableInsulation
  USE DaylightingDevices, ONLY: DistributeTDDAbsorbedSolar
  USE DataWindowEquivalentLayer

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: AbsExt             ! Solar absorptance of outermost layer (or movable insulation if present)
  REAL(r64)    :: AbsInt         ! Inside opaque surface solar absorptance
  REAL(r64)    :: AbsIntSurf    ! Inside opaque surface solar absorptance
  REAL(r64)    :: AbsIntSurfVis   ! Inside opaque surface visible absorptance
  REAL(r64)    :: HMovInsul          ! Resistance or "h" value of movable insulation (from EvalOutsideMovableInsulation, not used)
  INTEGER :: OtherZoneNum       ! DO loop counter for zones
  INTEGER :: RoughIndexMovInsul ! Roughness index of movable insulation
  INTEGER :: ConstrNum          ! Construction number
  INTEGER :: SurfNum            ! Surface number
  INTEGER :: ZoneNum            ! Zone number
  INTEGER :: ConstrNumSh        ! Shaded construction number
  INTEGER :: SurfNumAdjZone     ! Surface number in adjacent zone for interzone surfaces
  INTEGER :: IGlass             ! Glass layer counter
  INTEGER :: ShadeFlag          ! Shading flag
  REAL(r64)    :: DividerThermAbs    ! Window divider thermal absorptance
  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
  REAL(r64)    :: DividerSolRefl        ! Window divider solar reflectance
  INTEGER :: MatNumGl           ! Glass layer material number
  INTEGER :: MatNumSh           ! Shade layer material number
  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance
  INTEGER :: BlNum              ! Blind number
  INTEGER :: TotGlassLayers     ! Number of glass layers in a window construction
  REAL(r64)    :: BlAbsDiffBk        ! Glass layer back diffuse solar absorptance when blind in place
  REAL(r64)    :: AbsDiffBkBl        ! Blind diffuse back solar absorptance as part of glazing system
  REAL(r64)    :: EffBlEmiss         ! Blind emissivity (thermal absorptance) as part of glazing system
  REAL(r64) :: pulseMultipler     ! use to create a pulse for the load component report computations
  REAL(r64) :: curQL = 0.0d0        ! radiant value prior to adjustment for pulse for load component report
  REAL(r64) :: adjQL = 0.0d0        ! radiant value including adjustment for pulse for load component report
  INTEGER   :: EQLNum             ! equivalent layer fenestration index
  INTEGER   :: Lay                ! equivalent layer fenestration layer index

          ! FLOW:

  IF (.NOT. ALLOCATED(QS)) ALLOCATE(QS(NumOfZones))
  IF (.NOT. ALLOCATED(QSLights)) ALLOCATE(QSLights(NumOfZones))

  QS = 0.d0
  QSLights = 0.d0

          ! COMPUTE TOTAL SHORT-WAVE RADIATION ORIGINATING IN ZONE.
          ! Note: If sun is not up, QS is only internal gains
  DO ZoneNum = 1, NumOfZones
    QS(ZoneNum) = QD(ZoneNum)  + ZoneIntGain(ZoneNum)%QLTSW
    QSLights(ZoneNum) = ZoneIntGain(ZoneNum)%QLTSW
  END DO

  IF (InterZoneWindow) THEN  ! DO INTERZONE DISTRIBUTION.

    DO ZoneNum = 1, NumOfZones

      IF (RecDifShortFromZ(ZoneNum)) THEN

        DO OtherZoneNum = 1, NumOfZones

          IF ( (OtherZoneNum /= ZoneNum) .AND.(RecDifShortFromZ(OtherZoneNum)) ) THEN
            QS(ZoneNum) = QS(ZoneNum) + FractDifShortZtoZ(OtherZoneNum,ZoneNum)* &
                                        (QD(OtherZoneNum)+ZoneIntGain(OtherZoneNum)%QLTSW)
            ZoneDifSolFrIntWinsRep(ZoneNum) = ZoneDifSolFrIntWinsRep(ZoneNum) +  &
                                        FractDifShortZtoZ(OtherZoneNum,ZoneNum) * QD(OtherZoneNum)
            ZoneDifSolFrIntWinsRepEnergy(ZoneNum) = ZoneDifSolFrIntWinsRep(ZoneNum) * TimeStepZone * SecInHour
          END IF
        END DO

      END IF

    END DO

  END IF

          ! Beam and diffuse solar on inside surfaces from interior windows (for reporting)

  DO SurfNum = 1,TotSurfaces
    IF(.NOT.Surface(SurfNum)%HeatTransSurf) CYCLE
    !!!! Following may need to be removed or changed when shelves are considered in adjacent reflection calculations
    IF (Surface(SurfNum)%Class == SurfaceClass_Shading) CYCLE
      ZoneNum = Surface(SurfNum)%Zone
      IntBmIncInsSurfIntensRep(SurfNum)  = ZoneBmSolFrIntWinsRep(ZoneNum)/Zone(ZoneNum)%TotalSurfArea
      IntBmIncInsSurfAmountRep(SurfNum)  = IntBmIncInsSurfIntensRep(SurfNum) *  &
                                             (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea)
      IntBmIncInsSurfAmountRepEnergy(SurfNum)  = IntBmIncInsSurfAmountRep(SurfNum) * TimeStepZone * SecInHour
!      IntDifIncInsSurfIntensRep(SurfNum) = ZoneDifSolFrIntWinsRep(ZoneNum)/Zone(ZoneNum)%TotalSurfArea
!      IntDifIncInsSurfAmountRep(SurfNum) = IntDifIncInsSurfIntensRep(SurfNum) *  &
!                                             (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea)
!      IntDifIncInsSurfAmountRepEnergy(SurfNum) = IntDifIncInsSurfAmountRep(SurfNum) * TimeStepZone * SecInHour
  END DO

          ! COMPUTE CONVECTIVE GAINS AND ZONE FLUX DENSITY.
  DO ZoneNum = 1, NumOfZones
    QS(ZoneNum) = QS(ZoneNum) * FractDifShortZtoZ(ZoneNum,ZoneNum) * VMULT(ZoneNum)
     ! CR 8695, VMULT not based on visible
    QSLights(ZoneNum) = QSLights(ZoneNum) * FractDifShortZtoZ(ZoneNum,ZoneNum) * VMULT(ZoneNum)
  END DO

          ! COMPUTE RADIANT GAINS ON SURFACES
  DO SurfNum = 1, TotSurfaces

    ZoneNum = Surface(SurfNum)%Zone
    IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. ZoneNum == 0) CYCLE  ! Skip non-heat transfer surfaces
    IF (Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE  ! Skip tubular daylighting device domes

    ConstrNum = Surface(SurfNum)%Construction

    IF(Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Opaque surface
      AbsIntSurf = Construct(ConstrNum)%InsideAbsorpSolar
      AbsIntSurfVis = Construct(ConstrNum)%InsideAbsorpSolar !to fix CR 8695 change to this = Construct(ConstrNum)%InsideAbsorpVis
      HMovInsul = 0.0d0
      IF (Surface(SurfNum)%MaterialMovInsulInt.GT.0) &
        CALL EvalInsideMovableInsulation(SurfNum,HMovInsul,AbsInt)
      IF (HMovInsul > 0.0d0) AbsIntSurf = AbsInt
      QRadSWInAbs(SurfNum) = QRadSWInAbs(SurfNum) + QS(ZoneNum)*AbsIntSurf
      QRadSWLightsInAbs(SurfNum) = QRadSWLightsInAbs(SurfNum) + QSLights(ZoneNum)*AbsIntSurfVis
    ELSE  ! Window

      IF ( SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
          ConstrNumSh = Surface(SurfNum)%ShadedConstruction
          IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
            ConstrNum   = Surface(SurfNum)%StormWinConstruction
            ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
          END IF
          TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
          ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag

          ! These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
          pulseMultipler = 0.01d0   ! the W/sqft pulse for the zone
          IF (.NOT. doLoadComponentPulseNow) THEN
            QRadThermInAbs(SurfNum) = QL(ZoneNum) * TMULT(ZoneNum) * ITABSF(SurfNum)
          ELSE
            curQL = QL(ZoneNum)
            ! for the loads component report during the special sizing run increase the radiant portion
            ! a small amount to create a "pulse" of heat that is used for the
            adjQL = curQL + Zone(ZoneNum)%FloorArea * pulseMultipler
            ! ITABSF is the Inside Thermal Absorptance
            ! TMULT is a mulipliter for each zone
            ! QRadThermInAbs is the thermal radiation absorbed on inside surfaces
            QRadThermInAbs(SurfNum) = adjQL * TMULT(ZoneNum) * ITABSF(SurfNum)
          END IF

          IF(ShadeFlag <= 0) THEN  ! No window shading
            DO IGlass = 1,TotGlassLayers
              QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) +  &
                QS(ZoneNum)*Construct(ConstrNum)%AbsDiffBack(IGlass)
            END DO
          ELSE IF(ShadeFlag == IntShadeOn .OR. ShadeFlag >= 3) THEN
                ! Interior, exterior or between-glass shade, screen or blind in place
            DO IGlass = 1,Construct(ConstrNumSh)%TotGlassLayers
              IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) &
                QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) +  &
                  QS(ZoneNum)*Construct(ConstrNumSh)%AbsDiffBack(IGlass)
              IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn) THEN
                BlAbsDiffBk = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                                     Construct(ConstrNumSh)%BlAbsDiffBack(IGlass,1:MaxSlatAngs))
                QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) + QS(ZoneNum)*BlAbsDiffBk
              END IF
            END DO
            BlNum = SurfaceWindow(SurfNum)%BlindNumber
            IF(ShadeFlag == IntShadeOn) &
              SurfaceWindow(SurfNum)%IntLWAbsByShade = &
                QL(ZoneNum) * Construct(ConstrNumSh)%ShadeAbsorpThermal * TMULT(ZoneNum)
            IF(ShadeFlag == IntBlindOn) THEN
              EffBlEmiss = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                                     SurfaceWindow(SurfNum)%EffShBlindEmiss)
              SurfaceWindow(SurfNum)%IntLWAbsByShade = QL(ZoneNum) * EffBlEmiss * TMULT(ZoneNum)
            END IF
            IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) &
              SurfaceWindow(SurfNum)%IntSWAbsByShade = QS(ZoneNum)*Construct(ConstrNumSh)%AbsDiffBackShade
            IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==BGBlindOn) THEN
              AbsDiffBkBl = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                                     Construct(ConstrNumSh)%AbsDiffBackBlind)
              SurfaceWindow(SurfNum)%IntSWAbsByShade = QS(ZoneNum)*AbsDiffBkBl
            END IF
            ! Correct for divider shadowing
            IF(ShadeFlag == ExtShadeOn.OR.ShadeFlag == ExtBlindOn.OR.ShadeFlag == ExtScreenOn) &
              SurfaceWindow(SurfNum)%IntSWAbsByShade = SurfaceWindow(SurfNum)%IntSWAbsByShade * SurfaceWindow(SurfNum)%GlazedFrac

          ELSE IF(ShadeFlag == SwitchableGlazing) THEN  ! Switchable glazing
            DO IGlass = 1,TotGlassLayers
             QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) +  &
               QS(ZoneNum) * InterpSw(SurfaceWindow(SurfNum)%SwitchingFactor,  &
                                      Construct(ConstrNum)%AbsDiffBack(IGlass),  &
                                      Construct(ConstrNumSh)%AbsDiffBack(IGlass))
            END DO

          END IF  ! End of shading flag check

          IF(SurfaceWindow(SurfNum)%FrameArea > 0.0d0) &  ! Window has a frame
            ! Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
            SurfaceWindow(SurfNum)%FrameQRadInAbs = SurfaceWindow(SurfNum)%FrameQRadInAbs + &
              ( QS(ZoneNum) * SurfaceWindow(SurfNum)%FrameSolAbsorp + &
              (QL(ZoneNum)*TMULT(ZoneNum) + QHTRadSysSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + &
              QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)) * &
              SurfaceWindow(SurfNum)%FrameEmis ) * (1.0d0+0.5d0*SurfaceWindow(SurfNum)%ProjCorrFrIn)
          IF(SurfaceWindow(SurfNum)%DividerArea > 0.0d0) THEN  ! Window has dividers
            DividerThermAbs = SurfaceWindow(SurfNum)%DividerEmis
            DividerSolAbs = SurfaceWindow(SurfNum)%DividerSolAbsorp
            IF(SurfaceWindow(SurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
              TransGl = Material(MatNumGl)%Trans
              ReflGl = Material(MatNumGl)%ReflectSolBeamBack
              AbsGl = 1.0d0-TransGl-ReflGl
              DividerSolRefl = 1.0d0-DividerSolAbs
              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.0d0-DividerSolRefl*ReflGl)
              DividerThermAbs = Material(MatNumGl)%AbsorpThermalBack
            END IF
            ! Correct for interior shade transmittance
            IF(ShadeFlag == IntShadeOn) THEN
              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
              DividerSolAbs = DividerSolAbs * Material(MatNumSh)%Trans
              DividerThermAbs = DividerThermAbs * Material(MatNumSh)%TransThermal
            ELSE IF(ShadeFlag == IntBlindOn) THEN
              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                  SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
              DividerThermAbs = DividerThermAbs * InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                  SurfaceWindow(SurfNum)%MovableSlats, Blind(BlNum)%IRBackTrans)
            END IF
            ! Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains
            SurfaceWindow(SurfNum)%DividerQRadInAbs = SurfaceWindow(SurfNum)%DividerQRadInAbs + &
               ( QS(ZoneNum)*DividerSolAbs + &
               (QL(ZoneNum)*TMULT(ZoneNum) + QHTRadSysSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + &
               QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum))*DividerThermAbs) * &
               (1.0d0+SurfaceWindow(SurfNum)%ProjCorrDivIn)
          END IF

      ELSE IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN

        !ConstrNumSh = Surface(SurfNum)%ShadedConstruction
        ConstrNum   = Surface(SurfNum)%Construction
        !TotGlassLayers = Construct(ConstrNum)%TotGlassLayers

        ! These calculations are repeated from InitInternalHeatGains for the Zone Component Loads Report
        pulseMultipler = 0.01d0   ! the W/sqft pulse for the zone
        IF (.NOT. doLoadComponentPulseNow) THEN
            QRadThermInAbs(SurfNum) = QL(ZoneNum) * TMULT(ZoneNum) * ITABSF(SurfNum)
        ELSE
            curQL = QL(ZoneNum)
            ! for the loads component report during the special sizing run increase the radiant portion
            ! a small amount to create a "pulse" of heat that is used for the
            adjQL = curQL + Zone(ZoneNum)%FloorArea * pulseMultipler
            ! ITABSF is the Inside Thermal Absorptance
            ! TMULT is a mulipliter for each zone
            ! QRadThermInAbs is the thermal radiation absorbed on inside surfaces
            QRadThermInAbs(SurfNum) = adjQL * TMULT(ZoneNum) * ITABSF(SurfNum)
        END IF
        ! Radiations absorbed by the window layers coming from zone side
        EQLNum = Construct(ConstrNum)%EQLConsPtr
        DO Lay = 1, CFS(EQLNum)%NL
            QRadSWwinAbs(SurfNum,Lay) = QRadSWwinAbs(SurfNum,Lay) &
                                      + QS(ZoneNum) * Construct(ConstrNum)%AbsDiffBackEQL(Lay)
        END DO
        ! Window frame has not been included for equivalent layer model yet


      END IF ! end if for IF ( SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN

    END IF  ! End of opaque surface vs. window check

          ! OUTSIDE OF SURFACE CASES
    IF (Surface(SurfNum)%ExtBoundCond > 0) THEN  ! Interzone surface

      IF (Construct(ConstrNum)%TransDiff > 0.0d0) THEN  ! Interzone window

        ! Short-wave radiation absorbed in panes of corresponding window in adjacent zone
        SurfNumAdjZone = Surface(SurfNum)%ExtBoundCond

        IF (SurfaceWindow(SurfNumAdjZone)%WindowModelType /= WindowEQLModel) THEN
            DO IGlass =1,TotGlassLayers
              QRadSWwinAbs(SurfNumAdjZone,IGlass) =  QRadSWwinAbs(SurfNumAdjZone,IGlass) +  &
                QS(ZoneNum) * Construct(Surface(SurfNumAdjZone)%Construction)%AbsDiff(IGlass)
                ! Note that AbsDiff rather than AbsDiffBack is used in the above since the
                ! radiation from the current zone is incident on the outside of the adjacent
                ! zone's window.
            END DO
        ELSE  ! IF (SurfaceWindow(SurfNumAdjZone)%WindowModelType == WindowEQLModel) THEN
            ConstrNum   = Surface(SurfNumAdjZone)%Construction
            EQLNum = Construct(ConstrNum)%EQLConsPtr
            DO Lay = 1, CFS(EQLNum)%NL
                QRadSWwinAbs(SurfNumAdjZone,Lay) = QRadSWwinAbs(SurfNumAdjZone,Lay) &
                               + QS(ZoneNum) * Construct(ConstrNum)%AbsDiffFrontEQL(Lay)
                ! Note that AbsDiffFrontEQL rather than AbsDiffBackEQL is used in the above
                ! since the radiation from the current zone is incident on the outside of the
                ! adjacent zone's window.
            END DO
        ENDIF
      END IF

    ELSE  IF(Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Opaque exterior surface
      ! Calculate absorbed solar on outside if movable exterior insulation in place
      HMovInsul = 0.0d0
      IF (Surface(SurfNum)%MaterialMovInsulExt.GT.0) &
        CALL EvalOutsideMovableInsulation(SurfNum,HMovInsul,RoughIndexMovInsul,AbsExt)
      IF (HMovInsul > 0) THEN    ! Movable outside insulation in place
        QRadSWOutMvIns(SurfNum) = QRadSWOutAbs(SurfNum)*AbsExt &
                                         /Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar
          ! For transparent insulation, allow some sunlight to get through the movable insulation.
          ! The equation below is derived by taking what is transmitted through the layer and applying
          ! the fraction that is absorbed plus the back reflected portion (first order reflection only)
          ! to the plane between the transparent insulation and the exterior surface face.
        QRadSWOutAbs(SurfNum) = Material(Surface(SurfNum)%MaterialMovInsulExt)%Trans               &
                               *QRadSWOutMvIns(SurfNum)                                            &
                               *((Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar/AbsExt) &
                                 +(1-Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar))
        SWOutAbsTotalReport(SurfNum) = QRadSWOutAbs(SurfNum) * Surface(SurfNum)%Area
        SWOutAbsEnergyReport(SurfNum) = SWOutAbsTotalReport(SurfNum) * SecInHour * TimeStepZone
      END IF

    END IF

  END DO

  ! RJH 08/30/07 - Add InitialDifSolInAbs, InitialDifSolwinAbs, and InitialDifSolAbsByShade
  ! calced in CalcWinTransDifSolInitialDistribution to QRadSWInAbs, QRadSWwinAbs, and IntSWAbsByShade here
  DO SurfNum = 1, TotSurfaces
    ZoneNum = Surface(SurfNum)%Zone
    IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. ZoneNum == 0) CYCLE  ! Skip non-heat transfer surfaces
    IF (Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE  ! Skip tubular daylighting device domes
    ConstrNum = Surface(SurfNum)%Construction
    IF(Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Opaque surface
      QRadSWInAbs(SurfNum) = QRadSWInAbs(SurfNum) + InitialDifSolInAbs(SurfNum)
    ELSE  ! Window
       IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel .AND. &
             SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
          ConstrNumSh = Surface(SurfNum)%ShadedConstruction
          IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
            ConstrNum   = Surface(SurfNum)%StormWinConstruction
            ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
          END IF
          TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
          ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
          IF(ShadeFlag <= 0) THEN  ! No window shading
            DO IGlass = 1,TotGlassLayers
              QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) +  &
                InitialDifSolwinAbs(SurfNum,IGlass)
            END DO
          ELSE IF(ShadeFlag == IntShadeOn .OR. ShadeFlag >= 3) THEN
                ! Interior, exterior or between-glass shade, screen or blind in place
            DO IGlass = 1,Construct(ConstrNumSh)%TotGlassLayers
              QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) + InitialDifSolwinAbs(SurfNum,IGlass)
            END DO
            IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) &
              SurfaceWindow(SurfNum)%IntSWAbsByShade = SurfaceWindow(SurfNum)%IntSWAbsByShade  &
                                                        + SurfaceWindow(SurfNum)%InitialDifSolAbsByShade
            IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==BGBlindOn) THEN
              SurfaceWindow(SurfNum)%IntSWAbsByShade = SurfaceWindow(SurfNum)%IntSWAbsByShade  &
                                                        + SurfaceWindow(SurfNum)%InitialDifSolAbsByShade
            END IF
          ELSE IF(ShadeFlag == SwitchableGlazing) THEN  ! Switchable glazing
            DO IGlass = 1,TotGlassLayers
             QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) +  &
               InitialDifSolwinAbs(SurfNum,IGlass)
            END DO
          END IF  ! End of shading flag check
       ELSE IF (SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) THEN
          TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
          DO IGlass = 1,TotGlassLayers
            QRadSWwinAbs(SurfNum,IGlass) = QRadSWwinAbs(SurfNum,IGlass) +  &
                InitialDifSolwinAbs(SurfNum,IGlass)
          END DO
       ELSE IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN

          !ConstrNum   = Surface(SurfNum)%Construction
          EQLNum = Construct(ConstrNum)%EQLConsPtr

          DO Lay = 1, CFS(EQLNum)%NL
             QRadSWwinAbs(SurfNum,Lay) = QRadSWwinAbs(SurfNum,Lay) &
                                       + InitialDifSolwinAbs(SurfNum,Lay)
          END DO

       ENDIF
    END IF  ! End of Opaque surface vs. Window check
  END DO  ! End of SurfNum loop to initialize SW Absorbed values with CalcWinTransDifSolInitialDistribution results

  ! RJH 09/07/07 - report variables for surface absorbed short wave radiation
  DO SurfNum = 1, TotSurfaces
    SWwinAbsTotalReport(SurfNum) = 0.0d0
    SWInAbsTotalReport(SurfNum) = 0.0d0
    InitialDifSolInAbsReport(SurfNum) = 0.0d0
    InitialDifSolInTransReport(SurfNum) = 0.0d0
    ZoneNum = Surface(SurfNum)%Zone
    IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. ZoneNum == 0) CYCLE  ! Skip non-heat transfer surfaces
    IF (Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE  ! Skip tubular daylighting device domes
    ConstrNum = Surface(SurfNum)%Construction
    IF(Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Opaque surface
      ! Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
      InitialDifSolInAbsReport(SurfNum) = InitialDifSolInAbs(SurfNum) * Surface(SurfNum)%Area
      ! Total Shortwave Radiation Absorbed on Inside of Surface[W]
      SWInAbsTotalReport(SurfNum) = QRadSWInAbs(SurfNum) * Surface(SurfNum)%Area
    ELSE  ! Window
      ! Initial Transmitted Diffuse Solar Transmitted Through Inside of Surface[W]
      InitialDifSolInTransReport(SurfNum) = InitialDifSolInTransReport(SurfNum) &
                                              + InitialDifSolInTrans(SurfNum) * Surface(SurfNum)%Area
      IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
          ConstrNumSh = Surface(SurfNum)%ShadedConstruction
          IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
            ConstrNum   = Surface(SurfNum)%StormWinConstruction
            ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
          END IF
          IF (SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) THEN
            TotGlassLayers = Construct(ConstrNum)%TotSolidLayers
          ELSE
            TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
          END IF
          ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
          IF(ShadeFlag <= 0) THEN  ! No window shading
            DO IGlass = 1,TotGlassLayers
              ! Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
              InitialDifSolInAbsReport(SurfNum) = InitialDifSolInAbsReport(SurfNum) &
                                                    + InitialDifSolwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
              ! Total Shortwave Radiation Absorbed on Inside of Surface[W]
              SWInAbsTotalReport(SurfNum) = SWInAbsTotalReport(SurfNum) &
                                              + QRadSWwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
              ! Total Shortwave Absorbed:All Glass Layers[W]
              SWwinAbsTotalReport(SurfNum) = SWwinAbsTotalReport(SurfNum) &
                                                    + QRadSWwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
            END DO
          ELSE IF(ShadeFlag == IntShadeOn .OR. ShadeFlag >= 3) THEN
                ! Interior, exterior or between-glass shade, screen or blind in place
            DO IGlass = 1,Construct(ConstrNumSh)%TotGlassLayers
              ! Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
              InitialDifSolInAbsReport(SurfNum) = InitialDifSolInAbsReport(SurfNum) &
                                                    + InitialDifSolwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
              ! Total Shortwave Radiation Absorbed on Inside of Surface[W]
              SWInAbsTotalReport(SurfNum) = SWInAbsTotalReport(SurfNum) &
                                              + QRadSWwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
              ! Total Shortwave Absorbed:All Glass Layers[W]
              SWwinAbsTotalReport(SurfNum) = SWwinAbsTotalReport(SurfNum) &
                                              + QRadSWwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
            END DO
          ELSE IF(ShadeFlag == SwitchableGlazing) THEN  ! Switchable glazing
            DO IGlass = 1,TotGlassLayers
              ! Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
              InitialDifSolInAbsReport(SurfNum) = InitialDifSolInAbsReport(SurfNum) &
                                                    + InitialDifSolwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
              ! Total Shortwave Radiation Absorbed on Inside of Surface[W]
              SWInAbsTotalReport(SurfNum) = SWInAbsTotalReport(SurfNum) &
                                              + QRadSWwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
              ! Total Shortwave Absorbed:All Glass Layers[W]
              SWwinAbsTotalReport(SurfNum) = SWwinAbsTotalReport(SurfNum) &
                                              + QRadSWwinAbs(SurfNum,IGlass) * Surface(SurfNum)%Area
            END DO
          END IF  ! End of shading flag check
      ELSE !IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
        ConstrNum   = Surface(SurfNum)%Construction
        EQLNum = Construct(ConstrNum)%EQLConsPtr
        DO Lay = 1, CFS(EQLNum)%NL

          ! Initial Transmitted Diffuse Solar Absorbed on Inside of Surface[W]
          InitialDifSolInAbsReport(SurfNum) = InitialDifSolInAbsReport(SurfNum) &
                                            + InitialDifSolwinAbs(SurfNum,Lay) * Surface(SurfNum)%Area
          ! Total Shortwave Radiation Absorbed on Inside of Surface[W]
          SWInAbsTotalReport(SurfNum) = SWInAbsTotalReport(SurfNum) &
                                      + QRadSWwinAbs(SurfNum,Lay) * Surface(SurfNum)%Area
          ! Total Shortwave Absorbed:All solid Layers[W]
          SWwinAbsTotalReport(SurfNum) = SWwinAbsTotalReport(SurfNum) &
                                       + QRadSWwinAbs(SurfNum,Lay) * Surface(SurfNum)%Area

        END DO
      ENDIF
    END IF  ! End of Opaque surface vs. Window check
  END DO  ! End of SurfNum loop to report variables for surface total absorbed short wave radiation

  CALL DistributeTDDAbsorbedSolar()

  RETURN

END SUBROUTINE InitIntSolarDistribution


SUBROUTINE ComputeIntThermalAbsorpFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code (George Walton)
          !       DATE WRITTEN   Legacy: Dec 1976
          !       MODIFIED       Nov. 99, FCW: to take into account movable interior shades and switchable glazing
          !                      June 01, FCW: to take into account interior blinds.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine computes the fractions of long-wave radiation from lights, equipment and people
          ! that is absorbed by each zone surface.


          ! METHODOLOGY EMPLOYED:
          ! The fraction is assumed to be proportional to the product of the surface area times its thermal absorptivity.


          ! REFERENCES:
          ! BLAST Routine: CITAF - Compute Interior Thermal Absorption Factors

          ! USE STATEMENTS:
  USE HeatBalanceMovableInsulation
  USE General, ONLY: InterpSw, InterpSlatAng

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
  INTEGER          ConstrNum        ! Construction number
  INTEGER          ConstrNumSh      ! Shaded construction number
  INTEGER          FirstZoneSurf    ! Index of first surface in current zone
  INTEGER          LastZoneSurf     ! Index of last surface in current zone
  REAL(r64) SUM1             ! Intermediate calculation value
  REAL(r64) SUM2             ! Intermediate calculation value
  INTEGER          SurfNum          ! DO loop counter for zone surfaces
  INTEGER          ZoneNum          ! Loop counter for Zones
  INTEGER          ShadeFlag        ! Window shading flag
  REAL(r64)        HMovInsul        ! Conductance of movable insulation
  INTEGER          RoughIndexMovInsul ! Roughness index of movable insulation
  REAL(r64)        AbsExt           ! Solar absorptance of movable insulation
  REAL(r64)        DividerThermAbs  ! Window divider thermal absorptance
  INTEGER          MatNumSh         ! Shade layer material number
  REAL(r64)        TauShIR          ! Shade or blind IR transmittance
  REAL(r64)        EffShDevEmiss    ! Effective emissivity of shade or blind


  IF (.NOT. ALLOCATED(ITABSF)) THEN
    ALLOCATE(ITABSF(TotSurfaces))
    ITABSF=0.0d0
    ALLOCATE(TMULT(NumOfZones))
    TMULT=0.0d0
    ALLOCATE(TCONV(NumOfZones))
    TCONV=0.0d0
  ENDIF

  DO SurfNum = 1,TotSurfaces
    IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
    ConstrNum = Surface(SurfNum)%Construction
    ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
    ITABSF(SurfNum) = Construct(ConstrNum)%InsideAbsorpThermal
    IF(Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Opaque surface
      RoughIndexMovInsul = 0
      IF(Surface(SurfNum)%MaterialMovInsulExt > 0) &
        CALL EvalOutsideMovableInsulation(SurfNum,HMovInsul,RoughIndexMovInsul,AbsExt)
      IF (RoughIndexMovInsul > 0) & ! Movable outside insulation present
        ITABSF(SurfNum) = Material(Surface(SurfNum)%MaterialMovInsulExt)%AbsorpThermal
    END IF
    ! For window with an interior shade or blind, emissivity is a combination of glass and shade/blind emissivity
    IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==IntBlindOn) &
      ITABSF(SurfNum) = &  ! For shades, following interpolation just returns value of first element in array
          InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
            SurfaceWindow(SurfNum)%EffShBlindEmiss) + &
          InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
            SurfaceWindow(SurfNum)%EffGlassEmiss)
  END DO

  DO ZoneNum=1,NumOfZones

    SUM1 = 0.0D0
    SUM2 = 0.0D0

    FirstZoneSurf=Zone(ZoneNum)%SurfaceFirst
    LastZoneSurf=Zone(ZoneNum)%SurfaceLast

    DO SurfNum = FirstZoneSurf, LastZoneSurf

      IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE

      ConstrNum = Surface(SurfNum)%Construction
      ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
      IF(ShadeFlag /= SwitchableGlazing) THEN
        SUM1 = SUM1 + Surface(SurfNum)%Area * ITABSF(SurfNum)
      ELSE ! Switchable glazing
        SUM1 = SUM1 + Surface(SurfNum)%Area * InterpSw(SurfaceWindow(SurfNum)%SwitchingFactor,  &
            Construct(ConstrNum)%InsideAbsorpThermal, &
            Construct(SurfaceWindow(SurfNum)%ShadedConstruction)%InsideAbsorpThermal)
      END IF

                         ! Window frame and divider effects
      IF(SurfaceWindow(SurfNum)%FrameArea > 0.0d0) SUM1 = SUM1 + SurfaceWindow(SurfNum)%FrameArea * &
          (1.d0+0.5d0*SurfaceWindow(SurfNum)%ProjCorrFrIn) * SurfaceWindow(SurfNum)%FrameEmis
      IF(SurfaceWindow(SurfNum)%DividerArea > 0.0d0) THEN
        DividerThermAbs = SurfaceWindow(SurfNum)%DividerEmis
        IF(SurfaceWindow(SurfNum)%DividerType == Suspended) &
          ! Suspended (between-glass) divider; relevant emissivity is inner glass emissivity
          DividerThermAbs = Construct(ConstrNum)%InsideAbsorpThermal
        IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==IntBlindOn) THEN
          ! Interior shade or blind in place
          ConstrNumSh = SurfaceWindow(SurfNum)%ShadedConstruction
          MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
          TauShIR = Material(MatNumSh)%TransThermal
          EffShDevEmiss = SurfaceWindow(SurfNum)%EffShBlindEmiss(1)
          IF(ShadeFlag==IntBlindOn) THEN
            TauShIR = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                              Blind(SurfaceWindow(SurfNum)%BlindNumber)%IRBackTrans)
            EffShDevEmiss = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                              SurfaceWindow(SurfNum)%EffShBlindEmiss)
          END IF
          SUM1 = SUM1 + SurfaceWindow(SurfNum)%DividerArea * (EffShDevEmiss + DividerThermAbs * TauShIR)
        ELSE
          SUM1 = SUM1 + SurfaceWindow(SurfNum)%DividerArea * (1.d0+SurfaceWindow(SurfNum)%ProjCorrDivIn) * &
                            DividerThermAbs
        END IF

      END IF

    END DO  ! End of loop over surfaces in zone

    TMULT(ZoneNum) = 1.0d0/SUM1
    TCONV(ZoneNum) = SUM2/SUM1

  END DO  ! End of loop over zones

  RETURN

END SUBROUTINE ComputeIntThermalAbsorpFactors ! CITAF

SUBROUTINE ComputeIntSWAbsorpFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy (George Walton)
          !       DATE WRITTEN   Legacy (December 1980)
          !       MODIFIED       Nov. 99, FW; now called every time step to account for movable
          !                      window shades and insulation
          !                      Mar. 00, FW; change name from ComputeVisLightingAbsorpFactors
          !                      to ComputeIntSWAbsorpFactors
          !                      May 00, FW; add window frame and divider effects
          !                      June 01, FW: account for window blinds
          !                      Nov 01, FW: account for absorptance of exterior shades and interior or exterior blinds
          !                      Jan 03, FW: add between-glass shade/blind
          !                      May 06, RR: account for exterior window screens

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Computes VMULT, the inverse of the sum of area*(short-wave absorptance+transmittance) for
          ! the surfaces in a zone. VMULT is used to calculate the zone interior diffuse short-wave radiation
          ! absorbed by the inside of opaque zone surfaces or by the glass and shade/blind layers of zone windows.

          ! Sets VCONV to zero (VCONV was formerly used to calculate convective gain due to short-wave
          ! radiation absorbed by interior window shades).

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! BLAST Routine - CIVAF - Compute Surface Absorption Factors For Short Wave Radiation
          !                         From Zone Lights And Diffuse Solar.

          ! USE STATEMENTS:
  USE HeatBalanceMovableInsulation
  USE General, ONLY: InterpSw, InterpSlatAng
  USE DataWindowEquivalentLayer

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SmallestAreaAbsProductAllowed = 0.01d0   ! Avoid a division by zero of the user has entered a bunch
                                                            ! of surfaces with zero absorptivity on the inside

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          ConstrNum        ! DO loop counter for constructions
  INTEGER          FirstZoneSurf    ! Index of first surface in current zone
  INTEGER          LastZoneSurf     ! Index of last surface in current zone
  REAL(r64) SUM1             ! Intermediate calculation value for solar absorbed and transmitted
                                    !   by windows (including shade, blind or insulation, if present)
  INTEGER          SurfNum          ! DO loop counter for zone surfaces
  INTEGER          ZoneNum          ! Loop counter for Zones
  INTEGER          ShadeFlag        ! Shading flag
  INTEGER          ConstrNumSh      ! Shaded construction number
  REAL(r64)        SwitchFac        ! Switching factor
  INTEGER          Lay              ! Layer number
  REAL(r64)        AbsDiffLayWin    ! Window layer short-wave absorptance
  REAL(r64)        AbsDiffTotWin    ! Sum of window layer short-wave absorptances
  REAL(r64)        TransDiffWin     ! Window diffuse short-wave transmittance
  REAL(r64)        DiffAbsShade     ! Diffuse short-wave shade or blind absorptance
  REAL(r64)        AbsIntSurf, AbsInt ! Inside surface short-wave absorptance
  REAL(r64)        HMovInsul        ! Conductance of movable insulation
  REAL(r64)        DividerAbs       ! Window divider solar absorptance
  INTEGER          MatNumgl         ! Glass material number
  REAL(r64)        TransGl,ReflGl,AbsGl ! Glass layer short-wave transmittance, reflectance, absorptance
  REAL(r64)        DividerRefl      ! Window divider short-wave reflectance

  LOGICAL, SAVE :: FirstTime = .TRUE.   ! First time through routine
  LOGICAL, ALLOCATABLE, DIMENSION(:), SAVE :: FirstCalcZone ! for error message

          ! FLOW:

  IF (.NOT. ALLOCATED(VMULT)) THEN
    ALLOCATE(VMULT(NumOfZones))
    VMULT=0.0d0
    ALLOCATE(VCONV(NumOfZones))
    VCONV=0.0d0
  ENDIF
  IF (FirstTime) THEN
    ALLOCATE(FirstCalcZone(NumOfZones))
    FirstCalcZone=.true.
    FirstTime=.false.
  ENDIF

  DO ZoneNum = 1, NumOfZones

    SUM1 = 0.0D0

    FirstZoneSurf=Zone(ZoneNum)%SurfaceFirst
    LastZoneSurf=Zone(ZoneNum)%SurfaceLast

    DO SurfNum = FirstZoneSurf, LastZoneSurf

      IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE

      ConstrNum=Surface(SurfNum)%Construction

      IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN

        ! Opaque surface


        AbsIntSurf = Construct(ConstrNum)%InsideAbsorpSolar
        HMovInsul = 0.0d0
        IF (Surface(SurfNum)%MaterialMovInsulInt > 0) &
          CALL EvalInsideMovableInsulation(SurfNum,HMovInsul,AbsInt)
        IF (HMovInsul > 0.0d0) AbsIntSurf = AbsInt
        SUM1 = SUM1 + Surface(SurfNum)%Area*AbsIntSurf

      ELSE

        ! Window
         IF ( .NOT. Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
            ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
            AbsDiffTotWin = 0.0d0
            ConstrNumSh = Surface(SurfNum)%ShadedConstruction
            IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
              ConstrNum   = Surface(SurfNum)%StormWinConstruction
              ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
            END IF
            SwitchFac = SurfaceWindow(SurfNum)%SwitchingFactor

            ! Sum of absorptances of glass layers
            DO Lay = 1,Construct(ConstrNum)%TotGlassLayers
              AbsDiffLayWin = Construct(ConstrNum)%AbsDiffBack(Lay)

              ! Window with shade, screen or blind
              IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == ExtShadeOn .OR. ShadeFlag == BGShadeOn   &
                 .OR. ShadeFlag == ExtScreenOn) THEN
                AbsDiffLayWin = Construct(ConstrNumSh)%AbsDiffBack(Lay)
              ELSE IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == BGBlindOn) THEN
                AbsDiffLayWin = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                  SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlAbsDiffBack(Lay,1:MaxSlatAngs))
              END IF

              ! Switchable glazing
              IF(ShadeFlag == SwitchableGlazing) AbsDiffLayWin =  &
                InterpSw(SwitchFac, AbsDiffLayWin, Construct(ConstrNumSh)%AbsDiffBack(Lay))

              AbsDiffTotWin = AbsDiffTotWin + AbsDiffLayWin
            END DO

            TransDiffWin = Construct(ConstrNum)%TransDiff
            DiffAbsShade = 0.0d0

            ! Window with shade, screen or blind

            IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == ExtShadeOn .OR. ShadeFlag == BGShadeOn .OR. ShadeFlag == ExtScreenOn) THEN
              TransDiffWin = Construct(ConstrNumSh)%TransDiff
              DiffAbsShade = Construct(ConstrNumSh)%AbsDiffBackShade
            ELSE IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == BGBlindOn) THEN
              TransDiffWin = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                  SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%BlTransDiff)
              DiffAbsShade = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                  SurfaceWindow(SurfNum)%MovableSlats,Construct(ConstrNumSh)%AbsDiffBackBlind)
            END IF

            ! Switchable glazing

            IF(ShadeFlag == SwitchableGlazing) TransDiffWin =  &
               InterpSw(SwitchFac, TransDiffWin, Construct(ConstrNumSh)%TransDiff)

            SUM1 = SUM1 + Surface(SurfNum)%Area*(TransDiffWin + AbsDiffTotWin + DiffAbsShade)

            ! Window frame and divider effects (shade area is glazed area plus divider area)


            IF(SurfaceWindow(SurfNum)%FrameArea > 0.0d0) SUM1 = SUM1 + SurfaceWindow(SurfNum)%FrameArea *  &
              SurfaceWindow(SurfNum)%FrameSolAbsorp * (1.0d0+0.5d0*SurfaceWindow(SurfNum)%ProjCorrFrIn)
            IF(SurfaceWindow(SurfNum)%DividerArea > 0.0d0) THEN
              DividerAbs = SurfaceWindow(SurfNum)%DividerSolAbsorp
              IF(SurfaceWindow(SurfNum)%DividerType == Suspended) THEN
                !Suspended (between-glass) divider: account for glass on inside of divider
                MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
                TransGl = Material(MatNumGl)%Trans
                ReflGl = Material(MatNumGl)%ReflectSolBeamBack
                AbsGl = 1.0d0-TransGl-ReflGl
                DividerRefl = 1.0d0 - DividerAbs
                DividerAbs = AbsGl + TransGl*(DividerAbs + DividerRefl*AbsGl)/(1.0d0-DividerRefl*ReflGl)
              END IF
              IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==IntBlindOn) THEN
                SUM1 = SUM1 + SurfaceWindow(SurfNum)%DividerArea * (DividerAbs + DiffAbsShade)
              ELSE
                SUM1 = SUM1 + SurfaceWindow(SurfNum)%DividerArea * &
                  (1.0d0+SurfaceWindow(SurfNum)%ProjCorrDivIn) * DividerAbs
              END IF
            END IF
         ELSE       ! equivalent layer window
            ! In equivalent layer window solid layers (Glazing and shades) are treated equally
            ! frames and dividers are not supported
            AbsDiffTotWin = 0.0d0
            AbsDiffLayWin = 0.0d0
            TransDiffWin = Construct(ConstrNum)%TransDiff
            DO Lay = 1, CFS(Construct(ConstrNum)%EQLConsPtr)%NL
              AbsDiffLayWin = Construct(ConstrNum)%AbsDiffBackEQL(Lay)
              AbsDiffTotWin = AbsDiffTotWin + AbsDiffLayWin
            END DO
            SUM1 = SUM1 + Surface(SurfNum)%Area*(TransDiffWin + AbsDiffTotWin)
         ENDIF
      END IF  ! End of check if opaque surface or window
    END DO  ! End of loop over surfaces in zone

    IF (SUM1 > SmallestAreaAbsProductAllowed) THEN  ! Everything is okay, proceed with the regular calculation
      VMULT(ZoneNum) = 1.0d0/SUM1

    ELSE    ! the sum of area*solar absorptance for all surfaces in the zone is zero--either the user screwed up
            ! or they really want to disallow any solar from being absorbed on the inside surfaces.  Fire off a
            ! nasty warning message and then assume that no solar is ever absorbed (basically everything goes
            ! back out whatever window is there.  Note that this also assumes that the shade has no effect.
            ! That's probably not correct, but how correct is it to assume that no solar is absorbed anywhere
            ! in the zone?
      IF (FirstCalcZone(ZoneNum)) THEN
        CALL ShowWarningError('ComputeIntSWAbsorbFactors: Sum of area times inside solar absorption '//  &
                              'for all surfaces is zero in Zone: '//TRIM(Zone(ZoneNum)%Name))
        FirstCalcZone(ZoneNum) = .FALSE.
      END IF
      VMULT(ZoneNum) = 0.0d0

    END IF
  END DO  ! End of zone loop

  RETURN

END SUBROUTINE ComputeIntSWAbsorpFactors

SUBROUTINE ComputeDifSolExcZonesWIZWindows(NumberOfZones)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       Jun 2007 - Lawrie - Speed enhancements.
          !       RE-ENGINEERED  Winkelmann, Lawrie

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the diffuse solar exchange factors between zones with
          ! interzone windows.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NumberOfZones   ! Number of zones

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64), ALLOCATABLE, DIMENSION(:,:), SAVE :: D
      INTEGER SurfNum,IZ,JZ,KZ,LZ,MZ,NZ

      IF (.not. ALLOCATED(FractDifShortZtoZ)) THEN
        ALLOCATE(FractDifShortZtoZ(NumberOfZones,NumberOfZones))
        ALLOCATE(RecDifShortFromZ(NumberOfZones))
        ALLOCATE(D(NumberOfZones,NumberOfZones))
      ENDIF

      RecDifShortFromZ=.FALSE.
      FractDifShortZtoZ = 0.0d0
      D = 0.0d0
      DO NZ=1,NumberOfZones
        D(NZ,NZ)=1.0d0
      ENDDO

!      IF (.not. ANY(Zone%HasInterZoneWindow)) RETURN  ! this caused massive diffs
      IF (KickOffSimulation .or. KickOffSizing) RETURN
!            Compute fraction transmitted in one pass.

      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond  ==  SurfNum) CYCLE
        IF (Construct(Surface(SurfNum)%Construction)%TransDiff <= 0.0d0) CYCLE

        NZ=Surface(SurfNum)%Zone
        if (.not. Zone(NZ)%HasInterZoneWindow) CYCLE
        MZ=Surface(Surface(SurfNum)%ExtBoundCond)%Zone
        FractDifShortZtoZ(MZ,NZ)=FractDifShortZtoZ(MZ,NZ)+ &
                       Construct(Surface(SurfNum)%Construction)%TransDiff*VMULT(NZ)*Surface(SurfNum)%Area
        IF (VMULT(NZ) /= 0.0d0) RecDifShortFromZ(NZ)=.TRUE.
      END DO
 !          Compute fractions for multiple passes.

      DO NZ=1,NumberOfZones
        DO MZ=1,NumberOfZones
          IF (MZ == NZ) CYCLE
          D(MZ,NZ)=FractDifShortZtoZ(MZ,NZ)/(1.0d0-FractDifShortZtoZ(MZ,NZ)*FractDifShortZtoZ(NZ,MZ))
          D(NZ,NZ)=D(NZ,NZ)+FractDifShortZtoZ(NZ,MZ)*D(MZ,NZ)
        ENDDO
      ENDDO

      FractDifShortZtoZ=D
      ! added for CR 7999 & 7869
      DO NZ = 1, NumberOfZones
        DO MZ = 1, NumberOfZones
          IF (MZ == NZ) CYCLE
          IF (FractDifShortZtoZ(MZ,NZ) > 0.0d0) THEN
            RecDifShortFromZ(NZ) = .TRUE.
            EXIT
          ENDIF
        ENDDO
      ENDDO

!           Compute fractions for multiple zones.

      DO IZ=1,NumberOfZones
        IF(.NOT. RecDifShortFromZ(IZ)) CYCLE

        DO JZ=1,NumberOfZones
          IF(.NOT. RecDifShortFromZ(JZ)) CYCLE
          IF(IZ == JZ) CYCLE
          IF(D(JZ,IZ) == 0.0d0) CYCLE

          DO KZ=1,NumberOfZones
            IF(.NOT. RecDifShortFromZ(KZ)) CYCLE
            IF(IZ == KZ) CYCLE
            IF(JZ == KZ) CYCLE
            IF(D(KZ,JZ) == 0.0d0) CYCLE
            FractDifShortZtoZ(KZ,IZ)=FractDifShortZtoZ(KZ,IZ)+D(KZ,JZ)*D(JZ,IZ)

            DO LZ=1,NumberOfZones
              IF(.NOT. RecDifShortFromZ(LZ)) CYCLE
              IF(IZ == LZ) CYCLE
              IF(JZ == LZ) CYCLE
              IF(KZ == LZ) CYCLE
              IF(D(LZ,KZ) == 0.0d0) CYCLE
              FractDifShortZtoZ(LZ,IZ)=FractDifShortZtoZ(LZ,IZ)+D(LZ,KZ)*D(KZ,JZ)*D(JZ,IZ)

              DO MZ=1,NumberOfZones
                IF(.NOT. RecDifShortFromZ(MZ)) CYCLE
                IF(IZ == MZ) CYCLE
                IF(JZ == MZ) CYCLE
                IF(KZ == MZ) CYCLE
                IF(LZ == MZ) CYCLE
                IF(D(MZ,LZ) == 0.0d0) CYCLE
                FractDifShortZtoZ(MZ,IZ)=FractDifShortZtoZ(MZ,IZ)+D(MZ,LZ)*D(LZ,KZ)*D(KZ,JZ)*D(JZ,IZ)
              ENDDO  ! MZ Loop

            ENDDO  ! LZ Loop

          ENDDO  ! KZ Loop

        ENDDO  ! JZ Loop

      ENDDO  ! IZ Loop

      RETURN

END SUBROUTINE ComputeDifSolExcZonesWIZWindows

SUBROUTINE InitEMSControlledSurfaceProperties

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! initialize material and construction surface properties if being overriden by EMS

          ! METHODOLOGY EMPLOYED:
          ! update solar, thermal and visible absorptance values when actuated by EMS

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
  LOGICAL, SAVE  :: SurfPropOverridesPresent = .FALSE.  ! detect if EMS ever used for this and inits need to execute
  INTEGER  :: MaterNum ! do loop counter over materials
  INTEGER  :: ConstrNum ! do loop counter over constructions
  INTEGER  :: TotLayers ! count of material layers in a construction
  INTEGER  :: InsideMaterNum ! integer pointer for inside face's material layer
  INTEGER  :: OutsideMaterNum ! integer pointer for outside face's material layer


  ! first determine if anything needs to be done, once yes, then always init
  IF (ANY(Material%AbsorpSolarEMSOverrideOn))   SurfPropOverridesPresent = .TRUE.
  IF (ANY(Material%AbsorpThermalEMSOverrideOn)) SurfPropOverridesPresent = .TRUE.
  IF (ANY(Material%AbsorpVisibleEMSOverrideOn)) SurfPropOverridesPresent = .TRUE.

  IF ( .NOT. SurfPropOverridesPresent) RETURN ! quick return if nothing has ever needed to be done

  ! first, loop over materials
  DO MaterNum=1,TotMaterials
    IF (Material(MaterNum)%AbsorpSolarEMSOverrideOn) THEN
      Material(MaterNum)%AbsorpSolar = MAX(MIN(Material(MaterNum)%AbsorpSolarEMSOverride, 0.9999d0), 0.0001d0)
    ELSE
      Material(MaterNum)%AbsorpSolar = Material(MaterNum)%AbsorpSolarInput
    ENDIF
    IF (Material(MaterNum)%AbsorpThermalEMSOverrideOn) THEN
      Material(MaterNum)%AbsorpThermal =  MAX(MIN(Material(MaterNum)%AbsorpThermalEMSOverride, 0.9999d0), 0.0001d0)
    ELSE
      Material(MaterNum)%AbsorpThermal = Material(MaterNum)%AbsorpThermalInput
    ENDIF
    IF (Material(MaterNum)%AbsorpVisibleEMSOverrideOn) THEN
      Material(MaterNum)%AbsorpVisible = MAX(MIN(Material(MaterNum)%AbsorpVisibleEMSOverride, 0.9999d0), 0.0001d0)
    ELSE
      Material(MaterNum)%AbsorpVisible = Material(MaterNum)%AbsorpVisibleInput
    ENDIF

  ENDDO ! loop over materials

  ! second, loop over constructions
  DO ConstrNum = 1, TotConstructs
    IF (Construct(ConstrNum)%TypeIsWindow) CYCLE ! only override opaque constructions
    TotLayers = Construct(ConstrNum)%TotLayers
    IF (TotLayers == 0) CYCLE ! error condition
    InsideMaterNum=Construct(ConstrNum)%LayerPoint(TotLayers)
    IF (InsideMaterNum /= 0) THEN
      Construct(ConstrNum)%InsideAbsorpVis     = Material(InsideMaterNum)%AbsorpVisible
      Construct(ConstrNum)%InsideAbsorpSolar   = Material(InsideMaterNum)%AbsorpSolar
      Construct(ConstrNum)%InsideAbsorpThermal = Material(InsideMaterNum)%AbsorpThermal
    END IF

    OutsideMaterNum=Construct(ConstrNum)%LayerPoint(1)
    IF (OutsideMaterNum /= 0) THEN
      Construct(ConstrNum)%OutsideAbsorpVis     = Material(OutsideMaterNum)%AbsorpVisible
      Construct(ConstrNum)%OutsideAbsorpSolar   = Material(OutsideMaterNum)%AbsorpSolar
      Construct(ConstrNum)%OutsideAbsorpThermal = Material(OutsideMaterNum)%AbsorpThermal
    END IF

  ENDDO

  RETURN

END SUBROUTINE InitEMSControlledSurfaceProperties

SUBROUTINE InitEMSControlledConstructions

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! change construction on surface if overriden by EMS

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE DataRuntimeLanguage, ONLY: EMSConstructActuatorChecked, EMSConstructActuatorIsOkay
  USE HeatBalFiniteDiffManager, ONLY: ConstructFD

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
  LOGICAL, SAVE  :: SurfConstructOverridesPresent = .FALSE.  ! detect if EMS ever used for this and inits need to execute
  INTEGER :: SurfNum

  IF (ANY(Surface%EMSConstructionOverrideON)) SurfConstructOverridesPresent = .TRUE.

  IF (.NOT. SurfConstructOverridesPresent) RETURN

  DO SurfNum = 1, TotSurfaces

    IF (Surface(SurfNum)%EMSConstructionOverrideON .AND. (Surface(SurfNum)%EMSConstructionOverrideValue > 0) ) THEN

      IF( (EMSConstructActuatorChecked(SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue)) .AND. &
          (EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue)) )  THEN

        Surface(SurfNum)%Construction = Surface(SurfNum)%EMSConstructionOverrideValue
        Construct(Surface(SurfNum)%Construction)%IsUsed = .TRUE.

      ELSE ! have not checked yet or is not okay, so see if we need to warn about incompatible
        IF (.NOT. EMSConstructActuatorChecked(SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue)) THEN
          ! check if constructions appear compatible

          IF (Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%TypeIsWindow) THEN ! okay, allways allow windows
            EMSConstructActuatorChecked(SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.
            EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.

          ELSEIF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF .OR. &
                  Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD ) THEN
            ! compare old construction to new construction and see if terms match
            ! set as okay and turn false if find a big problem
            EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.
            EMSConstructActuatorChecked(SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.
            IF (Construct(Surface(SurfNum)%Construction)%NumHistories /= &
                Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%NumHistories) THEN
              !thow warning, but allow
              CALL ShowWarningError('InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, ' // &
                                     'incompatible CTF timescales are being used.')
              CALL ShowContinueError('Construction named = '//TRIM(Construct(Surface(SurfNum)%Construction)%Name)//  &
                          ' has CTF timesteps = '//TRIM(TrimSigDigits(Construct(Surface(SurfNum)%Construction)%NumHistories)) )
              CALL ShowContinueError('While construction named = '// &
                              TRIM(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%Name)//  &
                          ' has CTF timesteps = ' &
                           //TRIM(TrimSigDigits(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%NumHistories)) )
              CALL ShowContinueError('Transient heat transfer modeling may not be valid for surface name = ' &
                           //TRIM(Surface(SurfNum)%Name)//', and the simulation continues' )

            ENDIF
            IF (Construct(Surface(SurfNum)%Construction)%NumCTFTerms /= &
                Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%NumCTFTerms) THEN
              !thow warning, but allow
              CALL ShowWarningError('InitEMSControlledConstructions: EMS Construction State Actuator may be unrealistic, ' // &
                                     'incompatible CTF terms are being used.')
              CALL ShowContinueError('Construction named = '//TRIM(Construct(Surface(SurfNum)%Construction)%Name)//  &
                          ' has number of CTF terms = '//TRIM(TrimSigDigits(Construct(Surface(SurfNum)%Construction)%NumCTFTerms)) )
              CALL ShowContinueError('While construction named = '// &
                              TRIM(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%Name)//  &
                          ' has number of CTF terms = ' &
                           //TRIM(TrimSigDigits(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%NumCTFTerms)) )
              CALL ShowContinueError('The actuator is allowed but the transient heat transfer modeling may not be valid for' &
                           //' surface name = ' //TRIM(Surface(SurfNum)%Name)//', and the simulation continues')

            ENDIF

            IF (Construct(Surface(SurfNum)%Construction)%SourceSinkPresent) THEN
              IF (.NOT. Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%SourceSinkPresent) THEN
                !thow warning, and do not allow
                CALL ShowSevereError('InitEMSControlledConstructions: EMS Construction State Actuator not valid.')
                CALL ShowContinueError('Construction named = '//TRIM(Construct(Surface(SurfNum)%Construction)%Name)//  &
                            ' has internal source/sink' )
                CALL ShowContinueError('While construction named = '// &
                                TRIM(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%Name)//  &
                            ' is not an internal source/sink construction' )
                CALL ShowContinueError('This actuator is not allowed for surface name = ' &
                             //TRIM(Surface(SurfNum)%Name)// ', and the simulation continues without the override' )

                EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .FALSE.
              ENDIF

            ENDIF


            IF (EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue)) THEN
              Surface(SurfNum)%Construction = Surface(SurfNum)%EMSConstructionOverrideValue
            ENDIF

          ELSEIF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD) THEN
            EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.
            EMSConstructActuatorChecked(SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.
            IF (ConstructFD(Surface(SurfNum)%Construction)%TotNodes /=  &
                ConstructFD(Surface(SurfNum)%EMSConstructionOverrideValue)%TotNodes) THEN
              !thow warning, and do not allow
              CALL ShowSevereError('InitEMSControlledConstructions: EMS Construction State Actuator not valid.')
              CALL ShowContinueError('Construction named = '//TRIM(Construct(Surface(SurfNum)%Construction)%Name)//  &
                          ' has number of finite difference nodes =' &
                           //TRIM(TrimSigDigits(ConstructFD(Surface(SurfNum)%Construction)%TotNodes)) )
              CALL ShowContinueError('While construction named = '// &
                              TRIM(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%Name)//  &
                           'has number of finite difference nodes =' &
                           //TRIM(TrimSigDigits(ConstructFD(Surface(SurfNum)%EMSConstructionOverrideValue)%TotNodes)) )
              CALL ShowContinueError('This actuator is not allowed for surface name = ' &
                           //TRIM(Surface(SurfNum)%Name)// ', and the simulation continues without the override' )

              EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .FALSE.
            ENDIF

            IF (Construct(Surface(SurfNum)%Construction)%SourceSinkPresent) THEN
              IF (.NOT. Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%SourceSinkPresent) THEN
                !thow warning, and do not allow
                CALL ShowSevereError('InitEMSControlledConstructions: EMS Construction State Actuator not valid.')
                CALL ShowContinueError('Construction named = '//TRIM(Construct(Surface(SurfNum)%Construction)%Name)//  &
                            ' has internal source/sink' )
                CALL ShowContinueError('While construction named = '// &
                                TRIM(Construct(Surface(SurfNum)%EMSConstructionOverrideValue)%Name)//  &
                            ' is not an internal source/sink construction' )
                CALL ShowContinueError('This actuator is not allowed for surface name = ' &
                             //TRIM(Surface(SurfNum)%Name)// ', and the simulation continues without the override' )

                EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .FALSE.
              ENDIF

            ENDIF

            IF (EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue)) THEN
              Surface(SurfNum)%Construction = Surface(SurfNum)%EMSConstructionOverrideValue
            ENDIF

          ELSEIF(Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_HAMT) THEN ! don't allow
            CALL ShowSevereError('InitEMSControlledConstructions: EMS Construction State Actuator not available with ' &
                                  //'Heat transfer algorithm CombinedHeatAndMoistureFiniteElement.')
            CALL ShowContinueError('This actuator is not allowed for surface name = ' &
                             //TRIM(Surface(SurfNum)%Name)// ', and the simulation continues without the override' )
            EMSConstructActuatorChecked(SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .TRUE.
            EMSConstructActuatorIsOkay( SurfNum, Surface(SurfNum)%EMSConstructionOverrideValue) = .FALSE.


          ENDIF



        ELSE
          ! do nothing, has been checked and is not okay with single warning already issued.
        ENDIF

      ENDIF
    ELSE
      Surface(SurfNum)%Construction = Surface(SurfNum)%ConstructionStoredInputValue
    ENDIF

  ENDDO


  RETURN

END SUBROUTINE InitEMSControlledConstructions

! End Initialization Section of the Module
!******************************************************************************


! Begin Algorithm Section of the Module
!******************************************************************************


! Beginning of Record Keeping subroutines for the HB Module
! *****************************************************************************
SUBROUTINE UpdateFinalSurfaceHeatBalance

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! If a radiant system is present and was on for part of the time step,
          ! then we probably need to make yet another pass through the heat balance.
          ! This is necessary because the heat source/sink to the surface that is
          ! the radiant system may have varied during the system time steps.

          ! METHODOLOGY EMPLOYED:
          ! First, determine whether or not the radiant system was running.  If
          ! any of the Qsource terms are non-zero, then it was running.  Then,
          ! update the current source terms with the "average" value calculated
          ! by the radiant system algorithm.  This requires the "USE" of the
          ! radiant algorithm module.  Finally, using this source value, redo
          ! the inside and outside heat balances.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE LowTempRadiantSystem,   ONLY : UpdateRadSysSourceValAvg
  USE HighTempRadiantSystem,  ONLY : UpdateHTRadSourceValAvg
  USE HWBaseboardRadiator,    ONLY : UpdateBBRadSourceValAvg
  USE SteamBaseboardRadiator, ONLY : UpdateBBSteamRadSourceValAvg
  USE ElectricBaseboardRadiator, ONLY : UpdateBBElecRadSourceValAvg


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: LowTempRadSysOn      ! .TRUE. if a low temperature radiant system is running
  LOGICAL :: HighTempRadSysOn     ! .TRUE. if a high temperature radiant system is running
  LOGICAL :: HWBaseboardSysOn     ! .TRUE. if a water baseboard heater is running
  LOGICAL :: SteamBaseboardSysOn  ! .TRUE. if a steam baseboard heater is running
  LOGICAL :: ElecBaseboardSysOn   ! .TRUE. if a steam baseboard heater is running

          ! FLOW:
  CALL UpdateRadSysSourceValAvg(LowTempRadSysOn)
  CALL UpdateHTRadSourceValAvg(HighTempRadSysOn)
  CALL UpdateBBRadSourceValAvg(HWBaseboardSysOn)
  CALL UpdateBBSteamRadSourceValAvg(SteamBaseboardSysOn)
  CALL UpdateBBElecRadSourceValAvg(ElecBaseboardSysOn)

  IF (LowTempRadSysOn.OR.HighTempRadSysOn.OR.HWBaseboardSysOn.OR.SteamBaseboardSysOn.OR. &
        ElecBaseboardSysOn) THEN
          ! Solve the zone heat balance 'Detailed' solution
          ! Call the outside and inside surface heat balances
    CALL CalcHeatBalanceOutsideSurf
    CALL CalcHeatBalanceInsideSurf
  END IF

  RETURN

END SUBROUTINE UpdateFinalSurfaceHeatBalance

SUBROUTINE UpdateThermalHistories

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1990
          !       MODIFIED       na
          !       RE-ENGINEERED  Mar98 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates and shifts the thermal and flux histories.

          ! METHODOLOGY EMPLOYED:
          ! If a surface runs on the user selected subhourly time step, then the
          ! history terms for the temperatures and fluxes must simply be updated
          ! and shifted.  However, if the surface runs at a different (longer) time
          ! step, then the "master" history series is used for the interpolated
          ! update scheme.

          ! REFERENCES:
          ! (I)BLAST legacy routine UTHRMH
          ! Taylor et.al., Impact of Simultaneous Simulation of Buildings and
          ! Mechanical Systems in Heat Balance Based Energy Analysis Programs
          ! on System Response and Control, Building Simulation '91, IBPSA, Nice, France.

          ! USE STATEMENTS:
          ! na
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: ConstrNum       ! Construction index for the current surface
  INTEGER          :: HistTermNum     ! DO loop counter for history terms
  INTEGER          :: SideNum         ! DO loop counter for surfaces sides (inside, outside)
  INTEGER          :: SurfNum         ! Surface number DO loop counter
  INTEGER          :: ZoneNum         ! Zone number DO loop counter

  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: QExt1    ! Heat flux at the exterior surface during first time step/series
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: QInt1    ! Heat flux at the interior surface during first time step/series
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: TempInt1 ! Temperature of interior surface during first time step/series
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: TempExt1 ! Temperature of exterior surface during first time step/series
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: Qsrc1    ! Heat source/sink (during first time step/series)
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: Tsrc1    ! Temperature at source/sink (during first time step/series)
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: SumTime  ! Amount of time that has elapsed from start of master history to
                                                                ! the current time step

  LOGICAL, SAVE :: FirstTimeFlag=.true.
          ! FLOW:
  IF (FirstTimeFlag) THEN
    ALLOCATE(QExt1(TotSurfaces))
    QExt1 = 0.0D0
    ALLOCATE(QInt1(TotSurfaces))
    QInt1 = 0.0D0
    ALLOCATE(TempInt1(TotSurfaces))
    TempInt1 = 0.0D0
    ALLOCATE(TempExt1(TotSurfaces))
    TempExt1 = 0.0D0
    ALLOCATE(SumTime(TotSurfaces))
    SumTime = 0.0D0
    ALLOCATE(Qsrc1(TotSurfaces))
    Qsrc1 = 0.0D0
    ALLOCATE(Tsrc1(TotSurfaces))
    Tsrc1 = 0.0D0
    FirstTimeFlag=.false.
  END IF

  DO SurfNum = 1, TotSurfaces   ! Loop through all (heat transfer) surfaces...

    IF (Surface(SurfNum)%Class == SurfaceClass_Window .or. .NOT.Surface(SurfNum)%HeatTransSurf) CYCLE

    IF ((Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_CTF) .AND.  &
        (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_EMPD) ) CYCLE

    ConstrNum = Surface(SurfNum)%Construction

    IF (Construct(ConstrNum)%NumCTFTerms == 0) CYCLE    ! Skip surfaces with no history terms

          ! Sign convention for the various terms in the following two equations
          ! is based on the form of the Conduction Transfer Function equation
          ! given by:
          ! Qin,now  = (Sum of)(Y Tout) - (Sum of)(Z Tin) + (Sum of)(F Qin,old) + (Sum of)(V Qsrc)
          ! Qout,now = (Sum of)(X Tout) - (Sum of)(Y Tin) + (Sum of)(F Qout,old) + (Sum of)(W Qsrc)
          ! In both equations, flux is positive from outside to inside.  The V and W terms are for radiant systems only.

          ! Set current inside flux:
    QH(SurfNum,1,2) = TH(SurfNum,1,1)*Construct(ConstrNum)%CTFCross(0)        &
                     -TempSurfIn(SurfNum)*Construct(ConstrNum)%CTFInside(0)   &
                     +QsrcHist(SurfNum,1)*Construct(ConstrNum)%CTFSourceIn(0) & ! Heat source/sink term for radiant systems
                     +CTFConstInPart(SurfNum)
    IF(Surface(SurfNum)%Class==SurfaceClass_Floor .OR. Surface(SurfNum)%Class==SurfaceClass_Wall .OR.  &
       Surface(SurfNum)%Class==SurfaceClass_IntMass .or.  &
       Surface(SurfNum)%Class==SurfaceClass_Roof  .OR. Surface(SurfNum)%Class==SurfaceClass_Door) THEN
      OpaqSurfInsFaceConduction(SurfNum) = Surface(SurfNum)%Area * QH(SurfNum,1,2)
      OpaqSurfInsFaceConductionFlux(SurfNum) = QH(SurfNum,1,2) !CR 8901
!      IF (Surface(SurfNum)%Class/=SurfaceClass_IntMass)  &
!      ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) = ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) + &
!              OpaqSurfInsFaceConduction(SurfNum)
      OpaqSurfInsFaceCondGainRep(SurfNum) = 0.0d0
      OpaqSurfInsFaceCondLossRep(SurfNum) = 0.0d0
      IF(OpaqSurfInsFaceConduction(SurfNum) >= 0.0d0) THEN
        OpaqSurfInsFaceCondGainRep(SurfNum) = OpaqSurfInsFaceConduction(SurfNum)
      ELSE
        OpaqSurfInsFaceCondLossRep(SurfNum) = -OpaqSurfInsFaceConduction(SurfNum)
      END IF
    END IF

          ! Update the temperature at the source/sink location (if one is present)
    IF (Construct(ConstrNum)%SourceSinkPresent) THEN
      TsrcHist(SurfNum,1) = TH(SurfNum,1,1)*Construct(ConstrNum)%CTFTSourceOut(0)    &
                           +TempSurfIn(SurfNum)*Construct(ConstrNum)%CTFTSourceIn(0) &
                           +QsrcHist(SurfNum,1)*Construct(ConstrNum)%CTFTSourceQ(0)  &
                           +CTFTsrcConstPart(SurfNum)
      TempSource(SurfNum) = TsrcHist(SurfNum,1)
    END IF

    IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE   ! Don't need to evaluate outside for partitions

          ! Set current outside flux:
    QH(SurfNum,1,1) = TH(SurfNum,1,1)*Construct(ConstrNum)%CTFOutside(0)       &
                     -TempSurfIn(SurfNum)*Construct(ConstrNum)%CTFCross(0)     &
                     +QsrcHist(SurfNum,1)*Construct(ConstrNum)%CTFSourceOut(0) & ! Heat source/sink term for radiant systems
                     +CTFConstOutPart(SurfNum)

    IF(Surface(SurfNum)%Class==SurfaceClass_Floor .OR. Surface(SurfNum)%Class==SurfaceClass_Wall .OR.  &
       Surface(SurfNum)%Class==SurfaceClass_IntMass .or.  &
       Surface(SurfNum)%Class==SurfaceClass_Roof  .OR. Surface(SurfNum)%Class==SurfaceClass_Door) THEN
      OpaqSurfOutsideFaceConductionFlux(SurfNum) = - QH(SurfNum,1,1) ! switch sign for balance at outside face
      OpaqSurfOutsideFaceConduction(SurfNum)     = Surface(SurfNum)%Area * OpaqSurfOutsideFaceConductionFlux(SurfNum)


    END IF

  END DO    ! ...end of loop over all (heat transfer) surfaces...

  DO SurfNum = 1, TotSurfaces   ! Loop through all (heat transfer) surfaces...

    IF (Surface(SurfNum)%Class == SurfaceClass_Window .or. .NOT.Surface(SurfNum)%HeatTransSurf) CYCLE
    IF ((Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_CTF) .AND.  &
        (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_EMPD) .AND. &
        (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_TDD)) CYCLE
    IF (SUMH(SurfNum) == 0) THEN    ! First time step in a block for a surface, update arrays
      TempExt1(SurfNum) = TH(SurfNum,1,1)
      TempInt1(SurfNum) = TempSurfIn(SurfNum)
      Tsrc1(SurfNum)    = TsrcHist(SurfNum,1)
      QExt1(SurfNum)    = QH(SurfNum,1,1)
      QInt1(SurfNum)    = QH(SurfNum,1,2)
      Qsrc1(SurfNum)    = QsrcHist(SurfNum,1)
    END IF

  END DO    ! ...end of loop over all (heat transfer) surfaces...

          ! SHIFT TEMPERATURE AND FLUX HISTORIES:
          ! SHIFT AIR TEMP AND FLUX SHIFT VALUES WHEN AT BOTTOM OF ARRAY SPACE.
  DO SurfNum = 1, TotSurfaces   ! Loop through all (heat transfer) surfaces...

    IF (Surface(SurfNum)%Class == SurfaceClass_Window .or. Surface(SurfNum)%Class == SurfaceClass_TDD_Dome &
      .or. .NOT.Surface(SurfNum)%HeatTransSurf) CYCLE
    IF ((Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_CTF) .AND.  &
        (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_EMPD) .AND. &
        (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_TDD) ) CYCLE

    ConstrNum       = Surface(SurfNum)%Construction

    SUMH(SurfNum)    = SUMH(SurfNum)+1
    SumTime(SurfNum) = REAL(SUMH(SurfNum),r64)*TimeStepZone

    IF (SUMH(SurfNum) == Construct(ConstrNum)%NumHistories) THEN

      SUMH(SurfNum) = 0

      IF (Construct(ConstrNum)%NumCTFTerms > 1) THEN
        DO HistTermNum = Construct(ConstrNum)%NumCTFTerms+1, 3, -1
          DO SideNum = 1, 2
            THM(SurfNum,HistTermNum,SideNum) = THM(SurfNum,HistTermNum-1,SideNum)
            QHM(SurfNum,HistTermNum,SideNum) = QHM(SurfNum,HistTermNum-1,SideNum)
            TH(SurfNum,HistTermNum,SideNum)  = THM(SurfNum,HistTermNum,SideNum)
            QH(SurfNum,HistTermNum,SideNum)  = QHM(SurfNum,HistTermNum,SideNum)
          END DO
          TsrcHistM(SurfNum,HistTermNum) = TsrcHistM(SurfNum,HistTermNum-1)
          TsrcHist(SurfNum,HistTermNum)  = TsrcHistM(SurfNum,HistTermNum)
          QsrcHistM(SurfNum,HistTermNum) = QsrcHistM(SurfNum,HistTermNum-1)
          QsrcHist(SurfNum,HistTermNum)  = QsrcHistM(SurfNum,HistTermNum)
        END DO
      END IF

      THM(SurfNum,2,1)     = TempExt1(SurfNum)
      THM(SurfNum,2,2)     = TempInt1(SurfNum)
      TsrcHistM(SurfNum,2) = Tsrc1(SurfNum)
      QHM(SurfNum,2,1)     = QExt1(SurfNum)
      QHM(SurfNum,2,2)     = QInt1(SurfNum)
      QsrcHistM(SurfNum,2) = Qsrc1(SurfNum)

      TH(SurfNum,2,1)     = THM(SurfNum,2,1)
      TH(SurfNum,2,2)     = THM(SurfNum,2,2)
      TsrcHist(SurfNum,2) = TsrcHistM(SurfNum,2)
      QH(SurfNum,2,1)     = QHM(SurfNum,2,1)
      QH(SurfNum,2,2)     = QHM(SurfNum,2,2)
      QsrcHist(SurfNum,2) = QsrcHistM(SurfNum,2)

    ELSE

      IF (Construct(ConstrNum)%NumCTFTerms > 1) THEN
        DO HistTermNum = Construct(ConstrNum)%NumCTFTerms+1, 3, -1
          DO SideNum = 1, 2
            TH(SurfNum,HistTermNum,SideNum) = THM(SurfNum,HistTermNum,SideNum)       &
                                             -( THM(SurfNum,HistTermNum,SideNum)     &
                                               -THM(SurfNum,HistTermNum-1,SideNum) ) &
                                              *SumTime(SurfNum)                      &
                                              /Construct(ConstrNum)%CTFTimeStep

            QH(SurfNum,HistTermNum,SideNum) = QHM(SurfNum,HistTermNum,SideNum)       &
                                             -( QHM(SurfNum,HistTermNum,SideNum)     &
                                               -QHM(SurfNum,HistTermNum-1,SideNum) ) &
                                              *SumTime(SurfNum)                      &
                                              /Construct(ConstrNum)%CTFTimeStep
          END DO
          TsrcHist(SurfNum,HistTermNum) = TsrcHistM(SurfNum,HistTermNum)       &
                                         -( TsrcHistM(SurfNum,HistTermNum)     &
                                           -TsrcHistM(SurfNum,HistTermNum-1) ) &
                                          *SumTime(SurfNum)                    &
                                          /Construct(ConstrNum)%CTFTimeStep

          QsrcHist(SurfNum,HistTermNum) = QsrcHistM(SurfNum,HistTermNum)       &
                                         -( QsrcHistM(SurfNum,HistTermNum)     &
                                           -QsrcHistM(SurfNum,HistTermNum-1) ) &
                                          *SumTime(SurfNum)                    &
                                          /Construct(ConstrNum)%CTFTimeStep
        END DO
      ENDIF

      TH(SurfNum,2,1) = THM(SurfNum,2,1)                             &
                               -(THM(SurfNum,2,1)-TempExt1(SurfNum)) &
                                *SumTime(SurfNum)/Construct(ConstrNum)%CTFTimeStep
      TH(SurfNum,2,2) = THM(SurfNum,2,2)                             &
                               -(THM(SurfNum,2,2)-TempInt1(SurfNum)) &
                                *SumTime(SurfNum)/Construct(ConstrNum)%CTFTimeStep
      QH(SurfNum,2,1) = QHM(SurfNum,2,1)                          &
                               -(QHM(SurfNum,2,1)-QExt1(SurfNum)) &
                                *SumTime(SurfNum)/Construct(ConstrNum)%CTFTimeStep
      QH(SurfNum,2,2) = QHM(SurfNum,2,2)                          &
                               -(QHM(SurfNum,2,2)-QInt1(SurfNum)) &
                                *SumTime(SurfNum)/Construct(ConstrNum)%CTFTimeStep

      TsrcHist(SurfNum,2) = TsrcHistM(SurfNum,2)                      &
                               -(TsrcHistM(SurfNum,2)-Tsrc1(SurfNum)) &
                                *SumTime(SurfNum)/Construct(ConstrNum)%CTFTimeStep
      QsrcHist(SurfNum,2) = QsrcHistM(SurfNum,2)                      &
                               -(QsrcHistM(SurfNum,2)-Qsrc1(SurfNum)) &
                                *SumTime(SurfNum)/Construct(ConstrNum)%CTFTimeStep

    END IF

  END DO    ! ...end of loop over all (heat transfer) surfaces



  RETURN

END SUBROUTINE UpdateThermalHistories


SUBROUTINE CalculateZoneMRT(ZoneToResimulate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the current zone MRT for thermal comfort and radiation
          ! calculation purposes.

          ! METHODOLOGY EMPLOYED:
          ! If you have to ask...

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN), OPTIONAL :: ZoneToResimulate  ! if passed in, then only calculate surfaces that have this zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                         :: FirstTime = .TRUE.   ! Flag for first time calculations
  REAL(r64)                             :: SumAET               ! Intermediate calculational variable (area*emissivity*T) sum
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: SurfaceAE            ! Product of area and emissivity for each surface
  INTEGER                               :: SurfNum              ! Surface number
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: ZoneAESum            ! Sum of area times emissivity for all zone surfaces
  INTEGER                               :: ZoneNum              ! Zone number

          ! FLOW:
  IF (FirstTime) THEN
    ALLOCATE(SurfaceAE(TotSurfaces))
    ALLOCATE(ZoneAESum(NumOfZones))
    SurfaceAE = 0.0d0
    ZoneAESum = 0.0d0
    DO SurfNum = 1, TotSurfaces
      IF (Surface(SurfNum)%HeatTransSurf) THEN
        SurfaceAE(SurfNum) = Surface(SurfNum)%Area*Construct(Surface(SurfNum)%Construction)%InsideAbsorpThermal
        ZoneNum = Surface(SurfNum)%Zone
        IF (ZoneNum > 0) ZoneAESum(ZoneNum) = ZoneAESum(ZoneNum) + SurfaceAE(SurfNum)
      END IF
    END DO
  END IF

  DO ZoneNum = 1, NumOfZones
    IF ( PRESENT(ZoneToResimulate)  .AND. (ZoneNum /= ZoneToResimulate)) CYCLE
    SumAET = 0.0d0
    DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
      IF (Surface(SurfNum)%HeatTransSurf) THEN
        SumAET = SumAET + SurfaceAE(SurfNum)*TempSurfIn(SurfNum)
      END IF
    END DO
    IF (ZoneAESum(ZoneNum) > 0.01d0) THEN
      MRT(ZoneNum) = SumAET/ZoneAESum(ZoneNum)
    ELSE
      IF (FirstTime) THEN
        CALL ShowWarningError('Zone areas*inside surface emissivities are summing to zero, for Zone="'//  &
                               TRIM(Zone(ZoneNum)%Name)//'"')
        CALL ShowContinueError('As a result, MRT will be set to MAT for that zone')
      END IF
      MRT(ZoneNum) = MAT(ZoneNum)
    END IF
  END DO

  FirstTime = .FALSE.

  RETURN

END SUBROUTINE CalculateZoneMRT

! End of Record Keeping subroutines for the HB Module
! *****************************************************************************


! Beginning of Reporting subroutines for the HB Module
! *****************************************************************************

SUBROUTINE ReportSurfaceHeatBalance

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts the reporting part of the HBSurface Module in one area.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE SolarShading, ONLY: ReportSurfaceShading
  USE OutputReportTabular, ONLY: lightSWRadSeq,feneSolarRadSeq
  USE DataGlobals, ONLY: NumOfTimeStepInHour, CompLoadReportIsReq, isPulseZoneSizing
  USE DataSizing, ONLY: CurOverallSimDay

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum
  INTEGER :: ZoneNum
  INTEGER :: TimeStepInDay = 0

  ZoneMRT(1:NumOfZones) = MRT(1:NumOfZones)

  CALL ReportSurfaceShading

  ! update inside face radiation reports
  DO SurfNum = 1, TotSurfaces
    QdotRadNetSurfInRep(SurfNum)        = NetLWRadToSurf(SurfNum) * Surface(SurfNum)%Area
    QdotRadNetSurfInRepPerArea(SurfNum) = NetLWRadToSurf(SurfNum)
    QRadNetSurfInReport(SurfNum)        = QdotRadNetSurfInRep(SurfNum) * SecInHour * TimeStepZone

    IF(Surface(SurfNum)%Class /= SurfaceClass_Window) THEN ! not a window...
      QdotRadSolarInRepPerArea(SurfNum)   = QRadSWInAbs(SurfNum) - QRadSWLightsInAbs(SurfNum)
      QdotRadSolarInRep(SurfNum)          = QdotRadSolarInRepPerArea(SurfNum)  * Surface(SurfNum)%Area
      QRadSolarInReport(SurfNum)          = QdotRadSolarInRep(SurfNum)  * SecInHour * TimeStepZone

      QdotRadLightsInRepPerArea(SurfNum)  = QRadSWLightsInAbs(SurfNum)
      QdotRadLightsInRep(SurfNum)         = QdotRadLightsInRepPerArea(SurfNum) * Surface(SurfNum)%Area
      QRadLightsInReport(SurfNum)         = QdotRadLightsInRep(SurfNum) * SecInHour * TimeStepZone

      IF (ZoneSizingCalc .AND. CompLoadReportIsReq) THEN
       TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
       lightSWRadSeq(SurfNum,TimeStepInDay,CurOverallSimDay) = QdotRadLightsInRep(SurfNum)
       feneSolarRadSeq(SurfNum,TimeStepInDay,CurOverallSimDay) = QdotRadSolarInRep(SurfNum)
      END IF
    ELSE ! can we fill these for windows?


    ENDIF


    QdotRadIntGainsInRepPerArea(SurfNum)= QRadThermInAbs(SurfNum)
    QdotRadIntGainsInRep(SurfNum)       = QdotRadIntGainsInRepPerArea(SurfNum) * Surface(SurfNum)%Area
    QRadIntGainsInReport(SurfNum)       = QdotRadIntGainsInRep(SurfNum) * SecInHour * TimeStepZone

    QdotRadHVACInRepPerArea(SurfNum)    = QHTRadSysSurf(SurfNum) + QHWBaseboardSurf(SurfNum) &
                                          + QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)
    QdotRadHVACInRep(SurfNum)           = QdotRadHVACInRepPerArea(SurfNum)  * Surface(SurfNum)%Area
    QRadHVACInReport(SurfNum)           = QdotRadHVACInRep(SurfNum)  * SecInHour * TimeStepZone

    IF(Surface(SurfNum)%Class==SurfaceClass_Floor .OR. Surface(SurfNum)%Class==SurfaceClass_Wall .OR.  &
      Surface(SurfNum)%Class==SurfaceClass_IntMass .or.  &
      Surface(SurfNum)%Class==SurfaceClass_Roof  .OR. Surface(SurfNum)%Class==SurfaceClass_Door) THEN

      ! inside face conduction updates
      OpaqSurfInsFaceConductionEnergy(SurfNum) = OpaqSurfInsFaceConduction(SurfNum) * SecInHour * TimeStepZone
      ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) = ZoneOpaqSurfInsFaceCond(Surface(SurfNum)%Zone) + &
              OpaqSurfInsFaceConduction(SurfNum)
      OpaqSurfInsFaceCondGainRep(SurfNum) = 0.0d0
      OpaqSurfInsFaceCondLossRep(SurfNum) = 0.0d0
      IF(OpaqSurfInsFaceConduction(SurfNum) >= 0.0d0) THEN
        OpaqSurfInsFaceCondGainRep(SurfNum) = OpaqSurfInsFaceConduction(SurfNum)
      ELSE
        OpaqSurfInsFaceCondLossRep(SurfNum) = -OpaqSurfInsFaceConduction(SurfNum)
      END IF

      ! outside face conduction updates
      OpaqSurfOutsideFaceConductionEnergy(SurfNum)   = OpaqSurfOutsideFaceConduction(SurfNum) * SecInHour * TimeStepZone
      ZoneOpaqSurfExtFaceCond(Surface(SurfNum)%Zone) = ZoneOpaqSurfExtFaceCond(Surface(SurfNum)%Zone) + &
              OpaqSurfOutsideFaceConduction(SurfNum)
      OpaqSurfExtFaceCondGainRep(SurfNum) = 0.d0
      OpaqSurfExtFaceCondLossRep(SurfNum) = 0.d0
      IF(OpaqSurfOutsideFaceConduction(SurfNum) >= 0.d0) THEN
        OpaqSurfExtFaceCondGainRep(SurfNum) = OpaqSurfOutsideFaceConduction(SurfNum)
      ELSE
        OpaqSurfExtFaceCondLossRep(SurfNum) = -OpaqSurfOutsideFaceConduction(SurfNum)
      END IF

      ! do average surface conduction updates

      OpaqSurfAvgFaceConduction(SurfNum) = (OpaqSurfInsFaceConduction(SurfNum) - OpaqSurfOutsideFaceConduction(SurfNum)) / 2.d0
      OpaqSurfAvgFaceConductionFlux(SurfNum) = (OpaqSurfInsFaceConductionFlux(SurfNum) &
                                                - OpaqSurfOutsideFaceConductionFlux(SurfNum)) / 2.d0
      OpaqSurfAvgFaceConductionEnergy(SurfNum) = OpaqSurfAvgFaceConduction(SurfNum) * SecInHour * TimeStepZone
      OpaqSurfAvgFaceCondGainRep(SurfNum) = 0.d0
      OpaqSurfAvgFaceCondLossRep(SurfNum) = 0.d0
      IF (OpaqSurfAvgFaceConduction(SurfNum) >= 0.d0) THEN
        OpaqSurfAvgFaceCondGainRep(SurfNum) = OpaqSurfAvgFaceConduction(SurfNum)
      ELSE
        OpaqSurfAvgFaceCondLossRep(SurfNum) = - OpaqSurfAvgFaceConduction(SurfNum)
      ENDIF

      ! do surface storage rate updates
      OpaqSurfStorageConductionFlux(SurfNum) =   &
         - (OpaqSurfInsFaceConductionFlux(SurfNum) + OpaqSurfOutsideFaceConductionFlux(SurfNum))
      OpaqSurfStorageConduction(SurfNum) = - (OpaqSurfInsFaceConduction(SurfNum) + OpaqSurfOutsideFaceConduction(SurfNum))
      OpaqSurfStorageConductionEnergy(SurfNum) = OpaqSurfStorageConduction(SurfNum) * SecInHour * TimeStepZone
      OpaqSurfStorageGainRep(SurfNum) = 0.d0
      OpaqSurfStorageCondLossRep(SurfNum) = 0.d0
      IF (OpaqSurfStorageConduction(SurfNum) >= 0.d0 ) THEN
        OpaqSurfStorageGainRep(SurfNum) = OpaqSurfStorageConduction(SurfNum)
      ELSE
        OpaqSurfStorageCondLossRep(SurfNum) = - OpaqSurfStorageConduction(SurfNum)
      ENDIF

    ENDIF ! opaque heat transfer surfaces.

  ENDDO ! loop over surfaces


  DO ZoneNum = 1,NumOfZones
    IF(ZoneOpaqSurfInsFaceCond(ZoneNum) >= 0.0d0) THEN
      ZoneOpaqSurfInsFaceCondGainRep(ZoneNum) = ZoneOpaqSurfInsFaceCond(ZoneNum)
      ZnOpqSurfInsFaceCondGnRepEnrg(ZoneNum)  = ZoneOpaqSurfInsFaceCondGainRep(ZoneNum) * TimeStepZone * SecInHour
    ELSE
      ZoneOpaqSurfInsFaceCondLossRep(ZoneNum) = -ZoneOpaqSurfInsFaceCond(ZoneNum)
      ZnOpqSurfInsFaceCondLsRepEnrg(ZoneNum)  = ZoneOpaqSurfInsFaceCondLossRep(ZoneNum) * TimeStepZone * SecInHour
    END IF

    IF(ZoneOpaqSurfExtFaceCond(ZoneNum) >= 0.0d0) THEN
      ZoneOpaqSurfExtFaceCondGainRep(ZoneNum) = ZoneOpaqSurfExtFaceCond(ZoneNum)
      ZnOpqSurfExtFaceCondGnRepEnrg(ZoneNum)  = ZoneOpaqSurfExtFaceCondGainRep(ZoneNum) * TimeStepZone * SecInHour
    ELSE
      ZoneOpaqSurfExtFaceCondLossRep(ZoneNum) = - ZoneOpaqSurfExtFaceCond(ZoneNum)
      ZnOpqSurfExtFaceCondLsRepEnrg(ZoneNum)  = ZoneOpaqSurfExtFaceCondLossRep(ZoneNum) * TimeStepZone * SecInHour
    ENDIF
  END DO ! loop over zones

  RETURN

END SUBROUTINE ReportSurfaceHeatBalance



! End of Reporting subroutines for the HB Module
! *****************************************************************************


END MODULE HeatBalanceSurfaceManager


! *****************************************************************************
! *****************************************************************************
! *****************************************************************************
! *****************************************************************************

! EXTERNAL SUBROUTINES (heavily related to HeatBalanceSurfaceManager)

SUBROUTINE CalcHeatBalanceOutsideSurf(ZoneToResimulate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   December 1979
          !       MODIFIED       Jun 1990 (RDT for new CTF arrays);
          !                      Aug 2000 (RJL for MTF moisture calculations)
          !                      Sep 2000 (RKS for new radiant exchange algorithm)
          !                      Dec 2000 (RKS for radiant system model addition)
          !                      Apr 2002 (COP removed denominator from OSC calculation
          !                      Jul 2008 (P.Biddulph include calls to HAMT)
          !                      Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
          !                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  Mar 1998 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs a heat balance on the outside face of each
          ! surface in the building.

          ! METHODOLOGY EMPLOYED:
          ! Various boundary conditions are set and additional parameters are set-
          ! up.  Then, the proper heat balance equation is selected based on the
          ! presence of movable insulation, thermal mass of the surface construction,
          ! and convection model being used.

          ! REFERENCES:
          ! (I)BLAST legacy routine HBOUT
          ! 1989 ASHRAE Handbook of Fundamentals (Figure 1 on p. 22.4, convection correlations)

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals
  USE DataEnvironment
  USE DataHeatBalFanSys
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataSurfaces
  USE DataMoistureBalance,          ONLY : TempOutsideAirFD,RhoVaporAirOut,RhoVaporAirIn,HConvExtFD,HMassConvExtFD, &
                                           HConvInFD,HMassConvInFD,RhoVaporSurfIn, &
                                           HSkyFD,HGrndFD,HAirFD
  USE HeatBalanceMovableInsulation, ONLY : EvalOutsideMovableInsulation
  USE ConvectionCoefficients,       ONLY : InitExteriorConvectionCoeff, SetExtConvectionCoeff, SetIntConvectionCoeff
  USE HeatBalanceIntRadExchange,    ONLY : CalcInteriorRadExchange
  USE ScheduleManager,              ONLY : GetCurrentScheduleValue, GetScheduleIndex
  USE Psychrometrics
  USE EcoRoofManager, ONLY : CalcEcoRoof

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN), OPTIONAL :: ZoneToResimulate  ! if passed in, then only calculate surfaces that have this zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: AbsThermSurf        ! Thermal absoptance of the exterior surface
  INTEGER :: ConstrNum           ! Construction index for the current surface
  REAL(r64)    :: HGround             ! "Convection" coefficient from ground to surface
  REAL(r64)    :: HMovInsul           ! "Convection" coefficient of movable insulation
  REAL(r64)    :: HSky                ! "Convection" coefficient from sky to surface
  REAL(r64)    :: HAir                ! "Convection" coefficient from air to surface (radiation)
  REAL(r64)  :: ConstantTempCoef      ! Temperature Coefficient as input or modified using sine wave  COP mod
  INTEGER :: RoughSurf           ! Roughness index of the exterior surface
  INTEGER :: SurfNum             ! Surface number DO loop counter
  REAL(r64)    :: TempExt             ! Exterior temperature boundary condition
  INTEGER :: ZoneNum             ! Zone number the current surface is attached to
  INTEGER :: OPtr
  REAL(r64)    :: RhoVaporSat         ! Local temporary saturated vapor density for checking

          ! FUNCTION DEFINITIONS:
          ! na

          ! FLOW:
  DO SurfNum = 1, TotSurfaces
          ! Need to transfer any source/sink for a surface to the local array.  Note that
          ! the local array is flux (W/m2) while the QRadSysSource is heat transfer (W).
          ! This must be done at this location so that this is always updated correctly.
    IF (Surface(SurfNum)%Area > 0.0d0) &  ! Make sure we don't divide by zero...
      QsrcHist(SurfNum,1) = QRadSysSource(SurfNum)/Surface(SurfNum)%Area

          ! next we add source (actually a sink) from any integrated PV
    IF (Surface(SurfNum)%Area > 0.0d0) & ! Make sure we don't divide by zero...
      QsrcHist(SurfNum,1) = QsrcHist(SurfNum,1) + QPVSysSource(SurfNum)/Surface(SurfNum)%Area
  END DO

  IF (PRESENT(ZoneToResimulate)) THEN
    CALL CalcInteriorRadExchange(TH(:,1,2),0,NetLWRadToSurf, ZoneToResimulate,calledfrom='Outside')
  ELSE
    CALL CalcInteriorRadExchange(TH(:,1,2),0,NetLWRadToSurf,calledfrom='Outside')
  ENDIF

  DO SurfNum = 1, TotSurfaces   ! Loop through all surfaces...

    ZoneNum = Surface(SurfNum)%Zone

    IF (PRESENT(ZoneToResimulate)) THEN
      IF ((ZoneNum /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
        CYCLE ! skip surfaces that are not associated with this zone
      ENDIF
    ENDIF

    IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. ZoneNum == 0) CYCLE  ! Skip non-heat transfer surfaces

    IF(Surface(SurfNum)%Class == SurfaceClass_Window) CYCLE
    ! Interior windows in partitions use "normal" heat balance calculations
    ! For rest, Outside surface temp of windows not needed in Window5 calculation approach.
    ! Window layer temperatures are calculated in CalcHeatBalanceInsideSurf

          ! Initializations for this surface
    ConstrNum       = Surface(SurfNum)%Construction
    HMovInsul       = 0.0d0
    HSky            = 0.0d0
    HGround         = 0.0d0
    HAir            = 0.0d0
    HcExtSurf(SurfNum)   = 0.0d0
    HAirExtSurf(SurfNum) = 0.0d0
    HSkyExtSurf(SurfNum) = 0.0d0
    HGrdExtSurf(SurfNum) = 0.0d0

          ! Calculate the current outside surface temperature TH(SurfNum,1,1) for the
          ! various different boundary conditions
    SELECT CASE(Surface(SurfNum)%ExtBoundCond)

    CASE(Ground)     ! Surface in contact with ground

      TH(SurfNum,1,1) = GroundTemp

        ! Set the only radiant system heat balance coefficient that is non-zero for this case
      IF (Construct(ConstrNum)%SourceSinkPresent) RadSysToHBConstCoef(SurfNum) = TH(SurfNum,1,1)

! start HAMT
      IF (Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_HAMT) THEN
      ! Set variables used in the HAMT moisture balance
        TempOutsideAirFD(SurfNum)= GroundTemp
        RhoVaporAirOut(SurfNum)=PsyRhovFnTdbRh(GroundTemp,1.0d0,'HBSurfMan:Ground:HAMT')
        HConvExtFD(Surfnum)=HighHConvLimit

        HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,GroundTemp,  &
             PsyWFnTdbRhPb(GroundTemp,1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:GroundTemp'))+  &
             RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,GroundTemp))

        HSkyFD(SurfNum) = HSky
        HGrndFD(SurfNum) = HGround
        HAirFD(SurfNum) = HAir
      ENDIF
! end HAMT

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD ) THEN
      ! Set variables used in the FD moisture balance
        TempOutsideAirFD(SurfNum)= GroundTemp
        RhoVaporAirOut(SurfNum)=PsyRhovFnTdbRhLBnd0C(GroundTemp,1.0d0,'HBSurfMan:Ground:CondFD')
        HConvExtFD(Surfnum)=HighHConvLimit
        HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,GroundTemp,  &
             PsyWFnTdbRhPb(GroundTemp,1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:GroundTemp'))+  &
             RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,GroundTemp))
        HSkyFD(SurfNum) = HSky
        HGrndFD(SurfNum) = HGround
        HAirFD(SurfNum) = HAir
      ENDIF

    ! Added for FCfactor grounds
    CASE(GroundFCfactorMethod)     ! Surface in contact with ground

      TH(SurfNum,1,1) = GroundTempFC

      ! Set the only radiant system heat balance coefficient that is non-zero for this case
      IF (Construct(ConstrNum)%SourceSinkPresent) RadSysToHBConstCoef(SurfNum) = TH(SurfNum,1,1)

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
      ! Set variables used in the HAMT moisture balance
        TempOutsideAirFD(SurfNum)= GroundTempFC
        RhoVaporAirOut(SurfNum)=PsyRhovFnTdbRh(GroundTempFC,1.0d0,'HBSurfMan:GroundFC:HAMT')
        HConvExtFD(Surfnum)=HighHConvLimit

        HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,GroundTempFC,  &
             PsyWFnTdbRhPb(GroundTempFC,1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:GroundTempFC'))+  &
             RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,GroundTempFC))

        HSkyFD(SurfNum) = HSky
        HGrndFD(SurfNum) = HGround
        HAirFD(SurfNum) = HAir
      ENDIF

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD ) THEN
      ! Set variables used in the FD moisture balance
        TempOutsideAirFD(SurfNum)= GroundTempFC
        RhoVaporAirOut(SurfNum)=PsyRhovFnTdbRhLBnd0C(GroundTempFC,1.0d0,'HBSurfMan:GroundFC:CondFD')
        HConvExtFD(Surfnum)=HighHConvLimit
        HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,GroundTempFC,  &
             PsyWFnTdbRhPb(GroundTempFC,1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:GroundTempFC'))+  &
             RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,GroundTempFC))
        HSkyFD(SurfNum) = HSky
        HGrndFD(SurfNum) = HGround
        HAirFD(SurfNum) = HAir
      ENDIF


    CASE(OtherSideCoefNoCalcExt)
          ! Use Other Side Coefficients to determine the surface film coefficient and
          ! the exterior boundary condition temperature

      OPtr=Surface(SurfNum)%OSCPtr
          ! Set surface temp from previous timestep
      IF(BeginTimeStepFlag) THEN
        OSC(OPtr)%TOutsideSurfPast = TH(SurfNum,1,1)
      ENDIF

      IF (OSC(OPtr)%ConstTempScheduleIndex /= 0) THEN ! Determine outside temperature from schedule
         OSC(OPtr)%ConstTemp = GetCurrentScheduleValue(OSC(OPtr)%ConstTempScheduleIndex)
      ENDIF

      !  Allow for modification of TemperatureCoefficient with unitary sine wave.
      IF (OSC(OPtr)%SinusoidalConstTempCoef )THEN   ! Sine wave C4
         ConstantTempCoef = sin( 2*PI * CurrentTime/OSC(OPtr)%SinusoidPeriod)
      ELSE
         ConstantTempCoef = OSC(OPtr)%ConstTempCoef
      END IF

      OSC(OPtr)%OSCTempCalc = ( OSC(OPtr)%ZoneAirTempCoef*MAT(ZoneNum)      &
                                 +OSC(OPtr)%ExtDryBulbCoef*Surface(SurfNum)%OutDryBulbTemp &
                                 + ConstantTempCoef                         &
                                  *OSC(OPtr)%ConstTemp                        &
                                 +OSC(OPtr)%GroundTempCoef*GroundTemp &
                                 +OSC(OPtr)%WindSpeedCoef*Surface(SurfNum)%WindSpeed*Surface(SurfNum)%OutDryBulbTemp &
                                 +OSC(OPtr)%TPreviousCoef*OSC(OPtr)%TOutsideSurfPast)

            ! Enforce max/min limits if applicable
      IF(OSC(OPtr)%MinLimitPresent) OSC(OPtr)%OSCTempCalc = MAX(OSC(OPtr)%MinTempLimit,OSC(OPtr)%OSCTempCalc)
      IF(OSC(OPtr)%MaxLimitPresent) OSC(OPtr)%OSCTempCalc = MIN(OSC(OPtr)%MaxTempLimit,OSC(OPtr)%OSCTempCalc)

      TH(SurfNum,1,1) = OSC(OPtr)%OSCTempCalc

          ! Set the only radiant system heat balance coefficient that is non-zero for this case
        IF (Construct(ConstrNum)%SourceSinkPresent) RadSysToHBConstCoef(SurfNum) = TH(SurfNum,1,1)

        IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD .OR. &
            Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT ) THEN
        ! Set variables used in the FD moisture balance and HAMT
          TempOutsideAirFD(SurfNum)= TH(SurfNum,1,1)
          RhoVaporAirOut(SurfNum)=PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum),OutHumRat,OutBaroPress)
          HConvExtFD(SurfNum)=HighHConvLimit
          HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum),  &
               PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0D0,OutBaroPress,  &
                  'CalcHeatBalanceOutsideSurf:OtherSideCoefNoCalcExt'))+  &
               RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
          HSkyFD(SurfNum) = HSky
          HGrndFD(SurfNum) = HGround
          HAirFD(SurfNum) = HAir
        ENDIF

          ! This ends the calculations for this surface and goes on to the next SurfNum

    CASE(OtherSideCoefCalcExt) ! A surface with other side coefficients that define the outside environment

          ! First, set up the outside convection coefficient and the exterior temperature
          ! boundary condition for the surface
      OPtr=Surface(SurfNum)%OSCPtr
          ! Set surface temp from previous timestep
      IF(BeginTimeStepFlag) THEN
        OSC(OPtr)%TOutsideSurfPast = TH(SurfNum,1,1)
      ENDIF

      IF (OSC(OPtr)%ConstTempScheduleIndex /= 0) THEN ! Determine outside temperature from schedule
         OSC(OPtr)%ConstTemp = GetCurrentScheduleValue(OSC(OPtr)%ConstTempScheduleIndex)
      ENDIF

      HcExtSurf(SurfNum) = OSC(OPtr)%SurfFilmCoef

      OSC(OPtr)%OSCTempCalc = ( OSC(OPtr)%ZoneAirTempCoef*MAT(ZoneNum)      &
                    +OSC(OPtr)%ExtDryBulbCoef*Surface(SurfNum)%OutDryBulbTemp &
                    +OSC(OPtr)%ConstTempCoef*OSC(OPtr)%ConstTemp &
                    +OSC(OPtr)%GroundTempCoef*GroundTemp         &
                    +OSC(OPtr)%WindSpeedCoef*Surface(SurfNum)%WindSpeed*Surface(SurfNum)%OutDryBulbTemp &
                    +OSC(OPtr)%TPreviousCoef*OSC(OPtr)%TOutsideSurfPast)

            ! Enforce max/min limits if applicable
      IF(OSC(OPtr)%MinLimitPresent) OSC(OPtr)%OSCTempCalc = MAX(OSC(OPtr)%MinTempLimit,OSC(OPtr)%OSCTempCalc)
      IF(OSC(OPtr)%MaxLimitPresent) OSC(OPtr)%OSCTempCalc = MIN(OSC(OPtr)%MaxTempLimit,OSC(OPtr)%OSCTempCalc)

      TempExt  = OSC(OPtr)%OSCTempCalc

          ! Set the only radiant system heat balance coefficient that is non-zero for this case
      IF (Construct(ConstrNum)%SourceSinkPresent) RadSysToHBConstCoef(SurfNum) = TH(SurfNum,1,1)

      IF ( Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD .OR. &
            Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT ) THEN
      ! Set variables used in the FD moisture balance and HAMT
        TempOutsideAirFD(SurfNum)= TempExt
        RhoVaporAirOut(SurfNum)=PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum),OutHumRat,OutBaroPress)
        HConvExtFD(SurfNum)=HcExtSurf(SurfNum)
        HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum),  &
             PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:OtherSideCoefCalcExt'))+  &
             RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
        HSkyFD(SurfNum) = HSkyExtSurf(SurfNum)
        HGrndFD(SurfNum)= HGrdExtSurf(SurfNum)
        HAirFD(SurfNum) = HAirExtSurf(SurfNum)
      ENDIF

      ! Call the outside surface temp calculation and pass the necessary terms
      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF .OR. &
          Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) &
        CALL CalcOutsideSurfTemp(SurfNum,ZoneNum,ConstrNum,HMovInsul,TempExt)

          ! This ends the calculations for this surface and goes on to the next SurfNum

    CASE(OtherSideCondModeledExt) ! A surface with other side conditions determined from seperate, dynamic component
    !                               modeling that defines the "outside environment"


          ! First, set up the outside convection coefficient and the exterior temperature
          ! boundary condition for the surface
      OPtr=Surface(SurfNum)%OSCMPtr
      ! EMS overrides
      IF (OSCM(OPtr)%EMSOverrideOnTConv) OSCM(OPtr)%TConv = OSCM(OPtr)%EMSOverrideTConvValue
      IF (OSCM(OPtr)%EMSOverrideOnHConv) OSCM(OPtr)%HConv = OSCM(OPtr)%EMSOverrideHConvValue
      IF (OSCM(OPtr)%EMSOverrideOnTRad)  OSCM(OPtr)%TRad  = OSCM(OPtr)%EMSOverrideTRadValue
      IF (OSCM(OPtr)%EMSOverrideOnHrad)  OSCM(OPtr)%Hrad  = OSCM(OPtr)%EMSOverrideHradValue
      HcExtSurf(SurfNum) = OSCM(OPtr)%Hconv

      TempExt  = OSCM(OPtr)%TConv

          ! Set the only radiant system heat balance coefficient that is non-zero for this case
      IF (Construct(ConstrNum)%SourceSinkPresent) RadSysToHBConstCoef(SurfNum) = TH(SurfNum,1,1)

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD .OR. &
          Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT ) THEN
      ! Set variables used in the FD moisture balance and HAMT
        TempOutsideAirFD(SurfNum)= TempExt
        RhoVaporAirOut(SurfNum)=PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum),OutHumRat,OutBaroPress)
        HConvExtFD(SurfNum)=HcExtSurf(SurfNum)
        HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum),  &
             PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:OSCM'))+  &
             RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
        HSkyFD(SurfNum) = OSCM(OPtr)%Hrad !CR 8046, use sky term for surface to baffle IR
        HGrndFD(SurfNum)= 0.0D0 !CR 8046, null out and use only sky term for surface to baffle IR
        HAirFD(SurfNum) = 0.0D0 !CR 8046, null out and use only sky term for surface to baffle IR
      ENDIF

      ! Call the outside surface temp calculation and pass the necessary terms
      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF .OR. &
          Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) THEN

        IF ( Surface(SurfNum)%ExtCavityPresent ) THEN
          CALL CalcExteriorVentedCavity(SurfNum)
        ENDIF

        CALL CalcOutsideSurfTemp(SurfNum,ZoneNum,ConstrNum,HMovInsul,TempExt)
      ELSEIF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD .OR. &
              Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT ) THEN
        IF ( Surface(SurfNum)%ExtCavityPresent ) THEN
          CALL CalcExteriorVentedCavity(SurfNum)
        ENDIF
      END IF

          ! This ends the calculations for this surface and goes on to the next SurfNum
    CASE(ExternalEnvironment)

      !checking the EcoRoof presented in the external environment
      ! recompute each load by calling ecoroof

      IF(Surface(SurfNum)%ExtEcoRoof ) THEN
        CALL CalcEcoRoof(SurfNum,ZoneNum,ConstrNum,TempExt)
        CYCLE
      END IF

      IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNum = Surface(SurfNum)%StormWinConstruction
      RoughSurf    = Material(Construct(ConstrNum)%LayerPoint(1))%Roughness
      AbsThermSurf = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal

      ! Check for outside movable insulation
      IF (Surface(SurfNum)%MaterialMovInsulExt > 0) &
        CALL EvalOutsideMovableInsulation(SurfNum,HMovInsul,RoughSurf,AbsThermSurf)

      ! Check for exposure to wind (exterior environment)
      IF (Surface(SurfNum)%ExtWind) THEN

        ! Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in subroutine)
        CALL InitExteriorConvectionCoeff(SurfNum,HMovInsul,RoughSurf,AbsThermSurf,TH(SurfNum,1,1), &
          HcExtSurf(SurfNum),HSkyExtSurf(SurfNum),HGrdExtSurf(SurfNum),HAirExtSurf(SurfNum))

        IF (IsRain) THEN ! Raining: since wind exposed, outside surface gets wet

          IF (Surface(SurfNum)%ExtConvCoeff <= 0) THEN ! Reset HcExtSurf because of wetness
            HcExtSurf(SurfNum) = 1000.d0
          ELSE  ! User set
            HcExtSurf(SurfNum) = SetExtConvectionCoeff(SurfNum)
          ENDIF

          TempExt = Surface(SurfNum)%OutWetBulbTemp

! start HAMT
          IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
            ! Set variables used in the HAMT moisture balance
            TempOutsideAirFD(SurfNum)= TempExt
            RhoVaporAirOut(SurfNum)=PsyRhovFnTdbRh(TempOutsideAirFD(SurfNum),1.0d0,'HBSurfMan:Rain:HAMT')
            HConvExtFD(SurfNum)=HcExtSurf(SurfNum)
            HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum), &
                PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:extEnvWetSurf'))+  &
                RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
            HSkyFD(SurfNum) = HSkyExtSurf(SurfNum)
            HGrndFD(SurfNum)= HGrdExtSurf(SurfNum)
            HAirFD(SurfNum) = HAirExtSurf(SurfNum)
          ENDIF
! end HAMT

          IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD ) THEN
            ! Set variables used in the FD moisture balance
            TempOutsideAirFD(SurfNum)= TempExt
            RhoVaporAirOut(SurfNum)=PsyRhovFnTdbRhLBnd0C(TempOutsideAirFD(SurfNum),1.0d0,'HBSurfMan:Rain:CondFD')
            HConvExtFD(SurfNum)=HcExtSurf(SurfNum)
            HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum), &
                PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:extEnvWetSurf'))+  &
                RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
            HSkyFD(SurfNum) = HSkyExtSurf(SurfNum)
            HGrndFD(SurfNum)= HGrdExtSurf(SurfNum)
            HAirFD(SurfNum) = HAirExtSurf(SurfNum)
          ENDIF

        ELSE ! Surface is dry, use the normal correlation

          TempExt = Surface(SurfNum)%OutDryBulbTemp

          IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD .OR. &
              Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
            ! Set variables used in the FD moisture balance and HAMT
            TempOutsideAirFD(SurfNum)= TempExt
            RhoVaporAirOut(SurfNum)=PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum),OutHumRat,OutBaroPress)
            HConvExtFD(SurfNum)=HcExtSurf(SurfNum)
            HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum), &
                PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:extEnvDrySurf'))+  &
                RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
             !  check for saturation conditions of air
            RhoVaporSat=PsyRhovFnTdbRh(TempOutsideAirFD(SurfNum),1.0d0,'HBSurfMan:DrySurf:CondFD')
            IF(RhoVaporAirOut(SurfNum) .gt. RhoVaporSat) RhoVaporAirOut(SurfNum)=RhoVaporSat
            HSkyFD(SurfNum) = HSkyExtSurf(SurfNum)
            HGrndFD(SurfNum)= HGrdExtSurf(SurfNum)
            HAirFD(SurfNum) = HAirExtSurf(SurfNum)
          ENDIF

        END IF

      ELSE ! No wind

        ! Calculate exterior heat transfer coefficients for windspeed = 0
        CALL InitExteriorConvectionCoeff(SurfNum,HMovInsul,RoughSurf,AbsThermSurf,TH(SurfNum,1,1), &
          HcExtSurf(SurfNum),HSkyExtSurf(SurfNum),HGrdExtSurf(SurfNum),HAirExtSurf(SurfNum))

        TempExt = Surface(SurfNum)%OutDryBulbTemp

        IF (Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD .OR.      &
            Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
          ! Set variables used in the FD moisture balance and HAMT
          TempOutsideAirFD(SurfNum)= TempExt
          RhoVaporAirOut(SurfNum)=PsyRhovFnTdbWPb(TempOutsideAirFD(SurfNum),OutHumRat,OutBaroPress)
          HConvExtFD(SurfNum)=HcExtSurf(SurfNum)
          HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum), &
              PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:nowind'))+  &
              RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
          HSkyFD(SurfNum) = HSkyExtSurf(SurfNum)
          HGrndFD(SurfNum)= HGrdExtSurf(SurfNum)
          HAirFD(SurfNum) = HAirExtSurf(SurfNum)
        ENDIF

      END IF

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF .OR. &
          Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD)   &
          CALL CalcOutsideSurfTemp(SurfNum,ZoneNum,ConstrNum,HMovInsul,TempExt)

    CASE DEFAULT   ! for interior or other zone surfaces

      IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN  ! Regular partition/internal mass

        TH(SurfNum,1,1) = TempSurfIn(SurfNum)

        ! No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

        IF (Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD .OR.      &
            Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
          ! Set variables used in the FD moisture balance HAMT
          TempOutsideAirFD(SurfNum)= TempSurfIn(SurfNum)
          RhoVaporAirOut(SurfNum)=RhoVaporAirIn(SurfNum)
          HConvExtFD(SurfNum)=HConvIn(SurfNum)
          HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum),  &
              PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:interior/other'))+  &
              RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
          HSkyFD(SurfNum) = 0.0d0
          HGrndFD(SurfNum) = 0.0d0
          HAirFD(SurfNum) = 0.0d0
        ENDIF

      ELSE  ! Interzone partition

        TH(SurfNum,1,1) = TH(Surface(SurfNum)%ExtBoundCond,1,2)

        ! No need to set any radiant system heat balance coefficients here--will be done during inside heat balance

        IF (Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD .OR.      &
            Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
          ! Set variables used in the FD moisture balance and HAMT
          TempOutsideAirFD(SurfNum)= TH(Surface(SurfNum)%ExtBoundCond,1,2)
          RhoVaporAirOut(SurfNum)=RhoVaporAirIn(Surface(SurfNum)%ExtBoundCond)
          HConvExtFD(SurfNum)=HConvIn(Surface(SurfNum)%ExtBoundCond)
          HMassConvExtFD(SurfNum)=HConvExtFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,TempOutsideAirFD(SurfNum),  &
              PsyWFnTdbRhPb(TempOutsideAirFD(SurfNum),1.0d0,OutBaroPress,'CalcHeatBalanceOutsideSurf:IZPart'))+  &
              RhoVaporAirOut(SurfNum))*PsyCpAirFnWTdb(OutHumRat,TempOutsideAirFD(SurfNum)))
          HSkyFD(SurfNum) = 0.0d0
          HGrndFD(SurfNum) = 0.0d0
          HAirFD(SurfNum) = 0.0d0
        ENDIF

      END IF

          ! This ends the calculations for this surface and goes on to the next SurfNum
    END SELECT

    !fill in reporting values for outside face
    QdotConvOutRep(SurfNum)        = - Surface(SurfNum)%Area * HcExtSurf(SurfNum) *(TH(SurfNum,1,1) &
                                                               - Surface(SurfNum)%OutDryBulbTemp)

    IF (Surface(SurfNum)%OSCMPtr > 0) THEN !Optr is set above in this case, use OSCM boundary data
        QdotConvOutRepPerArea(SurfNum) = - OSCM(OPtr)%Hconv *(TH(SurfNum,1,1) - OSCM(OPtr)%Tconv)
    ELSE
        QdotConvOutRepPerArea(SurfNum) = - HcExtSurf(SurfNum) *(TH(SurfNum,1,1) - Surface(SurfNum)%OutDryBulbTemp)
    END IF

    QConvOutReport(SurfNum)        = QdotConvOutRep(SurfNum)* SecInHour * TimeStepZone

  END DO    ! ...end of DO loop over all surface (actually heat transfer surfaces)

  RETURN

END SUBROUTINE CalcHeatBalanceOutsideSurf

SUBROUTINE CalcHeatBalanceInsideSurf(ZoneToResimulate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   December 1979
          !       MODIFIED       Jun 1990 (RDT for new CTF arrays)
          !                      Dec 1999 (FCW for window calculation)
          !                      May 2000 (FCW for window frame and dividers)
          !                      Aug 2000 (RJL for MTF moisture calculations)
          !                      Sep 2000 (RKS for new radiant exchange algorithm)
          !                      Dec 2000 (RKS for radiant system model addition)
          !                      Jul 2003 (CC) set the reference temperatures for inside surface heat balance
          !                                    depending on convection algorithms and/or air models used
          !                      May 2006 (RR  account for exterior window screen)
          !                      Jul 2008 (P. Biddulph include calls to HAMT)
          !                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  Mar 1998 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs a heat balance on the outside face of each
          ! surface in the building.

          ! METHODOLOGY EMPLOYED:
          ! Various boundary conditions are set and additional parameters are set-
          ! up.  Then, the proper heat balance equation is selected based on whether
          ! the surface is a partition or not and on whether or not movable
          ! insulation is present on the inside face.

          ! REFERENCES:
          ! (I)BLAST legacy routine HBSRF

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals
  USE DataEnvironment
  USE DataHeatBalFanSys
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataSurfaces
  USE DataInterfaces,               ONLY: ShowMessage,ShowContinueError,ShowContinueErrorTimeStamp,ShowFatalError,  &
                ShowSevereError,ShowSevereMessage,ShowWarningError,ShowWarningMessage,ShowErrorMessage,  &
                ShowRecurringSevereErrorAtEnd,ShowRecurringWarningErrorAtEnd,ShowRecurringContinueErrorAtEnd,  &
                SetupOutputVariable
  USE DataDaylightingDevices
  USE DataMoistureBalance,          ONLY : TempOutsideAirFD,RhoVaporAirOut,RhoVaporAirIn,HConvExtFD,HMassConvExtFD, &
                                           HConvInFD,HMassConvInFD,RhoVaporSurfIn, &
                                           HSkyFD,HGrndFD,HAirFD
  USE DataMoistureBalanceEMPD,      ONLY : MoistEMPDNew, MoistEMPDFlux
  USE DataAirflowNetwork,           ONLY : SimulateAirflowNetwork,AirflowNetworkControlSimple

  USE HeatBalanceMovableInsulation, ONLY : EvalInsideMovableInsulation
  USE WindowManager,                ONLY : CalcWindowHeatBalance
  USE HeatBalFiniteDiffManager,     ONLY : ManageHeatBalFiniteDiff, SurfaceFD
  USE HeatBalanceHAMTManager,       ONLY : ManageHeatBalHAMT,UpdateHeatBalHAMT
  USE ConvectionCoefficients,       ONLY : InitExteriorConvectionCoeff, InitInteriorConvectionCoeffs, &
                                           SetExtConvectionCoeff, SetIntConvectionCoeff
  USE HeatBalanceIntRadExchange,    ONLY : CalcInteriorRadExchange
  USE MoistureBalanceEMPDManager,   ONLY : CalcMoistureBalanceEMPD,UpdateMoistureBalanceEMPD
  USE ScheduleManager,              ONLY : GetCurrentScheduleValue
  USE General,                      ONLY : RoundSigDigits
  USE DaylightingDevices,           ONLY : FindTDDPipe
  USE DataZoneEquipment,            ONLY : ZoneEquipConfig
  USE DataLoopNode,                 ONLY : Node
  USE HeatBalanceSurfaceManager,    ONLY : CalculateZoneMRT
  USE Psychrometrics
  USE OutputReportTabular, ONLY: loadConvectedNormal,loadConvectedWithPulse,netSurfRadSeq
  USE DataSizing, ONLY: CurOverallSimDay
  USE DataTimings
  USE WindowEquivalentLayer,        ONLY : EQLWindowOutsideEffectiveEmiss


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN), OPTIONAL :: ZoneToResimulate  ! if passed in, then only calculate surfaces that have this zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Sigma = 5.6697d-08          ! Stefan-Boltzmann constant
  REAL(r64), PARAMETER :: IterDampConst = 5.0d0         ! Damping constant for inside surface temperature iterations
  INTEGER,   PARAMETER :: ItersReevalConvCoeff = 30   ! Number of iterations between inside convection coefficient reevaluations
  REAL(r64), PARAMETER :: MaxAllowedDelTemp = 0.002d0   ! Convergence criteria for inside surface temperatures
  INTEGER,   PARAMETER :: MaxIterations = 500         ! Maximum number of iterations allowed for inside surface temps
  INTEGER,   PARAMETER :: IterationsForCondFDRelaxChange = 5 ! number of iterations for inside temps that triggers a change
                                                              ! in the CondFD relaxation factor.
  INTEGER,   PARAMETER :: MinEMPDIterations = 4       ! Minimum number of iterations required for EMPD solution

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)        :: AbsInt          ! Solar absorptance of inside movable insulation
  INTEGER          :: ConstrNum       ! Construction index for the current surface
  LOGICAL          :: Converged       ! .true. if inside heat balance has converged
  REAL(r64)        :: F1              ! Intermediate calculation value
  REAL(r64)        :: HMovInsul       ! "Convection" coefficient of movable insulation
  REAL(r64) :: MaxDelTemp      ! Maximum change in surface temperature for any
                                      !  opaque surface from one iteration to the next
  INTEGER          :: SurfNum         ! Surface number
  INTEGER          :: ZoneNum         ! Zone number the current surface is attached to
  INTEGER          :: ConstrNumSh     ! Shaded construction number for a window
  INTEGER          :: RoughSurf       ! Outside surface roughness
  REAL(r64)        :: EmisOut         ! Glass outside surface emissivity

  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: TempInsOld ! Holds previous iteration's value for convergence check
  REAL(r64)        :: RhoVaporSat     ! Local temporary saturated vapor density for checking
  REAL(r64) :: TempSurfOutTmp  ! Local Temporary Surface temperature for the outside surface face
  REAL(r64)        :: TempSurfInSat   ! Local temperary surface dew point temperature
  LOGICAL, SAVE    :: FirstTime = .TRUE. ! Used for trapping errors or other problems
  INTEGER          :: OtherSideSurfNum   ! Surface number index for other side of an interzone partition
  INTEGER, SAVE    :: MinIterations   ! Minimum number of iterations for the inside heat balance
!  CHARACTER(len=25):: ErrMsg
!  CHARACTER(len=5) :: TimeStmp
  INTEGER, SAVE    :: ErrCount=0
  INTEGER          :: PipeNum         ! TDD pipe object number
  INTEGER          :: SurfNum2        ! TDD:DIFFUSER object number
  REAL(r64)        :: Ueff            ! 1 / effective R value between TDD:DOME and TDD:DIFFUSER

  INTEGER           :: ZoneEquipConfigNum
!  LOGICAL           :: ControlledZoneAirFlag
  INTEGER           :: NodeNum
  REAL(r64)         :: SumSysMCp             ! Zone sum of air system MassFlowRate*Cp
  REAL(r64)         :: SumSysMCpT            ! Zone sum of air system MassFlowRate*Cp*T
  REAL(r64)         :: MassFlowRate
  REAL(r64)         :: NodeTemp
  REAL(r64)         :: CpAir
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: RefAirTemp ! reference air temperatures
  LOGICAL, SAVE     :: MyEnvrnFlag=.true.
!  LOGICAL, SAVE     :: DoThisLoop
  INTEGER, SAVE     :: InsideSurfErrCount=0
  REAL(r64) :: Wsurf ! Moisture ratio for HAMT
  REAL(r64) :: RhoAirZone ! Zone moisture density for HAMT
  INTEGER :: OtherSideZoneNum ! Zone Number index for other side of an interzone partition HAMT
  INTEGER,SAVE :: WarmupSurfTemp
  LOGICAL :: PartialResimulate
  INTEGER          :: TimeStepInDay=0 ! time step number

          ! FLOW:
  IF (FirstTime) THEN
    ALLOCATE(TempInsOld(TotSurfaces))
    ALLOCATE(RefAirTemp(TotSurfaces))
    IF (ANY(HeatTransferAlgosUsed == UseEMPD)) THEN
      MinIterations = MinEMPDIterations
    ELSE
      MinIterations = 1
    END IF
    IF (DisplayAdvancedReportVariables) THEN
      CALL SetupOutputVariable('Surface Inside Face Heat Balance Calculation Iteration Count []',InsideSurfIterations, &
                            'ZONE','Sum','Simulation')
    ENDIF
  ENDIF
  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    TempInsOld=23.0d0
    RefAirTemp=23.0d0
    TempEffBulkAir=23.0d0
    WarmupSurfTemp=0
    MyEnvrnFlag=.false.
  ENDIF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  PartialResimulate=.false.

  ! determine reference air temperatures
  DO SurfNum = 1, TotSurfaces
      ZoneNum = Surface(SurfNum)%Zone

      IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. ZoneNum == 0) CYCLE  ! Skip non-heat transfer surfaces
      IF (Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE  ! Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

      IF (PRESENT(ZoneToResimulate)) THEN
        PartialResimulate=.true.
        IF ((ZoneNum /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
          CYCLE ! skip surfaces that are not associated with this zone
        ENDIF
      ENDIF
      IF (PartialResimulate) THEN
         WinHeatGain(Surfnum)                     = 0.0d0
         WinHeatGainRep(Surfnum)                  = 0.0d0
         WinHeatLossRep(Surfnum)                  = 0.0d0
         WinGainConvGlazToZoneRep(Surfnum)        = 0.0D0
         WinGainIRGlazToZoneRep(Surfnum)          = 0.0D0
         WinLossSWZoneToOutWinRep(Surfnum)        = 0.0D0
         WinGainFrameDividertoZoneRep(Surfnum)    = 0.0D0
         WinGainConvGlazShadGapToZoneRep(Surfnum) = 0.0D0
         WinGainConvShadeToZoneRep(Surfnum)       = 0.0D0
         OtherConvGainInsideFaceToZoneRep(Surfnum)= 0.0D0
         WinGainIRShadeToZoneRep(Surfnum)         = 0.0D0
         SurfaceWindow(Surfnum)%FrameQRadOutAbs   = 0.0d0
         SurfaceWindow(Surfnum)%FrameQRadInAbs    = 0.0d0
         SurfaceWindow(Surfnum)%DividerQRadOutAbs = 0.0d0
         SurfaceWindow(Surfnum)%DividerQRadInAbs  = 0.0d0
      ENDIF

      SELECT CASE (Surface(SurfNum)%TAirRef)
        CASE (ZoneMeanAirTemp)
            RefAirTemp(SurfNum) = MAT(ZoneNum)
            TempEffBulkAir(SurfNum) = MAT(ZoneNum)  ! for reporting surf adjacent air temp
        CASE (AdjacentAirTemp)
            RefAirTemp(SurfNum) = TempEffBulkAir(SurfNum)
        CASE (ZoneSupplyAirTemp)
            ! determine ZoneEquipConfigNum for this zone
            ZoneEquipConfigNum = ZoneNum
            ! check whether this zone is a controlled zone or not
            IF (.NOT. Zone(ZoneNum)%IsControlled) THEN
                CALL ShowFatalError('Zones must be controlled for Ceiling-Diffuser Convection model. No system serves zone '//  &
                                    TRIM(Zone(ZoneNum)%Name))
                RETURN
            END IF
            ! determine supply air conditions
            SumSysMCp = 0.0d0
            SumSysMCpT = 0.0d0
            DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
                NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
                MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
                CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
                SumSysMCp = SumSysMCp + MassFlowRate * CpAir
                SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
            END DO
            ! a weighted average of the inlet temperatures.
            IF (SumSysMCp > 0.d0) THEN ! protect div by zero
              RefAirTemp(SurfNum)     = SumSysMCpT/SumSysMCp  ! BG changed 02-16-2005 to add index (SurfNum)
            ELSE
              RefAirTemp(SurfNum)     = NodeTemp
            ENDIF
            TempEffBulkAir(SurfNum) = RefAirTemp(SurfNum)   ! for reporting surf adjacent air temp
        CASE DEFAULT
            ! currently set to mean air temp but should add error warning here
            RefAirTemp(SurfNum)     = MAT(ZoneNum)
            TempEffBulkAir(SurfNum) = MAT(ZoneNum)  ! for reporting surf adjacent air temp
      END SELECT
  END DO

  InsideSurfIterations = 0
  ! Following variables must be reset due to possible recall of this routine by radiant and Resimulate routines.
  ! CalcWindowHeatBalance is called, then, multiple times and these need to be initialized before each call to
  ! CalcWindowHeatBalance.
  IF (.not. PartialResimulate) THEN
    WinHeatGain                     = 0.0d0
    WinHeatGainRep                  = 0.0d0
    WinHeatLossRep                  = 0.0d0
    WinGainConvGlazToZoneRep        = 0.0D0
    WinGainIRGlazToZoneRep          = 0.0D0
    WinLossSWZoneToOutWinRep        = 0.0D0
    WinGainFrameDividertoZoneRep    = 0.0D0
    WinGainConvGlazShadGapToZoneRep = 0.0D0
    WinGainConvShadeToZoneRep       = 0.0D0
    OtherConvGainInsideFaceToZoneRep= 0.0D0
    WinGainIRShadeToZoneRep         = 0.0D0
    SurfaceWindow%FrameQRadOutAbs   = 0.0d0
    SurfaceWindow%FrameQRadInAbs    = 0.0d0
    SurfaceWindow%DividerQRadOutAbs = 0.0d0
    SurfaceWindow%DividerQRadInAbs  = 0.0d0
  ENDIF

  Converged = .FALSE.
  DO WHILE (.NOT. Converged) ! Start of main inside heat balance DO loop...

    TempInsOld = TempSurfIn ! Keep track of last iteration's temperature values

    IF (PRESENT(ZoneToResimulate)) THEN
      CALL CalcInteriorRadExchange(TempSurfIn,InsideSurfIterations,NetLWRadToSurf,  &
         ZoneToResimulate,calledfrom='Inside') ! Update the radiation balance
    ELSE
      CALL CalcInteriorRadExchange(TempSurfIn,InsideSurfIterations,NetLWRadToSurf,  &
         calledfrom='Inside') ! Update the radiation balance
    ENDIF

          ! Every 30 iterations, recalculate the inside convection coefficients in case
          ! there has been a significant drift in the surface temperatures predicted.
          ! This is not fool-proof and it basically means that the outside surface
          ! heat balance is in error (potentially) once HConvIn is re-evaluated.
          ! The choice of 30 is not significant--just want to do this a couple of
          ! times before the iteration limit is hit.
    IF ((InsideSurfIterations>0).AND.(MOD(InsideSurfIterations,ItersReevalConvCoeff)==0)) THEN
      IF (PRESENT(ZoneToResimulate)) THEN
        CALL InitInteriorConvectionCoeffs(TempSurfIn,ZoneToResimulate)
      ELSE
        CALL InitInteriorConvectionCoeffs(TempSurfIn)
      ENDIF
    ENDIF

    DO SurfNum = 1, TotSurfaces   ! Perform a heat balance on all of the inside surface...

      ZoneNum = Surface(SurfNum)%Zone

      IF (.NOT. Surface(SurfNum)%HeatTransSurf .OR. ZoneNum == 0) CYCLE  ! Skip non-heat transfer surfaces
      IF (Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE  ! Skip TDD:DOME objects.  Inside temp is handled by TDD:DIFFUSER.

      IF (PRESENT(ZoneToResimulate)) THEN
        IF ((ZoneNum /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
          CYCLE ! skip surfaces that are not associated with this zone
        ENDIF
      ENDIF

      ConstrNum = Surface(SurfNum)%Construction

      !Calculate the inside surface moisture quantities
       !calculate the inside surface moisture transfer conditions
      RhoVaporAirIn(SurfNum)=PsyRhovFnTdbWPb(MAT(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress)
       !check for saturation conditions of air
      RhoVaporSat=PsyRhovFnTdbRh(MAT(ZoneNum),1.0d0,'HB,SurfMan:InsideSurf')
      IF (RhoVaporAirIn(SurfNum) .GT. RhoVaporSat) RhoVaporAirIn(SurfNum)=RhoVaporSat
      HConvInFD(SurfNum)=HConvIn(SurfNum)
      HMassConvInFD(SurfNum)=HConvInFD(SurfNum)/((PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))+ &
                   RhoVaporAirIn(SurfNum))*PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MAT(ZoneNum)))

          ! Perform heat balance on the inside face of the surface ...
          ! The following are possibilities here:
          !   (a) the surface is a partition, in which case the temperature of both sides are the same
          !   (b) standard (or interzone) opaque surface with no movable insulation, normal heat balance equation
          !   (c) standard (or interzone) window: call to CalcWindowHeatBalance to get window layer temperatures
          !   (d) standard opaque surface with movable insulation, special two-part equation
          ! In the surface calculation there are the following Algorithm types for opaque surfaces that
          ! do not have movable insulation:
          !   (a) the regular CTF calc (SolutionAlgo = UseCTF)
          !   (b) the EMPD calc (Solutionalgo = UseEMPD)
          !   (c) the CondFD calc (SolutionAlgo = UseCondFD)
          !   (d) the HAMT calc (solutionalgo = UseHAMT).

      IF (Surface(SurfNum)%ExtBoundCond == SurfNum .and. Surface(SurfNum)%Class /= SurfaceClass_Window) THEN
!CR6869 -- let Window HB take care of it      IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN
        ! Surface is a partition
        IF ( Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF .OR. &
             Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) THEN  !Regular CTF Surface and/or EMPD surface

           If (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) Then
             Call CalcMoistureBalanceEMPD(SurfNum, TempSurfInTmp(SurfNum), &
                   TH(SurfNum,2,2), MAT(ZoneNum),TempSurfInSat)
           End If
           TempSurfInTmp(SurfNum) = ( CTFConstInPart(SurfNum)             & ! Constant portion of conduction eq (history terms)
                                    +QRadThermInAbs(SurfNum)             & ! LW radiation from internal sources
                                    +QRadSWInAbs(SurfNum)                & ! SW radiation from internal sources
                                    +HConvIn(SurfNum)*RefAirTemp(SurfNum) & ! Convection from surface to zone air
                                    +NetLWRadToSurf(SurfNum)             & ! Net radiant exchange with other zone surfaces
                                    +Construct(ConstrNum)%CTFSourceIn(0) & ! Heat source/sink term for radiant systems
                                     *QsrcHist(SurfNum,1)                & ! (if there is one present)
                                    +QHTRadSysSurf(SurfNum)              & ! Radiant flux from a high temperature radiant heater
                                    +QHWBaseboardSurf(SurfNum)           & ! Radiant flux from a hot water baseboard heater
                                    +QSteamBaseboardSurf(SurfNum)        & ! Radiant flux from a steam baseboard heater
                                    +QElecBaseboardSurf(SurfNum)         & ! Radiant flux from an electric baseboard heater
                                    +IterDampConst                       &
                                     *TempInsOld(SurfNum) )              & ! Iterative damping term (for stability)
                                  /( Construct(ConstrNum)%CTFInside(0)   & ! Conduction term (both partition sides same temp)
                                    -Construct(ConstrNum)%CTFCross(0)    & ! Conduction term (both partition sides same temp)
                                    +HConvIn(SurfNum)+IterDampConst )   ! Convection and damping term

            If (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) Then
              TempSurfInTmp(SurfNum) = TempSurfInTmp(SurfNum)              &
                                      -MoistEMPDFlux(SurfNum)              &
                                      /( Construct(ConstrNum)%CTFInside(0) &  ! Conduction term (both partition sides same temp)
                                        -Construct(ConstrNum)%CTFCross(0)  &  ! Conduction term (both partition sides same temp)
                                        +HConvIn(SurfNum)+IterDampConst )   ! Convection and damping term
              if (TempSurfInSat .GT. TempSurfInTmp(SurfNum)) then
                 TempSurfInTmp(SurfNum) = TempSurfInSat    ! Surface temp cannot be below dew point
              end if
            End If
          ! if any mixed heat transfer models in zone, apply limits to CTF result
          IF (ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm == HeatTransferModel_CondFD)&
              .OR. &
              ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm == HeatTransferModel_HAMT) )&
              THEN
            TempSurfInTmp(SurfNum) = MAX(MinSurfaceTempLimit,MIN(MaxSurfaceTempLimit,TempSurfInTmp(SurfNum)))  !Limit Check
          ENDIF

          IF (Construct(ConstrNum)%SourceSinkPresent) THEN  ! Set the appropriate parameters for the radiant system

            ! Radiant system does not need the damping coefficient terms (hopefully)
            RadSysTiHBConstCoef(SurfNum) = ( CTFConstInPart(SurfNum)       & ! Constant portion of conduction eq (history terms)
                                            +QRadThermInAbs(SurfNum)       & ! LW radiation from internal sources
                                            +QRadSWInAbs(SurfNum)          & ! SW radiation from internal sources
                                            +HConvIn(SurfNum)*RefAirTemp(SurfNum) & ! Convection from surface to zone air
                                            +QHTRadSysSurf(SurfNum)        & ! Radiant flux from high temperature radiant heater
                                            +QHWBaseboardSurf(SurfNum)     & ! Radiant flux from a hot water baseboard heater
                                            +QSteamBaseboardSurf(SurfNum)  & ! Radiant flux from a steam baseboard heater
                                            +QElecBaseboardSurf(SurfNum)   & ! Radiant flux from an electric baseboard heater
                                            +NetLWRadToSurf(SurfNum) )     & ! Net radiant exchange with other zone surfaces
                                          /( Construct(ConstrNum)%CTFInside(0) & ! Cond term (both partition sides same temp)
                                            -Construct(ConstrNum)%CTFCross(0)  & ! Cond term (both partition sides same temp)
                                            +HConvIn(SurfNum) )              ! Convection and damping term
            RadSysTiHBToutCoef(SurfNum)  = 0.0d0  ! The outside temp is assumed to be equal to the inside temp for a partition
            RadSysTiHBQsrcCoef(SurfNum)  = Construct(ConstrNum)%CTFSourceIn(0) & ! QTF term for the source
                                          /( Construct(ConstrNum)%CTFInside(0) & ! Cond term (both partition sides same temp)
                                            -Construct(ConstrNum)%CTFCross(0)  & ! Cond term (both partition sides same temp)
                                            +HConvIn(SurfNum) )                  ! Convection and damping term

            RadSysToHBConstCoef(SurfNum) = RadSysTiHBConstCoef(SurfNum) ! Partitions are assumed to be symmetric
            RadSysToHBTinCoef(SurfNum)   = RadSysTiHBToutCoef(SurfNum)  ! Partitions are assumed to be symmetric
            RadSysToHBQsrcCoef(SurfNum)  = RadSysTiHBQsrcCoef(SurfNum)  ! Partitions are assumed to be symmetric

          END IF

        ELSE IF(Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CondFD .OR. &
                Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN

          IF(Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) &
             CALL ManageHeatBalHAMT(SurfNum,TempSurfInTmp(SurfNum),TempSurfOutTmp)  !HAMT

          If(Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD )  &
             CALL ManageHeatBalFiniteDiff(SurfNum,TempSurfInTmp(SurfNum),TempSurfOutTmp)

          TH(SurfNum,1,1) = TempSurfOutTmp

        End If

        TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum)

      ELSE  ! Standard surface or interzone surface

        IF (Surface(SurfNum)%Class /= SurfaceClass_Window) THEN ! Opaque surface

          HMovInsul = 0.0d0
          IF (Surface(SurfNum)%MaterialMovInsulInt > 0) &
            CALL EvalInsideMovableInsulation(SurfNum,HMovInsul,AbsInt)

          IF (HMovInsul <= 0.0d0) THEN  ! No movable insulation present, normal heat balance equation

            IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_CTF .OR. &
                Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) THEN ! Regular CTF Surface and/or EMPD surface

               IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) THEN
                  Call CalcMoistureBalanceEMPD(SurfNum, TempSurfInTmp(SurfNum), &
                      TH(SurfNum,2,2), MAT(ZoneNum),TempSurfInSat)
               End If
               TempSurfInTmp(SurfNum) = ( CTFConstInPart(SurfNum)              & ! Constant part of conduction eq (history terms)
                                        +QRadThermInAbs(SurfNum)              & ! LW radiation from internal sources
                                        +QRadSWInAbs(SurfNum)                 & ! SW radiation from internal sources
                                        +HConvIn(SurfNum)*RefAirTemp(SurfNum) & ! Convection from surface to zone air
                                        +NetLWRadToSurf(SurfNum)              & ! Net radiant exchange with other zone surfaces
                                        +Construct(ConstrNum)%CTFSourceIn(0)  & ! Heat source/sink term for radiant systems
                                         *QsrcHist(SurfNum,1)                 & ! (if there is one present)
                                        +QHTRadSysSurf(SurfNum)               & ! Radiant flux from high temp radiant heater
                                        +QHWBaseboardSurf(SurfNum)            & ! Radiant flux from a hot water baseboard heater
                                        +QSteamBaseboardSurf(SurfNum)         & ! Radiant flux from a steam baseboard heater
                                        +QElecBaseboardSurf(SurfNum)          & ! Radiant flux from an electric baseboard heater
                                        +IterDampConst                        &
                                         *TempInsOld(SurfNum)                 & ! Iterative damping term (for stability)
                                        +Construct(ConstrNum)%CTFCross(0)     & ! Current conduction from
                                         *TH(SurfNum,1,1) )                   & ! the outside surface
                                      /( Construct(ConstrNum)%CTFInside(0)    & ! Coefficient for conduction (current time)
                                        +HConvIn(SurfNum)+IterDampConst )         ! Convection and damping term
               IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) THEN
                 TempSurfInTmp(SurfNum) = TempSurfInTmp(SurfNum)              &
                                          -MoistEMPDFlux(SurfNum)              &
                                          /( Construct(ConstrNum)%CTFInside(0) &  ! Coefficient for conduction (current time)
                                            +HConvIn(SurfNum)+IterDampConst )   ! Convection and damping term
                 if (TempSurfInSat .GT. TempSurfInTmp(SurfNum)) then
                   TempSurfInTmp(SurfNum) = TempSurfInSat ! Surface temp cannot be below dew point
                 end if
               End If
             ! if any mixed heat transfer models in zone, apply limits to CTF result
              IF (ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm &
                       == HeatTransferModel_CondFD) .OR. &
                  ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm &
                       == HeatTransferModel_HAMT) )&
                  THEN
                TempSurfInTmp(SurfNum) = MAX(MinSurfaceTempLimit,MIN(MaxSurfaceTempLimit,TempSurfInTmp(SurfNum)))  !Limit Check
              ENDIF

              IF (Construct(ConstrNum)%SourceSinkPresent) THEN  ! Set the appropriate parameters for the radiant system

                ! Radiant system does not need the damping coefficient terms (hopefully)
                RadSysTiHBConstCoef(SurfNum) = ( CTFConstInPart(SurfNum)       & ! Constant portion of cond eq (history terms)
                                                +QRadThermInAbs(SurfNum)       & ! LW radiation from internal sources
                                                +QRadSWInAbs(SurfNum)          & ! SW radiation from internal sources
                                                +HConvIn(SurfNum)*RefAirTemp(SurfNum) & ! Convection from surface to zone air
                                                +QHTRadSysSurf(SurfNum)        & ! Radiant flux from high temp radiant heater
                                                +QHWBaseboardSurf(SurfNum)     & ! Radiant flux from a hot water baseboard heater
                                                +QSteamBaseboardSurf(SurfNum)  & ! Radiant flux from a steam baseboard heater
                                                +QElecBaseboardSurf(SurfNum)   & ! Radiant flux from an electric baseboard heater
                                                +NetLWRadToSurf(SurfNum) )     & ! Net radiant exchange with other zone surfaces
                                              /( Construct(ConstrNum)%CTFInside(0) & ! Cond term (both partition sides same temp)
                                                +HConvIn(SurfNum) )              ! Convection and damping term
                RadSysTiHBToutCoef(SurfNum)  = Construct(ConstrNum)%CTFCross(0)    & ! Outside temp=inside temp for a partition
                                              /( Construct(ConstrNum)%CTFInside(0) & ! Cond term (both partition sides same temp)
                                                +HConvIn(SurfNum) )              ! Convection and damping term
                RadSysTiHBQsrcCoef(SurfNum)  = Construct(ConstrNum)%CTFSourceIn(0) & ! QTF term for the source
                                              /( Construct(ConstrNum)%CTFInside(0) & ! Cond term (both partition sides same temp)
                                                +HConvIn(SurfNum) )              ! Convection and damping term

                IF (Surface(SurfNum)%ExtBoundCond > 0) THEN ! This is an interzone partition and we need to set outside params
                  ! The inside coefficients of one side are equal to the outside coefficients of the other side.  But,
                  ! the inside coefficients are set up once the heat balance equation for that side has been calculated.
                  ! For both sides to actually have been set, we have to wait until we get to the second side in the surface
                  ! derived type.  At that point, both inside coefficient sets have been evaluated.
                  IF (Surface(SurfNum)%ExtBoundCond < SurfNum) THEN ! Both of the inside coefficients have now been set
                    OtherSideSurfNum                      = Surface(SurfNum)%ExtBoundCond
                    RadSysToHBConstCoef(OtherSideSurfNum) = RadSysTiHBConstCoef(SurfNum)
                    RadSysToHBTinCoef(OtherSideSurfNum)   = RadSysTiHBToutCoef(SurfNum)
                    RadSysToHBQsrcCoef(OtherSideSurfNum)  = RadSysTiHBQsrcCoef(SurfNum)
                    RadSysToHBConstCoef(SurfNum)          = RadSysTiHBConstCoef(OtherSideSurfNum)
                    RadSysToHBTinCoef(SurfNum)            = RadSysTiHBToutCoef(OtherSideSurfNum)
                    RadSysToHBQsrcCoef(SurfNum)           = RadSysTiHBQsrcCoef(OtherSideSurfNum)
                  END IF
                END IF

              END IF

            Else If(Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD .OR. &
                    Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN

              IF(Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
                 IF (Surface(SurfNum)%ExtBoundCond > 0) THEN
                    ! HAMT get the correct other side zone zone air temperature --
                    OtherSideSurfNum = Surface(SurfNum)%ExtBoundCond
                    ZoneNum = Surface(SurfNum)%Zone
                    OtherSideZoneNum = Surface(OtherSideSurfNum)%Zone
                    TempOutsideAirFD(SurfNum)= MAT(OtherSideZoneNum)
                 ENDIF
                CALL ManageHeatBalHAMT(SurfNum,TempSurfInTmp(SurfNum),TempSurfOutTmp)
              ENDIF

              If(Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD )   &
                  CALL ManageHeatBalFiniteDiff(SurfNum,TempSurfInTmp(SurfNum),TempSurfOutTmp)

              TH(SurfNum,1,1) = TempSurfOutTmp

            End If

            TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum)


          ELSE    ! Movable insulation present

            IF (Construct(ConstrNum)%SourceSinkPresent.AND.FirstTime) &
              CALL ShowSevereError('Movable insulation is not valid with embedded sources/sinks')

            F1 = HMovInsul/( HMovInsul + HConvIn(SurfNum) + IterDampConst )

            TempSurfIn(SurfNum) = ( CTFConstInPart(SurfNum)                          &
                                   +QRadSWInAbs(SurfNum)                             &
                                   +Construct(ConstrNum)%CTFCross(0)*TH(SurfNum,1,1) &
                                   +F1*( QRadThermInAbs(SurfNum)                     &
                                        +HConvIn(SurfNum)*RefAirTemp(SurfNum)        & ! Convection from surface to zone air
                                        +NetLWRadToSurf(SurfNum)                     &
                                        +QHTRadSysSurf(SurfNum)                      &
                                        +QHWBaseboardSurf(SurfNum)                   &
                                        +QSteamBaseboardSurf(SurfNum)                &
                                        +QElecBaseboardSurf(SurfNum)                 &
                                        +IterDampConst*TempInsOld(SurfNum) )       ) &
                                 /( Construct(ConstrNum)%CTFInside(0)                &
                                   +HMovInsul - F1*HMovInsul )

            TempSurfInTmp(SurfNum) = ( Construct(ConstrNum)%CTFInside(0)     &
                                       *TempSurfIn(SurfNum)                  &
                                      +HMovInsul*TempSurfIn(SurfNum)         &
                                      -QRadSWInAbs(SurfNum)                  &
                                      -CTFConstInPart(SurfNum)               &
                                      -Construct(ConstrNum)%CTFCross(0)      &
                                       *TH(SurfNum,1,1) )                    &
                                    /( HMovInsul )
            ! if any mixed heat transfer models in zone, apply limits to CTF result
            IF (ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm &
                     == HeatTransferModel_CondFD) .OR. &
                ANY(Surface(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast)%HeatTransferAlgorithm &
                     == HeatTransferModel_HAMT) )&
                THEN
              TempSurfInTmp(SurfNum) = MAX(MinSurfaceTempLimit,MIN(MaxSurfaceTempLimit,TempSurfInTmp(SurfNum)))  !Limit Check
            ENDIF
          END IF

        ELSE  ! Window

          IF (Construct(ConstrNum)%SourceSinkPresent.AND.FirstTime) &
            CALL ShowSevereError('Windows are not allowed to have embedded sources/sinks')

          IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN ! Tubular daylighting device
            ! Lookup up the TDD:DOME object
            PipeNum = FindTDDPipe(SurfNum)
            SurfNum2 = TDDPipe(PipeNum)%Dome
            Ueff = 1.0d0 / TDDPipe(PipeNum)%Reff

            ! Similar to opaque surface but outside surface temp of TDD:DOME is used, and no embedded sources/sinks.
            ! Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
            !   = QRadSWwinAbs(SurfNum,1)/2.0
            TempSurfInTmp(SurfNum) = ( QRadThermInAbs(SurfNum)             & ! LW radiation from internal sources
                                      +QRadSWwinAbs(SurfNum,1)/2.0d0       & ! SW radiation from internal sources and solar
                                      +HConvIn(SurfNum)*RefAirTemp(SurfNum) & ! Convection from surface to zone air
                                      +NetLWRadToSurf(SurfNum)             & ! Net radiant exchange with other zone surfaces
                                      +IterDampConst                       &
                                       *TempInsOld(SurfNum)                & ! Iterative damping term (for stability)
                                      +Ueff*TH(SurfNum2,1,1) )             & ! Current conduction from the outside surface
                                    /( Ueff                                & ! Coefficient for conduction (current time)
                                      +HConvIn(SurfNum)+IterDampConst ) ! Convection and damping term

            TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum)

            ! Calculate window heat gain for TDD:DIFFUSER since this calculation is usually done in WindowManager
            WinHeatGain(SurfNum) = WinTransSolar(SurfNum) & ! Transmitted solar
              + HConvIn(SurfNum) * Surface(SurfNum)%Area * (TempSurfIn(SurfNum) - RefAirTemp(SurfNum)) & ! Convection
              + Construct(Surface(SurfNum)%Construction)%InsideAbsorpThermal * Surface(SurfNum)%Area & ! IR exchange
              * (Sigma * TempSurfIn(SurfNum)**4 - (SurfaceWindow(SurfNum)%IRfromParentZone + QHTRadSysSurf(SurfNum) + &
                 QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)))     & ! IR
              - QS(Surface(SurfNum)%Zone) * Surface(SurfNum)%Area * Construct(Surface(SurfNum)%Construction)%TransDiff
              ! Zone diffuse interior shortwave reflected back into the TDD

            !fill out report vars for components of Window Heat Gain
            WinGainConvGlazToZoneRep(SurfNum) = HConvIn(SurfNum) * Surface(SurfNum)%Area &
                                                * (TempSurfIn(SurfNum) - RefAirTemp(SurfNum))
            WinGainIRGlazToZoneRep(SurfNum) = Construct(Surface(SurfNum)%Construction)%InsideAbsorpThermal &
                                              * Surface(SurfNum)%Area  &
                 * (Sigma * TempSurfIn(SurfNum)**4 - (SurfaceWindow(SurfNum)%IRfromParentZone + QHTRadSysSurf(SurfNum) + &
                      QHWBaseboardSurf(SurfNum) + QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)))
            WinLossSWZoneToOutWinRep(SurfNum) = QS(Surface(SurfNum)%Zone) * Surface(SurfNum)%Area &
                                                 * Construct(Surface(SurfNum)%Construction)%TransDiff
            IF(WinHeatGain(SurfNum) >= 0.0d0) THEN
              WinHeatGainRep(SurfNum) = WinHeatGain(SurfNum)
              WinHeatGainRepEnergy(SurfNum) = WinHeatGainRep(SurfNum) * TimeStepZone * SecInHour
            ELSE
              WinHeatLossRep(SurfNum) = -WinHeatGain(SurfNum)
              WinHeatLossRepEnergy(SurfNum) = WinHeatLossRep(SurfNum) * TimeStepZone * SecInHour
            END IF

            TDDPipe(PipeNum)%HeatGain = WinHeatGainRep(SurfNum)
            TDDPipe(PipeNum)%HeatLoss = WinHeatLossRep(SurfNum)

          ELSE ! Regular window
            IF(InsideSurfIterations == 0) THEN  ! Do windows only once
              IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNum = Surface(SurfNum)%StormWinConstruction
              ! Get outside convection coeff for exterior window here to avoid calling
              ! InitExteriorConvectionCoeff from CalcWindowHeatBalance, which avoids circular reference
              ! (HeatBalanceSurfaceManager USEing and WindowManager and
              ! WindowManager USEing HeatBalanceSurfaceManager)
              IF(Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
                RoughSurf = Material(Construct(ConstrNum)%LayerPoint(1))%Roughness
                EmisOut = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermalFront
                IF(SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag==ExtBlindOn .OR. &
                   SurfaceWindow(SurfNum)%ShadingFlag == ExtScreenOn) THEN
                        ! Exterior shade in place
                  ConstrNumSh = SurfaceWindow(SurfNum)%ShadedConstruction
                  RoughSurf = Material(Construct(ConstrNumSh)%LayerPoint(1))%Roughness
                  EmisOut = Material(Construct(ConstrNumSh)%LayerPoint(1))%AbsorpThermal
                END IF

                ! Get the outside effective emissivity for Equivalent layer model
                IF ( Construct(ConstrNum)%WindowTypeEQL ) THEN
                   EmisOut = EQLWindowOutsideEffectiveEmiss(ConstrNum)
                ENDIF
                ! Set Exterior Convection Coefficient...
                IF (Surface(SurfNum)%ExtConvCoeff > 0) THEN

                  HcExtSurf(SurfNum)=SetExtConvectionCoeff(SurfNum)

                ELSEIF (Surface(SurfNum)%ExtWind) THEN  ! Window is exposed to wind (and possibly rain)

                  ! Calculate exterior heat transfer coefficients with windspeed (windspeed is calculated internally in subroutine)
                  CALL InitExteriorConvectionCoeff(SurfNum,0.0d0,RoughSurf,EmisOut,TH(SurfNum,1,1), &
                    HcExtSurf(SurfNum),HSkyExtSurf(SurfNum),HGrdExtSurf(SurfNum),HAirExtSurf(SurfNum))

                  IF (IsRain) THEN  ! Raining: since wind exposed, outside window surface gets wet
                    HcExtSurf(SurfNum)=1000.0d0 ! Reset HcExtSurf because of wetness
                  ENDIF

                ELSE  ! Not Wind exposed

                  ! Calculate exterior heat transfer coefficients for windspeed = 0
                  CALL InitExteriorConvectionCoeff(SurfNum,0.0d0,RoughSurf,EmisOut,TH(SurfNum,1,1), &
                    HcExtSurf(SurfNum),HSkyExtSurf(SurfNum),HGrdExtSurf(SurfNum),HAirExtSurf(SurfNum))

                END IF
              ELSE   ! Interior Surface

                IF (Surface(SurfNum)%ExtConvCoeff > 0) THEN
                  HcExtSurf(SurfNum)=SetExtConvectionCoeff(SurfNum)
                ELSE
                ! Exterior Convection Coefficient for the Interior or Interzone Window is the Interior Convection Coeff of same
                  HcExtSurf(SurfNum)=HConvIn(Surface(SurfNum)%ExtBoundCond)
                ENDIF

              ENDIF

              ! Following call determines inside surface temperature of glazing, and of
              ! frame and/or divider, if present
              CALL CalcWindowHeatBalance(SurfNum,HcExtSurf(SurfNum),TempSurfInTmp(SurfNum),TH(SurfNum,1,1))
              IF(WinHeatGain(SurfNum) >= 0.0d0) THEN
                WinHeatGainRep(SurfNum) = WinHeatGain(SurfNum)
                WinHeatGainRepEnergy(SurfNum) = WinHeatGainRep(SurfNum) * TimeStepZone * SecInHour
              ELSE
                WinHeatLossRep(SurfNum) = -WinHeatGain(SurfNum)
                WinHeatLossRepEnergy(SurfNum) = WinHeatLossRep(SurfNum) * TimeStepZone * SecInHour
              END IF

              TempSurfIn(SurfNum) = TempSurfInTmp(SurfNum)
            END IF
          END IF
        END IF
      END IF    ! ...end of inside surface heat balance equation selection

      TH(SurfNum,1,2)        = TempSurfIn(SurfNum)
      TempSurfInRep(SurfNum) = TempSurfIn(SurfNum)
      TempSurfOut(SurfNum)   = TH(SurfNum,1,1)  ! For reporting

      ! sign convention is positive means energy going into inside face from the air.
      QdotConvInRep(surfNum) = - Surface(SurfNum)%Area * HConvIn(SurfNum) * (TempSurfIn(SurfNum)-RefAirTemp(SurfNum))
      QdotConvInRepPerArea(surfNum) = - HConvIn(SurfNum) * (TempSurfIn(SurfNum)-RefAirTemp(SurfNum))
      QConvInReport(surfNum)          =  QdotConvInRep(surfNum)* SecInHour * TimeStepZone

      ! The QdotConvInRep which is called "Surface Inside Face Convection Heat Gain" is stored during
      ! sizing for both the normal and pulse cases so that load components can be derived later.
      IF (ZoneSizingCalc .AND. CompLoadReportIsReq) THEN
        IF (.NOT. WarmupFlag) THEN
          TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
          IF (isPulseZoneSizing) THEN
            loadConvectedWithPulse(surfNum,TimeStepInDay,CurOverallSimDay) = QdotConvInRep(surfNum)
          ELSE
            loadConvectedNormal(surfNum,TimeStepInDay,CurOverallSimDay) = QdotConvInRep(surfNum)
            netSurfRadSeq(surfNum,TimeStepInDay,CurOverallSimDay) = QdotRadNetSurfInRep(surfNum)
          END IF
        END IF
      END IF

      IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN ! Tubular daylighting device
        ! Tubular daylighting devices are treated as one big object with an effective R value.
        ! The outside face temperature of the TDD:DOME and the inside face temperature of the
        ! TDD:DIFFUSER are calculated with the outside and inside heat balances respectively.
        !
        ! Below, the resulting temperatures are copied to the inside face of the TDD:DOME
        ! and the outside face of the TDD:DIFFUSER for reporting.

        ! Set inside temp variables of TDD:DOME equal to inside temp of TDD:DIFFUSER
        TH(SurfNum2,1,2) = TempSurfIn(SurfNum)
        TempSurfIn(SurfNum2) = TempSurfIn(SurfNum)
        TempSurfInTmp(SurfNum2) = TempSurfIn(SurfNum)
        TempSurfInRep(SurfNum2) = TempSurfIn(SurfNum)

        ! Set outside temp reporting variable of TDD:DOME (since it gets skipped otherwise)
        TempSurfOut(SurfNum2) = TH(SurfNum2,1,1)

        ! Reset outside temp variables of TDD:DIFFUSER equal to outside temp of TDD:DOME
        TH(SurfNum,1,1) = TH(SurfNum2,1,1)
        TempSurfOut(SurfNum) = TH(SurfNum2,1,1)
      END IF

      IF ((TH(SurfNum,1,2) > MaxSurfaceTempLimit) .OR. &
          (TH(SurfNum,1,2) < MinSurfaceTempLimit) ) THEN
        IF (WarmupFlag) WarmupSurfTemp=WarmupSurfTemp+1
        IF (.not. WarmupFlag  .or. (WarmupFlag .and. WarmupSurfTemp > 10) .or. DisplayExtraWarnings) THEN
          IF (TH(SurfNum,1,2) < MinSurfaceTempLimit) THEN
            IF (Surface(SurfNum)%LowTempErrCount == 0) THEN
              CALL ShowSevereMessage('Temperature (low) out of bounds ['//TRIM(RoundSigDigits(TH(SurfNum,1,2),2))//  &
                                 '] for zone="'//trim(Zone(ZoneNum)%Name)//'", for surface="'//TRIM(Surface(SurfNum)%Name)//'"')
              CALL ShowContinueErrorTimeStamp(' ')
              IF (.not. Zone(ZoneNum)%TempOutOfBoundsReported) THEN
                CALL ShowContinueError('Zone="'//trim(Zone(ZoneNum)%Name)//'", Diagnostic Details:')
                IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
                  CALL ShowContinueError('...Internal Heat Gain ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains/Zone(ZoneNum)%FloorArea,3))//'] W/m2')
                ELSE
                  CALL ShowContinueError('...Internal Heat Gain (no floor) ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains,3))//'] W')
                ENDIF
                IF (SimulateAirflowNetwork <= AirflowNetworkControlSimple) THEN
                  CALL ShowContinueError('...Infiltration/Ventilation ['//  &
                         trim(RoundSigDigits(Zone(ZoneNum)%NominalInfilVent,3))//'] m3/s')
                  CALL ShowContinueError('...Mixing/Cross Mixing ['//  &
                         trim(RoundSigDigits(Zone(ZoneNum)%NominalMixing,3))//'] m3/s')
                ELSE
                  CALL ShowContinueError('...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.')
                ENDIF
                IF (Zone(ZoneNum)%isControlled) THEN
                  CALL ShowContinueError('...Zone is part of HVAC controlled system.')
                ELSE
                  CALL ShowContinueError('...Zone is not part of HVAC controlled system.')
                ENDIF
                Zone(ZoneNum)%TempOutOfBoundsReported=.true.
              ENDIF
              CALL ShowRecurringSevereErrorAtEnd('Temperature (low) out of bounds for zone='//trim(Zone(ZoneNum)%Name)//  &
                             ' for surface='//TRIM(Surface(SurfNum)%Name),  &
                             Surface(SurfNum)%LowTempErrCount,ReportMaxOf=TH(SurfNum,1,2),ReportMaxUnits='C',  &
                             ReportMinOf=TH(SurfNum,1,2),ReportMinUnits='C')
            ELSE
              CALL ShowRecurringSevereErrorAtEnd('Temperature (low) out of bounds for zone='//trim(Zone(ZoneNum)%Name)//  &
                             ' for surface='//TRIM(Surface(SurfNum)%Name),  &
                             Surface(SurfNum)%LowTempErrCount,ReportMaxOf=TH(SurfNum,1,2),ReportMaxUnits='C',  &
                             ReportMinOf=TH(SurfNum,1,2),ReportMinUnits='C')
            ENDIF
          ELSE
            IF (Surface(SurfNum)%HighTempErrCount == 0) THEN
              CALL ShowSevereMessage('Temperature (high) out of bounds ('//TRIM(RoundSigDigits(TH(SurfNum,1,2),2))//  &
                                 '] for zone="'//trim(Zone(ZoneNum)%Name)//'", for surface="'//TRIM(Surface(SurfNum)%Name)//'"')
              CALL ShowContinueErrorTimeStamp(' ')
              IF (.not. Zone(ZoneNum)%TempOutOfBoundsReported) THEN
                CALL ShowContinueError('Zone="'//trim(Zone(ZoneNum)%Name)//'", Diagnostic Details:')
                IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
                  CALL ShowContinueError('...Internal Heat Gain ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains/Zone(ZoneNum)%FloorArea,3))//'] W/m2')
                ELSE
                  CALL ShowContinueError('...Internal Heat Gain (no floor) ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains,3))//'] W')
                ENDIF
                IF (SimulateAirflowNetwork <= AirflowNetworkControlSimple) THEN
                  CALL ShowContinueError('...Infiltration/Ventilation ['//  &
                         trim(RoundSigDigits(Zone(ZoneNum)%NominalInfilVent,3))//'] m3/s')
                  CALL ShowContinueError('...Mixing/Cross Mixing ['//  &
                         trim(RoundSigDigits(Zone(ZoneNum)%NominalMixing,3))//'] m3/s')
                ELSE
                  CALL ShowContinueError('...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.')
                ENDIF
                IF (Zone(ZoneNum)%isControlled) THEN
                  CALL ShowContinueError('...Zone is part of HVAC controlled system.')
                ELSE
                  CALL ShowContinueError('...Zone is not part of HVAC controlled system.')
                ENDIF
                Zone(ZoneNum)%TempOutOfBoundsReported=.true.
              ENDIF
              CALL ShowRecurringSevereErrorAtEnd('Temperature (high) out of bounds for zone='//trim(Zone(ZoneNum)%Name)//  &
                             ' for surface='//TRIM(Surface(SurfNum)%Name),  &
                             Surface(SurfNum)%HighTempErrCount,ReportMaxOf=TH(SurfNum,1,2),ReportMaxUnits='C',  &
                             ReportMinOf=TH(SurfNum,1,2),ReportMinUnits='C')
            ELSE
              CALL ShowRecurringSevereErrorAtEnd('Temperature (high) out of bounds for zone='//trim(Zone(ZoneNum)%Name)//  &
                            ' for surface='//TRIM(Surface(SurfNum)%Name),  &
                             Surface(SurfNum)%HighTempErrCount,ReportMaxOf=TH(SurfNum,1,2),ReportMaxUnits='C',  &
                             ReportMinOf=TH(SurfNum,1,2),ReportMinUnits='C')
            ENDIF
          ENDIF
          IF (Zone(ZoneNum)%EnforcedReciprocity) THEN
            IF (WarmupSurfTemp > 3) THEN
              CALL ShowSevereError('CalcHeatBalanceInsideSurf: Zone="'//trim(Zone(ZoneNum)%Name)//  &
                 '" has view factor enforced reciprocity')
              CALL ShowContinueError(' and is having temperature out of bounds errors. Please correct zone geometry and rerun.')
              CALL ShowFatalError('CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.')
            ENDIF
          ELSEIF (WarmupSurfTemp > 10) THEN
              CALL ShowFatalError('CalcHeatBalanceInsideSurf: Program terminates due to preceding conditions.')
          ENDIF
        END IF
      END IF
      IF ((TH(SurfNum,1,2) > MaxSurfaceTempLimitBeforeFatal) .OR. &
          (TH(SurfNum,1,2) < MinSurfaceTempLimitBeforeFatal) ) THEN
        IF (.not. WarmupFlag) THEN
          IF (TH(SurfNum,1,2) < MinSurfaceTempLimitBeforeFatal) THEN
            CALL ShowSevereError('Temperature (low) out of bounds ['//TRIM(RoundSigDigits(TH(SurfNum,1,2),2))//  &
                                 '] for zone="'//trim(Zone(ZoneNum)%Name)//'", for surface="'//TRIM(Surface(SurfNum)%Name)//'"')
            CALL ShowContinueErrorTimeStamp(' ')
            IF (.not. Zone(ZoneNum)%TempOutOfBoundsReported) THEN
              CALL ShowContinueError('Zone="'//trim(Zone(ZoneNum)%Name)//'", Diagnostic Details:')
              IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
                CALL ShowContinueError('...Internal Heat Gain ['//  &
                     trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains/Zone(ZoneNum)%FloorArea,3))//'] W/m2')
              ELSE
                CALL ShowContinueError('...Internal Heat Gain (no floor) ['//  &
                     trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains/Zone(ZoneNum)%FloorArea,3))//'] W')
              ENDIF
              IF (SimulateAirflowNetwork <= AirflowNetworkControlSimple) THEN
                CALL ShowContinueError('...Infiltration/Ventilation ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%NominalInfilVent,3))//'] m3/s')
                CALL ShowContinueError('...Mixing/Cross Mixing ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%NominalMixing,3))//'] m3/s')
              ELSE
                CALL ShowContinueError('...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.')
              ENDIF
              IF (Zone(ZoneNum)%isControlled) THEN
                CALL ShowContinueError('...Zone is part of HVAC controlled system.')
              ELSE
                CALL ShowContinueError('...Zone is not part of HVAC controlled system.')
              ENDIF
              Zone(ZoneNum)%TempOutOfBoundsReported=.true.
            ENDIF
            CALL ShowFatalError('Program terminates due to preceding condition.')
          ELSE
            CALL ShowSevereError('Temperature (high) out of bounds ['//TRIM(RoundSigDigits(TH(SurfNum,1,2),2))//  &
                                 '] for zone="'//trim(Zone(ZoneNum)%Name)//'", for surface="'//TRIM(Surface(SurfNum)%Name)//'"')
            CALL ShowContinueErrorTimeStamp(' ')
            IF (.not. Zone(ZoneNum)%TempOutOfBoundsReported) THEN
              CALL ShowContinueError('Zone="'//trim(Zone(ZoneNum)%Name)//'", Diagnostic Details:')
              IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
                CALL ShowContinueError('...Internal Heat Gain ['//  &
                     trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains/Zone(ZoneNum)%FloorArea,3))//'] W/m2')
              ELSE
                CALL ShowContinueError('...Internal Heat Gain (no floor) ['//  &
                     trim(RoundSigDigits(Zone(ZoneNum)%InternalHeatGains/Zone(ZoneNum)%FloorArea,3))//'] W')
              ENDIF
              IF (SimulateAirflowNetwork <= AirflowNetworkControlSimple) THEN
                CALL ShowContinueError('...Infiltration/Ventilation ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%NominalInfilVent,3))//'] m3/s')
                CALL ShowContinueError('...Mixing/Cross Mixing ['//  &
                       trim(RoundSigDigits(Zone(ZoneNum)%NominalMixing,3))//'] m3/s')
              ELSE
                CALL ShowContinueError('...Airflow Network Simulation: Nominal Infiltration/Ventilation/Mixing not available.')
              ENDIF
              IF (Zone(ZoneNum)%isControlled) THEN
                CALL ShowContinueError('...Zone is part of HVAC controlled system.')
              ELSE
                CALL ShowContinueError('...Zone is not part of HVAC controlled system.')
              ENDIF
              Zone(ZoneNum)%TempOutOfBoundsReported=.true.
            ENDIF
            CALL ShowFatalError('Program terminates due to preceding condition.')
          ENDIF
        END IF
      END IF
    END DO  ! ...end of loop over all surfaces for inside heat balances

          ! Interzone surface updating: interzone surfaces have other side temperatures
          ! which can vary as the simulation iterates through the inside heat
          ! balance.  This block is intended to "lock" the opposite side (outside)
          ! temperatures to the correct value, namely the value calculated by the
          ! inside surface heat balance for the other side.
    DO SurfNum = 1, TotSurfaces
      IF (PRESENT(ZoneToResimulate)) THEN
        IF ((Surface(SurfNum)%Zone /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
          CYCLE ! skip surfaces that are not associated with this zone
        ENDIF
      ENDIF
          ! Interzones must have an exterior boundary condition greater than zero
          ! (meaning that the other side is a surface) and the surface number must
          ! not be the surface itself (which is just a simple partition)
      IF ( (Surface(SurfNum)%ExtBoundCond > 0) .AND. (Surface(SurfNum)%ExtBoundCond /= SurfNum) ) THEN
          ! Set the outside surface temperature to the inside surface temperature
          ! of the interzone pair and reassign the reporting variable.  By going
          ! through all of the surfaces, this should pick up the other side as well
          ! as affect the next iteration.
        TH(SurfNum,1,1)      = TH(Surface(SurfNum)%ExtBoundCond,1,2)
        TempSurfOut(SurfNum) = TH(SurfNum,1,1)
      END IF
    END DO

    InsideSurfIterations = InsideSurfIterations + 1

          ! Convergence check
    MaxDelTemp      = 0.0d0
    DO SurfNum = 1, TotSurfaces   ! Loop through all surfaces to check for convergence...

      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE  ! Skip non-heat transfer surfaces

      IF (PRESENT(ZoneToResimulate)) THEN
        IF ((Surface(SurfNum)%Zone /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
          CYCLE ! skip surfaces that are not associated with this zone
        ENDIF
      ENDIF

      ConstrNum = Surface(SurfNum)%Construction
      IF(Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Opaque surface
        MaxDelTemp = MAX(ABS(TempSurfIn(SurfNum)-TempInsOld(SurfNum)),MaxDelTemp)
        IF (Surface(SurfNum)%HeatTransferAlgorithm  == HeatTransferModel_CondFD) THEN
          ! also check all internal nodes as well as surface faces
          MaxDelTemp = MAX(MaxDelTemp, SurfaceFD(SurfNum)%MaxNodeDelTemp)
        ENDIF
      END IF


    END DO  ! ...end of loop to check for convergence

    IF (.NOT. ANY(HeatTransferAlgosUsed == UseCondFD)) THEN
      IF (MaxDelTemp <= MaxAllowedDelTemp) Converged = .TRUE.
    ELSE
      IF (MaxDelTemp <= MaxAllowedDelTempCondFD) Converged = .TRUE.

!Feb2012      IF ((InsideSurfIterations > IterationsForCondFDRelaxChange) .and. (.NOT. Converged) .AND.   &
!Feb2012          (.NOT. CondFDVariableProperties) ) THEN
!Feb2012          ! adjust relaxation factor down, assume large number of iterations is result of instability
!Feb2012        CondFDRelaxFactor = CondFDRelaxFactor * 0.9d0
!Feb2012        IF (CondFDRelaxFactor < 0.2d0) CondFDRelaxFactor = 0.2d0

  ! resets relaxation factor to speed up iterations when under-relaxatation is not needed.
      IF (InsideSurfIterations <=1) THEN
        CondFDRelaxFactor=CondFDRelaxFactorInput
      ENDIF
      IF ((InsideSurfIterations > IterationsForCondFDRelaxChange) .and. .not. Converged) THEN
          ! adjust relaxation factor down, assume large number of iterations is result of instability
        CondFDRelaxFactor = CondFDRelaxFactor * 0.9d0
        IF (CondFDRelaxFactor < 0.1d0) CondFDRelaxFactor = 0.1d0
      ENDIF

    ENDIF

#ifdef EP_Count_Calls
    NumMaxInsideSurfIterations=MAX(NumMaxInsideSurfIterations,InsideSurfIterations)
#endif

    IF (InsideSurfIterations < MinIterations) Converged = .FALSE.

    IF (InsideSurfIterations > MaxIterations) THEN
      IF (.NOT.WarmupFlag) THEN
        ErrCount=ErrCount+1
        IF (ErrCount < 16) THEN
          IF (.NOT. ANY(HeatTransferAlgosUsed == UseCondFD)) THEN
            CALL ShowWarningError('Inside surface heat balance did not converge '// &
                 'with Max Temp Difference [C] ='//TRIM(RoundSigDigits(MaxDelTemp,3))//  &
                 ' vs Max Allowed Temp Diff [C] ='//TRIM(RoundSigDigits(MaxAllowedDelTemp,3)))
            CALL ShowContinueErrorTimeStamp(' ')
          ELSE
            CALL ShowWarningError('Inside surface heat balance did not converge '// &
                 'with Max Temp Difference [C] ='//TRIM(RoundSigDigits(MaxDelTemp,3))//  &
                 ' vs Max Allowed Temp Diff [C] ='//TRIM(RoundSigDigits(MaxAllowedDelTempCondFD,6)))
            CALL ShowContinueErrorTimeStamp(' ')
          ENDIF
        ELSE
          CALL ShowRecurringWarningErrorAtEnd('Inside surface heat balance convergence problem continues',  &
                                               InsideSurfErrCount,ReportMaxOf=MaxDelTemp,ReportMinOf=MaxDelTemp,  &
                                               ReportMaxUnits='[C]',ReportMinUnits='[C]')
        ENDIF
      ENDIF
      EXIT ! DO loop
    END IF

  END DO    ! ...end of main inside heat balance DO loop (ends when Converged)

  ! Update SumHmXXXX
  IF ( ANY(HeatTransferAlgosUsed == UseCondFD) .OR. &
       ANY(HeatTransferAlgosUsed == UseEMPD) .OR. ANY(HeatTransferAlgosUsed == UseHAMT)) THEN
    DO SurfNum = 1, TotSurfaces
      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces
      IF (Surface(SurfNum)%Class == SurfaceClass_Window) CYCLE

      ZoneNum = Surface(SurfNum)%Zone

      IF (PRESENT(ZoneToResimulate)) THEN
        IF ((ZoneNum /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
          CYCLE ! skip surfaces that are not associated with this zone
        ENDIF
      ENDIF

      IF(Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_HAMT) THEN
         CALL UpdateHeatBalHAMT(SurfNum)

         SumHmAW(ZoneNum)= SumHmAW(ZoneNum)+HMassConvInFD(SurfNum)*Surface(SurfNum)%Area*  &
                           (RhoVaporSurfIn(SurfNum)-RhoVaporAirIn(SurfNum))

         RhoAirZone=PsyRhoAirFnPbTdbW(OutBaroPress,Mat(Surface(SurfNum)%Zone),PsyWFnTdbRhPb(Mat(Surface(SurfNum)%Zone),  &
              PsyRhFnTdbRhov(Mat(Surface(SurfNum)%Zone),RhoVaporAirIn(SurfNum),'RhoAirZone'),OutBaroPress))

         Wsurf=PsyWFnTdbRhPb(TempSurfInTmp(SurfNum), &
              PsyRhFnTdbRhov(TempSurfInTmp(SurfNum),RhoVaporSurfIn(SurfNum),'Wsurf'),OutBaroPress)


         SumHmARa(ZoneNum)= SumHmARa(ZoneNum)+HMassConvInFD(SurfNum)*Surface(SurfNum)%Area*RhoAirZone

         SumHmARaW(ZoneNum)= SumHmARaW(ZoneNum)+HMassConvInFD(SurfNum)*Surface(SurfNum)%Area*RhoAirZone*Wsurf
      ENDIF

      IF (Surface(SurfNum)%HeatTransferAlgorithm == HeatTransferModel_EMPD) THEN
         ! need to calculate the amount of moisture that is entering or
         ! leaving the zone  Qm [kg/sec] = hmi * Area * (Del Rhov)
         ! {Hmi [m/sec];     Area [m2];    Rhov [kg moist/m3]  }
         ! Positive values are into the zone and negative values are
         ! leaving the zone.  SumHmAw is the sum of the moisture entering or
         ! leaving the zone from all of the surfaces and is a rate.  Multiply
         ! by time to get the actual amount affecting the zone volume of air.

         CALL UpdateMoistureBalanceEMPD(SurfNum)
         RhoVaporSurfIn(SurfNum) = MoistEMPDNew(SurfNum)
            !SUMC(ZoneNum) = SUMC(ZoneNum)-MoistEMPDFlux(SurfNum)*Surface(SurfNum)%Area



         SumHmAW(ZoneNum)= SumHmAW(ZoneNum)+HMassConvInFD(SurfNum)*Surface(SurfNum)%Area*  &
                           (RhoVaporSurfIn(SurfNum)-RhoVaporAirIn(SurfNum))
         SumHmARa(ZoneNum)= SumHmARa(ZoneNum)+HMassConvInFD(SurfNum)*Surface(SurfNum)%Area* &
                      PsyRhoAirFnPbTdbW(OutBaroPress,  &
                                        TempSurfInTmp(SurfNum),  &
                                        PsyWFnTdbRhPb(TempSurfInTmp(SurfNum),  &
                                                      PsyRhFnTdbRhovLBnd0C(TempSurfInTmp(SurfNum),  &
                                                                           RhoVaporAirIn(SurfNum)),  &
                                                      OutBaroPress))
         SumHmARaW(ZoneNum)= SumHmARaW(ZoneNum)+HMassConvInFD(SurfNum)*Surface(SurfNum)%Area*RhoVaporSurfIn(SurfNum)
      END IF
    END DO
  ENDIF

! Calculate ZoneWinHeatGain/Loss
  IF (.not. PartialResimulate) THEN
    ZoneWinHeatGain         =0.d0
    ZoneWinHeatGainRep      =0.d0
    ZoneWinHeatGainRepEnergy=0.d0
    ZoneWinHeatLossRep      =0.d0
    ZoneWinHeatLossRepEnergy=0.d0
  ELSE
    ZoneWinHeatGain(ZoneToResimulate)         =0.d0
    ZoneWinHeatGainRep(ZoneToResimulate)      =0.d0
    ZoneWinHeatGainRepEnergy(ZoneToResimulate)=0.d0
    ZoneWinHeatLossRep(ZoneToResimulate)      =0.d0
    ZoneWinHeatLossRepEnergy(ZoneToResimulate)=0.d0
  ENDIF

  DO SurfNum=1,TotSurfaces
    IF(.not. Surface(SurfNum)%ExtSolar) CYCLE        ! WindowManager's definition of ZoneWinHeatGain/Loss
    IF(Surface(SurfNum)%Class /= SurfaceClass_Window) CYCLE
    ZoneNum=Surface(SurfNum)%Zone
    IF (ZoneNum == 0) CYCLE
    IF (PRESENT(ZoneToResimulate)) THEN
      IF ((ZoneNum /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
        CYCLE ! skip surfaces that are not associated with this zone
      ENDIF
    ENDIF
    ZoneWinHeatGain(ZoneNum) = ZoneWinHeatGain(ZoneNum) + WinHeatGain(SurfNum)
  ENDDO
  DO ZoneNum = 1,NumOfZones
    IF ( PRESENT(ZoneToResimulate)  .AND. (ZoneNum /= ZoneToResimulate)) CYCLE
    IF(ZoneWinHeatGain(ZoneNum) >= 0.0d0) THEN
      ZoneWinHeatGainRep(ZoneNum) = ZoneWinHeatGain(ZoneNum)
      ZoneWinHeatGainRepEnergy(ZoneNum) = ZoneWinHeatGainRep(ZoneNum) * TimeStepZone * SecInHour
    ELSE
      ZoneWinHeatLossRep(ZoneNum) = -ZoneWinHeatGain(ZoneNum)
      ZoneWinHeatLossRepEnergy(ZoneNum) = ZoneWinHeatLossRep(ZoneNum) * TimeStepZone * SecInHour
    END IF
  END DO

  IF (PRESENT(ZoneToResimulate)) THEN
    CALL CalculateZoneMRT(ZoneToResimulate) ! Update here so that the proper value of MRT is available to radiant systems
  ELSE
    CALL CalculateZoneMRT ! Update here so that the proper value of MRT is available to radiant systems
  ENDIF

  FirstTime = .FALSE.

  RETURN

END SUBROUTINE CalcHeatBalanceInsideSurf


SUBROUTINE CalcOutsideSurfTemp(SurfNum,ZoneNum,ConstrNum,HMovInsul,TempExt)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   December 1979
          !       MODIFIED       Jun 1990 (RDT for new CTF arrays)
          !                      Jul 2000 (RJL for Moisture algorithms)
          !                      Sep 2000 (RKS for new radiant exchange algorithm)
          !                      Dec 2000 (RKS for radiant system model addition)
          !                      Aug 2010 (BG added radiant heat flow rate reporting)
          !       RE-ENGINEERED  Mar 1998 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs a heat balance on the outside face of each
          ! surface in the building.  NOTE that this also sets some coefficients
          ! that are needed for radiant system modeling.  Thus, it is extremely
          ! important that if someone makes changes to the heat balance equations
          ! at a later date that they must also make changes to the coefficient
          ! setting portion of this subroutine as well.

          ! METHODOLOGY EMPLOYED:
          ! Various boundary conditions are set and additional parameters are set-
          ! up.  Then, the proper heat balance equation is selected based on the
          ! presence of movable insulation, thermal mass of the surface construction,
          ! and convection model being used.

          ! REFERENCES:
          ! (I)BLAST legacy routine HBOUT
          ! 1989 ASHRAE Handbook of Fundamentals (Figure 1 on p. 22.4, convection correlations)

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals
  USE DataInterfaces
  USE DataEnvironment
  USE DataHeatBalFanSys
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataSurfaces
  USE DataMoistureBalance,       ONLY: TempOutsideAirFD,RhoVaporAirOut,RhoVaporAirIn,HConvExtFD,HMassConvExtFD, &
                                       HConvInFD,HMassConvInFD,RhoVaporSurfIn, &
                                       HSkyFD,HGrndFD,HAirFD
!unused0909  USE DataMoistureBalanceEMPD,   ONLY: MoistEMPDNew, MoistEMPDFlux
  USE DataDaylightingDevices
  USE DaylightingDevices,        ONLY: FindTDDPipe
  USE Psychrometrics

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
  INTEGER, INTENT(IN) :: SurfNum      ! Surface number DO loop counter
  INTEGER, INTENT(IN) :: ZoneNum      ! Zone number the current surface is attached to
  INTEGER, INTENT(IN) :: ConstrNum    ! Construction index for the current surface
  REAL(r64), INTENT(IN) :: HMovInsul    ! "Convection" coefficient of movable insulation
  REAL(r64), INTENT(IN) :: TempExt      ! Exterior temperature boundary condition
          ! FUNCTION DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: F1         ! Intermediate calculation variable
  REAL(r64) :: F2         ! Intermediate calculation variable
  LOGICAL :: MovInsulPresent     ! .true. if movable insulation is currently present for surface
  LOGICAL :: QuickConductionSurf ! .true. if the cross CTF term is relatively large
  INTEGER :: PipeNum             ! TDD pipe object number
  INTEGER :: SurfNum2            ! TDD:DIFFUSER object number
  INTEGER :: ZoneNum2            ! TDD:DIFFUSER zone number
  REAL(r64)    :: Ueff                ! 1 / effective R value between TDD:DOME and TDD:DIFFUSER
  REAL(r64)    :: RadTemp             ! local value for Effective radiation temperature for OtherSideConditions model
  REAL(r64)    :: HRad                ! local value for effective (linearized) radiation coefficient

          ! FLOW:


          ! Determine whether or not movable insulation is present
      IF (HMovInsul > 0.0d0) THEN
        MovInsulPresent = .TRUE.
      ELSE
        MovInsulPresent = .FALSE.
      END IF

          ! Determine whether this surface is a "slow conductive" or "quick conductive"
          ! surface.  Designates are inherited from BLAST.  Basically, a "quick" surface
          ! requires the inside heat balance to be accounted for in the heat balance
          ! while a "slow" surface can used the last time step's value for inside
          ! surface temperature.
      IF (Construct(ConstrNum)%CTFCross(0) > 0.01d0) THEN
        QuickConductionSurf = .TRUE.
        F1 = Construct(ConstrNum)%CTFCross(0)/( Construct(ConstrNum)%CTFInside(0)+HConvIn(SurfNum) )
      ELSE
        QuickConductionSurf = .FALSE.
      END IF

          ! Now, calculate the outside surface temperature using the proper heat balance equation.
          ! Each case has been separated out into its own IF-THEN block for clarity.  Additional
          ! cases can simply be added anywhere in the following section.  This is the last step
          ! in the main loop.  Once the proper heat balance is done, the simulation goes on to
          ! the next SurfNum.

      ! Outside heat balance case: Tubular daylighting device
      IF ( Surface(SurfNum)%Class == SurfaceClass_TDD_Dome ) THEN

          ! Lookup up the TDD:DIFFUSER object
          PipeNum = FindTDDPipe(SurfNum)
          SurfNum2 = TDDPipe(PipeNum)%Diffuser
          ZoneNum2 = Surface(SurfNum2)%Zone
          Ueff = 1.0d0 / TDDPipe(PipeNum)%Reff
          F1 = Ueff / (Ueff + HConvIn(SurfNum2))

          ! Similar to opaque surface but inside conditions of TDD:DIFFUSER are used, and no embedded sources/sinks.
          ! Absorbed shortwave radiation is treated similar to a regular window, but only 1 glass layer is allowed.
          !   QRadSWOutAbs(SurfNum) does not apply for TDD:DOME, must use QRadSWwinAbs(SurfNum,1)/2.0 instead.
          TH(SurfNum,1,1) = ( QRadSWwinAbs(SurfNum,1)/2.0d0                     & ! Instead of QRadSWOutAbs(SurfNum)
                             +(HcExtSurf(SurfNum)+HAirExtSurf(SurfNum))*TempExt &
                             +HSkyExtSurf(SurfNum)*SkyTemp                      &
                             +HGrdExtSurf(SurfNum)*OutDryBulbTemp               &  ! ODB used to approx ground surface temp
                             !+Construct(ConstrNum)%CTFSourceOut(0)     &   TDDs cannot be radiant systems
                             ! *QsrcHist(SurfNum,1)                     &
                             +F1*( QRadSWwinAbs(SurfNum2,1)/2.0d0               & ! Use TDD:DIFFUSER surface
                                  +QRadThermInAbs(SurfNum2)                     & ! Use TDD:DIFFUSER surface
                                  !+Construct(ConstrNum)%CTFSourceIn(0) &   TDDs cannot be radiant systems
                                  ! *QsrcHist(SurfNum,1)                &
                                  +HConvIn(SurfNum2)*MAT(ZoneNum2)              & ! Use TDD:DIFFUSER surface and zone
                                  +NetLWRadToSurf(SurfNum2) ) )                 & ! Use TDD:DIFFUSER surface
                            /( Ueff+HcExtSurf(SurfNum)+HAirExtSurf(SurfNum)+    &
                                    HSkyExtSurf(SurfNum)+HGrdExtSurf(SurfNum)-F1*Ueff )

      ! Outside heat balance case: No movable insulation, slow conduction
      ELSE IF ( (.NOT. MovInsulPresent) .AND. (.NOT. QuickConductionSurf) ) THEN
        If ( Surface(SurfNum)%OSCMPtr == 0 ) THEN
          TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                                 &
                           +QRadSWOutAbs(SurfNum)                                      &
                           +(HcExtSurf(SurfNum)+HAirExtSurf(SurfNum))*TempExt          &
                           +HSkyExtSurf(SurfNum)*SkyTemp                               &
                           +HGrdExtSurf(SurfNum)*OutDryBulbTemp                        & ! ODB used to approx ground surface temp
                           +Construct(ConstrNum)%CTFCross(0)*TempSurfIn(SurfNum)       &
                           +Construct(ConstrNum)%CTFSourceOut(0)*QsrcHist(SurfNum,1) ) &
                         /( Construct(ConstrNum)%CTFOutside(0)                         &
                           +HcExtSurf(SurfNum)+HAirExtSurf(SurfNum)+                   &
                            HSkyExtSurf(SurfNum)+HGrdExtSurf(SurfNum) )
      ! Outside Heat Balance case: Other Side Conditions Model
        Else   !( Surface(SurfNum)%OSCMPtr > 0 ) THEN
         ! local copies of variables for clarity in radiation terms
         RadTemp = OSCM(Surface(SurfNum)%OSCMPtr)%TRad
         HRad    = OSCM(Surface(SurfNum)%OSCMPtr)%Hrad

         ! patterned after "No movable insulation, slow conduction," but with new radiation terms and no sun,
         TH(SurfNum,1,1) = ( -CTFConstOutPart(SurfNum)                                 &
                           +HcExtSurf(SurfNum)*TempExt                                 &
                           +HRad*RadTemp                                               &
                           +Construct(ConstrNum)%CTFCross(0)*TempSurfIn(SurfNum)       &
                           +Construct(ConstrNum)%CTFSourceOut(0)*QsrcHist(SurfNum,1) ) &
                         /( Construct(ConstrNum)%CTFOutside(0)                         &
                           +HcExtSurf(SurfNum) + HRad )
        ENDIF
      ! Outside heat balance case: No movable insulation, quick conduction
      ELSE IF ( (.NOT. MovInsulPresent) .AND. (QuickConductionSurf) ) THEN
        If ( Surface(SurfNum)%OSCMPtr == 0 ) THEN
            TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                          &
                               +QRadSWOutAbs(SurfNum)                             &
                               +(HcExtSurf(SurfNum)+HAirExtSurf(SurfNum))*TempExt &
                               +HSkyExtSurf(SurfNum)*SkyTemp                      &
                               +HGrdExtSurf(SurfNum)*OutDryBulbTemp               & ! ODB used to approx ground surface temp
                               +Construct(ConstrNum)%CTFSourceOut(0)              &
                                *QsrcHist(SurfNum,1)                              &
                               +F1*( CTFConstInPart(SurfNum)                      &
                                    +QRadSWInAbs(SurfNum)                         &
                                    +QRadThermInAbs(SurfNum)                      &
                                    +Construct(ConstrNum)%CTFSourceIn(0)          &
                                     *QsrcHist(SurfNum,1)                         &
                                    +HConvIn(SurfNum)*MAT(ZoneNum)                &  ! MAT use here is problem for room air models
                                    +NetLWRadToSurf(SurfNum) ) )                  &
                              /( Construct(ConstrNum)%CTFOutside(0)+              &
                                 HcExtSurf(SurfNum)+HAirExtSurf(SurfNum)+         &
                                HSkyExtSurf(SurfNum)+HGrdExtSurf(SurfNum)         &
                                     -F1*Construct(ConstrNum)%CTFCross(0) )
      ! Outside Heat Balance case: Other Side Conditions Model
        Else   !( Surface(SurfNum)%OSCMPtr > 0 ) THEN
         ! local copies of variables for clarity in radiation terms
         RadTemp = OSCM(Surface(SurfNum)%OSCMPtr)%TRad
         HRad    = OSCM(Surface(SurfNum)%OSCMPtr)%Hrad
         ! patterned after "No movable insulation, quick conduction," but with new radiation terms and no sun,
            TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                 &
                               +HcExtSurf(SurfNum)*TempExt               &
                               +HRad*RadTemp                             &
                               +Construct(ConstrNum)%CTFSourceOut(0)     &
                                *QsrcHist(SurfNum,1)                     &
                               +F1*( CTFConstInPart(SurfNum)             &
                                    +QRadSWInAbs(SurfNum)                &
                                    +QRadThermInAbs(SurfNum)             &
                                    +Construct(ConstrNum)%CTFSourceIn(0) &
                                     *QsrcHist(SurfNum,1)                &
                                    +HConvIn(SurfNum)*MAT(ZoneNum)       & ! MAT use here is problem for room air models
                                    +NetLWRadToSurf(SurfNum) ) )         &
                              /( Construct(ConstrNum)%CTFOutside(0)      &
                                  +HcExtSurf(SurfNum) + HRad             &
                                  -F1*Construct(ConstrNum)%CTFCross(0) )
        ENDIF
      ! Outside heat balance case: Movable insulation, slow conduction
      ELSE IF ( (MovInsulPresent) .AND. (.NOT. QuickConductionSurf) ) THEN

        F2 = HMovInsul/( HMovInsul + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) &
                              + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum) )

        TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                    &
                           +QRadSWOutAbs(SurfNum)                       &
                           +Construct(ConstrNum)%CTFCross(0)            &
                            *TempSurfIn(SurfNum)                        &
                           +F2*( QRadSWOutMvIns(SurfNum)                &
                           +(HcExtSurf(SurfNum)+HAirExtSurf(SurfNum))*TempExt  &
                           +HSkyExtSurf(SurfNum)*SkyTemp               &
                           +HGrdExtSurf(SurfNum)*OutDryBulbTemp ) ) & ! ODB used to approx ground surface temp
                         /( Construct(ConstrNum)%CTFOutside(0)          &
                           +HMovInsul - F2*HMovInsul )

      ! Outside heat balance case: Movable insulation, quick conduction
      ELSE IF ( (MovInsulPresent) .AND. (QuickConductionSurf) ) THEN

        F2 = HMovInsul/( HMovInsul + HcExtSurf(SurfNum) + HAirExtSurf(SurfNum) &
                              + HSkyExtSurf(SurfNum) + HGrdExtSurf(SurfNum) )

        TH(SurfNum,1,1) = (-CTFConstOutPart(SurfNum)                    &
                           +QRadSWOutAbs(SurfNum)                       &
                           +F1*( CTFConstInPart(SurfNum)                &
                                +QRadSWInAbs(SurfNum)                   &
                                +QRadThermInAbs(SurfNum)                &
                                +HConvIn(SurfNum)*MAT(ZoneNum)          &
                                +NetLWRadToSurf(SurfNum) )              &
                           +F2*( QRadSWOutMvIns(SurfNum)                &
                                +(HcExtSurf(SurfNum)+HAirExtSurf(SurfNum))*TempExt  &
                                +HSkyExtSurf(SurfNum)*SkyTemp           &
                                +HGrdExtSurf(SurfNum)*OutDryBulbTemp ) ) & ! ODB used to approx ground surface temp
                         /( Construct(ConstrNum)%CTFOutside(0)          &
                           +HMovInsul - F2*HMovInsul                    &
                           -F1*Construct(ConstrNum)%CTFCross(0) )

      END IF    ! ...end of outside heat balance cases IF-THEN block

   ! multiply out linearized radiation coeffs for reporting
   QdotRadOutRep(SurfNum) = - Surface(SurfNum)%Area &
                            * ( HSkyExtSurf(SurfNum)*(TH(SurfNum,1,1) - SkyTemp ) &
                              +  (HAirExtSurf(SurfNum))*(TH(SurfNum,1,1) - TempExt) &
                              +  HGrdExtSurf(SurfNum)*(TH(SurfNum,1,1) - OutDryBulbTemp) )
   QdotRadOutRepPerArea(SurfNum) =   - (HSkyExtSurf(SurfNum)*(TH(SurfNum,1,1)- SkyTemp ) &
                                   +  (HAirExtSurf(SurfNum))*(TH(SurfNum,1,1) - TempExt) &
                                   +  HGrdExtSurf(SurfNum)*(TH(SurfNum,1,1)- OutDryBulbTemp))
   QRadOutReport(SurfNum) =  QdotRadOutRep(SurfNum) * SecInHour * TimeStepZone
          ! Set the radiant system heat balance coefficients if this surface is also a radiant system
  IF (Construct(ConstrNum)%SourceSinkPresent) THEN

    IF (MovInsulPresent) THEN
          ! Note: if movable insulation is ever added back in correctly, the heat balance equations above must be fixed
      CALL ShowFatalError('Movable insulation is not allowed on a radiant system surface at this time')

    ELSE
      RadSysToHBConstCoef(SurfNum) = (-CTFConstOutPart(SurfNum)  &
                                      +QRadSWOutAbs(SurfNum)     &
                                      +(HcExtSurf(SurfNum)+HAirExtSurf(SurfNum))*TempExt &
                                      +HSkyExtSurf(SurfNum)*SkyTemp              &
                                      +HGrdExtSurf(SurfNum)*OutDryBulbTemp )  & ! ODB used to approx ground surface temp
                                    /(Construct(ConstrNum)%CTFOutside(0)+HcExtSurf(SurfNum)+HAirExtSurf(SurfNum)+  &
                                      HSkyExtSurf(SurfNum)+HGrdExtSurf(SurfNum))

      RadSysToHBTinCoef(SurfNum)   = Construct(ConstrNum)%CTFCross(0) &
                                    /(Construct(ConstrNum)%CTFOutside(0)+HcExtSurf(SurfNum)+HAirExtSurf(SurfNum)+  &
                                      HSkyExtSurf(SurfNum)+HGrdExtSurf(SurfNum))

      RadSysToHBQsrcCoef(SurfNum)  = Construct(ConstrNum)%CTFSourceOut(0) &
                                    /(Construct(ConstrNum)%CTFOutside(0)+HcExtSurf(SurfNum)+HAirExtSurf(SurfNum)+  &
                                      HSkyExtSurf(SurfNum)+HGrdExtSurf(SurfNum))
    END IF

  END IF

  RETURN

END SUBROUTINE CalcOutsideSurfTemp

SUBROUTINE CalcExteriorVentedCavity(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! manages calculating the temperatures of baffle and air cavity for
          ! multi-skin configuration.

          ! METHODOLOGY EMPLOYED:
          ! derived from CalcPassiveTranspiredCollector

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals     , ONLY: SecInHour
  USE DataEnvironment , ONLY: SkyTemp,  SunIsUp, OutBaroPress, OutEnthalpy, IsRain
  USE Psychrometrics  , ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb, PsyWFnTdbTwbPb
  USE DataSurfaces    , ONLY: Surface, ExtVentedCavity, TotExtVentCav, OSCM
!unused0909  USE DataHVACGlobals , ONLY: TimeStepSys
  USE ConvectionCoefficients, ONLY: InitExteriorConvectionCoeff

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: SurfNum ! index of surface

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
    SUBROUTINE CalcPassiveExteriorBaffleGap(SurfPtrARR, VentArea, Cv, Cd, HdeltaNPL, SolAbs, AbsExt, Tilt, AspRat, GapThick, &
                                  Roughness,QdotSource, TsBaffle, TaGap, HcGapRpt,  HrGapRpt,IscRpt , MdotVentRpt, &
                                  VdotWindRpt, VdotBouyRpt)
      USE DataPrecisionGlobals
      INTEGER, INTENT(IN), DIMENSION(:) :: SurfPtrARR  ! Array of indexes pointing to Surface structure in DataSurfaces
      REAL(r64), INTENT(IN)             :: VentArea    ! Area available for venting the gap [m2]
      REAL(r64), INTENT(IN)             :: Cv          ! Oriface coefficient for volume-based discharge, wind-driven [--]
      REAL(r64), INTENT(IN)             :: Cd          ! oriface coefficient for discharge,  bouyancy-driven [--]
      REAL(r64), INTENT(IN)             :: HdeltaNPL   ! Height difference from neutral pressure level [m]
      REAL(r64), INTENT(IN)             :: SolAbs      ! solar absorptivity of baffle [--]
      REAL(r64), INTENT(IN)             :: AbsExt      ! thermal absorptance/emittance of baffle material [--]
      REAL(r64), INTENT(IN)             :: Tilt        ! Tilt of gap [Degrees]
      REAL(r64), INTENT(IN)             :: AspRat      ! aspect ratio of gap  Height/gap [--]
      REAL(r64), INTENT(IN)             :: GapThick    ! Thickness of air space between baffle and underlying surface
      INTEGER, INTENT(IN)               :: Roughness   ! Roughness index (1-6), see DataHeatBalance parameters
      REAL(r64), INTENT(IN)             :: QdotSource  ! Source/sink term, e.g. electricity exported from solar cell [W]
      REAL(r64), INTENT(INOUT)          :: TsBaffle    ! Temperature of baffle (both sides) use lagged value on input [C]
      REAL(r64), INTENT(INOUT)          :: TaGap       ! Temperature of air gap (assumed mixed) use lagged value on input [C]
      REAL(r64), INTENT(OUT), OPTIONAL  :: HcGapRpt       !
      REAL(r64), INTENT(OUT), OPTIONAL  :: HrGapRpt       !
      REAL(r64), INTENT(OUT), OPTIONAL  :: IscRpt
      REAL(r64), INTENT(OUT), OPTIONAL  :: MdotVentRpt
      REAL(r64), INTENT(OUT), OPTIONAL  :: VdotWindRpt
      REAL(r64), INTENT(OUT), OPTIONAL  :: VdotBouyRpt
    END SUBROUTINE CalcPassiveExteriorBaffleGap
  END INTERFACE
          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ! local working variables
  REAL(r64)  :: AspRat      ! Aspect Ratio of gap
  REAL(r64)  :: TmpTscoll
  REAL(r64)  :: TmpTaplen
  REAL(r64)  :: RhoAir
  REAL(r64)  :: holeArea
  REAL(r64)  :: HrPlen
  REAL(r64)  :: HcPlen
  REAL(r64)  :: Isc
  REAL(r64)  :: MdotVent
  REAL(r64)  :: VdotWind
  REAL(r64)  :: VdotThermal
  INTEGER    :: CavNum  ! do loop counter
  INTEGER    :: iter  ! do loop counter
  INTEGER    :: thisOSCM
  REAL(r64)  :: TempExt
  REAL(r64)  :: OutHumRatExt

  CavNum     = Surface(SurfNum)%ExtCavNum

  TempExt    = Surface(SurfNum)%OutDryBulbTemp

  OutHumRatExt = PsyWFnTdbTwbPb(Surface(SurfNum)%OutDryBulbTemp, Surface(SurfNum)%OutWetBulbTemp, OutBaroPress)

  RhoAir     = PsyRhoAirFnPbTdbW(OutBaroPress,TempExt,OutHumRatExt)

  holeArea   = ExtVentedCavity(CavNum)%ActualArea*ExtVentedCavity(CavNum)%Porosity

  AspRat     = ExtVentedCavity(CavNum)%HdeltaNPL * 2.0d0 / ExtVentedCavity(CavNum)%PlenGapThick
  TmpTscoll  = ExtVentedCavity(CavNum)%TbaffleLast
  TmpTaplen  = ExtVentedCavity(CavNum)%TairLast

  ! all the work is done in this routine located in GeneralRoutines.f90

  DO iter =1, 3  ! this is a sequential solution approach.

    CALL CalcPassiveExteriorBaffleGap(ExtVentedCavity(CavNum)%SurfPtrs,holeArea, ExtVentedCavity(CavNum)%Cv, &
                             ExtVentedCavity(CavNum)%Cd,              ExtVentedCavity(CavNum)%HdeltaNPL, &
                             ExtVentedCavity(CavNum)%SolAbsorp,       ExtVentedCavity(CavNum)%LWEmitt, &
                             ExtVentedCavity(CavNum)%Tilt, AspRat,    ExtVentedCavity(CavNum)%PlenGapThick, &
                             ExtVentedCavity(CavNum)%BaffleRoughness, ExtVentedCavity(CavNum)%QdotSource, &
                             TmpTscoll, TmpTaPlen, HcPlen , HrPlen , Isc, MdotVent,VdotWind,VdotThermal )

  ENDDO ! sequential solution
    !now fill results into derived types
  ExtVentedCavity(CavNum)%Isc               = Isc
  ExtVentedCavity(CavNum)%TAirCav           = TmpTaPlen
  ExtVentedCavity(CavNum)%Tbaffle           = TmpTscoll
  ExtVentedCavity(CavNum)%HrPlen            = HrPlen
  ExtVentedCavity(CavNum)%HcPlen            = HcPlen
  ExtVentedCavity(CavNum)%PassiveACH        = (MdotVent/RhoAir) *(1.0d0/(ExtVentedCavity(CavNum)%ProjArea &
                                                                      *ExtVentedCavity(CavNum)%PlenGapThick))*SecInHour
  ExtVentedCavity(CavNum)%PassiveMdotVent   = MdotVent
  ExtVentedCavity(CavNum)%PassiveMdotWind   = VdotWind * RhoAir
  ExtVentedCavity(CavNum)%PassiveMdotTherm  = VdotThermal * RhoAir

    ! now do some updates
  ExtVentedCavity(CavNum)%TairLast      = ExtVentedCavity(CavNum)%TAirCav
  ExtVentedCavity(CavNum)%TbaffleLast   = ExtVentedCavity(CavNum)%Tbaffle

    ! update the OtherSideConditionsModel coefficients.
  thisOSCM  = ExtVentedCavity(CavNum)%OSCMPtr

  OSCM(thisOSCM)%TConv   = ExtVentedCavity(CavNum)%TAirCav
  OSCM(thisOSCM)%HConv   = ExtVentedCavity(CavNum)%HcPlen
  OSCM(thisOSCM)%TRad    = ExtVentedCavity(CavNum)%Tbaffle
  OSCM(thisOSCM)%HRad    = ExtVentedCavity(CavNum)%HrPlen

  RETURN

END SUBROUTINE CalcExteriorVentedCavity

SUBROUTINE GatherComponentLoadsSurfAbsFact
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gather values during sizing used for surface absorption factors

          ! METHODOLOGY EMPLOYED:
          !   Save sequence of values for report during sizing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE DataGlobals, ONLY: HourOfDay, TimeStep, NumOfZones, NumOfTimeStepInHour, CompLoadReportIsReq, isPulseZoneSizing
USE DataSizing, ONLY: CurOverallSimDay
USE DataHeatBalance, ONLY: TMULT,ITABSF
USE DataSurfaces, ONLY: TotSurfaces,Surface,SurfaceClass_TDD_Dome
USE OutputReportTabular, ONLY: ITABSFseq, TMULTseq

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iZone = 0
INTEGER :: jSurf = 0
INTEGER :: TimeStepInDay = 0

IF (CompLoadReportIsReq .AND. .NOT. isPulseZoneSizing) THEN
  TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
  DO iZone = 1, NumOfZones
    TMULTseq(iZone,TimeStepInDay,CurOverallSimDay) = TMULT(iZone)
  END DO
  DO jSurf = 1, TotSurfaces
    iZone = Surface(jSurf)%Zone
    IF (.NOT. Surface(jSurf)%HeatTransSurf .OR. iZone == 0) CYCLE  ! Skip non-heat transfer surfaces
    IF (Surface(jSurf)%Class == SurfaceClass_TDD_Dome) CYCLE  ! Skip tubular daylighting device domes
    ITABSFseq(jSurf,TimeStepInDay,CurOverallSimDay) = ITABSF(jSurf)
  END DO
END IF
END SUBROUTINE GatherComponentLoadsSurfAbsFact


! *****************************************************************************
! *****************************************************************************
! *****************************************************************************
! *****************************************************************************

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
