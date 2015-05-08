MODULE EcoRoofManager
          ! Module containing the heat balance simulation routines
          ! calculation (initialization) routines

          ! MODULE INFORMATION:
          !       AUTHOR         David Sailor and Toan Pham, Portland State University
          !       DATE WRITTEN   Jan 2007
          !       MODIFIED       Oct 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Module for implementing an ecoroof (aka Green Roof)
          !

          ! METHODOLOGY EMPLOYED:
          ! Vikram Madhusudan's Portland State Univ. MS Thesis (Dec 2005) based on FASST model
          ! of Frankenstein and Koenig (2004) - DRDC/CRREL Technical Report TR-04-25.
          ! Precipitation schedules and irrigation schedules can be used to define hourly moisture
          ! inputs (m). Moisture transport updated Oct 2010.
          ! REFERENCES:
          !
          ! OTHER NOTES:

          ! USE STATEMENTS:
! Use statements for data only modules
USE DataPrecisionGlobals
USE DataSurfaces
USE DataGlobals
USE DataLoopNode
USE DataHeatBalance
USE DataWater, ONLY: RainFall ,Irrigation, IrrSchedDesign,IrrSmartSched,RainSchedDesign
USE DataInterfaces
! Use statements for access to subroutines in other modules
USE ConductionTransferFunctionCalc

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE

  ! MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! MODULE VARIABLE DECLARATIONS:

  REAL(r64) :: CumRunoff = 0.0d0  ! Cumulative runoff, updated each time step (m) mult by roof area to get volume
  REAL(r64) :: CumET = 0.0d0      ! Cumulative evapotranspiration from soil and plants (m)
  REAL(r64) :: CumPrecip = 0.0d0
  REAL(r64) :: CumIrrigation = 0.0d0 ! Cumulative irrigation, updated each time step (m) mult by roof area to get volume
  REAL(r64) :: CurrentRunoff
  REAL(r64) :: CurrentET
  REAL(r64) :: CurrentPrecipitation  ! units of (m) per timestep
  REAL(r64) :: CurrentIrrigation     ! units of (m) per timestep

  REAL(r64) :: Tfold     ! leaf temperature from the previous time step
  REAL(r64) :: Tgold     ! ground temperature from the previous time step
  LOGICAL   :: EcoRoofbeginFlag = .TRUE.



PUBLIC CalcEcoRoof  !Algorithm for the module

PRIVATE UpdateSoilProps !Update Moisture this time step


CONTAINS

! MODULE SUBROUTINES:

!*************************************************************************


SUBROUTINE CalcEcoRoof(SurfNum,ZoneNum,ConstrNum,TempExt)
  ! SUBROUTINE INFORMATION
  !     AUTHOR          David Sailor and Toan Pham
  !     DATE WRITTEN    January 2007
  !     MODIFIED        David Sailor - to fix initialization between DD runs and during warm-up
  !     RE-ENGINEERED   na

  ! PURPOSE OF THIS MODULE:

  ! To calculate the heat balance for surfaces with eco roof specified as outside surface
  ! Note that only ONE ecoroof construction can be employed at present time. If multiple
  ! surfaces have ecoroof as the outside layer the energy balance is only calculated for
  ! the first such surface.

  ! METHODOLOGY EMPLOYED:
  ! Vikram Madhusudan's Portland State Univ. MS Thesis (Dec 2005) based on FASST model
  ! of Frankenstein and Koenig (2004) - DRDC/CRREL Technical Report TR-04-25.
  ! Some data used herein are from: European Centre for Medium-Range Weather Forecasts (ECMWF)
  ! IFS Documentation, CY25R1 (April 2002), www.ecmwf.int/research/ifsdocs/CY24r1/Physics/
  ! Physics-08-03.html.
  ! The Atmospheric Boundary Layer - by J.R. Garratt (Cambridge Atmos. & Space Science Series), 316pp.
  !
  USE DataGlobals
  USE DataEnvironment
  USE DataHeatBalFanSys
  USE DataHeatBalance
  USE DataHeatBalSurface
  USE DataSurfaces
!  USE DataDaylightingDevices
!  USE DaylightingDevices,        ONLY: FindTDDPipe
  USE Psychrometrics
  USE ConvectionCoefficients,       ONLY : InitExteriorConvectionCoeff, SetExtConvectionCoeff, SetIntConvectionCoeff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


  !SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: SurfNum            ! Indicator of Surface Number for the current surface
  INTEGER, INTENT(IN)    :: ZoneNum            ! Indicator for zone number where the current surface
  INTEGER, INTENT(INOUT) :: ConstrNum          ! Indicator for contruction index for the current surface
  REAL(r64)   , INTENT(OUT)   :: TempExt            ! Exterior temperature boundary condidtion

  !SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Kv         = 0.4d0         ! Von Karmen's constant (source FASST)
  REAL(r64), PARAMETER  :: rch        = 0.63d0        ! Turbulent Schimdt Number
  REAL(r64), PARAMETER  :: Ks         = 0.2d0         ! W/m.k. Thermal Conductivity of soil
  REAL(r64), PARAMETER  :: rche       = 0.71d0        ! Turbulent Prandtl Number
  REAL(r64), PARAMETER  :: Rv         = 461.53d0      ! Gas Constant of Water Vapor J/kg K
  REAL(r64), PARAMETER  :: Rair       = 0.286d3     ! Gas Constant of air J/Kg K
  REAL(r64), PARAMETER  :: g1         = 9.81d0        ! Gravity. In m/sec^2.
  REAL(r64), PARAMETER  :: Sigma      = 5.6697d-08  ! Stefan-Boltzmann constant W/m^2K^4
  REAL(r64), PARAMETER  :: Cpa        = 1005.6d0      ! Specific heat of Water Vapor. (J/Kg.K)


  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER,SAVE :: FirstEcoSurf=0        ! Indicates lowest numbered surface that is an ecoroof
                                 ! used to determine WHEN to updatesoilProps...
  INTEGER :: EcoLoop             ! an integer loop variable for the simultaneous solution iteration

  REAL(r64)    :: AbsThermSurf        ! Thermal absoptance of the exterior surface
  INTEGER :: RoughSurf           ! Roughness index of the exterior (ecoroof) surface.
  REAL(r64)    :: HMovInsul           ! "Convection" coefficient of movable insulation
!  REAL(r64)    :: HSky                ! "Convection" coefficient from sky to surface
!  REAL(r64)    :: HAir                ! "Convection" coefficient from air to surface (radiation)
!  INTEGER :: OPtr
!  INTEGER :: OSCScheduleIndex    ! Index number for OSC ConstTempSurfaceName

  LOGICAL :: QuickConductionSurf = .FALSE. ! indicator for quick conduction surface
  REAL(r64),SAVE :: LAI       = 0.2d0       ! Leaf area index
  REAL(r64),SAVE :: epsilonf  = 0.95d0      ! Leaf Emisivity
  REAL(r64),SAVE :: epsilong  = 0.95d0      ! Soil Emisivity
  REAL(r64),SAVE :: Alphag    = 0.3d0       ! Ground Albedo
  REAL(r64),SAVE :: Alphaf    = 0.2d0       ! Leaf Albedo (reflectivity to solar radiation)
  REAL(r64) :: e0        = 2.0d0       ! Windless lower limit of exchange coefficient (from FASST docs)
  REAL(r64) :: RH        =50.0d0       ! Relative humidity (%)
  REAL(r64) :: Pa        =101325.d0    ! Atmospheric Pressure (PA)
  REAL(r64),SAVE :: Tg        =10.0d0       ! Ground Surface temperature C ***** FROM PREVIOUS TIME STEP
  REAL(r64),SAVE :: Tf        =10.0d0       ! Leaf temperature C ***** FROM PREVIOUS TIME STEP
  REAL(r64) :: Tgk                   ! Ground temperature in Kelvin
  REAL(r64),SAVE :: Zf        = 0.2d0       ! Height of plants (m)
! DJS Oct 2007 release - note I got rid of the initialization of moisture and meanrootmoisture here as these
! values are now set at beginning of each new DD and each new warm-up loop.
! DJS
  REAL(r64),SAVE :: Moisture         ! m^3/m^3.The moisture content in the soil is the value provided by a user
  REAL(r64),SAVE :: MoistureResidual=0.05d0 ! m^3/m^3. Residual & maximum water contents are unique to each material.
                                ! See Frankenstein et al (2004b) for data.
  REAL(r64),SAVE :: MoistureMax      =0.5d0 ! Maximum volumetric moisture content (porosity) m^3/m^3
  REAL(r64),SAVE :: MeanRootMoisture  ! Mean value of root moisture m^3/m^3
  REAL(r64),SAVE :: SoilThickness    =0.2d0 ! Soil thickness (m)
  REAL(r64),SAVE :: StomatalResistanceMin ! s/m . ! Minimum stomatal resistance is unique for each veg. type.
  REAL(r64) :: f3               =1.0d0 ! As the value of gd for tall grass is 0, then f3 = 1
                                ! ECMWF 2002 CY25R1 report has gd=0.0 for all veg except trees where gd=0.03.

  REAL(r64) :: Ta                    ! current air temperature
  REAL(r64) :: Zog   = 0.001d0         ! Ground roughness length scale (m)
  REAL(r64) :: Za    = 2.0d0           ! Instrument height where atmospheric wind speed is measured (m)
  REAL(r64) :: Ws                    ! Wind Speed (m/s)
  REAL(r64) :: Waf                   ! Windspeed within canopy (m/s)

  REAL(r64) :: Latm                  ! Long Wave Radiation (W/m^2)
  REAL(r64) :: qaf                   ! mixing ratio of air near canopy

  REAL(r64) :: qg                    ! mixing ratio of air at surface.
  REAL(r64),SAVE :: Lf        ! latent heat flux
  REAL(r64),SAVE :: Vfluxf = 0.0d0    ! Water evapotr. rate associated with latent heat from vegetation [m/s]
  REAL(r64) :: RS                    ! shortwave radiation
  REAL(r64) :: Qsoil = 0.0d0           ! heat flux from the soil layer

  REAL(r64) :: EpsilonOne
!unused1208  REAL(r64) :: e
  REAL(r64) :: eair
  REAL(r64) :: Rhoa
  REAL(r64) :: Tak
  REAL(r64) :: qa       ! mixing ratio of air
  REAL(r64) :: Tafk
  REAL(r64) :: Taf
  REAL(r64) :: Rhof
  REAL(r64) :: Rhoaf    ! Average air density
  REAL(r64) :: sigmaf
  REAL(r64) :: Zd       ! zero displacement height (m)
  REAL(r64) :: Zo       ! foliage roughness length (m)
  REAL(r64) :: Cfhn     ! transfer coefficient at near-neutral conditions
  REAL(r64) :: Cf       ! bulk Transfer coefficient, equation 10 page 6 (FASST).
  REAL(r64), SAVE :: sheatf ! sensible heat flux coeff for foliage (W/m^2K)
  REAL(r64), SAVE :: sensiblef !  sensible heat transfer TO foliage (W/m^2) DJS Jan 2011
  REAL(r64) :: ra       ! Aerodynamic Resistance

  REAL(r64)  :: f1inv  ! intermediate calculation variable
  REAL(r64)  :: f2inv ! intermediate calculation variable

  REAL(r64) :: f1       ! intermediate calculation variable
  REAL(r64) :: f2       ! intermediate calculation variable
  REAL(r64) :: r_s      ! Minimum Stomatal Resistance, specific to each plant
  REAL(r64) :: Mg       ! Surface soil moisture content m^3/m^3 (Moisture / MoistureMax)
  REAL(r64) :: dOne     ! intermediate calculation variable
  REAL(r64) :: esf      ! the saturation vapor pressure (Pa)
  REAL(r64) :: qsf      ! Saturation specific humidity at leaf temperature (qfsat)
  REAL(r64) :: Lef      ! Latent heat of vaporation at leaf surface temperature (J/kg)

  REAL(r64) :: Desf     ! Derivative of Saturation vapor pressure
  REAL(r64) :: dqf      ! Derivative of saturation specific humidity
  REAL(r64) :: dqg      ! this is given by Clausius-Clapeyron equation
  REAL(r64) :: esg      ! Saturation vapor pressure (Pa)
  REAL(r64) :: qsg      ! Saturation specific humidity(mixing ratio?) at ground surface temperature
  REAL(r64) :: Leg      ! Latent heat vaporization  at the ground temperature (J/kg)
  REAL(r64) :: Desg     ! derivative of esg Saturation vapor pressure(?)
  REAL(r64) :: F1temp   ! intermediate variable in computing flux from the soil layer
  REAL(r64) :: P1       ! intermediate variable in the equation for Tf
  REAL(r64) :: P2       ! intermediate variable in the equation for Tf and Tg
  REAL(r64) :: P3       ! intermediate variable in the equation for Tf and Tg
  REAL(r64) :: Rhog     ! Density of air at the soil surface temperature
  REAL(r64) :: Rhoag    ! Average density of air with respect to ground surface and air temperature
  REAL(r64) :: Rib      ! Richardson Number
  REAL(r64) :: Chng     ! bulk transfer coefficient near ground
  REAL(r64) :: Ce       ! bulk transfer coefficient (this is in fact Ceg in equation 28 main report)
  REAL(r64) :: Gammah   ! latent heat exchange stability correction factor
  REAL(r64) :: Chg      ! in fact it is the same as Ce (=Ceg) is transfer coefficient (but wot?)
  REAL(r64), SAVE :: sheatg   ! intermediate calculation variable - sensible flux coef (W/m^2K for ground)
  REAL(r64), SAVE :: sensibleg ! sensible heat flux TO ground (w/m^2) DJS Jan 2011
  REAL(r64) :: T3G      ! intermediate variable in the equation for Tg
  REAL(r64) :: T2G      ! intermediate variable in the equation for Tg
  REAL(r64) :: LeafTK ! the current leaf's temperature (Kelvin)
  REAL(r64) :: SoilTK ! the current soil's temperature (Kelvin)
  REAL(r64) :: Chne     ! is similar to near ground bulk transfer coefficient for latent heat flux (at neutral condition)
  REAL(r64) :: Tif      ! previous leaf temperature
  REAL(r64) :: rn       ! rn is the combined effect of both stomatal and aerodynamic resistances
                   ! in fact this is called r'' in the main report
  REAL(r64),SAVE :: Lg  =0.0d0   ! latent heat flux from ground surface
  REAL(r64),SAVE :: Vfluxg = 0.0d0  ! Water evapotr. rate associated with latent heat from ground surface [m/s]
  REAL(r64) :: T1G        ! intermediate variable in the equation for Tg
  REAL(r64) :: Qsoilpart1 ! intermediate variable for evaluating Qsoil (part without the unknown)
  REAL(r64) :: Qsoilpart2 ! intermediate variable for evaluating Qsoil (part coeff of the ground temperature)

!  INTEGER,EXTERNAL :: getnewunitnumber ! external function to return a new (unique) unit for ecoroof writing
  INTEGER :: unit=0
  LOGICAL, SAVE :: MyEnvrnFlag=.true.

  WS=WindspeedAt(Surface(SurfNum)%Centroid%Z)  ! use windspeed at Z of roof
  IF (WS < 2.0d0) THEN    ! Later we need to adjust for building roof height...
    Ws = 2.0d0 ! Set minimum possible wind speed outside vegetation to 2.0 m/s
               ! consistent with FASST TR-04-25 p. x (W' = 2.0)
  END IF

  IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNum = Surface(SurfNum)%StormWinConstruction
  RoughSurf    = Material(Construct(ConstrNum)%LayerPoint(1))%Roughness
  AbsThermSurf = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal
  HMovInsul    = 0.0d0

  IF (Surface(SurfNum)%ExtWind) THEN
     CALL InitExteriorConvectionCoeff(SurfNum,HMovInsul,RoughSurf,AbsThermSurf,TH(SurfNum,1,1), &
            HcExtSurf(SurfNum),HSkyExtSurf(SurfNum),HGrdExtSurf(SurfNum),HAirExtSurf(SurfNum))
  END IF

  RS = BeamSolarRad + AnisoSkyMult(SurfNum)*DifSolarRad

  Latm = 1.0d0 * sigma * 1.0d0* Surface(SurfNum)%ViewFactorGround*( (GroundTempKelvin)**4) &
          + 1.0d0 * sigma * 1.0d0*Surface(SurfNum)%ViewFactorSky*(SkyTempKelvin)**4



  IF (EcoRoofbeginFlag) THEN
    EcoRoofbeginFlag = .FALSE.
    IF (Surface(SurfNum)%HeatTransferAlgorithm /= HeatTransferModel_CTF) &
      CALL ShowWarningError('CalcEcoRoof: EcoRoof simulation but HeatBalanceAlgorithm is not ConductionTransferFunction(CTF).'//  &
         ' Has not been tested under other solution approaches.')
    ! ONLY READ ECOROOF PROPERTIES IN THE FIRST TIME
    Zf      =  Material(Construct(ConstrNum)%LayerPoint(1))%HeightofPlants  ! Plant height (m)
    LAI      = Material(Construct(ConstrNum)%LayerPoint(1))%LAI             ! Leaf Area Index
    Alphag   = 1.0d0 - Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar ! albedo rather than absorptivity
    Alphaf   = Material(Construct(ConstrNum)%LayerPoint(1))%Lreflectivity   ! Leaf Reflectivity
    epsilonf = Material(Construct(ConstrNum)%LayerPoint(1))%Lemissitivity   ! Leaf Emisivity
    stomatalresistancemin = Material(Construct(ConstrNum)%LayerPoint(1))%Rstomata   ! Leaf min stomatal resistance
    epsilong = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermal   ! Soil Emisivity
    MoistureMax =  Material(Construct(ConstrNum)%LayerPoint(1))%porosity   ! Max moisture content in soil
    MoistureResidual =  Material(Construct(ConstrNum)%LayerPoint(1))%MinMoisture   ! Min moisture content in soil
    Moisture =  Material(Construct(ConstrNum)%LayerPoint(1))%InitMoisture   ! Initial moisture content in soil
    MeanRootMoisture = Moisture ! DJS Oct 2007 Release --> all soil at same initial moisture for Reverse DD fix

    SoilThickness = Material(Construct(ConstrNum)%LayerPoint(1))%Thickness ! Total thickness of soil layer (m)

  !DJS - This set of statements and the corresponding write statement in the UpdateSoilProps subroutine should
  !      be removed (or commented out) prior to deployment in a working version of EnergyPlus
  !
  !Open a unit for writing ecoroof specific data to output file (in EnergyPlus directory)
 ! unit=getnewunitnumber()
 ! open(unit,file='ecoroof.txt')

 ! write(unit,*)   " ECOROOF OUTPUT REPORT TRACE - HOURLY "
 ! write(unit,*)   " "
 ! write(unit,91)
 ! 91 FORMAT (" Day Hour Flux T_g  T_f MoistTop MoistRoot CumRain CumET CumRunoff TotalIrr Dens SpecHeat  Cond  Albedo")

    FirstEcoSurf =  Surfnum          ! this determines WHEN to updatesoilProps

! DJS NOVEMBER 2010 - Make calls to SetupOutput Variable to allow for reporting of ecoroof variables

    CALL SetupOutputVariable('Green Roof Soil Temperature [C]',Tg, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Vegetation Temperature [C]',Tf, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Soil Root Moisture Ratio []',MeanRootMoisture, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Soil Near Surface Moisture Ratio []',Moisture, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Soil Sensible Heat Transfer Rate per Area [W/m2]',sensibleg, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Vegetation Sensible Heat Transfer Rate per Area [W/m2]',sensiblef,   &
       'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Vegetation Moisture Transfer Rate [m/s]',vfluxf, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Soil Moisture Transfer Rate [m/s]',vfluxg, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Vegetation Latent Heat Transfer Rate per Area [W/m2]',Lf, 'Zone','State','Environment')
    CALL SetupOutputVariable('Green Roof Soil Latent Heat Transfer Rate per Area [W/m2]',Lg, 'Zone','State','Environment')

    CALL SetupOutputVariable('Green Roof Cumulative Precipitation Depth [m]',CumPrecip, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Cumulative Irrigation Depth [m]',CumIrrigation, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Cumulative Runoff Depth [m]',CumRunoff, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Cumulative Evapotranspiration Depth [m]',CumET, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Current Precipitation Depth [m]',CurrentPrecipitation, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Current Irrigation Depth [m]',CurrentIrrigation, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Current Runoff Depth [m]',CurrentRunoff, 'Zone','Sum','Environment')
    CALL SetupOutputVariable('Green Roof Current Evapotranspiration Depth [m]',CurrentET, 'Zone','Sum','Environment')

! DJS NOVEMBER 2010 - end of calls to setup output of ecoroof variables

  END IF ! Initialization statements for first entry into ecoroof routines


! DJS July 2007
! Make sure the ecoroof module resets its conditions at start of EVERY warmup day and every new design day
! for Reverse DD testing

  IF (BeginEnvrnFlag .or. WarmupFlag) THEN
     Moisture =  Material(Construct(ConstrNum)%LayerPoint(1))%InitMoisture   ! Initial moisture content in soil
     MeanRootMoisture= Moisture  ! Start the root zone moisture at the same value as the surface.
     Alphag   = 1.0d0 - Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar ! albedo rather than absorptivity
  ENDIF
! DJS July 2007


  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    Tgold = OutDryBulbTempAt(Surface(SurfNum)%Centroid%Z)    !OutDrybulbTemp           ! initial guess
    Tfold = OutDryBulbTempAt(Surface(SurfNum)%Centroid%Z)    !OutDrybulbTemp           ! initial guess
    Tg=10.0d0
    Tf=10.0d0
    VFluxf=0.0d0
    VFluxg=0.0d0
    CumRunoff = 0.0d0
    CumET = 0.0d0
    CumPrecip = 0.0d0
    CumIrrigation = 0.0d0
    CurrentRunoff = 0.0d0
    CurrentET = 0.0d0
    CurrentPrecipitation = 0.0d0
    CurrentIrrigation = 0.0d0
    MyEnvrnFlag=.false.
  ENDIF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  ! If current surface is = FirstEcoSurf then for this time step we need to update the soil moisture
  If (Surfnum .eq. FirstEcoSurf) then
    Call UpdateSoilProps(Moisture,MeanRootMoisture,MoistureMax,MoistureResidual,SoilThickness,Vfluxf,Vfluxg,  &
       ConstrNum,Alphag, unit,Tg,Tf,Qsoil)

    Ta = OutDryBulbTempAt(Surface(SurfNum)%Centroid%Z)    ! temperature outdoor - Surface is dry, use normal correlation
    Tg = Tgold
    Tf = Tfold

    IF (Construct(ConstrNum)%CTFCross(0) > 0.01d0) THEN
         QuickConductionSurf = .TRUE.
       F1temp = Construct(ConstrNum)%CTFCross(0)                             &
            /( Construct(ConstrNum)%CTFInside(0)+HConvIn(SurfNum) )
               Qsoilpart1 =  -CTFConstOutPart(SurfNum)                                   &
                     +F1temp*( CTFConstInPart(SurfNum)         &
                           +QRadSWInAbs(SurfNum)               &
                           +QRadThermInAbs(SurfNum)            &
                           +Construct(ConstrNum)%CTFSourceIn(0)*QsrcHist(SurfNum,1) &
                           +HConvIn(SurfNum)*MAT(ZoneNum) &
                           +NetLWRadToSurf(SurfNum) )
    ELSE
       Qsoilpart1 = -CTFConstOutPart(SurfNum)                             &
                 + Construct(ConstrNum)%CTFCross(0)*TempSurfIn(SurfNum)
       F1temp = 0.0d0
    END IF

    Qsoilpart2 = Construct(ConstrNum)%CTFOutside(0) - F1temp*Construct(ConstrNum)%CTFCross(0)

    Pa = StdBaroPress ! standard atmospheric pressure (apparently in Pascals)
    Tgk = Tg + KelvinConv
    Tak = Ta + KelvinConv

     sigmaf = 0.9d0 - 0.7d0*exp(-0.75d0*LAI)  ! Fractional veg cover based on (2) from FASST TR-04-25
                                        ! Formula for grasses modified to incorporate limits from
                                        ! Table 1 for sigmaf_max and min (0.20 to 0.9)

    EpsilonOne   = epsilonf + epsilong - epsilong * epsilonf ! Checked (eqn. 6 in FASST Veg Models)
    RH= OutRelHum ! Get humidity in % from the DataEnvironment.f90
    eair = (RH/100.0d0)*611.2d0*EXP( 17.67d0*Ta/(Tak-29.65d0) )
    qa   = ( 0.622d0*eair)/( Pa-1.000d0*eair ) ! Mixing Ratio of air
    Rhoa = Pa/(Rair*Tak) ! Density of air. kg/m^3
    Tif  = Tf

    ! Air Temperature within the canopy is given as
    ! (Deardorff (1987)). Kelvin. based of the previous temperatures
    Tafk = (1.d0-sigmaf)*Tak + &
         sigmaf*( 0.3d0*Tak+0.6d0*(Tif+KelvinConv)+0.1d0*Tgk)

    Taf = Tafk - KelvinConv    ! Air Temperature within canopy in Celcius (C).
    Rhof= Pa / (Rair*Tafk)     ! Density of air at the leaf temperature
    Rhoaf = (Rhoa + Rhof )/2.0d0 ! Average of air density
    Zd = 0.701d0 * Zf**0.979d0     ! Zero displacement height
    Zo = 0.131d0 * Zf**0.997d0     ! Foliage roughness length. (m) Source Ballick (1981)
    if (zo .lt. 0.02d0) zo = 0.02d0 ! limit based on p.7 TR-04-25 and Table 2

    !transfer coefficient at near-neutral condition Cfhn
    Cfhn = (Kv/LOG((Za - Zd)/Zo))**2   !Equation 12, page 7, FASST Model
    Waf = 0.83d0*Cfhn**0.5d0*sigmaf*Ws + (1.d0- sigmaf)*Ws ! Wind Speed inside foliage. Equation #6, FASST model
    Cf = 0.01d0*(1.0d0+0.3d0/Waf) ! The bulk Transfer coefficient, equation 10 page 6.
    sheatf = e0+1.1d0*LAI*Rhoaf*Cpa*Cf*Waf ! Intermediate calculation for Sensible Heat Transfer
    sensiblef = sheatf*(Taf - Tf) ! DJS Jan 2011 sensible flux TO foliage into air (Frankenstein 2004, eqn7)
    !sourced from Frankenstein et al (2004a). Added e0 windless correction factor.
    !an early version had (1.0-0.7)*sigmaf in calc of sensiblef... how did that get there!?! Fixed.

    !These parameters were taken from "The Atm Boundary Layer", By J.R. Garratt
    !NOTE the Garratt eqn. (A21) gives esf in units of hPA so we have multiplied
    !the constant 6.112 by a factor of 100.
    esf = 611.2d0 * EXP(17.67d0*Tif/(Tif+KelvinConv-29.65d0) )

    ! From Garratt - eqn. A21, p284. Note that Tif and Tif+KelvinConv usage is correct.
    ! Saturation specific humidity at leaf temperature again based on previous temperatures

    qsf = 0.622d0 * esf/(Pa-1.000d0*esf)  ! "The Atm Boundary Layer", J.R Garrat for Saturation mixing ratio
    ! Calculate stomatal resistance and atmospheric resistance Part
    ra =  1.0d0 / (Cf * Waf) ! Aerodynamic Resistance. Resistance that is caused
                         ! by the boundary layer on a leaf surface to transfer water vapor. It is measured in
                         ! s/m and depends on wind speed, leaf's surface roughness,
                         ! and stability of atsmophere.

    f1inv = MIN(1.0d0, (0.004d0*RS + 0.005d0)/(0.81d0 * (0.004d0*RS +1.0d0)))  ! SW radiation-related term
    f1= 1.d0/f1inv
    if (MoistureMax .eq. MoistureResidual) then
      f2inv = 1.0d10
    else
      f2inv = (MeanRootMoisture - MoistureResidual)/(MoistureMax - MoistureResidual) !Equation 19 p. 9 FASST model
    endif

    !In FASST, Eq 20 is used to compute Moisture.

    f2 = 1.d0/f2inv                     ! In dry areas f2 --> LARGE so that r_s --> LARGE
                                        ! and both rn and Latent flux --> 0.0
    f3 = 1.d0/( EXP(-0.0d0*(esf-eair)) )  ! Note the 0.0 here is gd which has value of 0.0
                                        ! for most plants and 0.03 for trees (see ECMWF
                                        ! note given above.
    r_s = StomatalResistanceMin*f1*f2*f3/LAI  !  Stomatal Resistance (r_s)
    rn = ra/(ra+r_s)   !rn is foliage surface wetness ... NOT a resistance

    ! This routine is to calculate ground moisture factor. This factor is from *****
    Mg = moisture / MoistureMax ! m^3/m^3.
    dOne = 1.0d0- sigmaf *(0.6d0*(1.0d0-rn)+0.1d0*(1.0d0-Mg))

    !Latent heat of vaporation at leaf surface temperature. The source of this
    !equation is Henderson-Sellers (1984)
    Lef = 1.91846d6 * ( (Tif+KelvinConv)/(Tif+KelvinConv-33.91d0) )**2
    !Check to see if ice is sublimating or frost is forming.
    if (tfold .lt. 0.0d0 ) Lef= 2.838d6  ! per FASST documentation p.15 after eqn. 37.

    !Derivative of Saturation vapor pressure, which is used in the calculation of
    !derivative of saturation specific humidity.

    desf = 611.2d0*exp(17.67d0*(Tf/(Tf+KelvinConv-29.65d0))) *      &
    ( 17.67d0*Tf*(-1.0d0)*(Tf+KelvinConv-29.65d0)**(-2) + 17.67d0/(KelvinConv-29.65d0 + Tf))
    dqf = ((0.622d0* Pa)/(Pa - esf)**2)*Desf !Derivative of saturation specific humidity
    esg = 611.2d0*EXP( 17.67d0*( Tg/((Tg+KelvinConv)-29.65d0)))       !Pa saturation vapor pressure
    ! From Garratt - eqn. A21, p284.
    ! Note that Tg and Tg+KelvinConv usage is correct.
    qsg = 0.622d0*esg/(Pa-esg) !Saturation mixing ratio at ground surface temperature.

    !Latent heat vaporization  at the ground temperature
    Leg = 1.91846d6*(Tgk/(Tgk-33.91d0))**2
    !Check to see if ice is sublimating or frost is forming.
    if (tgold .lt. 0.0d0 ) Leg= 2.838d6  ! per FASST documentation p.15 after eqn. 37.

    desg = 611.2d0*exp(17.67d0*(Tg/(Tg+KelvinConv-29.65d0))) *      &
       (17.67d0*Tg*(-1.0d0)*(Tg+KelvinConv-29.65d0)**(-2) + 17.67d0/(KelvinConv-29.65d0 + Tg))
    dqg = (0.622d0*Pa/(Pa-esg)**2)*Desg

    !Final Ground Atmosphere Energy Balance
    !Density of air at the soil surface temperature
    Rhog = Pa / (Rair*Tgk)

    !Average density of air with respect to ground surface and air temperature
    Rhoag = (Rhoa + Rhog)/2.d0
    Rib = 2.0d0*g1*Za*(Taf-Tg) / ( (Tafk+Tgk)*Waf**2 )   !Richardson Number

    ! Compute the stability factor Gammah
    IF (Rib < 0.0d0) THEN
      Gammah = (1.0d0-16.0d0*Rib)**(-0.5d0)
    ELSE
      IF (Rib >= 0.19d0) THEN
        Rib = .19d0
      ENDIF
      Gammah = (1.0d0-5.0d0*Rib)**(-0.5d0)
    ENDIF

    if (roughsurf == VerySmooth) then !  6= very smooth, 5=smooth, 4= med. sm. ,3= med. rough. , 2= rough, 1= Very rough
      zog = 0.0008d0
    else if (roughsurf == Smooth) then
      zog = 0.0010d0
    else if (roughsurf == MediumSmooth) then
      zog = 0.0015d0
    else if (roughsurf ==MediumRough) then
      zog = 0.0020d0
    else if (roughsurf ==Rough) then
      zog = 0.0030d0
    else  ! VeryRough
      zog = 0.005d0
    endif

    Chng = (Kv/LOG(Za/Zog))**2 / rch  ! bulk transfer coefficient near ground
    Chg = Gammah* ( (1.d0-sigmaf)*Chng+sigmaf*Cfhn ) !
    sheatg= e0+Rhoag*Cpa*Chg*Waf  ! added the e0 windless correction
    sensibleg = sheatg*(Taf - Tg)  ! sensible flux TO soil (W/m^2) DJS Jan 2011 (eqn. 32 in Frankenstein 2004)

    Chne = (Kv/LOG(Za/Zog))**2 / rche
    Ce = gammah * ((1.d0-sigmaf)*Chne+sigmaf*Cfhn)   ! this is in fact Ceg in eq (28)

    !we can approximate Gammae by Gammah (Supported by FASST Veg Models p. 15)
    qaf = ( (1.0d0-sigmaf)*qa+sigmaf*(0.3d0*qa+0.6d0*qsf*rn+0.1d0*qsg*Mg) )/&
        ( 1.0d0-sigmaf*(0.6d0*(1.d0-rn)+0.1d0*(1.0d0-Mg)) )
    qg = Mg*qsg + (1.0d0-Mg)*qaf !eq main report (13)
    ! According to FASST documentation this is correct.
    ! The following also used Rhoaf and Rhoag respectively... shouldn't these be water densities??!!
    Lf= Lef*LAI*Rhoaf*Cf*Waf*rn*(qaf - qsf) ! This had Leg but should be Lef...
    Lg = Ce*Leg*Waf*Rhoag*(qaf-qg) *Mg      ! In the FASST documentation there is NO Mg. However, in looking
                                            ! back at the Deardorff 1978 paper it appears that an alpha = Mg term is
                                            ! used to distinguish from POTENTIAL and ACTUAL ground surface evaporation...
                                            ! the Lf and Lg calculations are NOT used in this formulation
                                            ! rather, the effects are included in terms of dqg and dqf !
                                            ! These equations for Lf and Lg are based on Deardorff's paper, but there is
                                            ! a sign difference !!! (qsf -qaf) and (qg - qaf) ?!
                                            ! These may be useful, however for telling the UpdateSoilProps routine
                                            ! how much evaporation has taken place...
    Vfluxf= -1.0d0*Lf/Lef/990.0d0               ! water evapotranspire rate [m/s]
    Vfluxg= -1.0d0*Lg/Leg/990.0d0               ! water evapotranspire rate [m/s]
    if (Vfluxf .LT. 0.0d0) Vfluxf = 0.0d0       ! According to FASST Veg. Models p. 11, eqn 26-27, if Qfsat > qaf the actual
    if (Vfluxg .LT. 0.0d0) Vfluxg = 0.0d0       ! evaporative fluxes should be set to zero (delta_c = 1 or 0).

    ! P1, P2, P3 corespond to first, second and third terms of equation 37 in the main report.

    !   Note: the FASST model has a term -gamma_p*(1.0-exp...) in first line for P1 (c1_f) where gamma_p is
    !   a precipitation variable. So, if we assume no precip this term vanishes. We should
    !   revisit this issue later.
    !   Implement an iterative solution scheme to solve the simultaneous equations for Leaf and Soil temperature.
    !   Prior experience suggests that no more than 3 iterations are likely needed
    !
    LeafTK = Tf+KelvinConv
    SoilTK = Tg+KelvinConv

    do ecoloop=1,3
    P1 = sigmaf *( Rs * (1-Alphaf) + epsilonf * Latm ) &
      - 3.d0*sigmaf*epsilonf*epsilong*sigma*(SoilTK)**4/EpsilonOne &
      - 3.d0*( -sigmaf*epsilonf*sigma - sigmaf*epsilonf*epsilong*sigma/EpsilonOne )*(LeafTK)**4 &
      + sheatf*(1.d0-0.7d0*sigmaf)*(Ta+KelvinConv) + LAI *   &
       Rhoaf * Cf * Lef * Waf * rn * ((1.d0 - 0.7d0*sigmaf)/done)*qa + LAI*Rhoaf*Cf*Lef &
      * Waf * rn *(((0.6d0 * sigmaf*rn) / done ) - 1.d0) *( qsf-(LeafTK)*dqf)+LAI   &
      * Rhoaf * Cf * Lef * Waf * rn * ((0.1d0*sigmaf*Mg)/done)*(qsg-(SoilTK)* dqg)
    P2 = 4.0d0*(sigmaf*epsilonf*epsilong*sigma)*(SoilTK)**(3)/EpsilonOne + 0.1d0*sigmaf*sheatf+LAI*Rhoaf*Cf*Lef &
     *Waf*rn*(0.1d0*sigmaf*Mg)/done *dqg
    P3 = 4.0d0* (-sigmaf*epsilonf*sigma - (sigmaf*epsilonf*sigma*epsilong)/EpsilonOne) * (LeafTK)**3 + (0.6d0 * sigmaf - 1.d0) &
       * sheatf + LAI * Rhoaf * Cf * Lef * Waf * rn * (((0.6d0 * sigmaf * rn) / done)-1.0d0) * dqf

    !T1G, T2G, & T3G corresponds to first, second & third terms of equation 38
    !in the main report.
    !  as with the equations for vegetation the first term in the ground eqn in FASST has a
    !  term starting with gamma_p --- if no precip this vanishes. Again, revisit this issue later.

    T1G = (1.d0- sigmaf) * (Rs * (1.d0- Alphag) + epsilong * Latm)-                     &
        (3.0d0 * (sigmaf * epsilonf * epsilong * sigma)  / EpsilonOne) *          &
        (LeafTK)**4 - 3.0d0*(-(1.0d0-sigmaf) * epsilong * sigma           &
        - sigmaf*epsilonf*epsilong*sigma/EpsilonOne)*(SoilTK)**4         &
        + sheatg*(1.d0-0.7d0*sigmaf)*(Ta+KelvinConv)+ Rhoag*Ce*Leg*Waf*Mg*((1.d0 - 0.7d0*sigmaf)/dOne)*qa   &  !finished by T1G
        + Rhoag*Ce*Leg*Waf*Mg*(0.1d0*sigmaf*Mg/dOne - Mg)*(qsg-(SoilTK)*dqg)                 &
        + Rhoag*Ce*Leg*Waf*Mg*(0.6d0*sigmaf*rn/dOne)*(qsf-(LeafTK)*dqf)  + Qsoilpart1 + Qsoilpart2*(KelvinConv)

    T2G = 4.0d0*(-(1.0d0-sigmaf)*epsilong*sigma - sigmaf*epsilonf*epsilong*sigma/EpsilonOne)*(SoilTK)**3   &
      + (0.1d0*sigmaf-1.0d0)*sheatg + Rhoag*Ce*Leg*Waf*Mg*( 0.1d0*sigmaf*Mg/dOne - Mg )*dqg - Qsoilpart2

    T3G = ( 4.0d0*( sigmaf*epsilong*epsilonf*sigma )/EpsilonOne )*(LeafTK)**3 + 0.6d0*sigmaf*sheatg      &
      + Rhoag*Ce*Leg*Waf*Mg*( 0.6d0*sigmaf*rn/dOne ) * dqf

    LeafTK = 0.5d0* (LeafTK+( P1*T2G - P2*T1G )/( - P3*T2G + T3G*P2 ) ) ! take avg of old and new each iteration
    SoilTK = 0.5d0* (SoilTK+( P1*T3G - P3*T1G )/( - P2*T3G + P3*T2G ) ) ! take avg of old and new each iteration
        ! in earlier implementations we simply assigned new Leaf and Soil temps once (no iteration -- ecoloop =1,1, but
        ! with LeafTK = ( P1*T2G - P2*T1G )/( - P3*T2G + T3G*P2 )  and
        ! SoilTK =(SoilTK+( P1*T3G - P3*T1G )/( - P2*T3G + P3*T2G ). This iterative solution was initially attempted to
        ! deal with stability issues associated with the CTF. It has virtually no impact on temperatures and it is not
        ! clear how much it helped with stability problems. Eventually when CTF solution is replaced with a finite
        ! difference scheme this loop structure should be removed.

    end do ! This loop does an iterative solution of the simultaneous equations
    Qsoil = -1.0d0*(Qsoilpart1-Qsoilpart2*(SoilTK-KelvinConv))  ! This is heat flux INTO top of the soil
    Tfold = LeafTK - KelvinConv
    Tgold = SoilTK - KelvinConv

  Endif ! if firstecosurface (if not we do NOT need to recalculate ecoroof energybalance as all ecoroof surfaces MUST be the same
      ! this endif was moved here from the if statement regarding whether we are looking at the first ecoroof surface or not.

  TH(SurfNum,1,1) = Tgold ! SoilTemperature
  TempExt = Tgold

END SUBROUTINE CalcEcoRoof


SUBROUTINE UpdateSoilProps(Moisture,MeanRootMoisture,MoistureMax,MoistureResidual,SoilThickness,Vfluxf,Vfluxg,  &
                           ConstrNum,Alphag, unit,Tg,Tf,Qsoil)
  ! SUBROUTINE INFORMATION
  !     AUTHOR          David Sailor
  !     DATE WRITTEN    Jan 2007
  !     MODIFIED        Stephen Forner, Portland State University (SF); 7/15/2010
  !     RE-ENGINEERED   na

  ! PURPOSE OF THIS MODULE:

  ! Track moisture input/output to ecoroof soil media (precipitation, irrigation, evapotranspiration, runoff)
  ! Update soil thermal properties associated with variations in soil moisture and update CTF calculations
  ! for the ecoroof construction layer.

  ! METHODOLOGY EMPLOYED:
  ! Define 2 soil layers (top and root). Moisture redistribution between top and root layers is proportional
  ! to moisture differences. Input and Output of moisture are partitioned between layers.
  ! Soil thermal properties vary based on non-dimensionalization of experimental data for 8 typical soils.
  ! Specifically, THERMAL PROPERTY = Dry Value + (fraction of moisture content)*Wet Value

  USE DataGlobals
  USE DataInterfaces
  USE DataEnvironment
  USE DataSurfaces
  USE DataWater, ONLY: RainFall
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(INOUT) :: Moisture
  REAL(r64), INTENT(INOUT):: MeanRootMoisture
  REAL(r64), INTENT(IN)    :: MoistureMax
  REAL(r64), INTENT(IN)    :: MoistureResidual
  REAL(r64), INTENT(IN)    :: SoilThickness
  REAL(r64), INTENT(IN)    :: Vfluxf    ! Water mass flux from vegetation [m/s]
  REAL(r64), INTENT(IN)    :: Vfluxg    ! Water mass flux from soil surface [m/s]
  INTEGER, INTENT(INOUT) :: ConstrNum          ! Indicator for contruction index for the current surface
  REAL(r64), INTENT(INOUT) :: Alphag
  INTEGER, INTENT(IN) :: unit    !unused1208
  REAL(r64), INTENT(IN)    :: Tg !unused1208
  REAL(r64), INTENT(IN)    :: Tf !unused1208
  REAL(r64), INTENT(IN)    :: Qsoil !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

  !Soil Parameters from Reference listed in the code:
  REAL(r64), PARAMETER :: alpha=23.0d0   !These parameters are empirical constants
  REAL(r64), PARAMETER :: n=1.27d0     !These parameters are empirical constants
  REAL(r64), PARAMETER :: lambda=0.5d0 !These parameters are empirical constants
  !This is another parameter of the soil which describes the soil conductivity at the saturation point (m/s)
  REAL(r64), PARAMETER ::SoilConductivitySaturation=5.157d-7

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: RatioMax
  REAL(r64) :: RatioMin
  REAL(r64) :: MoistureDiffusion ! moisture transport down from near-surface to root zone
  REAL(r64),SAVE :: TopDepth  ! Thickness of "near-surface" soil layer
  REAL(r64),SAVE :: RootDepth ! Thickness of "root zone" soil layer
                    ! Note TopDepth+RootDepth = thickness of ecoroof soil layer
  REAL(r64),SAVE :: SecondsPerTimeStep  ! Seconds per TimeStep
  REAL(r64) :: SoilConductivity ! Moisture dependent conductivity to be fed back into CTF Calculator
  REAL(r64) :: SoilSpecHeat ! Moisture dependent Spec. Heat to be fed back into CTF Calculator
  REAL(r64) :: SoilAbsorpSolar  ! Moisture dependent Solar absorptance (1-albedo)
  REAL(r64) :: SoilDensity      ! Moisture dependent density to be fed back into CTF Calculator

  REAL(r64) :: SatRatio
  REAL(r64) :: TestRatio        ! Ratio to determine if timestep change in properties is too abrupt for CTF

  REAL(r64),SAVE :: DryCond         ! Dry soil value of conductivity
  REAL(r64),SAVE :: DryDens         ! Dry soil value of density
  REAL(r64),SAVE :: DryAbsorp       ! Dry soil value of solar absorptance (1-albedo)
  REAL(r64),SAVE :: DrySpecHeat     ! Dry soil value of specific heat
  REAL(r64) :: AvgMoisture     ! Average soil moisture over depth of ecoroof media

  LOGICAL,SAVE  :: UpdatebeginFlag = .TRUE.  ! one time flag

  REAL(r64), SAVE ::CapillaryPotentialTop=-3.8997d0 !This variable keeps track of the capillary potential of the soil in both layers and time (m)
  REAL(r64), SAVE ::CapillaryPotentialRoot=-3.8997d0 !This variable keeps track of the capillary potential of the soil in both layers and time (m)
  REAL(r64), SAVE ::SoilHydroConductivityTop=8.72d-6 !This is the soil water conductivity in the soil (m/s)
  REAL(r64), SAVE ::SoilHydroConductivityRoot=8.72d-6 !This is the soil water conductivity in the soil (m/s)
  REAL(r64), SAVE ::SoilConductivityAveTop=8.72d-6 !This is the average soil water conductivity (m/s)
  REAL(r64), SAVE ::SoilConductivityAveRoot=8.72d-6
  REAL(r64), SAVE ::RelativeSoilSaturationTop !Relative Soil Saturation (soil moisture-residual soil moisture)/(saturation soil moisture-residual soil moisture)
  REAL(r64), SAVE ::RelativeSoilSaturationRoot
  REAL(r64) ::TestMoisture=0.15d0 !This makes sure that the moisture cannot change by too much in each step
  INTEGER :: index1
  INTEGER :: ErrIndex=0


  ! NOTE:  As Energyplus calls the energy balance manager (and hence CalcEcoroof)
  ! once for each surface within each zone that has an ecoroof
  ! --- the CALCECOROOF routine is called multiple times within each time step
  ! So, it is important that the UpdateSoilProps subroutine is called ONLY ONCE for each time step!!!

  ! Recall Moisture = near-surface moisture value (m^3/m^3)
  ! Recall MeanRootMoisture = root zone moisture value (m^3/m^3)

    !DJS 2009 set the ratiomax and ratiomin values in the code now (rather than as parameters) so that
  !DJS 2009 we can link them to timesteps and make these limits apply to actual RATES...
  !DJS 2009 reasonable rates are +/- 10% change in properties per 15 minute period... Otherwise we have
  !DJS 2009 stability issues.
  !DJS 2011 FEB - Since we no longer use CTF with soil-dependent properties (Do not RECALL INITCONDUCTION...
  !DJS 2011 FEB - we may be able to get away with NO limits on rates of change when using CFD routine.
  !DJS 2011 FEB - for now we stick with 20% per quarter hour.
   RatioMax = 1.0d0 + 0.20d0*minutespertimestep/15.0d0
   RatioMin = 1.0d0 - 0.20d0*minutespertimestep/15.0d0

  If (UpdatebeginFlag) then

    ! SET dry values that NEVER CHANGE
    DryCond =Material(Construct(ConstrNum)%LayerPoint(1))%Conductivity
    DryDens =Material(Construct(ConstrNum)%LayerPoint(1))%Density
    DryAbsorp  =Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpSolar
    DrySpecHeat =Material(Construct(ConstrNum)%LayerPoint(1))%SpecHeat

    ! DETERMINE RELATIVE THICKNESS OF TWO LAYERS OF SOIL (also unchanging)
    If (SoilThickness .gt. 0.12d0) then
      TopDepth = 0.06d0 ! For now use 6cm as top depth - everything else is root layer
    else
      TopDepth = 0.5d0*SoilThickness ! In unusual case of very thin soil make topdepth half of total
    endif
    !This loop outputs the minimum number of time steps needed to keep the solution stable
    !The equation is minimum timestep in seconds=161240*((number of layers)**(-2.3))*(Total thickness of the soil)**2.07
    IF (Material(Construct(ConstrNum)%LayerPoint(1))%EcoRoofCalculationMethod == 2) THEN
      DO index1=1,20,1
        IF ((REAL(MinutesPerTimeStep/index1,r64)) .LE. (161240.d0*2.d0**(-2.3d0)*(TopDepth+RootDepth)**(2.07d0))/60.d0) EXIT
      END DO
      IF (index1 .GT. 1) THEN
        CALL ShowSevereError('CalcEcoRoof: Too few time steps per hour for stability.')
        CALL ShowContinueError('...Entered Timesteps per hour=['//trim(RoundSigDigits(NumOfTimeStepInHour))//  &
           '], Change to some value greater than ['//trim(RoundSigDigits(60*index1/MinutesPerTimeStep))//']'//  &
           ' for assured stability.')
!      CALL ShowFatalError('Program terminates due to previous condition.')
      END IF
    ENDIF

    RootDepth = SoilThickness-TopDepth
    !Next create a timestep in seconds
    SecondsPerTimeStep=MinutesPerTimeStep*60.d0

    UpdatebeginFlag = .FALSE.
  endif

  CurrentRunoff = 0.0d0 ! Initialize current time step runoff as it is used in several spots below...

  ! FIRST Subtract water evaporated by plants and at soil surface
  Moisture = Moisture - (Vfluxg)*MinutesPerTimeStep*60.0d0/TopDepth  ! soil surface evaporation
  MeanRootMoisture = MeanRootMoisture - (Vfluxf)*MinutesPerTimeStep*60.0d0/RootDepth ! plant extraction from root zone

  ! NEXT Update evapotranspiration summary variable for print out
  CurrentET= (Vfluxg+ Vfluxf)*MinutesPerTimeStep*60.0d0  ! units are meters
  IF (.not. WarmupFlag) THEN
    CumET = CumET + CurrentET
  ENDIF

  ! NEXT Add Precipitation to surface soil moisture variable (if a schedule exists)
  IF (.not. WarmupFlag) THEN
  CurrentPrecipitation = 0.0d0 ! first initialize to zero
  ENDIF
  CurrentPrecipitation = 0.0d0 ! first initialize to zero
  If (Rainfall%ModeID ==RainSchedDesign) then
    CurrentPrecipitation = Rainfall%CurrentAmount !  units of m
    Moisture = Moisture + CurrentPrecipitation/TopDepth  ! x (m) evenly put into top layer
    IF (.not. WarmupFlag) THEN
      CumPrecip = CumPrecip + CurrentPrecipitation
    ENDIF
  Endif

  ! NEXT Add Irrigation to surface soil moisture variable (if a schedule exists)
    CurrentIrrigation = 0.0d0 ! first initialize to zero
    irrigation%actualamount=0.0d0
    If (Irrigation%ModeID == IrrSchedDesign) then
      CurrentIrrigation = irrigation%ScheduledAmount ! units of m
      irrigation%actualamount=CurrentIrrigation
!    elseif (Irrigation%ModeID ==IrrSmartSched .and. moisture .lt. 0.4d0*MoistureMax) then
    elseif (Irrigation%ModeID ==IrrSmartSched .and. moisture .lt. Irrigation%IrrigationThreshold*MoistureMax) then
      ! Smart schedule only irrigates when scheduled AND the soil is less than 40% saturated
      CurrentIrrigation = irrigation%ScheduledAmount ! units of m
      irrigation%actualamount=CurrentIrrigation
    endif

    Moisture = Moisture + CurrentIrrigation/TopDepth ! irrigation in (m)/timestep put into top layer
    IF (.not. WarmupFlag) THEN
      CumIrrigation = CumIrrigation + CurrentIrrigation
    ENDIF

 ! Note: If soil top layer gets a massive influx of rain &/or irrigation some of
 ! the water will simply run right off the top and not penetrate at all!
 ! At the present time this limit is fairly small due to some minor stability issues
 ! in EnergyPlus. If the moisture changes too rapidly the code cannot handle the rapid changes in
 ! surface characteristics and heat fluxes. The result that I've noticed is a non-physical fluctation
 ! in ground surface temperature that oscillates up to 10 deg C from one hour to the next until the
 ! code catches up. The temporary solution is to simply limit how much moisture can enter the soil
 ! in any time step to 0.5"/hour. In the future this might be fixed by running with finer time steps
 ! or by using a finite difference temperature solution rather than the CTF.
 ! I suspect that 15 minute intervals may be needed. Another option is to have an internal moisture
 ! overflow bin that will hold extra moisture and then distribute it in subsequent hours. This way the
 ! soil still gets the same total moisture... it is just distributed over a longer period.
    if (CurrentIrrigation + CurrentPrecipitation .gt. 0.5d0*0.0254d0*MinutesPerTimeStep/60.0d0) then
      CurrentRunoff = CurrentIrrigation+CurrentPrecipitation - (0.5d0*0.0254d0*MinutesPerTimeStep/60.0d0)
      ! If we get here then TOO much moisture has already been added to soil (must now subtract excess)
      Moisture = Moisture - CurrentRunoff/TopDepth  ! currently any incident moisture in excess of 1/4 " per hour
                                                    ! simply runs off the top of the soil.
    endif
   ! Now, if top layer is beyond saturation... the excess simply runs off without penetrating into the lower
   ! layers.
   if (Moisture .gt. MoistureMax) then
     CurrentRunoff = CurrentRunoff + (Moisture-MoistureMax)*TopDepth
     Moisture = MoistureMax
   endif

  IF (Material(Construct(ConstrNum)%LayerPoint(1))%EcoRoofCalculationMethod == 1) THEN

  !THE SECTION BELOW WAS THE INITIAL MOISTURE DISTRIBUTION MODEL.
  !Any line with "!-" was code.  A line with "!" was just a comment.  This is done in case this code needs to be resurected in the future.
  !See below this commented out code for the new moisture distribution model.
  !*********************************************************************************************************
  !*********************************************************************************************************
  ! NEXT Redistribute moisture based on moisture diffusion.
  ! The effective diffusivities should be revisted when better moisture transport data in ecoroof soils are
  ! available.
  ! Here the diffusion rate is in units of [1/s]
  ! A value of 0.0001 would be ~ 36% / hour
  ! A value of 0.00005 would be ~ 18%/hour change in moisture
  ! A value of 0.000025 gives about a 9%/hour change in moisture
  ! a value of 0.0000125 gives 5%/hour...
  ! Note: This formulation allows moisture to have a directional bias - ie, it can sink into soil easier than
  ! it can be brought up.
    If (Moisture .GT. MeanRootMoisture) then
    ! move moisture from top layer into root zone
      MoistureDiffusion = min((MoistureMax-MeanRootMoisture)*RootDepth, (Moisture-MeanRootMoisture)*TopDepth)
      MoistureDiffusion = max(0.0d0,MoistureDiffusion) ! Safety net to keep positive (not needed?)
      ! at this point moistureDiffusion is in units of (m)/timestep
      MoistureDiffusion= 0.00005d0*MinutesPerTimeStep*60.0d0*MoistureDiffusion
      Moisture = Moisture - MoistureDiffusion/TopDepth
      MeanRootMoisture = MeanRootMoisture + MoistureDiffusion/RootDepth
    else if (MeanRootMoisture .GT. Moisture) then
    ! move moisture to top layer from root zone
      MoistureDiffusion = min((MoistureMax-Moisture)*TopDepth, (MeanRootMoisture-Moisture)*RootDepth)
      MoistureDiffusion = max(0.0d0,MoistureDiffusion) ! Safety net (not needed?)
    ! at this point moistureDiffusion is in units of (m)/timestep
      MoistureDiffusion= 0.00001d0*MinutesPerTimeStep*60.0d0*MoistureDiffusion
      Moisture = Moisture + MoistureDiffusion/TopDepth
      MeanRootMoisture = MeanRootMoisture - MoistureDiffusion/RootDepth
    endif
  ELSE
  !********************************************************************************************************
  !********************************************************************************************************
  !THE SECTION ABOVE WAS THE MOISTURE DISTRIBUTION MODEL. REPLACED SF-7/21/2010


  !NEXT redistribute the moisture in the soil based on:
  !Marcel G Schaap and Martinus Th van Genuchten, 2006, 'A modified Maulem-van
  !Genuchten Formulation for Improved Description of the Hydraulic Conductivity Near Saturation’.
  !Written in MATLAB by Vishal Sharma (of Portland State) and modified for FORTRAN by Stephen Forner Summer 2010
  !This model is based on curve fit data that describes the capillary motion of the water in combination with the gravitational
  !forces on the water.
  !This set of equations is unstable if the time step is larger than given by a curve fit equation.  This first DO loop is to
  !see how many time steps are needed to be stable.
  !This method of moisture distribution relies on variables which are experimentally determined: alpha, lambda, n and the
  !hydraulic conductivity at saturation.

  !Now, solve for the soil parameters for  of the top soil layer

    RelativeSoilSaturationTop=(Moisture-MoistureResidual)/(MoistureMax-MoistureResidual)
    IF (RelativeSoilSaturationTop < 0.0001d0) THEN
      IF (ErrIndex == 0) THEN
        CALL ShowWarningMessage('EcoRoof: UpdateSoilProps: Relative Soil Saturation Top Moisture <= 0.0001, Value=['// &
          trim(RoundSigDigits(RelativeSoilSaturationTop,5))//'].')
        CALL ShowContinueError('Value is set to 0.0001 and simulation continues.')
        CALL ShowContinueError('You may wish to increase the number of timesteps to attempt to alleviate the problem.')
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('EcoRoof: UpdateSoilProps: Relative Soil Saturation Top Moisture < 0. continues', &
        ErrIndex,ReportMaxOf=RelativeSoilSaturationTop,ReportMinOf=RelativeSoilSaturationTop)
      RelativeSoilSaturationTop=0.0001d0
    ENDIF
    SoilHydroConductivityTop=SoilConductivitySaturation*(RelativeSoilSaturationTop**lambda)*  &
       (1.d0-(1.d0-(RelativeSoilSaturationTop)**(n/(n-1.d0)))**((n-1.d0)/n))**2
    CapillaryPotentialTop=(-1.d0/alpha)*((((1.d0/RelativeSoilSaturationTop)**(n/(n-1.d0)))-1.d0)**(1.d0/n))

    !Then the soil parameters for the root soil layer
    RelativeSoilSaturationRoot=(MeanRootMoisture-MoistureResidual)/(MoistureMax-MoistureResidual)
    SoilHydroConductivityRoot=SoilConductivitySaturation*(RelativeSoilSaturationRoot**lambda)*  &
       (1.d0-(1.d0-(RelativeSoilSaturationRoot)**(n/(n-1.d0)))**((n-1.d0)/n))**2
    CapillaryPotentialRoot=(-1.d0/alpha)*((((1.d0/RelativeSoilSaturationRoot)**(n/(n-1.d0)))-1.d0)**(1.d0/n))

    !Next, using the soil parameters, solve for the soil moisture
    SoilConductivityAveTop=(SoilHydroConductivityTop+SoilHydroConductivityRoot)*0.5d0
    Moisture=Moisture+(SecondsPerTimeStep/TopDepth)*((SoilConductivityAveTop*(CapillaryPotentialTop&
    -CapillaryPotentialRoot)/TopDepth)-SoilConductivityAveTop)

    !Now limit the soil from going over the moisture maximum and takes excess to create runoff
    IF(Moisture .GE. MoistureMax) THEN !This statement makes sure that the top layer is not over the moisture maximum for the soil.
         Moisture=0.9999d0*MoistureMax   !then it takes any moisture over the maximum amount and makes it runoff
         CurrentRunOff=CurrentRunOff+(Moisture-MoistureMax*0.9999d0)*TopDepth
    END IF

    !Now make sure that the soil does not go below the moisture minimum
    IF(Moisture .LE. (1.01d0*MoistureResidual)) THEN
         Moisture=1.01d0*MoistureResidual
    END IF

    !Next, solve the parameters for the bottom layer
    SoilConductivityAveRoot=SoilHydroConductivityRoot

    !Now make sure the rate of liquid leaving the soil is more than one drop per hour
    IF((SoilConductivityAveRoot*3600.d0) .LE. (2.33d-7)) THEN
         SoilConductivityAveRoot=0.0d0
    END IF


    !Using the parameters above, distribute the Root Layer moisture
    TestMoisture=MeanRootMoisture
    MeanRootMoisture=MeanRootMoisture+(SecondsPerTimeStep/RootDepth)*  &
       ((SoilConductivityAveTop*(CapillaryPotentialTop-CapillaryPotentialRoot)  &
          /RootDepth)+SoilConductivityAveTop-SoilConductivityAveRoot)

    !Limit the moisture from going over the saturation limit and create runoff:
    IF(MeanRootMoisture .GE. MoistureMax) THEN
         MeanRootMoisture=0.9999d0*MoistureMax
         CurrentRunOff=CurrentRunOff+(Moisture-MoistureMax*0.9999d0)*RootDepth
    END IF

    !Limit the soil from going below the soil saturation limit:
    IF(MeanRootMoisture .LE. (1.01d0*MoistureResidual)) THEN
         MeanRootMoisture=1.01d0*MoistureResidual
    END IF

    !Next, track runoff from the bottom of the soil:
    CurrentRunOff=CurrentRunOff+SoilConductivityAveRoot*SecondsPerTimeStep

   !~~~END SF EDITS
  ENDIF

  ! NEXT Limit moisture values to saturation (create RUNOFF that we can track)
  ! CurrentRunoff is sum of "overwatering" in a timestep and excess moisture content
     IF (.not. WarmupFlag) THEN
       CumRunoff = CumRunoff + CurrentRunoff
     ENDIF

  if (MeanRootMoisture .LE. MoistureResidual*1.00001d0) then
    Moisture = Moisture - (MoistureResidual*1.00001d0 - MeanRootMoisture)*RootDepth/TopDepth
    ! If the plant has extracted more moisture than is in the root zone soil, then make it come from
    ! the top layer rather than the root zone... unless top is also dry...
    if (Moisture .lt. MoistureResidual*1.00001d0) Moisture = MoistureResidual*1.00001d0
    MeanRootMoisture = MoistureResidual*1.00001d0   ! Need to make sure we do not divide by zero later.
  endif

    ! ------------------------------------------------------------------------------------------
    ! Having updated moisture values now we modify soil thermal properties based on moisture content

    ! NOTE: Variables SoilAbsorpSolar, SoilDensity, SoilSpecHeat, and SoilConductivity are the values
    ! that the soil properties OUGHT to attain for the current moisture level. These values are then
    ! moderated using the TestRatio variable so that from one time step to the next no variable
    ! changes by more than a certain percentage (typically 5-20%).

    ! Note wet soil absorptance is generally 25-50% higher than dry soil absorptance (assume linear)
  SoilAbsorpSolar = DryAbsorp+ (0.92d0-DryAbsorp)*(Moisture -MoistureResidual)/(MoistureMax - MoistureResidual)
  ! Limit solar absorptivity to 95% so soil abledo is always above 5%
  if (SoilAbsorpSolar .GT. 0.95d0 ) SoilAbsorpSolar = 0.95d0
  ! Limit solar absorptivity to greater than 20% so that albedo is always less than 80%
  if (SoilAbsorpSolar .LT. 0.20d0) SoilAbsorpSolar = 0.20d0

    ! Need to use for albedo in CalcEcoroof
  TestRatio = (1.0d0-SoilAbsorpSolar)/Alphag
  If (TestRatio .GT. RatioMax) TestRatio = RatioMax
  If (TestRatio .LT. RatioMin) TestRatio = RatioMin
  Alphag = TestRatio*Alphag  !  included 1.0 - to make it albedo rather than SW absorptivity
    !
    ! Note wet soil density is calculated by simply adding the mass of water...
    AvgMoisture = (RootDepth*MeanRootMoisture+TopDepth*Moisture)/SoilThickness
    SoilDensity = DryDens + (AvgMoisture-MoistureResidual)*990.0d0
    ! Note 990 kg/m^3 is water density and the moisture is depth-averaged

  ! Note wet soil has specific heat that is 40% higher than dry soil (assume linear)
! OLD ::  SoilSpecHeat = DrySpecHeat*(1.0d0+ 0.4d0*(AvgMoisture-MoistureResidual)/(MoistureMax-MoistureResidual))
  ! This is now based on Melos Hagos's results for C (March 2009)
  !    SoilSpecHeat = DrySpecHeat + 3.09*(AvgMoisture) CLEARLY in ERROR BY FACTOR of 1000
  !    DJS - Melos report has Spec = Cdry + 1.9 theta (where C is in kJ/kg/K), so...
    SoilSpecHeat = DrySpecHeat + 1900.0d0*AvgMoisture

  ! Note wet soil has thermal conductivity that is up to 3 times that of  dry soil ...
  ! For now simply let it DOUBLE over the range of moisture

  ! Note wet soil has thermal conductivity that is up to 3 times that of  dry soil ...
  ! OLD :: SoilConductivity = DryCond* (1.0d0 + 1.0d0 * (AvgMoisture-MoistureResidual)/(MoistureMax-MoistureResidual))
  ! This is now based on Melos Hagos's results for k/kdry (March 2009)
    SatRatio = (AvgMoisture - MoistureResidual)/(MoistureMax - MoistureResidual)
    SoilConductivity = (DryCond/1.15d0) * (1.45d0 * exp(4.411d0 * SatRatio))/(1.0d0 + 0.45d0 * exp(4.411d0 * SatRatio))
  ! DJS 2009 - note, this allows the actual conductivity to dip a little below the dry value... corresponding to
  ! DJS 2009 - "bone dry" if you will, when moisture --> residual value.

  ! HERE WE RE-RUN THE CONDUCTION TRANSFER FUNCTION (CTF) CALCULATIONS

  ! NOTE: CTF uses the original Material( )%xxx variables, so must update these for current construction and
  !       time step...
  ! TestRatio variable is available just in case there are stability issues. If so, we can limit the amount
  ! by which soil properties are allowed to vary in one time step (10% in example below).

    TestRatio = SoilConductivity/Material(Construct(ConstrNum)%LayerPoint(1))%Conductivity
     if (TestRatio .GT. RatioMax) TestRatio=RatioMax
     if (TestRatio .LT. RatioMin) TestRatio=RatioMin
     Material(Construct(ConstrNum)%LayerPoint(1))%Conductivity = TestRatio*Material(Construct(ConstrNum)%LayerPoint(1))%Conductivity
     SoilConductivity =Material(Construct(ConstrNum)%LayerPoint(1))%Conductivity

    TestRatio = SoilDensity/Material(Construct(ConstrNum)%LayerPoint(1))%Density
     if (TestRatio .GT. RatioMax) TestRatio=RatioMax
     if (TestRatio .LT. RatioMin) TestRatio=RatioMin
    Material(Construct(ConstrNum)%LayerPoint(1))%Density = Material(Construct(ConstrNum)%LayerPoint(1))%Density*TestRatio
    SoilDensity =Material(Construct(ConstrNum)%LayerPoint(1))%Density

    TestRatio = SoilSpecHeat/Material(Construct(ConstrNum)%LayerPoint(1))%SpecHeat
     if (TestRatio .GT. RatioMax) TestRatio=RatioMax
     if (TestRatio .LT. RatioMin) TestRatio=RatioMin
    Material(Construct(ConstrNum)%LayerPoint(1))%SpecHeat = Material(Construct(ConstrNum)%LayerPoint(1))%SpecHeat*TestRatio
    SoilSpecHeat =Material(Construct(ConstrNum)%LayerPoint(1))%SpecHeat

  ! Now call InitConductionTransferFunction with the ConstrNum as the argument. As long as the argument is
  ! non-zero InitConductionTransferFunction will ONLY update this construction. If the argument is 0 it will
  ! rerun the ENTIRE InitConductionTransferFunction on all constructions (as in initial code start-up mode).



  ! DJS The following works for most simulations, but has stability issues in some cases.
  ! DJS - in the present version it seems best to NOT update soil thermal properties.
  !   Call InitConductionTransferFunctions(ConstrNum)
  ! DJS In future revision we will implement these modified soil thermal properties in the finite difference
  ! solution scheme.

  ! DJS - Note the following write/format statements should be commented out prior to releasing within EnergyPlus
  ! DJS - they are handy in doing hourly validation/evaluations, though, so leave them in for future development.

  !   write(unit,799) DayofYear, HourofDay, Qsoil,Tg, Tf, Moisture, MeanRootMoisture,CumPrecip &
  !  ,CumET,CumRunoff, CumIrrigation, SoilDensity, SoilSpecHeat,SoilConductivity,Alphag
  !799 format(' ',I3,' ',I3,' ',' ',f9.3,' ',f6.2,' ',f6.2,' ',f5.3,' ',f5.3,' ',f6.4, '  '  &
  !    f7.3, ' ', f7.3, ' ',f7.3, ' ',f6.1,' ',f7.1,'  ',f6.3,'  ',f6.2)

END SUBROUTINE UpdateSoilProps


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
END MODULE EcoRoofManager