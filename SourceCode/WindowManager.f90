MODULE WindowManager

          ! MODULE INFORMATION
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   September 1999
          !       MODIFIED       August 2001 (FW): add window shade thermal calculation;
          !                                        add window blind optical and thermal model.
          !                      February 2003 (FW/LKL): Name changed to WindowManager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Manages the window optical and thermal calculations derived
          ! from WINDOW 4 and WINDOW 5.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! WINDOW 4:
          ! D.Arasteh, M.Reilly and M.Rubin. A versative procedure for
          ! calculating heat transfer through windows. ASHRAE Trans. 1989, Vol. 95, Pt. 2.

          ! E.Finlayson, D.Arasteh, C.Huizenga, M.Rubin, and M.Reilly. WINDOW 4.0:
          ! Documentation of calculation procedures. LBL-33943. July 1993.

          ! WINDOW 5:
          ! ASHRAE Standard 142P (draft 1/13/98): Standard method for determining and expressing
          ! the heat transfer and total optical properties of fenestration products.

          ! Shade and blind thermal model:
          ! ISO/DIS 15099, Thermal Performance of Windows, Doors and Shading Devices,
          ! Detailed Calculations, 1/12/00.

          ! Blind optical model:
          ! H. Simmler, U. Fischer and Frederick Winkelmann, Solar-Thermal Window Blind Model
          ! for DOE-2, Lawrence Berkeley National Laboratory, Jan. 1996.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataEnvironment
USE DataHeatBalance
USE DataHeatBalFanSys
USE DataGlobals
USE DataSurfaces
USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE

          !MODULE PARAMETER DEFINITIONS:

REAL(r64), PRIVATE, PARAMETER :: sigma=5.6697d-8    ! Stefan-Boltzmann constant
REAL(r64),  PARAMETER :: TKelvin=KelvinConv  ! conversion from Kelvin to Celsius
INTEGER, PRIVATE, PARAMETER :: nume=107           ! Number of wavelength values in solar spectrum
INTEGER, PRIVATE, PARAMETER :: numt3=81           ! Number of wavelength values in the photopic response
!INTEGER, PRIVATE :: nume=107           ! Number of wavelength values in solar spectrum
!INTEGER, PRIVATE :: numt3=81           ! Number of wavelength values in the photopic response

REAL(r64),    PRIVATE, PARAMETER, DIMENSION(8) :: AirProps=  &
!               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
             (/ 1.29d0, -0.4d-2, 2.41d-2, 7.6d-5, 1.73d-5, 1.0d-7, 0.72d0,   1.8d-3  /)
! Air mass 1.5 terrestrial solar global spectral irradiance (W/m2-micron)
! on a 37 degree tilted surface; corresponds
! to wavelengths (microns) in following data block (ISO 9845-1 and ASTM E 892;
! derived from Optics5 data file ISO-9845GlobalNorm.std, 10-14-99)
!REAL(r64), PRIVATE, PARAMETER, DIMENSION(nume) :: wle= &   ! Solar spectrum wavelength values (microns)
REAL(r64), PRIVATE, DIMENSION(nume) :: wle= &   ! Solar spectrum wavelength values (microns)
    (/0.3000d0,0.3050d0,0.3100d0,0.3150d0,0.3200d0,0.3250d0,0.3300d0,0.3350d0,0.3400d0,0.3450d0, &
      0.3500d0,0.3600d0,0.3700d0,0.3800d0,0.3900d0,0.4000d0,0.4100d0,0.4200d0,0.4300d0,0.4400d0, &
      0.4500d0,0.4600d0,0.4700d0,0.4800d0,0.4900d0,0.5000d0,0.5100d0,0.5200d0,0.5300d0,0.5400d0, &
      0.5500d0,0.5700d0,0.5900d0,0.6100d0,0.6300d0,0.6500d0,0.6700d0,0.6900d0,0.7100d0,0.7180d0, &
      0.7244d0,0.7400d0,0.7525d0,0.7575d0,0.7625d0,0.7675d0,0.7800d0,0.8000d0,0.8160d0,0.8237d0, &
      0.8315d0,0.8400d0,0.8600d0,0.8800d0,0.9050d0,0.9150d0,0.9250d0,0.9300d0,0.9370d0,0.9480d0, &
      0.9650d0,0.9800d0,0.9935d0,1.0400d0,1.0700d0,1.1000d0,1.1200d0,1.1300d0,1.1370d0,1.1610d0, &
      1.1800d0,1.2000d0,1.2350d0,1.2900d0,1.3200d0,1.3500d0,1.3950d0,1.4425d0,1.4625d0,1.4770d0, &
      1.4970d0,1.5200d0,1.5390d0,1.5580d0,1.5780d0,1.5920d0,1.6100d0,1.6300d0,1.6460d0,1.6780d0, &
      1.7400d0,1.8000d0,1.8600d0,1.9200d0,1.9600d0,1.9850d0,2.0050d0,2.0350d0,2.0650d0,2.1000d0, &
      2.1480d0,2.1980d0,2.2700d0,2.3600d0,2.4500d0,2.4940d0,2.5370d0 /)

REAL(r64), PRIVATE, DIMENSION(nume) :: e=  &  ! Solar spectrum values corresponding to wle
    (/   0.0d0,   9.5d0,  42.3d0, 107.8d0, 181.0d0, 246.0d0, 395.3d0, 390.1d0, 435.3d0, 438.9d0, &
       483.7d0, 520.3d0, 666.2d0, 712.5d0, 720.7d0,1013.1d0,1158.2d0,1184.0d0,1071.9d0,1302.0d0, &
      1526.0d0,1599.6d0,1581.0d0,1628.3d0,1539.2d0,1548.7d0,1586.5d0,1484.9d0,1572.4d0,1550.7d0, &
      1561.5d0,1501.5d0,1395.5d0,1485.3d0,1434.1d0,1419.9d0,1392.3d0,1130.0d0,1316.7d0,1010.3d0, &
      1043.2d0,1211.2d0,1193.9d0,1175.5d0, 643.1d0,1030.7d0,1131.1d0,1081.6d0, 849.2d0, 785.0d0, &
       916.4d0, 959.9d0, 978.9d0, 933.2d0, 748.5d0, 667.5d0, 690.3d0, 403.6d0, 258.3d0, 313.6d0, &
       526.8d0, 646.4d0, 746.8d0, 690.5d0, 637.5d0, 412.6d0, 108.9d0, 189.1d0, 132.2d0, 339.0d0, &
       460.0d0, 423.6d0, 480.5d0, 413.1d0, 250.2d0,  32.5d0,   1.6d0,  55.7d0, 105.1d0, 105.5d0, &
       182.1d0, 262.2d0, 274.2d0, 275.0d0, 244.6d0, 247.4d0, 228.7d0, 244.5d0, 234.8d0, 220.5d0, &
       171.5d0,  30.7d0,   2.0d0,   1.2d0,  21.2d0,  91.1d0,  26.8d0,  99.5d0,  60.4d0,  89.1d0, &
        82.2d0,  71.5d0,  70.2d0,  62.0d0,  21.2d0,  18.5d0,   3.2d0 /)

! Phototopic response function and corresponding wavelengths (microns)
! (CIE 1931 observer; ISO/CIE 10527, CIE Standard Calorimetric Observers;
! derived from Optics5 data file "CIE 1931 Color Match from E308.txt", which is
! the same as WINDOW4 file Cie31t.dat)
REAL(r64), PRIVATE, DIMENSION(numt3) :: wlt3=  &  ! Wavelength values for photopic response
    (/.380d0,.385d0,.390d0,.395d0,.400d0,.405d0,.410d0,.415d0,.420d0,.425d0, &
      .430d0,.435d0,.440d0,.445d0,.450d0,.455d0,.460d0,.465d0,.470d0,.475d0, &
      .480d0,.485d0,.490d0,.495d0,.500d0,.505d0,.510d0,.515d0,.520d0,.525d0, &
      .530d0,.535d0,.540d0,.545d0,.550d0,.555d0,.560d0,.565d0,.570d0,.575d0, &
      .580d0,.585d0,.590d0,.595d0,.600d0,.605d0,.610d0,.615d0,.620d0,.625d0, &
      .630d0,.635d0,.640d0,.645d0,.650d0,.655d0,.660d0,.665d0,.670d0,.675d0, &
      .680d0,.685d0,.690d0,.695d0,.700d0,.705d0,.710d0,.715d0,.720d0,.725d0, &
      .730d0,.735d0,.740d0,.745d0,.750d0,.755d0,.760d0,.765d0,.770d0,.775d0, &
      .780d0 /)

REAL(r64), PRIVATE, DIMENSION(numt3) :: y30=  &   ! Photopic response corresponding to wavelengths in wlt3
    (/0.0000d0,0.0001d0,0.0001d0,0.0002d0,0.0004d0,0.0006d0,0.0012d0,0.0022d0,0.0040d0,0.0073d0, &
      0.0116d0,0.0168d0,0.0230d0,0.0298d0,0.0380d0,0.0480d0,0.0600d0,0.0739d0,0.0910d0,0.1126d0, &
      0.1390d0,0.1693d0,0.2080d0,0.2586d0,0.3230d0,0.4073d0,0.5030d0,0.6082d0,0.7100d0,0.7932d0, &
      0.8620d0,0.9149d0,0.9540d0,0.9803d0,0.9950d0,1.0000d0,0.9950d0,0.9786d0,0.9520d0,0.9154d0, &
      0.8700d0,0.8163d0,0.7570d0,0.6949d0,0.6310d0,0.5668d0,0.5030d0,0.4412d0,0.3810d0,0.3210d0, &
      0.2650d0,0.2170d0,0.1750d0,0.1382d0,0.1070d0,0.0816d0,0.0610d0,0.0446d0,0.0320d0,0.0232d0, &
      0.0170d0,0.0119d0,0.0082d0,0.0158d0,0.0041d0,0.0029d0,0.0021d0,0.0015d0,0.0010d0,0.0007d0, &
      0.0005d0,0.0004d0,0.0002d0,0.0002d0,0.0001d0,0.0001d0,0.0001d0,0.0000d0,0.0000d0,0.0000d0, &
      0.0000d0 /)


          ! MODULE VARIABLE DECLARATIONS:

INTEGER   :: ngllayer                     ! Number of glass layers
INTEGER   :: nglface                      ! Number of glass faces
INTEGER   :: nglfacep                     ! Number of glass faces, + 2 if shade layer present
REAL(r64) :: tout                         ! Outside air temperature (K)
REAL(r64) :: tin                          ! Inside air temperature (previous timestep) (K)
REAL(r64) :: tilt                         ! Window tilt (deg)
REAL(r64) :: tiltr                        ! Window tilt (radians)
REAL(r64) :: hcin                         ! Convective inside air film conductance (W/m2-K)
REAL(r64) :: hcout                        ! Convective outside air film conductance (W/m2-K)
REAL(r64) :: Ebout                        ! Sigma*(outside air temp)**4 (W/m2)
REAL(r64) :: Outir                        ! IR radiance of window's exterior surround (W/m2)
REAL(r64) :: Rmir                         ! IR radiance of window's interior surround (W/m2)
REAL(r64) :: rtot                         ! Total thermal resistance of window (m2-K/W)
REAL(r64) :: gcon(5,5,3) =0.0d0             ! Gas thermal conductivity coefficients for each gap
REAL(r64) :: gvis(5,5,3) =0.0d0             ! Gas viscosity coefficients for each gap
REAL(r64) :: gcp(5,5,3)  =0.0d0             ! Gas specific-heat coefficients for each gap
REAL(r64) :: gwght(5,5)  =0.0d0             ! Gas molecular weights for each gap
REAL(r64) :: gfract(5,5) =0.0d0             ! Gas fractions for each gap
INTEGER   :: gnmix(5)    =0               ! Number of gases in gap
REAL(r64) :: gap(5)      =0.0d0             ! Gap width (m)
REAL(r64) :: thick(5)    =0.0d0             ! Glass layer thickness (m)
REAL(r64) :: scon(5)     =0.0d0             ! Glass layer conductance--conductivity/thickness (W/m2-K)
REAL(r64) :: tir(10)     =0.0d0             ! Front and back IR transmittance for each glass layer
REAL(r64) :: emis(10)    =0.0d0             ! Front and back IR emissivity for each glass layer
REAL(r64) :: rir(10)     =0.0d0             ! Front and back IR reflectance for each glass layer
                                          !  (program calculates from tir and emis)
REAL(r64) :: AbsRadGlassFace(10) =0.0d0     ! Solar radiation and IR radiation from internal
                                                  !  gains absorbed by glass face
REAL(r64) :: thetas(10)          =0.0D0   ! Glass surface temperatures (K)
REAL(r64) :: thetasPrev(10)      =0.0D0   ! Previous-iteration glass surface temperatures (K)
REAL(r64) :: fvec(10)            =0.0D0   ! Glass face heat balance function
REAL(r64) :: fjac(10,10)         =0.0D0   ! Glass face heat balance Jacobian
REAL(r64) :: dtheta(5)           =0.0d0    ! Glass layer temperature difference factor [K]
REAL(r64) :: zir(10,10)          =0.0d0    ! IR transfer matrix
REAL(r64) :: ziri(10,10)         =0.0d0    ! Inverse of IR transfer matrix
REAL(r64) :: ddeldt(10,10)       =0.0d0    ! Matrix of derivatives of residuals wrt temperature
REAL(r64) :: dtddel(10,10)       =0.0d0    ! Inverse of matrix of derivatives of
                                          !   residuals wrt temperature
REAL(r64) :: qf(10)              =0.0d0    ! IR heat flux at each face [W/m2]
REAL(r64) :: hf(10)              =0.0d0    ! Component of convective flux at each face
REAL(r64) :: der(10,5)           =0.0d0    ! Derivative of IR sources wrt surface temperature
REAL(r64) :: dhf(10,5)           =0.0d0    ! Derivative of heat flux wrt surface temperature
REAL(r64) :: sour(10)            =0.0d0    ! IR source term at each face [W/m2]
REAL(r64) :: delta(5)            =0.0d0    ! Residual at each glass layer [W/m2]
REAL(r64) :: hcgap(5)            =0.0d0    ! Convective gap conductance
REAL(r64) :: hrgap(5)            =0.0d0    ! Radiative gap conductance
REAL(r64) :: rgap(6)             =0.0d0    ! Convective plus radiative gap resistance
                                          !   (inverse of hcgap + hrgap)
REAL(r64) :: rs(6)               =0.0d0    ! Outside film convective resistance, gap resistances,
                                          !   inside air film convective resistance
REAL(r64) :: arhs(6)             =0.0d0
REAL(r64) :: A23P,A32P,A45P,A54P,A67P,A76P ! Intermediate variables in glass face
REAL(r64) :: A23,A45,A67                  ! heat balance equations

REAL(r64) :: wlt(MaxSpectralDataElements,5)      =0.0d0    ! Spectral data wavelengths for each glass layer in a glazing system
   ! Following data, Spectral data for each layer for each wavelength in wlt
REAL(r64) :: t(MaxSpectralDataElements,5)        =0.0d0    ! normal transmittance
REAL(r64) :: rff(MaxSpectralDataElements,5)      =0.0d0    ! normal front reflectance
REAL(r64) :: rbb(MaxSpectralDataElements,5)      =0.0d0    ! normal back reflectance
REAL(r64) :: tPhi(MaxSpectralDataElements,5)     =0.0d0    ! transmittance at angle of incidence
REAL(r64) :: rfPhi(MaxSpectralDataElements,5)    =0.0d0    ! front reflectance at angle of incidence
REAL(r64) :: rbPhi(MaxSpectralDataElements,5)    =0.0d0    ! back reflectance at angle of incidence
REAL(r64) :: tadjPhi(MaxSpectralDataElements,5)  =0.0d0    ! transmittance at angle of incidence
REAL(r64) :: rfadjPhi(MaxSpectralDataElements,5) =0.0d0    ! front reflectance at angle of incidence
REAL(r64) :: rbadjPhi(MaxSpectralDataElements,5) =0.0d0    ! back reflectance at angle of incidence

INTEGER   :: numpt(5)            =0       ! Number of spectral data wavelengths for each layer; =2 if no spectra data for a layer
REAL(r64) :: stPhi(nume)         =0.0d0    ! Glazing system transmittance at angle of incidence for each wavelength in wle
REAL(r64) :: srfPhi(nume)        =0.0d0    ! Glazing system front reflectance at angle of incidence for each wavelength in wle
REAL(r64) :: srbPhi(nume)        =0.0d0    ! Glazing system back reflectance at angle of incidence for each wavelenth in wle
REAL(r64) :: saPhi(nume,5)       =0.0d0    ! For each layer, glazing system absorptance at angle of incidence
                                          ! for each wavelenth in wle
REAL(r64) :: top(5,5)            =0.0d0    ! Transmittance matrix for subr. op
REAL(r64) :: rfop(5,5)           =0.0d0    ! Front reflectance matrix for subr. op
REAL(r64) :: rbop(5,5)           =0.0d0    ! Back transmittance matrix for subr. op
REAL(r64) :: IndepVarCurveFit(10)=0.0d0    ! Values of independent variable (cos of inc. angle) for curve fit
REAL(r64) :: DepVarCurveFit(10)  =0.0d0    ! Values of dependent variable corresponding to IndepVarCurveFit values
REAL(r64) :: CoeffsCurveFit(6)   =0.0d0    ! Polynomial coefficients from curve fit
REAL(r64) :: tsolPhi(10)         =0.0d0    ! Glazing system solar transmittance for each angle of incidence
REAL(r64) :: rfsolPhi(10)        =0.0d0    ! Glazing system solar front reflectance for each angle of incidence
REAL(r64) :: rbsolPhi(10)        =0.0d0    ! Glazing system solar back reflectance for each angle of incidence
REAL(r64) :: solabsPhi(10,5)     =0.0d0    ! Glazing system solar absorptance for each angle of incidence
REAL(r64) :: solabsBackPhi(10,5) =0.0d0    ! Glazing system back solar absorptance for each angle of incidence
REAL(r64) :: solabsShadePhi(10)  =0.0d0    ! Glazing system interior shade solar absorptance for each angle of incidence
REAL(r64) :: tvisPhi(10)         =0.0d0    ! Glazing system visible transmittance for each angle of incidence
REAL(r64) :: rfvisPhi(10)        =0.0d0    ! Glazing system visible front reflectance for each angle of incidence
REAL(r64) :: rbvisPhi(10)        =0.0d0    ! Glazing system visible back reflectance for each angle of incidence
REAL(r64) :: CosPhiIndepVar(10)  =0.0d0    ! Cos of incidence angles at 10-deg increments for curve fits

          ! SUBROUTINE SPECIFICATIONS FOR MODULE WindowManager:
PUBLIC  InitGlassOpticalCalculations
!   Optical Calculation Routines
PRIVATE SystemSpectralPropertiesAtPhi
PRIVATE SystemPropertiesAtLambdaAndPhi
PRIVATE SolarSprectrumAverage
PRIVATE VisibleSprectrumAverage
PRIVATE CheckAndReadCustomSprectrumData
PRIVATE Interpolate
!   Heat Balance Routines
PUBLIC  CalcWindowHeatBalance
PRIVATE WindowHeatBalanceEquations
PRIVATE SolveForWindowTemperatures
PRIVATE ExtOrIntShadeNaturalFlow
PRIVATE BetweenGlassShadeNaturalFlow
PRIVATE BetweenGlassForcedFlow
PRIVATE LUdecomposition
PRIVATE LUsolution
PRIVATE WindowGasConductance
PRIVATE WindowGasPropertiesAtTemp
PRIVATE StartingWindowTemps
PRIVATE NusseltNumber
PRIVATE TransAndReflAtPhi
PRIVATE InterpolateBetweenTwoValues
PRIVATE InterpolateBetweenFourValues
PUBLIC  W5LsqFit
PRIVATE W5LsqFit2
PRIVATE DiffuseAverage
PRIVATE DiffuseAverageProfAngGnd
PRIVATE DiffuseAverageProfAngSky
PRIVATE CalcWinFrameAndDividerTemps
PUBLIC  CalcNominalWindowCond
PRIVATE WindowTempsForNominalCond
PRIVATE StartingWinTempsForNominalCond
PRIVATE ReportGlass
PRIVATE CalcWindowBlindProperties
PRIVATE CalcWindowScreenProperties
PRIVATE BlindOpticsDiffuse
PRIVATE BlindOpticsBeam
PRIVATE ViewFac
PRIVATE InvertMatrix
PRIVATE LUDCMP
PRIVATE LUBKSB

CONTAINS
          ! MODULE SUBROUTINES:

SUBROUTINE InitGlassOpticalCalculations

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   August 1999
          !       MODIFIED       May 2001 (FW): add window blinds
          !                      Jan 2002 (FW): add blinds with variable slat angle
          !                      Jan 2003 (FW): add between-glass shade/blind
          !                      May 2006 (RR): add exterior window screen
          !                      Aug 2010 (TH): allow spectral data for between-glass shade/blind
          !                      Aug 2013 (TH): allow user defined solar and visible spectrum data
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the calculation of the solar and visible properties of a multi-layer glazing
          ! system from the properties of the individual glazing and shading layers

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Vectors
  USE General, ONLY: TrimSigDigits
  USE WindowEquivalentLayer, ONLY: InitEquivalentLayerWindowCalculations

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
INTEGER           :: CoefNum                 ! Polynomial coefficient number
INTEGER           :: j                       ! Wavelength counter
INTEGER           :: TotLay                  ! Total solid and gas layers in a window construction
INTEGER           :: ConstrNum               ! Construction number
INTEGER           :: ConstrNumSh             ! Shaded construction number
INTEGER           :: SurfNum                 ! Surface number
INTEGER           :: ShadeLayNum             ! Layer number for shade or blind, if present
INTEGER           :: ShadeLayPtr             ! Material number for shade or blind
LOGICAL           :: lquasi                  ! True if one or more glass layers have no spectral data
LOGICAL           :: AllGlassIsSpectralAverage ! True if all glazing in a construction is spectral average
LOGICAL           :: IntShade,ExtShade,BGShade ! True if construction has an interior,exterior or between-glass shade
LOGICAL           :: IntBlind,ExtBlind,BGBlind ! True if construction has an interior,exterior or between-glass blind
LOGICAL           :: ExtScreen               ! True if construction has an exterior screen
LOGICAL           :: ScreenOn                ! True if construction has an exterior screen
LOGICAL           :: BlindOn                 ! True if IntBlind, ExtBlind or BGBlind is true
LOGICAL           :: ShadeOn                 ! True if IntShade, ExtShade or BGShade is true
INTEGER           :: BlNum                   ! Blind number
INTEGER           :: ScNum                   ! Screen number
REAL(r64)         :: sabsPhi(nume)           ! Glazing system absorptance for a glass layer
                                             !  and angle of incidence, for each wavelength
                                             !   glass layer for an angle of incidence, for each wavelength
REAL(r64)         :: solabsDiff(5)           ! Glazing system layer solar absorptance for each glass layer
REAL(r64)         :: solabsPhiLay(10)        ! Glazing system solar absorptance for a layer at each incidence angle
REAL(r64)         :: tsolPhiFit(10)          ! Glazing system solar transmittance from fit at each incidence angle
REAL(r64)         :: tvisPhiFit(10)          ! Glazing system visible transmittance from fit at each incidence angle
REAL(r64)         :: tBareSolPhi(10,5)       ! Isolated glass solar transmittance for each incidence angle
REAL(r64)         :: t1,t2                   ! = tBareSolPhi(,1)(,2)
REAL(r64)         :: tBareVisPhi(10,5)       ! Isolated glass visible transmittance for each incidence angle
REAL(r64)         :: t1v,t2v                 ! = tBareVisPhi(,1)(,2)
REAL(r64)         :: rfBareSolPhi(10,5)      ! Isolated glass front solar reflectance for each incidence angle
REAL(r64)         :: rfBareVisPhi(10,5)      ! Isolated glass front visible reflectance for each incidence angle
REAL(r64)         :: rbBareSolPhi(10,5)      ! Isolated glass back solar reflectance for each incidence angle
REAL(r64)         :: rbBareVisPhi(10,5)      ! Isolated glass back visible reflectance for each incidence angle
REAL(r64)         :: afBareSolPhi(10,5)      ! Isolated glass front solar absorptance for each incidence angle
REAL(r64)         :: af1,af2                 ! = afBareSolPhi(,1)(,2)
REAL(r64)         :: rbmf2                   ! Isolated glass #2 front beam reflectance
REAL(r64)         :: abBareSolPhi(10,5)      ! Isolated glass back solar absorptance for each incidence angle
REAL(r64)         :: ab1,ab2                 ! = abBareSolPhi(,1)(,2)
REAL(r64)         :: td1,td2,td3             ! Isolated glass diffuse solar transmittance
REAL(r64)         :: td1v,td2v,td3v          ! Isolated glass diffuse visible transmittance
REAL(r64)         :: rf1,rf2,rf3             ! Isolated glass diffuse solar front reflectance
REAL(r64)         :: rf1v,rf2v,rf3v          ! Isolated glass diffuse visible front reflectance
REAL(r64)         :: rb1,rb2,rb3             ! Isolated glass diffuse solar back reflectance
REAL(r64)         :: rb1v,rb2v,rb3v          ! Isolated glass diffuse visible back reflectance
REAL(r64)         :: afd1,afd2,afd3          ! Isolated glass diffuse solar front absorptance
REAL(r64)         :: abd1,abd2,abd3          ! Isolated glass diffuse solar back absorptance
REAL(r64)         :: TauShIR                 ! IR transmittance of isolated shade
REAL(r64)         :: EpsShIR                 ! IR absorptance of isolated shade
REAL(r64)         :: RhoShIR                 ! IR reflectance of isolated shade
REAL(r64)         :: EpsGlIR                 ! IR absorptance of front or back of isolated glass
REAL(r64)         :: RhoGlIR                 ! IR reflectance of inside face of inside glass
INTEGER           :: NGlass                  ! Number of glass layers in a construction
INTEGER           :: IGlass                  ! Glass layer counter
INTEGER           :: LayNum                  ! Layer number for a glass layer
INTEGER           :: LayPtr                  ! Material number corresponding to LayNum
INTEGER           :: IPhi                    ! Incidence angle counter
REAL(r64)         :: Phi                     ! Incidence angle (deg)
REAL(r64)         :: CosPhi                  ! Cosine of incidence angle
INTEGER           :: ILam                    ! Wavelength counter
REAL(r64)         :: tsolDiff                ! Glazing system diffuse solar transmittance
REAL(r64)         :: tvisDiff                ! Glazing system diffuse visible transmittance
INTEGER           :: IGlassBack              ! Glass layer number counted from back of window
REAL(r64)         :: ShadeAbs                ! Solar absorptance of isolated shade
REAL(r64)         :: ash                     ! = ShadeAbs
REAL(r64)         :: afsh                    ! Diffuse solar front absorptance of isolated blind
REAL(r64)         :: afshGnd,afshSky         ! Ground and sky diffuse solar front absorptance of isolated blind
REAL(r64)         :: absh                    ! Diffuse solar back absorptance of isolated blind
REAL(r64)         :: ShadeTrans              ! Solar transmittance of isolated shade/blind
REAL(r64)         :: ShadeTransGnd           ! Diffuse-diffuse transmittance of isolated vertical blind with
                                             ! horizontal slats for isotropic ground solar
REAL(r64)         :: ShadeTransSky           ! Diffuse-diffuse transmittance of isolated vertical blind with
                                             ! horizontal slats for isotropic sky solar
REAL(r64)         :: tsh                     ! = ShadeTrans
REAL(r64)         :: tshGnd,tshSky           ! = ShadeTransGnd,ShadeTransSky
REAL(r64)         :: tsh2                    ! = tsh**2
REAL(r64)         :: ShadeRefl               ! Solar reflectance of isolated shade
REAL(r64)         :: ShadeReflGnd            ! Front blind reflectance for ground diffuse solar
REAL(r64)         :: ShadeReflSky            ! Front blind reflectance for sky diffuse solar
REAL(r64)         :: rsh                     ! = ShadeRefl
REAL(r64)         :: rfsh                    ! Diffuse solar front reflectance of isolated blind
REAL(r64)         :: rfshGnd,rfshSky         ! Ground and sky diffuse solar front reflectance of isolated blind
REAL(r64)         :: rbsh                    ! Diffuse solar back reflectance of isolated blind
REAL(r64)         :: ShadeReflFac            ! Shade/blind solar reflection factor
REAL(r64)         :: ShadeTransVis           ! Visible transmittance of isolated shade/blind
REAL(r64)         :: tshv                    ! = ShadeTransVis
REAL(r64)         :: tshv2                   ! = tshv**2
REAL(r64)         :: ShadeReflVis            ! Visible reflectance of isolated shade
REAL(r64)         :: rshv                    ! = ShadeReflVis
REAL(r64)         :: rfshv                   ! Diffuse visible front reflectance of isolated blind
REAL(r64)         :: rbshv                   ! Diffuse visible back reflectance of isolated blind
REAL(r64)         :: ShadeReflFacVis         ! Shade/blind visible reflection factor
INTEGER           :: SpecDataNum             ! Spectral data set number
INTEGER           :: numptDAT                ! Number of wavelengths in a spectral data set
INTEGER           :: ISlatAng                ! Slat angle counter
LOGICAL           :: StormWinConst           ! True if a construction with a storm window
LOGICAL           :: Triangle                ! True if window is triangular
LOGICAL           :: Rectangle               ! True if window is rectangular
REAL(r64)         :: W1(3),W2(3),W3(3)       ! Window vertices (m)
REAL(r64)         :: W21(3),W23(3)           ! W1-W2, W3-W2, resp. (m)
LOGICAL           :: lSimpleGlazingSystem = .FALSE. ! true if using simple glazing system block model
REAL(r64)         :: SimpleGlazingSHGC = 0.0D0  ! value of SHGC for simple glazing system block model
REAL(r64)         :: SimpleGlazingU    = 0.0D0  ! value of U-factor for simple glazing system block model
LOGICAL           :: BGFlag = .FALSE.        ! True if between-glass shade or blind
REAL(r64)         :: tmpTrans = 0.0D0        ! solar transmittance calculated from spectral data
REAL(r64)         :: tmpTransVis = 0.0D0     ! visible transmittance calculated from spectral data
REAL(r64)         :: tmpReflectSolBeamFront = 0.0D0
REAL(r64)         :: tmpReflectSolBeamBack =0.0D0
REAL(r64)         :: tmpReflectVisBeamFront = 0.0D0
REAL(r64)         :: tmpReflectVisBeamBack = 0.0D0

!Debug
INTEGER                        ::  Idb
REAL(r64),DIMENSION(11)        ::  DbgTheta = (/ 0.d0,10.d0,20.d0,30.d0,40.d0,50.d0,60.d0,70.d0,80.d0,82.5d0,89.5d0 /)
REAL(r64),DIMENSION(11)        ::  DbgTSol  =0.0d0
REAL(r64),DIMENSION(11)        ::  DbgRbSol  =0.0d0
REAL(r64),DIMENSION(11)        ::  DbgTVis  =0.0d0
REAL(r64),DIMENSION(11,5)      ::  DbgFtAbs  =0.0d0
REAL(r64),DIMENSION(11,5)      ::  DbgBkAbs  =0.0d0
REAL(r64)                     ::  DbgTSolDiff  =0.0d0
REAL(r64)                     ::  DbgRBSolDiff  =0.0d0
REAL(r64),DIMENSION(5)        ::  DbgFTAbsDiff  =0.0d0
REAL(r64),DIMENSION(5)        ::  DbgBkAbsDiff  =0.0d0

!EndDebug

! check and read custom solar and/or visible spectrum data if any
CALL CheckAndReadCustomSprectrumData

CALL W5InitGlassParameters

! Calculate optical properties of blind-type layers entered with MATERIAL:WindowBlind
IF(TotBlinds > 0) CALL CalcWindowBlindProperties

! Initialize SurfaceScreen structure
IF(NumSurfaceScreens > 0) CALL CalcWindowScreenProperties

! Get glazing system optical properties of constructions with glass or glass plus
!   shade, screen or blind
! Loop over constructions and find those that are glazing constructions
DO ConstrNum = 1,TotConstructs
  IF (.not. Construct(ConstrNum)%TypeIsWindow) CYCLE
  IF ( Construct(ConstrNum)%WindowTypeBSDF ) CYCLE  !Skip Complex Fenestrations, they have separate
  IF ( Construct(ConstrNum)%WindowTypeEQL  ) CYCLE  !skip Equivalent Layer Fenestration
              !handling of optical properties

  TotLay = Construct(ConstrNum)%TotLayers

  ! First layer must be glass, shade, screen or blind to be a glazing construction
  IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group /= WindowGlass .AND.  &
     Material(Construct(ConstrNum)%LayerPoint(1))%Group /= Shade .AND. &
     Material(Construct(ConstrNum)%LayerPoint(1))%Group /= Screen .AND. &
     Material(Construct(ConstrNum)%LayerPoint(1))%Group /= WindowBlind .AND. &
     Material(Construct(ConstrNum)%LayerPoint(1))%Group /= WindowSimpleGlazing) CYCLE

  ShadeLayNum = 0
  ExtShade = .FALSE.
  IntShade = .FALSE.
  BGShade  = .FALSE.
  ExtBlind = .FALSE.
  IntBlind = .FALSE.
  BGBlind  = .FALSE.
  ExtScreen = .FALSE.
  StormWinConst = .false.
  lSimpleGlazingSystem = .FALSE.

  IF (Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowSimpleGlazing) Then
    ! what if outside layer is shade, blind, or screen?
    lSimpleGlazingSystem = .TRUE.
    SimpleGlazingSHGC    = Material(Construct(ConstrNum)%LayerPoint(1))%SimpleWindowSHGC
    SimpleGlazingU      = Material(Construct(ConstrNum)%LayerPoint(1))%SimpleWindowUfactor
  ENDIF

  IF(Construct(ConstrNum)%Name(1:28)=='BARECONSTRUCTIONWITHSTORMWIN' .OR.  &
     Construct(ConstrNum)%Name(1:30)=='SHADEDCONSTRUCTIONWITHSTORMWIN') StormWinConst = .true.

  ! Get layer number of shade/blind
  IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group == Shade) THEN
    ExtShade = .TRUE.
    ShadeLayNum = 1
  ELSE IF(Material(Construct(ConstrNum)%LayerPoint(TotLay))%Group == Shade) THEN
    IntShade = .TRUE.
    ShadeLayNum = TotLay
  ELSE IF(Construct(ConstrNum)%TotLayers == 5) THEN
    IF (Material(Construct(ConstrNum)%LayerPoint(3))%Group == Shade) THEN
      BGShade  = .TRUE.
      ShadeLayNum = 3
    ENDIF
  ELSE IF(Construct(ConstrNum)%TotLayers == 7) THEN
    IF (Material(Construct(ConstrNum)%LayerPoint(5))%Group == Shade) THEN
      BGShade  = .TRUE.
      ShadeLayNum = 5
    ENDIF
  ENDIF

  IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group == WindowBlind) THEN
    ExtBlind = .TRUE.
    ShadeLayNum = 1
    BlNum = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%BlindDataPtr
  ELSE IF(Material(Construct(ConstrNum)%LayerPoint(TotLay))%Group == WindowBlind) THEN
    IntBlind = .TRUE.
    ShadeLayNum = TotLay
    BlNum = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%BlindDataPtr
  ELSE IF(Construct(ConstrNum)%TotLayers == 5) THEN
    IF (Material(Construct(ConstrNum)%LayerPoint(3))%Group == WindowBlind) THEN
      BGBlind  = .TRUE.
      ShadeLayNum = 3
      BlNum = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%BlindDataPtr
    ENDIF
  ELSE IF(Construct(ConstrNum)%TotLayers == 7) THEN
    IF (Material(Construct(ConstrNum)%LayerPoint(5))%Group == WindowBlind) THEN
      BGBlind  = .TRUE.
      ShadeLayNum = 5
      BlNum = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%BlindDataPtr
    ENDIF
  END IF

  IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group == Screen) THEN
    ShadeLayNum = 1
    ScNum = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%ScreenDataPtr
!   Disregard orphaned constructs with exterior screen
    IF(ScNum .EQ. 0)CYCLE
    ExtScreen = .TRUE.
  END IF

  ScreenOn = ExtScreen
  BlindOn = IntBlind.OR.ExtBlind.OR.BGBlind
  ShadeOn = IntShade.OR.ExtShade.OR.BGShade
  BGFlag = BGBlind.OR.BGShade

  ! For construction with interior or exterior shade, get shade thermal absorptance (emissivity)
  ! (accounting for inter-reflection with glazing) and correct the inside glass InsideAbsorpThermal
  ! for presence of interior shade. Assumes inner and outer glass layers have zero thermal transmittance.

  IF(IntShade.OR.ExtShade.OR.ExtScreen) THEN
    ShadeLayPtr = Construct(ConstrNum)%LayerPoint(ShadeLayNum)
    IF(ExtScreen)THEN
      TauShIR = SurfaceScreens(ScNum)%DifDifTrans
    ELSE
      TauShIR = Material(ShadeLayPtr)%TransThermal
    ENDIF
    EpsShIR = Material(ShadeLayPtr)%AbsorpThermal
    RhoShIR = MAX(0.0d0,1.d0-TauShIR-EpsShIR)
    IF(ExtShade .OR. ExtScreen) THEN  ! Exterior shade or screen
      EpsGlIR = Material(Construct(ConstrNum)%LayerPoint(2))%AbsorpThermalFront
    ELSE               ! Interior shade
      EpsGlIR = Material(Construct(ConstrNum)%LayerPoint(TotLay-1))%AbsorpThermalBack
    END IF
    RhoGlIR = MAX(0.0d0,1.d0-EpsGlIR)
    Construct(ConstrNum)%ShadeAbsorpThermal = EpsShIR*(1.d0+TauShIR*RhoGlIR/(1.d0-RhoShIR*RhoGlIR))
    IF(IntShade) Construct(ConstrNum)%InsideAbsorpThermal = &
                      Construct(ConstrNum)%InsideAbsorpThermal*TauShIR/(1.d0-RhoShIR*RhoGlIR)
  END IF

  ! From the individual glass layer properties, get the glazing system optical properties
  ! for BARE GLASS (i.e., interior, exterior or between-glass shade or blind, or exterior screen, if present, not in place).
  ! Get one set of system properties for solar incident on front of
  ! window and a second set for solar incident on back of window. (The back-incident
  ! properties are used with interior short-wave radiation striking the window from inside.)

  ! After the front and back system optical properties are calculated for bare glass,
  ! a correction is made for the effect of a shade, screen or blind if one of these
  ! is present in the construction.

  NGlass = Construct(ConstrNum)%TotGlassLayers

  !--------------------------------------------------------------------------------------------
  ! Front calculation (solar incident from outside of room); bare glass portion of construction
  !--------------------------------------------------------------------------------------------

  lquasi = .FALSE.
  AllGlassIsSpectralAverage = .TRUE.

  ! Loop over glass layers in the construction
  DO IGlass = 1,NGlass
    LayNum = 1 + 2*(IGlass-1)
    IF(ExtShade.OR.ExtBlind.OR.ExtScreen) LayNum = 2 + 2*(IGlass-1)
    IF(BGShade.OR.BGBlind) THEN
      LayNum = 1
      IF(NGlass==2) THEN
        IF(IGlass==2) LayNum = 5
      ELSE  ! NGlass = 3
        IF(IGlass==2) LayNum = 3
        IF(IGlass==3) LayNum = 7
      END IF
    END IF

    LayPtr = Construct(ConstrNum)%LayerPoint(LayNum)
    SpecDataNum = Material(LayPtr)%GlassSpectralDataPtr
    IF(SpecDataNum /= 0) THEN
      IF (.NOT. BGFlag) AllGlassIsSpectralAverage = .FALSE.

      ! Get the spectral data for the transmittance, front reflectance and
      ! back reflectance (all at normal incidence) for this layer.
      ! In this case, "front" means incident from the outside and "back"
      ! means incident from the inside.
      numptDAT = SpectralData(SpecDataNum)%NumOfWavelengths
      numpt(IGlass) = numptDat

      DO ILam = 1,numptDat
        wlt(ILam,IGlass) = SpectralData(SpecDataNum)%Wavelength(ILam)
        t(ILam,IGlass)   = SpectralData(SpecDataNum)%Trans(ILam)
        IF((IGlass==1.OR.(IGlass==2.AND.StormWinConst)).AND.(.NOT.BGFlag)) t(ILam,IGlass) = &
           t(ILam,IGlass) * Material(LayPtr)%GlassTransDirtFactor
        rff(ILam,IGlass) = SpectralData(SpecDataNum)%ReflFront(ILam)
        rbb(ILam,IGlass) = SpectralData(SpecDataNum)%ReflBack(ILam)
      END DO

      ! TH 8/26/2010, CR 8206
      ! If there is spectral data for between-glass shades or blinds, calc the average spectral properties for use.
      IF (BGFlag) THEN
        ! 5/16/2012 CR 8793. Add warning message for the glazing defined with full spectral data.
        CALL ShowWarningError('Window glazing material "'//TRIM(Material(LayPtr)%Name)// &
          '" was defined with full spectral data and has been converted to average spectral data')
        CALL ShowContinueError('due to its use with between-glass shades or blinds of the window construction "'// &
          TRIM(Construct(ConstrNum)%Name) // '".')
        CALL ShowContinueError('All occurrences of this glazing material will be modeled as SpectralAverage.')
        CALL ShowContinueError('If this material is also used in other window constructions'// &
          '  without between-glass shades or blinds,')
        CALL ShowContinueError('then make a duplicate material (with new name) if you want to model those windows '//  &
           ' (and reference the new material) using the full spectral data.')
        ! calc Trans, TransVis, ReflectSolBeamFront, ReflectSolBeamBack, ReflectVisBeamFront, ReflectVisBeamBack
        !  assuming wlt same as wle
        CALL SolarSprectrumAverage(t, tmpTrans)
        CALL SolarSprectrumAverage(rff, tmpReflectSolBeamFront)
        CALL SolarSprectrumAverage(rbb, tmpReflectSolBeamBack)

        ! visible properties
        CALL VisibleSprectrumAverage(t, tmpTransVis)
        CALL VisibleSprectrumAverage(rff, tmpReflectVisBeamFront)
        CALL VisibleSprectrumAverage(rbb, tmpReflectVisBeamBack)

        ! set this material to average spectral data
        Material(LayPtr)%GlassSpectralDataPtr = 0
        Material(LayPtr)%Trans   = tmpTrans
        Material(LayPtr)%TransVis   = tmpTransVis
        Material(LayPtr)%ReflectSolBeamFront = tmpReflectSolBeamFront
        Material(LayPtr)%ReflectSolBeamBack = tmpReflectSolBeamBack
        Material(LayPtr)%ReflectVisBeamFront = tmpReflectVisBeamFront
        Material(LayPtr)%ReflectVisBeamBack = tmpReflectVisBeamBack
        SpecDataNum = 0
      ENDIF
    ENDIF

    IF(SpecDataNum == 0) THEN  ! No spectral data for this layer; use spectral average values
      lquasi = .TRUE.
      numpt(IGlass) = 2
      t(1,IGlass)   = Material(LayPtr)%Trans
      IF(IGlass==1.OR.(IGlass==2.AND.StormWinConst)) t(1,IGlass) =  &
         t(1,IGlass) * Material(LayPtr)%GlassTransDirtFactor
      t(2,IGlass)   = Material(LayPtr)%TransVis
      IF(IGlass==1.OR.(IGlass==2.AND.StormWinConst)) t(2,IGlass) =  &
         t(2,IGlass) * Material(LayPtr)%GlassTransDirtFactor
      rff(1,IGlass) = Material(LayPtr)%ReflectSolBeamFront
      rbb(1,IGlass) = Material(LayPtr)%ReflectSolBeamBack
      rff(2,IGlass) = Material(LayPtr)%ReflectVisBeamFront
      rbb(2,IGlass) = Material(LayPtr)%ReflectVisBeamBack
    END IF
  END DO  ! End of loop over glass layers in the construction for front calculation

  ! Loop over incidence angle from 0 to 90 deg in 10 deg increments.
  ! Get glass layer properties, then glazing system properties (which include the
  ! effect of inter-reflection among glass layers) at each incidence angle.

  DO IPhi = 1,10
    Phi = REAL(IPhi-1,r64)*10.d0
    CosPhi = COS(Phi*DegToRadians)
    if (abs(CosPhi) < .0001d0) CosPhi=0.0d0

    ! For each wavelength, get glass layer properties at this angle of incidence
    ! from properties at normal incidence
    DO IGlass = 1,NGlass
      DO ILam = 1,numpt(IGlass)

        CALL TransAndReflAtPhi(CosPhi,t(ILam,IGlass),rff(ILam,IGlass),rbb(ILam,IGlass), &
                        tPhi(ILam,IGlass),rfPhi(ILam,IGlass),rbPhi(ILam,IGlass), &
                        lSimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU)
      END DO

      ! For use with between-glass shade/blind, save angular properties of isolated glass
      ! for case that all glass layers were input with spectral-average properties
      !  only used by between-glass shades or blinds
      IF(AllGlassIsSpectralAverage) THEN
        tBareSolPhi(IPhi,IGlass)  = tPhi(1,IGlass)
        tBareVisPhi(IPhi,IGlass)  = tPhi(2,IGlass)
        rfBareSolPhi(IPhi,IGlass) = rfPhi(1,IGlass)
        rfBareVisPhi(IPhi,IGlass) = rfPhi(2,IGlass)
        rbBareSolPhi(IPhi,IGlass) = rbPhi(1,IGlass)
        rbBareVisPhi(IPhi,IGlass) = rbPhi(2,IGlass)
        afBareSolPhi(IPhi,IGlass) = MAX(0.0d0,1.d0-(tBareSolPhi(IPhi,IGlass)+rfBareSolPhi(IPhi,IGlass)))
        abBareSolPhi(IPhi,IGlass) = MAX(0.0d0,1.d0-(tBareSolPhi(IPhi,IGlass)+rbBareSolPhi(IPhi,IGlass)))
      END IF
    END DO

    ! For each wavelength in the solar spectrum, calculate system properties
    ! stPhi, srfPhi, srbPhi and saPhi at this angle of incidence.
    ! In the following the argument "1" indicates that spectral average solar values
    ! should be used for layers without spectral data.
    CALL SystemSpectralPropertiesAtPhi(1,NGlass,0.0d0,2.54d0)

    ! Get solar properties of system by integrating over solar irradiance spectrum.
    ! For now it is assumed that the exterior and interior irradiance spectra are the same.
    CALL SolarSprectrumAverage(stPhi, tsolPhi(IPhi))
    CALL SolarSprectrumAverage(srfPhi,rfsolPhi(IPhi))
    CALL SolarSprectrumAverage(srbPhi,rbsolPhi(IPhi))

    DO IGlass = 1, NGlass
      DO ILam = 1, nume
        sabsPhi(ILam) = saPhi(ILam,IGlass)
      END DO
      CALL SolarSprectrumAverage(sabsPhi,solabsPhi(IPhi,IGlass))
    END DO

    ! Get visible properties of system by integrating over solar irradiance
    ! spectrum weighted by photopic response.
    ! Need to redo the calculation of system spectral properties here only if
    ! one or more glass layers have no spectral data (lquasi = .TRUE.); in this
    ! case the spectral average visible properties will be used for the layers
    ! without spectral data, as indicated by the argument "2".

    if (lquasi) CALL SystemSpectralPropertiesAtPhi(2,NGlass,0.37d0,0.78d0)
    CALL VisibleSprectrumAverage(stPhi,  tvisPhi(IPhi))
    CALL VisibleSprectrumAverage(srfPhi, rfvisPhi(IPhi))
    CALL VisibleSprectrumAverage(srbPhi, rbvisPhi(IPhi))

  END DO  ! End of loop over incidence angles for front calculation

  !  only used by between-glass shades or blinds
  IF(AllGlassIsSpectralAverage) THEN
    DO IGlass = 1,NGlass
      CALL W5LsqFit(CosPhiIndepVar,tBareSolPhi (:,IGlass),6,1,10,Construct(ConstrNum)%tBareSolCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,tBareVisPhi (:,IGlass),6,1,10,Construct(ConstrNum)%tBareVisCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,rfBareSolPhi(:,IGlass),6,1,10,Construct(ConstrNum)%rfBareSolCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,rfBareVisPhi(:,IGlass),6,1,10,Construct(ConstrNum)%rfBareVisCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,rbBareSolPhi(:,IGlass),6,1,10,Construct(ConstrNum)%rbBareSolCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,rbBareVisPhi(:,IGlass),6,1,10,Construct(ConstrNum)%rbBareVisCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,afBareSolPhi(:,IGlass),6,1,10,Construct(ConstrNum)%afBareSolCoef(IGlass,:))
      CALL W5LsqFit(CosPhiIndepVar,abBareSolPhi(:,IGlass),6,1,10,Construct(ConstrNum)%abBareSolCoef(IGlass,:))
    END DO
  END IF

  Construct(ConstrNum)%ReflectSolDiffFront = DiffuseAverage(rfsolPhi)
  Construct(ConstrNum)%ReflectSolDiffBack  = DiffuseAverage(rbsolPhi)
  Construct(ConstrNum)%ReflectVisDiffFront = DiffuseAverage(rfvisPhi)
  Construct(ConstrNum)%ReflectVisDiffBack  = DiffuseAverage(rbvisPhi)

  tsolDiff = DiffuseAverage(tsolPhi)
  tvisDiff = DiffuseAverage(tvisPhi)
  Construct(ConstrNum)%TransDiff = tsolDiff
  Construct(ConstrNum)%TransDiffVis = tvisDiff
  DO IGlass = 1,NGlass
    solabsPhiLay(1:10) = solabsPhi(1:10,IGlass)
    solabsDiff(IGlass) = DiffuseAverage(solabsPhiLay)
    Construct(ConstrNum)%AbsDiff(IGlass) = solabsDiff(IGlass)

    ! For use with between-glass shade/blind, get diffuse properties of isolated glass for case when
    ! all glass layers were input with spectral-average properties
    !  only used by between-glass shades or blinds
    IF(AllGlassIsSpectralAverage) THEN
      Construct(ConstrNum)%tBareSolDiff(IGlass)  = DiffuseAverage(tBareSolPhi(1:10,IGlass))
      Construct(ConstrNum)%tBareVisDiff(IGlass)  = DiffuseAverage(tBareVisPhi(1:10,IGlass))
      Construct(ConstrNum)%rfBareSolDiff(IGlass) = DiffuseAverage(rfBareSolPhi(1:10,IGlass))
      Construct(ConstrNum)%rfBareVisDiff(IGlass) = DiffuseAverage(rfBareVisPhi(1:10,IGlass))
      Construct(ConstrNum)%rbBareSolDiff(IGlass) = DiffuseAverage(rbBareSolPhi(1:10,IGlass))
      Construct(ConstrNum)%rbBareVisDiff(IGlass) = DiffuseAverage(rbBareVisPhi(1:10,IGlass))
      Construct(ConstrNum)%afBareSolDiff(IGlass) =  &
        MAX(0.0d0,1.d0-(Construct(ConstrNum)%tBareSolDiff(IGlass)+Construct(ConstrNum)%rfBareSolDiff(IGlass)))
      Construct(ConstrNum)%abBareSolDiff(IGlass) =  &
        MAX(0.0d0,1.d0-(Construct(ConstrNum)%tBareSolDiff(IGlass)+Construct(ConstrNum)%rbBareSolDiff(IGlass)))
    END IF
  END DO

!------------------------------------------------------------------------------------------
! Back calculation (solar incident from inside of room); bare glass portion of construction
!------------------------------------------------------------------------------------------

  lquasi = .FALSE.

  ! Loop over glass layers in the construction.
  DO IGlass = 1,NGlass
    LayNum = 1 + (NGlass-IGlass)*2
    IF(ExtShade.OR.ExtBlind.OR.ExtScreen) LayNum = 2 + (NGlass-IGlass)*2
    IF(BGShade.OR.BGBlind) THEN
      IF(NGlass==2) THEN
        IF(IGlass==1) LayNum = 5
        IF(IGlass==2) LayNum = 1
      ELSE  ! NGlass = 3
        IF(IGlass==1) LayNum = 7
        IF(IGlass==2) LayNum = 3
        IF(IGlass==3) LayNum = 1
      END IF
    END IF
    LayPtr = Construct(ConstrNum)%LayerPoint(LayNum)

    SpecDataNum = Material(LayPtr)%GlassSpectralDataPtr
    IF(SpecDataNum /= 0) THEN

      ! Get the spectral data for the transmittance, front reflectance and
      ! back reflectance (all at normal incidence) for this layer.
      ! In this case, "front" means incident from the inside and "back"
      ! means incident from the outside.

      numptDAT = SpectralData(SpecDataNum)%NumOfWavelengths
      numpt(IGlass) = numptDat

      DO ILam = 1,numptDat
        wlt(ILam,IGlass) = SpectralData(SpecDataNum)%Wavelength(ILam)
        t(ILam,IGlass)   = SpectralData(SpecDataNum)%Trans(ILam)
        IF(IGlass==NGlass.OR.(IGlass==(NGlass-1).AND.StormWinConst)) t(ILam,IGlass) = &
           t(ILam,IGlass) * Material(LayPtr)%GlassTransDirtFactor
        rff(ILam,IGlass) = SpectralData(SpecDataNum)%ReflBack(ILam)
        rbb(ILam,IGlass) = SpectralData(SpecDataNum)%ReflFront(ILam)
      END DO

    ELSE  ! No spectral data for this layer; use spectral average values
      lquasi = .TRUE.
      numpt(IGlass) = 2
      t(1,IGlass)   = Material(LayPtr)%Trans
      IF(IGlass==NGlass.OR.(IGlass==(NGlass-1).AND.StormWinConst)) t(1,IGlass) =  &
         t(1,IGlass) * Material(LayPtr)%GlassTransDirtFactor
      t(2,IGlass)   = Material(LayPtr)%TransVis
      IF(IGlass==NGlass.OR.(IGlass==(NGlass-1).AND.StormWinConst)) t(2,IGlass) = &
         t(2,IGlass) * Material(LayPtr)%GlassTransDirtFactor
      rff(1,IGlass) = Material(LayPtr)%ReflectSolBeamBack
      rbb(1,IGlass) = Material(LayPtr)%ReflectSolBeamFront
      rff(2,IGlass) = Material(LayPtr)%ReflectVisBeamBack
      rbb(2,IGlass) = Material(LayPtr)%ReflectVisBeamFront
    END IF
  END DO  ! End of loop over glass layers in the construction for back calculation

  ! Loop over incidence angle from 0 to 90 deg in 10 deg increments.
  ! Get bare glass layer properties, then glazing system properties at each incidence angle.
  ! The glazing system properties include the effect of inter-reflection among glass layers,
  ! but exclude the effect of a shade or blind if present in the construction.
  DO IPhi = 1,10
    Phi = REAL(IPhi-1,r64)*10.d0
    CosPhi = COS(Phi*DegToRadians)
    if (abs(CosPhi) < .0001d0) CosPhi=0.0d0

    ! For each wavelength, get glass layer properties at this angle of incidence
    ! from properties at normal incidence
    DO IGlass = 1,NGlass
      DO ILam = 1,numpt(IGlass)

        CALL TransAndReflAtPhi(CosPhi,t(ILam,IGlass),rff(ILam,IGlass),rbb(ILam,IGlass), &
                        tPhi(ILam,IGlass),rfPhi(ILam,IGlass),rbPhi(ILam,IGlass), &
                        lSimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU)
      END DO
    END DO

    ! For each wavelength in the solar spectrum, calculate system properties
    ! stPhi, srfPhi, srbPhi and saPhi at this angle of incidence
    CALL SystemSpectralPropertiesAtPhi(1,NGlass,0.0d0,2.54d0)

    ! Get back absorptance properties of system by integrating over solar irradiance spectrum.
    ! For now it is assumed that the exterior and interior irradiance spectra are the same.

    DO IGlass=1,NGlass
      DO j=1,nume
        sabsPhi(j) = saPhi(j,IGlass)
      END DO
      CALL SolarSprectrumAverage(sabsPhi,solabsBackPhi(IPhi,IGlass))
   END DO

  END DO  ! End of loop over incidence angles for back calculation

  DO IGlass = 1,NGlass
    IGlassBack = NGlass-IGlass+1
    Construct(ConstrNum)%AbsDiffBack(IGlass) = DiffuseAverage(solabsBackPhi(1:10,IGlassBack))
  END DO

  !-----------------------------------------------------------------------
  ! Correction for effect of shade, screen or blind if present in the construction
  !-----------------------------------------------------------------------

  ! For construction with shade, screen or blind, get system shading device absorptance
  ! and correct the system glass layer absorptances for the effect of reflection
  ! and transmission by shade, screen or blind. Get system reflectance (front and back,
  ! solar and visible)

  IF(ShadeOn.OR.BlindOn.OR.ScreenOn) THEN

    ! Solar and visible properties of isolated shade or blind
    ! (Note: for shades or screen we go through the following loop over slat angles only once.)

    DO ISlatAng = 1,MaxSlatAngs

      IF(ShadeOn) THEN
        ShadeAbs      = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%AbsorpSolar
        ShadeTrans    = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%Trans
        ShadeTransVis = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%TransVis
        ShadeRefl     = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%ReflectShade
        ShadeReflVis  = Material(Construct(ConstrNum)%LayerPoint(ShadeLayNum))%ReflectShadeVis
        rsh  = ShadeRefl
        rshv = ShadeReflVis
        tsh  = ShadeTrans
        tshv = ShadeTransVis
        ash  = ShadeAbs
      ELSE IF(IntBlind.OR.ExtBlind) THEN
        ShadeTrans    = Blind(BlNum)%SolFrontDiffDiffTrans(ISlatAng)
        ShadeTransGnd = Blind(BlNum)%SolFrontDiffDiffTransGnd(ISlatAng)
        ShadeTransSky = Blind(BlNum)%SolFrontDiffDiffTransSky(ISlatAng)
        ShadeTransVis = Blind(BlNum)%VisFrontDiffDiffTrans(ISlatAng)
        IF(IntBlind) THEN   ! Interior blind
          ShadeAbs     = Blind(BlNum)%SolFrontDiffAbs(ISlatAng)
          ShadeRefl    = Blind(BlNum)%SolFrontDiffDiffRefl(ISlatAng)
          ShadeReflGnd = Blind(BlNum)%SolFrontDiffDiffReflGnd(ISlatAng)
          ShadeReflSky = Blind(BlNum)%SolFrontDiffDiffReflSky(ISlatAng)
          ShadeReflVis = Blind(BlNum)%VisFrontDiffDiffRefl(ISlatAng)
        ELSE                ! Exterior blind
          ShadeAbs     = Blind(BlNum)%SolBackDiffAbs(ISlatAng)
          ShadeRefl    = Blind(BlNum)%SolBackDiffDiffRefl(ISlatAng)
          ShadeReflVis = Blind(BlNum)%VisBAckDiffDiffRefl(ISlatAng)
        END IF
      ELSE IF(BGBlind) THEN
          tsh   = Blind(BlNum)%SolFrontDiffDiffTrans(ISlatAng)
          tshGnd= Blind(BlNum)%SolFrontDiffDiffTransGnd(ISlatAng)
          tshSky= Blind(BlNum)%SolFrontDiffDiffTransSky(ISlatAng)
          tshv  = Blind(BlNum)%VisFrontDiffDiffTrans(ISlatAng)
          rfsh  = Blind(BlNum)%SolFrontDiffDiffRefl(ISlatAng)
          rfshGnd = Blind(BlNum)%SolFrontDiffDiffReflGnd(ISlatAng)
          rfshSky = Blind(BlNum)%SolFrontDiffDiffReflSky(ISlatAng)
          rfshv = Blind(BlNum)%VisFrontDiffDiffRefl(ISlatAng)
          rbsh  = Blind(BlNum)%SolBackDiffDiffRefl(ISlatAng)
          rbshv = Blind(BlNum)%VisBackDiffDiffRefl(ISlatAng)
          afsh  = Blind(BlNum)%SolFrontDiffAbs(ISlatAng)
          afshGnd = Blind(BlNum)%SolFrontDiffAbsGnd(ISlatAng)
          afshSky = Blind(BlNum)%SolFrontDiffAbsSky(ISlatAng)
          absh  = Blind(BlNum)%SolBackDiffAbs(ISlatAng)
      ELSE IF(ScreenOn .AND. ScNum .GT. 0) THEN
!       diffuse screen properties are calculated during initialization (quarter-hemispherical integration of beam properties)
        ShadeAbs      = SurfaceScreens(ScNum)%DifScreenAbsorp
        ShadeTrans    = SurfaceScreens(ScNum)%DifDifTrans
        ShadeTransVis = SurfaceScreens(ScNum)%DifDifTransVis
        ShadeRefl     = SurfaceScreens(ScNum)%DifReflect
        ShadeReflVis  = SurfaceScreens(ScNum)%DifReflectVis
        rsh  = ShadeRefl
        rshv = ShadeReflVis
        tsh  = ShadeTrans
        tshv = ShadeTransVis
        ash  = ShadeAbs
      END IF

      ! Correction factors for inter-reflections between glass and shading device

      IF(ExtShade .OR. ExtBlind .OR. ExtScreen) THEN
        ShadeReflFac    = 1.d0 / (1.d0-ShadeRefl*Construct(ConstrNum)%ReflectSolDiffFront)
        ShadeReflFacVis = 1.d0 / (1.d0-ShadeReflVis*Construct(ConstrNum)%ReflectVisDiffFront)
      ELSE IF(IntShade .OR. IntBlind) THEN
        ShadeReflFac    = 1.d0 / (1.d0-ShadeRefl*Construct(ConstrNum)%ReflectSolDiffBack)
        ShadeReflFacVis = 1.d0 / (1.d0-ShadeReflVis*Construct(ConstrNum)%ReflectVisDiffBack)
      END IF

      IF(ExtShade .OR. ExtBlind .OR. ExtScreen) THEN  ! Exterior shade or blind

        ! Front incident solar, beam, exterior shade, screen or blind

        IF(ExtShade) THEN
          DO IPhi = 1,10
            DO IGlass = 1,NGlass
              solabsPhi(IPhi,IGlass) = ShadeTrans * solabsDiff(IGlass) * ShadeReflFac
            END DO
            tsolPhi(IPhi) = ShadeTrans * ShadeReflFac * tsolDiff
            tvisPhi(IPhi) = ShadeTransVis * ShadeReflFacVis * tvisDiff
            solabsShadePhi(IPhi) = ShadeAbs * &
              (1.d0 + ShadeTrans*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffFront)
          END DO
        END IF

        ! Front incident solar, diffuse, exterior shade/screen/blind

        DO IGlass = 1,NGlass
          IF(ExtBlind) THEN
            Construct(ConstrNum)%BlAbsDiff(IGlass,ISlatAng)    = ShadeTrans * ShadeReflFac * solabsDiff(IGlass)
            Construct(ConstrNum)%BlAbsDiffGnd(IGlass,ISlatAng) = ShadeTransGnd * ShadeReflFac * solabsDiff(IGlass)
            Construct(ConstrNum)%BlAbsDiffSky(IGlass,ISlatAng) = ShadeTransSky * ShadeReflFac * solabsDiff(IGlass)
          END IF
          IF(ExtShade .OR. ExtScreen) Construct(ConstrNum)%AbsDiff(IGlass) = ShadeTrans * ShadeReflFac * solabsDiff(IGlass)
        END DO
        IF(ExtBlind) THEN
          Construct(ConstrNum)%AbsDiffBlind(ISlatAng) = Blind(BlNum)%SolFrontDiffAbs(ISlatAng) +  &
             ShadeTrans*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffFront*ShadeAbs
          Construct(ConstrNum)%AbsDiffBlindGnd(ISlatAng) = Blind(BlNum)%SolFrontDiffAbsGnd(ISlatAng) +  &
             ShadeTransGnd*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffFront*ShadeAbs
          Construct(ConstrNum)%AbsDiffBlindSky(ISlatAng) = Blind(BlNum)%SolFrontDiffAbsSky(ISlatAng) +  &
             ShadeTransSky*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffFront*ShadeAbs
          Construct(ConstrNum)%BlTransDiff(ISlatAng)    = tsolDiff * ShadeReflFac * ShadeTrans
          Construct(ConstrNum)%BlTransDiffGnd(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransGnd
          Construct(ConstrNum)%BlTransDiffSky(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransSky
          Construct(ConstrNum)%BlTransDiffVis(ISlatAng) = tvisDiff * ShadeReflFacVis * ShadeTransVis
          Construct(ConstrNum)%BlReflectSolDiffFront(ISlatAng) = ShadeRefl + (ShadeTrans**2) * &
            Construct(ConstrNum)%ReflectSolDiffFront * ShadeReflFac
          Construct(ConstrNum)%BlReflectVisDiffFront(ISlatAng) = ShadeReflVis + (ShadeTransVis**2) * &
            Construct(ConstrNum)%ReflectVisDiffFront * ShadeReflFacVis
        END IF
        IF(ExtShade .OR. ExtScreen) THEN
          Construct(ConstrNum)%AbsDiffShade = ShadeAbs * &
            (1.d0 + ShadeTrans*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffFront)
          Construct(ConstrNum)%TransDiff    = tsolDiff * ShadeReflFac * ShadeTrans
          Construct(ConstrNum)%TransDiffVis = tvisDiff * ShadeReflFacVis * ShadeTransVis
          Construct(ConstrNum)%ReflectSolDiffFront = ShadeRefl + (ShadeTrans**2) * &
            Construct(ConstrNum)%ReflectSolDiffFront * ShadeReflFac
          Construct(ConstrNum)%ReflectVisDiffFront = ShadeReflVis + (ShadeTransVis**2) * &
            Construct(ConstrNum)%ReflectVisDiffFront * ShadeReflFacVis
        END IF

        ! Back incident solar, diffuse, exterior shade/blind

        IF(ExtBlind) THEN
          DO IGlass = 1,NGlass
            Construct(ConstrNum)%BlAbsDiffBack(IGlass,ISlatAng) =  Construct(ConstrNum)%AbsDiffBack(IGlass) +  &
              tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff(IGlass)
          END DO
          Construct(ConstrNum)%AbsDiffBackBlind(ISlatAng) = tsolDiff * ShadeReflFac * ShadeAbs
          Construct(ConstrNum)%BlReflectSolDiffBack(ISlatAng) = Construct(ConstrNum)%ReflectSolDiffBack + &
            (tsolDiff**2) * ShadeRefl * ShadeReflFac
          Construct(ConstrNum)%BlReflectVisDiffBack(ISlatAng) = Construct(ConstrNum)%ReflectVisDiffBack + &
            (tvisDiff**2) * ShadeReflVis * ShadeReflFacVis
        END IF
        IF(ExtShade .OR. ExtScreen) THEN
          DO IGlass = 1,NGlass
            Construct(ConstrNum)%AbsDiffBack(IGlass) =  Construct(ConstrNum)%AbsDiffBack(IGlass) +  &
              tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff(IGlass)
          END DO
          Construct(ConstrNum)%AbsDiffBackShade = tsolDiff * ShadeReflFac * ShadeAbs
          Construct(ConstrNum)%ReflectSolDiffBack = Construct(ConstrNum)%ReflectSolDiffBack + &
            (tsolDiff**2) * ShadeRefl * ShadeReflFac
          Construct(ConstrNum)%ReflectVisDiffBack = Construct(ConstrNum)%ReflectVisDiffBack + &
            (tvisDiff**2) * ShadeReflVis * ShadeReflFacVis
        END IF

      END IF  ! End check if exterior shade, screen or blind

      IF(IntShade.OR.IntBlind) THEN  ! Interior shade or blind

        ! Front incident solar, beam, interior shade

        IF(IntShade) THEN
          DO IPhi = 1,10
            DO IGlass = 1,NGlass
              solabsPhi(IPhi,IGlass) = solabsPhi(IPhi,IGlass) + &
                tsolPhi(IPhi) * ShadeRefl * ShadeReflFac * Construct(ConstrNum)%AbsDiffBack(IGlass)
            END DO
            solabsShadePhi(IPhi) = tsolPhi(IPhi) * ShadeReflFac * ShadeAbs
            tsolPhi(IPhi) = tsolPhi(IPhi) * ShadeReflFac * ShadeTrans
            tvisPhi(IPhi) = tvisPhi(IPhi) * ShadeReflFacVis * ShadeTransVis
          END DO
        END IF  ! End of check if interior shade

        ! Front incident solar, diffuse, interior blind

        IF(IntBlind) THEN
          DO IGlass = 1,NGlass
            Construct(ConstrNum)%BlAbsDiff(IGlass,ISlatAng) =  Construct(ConstrNum)%AbsDiff(IGlass) +  &
              tsolDiff * ShadeRefl * ShadeReflFac * Construct(ConstrNum)%AbsDiffBack(IGlass)
            Construct(ConstrNum)%BlAbsDiffGnd(IGlass,ISlatAng) =  Construct(ConstrNum)%AbsDiff(IGlass) +  &
              tsolDiff * ShadeReflGnd * ShadeReflFac * Construct(ConstrNum)%AbsDiffBack(IGlass)
            Construct(ConstrNum)%BlAbsDiffsky(IGlass,ISlatAng) =  Construct(ConstrNum)%AbsDiff(IGlass) +  &
              tsolDiff * ShadeReflSky * ShadeReflFac * Construct(ConstrNum)%AbsDiffBack(IGlass)
          END DO

          Construct(ConstrNum)%AbsDiffBlind(ISlatAng)   = tsolDiff * ShadeReflFac * ShadeAbs
          Construct(ConstrNum)%AbsDiffBlindGnd(ISlatAng)= tsolDiff * ShadeReflFac * Blind(BlNum)%SolFrontDiffAbsGnd(ISlatAng)
          Construct(ConstrNum)%AbsDiffBlindSky(ISlatAng)= tsolDiff * ShadeReflFac * Blind(BlNum)%SolFrontDiffAbsSky(ISlatAng)
          Construct(ConstrNum)%BlTransDiff(ISlatAng)    = tsolDiff * ShadeReflFac * ShadeTrans
          Construct(ConstrNum)%BlTransDiffGnd(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransGnd
          Construct(ConstrNum)%BlTransDiffSky(ISlatAng) = tsolDiff * ShadeReflFac * ShadeTransSky
          Construct(ConstrNum)%BlTransDiffVis(ISlatAng) = tvisDiff * ShadeReflFacVis * ShadeTransVis
          Construct(ConstrNum)%BlReflectSolDiffFront(ISlatAng) = Construct(ConstrNum)%ReflectSolDiffFront + &
            (tsolDiff**2) * ShadeRefl * ShadeReflFac
          Construct(ConstrNum)%BlReflectVisDiffFront(ISlatAng) = Construct(ConstrNum)%ReflectVisDiffFront + &
            (tvisDiff**2) * ShadeReflVis * ShadeReflFacVis

          ! Back incident solar, diffuse, interior blind

          DO IGlass = 1,NGlass
            Construct(ConstrNum)%BlAbsDiffBack(IGlass,ISlatAng) = &
              Construct(ConstrNum)%AbsDiffBack(IGlass) * ShadeTrans * ShadeReflFac
          END DO

          Construct(ConstrNum)%AbsDiffBackBlind(ISlatAng) = Blind(BlNum)%SolBackDiffAbs(ISlatAng) +  &
            ShadeTrans*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffBack*ShadeAbs
          Construct(ConstrNum)%BlReflectSolDiffBack(ISlatAng) = Blind(BlNum)%SolBackDiffDiffRefl(ISlatAng) + &
            (ShadeTrans**2) * Construct(ConstrNum)%ReflectSolDiffBack * ShadeReflFac
          Construct(ConstrNum)%BlReflectVisDiffBack(ISlatAng) = Blind(BlNum)%VisBackDiffDiffRefl(ISlatAng) + &
            (ShadeTransVis**2) * Construct(ConstrNum)%ReflectVisDiffBack * ShadeReflFacVis
        END IF ! End of check if interior blind

        ! Front incident solar, diffuse, interior shade

        IF(IntShade) THEN
          DO IGlass = 1,NGlass
            Construct(ConstrNum)%AbsDiff(IGlass) =  Construct(ConstrNum)%AbsDiff(IGlass) +  &
              tsolDiff * ShadeRefl * ShadeReflFac * solabsDiff(IGlass)
          END DO

          Construct(ConstrNum)%AbsDiffShade = tsolDiff * ShadeReflFac * ShadeAbs
          Construct(ConstrNum)%TransDiff    = tsolDiff * ShadeReflFac * ShadeTrans
          Construct(ConstrNum)%TransDiffVis = tvisDiff * ShadeReflFacVis * ShadeTransVis
          Construct(ConstrNum)%ReflectSolDiffFront = Construct(ConstrNum)%ReflectSolDiffFront + &
            (tsolDiff**2) * ShadeRefl * ShadeReflFac
          Construct(ConstrNum)%ReflectVisDiffFront = Construct(ConstrNum)%ReflectVisDiffFront + &
            (tvisDiff**2) * ShadeReflVis * ShadeReflFacVis

          ! Back incident solar, diffuse, interior shade

          DO IGlass = 1,NGlass
            Construct(ConstrNum)%AbsDiffBack(IGlass) = &
              Construct(ConstrNum)%AbsDiffBack(IGlass) * ShadeTrans * ShadeReflFac
          END DO

          Construct(ConstrNum)%AbsDiffBackShade = ShadeAbs * &
            (1 + ShadeTrans*ShadeReflFac*Construct(ConstrNum)%ReflectSolDiffBack)
          Construct(ConstrNum)%ReflectSolDiffBack = ShadeRefl + (ShadeTrans**2) * &
            Construct(ConstrNum)%ReflectSolDiffBack * ShadeReflFac
          Construct(ConstrNum)%ReflectVisDiffBack = ShadeReflVis + (ShadeTransVis**2) * &
            Construct(ConstrNum)%ReflectVisDiffBack * ShadeReflFacVis
        END IF ! End of check if interior shade

      END IF  ! End check if interior shade or blind

      IF(BGShade.OR.BGBlind) THEN    ! Between-glass shade/blind; assumed to be between glass #2 and glass #3

        tsh2  = tsh**2
        tshv2 = tshv**2
        td1   = Construct(ConstrNum)%tBareSolDiff(1)
        td2   = Construct(ConstrNum)%tBareSolDiff(2)
        td1v  = Construct(ConstrNum)%tBareVisDiff(1)
        td2v  = Construct(ConstrNum)%tBareVisDiff(2)
        afd1  = Construct(ConstrNum)%afBareSolDiff(1)
        afd2  = Construct(ConstrNum)%afBareSolDiff(2)
        abd1  = Construct(ConstrNum)%abBareSolDiff(1)
        abd2  = Construct(ConstrNum)%abBareSolDiff(2)
        rb1   = Construct(ConstrNum)%rbBareSolDiff(1)
        rb2   = Construct(ConstrNum)%rbBareSolDiff(2)
        rb1v  = Construct(ConstrNum)%rbBareVisDiff(1)
        rb2v  = Construct(ConstrNum)%rbBareVisDiff(2)
        rf1   = Construct(ConstrNum)%rfBareSolDiff(1)
        rf2   = Construct(ConstrNum)%rfBareSolDiff(2)
        rf1v  = Construct(ConstrNum)%rfBareVisDiff(1)
        rf2v  = Construct(ConstrNum)%rfBareVisDiff(2)

        IF(BGShade) THEN
          IF(NGlass==2) THEN

            ! Front incident solar, beam, between-glass shade, NGlass = 2

            DO IPhi = 1,10
              t1 = tBareSolPhi(IPhi,1)
              t1v = tBareVisPhi(IPhi,1)
              af1 = afBareSolPhi(IPhi,1)
              ab1 = abBareSolPhi(IPhi,1)
              tsolPhi(IPhi) = t1*(tsh + rsh*rb1*tsh + tsh*rf2*rsh)*td2
              tvisPhi(IPhi) = t1v*(tshv + rshv*rb1v*tshv + tshv*rf2v*rshv)*td2v
              solabsShadePhi(IPhi) = t1*(ash + rsh*rb1 + tsh*rf2)*ash
              solabsPhi(IPhi,1) = af1 + t1*(rsh + rsh*rb1*rsh + tsh*rf2*tsh)*abd1
              solabsPhi(IPhi,2) = t1*(tsh + rsh*rb1*tsh + tsh*rf2*rsh)*afd2
            END DO  ! End of loop over incidence angles

            ! Front incident solar, diffuse, between-glass shade, NGlass = 2

            Construct(ConstrNum)%TransDiff    = td1*(tsh + rsh*rb1*tsh + tsh*rb2*rsh)*td2
            Construct(ConstrNum)%TransDiffVis = td1v*(tshv + rshv*rb1v*tshv + tshv*rb2v*rshv)*td2v
            Construct(ConstrNum)%AbsDiffShade = td1*(ash + rsh*rb1*ash + tsh*rf2*ash)
            Construct(ConstrNum)%AbsDiff(1)   = afd1 + td1*(rsh + tsh*rb2*tsh)*abd1
            Construct(ConstrNum)%AbsDiff(2)   = td1*(tsh + rsh*rb1*tsh + tsh*rf2*rsh)*afd2
            Construct(ConstrNum)%ReflectSolDiffFront = rf1 + td1*(rsh + rsh*rb1*rsh + tsh*rf2*tsh)*td1
            Construct(ConstrNum)%ReflectVisDiffFront = rf1v + td1v*(rshv + rshv*rb1v*rshv + tshv*rf2v*tshv)*td1v

            ! Back incident solar, diffuse, between-glass shade, NGlass = 2

            Construct(ConstrNum)%AbsDiffBackShade = td2*(ash + rsh*rf2*ash + tsh*rb1*ash)
            Construct(ConstrNum)%AbsDiffBack(1)   = td2*(tsh + rsh*rf2*tsh + tsh*rb1*rsh)*abd1
            Construct(ConstrNum)%AbsDiffBack(2)   = abd2 + td2*(rsh + rsh*rf2*rsh + tsh*rb1*tsh)*afd2
            Construct(ConstrNum)%ReflectSolDiffBack = rb2 + td2*(rsh + rsh*rf2*rsh + tsh*rb1*tsh)*td2
            Construct(ConstrNum)%ReflectVisDiffBack = rb2v + td2v*(rshv + rshv*rf2v*rshv + tshv*rb1v*tshv)*td2v

          END IF  ! End of check if NGlass = 2

          IF(NGlass==3) THEN

            td3  = Construct(ConstrNum)%tBareSolDiff(3)
            td3v = Construct(ConstrNum)%tBareVisDiff(3)
            afd3 = Construct(ConstrNum)%afBareSolDiff(3)
            abd3 = Construct(ConstrNum)%abBareSolDiff(3)
            rb3  = Construct(ConstrNum)%rbBareSolDiff(3)
            rb3v = Construct(ConstrNum)%rbBareVisDiff(3)
            rf3  = Construct(ConstrNum)%rfBareSolDiff(3)
            rf3v = Construct(ConstrNum)%rfBareVisDiff(3)

            ! Front incident solar, beam, between-glass shade, NGlass = 3

            DO IPhi = 1,10
              t1  = tBareSolPhi(IPhi,1)
              t1v = tBareVisPhi(IPhi,1)
              t2  = tBareSolPhi(IPhi,2)
              t2v = tBareVisPhi(IPhi,2)
              af1 = afBareSolPhi(IPhi,1)
              af2 = afBareSolPhi(IPhi,2)
              ab1 = abBareSolPhi(IPhi,1)
              ab2 = abBareSolPhi(IPhi,2)
              rbmf2 = MAX(0.0d0,1.d0-(t2+af2))

              tsolPhi(IPhi) = t1*t2*(tsh + tsh*rf3*rsh + rsh*td2*rb1*td2*tsh + rsh*rb2*tsh)*td3
              tvisPhi(IPhi) = t1v*t2v*(tshv + tshv*rf3v*rshv + rshv*td2v*rb1v*td2v*tshv + rshv*rb2v*tshv)*td3v
              solabsShadePhi(IPhi)  = t1*t2*(1 + rsh*td2*rb1*td2 + rsh*rb2)*ash
              solabsPhi(IPhi,1) = af1 + rbmf2*ab1 + &
                                  t1*t2*rsh*(1 + rf3*tsh + rb2*rsh + td2*rb1*td2*rsh)*td2*abd1
              solabsPhi(IPhi,2) = t1*af2 + t1*t2*((rsh + tsh*rf3*tsh + rsh*rb2*rsh)*abd2 + rsh*td2*rb1*afd2)
              solabsPhi(IPhi,3) = t1*t2*(tsh + rsh*(rb2*tsh + td2*rb2*td2*tsh + rf3*rsh))*afd3
            END DO  ! End of loop over incidence angle

            ! Front incident solar, diffuse, between-glass shade, NGlass = 3

            Construct(ConstrNum)%TransDiff    = td1*td2*(tsh + rsh*td2*rb1*td2*tsh + rsh*rb2*tsh + tsh*rf3*rsh)*td3
            Construct(ConstrNum)%TransDiffVis = &
                     td1v*td2v*(tshv + rshv*td2v*rb1v*td2v*tshv + rshv*rb2v*tshv + tshv*rf3v*rshv)*td3v
            Construct(ConstrNum)%AbsDiffShade = td1*td2*(ash*(1 + rsh*td2*rb1*td2 + rsh*rb2*ash) + tsh*rf3*ash)
            Construct(ConstrNum)%AbsDiff(1) = &
                     afd1 + td1*(rf2 + td2*(rsh + rsh*rb2*rsh + tsh*rf3*tsh + rsh*td2*rb1*td2*rsh)*td2)*abd1
            Construct(ConstrNum)%AbsDiff(2) = td1*(afd2 + td2*(rsh + rsh*rb2*rsh + tsh*rf3*tsh)*abd2)
            Construct(ConstrNum)%AbsDiff(3) = td1*td2*(tsh + rsh*rb2*tsh + rsh*td2*rb1*td2*tsh + tsh*rf3*rsh)*afd3
            Construct(ConstrNum)%ReflectSolDiffFront = &
                     rf1 + td1*rf2*td1 + td1*td2*(rsh + tsh*rf3*tsh + rsh*rb2*rsh + rsh*td2*rb1*td2*rsh)*td2*td1
            Construct(ConstrNum)%ReflectVisDiffFront = &
                     rf1v + td1v*rf2v*td1v + td1v*td2v*(rshv + tshv*rf3v*tshv + rshv*rb2v*rshv + &
                     rshv*td2v*rb1v*td2v*rshv)*td2v*td1v

            ! Back incident solar, diffuse, between-glass shade, NGlass = 3

            Construct(ConstrNum)%AbsDiffBackShade   = td3*((1 + rsh*rf3)*ash + (tsh*td2*rb1*td2 + tsh*rb2)*ash)
            Construct(ConstrNum)%AbsDiffBack(1)     = td3*(tsh + rsh*rf3*tsh + tsh*rb2*rsh + tsh*td2*rb1*td2*rsh)*td2*abd1
            Construct(ConstrNum)%AbsDiffBack(2)     = td3*((tsh + rsh*rf3*tsh)*abd2 + (tsh*td2*rb1*td2 + tsh*rb2)*afd2)
            Construct(ConstrNum)%AbsDiffBack(3)     = abd3 + td3*(rsh + tsh*rb2*tsh + tsh*td2*rb1*td2*tsh)*afd3
            Construct(ConstrNum)%ReflectSolDiffBack = rb3 + td3*(rsh + rsh*rf3*rsh + tsh*rb2*tsh + tsh*td2*rb1*td2*tsh)*td3
            Construct(ConstrNum)%ReflectVisDiffBack = &
                     rb3v + td3v*(rshv + rshv*rf3*rshv + tshv*rb2v*tshv + tshv*td2v*rb1v*td2v*tshv)*td3v

          END IF  ! End of check if NGlass = 3

        END IF  ! End of check if between-glass shade

        IF(BGBlind) THEN

          IF(NGlass==2) THEN

            ! Front incident solar, diffuse, between-glass blind, NGlass = 2

            Construct(ConstrNum)%BlAbsDiff(1,ISlatAng)    = afd1 + td1*(rfsh + rfsh*rb1*rfsh + tsh*rb2*tsh)*abd1
            Construct(ConstrNum)%BlAbsDiffGnd(1,ISlatAng) = afd1 + td1*(rfshGnd + rfshGnd*rb1*rfshGnd + tshGnd*rb2*tsh)*abd1
            Construct(ConstrNum)%BlAbsDiffSky(1,ISlatAng) = afd1 + td1*(rfshSky + rfshSky*rb1*rfshSky + tshSky*rb2*tsh)*abd1
            Construct(ConstrNum)%BlAbsDiff(2,ISlatAng)    = td1*(tsh + rfsh*rb1*tsh + tsh*rf2*rbsh)*afd2
            Construct(ConstrNum)%BlAbsDiffGnd(2,ISlatAng) = td1*(tshGnd + rfshGnd*rb1*tsh + tshGnd*rf2*rbsh)*afd2
            Construct(ConstrNum)%BlAbsDiffSky(2,ISlatAng) = td1*(tshSky + rfshSky*rb1*tsh + tshSky*rf2*rbsh)*afd2
            Construct(ConstrNum)%AbsDiffBlind(ISlatAng)   = td1*(afsh + rfsh*rb1*afsh + tsh*rf2*absh)
            Construct(ConstrNum)%AbsDiffBlindGnd(ISlatAng)= td1*(afshGnd + rfsh*rb1*afsh + tshGnd*rf2*absh)
            Construct(ConstrNum)%AbsDiffBlindSky(ISlatAng)= td1*(afshSky + rfsh*rb1*afsh + tshSky*rf2*absh)
            Construct(ConstrNum)%BlTransDiff(ISlatAng)    = td1*(tsh + rfsh*rb1*tsh + tsh*rb2*rbsh)*td2
            Construct(ConstrNum)%BlTransDiffGnd(ISlatAng) = td1*(tshGnd + rfsh*rb1*tshGnd + tshGnd*rb2*rbsh)*td2
            Construct(ConstrNum)%BlTransDiffSky(ISlatAng) = td1*(tshSky + rfsh*rb1*tshSky + tshSky*rb2*rbsh)*td2
            Construct(ConstrNum)%BlTransDiffVis(ISlatAng) = td1v*(tshv + rfshv*rb1v*tshv + tshv*rb2v*rbshv)*td2v
            Construct(ConstrNum)%BlReflectSolDiffFront(ISlatAng) = rf1 + td1*(rfsh + rfsh*rb1*rfsh + tsh*rf2*tsh)*td1
            Construct(ConstrNum)%BlReflectVisDiffFront(ISlatAng) = &
                                     rf1v + td1v*(rfshv + rfshv*rb1v*rfshv + tshv*rf2v*tshv)*td1v

            ! Back incident solar, diffuse, between-glass blind, NGlass = 2

            Construct(ConstrNum)%BlAbsDiffBack(1,ISlatAng) = td2*(tsh + rbsh*rf2*tsh + tsh*rb1*rfsh)*abd1
            Construct(ConstrNum)%BlAbsDiffBack(2,ISlatAng) = abd2 + td2*(rbsh + rbsh*rf2*rbsh + tsh*rb1*tsh)*afd2
            Construct(ConstrNum)%AbsDiffBackBlind(ISlatAng) = td2*(absh + rbsh*rf2*absh + tsh*rb1*afsh)
            Construct(ConstrNum)%BlReflectSolDiffBack(ISlatAng) = rb2 + td2*(rbsh + rbsh*rf2*rbsh + tsh*rb1*tsh)*td2
            Construct(ConstrNum)%BlReflectVisDiffBack(ISlatAng) = &
                                     rb2v + td2v*(rbshv + rbshv*rf2v*rbshv + tshv*rb1v*tshv)*td2v

          END IF  ! End of check if NGlass = 2

          IF(NGlass==3) THEN

            td3  = Construct(ConstrNum)%tBareSolDiff(3)
            td3v = Construct(ConstrNum)%tBareVisDiff(3)
            afd3 = Construct(ConstrNum)%afBareSolDiff(3)
            abd3 = Construct(ConstrNum)%abBareSolDiff(3)
            rb3  = Construct(ConstrNum)%rbBareSolDiff(3)
            rb3v = Construct(ConstrNum)%rbBareVisDiff(3)
            rf3  = Construct(ConstrNum)%rfBareSolDiff(3)
            rf3v = Construct(ConstrNum)%rfBareVisDiff(3)

            ! Front incident solar, diffuse, between-glass blind, NGlass = 3

            Construct(ConstrNum)%BlAbsDiff(1,ISlatAng)          = &
                        afd1 + td1*(rf2 + td2*(rfsh + rfsh*rb2*rfsh + tsh*rf3*tsh + rfsh*td2*rb1*td2*rfsh)*td2)*abd1
            Construct(ConstrNum)%BlAbsDiffGnd(1,ISlatAng)       = &
                        afd1 + td1*(rf2 + td2*(rfshGnd + rfshGnd*rb2*rfsh + tshGnd*rf3*tsh + rfshGnd*td2*rb1*td2*rfsh)*td2)*abd1
            Construct(ConstrNum)%BlAbsDiffSky(1,ISlatAng)       = &
                        afd1 + td1*(rf2 + td2*(rfshSky + rfshSky*rb2*rfsh + tshSky*rf3*tsh + rfshSky*td2*rb1*td2*rfsh)*td2)*abd1
            Construct(ConstrNum)%BlAbsDiff(2,ISlatAng)          = &
                        td1*(afd2 + td2*(rfsh + rfsh*rb2*rfsh + tsh*rf3*tsh)*abd2)
            Construct(ConstrNum)%BlAbsDiffGnd(2,ISlatAng)       = &
                        td1*(afd2 + td2*(rfshGnd + rfshGnd*rb2*rfsh + tshGnd*rf3*tsh)*abd2)
            Construct(ConstrNum)%BlAbsDiffSky(2,ISlatAng)       = &
                        td1*(afd2 + td2*(rfshSky + rfshSky*rb2*rfsh + tshSky*rf3*tsh)*abd2)
            Construct(ConstrNum)%BlAbsDiff(3,ISlatAng)           = &
                        td1*td2*(tsh + rfsh*rb2*tsh + rfsh*td2*rb1*td2*tsh + tsh*rf3*rbsh)*afd3
            Construct(ConstrNum)%BlAbsDiffGnd(3,ISlatAng)        = &
                        td1*td2*(tshGnd + rfshGnd*rb2*tsh + rfshGnd*td2*rb1*td2*tsh + tshGnd*rf3*rbsh)*afd3
            Construct(ConstrNum)%BlAbsDiffSky(3,ISlatAng)        = &
                        td1*td2*(tshSky + rfshSky*rb2*tsh + rfshSky*td2*rb1*td2*tsh + tshSky*rf3*rbsh)*afd3
            Construct(ConstrNum)%AbsDiffBlind(ISlatAng)          = &
                        td1*td2*(afsh*(1 + rfsh*td2*rb1*td2) + rfsh*rb2*afsh + tsh*rf3*absh)
            Construct(ConstrNum)%AbsDiffBlindGnd(ISlatAng)       = &
                        td1*td2*(afshGnd + afsh*rfsh*(td2*rb1*td2 + rb2) + tshGnd*rf3*absh)
            Construct(ConstrNum)%AbsDiffBlindSky(ISlatAng)       = &
                        td1*td2*(afshSky + afsh*rfsh*(td2*rb1*td2 + rb2) + tshSky*rf3*absh)
            Construct(ConstrNum)%BlTransDiff(ISlatAng)           = &
                        td1*td2*(tsh + rfsh*td2*rb1*td2*tsh + rfsh*rb2*tsh + tsh*rf3*rbsh)*td3
            Construct(ConstrNum)%BlTransDiffGnd(ISlatAng)        = &
                        td1*td2*(tshGnd + rfsh*td2*rb1*td2*tshGnd + rfsh*rb2*tshGnd + tshGnd*rf3*rbsh)*td3
            Construct(ConstrNum)%BlTransDiffSky(ISlatAng)        = &
                        td1*td2*(tshSky + rfsh*td2*rb1*td2*tshSky + rfsh*rb2*tshSky + tshSky*rf3*rbsh)*td3
            Construct(ConstrNum)%BlTransDiffVis(ISlatAng)        = &
                        td1v*td2v*(tshv + rfshv*td2v*rb1v*td2v*tshv + rfshv*rb2v*tshv + tshv*rf3v*rbshv)*td3v
            Construct(ConstrNum)%BlReflectSolDiffFront(ISlatAng) = &
                        rf1 + td1*rf2*td1 + td1*td2*(rfsh + tsh*rf3*tsh + rfsh*rb2*rfsh + rfsh*td2*rb1*td2*rfsh)*td2*td1
            Construct(ConstrNum)%BlReflectVisDiffFront(ISlatAng) = &
                        rf1v + td1v*rf2v*td1v + td1v*td2v*(rfshv + tshv*rf3v*tshv + rfshv*rb2v*rfshv +   &
                        rfshv*td2v*rb1v*td2v*rfshv)*td2v*td1v

            ! Back incident solar, diffuse, between-glass blind, NGlass = 3

            Construct(ConstrNum)%BlAbsDiffBack(1,ISlatAng)      = &
                        td3*(tsh + rbsh*rf3*tsh + tsh*rb2*rfsh + tsh*td2*rb1*td2*rfsh)*td2*abd1
            Construct(ConstrNum)%BlAbsDiffBack(2,ISlatAng)      = &
                        td3*((tsh + rbsh*rf3*tsh)*abd2 + (tsh*td2*rb1*td2 + tsh*rb2)*afd2)
            Construct(ConstrNum)%BlAbsDiffBack(3,ISlatAng)      = &
                        abd3 + td3*(rbsh + tsh*rb2*tsh + tsh*td2*rb1*td2*tsh)*afd3
            Construct(ConstrNum)%AbsDiffBackBlind(ISlatAng)     = &
                        td3*((1 + rbsh*rf3)*absh + (tsh*td2*rb1*td2 + tsh*rb2)*afsh)
            Construct(ConstrNum)%BlReflectSolDiffBack(ISlatAng) = &
                        rb3 + td3*(rbsh + rbsh*rf3*rbsh + tsh*rb2*tsh + tsh*td2*rb1*td2*tsh)*td3
            Construct(ConstrNum)%BlReflectVisDiffBack(ISlatAng) = &
                        rb3v + td3v*(rbshv + rbshv*rf3v*rbshv + tshv*rb2v*tshv + tshv*td2v*rb1v*td2v*tshv)*td3v

          END IF  ! End of check if NGlass = 3

        END IF  ! End of check if between-glass blind

      END IF  ! End of check if between-glass shade or blind

              ! Continue loop over slat angles only for blinds with variable slat angle
      IF(ShadeOn .OR. ScreenOn) EXIT
      IF(BlindOn) THEN
        IF(Blind(BlNum)%SlatAngleType == FixedSlats) EXIT
      END IF
    END DO  ! End of slat angle loop
  END IF  ! End of check if construction has a shade or blind

  ! Curve fits to get solar transmittance, reflectance, layer absorptance and
  ! visible transmittance as polynomials in cosine of incidence angle

  IF(.NOT. BlindOn .AND. .NOT. ScreenOn) THEN  ! Bare glass or shade on
    CALL W5LsqFit(CosPhiIndepVar,tsolPhi,6,1,10,Construct(ConstrNum)%TransSolBeamCoef(1:6))
    CALL W5LsqFit(CosPhiIndepVar,rfsolPhi,6,1,10,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
    CALL W5LsqFit(CosPhiIndepVar,rbsolPhi,6,1,10,Construct(ConstrNum)%ReflSolBeamBackCoef(1:6))
    CALL W5LsqFit(CosPhiIndepVar,tvisPhi,6,1,10,Construct(ConstrNum)%TransVisBeamCoef)
    DO IGlass = 1,NGlass
      ! Front absorptance coefficients for glass layers
      DepVarCurveFit(1:10) = solabsPhi(1:10,IGlass)
      CALL W5LsqFit(CosPhiIndepVar,DepVarCurveFit,6,1,10,CoeffsCurveFit)
      Construct(ConstrNum)%AbsBeamCoef(IGlass,1:6) = CoeffsCurveFit
      ! Back absorptance coefficients for glass layers
      IGlassBack = NGlass-IGlass+1
      DepVarCurveFit(1:10) = solabsBackPhi(1:10,IGlassBack)
      CALL W5LsqFit(CosPhiIndepVar,DepVarCurveFit,6,1,10,CoeffsCurveFit)
      Construct(ConstrNum)%AbsBeamBackCoef(IGlass,1:6) = CoeffsCurveFit
    END DO

    ! To check goodness of fit
    DO IPhi = 1,10
      tsolPhiFit(IPhi) = 0.d0
      tvisPhiFit(IPhi) = 0.d0
      Phi = REAL(IPhi-1,r64)*10.d0
      CosPhi = COS(Phi*DegToRadians)
      if (abs(CosPhi) < .0001d0) CosPhi=0.0d0
      DO CoefNum = 1,6
        tsolPhiFit(IPhi) = tsolPhiFit(IPhi) + Construct(ConstrNum)%TransSolBeamCoef(CoefNum)* &
                           CosPhi**CoefNum
        tvisPhiFit(IPhi) = tvisPhiFit(IPhi) + Construct(ConstrNum)%TransVisBeamCoef(CoefNum)* &
                           CosPhi**CoefNum
      END DO
    END DO
  END IF

  IF(ShadeOn) CALL W5LsqFit(CosPhiIndepVar,solabsShadePhi,6,1,10,Construct(ConstrNum)%AbsBeamShadeCoef)

END DO  ! End of loop over constructions

! Get effective glass and shade/blind emissivities for windows that have interior blind or
! shade. These are used to calculate zone MRT contribution from window when
! interior blind/shade is deployed.

DO SurfNum = 1,TotSurfaces
  IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
  IF(.NOT.Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
  IF( SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel ) CYCLE  !Irrelevant for Complex Fen
  IF( Construct(Surface(SurfNum)%Construction)%WindowTypeEQL ) CYCLE       ! not required
  ConstrNumSh = SurfaceWindow(SurfNum)%ShadedConstruction
  IF(ConstrNumSh == 0) CYCLE
  TotLay = Construct(ConstrNumSh)%TotLayers
  IntShade = .FALSE.
  IntBlind = .FALSE.
  IF(Material(Construct(ConstrNumSh)%LayerPoint(TotLay))%Group == Shade) THEN
    IntShade = .TRUE.
    ShadeLayPtr = Construct(ConstrNumSh)%LayerPoint(TotLay)
  END IF
  IF(Material(Construct(ConstrNumSh)%LayerPoint(TotLay))%Group == WindowBlind) THEN
    IntBlind = .TRUE.
    BlNum = Material(Construct(ConstrNumSh)%LayerPoint(TotLay))%BlindDataPtr
  END IF

  IF(IntShade.OR.IntBlind) THEN
    DO ISlatAng = 1,MaxSlatAngs
      IF(IntShade.OR.IntBlind) THEN
        EpsGlIR = Material(Construct(ConstrNumSh)%LayerPoint(TotLay-1))%AbsorpThermalBack
        RhoGlIR = 1-EpsGlIR
      END IF
      IF(IntShade) THEN
        TauShIR = Material(ShadeLayPtr)%TransThermal
        EpsShIR = Material(ShadeLayPtr)%AbsorpThermal
        RhoShIR = MAX(0.d0,1.d0-TauShIR-EpsShIR)
        SurfaceWindow(SurfNum)%EffShBlindEmiss(1) = EpsShIR*(1.d0+RhoGlIR*TauShIR/(1.d0-RhoGlIR*RhoShIR))
        SurfaceWindow(SurfNum)%EffGlassEmiss(1)   = EpsGlIR*TauShIR/(1.d0-RhoGlIR*RhoShIR)
      END IF
      IF(IntBlind) THEN
        TauShIR = Blind(BlNum)%IRFrontTrans(ISlatAng)
        EpsShIR = Blind(BlNum)%IRBackEmiss(ISlatAng)
        RhoShIR = MAX(0.d0,1.d0-TauShIR-EpsShIR)
        SurfaceWindow(SurfNum)%EffShBlindEmiss(ISlatAng) = EpsShIR*(1.d0+RhoGlIR*TauShIR/(1.d0-RhoGlIR*RhoShIR))
        SurfaceWindow(SurfNum)%EffGlassEmiss(ISlatAng)   = EpsGlIR*TauShIR/(1.d0-RhoGlIR*RhoShIR)
      END IF
           ! Loop over remaining slat angles only if blind with movable slats
      IF(IntShade) EXIT  ! Loop over remaining slat angles only if blind
      IF(IntBlind) THEN
        IF(Blind(BlNum)%SlatAngleType == FixedSlats) EXIT
      END IF
    END DO  ! End of slat angle loop
  END IF  ! End of check if interior shade or interior blind
END DO  ! End of surface loop

DO SurfNum = 1,TotSurfaces
  IF(Surface(SurfNum)%Construction <= 0) CYCLE
  IF(.NOT.Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
    ConstrNum = Surface(SurfNum)%Construction
    ! Total thickness of glazing system (used in calculation of inside reveal reflection/absorption
    SurfaceWindow(SurfNum)%TotGlazingThickness = 0.0d0
    DO LayNum = 1,Construct(ConstrNum)%TotLayers
      SurfaceWindow(SurfNum)%TotGlazingThickness = SurfaceWindow(SurfNum)%TotGlazingThickness + &
        Material(Construct(ConstrNum)%LayerPoint(LayNum))%Thickness
    END DO
    ! Sine and cosine of azimuth and tilt
!    SurfaceWindow(SurfNum)%SinAzim = Surface(SurfNum)%SinAzim
!    SurfaceWindow(SurfNum)%CosAzim = Surface(SurfNum)%CosAzim
!    SurfaceWindow(SurfNum)%SinTilt = Surface(SurfNum)%SinTilt
!    SurfaceWindow(SurfNum)%CosTilt = Surface(SurfNum)%CosTilt
!    ! Outward normal unit vector (pointing away from room)
!    SurfaceWindow(SurfNum)%OutNormVec(1) = Surface(SurfNum)%OutNormVec(1)
!    SurfaceWindow(SurfNum)%OutNormVec(2) = Surface(SurfNum)%OutNormVec(2)
!    SurfaceWindow(SurfNum)%OutNormVec(3) = Surface(SurfNum)%OutNormVec(3)
!    write(outputfiledebug,*) 'window='//trim(surface(surfnum)%name)
!    write(outputfiledebug,*) '  swindow%outnormvec=',surfacewindow(surfnum)%outnormvec
!    write(outputfiledebug,*) '  surface%outnormvec=',surface(surfnum)%outnormvec
    ! Window center
    Rectangle = .FALSE.
    Triangle = .FALSE.
    IF(Surface(SurfNum)%Sides == 3) Triangle = .TRUE.
    IF(Surface(SurfNum)%Sides == 4) Rectangle = .TRUE.
    IF(Rectangle) THEN
      ! Vertices of window (numbered counter-clockwise starting at upper left as viewed from inside of room).
      ! Assumes original vertices are numbered counter-clockwise from upper left as viewed from outside.
      W3 = Surface(SurfNum)%Vertex(2)
      W2 = Surface(SurfNum)%Vertex(3)
      W1 = Surface(SurfNum)%Vertex(4)
    ELSE IF (Triangle) THEN
      W3 = Surface(SurfNum)%Vertex(2)
      W2 = Surface(SurfNum)%Vertex(3)
      W1 = Surface(SurfNum)%Vertex(1)
    END IF
    W21 = W1 - W2
    W23 = W3 - W2
    IF(Rectangle) THEN
      SurfaceWindow(SurfNum)%WinCenter = W2 + (W23 + W21) / 2.0d0
    ELSE IF(Triangle) THEN
      SurfaceWindow(SurfNum)%WinCenter = W2 + (W23 + W21) / 3.0d0
    END IF
END DO  ! End of surface loop

CALL ReportGlass

RETURN
END SUBROUTINE InitGlassOpticalCalculations

!*****************************************************************************************
SUBROUTINE W5InitGlassParameters

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   October 1999
          !       MODIFIED       Aug 2001 (FW): add blinds
          !                      Oct 2002 (FW): change ConstrNumSh =
          !         WindowShadingControl(Surface(SurfNum)%WindowShadingControlPtr)%ShadedConstruction
          !         to Surface(SurfNum)%ShadedConstruction
          !                      Jul 2003 (FW): remove unneeded warning if center-of-glass area < 0
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes variables used in the window optical and thermal calculation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE General, ONLY: RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER           :: ConstrNum               ! Construction number
INTEGER           :: SurfNum                 ! Surface number
INTEGER           :: IPhi                    ! Angle of incidence counter
INTEGER           :: FrDivNum                ! Pointer to frame/divider
REAL(r64)         :: FrWidth                 ! Window frame width {m}
REAL(r64)         :: FrEdgeWidth             ! Frame edge width {m}
REAL(r64)         :: DivWidth                ! Window divider width {m}
REAL(r64)         :: DivEdgeWidth            ! Divider edge width {m}
REAL(r64)         :: GlHeight                ! Height of glazed part of window {m}
REAL(r64)         :: GlWidth                 ! Width of glazed part of window {m}
INTEGER           :: NumHorDividers          ! Number of horizontal divider elements
INTEGER           :: NumVertDividers         ! Number of vertical divider elements
INTEGER           :: BaseSurfNum             ! Base surface number
INTEGER :: ShadingType             ! Window shading type
INTEGER           :: MatNum                  ! Material number
INTEGER   :: DifOverrideCount        ! Count the number of SolarDiffusing material overrides

          ! FLOW

DO ConstrNum = 1,TotConstructs
  IF(Construct(ConstrNum)%FromWindow5DataFile) CYCLE
  IF(Construct(ConstrNum)%WindowTypeBSDF) CYCLE
  Construct(ConstrNum)%TransDiff            = 0.0d0
  Construct(ConstrNum)%TransDiffVis         = 0.0d0
  Construct(ConstrNum)%AbsDiffBackShade     = 0.0d0
  Construct(ConstrNum)%ShadeAbsorpThermal   = 0.0d0
  Construct(ConstrNum)%ReflectSolDiffBack   = 0.0d0
  Construct(ConstrNum)%ReflectSolDiffFront  = 0.0d0
  Construct(ConstrNum)%ReflectVisDiffFront  = 0.0d0
  Construct(ConstrNum)%AbsBeamShadeCoef     = 0.0d0
  Construct(ConstrNum)%TransSolBeamCoef     = 0.0d0
  Construct(ConstrNum)%ReflSolBeamFrontCoef = 0.0d0
  Construct(ConstrNum)%ReflSolBeamBackCoef  = 0.0d0
  Construct(ConstrNum)%TransVisBeamCoef     = 0.0d0
  Construct(ConstrNum)%AbsBeamCoef          = 0.0d0
  Construct(ConstrNum)%AbsBeamBackCoef      = 0.0d0
  Construct(ConstrNum)%AbsDiff              = 0.0d0
  Construct(ConstrNum)%AbsDiffBack          = 0.0d0
END DO

DO SurfNum = 1,TotSurfaces
  ! For a window with shading device, get number of shaded construction and, if window
  ! has a blind (interior, exterior or between glass), get blind data pointer.

  ! TH 2/16/2010. CR 8010. The following code was modified and moved to GetSurfaceData
  !  in SurfaceGeometry module, because for blinds with variable slats new blinds were created and assigned
  IF(Surface(SurfNum)%WindowShadingControlPtr /= 0) THEN
  !  ConstrNumSh = Surface(SurfNum)%ShadedConstruction
    ShadingType = WindowShadingControl(Surface(SurfNum)%WindowShadingControlPtr)%ShadingType
  !  IF(ShadingType == WSC_ST_ExteriorBlind) THEN
  !    MatNum = Construct(ConstrNumSh)%LayerPoint(1)
  !    SurfaceWindow(SurfNum)%BlindNumber = Material(MatNum)%BlindDataPtr
  !  ELSE IF(ShadingType == WSC_ST_InteriorBlind) THEN
  !    MatNum = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
  !    SurfaceWindow(SurfNum)%BlindNumber = Material(MatNum)%BlindDataPtr
    ! Between glass blind is layer 3 for double glazing and layer 5 for triple glazing.
  !  ELSE IF(ShadingType == WSC_ST_BetweenGlassBlind) THEN
  !    IF(Construct(ConstrNumSh)%TotGlassLayers == 2) THEN
  !      SurfaceWindow(SurfNum)%BlindNumber = Material(Construct(ConstrNumSh)%LayerPoint(3))%BlindDataPtr
  !    ELSE
  !      SurfaceWindow(SurfNum)%BlindNumber = Material(Construct(ConstrNumSh)%LayerPoint(5))%BlindDataPtr
  !    END IF
  !  ELSE IF(ShadingType == WSC_ST_ExteriorScreen) THEN
    IF(ShadingType == WSC_ST_ExteriorScreen) THEN
!     Count number of exterior window screens, initialize in InitGlassOpticalCalculations after returning
!     from this subroutine. The blind structure is initialized first and then the screen structure is initialized.
      NumSurfaceScreens = NumSurfaceScreens + 1
    END IF
  END IF
END DO

! Set some static exterior-window frame and divider SurfaceWindow values
! from values in FrameDivider derived type
DO SurfNum = 1,TotSurfaces
  FrDivNum = Surface(SurfNum)%FrameDivider
  IF(FrDivNum > 0) THEN  ! Surface is a window with a frame and/or divider
    FrWidth = FrameDivider(FrDivNum)%FrameWidth
    GlHeight = Surface(SurfNum)%Height
    GlWidth  = Surface(SurfNum)%Width
    NumVertDividers = FrameDivider(FrDivNum)%VertDividers
    NumHorDividers = FrameDivider(FrDivNum)%HorDividers
    BaseSurfNum = Surface(SurfNum)%BaseSurf
    SurfaceWindow(SurfNum)%FrameConductance = FrameDivider(FrDivNum)%FrameConductance
    SurfaceWindow(SurfNum)%FrameSolAbsorp = FrameDivider(FrDivNum)%FrameSolAbsorp
    SurfaceWindow(SurfNum)%FrameVisAbsorp = FrameDivider(FrDivNum)%FrameVisAbsorp
    SurfaceWindow(SurfNum)%FrameEmis = FrameDivider(FrDivNum)%FrameEmis
    SurfaceWindow(SurfNum)%FrEdgeToCenterGlCondRatio = FrameDivider(FrDivNum)%FrEdgeToCenterGlCondRatio
    SurfaceWindow(SurfNum)%DividerType = DividedLite
    IF(FrameDivider(FrDivNum)%DividerType == Suspended) SurfaceWindow(SurfNum)%DividerType = Suspended
    DivWidth = FrameDivider(FrDivNum)%DividerWidth
    SurfaceWindow(SurfNum)%DividerConductance = FrameDivider(FrDivNum)%DividerConductance
    SurfaceWindow(SurfNum)%DividerSolAbsorp = FrameDivider(FrDivNum)%DividerSolAbsorp
    SurfaceWindow(SurfNum)%DividerVisAbsorp = FrameDivider(FrDivNum)%DividerVisAbsorp
    SurfaceWindow(SurfNum)%DividerEmis = FrameDivider(FrDivNum)%DividerEmis
    SurfaceWindow(SurfNum)%DivEdgeToCenterGlCondRatio =  FrameDivider(FrDivNum)%DivEdgeToCenterGlCondRatio

    SurfaceWindow(SurfNum)%OutsideRevealSolAbs = FrameDivider(FrDivNum)%OutsideRevealSolAbs
    SurfaceWindow(SurfNum)%InsideSillDepth = FrameDivider(FrDivNum)%InsideSillDepth
    SurfaceWindow(SurfNum)%InsideReveal = FrameDivider(FrDivNum)%InsideReveal
    SurfaceWindow(SurfNum)%InsideSillSolAbs = FrameDivider(FrDivNum)%InsideSillSolAbs
    SurfaceWindow(SurfNum)%InsideRevealSolAbs = FrameDivider(FrDivNum)%InsideRevealSolAbs

    FrEdgeWidth = FrameDivider(FrDivNum)%FrameEdgeWidth
    DivEdgeWidth = FrameDivider(FrDivNum)%DividerEdgeWidth
    SurfaceWindow(SurfNum)%FrameEdgeArea = 2 * FrEdgeWidth * &
      ( GlHeight - FrEdgeWidth - NumHorDividers*DivWidth + &
        GlWidth  - FrEdgeWidth - NumVertDividers*DivWidth )
    SurfaceWindow(SurfNum)%DividerEdgeArea = 2*DivEdgeWidth* &
      ( NumHorDividers*(GlWidth-2*FrEdgeWidth) + NumVertDividers*(GlHeight-2*FrEdgeWidth)) &
      - NumHorDividers*NumVertDividers*(4*DivEdgeWidth**2 + 4*FrEdgeWidth*DivWidth)
    SurfaceWindow(SurfNum)%CenterGlArea = Surface(SurfNum)%Area - &
      SurfaceWindow(SurfNum)%FrameEdgeArea - SurfaceWindow(SurfNum)%DividerEdgeArea
    SurfaceWindow(SurfNum)%EdgeGlCorrFac = &
      (SurfaceWindow(SurfNum)%FrameEdgeArea * SurfaceWindow(SurfNum)%FrEdgeToCenterGlCondRatio + &
       SurfaceWindow(SurfNum)%DividerEdgeArea * SurfaceWindow(SurfNum)%DivEdgeToCenterGlCondRatio + &
       SurfaceWindow(SurfNum)%CenterGlArea) / &
       (SurfaceWindow(SurfNum)%FrameEdgeArea + SurfaceWindow(SurfNum)%DividerEdgeArea + &
            SurfaceWindow(SurfNum)%CenterGlArea)
  END IF
END DO

! Set SolarDiffusing to true for exterior windows that have a construction with an innermost diffusing glass layer
DifOverrideCount=0
DO SurfNum = 1,TotSurfaces
  SurfaceWindow(SurfNum)%SolarDiffusing = .false.
  IF(Surface(SurfNum)%Class == SurfaceClass_Window .AND. Surface(SurfNum)%ExtBoundCond == ExternalEnvironment .AND. &
     Surface(SurfNum)%StormWinConstruction == 0) THEN
    ConstrNum = Surface(SurfNum)%Construction
    MatNum = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
    IF(Material(MatNum)%SolarDiffusing) THEN
      IF (Surface(SurfNum)%WindowShadingControlPtr == 0) THEN
        SurfaceWindow(SurfNum)%SolarDiffusing = .true.
      ELSE  ! There is a shading control
        IF (WindowShadingControl(Surface(SurfNum)%WindowShadingControlPtr)%ShadingType == SwitchableGlazing) THEN
          SurfaceWindow(SurfNum)%SolarDiffusing = .true.
        ELSE
          SurfaceWindow(SurfNum)%SolarDiffusing = .false.
          DifOverrideCount=DifOverrideCount+1
          IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError('W5InitGlassParameters: Window="'//trim(Surface(SurfNum)%Name)//  &
               '" has interior material with Solar Diffusing=Yes, but existing Window Shading Device sets Diffusing=No.')
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  END IF
END DO

IF (DifOverrideCount > 0) THEN
  IF (.not. DisplayExtraWarnings) THEN
    CALL ShowWarningError('W5InitGlassParameters: '//trim(RoundSigDigits(DifOverrideCount))//  &
       ' Windows had Solar Diffusing=Yes overridden by presence of Window Shading Device.')
  ELSE
    CALL ShowMessage('W5InitGlassParameters: '//trim(RoundSigDigits(DifOverrideCount))//  &
       ' Windows had Solar Diffusing=Yes overridden by presence of Window Shading Device.')
  ENDIF
ENDIF

DO IPhi = 1, 10
  CosPhiIndepVar(IPhi) = COS((IPhi-1)*10.0d0*DegToRadians)
END DO

RETURN
END SUBROUTINE W5InitGlassParameters

!****************************************************************************
! WINDOW 5 Optical Calculation Subroutines
!****************************************************************************
SUBROUTINE SystemSpectralPropertiesAtPhi(iquasi,ngllayer,wlbot,wltop)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by F.Winkelmann from WINDOW 5
          !                      subroutine opcalc
          !       DATE WRITTEN   August 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For a particular angle of incidence, calculates system properties
          ! for a multi-layer glazing for each wavelength in the solar spectrum.
          ! Handles the special case of one or more layers that do not have spectral data.

          ! Returns, for a particular angle of incidence:
          !   stPhi     transmissivity of system at each wavelength in swl
          !   srfPhi    front reflectance of system at each wavelength in swl
          !   srbPhi    back reflectance of system at each wavelength in swl
          !   sabsPhi   absorptance by layer at each wavelength in swl

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL(r64)         :: sabsPhi(5)              ! System solar absorptance in each glass layer for
                                             !   particular angle of incidence
INTEGER           :: in,i                    ! Glass layer counter
INTEGER           :: iwl,j                   ! Wavelength counter
REAL(r64)         :: wl                      ! Wavelength
REAL(r64)         :: wlbot,wltop             ! Lowest and highest wavelength considered
INTEGER           :: ngllayer                  ! Number of glass layers in construction
INTEGER           :: iquasi                  ! When there is no spectral data, this is the wavelength
                                             !   index to use in tPhi, rfPhi and rbPhi

! For each glass layer find tPhi, rfPhi, and rbPhi at each wavelength

    do in=1,ngllayer
      do iwl=1,nume
        wl = wle(iwl)
        if (wl < wlbot .OR. wl > wltop) CYCLE
        ! In the following numpt is the number of spectral data points for each layer;
        ! numpt = 2 if there is no spectral data for a layer.
        if (numpt(in) <= 2) then
          tadjPhi(iwl,in)  = tPhi(iquasi,in)
          rfadjPhi(iwl,in) = rfPhi(iquasi,in)
          rbadjPhi(iwl,in) = rbPhi(iquasi,in)
        else
          ! Interpolate to get properties at the solar spectrum wavelengths
          CALL Interpolate(wlt(1,in), tPhi(1,in),  numpt(in), wl, tadjPhi(iwl,in))
          CALL Interpolate(wlt(1,in), rfPhi(1,in), numpt(in), wl, rfadjPhi(iwl,in))
          CALL Interpolate(wlt(1,in), rbPhi(1,in), numpt(in), wl, rbadjPhi(iwl,in))
        endif
      END DO
    END DO

! Calculate system properties at each wavelength
    do j=1,nume
        wl = wle(j)
        if (wl < wlbot .OR. wl > wltop) CYCLE

! Set diagonal of matrix for subroutine SystemPropertiesAtLambdaAndPhi
      do i=1,ngllayer
          top(i,i)  = tadjPhi(j,i)
          rfop(i,i) = rfadjPhi(j,i)
          rbop(i,i) = rbadjPhi(j,i)
      END DO

! Calculate glazing system properties
      if (ngllayer .eq. 1) then  ! Single-layer system
          stPhi(j) = top(1,1)
          srfPhi(j) = rfop(1,1)
          srbPhi(j) = rbop(1,1)
          sabsPhi(1) = 1.0d0 - stPhi(j) - srfPhi(j)
      else                     ! Multilayer system
          ! Get glazing system properties stPhi, etc., at this wavelength and incidence angle
          CALL SystemPropertiesAtLambdaAndPhi(ngllayer, stPhi(j), srfPhi(j), srbPhi(j), sabsPhi)
      endif

      do i=1,ngllayer
        saPhi(j,i)=sabsPhi(i)
      END DO

    END DO  ! End of wavelength loop

    return
END SUBROUTINE SystemSpectralPropertiesAtPhi


!************************************************************************
SUBROUTINE SystemPropertiesAtLambdaAndPhi (n, tt, rft, rbt, aft)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by F. Winkelmann from WINDOW 5
          !                      subroutine op
          !       DATE WRITTEN   August 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For a given angle of incidence, finds the overall properties of
          ! of a series of layers at a particular wavelength

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER, INTENT(IN) :: n                       ! Number of glass layers
INTEGER             :: i,j                     ! Glass layer counters
REAL(r64)           :: denom,denom1,denom2     ! Intermediate variables
REAL(r64), INTENT(OUT)   :: tt                      ! System transmittance
REAL(r64), INTENT(OUT)   :: rft,rbt                 ! System front and back reflectance
REAL(r64)           :: t0,rb0,rf0              ! Transmittance, back reflectance and front
                                               !   reflectance variables
REAL(r64)           :: af,ab                   ! Front and back absorptance variables
REAL(r64), INTENT(OUT)   :: aft(5)                  ! System absorptance of each glass layer

          ! FLOW

  ! Calculate perimeter elements of rt matrix
  do i=1,n-1
    do J=i+1,n
      denom = 1.0d0 - rfop(j,j) * rbop(j-1,i)
      if (denom .EQ. 0.0) then
        top(i,j) = 0.0d0
        rfop(i,j) = 1.0d0
        rbop(j,i) = 1.0d0
      else
        top(i,j) = top(i,j-1) * top(j,j) / denom
        rfop(i,j) = rfop(i,j-1) + top(i,j-1)**2 * rfop(j,j) / denom
        rbop(j,i) = rbop(j,j) + top(j,j)**2 * rbop(j-1,i) / denom
      endif
    END DO
  END DO
    ! System properties: transmittance, front and back reflectance
    tt = top(1,n)
    rft = rfop(1,n)
    rbt = rbop(n,1)

    ! Absorptance in each layer
    do j=1,n
      if (j.eq.1) then
        t0 = 1.0d0
        rb0 = 0.0d0
      else
        t0 = top(1,j-1)
        rb0 = rbop(j-1,1)
      endif

      if (j.eq.n) then
        rf0 = 0.0d0
      else
        rf0 = rfop(j+1,n)
      endif

      af = 1.0d0 - top(j,j) - rfop(j,j)
      ab = 1.0d0 - top(j,j) - rbop(j,j)
      denom1 = 1.0d0 - rfop(j,n)*rb0
      denom2 = 1.0d0 - rbop(j,1)*rf0

      if (denom1 .EQ. 0.0 .OR. denom2 .EQ. 0.0) then
        aft(j) = 0.0d0
      else
        aft(j) = (t0 * af) / denom1 + (top(1,j) * rf0 * ab) / denom2
      endif
    END DO
    return
END SUBROUTINE SystemPropertiesAtLambdaAndPhi

!*************************************************************************
SUBROUTINE SolarSprectrumAverage(p, psol)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by F.Winkelmann from WINDOW 5 subroutine solar
          !       DATE WRITTEN   August 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates average of property p weighted by solar spectral irradiance, e

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL(r64)         :: up,down                 ! Intermediate variables
INTEGER           :: i                       ! Wavelength counter
REAL(r64)         :: esol                    ! Solar spectrum value times delta wavelength
REAL(r64)         :: p(nume)                 ! Quantity to be weighted by solar spectrum
REAL(r64)         :: psol                    ! Quantity p weighted by solar spectrum

          ! FLOW

      up = 0.0d0
      down = 0.0d0

      do i=1,nume-1
        esol = (wle(i+1) - wle(i)) * 0.5d0 * (e(i) + e(i+1))
        up = up + 0.5d0 * (p(i) + p(i+1)) * esol
        down = down + esol
      END DO

      psol = up / down

      return
END SUBROUTINE SolarSprectrumAverage

!********************************************************************
SUBROUTINE VisibleSprectrumAverage(p, pvis)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by F.Winkelmann from WINDOW 5
          !                      subroutine w4vis
          !       DATE WRITTEN   August 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates visible average of property p by weighting with solar
          ! spectral irradiance, e, and photopic response, y30

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL(r64)         :: up,down                 ! Intermediate variables
INTEGER           :: i                       ! Wavelength counter
REAL(r64)         :: p(nume)                 ! Quantity to be weighted by solar spectrum
REAL(r64)         :: pvis                    ! Quantity p weighted by solar spectrum and photopic
                                             !   response curve
REAL(r64)         :: y30ils1,y30new          ! Photopic response variables
REAL(r64)         :: evis                    ! Solar spectrum value times photopic response
                                             !   times delta wavelength
          ! FLOW

      down = 0.0d0
      up = 0.0d0
      y30ils1 = 0.0d0
      y30new = 0.0d0

      do i=1,nume
!EPTeam - Objexx suggested change
!do i=2,nume !Objexx:BoundsViolation e|wle|p(i-1) @ i=1: Changed start index from 1 to 2: wle values prevented this violation from occurring in practice
        ! Restrict to visible range
        if (wle(i) >= 0.37d0 .and. wle(i) <= 0.78d0) then
          call Interpolate(wlt3, y30, numt3, wle(i), y30new)
          evis = e(i-1) * 0.5d0 * (y30new+y30ils1) * (wle(i)-wle(i-1))
          up = up + 0.5d0*(p(i) + p(i-1)) * evis
          down = down + evis
          y30ils1 = y30new
        endif
      end do

      pvis = up / down
      return
END SUBROUTINE VisibleSprectrumAverage

!**********************************************************************
SUBROUTINE Interpolate (x, y, npts, xin, yout)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by F.Winkelmann from WINDOW 5 subroutine interp
          !       DATE WRITTEN   August 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Linearly interpolates between data points. Outputs yout, interpolated
          ! value of y corresponding to xin

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER           :: npts                    ! Number of data pairs
REAL(r64)         :: x(npts)                 ! Array of data points for independent variable
REAL(r64)         :: y(npts)                 ! Array of data points for dependent variable
INTEGER           :: i                       ! Counter
REAL(r64)         :: xin                     ! Given value of x
REAL(r64)         :: yout                    ! Interpolated value of y at xin

          ! FLOW

do i = 1, npts
  if (xin .LE. x(i)) THEN
    if (i .EQ. 1) then
      yout = y(1)
    else
      yout = y(i-1) + (y(i) - y(i-1))*(xin - x(i-1))/(x(i) - x(i-1))
    endif
    RETURN
  END IF
END DO

! Past the end of the array, so return endpoint
yout = y(npts)
RETURN
END SUBROUTINE Interpolate

!***********************************************************************************
! Window Thermal Calculation Subroutines
!***********************************************************************************
SUBROUTINE CalcWindowHeatBalance (SurfNum,HextConvCoeff,SurfInsideTemp,SurfOutsideTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   November 1999
          !       MODIFIED       FW, July 2000 (call better solution method)
          !                      FW, June 2001 (handle window blinds)
          !                      FW, Dec  2002 (add between-glass shades and blinds)
          !                      FW, Mar  2003 (extend condensation flag to airflow windows)
          !                      CC, Jul  2003 (set the reference temperatures for inside surface heat balance
          !                                    depending on convection algorithms and/or air models used)
          !                      FW, Sep  2003 (increment ZoneWinHeatGain only for exterior windows)
          !                      RR, May  2006 (add exterior window screen)
          !                      TH, Dec  2008 (add thermochromic windows)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Sets up information needed to calculate the window thermal behavior.
          ! Calls SolveForWindowTemperatures, which calculates the inside and outside
          ! face temperature of each glass layer by solving the heat balance
          ! equations on each face. Also calls CalcWinFrameAndDividerTemps,
          ! which calculates the outside and inside face temperatures of the
          ! window frame and divider if either of these are present.
          ! The resulting inside face temperature of the inner glass pane and the
          ! inside surface temperatures of frame and divider are used in the zone
          ! heat balance calculation. The inside face temperature of an interior shade
          ! or blind, if present, and the natural convection air flow between the
          ! shade/blind and inside glass face also appear in the zone heat balance calculation.
          !
          ! The logical variable NRSolution is currently set to false, which means
          ! that the Newton-Raphson solution method for the glass layer heat balance
          ! is not used (because it sometimes didn't converge for 3- and 4-pane
          ! constructions with one or more low-emissivity layers). Instead, a more
          ! robust solution method is used that successively solves linearized heat
          ! balance equations until convergence is reached (see SolveForWindowTemperatures).
          !
          ! CalcWindowHeatBalance is called by CalcHeatBalanceInsideSurface once each
          ! time step for each window.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  USE DataBSDFWindow
  USE General,                      ONLY: InterpSlatAng  ! Function for slat angle interpolation
  USE DataZoneEquipment,            ONLY : ZoneEquipConfig
  USE DataLoopNode,                 ONLY : Node
  USE Psychrometrics,               ONLY:PsyCpAirFnWTdb,PsyTdpFnWPb
  USE DataHeatBalSurface  ,         ONLY : QConvOutReport,QdotConvOutRep,QdotConvOutRepPerArea,&
                                           QRadOutReport, QdotRadOutRep, QdotRadOutRepPerArea
!unused0909  USE DataEnvironment, ONLY: CurMnDyHr
  USE InputProcessor, ONLY: SameString
  USE WindowComplexManager, ONLY: CalcComplexWindowThermal
  USE WindowEquivalentLayer, ONLY: EQLWindowSurfaceHeatBalance


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)              :: SurfNum         ! Surface number
REAL(r64), INTENT(IN)                 :: HextConvCoeff   ! Outside air film conductance coefficient
REAL(r64), INTENT(INOUT)  :: SurfInsideTemp  ! Inside window surface temperature
REAL(r64), INTENT(INOUT)  :: SurfOutsideTemp ! Outside surface temperature (C)
                                                        ! (temperature of innermost face) [C]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER   :: ZoneNum                      ! Zone number corresponding to SurfNum
INTEGER   :: BlNum                        ! Window blind number
INTEGER   :: SurfNumAdj                   ! An interzone surface's number in the adjacent zone
INTEGER   :: ZoneNumAdj                   ! An interzone surface's adjacent zone number
INTEGER   :: ConstrNum                    ! Construction number
!!unused INTEGER           :: ConstrNumSh                  ! Shaded construction number
INTEGER   :: IConst                       ! Construction number
INTEGER   :: TotLay                       ! Total number of layers in a construction
                                          !   (sum of solid layers and gap layers)
INTEGER   :: TotGlassLay                  ! Total number of glass layers in a construction
INTEGER   :: Lay                          ! Layer number
INTEGER   :: LayPtr                       ! Material number for a layer
INTEGER   :: IGlass                       ! glass layer number (1,2,3,...)
INTEGER   :: IGap                         ! Gap layer number (1,2,...)
INTEGER   :: IMix                         ! Gas number in a mixture of gases
INTEGER   :: ICoeff                       ! Gas property index (1,2,3)
INTEGER   :: ShadeFlag                    ! Flag indicating whether shade or blind is on, and shade/blind position
INTEGER   :: k                            ! Layer counter
!REAL(r64) :: tsky                         ! Sky temperature [K]
INTEGER   :: ShadeLayPtr                  ! Material number corresponding to a shade layer
REAL(r64) :: dth1,dth2,dth3,dth4          ! Temperature difference across glass layers [K]
REAL(r64) :: EffShBlEmiss                 ! Effective interior shade or blind emissivity
REAL(r64) :: EffGlEmiss                   ! Effective inside glass emissivity when interior shade or blind
REAL(r64) :: RoomHumRat                   ! Room air humidity ratio
REAL(r64) :: RoomDewPoint                 ! Room air dewpoint temperature (C)
REAL(r64) :: InsideGlassTemp              ! Temperature of room side of innermost glass layer (C)
REAL(r64) :: Tleft, Tright                ! For airflow windows, temperature of the glass faces adjacent
                                          !  to the airflow gap (C)
INTEGER   :: ZoneEquipConfigNum
INTEGER   :: NodeNum
REAL(r64) :: SumSysMCp                    ! Zone sum of air system MassFlowRate*Cp
REAL(r64) :: SumSysMCpT                   ! Zone sum of air system MassFlowRate*Cp*T
REAL(r64) :: MassFlowRate
REAL(r64) :: NodeTemp
REAL(r64) :: CpAir
REAL(r64) :: RefAirTemp ! reference air temperatures

! New variables for thermochromic windows calc
REAL(r64) :: locTCSpecTemp                ! The temperature corresponding to the specified optical properties of the TC layer
REAL(r64) :: locTCLayerTemp               ! TC layer temperature at each time step. C
LOGICAL   :: locTCFlag =.False.           ! True if this surface is a TC window
REAL(r64) :: deltaTemp(100) = 0.0d0
INTEGER   :: i
INTEGER   :: iMinDT(1) = 0
INTEGER   :: IDConst(100) = 0
REAL(r64) :: dT0 = 0.0d0
REAL(r64) :: dT1 = 0.0d0
REAL(r64) :: SurfOutsideEmiss ! temporary for result of outside surface emissivity
REAL(r64) :: Tsout ! temporary for result of outside surface temp in Kelvin
!integer :: CurrentThermalAlgorithm
integer :: CurrentThermalModelNumber
integer :: temp

!CurrentThermalAlgorithm = -1

IF (KickoffSizing .or. KickOffSimulation) RETURN

IF (SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN

  temp = 0

  !Simon: Complex fenestration state works only with tarcog
  CALL CalcComplexWindowThermal(SurfNum,temp,HextConvCoeff,SurfInsideTemp,SurfOutsideTemp,SurfOutsideEmiss,noCondition)

  ConstrNum = Surface(SurfNum)%Construction
  TotGlassLay = Construct(ConstrNum)%TotGlassLayers
  ngllayer = Construct(ConstrNum)%TotSolidLayers ! Simon: This is necessary to keep for frame calculations
  ! Simon: need to transfer surface temperatures because of frames calculation
  DO i = 1, 2*Construct(ConstrNum)%TotSolidLayers
    thetas(i) = SurfaceWindow(SurfNum)%ThetaFace(i)
  END DO
  hcout = HextConvCoeff

  ! This is code repeating and it is necessary to calculate report variables.  Do not know
  ! how to solve this in more elegant way :(
  IF(Surface(SurfNum)%ExtWind) THEN  ! Window is exposed to wind (and possibly rain)
    IF(IsRain) THEN  ! Raining: since wind exposed, outside window surface gets wet
      tout = Surface(SurfNum)%OutWetBulbTemp + TKelvin
    ELSE             ! Dry
      tout = Surface(SurfNum)%OutDryBulbTemp + TKelvin
    END IF
  ELSE                               ! Window not exposed to wind
    tout = Surface(SurfNum)%OutDryBulbTemp + TKelvin
  END IF

ELSEIF (SurfaceWindow(SurfNum)%WindowModelType  == WindowEQLModel) THEN

   CALL EQLWindowSurfaceHeatBalance( SurfNum, HextConvCoeff,SurfInsideTemp,SurfOutsideTemp,SurfOutsideEmiss,noCondition)
    hcout = HextConvCoeff
  ! Required for report variables calculations.
  IF(Surface(SurfNum)%ExtWind) THEN  ! Window is exposed to wind (and possibly rain)
    IF(IsRain) THEN  ! Raining: since wind exposed, outside window surface gets wet
      tout = Surface(SurfNum)%OutWetBulbTemp + TKelvin
    ELSE             ! Dry
      tout = Surface(SurfNum)%OutDryBulbTemp + TKelvin
    END IF
  ELSE                               ! Window not exposed to wind
    tout = Surface(SurfNum)%OutDryBulbTemp + TKelvin
  END IF

ELSE ! regular window, not BSDF, not EQL Window

  ConstrNum = Surface(SurfNum)%Construction
  IF(SurfaceWindow(SurfNum)%StormWinFlag > 0) ConstrNum = Surface(SurfNum)%StormWinConstruction


  ! Added for thermochromic windows
  locTCFlag = (Construct(ConstrNum)%TCFlag == 1)

  IF (locTCFlag) THEN
    locTCSpecTemp = Material(Construct(ConstrNum)%TCLayer)%SpecTemp
    SurfaceWindow(SurfNum)%SpecTemp = locTCSpecTemp
    ! Check to see whether needs to switch to a new TC window construction
    locTCLayerTemp = SurfaceWindow(SurfNum)%TCLayerTemp
    dT0 = ABS(locTCLayerTemp-locTCSpecTemp)
    IF (dT0 >= 1) THEN
      ! Find the TC construction that is closed to the TCLayerTemp
      i = 0
      deltaTemp = 0.0d0
      IDConst = 0
      DO k=1, TotConstructs
        IF (Construct(k)%TCMasterConst == Construct(ConstrNum)%TCMasterConst) THEN
          dT1 = ABS(locTCLayerTemp - Material(Construct(k)%TCLayer)%SpecTemp)
          IF (dT1 < dT0) THEN
            i = i + 1
            deltaTemp(i) = dT1
            IDConst(i) = k
          ENDIF
        ENDIF
      ENDDO
      IF (i >= 1) THEN
        ! Find the closest item
        iMinDT = MINLOC(deltaTemp, MASK = deltaTemp.GT.0.0d0)
        ! Use the new TC window construction
        ConstrNum = IDConst(iMinDT(1))
        Surface(SurfNum)%Construction = ConstrNum
        SurfaceWindow(SurfNum)%SpecTemp = Material(Construct(ConstrNum)%TCLayer)%SpecTemp
      ENDIF
    ENDIF
  ENDIF
  ! end new TC code


  ZoneNum = Surface(SurfNum)%Zone
  TotLay = Construct(ConstrNum)%TotLayers
  TotGlassLay = Construct(ConstrNum)%TotGlassLayers
  ngllayer = TotGlassLay
  nglface  = 2*ngllayer
  ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
  tilt = Surface(SurfNum)%Tilt
  tiltr = tilt * DegToRadians
  SurfNumAdj = Surface(SurfNum)%ExtBoundCond
  hcin = HConvIn(SurfNum)  ! Room-side surface convective film conductance

  ! determine reference air temperature for this surface
  SELECT CASE (Surface(SurfNum)%TAirRef)
      CASE (ZoneMeanAirTemp)
          RefAirTemp = MAT(ZoneNum)
          TempEffBulkAir(SurfNum) = RefAirTemp
      CASE (AdjacentAirTemp)
          RefAirTemp = TempEffBulkAir(SurfNum)
      CASE (ZoneSupplyAirTemp)
              ! determine ZoneEquipConfigNum for this zone
  !            ControlledZoneAirFlag = .FALSE.
          ZoneEquipConfigNum = ZoneNum
  !            DO ZoneEquipConfigNum = 1, NumOfControlledZones
  !                IF (ZoneEquipConfig(ZoneEquipConfigNum)%ActualZoneNum /= ZoneNum) CYCLE
  !                ControlledZoneAirFlag = .TRUE.
  !                EXIT
  !            END DO ! ZoneEquipConfigNum
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
              CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp,'CalcWindowHeatBalance')
              SumSysMCp = SumSysMCp + MassFlowRate * CpAir
              SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
          END DO
          ! a weighted average of the inlet temperatures.
          IF (SumSysMCp > 0.0d0) THEN
            RefAirTemp = SumSysMCpT/SumSysMCp
          ELSE
            RefAirTemp = NodeTemp
          ENDIF
          TempEffBulkAir(SurfNum) = RefAirTemp

      CASE DEFAULT
          ! currently set to mean air temp but should add error warning here
          RefAirTemp = MAT(ZoneNum)
          TempEffBulkAir(SurfNum) = RefAirTemp
  END SELECT

  Tin = RefAirTemp + TKelvin  ! Inside air temperature

  ! Reset hcin if necessary since too small a value sometimes causes non-convergence
  ! of window layer heat balance solution.
  IF (Surface(SurfNum)%IntConvCoeff == 0) THEN  !
    IF(hcin <= LowHConvLimit) then  ! may be redundent now, check is also in HeatBalanceConvectionCoeffs.f90
      !  hcin = 3.076d0  !BG this is rather high value and abrupt change. changed to set to lower limit
      hcin = LowHConvLimit
      HConvIn(SurfNum) = hcin ! store for accurate reporting.
    ENDIF
  ENDIF

  ! IR incident on window from zone surfaces and high-temp radiant sources
  rmir = SurfaceWindow(SurfNum)%IRfromParentZone + QHTRadSysSurf(SurfNum) + QHWBaseboardSurf(SurfNum) + &
          QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum)

  ! Short-wave radiation (from interior and exterior solar and zone lights)
  ! absorbed at each face. Assumes equal split between faces of short-wave absorbed in glass layer.

  DO IGlass = 1,TotGlassLay
    AbsRadGlassFace(2*IGlass-1) = QRadSWwinAbs(SurfNum,IGlass)/2.d0
    AbsRadGlassFace(2*IGlass)   = QRadSWwinAbs(SurfNum,IGlass)/2.d0
  END DO

  ! IR from zone internal gains (lights, equipment and people) absorbed on zone-side face
  ! (assumes inside glass layer is opaque to IR, so no contribution to other layers)

  AbsRadGlassFace(2*TotGlassLay) = AbsRadGlassFace(2*TotGlassLay) + QRadThermInAbs(SurfNum)

  ! Fill the layer properties needed for the thermal calculation.
  ! For switchable glazing it is assumed that thermal properties, such
  ! as surface emissivity, are the same for the unswitched and switched state,
  ! so the thermal properties of the unswitched state are used.
  ! For windows with a blind or shade it is assumed
  ! that the blind or shade does not affect the thermal properties of the glazing,
  ! so the thermal properties of the construction without the blind or shade are used.

  ! The layer and face numbering are as follows (for the triple glazing case):
  ! Glass layers are 1,2 and 3, where 1 is the outside (outside environment facing)
  !   layer and 3 is the inside (room-facing) layer;
  ! Faces (also called surfaces) are 1,2,3,4,5 and 6, where face 1 is the
  !   outside (front) face of glass layer 1, face 2 is the inside (back)
  !   face of glass layer 1, face 3 is the outer face of glass layer 2, face 4 is the
  !   inner face of glass layer 2, etc.
  ! Gap layers are 1 and 2, where gap layer 1 is between glass layers 1 and 2
  !   and gap layer 2 is between glass layers 2 and 3.
  ! If an exterior, interior or between-glass blind or shade is in place, 7 and 8
  !   are the blind/shade faces, from outside to inside. If an exterior or interior
  !   blind/shade is in place, gap layer 3 is between the blind/shade and adjacent
  !   glass layer and is assumed to be air.
  ! Between-glass blind/shade is modeled only for double and triple glazing.
  !   For double glazing, gap 1 is between glass 1 and blind/shade and gap 2 is between
  !   blind/shade and glass 2.
  !   For triple glazing, the blind/shade is assumed to be between the inner two glass
  !   layers, i.e., between glass layers 2 and 3. In this case gap 1 is between glass 1
  !   and glass 2, gap 2 is between glass 2 and blind/shade, and gap 3 is between
  !   blind/shade and glass 3.

  IConst = ConstrNum
  IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn &
      .OR.ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
    IConst = Surface(SurfNum)%ShadedConstruction
    IF(Surfacewindow(SurfNum)%StormWinFlag > 0) IConst = Surface(SurfNum)%StormWinShadedConstruction
  END IF
  TotLay = Construct(IConst)%TotLayers
  IGlass = 0
  IGap = 0

  ! Fill window layer properties needed for window layer heat balance calculation

  DO Lay = 1,TotLay
    LayPtr = Construct(IConst)%LayerPoint(Lay)

    IF(( Material(LayPtr)%Group == WindowGlass) .OR. (Material(LayPtr)%Group == WindowSimpleGlazing) ) THEN
      IGlass = IGlass + 1
      thick(IGlass) =    Material(LayPtr)%Thickness
      scon(IGlass) =     Material(LayPtr)%Conductivity/Material(LayPtr)%Thickness
      emis(2*IGlass-1) = Material(LayPtr)%AbsorpThermalFront
      emis(2*IGlass) =   Material(LayPtr)%AbsorpThermalBack
      tir(2*IGlass-1) =  Material(LayPtr)%TransThermal
      tir(2*IGlass) =    Material(LayPtr)%TransThermal
    END IF

    IF(Material(LayPtr)%Group == Shade .OR. Material(LayPtr)%Group == WindowBlind .OR. Material(LayPtr)%Group == Screen) THEN
      IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) &
        ShadeLayPtr = Construct(IConst)%LayerPoint(Construct(IConst)%TotLayers)
      IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) &
        ShadeLayPtr = Construct(IConst)%LayerPoint(1)
      IF(ShadeFlag == BGShadeOn .OR. ShadeFlag == BGBlindOn) THEN
        ShadeLayPtr = Construct(IConst)%LayerPoint(3)
        IF(TotGlassLay == 3) ShadeLayPtr = Construct(IConst)%LayerPoint(5)
      END IF
      IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == ExtShadeOn .OR. ShadeFlag == BGShadeOn .OR. ShadeFlag == ExtScreenOn) THEN
            ! Shade or screen on
        IF (AnyEnergyManagementSystemInModel) THEN  ! check to make sure the user hasn't messed up the shade control values
          IF (Material(ShadeLayPtr)%Group == WindowBlind) THEN
            CALL ShowSevereError('CalcWindowHeatBalance: ShadeFlag indicates Shade but Blind="'//  &
              trim(Material(ShadeLayPtr)%Name)//'" is being used.')
            CALL ShowContinueError('This is most likely a fault of the EMS values for shading control.')
            CALL ShowFatalError('Preceding condition terminates program.')
          ENDIF
        ENDIF
        thick(TotGlassLay+1) = Material(ShadeLayPtr)%Thickness
        scon(TotGlassLay+1) = Material(ShadeLayPtr)%Conductivity/Material(ShadeLayPtr)%Thickness
        IF(ShadeFlag == ExtScreenOn) THEN
          emis(nglface+1) = Material(ShadeLayPtr)%AbsorpThermalFront
          tir(nglface+1)  = SurfaceScreens(Material(ShadeLayPtr)%ScreenDataPtr)%DifDifTrans
          tir(nglface+2)  = SurfaceScreens(Material(ShadeLayPtr)%ScreenDataPtr)%DifDifTrans
        ELSE
          emis(nglface+1) = Material(ShadeLayPtr)%AbsorpThermal
          tir(nglface+1)  = Material(ShadeLayPtr)%TransThermal
          tir(nglface+2)  = Material(ShadeLayPtr)%TransThermal
        END IF
        emis(nglface+2) = Material(ShadeLayPtr)%AbsorpThermal

      ELSE
        IF (AnyEnergyManagementSystemInModel) THEN  ! check to make sure the user hasn't messed up the shade control values
          IF (Material(ShadeLayPtr)%Group == Shade .or. Material(ShadeLayPtr)%Group == Screen) THEN
            CALL ShowSevereError('CalcWindowHeatBalance: ShadeFlag indicates Blind but Shade/Screen="'//  &
              trim(Material(ShadeLayPtr)%Name)//'" is being used.')
            CALL ShowContinueError('This is most likely a fault of the EMS values for shading control.')
            CALL ShowFatalError('Preceding condition terminates program.')
          ENDIF
        ENDIF
            ! Blind on
        BlNum = SurfaceWindow(SurfNum)%BlindNumber
        thick(TotGlassLay+1) = Blind(BlNum)%SlatThickness
        scon(TotGlassLay+1)  = Blind(BlNum)%SlatConductivity/Blind(BlNum)%SlatThickness
        emis(nglface+1) = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                          SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%IRFrontEmiss)
        emis(nglface+2) = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                          SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%IRBackEmiss)
        tir(nglface+1)  = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                          SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%IRFrontTrans)
        tir(nglface+2)  = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS, &
                          SurfaceWindow(SurfNum)%MovableSlats,Blind(BlNum)%IRBackTrans)
      END IF
    END IF

    IF(Material(LayPtr)%Group == WindowGas .or. Material(LayPtr)%Group == WindowGasMixture) THEN
      IGap = IGap + 1
      gap(IGap)   = Material(LayPtr)%Thickness
      gnmix(IGap) = Material(LayPtr)%NumberOfGasesInMixture
      DO IMix = 1,gnmix(IGap)
        gwght(IGap,IMix)  = Material(LayPtr)%GasWght(IMix)
        gfract(IGap,IMix) = Material(LayPtr)%GasFract(IMix)
        DO ICoeff = 1,3
          gcon(IGap,IMix,ICoeff) = Material(LayPtr)%GasCon(IMix,ICoeff)
          gvis(IGap,IMix,ICoeff) = Material(LayPtr)%GasVis(IMix,ICoeff)
          gcp(IGap,IMix,ICoeff)  = Material(LayPtr)%GasCp(IMix,ICoeff)
        END DO
      END DO
    END IF

  END DO  ! End of loop over glass, gap and blind/shade layers in a window construction

  IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn &
          .OR.ShadeFlag==ExtBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
    ! Interior or exterior blind, shade or screen is on.
    ! Fill gap between blind/shade and adjacent glass with air properties.
    IGap = IGap + 1
    IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtScreenOn) THEN  ! Interior or exterior shade
      gap(IGap) = Material(ShadeLayPtr)%WinShadeToGlassDist
    ELSE                                                           ! Interior or exterior blind
      gap(IGap) = Blind(SurfaceWindow(SurfNum)%BlindNumber)%BlindToGlassDist
    END IF
    gnmix(IGap) = 1
    gwght(IGap,1) = GasWght(1)
    DO ICoeff = 1,3
      gcon(IGap,1,ICoeff) = GasCoeffsCon(1,ICoeff)
      gvis(IGap,1,ICoeff) = GasCoeffsVis(1,ICoeff)
      gcp(IGap,1,ICoeff)  = GasCoeffsCp (1,ICoeff)
    END DO
  END IF

  ! Exterior convection coefficient, exterior air temperature and IR radiance
  ! of exterior surround. Depend on whether window is interzone (in an interzone
  ! wall or exterior (in an exterior wall).

  hcout=HExtConvCoeff  ! Exterior convection coefficient is passed in from outer routine
  !tsky = SkyTemp + TKelvin

  IF(SurfNumAdj > 0) THEN  ! Interzone window

    ZoneNumAdj = Surface(SurfNumAdj)%Zone

     ! determine reference air temperature for this surface
    SELECT CASE (Surface(SurfNumAdj)%TAirRef)
      CASE (ZoneMeanAirTemp)
          RefAirTemp = MAT(ZoneNumAdj)
          TempEffBulkAir(SurfNumAdj) = RefAirTemp
      CASE (AdjacentAirTemp)
          RefAirTemp = TempEffBulkAir(SurfNumAdj)
      CASE (ZoneSupplyAirTemp)
          ! determine ZoneEquipConfigNum for this zone
          ZoneEquipConfigNum = ZoneNumAdj
          ! check whether this zone is a controlled zone or not
          IF (.NOT. Zone(ZoneNumAdj)%IsControlled) THEN
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
              CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNumAdj), NodeTemp, 'CalcWindowHeatBalance')
              SumSysMCp = SumSysMCp + MassFlowRate * CpAir
              SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
          END DO
          IF (SumSysMCp > 0.0d0) THEN
            ! a weighted average of the inlet temperatures.
            RefAirTemp = SumSysMCpT/SumSysMCp
          ELSE
            RefAirTemp = NodeTemp
          ENDIF
          TempEffBulkAir(SurfNumAdj) = RefAirTemp
      CASE DEFAULT
          ! currently set to mean air temp but should add error warning here
          RefAirTemp = MAT(ZoneNumAdj)
          TempEffBulkAir(SurfNumAdj) = RefAirTemp
    END SELECT

    Tout = RefAirTemp + TKelvin  ! outside air temperature

    ! Add long-wave radiation from adjacent zone absorbed by glass layer closest to the adjacent zone.

    AbsRadGlassFace(1) = AbsRadGlassFace(1) + QRadThermInAbs(SurfNumAdj)

    ! The IR radiance of this window's "exterior" surround is the IR radiance
    ! from surfaces and high-temp radiant sources in the adjacent zone

    outir = SurfaceWindow(SurfNumAdj)%IRfromParentZone + QHTRadSysSurf(SurfNumAdj) + QHWBaseboardSurf(SurfNumAdj) + &
              QSteamBaseboardSurf(SurfNumAdj) + QElecBaseboardSurf(SurfNumAdj)

  ELSE  ! Exterior window (ExtBoundCond = 0)

    IF(Surface(SurfNum)%ExtWind) THEN  ! Window is exposed to wind (and possibly rain)
      IF(IsRain) THEN  ! Raining: since wind exposed, outside window surface gets wet
        tout = Surface(SurfNum)%OutWetBulbTemp + TKelvin
      ELSE             ! Dry
        tout = Surface(SurfNum)%OutDryBulbTemp + TKelvin
      END IF
    ELSE                               ! Window not exposed to wind
      tout = Surface(SurfNum)%OutDryBulbTemp + TKelvin
    END IF
    Ebout = sigma * tout**4
    outir = Surface(SurfNum)%ViewFactorSkyIR *   &
       (AirSkyRadSplit(SurfNum)*sigma*SkyTempKelvin**4 + (1.d0-AirSkyRadSplit(SurfNum))*Ebout) +   &
        Surface(SurfNum)%ViewFactorGroundIR * Ebout

  END IF

  ! Factors used in window layer temperature solution
  IF(ngllayer >= 2) THEN
    A23P = -emis(3)/(1.0d0-(1.0d0-emis(2))*(1.0d0-emis(3)))
    A32P = emis(2)/(1.0d0-(1.0d0-emis(2))*(1.0d0-emis(3)))
    A23 = emis(2)*sigma*A23P
  END IF

  IF(ngllayer >= 3) THEN
    A45P = -emis(5)/(1.0d0-(1.0d0-emis(4))*(1.0d0-emis(5)))
    A54P = emis(4)/(1.0d0-(1.0d0-emis(4))*(1.0d0-emis(5)))
    A45 = emis(4)*sigma*A45P
  END IF

  IF(ngllayer == 4) THEN
    A67P = -emis(7)/(1.0d0-(1.0d0-emis(6))*(1.0d0-emis(7)))
    A76P = emis(6)/(1.0d0-(1.0d0-emis(6))*(1.0d0-emis(7)))
    A67 = emis(6)*sigma*A67P

  END IF

  thetas = 0.0d0
  thetasPrev = 0.0d0
  fvec = 0.0d0
  fjac = 0.0d0

  ! Calculate window face temperatures

  CALL SolveForWindowTemperatures(SurfNum)

  ! Temperature difference across glass layers (for debugging)

  dth1 = thetas(2)-thetas(1)
  dth2 = thetas(4)-thetas(3)
  dth3 = thetas(6)-thetas(5)
  dth4 = thetas(8)-thetas(7)

  IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
    SurfInsideTemp  = thetas(2*ngllayer+2) - TKelvin
    EffShBlEmiss = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                      SurfaceWindow(SurfNum)%EffShBlindEmiss)
    EffGlEmiss   = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                      SurfaceWindow(SurfNum)%EffGlassEmiss)
    SurfaceWindow(SurfNum)%EffInsSurfTemp = (EffShBlEmiss * SurfInsideTemp + EffGlEmiss * (thetas(2*ngllayer)-TKelvin)) / &
                                               (EffShBlEmiss + EffGlEmiss)
  ELSE
    SurfInsideTemp = thetas(2*ngllayer) - TKelvin
  END IF
  IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
    SurfOutsideTemp = thetas(2*ngllayer+1) - TKelvin  ! this index looks suspicious (CR 8202)
    !SurfOutsideEmiss = emis(1)  ! this index should be coordinated with previous line
    SurfOutsideEmiss = emis(2*ngllayer+1) ! fix for CR 8202
  ELSE
    SurfOutsideEmiss = emis(1)
    SurfOutsideTemp = thetas(1) - TKelvin
  END IF

  ! Save temperatures for use next time step

  DO k = 1,nglfacep
    SurfaceWindow(SurfNum)%ThetaFace(k) = thetas(k)
  END DO


  ! Added TH 12/23/2008 for thermochromic windows to save the current TC layer temperature
  IF (locTCFlag) THEN
    SurfaceWindow(SurfNum)%TCLayerTemp = (thetas(2*Construct(ConstrNum)%TCGlassID-1)+ &
    thetas(2*Construct(ConstrNum)%TCGlassID))/2 - TKelvin   ! degree C
  ENDIF
END IF !regular window, not BSDF, not EQL

  ! Set condensation flag to 1 if condensation expected to occur on the innermost glass face,
  ! or, for airflow windows, on either or the two glass faces in the airflow gap
IF (.NOT. Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
  InsideGlassTemp = thetas(2*ngllayer)-TKelvin
  RoomHumRat = ZoneAirHumRat(Surface(SurfNum)%Zone)
  RoomDewpoint = PsyTdpFnWPb(RoomHumRat,OutBaroPress)
  InsideGlassCondensationFlag(SurfNum) = 0
  IF(InsideGlassTemp < RoomDewPoint) InsideGlassCondensationFlag(SurfNum) = 1
      ! If airflow window, is there condensation on either glass face of the airflow gap?
  IF(SurfaceWindow(SurfNum)%AirflowThisTS > 0.0d0) THEN
    Tleft  = thetas(2*ngllayer-2)-TKelvin
    Tright = thetas(2*ngllayer-1)-TKelvin
    IF(SurfaceWindow(SurfNum)%AirflowSource == AirFlowWindow_Source_IndoorAir) THEN
      IF(Tleft < RoomDewpoint .OR. Tright < RoomDewpoint) InsideGlassCondensationFlag(SurfNum) = 1
    ELSE IF(SurfaceWindow(SurfNum)%AirflowSource == AirFlowWindow_Source_OutdoorAir) THEN
      IF(Tleft < OutDewpointTemp .OR. Tright < OutDewpointTemp) InsideGlassCondensationFlag(SurfNum) = 1
    END IF
  END IF

  ! Do frame and divider calculation
  IF(SurfaceWindow(SurfNum)%FrameArea > 0.0d0 .OR. SurfaceWindow(SurfNum)%DividerArea > 0.0d0) &
     CALL CalcWinFrameAndDividerTemps(SurfNum,tout,tin,hcout,hcin,outir,ConstrNum)
  IF(SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
     InsideFrameCondensationFlag(SurfNum) = 0
     IF(SurfaceWindow(SurfNum)%FrameTempSurfIn < RoomDewPoint) InsideFrameCondensationFlag(SurfNum) = 1
  END IF
  IF(SurfaceWindow(SurfNum)%DividerArea > 0.0d0) THEN
     InsideDividerCondensationFlag(SurfNum) = 0
     IF(SurfaceWindow(SurfNum)%DividerTempSurfIn < RoomDewPoint) InsideDividerCondensationFlag(SurfNum) = 1
  END IF
ENDIF
!update exterior environment surface heat loss reporting
Tsout                          = SurfOutsideTemp + TKelvin
QdotConvOutRep(SurfNum)        = - Surface(SurfNum)%Area * hcout *(Tsout - Tout)
QdotConvOutRepPerArea(SurfNum) = - hcout *(Tsout - Tout)
QConvOutReport(SurfNum)        = QdotConvOutRep(SurfNum)* SecInHour * TimeStepZone

QdotRadOutRep(SurfNum) = - Surface(SurfNum)%Area * SurfOutsideEmiss * &
                             ((1.0d0-AirSkyRadSplit(SurfNum))* Surface(SurfNum)%ViewFactorSkyIR  +   &
                                Surface(SurfNum)%ViewFactorGroundIR) &
                                * sigma * (Tsout**4 - tout**4) &
                          -  Surface(SurfNum)%Area *SurfOutsideEmiss *AirSkyRadSplit(SurfNum)*   &
                             Surface(SurfNum)%ViewFactorSkyIR  &
                                * sigma * (Tsout**4 - SkyTempKelvin**4)
QdotRadOutRepPerArea(SurfNum) =- SurfOutsideEmiss * &
                             ((1.0d0-AirSkyRadSplit(SurfNum))*Surface(SurfNum)%ViewFactorSkyIR +   &
                                Surface(SurfNum)%ViewFactorGroundIR) &
                                * sigma * (Tsout**4 - tout**4) &
                          - SurfOutsideEmiss *AirSkyRadSplit(SurfNum) * Surface(SurfNum)%ViewFactorSkyIR &
                               * sigma * (Tsout**4 - SkyTempKelvin**4)
QRadOutReport(SurfNum) =  QdotRadOutRep(SurfNum) * SecInHour * TimeStepZone

RETURN
END SUBROUTINE CalcWindowHeatBalance
!****************************************************************************

SUBROUTINE WindowHeatBalanceEquations(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   February 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Evaluates heat balance functions at each glass face.
          ! Also evaluates Jacobian.
          ! Currently limited to three glass layers.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER           :: SurfNum                      ! Surface number
REAL(r64)         :: hgap(5)                      ! Gap gas conductance
REAL(r64)  :: gr                           ! Gap gas Grashof number
REAL(r64)  :: con                          ! Gap gas conductivity
REAL(r64)  :: pr                           ! Gap gas Prandtl number
REAL(r64)  :: nu                           ! Gap gas Nusselt number

           ! FLOW

! Have to zero fvec each time since LUdecompostion and LUsolution may
! add values to this array in unexpected places
fvec = 0.0d0

SELECT CASE(ngllayer)

  CASE (1) ! single pane
    fvec(1) = outir*emis(1) - emis(1)*sigma*thetas(1)**4 + scon(1)*(thetas(2)-thetas(1)) &
                + hcout*(tout-thetas(1)) + AbsRadGlassFace(1)
    fvec(2) = rmir*emis(2) - emis(2)*sigma*thetas(2)**4 + scon(1)*(thetas(1)-thetas(2)) &
                + hcin*(tin-thetas(2)) + AbsRadGlassFace(2)

  CASE (2) ! double pane
    call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
    call NusseltNumber(SurfNum,thetas(2),thetas(3),1,gr,pr,nu)
    hgap(1) = (con/gap(1)*nu) * SurfaceWindow(SurfNum)%EdgeGlCorrFac

    fvec(1) = outir*emis(1) - emis(1)*sigma*thetas(1)**4 + scon(1)*(thetas(2)-thetas(1)) &
                + hcout*(tout-thetas(1)) + AbsRadGlassFace(1)
    fvec(2) = scon(1)*(thetas(1)-thetas(2)) + hgap(1)*(thetas(3)-thetas(2)) &
                + A23*(thetas(2)**4 - thetas(3)**4) + AbsRadGlassFace(2)
    fvec(3) = hgap(1)*(thetas(2)-thetas(3)) + scon(2)*(thetas(4)-thetas(3)) &
                + A23*(thetas(3)**4 - thetas(2)**4) + AbsRadGlassFace(3)
    fvec(4) = rmir*emis(4) - emis(4)*sigma*thetas(4)**4 + scon(2)*(thetas(3)-thetas(4)) &
                + hcin*(tin-thetas(4)) + AbsRadGlassFace(4)

  CASE (3) ! Triple Pane
    call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
    call NusseltNumber(SurfNum,thetas(2),thetas(3),1,gr,pr,nu)
    hgap(1) = con/gap(1)*nu * SurfaceWindow(SurfNum)%EdgeGlCorrFac

    call WindowGasConductance(thetas(4),thetas(5),2,con,pr,gr)
    call NusseltNumber(SurfNum,thetas(4),thetas(5),2,gr,pr,nu)
    hgap(2) = con/gap(2)*nu * SurfaceWindow(SurfNum)%EdgeGlCorrFac

    fvec(1) = outir*emis(1) - emis(1)*sigma*thetas(1)**4 + scon(1)*(thetas(2)-thetas(1)) &
                + hcout*(tout-thetas(1)) + AbsRadGlassFace(1)
    fvec(2) = scon(1)*(thetas(1)-thetas(2)) + hgap(1)*(thetas(3)-thetas(2)) &
                + A23*(thetas(2)**4 - thetas(3)**4) + AbsRadGlassFace(2)
    fvec(3) = hgap(1)*(thetas(2)-thetas(3)) + scon(2)*(thetas(4)-thetas(3)) &
                + A23*(thetas(3)**4 - thetas(2)**4) + AbsRadGlassFace(3)
    fvec(4) = scon(2)*(thetas(3)-thetas(4)) + hgap(2)*(thetas(5)-thetas(4)) &
                + A45*(thetas(4)**4 - thetas(5)**4) + AbsRadGlassFace(4)
    fvec(5) = hgap(2)*(thetas(4)-thetas(5)) + scon(3)*(thetas(6)-thetas(5)) &
                + A45*(thetas(5)**4 - thetas(4)**4) + AbsRadGlassFace(5)
    fvec(6) = rmir*emis(6) - emis(6)*sigma*thetas(6)**4 + scon(3)*(thetas(5)-thetas(6)) &
                + hcin*(tin-thetas(6)) + AbsRadGlassFace(6)

  CASE (4) ! Quad Pane
    call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
    call NusseltNumber(SurfNum,thetas(2),thetas(3),1,gr,pr,nu)
    hgap(1) = con/gap(1)*nu * SurfaceWindow(SurfNum)%EdgeGlCorrFac

    call WindowGasConductance(thetas(4),thetas(5),2,con,pr,gr)
    call NusseltNumber(SurfNum,thetas(4),thetas(5),2,gr,pr,nu)
    hgap(2) = con/gap(2)*nu * SurfaceWindow(SurfNum)%EdgeGlCorrFac

    call WindowGasConductance(thetas(6),thetas(7),3,con,pr,gr)
    call NusseltNumber(SurfNum,thetas(6),thetas(7),3,gr,pr,nu)
    hgap(3) = con/gap(3)*nu * SurfaceWindow(SurfNum)%EdgeGlCorrFac

    fvec(1) = outir*emis(1) - emis(1)*sigma*thetas(1)**4 + scon(1)*(thetas(2)-thetas(1)) &
                + hcout*(tout-thetas(1)) + AbsRadGlassFace(1)
    fvec(2) = scon(1)*(thetas(1)-thetas(2)) + hgap(1)*(thetas(3)-thetas(2)) &
                + A23*(thetas(2)**4 - thetas(3)**4) + AbsRadGlassFace(2)
    fvec(3) = hgap(1)*(thetas(2)-thetas(3)) + scon(2)*(thetas(4)-thetas(3)) &
                + A23*(thetas(3)**4 - thetas(2)**4) + AbsRadGlassFace(3)
    fvec(4) = scon(2)*(thetas(3)-thetas(4)) + hgap(2)*(thetas(5)-thetas(4)) &
                + A45*(thetas(4)**4 - thetas(5)**4) + AbsRadGlassFace(4)
    fvec(5) = hgap(2)*(thetas(4)-thetas(5)) + scon(3)*(thetas(6)-thetas(5)) &
                + A45*(thetas(5)**4 - thetas(4)**4) + AbsRadGlassFace(5)
    fvec(6) = scon(3)*(thetas(5)-thetas(6)) + hgap(3)*(thetas(7)-thetas(6)) &
                + A67*(thetas(6)**4 - thetas(7)**4) + AbsRadGlassFace(6)
    fvec(7) = hgap(3)*(thetas(6)-thetas(7)) + scon(4)*(thetas(8)-thetas(7)) &
                + A67*(thetas(7)**4 - thetas(6)**4) + AbsRadGlassFace(7)
    fvec(8) = rmir*emis(8) - emis(8)*sigma*thetas(8)**4 + scon(4)*(thetas(7)-thetas(8)) &
                + hcin*(tin-thetas(8)) + AbsRadGlassFace(8)
END SELECT

return
END SUBROUTINE WindowHeatBalanceEquations
!****************************************************************************
SUBROUTINE SolveForWindowTemperatures(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   July 2000
          !       MODIFIED       Oct 2000, FW: modify edge-of-glass correction to account
          !                       for gap radiative conductance affects
          !                      Feb 2001, FW: add interior or exterior shade to layer
          !                       heat balance calculation.
          !                      Mar 2001, FW: relax error tolerance if MaxIterations reached.
          !                      Jun 2001, FW: add interior or exterior blind
          !                      Nov 2002, FW: add relaxation on face temperatures
          !                       to improve convergence for multipane cases where outer pane
          !                       has high solar absorptance: temp --> 0.5*(temp + previous temp);
          !                       also, increase MaxIterations from 50 to 100.
          !                      Dec 2002, FW: add between-glass shade/blind for double and triple glazing.
          !                      Mar 2003, FW: remove redundant relaxation on radiative conductances
          !                      Mar 2003, FW: increase convergence tolerance from 0.01 to 0.02 to enhance
          !                                    convergence in difficult cases.
          !                      June 2003, FW: correct the expression for convective gain to zone air
          !                       from airflow windows with airflow destination = InsideAir. Previously
          !                       convective gain of air as it passed through gap was used, which is correct
          !                       for airflow source = InsideAir but incorrect for airflow source = OutsideAir.
          !                       Save SurfaceWindow%TAirflowGapOutlet for use in calculating convective heat
          !                       gain to return air when airflow source = InsideAir, destination = ReturnAir.
          !                      Dec 2003, FW: enhance converge for difficult cases by increasing relaxation
          !                       in layer surface temperatures for iterations > MaxIterations/4
          !                      May 2006, RR: add exterior window screen
          !                      January 2009, BG: inserted call to recalc inside face convection inside iteration loop
          !                        per ISO 15099 Section 8.3.2.2
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Evaluates the coefficients Aface and Bface in the system of linear
          ! algebraic equations
          !
          !     Sum [Aface(i,j)*thetas(j)] = Bface(i), i = 1,nglfacep, j=1,nglfacep
          !
          ! where
          !
          ! nglface  = number of glass faces (= 2 * number of glass layers), or, if shade or blind is present,
          ! nglgacep = number of glass faces + 2
          !
          ! thetas(j) = temperature of face j
          !
          ! If an interior, exterior or between-glass shade or blind, or exterior screen is present
          ! the face numbering is as follows:
          !   1 to 2*nglface are glass faces, from outside to inside;
          !   2*nglface+1 and 2*nglface+2 are the shade or blind faces, from outside to inside
          ! For example, the following diagram shows the face number for an exterior shade, screen or blind
          ! on double glazing:
          !
          !     ||   ||   ||
          !    5||6 1||2 3||4
          !     ||   ||   ||
          ! bl/sh/sc gl   gl

          ! And for a between-glass shade/blind in triple glazing:
          !
          !     ||   ||   ||   ||
          !    1||2 3||4 7||8 5||6
          !     ||   ||   ||   ||
          !     gl   gl  bl/sh gl

          ! METHODOLOGY EMPLOYED:
          ! The Aface and Bface coefficients are determined by the equations for
          ! heat balance at the glass and shade/blind faces. The system of linear equations is solved
          ! by LU decomposition.

          ! REFERENCES:
          ! na
  USE General, ONLY: InterpSw, InterpSlatAng, TrimSigDigits, RoundSigDigits
  USE Psychrometrics, ONLY:PsyCpAirFnWTdb,PsyRhoAirFnPbTdbW,PsyHFnTdbW,PsyTdbFnHW
  USE InputProcessor, ONLY: SameString
  USE ConvectionCoefficients, ONLY: CalcISO15099WindowIntConvCoeff
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: SurfNum               ! Surface number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIterations = 100   ! Maximum allowed number of iterations (increased 9/01 from 15 to 50,
                                              !   increased 11/02 from 50 to 100)
  REAL(r64), PARAMETER    :: errtemptol = 0.02d0     ! Tolerance on errtemp for convergence (increased from 0.01, 3/4/03)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER           :: ZoneNum               ! Zone number corresponding to SurfNum
INTEGER           :: i                     ! Counter
REAL(r64)         :: hgap(5)               ! Gap gas conductance (W/m2-K)
REAL(r64)  :: gr                    ! Grashof number of gas in a gap
REAL(r64)  :: con                   ! Gap gas conductivity
REAL(r64)  :: pr                    ! Gap gas Prandtl number
REAL(r64)  :: nu                    ! Gap gas Nusselt number
REAL(r64)         :: hr(10)                ! Radiative conductance (W/m2-K)
REAL(r64)         :: d                     ! +1 if number of row interchanges is even,
                                           ! -1 if odd (in LU decomposition)
INTEGER           :: indx(10)              ! Vector of row permutations in LU decomposition
REAL(r64)  :: Aface(10,10)          ! Coefficient in equation Aface*thetas = Bface
REAL(r64)  :: Bface(10)             ! Coefficient in equation Aface*thetas = Bface

INTEGER           :: iter                  ! Iteration number
REAL(r64)         :: hrprev(10)            ! Value of hr from previous iteration
REAL(r64)         :: errtemp               ! Absolute value of sum of face temperature differences
                                           !   between iterations, divided by number of faces
REAL(r64)         :: VGap                  ! Air velocity in gap between glass and shade/blind (m/s)
REAL(r64)         :: VAirflowGap           ! Air velocity in airflow gap between glass panes (m/s)
REAL(r64)         :: VGapPrev              ! Value of VGap from previous iteration
REAL(r64)         :: TGapNew               ! Average air temp in gap between glass and shade/blind (K)
REAL(r64)         :: TAirFlowGapNew        ! Average air temp in airflow gap between glass panes (K)
REAL(r64)         :: TGapOutlet            ! Temperature of air leaving gap between glass and shade/blind (K)
REAL(r64)         :: TAirflowGapOutlet     ! Temperature of air leaving airflow gap between glass panes (K)
REAL(r64)         :: TAirflowGapOutletC    ! Temperature of air leaving airflow gap between glass panes (C)
REAL(r64)         :: TGapNewBG(2)          ! For between-glass shade/blind, average gas temp in gaps on either
                                           !  side of shade/blind (K)
REAL(r64)         :: hcv                   ! Convection coefficient from gap glass or shade/blind to gap air (W/m2-K)
REAL(r64)         :: hcvAirflowGap         ! Convection coefficient from airflow gap glass to airflow gap air (W/m2-K)
REAL(r64)         :: hcvPrev               ! Value of hcv from previous iteration
REAL(r64)         :: hcvBG(2)              ! For between-glass shade/blind, convection coefficient from gap glass or
                                           !  shade/blind to gap gas on either side of shade/blind (W/m2-K)
REAL(r64)         :: ConvHeatFlowNatural   ! Convective heat flow from gap between glass and interior shade or blind (W)
REAL(r64)         :: ConvHeatFlowForced    ! Convective heat flow from forced airflow gap (W)
REAL(r64)         :: ShGlReflFacIR         ! Factor for long-wave inter-reflection between shade/blind and adjacent glass
REAL(r64)         :: RhoGlIR1,RhoGlIR2     ! Long-wave reflectance of glass surface facing shade/blind; 1=exterior shade/blind,
                                           !  2=interior shade/blind
REAL(r64)         :: RhoShIR1,RhoShIR2     ! Long-wave reflectance of shade/blind surface facing glass; 1=interior shade/blind,
                                           !  2=exterior shade/blind
REAL(r64)         :: EpsShIR1,EpsShIR2     ! Long-wave emissivity of shade/blind surface facing glass; 1=interior shade/blind,
                                           !  2=exterior shade/blind
REAL(r64)         :: TauShIR               ! Long-wave transmittance of isolated shade/blind
REAL(r64)         :: sconsh                ! shade/blind conductance (W/m2-K)
INTEGER           :: ShadeFlag             ! Shading flag
REAL(r64)         :: ShadeAbsFac1,ShadeAbsFac2 ! Fractions for apportioning absorbed radiation to shade/blind faces
REAL(r64)         :: AbsRadShadeFace(2)    ! Solar radiation, short-wave radiation from lights, and long-wave
                                           !  radiation from lights and zone equipment absorbed by faces of shade/blind (W/m2)
REAL(r64)         :: ShadeArea             ! shade/blind area (m2)
REAL(r64)         :: CondHeatGainGlass     ! Conduction through inner glass layer, outside to inside (W)
REAL(r64)         :: CondHeatGainShade     ! Conduction through shade/blind, outside to inside (W)
REAL(r64)         :: NetIRHeatGainGlass    ! Net IR heat gain to zone from shade/blind side of glass when interior
                                           !  shade/blind is present. Zero if shade/blind has zero IR transmittance (W)
REAL(r64)         :: NetIRHeatGainShade    ! Net IR heat gain to zone from interior shade/blind (W)
REAL(r64)         :: ConvHeatGainFrZoneSideOfShade ! Convective heat gain to zone from side of interior shade facing zone (W)
REAL(r64)         :: ConvHeatGainFrZoneSideOfGlass ! Convective heat gain to zone from side of glass facing zone when
                                                   !  no interior shade/blind is present (W)
REAL(r64)         :: IncidentSolar         ! Solar incident on outside of window (W)
INTEGER          :: ConstrNum, ConstrNumSh ! Construction number, bare and with shading device
REAL(r64)         :: TransDiff             ! Diffuse shortwave transmittance
REAL(r64)         :: RhoIR(10)             ! Face IR reflectance
REAL(r64)         :: FacRhoIR25            ! Intermediate variable
REAL(r64)         :: FacRhoIR63            ! Intermediate variable
REAL(r64)         :: RhoIRfp               ! Intermediate variable
REAL(r64)         :: RhoIRbp               ! Intermediate variable
REAL(r64)         :: FacRhoIR2fp           ! Intermediate variable
REAL(r64)         :: FacRhoIR3bp           ! Intermediate variable
REAL(r64)         :: FacRhoIR2fpRhoIR63    ! Intermediate variable
REAL(r64)         :: FacRhoIR3bpRhoIR25    ! Intermediate variable
REAL(r64)         :: FacRhoIR47            ! Intermediate variable
REAL(r64)         :: FacRhoIR85            ! Intermediate variable
REAL(r64)         :: FacRhoIR4fp           ! Intermediate variable
REAL(r64)         :: FacRhoIR5bp           ! Intermediate variable
REAL(r64)         :: FacRhoIR4fpRhoIR85    ! Intermediate variable
REAL(r64)         :: FacRhoIR5bpRhoIR47    ! Intermediate variable
REAL(r64)         :: ConvHeatGainToZoneAir ! Convective heat gain to zone air from window gap airflow (W)
REAL(r64)         :: TotAirflowGap         ! Total volumetric airflow through window gap (m3/s)
REAL(r64)         :: CpAirOutlet           ! Heat capacity of air from window gap (J/kg-K)
REAL(r64)         :: CpAirZone             ! Heat capacity of zone air (J/kg-K)
REAL(r64)         :: InletAirHumRat        ! Humidity ratio of air from window gap entering fan
!!unused REAL(r64)         :: RhoAir                ! Density of air from window gap entering fan (kg/m3)
!!unused REAL(r64)         :: MassFlow              ! Mass flow of air from window gap entering fan (kg/s)
REAL(r64)         :: ZoneTemp              ! Zone air temperature (C)
Integer           :: InsideFaceIndex       ! intermediate variable for index of inside face in thetas

iter = 0
ConvHeatFlowNatural = 0.0d0
ConvHeatFlowForced = 0.0d0
nglfacep = nglface
ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
ZoneNum = Surface(SurfNum)%Zone
AbsRadShadeFace = 0.0d0
TGapNew=0.0d0

IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR. &
   ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
  nglfacep = nglface + 2
  ShadeAbsFac1 = SurfaceWindow(SurfNum)%ShadeAbsFacFace(1)
  ShadeAbsFac2 = SurfaceWindow(SurfNum)%ShadeAbsFacFace(2)
  AbsRadShadeFace(1) = (SurfaceWindow(SurfNum)%ExtBeamAbsByShade + SurfaceWindow(SurfNum)%ExtDiffAbsByShade) * &
                          ShadeAbsFac1 + &
                       (SurfaceWindow(SurfNum)%IntBeamAbsByShade + SurfaceWindow(SurfNum)%IntSWAbsByShade) * &
                          ShadeAbsFac2
  AbsRadShadeFace(2) = (SurfaceWindow(SurfNum)%ExtBeamAbsByShade + SurfaceWindow(SurfNum)%ExtDiffAbsByShade) * &
                          ShadeAbsFac2 + &
                       (SurfaceWindow(SurfNum)%IntBeamAbsByShade + SurfaceWindow(SurfNum)%IntSWAbsByShade) * &
                          ShadeAbsFac1
  IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) &
    AbsRadShadeFace(2) = AbsRadShadeFace(2) + SurfaceWindow(SurfNum)%IntLWAbsByShade
  sconsh  = scon(ngllayer+1)
  TauShIR = tir(nglface+1)
  EpsShIR1 = emis(nglface+1)
  EpsShIR2 = emis(nglface+2)
  RhoShIR1 = MAX(0.d0,1.d0-TauShIR-EpsShIR1)
  RhoShIR2 = MAX(0.d0,1.d0-TauShIR-EpsShIR2)
  IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
    RhoGlIR2 = 1.d0-emis(2*ngllayer)
    ShGlReflFacIR = 1.d0-RhoGlIR2*RhoShIR1
  ELSE IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
    RhoGlIR1 = 1.d0-emis(1)
    ShGlReflFacIR = 1.d0-RhoGlIR1*RhoShIR2
  END IF
END IF  ! End of check if shade or blind is on

! Initialize face temperatures.

CALL StartingWindowTemps(SurfNum,AbsRadShadeFace)

hcvPrev=0.0d0
VGapPrev=0.0d0

! Calculate radiative conductances

errtemp=errtemptol*2.0d0

DO WHILE (iter < MaxIterations .AND. errtemp > errtemptol)

  DO i = 1,nglfacep
    hr(i) = emis(i) * sigma * thetas(i)**3
      ! Following line is redundant since thetas is being relaxed;
      ! removed by FCW, 3/4/03
      !!fw if(iter >= 1) hr(i) = 0.5*(hrprev(i)+hr(i))
    hrprev(i) = hr(i)
  END DO

! call for new interior film coeff (since it is temperature dependent) if using Detailed inside coef model
  IF (((Surface(SurfNum)%IntConvCoeff == 0) .AND. (Zone(ZoneNum)%InsideConvectionAlgo == ASHRAETARP)) &
     .OR. (Surface(SurfNum)%IntConvCoeff == -2)) Then
    ! coef model is "detailed" and not prescribed by user
    !need to find inside face index, varies with shade/blind etc.
    IF (ShadeFlag==IntShadeOn .OR. ShadeFlag==IntBlindOn) Then
      InsideFaceIndex = nglfacep
    ELSE
      InsideFaceIndex = nglface
    ENDIF
    CALL CalcISO15099WindowIntConvCoeff(SurfNum,thetas(InsideFaceIndex)-KelvinConv,tin-KelvinConv)
    hcin = HconvIn(SurfNum)
  ENDIF

  Aface = 0.0d0
  Bface = 0.0d0

! If interior or exterior shade or blind is present, get heat transfer
! coefficient from glass and shade/blind to gap between glass and shade/blind,
! effective gap air temperature, velocity of air in gap and gap outlet temperature.

  IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR. &
     ShadeFlag==ExtScreenOn) THEN
    CALL ExtOrIntShadeNaturalFlow(SurfNum,iter,VGap,TGapNew,TGapOutlet,hcv,ConvHeatFlowNatural)
    IF(iter >= 1) THEN
      hcv =  0.5d0*(hcvPrev + hcv)
      VGap = 0.5d0*(VGapPrev + VGap)
    END IF
    hcvPrev = hcv
    VGapPrev = VGap
  END IF

  TAirFlowGapOutlet=0.0d0
! If between-glass shade or blind is not present and this is an airflow window
! (i.e., with forced airflow in the gap for double glass or in the inner gap for triple glass)
! get glass-to-air forced convection heat transfer coefficient, average gap air temperature, and
! convective heat flow from gap.

  IF(ShadeFlag /= BGShadeOn .AND. ShadeFlag /= BGBlindOn .AND. SurfaceWindow(SurfNum)%AirflowThisTS > 0.0d0) THEN
    CALL BetweenGlassForcedFlow(SurfNum,iter,VAirflowGap,TAirFlowGapNew,TAirFlowGapOutlet,hcvAirflowGap,ConvHeatFlowForced)
  ENDIF

! If between-glass shade or blind is present, get convective heat transfer
! coefficients from glass and shade/blind to the two gaps on either side of the shade/blind.
! Also get average gas temperature in the two gaps, and, for airflow window, the sum of the
! convective heat flows from the gaps.

  IF(ShadeFlag == BGShadeOn .OR. ShadeFlag == BGBlindOn) THEN
    IF(SurfaceWindow(SurfNum)%AirflowThisTS == 0.0d0) THEN  ! Natural convection in gaps
      CALL BetweenGlassShadeNaturalFlow(SurfNum,iter,VGap,TGapNewBG,hcvBG)
    ELSE                                            ! Forced convection in gaps
      CALL BetweenGlassShadeForcedFlow(SurfNum,iter,VGap,TGapNewBG,TAirFlowGapOutlet,hcvBG,ConvHeatFlowForced)
    END IF
  END IF

  iter = iter + 1
  SurfaceWindow(SurfNum)%WindowCalcIterationsRep = iter

  ! Calculations based on number of glass layers
  SELECT CASE(ngllayer)

    CASE (1)
      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = rmir*emis(2) + hcin*tin + AbsRadGlassFace(2)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)
      Aface(2,1) = -scon(1)
      Aface(2,2) = hr(2) + scon(1) + hcin

      IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
        Bface(2) = rmir*emis(2)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(2)
        Bface(3) = rmir*TauShIR*RhoGlIR2*EpsShIR1/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(1)
        Bface(4) = rmir*EpsShIR2 + hcin*tin + AbsRadShadeFace(2)

        Aface(2,2) = hr(2)*(1-RhoShIR1)/ShGlReflFacIR + scon(1) + hcv
        Aface(2,3) = -emis(2)*hr(3)/ShGlReflFacIR
        Aface(3,2) = -hr(2)*EpsShIR1/ShGlReflFacIR
        Aface(3,3) = hr(3)*(1-RhoGlIR2*(EpsShIR1+RhoShIR1))/ShGlReflFacIR + sconsh + hcv
        Aface(3,4) = -sconsh
        Aface(4,3) = -sconsh
        Aface(4,4) = hr(4) + sconsh + hcin
      END IF

      IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
        Bface(1) = outir*emis(1)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(1)
        Bface(3) = outir*EpsShIR1 + hcout*tout + AbsRadShadeFace(1)
        Bface(4) = outir*TauShIR*RhoGlIR1*EpsShIR2/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(2)

        Aface(1,1) = hr(1)*(1-RhoShIR2)/ShGlReflFacIR + scon(1) + hcv
        Aface(1,4) = -emis(1)*hr(4)/ShGlReflFacIR
        Aface(3,3) = hr(3) + sconsh + hcout
        Aface(3,4) = -sconsh
        Aface(4,1) = -hr(1)*EpsShIR2/ShGlReflFacIR
        Aface(4,3) = -sconsh
        Aface(4,4) = hr(4)*(1-RhoGlIR1*(EpsShIR2+RhoShIR2))/ShGlReflFacIR + sconsh + hcv
      END IF

    CASE (2)
      call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
      call NusseltNumber(SurfNum,thetas(2),thetas(3),1,gr,pr,nu)
      hgap(1) = con/gap(1)*nu
      IF(SurfaceWindow(SurfNum)%EdgeGlCorrFac > 1.0d0) THEN ! Edge of glass correction
        hrgap(1) = 0.5d0*ABS(A23)*(thetas(2)+thetas(3))**3
        hgap(1) = hgap(1) * SurfaceWindow(SurfNum)%EdgeGlCorrFac + &
                  hrgap(1) * (SurfaceWindow(SurfNum)%EdgeGlCorrFac - 1.0d0)
      END IF

      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = AbsRadGlassFace(2)
      Bface(3) = AbsRadGlassFace(3)
      Bface(4) = rmir*emis(4) + hcin*tin + AbsRadGlassFace(4)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)

      Aface(2,1) = -scon(1)
      Aface(2,2) = scon(1) + hgap(1) - A23P*hr(2)
      Aface(2,3) = -hgap(1) - A32P*hr(3)

      Aface(3,2) = -hgap(1) + A23P*hr(2)
      Aface(3,3) = hgap(1) + scon(2) + A32P*hr(3)
      Aface(3,4) = -scon(2)

      Aface(4,3) = -scon(2)
      Aface(4,4) = hr(4) + scon(2) + hcin

      IF(ShadeFlag /= BGShadeOn .AND. ShadeFlag /= BGBlindOn .AND. SurfaceWindow(SurfNum)%AirflowThisTS > 0.0d0) THEN
        Bface(2) = AbsRadGlassFace(2) + hcvAirflowGap*TAirflowGapNew
        Bface(3) = AbsRadGlassFace(3) + hcvAirflowGap*TAirflowGapNew
        Aface(2,2) = scon(1) + hcvAirflowGap - A23P*hr(2)
        Aface(2,3) = -A32P*hr(3)
        Aface(3,2) = A23P*hr(2)
        Aface(3,3) = hcvAirflowGap + scon(2) + A32P*hr(3)
      END IF

      IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
        Bface(4) = rmir*emis(4)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(4)
        Bface(5) = rmir*TauShIR*RhoGlIR2*EpsShIR1/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(1)
        Bface(6) = rmir*EpsShIR2 + hcin*tin + AbsRadShadeFace(2)

        Aface(4,4) = hr(4)*(1-RhoShIR1)/ShGlReflFacIR + scon(2) + hcv
        Aface(4,5) = -emis(4)*hr(5)/ShGlReflFacIR
        Aface(5,4) = -hr(4)*EpsShIR1/ShGlReflFacIR
        Aface(5,5) = hr(5)*(1-RhoGlIR2*(EpsShIR1+RhoShIR1))/ShGlReflFacIR + sconsh + hcv
        Aface(5,6) = -sconsh
        Aface(6,5) = -sconsh
        Aface(6,6) = hr(6) + sconsh + hcin
      END IF

      IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
        Bface(1) = outir*emis(1)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(1)
        Bface(5) = outir*EpsShIR1 + hcout*tout + AbsRadShadeFace(1)
        Bface(6) = outir*TauShIR*RhoGlIR1*EpsShIR2/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(2)

        Aface(1,1) = hr(1)*(1-RhoShIR2)/ShGlReflFacIR + scon(1) + hcv
        Aface(1,6) = -emis(1)*hr(6)/ShGlReflFacIR
        Aface(5,5) = hr(5) + sconsh + hcout
        Aface(5,6) = -sconsh
        Aface(6,1) = -hr(1)*EpsShIR2/ShGlReflFacIR
        Aface(6,5) = -sconsh
        Aface(6,6) = hr(6)*(1-RhoGlIR1*(EpsShIR2+RhoShIR2))/ShGlReflFacIR + sconsh + hcv
      END IF

      IF(ShadeFlag == BGShadeOn .OR. ShadeFlag == BGBlindOn) THEN
        DO i = 1,6
          RhoIR(i) = MAX(0.0d0,1.d0-tir(i)-emis(i))
        END DO
        FacRhoIR25  = 1.d0-RhoIr(2)*RhoIR(5)
        FacRhoIR63  = 1.d0-RhoIr(6)*RhoIR(3)
        RhoIRfp     = RhoIR(5) + (tir(5)**2)*RhoIR(3)/FacRhoIR63
        RhoIRbp     = RhoIR(6) + (tir(5)**2)*RhoIR(2)/FacRhoIR25
        FacRhoIR2fp = 1.d0-RhoIRfp*RhoIR(2)
        FacRhoIR3bp = 1.d0-RhoIRbp*RhoIR(3)
        FacRhoIR2fpRhoIR63 = FacRhoIR2fp * FacRhoIR63
        FacRhoIR3bpRhoIR25 = FacRhoIR3bp * FacRhoIR25
        Aface(2,2)  = scon(1) + hcvBG(1) + hr(2)*(1-RhoIRfp*(emis(2)+RhoIR(2)))/FacRhoIR2fp
        Aface(2,3)  = -emis(2)*hr(3)*tir(5)/FacRhoIR2fpRhoIR63
        Aface(2,5)  = -emis(2)*hr(5)/FacRhoIR2fp
        Aface(2,6)  = -emis(2)*hr(6)*RhoIR(3)*tir(5)/FacRhoIR2fpRhoIR63
          Bface(2)  = hcvBG(1)*TGapNewBG(1) + AbsRadGlassFace(2)
        Aface(3,2)  = -emis(3)*hr(2)*tir(5)/FacRhoIR3bpRhoIR25
        Aface(3,3)  = scon(2) + hcvBG(2) + hr(3)*(1-RhoIRbp*(emis(3)+RhoIR(3)))/FacRhoIR3bp
        Aface(3,5)  = -emis(3)*hr(5)*RhoIR(2)*tir(5)/FacRhoIR3bpRhoIR25
        Aface(3,6)  = -emis(3)*hr(6)/FacRhoIR3bp
          Bface(3)  = hcvBG(2)*TGapNewBG(2) + AbsRadGlassFace(3)
        Aface(5,2)  = -emis(5)*hr(2)/FacRhoIR2fp
        Aface(5,3)  = -hr(3)*tir(5)*RhoIR(2)*emis(5)/FacRhoIR2fpRhoIR63
        Aface(5,5)  = sconsh + hcvBG(1) + hr(5)*(1-RhoIR(2)*emis(5)/FacRhoIr2fp)
        Aface(5,6)  = -sconsh - hr(6)*RhoIR(2)*tir(5)*RhoIR(3)*emis(5)/FacRhoIR2fpRhoIR63
          Bface(5)  = hcvBG(1)*TGapNewBG(1) + AbsRadShadeFace(1)
        Aface(6,2)  = -hr(2)*tir(5)*RhoIR(3)*emis(6)/FacRhoIR3bpRhoIR25
        Aface(6,3)  = -emis(6)*hr(3)/FacRhoIR3bp
        Aface(6,5)  = -sconsh - hr(5)*RhoIR(3)*tir(5)*RhoIR(2)*emis(6)/FacRhoIR3bpRhoIR25
        Aface(6,6)  = sconsh + hcvBG(2) + hr(6)*(1-RhoIR(3)*emis(6)/FacRhoIR3bp)
          Bface(6)  = hcvBG(2)*TGapNewBG(2) + AbsRadShadeFace(2)
      END IF

    CASE (3)
      call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
      call NusseltNumber(SurfNum,thetas(2),thetas(3),1,gr,pr,nu)
      hgap(1) = con/gap(1)*nu
      IF(SurfaceWindow(SurfNum)%EdgeGlCorrFac > 1.0d0) THEN ! Edge of glass correction
        hrgap(1) = 0.5d0*ABS(A23)*(thetas(2)+thetas(3))**3
        hgap(1) = hgap(1) * SurfaceWindow(SurfNum)%EdgeGlCorrFac + &
                  hrgap(1) * (SurfaceWindow(SurfNum)%EdgeGlCorrFac - 1.0d0)
      END IF

      call WindowGasConductance(thetas(4),thetas(5),2,con,pr,gr)
      call NusseltNumber(SurfNum,thetas(4),thetas(5),2,gr,pr,nu)
      hgap(2) = con/gap(2)*nu
      IF(SurfaceWindow(SurfNum)%EdgeGlCorrFac > 1.0d0) THEN ! Edge of glass correction
        hrgap(2) = 0.5d0*ABS(A45)*(thetas(4)+thetas(5))**3
        hgap(2) = hgap(2) * SurfaceWindow(SurfNum)%EdgeGlCorrFac + &
                  hrgap(2) * (SurfaceWindow(SurfNum)%EdgeGlCorrFac - 1.0d0)
      END IF

      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = AbsRadGlassFace(2)
      Bface(3) = AbsRadGlassFace(3)
      Bface(4) = AbsRadGlassFace(4)
      Bface(5) = AbsRadGlassFace(5)
      Bface(6) = rmir*emis(6) + hcin*tin + AbsRadGlassFace(6)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)

      Aface(2,1) = -scon(1)
      Aface(2,2) = scon(1) + hgap(1) - A23P*hr(2)
      Aface(2,3) = -hgap(1) - A32P*hr(3)

      Aface(3,2) = -hgap(1) + A23P*hr(2)
      Aface(3,3) = hgap(1) + scon(2) + A32P*hr(3)
      Aface(3,4) = -scon(2)

      Aface(4,3) = -scon(2)
      Aface(4,4) = scon(2) + hgap(2) - A45P*hr(4)
      Aface(4,5) = -hgap(2) - A54P*hr(5)

      Aface(5,4) = -hgap(2) + A45P*hr(4)
      Aface(5,5) = hgap(2) + scon(3) + A54P*hr(5)
      Aface(5,6) = -scon(3)

      Aface(6,5) = -scon(3)
      Aface(6,6) = hr(6) + scon(3) + hcin

      IF(ShadeFlag /= BGShadeOn .AND. ShadeFlag /= BGBlindOn .AND. SurfaceWindow(SurfNum)%AirflowThisTS > 0.0d0) THEN
        Bface(4) = AbsRadGlassFace(4) + hcvAirflowGap*TAirflowGapNew
        Bface(5) = AbsRadGlassFace(5) + hcvAirflowGap*TAirflowGapNew
        Aface(4,4) = scon(2) + hcvAirflowGap - A45P*hr(4)
        Aface(4,5) = -A54P*hr(5)
        Aface(5,4) = A45P*hr(4)
        Aface(5,5) = hcvAirflowGap + scon(3) + A54P*hr(5)
      END IF

      IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
        Bface(6) = rmir*emis(6)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(6)
        Bface(7) = rmir*TauShIR*RhoGlIR2*EpsShIR1/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(1)
        Bface(8) = rmir*EpsShIR2 + hcin*tin + AbsRadShadeFace(2)

        Aface(6,6) = hr(6)*(1-RhoShIR1)/ShGlReflFacIR + scon(3) + hcv
        Aface(6,7) = -emis(6)*hr(7)/ShGlReflFacIR
        Aface(7,6) = -hr(6)*EpsShIR1/ShGlReflFacIR
        Aface(7,7) = hr(7)*(1-RhoGlIR2*(EpsShIR1+RhoShIR1))/ShGlReflFacIR + sconsh + hcv
        Aface(7,8) = -sconsh
        Aface(8,7) = -sconsh
        Aface(8,8) = hr(8) + sconsh + hcin
      END IF

      IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
        Bface(1) = outir*emis(1)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(1)
        Bface(7) = outir*EpsShIR1 + hcout*tout + AbsRadShadeFace(1)
        Bface(8) = outir*TauShIR*RhoGlIR1*EpsShIR2/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(2)

        Aface(1,1) = hr(1)*(1-RhoShIR2)/ShGlReflFacIR + scon(1) + hcv
        Aface(1,8) = -emis(1)*hr(8)/ShGlReflFacIR
        Aface(7,7) = hr(7) + sconsh + hcout
        Aface(7,8) = -sconsh
        Aface(8,1) = -hr(1)*EpsShIR2/ShGlReflFacIR
        Aface(8,7) = -sconsh
        Aface(8,8) = hr(8)*(1-RhoGlIR1*(EpsShIR2+RhoShIR2))/ShGlReflFacIR + sconsh + hcv
      END IF

      IF(ShadeFlag == BGShadeOn .OR. ShadeFlag == BGBlindOn) THEN
        DO i = 1,8
          RhoIR(i) = MAX(0.0d0,1.d0-tir(i)-emis(i))
        END DO
        FacRhoIR47  = 1-RhoIr(4)*RhoIR(7)
        FacRhoIR85  = 1-RhoIr(8)*RhoIR(5)
        RhoIRfp     = RhoIR(7) + (tir(7)**2)*RhoIR(5)/FacRhoIR85
        RhoIRbp     = RhoIR(8) + (tir(7)**2)*RhoIR(4)/FacRhoIR47
        FacRhoIR4fp = 1-RhoIRfp*RhoIR(4)
        FacRhoIR5bp = 1-RhoIRbp*RhoIR(5)
        FacRhoIR4fpRhoIR85 = FacRhoIR4fp * FacRhoIR85
        FacRhoIR5bpRhoIR47 = FacRhoIR5bp * FacRhoIR47
        Aface(4,4)  = scon(2) + hcvBG(1) + hr(4)*(1-RhoIRfp*(emis(4)+RhoIR(4)))/FacRhoIR4fp
        Aface(4,5)  = -emis(4)*hr(5)*tir(7)/FacRhoIR4fpRhoIR85
        Aface(4,7)  = -emis(4)*hr(7)/FacRhoIR4fp
        Aface(4,8)  = -emis(4)*hr(8)*RhoIR(5)*tir(7)/FacRhoIR4fpRhoIR85
          Bface(4)  = hcvBG(1)*TGapNewBG(1) + AbsRadGlassFace(4)
        Aface(5,4)  = -emis(5)*hr(4)*tir(7)/FacRhoIR5bpRhoIR47
        Aface(5,5)  = scon(3) + hcvBG(2) + hr(5)*(1-RhoIRbp*(emis(5)+RhoIR(5)))/FacRhoIR5bp
        Aface(5,7)  = -emis(5)*hr(7)*RhoIR(4)*tir(7)/FacRhoIR5bpRhoIR47
        Aface(5,8)  = -emis(5)*hr(8)/FacRhoIR5bp
          Bface(5)  = hcvBG(2)*TGapNewBG(2) + AbsRadGlassFace(5)
        Aface(7,4)  = -emis(7)*hr(4)/FacRhoIR4fp
        Aface(7,5)  = -hr(5)*tir(7)*RhoIR(4)*emis(7)/FacRhoIR4fpRhoIR85
        Aface(7,7)  = sconsh + hcvBG(1) + hr(7)*(1-RhoIR(4)*emis(7)/FacRhoIr4fp)
        Aface(7,8)  = -sconsh - hr(8)*RhoIR(4)*tir(7)*RhoIR(5)*emis(7)/FacRhoIR4fpRhoIR85
          Bface(7)  = hcvBG(1)*TGapNewBG(1) + AbsRadShadeFace(1)
        Aface(8,4)  = -hr(4)*tir(7)*RhoIR(5)*emis(8)/FacRhoIR5bpRhoIR47
        Aface(8,5)  = -emis(8)*hr(5)/FacRhoIR5bp
        Aface(8,7)  = -sconsh - hr(7)*RhoIR(5)*tir(7)*RhoIR(4)*emis(8)/FacRhoIR5bpRhoIR47
        Aface(8,8)  = sconsh + hcvBG(2) + hr(8)*(1-RhoIR(5)*emis(8)/FacRhoIR5bp)
          Bface(8)  = hcvBG(2)*TGapNewBG(2) + AbsRadShadeFace(2)
      END IF

    CASE (4)
      call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
      call NusseltNumber(SurfNum,thetas(2),thetas(3),1,gr,pr,nu)
      hgap(1) = con/gap(1)*nu
      IF(SurfaceWindow(SurfNum)%EdgeGlCorrFac > 1.0d0) THEN ! Edge of glass correction
        hrgap(1) = 0.5d0*ABS(A23)*(thetas(2)+thetas(3))**3
        hgap(1) = hgap(1) * SurfaceWindow(SurfNum)%EdgeGlCorrFac + &
                  hrgap(1) * (SurfaceWindow(SurfNum)%EdgeGlCorrFac - 1.0d0)
      END IF

      call WindowGasConductance(thetas(4),thetas(5),2,con,pr,gr)
      call NusseltNumber(SurfNum,thetas(4),thetas(5),2,gr,pr,nu)
      hgap(2) = con/gap(2)*nu
      IF(SurfaceWindow(SurfNum)%EdgeGlCorrFac > 1.0d0) THEN ! Edge of glass correction
        hrgap(2) = 0.5d0*ABS(A45)*(thetas(4)+thetas(5))**3
        hgap(2) = hgap(2) * SurfaceWindow(SurfNum)%EdgeGlCorrFac + &
                  hrgap(2) * (SurfaceWindow(SurfNum)%EdgeGlCorrFac - 1.0d0)
      END IF

      call WindowGasConductance(thetas(6),thetas(7),3,con,pr,gr)
      call NusseltNumber(SurfNum,thetas(6),thetas(7),3,gr,pr,nu)
      hgap(3) = con/gap(3)*nu
      IF(SurfaceWindow(SurfNum)%EdgeGlCorrFac > 1.0d0) THEN ! Edge of glass correction
        hrgap(3) = 0.5d0*ABS(A67)*(thetas(6)+thetas(7))**3
        hgap(3) = hgap(3) * SurfaceWindow(SurfNum)%EdgeGlCorrFac + &
                  hrgap(3) * (SurfaceWindow(SurfNum)%EdgeGlCorrFac - 1.0d0)
      END IF
      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = AbsRadGlassFace(2)
      Bface(3) = AbsRadGlassFace(3)
      Bface(4) = AbsRadGlassFace(4)
      Bface(5) = AbsRadGlassFace(5)
      Bface(6) = AbsRadGlassFace(6)
      Bface(7) = AbsRadGlassFace(7)
      Bface(8) = rmir*emis(8) + hcin*tin + AbsRadGlassFace(8)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)

      Aface(2,1) = -scon(1)
      Aface(2,2) = scon(1) + hgap(1) - A23P*hr(2)
      Aface(2,3) = -hgap(1) - A32P*hr(3)

      Aface(3,2) = -hgap(1) + A23P*hr(2)
      Aface(3,3) = hgap(1) + scon(2) + A32P*hr(3)
      Aface(3,4) = -scon(2)

      Aface(4,3) = -scon(2)
      Aface(4,4) = scon(2) + hgap(2) - A45P*hr(4)
      Aface(4,5) = -hgap(2) - A54P*hr(5)

      Aface(5,4) = -hgap(2) + A45P*hr(4)
      Aface(5,5) = hgap(2) + scon(3) + A54P*hr(5)
      Aface(5,6) = -scon(3)

      Aface(6,5) = -scon(3)
      Aface(6,6) = scon(3) + hgap(3) - A67P*hr(6)
      Aface(6,7) = -hgap(3) - A76P*hr(7)

      Aface(7,6) = -hgap(3) + A67P*hr(6)
      Aface(7,7) = hgap(3) + scon(4) + A76P*hr(7)
      Aface(7,8) = -scon(4)

      Aface(8,7) = -scon(4)
      Aface(8,8) = hr(8) + scon(4) + hcin

      IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
        Bface(8) = rmir*emis(8)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(8)
        Bface(9) = rmir*TauShIR*RhoGlIR2*EpsShIR1/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(1)
        Bface(10) = rmir*EpsShIR2 + hcin*tin + AbsRadShadeFace(2)

        Aface(8,8) = hr(8)*(1-RhoShIR1)/ShGlReflFacIR + scon(4) + hcv
        Aface(8,9) = -emis(8)*hr(9)/ShGlReflFacIR
        Aface(9,8) = -hr(8)*EpsShIR1/ShGlReflFacIR
        Aface(9,9) = hr(9)*(1-RhoGlIR2*(EpsShIR1+RhoShIR1))/ShGlReflFacIR + sconsh + hcv
        Aface(9,10) = -sconsh
        Aface(10,9) = -sconsh
        Aface(10,10) = hr(10) + sconsh + hcin
      END IF

      IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
        Bface(1) = outir*emis(1)*TauShIR/ShGlReflFacIR + hcv*TGapNew + AbsRadGlassFace(1)
        Bface(9) = outir*EpsShIR1 + hcout*tout + AbsRadShadeFace(1)
        Bface(10) = outir*TauShIR*RhoGlIR1*EpsShIR2/ShGlReflFacIR + hcv*TGapNew + AbsRadShadeFace(2)

        Aface(1,1) = hr(1)*(1-RhoShIR2)/ShGlReflFacIR + scon(1) + hcv
        Aface(1,10) = -emis(1)*hr(10)/ShGlReflFacIR
        Aface(9,9) = hr(9) + sconsh + hcout
        Aface(9,10) = -sconsh
        Aface(10,1) = -hr(1)*EpsShIR2/ShGlReflFacIR
        Aface(10,9) = -sconsh
        Aface(10,10) = hr(10)*(1-RhoGlIR1*(EpsShIR2+RhoShIR2))/ShGlReflFacIR + sconsh + hcv
      END IF

    CASE DEFAULT
      CALL ShowFatalError('SolveForWindowTemperatures: Invalid number of Glass Layers='//  &
                                    TRIM(TrimSigDigits(ngllayer))//', up to 4 allowed.')
  END SELECT

  call LUdecomposition(Aface,nglfacep,indx,d)    ! Note that these routines change Aface;
  call LUsolution(Aface,nglfacep,indx,Bface)     ! face temperatures are returned in Bface

  DO i = 1,nglfacep
    thetasPrev(i) = thetas(i)
    IF(iter < MaxIterations/4) THEN
      thetas(i) = 0.5d0*thetas(i) + 0.5d0*Bface(i)
    ELSE
      thetas(i) = 0.75d0*thetas(i) + 0.25d0*Bface(i)
    END IF
  END DO

  errtemp = 0.0d0
  DO i = 1,nglfacep
    errtemp = errtemp + ABS(thetas(i)-thetasPrev(i))
  END DO
  errtemp = errtemp/nglfacep

END DO

       ! We have reached iteration limit or we have converged. If we have reached the
       ! iteration limit the following test relaxes the convergence tolerance.
       ! If we have converged (errtemp <= errtemptol) the following test has not effect.

IF(errtemp < 10*errtemptol) THEN

       ! Window heat balance solution has converged.

       ! For interior shade, add convective gain from glass/shade gap air flow to zone convective gain;
       ! For all cases, get total window heat gain for reporting. See CalcWinFrameAndDividerTemps for
       ! contribution of frame and divider.
  IncidentSolar = Surface(SurfNum)%Area * QRadSWOutIncident(SurfNum)
  IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
       ! Interior shade or blind
    SurfaceWindow(SurfNum)%ConvHeatFlowNatural = ConvHeatFlowNatural
       ! Window heat gain from glazing and shade/blind to zone. Consists of transmitted solar, convection
       !   from air exiting gap, convection from zone-side of shade/blind, net IR to zone from shade and net IR to
       !   zone from the glass adjacent to the shade/blind (zero if shade/blind IR transmittance is zero).
       ! Following assumes glazed area = window area (i.e., dividers ignored) in calculating
       !   IR to zone from glass when interior shade/blind is present.
    ShadeArea = Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea
    CondHeatGainShade  = ShadeArea * sconsh * (thetas(nglfacep-1) - thetas(nglfacep))
    NetIRHeatGainShade = ShadeArea * &
    EpsShIR2*(sigma*thetas(nglfacep)**4 - rmir) + &
    EpsShIR1*(sigma*thetas(nglfacep-1)**4 - rmir)*RhoGlIR2*TauShIR/ShGlReflFacIR
    NetIRHeatGainGlass = ShadeArea * &
       (emis(2*ngllayer)*TauShIR/ShGlReflFacIR) * (sigma*thetas(2*ngllayer)**4 - rmir)
    ConvHeatGainFrZoneSideOfShade = ShadeArea * hcin*(thetas(nglfacep) - tin)
    WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatFlowNatural + ConvHeatGainFrZoneSideOfShade + &
                         NetIRHeatGainGlass + NetIRHeatGainShade
    ! store components for reporting
    WinGainConvGlazShadGapToZoneRep(SurfNum) = ConvHeatFlowNatural
    WinGainConvShadeToZoneRep(SurfNum)       = ConvHeatGainFrZoneSideOfShade
    WinGainIRGlazToZoneRep(SurfNum)          = NetIRHeatGainGlass
    WinGainIRShadeToZoneRep(SurfNum)         =  NetIRHeatGainShade
  ELSE
       ! Interior shade or blind not present; innermost layer is glass
    CondHeatGainGlass = Surface(SurfNum)%Area * scon(ngllayer) * (thetas(2*ngllayer-1)-thetas(2*ngllayer))
    NetIRHeatGainGlass = Surface(SurfNum)%Area * emis(2*ngllayer)*(sigma*thetas(2*ngllayer)**4 - rmir)
    ConvHeatGainFrZoneSideOfGlass = Surface(SurfNum)%Area * hcin*(thetas(2*ngllayer) - tin)
    WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatGainFrZoneSideOfGlass + NetIRHeatGainGlass
    ! store components for reporting
    WinGainConvGlazToZoneRep(SurfNum) = ConvHeatGainFrZoneSideOfGlass
    WinGainIRGlazToZoneRep(SurfNum) = NetIRHeatGainGlass
  END IF

      ! Add convective heat gain from airflow window
      ! Note: effect of fan heat on gap outlet temperature is neglected since fan power (based
      ! on pressure drop through the gap) is extremely small

  WinGapConvHtFlowRep(SurfNum) = 0.0d0
  WinGapConvHtFlowRepEnergy(SurfNum) = 0.0d0
  TotAirflowGap = SurfaceWindow(SurfNum)%AirFlowThisTS * Surface(SurfNum)%Width
  TAirflowGapOutletC = TAirflowGapOutlet-TKelvin
  SurfaceWindow(SurfNum)%TAirflowGapOutlet = TAirflowGapOutletC
  IF(SurfaceWindow(SurfNum)%AirFlowThisTS > 0.0d0) THEN
    WinGapConvHtFlowRep(SurfNum) = ConvHeatFlowForced
    WinGapConvHtFlowRepEnergy(SurfNum) = WinGapConvHtFlowRep(SurfNum) * TimeStepZone * SecInHour
    ! Add heat from gap airflow to zone air if destination is inside air; save the heat gain to return
    ! air in case it needs to be sent to the zone (due to no return air determined in HVAC simulation)
    IF(SurfaceWindow(SurfNum)%AirFlowDestination == AirFlowWindow_Destination_IndoorAir  .or.   &
       SurfaceWindow(SurfNum)%AirFlowDestination == AirFlowWindow_Destination_ReturnAir) THEN
      IF (SurfaceWindow(SurfNum)%AirflowSource == AirFlowWindow_Source_IndoorAir) THEN
        InletAirHumRat = ZoneAirHumRat(ZoneNum)
      ELSE  ! AirflowSource = outside air
        InletAirHumRat = OutHumRat
      END IF
      ZoneTemp    = MAT(ZoneNum)  ! this should be Tin (account for different reference temps)
      CpAirOutlet = PsyCpAirFnWTdb(InletAirHumRat,TAirflowGapOutletC)
      CpAirZone   = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),ZoneTemp)
      ConvHeatGainToZoneAir = TotAirflowGap * (CpAirOutlet*(TAirflowGapOutletC) - CpAirZone*ZoneTemp)
      IF (SurfaceWindow(SurfNum)%AirFlowDestination == AirFlowWindow_Destination_IndoorAir) THEN
        SurfaceWindow(SurfNum)%ConvHeatGainToZoneAir = ConvHeatGainToZoneAir
        WinHeatGain(SurfNum) = WinHeatGain(SurfNum) + ConvHeatGainToZoneAir
      ELSE
        SurfaceWindow(SurfNum)%RetHeatGainToZoneAir = ConvHeatGainToZoneAir
      END IF
    END IF
    ! For AirflowDestination = ReturnAir in a controlled (i.e., conditioned) zone with return air, see CalcZoneLeavingConditions
    ! for calculation of modification of return-air temperature due to airflow from window gaps into return air.
  END IF

      ! Correct WinHeatGain for interior diffuse shortwave (solar and shortwave from lights) transmitted
      ! back out window
  ConstrNum = Surface(SurfNum)%Construction
  ConstrNumSh = Surface(SurfNum)%ShadedConstruction
  IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
    ConstrNum = Surface(SurfNum)%StormWinConstruction
    ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
  END IF

  TransDiff = Construct(ConstrNum)%TransDiff   ! Default value for TransDiff here
  IF(ShadeFlag <= 0) THEN
    TransDiff = Construct(ConstrNum)%TransDiff
  ELSE IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) THEN
    TransDiff = Construct(ConstrNumSh)%TransDiff
  ELSE IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR.ShadeFlag==BGBlindOn) THEN
    TransDiff = InterpSlatAng(SurfaceWindow(SurfNum)%SlatAngThisTS,SurfaceWindow(SurfNum)%MovableSlats, &
                               Construct(ConstrNumSh)%BlTransDiff)
  ELSE IF(ShadeFlag == SwitchableGlazing) THEN
    TransDiff = InterpSW(SurfaceWindow(SurfNum)%SwitchingFactor,Construct(ConstrNum)%TransDiff, &
                               Construct(ConstrNumSh)%TransDiff)
  END IF
  WinHeatGain(SurfNum) = WinHeatGain(SurfNum) - QS(Surface(SurfNum)%Zone) * Surface(SurfNum)%Area * TransDiff
    ! shouldn't this be + outward flowing fraction of absorbed SW? -- do not know whose comment this is?  LKL (9/2012)
  WinLossSWZoneToOutWinRep(SurfNum) = QS(Surface(SurfNum)%Zone) * Surface(SurfNum)%Area * TransDiff

  IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR. &
     ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
    WinShadingAbsorbedSolar(SurfNum) = (SurfaceWindow(SurfNum)%ExtBeamAbsByShade + &
                                       SurfaceWindow(SurfNum)%ExtDiffAbsByShade) * &
                                       (Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea)
    WinShadingAbsorbedSolarEnergy(SurfNum) = WinShadingAbsorbedSolar(SurfNum) * TimeStepZone * SecInHour
  END IF
  IF(SunIsUp) THEN
    WinSysSolTransmittance(SurfNum) = WinTransSolar(SurfNum) / &
      (QRadSWOutIncident(SurfNum)*(Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea)+0.0001d0)
    WinSysSolAbsorptance(SurfNum)   = (QRadSWwinAbsTot(SurfNum)+WinShadingAbsorbedSolar(SurfNum)) / &
      (QRadSWOutIncident(SurfNum)*(Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea)+0.0001d0)
    WinSysSolReflectance(SurfNum)   = 1.0d0 - WinSysSolTransmittance(SurfNum) - WinSysSolAbsorptance(SurfNum)
  ELSE
    WinSysSolTransmittance(SurfNum) = 0.0d0
    WinSysSolAbsorptance(SurfNum)   = 0.0d0
    WinSysSolReflectance(SurfNum)   = 0.0d0
  END IF

     ! Save hcv for use in divider calc with interior or exterior shade (see CalcWinFrameAndDividerTemps)
  IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. &
     ShadeFlag==ExtScreenOn) SurfaceWindow(SurfNum)%ConvCoeffWithShade = hcv
ELSE
     ! No convergence after MaxIterations even with relaxed error tolerance
  CALL ShowSevereError('Convergence error in SolveForWindowTemperatures for window '&
    //TRIM(Surface(SurfNum)%Name))
  Call ShowContinueErrorTimestamp(' ')

  If (DisplayExtraWarnings) Then
    !report out temperatures
    DO i = 1,nglfacep
      CAll ShowContinueError('Glazing face index = '//Trim(RoundSigDigits(i,1))// &
                         ' ; new temperature ='//Trim(RoundSigDigits(thetas(i)-KelvinConv,4))//'C '// &
                         ' ; previous temperature = '//Trim(RoundSigDigits(thetasPrev(i)-KelvinConv, 4))//'C' )
    END DO

  ENDIF

  CALL ShowFatalError('Program halted because of convergence error in SolveForWindowTemperatures for window '&
    //TRIM(Surface(SurfNum)%Name))

END IF

RETURN
END SUBROUTINE SolveForWindowTemperatures

!****************************************************************************

SUBROUTINE ExtOrIntShadeNaturalFlow(SurfNum,iter,VGap,TGapNew,TGapOutlet,hcv,QConvGap)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   December 2000
          !       MODIFIED       June 2001: add window blinds
          !                      May 2006 (RR): add exterior window screens
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by SolveForWindowTemperatures for windows that have an interior
          ! or exterior blind or shade in place.
          ! Solves for air flow in gap between glass and shade/blind.
          ! Finds temperature of gap air and coefficient for convective heat transfer
          ! from glass to gap air and shade/blind to gap air.

          ! METHODOLOGY EMPLOYED:
          ! Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
          ! Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT (IN) :: SurfNum             ! Surface number
INTEGER, INTENT (IN) :: iter                ! Iteration number for glass heat balance calculation
REAL(r64), INTENT (OUT)   :: VGap                ! Air velocity in glass-shade/blind gap (m/s)
REAL(r64), INTENT (INOUT)   :: TGapNew           ! Current-iteration average air temp in glass-shade/blind gap (K)
REAL(r64), INTENT (OUT)   :: TGapOutlet          ! Temperature of air leaving glass-shade/blind gap at top for upward
REAL(r64), INTENT (OUT)   :: hcv                 ! Convection coefficient from gap glass or shade to gap air (W/m2-K)
REAL(r64), INTENT (OUT)   :: QConvGap            ! Convective heat gain from glass-shade/blind gap for interior shade (W)
                                            !   air flow or bottom for downward air flow (K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER              :: ConstrNumSh         ! Shaded construction number
INTEGER              :: MatNumSh            ! Material number of shade/blind layer
INTEGER              :: nglassfaces         ! Number of glass faces in contruction
REAL(r64)            :: TGapInlet           ! Temperature of air entering glass-shade/blind gap at bottom for upward
                                            !   air flow or top for downward air flow (K)
REAL(r64)     :: TGlassFace          ! Temperature of glass surface facing glass-shade/blind gap (K)
REAL(r64)     :: TShadeFace          ! Temperature of shade surface facing glass-shade/blind gap (K)
REAL(r64)            :: hGapStill           ! Still-air glass-shade/blind gap conduction/convection coeff (W/m2-K)
REAL(r64)            :: TGapOld             ! Previous-iteration average air temp in glass-shade/blind gap (K)
REAL(r64)            :: GapHeight           ! Vertical length of glass-shade/blind gap (m)
REAL(r64)            :: GapDepth            ! Distance from shade to glass (m)
REAL(r64)            :: RhoAir              ! Density of glass-shade/blind gap air at a temperature of TGapOld (kg/m3)
REAL(r64)            :: RhoTRef             ! Density of glass-shade/blind air at reference temp = KelvinConv (kg/m3)
REAL(r64)            :: ViscAir             ! Viscosity of glass-shade/blind gap air at a temperature of TGapOld (kg/m3)
REAL(r64)            :: AGap                ! Cross sectional area of glass-shade/blind gap (m2); for vertical window, this
                                            !   is in horizontal plane normal to window.
REAL(r64)            :: ATopGap, ABotGap    ! Area of the top and bottom openings (m2)
REAL(r64)            :: ALeftGap, ARightGap ! Area of the left and right openings (m2)
REAL(r64)            :: AHolesGap           ! Area of the holes in the shade (assumed homogeneously
                                            !   distributed) (m2)
REAL(r64)            :: ATopLRH,ABotLRH     ! Intermediate variables
REAL(r64)            :: AEqInlet, AEqOutlet ! Equivalent inlet and outlet opening areas (m2)
REAL(r64)            :: Zinlet, Zoutlet     ! Inlet and outlet pressure loss factors
REAL(r64)            :: AVGap               ! Coeff. of VGap**2 term in pressure balance equation
REAL(r64)            :: BVGap               ! Coeff. of VGap term in pressure balance equation
REAL(r64)            :: CVGap               ! VGap-independent term in pressure balance equation
REAL(r64)            :: GapHeightChar       ! Characteristic height of the gap air temperature profile (m)
REAL(r64)            :: TAve                ! Average of TGlass and TShade (K)
!REAL(r64)            :: AirProps(8)         ! Air properties
INTEGER              :: TotGaps             ! Glass/glass gaps + glass-shade/blind gap
REAL(r64)     :: con                 ! Gap conductivity and derivative
REAL(r64)     :: gr                  ! glass-shade/blind gap Grashof number
REAL(r64)     :: pr                  ! glass-shade/blind gap Prandtl number
REAL(r64)     :: nu                  ! glass-shade/blind gap Nusselt number
INTEGER              :: ShadeFlag           ! Shading flag
INTEGER              :: BlNum               ! Blind number

! Air properties
!               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
!DATA AirProps / 1.29, -0.4e-2, 2.41e-2, 7.6e-5, 1.73e-5, 1.0e-7, 0.72,   1.8e-3  /

ConstrNumSh = SurfaceWindow(SurfNum)%ShadedConstruction
IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
nglassfaces = 2*Construct(ConstrNumSh)%TotGlassLayers
TotGaps = Construct(ConstrNumSh)%TotGlassLayers

IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN ! Interior shade or blind
  MatNumSh = Construct(ConstrNumSh)%LayerPoint(nglassfaces)
  TGapInlet = tin
  TGlassFace = thetas(nglassfaces)
  TShadeFace = thetas(nglassfaces+1)
ELSE ! Exterior shade, screen or blind
  MatNumSh = Construct(ConstrNumsh)%LayerPoint(1)
  TGapInlet = tout
  TGlassFace = thetas(1)
  TShadeFace = thetas(nglassfaces+2)
END IF
TAve = 0.5d0*(TGlassFace + TShadeFace)

IF(iter == 0) THEN
  TGapOld = 0.5d0*(TAve + TGapInlet)
ELSE
  TGapOld = TGapNew
END IF

! Conductance of gap between glass and shade assuming gap is sealed
CALL WindowGasConductance(TGlassFace,TShadeFace,TotGaps,con,pr,gr)
CALL NusseltNumber(SurfNum,TGlassFace,TShadeFace,TotGaps,gr,pr,nu)
hGapStill = con/gap(TotGaps)*nu

! For near-horizontal windows (i.e., no more than 5 deg from horizontal) assume
! there is no air flow thru gap

IF(ABS(Surface(SurfNum)%SinTilt) < 0.0872d0) THEN
  VGap = 0.0d0
  hcv = 2.d0*hGapStill
  QConvGap = 0.0d0
  TGapNew = TAve
  TGapOutlet = TAve
  RETURN
END IF

GapHeight = Surface(SurfNum)%Height

IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtScreenOn) THEN
     ! Shade or Screen on
  GapDepth = Material(MatNumSh)%WinShadeToGlassDist
  AGap = GapDepth * Surface(SurfNum)%Width
  ATopGap = Material(MatNumSh)%WinShadeTopOpeningMult * AGap
  ABotGap = Material(MatNumSh)%WinShadeBottomOpeningMult * AGap
  ALeftGap = Material(MatNumSh)%WinShadeLeftOpeningMult * GapHeight * GapDepth
  ARightGap = Material(MatNumSh)%WinShadeRightOpeningMult * GapHeight * GapDepth
  AHolesGap = Material(MatNumSh)%WinShadeAirFlowPermeability * GapHeight * Surface(SurfNum)%Width
ELSE
     ! Blind on
  BlNum = SurfaceWindow(SurfNum)%BlindNumber
  GapDepth = Blind(BlNum)%BlindToGlassDist
  AGap = GapDepth * Surface(SurfNum)%Width
  ATopGap = Blind(BlNum)%BlindTopOpeningMult * AGap
  ABotGap = Blind(BlNum)%BlindBottomOpeningMult * AGap
  ALeftGap = Blind(BlNum)%BlindLeftOpeningMult * GapHeight * GapDepth
  ARightGap = Blind(BlNum)%BlindRightOpeningMult * GapHeight * GapDepth
  AHolesGap = SurfaceWindow(SurfNum)%BlindAirFlowPermeability * GapHeight * Surface(SurfNum)%Width
END IF

RhoAir  = AirProps(1) + AirProps(2)*(TGapOld-TKelvin)
ViscAir = AirProps(5) + AirProps(6)*(TGapOld-TKelvin)
! The factor 12 in the next line is based on the solution of steady laminar flow between fixed
! parallel plates given in Sec. 6.9.1 of Fundamentals of Fluid Mechanics, Munson/Young/Okishi, Third Edition
! Update, John Wiley & Sons, 1998; ISO 15099 has 8 for this factor, which is for flow through a tube.
BVGap = 12.d0*ViscAir*GapHeight/(GapDepth**2)
! Adding 0.000001 and 0.000002 in the following gives ATopLRH = ABotLRH =
! 0.25*(ALeftGap + ARightGap + AHolesGap) when ABotGap = ATopGap = 0.0 (shade/blind sealed at
! bottom and top but possibly open at left side, right side and/or in-shade/blind)
ATopLRH = 0.5d0*((ATopGap+0.000001d0)/(ABotGap+ATopGap+0.000002d0))*(ALeftGap + ARightGap + AHolesGap)
ABotLRH = 0.5d0*((ABotGap+0.000001d0)/(ABotGap+ATopGap+0.000002d0))*(ALeftGap + ARightGap + AHolesGap)
IF(TGapOld > TGapInlet) THEN
  AEqInlet  = ABotGap + ATopLRH
  AEqOutlet = ATopGap + ABotLRH
ELSE
  AEqOutlet = ABotGap + ATopLRH
  AEqInlet  = ATopGap + ABotLRH
END IF
! Adding 0.000001 in the following gives very large value of Zinlet for AEqInlet = 0 and
! very large value of Zoutlet for AEqInlet = 0; this gives VGap close to zero, as required
! when there is no inlet and/or outlet for air. This then reduces to the
! case of a completely sealed shade, in which hcv = 2*hGapStill and QConvGap = 0.
Zinlet  = (AGap/(0.6d0*AEqInlet+0.000001d0)-1.d0)**2
Zoutlet = (AGap/(0.6d0*AEqOutlet+0.000001d0)-1.d0)**2
AVGap = 0.5d0*RhoAir*(1+Zinlet+Zoutlet)
RhoTRef = AirProps(1)*TKelvin
CVGap = RhoTRef*9.81d0*GapHeight*Surface(SurfNum)%SinTilt* &
  (TGapOld-TGapInlet)/(TGapOld*TGapInlet)

! Solution of quadratic equation in VGap
VGap = (SQRT(BVGap**2 + ABS(4.d0*AVGap*CVGap)) - BVGap)/(2.d0*AVGap)
hcv = 2.d0*hGapStill + 4.d0*VGap
GapHeightChar = RhoAir * 1008.d0 * GapDepth * VGap/(2.d0*hcv)
! The following avoids divide by zero and exponential underflow
IF(GapHeightChar == 0.0d0) THEN
  TGapOutlet = TAve
ELSE IF((GapHeight/GapHeightChar) > 15.d0) THEN
  TGapOutlet = TAve
ELSE
  TGapOutlet = TAve - (TAve-TGapInlet)*EXP(-GapHeight/GapHeightChar)
END IF
TGapNew = TAve - (GapHeightChar/GapHeight) * (TGapOutlet-TGapInlet)

! Convective heat flow from gap to room air for interior shade or blind
IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) THEN
  RhoAir = AirProps(1) + AirProps(2)*(TGapNew-TKelvin)
  QConvGap = RhoAir * AGap * VGap * 1008.d0 * (TGapOutlet - TGapInlet)
  ! Exclude convection to gap due to divider, if present; divider convection handled
  ! separately in CalcWinFrameAndDividerTemps
  QConvGap = QConvGap * 0.5d0 * (1.d0 + Surface(SurfNum)%Area/(Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea))
END IF

RETURN
END SUBROUTINE ExtOrIntShadeNaturalFlow

!****************************************************************************

SUBROUTINE BetweenGlassShadeNaturalFlow(SurfNum,iter,VGap,TGapNew,hcv)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   December 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by SolveForWindowTemperatures for windows that have a
          ! between-glass shade or blind in place.
          ! Solves for gas flow in the two gaps on either side of shade/blind.
          ! Finds average temperature of gas in the two gaps, and the coefficient
          ! for convective heat transfer from glass to gap gas and shade/blind to gap gas
          ! for the two gaps. The two gaps are assumed to have the same depth so that the
          ! gas velocity due to natural convection is the same in the two gaps.
          !
          ! The Between-glass shade/blind is between the two glass layers of double glazing
          ! or between the two inner glass layers of triple glazing. The quadruple glazing
          ! case is not considered.

          ! METHODOLOGY EMPLOYED:
          ! Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
          ! Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT (IN) :: SurfNum             ! Surface number
INTEGER, INTENT (IN) :: iter                ! Iteration number for glass heat balance calculation
REAL(r64), INTENT (OUT)   :: VGap                ! Gas velocity in gaps (m/s)
REAL(r64), INTENT (INOUT) :: TGapNew(2)          ! Current-iteration average gas temp in gaps (K)
REAL(r64), INTENT (OUT)   :: hcv(2)              ! Convection coefficient from gap glass or shade to gap gas (W/m2-K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER              :: ConstrNumSh         ! Shaded construction number
INTEGER              :: MatNumSh            ! Material number of shade/blind layer
INTEGER              :: nglassfaces         ! Number of glass faces in contruction
                                            ! In the following, "gaps" refer to the gaps on either side of the shade/blind
REAL(r64)     :: TGlassFace(2)       ! Temperature of glass surfaces facing gaps (K)
REAL(r64)     :: TShadeFace(2)       ! Temperature of shade surfaces facing gaps (K)
REAL(r64)            :: hGapStill(2)        ! Still-air conduction/convection coeffs for the gaps (W/m2-K)
REAL(r64)            :: TGapOld(2)          ! Previous-iteration average gas temp in gaps (K)
REAL(r64)            :: GapHeight           ! Vertical length of glass-shade/blind gap (m)
REAL(r64)            :: GapDepth            ! Distance from shade/blind to glass; assumed same for both gaps (m)
REAL(r64)            :: RhoGas(2)           ! Density of gap gas at a temperature of TGapOld (kg/m3)
REAL(r64)            :: RhoTRef             ! Density of gap gas at reference temp = KelvinConvK (kg/m3)
REAL(r64)            :: ViscGas(2)          ! Viscosity of gap gas at a temperature of TGapOld (kg/m3)
REAL(r64)            :: RhoGasZero          ! Gas density at KelvinConvK
REAL(r64)            :: ViscGasZero         ! Gas viscosity at KelvinConvK (not used)
REAL(r64)            :: AGap                ! Cross sectional area of gaps (m2); for vertical window, this
                                            !   is in horizontal plane normal to window.
REAL(r64)            :: ATopGap, ABotGap    ! Area of the top and bottom openings of shade/blind (m2)
REAL(r64)            :: ALeftGap, ARightGap ! Area of the left and right openings of shade/blind (m2)
REAL(r64)            :: AHolesGap           ! Area of the holes in the shade/blind (assumed homogeneously
                                            !   distributed) (m2)
REAL(r64)            :: ATopLRH,ABotLRH     ! Intermediate variables
REAL(r64)            :: AEqInlet, AEqOutlet ! Equivalent inlet and outlet opening areas (m2)
REAL(r64)            :: Zinlet, Zoutlet     ! Inlet and outlet pressure loss factors
REAL(r64)            :: AVGap               ! Coeff. of VGap**2 term in pressure balance equation
REAL(r64)            :: BVGap               ! Coeff. of VGap term in pressure balance equation
REAL(r64)            :: CVGap               ! VGap-independent term in pressure balance equation
REAL(r64)            :: GapHeightChar(2)    ! Characteristic height of the gap gas temperature profile (m)
REAL(r64)            :: EpsChar(2)          ! EXP(-GapHeight/GapHeightChar(IGap))
REAL(r64)            :: TAve(2)             ! Average of TGlass and TShade for the gaps (K)
REAL(r64)     :: con                 ! Gap gas conductivity and derivative
REAL(r64)     :: gr                  ! Gap gas Grashof number
REAL(r64)     :: pr                  ! Gap gas Prandtl number
REAL(r64)     :: nu                  ! Gap gas Nusselt number
INTEGER              :: ShadeFlag           ! Shading flag
INTEGER              :: BlNum               ! Blind number
INTEGER              :: IGap                ! Gap counter; 1 = gap on outer side of shade/blind, 2 = gap on inner side.
INTEGER              :: IGapInc             ! Gap increment (0 or 1)


ConstrNumSh = Surface(SurfNum)%ShadedConstruction
ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
nglassfaces = 2*Construct(ConstrNumSh)%TotGlassLayers

IF(Construct(ConstrNumSh)%TotGlassLayers == 2) THEN  ! Double glazing
  MatNumSh = Construct(ConstrNumSh)%LayerPoint(3)
  IGapInc = 0
  DO IGap = 1,2
    TGlassFace(IGap) = thetas(IGap+1)
    TShadeFace(IGap) = thetas(IGap+4)
  END DO
ELSE                                                 ! Triple glazing
  MatNumSh = Construct(ConstrNumSh)%LayerPoint(5)
  IGapInc = 1
  DO IGap = 1,2
    TGlassFace(IGap) = thetas(IGap+3)
    TShadeFace(IGap) = thetas(IGap+6)
  END DO
END IF

DO IGap = 1,2
  TAve(IGap) = 0.5d0*(TGlassFace(IGap) + TShadeFace(IGap))
  IF(iter == 0) THEN
    TGapOld(IGap) = TAve(IGap)
  ELSE
    TGapOld(IGap) = TGapNew(IGap)
  END IF
          ! Conductance of gaps on either side of shade/blind assuming gaps are sealed
  CALL WindowGasConductance(TGlassFace(IGap),TShadeFace(IGap),IGap+IGapInc,con,pr,gr)
  CALL NusseltNumber(SurfNum,TGlassFace(IGap),TShadeFace(IGap),IGap+IGapInc,gr,pr,nu)
  hGapStill(IGap) = con/gap(IGap+IGapInc)*nu
END DO

! For near-horizontal windows (i.e., no more than 5 deg from horizontal) assume
! there is no air flow thru gap

IF(ABS(Surface(SurfNum)%SinTilt) < 0.0872d0) THEN
  VGap = 0.0d0
  DO IGap = 1,2
    hcv(IGap) = 2.d0*hGapStill(IGap)
    TGapNew(IGap) = TAve(IGap)
  END DO
  RETURN
END IF

GapHeight = Surface(SurfNum)%Height
GapDepth = gap(1+IGapInc)
AGap = GapDepth * Surface(SurfNum)%Width

IF(ShadeFlag == BGShadeOn) THEN
     ! Shade on
  ATopGap   = Material(MatNumSh)%WinShadeTopOpeningMult * AGap
  ABotGap   = Material(MatNumSh)%WinShadeBottomOpeningMult * AGap
  ALeftGap  = Material(MatNumSh)%WinShadeLeftOpeningMult * GapHeight * GapDepth
  ARightGap = Material(MatNumSh)%WinShadeRightOpeningMult * GapHeight * GapDepth
  AHolesGap = Material(MatNumSh)%WinShadeAirFlowPermeability * GapHeight * Surface(SurfNum)%Width
ELSE
     ! Blind on
  BlNum = SurfaceWindow(SurfNum)%BlindNumber
  ATopGap = Blind(BlNum)%BlindTopOpeningMult * AGap
  ABotGap = Blind(BlNum)%BlindBottomOpeningMult * AGap
  ALeftGap = Blind(BlNum)%BlindLeftOpeningMult * GapHeight * GapDepth
  ARightGap = Blind(BlNum)%BlindRightOpeningMult * GapHeight * GapDepth
  AHolesGap = SurfaceWindow(SurfNum)%BlindAirFlowPermeability * GapHeight * Surface(SurfNum)%Width
END IF

DO IGap = 1,2
  CALL WindowGasPropertiesAtTemp(TGapOld(IGap),IGap+IGapInc,RhoGas(IGap),ViscGas(IGap))
END DO

BVGap = 12.d0*(ViscGas(1)+ViscGas(2))*GapHeight/(GapDepth**2)
! Adding 0.000001 and 0.000002 in the following gives ATopLRH = ABotLRH =
! 0.25*(ALeftGap + ARightGap + AHolesGap) when ABotGap = ATopGap = 0.0 (shade/blind sealed at
! bottom and top but possibly open at left side, right side and/or in shade/blind)
ATopLRH = 0.5d0*((ATopGap+0.000001d0)/(ABotGap+ATopGap+0.000002d0))*(ALeftGap + ARightGap + AHolesGap)
ABotLRH = 0.5d0*((ABotGap+0.000001d0)/(ABotGap+ATopGap+0.000002d0))*(ALeftGap + ARightGap + AHolesGap)
AEqInlet  = ABotGap + ATopLRH
AEqOutlet = ATopGap + ABotLRH

! Adding 0.000001 in the following gives very large value of Zinlet for AEqInlet = 0 and
! very large value of Zoutlet for AEqInlet = 0; this gives VGap close to zero, as required
! when there is no inlet and/or outlet for air. This then reduces to the
! case of a completely sealed shade, in which hcv = 2*hGapStill and QConvGap = 0.
Zinlet  = (AGap/(0.6d0*AEqInlet+0.000001d0)-1.d0)**2
Zoutlet = (AGap/(0.6d0*AEqOutlet+0.000001d0)-1.d0)**2
AVGap = 0.5d0*(RhoGas(1)+RhoGas(2))*(1.d0+Zinlet+Zoutlet)
CALL WindowGasPropertiesAtTemp(TKelvin,1+IGapInc,RhoGasZero,ViscGasZero)
RhoTRef = RhoGasZero*TKelvin
CVGap = RhoTRef*9.81d0*GapHeight*Surface(SurfNum)%SinTilt* &
  (TGapOld(1)-TGapOld(2))/(TGapOld(1)*TGapOld(2))

! Solution of quadratic equation in VGap

VGap = (SQRT(BVGap**2 + ABS(4*AVGap*CVGap)) - BVGap)/(2*AVGap)

DO IGap = 1,2
  hcv(IGap) = 2.d0*hGapStill(IGap) + 4.d0*VGap
  GapHeightChar(IGap) = RhoGas(IGap) * 1008.d0 * GapDepth * VGap/(2.d0*hcv(IGap))
     ! The following avoids divide by zero and exponential underflow
  IF(GapHeightChar(IGap) == 0.0d0) THEN
    EpsChar(IGap) = 0.0d0
  ELSE IF((GapHeight/GapHeightChar(IGap)) > 15.d0) THEN
    EpsChar(IGap) = 0.0d0
  ELSE
    EpsChar(IGap) = EXP(-GapHeight/GapHeightChar(IGap))
  END IF
END DO

TGapNew(1) = TAve(1) - (TAve(1)-TAve(2))*(GapHeightChar(1)/GapHeight) * &
             (1-EpsChar(1))*(1-EpsChar(2))/(1-EpsChar(1)*EpsChar(2))

TGapNew(2) = TAve(2) - (TAve(2)-TAve(1))*(GapHeightChar(2)/GapHeight) * &
             (1-EpsChar(1))*(1-EpsChar(2))/(1-EpsChar(1)*EpsChar(2))

RETURN
END SUBROUTINE BetweenGlassShadeNaturalFlow

!****************************************************************************

SUBROUTINE BetweenGlassForcedFlow(SurfNum,iter,VGap,TGapNew,TGapOutlet,hcv,QConvGap)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by SolveForWindowTemperatures for "airflow windows",i.e., windows
          ! with forced airflow in one of the gaps between layers of glass. Based on
          ! the velocity of airflow through gap, finds effective temperature of gap air,
          ! convective heat transfer coefficient from glass to gap air,
          ! the gap outlet temperature, and the outlet convective heat flow.

          ! Called only for double and triple glazing. For triple glazing the airflow
          ! is assumed to be between the inner two layers of glass (glass layers 2 and 3).

          ! METHODOLOGY EMPLOYED:
          ! Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
          ! Detailed Calculations"

          ! REFERENCES:
          ! na

  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE InputProcessor,  ONLY: SameString
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT (IN) :: SurfNum             ! Surface number
INTEGER, INTENT (IN) :: iter                ! Iteration number for glass heat balance calculation
REAL(r64), INTENT (OUT)   :: VGap                ! Air velocity in airflow gap (m/s)
REAL(r64), INTENT (INOUT) :: TGapNew             ! Current-iteration average air temp in airflow gap (K)
REAL(r64), INTENT (INOUT) :: TGapOutlet          ! Temperature of air leaving glass-shade/blind gap at top for upward
                                            !   air flow or bottom for downward air flow (K)
REAL(r64), INTENT (INOUT) :: QConvGap            ! Convective heat gain from air flow gap (W)
REAL(r64), INTENT (OUT)   :: hcv                 ! Convection coefficient from gap glass faces to gap air (W/m2-K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER              :: ConstrNum           ! Construction number of surface
INTEGER              :: NGlass              ! Number of glass layers in construction
INTEGER              :: GapNum              ! Number of airflow gap
REAL(r64)            :: TGapInlet           ! Temperature of air entering glass-shade/blind gap at bottom for upward
                                            !   air flow or top for downward air flow (K)
REAL(r64)     :: TGlassFace1         ! Temperature of left-hand glass surface facing airflow gap (K)
REAL(r64)     :: TGlassFace2         ! Temperature of right-hand glass surface facing airflow gap (K)
REAL(r64)            :: hGapStill           ! Still-air gap conduction/convection coeff (W/m2-K)
REAL(r64)            :: TGapOld             ! Previous-iteration average air temp in airflow gap (K)
REAL(r64)            :: GapHeight           ! Vertical length of airflow gap (m)
REAL(r64)            :: GapDepth            ! Thickness of airflow gap (m)
REAL(r64)            :: RhoAir              ! Density of airflow gap air at a temperature of TGapOld (kg/m3)
REAL(r64)            :: AGap                ! Cross sectional area of airflow gap (m2); for vertical window, this
                                            !   is in horizontal plane normal to window.
REAL(r64)            :: GapHeightChar       ! Characteristic height of the airflow gap air temperature profile (m)
REAL(r64)            :: TAve                ! Average of TGlassFace1 and TGlassFace2 (K)
!REAL(r64)            :: AirProps(8)         ! Air properties
REAL(r64)     :: con                 ! Gap conductivity and derivative
REAL(r64)     :: gr                  ! Gap air Grashof number
REAL(r64)     :: pr                  ! Gap air Prandtl number
REAL(r64)     :: nu                  ! Gap air Nusselt number

! Air properties
!               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
!DATA AirProps / 1.29, -0.4e-2, 2.41e-2, 7.6e-5, 1.73e-5, 1.0e-7, 0.72,   1.8e-3  /

ConstrNum = Surface(SurfNum)%Construction
NGlass = Construct(ConstrNum)%TotGlassLayers
TGlassFace1 = thetas(2*NGlass-2)
TGlassFace2 = thetas(2*NGlass-1)
GapNum = NGlass - 1
TAve = 0.5d0*(TGlassFace1 + TGlassFace2)

IF(SurfaceWindow(SurfNum)%AirFlowSource == AirFlowWindow_Source_IndoorAir) THEN
  TGapInlet = tin        ! Source is inside air
ELSE
  TGapInlet = tout       ! Source is outside air
END IF

IF(iter == 0) THEN
  TGapOld = 0.5d0*(TAve + TGapInlet)
ELSE
  TGapOld = TGapNew
END IF

    ! Conductance of gap assuming it is sealed
CALL WindowGasConductance(TGlassFace1,TGlassFace2,GapNum,con,pr,gr)
CALL NusseltNumber(SurfNum,TGlassFace1,TGlassFace2,GapNum,gr,pr,nu)
hGapStill = con/gap(GapNum)*nu
GapHeight = Surface(SurfNum)%Height
GapDepth = Material(Construct(ConstrNum)%LayerPoint(2*NGlass-2))%Thickness
AGap = GapDepth * Surface(SurfNum)%Width
VGap = SurfaceWindow(SurfNum)%AirFlowThisTS/GapDepth
hcv = 2.d0*hGapStill + 4.d0*VGap
RhoAir  = AirProps(1) + AirProps(2)*(TGapOld-TKelvin)
GapHeightChar = RhoAir * 1008.d0 * GapDepth * VGap/(2.d0*hcv)
    ! The following avoids divide by zero and exponential underflow
IF(GapHeightChar == 0.0d0) THEN
  TGapOutlet = TAve
ELSE IF((GapHeight/GapHeightChar) > 15.d0) THEN
  TGapOutlet = TAve
ELSE
  TGapOutlet = TAve - (TAve-TGapInlet)*EXP(-GapHeight/GapHeightChar)
END IF
TGapNew = TAve - (GapHeightChar/GapHeight) * (TGapOutlet-TGapInlet)
    ! Convective heat flow from gap [W]
RhoAir = AirProps(1) + AirProps(2)*(TGapNew-TKelvin)
QConvGap = RhoAir * AGap * VGap * 1008.d0 * (TGapOutlet - TGapInlet)

RETURN
END SUBROUTINE BetweenGlassForcedFlow

!****************************************************************************

SUBROUTINE BetweenGlassShadeForcedFlow(SurfNum,iter,VGap,TGapNew,TGapOutletAve,hcv,QConvTot)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   February 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by SolveForWindowTemperatures for airflow windows with a
          ! between-glass shade or blind over which fan-forced air flows.
          ! Based on the air flow velocity (which is assumed to be the same in the
          ! gaps on either side of the shade/blind), finds, for each gap: the average
          ! air temperature, the shade/blind or glass surface to air convective heat
          ! transfer coefficient, the gap outlet temperature, and the outlet convective heat flow.

          ! Called only for double and triple glazing. For triple glazing the airflow
          ! is assumed to be between the inner two layers of glass (glass layers 2 and 3),
          ! between which the shade/blind is located.

          ! METHODOLOGY EMPLOYED:
          ! Based on ISO/DIS 15099, "Thermal Performance of Windows, Doors and Shading Devices --
          ! Detailed Calculations," 1/12/2000, Chapter 7, "Shading Devices."

          ! REFERENCES:
          ! na

  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT (IN) :: SurfNum             ! Surface number
INTEGER, INTENT (IN) :: iter                ! Iteration number for glass heat balance calculation
REAL(r64), INTENT (INOUT) :: VGap                ! Air velocity in each gap (m/s)
REAL(r64), INTENT (INOUT) :: TGapNew(2)          ! Current-iteration average gas temp in gaps (K)
REAL(r64)            :: TGapOutletAve       ! Average of TGapOutlet(1) and TGapOutlet(2) (K)
REAL(r64), INTENT (INOUT) :: hcv(2)              ! Convection coefficient from gap glass or shade to gap gas (W/m2-K)
REAL(r64), INTENT (INOUT) :: QConvTot            ! Sum of convective heat flow from gaps (W)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER              :: ConstrNumSh         ! Shaded construction number
INTEGER              :: MatNumSh            ! Material number of shade/blind layer
                                            ! In the following, "gaps" refer to the gaps on either side of the shade/blind
REAL(r64)     :: TGlassFace(2)       ! Temperature of glass surfaces facing gaps (K)
REAL(r64)     :: TShadeFace(2)       ! Temperature of shade surfaces facing gaps (K)
REAL(r64)            :: hGapStill(2)        ! Still-air conduction/convection coeffs for the gaps (W/m2-K)
REAL(r64)            :: TGapOld(2)          ! Previous-iteration average gas temp in gaps (K)
REAL(r64)            :: GapHeight           ! Vertical length of glass-shade/blind gap (m)
REAL(r64)            :: GapDepth            ! Distance from shade/blind to glass; assumed same for both gaps (m)
REAL(r64)            :: RhoAir(2)           ! Density of gap air (kg/m3)
REAL(r64)            :: AGap                ! Cross sectional area of each gap (m2); for vertical window, this
                                            !   is in horizontal plane normal to window.
REAL(r64)            :: TGapInlet           ! Gap inlet air temperature (K)
REAL(r64)            :: TGapOutlet(2)       ! Gap outlet air temperature (K)
REAL(r64)            :: QConvGap(2)         ! Convective heat flow from each gap (W)
REAL(r64)            :: GapHeightChar(2)    ! Characteristic height of the gap air temperature profile (m)
REAL(r64)            :: TAve(2)             ! Average of TGlass and TShade for the gaps (K)
REAL(r64)     :: con                 ! Gap air conductivity and derivative
REAL(r64)     :: gr                  ! Gap air Grashof number
REAL(r64)     :: pr                  ! Gap air Prandtl number
REAL(r64)     :: nu                  ! Gap air Nusselt number
INTEGER              :: ShadeFlag           ! Shading flag
INTEGER              :: IGap                ! Gap counter; 1 = gap on outer side of shade/blind, 2 = gap on inner side.
INTEGER              :: IGapInc             ! Gap increment; =0, double glass, =1, triple glass
!REAL(r64)            :: AirProps(8)         ! Air properties

! Air properties
!               Dens  dDens/dT  Con    dCon/dT   Vis    dVis/dT Prandtl dPrandtl/dT
!DATA AirProps / 1.29, -0.4e-2, 2.41e-2, 7.6e-5, 1.73e-5, 1.0e-7, 0.72,   1.8e-3  /

ConstrNumSh = Surface(SurfNum)%ShadedConstruction
ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag

IF(Construct(ConstrNumSh)%TotGlassLayers == 2) THEN  ! Double glazing
  MatNumSh = Construct(ConstrNumSh)%LayerPoint(3)
  IGapInc = 0
  DO IGap = 1,2
    TGlassFace(IGap) = thetas(IGap+1)
    TShadeFace(IGap) = thetas(IGap+4)
  END DO
ELSE                                                 ! Triple glazing
  MatNumSh = Construct(ConstrNumSh)%LayerPoint(5)
  IGapInc = 1
  DO IGap = 1,2
    TGlassFace(IGap) = thetas(IGap+3)
    TShadeFace(IGap) = thetas(IGap+6)
  END DO
END IF

IF (SurfaceWindow(SurfNum)%AirFlowSource == AirFlowWindow_Source_IndoorAir) THEN
  TGapInlet = tin
ELSE
  TGapInlet = tout
END IF

GapHeight = Surface(SurfNum)%Height
GapDepth = gap(1+IGapInc)
AGap = GapDepth * Surface(SurfNum)%Width
          ! Factor of 2 below assumes gaps on either side of shade/blind have same depth
VGap = SurfaceWindow(SurfNum)%AirFlowThisTS/(2.d0*GapDepth)

DO IGap = 1,2
  TAve(IGap) = 0.5d0*(TGlassFace(IGap) + TShadeFace(IGap))
  IF(iter == 0) THEN
    TGapOld(IGap) = TAve(IGap)
  ELSE
    TGapOld(IGap) = TGapNew(IGap)
  END IF
          ! Conductance of gaps on either side of shade/blind assuming gaps are sealed
  CALL WindowGasConductance(TGlassFace(IGap),TShadeFace(IGap),IGap+IGapInc,con,pr,gr)
  CALL NusseltNumber(SurfNum,TGlassFace(IGap),TShadeFace(IGap),IGap+IGapInc,gr,pr,nu)
  hGapStill(IGap) = con/gap(IGap+IGapInc)*nu
          ! Shade/blind or glass surface to air convection coefficient
  hcv(IGap) = 2.d0*hGapStill(IGap) + 4.d0*VGap
  RhoAir(IGap) = AirProps(1) + AirProps(2)*(TGapOld(IGap)-TKelvin)
  hcv(IGap) = 2.d0*hGapStill(IGap) + 4.d0*VGap
  GapHeightChar(IGap) = RhoAir(IGap) * 1008.d0 * GapDepth * VGap/(2.d0*hcv(IGap))
          ! The following avoids divide by zero and exponential underflow
  IF(GapHeightChar(IGap) == 0.0d0) THEN
    TGapOutlet(IGap) = TAve(IGap)
  ELSE IF((GapHeight/GapHeightChar(IGap)) > 15.d0) THEN
    TGapOutlet(IGap) = TAve(IGap)
  ELSE
    TGapOutlet(IGap) = TAve(IGap) - (TAve(IGap)-TGapInlet)*EXP(-GapHeight/GapHeightChar(IGap))
  END IF
  TGapNew(IGap) = TAve(IGap) - (GapHeightChar(IGap)/GapHeight) * (TGapOutlet(IGap)-TGapInlet)
    ! Convective heat flow from gap [W]
  RhoAir(IGap) = AirProps(1) + AirProps(2)*(TGapNew(IGap)-TKelvin)
  QConvGap(IGap) = RhoAir(IGap) * AGap * VGap * 1008.d0 * (TGapOutlet(IGap) - TGapInlet)
END DO

QConvTot = QConvGap(1) + QConvGap(2)
TGapOutletAve = 0.5d0*(TGapOutlet(1)+TGapOutlet(2))

RETURN
END SUBROUTINE BetweenGlassShadeForcedFlow

!****************************************************************************

SUBROUTINE LUdecomposition(ajac,n,indx,d)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann, adapted from Numerical Recipes
          !       DATE WRITTEN   February 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Performs LU decomposition of a matrix.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)  :: ajac(10,10)                  ! As input: matrix to be decomposed;
                                                    !   as output: decomposed matrix
  INTEGER           :: n                            ! Dimension of matrix
  INTEGER           :: indx(10)                     ! Vector of row permutations
  REAL(r64)         :: d                            ! +1 if even number of row interchange is even, -1
                                                    !   if odd

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: i,j,k                        ! Counters
  INTEGER           :: imax                         ! Temporary variable
                                                    !   as output: decomposed matrix
  REAL(r64)  :: vv(500)                      ! Stores the implicit scaling of each row
  REAL(r64)  :: aamax                        ! Absolute value of largest element of matrix
  REAL(r64)  :: dum                          ! Temporary variable
  REAL(r64)  :: sum                          ! Sum of products of matrix elements

           ! FLOW

      d=1.0d0
      do i=1,n
        aamax=0.0d0
        do j=1,n
          if (abs(ajac(i,j)).gt.aamax) aamax=abs(ajac(i,j))
        end do
          if (aamax.eq.0.0d0) CALL ShowFatalError('Singular matrix in LUdecomposition, window calculations')
          vv(i)=1.0d0/aamax
      end do
      do j=1,n
        do i=1,j-1
          sum=ajac(i,j)
          do k=1,i-1
            sum=sum-ajac(i,k)*ajac(k,j)
          end do
          ajac(i,j)=sum
        end do
        aamax=0.0d0
        do i=j,n

          sum=ajac(i,j)
          do k=1,j-1
            sum=sum-ajac(i,k)*ajac(k,j)
          end do
          ajac(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
        end do
        if (j.ne.imax)then
          do k=1,n
            dum=ajac(imax,k)
            ajac(imax,k)=ajac(j,k)
            ajac(j,k)=dum
          end do
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(ajac(j,j).eq.0.0d0) ajac(j,j)=rTinyValue
        if(j.ne.n)then
          dum=1.0d0/ajac(j,j)

          do i=j+1,n
            ajac(i,j)=ajac(i,j)*dum
          end do
        endif
      end do
      return
END SUBROUTINE LUdecomposition

!**************************************************************************
SUBROUTINE LUsolution(a,n,indx,b)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann, adapted from Numerical Recipes
          !       DATE WRITTEN   February 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Solves set of linear equations a.x = b

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)  :: a(10,10)                     ! Matrix and vector in a.x = b;
                                                    !   b is also output as the solution, x
  INTEGER           :: n                            ! Dimension of a and b
  INTEGER           :: indx(10)                     ! Vector of row permutations
  REAL(r64)  :: b(10)                        ! Matrix and vector in a.x = b;
                                                    !   b is also output as the solution, x

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: i,j                          ! Counters
  INTEGER           :: ii,ll                        ! Intermediate variables
  REAL(r64)  :: sum                          ! Summation variable

          ! FLOW

      ii=0
      do i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do j=ii,i-1
            sum=sum-a(i,j)*b(j)
          end do
        else if (sum.ne.0.0d0) then
          ii=i
        endif
        b(i)=sum
      end do
      do i=n,1,-1
        sum=b(i)
        do j=i+1,n
          sum=sum-a(i,j)*b(j)
        end do
        b(i)=sum/a(i,i)
      end do
      return
END SUBROUTINE LUsolution
!******************************************************************************

SUBROUTINE WindowGasConductance(tleft,tright,IGap,con,pr,gr)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by Fred Winkelmann from Window5 subroutine gasses
          !       DATE WRITTEN   September 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the coefficient of convective/conductive heat transfer in the gas-filled gap
          ! between isothermal solid layers. The gap may be filled with a single gas or a gas mixture.

          ! METHODOLOGY EMPLOYED:
          ! Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
          ! "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
          ! The equation numbers below correspond to those in the standard.

          ! REFERENCES:
          ! Window5 source code; ISO 15099

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)         :: tleft             ! Temperature of gap surface closest to outside (K)
  REAL(r64), INTENT(IN)         :: tright            ! Temperature of gap surface closest to zone (K)
  INTEGER, INTENT(IN)                  :: IGap              ! Gap number
  REAL(r64)  , INTENT(OUT)      :: con               ! Gap gas conductance (W/m2-K)
  REAL(r64)  , INTENT(OUT)      :: pr                ! Gap gas Prandtl number
  REAL(r64)  , INTENT(OUT)      :: gr                ! Gap gas Grashof number


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER          :: pres = 1.0d5      ! Gap gas pressure (Pa)
  REAL(r64), PARAMETER          :: gaslaw = 8314.51d0  ! Molar gas constant (J/kMol-K)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: tmean             ! Average gap gas temperature (K)
  INTEGER                  :: IMix,i,j          ! Counters of gases in a mixture
  INTEGER                  :: NMix              ! Number of gases in a mixture
  REAL(r64)                :: molmix            ! Molecular weight of mixture
  REAL(r64)                :: kprime(10)        ! Monotonic thermal conductivity
  REAL(r64)                :: kdblprm(10)       ! Conductivity term accounting for additional energy moved by
                                                !  the diffusional transport of internal energy in polyatomic gases.
  REAL(r64)                :: kpmix,kdpmix      ! Monotonic thermal conductivity of mixture
  REAL(r64)                :: mukpdwn(10)       ! Denominator term
  REAL(r64)                :: kpdown(10),kdpdown(10) ! Denominator terms
  REAL(r64)                :: kmix              ! For accumulating conductance of gas mixture
  REAL(r64)                :: mumix             ! For accumulating viscosity of gas mixture
  REAL(r64)                :: visc              ! Dynamic viscosity of mixture at tmean (g/m-s)
  REAL(r64)                :: cp                ! Specific heat of mixture at tmean (J/m3-K)
  REAL(r64)                :: dens              ! Density of mixture at tmean (kg/m3)
  REAL(r64)                :: cpmixm            ! Gives cp when divided by molmix
  REAL(r64)                :: phimup            ! Numerator factor
  REAL(r64)                :: downer            ! Denominator factor
  REAL(r64)                :: psiup             ! Numerator factor
  REAL(r64)                :: psiterm           ! Factor
  REAL(r64)                :: phikup            ! Numerator factor
  REAL(r64)                :: rhomix            ! Density of gas mixture (kg/m3)
  REAL(r64)                :: frct(10)          ! Fraction of each gas in a mixture
  REAL(r64)                :: fvis(10)          ! Viscosity of each gas in a mixture (g/m-s)
  REAL(r64)                :: fcon(10)          ! Conductance of each gas in a mixture (W/m2-K)
  REAL(r64)                :: fdens(10)         ! Density of each gas in a mixture (kg/m3)
  REAL(r64)                :: fcp(10)           ! Specific heat of each gas in a mixture (J/m3-K)


      NMix = gnmix(IGap) !Objexx:Logic Either assert NMix>0 or handle NMix<=0 in logic so that con and locals guar. initialized before use

      DO IMix = 1,NMix
        frct(IMix) = gfract(IGap,IMix)
      END DO

      tmean = 0.5d0*(tleft+tright)

      fcon(1)  = gcon(IGap,1,1) + gcon(IGap,1,2)*tmean + gcon(IGap,1,3)*tmean**2
      fvis(1)  = gvis(IGap,1,1) + gvis(IGap,1,2)*tmean + gvis(IGap,1,3)*tmean**2
      fcp(1)   = gcp(IGap,1,1)  + gcp(IGap,1,2) *tmean + gcp(IGap,1,3) *tmean**2
      fdens(1) = pres*gwght(IGap,1)/(gaslaw*tmean)       ! Density using ideal gas law:
                                                         !  rho=(presure*molecweight)/(gasconst*tmean)

      IF(NMix == 1) then          ! Single gas
        con  =fcon(1)
        visc =fvis(1)
        cp   =fcp(1)
        dens =fdens(1)
      elseif (NMix >1) then                        ! Multiple gases; calculate mixture properties
        molmix = frct(1)*gwght(IGap,1)                   ! initialize eq. 56
        cpmixm = molmix*fcp(1)                           ! initialize eq. 58
        kprime(1)  = 3.75d0*(gaslaw/gwght(IGap,1))*fvis(1) ! eq. 67
        kdblprm(1) = fcon(1)-kprime(1)                   ! eq. 67

        ! Initialize summations for eqns 60-66
        mumix      = 0.0d0
        kpmix      = 0.0d0
        kdpmix     = 0.0d0
        mukpdwn(1) = 1.0d0
        kpdown(1)  = 1.0d0
        kdpdown(1) = 1.0d0

        ! Calculate properties of mixture constituents
        do i = 2, NMix
          fcon(i)  = gcon(IGap,i,1) + gcon(IGap,i,2)*tmean + gcon(IGap,i,3)*tmean**2
          fvis(i)  = gvis(IGap,i,1) + gvis(IGap,i,2)*tmean + gvis(IGap,i,3)*tmean**2
          fcp(i)   = gcp(IGap,i,1)  + gcp(IGap,i,2)*tmean +  gcp(IGap,i,3) *tmean**2
          fdens(i) = pres*gwght(IGap,i)/(gaslaw*tmean)
          molmix   = molmix+frct(i)*gwght(IGap,i)          ! eq. 56
          cpmixm   = cpmixm+frct(i)*fcp(i)*gwght(IGap,i)   ! eq. 58-59
          kprime(i)  = 3.75d0*gaslaw/gwght(IGap,i)*fvis(i) ! eq. 67
          kdblprm(i) = fcon(i)-kprime(i)                   ! eq. 68
          mukpdwn(i) = 1.0d0                               ! initialize denomonator of eq. 60
          kpdown(i)  = 1.0d0                               ! initialize denomonator of eq. 63
          kdpdown(i) = 1.0d0                               ! initialize denomonator of eq. 65
        end do

        do i = 1,NMix
          do j = 1,NMix
                        ! numerator of equation 61
            phimup = (1.0d0 + (fvis(i)/fvis(j))**0.5d0*(gwght(IGap,j)/gwght(IGap,i))**0.25d0)**2
                        ! denomonator of eq. 61, 64 and 66
            downer = 2.d0 * sqrt(2.d0) * (1+(gwght(IGap,i)/gwght(IGap,j)))**0.5d0
                        ! calculate the denominator of eq. 60
            if (i /= j) mukpdwn(i) = mukpdwn(i) + phimup/downer*frct(j)/frct(i)
                        ! numerator of eq. 64; psiterm is the multiplied term in backets
            psiup   = (1.d0 + (kprime(i)/kprime(j))**0.5d0*(gwght(IGap,i)/gwght(IGap,j))**0.25d0)**2
            psiterm = 1.d0 + 2.41d0*(gwght(IGap,i)-gwght(IGap,j))*(gwght(IGap,i)-0.142d0*gwght(IGap,j))  &
                           /(gwght(IGap,i) + gwght(IGap,j))**2
                        ! using the common denominator, downer, calculate the denominator for eq. 63
            if (i.ne.j) kpdown(i) = kpdown(i) + psiup*(psiterm/downer)*(frct(j)/frct(i))
                        ! calculate the numerator of eq. 66
            phikup = (1.d0+(kprime(i)/kprime(j))**0.5d0 * (gwght(IGap,i)/gwght(IGap,j))**0.25d0)**2
                        ! using the common denominator, downer, calculate the denomonator for eq. 65
            if (i.ne.j) kdpdown(i) = kdpdown(i) + (phikup/downer)*(frct(j)/frct(i))
          end do
          mumix = mumix + fvis(i)/mukpdwn(i)              ! eq. 60
          kpmix = kpmix + kprime(i)/kpdown(i)             ! eq. 63
          kdpmix = kdpmix + kdblprm(i)/kdpdown(i)         ! eq. 65
        end do

        ! Calculate the density of the mixture assuming an ideal gas
        rhomix = pres * molmix / (gaslaw * tmean)         ! eq. 57
        kmix = kpmix + kdpmix                             ! eq. 68-a

        ! Final mixture properties
        visc=mumix
        con=kmix
        dens=rhomix
        cp=cpmixm/molmix

      endif  ! End of check if single or multiple gases in gap

      pr = cp * visc / con
      gr = 9.807d0 * gap(IGap)**3 * ABS(tleft-tright) * dens**2 / (tmean * visc**2)

      RETURN

END SUBROUTINE WindowGasConductance

!******************************************************************************

SUBROUTINE WindowGasPropertiesAtTemp(tmean,IGap,dens,visc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   December 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Finds the density and viscosity of the gas in a gap at a particular temperature.
          ! The gap may be filled with a single gas or a gas mixture.
          ! Based on Subroutine WindowGasConductance.

          ! METHODOLOGY EMPLOYED:
          ! See Subr. WindowGasConductance

          ! REFERENCES:
          ! See Subr. WindowGasConductance

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)         :: tmean             ! Temperature of gas in gap (K)
  INTEGER, INTENT(IN)      :: IGap              ! Gap number
  REAL(r64), INTENT(OUT)        :: dens              ! Gap gas density at tmean (kg/m3)
  REAL(r64), INTENT(OUT)        :: visc              ! Gap gas dynamic viscosity at tmean (g/m-s)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER          :: pres = 1.0d5      ! Gap gas pressure (Pa)
  REAL(r64), PARAMETER          :: gaslaw = 8314.51d0  ! Molar gas constant (J/kMol-K)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: IMix,i,j          ! Counters of gases in a mixture
  INTEGER                  :: NMix              ! Number of gases in a mixture
  REAL(r64)                :: molmix            ! Molecular weight of mixture
  REAL(r64)                :: mukpdwn(10)       ! Denominator term
  REAL(r64)                :: mumix             ! For accumulating viscosity of gas mixture
  REAL(r64)                :: phimup            ! Numerator factor
  REAL(r64)                :: downer            ! Denominator factor
  REAL(r64)                :: rhomix            ! Density of gas mixture (kg/m3)
  REAL(r64)                :: frct(10)          ! Fraction of each gas in a mixture
  REAL(r64)                :: fvis(10)          ! Viscosity of each gas in a mixture (g/m-s)
  REAL(r64)                :: fdens(10)         ! Density of each gas in a mixture (kg/m3)

      NMix = gnmix(IGap)

      DO IMix = 1,NMix
        frct(IMix) = gfract(IGap,IMix)
      END DO

      fvis(1)  = gvis(IGap,1,1) + gvis(IGap,1,2)*tmean + gvis(IGap,1,3)*tmean**2
      fdens(1) = pres*gwght(IGap,1)/(gaslaw*tmean)       ! Density using ideal gas law:
                                                         !  rho=(presure*molecweight)/(gasconst*tmean)
      IF(NMix == 1) then          ! Single gas
        visc =fvis(1)
        dens =fdens(1)
      else                        ! Multiple gases; calculate mixture properties
        molmix = frct(1)*gwght(IGap,1)                   ! initialize eq. 56

        ! Initialize summations for eqns 60-66
        mumix      = 0.0d0
        mukpdwn(1) = 1.0d0

        ! Calculate properties of mixture constituents
        do i = 2, NMix
          fvis(i)  = gvis(IGap,i,1) + gvis(IGap,i,2)*tmean + gvis(IGap,i,3)*tmean**2
          fdens(i) = pres*gwght(IGap,i)/(gaslaw*tmean)
          molmix   = molmix+frct(i)*gwght(IGap,i)         ! eq. 56
          mukpdwn(i) = 1.0d0                              ! initialize denomonator of eq. 60
        end do

        do i = 1,NMix
          do j = 1,NMix
                        ! numerator of equation 61
            phimup = (1.0d0 + (fvis(i)/fvis(j))**0.5d0*(gwght(IGap,j)/gwght(IGap,i))**0.25d0)**2
                        ! denomonator of eq. 61, 64 and 66
            downer = 2.d0 * sqrt(2.d0) * (1+(gwght(IGap,i)/gwght(IGap,j)))**0.5d0
                        ! calculate the denominator of eq. 60
            if (i /= j) mukpdwn(i) = mukpdwn(i) + phimup/downer*frct(j)/frct(i)
          end do
          mumix = mumix + fvis(i)/mukpdwn(i)              ! eq. 60
        end do

        ! Calculate the density of the mixture assuming an ideal gas
        rhomix = pres * molmix / (gaslaw * tmean)         ! eq. 57

        ! Final mixture properties
        visc=mumix
        dens=rhomix

      endif  ! End of check if single or multiple gases in gap

      RETURN
END SUBROUTINE WindowGasPropertiesAtTemp

!********************************************************************************
SUBROUTINE StartingWindowTemps(SurfNum,AbsRadShade)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   January 2000
          !       MODIFIED       March 2003, FW: add rough calc of increase above ambient of
          !                        initial shade/blind temperature when shade/blind deployed
          !                        after having been off.
          !                      Jan 2004, FW: take into account whether storm window was added
          !                        or removed in the current time step.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes face temperature distribution prior to iteration

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER           :: SurfNum           ! Surface number
REAL(r64)         :: AbsRadShade(2)    ! Short-wave radiation absorbed by shade/blind faces

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64),PARAMETER  :: hrad = 5.3d0          ! Typical radiative conductance (W/m2-K)
REAL(r64),PARAMETER  :: resgap = 0.21d0       ! Typical gap resistance (m2-K/W)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER         :: i                   ! Face counter
INTEGER         :: ShadeFlag           ! Shading flag
REAL(r64)       :: rguess(11)          ! Combined radiative/convective resistance (m2-K/W) of
                                       ! inside or outside air film, or gap
REAL(r64)       :: restot              ! Total window resistance including outside
                                       !   and inside air films (m2-K/W)
REAL(r64)       :: temdiff             ! Inside/outside air temperature difference (K)
REAL(r64)       :: ressum              ! Resistance sum (m2-K/W)
INTEGER         :: StormWinFlagPrevDay ! Previous time step value (day) of storm window flag
INTEGER         :: StormWinFlagThisDay ! Current time step value (day) of storm window flag
INTEGER         :: nglfacePrevDay      ! Previous time step value (dya) of number of glass faces (may differ
                                       !   current time step value, nglface, if storm window was
                                       !   added or removed during the current time step).

    StormWinFlagPrevDay = SurfaceWindow(SurfNum)%StormWinFlagPrevDay
    StormWinFlagThisDay = SurfaceWindow(SurfNum)%StormWinFlag

    IF(BeginEnvrnFlag .OR. (StormWinFlagThisDay /= StormWinFlagPrevDay)) THEN

      ! Guess values of glass face temperatures based on a simple resistance-network solution
      ! that (1) ignores short- and long-wave radiation (from lights and zone equipment) absorbed
      ! by the glass faces, and (2) assumes zero glass resistance. Absorbed solar is also ignored
      ! since the tests on BeginEnvrnFlag and storm window transition can be true only at midnight.
      ! Interaction with shade or blind, if one of these is present, is ignored. See below for
      ! separate calculation of shade/blind temperature.

      rguess(1) = 1.0d0/(hcout+hrad)
      rguess(nglface+1) = 1.0d0/(hcin+hrad)

      do i = 2,nglface,2
        rguess(i) = 1.0d0/scon(i/2)
        if(i<nglface) rguess(i+1) = resgap
      end do

      restot = 0.0d0
      do i = 1,nglface+1
        restot = restot + rguess(i)
      enddo

      temdiff = tin - tout
      if(abs(temdiff)<0.5d0) temdiff = 2.0d0

      ressum = 0.0d0
      do i = 1,nglface
        ressum = ressum + rguess(i)
        thetas(i) = (ressum/restot)*temdiff + tout
      end do

    ELSE
    ! Use previous time step values
      do i = 1,nglface
        thetas(i)   = SurfaceWindow(SurfNum)%ThetaFace(i)
      end do

    END IF

    ! Initialize face temperatures of shade or blind, if present

    ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
    if(SurfaceWindow(SurfNum)%ExtIntShadePrevTS==IntShadeOn.OR. &
       SurfaceWindow(SurfNum)%ExtIntShadePrevTS==IntBlindOn.OR. &
       SurfaceWindow(SurfNum)%ExtIntShadePrevTS==ExtShadeOn.OR. &
       SurfaceWindow(SurfNum)%ExtIntShadePrevTS==ExtBlindOn.OR. &
       SurfaceWindow(SurfNum)%ExtIntShadePrevTS==BGShadeOn .OR. &
       SurfaceWindow(SurfNum)%ExtIntShadePrevTS==BGBlindOn) then
          ! Shade or blind is on during the previous TS; use previous-TS values of shade/blind face temps.
          ! Note that if shade or blind is NOT on in the current TS the following two
          ! temperature values, although calculated here, are not used. The shade/blind face numbers
          ! during the previous time step depend on whether a storm window glass layer was added to
          ! or removed from the window during the current time step.
      nglfacePrevDay = nglface
      IF(StormWinFlagPrevDay == 0 .AND. StormWinFlagThisDay == 1) nglfacePrevDay = nglface - 2
      IF(StormWinFlagPrevDay == 1 .AND. StormWinFlagThisDay == 0) nglfacePrevDay = nglface + 2
      thetas(nglface+1) = SurfaceWindow(SurfNum)%ThetaFace(nglfacePrevDay+1)
      thetas(nglface+2) = SurfaceWindow(SurfNum)%ThetaFace(nglfacePrevDay+2)
    else
          ! No shade or blind previous time step; guess starting values of shade/blind
          ! taking into account short- and long-wave radiation (from solar, lights and zone equipment)
          ! absorbed by shade/blind faces. Face temps are assumed to be the same and
          ! equal to shade/blind temp. For interior shade/blind, air temp on either side is
          ! assumed to be the same and equal to tin; for exterior blind it is assumed to be
          ! equal to tout. For between-glass shade/blind it is assumed to be equal to the
          ! average temperature of the adjacent glass faces.

      if(ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn) then
        thetas(nglface+1) = tin + (AbsRadShade(1)+AbsRadShade(2))/(2*(hcin+hrad))
        thetas(nglface+2) = thetas(nglface+1)
      end if
      if(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn) then
        thetas(nglface+1) = tout + (AbsRadShade(1)+AbsRadShade(2))/(2*(hcout+hrad))
        thetas(nglface+2) = thetas(nglface+1)
      end if
      if(ShadeFlag == BGShadeOn .OR. ShadeFlag == BGBlindOn) then
          ! Between-glass shade/blind allowed only for double and triple glazing.
          ! The factor 16.0 below is based on a combined convective/radiative heat transfer
          ! coefficient on either side of the shade/blind of 8.0 W/m2-K -- about 1.4 Btu/h-ft2-F.
        if(nglface==4) then  ! double glazing
          thetas(nglface+1) = 0.5d0*(thetas(2)+thetas(3)) + (AbsRadShade(1)+AbsRadShade(2))/16.0d0
          thetas(nglface+2) = thetas(nglface+1)
        else                 ! triple glazing
          thetas(nglface+1) = 0.5d0*(thetas(4)+thetas(5)) + (AbsRadShade(1)+AbsRadShade(2))/16.0d0
          thetas(nglface+2) = thetas(nglface+1)
        end if
      end if
    end if

   return
END SUBROUTINE StartingWindowTemps
   !****************************************************************************

SUBROUTINE NusseltNumber (SurfNum,tso,tsi,IGap,gr,pr,gnu)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Adapted by Fred Winkelmann from Window5 subroutine nusselt
          !       DATE WRITTEN   September 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Finds the Nusselt number for gas-filled gaps between isothermal solid layers.
          ! The gap may be filled with a single gas or a gas mixture.

          ! METHODOLOGY EMPLOYED:
          ! Based on methodology in Chapter 5 of the July 18, 2001 draft of ISO 15099,
          ! "Thermal Performance of Windows, Doors and Shading Devices--Detailed Calculations."
          ! The equation numbers below correspond to those in the standard.

          ! REFERENCES:
          ! Window5 source code; ISO 15099

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)         :: tso               ! Temperature of gap surface closest to outside (K)
  REAL(r64), INTENT(IN)         :: tsi               ! Temperature of gap surface closest to zone (K)
  INTEGER, INTENT(IN)                  :: SurfNum           ! Surface number
  INTEGER, INTENT(IN)                  :: IGap              ! Gap number
  REAL(r64), INTENT(IN)         :: pr                ! Gap gas Prandtl number
  REAL(r64), INTENT(IN)         :: gr                ! Gap gas Grashof number
  REAL(r64), INTENT(OUT)        :: gnu               ! Gap gas Nusselt number

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: asp                            ! Aspect ratio: window height to gap width
  REAL(r64)  :: ra                             ! Rayleigh number
  REAL(r64)  :: gnu901,gnu902,gnu90,gnu601     ! Nusselt number temporary variables for
  REAL(r64)  :: gnu602,gnu60,gnu601a,gnua,gnub !  different tilt and Ra ranges
  REAL(r64)  :: cra,a,b,g,ang                  ! Temporary variables


      IF(SurfNum > 0) THEN
        asp = Surface(SurfNum)%Height/gap(IGap)
      ELSE  ! SurfNum = 0 when NusseltNumber is called from CalcNominalWindowCond, which applies to a
            ! particular construction. So window height is not known and we assume 5 ft (1.524 m)
        asp = 1.524d0/gap(IGap)
      END IF

      tiltr = tilt * DegToRadians
      ra = gr*pr
                             !!fw if (ra > 2.0e6): error that outside range of Rayleigh number?

      if(ra <= 1.0d4)                  gnu901 = 1.d0 + 1.7596678d-10 * ra**2.2984755d0   ! eq. 51
      if(ra > 1.0d4 .and. ra <= 5.0d4) gnu901 =        0.028154d0      * ra**0.4134d0      ! eq. 50
      if(ra > 5.0d4)                   gnu901 =        0.0673838d0     * ra**(1.0d0/3.0d0)   ! eq. 49

      gnu902 = 0.242d0 * (ra/asp)**.272d0               ! eq. 52
      gnu90 = MAX(gnu901,gnu902)

      if(tso > tsi)then   ! window heated from above
        gnu = 1.0d0 + (gnu90-1.0d0)*sin(tiltr)                  ! eq. 53
      else                ! window heated from below
        if (tilt >= 60.0d0) then
          if (ra >= .001d0) then
            g       = 0.5d0 * (1.0d0+(ra/3160.d0)**20.6d0)**(-0.1d0)    ! eq. 47
          else
            g       = 0.5d0
          endif
          gnu601a = 1.0d0 + (0.0936d0*(ra**0.314d0)/(1.0d0+g))**7   ! eq. 45
          gnu601  = gnu601a**0.142857d0

          ! For any aspect ratio
          gnu602  = (0.104d0+0.175d0/asp) * ra**0.283d0           ! eq. 46
          gnu60   = MAX(gnu601,gnu602)

          ! linear interpolation for layers inclined at angles between 60 and 90 deg
          gnu     = ((90.0d0-tilt)*gnu60 + (tilt-60.0d0)*gnu90)/30.0d0
        endif
        if (tilt < 60.0d0) then                               ! eq. 42
          cra  = ra*cos(tiltr)
          a    = 1.0d0 - 1708.0d0/cra
          b    = (cra/5830.0d0)**0.33333d0-1.0d0
          gnua = (abs(a)+a)/2.0d0
          gnub = (abs(b)+b)/2.0d0
          ang  = 1708.0d0 * (sin(1.8d0*tiltr))**1.6d0
          gnu  = 1.0d0 + 1.44d0*gnua*(1.0d0-ang/cra) + gnub
        endif
      endif
      RETURN
END SUBROUTINE NusseltNumber
!*******************************************************************************************************

SUBROUTINE TransAndReflAtPhi(cs,tf0,rf0,rb0,tfp,rfp,rbp,SimpleGlazingSystem, SimpleGlazingSHGC, SimpleGlazingU)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   January 2000
          !       MODIFIED       5 June 2003, FCW: modify to correspond to WINDOW 4 and WINDOW 5.
          !                      Original routine was based on the method in E.U. Finlayson et al,
          !                      "WINDOW 4.0: Documentation of Calculation Procedures," LBL-33943,
          !                      July 1993, which is not used in either WINDOW 4 or WINDOW 5.
          !                      The current routine is based on ASHRAE Handbook of Fundamentals,
          !                      2001, pp. 30.20-23, "Optical Properties of Single Glazing Layers."
          !                      Original routine underpredicted transmittance at angles of
          !                      incidence > 60 degrees.
          !                      June 2009.  Brent Griffith.  add simple window correlation
          !                                   newer model from LBNL windows group 5/15/2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For a single glazing layer, calculate transmittance and reflectance at an arbitrary
          ! angle of incidence given transmittance and reflectance at zero incidence angle.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! ASHRAE Handbook of Fundamentals, 2001, pp. 30.20-23,
          ! "Optical Properties of Single Glazing Layers."

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

    REAL(r64), INTENT(IN)   :: cs                      ! Cosine of incidence angle
    REAL(r64), INTENT(IN)   :: tf0                     ! Transmittance at zero incidence angle
    REAL(r64), INTENT(IN)   :: rf0                     ! Front reflectance at zero incidence angle
    REAL(r64), INTENT(IN)   :: rb0                     ! Back reflectance at zero incidence angle
    REAL(r64), INTENT(OUT)  :: tfp                     ! Transmittance at cs
    REAL(r64), INTENT(OUT)  :: rfp                     ! Front reflectance at cs
    REAL(r64), INTENT(OUT)  :: rbp                     ! Back reflectance at cs
    LOGICAL, INTENT(IN)     :: SimpleGlazingSystem     ! .TRUE. if simple block model being used
    REAL(r64), INTENT(IN)   :: SimpleGlazingSHGC       ! SHGC value to use in alternate model for simple glazing system
    REAL(r64), INTENT(IN)   :: SimpleGlazingU          ! U-factor value to use in alternate model for simple glazing system

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tfp1,tfp2                   ! Transmittance at cs for each polarization
  REAL(r64) :: rfp1,rfp2                   ! Front reflectance at cs for each polarization
  REAL(r64) :: rbp1,rbp2                   ! Back reflectance at cs for each polarization
  REAL(r64) :: betaf,betab                 ! Intermediate variables
  REAL(r64) :: t0f,t0b,r0f,r0b,abf,abb     ! Intermediate variables
  REAL(r64) :: ngf,ngb                     ! Front and back index of refraction
  REAL(r64) :: cgf,cgb                     ! Intermediate variables
  REAL(r64) :: rpf1,rpb1,tpf1,tpb1         ! Front and back air/glass interface reflectivity
                                           !  and transmittivity for first polarization
  REAL(r64) :: rpf2,rpb2,tpf2,tpb2         ! Front and back air/glass interface reflectivity
                                           !  and transmittivity for second polarization
  REAL(r64) :: tcl,rcl                     ! Transmittance and reflectance for clear glass
  REAL(r64) :: tbnz,rbnz                   ! Transmittance and reflectance for bronze glass
  REAL(r64) :: expmabfdivcgf
  REAL(r64) :: expm2abfdivcgf
  REAL(r64) :: expmabbdivcgb
  REAL(r64) :: TransCurveA ! result for curve A for Transmission as a function of angle
  REAL(r64) :: TransCurveB ! result for curve B for Transmission as a function of angle
  REAL(r64) :: TransCurveC ! result for curve C for Transmission as a function of angle
  REAL(r64) :: TransCurveD ! result for curve D for Transmission as a function of angle
  REAL(r64) :: TransCurveE ! result for curve E for Transmission as a function of angle
  REAL(r64) :: TransCurveF ! result for curve F for Transmission as a function of angle
  REAL(r64) :: TransCurveG ! result for curve G for Transmission as a function of angle
  REAL(r64) :: TransCurveH ! result for curve H for Transmission as a function of angle
  REAL(r64) :: TransCurveI ! result for curve I for Transmission as a function of angle
  REAL(r64) :: TransCurveJ ! result for curve J for Transmission as a function of angle
  REAL(r64) :: ReflectCurveA ! result for curve A for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveB ! result for curve B for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveC ! result for curve C for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveD ! result for curve D for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveE ! result for curve E for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveF ! result for curve F for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveG ! result for curve G for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveH ! result for curve H for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveI ! result for curve I for Reflectance as a function of angle
  REAL(r64) :: ReflectCurveJ ! result for curve J for Reflectance as a function of angle

  REAL(r64) :: TransCurveFGHI ! average of curves F, G, H, and I
  REAL(r64) :: ReflectCurveFGHI ! average of curves F, G, H, and I
  REAL(r64) :: TransCurveFH ! average of curves F and H
  REAL(r64) :: ReflectCurveFH ! average of curves F and H
  REAL(r64) :: TransCurveBDCD ! average of curves B, D, C, and D (again)
  REAL(r64) :: ReflectCurveBDCD ! average of curves B, D, C, and D (again)
  REAL(r64) :: TransTmp ! temporary value for normalized transmission (carry out of if blocks)
  REAL(r64) :: ReflectTmp ! temporary value for normalized reflectance (carry out of if blocks)
  real(r64) :: testval    ! temporary value for calculations
  real(r64) :: tmp1       ! temporary value for calculations
  real(r64) :: tmp2       ! temporary value for calculations
  real(r64) :: tmp3       ! temporary value for calculations
  real(r64) :: tmp4       ! temporary value for calculations
  real(r64) :: tmp5       ! temporary value for calculations
  real(r64) :: tmp6       ! temporary value for calculations
  real(r64) :: tmp7       ! temporary value for calculations
  real(r64) :: tmp8       ! temporary value for calculations
  real(r64) :: tmp9       ! temporary value for calculations
           ! FLOW

  IF (SimpleGlazingSystem) Then ! use alternate angular dependence model for block model of simple glazing input

    TransCurveA = 1.4703D-02*cs**4 + 1.4858D0*cs**3   - 3.852D0*cs**2 + 3.3549D0*cs   - 1.4739D-03
    TransCurveB = 5.5455D-01*cs**4 + 3.563D-02*cs**3 - 2.4157D0*cs**2 + 2.8305D0*cs   - 2.0373D-03
    TransCurveC = 7.7087D-01*cs**4 - 6.3831D-01*cs**3 - 1.5755D0*cs**2 + 2.4482D0*cs   - 2.042D-03
    TransCurveD = 3.4624D-01*cs**4 + 3.9626D-01*cs**3 - 2.5819D0*cs**2 + 2.845D0*cs   - 2.8036D-04
    TransCurveE = 2.8825D0*cs**4   - 5.8734D0*cs**3   + 2.4887D0*cs**2 + 1.510D0*cs   - 2.5766D-03
    TransCurveF = 3.0254D0*cs**4   - 6.3664D0*cs**3   + 3.1371D0*cs**2 + 1.213D0*cs   - 1.3667D-03
    TransCurveG = 3.2292D0*cs**4   - 6.844D0*cs**3   + 3.5351D0*cs**2 + 1.0881D0*cs   - 2.8905D-03
    TransCurveH = 3.3341D0*cs**4   - 7.1306D0*cs**3   + 3.8287D0*cs**2 + 9.7663D-01*cs - 2.9521D-03
    TransCurveI = 3.1464D0*cs**4   - 6.8549D0*cs**3   + 3.9311D0*cs**2 + 7.85950D-01*cs - 2.9344D-03
    TransCurveJ = 3.744D0*cs**4   - 8.8364D0*cs**3   + 6.0178D0*cs**2 + 8.4071D-02*cs + 4.825D-04
    TransCurveFGHI = (TransCurveF + TransCurveG + TransCurveH + TransCurveI) / 4.0D0
    TransCurveFH = (TransCurveF + TransCurveH) / 2.0D0
    TransCurveBDCD = (TransCurveB + TransCurveD + TransCurveC + TransCurveD) / 4.0D0

    ReflectCurveA =  1.6322D+01*cs**4 - 5.7819D+01*cs**3 + 7.9244D+01*cs**2 - 5.0081D+01*cs + 1.3335D+01
    ReflectCurveB =  4.0478D+01*cs**4 - 1.1934D+02*cs**3 + 1.3477D+02*cs**2 - 7.0973D+01*cs + 1.6112D+01
    ReflectCurveC =  5.749D+01*cs**4 - 1.6451D+02*cs**3 + 1.780D+02*cs**2 - 8.8748D+01*cs + 1.8839D+01
    ReflectCurveD =  5.7139D0*cs**4   - 1.6666D+01*cs**3 + 1.8627D+01*cs**2 - 9.7561D0*cs   + 3.0743D0
    ReflectCurveE = -5.4884D-01*cs**4 - 6.4976D0*cs**3   + 2.11990D+01*cs**2 - 2.0971D+01*cs + 7.8138D0
    ReflectCurveF =  4.2902D0*cs**4   - 1.2671D+01*cs**3 + 1.4656D+01*cs**2 - 8.1534D0*cs   + 2.8711D0
    ReflectCurveG =  2.174D+01*cs**4 - 6.4436D+01*cs**3 + 7.4893D+01*cs**2 - 4.1792D+01*cs + 1.0624D+01
    ReflectCurveH =  4.3405D0*cs**4   - 1.280D+01*cs**3 + 1.4777D+01*cs**2 - 8.2034D0*cs   + 2.8793D0
    ReflectCurveI =  4.1357D+01*cs**4 - 1.1775D+02*cs**3 + 1.2756D+02*cs**2 - 6.4373D+01*cs + 1.426D+01
    ReflectCurveJ =  4.4901D0*cs**4   - 1.2658D+01*cs**3 + 1.3969D+01*cs**2 - 7.501D0 *cs  + 2.6928D0
    ReflectCurveFGHI = (ReflectCurveF + ReflectCurveG + ReflectCurveH + ReflectCurveI) / 4.0D0
    ReflectCurveFH = (ReflectCurveF + ReflectCurveH) / 2.0D0
    ReflectCurveBDCD = (ReflectCurveB + ReflectCurveD + ReflectCurveC + ReflectCurveD) / 4.0D0

    If (SimpleGlazingU < 1.4195D0) THEN ! cell 1, 2, or 3
      IF (SimpleGlazingSHGC > 0.45D0) THEN
        ! cell # 1
        ! Curve E
        TransTmp   = TransCurveE
        ReflectTmp = ReflectCurveE

      ELSEIF ((0.35D0 <= SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.45D0)) THEN
        ! cell # 2
        ! 2 way interpolation between Curve E and Curve J
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.35D0, 0.45D0, TransCurveJ, TransCurveE)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.35D0, 0.45D0, ReflectCurveJ, ReflectCurveE)

      ELSEIF (SimpleGlazingSHGC < 0.35d0) THEN
        ! cell # 3
        ! Curve J
        TransTmp   = TransCurveJ
        ReflectTmp = ReflectCurveJ

      ENDIF

    ELSEIF ((1.4195D0 <= SimpleGlazingU) .AND. (SimpleGlazingU <= 1.7034D0)) THEN ! cell 4, 5 , 6, 7, 8, 9, or 10
      IF (SimpleGlazingSHGC > 0.55D0) THEN
        ! cell # 4
        ! Curve E
        TransTmp   = TransCurveE
        ReflectTmp = ReflectCurveE

      ELSEIF ((0.5D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.55D0)) THEN
        ! cell # 5
        ! 4 way interpolation between Curve E , Curve E, Curve E and Curve FGHI

        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                1.4195D0,  1.7034D0 , 0.50D0, 0.55D0, &
                                TransCurveE, TransCurveE, TransCurveFGHI, TransCurveE)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                1.4195D0,  1.7034D0,  0.50D0, 0.55D0, &
                                ReflectCurveE, ReflectCurveE, ReflectCurveFGHI, ReflectCurveE)

      ELSEIF ((0.45D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.5D0))  THEN
        ! cell # 6
        ! 2 way interpolation between Curve E and Curve FGHI
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195D0, 1.7034D0, TransCurveE, TransCurveFGHI)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195D0, 1.7034D0, ReflectCurveE, ReflectCurveFGHI)

      ELSEIF ((0.35D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.45D0))  THEN
        ! cell # 7
        ! 4 way interpolation between Curve E , Curve FGHI, Curve J and Curve FGHI
        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                1.4195D0,  1.7034D0, 0.35D0, 0.45D0, &
                                TransCurveJ, TransCurveE, TransCurveFGHI, TransCurveFGHI)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                1.4195D0,  1.7034D0, 0.35D0, 0.45D0, &
                                ReflectCurveJ, ReflectCurveE, ReflectCurveFGHI, ReflectCurveFGHI)

      ELSEIF ((0.3D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.35D0))  THEN
        ! cell # 8
        ! 2 way interpolation between Curve J and Curve FGHI
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195D0, 1.7034D0, TransCurveJ, TransCurveFGHI)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195D0, 1.7034D0, ReflectCurveJ, ReflectCurveFGHI)

      ELSEIF ((0.25D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.3D0))  THEN
        ! cell # 9
        ! 4 way interpolation between Curve J, Curve FGHI, Curve J and Curve FH
        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                1.4195D0,  1.7034D0, 0.25D0, 0.3D0, &
                                TransCurveJ, TransCurveJ, TransCurveFH, TransCurveFGHI)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                1.4195D0,  1.7034D0, 0.25D0, 0.3D0, &
                                ReflectCurveJ, ReflectCurveJ, ReflectCurveFH, ReflectCurveFGHI)

      ELSEIF (SimpleGlazingSHGC <= 0.25D0) THEN
        ! cell # 10
        ! 2 way interpolation between Curve J and Curve FH
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195D0, 1.7034D0, TransCurveJ, TransCurveFH)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 1.4195D0, 1.7034D0, ReflectCurveJ, ReflectCurveFH)

      ENDIF
    ELSEIF ((1.7034D0 < SimpleGlazingU) .AND. (SimpleGlazingU < 3.4068D0)) THEN ! cell 11, 12, 13, 14, or 15
      IF (SimpleGlazingSHGC > 0.55D0) THEN
        ! cell # 11
        ! Curve E
        TransTmp   = TransCurveE
        ReflectTmp = ReflectCurveE

      ELSEIF ((0.5D0 <= SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.55D0)) THEN
        ! cell # 12
        ! 2 way interpolation between Curve E and Curve FGHI
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.5D0, 0.55D0, TransCurveFGHI, TransCurveE)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.5D0, 0.55D0, ReflectCurveFGHI, ReflectCurveE)

      ELSEIF ((0.3D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC < 0.5D0))  THEN
        ! cell # 13
        ! Curve FGHI
        TransTmp   = TransCurveFGHI
        ReflectTmp = ReflectCurveFGHI

      ELSEIF ((0.25D0 <= SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.3D0))  THEN
        ! cell # 14
        ! 2 way interpolation between Curve FGHI and Curve FH
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.25D0, 0.30D0, TransCurveFH, TransCurveFGHI)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.25D0, 0.30D0, ReflectCurveFH, ReflectCurveFGHI)

      ELSEIF (SimpleGlazingSHGC < 0.25D0) THEN
        ! cell # 15
        !Curve FH
        TransTmp   = TransCurveFH
        ReflectTmp = ReflectCurveFH

      ENDIF

    ELSEIF ((3.4068D0 <= SimpleGlazingU) .AND. (SimpleGlazingU <= 4.5424D0)) THEN ! cell 16, 17, 18, 19, 20, 21, 22, or 23
      IF (SimpleGlazingSHGC > 0.65D0) THEN
        ! cell # 16
        ! 2 way interpolation between Curve E and Curve A
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, TransCurveE, TransCurveA)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, ReflectCurveE, ReflectCurveA)

      ELSEIF ((0.6D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.65D0)) THEN
        ! cell # 17
        ! 4 way interpolation between Curve E , Curve E, Curve A, and Curve BDCD
        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.6D0, 0.65D0, &
                                TransCurveE, TransCurveE, TransCurveBDCD, TransCurveA)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.6D0, 0.65D0, &
                                ReflectCurveE, ReflectCurveE, ReflectCurveBDCD, ReflectCurveA)

      ELSEIF ((0.55D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.6D0))  THEN
        ! cell # 18
        ! 2 way interpolation between Curve E and Curve BDCD
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, TransCurveE, TransCurveBDCD)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, ReflectCurveE, ReflectCurveBDCD)

      ELSEIF ((0.5D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.55D0))  THEN
        ! cell # 19
        ! 4 way interpolation between Curve E , Curve FGHI, Curve BDCD and Curve BDCD
        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.5D0, 0.55D0, &
                                TransCurveFGHI, TransCurveE, TransCurveBDCD, TransCurveBDCD)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.5D0, 0.55D0, &
                                ReflectCurveFGHI, ReflectCurveE, ReflectCurveBDCD, ReflectCurveBDCD)

      ELSEIF ((0.45D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.5D0))  THEN
        ! cell # 20
        ! 2 way interpolation between Curve FGHI and Curve BDCD
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, TransCurveFGHI, TransCurveBDCD)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, ReflectCurveFGHI, ReflectCurveBDCD)

      ELSEIF ((0.3D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.45D0))  THEN
        ! cell # 21
        ! 4 way interpolation between Curve FGHI, Curve FGHI, Curve BDCD, and Curve D
        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.3D0, 0.45D0,                        &
                                TransCurveFGHI, TransCurveFGHI, TransCurveD, TransCurveBDCD)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.3D0, 0.45D0,                        &
                                ReflectCurveFGHI, ReflectCurveFGHI, ReflectCurveD, ReflectCurveBDCD)

      ELSEIF ((0.25D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.3D0))  THEN
        ! cell # 22
        ! 4 way interpolation between Curve FGHI, Curve FH, Curve D, and Curve D
        TransTmp   = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.25D0, 0.3D0,                        &
                                TransCurveFH, TransCurveFGHI, TransCurveD, TransCurveD)
        ReflectTmp = InterpolateBetweenFourValues(SimpleGlazingU, SimpleGlazingSHGC, &
                                3.4068D0,  4.5424D0, 0.25D0, 0.3D0,                        &
                                ReflectCurveFH, ReflectCurveFGHI, ReflectCurveD, ReflectCurveD)

      ELSEIF (SimpleGlazingSHGC <= 0.25D0) THEN
        ! cell # 23
        ! 2 way interpolation between Curve FH and Curve D
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, TransCurveFH, TransCurveD)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingU, 3.4068D0, 4.5424D0, ReflectCurveFH, ReflectCurveD)

      ENDIF
    ELSEIF (SimpleGlazingU > 4.5424D0) THEN ! cell 24, 25, 26, 27, or 28
      IF (SimpleGlazingSHGC > 0.65D0) THEN
        ! cell # 24
        ! Curve A
        TransTmp   = TransCurveA
        ReflectTmp = ReflectCurveA
      ELSEIF ((0.6D0 <= SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.65D0)) THEN
        ! cell # 25
        ! 2 way interpolation between Curve A and Curve BDCD
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.6D0, 0.65D0, TransCurveBDCD, TransCurveA)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.6D0, 0.65D0, ReflectCurveBDCD, ReflectCurveA)

      ELSEIF ((0.45D0 < SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC < 0.6D0))  THEN
        ! cell # 26
        ! Curve BDCD
        TransTmp   = TransCurveBDCD
        ReflectTmp = ReflectCurveBDCD

      ELSEIF ((0.3D0 <= SimpleGlazingSHGC) .AND. (SimpleGlazingSHGC <= 0.45D0))  THEN
        ! cell # 27
        ! 2 way interpolation between Curve BDCD and Curve D
        TransTmp   = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.3D0, 0.45D0, TransCurveD, TransCurveBDCD)
        ReflectTmp = InterpolateBetweenTwoValues(SimpleGlazingSHGC, 0.3D0, 0.45D0, ReflectCurveD, ReflectCurveBDCD)

      ELSEIF (SimpleGlazingSHGC < 0.3D0) THEN
        ! cell # 28
        ! Curve D
        TransTmp   = TransCurveD
        ReflectTmp = ReflectCurveD

      ENDIF
    ENDIF

    IF ( cs == 1.0D0) Then ! at 0 deg incident, TransTmp should be 1.0
      TransTmp = 1.0D0
    ENDIF

    ! now apply normalization factors to zero incidence angle properties
    tfp = tf0 * TransTmp
    IF (tfp < 0.0D0) tfp = 0.0D0
    IF (tfp > 1.0D0) tfp = 1.0D0
    rfp = rf0 * ReflectTmp

    IF (rfp < 0.0D0) rfp = 0.0D0
    IF (rfp > 1.0D0) rfp = 1.0D0

    rbp = rb0 * ReflectTmp
    IF (rbp < 0.0D0) rbp = 0.0D0
    IF (rbp > 1.0D0) rbp = 1.0D0

    IF ( cs == 0.0D0 ) THEN ! at 90 degree incident, reflectance should be 1.0
      rfp = 1.0D0
      rbp = 1.0D0
    ENDIF

!   older model, was in Version 3.1
!    IncidenceAngle = ACOS(cs)
!    CoefFuncSHGC   = 0.768d0 +0.817d0*SimpleGlazingSHGC**4
!
!    tfp = tf0 * cs * (1 + CoefFuncSHGC*(Sin(IncidenceAngle)**3))
!
!    f1     = (((2.403d0*cs - 6.192d0)*cs + 5.625d0)*cs - 2.095d0) * cs + 1
!    f2     = (((-1.188d0* cs + 2.022d0)* cs + 0.137d0) * cs - 1.71d0) * cs
!    Rfit_o = 0.7413d0 - (0.7396d0 * SQRT(SimpleGlazingSHGC))
!
!    rfp = rf0 * (f1 + f2*SQRT(SimpleGlazingSHGC))/Rfit_o
!    rbp = rfp  ! uncoated assumption, back equal front

    RETURN
  ENDIF


  IF (tf0 .LE. 0.0d0) THEN
    ! This is an opaque window.  For all angles, set transmittance to 0; set reflectance to that at zero incidence angle.
    tfp = 0.0d0
    rfp = rf0
    rbp = rb0
  ELSE

    betaf = tf0**2 - rf0**2 + 2.0d0*rf0 + 1.0d0
    betab = tf0**2 - rb0**2 + 2.0d0*rb0 + 1.0d0
    r0f = (betaf-sqrt(betaf**2-4.0d0*(2.0d0-rf0)*rf0))/(2.0d0*(2.0d0-rf0))
    r0b = (betab-sqrt(betab**2-4.0d0*(2.0d0-rb0)*rb0))/(2.0d0*(2.0d0-rb0))

    tmp1=abs(r0f-r0b)
    if (tmp1 /= 0.0d0) then
      testval=abs(r0f-r0b)/(r0f+r0b)
    else
      testval=0.0d0
    endif

    IF (testval.LT.0.001d0) THEN  ! CR8830, CR8942, implications of relaxation of glazing properties CR8413
         ! UNCOATED GLASS
      tmp1=r0f*tf0
      if (tmp1 /= 0.0d0) then
        abf=log(tmp1/(rf0-r0f))
      else
        abf=0.0d0
      endif
      tmp2=r0b*tf0
      if (tmp2 /= 0.0d0) then
       abb=log(tmp2/(rb0-r0b))
      else
        abb = 0.0d0
      endif
      ngf  = (1.0d0+sqrt(r0f))/(1.0d0-sqrt(r0f))
      ngb  = (1.0d0+sqrt(r0b))/(1.0d0-sqrt(r0b))
      cgf  = sqrt(1.0d0-(1.0d0-cs*cs)/(ngf**2))
      cgb  = sqrt(1.0d0-(1.0d0-cs*cs)/(ngb**2))
      tmp3=ngf*cs-cgf
      if (tmp3 /= 0.0d0) then
        rpf1 = (tmp3/(ngf*cs+cgf))**2
      else
        rpf1 = 0.0d0
      endif
      tmp4=ngf*cgf-cs
      if (tmp4 /= 0.0d0) then
        rpf2 = (tmp4/(ngf*cgf+cs))**2
      else
        rpf2=0.0d0
      endif
      tpf1 = 1 - rpf1
      tpf2 = 1 - rpf2
      tmp5=ngb*cs-cgb
      if (tmp5 /= 0.0d0) then
        rpb1 = (tmp5/(ngb*cs+cgb))**2
      else
        rpb1 = 0.0d0
      endif
      tmp6=ngb*cgb-cs
      if (tmp6 /= 0.0d0) then
        rpb2 = (tmp6/(ngb*cgb+cs))**2
      else
        rpb2 = 0.0d0
      endif
      tpb1 = 1 - rpf1
      tpb2 = 1 - rpf2
      tmp7=-abf
      if (tmp7 /= 0.0d0) THEN
        expmabfdivcgf=exp(tmp7/cgf)
      else
        expmabfdivcgf=0.0d0
      endif
      tmp8=-2.0d0*abf
      if (tmp8 /= 0.0d0) then
        expm2abfdivcgf=exp(tmp8/cgf)
      else
        expm2abfdivcgf=0.0d0
      endif
      if (tpf1 /= 0.0d0) then
        tfp1 = tpf1**2*expmabfdivcgf/(1.0d0-rpf1**2*expm2abfdivcgf)
      else
        tfp1 = 0.0d0
      endif
      rfp1 = rpf1*(1.0d0+tfp1*expmabfdivcgf)
      if (tpf2 /= 0.0d0) then
        tfp2 = tpf2**2*expmabfdivcgf/(1.0d0-rpf2**2*expm2abfdivcgf)
      else
        tfp2 = 0.0d0
      endif
      rfp2 = rpf2*(1.0d0+tfp2*expmabfdivcgf)
      tfp  = 0.5d0*(tfp1+tfp2)
      rfp  = 0.5d0*(rfp1+rfp2)
      tmp9=-abb
      if (tmp9 /= 0.0d0) then
        expmabbdivcgb=exp((tmp9/cgb))
      else
        expmabbdivcgb=0.0d0
      endif
      rbp1 = rpb1*(1.0d0+tfp1*expmabbdivcgb)
      rbp2 = rpb2*(1.0d0+tfp2*expmabbdivcgb)
      rbp  = 0.5d0*(rbp1+rbp2)
    ELSE
        ! COATED GLASS
      IF (tf0.GT.0.645d0) THEN
            ! Use clear glass angular distribution.
            ! Normalized clear glass transmittance and reflectance distribution
        IF(cs > 0.999d0) THEN        ! Angle of incidence = 0 deg
          tcl = 1.0d0
          rcl = 0.0d0
        ELSE IF(cs < 0.001d0) THEN   ! Angle of incidence = 90 deg
          tcl = 0.0d0
          rcl = 1.0d0
        ELSE
          tcl =  -0.0015d0 + ( 3.355d0+(-3.840d0+( 1.460d0  +0.0288d0*cs)*cs)*cs)*cs
          rcl =   0.999d0  + (-0.563d0+( 2.043d0+(-2.532d0  +1.054d0 *cs)*cs)*cs)*cs-tcl
        END IF
        tfp = tf0*tcl
        rfp = rf0*(1.0d0-rcl)+rcl
        rbp = rb0*(1.0d0-rcl)+rcl
      ELSE
           ! Use bronze glass angular distribution.
           ! Normalized bronze tinted glass transmittance and reflectance distribution
        IF(cs > 0.999d0) THEN        ! Angle of incidence = 0 deg
          tbnz = 1.0d0
          rbnz = 0.0d0
        ELSE IF(cs < 0.001d0) THEN   ! Angle of incidence = 90 deg
          tbnz = 0.0d0
          rbnz = 1.0d0
        ELSE
          tbnz = -0.002d0  + ( 2.813d0+(-2.341d0+(-0.05725d0+0.599d0 *cs)*cs)*cs)*cs
          rbnz =  0.997d0  + (-1.868d0+( 6.513d0+(-7.862d0  +3.225d0 *cs)*cs)*cs)*cs-tbnz
        END IF
        tfp = tf0*tbnz
        rfp = rf0*(1.d0-rbnz)+rbnz
        rbp = rb0*(1.d0-rbnz)+rbnz
      END IF
    END IF

  END IF

      RETURN
END SUBROUTINE TransAndReflAtPhi


FUNCTION InterpolateBetweenTwoValues(X, X0, X1, F0, F1 ) RESULT (InterpResult)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   June 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Interpolate between two results

          ! METHODOLOGY EMPLOYED:
          ! linear interpolation

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) , INTENT(IN) :: X
  REAL(r64) , INTENT(IN) :: X0
  REAL(r64) , INTENT(IN) :: X1
  REAL(r64) , INTENT(IN) :: F0
  REAL(r64) , INTENT(IN) :: F1
  REAL(r64)              :: InterpResult

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  InterpResult = F0 + ( (X - X0) / (X1 - X0) ) * (F1 - F0)
  RETURN

END FUNCTION InterpolateBetweenTwoValues

FUNCTION InterpolateBetweenFourValues(X, Y, X1, X2, Y1, Y2, Fx1y1, Fx1y2, Fx2y1, Fx2y2 ) RESULT (InterpResult)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   June 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Interpolate between four results.

          ! METHODOLOGY EMPLOYED:
          ! bilinear interpolation (approximate)

          ! REFERENCES:
          ! http://en.wikipedia.org/wiki/Bilinear_interpolation

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) , INTENT(IN) :: X
  REAL(r64) , INTENT(IN) :: Y
  REAL(r64) , INTENT(IN) :: X1
  REAL(r64) , INTENT(IN) :: X2
  REAL(r64) , INTENT(IN) :: Y1
  REAL(r64) , INTENT(IN) :: Y2
  REAL(r64) , INTENT(IN) :: Fx1y1
  REAL(r64) , INTENT(IN) :: Fx1y2
  REAL(r64) , INTENT(IN) :: Fx2y1
  REAL(r64) , INTENT(IN) :: Fx2y2
  REAL(r64)              :: InterpResult

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  InterpResult =  ( Fx1y1 /((X2-X1)*(Y2-Y1)) ) * (X2-X)*(Y2-Y)  &
                + ( Fx2y1 /((X2-X1)*(Y2-Y1)) ) * (X-X1)*(Y2-Y)  &
                + ( Fx1y2 /((X2-X1)*(Y2-Y1)) ) * (X2-X)*(Y-Y1)  &
                + ( Fx2y2 /((X2-X1)*(Y2-Y1)) ) * (X-X1)*(Y-Y1)
  RETURN

END FUNCTION InterpolateBetweenFourValues

!**************************************************************************

SUBROUTINE W5LsqFit(IndepVar,DepVar,N,N1,N2,CoeffsCurve)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   April 1976
          !       MODIFIED       November 1999 F.Winkelmann
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does least squares fit for coefficients of a polynomial
          ! that gives a window property, such as transmittance, as a function of
          ! the cosine of the angle of incidence. The polynomial is of the
          ! form C1*X + C2*X**2 + C3*X**3 + ... +CN*X**N, where N <= 6.
          ! Adapted from BLAST subroutine LSQFIT.


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64)              :: IndepVar(10)               ! Independent variables
REAL(r64)              :: DepVar(10)                 ! Dependent variables
INTEGER                :: N                          ! Order of polynomial
INTEGER                :: N1,N2                      ! First and last data points used
REAL(r64)              :: CoeffsCurve(6)             ! Polynomial coeffients from fit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL(r64)  :: A(6,6)                     ! Least squares derivative matrix
REAL(r64)  :: B(6)                       ! Least squares derivative vector
REAL(r64)  :: D(16,6)                    ! Powers of independent variable
REAL(r64)  :: ACON, SUM                  ! Intermediate variables
INTEGER           :: I, J, K, L, M, KP1, LP1, NM1 ! Loop parameters

           ! FLOW

! Set up least squares matrix
      DO M = N1,N2
        D(M,1) = IndepVar(M)
      END DO

      DO I = 2,N
        DO M = N1,N2
          D(M,I) = D(M,I-1)*IndepVar(M)
        END DO
      END DO

      DO I = 1,N
        SUM = 0.0d0
        DO M = N1,N2
          SUM = SUM+DepVar(M)*D(M,I)
        END DO
        B(I) = SUM
        DO J = 1,N
          SUM = 0.0d0
          DO M = N1,N2
            SUM = SUM+D(M,I)*D(M,J)
          END DO
          A(I,J) = SUM
          A(J,I) = SUM
        END DO
      END DO

! Solve the simultaneous equations using Gauss elimination
      NM1 = N-1
      DO K = 1,NM1
        KP1 = K+1
        DO I = KP1,N
          ACON = A(I,K)/A(K,K)
          B(I) = B(I)-B(K)*ACON
          DO J = K,N
            A(I,J) = A(I,J)-A(K,J)*ACON
          END DO
        END DO
      END DO

! Perform back substituion
      CoeffsCurve(N) = B(N)/A(N,N)
      LP1 = N
      L = N-1

      DO WHILE (L>0)
        SUM = 0.0d0
        DO J = LP1,N
          SUM = SUM+A(L,J)*CoeffsCurve(J)
        END DO
        CoeffsCurve(L) = (B(L)-SUM)/A(L,L)
        LP1 = L
        L = L-1
      END DO

RETURN
END SUBROUTINE W5LsqFit
!********************************************************************************

SUBROUTINE W5LsqFit2 (IndepVar,DepVar,N,N1,N2,CoeffsCurve)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   April 1976
          !       MODIFIED       November 1999 F.Winkelmann
          !                      May 2001 F. Winkelmann, to do 19 indep. variables
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does least squares fit for coefficients of a polynomial
          ! that gives a window property, such as transmittance, as a function of
          ! the cosine of the angle of incidence. The polynomial is of the
          ! form C1*X + C2*X**2 + C3*X**3 + ... +CN*X**N, where N <= 6.
          ! Adapted from BLAST subroutine LSQFIT.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64)              :: IndepVar(19)               ! Independent variables
REAL(r64)              :: DepVar(19)                 ! Dependent variables
INTEGER                :: N                          ! Order of polynomial
INTEGER                :: N1,N2                      ! First and last data points used
REAL(r64)              :: CoeffsCurve(6)             ! Polynomial coeffients from fit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: A(6,6)                     ! Least squares derivative matrix
REAL(r64) :: B(6)                       ! Least squares derivative vector
REAL(r64) :: D(16,6)                    ! Powers of independent variable
REAL(r64) :: ACON, SUM                  ! Intermediate variables
INTEGER          :: I, J, K, L, M, KP1, LP1, NM1 ! Loop parameters


! Set up least squares matrix
      DO M = N1,N2
        D(M,1) = IndepVar(M)
      END DO

      DO I = 2,N
        DO M = N1,N2
          D(M,I) = D(M,I-1)*IndepVar(M)
        END DO
      END DO

      DO I = 1,N
        SUM = 0.0d0
        DO M = N1,N2
          SUM = SUM+DepVar(M)*D(M,I)
        END DO
        B(I) = SUM
        DO J = 1,N
          SUM = 0.0d0
          DO M = N1,N2
            SUM = SUM+D(M,I)*D(M,J)
          END DO
          A(I,J) = SUM
          A(J,I) = SUM
        END DO
      END DO

! Solve the simultaneous equations using Gauss elimination
      NM1 = N-1
      DO K = 1,NM1
        KP1 = K+1
        DO I = KP1,N
          ACON = A(I,K)/A(K,K)
          B(I) = B(I)-B(K)*ACON
          DO J = K,N
            A(I,J) = A(I,J)-A(K,J)*ACON
          END DO
        END DO
      END DO

! Perform back substituion
      CoeffsCurve(N) = B(N)/A(N,N)
      LP1 = N
      L = N-1

      DO WHILE (L>0)
        SUM = 0.0d0
        DO J = LP1,N
          SUM = SUM+A(L,J)*CoeffsCurve(J)
        END DO
        CoeffsCurve(L) = (B(L)-SUM)/A(L,L)
        LP1 = L
        L = L-1
      END DO

RETURN
END SUBROUTINE W5LsqFit2
!***********************************************************************

REAL(r64) FUNCTION DiffuseAverage(PropertyValue)

        ! FUNCTION INFORMATION:
        !       AUTHOR         Fred Winkelmann
        !       DATE WRITTEN   November 1999
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! Calculate value of property, such as transmittance, for hemispherical
        ! diffuse radiation from property values at angles of incidence from
        ! 0 to 90 degrees in 10 degree increments.

        ! METHODOLOGY EMPLOYED:
        ! By Simpson's rule, evaluates the integral from 0 to 90 deg of
        ! 2*PropertyValue(phi)*cos(phi)*sin(phi)*dphi (which is same as
        ! PropertyValue(phi)*sin(2*phi)*dphi)

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64)         :: PropertyValue(10)        ! Property value at angles of incidence
                                              ! 0,10,20,...,80,90 degress

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64), PARAMETER   :: DPhiR = 10.d0*DegToRadians ! Half of 10-deg incidence angle increment (radians)
INTEGER           :: IPhi                     ! Incidence angle counter

        ! FLOW

DiffuseAverage = 0.0d0
DO IPhi = 1,9
  DiffuseAverage = DiffuseAverage + 0.5d0*DPhiR*  &
    (PropertyValue(IPhi)*SIN(2.d0*(IPhi-1)*DPhiR) + PropertyValue(IPhi+1)*SIN(2.d0*IPhi*DPhiR))
END DO
IF (DiffuseAverage < 0.0d0) DiffuseAverage=0.0d0

RETURN
END FUNCTION DiffuseAverage
!*************************************************************************************

REAL(r64) FUNCTION DiffuseAverageProfAngGnd(Property)

        ! FUNCTION INFORMATION:
        !       AUTHOR         Fred Winkelmann
        !       DATE WRITTEN   January 2004
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! Calculates diffuse average of Property, such as blind transmittance, over profile angles
        ! corresponding to (upgoing) radiation from the ground.

        ! METHODOLOGY EMPLOYED:
        ! Integration by Simpson's rule assuming uniform radiance distribution.

USE General, ONLY: InterpProfAng

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64)         :: Property(37)     ! Property value vs. profile angle

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)         :: Phi              ! Profile angle (radians)
REAL(r64)         :: DPhi             ! Phi increment
INTEGER           :: IPhi             ! Phi index
REAL(r64)         :: Sum,SumDenom     ! Sums

Sum = 0.0d0
SumDenom = 0.0d0
DPhi = 5.d0*DegToRadians

! Integrate from -90 to 0 deg
DO IPhi = 1,18
  Phi = -PiOvr2 + (IPhi-0.5d0)*DPhi
  Sum = Sum + COS(Phi)*DPhi*InterpProfAng(Phi,Property)
  SumDenom = SumDenom + COS(Phi)*Dphi
END DO

DiffuseAverageProfAngGnd = Sum/SumDenom
IF (DiffuseAverageProfAngGnd < 0.0d0) DiffuseAverageProfAngGnd=0.0d0

RETURN
END FUNCTION DiffuseAverageProfAngGnd
!*************************************************************************************

REAL(r64) FUNCTION DiffuseAverageProfAngSky(Property)

        ! FUNCTION INFORMATION:
        !       AUTHOR         Fred Winkelmann
        !       DATE WRITTEN   January 2004
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! Calculates diffuse average of Property, such as blind transmittance, over profile angles
        ! corresponding to (downgoing) radiation from the sky.

        ! METHODOLOGY EMPLOYED:
        ! Integration by Simpson's rule assuming uniform radiance distribution.

USE General, ONLY: InterpProfAng

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64)         :: Property(37)     ! Property value vs. profile angle

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)         :: Phi              ! Profile angle (radians)
REAL(r64)         :: DPhi             ! Phi increment
INTEGER           :: IPhi             ! Phi index
REAL(r64)         :: Sum,SumDenom     ! Sums

Sum = 0.0d0
SumDenom = 0.0d0
DPhi = 5.d0*DegToRadians

! Integrate from 0 to 90 deg
DO IPhi = 19,36
  Phi = -PiOvr2 + (IPhi-0.5d0)*DPhi
  Sum = Sum + COS(Phi)*DPhi*InterpProfAng(Phi,Property)
  SumDenom = SumDenom + COS(Phi)*Dphi
END DO

DiffuseAverageProfAngSky = Sum/SumDenom
IF (DiffuseAverageProfAngSky < 0.0d0) DiffuseAverageProfAngSky=0.0d0

RETURN
END FUNCTION DiffuseAverageProfAngSky
!*************************************************************************************

SUBROUTINE CalcWinFrameAndDividerTemps(SurfNum,tout,tin,HOutConv,HInConv,outir,ConstrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Aug 2000, FCW: Add effect of frame and divider projections
          !                      Jun 2001, FCW: Add frame/divider contribution to WinHeatGain
          !                      Aug 2003, FCW: Fix calculation of divider outside temp: frame
          !                       inside temp was being used instead of divider inside temp
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates window frame divider face temperatures from a linearized
          ! heat balance on the inside and outside faces

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER           :: SurfNum              ! Surface number
REAL(r64)         :: tout                 ! Outside air temperature (K)
REAL(r64)  :: tin                  ! Inside air temperature (K)
REAL(r64)         :: HOutConv             ! Outside convective air film conductance (W/m2-K)
REAL(r64)         :: HInConv              ! Inside convective air film conductance (W/m2-K)
REAL(r64)         :: outir                ! Exterior IR irradiance from sky and ground
INTEGER           :: ConstrNum            ! Construction number of window

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)         :: HInRad               ! Inside radiative conductance (W/m2-K)
REAL(r64)         :: HOutRad              ! Outside radiative conductance (W/m2-K)
INTEGER           :: FrDivNum             ! Frame/divider number
REAL(r64)         :: TInRad               ! Inside radiative temperature (K)
REAL(r64)         :: TInRadFr             ! Effective inside radiative temperature for frame (K)
REAL(r64)         :: TInRadDiv            ! Effective inside radiative temperature for divider (K)
REAL(r64)         :: TOutRad              ! Outside radiative temperature (K)
REAL(r64)         :: TOutRadFr            ! Effective outside radiative temperature for frame (K)
REAL(r64)         :: TOutRadDiv           ! Effective outside radiative temperature for divider (K)
INTEGER           :: ShadeFlag            ! Window shading flag
REAL(r64)         :: FrameCon             ! Frame conductance (W/m2-K)

REAL(r64)         :: Afac,Bfac,Dfac,Efac  ! Intermediate calculation variables
INTEGER           :: DivType              ! Divider type
REAL(r64)         :: DivCon               ! Divider conductance (W/m2-K)
REAL(r64)         :: DivEmisIn            ! Inside divider emissivity
REAL(r64)         :: DivEmisOut           ! Outside divider emissivity

REAL(r64)         :: ProjCorrFrOut        ! Outside correction factor for absorbed radiation
                                          !   for frame with outside projection
REAL(r64)         :: ProjCorrFrIn         ! Inside correction factor for absorbed radiation
                                          !   for frame with inside projection
REAL(r64)         :: HOutConvFr           ! Effective outside convective coeff for frame
                                          !   with outside projection (W/m2-K)
REAL(r64)         :: HOutConvDiv          ! Effective outside convective coeff for divider
                                          !   with outside projection (W/m2-K)
REAL(r64)         :: HInConvFr            ! Effective inside convective coeff for frame
                                          !   with inside projection (W/m2-K)
REAL(r64)         :: HInConvDiv           ! Effective inside convective coeff for divider
                                          !   with inside projection (W/m2-K)
REAL(r64)         :: EmisGlassOut         ! Outside surface emissivity of window glazing
REAL(r64)         :: EmisGlassIn          ! Inside surface emissivity of window glazing
INTEGER           :: TotGlassLayers       ! Total number of glass layers
INTEGER           :: TotLayers            ! Total number of layers in unshaded construction
REAL(r64)         :: DivTempOut           ! Outside surface divider temperature (K)
REAL(r64)         :: FrameHeatGain        ! Heat gain to zone from frame (W)
REAL(r64)         :: ProjCorrWinHeatGain  ! Inside projection correction to IR from divider to zone
                                          !   for window heat gain calculation
REAL(r64)         :: DividerConduction    ! Conduction through divider from outside to inside face (W)
REAL(r64)         :: DividerConvHeatGain  ! Convective heat gain from divider to zone (W)
REAL(r64)         :: DividerRadHeatGain   ! Convective IR radiative gain from divider to zone (W)
REAL(r64)         :: DividerHeatGain      ! Heat gain from divider to zone (W)

TInRad =  (SurfaceWindow(SurfNum)%IRfromParentZone/sigma)**0.25d0
TOutRad = (outir/sigma)**0.25d0
ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
FrDivNum = Surface(SurfNum)%FrameDivider
TotLayers = Construct(ConstrNum)%TotLayers
TotGlassLayers = Construct(ConstrNum)%TotSolidLayers
EmisGlassOut = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermalFront
EmisGlassIn  = Material(Construct(ConstrNum)%LayerPoint(TotLayers))%AbsorpThermalBack
FrameHeatGain = 0.0d0
DividerConduction = 0.0d0
SurfaceWindow(SurfNum)%FrameHeatGain = 0.0d0
SurfaceWindow(SurfNum)%FrameHeatLoss = 0.0d0
SurfaceWindow(SurfNum)%DividerHeatGain = 0.0d0
SurfaceWindow(SurfNum)%DividerHeatLoss = 0.0d0

IF(SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
  ! Window has a frame. Note that if a shade, screen or blind is present it covers only the glazed part of the
  ! window and is assumed not to shadow long- or short-wave radiation incident on the frame elements.
  ProjCorrFrOut = SurfaceWindow(SurfNum)%ProjCorrFrOut
  ProjCorrFrIn = SurfaceWindow(SurfNum)%ProjCorrFrIn
  TOutRadFr = TOutRad * ((1.d0+0.5d0*ProjCorrFrOut)/(1.d0+ProjCorrFrOut))**0.25d0
  TInRadFr  = TInRad * ((1.d0+0.5d0*ProjCorrFrIn)/(1.d0+ProjCorrFrIn))**0.25d0
  FrameCon = SurfaceWindow(SurfNum)%FrameConductance
  HInRad = 0.5d0 * SurfaceWindow(SurfNum)%FrameEmis * sigma * &
             (TInRadFr + SurfaceWindow(SurfNum)%FrameTempSurfIn + TKelvin)**3
  HInConvFr = 0.0d0
  HOutRad = 0.5d0 * SurfaceWindow(SurfNum)%FrameEmis * sigma * &
              (TOutRadFr + SurfaceWindow(SurfNum)%FrameTempSurfOut + TKelvin)**3
  HOutConvFr = HOutConv
  IF(FrameDivider(FrDivNum)%FrameProjectionOut > 0.0d0) THEN
    HOutRad = HOutRad * (1.d0+ProjCorrFrOut)
    HOutConvFr = HOutConv * (1.d0+ProjCorrFrOut)
    ! Add long-wave from outside window surface absorbed by frame outside projection
    SurfaceWindow(SurfNum)%FrameQRadOutAbs = SurfaceWindow(SurfNum)%FrameQRadOutAbs + &
      0.5d0 * SurfaceWindow(SurfNum)%ProjCorrFrOut * FrameDivider(FrDivNum)%FrameEmis * &
      EmisGlassOut * sigma * SurfaceWindow(SurfNum)%ThetaFace(1)**4
  END IF
  IF(FrameDivider(FrDivNum)%FrameProjectionIn > 0.0d0) THEN
    HInRad = HInRad * (1.d0+ProjCorrFrIn)
    HInConvFr = HInConv * (1.d0+ProjCorrFrIn)
    ! Add long-wave from inside window surface absorbed by frame inside projection
    SurfaceWindow(SurfNum)%FrameQRadInAbs = SurfaceWindow(SurfNum)%FrameQRadInAbs + &
      0.5d0 * SurfaceWindow(SurfNum)%ProjCorrFrIn * FrameDivider(FrDivNum)%FrameEmis * &
      EmisGlassIn * sigma * SurfaceWindow(SurfNum)%ThetaFace(2*TotGlassLayers)**4
  END IF
  Afac = (HOutRad*TOutRadFr + HOutConvFr*tout + SurfaceWindow(SurfNum)%FrameQRadOutAbs) / &
           (HOutRad + FrameCon + HOutConvFr)
  Bfac = FrameCon / (HOutRad + FrameCon + HOutConvFr)
  Dfac = (HInRad*TInRadFr + HInConvFr*tin + SurfaceWindow(SurfNum)%FrameQRadInAbs) / &
           (HInRad + FrameCon + HInConvFr)
  Efac = FrameCon / (HInRad + FrameCon + HInConvFr)
  SurfaceWindow(SurfNum)%FrameTempSurfIn = (Dfac + Efac*Afac)/(1.d0 - Efac*Bfac) - TKelvin
  SurfaceWindow(SurfNum)%FrameTempSurfOut = Afac +   &
                           Bfac*(SurfaceWindow(SurfNum)%FrameTempSurfIn+TKelvin) - TKelvin
  ! Heat gain to zone from frame

!  FrameHeatGain = SurfaceWindow(SurfNum)%FrameArea * (1.+SurfaceWindow(SurfNum)%ProjCorrFrIn) * &
!  ( SurfaceWindow(SurfNum)%FrameEmis*(sigma*(SurfaceWindow(SurfNum)%FrameTempSurfIn+TKelvin)**4 - rmir) + &
!    hcin*(SurfaceWindow(SurfNum)%FrameTempSurfIn+TKelvin - tin) )

  FrameHeatGain = SurfaceWindow(SurfNum)%FrameArea * (1.d0+SurfaceWindow(SurfNum)%ProjCorrFrIn) * &
  ( hcin*(SurfaceWindow(SurfNum)%FrameTempSurfIn+TKelvin - tin) )


  IF (FrameHeatGain > 0.0d0) THEN
    SurfaceWindow(SurfNum)%FrameHeatGain = FrameHeatGain
  ELSE
    SurfaceWindow(SurfNum)%FrameHeatLoss = ABS(FrameHeatGain)
  ENDIF

  WinHeatGain(SurfNum) = WinHeatGain(SurfNum) + FrameHeatGain
  WinGainFrameDividerToZoneRep(SurfNum) = FrameHeatGain
END IF  ! End of check if window has a frame

IF(SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%StormWinFlag < 1) THEN
 ! Window has divider. Note that if the window has a storm window layer in place (StormWinFlag = 1)
 ! the divider heat transfer calculation is not done.

  DivType = SurfaceWindow(SurfNum)%DividerType
  DivCon  = SurfaceWindow(SurfNum)%DividerConductance

  IF(DivType == DividedLite) THEN  ! Divided lite
    DivEmisIn = SurfaceWindow(SurfNum)%DividerEmis
    DivEmisOut = DivEmisIn
  ELSE                   ! Suspended (between-glass) divider
    DivEmisOut = Material(Construct(ConstrNum)%LayerPoint(1))%AbsorpThermalFront
    DivEmisIn =  Material(Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers))%AbsorpThermalBack

  END IF

  TOutRadDiv = TOutRad* &
    ((1.d0+SurfaceWindow(SurfNum)%ProjCorrDivOut)/(1.d0+2.d0*SurfaceWindow(SurfNum)%ProjCorrDivOut))**0.25d0
  TInRadDiv = TInRad* &
    ((1.d0+SurfaceWindow(SurfNum)%ProjCorrDivIn)/(1.d0+2.d0*SurfaceWindow(SurfNum)%ProjCorrDivIn))**0.25d0
  HInRad = 0.5d0 * DivEmisIn * sigma * &
             (TInRadDiv + SurfaceWindow(SurfNum)%DividerTempSurfIn + TKelvin)**3
  HOutRad = 0.5d0 * DivEmisOut * sigma * &
              (TOutRadDiv + SurfaceWindow(SurfNum)%DividerTempSurfOut + TKelvin)**3
  HOutConvDiv = HOutConv

  IF(FrameDivider(FrDivNum)%DividerProjectionOut > 0.0d0) THEN
    HOutRad = HOutRad * (1.d0+2.d0*SurfaceWindow(SurfNum)%ProjCorrDivOut)
    IF(SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) HOutConvDiv =  &
      SurfaceWindow(SurfNum)%ConvCoeffWithShade
    HOutConvDiv = HOutConvDiv * (1.d0+2.d0*SurfaceWindow(SurfNum)%ProjCorrDivOut)
    ! Add long-wave from outside window surface absorbed by divider outside projection
    SurfaceWindow(SurfNum)%DividerQRadOutAbs = SurfaceWindow(SurfNum)%DividerQRadOutAbs + &
      SurfaceWindow(SurfNum)%ProjCorrDivOut * FrameDivider(FrDivNum)%DividerEmis * &
      EmisGlassOut * sigma * SurfaceWindow(SurfNum)%ThetaFace(1)**4
  END IF

  HInConvDiv = HInConv

  IF(FrameDivider(FrDivNum)%DividerProjectionIn > 0.0d0) THEN
    HInRad = HInRad *  (1.d0+2.d0*SurfaceWindow(SurfNum)%ProjCorrDivIn)
    IF(SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn) HInConvDiv =  &
      SurfaceWindow(SurfNum)%ConvCoeffWithShade
    HInConvDiv = HInConvDiv * (1.d0+2.d0*SurfaceWindow(SurfNum)%ProjCorrDivIn)
    ! Add long-wave from inside window surface absorbed by divider inside projection
    SurfaceWindow(SurfNum)%DividerQRadInAbs = SurfaceWindow(SurfNum)%DividerQRadInAbs + &
      SurfaceWindow(SurfNum)%ProjCorrDivIn * FrameDivider(FrDivNum)%DividerEmis * &
      EmisGlassIn * sigma * SurfaceWindow(SurfNum)%ThetaFace(2*TotGlassLayers)**4
  END IF
  Afac = (HOutRad*TOutRadDiv + HOutConvDiv*tout + SurfaceWindow(SurfNum)%DividerQRadOutAbs) / &
           (HOutRad + DivCon + HOutConvDiv)
  Bfac = DivCon / (HOutRad + DivCon + HOutConvDiv)
  Dfac = (HInRad*TInRadDiv + HInConvDiv*tin + SurfaceWindow(SurfNum)%DividerQRadInAbs) / &
           (HInRad + DivCon + HInConvDiv)
  Efac = DivCon / (HInRad + DivCon + HInConvDiv)
  SurfaceWindow(SurfNum)%DividerTempSurfIn = (Dfac + Efac*Afac)/(1 - Efac*Bfac) - TKelvin
  SurfaceWindow(SurfNum)%DividerTempSurfOut = Afac + Bfac*(SurfaceWindow(SurfNum)%DividerTempSurfIn &
                                                      + TKelvin) - TKelvin
  ! Contribution of divider to window heat gain
  ProjCorrWinHeatGain = 1.d0 + 2.d0*SurfaceWindow(SurfNum)%ProjCorrDivIn
  DividerConduction = SurfaceWindow(SurfNum)%DividerArea * DivCon * &
    (SurfaceWindow(SurfNum)%DividerTempSurfOut - SurfaceWindow(SurfNum)%DividerTempSurfin)
  IF (DividerConduction > 0.0d0) THEN
    SurfaceWindow(SurfNum)%DividerHeatGain = DividerConduction
  ELSE
    SurfaceWindow(SurfNum)%DividerHeatLoss = ABS(DividerConduction)
  ENDIF
  WinHeatGain(SurfNum) = WinHeatGain(SurfNum) + DividerConduction
  WinGainFrameDividerToZoneRep(SurfNum) = WinGainFrameDividerToZoneRep(SurfNum) + DividerConduction
  ! The following three statements are for debugging purposes only
    DividerConvHeatGain = SurfaceWindow(SurfNum)%DividerArea *  &
      HInConvDiv * (SurfaceWindow(SurfNum)%DividerTempSurfIn + TKelvin - tin)
    DividerRadHeatGain = SurfaceWindow(SurfNum)%DividerArea *  &
      HInRad * (SurfaceWindow(SurfNum)%DividerTempSurfIn + TKelvin - TInRadDiv)  &
      - SurfaceWindow(SurfNum)%DividerArea * SurfaceWindow(SurfNum)%DividerQRadInAbs
    DividerHeatGain = DividerConvHeatGain + DividerRadHeatGain
  ! If interior shade is present it is assumed that both the convective and IR radiative gain
  ! from the inside surface of the divider goes directly into the zone air -- i.e., the IR radiative
  ! interaction between divider and shade is ignored due to the difficulty of calculating this interaction
  ! at the same time that the interaction between glass and shade is calculated.
  IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) &
    SurfaceWindow(SurfNum)%DividerConduction = DividerConduction
  DivTempOut = SurfaceWindow(SurfNum)%DividerTempSurfOut + TKelvin
END IF  ! End of check if window has dividers

END SUBROUTINE CalcWinFrameAndDividerTemps
!************************************************************************************

SUBROUTINE CalcNominalWindowCond (ConstrNum,WinterSummerFlag,NominalConductance,SHGC,TSolNorm,TVisNorm,ErrFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Oct 2000, FW: add solar heat gain coefficient
          !                      June 2001, FW: account for blinds; change summer outside air
          !                       temp from 35.0C to 31.7C to correspond to ASHRAE value
          !                      Feb 2003, FW: add comments that this routine is not called for
          !                       between-glass shade/blind constructions.
          !                      May 2006, RR: account for screens
          !                      Oct 2007, LKL: change temps to match Window 5 values
          !                      Feb 2009, BG: Changes for CR7682 (SHGC)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates nominal center-of-glass U-value and solar heat gain coefficient
          ! (SHGC) of a window construction for ASHRAE winter and summer conditions.
          !
          ! Winter:
          ! Inside air temperature = 21.C (69.80F)
          ! Outside air temperature = -18C (-.4F)
          ! Windspeed = 5.5 m/s (12.3 mph)
          ! No solar radiation
          ! Replaced Winter:
          ! Inside air temperature = 21.1C (70F)
          ! Outside air temperature = -17.8C (0F)
          ! Windspeed = 6.71 m/s (15 mph)
          ! No solar radiation
          !
          ! Summer:
          ! Inside air temperature = 24C (75.2F)
          ! Outside air temperature = 32C (89.6F)
          ! Windspeed = 2.8 m/s (6.2 mph)
          ! 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
          ! Replaced Summer:
          ! Inside air temperature = 24C (75.2F) ! BG changed again Feb. 2009 by 0.1 (per Window5 team)
          ! Outside air temperature = 31.7C (89F)
          ! Windspeed = 3.35 m/s (7.5 mph)
          ! 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
          !
          ! The window's inside surround is assumed to be a black body at the inside air temp.
          ! The window's outside surround is assumed t be a black body at the outside air temp.
          !
          ! Note that in this routine we use a value of 26 W/m2 for the outside convective
          ! air film conductance for 5.5 m/s (12.3 mph) wind speed.
          ! This is the value used in Window 5 and is also the value for which the center-of-glass
          ! conductances in the EnergyPlus window construction reference data set were calculated.
          ! However, in the time step loop we will have different values of outside film
          ! conductance depending on that time step's wind speed, wind direction, surface-to-air temp difference,
          ! etc.(see subroutine InitExteriorConvectionCoeff).
          !
          ! This routine will return an error and exit for window constructions with between-glass shade or blind
          ! until a method is worked out to determine the nominal conductance and SHGC in this case.
          !
          ! If an interior or exterior shade or blind is present in the construction,
          ! the conductance calculation does not include the effect of the shade or blind.
          ! This is because in this case the conductance depends on the natural convective
          ! air flow in the shade/glass, screen/glass or blind/glass channel, which in turn is highly dependent
          ! on window height and other parameters that are not part of the construction definition.
          ! Therefore, the reported conductance value will be too high for windows with a tightly fitting
          ! shade, screen or blind with a relatively high thermal resistance.
          !
          ! For SHGC calculation, all solar absorbed by interior blind or shade is assumed
          ! to go into zone air. (This is not true in general; the fraction of this absorbed solar that
          ! is conducted back out is accounted for in the time-step glazing calculation.)
          !
          ! For CR 7682, the SHGC calculations were changed to model the absorbed solar arriving at the middle of the layer
          ! rather than at the outer face of the layer.  The resistances changed by one half the glazing layer, or 0.5/scon(n).
          ! (CR 7682 also changed WindowTempsForNominalCond to include absorbed solar, a bigger change)
          !
          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: POLYF, InterpBlind, InterpSlatAng, InterpProfSlatAng, BlindBeamBeamTrans

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER           :: ConstrNum                    ! Construction number
INTEGER           :: WinterSummerFlag             ! 1=winter, 2=summer
REAL(r64)         :: NominalConductance           ! Nominal center-of-glass conductance, including air films
REAL(r64)         :: SHGC                         ! Nominal center-of-glass solar heat gain coefficient for
                                                  !   normal incidence beam solar radiation
REAL(r64)         :: TSolNorm                     ! Overall beam solar transmittance at normal incidence
REAL(r64)         :: TVisNorm                     ! Overall beam visible transmittance at normal incidence
INTEGER           :: ErrFlag                      ! Error flag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER           :: TotLay                       ! Total number of layers in a construction
                                                  !   (sum of solid layers and gap layers)
INTEGER           :: TotGlassLay                  ! Total number of glass layers in a construction
INTEGER           :: Lay                          ! Layer number
INTEGER           :: LayPtr                       ! Material number for a layer
INTEGER           :: IGlass                       ! glass layer number (1,2,3,...)
INTEGER           :: IGap                         ! Gap layer number (1,2,...)
INTEGER           :: IMix                         ! Gas component loop index for gap gas mixture
INTEGER           :: ICoeff                       ! Gas property coefficient index

REAL(r64)         :: BeamSolarInc                 ! Incident beam radiation at zero angle of incidence (W/m2)
REAL(r64)         :: hOutRad,hInRad               ! Radiative conductance of outside and inside airfilm [W/m2-K]
REAL(r64)         :: rOut, rIn                    ! Combined radiative and conductive outside and inside film
                                                  !   resistance [m2-K/W]
REAL(r64)         :: hgap(5)                      ! Conductive gap conductance [W/m2-K]
REAL(r64)         :: hGapTot(5)                   ! Combined radiative and conductive gap conductance [W/m2-K]
REAL(r64)         :: Rbare                        ! Nominal center-of-glass resistance without air films [m2-K/W]
INTEGER           :: ShadeFlag                    ! Shading flag
REAL(r64)         :: ShadeRes                     ! Thermal resistance of shade
INTEGER           :: MatOutside                   ! Material number of outside layer of construction
INTEGER           :: MatInside                    ! Material number of inside layer of construction
INTEGER           :: MatShade                     ! Material number of shade layer
REAL(r64)         :: AbsBeamNorm(5)               ! Beam absorptance at normal incidence for each glass layer
REAL(r64)         :: AbsBeamShadeNorm             ! Shade solar absorptance at normal incidence
INTEGER           :: ConstrNum1                   ! Construction counter
INTEGER           :: ConstrNumBare                ! Construction without shading device
INTEGER           :: BlNum                        ! Blind number
INTEGER           :: ScNum                        ! Screen number
LOGICAL           :: VarSlats                     ! True if slats in blind are variable angle
REAL(r64)         :: SlatAng                      ! Slat angle (rad)
INTEGER           :: LayPtrSh                     ! Layer pointer of blind
REAL(r64)         :: TBmBm,TBmBmVis               ! Bare glass normal incidence beam-beam transmittance
REAL(r64)         :: TBlBmBm                      ! Normal incidence blind beam-beam transmittance
REAL(r64)         :: TScBmBm, TScBmBmVis          ! Screen incident beam-beam transmittance
REAL(r64)         :: TBmBmBl,TBmBmBlVis           ! TBmBm * TBlBmBm, TBmBmVis * TBlBmBm
REAL(r64)         :: RGlDiffBack,RGlDiffBackVis   ! Bare glass back sol/vis reflectance
REAL(r64)         :: RGlDiffFront,RGlDiffFrontVis ! Bare glass front sol/vis reflectance
REAL(r64)         :: RhoBlFront,RhoBlFrontVis     ! Blind normal front beam-diffuse sol/vis reflectance
REAL(r64)         :: RhoBlBack,RhoBlBackVis       ! Blind normal back beam-diffuse sol/vis reflectance
REAL(r64)         :: RScBack,RScBackVis           ! Screen back beam-diffuse sol/vis reflectance (same as front)
REAL(r64)         :: AbsBlFront                   ! Blind normal front beam solar absorptance
REAL(r64)         :: AbsBlBack                    ! Blind normal back beam solar absorptance
REAL(r64)         :: RhoBlDiffFront,RhoBlDiffFrontVis ! Blind front diffuse-diffuse sol/vis reflectance
REAL(r64)         :: AbsBlDiffFront               ! Blind front diffuse solar absorptance
REAL(r64)         :: AbsBlDiffBack                ! Blind back diffuse solar absorptance
REAL(r64)         :: RGlFront,RGlFrontVis         ! Bare glass normal front beam sol/vis reflectance
REAL(r64)         :: RhoBlDiffBack,RhoBlDiffBackVis ! Blind back diffuse-diffuse sol/vis reflectance
REAL(r64)         :: RScDifBack,RScDifBackVis     ! Screen back diffuse-diffuse sol/vis reflectance (doesn't change with sun angle)
REAL(r64)         :: TBlBmDif,TBlBmDifVis         ! Blind front normal beam-diffuse sol/vis transmittance
REAL(r64)         :: TBlDifDif,TBlDifDifVis       ! Blind front diffuse-diffuse sol/vis transmittance
REAL(r64)         :: TScBmDif,TScBmDifVis         ! Screen front beam-diffuse sol/vis transmittance
REAL(r64)         :: TDif,TDifVis                 ! Bare glass diffuse sol/vis transmittance
REAL(r64)         :: AGlDiffBack                  ! Back diffuse solar absorptance of a glass layer

ErrFlag = 0
TotLay = Construct(ConstrNum)%TotLayers
TotGlassLay = Construct(ConstrNum)%TotGlassLayers
ngllayer = TotGlassLay !Objexx:Uninit This routine needs to check/enforce 1<=ngllayer<=4
!EPTeam - believe that is done on input.
nglface  = 2*ngllayer
tilt = 90.d0   ! Assume vertical window

IF(WinterSummerFlag == 1) THEN  ! Winter
  ! LKL Oct 2007:  According to Window5, Winter environmental conditions are:
  tin = 294.15d0   ! Inside air temperature (69.8F, 21.C)
  tout = 255.15d0  ! Outside air temperature (-.4F, -18C)
  hcout = 26.d0    ! Outside convective film conductance for 5.5 m/s (12.3 mph) wind speed
                 ! (the value used in Window 5)
!  tin = 294.26   ! Inside air temperature (70F, 21.1C)
!  tout = 255.35  ! Outside air temperature (0F, -17.8C)
!  hcout = 25.47  ! Outside convective film conductance for 6.71 m/s (15 mph) wind speed
!                 ! (the value used in Window 4)
  BeamSolarInc = 0.0d0
ELSE  ! Summer
  ! LKL Oct 2007: According to Window5, Summer environmental conditions are:
  !tin = 297.05d0   ! Inside air temperature (75.2F, 24C)
  ! BG Feb. 2009: According to Window5 Expert Christian Kohler, it is exactly 24C or 297.15
  tin = 297.15d0
  tout = 305.15d0  ! Outside air temperature (89.6F, 32C)
  hcout = 15.d0    ! Outside convective film conductance for 2.8 m/s (6.2 mph) wind speed
                 ! (the value used in Window 5)
!  tin = 297.05   ! Inside air temperature (75F, 23.9C)
!  !tout = 308.15  ! Outside air temperature (95F, 35.0C)
!  ! Changed 6/20/01 by FCW to make consistent with Window 4 and 5.
!  tout = 304.82  ! Outside air temperature (89F, 31.7C)
!  hcout = 18.86  ! Outside convective film conductance for 3.35 m/s (7.5 mph) wind speed
!                 ! (average of Window 4 0 m/s and 6.71 m/s values)
  BeamSolarInc = 783.0d0
END IF

! IR incident on inside of glazing (inside surround assumed to be
! a black body at inside air temperature)
rmir = sigma * tin**4

! IR incident on outside of glazing
! (outside surround is assumed to be a black body at outside air temperature)
outir = sigma * tout**4

! Determine whether construction has an exterior or interior shade or blind
ShadeFlag = NoShade
ShadeRes = 0.0d0
MatOutside = Construct(ConstrNum)%LayerPoint(1)
MatInside = Construct(ConstrNum)%LayerPoint(TotLay)
IF(Material(MatOutside)%Group == 2) THEN       ! Exterior shade present
  MatShade = MatOutside
  ShadeFlag = ExtShadeOn
  ! Set glazing outside convection coefficient to Window 4 still-air value
  hcout = 12.25d0
ELSE IF(Material(MatOutside)%Group == 7) THEN  ! Exterior screen present
  MatShade = MatOutside
  ScNum = Material(MatShade)%ScreenDataPtr
! Orphaned constructs with exterior screen are ignored
  IF(ScNum .GT. 0)ShadeFlag = ExtScreenOn
  hcout = 12.25d0
ELSE IF(Material(MatOutside)%Group == 5) THEN  ! Exterior blind present
  MatShade = MatOutside
  ShadeFlag = ExtBlindOn
  BlNum = Material(MatShade)%BlindDataPtr
  hcout = 12.25d0
ELSE IF(Material(MatInside)%Group == 2) THEN   ! Interior shade present
  MatShade = MatInside
  ShadeFlag = IntShadeOn
ELSE IF(Material(MatInside)%Group == 5) THEN   ! Interior blind present
  MatShade = MatInside
  BlNum = Material(MatShade)%BlindDataPtr
  ShadeFlag = IntBlindOn
ELSE IF(TotGlassLay==2) THEN
  IF(Material(Construct(ConstrNum)%LayerPoint(3))%Group == 2) ShadeFlag = BGShadeOn
  IF(Material(Construct(ConstrNum)%LayerPoint(3))%Group == 5) ShadeFlag = BGBlindOn
ELSE IF(TotGlassLay==3) THEN
  IF(Material(Construct(ConstrNum)%LayerPoint(5))%Group == 2) ShadeFlag = BGShadeOn
  IF(Material(Construct(ConstrNum)%LayerPoint(5))%Group == 5) ShadeFlag = BGBlindOn
END IF

IF(ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn) THEN
  ErrFlag = 2
  RETURN
END IF

TSolNorm = POLYF(1.0d0,Construct(ConstrNum)%TransSolBeamCoef(1:6))
TVisNorm = POLYF(1.0d0,Construct(ConstrNum)%TransVisBeamCoef(1:6))
AbsBeamShadeNorm = 0.0d0
IF(ShadeFlag == IntShadeOn .OR. ShadeFlag == ExtShadeOn) THEN  ! Exterior or interior shade on
  AbsBeamShadeNorm = POLYF(1.0d0,Construct(ConstrNum)%AbsBeamShadeCoef(1:6))
! Exterior blind or screen or interior blind on
ELSE IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == ExtScreenOn) THEN
  ! Find unshaded construction that goes with this construction w/blind or screen
  ConstrNumBare = 0
  DO ConstrNum1 = 1, TotConstructs
    IF(ConstrNum1 /= ConstrNum .AND. Construct(ConstrNum1)%TypeIsWindow .AND. &
       Construct(ConstrNum1)%TotGlassLayers == Construct(ConstrNum1)%TotSolidLayers .AND. &
       Construct(ConstrNum1)%TotGlassLayers == Construct(ConstrNum)%TotGlassLayers) THEN
      ! We have an unshaded window construction with the same number of glass layers as ConstrNum;
      ! see if the glass and gas layers match
      ConstrNumBare = ConstrNum1
      DO Lay = 1,Construct(ConstrNum1)%TotLayers
        Layptr = Construct(ConstrNum1)%LayerPoint(Lay)
        IF(ShadeFlag == IntBlindOn) THEN  ! The shaded construction has an interior blind
          LayPtrSh = Construct(ConstrNum)%LayerPoint(Lay)
        ELSE                 ! The shaded construction has an exterior blind or screen
          LayPtrSh = Construct(ConstrNum)%LayerPoint(Lay+1)
        END IF
        IF(LayPtrSh /= LayPtr) ConstrNumBare = 0
      END DO
      IF(ConstrNumBare /= 0) EXIT
    END IF
  END DO
  IF(ConstrNumBare == 0) THEN
    ! No matching bare construction found for this construction with blind or screen
    ErrFlag = 1
    RETURN
  END IF

  TBmBm       = POLYF(1.0d0,Construct(ConstrNumBare)%TransSolBeamCoef(1:6))
  TBmBmVis    = POLYF(1.0d0,Construct(ConstrNumBare)%TransVisBeamCoef(1:6))
  IF(ShadeFlag == ExtScreenOn)THEN
!   Don't need to call subroutine, use normal incident properties (SUBROUTINE CalcNominalWindowCond)
!   Last call to CalcScreenTransmittance(ISurf) was done at direct normal angle (0,0) in CalcWindowScreenProperties
    TScBmBm       = SurfaceScreens(ScNum)%BmBmTrans
    TScBmBmVis    = SurfaceScreens(ScNum)%BmBmTransVis
    TScBmDif      = SurfaceScreens(ScNum)%BmDifTrans
    TScBmDifVis   = SurfaceScreens(ScNum)%BmDifTransVis
    TDif          = Construct(ConstrNumBare)%TransDiff
    TDifVis       = Construct(ConstrNumBare)%TransDiffVis
    RScBack       = SurfaceScreens(ScNum)%ReflectScreen
    RScBackVis    = SurfaceScreens(ScNum)%ReflectScreenVis
    RScDifBack    = SurfaceScreens(ScNum)%DifReflect
    RScDifBackVis = SurfaceScreens(ScNum)%DifReflectVis
    RGlFront      = POLYF(1.0d0,Construct(ConstrNumBare)%ReflSolBeamFrontCoef(1:6))
    RGlFrontVis   = POLYF(1.0d0,Construct(ConstrNumBare)%ReflSolBeamFrontCoef(1:6))
    RGlDiffFront  = Construct(ConstrNumBare)%ReflectSolDiffFront
    RGlDiffFrontVis = Construct(ConstrNumBare)%ReflectVisDiffFront
    TSolNorm = TScBmBm * (TBmBm + TDif*RGlFront*RScBack/(1-RGlDiffFront*RScDifBack)) +  &
                              TScBmDif*TDif/(1-RGlDiffFront*RScDifBack)
    TVisNorm = TScBmBmVis * (TBmBmVis + TDifVis*RGlFrontVis*RScBackVis/(1-RGlDiffFrontVis*RScDifBackVis)) +  &
                              TScBmDifVis*TDifVis/(1-RGlDiffFrontVis*RScDifBackVis)
  ELSE
    VarSlats = .FALSE.
    IF(Blind(BlNum)%SlatAngleType == VariableSlats) VarSlats = .TRUE.
    SlatAng = Blind(BlNum)%SlatAngle * DegToRadians
    TBlBmBm     = BlindBeamBeamTrans(0.0d0,SlatAng,Blind(BlNum)%SlatWidth,Blind(BlNum)%SlatSeparation, &
                                     Blind(BlNum)%SlatThickness)
    TBmBmBl     = TBmBm * TBlBmBm
    TBmBmBlVis  = TBmBmVis * TBlBmBm
    TBlBmDif    = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffTrans)
    TBlBmDifVis = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%VisFrontBeamDiffTrans)
    TDif        = Construct(ConstrNumBare)%TransDiff
    TDifVis     = Construct(ConstrNumBare)%TransDiffVis
    IF(ShadeFlag == IntBlindOn) THEN
      RGlDiffBack = Construct(ConstrNumBare)%ReflectSolDiffBack
      RGlDiffBackVis = Construct(ConstrNumBare)%ReflectVisDiffBack
      RhoBlFront = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffRefl)
      RhoBlFrontVis = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%VisFrontBeamDiffRefl)
      AbsBlFront = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamAbs)
      RhoBlDiffFront = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffRefl)
      RhoBlDiffFrontVis = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%VisFrontDiffDiffRefl)
      AbsBlDiffFront = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffAbs)
      AbsBeamShadeNorm = TBmBm * (AbsBlFront + &
        RhoBlFront*RGlDiffBAck*AbsBlDiffFront/(1.d0-RhoBlDiffFront*RGlDiffBack))
      TBlDifDif     = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
      TBlDifDifVis  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%VisFrontDiffDiffTrans)
      TSolNorm = TBmBm*( TBlBmBm + TBlBmDif + TBlDifDif*RhoBlFront*RGlDiffBack/(1.d0-RhoBlDiffFront*RGlDiffBack) )
!     use of TBlBmBm here is correct, visible and IR transmittance are the same (reference deleted CR6925 on 3/20/2006)
      TVisNorm = TBmBmVis*( TBlBmBm + TBlBmDifVis + TBlDifDifVis*RhoBlFrontVis*RGlDiffBackVis/ &
                            (1.d0-RhoBlDiffFrontVis*RGlDiffBackVis) )
    END IF ! (IntBlind)
    IF(ShadeFlag == ExtBlindOn) THEN
      TBlBmBm = BlindBeamBeamTrans(0.0d0,SlatAng,Blind(BlNum)%SlatWidth,Blind(BlNum)%SlatSeparation, &
                                  Blind(BlNum)%SlatThickness)
      RGlFront = POLYF(1.0d0,Construct(ConstrNumBare)%ReflSolBeamFrontCoef(1:6))
      RGlFrontVis = POLYF(1.0d0,Construct(ConstrNumBare)%ReflSolBeamFrontCoef(1:6))
      AbsBlFront = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamAbs)
      AbsBlBack = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamAbs)
      AbsBlDiffBack = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffAbs)
      RGlDiffFront = Construct(ConstrNumBare)%ReflectSolDiffFront
      RGlDiffFrontVis = Construct(ConstrNumBare)%ReflectVisDiffFront
      RhoBlDiffBack = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffDiffRefl)
      RhoBlDiffBackVis = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%VisBackDiffDiffRefl)
      RhoBlBack = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffRefl)
      RhoBlBackVis = InterpProfSlatAng(0.0d0,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffRefl)
      AbsBeamShadeNorm = AbsBlFront + AbsBlBack * RGlFront * TBlBmBm &
        + (AbsBlDiffBack*RGlDiffFront/(1.d0-RhoBlDiffBack*RGlDiffFront)) * &
          (RGlFront*TBlBmBm*RhoBlBack + TBlBmDif)
      RGlDiffFront = Construct(ConstrNumBare)%ReflectSolDiffFront
      TSolNorm = TBlBmBm * (TBmBm + TDif*RGlFront*RhoBlBack/(1-RGlDiffFront*RhoBlDiffBack)) +  &
                              TBlBmDif*TDif/(1.d0-RGlDiffFront*RhoBlDiffBack)
      TVisNorm = TBlBmBm * (TBmBmVis + TDifVis*RGlFrontVis*RhoBlBackVis/(1-RGlDiffFrontVis*RhoBlDiffBackVis)) +  &
                              TBlBmDifVis*TDifVis/(1.d0-RGlDiffFrontVis*RhoBlDiffBackVis)
    END IF ! (ExtBlind)
  END IF ! (Screen or Blind)
END IF ! (Shade, Blind, or Screen)

! Fill the layer properties needed for the thermal calculation.

! The layer and face numbering are as follows (for the triple glazing case):
! Glass layers are 1,2 and 3, where 1 is the outside (outside environment facing)
!   layer and 3 is the inside (room-facing) layer;
! Faces (also called surfaces) are 1,2,3,4,5 and 6, where face 1 is the
!   outside (front) face of glass layer 1, face 2 is the inside (back)
!   face of glass layer 1, face 3 is the outer face of glass layer 2, face 4 is the
!   inner face of glass layer 2, etc.
! Gap layers are 1 and 2, where gap layer 1 is between glass layers 1 and 2
!   and gap layer 2 is between glass layers 2 and 3.

IGlass = 0
IGap = 0

DO Lay = 1,TotLay
  LayPtr = Construct(ConstrNum)%LayerPoint(Lay)
  IF(( Material(LayPtr)%Group == WindowGlass) .OR. (Material(LayPtr)%Group == WindowSimpleGlazing) ) THEN
    IGlass = IGlass + 1
    thick(IGlass) =    Material(LayPtr)%Thickness
    scon(IGlass) =     Material(LayPtr)%Conductivity/Material(LayPtr)%Thickness
    emis(2*IGlass-1) = Material(LayPtr)%AbsorpThermalFront
    emis(2*IGlass) =   Material(LayPtr)%AbsorpThermalBack
    tir(2*IGlass-1) =  Material(LayPtr)%TransThermal
    tir(2*IGlass) =    Material(LayPtr)%TransThermal
    AbsBeamNorm(IGlass) = POLYF(1.0d0,Construct(ConstrNum)%AbsBeamCoef(IGlass,1:6))
    IF(ShadeFlag == IntBlindOn) THEN       ! Interior blind on
      AbsBeamNorm(IGlass) = POLYF(1.0d0,Construct(ConstrNumBare)%AbsBeamCoef(IGlass,1:6))
      AGlDiffBack = Construct(ConstrNumBare)%AbsDiffBack(IGlass)
      AbsBeamNorm(IGlass) = AbsBeamNorm(IGlass) + TBmBm*AGlDiffBack*RhoBlFront/(1.d0-RhoBlFront*RGlDiffBack)
    ELSE IF(ShadeFlag == ExtBlindOn) THEN  ! Exterior blind on
      AbsBeamNorm(IGlass) = POLYF(1.0d0,Construct(ConstrNumBare)%AbsBeamCoef(IGlass,1:6))
      AbsBeamNorm(IGlass) = TBlBmBm*AbsBeamNorm(IGlass) + (TBlBmBm*RGlFront*RhoBlBack + TBlBmDif) *  &
                                Construct(ConstrNumBare)%AbsDiff(IGlass)/(1.d0-RGlDiffFront*RhoBlDiffBack)
    ELSE IF(ShadeFlag == ExtScreenOn) THEN  ! Exterior screen on
      AbsBeamNorm(IGlass) = POLYF(1.0d0,Construct(ConstrNumBare)%AbsBeamCoef(IGlass,1:6))
      AbsBeamNorm(IGlass) = TScBmBm*AbsBeamNorm(IGlass) + (TScBmBm*RGlFront*RScBack + TScBmDif) *  &
                                Construct(ConstrNumBare)%AbsDiff(IGlass)/(1.d0-RGlDiffFront*RScDifBack)
    END IF
    AbsRadGlassFace(2*IGlass-1) = 0.5d0*BeamSolarInc*AbsBeamNorm(IGlass)
    AbsRadGlassFace(2*IGlass)   = 0.5d0*BeamSolarInc*AbsBeamNorm(IGlass)
  END IF
  IF(Material(LayPtr)%Group == WindowGas .OR. Material(LayPtr)%Group == WindowGasMixture .OR. &
    Material(LayPtr)%Group == ComplexWindowGap) THEN  ! Gap layer
    IGap = IGap + 1
    !Simon: Need to re-reference gas data in casee of complex fenestration gap
    IF(Material(LayPtr)%Group == ComplexWindowGap) THEN
      LayPtr = Material(LayPtr)%GasPointer
    END IF
    gap(IGap)    = Material(LayPtr)%Thickness
    gnmix(IGap)  = Material(LayPtr)%NumberOfGasesInMixture
    DO IMix = 1,gnmix(IGap)
      gwght(IGap,IMix)  = Material(LayPtr)%GasWght(IMix)
      gfract(IGap,IMix) = Material(LayPtr)%GasFract(IMix)
      DO ICoeff = 1,3
        gcon(IGap,IMix,ICoeff) = Material(LayPtr)%GasCon(IMix,ICoeff)
        gvis(IGap,IMix,ICoeff) = Material(LayPtr)%GasVis(IMix,ICoeff)
        gcp(IGap,IMix,ICoeff)  = Material(LayPtr)%GasCp(IMix,ICoeff)
      END DO
    END DO
  END IF
END DO

! Factors used in glass temperature solution
IF(ngllayer >= 2) THEN
  A23P = -emis(3)/(1.0d0-(1.0d0-emis(2))*(1.0d0-emis(3)))
  A32P = emis(2)/(1.0d0-(1.0d0-emis(2))*(1.0d0-emis(3)))
  A23 = emis(2)*sigma*A23P
END IF

IF(ngllayer >= 3) THEN
  A45P = -emis(5)/(1.0d0-(1.0d0-emis(4))*(1.0d0-emis(5)))
  A54P = emis(4)/(1.0d0-(1.0d0-emis(4))*(1.0d0-emis(5)))
  A45 = emis(4)*sigma*A45P
END IF

IF(ngllayer == 4) THEN
  A67P = -emis(7)/(1.0d0-(1.0d0-emis(6))*(1.0d0-emis(7)))
  A76P = emis(6)/(1.0d0-(1.0d0-emis(6))*(1.0d0-emis(7)))
  A67 = emis(6)*sigma*A67P
END IF

thetas = 0.0d0

CALL WindowTempsForNominalCond(ConstrNum,hgap)

! Get center-of-glass conductance and solar heat gain coefficient
! including inside and outside air films

hOutRad = emis(1)*sigma*0.5d0*(tout+thetas(1))**3
rOut = 1.0d0/(hOutRad + hcout)
hInRad = emis(nglface)*sigma*0.5d0*(tin+thetas(nglface))**3
rIn = 1.0d0/(hInRad + hcin)

IF (.not. (ShadeFlag == IntShadeOn .OR. ShadeFlag == IntBlindOn)) AbsBeamShadeNorm=0.0d0

SELECT CASE(ngllayer)

  CASE(1)
    Rbare = 1.0D0/scon(1)
    Rtot = rOut + Rbare + rIn
    SHGC = AbsBeamNorm(1) * (rOut + (0.5d0/scon(1)))/Rtot ! BG changed for CR7682 (solar absorbed in middle of layer)
    SHGC = SHGC + AbsBeamShadeNorm
    SHGC = SHGC + TSolNorm

  CASE(2)
    hGapTot(1) = hgap(1) + ABS(A23)*0.5d0*(thetas(2)+thetas(3))**3
    Rbare = 1.0D0/scon(1) + 1.0D0/hGapTot(1) + 1.0D0/scon(2)
    Rtot = rOut + Rbare + rIn
    SHGC = AbsBeamNorm(1)*(rOut + 0.5D0/scon(1))/Rtot &
           + AbsBeamNorm(2)*(rOut + 1.0D0/scon(1) + 1.0D0/hgapTot(1) + 0.5D0/scon(2) )/Rtot !CR7682
    SHGC = SHGC + AbsBeamShadeNorm
    SHGC = SHGC + TSolNorm

  CASE(3)
    hGapTot(1) = hgap(1) + ABS(A23)*0.5d0*(thetas(2)+thetas(3))**3
    hGapTot(2) = hgap(2) + ABS(A45)*0.5d0*(thetas(4)+thetas(5))**3
    Rbare = 1.0D0/scon(1) + 1.0D0/hGapTot(1) + 1.0D0/scon(2) + 1.0D0/hGapTot(2) + 1.0D0/scon(3)
    Rtot = rOut + Rbare + rIn
    SHGC = AbsBeamNorm(1)*(rOut + 0.5D0/scon(1))/Rtot  &
          + AbsBeamNorm(2)*(rOut + 1.0D0/scon(1) + 1.0D0/hgapTot(1) + 0.5D0/scon(2) )/Rtot   &
          + AbsBeamNorm(3)*(rOut + 1.0D0/scon(1) + 1.0D0/hgapTot(1) + 1.0D0/scon(2) + 1.0D0/hGapTot(2) + 0.5D0/scon(3) )/Rtot
    SHGC = SHGC + AbsBeamShadeNorm
    SHGC = SHGC + TSolNorm

  CASE(4)
    hGapTot(1) = hgap(1) + ABS(A23)*0.5d0*(thetas(2)+thetas(3))**3
    hGapTot(2) = hgap(2) + ABS(A45)*0.5d0*(thetas(4)+thetas(5))**3
    hGapTot(3) = hgap(3) + ABS(A67)*0.5d0*(thetas(6)+thetas(7))**3
    Rbare = 1.0D0/scon(1) + 1.0D0/hGapTot(1) + 1.0D0/scon(2) + 1.0D0/hGapTot(2) + 1.0D0/scon(3) +  &
                         1.0D0/hGapTot(3) + 1.0D0/scon(4)
    Rtot = rOut + Rbare + rIn
    SHGC = AbsBeamNorm(1)*(rOut + 0.5D0/scon(1))/Rtot &
           + AbsBeamNorm(2)*(rOut + 1.0D0/scon(1) + 1.0D0/hgapTot(1) + 0.5D0/scon(2) )/Rtot   &
           + AbsBeamNorm(3)*(rOut + 1.0D0/scon(1) + 1.0D0/hgapTot(1) + 1.0D0/scon(2) + 1.0D0/hGapTot(2) + 0.5D0/scon(3) )/Rtot &
           + AbsBeamNorm(4)*(rOut + 1.0D0/scon(1) + 1.0D0/hgapTot(1) + 1.0D0/scon(2) + 1.0D0/hGapTot(2) + 1.0D0/scon(3) &
                              + 1.0D0/hGapTot(3) + 0.5D0/scon(4) )/Rtot  !CR7682
    SHGC = SHGC + AbsBeamShadeNorm
    SHGC = SHGC + TSolNorm

 END SELECT

NominalConductance = 1.0D0/(rOut + Rbare + rIn) !Objexx:Uninit Rbare uninitialized if ngllayer>4
!EPTeam - again -- believe that is enforced in input

RETURN
END SUBROUTINE CalcNominalWindowCond
!****************************************************************************

SUBROUTINE WindowTempsForNominalCond(ConstrNum,hgap)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Nov 2002, FW: increase MaxIterations from 15 to 100, add face
          !                       temperature relaxation, and increase convergence tolerance by
          !                       a factor of 10 if no convergence after MaxIterations,
          !                       all for consistency with SolveForWindowTemperatures.
          !                      Mar 2003, FW: increase convergence tolerance from 0.01 to 0.02;
          !                       remove redundant relaxation on radiative conductances (both of
          !                       these were also done in SolveForWindowTemperatures).
          !                      Jan 2009, BG: changed interior convection coefficient correlation to match
          !                       ISO 15099.
          !                      Feb 2009, BG: extended coefficient to include absorbed radiation
          !                       to cover summer conditions for SHGC determination.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is a shortened form of SolveForWindowTemperatures tailored
          ! for calculation of the nominal center-of-glass U-value for a window
          ! construction at ASHRAE winter conditions and for determining conditions at
          ! summer conditions for calculationg SHGC.
          !
          ! Evaluates the coefficients Aface and Bface in the system of linear
          ! algebraic equations
          !
          !     Sum    [Aface(i,j)*thetas(j)] = Bface(i), i = 1,nglface
          !  j=1,nglface
          !
          ! where
          !
          ! nglface = number of glass faces (= 2 * number of layers) and
          ! thetas(j) = temperature of face j

          ! METHODOLOGY EMPLOYED:
          ! The Aface and Bface coefficients are determined by the equations for
          ! heat balance at the glass faces. The system of linear equations is solved
          ! by LU decomposition.

          ! REFERENCES:
          ! na
  USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW
!unused0909  USE DataEnvironment, ONLY: StdBaroPress
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER           :: ConstrNum             ! Construction number
REAL(r64)         :: hgap(5)               ! Gap gas conductive conductance (W/m2-K)

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: MaxIterations = 100   ! Maximum allowed number of iterations
REAL(r64), PARAMETER    :: errtemptol = 0.02d0     ! Tolerance on errtemp for convergence

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER    :: i                     ! Counter
REAL(r64)  :: gr                    ! Grashof number of gas in a gap
REAL(r64)  :: con                   ! Gap gas conductivity
REAL(r64)  :: pr                    ! Gap gas Prandtl number
REAL(r64)  :: nu                    ! Gap gas Nusselt number
REAL(r64)  :: hr(10)                ! Radiative conductance (W/m2-K)
REAL(r64)  :: hrprev(10)            ! Value of hr from previous iteration
REAL(r64)  :: hcinprev              ! Value of hcin from previous iteration
REAL(r64)  :: d                     ! +1 if number of row interchanges is even,
                                    ! -1 if odd (in LU decomposition)
INTEGER    :: indx(10)              ! Vector of row permutations in LU decomposition
REAL(r64)  :: Aface(10,10)            ! Coefficient in equation Aface*thetas = Bface
REAL(r64)  :: Bface(10)             ! Coefficient in equation Aface*thetas = Bface
INTEGER    :: iter                  ! Iteration number
REAL(r64)  :: errtemp               ! Absolute value of sum of face temperature differences
                                           !   between iterations, divided by number of faces
REAL(r64)  :: TmeanFilm  ! mean film temperature
REAL(r64)  :: TmeanFilmKelvin !  mean film temperature for property evaluation
REAL(r64)  :: rho     ! density of (apparently dry) air [kg/m3]
REAL(r64)  :: g       ! acceleration due to gravity [m/s2]
REAL(r64)  :: Height  ! window cavity height [m]
REAL(r64)  :: Cp      ! specific heat of air [J/kg-K]
REAL(r64)  :: lambda  ! thermal conductivity of air [W/m-K]
REAL(r64)  :: mu      ! dynamic viscosity of air [kg/m-s]
REAL(r64)  :: RaH     ! Rayleigh number for cavity height [ Non dim]
REAL(r64)  :: tiltDeg ! glazing tilt in degrees
REAL(r64)  :: sineTilt ! sine of glazing tilt
REAL(r64)  :: Nuint    ! Nusselt number for interior surface convection

iter = 0

! Initialize face temperatures
CALL StartingWinTempsForNominalCond

! Calculate radiative conductance
errtemp=errtemptol*2.0d0

TiltDeg= 90.0D0

sineTilt = SIN(tiltDeg*DegToRadians)  !degrees as arg


DO WHILE (iter < MaxIterations .and. errtemp > errtemptol)
  DO i = 1,nglface
    hr(i) = emis(i) * sigma * thetas(i)**3
      !!fw 3/4/03 if(iter >= 1) hr(i) = 0.5*(hrprev(i)+hr(i))
    hrprev(i) = hr(i)
  END DO

  Aface = 0.0d0
  Bface = 0.0d0

  ! Inside convective film conductance for vertical window
  if (iter >= 1) then
    hcinprev = hcin
  endif
  ! CR7670 BG this next correlation was used for hcin but is not "standard" for windows
  !  hcin = 1.31d0*((ABS(thetas(nglface)-tin))**0.3333d0)
  ! Begin calculating for ISO 15099 method.
  ! mean film temperature
  TmeanFilmKelvin = tin + 0.25D0*(thetas(nglface) - tin) ! eq. 133 in ISO 15099
  TmeanFilm = TmeanFilmKelvin - 273.15D0
  ! the following properties are constants or linear relations for "standard" type reporting
  rho    = PsyRhoAirFnPbTdbW(101325.0D0, TmeanFilm, 0.0D0, 'WindowTempsForNominalCond') ! dry air assumption
  g      = 9.81D0
  Height = 1.0D0 ! standard window rating practice is to use 1 meter (rather than actual)

  lambda = 2.873D-3 + 7.76D-5   * TmeanFilmKelvin ! Table B.1 in ISO 15099
  mu     = 3.723D-6 + 4.94D-8   * TmeanFilmKelvin ! Table B.2 in ISO 15099
  Cp     = 1002.737D0 + 1.2324D-2 * TmeanFilmKelvin ! Table B.3 in ISO 15099

  RaH = ( rho**2 * Height**3 * g * Cp*(ABS(thetas(nglface)-tin) ) ) &
           / (TmeanFilmKelvin * mu * lambda) ! eq 132 in ISO 15099

  Nuint = 0.56D0*(RaH * sineTilt)**0.25D0  ! eq. 135 in ISO 15099 (only need this one because tilt is 90 deg

  hcin  = Nuint * lambda / Height

  ! End calculations for ISO 15099 method.

  if(iter >= 1) hcin = 0.5D0*(hcinprev+hcin)

  iter = iter + 1

  SELECT CASE(ngllayer)

    CASE(1)
      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = rmir*emis(2) + hcin*tin    + AbsRadGlassFace(2)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)
      Aface(2,1) = -scon(1)
      Aface(2,2) = hr(2) + scon(1) + hcin

    CASE(2)
      call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
      call NusseltNumber(0,thetas(2),thetas(3),1,gr,pr,nu)
      hgap(1) = con/gap(1)*nu

      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = AbsRadGlassFace(2)
      Bface(3) = AbsRadGlassFace(3)
      Bface(4) = rmir*emis(4) + hcin*tin + AbsRadGlassFace(4)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)

      Aface(2,1) = -scon(1)
      Aface(2,2) = scon(1) + hgap(1) - A23P*hr(2)
      Aface(2,3) = -hgap(1) - A32P*hr(3)

      Aface(3,2) = -hgap(1) + A23P*hr(2)
      Aface(3,3) = hgap(1) + scon(2) + A32P*hr(3)
      Aface(3,4) = -scon(2)

      Aface(4,3) = -scon(2)
      Aface(4,4) = hr(4) + scon(2) + hcin

    CASE(3)
      call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
      call NusseltNumber(0,thetas(2),thetas(3),1,gr,pr,nu)
      hgap(1) = con/gap(1)*nu

      call WindowGasConductance(thetas(4),thetas(5),2,con,pr,gr)
      call NusseltNumber(0,thetas(4),thetas(5),2,gr,pr,nu)
      hgap(2) = con/gap(2)*nu

      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = AbsRadGlassFace(2)
      Bface(3) = AbsRadGlassFace(3)
      Bface(4) = AbsRadGlassFace(4)
      Bface(5) = AbsRadGlassFace(5)
      Bface(6) = rmir*emis(6) + hcin*tin + AbsRadGlassFace(6)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)

      Aface(2,1) = -scon(1)
      Aface(2,2) = scon(1) + hgap(1) - A23P*hr(2)
      Aface(2,3) = -hgap(1) - A32P*hr(3)

      Aface(3,2) = -hgap(1) + A23P*hr(2)
      Aface(3,3) = hgap(1) + scon(2) + A32P*hr(3)
      Aface(3,4) = -scon(2)

      Aface(4,3) = -scon(2)
      Aface(4,4) = scon(2) + hgap(2) - A45P*hr(4)
      Aface(4,5) = -hgap(2) - A54P*hr(5)

      Aface(5,4) = -hgap(2) + A45P*hr(4)
      Aface(5,5) = hgap(2) + scon(3) + A54P*hr(5)
      Aface(5,6) = -scon(3)

      Aface(6,5) = -scon(3)
      Aface(6,6) = hr(6) + scon(3) + hcin

    CASE(4)
      call WindowGasConductance(thetas(2),thetas(3),1,con,pr,gr)
      call NusseltNumber(0,thetas(2),thetas(3),1,gr,pr,nu)
      hgap(1) = con/gap(1)*nu

      call WindowGasConductance(thetas(4),thetas(5),2,con,pr,gr)
      call NusseltNumber(0,thetas(4),thetas(5),2,gr,pr,nu)
      hgap(2) = con/gap(2)*nu

      call WindowGasConductance(thetas(6),thetas(7),3,con,pr,gr)
      call NusseltNumber(0,thetas(6),thetas(7),3,gr,pr,nu)
      hgap(3) = con/gap(3)*nu

      Bface(1) = outir*emis(1) + hcout*tout + AbsRadGlassFace(1)
      Bface(2) = AbsRadGlassFace(2)
      Bface(3) = AbsRadGlassFace(3)
      Bface(4) = AbsRadGlassFace(4)
      Bface(5) = AbsRadGlassFace(5)
      Bface(6) = AbsRadGlassFace(6)
      Bface(7) = AbsRadGlassFace(7)
      Bface(8) = rmir*emis(8) + hcin*tin + AbsRadGlassFace(8)

      Aface(1,1) = hr(1) + scon(1) + hcout
      Aface(1,2) = -scon(1)

      Aface(2,1) = -scon(1)
      Aface(2,2) = scon(1) + hgap(1) - A23P*hr(2)
      Aface(2,3) = -hgap(1) - A32P*hr(3)

      Aface(3,2) = -hgap(1) + A23P*hr(2)
      Aface(3,3) = hgap(1) + scon(2) + A32P*hr(3)
      Aface(3,4) = -scon(2)

      Aface(4,3) = -scon(2)
      Aface(4,4) = scon(2) + hgap(2) - A45P*hr(4)
      Aface(4,5) = -hgap(2) - A54P*hr(5)

      Aface(5,4) = -hgap(2) + A45P*hr(4)
      Aface(5,5) = hgap(2) + scon(3) + A54P*hr(5)
      Aface(5,6) = -scon(3)

      Aface(6,5) = -scon(3)
      Aface(6,6) = scon(3) + hgap(3) - A67P*hr(6)
      Aface(6,7) = -hgap(3) - A76P*hr(7)

      Aface(7,6) = -hgap(3) + A67P*hr(6)
      Aface(7,7) = hgap(3) + scon(4) + A76P*hr(7)
      Aface(7,8) = -scon(4)

      Aface(8,7) = -scon(4)
      Aface(8,8) = hr(8) + scon(4) + hcin

  END SELECT

  call LUdecomposition(Aface,nglface,indx,d)    ! Note that these routines change Aface;
  call LUsolution(Aface,nglface,indx,Bface)     ! face temperatures are returned in Bface

  errtemp = 0.0d0
  DO i = 1,nglface
    errtemp = errtemp + ABS(thetas(i)-Bface(i))/nglface
  END DO

  DO i = 1,nglface
    thetas(i) = 0.5d0*(thetas(i) + Bface(i))
  END DO

END DO

! No convergence after MaxIterations; and/or error tolerance
IF (errtemp >= 10*errtemptol) THEN
  ! Fatal error: didn't converge
  call ShowFatalError('Convergence error in WindowTempsForNominalCond for construction '&
    //TRIM(Construct(ConstrNum)%Name))
END IF

return
END SUBROUTINE WindowTempsForNominalCond
!****************************************************************************
SUBROUTINE StartingWinTempsForNominalCond

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes face temperature distribution prior to iteration.
          ! This is a shortened form of StartingWindowTemps for use in calculating
          ! the nominal center-of-glass U-value.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64),PARAMETER    :: hrad = 5.3d0                  ! Typical radiative conductance (W/m2-K)
REAL(r64),PARAMETER    :: hcinStartValue = 3.2d0        ! Starting value for inside air film convective
                                                 !   conductance (estimated for typical double glazing
                                                 !   using 1.31(dT**0.333), where dT =
                                                 !   room air temp - inside surface temp = 14.2K)
REAL(r64),PARAMETER    :: resgap = 0.21d0               ! Typical gap resistance (m2-K/W)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER           :: i                           ! Face counter
REAL(r64)         :: rguess(11)                  ! Combined radiative/convective resistance (m2-K/W) of
                                                 ! inside or outside air film, or gap
REAL(r64)         :: restot                      ! Total window resistance including outside
                                                 !   and inside air films (m2-K/W)
REAL(r64)         :: temdiff                     ! Inside/outside air temperature difference (K)
REAL(r64)         :: ressum                      ! Resistance sum (m2-K/W)

      rguess(1) = 1.0d0/(hcout+hrad)
      rguess(nglface+1) = 1.0d0/(hcinStartValue+hrad)

      do i = 2,nglface,2
        rguess(i) = 1.0d0/scon(i/2)
        if(i<nglface) rguess(i+1) = resgap
      end do
      restot = 0.0d0

      do i = 1,nglface+1
        restot = restot + rguess(i)
      enddo

      temdiff = tin - tout
      if(abs(temdiff)<0.5d0) temdiff = 2.0d0
      ressum = 0.0d0

      do i = 1,nglface
        ressum = ressum + rguess(i)
        thetas(i) = (ressum/restot)*temdiff + tout
      end do

   return
END SUBROUTINE StartingWinTempsForNominalCond
!****************************************************************************

SUBROUTINE ReportGlass

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gives a detailed report to the user about
          ! the calculation parameters for windows and their associated
          ! materials.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: POLYF, ScanForReports, RoundSigDigits
                     ! InterpBlind ! Blind profile angle interpolation function
  USE WindowComplexManager, ONLY: CalcComplexWindowThermal, UpdateComplexWindows

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*),PARAMETER,DIMENSION(6)  :: Roughness = (/'VeryRough   ','Rough       ','MediumRough ', &
                                                   'MediumSmooth','Smooth      ','VerySmooth  '/)
  CHARACTER(len=*),PARAMETER,DIMENSION(0:4):: GasTypeName=(/'Custom ','Air    ','Argon  ','Krypton','Xenon  '/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: DoReport=.false.
  LOGICAL :: HasWindows=.false.
  LOGICAL :: HasComplexWindows = .false.
  LOGICAL :: HasEQLWindows = .false.               ! equivalent layer window defined
  INTEGER :: SurfConstr = 0
  REAL(r64) :: TempVar = 0.0d0 ! just temporary usage for complex fenestration

  INTEGER ThisNum
  INTEGER Layer
  INTEGER BlNum                      ! Blind number
  INTEGER I
  REAL(r64) NominalConductanceWinter      ! Nominal center-of-glass conductance of a window construction
                                     ! for ASHRAE winter conditions (W/m2-K):
                                     ! Inside air temperature = 21.1C (70F)
                                     ! Outside air temperature = -17.8C (0F)
                                     ! Windspeed = 6.71 m/s (15 mph)
                                     ! No solar radiation
  REAL(r64) NominalConductanceSummer      ! Nominal center-of-glass conductance of a window construction
                                     ! for ASHRAE summer conditions (W/m2-K):
                                     ! Inside air temperature = 23.9C (75F)
                                     ! Outside air temperature = 35.0C (95F)
                                     ! Windspeed = 3.35 m/s (7.5 mph)
                                     ! 783 W/m2 (248 Btu/h-ft2) incident beam solar radiation normal to glazing
  REAL(r64) SHGCWinter, SHGCSummer        ! Center-of-glass solar heat gain coefficient for ASHRAE
                                     ! winter and summer conditions
  REAL(r64) TransSolNorm                  ! Window construction solar transmittance at normal incidence
  REAL(r64) TransVisNorm                  ! Window construction visible transmittance at normal incidence
  INTEGER ErrFlag                    ! Error flag
  CHARACTER(len=3) SolarDiffusing    ! 'Yes' if glass is solar diffusing; otherwise 'No' (clear glass)
  CHARACTER(len=MaxNameLength) :: SpectralDataName
  CHARACTER(len=MaxNameLength) :: OpticalDataType
  CHARACTER(len=MaxNameLength) :: SlateOrientation
  CHARACTER(len=MaxNameLength) :: GapVentType

  CALL ScanForReports('Constructions',DoReport,'Constructions')

!  DO ThisNum=1,TotConstructs
!    IF (.not. Construct(ThisNum)%TypeIsWindow) CYCLE
!    HasWindows=.true.
!    EXIT
!  ENDDO

  IF (ANY(Construct%TypeIsWindow)) HasWindows=.true.
  IF (ANY(Construct%WindowTypeBSDF)) HasComplexWindows=.true.  ! Yes, this is a bit different than actually using them.
  IF (ANY(Construct%WindowTypeEQL)) HasEQLWindows=.true.       ! for reporting purpose only

!  DO ThisNum=1,TotSurfaces
!    SurfConstr = Surface(ThisNum)%Construction
!    IF (SurfConstr /= 0) THEN
!      IF (Construct(SurfConstr)%WindowTypeBSDF) THEN
!        HasComplexWindows=.true.
!        EXIT
!      END IF
!    END IF
!  ENDDO

  IF (DoReport .and. (HasWindows .or. HasComplexWindows .OR. HasEQLWindows)) THEN
!
!
!                                      Write Descriptions

    Write(OutputFileInits,'(A)') '! <WindowConstruction>,Construction Name,Index,#Layers,'//  &
                             'Roughness,Conductance {W/m2-K},SHGC,'// &
                             'Solar Transmittance at Normal Incidence,Visible Transmittance at Normal Incidence'
    IF( (TotSimpleWindow > 0) .OR. (W5GlsMat > 0) .OR. (W5GlsMatAlt > 0) ) &
      Write(OutputFileInits, '(A)') '! <WindowMaterial:Glazing>, Material Name, Optical Data Type, Spectral Data Set Name, '// &
                               'Thickness {m}, Solar Transmittance,Front Solar Reflectance, Back Solar Reflectance, ' //&
                               'Visible Transmittance, Front Visible Reflectance,Back Visible Reflectance,' //  &
                               'Infrared Transmittance, Front Thermal Emissivity, Back Thermal Emissivity,' //  &
                               'Conductivity {W/m-K},Dirt Factor,Solar Diffusing'
    IF ( (W5GasMat > 0) .OR. (W5GasMatMixture > 0) ) &
      Write(OutputFileInits,'(A)') '! <WindowMaterial:Gas>,Material Name,GasType,Thickness {m}'
    IF(TotShades .GT. 0) &
      Write(OutputFileInits,'(A)') '! <WindowMaterial:Shade>,Material Name,Thickness {m},Conductivity {W/m-K},'//  &
                               'Thermal Absorptance,Transmittance,Visible Transmittance,Shade Reflectance'
    IF(TotScreens .GT. 0) &
      Write(OutputFileInits,'(A)') '! <WindowMaterial:Screen>,Material Name,Thickness {m},Conductivity {W/m-K},'//  &
                               'Thermal Absorptance,Transmittance,Reflectance,Visible Reflectance,'// &
                               'Diffuse Reflectance,Diffuse Visible Reflectance,'// &
                               'Screen Material Diameter To Spacing Ratio,Screen To GlassDistance {m}'
    IF(TotBlinds .GT. 0) &
      Write(OutputFileInits,'(A)') '! <WindowMaterial:Blind>,Material Name,Slat Width {m},Slat Separation {m},'//  &
                               'Slat Thickness {m},Slat Angle {deg},Slat Beam Solar Transmittance,'//  &
                               'Slat Beam Solar Front Reflectance,Blind To Glass Distance {m}'

    IF (HasComplexWindows)  Write(OutputFileInits,'(A)') '! <WindowConstruction:Complex>,Construction Name,Index,#Layers,'//  &
                            'U-factor {W/m2-K},SHGC'

    IF (HasEQLWindows)  &
      Write(OutputFileInits,'(A)') '! <Construction:WindowEquivalentLayer>,Construction Name,Index,#Layers,'//  &
                            'U-factor {W/m2-K},SHGC, Solar Transmittance at Normal Incidence'
    IF( W5GlsMatEQL .GT. 0) &
      Write(OutputFileInits, '(A)') '! <WindowMaterial:Glazing:EquivalentLayer>, '// &
                               'Material Name, Optical Data Type, Spectral Data Set Name, '// &
                               'Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, '//&
                               'Front Side Beam-Beam Solar Reflectance, Back Side Beam-Beam Solar Reflectance, '//&
                               'Front Side Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , '//&
                               'Front Side Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, '//&
                               'Diffuse-Diffuse Solar Transmittance, Front Side Diffuse-Diffuse Solar Reflectance, '//&
                               'Back Side Diffuse-Diffuse Solar Reflectance, Infrared Transmittance, '//&
                               'Front Side Infrared Emissivity, Back Side Infrared Emissivity'
    IF( TotShadesEQL .GT. 0) &
      Write(OutputFileInits, '(A)') '! <WindowMaterial:Shade:EquivalentLayer>, '// &
                               'Material Name, '// &
                               'Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, '//&
                               'Front Side Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , '//&
                               'Front Side Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, '//&
                               'Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared Emissivity'

    IF( TotDrapesEQL .GT. 0) &
      Write(OutputFileInits, '(A)') '! <WindowMaterial:Drape:EquivalentLayer>, '// &
                               'Material Name, '// &
                               'Front Side Beam-Beam Solar Transmittance, Back Side Beam-Beam Solar Transmittance, '//&
                               'Front Side Beam-Diffuse Solar Transmittance, Back Side Beam-Diffuse Solar Transmittance, , '//&
                               'Front Side Beam-Diffuse Solar Reflectance, Back Side Beam-Diffuse Solar Reflectance, '//&
                               'Infrared Transmittance, Front Side Infrared Emissivity, Back Side Infrared Emissivity, '//&
                               'Width of Pleated Fabric, Length of Pleated Fabric'

    IF( TotBlindsEQL .GT. 0) &
      Write(OutputFileInits, '(A)') '! <WindowMaterial:Blind:EquivalentLayer>, '// &
                     'Material Name, '// &
                     'Slat Orientation, Slat Width, Slat Separation, Slat Crown, Slat Angle, '//&
                     'Front Side Slate Beam-Diffuse Solar Transmittance, Back Side Slate Beam-Diffuse Solar Transmittance, '//&
                     'Front Side Slate Beam-Diffuse Solar Reflectance, Back Side Slate Beam-Diffuse Solar Reflectance, '//&
                     'Slat Diffuse-Diffuse Solar Transmittance, Front Side Slat Diffuse-Diffuse Solar Reflectance, '//&
                     'Back Side Slat Diffuse-Diffuse Solar Reflectance, Infrared Transmittance, '//&
                     'Front Side Infrared Emissivity, Back Side Infrared Emissivity, Slat Angle Control'
    IF( TotScreensEQL .GT. 0) &
      Write(OutputFileInits, '(A)') '! <WindowMaterial:Screen:EquivalentLayer>, '// &
                     'Material Name, '// &
                     'Screen Beam-Beam Solar Transmittance, Screen Beam-Diffuse Solar Transmittance, '//&
                     'Screen Beam-Diffuse Solar Reflectance, Screen Infrared Transmittance, '//&
                     'Screen Infrared Emissivity, Screen Wire Spacing, Screen Wire Diameter'
    IF( W5GapMatEQL .GT. 0) &
      Write(OutputFileInits,'(A)')   &
         '! <WindowMaterial:Gap:EquivalentLayer>, Material Name, GasType, Gap Thickness {m}, Gap Vent Type'


    DO ThisNum=1,TotConstructs

      IF (Construct(ThisNum)%WindowTypeBSDF) THEN

        I = ThisNum
        CALL CalcComplexWindowThermal(0, I, TempVar, TempVar, TempVar, TempVar, winterCondition)
        CALL CalcComplexWindowThermal(0, I, TempVar, TempVar, TempVar, TempVar, summerCondition)

        Write(OutputFileInits,800) trim(Construct(ThisNum)%Name), trim(RoundSigDigits(ThisNum)), &
                                   trim(RoundSigDigits(Construct(ThisNum)%TotSolidLayers)), &
                                   trim(RoundSigDigits(NominalU(ThisNum),3)), &
                                   trim(RoundSigDigits(Construct(ThisNum)%SummerSHGC,3))

      ELSE IF (Construct(ThisNum)%TypeIsWindow) THEN
        ! Calculate for ASHRAE winter and summer conditions:
        ! (1) nominal center-of-glass conductance, including inside and outside air films,
        ! (2) solar heat gain coefficient (SHGC),
        ! (3) solar transmittance at normal incidence, and (4) visible transmittance at normal incidence.

        IF (Construct(ThisNum)%WindowTypeEQL) THEN
            ! for equivalent layer Window already calculated
            ! NominalU(ThisNum)=NominalConductanceWinter
            ! Save the SHGC for later use in tabular report IVRS
            ! Construct(ThisNum)%SummerSHGC = SHGCSummer
            Construct(ThisNum)%VisTransNorm =0.0d0   ! TODO list

            Write(OutputFileInits,799) trim(Construct(ThisNum)%Name), trim(RoundSigDigits(ThisNum)), &
                                       trim(RoundSigDigits(Construct(ThisNum)%TotSolidLayers)), &
                                       trim(RoundSigDigits(NominalU(ThisNum),3)), &
                                       trim(RoundSigDigits(Construct(ThisNum)%SummerSHGC,3)), &
                                       trim(RoundSigDigits(Construct(ThisNum)%SolTransNorm,3))

        ELSE

            CALL CalcNominalWindowCond(ThisNum,1,NominalConductanceWinter,SHGCWinter,TransSolNorm,TransVisNorm,ErrFlag)

            IF(ErrFlag == 1) THEN
              CALL ShowWarningError('Window construction '//TRIM(Construct(ThisNum)%Name)// &
                 ' has an interior or exterior blind')
              CALL ShowContinueError('but the corresponding construction without the blind cannot be found.')
              CALL ShowContinueError('The ReportGlass entry for this construction will not be printed in eplusout.eio.')
              CYCLE
            END IF

            ! Skip constructions with between-glass shade/blind until method is worked out to determine
            ! nominal conductance and SHGC.

            IF(ErrFlag == 2) THEN
              CALL ShowWarningError('Window construction '//TRIM(Construct(ThisNum)%Name)// &
                 ' has a between-glass shade or blind')
              CALL ShowContinueError('The ReportGlass entry for this construction will not be printed in eplusout.eio.')
              CYCLE
            END IF

            NominalU(ThisNum)=NominalConductanceWinter
            IF (.NOT. Construct(ThisNum)%WindowTypeEQL) THEN
               CALL CalcNominalWindowCond(ThisNum,2,NominalConductanceSummer,SHGCSummer,TransSolNorm,TransVisNorm,ErrFlag)
            ENDIF
            ! Save the SHGC for later use in tabular report IVRS
            Construct(ThisNum)%SummerSHGC = SHGCSummer
            Construct(ThisNum)%VisTransNorm = TransVisNorm

            Write(OutputFileInits,700) TRIM(Construct(ThisNum)%Name),trim(RoundSigDigits(ThisNum)),   &
                                     trim(RoundSigDigits(Construct(ThisNum)%TotLayers)),              &
                                     trim(Roughness(Construct(ThisNum)%OutsideRoughness)),            &
                                     trim(RoundSigDigits(NominalConductanceWinter,3)),                &
                                     trim(RoundSigDigits(SHGCSummer,3)),                              &
                                     trim(RoundSigDigits(TransSolNorm,3)),                            &
                                    trim(RoundSigDigits(TransVisNorm,3))
        ENDIF
    !    Write(OutputFileConstrainParams, 705)  TRIM(Construct(ThisNum)%Name), SHGCSummer ,TransVisNorm

 700  FORMAT(' WindowConstruction',8(',',A))
 702  FORMAT(' WindowMaterial:Gas',3(',',A))
 703  FORMAT(' WindowMaterial:Shade,',7(',',A))
 704  FORMAT(' WindowMaterial:Blind',8(',',A))
 706  FORMAT(' WindowMaterial:Screen',11(',',A))
 707  FORMAT(' WindowMaterial:Glazing',16(',',A))

 708  FORMAT(' WindowMaterial:Glazing:EquivalentLayer',17(',',A))
 709  FORMAT(' WindowMaterial:Shade:EquivalentLayer',10(',',A))
 710  FORMAT(' WindowMaterial:Drape:EquivalentLayer',11(',',A))
 711  FORMAT(' WindowMaterial:Screen:EquivalentLayer',11(',',A))
 712  FORMAT(' WindowMaterial:Blind:EquivalentLayer',16(',',A))
 713  FORMAT(' WindowMaterial:Gap:EquivalentLayer',4(',',A))
 799  FORMAT(' Construction:WindowEquivalentLayer',6(',',A))

        DO I=1,Construct(ThisNum)%TotLayers
          Layer=Construct(ThisNum)%LayerPoint(I)
          SELECT CASE (Material(Layer)%Group)
          CASE (WindowGas)
            Write(OutputFileInits,702) TRIM(Material(Layer)%Name),TRIM(GasTypeName(Material(Layer)%GasType(1))), &
                                             trim(RoundSigDigits(Material(Layer)%Thickness,3))

          !!fw CASE(WindowGasMixture)

          CASE (Shade)
            Write(OutputFileInits,703) TRIM(Material(Layer)%Name),                             &
                                       trim(RoundSigDigits(Material(Layer)%Thickness,3)),      &
                                       trim(RoundSigDigits(Material(Layer)%Conductivity,3)),   &
                                       trim(RoundSigDigits(Material(Layer)%AbsorpThermal,3)),  &
                                       trim(RoundSigDigits(Material(Layer)%Trans,3)),          &
                                       trim(RoundSigDigits(Material(Layer)%TransVis,3)),       &
                                       trim(RoundSigDigits(Material(Layer)%ReflectShade,3))

          CASE (WindowBlind)
            BlNum = Material(Layer)%BlindDataPtr
            Write(OutputFileInits,704) TRIM(Material(Layer)%Name),                                    &
                                       trim(RoundSigDigits(Blind(BlNum)%SlatWidth,4)),                &
                                       trim(RoundSigDigits(Blind(BlNum)%SlatSeparation,4)),           &
                                       trim(RoundSigDigits(Blind(BlNum)%SlatThickness,4)),            &
                                       trim(RoundSigDigits(Blind(BlNum)%SlatAngle,3)),                &
                                       trim(RoundSigDigits(Blind(BlNum)%SlatTransSolBeamDiff,3)),     &
                                       trim(RoundSigDigits(Blind(BlNum)%SlatFrontReflSolBeamDiff,3)), &
                                       trim(RoundSigDigits(Blind(BlNum)%BlindToGlassDist,3))
          CASE (Screen)
            IF(Material(Layer)%ScreenDataPtr .GT. 0)&
            Write(OutputFileInits,706) TRIM(Material(Layer)%Name),                                                        &
                      trim(RoundSigDigits(Material(Layer)%Thickness,5)),                                                  &
                      trim(RoundSigDigits(Material(Layer)%Conductivity,3)),                                               &
                      trim(RoundSigDigits(Material(Layer)%AbsorpThermal,3)),                                              &
                      trim(RoundSigDigits(SurfaceScreens(Material(Layer)%ScreenDataPtr)%BmBmTrans,3)),                    &
                      trim(RoundSigDigits(SurfaceScreens(Material(Layer)%ScreenDataPtr)%ReflectSolBeamFront,3)),          &
                      trim(RoundSigDigits(SurfaceScreens(Material(Layer)%ScreenDataPtr)%ReflectVisBeamFront,3)),          &
                      trim(RoundSigDigits(SurfaceScreens(Material(Layer)%ScreenDataPtr)%DifReflect,3)),                   &
                      trim(RoundSigDigits(SurfaceScreens(Material(Layer)%ScreenDataPtr)%DifReflectVis,3)),                &
                      trim(RoundSigDigits(SurfaceScreens(Material(Layer)%ScreenDataPtr)%ScreenDiameterToSpacingRatio,3)), &
                      trim(RoundSigDigits(Material(Layer)%WinShadeToGlassDist,3))


          CASE (WindowGlass, WindowSimpleGlazing)
            SolarDiffusing = 'No'
            IF(Material(Layer)%SolarDiffusing) SolarDiffusing = 'Yes'
            OpticalDataType = 'SpectralAverage'
            SpectralDataName = ' '
            IF (Material(Layer)%GlassSpectralDataPtr > 0) THEN
              OpticalDataType = 'Spectral'
              SpectralDataName = SpectralData(Material(Layer)%GlassSpectralDataPtr)%Name
            ENDIF
            Write(OutputFileInits,707) TRIM(Material(Layer)%Name),TRIM(OpticalDataType), Trim(SpectralDataName), &
                                       trim(RoundSigDigits(Material(Layer)%Thickness,5)),                        &
                                       trim(RoundSigDigits(Material(Layer)%Trans,5)),                            &
                                       trim(RoundSigDigits(Material(Layer)%ReflectSolBeamFront,5)),              &
                                       trim(RoundSigDigits(Material(Layer)%ReflectSolBeamBack,5)),               &
                                       trim(RoundSigDigits(Material(Layer)%TransVis,5)),                         &
                                       trim(RoundSigDigits(Material(Layer)%ReflectVisBeamFront,5)),              &
                                       trim(RoundSigDigits(Material(Layer)%ReflectVisBeamBack,5)),               &
                                       trim(RoundSigDigits(Material(Layer)%TransThermal,5)),                     &
                                       trim(RoundSigDigits(Material(Layer)%AbsorpThermalFront,5)),               &
                                       trim(RoundSigDigits(Material(Layer)%AbsorpThermalBack,5)),                &
                                       trim(RoundSigDigits(Material(Layer)%Conductivity,5)),                     &
                                       trim(RoundSigDigits(Material(Layer)%GlassTransDirtFactor,5)),             &
                                       trim(SolarDiffusing)

          CASE (GlassEquivalentLayer)
            OpticalDataType = 'SpectralAverage'
            SpectralDataName = ' '
            Write(OutputFileInits,708) TRIM(Material(Layer)%Name), &
                                       trim(OpticalDataType), trim(SpectralDataName), &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamBeam,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamBeam,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontBeamBeam,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackBeamBeam,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamDiff,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamDiff,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontBeamDiff,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackBeamDiff,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausDiffDiff,5)),       &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontDiffDiff,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackDiffDiff,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausThermal,5)),        &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalFront,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalBack,5))

          CASE (ShadeEquivalentLayer)
            Write(OutputFileInits,709) TRIM(Material(Layer)%Name),                                 &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamBeam,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamBeam,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamDiff,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamDiff,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontBeamDiff,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackBeamDiff,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausThermal,4)),        &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalFront,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalBack,4))

          CASE (DrapeEquivalentLayer)
            Write(OutputFileInits,710) TRIM(Material(Layer)%Name),                                 &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamBeam,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamDiff,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamDiff,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontBeamDiff,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackBeamDiff,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausThermal,4)),        &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalFront,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalBack,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%PleatedDrapeWidth,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%PleatedDrapeLength,5))

          CASE (ScreenEquivalentLayer)
              Write(OutputFileInits,711) TRIM(Material(Layer)%Name),                               &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamBeam,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamDiff,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamDiff,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontBeamDiff,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackBeamDiff,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausThermal,4)),        &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalFront,4)),  &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalBack,4)),   &
                                       trim(RoundSigDigits(Material(Layer)%ScreenWireSpacing,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%ScreenWireDiameter,5))

          CASE (BlindEquivalentLayer)
              SlateOrientation = 'Horizontal'
              IF (Material(Layer)%SlatOrientation == Vertical) THEN
                SlateOrientation = 'Vertical'
              ENDIF
              Write(OutputFileInits,712) TRIM(Material(Layer)%Name), &
                                       trim(SlateOrientation), &
                                       trim(RoundSigDigits(Material(Layer)%SlatWidth,5)),          &
                                       trim(RoundSigDigits(Material(Layer)%SlatSeparation,5)),     &
                                       trim(RoundSigDigits(Material(Layer)%SlatCrown,5)),          &
                                       trim(RoundSigDigits(Material(Layer)%SlatAngle,5)),          &
                                       trim(RoundSigDigits(Material(Layer)%TausFrontBeamDiff,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%TausBackBeamDiff,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontBeamDiff,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackBeamDiff,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausDiffDiff,5)),       &
                                       trim(RoundSigDigits(Material(Layer)%ReflFrontDiffDiff,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%ReflBackDiffDiff,5)),   &
                                       trim(RoundSigDigits(Material(Layer)%TausThermal,5)),        &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalFront,5)),  &
                                       trim(RoundSigDigits(Material(Layer)%EmissThermalBack,5))

          CASE (GapEquivalentLayer)
              GapVentType = 'Sealed'
              IF (Material(Layer)%GapVentType == 2) THEN
                GapVentType = 'VentedIndoor'
              ELSEIF(Material(Layer)%GapVentType == 3) THEN
                GapVentType = 'VentedOutdoor'
              ENDIF
              Write(OutputFileInits,713) TRIM(Material(Layer)%Name), &
                                       trim(GasTypeName(Material(Layer)%GasType(1))),      &
                                       trim(RoundSigDigits(Material(Layer)%Thickness,3)),  &
                                       trim(GapVentType)
          END SELECT
        ENDDO

      ENDIF

    ENDDO

  ELSEIF (HasWindows) THEN

    DO ThisNum=1,TotConstructs

      IF (.not. Construct(ThisNum)%TypeIsWindow) CYCLE
      IF (Construct(ThisNum)%WindowTypeEQL) CYCLE        ! skip if equivalent layer window

      ! Calculate for ASHRAE winter and summer conditions: (1)nominal center-of-glass conductance,
      ! (2) solar heat gain coefficient (SHGC), including inside and outside air films,
      ! (3) solar transmittance at normal incidence, and (4) visible transmittance at normal incidence.

      CALL CalcNominalWindowCond(ThisNum,1,NominalConductanceWinter,SHGCWinter,TransSolNorm,TransVisNorm,ErrFlag)

      IF(ErrFlag == 1 .OR. ErrFlag == 2) CYCLE
      NominalU(ThisNum)=NominalConductanceWinter

    ENDDO

  ENDIF

  !IF (HasComplexWindows) THEN
    !DO ThisNum=1,TotSurfaces
    !  SurfConstr = Surface(ThisNum)%Construction
    !  IF (SurfConstr /= 0) THEN
    !    IF (Construct(SurfConstr)%WindowTypeBSDF) THEN
    !      Write(OutputFileInits,'(A)') '! <WindowConstructionComplex>,Construction Name,Index,#Layers,'//  &
    !                         'U-factor {W/m2-K},SHGC'
    !      CALL CalcComplexWindowThermal(ThisNum, TempVar, TempVar, TempVar, TempVar, winterCondition)
    !      CALL CalcComplexWindowThermal(ThisNum, TempVar, TempVar, TempVar, TempVar, summerCondition)
    !
    !      Write(OutputFileInits,800) trim(Construct(SurfConstr)%Name), trim(RoundSigDigits(SurfConstr)), &
    !                                 trim(RoundSigDigits(Construct(SurfConstr)%TotSolidLayers)), &
    !                                 trim(RoundSigDigits(NominalU(SurfConstr),3)), &
    !                                 trim(RoundSigDigits(Construct(SurfConstr)%SummerSHGC,3))
    !    END IF
    !  END IF
    !ENDDO

  800  FORMAT(' WindowConstruction:Complex',5(',',A))
  !END IF

  RETURN

END SUBROUTINE ReportGlass
!*************************************************************************************

SUBROUTINE CalcWindowBlindProperties

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hans Simmler
          !       DATE WRITTEN   July-Aug 1995
          !       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
          !                      Dec 2001 (FCW): add variable slat angle
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates solar-optical properties of a window blind
          ! from slat properties and solar profile angle. Assumes flat slats.

          ! METHODOLOGY EMPLOYED:
          ! The solar profile angle is varied from -90 to +90 deg and slat angle is varied from 0 to 180deg,
          ! covering the full range of possible profile angles and slat angles.
          ! (The profile angle is defined as the angle of incidence when the radiation
          ! source is located in a plane that (1)is perpendicular to the  plane of the blinds [which is
          ! the same as the window plane] and (2) contains the slat normal vector.)

          ! In the time-step calculation,the blind properties vs. profile angle and slat angle
          ! that are calculated here will be applicable to windows and slats
          ! of arbitrary orientation, and to arbitrary sun positions, as long as the appropiate
          ! profile angle is used. The slat angle for a particular window with blinds is determined
          ! each time step in subroutine WindowShadingManager on the basis of user-specified
          ! slat control options.

          ! REFERENCES:
          ! "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
          ! F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

          ! USE STATEMENTS:na
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:na
          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

      REAL(r64)  bld_pr(15)           ! Slat properties
      REAL(r64)  st_lay(16)           ! Solar-optical blind/glazing system properties
      REAL(r64)  sun_el               ! Solar profile angle (radians)
      REAL(r64)  sun_el_deg(37)       ! Solar profile angle (deg) corresponding to sun_el values
      REAL(r64)  bld_el               ! Slat angle (elevation of slat normal vector in plane
                                      !  perpendicular to window and containing the slat normal vector) (radians)
      INTEGER    ISolVis              ! 1 = do solar and IR calculation; 2 = do visible calculation
      INTEGER    IProfAng             ! Profile angle index
      INTEGER    BlindNum             ! Blind number
      INTEGER    ISlatAng             ! Slat angle index

DO BlindNum = 1,TotBlinds

  BLD_PR(2)  = Blind(BlindNum)%SlatWidth
  BLD_PR(3)  = Blind(BlindNum)%SlatSeparation

  DO ISolVis = 1,2
    IF(ISolVis == 1) THEN  ! For solar calculation
      BLD_PR( 4) = 0.0d0
      BLD_PR( 5) = 0.0d0
      BLD_PR( 6) = 0.0d0
      BLD_PR( 7) = Blind(BlindNum)%SlatTransSolBeamDiff
      BLD_PR( 8) = Blind(BlindNum)%SlatFrontReflSolBeamDiff
      BLD_PR( 9) = Blind(BlindNum)%SlatBackReflSolBeamDiff
      BLD_PR(10) = Blind(BlindNum)%SlatTransSolDiffDiff
      BLD_PR(11) = Blind(BlindNum)%SlatFrontReflSolDiffDiff
      BLD_PR(12) = Blind(BlindNum)%SlatBackReflSolDiffDiff
    ELSE                   ! For visible calculation
      BLD_PR( 4) = 0.0d0
      BLD_PR( 5) = 0.0d0
      BLD_PR( 6) = 0.0d0
      BLD_PR( 7) = Blind(BlindNum)%SlatTransVisBeamDiff
      BLD_PR( 8) = Blind(BlindNum)%SlatFrontReflVisBeamDiff
      BLD_PR( 9) = Blind(BlindNum)%SlatBackReflVisBeamDiff
      BLD_PR(10) = Blind(BlindNum)%SlatTransVisDiffDiff
      BLD_PR(11) = Blind(BlindNum)%SlatFrontReflVisDiffDiff
      BLD_PR(12) = Blind(BlindNum)%SlatBackReflVisDiffDiff
    END IF
                           ! For IR calculation
    BLD_PR(13) = Blind(BlindNum)%SlatTransIR
    BLD_PR(14) = Blind(BlindNum)%SlatFrontEmissIR
    BLD_PR(15) = Blind(BlindNum)%SlatBackEmissIR

    ! Calculate diffuse properties of blind. If blind has variable slat angle, &
    ! vary slat angle from 0 to 180 deg in 10-deg steps (for MaxSlatAngs = 19).
    ! If blind has fixed slat angle, calculate properties at that angle only.

    DO ISlatAng = 1,MaxSlatAngs

      st_lay = 0.0d0
      IF(Blind(BlindNum)%SlatAngleType == FixedSlats) THEN
        bld_el = Blind(BlindNum)%SlatAngle * DegToRadians
      ELSE  ! Variable slat angle
        bld_el = (PI/(MaxSlatAngs-1))*(ISlatAng-1)  ! 0 <= bld_el <= 180 deg
      END IF
      CALL BlindOpticsDiffuse(BlindNum,ISolVis,BLD_PR,bld_el,st_lay)

      IF(ISolVis == 1) THEN  ! Fill blind diffuse solar and IR properties
        Blind(BlindNum)%SolFrontDiffDiffTrans(ISlatAng) = st_lay(9)
        Blind(BlindNum)%SolFrontDiffDiffRefl(ISlatAng)  = st_lay(10)
        Blind(BlindNum)%SolBackDiffDiffTrans(ISlatAng)  = st_lay(11)
        Blind(BlindNum)%SolBackDiffDiffRefl(ISlatAng)   = st_lay(12)
        Blind(BlindNum)%SolFrontDiffAbs(ISlatAng)       = MAX(0.0d0,1.d0-st_lay(9)-st_lay(10))
        Blind(BlindNum)%SolBackDiffAbs(ISlatAng)        = MAX(0.0d0,1.d0-st_lay(11)-st_lay(12))
        Blind(BlindNum)%IRFrontTrans(ISlatAng)          = st_lay(13)
        Blind(BlindNum)%IRFrontEmiss(ISlatAng)          = st_lay(14)
        !Blind(BlindNum)%IRBackTrans(ISlatAng)           = st_lay(15)
        !Blind(BlindNum)%IRBackEmiss(ISlatAng)           = st_lay(16)
        !  Above two lines are incorrect; replaced by (FCW, 2/10/03)
        Blind(BlindNum)%IRBackTrans(ISlatAng)           = st_lay(13)
        Blind(BlindNum)%IRBackEmiss(ISlatAng)           = st_lay(15)
      ELSE                   ! Fill blind diffuse visible properties
        Blind(BlindNum)%VisFrontDiffDiffTrans(ISlatAng) = st_lay(9)
        Blind(BlindNum)%VisFrontDiffDiffRefl(ISlatAng)  = st_lay(10)
        Blind(BlindNum)%VisBackDiffDiffTrans(ISlatAng)  = st_lay(11)
        Blind(BlindNum)%VisBackDiffDiffRefl(ISlatAng)   = st_lay(12)
      END IF

      IF(Blind(BlindNum)%SlatAngleType == FixedSlats) EXIT
    END DO  ! End of slat angle loop

    ! Calculate beam properties of blind. Vary profile angle from -90 to +90 deg in 5-deg steps.
    ! If blind has variable slat angle, vary slat angle from 0 to 180 deg in 10-deg steps
    ! (for MaxSlatAngs = 19). If blind has fixed slat angle, calculate properties at that angle only.

    DO IProfAng = 1,37
      sun_el = -Pi/2.d0 + (Pi/36.d0)*(IProfAng-1)
      sun_el_deg(IProfAng) = 57.2958d0 * sun_el

      DO ISlatAng = 1,MaxSlatAngs
        st_lay = 0.0d0
        IF(Blind(BlindNum)%SlatAngleType == FixedSlats) THEN
          bld_el = Blind(BlindNum)%SlatAngle * DegToRadians
        ELSE  ! Variable slat angle
          bld_el = (PI/(MaxSlatAngs-1))*(ISlatAng-1)  ! 0 <= bld_el <= 180 deg
        END IF

        ! Beam solar-optical properties of blind for given profile angle and slat angle

        CALL BlindOpticsBeam(BlindNum,bld_pr,bld_el,sun_el,st_lay)

        IF(ISolVis == 1) THEN  ! Fill blind beam solar properties
          Blind(BlindNum)%SolFrontBeamBeamTrans(IProfAng,ISlatAng) = st_lay(1)
          Blind(BlindNum)%SolFrontBeamBeamRefl(IProfAng,ISlatAng)  = st_lay(2)
          Blind(BlindNum)%SolBackBeamBeamTrans(IProfAng,ISlatAng)  = st_lay(3)
          Blind(BlindNum)%SolBackBeamBeamRefl(IProfAng,ISlatAng)   = st_lay(4)
          Blind(BlindNum)%SolFrontBeamDiffTrans(IProfAng,ISlatAng) = st_lay(5)
          Blind(BlindNum)%SolFrontBeamDiffRefl(IProfAng,ISlatAng)  = st_lay(6)
          Blind(BlindNum)%SolBackBeamDiffTrans(IProfAng,ISlatAng)  = st_lay(7)
          Blind(BlindNum)%SolBackBeamDiffRefl(IProfAng,ISlatAng)   = st_lay(8)
          Blind(BlindNum)%SolFrontBeamAbs(IProfAng,ISlatAng) = MAX(0.0d0,1.d0-st_lay(6)-st_lay(1)-st_lay(5))
          Blind(BlindNum)%SolBackBeamAbs(IProfAng,ISlatAng)  = MAX(0.0d0,1.d0-st_lay(7)-st_lay(3)-st_lay(8))

        ELSE                   ! Fill blind beam visible properties
          Blind(BlindNum)%VisFrontBeamBeamTrans(IProfAng,ISlatAng) = st_lay(1)
          Blind(BlindNum)%VisFrontBeamBeamRefl(IProfAng,ISlatAng)  = st_lay(2)
          Blind(BlindNum)%VisBackBeamBeamTrans(IProfAng,ISlatAng)  = st_lay(3)
          Blind(BlindNum)%VisBackBeamBeamRefl(IProfAng,ISlatAng)   = st_lay(4)
          Blind(BlindNum)%VisFrontBeamDiffTrans(IProfAng,ISlatAng) = st_lay(5)
          Blind(BlindNum)%VisFrontBeamDiffRefl(IProfAng,ISlatAng)  = st_lay(6)
          Blind(BlindNum)%VisBackBeamDiffTrans(IProfAng,ISlatAng)  = st_lay(7)
          Blind(BlindNum)%VisBackBeamDiffRefl(IProfAng,ISlatAng)   = st_lay(8)
        END IF

        IF(Blind(BlindNum)%SlatAngleType == FixedSlats) EXIT
      END DO  ! End of loop over slat angles
    END DO  ! End of loop over profile angles

    IF(ISolVis == 1) THEN
      DO ISlatAng = 1,MaxSlatAngs
        Blind(BlindNum)%SolFrontDiffDiffTransGnd(ISlatAng) = &
          DiffuseAverageProfAngGnd(Blind(BlindNum)%SolFrontBeamBeamTrans(1:37,ISlatAng)) + &
          DiffuseAverageProfAngGnd(Blind(BlindNum)%SolFrontBeamDiffTrans(1:37,ISlatAng))
        Blind(BlindNum)%SolFrontDiffDiffTransSky(ISlatAng) = &
          DiffuseAverageProfAngSky(Blind(BlindNum)%SolFrontBeamBeamTrans(1:37,ISlatAng)) + &
          DiffuseAverageProfAngSky(Blind(BlindNum)%SolFrontBeamDiffTrans(1:37,ISlatAng))
        Blind(BlindNum)%SolFrontDiffAbsGnd(ISlatAng) = &
          DiffuseAverageProfAngGnd(Blind(BlindNum)%SolFrontBeamAbs(1:37,ISlatAng))
        Blind(BlindNum)%SolFrontDiffAbsSky(ISlatAng) = &
          DiffuseAverageProfAngSky(Blind(BlindNum)%SolFrontBeamAbs(1:37,ISlatAng))
        Blind(BlindNum)%SolFrontDiffDiffReflGnd(ISlatAng) = &
          DiffuseAverageProfAngGnd(Blind(BlindNum)%SolFrontBeamDiffRefl(1:37,ISlatAng))
        Blind(BlindNum)%SolFrontDiffDiffReflSky(ISlatAng) = &
          DiffuseAverageProfAngSky(Blind(BlindNum)%SolFrontBeamDiffRefl(1:37,ISlatAng))

        ! TH 2/17/2010. Added. Loop only for movable slat blinds
        IF(Blind(BlindNum)%SlatAngleType == FixedSlats) EXIT
      END DO
    END IF

  END DO  ! End of loop over solar vs. visible properties

END DO  ! End of loop over blinds

END SUBROUTINE CalcWindowBlindProperties
!*************************************************************************************

SUBROUTINE CalcWindowScreenProperties

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize static properties of window screens.

          ! METHODOLOGY EMPLOYED:
          ! Loop through all surfaces to determine which window has an exterior screen. Static
          ! variables are defined here, dynamic variables are calculated in CalcScreenTransmittance.

          ! REFERENCES: na

          ! USE STATEMENTS:
USE InputProcessor, ONLY: SameString
USE General, ONLY: RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: M = 18
  INTEGER, PARAMETER :: N = 18
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum          ! Index to surface number
  INTEGER :: ScreenNum        ! Index to each screen used on exterior of window
  INTEGER :: ConstrNumSh      ! Index to shaded constuction
  INTEGER :: MatNum           ! Index to material number
  INTEGER :: I, J             ! Integration loop counters
  REAL(r64)    :: SumTrans         ! Integration variable for transmittance
  REAL(r64)    :: SumTransVis      ! Integration variable for visible transmittance
  REAL(r64)    :: SumReflect       ! Integration variable for reflectance
  REAL(r64)    :: SumReflectVis    ! Integration variable for visible reflectance
  REAL(r64)    :: SumArea          ! Integration variable for area of quarter hemisphere
  REAL(r64)    :: SkyArea          ! Area of integration
  REAL(r64)    :: SunAzimuth       ! Azimuth angle of sun during integration
  REAL(r64)    :: SunAltitude      ! Altitude angle of sun during integration
  REAL(r64)    :: RelativeAzimuth  ! Relative azimuth angle of sun with respect to surface outward normal
  REAL(r64)    :: RelativeAltitude ! Relative altitude angle of sun with respect to surface outward normal
  INTEGER :: ShadingType    ! Type of shading device
  INTEGER, EXTERNAL :: GetNewUnitNumber ! Function to identify next available unit number
  INTEGER :: ScreenTransUnitNo ! Unit number of screen transmittance data file
  LOGICAL :: FoundMaterial    ! Flag to avoid printing screen transmittance data multiple times when Material:WindowScreen
                              ! is used on multiple surfaces
  LOGICAL :: PrintTransMap    ! Flag used to print transmittance map

  ALLOCATE(SurfaceScreens(NumSurfaceScreens))
  ALLOCATE(ScreenTrans(NumSurfaceScreens))
  ScreenNum = 0

PrintTransMap = .FALSE.
DO SurfNum = 1, TotSurfaces

  IF(Surface(SurfNum)%WindowShadingControlPtr /= 0) THEN
    ConstrNumSh = Surface(SurfNum)%ShadedConstruction
    MatNum = Construct(ConstrNumSh)%LayerPoint(1)
    ShadingType = WindowShadingControl(Surface(SurfNum)%WindowShadingControlPtr)%ShadingType
    IF(ShadingType == WSC_ST_ExteriorScreen) THEN

      IF(Material(MatNum)%ScreenMapResolution .GT. 0)PrintTransMap = .TRUE.
      ScreenNum = ScreenNum + 1
      SurfaceWindow(SurfNum)%ScreenNumber   = ScreenNum
!     If a screen material is used more than once, the Material structure's screen data pointer holds the screen number
!     of the last window surface. Use this method to access the screen parameter's only for static variables such as
!     diffuse properties (InitGlassOpticalCalculations). For all cases where the screen properties are a function of
!     sun azimuth and altitude angles, use the SurfaceScreens structure.
      Material(MatNum)%ScreenDataPtr        = ScreenNum
      SurfaceScreens(ScreenNum)%MaterialNumber = MatNum
!     Invert calculation done in GetMaterialInput to find Diameter to Spacing ratio (Props(7)/Props(6))
!     Material(MaterNum)%Trans = (1 - MaterialProps(7)/MaterialProps(6))**2.0
      SurfaceScreens(ScreenNum)%ScreenDiameterToSpacingRatio = 1.0d0 - SQRT(Material(MatNum)%Trans)

      IF(SameString(Material(MatNum)%ReflectanceModeling,'DoNotModel'))THEN
        SurfaceScreens(ScreenNum)%ScreenBeamReflectanceAccounting = DoNotModel
      ELSE IF(SameString(Material(MatNum)%ReflectanceModeling,'ModelAsDirectBeam'))THEN
        SurfaceScreens(ScreenNum)%ScreenBeamReflectanceAccounting = ModelAsDirectBeam
      ELSE IF(SameString(Material(MatNum)%ReflectanceModeling,'ModelAsDiffuse'))THEN
        SurfaceScreens(ScreenNum)%ScreenBeamReflectanceAccounting = ModelAsDiffuse
      END IF

      ! Reflectance of screen material only
      SurfaceScreens(ScreenNum)%ReflectCylinder      = Material(MatNum)%ReflectShade/(1 - Material(MatNum)%Trans)
      SurfaceScreens(ScreenNum)%ReflectCylinderVis   = Material(MatNum)%ReflectShadeVis/(1 - Material(MatNum)%Trans)

!     Integrate the transmittance over a quarter hemisphere for use in diffuse calculations
      SumTrans      = 0.0d0
      SumTransVis   = 0.0d0
      SumReflect    = 0.0d0
      SumReflectVis = 0.0d0
      SumArea       = 0.0d0
!     Integration over quarter hemisphere in polar coordinates and converting to rectangular to call screen model.
!     Proceed in reverse order such that the last calculation yields zero sun angle to window screen normal (angles=0,0).
!     The properties calculated at zero sun angle are then used elsewhere prior to the start of the actual simulation.
      DO J = N, 1, -1
        DO I = M, 1, -1
          SunAzimuth  = (90.0d0/N)*(J-1)*(Pi/180.0d0)
          SunAltitude = (90.0d0/M)*(I-1)*(Pi/180.0d0)
          SkyArea = SIN(SunAltitude) * COS(SunAltitude)
!         Integrate transmittance using coordiante transform
          RelativeAzimuth = ASIN(SIN(SunAltitude)*COS(SunAzimuth))  ! phi prime
          RelativeAltitude = ATAN(TAN(SunAltitude)*SIN(SunAzimuth)) ! alpha
          CALL CalcScreenTransmittance(0, Phi=RelativeAltitude, Theta=RelativeAzimuth, ScreenNumber=ScreenNum)
          SumTrans  = SumTrans + (SurfaceScreens(ScreenNum)%BmBmTrans+SurfaceScreens(ScreenNum)%BmDifTrans) * SkyArea
          SumTransVis  = SumTransVis + (SurfaceScreens(ScreenNum)%BmBmTransVis+SurfaceScreens(ScreenNum)%BmDifTransVis) * SkyArea
          SumReflect  = SumReflect + SurfaceScreens(ScreenNum)%ReflectSolBeamFront * SkyArea
          SumReflectVis  = SumReflectVis + SurfaceScreens(ScreenNum)%ReflectVisBeamFront * SkyArea
          SumArea  = SumArea +  SkyArea
        END DO
      END DO

      ! Reflectance of overall screen including openings and scattered transmittance
      SurfaceScreens(ScreenNum)%ReflectScreen    = SurfaceScreens(ScreenNum)%ReflectCylinder * &
                       (1.0d0 - (SurfaceScreens(ScreenNum)%BmBmTrans+SurfaceScreens(ScreenNum)%BmDifTrans))
      SurfaceScreens(ScreenNum)%ReflectScreenVis = SurfaceScreens(ScreenNum)%ReflectCylinderVis * &
                       (1.0d0 - (SurfaceScreens(ScreenNum)%BmBmTransVis+SurfaceScreens(ScreenNum)%BmDifTransVis))

      IF(SumArea .NE. 0)THEN
        SurfaceScreens(ScreenNum)%DifDifTrans        = SumTrans/SumArea
        SurfaceScreens(ScreenNum)%DifDifTransVis     = SumTransVis/SumArea
        SurfaceScreens(ScreenNum)%DifReflect         = SumReflect/SumArea
        SurfaceScreens(ScreenNum)%DifReflectVis      = SumReflectVis/SumArea
      END IF
      SurfaceScreens(ScreenNum)%DifScreenAbsorp     = MAX(0.0d0,(1.0d0 - SurfaceScreens(ScreenNum)%DifDifTrans - &
                                                                SurfaceScreens(ScreenNum)%DifReflect))

      Material(MatNum)%AbsorpThermalBack   = SurfaceScreens(ScreenNum)%DifScreenAbsorp
      Material(MatNum)%AbsorpThermalFront  = SurfaceScreens(ScreenNum)%DifScreenAbsorp
      Material(MatNum)%ReflectSolBeamFront = SurfaceScreens(ScreenNum)%DifReflect
      Material(MatNum)%ReflectSolBeamBack  = SurfaceScreens(ScreenNum)%DifReflect

    END IF ! (ShadingType == 'EXTERIORSCREEN')
  END IF !(Surface(SurfNum)%WindowShadingControlPtr /= 0)

ENDDO ! End of screen surface initialization

! Write transmittance versus direct normal angle to csv file

IF(PrintTransMap)THEN
ScreenTransUnitNo=GetNewUnitNumber()
OPEN(UNIT=ScreenTransUnitNo,FILE='eplusscreen.csv',ERR=99999,STATUS='unknown',action='write')
!  WRITE(ScreenTransUnitNo,*)' '
  DO ScreenNum = 1, NumSurfaceScreens
    MatNum = SurfaceScreens(ScreenNum)%MaterialNumber
!   Do not print transmittance map if angle increment is equal to 0
    IF(Material(MatNum)%ScreenMapResolution .EQ. 0)CYCLE
    FoundMaterial = .FALSE.
    DO I = ScreenNum+1, NumSurfaceScreens
!     Write out transmittance data once for each Material:WindowScreen object
      IF(MatNum .EQ. SurfaceScreens(I)%MaterialNumber)FoundMaterial = .TRUE.
    END DO
    IF(FoundMaterial)CYCLE
!   Store transmittance at direct normal angle
    IF(Material(MatNum)%ScreenMapResolution .NE. 0)THEN
      ALLOCATE(ScreenTrans(ScreenNum)%Trans(90/Material(MatNum)%ScreenMapResolution+1,90/Material(MatNum)%ScreenMapResolution+1))
      ALLOCATE(ScreenTrans(ScreenNum)%Scatt(90/Material(MatNum)%ScreenMapResolution+1,90/Material(MatNum)%ScreenMapResolution+1))
      ScreenTrans(ScreenNum)%Trans = 0.0d0
      ScreenTrans(ScreenNum)%Scatt = 0.0d0
      DO J = 90/Material(MatNum)%ScreenMapResolution+1, 1, -1
        DO I = 90/Material(MatNum)%ScreenMapResolution+1, 1, -1
          SunAzimuth  = Material(MatNum)%ScreenMapResolution*(J-1)*(Pi/180.0d0)
          SunAltitude = Material(MatNum)%ScreenMapResolution*(I-1)*(Pi/180.0d0)
          CALL CalcScreenTransmittance(0, Phi=SunAltitude, Theta=SunAzimuth, ScreenNumber=ScreenNum)
          ScreenTrans(ScreenNum)%Trans(J,I) = SurfaceScreens(ScreenNum)%BmBmTrans
          ScreenTrans(ScreenNum)%Scatt(J,I) = SurfaceScreens(ScreenNum)%BmDifTrans
        END DO
      END DO

      WRITE(ScreenTransUnitNo,fmta)'MATERIAL:WINDOWSCREEN:'//TRIM(Material(SurfaceScreens(ScreenNum)%MaterialNumber)%Name)
      WRITE(ScreenTransUnitNo,fmta)'Tabular data for beam solar transmittance at varying "relative" azimuth (row) and '// &
                                    'altitude (column) angles (deg) [relative to surface normal].'
      WRITE(ScreenTransUnitNo,fmta,Advance='No')',90'
      DO I = 90/Material(MatNum)%ScreenMapResolution,2,-1
        WRITE(ScreenTransUnitNo,fmta,Advance='No')','//TRIM(RoundSigDigits(((I-1)*Material(MatNum)%ScreenMapResolution),1))
      END DO
      WRITE(ScreenTransUnitNo,fmta)',0'

      DO J = 1,90/Material(MatNum)%ScreenMapResolution+1
        WRITE(ScreenTransUnitNo,fmta,Advance='No')TRIM(RoundSigDigits(((J-1)*Material(MatNum)%ScreenMapResolution),1))
        DO I = 90/Material(MatNum)%ScreenMapResolution+1,2,-1
          WRITE(ScreenTransUnitNo,fmta,Advance='No')','//TRIM(RoundSigDigits(ScreenTrans(ScreenNum)%Trans(J,I),6))
        END DO
        WRITE(ScreenTransUnitNo,fmta)','//TRIM(RoundSigDigits(ScreenTrans(ScreenNum)%Trans(J,I),6))
      END DO
      WRITE(ScreenTransUnitNo,fmta) ' '
      WRITE(ScreenTransUnitNo,fmta) ' '

      WRITE(ScreenTransUnitNo,fmta)'MATERIAL:WINDOWSCREEN:'//TRIM(Material(SurfaceScreens(ScreenNum)%MaterialNumber)%Name)
      WRITE(ScreenTransUnitNo,fmta)'Tabular data for scattered solar transmittance at varying "relative" azimuth (row) and '// &
                                    'altitude (column) angles (deg) [relative to surface normal].'
      DO I = 1,90/Material(MatNum)%ScreenMapResolution
        WRITE(ScreenTransUnitNo,fmta,Advance='No')','//TRIM(RoundSigDigits(((I-1)*Material(MatNum)%ScreenMapResolution),1))
      END DO
      WRITE(ScreenTransUnitNo,fmta)','//TRIM(RoundSigDigits(((I-1)*Material(MatNum)%ScreenMapResolution),1))

      DO J = 1,90/Material(MatNum)%ScreenMapResolution+1
        WRITE(ScreenTransUnitNo,fmta,Advance='No')TRIM(RoundSigDigits(((J-1)*Material(MatNum)%ScreenMapResolution),1))
        DO I = 1,90/Material(MatNum)%ScreenMapResolution
          WRITE(ScreenTransUnitNo,fmta,Advance='No')','//TRIM(RoundSigDigits(ScreenTrans(ScreenNum)%Scatt(J,I),6))
        END DO
        WRITE(ScreenTransUnitNo,fmta)','//  &
               TRIM(RoundSigDigits(ScreenTrans(ScreenNum)%Scatt(J,90/Material(MatNum)%ScreenMapResolution+1),6))
      END DO
      WRITE(ScreenTransUnitNo,fmta) ' '
      WRITE(ScreenTransUnitNo,fmta) ' '
    END IF
  END DO
99999 CLOSE(UNIT=ScreenTransUnitNo)
END IF
DEALLOCATE(ScreenTrans)
END SUBROUTINE CalcWindowScreenProperties

SUBROUTINE BlindOpticsDiffuse (BlindNum,ISolVis,c,b_el,p)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hans Simmler
          !       DATE WRITTEN   July-Aug 1995
          !       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
          !                      Aug 2002 (FCW): make corrections so that calculations are consistent with
          !                       G(i) = Sum over j of J(j)*F(j,i). Previously, i,j was
          !                      interchanged in F, so that
          !                       G(i) = Sum over j of J(j)*F(i,j), which is wrong.
          !                      This change was made to resolve discrepancies between EnergyPlus results
          !                      and blind transmittance measurements made at Oklahoma State Univ.
          !                      Feb 2004 (FCW): modify slat edge correction calc to avoid possible divide by zero
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! From the slat properties, calculates the diffuse solar, diffuse visible and IR
          ! transmission and reflection properties of a window blind.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
          ! F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

          ! USE STATEMENTS:na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

      INTEGER, INTENT(IN)    ::  BlindNum             ! Blind number
      INTEGER, INTENT(IN)    ::  Isolvis              ! 1 = solar and IR calculation; 2 = visible calculation
      REAL(r64), INTENT(IN)       ::  c(15)                ! Slat properties
      REAL(r64), INTENT(IN)       ::  b_el                 ! Slat elevation (radians)
      REAL(r64), INTENT(OUT)      ::  p(16)                ! Blind properties

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

      REAL(r64)       ri,rib               ! Front and back IR slat reflectance
      REAL(r64)       phib                 ! Elevation of slat normal vector (radians)
      REAL(r64)       phis                 ! Source elevation (radians)
      REAL(r64)       delphis              ! Angle increment for integration over source distribution (radians)
      REAL(r64)       fEdgeSource(10)      ! Slat edge correction factor vs source elevation
      REAL(r64)       fEdgeA(2)            ! Average slat edge correction factor for upper and lower quadrants
                                           !  seen by window blind
      REAL(r64)       gamma                ! phib - phis
      INTEGER         Iphis                ! Source elevation counter
      INTEGER         IUpDown              ! =1 for source in upper quadrant, =2 for source in lower quadrant
      REAL(r64)       fEdge,fEdge1         ! Slat edge correction factor
      REAL(r64)       J(6)                 ! Slat section radiosity vector
      REAL(r64)       G(6)                 ! Slat section irradiance vector
      REAL(r64)       Q(6)                 ! Slat section radiance vector
      REAL(r64)       F(6,6)               ! View factor array
      REAL(r64)       X(4,4)               ! Exchange matrix
      REAL(r64)       Xinv(4,4)            ! Inverse of exchange matrix
      INTEGER         k,m                  ! Array indices
      INTEGER         indx(4)              ! LU decomposition indices
      REAL(r64)       BlindIRreflFront     ! Blind front IR reflectance
      REAL(r64)       BlindIRreflBack      ! Blind back IR reflectance


  ! The slat input properties are:
  ! c(1)    0. (unused)
  ! c(2)    Slat width (m)
  ! c(3)    Slat separation (m)
  ! c(4)    0. (unused)
  ! c(5)    0. (unused)
  ! c(6)    0. (unused)
  !      The following are solar or visible properties
  ! c(7)    trans beam-diff
  ! c(8)    refl front beam-diff
  ! c(9)    refl back beam-diff
  ! c(10)   trans diff-diff
  ! c(11)   refl front diff-diff
  ! c(12)   refl back diff-diff
  !      The following are hemispherical thermal IR properties
  ! c(13)   trans diff-diff
  ! c(14)   emiss front diff
  ! c(15)   emiss back diff

  ! The calculated blind properties are:
  !      The following are solar or visible properties
  ! p(1)    trans front beam-beam
  ! p(2)    refl front beam-beam
  ! p(3)    trans back beam-beam
  ! p(4)    refl back beam-beam
  ! p(5)    trans front beam-diff
  ! p(6)    refl front beam-diff
  ! p(7)    trans back beam-diff
  ! p(8)    refl back beam-diff
  ! p(9)    trans front diff-diff
  ! p(10)   refl front diff-diff
  ! p(11)   trans back diff-diff
  ! p(12)   refl back diff-diff
  !      The following are IR properties
  ! p(13)   IR trans front (same as IR trans back)
  ! p(14)   IR emissivity front
  ! p(15)   IR emissivity back
  ! p(16)   0.0 (unused)

!     Calculate view factors between slat sections (slat is divided longitudinally into two equal parts)

      CALL VIEWFAC(c(2),c(3),b_el,PiOvr2,F)

!     Set up exchange matrix X for diffuse properties

      do k=3,5,2
          do m=3,6
              X(k-2,m-2)=-c(12)*F(m,k)-c(10)*F(m,k+1)
              X(k-1,m-2)=-c(10)*F(m,k)-c(11)*F(m,k+1)
          end do
      end do

      do k=1,4
          X(k,k)=X(k,k)+1.0d0
      end do

      indx = 0
      CALL InvertMatrix(X,Xinv,indx,4,4) !Objexx:Note X modified by this call

!---------Calculate diffuse short-wave properties for the front side of the blind

!     Sources

      Q(3) = c(12)*F(1,3) + c(10)*F(1,4)
      Q(4) = c(10)*F(1,3) + c(11)*F(1,4)
      Q(5) = c(12)*F(1,5) + c(10)*F(1,6)
      Q(6) = c(10)*F(1,5) + c(11)*F(1,6)

!     Radiosities

      J(1)=1.0d0
      J(2)=0.0d0
      do k=3,6
          J(k)=0.0d0
          do m=3,6
              J(k)=J(k)+Xinv(k-2,m-2)*Q(m)
          end do
      end do

!     Irradiances

      do k=1,6
          G(k)=0.0d0
          do m=1,6
              !G(k)=G(k)+F(k,m)*J(m)
              G(k)=G(k)+J(m)*F(m,k)
          end do
      end do

!     Slat edge correction factor
        phib = b_el
        delphis = PiOvr2/10.d0
        DO IUpDown = 1,2
          DO Iphis = 1,10
            phis = -(iphis-0.5d0)*delphis
            IF(IUpDown == 2) phis = (iphis-0.5d0)*delphis
            fEdgeSource(Iphis) = 0.0d0
            fEdge1 = 0.0d0
            gamma = phib - phis
            IF(ABS(SIN(gamma))>0.01d0) THEN
              IF((phib > 0.0 .AND. phib <= PiOvr2 .AND. phis <= phib) .OR. &
                 (phib > PiOvr2 .AND. phib <= Pi .AND. phis > -(Pi-phib))) &
                  fEdge1 = Blind(BlindNum)%SlatThickness * ABS(SIN(gamma)) / &
                ((Blind(BlindNum)%SlatSeparation + Blind(BlindNum)%SlatThickness/ABS(SIN(phib)))*COS(phis))
              fEdgeSource(Iphis) = MIN(1.0d0,ABS(fEdge1))
            END IF
          END DO
          fEdgeA(IUpDown) = DiffuseAverage(fEdgeSource)
        END DO
        fEdge = 0.5d0*(fEdgeA(1) + fEdgeA(2))

!     Front diffuse-diffuse transmittance (transmittance of slat edge assumed zero)
      p(9) = G(2)*(1.0d0-fEdge)

!     Front diffuse-diffuse reflectance (edge of slat is assumed to have same diffuse
!     reflectance as front side of slat, c(11))
      p(10) = G(1)*(1.0d0-fEdge) + fEdge*C(11)

!-----------Calculate diffuse short-wave properties for the back side of the blind

!     Sources

      Q(3)=c(12)*F(2,3)+c(10)*F(2,4)
      Q(4)=c(10)*F(2,3)+c(11)*F(2,4)
      Q(5)=c(12)*F(2,5)+c(10)*F(2,6)
      Q(6)=c(10)*F(2,5)+c(11)*F(2,6)

!     Radiosities

      J(1)=0.0d0
      J(2)=1.0d0
      do k=3,6
          J(k)=0.0d0
          do m=3,6
              J(k)=J(k)+Xinv(k-2,m-2)*Q(m)
          end do
      end do

!     Irradiances

      do k=1,6
          G(k)=0.0d0
          do m=1,6
             !G(k)=G(k)+F(k,m)*J(m)
              G(k)=G(k)+J(m)*F(m,k)
          end do
      end do

!     Back diffuse-diffuse transmittance
      p(11)=G(1)*(1.0d0-fEdge)

!     Back hemi-hemi reflectance
      p(12)=G(2)*(1.0d0-fEdge) + fEdge*C(11)

IF(ISolVis == 1) THEN

!-----------Calculate IR properties of the blind
!           (use same set of view factors as for diffuse short-wave properties)

!     Front and back slat IR reflectances
      ri =1-c(13)-c(14)
      rib=1-c(13)-c(15)

!     Set up exchange matrix X for diffuse properties

      do k=3,5,2
          do m=3,6
              X(k-2,m-2)=-rib  *F(m,k) -c(13)*F(m,k+1)
              X(k-1,m-2)=-c(13)*F(m,k) -ri   *F(m,k+1)
          end do
      end do

      do k=1,4
          X(k,k)=X(k,k)+1.0d0
      end do

      indx = 0
      CALL InvertMatrix(X,Xinv,indx,4,4) !Objexx: Note X modified by this call

!---------Calculate diffuse IR properties for the FRONT side of the blind

!     Sources

      Q(3) = rib  *F(1,3) + c(13)*F(1,4)
      Q(4) = c(13)*F(1,3) + ri   *F(1,4)
      Q(5) = rib  *F(1,5) + c(13)*F(1,6)
      Q(6) = c(13)*F(1,5) + ri   *F(1,6)

!     Radiosities

      J(1)=1.0d0
      J(2)=0.0d0
      do k=3,6
          J(k)=0.0d0
          do m=3,6
              J(k)=J(k)+Xinv(k-2,m-2)*Q(m)
          end do
      end do

!     Irradiances
      do k=1,6
          G(k)=0.0d0
          do m=1,6
              !G(k)=G(k)+F(k,m)*J(m)
              G(k)=G(k)+J(m)*F(m,k)
          end do
      end do

!     Front diffuse-diffuse IR transmittance (transmittance of slat edge assumed zero)
      p(13) = G(2)*(1.d0-fEdge)

!     Front diffuse-diffuse IR reflectance (edge of slat is assumed to have same IR
!     reflectance as front side of slat, ri)
      BlindIRreflFront = G(1)*(1.d0-fEdge) + fEdge*ri

!     Front IR emissivity
      p(14) = MAX(0.0001d0,1.0d0-p(13)-BlindIRreflFront)

!-----------Calculate diffuse IR properties for the BACK side of the blind

!     Sources

      Q(3)= rib  *F(2,3) + c(13)*F(2,4)
      Q(4)= c(13)*F(2,3) + ri   *F(2,4)
      Q(5)= rib  *F(2,5) + c(13)*F(2,6)
      Q(6)= c(13)*F(2,5) + ri   *F(2,6)

!     Radiosities

      J(1)=0.0d0
      J(2)=1.0d0
      do k=3,6
          J(k)=0.0d0
          do m=3,6
              J(k)=J(k)+Xinv(k-2,m-2)*Q(m)
          end do
      end do

!     Irradiances

      do k=1,6
          G(k)=0.0d0
          do m=1,6
             !G(k)=G(k)+F(k,m)*J(m)
              G(k)=G(k)+J(m)*F(m,k)
          end do
      end do

!     Back diffuse-diffuse IR reflectance
      BlindIRreflBack = G(2)*(1.d0-fEdge) + fEdge*ri

!     Back IR emissivity
      p(15) = MAX(0.0001d0,1.0d0-p(13)-BlindIRreflBack)

END IF ! End of IR properties calculation

RETURN
END SUBROUTINE BlindOpticsDiffuse
!**********************************************************************************************

SUBROUTINE BlindOpticsBeam (BlindNum,c,b_el,s_el,p)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hans Simmler
          !       DATE WRITTEN   July-Aug 1995
          !       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
          !                      Aug 2002 (FCW): make corrections so that calculations are consistent with
          !                       G(i) = Sum over j of J(j)*F(j,i). Previously, i,j was
          !                      interchanged in F, so that
          !                       G(i) = Sum over j of J(j)*F(i,j), which is wrong.
          !                      This change was made to resolve discrepancies between EnergyPlus results
          !                      and blind transmittance measurements made at Oklahoma State Univ.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     Calculates the beam radiation properties of a
          !     window blind consisting of flat slats with known material properties.
          !     The calculation for the reverse direction is done with the radiation source
          !     reflected at the window plane.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
          ! F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

          ! USE STATEMENTS:

      USE General, ONLY: BlindBeamBeamTrans ! Blind beam-to-beam transmittance function

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER, INTENT(IN) :: BlindNum          ! Blind number
      REAL(r64), INTENT(IN)    :: c(15)             ! Slat properties (equivalent to BLD_PR)
      REAL(r64), INTENT(IN)    :: b_el              ! Slat elevation (radians)
      REAL(r64), INTENT(IN)    :: s_el              ! Solar profile angle (radians)
      REAL(r64), INTENT(OUT)   :: p(16)             ! Blind properties (equivalent to ST_LAY)

  ! The slat input properties are:
  ! c(1)    0. (unused)
  ! c(2)    Slat width (m)
  ! c(3)    Slat separation (m)
  ! c(4)    0. (unused)
  ! c(5)    0. (unused)
  ! c(6)    0. (unused)
  !      The following are solar or visible properties
  ! c(7)    trans beam-diff
  ! c(8)    refl front beam-diff
  ! c(9)    refl back beam-diff
  ! c(10)   trans diff-diff
  ! c(11)   refl front diff-diff
  ! c(12)   refl back diff-diff
  !      The following are hemispherical thermal IR properties
  ! c(13)   trans diff-diff
  ! c(14)   emiss front diff
  ! c(15)   emiss back diff

  ! The calculated blind properties are:
  !      The following are solar or visible properties
  ! p(1)    trans front beam-beam
  ! p(2)    refl front beam-beam
  ! p(3)    trans back beam-beam
  ! p(4)    refl back beam-beam
  ! p(5)    trans front beam-diff
  ! p(6)    refl front beam-diff
  ! p(7)    trans back beam-diff
  ! p(8)    refl back beam-diff
  ! p(9)    trans front diff-diff
  ! p(10)   refl front diff-diff
  ! p(11)   trans back diff-diff
  ! p(12)   refl back diff-diff
  !      The following are IR properties
  ! p(13)   IR trans front (same as IR trans back)
  ! p(14)   IR emissivity front
  ! p(15)   IR emissivity back
  ! p(16)   0.0 (unused)

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) phib                   ! Elevation angle of normal vector to front of slat (0 to pi radians)
      REAL(r64) phis                   ! Elevation angle of source vector; same as "profile angle" (-pi/2 to pi/2 radians)
      REAL(r64) Gamma                  ! phib - phis (radians)
      REAL(r64) J(6)                   ! Slat surface section radiosity vector
      REAL(r64) G(6)                   ! Slat surface section irradiance vector
      REAL(r64) Q(6)                   ! Slat surface section source vector
      REAL(r64) F(6,6)                 ! View factor array
      REAL(r64) X(4,4)                 ! X*J = Q
      REAL(r64) Xinv(4,4)              ! J = Xinv*Q
      REAL(r64) fEdge,fEdge1           ! Slat edge correction factor
      INTEGER i,k,m               ! Array indices
      INTEGER indx(4)             ! Indices for LU decomposition

      p = 0.0d0

!     Elevation of radiation source; source is assumed to be in a plane that
!     (1) contains the slat outward normal and (2) is perpendicular to plane of the blinds.
      phis = s_el

!     Elevation of slat outward normal
      phib = b_el

!     Loop twice for front and back side properties of blind
      DO i=0,2,2

!       For back-side properties, reflect the source position so that it is the mirror
!       image of the original source position, where the "mirror" is in the plane of the
!       blinds. This is equivalent to keeping the original source position but rotating
!       the slats so that the original slat angle (e.g., 45 deg) becomes 180 - original slat
!       angle (135 deg).

        if (i.eq.2) then
          phib = PI - phib
        end if

!       Correction factor that accounts for finite thickness of slats. It is used to modify the
!       blind transmittance and reflectance to account for reflection and absorption by the
!       edge of the slat. fEdge is ratio of area subtended by edge of slat
!       to area between tops of adjacent slats.

        fEdge  = 0.0d0
        fEdge1 = 0.0d0
        gamma = phib - phis
        IF(ABS(SIN(gamma))>0.01d0) THEN
          IF((phib > 0.0 .AND. phib <= PiOvr2 .AND. phis <= phib) .OR. &
             (phib > PiOvr2 .AND. phib <= Pi .AND. phis > -(Pi-phib))) &
          fEdge1 = Blind(BlindNum)%SlatThickness * ABS(SIN(gamma)) / &
            ((Blind(BlindNum)%SlatSeparation + Blind(BlindNum)%SlatThickness/ABS(SIN(phib)))*COS(phis))
          fEdge = MIN(1.0d0,ABS(fEdge1))
        END IF

!       Direct-to-direct transmittance (portion of beam that passes between slats without
!       without touching them

        p(1+i) = BlindBeamBeamTrans(phis,phib,Blind(BlindNum)%SlatWidth,Blind(BlindNum)%SlatSeparation, &
                    Blind(BlindNum)%SlatThickness)
!       Direct-to-direct reflectance; this is zero for now since all reflection is assumed to be diffuse.
        p(2+i)=0.0d0

!       View factors between slat sections for calculating direct-to-diffuse transmittance and reflectance
        CALL ViewFac(c(2),c(3),phib,phis,F)

!       Set up exchange matrix X for calculating direct-to-diffuse properties

        do k=3,5,2
          do m=3,6
            X(k-2,m-2)=-c(12)*F(m,k)-c(10)*F(m,k+1)
            X(k-1,m-2)=-c(10)*F(m,k)-c(11)*F(m,k+1)
          end do
        end do

        do k=1,4
          X(k,k)=X(k,k)+1.0d0
        end do

        indx = 0
        ! In the following, note that InvertMatrix changes X
        CALL InvertMatrix(X,Xinv,indx,4,4)

!       Set up sources for direct-diffuse slat properties
        if (ABS(phis-phib) <= PiOvr2) then    ! Beam hits front of slat
          Q(3) = c(4) + c(7)    ! beam-beam trans of slat + beam-diff trans of slat
          Q(4) = c(5) + c(8)    ! front beam-beam refl of slat + front beam-diff refl of slat
        else                                  ! Beam hits back of slat
          Q(3) = c(6) + c(9)    ! back beam-beam refl of slat  + back beam-diff refl of slat
          Q(4) = c(4) + c(7)    ! beam-beam trans of slat + beam-diff trans of slat
        end if

!       Correct for fraction of beam that is not directly transmitted; 1 - this fraction is
!       the fraction of the incoming beam that is incident on the front or back surfaces of the slats.
        Q(3) = Q(3)*(1.0d0-p(1+i))
        Q(4) = Q(4)*(1.0d0-p(1+i))

!       Radiosities (radiance of slat sections)
        J(1)=0.0d0
        J(2)=0.0d0
        do k=3,6
          J(k)=0.0d0
          do m=3,4
            J(k)=J(k) + Xinv(k-2,m-2)*Q(m)
          end do
        end do

!       Irradiance on slat sections
        do k=1,6
          G(k)=0.0d0
          do m=3,6
            G(k)=G(k)+J(m)*F(m,k)
          end do
        end do

!       Direct-to-diffuse transmittance
        p(5+i)=G(2)*(1.0d0-fEdge)

!       Direct-to-diffuse reflectance (assuming the edge reflectance is the same as the
!       reflectance of the front side of the slat, C(8))
        p(6+i)=G(1)*(1.0d0-fEdge) + fEdge*C(8)

      END DO  ! End of loop over front and back side properties of blind

      RETURN
END SUBROUTINE BlindOpticsBeam
!********************************************************************************************

SUBROUTINE ViewFac(s,h,phib,phis,F)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hans Simmler
          !       DATE WRITTEN   July-Aug 1995
          !       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
          !                      Apr 2002 (FCW): prevent sqrt of small negative argument
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     Calculates the view factors between sections of adjacent slats,
          !     where each slat is divided longitudinally into two equal sections whose
          !     dimensions depend on source profile angle and slat geometry. The view
          !     factors are used in BlindOpticsBeam and BlindOpticsDiffuse to determine blind
          !     transmittance and reflectance for short-wave and long-wave radiation.

          ! METHODOLOGY EMPLOYED:
          !     Uses expressions for view factor between flat strips with a common edge
          !     and flat strips displaced from one another. See engineering documentation.

          ! REFERENCES:
          ! "Solar-Thermal Window Blind Model for DOE-2," H. Simmler, U. Fischer and
          ! F. Winkelmann, Lawrence Berkeley National Laboratory, Jan. 1996.

          ! USE STATEMENTS:na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      REAL(r64), INTENT(IN)    ::  s                ! Slat width (m)
      REAL(r64), INTENT(IN)    ::  h                ! Distance between faces of adjacent slats (m)
      REAL(r64), INTENT(IN)    ::  phib             ! Elevation angle of normal to slat (radians)
      REAL(r64), INTENT(IN)    ::  phis             ! Profile angle of radiation source (radians)
      REAL(r64), INTENT(OUT)   ::  F(6,6)           ! View factor array

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64)     ::  L(6),L3,L5       ! Length of slat sections: L1 = L2 = h; L3, L5 = length
                                         !  of upper slat sections; L4, L6 = length of lower slat
                                         !  slat sections (m)
      REAL(r64)     ::  d1,d2,d3,d4,d5,d6 ! Slat geometry variables (m)
      REAL(r64)     ::  h2               ! h**2
      REAL(r64)     ::  ht               ! 2*h
      REAL(r64)     ::  w                ! Slat geometry variable (m)
      REAL(r64)     ::  a                ! Intermediate variable (m)
      REAL(r64)     ::  co               ! Cosine of source profile angle
      INTEGER       ::  i,j              ! View factor array indices

      h2=h**2
      ht=2.0d0*h
      co=cos(phis)
      if (abs(co) < 0.001d0) co=0.0d0
      w=ht
      if (co.ne.0.0d0) w=s*cos(phib-phis)/co
      L3=s*h/abs(w)
      if (L3 > s) L3=s
      L5=s-L3
      a=ht*cos(phib)
      ! MAX(0.,...) in the following prevents small negative argument for sqrt
      d1=sqrt(MAX(0.d0,s*s+h2+a*s))
      d2=sqrt(MAX(0.d0,s*s+h2-a*s))
      d3=sqrt(MAX(0.d0,L3*L3+h2+a*L3))
      d4=sqrt(MAX(0.d0,L3*L3+h2-a*L3))
      d5=sqrt(MAX(0.d0,L5*L5+h2-a*L5))
      d6=sqrt(MAX(0.d0,L5*L5+h2+a*L5))
      do i=1,6
        F(i,i)=0.0d0
      end do
      F(1,1)=0.0d0
      F(1,2)=(d1+d2-2.0d0*s)/ht
      F(1,3)=(h+L3-d3)/ht
      F(1,4)=(h+L3-d4)/ht
      F(1,5)=(L5+d3-d1)/ht
      F(1,6)=(L5+d4-d2)/ht
      F(2,3)=(L3+d5-d2)/ht
      F(2,4)=(L3+d6-d1)/ht
      F(2,5)=(h+L5-d5)/ht
      F(2,6)=(h+L5-d6)/ht
      F(3,4)=(d3+d4-ht)/(2.0d0*L3)
      F(3,5)=0.0d0
      F(3,6)=(d2+h-d4-d5)/(2.0d0*L3)
      F(4,5)=(d1+h-d3-d6)/(2.0d0*L3)
      F(4,6)=0.0d0
      F(5,6)=0.0d0
      if (L5 > 0.0d0) F(5,6)=(d5+d6-ht)/(2.0d0*L5)
      L(1)=h
      L(2)=h
      L(3)=L3
      L(4)=L3
      L(5)=L5
      L(6)=L5
      do i=2,6
        do j=1,i-1
          F(i,j)=0.0d0
          if (L(i) > 0.0d0) F(i,j)=F(j,i)*L(j)/L(i)
        end do
      end do
RETURN
END SUBROUTINE VIEWFAC

!*****************************************************************************************

SUBROUTINE InvertMatrix(a, y, indx, np, n)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hans Simmler
          !       DATE WRITTEN   July-Aug 1995
          !       MODIFIED       Aug 2001 (FCW): adapt to EnergyPlus
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     Inverts a matrix.

          ! METHODOLOGY EMPLOYED:
          !     Uses LU decomposition.

          ! REFERENCES:na

          ! USE STATEMENTS:na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER, INTENT(IN)      :: np, n       ! Dimension of matrix
      REAL(r64), INTENT(INOUT) :: a(np,np) ! Matrix to be inverted !Objexx:ArgIN Changed IN to INOUT: LUDCMP is in-place LU decomposition
      REAL(r64), INTENT(OUT)   :: y(np,np)    ! Inverse of matrix a
      INTEGER, INTENT(OUT)     :: indx(np)    ! Index vector for LU decomposition

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
      INTEGER                  :: i,j         ! Array indices
      INTEGER                  :: d

      y=0.0d0
      DO i=1,n
        y(i,i) = 1.0d0
      END DO
      indx=0

      CALL LUDCMP(a,n,np,indx,d) !Objexx:ArgIN a was marked IN but is modified by this in-place LU decomposition

      DO j=1,n
        CALL LUBKSB(a,n,np,indx,y(1,j))
      END DO

RETURN
END SUBROUTINE InvertMatrix
!*****************************************************************************************

SUBROUTINE LUDCMP(A,N,NP,INDX,D)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann?
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs a LU decompostion of given matrix.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      integer np,n
      REAL(r64) :: A(NP,NP) ! matrix
      integer INDX(N)
      INTEGER   D

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) VV(100),sum,aamax,dum
      INTEGER   I,J,K,imax

      D=1
      DO I=1,N
          AAMAX=0.0d0
          DO J=1,N
              IF (ABS(A(I,J)) > AAMAX) AAMAX=ABS(A(I,J))
          END DO

          IF (AAMAX.EQ.0.0d0) THEN
            CALL ShowFatalError('Singular matrix in LUDCMP, window calculations')
          ENDIF
          VV(I)=1.0d0/AAMAX           ! Was commented out prior to 10/5/01, which caused overflows
                                   ! in this routine in rare cases

      END DO

      DO J=1,N
          IF (J > 1) THEN
              DO I=1,J-1
                  SUM=A(I,J)
                  IF (I > 1)THEN
                      DO K=1,I-1
                          SUM=SUM-A(I,K)*A(K,J)
                      ENDDO

                      A(I,J)=SUM
                  ENDIF
              ENDDO
          ENDIF
          AAMAX=0.0d0
          DO I=J,N
              SUM=A(I,J)
              IF (J > 1)THEN
                  DO K=1,J-1
                      SUM=SUM-A(I,K)*A(K,J)
                  ENDDO
                  A(I,J)=SUM
              ENDIF

              DUM=VV(I)*ABS(SUM)
              IF (DUM.GE.AAMAX) THEN
                  IMAX=I
                  AAMAX=DUM
              ENDIF
          ENDDO

          IF (J.NE.IMAX)THEN
              DO K=1,N
                  DUM=A(IMAX,K)
                  A(IMAX,K)=A(J,K)
                  A(J,K)=DUM
              ENDDO

              D=-D
              VV(IMAX)=VV(J)
          ENDIF

          INDX(J)=IMAX
          IF(J.NE.N)THEN
              IF(A(J,J).EQ.0.0d0) A(J,J)=rTinyValue

              DUM=1.0d0/A(J,J)
              DO I=J+1,N
                  A(I,J)=A(I,J)*DUM
              ENDDO

          ENDIF
      ENDDO

      IF(A(N,N).EQ.0.0d0) A(N,N)=rTinyValue
      RETURN
END SUBROUTINE LUDCMP
!*****************************************************************************************

SUBROUTINE LUBKSB(A,N,NP,INDX,B)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs back substitution of a LU matrix.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      integer n,np
      REAL(r64) A(NP,NP),B(N)
      INTEGER INDX(N)
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

      integer j,i,ii,LL
      REAL(r64) sum

      II=0

      DO I=1,N
          LL=INDX(I)
          SUM=B(LL)
          B(LL)=B(I)
          IF (II.NE.0)THEN
              DO J=II,I-1
                  SUM=SUM-A(I,J)*B(J)
              ENDDO
          ELSE IF (SUM.NE.0.0d0) THEN
              II=I
          ENDIF

          B(I)=SUM
      ENDDO

      DO I=N,1,-1
          SUM=B(I)
          IF(I.LT.N)THEN
              DO J=I+1,N
                  SUM=SUM-A(I,J)*B(J)
              ENDDO
          ENDIF

          B(I)=SUM/A(I,I)
      ENDDO
      RETURN
END SUBROUTINE LUBKSB


! added for custom solar or visible spectrum
SUBROUTINE CheckAndReadCustomSprectrumData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         T. Hong
          !       DATE WRITTEN   August 2013
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Check, read, and assign the custom solar or visible spectrum to:
          !  solar: nume, wle(nume), e(nume). nume = 107
          !  visible: numt3, wlt3(numt3), y30(numt3). numt3 = 81
          !
          ! Three related IDD objects:
          !  EnergyManagementSystem:ConstructionIndexVariable
          !  Site:SolarAndVisibleSpectrum, Site:SpectrumData

          ! METHODOLOGY EMPLOYED:
          ! Overwriting the default values

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE InputProcessor
  !USE DataGlobals ,    ONLY: AnyEnergyManagementSystemInModel

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
  LOGICAL :: ErrorsFound = .false.    ! If errors detected in input
  INTEGER :: NumAlphas                ! Number of Alphas for each GetobjectItem call
  INTEGER :: NumNumbers               ! Number of Numbers for each GetobjectItem call
  INTEGER :: NumArgs
  INTEGER :: IOStatus
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaArgs     ! Alpha input items for object
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: rNumericArgs      ! Numeric input items for object

  LOGICAL, SAVE :: RunMeOnceFlag = .FALSE. ! This subroutine only needs to be run once
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  CHARACTER(len=MaxNameLength) :: cSolarSpectrum
  CHARACTER(len=MaxNameLength) :: cVisibleSpectrum
  INTEGER :: iSolarSpectrum = 0
  INTEGER :: iVisibleSpectrum = 0
  INTEGER :: NumSiteSpectrum = 0
  INTEGER :: Loop
  INTEGER :: iTmp

  IF (RunMeOnceFlag) RETURN

  ! Step 1 - check whether there is custom solar or visible spectrum
  cCurrentModuleObject = 'Site:SolarAndVisibleSpectrum'
  NumSiteSpectrum = GetNumObjectsFound(cCurrentModuleObject)

  ! no custom spectrum data, done!
  IF (NumSiteSpectrum == 0) THEN
    RunMeOnceFlag = .TRUE.
    RETURN
  ENDIF

  ! read custom spectrum data from Site:SolarAndVisibleSpectrum
  IF (NumSiteSpectrum > 1) THEN ! throw error
    Call ShowSevereError('Only one '//TRIM(cCurrentModuleObject)//' object is allowed')
    Errorsfound = .true.
  ENDIF

   CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlphas,NumNumbers)
   ALLOCATE(cAlphaArgs(NumAlphas))
   cAlphaArgs=' '
   ALLOCATE(rNumericArgs(NumNumbers))
   rNumericArgs=0.0d0

  IF (NumSiteSpectrum == 1) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)

    ! use default spectrum data, done!
    IF (SameString(cAlphaArgs(2), 'Default')) THEN
      RunMeOnceFlag = .TRUE.
      RETURN
    ENDIF

    ! now read custom solar and visible spectrum data
    cSolarSpectrum = cAlphaArgs(3)
    cVisibleSpectrum = cAlphaArgs(4)

    cCurrentModuleObject = 'Site:SpectrumData'
    NumSiteSpectrum = GetNumObjectsFound(cCurrentModuleObject)
    IF (NumSiteSpectrum == 0) THEN ! throw error
      Call ShowSevereError('No '//TRIM(cCurrentModuleObject)//' object is found')
      Errorsfound = .true.
    ENDIF

   DEALLOCATE (cAlphaArgs)
   DEALLOCATE (rNumericArgs)

   CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumArgs,NumAlphas,NumNumbers)
   ALLOCATE(cAlphaArgs(NumAlphas))
   cAlphaArgs=' '
   ALLOCATE(rNumericArgs(NumNumbers))
   rNumericArgs=0.0d0

    iSolarSpectrum = 0
    iVisibleSpectrum = 0
    DO Loop = 1, NumSiteSpectrum
      ! Step 2 - read user-defined spectrum data
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
      IF (SameString(cAlphaArgs(1), cSolarSpectrum)) THEN
        iSolarSpectrum = Loop
        ! overwrite the default solar spectrum
        IF (NumNumbers > 2*nume) THEN
          Call ShowSevereError('Solar spectrum data pair is more than 107 - '//  &
             TRIM(cCurrentModuleObject)//' - '//TRIM(cAlphaArgs(1)))
          Errorsfound = .true.
        ELSE
          ! Step 3 - overwrite default solar spectrum data
          DO iTmp = 1, nume
            IF (iTmp <= NumNumbers /2) THEN
              wle(iTmp) = rNumericArgs(2*iTmp-1)
              e(iTmp) = rNumericArgs(2*iTmp)
            ELSE
              wle(iTmp) = 0.0D0
              e(iTmp) = 0.0D0
            ENDIF
          END DO
        ENDIF
      ENDIF
      IF (SameString(cAlphaArgs(1), cVisibleSpectrum)) THEN
        iVisibleSpectrum = Loop
        ! overwrite the default solar spectrum
        IF (NumNumbers > 2*numt3) THEN
          Call ShowSevereError('Visible spectrum data pair is more than 81 - '//  &
             TRIM(cCurrentModuleObject)//' - '//TRIM(cAlphaArgs(1)))
          Errorsfound = .true.
        ELSE
          ! Step 3 - overwrite default visible spectrum data
          DO iTmp = 1, numt3
            IF (iTmp <= NumNumbers /2) THEN
              wlt3(iTmp) = rNumericArgs(2*iTmp-1)
              y30(iTmp) = rNumericArgs(2*iTmp)
            ELSE
              wlt3(iTmp) = 0.0D0
              y30(iTmp) = 0.0D0
            ENDIF
          END DO
        ENDIF
      ENDIF
      IF ((iSolarSpectrum > 0) .AND. (iVisibleSpectrum > 0)) EXIT
    END DO
  ENDIF

  DEALLOCATE (cAlphaArgs)
  DEALLOCATE (rNumericArgs)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for user-defined solar/visible spectrum')
  ENDIF

  RunMeOnceFlag = .TRUE.

  RETURN
END SUBROUTINE CheckAndReadCustomSprectrumData


!*****************************************************************************************

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

END MODULE WindowManager
