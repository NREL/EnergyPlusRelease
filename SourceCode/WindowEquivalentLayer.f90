MODULE WindowEquivalentLayer

          ! MODULE INFORMATION
          !       AUTHOR         Bereket A. Nigusse, FSEC/UCF
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !  Manages the equivalent layer (ASHWAT) window model optical and thermal
          !  calculations
          !
          ! METHODOLOGY EMPLOYED:
          !  Uses net radiation method to calculate the optical properties of a multi-layer
          !  window construction. Most of the routines in this module were adopted directly
          !  from ASHRAE 1311-RP.
          !
          ! REFERENCES:
          !
          ! John L. Wright,    Charles S. Barnaby, Michael R. Collins, and Nathan A. Kotey.
          ! Improving Cooling Load Calculations for Fenestration with Shading Devices
          ! ASHRAE 1311-RP, Final Report, February 11, 2009.
          !
          ! Edwards, D.K. 1977.  Solar absorption by each element in an absorber-coverglass
          !  array,Technical Note, Solar Energy, Vol. 19, pp. 401-402.
          !
          ! Kotey, N. A., J. L. Wright, and M. R. Collins.  2008.  “Determining Longwave
          !  RadiativeProperties of Flat Shading Materials,” 33rd Annual SESCI / 3rd CSBC
          !  Conference Proceedings, Fredericton, NB.
          !
          ! Kotey, N.A., Wright, J.L., M. R. Collins. 2009a. "Determination of Angle-Dependent
          !  SolarOptical Properties of Roller Blind Materials," drafted for submission to
          !  ASHRAE Transactions, Vol. 115, Pt. 1.

          ! Kotey, N.A., Wright, J.L., M. R. Collins. 2009b. "Determination of Angle-Dependent
          !  Solar Optical Properties of Drapery Fabrics," in review, ASHRAE Transactions,
          !  Vol. 115, Pt. 2.
          !
          ! Wright, J. L.  2008.  "Calculating Centre-Glass Performance Indices of Glazing
          !  Systems with Shading Devices," ASHRAE Transactions, Vol. 114, Pt. 2.
          !
          ! Wright, J. L., N. Y. T. Huang, and M. R. Collins.  2008.  "Thermal Resistance
          !  of a Window with an Enclosed Venetian Blind: A Simplified Model,"
          !  ASHRAE Transactions, Vol. 114, Pt. 1.

          ! Yahoda, D. S. and J. L. Wright.  2004.  "Methods for Calculating the Effective
          !  Longwave Radiative Properties of a Venetian Blind Layer," ASHRAE Transactions,
          !  Vol. 110, Pt. 1., pp. 463-473.
          !
          ! Yahoda, D. S. and J. L. Wright.  2005.  "Methods for Calculating the Effective
          !  Solar-Optical Properties of a Venetian Blind Layer," ASHRAE Transactions,
          !  Vol. 111, Pt. 1, pp. 572-586.
          !
          ! Yahoda, D. S. and J. L. Wright.  2004.  "Heat Transfer Analysis of a Between-Panes
          !  Venetian Blind Using Effective Longwave Radiative Properties," ASHRAE Transactions,
          !  Vol. 110, Pt. 1., pp. 455-462.
          !
          ! USE STATEMENTS:
          !
    USE DataHeatBalance
    USE DataWindowEquivalentLayer
    USE DataPrecisionGlobals
    USE DataSurfaces
    USE DataGlobals,     ONLY: TimeStep, HourOfDay, Pi, PiOvr2, StefanBoltzmann, KelvinConv, &
                               UniversalGasConst, GravityConstant, CurrentTime, WarmupFlag, &
                               MaxNameLength
    USE DataEnvironment, ONLY: Month, DayOfMonth
    USE DataInterfaces
    USE General,         ONLY: TrimSigDigits

    IMPLICIT NONE   ! Enforce explicit typing of all variables
    PRIVATE
    REAL(r64),    PARAMETER :: RadiansToDeg = 180.d0/Pi ! Conversion for Radians to Degrees
    REAL(r64),    PARAMETER :: PAtmSeaLevel = 101325.d0 ! Standard atmospheric pressure at sea level (Pa)
    INTEGER,      PARAMETER :: hipRHO = 1               ! return reflectance
    INTEGER,      PARAMETER :: hipTAU = 2               ! return transmittance
    REAL(r64),    PARAMETER :: SMALL_ERROR = 0.000001d0 ! small number
    ! CFSGAP: space between layers (gap types)
    INTEGER, PARAMETER :: gtySEALED  = 1    ! sealed
    INTEGER, PARAMETER :: gtyOPENin  = 2    ! open to indoor air  (re Open Channel Flow (OCF))
    INTEGER, PARAMETER :: gtyOPENout = 3    ! open to outdoor air (re Open Channel Flow (OCF))
    ! shade control options
    INTEGER, PARAMETER :: lscNONE   = 0     ! no control
    INTEGER, PARAMETER :: lscVBPROF = 1     ! VB slatA = ProfA (max gain)
    INTEGER, PARAMETER :: lscVBNOBM = 2     ! VB slatA just exclude beam
    ! Constants
    INTEGER, PARAMETER :: hipRHO_BT0 = 1
    INTEGER, PARAMETER :: hipTAU_BT0 = 2
    INTEGER, PARAMETER :: hipTAU_BB0 = 3
    INTEGER, PARAMETER :: hipDIM     = 3    ! dimension of parameter array

    REAL(r64), PUBLIC, ALLOCATABLE, DIMENSION(:,:,:) :: CFSDiffAbsTrans
    LOGICAL,               ALLOCATABLE, DIMENSION(:) :: EQLDiffPropFlag

    ! MODULE SUBROUTINES:
    ! Initialization routines for module
    PUBLIC  InitEquivalentLayerWindowCalculations
    PRIVATE SetEquivalentLayerWindowProperties
    PRIVATE CheckAndFixCFSLayer

    ! Standard Ratings calculation routines
    PRIVATE CalcEQLWindowStandardRatings
    PRIVATE CalcEQLWindowSHGCAndTransNormal
    PRIVATE CalcEQLWindowUvalue
    PRIVATE HCInWindowStandardRatings

    ! Calculation routines for the module
    PUBLIC  CalcEQLOpticalProperty
    PUBLIC  EQLWindowSurfaceHeatBalance
    PUBLIC  EQLWindowInsideEffectiveEmiss
    PUBLIC  EQLWindowOutsideEffectiveEmiss
    PRIVATE CalcEQLWindowOpticalProperty
    PRIVATE ASHWAT_Solar
    PRIVATE ASHWAT_Thermal
    PRIVATE ASHWAT_OffNormalProperties
    PRIVATE Specular_OffNormal
    PRIVATE AdjustVBGap
    PRIVATE BuildGap
    PRIVATE DL_RES_r2
    PRIVATE Fabric_EstimateDiffuseProps
    PRIVATE FillDefaultsSWP
    PRIVATE FinalizeCFS
    PRIVATE FinalizeCFSLAYER
    PRIVATE FM_BEAM
    PRIVATE FM_DIFF
    PRIVATE GLtoAMB
    PRIVATE IS_BEAM
    PRIVATE IS_DIFF
    PRIVATE NETRAD
    PRIVATE OPENNESS_LW
    PRIVATE PD_BEAM
    PRIVATE PD_BEAM_CASE_I
    PRIVATE PD_BEAM_CASE_II
    PRIVATE PD_BEAM_CASE_III
    PRIVATE PD_BEAM_CASE_IV
    PRIVATE PD_BEAM_CASE_V
    PRIVATE PD_BEAM_CASE_VI
    PRIVATE PD_DIFF
    PRIVATE PD_LW
    PRIVATE RB_BEAM
    PRIVATE RB_DIFF
    PRIVATE SETUP4x4_A
    PRIVATE SLtoGL
    PRIVATE SOLMATS
    PRIVATE Specular_Adjust
    PRIVATE Specular_EstimateDiffuseProps
    PRIVATE Specular_RATDiff
    PRIVATE Specular_SWP
    PRIVATE TDMA
    PRIVATE TDMA_R
    PRIVATE AUTOTDMA
    PRIVATE VB_DIFF
    PRIVATE VB_SOL4
    PRIVATE VB_SOL46_CURVE
    PRIVATE VB_SOL6

    CONTAINS
          ! MODULE SUBROUTINES:

SUBROUTINE InitEquivalentLayerWindowCalculations

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the optical properties for the Equivalent Layer (ASHWAT) Window
          ! model
          !
          ! METHODOLOGY EMPLOYED:
          ! Gets the EquivalentLayer Window Layers Inputs.  Fills in the derived data type
          ! based on the inputs specified.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
    INTEGER           :: ConstrNum               ! Construction number
    INTEGER           :: EQLConNum               ! Construction number for equivalent layer windows
    INTEGER           :: SurfNum                 ! surface number
          ! Flow

     IF (TotWinEquivLayerConstructs < 1) RETURN
     IF (.NOT. ALLOCATED( CFS) ) ALLOCATE( CFS(TotWinEquivLayerConstructs))
     IF (.NOT. ALLOCATED( EQLDiffPropFlag) ) ALLOCATE( EQLDiffPropFlag(TotWinEquivLayerConstructs) )
     IF (.NOT. ALLOCATED( CFSDiffAbsTrans) ) ALLOCATE( CFSDiffAbsTrans(TotWinEquivLayerConstructs,CFSMAXNL+1,2) )

     EQLDiffPropFlag = .TRUE.
     CFSDiffAbsTrans = 0.0d0

     DO ConstrNum = 1,TotConstructs
      IF (.NOT. Construct(ConstrNum)%TypeIsWindow) CYCLE  !
      IF (.NOT. Construct(ConstrNum)%WindowTypeEQL) CYCLE ! skip if not equivalent layer window

      CALL SetEquivalentLayerWindowProperties(ConstrNum)

     END DO !  end do for TotConstructs

     DO SurfNum = 1, TotSurfaces
        IF (.NOT. Construct(Surface(SurfNum)%Construction)%TypeIsWindow ) CYCLE
        IF (.NOT. Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) CYCLE

        SurfaceWindow(SurfNum)%WindowModelType = WindowEQLModel

     END DO !  end do for SurfNum

     RETURN
END SUBROUTINE InitEquivalentLayerWindowCalculations

SUBROUTINE SetEquivalentLayerWindowProperties(ConstrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Polpulates the the equivalent layer window model optical and thermal
          ! properties, fills default values and shades geomterical calculations

          ! METHODOLOGY EMPLOYED:
          ! uses some routine developed for ASHRAE RP-1311 (ASHWAT Model)
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
 IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
 INTEGER, INTENT(IN) :: ConstrNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
 INTEGER      :: Layer            ! layer index
 INTEGER      :: MaterNum         ! material index of a layer in a construction
 INTEGER      :: gLayer           ! gap layer index
 INTEGER      :: sLayer           ! glazing and shade layers (non-gas layers) index
 INTEGER      :: EQLNum           ! equivalent layer window construction index
 INTEGER      :: NumGLayers       ! number of gap layers
 INTEGER      :: NumSLayers       ! number of glazing and shade layers (non-gas layers)
 INTEGER      :: DoWhat           ! DoWhat =1, index for diffuse, and =2 index for beam
 REAL(r64)    :: SysAbs1(CFSMAXNL+1,2)   ! layers absorptance and system transmittance
          ! Flow

 IF (.NOT. ALLOCATED( CFSLayers) ) ALLOCATE( CFSLayers(Construct(ConstrNum)%TotLayers))

  sLayer = 0
  gLayer = 0
  EQLNum = Construct(ConstrNum)%EQLConsPtr

  CFS(EQLNum)%Name = Construct(ConstrNum)%Name

  DO Layer = 1, Construct(ConstrNum)%TotLayers

    MaterNum=Construct(ConstrNum)%LayerPoint(Layer)

    IF(Material(Construct(ConstrNum)%LayerPoint(1))%Group /= GlassEquivalentLayer .AND.  &
       Material(Construct(ConstrNum)%LayerPoint(1))%Group /= ShadeEquivalentLayer .AND.  &
       Material(Construct(ConstrNum)%LayerPoint(1))%Group /= DrapeEquivalentLayer .AND.  &
       Material(Construct(ConstrNum)%LayerPoint(1))%Group /= ScreenEquivalentLayer.AND.  &
       Material(Construct(ConstrNum)%LayerPoint(1))%Group /= BlindEquivalentLayer .AND.  &
       Material(Construct(ConstrNum)%LayerPoint(1))%Group /= GapEquivalentLayer) CYCLE

       IF(Material(MaterNum)%Group == GapEquivalentLayer )THEN
        ! Gap or Gas Layer
        gLayer = gLayer + 1
       ELSE
        ! Solid (Glazing or Shade) Layer
        sLayer = sLayer + 1
        CFS(EQLNum)%L(sLayer)%Name = TRIM(Material(MaterNum)%Name)
        ! longwave property input
        CFS(EQLNum)%L(sLayer)%LWP_MAT%EPSLF=Material(MaterNum)%EmissThermalFront
        CFS(EQLNum)%L(sLayer)%LWP_MAT%EPSLB=Material(MaterNum)%EmissThermalBack
        CFS(EQLNum)%L(sLayer)%LWP_MAT%TAUL= Material(MaterNum)%TausThermal
       ENDIF

       IF (Material(MaterNum)%Group == BlindEquivalentLayer) THEN
            IF (Material(MaterNum)%SlatOrientation == Horizontal) THEN
                CFS(EQLNum)%L(sLayer)%LTYPE = ltyVBHOR
            ELSEIF(Material(MaterNum)%SlatOrientation == Vertical) THEN
                CFS(EQLNum)%L(sLayer)%LTYPE = ltyVBVER
            ENDIF
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFBD=Material(MaterNum)%ReflFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBBD=Material(MaterNum)%ReflBackBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBD=Material(MaterNum)%TausFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBD=Material(MaterNum)%TausBackBeamDiff

            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFDD=Material(MaterNum)%ReflFrontDiffDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBDD=Material(MaterNum)%ReflBackDiffDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUS_DD=Material(MaterNum)%TausDiffDiff
            CFS(EQLNum)%L(sLayer)%PHI_DEG=Material(MaterNum)%SlatAngle
            CFS(EQLNum)%L(sLayer)%S=Material(MaterNum)%SlatSeparation
            CFS(EQLNum)%L(sLayer)%W=Material(MaterNum)%SlatWidth
            CFS(EQLNum)%L(sLayer)%C=Material(MaterNum)%SlatCrown
       ELSEIF (Material(MaterNum)%Group == GlassEquivalentLayer) THEN
            ! glazing
            CFS(EQLNum)%L(sLayer)%LTYPE = ltyGLAZE
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFBB=Material(MaterNum)%ReflFrontBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBBB=Material(MaterNum)%ReflBackBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBB=Material(MaterNum)%TausFrontBeamBeam

            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFBD=Material(MaterNum)%ReflFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBBD=Material(MaterNum)%ReflBackBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBD=Material(MaterNum)%TausFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBD=Material(MaterNum)%TausBackBeamDiff

            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFDD=Material(MaterNum)%ReflFrontDiffDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBDD=Material(MaterNum)%ReflBackDiffDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUS_DD=Material(MaterNum)%TausDiffDiff
       ELSEIF (Material(MaterNum)%Group == ShadeEquivalentLayer) THEN
            ! roller blind
            CFS(EQLNum)%L(sLayer)%LTYPE = ltyROLLB
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBB=Material(MaterNum)%TausFrontBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBB=Material(MaterNum)%TausBackBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFBD=Material(MaterNum)%ReflFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBBD=Material(MaterNum)%ReflBackBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBD=Material(MaterNum)%TausFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBD=Material(MaterNum)%TausBackBeamDiff

        ELSEIF (Material(MaterNum)%Group == DrapeEquivalentLayer) THEN
            ! drapery fabric
            CFS(EQLNum)%L(sLayer)%LTYPE = ltyDRAPE
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBB=Material(MaterNum)%TausFrontBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBB=Material(MaterNum)%TausBackBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFBD=Material(MaterNum)%ReflFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBBD=Material(MaterNum)%ReflBackBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBD=Material(MaterNum)%TausFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBD=Material(MaterNum)%TausBackBeamDiff

            CFS(EQLNum)%L(sLayer)%S=Material(MaterNum)%PleatedDrapeLength
            CFS(EQLNum)%L(sLayer)%W=Material(MaterNum)%PleatedDrapeWidth
            !
            ! init diffuse SWP to force default derivation
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFDD = -1.0d0
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBDD = -1.0d0
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUS_DD = -1.0d0
       ELSEIF (Material(MaterNum)%Group == ScreenEquivalentLayer) THEN
            ! insect screen
            CFS(EQLNum)%L(sLayer)%LTYPE = ltyINSCRN
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBB=Material(MaterNum)%TausFrontBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBB=Material(MaterNum)%TausBackBeamBeam
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSFBD=Material(MaterNum)%ReflFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%RHOSBBD=Material(MaterNum)%ReflBackBeamDiff

            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBD=Material(MaterNum)%TausFrontBeamDiff
            CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBD=Material(MaterNum)%TausBackBeamDiff
            ! wire geometry
            CFS(EQLNum)%L(sLayer)%S=Material(MaterNum)%ScreenWireSpacing
            CFS(EQLNum)%L(sLayer)%W=Material(MaterNum)%ScreenWireDiameter
       ELSE IF(Material(MaterNum)%Group == GapEquivalentLayer )THEN
            ! This layer is a gap.  Fill in the parameters
            CFS(EQLNum)%G(gLayer)%Name     = TRIM(Material(MaterNum)%Name)
            CFS(EQLNum)%G(gLayer)%GType    = Material(MaterNum)%GapVentType
            CFS(EQLNum)%G(gLayer)%TAS      = Material(MaterNum)%Thickness
            CFS(EQLNum)%G(gLayer)%FG%Name  = TRIM(Material(MaterNum)%GasName)
            CFS(EQLNum)%G(gLayer)%FG%AK    = Material(MaterNum)%GasCon(1,1)
            CFS(EQLNum)%G(gLayer)%FG%BK    = Material(MaterNum)%GasCon(1,2)
            CFS(EQLNum)%G(gLayer)%FG%CK    = Material(MaterNum)%GasCon(1,3)
            CFS(EQLNum)%G(gLayer)%FG%ACp   = Material(MaterNum)%GasCp(1,1)
            CFS(EQLNum)%G(gLayer)%FG%BCp   = Material(MaterNum)%GasCp(1,2)
            CFS(EQLNum)%G(gLayer)%FG%CCp   = Material(MaterNum)%GasCp(1,3)
            CFS(EQLNum)%G(gLayer)%FG%AVisc = Material(MaterNum)%GasVis(1,1)
            CFS(EQLNum)%G(gLayer)%FG%BVisc = Material(MaterNum)%GasVis(1,2)
            CFS(EQLNum)%G(gLayer)%FG%CVisc = Material(MaterNum)%GasVis(1,3)
            CFS(EQLNum)%G(gLayer)%FG%MHAT  = Material(MaterNum)%GasWght(1)
            ! fills gas density and effective gap thickness
            CALL BuildGap(CFS(EQLNum)%G(gLayer),       &
                          CFS(EQLNum)%G(gLayer)%GType, &
                          CFS(EQLNum)%G(gLayer)%TAS )
       ELSE
            CFS(EQLNum)%L(sLayer)%LTYPE = ltyNONE
       END IF
       ! beam beam transmittance is the same for front and back side
       CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSBBB = CFS(EQLNum)%L(sLayer)%SWP_MAT%TAUSFBB
       NumSLayers = sLayer
       NumGLayers = gLayer
       CFS(EQLNum)%NL = sLayer

       ! checks optical properties and fill in default values for diffuse optical
       ! properties by calculating from other optical inputs, also fills in geometrical inputs
       CALL CheckAndFixCFSLayer(CFS(EQLNum)%L(sLayer))

  END DO   ! end do for Construct(ConstrNum)%TotLayers

  ! Finalize CFS after get input.  Correct effective gap thickness for VB
  CALL FinalizeCFS( CFS(EQLNum))

  ! get total solid layers (glazing layers + shade layers)
  Construct(ConstrNum)%TotSolidLayers = CFS(EQLNum)%NL

  ! Calculate layers diffuse absorptance and system diffuse transmittance
  CALL CalcEQLWindowOpticalProperty( CFS(EQLNum), isDIFF, SysAbs1, 0.0d0, 0.0d0, 0.0d0)
  Construct(ConstrNum)%TransDiffFrontEQL = SysAbs1(CFS(EQLNum)%NL+1,1)
  CFSDiffAbsTrans(EQLNum,:,:) = SysAbs1
  Construct(ConstrNum)%AbsDiffFrontEQL(1:CFSMAXNL) = SysAbs1(1:CFSMAXNL,1)
  Construct(ConstrNum)%AbsDiffBackEQL(1:CFSMAXNL)  = SysAbs1(1:CFSMAXNL,2)
  ! get construction front and back diffuse effective reflectance
  Construct(ConstrNum)%ReflectSolDiffFront = CFS(EQLNum)%L(1)%SWP_EL%RHOSFDD
  Construct(ConstrNum)%ReflectSolDiffBack  = CFS(EQLNum)%L(CFS(EQLNum)%NL)%SWP_EL%RHOSBDD
  ! calculate U-Value, SHGC and Normal Transmittance of EQL Window
  Call CalcEQLWindowStandardRatings(ConstrNum)

  IF ( CFSHasControlledShade(CFS(EQLNum)) > 0 ) CFS(EQLNum)%ISControlled = .TRUE. ! is controlled

  RETURN
END SUBROUTINE SetEquivalentLayerWindowProperties

SUBROUTINE CalcEQLWindowUvalue(FS, UNFRC)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT/Chip Barnaby
          !       DATE WRITTEN   Last Modified February 2008
          !
          !       MODIFIED       Bereket Nigusse, May 2013
          !                      Replaced inside convection calculation
          !                      with ISO Std 15099
          !
          !       RE-ENGINEERED   na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates U-value of equivalent layer window at standard
          ! fenestration winter rating conditions

          ! METHODOLOGY EMPLOYED:
          ! uses routine developed for ASHRAE RP-1311 (ASHWAT Model)
          !
          ! NFRC rated *HEATING* U-factor or Winter Rating Condition
          ! tin = 294.15d0   ! Inside air temperature (69.8F, 21.0C)
          ! tout = 255.15d0  ! Outside air temperature (-0.4F, -18C)
          ! hcout = 26.d0    ! Outside convective film conductance at 5.5 m/s (12.3 mph)
          !                  ! wind speed (the value used in Window 5)
          ! BeamSolarInc = 0.0
          !
          ! REFERENCES:
          ! na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE (CFSTY),    INTENT(IN) :: FS             ! CFS to be calculated
    REAL(r64),      INTENT(OUT) :: UNFRC          ! NFRC U-factor, W/m2-K

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64),        PARAMETER :: Height = 1.0d0   ! window height, m
    REAL(r64),        PARAMETER :: TOUT   =-18.0d0  ! outdoor air temperature, C
    REAL(r64),        PARAMETER :: TIN    = 21.0d0  ! indoor air temperature, C
    CHARACTER(len=*), PARAMETER :: RoutineName='CalcEQLWindowUvalue: '
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
    REAL(r64)                 :: U          ! U-factor, W/m2-K
    REAL(r64)                 :: UOld       ! U-factor during pevious iteration step, W/m2-K
    REAL(r64)                 :: HXO        ! outdoor combined conv+rad surf coeff, W/m2-K
    REAL(r64)                 :: HXI        ! indoor combined conf+rad surf coeff, W/m2-K
    REAL(r64)                 :: HRO        ! outdoor side radiation surf coeff, W/m2-K
    REAL(r64)                 :: HCO        ! outdoor side convection surf coeff, W/m2-K
    REAL(r64)                 :: HRI        ! indoor side radiation surf coeff, W/m2-K
    REAL(r64)                 :: HCI        ! indoor side convection surf coeff, W/m2-K
    REAL(r64)                 :: TGO
    REAL(r64)                 :: TGI
    REAL(r64)                 :: TGIK
    REAL(r64)                 :: TIK
    REAL(r64)                 :: DT         ! temperature difference, K
    REAL(r64)                 :: EO         ! outside face effective emissivity, (-)
    REAL(r64)                 :: EI         ! inside face effective emissivity, (-)
    INTEGER                   :: I          ! index
    LOGICAL                   :: CFSURated  ! false if U-Value calculation failed
          ! Flow

    CFSURated = .FALSE.

    ! Intial guess value for combined conductance
    HXO = 29.0d0    ! 1/FenROut
    HXI =  7.0d0    ! 1/FenRIn
    HCO = 26.0d0    !
    HCI =  3.0d0    ! Initial guess

    DT = TIN - TOUT                        ! note DT == 0 detected in CFSUFactor()
    EO = FS%L(1)%LWP_EL%EPSLF              ! emissivities outside
    EI = FS%L(FS%NL)%LWP_EL%EPSLB          ! emissivities inside
    U  = 5.0d0 / FS%NL                     ! initial guess

    ! Iterate: find surface temperature, update coeffs, converge to U
    DO I = 1, 10
        TGO = TOUT + U * DT / HXO          ! update glazing surface temps
        TGI = TIN - U * DT / HXI
        HRO = StefanBoltzmann*EO*((TGO + KelvinConv)**2 + (TOUT + KelvinConv)**2) &
            * ((TGO + KelvinConv)+(TOUT + KelvinConv))
        HRI = StefanBoltzmann*EI*((TGI + KelvinConv)**2 + (TIN + KelvinConv)**2)  &
            * ((TGI + KelvinConv)+(TIN + KelvinConv))
        !HCI = HIC_ASHRAE( Height, TGI, TI)  ! BAN June 2103 Raplaced with ISO Std 15099
        TGIK = TGI + KelvinConv
        TIK  = TIN + KelvinConv
        HCI = HCInWindowStandardRatings(Height, TGIK, TIK)
        IF (HCI < 0.001d0) EXIT
        HXI = HCI + HRI
        HXO = HCO + HRO
        UOld = U
        IF (.NOT. CFSUFactor( FS, TOUT, HCO, TIN, HCI, U)) EXIT
        IF (I > 1 .AND. FEQX( U, UOld, 0.001d0)) THEN
            CFSURated = .TRUE.
            EXIT
        END IF
    END DO
    IF (.NOT. CFSURated) THEN
        CALL ShowWarningMessage(RoutineName//'Fenestration U-Value calculation failed for '//TRIM(FS%Name))
        CALL ShowContinueError('...Calculated U-value = '//TRIM(TrimSigDigits(U,4)))
        CALL ShowContinueError('...Check consistency of inputs')
     END IF
    UNFRC = U
    RETURN
END SUBROUTINE CalcEQLWindowUvalue

SUBROUTINE CalcEQLWindowSHGCAndTransNormal(FS, SHGCSummer, TransNormal)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates SHGC and Normal Transmittance of equivalent layer
          ! fenestration.

          ! METHODOLOGY EMPLOYED:
          ! Uses routine developed for ASHRAE RP-1311 (ASHWAT Model)
          !
          ! Summer Window Rating Conditoions
          ! tin = 297.15d0         ! indoor air condition (75.2F,  24.0C)
          ! tout = 305.15d0        ! Outside air temperature (89.6F, 32C)
          ! hcout = 15.d0          ! Outside convective film conductance at 2.8 m/s (6.2 mph) wind speed
          ! BeamSolarInc = 783.0d0 ! Direct normal incident solar radiation, W/m2
          !
          ! REFERENCES:
          ! na
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE (CFSTY), INTENT( INOUT) :: FS                ! fenestration system
    REAL(r64),       INTENT(OUT) :: TransNormal       ! transmittance at normal incidence
    REAL(r64),       INTENT(OUT) :: SHGCSummer        ! solar heat gain coefficient

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64),         PARAMETER :: TOL = 0.01d0
    REAL(r64),         PARAMETER :: TIN = 297.15d0
    REAL(r64),         PARAMETER :: TOUT= 305.15d0
    REAL(r64),         PARAMETER :: BeamSolarInc = 783.0d0
    CHARACTER(len=*),  PARAMETER :: RoutineName='CalcEQLWindowSHGCAndTransNormal: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)     :: HCOUT
    REAL(r64)     :: TRMOUT
    REAL(r64)     :: TRMIN
    REAL(r64)     :: HCIN
    TYPE(CFSSWP)  :: SWP_ON(CFSMAXNL)
    REAL(r64)     :: QOCF(CFSMAXNL)
    REAL(r64)     :: JB(0:CFSMAXNL)
    REAL(r64)     :: JF(1:CFSMAXNL+1)
    REAL(r64)     :: T(CFSMAXNL)
    REAL(r64)     :: Q(0:CFSMAXNL)
    REAL(r64)     :: H(0:CFSMAXNL+1)
    REAL(r64)     :: Abs1(CFSMAXNL+1, 2)
    REAL(r64)     :: QRSW
    REAL(r64)     :: QRLW
    REAL(r64)     :: QCONV
    REAL(r64)     :: QOCFRoom
    REAL(r64)     :: QROOM
    REAL(r64)     :: UCG
    REAL(r64)     :: SHGC
    REAL(r64)     :: SHGCCheck
    REAL(r64)     :: IncA
    REAL(r64)     :: VProfA
    REAL(r64)     :: HProfA
    INTEGER       :: NL
    INTEGER       :: I
    INTEGER       :: iL
    INTEGER       :: iLC1
    LOGICAL       :: DoShadeControlR
    LOGICAL       :: CFSSHGC
          ! Flow

    CFSSHGC = .TRUE.
    NL = FS%NL
    IncA   = 0.0d0
    VProfA = 0.0d0
    HProfA = 0.0d0
    ABS1   = 0.0d0
    HCIN   = 3.0d0    ! Initial guess
    HCOUT  = 15.0d0
    IF ( FS%L(1)%LTYPE == ltyROLLB .OR. & ! Exterior Roller Blind Present
         FS%L(1)%LTYPE == ltyDRAPE .OR. & ! Exterior Drape Fabric
         FS%L(1)%LTYPE == ltyINSCRN.OR. & ! Exterior Insect Screen Present
         FS%L(1)%LTYPE == ltyVBHOR .OR. FS%L(1)%LTYPE == ltyVBVER  ) THEN ! Exterior Venetian Blind Present
         ! Reduced convection coefficient due to external attachment
         HCOUT = 12.25d0
    ENDIF

    ! Temperatures
    TRMOUT = TOUT
    TRMIN  = TIN

    !  Convert direct-normal solar properties for beam incidence to current incident angle
    DO I = 1, NL
        CALL ASHWAT_OffNormalProperties( FS%L(I), IncA, VProfA, HProfA, SWP_ON( I))
    END DO
    CALL ASHWAT_Solar( FS%NL, SWP_ON, SWP_ROOMBLK, 1.0d0,  0.0d0,  0.0d0, Abs1( :, 1), Abs1( :, 2))
    TransNormal = Abs1(NL+1,1)

    ! Calculate SHGC using net radiation method (ASHWAT Model)
    CFSSHGC = ASHWAT_Thermal( FS, TIN, TOUT, HCIN, HCOUT, TRMOUT, TRMIN, BeamSolarInc, &
                              BeamSolarInc*Abs1(1:NL+1,1), TOL, QOCF, QOCFRoom, &
                              T, Q, JF, JB, H, UCG, SHGC, .TRUE.)

    IF (.NOT. CFSSHGC) THEN
        CALL ShowWarningMessage(RoutineName//'Solar heat gain coefficient calculation failed for '//FS%Name)
        CALL ShowContinueError('...Calculated SHGC = '//TRIM(TrimSigDigits(SHGC,4)))
        CALL ShowContinueError('...Calculated U-Value = '//TRIM(TrimSigDigits(UCG,4)))
        CALL ShowContinueError('...Check consistency of inputs.')
     RETURN
    ENDIF
    SHGCSummer = SHGC

    RETURN
END SUBROUTINE CalcEQLWindowSHGCAndTransNormal

SUBROUTINE CalcEQLWindowOpticalProperty(FS, DiffBeamFlag, Abs1, IncA, VProfA, HProfA)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         University of WaterLoo
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse, May 2013
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates absorptance for each layer, and transmittance of the
          ! fenestration for beam and diffuse solar radiation

          ! METHODOLOGY EMPLOYED:
          ! uses routine developed for ASHRAE RP-1311 (ASHWAT Model).  Uses net radiation
          ! method.
          !
          ! REFERENCES:
          ! na
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE (CFSTY), INTENT(INOUT) :: FS            ! fenestration system
    INTEGER,         INTENT(IN) :: DiffBeamFlag  ! isDIFF: calc diffuse properties
                                                 ! else: isBEAM
    REAL(r64),      INTENT(OUT) :: Abs1( CFSMAXNL+1, 2)
                                                 ! returned: layer abs for unit (1 W/m2) incident
                                                 !   if beam, Abs1( :, 1) = abs for IncA
                                                 !            Abs1( :, 2) = trans Beam-Diffuse only
                                                 !   if diff, Abs1( :, 1) = abs for outside diff
                                                 !            Abs1( :, 2) = abs for inside diff
    REAL(r64),      INTENT(IN)  :: IncA          ! angle of incidence, radians
    REAL(r64),      INTENT(IN)  :: VProfA        ! inc solar vertical profile angle, radians
                                                 !   + = up-from-horizontal
    REAL(r64),      INTENT(IN)  :: HProfA        ! inc solar horizontal profile angle, radians
                                                 !   + = west-of-normal
                                                 ! convect coefficients, W/m2-K

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    TYPE( CFSSWP) :: SWP_ON(CFSMAXNL)
    INTEGER       :: NL
    INTEGER       :: I
    INTEGER       :: iL
    LOGICAL       :: DoShadeControlR
          ! Flow

    NL = FS%NL
    ABS1 = 0.0d0

    IF (FS%ISControlled) THEN        ! at least 1 controlled layer found
        DO iL = 1, NL
            ! If there is shade control (Venetian Blind Only).
            IF ( IsControlledShade(FS%L(iL)) ) THEN
                 DoShadeControlR = DoShadeControl( FS%L( iL), IncA, VProfA, HProfA)
            ENDIF
        END DO
    END IF

    IF (DiffBeamFlag /= isDIFF) THEN
        !  Beam: Convert direct-normal solar properties to off-normal properties
        DO I = 1, NL
            CALL ASHWAT_OffNormalProperties( FS%L( I), IncA, VProfA, HProfA, SWP_ON( I))
        END DO
        CALL ASHWAT_Solar( FS%NL, SWP_ON, SWP_ROOMBLK, 1.0d0,  0.0d0,  0.0d0, Abs1( :, 1), Abs1( :, 2))
    ELSE
        ! diffuse
        CALL ASHWAT_Solar( FS%NL, FS%L%SWP_EL, SWP_ROOMBLK, 0.0d0, 1.0d0, 0.0d0, Abs1( :, 1))
        CALL ASHWAT_Solar( FS%NL, FS%L%SWP_EL, SWP_ROOMBLK, 0.0d0, 0.0d0, 1.0d0, Abs1( :, 2))
        !CFSFenProp = LOK1 .AND. LOK2
    END IF
    RETURN
END SUBROUTINE CalcEQLWindowOpticalProperty

SUBROUTINE EQLWindowSurfaceHeatBalance(SurfNum,HcOut, SurfInsideTemp, SurfOutsideTemp, &
                                       SurfOutsideEmiss, CalcCondition)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! performs surface heat balance and returns in the inside and outside surface
          ! temperatures

          ! METHODOLOGY EMPLOYED:
          ! uses the solar-thermal routine developed for ASHRAE RP-1311 (ASHWAT Model).
          !
          !
          ! REFERENCES:
          ! na
          !
          ! USE STATEMENTS:
  USE DataZoneEquipment,  ONLY : ZoneEquipConfig
  USE DataLoopNode,       ONLY : Node
  USE Psychrometrics,     ONLY : PsyCpAirFnWTdb,PsyTdpFnWPb
  USE General,            ONLY : InterpSlatAng , InterpSw
  USE InputProcessor,     ONLY : SameString
  USE DataHeatBalSurface, ONLY : HcExtSurf
  USE DataGlobals,        ONLY : StefanBoltzmann
  USE DataEnvironment,    ONLY : SunIsUpValue, SkyTempKelvin, IsRain, SunIsUp
  USE DataHeatBalFanSys

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)     :: SurfNum              ! Surface number
  INTEGER,   INTENT(IN)     :: CalcCondition        ! Calucation condition (summer, winter or no condition)
  REAL(r64), INTENT(IN)     :: HcOut                ! outside convection coeficient at this timestep, W/m2K
  REAL(r64), INTENT(INOUT)  :: SurfInsideTemp       ! Inside window surface temperature (innermost face) [C]
  REAL(r64), INTENT(INOUT)  :: SurfOutsideTemp      ! Outside surface temperature (C)
  REAL(r64), INTENT(INOUT)  :: SurfOutsideEmiss     !

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),      PARAMETER :: TOL = 0.0001d0       ! convergence tolerance

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: NL                           ! Number of layers
  REAL(r64)         :: TIN, TMrtK, TRMIN, TOUT, TRMOUT, UCG, SHGC, QRLW, QIGLW, QRLWX, QCONV, TSX
  REAL(r64)         :: QOCF( CFSMAXNL), QOCFRoom
  REAL(r64)         :: JB(0:CFSMAXNL), JF(1:CFSMAXNL+1), T( CFSMAXNL), Q(0:CFSMAXNL), H(0:CFSMAXNL+1)
  REAL(r64)         :: QAllSWwinAbs(1:CFSMAXNL+1)

  LOGICAL           :: ASHWAT_ThermalR     ! net long wave radiation flux on the inside face of window
  INTEGER           :: EQLNum              ! equivalent layer window index
  INTEGER           :: ZoneNum             ! Zone number corresponding to SurfNum
  INTEGER           :: ConstrNum           ! Construction number

  INTEGER           :: ZoneEquipConfigNum
  INTEGER           :: NodeNum
  REAL(r64)         :: SumSysMCp           ! Zone sum of air system MassFlowRate*Cp
  REAL(r64)         :: SumSysMCpT          ! Zone sum of air system MassFlowRate*Cp*T
  REAL(r64)         :: MassFlowRate
  REAL(r64)         :: NodeTemp
  REAL(r64)         :: CpAir
  REAL(r64)         :: RefAirTemp          ! reference air temperatures
  INTEGER           :: tmpGasType
  INTEGER           :: SurfNumAdj          ! An interzone surface's number in the adjacent zone
  INTEGER           :: ZoneNumAdj          ! An interzone surface's adjacent zone number
  REAL(r64)         :: LWAbsIn             ! effective long wave absorptance/emissivity back side
  REAL(r64)         :: LWAbsOut            ! effective long wave absorptance/emissivity front side
  REAL(r64)         :: QLWAbsIn            ! Inside surface long wave absorbed flux, W/m2
  REAL(r64)         :: outir
  REAL(r64)         :: RMIR
  REAL(r64)         :: EBOUT
  REAL(r64)         :: QXConv              ! extra convective gain from this surface
  REAL(r64)         :: TaIn                ! zone air temperature
  REAL(r64)         :: tsky                ! sky temperature
  REAL(r64)         :: HcIn                ! inside convection coeficient at this timestep, W/m2K
  REAL(r64)         :: ConvHeatFlowNatural ! Convective heat flow from gap between glass and interior shade or blind (W)
  REAL(r64)         :: ConvHeatFlowForced  ! Convective heat flow from forced airflow gap (W)
  REAL(r64)         :: NetIRHeatGainWindow ! net radiation gain from the window surface to the zone (W)
  REAL(r64)         :: ConvHeatGainWindow  ! net convection heat gain from inside surface of window to zone air (W)
  INTEGER           :: InSideLayerType     ! interior shade type
          ! Flow

  If (CalcCondition /= noCondition) RETURN

  ConstrNum = Surface(SurfNum)%Construction
  QXConv = 0.0d0
  ConvHeatFlowNatural = 0.0d0

  EQLNum = Construct(ConstrNum)%EQLConsPtr
  HcIn = HConvIn(SurfNum)                  ! windows inside surface convective film conductance

  If (CalcCondition == noCondition) Then
    ZoneNum = Surface(SurfNum)%Zone
    SurfNumAdj = Surface(SurfNum)%ExtBoundCond

    ! determine reference air temperature for this surface
    SELECT CASE (Surface(SurfNum)%TAirRef)
      CASE (ZoneMeanAirTemp)
          RefAirTemp = MAT(ZoneNum)
      CASE (AdjacentAirTemp)
          RefAirTemp = TempEffBulkAir(SurfNum)
      CASE (ZoneSupplyAirTemp)
          ZoneEquipConfigNum = ZoneNum
          ! check whether this zone is a controlled zone or not
          IF (.NOT. Zone(ZoneNum)%IsControlled) THEN
             RETURN
          END IF
          ! determine supply air conditions
          SumSysMCp = 0.0d0
          SumSysMCpT = 0.0d0
          DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
              NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
              MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
              CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp,'EQLWindowSurfaceHeatBalance')
              SumSysMCp = SumSysMCp + MassFlowRate * CpAir
              SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
          END DO
            ! a weighted average of the inlet temperatures.
            RefAirTemp = SumSysMCpT/SumSysMCp
        CASE DEFAULT
            ! currently set to mean air temp but should add error warning here
            RefAirTemp = MAT(ZoneNum)
    END SELECT
    TaIn = RefAirTemp
    TIN = TaIn + KelvinConv           ! Inside air temperature, K

    ! now get "outside" air temperature
    IF(SurfNumAdj > 0) THEN
      ! this is interzone window. the outside condition is determined from the adjacent zone
      ! condition
      ZoneNumAdj = Surface(SurfNumAdj)%Zone

       ! determine reference air temperature for this surface
      SELECT CASE (Surface(SurfNumAdj)%TAirRef)
        CASE (ZoneMeanAirTemp)
            RefAirTemp = MAT(ZoneNumAdj)
        CASE (AdjacentAirTemp)
            RefAirTemp = TempEffBulkAir(SurfNumAdj)
        CASE (ZoneSupplyAirTemp)
            ! determine ZoneEquipConfigNum for this zone
            ZoneEquipConfigNum = ZoneNum
            ! check whether this zone is a controlled zone or not
            IF (.NOT. Zone(ZoneNum)%IsControlled) THEN
              RETURN
            END IF
            ! determine supply air conditions
            SumSysMCp = 0.0d0
            SumSysMCpT = 0.0d0
            DO NodeNum = 1, ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
                  NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
                  MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
                  CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNumAdj), NodeTemp, 'EQLWindowSurfaceHeatBalance')
                  SumSysMCp = SumSysMCp + MassFlowRate * CpAir
                  SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
            END DO
            ! a weighted average of the inlet temperatures.
            RefAirTemp = SumSysMCpT/SumSysMCp
        CASE DEFAULT
            ! currently set to mean air temp but should add error warning here
            RefAirTemp = MAT(ZoneNumAdj)
      END SELECT

      Tout = RefAirTemp + KelvinConv      ! outside air temperature
      tsky = MRT(ZoneNumAdj) + KelvinConv ! TODO this misses IR from sources such as high temp radiant and baseboards

      ! The IR radiance of this window's "exterior" surround is the IR radiance
      ! from surfaces and high-temp radiant sources in the adjacent zone
      outir = SurfaceWindow(SurfNumAdj)%IRfromParentZone + QHTRadSysSurf(SurfNumAdj) &
            + QHWBaseboardSurf(SurfNumAdj) + QRadThermInAbs(SurfNumAdj)

    ELSE  ! Exterior window (ExtBoundCond = 0)

      IF(Surface(SurfNum)%ExtWind) THEN  ! Window is exposed to wind (and possibly rain)
        IF(IsRain) THEN                  ! Raining: since wind exposed, outside window surface gets wet
          Tout = Surface(SurfNum)%OutWetBulbTemp + KelvinConv
        ELSE                             ! Dry
          Tout = Surface(SurfNum)%OutDryBulbTemp + KelvinConv
        END IF
      ELSE                               ! Window not exposed to wind
        Tout = Surface(SurfNum)%OutDryBulbTemp + KelvinConv
      END IF
      tsky = SkyTempKelvin
      Ebout = StefanBoltzmann * Tout**4
      ! ASHWAT model may be slightly different
      outir = Surface(SurfNum)%ViewFactorSkyIR * (AirSkyRadSplit(SurfNum)*StefanBoltzmann*tsky**4 &
            + (1.-AirSkyRadSplit(SurfNum))*Ebout) + Surface(SurfNum)%ViewFactorGroundIR * Ebout
    END IF
   End If
   ! Outdoor conditions
   TRMOUT = ( outir / StefanBoltzmann)**0.25d0  ! it is in Kelvin scale
   ! indoor conditions
   LWAbsIn = EffectiveEPSLB(CFS(EQLNum))   ! windows inside face effective thermal emissivity
   LWAbsOut = EffectiveEPSLF(CFS(EQLNum))  ! windows outside face effective thermal emissivity
   SurfOutsideEmiss = LWAbsOut
   ! Indoor mean radiant temperature.
   ! IR incident on window from zone surfaces and high-temp radiant sources
   rmir = SurfaceWindow(SurfNum)%IRfromParentZone + QHTRadSysSurf(SurfNum) + QHWBaseboardSurf(SurfNum) &
        + QSteamBaseboardSurf(SurfNum) + QElecBaseboardSurf(SurfNum) + QRadThermInAbs(SurfNum)
   trmin = ( rmir / StefanBoltzmann)**0.25d0  ! TODO check model equation.

   NL = CFS(EQLNum)%NL
   QAllSWwinAbs(1:NL+1) = QRadSWwinAbs(SurfNum,1:NL+1)
   !  Solve energy balance(s) for temperature at each node/layer and
   !  heat flux, including components, between each pair of nodes/layers
   ASHWAT_ThermalR = ASHWAT_Thermal( CFS(EQLNum), TIN, TOUT, HcIn, HcOut, TRMOUT, TRMIN, 0.0d0, &
                     QAllSWwinAbs(1:NL+1), TOL, QOCF, QOCFRoom, T, Q, JF, JB, H, UCG, SHGC)
   ! long wave radiant power to room not including reflected
   QRLWX = JB( NL) - (1.0d0 - LWAbsIn) * JF( NL+1)
   ! nominal surface temp = effective radiant temperature
   SurfInsideTemp = TRadC( QRLWX, LWAbsIn)
   ! Convective to room
   QCONV = H( NL) * (T( NL) - TIN)
   ! Other convective = total conv - standard model prediction
   QXConv = QCONV - HcIn * (SurfInsideTemp - TaIn)
   ! Save the extra convection term. This term is added to the zone air heat
   ! balance equation
   SurfaceWindow(SurfNum)%OtherConvHeatGain = Surface(SurfNum)%Area * QXConv
   SurfOutsideTemp = T(1) - KelvinConv
   ! Various reporting calculations
   InSideLayerType = CFS(EQLNum)%L(NL)%LTYPE
   IF ( InSideLayerType == ltyGLAZE ) THEN
      ConvHeatFlowNatural = 0.0d0
   ELSE
      ConvHeatFlowNatural = Surface(SurfNum)%Area * QOCFRoom
   ENDIF
   SurfaceWindow(SurfNum)%EffInsSurfTemp = SurfInsideTemp
   NetIRHeatGainWindow = Surface(SurfNum)%Area * LWAbsIn * (StefanBoltzmann * (SurfInsideTemp + KelvinConv)**4 - rmir)
   ConvHeatGainWindow = Surface(SurfNum)%Area * HcIn * (SurfInsideTemp - TaIn)
   ! Window heat gain (or loss) is calculated here
   WinHeatGain(SurfNum) = WinTransSolar(SurfNum) + ConvHeatGainWindow &
                        + NetIRHeatGainWindow + ConvHeatFlowNatural
   SurfaceWindow(SurfNum)%ConvHeatFlowNatural = ConvHeatFlowNatural
   ! store for component reporting
   WinGainConvGlazShadGapToZoneRep(SurfNum) = ConvHeatFlowNatural
   WinGainConvShadeToZoneRep(SurfNum)       = ConvHeatGainWindow
   WinGainIRGlazToZoneRep(SurfNum)          = NetIRHeatGainWindow
   WinGainIRShadeToZoneRep(SurfNum)         = NetIRHeatGainWindow
   IF ( InSideLayerType == ltyGLAZE ) THEN
     ! no interior sade
     WinGainIRShadeToZoneRep(SurfNum)       = 0.0d0
   ELSE
     ! Interior shade exists
     WinGainIRGlazToZoneRep(SurfNum)        = 0.0d0
   ENDIF
   ! Advanced report variable (DisplayAdvancedReportVariables)
   OtherConvGainInsideFaceToZoneRep(SurfNum) = SurfaceWindow(SurfNum)%OtherConvHeatGain

   RETURN
END SUBROUTINE EQLWindowSurfaceHeatBalance

SUBROUTINE OPENNESS_LW(OPENNESS, EPSLW0, TAULW0, EPSLW, TAULW)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright and Nathan Kotey, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Modifies long wave properties for shade types characterized by openness.
          ! Applies to shade type: insect screen, roller blind, drape fabric
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:

    REAL(r64),  INTENT(IN) :: OPENNESS  ! shade openness (=tausbb at normal incidence)
    REAL(r64),  INTENT(IN) :: EPSLW0    ! apparent LW emittance of shade at 0 openness
                                        !   (= wire or thread emittance)
                                        !   typical (default) values
                                        !      dark insect screen = .93
                                        !      metalic insect screen = .32
                                        !      roller blinds = .91
                                        !      drape fabric = .87
    REAL(r64),  INTENT(IN) :: TAULW0    ! apparent LW transmittance of shade at 0 openness
                                        !   typical (default) values
                                        !      dark insect screen = .02
                                        !      metalic insect screen = .19
                                        !      roller blinds = .05
                                        !      drape fabric = .05
    REAL(r64), INTENT (OUT) :: EPSLW    ! returned: effective LW emittance of shade
    REAL(r64), INTENT (OUT) :: TAULW    ! returned: effective LW transmittance of shade

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          !
          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
          !
    EPSLW = EPSLW0*(1.0d0 - OPENNESS)
    TAULW = TAULW0*(1.0d0 - OPENNESS) + OPENNESS
    RETURN
END SUBROUTINE OPENNESS_LW

REAL(r64) FUNCTION P01(P, WHAT)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse, May 2013
          !                      Added error messages
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Constrains property to range 0 - 1
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64),        INTENT(IN) :: P           ! property
    CHARACTER(len=*), INTENT(IN) :: WHAT        ! identifier for err msg

          ! FUNCTION PARAMETER DEFINITIONS:
    CHARACTER(len=*),  PARAMETER :: RoutineName='P01: '
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow

    IF (P < -0.05d0 .OR. P > 1.05d0)    THEN
       CALL ShowWarningMessage(RoutineName//'property value should have been between 0 and 1')
       CALL ShowContinueError(WHAT//'=: '//' property value is ='//TRIM(TrimSigDigits(P,4)) )
       IF ( P < 0.0d0 ) Then
          CALL ShowContinueError('property value is reset to 0.0')
       ELSE IF ( P > 1.0d0 ) Then
          CALL ShowContinueError('property value is reset to 1.0')
       ENDIF
    ENDIF
    P01 = MAX(0.0d0, MIN( 1.0d0, P))

    RETURN
END FUNCTION P01

REAL(r64) FUNCTION HEMINT(F, F_Opt, F_P)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Romberg Integration of Property function over hemispeherical dome
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  Romberg Integration.
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64),    EXTERNAL :: F                ! property integrand function
    INTEGER,   INTENT( IN) :: F_Opt            ! options passed to F() (hipRHO, hipTAU)
    REAL(r64), INTENT( IN) :: F_P(hipDIM)      ! parameters passed to F()

          ! FUNCTION PARAMETER DEFINITIONS:
    INTEGER,   PARAMETER :: KMAX = 8           ! max steps
    INTEGER,   PARAMETER :: NPANMAX = 2**KMAX
    REAL(r64), PARAMETER :: TOL = 0.0005d0     ! convergence tolerance
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: T( KMAX, KMAX), FX
    REAL(r64) :: X1, X2, X, DX, SUM, DIFF
    INTEGER   :: nPan, I, K, L, iPX
          ! Flow

    X1 = 0.0d0            ! integration limits
    X2 = PiOvr2
    nPan=1
    SUM = 0.0d0
    DO K = 1, KMAX
        DX = (X2-X1)/nPan
        iPX = NPANMAX / nPan
        DO I = 0, nPan
            IF (K == 1 .OR. MOD( I*iPX, iPX*2) /= 0) THEN
                !   evaluate integrand function for new X values
                !   2 * sin( x) * cos( x) covers hemisphere with single integral
                X = X1 + I*DX
                FX  = 2.0d0 * SIN( X) * COS( X) * F( X, F_Opt, F_P)
                IF (K == 1) FX = FX / 2.0d0
                SUM = SUM + FX
            END IF
        END DO

        T(1,K) = DX * SUM
        ! trapezoid result - i.e., first column Romberg entry
        ! Now complete the row
        IF (K > 1) THEN
            DO L=2,K
                T(L,K) = ((4.0d0**(L-1))*T(L-1,K) - T(L-1,K-1)) / (4.0d0**(L-1)-1.0d0)
            END DO
            !    check for convergence
            !    do 8 panels minimum, else can miss F() features
            IF (nPan >= 8) THEN
                DIFF = ABS( T(K,K) - T(K-1, K-1))
                IF (DIFF < TOL) EXIT
            END IF
        END IF
        nPan = 2 * nPan
    END DO
    IF (K > KMAX) THEN
        K = KMAX
    END IF
    HEMINT = P01(T(K, K), "HEMINT")
    RETURN
END FUNCTION HEMINT

SUBROUTINE RB_DIFF(RHO_BT0, TAU_BT0, TAU_BB0, RHO_DD, TAU_DD)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright and Nathan Kotey, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates roller blind diffuse-diffuse solar optical properties by integrating
          ! the corresponding properties over the hemisphere
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: RHO_BT0    ! normal incidence beam-total reflectance
    REAL(r64), INTENT(IN)  :: TAU_BT0    ! normal incidence beam-total transmittance
                                         !   TAUFF_BT0 = TAUFF_BB0 + TAUFF_BD0
    REAL(r64), INTENT(IN)  :: TAU_BB0    ! normal incidence beam-beam transmittance
    REAL(r64), INTENT(OUT) :: RHO_DD     ! returned: diffuse-diffuse reflectance
    REAL(r64), INTENT(OUT) :: TAU_DD     ! returned: diffuse-diffuse transmittance

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*),  PARAMETER :: RoutineName='RB_DIFF: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: P( hipDIM)
    REAL(r64) :: SumRefAndTran           ! sum of the reflectance and transmittance
          ! Flow

    RHO_DD         = RHO_BT0
    P( hipRHO_BT0) = RHO_BT0
    P( hipTAU_BT0) = TAU_BT0
    P( hipTAU_BB0) = TAU_BB0

    TAU_DD = HEMINT( RB_F, 0, P)

    IF (RHO_DD + TAU_DD > 1.0d0) THEN
        SumRefAndTran = RHO_DD + TAU_DD
        CALL ShowWarningMessage(RoutineName//'Roller blind diffuse-diffuse properties are inconsistent')
        CALL ShowContinueError('...The diffuse-diffuse reflectance = '//TRIM(TrimSigDigits(RHO_DD,4)))
        CALL ShowContinueError('...The diffuse-diffuse tansmittance = '//TRIM(TrimSigDigits(TAU_DD,4)))
        CALL ShowContinueError('...Sum of diffuse reflectance and tansmittance = '//TRIM(TrimSigDigits(SumRefAndTran,4)))
        CALL ShowContinueError('...This sum cannot be > 1.0. Transmittance will be reset to 1 minus reflectance')
        TAU_DD = 1.0d0 - RHO_DD
    END IF
    RETURN
END SUBROUTINE RB_DIFF

REAL(r64) FUNCTION RB_F(THETA, OPT, P)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Roller blind integrand
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64),   INTENT(IN):: THETA        ! incidence angle, radians
    INTEGER,     INTENT(IN):: OPT          ! options (unused)
    REAL(r64),   INTENT(IN):: P( hipDIM)   ! parameters

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)              :: RHO_BD
    REAL(r64)              :: TAU_BB
    REAL(r64)              :: TAU_BD
          ! Flow

    CALL RB_BEAM(THETA, P( hipRHO_BT0), P( hipTAU_BT0), P( hipTAU_BB0), &
                 RHO_BD, TAU_BB, TAU_BD)

    RB_F = TAU_BB+TAU_BD
    RETURN
END FUNCTION RB_F

SUBROUTINE RB_BEAM(xTHETA, RHO_BT0, TAU_BT0, TAU_BB0, RHO_BD, TAU_BB, TAU_BD)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the roller blind off-normal properties using semi-empirical relations
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: xTHETA     ! angle of incidence, radians (0 - PI/2)
    REAL(r64),  INTENT(IN):: RHO_BT0    ! normal incidence beam-total front reflectance
    REAL(r64),  INTENT(IN):: TAU_BT0    ! normal incidence beam-total transmittance
                                        !   TAU_BT0 = TAU_BB0 + TAU_BD0
    REAL(r64),  INTENT(IN):: TAU_BB0    ! normal incidence beam-beam transmittance
                                        !   (openness)
    REAL(r64), INTENT(OUT):: RHO_BD     ! returned: beam-diffuse front reflectance
    REAL(r64), INTENT(OUT):: TAU_BB     ! returned: beam-beam transmittance
    REAL(r64), INTENT(OUT):: TAU_BD     ! returned: beam-diffuse transmittance
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: THETA                  ! working angle of incidence (limited < 90 deg)
    REAL(r64) :: TAUM0                  ! apparent blind material transmittance at normal incidence
    REAL(r64) :: THETA_CUTOFF           ! cutoff angle, radians (angle beyond which total transmittance goes to zero)
    REAL(r64) :: TAUBT_EXPO             ! exponent in the beam-total transmittance model
    REAL(r64) :: TAUBB_EXPO             ! exponent in the beam-beam transmittance model
    REAL(r64) :: TAU_BT                 ! beam-total transmittance
          ! Flow

    THETA = MIN( 89.99d0 * DegToRadians, xTHETA)

    IF (TAU_BB0 > 0.9999d0) THEN
        TAU_BB = 1.0d0
        TAU_BT = 1.0d0
    ELSE
        ! beam total
        TAUM0 = MIN( 1.0d0, (TAU_BT0 - TAU_BB0) / (1.0d0-TAU_BB0))
        IF (TAUM0 <= 0.33d0) THEN
            TAUBT_EXPO = 0.133d0 * (TAUM0 + 0.003d0)**(-0.467d0)
        ELSE
            TAUBT_EXPO = 0.33d0 * (1.0d0 - TAUM0)
        ENDIF
        TAU_BT = TAU_BT0 * COS( THETA)**TAUBT_EXPO    ! always 0 - 1

        THETA_CUTOFF = DegToRadians*(90.d0 - 25.d0 * COS( TAU_BB0 * PiOvr2))
        IF (THETA >= THETA_CUTOFF) THEN
            TAU_BB = 0.0d0
        ELSE
            TAUBB_EXPO = 0.6d0 * COS( TAU_BB0 * PiOvr2)**0.3d0
            TAU_BB = TAU_BB0 * COS( PiOvr2*THETA/THETA_CUTOFF)**TAUBB_EXPO
            ! BB correlation can produce results slightly larger than BT
            ! Enforce consistency
            TAU_BB = MIN( TAU_BT, TAU_BB)
        END IF
    END IF

    RHO_BD = RHO_BT0
    TAU_BD = P01( TAU_BT-TAU_BB, "RB_BEAM TauBD")
    RETURN
END SUBROUTINE RB_BEAM

SUBROUTINE IS_DIFF(RHO_BT0, TAU_BT0, TAU_BB0, RHO_DD, TAU_DD)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates insect screen diffuse-diffuse solar optical properties by integrating
          ! the corresponding properties over the hemisphere
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT (IN) :: RHO_BT0    ! normal incidence beam-total reflectance
    REAL(r64),  INTENT (IN) :: TAU_BT0    ! normal incidence beam-total transmittance
                                          !   TAU_BT0 = TAU_BB0 + TAU_BD0
    REAL(r64),  INTENT (IN) :: TAU_BB0    ! normal incidence beam-beam transmittance
    REAL(r64), INTENT (OUT) :: RHO_DD     ! returned: diffuse-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_DD     ! returned: diffuse-diffuse transmittance
    REAL(r64)               :: P( hipDIM)
          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*),  PARAMETER :: RoutineName='IS_DIFF: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                    :: SumRefAndTran
          ! Flow

    P( hipRHO_BT0) = RHO_BT0
    P( hipTAU_BT0) = TAU_BT0
    P( hipTAU_BB0) = TAU_BB0

    RHO_DD = HEMINT( IS_F, hipRHO, P)
    TAU_DD = HEMINT( IS_F, hipTAU, P)

    IF (RHO_DD + TAU_DD > 1.0d0) THEN
        SumRefAndTran = RHO_DD + TAU_DD
        CALL ShowWarningMessage(RoutineName//'Calculated insect screen diffuse-diffuse properties are inconsistent')
        CALL ShowContinueError('...The diffuse-diffuse reflectance = '//TRIM(TrimSigDigits(RHO_DD,4)))
        CALL ShowContinueError('...The diffuse-diffuse tansmittance = '//TRIM(TrimSigDigits(TAU_DD,4)))
        CALL ShowContinueError('...Sum of diffuse reflectance and tansmittance = '//TRIM(TrimSigDigits(SumRefAndTran,4)))
        CALL ShowContinueError('...This sum cannot be > 1.0. Transmittance will be reset to 1 minus reflectance')
        TAU_DD = 1.d0 - RHO_DD
    END IF
    RETURN
END SUBROUTINE IS_DIFF

REAL(r64) FUNCTION IS_F(THETA, OPT, P)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Insect screen integrand
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT (IN) :: THETA            ! incidence angle, radians
    INTEGER,   INTENT( IN) :: OPT              ! options (1=reflectance, 2=transmittance)
    REAL(r64), INTENT( IN) :: P( hipDIM)       ! parameters
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)              :: RHO_BD
    REAL(r64)              :: TAU_BB
    REAL(r64)              :: TAU_BD
          ! Flow

    CALL IS_BEAM( THETA, P( hipRHO_BT0), P( hipTAU_BT0), P( hipTAU_BB0), &
            RHO_BD, TAU_BB, TAU_BD)

    IF (OPT == hipRHO) THEN
        IS_F = RHO_BD
    ELSE IF (OPT == hipTAU) THEN
        IS_F = TAU_BB+TAU_BD
    ELSE
        IS_F = -1.0d0
    ENDIF
    RETURN
END FUNCTION IS_F

SUBROUTINE IS_BEAM(xTHETA, RHO_BT0, TAU_BT0, TAU_BB0, RHO_BD, TAU_BB, TAU_BD)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates insect screen off-normal solar optical properties
          ! using semi-empirical relations.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT (IN) :: xTHETA     ! incidence angle, radians (0 - PI/2)
    REAL(r64),  INTENT (IN) :: RHO_BT0    ! beam-total reflectance
    REAL(r64),  INTENT (IN) :: TAU_BT0    ! beam-total transmittance at normal incidence
                                          !   TAU_BTO = TAU_BB0 + TAU_BD0
    REAL(r64),  INTENT (IN) :: TAU_BB0    ! beam-beam transmittance at normal incidence
    REAL(r64), INTENT (OUT) :: RHO_BD     ! returned: beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BB     ! returned: beam-beam transmittance
    REAL(r64), INTENT (OUT) :: TAU_BD     ! returned: beam-diffuse transmittance
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: THETA            ! working incident angle, radians
    REAL(r64) :: COSTHETA         ! cosine( theta)
    REAL(r64) :: THETA_CUTOFF     ! cutoff angle, radians (beyond which TAU_BB = 0)
    REAL(r64) :: B                ! working temp
    REAL(r64) :: RHO_W            ! apparent wire reflectance
    REAL(r64) :: RHO_BT90         ! beam-total reflectance at 90 deg incidence
    REAL(r64) :: TAU_BT           ! beam-total transmittance
          ! Flow

    THETA = MIN( 89.99d0*DegToRadians, xTHETA)
    COSTHETA = COS( THETA)

    RHO_W = RHO_BT0/MAX(0.00001d0, 1.d0 - TAU_BB0)
    B = -0.45d0 * LOG( MAX( RHO_W, 0.01d0))

    RHO_BT90 = RHO_BT0 + (1.d0 - RHO_BT0)*(0.35d0 * RHO_W)

    RHO_BD = P01( RHO_BT0 + (RHO_BT90 - RHO_BT0) * (1.d0 -COSTHETA**B), "IS_BEAM RhoBD")

    IF (TAU_BT0 < 0.00001d0) THEN
        TAU_BB = 0.0d0
        TAU_BT = 0.0d0
    ELSE
        THETA_CUTOFF = ACOS( IS_DSRATIO( TAU_BB0))

        IF (THETA >= THETA_CUTOFF) THEN
            TAU_BB = 0.0d0
        ELSE
            B = -0.45d0 * LOG( MAX( TAU_BB0, 0.01d0)) + 0.1d0
            TAU_BB = P01( TAU_BB0 * COS( PiOvr2*THETA/THETA_CUTOFF)**B, "IS_BEAM TauBB")
        END IF

        B = -0.65d0 * LOG( MAX( TAU_BT0, 0.01d0)) + 0.1d0
        TAU_BT = P01( TAU_BT0 * COSTHETA**B, "IS_BEAM TauBT")
    END IF

    TAU_BD = P01( TAU_BT-TAU_BB, "IS_BEAM TauBD")
    RETURN
END SUBROUTINE IS_BEAM

REAL(r64) FUNCTION IS_OPENNESS(D, S)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Returns openness from wire geometry.
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT (IN) :: D            ! wire diameter
    REAL(r64), INTENT (IN) :: S            ! wire spacing
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow

    IF (S > 0.d0) THEN
        IS_OPENNESS = (MAX( S-D, 0.0d0) / S)**2
    ELSE
        IS_OPENNESS = 0.0d0
    END IF
    RETURN
END FUNCTION IS_OPENNESS

REAL(r64) FUNCTION IS_DSRATIO(OPENNESS)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Returns ratio of diameter to spacing
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT (IN) :: OPENNESS         ! openness
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow

    IF (OPENNESS > 0.d0) THEN
        IS_DSRATIO = 1.0d0 - MIN( SQRT( OPENNESS), 1.0d0)
    ELSE
        IS_DSRATIO = 0.0d0
    END IF
    RETURN
END FUNCTION IS_DSRATIO

SUBROUTINE FM_DIFF(RHO_BT0, TAU_BT0, TAU_BB0, RHO_DD, TAU_DD)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates drape fabric diffuse-diffuse solar optical properties by integrating
          ! the corresponding beam properties over the hemisphere.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: RHO_BT0       ! fabric beam-total reflectance at normal incidence
    REAL(r64),  INTENT(IN):: TAU_BT0       ! fabric beam-total transmittance at normal incidence
                                           !   (TAU_BT0 = TAU_BB0 + TAU_BD0)
    REAL(r64),  INTENT(IN):: TAU_BB0       ! forward facing fabric beam-beam transmittance at normal incidence
    REAL(r64), INTENT(OUT):: RHO_DD        ! returned: fabric diffuse-diffuse reflectance
    REAL(r64), INTENT(OUT):: TAU_DD        ! returned: fabric diffuse-diffuse transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*),  PARAMETER :: RoutineName='FM_DIFF: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)             :: TAU_BD0
    REAL(r64)             :: P( hipDIM)
    REAL(r64)             :: SumRefAndTran
          ! flow

    TAU_BD0 = TAU_BT0 - TAU_BB0

    P( hipRHO_BT0) = RHO_BT0
    P( hipTAU_BT0) = TAU_BT0
    P( hipTAU_BB0) = TAU_BB0

    RHO_DD = HEMINT( FM_F, hipRHO, P)
    TAU_DD = HEMINT( FM_F, hipTAU, P)

    IF (RHO_DD + TAU_DD > 1.0d0) THEN
        SumRefAndTran = RHO_DD + TAU_DD
        CALL ShowWarningMessage(RoutineName//'Calculated drape fabric diffuse-diffuse properties are inconsistent')
        CALL ShowContinueError('...The diffuse-diffuse reflectance = '//TRIM(TrimSigDigits(RHO_DD,4)))
        CALL ShowContinueError('...The diffuse-diffuse tansmittance = '//TRIM(TrimSigDigits(TAU_DD,4)))
        CALL ShowContinueError('...Sum of diffuse reflectance and tansmittance = '//TRIM(TrimSigDigits(SumRefAndTran,4)))
        CALL ShowContinueError('...This sum cannot be > 1.0. Transmittance will be reset to 1 minus reflectance')
        TAU_DD = 1.0d0 - RHO_DD
    END IF
    RETURN
END SUBROUTINE FM_DIFF

REAL(r64) FUNCTION FM_F(THETA, Opt, P)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Drape fabric property integrand.
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: THETA           ! incidence angle, radians
    INTEGER,    INTENT(IN):: OPT             ! options (hipRHO, hipTAU)
    REAL(r64),  INTENT(IN):: P( hipDIM)      ! parameters
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)             :: RHO_BD
    REAL(r64)             :: TAU_BB
    REAL(r64)             :: TAU_BD
          ! Flow

    CALL FM_BEAM( THETA, P( hipRHO_BT0), P( hipTAU_BT0), P( hipTAU_BB0), &
            RHO_BD, TAU_BB, TAU_BD)

    IF (OPT == hipRHO) THEN
        FM_F = RHO_BD
    ELSE IF (OPT == hipTAU) THEN
        FM_F = TAU_BB+TAU_BD
    ELSE
        FM_F = -1.0d0
    ENDIF
    RETURN
END FUNCTION FM_F

SUBROUTINE FM_BEAM(xTHETA, RHO_BT0, TAU_BT0, TAU_BB0, RHO_BD, TAU_BB, TAU_BD)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the solar optical properties of a fabric for beam radiation incident
          ! on the forward facingsurface using optical properties at normal incidence and
          ! semi-empirical relations.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: xTHETA     ! incidence angle, radians (0 - PI/2)
    REAL(r64),  INTENT(IN):: RHO_BT0    ! fabric beam-total reflectance
    REAL(r64),  INTENT(IN):: TAU_BT0    ! fabric beam-total transmittance at normal incidence
                                        !   TAU_BTO = TAU_BB0 + TAU_BD0
    REAL(r64),  INTENT(IN):: TAU_BB0    ! fabric beam-beam transmittance at normal incidence
                                        !   = openness
    REAL(r64), INTENT(OUT):: RHO_BD     ! returned: fabric beam-diffuse reflectance
    REAL(r64), INTENT(OUT):: TAU_BB     ! returned: fabric beam-beam transmittance
    REAL(r64), INTENT(OUT):: TAU_BD     ! returned: fabric beam-diffuse transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)             :: THETA      ! working incident angle, radians
    REAL(r64)             :: COSTHETA   ! cosine( theta)
    REAL(r64)             :: R, B       ! working temps
    REAL(r64)             :: RHO_Y      ! apparent yarn reflectance
    REAL(r64)             :: RHO_BT90   ! beam-total reflectance at 90 deg incidence
    REAL(r64)             :: TAU_BT     ! beam-total transmittance
          ! Flow

    THETA = ABS( MAX( -89.99d0*DegToRadians, MIN( 89.99d0*DegToRadians, xTHETA)))
                    ! limit -89.99 - +89.99
                    ! by symmetry, optical properties same at +/- theta
    COSTHETA = COS( THETA)

    RHO_Y = RHO_BT0/MAX( 0.00001d0, 1.d0 - TAU_BB0)
    R = 0.7d0 * RHO_Y**0.7d0
    RHO_BT90 = RHO_BT0 + (1.d0 - RHO_BT0)*R
    B = 0.6d0
    RHO_BD = P01( RHO_BT0 + (RHO_BT90 - RHO_BT0) * (1.d0 - COSTHETA**B), "FM_BEAM RhoBD")

    IF (TAU_BT0 < 0.00001d0) THEN
        TAU_BB = 0.0d0
        TAU_BD = 0.0d0
    ELSE
        B = MAX( -0.5d0 * LOG( MAX( TAU_BB0, 0.01d0)), 0.35d0)
        TAU_BB = TAU_BB0 * COSTHETA**B

        B = MAX( -0.5d0 * LOG( MAX( TAU_BT0, .01d0)), 0.35d0)
        TAU_BT = TAU_BT0 * COSTHETA**B

        TAU_BD = P01( TAU_BT - TAU_BB, "FM_BEAM TauBD")
    END IF
    RETURN
END SUBROUTINE FM_BEAM

SUBROUTINE PD_LW(S, W, OPENNESS_FABRIC, EPSLWF0_FABRIC, EPSLWB0_FABRIC, TAULW0_FABRIC, &
                 EPSLWF_PD, TAULW_PD)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculates the effective longwave emittance and transmittance of a drapery layer
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: S                  ! pleat spacing (> 0)
    REAL(r64),  INTENT(IN):: W                  ! pleat depth (>=0, same units as S)
    REAL(r64),  INTENT(IN):: OPENNESS_FABRIC    ! fabric openness, 0-1 (=tausbb at normal incidence)
    REAL(r64),  INTENT(IN):: EPSLWF0_FABRIC     ! fabric LW front emittance at 0 openness
                                                !    typical (default) = 0.92
    REAL(r64),  INTENT(IN):: EPSLWB0_FABRIC     ! fabric LW back emittance at 0 openness
                                                !    typical (default) = 0.92
    REAL(r64),  INTENT(IN):: TAULW0_FABRIC      ! fabric LW transmittance at 0 openness
                                                !    nearly always 0
    REAL(r64), INTENT(OUT):: EPSLWF_PD          ! returned: drape front effective LW emittance
    REAL(r64), INTENT(OUT):: TAULW_PD           ! returned: drape effective LW transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)             :: RHOLWF_FABRIC
    REAL(r64)             :: RHOLWB_FABRIC
    REAL(r64)             :: TAULW_FABRIC
    REAL(r64)             :: EPSLWF_FABRIC
    REAL(r64)             :: EPSLWB_FABRIC
    REAL(r64)             :: TAULX
    REAL(r64)             :: RHOLWF_PD
          ! Flow

    CALL OPENNESS_LW(OPENNESS_FABRIC, EPSLWF0_FABRIC, TAULW0_FABRIC, EPSLWF_FABRIC, TAULW_FABRIC)
    CALL OPENNESS_LW(OPENNESS_FABRIC, EPSLWB0_FABRIC, TAULW0_FABRIC, EPSLWB_FABRIC, TAULX)

    RHOLWF_FABRIC = P01( 1.0d0 - EPSLWF_FABRIC - TAULW_FABRIC, "PD_LW RhoLWF")
    RHOLWB_FABRIC = P01( 1.0d0 - EPSLWB_FABRIC - TAULW_FABRIC, "PD_LW RhoLWB")

    CALL PD_DIFF(S, W, RHOLWF_FABRIC, RHOLWB_FABRIC, TAULW_FABRIC, RHOLWF_PD, TAULW_PD)

    EPSLWF_PD = P01(1.0d0 - TAULW_PD - RHOLWF_PD, "PD_LW EpsLWF")
    RETURN
END SUBROUTINE PD_LW

SUBROUTINE PD_DIFF(S, W, RHOFF_DD, RHOBF_DD, TAUF_DD, RHOFDD, TAUFDD)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculates the effective diffuse transmittance and reflectance of a drapery layer.
          !  Used for both LW and solar diffuse.
          !
          ! METHODOLOGY EMPLOYED:
          ! Eight surface flat-fabric model with rectangular enclosure. If you want the back-side
          ! reflectance call this routine a second time with reversed front and back properties
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: S                ! pleat spacing (> 0)
    REAL(r64),  INTENT(IN):: W                ! pleat depth (>=0, same units as S)
    REAL(r64),  INTENT(IN):: RHOFF_DD         ! fabric front diffuse-diffuse reflectance
    REAL(r64),  INTENT(IN):: RHOBF_DD         ! fabric back diffuse-diffuse reflectance
    REAL(r64),  INTENT(IN):: TAUF_DD          ! fabric diffuse-diffuse transmittance
    REAL(r64), INTENT(OUT):: RHOFDD           ! returned: drape diffuse-diffuse reflectance
    REAL(r64), INTENT(OUT):: TAUFDD           ! returned: drape diffuse-diffuse transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,    PARAMETER :: N = 6

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AK, CG                        ! length of diagonal strings of the rectangular enclosure
    REAL(r64) :: F12, F14, F32, F21, F31, F34, F24, F41, F42        ! shape factors
    REAL(r64) :: F57, F56, F58, F67, F65, F68, F75, F76, F78, F85, F87, F86
    REAL(r64) :: J1, J2, J4, J7, J6, J8        ! radiosity, surface i
    REAL(r64) :: G1, G3, G5, G7                ! irradiance, surface i
    REAL(r64) :: A( N, N+2)
    REAL(r64) :: XSOL( N)
          ! Flow

    IF (W/S < SMALL_ERROR) THEN
        ! flat drape (no pleats)
        RHOFDD = RHOFF_DD
        TAUFDD = TAUF_DD
        RETURN
    END IF

   ! SOLVE FOR DIAGONAL STRINGS AND SHAPE FACTORS

    AK = SQRT (S*S + W*W)
    CG = AK
    F12 = (S+W-AK)/(2.d0*S)
    F14 = (S+W-CG)/(2.d0*S)
    F32 = F14
    F31 = (AK+CG-2.d0*W)/(2.d0*S)
    F34 = F12
    F21 = (S+W-AK)/(2.d0*W)
    F24 = (AK+CG-2.d0*S)/(2.d0*W)
    F41 = (S+W-CG)/(2.d0*W)
    F42 = F24
    F57 = F31
    F56 = F12
    F58 = F14
    F75 = F31
    F76 = F32
    F78 = F34
    F67 = F41
    F65 = F21
    F68 = F24
    F85 = F41
    F87 = F21
    F86 = F42

    A = 0.0d0        ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS
    XSOL = 0.0d0     ! INITIALIZE SOLUTION VECTOR COEFFICIENTS

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1) = 1.0d0
    A(1,2) = -RHOBF_DD*F12
    A(1,3) = -RHOBF_DD*F14
    A(1,4) = 0.0d0
    A(1,5) = 0.0d0
    A(1,6) = 0.0d0
    A(1,7) = TAUF_DD
    A(2,1) = -RHOBF_DD*F21
    A(2,2) = 1.0d0
    A(2,3) = -RHOBF_DD*F24
    A(2,4) = -TAUF_DD*F87
    A(2,5) = -TAUF_DD*F86
    A(2,6) = 0.0d0
    A(2,7) = TAUF_DD*F85
    A(3,1) = -RHOBF_DD*F41
    A(3,2) = -RHOBF_DD*F42
    A(3,3) = 1.0d0
    A(3,4) = -TAUF_DD*F67
    A(3,5) = 0.0d0
    A(3,6) = -TAUF_DD*F68
    A(3,7) = TAUF_DD*F65
    A(4,1) = 0.0d0
    A(4,2) = 0.0d0
    A(4,3) = 0.0d0
    A(4,4) = 1.0d0
    A(4,5) = -RHOFF_DD*F76
    A(4,6) = -RHOFF_DD*F78
    A(4,7) = RHOFF_DD*F75
    A(5,1) = -TAUF_DD*F41
    A(5,2) = -TAUF_DD*F42
    A(5,3) = 0.0d0
    A(5,4) = -RHOFF_DD*F67
    A(5,5) = 1.0d0
    A(5,6) = -RHOFF_DD*F68
    A(5,7) = RHOFF_DD*F65
    A(6,1) = -TAUF_DD*F21
    A(6,2) = 0.0d0
    A(6,3) = -TAUF_DD*F24
    A(6,4) = -RHOFF_DD*F87
    A(6,5) = -RHOFF_DD*F86
    A(6,6) = 1.0d0
    A(6,7) = RHOFF_DD*F85

    CALL SOLMATS( N, A, XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J4 = XSOL(3)
    J7 = XSOL(4)
    J6 = XSOL(5)
    J8 = XSOL(6)

    G1 = F12*J2+F14*J4
    G3 = F32*J2+F31*J1+F34*J4
    G5 = F57*J7+F56*J6+F58*J8
    G7 = F75+F76*J6+F78*J8

    TAUFDD = P01( (G3+TAUF_DD*G7)/2.0d0, "PD_DIFF TauDD")
    RHOFDD = P01( (RHOFF_DD+TAUF_DD*G1+G5)/2.0d0, "PD_DIFF RhoDD")
    RETURN
END SUBROUTINE PD_DIFF

SUBROUTINE PD_BEAM(S, W, OHM_V_RAD, OHM_H_RAD, &
                   RHOFF_BT0, TAUFF_BB0, TAUFF_BD0, RHOFF_DD, TAUFF_DD, &
                   RHOBF_BT0, TAUBF_BB0, TAUBF_BD0, RHOBF_DD, TAUBF_DD, &
                   RHO_BD, TAU_BB, TAU_BD)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! Pleated drape flat-fabric model with rectangular enclosure
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S            ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W            ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OHM_V_RAD    ! vertical profile angle, radians +=above horiz
    REAL(r64), INTENT (IN) :: OHM_H_RAD    ! horizontal profile angle, radians=clockwise when viewed from above
    REAL(r64), INTENT (IN) :: RHOFF_BT0    ! beam total reflectance front (outside)
    REAL(r64), INTENT (IN) :: TAUFF_BB0    ! beam beam transmittance front (outside)
    REAL(r64), INTENT (IN) :: TAUFF_BD0    ! beam diffuse transmittance front (outside)
    REAL(r64), INTENT (IN) :: RHOFF_DD     ! diffuse-diffuse reflectance front (outside)
    REAL(r64), INTENT (IN) :: TAUFF_DD     ! diffuse-diffuse transmittance front (outside)
    REAL(r64), INTENT (IN) :: RHOBF_BT0    ! beam total reflectance back (inside)
    REAL(r64), INTENT (IN) :: TAUBF_BB0    ! beam beam total transmittance back (inside)
    REAL(r64), INTENT (IN) :: TAUBF_BD0    ! beam diffuse transmittance back (inside)
    REAL(r64), INTENT (IN) :: RHOBF_DD     ! diffuse-diffuse reflectance front (outside)
    REAL(r64), INTENT (IN) :: TAUBF_DD     ! diffuse-diffuse transmittance front (outside)
    REAL(r64), INTENT (OUT) :: RHO_BD      ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BB      ! returned: drape beam-beam transmittance
    REAL(r64), INTENT (OUT) :: TAU_BD      ! returned: drape beam-diffuse transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: DE ! length of directly illuminated surface on side of pleat that
                    !   is open on front (same units as S and W)
    REAL(r64) :: EF ! length of pleat side shaded surface (W-DE) (same units as S and W)
    REAL(r64) :: OMEGA_V, OMEGA_H          ! profile angles limited to +/- PI/2
    REAL(r64) :: TAUFF_BT0, TAUBF_BT0
    REAL(r64) :: THETA_PARL, THETA_PERP    ! beam incidence angles on pleat surface parallel / perpendicular
                                           ! to window plane
    REAL(r64) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP
          ! Flow

    OMEGA_V = ABS( MAX( -89.5d0*DegToRadians, MIN( 89.5d0*DegToRadians, OHM_V_RAD)))
    OMEGA_H = ABS( MAX( -89.5d0*DegToRadians, MIN( 89.5d0*DegToRadians, OHM_H_RAD)))
                        ! limit profile angles -89.5 - +89.5
                        ! by symmetry, properties same for +/- profile angle

    ! incidence angles on pleat front/back (_PARL) and sides (_PERP)
    THETA_PARL = ACOS(ABS(COS(ATAN(TAN(OMEGA_V)*COS(OMEGA_H)))*COS(OMEGA_H)))
    THETA_PERP = ACOS(ABS(COS(ATAN(TAN(OMEGA_V)*SIN(OMEGA_H)))*SIN(OMEGA_H)))

    ! off-normal fabric properties, front surface
    TAUFF_BT0 = TAUFF_BB0 + TAUFF_BD0
    CALL FM_BEAM( THETA_PARL, RHOFF_BT0, TAUFF_BT0, TAUFF_BB0, RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL)
    IF (W/S < SMALL_ERROR) THEN
        ! flat drape (no pleats) -- return fabric properties
        RHO_BD = RHOFF_BT_PARL
        TAU_BD = TAUFF_BD_PARL
        TAU_BB = TAUFF_BB_PARL
        RETURN
    END IF

    CALL FM_BEAM( THETA_PERP, RHOFF_BT0, TAUFF_BT0, TAUFF_BB0, RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP)

    ! Off-normal fabric properties, back surface
    TAUBF_BT0 = TAUBF_BB0 + TAUBF_BD0
    CALL FM_BEAM( THETA_PARL, RHOBF_BT0, TAUBF_BT0, TAUBF_BB0, RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL)
    CALL FM_BEAM( THETA_PERP, RHOBF_BT0, TAUBF_BT0, TAUBF_BB0, RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP)

    DE = S * ABS(COS(OMEGA_H) / MAX( .000001d0, SIN(OMEGA_H)) )
    EF = W-DE

    ! select geometric case
    IF ( DE < W - SMALL_ERROR) THEN
        ! illuminated length less than pleat depth
        IF (DE < EF - SMALL_ERROR) THEN
            ! illum < shade
            CALL PD_BEAM_CASE_I( S, W, OMEGA_H, DE, &
                RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                RHO_BD, TAU_BD, TAU_BB)
        ELSE IF (DE <= EF + SMALL_ERROR) THEN
            ! illum and shade equal
            CALL PD_BEAM_CASE_II( S, W, OMEGA_H, DE, &
                RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                RHO_BD, TAU_BD, TAU_BB)
        ELSE
            ! illum > shade
            CALL PD_BEAM_CASE_III( S, W, OMEGA_H, DE, &
                RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                RHO_BD, TAU_BD, TAU_BB)
        END IF
    ELSE IF (DE <= W + SMALL_ERROR) THEN
        ! illum length same as pleat depth
        CALL PD_BEAM_CASE_IV( S, W, OMEGA_H, DE, &
            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
            RHO_BD, TAU_BD, TAU_BB)
    ELSE IF (DE < 9000.*S) THEN
        ! some direct illum on pleat back
        CALL PD_BEAM_CASE_V( S, W, OMEGA_H, DE, &
            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
            RHO_BD, TAU_BD, TAU_BB)
    ELSE
        ! beam parallel to pleat sides (no direct illum on pleat back)
        CALL PD_BEAM_CASE_VI( S, W, OMEGA_H, DE, &
            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
            RHO_BD, TAU_BD, TAU_BB)
    ENDIF
    RETURN
END SUBROUTINE PD_BEAM

SUBROUTINE PD_BEAM_CASE_I(S, W,  OMEGA_H, DE, &
                          RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                          RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                          RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                          RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                          RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                          RHO_BD, TAU_BD, TAU_BB)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! FOURTEEN SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S              ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W              ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OMEGA_H        ! horizontal profile angle, radians
    REAL(r64), INTENT (IN) :: DE             ! width of illumination on pleat bottom (same units as S)
    ! fabric properties at current (off-normal) incidence
    !   _PARL = surface parallel to window (pleat top/bot)
    !   _PERP = surface perpendicular to window (pleat side)
    REAL(r64), INTENT (IN) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

    REAL(r64), INTENT (IN) :: RHOFF_DD       ! fabric front diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: RHOBF_DD       ! fabric back diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: TAUFF_DD       ! fabric front diffuse-diffuse transmittance
    REAL(r64), INTENT (IN) :: TAUBF_DD       ! fabric back diffuse-diffuse transmittance
    REAL(r64), INTENT (OUT) :: RHO_BD        ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BD        ! returned: drape front beam-diffuse transmittance
    REAL(r64), INTENT (OUT) :: TAU_BB        ! returned: drape front beam-beam transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,      PARAMETER :: N = 12

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAUBF_BT_PERP
    REAL(r64) :: AB, GN, NP, GP, NK, PK, BC, AN, AP, AK, BG, BP, CG, BK, CP, CN  ! lengths of surfaces and diagonal strings
    REAL(r64) :: Z1_BB, Z7_BB                                         ! beam source terms
    REAL(r64) :: Z1_BD, Z2_BD, Z7_BD, Z3_BD, Z9_BD, Z13_BD, Z14_BD    ! diffuse source terms due to incident beam radiation
    ! shape factors
    REAL(r64) :: F12, F13, F14, F16, F17, F21, F25, F26, F27, F31, F35, F36, &
                 F37, F41, F45, F46, F47, F51, F52, F53, F54, F56, F57, F61
    REAL(r64) :: F62, F63, F64, F71, F72, F73, F74, F89, F810, F811, F812,F813, &
                 F814, F911, F912, F913, F914, F1011, F1012, F1013, F1014
    REAL(r64) :: F119, F1110, F1112, F1113, F1114, F129, F1210, F1211, F139, &
                 F1310, F1311, F149, F1410, F1411
    REAL(r64) :: J1, J2, J3, J4, J6, J7, J9, J10, J11, J12, J13, J14     ! radiosity, surface i
    REAL(r64) :: G1, G5, G8, G11                                         ! irradiance, surface i
    REAL(r64) :: A( N, N+2)    ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
          ! Flow

    TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

    AB = DE
    GN = DE
    NP = DE
    GP = 2.0d0*DE
    NK = W-DE
    PK = W-2.0d0*DE
    BC = NK
    AN = SQRT(S*S+DE*DE)
    AP = SQRT(S*S+GP*GP)
    AK = SQRT(W*W+S*S)
    BG = AN
    BP = AN
    CG = AK
    BK = SQRT(S*S+BC*BC)
    CP = SQRT(S*S+PK*PK)
    CN = SQRT(S*S+NK*NK)

    Z1_BB = TAUFF_BB_PARL
    Z1_BD = TAUFF_BD_PARL
    Z2_BD = Z1_BB*RHOBF_BT_PERP*S/GN
    Z7_BB = TAUFF_BB_PERP*S/DE
    Z7_BD = TAUFF_BD_PERP*S/DE
    Z3_BD = Z7_BB*RHOBF_BT_PERP
    Z9_BD = RHOFF_BT_PERP*S/DE
    Z13_BD = Z7_BB*TAUBF_BT_PERP
    Z14_BD = Z1_BB*TAUBF_BT_PERP*S/GN

    F12 = (S+GN-AN)/(2.d0*S)
    F13 = (AN+GP-(GN+AP))/(2.d0*S)
    F14 = (AP+W-(GP+AK))/(2.d0*S)
    F16 = (W+BG-(AB+CG))/(2.d0*S)
    F17 = (S+AB-BG)/(2.d0*S)
    F21 = (S+GN-AN)/(2.d0*GN)
    F25 = (W+CN-(CG+NK))/(2.d0*GN)
    F26 = (CG+S-(BG+CN))/(2.d0*GN)
    F27 = (AN+BG-2.d0*S)/(2.d0*GN)
    F31 = (AN+GP-(GN+AP))/(2.d0*NP)
    F35 = (NK+CP-(CN+PK))/(2.d0*NP)
    F36 = (CN+BP-(S+CP))/(2.d0*NP)
    F37 = (S+AP-(AN+BP))/(2.d0*NP)
    F41 = (W+AP-(GP+AK))/(2.d0*PK)
    F45 = (S+PK-CP)/(2.d0*PK)
    F46 = (CP+BK-(S+BP))/(2.d0*PK)
    F47 = (BP+AK-(AP+BK))/(2.d0*PK)
    F51 = (AK+CG-2.d0*W)/(2.d0*S)
    F52 = (W+CN-(CG+NK))/(2.d0*S)
    F53 = (NK+CP-(CN+PK))/(2.d0*S)
    F54 = (S+PK-CP)/(2.d0*S)
    F56 = (S+BC-BK)/(2.d0*S)
    F57 = (W+BK-(BC+AK))/(2.d0*S)
    F61 = (W+BG-(AB+CG))/(2.d0*BC)
    F62 = (S+CG-(BG+CN))/(2.d0*BC)
    F63 = (CN+BP-(S+CP))/(2.d0*BC)
    F64 = (BK+CP-(S+BP))/(2.d0*BC)
    F71 = F21
    F72 = F27
    F73 = F37
    F74 = (BP+AK-(BK+AP))/(2.d0*AB)
    F89 = F12
    F810 = F16
    F811 = F51
    F812 = F14
    F813 = F13
    F814 = F12
    F911 = F25
    F912 = F74
    F913 = F73
    F914 = F27
    F1011 = (BC+S-BK)/(2.d0*BC)
    F1012 = F64
    F1013 = F63
    F1014 = F62
    F119 = F57
    F1110 = F56
    F1112 = F54
    F1113 = F53
    F1114 = F52
    F129 = F47
    F1210 = F46
    F1211 = F45
    F139 = F37
    F1310 = F36
    F1311 = F35
    F149 = F27
    F1410 = F26
    F1411 = F25

    A = 0.0d0           ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS
    XSOL = 0.0d0        ! INITIALIZE SOLUTION VECTOR COEFFICIENTS

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1)    = 1.0d0
    A(1,2)    = -RHOBF_DD*F12
    A(1,3)    = -RHOBF_DD*F13
    A(1,4)    = -RHOBF_DD*F14
    A(1,5)    = -RHOBF_DD*F16
    A(1,6)    = -RHOBF_DD*F17
    A(1,7)    = 0.0d0
    A(1,8)    = 0.0d0
    A(1,9)    = 0.0d0
    A(1,10)    = 0.0d0
    A(1,11)    = 0.0d0
    A(1,12)    = 0.0d0
    A(1,13)    = Z1_BD
    A(2,1)    = -RHOBF_DD*F21
    A(2,2)    = 1.0d0
    A(2,3)    = 0.0d0
    A(2,4)    = 0.0d0
    A(2,5)    = -RHOBF_DD*F26
    A(2,6)    = -RHOBF_DD*F27
    A(2,7)    = -TAUFF_DD*F149
    A(2,8)    = -TAUFF_DD*F1410
    A(2,9)    = -TAUFF_DD*F1411
    A(2,10)    = 0.0d0
    A(2,11)    = 0.0d0
    A(2,12)    = 0.0d0
    A(2,13)    = Z2_BD
    A(3,1)    = -RHOBF_DD*F31
    A(3,2)    = 0.0d0
    A(3,3)    = 1.0d0
    A(3,4)    = 0.0d0
    A(3,5)    = -RHOBF_DD*F36
    A(3,6)    = -RHOBF_DD*F37
    A(3,7)    = -TAUFF_DD*F139
    A(3,8)    = -TAUFF_DD*F1310
    A(3,9)    = -TAUFF_DD*F1311
    A(3,10)    = 0.0d0
    A(3,11)    = 0.0d0
    A(3,12)    = 0.0d0
    A(3,13)    = Z3_BD
    A(4,1)    = -RHOBF_DD*F41
    A(4,2)    = 0.0d0
    A(4,3)    = 0.0d0
    A(4,4)    = 1.0d0
    A(4,5)    = -RHOBF_DD*F46
    A(4,6)    = -RHOBF_DD*F47
    A(4,7)    = -TAUFF_DD*F129
    A(4,8)    = -TAUFF_DD*F1210
    A(4,9)    = -TAUFF_DD*F1211
    A(4,10)    = 0.0d0
    A(4,11)    = 0.0d0
    A(4,12)    = 0.0d0
    A(4,13)    = 0.0d0
    A(5,1)    = -RHOBF_DD*F61
    A(5,2)    = -RHOBF_DD*F62
    A(5,3)    = -RHOBF_DD*F63
    A(5,4)    = -RHOBF_DD*F64
    A(5,5)    = 1.0d0
    A(5,6)    = 0.0d0
    A(5,7)    = 0.0d0
    A(5,8)    = 0.0d0
    A(5,9)    = -TAUFF_DD*F1011
    A(5,10)    = -TAUFF_DD*F1012
    A(5,11)    = -TAUFF_DD*F1013
    A(5,12)    = -TAUFF_DD*F1014
    A(5,13)    = 0.0d0
    A(6,1)    = -RHOBF_DD*F71
    A(6,2)    = -RHOBF_DD*F72
    A(6,3)    = -RHOBF_DD*F73
    A(6,4)    = -RHOBF_DD*F74
    A(6,5)    = 0.0d0
    A(6,6)    = 1.0d0
    A(6,7)    = 0.0d0
    A(6,8)    = 0.0d0
    A(6,9)    = -TAUFF_DD*F911
    A(6,10)    = -TAUFF_DD*F912
    A(6,11)    = -TAUFF_DD*F913
    A(6,12)    = -TAUFF_DD*F914
    A(6,13)    = Z7_BD
    A(7,1)    = -TAUBF_DD*F71
    A(7,2)    = -TAUBF_DD*F72
    A(7,3)    = -TAUBF_DD*F73
    A(7,4)    = -TAUBF_DD*F74
    A(7,5)    = 0.0d0
    A(7,6)    = 0.0d0
    A(7,7)    = 1.0d0
    A(7,8)    = 0.0d0
    A(7,9)    = -RHOFF_DD*F911
    A(7,10)    = -RHOFF_DD*F912
    A(7,11)    = -RHOFF_DD*F913
    A(7,12)    = -RHOFF_DD*F914
    A(7,13)    = Z9_BD
    A(8,1)    = -TAUBF_DD*F61
    A(8,2)    = -TAUBF_DD*F62
    A(8,3)    = -TAUBF_DD*F63
    A(8,4)    = -TAUBF_DD*F64
    A(8,5)    = 0.0d0
    A(8,6)    = 0.0d0
    A(8,7)    = 0.0d0
    A(8,8)    = 1.0d0
    A(8,9)    = -RHOFF_DD*F1011
    A(8,10)    = -RHOFF_DD*F1012
    A(8,11)    = -RHOFF_DD*F1013
    A(8,12)    = -RHOFF_DD*F1014
    A(8,13)    = 0.0d0
    A(9,1)    = 0.0d0
    A(9,2)    = 0.0d0
    A(9,3)    = 0.0d0
    A(9,4)    = 0.0d0
    A(9,5)    = 0.0d0
    A(9,6)    = 0.0d0
    A(9,7)    = -RHOFF_DD*F119
    A(9,8)    = -RHOFF_DD*F1110
    A(9,9)    = 1.0d0
    A(9,10)    = -RHOFF_DD*F1112
    A(9,11)    = -RHOFF_DD*F1113
    A(9,12)    = -RHOFF_DD*F1114
    A(9,13)    = 0.0d0
    A(10,1)    = -TAUBF_DD*F41
    A(10,2)    = 0.0d0
    A(10,3)    = 0.0d0
    A(10,4)    = 0.0d0
    A(10,5)    = -TAUBF_DD*F46
    A(10,6)    = -TAUBF_DD*F47
    A(10,7)    = -RHOFF_DD*F129
    A(10,8)    = -RHOFF_DD*F1210
    A(10,9)    = -RHOFF_DD*F1211
    A(10,10) = 1.0d0
    A(10,11)    = 0.0d0
    A(10,12)    = 0.0d0
    A(10,13)    = 0.0d0
    A(11,1)    = -TAUBF_DD*F31
    A(11,2)    = 0.0d0
    A(11,3)    = 0.0d0
    A(11,4)    = 0.0d0
    A(11,5)    = -TAUBF_DD*F36
    A(11,6)    = -TAUBF_DD*F37
    A(11,7)    = -RHOFF_DD*F139
    A(11,8)    = -RHOFF_DD*F1310
    A(11,9)    = -RHOFF_DD*F1311
    A(11,10)    = 0.0d0
    A(11,11)    = 1.0d0
    A(11,12)    = 0.0d0
    A(11,13)    = Z13_BD
    A(12,1)    = -TAUBF_DD*F21
    A(12,2)    = 0.0d0
    A(12,3)    = 0.0d0
    A(12,4)    = 0.0d0
    A(12,5)    = -TAUBF_DD*F26
    A(12,6)    = -TAUBF_DD*F27
    A(12,7)    = -RHOFF_DD*F149
    A(12,8)    = -RHOFF_DD*F1410
    A(12,9)    = -RHOFF_DD*F1411
    A(12,10)    = 0.0d0
    A(12,11)    = 0.0d0
    A(12,12)    = 1.0d0
    A(12,13)    = Z14_BD


    CALL SOLMATS( N, A,XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J3 = XSOL(3)
    J4 = XSOL(4)
    J6 = XSOL(5)
    J7 = XSOL(6)
    J9 = XSOL(7)
    J10 = XSOL(8)
    J11 = XSOL(9)
    J12 = XSOL(10)
    J13 = XSOL(11)
    J14 = XSOL(12)

    G1 = F12*J2+F13*J3+F14*J4+F16*J6+F17*J7
    G5 = F56*J6+F57*J7+F51*J1+F52*J2+F53*J3+F54*J4
    G8 = F89*J9+F810*J10+F811*J11+F812*J12+F813*J13+F814*J14
    G11 = F1112*J12+F1113*J13+F1114*J14+F119*J9+F1110*J10

    TAU_BB = 0.0d0
    TAU_BD = (G5+TAUFF_DD*G11)/2.d0
    RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G8)/2.d0
    RETURN
END SUBROUTINE PD_BEAM_CASE_I

SUBROUTINE PD_BEAM_CASE_II(S, W, OMEGA_H, DE, &
                           RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                           RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                           RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                           RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                           RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                           RHO_BD, TAU_BD, TAU_BB)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! TWELVE SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S              ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W              ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OMEGA_H        ! horizontal profile angle, radians
    REAL(r64), INTENT (IN) :: DE             ! width of illumination on pleat bottom (same units as S)

    ! fabric properties at current (off-normal) incidence
    !   _PARL = surface parallel to window (pleat top/bot)
    !   _PERP = surface perpendicular to window (pleat side)
    REAL(r64), INTENT (IN) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

    REAL(r64), INTENT (IN) :: RHOFF_DD      ! fabric front diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: RHOBF_DD      ! fabric back diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: TAUFF_DD      ! fabric front diffuse-diffuse transmittance
    REAL(r64), INTENT (IN) :: TAUBF_DD      ! fabric back diffuse-diffuse transmittance
    REAL(r64), INTENT (OUT) :: RHO_BD       ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BD       ! returned: drape front beam-diffuse transmittance
    REAL(r64), INTENT (OUT) :: TAU_BB       ! returned: drape front beam-beam transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,      PARAMETER :: N = 10

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAUBF_BT_PERP
    REAL(r64) :: AB, GN, NK, BC, AN, AK, BG, CG, BK, CN             ! lengths of surfaces and diagonal strings
    REAL(r64) :: Z1_BD, Z2_BD, Z3_BD, Z6_BD, Z8_BD, Z11_BD, Z12_BD  ! diffuse source terms due to incident beam radiation
    REAL(r64) :: Z1_BB, Z6_BB    ! beam source terms due to incident beam radiation
    ! shape factors
    REAL(r64) :: F12, F13, F15, F16, F21, F25, F26, F31, F35, F36, F41
    REAL(r64) :: F42, F43, F45, F46, F51, F52, F53, F54, F61, F62, F63
    REAL(r64) :: F78, F79, F710, F711, F712, F810, F811, F812, F910, F911
    REAL(r64) :: F912, F108, F109, F1011, F1012, F118, F119, F1110, F128, F129, F1210

    REAL(r64) :: J1, J2, J3, J5, J6, J8, J9, J10, J11, J12      ! radiosity, surface i
    REAL(r64) :: G1, G4, G7, G10                                ! irradiance, surface i
    REAL(r64) :: A( N, N+2)    ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
          ! Flow

    TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

    AB = DE
    GN = DE
    NK = W-DE
    BC = NK
    AN = SQRT(S*S+DE*DE)
    AK = SQRT(W*W+S*S)
    BG = AN
    CG = AK
    BK = SQRT(S*S+BC*BC)
    CN = SQRT(S*S+NK*NK)

    Z1_BB = TAUFF_BB_PARL
    Z1_BD = TAUFF_BD_PARL
    Z2_BD = Z1_BB*RHOBF_BT_PERP*S/GN
    Z6_BB = TAUFF_BB_PERP*S/DE
    Z6_BD = TAUFF_BD_PERP*S/DE
    Z3_BD = Z6_BB*RHOBF_BT_PERP
    Z8_BD = RHOFF_BT_PERP*S/DE
    Z11_BD = Z6_BB*TAUBF_BT_PERP
    Z12_BD = Z1_BB*TAUBF_BT_PERP*S/GN

    F12 = (S+GN-AN)/(2.d0*S)
    F13 = (W+AN-(GN+AK))/(2.d0*S)
    F15 = (W+BG-(AB+CG))/(2.d0*S)
    F16 = (S+AB-BG)/(2.d0*S)
    F21 = (S+GN-AN)/(2.d0*GN)
    F25 = (S+CG-(BG+CN))/(2.d0*GN)
    F26 = (AN+BG-2.d0*S)/(2.d0*GN)
    F31 = (W+AN-(GN+AK))/(2.d0*NK)
    F35 = (BK+CN-2.d0*S)/(2.d0*NK)
    F36 = (S+AK-(AN+BK))/(2.d0*NK)
    F41 = (AK+CG-2.d0*W)/(2.d0*S)
    F42 = (W+CN-(CG+NK))/(2.d0*S)
    F43 = (S+NK-CN)/(2.d0*S)
    F45 = (S+BC-BK)/(2.d0*S)
    F46 = (W+BK-(AK+BC))/(2.d0*S)
    F51 = (W+BG-(AB+CG))/(2.d0*BC)
    F52 = (S+CG-(BG+CN))/(2.d0*BC)
    F53 = (BK+CN-2.d0*S)/(2.d0*BC)
    F54 = (S+BC-BK)/(2.d0*BC)
    F61 = (S+AB-BG)/(2.d0*AB)
    F62 = (AN+BG-2.d0*S)/(2.d0*AB)
    F63 = (S+AK-(AN+BK))/(2.d0*AB)
    F78 = F12
    F79 = F13
    F710 = (AK+CG-2.d0*W)/(2.d0*S)
    F711 = F15
    F712 = F16
    F810 = (W+CN-(CG+NK))/(2.d0*S)
    F811 = F25
    F812 = F26
    F910 = (S+NK-CN)/(2.d0*NK)
    F911 = F35
    F912 = F36
    F108 = F42
    F109 = F43
    F1011 = F45
    F1012 = F46
    F118 = F52
    F119 = F53
    F1110 = (S+BC-BK)/(2.d0*NK)
    F128 = F62
    F129 = F63
    F1210 = (W+BK-(AK+BC))/(2.d0*GN)


    A = 0.0d0           ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS
    XSOL = 0.0d0        ! INITIALIZE SOLUTION VECTOR COEFFICIENTS

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1)    = 1.0d0
    A(1,2)    = -RHOBF_DD*F12
    A(1,3)    = -RHOBF_DD*F13
    A(1,4)    = -RHOBF_DD*F15
    A(1,5)    = -RHOBF_DD*F16
    A(1,6)    = 0.0d0
    A(1,7)    = 0.0d0
    A(1,8)    = 0.0d0
    A(1,9)    = 0.0d0
    A(1,10)    = 0.0d0
    A(1,11)    = Z1_BD
    A(2,1)    = -RHOBF_DD*F21
    A(2,2)    = 1.0d0
    A(2,3)    = 0.0d0
    A(2,4)    = -RHOBF_DD*F25
    A(2,5)    = -RHOBF_DD*F26
    A(2,6)    = -TAUFF_DD*F128
    A(2,7)    = -TAUFF_DD*F129
    A(2,8)    = -TAUFF_DD*F1210
    A(2,9)    = 0.0d0
    A(2,10)    = 0.0d0
    A(2,11)    = Z2_BD
    A(3,1)    = -RHOBF_DD*F31
    A(3,2)    = 0.0d0
    A(3,3)    = 1.0d0
    A(3,4)    = -RHOBF_DD*F35
    A(3,5)    = -RHOBF_DD*F36
    A(3,6)    = -TAUFF_DD*F118
    A(3,7)    = -TAUFF_DD*F119
    A(3,8)    = -TAUFF_DD*F1110
    A(3,9)    = 0.0d0
    A(3,10)    = 0.0d0
    A(3,11)    = Z3_BD
    A(4,1)    = -RHOBF_DD*F51
    A(4,2)    = -RHOBF_DD*F52
    A(4,3)    = -RHOBF_DD*F53
    A(4,4)    = 1.0d0
    A(4,5)    = 0.0d0
    A(4,6)    = 0.0d0
    A(4,7)    = 0.0d0
    A(4,8)    = -TAUFF_DD*F910
    A(4,9)    = -TAUFF_DD*F911
    A(4,10)    = -TAUFF_DD*F912
    A(4,11)    = 0.0d0
    A(5,1)    = -RHOBF_DD*F61
    A(5,2)    = -RHOBF_DD*F62
    A(5,3)    = -RHOBF_DD*F63
    A(5,4)    = 0.0d0
    A(5,5)    = 1.0d0
    A(5,6)    = 0.0d0
    A(5,7)    = 0.0d0
    A(5,8)    = -TAUFF_DD*F810
    A(5,9)    = -TAUFF_DD*F811
    A(5,10)    = -TAUFF_DD*F812
    A(5,11)    = Z6_BD
    A(6,1)    = -TAUBF_DD*F61
    A(6,2)    = -TAUBF_DD*F62
    A(6,3)    = -TAUBF_DD*F63
    A(6,4)    = 0.0d0
    A(6,5)    = 0.0d0
    A(6,6)    = 1.0d0
    A(6,7)    = 0.0d0
    A(6,8)    = -RHOFF_DD*F810
    A(6,9)    = -RHOFF_DD*F811
    A(6,10)    = -RHOFF_DD*F812
    A(6,11)    = Z8_BD
    A(7,1)    = -TAUBF_DD*F51
    A(7,2)    = -TAUBF_DD*F52
    A(7,3)    = -TAUBF_DD*F53
    A(7,4)    = 0.0d0
    A(7,5)    = 0.0d0
    A(7,6)    = 0.0d0
    A(7,7)    = 1.0d0
    A(7,8)    = -RHOFF_DD*F910
    A(7,9)    = -RHOFF_DD*F911
    A(7,10)    = -RHOFF_DD*F912
    A(7,11)    = 0.0d0
    A(8,1)    = 0.0d0
    A(8,2)    = 0.0d0
    A(8,3)    = 0.0d0
    A(8,4)    = 0.0d0
    A(8,5)    = 0.0d0
    A(8,6)    = -RHOFF_DD*F108
    A(8,7)    = -RHOFF_DD*F109
    A(8,8)    = 1.0d0
    A(8,9)    = -RHOFF_DD*F1011
    A(8,10)    = -RHOFF_DD*F1012
    A(8,11)    = 0.0d0
    A(9,1)    = -TAUBF_DD*F31
    A(9,2)    = 0.0d0
    A(9,3)    = 0.0d0
    A(9,4)    = -TAUBF_DD*F35
    A(9,5)    = -TAUBF_DD*F36
    A(9,6)    = -RHOFF_DD*F118
    A(9,7)    = -RHOFF_DD*F119
    A(9,8)    = -RHOFF_DD*F1110
    A(9,9)    = 1.0d0
    A(9,10)    = 0.0d0
    A(9,11)    = Z11_BD
    A(10,1)    = -TAUBF_DD*F21
    A(10,2)    = 0.0d0
    A(10,3)    = 0.0d0
    A(10,4)    = -TAUBF_DD*F25
    A(10,5)    = -TAUBF_DD*F26
    A(10,6)    = -RHOFF_DD*F128
    A(10,7)    = -RHOFF_DD*F129
    A(10,8)    = -RHOFF_DD*F1210
    A(10,9)    = 0.0d0
    A(10,10)    = 1.0d0
    A(10,11)    = Z12_BD

    CALL SOLMATS(N,A,XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J3 = XSOL(3)
    J5 = XSOL(4)
    J6 = XSOL(5)
    J8 = XSOL(6)
    J9 = XSOL(7)
    J10 = XSOL(8)
    J11 = XSOL(9)
    J12 = XSOL(10)

    G1 = F12*J2+F13*J3+F15*J5+F16*J6
    G4 = F41*J1+F42*J2+F43*J3+F45*J5+F46*J6
    G7 = F78*J8+F79*J9+F710*J10+F711*J11+F712*J12
    G10 = F108*J8+F109*J9+F1011*J11+F1012*J12

    TAU_BB = 0.0d0
    TAU_BD = (G4+TAUFF_DD*G10)/2.d0
    RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G7)/2.d0
    RETURN
END SUBROUTINE PD_BEAM_CASE_II

SUBROUTINE PD_BEAM_CASE_III(S, W, OMEGA_H, DE, &
                            RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                            RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                            RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                            RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                            RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                            RHO_BD, TAU_BD, TAU_BB)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! TWELVE SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S              ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W              ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OMEGA_H        ! horizontal profile angle, radians
    REAL(r64), INTENT (IN) :: DE             ! width of illumination on pleat bottom (same units as S)

    ! fabric properties at current (off-normal) incidence
    !   _PARL = surface parallel to window (pleat top/bot)
    !   _PERP = surface perpendicular to window (pleat side)
    REAL(r64), INTENT (IN) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP

    REAL(r64), INTENT (IN) :: RHOFF_DD       ! fabric front diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: RHOBF_DD       ! fabric back diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: TAUFF_DD       ! fabric front diffuse-diffuse transmittance
    REAL(r64), INTENT (IN) :: TAUBF_DD       ! fabric back diffuse-diffuse transmittance
    REAL(r64), INTENT (OUT) :: RHO_BD        ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BD        ! returned: drape front beam-diffuse transmittance
    REAL(r64), INTENT (OUT) :: TAU_BB        ! returned: drape front beam-beam transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,      PARAMETER :: N = 10

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAUBF_BT_PERP
    REAL(r64) :: AB, GN, NK, BC, AN, AK, BG, CG, BK, CN      ! lengths for surfaces and diagonal strings
    REAL(r64) :: Z1_BB, Z6_BB                                ! beam source terms
    REAL(r64) :: Z1_BD, Z2_BD, Z6_BD, Z3_BD, Z8_BD, Z11_BD, Z12_BD    ! diffuse source terms
    ! shape factors
    REAL(r64) :: F12, F13, F15, F16, F21, F25, F26, F31, F35, F36, F41, F42, F43, F45, F46, F51, F52, F53, F54, F61, F62, F63
    REAL(r64) :: F78, F79, F710, F711, F712, F810, F811, F812, F910, F911, F912, F108, F109, F1011, F1012, F118, F119, F1110
    REAL(r64) :: F128, F129, F1210
    REAL(r64) :: J1, J2, J3, J5, J6, J8, J9, J10, J11, J12    ! radiosity, surface i
    REAL(r64) :: G1, G4, G7, G10                              ! irradiance, surface i

    REAL(r64) :: A( N, N+2)    ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
          ! Flow

    TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

    AB = DE
    GN = DE
    NK = W-DE
    BC = NK
    AN = SQRT(S*S+DE*DE)
    AK = SQRT(W*W+S*S)
    BG = AN
    CG = AK
    BK = SQRT(S*S+BC*BC)
    CN = SQRT(S*S+NK*NK)

    Z1_BB = TAUFF_BB_PARL
    Z1_BD = TAUFF_BD_PARL
    Z2_BD = Z1_BB*RHOBF_BT_PERP*S/GN
    Z6_BB = TAUFF_BB_PERP*S/DE
    Z6_BD = TAUFF_BD_PERP*S/DE
    Z3_BD = Z6_BB*RHOBF_BT_PERP
    Z8_BD = RHOFF_BT_PERP*S/DE
    Z11_BD = Z6_BB*TAUBF_BT_PERP
    Z12_BD = Z1_BB*TAUBF_BT_PERP*S/GN

    F12 = (S+GN-AN)/(2.d0*S)
    F13 = (W+AN-(GN+AK))/(2.d0*S)
    F15 = (W+BG-(AB+CG))/(2.d0*S)
    F16 = (S+AB-BG)/(2.d0*S)
    F21 = (S+GN-AN)/(2.d0*GN)
    F25 = (S+CG-(BG+CN))/(2.d0*GN)
    F26 = (AN+BG-2.d0*S)/(2.d0*GN)
    F31 = (W+AN-(GN+AK))/(2.d0*NK)
    F35 = (BK+CN-2.d0*S)/(2.d0*NK)
    F36 = (S+AK-(AN+BK))/(2.d0*NK)
    F41 = (AK+CG-2.d0*W)/(2.d0*S)
    F42 = (W+CN-(CG+NK))/(2.d0*S)
    F43 = (S+NK-CN)/(2.d0*S)
    F45 = (S+BC-BK)/(2.d0*S)
    F46 = (W+BK-(AK+BC))/(2.d0*S)
    F51 = (W+BG-(AB+CG))/(2.d0*BC)
    F52 = (S+CG-(BG+CN))/(2.d0*BC)
    F53 = (BK+CN-2.d0*S)/(2.d0*BC)
    F54 = (S+BC-BK)/(2.d0*BC)
    F61 = (S+AB-BG)/(2.d0*AB)
    F62 = (AN+BG-2.d0*S)/(2.d0*AB)
    F63 = (S+AK-(AN+BK))/(2.d0*AB)
    F78 = F12
    F79 = F13
    F710 = (AK+CG-2.d0*W)/(2.d0*S)
    F711 = F15
    F712 = F16
    F810 = (W+CN-(CG+NK))/(2.d0*S)
    F811 = F25
    F812 = F26
    F910 = (S+NK-CN)/(2.d0*NK)
    F911 = F35
    F912 = F36
    F108 = F42
    F109 = F43
    F1011 = F45
    F1012 = F46
    F118 = F52
    F119 = F53
    F1110 = (S+BC-BK)/(2.d0*NK)
    F128 = F62
    F129 = F63
    F1210 = (W+BK-(AK+BC))/(2.d0*GN)

    A = 0.0d0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS
    XSOL = 0.0d0         ! INITIALIZE SOLUTION VECTOR COEFFICIENTS

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1)    = 1.0d0
    A(1,2)    = -RHOBF_DD*F12
    A(1,3)    = -RHOBF_DD*F13
    A(1,4)    = -RHOBF_DD*F15
    A(1,5)    = -RHOBF_DD*F16
    A(1,6)    = 0.0d0
    A(1,7)    = 0.0d0
    A(1,8)    = 0.0d0
    A(1,9)    = 0.0d0
    A(1,10)   = 0.0d0
    A(1,11)   = Z1_BD
    A(2,1)    = -RHOBF_DD*F21
    A(2,2)    = 1.0d0
    A(2,3)    = 0.0d0
    A(2,4)    = -RHOBF_DD*F25
    A(2,5)    = -RHOBF_DD*F26
    A(2,6)    = -TAUFF_DD*F128
    A(2,7)    = -TAUFF_DD*F129
    A(2,8)    = -TAUFF_DD*F1210
    A(2,9)    = 0.0d0
    A(2,10)   = 0.0d0
    A(2,11)   = Z2_BD
    A(3,1)    = -RHOBF_DD*F31
    A(3,2)    = 0.0d0
    A(3,3)    = 1.0d0
    A(3,4)    = -RHOBF_DD*F35
    A(3,5)    = -RHOBF_DD*F36
    A(3,6)    = -TAUFF_DD*F118
    A(3,7)    = -TAUFF_DD*F119
    A(3,8)    = -TAUFF_DD*F1110
    A(3,9)    = 0.0d0
    A(3,10)   = 0.0d0
    A(3,11)   = Z3_BD
    A(4,1)    = -RHOBF_DD*F51
    A(4,2)    = -RHOBF_DD*F52
    A(4,3)    = -RHOBF_DD*F53
    A(4,4)    = 1.0d0
    A(4,5)    = 0.0d0
    A(4,6)    = 0.0d0
    A(4,7)    = 0.0d0
    A(4,8)    = -TAUFF_DD*F910
    A(4,9)    = -TAUFF_DD*F911
    A(4,10)   = -TAUFF_DD*F912
    A(4,11)   = 0.0d0
    A(5,1)    = -RHOBF_DD*F61
    A(5,2)    = -RHOBF_DD*F62
    A(5,3)    = -RHOBF_DD*F63
    A(5,4)    = 0.0d0
    A(5,5)    = 1.0d0
    A(5,6)    = 0.0d0
    A(5,7)    = 0.0d0
    A(5,8)    = -TAUFF_DD*F810
    A(5,9)    = -TAUFF_DD*F811
    A(5,10)   = -TAUFF_DD*F812
    A(5,11)   = Z6_BD
    A(6,1)    = -TAUBF_DD*F61
    A(6,2)    = -TAUBF_DD*F62
    A(6,3)    = -TAUBF_DD*F63
    A(6,4)    = 0.0d0
    A(6,5)    = 0.0d0
    A(6,6)    = 1.0d0
    A(6,7)    = 0.0d0
    A(6,8)    = -RHOFF_DD*F810
    A(6,9)    = -RHOFF_DD*F811
    A(6,10)   = -RHOFF_DD*F812
    A(6,11)   = Z8_BD
    A(7,1)    = -TAUBF_DD*F51
    A(7,2)    = -TAUBF_DD*F52
    A(7,3)    = -TAUBF_DD*F53
    A(7,4)    = 0.0d0
    A(7,5)    = 0.0d0
    A(7,6)    = 0.0d0
    A(7,7)    = 1.0d0
    A(7,8)    = -RHOFF_DD*F910
    A(7,9)    = -RHOFF_DD*F911
    A(7,10)   = -RHOFF_DD*F912
    A(7,11)   = 0.0d0
    A(8,1)    = 0.0d0
    A(8,2)    = 0.0d0
    A(8,3)    = 0.0d0
    A(8,4)    = 0.0d0
    A(8,5)    = 0.0d0
    A(8,6)    = -RHOFF_DD*F108
    A(8,7)    = -RHOFF_DD*F109
    A(8,8)    = 1.0d0
    A(8,9)    = -RHOFF_DD*F1011
    A(8,10)   = -RHOFF_DD*F1012
    A(8,11)   = 0.0d0
    A(9,1)    = -TAUBF_DD*F31
    A(9,2)    = 0.0d0
    A(9,3)    = 0.0d0
    A(9,4)    = -TAUBF_DD*F35
    A(9,5)    = -TAUBF_DD*F36
    A(9,6)    = -RHOFF_DD*F118
    A(9,7)    = -RHOFF_DD*F119
    A(9,8)    = -RHOFF_DD*F1110
    A(9,9)    = 1.0d0
    A(9,10)   = 0.0d0
    A(9,11)   = Z11_BD
    A(10,1)   = -TAUBF_DD*F21
    A(10,2)   = 0.0d0
    A(10,3)   = 0.0d0
    A(10,4)   = -TAUBF_DD*F25
    A(10,5)   = -TAUBF_DD*F26
    A(10,6)   = -RHOFF_DD*F128
    A(10,7)   = -RHOFF_DD*F129
    A(10,8)   = -RHOFF_DD*F1210
    A(10,9)   = 0.0d0
    A(10,10)  = 1.0d0
    A(10,11)  = Z12_BD

    CALL SOLMATS(N,A,XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J3 = XSOL(3)
    J5 = XSOL(4)
    J6 = XSOL(5)
    J8 = XSOL(6)
    J9 = XSOL(7)
    J10 = XSOL(8)
    J11 = XSOL(9)
    J12 = XSOL(10)

    G1 = F12*J2+F13*J3+F15*J5+F16*J6
    G4 = F41*J1+F42*J2+F43*J3+F45*J5+F46*J6
    G7 = F78*J8+F79*J9+F710*J10+F711*J11+F712*J12
    G10 = F108*J8+F109*J9+F1011*J11+F1012*J12

    TAU_BB = (TAUFF_BB_PERP*(AB-NK)*ABS(SIN(OMEGA_H)))/(2.d0*S*ABS(COS(OMEGA_H)))
    TAU_BD = (G4+TAUFF_DD*G10)/2.d0
    RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G7)/2.d0
    RETURN
END SUBROUTINE PD_BEAM_CASE_III

SUBROUTINE PD_BEAM_CASE_IV(S, W, OMEGA_H, DE, &
                           RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                           RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                           RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                           RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                           RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                           RHO_BD, TAU_BD, TAU_BB)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! Eight surface flat-fabric model with rectangular enclosure
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S              ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W              ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OMEGA_H        ! horizontal profile angle, radians
    REAL(r64), INTENT (IN) :: DE             ! width of illumination on pleat bottom (same units as S)

    ! fabric properties at current (off-normal) incidence
    !   _PARL = surface parallel to window (pleat top/bot)
    !   _PERP = surface perpendicular to window (pleat side)
    REAL(r64), INTENT (IN) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOFF_DD       ! fabric front diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: RHOBF_DD       ! fabric back diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: TAUFF_DD       ! fabric front diffuse-diffuse transmittance
    REAL(r64), INTENT (IN) :: TAUBF_DD       ! fabric back diffuse-diffuse transmittance
    REAL(r64), INTENT (OUT):: RHO_BD         ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT):: TAU_BD         ! returned: drape front beam-diffuse transmittance
    REAL(r64), INTENT (OUT):: TAU_BB         ! returned: drape front beam-beam transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,      PARAMETER :: N = 6

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) ::TAUBF_BT_PERP
    REAL(r64) :: AK, CG                               ! length of diagonal strings
    REAL(r64) :: Z1_BB                                ! beam source term
    REAL(r64) :: Z1_BD, Z2_BD, Z4_BD, Z6_BD, Z8_BD    ! diffuse source terms
    ! shape factors
    REAL(r64) :: F12, F14, F21, F24, F31, F32, F34, F41, F42, F56, F57, F58
    REAL(r64) :: F67, F68, F76, F78, F86, F87
    REAL(r64) :: J1, J2, J4, J6, J7, J8               ! radiosity, surface i
    REAL(r64) :: G1, G3, G5, G7                       ! irradiance, surface i
    REAL(r64) :: A( N, N+2)    ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
          ! Flow

    TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

    AK = SQRT(W*W+S*S)
    CG = AK

    Z1_BB = TAUFF_BB_PARL
    Z1_BD = TAUFF_BD_PARL
    Z2_BD = Z1_BB*RHOBF_BT_PERP*S/W
    Z4_BD = TAUFF_BD_PERP*S/W
    Z6_BD = RHOFF_BT_PERP*S/W
    Z8_BD = Z1_BB*TAUBF_BT_PERP*S/W

    F12 = (S+W-AK)/(2.d0*S)
    F14 = (S+W-CG)/(2.d0*S)
    F21 = (S+W-AK)/(2.d0*W)
    F24 = (AK+CG-2.d0*S)/(2.d0*W)
    F31 = (AK+CG-2.d0*W)/(2.d0*S)
    F32 = F12
    F34 = F12
    F41 = F21
    F42 = F24
    F56 = F12
    F57 = F31
    F58 = F14
    F67 = F41
    F68 = F24
    F76 = F32
    F78 = F34
    F86 = F42
    F87 = F21

    A = 0.0d0           ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS
    XSOL = 0.0d0        ! INITIALIZE SOLUTION VECTOR COEFFICIENTS

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1)    = 1.0d0
    A(1,2)    = -RHOBF_DD*F12
    A(1,3)    = -RHOBF_DD*F14
    A(1,4)    = 0.0d0
    A(1,5)    = 0.0d0
    A(1,6)    = 0.0d0
    A(1,7)    = Z1_BD
    A(2,1)    = -RHOBF_DD*F21
    A(2,2)    = 1.0d0
    A(2,3)    = -RHOBF_DD*F24
    A(2,4)    = -TAUFF_DD*F86
    A(2,5)    = -TAUFF_DD*F87
    A(2,6)    = 0.0d0
    A(2,7)    = Z2_BD
    A(3,1)    = -RHOBF_DD*F41
    A(3,2)    = -RHOBF_DD*F42
    A(3,3)    = 1.0d0
    A(3,4)    = 0.0d0
    A(3,5)    = -TAUFF_DD*F67
    A(3,6)    = -TAUFF_DD*F68
    A(3,7)    = Z4_BD
    A(4,1)    = -TAUBF_DD*F41
    A(4,2)    = -TAUBF_DD*F42
    A(4,3)    = 0.0d0
    A(4,4)    = 1.0d0
    A(4,5)    = -RHOFF_DD*F67
    A(4,6)    = -RHOFF_DD*F68
    A(4,7)    = Z6_BD
    A(5,1)    = 0.0d0
    A(5,2)    = 0.0d0
    A(5,3)    = 0.0d0
    A(5,4)    = -RHOFF_DD*F76
    A(5,5)    = 1.0d0
    A(5,6)    = -RHOFF_DD*F78
    A(5,7)    = 0.0d0
    A(6,1)    = -TAUBF_DD*F21
    A(6,2)    = 0.0d0
    A(6,3)    = -TAUBF_DD*F24
    A(6,4)    = -RHOFF_DD*F86
    A(6,5)    = -RHOFF_DD*F87
    A(6,6)    = 1.0d0
    A(6,7)    = Z8_BD

    CALL SOLMATS(N,A,XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J4 = XSOL(3)
    J6 = XSOL(4)
    J7 = XSOL(5)
    J8 = XSOL(6)

    G1 = F12*J2+F14*J4
    G3 = F31*J1+F32*J2+F34*J4
    G5 = F56*J6+F57*J7+F58*J8
    G7 = F76*J6+F78*J8

    TAU_BB = TAUFF_BB_PERP/2.d0
    TAU_BD = (G3+TAUFF_DD*G7)/2.d0
    RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G5)/2.d0
    RETURN
END SUBROUTINE PD_BEAM_CASE_IV

SUBROUTINE PD_BEAM_CASE_V(S, W, OMEGA_H, DE, &
                          RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                          RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                          RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                          RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                          RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                          RHO_BD, TAU_BD, TAU_BB)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! NINE SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S             ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W             ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OMEGA_H       ! horizontal profile angle, radians
    REAL(r64), INTENT (IN) :: DE            ! width of illumination on pleat bottom (same units as S)

    ! fabric properties at current (off-normal) incidence
    !   _PARL = surface parallel to window (pleat top/bot)
    !   _PERP = surface perpendicular to window (pleat side)
    REAL(r64), INTENT (IN) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOFF_DD      ! fabric front diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: RHOBF_DD      ! fabric back diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: TAUFF_DD      ! fabric front diffuse-diffuse transmittance
    REAL(r64), INTENT (IN) :: TAUBF_DD      ! fabric back diffuse-diffuse transmittance
    REAL(r64), INTENT (OUT) :: RHO_BD       ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BD       ! returned: drape front beam-diffuse transmittance
    REAL(r64), INTENT (OUT) :: TAU_BB       ! returned: drape front beam-beam transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,      PARAMETER :: N = 7

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAUBF_BT_PERP
    REAL(r64) :: AK, CG, MK, DK, MF, DM, GM, GF       ! lengths of surfaces and diagonal strings
    REAL(r64) :: Z1_BB                                ! beam source term
    REAL(r64) :: Z1_BD, Z2_BD, Z4_BD, Z6_BD, Z7_BD, Z9_BD    ! diffuse source terms
    ! shape factors
    REAL(r64) :: F12, F14, F21, F24, F31, F32, F34, F41, F42, F56, F57, F58, F59
    REAL(r64) :: F67, F68, F69, F76, F79, F86, F89, F96, F97, F98
    REAL(r64) :: J1, J2, J4, J6, J7, J8, J9         ! radiosities
    REAL(r64) :: G1, G3, G5, G7, G8                 ! irradiances

    REAL(r64) :: A( N, N+2)    ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
          ! Flow

    TAUBF_BT_PERP = TAUBF_BD_PERP + TAUBF_BB_PERP

    AK = SQRT(W*W+S*S)
    CG = AK
    MK = (W*ABS(SIN(OMEGA_H)))/ABS(COS(OMEGA_H))
    DK = AK
    MF = S-MK
    DM = SQRT(W*W+MF*MF)
    GM = SQRT(W*W+MK*MK)
    GF = AK

    Z1_BB = TAUFF_BB_PARL
    Z1_BD = TAUFF_BD_PARL
    Z2_BD = Z1_BB*RHOBF_BT_PERP*S/DE
    Z4_BD = TAUFF_BD_PERP*S/DE
    Z6_BD = RHOFF_BT_PERP*S/DE
    Z7_BD = RHOFF_BT_PARL
    Z9_BD = Z1_BB*TAUBF_BT_PERP*S/DE

    F12 = (S+W-AK)/(2.d0*S)
    F14 = (S+W-CG)/(2.d0*S)
    F21 = (S+W-AK)/(2.d0*W)
    F24 = (AK+CG-2.d0*S)/(2.d0*W)
    F31 = (AK+CG-2.d0*W)/(2.d0*S)
    F32 = F14
    F34 = F12
    F41 = F21
    F42 = F24
    F56 = F12
    F57 = (DM+GF-(GM+W))/(2.d0*S)
    F58 = (DK+GM-(DM+W))/(2.d0*S)
    F59 = F14
    F67 = (W+MF-DM)/(2.d0*W)
    F68 = (DM+S-(DK+MF))/(2.d0*W)
    F69 = F24
    F76 = (W+MF-DM)/(2.d0*MF)
    F79 = (GM+S-(GF+MK))/(2.d0*MF)
    F86 = (DM+S-(DK+MF))/(2.d0*MK)
    F89 = (W+MK-GM)/(2.d0*MK)
    F96 = F42
    F97 = (GM+S-(GF+MK))/(2.d0*W)
    F98 = (W+MK-GM)/(2.d0*W)


    A = 0.0d0            ! INITIALIZE RADIOSITY MATRIX COEFFICIENTS
    XSOL = 0.0d0         ! INITIALIZE SOLUTION VECTOR COEFFICIENTS

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1)    = 1.0d0
    A(1,2)    = -RHOBF_DD*F12
    A(1,3)    = -RHOBF_DD*F14
    A(1,4)    = 0.0d0
    A(1,5)    = 0.0d0
    A(1,6)    = 0.0d0
    A(1,7)    = 0.0d0
    A(1,8)    = Z1_BD
    A(2,1)    = -RHOBF_DD*F21
    A(2,2)    = 1.0d0
    A(2,3)    = -RHOBF_DD*F24
    A(2,4)    = -TAUFF_DD*F96
    A(2,5)    = -TAUFF_DD*F97
    A(2,6)    = -TAUFF_DD*F98
    A(2,7)    = 0.0d0
    A(2,8)    = Z2_BD
    A(3,1)    = -RHOBF_DD*F41
    A(3,2)    = -RHOBF_DD*F42
    A(3,3)    = 1.0d0
    A(3,4)    = 0.0d0
    A(3,5)    = -TAUFF_DD*F67
    A(3,6)    = -TAUFF_DD*F68
    A(3,7)    = -TAUFF_DD*F69
    A(3,8)    = Z4_BD
    A(4,1)    = -TAUBF_DD*F41
    A(4,2)    = -TAUBF_DD*F42
    A(4,3)    = 0.0d0
    A(4,4)    = 1.0d0
    A(4,5)    = -RHOFF_DD*F67
    A(4,6)    = -RHOFF_DD*F68
    A(4,7)    = -RHOFF_DD*F69
    A(4,8)    = Z6_BD
    A(5,1)    = 0.0d0
    A(5,2)    = 0.0d0
    A(5,3)    = 0.0d0
    A(5,4)    = -RHOFF_DD*F76
    A(5,5)    = 1.0d0
    A(5,6)    = 0.0d0
    A(5,7)    = -RHOFF_DD*F79
    A(5,8)    = Z7_BD
    A(6,1)    = 0.0d0
    A(6,2)    = 0.0d0
    A(6,3)    = 0.0d0
    A(6,4)    = -RHOFF_DD*F86
    A(6,5)    = 0.0d0
    A(6,6)    = 1.0d0
    A(6,7)    = -RHOFF_DD*F89
    A(6,8)    = 0.0d0
    A(7,1)    = -TAUBF_DD*F21
    A(7,2)    = 0.0d0
    A(7,3)    = -TAUBF_DD*F24
    A(7,4)    = -RHOFF_DD*F96
    A(7,5)    = -RHOFF_DD*F97
    A(7,6)    = -RHOFF_DD*F98
    A(7,7)    = 1.0d0
    A(7,8)    = Z9_BD

    CALL SOLMATS(N,A,XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J4 = XSOL(3)
    J6 = XSOL(4)
    J7 = XSOL(5)
    J8 = XSOL(6)
    J9 = XSOL(7)

    G1 = F12*J2+F14*J4
    G3 = F31*J1+F32*J2+F34*J4
    G5 = F56*J6+F57*J7+F58*J8+F59*J9
    G7 = F76*J6+F79*J9
    G8 = F86*J6+F89*J9

    TAU_BB = (2.d0*(DE-W)*ABS(SIN(OMEGA_H))*TAUFF_BB_PARL+(S*ABS(COS(OMEGA_H))-(DE-W)*ABS(SIN(OMEGA_H)))*TAUFF_BB_PERP) &
           / (2.d0*S*ABS(COS(OMEGA_H)))
    TAU_BD = (S*G3+TAUFF_DD*(MK*G8+MF*G7)+MF*TAUFF_BD_PARL)/(2.d0*S)
    RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G5)/2.d0
    RETURN
END SUBROUTINE PD_BEAM_CASE_V

SUBROUTINE PD_BEAM_CASE_VI(S, W, OMEGA_H, DE, &
                           RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL, &
                           RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL, &
                           RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP, &
                           RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP, &
                           RHOBF_DD, RHOFF_DD, TAUFF_DD, TAUBF_DD, &
                           RHO_BD, TAU_BD, TAU_BB)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  calculates the effective front-side solar optical properties of a drapery layer.
          !
          ! METHODOLOGY EMPLOYED:
          ! EIGHT SURFACE FLAT-FABRIC MODEL WITH RECTANGULAR ENCLOSURE
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S             ! pleat spacing (> 0)
    REAL(r64), INTENT( IN) :: W             ! pleat depth (>=0, same units as S)
    REAL(r64), INTENT (IN) :: OMEGA_H       ! horizontal profile angle, radians
    REAL(r64), INTENT (IN) :: DE            ! width of illumination on pleat bottom (same units as S)
    ! fabric properties at current (off-normal) incidence
    !   _PARL = surface parallel to window (pleat top/bot)
    !   _PERP = surface perpendicular to window (pleat side)
    REAL(r64), INTENT (IN) :: RHOFF_BT_PARL, TAUFF_BB_PARL, TAUFF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOBF_BT_PARL, TAUBF_BB_PARL, TAUBF_BD_PARL
    REAL(r64), INTENT (IN) :: RHOFF_BT_PERP, TAUFF_BB_PERP, TAUFF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOBF_BT_PERP, TAUBF_BB_PERP, TAUBF_BD_PERP
    REAL(r64), INTENT (IN) :: RHOFF_DD      ! fabric front diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: RHOBF_DD      ! fabric back diffuse-diffuse reflectance
    REAL(r64), INTENT (IN) :: TAUFF_DD      ! fabric front diffuse-diffuse transmittance
    REAL(r64), INTENT (IN) :: TAUBF_DD      ! fabric back diffuse-diffuse transmittance
    REAL(r64), INTENT (OUT) :: RHO_BD       ! returned: drape front beam-diffuse reflectance
    REAL(r64), INTENT (OUT) :: TAU_BD       ! returned: drape front beam-diffuse transmittance
    REAL(r64), INTENT (OUT) :: TAU_BB       ! returned: drape front beam-beam transmittance
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER,      PARAMETER :: N = 6

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAUBF_BT_PERP
    REAL(r64) :: AK, CG            ! length of diagonal strings
    REAL(r64) :: Z1_BD, Z7_BD      ! diffuse source termps
    ! shape factors
    REAL(r64) :: F12, F14, F21, F24, F31, F32, F34, F41, F42, F56, F57, F58, F67, F68, F76, F78, F86, F87
    REAL(r64) :: J1, J2, J4, J6, J7, J8     ! radiosity, surface i
    REAL(r64) :: G1, G3, G5, G7             ! irradiance, surface i
    REAL(r64) :: A( N, N+2)        ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)          ! solution vector (obtained after solving the radiosity equations matrix)
          ! Flow

    AK = SQRT(W*W+S*S)
    CG = AK

    Z1_BD = TAUFF_BD_PARL
    Z7_BD = RHOFF_BT_PARL

    F12 = (S+W-AK)/(2.d0*S)
    F14 = (S+W-CG)/(2.d0*S)
    F21 = (S+W-AK)/(2.d0*W)
    F24 = (AK+CG-2.d0*S)/(2.d0*W)
    F31 = (AK+CG-2.d0*W)/(2.d0*S)
    F32 = F12
    F34 = F14
    F41 = F21
    F42 = F24
    F56 = F12
    F57 = F31
    F58 = F14
    F67 = F41
    F68 = F24
    F76 = F14
    F78 = F14
    F86 = F42
    F87 = F21

    A = 0.0d0
    XSOL = 0.0d0

    ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

    A(1,1)    = 1.0d0
    A(1,2)    = -RHOBF_DD*F12
    A(1,3)    = -RHOBF_DD*F14
    A(1,4)    = 0.0d0
    A(1,5)    = 0.0d0
    A(1,6)    = 0.0d0
    A(1,7)    = Z1_BD
    A(2,1)    = -RHOBF_DD*F21
    A(2,2)    = 1.0d0
    A(2,3)    = -RHOBF_DD*F24
    A(2,4)    = -TAUFF_DD*F86
    A(2,5)    = -TAUFF_DD*F87
    A(2,6)    = 0.0d0
    A(2,7)    = 0.0d0
    A(3,1)    = -RHOBF_DD*F41
    A(3,2)    = -RHOBF_DD*F42
    A(3,3)    = 1.0d0
    A(3,4)    = 0.0d0
    A(3,5)    = -TAUFF_DD*F67
    A(3,6)    = -TAUFF_DD*F68
    A(3,7)    = 0.0d0
    A(4,1)    = -TAUBF_DD*F41
    A(4,2)    = -TAUBF_DD*F42
    A(4,3)    = 0.0d0
    A(4,4)    = 1.0d0
    A(4,5)    = -RHOFF_DD*F67
    A(4,6)    = -RHOFF_DD*F68
    A(4,7)    = 0.0d0
    A(5,1)    = 0.0d0
    A(5,2)    = 0.0d0
    A(5,3)    = 0.0d0
    A(5,4)    = -RHOFF_DD*F76
    A(5,5)    = 1.0d0
    A(5,6)    = -RHOFF_DD*F78
    A(5,7)    = Z7_BD
    A(6,1)    = -TAUBF_DD*F21
    A(6,2)    = 0.0d0
    A(6,3)    = -TAUBF_DD*F24
    A(6,4)    = -RHOFF_DD*F86
    A(6,5)    = -RHOFF_DD*F87
    A(6,6)    = 1.0d0
    A(6,7)    = 0.0d0

    CALL SOLMATS(N,A,XSOL)

    J1 = XSOL(1)
    J2 = XSOL(2)
    J4 = XSOL(3)
    J6 = XSOL(4)
    J7 = XSOL(5)
    J8 = XSOL(6)

    G1 = F12*J2+F14*J4
    G3 = F31*J1+F32*J2+F34*J4
    G5 = F56*J6+F57*J7+F58*J8
    G7 = F76*J6+F78*J8

    TAU_BB = TAUFF_BB_PARL
    TAU_BD = (G3+TAUFF_DD*G7+TAUFF_BD_PARL)/2.d0
    RHO_BD = (RHOFF_BT_PARL+TAUBF_DD*G1+G5)/2.d0
    RETURN
END SUBROUTINE PD_BEAM_CASE_VI

SUBROUTINE VB_DIFF(S, W, PHI, RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT, RHOFVB, TAUVB)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculates the venetian blind layer effective diffuse transmittance and reflectance.
          !
          ! METHODOLOGY EMPLOYED:
          ! four surface flat-slat model with slat transmittance
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S           ! slat spacing (any length units; same units as W)
                                          !    must be > 0
    REAL(r64), INTENT( IN) :: W           ! slat tip-to-tip width (any length units; same units as S)
                                          !   must be > 0
    REAL(r64), INTENT( IN) :: PHI         ! slat angle, radians (-PI/2 <= PHI <= PI/2)
                                          !   ltyVBHOR: + = front-side slat tip below horizontal
                                          !   ltyVBVER: + = front-side slat tip is counter-
                                          !                 clockwise from normal (viewed from above)
    REAL(r64), INTENT( IN) :: RHODFS_SLAT !  reflectance of downward-facing slat surfaces (concave?)
    REAL(r64), INTENT( IN) :: RHOUFS_SLAT !  reflectance of upward-facing slat surfaces (convex?)
    REAL(r64), INTENT( IN) :: TAU_SLAT    !  diffuse transmitance of slats
    REAL(r64), INTENT( OUT) :: RHOFVB     !  returned: front side effective diffuse reflectance of venetian blind
    REAL(r64), INTENT( OUT) :: TAUVB      !  returned: effective diffuse transmittance of venetian blind
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: CD, AF                          ! lengths of the diagonal strings used in the four-surface model
    REAL(r64) :: F13, F14, F12, F31, F41, FSS    ! shape factors
    REAL(r64) :: C3, B3, C4, B4, K3, K4, DEN     ! temporaries
          ! flow

    CD = SQRT ((W*COS(PHI))**2   +   (S + W*SIN(PHI))**2)
    AF = SQRT ((W*COS(PHI))**2   +   (S - W*SIN(PHI))**2)

    F13 = (W+S-CD)/(2.d0*S)       ! SHAPE FACTOR FRONT OPENING TO TOP SLAT
    F14 = (W+S-AF)/(2.d0*S)       ! SHAPE FACTOR FRONT OPENING TO BOTTOM SLAT
    FSS = 1.d0 - (S/W)*(F13+F14)  ! SLAT-TO-SLAT SHAPE FACTOR
    F31 = (S/W)*F13               ! SHAPE FACTOR - TOP TO FRONT
    F41 = (S/W)*F14               ! SHAPE FACTOR - BOTTOM TO FRONT
    F12 = 1.d0 - F13 - F14        ! FRONT OPENING TO BACK OPENING SHAPE FACTOR
    DEN = 1.d0 - (TAU_SLAT*FSS)   ! DENOMINATOR - USED FOUR TIMES
    C3 = (RHODFS_SLAT*F31 + TAU_SLAT*F41)/DEN
    B3 = (RHODFS_SLAT*FSS)/DEN
    C4 = (RHOUFS_SLAT*F41 + TAU_SLAT*F31)/DEN
    B4 = (RHOUFS_SLAT*FSS)/DEN

    K3 = (C3 + (B3*C4))/(1.d0 - (B3*B4))
    K4 = (C4 + (B4*C3))/(1.d0 - (B3*B4))
    ! transmittance of VB (equal front/back)
    TAUVB = P01( F12 + (F14*K3) + (F13*K4), "VB_DIFF Tau")
    ! diffuse reflectance of VB front-side
    RHOFVB = P01( (F13*K3) + (F14*K4), "VB_DIFF RhoF")
    RETURN
END SUBROUTINE VB_DIFF

REAL(r64) FUNCTION VB_SLAT_RADIUS_RATIO(W, C)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Returns curved slat radius ratio (W / R)
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN):: W  ! slat tip-to-tip (chord) width (any units; same units as C) must be > 0
    REAL(r64), INTENT(IN):: C  ! slat crown height (any units, same units as W) must be >= 0
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)            :: CX !
          ! Flow

    IF (C <= 0.0d0 .OR. W <= 0.0d0) THEN
        ! it is flat
        VB_SLAT_RADIUS_RATIO = 0.0d0
    ELSE
        CX = MIN( C, W/2.001d0)
        VB_SLAT_RADIUS_RATIO = 2.0d0 * W * CX / (CX*CX + W*W/4)
    END IF
    RETURN
END FUNCTION VB_SLAT_RADIUS_RATIO

SUBROUTINE VB_SOL46_CURVE(S, W, SL_WR, PHIx, OMEGAx, RHODFS_SLAT, RHOUFS_SLAT, &
                          TAU_SLAT, RHO_BD, TAU_BB, TAU_BD)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculates the venetian blind layer effective solar transmittance and reflectance.
          !
          ! METHODOLOGY EMPLOYED:
          ! Four and six surface curve-slat model with slat transmittance. For back side
          ! reflectance call this routine a second time with the same input data - except
          ! negative the slat angle, PHI_DEG.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: S        ! slat spacing (any length units; same units as W)
                                      !    must be > 0
    REAL(r64),  INTENT(IN):: W        ! slat tip-to-tip (chord) width (any length units; same units as S)
                                      !   must be > 0
    REAL(r64),  INTENT(IN):: SL_WR    ! slat curvature radius ratio (= W/R)
                                      !   0 = flat
    REAL(r64),  INTENT(IN):: PHIx     ! slat angle, radians (-PI/2 <= PHI <= PI/2)
                                      !   ltyVBHOR: + = front-side slat tip below horizontal
                                      !   ltyVBVER: + = front-side slat tip is counter-
                                      !                 clockwise from normal (viewed from above)
    REAL(r64),  INTENT(IN):: OMEGAx   ! incident beam profile angle (radians)
                                      !   ltyVBHOR: +=above horizontal
                                      !   ltyVBVER: +=clockwise when viewed from above
                                      !   Note: All solar slat properties are incident-to-diffuse
                                      !         Specular effects not covered by model
    REAL(r64),  INTENT(IN):: RHODFS_SLAT  ! SW (solar) reflectance downward-facing slat surfaces (concave?)
    REAL(r64),  INTENT(IN):: RHOUFS_SLAT  ! SW (solar) reflectance upward-facing slat surfaces (convex?)
    REAL(r64),  INTENT(IN):: TAU_SLAT     ! SW (solar) transmittance of slats
    REAL(r64), INTENT(OUT):: RHO_BD   ! returned: effective SW (solar) beam-to-diffuse reflectance front side
    REAL(r64), INTENT(OUT):: TAU_BB   ! returned: effective SW (solar) beam-to-beam transmittance front side
    REAL(r64), INTENT(OUT):: TAU_BD   ! returned: effective SW (solar) beam-to-diffuse transmittance front side
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: DE   ! distance from front tip of any slat to shadow (caused by the adjacent slat) on
                      ! the plane of the same slat; DE may be greater than the slat width, W
    REAL(r64) :: PHI
    REAL(r64) :: OMEGA
    REAL(r64) :: SL_RAD,SL_THETA,Slope,T_CORR_D,T_CORR_F,RHO_TEMP,TAU_TEMP
    REAL(r64) :: XA,XB,XC,XD,XE,XF,YA,YB,YC,YD,YE,YF
    INTEGER   :: CORR
          ! Flow

    DE = 0.0d0          ! INITIALIZE DE
    CORR=1

    ! limit slat angle to +/- 90 deg
    PHI = MAX( -DegToRadians*90.0d0, MIN( DegToRadians*90.0d0, PHIx))
    ! limit profile angle to +/- 89.5 deg
    OMEGA = MAX( -DegToRadians*89.5d0, MIN( DegToRadians*89.5d0, OMEGAx))

    SL_RAD = W / MAX( SL_WR, 0.0000001d0)
    SL_THETA = 2.d0 * ASIN( 0.5d0*SL_WR)

    IF (CORR>0) THEN    ! CORRECT FOR SLAT CURVATURE BY SETTING CORR = 1

    !  DETERMINE BOUNDS FOR CURVATURE CORRECTION AND APPLY CORRECTION TO BEAM-BEAM TRANSMITTANCE
    IF( ABS(PHI+OMEGA) < SL_THETA/2.d0) THEN
        !  CALCULATE BEAM TRANSMISSION
        XA=SL_RAD*SIN(-SL_THETA/2.d0)    !Glass-side end coordinate
        YA=SL_RAD*COS(-SL_THETA/2.d0)
        XB=-XA                        !Indoor-side end coordinate
        YB=YA
        YC=SL_RAD*COS(PHI+OMEGA)    !Tangent to slat in irradiance direction
        XC=SQRT(SL_RAD**2-YC**2)
        Slope=-XC/YC
        IF (ABS(Slope) < SMALL_ERROR) THEN
            XD=0.0d0
            YD=YA
            XE=0.0d0
            YE=YD
        ELSE
            IF ((PHI+OMEGA) < 0.0d0) THEN
                XC=-XC
                Slope=-Slope
                XD=(YB-Slope*XB)/(-1.d0/Slope-Slope)
                XF=(YA-Slope*XA)/(-1.d0/Slope-Slope)
                XE=XA+2.d0*ABS(XA-XF)
            ELSE
                XD=(YA-Slope*XA)/(-1.d0/Slope-Slope)
                XF=(YB-Slope*XB)/(-1.d0/Slope-Slope)
                XE=XB-2.d0*ABS(XB-XF)
            ENDIF
            YD=-XD/Slope
            YE=-XE/Slope
            YF=-XF/Slope
        ENDIF

        T_CORR_D=SQRT((XC-XD)**2+(YC-YD)**2)    !Slat thickness perpendicular to light direction
        T_CORR_F=SQRT((XC-XF)**2+(YC-YF)**2)

        TAU_BB=1.0d0 - T_CORR_D/(S*COS(OMEGA))

    ELSE
        ! DO NOT APPLY CURVATURE CORRECTION TO BEAM-BEAM TRANSMITTANCE
        IF (ABS( OMEGA + PHI) < 0.0001d0) THEN
            DE = S*1000000.0d0
        ELSE
            DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
        ENDIF
        !  CHECK TO SEE IF THERE IS DIRECT BEAM TRANSMISSION
        IF ((DE/W) > (1.0d0 - SMALL_ERROR)) THEN   ! YES
            TAU_BB = MAX( 0.d0, (DE-W)/DE)
        ELSE                                ! NO
            TAU_BB = 0.0d0
        ENDIF

    ENDIF

    ! CHECK TO SEE IF CURVATURE CORRECTION INCLUDES DOUBLE BLOCKAGE
    ! (TAU_BB < 0.0 AND SET TAU_BB = 0.0)
    IF (TAU_BB < 0.0d0) THEN  ! YES, THERE IS DOUBLE BLOCKAGE

        TAU_BB = 0.0d0

    ! DO NOT APPLY CURVATURE CORRECTION TO RHO_BD, TAU_BD IF TAU_BB < 0.0
        IF (ABS( OMEGA + PHI) < 0.0001d0) THEN
            DE = S*1000000.0d0
        ELSE
            DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
        ENDIF
        IF((DE/W) > (1.0d0 - SMALL_ERROR)) THEN          ! YES
            CALL VB_SOL4(S, W, OMEGA, DE, PHI, &
                        RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                        RHO_BD, TAU_BD)

        ELSE                                ! NO
            CALL VB_SOL6(S, W, OMEGA, DE, PHI, &
                       RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                       RHO_BD, TAU_BD)
        ENDIF

    ELSE  ! NO, THERE IS NO DOUBLE BLOCKAGE

        IF(ABS(PHI+OMEGA)<(SL_THETA/2.d0)) THEN  ! YES, APPLY CURVATURE CORRECTION

            XA=SL_RAD*SIN(-SL_THETA/2.d0)    !Glass-side end coordinate
            YA=SL_RAD*COS(-SL_THETA/2.d0)
            XB=-XA                        !Indoor-side end coordinate
            YB=YA
            YC=SL_RAD*COS(PHI+OMEGA)    !Tangent to slat in irradiance direction
            XC=SQRT(SL_RAD**2-YC**2)
            Slope=-XC/YC
            IF (ABS(Slope) < SMALL_ERROR) THEN
                XD=0.0d0
                YD=YA
                XE=0.0d0
                YE=YD
            ELSE
                IF ((PHI+OMEGA) < 0.d0) THEN
                    XC=-XC
                    Slope=-Slope
                    XD=(YB-Slope*XB)/(-1.d0/Slope-Slope)
                    XF=(YA-Slope*XA)/(-1.d0/Slope-Slope)
                    XE=XA+2.0d0*ABS(XA-XF)
                ELSE
                    XD=(YA-Slope*XA)/(-1.d0/Slope-Slope)
                    XF=(YB-Slope*XB)/(-1.d0/Slope-Slope)
                    XE=XB-2.d0*ABS(XB-XF)
                ENDIF
                YD=-XD/Slope
                YE=-XE/Slope
                YF=-XF/Slope
            ENDIF
            T_CORR_D=SQRT((XC-XD)**2+(YC-YD)**2)    ! Slat thickness perpendicular to light direction
            T_CORR_F=SQRT((XC-XF)**2+(YC-YF)**2)


            IF ((PHI+OMEGA)>= 0.0d0) THEN           ! Slat is lit from above
                DE=XC-XA
                CALL VB_SOL6(S, W, OMEGA, DE, PHI, &
                                 RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                             RHO_BD, TAU_BD)
                RHO_BD=RHO_BD*T_CORR_D/(S*COS(OMEGA))
                TAU_BD=TAU_BD*T_CORR_D/(S*COS(OMEGA))


            ELSE                            ! Slat is lit from below
                DE=XC-XA
                CALL VB_SOL6(S, W, OMEGA, DE, PHI, &
                                 RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                             RHO_BD, TAU_BD)
                RHO_TEMP=RHO_BD*T_CORR_F/(S*COS(OMEGA))
                TAU_TEMP=TAU_BD*T_CORR_F/(S*COS(OMEGA))
                DE=ABS(XB-XF)
                CALL VB_SOL6(S, W, OMEGA, DE, PHI, &
                                 RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                             RHO_BD, TAU_BD)
                RHO_BD=RHO_BD*(T_CORR_D-T_CORR_F)/(S*COS(OMEGA))+RHO_TEMP
                TAU_BD=TAU_BD*(T_CORR_D-T_CORR_F)/(S*COS(OMEGA))+TAU_TEMP

            ENDIF


    ELSE ! NO, DO NOT APPLY CURVATURE CORRECTION
        IF (ABS( OMEGA + PHI) < 0.0001d0) THEN
            DE = S*1000000.0d0
        ELSE
            DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
        ENDIF
        IF((DE/W) > (1.0d0 - SMALL_ERROR)) THEN  ! YES
            CALL VB_SOL4(S, W, OMEGA, DE, PHI, &
                            RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                            RHO_BD, TAU_BD)

        ELSE                                     ! NO
            CALL VB_SOL6(S, W, OMEGA, DE, PHI, &
                             RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                             RHO_BD, TAU_BD)

        ENDIF

    ENDIF

    ENDIF

ELSE   ! DO NOT CORRECT FOR SLAT CURVATURE

    !  CHECK TO SEE IF BEAM IS ALLIGNED WITH SLATS
    IF(ABS(PHI + OMEGA) < SMALL_ERROR) THEN  ! YES!
        RHO_BD =       0.0d0
        TAU_BB =       1.0d0
        TAU_BD =       0.0d0

    ELSE                             ! BEAM NOT ALIGNED WITH SLATS
        RHO_BD =       0.0d0
        TAU_BB =       0.0d0
        TAU_BD =       0.0d0
        DE = S * ABS(COS(OMEGA) / SIN(OMEGA + PHI))
      !  CHECK TO SEE IF THERE IS DIRECT BEAM TRANSMISSION
      IF((DE/W) > (1.0d0 - SMALL_ERROR)) THEN          ! YES
        TAU_BB = (DE-W)/DE
        IF(TAU_BB < 0.d0) TAU_BB = 0.0d0
          CALL VB_SOL4(S, W, OMEGA, DE, PHI, &
                       RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                       RHO_BD, TAU_BD)
        ELSE                                ! NO
          TAU_BB = 0.0d0
          CALL VB_SOL6(S, W, OMEGA, DE, PHI, &
                       RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT,  &
                       RHO_BD, TAU_BD)
        ENDIF  !  END CHECK FOR DIRECT BEAM TRANSMISSION
    ENDIF  ! END CHECK TO SEE IF BEAM ALLIGNED WITH SLATS
ENDIF
RETURN
END SUBROUTINE VB_SOL46_CURVE

SUBROUTINE VB_SOL4(S, W, OMEGA, DE, PHI, RHODFS_SLAT, RHOUFS_SLAT, &
                   TAU_SLAT, RHO_BD, TAU_BD)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculates the venetian blind layer effective solar transmittance and reflectance.
          !
          ! METHODOLOGY EMPLOYED:
          !  Four surface Flat-Plate Model with slat transmittance
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S        ! slat spacing (any length units; same units as W)
                                       !    must be > 0
    REAL(r64), INTENT( IN) :: W        ! slat tip-to-tip width (any length units; same units as S)
                                       !   must be > 0
    REAL(r64), INTENT (IN) :: OMEGA    ! incident beam profile angle (radians)
                                       !   ltyVBHOR: +=above horizontal
                                       !   ltyVBVER: +=clockwise when viewed from above
    REAL(r64), INTENT (IN) :: DE       ! distance from front tip of any slat to shadow (caused by the adjacent slat) on
                                       !    the plane of the same slat de may be greater than the slat width, w
    REAL(r64), INTENT( IN) :: PHI      ! slat angle, radians (-PI/2 <= PHI <= PI/2)
                                       !   ltyVBHOR: + = front-side slat tip below horizontal
                                       !   ltyVBVER: + = front-side slat tip is counter-
                                       !                 clockwise from normal (viewed from above)
    REAL(r64), INTENT (IN) :: RHODFS_SLAT ! solar reflectance downward-facing slat surfaces (concave?)
    REAL(r64), INTENT (IN) :: RHOUFS_SLAT ! solar reflectance upward-facing slat surfaces (convex?)
    REAL(r64), INTENT (IN) :: TAU_SLAT    ! solar transmittance of slat
                                          !    Note: all solar slat properties - incident-to-diffuse
    REAL(r64), INTENT (OUT) :: RHO_BD     ! returned: solar beam-to-diffuse reflectance the venetian blind (front side)
    REAL(r64), INTENT (OUT) :: TAU_BD     ! returned: solar beam-to-diffuse transmittance of the venetian blind (front side)
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AF, CD    ! lengths of diagonal strings used in the four-surface model
    REAL(r64) :: F13, F14, F23, F24, F34, F43    ! Shape factors
    REAL(r64) :: Z3, Z4    ! diffuse source terms from surfaces 3 and 4 due to incident beam radiation
    REAL(r64) :: J3, J4    ! radiosity, surface i
    REAL(r64) :: B3, B4, C3, C4 ! temporaries
          ! flow

    AF = SQRT ((W*COS(PHI))**2   +   (S - W*SIN(PHI))**2)
    CD = SQRT ((W*COS(PHI))**2   +   (S + W*SIN(PHI))**2)
    !
    !  CHECK TO SEE WHICH SIDE OF SLAT IS SUNLIT
    IF((PHI + OMEGA) >= 0.d0) THEN   ! SUN SHINES ON TOP OF SLAT

        Z3 = TAU_SLAT*S/DE
        Z4 = RHOUFS_SLAT*S/DE
    !  PRINT *, PHI, OMEGA, DE, 'TOPLIT'

    ELSE            ! SUN SHINES ON BOTTOM OF SLAT
        Z3 = RHODFS_SLAT*S/DE
        Z4 = TAU_SLAT*S/DE
    !      PRINT *, PHI, OMEGA, DE, 'BOTLIT'
    ENDIF
    !
    !  CHECK TO SEE IF VENETIAN BLIND IS CLOSED
    IF( ABS(PHI - PiOvr2) < SMALL_ERROR ) THEN    !VENETIAN BLIND IS CLOSED

       ! CHECK TO SEE IF THERE ARE GAPS IN BETWEEN SLATS WHEN THE BLIND IS CLOSED
       IF(W < S) THEN        !YES, THERE ARE GAPS IN BETWEEN SLATS
            RHO_BD = (W/S)*RHOUFS_SLAT
            TAU_BD = (W/S)*TAU_SLAT
       ELSE                    ! NO, THERE ARE NO GAPS IN BETWEEN SLATS
            RHO_BD = RHOUFS_SLAT
            TAU_BD = TAU_SLAT
       ENDIF    ! END OF CHECK FOR GAPS IN BETWEEN SLATS

    ELSE        ! VENETIAN BLIND IS OPENED

        F13 = (S+W-CD)/(2.d0*S)
        F14 = (S+W-AF)/(2.d0*S)
        F23 = (S+W-AF)/(2.d0*S)
        F24 = (S+W-CD)/(2.d0*S)
        F34 = (CD+AF-2.d0*S)/(2.d0*W)
        F43 = (CD+AF-2.d0*S)/(2.d0*W)

        C3 = 1.d0 / (1.d0 - TAU_SLAT*F43)
        B3 = (RHODFS_SLAT*F34) / (1.d0 - TAU_SLAT*F43)
        C4 = 1.d0 / (1.d0 - TAU_SLAT*F34)
        B4 = (RHOUFS_SLAT*F43) / (1.d0 - TAU_SLAT*F34)
        J3 = (C3*Z3 + B3*C4*Z4) / (1.d0 - B3*B4)
        J4 = (C4*Z4 + B4*C3*Z3) / (1.d0 - B3*B4)

        RHO_BD = F13*J3 + F14*J4
        TAU_BD = F23*J3 + F24*J4

    ENDIF        ! END OF CHECK FOR CLOSED BLIND
    RETURN
END SUBROUTINE VB_SOL4

SUBROUTINE VB_SOL6(S, W, OMEGA, DE, PHI, RHODFS_SLAT, RHOUFS_SLAT, TAU_SLAT, &
                   RHO_BD, TAU_BD)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculates the venetian blind layer effective solar transmittance and reflectance.
          !
          ! METHODOLOGY EMPLOYED:
          !  six surface flat-slat model with slat transmittance. If you want the back
          !  side reflectance call the routine a second time with the same input data
          !  except negative the slat angle, PHI_DEG
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: S        ! slat spacing (any length units; same units as W)
                                       !    must be > 0
    REAL(r64), INTENT( IN) :: W        ! slat tip-to-tip width (any length units; same units as S)
                                       !   must be > 0
    REAL(r64), INTENT (IN) :: OMEGA    ! incident beam profile angle (radians)
                                       !   ltyVBHOR: +=above horizontal
                                       !   ltyVBVER: +=clockwise when viewed from above
    REAL(r64), INTENT (IN) :: DE       ! distance from front tip of any slat to shadow (caused by the adjacent slat) on
                                       !    the plane of the same slat DE may be greater than the slat width, w
    REAL(r64), INTENT( IN) :: PHI      ! slat angle, radians (-PI/2 <= PHI <= PI/2)
                                       !   ltyVBHOR: + = front-side slat tip below horizontal
                                       !   ltyVBVER: + = front-side slat tip is counter-
                                       !                 clockwise from normal (viewed from above)
    REAL(r64), INTENT (IN) :: RHODFS_SLAT ! solar reflectance downward-facing slat surfaces (concave)
    REAL(r64), INTENT (IN) :: RHOUFS_SLAT ! solar reflectance upward-facing slat surfaces (convex)
    REAL(r64), INTENT (IN) :: TAU_SLAT    ! solar transmittance of slat
                                          !    Note: all solar slat properties - incident-to-diffuse
    REAL(r64), INTENT (OUT) :: RHO_BD     ! returned: solar beam-to-diffuse reflectance the venetian blind (front side)
    REAL(r64), INTENT (OUT) :: TAU_BD     ! returned: solar beam-to-diffuse transmittance of the venetian blind (front side)
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: N = 4

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AB, AE, AF, BC, BD, BF, CD, CE, EF        ! lengths of slat segments and diagonal strings
                                                           !  used in the six-surface model
    REAL(r64) :: F13, F14, F23, F24, F34, F36, F15, F16    ! shape factors
    REAL(r64) :: F43, F45, F54, F56, F63, F65, F25, F26
    REAL(r64) :: Z3, Z4            ! diffuse source terms from surfaces 3 and 4 due to incident beam radiation
    REAL(r64) :: J3, J4, J5, J6    ! radiosity, surface i
    REAL(r64) :: A( N, N+2)    ! coefficients of the radiosity equations matrix
    REAL(r64) :: XSOL( N)      ! solution vector (obtained after solving the radiosity equations matrix)
          ! flow

    !  CHECK TO SEE WHICH SIDE OF SLAT IS SUNLIT
    IF((PHI + OMEGA) >= 0.0d0) THEN   ! SUN SHINES ON TOP OF SLAT
        Z3 = TAU_SLAT*S/DE
        Z4 = RHOUFS_SLAT*S/DE
    !      PRINT *, PHI, OMEGA, DE, 'TOPLIT'

    ELSE            ! SUN SHINES ON BOTTOM OF SLAT
        Z3 = RHODFS_SLAT*S/DE
        Z4 = TAU_SLAT*S/DE
    !      PRINT *, PHI, OMEGA, DE, 'BOTLIT'
    ENDIF

    !  CHECK TO SEE IF VENETIAN BLIND IS CLOSED
    IF( ABS(PHI - PiOvr2) < SMALL_ERROR ) THEN  !VENETIAN BLIND IS CLOSED

       ! CHECK TO SEE IF THERE ARE GAPS IN BETWEEN SLATS WHEN THE BLIND IS CLOSED
       IF(W < S) THEN        !YES, THERE ARE GAPS IN BETWEEN SLATS
            RHO_BD = (W/S)*RHOUFS_SLAT
            TAU_BD = (W/S)*TAU_SLAT
       ELSE                    ! NO, THERE ARE NO GAPS IN BETWEEN SLATS
            RHO_BD = RHOUFS_SLAT
            TAU_BD = TAU_SLAT
       ENDIF        ! END OF CHECK FOR GAPS IN BETWEEN SLATS

    ELSE                                            !VENETIAN BLIND IS OPENED
        AB = DE
        AF = SQRT ((W*COS(PHI))**2   +   (S - W*SIN(PHI))**2)
        BC = W - AB
        EF = BC
        BD = SQRT ((DE*COS(PHI))**2   +   (S + DE*SIN(PHI))**2)
        BF = SQRT ((EF*COS(PHI))**2   +   (S - EF*SIN(PHI))**2)
        CD = SQRT ((W*COS(PHI))**2    +   (S + W*SIN(PHI))**2)
        CE = SQRT ((EF*COS(PHI))**2   +   (S + EF*SIN(PHI))**2)
        AE = SQRT ((DE*COS(PHI))**2   +   (S - DE*SIN(PHI))**2)

        F13 = (S + AB - BD) / (2.d0*S)
        F14 = (S + DE - AE) / (2.d0*S)
        F15 = (W + BD - (AB + CD)) / (2.d0*S)
        F16 = (W + AE - (AF + DE)) / (2.d0*S)
        F23 = (W + BF - (BC + AF)) / (2.d0*S)
        F24 = (W + CE - (CD + EF)) / (2.d0*S)
        F25 = (S + BC - BF) / (2.d0*S)
        F26 = (S + EF - CE) / (2.d0*S)
        F34 = (AE + BD - 2.d0*S) / (2.d0*AB)
        F36 = (AF + S - (AE + BF)) / (2.d0*AB)
        F43 = (AE + BD - 2.d0*S) / (2.d0*DE)
        F45 = (CD + S - (BD + CE)) / (2.d0*DE)
        F54 = (CD + S - (BD + CE)) / (2.d0*BC)
        F56 = (CE + BF - 2.d0*S) / (2.d0*BC)
        F63 = (AF + S - (AE + BF)) / (2.d0*EF)
        F65 = (BF + CE - 2.d0*S) / (2.d0*EF)

        ! POPULATE THE COEFFICIENTS OF THE RADIOSITY MATRIX

        A(1,1) = 1.d0 - TAU_SLAT*F43
        A(1,2) = -RHODFS_SLAT*F34
        A(1,3) = -TAU_SLAT*F45
        A(1,4) = -RHODFS_SLAT*F36
        A(1,5) = Z3
        A(2,1) = -RHOUFS_SLAT*F43
        A(2,2) = 1.d0 - TAU_SLAT*F34
        A(2,3) = -RHOUFS_SLAT*F45
        A(2,4) = -TAU_SLAT*F36
        A(2,5) = Z4
        A(3,1) = -TAU_SLAT*F63
        A(3,2) = -RHODFS_SLAT*F54
        A(3,3) = 1.d0 - TAU_SLAT*F65
        A(3,4) = -RHODFS_SLAT*F56
        A(3,5) = 0.0d0
        A(4,1) = -RHOUFS_SLAT*F63
        A(4,2) = -TAU_SLAT*F54
        A(4,3) = -RHOUFS_SLAT*F65
        A(4,4) = 1.0d0 - TAU_SLAT*F56
        A(4,5) = 0.0d0

        CALL SOLMATS(N,A,XSOL)

        J3 = XSOL(1)
        J4 = XSOL(2)
        J5 = XSOL(3)
        J6 = XSOL(4)

        RHO_BD = F13*J3 + F14*J4 + F15*J5 + F16*J6
        TAU_BD = F23*J3 + F24*J4 + F25*J5 + F26*J6
    ENDIF            ! END OF CHECK FOR CLOSED BLIND
    RETURN
END SUBROUTINE VB_SOL6

SUBROUTINE SOLMATS(N, A, XSOL)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Matrix solver.
          !
          ! METHODOLOGY EMPLOYED:
          !  Solves matrix by the elimination method supplemented by a search for the
          !  largest pivotal element at each stage
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER,      INTENT(IN) :: N            ! # of active rows in A
    REAL(r64), INTENT(INOUT) ::  A(:,:)      ! matrix, minimum required dimensions: A( N, N+2)
                                             !   modified in place
    REAL(r64),   INTENT(OUT) ::  XSOL(:)     ! returned: solution vector, min req dimension: XSOL( N)
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: CMAX, TEMP, C, Y, D
    INTEGER   :: NM1, NP1, NP2, I, J, L, LP, NOS, NI, NJ

    NM1=N-1
    NP1=N+1
    NP2=N+2

    DO I=1,N
        A(I,NP2)=0.0d0
        ! DO 1 J=1,NP1    ! TODO ?
    END DO

    DO I=1,N
        DO J=1,NP1
            A(I,NP2)=A(I,NP2)+A(I,J)
        END DO
    END DO

    DO L=1,N-1
        CMAX=A(L,L)
        LP=L+1
        NOS=L

        DO I=LP,N
            IF (ABS(CMAX).LT.ABS(A(I,L))) THEN
                CMAX=A(I,L)
                NOS=I
            ENDIF
        END DO

        ! Swap rows
        IF (NOS.NE.L) THEN
            DO J=1,NP2
                TEMP=A(L,J)
                A(L,J)=A(NOS,J)
                A(NOS,J)=TEMP
            END DO
        END IF

        DO I=LP,N
            C=0.0d0
            Y=-A(I,L)/A(L,L)
            DO J=L,NP2
                A(I,J)=A(I,J)+Y*A(L,J)
            END DO
            DO J=L,NP1
                C=C+A(I,J)
            END DO
        END DO
    END DO

    ! back-substitute
    XSOL(N)=A(N,NP1)/A(N,N)
    DO I=1,NM1
        NI=N-I
        D=0.0d0
        DO J=1,I
            NJ=N+1-J
            D=D+A(NI,NJ)*XSOL(NJ)
        END DO
        XSOL(NI)=(A(NI,NP1)-D)/A(NI,NI)
    END DO
    RETURN
END SUBROUTINE SOLMATS

LOGICAL FUNCTION ASHWAT_Thermal(FS, TIN, TOUT, HCIN, HCOUT, &
                                TRMOUT, TRMIN, ISOL, SOURCE, TOL,     &
                                QOCF, QOCFRoom, T, Q, JF, JB, HC,     &
                                UCG, SHGC, HCInFlag)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT (University of Waterloo, Mechanical Engineering)
          !                      Chip Barnaby (WrightSoft)
          !
          !       DATE WRITTEN   LATEST MODIFICATIONS, February 2008
          !
          !       MODIFIED       Bereket Nigusse, June 2013
          !                      added standard 155099 inside convection
          !                      coefficient calculation for U-Factor
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     Subroutine to calculate the glazing temperatures of the
          !     various elements of a window/shade array while solving an energy
          !     balance which accounts for absorbed solar radiation, indoor-
          !     outdoor temperature difference, any combination of hemispherical
          !     IR optical properties of the various glazings/shading layers.
          !     Mean radiant temperatures can differ from air temperature on
          !     both the indoor and outdoor sides.
          !     It is also possible to allow air-flow between the two layers
          !     adjacent to the indoor side and/or the two layers adjacent the
          !     outdoor side. U-factor and SHGC calculations are also included (optional)

          ! METHODOLOGY EMPLOYED:
          ! Uses the net radiation method developed for ASHWAT fenestration
          ! model by John Wright, the University of WaterLoo

          ! REFERENCES:
          !  ASHRAE RP-1311

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSTY), INTENT( IN) :: FS             ! fenestration system
                                                !   FS%NL determines # of layers modelled
    REAL(r64), INTENT( IN) :: TIN, TOUT         ! indoor / outdoor air temperature, K
    REAL(r64), INTENT( IN) :: HCIN, HCOUT       ! indoor / outdoor convective heat transfer
                                                !   coefficient, W/m2K
    REAL(r64), INTENT( IN) :: TRMIN, TRMOUT     ! indoor / outdoor mean radiant temp, K
    REAL(r64), INTENT( IN) :: ISOL              ! total incident solar, W/m2 (values used for SOURCE derivation)
                                                !   = outside direct + outside diffuse + inside diffuse
    REAL(r64), INTENT( IN) :: SOURCE( FS%NL+1)  ! absorbed solar by layer,  W/m2
    REAL(r64), INTENT( IN) :: TOL               ! convergence tolerance, usually
                                                !   0.001 (good) or 0.0001 (tight)
    REAL(r64), INTENT( OUT) :: QOCF( FS%NL)     ! returned: heat flux to layer i from gaps i-1 and i
                                                !   due to open channel flow, W/m2
    REAL(r64), INTENT( OUT) :: QOCFRoom         ! returned: open channel heat gain to room, W/m2
    REAL(r64), INTENT( OUT) :: T( FS%NL)        ! returned: layer temperatures, 1=outside-most layer, K
    REAL(r64), INTENT( OUT) :: Q(0:)            ! returned: heat flux at ith gap (betw layers i and i+1), W/m2
                                                !   + = heat flow indoor to outdoor
    REAL(r64), INTENT( OUT) :: JF( FS%NL+1)     ! returned: front (outside facing) radiosity of surfaces, W/m2
                                                !   JF( NL+1) = room radiosity
    REAL(r64), INTENT( OUT) :: JB(0:FS%NL)      ! returned: back (inside facing) radiosity, W/m2
                                                !   JB( 0) = outside environment radiosity
    REAL(r64), INTENT( OUT) :: HC(0:FS%NL)      ! returned: gap convective heat transfer coefficient, W/m2K
                                                !   0=outside, 1=betw layer 1-2, ..., NL=inside
    REAL(r64), INTENT( OUT) :: UCG              ! returned: center-glass U-factor, W/m2-K
    REAL(r64), INTENT( OUT) :: SHGC             ! returned: center-glass SHGC (Solar Heat Gain Coefficient)
    LOGICAL, OPTIONAL,INTENT( IN)  :: HCInFlag  ! If true uses ISO Std 150099 routine for HCIn calc

          ! FUNCTION PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: Height = 1.0d0      ! Window height (m) for standard ratings calculation
    CHARACTER(len=MaxNameLength), PARAMETER:: RoutineName= 'ASHWAT_Thermal: '
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: ALPHA, HCOCFout
    REAL(r64) :: A( 3*FS%NL+2, 3*FS%NL+4), XSOL( 3*FS%NL+2)
    REAL(r64) :: MAXERR
    REAL(r64) :: TNEW( FS%NL)    ! latest estimate of layer temperatures, K
    REAL(r64) :: EB( 0:FS%NL+1)  ! black emissive power by layer, W/m2
                                 !   EB( 0) = outdoor environment, EB( NL+1) = indoor environment
    REAL(r64) :: HHAT(0:FS%NL)   !  convective heat transfer coefficient (W/m2.K4)
                                 !   based on EB, NOT temperature difference
    REAL(r64) :: RHOF_ROOM, TAU_ROOM, EPSF_ROOM   ! effective longwave room-side properties
    REAL(r64) :: RHOB_OUT,  TAU_OUT,  EPSB_OUT    ! effective longwave outdoor environment properties
    REAL(r64) :: QNET( FS%NL)    ! checksum - net heat flux to a layer - should be zero - not needed
    INTEGER   :: ADIM            ! dimension of the A matrix
    INTEGER   :: CONVRG
    INTEGER   :: NL, I, J, L, ITRY
    INTEGER   :: hin_scheme      ! flags different schemes for indoor convection coefficients
    INTEGER ISDL(0:FS%NL+1) !  Flag to mark diathermanous layers, 0=opaque
    INTEGER NDLIAR          !  Number of Diathermanous Layers In A Row (i.e., consecutive)
    INTEGER IB, IE          !  Counter begin and end limits
    INTEGER IDV             !  Integer dummy variable, general utility
    INTEGER IM_ON           !  Turns on calculation of Indices of Merit if IM_ON=1
    REAL(r64) :: QOCF_F(FS%NL)     !  heat flux to outdoor-facing surface of layer i, from gap i-1,
                                   !   due to open channel flow, W/m2
    REAL(r64) :: QOCF_B(FS%NL)     !  heat flux to indoor-facing surface of layer i, from gap i,
                                 !   due to open channel flow, W/m2
    REAL(r64) :: Rvalue          !  R-value in IP units [hr.ft2.F/BTU]
    REAL(r64) :: TAE_IN, TAE_OUT !  Indoor and outdoor effective ambient temperatures [K]
    REAL(r64) :: HR(0:FS%NL)           !  Radiant heat transfer coefficient [W/m2K]
    REAL(r64) :: HJR(FS%NL),HJC(FS%NL) ! radiative and convective jump heat transfer coefficients
    REAL(r64) :: FHR_OUT, FHR_IN ! hre/(hre+hce) fraction radiant h, outdoor or indoor, used for TAE
    REAL(r64) :: Q_IN            ! net gain to the room [W/m2], including transmitted solar
    REAL(r64) :: RHOF(0:FS%NL+1) ! longwave reflectance, front    !  these variables help simplify
    REAL(r64) :: RHOB(0:FS%NL+1) ! longwave reflectance, back     !  the code because it is useful to
    REAL(r64) :: EPSF(0:FS%NL+1) ! longwave emisivity,   front    !  increase the scope of the arrays
    REAL(r64) :: EPSB(0:FS%NL+1) ! longwave emisivity,   back     !  to include indoor and outdoor
    REAL(r64) :: TAU(0:FS%NL+1)     ! longwave transmittance         !  nodes - more general
    REAL(r64) :: RTOT            ! total resistance from TAE_OUT to TAE_IN [m2K/W]
    REAL(r64) :: HC2D(6, 6)      ! convective heat transfer coefficients between layers i and j
    REAL(r64) :: HR2D(6, 6)      ! radiant heat transfer coefficients between layers i and j
    REAL(r64) :: HCIout(6), HRIout(6)  ! convective and radiant heat transfer coefficients between
                                       ! layer i and outdoor air or mean radiant temperature, resp.
    REAL(r64) :: HCIin(6), HRIin(6)    ! convective and radiant heat transfer coefficients between
                                       ! layer i and indoor air or mean radiant temperature, resp.
    REAL(r64) :: HCinout, HRinout      ! convective and radiant heat transfer coefficients between
                                       ! indoor and outdoor air or mean radiant temperatures
                                       ! (almost always zero)
    !  Indoor side convection coefficients - used for Open Channel Flow on indoor side
    REAL(r64) :: HFS                   ! nominal height of fen system (assumed 1 m)
    REAL(r64) :: TOC_EFF               ! effective thickness of open channel, m
    Real(r64) :: ConvF                 ! convection factor: accounts for enhanced convection
                                       !   for e.g. VB adjacent to open channel
    Real(r64) :: HC_GA                 ! convection - glass to air
    Real(r64) :: HC_SA                 ! convection - shade (both sides) to air
    Real(r64) :: HC_GS                 ! convection - glass to shade (one side)
    REAL(r64) :: TINdv, TOUTdv         ! dummy variables used
    REAL(r64) :: TRMINdv, TRMOUTdv     ! for boundary conditions in calculating
    REAL(r64) :: SOURCEdv( FS%NL+1)    ! indices of merit
    REAL(r64) :: SUMERR                ! error summation used to check validity of code/model
    REAL(r64) :: QGAIN                 ! total gain to conditioned space [[W/m2]
    REAL(r64) :: SaveHCNLm             ! place to save HC(NL-1) - two resistance networks differ
    REAL(r64) :: SaveHCNL              ! place to save HC(NL)   - two resistance networks differ
                                       ! in their definitions of these heat transfer coefficients
    LOGICAL DoPrint                    ! set true to print debugging info
        ! Flow

    ASHWAT_Thermal = .FALSE.           ! init to failure
    NL = FS%NL                         ! working copy
    IF (NL < 1) RETURN

    HCOCFout = HCOUT                   ! outdoor side

    IM_ON = 1

    HHAT=0.0d0
    HC = 0.0d0
    HR = 0.0d0
    T=0.0d0
    TNEW=0.0d0
    EB=0.0d0
    JF=0.0d0
    JB=0.0d0
    Q=0.0d0
    QOCF_F=0.0d0
    QOCF_B=0.0d0
    QOCF = 0.0d0
    QOCFRoom = 0.0d0
    QNET=0.0d0
    QGAIN=0.0d0
    TAU  = 0.0d0
    RHOF = 0.0d0
    RHOB = 0.0d0
    EPSF = 0.0d0
    EPSB = 0.0d0
    HC_GA = 0.0d0
    HC_SA = 0.0d0
    HC_GS = 0.0d0

    ITRY=0

    EB( 0)    = StefanBoltzmann * TOUT**4
    EB( NL+1) = StefanBoltzmann * TIN**4

    ADIM=3*NL + 2    ! DIMENSION OF A-MATRIX

    ! organize longwave radiant properties - book-keeping

    TAU_ROOM  = 0.0d0                      ! must always be zero
    RHOF_ROOM = 0.0d0                      ! almost always zero
    EPSF_ROOM = 1.d0- TAU_ROOM - RHOF_ROOM ! almost always unity
    RHOF(NL+1) = RHOF_ROOM
    EPSF(NL+1) = EPSF_ROOM
    TAU (NL+1) = TAU_ROOM

    DO I=1,NL
        EPSF(I) = FS%L(I)%LWP_EL%EPSLF
        EPSB(I) = FS%L(I)%LWP_EL%EPSLB
        TAU (I) = FS%L(I)%LWP_EL%TAUL
        RHOF(I) = 1.d0 - FS%L(I)%LWP_EL%EPSLF - FS%L(I)%LWP_EL%TAUL
        RHOB(I) = 1.d0 - FS%L(I)%LWP_EL%EPSLB - FS%L(I)%LWP_EL%TAUL
    END DO

    TAU_OUT =  0.0d0                     ! must always be zero
    RHOB_OUT = 0.0d0                     ! DON'T CHANGE
    EPSB_OUT = 1.d0 - TAU_OUT - RHOB_OUT ! should always be unity
    TAU (0)  = TAU_OUT
    EPSB(0)  = EPSB_OUT
    RHOB(0)  = RHOB_OUT

    ! Later could add RHOF_ROOM to the parameter list
    ! Relaxation needed to keep solver stable if OCF is present

    ALPHA=1.0d0
    IF (NL.GE.2) THEN
      IF (FS%G(NL-1)%GTYPE .EQ. gtyOPENin)  ALPHA=0.5d0
      IF (FS%G( 1)  %GTYPE .EQ. gtyOPENout) ALPHA=0.1d0
    ENDIF


    !   FIRST ESTIMATE OF GLAZING TEMPERATURES AND BLACK EMISSIVE POWERS
    DO I=1,NL
        T(I) = TOUT + (REAL(I,r64))/(REAL(NL+1,r64)) * (TIN-TOUT)
        EB(I)= StefanBoltzmann * T(I)**4
    END DO

    CONVRG=0

    !  Start the solver
    !  ITERATION RE-ENTRY

    DO ITRY=1,100


     !  CALCULATE GAS LAYER CONVECTIVE HEAT TRANSFER COEFFICIENTS

      hin_scheme = 3    !  different schemes for calculating convection
                        !  coefficients glass-to-air and shade-to-air
                        !  if open channel air flow is allowed
                        !  see the corresponding subroutines for detail
                        !  = 1 gives dependence of height, spacing, delta-T
                        !  = 2 gives dependence of spacing, delta-T but
                        !    returns unrealistic values for large spacing
                        !  = 3 glass-shade spacing dependence only on HCIN
                        !  = negative, applies HCIN without adjusting for
                        !    temperature, height, spacing, slat angle
                        !  Recommended -> hin_scheme=3 for use with HBX,
                        !              simplicity, right trends wrt spacing

      ! start by assuming no open channel flow on indoor side

        HC(NL)=HCIN            !  default - HC(NL) supplied by calling routine
                               !  use this for HBX
      ! or
      ! trigger calculation of HC(NL) using ASHRAE correlation
      !  HC(NL) = HIC_ASHRAE(1.0d0, T(NL), TIN)  ! h - flat plate
      !
      ! Add by BAN June 2013 for standard ratings U-value and SHGC calc only
        IF (PRESENT(HCInFlag)) THEN
            IF ( HCInFlag) HC(NL) = HCInWindowStandardRatings(Height, T(NL), TIN)
        ENDIF
        HC(0) = HCOUT    ! HC(0) supplied by calling routine as HCOUT

        ! Check for open channels -  only possible with at least two layers
        IF (NL >= 2) THEN
          DO I=1,NL-1  ! Scan gaps between layers

            ! DEAL WITH INDOOR OPEN CHANNEL FLOW HERE
            IF ((I .EQ. NL-1).AND.(FS%G( I)%GTYPE .EQ. gtyOPENin)) THEN

                !TOC_EFF = FS%G( I)%TAS_EFF / 1000.    ! effective thickness of OC gap, m
                TOC_EFF = FS%G( I)%TAS_EFF             ! effective thickness of OC gap, m Modified by BAN May 9, 2013
                HFS = 1.                               ! nominal height of system (m)

                ! convection - glass to air
                CALL GLtoAMB( TOC_EFF, HFS, T( NL-1), TIN, HCIN, HC_GA, hin_scheme)
                ! CALL GLtoAMB( 1.0, HFS, T( NL-1), TIN, HCIN, HC_GA, hin_scheme)
                            !   ^ VERY WIDE GAP

                ! convection - shade (both sides) to air
                ConvF = ConvectionFactor( FS%L( I+1))
                HC_SA = ConvF * SLtoAMB( TOC_EFF, HFS, T(NL), TIN, HCIN, hin_scheme)
                ! HC_SA = ConvF * SLtoAMB( 1.0, HFS, T(NL), TIN, HCIN, hin_scheme)
                               !  ^ VERY WIDE GAP

                ! convection - glass to shade (one side)
                CALL  SLtoGL( TOC_EFF, T(NL), T(NL-1), HC_GS, 1)
                ! CALL  SLtoGL( 1.0, T(NL), T(NL-1), HC_GS, 2)   !  REMOVE LATER
                           !  ^ VERY WIDE GAP, should return near zero
                           !  Don't use hin_scheme as last parameter - set manually
                           !  1 = Conduction, 2 = slight Ra penalty
                           !  Set negative for default HC_GS=0
                           !  Recommended:  2
                HC(NL-1) = HC_GS
                HC(NL  ) = HCIN * ConvF
                QOCF_B(NL-1) = (TIN - T(NL-1))*HC_GA
                QOCF_F(NL  ) = (TIN - T(NL  ))*(HC_SA - HC( NL))
                QOCFRoom = - QOCF_B( NL-1) - QOCF_F( NL)
                ! end of gap open to indoor side

            ELSE IF ((I .EQ. 1).AND.(FS%G( I)%GTYPE .EQ. gtyOPENout)) THEN
                ! outdoor open channel
                QOCF_B(1) = (TOUT - T(1))*HCOCFout
                QOCF_F(2) = (TOUT - T(2))*HCOCFout
                HC(1)=0.0d0
                HC(0)=HCOCFout
            ELSE
                ! normal gap
                HC(I) = HConvGap( FS%G(I), T(I), T(I+1))
            ENDIF
          END DO  !  end scan through gaps

          ! total OCF gain to each layer
          QOCF = QOCF_F + QOCF_B

        ENDIF  !  end IF (NL .GE. 2)


        !  CONVERT TEMPERATURE POTENTIAL CONVECTIVE COEFFICIENTS to
        !  BLACK EMISSIVE POWER POTENTIAL CONVECTIVE COEFFICIENTS

        HHAT(0) = HC(0)*(1.d0/StefanBoltzmann)/( ((TOUT**2+T(1)**2)) *  ((TOUT+T(1))) )

        DO I=1,NL-1                        ! Scan the cavities
            HHAT(I) = HC(I)*(1.d0/StefanBoltzmann)/( ((T(I)**2+T(I+1)**2)) * ((T(I)+T(I+1))) )
        END DO

        HHAT(NL) = HC(NL)*(1.d0/StefanBoltzmann)/( ((T(NL)**2+TIN**2)) * ((T(NL)+TIN)) )


        !  SET UP MATRIX
        XSOL = 0.0d0
        A = 0.0d0

        L=1
        A(L,1)=1.d0
        A(L,2)= -1.d0 * RHOB(0)            !  -1.0 * RHOB_OUT
        A(L,ADIM+1)= EPSB_OUT * StefanBoltzmann * TRMOUT**4

        DO I=1,NL
            L=3*I-1
            A(L,3*I-2) = RHOF( I)
            A(L,3*I-1) = -1.0d0
            A(L,3*I  ) = EPSF(I)          !  LWP( I)%EPSLF
            A(L,3*I+2) = TAU (I)          !  LWP( I)%TAUL
            A(L,ADIM+1) = 0.0d0

            L=3*I
            IF (NL == 1) THEN
                A(L,1)= 1.0d0        ! Single layer
                A(L,2)=-1.0d0
                A(L,3)=-1.0d0*(HHAT(0)+HHAT(1))
                A(L,4)=-1.0d0
                A(L,5)= 1.0d0
                A(L,ADIM+1)= -1.0d0*(HHAT(0)*EB( 0) + HHAT(1)*EB(2) + &
                                           SOURCE(1)+QOCF(1)  )
            ELSE IF (I == 1 ) THEN
                A(L,1)=  1.0d0           !  Outdoor layer
                A(L,2)= -1.0d0
                A(L,3)= -1.0d0*(HHAT(0) + HHAT(1))
                A(L,4)= -1.0d0
                A(L,5)=  1.0d0
                A(L,6)=  HHAT(1)
                A(L,ADIM+1)=-1.0d0*( HHAT(0)*EB( 0) + SOURCE(1) + QOCF(1) )
            ELSE IF (I == NL) THEN
                A(L,3*NL-3)= HHAT(NL-1) ! Indoor layer
                A(L,3*NL-2)= 1.0d0
                A(L,3*NL-1)=-1.0d0
                A(L,3*NL  )=-1.0d0*(HHAT(NL)+HHAT(NL-1))
                A(L,3*NL+1)=-1.0d0
                A(L,3*NL+2)= 1.0d0
                A(L,ADIM+1)=-1.0d0*( HHAT(NL)*EB( NL+1) + SOURCE(NL)+ QOCF(NL) )
            ELSE
                A(L,3*I-3)  = HHAT(I-1)
                A(L,3*I-2)  = 1.0d0
                A(L,3*I-1)  =-1.0d0
                A(L,3*I  )  =-1.0d0*(HHAT(I)+HHAT(I-1))
                A(L,3*I+1)  =-1.0d0
                A(L,3*I+2)  = 1.0d0
                A(L,3*I+3)  = HHAT(I)
                A(L,ADIM+1) =-1.0d0*(SOURCE(I)+QOCF(I))
            END IF
            L=3*I+1
            A(L,3*I-2) = TAU(I)              !   LWP( I)%TAUL
            A(L,3*I  ) = EPSB(I)             !   LWP( I)%EPSLB
            A(L,3*I+1) = -1.0d0
            A(L,3*I+2) = RHOB(I)
            A(L,ADIM+1)   = 0.0d0
        END DO

        L=3*NL+2
        A(L,3*NL+1)= - 1.d0 * RHOF(NL+1)      !   - 1.0 * RHOF_ROOM
        A(L,3*NL+2)= 1.0d0
        A(L,ADIM+1)= EPSF_ROOM * StefanBoltzmann * TRMIN**4

        !  SOLVE MATRIX

        !  Call SOLMATS for single precision matrix solution
        CALL SOLMATS( ADIM, A, XSOL)

        !  UNPACK SOLUTION VECTOR AND RECORD LARGEST TEMPERATURE CHANGE
        JB(0)=XSOL(1)

        MAXERR=0.0d0
        DO I=1,NL
            J=3*I-1
            JF(I)=XSOL(J)
            J=J+1
            EB(I)=MAX( 1.d0, XSOL(J))        ! prevent impossible temps
            TNEW(I)=((EB(I)/StefanBoltzmann)**0.25d0)
            J=J+1
            JB(I)=XSOL(J)
            MAXERR = MAX( MAXERR, ABS( TNEW(I)-T(I))/TNEW(I))
        END DO

        JF(NL+1)=XSOL(ADIM)

        !  CALCULATE HEAT FLUX AT EACH GAP, Q
        DO I=0,NL      ! Loop gaps (including inside and outside
            Q(I)=JF(I+1)-JB(I) +  HHAT(I)*(EB(I+1)-EB(I))
        END DO

        !  A CHECK - NET HEAT FLUX INTO ANY LAYER, AT STEADY-STATE,
        !  SHOULD BE ZERO
        DO I=1,NL
          QNET(I)=SOURCE(I) + QOCF(I) +  Q(I) - Q(I-1)
        END DO

        !  UPDATE GLAZING TEMPERATURES AND BLACK EMISSIVE POWERS
        DO I=1,NL
            T(I) = T(I) + ALPHA * (TNEW(I)-T(I))
            EB(I)= StefanBoltzmann * T(I)**4
        END DO

        !  CHECK FOR CONVERGENCE
        IF (CONVRG > 0) EXIT
        IF (MAXERR < TOL) CONVRG=CONVRG+1

    END DO        ! main iteration

    IF (CONVRG == 0) THEN
        CALL ShowSevereError(RoutineName//'Net radiation analysis did not converge for '//TRIM( FS%Name))
        CALL ShowContinueError('...Maximum error is = '//TRIM(TrimSigDigits(MAXERR,6)))
        CALL ShowContinueError('...Convergence tolerance is = '//TRIM(TrimSigDigits(TOL,6)))
    END IF

    !  NOTE:  HC_SA, HC_GA and HC_SG are only available if there is
    !         an open channel on the indoor side and the calculation of
    !         these coefficients was triggered earlier
    QGAIN = SOURCE(NL+1) + HC(NL)*(T(NL)-TIN) + JB(NL) - JF(NL+1)
    ! Modified by BAN May 3, 2013 to avoid zero layer index
    IF ( NL .GE. 2 ) THEN
      IF (FS%G(NL-1)%GTYPE .EQ. gtyOPENin) THEN
        QGAIN = SOURCE(NL+1) + (HC_SA/2.0d0)*(T(NL)-TIN) + JB(NL) - JF(NL+1)
        QGAIN = QGAIN  +  HC_GA      * (T(NL-1) - TIN)              &
                       + (HC_SA/2.0d0) * (T(NL  ) - TIN)
      ENDIF
    END IF


    ASHWAT_Thermal = .TRUE.

    ! New code follows from here - for calculating Ucg and SHGC
    ! NOTE: This code can be bypassed if
    !       indices of merit are not needed


  IF (IM_ON .NE. 1) RETURN

!  Initialize various things
   HR      = 0.0d0
   HJC =     0.0d0
   HJR =     0.0d0
   TAE_OUT = 0.0d0
   TAE_IN =  0.0d0
   FHR_OUT = 0.0d0
   FHR_IN =  0.0d0
   Q_IN=0.0d0
   RTOT=0.0d0
   UCG=0.0d0
   SHGC=0.0d0
   Rvalue = 0.0d0
   HC2D=0.0d0
   HR2D=0.0d0
   HCIout=0.0d0
   HRIout=0.0d0
   HCIin=0.0d0
   HRIin=0.0d0
   HCinout=0.0d0
   HRinout=0.0d0
   TNEW=0.0d0
   TINdv=0.0d0
   TOUTdv=0.0d0
   TRMINdv=0.0d0
   TRMOUTdv=0.0d0
   SOURCEdv=0.0d0


   !  Identify the diathermanous layers
   ISDL = 0
   DO I=1,NL
     IF(FS%L( I)%LWP_EL%TAUL .GT. 0.001d0) ISDL(I)=1  ! layer is diathermanous
                                            ! of tau_lw > 0.001 (ie 0.1%)
                                            ! Note:  ISDL(0) and ISDL(NL+1)
                                            !        must both be zero
   END DO  ! end loop to calculate ISDL(i)

   !  determine the largest number of consecutive diathermanous layers, NDLIAR
   !                   i.e., the number of diathermanous layers in a row
   NDLIAR = 0
   IDV = 0
   DO I=1,NL
     IF(ISDL(I) .EQ. 1) THEN
       IDV=IDV+1
     ELSE
       IDV=0
     ENDIF
     IF(IDV.GT. NDLIAR) NDLIAR=IDV
   END DO  ! end loop to calculate NDLIAR


    IF(NDLIAR .GT. 1)RETURN ! cannot handle two (or more) consecutive
                           ! diathermanous layers, U/SHGC calculation
                           ! will be skipped entirely
                           ! CHANGE TO (NDLIAR .GT. 2) ONCE
                           ! SUBROUTINE DL2_RES IS AVAILABLE


    !   calculate radiant heat transfer coefficents between adjacent opaque
    !   layers
    DO I=0,NL  ! scan through all gaps - including indoor/outdoor
        IF ((ISDL(I) .EQ. 0) .AND. (ISDL(I+1) .EQ. 0)) THEN
            IF (I .EQ.0) THEN     !  outdoor side
                HR(I) = HRadPar( T(1), TRMOUT, EPSF(1), EPSB(0))
            ELSE IF (I .EQ. NL) THEN  !  indoor side
                HR(I) = HRadPar( T(NL), TRMIN,EPSF(NL+1),EPSB(NL))
            ELSE                !  cavities
                HR(I) = HRadPar( T(I), T(I+1), EPSF(I+1), EPSB(I))
            ENDIF
        ENDIF
    END DO   ! end loop through gaps


    !   calculate radiant heat transfer coefficents at single diathermanous
    !   layers,three coefficients in each case

           DO I=0,NL-1  ! scan through all layers - look for single DL
                        ! layers between two opaque layers
             IF((ISDL(I) .EQ. 0) .AND. (ISDL(I+1) .EQ. 1)    &
                                 .AND. (ISDL(I+2) .EQ. 0)) THEN
               IF (I .EQ. 0) THEN    !  outdoor layer is diathermanous
                IF(NL .EQ. 1) THEN
                 CALL DL_RES_r2 (TRMOUT, T(1), TRMIN,                   &
                              RHOB(0),RHOF(1),RHOB(1),TAU(1),RHOF(2),&
                                 HJR(1), HR(0), HR(1))
                ELSE
                  CALL DL_RES_r2 (TRMOUT, T(1), T(2),                      &
                              RHOB(0),RHOF(1),RHOB(1),TAU(1),RHOF(2),&
                                 HJR(1), HR(0), HR(1))
                ENDIF
               ELSE   !  with IF (I .EQ. 0)   i.e., i != 0
                 IF(I .EQ. NL-1) THEN  !  indoor layer is diathermanous
                 CALL DL_RES_r2 (T(NL-1), T(NL), TRMIN,                       &
                              RHOB(NL-1),RHOF(NL),RHOB(NL),TAU(NL),RHOF(NL+1),&
                                 HJR(NL), HR(NL-1), HR(NL))
                 ELSE   ! some intermediate layer is diathermanous
                   CALL DL_RES_r2 (T(I), T(I+1), T(I+2),                    &
                              RHOB(I),RHOF(I+1),RHOB(I+1),TAU(I+1),RHOF(I+2),&
                                 HJR(I+1), HR(I), HR(I+1))
                 ENDIF   !   end of IF/ELSE (I .EQ. NL-1)
               ENDIF   !  end of IF/ELSE (I .EQ. 0)
             ENDIF    !  end of IF(ISDL(I) .EQ. 0) .AND. .....
           END DO   !   end of scan through all layers


        !   calculate radiant heat transfer coefficents at double diathermanous
        !   layers,six coefficients in each case
        !   THIS SECTION NOT ACTIVE YET

        IF (NL .GE. 2) THEN
           DO I=0,NL-2  ! scan through all layers - look for double DL
                        ! layers between two opaque layers
             IF((ISDL(I) .EQ. 0) .AND. (ISDL(I+1) .EQ. 1)    &
                                 .AND. (ISDL(I+2) .EQ. 1)    &
                                 .AND. (ISDL(I+3) .EQ. 0)) THEN
               IF (I .EQ. 0) THEN
 !                CALL DL2_RES (TRMOUT, T(1), T(2), T(3) etc)
               ELSE
                 IF(I .EQ. NL-2) THEN
 !                CALL DL2_RES (T(NL-2), T(NL-1), T(NL), TRMIN, etc)
                 ELSE
 !                CALL DL2_RES (T(I), T(I+1), T(I+2), T(I+3) etc)
                 ENDIF   !   end of IF/ELSE (I .EQ. NL-1)
               ENDIF   !  end of IF/ELSE (I .EQ. 0)
             ENDIF    !  end of IF(ISDL(I) .EQ. 0) .AND. .....
           END DO   !   end of scan through all layers
        ENDIF

        !  calculate convective OCF/jump heat transfer coefficients

       IF(NL .GE. 2) THEN  ! no OCF unless at least two layers exist
         !  It is not possible for both of the following cases to be
         !  true for the same gap (i.e., for NL=2)

         IF (FS%G(NL-1)%GTYPE .EQ. gtyOPENin) THEN
           SaveHCNLm = HC(NL-1)
           SaveHCNL =   HC(NL)
           HC(NL-1) = HC_GS
           HC(NL) =   HC_SA
           HJC(NL) =  HC_GA
         ENDIF

         HC(0) = HCOUT
         IF (FS%G(1)%GTYPE .EQ. gtyOPENout) THEN
           HC(0) = HCOUT + HCOCFout
           HJC(1) = HCOCFout
         ENDIF

       ENDIF

    !  copy convective heat transfer coefficients to 2D arrays
    !  adjacent layers
     IB=1
     IE=NL-1
     IF (IB .LE. IE) THEN
       DO I=IB,IE
         HC2D(I,I+1) = HC(I)
         HC2D(I+1,I) = HC2D(I,I+1)
       END DO
     ENDIF

    !  jumpers
     IB=2
     IE=NL-1
     IF (IB .LE. IE) THEN
       DO I=IB,IE
         HC2D(I-1,I+1) = HJC(I)
         HC2D(I+1,I-1) = HC2D(I-1,I+1)
       END DO
     ENDIF

    !  double jumpers  - NOT ACTIVE YET
     IB=2
     IE=NL-2
     IF (IB .LE. IE) THEN
       DO I=IB,IE
    !         HC2D(I-1,I+2) = H2JC(I)
    !         HC2D(I+2,I-1) = HC2D(I-1,I+2)
       END DO
     ENDIF

    !  outdoor side
       HCIout(1)=HC(0)
       IF(NL .GE. 2) HCIout(2)=HJC(1)

    !  indoor side
       HCIin(NL) = HC(NL)
       IF(NL .GE. 2) HCIin(NL-1)=HJC(NL)

    !  special case - indoor-to-outdoor convection (?)
       HCinout=0.0d0

    !  copy radiative heat transfer coefficients to 2D arrays
    !  adjacent layers
     IB=1
     IE=NL-1
     IF (IB .LE. IE) THEN
       DO I=IB,IE
         HR2D(I,I+1) = HR(I)
         HR2D(I+1,I) = HR2D(I,I+1)
       END DO
     ENDIF

    !  jumpers
     IB=2
     IE=NL-1
     IF (IB .LE. IE) THEN
       DO I=IB,IE
         HR2D(I-1,I+1) = HJR(I)
         HR2D(I+1,I-1) = HR2D(I-1,I+1)
       END DO
     ENDIF

    !  double jumpers
     IB=2
     IE=NL-2
     IF (IB .LE. IE) THEN
       DO I=IB,IE
    !         HR2D(I-1,I+2) = H2JR(I)
    !         HR2D(I+2,I-1) = HR2D(I-1,I+2)
       END DO
     ENDIF

    !  outdoor side
    HRIout(1)=HR(0)
    IF(NL .GE. 2)  HRIout(2)=HJR(1)

    !  indoor side
    HRIin(NL)=HR(NL)
    IF(NL .GE. 2)  HRIin(NL-1)=HJR(NL)

    !  special case - indoor-to-outdoor radiation
    IF(NL .EQ. 1)  HRinout=HJR(1)
    !       IF(NL .EQ. 2)  HRinout=H2JR(1)

    !  CONFIRM VALIDITY OF CODE

 IF (1 .EQ. 0) THEN    !  was used for debugging - successfully
                       !  and can now be bypassed
                       !  - code in this section generates the
                       !  same solution of temperatures (TNEW(i))
                       !  that was found by the net radiation
                       !  solver above (T(i))

   ADIM = NL
   A=0.0d0
   XSOL=0.0d0
   TOUTdv     =TOUT        ! solution for TNEW should
   TRMOUTdv   =TRMOUT      ! match existing solution
   TINdv      =TIN         ! for T
   TRMINdv    =TRMIN
   SOURCEdv   =SOURCE

   DO I=1,NL
     A(I,ADIM+1) = HCIout(I)*TOUTdv + HRIout(I)*TRMOUTdv +   &
                   HCIin(I) *TINdv  + HRIin(I) *TRMINdv  +   &
                   SOURCEdv(I)
     A(I,I) = HCIout(I) + HRIout(I) + HCIin(I) + HRIin(I)
     DO J=1,NL
       IF(J .NE. I) THEN
         A(I,I) = A(I,I) + HC2D(I,J) + HR2D(I,J)
         A(I,J) =  -1.0d0 * (HC2D(I,J) + HR2D(I,J))
       END IF
     END DO
   END DO

   !  SOLVE MATRIX
   !  Call SOLMATS for single precision matrix solution
    CALL SOLMATS( ADIM, A, XSOL)

   !  UNPACK SOLUTION VECTOR

    SUMERR=0.0d0
      DO I=1,NL
         TNEW(I)=XSOL(I)
         SUMERR=SUMERR+ABS(TNEW(I)-T(I))
      END DO

 END IF !   end (1 .EQ. 0)    code disabled

   !  calculate U-factor

   ADIM = NL
   A=0.0d0
   XSOL=0.0d0
   TNEW =0.0d0
   TOUTdv     =1.0d0
   TRMOUTdv   =1.0d0
   TINdv      =0.0d0
   TRMINdv    =0.0d0
   SOURCEdv   =0.0d0

   DO I=1,NL
     A(I,ADIM+1) = HCIout(I)*TOUTdv + HRIout(I)*TRMOUTdv +   &
                   HCIin(I) *TINdv  + HRIin(I) *TRMINdv  +   &
                   SOURCEdv(I)
     A(I,I) = HCIout(I) + HRIout(I) + HCIin(I) + HRIin(I)
     DO J=1,NL
       IF(J .NE. I) THEN
         A(I,I) = A(I,I) + HC2D(I,J) + HR2D(I,J)
         A(I,J) = -1.d0 * (HC2D(I,J) + HR2D(I,J))
       END IF
     END DO

   END DO

   !  SOLVE MATRIX
   !  Call SOLMATS for single precision matrix solution
   CALL SOLMATS( ADIM, A, XSOL)
   !  UNPACK SOLUTION VECTOR

   DO I=1,NL
       TNEW(I)=XSOL(I)
   END DO

   Q_IN = HCinout* (TOUTdv-TINdv) + HRinout *      &
                                       (TRMOUTdv-TRMINdv)
   DO I=1,NL
       Q_IN = Q_IN + HCIin(I)*(TNEW(I)-TINdv  )    &
                   + HRIin(I)*(TNEW(I)-TRMINdv)
   END DO
   Q_IN = Q_IN + SOURCEdv(NL+1)  ! this line not needed
   UCG = Q_IN
   Rvalue  = 5.678d0/UCG

  !  calculate SHGC

  SHGC = 0.0d0
  IF(ABS(ISOL) .GT. 0.01d0) THEN

   ADIM = NL
   A=0.0d0
   XSOL  =0.0d0
   TNEW =0.0d0
   TOUTdv     =0.0d0
   TRMOUTdv   =0.0d0
   TINdv      =0.0d0
   TRMINdv    =0.0d0
   DO I=1,NL+1
    SOURCEdv(I)   = SOURCE(I)
   END DO

   DO I=1,NL
     A(I,ADIM+1) = HCIout(I)*TOUTdv + HRIout(I)*TRMOUTdv +   &
                   HCIin(I) *TINdv  + HRIin(I) *TRMINdv  +   &
                   SOURCEdv(I)
     A(I,I) = HCIout(I) + HRIout(I) + HCIin(I) + HRIin(I)
     DO J=1,NL
       IF(J .NE. I) THEN
         A(I,I) = A(I,I) + HC2D(I,J) + HR2D(I,J)
         A(I,J) =  -1.0d0 * (HC2D(I,J) + HR2D(I,J))
       END IF
     END DO
   END DO

    !  SOLVE MATRIX
    !  Call SOLMATS for single precision matrix solution
    CALL SOLMATS( ADIM, A, XSOL)

    !  UNPACK SOLUTION VECTOR
    DO I=1,NL
        TNEW(I)=XSOL(I)
    END DO

    Q_IN = HCinout* (TOUTdv-TINdv) + HRinout *      &
                                    (TRMOUTdv-TRMINdv)
    DO I=1,NL
        Q_IN = Q_IN + HCIin(I)*(TNEW(I)-TINdv  )    &
                    + HRIin(I)*(TNEW(I)-TRMINdv)
    END DO
    Q_IN = Q_IN + SOURCEdv(NL+1)

    SHGC = Q_IN/ISOL  ! only executed if ISOL > 0.01 [W/m2]

  ENDIF               !  end if (ABS(ISOL) .GT. 0.01)

  !  calculate FHR_OUT

  ADIM = NL
  A=0.0d0
  XSOL =0.0d0
  TNEW =0.0d0
  TOUTdv     =1.0d0
  TRMOUTdv   =0.0d0
  TINdv      =0.0d0
  TRMINdv    =0.0d0
  SOURCEdv   =0.0d0

   DO I=1,NL
     A(I,ADIM+1) = HCIout(I)*TOUTdv + HRIout(I)*TRMOUTdv +   &
                   HCIin(I) *TINdv  + HRIin(I) *TRMINdv  +   &
                   SOURCEdv(I)
     A(I,I) = HCIout(I) + HRIout(I) + HCIin(I) + HRIin(I)
     DO J=1,NL
       IF(J .NE. I) THEN
         A(I,I) = A(I,I) + HC2D(I,J) + HR2D(I,J)
         A(I,J) = -1.d0 * (HC2D(I,J) + HR2D(I,J))
       END IF
     END DO
   END DO

   !  SOLVE MATRIX
   !  Call SOLMATS for single precision matrix solution
   CALL SOLMATS( ADIM, A, XSOL)

   !  UNPACK SOLUTION VECTOR

   DO I=1,NL
      TNEW(I)=XSOL(I)
   END DO

   Q_IN = HCinout* (TOUTdv-TINdv) + HRinout *      &
                                   (TRMOUTdv-TRMINdv)
   DO I=1,NL
       Q_IN = Q_IN + HCIin(I)*(TNEW(I)-TINdv  )      &
                   + HRIin(I)*(TNEW(I)-TRMINdv)
   END DO
   Q_IN = Q_IN + SOURCEdv(NL+1)

   FHR_OUT = 1.0d0 - (Q_IN/UCG)
   TAE_OUT = FHR_OUT*TRMOUT + (1.d0 - FHR_OUT)*TOUT

   !  calculate FHR_IN

   ADIM = NL
   A=0.0d0
   XSOL =0.0d0
   TNEW =0.0d0
   TOUTdv     =0.0d0
   TRMOUTdv   =0.0d0
   TINdv      =1.0d0
   TRMINdv    =0.0d0
   SOURCEdv   =0.0d0

   DO I=1,NL
     A(I,ADIM+1) = HCIout(I)*TOUTdv + HRIout(I)*TRMOUTdv +   &
                   HCIin(I) *TINdv  + HRIin(I) *TRMINdv  +   &
                   SOURCEdv(I)
     A(I,I) = HCIout(I) + HRIout(I) + HCIin(I) + HRIin(I)
     DO J=1,NL
       IF(J .NE. I) THEN
         A(I,I) = A(I,I) + HC2D(I,J) + HR2D(I,J)
         A(I,J) = -1.d0 * (HC2D(I,J) + HR2D(I,J))
       END IF
     END DO
   END DO

   !  SOLVE MATRIX
   !  Call SOLMATS for single precision matrix solution
   CALL SOLMATS( ADIM, A, XSOL)

   !  UNPACK SOLUTION VECTOR

   DO I=1,NL
      TNEW(I)=XSOL(I)
   END DO

   Q_IN = HCinout* (TOUTdv-TINdv) + HRinout *      &
                                (TRMOUTdv-TRMINdv)
   DO I=1,NL
      Q_IN = Q_IN + HCIin(I)*(TNEW(I)-TINdv  )      &
                  + HRIin(I)*(TNEW(I)-TRMINdv)
   END DO
   Q_IN = Q_IN + SOURCEdv(NL+1)

   FHR_IN = 1.0d0 + (Q_IN/UCG)
   TAE_IN = FHR_IN*TRMIN + (1.0d0 - FHR_IN)*TIN

    !   double check heat gain to room
    !   Q_IN calculated this way should be equal to QGAIN calculated
    !   above with raw results from the net radiation solution
    !   The difference between the two is printed below
    !   Both include the directly transmitted solar gain

   Q_IN = UCG*(TAE_OUT - TAE_IN) + SHGC*ISOL

    ! End of new code - for calculating Ucg and SHGC
    !  restore convective heat transfer coefficients if alterred earlier
    !  for more general resistor network - otherwise mainline will
    !  receive faulty data
   IF(NL .GE. 2) THEN  ! no OCF unless at least two layers exist
      IF (FS%G(NL-1)%GTYPE .EQ. gtyOPENin) THEN
        HC(NL-1) = SaveHCNLm
        HC(NL) =   SaveHCNL
      ENDIF
   ENDIF

   RETURN

END FUNCTION ASHWAT_Thermal

SUBROUTINE DL_RES_r2 (Tg,Td,Tm,rhog,rhodf,rhodb,taud,rhom,hr_gm,hr_gd,hr_md)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Returns the radiant heat transfer coefficients between parallel surfaces:
          !
          ! METHODOLOGY EMPLOYED:
          !  Solves radiant heat transfer coefficients between three parallel surfaces.
          !  The left and right surfcaes are opaque with reflectance rhog and rhom, respectively.
          !  And the middle layer is diathermanous with transmittance taud AND reflectance rhodf
          !  and rhodb on the left and rightsides, respectively.
          !  The subscripts g, d and m apply to Glass, Diathermanous layer, and mean-radiant room
          !  temperature in a configuration of a window with an indoor-side shading attachment
          !  but the analysis can be applied to any three layers in the configuration described
          !  above.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
   REAL(r64), INTENT(IN) :: Tg      ! mean glass layer temperature, {K}
   REAL(r64), INTENT(IN) :: Td      ! mean diathermanous layer temperature, {K}
   REAL(r64), INTENT(IN) :: Tm      ! mean radiant room temperature, {K}
   REAL(r64), INTENT(IN) :: rhog    ! reflectance of glass layer, {-}
   REAL(r64), INTENT(IN) :: rhodf   ! front reflectance of diathermanous layer, {-}
   REAL(r64), INTENT(IN) :: rhodb   ! back reflectance of diathermanous layer, {-}
   REAL(r64), INTENT(IN) :: taud    ! transmittance of diathermanous layer, {-}
   REAL(r64), INTENT(IN) :: rhom    ! reflectance of the room, {-}
   REAL(r64), INTENT(OUT):: hr_gm   ! heat transfer coefficient between left and right surface {W/m2K}
   REAL(r64), INTENT(OUT):: hr_gd   ! heat transfer coefficient between left and middle surface {W/m2K}
   REAL(r64), INTENT(OUT):: hr_md   ! heat transfer coefficient between right and middle surface {W/m2K}
          !
          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) :: Epsg, Epsdf, Epsdb, Epsm
   REAL(r64) :: A(20,22)
   REAL(r64) :: X(20)
   ! real FSg_g, FSdf_g, FSdb_g, FSm_g
   REAL(r64) :: FSg_df,FSdf_df,FSdb_df,FSm_df
   REAL(r64) :: FSg_db,FSdf_db,FSdb_db,FSm_db
   REAL(r64) :: FSg_m, FSdf_m, FSdb_m, FSm_m


!  Calculate 4 emissivities/absorptivities

   Epsg  = 1.d0 - rhog
   Epsdf = 1.d0 - rhodf - taud
   Epsdb = 1.d0 - rhodb - taud
   Epsm  = 1.d0 - rhom

!  Calculate script F shape factors
!  FSx_y is the portion of radiation emitted
!  by surface x that arrives at surface y
!  via any path - including reflections
!  By reciprocity FSxy=FSyx

! step 1:  unit emission from (g) only

   call SETUP4x4_A(rhog,rhodf,rhodb,taud,rhom,A)
   A(1,5) = 1.0d0        ! unit source of radiation
   call SOLMATS(4,A,X)
   FSg_df  = X(1)
!  FSg_g   = X(2)
   FSg_m   = X(3)
   FSg_db  = X(4)

! step 2:  unit emission from (df) only

!   call SETUP4x4_A(rhog,rhodf,rhodb,taud,rhom,A)
!   A(2,5) = 1.0        ! unit source of radiation
!   call SOLMATS(4,A,X)
!   FSdf_df  = X(1)
!   FSdf_g   = X(2)
!   FSdf_m   = X(3)
!   FSdf_db  = X(4)

! step 3:  unit emission from (db) only

!   call SETUP4x4_A(rhog,rhodf,rhodb,taud,rhom,A)
!   A(3,5) = 1.0        ! unit source of radiation
!   call SOLMATS(4,A,X)
!   FSdb_df  = X(1)
!   FSdb_g   = X(2)
!   FSdb_m   = X(3)
!   FSdb_db  = X(4)

! step 4:  unit emission from (m) only

   call SETUP4x4_A(rhog,rhodf,rhodb,taud,rhom,A)
   A(4,5) = 1.0d0        ! unit source of radiation
   call SOLMATS(4,A,X)
   FSm_df  = X(1)
!  FSm_g   = X(2)
!  FSm_m   = X(3)
   FSm_db  = X(4)

!  calculate heat transfer coefficients
!  hr_xy is the heat transfer coefficient from x to y [W/m2]
!  Note:  If the emissivity of either surface x or surface y is zero
!         then q_xy will also be zero
!  Note:  This code has no problem with temperatures being equal

    hr_gm = Epsg * Epsm * FSg_m * StefanBoltzmann * (Tg + Tm)*(Tg**2 + Tm**2)
    hr_gd = Epsg * Epsdf * FSg_df * StefanBoltzmann * (Td + Tg)*(Td**2 + Tg**2)   &
          + Epsg * Epsdb * FSg_db * StefanBoltzmann * (Td + Tg)*(Td**2 + Tg**2)

    hr_md = Epsm * Epsdf * FSm_df * StefanBoltzmann * (Td + Tm)*(Td**2 + Tm**2)   &
          + Epsm * Epsdb * FSm_db * StefanBoltzmann * (Td + Tm)*(Td**2 + Tm**2)
    RETURN
END SUBROUTINE DL_RES_r2

SUBROUTINE SETUP4x4_A(rhog,rhodf,rhodb,taud,rhom,A)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Returns the 4 X 4 matrix for DL_RES_r2 routine:
          !
          ! METHODOLOGY EMPLOYED:
          !  fills in the matrix coefficients
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
   REAL(r64),  INTENT(IN) :: rhog
   REAL(r64),  INTENT(IN) :: rhodf
   REAL(r64),  INTENT(IN) :: rhodb
   REAL(r64),  INTENT(IN) :: taud
   REAL(r64),  INTENT(IN) :: rhom
   REAL(r64), INTENT(OUT) :: A(20,22)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow

   A=0.0d0
   A(1,1) =  1.0d0
   A(1,2) = -1.0d0 * rhog
   A(2,1) = -1.0d0 * rhodf
   A(2,2) =  1.0d0
   A(2,4) = -1.0d0 * taud
   A(3,1) = -1.0d0 * taud
   A(3,3) =  1.0d0
   A(3,4) = -1.0d0 * rhodb
   A(4,3) = -1.0d0 * rhom
   A(4,4) =  1.0d0

  RETURN
END SUBROUTINE SETUP4x4_A

REAL(r64) FUNCTION FRA(TM,T,DT, AK,BK,CK, ACP,BCP,CCP, AVISC,BVISC,CVISC, RHOGAS)
          !
          !       AUTHOR         (John Wright, University of WaterLoo, ASHRAE 1311-RP)
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse, FSEC/UCF, May 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns Rayleigh number given surface temperatures, and coefficients of
          ! quadratic correlations as a function of temperature for gas properties
          !
          !

          ! METHODOLOGY EMPLOYED:
          !  Ra = Gr * Pr
          !
          !

          ! REFERENCES:
          !  ASHRAE 1311-RP
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: TM                ! mean gas temp, K
    REAL(r64), INTENT( IN) :: T                    ! gas layer thickness, m
                                                !   (as adjusted e.g. re VB models)
    REAL(r64), INTENT( IN) :: DT                ! temp difference across layer, K
    REAL(r64), INTENT( IN) :: AK, BK, CK        ! gas conductance coeffs, K = AK + BK*TM + CK*TM*TM
    REAL(r64), INTENT( IN) :: ACP, BCP, CCP        ! gas specific heat coeffs, CP = ACP + BCP*TM + CCP*TM*TM
    REAL(r64), INTENT( IN) :: AVISC,BVISC,CVISC ! gas viscosity coeffs, VISC = AVISC + BVISC*TM + CVISC*TM*TM
    REAL(r64), INTENT( IN) :: RHOGAS            ! gas density, kg/m3

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Z, K, CP, VISC
          ! flow
    Z   = 1.0d0
    K   = AK + BK*TM + CK*TM*TM
    CP  = ACP + BCP*TM + BCP*TM*TM
    VISC= AVISC + BVISC*TM + BVISC*TM*TM

    FRA =(GravityConstant * RHOGAS * RHOGAS * DT * T * T * T * CP)/(VISC * K * TM * Z * Z)

    RETURN
END FUNCTION FRA

REAL(r64) FUNCTION FNU(RA)
          !       AUTHOR         (John Wright, University of WaterLoo, ASHRAE 1311-RP)
          !       DATE WRITTEN
          !       MODIFIED       Bereket Nigusse, FSEC/UCF, May 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns Nusselt number given Rayleigh number
          !

          ! METHODOLOGY EMPLOYED:
          ! Uses empirical correlation
          !

          ! REFERENCES:
          !  ASHRAE 1311-RP
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: RA        ! Rayleigh number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)              :: ARA

    ARA = ABS( RA)
    IF ( ARA .LE. 10000.0d0 ) THEN
        FNU =1.0d0 + 1.75967d-10 * (ARA**2.2984755d0)
    ELSE IF (ARA .LE. 50000.0d0) THEN
        FNU=0.028154d0 * (ARA**0.413993d0)
    ELSE
        FNU=0.0673838d0 * (ARA**(1.0d0/3.0d0))
    ENDIF
    RETURN
END FUNCTION FNU

REAL(r64) FUNCTION HConvGap(G, T1, T2)
          !
          !       AUTHOR         (University of WaterLoo, ASHRAE 1311-RP)
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse, FSEC/UCF, May 2013
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns convective coefficient for a gap separated between two surfaces at
          ! temperatures T1 and T2 , W/m2-K
          !
          ! METHODOLOGY EMPLOYED:
          !  Hconv = "Nusselt Number" * "Conductivity Of Gas"  / "Thickness Of Gap"
          !
          !
          ! REFERENCES:
          !  ASHRAE 1311-RP
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSGAP), INTENT( IN) :: G            ! gap
    REAL(r64),     INTENT( IN) :: T1, T2    ! bounding surface temps (K)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                  :: TM        ! Mean temperature, K
    REAL(r64)                  :: DT        ! temperature difference, (K)
    REAL(r64)                  :: RA        ! Rayleigh Number, (-)
    REAL(r64)                  :: NU        ! Nusselt Number, (-)
    REAL(r64)                  :: KGAS      ! Gas conductivity at film temp, (W/m.K)
    REAL(r64)                  :: T         ! effective gap spacing, m
          ! Flow

    T = G%TAS_EFF
    TM = (T1 + T2) / 2.d0
    DT = T1 - T2
    RA=FRA( TM, T, DT, G%FG%AK, G%FG%BK, G%FG%CK, G%FG%ACP, G%FG%BCP, &
            G%FG%CCP, G%FG%AVISC, G%FG%BVISC, G%FG%CVISC, G%RHOGAS)
    NU=FNU( RA)

    KGAS = G%FG%AK+G%FG%BK*TM+G%FG%CK*TM*TM
    HConvGap = NU*KGAS/T
    RETURN
END FUNCTION HConvGap

REAL(r64) FUNCTION HRadPar(T1, T2, E1, E2)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns radiative coefficient between two surfaces, hr, W/m2-K
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  Radiative coefficient for parallel, opaque plates configuration and
          !  automatically reverts to small-object-in-large-enclosure if one of
          !  the emissivities is set to unity  i.e., set E1=1 and surface 2 is the
          !  small object with hr based on area A2 If one emissivity is zero then
          !  hr=0, division by zero is, avoided even if T1=T2.
          !
          !
          ! REFERENCES:
          !  ASHRAE 1311-RP
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64),   INTENT(IN):: T1, T2        ! bounding surface temps [K]
    REAL(r64),   INTENT(IN):: E1, E2        ! bounding surface emissivities

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)              :: DV            ! dummy variable
          ! Flow

     HRadPar = 0.0d0
     IF((E1 .GT. 0.001d0) .AND. (E2 .GT. 0.001d0)) THEN
        DV    = (1.0d0/E1) + (1.0d0/E2) - 1.0d0
        HRadPar = (StefanBoltzmann/DV) * (T1+T2) * (T1**2 + T2**2)
     ENDIF
     RETURN
END FUNCTION HRadPar

REAL(r64) FUNCTION HIC_ASHRAE(L, TG, TI)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns inside surface convective coefficient, W/m2-K
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  Footnote on Table 2, p. 31.6 (Fenestration) HOF 2005
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: L         ! glazing height, m
    REAL(r64), INTENT( IN) :: TG        ! glazing inside surf temp, C or K
    REAL(r64), INTENT( IN) :: TI        ! inside air temp, C or K
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow

    HIC_ASHRAE = 1.46d0 * (ABS( TG-TI) / MAX( L, 0.001d0))**0.25d0
    RETURN
END FUNCTION HIC_ASHRAE

SUBROUTINE SLtoGL(breal,Ts,Tg,hsg, scheme)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Returns the heat transfer coefficient, shade-to-glass
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64),  INTENT(IN):: breal        ! distance from shade to glass (m)
    REAL(r64),  INTENT(IN):: Ts           ! shade temperature (K)
    REAL(r64),  INTENT(IN):: Tg           ! glass temperature (K)
    INTEGER,    INTENT(IN):: scheme       !
    REAL(r64), INTENT(OUT):: hsg          ! the heat transfer coefficient, shade-to-glass, {W/m2K}
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)             :: b
    REAL(r64)             :: Tavg
    REAL(r64)             :: P
    REAL(r64)             :: rho
    REAL(r64)             :: beta
    REAL(r64)             :: dvisc
    REAL(r64)             :: Cp
    REAL(r64)             :: k
    REAL(r64)             :: Rabsg
    REAL(r64)             :: Nubsg
          ! Flow

    hsg = 0.0d0                 !  default - large spacing, b

    IF (scheme .eq. 1) then     !  simple conduction layer, Nu=1

        b=breal
        If (b .LT. 0.00001d0) b=0.00001d0  ! avoid division by zero in
                                           ! calculation of this scheme

        Tavg=(Ts+Tg)/2.d0                    ! T for properties calculations
        k= 0.02538d0+((Tavg - 290.d0)/10.d0)*(0.02614d0 - 0.02538d0) ! conductivity (W/m.K)
        hsg=k/b

    ELSE IF (scheme .eq. 2) then                ! similar to Nu=1 but small penalty at
                                                ! larger Ra    (Collins)
        b=breal
        if (b .LT. 0.00001d0) b=0.00001d0       ! avoid division by zero in
                                                ! calculation of this scheme

        Tavg=(Ts+Tg)/2.d0                        ! T for properties calculations

        ! properties of AIR
        rho=PAtmSeaLevel/(287.097d0 * Tavg)     ! density (kg/m3) <- temperature in (K)
        beta=1.d0/Tavg                          ! thermal expansion coef(/K)
        dvisc = (18.05d0 + ((Tavg - 290.d0)/10.d0) * (18.53d0 - 18.05d0)) * 1.0d-6
                       !  dynamic viscosity (kg/m.sec) or (N.sec/m2)
        Cp= 1044.66d0 - 0.31597d0*Tavg+0.000707908d0*Tavg**2 - 0.00000027034d0*Tavg**3
                       !  specific heat at constant pressure (J/kg.K)
        k= 0.02538d0+((Tavg - 290.d0)/10.d0)*(0.02614d0 - 0.02538d0) ! conductivity (W/m.K)

        Rabsg= (9.81d0*beta*(b**3)*ABS(Ts-Tg)*(rho**2)*Cp)/(dvisc*k)
        Nubsg=1.d0 + 0.2d0 * (1.0d0 -EXP(-0.005d0*Rabsg))

        hsg=Nubsg*k/b
    END IF   !  end of scheme .eq. 2
    RETURN
END SUBROUTINE SLtoGL

REAL(r64) FUNCTION SLtoAMB(b, L, Ts, Tamb, hc_in, scheme)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns shade to room air heat transfer coefficient
          !
          !
          ! METHODOLOGY EMPLOYED:
          !
          ! fill gas is always air, orientation is always vertical
          ! hsamb should be h-flatplate at b=0 and 2*h-flatplate at b=large.  Note
          ! that hsamb is the same at slat angle = 0, 90, -90 degrees but increase
          ! by 20% at slat angle =45 degrees to mimic air pumping between slats
          ! therefore, specify slat angle=0 or 90 or -90 is shade is other than
          ! a venetian blind
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na

      IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      REAL(r64), INTENT(IN) :: b            ! distance from shade to glass (m) where air flow takes place
      REAL(r64), INTENT(IN) :: L            ! window height, m (usually taken as 1 m)
      REAL(r64), INTENT(IN) :: Ts           ! shade temperature, K
      REAL(r64), INTENT(IN) :: Tamb         ! room air temperature, K
      REAL(r64), INTENT(IN) :: hc_in        ! indoor (room) convective transfer coeff, W/m2K)
      INTEGER,   INTENT(IN) :: scheme       ! flag to select model, scheme=2 has problems
                                            !  scheme=3 recommended
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! a
      REAL(r64)             :: Tavg
      REAL(r64)             :: P
      REAL(r64)             :: rho
      REAL(r64)             :: beta
      REAL(r64)             :: dvisc
      REAL(r64)             :: Cp
      REAL(r64)             :: k
      REAL(r64)             :: Rabsa
      REAL(r64)             :: hfp
          ! Flow

      SLtoAmb = 2.d0 * hc_in    !    DEFAULT - convection from both sides
                               !    of shading layer - large spacing, b

      IF (scheme .eq. 1) then
        ! properties of AIR
        Tavg=(Ts+Tamb)/2.d0
        rho=PAtmSeaLevel/(287.097d0*Tavg)  ! density (kg/m3) <- temperature in (K)
        beta=1.d0/Tavg              ! thermal expansion coef(/K)
        dvisc = (18.05d0 +  ((Tavg-290.d0)/10.d0)  * (18.53d0-18.05d0)) * 1.0d-6
                       !  dynamic viscosity (kg/m.sec) or (N.sec/m2)
        Cp= 1044.66d0-0.31597d0*Tavg+0.000707908*Tavg**2-0.00000027034d0*Tavg**3
                       !  specific heat at constant pressure (J/kg.K)
        k= 0.02538d0+((Tavg-290.d0)/10.d0)*(0.02614d0-0.02538d0) ! conductivity (W/m.K)

        Rabsa= ( 9.81d0*beta*(b**3)*ABS(Ts-Tamb)*(rho**2)*Cp )/(dvisc*k)
        IF (Rabsa .LE. 1.d0) then
          Rabsa=1.0d0
        ENDIF

        hfp=   HIC_ASHRAE(L, Ts, Tamb)  ! h - flat plate, influence by
                                        ! window height and temperature
                                        ! difference.  Note:  hfp goes to
                                        ! zero as delta-T goes to zero

        !  now adjust for distance from window glass
        SLtoAmb=hfp*(1.d0 + EXP(-6000.d0/Rabsa))
        !  SLtoAmb goes to 2*hfp at large b and hfp at small b and small (20%)
        !  penalty is applied if slat angle is not zero or +/- 90 degrees
        !  Note:  influence of distance is lost if delta-T goes to zero
        !  Note:  as delta-T -> zero, Rabga->0, SLtoAmb -> hfp, not 2hfp,
        !        for any spacing, even large b.  This is a problem

      ELSE IF (scheme .eq. 2) then
        ! properties of AIR
        Tavg=(Ts+Tamb)/2.d0
        rho=PAtmSeaLevel/(287.097d0*Tavg)     ! density (kg/m3) <- temperature in (K)
        beta=1.d0/Tavg              ! thermal expansion coef(/K)
        dvisc = (18.05d0 +  ((Tavg - 290.d0)/10.d0)  * (18.53d0 - 18.05d0)) * 1.0d-6
                       !  dynamic viscosity (kg/m.sec) or (N.sec/m2)
        Cp= 1044.66d0 - 0.31597d0*Tavg+0.000707908d0*Tavg**2 -0.00000027034d0*Tavg**3
                       !  specific heat at constant pressure (J/kg.K)
        k= 0.02538d0+((Tavg-290.d0)/10.d0)*(0.02614d0 - 0.02538d0) ! conductivity (W/m.K)

        Rabsa= ( 9.81d0*beta*(b**3)*ABS(Ts-Tamb)*(rho**2)*Cp )/(dvisc*k)
        IF (Rabsa .LE. 1.d0) then
          Rabsa=1.0d0
        ENDIF

        hfp=   hc_in                 ! h - flat plate - from calling routine
            !Note:  using this approach, L no longer has influence on hfp

        ! now adjust for distance from window glass
        SLtoAmb=hfp*(1.d0 + EXP(-6000.d0/Rabsa))
            ! Note:  as delta-T -> zero, Rabga->0, SLtoAmb -> hfp, not 2hfp,
            !        for any spacing, even large b.  This is a problem

      ELSE IF (scheme .eq. 3) then

        hfp=   hc_in            ! h - flat plate - from calling routine
        ! now adjust for distance from window glass
        SLtoAmb=hfp*(2.d0 - exp(-4.6d0 * b /0.1d0))
            !Note:  using this approach, L and temperatures no longer have
            !                               influence on result
            !  SLtoAmb = 2*hc_in when glass/shade spacing, b, is large
            !  SLtoAmb = hc_in when glass/shade spacing, b, is zero
            !  The exponential decay is 99% complete at b=4 inches = 0.1 m
            !                                               ln(0.01) = -4.6
            !  This coefficient could be fine tuned in future versions, perhaps
            !  as a function of boundary layer thickness for specific values
            !  of glass and shade temperatures
      END IF   !  end of scheme .eq. 3
      RETURN
END FUNCTION SLtoAMB

SUBROUTINE GLtoAMB(b,L,Tg,Tamb, hc_in, hgamb, scheme)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         John L. Wright, University of Waterloo,
          !                      Mechanical Engineering, Advanced Glazing System Laboratory
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Returns the glass to room air heat transfer coefficient
          !
          ! METHODOLOGY EMPLOYED:
          ! scheme = flag to select model, scheme=2 has problems, scheme=3 recommended
          ! fill gas is always air, orientation is always vertical
          ! hgamb should be zero at b=0, h-flatplate at b=large
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE   ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),  INTENT(IN) :: b            ! distance from shade to glass {m}
  REAL(r64),  INTENT(IN) :: L            ! window height {m}, usually taken as 1 meter
  REAL(r64),  INTENT(IN) :: Tg           ! glass temperature {K}
  REAL(r64),  INTENT(IN) :: Tamb         ! room air temperature, {K}
  REAL(r64),  INTENT(IN) :: hc_in        ! inside convection coefficient, {W/m2K}
  REAL(r64), INTENT(OUT) :: hgamb        ! glass to room air heat transfer coefficient
  INTEGER,    INTENT(IN) :: scheme
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: Tavg
  REAL(r64)              :: P
  REAL(r64)              :: rho
  REAL(r64)              :: beta
  REAL(r64)              :: dvisc
  REAL(r64)              :: Cp
  REAL(r64)              :: k
  REAL(r64)              :: Rabga
  REAL(r64)              :: hfp
          ! Flow

    hgamb = hc_in            ! default - good for large glass/shade spacing

  IF (scheme .eq. 1) Then    !  Collins

    Tavg=(Tg+Tamb)/2.d0    !T for properties calculations

        ! properties of AIR
        rho=PAtmSeaLevel/(287.097d0*Tavg)  ! density (kg/m3) <- temperature in (K)
        beta=1.d0/Tavg                   ! thermal expansion coef(/K)
        dvisc = (18.05d0 +  ((Tavg-290.d0)/10.d0)  * (18.53d0 - 18.05d0)) * 1.0d-6
                       !  dynamic viscosity (kg/m.sec) or (N.sec/m2)
        Cp= 1044.66d0-0.31597d0*Tavg+0.000707908d0*Tavg**2-0.00000027034d0*Tavg**3
                       !  specific heat at constant pressure (J/kg.K)
        k= 0.02538d0+((Tavg-290.d0)/10.d0)*(0.02614d0-0.02538d0) ! conductivity (W/m.K)

    Rabga= (9.81d0*beta*(b**3)*ABS(Tg-Tamb)*(rho**2)*Cp)/(dvisc*k)
    IF (Rabga .LE. 1.d0) then
        Rabga=1.0d0
    ENDIF

    hfp=   HIC_ASHRAE(L, Tg, Tamb)  ! h - flat plate
            ! Note:  as delta-T goes to zero, hfp will also go to zero

    hgamb= hfp*exp(-50.d0/Rabga)
            ! Note:  as delta-T -> zero, Rabga->0, hgamb -> zero too
            !        for any spacing, even large b.  This is a problem

  ELSE IF (scheme .eq. 2) then

    Tavg=(Tg+Tamb)/2.d0    !T for properties calculations

        ! properties of AIR
        rho=PAtmSeaLevel/(287.097d0*Tavg)     ! density (kg/m3) <- temperature in (K)
        beta=1.d0/Tavg                           ! thermal expansion coef(/K)
        dvisc = (18.05d0 +  ((Tavg-290.d0)/10.d0)  * (18.53d0-18.05d0)) * 1.0d-6
                       !  dynamic viscosity (kg/m.sec) or (N.sec/m2)
        Cp= 1044.66d0-0.31597d0*Tavg+0.000707908d0*Tavg**2-0.00000027034d0*Tavg**3
                       !  specific heat at constant pressure (J/kg.K)
        k= 0.02538d0+((Tavg-290.d0)/10.d0)*(0.02614d0-0.02538d0) ! conductivity (W/m.K)

    Rabga= (9.81d0*beta*(b**3)*ABS(Tg-Tamb)*(rho**2)*Cp)/(dvisc*k)
    IF (Rabga .LE. 1.d0) then
        Rabga=1.0d0
    ENDIF

     hfp=   hc_in                    ! h - flat plate - from calling routine
            !Note:  using this approach, L no longer has influence on result
            !       but temperature does and it will drive hgamb to zero when
            !       the temperature difference goes to zero

    hgamb= hfp*exp(-50.0d0/Rabga)
            ! Note:  as delta-T -> zero, Rabga->0, hgamb -> zero too
            !        for any spacing, even large b.  This is a problem

  ELSE IF (scheme .eq. 3) then

    hfp=   hc_in                    ! h - flat plate - from calling routine
    hgamb= hfp*(1.d0 - exp(-4.6d0 * b /0.1d0))
            !Note:  using this approach, L and temperatures no longer have
            !                               influence on result
            !  hgamb = hc_in when glass/shade spacing, b, is large
            !  hgamb = zero  when glass/shade spacing, b, is zero
            !  The exponential decay is 99% complete at b=4 inches = 0.1 m
            !                                               ln(0.01) = -4.6
            !  This coefficient could be fine tuned in future versions, perhaps
            !  as a function of boundary layer thickness for specific values
            !  of glass and shade temperatures

  EndIf     !  end of scheme .eq. 3
  RETURN
END SUBROUTINE GLtoAMB

REAL(r64) FUNCTION ConvectionFactor(L)
          !
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          !  Modifies convection rate per shade configuration, layer convection enhancement
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE(CFSLayer), INTENT(IN) :: L         ! window layer
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                  :: SlatADeg
          ! Flow

    IF (L%LTYPE == ltyVBHOR) THEN
        ! horiz VB: enhanced convection at +/- 45 due to "pumping"
        SlatADeg = MIN( 90.0d0, ABS( L%PHI_DEG))
        ConvectionFactor = 1.0d0 + 0.2d0 * SIN( 2.0d0 * SlatADeg)
    ELSE
        ConvectionFactor = 1.0d0
    END IF
    RETURN
END FUNCTION ConvectionFactor

LOGICAL FUNCTION CFSUFactor( FS, TOUT, HCOUT, TIN, HCIN, U)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         unknown (University of WaterLoo, ASHRAE 1311-RP)
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse, FSEC/UCF, June 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! ! returns .TRUE. if the U-value calculation succeeded, .FALSE. if error
          !

          ! METHODOLOGY EMPLOYED:
          !  uses net radiation method to solve for window surface temperatures and
          !  heat fluxes. Then calculates the U-value from the flux and over all
          !  temperature difference.

          ! REFERENCES:
          !  ASHRAE 1311-RP
          !
          !
          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE (CFSTY), INTENT(IN):: FS            ! fenestration system
    REAL(r64),    INTENT(IN):: TOUT          ! outdoor temperature, C (air and MRT)
    REAL(r64),    INTENT(IN):: HCOUT         ! outdoor convective coefficient, W/m2-K
    REAL(r64),    INTENT(IN):: TIN           ! indoor air temperature, C
    REAL(r64),    INTENT(IN):: HCIN          ! indoor convective coefficient, W/m2-K
    REAL(r64),   INTENT(OUT):: U             ! returned: U factor, W/m2-K
                                             ! for conditions specified (no incident solar)
          !
          ! FUNCTION PARAMETER DEFINITIONS:
    REAL(r64),     PARAMETER:: TOL = 0.01d0  ! 0.0001d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER   :: NL
    REAL(r64) :: TOABS, TRMOUT, TIABS, TRMIN
    REAL(r64) :: QOCF( FS%NL), QOCFRoom
    REAL(r64) :: JB(0:FS%NL), JF(1:FS%NL+1), T( FS%NL), Q(0:FS%NL), H(0:FS%NL+1)
    REAL(r64) :: SOURCE( FS%NL+1)
    REAL(r64) :: ISOL, UX, SHGC, QRLW, QCONV, QROOM
          ! Flow

    CFSUFactor = .FALSE.
    IF (ABS( TOUT - TIN) < 0.01d0) THEN
        U = -1.0d0
        RETURN
    END IF

    TOABS  = TOUT + KelvinConv
    TRMOUT = TOABS
    TIABS  = TIN + KelvinConv
    TRMIN  = TIABS

    NL = FS%NL
    ISOL = 0.0d0        ! no solar winter condition
    SOURCE = 0.0d0

    CFSUFactor = ASHWAT_Thermal( FS, TIABS, TOABS, HCIN, HCOUT, TRMOUT, TRMIN, ISOL, &
                 SOURCE(1:NL+1), TOL, QOCF, QOCFRoom, T, Q, JF, JB, H, U, SHGC, .TRUE.)
    IF ( .NOT. CFSUFactor) RETURN
    CFSUFactor = .TRUE.
    RETURN
END FUNCTION CFSUFactor

SUBROUTINE ASHWAT_Solar(NL, LSWP_ON, SWP_ROOM, IBEAM, IDIFF, ILIGHTS, SOURCE, SourceBD)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT and NATHAN KOTEY,
          !       DATE WRITTEN   June, 2006
          !       MODIFIED       Bereket Nigusse, JUNE 2013
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns the optical properties of multi-layer fenestration system model given optical
          ! properties of the layers
          !
          ! METHODOLOGY EMPLOYED:
          !    Ues combination net radiation method and TDMA solver
          !
          ! REFERENCES:
          !  JOHN L. WRIGHT and NATHAN KOTEY (2006). Solar Absorption By each Element in a Glazing/Shading
          !   Layer Array, ASHRAE Transactions, Vol. 112, Pt. 2. pp. 3-12.
          !   University of Waterloo, Mechanical Engineering
          !   Advanced Glazing System Laboratory
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER,        INTENT(IN) :: NL            ! # of layers
    TYPE (CFSSWP),  INTENT(IN) :: LSWP_ON(:)    ! layer SW (solar) properties (off-normal adjusted)
                                                !   1=outside .. NL=inside
    TYPE (CFSSWP),  INTENT(IN) :: SWP_ROOM      ! effective SW (solar) properties of room
                                                !   generally black or minimally reflective
    REAL(r64),      INTENT(IN) :: IBEAM         ! incident beam insolation (W/m2 aperture)
    REAL(r64),      INTENT(IN) :: IDIFF         ! incident diffuse insolation (W/m2 aperture)
    REAL(r64),      INTENT(IN) :: ILIGHTS       ! incident diffuse insolation (W/m2 aperture)
                                                !     on inside surface (e.g., from lights)
    REAL(r64),      INTENT(OUT):: SOURCE( NL+1) ! returned: layer-by-layer flux of absorbed
                                                !  solar radiation (beam-beam + beam-diffuse) (W/m2)
                                                ! SOURCE(NL+1) is the flux of solar radiation
                                                !  absorbed in conditioned space (W/m2 aperture area)
    REAL(r64),OPTIONAL,INTENT(OUT):: SourceBD(NL+1) ! returned: layer-by-layer flux of absorbed
                                                !  beam-diffuse solar radiation (W/m2)
                                                ! SOURCE_BD(NL+1) is the flux of beam-diffuse solar radiation
                                                !  absorbed in conditioned space (W/m2 aperture area)
                                                ! or this beam-diffuse solar transmittance of the system

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
              !
    REAL(r64) :: BPLUS(0:NL),BMINUS(0:NL)       ! beam solar fluxes flowing in outward and inward directions
                                                !   correspond to Edwards QPLUS and QMINUS (except note
                                                !   reverse layer numbering)
    REAL(r64) :: CPLUS(0:NL),CMINUS(0:NL)       ! diffuse solar fluxes caused by BPLUS and BMINUS;
                                                !   appear as sources in diffuse calculation
    REAL(r64) :: DPLUS(0:NL),DMINUS(0:NL)       ! diffuse solar fluxes flowing in outward and inward
                                                !   directions (W/m2)
    REAL(r64) :: AP(2*NL)
    REAL(r64) :: AE(2*NL)
    REAL(r64) :: AW(2*NL)
    REAL(r64) :: BP(2*NL)
    REAL(r64) :: X(2*NL)
    REAL(r64) :: CHKSUM
    REAL(r64) :: BeamDiffuseAbs(NL+1)           ! beam-diffuse absorbed fraction of beam radiation (W/m2)
    INTEGER   :: N_TDMA
    INTEGER   :: I
    INTEGER   :: LINE
          ! Flow

    IF (NL < 1) RETURN

    !  STEP ONE: THE BEAM-BEAM ANALYSIS TO FIND BPLUS AND BMINUS
    CALL NETRAD( NL, LSWP_ON%RHOSFBB,LSWP_ON%RHOSBBB, &
                     LSWP_ON%TAUSFBB, LSWP_ON%TAUSBBB, &
                     SWP_ROOM%RHOSFBB, IBEAM, BPLUS, BMINUS)


    !  STEP TWO: CALCULATE THE DIFFUSE-CAUSED-BY-BEAM SOURCES CPLUS AND CMINUS
    CPLUS( NL) = SWP_ROOM%RHOSFBD * BMINUS( NL)
    DO I=NL,1,-1   ! March through layers, indoor to outdoor
        CPLUS(I-1) = LSWP_ON(I)%RHOSFBD*BMINUS(I-1) &
                                  + LSWP_ON(I)%TAUSBBD*BPLUS(I)
        CMINUS(I)  =  LSWP_ON(I)%RHOSBBD*BPLUS(I) &
                                  + LSWP_ON(I)%TAUSFBD*BMINUS(I-1)
    END DO
    CMINUS(0) = 0.0d0


    !  STEP THREE: DIFFUSE FLUXES, DPLUS AND DMINUS,
    !  CAUSED BY DIFFUSE INCIDENT, IDIFF ON THE OUTDOOR SIDE
    !  AND BY ILIGHTS ON THE INDOOR SIDE, AND BY
    !  DIFFUSE SOURCE (FROM BEAM) FLUXES, CPLUS AND CMINUS

    N_TDMA = 2*NL

    DO I=1,NL
        LINE = (2*I)-1
          AP(LINE) = LSWP_ON(I)%RHOSBDD
          AE(LINE) = 1.0d0
          IF(LINE .NE. 1) THEN     ! default
            AW(LINE) = -1.0d0 * LSWP_ON(I)%TAUS_DD
            BP(LINE) = -1.0d0 * CMINUS(I)
          ELSE      !  special case at west-most node
            AW(1) = 0.0d0
            BP(1) = -1.0d0*LSWP_ON(1)%TAUS_DD*IDIFF - CMINUS(1)
          ENDIF

        LINE = (2*I)
        AW(LINE) = 1.0d0
          IF(LINE.NE.N_TDMA) THEN     ! default
            AP(LINE) = LSWP_ON(I+1)%RHOSFDD
            AE(LINE) = -1.0d0 * LSWP_ON(I+1)%TAUS_DD
            BP(LINE) = -1.0d0 * CPLUS(I)
          ELSE          !  special case at east-most node
            AP(LINE) = SWP_ROOM%RHOSFDD
            BP( N_TDMA) = -1.0d0 * (CPLUS(NL)+ILIGHTS)
            AE( N_TDMA) = 0.0d0
          ENDIF

    END DO

    CALL AUTOTDMA( X, AP, AE, AW, BP , N_TDMA)

    !   UNPACK TDMA SOLUTION VECTOR
    DO I=1,NL
        LINE = (2*I)-1
        DPLUS(I) = X(LINE)
        LINE = (2*I)
        DMINUS(I)  = X(LINE)
    END DO

    !  Finish up diffuse calculations
    DMINUS(0) = IDIFF
    DPLUS(0)  = LSWP_ON(1)%RHOSFDD*DMINUS(0) &
              + LSWP_ON(1)%TAUS_DD*DPLUS(1) &
              + CPLUS(0)

    !  STEP FOUR: ABSORBED SOLAR RADIATION AT EACH LAYER/NODE
    SOURCE = 0.0d0
    SOURCE(NL+1) = BMINUS(NL)-BPLUS(NL)+ &  ! SOLAR FLUX
                   DMINUS(NL)-DPLUS(NL)+ &  ! TRANSMITTED TO
                   ILIGHTS                  ! ROOM

    !  NOTE:  In calculating SOURCE(room) there is a trick included in the
    !         previous line:  ILIGHTS is added because it is included
    !         in DPLUS(NL) but ILIGHTS should not be included in this
    !         type of calculation of SOURCE(i).  No similar adjustment
    !         is needed for any of the other values of SOURCE(i)
    !         As an alternative get the same result using:
    !     SOURCE(NL+1) = BMINUS(NL)*(1.0 - SWP_ROOM%RHOSFBB - SWP_ROOM%RHOSFBD) +
    !    &               DMINUS(NL)*(1.0 - SWP_ROOM%RHOSFDD)
    !         Take your pick

   ! Added by BAN, June 7, 2013 to extract the beam-diffuse component for use
   ! in the EnergyPLus heat balance.  EnergyPlus requires the beam-beam and
   ! Beam-diffuse components separately.
   BeamDiffuseAbs = 0.0d0
   BeamDiffuseAbs(NL+1) = DMINUS(NL)- DPLUS(NL)     ! beam-diffuse transmitted to the room
   DO I=1,NL
      SOURCE(I) = BPLUS(I) - BMINUS(I) - BPLUS(I-1) + BMINUS(I-1) +   &
                  DPLUS(I) - DMINUS(I) - DPLUS(I-1) + DMINUS(I-1)
      ! Added by BAN June 7, 2013
      BeamDiffuseAbs(I) = 0.0d0
   END DO

   IF ( PRESENT(SourceBD) )THEN
       SourceBD = BeamDiffuseAbs
   ENDIF
   !  CHECKSUM - ALL INCOMING SOLAR FLUX MUST GO SOMEWHERE, SHOULD EQUAL ZERO
   CHKSUM=IBEAM+IDIFF+ILIGHTS-BPLUS(0)-DPLUS(0)
   DO I=1,NL+1
      CHKSUM=CHKSUM-SOURCE(I)
   END DO

   RETURN
END SUBROUTINE ASHWAT_Solar

SUBROUTINE NETRAD(NL, RHOF, RHOB, TAUF, TAUB, RHO_room, ISOL, QPLUS, QMINUS)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns the solar radiant fluxes between glazing layers
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  Net Radiation Method by LARGELY EDWARDS
          !  TED, RED, QPLUS, QMINUS correspond to variables found in "Edwards"
          !  but with reversed layers order indexing (layer 1=outside .. NL=inside)
          !
          !  GAP I is between layer I and I+1
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER,    INTENT(IN) :: NL             ! # of layers, 1=outside .. NL=inside
    REAL(r64),  INTENT(IN) :: RHOF(:)        ! solar reflectance of layer outside facing sides
    REAL(r64),  INTENT(IN) :: RHOB(:)        ! solar reflectance of layer inside facing sides
    REAL(r64),  INTENT(IN) :: TAUF(:)        ! solar transmittance of layer, for incidence
    REAL(r64),  INTENT(IN) :: TAUB(:)        ! solar transmittance of layer, back incidence
    REAL(r64),  INTENT(IN) :: RHO_room       ! effective solar reflectance of room (at inside)
    REAL(r64),  INTENT(IN) :: ISOL           ! incident flux (W/m2)
    REAL(r64), INTENT(OUT) :: QPLUS( 0:)     ! returned: see Edwards paper
    REAL(r64), INTENT(OUT) :: QMINUS( 0:)    ! returned: see Edwards paper
          !
          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)              :: TED( NL+1)
    REAL(r64)              :: RED( NL+1)
    INTEGER                :: I
          ! Flow

    IF (NL < 1) RETURN

    !   Reflectance and Transmittance

    RED(NL+1) = RHO_room
    TED(NL+1) = 0.0d0
    !#if 1
    DO I=NL,1,-1
        TED(I) = TAUF( I) / MAX( 0.00001d0, 1.0d0 - RHOB( I)*RED(I+1))
        RED(I) = RHOF(I) + TED( I)*TAUB(I)*RED(I+1)
    END DO
    !#else
    !I = SIZE( RED)
    !DO I=NL,1,-1
    !    RED(I) = RHOF(I) + (TAUF(I)*TAUB(I)*RED(I+1)) &
    !             / (1.0d0 -(RHOB(I)*RED(I+1)))
    !    TED(I) = TAUF(I)/(1.0d0 -RHOB(I)*RED(I+1))
    !END DO
    !#endif

    !   Outward and Inward Solar Fluxes, QPLUS AND QMINUS, Respectively
    QMINUS(0)=ISOL
    QPLUS(0)=QMINUS(0)*RED(1)
    DO I=1,NL
        QMINUS(I) = QMINUS(I-1)*TED(I)
        QPLUS(I)  = QMINUS(I)*RED(I+1)
    END DO
    RETURN
END SUBROUTINE NETRAD

SUBROUTINE TDMA_R(X, AP, AE, AW, BP, N)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! TDMA solver
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  1-D TDMA reverse solver. East/West sweep followed by West/East sweep
          !
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(OUT) :: X(:)
    REAL(r64),  INTENT(IN) :: AP(:)
    REAL(r64),  INTENT(IN) :: AE(:)
    REAL(r64),  INTENT(IN) :: AW(:)
    REAL(r64),  INTENT(IN) :: BP(:)
    INTEGER,    INTENT(IN) :: N
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER   :: J
    REAL(r64) :: ALPHA( N), BETA( N)
          ! Flow

    ALPHA(N)=AW(N)/AP(N)
    BETA(N) =BP(N)/AP(N)

    DO J=N-1,1,-1
        ALPHA(J)= AW(J) / ( AP(J)-(ALPHA(J+1)*AE(J)) )
        BETA(J) = ((AE(J)*BETA(J+1)) + BP(J)) &
                  / ( AP(J)-(ALPHA(J+1)*AE(J)) )
    END DO

    X(1) = BETA(1)
    DO J=2,N
        X(J)=( ALPHA(J)*X(J-1) ) + BETA(J)
    END DO
    RETURN
END SUBROUTINE TDMA_R

SUBROUTINE TDMA(X,AP,AE,AW,BP,N)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Matrix solver
          !
          ! METHODOLOGY EMPLOYED:
          !  1-D TDMA solver.
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(OUT) :: X(:)
    REAL(r64),  INTENT(IN) :: AP(:)
    REAL(r64),  INTENT(IN) :: AE(:)
    REAL(r64),  INTENT(IN) :: AW(:)
    REAL(r64),  INTENT(IN) :: BP(:)
    INTEGER,    INTENT(IN) :: N
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: J
    REAL(r64) :: ALPHA( N), BETA( N), D
          ! Flow

    ALPHA(1)=AE(1)/AP(1)
    BETA(1) =BP(1)/AP(1)

    DO J=2,N
        D = AP(J)-(ALPHA(J-1)*AW(J))
        IF ( ABS( D) < .0001d0) THEN
            ALPHA( J) = 0.0d0
            BETA( J) = 0.0d0
        ELSE
            ALPHA(J)= AE(J) / D
            BETA(J) = ((AW(J)*BETA(J-1)) + BP(J)) / D
        END IF
    END DO

    X( N) = BETA(N)
    DO J=N-1,1,-1
        X( J)=( ALPHA(J)*X(J+1) ) + BETA(J)
    END DO
    RETURN
END SUBROUTINE TDMA

SUBROUTINE AUTOTDMA(X,AP,AE,AW,BP, N)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Matrix solver manager routine
          !
          ! METHODOLOGY EMPLOYED:
          !  1-D TDMA solver.
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( INOUT) :: AP(:)
    REAL(r64), INTENT( IN)    :: AE(:),AW(:),BP(:)
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                 :: X(:)
    INTEGER                   :: N
          ! Flow

    !  Call TDMA for forward (i.e., west-to-east and back) calculation
    !  or TDMA_R for reverse (i.e., east-to-west and back) calculation
    !      TDMA   won't tolerate RHOSFxx(1)=0   (i.e., ap(1)=0)
    !  but TDMA_R won't tolerate RHOSBxx(N-1)=0 (i.e., ap(n)=0)
    !  where n-1 refers to the outdoor layer (glazing or shading layer)

    !  This if-statement will catch the situation where RHOSFxx(1)=0.
    !  i.e., AP(1)=0.

    IF (AP(1) .LT. AP(N)) THEN
        CALL TDMA_R(X,AP,AE,AW,BP,N)
    ELSE
    !  This "fix" (on the next line) is only used as a last resort
    !  The if-statement will catch the very unusual situation where both
    !  RHOSBxx(N-1)=0.   AND     RHOSFxx(1)=0.
        IF (AP(1) .LT. 0.0001d0) AP(1)=0.0001d0
        CALL TDMA(X,AP,AE,AW,BP,N)
    ENDIF
    RETURN
END SUBROUTINE AUTOTDMA

SUBROUTINE ASHWAT_OffNormalProperties(L, THETA, OMEGA_V, OMEGA_H, LSWP_ON)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns off-normal properties (total solar, beam-beam and beam diffuse) given
          ! direct-normal, total solar, beam-beam and beam diffuse properties of layers
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN) :: L   ! layer for which to derive off-normal properties
                                        !   Used: LTYPE, SWP_EL, geometry
                                        !   Note: not altered (return is in LSWP_ON)
    REAL(r64), INTENT( IN) :: THETA     ! solar beam angle of incidence, from normal, radians
                                        !    0 <= THETA <= PI/2
    REAL(r64), INTENT( IN) :: OMEGA_V   ! solar beam vertical profile angle, +=above horizontal, radians
                                        !   = solar elevation angle for a vertical wall with
                                        !     wall-solar azimuth angle equal to zero
    REAL(r64), INTENT( IN) :: OMEGA_H   ! solar beam horizontal profile angle, +=clockwise when viewed
                                        !   from above (radians)
                                        !   = wall-solar azimuth angle for a vertical wall
                                        !     Used for PD and vertical VB
    TYPE (CFSSWP), INTENT( OUT) :: LSWP_ON  ! returned: off-normal properties
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL :: OKAY
          ! Flow

    LSWP_ON = L%SWP_EL        ! init to normal properties
                              !  calls below modify in place

    IF (IsGlazeLayerX( L)) THEN
        ! specular glazing
        ! HBX note: ltyGZS here iff modelOption F=x; spectral cases elsewhere
        CALL Specular_SWP( LSWP_ON, THETA)
    ELSE IF (L%LTYPE == ltyVBHOR) THEN
        OKAY = VB_SWP( L, LSWP_ON, OMEGA_V)
    ELSE IF (L%LTYPE == ltyVBVER) THEN
        OKAY = VB_SWP( L, LSWP_ON, OMEGA_H)
    ELSE IF (L%LTYPE == ltyDRAPE) THEN
        OKAY = PD_SWP( L, LSWP_ON, OMEGA_V, OMEGA_H)
    ELSE IF (L%LTYPE == ltyROLLB) THEN
        OKAY = RB_SWP( L, LSWP_ON, THETA)
    ELSE IF (L%LTYPE == ltyINSCRN) THEN
        OKAY = IS_SWP( L, LSWP_ON, THETA)
    ELSE IF (L%LTYPE == ltyNONE .OR. L%LTYPE == ltyROOM) THEN
        ! none or room: do nothing
    ELSE
        ! placeholder for add'l non-specular layers
    ENDIF
    RETURN
END SUBROUTINE ASHWAT_OffNormalProperties

LOGICAL FUNCTION Specular_OffNormal(THETA, RAT_1MR, RAT_TAU)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns ratio of off-normal to normal of opetical properties.
          !
          ! METHODOLOGY EMPLOYED:
          !  Uses a reference glass property.
          !
          ! returns TRUE if RAT_TAU < 1 or RAT_1MR < 1 (and thus Specular_Adjust s/b called)
          !    else FALSE
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64),   INTENT(IN):: THETA         ! solar beam angle of incidence, from normal radians
                                            !    0 <= THETA <= PI/2
    REAL(r64),  INTENT(OUT):: RAT_1MR       ! returned: ratio of off-normal to normal solar (1-reflectance)
                                            !   NOTE: rhoAdj = 1-(1-rho)*RAT_1MR
    REAL(r64),  INTENT(OUT):: RAT_TAU       ! returned: ratio of off-normal to normal solar transmittance
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAU0, RHO0, THETA1, THETA2, TAU_ON, RHO_ON, TAU_A
    REAL(r64) :: RPERP, RPARL               ! interface reflectance with respect to perpendicular
                                            ! and parallel polarization components of solar radiation
    REAL(r64) ::  TAUPERP, TAUPARL, RHOPERP, RHOPARL
    REAL(r64) ::  N2                        ! reference refractive index for generating general off-normal
                                            !  curves for specular glazings
    REAL(r64) ::  KL                        ! extinction coefficient - thickness product, also used as a
                                            !  reference value to generate off-normal curves for specular layers
          ! Flow

    Specular_OffNormal = .TRUE.
    THETA1=ABS( THETA)
    IF (THETA1 > PiOvr2 - DegToRadians)  THEN
        ! theta > 89 deg
        RAT_TAU = 0.0d0
        RAT_1MR = 0.0d0
    ELSE IF (THETA1 >= DegToRadians) THEN
        ! theta >= 1 deg
        N2=1.526d0
        KL=55.d0 * 0.006d0
        TAU_A = EXP(-1.d0*KL)
        RPERP=((N2-1.d0)/(N2+1.d0))**2
        TAU0=TAU_A*(1.d0 -RPERP)*(1.d0 -RPERP) / (1.d0 -(RPERP*RPERP*TAU_A*TAU_A))
        RHO0=RPERP*(1.d0 +(TAU_A*TAU0))
        THETA2 = ASIN((SIN(THETA1))/N2)
        TAU_A = EXP(-1.d0 * KL/COS(THETA2))
        RPERP=((SIN(THETA2-THETA1))/(SIN(THETA2+THETA1)))**2
        RPARL=((TAN(THETA2-THETA1))/(TAN(THETA2+THETA1)))**2
        TAUPERP=TAU_A*(1.d0-RPERP)*(1.d0-RPERP) &
                       / (1.d0-(RPERP*RPERP*TAU_A*TAU_A))
        TAUPARL=TAU_A*(1.d0-RPARL)*(1.d0-RPARL) &
                    / (1.d0-(RPARL*RPARL*TAU_A*TAU_A))
        RHOPERP=RPERP*(1.d0+(TAU_A*TAUPERP))
        RHOPARL=RPARL*(1.d0+(TAU_A*TAUPARL))
        TAU_ON=(TAUPERP+TAUPARL)/2.d0
        RHO_ON=(RHOPERP+RHOPARL)/2.d0
        RAT_TAU=TAU_ON/TAU0
        RAT_1MR=(1.d0-RHO_ON)/(1.d0-RHO0)
    ELSE
        Specular_OffNormal = .FALSE.
        RAT_TAU = 1.0d0
        RAT_1MR = 1.0d0
    END IF
    RETURN
END FUNCTION Specular_OffNormal

SUBROUTINE Specular_SWP(SWP, OMEGA)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the off-normal solar properties calculation
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE (CFSSWP), INTENT(INOUT) :: SWP          ! short wave properties (adjusted in place)
    REAL(r64),        INTENT(IN) :: OMEGA        ! incident angle, radians
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RAT_1MR                         ! adjustment factors, see Specular_OffNormal()
    REAL(r64) :: RAT_TAU                         ! adjustment factors, see Specular_OffNormal()
    LOGICAL   :: Specular_OffNormalReturn = .TRUE.
          ! Flow

    Specular_OffNormalReturn = Specular_OffNormal( OMEGA, RAT_1MR, RAT_TAU)

    IF(Specular_OffNormalReturn) THEN
        CALL Specular_Adjust( SWP, RAT_1MR, RAT_TAU)
    END IF
    RETURN
END SUBROUTINE Specular_SWP

SUBROUTINE Specular_Adjust(SWP, RAT_1MR, RAT_TAU)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! adjusts the off-normal solar properties
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE (CFSSWP), INTENT(INOUT) :: SWP          ! short wave properties (adjusted in place)
    REAL(r64),        INTENT(IN) :: RAT_1MR      ! adjustment factors, see Specular_OffNormal()
    REAL(r64),        INTENT(IN) :: RAT_TAU      ! adjustment factors, see Specular_OffNormal()
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
    SWP%TAUSFBB = RAT_TAU * SWP%TAUSFBB
    SWP%TAUSBBB = RAT_TAU * SWP%TAUSBBB
    SWP%RHOSFBB = 1.d0 - RAT_1MR * (1.d0 - SWP%RHOSFBB)
    SWP%RHOSBBB = 1.d0 - RAT_1MR * (1.d0 - SWP%RHOSBBB)
    RETURN
END SUBROUTINE Specular_Adjust

SUBROUTINE Specular_RATDiff(RAT_1MRDiff, RAT_TAUDiff)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Returns property ratios for estimating diffuse properties.
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(OUT) :: RAT_1MRDiff
    REAL(r64), INTENT(OUT) :: RAT_TAUDiff
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64), SAVE :: X1MRDiff = -1.0d0
    REAL(r64), SAVE :: XTAUDiff = -1.0d0
    REAL(r64)       :: P( hipDIM)
          ! Flow

    IF (XTAUDiff < 0.0d0) THEN
        ! calculate and save on first call
        X1MRDiff = HEMINT( Specular_F, hipRHO, P)
        XTAUDiff = HEMINT( Specular_F, hipTAU, P)
    END IF
    RAT_TAUDiff = XTAUDiff
    RAT_1MRDiff = X1MRDiff
    RETURN
END SUBROUTINE Specular_RATDiff

REAL(r64) FUNCTION Specular_F(THETA, OPT, P)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! integrand fcn for specular properties.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: THETA      ! incidence angle, radians
    INTEGER,   INTENT(IN) :: OPT        ! options (unused)
                                        !   1: reflectance
                                        !   2: transmittance
    REAL(r64), INTENT(IN) :: P( hipDIM) ! parameters (none defined)
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)             :: RAT_TAU
    REAL(r64)             :: RAT_1MR
    LOGICAL               :: Specular_OffNormalReturn = .TRUE.
          ! Flow

    ! Modified by BAN April 19, 2013
    Specular_OffNormalReturn = Specular_OffNormal( THETA, RAT_1MR, RAT_TAU)

    IF (OPT == hipRHO) THEN
        Specular_F = RAT_1MR
    ELSE IF (OPT == hipTAU) THEN
        Specular_F = RAT_TAU
    ELSE
        Specular_F = -1.0d0
    ENDIF
    RETURN
END FUNCTION Specular_F

SUBROUTINE Specular_EstimateDiffuseProps(SWP)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Estimates diffuse-diffuse properties.
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSSWP), INTENT(INOUT) :: SWP        ! short wave properties
                                               ! sets diffuse members
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                    :: RAT_TAU
    REAL(r64)                    :: RAT_1MR
          ! Flow

    !#if 1
    CALL Specular_RATDiff( RAT_1MR, RAT_TAU)
    !#else
    !    ! estimate diffuse properties as 60 deg angle of incidence
    !    CALL Specular_RAT60( RAT_TAU, RAT_1MR)
    !#endif
    SWP%TAUS_DD = RAT_TAU * SWP%TAUSFBB
    SWP%RHOSFDD = 1.0d0 - RAT_1MR * (1.0d0 - SWP%RHOSFBB)
    SWP%RHOSBDD = 1.0d0 - RAT_1MR * (1.0d0 - SWP%RHOSBBB)
  RETURN
END SUBROUTINE Specular_EstimateDiffuseProps

LOGICAL FUNCTION RB_LWP(L, LLWP)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Modifies roller blind longwave properties. If not roller blind layer
          ! returns False.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER),  INTENT( IN) :: L       ! RB layer
    TYPE( CFSLWP), INTENT( INOUT) :: LLWP    ! returned: equivalent layer long wave properties
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAULX
    REAL(r64) :: OPENNESS

    RB_LWP = .FALSE.
    IF (L%LTYPE /= ltyROLLB) RETURN

    OPENNESS = L%SWP_MAT%TAUSFBB

    CALL OPENNESS_LW(OPENNESS, L%LWP_MAT%EPSLF, L%LWP_MAT%TAUL, &
        LLWP%EPSLF, LLWP%TAUL)

    CALL OPENNESS_LW(OPENNESS, L%LWP_MAT%EPSLB, L%LWP_MAT%TAUL, &
        LLWP%EPSLB, TAULX)

    RB_LWP = .TRUE.
    RETURN
END FUNCTION RB_LWP

LOGICAL FUNCTION RB_SWP(L, LSWP, THETA)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Modifies roller blind shortwave properties. If not roller blind layer
          !  returns False.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER),      INTENT(IN) :: L        ! RB layer
    TYPE( CFSSWP),     INTENT(INOUT) :: LSWP     ! returned: equivalent layer properties set
                                                 !   sets ONLY RHOSFDD, RHOSBDD, TAUS_DD
    REAL(r64), OPTIONAL, INTENT (IN) :: THETA    ! incident angle, 0 <= theta <= PI/2
                                                 !  if missing, derive diffuse properties
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    LOGICAL                          :: DODIFFUSE
    REAL(r64)                        :: RHOBF_BT0
    REAL(r64)                        :: RHOFF_BT0
    REAL(r64)                        :: TAUBF_BT0
    REAL(r64)                        :: TAUFF_BT0
    REAL(r64)                        :: TAUX
          ! Flow

    RB_SWP = .FALSE.
    IF (L%LTYPE /= ltyROLLB) RETURN

    DODIFFUSE = .NOT. PRESENT( THETA)

    ! normal beam-total properties of fabric
    RHOFF_BT0 = L%SWP_MAT%RHOSFBB + L%SWP_MAT%RHOSFBD    ! front rho
    RHOBF_BT0 = L%SWP_MAT%RHOSBBB + L%SWP_MAT%RHOSBBD    ! back rho

    TAUFF_BT0 = L%SWP_MAT%TAUSFBB + L%SWP_MAT%TAUSFBD    ! front tau
    TAUBF_BT0 = L%SWP_MAT%TAUSBBB + L%SWP_MAT%TAUSBBD    ! back tau

    IF (DODIFFUSE) THEN
        ! front
        CALL RB_DIFF( RHOFF_BT0, TAUFF_BT0, L%SWP_MAT%TAUSFBB, &
                LSWP%RHOSFDD, LSWP%TAUS_DD)
        ! back
        CALL RB_DIFF( RHOBF_BT0, TAUBF_BT0, L%SWP_MAT%TAUSBBB, &
                LSWP%RHOSBDD, TAUX)
    ELSE
        CALL RB_BEAM( THETA, RHOFF_BT0, TAUFF_BT0, L%SWP_MAT%TAUSFBB, &
            LSWP%RHOSFBD, LSWP%TAUSFBB, LSWP%TAUSFBD)

        CALL RB_BEAM( THETA, RHOBF_BT0, TAUBF_BT0, L%SWP_MAT%TAUSBBB, &
            LSWP%RHOSBBD, LSWP%TAUSBBB, LSWP%TAUSBBD)

    END IF
    RB_SWP = .TRUE.
    RETURN
END FUNCTION RB_SWP

LOGICAL FUNCTION IS_LWP(L, LLWP)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Modifies Insect Screen longwave properties. If not Insect Screen layer
          !  returns False.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN)  :: L        ! IS layer
    TYPE( CFSLWP),   INTENT( OUT) :: LLWP    ! returned: equivalent layer long wave properties
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)    :: OPENNESS
    REAL(r64)    :: TAULX
          ! Flow

    IS_LWP = .FALSE.
    IF (L%LTYPE /= ltyINSCRN) RETURN

    OPENNESS = L%SWP_MAT%TAUSFBB

    CALL OPENNESS_LW(OPENNESS, L%LWP_MAT%EPSLF, L%LWP_MAT%TAUL, &
                     LLWP%EPSLF, LLWP%TAUL)

    CALL OPENNESS_LW(OPENNESS, L%LWP_MAT%EPSLB, L%LWP_MAT%TAUL,  &
                     LLWP%EPSLB, TAULX)
    IS_LWP = .TRUE.
    RETURN
END FUNCTION IS_LWP

LOGICAL FUNCTION IS_SWP(L, LSWP, THETA)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Modifies Insect Screen shortwave properties. If not Insect Screen layer
          ! returns False.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN) :: L            ! PD layer
    TYPE( CFSSWP),  INTENT( INOUT) :: LSWP       ! returned: equivalent layer properties set
                                                 !   sets ONLY RHOSFDD, RHOSBDD, TAUS_DD
    REAL(r64), OPTIONAL, INTENT (IN) :: THETA    ! incident angle, 0 <= theta <= PI/2
                                                 !  if missing, derive diffuse properties
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    LOGICAL   :: DODIFFUSE
    REAL(r64) :: RHOBF_BT0
    REAL(r64) :: RHOFF_BT0
    REAL(r64) :: TAUBF_BT0
    REAL(r64) :: TAUFF_BT0
    REAL(r64) :: TAUX
          ! Flow

    IS_SWP = .FALSE.
    IF (L%LTYPE /= ltyINSCRN) RETURN

    DODIFFUSE = .NOT. PRESENT( THETA)

    ! normal beam-total properties
    RHOFF_BT0 = L%SWP_MAT%RHOSFBB + L%SWP_MAT%RHOSFBD    ! front rho
    RHOBF_BT0 = L%SWP_MAT%RHOSBBB + L%SWP_MAT%RHOSBBD    ! back rho

    TAUFF_BT0 = L%SWP_MAT%TAUSFBB + L%SWP_MAT%TAUSFBD    ! front tau
    TAUBF_BT0 = L%SWP_MAT%TAUSBBB + L%SWP_MAT%TAUSBBD    ! back tau

    IF (DODIFFUSE) THEN

        ! front
        CALL IS_DIFF( RHOFF_BT0, TAUFF_BT0, L%SWP_MAT%TAUSFBB, &
                LSWP%RHOSFDD, LSWP%TAUS_DD)
        ! back
        CALL IS_DIFF( RHOBF_BT0, TAUBF_BT0, L%SWP_MAT%TAUSBBB, &
                LSWP%RHOSBDD, TAUX)
    ELSE
        ! front
        CALL IS_BEAM( THETA, RHOFF_BT0, TAUFF_BT0, L%SWP_MAT%TAUSFBB, &
            LSWP%RHOSFBD, LSWP%TAUSFBB, LSWP%TAUSFBD)

        ! back -- call with reverse material properies
        CALL IS_BEAM( THETA, RHOBF_BT0, TAUBF_BT0, L%SWP_MAT%TAUSBBB, &
            LSWP%RHOSBBD, LSWP%TAUSBBB, LSWP%TAUSBBD)

    END IF
    IS_SWP = .TRUE.
    RETURN
END FUNCTION IS_SWP

SUBROUTINE Fabric_EstimateDiffuseProps(SWP)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Estimates diffuse properties of drape fabrics.
          ! sets RHOSFDD, RHOSBDD, TAUS_DD
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSSWP), INTENT( INOUT) :: SWP    ! fabric short wave properties
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RHOBF_BT0                  ! total back reflectance
    REAL(r64) :: RHOFF_BT0                  ! total front reflectance
    REAL(r64) :: TAUBF_BT0                  ! total back transmittance
    REAL(r64) :: TAUFF_BT0                  ! total front transmittance
    REAL(r64) :: TAUX
          ! flow

    RHOFF_BT0 = SWP%RHOSFBB + SWP%RHOSFBD   ! front rho
    RHOBF_BT0 = SWP%RHOSBBB + SWP%RHOSBBD   ! back rho
    TAUFF_BT0 = SWP%TAUSFBB + SWP%TAUSFBD   ! front tau
    TAUBF_BT0 = SWP%TAUSBBB + SWP%TAUSBBD   ! back tau
    !
    CALL FM_DIFF( RHOFF_BT0, TAUFF_BT0, SWP%TAUSFBB, SWP%RHOSFDD, SWP%TAUS_DD)
    CALL FM_DIFF( RHOBF_BT0, TAUBF_BT0, SWP%TAUSBBB, SWP%RHOSBDD, TAUX)
    !
    RETURN
END SUBROUTINE Fabric_EstimateDiffuseProps

LOGICAL FUNCTION PD_LWP(L, LLWP)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Modifies Drape longwave properties for openness. If not Drape Fabric layer
          !  returns False.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN) :: L         ! PD layer
    TYPE( CFSLWP),  INTENT( INOUT) :: LLWP    ! returned: equivalent layer long wave properties
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: TAULX
    REAL(r64) :: OPENNESS_FABRIC
          ! Flow

    PD_LWP = .FALSE.
    IF (L%LTYPE /= ltyDRAPE) RETURN

    OPENNESS_FABRIC = L%SWP_MAT%TAUSFBB

    CALL PD_LW( L%S, L%W, OPENNESS_FABRIC, L%LWP_MAT%EPSLF, L%LWP_MAT%EPSLB, L%LWP_MAT%TAUL, &
        LLWP%EPSLF, LLWP%TAUL)

    CALL PD_LW( L%S, L%W, OPENNESS_FABRIC, L%LWP_MAT%EPSLB, L%LWP_MAT%EPSLF, L%LWP_MAT%TAUL, &
        LLWP%EPSLB, TAULX)

    PD_LWP = .TRUE.
    RETURN
END FUNCTION PD_LWP

LOGICAL FUNCTION PD_SWP(L, LSWP, OHM_V_RAD, OHM_H_RAD)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Modifies drape fabric shortwave properties for openness. If not drape Fabric layer
          ! returns false. If profile angles not specified diffuse properties are returned.
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          !  na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER),      INTENT(IN) :: L           ! PD layer
    TYPE( CFSSWP),     INTENT(INOUT) :: LSWP        ! returned: equivalent layer properties set
    REAL(r64), OPTIONAL, INTENT (IN) :: OHM_V_RAD   ! vertical VB profile angles, radians
    REAL(r64), OPTIONAL, INTENT (IN) :: OHM_H_RAD   ! horizonatl VB profile angles, radians
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    LOGICAL   :: DODIFFUSE
    REAL(r64) :: RHOBF_BT0
    REAL(r64) :: RHOFF_BT0
    REAL(r64) :: TAUBF_BT0
    REAL(r64) :: TAUFF_BT0
    REAL(r64) :: TAUX
          ! Flow

    PD_SWP = .FALSE.
    IF (.NOT.L%LTYPE == ltyDRAPE) RETURN

    DODIFFUSE = .NOT.( PRESENT( OHM_V_RAD) .AND. PRESENT( OHM_H_RAD))

    IF (DODIFFUSE) THEN
        CALL PD_DIFF( L%S, L%W, L%SWP_MAT%RHOSFDD, L%SWP_MAT%RHOSBDD, L%SWP_MAT%TAUS_DD, &
            LSWP%RHOSFDD, LSWP%TAUS_DD)

        CALL PD_DIFF( L%S, L%W, L%SWP_MAT%RHOSBDD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
            LSWP%RHOSBDD, TAUX)
    ELSE
        ! normal beam-total properties of fabric
        RHOFF_BT0 = L%SWP_MAT%RHOSFBB + L%SWP_MAT%RHOSFBD    ! front rho
        RHOBF_BT0 = L%SWP_MAT%RHOSBBB + L%SWP_MAT%RHOSBBD    ! back rho

        ! drape front properties
        CALL PD_BEAM( L%S, L%W, OHM_V_RAD, OHM_H_RAD, &
            RHOFF_BT0, L%SWP_MAT%TAUSFBB, L%SWP_MAT%TAUSFBD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
            RHOBF_BT0, L%SWP_MAT%TAUSBBB, L%SWP_MAT%TAUSBBD, L%SWP_MAT%RHOSBDD, L%SWP_MAT%TAUS_DD, &
            LSWP%RHOSFBD, LSWP%TAUSFBB, LSWP%TAUSFBD)

        ! drape back properties: call with reversed fabric properies
        CALL PD_BEAM( L%S, L%W, OHM_V_RAD, OHM_H_RAD, &
            RHOBF_BT0, L%SWP_MAT%TAUSBBB, L%SWP_MAT%TAUSBBD, L%SWP_MAT%RHOSBDD, L%SWP_MAT%TAUS_DD, &
            RHOFF_BT0, L%SWP_MAT%TAUSFBB, L%SWP_MAT%TAUSFBD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
            LSWP%RHOSBBD, LSWP%TAUSBBB, LSWP%TAUSBBD)

    ENDIF
    PD_SWP = .TRUE.
    RETURN
END FUNCTION PD_SWP

LOGICAL FUNCTION VB_LWP(L, LLWP)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Return venetian blind longwave properties from slat properties and geometry.
          ! If not VB layer returns False.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER),   INTENT(IN) :: L       ! VB layer
    TYPE( CFSLWP),  INTENT(INOUT) :: LLWP    ! returned: equivalent layer long wave properties
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RHODFS_SLAT
    REAL(r64) :: RHOUFS_SLAT
    REAL(r64) :: RHOLF
    REAL(r64) :: RHOLB
    REAL(r64) :: TAULX
          ! Flow

    VB_LWP = .FALSE.
    IF (.NOT.IsVBLayer( L)) RETURN

    ! slat reflectances
    RHODFS_SLAT = 1. - L%LWP_MAT%EPSLB - L%LWP_MAT%TAUL    ! downward surface
    RHOUFS_SLAT = 1. - L%LWP_MAT%EPSLF - L%LWP_MAT%TAUL    ! upward surface

    ! TODO: are there cases where 2 calls not needed (RHODFS_SLAT == RHOUFS_SLAT??)
    CALL VB_DIFF( L%S, L%W, DegToRadians*L%PHI_DEG, RHODFS_SLAT, RHOUFS_SLAT, L%LWP_MAT%TAUL, &
        RHOLF, LLWP%TAUL)
    LLWP%EPSLF = 1. - RHOLF - LLWP%TAUL

    CALL VB_DIFF( L%S, L%W, -DegToRadians*L%PHI_DEG, RHODFS_SLAT, RHOUFS_SLAT, L%LWP_MAT%TAUL, &
        RHOLB, TAULX)
    LLWP%EPSLB = 1. - RHOLB - LLWP%TAUL

    VB_LWP = .TRUE.
    RETURN
END FUNCTION VB_LWP

LOGICAL FUNCTION VB_SWP(L, LSWP, OMEGA)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns venetian blind off-normal short wave properties. If not VB layer
          ! returns False.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER),     INTENT(IN) :: L       ! VB layer
    TYPE( CFSSWP),    INTENT(INOUT) :: LSWP    ! returned: equivalent off-normal properties
                                               !   sets: RHOSFBD, TAUSFBB, TAUSFBD
    REAL(r64), OPTIONAL, INTENT(IN) :: OMEGA   ! incident profile angle (radians)
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: SL_WR
    REAL(r64) :: TAUX
    LOGICAL   :: DODIFFUSE
          ! Flow

    VB_SWP = .FALSE.
    IF (.NOT.IsVBLayer( L)) RETURN

    SL_WR = VB_SLAT_RADIUS_RATIO( L%W, L%C)

    DODIFFUSE = .NOT. PRESENT( OMEGA)

    IF (DODIFFUSE) THEN

        CALL VB_DIFF( L%S, L%W, DegToRadians*L%PHI_DEG,    &
            L%SWP_MAT%RHOSBDD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
            LSWP%RHOSFDD, LSWP%TAUS_DD)

        CALL VB_DIFF( L%S, L%W, -DegToRadians*L%PHI_DEG, &
            L%SWP_MAT%RHOSBDD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
            LSWP%RHOSBDD, TAUX)
    ELSE
        ! modify angle-dependent values for actual profile angle
        CALL VB_SOL46_CURVE( L%S, L%W, SL_WR, DegToRadians*L%PHI_DEG, OMEGA, &
                L%SWP_MAT%RHOSBDD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
                LSWP%RHOSFBD, LSWP%TAUSFBB, LSWP%TAUSFBD)

        CALL VB_SOL46_CURVE( L%S, L%W, SL_WR, -DegToRadians*L%PHI_DEG, OMEGA, &
                L%SWP_MAT%RHOSBDD, L%SWP_MAT%RHOSFDD, L%SWP_MAT%TAUS_DD, &
                LSWP%RHOSBBD, LSWP%TAUSBBB, LSWP%TAUSBBD)
    END IF
    VB_SWP = .TRUE.
    RETURN
END FUNCTION VB_SWP

LOGICAL FUNCTION VB_ShadeControl(L, OMEGA_DEG)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Modifies slat angle if shade control is true. If not uses the fixed
          !  slate angle and returns false.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( INOUT) :: L          ! VB layer
    REAL(r64),          INTENT( IN) :: OMEGA_DEG  ! incident profile angle (degrees)
                                                  !   see comments elsewhere re sign convention
                                                  !   < 0 = diffuse
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: SLATA
          ! Flow

    SLATA = L%PHI_DEG

    IF (L%CNTRL == lscVBPROF) THEN
        ! slatA = profA (max gain)
        IF (OMEGA_DEG < 0.0d0) THEN
            SLATA = -30.0d0
        ELSE
            SLATA = -OMEGA_DEG
        END IF
    ELSE IF (L%CNTRL == lscVBNOBM) THEN
        ! slatA set to just exclude beam
        IF (OMEGA_DEG < 0.0d0) THEN
            SLATA = VB_CriticalSlatAngle( L, 30.0d0)    ! assume 30 deg for diffuse
        ELSE
            SLATA = VB_CriticalSlatAngle( L, OMEGA_DEG)
        END IF
    END IF

    VB_ShadeControl = ABS( SLATA - L%PHI_DEG) > 0.01d0
    IF (VB_ShadeControl) THEN
        L%PHI_DEG = SLATA
    END IF
    RETURN
END FUNCTION VB_ShadeControl
!
REAL(r64) FUNCTION VB_CriticalSlatAngle(L, OMEGA_DEG)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns slat angle that just excludes beam radiation.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN) :: L        ! VB layer
    REAL(r64), INTENT( IN) :: OMEGA_DEG      ! incident profile angle (degrees)
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: RAT
          ! Flow

    ! TODO handle vert blind cases etc
    RAT = L%S * COS( OMEGA_DEG) / L%W
    ! limit upward slat angle to horiz = max visibility
    VB_CriticalSlatAngle = MAX( 0.d0, RadiansToDeg * ASIN( RAT) - OMEGA_DEG)
    RETURN
END FUNCTION VB_CriticalSlatAngle

LOGICAL FUNCTION DoShadeControl(L, THETA, OMEGA_V, OMEGA_H)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns .TRUE. if L is modified for shade control.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT(INOUT):: L        ! layer (returned updated)
    REAL(r64),          INTENT(IN):: THETA    ! solar beam angle of incidence, from normal, (radians)
                                              ! 0 <= THETA <= PI/2
    REAL(r64),          INTENT(IN):: OMEGA_V  ! solar beam vertical profile angle, +=above horizontal (radians)
                                              !   = solar elevation angle for a vertical wall with
                                              !     wall-solar azimuth angle equal to zero
    REAL(r64),          INTENT(IN):: OMEGA_H  ! solar beam horizontal profile angle, +=clockwise when viewed
                                              !   from above (radians)
                                              !   = wall-solar azimuth angle for a vertical wall
                                              !     Used for PD and vertical VB
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: OMEGA_DEG        ! controlling profile angel, degrees
          ! Flow

    DoShadeControl = .FALSE.      ! default: no shade controls implemented

    ! must be consistent with IsControlledShade()
    IF (IsVBLayer( L) .AND. L%CNTRL /= lscNONE) THEN
        IF (THETA < 0.0d0 .OR. THETA >= PiOvr2) THEN
            OMEGA_DEG = -1.        ! diffuse only
        ELSE IF (L%LTYPE == ltyVBHOR) THEN
            ! horiz VB
            OMEGA_DEG = RadiansToDeg * OMEGA_V
        ELSE
            ! vert VB
            OMEGA_DEG = RadiansToDeg * OMEGA_H
        ENDIF
        IF (VB_ShadeControl( L, OMEGA_DEG) ) THEN
            CALL FinalizeCFSLAYER( L)
            DoShadeControl = .TRUE.
        END IF
    END IF
    RETURN
END FUNCTION DoShadeControl
!
SUBROUTINE FinalizeCFSLAYER(L)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          !  Sets equivalent layer properties of a construction.
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( INOUT) :: L    !   layer, input: LTYPE, LWP_MAT, SWP_MAT
                                            !          geometry (per LTYPE)
                                            !   output: LWP_EL, SWP_EL
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL :: LOK
    LOGICAL :: DOK
    LOGICAL :: BOK
    LOGICAL :: CFSLAYERFlag
          ! Flow

    LOK = .FALSE.
    DOK = .FALSE.
    BOK = .FALSE.

    IF (IsVBLayer( L)) THEN
        LOK = VB_LWP( L, L%LWP_EL)
        DOK = VB_SWP( L, L%SWP_EL)          ! SW diffuse
        BOK = VB_SWP( L, L%SWP_EL, 0.0d0)   ! SW properties w/ profile ang = 0
    ELSE
        L%PHI_DEG = 0.0d0                     ! phi, C, CNTRL are VB only
        L%C = 0.0d0
        L%CNTRL = lscNONE
        IF (L%LTYPE == ltyDRAPE) THEN
            LOK = PD_LWP( L, L%LWP_EL)
            DOK = PD_SWP( L, L%SWP_EL)      ! SW diffuse
            BOK = PD_SWP( L, L%SWP_EL, 0.0d0, 0.0d0) ! SW properties w/ profile angs = 0
        ELSE IF (L%LTYPE == ltyINSCRN) THEN
            LOK = IS_LWP( L, L%LWP_EL)      ! LW
            DOK = IS_SWP( L, L%SWP_EL)      ! SW diffuse
            BOK = IS_SWP( L, L%SWP_EL, 0.0d0) ! SW beam w/ theta = 0
        ELSE
            L%S = 0.0d0                       ! geometry mbrs unused
            L%W = 0.0d0
            IF (L%LTYPE == ltyROLLB) THEN
                LOK = RB_LWP( L, L%LWP_EL)  ! LW
                DOK = RB_SWP( L, L%SWP_EL)  ! SW diffuse
                BOK = RB_SWP( L, L%SWP_EL, 0.0d0)    ! SW beam w/ theta = 0
            !ELSE IF (ISGZSLayer( L)) THEN
            ! spectral glazing. Set layer xxx_MAT from GZS file data
            !    BOK = GZSLayerInit( L) .EQ. 0
            !    L%SWP_EL = L%SWP_MAT
            !    L%LWP_EL = L%LWP_MAT
            !    LOK = .TRUE.
            !    DOK = .TRUE.
            ELSE
                ! glazing
                L%SWP_EL = L%SWP_MAT
                L%LWP_EL = L%LWP_MAT
                LOK = .TRUE.
                DOK = .TRUE.
                BOK = .TRUE.
            END IF
        END IF
    END IF
    CFSLAYERFlag = LOK .AND. DOK .AND. BOK
    RETURN
END SUBROUTINE FinalizeCFSLAYER

LOGICAL FUNCTION IsGZSLayer(L)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns .TRUE. if Layer has glazing data from external file or returns .FALSE.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN) :: L
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
    IsGZSLayer = L%LTYPE == ltyGZS
    RETURN
END FUNCTION IsGZSLayer

LOGICAL FUNCTION IsGlazeLayerX(L)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns .TRUE. if Layer has glazing (including GZS) or returns .FALSE.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT( IN) :: L
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
    IsGlazeLayerX = L%LTYPE == ltyGLAZE .OR. IsGZSLayer(L)
    RETURN
END FUNCTION IsGlazeLayerX

LOGICAL FUNCTION IsControlledShade(L)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns .TRUE. if Layer is Venetian blind and is controlled or returns .FALSE.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
TYPE( CFSLAYER), INTENT( IN) :: L
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
    IsControlledShade = IsVBLayer(L) .AND. L%CNTRL /= lscNONE
    RETURN
END FUNCTION IsControlledShade

LOGICAL FUNCTION IsVBLayer(L)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         JOHN L. WRIGHT, University of Waterloo, Mechanical Engineering
          !                      Advanced Glazing System Laboratory
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns .TRUE. if Layer is Venetian blind, or returns .FALSE.
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          ! na
          !
    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
TYPE( CFSLAYER), INTENT( IN) :: L
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    IsVBLayer = L%LTYPE == ltyVBHOR .OR. L%LTYPE == ltyVBVER
    RETURN
END FUNCTION IsVBLayer
!
SUBROUTINE BuildGap(G, GType, TAS, xTMan, xPMan)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse, June 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! fills in the effective gap thickness and calculates the gas density
          ! The gas density is calculated at a standard manufactuered condition
          ! if a different condition is not specified.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSGAP),    INTENT(INOUT):: G       ! returned
    INTEGER,             INTENT(IN):: GType   ! gap type (gtyOPENin, gtyOPENout or gtySEALED)
    REAL(r64),        INTENT(INOUT):: TAS     ! gap thickness, m
    REAL(r64), OPTIONAL, INTENT(IN):: xTMan   ! re density calc -- temp (C) and pressure (Pa)
    REAL(r64), OPTIONAL, INTENT(IN):: xPMan   ! re density calc -- temp (C) and pressure (Pa)
                                              ! at time of manufacture, default = 21 C / 1 ATM

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64)                      :: GapThickMin = 0.0001d0  ! Minimum gap thickness allowed, m
    CHARACTER(len=*),    PARAMETER :: RoutineName = 'BuildGap: '
          !
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                      :: PMan
    REAL(r64)                      :: TMan
          ! Flow

    IF ( TAS < GapThickMin ) THEN
        CALL ShowSevereError(RoutineName//TRIM(G%Name) )
        CALL ShowContinueError('...specified gap thickness is < 0.0001 m.  Reset to 0.00001 m')
        TAS = GapThickMin
    ENDIF
    G%TAS = TAS
    G%TAS_EFF = G%TAS
    ! effective gap thickness will be adjusted later if there is in between
    ! venetian blind, see AdjustVBGap() routine

    G%GType = GType
    TMan = 21.0d0
    IF (PRESENT( xTMan)) TMan = xTMan
    PMan = PAtmSeaLevel
    IF (PRESENT( xPMan)) PMan = xPMan

    G%RHOGAS = DensityCFSFILLGAS( G%FG, PMan, TMan+KelvinConv)

    RETURN
END SUBROUTINE BuildGap

SUBROUTINE AdjustVBGap(G, L)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Adjusts thickness of adjacent gaps seperated by
          ! in between slatted blind.

          ! METHODOLOGY EMPLOYED:
          ! Treat VB layer as if it has 70% of actual thickness

          ! REFERENCES:
          !  Wright, J. L., N. Y. T. Huang, and M. R. Collins.  2008.
          !  "Thermal Resistance of a Window with an Enclosed Venetian Blind: A Simplified Model,"
          !  ASHRAE Transactions, Vol. 114, Pt. 1.

          ! USE STATEMENTS:
          ! na
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSGAP), INTENT( INOUT) :: G        ! gap, returned updated
    TYPE( CFSLAYER),  INTENT( IN) :: L        ! adjacent layer

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                     :: VBTHICK
          ! Flow

    IF (.NOT. IsVBLayer( L)) RETURN        ! insurance

    VBTHICK = L%W * COS( L%PHI_DEG)       ! VB layer thickness at slat angle
    G%TAS_EFF = G%TAS + (L%W - 0.7d0 * VBTHICK) / 2.d0
    RETURN
END SUBROUTINE AdjustVBGap

REAL FUNCTION DensityCFSFillGas(FG, P, T)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns gas density at P and T, kg/m3
          !
          ! METHODOLOGY EMPLOYED:
          ! Uses ideal gas relations
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE (CFSFILLGAS), INTENT( IN):: FG        ! gas properties
    REAL(r64),         INTENT( IN):: P        ! pressure, Pa
    REAL(r64),         INTENT( IN):: T        ! temperature, K
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow
    DensityCFSFillGas = (P * FG%MHAT)/(UniversalGasConst * MAX(T, 1.d0))

    RETURN
END FUNCTION DensityCFSFillGas

INTEGER FUNCTION CFSNGlz(FS)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns the number of glazing layers
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSTY), INTENT( IN) :: FS        ! CFS
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER :: iL
          ! Flow

    CFSNGlz = 0
    DO iL = 1, FS%NL
        IF (IsGlazeLayerX( FS%L( iL))) THEN
            CFSNGlz = CFSNGlz + 1
        END IF
    END DO
END FUNCTION CFSNGlz

INTEGER FUNCTION CFSHasControlledShade( FS)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns index of the controlled layer in a fenestratio. If no
          ! controlled layer, then returns zero.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSTY), INTENT( IN) :: FS
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    INTEGER                   :: iL
          ! Flow

    CFSHasControlledShade = 0
    DO iL = 1, FS%NL
        IF (IsControlledShade( FS%L( iL))) THEN
            CFSHasControlledShade = iL
            EXIT
        END IF
    END DO
END FUNCTION CFSHasControlledShade

SUBROUTINE CheckAndFixCFSLayer(Layer )
          !
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          ! Verify CFS layer validity, sets bad items to valid defaults if possible
          !
          !
          ! METHODOLOGY EMPLOYED:
          !  na
          !
          ! REFERENCES:
          !  na
          !
          ! USE STATEMENTS:
          !  na
          !
          !
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    TYPE( CFSLAYER), INTENT(INOUT) :: Layer
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na
          !
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
          !
          ! Flow

    CALL FillDefaultsSWP(Layer, Layer%SWP_MAT)
    CALL FinalizeCFSLAYER(Layer)

    RETURN
END SUBROUTINE CheckAndFixCFSLayer
!
SUBROUTINE FillDefaultsSWP(L, SWP)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         The University of WaterLoo
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse/FSEC, June 2013
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fills in defaulted short wave optical properties for equivalent window
          ! layers

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE
    TYPE( CFSLAYER),  INTENT( IN) :: L      ! CFSLayer (input properties must be set)
    TYPE( CFSSWP), INTENT( INOUT) :: SWP    ! properties to fill
                                            ! may be within L
    CHARACTER (len=*),  PARAMETER :: RoutineName = 'FillDefaultsSWP: '
    LOGICAL :: OK
    LOGICAL                       :: ErrorsFound
          ! Flow

    ! default back taus to front (often equal)
    IF (SWP%TAUSBBB < 0.0d0) SWP%TAUSBBB = SWP%TAUSFBB
    IF( SWP%TAUSBBD < 0.0d0) SWP%TAUSBBD = SWP%TAUSFBD

    IF (L%LTYPE == ltyGLAZE) THEN
        ! estimate diffuse properties if any < 0 or autocalculate
        IF (MIN( SWP%RHOSBDD, SWP%RHOSFDD, SWP%TAUS_DD) < 0.0d0) THEN
            CALL Specular_EstimateDiffuseProps( SWP)
        END IF
    ELSE IF (L%LTYPE == ltyVBHOR .OR. L%LTYPE == ltyVBVER) THEN


    ELSE IF (L%LTYPE == ltyDRAPE) THEN
        ! estimate diffuse properties if any < 0
        IF (MIN( SWP%RHOSBDD, SWP%RHOSFDD, SWP%TAUS_DD) < 0.0d0) THEN
            CALL Fabric_EstimateDiffuseProps( SWP)
        END IF
    ELSE IF (L%LTYPE == ltyROLLB) THEN
        ! estimate diffuse properties if any < 0
        IF (MIN( SWP%RHOSBDD, SWP%RHOSFDD, SWP%TAUS_DD) < 0.0d0) THEN
            OK = RB_SWP( L, SWP)    ! TODO RB
        END IF
    ELSE IF (L%LTYPE == ltyINSCRN) THEN
        IF (SWP%TAUSFBB < 0.0d0) THEN
            SWP%TAUSFBB = IS_OPENNESS( L%S, L%W)
            IF (SWP%TAUSBBB < 0.0d0) SWP%TAUSBBB = SWP%TAUSFBB
        END IF
        IF (MIN( SWP%RHOSBDD, SWP%RHOSFDD, SWP%TAUS_DD) < 0.0d0) THEN
            OK = IS_SWP( L, SWP)    ! TODO IS
        END IF
    ELSE IF (L%LTYPE == ltyNONE .OR. L%LTYPE == ltyROOM) THEN
        ! none or room: do nothing
    ELSE
        CALL ShowSevereError( RoutineName//trim(L%Name)//'.')
        CALL ShowContinueError('...invalid layer type specified.')

    END IF
    RETURN
END SUBROUTINE FillDefaultsSWP
!
SUBROUTINE FinalizeCFS(FS)
          !
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         The University of WaterLoo
          !       DATE WRITTEN   unknown
          !       MODIFIED       Bereket Nigusse/FSEC, May 2013
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Complete CFS after BuildCFS by checking the shade type and
          ! gap type

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE
          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSTY),          INTENT( INOUT) :: FS
          !
          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='FinalizeCFS: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: iL
    INTEGER :: gType
    LOGICAL :: LVBPREV
    CHARACTER (len=MaxNameLength)    :: CurrentModuleObject
    LOGICAL                          :: ErrorsFound
          ! Flow

    CurrentModuleObject = 'WindowConstruction:EquivalentLayer'
    ErrorsFound = .FALSE.

    LVBPREV = .FALSE.        ! .TRUE. if previous layer is VB

    DO iL = 1, FS%NL
        IF (.NOT. IsVBLayer( FS%L( iL))) THEN
            LVBPREV = .FALSE.
        ELSE IF (LVBPREV) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(FS%Name)//'", illegal.')
            CALL ShowContinueError('...adjacent VB layers are specified.')
            ErrorsFound=.true.
        ELSE
            LVBPREV = .TRUE.
            IF (IL > 1)     CALL AdjustVBGap( FS%G( iL-1), FS%L( iL))
            IF (IL < FS%NL) CALL AdjustVBGap( FS%G( iL),   FS%L( iL))
        END IF
        IF (iL < FS%NL) THEN
            gType = FS%G( iL)%GTYPE
            IF (gType == gtyOPENOut .AND. iL /= 1) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(FS%Name))
                CALL ShowContinueError('...invalid EquivalentLayer window gap type specified ='//trim(FS%G(iL)%Name)//'.')
                CALL ShowContinueError('...VentedOutDoor gap is not outermost.')
            END IF
            IF (gType == gtyOPENIn .AND. iL /= FS%NL-1) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//trim(FS%Name))
                CALL ShowContinueError('...invalid EquivalentLayer window gap type specified ='//trim(FS%G(iL)%Name)//'.')
                CALL ShowContinueError('...VentedIndoor gap is not innermost.')
            END IF
        END IF
    END DO
    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Program terminates for preceding reason(s).')
    ENDIF

    RETURN
END SUBROUTINE FinalizeCFS
!
REAL(r64) FUNCTION EffectiveEPSLF(FS)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         <unknown>, ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns effective outside Longwave emissivity. Handles partially
          ! transparent layers
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSTY), INTENT( IN) :: FS   ! Complex Fenestration
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                 :: E    ! Effective emissivity
    REAL(r64)                 :: TX   ! correction factor
    INTEGER                   :: iL   ! layers index
          ! Flow

    E = 0.0d0
    TX = 1.0d0
    DO iL = 1, FS%NL+1
        IF (iL == FS%NL+1) THEN
            E = E + 0.9d0 * TX
        ELSE
            E = E + FS%L( iL)%LWP_EL%EPSLF * TX
            IF (FS%L( iL)%LWP_EL%TAUL < 0.001d0) EXIT
            TX = TX * FS%L( iL)%LWP_EL%TAUL
        END IF
    END DO
    EffectiveEPSLF = E
    RETURN
END FUNCTION EffectiveEPSLF

REAL(r64) FUNCTION EffectiveEPSLB(FS)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         <unknown>, ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns effective inside (room side) Longwave emissivity. Handles partially
          ! transparent layers
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    TYPE( CFSTY), INTENT( IN) :: FS   ! Complex Fenestration
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                 :: E    ! Effective emissivity
    REAL(r64)                 :: TX   ! correction factor
    INTEGER                   :: iL   ! layers index
          ! Flow

    E = 0.0d0
    TX = 1.0d0
    DO iL = FS%NL, 0, -1
        IF (iL == 0) THEN
            E = E + 0.9d0 * TX
        ELSE
            E = E + FS%L( iL)%LWP_EL%EPSLB * TX
            IF (FS%L( iL)%LWP_EL%TAUL < 0.001d0) EXIT
            TX = TX * FS%L( iL)%LWP_EL%TAUL
        END IF
    END DO
    EffectiveEPSLB = E
    RETURN
END FUNCTION EffectiveEPSLB

LOGICAL FUNCTION FEQX(a, b, tolF, tolAbs)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         <unknown>, ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns true if the difference between two real numbers is within the
          ! tolerance limit specified.
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: a, b, tolF  ! values to compare, fractional tolerance
    REAL(r64),   OPTIONAL :: tolAbs      ! absolute tolerance
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: d
    REAL(r64) :: tolAbsX
          ! Flow

    IF (PRESENT( tolAbs)) THEN
        tolAbsX = MAX( tolAbs, 1.d-10)
    ELSE
        tolAbsX = 1.d-10
    END IF
    d = ABS( a - b);
    IF (d < tolAbsX) THEN
        FEQX = .TRUE.
    ELSE
        FEQX = (2.d0 * d / (ABS( a) + ABS( b))) < tolF
    END IF
    RETURN
END FUNCTION FEQX

REAL(r64) FUNCTION TRadC(J, Emiss)
          !
          ! FUNCTION INFORMATION:
          !       AUTHOR         <unknown>, ASHRAE 1311-RP
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS FUNCTION:
          ! Returns equivalent celsius scale temperature from radiosity
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! na
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: J            ! radiosity, W/m2
    REAL(r64), INTENT( IN) :: Emiss        ! surface emissivity
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          !
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! Flow
    TRadC = (  (J / (StefanBoltzmann * MAX( Emiss, 0.001d0)))**0.25d0) - KelvinConv
    RETURN
END FUNCTION TRadC

SUBROUTINE CalcEQLOpticalProperty(SurfNum, BeamDIffFlag, CFSAbs)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the system optical properties from the individual layers
          ! properties at each time step. The values returned are the layer-by-layer
          ! absorptance and system transmittance for both beam and diffuse radiation.

          ! METHODOLOGY EMPLOYED:
          ! Uses the net radiation method developed for ASHWAT fenestration
          ! model (ASHRAE RP-1311) by John Wright, the University of WaterLoo

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,    ONLY: SOLCOS
  USE DaylightingManager, ONLY: ProfileAngle

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: SurfNum
  INTEGER,   INTENT(IN)  :: BeamDIffFlag                 ! identifier index of diffuse and beam SW radiation
  REAL(r64), INTENT(OUT) :: CFSAbs(CFSMAXNL+1,2)         ! absorbed beam solar radiation by layers fraction

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !
  REAL(r64)          :: ProfAngHor            ! Solar profile angle (radians) for horizontal blind
  REAL(r64)          :: ProfAngVer            ! Solar profile angle (radians) for vertical blind
  REAL(r64)          :: IncAng                ! incident angle degree
  REAL(r64)          :: IncidAngle            ! = ACOS(SOLCOS(3))
  REAL(r64)          :: Abs1( CFSMAXNL+1, 2)  !
  INTEGER            :: Lay                   ! window layer index
  INTEGER            :: EQLNum                ! equivalent layer window construction index
  INTEGER            :: ConstrNum             ! construction index
  INTEGER            :: I                     ! index
  INTEGER            :: J                     ! index
         ! Flow

  CFSAbs = 0.0d0
  ProfAngHor = 0.0d0
  ProfAngVer = 0.0d0
  ConstrNum = Surface(SurfNum)%Construction
  EQLNum = Construct(Surface(SurfNum)%Construction)%EQLConsPtr

  IF ( BeamDIffFlag /= isDIFF) THEN
     IF(CosIncAng(SurfNum,HourOfDay,TimeStep) <= 0.0d0) RETURN

     DO Lay = 1, CFS(EQLNum)%NL
        IF ( ISVBLayer(CFS(EQLNum)%L(Lay)) )THEN
           IF (CFS(EQLNum)%L(Lay)%LTYPE == ltyVBHOR) THEN
               CALL ProfileAngle(SurfNum,SOLCOS,Horizontal,ProfAngHor)
           ELSEIF(CFS(EQLNum)%L(Lay)%LTYPE == ltyVBVER) THEN
               CALL ProfileAngle(SurfNum,SOLCOS,Vertical,ProfAngVer)
           ENDIF
        ENDIF
     END DO
     ! Incident angle
     IncAng = ACOS(CosIncAng(SurfNum,HourOfDay,TimeStep))
     CALL CalcEQLWindowOpticalProperty(CFS(EQLNum), BeamDIffFlag, Abs1, IncAng, ProfAngVer, ProfAngHor)
     CFSAbs(1:CFSMAXNL+1,1)= Abs1(1:CFSMAXNL+1, 1)
     CFSAbs(1:CFSMAXNL+1,2)= Abs1(1:CFSMAXNL+1, 2)
  ELSE
     IF ( EQLDiffPropFlag(EQLNum) ) THEN
         DO Lay = 1, CFS(EQLNum)%NL
            IF ( ISVBLayer(CFS(EQLNum)%L(Lay)) )THEN
               IF (CFS(EQLNum)%L(Lay)%LTYPE == ltyVBHOR) THEN
                   CALL ProfileAngle(SurfNum,SOLCOS,Horizontal,ProfAngHor)
               ELSEIF(CFS(EQLNum)%L(Lay)%LTYPE == ltyVBVER) THEN
                   CALL ProfileAngle(SurfNum,SOLCOS,Vertical,ProfAngVer)
               ENDIF
            ENDIF
         END DO
         CALL CalcEQLWindowOpticalProperty( CFS(EQLNum), BeamDIffFlag, Abs1, IncAng, ProfAngVer, ProfAngHor)
         CFSAbs(1:CFSMAXNL+1,:) = Abs1(1:CFSMAXNL+1, :)
         CFSDiffAbsTrans(EQLNum,1:CFSMAXNL+1,:) = Abs1(1:CFSMAXNL+1,:)
         Construct(ConstrNum)%TransDiff = Abs1(CFS(EQLNum)%NL+1, 1)
         Construct(ConstrNum)%AbsDiffFrontEQL(1:CFSMAXNL) = Abs1(1:CFSMAXNL,1)
         Construct(ConstrNum)%AbsDiffBackEQL(1:CFSMAXNL)  = Abs1(1:CFSMAXNL,2)
         Construct(ConstrNum)%ReflectSolDiffFront = CFS(EQLNum)%L(1)%SWP_EL%RHOSFDD
         Construct(ConstrNum)%ReflectSolDiffBack  = CFS(EQLNum)%L(CFS(EQLNum)%NL)%SWP_EL%RHOSBDD
         IF (.NOT. CFS(EQLNum)%ISControlled) EQLDiffPropFlag(EQLNum) = .FALSE.
     ELSE
         CFSAbs(1:CFSMAXNL+1,:) = CFSDiffAbsTrans(EQLNum,1:CFSMAXNL+1,:)
         Construct(ConstrNum)%TransDiff = CFSDiffAbsTrans(EQLNum,CFS(EQLNum)%NL+1,1)
         Construct(ConstrNum)%AbsDiffFrontEQL(1:CFSMAXNL) = CFSAbs(1:CFSMAXNL,1)
         Construct(ConstrNum)%AbsDiffBackEQL(1:CFSMAXNL)  = CFSAbs(1:CFSMAXNL,2)
     ENDIF
  ENDIF

  RETURN
END SUBROUTINE CalcEQLOpticalProperty

SUBROUTINE CalcEQLWindowStandardRatings(ConstrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the U-value, SHGC and Normal Transmittance of equivalent layer
          ! fenestration.

          ! METHODOLOGY EMPLOYED:
          ! Uses routine developed for ASHRAE RP-1311 (ASHWAT Model)
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER,   INTENT(IN)        :: ConstrNum ! construction index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64)                    :: UValue
    INTEGER                      :: EQLNum
    REAL(r64)                    :: SHGCSummer
    REAL(r64)                    :: TransNormal
          ! flow

    Uvalue = 0.d0
    SHGCSummer = 0.d0
    TransNormal = 0.d0

    EQLNum = Construct(ConstrNum)%EQLConsPtr

    ! calculate fenestration air-to-air U-value
    CALL CalcEQLWindowUvalue( CFS(EQLNum), UValue)
    NominalU(ConstrNum) = UValue

    ! calculate the SHGC and Normal Transmittance
    CALL CalcEQLWindowSHGCAndTransNormal( CFS(EQLNum), SHGCSummer, TransNormal)
    Construct(ConstrNum)%SummerSHGC = SHGCSummer
    Construct(ConstrNum)%SolTransNorm = TransNormal

    RETURN
END SUBROUTINE CalcEQLWindowStandardRatings

FUNCTION EQLWindowInsideEffectiveEmiss(ConstrNum) Result(InsideLWEmiss)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket A Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the consruction number, returns the equivalent layer inside
          ! face effective longwave emmisivity.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES: na
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ConstrNum
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: EQLNum        ! EQL Window object number
  REAL(r64)           :: InsideLWEmiss ! LW inside emissivity

          ! FLOW:
  EQLNum = Construct(ConstrNum)%EQLConsPtr
  InsideLWEmiss = EFFECTIVEEPSLB(CFS(EQLNum))

  RETURN
END FUNCTION EQLWindowInsideEffectiveEmiss

FUNCTION EQLWindowOutsideEffectiveEmiss(ConstrNum) Result(OutSideLWEmiss)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket A Nigusse
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given the consruction number, returns the equivalent layer outside
          ! face effective longwave emmisivity.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES: na
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ConstrNum
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: EQLNum         ! EQL Window object number
  REAL(r64)           :: OutSideLWEmiss ! LW outside emissivity

          ! FLOW:
  EQLNum = Construct(ConstrNum)%EQLConsPtr
  OutSideLWEmiss = EFFECTIVEEPSLF(CFS(EQLNum))

  RETURN
END FUNCTION EQLWindowOutsideEffectiveEmiss

FUNCTION HCInWindowStandardRatings(Height, TSurfIn, TAirIn) Result(hcin)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse
          !       DATE WRITTEN   June 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Return the inside convection coefficient for fenestration ratings.
          ! This procedure is adopted from WindowTempsForNominalCond routine.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! Uses ISO Standard 15099 method to calculate the inside surface
          ! convection coefficient for fenestration ratings.

          ! REFERENCES: na
          !
          ! USE STATEMENTS:
          !
    USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW
   IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Height   ! Window height, 1.0 m
    REAL(r64), INTENT(IN) :: TSurfIn  ! Inside surface temperature
    REAL(r64), INTENT(IN) :: TAirIn   ! Zone Air Temperature
          !
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64)  :: TmeanFilm       ! mean film temperature
    REAL(r64)  :: TmeanFilmKelvin !  mean film temperature for property evaluation
    REAL(r64)  :: rho      ! density of (apparently dry) air [kg/m3]
    REAL(r64)  :: Cp       ! specific heat of air [J/kg-K]
    REAL(r64)  :: lambda   ! thermal conductivity of air [W/m-K]
    REAL(r64)  :: mu       ! dynamic viscosity of air [kg/m-s]
    REAL(r64)  :: RaH      ! Rayleigh number for cavity height [ Non dim]
    REAL(r64)  :: tiltDeg  ! glazing tilt in degrees
    REAL(r64)  :: sineTilt ! sine of glazing tilt
    REAL(r64)  :: Nuint    ! Nusselt number for interior surface convection
    REAL(r64)  :: hcin     ! interior surface convection coefficient

    TiltDeg= 90.0D0
    sineTilt = SIN(tiltDeg*DegToRadians)  !degrees as arg

    ! Begin calculating for ISO 15099 method.
    ! mean film temperature
    TmeanFilmKelvin = TAirIn + 0.25D0*(TSurfIn - TAirIn) ! eq. 133 in ISO 15099
    TmeanFilm = TmeanFilmKelvin - 273.15D0
    ! the following properties are constants or linear relations for "standard" type reporting
    rho    = PsyRhoAirFnPbTdbW(101325.0D0, TmeanFilm, 0.0D0, 'HCInWindowStandardRatings') ! dry air assumption

    lambda = 2.873D-3 + 7.76D-5 * TmeanFilmKelvin     ! Table B.1 in ISO 15099
    mu     = 3.723D-6 + 4.94D-8 * TmeanFilmKelvin     ! Table B.2 in ISO 15099
    Cp     = 1002.737D0 + 1.2324D-2 * TmeanFilmKelvin ! Table B.3 in ISO 15099

    RaH = ( rho**2 * Height**3 * GravityConstant * Cp*(abs(TSurfIn - TAirIn)) ) &
            / (TmeanFilmKelvin * mu * lambda)         ! eq 132 in ISO 15099

    ! eq. 135 in ISO 15099 (only need this one because tilt is 90 deg)
    Nuint = 0.56D0*(RaH * sineTilt)**0.25D0
    hcin  = Nuint * lambda / Height

    RETURN
END FUNCTION HCInWindowStandardRatings
!
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

END MODULE WindowEquivalentLayer