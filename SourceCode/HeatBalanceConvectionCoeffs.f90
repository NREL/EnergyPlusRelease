MODULE ConvectionCoefficients

  ! Module containing the routines dealing with the convection coefficients

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   August 2000
  !       MODIFIED       Brent Griffith, August 2010 expanded model choices
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module contain the routines dealing with convection coefficients.
  ! This module collects correlations/calculations for both the interior and exterior
  ! Manages a portion of the input and calculations for Hc values for use in surface heat balances
  ! .

  ! METHODOLOGY EMPLOYED:
  !
  ! Subroutines are called to fill the variable HConvIn with the convection coefficient at
  ! the inside face.  or outside face for the current surface.

  ! REFERENCES:


  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataLoopNode
USE DataHeatBalance
USE DataSurfaces
USE DataInterfaces
USE DataVectorTypes
USE General, ONLY: RoundSigDigits

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER :: AdaptiveHcInsideLowLimit  = 0.5d0 ! W/m2-K
REAL(r64), PARAMETER :: AdaptiveHcOutsideLowLimit = 1.d0 ! W/m2-K
character(len=*), PARAMETER :: fmtx='(A,I4,1x,A,1x,6f16.8)'
character(len=*), PARAMETER :: fmty='(A,1x,6f16.8)'

REAL(r64), PARAMETER :: MinFlow = 0.01d0      ! Minimum mass flow rate
REAL(r64), PARAMETER :: MaxACH = 100.0d0      ! Maximum ceiling diffuser correlation limit
CHARACTER(len=*), PARAMETER :: Blank = ' '

REAL(r64), PARAMETER :: OneThird  = (1.d0/3.d0)  ! 1/3 in highest precision
REAL(r64), PARAMETER :: OneFourth = (1.d0/4.d0)  ! 1/4 in highest precision
REAL(r64), PARAMETER :: OneFifth  = (1.d0/5.d0)  ! 1/5 in highest precision
REAL(r64), PARAMETER :: OneSixth  = (1.d0/6.d0)  ! 1/6 in highest precision
REAL(r64), PARAMETER :: FourFifths = (4.d0/5.d0) ! 4/5 in highest precision


! Coefficients that modify the convection coeff based on surface roughness
REAL(r64), PARAMETER, DIMENSION(6) :: RoughnessMultiplier = (/ 2.17d0, 1.67d0, 1.52d0, 1.13d0, 1.11d0, 1.0d0 /)

! parameters for identifying more specific hc model equations, inside face
INTEGER, PARAMETER :: HcInt_UserValue                               = 200
INTEGER, PARAMETER :: HcInt_UserSchedule                            = 201
INTEGER, PARAMETER :: HcInt_UserCurve                               = 202
INTEGER, PARAMETER :: HcInt_ASHRAEVerticalWall                      = 203
INTEGER, PARAMETER :: HcInt_WaltonUnstableHorizontalOrTilt          = 204
INTEGER, PARAMETER :: HcInt_WaltonStableHorizontalOrTilt            = 205
INTEGER, PARAMETER :: HcInt_FisherPedersenCeilDiffuserFloor         = 206
INTEGER, PARAMETER :: HcInt_FisherPedersenCeilDiffuserCeiling       = 207
INTEGER, PARAMETER :: HcInt_FisherPedersenCeilDiffuserWalls         = 208
INTEGER, PARAMETER :: HcInt_AlamdariHammondStableHorizontal         = 209
INTEGER, PARAMETER :: HcInt_AlamdariHammondVerticalWall             = 210
INTEGER, PARAMETER :: HcInt_AlamdariHammondUnstableHorizontal       = 211
INTEGER, PARAMETER :: HcInt_KhalifaEq3WallAwayFromHeat              = 212
INTEGER, PARAMETER :: HcInt_KhalifaEq4CeilingAwayFromHeat           = 213
INTEGER, PARAMETER :: HcInt_KhalifaEq5WallNearHeat                  = 214
INTEGER, PARAMETER :: HcInt_KhalifaEq6NonHeatedWalls                = 215
INTEGER, PARAMETER :: HcInt_KhalifaEq7Ceiling                       = 216
INTEGER, PARAMETER :: HcInt_AwbiHattonHeatedFloor                   = 217
INTEGER, PARAMETER :: HcInt_AwbiHattonHeatedWall                    = 218
INTEGER, PARAMETER :: HcInt_BeausoleilMorrisonMixedAssistingWall    = 219
INTEGER, PARAMETER :: HcInt_BeausoleilMorrisonMixedOppossingWall    = 220
INTEGER, PARAMETER :: HcInt_BeausoleilMorrisonMixedStableCeiling    = 221
INTEGER, PARAMETER :: HcInt_BeausoleilMorrisonMixedUnstableCeiling  = 222
INTEGER, PARAMETER :: HcInt_BeausoleilMorrisonMixedStableFloor      = 223
INTEGER, PARAMETER :: HcInt_BeausoleilMorrisonMixedUnstableFloor    = 224
INTEGER, PARAMETER :: HcInt_FohannoPolidoriVerticalWall             = 225
INTEGER, PARAMETER :: HcInt_KaradagChilledCeiling                   = 226
INTEGER, PARAMETER :: HcInt_ISO15099Windows                         = 227
INTEGER, PARAMETER :: HcInt_GoldsteinNovoselacCeilingDiffuserWindow = 228
INTEGER, PARAMETER :: HcInt_GoldsteinNovoselacCeilingDiffuserWalls  = 229
INTEGER, PARAMETER :: HcInt_GoldsteinNovoselacCeilingDiffuserFloor  = 230

!parameters for identifying more specific hc model equations, outside face
INTEGER, PARAMETER :: HcExt_None                                    = 300 !none is allowed because Hn and Hf are split
INTEGER, PARAMETER :: HcExt_UserValue                               = 301
INTEGER, PARAMETER :: HcExt_UserSchedule                            = 302
INTEGER, PARAMETER :: HcExt_UserCurve                               = 303
INTEGER, PARAMETER :: HcExt_ASHRAESimpleCombined                    = 304
INTEGER, PARAMETER :: HcExt_NaturalASHRAEVerticalWall               = 305
INTEGER, PARAMETER :: HcExt_NaturalWaltonUnstableHorizontalOrTilt   = 306
INTEGER, PARAMETER :: HcExt_NaturalWaltonStableHorizontalOrTilt     = 307
INTEGER, PARAMETER :: HcExt_SparrowWindward                         = 308
INTEGER, PARAMETER :: HcExt_SparrowLeeward                          = 309
INTEGER, PARAMETER :: HcExt_MoWiTTWindward                          = 310
INTEGER, PARAMETER :: HcExt_MoWiTTLeeward                           = 311
INTEGER, PARAMETER :: HcExt_DOE2Windward                            = 312
INTEGER, PARAMETER :: HcExt_DOE2Leeward                             = 313
INTEGER, PARAMETER :: HcExt_NusseltJurges                           = 314
INTEGER, PARAMETER :: HcExt_McAdams                                 = 315
INTEGER, PARAMETER :: HcExt_Mitchell                                = 316
INTEGER, PARAMETER :: HcExt_ClearRoof                               = 317
INTEGER, PARAMETER :: HcExt_BlockenWindward                         = 318
INTEGER, PARAMETER :: HcExt_EmmelVertical                           = 319
INTEGER, PARAMETER :: HcExt_EmmelRoof                               = 320
INTEGER, PARAMETER :: HcExt_AlamdariHammondVerticalWall             = 321
INTEGER, PARAMETER :: HcExt_FohannoPolidoriVerticalWall             = 322
INTEGER, PARAMETER :: HcExt_ISO15099Windows                         = 323
INTEGER, PARAMETER :: HcExt_AlamdariHammondStableHorizontal         = 324
INTEGER, PARAMETER :: HcExt_AlamdariHammondUnstableHorizontal       = 325

!parameters, by zone, for flow regimes for adaptive convection on inside face
INTEGER, PARAMETER :: InConvFlowRegime_A1 = 1 ! In-floor heating or in-ceiling cooling
INTEGER, PARAMETER :: InConvFlowRegime_A2 = 2 ! In-wall heating
INTEGER, PARAMETER :: InConvFlowRegime_A3 = 3 ! no HVAC system, all bouyancy
INTEGER, PARAMETER :: InConvFlowRegime_B  = 4 ! Convective heater in zone
INTEGER, PARAMETER :: InConvFlowRegime_C  = 5 ! central mechanical air
INTEGER, PARAMETER :: InConvFlowRegime_D  = 6 ! zone mechanical air
INTEGER, PARAMETER :: InConvFlowRegime_E  = 7 ! mixed. mechancial air and bouyancy

!params for reference temperature type
INTEGER, PARAMETER :: RefTempMeanAirTemp     = 1
INTEGER, PARAMETER :: RefTempAdjacentAirTemp = 2
INTEGER, PARAMETER :: RefTempSupplyAirTemp   = 3
INTEGER, PARAMETER :: RefTempOutDryblubAtZ   = 4
INTEGER, PARAMETER :: RefTempOutDrybulbEPW   = 5
INTEGER, PARAMETER :: RefTempOutWetbulbAtZ   = 6
INTEGER, PARAMETER :: RefTempOutWetbulbEPW   = 7

!params for wind speed type
INTEGER, PARAMETER :: RefWindWeatherFile     = 1
INTEGER, PARAMETER :: RefWindAtZ             = 2
INTEGER, PARAMETER :: RefWindParallComp      = 3
INTEGER, PARAMETER :: RefWindParallCompAtZ   = 4

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! MODULE VARIABLE DECLARATIONS:

  TYPE HcInsideFaceUserCurveStruct
    CHARACTER(len=MaxNameLength) :: Name     = ' ' ! user's name for object

    INTEGER :: ReferenceTempType             = 0 !
    INTEGER :: HcFnTempDiffCurveNum          = 0 !
    INTEGER :: HcFnTempDiffDivHeightCurveNum = 0 !
    INTEGER :: HcFnACHCurveNum               = 0 !
    INTEGER :: HcFnACHDivPerimLengthCurveNum = 0 !
  ENDTYPE HcInsideFaceUserCurveStruct

  TYPE HcOutsideFaceUserCurveStruct
    CHARACTER(len=MaxNameLength) :: Name     = ' '
    INTEGER :: ReferenceTempType             = 0
    LOGICAL :: SuppressRainChange            = .FALSE.
    INTEGER :: WindSpeedType                 = 0
    INTEGER :: HfFnWindSpeedCurveNum         = 0
    INTEGER :: HnFnTempDiffCurveNum          = 0
    INTEGER :: HnFnTempDiffDivHeightCurveNum = 0
  ENDTYPE HcOutsideFaceUserCurveStruct

  TYPE InsideFaceAdaptiveConvAlgoStruct
    LOGICAL :: EnteredByUser = .FALSE.  !
    CHARACTER(len=MaxNameLength) :: Name                      = ' '
    INTEGER :: SimpleBouyVertWallEqNum                        = HcInt_FohannoPolidoriVerticalWall ! InConvClass_A3_VertWalls
    INTEGER :: SimpleBouyVertWallUserCurveNum                 = 0 !
    INTEGER :: SimpleBouyStableHorizEqNum                     = HcInt_AlamdariHammondStableHorizontal !InConvClass_A3_StableHoriz
    INTEGER :: SimpleBouyStableHorizUserCurveNum              = 0
    INTEGER :: SimpleBouyUnstableHorizEqNum                   = HcInt_AlamdariHammondUnstableHorizontal !InConvClass_A3_UnstableHoriz
    INTEGER :: SimpleBouyUnstableHorizUserCurveNum            = 0 !
    INTEGER :: SimpleBouyStableTiltedEqNum                    = HcInt_WaltonStableHorizontalOrTilt !InConvClass_A3_StableTilted
    INTEGER :: SimpleBouyStableTiltedUserCurveNum             = 0
    INTEGER :: SimpleBouyUnstableTiltedEqNum                  = HcInt_WaltonUnstableHorizontalOrTilt !InConvClass_A3_UnstableTilted
    INTEGER :: SimpleBouyUnstableTiltedUserCurveNum           = 0 !
    INTEGER :: SimpleBouyWindowsEqNum                         = HcInt_ISO15099Windows  !InConvClass_A3_Windows
    INTEGER :: SimpleBouyWindowsUserCurveNum                  = 0
    INTEGER :: FloorHeatCeilingCoolVertWallEqNum              = HcInt_KhalifaEq3WallAwayFromHeat !InConvClass_A1_VertWalls
    INTEGER :: FloorHeatCeilingCoolVertWallUserCurveNum       = 0 !
    INTEGER :: FloorHeatCeilingCoolStableHorizEqNum           = HcInt_AlamdariHammondStableHorizontal !InConvClass_A1_StableHoriz
    INTEGER :: FloorHeatCeilingCoolStableHorizUserCurveNum    = 0
    INTEGER :: FloorHeatCeilingCoolUnstableHorizEqNum         = HcInt_KhalifaEq4CeilingAwayFromHeat !InConvClass_A1_UntableHoriz
    INTEGER :: FloorHeatCeilingCoolUnstableHorizUserCurveNum  = 0
    INTEGER :: FloorHeatCeilingCoolHeatedFloorEqNum           = HcInt_AwbiHattonHeatedFloor  !InConvClass_A1_HeatedFloor
    INTEGER :: FloorHeatCeilingCoolHeatedFloorUserCurveNum    = 0
    INTEGER :: FloorHeatCeilingCoolChilledCeilingEqNum        = HcInt_KaradagChilledCeiling !InConvClass_A1_ChilledCeil
    INTEGER :: FloorHeatCeilingCoolChilledCeilingUserCurveNum = 0
    INTEGER :: FloorHeatCeilingCoolStableTiltedEqNum          = HcInt_WaltonStableHorizontalOrTilt !InConvClass_A1_StableTilted
    INTEGER :: FloorHeatCeilingCoolStableTiltedUserCurveNum   = 0
    INTEGER :: FloorHeatCeilingCoolUnstableTiltedEqNum        = HcInt_WaltonUnstableHorizontalOrTilt !InConvClass_A1_UnstableTilted
    INTEGER :: FloorHeatCeilingCoolUnstableTiltedUserCurveNum = 0
    INTEGER :: FloorHeatCeilingCoolWindowsEqNum               = HcInt_ISO15099Windows !InConvClass_A1_Windows
    INTEGER :: FloorHeatCeilingCoolWindowsUserCurveNum        = 0
    INTEGER :: WallPanelHeatVertWallEqNum                     = HcInt_KhalifaEq6NonHeatedWalls !InConvClass_A2_VertWallsNonHeated
    INTEGER :: WallPanelHeatVertWallUserCurveNum              = 0
    INTEGER :: WallPanelHeatHeatedWallEqNum                   = HcInt_AwbiHattonHeatedWall !InConvClass_A2_HeatedVerticalWall
    INTEGER :: WallPanelHeatHeatedWallUserCurveNum            = 0
    INTEGER :: WallPanelHeatStableHorizEqNum                  = HcInt_AlamdariHammondStableHorizontal !InConvClass_A2_StableHoriz
    INTEGER :: WallPanelHeatStableHorizUserCurveNum           = 0
    INTEGER :: WallPanelHeatUnstableHorizEqNum                = HcInt_KhalifaEq7Ceiling !InConvClass_A2_UnstableHoriz
    INTEGER :: WallPanelHeatUnstableHorizUserCurveNum         = 0
    INTEGER :: WallPanelHeatStableTiltedEqNum                 = HcInt_WaltonStableHorizontalOrTilt !InConvClass_A2_StableTilted
    INTEGER :: WallPanelHeatStableTiltedUserCurveNum          = 0
    INTEGER :: WallPanelHeatUnstableTiltedEqNum               = HcInt_WaltonUnstableHorizontalOrTilt !InConvClass_A2_UnstableTilted
    INTEGER :: WallPanelHeatUnstableTiltedUserCurveNum        = 0
    INTEGER :: WallPanelHeatWindowsEqNum                      = HcInt_ISO15099Windows !InConvClass_A2_Windows
    INTEGER :: WallPanelHeatWindowsUserCurveNum               = 0
    INTEGER :: ConvectiveHeatVertWallEqNum                    = HcInt_FohannoPolidoriVerticalWall
    INTEGER :: ConvectiveHeatVertWallUserCurveNum             = 0
    INTEGER :: ConvectiveHeatVertWallNearHeaterEqNum          = HcInt_KhalifaEq5WallNearHeat
    INTEGER :: ConvectiveHeatVertWallNearHeaterUserCurveNum   = 0
    INTEGER :: ConvectiveHeatStableHorizEqNum                 = HcInt_AlamdariHammondStableHorizontal
    INTEGER :: ConvectiveHeatStableHorizUserCurveNum          = 0
    INTEGER :: ConvectiveHeatUnstableHorizEqNum               = HcInt_KhalifaEq7Ceiling
    INTEGER :: ConvectiveHeatUnstableHorizUserCurveNum        = 0
    INTEGER :: ConvectiveHeatStableTiltedEqNum                = HcInt_WaltonStableHorizontalOrTilt
    INTEGER :: ConvectiveHeatStableTiltedUserCurveNum         = 0
    INTEGER :: ConvectiveHeatUnstableTiltedEqNum              = HcInt_WaltonUnstableHorizontalOrTilt
    INTEGER :: ConvectiveHeatUnstableTiltedUserCurveNum       = 0
    INTEGER :: ConvectiveHeatWindowsEqNum                     = HcInt_ISO15099Windows
    INTEGER :: ConvectiveHeatWindowsUserCurveNum              = 0
    INTEGER :: CentralAirWallEqNum                            = HcInt_GoldsteinNovoselacCeilingDiffuserWalls
    INTEGER :: CentralAirWallUserCurveNum                     = 0
    INTEGER :: CentralAirCeilingEqNum                         = HcInt_FisherPedersenCeilDiffuserCeiling
    INTEGER :: CentralAirCeilingUserCurveNum                  = 0
    INTEGER :: CentralAirFloorEqNum                           = HcInt_GoldsteinNovoselacCeilingDiffuserFloor
    INTEGER :: CentralAirFloorUserCurveNum                    = 0
    INTEGER :: CentralAirWindowsEqNum                         = HcInt_GoldsteinNovoselacCeilingDiffuserWindow
    INTEGER :: CentralAirWindowsUserCurveNum                  = 0
    INTEGER :: ZoneFanCircVertWallEqNum                       = HcInt_KhalifaEq3WallAwayFromHeat
    INTEGER :: ZoneFanCircVertWallUserCurveNum                = 0
    INTEGER :: ZoneFanCircStableHorizEqNum                    = HcInt_AlamdariHammondStableHorizontal
    INTEGER :: ZoneFanCircStableHorizUserCurveNum             = 0
    INTEGER :: ZoneFanCircUnstableHorizEqNum                  = HcInt_KhalifaEq4CeilingAwayFromHeat
    INTEGER :: ZoneFanCircUnstableHorizUserCurveNum           = 0
    INTEGER :: ZoneFanCircStableTiltedEqNum                   = HcInt_WaltonStableHorizontalOrTilt
    INTEGER :: ZoneFanCircStableTiltedUserCurveNum            = 0
    INTEGER :: ZoneFanCircUnstableTiltedEqNum                 = HcInt_WaltonUnstableHorizontalOrTilt
    INTEGER :: ZoneFanCircUnstableTiltedUserCurveNum          = 0
    INTEGER :: ZoneFanCircWindowsEqNum                        = HcInt_ISO15099Windows
    INTEGER :: ZoneFanCircWindowsUserCurveNum                 = 0
    INTEGER :: MixedBouyAssistingFlowWallEqNum                = HcInt_BeausoleilMorrisonMixedAssistingWall
    INTEGER :: MixedBouyAssistingFlowWallUserCurveNum         = 0
    INTEGER :: MixedBouyOppossingFlowWallEqNum                = HcInt_BeausoleilMorrisonMixedOppossingWall
    INTEGER :: MixedBouyOppossingFlowWallUserCurveNum         = 0
    INTEGER :: MixedStableFloorEqNum                          = HcInt_BeausoleilMorrisonMixedStableFloor
    INTEGER :: MixedStableFloorUserCurveNum                   = 0
    INTEGER :: MixedUnstableFloorEqNum                        = HcInt_BeausoleilMorrisonMixedUnstableFloor
    INTEGER :: MixedUnstableFloorUserCurveNum                 = 0
    INTEGER :: MixedStableCeilingEqNum                        = HcInt_BeausoleilMorrisonMixedStableCeiling
    INTEGER :: MixedStableCeilingUserCurveNum                 = 0
    INTEGER :: MixedUnstableCeilingEqNum                      = HcInt_BeausoleilMorrisonMixedUnstableCeiling
    INTEGER :: MixedUnstableCeilingUserCurveNum               = 0
    INTEGER :: MixedWindowsEqNum                              = HcInt_GoldsteinNovoselacCeilingDiffuserWindow
    INTEGER :: MixedWindowsUserCurveNum                       = 0
  END TYPE InsideFaceAdaptiveConvAlgoStruct

  TYPE OutsideFaceAdpativeConvAlgoStruct
    LOGICAL :: EnteredByUser = .FALSE.  !
    CHARACTER(len=MaxNameLength) :: Name        = ' '
    LOGICAL :: SuppressRainChange               = .FALSE.
    INTEGER :: HWindWallWindwardEqNum           = HcExt_BlockenWindward
    INTEGER :: HWindWallWindwardUserCurveNum    = 0
    INTEGER :: HWindWallLeewardEqNum            = HcExt_EmmelVertical
    INTEGER :: HWindWallLeewardUserCurveNum     = 0
    INTEGER :: HWindHorizRoofEqNum              = HcExt_ClearRoof
    INTEGER :: HWindHorizRoofUserCurveNum       = 0
    INTEGER :: HNatVertWallEqNum                = HcExt_NaturalASHRAEVerticalWall
    INTEGER :: HNatVertWallUserCurveNum         = 0
    INTEGER :: HNatStableHorizEqNum             = HcExt_NaturalWaltonStableHorizontalOrTilt
    INTEGER :: HNatStableHorizUserCurveNum      = 0
    INTEGER :: HNatUnstableHorizEqNum           = HcExt_NaturalWaltonUnstableHorizontalOrTilt
    INTEGER :: HNatUstableHorizUserCurveNum     = 0
  END TYPE OutsideFaceAdpativeConvAlgoStruct

  TYPE BoundingBoxVertStruct
    INTEGER   :: SurfNum    =0
    INTEGER   :: VertNum    =0
    TYPE(vector) :: Vertex  =vector(0.0d0,0.0d0,0.0d0)
  END TYPE BoundingBoxVertStruct

  TYPE RoofGeoCharactisticsStruct

    TYPE(BoundingBoxVertStruct) :: XdYdZd ! 1 low x, low y, low z
    TYPE(BoundingBoxVertStruct) :: XdYdZu ! 2 low x, low y, hi z
    TYPE(BoundingBoxVertStruct) :: XdYuZd ! 3 low x, hi y, low z
    TYPE(BoundingBoxVertStruct) :: XdYuZu ! 4 low x, hi y, hi z
    TYPE(BoundingBoxVertStruct) :: XuYdZd ! 5 hi x, low y, low z
    TYPE(BoundingBoxVertStruct) :: XuYuZd ! 6 hi x, hi y, low z
    TYPE(BoundingBoxVertStruct) :: XuYdZu ! 7 hi x, low y, hi z
    TYPE(BoundingBoxVertStruct) :: XuYuZu ! 8 hi x, hi y, hi z
    TYPE(vector), DIMENSION(4)  :: BoundSurf !long edge of roof group bounding surface
    REAL(r64) :: Area          =0.0d0
    REAL(r64) :: Perimeter     =0.0d0
    REAL(r64) :: Height        =0.0d0
  END TYPE RoofGeoCharactisticsStruct

  TYPE(InsideFaceAdaptiveConvAlgoStruct),SAVE  :: InsideFaceAdaptiveConvectionAlgo !stores rules for Hc model equations
  TYPE(OutsideFaceAdpativeConvAlgoStruct),SAVE :: OutsideFaceAdaptiveConvectionAlgo
  TYPE(HcInsideFaceUserCurveStruct),  DIMENSION(:), ALLOCATABLE :: HcInsideUserCurve
  TYPE(HcOutsideFaceUserCurveStruct), DIMENSION(:), ALLOCATABLE :: HcOutsideUserCurve
  TYPE(RoofGeoCharactisticsStruct),SAVE ::  RoofGeo

  INTEGER :: TotOutsideHcUserCurves  = 0
  INTEGER :: TotInsideHcUserCurves   = 0
  LOGICAL :: GetUserSuppliedConvectionCoeffs = .true.  ! Get user input first call for Init

  LOGICAL :: ConvectionGeometryMetaDataSetup = .false. ! set to true once geometry meta data are setup
  REAL(r64) :: CubeRootOfOverallBuildingVolume = 0.d0 ! building meta data. cube root of the volume of all the zones
  REAL(r64) :: RoofLongAxisOutwardAzimuth      = 0.d0 ! roof surfaces meta data. outward normal azimuth for longest roof edge

  ! SUBROUTINE SPECIFICATIONS:
PUBLIC  InitInteriorConvectionCoeffs
PUBLIC  InitExteriorConvectionCoeff
PRIVATE CalcHfExteriorSparrow
PRIVATE CalcHnASHRAETARPExterior
PRIVATE Windward
PRIVATE GetUserConvectionCoefficients
!PRIVATE ApplyConvectionValue ! internal to GetUserConvectionCoefficients
PUBLIC  CalcASHRAESimpExtConvectCoeff
PRIVATE CalcASHRAESimpleIntConvCoeff
PRIVATE CalcASHRAEDetailedIntConvCoeff
PUBLIC  CalcDetailedHcINforDVModel
PRIVATE CalcCeilingDiffuserIntConvCoeff
PRIVATE CalcCeilingDiffuserInletCorr
PRIVATE CalcTrombeWallIntConvCoeff
PRIVATE CalcNusselt
PUBLIC  SetExtConvectionCoeff
PUBLIC  SetIntConvectionCoeff
PUBLIC  CalcISO15099WindowIntConvCoeff

PRIVATE SetupAdaptiveConvectionStaticMetaData
PRIVATE SetupAdaptiveConvectionRadiantSurfaceData
PRIVATE ManageInsideAdaptiveConvectionAlgo
PRIVATE ManageOutsideAdaptiveConvectionAlgo
PRIVATE EvaluateIntHcModels
PRIVATE EvaluateExtHcModels
PRIVATE DynamicIntConvSurfaceClassification
PRIVATE MapIntConvClassificationToHcModels
PRIVATE DynamicExtConvSurfaceClassification
PRIVATE MapExtConvClassificationToHcModels

!more specific Hc model equations
PRIVATE CalcUserDefinedInsideHcModel
PRIVATE CalcUserDefinedOutsideHcModel
PRIVATE CalcASHRAEVerticalWall
PRIVATE CalcWaltonUnstableHorizontalOrTilt
PRIVATE CalcWaltonStableHorizontalOrTilt
PRIVATE CalcAlamdariHammondUnstableHorizontal
PRIVATE CalcAlamdariHammondStableHorizontal
PRIVATE CalcAlamdariHammondVerticalWall
PRIVATE CalcKhalifaEq3WallAwayFromHeat
PRIVATE CalcKhalifaEq4CeilingAwayFromHeat
PRIVATE CalcKhalifaEq5WallsNearHeat
PRIVATE CalcKhalifaEq6NonHeatedWalls
PRIVATE CalcKhalifaEq7Ceiling
PRIVATE CalcAwbiHattonHeatedFloor
PRIVATE CalcAwbiHattonHeatedWall
PRIVATE CalcBeausoleilMorrisonMixedAssistedWall
PRIVATE CalcBeausoleilMorrisonMixedOpposingWall
PRIVATE CalcBeausoleilMorrisonMixedStableFloor
PRIVATE CalcBeausoleilMorrisonMixedUnstableFloor
PRIVATE CalcBeausoleilMorrisonMixedStableCeiling
PRIVATE CalcBeausoleilMorrisonMixedUnstableCeiling
PRIVATE CalcFohannoPolidoriVerticalWall
PRIVATE CalcKaradagChilledCeiling
PRIVATE CalcGoldsteinNovoselacCeilingDiffuserWindow
PRIVATE CalcGoldsteinNovoselacCeilingDiffuserWall
PRIVATE CalcGoldsteinNovoselacCeilingDiffuserFloor

PRIVATE CalcSparrowWindward
PRIVATE CalcSparrowLeeward
PRIVATE CalcMoWITTWindward
PRIVATE CalcMoWITTLeeward
PRIVATE CalcDOE2Windward
PRIVATE CalcDOE2Leeward
PRIVATE CalcNusseltJurges
PRIVATE CalcMcAdams
PRIVATE CalcMitchell
PRIVATE CalcBlockenWindward
PRIVATE CalcEmmelVertical
PRIVATE CalcEmmelRoof
PRIVATE CalcClearRoof


CONTAINS

SUBROUTINE InitInteriorConvectionCoeffs(SurfaceTemperatures,ZoneToResimulate)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   March 1998
          !       MODIFIED       Dan Fisher, Nov 2000
          !                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the arrays associated with interior
          ! surface convection.  The main parameter which is initialized
          ! in this routine is HConvIn, the convection coefficient on the
          ! inside surface.

          ! METHODOLOGY EMPLOYED:
          ! Determine the temperature difference between the surface and the
          ! zone air for the last time step and then base the calculation
          ! of the convection coefficient on that value and the surface tilt.

          ! REFERENCES:
          ! (I)BLAST legacy routine VARTMP
          ! 1.  Passive Solar Extension of the BLAST Program
          !       Appendix E. p. 17,18
          ! 2.  ASHRAE
          !       Simple Algorithm:    ASHRAE Handbook of Fundamentals 1985, p. 23.2, Table 1
          !       Detailed Algorithm:  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5
          ! 3.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
          !     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80
          ! 4.  Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
          !       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137
          ! 5.  ISO Standard 15099:2003e

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: MAT
  USE DataHeatBalance,   ONLY: Construct
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled, ZoneEquipSimulatedOnce
  USE DataGlobals,       ONLY: BeginEnvrnFlag
  USE DataLoopNode,      ONLY: Node, NumOfNodes
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), DIMENSION(:), INTENT(IN) :: SurfaceTemperatures ! Temperature of surfaces for evaluation of HcIn
  INTEGER, INTENT(IN), OPTIONAL :: ZoneToResimulate  ! if passed in, then only calculate surfaces that have this zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneNum            ! DO loop counter for zones
  INTEGER :: SurfNum            ! DO loop counter for surfaces in zone
  LOGICAL,SAVE :: NodeCheck=.true.  ! for CeilingDiffuser Zones
  LOGICAL,SAVE :: ActiveSurfaceCheck=.TRUE. ! for radiant surfaces in zone
  LOGICAL, SAVE :: MyEnvirnFlag = .TRUE.

          ! FLOW:
  IF (GetUserSuppliedConvectionCoeffs) THEN
    CALL GetUserConvectionCoefficients
    GetUserSuppliedConvectionCoeffs=.false.
  ENDIF

  IF (NodeCheck) THEN   ! done once when conditions are ready...
    IF (.not. SysSizingCalc .AND. .not. ZoneSizingCalc .AND. ZoneEquipInputsFilled .and. ALLOCATED(Node)) THEN
      NodeCheck=.false.
      DO ZoneNum=1,NumOfZones
        IF (Zone(ZoneNum)%InsideConvectionAlgo /= CeilingDiffuser)  CYCLE
        IF (Zone(ZoneNum)%SystemZoneNodeNumber /= 0) CYCLE
        CALL ShowSevereError('InitInteriorConvectionCoeffs: Inside Convection=CeilingDiffuser, '//  &
                             'but no system inlet node defined, Zone='//TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Defaulting inside convection to TARP. Check ZoneHVAC:EquipmentConnections for Zone='// &
                             TRIM(Zone(ZoneNum)%Name))
        Zone(ZoneNum)%InsideConvectionAlgo=ASHRAETARP
      ENDDO
      !insert one-time setup for adpative inside face
    ENDIF
  ENDIF

  IF (ActiveSurfaceCheck .AND. .NOT. SysSizingCalc .AND. .NOT. ZoneSizingCalc .AND. ZoneEquipSimulatedOnce) THEN
    CALL SetupAdaptiveConvectionRadiantSurfaceData
    ActiveSurfaceCheck = .FALSE.
  ENDIF

  IF (BeginEnvrnFlag .AND. MyEnvirnFlag) THEN
    IF (ANY(Surface%IntConvCoeff == AdaptiveConvectionAlgorithm) .OR. &
        ANY(Zone%InsideConvectionAlgo== AdaptiveConvectionAlgorithm)) THEN
      !need to clear out node conditions because dynamic assignments will be affected
      IF (NumOfNodes > 0 .AND. ALLOCATED(Node)) THEN
        Node%Temp                      = DefaultNodeValues%Temp
        Node%TempMin                   = DefaultNodeValues%TempMin
        Node%TempMax                   = DefaultNodeValues%TempMax
        Node%TempSetPoint              = DefaultNodeValues%TempSetPoint
        Node%MassFlowRate              = DefaultNodeValues%MassFlowRate
        Node%MassFlowRateMin           = DefaultNodeValues%MassFlowRateMin
        Node%MassFlowRateMax           = DefaultNodeValues%MassFlowRateMax
        Node%MassFlowRateMinAvail      = DefaultNodeValues%MassFlowRateMinAvail
        Node%MassFlowRateMaxAvail      = DefaultNodeValues%MassFlowRateMaxAvail
        Node%MassFlowRateSetPoint      = DefaultNodeValues%MassFlowRateSetPoint
        Node%Quality                   = DefaultNodeValues%Quality
        Node%Press                     = DefaultNodeValues%Press
        Node%Enthalpy                  = DefaultNodeValues%Enthalpy
        Node%HumRat                    = DefaultNodeValues%HumRat
        Node%HumRatMin                 = DefaultNodeValues%HumRatMin
        Node%HumRatMax                 = DefaultNodeValues%HumRatMax
        Node%HumRatSetPoint            = DefaultNodeValues%HumRatSetPoint
        Node%TempSetPointHi            = DefaultNodeValues%TempSetPointHi
        Node%TempSetPointLo            = DefaultNodeValues%TempSetPointLo
        IF (ALLOCATED(MoreNodeInfo)) THEN
          MoreNodeInfo%WetbulbTemp       = DefaultNodeValues%Temp
          MoreNodeInfo%RelHumidity       = 0.0d0
          MoreNodeInfo%ReportEnthalpy    = DefaultNodeValues%Enthalpy
          MoreNodeInfo%VolFlowRateStdRho = 0.0d0
          MoreNodeInfo%VolFlowRateCrntRho= 0.0d0
          MoreNodeInfo%Density           = 0.0d0
        ENDIF
      ENDIF
    ENDIF
    MyEnvirnFlag = .FALSE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) MyEnvirnFlag = .TRUE.
ZoneLoop1:  DO ZoneNum = 1, NumOfZones

    SELECT CASE (Zone(ZoneNum)%InsideConvectionAlgo)
      ! Ceiling Diffuser and Trombe Wall only make sense at Zone Level
      ! Interior convection coeffs are first calculated here and then at surface level
      CASE (CeilingDiffuser)
        CALL CalcCeilingDiffuserIntConvCoeff(ZoneNum)

      CASE (TrombeWall)
        CALL CalcTrombeWallIntConvCoeff(ZoneNum,SurfaceTemperatures)

      CASE DEFAULT

    END SELECT

  END DO ZoneLoop1
ZoneLoop2:  DO ZoneNum = 1, NumOfZones

  SurfLoop:  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast

      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE  ! Skip non-heat transfer surfaces

      IF (PRESENT(ZoneToResimulate)) THEN
        IF ((ZoneNum /= ZoneToResimulate) .AND. (AdjacentZoneToSurface(SurfNum) /= ZoneToResimulate)) THEN
          CYCLE ! skip surfaces that are not associated with this zone
        ENDIF
      ENDIF

      SELECT CASE(Surface(SurfNum)%IntConvCoeff)

        CASE(:-1)  ! Set by user using one of the standard algorithms...

          SELECT CASE (ABS(Surface(SurfNum)%IntConvCoeff))

            CASE (ASHRAESimple)
              CALL CalcASHRAESimpleIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
              ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
              IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

            CASE (ASHRAETARP)
              IF (.NOT. Construct(Surface(SurfNum)%Construction)%TypeIsWindow) THEN
                CALL CalcASHRAEDetailedIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
              ELSE
                CALL CalcISO15099WindowIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
              ENDIF

              ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
              IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

            CASE (AdaptiveConvectionAlgorithm)

              CALL ManageInsideAdaptiveConvectionAlgo(SurfNum)

            CASE DEFAULT
              CALL ShowFatalError('Unhandled convection coefficient algorithm.')
          END SELECT

        CASE(0)    ! Not set by user, uses Zone Setting

          SELECT CASE (Zone(ZoneNum)%InsideConvectionAlgo)

            CASE (ASHRAESimple)
              CALL CalcASHRAESimpleIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
              ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
              IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

            CASE (ASHRAETARP)
              IF (.NOT. Construct(Surface(SurfNum)%Construction)%TypeIsWindow) THEN
                CALL CalcASHRAEDetailedIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
              ELSE
                CALL CalcISO15099WindowIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
              ENDIF
              ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
              IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

            CASE (AdaptiveConvectionAlgorithm)

              CALL ManageInsideAdaptiveConvectionAlgo(SurfNum)

            CASE (CeilingDiffuser,TrombeWall)
              ! Already done above and can't be at individual surface

            CASE DEFAULT
              CALL ShowFatalError('Unhandled convection coefficient algorithm.')

          END SELECT

        CASE DEFAULT  ! Interior convection has been set by the user with "value" or "schedule"
          HConvIn(SurfNum)=SetIntConvectionCoeff(SurfNum)
          ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
          IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

      END SELECT

      IF (Surface(SurfNum)%EMSOverrideIntConvCoef) HConvIn(SurfNum) = Surface(SurfNum)%EMSValueForIntConvCoef

    END DO SurfLoop

  END DO ZoneLoop2

  RETURN

END SUBROUTINE InitInteriorConvectionCoeffs


SUBROUTINE InitExteriorConvectionCoeff(SurfNum,HMovInsul,Roughness,AbsExt,TempExt,HExt,HSky,HGround,HAir)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   January 1990
          !       MODIFIED       na
          !       RE-ENGINEERED  Mar98 (RKS); Sep03 (LKL): Add additional flavors of Ext Convection Coeff.
          !                      Dec03 (PGE): Re-eng'd ASHRAEDetailed to match BLAST and TARP.
          !                      Aug04 (PGE): Corrected error for calculating local wind speeds for different terrains.
          !                      Aug 2010 B. Griffith.  for outside air convection, added new adaptive convection algorithm etc.

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines the outside convection coefficient for
          ! a particular surface.

          ! METHODOLOGY EMPLOYED:
          ! Based on the properties of a particular surface, determine what the
          ! outside convection coefficients are for outside air, the sky, and
          ! the ground.  Convection coefficients for the sky and ground are
          ! actually linearized radiation coefficients.  The ground surface is
          ! assumed to be the same temperature as the outside air.

          ! REFERENCES:
          ! (I)BLAST legacy routine OCNVCO
          ! TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: SkyTempKelvin, WindDir

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: SurfNum       ! Surface number (in Surface derived type)
  INTEGER, INTENT(IN)  :: Roughness     ! Roughness index (1-6), see DataHeatBalance parameters
  REAL(r64),    INTENT(IN)  :: HMovInsul     ! Equivalent convection coefficient of movable insulation
  REAL(r64),    INTENT(IN)  :: AbsExt        ! Exterior thermal absorptance
  REAL(r64),    INTENT(IN)  :: TempExt       ! Exterior surface temperature (C)
!  REAL(r64),    INTENT(IN)  :: WindSpeedExt  ! Exterior wind speed (m/s)  **No longer used
  REAL(r64),    INTENT(OUT) :: HExt          ! Convection coefficient to exterior air
  REAL(r64),    INTENT(OUT) :: HSky          ! "Convection" coefficient to sky temperature
  REAL(r64),    INTENT(OUT) :: HGround       ! "Convection" coefficient to ground temperature
  REAL(r64),    INTENT(OUT) :: HAir          ! Radiation to Air Component

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: MoWiTTTurbulentConstant = 0.84d0  ! Turbulent natural convection constant

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: TAir           ! Absolute dry bulb temperature of outdoor air (K)
!  REAL(r64) :: TSky           ! Absolute temperature of the sky (K)
  REAL(r64) :: TSurf          ! Absolute temperature of the exterior surface (K)
  REAL(r64) :: SurfWindSpeed  ! Local wind speed at height of the heat transfer surface (m/s)
  REAL(r64) :: ConstantA      ! = a, Constant, W/(m2K(m/s)^b)
  REAL(r64) :: ConstantB      ! = b, Constant, W/(m2K^(4/3))
  REAL(r64) :: Hn             ! Natural part of exterior convection
  REAL(r64) :: Hf             ! Forced part of exterior convection
  REAL(r64) :: HcGlass
  REAL(r64) :: rcalcPerimeter ! approximation for Perimeter
  INTEGER :: BaseSurf
! real(r64) :: flag

          ! FLOW:
  IF (GetUserSuppliedConvectionCoeffs) THEN
    CALL GetUserConvectionCoefficients
    GetUserSuppliedConvectionCoeffs = .FALSE.
  ENDIF

  TAir = Surface(SurfNum)%OutDryBulbTemp + KelvinConv
  TSurf = TempExt + KelvinConv

  BaseSurf = Surface(SurfNum)%BaseSurf ! If this is a base surface, BaseSurf = SurfNum

  IF (.NOT. Surface(SurfNum)%ExtWind) THEN
    SurfWindSpeed = 0.0d0  ! No wind exposure
  ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
    SurfWindSpeed = 0.0d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
  ELSE
    SurfWindSpeed = Surface(SurfNum)%WindSpeed
  ENDIF

  ! Check if exterior is to be set by user
  SELECT CASE(Surface(SurfNum)%ExtConvCoeff)

    CASE(:-1)  ! Set by user using one of the standard algorithms...

      SELECT CASE(ABS(Surface(SurfNum)%ExtConvCoeff))

        CASE(ASHRAESimple)

          HExt = CalcASHRAESimpExtConvectCoeff(Roughness, SurfWindSpeed) ! includes radiation to sky, ground, and air

        CASE(ASHRAETARP, BLASTHcOutside, TarpHcOutside)
          !   Convection is split into forced and natural components. The total
          !   convective heat transfer coefficient is the sum of these components.
          !
          !   Coefficients for subsurfaces are handled in a special way.  The values for perimeter and gross area
          !   are actually referencing the base surface because a subsurface does not initiate a completely new
          !   thermal boundary layer (although it may add some additional complexity that cannot be accounted for
          !   here).  The values for height (Z) and roughness do, however, come from the subsurface.
          !
          !   BLAST algorithm has been replaced by this one since it was identical except for the standard wind
          !   speed measurement height which was only different because of unit conversions:  10 m vs. 30 ft (= 9.14 m).
          !
          !   ASHRAE/BLAST REFERENCES:
          !   ?
          !
          !   TARP REFERENCES:
          !   Walton, G. N.  1983.  Thermal Analysis Research Program Reference Manual.
          !   National Bureau of Standards.  NBSSIR 83-2655.

          ! due to outlying calculations when perimeter is very small compared to area, use Perimeter
          ! approximation calculation

          IF (Surface(BaseSurf)%GrossArea /= 0.0d0 .and. Surface(BaseSurf)%Height /= 0.0d0) THEN
            rCalcPerimeter = 2.0d0  * (Surface(BaseSurf)%GrossArea / Surface(BaseSurf)%Height + Surface(BaseSurf)%Height)
            Hf = CalcHfExteriorSparrow(SurfWindSpeed, Surface(BaseSurf)%GrossArea, rCalcPerimeter, &
              Surface(SurfNum)%CosTilt, Surface(SurfNum)%Azimuth, Roughness, WindDir)
          ELSE
            Hf = 0.0d0
          ENDIF

          IF (HMovInsul > 0.0d0) TSurf = (HMovInsul * TSurf + Hf * TAir) / (HMovInsul + Hf)
          Hn = CalcHnASHRAETARPExterior(TSurf,TAir,Surface(SurfNum)%CosTilt)
          HExt = Hn + Hf

        CASE(MoWiTTHcOutside)
          !   The MoWiTT model is based on measurements taken at the Mobile Window
          !   Thermal Test (MoWiTT) facility.  Appropriate for very smooth surfaces.
          !
          !   REFERENCES:
          !   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
          !   film coefficient for windows in low-rise buildings.
          !   ASHRAE Transactions 100(1):  1087.

          IF (Windward(Surface(SurfNum)%CosTilt,Surface(SurfNum)%Azimuth, WindDir)) THEN
           ConstantA = 3.26d0
           ConstantB = 0.89d0
          ELSE ! leeward
           ConstantA = 3.55d0
           ConstantB = 0.617d0
          END IF

          ! NOTE: Movable insulation is not taken into account here
          HExt = SQRT((MoWiTTTurbulentConstant * (ABS(TAir-TSurf))** OneThird) ** 2 &
                         +(ConstantA * SurfWindSpeed ** ConstantB) ** 2)

        CASE(DOE2HcOutside)
          !   The DOE-2 convection model is a combination of the MoWiTT and the BLAST
          !   convection models. However, it calculates the coefficient for very smooth
          !   surfaces (glass) first and then modified for other surfaces.
          !
          !   REFERENCES:
          !   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.

          IF (Windward(Surface(SurfNum)%CosTilt,Surface(SurfNum)%Azimuth, WindDir)) THEN
            ConstantA = 3.26d0
            ConstantB = 0.89d0
          ELSE ! leeward
            ConstantA = 3.55d0
            ConstantB = 0.617d0
          END IF

          Hn = CalcHnASHRAETARPExterior(TSurf,TAir,Surface(SurfNum)%CosTilt)
          HcGlass = SQRT(Hn**2 + (ConstantA * SurfWindSpeed ** ConstantB)**2)
          Hf = RoughnessMultiplier(Roughness) * (HcGlass - Hn)
          IF (HMovInsul > 0.0d0) THEN
            TSurf = (HMovInsul * TSurf + Hf * TAir) / (HMovInsul + Hf)
            Hn = CalcHnASHRAETARPExterior(TSurf,TAir,Surface(SurfNum)%CosTilt)
            ! Better if there was iteration for movable insulation?
          END IF

          HExt = Hn + Hf

        CASE (AdaptiveConvectionAlgorithm)

          CALL ManageOutsideAdaptiveConvectionAlgo(SurfNum, HExt)

        CASE DEFAULT
          CALL ShowFatalError('InitExtConvection Coefficients: invalid parameter -- outside convection type, Surface='//  &
                             TRIM(Surface(SurfNum)%Name))

       END SELECT  ! choice of algorithm type

       IF (Surface(SurfNum)%EMSOverrideExtConvCoef) HExt = Surface(SurfNum)%EMSValueForExtConvCoef

       IF (TSurf == SkyTempKelvin .OR. ABS(Surface(SurfNum)%ExtConvCoeff) == ASHRAESimple) THEN
         HSky = 0.0d0
       ELSE
         ! Compute sky radiation coefficient
         HSky = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorSkyIR &
           *AirSkyRadSplit(SurfNum)*((TSurf**4)-(SkyTempKelvin**4))/(TSurf-SkyTempKelvin)
       END IF

       IF (TSurf == TAir .OR. ABS(Surface(SurfNum)%ExtConvCoeff) == ASHRAESimple) THEN
         HGround = 0.0d0
         HAir = 0.0d0
       ELSE
         ! Compute ground radiation coefficient
         HGround = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorGroundIR &
           *((TSurf**4)-(TAir**4))/(TSurf-TAir)

         ! Compute air radiation coefficient
         HAir = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorSkyIR &
           *(1.d0-AirSkyRadSplit(SurfNum))*((TSurf**4)-(TAir**4))/(TSurf-TAir)
       END IF

    CASE(0) ! Not set by user  -- uses Zone setting

      SELECT CASE(Zone(Surface(SurfNum)%Zone)%OutsideConvectionAlgo)  ! Algorithm type

        CASE(ASHRAESimple)

          HExt = CalcASHRAESimpExtConvectCoeff(Roughness, SurfWindSpeed) ! includes radiation to sky, ground, and air

        CASE(ASHRAETARP, BLASTHcOutside, TarpHcOutside)
          !   Convection is split into forced and natural components. The total
          !   convective heat transfer coefficient is the sum of these components.
          !
          !   Coefficients for subsurfaces are handled in a special way.  The values for perimeter and gross area
          !   are actually referencing the base surface because a subsurface does not initiate a completely new
          !   thermal boundary layer (although it may add some additional complexity that cannot be accounted for
          !   here).  The values for height (Z) and roughness do, however, come from the subsurface.
          !
          !   BLAST algorithm has been replaced by this one since it was identical except for the standard wind
          !   speed measurement height which was only different because of unit conversions:  10 m vs. 30 ft (= 9.14 m).
          !
          !   ASHRAE/BLAST REFERENCES:
          !   ?
          !
          !   TARP REFERENCES:
          !   Walton, G. N.  1983.  Thermal Analysis Research Program Reference Manual.
          !   National Bureau of Standards.  NBSSIR 83-2655.

          ! due to outlying calculations when perimeter is very small compared to area, use Perimeter
          ! approximation calculation

          IF (Surface(BaseSurf)%GrossArea /= 0.0d0 .and. Surface(BaseSurf)%Height /= 0.0d0) THEN
            rCalcPerimeter = 2.0d0  * (Surface(BaseSurf)%GrossArea / Surface(BaseSurf)%Height + Surface(BaseSurf)%Height)
            Hf = CalcHfExteriorSparrow(SurfWindSpeed, Surface(BaseSurf)%GrossArea, rCalcPerimeter, &
              Surface(SurfNum)%CosTilt, Surface(SurfNum)%Azimuth, Roughness, WindDir)
          ELSE
            Hf = 0.0d0
          ENDIF

          IF (HMovInsul > 0.0d0) TSurf = (HMovInsul * TSurf + Hf * TAir) / (HMovInsul + Hf)
          Hn = CalcHnASHRAETARPExterior(TSurf,TAir,Surface(SurfNum)%CosTilt)
          HExt = Hn + Hf

        CASE(MoWiTTHcOutside)
          !   The MoWiTT model is based on measurements taken at the Mobile Window
          !   Thermal Test (MoWiTT) facility.  Appropriate for very smooth surfaces.
          !
          !   REFERENCES:
          !   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
          !   film coefficient for windows in low-rise buildings.
          !   ASHRAE Transactions 100(1):  1087.
          IF (Windward(Surface(SurfNum)%CosTilt,Surface(SurfNum)%Azimuth, WindDir)) THEN
           ConstantA = 3.26d0
           ConstantB = 0.89d0
          ELSE ! leeward
           ConstantA = 3.55d0
           ConstantB = 0.617d0
          END IF

          ! NOTE: Movable insulation is not taken into account here
          HExt = SQRT((MoWiTTTurbulentConstant * (ABS(TAir-TSurf))** OneThird) ** 2 &
                         +(ConstantA * SurfWindSpeed ** ConstantB) ** 2)

        CASE(DOE2HcOutside)
          !   The DOE-2 convection model is a combination of the MoWiTT and the BLAST
          !   convection models. However, it calculates the coefficient for very smooth
          !   surfaces (glass) first and then modified for other surfaces.
          !
          !   REFERENCES:
          !   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.

          IF (Windward(Surface(SurfNum)%CosTilt,Surface(SurfNum)%Azimuth, WindDir)) THEN
            ConstantA = 3.26d0
            ConstantB = 0.89d0
          ELSE ! leeward
            ConstantA = 3.55d0
            ConstantB = 0.617d0
          END IF

          Hn = CalcHnASHRAETARPExterior(TSurf,TAir,Surface(SurfNum)%CosTilt)
          HcGlass = SQRT(Hn**2 + (ConstantA * SurfWindSpeed ** ConstantB)**2)
          Hf = RoughnessMultiplier(Roughness) * (HcGlass - Hn)
          IF (HMovInsul > 0.0d0) THEN
            TSurf = (HMovInsul * TSurf + Hf * TAir) / (HMovInsul + Hf)
            Hn = CalcHnASHRAETARPExterior(TSurf,TAir,Surface(SurfNum)%CosTilt)
            ! Better if there was iteration for movable insulation?
          END IF

          HExt = Hn + Hf

        CASE (AdaptiveConvectionAlgorithm)

          CALL ManageOutsideAdaptiveConvectionAlgo(SurfNum, HExt)

        CASE DEFAULT
          CALL ShowFatalError('InitExtConvection Coefficients: invalid parameter -- outside convection type, Surface='//  &
                             TRIM(Surface(SurfNum)%Name))

       END SELECT  ! choice of algorithm type

       IF (Surface(SurfNum)%EMSOverrideExtConvCoef) HExt = Surface(SurfNum)%EMSValueForExtConvCoef

       IF (TSurf == SkyTempKelvin .OR. Zone(Surface(SurfNum)%Zone)%OutsideConvectionAlgo == ASHRAESimple) THEN
         HSky = 0.0d0
       ELSE
         ! Compute sky radiation coefficient
         HSky = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorSkyIR &
           *AirSkyRadSplit(SurfNum)*((TSurf**4)-(SkyTempKelvin**4))/(TSurf-SkyTempKelvin)
       END IF

       IF (TSurf == TAir .OR. Zone(Surface(SurfNum)%Zone)%OutsideConvectionAlgo == ASHRAESimple) THEN
         HGround = 0.0d0
         HAir = 0.0d0
       ELSE
         ! Compute ground radiation coefficient
         HGround = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorGroundIR &
           *((TSurf**4)-(TAir**4))/(TSurf-TAir)

         ! Compute air radiation coefficient
         HAir = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorSkyIR &
           *(1.d0-AirSkyRadSplit(SurfNum))*((TSurf**4)-(TAir**4))/(TSurf-TAir)
       END IF

    CASE DEFAULT  ! Exterior convection scheme for this surface has been set by user

      HExt = SetExtConvectionCoeff(SurfNum)

      IF (Surface(SurfNum)%EMSOverrideExtConvCoef) HExt = Surface(SurfNum)%EMSValueForExtConvCoef

      IF (TSurf == SkyTempKelvin .OR. Zone(Surface(SurfNum)%Zone)%OutsideConvectionAlgo == ASHRAESimple) THEN
        HSky = 0.0d0
      ELSE
        ! Compute sky radiation coefficient
        HSky = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorSkyIR &
          *AirSkyRadSplit(SurfNum)*((TSurf**4)-(SkyTempKelvin**4))/(TSurf-SkyTempKelvin)
      END IF

      IF (TSurf == TAir .OR. Zone(Surface(SurfNum)%Zone)%OutsideConvectionAlgo == ASHRAESimple) THEN
        HGround = 0.0d0
        HAir = 0.0d0
      ELSE
        ! Compute ground radiation coefficient
        HGround = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorGroundIR &
          *((TSurf**4)-(TAir**4))/(TSurf-TAir)

        ! Compute air radiation coefficient
        HAir = StefanBoltzmann*AbsExt*Surface(SurfNum)%ViewFactorSkyIR &
          *(1.d0-AirSkyRadSplit(SurfNum))*((TSurf**4)-(TAir**4))/(TSurf-TAir)
      END IF

  END SELECT

  RETURN

END SUBROUTINE InitExteriorConvectionCoeff


FUNCTION CalcHfExteriorSparrow(SurfWindSpeed, GrossArea, Perimeter, CosTilt, Azimuth, Roughness, WindDirection)  &
                        RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the forced convection piece of the
          ! exterior convection coefficient.

          ! METHODOLOGY EMPLOYED:
          ! The forced convection calculation is based on a semi-empirical correlation
          ! developed by Sparrow, Ramsey, and Mass.


          ! REFERENCES:
          !   1. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
          !   width on heat transfer and fluid flow about an inclined rectangular plate.
          !   Journal of Heat Transfer 101:  204.
          !   2. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
          !   procedure with a detailed study of outside heat transfer parameters.
          !   M.S. Thesis, Department of Mechanical and Industrial Engineering,
          !   University of Illinois at Urbana-Champaign.
          !   3. ASHRAE Loads Toolkit.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64),    INTENT(IN)  :: SurfWindSpeed  ! Local wind speed at height of the heat transfer surface (m/s)
  REAL(r64),    INTENT(IN)  :: GrossArea     ! Gross surface area {m2}
  REAL(r64),    INTENT(IN)  :: CosTilt       ! Cosine of the Surface Tilt Angle
                                        ! (Angle between the ground and the surface outward normal)
  REAL(r64),    INTENT(IN)  :: Azimuth       ! Facing angle (degrees) of the surface outward normal
  REAL(r64),    INTENT(IN)  :: Perimeter     ! Surface perimeter length {m}
  INTEGER, INTENT(IN)  :: Roughness     ! Surface roughness index (6=very smooth, 5=smooth, 4=medium smooth,
                                        ! 3=medium rough,2=rough,1=very rough)
  REAL(r64),    INTENT(IN)  :: WindDirection ! Wind (compass) direction (degrees)
  REAL(r64)            :: Hf            ! Surface exterior forced convective heat transfer coefficient, W/(m2-K)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: WindDirectionModifier

  IF ( Windward(CosTilt,Azimuth,WindDirection) ) THEN
     WindDirectionModifier = 1.0d0
  ELSE
     WindDirectionModifier = 0.5d0
  END IF


  Hf = 2.537d0 * WindDirectionModifier * RoughnessMultiplier(Roughness) &
    * SQRT(SurfWindSpeed * Perimeter / GrossArea)

  RETURN

END FUNCTION CalcHfExteriorSparrow


FUNCTION CalcHnASHRAETARPExterior(TOutSurf, TAir, CosTilt) RESULT(Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the natural convection piece of the
          ! exterior convection coefficient.


          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          !   1. ASHRAE.  1993.  ASHRAE Handbook - 1993 Fundamentals.  Atlanta.
          !   2. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
          !   procedure with a detailed study of outside heat transfer parameters.
          !   M.S. Thesis, Department of Mechanical and Industrial Engineering,
          !   University of Illinois at Urbana-Champaign.
          !   3. ASHRAE Loads Toolkit

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: TOutSurf ! Exterior surface temperature
  REAL(r64), INTENT(IN)  :: TAir     ! Outdoor Air temperature
  REAL(r64), INTENT(IN)  :: CosTilt  ! Cosine of the Surface Tilt Angle (Angle between the ground outward normal and
                                ! the surface outward normal)
  REAL(r64)         :: Hn       ! Natural convective heat transfer coefficient,{W/(m2-K)}

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: OneThird = (1.d0/3.d0)  ! 1/3 in highest precision

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Notes: CosTilt > 0 = faces up (roof), CosTilt < 0 = faces down (floor),
  ! CosTilt=0 = vertical surface (wall)

  Hn=0.0d0

  IF (CosTilt == 0.0d0) THEN ! Vertical Surface

    Hn = 1.31d0*(ABS((TOutSurf-TAir))**OneThird)

  ELSE IF ( ((CosTilt < 0.0d0) .AND. (TOutSurf < TAir)) .OR. &
           ((CosTilt > 0.0d0) .AND. (TOutSurf > TAir)) ) THEN   ! Enhanced convection

    Hn = 9.482d0*(ABS((TOutSurf-TAir))**OneThird)/(7.238d0-ABS(CosTilt))

  ELSE IF ( ((CosTilt < 0.0d0) .AND. (TOutSurf > TAir)) .OR. &
           ((CosTilt > 0.0d0) .AND. (TOutSurf < TAir)) ) THEN   ! Reduced convection

    Hn = 1.810d0*(ABS((TOutSurf-TAir))**OneThird)/(1.382d0+ABS(CosTilt))

  END IF    ! Only other condition is TOutSurf=TAir, in which case there is no natural convection part.

  RETURN

END FUNCTION CalcHnASHRAETARPExterior


FUNCTION Windward(CosTilt, Azimuth, WindDirection) RESULT(AgainstWind)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function determines if a surface is "windward" or "leeward" (that is,
          ! into / against the wind (true) or in shelter from wind (false).

          ! METHODOLOGY EMPLOYED:
          ! Leeward is defined as greater than 100 degrees from normal incidence.
          ! Note that a sufficiently horizontal surface is always considered windward.


          ! REFERENCES:
          !   Walton, G. N.  1981.  Passive solar extension of the Building Loads
          !   Analysis and System Thermodynamics (BLAST) program.  Technical Report,
          !   United States Army Construction Engineering Research Laboratory,
          !   Champaign, IL.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: CosTilt       ! Cosine of the surface tilt angle
    REAL(r64), INTENT(IN) :: Azimuth       ! or Facing, Direction the surface outward normal faces (degrees)
    REAL(r64), INTENT(IN) :: WindDirection ! Wind direction measured clockwise from geographhic North
    LOGICAL          :: AgainstWind   ! True for windward, false for leeward.

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: Diff ! Difference between the wind direction and the surface azimuth

    AgainstWind = .True.
    IF (ABS(CosTilt) < 0.98d0) THEN    ! Surface is not horizontal
       Diff = ABS(WindDirection - Azimuth)
       IF ((Diff-180.d0) > .001d0 ) Diff  = Diff - 360.0d0
       IF ((ABS(Diff)-100.d0) > .001d0) AgainstWind = .False. ! Surface is leeward
    ENDIF

  RETURN

END FUNCTION Windward


SUBROUTINE GetUserConvectionCoefficients

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 2003
          !       MODIFIED       November 2004; add more "user supplied convection coefficients"
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the object "Convection Coefficients" which
          ! can be specified by a user to override the "normally" calculated convection coefficients.  The
          ! change (November 2004) allows the user to specify down to the "surface level" the
          ! exterior or interior algorithm to be used.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! This routine gets the objects:
          !SurfaceProperty:ConvectionCoefficients,
          !      \memo Allow user settable interior and/or exterior convection coefficients.
          !      \memo Note that some other factors may limit the lower bounds for these values, such as
          !      \memo for windows, the interior convection coefficient must be >.28,
          !      \memo for trombe wall algorithm selection (zone), the interior convection coefficient must be >.1
          !      \memo for detailed interior convection, the lower limit is also .1
          !  A1, \field Surface Name
          !      \required-field
          !      \type object-list
          !      \object-list SurfaceNames
          !  A2, \field Convection Coefficient 1 Location
          !      \required-field
          !      \type choice
          !      \key Outside
          !      \key Inside
          !  A3, \field Convection Coefficient 1 Type
          !      \required-field
          !      \type choice
          !      \key Value
          !      \key Schedule
          !      \key Simple
          !      \key Detailed
          !      \key BLAST
          !      \key TARP
          !      \key DOE-2
          !      \key MoWitt
          !  N1, \field Convection Coefficient 1
          !      \note used if Convection Value Type=value, then minimum must be > 0.0, Maximum <= 1000.
          !      \units W/m2-K
          !  A4, \field Convection Coefficient 1 Schedule Name
          !       \note used if Convection Value Type=Schedule Name
          !       \type object-list
          !       \object-list ScheduleNames
          !   < Remainder fields are a repeat of A2, A3, N1, A4>
          !
          !SurfaceProperty:ConvectionCoefficients:MultipleSurface,
          !      \memo Allow user settable interior and/or exterior convection coefficients.
          !      \memo Note that some other factors may limit the lower bounds for these values, such as
          !      \memo for windows, the interior convection coefficient must be >.28,
          !      \memo for trombe wall algorithm selection (zone), the interior convection coefficient must be >.1
          !      \memo for detailed interior convection, the lower limit is also .1
          !  A1, \field Surface Type
          !      \required-field
          !      \type choice
          !      \key AllExteriorSurfaces
          !      \key AllExteriorWindows
          !      \key AllExteriorWalls
          !      \key AllExteriorRoofs
          !      \key AllExteriorFloors
          !      \key AllInteriorSurfaces
          !      \key AllInteriorWalls
          !      \key AllInteriorWindows
          !      \key AllInteriorCeilings
          !      \key AllInteriorFloors
          !  A2, \field Convection Coefficient 1 Location
          !      \required-field
          !      \type choice
          !      \key Outside
          !      \key Inside
          !  A3, \field Convection Coefficient 1 Type
          !      \required-field
          !      \type choice
          !      \key Value
          !      \key Schedule
          !      \key Simple
          !      \key Detailed
          !      \key BLAST
          !      \key TARP
          !      \key DOE-2
          !      \key MoWitt
          !  N1, \field Convection Coefficient 1
          !      \note used if Convection Value Type=value, then minimum must be > 0.0, Maximum <= 1000.
          !      \units W/m2-K
          !  A4, \field Convection Coefficient 1 Schedule Name
          !       \note used if Convection Coefficient Type=Schedule
          !       \type object-list
          !       \object-list ScheduleNames
          !   < Remainder fields are a repeat of A2, A3, N1, A4>

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, SameString
  USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax
  USE CurveManager,    ONLY: GetCurveIndex, GetCurveType
  USE DataErrorTracking, ONLY: TotalSevereErrors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
 CHARACTER(len=*), PARAMETER :: RoutineName='GetUserConvectionCoefficients'
 INTEGER, PARAMETER :: NumValidSurfaceTypes=11
 CHARACTER(len=*), PARAMETER :: ValidSurfaceTypes(11)=  &
            (/'ALLEXTERIORSURFACES',  &
              'ALLEXTERIORWINDOWS ',  &
              'ALLEXTERIORWALLS   ',  &
              'ALLEXTERIORROOFS   ',  &
              'ALLEXTERIORFLOORS  ',  &
              'ALLINTERIORSURFACES',  &
              'ALLINTERIORWINDOWS ',  &
              'ALLINTERIORWALLS   ',  &
              'ALLINTERIORROOFS   ',  &
              'ALLINTERIORCEILINGS',  &
              'ALLINTERIORFLOORS  '/)

 INTEGER, PARAMETER :: NumValidExtConvectionValueTypes=22
 CHARACTER(len=*), PARAMETER :: ValidExtConvectionValueTypes(22)=  &
             (/'VALUE                            ',  &
               'SCHEDULE                         ',  &
               'SIMPLECOMBINED                   ',  &
               'TARP                             ',  &
               'MOWITT                           ',  &
               'DOE-2                            ',  &
               'ADAPTIVECONVECTIONALGORITHM      ' , &
               'USERCURVE                        ' , &
               'ASHRAEVERTICALWALL               ' , &
               'WALTONUNSTABLEHORIZONTALORTILT   ' , &
               'WALTONSTABLEHORIZONTALORTILT     ' , &
               'NUSSELTJURGES                    ' ,&
               'MCADAMS                          ', &
               'MITCHELL                         ', &
               'CLEARROOF                        ', &
               'EMMELVERTICAL                    ', &
               'EMMELROOF                        ', &
               'ALAMDARIHAMMONDVERTICALWALL      ', &
               'FOHANNOPOLIDORIVERTICALWALL      ', &
               'ISO15099WINDOWS                  ', &
               'ALAMDARIHAMMONDSTABLEHORIZONTAL  ', &
               'ALAMDARIHAMMONDUNSTABLEHORIZONTAL'/)

 INTEGER, PARAMETER :: ExtConvectionValue(22)=  &
              (/-999                       , &
                -999                       , &
                ASHRAESimple               , &
                TarpHcOutside              , &
                MoWittHcOutside            , &
                DOE2HcOutside              , &
                AdaptiveConvectionAlgorithm, &
                HcExt_UserCurve            , &
                HcExt_NaturalASHRAEVerticalWall               , &
                HcExt_NaturalWaltonUnstableHorizontalOrTilt   , &
                HcExt_NaturalWaltonStableHorizontalOrTilt     , &
                HcExt_NusseltJurges                        , &
                HcExt_McAdams                             , &
                HcExt_Mitchell                             , &
                HcExt_ClearRoof                            , &
                HcExt_EmmelVertical                        , &
                HcExt_EmmelRoof                             , &
                HcExt_AlamdariHammondVerticalWall           , &
                HcExt_FohannoPolidoriVerticalWall           , &
                HcExt_ISO15099Windows                       , &
                HcExt_AlamdariHammondStableHorizontal        , &
                HcExt_AlamdariHammondUnstableHorizontal     /)


 INTEGER, PARAMETER :: NumValidSpecificExtWindConvValueTypes=15
 CHARACTER(len=*), PARAMETER :: ValidSpecificExtWindConvValueTypes(15)=  &
             (/'SIMPLECOMBINED         ', &
               'TARPWINDWARD           ', &
               'TARPLEEWARD            ', &
               'MOWITTWINDWARD         ', &
               'MOWITTLEEWARD          ', &
               'DOE2WINDWARD           ', &
               'DOE2LEEWARD            ', &
               'NUSSELTJURGES          ', &
               'MCADAMS                ', &
               'MITCHELL               ', &
               'EMMELVERTICAL          ', &
               'EMMELROOF              ', &
               'BLOCKENWINDWARD        ', &
               'CLEARROOF              ', &
               'USERCURVE              '/)
 INTEGER, PARAMETER :: MoreSpecificExtWindConvectionValue(15)=  &
              (/HcExt_ASHRAESimpleCombined   , &
                HcExt_SparrowWindward        , &
                HcExt_SparrowLeeward         , &
                HcExt_MoWiTTWindward         , &
                HcExt_DOE2Windward           , &
                HcExt_MoWiTTLeeward          , &
                HcExt_DOE2Leeward            , &
                HcExt_NusseltJurges          , &
                HcExt_McAdams                , &
                HcExt_Mitchell               , &
                HcExt_EmmelVertical          , &
                HcExt_EmmelRoof              , &
                HcExt_BlockenWindward        , &
                HcExt_ClearRoof              , &
                HcExt_UserCurve /)

 INTEGER, PARAMETER :: NumValidSpecificExtNatConvectValueTypes=10
 CHARACTER(len=*), PARAMETER :: ValidSpecificExtNatConvectValueTypes(10)=  &
             (/'ASHRAEVERTICALWALL                ', &
               'ALAMDARIHAMMONDVERTICALWALL       ', &
               'FOHANNOPOLIDORIVERTICALWALL       ', &
               'WALTONUNSTABLEHORIZONTALORTILT    ', &
               'WALTONSTABLEHORIZONTALORTILT      ', &
               'ALAMDARIHAMMONDSTABLEHORIZONTAL   ', &
               'ALAMDARIHAMMONDUNSTABLEHORIZONTAL ', &
               'ISO15099WINDOWS                   ', &
               'USERCURVE                         ', &
               'NONE                              ' /)
 INTEGER, PARAMETER :: SpecificExtNatConvectionValue(10)=  &
              (/HcExt_NaturalASHRAEVerticalWall            , &
                HcExt_AlamdariHammondVerticalWall          , &
                HcExt_FohannoPolidoriVerticalWall          , &
                HcExt_NaturalWaltonUnstableHorizontalOrTilt, &
                HcExt_NaturalWaltonStableHorizontalOrTilt  , &
                HcExt_AlamdariHammondStableHorizontal      , &
                HcExt_AlamdariHammondUnstableHorizontal    , &
                HcExt_ISO15099Windows                      , &
                HcExt_UserCurve                            , &
                HcExt_None  /)

 ! CeilingDiffuser and TrombeWall Interior types are only Zone Level settings.
 INTEGER, PARAMETER :: NumValidIntConvectionValueTypes=34
 CHARACTER(len=*), PARAMETER :: ValidIntConvectionValueTypes(34)=  &
             (/'VALUE                                  ',  &
               'SCHEDULE                               ',  &
               'SIMPLE                                 ',  &
               'TARP                                   ',  &
               'ADAPTIVECONVECTIONALGORITHM            ',  &
               'USERCURVE                              ',  &
               'ASHRAEVERTICALWALL                     ',  &
               'WALTONUNSTABLEHORIZONTALORTILT         ',  &
               'WALTONSTABLEHORIZONTALORTILT           ',  &
               'FISHERPEDERSENCEILINGDIFFUSERWALLS     ',  &
               'FISHERPEDERSENCEILINGDIFFUSERCEILING   ',  &
               'FISHERPEDERSENCEILINGDIFFUSERFLOOR     ',  &
               'ALAMDARIHAMMONDSTABLEHORIZONTAL        ',  &
               'ALAMDARIHAMMONDUNSTABLEHORIZONTAL      ',  &
               'ALAMDARIHAMMONDVERTICALWALL            ',  &
               'KHALIFAEQ3WALLAWAYFROMHEAT             ',  &
               'KHALIFAEQ4CEILINGAWAYFROMHEAT          ',  &
               'KHALIFAEQ5WALLNEARHEAT                 ',  &
               'KHALIFAEQ6NONHEATEDWALLS               ',  &
               'KHALIFAEQ7CEILING                      ',  &
               'AWBIHATTONHEATEDFLOOR                  ',  &
               'AWBIHATTONHEATEDWALL                   ',  &
               'BEAUSOLEILMORRISONMIXEDASSISTEDWALL    ',  &
               'BEAUSOLEILMORRISONMIXEDOPPOSINGWALL    ',  &
               'BEAUSOLEILMORRISONMIXEDSTABLEFLOOR     ',  &
               'BEAUSOLEILMORRISONMIXEDUNSTABLEFLOOR   ',  &
               'BEAUSOLEILMORRISONMIXEDSTABLECEILING   ',  &
               'BEAUSOLEILMORRISONMIXEDUNSTABLECEILING ',  &
               'FOHANNOPOLIDORIVERTICALWALL            ',  &
               'KARADAGCHILLEDCEILING                  ',  &
               'ISO15099WINDOWS                        ',  &
               'GOLDSTEINNOVOSELACCEILINGDIFFUSERWINDOW',  &
               'GOLDSTEINNOVOSELACCEILINGDIFFUSERWALLS ',  &
               'GOLDSTEINNOVOSELACCEILINGDIFFUSERFLOOR '/)
 INTEGER, PARAMETER :: IntConvectionValue(34)=  &
              (/-999,                                        &
                -999,                                        &
               ASHRAESimple,                                 &
               ASHRAETARP,                                   &
               AdaptiveConvectionAlgorithm,                  &
               HcInt_UserCurve,                              &
               HcInt_ASHRAEVerticalWall,                     &
               HcInt_WaltonUnstableHorizontalOrTilt,         &
               HcInt_WaltonStableHorizontalOrTilt,           &
               HcInt_FisherPedersenCeilDiffuserWalls,        &
               HcInt_FisherPedersenCeilDiffuserCeiling,      &
               HcInt_FisherPedersenCeilDiffuserFloor,        &
               HcInt_AlamdariHammondStableHorizontal,        &
               HcInt_AlamdariHammondUnstableHorizontal,      &
               HcInt_AlamdariHammondVerticalWall,            &
               HcInt_KhalifaEq3WallAwayFromHeat,             &
               HcInt_KhalifaEq4CeilingAwayFromHeat,          &
               HcInt_KhalifaEq5WallNearHeat,                 &
               HcInt_KhalifaEq6NonHeatedWalls,               &
               HcInt_KhalifaEq7Ceiling,                      &
               HcInt_AwbiHattonHeatedFloor,                  &
               HcInt_AwbiHattonHeatedWall,                   &
               HcInt_BeausoleilMorrisonMixedAssistingWall,   &
               HcInt_BeausoleilMorrisonMixedOppossingWall,   &
               HcInt_BeausoleilMorrisonMixedStableFloor,     &
               HcInt_BeausoleilMorrisonMixedUnstableFloor,   &
               HcInt_BeausoleilMorrisonMixedStableCeiling,   &
               HcInt_BeausoleilMorrisonMixedUnstableCeiling, &
               HcInt_FohannoPolidoriVerticalWall,            &
               HcInt_KaradagChilledCeiling,                  &
               HcInt_ISO15099Windows,                        &
               HcInt_GoldsteinNovoselacCeilingDiffuserWindow,&
               HcInt_GoldsteinNovoselacCeilingDiffuserWalls, &
               HcInt_GoldsteinNovoselacCeilingDiffuserFloor/)


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: Alphas(9)
  REAL(r64) :: Numbers(2)
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: Count
  INTEGER :: Status
  INTEGER :: Found
  LOGICAL :: ErrorsFound=.false.
  LOGICAL :: ErrFlag=.false.
  LOGICAL :: IsValidType=.false.
  INTEGER :: ExtValue
  INTEGER :: IntValue
  INTEGER :: Ptr
  INTEGER :: Pass
  INTEGER :: FieldNo
  CHARACTER(len=25) :: CFieldNo
  INTEGER :: NumField
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject
  INTEGER :: PotentialAssignedValue
  INTEGER :: ZoneNum
  INTEGER :: SurfNum

  ! first get user-defined H models so they can be processed for later objects
  CurrentModuleObject='SurfaceConvectionAlgorithm:Inside:UserCurve'
  TotInsideHcUserCurves = GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(HcInsideUserCurve(TotInsideHcUserCurves))
  DO Loop=1,TotInsideHcUserCurves
    CALL GetObjectItem(CurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    HcInsideUserCurve(loop)%Name = cAlphaArgs(1)
    SELECT CASE (cAlphaArgs(2))
    CASE ('MEANAIRTEMPERATURE')
      HcInsideUserCurve(loop)%ReferenceTempType  = RefTempMeanAirTemp
    CASE ('ADJACENTAIRTEMPERATURE')
      HcInsideUserCurve(loop)%ReferenceTempType  = RefTempAdjacentAirTemp
    CASE ('SUPPLYAIRTEMPERATURE')
      HcInsideUserCurve(loop)%ReferenceTempType  = RefTempSupplyAirTemp
    CASE DEFAULT
      CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                          'Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    END SELECT

    IF (.NOT. lAlphaFieldBlanks(3)) THEN
      HcInsideUserCurve(loop)%HcFnTempDiffCurveNum = GetCurveIndex(cAlphaArgs(3))
      IF (HcInsideUserCurve(loop)%HcFnTempDiffCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                                 'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcInsideUserCurve(loop)%HcFnTempDiffCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(3))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(3)))))
          ErrorsFound=.true.
        END SELECT

      ENDIF
    ELSE
      HcInsideUserCurve(loop)%HcFnTempDiffCurveNum = 0
    ENDIF

    IF (.NOT. lAlphaFieldBlanks(4)) THEN
      HcInsideUserCurve(loop)%HcFnTempDiffDivHeightCurveNum = GetCurveIndex(cAlphaArgs(4))
      IF (HcInsideUserCurve(loop)%HcFnTempDiffDivHeightCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                                 'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcInsideUserCurve(loop)%HcFnTempDiffDivHeightCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(4))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(4)))))
          ErrorsFound=.true.
        END SELECT
      ENDIF
    ELSE
      HcInsideUserCurve(loop)%HcFnTempDiffDivHeightCurveNum = 0
    ENDIF

    IF (.NOT. lAlphaFieldBlanks(5)) THEN
      HcInsideUserCurve(loop)%HcFnACHCurveNum = GetCurveIndex(cAlphaArgs(5))
      IF (HcInsideUserCurve(loop)%HcFnACHCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                                 'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcInsideUserCurve(loop)%HcFnACHCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(5))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(5)))))
          ErrorsFound=.true.
        END SELECT
      ENDIF
    ELSE
      HcInsideUserCurve(loop)%HcFnACHCurveNum = 0
    ENDIF

    IF (.NOT. lAlphaFieldBlanks(6)) THEN
      HcInsideUserCurve(loop)%HcFnACHDivPerimLengthCurveNum = GetCurveIndex(cAlphaArgs(6))
      IF (HcInsideUserCurve(loop)%HcFnACHDivPerimLengthCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                             'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcInsideUserCurve(loop)%HcFnACHDivPerimLengthCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(6))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(6)))))
          ErrorsFound=.true.
        END SELECT
      ENDIF
    ELSE
      HcInsideUserCurve(loop)%HcFnACHDivPerimLengthCurveNum = 0
    ENDIF


  ENDDO !end of 'SurfaceConvectionAlgorithm:Inside:UserCurve'

  CurrentModuleObject='SurfaceConvectionAlgorithm:Outside:UserCurve'
  TotOutsideHcUserCurves = GetNumObjectsFound(CurrentModuleObject)
  ALLOCATE(HcOutsideUserCurve(TotOutsideHcUserCurves))
  DO Loop=1,TotOutsideHcUserCurves
    CALL GetObjectItem(CurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    HcOutsideUserCurve(loop)%Name = cAlphaArgs(1)

    SELECT CASE ( cAlphaArgs(2) )

    CASE ('WEATHERFILE')
      HcOutsideUserCurve(loop)%WindSpeedType = RefWindWeatherFile
    CASE ('HEIGHTADJUST')
      HcOutsideUserCurve(loop)%WindSpeedType = RefWindAtZ
    CASE ('PARALLELCOMPONENT')
      HcOutsideUserCurve(loop)%WindSpeedType = RefWindParallComp
    CASE ('PARALLELCOMPONENTHEIGHTADJUST')
      HcOutsideUserCurve(loop)%WindSpeedType = RefWindParallCompAtZ
    CASE DEFAULT
      CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                            'Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    END SELECT

    ! A3 , \field Hf Function of Wind Speed Curve Name
    IF (.NOT. lAlphaFieldBlanks(3)) THEN
      HcOutsideUserCurve(loop)%HfFnWindSpeedCurveNum = GetCurveIndex(cAlphaArgs(3))
      IF (HcOutsideUserCurve(loop)%HfFnWindSpeedCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                              'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcOutsideUserCurve(loop)%HfFnWindSpeedCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(3))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(3)))))
          ErrorsFound=.true.
        END SELECT
      ENDIF
    ELSE
      HcOutsideUserCurve(loop)%HfFnWindSpeedCurveNum = 0
    ENDIF

    !  A4 , \field Hn Function of Temperature Difference Curve Name
    IF (.NOT. lAlphaFieldBlanks(4)) THEN
      HcOutsideUserCurve(loop)%HnFnTempDiffCurveNum = GetCurveIndex(cAlphaArgs(4))
      IF (HcOutsideUserCurve(loop)%HnFnTempDiffCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                              'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcOutsideUserCurve(loop)%HnFnTempDiffCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(4))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(4)))))
          ErrorsFound=.true.
        END SELECT
      ENDIF
    ELSE
      HcOutsideUserCurve(loop)%HnFnTempDiffCurveNum = 0
    ENDIF

    !  A5 , \field Hn Function of Temperature Difference Divided by Height Curve Name
    IF (.NOT. lAlphaFieldBlanks(5)) THEN
      HcOutsideUserCurve(loop)%HnFnTempDiffDivHeightCurveNum = GetCurveIndex(cAlphaArgs(5))
      IF (HcOutsideUserCurve(loop)%HnFnTempDiffDivHeightCurveNum == 0) THEN
        CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//': '// &
                              'Invalid Name Entered, for '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
        ErrorsFound=.true.
      ELSE ! check type
        SELECT CASE (GetCurveType(HcOutsideUserCurve(loop)%HnFnTempDiffDivHeightCurveNum))

        CASE ('LINEAR', 'QUADRATIC', 'CUBIC', 'EXPONENT', 'QUARTIC')
         !okay do nothing, have the right type of curve (single independent variable)
        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFieldNames(5))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(cAlphaArgs(5)))))
          ErrorsFound=.true.
        END SELECT
      ENDIF
    ELSE
      HcOutsideUserCurve(loop)%HnFnTempDiffDivHeightCurveNum = 0
    ENDIF


  ENDDO ! 'SurfaceConvectionAlgorithm:Outside:UserCurve'



  ! now get user directed overrides at the surface level.
  TotIntConvCoeff=0
  TotExtConvCoeff=0
  CurrentModuleObject='SurfaceProperty:ConvectionCoefficients:MultipleSurface'
  Count=GetNumObjectsFound(CurrentModuleObject)
  DO Loop=1,Count
    CALL GetObjectItem(CurrentModuleObject,Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IF (Alphas(2) == 'INSIDE') THEN
      TotIntConvCoeff=TotIntConvCoeff+1
    ENDIF
    IF (Alphas(6) == 'INSIDE') THEN
      TotIntConvCoeff=TotIntConvCoeff+1
    ENDIF
    IF (Alphas(2) == 'OUTSIDE') THEN
      TotExtConvCoeff=TotExtConvCoeff+1
    ENDIF
    IF (Alphas(6) == 'OUTSIDE') THEN
      TotExtConvCoeff=TotExtConvCoeff+1
    ENDIF
    IF (NumAlphas >= 2 .and. lAlphaFieldBlanks(2)) THEN
      CALL ShowWarningError('GetUserConvectionCoefficients: '//TRIM(CurrentModuleObject)//', '//  &
                        ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(Alphas(1)))
      CALL ShowContinueError(trim(cAlphaFieldNames(2))//' is blank and rest of fields will not be processed.')
    ENDIF
    IF (NumAlphas >= 6 .and. lAlphaFieldBlanks(6)) THEN
      CALL ShowWarningError('GetUserConvectionCoefficients: '//TRIM(CurrentModuleObject)//', '//  &
                        ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(Alphas(1)))
      CALL ShowContinueError(trim(cAlphaFieldNames(6))//' is blank and rest of fields will not be processed.')
    ENDIF
  ENDDO
  CurrentModuleObject='SurfaceProperty:ConvectionCoefficients'
  Count=GetNumObjectsFound(CurrentModuleObject)
  DO Loop=1,Count
    CALL GetObjectItem(CurrentModuleObject,Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IF (Alphas(2) == 'INSIDE') THEN
      TotIntConvCoeff=TotIntConvCoeff+1
    ENDIF
    IF (Alphas(6) == 'INSIDE') THEN
      TotIntConvCoeff=TotIntConvCoeff+1
    ENDIF
    IF (Alphas(2) == 'OUTSIDE') THEN
      TotExtConvCoeff=TotExtConvCoeff+1
    ENDIF
    IF (Alphas(6) == 'OUTSIDE') THEN
      TotExtConvCoeff=TotExtConvCoeff+1
    ENDIF
    IF (NumAlphas >= 2 .and. lAlphaFieldBlanks(2)) THEN
      CALL ShowWarningError('GetUserConvectionCoefficients: '//TRIM(CurrentModuleObject)//', '//  &
                        ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(Alphas(1)))
      CALL ShowContinueError(trim(cAlphaFieldNames(2))//' is blank and rest of fields will not be processed.')
    ENDIF
    IF (NumAlphas >= 6 .and. lAlphaFieldBlanks(6)) THEN
      CALL ShowWarningError('GetUserConvectionCoefficients: '//TRIM(CurrentModuleObject)//', '//  &
                        ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(Alphas(1)))
      CALL ShowContinueError(trim(cAlphaFieldNames(6))//' is blank and rest of fields will not be processed.')
    ENDIF
  ENDDO

  ALLOCATE(UserIntConvectionCoeffs(TotIntConvCoeff))
  ALLOCATE(UserExtConvectionCoeffs(TotExtConvCoeff))

  TotIntConvCoeff=0
  TotExtConvCoeff=0

  !   Now, get for real and check for consistency
  CurrentModuleObject='SurfaceProperty:ConvectionCoefficients'
  Count=GetNumObjectsFound(CurrentModuleObject)
  DO Loop=1,Count
    CALL GetObjectItem(CurrentModuleObject,Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    Found=FindItemInList(Alphas(1),Surface%Name,TotSurfaces)
    IF (Found == 0) THEN
      CALL ShowSevereError('GetUserConvectionCoefficients: '//TRIM(CurrentModuleObject)//', '//  &
                           'illegal value for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(Alphas(1)))
      ErrorsFound=.true.
      CYCLE
    ENDIF

    Ptr=2
    FieldNo=2
    NumField=1
    DO Pass=1,2

      SELECT CASE(Alphas(Ptr))
        CASE ('OUTSIDE')
          IF (Surface(Found)%OSCPtr > 0) THEN
            CALL ShowSevereError('GetUserSuppliedConvectionCoefficients: '//TRIM(CurrentModuleObject)//', '//  &
                  'OUTSIDE '//TRIM(CurrentModuleObject)//' cannot be specified for OtherSideCoefficient Surface='//  &
                  TRIM(Alphas(1)))
            ErrorsFound=.true.
          ENDIF
          IsValidType=.false.
          ExtValue=0
          PotentialAssignedValue=0
          DO Loop1=1,NumValidExtConvectionValueTypes
            IF (Alphas(Ptr+1) /= ValidExtConvectionValueTypes(Loop1)) CYCLE
            ExtValue=ExtConvectionValue(Loop1)
            IsValidType=.true.
            EXIT
          ENDDO

          IF (IsValidType .and. (Loop1 > 2) .AND. (Loop1 <= 7)) THEN
            PotentialAssignedValue=-ExtValue
          ELSEIF (IsValidType .and. Loop1 == 1) THEN  ! Value
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(1)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=Found
            IF (Numbers(NumField) < LowHConvLimit .or. Numbers(NumField) > HighHConvLimit) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', out of range value')
              CALL ShowContinueError(trim(cAlphaFieldNames(Ptr))//'='//trim(Alphas(Ptr))//', '//  &
                 trim(cNumericFieldNames(NumField))//'=['//trim(RoundSigDigits(Numbers(NumField),5))//'].')
              CALL ShowContinueError('Out-of-range from low/high limits=[>='//trim(RoundSigDigits(LowHConvLimit,9))//', '//  &
                 '<='//trim(RoundSigDigits(HighHConvLimit,1))//'].')
!            CALL RangeCheck(ErrFlag,'"'//trim(cAlphaFieldNames(FieldNo+1))//'"','object',  &
!                       'SEVERE','>='//trim(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
!                       '<='//trim(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
              CALL ShowContinueError('Limits are set (or default) in HeatBalanceAlgorithm object.')
              ErrorsFound=.true.
              ErrFlag=.false.
            ENDIF
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefValue
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideValue=Numbers(NumField)
            IF (.not. lAlphaFieldBlanks(Ptr+2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', duplicate value')
              CALL ShowContinueError('Since VALUE is used for "'//trim(cAlphaFieldNames(FieldNo+2))//  &
                                    '", '//TRIM(cAlphaFieldNames(Ptr+2))//'='//  &
                                     TRIM(Alphas(Ptr+2))//' is ignored.')
            ENDIF
            PotentialAssignedValue=TotExtConvCoeff
          ELSEIF (IsValidType .and. Loop1 == 2) THEN  ! Schedule
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(1)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=Found
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefSchedule
            UserExtConvectionCoeffs(TotExtConvCoeff)%ScheduleIndex=GetScheduleIndex(Alphas(Ptr+2))
            IF (UserExtConvectionCoeffs(TotExtConvCoeff)%ScheduleIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+2))//' entered='//TRIM(Alphas(Ptr+2)))
              ErrorsFound=.true.
            ELSE
              UserExtConvectionCoeffs(TotExtConvCoeff)%ScheduleName=Alphas(Ptr+2)
            ENDIF
            PotentialAssignedValue=TotExtConvCoeff
          ELSEIF (IsValidType .AND. ExtValue == HcExt_UserCurve) THEN ! User curve

            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(1)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=Found
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefUserCurve
            UserExtConvectionCoeffs(TotExtConvCoeff)%UserCurveIndex = FindItemInList(Alphas(Ptr+3), &
                                                     HcOutsideUserCurve%Name, TotOutsideHcUserCurves)
            IF (UserExtConvectionCoeffs(TotExtConvCoeff)%UserCurveIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+3))//' entered='//TRIM(Alphas(Ptr+3)))
              ErrorsFound = .true.
            ENDIF
            PotentialAssignedValue=TotExtConvCoeff
          ELSEIF (IsValidType .AND. ExtValue > HcExt_UserCurve) THEN
            ! specificmodel
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(1)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=Found
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefSpecifiedModel
            UserExtConvectionCoeffs(TotExtConvCoeff)%HcModelEq = ExtValue
            PotentialAssignedValue = TotExtConvCoeff

          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', check input')
            CALL ShowContinueError('Check Input Entered :'//TRIM(Alphas(Ptr+1)))
            ErrorsFound=.true.
          ENDIF
          IF (Surface(Found)%ExtConvCoeff /= 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
            CALL ShowContinueError('Duplicate (Outside) assignment attempt')
            ErrorsFound=.true.
          ELSE
            Surface(Found)%ExtConvCoeff=PotentialAssignedValue
          ENDIF

        CASE ('INSIDE')
          IsValidType=.false.
          IntValue=0
          PotentialAssignedValue=0
          DO Loop1=1,NumValidIntConvectionValueTypes
            IF (Alphas(Ptr+1) /= ValidIntConvectionValueTypes(Loop1)) CYCLE
            IntValue=IntConvectionValue(Loop1)
            IsValidType=.true.
            EXIT
          ENDDO

          IF (IsValidType .AND. (Loop1 > 2) .AND. (Loop1 <= 5)) THEN
            PotentialAssignedValue=-IntValue
          ELSEIF (IsValidType .and. Loop1 == 1) THEN  ! Value
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(1)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=Found
            IF (Numbers(NumField) < LowHConvLimit .or. Numbers(NumField) > HighHConvLimit) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', out of range value')
              CALL ShowContinueError(trim(cAlphaFieldNames(Ptr))//'='//trim(Alphas(Ptr))//', '//  &
                 trim(cNumericFieldNames(NumField))//'=['//trim(RoundSigDigits(Numbers(NumField),5))//'].')
              CALL ShowContinueError('Out-of-range from low/high limits=[>='//trim(RoundSigDigits(LowHConvLimit,9))//', '//  &
                 '<='//trim(RoundSigDigits(HighHConvLimit,1))//'].')
!            CALL RangeCheck(ErrFlag,'"'//trim(cAlphaFieldNames(FieldNo+1))//'"','object',  &
!                       'SEVERE','>='//trim(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
!                       '<='//trim(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
              CALL ShowContinueError('Limits are set (or default) in HeatBalanceAlgorithm object.')
              ErrorsFound=.true.
              ErrFlag=.false.
            ENDIF
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefValue
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideValue=Numbers(NumField)
            IF (.not. lAlphaFieldBlanks(Ptr+2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', duplicate value')
              CALL ShowContinueError('Since VALUE is used for "'//trim(cAlphaFieldNames(FieldNo+1))//  &
                                    '", '//TRIM(cAlphaFieldNames(Ptr+2))//'='//  &
                                     TRIM(Alphas(Ptr+2))//' is ignored.')
            ENDIF
            PotentialAssignedValue=TotIntConvCoeff
          ELSEIF (IsValidType .and. Loop1 == 2) THEN  ! Schedule
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(1)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=Found
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefSchedule
            UserIntConvectionCoeffs(TotIntConvCoeff)%ScheduleIndex=GetScheduleIndex(Alphas(Ptr+2))
            IF (UserIntConvectionCoeffs(TotIntConvCoeff)%ScheduleIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+2))//' entered='//TRIM(Alphas(Ptr+2)))
              ErrorsFound=.true.
            ELSE
              UserIntConvectionCoeffs(TotIntConvCoeff)%ScheduleName=Alphas(Ptr+2)
            ENDIF
            PotentialAssignedValue=TotIntConvCoeff
          ELSEIF ( IsValidType .AND. IntValue == HcInt_UserCurve) THEN
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(1)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=Found
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefUserCurve
            UserIntConvectionCoeffs(TotIntConvCoeff)%UserCurveIndex = FindItemInList(Alphas(Ptr+3), &
                                                                     HcInsideUserCurve%Name, TotInsideHcUserCurves)
            IF (UserIntConvectionCoeffs(TotIntConvCoeff)%UserCurveIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+3))//' entered='//TRIM(Alphas(Ptr+3)))
              ErrorsFound=.true.
            ENDIF
            PotentialAssignedValue=TotIntConvCoeff
          ELSEIF (IsValidType .AND. IntValue > HcInt_UserCurve) THEN
            ! specificmodel
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(1)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=Found
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefSpecifiedModel
            UserIntConvectionCoeffs(TotIntConvCoeff)%HcModelEq   = IntValue
            PotentialAssignedValue = TotIntConvCoeff


          ELSE
            ! treat CeilingDiffuser and TrombeWall special
            IF (SameString(Alphas(Ptr+1),'CEILINGDIFFUSER') .or. SameString(Alphas(Ptr+1),'TROMBEWALL')) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError('Invalid Value Entered, for '//TRIM(cAlphaFieldNames(Ptr))//'='//TRIM(Alphas(Ptr)))
              CALL ShowContinueError('invalid value in '//TRIM(cAlphaFieldNames(Ptr+1))//'='//TRIM(Alphas(Ptr+1))// &
                                     '". This type is only applicable at a Zone level.')
              ErrorsFound=.true.
            ELSE  ! really invalid
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError('Invalid Value Entered, for '//TRIM(cAlphaFieldNames(Ptr))//'='//TRIM(Alphas(Ptr)))
              CALL ShowContinueError('invalid value in '//TRIM(cAlphaFieldNames(Ptr+1))//'='//TRIM(Alphas(Ptr+1)))
              ErrorsFound=.true.
            ENDIF
          ENDIF
          IF (Surface(Found)%IntConvCoeff /= 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', duplicate (inside)')
            CALL ShowContinueError('Duplicate (Inside) assignment attempt.')
            ErrorsFound=.true.
          ELSE
            Surface(Found)%IntConvCoeff=PotentialAssignedValue
          ENDIF

        CASE (' ')  ! Blank

        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
          CALL ShowContinueError('Invalid Value Entered, for '//TRIM(cAlphaFieldNames(Ptr))//'='//TRIM(Alphas(Ptr)))
          ErrorsFound=.true.
      END SELECT

      Ptr=Ptr+4
      FieldNo=FieldNo+4
      NumField=NumField+1
    ENDDO
  ENDDO

  CurrentModuleObject='SurfaceProperty:ConvectionCoefficients:MultipleSurface'
  Count=GetNumObjectsFound(CurrentModuleObject)
  DO Loop=1,Count
    CALL GetObjectItem(CurrentModuleObject,Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ! Check Field 1 for validity
    IsValidType=.false.
    DO Loop1=1,NumValidSurfaceTypes
      IF (Alphas(1) /= ValidSurfaceTypes(Loop1)) CYCLE
      IsValidType=.true.
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
      CALL ShowContinueError('illegal value for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(Alphas(1)))
      ErrorsFound=.true.
      CYCLE
    ENDIF
    Ptr=2
    FieldNo=2
    NumField=1
    DO Pass=1,2

      SELECT CASE(Alphas(Ptr))
        CASE ('OUTSIDE')
          IsValidType=.false.
          DO Loop1=1,NumValidExtConvectionValueTypes
            IF (Alphas(Ptr+1) /= ValidExtConvectionValueTypes(Loop1)) CYCLE
            ExtValue=ExtConvectionValue(Loop1)
            IsValidType=.true.
            EXIT
          ENDDO

          IF (IsValidType .and. (Loop1 > 2) .AND. (Loop1 <=7)) THEN
            CALL ApplyConvectionValue(Alphas(1),'OUTSIDE',-ExtValue)
          ELSEIF (IsValidType .and. Loop1 == 1) THEN  ! Value
            ! SimpleValueAssignment via UserExtConvectionCoeffs array
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(Ptr)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=-999
            IF (Numbers(NumField) < LowHConvLimit .or. Numbers(NumField) > HighHConvLimit) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', out of range value')
              CALL ShowContinueError(trim(cAlphaFieldNames(Ptr))//'='//trim(Alphas(Ptr))//', '//  &
                 trim(cNumericFieldNames(NumField))//'=['//trim(RoundSigDigits(Numbers(NumField),5))//'].')
              CALL ShowContinueError('Out-of-range from low/high limits=[>='//trim(RoundSigDigits(LowHConvLimit,9))//', '//  &
                 '<='//trim(RoundSigDigits(HighHConvLimit,1))//'].')
!            CALL RangeCheck(ErrFlag,'"'//trim(cAlphaFieldNames(FieldNo+1))//'"','object',  &
!                       'SEVERE','>='//trim(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
!                       '<='//trim(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
              CALL ShowContinueError('Limits are set (or default) in HeatBalanceAlgorithm object.')
              ErrorsFound=.true.
              ErrFlag=.false.
            ENDIF
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefValue
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideValue=Numbers(NumField)
            IF (.not. lAlphaFieldBlanks(Ptr+2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', duplicate value')
              CALL ShowContinueError('Since VALUE is used for "'//trim(cAlphaFieldNames(FieldNo+2))//  &
                                    '", '//TRIM(cAlphaFieldNames(Ptr+2))//'='//  &
                                     TRIM(Alphas(Ptr+2))//' is ignored.')
            ENDIF
            CALL ApplyConvectionValue(Alphas(1),'OUTSIDE',TotExtConvCoeff)
          ELSEIF (IsValidType .and. Loop1 == 2) THEN  ! Schedule
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(Ptr)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=-999
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefSchedule
            UserExtConvectionCoeffs(TotExtConvCoeff)%ScheduleIndex=GetScheduleIndex(Alphas(Ptr+2))
            IF (UserExtConvectionCoeffs(TotExtConvCoeff)%ScheduleIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+2))//' entered='//TRIM(Alphas(Ptr+2)))
              ErrorsFound=.true.
            ELSE
              UserExtConvectionCoeffs(TotExtConvCoeff)%ScheduleName=Alphas(Ptr+2)
            ENDIF
            CALL ApplyConvectionValue(Alphas(1),'OUTSIDE',TotExtConvCoeff)
          ELSEIF (IsValidType .AND. ExtValue == HcExt_UserCurve) THEN ! User curve
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(Ptr)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=-999
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefUserCurve
            UserExtConvectionCoeffs(TotExtConvCoeff)%UserCurveIndex = FindItemInList(Alphas(Ptr+3), &
                                                     HcOutsideUserCurve%Name, TotOutsideHcUserCurves)
            IF (UserExtConvectionCoeffs(TotExtConvCoeff)%UserCurveIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+3))//' entered='//TRIM(Alphas(Ptr+3)))
              ErrorsFound = .true.
            ENDIF
            PotentialAssignedValue=TotExtConvCoeff
            CALL ApplyConvectionValue(Alphas(1),'OUTSIDE',TotExtConvCoeff)

          ELSEIF (IsValidType .AND. ExtValue > HcExt_UserCurve) THEN
            ! specificmodel
            TotExtConvCoeff=TotExtConvCoeff+1
            UserExtConvectionCoeffs(TotExtConvCoeff)%SurfaceName=Alphas(Ptr)
            UserExtConvectionCoeffs(TotExtConvCoeff)%WhichSurface=-999
            UserExtConvectionCoeffs(TotExtConvCoeff)%OverrideType=ConvCoefSpecifiedModel
            UserExtConvectionCoeffs(TotExtConvCoeff)%HcModelEq = ExtValue
            PotentialAssignedValue = TotExtConvCoeff
            CALL ApplyConvectionValue(Alphas(1),'OUTSIDE',TotExtConvCoeff)
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', check input')
            CALL ShowContinueError('Check Input Entered :'//TRIM(Alphas(Ptr+1)))
            ErrorsFound=.true.
          ENDIF

        CASE ('INSIDE')
          IsValidType=.false.
          DO Loop1=1,NumValidIntConvectionValueTypes
            IF (Alphas(Ptr+1) /= ValidIntConvectionValueTypes(Loop1)) CYCLE
            IntValue=IntConvectionValue(Loop1)
            IsValidType=.true.
            EXIT
          ENDDO

          IF (IsValidType .AND. (Loop1 > 2 ).AND. (Loop1 <= 5)) THEN
            CALL ApplyConvectionValue(Alphas(1),'INSIDE',-IntValue)
          ELSEIF (IsValidType .and. Loop1 == 1) THEN  ! Value
            ! SimpleValueAssignment via UserExtConvectionCoeffs array
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(Ptr)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=-999
            IF (Numbers(NumField) < LowHConvLimit .or. Numbers(NumField) > HighHConvLimit) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', out of range value')
              CALL ShowContinueError(trim(cAlphaFieldNames(Ptr))//'='//trim(Alphas(Ptr))//', '//  &
                 trim(cNumericFieldNames(NumField))//'=['//trim(RoundSigDigits(Numbers(NumField),5))//'].')
              CALL ShowContinueError('Out-of-range from low/high limits=[>='//trim(RoundSigDigits(LowHConvLimit,9))//', '//  &
                 '<='//trim(RoundSigDigits(HighHConvLimit,1))//'].')
!            CALL RangeCheck(ErrFlag,'"'//trim(cAlphaFieldNames(FieldNo+1))//'"','object',  &
!                       'SEVERE','>='//trim(RoundSigDigits(LowHConvLimit,9)),(Numbers(NumField)>=LowHConvLimit),&
!                       '<='//trim(RoundSigDigits(HighHConvLimit,1)),(Numbers(NumField)<=HighHConvLimit))
              CALL ShowContinueError('Limits are set (or default) in HeatBalanceAlgorithm object.')
              ErrorsFound=.true.
              ErrFlag=.false.
            ENDIF
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefValue
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideValue=Numbers(NumField)
            IF (.not. lAlphaFieldBlanks(Ptr+2)) THEN
              CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', duplicate value')
              CALL ShowContinueError('Since VALUE is used for "'//trim(cAlphaFieldNames(FieldNo+2))//  &
                                    '", '//TRIM(cAlphaFieldNames(Ptr+2))//'='//  &
                                     TRIM(Alphas(Ptr+2))//' is ignored.')
            ENDIF
            CALL ApplyConvectionValue(Alphas(1),'INSIDE',TotIntConvCoeff)
          ELSEIF (IsValidType .and. Loop1 == 2) THEN  ! Schedule
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(Ptr)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=-999
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefSchedule
            UserIntConvectionCoeffs(TotIntConvCoeff)%ScheduleIndex=GetScheduleIndex(Alphas(Ptr+2))
            IF (UserIntConvectionCoeffs(TotIntConvCoeff)%ScheduleIndex == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+2))//' entered='//TRIM(Alphas(Ptr+2)))
              ErrorsFound=.true.
            ELSE
              UserIntConvectionCoeffs(TotIntConvCoeff)%ScheduleName=Alphas(Ptr+2)
            ENDIF
            CALL ApplyConvectionValue(Alphas(1),'INSIDE',TotIntConvCoeff)
          ELSEIF ( IsValidType .AND. IntValue == HcInt_UserCurve) THEN
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(Ptr)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=-999
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefUserCurve
            UserIntConvectionCoeffs(TotIntConvCoeff)%UserCurveIndex = FindItemInList(Alphas(Ptr+3), &
                                                                     HcInsideUserCurve%Name, TotInsideHcUserCurves)
            IF (UserIntConvectionCoeffs(TotIntConvCoeff)%UserCurveIndex == 0) THEN

              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+3))//' entered='//TRIM(Alphas(Ptr+3)))
              ErrorsFound=.true.
            ENDIF
            PotentialAssignedValue=TotIntConvCoeff
            CALL ApplyConvectionValue(Alphas(1),'INSIDE',TotIntConvCoeff)
          ELSEIF (IsValidType .AND. IntValue > HcInt_UserCurve) THEN
            ! specificmodel
            TotIntConvCoeff=TotIntConvCoeff+1
            UserIntConvectionCoeffs(TotIntConvCoeff)%SurfaceName=Alphas(Ptr)
            UserIntConvectionCoeffs(TotIntConvCoeff)%WhichSurface=-999
            UserIntConvectionCoeffs(TotIntConvCoeff)%OverrideType=ConvCoefSpecifiedModel
            UserIntConvectionCoeffs(TotIntConvCoeff)%HcModelEq   = IntValue
            PotentialAssignedValue = TotIntConvCoeff
            CALL ApplyConvectionValue(Alphas(1),'INSIDE',TotIntConvCoeff)

          ELSE
            ! treat CeilingDiffuser and TrombeWall special
            IF (SameString(Alphas(Ptr+1),'CEILINGDIFFUSER') .or. SameString(Alphas(Ptr+1),'TROMBEWALL')) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr))//' entered='//TRIM(Alphas(Ptr)))
              CALL ShowContinueError('invalid value in '//TRIM(cAlphaFieldNames(Ptr+1))//'='//TRIM(Alphas(Ptr+1))// &
                                     '". This type is only applicable at a Zone level.')
              ErrorsFound=.true.
            ELSE  ! really invalid
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
              CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr+1))//' entered='//TRIM(Alphas(Ptr+1)))
              ErrorsFound=.true.
            ENDIF
          ENDIF

        CASE (' ')  ! Blank

        CASE DEFAULT  ! Error Case
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
          CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(Ptr))//' entered='//TRIM(Alphas(Ptr)))
          ErrorsFound=.true.
      END SELECT

      Ptr=Ptr+4
      FieldNo=FieldNo+4
      NumField=NumField+1
    ENDDO
  ENDDO

  DO Loop=1,TotIntConvCoeff
    IF (UserIntConvectionCoeffs(Loop)%OverrideType /= ConvCoefSchedule) CYCLE
    IF (UserIntConvectionCoeffs(Loop)%ScheduleIndex == 0) CYCLE
    IF (CheckScheduleValueMinMax(UserIntConvectionCoeffs(Loop)%ScheduleIndex,'>=',LowHConvLimit,'<=',HighHConvLimit)) CYCLE
    CALL ShowSevereError(RoutineName//'Surface="'//  &
           TRIM(UserIntConvectionCoeffs(Loop)%SurfaceName)//'", out-of-range convection coefficient:')
    CALL ShowContinueError('Out-of-range value found in schedule='//TRIM(UserIntConvectionCoeffs(Loop)%ScheduleName))
    CALL ShowContinueError('User supplied convection coefficients must be in range [>=' // &
                           trim(RoundSigDigits(LowHConvLimit,9))//', <='// &
                           trim(RoundSigDigits(HighHConvLimit,1))// ']')
    CALL ShowContinueError('Limits are set (or default) in HeatBalanceAlgorithm object.')
    ErrorsFound=.true.
  ENDDO

  DO Loop=1,TotExtConvCoeff
    IF (UserExtConvectionCoeffs(Loop)%OverrideType /= ConvCoefSchedule) CYCLE
    IF (UserExtConvectionCoeffs(Loop)%ScheduleIndex == 0) CYCLE
    IF (CheckScheduleValueMinMax(UserExtConvectionCoeffs(Loop)%ScheduleIndex,'>=',LowHConvLimit,'<=',HighHConvLimit)) CYCLE
    CALL ShowSevereError(RoutineName//'Surface="'//  &
           TRIM(UserExtConvectionCoeffs(Loop)%SurfaceName)//'", out-of-range convection coefficient:')
    CALL ShowContinueError('Out-of-range value found in schedule='//TRIM(UserExtConvectionCoeffs(Loop)%ScheduleName))
    CALL ShowContinueError('User supplied convection coefficients must be in range [>=' // &
                           trim(RoundSigDigits(LowHConvLimit,9))//', <='// &
                           trim(RoundSigDigits(HighHConvLimit,1))// ']')
    CALL ShowContinueError('Limits are set (or default) in HeatBalanceAlgorithm object.')
    ErrorsFound=.true.
  ENDDO

  IF (DefaultOutsideConvectionAlgo == ASHRAESimple .or. ANY(Zone%OutsideConvectionAlgo == ASHRAESimple)) THEN
    Count=0
    DO Loop=1,TotExtConvCoeff
      SurfNum=UserExtConvectionCoeffs(Loop)%WhichSurface
      ! Tests show that Zone will override the simple convection specification of global.
      IF (SurfNum <= 0) CYCLE  ! ignore this error condition
      IF (Surface(SurfNum)%Zone == 0) CYCLE  ! ignore this error condition
      IF (Zone(Surface(SurfNum)%Zone)%OutsideConvectionAlgo == ASHRAESimple .and.  &
          (UserExtConvectionCoeffs(Loop)%OverrideType == ConvCoefSpecifiedModel .and.   &
           UserExtConvectionCoeffs(Loop)%HcModelEq  /= ASHRAESimple) .or.  &
           UserExtConvectionCoeffs(Loop)%OverrideType /= ConvCoefSpecifiedModel) THEN
        Count=Count+1
        IF (DisplayExtraWarnings) THEN
          CALL ShowSevereError(RoutineName//'Surface="'//  &
             TRIM(UserExtConvectionCoeffs(Loop)%SurfaceName)//'", mixed algorithms.')
          CALL ShowContinueError('Zone Outside Convection Algorithm specifies "SimpleCombined". '//  &
             'SimpleCombined will be used for this surface.')
        ENDIF
      ENDIF
    ENDDO
    IF (Count > 0) THEN
      CALL ShowSevereMessage(RoutineName//trim(RoundSigDigits(Count))//  &
         ' surfaces had different outside convection algorithms specified when')
      CALL ShowContinueError('the Zone Outside Convection Algorithm specifies "SimpleCombined".'//  &
            'SimpleCombined will be used for these surfaces.')
      IF (.not. DisplayExtraWarnings) THEN
        CALL ShowContinueError('Use OutputDiagnostics,DisplayExtraWarnings; to see specific instances.')
        TotalSevereErrors=TotalSevereErrors+Count
      ENDIF
    ENDIF
  ENDIF


  !get SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections

  CurrentModuleObject='SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections'
  Count = GetNumObjectsFound(CurrentModuleObject)
  !IF (Count > 1) ! throw  error ... TODO or IP handles it
  IF (Count == 1) THEN
    CALL GetObjectItem(CurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    InsideFaceAdaptiveConvectionAlgo%Name = cAlphaArgs(1) !not used by E+, unique object
    InsideFaceAdaptiveConvectionAlgo%EnteredByUser = .TRUE.

    ! A2 , \field Simple Bouyancy Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(2)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%SimpleBouyVertWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyVertWallEqNum == HcInt_UserCurve) THEN
        ! A3 , \field Simple Bouyancy Vertical Wall User Curve Name
        InsideFaceAdaptiveConvectionAlgo%SimpleBouyVertWallUserCurveNum = FindItemInList(cAlphaArgs(3), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
 
        IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyVertWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//', invalid value')
          CALL ShowContinueError(' Invalid '//TRIM(cAlphaFieldNames(3))//' entered='//TRIM(cAlphaArgs(3)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    ENDIF

    ! A4 , \field Simple Bouyancy Stable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(4)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableHorizEqNum == HcInt_UserCurve) THEN
        ! A5 , \field Simple Bouyancy Stable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableHorizUserCurveNum = FindItemInList(cAlphaArgs(5), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      ErrorsFound=.true.
    ENDIF

    ! A6 , \field Simple Bouyancy Unstable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(6)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableHorizEqNum == HcInt_UserCurve) THEN
        ! A7 , \field Simple Bouyancy Unstable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableHorizUserCurveNum = FindItemInList(cAlphaArgs(7), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      ErrorsFound=.true.
    ENDIF

    ! A8 , \field Simple Bouyancy Stable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(8)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableTiltedEqNum == HcInt_UserCurve) THEN
        ! A9 , \field Simple Bouyancy Stable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableTiltedUserCurveNum = FindItemInList(cAlphaArgs(9), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      ErrorsFound=.true.
    ENDIF

     ! A10 , \field Simple Bouyancy Unstable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(10)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableTiltedEqNum == HcInt_UserCurve) THEN
        ! A11, \field Simple Bouyancy Unstable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableTiltedUserCurveNum = FindItemInList(cAlphaArgs(11), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      ErrorsFound=.true.
    ENDIF

     ! A12, \field Simple Bouyancy Windows Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(12)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%SimpleBouyWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyWindowsEqNum == HcInt_UserCurve) THEN
        ! A13, \field Simple Bouyancy Windows Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%SimpleBouyWindowsUserCurveNum = FindItemInList(cAlphaArgs(13), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%SimpleBouyWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(13))//'='//TRIM(cAlphaArgs(13)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(12))//'='//TRIM(cAlphaArgs(12)))
      ErrorsFound=.true.
    ENDIF

     ! A14, \field Floor Heat Ceiling Cool Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(14)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolVertWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolVertWallEqNum == HcInt_UserCurve) THEN
        !  A15, \field Floor Heat Ceiling Cool Vertical Wall Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolVertWallUserCurveNum = FindItemInList(cAlphaArgs(15), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolVertWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(15))//'='//TRIM(cAlphaArgs(15)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(14))//'='//TRIM(cAlphaArgs(14)))
      ErrorsFound=.true.
    ENDIF


     ! A16, \field Floor Heat Ceiling Cool Stable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(16)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableHorizEqNum == HcInt_UserCurve) THEN
        !  A17, \field Floor Heat Ceiling Cool Stable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableHorizUserCurveNum = FindItemInList(cAlphaArgs(17), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(17))//'='//TRIM(cAlphaArgs(17)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(16))//'='//TRIM(cAlphaArgs(16)))
      ErrorsFound=.true.
    ENDIF

     ! A18, \field Floor Heat Ceiling Cool Unstable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(18)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableHorizEqNum == HcInt_UserCurve) THEN
        ! A19, \field Floor Heat Ceiling Cool Unstable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableHorizUserCurveNum = FindItemInList(cAlphaArgs(19), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(19))//'='//TRIM(cAlphaArgs(19)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(18))//'='//TRIM(cAlphaArgs(18)))
      ErrorsFound=.true.
    ENDIF

     ! A20, \field Floor Heat Ceiling Cool Heated Floor Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(20)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolHeatedFloorEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolHeatedFloorEqNum == HcInt_UserCurve) THEN
        ! A21, \field Floor Heat Ceiling Cool Heated Floor Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolHeatedFloorUserCurveNum = FindItemInList(cAlphaArgs(21), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolHeatedFloorUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(21))//'='//TRIM(cAlphaArgs(21)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(20))//'='//TRIM(cAlphaArgs(20)))
      ErrorsFound=.true.
    ENDIF

     ! A22, \field Floor Heat Ceiling Cool Chilled Ceiling Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(22)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolChilledCeilingEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolChilledCeilingEqNum == HcInt_UserCurve) THEN
        ! A23, \field Floor Heat Ceiling Cool Chilled Ceiling Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolChilledCeilingUserCurveNum = FindItemInList(cAlphaArgs(23), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolChilledCeilingUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(23))//'='//TRIM(cAlphaArgs(23)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(22))//'='//TRIM(cAlphaArgs(22)))
      ErrorsFound=.true.
    ENDIF

     ! A24, \field Floor Heat Ceiling Cool Stable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(24)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableTiltedEqNum == HcInt_UserCurve) THEN
        !   A25, \field Floor Heat Ceiling Cool Stable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableTiltedUserCurveNum = FindItemInList(cAlphaArgs(25), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(25))//'='//TRIM(cAlphaArgs(25)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(24))//'='//TRIM(cAlphaArgs(24)))
      ErrorsFound=.true.
    ENDIF

     ! A26, \field Floor Heat Ceiling Cool Unstable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(26)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableTiltedEqNum == HcInt_UserCurve) THEN
        !   A27, \field Floor Heat Ceiling Cool Unstable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableTiltedUserCurveNum = FindItemInList(cAlphaArgs(27), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(27))//'='//TRIM(cAlphaArgs(27)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(26))//'='//TRIM(cAlphaArgs(26)))
      ErrorsFound=.true.
    ENDIF

     ! A28, \field Floor Heat Ceiling Cool Window Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(28)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolWindowsEqNum == HcInt_UserCurve) THEN
        !    A29, \field Floor Heat Ceiling Cool Window Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolWindowsUserCurveNum = FindItemInList(cAlphaArgs(29), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(29))//'='//TRIM(cAlphaArgs(29)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(28))//'='//TRIM(cAlphaArgs(28)))
      ErrorsFound=.true.
    ENDIF

     ! A30, \field Wall Panel Heating Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(30)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatVertWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatVertWallEqNum == HcInt_UserCurve) THEN
        !    A31, \field Wall Panel Heating Vertical Wall Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatVertWallUserCurveNum = FindItemInList(cAlphaArgs(31), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatVertWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(31))//'='//TRIM(cAlphaArgs(31)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(30))//'='//TRIM(cAlphaArgs(30)))
      ErrorsFound=.true.
    ENDIF

     !  A32, \field Wall Panel Heating Heated Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(32)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatHeatedWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatHeatedWallEqNum == HcInt_UserCurve) THEN
        !   A33, \field Wall Panel Heating Heated Wall Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatHeatedWallUserCurveNum = FindItemInList(cAlphaArgs(33), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatHeatedWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(33))//'='//TRIM(cAlphaArgs(33)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(32))//'='//TRIM(cAlphaArgs(32)))
      ErrorsFound=.true.
    ENDIF

     !  A34, \field Wall Panel Heating Stable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(34)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableHorizEqNum == HcInt_UserCurve) THEN
        !   A35, \field Wall Panel Heating Stable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableHorizUserCurveNum = FindItemInList(cAlphaArgs(35), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(35))//'='//TRIM(cAlphaArgs(35)))
          ErrorsFound=.true.

        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(34))//'='//TRIM(cAlphaArgs(34)))
      ErrorsFound=.true.
    ENDIF

     ! A36, \field Wall Panel Heating Unstable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(36)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableHorizEqNum == HcInt_UserCurve) THEN
        !  A37, \field Wall Panel Heating Unstable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableHorizUserCurveNum = FindItemInList(cAlphaArgs(37), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(37))//'='//TRIM(cAlphaArgs(37)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(36))//'='//TRIM(cAlphaArgs(36)))
      ErrorsFound=.true.
    ENDIF

     ! A38, \field Wall Panel Heating Stable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(38)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableTiltedEqNum == HcInt_UserCurve) THEN
        !  A39, \field Wall Panel Heating Stable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableTiltedUserCurveNum = FindItemInList(cAlphaArgs(39), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(39))//'='//TRIM(cAlphaArgs(39)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(38))//'='//TRIM(cAlphaArgs(38)))
      ErrorsFound=.true.
    ENDIF

     !   A40, \field Wall Panel Heating Unstable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(40)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableTiltedEqNum == HcInt_UserCurve) THEN
        !  A41, \field Wall Panel Heating Unstable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableTiltedUserCurveNum = FindItemInList(cAlphaArgs(41), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(41))//'='//TRIM(cAlphaArgs(41)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(40))//'='//TRIM(cAlphaArgs(40)))
      ErrorsFound=.true.
    ENDIF

     !  A42, \field Wall Panel Heating Window Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(42)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%WallPanelHeatWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatWindowsEqNum == HcInt_UserCurve) THEN
        !  A43, \field Wall Panel Heating Window Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%WallPanelHeatWindowsUserCurveNum = FindItemInList(cAlphaArgs(43), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%WallPanelHeatWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(43))//'='//TRIM(cAlphaArgs(43)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(42))//'='//TRIM(cAlphaArgs(42)))
      ErrorsFound=.true.
    ENDIF

     !  A44, \field Convective Zone Heater Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(44)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallEqNum == HcInt_UserCurve) THEN
        ! A45, \field Convective Zone Heater Vertical Wall Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallUserCurveNum = FindItemInList(cAlphaArgs(45), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(45))//'='//TRIM(cAlphaArgs(45)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(44))//'='//TRIM(cAlphaArgs(44)))
      ErrorsFound=.true.
    ENDIF

     !  A46, \field Convective Zone Heater Vertical Walls Near Heater Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(46)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallNearHeaterEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallNearHeaterEqNum == HcInt_UserCurve) THEN
        ! A47, \field Convective Zone Heater Vertical Walls Near Heater Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallNearHeaterUserCurveNum = FindItemInList(cAlphaArgs(47), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallNearHeaterUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(47))//'='//TRIM(cAlphaArgs(47)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(46))//'='//TRIM(cAlphaArgs(46)))
      ErrorsFound=.true.
    ENDIF

     !  A48, \field Convective Zone Heater Stable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(48)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableHorizEqNum == HcInt_UserCurve) THEN
        ! A49, \field Convective Zone Heater Stable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableHorizUserCurveNum = FindItemInList(cAlphaArgs(49), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(49))//'='//TRIM(cAlphaArgs(49)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(48))//'='//TRIM(cAlphaArgs(48)))
      ErrorsFound=.true.
    ENDIF

     !  A50, \field Convective Zone Heater Unstable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(50)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableHorizEqNum == HcInt_UserCurve) THEN
        !  A51, \field Convective Zone Heater Unstable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableHorizUserCurveNum = FindItemInList(cAlphaArgs(51), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(51))//'='//TRIM(cAlphaArgs(51)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(50))//'='//TRIM(cAlphaArgs(50)))
      ErrorsFound=.true.
    ENDIF

     !  A52, \field Convective Zone Heater Stable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(52)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableTiltedEqNum == HcInt_UserCurve) THEN
        !  A53, \field Convective Zone Heater Stable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableTiltedUserCurveNum = FindItemInList(cAlphaArgs(53), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(53))//'='//TRIM(cAlphaArgs(53)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(52))//'='//TRIM(cAlphaArgs(52)))
      ErrorsFound=.true.
    ENDIF

     !  A54, \field Convective Zone Heater Unstable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(54)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableTiltedEqNum == HcInt_UserCurve) THEN
        !  A55, \field Convective Zone Heater Unstable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableTiltedUserCurveNum = FindItemInList(cAlphaArgs(55), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(55))//'='//TRIM(cAlphaArgs(55)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(54))//'='//TRIM(cAlphaArgs(54)))
      ErrorsFound=.true.
    ENDIF

     !  A56, \field Convective Zone Heater Windows Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(56)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatWindowsEqNum == HcInt_UserCurve) THEN
        !   A57, \field Convective Zone Heater Windows Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatWindowsUserCurveNum = FindItemInList(cAlphaArgs(57), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(57))//'='//TRIM(cAlphaArgs(57)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(56))//'='//TRIM(cAlphaArgs(56)))
      ErrorsFound=.true.
    ENDIF

     !  A58, \field Central Air Diffuser Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(58)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%CentralAirWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%CentralAirWallEqNum == HcInt_UserCurve) THEN
        !   A59, \field Central Air Diffuser Wall Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%CentralAirWallUserCurveNum = FindItemInList(cAlphaArgs(59), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%CentralAirWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(59))//'='//TRIM(cAlphaArgs(59)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(58))//'='//TRIM(cAlphaArgs(58)))
      ErrorsFound=.true.
    ENDIF

     !   A60, \field Central Air Diffuser Ceiling Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(60)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%CentralAirCeilingEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%CentralAirCeilingEqNum == HcInt_UserCurve) THEN
        !   A61, \field Central Air Diffuser Ceiling Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%CentralAirCeilingUserCurveNum = FindItemInList(cAlphaArgs(61), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%CentralAirCeilingUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(61))//'='//TRIM(cAlphaArgs(61)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(60))//'='//TRIM(cAlphaArgs(60)))
      ErrorsFound=.true.
    ENDIF

     !  A62, \field Central Air Diffuser Floor Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(62)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%CentralAirFloorEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%CentralAirFloorEqNum == HcInt_UserCurve) THEN
        !  A63, \field Central Air Diffuser Floor Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%CentralAirFloorUserCurveNum = FindItemInList(cAlphaArgs(63), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%CentralAirFloorUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(63))//'='//TRIM(cAlphaArgs(63)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(62))//'='//TRIM(cAlphaArgs(62)))
      ErrorsFound=.true.
    ENDIF

     !  A64, \field Central Air Diffuser Window Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(64)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsEqNum == HcInt_UserCurve) THEN
        !   A65, \field Central Air Diffuser Window Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsUserCurveNum = FindItemInList(cAlphaArgs(65), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(65))//'='//TRIM(cAlphaArgs(65)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(64))//'='//TRIM(cAlphaArgs(64)))
      ErrorsFound=.true.
    ENDIF

     ! A66, \field Mechanical Zone Fan Circulation Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(66)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ZoneFanCircVertWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircVertWallEqNum == HcInt_UserCurve) THEN
        !   A67, \field Mechanical Zone Fan Circulation Vertical Wall Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ZoneFanCircVertWallUserCurveNum = FindItemInList(cAlphaArgs(67), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircVertWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(67))//'='//TRIM(cAlphaArgs(67)))
          ErrorsFound=.true.

        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(66))//'='//TRIM(cAlphaArgs(66)))
      ErrorsFound=.true.
    ENDIF

     ! A68, \field Mechanical Zone Fan Circulation Stable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(68)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableHorizEqNum == HcInt_UserCurve) THEN
        !   A69, \field Mechanical Zone Fan Circulation Stable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableHorizUserCurveNum = FindItemInList(cAlphaArgs(69), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(69))//'='//TRIM(cAlphaArgs(69)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(68))//'='//TRIM(cAlphaArgs(68)))
      ErrorsFound=.true.
    ENDIF

     ! A70, \field Mechanical Zone Fan Circulation Unstable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(70)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableHorizEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableHorizEqNum == HcInt_UserCurve) THEN
        !   A71, \field Mechanical Zone Fan Circulation Unstable Horizontal Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableHorizUserCurveNum = FindItemInList(cAlphaArgs(71), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(71))//'='//TRIM(cAlphaArgs(71)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(70))//'='//TRIM(cAlphaArgs(70)))
      ErrorsFound=.true.
    ENDIF

     ! A72, \field Mechanical Zone Fan Circulation Stable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(72)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableTiltedEqNum == HcInt_UserCurve) THEN
        !  A73, \field Mechanical Zone Fan Circulation Stable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableTiltedUserCurveNum = FindItemInList(cAlphaArgs(73), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(73))//'='//TRIM(cAlphaArgs(73)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(72))//'='//TRIM(cAlphaArgs(72)))
      ErrorsFound=.true.
    ENDIF

     ! A74, \field Mechanical Zone Fan Circulation Unstable Tilted Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(74)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableTiltedEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableTiltedEqNum == HcInt_UserCurve) THEN
        !  A75, \field Mechanical Zone Fan Circulation Unstable Tilted Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableTiltedUserCurveNum = FindItemInList(cAlphaArgs(75), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableTiltedUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(75))//'='//TRIM(cAlphaArgs(75)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(74))//'='//TRIM(cAlphaArgs(74)))
      ErrorsFound=.true.
    ENDIF

     ! A76, \field Mechanical Zone Fan Circulation Window Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(76)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%ZoneFanCircWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircWindowsEqNum == HcInt_UserCurve) THEN
        !  A77, \field Mechanical Zone Fan Circulation Window Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%ZoneFanCircWindowsUserCurveNum = FindItemInList(cAlphaArgs(77), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%ZoneFanCircWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(77))//'='//TRIM(cAlphaArgs(77)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(76))//'='//TRIM(cAlphaArgs(76)))
      ErrorsFound=.true.
    ENDIF

     ! A78, \field Mixed Regime Bouyancy Assisting Flow on Walls Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(78)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedBouyAssistingFlowWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedBouyAssistingFlowWallEqNum == HcInt_UserCurve) THEN
        !  A79, \field Mixed Regime Bouyancy Assisting Flow on Walls Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedBouyAssistingFlowWallUserCurveNum = FindItemInList(cAlphaArgs(79), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedBouyAssistingFlowWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(79))//'='//TRIM(cAlphaArgs(79)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(78))//'='//TRIM(cAlphaArgs(78)))
      ErrorsFound=.true.
    ENDIF

     ! A80, \field Mixed Regime Bouyancy Oppossing Flow on Walls Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(80)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedBouyOppossingFlowWallEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedBouyOppossingFlowWallEqNum == HcInt_UserCurve) THEN
        !  A81, \field Mixed Regime Bouyancy Oppossing Flow on Walls Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedBouyOppossingFlowWallUserCurveNum = FindItemInList(cAlphaArgs(81), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedBouyOppossingFlowWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(81))//'='//TRIM(cAlphaArgs(81)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(80))//'='//TRIM(cAlphaArgs(80)))
      ErrorsFound=.true.
    ENDIF

     ! A82, \field Mixed Regime Stable Floor Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(82)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedStableFloorEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedStableFloorEqNum == HcInt_UserCurve) THEN
        !  A83, \field Mixed Regime Stable Floor Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedStableFloorUserCurveNum = FindItemInList(cAlphaArgs(83), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedStableFloorUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(83))//'='//TRIM(cAlphaArgs(83)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(82))//'='//TRIM(cAlphaArgs(82)))
      ErrorsFound=.true.
    ENDIF

     ! A84, \field Mixed Regime Unstable Floor Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(84)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedUnstableFloorEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedUnstableFloorEqNum == HcInt_UserCurve) THEN
        !  A85, \field Mixed Regime Unstable Floor Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedUnstableFloorUserCurveNum = FindItemInList(cAlphaArgs(85), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedUnstableFloorUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(85))//'='//TRIM(cAlphaArgs(85)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(84))//'='//TRIM(cAlphaArgs(84)))
      ErrorsFound=.true.
    ENDIF

     ! A86, \field Mixed Regime Stable Ceiling Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(86)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedStableCeilingEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedStableCeilingEqNum == HcInt_UserCurve) THEN
        !  A87, \field Mixed Regime Stable Ceiling Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedStableCeilingUserCurveNum = FindItemInList(cAlphaArgs(87), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedStableCeilingUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(87))//'='//TRIM(cAlphaArgs(87)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(86))//'='//TRIM(cAlphaArgs(86)))
      ErrorsFound=.true.
    ENDIF

     ! A88, \field Mixed Regime Unstable Ceiling Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(88)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedUnstableCeilingEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedUnstableCeilingEqNum == HcInt_UserCurve) THEN
        !  A89, \field Mixed Regime Unstable Ceiling Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedUnstableCeilingUserCurveNum = FindItemInList(cAlphaArgs(89), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedUnstableCeilingUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(89))//'='//TRIM(cAlphaArgs(89)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(88))//'='//TRIM(cAlphaArgs(88)))
      ErrorsFound=.true.
    ENDIF

     ! A90, \field Mixed Regime Window Equation Source
    IsValidType=.false.
    DO Loop1=6, NumValidIntConvectionValueTypes !skipping first 5 whole-model types
      IF (Trim(cAlphaArgs(90)) /= Trim(ValidIntConvectionValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      InsideFaceAdaptiveConvectionAlgo%MixedWindowsEqNum = IntConvectionValue(Loop1)
      IF (InsideFaceAdaptiveConvectionAlgo%MixedWindowsEqNum == HcInt_UserCurve) THEN
        !   A91; \field Mixed Regime Window Equation User Curve Name
        InsideFaceAdaptiveConvectionAlgo%MixedWindowsUserCurveNum = FindItemInList(cAlphaArgs(91), &
                                                             HcInsideUserCurve%Name , TotInsideHcUserCurves )
        IF (InsideFaceAdaptiveConvectionAlgo%MixedWindowsUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(91))//'='//TRIM(cAlphaArgs(91)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT !found it
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(90))//'='//TRIM(cAlphaArgs(90)))
      ErrorsFound=.true.
    ENDIF

  ENDIF !end of 'SurfaceConvectionAlgorithm:Inside:AdaptiveModelSelections'

  CurrentModuleObject='SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections'
  Count = GetNumObjectsFound(CurrentModuleObject)
  !IF (Count > 1) ! throw  error ... TODO or IP handles it
  IF (Count == 1) THEN
    CALL GetObjectItem(CurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,Status,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    OutsideFaceAdaptiveConvectionAlgo%Name = cAlphaArgs(1) !not used by E+, unique object
    OutsideFaceAdaptiveConvectionAlgo%EnteredByUser = .TRUE.



    ! A2 , \field Wind Convection Windward Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=2, NumValidSpecificExtWindConvValueTypes
      IF (Trim(cAlphaArgs(2)) /= Trim(ValidSpecificExtWindConvValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      OutsideFaceAdaptiveConvectionAlgo%HWindWallWindwardEqNum  = MoreSpecificExtWindConvectionValue(Loop1)
      IF (OutsideFaceAdaptiveConvectionAlgo%HWindWallWindwardEqNum == HcExt_UserCurve) THEN
        !  A3 , \field Wind Convection Windward Equation Vertical Wall User Curve Name
        OutsideFaceAdaptiveConvectionAlgo%HWindWallWindwardUserCurveNum  = FindItemInList(cAlphaArgs(3), &
                                                             HcOutsideUserCurve%Name , TotOutsideHcUserCurves )
        IF (OutsideFaceAdaptiveConvectionAlgo%HWindWallWindwardUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    ENDIF

    ! A4 , \field Wind Convection Leeward Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=2, NumValidSpecificExtWindConvValueTypes
      IF (Trim(cAlphaArgs(4)) /= Trim(ValidSpecificExtWindConvValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      OutsideFaceAdaptiveConvectionAlgo%HWindWallLeewardEqNum  = MoreSpecificExtWindConvectionValue(Loop1)
      IF (OutsideFaceAdaptiveConvectionAlgo%HWindWallLeewardEqNum == HcExt_UserCurve) THEN
        ! A5 , \field Wind Convection Leeward Vertical Wall Equation User Curve Name
        OutsideFaceAdaptiveConvectionAlgo%HWindWallLeewardUserCurveNum  = FindItemInList(cAlphaArgs(5), &
                                                             HcOutsideUserCurve%Name , TotOutsideHcUserCurves )
        IF (OutsideFaceAdaptiveConvectionAlgo%HWindWallLeewardUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      ErrorsFound=.true.
    ENDIF

    ! A6 , \field Wind Convection Horizontal Roof Equation Source
    IsValidType=.false.
    DO Loop1=1, NumValidSpecificExtWindConvValueTypes
      IF (Trim(cAlphaArgs(6)) /= Trim(ValidSpecificExtWindConvValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofEqNum  = MoreSpecificExtWindConvectionValue(Loop1)
      IF (OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofEqNum == HcExt_UserCurve) THEN
        !  A7 , \field Wind Convection Horizontal Roof User Curve Name
        OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofUserCurveNum  = FindItemInList(cAlphaArgs(7), &
                                                             HcOutsideUserCurve%Name , TotOutsideHcUserCurves )
        IF (OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      ErrorsFound=.true.
    ENDIF

    !  A8 , \field Natural Convection Vertical Wall Equation Source
    IsValidType=.false.
    DO Loop1=1, NumValidSpecificExtNatConvectValueTypes
      IF (Trim(cAlphaArgs(8)) /= Trim(ValidSpecificExtNatConvectValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      OutsideFaceAdaptiveConvectionAlgo%HNatVertWallEqNum  = SpecificExtNatConvectionValue(Loop1)
      IF (OutsideFaceAdaptiveConvectionAlgo%HNatVertWallEqNum == HcExt_UserCurve) THEN
        !  A9 , \field Natural Convection Vertical Wall Equation User Curve Name
        OutsideFaceAdaptiveConvectionAlgo%HNatVertWallUserCurveNum  = FindItemInList(cAlphaArgs(9), &
                                                             HcOutsideUserCurve%Name , TotOutsideHcUserCurves )
        IF (OutsideFaceAdaptiveConvectionAlgo%HNatVertWallUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      ErrorsFound=.true.
    ENDIF

    !  A10, \field Natural Convection Stable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=1, NumValidSpecificExtNatConvectValueTypes
      IF (Trim(cAlphaArgs(10)) /= Trim(ValidSpecificExtNatConvectValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizEqNum  = SpecificExtNatConvectionValue(Loop1)
      IF (OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizEqNum == HcExt_UserCurve) THEN
        !  A11, \field Natural Convection Stable Horizontal Equation User Curve Name
        OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizUserCurveNum  = FindItemInList(cAlphaArgs(11), &
                                                             HcOutsideUserCurve%Name , TotOutsideHcUserCurves )
        IF (OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      ErrorsFound=.true.
    ENDIF

    !   A12, \field Natural Convection Unstable Horizontal Equation Source
    IsValidType=.false.
    DO Loop1=1, NumValidSpecificExtNatConvectValueTypes
      IF (Trim(cAlphaArgs(12)) /= Trim(ValidSpecificExtNatConvectValueTypes(Loop1))) CYCLE
      IsValidType=.true.
      OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizEqNum  = SpecificExtNatConvectionValue(Loop1)
      IF (OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizEqNum == HcExt_UserCurve) THEN
        ! A13; \field Natural Convection Unstable Horizontal Equation User Curve Name
        OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizUserCurveNum  = FindItemInList(cAlphaArgs(13), &
                                                             HcOutsideUserCurve%Name , TotOutsideHcUserCurves )
        IF (OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizUserCurveNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
          CALL ShowContinueError('Invalid Name choice Entered, for '//TRIM(cAlphaFieldNames(13))//'='//TRIM(cAlphaArgs(13)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      EXIT
    ENDDO
    IF (.not. IsValidType) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//', invalid value')
      CALL ShowContinueError('Invalid Key choice Entered, for '//TRIM(cAlphaFieldNames(12))//'='//TRIM(cAlphaArgs(12)))
      ErrorsFound=.true.
    ENDIF

  ENDIF ! end of 'SurfaceConvectionAlgorithm:Outside:AdaptiveModelSelections'


  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found getting input.  Program termination.')
  ENDIF

  CALL SetupAdaptiveConvectionStaticMetaData


  RETURN

CONTAINS
! Internal subroutines to GetUserConvectionCoefficients
SUBROUTINE ApplyConvectionValue(SurfaceTypes,ConvectionType,Value)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine applies a convection type to a set of surfaces.  This is
          ! one of the "regular" convection types and becomes a "negative" convection
          ! type to that surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SurfaceTypes
  CHARACTER(len=*), INTENT(IN) :: ConvectionType
  INTEGER, INTENT(IN) :: Value

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER SurfNum
  LOGICAL SurfacesOfType
  INTEGER SurfaceCountOutside
  INTEGER SurfaceCountInside
  CHARACTER(len=52) :: OverwriteMessage

  SELECT CASE(SurfaceTypes)

    CASE ('ALLEXTERIORSURFACES')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE ('ALLEXTERIORWINDOWS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
        IF (.not. Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE ('ALLEXTERIORWALLS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
        IF (.not. Surface(SurfNum)%Class == SurfaceClass_Wall) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE('ALLEXTERIORROOFS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
        IF (.not. Surface(SurfNum)%Class == SurfaceClass_Roof) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE('ALLEXTERIORFLOORS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
        IF (.not. Surface(SurfNum)%Class == SurfaceClass_Floor) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE ('ALLINTERIORSURFACES')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE ('ALLINTERIORWINDOWS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
        IF (.not. Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE ('ALLINTERIORWALLS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
        IF (.not. Surface(SurfNum)%Class == SurfaceClass_Wall) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE('ALLINTERIORROOFS','ALLINTERIORCEILINGS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
        IF (.not. Surface(SurfNum)%Class == SurfaceClass_Roof) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE('ALLINTERIORFLOORS')
      SurfacesOfType=.false.
      SurfaceCountOutside=0
      SurfaceCountInside=0
      DO SurfNum=1,TotSurfaces
        IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
        IF (.not. Surface(SurfNum)%Class == SurfaceClass_Floor) CYCLE
        SurfacesOfType=.true.
        IF (ConvectionType == 'OUTSIDE') THEN
          IF (Surface(SurfNum)%OSCPtr > 0) CYCLE
          IF (Surface(SurfNum)%ExtConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Outside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountOutside=SurfaceCountOutside+1
            ENDIF
          ELSE
            Surface(SurfNum)%ExtConvCoeff=Value
          ENDIF
        ELSE
          IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN
            IF (DisplayExtraWarnings) THEN
              CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
                 '", not overwriting already assigned value for (Inside) in Surface='//trim(Surface(SurfNum)%Name))
            ELSE
              SurfaceCountInside=SurfaceCountInside+1
            ENDIF
          ELSE
            Surface(SurfNum)%IntConvCoeff=Value
          ENDIF
        ENDIF
      ENDDO
      IF (.not. DisplayExtraWarnings .and. (SurfaceCountOutside > 0 .or. SurfaceCountInside > 0) ) THEN
        IF (SurfaceCountOutside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountOutside))//' Outside'
        ENDIF
        IF (SurfaceCountInside > 0) THEN
          OverwriteMessage=trim(TrimSigDigits(SurfaceCountInside))//' Inside'
        ENDIF
        CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
            '", not overwriting already assigned values for '//trim(OverwriteMessage)//' assignments.')
      ENDIF

    CASE DEFAULT
      SurfacesOfType=.false.

  END SELECT

  IF (.not. SurfacesOfType) THEN
    CALL ShowWarningError('User Supplied Convection Coefficients, Multiple Surface Assignments="'//trim(SurfaceTypes)//  &
       '", there were no surfaces of that type found for '//trim(ConvectionType)//' assignment.')
  ENDIF

  RETURN

END SUBROUTINE ApplyConvectionValue

END SUBROUTINE GetUserConvectionCoefficients

REAL(r64) FUNCTION CalcASHRAESimpExtConvectCoeff(Roughness, SurfWindSpeed)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the exterior convection coefficient
          ! using the ASHRAE Simple Method from a correlation from Figure 1
          ! on p. 22.4 of the 1989 ASHRAE Handbook of Fundamentals.
          ! This is a combined coefficient that includes radiation to sky, ground, and air.

          ! METHODOLOGY EMPLOYED:
          ! Apply the correlation based on the input data.

          ! REFERENCES:
          ! ASHRAE Handbook of Fundamentals 1989, p.22.4

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Roughness  ! Integer index for roughness, relates to parameter array indices
  REAL(r64),    INTENT(IN) :: SurfWindSpeed  ! Current wind speed, m/s

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER, DIMENSION(6) :: D = (/ 11.58d0, 12.49d0, 10.79d0, 8.23d0, 10.22d0, 8.23d0 /)
  REAL(r64), PARAMETER, DIMENSION(6) :: E = (/ 5.894d0, 4.065d0, 4.192d0, 4.00d0, 3.100d0, 3.33d0 /)
  REAL(r64), PARAMETER, DIMENSION(6) :: F = (/ 0.0d0, 0.028d0, 0.0d0, -0.057d0, 0.0d0, -0.036d0 /)


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  CalcASHRAESimpExtConvectCoeff =  D(Roughness) + E(Roughness)*SurfWindSpeed &
                                                + F(Roughness)*SurfWindSpeed**2

  RETURN

END FUNCTION CalcASHRAESimpExtConvectCoeff


SUBROUTINE CalcASHRAESimpleIntConvCoeff(SurfNum,SurfaceTemperature,ZoneMeanAirTemperature)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the interior convection coefficient for a surface.

          ! METHODOLOGY EMPLOYED:
          ! The convection coefficients are taken directly from the TARP Reference Manual.  TARP calculated
          ! its coefficients using the surface conductances for e=0.9 found in ASHRAE Handbook of Fundamentals
          ! 1985 in Table 1 on p. 23.2, but subtracted off the radiative component which was estimated at
          ! 1.02 * 0.9 = 0.918 BTU/h-ft2-F.  Coefficients were then converted to SI units to yield the values
          ! in this subroutine.

          ! REFERENCES:
          ! 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
          !     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79.
          ! 2.  ASHRAE Handbook of Fundamentals 1985, p. 23.2, Table 1.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: SurfNum ! surface number for which coefficients are being calculated
  REAL(r64), INTENT(IN)   :: SurfaceTemperature ! Temperature of surface for evaluation of HcIn
  REAL(r64), INTENT(IN)   :: ZoneMeanAirTemperature  ! Mean Air Temperature of Zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: DeltaTemp          ! Temperature difference between the zone air and the surface


  IF (ABS(Surface(SurfNum)%CosTilt) >= 0.3827d0) THEN   ! Recalculate HConvIn

    DeltaTemp = ZoneMeanAirTemperature - SurfaceTemperature

    ! Set HConvIn using the proper correlation based on DeltaTemp and Cosine of the Tilt of the Surface
    IF (ABS(Surface(SurfNum)%CosTilt) >= 0.9239d0) THEN  ! Horizontal Surface

      IF (DeltaTemp*Surface(SurfNum)%CosTilt < 0.0d0) THEN ! Horizontal, Reduced Convection

        HConvIn(SurfNum) = 0.948d0

      ELSEIF (DeltaTemp*Surface(SurfNum)%CosTilt == 0.0d0) THEN ! Vertical Surface

        HConvIn(SurfNum) = 3.076d0

      ELSEIF (DeltaTemp*Surface(SurfNum)%CosTilt > 0.0d0) THEN ! Horizontal, Enhanced Convection

        HConvIn(SurfNum) = 4.040d0

      END IF

    ELSE  ! Tilted Surface

      IF (DeltaTemp*Surface(SurfNum)%CosTilt < 0.0d0) THEN ! Tilted, Reduced Convection

        HConvIn(SurfNum) = 2.281d0

      ELSEIF (DeltaTemp*Surface(SurfNum)%CosTilt == 0.0d0) THEN ! Vertical Surface

        HConvIn(SurfNum) = 3.076d0

      ELSEIF (DeltaTemp*Surface(SurfNum)%CosTilt > 0.0d0) THEN ! Tilted, Enhanced Convection

        HConvIn(SurfNum) = 3.870d0

      END IF

    END IF    ! ...end of correlation selection IF-THEN block

  END IF  ! ...end of HConvIn recalculation IF-THEN block

  ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
  IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

  RETURN

END SUBROUTINE CalcASHRAESimpleIntConvCoeff


SUBROUTINE CalcASHRAEDetailedIntConvCoeff(SurfNum,SurfaceTemperature,ZoneMeanAirTemperature)

            !SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the interior convection coefficient for a surface.

          ! METHODOLOGY EMPLOYED:
          ! The algorithm for convection coefficients is taken directly from the TARP Reference Manual.
          ! ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5 gives equations for natural convection
          ! heat transfer coefficients in the turbulent range for large, vertical plates and for large,
          ! horizontal plates facing upward when heated (or downward when cooled).  A note in the text
          ! also gives an approximation for large, horizontal places facing downward when heated (or
          ! upward when cooled) recommending that it should be half of the facing upward value.
          ! TARP then adds a curve fit as a function of the cosine of the tilt angle to provide intermediate
          ! values between vertical and horizontal.  The curve fit values at the extremes match the ASHRAE
          ! values very well.

          ! REFERENCES:
          ! 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
          !     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.
          ! 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: SurfNum ! surface number for which coefficients are being calculated
  REAL(r64), INTENT(IN)   :: SurfaceTemperature ! Temperature of surface for evaluation of HcIn
  REAL(r64), INTENT(IN)   :: ZoneMeanAirTemperature  ! Mean Air Temperature of Zone

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: OneThird = (1.d0/3.d0)  ! 1/3 in highest precision

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: DeltaTemp          ! Temperature difference between the zone air and the surface

          ! FLOW:

  DeltaTemp = SurfaceTemperature - ZoneMeanAirTemperature

    ! Set HConvIn using the proper correlation based on DeltaTemp and Surface (Cosine Tilt)

  IF ((DeltaTemp == 0.0d0) .OR. (Surface(SurfNum)%CosTilt == 0.0d0)) THEN   ! Vertical Surface

    HConvIn(SurfNum) = CalcASHRAEVerticalWall(DeltaTemp)

  ELSEIF ( ((DeltaTemp < 0.0d0) .AND. (Surface(SurfNum)%CosTilt > 0.0d0)) .OR. &
           ((DeltaTemp > 0.0d0) .AND. (Surface(SurfNum)%CosTilt < 0.0d0)) ) THEN  ! Enhanced Convection

    HConvIn(SurfNum) = CalcWaltonUnstableHorizontalOrTilt(DeltaTemp, Surface(SurfNum)%CosTilt)

  ELSEIF ( ((DeltaTemp > 0.0d0) .AND. (Surface(SurfNum)%CosTilt > 0.0d0)) .OR. &
           ((DeltaTemp < 0.0d0) .AND. (Surface(SurfNum)%CosTilt < 0.0d0)) ) THEN  ! Reduced Convection

    HConvIn(SurfNum) = CalcWaltonStableHorizontalOrTilt(DeltaTemp, Surface(SurfNum)%CosTilt)

  END IF  ! ...end of IF-THEN block to set HConvIn

  ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
  IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

  RETURN

END SUBROUTINE CalcASHRAEDetailedIntConvCoeff


SUBROUTINE CalcDetailedHcInForDVModel(SurfNum,SurfaceTemperatures,HcIn,Vhc)

            !SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2000
          !       MODIFIED       Used for DV model; Feb 2004, LKL
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the interior convection coefficient for a surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: MAT
  USE DataRoomAirModel, ONLY: AirModel, RoomAirModel_UCSDDV, RoomAirModel_UCSDCV, RoomAirModel_UCSDUFI, RoomAirModel_UCSDUFE

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                        :: SurfNum ! surface number for which coefficients are being calculated
  REAL(r64), DIMENSION(:), INTENT(IN) :: SurfaceTemperatures ! Temperature of surfaces for evaluation of HcIn
  REAL(r64), DIMENSION(:), INTENT(INOUT)          :: HcIn ! Interior Convection Coeff Array
  REAL(r64), DIMENSION(:), INTENT(IN), OPTIONAL   :: Vhc !Velocity array for forced convection coeff calculation


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: OneThird = (1.d0/3.d0)  ! 1/3 in highest precision

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: DeltaTemp          ! Temperature difference between the zone air and the surface
  REAL(r64)    :: TAirConv
  REAL(r64)    :: Hf

          ! FLOW:

  IF (Surface(SurfNum)%HeatTransSurf) THEN  ! Only treat heat transfer surfaces

! UCSD
    SELECT CASE (Surface(SurfNum)%TAirRef)
      CASE (AdjacentAirTemp)
        TAirConv = TempEffBulkAir(SurfNum)
      CASE DEFAULT
        ! currently set to mean air temp but should add error warning here
        TAirConv = MAT(Surface(SurfNum)%Zone)
    END SELECT
    DeltaTemp = SurfaceTemperatures(SurfNum) - TAirConv


    IF (AirModel(Surface(SurfNum)%Zone)%AirModelType == RoomAirModel_UCSDDV .or. &
        AirModel(Surface(SurfNum)%Zone)%AirModelType == RoomAirModel_UCSDUFI .or. &
        AirModel(Surface(SurfNum)%Zone)%AirModelType == RoomAirModel_UCSDUFE) THEN

      ! Set HConvIn using the proper correlation based on DeltaTemp and CosTiltSurf
      IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN

        HcIn(SurfNum)=SetIntConvectionCoeff(SurfNum)

      ELSEIF ((DeltaTemp == 0.0d0) .OR. (Surface(surfnum)%CosTilt == 0.0d0)) THEN   ! Vertical Surface

        HcIn(SurfNum) = 1.31d0*((ABS(DeltaTemp))**OneThird)

      ELSEIF ( ((DeltaTemp < 0.0d0) .AND. (Surface(SurfNum)%CosTilt > 0.0d0)) .OR. &
             ((DeltaTemp > 0.0d0) .AND. (Surface(SurfNum)%CosTilt < 0.0d0)) ) THEN  ! Enhanced Convection

        HcIn(SurfNum) = 9.482d0*((ABS(DeltaTemp))**OneThird) &
                                /(7.283d0 - ABS(Surface(SurfNum)%CosTilt))

      ELSEIF ( ((DeltaTemp > 0.0d0) .AND. (Surface(SurfNum)%CosTilt > 0.0d0)) .OR. &
             ((DeltaTemp < 0.0d0) .AND. (Surface(SurfNum)%CosTilt < 0.0d0)) ) THEN  ! Reduced Convection

        HcIn(SurfNum) = 1.810d0*((ABS(DeltaTemp))**OneThird) &
                                /(1.382d0 + ABS(Surface(SurfNum)%CosTilt))

      END IF  ! ...end of IF-THEN block to set HConvIn

    ELSEIF (AirModel(Surface(SurfNum)%Zone)%AirModelType == RoomAirModel_UCSDCV) THEN

      Hf=4.3d0*Vhc(Surface(SurfNum)%Zone)

      ! Set HConvIn using the proper correlation based on DeltaTemp and CosTiltSurf
      IF (Surface(SurfNum)%IntConvCoeff /= 0) THEN

        HcIn(SurfNum)=SetIntConvectionCoeff(SurfNum)


      ELSEIF ((DeltaTemp == 0.0d0) .OR. (Surface(SurfNum)%CosTilt == 0.0d0)) THEN   ! Vertical Surface

        HcIn(SurfNum) = 1.31d0*((ABS(DeltaTemp))**OneThird)

        HcIn(SurfNum)= (HcIn(SurfNum)**3.2d0+Hf**3.2d0)**(1.d0/3.2d0)


      ELSEIF ( ((DeltaTemp < 0.0d0) .AND. (Surface(SurfNum)%CosTilt > 0.0d0)) .OR. &
             ((DeltaTemp > 0.0d0) .AND. (Surface(SurfNum)%CosTilt < 0.0d0)) ) THEN  ! Enhanced Convection

        HcIn(SurfNum) = 9.482d0*((ABS(DeltaTemp))**(1.d0/3.d0)) &
                                /(7.283 - ABS(Surface(SurfNum)%CosTilt))
        HcIn(SurfNum)= (HcIn(SurfNum)**3.2d0+Hf**3.2d0)**(1.d0/3.2d0)


      ELSEIF ( ((DeltaTemp > 0.0d0) .AND. (Surface(SurfNum)%CosTilt > 0.0d0)) .OR. &
             ((DeltaTemp < 0.0d0) .AND. (Surface(SurfNum)%CosTilt < 0.0d0)) ) THEN  ! Reduced Convection

        HcIn(SurfNum) = 1.810d0*((ABS(DeltaTemp))**OneThird) &
                                /(1.382d0 + ABS(Surface(SurfNum)%CosTilt))
        HcIn(SurfNum)= (HcIn(SurfNum)**3.2d0+Hf**3.2d0)**(1.d0/3.2d0)


      END IF  ! ...end of IF-THEN block to set HConvIn


    END IF

  END IF

  ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    IF (HcIn(SurfNum) < LowHConvLimit) HcIn(SurfNum) = LowHConvLimit


  RETURN


END SUBROUTINE CalcDetailedHcInForDVModel

SUBROUTINE CalcCeilingDiffuserIntConvCoeff(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the interior convection coefficients
          ! for ceiling diffusers correlated to the outlet air temperature.

          ! METHODOLOGY EMPLOYED:
          ! call functions with the actual model equations

          ! REFERENCES:
          ! Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
          !       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137

          ! OTHER NOTES:
          ! The correlations shown below differ from (and are less accurate than) those shown
          ! in the reference above (Fisher 1997).  They have been reformulated with an outlet
          ! temperature reference in order to accomodate the structure of the EnergyPlus code.

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: OutBaroPress
  USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW, PsyWFnTdpPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum ! zone number for which coefficients are being calculated

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum            ! DO loop counter for surfaces
  REAL(r64)    :: ACH                ! Air changes per hour
  INTEGER :: ZoneNode           ! Zone node as defined in system simulation
  REAL(r64)    :: ZoneVolume         ! Zone node as defined in system simulation
  REAL(r64)    :: ZoneMassFlowRate   ! Zone node as defined in system simulation
  REAL(r64)    :: AirDensity         ! zone air density
  REAL(r64)    :: ZoneMult

          ! FLOW:
  IF (SysSizingCalc .OR. ZoneSizingCalc .OR. .NOT. ALLOCATED(Node)) THEN
    ACH = 0.0d0
  ELSE
    ! Set local variables
    ZoneVolume = Zone(ZoneNum)%Volume
    ZoneNode  = Zone(ZoneNum)%SystemZoneNodeNumber
    ZoneMult  = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
    IF (.not. BeginEnvrnFlag) THEN
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      ZoneMassFlowRate = Node(ZoneNode)%MassFlowRate / ZoneMult
    ELSE  ! because these are not updated yet for new environment
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,0.0d0,PsyWFnTdpPb(0.0d0,OutBaroPress))
      ZoneMassFlowRate = 0.0d0
    ENDIF

    IF (ZoneMassFlowRate < MinFlow) THEN
      ACH = 0.0d0
    ELSE
      ! Calculate ACH
      ACH = ZoneMassFlowRate/AirDensity/ZoneVolume*SecInHour
      ! Limit ACH to range of correlation
      ACH = MIN(ACH, MaxACH)
      ACH = MAX(ACH, 0.0d0)
    END IF
  END IF

  ! If the Ceiling Diffuser option is selected the following correlations are used.
  ! The development of the ceiling diffuser convection correlations is shown in reference 4.
  ! The correlations shown below differ from (and are less accurate than) those shown in reference 4 because they have been
  ! reformulated with an outlet temperature reference in order to accomodate the structure of the
  ! EnergyPlus code.
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE  ! Skip non-heat transfer surfaces

    ! Set HConvIn using the proper correlation based on Surface Tilt
    IF (Surface(SurfNum)%Tilt > 135.0d0) THEN
      HConvIn(SurfNum) = CalcFisherPedersenCeilDiffuserFloor(ACH) ! Floor correlation
    ELSEIF (Surface(SurfNum)%Tilt < 45.0d0 ) THEN
      HConvIn(SurfNum) =CalcFisherPedersenCeilDiffuserCeiling(ACH) ! Ceiling correlation
    ELSE
      HConvIn(SurfNum) = CalcFisherPedersenCeilDiffuserWalls(ACH) ! Wall correlation
    END IF
    ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

  END DO ! SurfNum

  RETURN

END SUBROUTINE CalcCeilingDiffuserIntConvCoeff


! CalcCeilingDiffuserInletCorr should replace CalcCeilingDiffuser (above), if ZoneTempPredictorCorrector can
! ever be made to work correctly with the inlet air temperature.

SUBROUTINE CalcCeilingDiffuserInletCorr(ZoneNum, SurfaceTemperatures)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   August 2000
          !       RE-ENGINEERED  July 2003 (Peter Graham Ellis)
          !       MODIFIED       July 2003, (CC) set a flag for reference temperature so that supply air temperature
          !                                      is used as the reference in the inside heat balance calculations

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the interior convection coefficients
          ! for ceiling diffusers correlated to the inlet air temperature.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
          !   Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.137

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: OutBaroPress
  USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW, PsyWFnTdpPb
  USE DataHeatBalFanSys, ONLY: MAT

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: ZoneNum             ! Zone number
  REAL(r64), DIMENSION(:), INTENT(IN) :: SurfaceTemperatures ! For CalcASHRAEDetailed, if called

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: ACH              ! Air changes per hour
  INTEGER :: ZoneNode         ! Zone node as defined in system simulation
  REAL(r64)    :: ZoneVolume       ! Zone node as defined in system simulation
  REAL(r64)    :: ZoneMassFlowRate ! Zone node as defined in system simulation
  REAL(r64)    :: AirDensity       ! zone air density
  INTEGER :: SurfNum          ! DO loop counter for surfaces
  REAL(r64)    :: Tilt             ! Surface tilt
  REAL(r64)    :: ZoneMult

          ! FLOW:
  IF (SysSizingCalc .OR. ZoneSizingCalc .OR. .NOT. ALLOCATED(Node)) THEN
    ACH = 0.0d0
  ELSE
    ! Set local variables
    ZoneVolume = Zone(ZoneNum)%Volume
    ZoneNode  = Zone(ZoneNum)%SystemZoneNodeNumber
    ZoneMult  = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
    AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress, Node(ZoneNode)%Temp, PsyWFnTdpPb(Node(ZoneNode)%Temp, OutBaroPress))
    ZoneMassFlowRate = Node(ZoneNode)%MassFlowRate/ZoneMult

    IF (ZoneMassFlowRate < MinFlow) THEN
      ACH = 0.0d0
    ELSE
      ! Calculate ACH
      ACH = ZoneMassFlowRate / AirDensity / ZoneVolume * SecInHour
      ! Limit ACH to range of correlation
      ACH = MIN(ACH, MaxACH)
      ACH = MAX(ACH, 0.0d0)
    END IF
  END IF

    DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
      IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

      IF (ACH <= 3.0d0) THEN ! Use the other convection algorithm
        IF (.NOT. Construct(Surface(SurfNum)%Construction)%TypeIsWindow) THEN
          CALL CalcASHRAEDetailedIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
        ELSE
          CALL CalcISO15099WindowIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))
        ENDIF
      ELSE ! Use forced convection correlations
        Tilt = Surface(SurfNum)%Tilt

        ! assume that reference air temp for user defined convection coefficient is the mean air temperature (=MAT)
        ! Calculate the convection coefficient based on inlet (supply) air conditions
        IF (Tilt < 45.0d0) THEN
          HConvIn(SurfNum) = 0.49d0 * ACH**0.8d0 ! Ceiling correlation
        ELSE IF (Tilt > 135.0d0) THEN
          HConvIn(SurfNum) = 0.13d0 * ACH**0.8d0 ! Floor correlation
        ELSE
          HConvIn(SurfNum) = 0.19d0 * ACH**0.8d0 ! Wall correlation
        END IF
        ! set flag for reference air temperature
        Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
      END IF

      ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
      IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

    END DO ! SurfNum

    IF (ACH > 100.0d0) CALL ShowWarningError('CeilingDiffuser convection correlation is out of range: ACH > 100')

  RETURN

END SUBROUTINE CalcCeilingDiffuserInletCorr


SUBROUTINE CalcTrombeWallIntConvCoeff(ZoneNum,SurfaceTemperatures)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   ?????
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine calculates the interior convection coefficient
          ! using the Trombe Wall correlation ?????

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: MAT

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

  INTEGER, INTENT(IN)            :: ZoneNum             ! Zone number for which coefficients are being calculated
  REAL(r64), DIMENSION(:), INTENT(IN) :: SurfaceTemperatures ! Temperature of surfaces for evaluation of HcIn

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: g = 9.81d0       ! gravity constant (m/s**2)
  REAL(r64), PARAMETER :: v = 15.89d-6   ! kinematic viscosity (m**2/s) for air at 300 K
  REAL(r64), PARAMETER :: k = 0.0263d0     ! thermal conductivity (W/m K) for air at 300 K
  REAL(r64), PARAMETER :: Pr = 0.71d0      ! Prandtl number for air at ?

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum            ! DO loop counter for surfaces
  INTEGER :: Surf1              ! first major wall surface
  INTEGER :: Surf2              ! second major wall surface

  REAL(r64)    :: H                  ! height of enclosure
  REAL(r64)    :: minorW             ! width of enclosure (narrow dimension)
  REAL(r64)    :: majorW             ! width of major surface
  REAL(r64)    :: gapW               ! width of air gap
  REAL(r64)  :: asp      ! aspect ratio H/gapW
  REAL(r64)  :: beta     ! volumetric thermal expansion coefficient
  REAL(r64)  :: Gr       ! Grashof number
  REAL(r64)  :: Nu       ! Nusselt number
  REAL(r64)    :: HConvNet           ! net heat transfer coefficient from wall to wall
  REAL(r64)  :: Tso      ! outside surface temperature [K]
  REAL(r64)  :: Tsi      ! inside surface temperature [K]

  ! If the Trombe Wall option is selected the following correlations
  ! will be used based on references by .....
  ! tall enclosed rectangular cavity

  ! This routine assumes that the major Trombe wall surfaces are of the
  ! "WALL" class and are vertical.  The important heat transfer surfaces
  ! are assumed to have exactly equal widths AND must have a greater
  ! width than the side surfaces.
  !

  Surf1 = 0
  Surf2 = 0

  H = Zone(ZoneNum)%CeilingHeight
  minorW = 100000.0d0 ! An impossibly big width
  majorW = 0.0d0
  gapW = 0.0d0

  Tso = 0.0d0
  Tsi = 0.0d0
  HConvNet = 0.0d0

  ! determine major width and minor width
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (Surface(SurfNum)%Class .NE. SurfaceClass_Wall) CYCLE

    IF (Surface(SurfNum)%Width > majorW) THEN
      majorW = Surface(SurfNum)%Width
    END IF

    IF (Surface(SurfNum)%Width < minorW) THEN
      minorW = Surface(SurfNum)%Width
    END IF
  END DO

  ! assign major surfaces
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (Surface(SurfNum)%Class .NE. SurfaceClass_Wall) CYCLE

    IF (Surface(SurfNum)%Width == majorW) THEN
      IF (Surf1 == 0) THEN
        Surf1 = SurfNum
      ELSE
        Surf2 = SurfNum

        EXIT ! both major surfaces are now assigned
      END IF
    END IF
  END DO

  ! check to make sure major surfaces were found
  IF (Surf1 > 0 .AND. Surf2 > 0) THEN
    gapW = minorW
    asp = H/gapW  ! This calc should only be done once for the zone

    ! make sure inside surface is hot, outside is cold
    ! NOTE: this is not ideal.  could have circumstances that reverse this?
    IF (SurfaceTemperatures(Surf1) > SurfaceTemperatures(Surf2)) THEN
      Tsi = SurfaceTemperatures(Surf1) + KelvinConv
      Tso = SurfaceTemperatures(Surf2) + KelvinConv
    ELSE
      Tso = SurfaceTemperatures(Surf1) + KelvinConv
      Tsi = SurfaceTemperatures(Surf2) + KelvinConv
    ENDIF

    beta = 2.0d0/(Tso + Tsi)

    Gr = (g*beta*ABS(Tsi - Tso)*gapW**3)/(v**2) ! curve fit for v = v(T)?

    CALL CalcNusselt(SurfNum, asp, Tso, Tsi, Gr, Pr, Nu) ! curve fit for Pr = Pr(T)?

    HConvNet = (k/gapW)*Nu ! curve fit for k = k(T)?

  ELSE
    ! fatal Error msg "heat transfer surfaces not found"
  END IF

  ! Assign convection coefficients
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    ! Use ASHRAESimple correlation to give values for all the minor surfaces
    CALL CalcASHRAESimpleIntConvCoeff(SurfNum,SurfaceTemperatures(SurfNum),MAT(ZoneNum))

    ! assign the convection coefficent to the major surfaces and any subsurfaces on them
    IF ((Surface(SurfNum)%BaseSurf == Surf1) .OR. (Surface(SurfNum)%BaseSurf == Surf2)) THEN
      HConvIn(SurfNum) = 2.0d0*HConvNet
    END IF

    ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
    IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit
  END DO

  RETURN

END SUBROUTINE CalcTrombeWallIntConvCoeff


SUBROUTINE CalcNusselt(SurfNum, asp, tso, tsi, gr, pr, gnu)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis, based on code adapted by Fred Winkelmann
          !                      from Window5 subroutine NusseltNumber
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
  REAL(r64)  , INTENT(IN)       :: asp               ! Aspect ratio: window height to gap width
  REAL(r64)  , INTENT(IN)       :: pr                ! Gap gas Prandtl number
  REAL(r64)  , INTENT(IN)       :: gr                ! Gap gas Grashof number
  REAL(r64)  , INTENT(OUT)      :: gnu               ! Gap gas Nusselt number

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
  REAL(r64)  :: ra                                 ! Rayleigh number
  REAL(r64)  :: gnu901, gnu902, gnu90, gnu601      ! Nusselt number temporary variables for
  REAL(r64)  :: gnu602, gnu60, gnu601a, gnua, gnub !  different tilt and Ra ranges
  REAL(r64)  :: cra, a, b, g, ang, tilt, tiltr     ! Temporary variables
  REAL(r64)  :: costilt,sintilt


  tilt = Surface(SurfNum)%Tilt
  tiltr = tilt * DegToRadians
  costilt = Surface(SurfNum)%CosTilt
  sintilt = Surface(SurfNum)%SinTilt
  ra = gr*pr
                         !!fw if (ra > 2.0e6): error that outside range of Rayleigh number?

  IF (ra <= 1.0d4)                  gnu901 = 1.d0 + 1.7596678d-10 * ra**2.2984755d0   ! eq. 51
  IF (ra > 1.0d4 .and. ra <= 5.0d4) gnu901 =      0.028154d0      * ra**0.4134d0      ! eq. 50
  IF (ra > 5.0d4)                   gnu901 =      0.0673838d0     * ra**(1.0d0/3.0d0)   ! eq. 49

  gnu902 = 0.242d0 * (ra/asp)**.272d0               ! eq. 52
  gnu90 = MAX(gnu901,gnu902)

  IF (tso > tsi) THEN ! window heated from above
    gnu = 1.0d0 + (gnu90-1.0d0)*sintilt                     ! eq. 53
  ELSE                ! window heated from below
    IF (tilt >= 60.0d0) THEN
      g       = 0.5d0 * (1.0d0+(ra/3160.d0)**20.6d0)**(-0.1d0)    ! eq. 47
      gnu601a = 1.0d0 + (0.0936d0*(ra**0.314d0)/(1.0d0+g))**7   ! eq. 45
      gnu601  = gnu601a**0.142857d0

      ! For any aspect ratio
      gnu602  = (0.104d0+0.175d0/asp) * ra**0.283d0           ! eq. 46
      gnu60   = MAX(gnu601,gnu602)

      ! linear interpolation for layers inclined at angles between 60 and 90 deg
      gnu     = ((90.0d0-tilt)*gnu60 + (tilt-60.0d0)*gnu90)/30.0d0
    ENDIF
    IF (tilt < 60.0d0) THEN                               ! eq. 42
      cra  = ra*costilt
      a    = 1.0d0 - 1708.0d0/cra
      b    = (cra/5830.0d0)**0.33333d0-1.0d0    ! LKL- replace .333 with OneThird?
      gnua = (ABS(a)+a)/2.0d0
      gnub = (ABS(b)+b)/2.0d0
      ang  = 1708.0d0 * (SIN(1.8d0*tiltr))**1.6d0
      gnu  = 1.0d0 + 1.44d0*gnua*(1.0d0-ang/cra) + gnub
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CalcNusselt

REAL(r64) FUNCTION SetExtConvectionCoeff(SurfNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   May 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accesses the data structure for the User
          ! Supplied Exterior Convection Coefficients and returns that
          ! as the result of this function.  The surface has already
          ! been verified to have user supplied exterior convection values.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfNum  ! Surface Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) HExt   ! Will become the returned value

    SELECT CASE (UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%OverrideType)

    CASE (ConvCoefValue)
      HExt=UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%OverrideValue
      Surface(SurfNum)%OutConvHfModelEq = HcExt_UserValue  !reporting
      Surface(SurfNum)%OutConvHnModelEq = HcExt_None  !reporting
    CASE (ConvCoefSchedule)
      HExt=GetCurrentScheduleValue(UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%ScheduleIndex)
      ! Need to check for validity
      Surface(SurfNum)%OutConvHfModelEq = HcExt_UserSchedule  !reporting
      Surface(SurfNum)%OutConvHnModelEq = HcExt_None  !reporting
    CASE (ConvCoefUserCurve)
      Call CalcUserDefinedOutsideHcModel(SurfNum, &
                            UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%UserCurveIndex, HExt)
      Surface(SurfNum)%OutConvHfModelEq = HcExt_UserCurve  !reporting
      Surface(SurfNum)%OutConvHnModelEq = HcExt_None  !reporting
    CASE (ConvCoefSpecifiedModel)
      CALL EvaluateExtHcModels(SurfNum, &
                       UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%HcModelEq,  &
                       UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%HcModelEq, HExt)
      Surface(SurfNum)%OutConvHfModelEq = UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%HcModelEq  !reporting
      Surface(SurfNum)%OutConvHnModelEq = UserExtConvectionCoeffs(Surface(SurfNum)%ExtConvCoeff)%HcModelEq  !reporting
    END SELECT

    SetExtConvectionCoeff=HExt

  RETURN

END FUNCTION SetExtConvectionCoeff

REAL(r64) FUNCTION SetIntConvectionCoeff(SurfNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   May 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accesses the data structre for the User
          ! Supplied Interior Convection Coefficients and returns that
          ! as the result of this function.  The surface has already
          ! been verified to have user supplied interior convection values.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfNum  ! Surface Number

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    REAL(r64) HInt   ! Will become the returned value

    SELECT CASE (UserIntConvectionCoeffs(Surface(SurfNum)%IntConvCoeff)%OverrideType)

    CASE (ConvCoefValue)
      HInt=UserIntConvectionCoeffs(Surface(SurfNum)%IntConvCoeff)%OverrideValue
      Surface(SurfNum)%IntConvHcModelEq = HcInt_UserValue !reporting
    CASE (ConvCoefSchedule)
      HInt=GetCurrentScheduleValue(UserIntConvectionCoeffs(Surface(SurfNum)%IntConvCoeff)%ScheduleIndex)
      ! Need to check for validity
      Surface(SurfNum)%IntConvHcModelEq = HcInt_UserSchedule !reporting
    CASE (ConvCoefUserCurve)

      CALL CalcUserDefinedInsideHcModel(SurfNum, &
                    UserIntConvectionCoeffs(Surface(SurfNum)%IntConvCoeff)%UserCurveIndex, HInt)
        Surface(SurfNum)%IntConvHcModelEq = HcInt_UserCurve !reporting
    CASE ( ConvCoefSpecifiedModel )

      CALL EvaluateIntHcModels(SurfNum, UserIntConvectionCoeffs(Surface(SurfNum)%IntConvCoeff)%HcModelEq, HInt)
      Surface(SurfNum)%IntConvHcModelEq = UserIntConvectionCoeffs(Surface(SurfNum)%IntConvCoeff)%HcModelEq
    END SELECT

    SetIntConvectionCoeff=HInt

  RETURN

END FUNCTION SetIntConvectionCoeff

SUBROUTINE CalcISO15099WindowIntConvCoeff(SurfNum,SurfaceTemperature,AirTemperature)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   January 2009
          !       MODIFIED       BG May 2009, added EMS override for window coeffs.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate interior surface convection coefficients for windows

          ! METHODOLOGY EMPLOYED:
          ! correlation documented in ISO 15099, Section 8.3.2.2

          ! REFERENCES:
          ! Internation Standard ISO 15099. Thermal performance of windows, doors and shading devices -- Detailed Calculations
          ! First Edition 2003-11-15. ISO 15099:2003(E)

          ! USE STATEMENTS:
  USE Psychrometrics ,   ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW
  USE DataHeatBalFanSys, ONLY: ZoneAirHumRatAvg
  USE DataEnvironment,   ONLY: OutHumRat, OutBaroPress
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: SurfNum ! surface number for which coefficients are being calculated
  REAL(r64), INTENT(IN)   :: SurfaceTemperature ! Temperature of surface for evaluation of HcIn
  REAL(r64), INTENT(IN)   :: AirTemperature  ! Mean Air Temperature of Zone (or adjacent air temperature)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: OneThird = (1.d0/3.d0)  ! 1/3 in highest precision

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: DeltaTemp          ! Temperature difference between the zone air and the surface
  REAL(r64)  :: TmeanFilm  ! mean film temperature
  REAL(r64)  :: TmeanFilmKelvin !  mean film temperature for property evaluation
  REAL(r64)  :: rho     ! density of air [kg/m3]
  REAL(r64)  :: g       ! acceleration due to gravity [m/s2]
  REAL(r64)  :: Height  ! window cavity height [m]
  REAL(r64)  :: Cp      ! specific heat of air [J/kg-K]
  REAL(r64)  :: lambda  ! thermal conductivity of air [W/m-K]
  REAL(r64)  :: mu      ! dynamic viscosity of air [kg/m-s]
  REAL(r64)  :: RaH     ! Rayleigh number for cavity height [ Non dim]
  REAL(r64)  :: RaCV    ! Rayleigh number for slanted cavity
  REAL(r64)  :: tiltDeg ! glazing tilt in degrees
  REAL(r64)  :: sineTilt ! sine of glazing tilt
  REAL(r64)  :: Nuint    ! Nusselt number for interior surface convection
  REAL(r64)  :: SurfTempKelvin ! surface temperature in Kelvin
  REAL(r64)  :: AirTempKelvin  ! air temperature in Kelvin
  REAL(r64)  :: AirHumRat  ! air humidity ratio

  SurfTempKelvin = SurfaceTemperature +  273.15D0
  AirTempKelvin  = AirTemperature     + 273.15D0
  DeltaTemp      = SurfaceTemperature - AirTemperature

  ! protect against wildly out of range temperatures
  IF ((AirTempKelvin < 200.0D0) .OR. (AirTempKelvin > 400.0D0)) THEN ! out of range
    HConvIn(SurfNum) = LowHConvLimit
    RETURN
  ENDIF
  IF ((SurfTempKelvin < 180.0D0) .OR. (SurfTempKelvin > 450.0D0)) THEN ! out of range
    HConvIn(SurfNum) = LowHConvLimit
    RETURN
  ENDIF

  ! Get humidity ratio
  IF (Surface(SurfNum)%Zone > 0) Then
    AirHumRat = ZoneAirHumRatAvg(Surface(SurfNum)%Zone)
  ELSE
    AirHumRat = OutHumRat
  ENDIF

  ! mean film temperature
  TmeanFilmKelvin = AirTempKelvin + 0.25D0*(SurfTempKelvin - AirTempKelvin) ! eq. 133 in ISO 15099
  TmeanFilm = TmeanFilmKelvin - 273.15D0

  rho    = PsyRhoAirFnPbTdbW(OutBaroPress, TmeanFilm, AirHumRat, 'WindowTempsForNominalCond')
  g      = 9.81D0
  Height = Surface(SurfNum)%Height

  ! the following properties are probably for dry air, should maybe be remade for moist-air
  lambda = 2.873D-3 + 7.76D-5   * TmeanFilmKelvin ! Table B.1 in ISO 15099,
  mu     = 3.723D-6 + 4.94D-8   * TmeanFilmKelvin ! Table B.2 in ISO 15099

  Cp     = PsyCpAirFnWTdb(AirHumRat,  TmeanFilm, 'WindowTempsForNominalCond')

  TiltDeg= Surface(SurfNum)%Tilt
  sineTilt=Surface(SurfNum)%SinTilt

  ! four cases depending on tilt and DeltaTemp (heat flow direction )
  If (DeltaTemp > 0.0d0 ) TiltDeg = 180.0D0 - TiltDeg ! complement angle if cooling situation

  RaH = ( rho**2 * Height**3 * g * Cp *(ABS(SurfTempKelvin-AirTempKelvin) ) ) &
           / (TmeanFilmKelvin * mu * lambda) ! eq 132 in ISO 15099

  ! case a)
  IF ( (0.0D0 <= TiltDeg) .AND. (TiltDeg  < 15.0D0) ) THEN

    Nuint =  0.13d0*(RaH)**OneThird

  ! case b)
  ELSEIF ( (15.0D0 <= TiltDeg) .AND. (TiltDeg  <= 90.0D0) ) THEN

    RaCV = 2.5D+5 * ( EXP(0.72d0*TiltDeg) / sineTilt)**0.2D0 ! eq. 137

    IF (RaH <= RaCV) Then
      Nuint = 0.56D0*(RaH * sineTilt)**0.25d0  ! eq. 135 in ISO 15099
    ELSE
      Nuint = 0.13D0 * (RaH**OneThird - RaCV**OneThird) &
              + 0.56D0 * (RaCV * sineTilt )**0.25D0 ! eq. 136 in ISO 15099
    ENDIF

  !case c)
  ELSEIF  ( (90.0D0 < TiltDeg) .AND. (TiltDeg  <= 179.0D0) ) THEN
    ! bound by applicability
    IF (RaH * sineTilt < 1.0D+5) THEN
      Nuint = 0.56D0*(1.0D+5)**0.25d0 ! bounded
    ElseIF (RaH * sineTilt >= 1.0D+11 ) THEN
      Nuint = 0.56D0*(1.0D+11)**0.25d0 ! bounded
    Else
      Nuint = 0.56D0*(RaH * sineTilt)**0.25d0 ! eq.. 138
    ENDIF

  ! case d)
  ELSEIF  ( (179.0D0 < TiltDeg) .AND. (TiltDeg  <= 180.0D0) ) THEN

    IF (RaH > 1.0D+11) THEN
      Nuint = 0.58d0*1D+11**0.2D0  ! bounded
    ELSE
      Nuint = 0.58d0*RaH**0.2D0
    ENDIF

  ENDIF

  HConvIn(SurfNum) =  Nuint * lambda / Height

  ! EMS override point (Violates Standard 15099?  throw warning? scary.
  IF (Surface(SurfNum)%EMSOverrideIntConvCoef) HConvIn(SurfNum) = Surface(SurfNum)%EMSValueForIntConvCoef

  ! Establish some lower limit to avoid a zero convection coefficient (and potential divide by zero problems)
  IF (HConvIn(SurfNum) < LowHConvLimit) HConvIn(SurfNum) = LowHConvLimit

  RETURN

END SUBROUTINE CalcISO15099WindowIntConvCoeff

SUBROUTINE SetupAdaptiveConvectionStaticMetaData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !do one-time setup needed to store static data
          ! for adaptive convection algorithm

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! none, developed for EnergyPlus version 6.0, see Eng Ref.

          ! USE STATEMENTS:
  USE Vectors, ONLY: Distance, DetermineAzimuthAndTilt, CreateNewellSurfaceNormalVector,CreateNewellAreaVector, VecLength
  USE General, ONLY: ScanForReports, RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:


  TYPE FacadeGeoCharactisticsStruct
    REAL(r64) :: AzimuthRangeLow
    REAL(r64) :: AzimuthRangeHi
    REAL(r64) :: Zmax
    REAL(r64) :: Zmin
    REAL(r64) :: Ymax
    REAL(r64) :: Ymin
    REAL(r64) :: Xmax
    REAL(r64) :: Xmin
    REAL(r64) :: Area
    REAL(r64) :: Perimeter
    REAL(r64) :: Height
  END TYPE FacadeGeoCharactisticsStruct


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: FirstRoofSurf = .TRUE.
  INTEGER :: ZoneLoop !
  INTEGER :: SurfLoop !
  INTEGER :: VertLoop
!  REAL(r64) :: thisZoneHeight
  REAL(r64) :: BldgVolumeSum
  REAL(r64) :: PerimExtLengthSum

  REAL(r64) :: thisWWR
  REAL(r64) :: thisZoneSimplePerim
  REAL(r64) :: thisZoneHorizHydralicDiameter
  INTEGER   :: ExtWallCount
  INTEGER   :: ExtWindowCount
  REAL(r64) :: thisAzimuth
  REAL(r64) :: thisArea
  INTEGER   :: thisZone
  REAL(r64), DIMENSION(8)    :: RoofBoundZvals
  REAL(r64), DIMENSION(4)    :: TestDist
!  TYPE(vector), DIMENSION(4) :: BoundSurf
  Type(vector)               :: BoundNewellVec
  Type(vector)               :: BoundNewellAreaVec
  REAL(r64)                  :: surfacearea
  REAL(r64)                  :: BoundTilt
  REAL(r64)                  :: BoundAzimuth
  Type(vector)               :: dummy1
  Type(vector)               :: dummy2
  Type(vector)               :: dummy3
  LOGICAL DoReport
  REAL(r64) :: SideALength
  REAL(r64) :: SideBLength
  REAL(r64) :: SideCLength
  REAL(r64) :: SideDLength
  CHARACTER(len=3) :: YesNo1
  CHARACTER(len=3) :: YesNo2

  TYPE(FacadeGeoCharactisticsStruct) ::  NorthFacade = &
       FacadeGeoCharactisticsStruct(332.5d0, 22.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  NorthEastFacade = &
       FacadeGeoCharactisticsStruct(22.5d0, 67.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  EastFacade = &
       FacadeGeoCharactisticsStruct(67.5d0, 112.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  SouthEastFacade = &
       FacadeGeoCharactisticsStruct(112.5d0, 157.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  SouthFacade = &
       FacadeGeoCharactisticsStruct(157.5d0, 202.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  SouthWestFacade = &
       FacadeGeoCharactisticsStruct(202.5d0, 247.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  WestFacade = &
       FacadeGeoCharactisticsStruct(247.5d0, 287.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )
  TYPE(FacadeGeoCharactisticsStruct) ::  NorthWestFacade = &
       FacadeGeoCharactisticsStruct(287.5d0, 332.5d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 )



  BldgVolumeSum = 0.d0
  RoofBoundZVals=0.0d0
  DO ZoneLoop=1, NumOfZones

    BldgVolumeSum = BldgVolumeSum + Zone(ZoneLoop)%Volume*Zone(ZoneLoop)%Multiplier*Zone(ZoneLoop)%ListMultiplier
    PerimExtLengthSum   = 0.d0 ! init
    ExtWallCount        = 0    ! init
    ExtWindowCount      = 0    ! init
    !model perimeter of bounding horizontal rectangle from max and min x and y values
    thisZoneSimplePerim =  2.d0 * (Zone(ZoneLoop)%MaximumY - Zone(ZoneLoop)%MinimumY) &
                         + 2.d0 * (Zone(ZoneLoop)%MaximumX - Zone(ZoneLoop)%MinimumX)
    IF (thisZoneSimplePerim > 0.d0) THEN
      thisZoneHorizHydralicDiameter = 4.d0*Zone(ZoneLoop)%FloorArea / thisZoneSimplePerim
    ELSE
      IF (Zone(ZoneLoop)%FloorArea > 0.d0) THEN
        thisZoneHorizHydralicDiameter = SQRT(Zone(ZoneLoop)%FloorArea)
      ENDIF
    ENDIF

    IF (Zone(ZoneLoop)%ExtGrossWallArea > 0.d0 ) THEN
     thisWWR = Zone(ZoneLoop)%ExtWindowArea / Zone(ZoneLoop)%ExtGrossWallArea
    ELSE
     thisWWR = -999.d0  !throw error?
    ENDIF
    ! first pass thru this zones surfaces to gather data
    DO SurfLoop = Zone(ZoneLoop)%SurfaceFirst, Zone(ZoneLoop)%SurfaceLast
      !first catch exterior walls and do summations
      IF (  ( Surface(SurfLoop)%ExtBoundCond == ExternalEnvironment) &
         .AND. (Surface(SurfLoop)%Class == SurfaceClass_Wall)  ) THEN
        PerimExtLengthSum   = PerimExtLengthSum + Surface(SurfLoop)%Width
        ExtWallCount = ExtWallCount + 1
      ENDIF
      IF (  ( Surface(SurfLoop)%ExtBoundCond == ExternalEnvironment) &
         .AND. ((Surface(SurfLoop)%Class == SurfaceClass_Window)     &
               .OR. (Surface(SurfLoop)%Class ==  SurfaceClass_GlassDoor) )) THEN
        ExtWindowCount   = ExtWindowCount + 1
      ENDIF
    ENDDO

    !second pass thru zone surfs to fill data
    DO SurfLoop = Zone(ZoneLoop)%SurfaceFirst, Zone(ZoneLoop)%SurfaceLast
      !now fill values
      Surface(SurfLoop)%IntConvZoneWallHeight    = Zone(ZoneLoop)%CeilingHeight
      Surface(SurfLoop)%IntConvZonePerimLength   = PerimExtLengthSum
      Surface(SurfLoop)%IntConvZoneHorizHydrDiam = thisZoneHorizHydralicDiameter
      Surface(SurfLoop)%IntConvWindowWallRatio   = thisWWR
    ENDDO !2nd pass over surfaces.

    !third pass for window locations
    IF ((ExtWindowCount > 0) .AND. (ExtWallCount > 0)) THEN
      DO SurfLoop = Zone(ZoneLoop)%SurfaceFirst, Zone(ZoneLoop)%SurfaceLast
        IF (  ( Surface(SurfLoop)%ExtBoundCond == ExternalEnvironment) &
           .AND. ((Surface(SurfLoop)%Class == SurfaceClass_Window)     &
               .OR. (Surface(SurfLoop)%Class ==  SurfaceClass_GlassDoor) )) THEN
          IF (Surface(SurfLoop)%IntConvWindowWallRatio < 0.5d0) THEN
            IF (Surface(SurfLoop)%Centroid%Z < Zone(ZoneLoop)%Centroid%Z) THEN
              Surface(SurfLoop)%IntConvWindowLocation = InConvWinLoc_LowerPartOfExteriorWall
            ELSE
              Surface(SurfLoop)%IntConvWindowLocation = InConvWinLoc_UpperPartOfExteriorWall
            ENDIF
          ELSE
            Surface(SurfLoop)%IntConvWindowLocation = InConvWinLoc_LargePartOfExteriorWall
          ENDIF
          IF (  ( Surface(Surface(SurfLoop)%BaseSurf)%ExtBoundCond == ExternalEnvironment) &
              .AND. (Surface(Surface(SurfLoop)%BaseSurf)%Class == SurfaceClass_Wall)  ) THEN
            IF (Surface(Surface(SurfLoop)%BaseSurf)%Centroid%Z < Surface(SurfLoop)%Centroid%z) THEN
              Surface(Surface(SurfLoop)%BaseSurf)%IntConvWindowLocation = InConvWinLoc_WindowAboveThis
            ELSE
              Surface(Surface(SurfLoop)%BaseSurf)%IntConvWindowLocation = InConvWinLoc_WindowBelowThis
            ENDIF
          ENDIF
        ENDIF
        IF (  ( Surface(SurfLoop)%ExtBoundCond == ExternalEnvironment) &
           .AND. (Surface(SurfLoop)%Class == SurfaceClass_Wall)        &
           .AND.  (Surface(SurfLoop)%IntConvWindowLocation == InConvWinLoc_NotSet)) THEN
          IF (Surface(SurfLoop)%Centroid%z < Zone(ZoneLoop)%Centroid%z) THEN
            Surface(SurfLoop)%IntConvWindowLocation = InConvWinLoc_WindowAboveThis
          ELSE
            Surface(SurfLoop)%IntConvWindowLocation = InConvWinLoc_WindowBelowThis
          ENDIF

        ENDIF
      ENDDO !third pass over surfaces

    ENDIF
  ENDDO !loop over zones for inside face parameters

  CubeRootOfOverallBuildingVolume = (BldgVolumeSum)**OneThird

  ! first pass over surfaces for outside face params
  DO SurfLoop=1, TotSurfaces
    IF (Surface(SurfLoop)%ExtBoundCond /= ExternalEnvironment) CYCLE
    IF (.NOT. Surface(SurfLoop)%HeatTransSurf) CYCLE
    thisAzimuth = Surface(SurfLoop)%Azimuth
    thisArea    = Surface(SurfLoop)%Area
    thisZone    = Surface(SurfLoop)%Zone
    IF ((Surface(SurfLoop)%Tilt >= 45.d0) .AND. (Surface(SurfLoop)%Tilt < 135.d0) ) THEN
     !treat as vertical wall
      IF ((thisAzimuth >= NorthFacade%AzimuthRangeLow) .OR. &
          (thisAzimuth < NorthFacade%AzimuthRangeHi)) THEN
        NorthFacade%Area = NorthFacade%Area + thisArea
        NorthFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  NorthFacade%Zmax)
        NorthFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  NorthFacade%Zmin)
        NorthFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  NorthFacade%Ymax)
        NorthFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  NorthFacade%Ymin)
        NorthFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  NorthFacade%Xmax)
        NorthFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  NorthFacade%Xmin)

      ELSEIF ((thisAzimuth >= NorthEastFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < NorthEastFacade%AzimuthRangeHi)) THEN
        NorthEastFacade%Area = NorthEastFacade%Area + thisArea
        NorthEastFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  NorthEastFacade%Zmax)
        NorthEastFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  NorthEastFacade%Zmin)
        NorthEastFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  NorthEastFacade%Ymax)
        NorthEastFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  NorthEastFacade%Ymin)
        NorthEastFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  NorthEastFacade%Xmax)
        NorthEastFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  NorthEastFacade%Xmin)

      ELSEIF ((thisAzimuth >= EastFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < EastFacade%AzimuthRangeHi)) THEN
        EastFacade%Area = EastFacade%Area + thisArea
        EastFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  EastFacade%Zmax)
        EastFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  EastFacade%Zmin)
        EastFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  EastFacade%Ymax)
        EastFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  EastFacade%Ymin)
        EastFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  EastFacade%Xmax)
        EastFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  EastFacade%Xmin)
      ELSEIF ((thisAzimuth >= SouthEastFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < SouthEastFacade%AzimuthRangeHi)) THEN
        SouthEastFacade%Area = SouthEastFacade%Area + thisArea
        SouthEastFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  SouthEastFacade%Zmax)
        SouthEastFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  SouthEastFacade%Zmin)
        SouthEastFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  SouthEastFacade%Ymax)
        SouthEastFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  SouthEastFacade%Ymin)
        SouthEastFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  SouthEastFacade%Xmax)
        SouthEastFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  SouthEastFacade%Xmin)

      ELSEIF ((thisAzimuth >= SouthFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < SouthFacade%AzimuthRangeHi)) THEN
        SouthFacade%Area = SouthFacade%Area + thisArea
        SouthFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  SouthFacade%Zmax)
        SouthFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  SouthFacade%Zmin)
        SouthFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  SouthFacade%Ymax)
        SouthFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  SouthFacade%Ymin)
        SouthFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  SouthFacade%Xmax)
        SouthFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  SouthFacade%Xmin)

      ELSEIF ((thisAzimuth >= SouthWestFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < SouthWestFacade%AzimuthRangeHi)) THEN
        SouthWestFacade%Area = SouthWestFacade%Area + thisArea
        SouthWestFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  SouthWestFacade%Zmax)
        SouthWestFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  SouthWestFacade%Zmin)
        SouthWestFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  SouthWestFacade%Ymax)
        SouthWestFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  SouthWestFacade%Ymin)
        SouthWestFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  SouthWestFacade%Xmax)
        SouthWestFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  SouthWestFacade%Xmin)

      ELSEIF ((thisAzimuth >= WestFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < WestFacade%AzimuthRangeHi)) THEN
        WestFacade%Area = WestFacade%Area + thisArea
        WestFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  WestFacade%Zmax)
        WestFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  WestFacade%Zmin)
        WestFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  WestFacade%Ymax)
        WestFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  WestFacade%Ymin)
        WestFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  WestFacade%Xmax)
        WestFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  WestFacade%Xmin)

      ELSEIF ((thisAzimuth >= NorthWestFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < NorthWestFacade%AzimuthRangeHi)) THEN
        NorthWestFacade%Area = NorthWestFacade%Area + thisArea
        NorthWestFacade%Zmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  NorthWestFacade%Zmax)
        NorthWestFacade%Zmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%z),  NorthWestFacade%Zmin)
        NorthWestFacade%Ymax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  NorthWestFacade%Ymax)
        NorthWestFacade%Ymin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%y),  NorthWestFacade%Ymin)
        NorthWestFacade%Xmax = MAX(MAXVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  NorthWestFacade%Xmax)
        NorthWestFacade%Xmin = MIN(MINVAL(Surface(SurfLoop)%Vertex(1:Surface(SurfLoop)%Sides)%x),  NorthWestFacade%Xmin)

      ENDIF
    ELSEIF (Surface(SurfLoop)%Tilt < 45.0d0) THEN !TODO Double check tilt wrt outside vs inside

      IF (FirstRoofSurf) THEN !Init with something in the group
        RoofGeo%XdYdZd%SurfNum = SurfLoop
        RoofGeo%XdYdZd%VertNum = 1
        RoofGeo%XdYdZd%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XdYdZu%SurfNum = SurfLoop
        RoofGeo%XdYdZu%VertNum = 1
        RoofGeo%XdYdZu%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XdYuZd%SurfNum = SurfLoop
        RoofGeo%XdYuZd%VertNum = 1
        RoofGeo%XdYuZd%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XdYuZu%SurfNum = SurfLoop
        RoofGeo%XdYuZu%VertNum = 1
        RoofGeo%XdYuZu%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XuYdZd%SurfNum = SurfLoop
        RoofGeo%XuYdZd%VertNum = 1
        RoofGeo%XuYdZd%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XuYuZd%SurfNum = SurfLoop
        RoofGeo%XuYuZd%VertNum = 1
        RoofGeo%XuYuZd%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XuYdZu%SurfNum = SurfLoop
        RoofGeo%XuYdZu%VertNum = 1
        RoofGeo%XuYdZu%Vertex = Surface(SurfLoop)%Vertex(1)

        RoofGeo%XuYuZu%SurfNum = SurfLoop
        RoofGeo%XuYuZu%VertNum = 1
        RoofGeo%XuYuZu%Vertex = Surface(SurfLoop)%Vertex(1)

        FirstRoofSurf = .FALSE.
      ENDIF
      ! treat as part of roof group
      RoofGeo%Area = RoofGeo%Area + thisArea
      DO VertLoop = 1, Surface(SurfLoop)%Sides

        !1 low x, low y, low z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X <= RoofGeo%XdYdZd%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y <= RoofGeo%XdYdZd%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z <= RoofGeo%XdYdZd%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XdYdZd%SurfNum = SurfLoop
            RoofGeo%XdYdZd%VertNum = VertLoop
            RoofGeo%XdYdZd%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(1)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 2 low x, low y, hi z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X <= RoofGeo%XdYdZu%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y <= RoofGeo%XdYdZu%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z >= RoofGeo%XdYdZu%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XdYdZu%SurfNum = SurfLoop
            RoofGeo%XdYdZu%VertNum = VertLoop
            RoofGeo%XdYdZu%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(2)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 3 low x, hi y, low z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X <= RoofGeo%XdYuZd%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y >= RoofGeo%XdYuZd%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z <= RoofGeo%XdYuZd%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XdYuZd%SurfNum = SurfLoop
            RoofGeo%XdYuZd%VertNum = VertLoop
            RoofGeo%XdYuZd%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(3)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 4 low x, hi y, hi z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X <= RoofGeo%XdYuZu%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y >= RoofGeo%XdYuZu%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z >= RoofGeo%XdYuZu%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XdYuZu%SurfNum = SurfLoop
            RoofGeo%XdYuZu%VertNum = VertLoop
            RoofGeo%XdYuZu%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(4)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 5 hi x, low y, low z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X >= RoofGeo%XuYdZd%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y <= RoofGeo%XuYdZd%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z <= RoofGeo%XuYdZd%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XuYdZd%SurfNum = SurfLoop
            RoofGeo%XuYdZd%VertNum = VertLoop
            RoofGeo%XuYdZd%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(5)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 6 hi x, hi y, low z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X >= RoofGeo%XuYuZd%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y >= RoofGeo%XuYuZd%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z <= RoofGeo%XuYuZd%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XuYuZd%SurfNum = SurfLoop
            RoofGeo%XuYuZd%VertNum = VertLoop
            RoofGeo%XuYuZd%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(6)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 7 hi x, low y, hi z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X >= RoofGeo%XuYdZu%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y <= RoofGeo%XuYdZu%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z >= RoofGeo%XuYdZu%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XuYdZu%SurfNum = SurfLoop
            RoofGeo%XuYdZu%VertNum = VertLoop
            RoofGeo%XuYdZu%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(7)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

        ! 8 hi x, hi y, hi z
        IF  (     (Surface(SurfLoop)%Vertex(VertLoop)%X >= RoofGeo%XuYuZu%Vertex%X) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Y >= RoofGeo%XuYuZu%Vertex%Y) &
            .AND. (Surface(SurfLoop)%Vertex(VertLoop)%Z >= RoofGeo%XuYuZu%Vertex%Z) ) THEN
            !this point is more toward this bound
            RoofGeo%XuYuZu%SurfNum = SurfLoop
            RoofGeo%XuYuZu%VertNum = VertLoop
            RoofGeo%XuYuZu%Vertex  = Surface(SurfLoop)%Vertex(VertLoop)
            RoofBoundZvals(8)      = Surface(SurfLoop)%Vertex(VertLoop)%Z
        ENDIF

      ENDDO
    ENDIF
  ENDDO ! fist loop over surfaces for outside face params

  NorthFacade%Perimeter     = 2.d0 * (((NorthFacade%Xmax - NorthFacade%Xmin)**2   &
                                     + (NorthFacade%Ymax - NorthFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (NorthFacade%Zmax - NorthFacade%Zmin)
  NorthFacade%Height        = NorthFacade%Zmax - NorthFacade%Zmin

  NorthEastFacade%Perimeter = 2.d0 * (((NorthEastFacade%Xmax - NorthEastFacade%Xmin)**2   &
                                     + (NorthEastFacade%Ymax - NorthEastFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (NorthEastFacade%Zmax - NorthEastFacade%Zmin)
  NorthEastFacade%Height    = NorthEastFacade%Zmax - NorthEastFacade%Zmin

  EastFacade%Perimeter      = 2.d0 * (((EastFacade%Xmax - EastFacade%Xmin)**2   &
                                     + (EastFacade%Ymax - EastFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (EastFacade%Zmax - EastFacade%Zmin)
  EastFacade%Height         = EastFacade%Zmax - EastFacade%Zmin

  SouthEastFacade%Perimeter = 2.d0 * (((SouthEastFacade%Xmax - SouthEastFacade%Xmin)**2   &
                                     + (SouthEastFacade%Ymax - SouthEastFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (SouthEastFacade%Zmax - SouthEastFacade%Zmin)
  SouthEastFacade%Height    = SouthEastFacade%Zmax - SouthEastFacade%Zmin

  SouthFacade%Perimeter     = 2.d0 * (((SouthFacade%Xmax - SouthFacade%Xmin)**2   &
                                     + (SouthFacade%Ymax - SouthFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (SouthFacade%Zmax - SouthFacade%Zmin)
  SouthFacade%Height        = SouthFacade%Zmax - SouthFacade%Zmin

  SouthWestFacade%Perimeter = 2.d0 * (((SouthWestFacade%Xmax - SouthWestFacade%Xmin)**2   &
                                     + (SouthWestFacade%Ymax - SouthWestFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (SouthWestFacade%Zmax - SouthWestFacade%Zmin)
  SouthWestFacade%Height    = SouthWestFacade%Zmax - SouthWestFacade%Zmin

  WestFacade%Perimeter      = 2.d0 * (((WestFacade%Xmax - WestFacade%Xmin)**2   &
                                     + (WestFacade%Ymax - WestFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (WestFacade%Zmax - WestFacade%Zmin)
  WestFacade%Height         = WestFacade%Zmax - WestFacade%Zmin

  NorthWestFacade%Perimeter = 2.d0 * (((NorthWestFacade%Xmax - NorthWestFacade%Xmin)**2   &
                                     + (NorthWestFacade%Ymax - NorthWestFacade%Ymin)**2)**0.5d0) + &
                              2.d0 *   (NorthWestFacade%Zmax - NorthWestFacade%Zmin)
  NorthWestFacade%Height    = NorthWestFacade%Zmax - NorthWestFacade%Zmin


  !now model roof perimeter
  ! move around bounding boxes side walls and find the longest of the four distances
  ! Side A: Y low -- uses XdYdZd, XdYdZu, XuYdZd, XuYdZu
  TestDist(1) = Distance(RoofGeo%XdYdZd%Vertex, RoofGeo%XuYdZd%Vertex)
  TestDist(2) = Distance(RoofGeo%XdYdZd%Vertex, RoofGeo%XuYdZu%Vertex)
  TestDist(3) = Distance(RoofGeo%XdYdZu%Vertex, RoofGeo%XuYdZd%Vertex)
  TestDist(4) = Distance(RoofGeo%XdYdZu%Vertex, RoofGeo%XuYdZu%Vertex)
  SideALength = MAXVAL(TestDist)

  ! Side B: X Hi -- uses XuYdZd, XuYuZd, XuYdZu, XuYuZu
  TestDist(1) = Distance(RoofGeo%XuYdZd%Vertex, RoofGeo%XuYuZd%Vertex)
  TestDist(2) = Distance(RoofGeo%XuYdZd%Vertex, RoofGeo%XuYuZu%Vertex)
  TestDist(3) = Distance(RoofGeo%XuYdZu%Vertex, RoofGeo%XuYuZd%Vertex)
  TestDist(4) = Distance(RoofGeo%XuYdZu%Vertex, RoofGeo%XuYuZu%Vertex)
  SideBLength = MAXVAL(TestDist)

  ! Side C: Y Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
  TestDist(1) = Distance(RoofGeo%XdYuZd%Vertex, RoofGeo%XuYuZd%Vertex)
  TestDist(2) = Distance(RoofGeo%XdYuZd%Vertex, RoofGeo%XuYuZu%Vertex)
  TestDist(3) = Distance(RoofGeo%XdYuZu%Vertex, RoofGeo%XuYuZd%Vertex)
  TestDist(4) = Distance(RoofGeo%XdYuZu%Vertex, RoofGeo%XuYuZu%Vertex)
  SideCLength = MAXVAL(TestDist)

  ! Side D: X Lo Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
  TestDist(1) = Distance(RoofGeo%XdYuZd%Vertex, RoofGeo%XuYuZd%Vertex)
  TestDist(2) = Distance(RoofGeo%XdYuZd%Vertex, RoofGeo%XuYuZu%Vertex)
  TestDist(3) = Distance(RoofGeo%XdYuZu%Vertex, RoofGeo%XuYuZd%Vertex)
  TestDist(4) = Distance(RoofGeo%XdYuZu%Vertex, RoofGeo%XuYuZu%Vertex)
  SideDLength = MAXVAL(TestDist)

  RoofGeo%Perimeter   = SideALength + SideBLength + SideCLength + SideDLength

  RoofGeo%Height = MAXVAL(RoofBoundZvals) - MINVAL(RoofBoundZvals)

  ! now find the longest bound face
  IF (       ( SideALength >= SideBLength) &
       .AND. ( SideALength >= SideCLength) &
       .AND. ( SideALength >= SideDLength) ) THEN
       ! Side A: Y low -- uses XdYdZd, XdYdZu, XuYdZd, XuYdZu
       RoofGeo%BoundSurf(1) = RoofGeo%XdYdZd%Vertex
       RoofGeo%BoundSurf(2) = RoofGeo%XuYdZd%Vertex
       RoofGeo%BoundSurf(3) = RoofGeo%XuYdZu%Vertex
       RoofGeo%BoundSurf(4) = RoofGeo%XdYdZu%Vertex

  ELSEIF (   ( SideBLength >= SideALength) &
       .AND. ( SideBLength >= SideCLength) &
       .AND. ( SideBLength >= SideDLength) ) THEN
       ! Side B: X Hi -- uses XuYdZd, XuYuZd, XuYdZu, XuYuZu
       RoofGeo%BoundSurf(1) = RoofGeo%XuYdZd%Vertex
       RoofGeo%BoundSurf(2) = RoofGeo%XuYuZd%Vertex
       RoofGeo%BoundSurf(3) = RoofGeo%XuYuZu%Vertex
       RoofGeo%BoundSurf(4) = RoofGeo%XuYdZu%Vertex
  ELSEIF (   ( SideCLength >= SideALength) &
       .AND. ( SideCLength >= SideBLength) &
       .AND. ( SideCLength >= SideDLength) ) THEN
       ! Side C: Y Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
       RoofGeo%BoundSurf(1) = RoofGeo%XdYuZd%Vertex
       RoofGeo%BoundSurf(2) = RoofGeo%XuYuZd%Vertex
       RoofGeo%BoundSurf(3) = RoofGeo%XuYuZu%Vertex
       RoofGeo%BoundSurf(4) = RoofGeo%XdYuZu%Vertex
  ELSEIF (   ( SideDLength >= SideALength) &
       .AND. ( SideDLength >= SideCLength) &
       .AND. ( SideDLength >= SideBLength) ) THEN
       ! Side D: X Lo Hi -- uses XdYuZd, XdYuZu, XuYuZd, XuYuZu
       RoofGeo%BoundSurf(1) = RoofGeo%XdYuZd%Vertex
       RoofGeo%BoundSurf(2) = RoofGeo%XuYuZd%Vertex
       RoofGeo%BoundSurf(3) = RoofGeo%XuYuZu%Vertex
       RoofGeo%BoundSurf(4) = RoofGeo%XdYuZu%Vertex
  ENDIF

  CALL CreateNewellAreaVector(RoofGeo%BoundSurf, 4, BoundNewellAreaVec)
  surfacearea=VecLength(BoundNewellAreaVec)
  IF (surfacearea > .001d0) THEN  ! Roof is not flat
    CALL CreateNewellSurfaceNormalVector(RoofGeo%BoundSurf, 4, BoundNewellVec)
    CALL DetermineAzimuthAndTilt(RoofGeo%BoundSurf, 4, BoundAzimuth, BoundTilt, dummy1, dummy2, dummy3,   &
       surfacearea, BoundNewellVec)
    RoofLongAxisOutwardAzimuth = BoundAzimuth
  ELSE
    RoofLongAxisOutwardAzimuth = 0.0d0  ! flat roofs don't really have azimuth
  ENDIF

  DO SurfLoop=1, TotSurfaces
    IF (Surface(SurfLoop)%ExtBoundCond /= ExternalEnvironment) CYCLE
    IF (.NOT. Surface(SurfLoop)%HeatTransSurf) CYCLE
    thisAzimuth = Surface(SurfLoop)%Azimuth
    IF ((Surface(SurfLoop)%Tilt >= 45.d0) .AND. (Surface(SurfLoop)%Tilt < 135.d0) ) THEN
     !treat as vertical wall
      IF ((thisAzimuth >= NorthFacade%AzimuthRangeLow) .OR. &
          (thisAzimuth < NorthFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(NorthFacade%Area     ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(NorthFacade%Perimeter,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(NorthFacade%Height , &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= NorthEastFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < NorthEastFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(NorthEastFacade%Area     ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(NorthEastFacade%Perimeter,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(NorthEastFacade%Height , &
                                                 (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= EastFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < EastFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(EastFacade%Area     ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(EastFacade%Perimeter,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(EastFacade%Height, &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= SouthEastFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < SouthEastFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(SouthEastFacade%Area     ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(SouthEastFacade%Perimeter,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(SouthEastFacade%Height, &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= SouthFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < SouthFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(SouthFacade%Area     ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(SouthFacade%Perimeter,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(SouthFacade%Height, &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= SouthWestFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < SouthWestFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(SouthWestFacade%Area     ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(SouthWestFacade%Perimeter,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(SouthWestFacade%Height, &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= WestFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < WestFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(WestFacade%Area      ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(WestFacade%Perimeter ,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(WestFacade%Height, &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ELSEIF ((thisAzimuth >= NorthWestFacade%AzimuthRangeLow) .AND. &
              (thisAzimuth < NorthWestFacade%AzimuthRangeHi)) THEN
        Surface(SurfLoop)%OutConvFaceArea      = MAX(NorthWestFacade%Area      ,Surface(SurfLoop)%GrossArea)
        Surface(SurfLoop)%OutConvFacePerimeter = MAX(NorthWestFacade%Perimeter ,Surface(SurfLoop)%Perimeter)
        Surface(SurfLoop)%OutConvFaceHeight    = MAX(NorthWestFacade%Height , &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
      ENDIF
    ELSEIF (Surface(SurfLoop)%Tilt < 45.0d0) THEN ! assume part of roof
      Surface(SurfLoop)%OutConvFaceArea      = MAX(RoofGeo%Area ,Surface(SurfLoop)%GrossArea)
      Surface(SurfLoop)%OutConvFacePerimeter = MAX(RoofGeo%Perimeter ,Surface(SurfLoop)%Perimeter)
      Surface(SurfLoop)%OutConvFaceHeight    = MAX(RoofGeo%Height, &
                                                   (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) ) )
    ELSEIF (Surface(SurfLoop)%Tilt >= 135.0d0) THEN !assume floor over exterior, just use surface's geometry
      Surface(SurfLoop)%OutConvFaceArea      = Surface(SurfLoop)%GrossArea
      Surface(SurfLoop)%OutConvFacePerimeter = Surface(SurfLoop)%Perimeter
      Surface(SurfLoop)%OutConvFaceHeight    = (MAXVAL(Surface(SurfLoop)%Vertex%Z) - MINVAL(Surface(SurfLoop)%Vertex%Z) )
    ENDIF
  ENDDO ! second pass thru surfs for outside face convection params.


  ! now send to EIO if surface reporting selected
  CALL ScanForReports('Surfaces',DoReport,'Details')
  IF (DoReport) THEN ! echo out static geometry data related to convection models

    Write(OutputFileInits, 900)     !header
    DO SurfLoop=1, TotSurfaces
      IF (.NOT. Surface(SurfLoop)%HeatTransSurf) cycle
      IF (Surface(SurfLoop)%IntConvSurfGetsRadiantHeat) THEN
        YesNo1='Yes'
      ELSE
        YesNo1='No'
      ENDIF
      IF (Surface(SurfLoop)%IntConvSurfHasActiveInIt) THEN
        YesNo2='Yes'
      ELSE
        YesNo2='No'
      ENDIF
      Write(OutputFileInits, 901) &
       TRIM( Surface(SurfLoop)%Name ) , &
       TRIM( RoundSigDigits(Surface(SurfLoop)%ExtConvCoeff)) , &
       TRIM( RoundSigDigits(Surface(SurfLoop)%OutConvFaceArea, 2)), &
       TRIM( RoundSigDigits(Surface(SurfLoop)%OutConvFacePerimeter, 2)), &
       TRIM( RoundSigDigits(Surface(SurfLoop)%OutConvFaceHeight, 2)), &
       TRIM( RoundSigDigits(Surface(SurfLoop)%IntConvCoeff)) , &
       TRIM( RoundSigDigits(Surface(SurfLoop)%IntConvZoneWallHeight, 2)), &
       TRIM( RoundSigDigits(Surface(SurfLoop)%IntConvZonePerimLength , 2)), & ! [m] length of perimeter zone's exterior wall
       TRIM( RoundSigDigits(Surface(SurfLoop)%IntConvZoneHorizHydrDiam, 2)), & ! [m] hydraulic diameter, usually 4 times the zone floor area div by perimeter
       TRIM( RoundSigDigits(Surface(SurfLoop)%IntConvWindowWallRatio, 2)), & ! [-] area of windows over area of exterior wall for zone
       TRIM( RoundSigDigits(Surface(SurfLoop)%IntConvWindowLocation )), & ! relative location of window in zone for interior Hc models
       TRIM(YesNo1),TRIM(YesNo2)

    ENDDO

    !if display advanced reports also dump meta group data used for convection geometry
    IF (DisplayAdvancedReportVariables) THEN
      WRITE (OutputFileInits, 8000) !header for north facade
      WRITE (OutputFileInits, 8001)  &
          TRIM( RoundSigDigits( NorthFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( NorthFacade%Height   , 2)), &
          TRIM( RoundSigDigits( NorthFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( NorthFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( NorthFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( NorthFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( NorthFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( NorthFacade%Zmax   , 2))
      WRITE (OutputFileInits, 8100) !header for northeast facade
      WRITE (OutputFileInits, 8101)  &
          TRIM( RoundSigDigits( NorthEastFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Height   , 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( NorthEastFacade%Zmax   , 2))
      WRITE (OutputFileInits, 8200) !header for east facade
      WRITE (OutputFileInits, 8201)  &
          TRIM( RoundSigDigits( EastFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( EastFacade%Height   , 2)), &
          TRIM( RoundSigDigits( EastFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( EastFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( EastFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( EastFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( EastFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( EastFacade%Zmax   , 2))

      WRITE (OutputFileInits, 8300) !header for southeast facade
      WRITE (OutputFileInits, 8301)  &
          TRIM( RoundSigDigits( SouthEastFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Height   , 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( SouthEastFacade%Zmax   , 2))

      WRITE (OutputFileInits, 8400) !header for south facade
      WRITE (OutputFileInits, 8401)  &
          TRIM( RoundSigDigits( SouthFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( SouthFacade%Height   , 2)), &
          TRIM( RoundSigDigits( SouthFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( SouthFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( SouthFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( SouthFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( SouthFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( SouthFacade%Zmax   , 2))
      WRITE (OutputFileInits, 8500) !header for southwest facade
      WRITE (OutputFileInits, 8501)  &
          TRIM( RoundSigDigits( SouthWestFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Height   , 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( SouthWestFacade%Zmax   , 2))
      WRITE (OutputFileInits, 8600) !header for west facade
      WRITE (OutputFileInits, 8601)  &
          TRIM( RoundSigDigits( WestFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( WestFacade%Height   , 2)), &
          TRIM( RoundSigDigits( WestFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( WestFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( WestFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( WestFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( WestFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( WestFacade%Zmax   , 2))
      WRITE (OutputFileInits, 8700) !header for northwest facade
      WRITE (OutputFileInits, 8701)  &
          TRIM( RoundSigDigits( NorthWestFacade%Perimeter, 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Height   , 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Xmin   , 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Xmax   , 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Ymin   , 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Ymax   , 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Zmin   , 2)), &
          TRIM( RoundSigDigits( NorthWestFacade%Zmax   , 2))
      WRITE (OutputFileInits, 8800) ! header for roof
      WRITE (OutputFileInits, 8801, advance='No')  &
          TRIM( RoundSigDigits( RoofGeo%Area, 2)), &
          TRIM( RoundSigDigits( RoofGeo%Perimeter, 2)), &
          TRIM( RoundSigDigits( RoofGeo%Height, 2)), &
          TRIM( RoundSigDigits( RoofGeo%XdYdZd%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYdZd%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYdZd%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYdZu%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYdZu%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYdZu%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYuZd%Vertex%X, 3))
      WRITE (OutputFileInits, 88012, advance='No')  &
          TRIM( RoundSigDigits( RoofGeo%XdYuZd%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYuZd%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYuZu%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYuZu%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XdYuZu%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYdZd%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYdZd%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYdZd%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYuZd%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYuZd%Vertex%Y, 3))
      WRITE (OutputFileInits, 88013)  &
          TRIM( RoundSigDigits( RoofGeo%XuYuZd%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYdZu%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYdZu%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYdZu%Vertex%Z, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYuZu%Vertex%X, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYuZu%Vertex%Y, 3)), &
          TRIM( RoundSigDigits( RoofGeo%XuYuZu%Vertex%Z, 3))

    ENDIF


  ENDIF

  900 format('! <Surface Convection Parameters>, Surface Name, Outside Model Assignment, Outside Area [m2], ', &
             'Outside Perimeter [m], Outside Height [m], Inside Model Assignment, ', &
             'Inside Height [cm], Inside Perimeter Envelope [m], Inside Hydraulic Diameter [m], Window Wall Ratio [ ], ', &
             'Window Location [ ], Near Radiant [Yes/No], Has Active HVAC [Yes/No]')

  901 format('Surface Convection Parameters,',A,',',A,',',A,',',A,',',A,',',A,',',A,',' &
                 ,A,',',A,',',A,',',A,',',A,',',A )

  8000 format('! <Building Convection Parameters:North Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8001 format('Building Convection Parameters:North Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)

  8100 format('! <Building Convection Parameters:Northeast Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8101 format('Building Convection Parameters:Northeast Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)

  8200 format('! <Building Convection Parameters:East Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8201 format('Building Convection Parameters:East Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)

  8300 format('! <Building Convection Parameters:Southeast Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8301 format('Building Convection Parameters:Southeast Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)
  8400 format('! <Building Convection Parameters:South Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8401 format('Building Convection Parameters:South Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)
  8500 format('! <Building Convection Parameters:Southwest Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8501 format('Building Convection Parameters:Southwest Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)
  8600 format('! <Building Convection Parameters:West Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8601 format('Building Convection Parameters:West Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)
  8700 format('! <Building Convection Parameters:Northwest Facade>, Perimeter, Height, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax ')
  8701 format('Building Convection Parameters:NorthwWest Facade, ',A, ',' , A,',', A,',',A,',', A,',', A,',', A ,',',A)

  8800 format('! <Building Convection Parameters:Roof>, Area [m2], Perimeter [m], Height [m], ', &
                               'XdYdZd:X, XdYdZd:Y, XdYdZd:Z' , & ! 1 low x, low y, low z
                               ',XdYdZu:X, XdYdZu:Y, XdYdZu:Z', & ! 2 low x, low y, hi z
                               ',XdYuZd:X, XdYuZd:Y, XdYuZd:Z', & ! 3 low x, hi y, low z
                               ',XdYuZu:X, XdYuZu:Y, XdYuZu:Z', & ! 4 low x, hi y, hi z
                               ',XuYdZd:X, XuYdZd:Y, XuYdZd:Z', & ! 5 hi x, low y, low z
                               ',XuYuZd:X, XuYuZd:Y, XuYuZd:Z', & ! 6 hi x, hi y, low z
                               ',XuYdZu:X, XuYdZu:Y, XuYdZu:Z', & ! 7 hi x, low y, hi z
                               ',XuYuZu:X, XuYuZu:Y, XuYuZu:Z' ) ! 8 hi x, hi y, hi z
  8801 format('Building Convection Parameters:Roof,',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',' )
  88012 format(A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',' )
  88013 format(A,',',A,',',A,',',A,',',A,',',A,',',A)


  ConvectionGeometryMetaDataSetup = .TRUE.

  RETURN

END SUBROUTINE SetupAdaptiveConvectionStaticMetaData

SUBROUTINE SetupAdaptiveConvectionRadiantSurfaceData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! identify Zones that have active radiant elements for convection model classifications

          ! METHODOLOGY EMPLOYED:
          ! Need to fill in values for ZoneEquipConfig%InWallActiveElement, ZoneEquipConfig%InCeilingActiveElement
          ! and ZoneEquipConfig(ZoneNum)%InFloorActiveElement.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment

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
  INTEGER   :: ActiveWallCount    = 0
  REAL(r64) :: ActiveWallArea     = 0.d0
  INTEGER   :: ActiveCeilingCount = 0
  Real(r64) :: ActiveCeilingArea  = 0.d0
  INTEGER   :: ActiveFloorCount   = 0
  REAL(r64) :: ActiveFloorArea    = 0.d0
  INTEGER   :: ZoneLoop
  INTEGER   :: SurfLoop

  DO ZoneLoop=1, NumOfZones
    ActiveWallCount    = 0
    ActiveWallArea     = 0.d0
    ActiveCeilingCount = 0
    ActiveCeilingArea  = 0.d0
    ActiveFloorCount   = 0
    ActiveFloorArea    = 0.d0

    DO SurfLoop = Zone(ZoneLoop)%SurfaceFirst, Zone(ZoneLoop)%SurfaceLast
      IF (.NOT. Surface(SurfLoop)%IntConvSurfHasActiveInIt) CYCLE
      IF (Surface(SurfLoop)%Class == SurfaceClass_Wall  &
          .OR. Surface(SurfLoop)%Class == SurfaceClass_Door ) THEN
        ActiveWallCount = ActiveWallCount + 1
        ActiveWallArea  = ActiveWallArea + Surface(SurfLoop)%Area
      ELSEIF (Surface(SurfLoop)%Class == SurfaceClass_Roof) THEN
        ActiveCeilingCount = ActiveCeilingCount + 1
        ActiveCeilingArea  = ActiveCeilingArea + Surface(SurfLoop)%Area
      ELSEIF (Surface(SurfLoop)%Class == SurfaceClass_Floor) THEN
        ActiveFloorCount   = ActiveFloorCount + 1
        ActiveFloorArea    = ActiveFloorArea + Surface(SurfLoop)%Area
      ENDIF
    ENDDO ! surface loop

    IF ((ActiveWallCount > 0) .AND. (ActiveWallArea > 0.d0 )) THEN
      ZoneEquipConfig(ZoneLoop)%InWallActiveElement    = .TRUE.
    ENDIF
    IF ((ActiveCeilingCount > 0) .AND. (ActiveCeilingArea > 0.d0)) THEN
      ZoneEquipConfig(ZoneLoop)%InCeilingActiveElement = .TRUE.
    ENDIF
    IF (( ActiveFloorCount > 0) .AND. (ActiveFloorArea > 0)) THEN
      ZoneEquipConfig(ZoneLoop)%InFloorActiveElement   = .TRUE.
    ENDIF
  ENDDO ! zone loop

  RETURN

END SUBROUTINE SetupAdaptiveConvectionRadiantSurfaceData


SUBROUTINE ManageInsideAdaptiveConvectionAlgo(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the calculation of interior convection coefficient for a surface.

          ! METHODOLOGY EMPLOYED:
          ! This routine implements the Adaptive Convection Algorithm developed by IB-M 2000 and IB-M 2002
          !  - first calls a large routine, DynamicIntConvSurfaceClassification, that has most of the complex logic
          !  - then calls a straightforward routine that maps the classification to model equation
          !  - then calls a routine with a large case statement that calls model equations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: SurfNum ! surface number for which coefficients are being calculated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  ! this next call sets up the flow regime and assigns a classification to surface
  !  TODO: candidate for rework to do zone level calcs once rather than for each surface
  CALL DynamicIntConvSurfaceClassification(SurfNum)

  ! simple worker routine takes surface classification and fills in model to use (IntConvHcModelEq) for that surface
  CALL MapIntConvClassificationToHcModels(SurfNum)

  CALL EvaluateIntHcModels(SurfNum, Surface(SurfNum)%IntConvHcModelEq, HconvIn(SurfNum))


  RETURN

END SUBROUTINE ManageInsideAdaptiveConvectionAlgo

SUBROUTINE ManageOutsideAdaptiveConvectionAlgo(SurfNum, Hc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the convection coefficient for the outside face of a surface

          ! METHODOLOGY EMPLOYED:
          ! This routine implements an adpative struture and classification system for outdoor
          !   It calls a series of separable worker routines

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: SurfNum ! surface number for which coefficients are being calculated
  REAL(r64), INTENT(OUT)  :: Hc  !  result for Hc Outside face, becomes HExt.


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  CALL DynamicExtConvSurfaceClassification(SurfNum)

  CALL MapExtConvClassificationToHcModels(SurfNum)

  CALL EvaluateExtHcModels(SurfNum, Surface(SurfNum)%OutConvHnModelEq, &
                                    Surface(SurfNum)%OutConvHfModelEq, Hc)


  RETURN

END SUBROUTINE ManageOutsideAdaptiveConvectionAlgo

SUBROUTINE EvaluateIntHcModels(SurfNum, ConvModelEquationNum, Hc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! central case statement for calling inside convection models

          ! METHODOLOGY EMPLOYED:
          !  - fills value for Hc by calling the appropriate convection model, usually as a function.
          !     preperation of argument values for the function calls is contained in each Case block (repeats)
          !  - also updates the reference air temperature type for use in the surface heat balance calcs



          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalSurface, ONLY: TH, QdotConvInRepPerArea
  USE DataHeatBalFanSys,  ONLY: MAT
  USE DataZoneEquipment
  USE DataEnvironment,    ONLY: OutBaroPress
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW, PsyWFnTdpPb

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: SurfNum
  INTEGER,   INTENT(IN)  :: ConvModelEquationNum
  REAL(R64), INTENT(OUT) :: Hc  ! calculated Hc value
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64)    :: SupplyAirTemp
  REAL(r64)    :: AirChangeRate
  INTEGER      :: ZoneNum  ! zone associated with inside face of surface
  INTEGER      :: ZoneNode ! the system node for the zone, node index
  INTEGER      :: EquipNum
  REAL(r64)    :: SumMdotTemp
  REAL(r64)    :: SumMdot
  REAL(r64)    :: AirDensity
  REAL(r64)    :: AirSystemVolFlowRate
  INTEGER      :: thisZoneInletNode
  REAL(r64)    :: tmpHc
  REAL(r64)    :: ZoneMult  ! local product of zone multiplier and zonelist multipler
  tmpHc = 0.d0
!now call appropriate function to calculate Hc
  SELECT CASE (ConvModelEquationNum)

  CASE(HcInt_UserCurve)
    CALL CalcUserDefinedInsideHcModel(SurfNum, Surface(SurfNum)%IntConvHcUserCurveIndex, tmpHc )
  CASE(HcInt_ASHRAEVerticalWall)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcASHRAEVerticalWall((TH(SurfNum,1,2) - MAT(ZoneNum)))
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_WaltonUnstableHorizontalOrTilt)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcWaltonUnstableHorizontalOrTilt((TH(SurfNum,1,2) - MAT(ZoneNum)), &
                                              Surface(SurfNum)%CosTilt ) !TODO verify CosTilt in vs out
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_WaltonStableHorizontalOrTilt)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcWaltonStableHorizontalOrTilt((TH(SurfNum,1,2) - MAT(ZoneNum)), &
                                                         Surface(SurfNum)%CosTilt ) !TODO verify CosTilt in vs out
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_FisherPedersenCeilDiffuserFloor)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      AirChangeRate = MIN(AirChangeRate, MaxACH)
      AirChangeRate = MAX(AirChangeRate, 0.0d0)
      Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
    ELSE
      AirChangeRate = 0.d0
      Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
    ENDIF
    tmpHc = CalcFisherPedersenCeilDiffuserFloor(AirChangeRate)
    
  CASE(HcInt_FisherPedersenCeilDiffuserCeiling)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0 ) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      AirChangeRate = MIN(AirChangeRate, MaxACH)
      AirChangeRate = MAX(AirChangeRate, 0.0d0)
      Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
    ELSE
      AirChangeRate = 0.d0
      Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
    ENDIF
    tmpHc = CalcFisherPedersenCeilDiffuserCeiling(AirChangeRate)

  CASE(HcInt_FisherPedersenCeilDiffuserWalls)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      AirChangeRate = MIN(AirChangeRate, MaxACH)
      AirChangeRate = MAX(AirChangeRate, 0.0d0)
      Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
    ELSE
      AirChangeRate = 0.d0
      Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
    ENDIF
    tmpHc = CalcFisherPedersenCeilDiffuserWalls(AirChangeRate)

  CASE(HcInt_AlamdariHammondStableHorizontal)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcAlamdariHammondStableHorizontal((TH(SurfNum,1,2) - MAT(ZoneNum)), &
                                                 Surface(SurfNum)%IntConvZoneHorizHydrDiam, SurfNum )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_AlamdariHammondVerticalWall)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcAlamdariHammondVerticalWall((TH(SurfNum,1,2) - MAT(ZoneNum)), &
                                             Surface(SurfNum)%IntConvZoneWallHeight, SurfNum )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_AlamdariHammondUnstableHorizontal)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcAlamdariHammondUnstableHorizontal((TH(SurfNum,1,2) - MAT(ZoneNum)), &
                                                              Surface(SurfNum)%IntConvZoneHorizHydrDiam , SurfNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_KhalifaEq3WallAwayFromHeat)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcKhalifaEq3WallAwayFromHeat((TH(SurfNum,1,2) - MAT(ZoneNum)) )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_KhalifaEq4CeilingAwayFromHeat)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcKhalifaEq4CeilingAwayFromHeat((TH(SurfNum,1,2) - MAT(ZoneNum)) )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_KhalifaEq5WallNearHeat)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcKhalifaEq5WallsNearHeat((TH(SurfNum,1,2) - MAT(ZoneNum)) )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_KhalifaEq6NonHeatedWalls)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcKhalifaEq6NonHeatedWalls((TH(SurfNum,1,2) - MAT(ZoneNum)) )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_KhalifaEq7Ceiling)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcKhalifaEq7Ceiling((TH(SurfNum,1,2) - MAT(ZoneNum)) )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_AwbiHattonHeatedFloor)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcAwbiHattonHeatedFloor((TH(SurfNum,1,2) - MAT(ZoneNum)),  &
                                                  Surface(SurfNum)%IntConvZoneHorizHydrDiam )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_AwbiHattonHeatedWall)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcAwbiHattonHeatedWall((TH(SurfNum,1,2) - MAT(ZoneNum)),  &
                                                  Surface(SurfNum)%IntConvZoneHorizHydrDiam )
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_BeausoleilMorrisonMixedAssistingWall)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0 ) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      SumMdotTemp = 0.d0
      SumMdot     = 0.d0
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%NumOutlets > 0) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
            SumMdot     = SumMdot     + Node(thisZoneInletNode)%MassFlowRate
          ENDIF
        ENDIF
      ENDDO
      IF (SumMdot > 0.d0) THEN
        SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
      ELSE
        IF (thisZoneInletNode > 0) THEN
          SupplyAirTemp = Node(thisZoneInletNode)%Temp
        ELSE
          SupplyAirTemp = Node(ZoneNode)%Temp
        ENDIF
      ENDIF
    ELSE
      AirChangeRate = 0.d0
      SupplyAirTemp = Node(ZoneNode)%Temp
    ENDIF
    tmpHc = CalcBeausoleilMorrisonMixedAssistedWall((TH(SurfNum,1,2) - MAT(ZoneNum)),       &
                                                               Surface(SurfNum)%IntConvZoneWallHeight, &
                                                               TH(SurfNum,1,2),   &
                                                               SupplyAirTemp,     &
                                                               AirChangeRate, ZoneNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_BeausoleilMorrisonMixedOppossingWall)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume* ZoneMult)
      SumMdotTemp = 0.d0
      SumMdot     = 0.d0
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%NumOutlets > 0) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
            SumMdot     = SumMdot     + Node(thisZoneInletNode)%MassFlowRate
          ENDIF
        ENDIF
      ENDDO
      IF (SumMdot > 0.d0) THEN
        SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
      ELSE
        IF (thisZoneInletNode > 0) THEN
          SupplyAirTemp = Node(thisZoneInletNode)%Temp
        ELSE
          SupplyAirTemp = Node(ZoneNode)%Temp
        ENDIF
      ENDIF
    ELSE
      AirChangeRate = 0.d0
      SupplyAirTemp = Node(ZoneNode)%Temp
    ENDIF

    tmpHc = CalcBeausoleilMorrisonMixedOpposingWall((TH(SurfNum,1,2) - MAT(ZoneNum)),       &
                                                               Surface(SurfNum)%IntConvZoneWallHeight, &
                                                               TH(SurfNum,1,2),   &
                                                               SupplyAirTemp,     &
                                                               AirChangeRate, ZoneNum)

    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_BeausoleilMorrisonMixedStableCeiling)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      SumMdotTemp = 0.d0
      SumMdot     = 0.d0
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%NumOutlets > 0) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
            SumMdot     = SumMdot     + Node(thisZoneInletNode)%MassFlowRate
          ENDIF
        ENDIF
      ENDDO
      IF (SumMdot > 0.d0) THEN
        SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
      ELSE
        IF (thisZoneInletNode > 0) THEN
          SupplyAirTemp = Node(thisZoneInletNode)%Temp
        ELSE
          SupplyAirTemp = Node(ZoneNode)%Temp
        ENDIF
      ENDIF
    ELSE
      AirChangeRate = 0.d0
      SupplyAirTemp = Node(ZoneNode)%Temp
    ENDIF
    tmpHc = CalcBeausoleilMorrisonMixedStableCeiling((TH(SurfNum,1,2) - MAT(ZoneNum)),       &
                                                               Surface(SurfNum)%IntConvZoneHorizHydrDiam, &
                                                               TH(SurfNum,1,2),   &
                                                               SupplyAirTemp,     &
                                                               AirChangeRate, ZoneNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_BeausoleilMorrisonMixedUnstableCeiling)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      SumMdotTemp = 0.d0
      SumMdot     = 0.d0
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%NumOutlets > 0) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
            SumMdot     = SumMdot     + Node(thisZoneInletNode)%MassFlowRate
          ENDIF
        ENDIF
      ENDDO
      IF (SumMdot > 0.d0) THEN
        SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
      ELSE
        IF (thisZoneInletNode > 0) THEN
          SupplyAirTemp = Node(thisZoneInletNode)%Temp
        ELSE
          SupplyAirTemp = Node(ZoneNode)%Temp
        ENDIF
      ENDIF
    ELSE
      AirChangeRate = 0.d0
      SupplyAirTemp = Node(ZoneNode)%Temp
    ENDIF
    tmpHc = CalcBeausoleilMorrisonMixedUnstableCeiling((TH(SurfNum,1,2) - MAT(ZoneNum)),       &
                                                               Surface(SurfNum)%IntConvZoneHorizHydrDiam, &
                                                               TH(SurfNum,1,2),   &
                                                               SupplyAirTemp,     &
                                                               AirChangeRate, ZoneNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_BeausoleilMorrisonMixedStableFloor)
    ZoneNum  = Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume * ZoneMult)
      SumMdotTemp = 0.d0
      SumMdot     = 0.d0
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%NumOutlets > 0) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
            SumMdot     = SumMdot     + Node(thisZoneInletNode)%MassFlowRate
          ENDIF
        ENDIF
      ENDDO
      IF (SumMdot > 0.d0) THEN
        SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
      ELSE
        IF (thisZoneInletNode > 0) THEN
          SupplyAirTemp = Node(thisZoneInletNode)%Temp
        ELSE
          SupplyAirTemp = Node(ZoneNode)%Temp
        ENDIF
      ENDIF
    ELSE
      AirChangeRate = 0.d0
      SupplyAirTemp = Node(ZoneNode)%Temp
    ENDIF
    tmpHc = CalcBeausoleilMorrisonMixedStableFloor((TH(SurfNum,1,2) - MAT(ZoneNum)),       &
                                                               Surface(SurfNum)%IntConvZoneHorizHydrDiam, &
                                                               TH(SurfNum,1,2),   &
                                                               SupplyAirTemp,     &
                                                               AirChangeRate, ZoneNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_BeausoleilMorrisonMixedUnstableFloor)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0.d0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume  * ZoneMult)
      SumMdotTemp = 0.d0
      SumMdot     = 0.d0
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%NumOutlets > 0) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
            SumMdot     = SumMdot     + Node(thisZoneInletNode)%MassFlowRate
          ENDIF
        ENDIF
      ENDDO
      IF (SumMdot > 0.d0) THEN
        SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
      ELSE
        IF (thisZoneInletNode > 0) THEN
          SupplyAirTemp = Node(thisZoneInletNode)%Temp
        ELSE
          SupplyAirTemp = Node(ZoneNode)%Temp
        ENDIF
      ENDIF
    ELSE
      AirChangeRate = 0.d0
      SupplyAirTemp = Node(ZoneNode)%Temp
    ENDIF
    tmpHc = CalcBeausoleilMorrisonMixedUnstableFloor((TH(SurfNum,1,2) - MAT(ZoneNum)),       &
                                                               Surface(SurfNum)%IntConvZoneHorizHydrDiam, &
                                                               TH(SurfNum,1,2),   &
                                                               SupplyAirTemp,     &
                                                               AirChangeRate, ZoneNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_FohannoPolidoriVerticalWall)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcFohannoPolidoriVerticalWall((TH(SurfNum,1,2) - MAT(ZoneNum)), &
                                    Surface(SurfNum)%IntConvZoneWallHeight, &
                                    TH(SurfNum,1,2), &
                                    - QdotConvInRepPerArea(SurfNum), SurfNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_KaradagChilledCeiling)
    ZoneNum =  Surface(SurfNum)%Zone
    tmpHc = CalcKaradagChilledCeiling((TH(SurfNum,1,2) - MAT(ZoneNum)))
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_ISO15099Windows)
    ZoneNum =  Surface(SurfNum)%Zone
    CALL CalcISO15099WindowIntConvCoeff(SurfNum,TH(SurfNum,1,2),MAT(ZoneNum))
    tmpHc = HconvIn(SurfNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE(HcInt_GoldsteinNovoselacCeilingDiffuserWindow)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirSystemVolFlowRate = Node(ZoneNode)%MassFlowRate /(AirDensity * ZoneMult)
      Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
    ELSE
      AirSystemVolFlowRate = 0.d0
      Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
    ENDIF
    tmpHc = CalcGoldsteinNovoselacCeilingDiffuserWindow(AirSystemVolFlowRate, &
                             Surface(SurfNum)%IntConvZonePerimLength, &
                             Surface(SurfNum)%IntConvWindowWallRatio,  &
                             Surface(SurfNum)%IntConvWindowLocation, ZoneNum)

  CASE(HcInt_GoldsteinNovoselacCeilingDiffuserWalls)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirSystemVolFlowRate = Node(ZoneNode)%MassFlowRate /(AirDensity * ZoneMult)
      Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
    ELSE
      AirSystemVolFlowRate = 0.d0
      Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
    ENDIF
    tmpHc = CalcGoldsteinNovoselacCeilingDiffuserWall(AirSystemVolFlowRate, &
                            Surface(SurfNum)%IntConvZonePerimLength, &
                            Surface(SurfNum)%IntConvWindowLocation, ZoneNum )

  CASE(HcInt_GoldsteinNovoselacCeilingDiffuserFloor)
    ZoneNum =  Surface(SurfNum)%Zone
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    IF (ZoneNode > 0) THEN
      ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      AirSystemVolFlowRate = Node(ZoneNode)%MassFlowRate / (AirDensity * ZoneMult)
      Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
    ELSE
      AirSystemVolFlowRate = 0.d0
      Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
    ENDIF
    tmpHc = CalcGoldsteinNovoselacCeilingDiffuserFloor(AirSystemVolFlowRate, &
                            Surface(SurfNum)%IntConvZonePerimLength, ZoneNum )

  END SELECT

  IF (tmpHc < AdaptiveHcInsideLowLimit) tmpHc = AdaptiveHcInsideLowLimit

  Hc = tmpHc

  RETURN

END SUBROUTINE EvaluateIntHcModels


SUBROUTINE EvaluateExtHcModels(SurfNum, NaturalConvModelEqNum, ForcedConvModelEqNum, Hc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Grifith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! central case statement for evaluating exterior specific convection models

          ! METHODOLOGY EMPLOYED:
          ! separated out long case statement for selecting models.


          ! REFERENCES:
          !

          ! USE STATEMENTS:
  USE DataHeatBalSurface, ONLY: TH, QdotConvOutRepPerArea
  USE DataEnvironment   , ONLY: WindSpeed, WindDir

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfNum
  INTEGER, INTENT(IN) :: NaturalConvModelEqNum
  INTEGER, INTENT(IN) :: ForcedConvModelEqNum
  REAL(r64), INTENT(OUT) :: Hc

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(R64) :: Hf = 0.d0 ! the forced, or wind driven portion of film coefficient
  REAL(R64) :: Hn = 0.d0 ! the natural, or bouyancy driven portion of film coefficient
  INTEGER   :: ConstructNum
  REAL(r64) :: SurfWindSpeed
  REAL(r64) :: HydraulicDiameter

  ! first call Hn models
  SELECT CASE (NaturalConvModelEqNum)

  CASE (HcExt_None)
    Hn = 0.d0
  CASE (HcExt_UserCurve)

   CALL CalcUserDefinedOutsideHcModel(SurfNum, Surface(SurfNum)%OutConvHnUserCurveIndex, Hn)

  CASE (HcExt_NaturalASHRAEVerticalWall)
    Hn = CalcASHRAEVerticalWall( (TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp) )
  CASE (HcExt_NaturalWaltonUnstableHorizontalOrTilt)
    Hn = CalcWaltonUnstableHorizontalOrTilt( (TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp) , &
                                             Surface(SurfNum)%CosTilt ) !TODO verify CosTilt in vs out
  CASE (HcExt_NaturalWaltonStableHorizontalOrTilt)
    Hn = CalcWaltonStableHorizontalOrTilt( (TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp) , &
                                             Surface(SurfNum)%CosTilt ) !TODO verify CosTilt in vs out
  CASE (HcExt_AlamdariHammondVerticalWall)

    Hn = CalcAlamdariHammondVerticalWall( (TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp) , &
                                          Surface(SurfNum)%OutConvFaceHeight , SurfNum)
  CASE (HcExt_FohannoPolidoriVerticalWall)
    Hn = CalcFohannoPolidoriVerticalWall( (TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp) , &
                                          Surface(SurfNum)%OutConvFaceHeight , &
                                          TH(SurfNum, 1, 1), &
                                          -QdotConvOutRepPerArea(SurfNum) , SurfNum)
!  CASE (HcExt_ISO15099Windows)

  CASE (HcExt_AlamdariHammondStableHorizontal)
    IF (Surface(SurfNum)%OutConvFacePerimeter > 0.d0) THEN
      HydraulicDiameter = 4.d0 * Surface(SurfNum)%OutConvFaceArea / Surface(SurfNum)%OutConvFacePerimeter
    ELSE
      HydraulicDiameter = SQRT(Surface(SurfNum)%OutConvFaceArea)
    ENDIF
    Hn = CalcAlamdariHammondStableHorizontal((TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp), &
                                               HydraulicDiameter, SurfNum )
  CASE (HcExt_AlamdariHammondUnstableHorizontal)
    IF (Surface(SurfNum)%OutConvFacePerimeter > 0.d0) THEN
      HydraulicDiameter = 4.d0 * Surface(SurfNum)%OutConvFaceArea / Surface(SurfNum)%OutConvFacePerimeter
    ELSE
      HydraulicDiameter = SQRT(Surface(SurfNum)%OutConvFaceArea)
    ENDIF
    Hn = CalcAlamdariHammondUnstableHorizontal((TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp), &
                                               HydraulicDiameter, SurfNum )
  END SELECT


  SELECT CASE (ForcedConvModelEqNum)

  CASE (HcExt_None)
    Hf = 0.d0
  CASE (HcExt_UserCurve)
   Call CalcUserDefinedOutsideHcModel(SurfNum, Surface(SurfNum)%OutConvHfUserCurveIndex, Hf)
  CASE (HcExt_SparrowWindward)
    ConstructNum = Surface(SurfNum)%Construction
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcSparrowWindward(Material(Construct(ConstructNum)%LayerPoint(1))%Roughness, &
                             Surface(SurfNum)%OutConvFacePerimeter, &
                             Surface(SurfNum)%OutConvFaceArea, &
                             SurfWindSpeed , SurfNum)

  CASE (HcExt_SparrowLeeward)
    ConstructNum = Surface(SurfNum)%Construction
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcSparrowLeeward(Material(Construct(ConstructNum)%LayerPoint(1))%Roughness, &
                             Surface(SurfNum)%OutConvFacePerimeter, &
                             Surface(SurfNum)%OutConvFaceArea, &
                             SurfWindSpeed, SurfNum )
  CASE (HcExt_MoWiTTWindward)
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcMoWITTWindward( TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp, SurfWindSpeed)
  CASE (HcExt_MoWiTTLeeward)
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcMoWITTLeeward( (TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp), SurfWindSpeed)
  CASE (HcExt_DOE2Windward)
    ConstructNum = Surface(SurfNum)%Construction
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcDOE2Windward(TH(SurfNum, 1, 1), Surface(SurfNum)%OutDryBulbTemp, &
                          Surface(SurfNum)%CosTilt, SurfWindSpeed, &
                          Material(Construct(ConstructNum)%LayerPoint(1))%Roughness)
  CASE (HcExt_DOE2Leeward)
    ConstructNum = Surface(SurfNum)%Construction
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcDOE2Leeward(TH(SurfNum, 1, 1), Surface(SurfNum)%OutDryBulbTemp, &
                          Surface(SurfNum)%CosTilt, SurfWindSpeed, &
                          Material(Construct(ConstructNum)%LayerPoint(1))%Roughness)
  CASE (HcExt_NusseltJurges)
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcNusseltJurges(SurfWindSpeed)

  CASE (HcExt_McAdams)
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcMcAdams(SurfWindSpeed)
  CASE (HcExt_Mitchell)
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcMitchell(SurfWindSpeed,CubeRootOfOverallBuildingVolume, SurfNum)

  CASE (HcExt_ClearRoof)
    IF (.NOT. Surface(SurfNum)%ExtWind) THEN
      SurfWindSpeed = 0.d0  ! No wind exposure
    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_Window .AND. SurfaceWindow(SurfNum)%ShadingFlag == ExtShadeOn) THEN
      SurfWindSpeed = 0.d0  ! Assume zero wind speed at outside glass surface of window with exterior shade
    ELSE
      SurfWindSpeed = Surface(SurfNum)%WindSpeed
    ENDIF
    Hf = CalcClearRoof(SurfNum,TH(SurfNum, 1, 1), Surface(SurfNum)%OutDryBulbTemp, &
                       SurfWindSpeed , WindDir , Surface(SurfNum)%OutConvFaceArea, &
                       Surface(SurfNum)%OutConvFacePerimeter)
  CASE (HcExt_BlockenWindward)
    Hf = CalcBlockenWindward(WindSpeed, WindDir, Surface(SurfNum)%Azimuth)
  CASE (HcExt_EmmelVertical)
    Hf = CalcEmmelVertical(WindSpeed, WindDir, Surface(SurfNum)%Azimuth, SurfNum)
  CASE (HcExt_EmmelRoof)

    Hf = CalcEmmelRoof(WindSpeed, WindDir, RoofLongAxisOutwardAzimuth, SurfNum)

  END SELECT

  Hc = Hf + Hn
  IF (Hc < AdaptiveHcOutsideLowLimit ) Hc = AdaptiveHcOutsideLowLimit

  RETURN

END SUBROUTINE EvaluateExtHcModels



SUBROUTINE DynamicExtConvSurfaceClassification(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! Decide surface classification based on wind and bouyancy, class, orientation

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalSurface, ONLY: TH
  USE DataEnvironment,    ONLY: WindDir

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: SurfNum ! surface number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DeltaTemp = 0.d0



  IF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
    DeltaTemp = TH(SurfNum, 1, 1) - Surface(SurfNum)%OutDryBulbTemp
    IF (DeltaTemp < 0.d0) THEN
      Surface(SurfNum)%OutConvClassification = OutConvClass_RoofStable
    ELSE
      Surface(SurfNum)%OutConvClassification = OutConvClass_RoofUnstable
    ENDIF

  ELSE

    IF (Windward(Surface(SurfNum)%CosTilt,Surface(SurfNum)%Azimuth, WindDir)) THEN
      Surface(SurfNum)%OutConvClassification = OutConvClass_WindwardVertWall
    ELSE
      Surface(SurfNum)%OutConvClassification = OutConvClass_LeewardVertWall
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE DynamicExtConvSurfaceClassification

SUBROUTINE MapExtConvClassificationToHcModels(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: SurfNum ! surface number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  SELECT CASE (Surface(SurfNum)%OutConvClassification)

  CASE (OutConvClass_WindwardVertWall)
    Surface(SurfNum)%OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo%HWindWallWindwardEqNum
    IF (Surface(SurfNum)%OutConvHfModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HWindWallWindwardUserCurveNum
    ENDIF
    Surface(SurfNum)%OutConvHnModelEq = OutsideFaceAdaptiveConvectionAlgo%HNatVertWallEqNum
    IF (Surface(SurfNum)%OutConvHnModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHnUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HNatVertWallUserCurveNum
    ENDIF
  CASE (OutConvClass_LeewardVertWall)
    Surface(SurfNum)%OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo%HWindWallLeewardEqNum
    IF (Surface(SurfNum)%OutConvHfModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HWindWallLeewardUserCurveNum
    ENDIF
    Surface(SurfNum)%OutConvHnModelEq =OutsideFaceAdaptiveConvectionAlgo%HNatVertWallEqNum
    IF (Surface(SurfNum)%OutConvHnModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HNatVertWallUserCurveNum
    ENDIF

  CASE (OutConvClass_RoofStable)
    Surface(SurfNum)%OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofEqNum
    IF (Surface(SurfNum)%OutConvHfModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofUserCurveNum
    ENDIF
    Surface(SurfNum)%OutConvHnModelEq =OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizEqNum
    IF (Surface(SurfNum)%OutConvHnModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HNatStableHorizUserCurveNum
    ENDIF
  CASE (OutConvClass_RoofUnstable)
    Surface(SurfNum)%OutConvHfModelEq = OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofEqNum
    IF (Surface(SurfNum)%OutConvHfModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HWindHorizRoofUserCurveNum
    ENDIF
    Surface(SurfNum)%OutConvHnModelEq =OutsideFaceAdaptiveConvectionAlgo%HNatUnstableHorizEqNum
    IF (Surface(SurfNum)%OutConvHnModelEq == HcExt_UserCurve) THEN
      Surface(SurfNum)%OutConvHfUserCurveIndex = OutsideFaceAdaptiveConvectionAlgo%HNatUstableHorizUserCurveNum
    ENDIF
  CASE DEFAULT
    CAll ShowSevereError('MapExtConvClassificationToHcModels: caught unknown outdoor surfce classification:'&
                        //TRIM(RoundSigDigits(Surface(SurfNum)%OutConvClassification)) )
  END SELECT

  RETURN

END SUBROUTINE MapExtConvClassificationToHcModels


SUBROUTINE DynamicIntConvSurfaceClassification(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR        Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! collects dynamic updates needed for adaptive convectin algorithm

          ! METHODOLOGY EMPLOYED:
          ! Decide flow regime to set IntConvClassification
          !  done by zone using the following rules
          !

          ! Using zone flow regime, and surface's characteristics assign IntConvHcModelEq

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment
  USE DataHeatBalSurface, ONLY: TH
  USE DataHeatBalFanSys,  ONLY: MAT
  USE DataEnvironment,    ONLY: OutBaroPress
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW, PsyWFnTdpPb

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: SurfNum ! surface number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: g  = 9.81d0       ! gravity constant (m/s**2)
  REAL(r64), PARAMETER :: v  = 15.89d-6     ! kinematic viscosity (m**2/s) for air at 300 K
  REAL(r64), PARAMETER :: ActiveDelTempThreshold = 1.5d0  ! deg C, temperature difference for surfaces to be considered "active"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneNum = 0
  INTEGER :: PriorityEquipOn = 0
  INTEGER, DIMENSION(0:10) :: HeatingPriorityStack = 0
  INTEGER, DIMENSION(0:10) :: CoolingPriorityStack = 0
  INTEGER, DIMENSION(0:10) :: FlowRegimeStack      = 0
  INTEGER :: EquipNum = 0
  INTEGER :: ZoneNode = 0
  INTEGER :: EquipOnCount = 0
  INTEGER :: EquipOnLoop = 0
  INTEGER :: thisZoneInletNode = 0
!  INTEGER :: thisZnEqInletNode = 0
  INTEGER :: FinalFlowRegime = 0
  REAL(r64) :: Tmin = 0.0d0 ! temporary min surf temp
  REAL(r64) :: Tmax = 0.0d0 ! temporary max surf temp
  REAL(r64) :: GrH = 0.d0 ! Grashof number for zone height H
  REAL(r64) :: Re = 0.d0 ! Reynolds number for zone air system flow
  REAL(r64) :: Ri = 0.d0 ! Richardson Number, Gr/Re**2 for determining mixed regime
  REAL(r64) :: AirDensity = 0.d0 ! temporary zone air density
  REAL(r64) :: DeltaTemp = 0.d0 ! temporary temperature difference (Tsurf - Tair)
  INTEGER   :: SurfLoop  ! local for separate looping across surfaces in the zone that has SurfNum

  EquipOnCount = 0
  ZoneNum  = Surface(SurfNum)%Zone
  ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
  FlowRegimeStack = 0

  !HVAC connections
  IF (.NOT. Zone(ZoneNum)%IsControlled) Then ! no HVAC control
    FlowRegimeStack(0) = InConvFlowRegime_A3
  ELSE ! is controlled, lets see by how and if that means is currently active

    IF (.NOT. (ZoneEquipConfig(ZoneNum)%EquipListIndex > 0) ) THEN
      FlowRegimeStack(0) = InConvFlowRegime_A3
    ELSE

      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes

        SELECT CASE (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipType_Num(EquipNum))

        CASE (AirDistUnit_Num,DirectAir_Num, PurchasedAir_Num) ! central air equipment
          IF (.NOT. (ALLOCATED( ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums))) CYCLE
          !get inlet node, not zone node if possible
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF (thisZoneInletNode > 0) THEN
            IF (Node(thisZoneInletNode)%MassFlowRate > 0.d0) THEN
              EquipOnCount = MIN(EquipOnCount + 1, 10)
              FlowRegimeStack(EquipOnCount) = InConvFlowRegime_C
              HeatingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
              CoolingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
            ENDIF
          ELSE
            IF (Node(ZoneNode)%MassFlowRate > 0.d0) THEN
              EquipOnCount = MIN(EquipOnCount + 1, 10)
              FlowRegimeStack(EquipOnCount) = InConvFlowRegime_C
              HeatingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
              CoolingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
            ENDIF
          ENDIF
        CASE (WindowAC_Num, PkgTermHPAirToAir_Num, PkgTermACAirToAir_Num, ZoneDXDehumidifier_Num, &
              PkgTermHPWaterToAir_Num, FanCoil4Pipe_Num, UnitVentilator_Num, UnitHeater_Num, &
              OutdoorAirUnit_Num)
          IF (.NOT. (ALLOCATED( ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums))) CYCLE
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF (thisZoneInletNode > 0) THEN
            IF (Node(thisZoneInletNode)%MassFlowRate > 0.d0) THEN
              EquipOnCount = MIN(EquipOnCount + 1, 10)
              FlowRegimeStack(EquipOnCount) = InConvFlowRegime_D
              HeatingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
              CoolingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
            ENDIF
          ELSE
            IF (Node(ZoneNode)%MassFlowRate > 0.d0) THEN
              EquipOnCount = MIN(EquipOnCount + 1, 10)
              FlowRegimeStack(EquipOnCount) = InConvFlowRegime_D
              HeatingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
              CoolingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
            ENDIF
          ENDIF
        CASE ( BBSteam_Num, BBWaterConvective_Num, BBElectricConvective_Num, BBWater_Num)

          IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%ON) THEN
            EquipOnCount = MIN(EquipOnCount + 1, 10)
            FlowRegimeStack(EquipOnCount) = InConvFlowRegime_B
            HeatingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
            CoolingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
          ENDIF
        CASE (BBElectric_Num, HiTempRadiant_Num)
          IF (ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%ON) THEN
            EquipOnCount = MIN(EquipOnCount + 1, 10)
            FlowRegimeStack(EquipOnCount) = InConvFlowRegime_B
            HeatingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
            CoolingPriorityStack(EquipOnCount) = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
          ENDIF
        CASE (VentilatedSlab_Num, LoTempRadiant_Num)

          IF (ZoneEquipConfig(ZoneNum)%InFloorActiveElement) THEN
            DO SurfLoop = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
              IF (.NOT. Surface(SurfLoop)%IntConvSurfHasActiveInIt) CYCLE
              IF (Surface(SurfLoop)%Class == SurfaceClass_Floor) THEN
                DeltaTemp = TH(SurfLoop,1,2) - MAT(ZoneNum)
                IF (DeltaTemp > ActiveDelTempThreshold) then ! assume heating with floor
                  ! system ON is not enough because floor surfaces can continue to heat because of thermal capacity
                  EquipOnCount = MIN(EquipOnCount + 1, 10)
                  FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A1
                  HeatingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
                  CoolingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
                  EXIT
                ENDIF
              ENDIF
            ENDDO
          ENDIF

          IF (ZoneEquipConfig(ZoneNum)%InCeilingActiveElement) THEN
            DO SurfLoop = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
              IF (.NOT. Surface(SurfLoop)%IntConvSurfHasActiveInIt) CYCLE
              IF (Surface(SurfLoop)%Class == SurfaceClass_Roof) THEN
                DeltaTemp = TH(SurfLoop,1,2) - MAT(ZoneNum)
                IF (DeltaTemp < ActiveDelTempThreshold) then ! assume cooling with ceiling
                  ! system ON is not enough because  surfaces can continue to cool because of thermal capacity
                  EquipOnCount = MIN(EquipOnCount + 1, 10)
                  FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A1
                  HeatingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
                  CoolingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
                  EXIT
                ENDIF
              ENDIF
            ENDDO
          ENDIF

          IF (ZoneEquipConfig(ZoneNum)%InWallActiveElement) THEN
            DO SurfLoop = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
              IF (.NOT. Surface(SurfLoop)%IntConvSurfHasActiveInIt) CYCLE
              IF (Surface(SurfLoop)%Class == SurfaceClass_Wall  &
                    .OR. Surface(SurfLoop)%Class == SurfaceClass_Door) THEN
                DeltaTemp = TH(SurfLoop,1,2) - MAT(ZoneNum)
                IF (DeltaTemp > ActiveDelTempThreshold) then ! assume heating with wall panel
                  ! system ON is not enough because  surfaces can continue to heat because of thermal capacity
                  EquipOnCount = MIN(EquipOnCount + 1, 10)
                  FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A2
                  HeatingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
                  CoolingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
                ELSE ! not heating, no special models wall cooling so use simple bouyancy
                  EquipOnCount = MIN(EquipOnCount + 1, 10)
                  FlowRegimeStack(EquipOnCount) = InConvFlowRegime_A3
                  HeatingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%HeatingPriority(EquipNum)
                  CoolingPriorityStack(EquipOnCount) &
                           = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%CoolingPriority(EquipNum)
                ENDIF
              ENDIF
            ENDDO
          ENDIF

        END SELECT
      ENDDO !loop over equipment for this zone
    ENDIF
  ENDIF

  ! now select which equipment type is dominant compared to all those that are ON
  IF (EquipOnCount > 0) THEN
    If (SNLoadPredictedRate(ZoneNum) >= 0.d0 ) Then ! heating load
      PriorityEquipOn = 1
      DO EquipOnLoop = 1, EquipOnCount
        !assume highest priority/first sim order is dominant for flow regime
        IF (HeatingPriorityStack(EquipOnLoop) < HeatingPriorityStack(PriorityEquipOn)) THEN
          PriorityEquipOn = EquipOnLoop
        ENDIF
      ENDDO
    ELSEIF (SNLoadPredictedRate(ZoneNum) < 0.d0) THEN ! cooling load
      PriorityEquipOn = 1
      DO EquipOnLoop = 1, EquipOnCount
        !assume highest priority/first sim order is dominant for flow regime
        IF (CoolingPriorityStack(EquipOnLoop) < CoolingPriorityStack(PriorityEquipOn)) THEN
          PriorityEquipOn = EquipOnLoop
        ENDIF
      ENDDO
    ENDIF
    FinalFlowRegime = FlowRegimeStack(PriorityEquipOn)
  ELSE
    ! no equipment on, so simple bouyancy flow regime
    FinalFlowRegime = InConvFlowRegime_A3
  ENDIF

  ! now if flow regimes C or D, then check for Mixed regime or very low flow rates
  IF ((FinalFlowRegime == InConvFlowRegime_C) .or. &
      (FinalFlowRegime == InConvFlowRegime_D) ) THEN

    !Calculate Grashof, Reynolds, and Richardson numbers for the zone
    !
    !Grashof for zone air based on largest delta T between surfaces and zone height
    Tmin = MINVAL(TH(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast,1, 2))
    Tmax = MAXVAL(TH(Zone(ZoneNum)%SurfaceFirst:Zone(ZoneNum)%SurfaceLast,1, 2))
    GrH  = (g * (Tmax - Tmin) * (Zone(ZoneNum)%CeilingHeight**3)) &
            /((MAT(ZoneNum) + KelvinConv) * (v**2))

    ! Reynolds number = Vdot supply / v * cube root of zone volume (Goldstein and Noveselac 2010)
    IF (Node(ZoneNode)%MassFlowRate > 0.d0) THEN
      AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
      Re = Node(ZoneNode)%MassFlowRate /(v *AirDensity *(Zone(ZoneNum)%Volume**OneThird))
    ELSE
      Re = 0.d0
    ENDIF

    IF (Re > 0.d0) THEN
      Ri = GrH/(Re**2)  !Richardson Number
      IF (Ri > 10.d0 ) Then ! natural convection expected
        FinalFlowRegime = InConvFlowRegime_A3
      ELSEIF (Ri < 0.1d0) THEN !forced
        ! no change, already a forced regime
      ELSE  ! mixed
        FinalFlowRegime = InConvFlowRegime_E
      ENDIF
    ELSE ! natural convection expected
      FinalFlowRegime = InConvFlowRegime_A3
    ENDIF
  ENDIF

  ! now finish out specific model eq for this surface
  !Surface(SurfNum)%IntConvClassification = 0 !init/check
  SELECT CASE (FinalFlowRegime)

  CASE (InConvFlowRegime_A1)
    DeltaTemp = TH(SurfNum,1,2) - MAT(ZoneNum)
    IF (Surface(SurfNum)%Class == SurfaceClass_Wall &
       .OR. Surface(SurfNum)%Class == SurfaceClass_Door ) THEN

      IF  ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN !vertical wall
        Surface(SurfNum)%IntConvClassification = InConvClass_A1_VertWalls
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableTilted
        ENDIF
      ELSEIF (Surface(SurfNum)%Tilt <= 85.d0 ) THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      IF (Surface(SurfNum)%IntConvSurfHasActiveInIt) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_A1_ChilledCeil
      ELSEIF (Surface(SurfNum)%Tilt < 5.0d0) THEN
        IF (DeltaTemp <0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableHoriz
        Else
          Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt >= 5.d0) .AND. ((Surface(SurfNum)%Tilt < 95.d0)))THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableTilted
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN  !vertical wall
        Surface(SurfNum)%IntConvClassification = InConvClass_A1_VertWalls
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      IF (Surface(SurfNum)%IntConvSurfHasActiveInIt) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_A1_HeatedFloor
      ELSEIF (Surface(SurfNum)%Tilt > 175.d0) THEN !floor
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableHoriz
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt <= 175.d0) .AND. (Surface(SurfNum)%Tilt >= 95.d0)) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableTilted
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableTilted
        ENDIF
      ENDIF
    ELSEIF ((Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_A1_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      ! assume horizontal upwards.
      IF (DeltaTemp > 0.d0) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_A1_UnstableHoriz
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_A1_StableHoriz
      ENDIF
    ENDIF

    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for A1 surface named'&
                           //Trim(Surface(SurfNum)%Name) )
    ENDIF

  CASE (InConvFlowRegime_A2)
    DeltaTemp = TH(SurfNum,1,2) - MAT(ZoneNum)
    IF (Surface(SurfNum)%Class == SurfaceClass_Wall &
         .OR. Surface(SurfNum)%Class == SurfaceClass_Door ) THEN

      IF (Surface(SurfNum)%IntConvSurfHasActiveInIt) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_A2_HeatedVerticalWall
      ELSEIF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN !vertical wall
        Surface(SurfNum)%IntConvClassification = InConvClass_A2_VertWallsNonHeated
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableTilted
        ENDIF
      ELSEIF (Surface(SurfNum)%Tilt <= 85.d0 ) THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      IF (Surface(SurfNum)%Tilt < 5.0d0) THEN
        IF (DeltaTemp <0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableHoriz
        Else
          Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt >= 5.d0) .AND. ((Surface(SurfNum)%Tilt < 95.d0)))THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableTilted
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN  !vertical wall
        Surface(SurfNum)%IntConvClassification = InConvClass_A2_VertWallsNonHeated
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      IF (Surface(SurfNum)%Tilt > 175.d0) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableHoriz
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt <= 175.d0) .AND. (Surface(SurfNum)%Tilt >= 95.d0)) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableTilted
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableTilted
        ENDIF
      ENDIF
    ELSEIF ((Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_A2_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      ! assume horizontal upwards.
      IF (DeltaTemp > 0.d0) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_A2_UnstableHoriz
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_A2_StableHoriz
      ENDIF
    ENDIF

    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for A2 surface named'&
                           //Trim(Surface(SurfNum)%Name) )
    ENDIF
  CASE (InConvFlowRegime_A3)
    DeltaTemp = TH(SurfNum,1,2) - MAT(ZoneNum)
    IF (Surface(SurfNum)%Class == SurfaceClass_Wall &
         .OR. Surface(SurfNum)%Class == SurfaceClass_Door) THEN

      IF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN !vertical wall
        Surface(SurfNum)%IntConvClassification = InConvClass_A3_VertWalls
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableTilted
        ENDIF
      ELSEIF (Surface(SurfNum)%Tilt <= 85.d0 ) THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      IF (Surface(SurfNum)%Tilt < 5.0d0) THEN
        IF (DeltaTemp <0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableHoriz
        Else
          Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt > 5.d0) .AND. ((Surface(SurfNum)%Tilt < 85.d0)))THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableTilted
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN  !vertical wall
        Surface(SurfNum)%IntConvClassification = InConvClass_A3_VertWalls
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      IF (Surface(SurfNum)%Tilt > 175.d0) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableHoriz
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt <= 175.d0) .AND. (Surface(SurfNum)%Tilt >= 95.d0)) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableTilted
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableTilted
        ENDIF
      ENDIF
    ELSEIF ((Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_A3_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      ! assume horizontal upwards.
      IF (DeltaTemp >= 0.d0) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_A3_UnstableHoriz
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_A3_StableHoriz
      ENDIF
    ENDIF

    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for A3 surface named'&
                           //Trim(Surface(SurfNum)%Name) )
    ENDIF
  CASE (InConvFlowRegime_B)
    DeltaTemp = TH(SurfNum,1,2) - MAT(ZoneNum)
    IF (Surface(SurfNum)%Class == SurfaceClass_Wall &
        .OR. Surface(SurfNum)%Class == SurfaceClass_Door) THEN

      IF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN !vertical wall
        IF (Surface(SurfNum)%IntConvSurfGetsRadiantHeat) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_B_VertWallsNearHeat
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_B_VertWalls
        ENDIF

      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_B_StableTilted
        ENDIF
      ELSEIF (Surface(SurfNum)%Tilt <= 85.d0 ) THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_B_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      IF (Surface(SurfNum)%Tilt < 5.0d0) THEN
        IF (DeltaTemp <0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableHoriz
        Else
          Surface(SurfNum)%IntConvClassification = InConvClass_B_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt >= 5.d0) .AND. ((Surface(SurfNum)%Tilt < 85.d0)))THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_B_StableTilted
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN  !vertical wall
        IF (Surface(SurfNum)%IntConvSurfGetsRadiantHeat) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_B_VertWallsNearHeat
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_B_VertWalls
        ENDIF
      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_B_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      IF (Surface(SurfNum)%Tilt > 175.d0) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableHoriz
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_B_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt <= 175.d0) .AND. (Surface(SurfNum)%Tilt >= 95.d0)) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableTilted
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_B_StableTilted
        ENDIF
      ENDIF
    ELSEIF (      (Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_B_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      ! assume horizontal upwards.
      IF (DeltaTemp > 0.d0) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_B_UnstableHoriz
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_B_StableHoriz
      ENDIF
    ENDIF

    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for B surface named'&
                           //Trim(Surface(SurfNum)%Name) )
    ENDIF
  CASE (InConvFlowRegime_C)
    IF     (Surface(SurfNum)%Class == SurfaceClass_Wall &
            .OR. Surface(SurfNum)%Class == SurfaceClass_Door) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_C_Walls
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_C_Ceiling
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_C_Floor
    ELSEIF (      (Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_C_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_C_Floor
    ENDIF
    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for C surface named'&
                           //Trim(Surface(SurfNum)%Name) )
    ENDIF

  CASE (InConvFlowRegime_D)

    DeltaTemp = TH(SurfNum,1,2) - MAT(ZoneNum)
    IF (Surface(SurfNum)%Class == SurfaceClass_Wall &
         .OR. Surface(SurfNum)%Class == SurfaceClass_Door) THEN

      IF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN !vertical wall

        Surface(SurfNum)%IntConvClassification = InConvClass_D_Walls

      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_D_StableTilted
        ENDIF
      ELSEIF (Surface(SurfNum)%Tilt <= 85.d0 ) THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_D_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      IF (Surface(SurfNum)%Tilt < 5.0d0) THEN
        IF (DeltaTemp <0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableHoriz
        Else
          Surface(SurfNum)%IntConvClassification = InConvClass_D_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt >= 5.d0) .AND. ((Surface(SurfNum)%Tilt <= 85.d0)))THEN !tilted downwards
        IF (DeltaTemp < 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_D_StableTilted
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt > 85.d0) .AND. (Surface(SurfNum)%Tilt < 95.d0 ) ) THEN  !vertical wall

        Surface(SurfNum)%IntConvClassification = InConvClass_D_Walls

      ELSEIF (Surface(SurfNum)%Tilt >= 95.d0) THEN !tilted upwards
        IF (DeltaTemp > 0.d0) THEN
         Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableTilted
        ELSE
         Surface(SurfNum)%IntConvClassification = InConvClass_D_StableTilted
        ENDIF
      ENDIF

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      IF (Surface(SurfNum)%Tilt > 175.d0) THEN !floor
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableHoriz
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_D_StableHoriz
        ENDIF
      ELSEIF ((Surface(SurfNum)%Tilt <= 175.d0) .AND. (Surface(SurfNum)%Tilt >= 95.d0)) THEN
        IF (DeltaTemp > 0.d0) THEN
          Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableTilted
        ELSE
          Surface(SurfNum)%IntConvClassification = InConvClass_D_StableTilted
        ENDIF
      ENDIF
    ELSEIF (      (Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_D_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      ! assume horizontal upwards.
      IF (DeltaTemp > 0.d0) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_D_UnstableHoriz
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_D_StableHoriz
      ENDIF
    ENDIF

    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for D surface named'&
                           //Trim(Surface(SurfNum)%Name) )
    ENDIF


  CASE (InConvFlowRegime_E)

    DeltaTemp = TH(SurfNum,1,2) - MAT(ZoneNum)
    IF (Surface(SurfNum)%Class == SurfaceClass_Wall &
         .OR. Surface(SurfNum)%Class == SurfaceClass_Door) THEN

      !mixed regime, but need to know what regime it was before it was mixed
      SELECT CASE (FlowRegimeStack(PriorityEquipOn))

      CASE (InConvFlowRegime_C)
        !assume forced flow is down along wall (ceiling diffuser)
        IF (DeltaTemp > 0.d0) THEN ! surface is hotter so plume upwards and forces oppose
          Surface(SurfNum)%IntConvClassification = InConvClass_E_OpposFlowWalls
        ELSE ! surface is cooler so plume down and forces assist
          Surface(SurfNum)%IntConvClassification = InConvClass_E_AssistFlowWalls
        ENDIF
      CASE (InConvFlowRegime_D)
        ! assume forced flow is upward along wall (perimeter zone HVAC with fan)
        IF (DeltaTemp > 0.d0) THEN ! surface is hotter so plume up and forces assist
          Surface(SurfNum)%IntConvClassification = InConvClass_E_AssistFlowWalls
        ELSE ! surface is cooler so plume downward and forces oppose
          Surface(SurfNum)%IntConvClassification = InConvClass_E_OpposFlowWalls
        ENDIF
      END SELECT

    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
      IF (DeltaTemp > 0.d0) THEN !surface is hotter so stable
        Surface(SurfNum)%IntConvClassification = InConvClass_E_StableCeiling
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_E_UnstableCieling
      ENDIF
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
      IF (DeltaTemp > 0.d0) THEN !surface is hotter so unstable
        Surface(SurfNum)%IntConvClassification = InConvClass_E_UnstableFloor
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_E_StableFloor
      ENDIF
    ELSEIF (      (Surface(SurfNum)%Class == SurfaceClass_Window)     &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_GlassDoor) &
            .OR.  (Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser)) THEN
      Surface(SurfNum)%IntConvClassification = InConvClass_E_Windows
    ELSEIF (Surface(SurfNum)%Class == SurfaceClass_IntMass) THEN
      IF (DeltaTemp > 0.d0) THEN
        Surface(SurfNum)%IntConvClassification = InConvClass_E_UnstableFloor
      ELSE
        Surface(SurfNum)%IntConvClassification = InConvClass_E_StableFloor
      ENDIF
    ENDIF
    IF (Surface(SurfNum)%IntConvClassification == 0) THEN
      CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to resolve Hc model for D surface named '&
                         //Trim(Surface(SurfNum)%Name) )
    ENDIF
  CASE DEFAULT
    CALL ShowSevereError('DynamicIntConvSurfaceClassification: failed to deterime zone flow regime for surface named ' &
                         //Trim(Surface(SurfNum)%Name) )


  END SELECT

  RETURN

END SUBROUTINE DynamicIntConvSurfaceClassification

SUBROUTINE MapIntConvClassificationToHcModels(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Map Hc model equation data from central structure to surface structure

          ! METHODOLOGY EMPLOYED:
          ! Long case statement depends on surface classification determined in DynamicIntConvSurfaceClassification
          ! then simply map data stored in InsideFaceAdaptiveConvectionAlgo into the surface's structure
          ! if model type is user-defined, also store the index to the user curve to be used.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfNum ! surface pointer index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  SELECT CASE (Surface(SurfNum)%IntConvClassification)

  CASE ( InConvClass_A1_VertWalls )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolVertWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolVertWallUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_StableHoriz  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_UnstableHoriz )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_HeatedFloor   )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolHeatedFloorEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolHeatedFloorUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_ChilledCeil   )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolChilledCeilingEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolChilledCeilingUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_StableTilted  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolStableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_UnstableTilted )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolUnstableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_A1_Windows        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolWindowsEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%FloorHeatCeilingCoolWindowsUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_VertWallsNonHeated)
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatVertWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatVertWallUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_HeatedVerticalWall )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatHeatedWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatHeatedWallUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_StableHoriz      )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_UnstableHoriz  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_StableTilted   )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatStableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_UnstableTilted )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatUnstableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_A2_Windows        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatWindowsEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%WallPanelHeatWindowsUserCurveNum
    ENDIF
  CASE ( InConvClass_A3_VertWalls      )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%SimpleBouyVertWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%SimpleBouyVertWallUserCurveNum
    ENDIF
  CASE ( InConvClass_A3_StableHoriz    )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_A3_UnstableHoriz  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_A3_StableTilted   )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%SimpleBouyStableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_A3_UnstableTilted )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%SimpleBouyUnstableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_A3_Windows        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%SimpleBouyWindowsEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%SimpleBouyWindowsUserCurveNum
    ENDIF
  CASE ( InConvClass_B_VertWalls       )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallUserCurveNum
    ENDIF
  CASE ( InConvClass_B_VertWallsNearHeat)
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallNearHeaterEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatVertWallNearHeaterUserCurveNum
    ENDIF
  CASE ( InConvClass_B_StableHoriz    )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_B_UnstableHoriz  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_B_StableTilted   )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatStableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_B_UnstableTilted )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatUnstableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_B_Windows        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatWindowsEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ConvectiveHeatWindowsUserCurveNum
    ENDIF
  CASE ( InConvClass_C_Walls          )
    IF ((Surface(SurfNum)%IntConvZonePerimLength == 0.d0) &
        .AND. (InsideFaceAdaptiveConvectionAlgo%CentralAirWallEqNum == HcInt_GoldsteinNovoselacCeilingDiffuserWalls)) THEN
      ! no perimeter, Goldstein Novolselac model not good so revert to fisher pedersen model
      Surface(SurfNum)%IntConvHcModelEq          = HcInt_FisherPedersenCeilDiffuserWalls
    ELSE
      Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%CentralAirWallEqNum
    ENDIF
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%CentralAirWallUserCurveNum
    ENDIF
  CASE ( InConvClass_C_Ceiling        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%CentralAirCeilingEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%CentralAirCeilingUserCurveNum
    ENDIF
  CASE ( InConvClass_C_Floor          )
    IF ((Surface(SurfNum)%IntConvZonePerimLength == 0.d0) &
        .AND. (InsideFaceAdaptiveConvectionAlgo%CentralAirFloorEqNum == HcInt_GoldsteinNovoselacCeilingDiffuserFloor)) THEN
      ! no perimeter, Goldstein Novolselac model not good so revert to fisher pedersen model
      Surface(SurfNum)%IntConvHcModelEq          = HcInt_FisherPedersenCeilDiffuserFloor
    ELSE
      Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%CentralAirFloorEqNum
    ENDIF
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%CentralAirFloorUserCurveNum
    ENDIF
  CASE ( InConvClass_C_Windows        )
    IF ((Surface(SurfNum)%IntConvZonePerimLength == 0.d0) &
        .AND. (InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsEqNum == HcInt_GoldsteinNovoselacCeilingDiffuserWindow)) THEN
      ! no perimeter, Goldstein Novolselac model not good so revert to ISO15099
      Surface(SurfNum)%IntConvHcModelEq          = HcInt_ISO15099Windows
    ELSE
      Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsEqNum
    ENDIF
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%CentralAirWindowsUserCurveNum
    ENDIF
  CASE ( InConvClass_D_Walls          )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircVertWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircVertWallUserCurveNum
    ENDIF
  CASE ( InConvClass_D_StableHoriz    )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_D_UnstableHoriz  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableHorizEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableHorizUserCurveNum
    ENDIF
  CASE ( InConvClass_D_StableTilted   )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircStableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_D_UnstableTilted )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableTiltedEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircUnstableTiltedUserCurveNum
    ENDIF
  CASE ( InConvClass_D_Windows        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircWindowsEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%ZoneFanCircWindowsUserCurveNum
    ENDIF
  CASE ( InConvClass_E_AssistFlowWalls)
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedBouyAssistingFlowWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedBouyAssistingFlowWallUserCurveNum
    ENDIF
  CASE ( InConvClass_E_OpposFlowWalls )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedBouyOppossingFlowWallEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedBouyOppossingFlowWallUserCurveNum
    ENDIF
  CASE ( InConvClass_E_StableFloor    )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedStableFloorEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedStableFloorUserCurveNum
    ENDIF
  CASE ( InConvClass_E_UnstableFloor  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedUnstableFloorEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedUnstableFloorUserCurveNum
    ENDIF
  CASE ( InConvClass_E_StableCeiling  )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedStableCeilingEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedStableCeilingUserCurveNum
    ENDIF
  CASE ( InConvClass_E_UnstableCieling)
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedUnstableCeilingEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedUnstableCeilingUserCurveNum
    ENDIF
  CASE ( InConvClass_E_Windows        )
    Surface(SurfNum)%IntConvHcModelEq          = InsideFaceAdaptiveConvectionAlgo%MixedWindowsEqNum
    IF (Surface(SurfNum)%IntConvHcModelEq == HcInt_UserCurve) THEN
      Surface(SurfNum)%IntConvHcUserCurveIndex = InsideFaceAdaptiveConvectionAlgo%MixedWindowsUserCurveNum
    ENDIF
  END SELECT

  RETURN

END SUBROUTINE MapIntConvClassificationToHcModels

SUBROUTINE CalcUserDefinedInsideHcModel(SurfNum, UserCurveNum, Hc)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate user-defined convection correlations for inside face

          ! METHODOLOGY EMPLOYED:
          ! call curve objects to evaluate user's model equation
          ! prepare independent parameters for x values

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment
  USE DataHeatBalSurface, ONLY: TH
  USE DataHeatBalFanSys,  ONLY: MAT
  USE DataEnvironment,    ONLY: OutBaroPress
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW, PsyWFnTdpPb
  USE CurveManager,       ONLY: CurveValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)  :: SurfNum
  INTEGER , INTENT(IN)  :: UserCurveNum
  REAL(r64), INTENT(OUT):: Hc

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpHc
  REAL(r64) :: tmpAirTemp
  REAL(r64)    :: SupplyAirTemp
  REAL(r64)    :: AirChangeRate
  INTEGER      :: ZoneNum
  INTEGER      :: ZoneNode
  INTEGER      :: EquipNum
  REAL(r64)    :: SumMdotTemp
  REAL(r64)    :: SumMdot
  REAL(r64)    :: AirDensity
  INTEGER      :: thisZoneInletNode

  ZoneNum = Surface(SurfNum)%Zone
  SumMdotTemp = 0.d0
  SumMdot     = 0.d0
  SupplyAirTemp = MAT(ZoneNum)
  IF (Zone(ZoneNum)%IsControlled) THEN
    ZoneNode = Zone(ZoneNum)%SystemZoneNodeNumber
    AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Node(ZoneNode)%Temp,PsyWFnTdpPb(Node(ZoneNode)%Temp,OutBaroPress))
    AirChangeRate = (Node(ZoneNode)%MassFlowRate * SecInHour)/ (AirDensity * Zone(ZoneNum)%Volume)
    IF (ZoneEquipConfig(ZoneNum)%EquipListIndex > 0) THEN
      DO EquipNum = 1, ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%NumOfEquipTypes
        IF (ALLOCATED(ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums)) THEN
          thisZoneInletNode = ZoneEquipList(ZoneEquipConfig(ZoneNum)%EquipListIndex)%EquipData(EquipNum)%OutletNodeNums(1)
          IF ((thisZoneInletNode > 0) .AND. (Node(thisZoneInletNode)%MassFlowRate > 0.d0)) THEN
            SumMdotTemp = SumMdotTemp + Node(thisZoneInletNode)%MassFlowRate * Node(thisZoneInletNode)%Temp
          ENDIF
        ENDIF
      ENDDO
    ENDIF
    If (SumMdot > 0.d0) THEN
      SupplyAirTemp = SumMdotTemp / SumMdot      ! mass flow weighted inlet temperature
    ENDIF
  ENDIF

  SELECT CASE (HcInsideUserCurve(UserCurveNum)%ReferenceTempType)

  CASE (RefTempMeanAirTemp)
    tmpAirTemp = MAT(ZoneNum)
    Surface(SurfNum)%TAirRef = ZoneMeanAirTemp
  CASE (RefTempAdjacentAirTemp)
    tmpAirTemp = TempEffBulkAir(SurfNum)
    Surface(SurfNum)%TAirRef = AdjacentAirTemp
  CASE (RefTempSupplyAirTemp)
    tmpAirTemp = SupplyAirTemp
    Surface(SurfNum)%TAirRef = ZoneSupplyAirTemp
  END SELECT

  tmpHc = 0.d0
  IF (HcInsideUserCurve(UserCurveNum)%HcFnTempDiffCurveNum > 0) THEN
    tmpHc = CurveValue(HcInsideUserCurve(UserCurveNum)%HcFnTempDiffCurveNum, &
                              ABS(TH(SurfNum,1, 2) - tmpAirTemp) )
  ENDIF

  IF (HcInsideUserCurve(UserCurveNum)%HcFnTempDiffDivHeightCurveNum > 0) THEN
    tmpHc = tmpHc + CurveValue(HcInsideUserCurve(UserCurveNum)%HcFnTempDiffDivHeightCurveNum, &
                              (ABS(TH(SurfNum,1, 2) - tmpAirTemp)/Surface(SurfNum)%IntConvZoneWallHeight) )
  ENDIF

  IF (HcInsideUserCurve(UserCurveNum)%HcFnACHCurveNum > 0) THEN
    tmpHc = tmpHc + CurveValue(HcInsideUserCurve(UserCurveNum)%HcFnACHCurveNum, AirChangeRate)
  ENDIF

  IF (HcInsideUserCurve(UserCurveNum)%HcFnACHDivPerimLengthCurveNum > 0) THEN
    tmpHc = tmpHc + CurveValue(HcInsideUserCurve(UserCurveNum)%HcFnACHDivPerimLengthCurveNum, &
                  (AirChangeRate / Surface(SurfNum)%IntConvZonePerimLength) )
  ENDIF

  Hc = tmpHc

  RETURN

END SUBROUTINE CalcUserDefinedInsideHcModel


SUBROUTINE CalcUserDefinedOutsideHcModel(SurfNum, UserCurveNum, H)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate user-defined convection correlations for outside face

          ! METHODOLOGY EMPLOYED:
          ! call curve objects to evaluate user's model equation
          ! prepare independent parameters for x values

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,  ONLY: WindSpeed, WindDir
  USE CurveManager,       ONLY: CurveValue
  USE DataHeatBalSurface, ONLY: TH

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)  :: SurfNum
  INTEGER , INTENT(IN)  :: UserCurveNum
  REAL(r64),INTENT(OUT) :: H

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: tmpHc
  REAL(r64) :: windVel
  REAL(r64) :: theta
  REAL(r64) :: ThetaRad

  SELECT CASE (HcOutsideUserCurve(UserCurveNum)%WindSpeedType)

  CASE (RefWindWeatherFile)
    windVel = WindSpeed
  CASE (RefWindAtZ)
    windVel = Surface(SurfNum)%WindSpeed
  CASE (RefWindParallComp)
    ! WindSpeed , WindDir, surface Azimuth
     Theta = WindDir - Surface(SurfNum)%Azimuth - 90.d0 !TODO double check theta
     ThetaRad = Theta * DegToRadians
     windVel = cos(ThetaRad) * WindSpeed
  CASE (RefWindParallCompAtZ)
    ! Surface WindSpeed , WindDir, surface Azimuth
     Theta = WindDir - Surface(SurfNum)%Azimuth - 90.d0 !TODO double check theta
     ThetaRad = Theta * DegToRadians
     windVel = cos(ThetaRad) * Surface(SurfNum)%WindSpeed
  END SELECT


  tmpHc = 0.d0
  IF (  HcOutsideUserCurve(UserCurveNum)%HfFnWindSpeedCurveNum > 0) THEN
    tmpHc = CurveValue(HcOutsideUserCurve(UserCurveNum)%HfFnWindSpeedCurveNum, &
                        windVel )
  ENDIF

  IF (  HcOutsideUserCurve(UserCurveNum)%HnFnTempDiffCurveNum > 0) THEN
    tmpHc = tmpHc + CurveValue(HcOutsideUserCurve(UserCurveNum)%HnFnTempDiffCurveNum, &
                        ABS(TH(SurfNum,1, 1) - Surface(SurfNum)%OutDryBulbTemp) )
  ENDIF

  IF (  HcOutsideUserCurve(UserCurveNum)%HnFnTempDiffDivHeightCurveNum > 0) THEN
    IF (Surface(SurfNum)%OutConvFaceHeight > 0.d0) THEN
      tmpHc = tmpHc + CurveValue(HcOutsideUserCurve(UserCurveNum)%HnFnTempDiffDivHeightCurveNum, &
                        ((ABS(TH(SurfNum,1, 1) - Surface(SurfNum)%OutDryBulbTemp))&
                        /Surface(SurfNum)%OutConvFaceHeight)  )
    ENDIF
  ENDIF

  H = tmpHc

  RETURN

END SUBROUTINE CalcUserDefinedOutsideHcModel



!** Begin catalog of Hc equation functions. **** !*************************************************
FUNCTION CalcASHRAEVerticalWall(DeltaTemp) RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the model equation attributed to ASHRAE for vertical walls for natural convection

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! 2.  ASHRAE Handbook of Fundamentals 2001, p. 3.12, Table 5.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64)             :: Hn ! function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  Hn = 1.31d0 *( (ABS(DeltaTemp))**OneThird)

  RETURN

END FUNCTION CalcASHRAEVerticalWall

FUNCTION CalcWaltonUnstableHorizontalOrTilt(DeltaTemp, CosineTilt) RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the model equation attributed to Walton's TARP program for horizontal
          ! and tilted surfaces with enhanced, thermally unstable natural convection

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
          !     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp     ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: CosineTilt   ! Cosine of tilt angle
  REAL(r64)             :: Hn ! function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  Hn =  9.482d0*((ABS(DeltaTemp))**OneThird) &
                      /(7.283d0 - ABS(CosineTilt))

  RETURN

END FUNCTION CalcWaltonUnstableHorizontalOrTilt

FUNCTION CalcWaltonStableHorizontalOrTilt(DeltaTemp, CosineTilt) RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the model equation attributed to Walton's TARP program for horizontal
          ! and tilted surfaces with reduced, thermally stable natural convection

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! 1.  Walton, G. N. 1983. Thermal Analysis Research Program (TARP) Reference Manual,
          !     NBSSIR 83-2655, National Bureau of Standards, "Surface Inside Heat Balances", pp 79-80.


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp     ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: CosineTilt   ! Cosine of tilt angle
  REAL(r64)             :: Hn ! function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  Hn = 1.810d0*((ABS(DeltaTemp))**OneThird) &
                      /(1.382d0 + ABS(CosineTilt))

  RETURN

END FUNCTION CalcWaltonStableHorizontalOrTilt

FUNCTION CalcFisherPedersenCeilDiffuserFloor(AirChangeRate) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the model equation by Fisher and Pedersen for floors with ceiling diffusers

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
          !       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AirChangeRate    ! [1/hr] air system air change rate
  REAL(r64)             :: Hc ! function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  Hc = 3.873d0 + 0.082d0 * (AirChangeRate**0.98d0)

  RETURN

END FUNCTION CalcFisherPedersenCeilDiffuserFloor

FUNCTION CalcFisherPedersenCeilDiffuserCeiling(AirChangeRate) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the model equation by Fisher and Pedersen for ceilings with ceiling diffusers

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
          !       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AirChangeRate    ! [1/hr] air system air change rate
  REAL(r64)             :: Hc ! function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  Hc = 2.234d0 + 4.099d0 * (AirChangeRate**0.503d0)

  RETURN

END FUNCTION CalcFisherPedersenCeilDiffuserCeiling

FUNCTION CalcFisherPedersenCeilDiffuserWalls(AirChangeRate) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the model equation by Fisher and Pedersen for walls with ceiling diffusers

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! Fisher, D.E. and C.O. Pedersen, Convective Heat Transfer in Building Energy and
          !       Thermal Load Calculations, ASHRAE Transactions, vol. 103, Pt. 2, 1997, p.13


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AirChangeRate    ! [1/hr] air system air change rate
  REAL(r64)             :: Hc ! function result
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  Hc = 1.208d0 + 1.012d0 * (AirChangeRate**0.604d0)

  RETURN

END FUNCTION CalcFisherPedersenCeilDiffuserWalls

FUNCTION CalcAlamdariHammondUnstableHorizontal(DeltaTemp, HydraulicDiameter, SurfNum )  RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Alamdari and Hammond
          ! This function only for the Unstable heat flow direction for horizontal surfaces

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
          ! for buoyancy-driven convection in rooms.  Building Services Engineering
          ! Research & Technology. Vol. 4, No. 3.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  INTEGER,   INTENT(IN) :: SurfNum  ! for messages
  REAL(r64)             :: Hn ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (HydraulicDiameter > 0.d0) THEN
    Hn  = (  ((1.4d0 * (( ABS(DeltaTemp)/HydraulicDiameter)**OneFourth))**6) &
           + ((1.63d0*( (ABS(DeltaTemp))**OneThird)**6) ))**OneSixth
  ELSE
    Hn = 9.999d0
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcAlamdariHammondUnstableHorizontal: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for surface =' &
                              //TRIM(Surface(SurfNum)%name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')

    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcAlamdariHammondUnstableHorizontal: Convection model not evaluated because zero' &
                               //' hydraulic diameter and set to 9.999 [W/m2-K]',  ErrorIndex )
  ENDIF

  RETURN

END FUNCTION CalcAlamdariHammondUnstableHorizontal

FUNCTION CalcAlamdariHammondStableHorizontal(DeltaTemp, HydraulicDiameter, SurfNum )  RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Alamdari and Hammond
          ! This function only for the Stable heat flow direction for horizontal surfaces

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
          ! for buoyancy-driven convection in rooms.  Building Services Engineering
          ! Research & Technology. Vol. 4, No. 3.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  INTEGER,   INTENT(IN) :: SurfNum  ! for messages
  REAL(r64)             :: Hn ! function result, natural convection Hc value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (HydraulicDiameter > 0.d0) THEN
    Hn  = 0.6d0 * (( ABS(DeltaTemp)/(HydraulicDiameter**2 )) ** OneFifth)
  ELSE
    Hn = 9.999d0
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcAlamdariHammondStableHorizontal: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for surface =' &
                              //TRIM(Surface(SurfNum)%name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcAlamdariHammondStableHorizontal: Convection model not evaluated because zero' &
                               //' hydraulic diameter and set to 9.999 [W/m2-K]' , ErrorIndex )
  ENDIF

  RETURN

END FUNCTION CalcAlamdariHammondStableHorizontal

FUNCTION CalcAlamdariHammondVerticalWall(DeltaTemp, Height, SurfNum )  RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Alamdari and Hammond
          ! This function only for the vertical wall surfaces

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Alamdari, F. and G.P. Hammond. 1983. Improved data correlations
          ! for buoyancy-driven convection in rooms.  Building Services Engineering
          ! Research & Technology. Vol. 4, No. 3.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: Height ! [m] characteristic size, = zone height
  INTEGER,   INTENT(IN) :: SurfNum  ! for messages
  REAL(r64)             :: Hn ! function result, natural convection Hc value

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (Height > 0.d0) THEN
    Hn  = (  ((1.5d0 * (( ABS(DeltaTemp)/Height)**OneFourth))**6) &
           + ((1.23d0*( (ABS(DeltaTemp))**OneThird)**6) ))**OneSixth
  ELSE
    Hn  = 9.999d0
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcAlamdariHammondVerticalWall: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for surface =' &
                              //TRIM(Surface(SurfNum)%name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcAlamdariHammondVerticalWall: Convection model not evaluated because zero' &
                               //' hydraulic diameter and set to 9.999 [W/m2-K]' , ErrorIndex)
  ENDIF

  RETURN

END FUNCTION CalcAlamdariHammondVerticalWall

FUNCTION CalcKhalifaEq3WallAwayFromHeat(DeltaTemp )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Khalifa's Eq 3 for Walls Away From Heat

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
          !   University of Wales College of Cardiff, Cardiff, UK.
          !
          ! Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Hc = 2.07d0*((ABS(DeltaTemp))**0.23d0)

  RETURN

END FUNCTION CalcKhalifaEq3WallAwayFromHeat

FUNCTION CalcKhalifaEq4CeilingAwayFromHeat(DeltaTemp )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Khalifa's Eq 4 for Ceilings Away From Heat

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
          !   University of Wales College of Cardiff, Cardiff, UK.
          !
          ! Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Hc = 2.72d0*((ABS(DeltaTemp))**0.13d0)

  RETURN

END FUNCTION CalcKhalifaEq4CeilingAwayFromHeat

FUNCTION CalcKhalifaEq5WallsNearHeat(DeltaTemp )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Khalifa's Eq 5 for Walls near the heater

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
          !   University of Wales College of Cardiff, Cardiff, UK.
          !
          ! Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Hc = 1.98d0*((ABS(DeltaTemp))**0.32d0)

  RETURN

END FUNCTION CalcKhalifaEq5WallsNearHeat

FUNCTION CalcKhalifaEq6NonHeatedWalls(DeltaTemp )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Khalifa's Eq 6 for non-heated walls

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
          !   University of Wales College of Cardiff, Cardiff, UK.
          !
          ! Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Hc = 2.30d0*((ABS(DeltaTemp))**0.24d0)

  RETURN

END FUNCTION CalcKhalifaEq6NonHeatedWalls

FUNCTION CalcKhalifaEq7Ceiling(DeltaTemp )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Khalifa's Eq 7 for ceilings

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Khalifa AJN. 1989 Heat transfer processes in buildings. Ph.D. Thesis,
          !   University of Wales College of Cardiff, Cardiff, UK.
          !
          ! Equations actually from Beausoleil-Morrison 2000 who referenced Khalifa
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Hc = 3.10d0*((ABS(DeltaTemp))**0.17d0)

  RETURN

END FUNCTION CalcKhalifaEq7Ceiling

FUNCTION CalcAwbiHattonHeatedFloor(DeltaTemp, HydraulicDiameter)  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Awbi and Hatton for heated floors

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.
          ! apply numerical protection for low values of hydraulic diameter

          ! REFERENCES:
          ! Awbi, H.B. and A. Hatton. 1999. Natural convection from heated room surfaces.
          !   Energy and Buildings 30 (1999) 233-244.
          !   This function is for equation 15 in the reference

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  IF (HydraulicDiameter > 1.d0) THEN
    Hc = 2.175d0*((ABS(DeltaTemp))**0.308d0)/ (HydraulicDiameter**0.076d0)
  ELSE
    Hc = 2.175d0*((ABS(DeltaTemp))**0.308d0)/ (1.d0**0.076d0)
  ENDIF

  RETURN

END FUNCTION CalcAwbiHattonHeatedFloor

FUNCTION CalcAwbiHattonHeatedWall(DeltaTemp, HydraulicDiameter)  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for Awbi and Hatton for heated walls

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Awbi, H.B. and A. Hatton. 1999. Natural convection from heated room surfaces.
          !   Energy and Buildings 30 (1999) 233-244.
          !   This function is for equation 12 in the reference

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  IF (HydraulicDiameter > 1.d0) THEN
    Hc = 1.823d0*((ABS(DeltaTemp))**0.293d0)/ (HydraulicDiameter**0.121d0)
  ELSE
    Hc = 1.823d0*((ABS(DeltaTemp))**0.293d0)/ (1.d0**0.121d0)
  ENDIF

  RETURN

END FUNCTION CalcAwbiHattonHeatedWall

FUNCTION CalcBeausoleilMorrisonMixedAssistedWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum)  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation Beausoleil-Morrison's mixed flow regime
          ! with mechanical and bouyancy forces assisting each other along a Wall

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: Height ! [m] characteristic size
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: SupplyAirTemp ![C] temperature of supply air into zone
  REAL(r64), INTENT(IN) :: AirChangeRate ! [ACH] [1/hour] supply air ACH for zone
  INTEGER,   INTENT(IN) :: ZoneNum  ! index of zone for messaging
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF ((DeltaTemp /= 0.d0) .and. (Height /= 0.d0)) THEN
    Hc =   (  ((  ((1.5d0 * (( ABS(DeltaTemp)/Height)**OneFourth))**6)            &
             + ((1.23d0*( (ABS(DeltaTemp))**OneThird)**6) )**OneSixth)**0.5d0   &
             + (( ((SurfTemp -  SupplyAirTemp)/ABS(DeltaTemp)) *(-0.199d0 + 0.190d0*(AirChangeRate**0.8d0) ) )**3) ))**OneThird
  ELSE
    Hc = 9.999d0
    IF (Height == 0.d0) THEN
      CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective height is zero, convection model not applicable for zone named =' &
                                //TRIM(Zone(ZoneNum)%Name))
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    IF (DeltaTemp == 0.d0 .AND. .NOT. WarmUpFlag) THEN
      IF (ErrorIndex == 0) THEN
        CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('The temperature difference between surface and air is zero')
        CALL ShowContinueError('Occurs for zone named = ' //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF

      CALL ShowRecurringWarningErrorAtEnd('CalcBeausoleilMorrisonMixedAssistedWall: Convection model not evaluated because' &
                  //' of zero temperature difference and set to 9.999 [W/m2-K]',  ErrorIndex )
    ENDIF

  ENDIF
  RETURN

END FUNCTION CalcBeausoleilMorrisonMixedAssistedWall

FUNCTION CalcBeausoleilMorrisonMixedOpposingWall(DeltaTemp, Height, SurfTemp, SupplyAirTemp, AirChangeRate, ZoneNum)  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation Beausoleil-Morrison's mixed flow regime
          ! with mechanical and bouyancy forces opposing each other along a Wall

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: Height ! [m] characteristic size
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: SupplyAirTemp ![C] temperature of supply air into zone
  REAL(r64), INTENT(IN) :: AirChangeRate ! [ACH] [1/hour] supply air ACH for zone
  INTEGER,   INTENT(IN) :: ZoneNum  ! index of zone for messaging
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: HcTmp1
  REAL(r64) :: HcTmp2
  REAL(r64) :: HcTmp3
  INTEGER, SAVE :: ErrorIndex  = 0
  INTEGER, SAVE :: ErrorIndex2 = 0

  IF ((DeltaTemp /= 0.d0) ) THEN ! protect divide by zero

    IF (Height /= 0.d0) THEN
      HcTmp1 =   (  ((  ((1.5d0 * (( ABS(DeltaTemp)/Height)**OneFourth))**6)            &
            + ((1.23d0*( (ABS(DeltaTemp))**OneThird)**6) )**OneSixth)**0.5d0   &
            - (( ((SurfTemp -  SupplyAirTemp)/ABS(DeltaTemp)) *(-0.199d0 + 0.190d0*(AirChangeRate**0.8d0) )  )**3) ))**OneThird

      HcTmp2 = 0.8d0 * ((  ((1.5d0 * (( ABS(DeltaTemp)/Height)**OneFourth))**6)            &
                 + ((1.23d0*( (ABS(DeltaTemp))**OneThird)**6) ))**OneSixth)
    ELSE
      HcTmp1 = 9.999d0
      HcTmp2 = 9.999d0
      IF (ErrorIndex2 == 0) THEN
        CALL ShowSevereMessage('CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('Effective height is zero, convection model not applicable for zone named =' &
                                //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')

      ENDIF
      CALL ShowRecurringSevereErrorAtEnd('CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated because ' &
                               //' of zero height and set to 9.999 [W/m2-K]', ErrorIndex2 )

    ENDIF
    HcTmp3 = 0.8d0 *  ((SurfTemp -  SupplyAirTemp)/ABS(DeltaTemp)) *(-0.199d0 + 0.190d0*(AirChangeRate**0.8d0) )

    Hc = MAX( MAX(HcTmp1,HcTmp2), HcTmp3)

  ELSE
    Hc = 9.999d0
    IF (.NOT. WarmUpFlag) THEN
      IF (ErrorIndex == 0) Then
        CALL ShowSevereMessage('CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('The temperature difference between surface and air is zero')
        CALL ShowContinueError('Occurs for zone named = ' //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF
      CALL ShowRecurringSevereErrorAtEnd('CalcBeausoleilMorrisonMixedOpposingWall: Convection model not evaluated because ' &
                  //'of zero temperature difference and set to 9.999 [W/m2-K]',  ErrorIndex )
    ENDIF
  ENDIF

  RETURN

END FUNCTION CalcBeausoleilMorrisonMixedOpposingWall

FUNCTION CalcBeausoleilMorrisonMixedStableFloor(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate, &
                                ZoneNum)  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation Beausoleil-Morrison's mixed flow regime
          ! with mechanical and bouyancy forces acting on an thermally stable floor

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: SupplyAirTemp ![C] temperature of supply air into zone
  REAL(r64), INTENT(IN) :: AirChangeRate ! [ACH] [1/hour] supply air ACH for zone
  INTEGER,   INTENT(IN) :: ZoneNum  ! index of zone for messaging
  REAL(r64)             :: Hc ! function result

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF ((HydraulicDiameter /= 0.d0) .AND. (DeltaTemp /= 0.d0)) THEN
    Hc = ( (0.6d0 * (ABS(DeltaTemp)/HydraulicDiameter)**OneFifth)**3  &
        + (((SurfTemp -  SupplyAirTemp )/ABS(DeltaTemp))* (0.159d0 + 0.116d0* (AirChangeRate **0.8d0)) )**3 )**OneThird
  ELSE
    Hc = 9.999d0
    IF (HydraulicDiameter == 0.d0) THEN
      CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for zone named =' &
                                //TRIM(Zone(ZoneNum)%Name))
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    IF (DeltaTemp == 0.d0 .AND. .NOT. WarmUpFlag) THEN
      IF (ErrorIndex == 0) THEN
        CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('The temperature difference between surface and air is zero')
        CALL ShowContinueError('Occurs for zone named = ' //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF

      CALL ShowRecurringWarningErrorAtEnd('CalcBeausoleilMorrisonMixedStableFloor: Convection model not evaluated because' &
                  //' of zero temperature difference and set to 9.999 [W/m2-K]',  ErrorIndex )
    ENDIF

  ENDIF
  RETURN

END FUNCTION CalcBeausoleilMorrisonMixedStableFloor

FUNCTION CalcBeausoleilMorrisonMixedUnstableFloor(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate, &
                                                  ZoneNum) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation Beausoleil-Morrison's mixed flow regime
          ! with mechanical and bouyancy forces acting on an thermally unstable floor

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: SupplyAirTemp ![C] temperature of supply air into zone
  REAL(r64), INTENT(IN) :: AirChangeRate ! [ACH] [1/hour] supply air ACH for zone
  INTEGER,   INTENT(IN) :: ZoneNum  ! index of zone for messaging
  REAL(r64)             :: Hc ! function result, total convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0


  IF ((HydraulicDiameter /= 0.d0) .AND. (DeltaTemp /= 0.d0)) THEN
    Hc = ( ((1.4d0 * (ABS(DeltaTemp)/HydraulicDiameter)**OneFourth)**6 + ( 1.63d0*(ABS(DeltaTemp)**OneThird))**6 )**0.5d0 &
        + (((SurfTemp -  SupplyAirTemp )/ABS(DeltaTemp))* (0.159d0 + 0.116d0* (AirChangeRate **0.8d0)) )**3 )**OneThird
  ELSE
    Hc = 9.999d0
    IF (HydraulicDiameter == 0.d0) THEN
      CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for zone named =' &
                                //TRIM(Zone(ZoneNum)%Name))
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    IF (DeltaTemp == 0.d0 .AND. .NOT. WarmUpFlag) THEN
      IF (ErrorIndex == 0) THEN
        CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('The temperature difference between surface and air is zero')
        CALL ShowContinueError('Occurs for zone named = ' //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF

      CALL ShowRecurringWarningErrorAtEnd('CalcBeausoleilMorrisonMixedUnstableFloor: Convection model not evaluated because' &
                  //' of zero temperature difference and set to 9.999 [W/m2-K]',  ErrorIndex )
    ENDIF

  ENDIF

  RETURN

END FUNCTION CalcBeausoleilMorrisonMixedUnstableFloor

FUNCTION CalcBeausoleilMorrisonMixedStableCeiling(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp, AirChangeRate, &
                                                    ZoneNum) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation Beausoleil-Morrison's mixed flow regime
          ! with mechanical and bouyancy forces acting on a thermally stable ceiling

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: SupplyAirTemp ![C] temperature of supply air into zone
  REAL(r64), INTENT(IN) :: AirChangeRate ! [ACH] [1/hour] supply air ACH for zone
  INTEGER,   INTENT(IN) :: ZoneNum  ! index of zone for messaging
  REAL(r64)             :: Hc ! function result, total convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF ((HydraulicDiameter /= 0.d0) .AND. (DeltaTemp /= 0.d0)) THEN
    Hc = ( (0.6d0 * (ABS(DeltaTemp)/HydraulicDiameter)**OneFifth)**3 &
           + ( ((SurfTemp -  SupplyAirTemp )/ABS(DeltaTemp))* (-0.166d0 + 0.484d0* (AirChangeRate **0.8d0)) )**3 )**OneThird
  ELSE
    Hc = 9.999d0
    IF (HydraulicDiameter == 0.d0) THEN
      CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for zone named =' &
                                //TRIM(Zone(ZoneNum)%Name))
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    IF (DeltaTemp == 0.d0 .AND. .NOT. WarmUpFlag) THEN
      IF (ErrorIndex == 0) THEN
        CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('The temperature difference between surface and air is zero')
        CALL ShowContinueError('Occurs for zone named = ' //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF

      CALL ShowRecurringWarningErrorAtEnd('CalcBeausoleilMorrisonMixedStableCeiling: Convection model not evaluated because' &
                  //' of zero temperature difference and set to 9.999 [W/m2-K]',  ErrorIndex )
    ENDIF

  ENDIF
  RETURN

END FUNCTION CalcBeausoleilMorrisonMixedStableCeiling

FUNCTION CalcBeausoleilMorrisonMixedUnstableCeiling(DeltaTemp, HydraulicDiameter, SurfTemp, SupplyAirTemp,   &
                          AirChangeRate, ZoneNum) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation Beausoleil-Morrison's mixed flow regime
          ! with mechanical and bouyancy forces acting on a thermally unstable ceiling

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Beausoleil-Morrison, I. 2000. The adaptive coupling of heat and
          !  air flow modeling within dynamic whole-building simulations.
          !  PhD. Thesis. University of Strathclyde, Glasgow, UK.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY : WarmUpFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: HydraulicDiameter ! [m] characteristic size, = (4 * area) / perimeter
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: SupplyAirTemp ![C] temperature of supply air into zone
  REAL(r64), INTENT(IN) :: AirChangeRate ! [ACH] [1/hour] supply air ACH for zone
  INTEGER,   INTENT(IN) :: ZoneNum  ! index of zone for messaging
  REAL(r64)             :: Hc ! function result, total convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF ((HydraulicDiameter /= 0.d0) .AND. (DeltaTemp /= 0.d0)) THEN
    Hc =  ( ((1.4d0 * (ABS(DeltaTemp)/HydraulicDiameter)**OneFourth)**6 + ( 1.63d0*(ABS(DeltaTemp)**OneThird))**6 )**0.5d0 &
          + (((SurfTemp -  SupplyAirTemp )/ABS(DeltaTemp))* (-0.166d0 + 0.484d0* (AirChangeRate **0.8d0)) )**3 )**OneThird
  ELSE
    Hc = 9.999d0
    IF (HydraulicDiameter == 0.d0) THEN
      CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective hydraulic diameter is zero, convection model not applicable for zone named =' &
                                //TRIM(Zone(ZoneNum)%Name))
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    IF (DeltaTemp == 0.d0 .AND. .NOT. WarmUpFlag) THEN
      IF (ErrorIndex == 0) THEN
        CALL ShowWarningMessage('CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated (would divide by zero)')
        CALL ShowContinueError('The temperature difference between surface and air is zero')
        CALL ShowContinueError('Occurs for zone named = ' //TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF

      CALL ShowRecurringWarningErrorAtEnd('CalcBeausoleilMorrisonMixedUnstableCeiling: Convection model not evaluated because' &
                  //' of zero temperature difference and set to 9.999 [W/m2-K]',  ErrorIndex )
    ENDIF
  ENDIF
  RETURN

END FUNCTION CalcBeausoleilMorrisonMixedUnstableCeiling

FUNCTION CalcFohannoPolidoriVerticalWall(DeltaTemp, Height, SurfTemp, QdotConv, SurfNum)  RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for natural convection

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Fohanno, S., and G. Polidori. 2006. Modelling of natural convective heat transfer
          ! at an internal surface. Energy and Buildings 38 (2006) 548 - 553

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp         ! [C] temperature difference between surface and air
  REAL(r64), INTENT(IN) :: Height ! [m] characteristic size, height of zone
  REAL(r64), INTENT(IN) :: SurfTemp ![C] surface temperature
  REAL(r64), INTENT(IN) :: QdotConv ![W/m2] heat flux rate for rayleigh #
  INTEGER,   INTENT(IN) :: SurfNum  ! for messages
  REAL(r64)             :: Hn ! function result, natural convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: g  = 9.81d0       ! gravity constant (m/s**2)
  REAL(r64), PARAMETER :: v  = 15.89d-6   ! kinematic viscosity (m**2/s) for air at 300 K
  REAL(r64), PARAMETER :: k  = 0.0263d0     ! thermal conductivity (W/m K) for air at 300 K
  REAL(r64), PARAMETER :: Pr = 0.71d0      ! Prandtl number for air at ?
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: RaH = 0.d0
  REAL(r64) :: BetaFilm = 0.d0
  INTEGER, SAVE :: ErrorIndex = 0

  BetaFilm = 1.d0 / (KelvinConv + SurfTemp + 0.5d0 * DeltaTemp) ! TODO check sign on DeltaTemp
  IF (Height > 0.d0) THEN
    RaH = (g * BetaFilm * QdotConv * (Height**4) * Pr )/(k * v**2)

    IF (RaH <= 6.3d09) Then
      Hn = 1.332d0*((ABS(DeltaTemp)/Height)**OneFourth)
    ELSE
      Hn = 1.235d0*EXP(0.0467d0*Height)*(ABS(DeltaTemp))**0.316d0
    ENDIF
  ELSE
   ! bad value for Height, but we have little info to identify calling culprit
    Hn = 9.999d0
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcFohannoPolidoriVerticalWall: Convection model not evaluated (would divide by zero)')
      CALL ShowContinueError('Effective surface height is zero, convection model not applicable for surface =' &
                              //TRIM(Surface(SurfNum)%name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')

    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcFohannoPolidoriVerticalWall: Convection model not evaluated because zero' &
                               //' height and set to 9.999 [W/m2-K]' , ErrorIndex)
  ENDIF

  RETURN

END FUNCTION CalcFohannoPolidoriVerticalWall

FUNCTION CalcKaradagChilledCeiling(DeltaTemp)  RESULT (Hn)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jul 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for natural convection developed by Karadag for chilled ceilings

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Karadag, R. 2009. New approach relevant to total heat transfer coefficient
          !   including the effect of radiation and convection at the ceiling in a cooled
          !   ceiling room.  Applied Thermal Engineering 29 (2009) 1561-1565
          !    This function is for equation 8 in the reference

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64),   INTENT(IN) :: DeltaTemp  ! [C] temperature difference between surface and air
  REAL(r64)               :: Hn ! function result, natural convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  Hn = 3.1d0* (ABS(DeltaTemp))**0.22d0

  RETURN

END FUNCTION CalcKaradagChilledCeiling


FUNCTION CalcGoldsteinNovoselacCeilingDiffuserWindow(AirSystemFlowRate, ZoneExtPerimLength, WindWallRatio,  &
                             WindowLocationType , ZoneNum)  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for windows in zones with slot diffusers on them
          !  developed by Novoselac for RP-1416

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
          !  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AirSystemFlowRate  ! [m3/s] air system flow rate
  REAL(r64), INTENT(IN) :: ZoneExtPerimLength ! [m] length of zone perimeter with exterior walls
  REAL(r64), INTENT(IN) :: WindWallRatio ![ ] fraction of window area to wall area for zone
  INTEGER, INTENT(IN)   :: WindowLocationType !index for location types
  INTEGER, INTENT(IN)   :: ZoneNum ! for messages
  REAL(r64)             :: Hc ! function result, total convection coefficient


          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0
  INTEGER, SAVE :: ErrorIndex2 = 0

  IF (ZoneExtPerimLength > 0.d0) THEN
    IF (WindWallRatio <= 0.5d0) THEN

      IF (WindowLocationType == InConvWinLoc_UpperPartOfExteriorWall) THEN
        Hc = 0.117d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)
      ELSEIF(WindowLocationType == InConvWinLoc_LowerPartOfExteriorWall) THEN
        Hc = 0.093d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)
      ELSEIF(WindowLocationType == InConvWinLoc_LargePartOfExteriorWall) THEN
        Hc = 0.117d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0) ! assumption for case not covered by model
      ELSEIF(WindowLocationType == InConvWinLoc_NotSet) THEN
        Hc = 0.117d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0) ! assumption for case not covered by model
      ELSE
        !shouldn'tcome
        Hc = 9.999d0
        IF (ErrorIndex == 0) THEN
          CALL ShowSevereMessage('CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated ' &
                                  //'( bad relative window location)')
          CALL ShowContinueError('Value for window location = '//TRIM(RoundSigDigits(WindowLocationType)))
          CALL ShowContinueError('Occurs for zone named = '//TRIM(Zone(ZoneNum)%Name) )
          CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated because ' &
                               //'bad window location and set to 9.999 [W/m2-K]', ErrorIndex )
      ENDIF
    ELSE
      Hc = 0.103d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)
    ENDIF
  ELSE
    Hc = 9.999d0
    IF (ErrorIndex2 == 0) THEN
      CALL ShowSevereMessage('CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated ' &
                               //'(zero zone exterior perimeter length)' )
      CALL ShowContinueError('Value for zone exterior perimeter length = ' //TRIM(RoundSigDigits(ZoneExtPerimLength,5)))
      CALL ShowContinueError('Occurs for zone named = '//TRIM(Zone(ZoneNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcGoldsteinNovoselacCeilingDiffuserWindow: Convection model not evaluated because ' &
                                //' bad perimeter length and set to 9.999 [W/m2-K]' , ErrorIndex2 )
  ENDIF
  RETURN

END FUNCTION CalcGoldsteinNovoselacCeilingDiffuserWindow

FUNCTION CalcGoldsteinNovoselacCeilingDiffuserWall(AirSystemFlowRate, ZoneExtPerimLength, &
                            WindowLocationType, ZoneNum )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for exterior walls in zones with slot diffusers on them
          !  developed by Novoselac for RP-1416

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
          !  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AirSystemFlowRate  ! [m3/s] air system flow rate
  REAL(r64), INTENT(IN) :: ZoneExtPerimLength ! [m] length of zone perimeter with exterior walls
  INTEGER, INTENT(IN)   :: WindowLocationType !index for location types
  INTEGER, INTENT(IN)   :: ZoneNum ! for messages
  REAL(r64)             :: Hc ! function result, total convection coefficient


          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0
  INTEGER, SAVE :: ErrorIndex2 = 0

  IF (ZoneExtPerimLength > 0.d0) THEN
    IF (WindowLocationType == InConvWinLoc_WindowAboveThis) THEN
      Hc = 0.063d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)
    ELSEIF(WindowLocationType == InConvWinLoc_WindowBelowThis) THEN
      Hc = 0.093d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)
    ELSEIF(WindowLocationType == InConvWinLoc_NotSet) THEN
      Hc = 0.063d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)  ! assumption for case not covered by model
    ELSE
      Hc = 9.999d0
      IF (ErrorIndex == 0) THEN
        CALL ShowSevereMessage('CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated ' &
                                //'( bad relative window location)')
        CALL ShowContinueError('Value for window location = '//TRIM(RoundSigDigits(WindowLocationType)))
        CALL ShowContinueError('Occurs for zone named = '//TRIM(Zone(ZoneNum)%Name) )
        CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
      ENDIF
      CALL ShowRecurringSevereErrorAtEnd('CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated because ' &
                              //'bad window location and set to 9.999 [W/m2-K]', ErrorIndex )

    ENDIF
  ELSE
    Hc = 9.999d0
    IF (ErrorIndex2 == 0) THEN
      CALL ShowSevereMessage('CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated ' &
                               //'(zero zone exterior perimeter length)' )
      CALL ShowContinueError('Value for zone exterior perimeter length = ' //TRIM(RoundSigDigits(ZoneExtPerimLength,5)))
      CALL ShowContinueError('Occurs for zone named = '//TRIM(Zone(ZoneNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcGoldsteinNovoselacCeilingDiffuserWall: Convection model not evaluated because ' &
                                //' bad perimeter length and set to 9.999 [W/m2-K]' , ErrorIndex2 )

  ENDIF
  RETURN

END FUNCTION CalcGoldsteinNovoselacCeilingDiffuserWall

FUNCTION CalcGoldsteinNovoselacCeilingDiffuserFloor(AirSystemFlowRate, ZoneExtPerimLength, ZoneNum )  RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate model equation for floors in zones with slot diffusers on them
          !  developed by Novoselac for RP-1416

          ! METHODOLOGY EMPLOYED:
          ! isolate function for equation.

          ! REFERENCES:
          ! Goldstien, K. and A. Novoselac. 2010. Convective Heat Transfer in Rooms
          !  With Ceiling Slot Diffusers (RP-1416). HVAC&R Research Journal TBD

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AirSystemFlowRate  ! [m3/s] air system flow rate
  REAL(r64), INTENT(IN) :: ZoneExtPerimLength ! [m] length of zone perimeter with exterior walls
  INTEGER, INTENT(IN)   :: ZoneNum ! for messages
  REAL(r64)             :: Hc ! function result, total convection coefficient


          ! FUNCTION PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (ZoneExtPerimLength > 0.d0) THEN
    Hc = 0.048d0*((AirSystemFlowRate / ZoneExtPerimLength)**0.8d0)
  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcGoldsteinNovoselacCeilingDiffuserFloor: Convection model not evaluated ' &
                               //'(zero zone exterior perimeter length)' )
      CALL ShowContinueError('Value for zone exterior perimeter length = ' //TRIM(RoundSigDigits(ZoneExtPerimLength,5)))
      CALL ShowContinueError('Occurs for zone named = '//TRIM(Zone(ZoneNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcGoldsteinNovoselacCeilingDiffuserFloor: Convection model not evaluated because ' &
                                //' bad perimeter length and set to 9.999 [W/m2-K]' , ErrorIndex )

    Hc = 9.999d0 ! safe but noticeable
  ENDIF
  RETURN

END FUNCTION CalcGoldsteinNovoselacCeilingDiffuserFloor

FUNCTION CalcSparrowWindward(RoughnessIndex, FacePerimeter, FaceArea, WindAtZ, SurfNum) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate Sparrow Hf for windward surfaces

          ! METHODOLOGY EMPLOYED:
          ! encapsulate equation as a function

          ! REFERENCES:

          !   1. TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff
          !   2. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
          !   width on heat transfer and fluid flow about an inclined rectangular plate.
          !   Journal of Heat Transfer 101:  204.
          !   3. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
          !   procedure with a detailed study of outside heat transfer parameters.
          !   M.S. Thesis, Department of Mechanical and Industrial Engineering,
          !   University of Illinois at Urbana-Champaign.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: RoughnessIndex
  REAL(r64), INTENT(IN) :: FacePerimeter
  REAL(r64), INTENT(IN) :: FaceArea
  REAL(r64), INTENT(IN) :: WindAtZ
  INTEGER  , INTENT(IN) :: SurfNum
  REAL(r64)             :: Hf

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (FaceArea > 0.d0) THEN
    Hf = 2.53d0 * RoughnessMultiplier(RoughnessIndex)*(( FacePerimeter * WindAtZ/FaceArea)**0.5d0)

  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcSparrowWindward: Convection model not evaluated (bad face area)')
      CALL ShowContinueError('Value for effective face area = ' //TRIM(RoundSigDigits(FaceArea,5)))
      CALL ShowContinueError('Occurs for surface named = ' //TRIM(Surface(SurfNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcSparrowWindward: Convection model not evaluated because ' &
                                      //'bad face area and set to 9.999 [W/m2-k]' , ErrorIndex )
    Hf = 9.999d0 ! safe but noticeable
  ENDIF
  RETURN

END FUNCTION CalcSparrowWindward

FUNCTION CalcSparrowLeeward(RoughnessIndex, FacePerimeter, FaceArea, WindAtZ, SurfNum) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate Sparrow Hf for leeward surfaces

          ! METHODOLOGY EMPLOYED:
          ! encapsulate equation as a function

          ! REFERENCES:

          !   1. TARP Reference Manual, "Surface Outside Heat Balances", pp 71ff
          !   2. Sparrow, E. M., J. W. Ramsey, and E. A. Mass.  1979.  Effect of finite
          !   width on heat transfer and fluid flow about an inclined rectangular plate.
          !   Journal of Heat Transfer 101:  204.
          !   3. McClellan, T.M.  1996.  Investigation of a heat balance cooling load
          !   procedure with a detailed study of outside heat transfer parameters.
          !   M.S. Thesis, Department of Mechanical and Industrial Engineering,
          !   University of Illinois at Urbana-Champaign.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: RoughnessIndex
  REAL(r64), INTENT(IN) :: FacePerimeter
  REAL(r64), INTENT(IN) :: FaceArea
  REAL(r64), INTENT(IN) :: WindAtZ
  INTEGER  , INTENT(IN) :: SurfNum
  REAL(r64)             :: Hf

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (FaceArea > 0.d0) THEN
    Hf = 2.53d0 * 0.5d0 * RoughnessMultiplier(RoughnessIndex)*(( FacePerimeter * WindAtZ/FaceArea)**0.5d0)
  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcSparrowLeeward: Convection model not evaluated (bad face area)')
      CALL ShowContinueError('Value for effective face area = ' //TRIM(RoundSigDigits(FaceArea,5)))
      CALL ShowContinueError('Occurs for surface named = ' //TRIM(Surface(SurfNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcSparrowLeeward: Convection model not evaluated because ' &
                                      //'bad face area and set to 9.999 [W/m2-k]' , ErrorIndex )

    Hf = 9.999d0 ! safe but noticeable
  ENDIF
  RETURN

END FUNCTION CalcSparrowLeeward

FUNCTION CalcMoWITTWindward(DeltaTemp, WindAtZ) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate MoWITT Hc equation for windward surfaces

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model equation in a function

          ! REFERENCES:
          !   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
          !   film coefficient for windows in low-rise buildings.
          !   ASHRAE Transactions 100(1):  1087.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp
  REAL(r64), INTENT(IN) :: WindAtZ
  REAL(r64)             :: Hc ! total convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  Hc = ((  0.84d0*((ABS(DeltaTemp))**OneThird)  )**2 + (3.26d0 * (WindAtZ**0.89d0) )**2) **0.5d0

  RETURN

END FUNCTION CalcMoWITTWindward

FUNCTION CalcMoWITTLeeward(DeltaTemp, WindAtZ) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate MoWITT Hc equation for leeward surfaces

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model equation in a function

          ! REFERENCES:
          !   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
          !   film coefficient for windows in low-rise buildings.
          !   ASHRAE Transactions 100(1):  1087.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DeltaTemp
  REAL(r64), INTENT(IN) :: WindAtZ
  REAL(r64)             :: Hc ! total convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  Hc = ((  0.84d0*((ABS(DeltaTemp))**OneThird)  )**2 + (3.55d0 * (WindAtZ**0.617d0) )**2) **0.5d0

  RETURN

END FUNCTION CalcMoWITTLeeward

FUNCTION CalcDOE2Windward(SurfaceTemp, AirTemp, CosineTilt, WindAtZ, RoughnessIndex) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate DOE-2 Hc equation for windward surfaces

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model equation in a function

          ! REFERENCES:
          !   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.
          !   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
          !   film coefficient for windows in low-rise buildings.
          !   ASHRAE Transactions 100(1):  1087.


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: SurfaceTemp
  REAL(r64), INTENT(IN) :: AirTemp
  REAL(r64), INTENT(IN) :: CosineTilt
  REAL(r64), INTENT(IN) :: WindAtZ
  INTEGER  , INTENT(IN) :: RoughnessIndex
  REAL(r64)             :: Hf ! forced convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: HcSmooth
  REAL(r64) :: Hn
  REAL(r64) :: DeltaTemp

  DeltaTemp = SurfaceTemp - AirTemp

  Hn = CalcHnASHRAETARPExterior(SurfaceTemp, AirTemp, CosineTilt)

  HcSmooth = SQRT(Hn**2 + (3.26d0 * WindAtZ ** 0.89d0)**2)

  Hf = RoughnessMultiplier(RoughnessIndex) * (HcSmooth - Hn)

  RETURN

END FUNCTION CalcDOE2Windward

FUNCTION CalcDOE2Leeward(SurfaceTemp, AirTemp, CosineTilt, WindAtZ, RoughnessIndex) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate DOE-2 Hc equation for leeward surfaces

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model equation in a function

          ! REFERENCES:
          !   Lawrence Berkeley Laboratory.  1994.  DOE2.1E-053 source code.
          !   Yazdanian, M. and J.H. Klems.  1994.  Measurement of the exterior convective
          !   film coefficient for windows in low-rise buildings.
          !   ASHRAE Transactions 100(1):  1087.


          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: SurfaceTemp
  REAL(r64), INTENT(IN) :: AirTemp
  REAL(r64), INTENT(IN) :: CosineTilt
  REAL(r64), INTENT(IN) :: WindAtZ
  INTEGER  , INTENT(IN) :: RoughnessIndex
  REAL(r64)             :: Hf ! forced convection coefficient

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: HcSmooth
  REAL(r64) :: Hn
  REAL(r64) :: DeltaTemp

  DeltaTemp = SurfaceTemp - AirTemp

  Hn = CalcHnASHRAETARPExterior(SurfaceTemp, AirTemp, CosineTilt)

  HcSmooth = SQRT(Hn**2 + (3.55d0 * WindAtZ ** 0.617d0)**2)

  Hf = RoughnessMultiplier(RoughnessIndex) * (HcSmooth - Hn)

  RETURN

END FUNCTION CalcDOE2Leeward

FUNCTION CalcNusseltJurges(WindAtZ) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate model equation for forced convection using Nusselt Jurges correlation
          ! model is attributed to Nusselt and Jurges but the equation is recast in current units
          ! by Palyvos

          ! METHODOLOGY EMPLOYED:
          ! encapsulate the model equation in a function

          ! REFERENCES:
          !
          ! 1. Nusselt, W., W. Jurges. 1922. Die Kuhlung einer ebenen Wand durch einen Luftstrom
          !     (The cooling of a plane wall by an air flow). Gesundheits Ingenieur 52, Heft, 45, Jargang.
          ! 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
          !     envelope energy systems’ modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: WindAtZ
  REAL(r64)             :: Hc

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  Hc = 5.8d0 + 3.94d0 * WindAtZ

  RETURN

END FUNCTION CalcNusseltJurges

FUNCTION CalcMcAdams(WindAtZ) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate model equation for forced convection using McAdams correlation
          ! model is attributed to McAdams but the equation is as recast in current units
          ! by Palyvos

          ! METHODOLOGY EMPLOYED:
          ! encapsulate the model equation in a function

          ! REFERENCES:
          !
          ! 1. McAdams, W.H., 1954. Heat Transmission, third ed., McGraw-Hill, New York.
          ! 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
          !     envelope energy systems’ modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: WindAtZ
  REAL(r64)             :: Hc

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  Hc = 5.8d0 + 3.8d0 * WindAtZ

  RETURN

END FUNCTION CalcMcAdams

FUNCTION CalcMitchell(WindAtZ, LengthScale , SurfNum) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate model equation for forced convection using Mitchell correlation
          ! model is attributed to Mitchell but the equation is as recast in current units
          ! by Palyvos

          ! METHODOLOGY EMPLOYED:
          ! encapsulate the model equation in a function

          ! REFERENCES:
          ! 1. Mitchell, J.W., 1976. Heat transfer from spheres and other animal forms. Biophy. J. 16 (1976) 561
          ! 2. Palyvos, J.A., 2008. A survey of wind convection coefficient correlations for building
          !     envelope energy systems’ modeling. Applied Thermal Engineering 28 (2008) 801-808. Elsevier.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: WindAtZ
  REAL(r64), INTENT(IN) :: LengthScale
  INTEGER  , INTENT(IN) :: SurfNum
  REAL(r64)             :: Hf

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE :: ErrorIndex = 0

  IF (LengthScale > 0.d0) THEN
    Hf = 8.6d0 * (WindAtZ**0.6d0) / (LengthScale**0.4d0)
  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcMitchell: Convection model not evaluated (bad length scale)')
      CALL ShowContinueError('Value for effective length scale = ' //TRIM(RoundSigDigits(LengthScale,5)))
      CALL ShowContinueError('Occurs for surface named = ' //TRIM(Surface(SurfNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcMitchell: Convection model not evaluated because ' &
                                      //'bad length scale and set to 9.999 [W/m2-k]' , ErrorIndex )
    Hf = 9.999d0 ! safe but noticeable
  ENDIF
  RETURN

END FUNCTION CalcMitchell

FUNCTION CalcBlockenWindward(WindAt10m, WindDir, SurfAzimuth) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate model equation for forced convection using Blocken correlation

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model in function

          ! REFERENCES:
          ! Blocken, B., T. Defraeye, D. Derome, J. Carmeliet. 2009.
          !  High-Resolution CFD Simulations for Forced Convection
          !   Heat Transfer Coefficients at the Facade of a Low-Rise Building.
          !   Building and Environment 44 (2009) 2396 – 2412.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) , INTENT(IN) :: WindAt10m
  REAL(r64) , INTENT(IN) :: WindDir ! Wind direction measured clockwise from geographhic North
  REAL(r64) , INTENT(IN) :: SurfAzimuth ! or Facing, Direction the surface outward normal faces (degrees)
  REAL(r64)              :: Hf

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Theta ! angle between wind and surface azimuth

  Theta = WindDir - SurfAzimuth - 90.d0 !TODO double check theta
  IF (Theta > 180.0d0) Theta = Theta - 360.00

  IF (Theta <= 11.25d0) THEN
    Hf = 4.6d0 * (WindAt10m**0.89d0)
  ELSEIF ((11.25d0 < Theta) .AND. (Theta <= 33.75d0)) THEN
    Hf = 5.d0 * (WindAt10m**0.8d0)
  ELSEIF ((33.75d0 < Theta) .AND. (Theta <= 56.25d0)) THEN
    Hf = 4.6d0 * (WindAt10m**0.84d0)
  ELSEIF ((56.25d0 < Theta) .AND. (Theta <= 100.d0)) THEN
    Hf = 4.5d0 * (WindAt10m**0.81d0)
  ELSE
    ! should not be used for leeward... check why come here?
    Hf = 3.54d0 * (WindAt10m**0.76d0)  !emmel model for robustness?
  ENDIF
  RETURN

END FUNCTION CalcBlockenWindward

FUNCTION CalcEmmelVertical(WindAt10m, WindDir, SurfAzimuth, SurfNum) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate model equation for forced convection using Emmel correlation
          ! for vertical walls

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model in function

          ! REFERENCES:
          ! Emmel, M.G., M.O. Abadie, N. Mendes. 2007. New external convective
          !   heat transfer coefficient correlations for isolated low-rise buildings.
          !    Energy and Buildings 39 (2007) 335- 342

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) , INTENT(IN) :: WindAt10m
  REAL(r64) , INTENT(IN) :: WindDir ! Wind direction measured clockwise from geographhic North
  REAL(r64) , INTENT(IN) :: SurfAzimuth ! or Facing, Direction the surface outward normal faces (degrees)
  INTEGER   , INTENT(IN) :: SurfNum
  REAL(r64)              :: Hf

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Theta ! angle between wind and surface azimuth
  INTEGER, SAVE :: ErrorIndex = 0

  Theta = WindDir - SurfAzimuth - 90.d0 !TODO double check theta
  IF (Theta > 180 ) Theta = Theta - 360.0d0

  IF (Theta <= 22.5d0) THEN
    Hf = 5.15d0 * (WindAt10m**0.81d0)
  ELSEIF ((22.5d0 < Theta) .AND. (Theta <= 67.5d0)) THEN
    Hf = 3.34d0 * (WindAt10m**0.84d0)
  ELSEIF ((67.5d0 < Theta) .AND. (Theta <= 112.5d0)) THEN
    Hf = 4.78d0 * (WindAt10m**0.71d0)
  ELSEIF ((112.5d0 < Theta) .AND. (Theta <= 157.5d0)) THEN
    Hf = 4.05d0 * (WindAt10m**0.77d0)
  ELSEIF ((157.5d0 < Theta) .AND. (Theta <= 180.d0)) THEN
    Hf = 3.54d0 * (WindAt10m**0.76d0)

  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcEmmelVertical: Convection model wind angle calculation suspect' &
                               //'(developer issue)' )
      CALL ShowContinueError('Value for theta angle = ' //TRIM(RoundSigDigits(Theta,5)))
      CALL ShowContinueError('Occurs for surface named = ' //TRIM(Surface(SurfNum)%Name) )
      CALL ShowContinueError('Convection model uses high theta correlation and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcEmmelVertical: Convection model wind angle calculation suspect' &
                             //' and high theta correlation', ErrorIndex)
    Hf = 3.54d0 * (WindAt10m**0.76d0)
  ENDIF
  RETURN

END FUNCTION CalcEmmelVertical

FUNCTION CalcEmmelRoof(WindAt10m, WindDir, LongAxisOutwardAzimuth, SurfNum) RESULT (Hf)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate model equation for forced convection using Emmel correlation
          ! for horizontal roofs

          ! METHODOLOGY EMPLOYED:
          ! encapsulate model in function

          ! REFERENCES:
          ! Emmel, M.G., M.O. Abadie, N. Mendes. 2007. New external convective
          !   heat transfer coefficient correlations for isolated low-rise buildings.
          !    Energy and Buildings 39 (2007) 335- 342

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) , INTENT(IN) :: WindAt10m
  REAL(r64) , INTENT(IN) :: WindDir ! Wind direction measured clockwise from geographhic North
  REAL(r64) , INTENT(IN) :: LongAxisOutwardAzimuth ! or Facing, Direction the surface outward normal faces (degrees)
  INTEGER   , INTENT(IN) :: SurfNum
  REAL(r64)              :: Hf

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Theta ! angle between wind and surface azimuth
  INTEGER, SAVE :: ErrorIndex = 0

  Theta = WindDir - LongAxisOutwardAzimuth - 90.d0 !TODO double check theta
  IF (Theta > 180.0d0) Theta = Theta - 360.0d0

  IF (Theta <= 22.5d0) THEN
    Hf = 5.15d0 * (WindAt10m**0.81d0)
  ELSEIF ((22.5d0 < Theta) .AND. (Theta <= 67.5d0)) THEN
    Hf = 3.34d0 * (WindAt10m**0.84d0)
  ELSEIF ((67.5d0 < Theta) .AND. (Theta <= 112.5d0)) THEN
    Hf = 4.78d0 * (WindAt10m**0.71d0)
  ELSEIF ((112.5d0 < Theta) .AND. (Theta <= 157.5d0)) THEN
    Hf = 4.05d0 * (WindAt10m**0.77d0)
  ELSEIF ((157.5d0 < Theta) .AND. (Theta <= 180.d0)) THEN
    Hf = 3.54d0 * (WindAt10m**0.76d0)

  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcEmmelRoof: Convection model wind angle calculation suspect' &
                               //'(developer issue)' )
      CALL ShowContinueError('Value for theta angle = ' //TRIM(RoundSigDigits(Theta,5)))
      CALL ShowContinueError('Occurs for surface named = ' //TRIM(Surface(SurfNum)%Name) )
      CALL ShowContinueError('Convection model uses high theta correlation and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcEmmelRoof: Convection model wind angle calculation suspect' &
                             //' and high theta correlation', ErrorIndex)

    Hf = 3.54d0 * (WindAt10m**0.76d0)
  ENDIF
  RETURN

END FUNCTION CalcEmmelRoof

FUNCTION CalcClearRoof(SurfNum, SurfTemp, AirTemp, WindAtZ, WindDirect, RoofArea, RoofPerimeter) RESULT (Hc)

          ! FUNCTION INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: OutBaroPress, OutHumRat
  USE psychrometrics,  ONLY: PsyRhoAirFnPbTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,    INTENT(IN) :: SurfNum
  REAL(r64) , INTENT(IN) :: SurfTemp
  REAL(r64) , INTENT(IN) :: AirTemp
  REAL(r64) , INTENT(IN) :: WindAtZ
  REAL(r64) , INTENT(IN) :: WindDirect ! Wind direction measured clockwise from geographhic North
  REAL(R64) , INTENT(IN) :: RoofArea
  REAL(r64) , INTENT(IN) :: RoofPerimeter
  REAL(r64)              :: Hc

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: g  = 9.81d0       ! gravity constant (m/s**2)
  REAL(r64), PARAMETER :: v  = 15.89d-6   ! kinematic viscosity (m**2/s) for air at 300 K
  REAL(r64), PARAMETER :: k  = 0.0263d0     ! thermal conductivity (W/m K) for air at 300 K
  REAL(r64), PARAMETER :: Pr = 0.71d0      ! Prandtl number for air at ?

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DeltaTemp
  REAL(r64) :: Ln
  REAL(r64) :: RaLn ! Rayleigh number
  REAL(r64) :: GrLn ! Grashof number
  REAL(r64) :: AirDensity
  REAL(r64) :: Rex ! Reynolds number
  REAL(r64) :: x  ! distance to roof edge toward wind direction
  REAL(r64) :: eta
  REAL(r64), DIMENSION(6) :: RfARR
  REAL(r64) :: Rf
  REAL(r64) :: BetaFilm
  INTEGER, SAVE :: ErrorIndex = 0

  RfARR = (/2.10d0, 1.67d0, 1.52d0, 1.13d0,1.11d0, 1.0d0/)

  Rf = RfARR(Material(Construct(Surface(SurfNum)%Construction)%LayerPoint(1))%Roughness)
  !find x, don't know x. avoid time consuming geometry algorithm
  x = SQRT(RoofArea)/2.d0 ! quick simplification, geometry routines to develop

  IF (RoofPerimeter >0.d0 ) THEN
    Ln = RoofArea / RoofPerimeter
  ELSE
    Ln = SQRT(RoofArea)
  ENDIF
  DeltaTemp = SurfTemp - AirTemp
  BetaFilm = 1.d0 / (KelvinConv + SurfTemp + 0.5d0 * DeltaTemp)
  AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,AirTemp,OutHumRat)

  GrLn = g * (AirDensity**2) * (Ln**3 ) * ABS(DeltaTemp) * BetaFilm / v**2
  RaLn = GrLn * Pr

  Rex = WindAtZ * AirDensity * x / v

  IF (Rex > 0.1d0) THEN !avoid zero and crazy small denominators
    eta = (log(1.0d0+GrLn/Rex**2))/(1.0d0 + Log(1.0d0 + GrLn/(Rex**2)))
  ELSE
    eta = 1.d0 ! forced convection gone because no wind
  ENDIF

  IF ( x > 0.d0) THEN
    Hc = eta *(k/Ln)*0.15d0*(RaLn**OneThird) + (k/x)*Rf*0.0296d0*(Rex**FourFifths)*(Pr**OneThird)
  ELSE
    IF (ErrorIndex == 0) THEN
      CALL ShowSevereMessage('CalcClearRoof: Convection model not evaluated (bad value for distance to roof edge)')
      CALL ShowContinueError('Value for distance to roof edge ='//TRIM(RoundSigDigits(x,3)))
      CALL ShowContinueError('Occurs for surface named = ' //TRIM(Surface(SurfNum)%Name) )
      CALL ShowContinueError('Convection surface heat transfer coefficient set to 9.999 [W/m2-K] and the simulation continues')
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcClearRoof: Convection model not evaluated because ' &
                                       //'bad value for distance to roof edge and set to 9.999 [W/m2-k]' , ErrorIndex)
    Hc = 9.9999d0 ! safe but noticeable
  ENDIf
  RETURN

END FUNCTION CalcClearRoof

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

END MODULE ConvectionCoefficients
