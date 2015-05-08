MODULE HeatBalFiniteDiffManager

          ! Module containing the heat balance simulation routines

          ! MODULE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2003
          !       RE-ENGINEERED  Curtis Pedersen, 2006, Changed to Implicit FD calc for conduction.
          !                      and included enthalpy formulations for phase change materials
          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms required to
          ! manage the fiite difference heat balance simulation on the building.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! The MFD moisture balance method
          !  C. O. Pedersen, Enthalpy Formulation of conduction heat transfer problems
          !    involving latent heat, Simulation, Vol 18, No. 2, February 1972

          ! OTHER NOTES:
          !
          !

          ! USE STATEMENTS:
          ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, Only:MaxNameLength,KelvinConv,outputfiledebug,   &
                      TimeStepZone,Hourofday,TimeStep,OutputFileInits,BeginEnvrnFlag,DayofSim,WarmupFlag,SecInHour
USE DataInterfaces
USE DataMoistureBalance
USE DataHeatBalance, Only: Material, TotMaterials,MaxLayersInConstruct, QRadThermInAbs,Construct,      &
                           TotConstructs,SolutionAlgo,UseCondFD,UseCondFDSimple,RegularMaterial,Air
USE DataHeatBalSurface, Only: NetLWRadToSurf, QRadSWOutAbs, QRadSWInAbs,QRadSWOutMvIns, TempSource, &
                         OpaqSurfInsFaceConductionFlux,OpaqSurfOutsideFaceConductionFlux,QsrcHist,OpaqSurfInsFaceConduction,  &
                         QdotRadOutRepPerArea,MinSurfaceTempLimit,MaxSurfaceTempLimit
Use DataSurfaces, Only: Surface, TotSurfaces,Ground,SurfaceClass_Window
Use DataHeatBalFanSys, Only: Mat, ZoneAirHumRat, QHTRadSysSurf, QHWBaseboardSurf, QSteamBaseboardSurf, QElecBaseboardSurf
Use DataEnvironment, Only: SkyTemp, IsRain
USE Psychrometrics , Only:PsyRhFnTdbRhovLBnd0C,PsyWFnTdbRhPb,PsyHgAirFnWTdb
  ! Fan system Source/Sink heat value, and source/sink location temp from CondFD
USE DataHeatBAlFanSys,  ONLY: QRadSysSource,TCondFDSourceNode,QPVSysSource
USE HeatBalanceMovableInsulation,  ONLY : EvalOutsideMovableInsulation

IMPLICIT NONE   ! Enforce explicit typing of all variables

PRIVATE   !Make everything private except what is specifically Public

          ! MODULE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER :: Lambda=2500000.0d0

INTEGER,   PARAMETER :: CrankNicholsonSecondOrder = 1 ! original CondFD scheme.  semi implicit, second order in time
INTEGER,   PARAMETER :: FullyImplicitFirstOrder  = 2 ! fully implicit scheme, first order in time.

          ! DERIVED TYPE DEFINITIONS:
TYPE ConstructionDataFD
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Name ! Name of construction
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: DelX                 !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TempStability
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: MoistStability

  Integer, ALLOCATABLE, DIMENSION(:) :: NodeNumPoint  !
!  INTEGER, ALLOCATABLE, DIMENSION(:) :: InterfaceNodeNums   ! Layer interfaces occur at these nodes

  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: Thickness
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: NodeXlocation ! sized to TotNode, contains X distance in m from outside face
  INTEGER :: TotNodes=0

  Integer :: DeltaTime=0

END TYPE ConstructionDataFD


TYPE, PUBLIC :: SurfaceDataFD
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: T             !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TOld          !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TT
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: Rhov          !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: RhovOld       !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: RhoT
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TD            !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TDT            !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TDTLast
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TDOld         !
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: TDreport
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: RH
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: RHreport
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: EnthOld        ! Current node enthalpy
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: EnthNew        ! Node enthalpy at new time
  REAL(r64),    ALLOCATABLE, DIMENSION(:) :: EnthLast
  INTEGER                                 :: GSloopCounter = 0 ! count of inner loop iterations
  INTEGER                                 :: GSloopErrorCount = 0 ! recurring error counter
  REAL(r64)                               :: MaxNodeDelTemp = 0 ! largest change in node temps after calc

END TYPE SurfaceDataFD

          ! MODULE VARIABLE DECLARATIONS:
TYPE (ConstructionDataFD),      ALLOCATABLE, DIMENSION(:) :: ConstructFD
TYPE (SurfaceDataFD), PUBLIC,   ALLOCATABLE, DIMENSION(:) :: SurfaceFD

!REAL(r64) :: TFDout   =0.0d0
!REAL(r64) :: TFDin    =0.0d0
!REAL(r64) :: rhovFDout=0.0d0
!REAL(r64) :: rhovFDin =0.0d0
!REAL(r64) :: TDryout  =0.0d0
!REAL(r64) :: Tdryin   =0.0d0
!REAL(r64) :: RHOut    =0.0d0
!REAL(r64) :: RHIn     =0.0d0
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SigmaR      !  Total Resistance of construction layers
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SigmaC      !  Total Capacitance of construction layers

!REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WSurfIn         !Humidity Ratio of the inside surface for reporting
!REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QMassInFlux     !MassFlux on Surface for reporting
!REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QMassOutFlux    !MassFlux on Surface for reporting
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QHeatInFlux     !HeatFlux on Surface for reporting
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QHeatOutFlux    !HeatFlux on Surface for reporting
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxZoneToInSurf !sum of Heat flows at the surface to air interface, zone-side boundary conditions W/m2
                                                           ! before CR 8280 was not reported, but was calculated.
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxOutsideToOutSurf !sum of Heat flows at the surface to air interface, Out-side boundary conditions W/m2
                                                           ! before CR 8280 was
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxInArrivSurfCond !conduction between surface node and first node into the surface (sensible)
                                                           ! before CR 8280 was -- Qdryin    !HeatFlux on Surface for reporting for Sensible only
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QFluxOutArrivSurfCond  !HeatFlux on Surface for reporting for Sensible only
                                                                 ! before CR 8280 -- Qdryout         !HeatFlux on Surface for reporting for Sensible only


INTEGER   :: CondFDSchemeType = CrankNicholsonSecondOrder  ! solution scheme for CondFD
REAL(r64) :: SpaceDescritConstant = 3.d0 ! spatial descritization constant,
REAL(r64) :: MinTempLimit = -100.d0 ! lower limit check, degree C
REAL(r64) :: MaxTempLimit =  100.d0 ! upper limit check, degree C
INTEGER   :: MaxGSiter = 200  ! maximum number of Gauss Seidel iterations
LOGICAL   :: GetInputFlag=.true.
          ! Subroutine Specifications for the Heat Balance Module
          ! Driver Routines
PUBLIC  ManageHeatBalFiniteDiff

          ! Initialization routines for module
PUBLIC  InitHeatBalFiniteDiff
PRIVATE GetCondFDInput

          ! Algorithms for the module
PRIVATE CalcHeatBalFiniteDiff
PRIVATE ExteriorBCEqns
PRIVATE InteriorBCEqns
PRIVATE terpld
PRIVATE InteriorNodeEqns
PRIVATE IntInterfaceNodeEqns


          ! Reporting routines for module
PRIVATE ReportHeatBalFiniteDiff
PRIVATE ReportFiniteDiffInits

          ! Update Data Routine
PUBLIC  UpdateMoistureBalanceFD


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE ManageHeatBalFiniteDiff(SurfNum,TempSurfInTmp,TempSurfOutTmp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the moisture balance method.  It is called
          ! from the HeatBalanceManager at the time step level.
          ! This driver manages the calls to all of
          ! the other drivers and simulation algorithms.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  Integer, Intent(In) :: SurfNum
  REAL(r64), Intent(InOut) :: TempSurfInTmp       !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64), Intent(InOut) :: TempSurfOutTmp      !Outside Surface Temperature of each Heat Transfer Surface

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


          ! FLOW:

  ! Get the moisture balance input at the beginning of the simulation only
  IF (GetInputFlag) THEN
      ! Obtains conduction FD related parameters from input file

!    If(SolutionAlgo == UseCondFD .or. SolutionAlgo == UseCondFDSimple) CALL GetCondFDInput

    CALL GetCondFDInput
    CALL InitHeatBalFiniteDiff  ! Initialize all heat balance related parameters

    CALL ReportFiniteDiffInits  ! Report the results from the Finite Diff Inits

    GetInputFlag=.false.
  ENDIF

  ! Solve the zone heat & moisture balance using a finite difference solution
  CALL CalcHeatBalFiniteDiff(SurfNum,TempSurfInTmp,TempSurfOutTmp)

  CALL ReportHeatBalFiniteDiff(SurfNum)

  RETURN

END SUBROUTINE ManageHeatBalFiniteDiff


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetCondFDInput
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Curtis Pedersen
          !       DATE WRITTEN   July 2006
          !       MODIFIED       Brent Griffith Mar 2011, user settings
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver for initializations for the variable property CondFD part of the
          ! MFD algorithm

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList
  USE DataHeatBalance, ONLY: MaxAllowedDelTempCondFD, CondFDRelaxFactor, CondFDRelaxFactorInput, CondFDVariableProperties

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
            ! na
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat           ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(3) &
          :: MaterialNames ! Number of Material Alpha names defined
  CHARACTER(len=MaxNameLength),DIMENSION(3)  :: ConstructionName ! Name of Construction with CondFDsimplified
  INTEGER :: MaterNum         ! Counter to keep track of the material number
  INTEGER :: MaterialNumAlpha ! Number of material alpha names being passed
  INTEGER :: MaterialNumProp  ! Number of material properties being passed
  REAL(r64), DIMENSION(40) :: MaterialProps !Temporary array to transfer material properties
  LOGICAL :: ErrorsFound = .false. ! If errors detected in input
  INTEGER :: CondFDMat                ! Number of variable property CondFD materials in input
  INTEGER :: ConstructNumber     ! Cconstruction with CondHBsimple to be overridden with CondHBdetailed
  INTEGER :: TotHBAlgorithmOverrideConstructs  ! Number of constructions where HBAlgoSimplified will be overriddem with detailed.
  INTEGER :: NumConstructionAlpha
  Integer :: Loop
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers

  ! user settings for numerical parameters
  cCurrentModuleObject = 'HeatBalanceSettings:ConductionFiniteDifference'

  IF (GetNumObjectsFound(TRIM(cCurrentModuleObject)) > 0) THEN
    CALL GetObjectItem(TRIM(cCurrentModuleObject),1,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNumbers,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IF (.NOT. lAlphaFieldBlanks(1)) THEN

      SELECT CASE (cAlphaArgs(1))

      CASE ('CRANKNICHOLSONSECONDORDER')
        CondFDSchemeType = CrankNicholsonSecondOrder
      CASE ('FULLYIMPLICITFIRSTORDER')
        CondFDSchemeType = FullyImplicitFirstOrder
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
         ' entered='//TRIM(MaterialNames(1))//', must match CrankNicholsonSecondOrder or FullyImplicitFirstOrder.')
      END SELECT

    ENDIF

    IF (.NOT. lNumericFieldBlanks(1)) THEN
      SpaceDescritConstant = rNumericArgs(1)
    ENDIF
    IF (.NOT. lNumericFieldBlanks(2)) THEN
      CondFDRelaxFactorInput = rNumericArgs(2)
      CondFDRelaxFactor      = CondFDRelaxFactorInput
    ENDIF
    IF (.NOT. lNumericFieldBlanks(3)) THEN
      MaxAllowedDelTempCondFD = rNumericArgs(3)
    ENDIF

  ENDIF ! settings object


  ! Load the additional CondFD Material properties
  cCurrentModuleObject='MaterialProperty:PhaseChange'    ! Phase Change Information First
  CondFDMat=GetNumObjectsFound(TRIM(cCurrentModuleObject))

  IF ( CondFDMat .NE. 0 ) Then                      !  Get Phase Change info
      CondFDVariableProperties = .TRUE.
      DO Loop=1,CondFDMat

    !Call Input Get routine to retrieve material data
    CALL GetObjectItem(TRIM(cCurrentModuleObject),Loop,MaterialNames,MaterialNumAlpha, &
                       MaterialProps,MaterialNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


    !Load the material derived type from the input data.
    MaterNum = FindItemInList(MaterialNames(1),Material%Name,TotMaterials)
    IF (MaterNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
         ' entered='//TRIM(MaterialNames(1))//', must match to a valid Material name.')
      ErrorsFound=.true.
      Cycle
    ENDIF

    IF (Material(MaterNum)%Group /= RegularMaterial) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
         ': Reference Material is not appropriate type for CondFD properties, material='//  &
         TRIM(Material(MaterNum)%Name)//', must have regular properties (L,Cp,K,D)')
      ErrorsFound=.true.
    ENDIF


    ! Once the material derived type number is found then load the additional CondFD variable material properties
    !   Some or all may be zero (default).  They will be checked when calculating node temperatures
    Material(MaterNum)%tk1             = MaterialProps(1)
    Material(MaterNum)%TempEnth(1,1)   = MaterialProps(2)
    Material(MaterNum)%TempEnth(1,2)   = MaterialProps(3)
    Material(MaterNum)%TempEnth(2,1)   = MaterialProps(4)
    Material(MaterNum)%TempEnth(2,2)   = MaterialProps(5)
    Material(MaterNum)%TempEnth(3,1)   = MaterialProps(6)
    Material(MaterNum)%TempEnth(3,2)   = MaterialProps(7)
    Material(MaterNum)%TempEnth(4,1)   = MaterialProps(8)
    Material(MaterNum)%TempEnth(4,2)   = MaterialProps(9)
    Material(MaterNum)%TempEnth(5,1)   = MaterialProps(10)
    Material(MaterNum)%TempEnth(5,2)   = MaterialProps(11)
    Material(MaterNum)%TempEnth(6,1)   = MaterialProps(12)
    Material(MaterNum)%TempEnth(6,2)   = MaterialProps(13)
    Material(MaterNum)%TempEnth(7,1)   = MaterialProps(14)
    Material(MaterNum)%TempEnth(7,2)   = MaterialProps(15)
    Material(MaterNum)%TempEnth(8,1)   = MaterialProps(16)
    Material(MaterNum)%TempEnth(8,2)   = MaterialProps(17)
    Material(MaterNum)%TempEnth(9,1)   = MaterialProps(18)
    Material(MaterNum)%TempEnth(9,2)   = MaterialProps(19)
    Material(MaterNum)%TempEnth(10,1)   = MaterialProps(20)
    Material(MaterNum)%TempEnth(10,2)   = MaterialProps(21)
    Material(MaterNum)%TempEnth(11,1)   = MaterialProps(22)
    Material(MaterNum)%TempEnth(11,2)   = MaterialProps(23)
    Material(MaterNum)%TempEnth(12,1)   = MaterialProps(24)
    Material(MaterNum)%TempEnth(12,2)   = MaterialProps(25)
    Material(MaterNum)%TempEnth(13,1)   = MaterialProps(26)
    Material(MaterNum)%TempEnth(13,2)   = MaterialProps(27)
    Material(MaterNum)%TempEnth(14,1)   = MaterialProps(28)
    Material(MaterNum)%TempEnth(14,2)   = MaterialProps(29)
    Material(MaterNum)%TempEnth(15,1)   = MaterialProps(30)
    Material(MaterNum)%TempEnth(15,2)   = MaterialProps(31)
    Material(MaterNum)%TempEnth(16,1)   = MaterialProps(32)
    Material(MaterNum)%TempEnth(16,2)   = MaterialProps(33)

   ENDDO
  END IF
!   Get CondFD Variable Thermal Conductivity Input

  cCurrentModuleObject='MaterialProperty:VariableThermalConductivity'    ! Variable Thermal Conductivity Info next
  CondFDMat=GetNumObjectsFound(TRIM(cCurrentModuleObject))
  IF ( CondFDMat .NE. 0 ) Then   !  variable k info
    CondFDVariableProperties = .TRUE.
    DO Loop=1,CondFDMat

      !Call Input Get routine to retrieve material data
      CALL GetObjectItem(TRIM(cCurrentModuleObject),Loop,MaterialNames,MaterialNumAlpha, &
                       MaterialProps,MaterialNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


      !Load the material derived type from the input data.
      MaterNum = FindItemInList(MaterialNames(1),Material%Name,TotMaterials)
      IF (MaterNum == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
         ' entered='//TRIM(MaterialNames(1))//', must match to a valid Material name.')
        ErrorsFound=.true.
        Cycle
      ENDIF

      IF (Material(MaterNum)%Group /= RegularMaterial) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
           ': Reference Material is not appropriate type for CondFD properties, material='//  &
           TRIM(Material(MaterNum)%Name)//', must have regular properties (L,Cp,K,D)')
        ErrorsFound=.true.
      ENDIF


    ! Once the material derived type number is found then load the additional CondFD variable material properties
    !   Some or all may be zero (default).  They will be checked when calculating node temperatures
      Material (MaterNum)%TempCond(1,1)       =  MaterialProps(1)
      Material (MaterNum)%TempCond(1,2)       =  MaterialProps(2)
      Material (MaterNum)%TempCond(2,1)       =  MaterialProps(3)
      Material (MaterNum)%TempCond(2,2)       =  MaterialProps(4)
      Material (MaterNum)%TempCond(3,1)       =  MaterialProps(5)
      Material (MaterNum)%TempCond(3,2)       =  MaterialProps(6)
      Material (MaterNum)%TempCond(4,1)       =  MaterialProps(7)
      Material (MaterNum)%TempCond(4,2)       =  MaterialProps(8)
      Material (MaterNum)%TempCond(5,1)       =  MaterialProps(9)
      Material (MaterNum)%TempCond(5,2)       =  MaterialProps(10)
      Material (MaterNum)%TempCond(6,1)       =  MaterialProps(11)
      Material (MaterNum)%TempCond(6,2)       =  MaterialProps(12)
      Material (MaterNum)%TempCond(7,1)       =  MaterialProps(13)
      Material (MaterNum)%TempCond(7,2)       =  MaterialProps(14)
      Material (MaterNum)%TempCond(8,1)       =  MaterialProps(15)
      Material (MaterNum)%TempCond(8,2)       =  MaterialProps(16)
      Material (MaterNum)%TempCond(9,1)       =  MaterialProps(17)
      Material (MaterNum)%TempCond(9,2)       =  MaterialProps(18)
      Material (MaterNum)%TempCond(10,1)      =  MaterialProps(19)
      Material (MaterNum)%TempCond(10,2)       =  MaterialProps(20)
      Material (MaterNum)%TempCond(11,1)       =  MaterialProps(21)
      Material (MaterNum)%TempCond(11,2)       =  MaterialProps(22)
      Material (MaterNum)%TempCond(12,1)       =  MaterialProps(23)
      Material (MaterNum)%TempCond(12,2)       =  MaterialProps(24)
      Material (MaterNum)%TempCond(13,1)       =  MaterialProps(25)
      Material (MaterNum)%TempCond(13,2)       =  MaterialProps(26)
      Material (MaterNum)%TempCond(14,1)       =  MaterialProps(27)
      Material (MaterNum)%TempCond(14,2)       =  MaterialProps(28)
      Material (MaterNum)%TempCond(15,1)       =  MaterialProps(29)
      Material (MaterNum)%TempCond(15,2)       =  MaterialProps(30)
      Material (MaterNum)%TempCond(16,1)       =  MaterialProps(31)
      Material (MaterNum)%TempCond(16,2)       =  MaterialProps(32)


    ENDDO
  END IF



  cCurrentModuleObject='ConstructionProperty:UseHBAlgorithmCondFDDetailed'    ! Simple algorithm override.
  TotHBAlgorithmOverrideConstructs = GetNumObjectsFound(trim(cCurrentModuleObject))
  IF ( TotHBAlgorithmOverrideConstructs .NE. 0 ) Then   !  overrides are present
    DO Loop=1,TotHBAlgorithmOverrideConstructs

      !Call GetObject routine to retrieve algorithm overrides
      CALL GetObjectItem(TRIM(cCurrentModuleObject),Loop,ConstructionName,NumConstructionAlpha, &
                   MaterialProps,MaterialNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ConstructNumber = FindIteminList( Constructionname(1),Construct%Name,TotConstructs)
      IF (ConstructNumber > 0) THEN
        Construct(ConstructNumber)%UseHBAlgorithmCondFDDetailed = .True.
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
           ' entered='//TRIM(Constructionname(1))//', must match to a valid Construction name.')
        ErrorsFound=.true.
      ENDIF
    End Do

  End If

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetCondFDInput: Errors found getting '//TRIM(cCurrentModuleObject)//' properties. Program terminates.')
  ENDIF


  RETURN

END SUBROUTINE GetCondFDInput

SUBROUTINE InitHeatBalFiniteDiff

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Oct 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  C O Pedersen 2006
          !                      B. Griffith May 2011 move begin-environment and every-timestep inits, cleanup formatting

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets the initial values for the FD moisture calculation

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY:SetupOutputVariable

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: OneTimeFlag = .true.
  LOGICAL,SAVE        :: MyEnvrnFlag=.TRUE.
  INTEGER :: Lay
  INTEGER :: SurfNum
  CHARACTER(len=MaxNameLength) :: LayChar

  REAL(r64)             :: dxn        ! Intermediate calculation of nodal spacing. This is the full dx. There is
                                                  ! a half dxn thick node at each surface. dxn is the "capacitor" spacing.
  INTEGER                           :: ipts1      ! Intermediate calculation for number of full thickness nodes per layer. There
                                                  ! are always two half nodes at the layer faces.
  INTEGER :: Layer      ! Loop counter
  INTEGER :: OutwardMatLayerNum ! layer index, layer outward of the current layer
  INTEGER :: layerNode
  INTEGER :: Delt
  INTEGER :: ConstrNum    ! Loop counter
  INTEGER :: TotNodes     ! Loop counter
  INTEGER :: CurrentLayer ! Loop counter
  INTEGER :: Surf         ! Loop counter
  INTEGER :: index        ! Loop Counters

  REAL(r64) :: Alpha
  REAL(r64) :: Malpha
  REAL(r64) :: stabilitytemp
  REAL(r64) :: stabilitymoist
  REAL(r64) :: TempInit = 20.0d0
  REAL(r64) :: RhovInit = 0.0115d0
  REAL(r64) :: RHInit
  REAL(r64) :: Uinit
  REAL(r64) :: EnthInit = 100.0d0
  REAL(r64) :: At
  REAL(r64) :: Bv
  REAL(r64) :: a
  REAL(r64) :: b
  REAL(r64) :: c
  REAL(r64) :: d
  REAL(r64) :: kt
  REAL(r64) :: RhoS
  REAL(r64) :: Por
  REAL(r64) :: Cp
  REAL(r64) :: Dv
  LOGICAL :: errorsFound

  IF (GetInputFlag) THEN
      ! Obtains conduction FD related parameters from input file
    CALL GetCondFDInput
  ENDIF

  errorsFound=.false.
  IF (OneTimeFlag) Then  ! change 3/07

    ALLOCATE(ConstructFD(TotConstructs))
    ALLOCATE(SigmaR(TotConstructs))
    ALLOCATE(SigmaC(TotConstructs))

    ALLOCATE(SurfaceFD(TotSurfaces))
    ALLOCATE(QHeatInFlux(TotSurfaces))
    ALLOCATE(QHeatOutFlux(TotSurfaces))
    ALLOCATE(QFluxInArrivSurfCond(TotSurfaces))
    ALLOCATE(QFluxOutArrivSurfCond(TotSurfaces))
    ALLOCATE(OpaqSurfInsFaceConductionFlux(TotSurfaces))
    ALLOCATE(OpaqSurfOutsideFaceConductionFlux(TotSurfaces))
    ALLOCATE(QFluxZoneToInSurf(TotSurfaces))
    ALLOCATE(QFluxOutsideToOutSurf(TotSurfaces))

   ! And then initialize
    TempInit = 20.0d0
    RhovInit = 0.0115d0
    QHeatInFlux  = 0.d0
    QHeatOutFlux  = 0.d0
    QFluxZoneToInSurf  = 0.d0
    QFluxOutsideToOutSurf  = 0.d0
    QFluxInArrivSurfCond  = 0.d0
    QFluxOutArrivSurfCond  = 0.d0
    OpaqSurfInsFaceConductionFlux = 0.d0
    OpaqSurfOutsideFaceConductionFlux = 0.d0
    EnthInit = 100.0d0

    ! Setup Output Variables

    !  set a Delt that fits the zone time step and keeps it below 200s.

    DO index = 1, 20
      Delt = (TimeStepZone*SecInHour)/index   ! TimeStepZone = Zone time step in fractional hours
      IF ( Delt <= 200 )  EXIT
    END DO

    DO ConstrNum = 1, TotConstructs
      !Need to skip window constructions and eventually window materials
      IF(Construct(ConstrNum)%TypeIsWindow) CYCLE

      ALLOCATE(ConstructFD(ConstrNum)%Name(Construct(ConstrNum)%TotLayers))
      ALLOCATE(ConstructFD(ConstrNum)%Thickness(Construct(ConstrNum)%TotLayers))
      ALLOCATE(ConstructFD(ConstrNum)%NodeNumPoint(Construct(ConstrNum)%TotLayers))
      ALLOCATE(ConstructFD(ConstrNum)%DelX(Construct(ConstrNum)%TotLayers))
      ALLOCATE(ConstructFD(ConstrNum)%TempStability(Construct(ConstrNum)%TotLayers))
      ALLOCATE(ConstructFD(ConstrNum)%MoistStability(Construct(ConstrNum)%TotLayers))
        ! Node Number of the Interface node following each layer
  !    ALLOCATE(ConstructFD(ConstrNum)%InterfaceNodeNums(Construct(ConstrNum)%TotLayers))


      TotNodes = 0
      SigmaR(ConstrNum) =0.0d0
      SigmaC(ConstrNum) =0.0d0

      DO Layer = 1, Construct(ConstrNum)%TotLayers    ! Begin layer loop ...

            ! Loop through all of the layers in the current construct. The purpose
            ! of this loop is to define the thermal properties  and to.
            ! determine the total number of full size nodes in each layer.
            ! The number of temperature points is one more than this
            ! because of the two half nodes at the layer faces.
            ! The calculation of dxn used here is based on a standard stability
            ! criteria for explicit finite difference solutions.  This criteria
            ! was chosen not because it is viewed to be correct, but rather for
            ! lack of any better criteria at this time.  The use of a Fourier
            ! number based criteria such as this is probably physically correct.
            !  Change to implicit formulation still uses explicit stability, but
            ! now there are special equations for R-only layers.

        CurrentLayer = Construct(ConstrNum)%LayerPoint(Layer)


        ConstructFD(ConstrNum)%Name(Layer) = Material(CurrentLayer)%Name
        ConstructFD(ConstrNum)%Thickness(Layer) = Material(CurrentLayer)%Thickness

       ! Do some quick error checks for this section.

        IF (Material(CurrentLayer)%ROnly  ) THEN  ! Rlayer

          !  These values are only needed temporarily and to calculate flux,
          !   Layer will be handled
          !  as a pure R in the temperature calc.
          ! assign other properties based on resistance

          Material(CurrentLayer)%SpecHeat  = 0.0001d0
          Material(CurrentLayer)%Density = 1.d0
          Material(currentLayer)%Thickness = 0.1d0  !  arbitrary thickness for R layer
          Material(CurrentLayer)%Conductivity= &
          Material(currentLayer)%Thickness/Material(CurrentLayer)%Resistance
          kt = Material(CurrentLayer)%Conductivity
          ConstructFD(ConstrNum)%Thickness(Layer) = Material(CurrentLayer)%Thickness

          SigmaR(ConstrNum) = SigmaR(ConstrNum) + Material(CurrentLayer)%Resistance  ! add resistance of R layer
          SigmaC(ConstrNum) = SigmaC(ConstrNum)  + 0.0                               !  no capacitance for R layer

          Alpha = kt/(Material(CurrentLayer)%Density*Material(CurrentLayer)%SpecHeat)

          mAlpha = 0.d0

        ELSE IF (Material(CurrentLayer)%Group == 1 ) THEN   !  Group 1 = Air

                  !  Again, these values are only needed temporarily and to calculate flux,
                  !   Air layer will be handled
                  !  as a pure R in the temperature calc.
                  !
                  ! assign
                  ! other properties based on resistance

          Material(CurrentLayer)%SpecHeat  = 0.0001d0
          Material(CurrentLayer)%Density = 1.d0
          Material(currentLayer)%Thickness = 0.1d0  !  arbitrary thickness for R layer
          Material(currentLayer)%Conductivity= &
          Material(currentLayer)%Thickness/Material(CurrentLayer)%Resistance
          kt = Material(CurrentLayer)%Conductivity
          ConstructFD(ConstrNum)%Thickness(Layer) = Material(CurrentLayer)%Thickness

          SigmaR(ConstrNum) = SigmaR(ConstrNum) + Material(CurrentLayer)%Resistance  ! add resistance of R layer
          SigmaC(ConstrNum) = SigmaC(ConstrNum)  + 0.0                               !  no capacitance for R layer

          Alpha = kt/(Material(CurrentLayer)%Density*Material(CurrentLayer)%SpecHeat)
          mAlpha = 0.d0
        ELSE IF (Construct(ConstrNum)%TypeIsIRT) THEN  ! make similar to air? (that didn't seem to work well)
          CALL ShowSevereError('InitHeatBalFiniteDiff: Construction ="'//trim(Construct(ConstrNum)%Name)//  &
               '" uses Material:InfraredTransparent. Cannot be used currently with finite difference calculations.')
          IF (Construct(ConstrNum)%IsUsed) THEN
            CALL ShowContinueError('...since this construction is used in a surface, the simulation is not allowed.')
            errorsFound=.true.
          ELSE
            CALL ShowContinueError('...if this construction were used in a surface, the simulation would be terminated.')
          ENDIF
          CYCLE
!            ! set properties to get past other initializations.
!          Material(CurrentLayer)%SpecHeat  = 0.0001d0
!          Material(CurrentLayer)%Density = 0.0001d0
!          Material(currentLayer)%Thickness = 0.1d0  !  arbitrary thickness for R layer
!          Material(currentLayer)%Conductivity= &
!    -      Material(currentLayer)%Thickness/Material(CurrentLayer)%Resistance
!          kt = Material(CurrentLayer)%Conductivity
!          ConstructFD(ConstrNum)%Thickness(Layer) = Material(CurrentLayer)%Thickness
!
!          SigmaR(ConstrNum) = SigmaR(ConstrNum) + Material(CurrentLayer)%Resistance  ! add resistance of R layer
!          SigmaC(ConstrNum) = SigmaC(ConstrNum)  + 0.0                               !  no capacitance for R layer
!
!          Alpha = kt/(Material(CurrentLayer)%Density*Material(CurrentLayer)%SpecHeat)
!          mAlpha = 0.d0
        ELSE
         !    Regular material Properties
          a = Material(CurrentLayer)%MoistACoeff
          b = Material(CurrentLayer)%MoistBCoeff
          c = Material(CurrentLayer)%MoistCCoeff
          d = Material(CurrentLayer)%MoistDCoeff
          kt = Material(CurrentLayer)%Conductivity
          RhoS = Material(CurrentLayer)%Density
          Por = Material(CurrentLayer)%Porosity
          Cp = Material(CurrentLayer)%SpecHeat
           ! Need Resistance for reg layer
          Material(CurrentLayer)%Resistance = Material(currentLayer)%Thickness/Material(CurrentLayer)%Conductivity
          Dv = Material(CurrentLayer)%VaporDiffus
          SigmaR(ConstrNum) = SigmaR(ConstrNum) + Material(CurrentLayer)%Resistance  ! add resistance
          SigmaC(ConstrNum) = SigmaC(ConstrNum)  + &
          Material(CurrentLayer)%Density*Material(CurrentLayer)%SpecHeat*Material(CurrentLayer)%Thickness
          Alpha = kt/(RhoS*Cp)
          !   Alpha = kt*(Por+At*RhoS)/(RhoS*(Bv*Por*Lambda+Cp*(Por+At*RhoS)))
          MAlpha = 0.d0

        END IF  !  R, Air  or regular material properties and parameters

       ! Proceed with setting node sizes in layers

        DXN=SQRT(Alpha*Delt*SpaceDescritConstant)  ! The Fourier number is set using user constant

         ! number of nodes=thickness/spacing.  This is number of full size node spaces across layer.
        Ipts1=INT(Material(CurrentLayer)%Thickness/DXN)
         !  set high conductivity layers to a single full size node thickness. (two half nodes)
        IF(Ipts1 <= 1) Ipts1 = 1
        IF (Material(CurrentLayer)%ROnly .or. Material(CurrentLayer)%Group == 1 ) THEN

          Ipts1 = 1  !  single full node in R layers- surfaces of adjacent material or inside/outside layer
        END If

        Dxn=Material(CurrentLayer)%Thickness/REAL(Ipts1,r64)  ! full node thickness

        StabilityTemp = Alpha*Delt/dxn**2
        StabilityMoist = malpha*Delt/dxn**2
        ConstructFD(ConstrNum)%TempStability(Layer) = StabilityTemp
        ConstructFD(ConstrNum)%MoistStability(Layer) = StabilityMoist
        ConstructFD(ConstrNum)%Delx(Layer) = DXN

        TotNodes = TotNodes + Ipts1   !  number of full size nodes
        ConstructFD(ConstrNum)%NodeNumPoint(Layer) = Ipts1   !  number of full size nodes
      END DO   !  end of layer loop.


      IF( SolutionAlgo == UseCondFDSimple .and. &
                 .not. construct(ConstrNum)%UseHBAlgorithmCondFDDetailed) THEN
        TotNodes = 1                        !  Override for SigmaR SigmaC constructs*****************
        Construct(ConstrNum)%TotLayers = 1  !  Simple one layer approximation
      END IF

      ConstructFD(ConstrNum)%TotNodes = TotNodes
      ConstructFD(ConstrNum)%DeltaTime = Delt



    END DO            ! End of Construction Loop.  TotNodes in each construction now set

    ! now determine x location, or distance that nodes are from the outside face in meters
    IF (SolutionAlgo /= UseCondFDSimple) THEN
      DO ConstrNum = 1, TotConstructs
         IF (ConstructFD(ConstrNum)%TotNodes > 0) THEN
           ALLOCATE(ConstructFD(ConstrNum)%NodeXlocation(ConstructFD(ConstrNum)%TotNodes + 1 ))
           ConstructFD(ConstrNum)%NodeXlocation = 0.d0 ! init them all
           Ipts1 = 0 ! init counter
           DO Layer = 1, Construct(ConstrNum)%TotLayers
             OutwardMatLayerNum = Layer - 1
             DO layerNode = 1, ConstructFD(ConstrNum)%NodeNumPoint(Layer)
               Ipts1 = Ipts1 +1
               IF (Ipts1 == 1) THEN
                 ConstructFD(ConstrNum)%NodeXlocation(Ipts1) = 0.d0 ! first node is on outside face

               ELSEIF (LayerNode == 1) THEN
                 IF ( OutwardMatLayerNum > 0 .AND. OutwardMatLayerNum <= Construct(ConstrNum)%TotLayers ) THEN
                 ! later nodes are Delx away from previous, but use Delx from previous layer
                   ConstructFD(ConstrNum)%NodeXlocation(Ipts1) = ConstructFD(ConstrNum)%NodeXlocation(Ipts1 - 1) &
                                                               + ConstructFD(ConstrNum)%Delx(OutwardMatLayerNum)
                 ENDIF
               ELSE
                 ! later nodes are Delx away from previous
                 ConstructFD(ConstrNum)%NodeXlocation(Ipts1) = ConstructFD(ConstrNum)%NodeXlocation(Ipts1 - 1) &
                                                               + ConstructFD(ConstrNum)%Delx(Layer)
               ENDIF

             ENDDO
           ENDDO
           Layer = Construct(ConstrNum)%TotLayers
           Ipts1 = Ipts1 +1
           ConstructFD(ConstrNum)%NodeXlocation(Ipts1) = ConstructFD(ConstrNum)%NodeXlocation(Ipts1 - 1) &
                                                               + ConstructFD(ConstrNum)%Delx(Layer)
         ENDIF
      ENDDO
    ENDIF

    DO Surf = 1,TotSurfaces
      IF(Surface(Surf)%Construction <= 0) CYCLE  ! Shading surface, not really a heat transfer surface
      ConstrNum=Surface(surf)%Construction
      IF(Construct(ConstrNum)%TypeIsWindow) CYCLE  !  Windows simulated in Window module
      TotNodes = ConstructFD(ConstrNum)%TotNodes

       !Allocate the Surface Arrays
      ALLOCATE(SurfaceFD(Surf)%T(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TOld(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TT(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%Rhov(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%RhovOld(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%RhoT(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TD(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TDT(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TDTLast(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TDOld(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%TDreport(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%RH(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%RHreport(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%EnthOld(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%EnthNew(TotNodes + 1))
      ALLOCATE(SurfaceFD(Surf)%EnthLast(TotNodes + 1))


      !Initialize the allocated arrays.
      SurfaceFD(Surf)%T              = TempInit
      SurfaceFD(Surf)%TOld           = TempInit
      SurfaceFD(Surf)%TT             = TempInit
      SurfaceFD(Surf)%Rhov           = RhovInit
      SurfaceFD(Surf)%RhovOld        = RhovInit
      SurfaceFD(Surf)%RhoT           = RhovInit
      SurfaceFD(Surf)%TD             = TempInit
      SurfaceFD(Surf)%TDT            = TempInit
      SurfaceFD(Surf)%TDTLast        = TempInit
      SurfaceFD(Surf)%TDOld          = TempInit
      SurfaceFD(Surf)%TDreport       = TempInit
      SurfaceFD(Surf)%RH             = 0.0
      SurfaceFD(Surf)%RHreport       = 0.0
      SurfaceFD(Surf)%EnthOld        = EnthInit
      SurfaceFD(Surf)%EnthNew        = EnthInit
      SurfaceFD(Surf)%EnthLast       = EnthInit
    END DO


    DO SurfNum=1,TotSurfaces
      IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
      IF (Surface(SurfNum)%Class == SurfaceClass_Window) CYCLE
 !   If(SolutionAlgo == UseCondFD .or. SolutionAlgo == UseCondFDSimple)Then



      CALL SetupOutputVariable('CondFD Outside Surface Heat Flux [W/m2]',   QFluxOutArrivSurfCond(SurfNum), &
                               'Zone','State',TRIM(Surface(SurfNum)%Name))
      CALL SetupOutputVariable('CondFD Inside Surface Heat Flux [W/m2]',    QFluxInArrivSurfCond(SurfNum), &
                               'Zone','State',TRIM(Surface(SurfNum)%Name))
      CALL SetupOutputVariable('CondFD Outside Heat Flux to Surface [W/m2]',QFluxOutsideToOutSurf(SurfNum), &
                               'Zone','State',TRIM(Surface(SurfNum)%Name))
      CALL SetupOutputVariable('CondFD Inside Heat Flux to Surface [W/m2]', QFluxZoneToInSurf(SurfNum), &
                               'Zone','State',TRIM(Surface(SurfNum)%Name))

      CALL SetupOutputVariable('CondFD Inner Solver Loop Iterations [ ]', SurfaceFD(SurfNum)%GSloopCounter, &
                               'Zone','Sum',TRIM(Surface(SurfNum)%Name))

!       CALL SetupOutputVariable('Source Location Temperature[C]',TempSource(SurfNum),'Zone','State',Trim(Surface(SurfNum)%Name))

      TotNodes = ConstructFD(Surface(SurfNum)%Construction)%TotNodes  ! Full size nodes, start with outside face.
      DO Lay = 1,TotNodes +1  ! include inside face node
        WRITE(LayChar,*)Lay
        CALL SetupOutputVariable('CondFD Nodal Temperature[C]',SurfaceFD(SurfNum)%TDreport(Lay),  &
            'Zone','State', TRIM(Surface(SurfNum)%Name)//" Node #"//TRIM(ADJUSTL(LayChar)))
      END DO

    ENDDO  ! End of the Surface Loop for Report Variable Setup

    OneTimeFlag = .false.
    IF (errorsFound) CALL ShowFatalError('InitHeatBalFiniteDiff: program terminates due to preceding errors.')
  END IF  ! End the One Time Only If Block

  IF (GetInputFlag) THEN
      ! Obtains conduction FD related parameters from input file
    CALL ReportFiniteDiffInits  ! Report the results from the Finite Diff Inits
    GetInputFlag=.false.
  ENDIF
  ! end one time and setup inits

  ! now do begin environment inits.
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    DO SurfNum=1,TotSurfaces
      IF(Surface(SurfNum)%Construction <= 0) CYCLE  ! Shading surface, not really a heat transfer surface
      ConstrNum=Surface(SurfNum)%Construction
      IF(Construct(ConstrNum)%TypeIsWindow) CYCLE  !  Windows simulated in Window module
      SurfaceFD(SurfNum)%T        = TempInit
      SurfaceFD(SurfNum)%TOld     = TempInit
      SurfaceFD(SurfNum)%TT       = TempInit
      SurfaceFD(SurfNum)%Rhov     = RhovInit
      SurfaceFD(SurfNum)%RhovOld  = RhovInit
      SurfaceFD(SurfNum)%RhoT     = RhovInit
      SurfaceFD(SurfNum)%TD       = TempInit
      SurfaceFD(SurfNum)%TDT      = TempInit
      SurfaceFD(SurfNum)%TDTLast  = TempInit
      SurfaceFD(SurfNum)%TDOld    = TempInit
      SurfaceFD(SurfNum)%TDreport = TempInit
      SurfaceFD(SurfNum)%RH       = 0.0
      SurfaceFD(SurfNum)%RHreport = 0.0
      SurfaceFD(SurfNum)%EnthOld  = EnthInit
      SurfaceFD(SurfNum)%EnthNew  = EnthInit
      SurfaceFD(SurfNum)%EnthLast = EnthInit
    ENDDO
    MyEnvrnFlag = .FALSE.
  END IF
  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.TRUE.
  ENDIF

  ! now do every timestep inits

  DO SurfNum=1,TotSurfaces
    IF(Surface(SurfNum)%Construction <= 0) CYCLE  ! Shading surface, not really a heat transfer surface
    ConstrNum=Surface(SurfNum)%Construction
    IF(Construct(ConstrNum)%TypeIsWindow) CYCLE  !  Windows simulated in Window module
    SurfaceFD(SurfNum)%T        = SurfaceFD(SurfNum)%TOld
    SurfaceFD(SurfNum)%Rhov     = SurfaceFD(SurfNum)%RhovOld
    SurfaceFD(SurfNum)%TD       = SurfaceFD(SurfNum)%TDOld
    SurfaceFD(SurfNum)%TDT      = SurfaceFD(SurfNum)%TDreport !PT changes from TDold to TDreport
    SurfaceFD(SurfNum)%TDTLast  = SurfaceFD(SurfNum)%TDOld
    SurfaceFD(SurfNum)%EnthOld  = SurfaceFD(SurfNum)%EnthOld
    SurfaceFD(SurfNum)%EnthNew  = SurfaceFD(SurfNum)%EnthOld
    SurfaceFD(SurfNum)%EnthLast = SurfaceFD(SurfNum)%EnthOld
  ENDDO

  RETURN

END SUBROUTINE InitHeatBalFiniteDiff


SUBROUTINE CalcHeatBalFiniteDiff(Surf,TempSurfInTmp,TempSurfOutTmp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Oct 2003
          !       MODIFIED       Aug 2006 by C O Pedersen to include implicit solution and variable properties with
          !                                material enthalpy added for Phase Change Materials.
          !                      Sept 2010 B. Griffith, remove allocate/deallocate, use structure variables
          !                      March 2011 P. Tabares, add relaxation factor and add surfIteration to
          !                                 update TD and TDT, correct interzone partition
          !                      May 2011  B. Griffith add logging and errors when inner GS loop does not converge

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! this routine controls the calculation of the fluxes and temperatures using
          !      finite difference procedures for
          !      all building surface constructs.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY : RoundSigDigits
  USE DataHeatBalance, ONLY : CondFDRelaxFactor
  USE DataGlobals,     ONLY : KickOffSimulation

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(In)    :: Surf
  REAL(r64), INTENT(InOut) :: TempSurfInTmp       !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64), INTENT(InOut) :: TempSurfOutTmp      !Outside Surface Temperature of each Heat Transfer Surface

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64)  :: HMovInsul           !  Equiv H for TIM layer,  Comes with call to
                                                 ! EvalOutsideMovableInsulation
  INTEGER    :: RoughIndexMovInsul   ! roughness  Movable insulation
  REAL(r64)  :: AbsExt              ! exterior absorptivity  movable insulation

  INTEGER  :: I       !  Node number in construction
  INTEGER  :: J
  INTEGER  :: Lay
  INTEGER  :: ctr
  INTEGER  :: ConstrNum
  INTEGER  :: TotLayers
  INTEGER  :: TotNodes
  INTEGER  :: delt
  INTEGER  :: GSiter !  iteration counter for implicit repeat calculation
  INTEGER, SAVE    :: ErrCount=0
  REAL(r64) :: ErrorSignal
  INTEGER StartingI     ! Starting node number in layer node arrangement
  REAL(r64) :: MaxDelTemp = 0
  INTEGER   :: NodeNum

  ConstrNum=Surface(surf)%Construction

  TotNodes = ConstructFD(ConstrNum)%TotNodes
  TotLayers = Construct(ConstrNum)%TotLayers

  Delt = ConstructFD(ConstrNum)%DeltaTime    !   (seconds)

!  SurfaceFD(Surf)%T        = SurfaceFD(Surf)%TOld
!  SurfaceFD(Surf)%Rhov     = SurfaceFD(Surf)%RhovOld
!  SurfaceFD(Surf)%TD       = SurfaceFD(Surf)%TDreport   !PT changes from TDold to TDreport
!  SurfaceFD(Surf)%TDT      = SurfaceFD(Surf)%TDreport   !PT changes from TDold to TDreport
!  SurfaceFD(Surf)%TDTLast  = SurfaceFD(Surf)%TDOld
!  SurfaceFD(Surf)%EnthOld  = SurfaceFD(Surf)%EnthOld
!  SurfaceFD(Surf)%EnthNew  = SurfaceFD(Surf)%EnthOld
!  SurfaceFD(Surf)%EnthLast = SurfaceFD(Surf)%EnthOld

  CALL EvalOutsideMovableInsulation(Surf,HMovInsul,RoughIndexMovInsul,AbsExt)
 ! Start stepping through the slab with time.
  DO J=1,INT((TimeStepZone*SecInHour)/Delt)

    DO GSiter = 1, MaxGSiter  !  Iterate implicit equations
      SurfaceFD(Surf)%TDTLast = SurfaceFD(Surf)%TDT    !  Save last iteration's TDT (New temperature) values
      SurfaceFD(Surf)%EnthLast = SurfaceFD(Surf)%EnthNew  ! Last iterations new enthalpy value


      IF ( SolutionAlgo == UseCondFD  .or. Construct(ConstrNum)%USEHBAlgorithmCondFDDetailed ) Then  ! full Detailed node solution

!  Original loop version
        I= 1   !  Node counter
        DO Lay = 1, TotLayers    ! Begin layer loop ...

          !For the exterior surface node with a convective boundary condition
          IF(I == 1 .and. Lay ==1)THEN
            CALL ExteriorBCEqns(Delt,I,Lay,Surf,SurfaceFD(Surf)%T, &
                                          SurfaceFD(Surf)%TT, &
                                          SurfaceFD(Surf)%Rhov, &
                                          SurfaceFD(Surf)%RhoT, &
                                          SurfaceFD(Surf)%RH, &
                                          SurfaceFD(Surf)%TD, &
                                          SurfaceFD(Surf)%TDT,&
                                          SurfaceFD(Surf)%EnthOld, &
                                          SurfaceFD(Surf)%EnthNew, &
                                          TotNodes,HMovInsul)
          END IF

          !For the Layer Interior nodes.  Arrive here after exterior surface node or interface node

          IF(TotNodes .ne. 1) THEN

            DO Ctr=2,ConstructFD(ConstrNum)%NodeNumPoint(Lay)
              I=I+1
              CALL InteriorNodeEqns(Delt,I,Lay,Surf,SurfaceFD(Surf)%T, &
                                    SurfaceFD(Surf)%TT, &
                                    SurfaceFD(Surf)%Rhov, &
                                    SurfaceFD(Surf)%RhoT,&
                                    SurfaceFD(Surf)%RH, &
                                    SurfaceFD(Surf)%TD, &
                                    SurfaceFD(Surf)%TDT, &
                                    SurfaceFD(Surf)%EnthOld, &
                                    SurfaceFD(Surf)%EnthNew)
            END DO
          END IF

          IF(Lay < TotLayers .and. TotNodes .ne. 1) THEN
            !Interface equations for 2 capactive materials
            I=I+1
            CALL IntInterfaceNodeEqns(Delt,I,Lay,Surf,SurfaceFD(Surf)%T, &
                                     SurfaceFD(Surf)%TT, &
                                     SurfaceFD(Surf)%Rhov, &
                                     SurfaceFD(Surf)%RhoT, &
                                     SurfaceFD(Surf)%RH, &
                                     SurfaceFD(Surf)%TD, &
                                     SurfaceFD(Surf)%TDT, &
                                     SurfaceFD(Surf)%EnthOld, &
                                     SurfaceFD(Surf)%EnthNew,GSiter)

          ELSE IF (Lay == TotLayers) THEN
            !For the Interior surface node with a convective boundary condition
            I=I+1
            CALL InteriorBCEqns(Delt,I,Lay,Surf, &
                                     SurfaceFD(Surf)%T, &
                                     SurfaceFD(Surf)%TT, &
                                     SurfaceFD(Surf)%Rhov, &
                                     SurfaceFD(Surf)%RhoT, &
                                     SurfaceFD(Surf)%RH, &
                                     SurfaceFD(Surf)%TD, &
                                     SurfaceFD(Surf)%TDT, &
                                     SurfaceFD(Surf)%EnthOld, &
                                     SurfaceFD(Surf)%EnthNew)
          END IF

        END DO    !The end of the layer loop

      ELSE IF (SolutionAlgo == UseCondFDSimple ) Then  !  Do Two node Formulation
        I = 1
        Lay=1  ! not used by Simple CondFD -- left for consistency and avoid crashes.
        CALL ExteriorBCEqns(Delt,I,Lay,Surf,SurfaceFD(Surf)%T,&
                                         SurfaceFD(Surf)%TT, &
                                         SurfaceFD(Surf)%Rhov, &
                                         SurfaceFD(Surf)%RhoT, &
                                         SurfaceFD(Surf)%RH, &
                                         SurfaceFD(Surf)%TD, &
                                         SurfaceFD(Surf)%TDT, &
                                         SurfaceFD(Surf)%EnthOld, &
                                         SurfaceFD(Surf)%EnthNew,TotNodes,HMovInsul)
        I = TotNodes + 1
        CALL InteriorBCEqns(Delt,I,Lay,Surf,SurfaceFD(Surf)%T, &
                                         SurfaceFD(Surf)%TT, &
                                         SurfaceFD(Surf)%Rhov, &
                                         SurfaceFD(Surf)%RhoT, &
                                         SurfaceFD(Surf)%RH, &
                                         SurfaceFD(Surf)%TD, &
                                         SurfaceFD(Surf)%TDT, &
                                         SurfaceFD(Surf)%EnthOld, &
                                         SurfaceFD(Surf)%EnthNew)

      END IF

      ! the following could blow up when all the node temps sum to less than 1.0.  seems poorly formulated for temperature in C.
      IF (Gsiter .gt. 3  .and.ABS(SUM(SurfaceFD(Surf)%TDT-SurfaceFD(Surf)%TDTLast)/SUM(SurfaceFD(Surf)%TDT)) < 0.000001d0 )  EXIT
      SurfaceFD(Surf)%GSloopCounter = Gsiter
!      IF ((GSiter == MaxGSiter) .AND. (SolutionAlgo /= UseCondFDSimple)) THEN ! didn't ever converge
!        IF (.NOT. WarmupFlag .AND. (.NOT. KickOffSimulation)) THEN
!          ErrCount=ErrCount+1
!          ErrorSignal = ABS(SUM(SurfaceFD(Surf)%TDT-SurfaceFD(Surf)%TDTLast)/SUM(SurfaceFD(Surf)%TDT))
!          IF (ErrCount < 10) THEN
!            CALL ShowWarningError('ConductionFiniteDifference inner iteration loop did not converge for surface named ='// &
!                          TRIM(Surface(Surf)%Name) // &
!                          ', with error signal ='//TRIM(RoundSigDigits(ErrorSignal, 8)) // &
!                          ' vs criteria of 0.000001')
!            CALL ShowContinueErrorTimeStamp(' ')
!          ELSE
!            CALL ShowRecurringWarningErrorAtEnd('ConductionFiniteDifference convergence problem continues for surface named ='// &
!                                                TRIM(Surface(Surf)%Name) , &
!                                               SurfaceFD(Surf)%GSloopErrorCount,ReportMaxOf=ErrorSignal,ReportMinOf=ErrorSignal,  &
!                                               ReportMaxUnits='[ ]',ReportMinUnits='[ ]')
!          ENDIF
!
!        ENDIF
!
!      ENDIF

    END DO ! End of Gauss Seidell iteration loop

    IF (CondFDRelaxFactor/=1.0d0) THEN
     !apply Relaxation factor for stability, use current (TDT) and previous (TDreport) temperature values
     !   to obtain the actual temperature that is going to be exported/use
      SurfaceFD(Surf)%TDT = SurfaceFD(Surf)%TDreport+ (SurfaceFD(Surf)%TDT-SurfaceFD(Surf)%TDreport) * CondFDRelaxFactor
      SurfaceFD(Surf)%EnthOld = SurfaceFD(Surf)%EnthNew
    ENDIF

  END DO     !The end of the Time Loop

  TempSurfOutTmp = SurfaceFD(Surf)%TDT(1)
  TempSurfInTmp  = SurfaceFD(Surf)%TDT(TotNodes+1)
  RhoVaporSurfIn(surf) = 0.0

  ! determine largest change in node temps
  MaxDelTemp      = 0.0
  DO NodeNum = 1, TotNodes
    MaxDelTemp = MAX(ABS(SurfaceFD(Surf)%TDT(NodeNum) - SurfaceFD(Surf)%TDreport(NodeNum)), MaxDelTemp)
  ENDDO
  SurfaceFD(Surf)%MaxNodeDelTemp = MaxDelTemp
  SurfaceFD(Surf)%TDreport       = SurfaceFD(Surf)%TDT
  SurfaceFD(Surf)%EnthOld        = SurfaceFD(Surf)%EnthNew

 RETURN


End SUBROUTINE CalcHeatBalFiniteDiff


! Beginning of Reporting subroutines
! *****************************************************************************

SUBROUTINE ReportHeatBalFiniteDiff(Surf)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   September 2000
          !       MODIFIED       B. Griffith, add variable k to calculation, clean up
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the coils.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(In) :: Surf

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: kt
  REAL(r64) :: kto !  base 20 C thermal conductivity
  REAL(r64) :: kt1 !  temperature coefficient for simple temp dep k.
  REAL(r64) :: Delx
  REAL(r64) :: InsideLayerR
  REAL(r64) :: OutsideLayerR
  INTEGER   :: TotNodes
  INTEGER   :: TotLayers
  INTEGER   :: Lay
  INTEGER   :: ConstrNum
  INTEGER   :: DepVarCol
  INTEGER   :: IndVarCol

  ConstrNum = Surface(surf)%Construction

  TotNodes  = ConstructFD(ConstrNum)%TotNodes
  TotLayers = Construct(ConstrNum)%TotLayers

 !Inside Layer Material Properties
  Lay = Construct(ConstrNum)%LayerPoint(TotLayers)
  kto = Material(Lay)%Conductivity
  kt1 = Material(Lay)%tk1  !  linear coefficient (normally zero)
  kt  = kto + kt1*((SurfaceFD(Surf)%TDreport(TotNodes+1)+SurfaceFD(Surf)%TDreport(TotNodes))/2.d0 - 20.0d0)
  IF( SUM(Material(Lay)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function
    DepVarCol = 2  ! thermal conductivity
    IndVarCol = 1  !temperature
    kt = terpld(Size(Material(Lay)%TempCond,1),Material(Lay)%TempCond, &
                  (SurfaceFD(Surf)%TDreport(TotNodes+1)+SurfaceFD(Surf)%TDreport(TotNodes))/2.,IndVarCol,DepVarCol)
  ENDIF

  Delx = ConstructFD(ConstrNum)%Delx(TotLayers)
  InsideLayerR=Material(Lay)%Resistance

  QHeatInFlux(Surf) = kt*(SurfaceFD(Surf)%TDreport(TotNodes+1)-SurfaceFD(Surf)%TDreport(TotNodes))/Delx

  QFluxInArrivSurfCond(Surf) = -kt*(SurfaceFD(Surf)%TDreport(TotNodes+1)-SurfaceFD(Surf)%TDreport(TotNodes))/Delx
  IF (Material(Lay)%Ronly) THEN
    QFluxInArrivSurfCond(Surf)=(SurfaceFD(Surf)%TDreport(TotNodes+1)-SurfaceFD(Surf)%TDreport(TotNodes))/InsideLayerR
  END IF

! Outside Layer Material Properties
  Lay = Construct(ConstrNum)%LayerPoint(1)
  kto = Material(Lay)%Conductivity
  kt1 = Material(Lay)%tk1  !  linear coefficient (normally zero)
  kt  = kto + kt1*((SurfaceFD(Surf)%TDreport(2)+SurfaceFD(Surf)%TDreport(1))/2.d0 - 20.0d0)
  IF( SUM(Material(Lay)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function
    DepVarCol = 2  ! thermal conductivity
    IndVarCol = 1  !temperature
    kt = terpld(Size(Material(Lay)%TempCond,1),Material(Lay)%TempCond, &
                  (SurfaceFD(Surf)%TDreport(1)+SurfaceFD(Surf)%TDreport(2))/2.,IndVarCol,DepVarCol)
  ENDIF

  Delx = ConstructFD(ConstrNum)%Delx(1)
  OutsideLayerR = Material(Lay)%Resistance

  QHeatOutFlux(Surf) = -kt*(SurfaceFD(Surf)%T(2)-SurfaceFD(Surf)%T(1))/Delx

  QFluxOutArrivSurfCond(Surf) = -kt*(SurfaceFD(Surf)%TDreport(2)-SurfaceFD(Surf)%TDreport(1))/Delx
  IF(Material(Lay)%Ronly) THEN
    QFluxOutArrivSurfCond(Surf)=(SurfaceFD(Surf)%TDreport(2)-SurfaceFD(Surf)%TDreport(1))/OutsideLayerR
  END IF


  RETURN
END Subroutine ReportHeatBalFiniteDiff
!        End of Reporting subroutines for the Moisture Balance Module


SUBROUTINE UpdateMoistureBalanceFD(Surf)

    ! SUBROUTINE INFORMATION:
    !   Authors:        Richard Liesen
    !   Date writtenn:  November, 2003
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Update the data structures after the inside surface heat balance has converged.
    ! METHODOLOGY EMPLOYED:
    !

    ! USE STATEMENTS:

    IMPLICIT NONE
    Integer, INTENT(IN)  :: Surf ! Surface number

    INTEGER   :: ConstrNum

    ConstrNum = Surface(surf)%Construction
    SurfaceFD(Surf)%TOld = SurfaceFD(Surf)%T
    SurfaceFD(Surf)%RhovOld = SurfaceFD(Surf)%Rhov
    SurfaceFD(Surf)%TDOld = SurfaceFD(Surf)%TDreport



RETURN

END SUBROUTINE UpdateMoistureBalanceFD



SUBROUTINE ReportFiniteDiffInits

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   November 2003
          !       MODIFIED       B. Griffith, May 2011 add reporting of node x locations
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gives a detailed report to the user about
          ! the initializations for the Fintie Difference calculations
          ! of each construction.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: ScanForReports, RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: DoReport
  CHARACTER(len=MaxNameLength) :: InodesChar
  INTEGER :: ThisNum
  INTEGER :: Layer
  INTEGER :: OutwardMatLayerNum
  INTEGER :: layerNode
  INTEGER :: Inodes

  CALL ScanForReports('Constructions',DoReport,'Constructions')

  IF (DoReport) THEN

!                                      Write Descriptions
    Write(OutputFileInits,'(A)') '! <Construction>,Construction Name,#Layers,#Nodes,Time Step {hours}'
    Write(OutputFileInits,'(A)') '! <Material>,Material Name,Thickness {m},#Layer Elements,Layer Delta X,'//  &
                             'Layer Alpha*Delt/Delx**2,Layer Moisture Stability'
    IF (SolutionAlgo /= UseCondFDSimple) WRITE(OutputFileInits,'(A)') '! <ConductionFiniteDifference Node>,Node Identifier, '// &
        ' Node Distance From Outside Face {m}, Construction Name, Outward Material Name (or Face), Inward Material Name (or Face)'
    DO ThisNum=1,TotConstructs

      IF (Construct(ThisNum)%TypeIsWindow) CYCLE
      IF (Construct(ThisNum)%TypeIsIRT) CYCLE

      Write(OutputFileInits,700) TRIM(Construct(ThisNum)%Name),Construct(ThisNum)%TotLayers,Int(ConstructFD(ThisNum)%TotNodes+1), &
                                 ConstructFD(ThisNum)%DeltaTime/SecInHour
!
 700  FORMAT(' Construction,',A,1(',',I6),',',I6,',',F9.6)
 701  FORMAT(' Material,',A,',',G15.4,',',I4,',',F15.8,',',F15.8,',',F15.8)
 702  FORMAT(' ConductionFiniteDifference Node,', A, ',', A, ',', A, ',', A, ',', A)


      DO Layer=1,Construct(ThisNum)%TotLayers
            Write(OutputFileInits,701) TRIM(ConstructFD(ThisNum)%Name(Layer)),ConstructFD(ThisNum)%Thickness(Layer),      &
                                       ConstructFD(ThisNum)%NodeNumPoint(Layer),ConstructFD(ThisNum)%Delx(Layer),  &
                                       ConstructFD(ThisNum)%TempStability(Layer),ConstructFD(ThisNum)%MoistStability(Layer)
      ENDDO

      !now list each CondFD Node with its X distance from outside face in m along with other identifiers
      Inodes = 0
      IF (SolutionAlgo /= UseCondFDSimple) THEN

        DO Layer=1,Construct(ThisNum)%TotLayers
          OutwardMatLayerNum = Layer - 1
          DO layerNode = 1, ConstructFD(ThisNum)%NodeNumPoint(Layer)
            Inodes = Inodes + 1
            WRITE(InodesChar,*)Inodes
            IF (Inodes == 1) THEN
              WRITE(OutputFileInits,702) TRIM('Node #'//TRIM(ADJUSTL(InodesChar))), &
                                       TRIM(RoundSigDigits(ConstructFD(ThisNum)%NodeXlocation(Inodes), 8)), &
                                       TRIM(Construct(ThisNum)%Name) ,          &
                                       TRIM('Surface Outside Face'),          &
                                       TRIM(ConstructFD(ThisNum)%Name(Layer))

            ELSEIF (layerNode== 1 ) THEN

              IF ( OutwardMatLayerNum > 0 .AND. OutwardMatLayerNum <= Construct(ThisNum)%TotLayers ) THEN
                 WRITE(OutputFileInits,702) TRIM('Node #'//TRIM(ADJUSTL(InodesChar))), &
                                       TRIM(RoundSigDigits(ConstructFD(ThisNum)%NodeXlocation(Inodes), 8)), &
                                       TRIM(Construct(ThisNum)%Name) ,          &
                                       TRIM(ConstructFD(ThisNum)%Name(OutwardMatLayerNum)), &
                                       TRIM(ConstructFD(ThisNum)%Name(Layer))
  !            ELSE
  !               WRITE(OutputFileInits,702) TRIM('Node #'//TRIM(ADJUSTL(InodesChar))), &
  !                                     TRIM(RoundSigDigits(ConstructFD(ThisNum)%NodeXlocation(Inodes), 8)), &
  !                                     TRIM(Construct(ThisNum)%Name) ,          &
  !                                     TRIM('!?WHY DOES IT COME HERE?!'), &
  !                                     TRIM(ConstructFD(ThisNum)%Name(Layer))
              ENDIF
            ELSEIF (layerNode >1) THEN
              OutwardMatLayerNum = Layer
              WRITE(OutputFileInits,702) TRIM('Node #'//TRIM(ADJUSTL(InodesChar))), &
                                       TRIM(RoundSigDigits(ConstructFD(ThisNum)%NodeXlocation(Inodes), 8)), &
                                       TRIM(Construct(ThisNum)%Name) ,          &
                                       TRIM(ConstructFD(ThisNum)%Name(OutwardMatLayerNum)), &
                                       TRIM(ConstructFD(ThisNum)%Name(Layer))
            ENDIF

          ENDDO
        ENDDO

        Layer = Construct(ThisNum)%TotLayers
        Inodes = Inodes + 1
        WRITE(InodesChar,*)Inodes
        WRITE(OutputFileInits,702) TRIM('Node #'//TRIM(ADJUSTL(InodesChar))), &
                                   TRIM(RoundSigDigits(ConstructFD(ThisNum)%NodeXlocation(Inodes), 8)), &
                                   TRIM(Construct(ThisNum)%Name) ,          &
                                   TRIM(ConstructFD(ThisNum)%Name(Layer)), &
                                   TRIM('Surface Inside Face')
      ENDIF ! detailed CondFD
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE ReportFiniteDiffInits

! Equation Types of the Module
!******************************************************************************
!******************************************************************************
SUBROUTINE ExteriorBCEqns(Delt,I,Lay,Surf,T,TT,Rhov,RhoT,RH,TD,TDT,EnthOld,EnthNew,TotNodes,HMovInsul)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   November, 2003
          !       MODIFIED       B. Griffith 2010, fix adiabatic and other side surfaces
          !                      May 2011, B. Griffith, P. Tabares
          !       RE-ENGINEERED  Curtis Pedersen 2006

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces,          ONLY : OtherSideCondModeledExt, OSCM
  USE DataHeatBalSurface  ,  ONLY : QdotRadOutRepPerArea, QdotRadOutRep, QRadOutReport
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Delt  ! Time Increment
  INTEGER, INTENT(IN)  :: I     ! Node Index
  INTEGER, INTENT(IN)  :: Lay   ! Layer Number for Construction
  INTEGER, INTENT(IN)  :: Surf  ! Surface number
  INTEGER, INTENT(IN)  :: TotNodes !  Total nodes in layer
  REAL(r64),DIMENSION(:), INTENT(In)    :: T     !Old node Temperature in MFD finite difference solution
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TT    !New node Temperature in MFD finite difference solution.
  REAL(r64),DIMENSION(:), INTENT(In)    :: Rhov  !MFD Nodal Vapor Density[kg/m3] and is the old or last time step result.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RhoT  !MFD vapor density for the new time step.
  REAL(r64),DIMENSION(:), INTENT(In)    :: TD    !The old dry Temperature at each node for the CondFD algorithm..
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TDT   !The current or new Temperature at each node location for the CondFD solution..
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RH    !Nodal relative humidity
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthOld    ! Old Nodal enthalpy
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthNew    ! New Nodal enthalpy

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: QRadSWOutFD   !Short wave radiation absorbed on outside of opaque surface
  REAL(r64) :: Delx
  INTEGER   :: ConstrNum
  INTEGER   :: MatLay
  INTEGER   :: IndVarCol
  INTEGER   :: DepVarCol
  REAL(r64) :: hconvo
  REAL(r64) :: kt  ! temperature dependent thermal conductivity,  kt=ko +kt1(T-20)
  REAL(r64) :: kto  ! Base 20C thermal conductivity
  REAL(r64) :: kt1 ! Thermal conductivity gradient coefficient where: kt=ko +kt1(T-20)
  REAL(r64) :: Cpo ! Specific heat from idf
  REAL(r64) :: Cp  ! specific heat modified if PCM, otherwise equal to Cpo
  REAL(r64) :: RhoS

  REAL(r64) :: Toa
  REAL(r64) :: Rhovo
  REAL(r64) :: hmasso

  REAL(r64) :: hgnd
  REAL(r64) :: hrad
  REAL(r64) :: hsky
  REAL(r64) :: Tgnd
  REAL(r64) :: Tsky
  REAL(r64) :: Tia
  REAL(r64) :: SigmaRLoc
  REAL(r64) :: SigmaCLoc
  REAL(r64) :: Rlayer
  REAL(r64) :: QNetSurfFromOutside  !  Combined outside surface net heat transfer terms
  REAL(r64) :: TInsulOut  ! Temperature of outisde face of Outside Insulation
  REAL(r64) :: HMovInsul  !  Conductance of movable(transparent) insulation.
  REAL(r64) :: QRadSWOutMvInsulFD  ! SW radiation at outside of Movable Insulation
  INTEGER   :: LayIn  ! layer number for call to interior eqs
  INTEGER   :: NodeIn ! node number "I" for call to interior eqs

  ConstrNum=Surface(surf)%Construction

!Boundary Conditions from Simulation for Exterior
  hconvo = HConvExtFD(surf)
  hmasso = HMassConvExtFD(surf)

  hrad = HAirFD(surf)
  hsky = HSkyFD(surf)
  hgnd = HGrndFD(surf)

  Toa = TempOutsideAirFD(surf)
  rhovo = RhoVaporAirOut(surf)
  Tgnd = TempOutsideAirFD(surf)
  IF (Surface(Surf)%ExtBoundCond == OtherSideCondModeledExt) THEN
   !CR8046 switch modeled rad temp for sky temp.
    TSky = OSCM(Surface(Surf)%OSCMPtr)%TRad
    QRadSWOutFD = 0.0D0 ! eliminate incident shortwave on underlying surface

  ELSE
  !Set the external conditions to local variables
    QRadSWOutFD = QRadSWOutAbs(surf)
    QRadSWOutMvInsulFD = QRadSWOutMvIns(Surf)
    TSky = SkyTemp
  ENDIF
  Tia = Mat(Surface(Surf)%Zone)
  SigmaRLoc=SigmaR(ConstrNum)
  SigmaCLoc=SigmaC(ConstrNum)

  MatLay = Construct(ConstrNum)%LayerPoint(Lay)

  IF(Surface(Surf)%ExtBoundCond == Ground .or. IsRain)THEN
    TDT(I) = Toa
    TT(I) = Toa
    RhoT(I)= rhovo
  ELSEIF (Surface(Surf)%ExtBoundCond > 0) THEN
   ! this is actually the inside face of another surface, or maybe this same surface if adiabatic
   ! switch around arguments for the other surf and call routines as for interior side BC from opposite face

    LayIn = Construct(Surface(Surface(Surf)%ExtBoundCond)%Construction)%TotLayers
    nodeIn = ConstructFD(Surface(Surface(Surf)%ExtBoundCond)%Construction)%TotNodes + 1

    CALL InteriorBCEqns(Delt,nodeIn,LayIn,Surface(Surf)%ExtBoundCond,SurfaceFD(Surface(Surf)%ExtBoundCond)%T, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%TT, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%Rhov, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%RhoT, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%RH, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%TD, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%TDT, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%EnthOld, &
                                         SurfaceFD(Surface(Surf)%ExtBoundCond)%EnthNew)
!
!    CALL InteriorBCEqns(Delt,nodeIn,Layin,Surface(Surf)%ExtBoundCond,SurfaceFD(Surf)%T, &
!                                         SurfaceFD(Surf)%TT, &
!                                         SurfaceFD(Surf)%Rhov, &
!                                         SurfaceFD(Surf)%RhoT, &
!                                         SurfaceFD(Surf)%RH, &
!                                         SurfaceFD(Surf)%TD, &
!                                         SurfaceFD(Surf)%TDT, &
!                                         SurfaceFD(Surf)%EnthOld, &
!                                         SurfaceFD(Surf)%EnthNew)

    ! now fill results from interior BC model eqns into local result for current call
    TDT(I)  = SurfaceFD(Surface(Surf)%ExtBoundCond)%TDT(TotNodes + 1)
    TT(I)   = SurfaceFD(Surface(Surf)%ExtBoundCond)%TT(TotNodes + 1)
    RhoT(I) = SurfaceFD(Surface(Surf)%ExtBoundCond)%RhoT(TotNodes + 1)
!    TDT(I)  = SurfaceFD(Surf)%TDT( i)
!    TT(I)   = SurfaceFD(Surf)%TT( i)
!    RhoT(I) = SurfaceFD(Surf)%RhoT( i)

    QNetSurfFromOutside = OpaqSurfInsFaceConductionFlux(Surface(Surf)%ExtBoundCond) !filled in InteriorBCEqns
    QFluxOutsideToOutSurf(Surf)       = QnetSurfFromOutside
    OpaqSurfOutsideFaceConductionFlux(Surf)= QnetSurfFromOutside
    QHeatOutFlux(Surf)  = QnetSurfFromOutside

  ELSEIF (Surface(Surf)%ExtBoundCond <= 0) THEN   ! regular outside conditions

!++++++++++++++++++++++++++++++++++++++++++++++++++++++
 !    Do detailed FD for  the surface   Else will switch to SigmaR,SigmaC
    IF ( SolutionAlgo == UseCondFD  .or. Construct(ConstrNum)%USEHBAlgorithmCondFDDetailed ) THEN
                      ! Use detailed CondFD  when specified or
                      !  when overridden by special construction field

    ! regular outside conditions

     !  Set Thermal Conductivity.  Can be constant, simple linear temp dep or multiple linear segment temp function dep.


      kto = Material(MatLay)%Conductivity   !  20C base conductivity
      kt1 = Material(MatLay)%tk1  !  linear coefficient (normally zero)
      kt= kto + kt1*((TDT(I)+TDT(I+1))/2.d0 - 20.d0)

      IF( SUM(Material(MatLay)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function

        DepVarCol= 2  ! thermal conductivity
        IndVarCol=1  !temperature
            !  Use average temp of surface and first node for k
        kt =terpld(SIZE(Material(MatLay)%TempCond,1),Material(MatLay)%TempCond,(TDT(I)+TDT(I+1))/2.,IndVarCol,DepVarCol)

      END IF


      RhoS = Material(MatLay)%Density
      Cpo = Material(MatLay)%SpecHeat
      Cp=Cpo  !  Will be changed if PCM
      Delx = ConstructFD(ConstrNum)%Delx(Lay)

          !Calculate the Dry Heat Conduction Equation

      MatLay = Construct(ConstrNum)%LayerPoint(Lay)
      IF (Material(MatLay)%ROnly .or. Material(MatLay)%Group == 1 ) THEN  ! R Layer or Air Layer  **********
               !  Use algebraic equation for TDT based on R

        Rlayer = Material(MatLay)%Resistance

        TDT(I)=(QRadSWOutFD*Rlayer + TDT(I+1) + hgnd*Rlayer*Tgnd + &
                   hconvo*Rlayer*Toa + hrad*Rlayer*Toa +  &
                   hsky*Rlayer*Tsky)/  &
                   (1 + hconvo*Rlayer + hgnd*Rlayer + hrad*Rlayer + &
                    hsky*Rlayer)
        TDT(I) = MAX(MinSurfaceTempLimit,MIN(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

      ELSE  ! Regular or phase change material layer

             !  check for phase change material
        IF( SUM(Material(MatLay)%TempEnth(1:3,2)) >= 0.) THEN    !  phase change material,  Use TempEnth Data to generate Cp
!               CheckhT = Material(MatLay)%TempEnth       ! debug

          !       Enthalpy function used to get average specific heat.  Updated by GS so enthalpy function is followed.

          DepVarCol= 2  ! enthalpy
          IndVarCol=1  !temperature
          EnthOld(I) =terpld(SIZE(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TD(I),IndVarCol,DepVarCol)
          EnthNew(I) =terpld(SIZE(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TDT(I),IndVarCol,DepVarCol)
          IF (EnthNew(I)==EnthOld(I))  THEN
            Cp=Cpo
          ELSE
            Cp=MAX(Cpo,(EnthNew(I) -EnthOld(I))/(TDT(I)-TD(I)))
          END IF

        ELSE
          Cp = Cpo

        END IF ! Phase Change Material option

!     Choose Regular or Transparent Insulation Case

        IF(HMovInsul <= 0. ) THEN  !  regular  case

          SELECT CASE (CondFDSchemeType)
          CASE (CrankNicholsonSecondOrder)
             !  Second Order equation
            TDT(I)= (1.*QRadSWOutFD + (0.5d0*Cp*Delx*RhoS*TD(I))/DelT + (0.5*kt*(-1.*TD(I) + TD(I+1)))/Delx  &
                     + (0.5d0*kt*TDT(I+1))/Delx + 0.5d0*hgnd*Tgnd + 0.5d0*hgnd*(-1.*TD(I) + Tgnd) + 0.5d0*hconvo*Toa +   &
                        0.5d0*hrad*Toa  &
                     + 0.5d0*hconvo*(-1.d0*TD(I) + Toa) + 0.5d0*hrad*(-1.d0*TD(I) + Toa) + 0.5d0*hsky*Tsky +   &
                        0.5d0*hsky*(-1.d0*TD(I) + Tsky))/  &
                       (0.5d0*hconvo + 0.5d0*hgnd + 0.5d0*hrad + 0.5d0*hsky + (0.5d0*kt)/Delx + (0.5d0*Cp*Delx*RhoS)/DelT)

          CASE (FullyImplicitFirstOrder)
           !   First Order
            TDT(I)=(2*Delt*Delx*QRadSWOutFD + Cp*Delx**2*RhoS*TD(I) &
                    + 2*Delt*kt*TDT(I+1) + 2*Delt*Delx*hgnd*Tgnd  &
                    + 2*Delt*Delx*hconvo*Toa + 2*Delt*Delx*hrad*Toa + 2*Delt*Delx*hsky*Tsky)/  &
                     (2*Delt*Delx*hconvo + 2*Delt*Delx*hgnd + 2*Delt*Delx*hrad +  &
                       2*Delt*Delx*hsky + 2*Delt*kt + Cp*Delx**2*RhoS)

          END SELECT


        ELSE IF ( HMovInsul > 0.) Then      !  Transparent insulation on outside
      !  Transparent insulaton additions

        !Movable Insulation Layer Outside surface temp

          TInsulOut = (QRadSWOutMvInsulFD + hgnd*Tgnd + HmovInsul*TDT(I) + &
                   hconvo*Toa + hrad*Toa + hsky*Tsky)/  &
                 (hconvo + hgnd + HmovInsul + hrad + hsky)

          !List(List(Rule(TDT,(2*Delt*Delx*QradSWOutAbs +
          !-       Cp*Delx**2*Rhos*TD + 2*Delt*kt*TDTP1 +
          !-       2*Delt*Delx*HmovInsul*Tiso)/
          !-     (2*Delt*Delx*HmovInsul + 2*Delt*kt + Cp*Delx**2*Rhos))))


         ! Wall first node temperature behind Movable insulation
          SELECT CASE (CondFDSchemeType)
          CASE (CrankNicholsonSecondOrder)
            TDT(I)= (2*Delt*Delx*QradSWOutFD +  &
                     Cp*Delx**2*Rhos*TD(I) + 2*Delt*kt*TDT(I+1) +  &
                      2*Delt*Delx*HmovInsul*TInsulOut)/  &
                       (2*Delt*Delx*HmovInsul + 2*Delt*kt + Cp*Delx**2*Rhos)

          CASE (FullyImplicitFirstOrder)
            ! Currently same as Crank Nicholson, need fully implicit formulation
            TDT(I)= (2*Delt*Delx*QradSWOutFD +  &
                     Cp*Delx**2*Rhos*TD(I) + 2*Delt*kt*TDT(I+1) +  &
                      2*Delt*Delx*HmovInsul*TInsulOut)/  &
                       (2*Delt*Delx*HmovInsul + 2*Delt*kt + Cp*Delx**2*Rhos)

          END SELECT

        End IF   !  Regular layer or Movable insulation cases

        TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check


      END IF  ! R layer or Regular layer


    ELSE IF (SolutionAlgo == UseCondFDSimple) Then    !  Do sigmaR SigmaC approximation. This is CondFDSimple

!      List(List(Rule(TDT,(1.*QRadSWOutMFD + (0.5*SigmaC*TD)/DelT + (0.5*(-1.*TD + TDP1))/SigmaR + (0.5*TDTP1)/SigmaR + 0.5*hgnd*Tgnd +
!     -       0.5*hgnd*(-1.*TD + Tgnd) + 0.5*hconvo*Toa + 0.5*hrad*Toa + 0.5*hconvo*(-1.*TD + Toa) + 0.5*hrad*(-1.*TD + Toa) + 0.5*hsky*Tsky +
!     -       0.5*hsky*(-1.*TD + Tsky))/(0.5*hconvo + 0.5*hgnd + 0.5*hrad + 0.5*hsky + (0.5*SigmaC)/DelT + 0.5/SigmaR))))


!   TDT(I) = (1.*QRadSWOutFD + (0.5*SigmaCLoc*TD(I))/Delt + (0.5*(-1.*TD(I) + TD(I+1)))/SigmaRLoc + (0.5*TDT(I+1))/SigmaRLoc + 0.5*hgnd*Tgnd + &
!           0.5*hgnd*(-1.*TD(I) + Tgnd) + 0.5*hconvo*Toa + 0.5*hrad*Toa + 0.5*hconvo*(-1.*TD(I) + Toa) + 0.5*hrad*(-1.*TD(I) + Toa) + 0.5*hsky*Tsky +  &
!           0.5*hsky*(-1.*TD(I) + Tsky))/(0.5*hconvo + 0.5*hgnd + 0.5*hrad + 0.5*hsky + (0.5*SigmaCLoc)/Delt + 0.5/SigmaRLoc)

   !  First Order AM
! (2*DelT*QRadSWOutMFD*SigmaR + SigmaC*SigmaR*TD + 2*DelT*TDTP1 + 2*DelT*hgnd*SigmaR*Tgnd + 2*DelT*hconvo*SigmaR*Toa + 2*DelT*hrad*SigmaR*Toa + 2*DelT*hsky*SigmaR*Tsky)/
 !    -     (2*DelT + 2*DelT*hconvo*SigmaR + 2*DelT*hgnd*SigmaR + 2*DelT*hrad*SigmaR + 2*DelT*hsky*SigmaR + SigmaC*SigmaR))


      TDT(I)= (2.d0*delt*QRadSWOutFD*SigmaRLoc + SigmaCLoc*SigmaRLoc*TD(I) +   &
             2.d0*delt*TDT(I+1) + 2.d0*delt*hgnd*SigmaRLoc*Tgnd + 2.d0*delt*hconvo*SigmaRLoc*Toa +  &
             2.d0*delt*hrad*SigmaRLoc*Toa + 2.d0*delt*hsky*SigmaRLoc*Tsky)/  &
            (2.d0*delt + 2.d0*delt*hconvo*SigmaRLoc + 2*delt*hgnd*SigmaRLoc +   &
             2.d0*delt*hrad*SigmaRLoc + 2.d0*delt*hsky*SigmaRLoc + SigmaCLoc*SigmaRLoc)

      TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check


    END IF   !End IF--ELSE SECTION (regular detailed FD part or SigmaR SigmaC part

  !  Determine net heat flux to ooutside face.
!        QNetSurfaceFromOutisde.eq.
!     -  QRadSWOutMFD + 0.5*(hgnd*(-TD + Tgnd) + hgnd*(-TDT + Tgnd) + hconvo*(-TD + Toa) + hrad*(-TD + Toa) +
!     -      hconvo*(-TDT + Toa) + hrad*(-TDT + Toa) + hsky*(-TD + Tsky) + hsky*(-TDT + Tsky))
    SELECT CASE (CondFDSchemeType)
    CASE (CrankNicholsonSecondOrder)
      !AM 2o order
      QNetSurfFromOutside = QRadSWOutFD + 0.5d0*(hgnd*(-TD(I) + Tgnd) + hgnd*(-TDT(I) + Tgnd) +  &
             hconvo*(-TD(I) + Toa) + hrad*(-TD(I) + Toa) + hconvo*(-TDT(I) + Toa) + &
             hrad*(-TDT(I) + Toa) + hsky*(-TD(I) + Tsky) + hsky*(-TDT(I) + Tsky))
    CASE (FullyImplicitFirstOrder)
      !AM first order
      QNetSurfFromOutside = QRadSWOutFD + (hgnd*(-TDT(I) + Tgnd) +  &
             hconvo*(-TDT(I) + Toa) + hrad*(-TDT(I) + Toa) + hsky*(-TDT(I) + Tsky))
    END SELECT
    QFluxOutsideToOutSurf(Surf) = QnetSurfFromOutside
    OpaqSurfOutsideFaceConductionFlux(Surf)= QnetSurfFromOutside
    QHeatOutFlux(Surf)= QnetSurfFromOutside
    QdotRadOutRepPerArea(Surf) = (hgnd*(TDT(I) - Tgnd) +  hrad*(TDT(I) - Toa) + hsky*( TDT(I) - Tsky))
    QdotRadOutRep(Surf)        = Surface(Surf)%Area * (hgnd*(TDT(I) - Tgnd) +  hrad*(TDT(I) - Toa) + hsky*( TDT(I) - Tsky))
    QRadOutReport(Surf)     = QdotRadOutRep(Surf) * SecInHour * TimeStepZone
  END IF  !End IF --ELSE SECTION (regular BC part of the ground and Rain check)


RETURN

END SUBROUTINE ExteriorBCEqns

REAL(r64) Function terpld(N,a,x1,nind,ndep)
    !
    !author:c. o. pedersen
    !
    !purpose:
    !   this function performs a linear interpolation
    !     on a two dimensional array containing both
    !     dependent and independent variables.



    !inputs:
    !  a = two dimensional array
    !  nind=column containing independent variable
    !  ndep=column containing the dependent variable
    !   x1 = specific independent variable value for which
    !      interpolated oputput is wanted
    !outputs:
    !    the value of dependent variable corresponding
    !       to x1
    !    routine returns first or last dependent variable
    !      for out of range x1.
    !
     INTEGER, intent (IN)  :: N
     REAL(r64), DIMENSION(N,2),intent (IN)     :: a
     INTEGER, intent (IN)   :: nind
     INTEGER, intent (IN)   :: ndep
     REAL(r64), intent (IN)     :: x1
     INTEGER :: npts,first,last,i1,i2,i,irange
     INTEGER, DIMENSION(2) :: MaxLocArray
     REAL(r64) ::  fract
     npts = size(a,1)
     first=lbound(a,1)
     MaxLocArray=MaxLoc(a,1)
     last = MaxLocArray(1)
     IF ( npts ==1 .or. x1 <= a(first,nind) )then
       terpld=a(first,ndep)
     ELSEIF ( x1>= a(last,nind))then
       terpld=a(last,ndep)
     ELSE
       i1=first
       i2=last
       DO while ((i2-i1) > 1)
         irange=i2-i1
         i=i1+irange/2
         IF ( x1 < a(i,nind)) then
           i2=i
         ELSE
           i1=i
         END IF
      End DO
      i=i2
      fract=(x1-a(i-1,nind))/(a(i,nind)-a(i-1,nind))
      terpld=a(i-1,ndep)+fract*(a(i,ndep)-a(i-1,ndep))
    END IF
END Function


SUBROUTINE InteriorNodeEqns(Delt,I,Lay,Surf,T,TT,Rhov,RhoT,RH,TD,TDT,EnthOld,EnthNew)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   November, 2003
          !       MODIFIED       May 2011, B. Griffith and P. Tabares
          !       RE-ENGINEERED  C. O. Pedersen, 2006

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Delt  ! Time Increment
  INTEGER, INTENT(IN)  :: I     ! Node Index
  INTEGER, INTENT(IN)  :: Lay   ! Layer Number for Construction
  INTEGER, INTENT(IN)  :: Surf  ! Surface number
  REAL(r64),DIMENSION(:), INTENT(In)    :: T     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TT    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(In)    :: Rhov     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RhoT  !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(In)    :: TD     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TDT    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RH    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthOld    ! Old Nodal enthalpy
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthNew    ! New Nodal enthalpy


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: NinetyNine=99.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  REAL(r64) :: Delx

  INTEGER :: ConstrNum
  INTEGER :: MatLay
  INTEGER :: DepVarCol
  INTEGER :: IndVarCol

  REAL(r64) :: kt  !  Thermal conductivity in temperature equation
  REAL(r64) :: ktA1  !  Variable Outer Thermal conductivity in temperature equation
  REAL(r64) :: ktA2  !  Thermal Inner conductivity in temperature equation
  REAL(r64) :: kto !  base 20 C thermal conductivity
  REAL(r64) :: kt1 !  temperature coefficient for simple temp dep k.
  REAL(r64) :: Cp   !  Cp used
  REAL(r64) :: Cpo  !  Const Cp from input
  REAL(r64) :: RhoS

  ConstrNum=Surface(surf)%Construction

  MatLay = Construct(ConstrNum)%LayerPoint(Lay)

   !  Set Thermal Conductivity.  Can be constant, simple linear temp dep or multiple linear segment temp function dep.
  kto = Material(MatLay)%Conductivity   !  20C base conductivity
  kt1 = Material(MatLay)%tk1  !  linear coefficient (normally zero)
  kt  = kto + kt1*((TDT(I)+TDT(I-1))/2.d0 - 20.0d0)
  ktA1 = kto + kt1*((TDT(I)+TDT(I+1))/2.d0 - 20.0d0)   ! Will be overridden if variable k
  ktA2 = kto + kt1*((TDT(I)+TDT(I-1))/2.d0 - 20.0d0)   ! Will be overridden if variable k

  IF( SUM(Material(MatLay)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function

    DepVarCol = 2  ! thermal conductivity
    IndVarCol = 1  !temperature
    ktA1 = terpld(Size(Material(MatLay)%TempCond,1),Material(MatLay)%TempCond,(TDT(I)+TDT(I+1))/2.,IndVarCol,DepVarCol)
    ktA2 = terpld(Size(Material(MatLay)%TempCond,1),Material(MatLay)%TempCond,(TDT(I)+TDT(I-1))/2.,IndVarCol,DepVarCol)

  END IF

  RhoS = Material(MatLay)%Density
  Cpo  = Material(MatLay)%SpecHeat
  Cp   = Cpo  ! Will be changed if PCM
  Delx = ConstructFD(ConstrNum)%Delx(Lay)

  IF( SUM(Material(MatLay)%TempEnth(1:3,2)) >= 0.) THEN    !  phase change material,  Use TempEnth Data

    DepVarCol = 2  ! enthalpy

    IndVarCol = 1  !temperature

    EnthOld(I) =terpld(SIZE(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TD(I),IndVarCol,DepVarCol)

    EnthNew(I) =terpld(SIZE(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TDT(I),IndVarCol,DepVarCol)
    IF (EnthNew(I)==EnthOld(I))  THEN
      Cp = Cpo
    ELSE
      Cp = MAX(Cpo,(EnthNew(I) -EnthOld(I))/(TDT(I)-TD(I)))
    END IF

  ELSE  ! No phase change

    Cp = Cpo

  END IF ! Phase Change case

  SELECT CASE (CondFDSchemeType)

  CASE (CrankNicholsonSecondOrder)
    ! Adams-Moulton second order
    TDT(I)= ((Cp*Delx*RhoS*TD(I))/Delt +   &
              0.5d0*((ktA2*(-1.*TD(I) + TD(I-1)))/Delx + (ktA1*(-1.d0*TD(I) + TD(I+1)))/Delx) +   &
              (0.5d0*ktA2*TDT(I-1))/Delx + (0.5d0*ktA1*TDT(I+1))/Delx)/  &
              ((0.5d0*(ktA1+ktA2))/Delx + (Cp*Delx*RhoS)/Delt)

  CASE (FullyImplicitFirstOrder)
    ! Adams-Moulton First order
        TDT(I)= ((Cp*Delx*RhoS*TD(I))/Delt +   &
                 (1.d0*ktA2*TDT(I-1))/Delx + (1.d0*ktA1*TDT(I+1))/Delx)/  &
                 ((2.d0*(ktA1+ktA2)/2.d0)/Delx + (Cp*Delx*RhoS)/Delt)


  END SELECT

  TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

RETURN

END SUBROUTINE InteriorNodeEqns


SUBROUTINE IntInterfaceNodeEqns(Delt,I,Lay,Surf,T,TT,Rhov,RhoT,RH,TD,TDT,EnthOld,EnthNew,GSiter)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   November, 2003
          !       MODIFIED       May 2011, B. Griffith, P. Tabares,  add first order fully implicit, bug fixes, cleanup
          !       RE-ENGINEERED  Curtis Pedersen, Changed to Implit mode and included enthalpy.  FY2006

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate finite difference heat transfer for nodes that interface two different material layers inside construction

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN)  :: Delt  ! Time Increment
  INTEGER, INTENT(IN)  :: I     ! Node Index
  INTEGER, INTENT(IN)  :: Lay   ! Layer Number for Construction
  INTEGER, INTENT(IN)  :: Surf  ! Surface number
  INTEGER, INTENT(IN)  :: GSiter  ! Iteration number of Gauss Seidell iteration
  REAL(r64),DIMENSION(:), INTENT(In)    :: T     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TT    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(In)    :: Rhov     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RhoT  !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(In)    :: TD     !OLD NODE TEMPERATURES OF EACH HEAT TRANSFER SURF IN CONDFD.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TDT    !NEW NODE TEMPERATURES OF EACH HEAT TRANSFER SURF IN CONDFD.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RH    !RELATIVE HUMIDITY.
  REAL(r64),DIMENSION(:), INTENT(In)    :: EnthOld    ! Old Nodal enthalpy
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthNew    ! New Nodal enthalpy

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: NinetyNine=99.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: ConstrNum
  INTEGER :: MatLay
  INTEGER :: MatLay2
  INTEGER :: IndVarCol
  INTEGER :: DepVarCol

  REAL(r64) :: kt1
  REAL(r64) :: kt2
  REAL(r64) :: kt1o
  REAL(r64) :: kt2o
  REAL(r64) :: kt11
  REAL(r64) :: kt21
  REAL(r64) :: Cp1
  REAL(r64) :: Cp2
  REAL(r64) :: Cpo1
  REAL(r64) :: Cpo2
  REAL(r64) :: RhoS1
  REAL(r64) :: RhoS2

  REAL(r64) :: Delx1
  REAL(r64) :: Delx2
  REAL(r64) :: Enth1New
  REAL(r64) :: Enth2New
  REAL(r64) :: Enth1Old
  REAL(r64) :: Enth2Old

  REAL(r64) :: Rlayer   !  resistance value of R Layer
  REAL(r64) :: Rlayer2  !  resistance value of next layer to inside
  REAL(r64) :: QSSFlux  !  Source/Sink flux value at a layer interface
  LOGICAL   :: RlayerPresent  = .FALSE.
  LOGICAL   :: RLayer2Present = .FALSE.

  ConstrNum=Surface(surf)%Construction

  MatLay = Construct(ConstrNum)%LayerPoint(Lay)
  MatLay2 = Construct(ConstrNum)%LayerPoint(Lay+1)
  !  Set Thermal Conductivity.  Can be constant, simple linear temp dep or multiple linear segment temp function dep.

  kt1o = Material(MatLay)%Conductivity
  kt11=  Material(MatLay)%tk1
  kt1= kt1o + kt11*((TDT(I)+TDT(I-1))/2.d0 -20.d0)

  IF( SUM(Material(MatLay)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function

    DepVarCol = 2  ! thermal conductivity
    IndVarCol = 1  !temperature
    kt1       = terpld(Size(Material(MatLay)%TempCond,1),Material(MatLay)%TempCond,(TDT(I)+TDT(I-1))/2.d0,IndVarCol,DepVarCol)

  END IF

  RhoS1 = Material(MatLay)%Density
  Cpo1 = Material(MatLay)%SpecHeat   ! constant Cp from input file
  Delx1 = ConstructFD(ConstrNum)%Delx(Lay)
  Rlayer = Material(MatLay)%Resistance

  kt2o = Material(MatLay2)%Conductivity
  kt21=  Material(MatLay2)%tk1
  kt2= kt2o + kt21*((TDT(I)+TDT(I+1))/2.d0 -20.d0)

  IF( SUM(Material(MatLay2)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function

    DepVarCol= 2  ! thermal conductivity
    IndVarCol=1  !temperature
    kt2 =terpld(Size(Material(MatLay2)%TempCond,1),Material(MatLay2)%TempCond,(TDT(I)+TDT(I+1))/2.d0,IndVarCol,DepVarCol)

  END IF

  RhoS2 = Material(MatLay2)%Density
  Cpo2 = Material(MatLay2)%SpecHeat
  Delx2 = ConstructFD(ConstrNum)%Delx(Lay+1)
  Rlayer2 = Material(MatLay2)%Resistance
  Cp1=Cpo1   !  Will be reset if PCM
  Cp2=Cpo2   !  will be reset if PCM

  IF ( SolutionAlgo == UseCondFD  .or. Construct(ConstrNum)%USEHBAlgorithmCondFDDetailed )THEN
              !Calculate the Dry Heat Conduction Equation
    RLayerPresent = .FALSE.
    RLayer2Present = .false.

!     Source/Sink Flux Capability ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    QSSFlux = 0.0
    IF (Surface(Surf)%Area >0.0 .and.   &
        Construct(ConstrNum)%SourceSinkPresent .and. Lay == Construct(ConstrNum)%SourceAfterLayer) then

      QSSFlux = QRadSysSource(Surf)/Surface(Surf)%Area   &
                 + QPVSysSource(Surf)/Surface(Surf)%Area     ! Includes QPV Source

    END IF

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    IF (Material(MatLay)%ROnly .or. Material(MatLay)%Group == 1)  RLayerPresent = .TRUE.

    IF (Material(MatLay2)%ROnly .or. Material(MatLay2)%Group ==1) Rlayer2Present =.TRUE.

    IF ( RLayerPresent .and. RLayer2Present) THEN
      TDT(I)=(Rlayer2*TDT(I-1) + Rlayer*TDT(I+1))/(Rlayer + Rlayer2)   ! two adjacent R layers

    ELSE IF ( RLayerPresent .and. .not. RLayer2Present  ) THEN  ! R-layer first

     !  Check for PCM second layer

      IF( Sum(Material(MatLay)%TempEnth(1:3,2)) < 0.  &
               .and. Sum(Material(MatLay2)%TempEnth(1:3,2)) > 0.) THEN    !  phase change material Layer2,  Use TempEnth Data

        IndVarCol= 1
        DepVarCol=2
        Enth2Old = terpld(Size(Material(MatLay2)%TempEnth,1),Material(MatLay2)%TempEnth,TD(I),IndVarCol,DepVarCol)
        Enth2New = terpld(Size(Material(MatLay2)%TempEnth,1),Material(MatLay2)%TempEnth,TDT(I),IndVarCol,DepVarCol)
        EnthNew(I) = Enth2New  !  This node really doesn't have an enthalpy, this gives it a value

        IF (Enth2New==Enth2Old)  THEN
          Cp2 = Cpo2
        ELSE
          Cp2 = MAX(Cpo2,(Enth2New -Enth2Old)/(TDT(I)-TD(I)))
        END IF
      END IF

         ! R layer first, then PCM or regular layer.
      SELECT CASE (CondFDSchemeType)
      CASE (CrankNicholsonSecondOrder)

        TDT(I) =( 2.d0*delt*Delx2*QSSFlux*Rlayer - delt*Delx2*TD(I) - delt*kt2*Rlayer*TD(I) +  &
                Cp2*Delx2**2*RhoS2*Rlayer*TD(I) + delt*Delx2*TD(I-1) + delt*kt2*Rlayer*TD(I+1) + delt*Delx2*TDT(I-1) +  &
                delt*kt2*Rlayer*TDT(I+1))/(delt*Delx2 + delt*kt2*Rlayer + Cp2*Delx2**2.d0*RhoS2*Rlayer)

      CASE (FullyImplicitFirstOrder)

        TDT(I) =( 2.d0*delt*Delx2*QSSFlux*Rlayer + Cp2*Delx2**2*RhoS2*Rlayer*TD(I) + 2.d0*delt*Delx2*TDT(I-1) +  &
                 2.d0*delt*kt2*Rlayer*TDT(I+1))/(2.d0*delt*Delx2 + 2.d0*delt*kt2*Rlayer + Cp2*Delx2**2.d0*RhoS2*Rlayer)

      END SELECT

      TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

    ELSE IF (.not. RLayerPresent .and.  RLayer2Present) THEN  ! R-layer second

      !  check for PCM layer before R layer

      IF( SUM(Material(MatLay)%TempEnth(1:3,2)) > 0.  &
          .and. SUM(Material(MatLay2)%TempEnth(1:3,2)) < 0.) THEN    !  phase change material Layer1,  Use TempEnth Data

        Enth1Old = terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TD(I),IndVarCol,DepVarCol)
        Enth1New = terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TDT(I),IndVarCol,DepVarCol)
        EnthNew(I) = Enth1New    !  This node really doesn't have an enthalpy, this gives it a value

        IF (Enth1New==Enth1Old)  THEN
          Cp1=Cpo1
        ELSE
          Cp1=Max(Cpo1,(Enth1New -Enth1Old)/(TDT(I)-TD(I)))
        END IF

      END IF

      SELECT CASE (CondFDSchemeType)

      CASE (CrankNicholsonSecondOrder)
        TDT(I)=(2.d0*delt*Delx1*QSSFlux*Rlayer2 - delt*Delx1*TD(I) -  &
                delt*kt1*Rlayer2*TD(I) + Cp1*Delx1**2*RhoS1*Rlayer2*TD(I) + delt*kt1*Rlayer2*TD(I-1) +  &
                delt*Delx1*TD(I+1) + delt*kt1*Rlayer2*TDT(I-1) + delt*Delx1*TDT(I+1))/  &
               (delt*Delx1 + delt*kt1*Rlayer2 + Cp1*Delx1**2*RhoS1*Rlayer2)
      CASE (FullyImplicitFirstOrder)
        TDT(I)=(2.d0*delt*Delx1*QSSFlux*Rlayer2  + Cp1*Delx1**2*RhoS1*Rlayer2*TD(I) + 2.d0*delt*kt1*Rlayer2*TDT(I-1) + &
            2.d0*delt*Delx1*TDT(I+1))/ (2.d0*delt*Delx1 + 2.d0*delt*kt1*Rlayer2 + Cp1*Delx1**2*RhoS1*Rlayer2)

      END SELECT

      TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

    ELSE  !   Regular or Phase Change on both sides of interface
          !   Consider the various PCM material location cases
      Cp1 = Cpo1    !  Will be changed if PCM
      Cp2 = Cpo2    !  Will be changed if PCM
      IndVarCol = 1
      DepVarCol =2

      IF( Sum(Material(MatLay)%TempEnth(1:3,2)) > 0.  &
          .and. Sum(Material(MatLay2)%TempEnth(1:3,2)) > 0.) THEN    !  phase change material both layers,  Use TempEnth Data

        Enth1Old = terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TD(I),IndVarCol,DepVarCol)
        Enth2Old = terpld(Size(Material(MatLay2)%TempEnth,1),Material(MatLay2)%TempEnth,TD(I),IndVarCol,DepVarCol)
        Enth1New = terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TDT(I),IndVarCol,DepVarCol)
        Enth2New = terpld(Size(Material(MatLay2)%TempEnth,1),Material(MatLay2)%TempEnth,TDT(I),IndVarCol,DepVarCol)

        EnthNew(I) = Enth1New    !  This node really doesn't have an enthalpy, this gives it a value

        IF (Enth1New==Enth1Old)  THEN
          Cp1 = Cpo1
        ELSE
          Cp1 = MAX(Cpo1,(Enth1New -Enth1Old)/(TDT(I)-TD(I)))
        END IF

        IF (Enth2New==Enth2Old)  THEN
          Cp2 = Cpo2
        ELSE
          Cp2 = Max(Cpo2,(Enth2New -Enth2Old)/(TDT(I)-TD(I)))
        END IF

      ELSE IF( SUM(Material(MatLay)%TempEnth(1:3,2)) > 0.  &
              .and. SUM(Material(MatLay2)%TempEnth(1:3,2)) < 0.) THEN    !  phase change material Layer1,  Use TempEnth Data

        Enth1Old = terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TD(I),IndVarCol,DepVarCol)
        Enth1New = terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TDT(I),IndVarCol,DepVarCol)
        EnthNew(I) = Enth1New    !  This node really doesn't have an enthalpy, this gives it a value

        IF (Enth1New==Enth1Old)  THEN
          Cp1 = Cpo1
        ELSE
          Cp1 = Max(Cpo1,(Enth1New -Enth1Old)/(TDT(I)-TD(I)))
        END IF

        Cp2 = Cpo2

      ELSE IF( SUM(Material(MatLay)%TempEnth(1:3,2)) < 0.  &
              .and. SUM(Material(MatLay2)%TempEnth(1:3,2)) > 0.) THEN    !  phase change material Layer2,  Use TempEnth Data

        Enth2Old = terpld(Size(Material(MatLay2)%TempEnth,1),Material(MatLay2)%TempEnth,TD(I),IndVarCol,DepVarCol)
        Enth2New = terpld(Size(Material(MatLay2)%TempEnth,1),Material(MatLay2)%TempEnth,TDT(I),IndVarCol,DepVarCol)
        EnthNew(I) = Enth2New  !  This node really doesn't have an enthalpy, this gives it a value

        IF (Enth2New==Enth2Old)  THEN
          Cp2 = Cpo2
        ELSE
          Cp2 = MAX(Cpo2,(Enth2New -Enth2Old)/(TDT(I)-TD(I)))
        END IF

          Cp1 = Cpo1

      END If  ! Phase change material check

      SELECT CASE (CondFDSchemeType)

      CASE (CrankNicholsonSecondOrder)
        !     Regular Internal Interface Node with Source/sink using Adams Moulton second order
        TDT(I) = MIN(NinetyNine,(2.d0*delt*Delx1*Delx2*QSSFlux - delt*Delx2*kt1*TD(I) - delt*Delx1*kt2*TD(I) +  &
                 Cp1*Delx1**2.d0*Delx2*RhoS1*TD(I) + Cp2*Delx1*Delx2**2.d0*RhoS2*TD(I) + delt*Delx2*kt1*TD(I-1) +  &
                 delt*Delx1*kt2*TD(I+1) + delt*Delx2*kt1*TDT(I-1) + delt*Delx1*kt2*TDT(I+1))/   &
                 (delt*Delx2*kt1 + delt*Delx1*kt2 + Cp1*Delx1**2.d0*Delx2*RhoS1 + Cp2*Delx1*Delx2**2.d0*RhoS2))

      CASE (FullyImplicitFirstOrder)
        ! first order adams moulton
        TDT(I) = MIN(NinetyNine,(2.d0*delt*Delx1*Delx2*QSSFlux + &
                 Cp1*Delx1**2.d0*Delx2*RhoS1*TD(I) + Cp2*Delx1*Delx2**2.d0*RhoS2*TD(I) +  &
                 2.d0*delt*Delx2*kt1*TDT(I-1) + 2.d0*delt*Delx1*kt2*TDT(I+1))/   &
                 (2.d0*delt*Delx2*kt1 + 2.d0*delt*Delx1*kt2 + Cp1*Delx1**2.d0*Delx2*RhoS1 + Cp2*Delx1*Delx2**2.d0*RhoS2))

      END SELECT

      TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

      IF (Construct(ConstrNum)%SourceSinkPresent .and. Lay == Construct(ConstrNum)%SourceAfterLayer) Then
        TcondFDSourceNode(Surf)= TDT(I) ! transfer node temp to Radiant System
        TempSource(Surf) = TDT(I)  !  Transfer node temp to DataHeatBalSurface  module.

      ENDIF
!+++++++++++++++++++++++++++++++
    END IF   !  end of R-layer and Regular check

  END IF  !End of the CondFD if block


RETURN

END SUBROUTINE IntInterfaceNodeEqns


SUBROUTINE InteriorBCEqns(Delt,I,Lay,Surf,T,TT,Rhov,RhoT,RH,TD,TDT,EnthOld,EnthNew)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   November, 2003
          !       MODIFIED       B. Griffith, P. Tabares, May 2011, add first order fully implicit, bug fixes, cleanup
          !       RE-ENGINEERED  C. O. Pedersen 2006

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the heat transfer at the node on the surfaces inside face (facing zone)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY: Mat, ZoneAirHumRat, QHTRadSysSurf, QHWBaseboardSurf, QSteamBaseboardSurf, QElecBaseboardSurf
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Delt  ! Time Increment
  INTEGER, INTENT(IN)  :: I     ! Node Index
  INTEGER, INTENT(IN)  :: Lay   ! Layer Number for Construction
  INTEGER, INTENT(IN)  :: Surf  ! Surface number
  REAL(r64),DIMENSION(:), INTENT(In)    :: T     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF (Old).
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TT    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF (New).
  REAL(r64),DIMENSION(:), INTENT(In)    :: Rhov     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RhoT  !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(In)    :: TD     !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: TDT    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: RH    !INSIDE SURFACE TEMPERATURE OF EACH HEAT TRANSFER SURF.
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthOld    ! Old Nodal enthalpy
  REAL(r64),DIMENSION(:), INTENT(InOut) :: EnthNew    ! New Nodal enthalpy

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: NetLWRadToSurfFD !Net interior long wavelength radiation to surface from other surfaces
  REAL(r64) :: QRadSWInFD    !Short wave radiation absorbed on inside of opaque surface
  REAL(r64) :: QHtRadSysSurfFD  ! Current radiant heat flux at a surface due to the presence of high temperature radiant heaters
  Real(r64) :: QHWBaseboardSurfFD  ! Current radiant heat flux at a surface due to the presence of hot water baseboard heaters
  Real(r64) :: QSteamBaseboardSurfFD  ! Current radiant heat flux at a surface due to the presence of steam baseboard heaters
  Real(r64) :: QElecBaseboardSurfFD  ! Current radiant heat flux at a surface due to the presence of electric baseboard heaters
  REAL(r64) :: QRadThermInFD !Thermal radiation absorbed on inside surfaces
  REAL(r64) :: Delx

  INTEGER    :: ConstrNum
  INTEGER    :: MatLay
  INTEGER    :: IndVarCol
  INTEGER    :: DepVarCol

  REAL(r64)  :: kto
  REAL(r64)  :: kt1
  REAL(r64)  :: kt
  REAL(r64)  :: Cp
  REAL(r64)  :: Cpo
  REAL(r64)  :: RhoS
  REAL(r64)  :: Tia
  REAL(r64)  :: Rhovi
  REAL(r64)  :: hmassi
  REAL(r64)  :: hconvi

  REAL(r64)  :: Rlayer
  REAL(r64)  :: SigmaRLoc
  REAL(r64)  :: SigmaCLoc
  REAL(r64)  :: QNetSurfInside


  ConstrNum = Surface(surf)%Construction
  SigmaRLoc = SigmaR(ConstrNum)
  SigmaCLoc = SigmaC(ConstrNum)

  !Set the internal conditions to local variables
  NetLWRadToSurfFD      = NetLWRadToSurf(surf)
  QRadSWInFD            = QRadSWInAbs(surf)
  QHtRadSysSurfFD       = QHtRadSysSurf(surf)
  QHWBaseboardSurfFD    = QHWBaseboardSurf(surf)
  QSteamBaseboardSurfFD = QSteamBaseboardSurf(surf)
  QElecBaseboardSurfFD  = QElecBaseboardSurf(surf)
  QRadThermInFD         = QRadThermInAbs(Surf)

  !Boundary Conditions from Simulation for Interior
  hconvi = HConvInFD(surf)
  hmassi = HMassConvInFD(surf)

  Tia    = Mat(Surface(Surf)%Zone)
  rhovi  = RhoVaporAirIn(surf)

!++++++++++++++++++++++++++++++++++++++++++++++++++++++
   !    Do all the nodes in the surface   Else will switch to SigmaR,SigmaC
  IF (SolutionAlgo== UseCondFD .or. Construct(ConstrNum)%UseHBAlgorithmCondFDDetailed)  THEN

    MatLay = Construct(ConstrNum)%LayerPoint(Lay)
     !  Set Thermal Conductivity.  Can be constant, simple linear temp dep or multiple linear segment temp function dep.
    kto = Material(MatLay)%Conductivity   !  20C base conductivity
    kt1 = Material(MatLay)%tk1  !  linear coefficient (normally zero)
    kt  = kto + kt1*((TDT(I)+TDT(i-1))/2.d0 - 20.d0)


    IF( SUM(Material(MatLay)%TempCond(1:3,2)) >= 0.) THEN ! Multiple Linear Segment Function

      DepVarCol= 2  ! thermal conductivity
      IndVarCol=1  !temperature
          !  Use average  of surface and first node temp for determining k
      kt  = terpld(Size(Material(MatLay)%TempCond,1),Material(MatLay)%TempCond,(TDT(I)+TDT(I-1))/2.,IndVarCol,DepVarCol)

    END IF


    RhoS = Material(MatLay)%Density
    Cpo = Material(MatLay)%SpecHeat
    Cp  = Cpo  !  Will be changed if PCM
    Delx = ConstructFD(ConstrNum)%Delx(Lay)

    !Calculate the Dry Heat Conduction Equation

    Rlayer = Material(MatLay)%Resistance
    If (Material(MatLay)%ROnly .or. Material(MatLay)%Group == 1 ) THEN  ! R Layer or Air Layer
            !  Use algebraic equation for TDT based on R

      IF (Surface(Surf)%ExtBoundCond > 0 .and. i==1.d0) THEN  !this is for an interzone partition
        TDT(I)=(QHtRadSysSurfFD*Rlayer + QHWBaseboardSurfFD*Rlayer + QSteamBaseboardSurfFD*Rlayer + &
                    QElecBaseboardSurfFD*Rlayer + &
                    QRadSWInFD*Rlayer + QRadThermInFD*Rlayer + TDT(I+1) + hconvi*Rlayer*Tia)/(1 + hconvi*Rlayer)

      ELSE !PT,regular wall
        TDT(I)=(QHtRadSysSurfFD*Rlayer + QHWBaseboardSurfFD*Rlayer + QSteamBaseboardSurfFD*Rlayer + &
                    QElecBaseboardSurfFD*Rlayer + &
                    QRadSWInFD*Rlayer + QRadThermInFD*Rlayer + TDT(I-1) + hconvi*Rlayer*Tia)/(1 + hconvi*Rlayer)

      ENDIF

      TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

    ELSE  !  Regular or PCM


      IF( Sum(Material(MatLay)%TempEnth(1:3,2)) >= 0.) THEN    !  phase change material,  Use TempEnth Data

        DepVarCol= 2  ! enthalpy

        IndVarCol=1  !temperature

        EnthOld(I) =terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TD(I),IndVarCol,DepVarCol)

        EnthNew(I) =terpld(Size(Material(MatLay)%TempEnth,1),Material(MatLay)%TempEnth,TDT(I),IndVarCol,DepVarCol)
        IF (EnthNew(I)==EnthOld(I))  THEN
          Cp = Cpo
        ELSE
          Cp = MAX(Cpo,(EnthNew(I) -EnthOld(I))/(TDT(I)-TD(I)))
        END IF

      ELSE  ! Not phase change material
        Cp= Cpo

      END IF  ! Phase change material check


      IF (Surface(Surf)%ExtBoundCond > 0 .and. i==1.d0) THEN !this is for an interzone partition
        SELECT CASE (CondFDSchemeType)

        CASE (CrankNicholsonSecondOrder)
          ! Adams-Moulton second order
          TDT(I)=(2.d0*Delt*Delx*NetLWRadToSurfFD + 2.d0*Delt*Delx*QHtRadSysSurfFD + 2.d0*Delt*Delx*QHWBaseboardSurfFD + &
                 2.d0*Delt*Delx*QSteamBaseboardSurfFD + 2.d0*Delt*Delx*QElecBaseboardSurfFD + &
                 2.d0*Delt*Delx*QRadSWInFD + 2.d0*Delt*Delx*QRadThermInFD -  &
                  Delt*Delx*hconvi*TD(I) - Delt*kt*TD(I) + Cp*Delx**2*RhoS*TD(I) +  &
                  Delt*kt*TD(I+1) + Delt*kt*TDT(I+1) + 2.d0*Delt*Delx*hconvi*Tia)/  &
                  (Delt*Delx*hconvi + Delt*kt + Cp*Delx**2*RhoS)

        CASE (FullyImplicitFirstOrder)
        ! Adams-Moulton First order
          TDT(I)=(2.d0*Delt*Delx*NetLWRadToSurfFD + 2.d0*Delt*Delx*QHtRadSysSurfFD + 2.d0*Delt*Delx*QHWBaseboardSurfFD + &
                 2.d0*Delt*Delx*QSteamBaseboardSurfFD + 2.d0*Delt*Delx*QElecBaseboardSurfFD + &
                 2.d0*Delt*Delx*QRadSWInFD + 2.d0*Delt*Delx*QRadThermInFD +  &
                  Cp*Delx**2*RhoS*TD(I) + 2.d0*Delt*kt*TDT(I+1) + 2.d0*Delt*Delx*hconvi*Tia)/  &
                  (2.d0*Delt*Delx*hconvi + 2.d0*Delt*kt + Cp*Delx**2*RhoS)
        END SELECT

      ELSE
        SELECT CASE (CondFDSchemeType)

        CASE (CrankNicholsonSecondOrder)
          TDT(I)=(2.d0*Delt*Delx*NetLWRadToSurfFD + 2.d0*Delt*Delx*QHtRadSysSurfFD + 2.d0*Delt*Delx*QHWBaseboardSurfFD + &
               2.d0*Delt*Delx*QSteamBaseboardSurfFD + 2.d0*Delt*Delx*QElecBaseboardSurfFD + &
               2.d0*Delt*Delx*QRadSWInFD + 2.d0*Delt*Delx*QRadThermInFD -  &
                Delt*Delx*hconvi*TD(I) - Delt*kt*TD(I) + Cp*Delx**2*RhoS*TD(I) +  &
                Delt*kt*TD(I-1) + Delt*kt*TDT(I-1) + 2.d0*Delt*Delx*hconvi*Tia)/  &
                (Delt*Delx*hconvi + Delt*kt + Cp*Delx**2*RhoS)
        CASE (FullyImplicitFirstOrder)
          TDT(I)=(2.d0*Delt*Delx*NetLWRadToSurfFD + 2.d0*Delt*Delx*QHtRadSysSurfFD + 2.d0*Delt*Delx*QHWBaseboardSurfFD + &
               2.d0*Delt*Delx*QSteamBaseboardSurfFD + 2.d0*Delt*Delx*QElecBaseboardSurfFD + &
               2.d0*Delt*Delx*QRadSWInFD + 2.d0*Delt*Delx*QRadThermInFD +  &
                Cp*Delx**2*RhoS*TD(I) + 2.d0*Delt*kt*TDT(I-1) + 2.d0*Delt*Delx*hconvi*Tia)/  &
                (2.d0*Delt*Delx*hconvi + 2.d0*Delt*kt + Cp*Delx**2*RhoS)
        END SELECT
      ENDIF

      TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

!  Pass inside conduction Flux [W/m2] to DataHeatBalanceSurface array
!          OpaqSurfInsFaceConductionFlux(Surf)= (TDT(I-1)-TDT(I))*kt/Delx
    END IF  ! Regular or R layer


  ELSE IF (SolutionAlgo == UseCondFDSimple) Then  !  Use SigmaR SigmaC  formulation  for speed  (CondFDSimple)


   !Interior Node equation for second order Adams-Moulton SigmaR SigmaC formulation with two surface nodes only

!   List(List(Rule(TDT,(2*Delt*NetLWRadToSurfMFD*SigmaR + 2*Delt*QHtRadSysSurfMFD*SigmaR + 2*Delt*QRadSWInMFD*SigmaR + 2*Delt*QRadThermInMFD*SigmaR -
!     -       Delt*TD - Delt*hconvi*SigmaR*TD + SigmaC*SigmaR*TD + Delt*TDM1 + Delt*TDTM1 + 2*Delt*hconvi*SigmaR*Tia)/
!     -     (Delt + Delt*hconvi*SigmaR + SigmaC*SigmaR))))



!     TDT(I) = (2*Delt*NetLWRadToSurfFD*SigmaRLoc + 2*Delt*QHtRadSysSurfFD*SigmaRLoc + 2*Delt*QHWBaseboardSurfFD*SigmaRLoc + &
 !              2*Delt*QRadSWInFD*SigmaRLoc + 2*Delt*QRadThermInFD*SigmaRLoc -  &
!           Delt*TD(I) - Delt*hconvi*SigmaRLoc*TD(I) + SigmaCLoc*SigmaRLoc*TD(I) +  &
!           Delt*TD(I-1) + Delt*TDT(I-1) + 2*Delt*hconvi*SigmaRLoc*Tia)/  &
 !          (Delt + Delt*hconvi*SigmaRLoc + SigmaCLoc*SigmaRLoc)

!first Order AM
!   List(List(Rule(TDT,(2*Delt*NetLWRadToSurfMFD*SigmaR +
!     -       2*Delt*QHtRadSysSurfMFD*SigmaR +
!     -       2*Delt*QRadSWInMFD*SigmaR + 2*Delt*QRadThermInMFD*SigmaR +
!     -       SigmaC*SigmaR*TD + 2*Delt*TDTM1 + 2*Delt*hconvi*SigmaR*Tia)/
!     -     (2*Delt + 2*Delt*hconvi*SigmaR + SigmaC*SigmaR))))


!Tia = 21.   !**************************************TEST OVERIDE OF TIA

    TDT(I) =(2.d0*Delt*NetLWRadToSurfFD*SigmaRLoc +  &
            2.d0*Delt*QHtRadSysSurfFD*SigmaRLoc +  &
            2.d0*Delt*QRadSWInFD*SigmaRLoc + 2.d0*Delt*QRadThermInFD*SigmaRLoc +  &
            SigmaCLoc*SigmaRLoc*TD(I) + 2*Delt*TDT(I-1) + 2.d0*Delt*hconvi*SigmaRLoc*Tia)/  &
          (2.d0*Delt + 2.d0*Delt*hconvi*SigmaRLoc + SigmaCLoc*SigmaRLoc)



    TDT(I) = Max(MinSurfaceTempLimit,Min(MaxSurfaceTempLimit,TDT(I)))  !  +++++ Limit Check

  END IF   !  End of Regular node or SigmaR SigmaC option


  QNetSurfInside=NetLWRadToSurfFD + QHtRadSysSurfFD + QRadSWInFD + QRadThermInFD + QHWBaseboardSurfFD  + &
             QSteamBaseboardSurfFD+QElecBaseboardSurfFD+hconvi*(-TDT(I) + Tia)

    !  Pass inside conduction Flux [W/m2] to DataHeatBalanceSurface array
  OpaqSurfInsFaceConductionFlux(Surf)= QNetSurfInside
  QFluxZoneToInSurf(Surf) = QNetSurfInside
  OpaqSurfInsFaceConduction(Surf)=QNetSurfInside*Surface(Surf)%Area   !for reporting as in CTF, PT

RETURN

END SUBROUTINE InteriorBCEqns
! *****************************************************************************

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

End Module HeatBalFiniteDiffManager
