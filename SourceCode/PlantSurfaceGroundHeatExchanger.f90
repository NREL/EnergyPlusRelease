MODULE SurfaceGroundHeatExchanger

  ! Module containing the routines dealing with surface/panel ground heat exchangers

  ! MODULE INFORMATION:
  !       AUTHOR         Simon Rees
  !       DATE WRITTEN   August 2002
  !       MODIFIED       Brent Griffith, Sept 2010, plant upgrades
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate hydronic Surface Ground Heat
  ! Exchangers. This includes pavement surfaces with embedded pipes for snow-
  ! melting or heat rejection from hybrid ground source heat pump systems.
  ! The heat exchanger may be gound coupled or not. In the latter case the
  ! bottom surface is exposed to the wind but not solar gains.

  ! METHODOLOGY EMPLOYED:
  ! This model is based on the QTF formulation of heat transfer through
  ! building elements with embedded heat sources/sinks. The model uses
  ! a heat exchanger analogy to relate the inlet fluid temperature to the
  ! net heat transfer rate and consequently outlet temperature. The model
  ! is entirely passive i.e. it does not set any flow rates or incorporate
  ! any controls. In order to deal with the non-linear boundary conditions
  ! at the top surface due to the presence of ice/snow fluxes have to be
  ! calculated by the QTF model and temperature calculated from the surface
  ! heat balance. This requires some iteration.
  !
  ! Note: top surface variables correspond to 'outside' variables in standard
  ! CTF/QTF definition. Bottom surface variables correspond to 'inside' variables.

  ! REFERENCES:
  ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
  !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
  !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
  !   Engineering.
  ! Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
  !   of Wisconsin-Madison.


  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,       ONLY : MaxNameLength, BeginTimeStepFlag, KelvinConv
USE DataInterfaces,    ONLY : ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError, ShowContinueErrorTimeStamp, &
                              SetupOutputVariable, ShowWarningMessage, ShowRecurringWarningErrorAtEnd
USE DataLoopNode
USE DataHeatBalance,   ONLY : MaxCTFTerms


  ! Use statements for access to subroutines in other modules


IMPLICIT NONE         ! Enforce explicit typing of all variables


PRIVATE ! Everything private unless explicitly made public


  ! MODULE PARAMETER DEFINITIONS
  REAL(r64), PARAMETER :: SmallNum      = 1.0d-30        ! Very small number to avoid div0 errors
  REAL(r64), PARAMETER :: StefBoltzmann = 5.6697d-08     ! Stefan-Boltzmann constant
  REAL(r64), PARAMETER     :: SurfaceHXHeight=0.0d0        ! Surface Height above ground -- used in height dependent calcs.
  CHARACTER(len=*), PARAMETER :: Blank = ' '

  INTEGER, PARAMETER :: SurfCond_Ground=1
  INTEGER, PARAMETER :: SurfCond_Exposed=2


  ! DERIVED TYPE DEFINITIONS
TYPE SurfaceGroundHeatExchangerData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name              =Blank ! name of surface GHE
  CHARACTER(len=MaxNameLength) :: ConstructionName  =Blank ! name of the associated construction
  CHARACTER(len=MaxNameLength) :: InletNode         =Blank ! surface GHE inlet fluid node
  CHARACTER(len=MaxNameLength) :: OutletNode        =Blank ! surface GHE outlet fluid node
  REAL(r64)                    :: DesignMassFlowRate =0.d0 !
  REAL(r64)                    :: TubeDiameter      =0.0d0   ! hydronic tube inside diameter
  REAL(r64)                    :: TubeSpacing       =0.0d0   ! tube spacing
  REAL(r64)                    :: SurfaceLength     =0.0d0   ! active length of surface GHE
  REAL(r64)                    :: SurfaceWidth      =0.0d0   ! active width of surface GHE
  REAL(r64)                    :: TopThermAbs       =0.0d0   ! Thermal absortivity of top layer
  REAL(r64)                    :: TopSolarAbs       =0.0d0   ! solar absortivity of top layer
  REAL(r64)                    :: BtmThermAbs       =0.0d0   ! Thermal absortivity of bottom layer
  INTEGER                      :: LowerSurfCond     =0     ! Type of lower surf. boundary condition
  INTEGER                      :: TubeCircuits      =0     ! number of circuits in total
  INTEGER                      :: ConstructionNum   =0     ! construction index number
  INTEGER                      :: InletNodeNum      =0     ! inlet node number
  INTEGER                      :: OutletNodeNum     =0     ! oulet node number
  INTEGER                      :: TopRoughness      =0     ! roughness of top layer
  INTEGER                      :: BtmRoughness      =0     ! roughness of bottom layer
  INTEGER                      :: FrozenErrIndex1   =0     ! recurring error index
  INTEGER                      :: FrozenErrIndex2   =0     ! recurring error index
  INTEGER                      :: ConvErrIndex1     =0     ! recurring error index
  INTEGER                      :: ConvErrIndex2     =0     ! recurring error index
  INTEGER                      :: ConvErrIndex3     =0     ! recurring error index
  !loop topology variables
  INTEGER                      :: LoopNum           =0
  INTEGER                      :: LoopSideNum       =0
  INTEGER                      :: BranchNum         =0
  INTEGER                      :: CompNum           =0
END TYPE SurfaceGroundHeatExchangerData


TYPE SurfaceGroundHeatExchangerQTF
  ! QTF Constants
  REAL(r64) :: TsrcConstCoef                            =0.0D0
  REAL(r64) :: TsrcVarCoef                              =0.0D0
  REAL(r64) :: QbtmConstCoef                            =0.0D0
  REAL(r64) :: QbtmVarCoef                              =0.0D0
  REAL(r64) :: QtopConstCoef                            =0.0D0
  REAL(r64) :: QtopVarCoef                              =0.0D0
  ! conventional CTF terms
  INTEGER :: NumCTFTerms                                = 0 ! number of terms for surface
  ! could be allocated rather than hard dimensioning.
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFin         =0.0D0 ! surf flux in ctf - X
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFout        =0.0D0 ! surf flux in ctf - Z
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFcross      =0.0D0 ! surf flux in ctf - Y
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFflux       =0.0D0 ! surf flux in ctf - F
  ! QTF coefficients
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFSourceIn   =0.0D0 ! surf flux in ctf - Wi
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFSourceOut  =0.0D0 ! surf flux out ctf - Wo
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFTSourceOut =0.0D0 ! surf flux in qtf - x
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFTSourceIn  =0.0D0 ! surf flux in qtf - y
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: CTFTSourceQ   =0.0D0 ! surf flux in qtf - f


  ! History data
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: TbtmHistory   =0.0D0
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: TtopHistory   =0.0D0
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: TsrcHistory   =0.0D0
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: QbtmHistory   =0.0D0
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: QtopHistory   =0.0D0
  REAL(r64),DIMENSION(0:MaxCTFTerms-1) :: QsrcHistory   =0.0D0
  REAL(r64)        :: Qsrc                         =0.0D0
  REAL(r64)        :: QsrcAvg                      =0.0D0
  REAL(r64)        :: LastQSrc                     =0.0D0
  REAL(r64)        :: LastSysTimeElapsed           =0.0D0
  REAL(r64)        :: LastTimeStepSys              =0.0D0
END TYPE SurfaceGroundHeatExchangerQTF


TYPE SurfaceGroundHeatExchngrReport
  ! Report data
  REAL(r64)                    :: InletTemp             =0.0d0 ! water inlet temperature
  REAL(r64)                    :: OutletTemp            =0.0d0 ! water outlet temperature
  REAL(r64)                    :: MassFlowRate          =0.0d0 ! water mass flow rate
  REAL(r64)                    :: TopSurfaceTemp        =0.0d0 ! Top surface temperature
  REAL(r64)                    :: BtmSurfaceTemp        =0.0d0 ! Bottom  surface temperature
  REAL(r64)                    :: TopSurfaceFlux        =0.0d0 ! Top  surface heat flux
  REAL(r64)                    :: BtmSurfaceFlux        =0.0d0 ! Bottom  surface heat flux
  REAL(r64)                    :: HeatTransferRate      =0.0d0 ! total fluid heat transfer rate, Watts
  REAL(r64)                    :: SurfHeatTransferRate  =0.0d0 ! total surface heat transfer rate, Watts
  REAL(r64)                    :: Energy                =0.0d0 ! cumulative energy, Joules
  REAL(r64)                    :: SurfEnergy            =0.0d0 ! cumulative energy, Joules
  REAL(r64)                    :: SourceTemp            =0.0d0 ! Source temperature
END TYPE SurfaceGroundHeatExchngrReport


TYPE(SurfaceGroundHeatExchangerData), DIMENSION(:), ALLOCATABLE :: SurfaceGHE
TYPE(SurfaceGroundHeatExchangerQTF),  DIMENSION(:), ALLOCATABLE :: SurfaceGHEQTF
TYPE(SurfaceGroundHeatExchngrReport), DIMENSION(:), ALLOCATABLE :: SurfaceGHEReport



  ! MODULE VARIABLE DECLARATIONS:
  ! utility variables initialized once
INTEGER :: NumOfSurfaceGHEs   =0         ! Number of surface GHE ground heat exchangers
LOGICAL :: NoSurfaceGroundTempObjWarning=.true.  ! This will cause a warning to be issued if no "surface" ground
                                                 ! temperature object was input.
  ! Utility variables - initialized for each instance of a surface GHE
INTEGER :: InletNodeNum       =0         ! inlet node number
INTEGER :: OutletNodeNum      =0         ! oulet node number
INTEGER :: ConstructionNum    =0         ! construction index number
INTEGER :: TopRoughness       =0         ! roughness of top layer
INTEGER :: BtmRoughness       =0         ! roughness of bottom layer
REAL(r64)    :: InletTemp          =0.0d0       ! water inlet temperature
REAL(r64)    :: OutletTemp         =0.0d0       ! water outlet temperature
REAL(r64)    :: FlowRate           =0.0d0       ! water mass flow rate
REAL(r64)    :: TopSurfTemp        =0.0d0       ! Top  surface temperature
REAL(r64)    :: BtmSurfTemp        =0.0d0       ! Bottom  surface temperature
REAL(r64)    :: TopSurfFlux        =0.0d0       ! Top  surface heat flux
REAL(r64)    :: BtmSurfFlux        =0.0d0       ! Bottom  surface heat flux
REAL(r64)    :: SourceFlux         =0.0d0       ! total heat transfer rate, Watts
REAL(r64)    :: SourceTemp         =0.0d0       ! total heat transfer rate, Watts
REAL(r64)    :: SurfaceArea        =0.0d0       ! surface GHE surface area
REAL(r64)    :: TopThermAbs        =0.0d0       ! Thermal absortivity of top layer
REAL(r64)    :: BtmThermAbs        =0.0d0       ! Thermal absortivity of bottom layer
REAL(r64)    :: TopSolarAbs        =0.0d0       ! Solar absortivity of top layer
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! weather data records updated every zone time step
REAL(r64)    :: PastBeamSolarRad   =0.0d0       ! Previous beam normal solar irradiance
REAL(r64)    :: PastSolarDirCosVert=0.0d0       ! Previous vertical component of solar normal
REAL(r64)    :: PastDifSolarRad    =0.0d0       ! Previous sky diffuse solar horizontal irradiance
REAL(r64)    :: PastGroundTemp     =0.0d0       ! Previous ground temperature
LOGICAL :: PastIsRain         =.false.   ! Previous Surfaces are wet for this time interval
LOGICAL :: PastIsSnow         =.false.   ! Previous Snow on the ground for this time interval
REAL(r64)    :: PastOutBaroPress   =0.0d0       ! Previous outdoor air barometric pressure
REAL(r64)    :: PastOutDryBulbTemp =0.0d0       ! Previous outdoor air dry bulb temperature
REAL(r64)    :: PastOutHumRat      =0.0d0       ! Previous outdoor air humidity ratio
REAL(r64)    :: PastOutAirDensity  =0.0d0       ! Previous outdoor air density
REAL(r64)    :: PastOutWetBulbTemp =0.0d0       ! Previous outdoor air wet bulb temperature
REAL(r64)    :: PastOutDewPointTemp=0.0d0       ! Previous outdoor dewpoint temperature
REAL(r64)    :: PastSkyTemp        =0.0d0       ! Previous sky temperature
REAL(r64)    :: PastWindSpeed      =0.0d0       ! Previous outdoor air wind speed
REAL(r64)    :: PastCloudFraction  =0.0d0       ! Previous Fraction of sky covered by clouds


  ! time keeping variables used for keeping track of average flux over each time step
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSysSrcAvg        ! Average source over the time step
REAL(r64), ALLOCATABLE, DIMENSION(:)             :: LastSysTimeElapsed   ! record of system time
REAL(r64), ALLOCATABLE, DIMENSION(:)             :: LastTimeStepSys      ! previous time step size


  ! SUBROUTINE SPECIFICATIONS FOR MODULE PlantSurfaceGroundHeatExchangers
PUBLIC  SimSurfaceGroundHeatExchanger
PRIVATE GetSurfaceGroundHeatExchanger
PRIVATE InitSurfaceGroundHeatExchanger
PRIVATE CalcSurfaceGroundHeatExchanger
PRIVATE CalcBottomFluxCoefficents
PRIVATE CalcTopFluxCoefficents
PRIVATE CalcSourceTempCoefficents
PRIVATE CalcSourceFlux
PRIVATE CalcTopSurfTemp
PRIVATE CalcBottomSurfTemp
PRIVATE CalcHXEffectTerm
PRIVATE UpdateHistories
PRIVATE UpdateSurfaceGroundHeatExchngr
PRIVATE ReportSurfaceGroundHeatExchngr


CONTAINS


!==============================================================================

SUBROUTINE SimSurfaceGroundHeatExchanger(CompName,CompIndex,FirstHVACIteration,RunFlag,InitLoopEquip)    !DSU

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the public routine that is used to simulate
          ! the operation of surface ground heat exchangers at each system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! Several private routines are called to get data, make the calculations
          ! and update stuff. This is called for each instance of surface GHE components.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the surface GHE
  INTEGER,          INTENT(INOUT) :: CompIndex
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  !INTEGER,          INTENT(IN)  :: FlowLock            ! flow initialization/condition flag    !DSU
  LOGICAL,          INTENT(IN)  :: RunFlag             ! TRUE if equipment is operating
  LOGICAL                       :: InitLoopEquip

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag = .TRUE.    ! Flag first time, input is fetched
  INTEGER       :: SurfaceGHENum            ! index in local derived types

          ! check for input
  IF (GetInputFlag) THEN
    CALL GetSurfaceGroundHeatExchanger
    GetInputFlag=.FALSE.
  ENDIF

  IF (InitLoopEquip) THEN
    SurfaceGHENum = FindItemInList(CompName,SurfaceGHE%Name,NumOfSurfaceGHEs)
    CompIndex=SurfaceGHENum
    RETURN
  ENDIF

  ! Find the correct Surface Ground Heat Exchanger
  IF (CompIndex <= 0) THEN
    CALL ShowFatalError('SimSurfaceGroundHeatExchanger: Unit not found='//TRIM(CompName))
  ELSE
    SurfaceGHENum=CompIndex
    IF (SurfaceGHENum > NumOfSurfaceGHEs .or. SurfaceGHENum < 1) THEN
      CALL ShowFatalError('SimSurfaceGroundHeatExchanger:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(SurfaceGHENum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfSurfaceGHEs))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(SurfaceGHENum)) THEN
      IF (CompName /= SurfaceGHE(SurfaceGHENum)%Name) THEN
        CALL ShowFatalError('SimSurfaceGroundHeatExchanger: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(SurfaceGHENum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(SurfaceGHE(SurfaceGHENum)%Name))
      ENDIF
      CheckEquipName(SurfaceGHENum)=.false.
    ENDIF
  ENDIF

  ! initialize
  CALL InitSurfaceGroundHeatExchanger(SurfaceGHENum,RunFlag)    !DSU
  ! make the calculations
  CALL CalcSurfaceGroundHeatExchanger(SurfaceGHENum, FirstHVACIteration)    !DSU
  ! update vaiables
  CALL UpdateSurfaceGroundHeatExchngr(SurfaceGHENum)    !DSU
  ! update report variables
  CALL ReportSurfaceGroundHeatExchngr(SurfaceGHENum)

  RETURN

END SUBROUTINE SimSurfaceGroundHeatExchanger

!==============================================================================

SUBROUTINE GetSurfaceGroundHeatExchanger

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for hydronic Surface Ground Heat Exchangers
          ! from the user input file.  This will contain all of the information
          ! needed to define and simulate the surface.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance,    ONLY : Construct, TotConstructs
  USE InputProcessor,     ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList, &
                                 SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,   ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY : TestCompSet
  USE FluidProperties,    ONLY : CheckFluidPropertyName, FindGlycol
  USE DataEnvironment,    ONLY : GroundTemp_Surface,GroundTemp_SurfaceObjInput
  USE General,            ONLY : RoundSigDigits
  USE DataLoopNode

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

  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input,
                                                         ! fatal at end of routine
  INTEGER                        :: IOStatus             ! Used in GetObjectItem
  INTEGER                        :: Item                 ! Item to be "gotten"
  INTEGER                        :: NumAlphas            ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers           ! Number of Numbers for each GetObjectItem call
!  INTEGER                        :: NumFluids            ! number of fluids in sim.

          ! Initializations and allocations
  cCurrentModuleObject = 'GroundHeatExchanger:Surface'
  NumOfSurfaceGHEs = GetNumObjectsFound(cCurrentModuleObject)
  ! allocate data structures
  IF(ALLOCATED(SurfaceGHE)) DEALLOCATE(SurfaceGHE)
  IF(ALLOCATED(SurfaceGHEQTF)) DEALLOCATE(SurfaceGHEQTF)
  IF(ALLOCATED(SurfaceGHEReport)) DEALLOCATE(SurfaceGHEReport)

  ALLOCATE(SurfaceGHE(NumOfSurfaceGHEs))
  ALLOCATE(SurfaceGHEQTF(NumOfSurfaceGHEs))
  ALLOCATE(SurfaceGHEReport(NumOfSurfaceGHEs))
  ALLOCATE(CheckEquipName(NumOfSurfaceGHEs))
  CheckEquipName=.true.

  ! initialize data structures
  ! surface data
  ! Obtain all of the user data related to the surfaces...
  DO Item = 1, NumOfSurfaceGHEs

    ! get the input data
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ! General user input data
    SurfaceGHE(Item)%Name = cAlphaArgs(1)
    SurfaceGHE(Item)%ConstructionName = cAlphaArgs(2)
    SurfaceGHE(Item)%ConstructionNum  = FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

    IF (SurfaceGHE(Item)%ConstructionNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! Error checking for surfaces, zones, and construction information
    IF (.NOT. Construct(SurfaceGHE(Item)%ConstructionNum)%SourceSinkPresent) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Construction must have internal source/sink and use Construction:InternalSource object' )
      ErrorsFound=.true.
    END IF

    !get inlet node data
    SurfaceGHE(Item)%InletNode = cAlphaArgs(3)
    SurfaceGHE(Item)%InletNodeNum  = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    IF (SurfaceGHE(Item)%InletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! get outlet node data
    SurfaceGHE(Item)%OutletNode = cAlphaArgs(4)
    SurfaceGHE(Item)%OutletNodeNum  = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    IF (SurfaceGHE(Item)%OutletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Condenser Water Nodes')

    ! tube data
    SurfaceGHE(Item)%TubeDiameter = rNumericArgs(1)
    SurfaceGHE(Item)%TubeCircuits = rNumericArgs(2)
    SurfaceGHE(Item)%TubeSpacing = rNumericArgs(3)


    IF (rNumericArgs(2) == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    IF (rNumericArgs(3) == 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(3))//'='//TRIM(RoundSigDigits(rNumericArgs(3),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF

    ! surface geometry data
    SurfaceGHE(Item)%SurfaceLength = rNumericArgs(4)
    SurfaceGHE(Item)%SurfaceWidth   = rNumericArgs(5)
    IF (rNumericArgs(4) <= 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(4))//'='//TRIM(RoundSigDigits(rNumericArgs(4),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    IF (rNumericArgs(5) <= 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(5))//'='//TRIM(RoundSigDigits(rNumericArgs(5),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF

    ! get lower b.c. type
    IF (SameString(cAlphaArgs(5),'GROUND') ) THEN
      SurfaceGHE(Item)%LowerSurfCond = SurfCond_Ground
    ELSEIF (SameString(cAlphaArgs(5),'EXPOSED') ) THEN
      SurfaceGHE(Item)%LowerSurfCond = SurfCond_Exposed
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Only "Ground" or "Exposed" is allowed.')
      ErrorsFound=.true.
    END IF

  END DO  ! end of input loop


  ! final error check
  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  END IF


  ! Set up the output variables
  DO Item = 1, NumOfSurfaceGHEs
    CALL SetupOutputVariable('Ground Heat Exchanger Heat Transfer Rate [W]',    &
                              SurfaceGHEReport(Item)%HeatTransferRate,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Surface Heat Transfer Rate [W]',    &
                              SurfaceGHEReport(Item)%SurfHeatTransferRate,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Heat Transfer Energy [J]', &
                              SurfaceGHEReport(Item)%Energy,'Plant','Sum',SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Mass Flow Rate [kg/s]',      &
                              SurfaceGHEReport(Item)%MassFlowRate,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Inlet Temperature [C]',     &
                              SurfaceGHEReport(Item)%InletTemp,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Outlet Temperature [C]',     &
                              SurfaceGHEReport(Item)%OutletTemp,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Top Surface Temperature [C]',     &
                              SurfaceGHEReport(Item)%TopSurfaceTemp,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Bottom Surface Temperature [C]',     &
                              SurfaceGHEReport(Item)%BtmSurfaceTemp,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Top Surface Heat Transfer Energy per Area [J/m2]',     &
                              SurfaceGHEReport(Item)%TopSurfaceFlux,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Bottom Surface Heat Transfer Energy per Area [J/m2]',     &
                              SurfaceGHEReport(Item)%BtmSurfaceFlux,'Plant','Average', &
                              SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Surface Heat Transfer Energy [J]', &
                              SurfaceGHEReport(Item)%SurfEnergy,'Plant','Sum',SurfaceGHE(Item)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Source Temperature [C]',     &
                              SurfaceGHEReport(Item)%SourceTemp,'Plant','Average', &
                              SurfaceGHE(Item)%Name)

  END DO


  IF (NoSurfaceGroundTempObjWarning) THEN
    IF (.not. GroundTemp_SurfaceObjInput) THEN
      CALL ShowWarningError('GetSurfaceGroundHeatExchanger: No "Site:GroundTemperature:Shallow" were input.')
      CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp_Surface,1))// &
                             ') will be used.')
    ENDIF
    NoSurfaceGroundTempObjWarning=.false.
  ENDIF


  RETURN


END SUBROUTINE GetSurfaceGroundHeatExchanger


!==============================================================================


SUBROUTINE InitSurfaceGroundHeatExchanger(SurfaceGHENum,RunFlag)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine Resets the elements of the data structure as necessary
          ! at the first HVAC iteration of each time step. The weather and QTF data
          ! is initialized once only.


          ! METHODOLOGY EMPLOYED:
          ! Check flags and update data structure


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:


  USE DataGlobals,     ONLY : BeginTimeStepFlag, PI, BeginEnvrnFlag
  USE DataEnvironment
  USE DataLoopNode,    ONLY : Node
  USE DataHeatBalance, ONLY : MaxCTFTerms, TotConstructs, Construct, Material
  USE InputProcessor,  ONLY : SameString
  USE DataPlant,       ONLY : TypeOf_GrndHtExchgSurface, PlantLoop, ScanPlantLoopsForObject
  USE FluidProperties, ONLY : GetDensityGlycol
  USE PlantUtilities,  ONLY : InitComponentNodes,SetComponentFlowRate,RegisterPlantCompDesignFlow,RegulateCondenserCompFlowReqOp


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum       ! component number
  !INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU
  LOGICAL, INTENT(IN) :: RunFlag             ! TRUE if equipment is operating


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER     :: DesignVelocity=0.5d0 ! Hypothetical design max pipe velocity [m/s]


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  REAL(r64)           :: DesignFlow          ! Hypothetical design flow rate
  LOGICAL,SAVE        :: InitQTF = .TRUE.    ! one time flag
  LOGICAL,SAVE        :: MyEnvrnFlag = .TRUE.
  INTEGER             :: Cons                ! construction counter
  INTEGER             :: Surface             ! Surface number counter
  INTEGER             :: LayerNum            ! material layer number for bottom
  REAL(r64)           :: OutDryBulb          ! Height Dependent dry bulb.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64) :: rho !local fluid density
  LOGICAL :: errFlag
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumOfSurfaceGHEs))
    MyOneTimeFlag = .false.
    MyFlag = .TRUE.
  END IF

  ! Init more variables
  IF (MyFlag(SurfaceGHENum)) THEN
    ! Locate the hx on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(SurfaceGHE(SurfaceGHENum)%Name, &
                                 TypeOf_GrndHtExchgSurface, &
                                 SurfaceGHE(SurfaceGHENum)%LoopNum, &
                                 SurfaceGHE(SurfaceGHENum)%LoopSideNum, &
                                 SurfaceGHE(SurfaceGHENum)%BranchNum, &
                                 SurfaceGHE(SurfaceGHENum)%CompNum,  &
                                 errFlag=errFlag)

    IF (errFlag) THEN
      CALL ShowFatalError('InitSurfaceGroundHeatExchanger: Program terminated due to previous condition(s).')
    ENDIF
    rho = GetDensityGlycol(PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%fluidName, &
                                       constant_zero,&
                                       PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%fluidIndex, &
                                       'InitSurfaceGroundHeatExchanger')
    SurfaceGHE(SurfaceGHENum)%DesignMassFlowRate =  &
            PI/4.0d0 * SurfaceGHE(SurfaceGHENum)%TubeDiameter**2 * DesignVelocity * &
                rho * SurfaceGHE(SurfaceGHENum)%TubeCircuits
    CALL InitComponentNodes(0.d0, SurfaceGHE(SurfaceGHENum)%DesignMassFlowRate, &
                                 SurfaceGHE(SurfaceGHENum)%InletNodeNum,&
                                 SurfaceGHE(SurfaceGHENum)%OutletNodeNum,&
                                 SurfaceGHE(SurfaceGHENum)%LoopNum, &
                                 SurfaceGHE(SurfaceGHENum)%LoopSideNum, &
                                 SurfaceGHE(SurfaceGHENum)%BranchNum, &
                                 SurfaceGHE(SurfaceGHENum)%CompNum)
    CALL RegisterPlantCompDesignFlow(SurfaceGHE(SurfaceGHENum)%InletNodeNum, SurfaceGHE(SurfaceGHENum)%DesignMassFlowRate / rho)

    MyFlag(SurfaceGHENum)=.FALSE.
  ENDIF

    ! get QTF data - only once
  IF(InitQTF)THEN
    Do Surface =1,NumOfSurfaceGHEs
      DO Cons = 1, TotConstructs
        IF(SameString(Construct(Cons)%Name,SurfaceGHE(Surface)%ConstructionName))THEN
          ! some error checking ??
          ! CTF stuff
          LayerNum = Construct(Cons)%TotLayers
          SurfaceGHEQTF(Surface)%NumCTFTerms   = Construct(Cons)%NumCTFTerms
          SurfaceGHEQTF(Surface)%CTFin         = Construct(Cons)%CTFInside       ! Z coefficents
          SurfaceGHEQTF(Surface)%CTFout        = Construct(Cons)%CTFOutside      ! X coefficents
          SurfaceGHEQTF(Surface)%CTFcross      = Construct(Cons)%CTFCross        ! Y coefficents
          SurfaceGHEQTF(Surface)%CTFflux(1: )  = Construct(Cons)%CTFFlux         ! F & f coefficents
          ! QTF stuff
          SurfaceGHEQTF(Surface)%CTFSourceIn   = Construct(Cons)%CTFSourceIn     ! Wi coefficents
          SurfaceGHEQTF(Surface)%CTFSourceOut  = Construct(Cons)%CTFSourceOut    ! Wo coefficents
          SurfaceGHEQTF(Surface)%CTFTSourceOut = Construct(Cons)%CTFTSourceOut   ! y coefficents
          SurfaceGHEQTF(Surface)%CTFTSourceIn  = Construct(Cons)%CTFTSourceIn    ! x coefficents
          SurfaceGHEQTF(Surface)%CTFTSourceQ   = Construct(Cons)%CTFTSourceQ     ! w coefficents
          SurfaceGHE(Surface)%ConstructionNum  = Cons
          ! set the initial history
!          SurfaceGHEQTF(Surface)%CTFflux(0)    = 0.0D0
!          SurfaceGHEQTF(Surface)%TbtmHistory    = OutDryBulbTemp
!          SurfaceGHEQTF(Surface)%TtopHistory   = OutDryBulbTemp
!          SurfaceGHEQTF(Surface)%TsrcHistory   = OutDryBulbTemp
!          SurfaceGHEQTF(Surface)%QbtmHistory    = 0.0D0
!          SurfaceGHEQTF(Surface)%QtopHistory   = 0.0D0
!          SurfaceGHEQTF(Surface)%QsrcHistory   = 0.0D0
         ! surface properties
          SurfaceGHE(Surface)%BtmRoughness  = Material(Construct(Cons)%LayerPoint(LayerNum))%Roughness
          SurfaceGHE(Surface)%TopThermAbs   = Material(Construct(Cons)%LayerPoint(LayerNum))%AbsorpThermal
          SurfaceGHE(Surface)%TopRoughness  = Material(Construct(Cons)%LayerPoint(1))%Roughness
          SurfaceGHE(Surface)%TopThermAbs   = Material(Construct(Cons)%LayerPoint(1))%AbsorpThermal
          SurfaceGHE(Surface)%TopSolarAbs   = Material(Construct(Cons)%LayerPoint(1))%AbsorpSolar
        END IF
      END DO
    END DO
    ! set one-time flag
    InitQTF = .FALSE.
  END IF

    IF(MyEnvrnFlag .AND. BeginEnvrnFlag) THEN
     OutDryBulb=OutDryBulbTempAt(SurfaceHXHeight)
     DO Surface =1,NumOfSurfaceGHEs
      SurfaceGHEQTF(Surface)%CTFflux(0)  = 0.0D0
      SurfaceGHEQTF(Surface)%TbtmHistory = OutDryBulb
      SurfaceGHEQTF(Surface)%TtopHistory = OutDryBulb
      SurfaceGHEQTF(Surface)%TsrcHistory = OutDryBulb
      SurfaceGHEQTF(Surface)%QbtmHistory = 0.0D0
      SurfaceGHEQTF(Surface)%QtopHistory = 0.0D0
      SurfaceGHEQTF(Surface)%QsrcHistory = 0.0D0
      SurfaceGHEQTF(Surface)%TsrcConstCoef  = 0.0d0
      SurfaceGHEQTF(Surface)%TsrcVarCoef    = 0.0d0
      SurfaceGHEQTF(Surface)%QbtmConstCoef  = 0.0d0
      SurfaceGHEQTF(Surface)%QbtmVarCoef    = 0.0d0
      SurfaceGHEQTF(Surface)%QtopConstCoef  = 0.0d0
      SurfaceGHEQTF(Surface)%QtopVarCoef    = 0.0d0
      SurfaceGHEQTF(Surface)%QSrc           = 0.0d0
      SurfaceGHEQTF(Surface)%QSrcAvg        = 0.0d0
      SurfaceGHEQTF(Surface)%LastQSrc       = 0.0d0
      SurfaceGHEQTF(Surface)%LastSysTimeElapsed   = 0.0d0
      SurfaceGHEQTF(Surface)%LastTimeStepSys      = 0.0d0

     END DO
     ! initialize past weather variables
     PastBeamSolarRad         = BeamSolarRad
     PastSolarDirCosVert      = SOLCOS(3)
     PastDifSolarRad          = DifSolarRad
     PastGroundTemp           = GroundTemp_Surface
     PastIsRain               = IsRain
     PastIsSnow               = IsSnow
     PastOutBaroPress         = OutBaroPress
     PastOutDryBulbTemp       = OutDryBulbTempAt(SurfaceHXHeight)
     PastOutHumRat            = OutHumRat
     PastOutAirDensity        = OutAirDensity
     PastOutWetBulbTemp       = OutWetBulbTempAt(SurfaceHXHeight)
     PastOutDewPointTemp      = OutDewPointTemp
     PastSkyTemp              = SkyTemp
     PastWindSpeed            = WindSpeedAt(SurfaceHXHeight)
     PastCloudFraction        = CloudFraction
     MyEnvrnFlag = .FALSE.
    END IF

    IF(.NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

  ! always initialize - module variables
  InletNodeNum    = SurfaceGHE(SurfaceGHENum)%InletNodeNum
  OutletNodeNum   = SurfaceGHE(SurfaceGHENum)%OutletNodeNum
  ConstructionNum = SurfaceGHE(SurfaceGHENum)%ConstructionNum
  SurfaceArea      = SurfaceGHE(SurfaceGHENum)%SurfaceLength * SurfaceGHE(SurfaceGHENum)%SurfaceWidth
  InletTemp       = Node(InletNodeNum)%Temp
  OutletTemp      = Node(OutletNodeNum)%Temp
  TopThermAbs     = SurfaceGHE(SurfaceGHENum)%TopThermAbs
  TopRoughness    = SurfaceGHE(SurfaceGHENum)%TopRoughness
  BtmRoughness    = SurfaceGHE(SurfaceGHENum)%BtmRoughness
  BtmThermAbs     = SurfaceGHE(SurfaceGHENum)%BtmThermAbs
  TopSolarAbs     = SurfaceGHE(SurfaceGHENum)%TopSolarAbs
  LoopNum         = SurfaceGHE(SurfaceGHENum)%LoopNum
  LoopSideNum     = SurfaceGHE(SurfaceGHENum)%LoopSideNum


  ! If loop operation is controlled by an environmental variable (DBtemp, WBtemp, etc)
  ! then shut branch down when equipment is not scheduled to run.
      DesignFlow = RegulateCondenserCompFlowReqOp(SurfaceGHE(SurfaceGHENum)%LoopNum,&
                                                  SurfaceGHE(SurfaceGHENum)%LoopSideNum,&
                                                  SurfaceGHE(SurfaceGHENum)%BranchNum,&
                                                  SurfaceGHE(SurfaceGHENum)%CompNum,     &
                                                  SurfaceGHE(SurfaceGHENum)%DesignMassFlowRate)


  CALL SetComponentFlowRate(DesignFlow, &
                              SurfaceGHE(SurfaceGHENum)%InletNodeNum,&
                              SurfaceGHE(SurfaceGHENum)%OutletNodeNum,&
                              SurfaceGHE(SurfaceGHENum)%LoopNum,&
                              SurfaceGHE(SurfaceGHENum)%LoopSideNum,&
                              SurfaceGHE(SurfaceGHENum)%BranchNum,&
                              SurfaceGHE(SurfaceGHENum)%CompNum)

  ! get the current flow rate - module variable
  FlowRate   = Node(InletNodeNum)%MassFlowRate



END SUBROUTINE InitSurfaceGroundHeatExchanger


!==============================================================================


SUBROUTINE CalcSurfaceGroundHeatExchanger(SurfaceGHENum, FirstHVACIteration)    !DSU


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a surface ground heat exchanger.  Calls are made to appropriate subroutines
          ! either in this module or outside of it.


          ! METHODOLOGY EMPLOYED:
          ! To update temperature and flux histories it is necessary to make a surface
          ! flux/temperature calculation at the begining of each zone time step using the
          ! weather data from the previous step, and using the average source flux.
          ! Once this has been done a new source flux, and current surface temperatures,
          ! are calculated using the current weather data. These surface temperatures and
          ! fluxes are used for the rest of the system time steps. During subsequent system
          ! time steps only the source flux is updated.


          ! Surface fluxes are calculated from the QTF equations using assumed surface
          ! temperatures. Surface fluxes are then dependant only on source flux. Constant
          ! and terms and terms that multiply the source flux from the QTF equations, are
          ! grouped together for convenience. These are calculated in "CalcBottomFluxCoefficents"
          ! etc. It is necessary to iterate on these equations, updating the current surface
          ! temperatures at each step.


          ! REFERENCES:
          ! See 'LowTempRadiantSystem' module
          ! IBLAST-QTF research program, completed in January 1995 (unreleased)
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.
          ! Seem, J.E. 1986. "Heat Transfer in Buildings", Ph.D. dissertation, University
          !   of Wisconsin-Madison.


          ! USE STATEMENTS:
  USE DataLoopNode,      ONLY : Node
  USE DataHVACGlobals,   ONLY : TimeStepSys, SysTimeElapsed, FirstTimeStepSysFlag
  USE DataEnvironment
  USE DataPlant,         ONLY : PlantLoop
  USE General,           ONLY : TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration   ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT(IN) :: SurfaceGHENum        ! component number
 ! INTEGER, INTENT(IN) :: FlowLock             ! flow initialization/condition flag    !DSU


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SurfFluxTol = 0.001d0      ! tolerance on the surface fluxes
  REAL(r64), PARAMETER :: SrcFluxTol  = 0.001d0      ! tolerance on the source flux
  REAL(r64), PARAMETER :: RelaxT      = 0.1d0        ! temperature relaxation factor
  INTEGER, PARAMETER :: Maxiter = 100
  INTEGER, PARAMETER :: Maxiter1 = 100


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  ! variables used with previous environmental conditions
!! not used  REAL(r64)    :: Concentration    ! set to 0.5 if glycol, 0.0 if water
  REAL(r64)    :: PastFluxTop      ! top surface flux - past value
  REAL(r64)    :: PastFluxBtm      ! bottom surface flux - past value
  REAL(r64)    :: PastTempBtm      ! bottom surface temp - past value
  REAL(r64)    :: PastTempTop      ! top surface temp - past value
  REAL(r64)    :: OldPastFluxTop   ! top surface flux - past value used during iteration
  REAL(r64)    :: OldPastFluxBtm   ! bottom surface flux - past value used during iteration
  ! variables used with current environmental conditions
  REAL(r64),SAVE    :: FluxTop          ! top surface flux
  REAL(r64),SAVE    :: FluxBtm          ! bottom surface flux
  REAL(r64),SAVE    :: TempBtm          ! bottom surface temp
  REAL(r64),SAVE    :: TempTop          ! top surface temp
  REAL(r64)    :: TempT            ! top surface temp - used in underrelaxation
  REAL(r64)    :: TempB            ! bottom surface temp - used in underrelaxation
  REAL(r64)    :: OldFluxTop       ! top surface flux - value used during iteration
  REAL(r64)    :: OldFluxBtm       ! bottom surface flux - value used during iteration
  REAL(r64)    :: OldSourceFlux    ! previous value of source flux - used during iteration
  INTEGER :: Iter
  INTEGER :: Iter1
!  INTEGER, SAVE ::ErrCount1=0
!  INTEGER, SAVE ::ErrCount2=0
!  INTEGER, SAVE ::ErrCount3=0
  LOGICAL, SAVE ::InitializeTempTop = .FALSE.
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum

  LoopNum = SurfaceGHE(SurfaceGHENum)%LoopNum
  LoopSideNum = SurfaceGHE(SurfaceGHENum)%LoopSideNum

  ! check if we are in very first call for this zone time step
  IF (BeginTimeStepFlag.AND.FirstHVACIteration.AND.PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock==1) THEN !DSU
    ! calc temps and fluxes with past env. conditions and average source flux
    SourceFlux = SurfaceGHEQTF(SurfaceGHENum)%QsrcAvg
    ! starting values for the surface temps
    PastTempBtm = SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(1)
    PastTempTop = SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(1)
    OldPastFluxTop = 1.0d+30
    OldPastFluxBtm = 1.0d+30
    OldSourceFlux = 1.0d+30
    TempB=0.0d0
    TempT=0.0d0
    iter=0
    DO  ! iterate to find surface heat balances
        ! update coefficients


      iter = iter +1
      CALL CalcTopFluxCoefficents(SurfaceGHENum, PastTempBtm, PastTempTop)
        ! calc top surface flux
      PastFluxTop = SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef + &
                    SurfaceGHEQTF(SurfaceGHENum)%QtopVarCoef * SourceFlux


        !calc new top surface temp
      CALL CalcTopSurfTemp(SurfaceGHENum, -PastFluxTop, TempT, PastOutDryBulbTemp, &
                           PastOutWetBulbTemp, PastSkyTemp, PastBeamSolarRad, &
                           PastDifSolarRad, PastSolarDirCosVert, PastWindSpeed, &
                           PastIsRain, PastIsSnow)
      ! under relax
      PastTempTop = PastTempTop*(1.0-RelaxT) + RelaxT*TempT


        ! update coefficients
      CALL CalcBottomFluxCoefficents(SurfaceGHENum, PastTempBtm, PastTempTop)
      PastFluxBtm = SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef + &
                    SurfaceGHEQTF(SurfaceGHENum)%QbtmVarCoef * SourceFlux


      IF(ABS((OldPastFluxTop - PastFluxTop)/OldPastFluxTop) <= SurfFluxTol .AND. &
         ABS((OldPastFluxBtm - PastFluxBtm)/OldPastFluxBtm) <= SurfFluxTol) EXIT


        !calc new surface temps
      CALL CalcBottomSurfTemp(SurfaceGHENum, PastFluxBtm, TempB, PastOutDryBulbTemp, &
                              PastWindSpeed, PastGroundTemp)
      ! underrelax
      PastTempBtm = PastTempBtm*(1.0-RelaxT) + RelaxT*TempB
      ! update flux record
      OldPastFluxTop = PastFluxTop
      OldPastFluxBtm = PastFluxBtm


      !Check for non-convergence
      IF(iter > maxiter) THEN
        IF (SurfaceGHE(SurfaceGHENum)%ConvErrIndex1 == 0) THEN
          CALL ShowWarningMessage('CalcSurfaceGroundHeatExchanger="'//TRIM(SurfaceGHE(SurfaceGHENum)%Name)//  &
             '", Did not converge (part 1), Iterations='//trim(TrimSigDigits(maxiter)))
          CALL ShowContinueErrorTimeStamp(' ')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('CalcSurfaceGroundHeatExchanger="'//TRIM(SurfaceGHE(SurfaceGHENum)%Name)//  &
             '", Did not converge (part 1)',SurfaceGHE(SurfaceGHENum)%ConvErrIndex1)
        EXIT
      ENDIF
    END DO


    IF(.NOT. InitializeTempTop) THEN
      TempTop = TempT
      TempBtm = TempB
      FluxTop = PastFluxTop
      FluxBtm = PastFluxBtm
      InitializeTempTop = .TRUE.
    END IF


    ! update module variables
    TopSurfTemp = TempTop
    BtmSurfTemp = TempBtm
    TopSurfFlux = -FluxTop
    BtmSurfFlux = FluxBtm


      ! get source temp for output
    CALL CalcSourceTempCoefficents(SurfaceGHENum, PastTempBtm, PastTempTop)
    SourceTemp = SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef + &
                 SurfaceGHEQTF(SurfaceGHENum)%TsrcVarCoef * SourceFlux
    ! update histories
    CALL UpdateHistories(SurfaceGHENum, PastFluxTop, PastFluxBtm, SourceFlux, SourceTemp)


    ! At the beginning of a time step, reset to zero so average calculation can start again
    SurfaceGHEQTF(SurfaceGHENum)%QsrcAvg            = 0.0D0
    SurfaceGHEQTF(SurfaceGHENum)%LastSysTimeElapsed = 0.0D0
    SurfaceGHEQTF(SurfaceGHENum)%LastTimeStepSys    = 0.0D0


    ! get current env. conditions
    PastBeamSolarRad         = BeamSolarRad
    PastSolarDirCosVert      = SOLCOS(3)
    PastDifSolarRad          = DifSolarRad
    PastGroundTemp           = GroundTemp_Surface
    PastIsRain               = IsRain
    PastIsSnow               = IsSnow
    PastOutBaroPress         = OutBaroPress
    PastOutDryBulbTemp       = OutDryBulbTempAt(SurfaceHXHeight)
    PastOutHumRat            = OutHumRat
    PastOutAirDensity        = OutAirDensity
    PastOutWetBulbTemp       = OutWetBulbTempAt(SurfaceHXHeight)
    PastOutDewPointTemp      = OutDewPointTempAt(SurfaceHXHeight)
    PastSkyTemp              = SkyTemp
    PastWindSpeed            = WindSpeedAt(SurfaceHXHeight)
    PastCloudFraction        = CloudFraction



    TempBtm = SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(1)
    TempTop = SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(1)
    OldFluxTop = 1.0d+30
    OldFluxBtm = 1.0d+30
    OldSourceFlux = 1.0d+30
    SourceFlux = CalcSourceFlux(SurfaceGHENum)
    iter = 0
    DO    ! iterate to find source flux
      iter = iter +1
      iter1=0
      DO  ! iterate to find surface heat balances
        iter1=iter1+1
        ! update top coefficients
        CALL CalcTopFluxCoefficents(SurfaceGHENum, TempBtm, TempTop)
        ! calc top surface fluxe
        FluxTop = SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef + &
                      SurfaceGHEQTF(SurfaceGHENum)%QtopVarCoef * SourceFlux
        !calc new surface temps
        CALL CalcTopSurfTemp(SurfaceGHENum, -FluxTop, TempT, PastOutDryBulbTemp, &
                           PastOutWetBulbTemp, PastSkyTemp, PastBeamSolarRad, &
                           PastDifSolarRad, PastSolarDirCosVert, PastWindSpeed, &
                           PastIsRain, PastIsSnow)
        ! under-relax
        TempTop = TempTop*(1.0-RelaxT) + RelaxT*TempT
        ! update bottom coefficients
        CALL CalcBottomFluxCoefficents(SurfaceGHENum, TempBtm, TempTop)
        FluxBtm = SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef + &
                      SurfaceGHEQTF(SurfaceGHENum)%QbtmVarCoef * SourceFlux
        ! convergence test on surface fluxes
        IF(ABS((OldFluxTop - FluxTop)/OldFluxTop) <= SurfFluxTol .AND. &
           ABS((OldFluxBtm - FluxBtm)/OldFluxBtm) <= SurfFluxTol) EXIT


        !calc new surface temps
        CALL CalcBottomSurfTemp(SurfaceGHENum, FluxBtm, TempB, PastOutDryBulbTemp, &
                                PastOutDryBulbTemp, GroundTemp_Surface)
        ! under-relax
        TempBtm = TempBtm*(1.0-RelaxT) + RelaxT*TempB
        ! update flux record
        OldFluxBtm = FluxBtm
        OldFluxTop = FluxTop


        !Check for non-convergence
        IF(iter1 > maxiter1) THEN
          IF (SurfaceGHE(SurfaceGHENum)%ConvErrIndex2 == 0) THEN
            CALL ShowWarningMessage('CalcSurfaceGroundHeatExchanger="'//TRIM(SurfaceGHE(SurfaceGHENum)%Name)//  &
               '", Did not converge (part 2), Iterations='//trim(TrimSigDigits(maxiter)))
            CALL ShowContinueErrorTimeStamp(' ')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('CalcSurfaceGroundHeatExchanger="'//TRIM(SurfaceGHE(SurfaceGHENum)%Name)//  &
               '", Did not converge (part 2)',SurfaceGHE(SurfaceGHENum)%ConvErrIndex2)
          EXIT
        ENDIF
      END DO
      ! update the source temp coefficients and update the source flux
      CALL CalcSourceTempCoefficents(SurfaceGHENum, TempBtm, TempTop)
      SourceFlux = CalcSourceFlux(SurfaceGHENum)
      ! check source flux convergence
      IF(ABS((OldSourceFlux - SourceFlux)/(1.0d-20+OldSourceFlux)) <= SrcFluxTol) EXIT
      OldSourceFlux = SourceFlux


      !Check for non-convergence
      IF(iter > maxiter) THEN
        IF (SurfaceGHE(SurfaceGHENum)%ConvErrIndex3 == 0) THEN
          CALL ShowWarningMessage('CalcSurfaceGroundHeatExchanger="'//TRIM(SurfaceGHE(SurfaceGHENum)%Name)//  &
             '", Did not converge (part 3), Iterations='//trim(TrimSigDigits(maxiter)))
          CALL ShowContinueErrorTimeStamp(' ')
        ENDIF
        CALL ShowRecurringWarningErrorAtEnd('CalcSurfaceGroundHeatExchanger="'//TRIM(SurfaceGHE(SurfaceGHENum)%Name)//  &
             '", Did not converge (part 3)',SurfaceGHE(SurfaceGHENum)%ConvErrIndex3)
        EXIT
      ENDIF
    END DO   ! end surface heat balance iteration


  ELSE    ! end source flux iteration

  ! For the rest of the system time steps ...
  ! update source flux from Twi
    SourceFlux = CalcSourceFlux(SurfaceGHENum)

  END IF
  RETURN


END SUBROUTINE CalcSurfaceGroundHeatExchanger


!==============================================================================


SUBROUTINE CalcBottomFluxCoefficents(SurfaceGHENum, Tbottom, Ttop)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates current version of constant variable parts of QTF equations.


          ! METHODOLOGY EMPLOYED:
          ! For given current surface temperatures the terms of the QTF equations can be
          ! grouped into constant terms, and those depending on the current source flux.
          ! This routine calculates the current coefficient values for the bottom flux
          ! equation.


          ! REFERENCES:
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum       ! component number
  REAL(r64),    INTENT(IN) :: Tbottom             ! current bottom (lower) surface temperature
  REAL(r64),    INTENT(IN) :: Ttop                ! current top (upper) surface temperature


          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Term

  ! add current surface temperatures to history data
  SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(0) = Tbottom
  SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(0) = Ttop

  ! Bottom Surface Coefficients
  SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef = 0.0d0
  DO Term = 0, SurfaceGHEQTF(SurfaceGHENum)%NumCTFTerms-1

    SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef =      &
       SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef +   &
       (-SurfaceGHEQTF(SurfaceGHENum)%CTFin(Term) * SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(Term)) +     &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFcross(Term) * SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(Term)) +   &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFflux(Term) * SurfaceGHEQTF(SurfaceGHENum)%QbtmHistory(Term)) +    &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFSourceIn(Term) * SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(Term))

  ENDDO

!     SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef =  SUM(-SurfaceGHEQTF(SurfaceGHENum)%CTFin * &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory + &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%CTFcross * &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%TtopHistory + &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%CTFflux * &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%QbtmHistory + &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%CTFSourceIn * &
!                                                  SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory)
  ! correct for extra bottom surface flux term
  SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef =  SurfaceGHEQTF(SurfaceGHENum)%QbtmConstCoef - &
                                               SurfaceGHEQTF(SurfaceGHENum)%CTFSourceIn(0) * &
                                               SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(0)
  ! source flux current coefficient
  SurfaceGHEQTF(SurfaceGHENum)%QbtmVarCoef    = SurfaceGHEQTF(SurfaceGHENum)%CTFSourceIn(0)



END SUBROUTINE CalcBottomFluxCoefficents


!==============================================================================


SUBROUTINE CalcTopFluxCoefficents(SurfaceGHENum, Tbottom, Ttop)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates current version of constant variable parts of QTF equations.


          ! METHODOLOGY EMPLOYED:
          ! For given current surface temperatures the terms of the QTF equations can be
          ! grouped into constant terms, and those depending on the current source flux.
          ! This routine calculates the current coefficient values for the top flux
          ! equation.


          ! REFERENCES:
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum       ! component number
  REAL(r64),    INTENT(IN) :: Tbottom             ! current bottom (lower) surface temperature
  REAL(r64),    INTENT(IN) :: Ttop                ! current top (upper) surface temperature


          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Term

  ! add current surface temperatures to history data
  SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(0) = Tbottom
  SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(0) = Ttop


  ! Top Surface Coefficients
  SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef = 0.0d0
  DO Term = 0, SurfaceGHEQTF(SurfaceGHENum)%NumCTFTerms-1

    SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef =      &
       SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef +   &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFout(Term) * SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(Term)) -     &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFcross(Term) * SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(Term)) +   &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFflux(Term) * SurfaceGHEQTF(SurfaceGHENum)%QtopHistory(Term)) +    &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFSourceOut(Term) * SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(Term))

  ENDDO

!     ! Top Surface Coefficients
!     SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef = SUM(SurfaceGHEQTF(SurfaceGHENum)%CTFout * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%TtopHistory - &
!                                              SurfaceGHEQTF(SurfaceGHENum)%CTFcross * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory + &
!                                              SurfaceGHEQTF(SurfaceGHENum)%CTFflux * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%QtopHistory + &
!                                              SurfaceGHEQTF(SurfaceGHENum)%CTFSourceOut * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory)

  ! correct for extra top surface flux term
  SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef = SurfaceGHEQTF(SurfaceGHENum)%QtopConstCoef - &
                      (SurfaceGHEQTF(SurfaceGHENum)%CTFSourceOut(0) * SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(0))
  ! surface flux current coefficient
  SurfaceGHEQTF(SurfaceGHENum)%QtopVarCoef    = SurfaceGHEQTF(SurfaceGHENum)%CTFSourceOut(0)



END SUBROUTINE CalcTopFluxCoefficents


!==============================================================================


SUBROUTINE CalcSourceTempCoefficents(SurfaceGHENum, Tbottom, Ttop)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates current version of constant variable parts of QTF equations.


          ! METHODOLOGY EMPLOYED:
          ! For given current surface temperatures the terms of the QTF equations can be
          ! grouped into constant terms, and those depending on the current source flux.
          ! This routine calculates the current coefficient values for the source temperature
          ! equation.


          ! REFERENCES:
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum         ! component number
  REAL(r64),    INTENT(IN) :: Tbottom               ! current bottom (lower) surface temperature
  REAL(r64),    INTENT(IN) :: Ttop                  ! current top (upper) surface temperature


          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Term

  ! add current surface temperatures to history data
  SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(0) = Tbottom
  SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(0) = Ttop


  SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef = 0.0d0
  DO Term = 0, SurfaceGHEQTF(SurfaceGHENum)%NumCTFTerms-1

    SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef =      &
       SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef +   &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceIn(Term) * SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory(Term)) +     &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceOut(Term) * SurfaceGHEQTF(SurfaceGHENum)%TtopHistory(Term)) +   &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFflux(Term) * SurfaceGHEQTF(SurfaceGHENum)%TsrcHistory(Term)) +    &
       (SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceQ(Term) * SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(Term))

  ENDDO

  ! Source Temperature terms
!     SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef = SUM(SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceIn * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory + &
!                                              SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceOut * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%TtopHistory + &
!                                              SurfaceGHEQTF(SurfaceGHENum)%CTFflux * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%TsrcHistory + &
!                                              SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceQ * &
!                                              SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory)
  ! correct for extra source flux term
  SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef = SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef - &
                                           SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceQ(0) * &
                                           SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(0)
  ! source flux current coefficient
  SurfaceGHEQTF(SurfaceGHENum)%TsrcVarCoef   = SurfaceGHEQTF(SurfaceGHENum)%CTFTSourceQ(0)



END SUBROUTINE CalcSourceTempCoefficents


!==============================================================================


FUNCTION CalcSourceFlux(SurfaceGHENum)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This calculates the source flux given the inlet fluid temperature. A
          ! heat exchanger analogy is used, with the surface as a 'Fixed' fluid.


          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:
          ! Strand, R.K. 1995. "Heat Source Transfer Functions and Their Application to
          !   Low Temperature Radiant Heating Systems", Ph.D. dissertation, University
          !   of Illinois at Urbana-Champaign, Department of Mechanical and Industrial
          !   Engineering.


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum         ! component number
  REAL(r64)     :: CalcSourceFlux

         ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: EpsMdotCp  ! Epsilon (heat exchanger terminology) times water mass flow rate times water specific heat



  ! Effectiveness * Modot * specific heat
  IF(FlowRate > 0.0d0)THEN
    EpsMdotCp = CalcHXEffectTerm(SurfaceGHENum,InletTemp,FlowRate)
    ! calc flux
    CalcSourceFlux = (InletTemp - SurfaceGHEQTF(SurfaceGHENum)%TsrcConstCoef  ) / &
                     (SurfaceArea/EpsMdotCp + SurfaceGHEQTF(SurfaceGHENum)%TsrcVarCoef )
  ELSE
    EpsMdotCp = 0.0d0
    CalcSourceFlux = 0.0d0
  ENDIF




END FUNCTION CalcSourceFlux


!==============================================================================


SUBROUTINE UpdateHistories(SurfaceGHENum, TopFlux, BottomFlux, SourceFlux, &
                           SourceTemp)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This is used to update the temperature and flux records for the QTF
          ! calculations. This is called at the start of each zone timestep.


          ! METHODOLOGY EMPLOYED:
          ! Just shift along and replace zero index element with current value.


          ! REFERENCES:
          ! n/a


          ! USE STATEMENTS:


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum             ! component number
  REAL(r64), INTENT(IN)    :: TopFlux                   ! current top (top) surface flux
  REAL(r64), INTENT(IN)    :: BottomFlux                ! current bottom (bottom) surface flux
  REAL(r64), INTENT(IN)    :: SourceFlux                ! current source surface flux
  REAL(r64), INTENT(IN)    :: SourceTemp                ! current source temperature



          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  ! update top surface temps
  SurfaceGHEQTF(SurfaceGHENum)%TtopHistory = EOSHIFT(SurfaceGHEQTF(SurfaceGHENum)%TtopHistory,SHIFT= -1)


  ! update bottom surface temps
  SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory = EOSHIFT(SurfaceGHEQTF(SurfaceGHENum)%TbtmHistory, SHIFT= -1)


  ! update bottom surface temps
  SurfaceGHEQTF(SurfaceGHENum)%TsrcHistory = EOSHIFT(SurfaceGHEQTF(SurfaceGHENum)%TsrcHistory,SHIFT= -1)
  SurfaceGHEQTF(SurfaceGHENum)%TsrcHistory(1) = SourceTemp


  ! update bottom surface fluxes
  SurfaceGHEQTF(SurfaceGHENum)%QbtmHistory = EOSHIFT(SurfaceGHEQTF(SurfaceGHENum)%QbtmHistory, SHIFT= -1)
  SurfaceGHEQTF(SurfaceGHENum)%QbtmHistory(1) = BottomFlux


  ! update bottom surface fluxes
  SurfaceGHEQTF(SurfaceGHENum)%QtopHistory = EOSHIFT(SurfaceGHEQTF(SurfaceGHENum)%QtopHistory, SHIFT= -1)
  SurfaceGHEQTF(SurfaceGHENum)%QtopHistory(1) = TopFlux


  ! update bottom surface fluxes
  SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory = EOSHIFT(SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory,SHIFT= -1)
  SurfaceGHEQTF(SurfaceGHENum)%QsrcHistory(1) = SourceFlux



END SUBROUTINE UpdateHistories


!==============================================================================


REAL(r64) FUNCTION CalcHXEffectTerm(SurfaceGHENum,Temperature,WaterMassFlow)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   December 2000
          !       MODIFIED       Simon Rees, August 2002
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the "heat exchanger"
          ! effectiveness term.  This is equal to the mass flow rate of water
          ! times the specific heat of water times the effectiveness of
          ! the surface heat exchanger. This routine is adapted from that in
          ! the low temp radiant surface model.


          ! METHODOLOGY EMPLOYED:
          ! Assumes that the only REAL(r64) heat transfer term that we have to
          ! deal with is the convection from the water to the tube.  The
          ! other assumptions are that the tube bottom surface temperature
          ! is equal to the "source location temperature" and that it is
          ! a CONSTANT throughout the surface.


          ! REFERENCES:
          ! See RadiantSystemLowTemp module.
          ! Property data for water shown below as parameters taken from
          !   Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
          ! Heat exchanger information also from Incropera and DeWitt.
          ! Code based loosely on code from IBLAST program (research version)


          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : PI
  USE General,         ONLY : RoundSigDigits
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum  ! Index number of surface under consideration
  REAL(r64),    INTENT(IN) :: Temperature    ! Temperature of water entering the surface, in C
  REAL(r64),    INTENT(IN) :: WaterMassFlow  ! Mass flow rate, in kg/s


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER    :: MaxLaminarRe       = 2300.d0    ! Maximum Reynolds number for laminar flow
  INTEGER, PARAMETER :: NumOfPropDivisions = 13       ! intervals in property correlation
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Temps=  &   ! Temperature, in C
                   (/1.85d0,6.85d0,11.85d0,16.85d0,21.85d0,26.85d0,31.85d0,36.85d0,41.85d0,  &
                    46.85d0,51.85d0,56.85d0,61.85d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Mu=  &      ! Viscosity, in Ns/m2
                   (/.001652d0,.001422d0,.001225d0,.00108d0,.000959d0,.000855d0,.000769d0,.000695d0,  &
                     .000631d0,.000577d0,.000528d0,.000489d0,.000453d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Conductivity=  &     ! Conductivity, in W/mK
                   (/.574d0,.582d0,.590d0,.598d0,.606d0,.613d0,.620d0,.628d0,.634d0,.640d0,.645d0,.650d0,.656d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Pr=  &      ! Prandtl number (dimensionless)
                   (/12.22d0,10.26d0,8.81d0,7.56d0,6.62d0,5.83d0,5.20d0,4.62d0,4.16d0,3.77d0,3.42d0,3.15d0,2.88d0/)
  INTEGER,   PARAMETER :: WaterIndex = 1

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Index
  REAL(r64)    :: InterpFrac
  REAL(r64)    :: NuD
  REAL(r64)    :: ReD
  REAL(r64)    :: NTU
  REAL(r64)    :: CpWater
  REAL(r64)    :: Kactual
  REAL(r64)    :: MUactual
  REAL(r64)    :: PRactual
  REAL(r64)    :: PipeLength
!  INTEGER, SAVE                             :: ErrCount


          ! FLOW:
          ! First find out where we are in the range of temperatures
  Index = 1
  DO WHILE (Index <= NumOfPropDivisions)
    IF (Temperature < Temps(Index)) EXIT ! DO loop
    Index = Index + 1
  END DO


          ! Initialize thermal properties of water
  IF (Index == 1) THEN
    MUactual = Mu(Index)
    Kactual  = Conductivity(Index)
    PRactual = Pr(Index)
  ELSE IF (Index > NumOfPropDivisions) THEN
    Index    = NumOfPropDivisions
    MUactual = Mu(Index)
    Kactual  = Conductivity(Index)
    PRactual = Pr(Index)
  ELSE
    InterpFrac = (Temperature-Temps(Index-1))/(Temps(Index)-Temps(Index-1))
    MUactual   = Mu(Index-1) + InterpFrac*(Mu(Index)-Mu(Index-1))
    Kactual    = Conductivity(Index-1) + InterpFrac*(Conductivity(Index)-Conductivity(Index-1))
    PRactual   = Pr(Index-1) + InterpFrac*(Pr(Index)-Pr(Index-1))
  END IF
    ! arguments are glycol name, temperature, and concentration
  IF (Temperature < 0.d0) THEN ! check if fluid is water and would be freezing
    IF(PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%FluidIndex == WaterIndex) THEN
      IF (SurfaceGHE(SurfaceGHENum)%FrozenErrIndex1 == 0) THEN
        CALL ShowWarningMessage('GroundHeatExchanger:Surface="'//trim(SurfaceGHE(SurfaceGHENum)%Name)//  &
           '", water is frozen; Model not valid. Calculated Water Temperature=['//  &
           trim(RoundSigDigits(InletTemp,2))//'] C')
        CALL ShowContinueErrorTimeStamp(' ')
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('GroundHeatExchanger:Surface="'//trim(SurfaceGHE(SurfaceGHENum)%Name)//  &
           '", water is frozen',SurfaceGHE(SurfaceGHENum)%FrozenErrIndex1, ReportMinOf=InletTemp,ReportMinUnits='[C]',  &
           ReportMaxOf=InletTemp,ReportMaxUnits='[C]')
      InletTemp = MAX(InletTemp, 0.0d0)
    ENDIF
  ENDIF
  CpWater = GetSpecificHeatGlycol(PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%FluidName,Temperature,  &
             PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%FluidIndex,'SurfaceGroundHeatExchanger:CalcHXEffectTerm')


          ! Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
  ReD = 4.0d0 * WaterMassFlow / ( PI * MUactual * SurfaceGHE(SurfaceGHENum)%TubeDiameter * &
                                SurfaceGHE(SurfaceGHENum)%TubeCircuits)


          ! Calculate the Nusselt number based on what flow regime one is in
  IF (ReD >= MaxLaminarRe) THEN ! Turbulent flow --> use Colburn equation
    NuD = 0.023d0*(ReD**(0.8d0))*(PRactual**(1.d0/3.d0))
  ELSE    ! Laminar flow --> use constant surface temperature relation
    NuD = 3.66d0
  END IF
          ! Calculate the NTU parameter
          ! NTU = UA/[(Mdot*Cp)min]
          ! where: U = h (convection coefficient) and h = (k)(Nu)/D
          !        A = Pi*D*TubeLength
          !  NTU = PI * Kactual * NuD * SurfaceGHE(SurfaceGHENum)%TubeLength / (WaterMassFlow * CpWater)



  PipeLength = SurfaceGHE(SurfaceGHENum)%SurfaceLength * SurfaceGHE(SurfaceGHENum)%SurfaceWidth / &
               SurfaceGHE(SurfaceGHENum)%TubeSpacing


  NTU = PI * Kactual * NuD * PipeLength / (WaterMassFlow * CpWater)
          ! Calculate Epsilon*MassFlowRate*Cp
  IF (-NTU >= EXP_LowerLimit) THEN
    CalcHXEffectTerm = (1.0d0-EXP(-NTU))*WaterMassFlow*CpWater
  ELSE
    CalcHXEffectTerm = 1.0d0*WaterMassFlow*CpWater
  ENDIF


  RETURN


END FUNCTION CalcHXEffectTerm


!==============================================================================


SUBROUTINE CalcTopSurfTemp (SurfaceNum, FluxTop, TempTop, ThisDrybulb, ThisWetBulb, &
                            ThisSkyTemp, ThisBeamSolarRad, ThisDifSolarRad, &
                            ThisSolarDirCosVert, ThisWindSpeed, ThisIsRain, ThisIsSnow)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is used to calculate the top surface
          ! temperature for the given surface flux.


          ! METHODOLOGY EMPLOYED:
          ! calc surface heat balance


          ! REFERENCES:
          ! USE STATEMENTS:


  USE ConvectionCoefficients, ONLY : CalcASHRAESimpExtConvectCoeff


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceNum               ! surface index number
  REAL(r64), INTENT(IN)    :: FluxTop                  ! top surface flux
  REAL(r64), INTENT(OUT)   :: TempTop                  ! top surface temperature
  REAL(r64), INTENT(IN)    :: ThisDrybulb              ! dry bulb temperature
  REAL(r64), INTENT(IN)    :: ThisWetbulb              ! wet bulb temperature
  REAL(r64), INTENT(IN)    :: ThisSkyTemp              ! sky temperature
  REAL(r64), INTENT(IN)    :: ThisBeamSolarRad         ! beam solar radiation
  REAL(r64), INTENT(IN)    :: ThisDifSolarRad          ! diffuse solar radiation
  REAL(r64), INTENT(IN)    :: ThisSolarDirCosVert      ! vertical component of solar normal
  REAL(r64), INTENT(IN)    :: ThisWindSpeed            ! wind speed
  LOGICAL, INTENT(IN) :: ThisIsRain               ! rain flag
  LOGICAL, INTENT(IN) :: ThisIsSnow               ! snow flag


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! n/a
          ! INTERFACE BLOCK SPECIFICATIONS
          ! n/a


          ! DERIVED TYPE DEFINITIONS
          ! n/a


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: ConvCoef         ! convection coefficient
  REAL(r64)    :: RadCoef          ! radiation coefficient
  REAL(r64)    :: ExternalTemp     ! external environmental temp - drybulb or wetbulb
  REAL(r64)    :: OldSurfTemp      ! previous surface temperature
  REAL(r64)    :: QSolAbsorbed     ! absorbed solar flux
  REAL(r64)    :: SurfTempAbs      ! absolute value of surface temp
  REAL(r64)    :: SkyTempAbs       ! absolute value of sky temp


  ! make a surface heat balance and solve for temperature


  ! set appropriate external temp
  IF(ThisIsSnow)THEN
    ExternalTemp = ThisWetBulb
  ELSE IF(ThisIsRain)THEN
    ExternalTemp = ThisWetBulb
  ELSE  ! normal dry conditions
    ExternalTemp = ThisDrybulb
  END IF


  ! set previous surface temp
  OldSurfTemp = SurfaceGHEQTF(SurfaceNum)%TtopHistory(1)
  ! absolute temperatures
  SurfTempAbs = OldSurfTemp + KelvinConv
  SkyTempAbs  = ThisSkyTemp + KelvinConv


  ! ASHRAE simple convection coefficient model for external surfaces.
  ConvCoef     = CalcASHRAESimpExtConvectCoeff(TopRoughness,ThisWindSpeed)
  ! radiation coefficient using surf temp from past time step
  IF (ABS(SurfTempAbs-SkyTempAbs) > SmallNum) THEN
    RadCoef      = StefBoltzmann*TopThermAbs*((SurfTempAbs**4)-(SkyTempAbs**4))/ &
                               (SurfTempAbs-SkyTempAbs)
  ELSE
    RadCoef =  0.0d0
  ENDIF


  ! total absorbed solar - no ground solar
  QSolAbsorbed = TopSolarAbs*(MAX(ThisSolarDirCosVert,0.0d0)*ThisBeamSolarRad + ThisDifSolarRad)


  ! solve for temperature
  TempTop = (FluxTop + ConvCoef*ExternalTemp + RadCoef*ThisSkyTemp + QSolAbsorbed)/ &
            (ConvCoef + RadCoef)


END SUBROUTINE CalcTopSurfTemp


!==============================================================================


SUBROUTINE CalcBottomSurfTemp (SurfaceNum, FluxBtm, TempBtm, ThisDrybulb, ThisWindSpeed, &
                               ThisGroundTemp)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is used to calculate the bottom surface
          ! temperature for the given surface flux.


          ! METHODOLOGY EMPLOYED:
          ! calc surface heat balances


          ! REFERENCES:
          ! USE STATEMENTS:


  USE ConvectionCoefficients, ONLY : CalcASHRAESimpExtConvectCoeff


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceNum         ! surface index number
  REAL(r64), INTENT(IN)    :: FluxBtm            ! bottom surface flux
  REAL(r64), INTENT(OUT)   :: TempBtm            ! bottom surface temperature
  REAL(r64), INTENT(IN)    :: ThisDrybulb        ! dry bulb temperature
  REAL(r64), INTENT(IN)    :: ThisWindSpeed      ! wind speed
  REAL(r64), INTENT(IN)    :: ThisGroundTemp     ! ground temperature


          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  REAL(r64)    :: ConvCoef         ! convection coefficient
  REAL(r64)    :: RadCoef          ! radiation coefficient
  REAL(r64)    :: OldSurfTemp      ! previous surface temperature
  REAL(r64)    :: SurfTempAbs      ! absolute value of surface temp
  REAL(r64)    :: ExtTempAbs       ! absolute value of sky temp


  IF(SurfaceGHE(SurfaceNum)%LowerSurfCond == SurfCond_Exposed)THEN


    ! make a surface heat balance and solve for temperature
    OldSurfTemp = SurfaceGHEQTF(SurfaceNum)%TbtmHistory(1)
    ! absolute temperatures
    SurfTempAbs = OldSurfTemp + KelvinConv
    ExtTempAbs  = ThisDrybulb + KelvinConv


    ! ASHRAE simple convection coefficient model for external surfaces.
    ConvCoef = CalcASHRAESimpExtConvectCoeff(TopRoughness,ThisWindSpeed)


    ! radiation coefficient using surf temp from past time step
    IF (ABS(SurfTempAbs-ExtTempAbs) > SmallNum) THEN
      RadCoef = StefBoltzmann*TopThermAbs*((SurfTempAbs**4)-(ExtTempAbs**4))/ &
                                (SurfTempAbs-ExtTempAbs)
    ELSE
      RadCoef=0.0d0
    ENDIF

    ! total absorbed solar - no ground solar
    TempBtm = (FluxBtm + ConvCoef*ThisDrybulb + RadCoef*ThisDrybulb)/ &
              (ConvCoef + RadCoef)

  ELSE ! ground coupled
    ! just use the supplied ground temperature
    TempBtm = ThisGroundTemp
  END IF


END SUBROUTINE CalcBottomSurfTemp


!==============================================================================


SUBROUTINE UpdateSurfaceGroundHeatExchngr(SurfaceGHENum)    !DSU


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does any updating that needs to be done for surface
          ! ground heat exchangers.  One of the most important functions of
          ! this routine is to update the average heat source/sink for a
          ! particular system over the various system time steps that make up
          ! the zone time step. This routine must also set the outlet water conditions.


          ! METHODOLOGY EMPLOYED:
          ! For the source/sink average update, if the system time step elapsed
          ! is still what it used to be, then either we are still iterating or
          ! we had to go back and shorten the time step.  As a result, we have
          ! to subtract out the previous value that we added.  If the system
          ! time step elapsed is different, then we just need to add the new
          ! values to the running average.


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : TimeStepZone
  USE DataHVACGlobals, ONLY : TimeStepSys, SysTimeElapsed
  USE DataLoopNode,    ONLY : Node
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop
  USE PlantUtilities,  ONLY : SafeCopyPlantNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum       ! Index for the surface
!  INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU


          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: CpFluid            ! Specific heat of working fluid
!  INTEGER,SAVE    :: ErrCount
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum

  ! update flux
  SurfaceGHEQTF(SurfaceGHENum)%QSrc = SourceFlux

  LoopNum = SurfaceGHE(SurfaceGHENum)%LoopNum
  LoopSideNum = SurfaceGHE(SurfaceGHENum)%LoopSideNum
  IF(PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock > 0)THEN  ! only update in normal mode !DSU
    IF (SurfaceGHEQTF(SurfaceGHENum)%LastSysTimeElapsed == SysTimeElapsed) THEN
      ! Still iterating or reducing system time step, so subtract old values which were
      ! not valid
       SurfaceGHEQTF(SurfaceGHENum)%QsrcAvg = SurfaceGHEQTF(SurfaceGHENum)%QsrcAvg - &
                                          SurfaceGHEQTF(SurfaceGHENum)%LastQSrc * &
                                          SurfaceGHEQTF(SurfaceGHENum)%LastTimeStepSys/TimeStepZone
    END IF


    ! Update the running average and the "last" values with the current values of the appropriate variables
    SurfaceGHEQTF(SurfaceGHENum)%QsrcAvg = SurfaceGHEQTF(SurfaceGHENum)%QsrcAvg + &
                                       SurfaceGHEQTF(SurfaceGHENum)%Qsrc *TimeStepSys/TimeStepZone


    SurfaceGHEQTF(SurfaceGHENum)%LastQSrc           = SourceFlux
    SurfaceGHEQTF(SurfaceGHENum)%LastSysTimeElapsed = SysTimeElapsed
    SurfaceGHEQTF(SurfaceGHENum)%LastTimeStepSys    = TimeStepSys


  END IF


  ! Calculate the water side outlet conditions and set the
  ! appropriate conditions on the correct HVAC node.
  IF(PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%FluidName =='WATER')THEN
    IF (InletTemp .LT. 0.0d0) THEN
      CALL ShowRecurringWarningErrorAtEnd('UpdateSurfaceGroundHeatExchngr: Water is frozen in Surf HX='//  &
                     TRIM(SurfaceGHE(SurfaceGHENum)%Name),SurfaceGHE(SurfaceGHENum)%FrozenErrIndex2,InletTemp,InletTemp)
    END IF
    InletTemp = MAX(InletTemp, 0.0d0)
  ENDIF

  CpFluid = GetSpecificHeatGlycol(PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%FluidName,InletTemp,  &
     PlantLoop(SurfaceGHE(SurfaceGHENum)%LoopNum)%FluidIndex,'SurfaceGroundHeatExchanger:Update')

  Call SafeCopyPlantNode(SurfaceGHE(SurfaceGHENum)%InletNodeNum, SurfaceGHE(SurfaceGHENum)%OutletNodeNum)
  ! check for flow
  IF ( (CpFluid > 0.0d0) .AND. (FlowRate > 0.0d0) ) THEN
    Node(SurfaceGHE(SurfaceGHENum)%OutletNodeNum)%Temp   = InletTemp - SurfaceArea*SourceFlux / (FlowRate*CpFluid)
    Node(SurfaceGHE(SurfaceGHENum)%OutletNodeNum)%Enthalpy = Node(SurfaceGHE(SurfaceGHENum)%OutletNodeNum)%Temp *CpFluid
  END IF

  RETURN


END SUBROUTINE UpdateSurfaceGroundHeatExchngr


!==============================================================================


SUBROUTINE ReportSurfaceGroundHeatExchngr(SurfaceGHENum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simply produces output for Surface ground heat exchangers


          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE DataLoopNode,    ONLY : Node


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfaceGHENum      ! Index for the surface under consideration


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na



          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


          ! FLOW:

  ! update flows and temps from node data
  SurfaceGHEReport(SurfaceGHENum)%InletTemp     = Node(SurfaceGHE(SurfaceGHENum)%InletNodeNum)%Temp
  SurfaceGHEReport(SurfaceGHENum)%OutletTemp    = Node(SurfaceGHE(SurfaceGHENum)%OutletNodeNum)%Temp
  SurfaceGHEReport(SurfaceGHENum)%MassFlowRate  = Node(SurfaceGHE(SurfaceGHENum)%InletNodeNum)%MassFlowRate


  ! update other variables from module variables
  SurfaceGHEReport(SurfaceGHENum)%HeatTransferRate      = SourceFlux*SurfaceArea
  SurfaceGHEReport(SurfaceGHENum)%SurfHeatTransferRate  = SurfaceArea*(TopSurfFlux+BtmSurfFlux)
  SurfaceGHEReport(SurfaceGHENum)%Energy                = SourceFlux*SurfaceArea*TimeStepSys*SecInHour
  SurfaceGHEReport(SurfaceGHENum)%TopSurfaceTemp        = TopSurfTemp
  SurfaceGHEReport(SurfaceGHENum)%BtmSurfaceTemp        = BtmSurfTemp
  SurfaceGHEReport(SurfaceGHENum)%TopSurfaceFlux        = TopSurfFlux
  SurfaceGHEReport(SurfaceGHENum)%BtmSurfaceFlux        = BtmSurfFlux
  SurfaceGHEReport(SurfaceGHENum)%SurfEnergy            = SurfaceArea*(TopSurfFlux+BtmSurfFlux)*TimeStepSys*SecInHour
  SurfaceGHEReport(SurfaceGHENum)%SourceTemp            = SourceTemp


  RETURN


END SUBROUTINE ReportSurfaceGroundHeatExchngr


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


END MODULE SurfaceGroundHeatExchanger