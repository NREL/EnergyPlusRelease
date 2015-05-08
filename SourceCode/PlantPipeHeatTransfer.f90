MODULE PipeHeatTransfer

  ! Module containing the routines dealing with pipes with transport delay
  ! and heat transfer.

  ! MODULE INFORMATION:
  !       AUTHOR         Simon Rees
  !       DATE WRITTEN   July 2007
  !       MODIFIED       May 2008
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate a pipe with heat transfer

  ! METHODOLOGY EMPLOYED:
  ! An implicit finite difference method is used to solve the temperature distribution of the
  ! fluid in the pipe as a result of the transport delay and heat transfer to the environment.
  ! For buried pipes, the simulation involves an implicit finite difference model of the soil,
  ! which was originally based on Piechowski's thesis (below).  Equation numbers for
  ! pipe:underground calculations are from Piechowski's thesis.  In Piechowski, the near-pipe
  ! region is solved with a detailed finite difference grid, this current model makes use of
  ! the Hanby model to simulate the actual pipe.

  ! Kusuda, T. & Achenbach, P. (1965), ‘Earth temperature and thermal diffusivity at
  !     selected stations in the united states’, ASHRAE Transactions 71(1), 61-75.
  ! Piechowski, M. (1996), A Ground Coupled Heat Pump System with Energy Storage,
  !     PhD thesis, University of Melbourne.

  ! OTHER NOTES: Equation Numbers listed in buried pipe routines are from Piechowski's thesis

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,       ONLY : MaxNameLength
USE DataInterfaces
USE DataPlant,         ONLY : TypeOf_PipeExterior,TypeOf_PipeInterior, TypeOf_PipeUnderground

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER :: Blank = ' '

  INTEGER, PARAMETER  :: None               = 0
  INTEGER, PARAMETER  :: ZoneEnv            = 1
  INTEGER, PARAMETER  :: ScheduleEnv        = 2
  INTEGER, PARAMETER  :: OutsideAirEnv      = 3
  INTEGER, PARAMETER  :: GroundEnv          = 4

  INTEGER, PARAMETER  :: PreviousTimeIndex  = 1
  INTEGER, PARAMETER  :: CurrentTimeIndex   = 2
  INTEGER, PARAMETER  :: TentativeTimeIndex = 3

  REAL(r64), PARAMETER :: InnerDeltaTime = 60.0d0 !one minute time step in seconds

  ! DERIVED TYPE DEFINITIONS
TYPE PipeHTData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                  = Blank  ! name of the component
  CHARACTER(len=MaxNameLength) :: Construction          = Blank  ! construction object name
  CHARACTER(len=MaxNameLength) :: Environment           = Blank  ! keyword:  'Schedule', 'OutdoorAir', 'Zone'
  CHARACTER(len=MaxNameLength) :: EnvrSchedule          = Blank  ! temperature schedule for environmental temp
  CHARACTER(len=MaxNameLength) :: EnvrVelSchedule       = Blank  ! temperature schedule for environmental temp
  CHARACTER(len=MaxNameLength) :: EnvrZone              = Blank  ! zone providing environmental temp
  CHARACTER(len=MaxNameLength) :: EnvrAirNode           = Blank  ! outside air node providing environmental temp
  REAL(r64)                    :: Length                =0.0d0     ! total pipe length [m]
  REAL(r64)                    :: PipeID                =0.0d0     ! pipe inside diameter [m]
  CHARACTER(len=MaxNameLength) :: InletNode             = Blank  ! inlet node name
  CHARACTER(len=MaxNameLength) :: OutletNode            = Blank  ! outlet node name
  INTEGER                      :: InletNodeNum          =0       ! inlet node number
  INTEGER                      :: OutletNodeNum         =0       ! outlet node number
  INTEGER                      :: TypeOf                =0       ! Type of pipe
  ! derived data
  INTEGER                      :: ConstructionNum       =0       ! construction ref number
  INTEGER                      :: EnvironmentPtr        =0
  INTEGER                      :: EnvrSchedPtr          =0       ! pointer to schedule used to set environmental temp
  INTEGER                      :: EnvrVelSchedPtr       =0       ! pointer to schedule used to set environmental temp
  INTEGER                      :: EnvrZonePtr           =0       ! pointer to zone number used to set environmental temp
  INTEGER                      :: EnvrAirNodeNum        =0       ! pointer to outside air node used to set environmental temp
  INTEGER                      :: NumSections           =0       ! total number of nodes along pipe length
  REAL(r64)                    :: FluidSpecHeat         =0.0d0     ! fluid Cp [J/kg.K]
  REAL(r64)                    :: FluidDensity          =0.0d0     ! density [kg/m3]
  REAL(r64)                    :: MaxFlowRate           =0.0d0     ! max flow rate (from loop/node data)
  REAL(r64)                    :: FluidSectionVol       =0.0d0     ! volume of each pipe section (node) [m^3]
  REAL(r64)                    :: InsideArea            =0.0d0     ! pipe section inside surface area [m^2]
  REAL(r64)                    :: OutsideArea           =0.0d0     ! pipe section outside surface area [m^2]
  REAL(r64)                    :: SectionArea           =0.0d0     ! cross sectional area [m^2]
  REAL(r64)                    :: PipeHeatCapacity      =0.0d0     ! heat capacity of pipe section [J/m.K]
  REAL(r64)                    :: PipeOD                =0.0d0     ! pipe outside diameter [m]
  REAL(r64)                    :: PipeCp                =0.0d0     ! pipe materail Cp [J/kg.K]
  REAL(r64)                    :: PipeDensity           =0.0d0     ! pipe material density [kg/m3]
  REAL(r64)                    :: PipeConductivity      =0.0d0     ! pipe material thermal conductivity [W/m.K]
  REAL(r64)                    :: InsulationOD          =0.0d0     ! insulation outside diameter [m]
  REAL(r64)                    :: InsulationCp          =0.0d0     ! insulation  specific heat [J/kg.K]
  REAL(r64)                    :: InsulationDensity     =0.0d0     ! insulation density [kg/m3]
  REAL(r64)                    :: InsulationConductivity=0.0d0     ! insulation conductivity [W/m.K]
  REAL(r64)                    :: InsulationThickness   =0.0d0     ! insulation thickness [m]
  REAL(r64)                    :: InsulationResistance  =0.0d0     ! Insulation thermal resistance [m2.K/W]
  REAL(r64)                    :: CurrentSimTime        =0.0d0     ! Current simulation time [hr]
  REAL(r64)                    :: PreviousSimTime       =0.0d0     ! simulation time the report data was last updated
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TentativeFluidTemp
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: FluidTemp              ! arrays for fluid and pipe temperatures at each node
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PreviousFluidTemp
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TentativePipeTemp
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PipeTemp
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: PreviousPipeTemp
  INTEGER                      :: NumDepthNodes         =0       ! number of soil grid points in the depth direction
  INTEGER                      :: PipeNodeDepth         =0       ! soil depth grid point where pipe is located
  INTEGER                      :: PipeNodeWidth         =0       ! soil width grid point where pipe is located
  REAL(r64)                    :: PipeDepth             =0.0d0     ! pipe burial depth [m]
  REAL(r64)                    :: DomainDepth           =0.0d0     ! soil grid depth [m]
  REAL(r64)                    :: dSregular             =0.0d0     ! grid spacing in cartesian domain [m]
  REAL(r64)                    :: OutdoorConvCoef       =0.0d0     ! soil to air convection coefficient [W/m2.K]
  CHARACTER(len=MaxNameLength) :: SoilMaterial          =Blank   ! name of soil material:regular object
  INTEGER                      :: SoilMaterialNum       =0       ! soil material index in material data structure
  INTEGER                      :: MonthOfMinSurfTemp    =0       ! month of minimum ground surface temperature
  REAL(r64)                    :: AvgGroundTemp         =0.0d0     ! annual average ground temperature [C]
  REAL(r64)                    :: AvgGndTempAmp         =0.0d0     ! annual average amplitude of gnd temp [C]
  INTEGER                      :: PhaseShiftDays        =0       ! shift of minimum gnd surf temp from 1/1  [days]
  REAL(r64)                    :: MinSurfTemp           =0.0d0     ! minimum annual surface temperature [C]
  REAL(r64)                    :: SoilDensity           =0.0d0     ! density of soil [kg/m3]
  REAL(r64)                    :: SoilDepth             =0.0d0     ! thickness of soil [m]
  REAL(r64)                    :: SoilCp                =0.0d0     ! specific heat of soil [J/kg.K]
  REAL(r64)                    :: SoilConductivity      =0.0d0     ! thermal conductivity of soil [W/m.K]
  REAL(r64)                    :: SoilRoughness         =0.0d0     ! ground surface roughness
  REAL(r64)                    :: SoilThermAbs          =0.0d0     ! ground surface thermal absorptivity
  REAL(r64)                    :: SoilSolarAbs          =0.0d0     ! ground surface solar absorptivity
  REAL(r64)                    :: CoefS1                =0.0d0     ! soil surface finite difference coefficient
  REAL(r64)                    :: CoefS2                =0.0d0     ! soil surface finite difference coefficient
  REAL(r64)                    :: CoefA1                =0.0d0     ! soil finite difference coefficient
  REAL(r64)                    :: CoefA2                =0.0d0     ! soil finite difference coefficient
  REAL(r64)                    :: FourierDS             =0.0d0     ! soil Fourier number based on grid spacing
  REAL(r64)                    :: SoilDiffusivity       =0.0d0     ! soil thermal diffusivity [m2/s]
  REAL(r64)                    :: SoilDiffusivityPerDay =0.0d0     ! soil thermal diffusivity [m2/day]
  INTEGER                      :: AvgAnnualManualInput  =0       ! flag for method of bringing in annual avg data yes-1 no-0
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: T                ! soil temperature array
  LOGICAL                      :: BeginSimInit          =.TRUE.  ! begin sim and begin environment flag
  LOGICAL                      :: BeginSimEnvrn         =.TRUE.  ! begin sim and begin environment flag
  LOGICAL                      :: FirstHVACupdateFlag   =.TRUE.
  LOGICAL                      :: BeginEnvrnupdateFlag  =.TRUE.
  LOGICAL                      :: SolarExposed          =.TRUE.  ! Flag to determine if solar is included at ground surface
  REAL(r64)                    :: SumTK                 =0.0d0     ! Sum of thickness/conductivity over all material layers
  REAL(r64)                    :: ZoneHeatGainRate      =0.0d0     ! Lagged energy summation for zone heat gain {W}
  INTEGER                      :: LoopNum               =0       ! PlantLoop index where this pipe lies
  INTEGER                      :: LoopSideNum           =0       ! PlantLoop%LoopSide index where this pipe lies
  INTEGER                      :: BranchNum             =0       ! ..LoopSide%Branch index where this pipe lies
  INTEGER                      :: CompNum               =0       ! ..Branch%Comp index where this pipe lies
  LOGICAL                      :: CheckEquipName        =.TRUE.
END TYPE PipeHTData

TYPE PipeHeatTransferReport
  ! Report data
  REAL(r64)                    :: FluidInletTemp          =0.0d0 ! inlet temperature [C]
  REAL(r64)                    :: FluidOutletTemp         =0.0d0 ! outlet temperature [C]
  REAL(r64)                    :: MassFlowRate            =0.0d0 ! mass flow rate [kg/s]
  REAL(r64)                    :: FluidHeatLossRate       =0.0d0 ! overall heat transfer rate from fluid to pipe [W]
  REAL(r64)                    :: FluidHeatLossEnergy     =0.0d0 ! energy transferred from fluid to pipe [J]
  REAL(r64)                    :: PipeInletTemp           =0.0d0 ! pipe temperature at inlet [C]
  REAL(r64)                    :: PipeOutletTemp          =0.0d0 ! pipe temperature at Oulet [C]
  REAL(r64)                    :: EnvironmentHeatLossRate =0.0d0 ! overall heat transfer rate from pipe to environment [W]
  REAL(r64)                    :: EnvHeatLossEnergy       =0.0d0 ! energy transferred from pipe to environment [J]
  REAL(r64)                    :: VolumeFlowRate          =0.0d0
END TYPE PipeHeatTransferReport

! the model data structures
TYPE(PipeHTData), DIMENSION(:), ALLOCATABLE             :: PipeHT
TYPE(PipeHeatTransferReport), DIMENSION(:), ALLOCATABLE :: PipeHTReport

  ! MODULE VARIABLE DECLARATIONS:
INTEGER      :: NumOfPipeHT            =0        ! Number of Pipe Heat Transfer objects
INTEGER      :: InletNodeNum           =0        ! module variable for inlet node number
INTEGER      :: OutletNodeNum          =0        ! module variable for outlet node number
INTEGER      :: PipeHTNum              =0        ! object index
REAL(r64)    :: MassFlowRate           =0.0d0      ! pipe mass flow rate
REAL(r64)    :: VolumeFlowRate         =0.0d0      ! pipe volumetric flow rate
REAL(r64)    :: DeltaTime              =0.0d0      ! time change from last update
REAL(r64)    :: InletTemp              =0.0d0      ! pipe inlet temperature
REAL(r64)    :: OutletTemp             =0.0d0      ! pipe outlet temperature
REAL(r64)    :: EnvironmentTemp        =0.0d0      ! environmental temperature (surrounding pipe)
REAL(r64)    :: EnvHeatLossRate        =0.0d0      ! heat loss rate from pipe to the environment
REAL(r64)    :: FluidHeatLossRate      =0.0d0      ! overall heat loss from fluid to pipe
LOGICAL      :: GetPipeInputFlag       = .TRUE.  ! First time, input is "gotten"
INTEGER      :: NumInnerTimeSteps      =0        ! the number of "inner" time steps for our model

  ! SUBROUTINE SPECIFICATIONS FOR MODULE
PUBLIC  SimPipesHeatTransfer
PUBLIC  GetPipesHeatTransfer   ! this must be called by SimPipes
PRIVATE InitPipesHeatTransfer
PUBLIC  InitializeHeatTransferPipes
PRIVATE CalcPipesHeatTransfer
PRIVATE CalcBuriedPipeSoil
PRIVATE UpdatePipesHeatTransfer
PRIVATE ReportPipesHeatTransfer
PUBLIC  CalcZonePipesHeatGain
PRIVATE OutsidePipeHeatTransCoef
PRIVATE CalcPipeHeatTransCoef
PRIVATE TBND

CONTAINS

!==============================================================================

SUBROUTINE SimPipesHeatTransfer(EquipType,EquipName,EqNum,InitLoopEquip,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the public interface to this component.
          ! Other calcs are made by calling private routines.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY : FindItemInList
  USE General,         ONLY: TrimSigDigits
  USE DataLoopNode,    ONLY : Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: EquipType
  CHARACTER(len=*), INTENT(IN)    :: EquipName        ! name of the Pipe Heat Transfer.
  INTEGER,          INTENT(INOUT) :: EqNum            ! index in local derived types for external calling
  LOGICAL,          INTENT(IN)    :: FirstHVACIteration       ! component number
  LOGICAL,          INTENT(IN)    :: InitLoopEquip

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InnerTimeStepCtr

            ! check for input
  IF (GetPipeInputFlag) THEN
    CALL GetPipesHeatTransfer
    GetPipeInputFlag=.FALSE.
  ENDIF

  IF (EqNum == 0) THEN
      PipeHTNum=FindItemInList(EquipName,PipeHT%Name,NumOfPipeHT)
      IF (PipeHTNum == 0) THEN
      CALL ShowFatalError('SimPipeHeatTransfer: Pipe:heat transfer requested not found=' &
                           //TRIM(EquipName)) ! Catch any bad names before crashing
      ENDIF
      EqNum=PipeHTNum
  ELSE
    PipeHTNum=EqNum
    IF (PipeHTNum > NumOfPipeHT .or. PipeHTNum < 1) THEN
      CALL ShowFatalError('SimPipeHeatTransfer:  Invalid component index passed='//  &
                          TRIM(TrimSigDigits(PipeHTNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfPipeHT))//  &
                          ', Entered Unit name='//TRIM(EquipName))
    ENDIF
    IF (PipeHT(PipeHTNum)%CheckEquipName) THEN
      IF (EquipName /= PipeHT(PipeHTNum)%Name) THEN
        CALL ShowFatalError('SimPipeHeatTransfer: Invalid component name passed='//  &
                            TRIM(TrimSigDigits(PipeHTNum))// &
                            ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                            TRIM(PipeHT(PipeHTNum)%Name))
      ENDIF
      PipeHT(PipeHTNum)%CheckEquipName=.false.
    ENDIF
  ENDIF

  IF(InitLoopEquip) RETURN
  ! initialize
  CALL InitPipesHeatTransfer(EquipType,PipeHTNum,FirstHVACIteration)
  ! make the calculations
  DO InnerTimeStepCtr = 1, NumInnerTimeSteps
      SELECT CASE (PipeHT(PipeHTNum)%EnvironmentPtr)
        CASE(GroundEnv)
          CALL CalcBuriedPipeSoil(PipeHTNum)
        CASE DEFAULT
          CALL CalcPipesHeatTransfer(PipeHTNum)
      END SELECT
      CALL PushInnerTimeStepArrays(PipeHTNum)
  END DO
  ! update vaiables
  CALL UpdatePipesHeatTransfer
  ! update report variables
  CALL ReportPipesHeatTransfer(PipeHTNum)

  RETURN

END SUBROUTINE SimPipesHeatTransfer

!==============================================================================

SUBROUTINE PushInnerTimeStepArrays(PipeHTNum)

INTEGER, INTENT(IN) :: PipeHTNum
INTEGER :: LengthIndex, DepthIndex, WidthIndex

    IF (PipeHT(PipeHTNum)%EnvironmentPtr .EQ. GroundEnv) THEN
       Do LengthIndex = 2, PipeHT(PipeHTNum)%NumSections
        Do DepthIndex = 1,  PipeHT(PipeHTNum)%NumDepthNodes
          Do WidthIndex = 2, PipeHT(PipeHTNum)%PipeNodeWidth
           !This will store the old 'current' values as the new 'previous values'  This allows
           ! us to use the previous time array as history terms in the equations
           PipeHT(PipeHTNum)%T(PreviousTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
             PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex)
          EndDo
        EndDo
      EndDo
    ENDIF

    !Then update the Hanby near pipe model temperatures
    PipeHT(PipeHTNum)%PreviousFluidTemp = PipeHT(PipeHTNum)%FluidTemp
    PipeHT(PipeHTNum)%PreviousPipeTemp  = PipeHT(PipeHTNum)%PipeTemp

   RETURN

END SUBROUTINE

SUBROUTINE GetPipesHeatTransfer

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for hydronic Pipe Heat Transfers
          ! from the user input file.  This will contain all of the information
          ! needed to define and simulate the surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:


          ! USE STATEMENTS:
  USE DataGlobals,           ONLY : NumOfZones,SecInHour,PI
  USE DataHeatBalance,       ONLY : Construct, TotConstructs, Zone, Material, TotMaterials, &
                                    IntGainTypeOf_PipeIndoor
  USE InputProcessor,        ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList, &
                                    SameString, VerifyName, MakeUPPERCase
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,      ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY : TestCompSet
  USE General,               ONLY : RoundSigDigits
  USE DataLoopNode
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
 INTEGER, PARAMETER   :: NumPipeSections    = 20
 INTEGER, PARAMETER   :: NumberOfDepthNodes = 8  ! Number of nodes in the cartesian grid-Should be an even # for now
 REAL(r64), PARAMETER :: SecondsInHour      = SecInHour
 REAL(r64), PARAMETER :: HoursInDay         = 24.0d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input,
  LOGICAL                        :: IsNotOK=.false.
  LOGICAL                        :: IsBlank=.false.

                                                         ! fatal at end of routine
  INTEGER                        :: IOStatus             ! Used in GetObjectItem
  INTEGER                        :: Item                 ! Item to be "gotten"
  INTEGER                        :: PipeItem
  INTEGER                        :: NumAlphas            ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers           ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: NumFluids            ! number of fluids in sim.
  INTEGER                        :: NumOfPipeHTInt       ! Number of Pipe Heat Transfer objects
  INTEGER                        :: NumOfPipeHTExt       ! Number of Pipe Heat Transfer objects
  INTEGER                        :: NumOfPipeHTUG        ! Number of Pipe Heat Transfer objects
  INTEGER                        :: NumSections          ! total number of sections in pipe


          ! Initializations and allocations
  cCurrentModuleObject = 'Pipe:Indoor'
  NumOfPipeHTInt = GetNumObjectsFound(cCurrentModuleObject)
  cCurrentModuleObject = 'Pipe:Outdoor'
  NumOfPipeHTExt = GetNumObjectsFound(cCurrentModuleObject)
  cCurrentModuleObject = 'Pipe:Underground'
  NumOfPipeHTUG  = GetNumObjectsFound(cCurrentModuleObject)

  NumofPipeHT = NumOfPipeHTInt + NumOfPipeHTExt + NumOfPipeHTUG
  ! allocate data structures
  IF(ALLOCATED(PipeHT)) DEALLOCATE(PipeHT)
  IF(ALLOCATED(PipeHTReport)) DEALLOCATE(PipeHTReport)

  ALLOCATE(PipeHT(NumOfPipeHT))
  ALLOCATE(PipeHTReport(NumOfPipeHT))

!  Numbers = 0.0
!  Alphas = Blank
  Item=0

  cCurrentModuleObject = 'Pipe:Indoor'
  DO PipeItem = 1, NumOfPipeHTInt
    Item = Item+1
    ! get the object name
    CALL GetObjectItem(cCurrentModuleObject,PipeItem,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PipeHT%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PipeHT(Item)%Name = cAlphaArgs(1)
    PipeHT(Item)%TypeOf = TypeOf_PipeInterior

    ! General user input data
    PipeHT(Item)%Construction = cAlphaArgs(2)
    PipeHT(Item)%ConstructionNum  = FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

    IF (PipeHT(Item)%ConstructionNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
!    ELSE
!      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
!         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
    END IF

    !get inlet node data
    PipeHT(Item)%InletNode = cAlphaArgs(3)
    PipeHT(Item)%InletNodeNum  = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    IF (PipeHT(Item)%InletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! get outlet node data
    PipeHT(Item)%OutletNode = cAlphaArgs(4)
    PipeHT(Item)%OutletNodeNum  = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    IF (PipeHT(Item)%OutletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Pipe Nodes')

    ! get environmental boundary condition type

    IF(lAlphaFieldBlanks(5)) cAlphaArgs(5) = 'ZONE'

    SELECT CASE (cAlphaArgs(5))

     CASE ('ZONE')
       PipeHT(Item)%EnvironmentPtr = ZoneEnv
       PipeHT(Item)%EnvrZone = cAlphaArgs(6)
       PipeHT(Item)%EnvrZonePtr = FindItemInList(cAlphaArgs(6),Zone%Name,NumOfZones)
       IF (PipeHT(Item)%EnvrZonePtr .EQ. 0) THEN
         CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
         ErrorsFound = .TRUE.
       END IF

     CASE ('SCHEDULE')
       PipeHT(Item)%EnvironmentPtr = ScheduleEnv
       PipeHT(Item)%EnvrSchedule = cAlphaArgs(7)
       PipeHT(Item)%EnvrSchedPtr = GetScheduleIndex(PipeHT(Item)%EnvrSchedule)
       PipeHT(Item)%EnvrVelSchedule = cAlphaArgs(8)
       PipeHT(Item)%EnvrVelSchedPtr = GetScheduleIndex(PipeHT(Item)%EnvrVelSchedule)
       IF (PipeHT(Item)%EnvrSchedPtr .EQ. 0) THEN
         CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
         ErrorsFound = .TRUE.
       END IF
       IF (PipeHT(Item)%EnvrVelSchedPtr .EQ. 0) THEN
         CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
         ErrorsFound = .TRUE.
       END IF

     CASE DEFAULT
       CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
       CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
       CALL ShowContinueError('Should be "ZONE" or "SCHEDULE"')  !TODO rename point
       ErrorsFound = .TRUE.

    END SELECT

    ! dimensions
    PipeHT(Item)%PipeID = rNumericArgs(1)
    IF (rNumericArgs(1) <= 0.0d0) THEN ! not really necessary because idd field has "minimum> 0"
      CALL ShowSevereError('GetPipesHeatTransfer: invalid '//TRIM(cNumericFieldNames(1))//  &
         ' of '//TRIM(RoundSigDigits(rNumericArgs(1), 4)) )
      CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' must be > 0.0')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))

      ErrorsFound=.true.
    END IF

    PipeHT(Item)%Length = rNumericArgs(2)
    IF (rNumericArgs(2) <= 0.0d0) THEN ! not really necessary because idd field has "minimum> 0"
      CALL ShowSevereError('GetPipesHeatTransfer: invalid '//TRIM(cNumericFieldNames(2))//' of '//  &
         TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
      CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' must be > 0.0')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    IF (PipeHT(Item)%ConstructionNum /= 0) THEN
      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
    END IF

  END DO  ! end of input loop

  cCurrentModuleObject = 'Pipe:Outdoor'
  DO PipeItem = 1, NumOfPipeHTExt
    Item = Item + 1
    ! get the object name
    CALL GetObjectItem(cCurrentModuleObject,PipeItem,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PipeHT%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PipeHT(Item)%Name = cAlphaArgs(1)
    PipeHT(Item)%TypeOf = TypeOf_PipeExterior

    ! General user input data
    PipeHT(Item)%Construction = cAlphaArgs(2)
    PipeHT(Item)%ConstructionNum  = FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

    IF (PipeHT(Item)%ConstructionNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
!    ELSE
!      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
!         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
    END IF

    !get inlet node data
    PipeHT(Item)%InletNode = cAlphaArgs(3)
    PipeHT(Item)%InletNodeNum  = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    IF (PipeHT(Item)%InletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! get outlet node data
    PipeHT(Item)%OutletNode = cAlphaArgs(4)
    PipeHT(Item)%OutletNodeNum  = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    IF (PipeHT(Item)%OutletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Pipe Nodes')

    ! get environmental boundary condition type
!    PipeHT(Item)%Environment = 'OutdoorAir'
    PipeHT(Item)%EnvironmentPtr = OutsideAirEnv

    PipeHT(Item)%EnvrAirNode = cAlphaArgs(5)
    PipeHT(Item)%EnvrAirNodeNum = GetOnlySingleNode(cAlphaArgs(5), ErrorsFound, &
        TRIM(cCurrentModuleObject), cAlphaArgs(1), NodeType_Air, NodeConnectionType_OutsideAirReference, 1, ObjectIsNotParent)
    IF (.not. lAlphaFieldBlanks(5) ) THEN
      IF (.not. CheckOutAirNodeNumber(PipeHT(Item)%EnvrAirNodeNum)) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Outdoor Air Node not on OutdoorAir:NodeList or OutdoorAir:Node')
        ErrorsFound=.true.
      ENDIF
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('An '//TRIM(cAlphaFieldNames(5))//' must be used ')
      ErrorsFound = .TRUE.
    ENDIF

    ! dimensions
    PipeHT(Item)%PipeID = rNumericArgs(1)
    IF (rNumericArgs(1) <= 0.0d0) THEN ! not really necessary because idd field has "minimum> 0"
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//' of '//TRIM(RoundSigDigits(rNumericArgs(1), 4)) )
      CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' must be > 0.0')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    PipeHT(Item)%Length = rNumericArgs(2)
    IF (rNumericArgs(2) <= 0.0d0) THEN ! not really necessary because idd field has "minimum> 0"
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//' of '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
      CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' must be > 0.0')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    IF (PipeHT(Item)%ConstructionNum /= 0) THEN
      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
    END IF

  END DO  ! end of input loop

  cCurrentModuleObject = 'Pipe:Underground'
  DO PipeItem = 1, NumOfPipeHTUG

    Item = Item + 1
    ! get the object name
    CALL GetObjectItem(cCurrentModuleObject,PipeItem,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PipeHT%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PipeHT(Item)%Name = cAlphaArgs(1)
    PipeHT(Item)%TypeOf = TypeOf_PipeUnderground

    ! General user input data
    PipeHT(Item)%Construction = cAlphaArgs(2)
    PipeHT(Item)%ConstructionNum  = FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

    IF (PipeHT(Item)%ConstructionNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
!    ELSE
!      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
!         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
    END IF

    !get inlet node data
    PipeHT(Item)%InletNode = cAlphaArgs(3)
    PipeHT(Item)%InletNodeNum  = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    IF (PipeHT(Item)%InletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! get outlet node data
    PipeHT(Item)%OutletNode = cAlphaArgs(4)
    PipeHT(Item)%OutletNodeNum  = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    IF (PipeHT(Item)%OutletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Pipe Nodes')

    PipeHT(Item)%EnvironmentPtr = GroundEnv

    ! Solar inclusion flag
    ! A6,  \field Sun Exposure
    IF (SameString(cAlphaArgs(5),'SUNEXPOSED')) THEN
      PipeHT(Item)%SolarExposed = .TRUE.
    ELSE IF (SameString(cAlphaArgs(5),'NOSUN')) THEN
      PipeHT(Item)%SolarExposed = .FALSE.
    ELSE
      Call ShowSevereError('GetPipesHeatTransfer: invalid key for sun exposure flag for '//TRIM(cAlphaArgs(1)))
      Call ShowContinueError('Key should be either SunExposed or NoSun.  Entered Key: '//TRIM(cAlphaArgs(5)))
      ErrorsFound = .TRUE.
    ENDIF

    ! dimensions
    PipeHT(Item)%PipeID = rNumericArgs(1)
    IF (rNumericArgs(1) <= 0.0d0) THEN! not really necessary because idd field has "minimum> 0"
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//' of '//TRIM(RoundSigDigits(rNumericArgs(1), 4)) )
      CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' must be > 0.0')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    PipeHT(Item)%Length = rNumericArgs(2)
    IF (rNumericArgs(2) <= 0.0d0) THEN ! not really necessary because idd field has "minimum> 0"
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//' of '//TRIM(RoundSigDigits(rNumericArgs(2), 4)) )
      CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' must be > 0.0')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! Also get the soil material name
    ! A7,  \field Soil Material
    PipeHT(Item)%SoilMaterial = cAlphaArgs(6)
    PipeHT(Item)%SoilMaterialNum  = FindIteminList(cAlphaArgs(6),Material%Name,TotMaterials)
    IF (PipeHT(Item)%SoilMaterialNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(PipeHT(Item)%SoilMaterial))
      CALL ShowContinueError('Found in '//TRIM(cCurrentModuleObject)//'='//TRIM(PipeHT(Item)%Name))
      ErrorsFound=.true.
    ELSE
      PipeHT(Item)%SoilDensity=Material(PipeHT(Item)%SoilMaterialNum)%Density
      PipeHT(Item)%SoilDepth=Material(PipeHT(Item)%SoilMaterialNum)%Thickness
      PipeHT(Item)%SoilCp=Material(PipeHT(Item)%SoilMaterialNum)%SpecHeat
      PipeHT(Item)%SoilConductivity=Material(PipeHT(Item)%SoilMaterialNum)%Conductivity
      PipeHT(Item)%SoilThermAbs=Material(PipeHT(Item)%SoilMaterialNum)%AbsorpThermal
      PipeHT(Item)%SoilSolarAbs=Material(PipeHT(Item)%SoilMaterialNum)%AbsorpSolar
      PipeHT(Item)%SoilRoughness=Material(PipeHT(Item)%SoilMaterialNum)%Roughness
      PipeHT(Item)%PipeDepth=PipeHT(Item)%SoilDepth+PipeHT(Item)%PipeID/2.0d0
      PipeHT(Item)%DomainDepth=PipeHT(Item)%PipeDepth*2.0d0
      PipeHT(Item)%SoilDiffusivity=PipeHT(Item)%SoilConductivity/(PipeHT(Item)%SoilDensity*PipeHT(Item)%SoilCp)
      PipeHT(Item)%SoilDiffusivityPerDay=PipeHT(Item)%SoilDiffusivity*SecondsInHour*HoursInDay

      ! Mesh the cartesian domain
      PipeHT(Item)%NumDepthNodes=NumberOfDepthNodes
      PipeHT(Item)%PipeNodeDepth = PipeHT(Item)%NumDepthNodes/2
      PipeHT(Item)%PipeNodeWidth = PipeHT(Item)%NumDepthNodes/2
      PipeHT(Item)%DomainDepth = PipeHT(Item)%PipeDepth * 2.0d0
      PipeHT(Item)%dSregular = PipeHT(Item)%DomainDepth / (PipeHT(Item)%NumDepthNodes-1)
    END IF

    ! Now we need to see if average annual temperature data is brought in here
    IF (NumNumbers .GE. 3) THEN
      PipeHT(Item)%AvgAnnualManualInput=1

      !If so, we need to read in the data
      ! N3,  \field Average soil surface temperature
      PipeHT(Item)%AvgGroundTemp=rNumericArgs(3)
      !IF (PipeHT(Item)%AvgGroundTemp == 0) THEN
      !  CALL ShowSevereError('GetPipesHeatTransfer: Invalid Average Ground Temp for PIPE:UNDERGROUND=' &
      !                        //TRIM(PipeHT(Item)%Name))
      !  CALL ShowContinueError('If any one annual ground temperature item is entered, all 3 items must be entered')
      !  ErrorsFound=.true.
      !ENDIF

      ! N4,  \field Amplitude of soil surface temperature
      IF (NumNumbers >= 4) THEN
        PipeHT(Item)%AvgGndTempAmp=rNumericArgs(4)
        IF (PipeHT(Item)%AvgGndTempAmp < 0.0d0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(4))//'='//TRIM(RoundSigDigits(PipeHT(Item)%AvgGndTempAmp,2)))
          CALL ShowContinueError('Found in '//TRIM(cCurrentModuleObject)//'='//TRIM(PipeHT(Item)%Name))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      ! N5;  \field Phase constant of soil surface temperature
      IF (NumNumbers >= 5) THEN
        PipeHT(Item)%PhaseShiftDays=rNumericArgs(5)
        IF (PipeHT(Item)%PhaseShiftDays < 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(5))//'='//TRIM(RoundSigDigits(PipeHT(Item)%PhaseShiftDays)))
          CALL ShowContinueError('Found in '//TRIM(cCurrentModuleObject)//'='//TRIM(PipeHT(Item)%Name))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      IF (NumNumbers >=3 .and. NumNumbers < 5) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(PipeHT(Item)%Name))
        CALL ShowContinueError('If any one annual ground temperature item is entered, all 3 items must be entered')
        ErrorsFound=.true.
      ENDIF

    ENDIF

    IF (PipeHT(Item)%ConstructionNum /= 0) THEN
      CALL ValidatePipeConstruction(TRIM(cCurrentModuleObject),TRIM(cAlphaArgs(2)),TRIM(cAlphaFieldNames(2)),  &
         PipeHT(Item)%ConstructionNum,Item,ErrorsFound)
    END IF

    ! Select number of pipe sections.  Hanby's optimal number of 20 section is selected.
    NumSections = NumPipeSections
    PipeHT(Item)%NumSections = NumPipeSections

      ! For buried pipes, we need to allocate the cartesian finite difference array
    ALLOCATE(PipeHT(Item)%T(TentativeTimeIndex, &
                                PipeHT(Item)%NumSections, &
                                PipeHT(Item)%NumDepthNodes, &
                                PipeHT(Item)%PipeNodeWidth))
    PipeHT(Item)%T=0.0d0

  END DO  ! PipeUG input loop

  DO Item=1,NumofPipeHT
    ! Select number of pipe sections.  Hanby's optimal number of 20 section is selected.
    NumSections = NumPipeSections
    PipeHT(Item)%NumSections = NumPipeSections

    ! We need to allocate the Hanby model arrays for all pipes, including buried
    ALLOCATE(PipeHT(Item)%TentativeFluidTemp(0:NumSections), PipeHT(Item)%TentativePipeTemp(0:NumSections), &
             PipeHT(Item)%FluidTemp(0:NumSections), PipeHT(Item)%PreviousFluidTemp(0:NumSections), &
             PipeHT(Item)%PipeTemp(0:NumSections), PipeHT(Item)%PreviousPipeTemp(0:NumSections))

    PipeHT(Item)%TentativeFluidTemp = 0.0d0
    PipeHT(Item)%FluidTemp          = 0.0d0
    PipeHT(Item)%PreviousFluidTemp  = 0.0d0
    PipeHT(Item)%TentativePipeTemp  = 0.0d0
    PipeHT(Item)%PipeTemp           = 0.0d0
    PipeHT(Item)%PreviousPipeTemp   = 0.0d0

    ! work out heat transfer areas (area per section)
    PipeHT(Item)%InsideArea  = PI * PipeHT(Item)%PipeID * PipeHT(Item)%Length/NumSections
    PipeHT(Item)%OutsideArea = PI * (PipeHT(Item)%PipeOD + 2*PipeHT(Item)%InsulationThickness) * &
                                        PipeHT(Item)%Length/NumSections

    ! cross sectional area
    PipeHT(Item)%SectionArea = PI * 0.25d0 * PipeHT(Item)%PipeID**2

    ! pipe & insulation mass
    PipeHT(Item)%PipeHeatCapacity = PipeHT(Item)%PipeCp * PipeHT(Item)%PipeDensity * &          ! the metal component
                                      (PI * 0.25d0 * PipeHT(Item)%PipeOD**2 - PipeHT(Item)%SectionArea)
  ENDDO

  ! final error check
  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetPipesHeatTransfer: Errors found in input. Preceding conditions cause termination.')
  END IF

  ! Set up the output variables CurrentModuleObject='Pipe:Indoor/Outdoor/Underground'
  DO Item = 1, NumOfPipeHT

    CALL SetupOutputVariable('Pipe Fluid Heat Transfer Rate [W]',    &
                              PipeHTReport(Item)%FluidHeatLossRate,'Plant','Average', &
                              PipeHT(Item)%Name)
    CALL SetupOutputVariable('Pipe Fluid Heat Transfer Energy [J]', &
                              PipeHTReport(Item)%FluidHeatLossEnergy,'Plant','Sum',PipeHT(Item)%Name)

    IF(PipeHT(Item)%EnvironmentPtr .EQ. ZoneEnv)THEN
      CALL SetupOutputVariable('Pipe Ambient Heat Transfer Rate [W]',    &
                              PipeHTReport(Item)%EnvironmentHeatLossRate,'Plant','Average', &
                              PipeHT(Item)%Name)
      CALL SetupOutputVariable('Pipe Ambient Heat Transfer Energy [J]', &
                              PipeHTReport(Item)%EnvHeatLossEnergy,'Plant','Sum',PipeHT(Item)%Name)

      CALL SetupZoneInternalGain(PipeHT(Item)%EnvrZonePtr, &
                     'Pipe:Indoor',  &
                     PipeHT(Item)%Name, &
                     IntGainTypeOf_PipeIndoor,    &
                     ConvectionGainRate    = PipeHT(Item)%ZoneHeatGainRate)

    ENDIF

    CALL SetupOutputVariable('Pipe Mass Flow Rate [kg/s]',      &
                              PipeHTReport(Item)%MassFlowRate,'Plant','Average',   &
                              PipeHT(Item)%Name)
    CALL SetupOutputVariable('Pipe Volume Flow Rate [m3/s]',    &
                              PipeHTReport(Item)%VolumeFlowRate,'Plant','Average', &
                              PipeHT(Item)%Name)
    CALL SetupOutputVariable('Pipe Inlet Temperature [C]',      &
                              PipeHTReport(Item)%FluidInletTemp,'Plant','Average', &
                              PipeHT(Item)%Name)
    CALL SetupOutputVariable('Pipe Outlet Temperature [C]',     &
                              PipeHTReport(Item)%FluidOutletTemp,'Plant','Average',&
                              PipeHT(Item)%Name)
  END DO

  RETURN


END SUBROUTINE GetPipesHeatTransfer

SUBROUTINE ValidatePipeConstruction(PipeType,ConstructionName,FieldName,ConstructionNum,PipeNum,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine, called from GetInput, validates the pipe construction usage.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance,   ONLY : Construct, Material
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: PipeType   ! module object of pipe (error messages)
  CHARACTER(len=*), INTENT(IN) :: ConstructionName   ! construction name of pipe (error messages)
  CHARACTER(len=*), INTENT(IN) :: FieldName   ! fieldname of pipe (error messages)
  INTEGER, INTENT(IN)    :: ConstructionNum  ! pointer into construction data
  INTEGER, INTENT(IN)    :: PipeNum          ! pointer into pipe data
  LOGICAL, INTENT(INOUT) :: ErrorsFound   ! set to true if errors found here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: Resistance             ! overall thermal resistance [m^2.C/W]
  REAL(r64)           :: Density                ! average density [kg/m^3]
  REAL(r64)           :: TotThickness           ! total thickness of all layers
  REAL(r64)           :: SpHeat                 ! average specific heat [J/kg.K]
  INTEGER             :: LayerNum
  INTEGER             :: TotalLayers            ! total number of layers (pipe layer + insulation layers)

  Resistance=0.0d0
  TotThickness=0.0d0

  ! CTF stuff
  TotalLayers = Construct(ConstructionNum)%TotLayers
  ! get pipe properties
  IF(TotalLayers == 1) THEN  ! no insulation layer

    PipeHT(PipeNum)%PipeConductivity  = Material(Construct(ConstructionNum)%LayerPoint(1))%Conductivity
    PipeHT(PipeNum)%PipeDensity  = Material(Construct(ConstructionNum)%LayerPoint(1))%Density
    PipeHT(PipeNum)%PipeCp  = Material(Construct(ConstructionNum)%LayerPoint(1))%SpecHeat
    PipeHT(PipeNum)%PipeOD  = PipeHT(PipeNum)%PipeID + &
                                       2.0*Material(Construct(ConstructionNum)%LayerPoint(1))%Thickness
    PipeHT(PipeNum)%InsulationOD = PipeHT(PipeNum)%PipeOD
    PipeHT(PipeNum)%SumTK = Material(Construct(ConstructionNum)%LayerPoint(1))%Thickness /  &
                            Material(Construct(ConstructionNum)%LayerPoint(1))%Conductivity

  ELSEIF(TotalLayers >= 2)THEN ! first layers are insulation, last layer is pipe

    DO LayerNum = 1, TotalLayers - 1
      Resistance = Resistance + Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Thickness &
                               /Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Conductivity
      Density    = Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Density &
                   * Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Thickness
      TotThickness = TotThickness + Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Thickness
      SpHeat    = Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%SpecHeat &
                                    * Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Thickness
      PipeHT(PipeNum)%InsulationThickness = Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Thickness
      PipeHT(PipeNum)%SumTK = PipeHT(PipeNum)%SumTK + &
                              Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Thickness / &
                              Material(Construct(ConstructionNum)%LayerPoint(LayerNum))%Conductivity
    ENDDO

    PipeHT(PipeNum)%InsulationResistance    = Resistance
    PipeHT(PipeNum)%InsulationConductivity  = TotThickness/Resistance
    PipeHT(PipeNum)%InsulationDensity       = Density/TotThickness
    PipeHT(PipeNum)%InsulationCp            = SpHeat/TotThickness
    PipeHT(PipeNum)%InsulationThickness     = TotThickness


    PipeHT(PipeNum)%PipeConductivity  = Material(Construct(ConstructionNum)%LayerPoint(TotalLayers))%Conductivity
    PipeHT(PipeNum)%PipeDensity  = Material(Construct(ConstructionNum)%LayerPoint(TotalLayers))%Density
    PipeHT(PipeNum)%PipeCp  = Material(Construct(ConstructionNum)%LayerPoint(TotalLayers))%SpecHeat

    PipeHT(PipeNum)%PipeOD  = PipeHT(PipeNum)%PipeID + &
                                       2.0*Material(Construct(ConstructionNum)%LayerPoint(TotalLayers))%Thickness
    PipeHT(PipeNum)%InsulationOD = PipeHT(PipeNum)%PipeOD + 2.0*PipeHT(PipeNum)%InsulationThickness

  ELSE
    CALL ShowSevereError(PipeType//': invalid '//FieldName//'="'//ConstructionName//   &
       '", too many layers=['//trim(TrimSigDigits(TotalLayers))//'], only 1 or 2 allowed.')
    ErrorsFound=.true.
  END IF

  RETURN

END SUBROUTINE ValidatePipeConstruction

!==============================================================================

SUBROUTINE InitPipesHeatTransfer(PipeType,PipeHTNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       L. Gu, 6/19/08, pipe wall heat capacity has metal layer only
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine Resets the elements of the data structure as necessary
          ! at the first step, and start of each call to simulated

          ! METHODOLOGY EMPLOYED:
          ! Check flags and update data structure

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataGlobals,       ONLY : BeginSimFlag, BeginEnvrnFlag, PI, DayOfSim, HourOfDay,TimeStep, &
                                TimeStepZone, SecInHour, BeginTimeStepFlag
  USE DataHVACGlobals,   ONLY : SysTimeElapsed, TimeStepSys, ShortenTimeStepSys
  USE DataEnvironment,   ONLY : OutDryBulbTemp, GroundTemp, PubGroundTempSurface, PubGroundTempSurfFlag
  USE DataLoopNode,      ONLY : Node
  USE DataHeatBalance,   ONLY : TotConstructs, TotMaterials, Construct, Material
  USE DataHeatBalFanSys, ONLY : MAT !average (mean) zone air temperature [C]
  USE InputProcessor,    ONLY : SameString
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE FluidProperties,   ONLY : GetSpecificHeatGlycol,GetDensityGlycol
  USE DataPlant,         ONLY : PlantLoop, DemandSide, ScanPlantLoopsForObject

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: PipeType
  INTEGER, INTENT(IN) :: PipeHTNum       ! component number
  LOGICAL, INTENT(IN) :: FirstHVACIteration       ! component number

          ! SUBROUTINE PARAMETER DEFINITIONS:
 INTEGER, PARAMETER   :: NumPipeSections    = 20     ! Number of length nodes in Hanby model
 INTEGER, PARAMETER   :: NumberOfDepthNodes = 8      ! Number of nodes in the cartesian grid
 INTEGER, PARAMETER   :: MonthsInYear       = 12     ! Number of months in the year
 INTEGER, PARAMETER   :: AvgDaysInMonth     = 30     ! Average days in a month
 INTEGER, PARAMETER   :: DemandLoopSide     = 1      ! Demand Loop side indicator
 REAL(r64), PARAMETER :: LargeNumber        = 9999.9d0 ! Large number (compared to temperature values)
 REAL(r64), PARAMETER :: SecondsInHour      = 3600.0d0 ! Number of seconds in hour
 REAL(r64), PARAMETER :: HoursInDay         = 24.0d0   ! Number of hours in day

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  LOGICAL,SAVE        :: OneTimeInit = .TRUE.   ! one time flag
  REAL(r64)           :: FirstTemperatures      ! initial temperature of every node in pipe (set to inlet temp) [C]
  INTEGER             :: PipeNum                ! number of pipes
  INTEGER             :: MonthIndex
  INTEGER             :: TimeIndex
  INTEGER             :: LengthIndex
  INTEGER             :: DepthIndex
  INTEGER             :: WidthIndex
  REAL(r64)           :: CurrentDepth
  REAL(r64)           :: CurTemp
  REAL(r64)           :: CurSimDay
  INTEGER             :: PlantLoopCtr
  INTEGER             :: LoopSideCtr
  INTEGER             :: BranchCtr
  INTEGER             :: CompCtr
  LOGICAL             :: PushArrays
  LOGICAL             :: errFlag

  ! Assign variable
  CurSimDay      = REAL(DayOfSim,r64)

  ! some useful module variables
  InletNodeNum  = PipeHT(PipeHTNum)%InletNodeNum
  OutletNodeNum = PipeHT(PipeHTNum)%OutletNodeNum
  MassFlowRate  = Node(InletNodeNum)%MassFlowRate
  InletTemp = Node(InletNodeNum)%Temp

  ! get some data only once
  IF(OneTimeInit)THEN

    errFlag=.false.
    Do PipeNum =1,NumOfPipeHT

      CALL ScanPlantLoopsForObject(PipeHT(PipeNum)%Name, &
                                 PipeHT(PipeNum)%TypeOf, &
                                 PipeHT(PipeNum)%LoopNum, &
                                 PipeHT(PipeNum)%LoopSideNum, &
                                 PipeHT(PipeNum)%BranchNum, &
                                 PipeHT(PipeNum)%CompNum,  &
                                 errFlag=errFlag)

    ! 2010-03-15 ESL:
    ! The following code was in place because during the first implementation stage, bizarre MaxIters were found when
    !  heat transfer pipes were placed on demand sides
    ! Since then, a large number of plant and component upgrades were performed.  As such, heat transfer pipes were
    !  re-tested on several locations of the demand side
    ! No problems were encountered placing the pipes on the demand side.  This restriction is removed unless there are any
    !  problems encountered.  If problems are encountered, it is expected that this restriction will still be avoided, and
    !  the proper fix implemented to allow the pipes to be placed on the demand side
    !
    !  IF (PipeHT(PipeNum)%LoopSideNum == DemandSide) THEN
    !    CALL ShowSevereError('InitPipesHeatTransfer: Heat Transfer Pipe='//TRIM(PipeHT(PipeNum)%Name)//&
    !                      ' was encountered on the demand side of loop: '//TRIM(PlantLoop(PipeHT(PipeNum)%LoopNum)%Name)//'.')
    !    CALL ShowContinueError('Due to simulation restrictions, heat transfer pipes are only allowed on supply sides.')
    !    CALL ShowFatalError('Preceding errors cause termination')
    !  END IF

      IF (errFlag) CYCLE

      !If there are any underground buried pipes, we must bring in data
      IF(PipeHT(PipeNum)%EnvironmentPtr .EQ. GroundEnv)THEN

        !If ground temp data was not brought in manually in GETINPUT,
        ! then we must get it from the surface ground temperatures
        IF (PipeHT(PipeNum)%AvgAnnualManualInput .EQ. 0)THEN

          IF (.NOT. PubGroundTempSurfFlag) THEN
            CALL ShowFatalError('No Site:GroundTemperature:Shallow object found.  This is required for a Pipe:Underground object.')
          END IF

          !Calculate Average Ground Temperature for all 12 months of the year:
          PipeHT(PipeNum)%AvgGroundTemp = 0.0d0
          Do MonthIndex = 1, MonthsInYear
            PipeHT(PipeNum)%AvgGroundTemp = PipeHT(PipeNum)%AvgGroundTemp + PubGroundTempSurface(MonthIndex)
          END Do
          PipeHT(PipeNum)%AvgGroundTemp = PipeHT(PipeNum)%AvgGroundTemp / MonthsInYear

          !Calculate Average Amplitude from Average:
          PipeHT(PipeNum)%AvgGndTempAmp = 0.0d0
          Do MonthIndex = 1, MonthsInYear
            PipeHT(PipeNum)%AvgGndTempAmp = PipeHT(PipeNum)%AvgGndTempAmp + &
                                               ABS(PubGroundTempSurface(MonthIndex) - PipeHT(PipeNum)%AvgGroundTemp)
          END Do
          PipeHT(PipeNum)%AvgGndTempAmp = PipeHT(PipeNum)%AvgGndTempAmp / MonthsInYear

          !Also need to get the month of minimum surface temperature to set phase shift for Kusuda and Achenbach:
          PipeHT(PipeNum)%MonthOfMinSurfTemp = 0
          PipeHT(PipeNum)%MinSurfTemp = LargeNumber !Set high so that the first months temp will be lower and actually get updated
          Do MonthIndex = 1, MonthsInYear
            If (PubGroundTempSurface(MonthIndex) <= PipeHT(PipeNum)%MinSurfTemp) THEN
              PipeHT(PipeNum)%MonthOfMinSurfTemp = MonthIndex
              PipeHT(PipeNum)%MinSurfTemp = PubGroundTempSurface(MonthIndex)
            END If
          END Do
          PipeHT(PipeNum)%PhaseShiftDays = PipeHT(PipeNum)%MonthOfMinSurfTemp * AvgDaysInMonth
        ENDIF !End manual ground data input structure

      ENDIF

    END DO
    IF (errFlag) THEN
      CALL ShowFatalError('InitPipesHeatTransfer: Program terminated due to previous condition(s).')
    ENDIF
    ! unset one-time flag
    OneTimeInit = .FALSE.

  END IF

  ! initialize temperatures by inlet node temp
  IF((BeginSimFlag.AND. PipeHT(PipeHTNum)%BeginSimInit) .OR. (BeginEnvrnFlag .AND. PipeHT(PipeHTNum)%BeginSimEnvrn)) THEN

    ! For underground pipes, we need to re-init the cartesian array each environment
    Do PipeNum =1,NumOfPipeHT
      IF(PipeHT(PipeNum)%EnvironmentPtr.EQ.GroundEnv)THEN
        Do TimeIndex = PreviousTimeIndex, TentativeTimeIndex
          !Loop through all length, depth, and width of pipe to init soil temperature
          Do LengthIndex = 1, PipeHT(PipeNum)%NumSections
            Do DepthIndex = 1, PipeHT(PipeNum)%NumDepthNodes
              Do WidthIndex = 1, PipeHT(PipeNum)%PipeNodeWidth
                CurrentDepth = (DepthIndex - 1) * PipeHT(PipeNum)%dSregular
                PipeHT(PipeNum)%T(TimeIndex, LengthIndex, DepthIndex, WidthIndex) = TBND(CurrentDepth, CurSimDay, PipeNum)
              EndDo
            EndDo
          EndDo
        EndDo
      ENDIF
    ENDDO

    ! We also need to re-init the Hanby arrays for all pipes, including buried
    FirstTemperatures = 21.0d0 !Node(InletNodeNum)%Temp
    PipeHT(PipeHTNum)%TentativeFluidTemp = FirstTemperatures
    PipeHT(PipeHTNum)%FluidTemp          = FirstTemperatures
    PipeHT(PipeHTNum)%PreviousFluidTemp  = FirstTemperatures
    PipeHT(PipeHTNum)%TentativePipeTemp  = FirstTemperatures
    PipeHT(PipeHTNum)%PipeTemp           = FirstTemperatures
    PipeHT(PipeHTNum)%PreviousPipeTemp   = FirstTemperatures
    PipeHT(PipeHTNum)%PreviousSimTime    = 0.0d0
    DeltaTime                            = 0.0d0
    OutletTemp                           = 0.0d0
    EnvironmentTemp                      = 0.0d0
    EnvHeatLossRate                      = 0.0d0
    FluidHeatLossRate                    = 0.0d0

    PipeHT(PipeHTNum)%BeginSimInit = .FALSE.
    PipeHT(PipeHTNum)%BeginSimEnvrn = .FALSE.

  END IF

  IF (.NOT. BeginSimFlag) PipeHT(PipeHTNum)%BeginSimInit = .TRUE.
  IF (.NOT. BeginEnvrnFlag) PipeHT(PipeHTNum)%BeginSimEnvrn = .TRUE.

  ! time step in seconds
  DeltaTime = TimeStepSys*SecInHour
  NumInnerTimeSteps = INT(DeltaTime / InnerDeltaTime)


  ! previous temps are updated if necessary at start of timestep rather than end
  IF( (FirstHVACIteration .and. PipeHT(PipeHTNum)%FirstHVACupdateFlag) .or. &
      (BeginEnvrnFlag     .and. PipeHT(PipeHTNum)%BeginEnvrnupdateFlag)  )THEN

    !We need to update boundary conditions here, as well as updating the arrays
    IF(PipeHT(PipeHTNum)%EnvironmentPtr.EQ.GroundEnv)THEN

      ! And then update Ground Boundary Conditions
      Do TimeIndex = 1, TentativeTimeIndex
        Do LengthIndex = 1, PipeHT(PipeHTNum)%NumSections
          Do DepthIndex = 1, PipeHT(PipeHTNum)%NumDepthNodes
            !Farfield boundary
            CurrentDepth = (DepthIndex - 1) * PipeHT(PipeHTNum)%dSregular
            CurTemp = TBND(CurrentDepth, CurSimDay, PipeHTNum)
            PipeHT(PipeHTNum)%T(TimeIndex, LengthIndex, DepthIndex, 1) = CurTemp
          EndDo
          Do WidthIndex = 1, PipeHT(PipeHTNum)%PipeNodeWidth
            !Bottom side of boundary
            CurrentDepth = PipeHT(PipeHTNum)%DomainDepth
            CurTemp = TBND(CurrentDepth, CurSimDay, PipeHTNum)
            PipeHT(PipeHTNum)%T(TimeIndex, LengthIndex, PipeHT(PipeHTNum)%NumDepthNodes, WidthIndex) = CurTemp
          EndDo
        EndDo
      EndDo
    EndIf

  ! should next choose environment temperature according to coupled with air or ground
   SELECT CASE (PipeHT(PipeHTNum)%EnvironmentPtr)
      CASE(GroundEnv)
         !EnvironmentTemp = GroundTemp
      CASE(OutsideAirEnv)
         EnvironmentTemp = OutDryBulbTemp
      CASE(ZoneEnv)
         EnvironmentTemp = MAT(PipeHT(PipeHTNum)%EnvrZonePtr)
      CASE (ScheduleEnv)
         EnvironmentTemp = GetCurrentScheduleValue(PipeHT(PipeHTNum)%EnvrSchedPtr)
      CASE(None) !default to outside temp
         EnvironmentTemp = OutDryBulbTemp
   END SELECT

    PipeHT(PipeHTNum)%BeginEnvrnupdateFlag = .false.
    PipeHT(PipeHTNum)%FirstHVACupdateFlag = .false.

  END IF

  IF(.NOT. BeginEnvrnFlag) PipeHT(PipeHTNum)%BeginEnvrnupdateFlag = .true.
  IF(.NOT. FirstHVACIteration) PipeHT(PipeHTNum)%FirstHVACupdateFlag = .true.

  !Calculate the current sim time for this pipe (not necessarily structure variable, but it is ok for consistency)
  PipeHT(PipeHTNum)%CurrentSimTime = (dayofSim-1)*24 + hourofday-1 + (timestep-1)*timestepZone + SysTimeElapsed
  IF (ABS(PipeHT(PipeHTNum)%CurrentSimTime - PipeHT(PipeHTNum)%PreviousSimTime) > 1.0d-6) THEN
    PushArrays = .TRUE.
    PipeHT(PipeHTNum)%PreviousSimTime = PipeHT(PipeHTNum)%CurrentSimTime
  ELSE
    PushArrays = .FALSE.  !Time hasn't passed, don't accept the tentative values yet!
  END IF

  IF (PushArrays) THEN

     !If sim time has changed all values from previous runs should have been acceptable.
     ! Thus we will now shift the arrays from 2>1 and 3>2 so we can then begin
     ! to update 2 and 3 again.
     IF (PipeHT(PipeHTNum)%EnvironmentPtr .EQ. GroundEnv) THEN
       Do LengthIndex = 2, PipeHT(PipeHTNum)%NumSections
        Do DepthIndex = 1,  PipeHT(PipeHTNum)%NumDepthNodes
          Do WidthIndex = 2, PipeHT(PipeHTNum)%PipeNodeWidth
           !This will essentially 'accept' the tentative values that were calculated last iteration
           ! as the new officially 'current' values
           PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
              PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex)
          EndDo
        EndDo
      EndDo
    ENDIF

    !Then update the Hanby near pipe model temperatures
    PipeHT(PipeHTNum)%FluidTemp         = PipeHT(PipeHTNum)%TentativeFluidTemp
    PipeHT(PipeHTNum)%PipeTemp          = PipeHT(PipeHTNum)%TentativePipeTemp

  ELSE !  IF(.NOT. FirstHVACIteration)THEN

     !If we don't have FirstHVAC, the last iteration values were not accepted, and we should
     ! not step through time.  Thus we will revert our T(3,:,:,:) array back to T(2,:,:,:) to
     ! start over with the same values as last time.
     Do LengthIndex = 2, PipeHT(PipeHTNum)%NumSections
      Do DepthIndex = 1,  PipeHT(PipeHTNum)%NumDepthNodes
        Do WidthIndex = 2, PipeHT(PipeHTNum)%PipeNodeWidth
         !This will essentially erase the past iterations and revert back to the correct values
         PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
            PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex)
        EndDo
      EndDo
    EndDo

    !Similarly for Hanby model arrays
    PipeHT(PipeHTNum)%TentativeFluidTemp = PipeHT(PipeHTNum)%FluidTemp
    PipeHT(PipeHTNum)%TentativePipeTemp = PipeHT(PipeHTNum)%PipeTemp

  ENDIF

  !This still catches even in winter design day
  !Even though the loop eventually has no flow rate, it appears it initializes to a value, then converges to OFF
  !Thus, this is called at the beginning of every time step once.

  PipeHT(PipeHTNum)%FluidSpecHeat = GetSpecificHeatGlycol(PlantLoop(PipeHT(PipeHTNum)%LoopNum)%FluidName,  &
                                                          InletTemp,                      &
                                                          PlantLoop(PipeHT(PipeHTNum)%LoopNum)%FluidIndex, &
                                                          'InitPipesHeatTransfer')
  PipeHT(PipeHTNum)%FluidDensity = GetDensityGlycol(PlantLoop(PipeHT(PipeHTNum)%LoopNum)%FluidName,  &
                                                    InletTemp, &
                                                    PlantLoop(PipeHT(PipeHTNum)%LoopNum)%FluidIndex,&
                                                    'InitPipesHeatTransfer')


  ! At this point, for all Pipe:Interior objects we should zero out the energy and rate arrays
  PipeHTReport(PipeHTNum)%FluidHeatLossRate       = 0.0d0
  PipeHTReport(PipeHTNum)%FluidHeatLossEnergy     = 0.0d0
  PipeHTReport(PipeHTNum)%EnvironmentHeatLossRate = 0.0d0
  PipeHTReport(PipeHTNum)%EnvHeatLossEnergy       = 0.0d0
  PipeHT(PipeHTNum)%ZoneHeatGainRate              = 0.d0
  FluidHeatLossRate                               = 0.0d0
  EnvHeatLossRate                                 = 0.0d0
  OutletTemp                                      = 0.0d0

  IF(PipeHT(PipeHTNum)%FluidDensity .GT. 0.0d0) THEN
    !The density will only be zero the first time through, which will be a warmup day, and not reported
    VolumeFlowRate= MassFlowRate / PipeHT(PipeHTNum)%FluidDensity
  ENDIF

 RETURN

END SUBROUTINE InitPipesHeatTransfer

!==============================================================================

SUBROUTINE InitializeHeatTransferPipes(PipeType,PipeName,PipeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provide an external call to initialize Pipes/index numbers.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeType  ! Type of Pipe
  CHARACTER(len=*), INTENT(IN) :: PipeName  ! Name of Pipe
  INTEGER, INTENT(INOUT)       :: PipeNum   ! Index into pipe structure for name

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetPipeInputFlag) THEN
    CALL GetPipesHeatTransfer
    GetPipeInputFlag=.FALSE.
  ENDIF

      IF (PipeNum == 0) THEN
        PipeNum=FindItemInList(PipeName,PipeHT%Name,NumOfPipeHT)
        IF (PipeNum == 0) THEN
          CALL ShowFatalError('SimPipes: Pipe requested not found ='//TRIM(PipeName)) ! Catch any bad names before crashing
        ENDIF
      ELSE
        IF (PipeNum > NumOfPipeHT .or. PipeNum < 1) THEN
          CALL ShowFatalError('InitializePipe: Invalid PipeNum passed='//TRIM(TrimSigDigits(PipeNum))// &
                              ', Number of Pipes='//TRIM(TrimSigDigits(NumOfPipeHT))//', Pipe name='//TRIM(PipeName))
        ENDIF
        IF (PipeName /= PipeHT(PipeNum)%Name) THEN
          CALL ShowFatalError('InitializePipe: Invalid PipeNum passed='//TRIM(TrimSigDigits(PipeNum))// &
                              ', Pipe name='//TRIM(PipeName)//', stored Pipe Name for that index='//TRIM(PipeHT(PipeNum)%Name))
        ENDIF
      ENDIF


  RETURN

END SUBROUTINE InitializeHeatTransferPipes

!==============================================================================

SUBROUTINE CalcPipesHeatTransfer(PipeHTNum, LengthIndex)


          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a Pipe Heat Transfer.  Calls are made to appropriate routines
          ! for heat transfer coefficients


          ! METHODOLOGY EMPLOYED:
          ! Differential equations for pipe and fluid nodes along the pipe are solved
          ! taking backward differences in time.
          ! The heat loss/gain calculations are run continuously, even when the loop is off.
          ! Fluid temps will drift according to environmental conditions when there is zero flow.


          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataEnvironment

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeHTNum        ! component number
  INTEGER, OPTIONAL, INTENT(IN) :: LengthIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

            ! fluid node heat balance (see engineering doc).
  REAL(r64)        :: A1 = 0.0d0   !sum of the heat balance terms
  REAL(r64)        :: A2 = 0.0d0   !mass flow term
  REAL(r64)        :: A3 = 0.0d0   !inside pipe wall convection term
  REAL(r64)        :: A4 = 0.0d0   !fluid node heat capacity term
            ! pipe wall node heat balance (see engineering doc).
  REAL(r64)        :: B1 = 0.0d0   !sum of the heat balance terms
  REAL(r64)        :: B2 = 0.0d0   !inside pipe wall convection term
  REAL(r64)        :: B3 = 0.0d0   !outside pipe wall convection term
  REAL(r64)        :: B4 = 0.0d0   !fluid node heat capacity term

  REAL(r64)        :: AirConvCoef           = 0.0d0    ! air-pipe convection coefficient
  REAL(r64)        :: FluidConvCoef         = 0.0d0    ! fluid-pipe convection coefficient
  REAL(r64)        :: EnvHeatTransCoef      = 0.0d0    ! external convection coefficient (outside pipe)
  REAL(r64)        :: FluidNodeHeatCapacity = 0.0d0    ! local var for MCp for single node of pipe

  INTEGER          :: PipeDepth = 0
  INTEGER          :: PipeWidth = 0
  INTEGER          :: curnode
  REAL(r64)        :: TempBelow
  REAL(r64)        :: TempBeside
  REAL(r64)        :: TempAbove
  REAL(r64)        :: Numerator
  REAL(r64)        :: Denominator
  REAL(r64)        :: SurfaceTemp

  ! traps fluid properties problems such as freezing conditions
  IF(PipeHT(PipeHTNum)%FluidSpecHeat <= 0.0d0 .OR. PipeHT(PipeHTNum)%FluidDensity <= 0.0d0)THEN
    ! leave the state of the pipe as it was
    OutletTemp = PipeHT(PipeHTNum)%TentativeFluidTemp(PipeHT(PipeHTNum)%NumSections)
    ! set heat transfer rates to zero for consistency
    EnvHeatLossRate   = 0.0d0
    FluidHeatLossRate = 0.0d0
    RETURN
  END IF

!  AirConvCoef =  OutsidePipeHeatTransCoef(PipeHTNum)
! Revised by L. Gu by including insulation conductance 6/19/08

IF (PipeHT(PipeHTNum)%EnvironmentPtr.NE.GroundEnv) THEN
  AirConvCoef = 1.0/(1.0/OutsidePipeHeatTransCoef(PipeHTNum)+PipeHT(PipeHTNum)%InsulationResistance)
ENDIF

  FluidConvCoef =  CalcPipeHeatTransCoef(PipeHTNum, InletTemp,MassFlowRate, PipeHT(PipeHTNum)%PipeID)

  ! heat transfer to air or ground
   SELECT CASE (PipeHT(PipeHTNum)%EnvironmentPtr)
      CASE(GroundEnv)
         !Approximate conductance using ground conductivity, (h=k/L), where L is grid spacing
         ! between pipe wall and next closest node.
         EnvHeatTransCoef = PipeHT(PipeHTNum)%SoilConductivity/(PipeHT(PipeHTNum)%dSregular-(PipeHT(PipeHTNum)%PipeID/2.0d0))
      CASE(OutsideAirEnv)
         EnvHeatTransCoef = AirConvCoef
      CASE(ZoneEnv)
         EnvHeatTransCoef = AirConvCoef
      CASE(ScheduleEnv)
         EnvHeatTransCoef = AirConvCoef
      CASE(None)
         EnvHeatTransCoef = 0.0d0
      CASE DEFAULT
         EnvHeatTransCoef = 0.0d0
   END SELECT

  ! work out the coefficients
  FluidNodeHeatCapacity = PipeHT(PipeHTNum)%SectionArea * PipeHT(PipeHTNum)%Length / PipeHT(PipeHTNum)%NumSections  * &
                          PipeHT(PipeHTNum)%FluidSpecHeat * PipeHT(PipeHTNum)%FluidDensity ! Mass of Node x Specific heat

  ! coef of fluid heat balance
  A1 = FluidNodeHeatCapacity + MassFlowRate * PipeHT(PipeHTNum)%FluidSpecHeat * DeltaTime + &
       FluidConvCoef * PipeHT(PipeHTNum)%InsideArea * DeltaTime

  A2 = MassFlowRate * PipeHT(PipeHTNum)%FluidSpecHeat * DeltaTime

  A3 = FluidConvCoef * PipeHT(PipeHTNum)%InsideArea * DeltaTime

  A4 = FluidNodeHeatCapacity

  ! coef of pipe heat balance
  B1 = PipeHT(PipeHTNum)%PipeHeatCapacity + FluidConvCoef * PipeHT(PipeHTNum)%InsideArea * DeltaTime + &
       EnvHeatTransCoef * PipeHT(PipeHTNum)%OutsideArea * DeltaTime

  B2 = A3

  B3 = EnvHeatTransCoef * PipeHT(PipeHTNum)%OutsideArea * DeltaTime

  B4 = PipeHT(PipeHTNum)%PipeHeatCapacity

  PipeHT(PipeHTNum)%TentativeFluidTemp(0) = InletTemp

  PipeHT(PipeHTNum)%TentativePipeTemp(0)  = PipeHT(PipeHTNum)%PipeTemp(1) ! for convenience

  IF(PRESENT(LengthIndex))THEN  !Just simulate the single section if being called from Pipe:Underground

    PipeDepth=PipeHT(PipeHTNum)%PipeNodeDepth
    PipeWidth=PipeHT(PipeHTNum)%PipeNodeWidth
    TempBelow=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, PipeDepth+1, PipeWidth)
    TempBeside=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, PipeDepth, PipeWidth-1)
    TempAbove=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, PipeDepth-1, PipeWidth)
    EnvironmentTemp=(TempBelow+TempBeside+TempAbove)/3.0d0

    PipeHT(PipeHTNum)%TentativeFluidTemp(LengthIndex) = (A2 * PipeHT(PipeHTNum)%TentativeFluidTemp(LengthIndex-1) + &
                                     A3/B1 * (B3* EnvironmentTemp + B4 * PipeHT(PipeHTNum)%PreviousPipeTemp(LengthIndex)) + &
                                     A4 * PipeHT(PipeHTNum)%PreviousFluidTemp(LengthIndex)) /(A1 - A3*B2/B1)

    PipeHT(PipeHTNum)%TentativePipeTemp(LengthIndex) = (B2 * PipeHT(PipeHTNum)%TentativeFluidTemp(LengthIndex) + B3 * &
                                     EnvironmentTemp + B4 * PipeHT(PipeHTNum)%PreviousPipeTemp(LengthIndex)) / B1

    ! Get exterior surface temperature from energy balance at the surface
    Numerator = EnvironmentTemp - PipeHT(PipeHTNum)%TentativeFluidTemp(LengthIndex)
    Denominator = EnvHeatTransCoef * ( (1/EnvHeatTransCoef) + PipeHT(PipeHTNum)%SumTK )
    SurfaceTemp = EnvironmentTemp - Numerator/Denominator

    ! keep track of environmental heat loss rate - not same as fluid loss at same time
    EnvHeatLossRate = EnvHeatLossRate + &
                        EnvHeatTransCoef * PipeHT(PipeHTNum)%OutsideArea * (SurfaceTemp - EnvironmentTemp)

  ELSE  !Simulate all sections at once if not pipe:underground

    ! start loop along pipe
    ! b1 must not be zero but this should have been checked on input
    DO curnode=1, PipeHT(PipeHTNum)%NumSections
      PipeHT(PipeHTNum)%TentativeFluidTemp(curnode) = (A2 * PipeHT(PipeHTNum)%TentativeFluidTemp(curnode-1) + &
                                     A3/B1 * (B3* EnvironmentTemp + B4 * PipeHT(PipeHTNum)%PreviousPipeTemp(curnode)) + &
                                     A4 * PipeHT(PipeHTNum)%PreviousFluidTemp(curnode)) /(A1 - A3*B2/B1)

      PipeHT(PipeHTNum)%TentativePipeTemp(curnode) = (B2 * PipeHT(PipeHTNum)%TentativeFluidTemp(curnode) + B3 * EnvironmentTemp + &
                                     B4 * PipeHT(PipeHTNum)%PreviousPipeTemp(curnode)) / B1

      ! Get exterior surface temperature from energy balance at the surface
      Numerator = EnvironmentTemp - PipeHT(PipeHTNum)%TentativeFluidTemp(curnode)
      Denominator = EnvHeatTransCoef * ( (1/EnvHeatTransCoef) + PipeHT(PipeHTNum)%SumTK )
      SurfaceTemp = EnvironmentTemp - Numerator/Denominator

      ! Keep track of environmental heat loss
      EnvHeatLossRate = EnvHeatLossRate + &
                        EnvHeatTransCoef * PipeHT(PipeHTNum)%OutsideArea * (SurfaceTemp - EnvironmentTemp)

    END DO

  END IF

  FluidHeatLossRate = MassFlowRate * PipeHT(PipeHTNum)%FluidSpecHeat * (PipeHT(PipeHTNum)%TentativeFluidTemp(0) - &
                      PipeHT(PipeHTNum)%TentativeFluidTemp(PipeHT(PipeHTNum)%NumSections))

  OutletTemp = PipeHT(PipeHTNum)%TentativeFluidTemp(PipeHT(PipeHTNum)%NumSections)

  RETURN

END SUBROUTINE CalcPipesHeatTransfer

!==============================================================================

Subroutine CalcBuriedPipeSoil(PipeHTNum)

          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   May 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! soil heat transfer with a Buried Pipe.

          ! METHODOLOGY EMPLOYED:
          ! An implicit pseudo 3D finite difference grid
          ! is set up, which simulates transient behavior in the soil.
          ! This then interfaces with the Hanby model for near-pipe region

          ! REFERENCES: See Module Level Description

          ! USE STATEMENTS:
  USE DataLoopNode,           ONLY : Node
  USE DataEnvironment,        ONLY : OutDryBulbTemp, SkyTemp, WindSpeed, BeamSolarRad, &
                                     DifSolarRad, SOLCOS
  USE DataGlobals,            ONLY : PI, WarmupFlag, TimeStep, HourOfDay, KelvinConv, rTinyValue
  USE ConvectionCoefficients, ONLY : CalcASHRAESimpExtConvectCoeff

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  ::  PipeHTNum    ! Current Simulation Pipe Number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER ::  NumSections   = 20
  REAL(r64), PARAMETER ::  ConvCrit      = 0.05d0
  INTEGER,   PARAMETER ::  MaxIterations = 200
  REAL(r64), PARAMETER ::  StefBoltzmann = 5.6697d-08     ! Stefan-Boltzmann constant

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: IterationIndex = 0    ! Index when stepping through equations
  INTEGER          :: LengthIndex    = 0    ! Index for nodes along length of pipe
  INTEGER          :: DepthIndex     = 0    ! Index for nodes in the depth direction
  INTEGER          :: WidthIndex     = 0    ! Index for nodes in the width direction
  REAL(r64)        :: ConvCoef       = 0.0d0  ! Current convection coefficient = f(Wind Speed,Roughness)
  REAL(r64)        :: RadCoef        = 0.0d0  ! Current radiation coefficient
  REAL(r64)        :: QSolAbsorbed   = 0.0d0  ! Current total solar energy absorbed
  REAL(r64), DIMENSION(NumSections, PipeHT(PipeHTNum)%NumDepthNodes, PipeHT(PipeHTNum)%PipeNodeWidth) :: T_O

  !Local variable placeholders for code readability
  REAL(r64)        :: A1             = 0.0d0  ! Placeholder for CoefA1
  REAL(r64)        :: A2             = 0.0d0  ! Placeholder for CoefA2
  REAL(r64)        :: NodeBelow      = 0.0d0  ! Placeholder for Node temp below current node
  REAL(r64)        :: NodeAbove      = 0.0d0  ! Placeholder for Node temp above current node
  REAL(r64)        :: NodeRight      = 0.0d0  ! Placeholder for Node temp to the right of current node
  REAL(r64)        :: NodeLeft       = 0.0d0  ! Placeholder for Node temp to the left of current node
  REAL(r64)        :: NodePast       = 0.0d0  ! Placeholder for Node temp at current node but previous time step
  REAL(r64)        :: PastNodeTempAbs= 0.0d0  ! Placeholder for absolute temperature (K) version of NodePast
  REAL(r64)        :: Ttemp          = 0.0d0  ! Placeholder for a current temperature node in convergence check
  REAL(r64)        :: SkyTempAbs     = 0.0d0  ! Placeholder for current sky temperature in Kelvin
  INTEGER          :: TopRoughness   = 0    ! Placeholder for soil surface roughness
  REAL(r64)        :: TopThermAbs    = 0.0d0  ! Placeholder for soil thermal radiation absorptivity
  REAL(r64)        :: TopSolarAbs    = 0.0d0  ! Placeholder for soil solar radiation absorptivity
  REAL(r64)        :: kSoil          = 0.0d0  ! Placeholder for soil conductivity
  REAL(r64)        :: dS             = 0.0d0  ! Placeholder for soil grid spacing
  REAL(r64)        :: rho            = 0.0d0  ! Placeholder for soil density
  REAL(r64)        :: Cp             = 0.0d0  ! Placeholder for soil specific heat

  ! There are a number of coefficients which change through the simulation, and they are updated here
  PipeHT(PipeHTNum)%FourierDS = PipeHT(PipeHTNum)%SoilDiffusivity *     &
      DeltaTime / (PipeHT(PipeHTNum)%dSregular ** 2)                 !Eq. D4
  PipeHT(PipeHTNum)%CoefA1 = PipeHT(PipeHTNum)%FourierDS / (1 + 4 * PipeHT(PipeHTNum)%FourierDS)         !Eq. D2
  PipeHT(PipeHTNum)%CoefA2 = 1 / (1 + 4 * PipeHT(PipeHTNum)%FourierDS)                           !Eq. D3

  IterationLoop: DO IterationIndex=1,MaxIterations
    IF (IterationIndex.EQ.MaxIterations)THEN
      CALL ShowWarningError('BuriedPipeHeatTransfer: Large number of iterations detected in object: '//TRIM(PipeHT(PipeHTNum)%Name))
    ENDIF

    !Store computed values in T_O array
    DO LengthIndex = 2, PipeHT(PipeHTNum)%NumSections
      DO DepthIndex = 1, PipeHT(PipeHTNum)%NumDepthNodes - 1
        DO WidthIndex = 2, PipeHT(PipeHTNum)%PipeNodeWidth
          T_O(LengthIndex, DepthIndex, WidthIndex) = PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex)
        ENDDO
      ENDDO
    ENDDO

    !Loop along entire length of pipe, analyzing cross sects
    DO LengthIndex = 1, PipeHT(PipeHTNum)%NumSections
      DO DepthIndex = 1, PipeHT(PipeHTNum)%NumDepthNodes - 1
        DO WidthIndex = 2, PipeHT(PipeHTNum)%PipeNodeWidth

          IF (DepthIndex.EQ.1) THEN !Soil Surface Boundary

            !If on soil boundary, load up local variables and perform calculations
            NodePast = PipeHT(PipeHTNum)%T(PreviousTimeIndex, LengthIndex, DepthIndex, WidthIndex)
            PastNodeTempAbs = NodePast + KelvinConv
            SkyTempAbs = SkyTemp + KelvinConv
            TopRoughness = PipeHT(PipeHTNum)%SoilRoughness
            TopThermAbs = PipeHT(PipeHTNum)%SoilThermAbs
            TopSolarAbs = PipeHT(PipeHTNum)%SoilSolarAbs
            kSoil = PipeHT(PipeHTNum)%SoilConductivity
            dS = PipeHT(PipeHTNum)%dSRegular
            rho = PipeHT(PipeHTNum)%SoilDensity
            Cp = PipeHT(PipeHTNum)%SoilCp

            ! ASHRAE simple convection coefficient model for external surfaces.
            PipeHT(PipeHTNum)%OutdoorConvCoef = CalcASHRAESimpExtConvectCoeff(TopRoughness,WindSpeed)
            ConvCoef = PipeHT(PipeHTNum)%OutdoorConvCoef

            ! thermal radiation coefficient using surf temp from past time step
            IF (ABS(PastNodeTempAbs-SkyTempAbs) > rTinyValue) THEN
              RadCoef = StefBoltzmann*TopThermAbs*((PastNodeTempAbs**4)-(SkyTempAbs**4))/(PastNodeTempAbs-SkyTempAbs)
            ELSE
              RadCoef = 0.0d0
            ENDIF

            ! total absorbed solar - no ground solar
            QSolAbsorbed = TopSolarAbs*(MAX(SOLCOS(3),0.0d0)*BeamSolarRad + DifSolarRad)

            ! If sun is not exposed, then turn off both solar and thermal radiation
            IF (.NOT. PipeHT(PipeHTNum)%SolarExposed) THEN
              RadCoef = 0.0d0
              QSolAbsorbed = 0.0d0
            ENDIF

            IF (WidthIndex.EQ.PipeHT(PipeHTNum)%PipeNodeWidth) THEN !Symmetric centerline boundary

              !-Coefficients and Temperatures
              NodeBelow=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex + 1, WidthIndex)
              NodeLeft=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex - 1)

              !-Update Equation, basically a detailed energy balance at the surface
              PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
                      (QSolAbsorbed + RadCoef * SkyTemp + ConvCoef * OutDryBulbTemp + &
                      (kSoil/dS)*(NodeBelow+2*NodeLeft) + (rho*Cp/DeltaTime)*NodePast) / &
                      (RadCoef+ConvCoef+3*(kSoil/dS)+(rho*Cp/DeltaTime))

            ELSE !Soil surface, but not on centerline

              !-Coefficients and Temperatures
              NodeBelow=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex + 1, WidthIndex)
              NodeLeft=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex - 1)
              NodeRight=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex + 1)

              !-Update Equation
              PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
                      (QSolAbsorbed + RadCoef * SkyTemp + ConvCoef * OutDryBulbTemp + &
                      (kSoil/dS)*(NodeBelow+NodeLeft+NodeRight) + (rho*Cp/DeltaTime)*NodePast) / &
                      (RadCoef+ConvCoef+3*(kSoil/dS)+(rho*Cp/DeltaTime))

            ENDIF !Soil-to-air surface node structure

          ELSEIF (WidthIndex.EQ.PipeHT(PipeHTNum)%PipeNodeWidth) THEN !On Symmetric centerline boundary

            IF (DepthIndex.EQ.PipeHT(PipeHTNum)%PipeNodeDepth) THEN !On the node containing the pipe

              !-Call to simulate a single pipe segment (by passing OPTIONAL LengthIndex argument)
              CALL CalcPipesHeatTransfer(PipeHTNum, LengthIndex)

              !-Update node for cartesian system
              PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex) = PipeHT(PipeHTNum)%PipeTemp(LengthIndex)

            ELSEIF (DepthIndex.NE.1) THEN !Not surface node

              !-Coefficients and Temperatures
              NodeLeft=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex - 1)
              NodeAbove=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex - 1, WidthIndex)
              NodeBelow=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex + 1, WidthIndex)
              NodePast=PipeHT(PipeHTNum)%T(CurrentTimeIndex - 1, LengthIndex, DepthIndex, WidthIndex)
              A1=PipeHT(PipeHTNum)%CoefA1
              A2=PipeHT(PipeHTNum)%CoefA2

              !-Update Equation
              PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
                   A1 * (NodeBelow + NodeAbove + 2 * NodeLeft) + A2 * NodePast

            ENDIF !Symmetric centerline node structure

          ELSE   !All Normal Interior Nodes

            !-Coefficients and Temperatures
            A1=PipeHT(PipeHTNum)%CoefA1
            A2=PipeHT(PipeHTNum)%CoefA2
            NodeBelow=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex + 1, WidthIndex)
            NodeAbove=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex - 1, WidthIndex)
            NodeRight=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex + 1)
            NodeLeft=PipeHT(PipeHTNum)%T(CurrentTimeIndex, LengthIndex, DepthIndex, WidthIndex - 1)
            NodePast=PipeHT(PipeHTNum)%T(CurrentTimeIndex - 1, LengthIndex, DepthIndex, WidthIndex)

            !-Update Equation
            PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex) = &
                 A1 * (NodeBelow + NodeAbove + NodeRight + NodeLeft) + A2 * NodePast !Eq. D1

          ENDIF
        ENDDO
      ENDDO
    ENDDO

    !Check for convergence
    DO LengthIndex = 2, PipeHT(PipeHTNum)%NumSections
      DO DepthIndex = 1, PipeHT(PipeHTNum)%NumDepthNodes - 1
        DO WidthIndex = 2, PipeHT(PipeHTNum)%PipeNodeWidth
          Ttemp=PipeHT(PipeHTNum)%T(TentativeTimeIndex, LengthIndex, DepthIndex, WidthIndex)
          IF(ABS(T_O(LengthIndex, DepthIndex, WidthIndex) - Ttemp) .GT. ConvCrit) CYCLE IterationLoop
        ENDDO
      ENDDO
    ENDDO

    !If we didn't cycle back, then the system is converged
    !PipeHTReport(PipeHTNum)%PipeUGIters=IterationIndex
    EXIT IterationLoop

  ENDDO IterationLoop

  CLOSE(112)
  RETURN

END SUBROUTINE

!==============================================================================

SUBROUTINE UpdatePipesHeatTransfer


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does any updating that needs to be done for
          ! Pipe Heat Transfers. This routine must also set the outlet water conditions.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE DataLoopNode,    ONLY : Node
  USE DataPlant, ONLY : PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  !INTEGER, INTENT(IN) :: PipeHTNum       ! Index for the surface

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:




  ! only outlet node temp should need updating
  Node(OutletNodeNum)%Temp                 = OutletTemp

  ! pass everything else through
  Node(OutletNodeNum)%TempMin              = Node(InletNodeNum)%TempMin
  Node(OutletNodeNum)%TempMax              = Node(InletNodeNum)%TempMax
  Node(OutletNodeNum)%MassFlowRate         = Node(InletNodeNum)%MassFlowRate
  Node(OutletNodeNum)%MassFlowRateMin      = Node(InletNodeNum)%MassFlowRateMin
  Node(OutletNodeNum)%MassFlowRateMax      = Node(InletNodeNum)%MassFlowRateMax
  Node(OutletNodeNum)%MassFlowRateMinAvail = Node(InletNodeNum)%MassFlowRateMinAvail
  Node(OutletNodeNum)%MassFlowRateMaxAvail = Node(InletNodeNum)%MassFlowRateMaxAvail
  Node(OutletNodeNum)%Quality              = Node(InletNodeNum)%Quality
  !Only pass pressure if we aren't doing a pressure simulation
  IF (PlantLoop(PipeHT(PipeHTNum)%LoopNum)%PressureSimType > 1) THEN
    !Don't do anything
  ELSE
    Node(OutletNodeNum)%Press              = Node(InletNodeNum)%Press
  END IF
  Node(OutletNodeNum)%Enthalpy             = Node(InletNodeNum)%Enthalpy
  Node(OutletNodeNum)%HumRat               = Node(InletNodeNum)%HumRat

  RETURN

END SUBROUTINE UpdatePipesHeatTransfer

!==============================================================================

SUBROUTINE ReportPipesHeatTransfer(PipeHTNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simply updates the report data

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeHTNum      ! Index for the surface under consideration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  ! update flows and temps from module variables
  PipeHTReport(PipeHTNum)%FluidInletTemp          = InletTemp
  PipeHTReport(PipeHTNum)%FluidOutletTemp         = OutletTemp
  PipeHTReport(PipeHTNum)%MassFlowRate            = MassFlowRate
  PipeHTReport(PipeHTNum)%VolumeFlowRate          = VolumeFlowRate

  ! update other variables from module variables
  PipeHTReport(PipeHTNum)%FluidHeatLossRate       = FluidHeatLossRate
  PipeHTReport(PipeHTNum)%FluidHeatLossEnergy     = FluidHeatLossRate * DeltaTime      ! DeltaTime is in seconds
  PipeHTReport(PipeHTNum)%PipeInletTemp           = PipeHT(PipeHTNum)%PipeTemp(1)
  PipeHTReport(PipeHTNum)%PipeOutletTemp          = PipeHT(PipeHTNum)%PipeTemp(PipeHT(PipeHTNum)%NumSections)

  ! need to average the heat rate because it is now summing over multiple inner time steps
  PipeHTReport(PipeHTNum)%EnvironmentHeatLossRate = EnvHeatLossRate / NumInnerTimeSteps
  PipeHTReport(PipeHTNum)%EnvHeatLossEnergy       = PipeHTReport(PipeHTNum)%EnvironmentHeatLossRate * DeltaTime

  ! for zone heat gains, we assign the averaged heat rate over all inner time steps
  IF (PipeHT(PipeHTNum)%EnvironmentPtr .EQ. ZoneEnv) THEN
      PipeHT(PipeHTNum)%ZoneHeatGainRate = PipeHTReport(PipeHTNum)%EnvironmentHeatLossRate
  END IF

  RETURN

END SUBROUTINE ReportPipesHeatTransfer

!==============================================================================

SUBROUTINE CalcZonePipesHeatGain

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   September 2008
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the zone internal gains due to pipe heat transfer objects.

          ! METHODOLOGY EMPLOYED:
          ! Sums the heat losses from all of the water heaters in the zone to add as a gain to the zone.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag
  USE DataHeatBalance, ONLY: ZoneIntGain

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER :: PipeNum
!  INTEGER :: ZoneNum
  LOGICAL, SAVE :: MyEnvrnFlag=.true.
!  REAL(r64) :: QLossToZone

          ! FLOW:
  IF (NumOfPipeHT == 0) RETURN

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    PipeHT%ZoneHeatGainRate = 0.0d0
    MyEnvrnFlag = .false.
  ENDIF

  IF (.not. BeginEnvrnFlag) MyEnvrnFlag=.true.


! this routine needs to model approx zone pipe gains for use during sizing
!  IF(DoingSizing)THEN
!    DO PipeNum = 1, NumOfPipeHT
!
!      PipeHT(pipeNum)%ZoneHeatGainRate =
!
!    ENDDO
!
!  ENDIF


  RETURN

END SUBROUTINE CalcZonePipesHeatGain

!==============================================================================

REAL(r64) FUNCTION CalcPipeHeatTransCoef(PipeHTNum,Temperature,MassFlowRate,Diameter)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates pipe/fluid heat transfer coefficients.
          ! This routine is adapted from that in the low temp radiant surface model.

          ! METHODOLOGY EMPLOYED:
          ! Currently assumes water data when calculating Pr and Re

          ! REFERENCES:
          ! See RadiantSystemLowTemp module.
          ! Property data for water shown below as parameters taken from
          ! Incropera and DeWitt, Introduction to Heat Transfer, Table A.6.
          ! Heat exchanger information also from Incropera and DeWitt.
          ! Code based loosely on code from IBLAST program (research version)

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: PI
  USE DataPlant,       ONLY: PlantLoop
  USE FluidProperties, ONLY: GetConductivityGlycol, GetViscosityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,      INTENT(IN) :: PipeHTNum
  REAL(r64),    INTENT(IN) :: Temperature   ! Temperature of water entering the surface, in C
  REAL(r64),    INTENT(IN) :: MassFlowRate  ! Mass flow rate, in kg/s
  REAL(r64),    INTENT(IN) :: Diameter      ! Pipe diameter, m

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(*), PARAMETER :: RoutineName = 'PipeHeatTransfer::CalcPipeHeatTransCoef: '
  REAL(r64), PARAMETER    :: MaxLaminarRe       = 2300.d0    ! Maximum Reynolds number for laminar flow
  INTEGER, PARAMETER :: NumOfPropDivisions = 13       ! intervals in property correlation
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Temps=  &   ! Temperature, in C
                   (/1.85d0,6.85d0,11.85d0,16.85d0,21.85d0,26.85d0,31.85d0,36.85d0,41.85d0,46.85d0,51.85d0,56.85d0,61.85d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Mu=  &      ! Viscosity, in Ns/m2
                   (/.001652d0,.001422d0,.001225d0,.00108d0,.000959d0,.000855d0,.000769d0,.000695d0,.000631d0,.000577d0,  &
                     .000528d0,.000489d0,.000453d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Conductivity=  &     ! Conductivity, in W/mK
                   (/.574d0,.582d0,.590d0,.598d0,.606d0,.613d0,.620d0,.628d0,.634d0,.640d0,.645d0,.650d0,.656d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Pr=  &      ! Prandtl number (dimensionless)
                   (/12.22d0,10.26d0,8.81d0,7.56d0,6.62d0,5.83d0,5.20d0,4.62d0,4.16d0,3.77d0,3.42d0,3.15d0,2.88d0/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Index
  REAL(r64)    :: InterpFrac
  REAL(r64)    :: NuD
  REAL(r64)    :: ReD
  REAL(r64)    :: Kactual
  REAL(r64)    :: MUactual
  REAL(r64)    :: PRactual
  INTEGER :: LoopNum

  !retrieve loop index for this component so we can look up fluid properties
  LoopNum = PipeHT(PipeHTNum)%LoopNum

  !since the fluid properties routine doesn't have Prandtl, we'll just use water values
  Index = 1
  DO WHILE (Index <= NumOfPropDivisions)
    IF (Temperature < Temps(Index)) THEN
      IF (Index == 1) THEN
        PRactual = Pr(Index)
      ELSE IF (Index > NumOfPropDivisions) THEN
        PRactual = Pr(NumOfPropDivisions) !CR 8566
      ELSE
        InterpFrac = (Temperature-Temps(Index-1))/(Temps(Index)-Temps(Index-1))
        PRactual   = Pr(Index-1) + InterpFrac*(Pr(Index)-Pr(Index-1))
      END IF
      EXIT ! DO loop
    ELSE !CR 8566
      PRactual   = Pr(NumOfPropDivisions)
    END IF
    Index = Index + 1
  END DO

  !look up conductivity and viscosity
  Kactual = GetConductivityGlycol(PlantLoop(LoopNum)%FluidName, &
                                         PipeHT(PipeHTNum)%FluidTemp(0), &
                                         PlantLoop(LoopNum)%FluidIndex, &
                                         RoutineName) !W/m-K
  MuActual = GetViscosityGlycol(PlantLoop(LoopNum)%FluidName, &
                                         PipeHT(PipeHTNum)%FluidTemp(0), &
                                         PlantLoop(LoopNum)%FluidIndex, &
                                         RoutineName) / 1000.0d0  !Note fluid properties routine returns mPa-s, we need Pa-s

  ! Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter) - as RadiantSysLowTemp
  ReD = 4.0d0 * MassFlowRate / ( PI * MUactual * Diameter)

  IF (ReD == 0.0d0) THEN  ! No flow

    !For now just leave it how it was doing it before
    NuD = 3.66d0
    !Although later it would be nice to have a natural convection correlation

  ELSE ! Calculate the Nusselt number based on what flow regime one is in

    IF (ReD >= MaxLaminarRe) THEN ! Turbulent flow --> use Colburn equation
      NuD = 0.023d0*(ReD**(0.8d0))*(PRactual**(1.d0/3.d0))
    ELSE    ! Laminar flow --> use constant surface temperature relation
      NuD = 3.66d0
    END IF

  ENDIF

  CalcPipeHeatTransCoef = Kactual * NuD / Diameter

  RETURN

END FUNCTION CalcPipeHeatTransCoef

!==============================================================================

REAL(r64) FUNCTION OutsidePipeHeatTransCoef(PipeHTNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the convection heat transfer
          ! coefficient for a cylinder in cross flow.

          ! REFERENCES:
          ! Fundamentals of Heat and Mass Transfer: Incropera and DeWitt, 4th ed.
          ! p. 369-370 (Eq. 7:55b)


          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY : MAT !average (mean) zone air temperature [C]
  USE DataLoopNode,      ONLY : Node
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue
  USE DataEnvironment,   ONLY : WindSpeed

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeHTNum     ! Index number of surface under consideration


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER    :: Pr      = 0.7d0   ! Prandl number for air (assume constant)
  REAL(r64), PARAMETER    :: CondAir      = 0.025d0   ! thermal conductivity of air (assume constant) [W/m.K]
  REAL(r64), PARAMETER    :: RoomAirVel  = 0.381d0   !room air velocity of 75 ft./min [m/s]
  REAL(r64), PARAMETER    :: NaturalConvNusselt = 0.36d0
                             !Nusselt for natural convection for horizontal cylinder
                             !from: Correlations for Convective Heat Transfer
                             !      Dr. Bernhard Spang
                             !      Chemical Engineers’ Resource Page: http://www.cheresources.com/convection.pdf
  INTEGER, PARAMETER :: NumOfParamDivisions = 5       ! intervals in property correlation
  INTEGER, PARAMETER :: NumOfPropDivisions = 12       ! intervals in property correlation

  REAL(r64), PARAMETER, DIMENSION(NumOfParamDivisions) :: CCoef =  &   ! correlation coefficient
                   (/0.989d0,0.911d0,0.683d0,0.193d0,0.027d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfParamDivisions) :: mExp =  &      ! exponent
                   (/0.33d0,0.385d0,0.466d0,0.618d0,0.805d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfParamDivisions) :: LowerBound =  &     ! upper bound of correlation range
                   (/0.4d0,4.d0,40.d0,4000.d0,40000.d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfParamDivisions) :: UpperBound =  &      ! lower bound of correlation range
                   (/4.d0,40.d0,4000.d0,40000.d0,400000.d0/)

  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: Temperature =  &      ! temperature [C]
                   (/-73.d0,-23.d0,-10.d0,0.d0,10.d0,20.d0,27.d0,30.d0,40.d0,50.d0,76.85d0,126.85d0/)
  REAL(r64), PARAMETER, DIMENSION(NumOfPropDivisions) :: DynVisc =  &      ! dynamic viscosity [m^2/s]
                   (/75.52d-7,11.37d-6,12.44d-6,13.3d-6,14.18d-6,15.08d-6,15.75d-6,16d-6,16.95d-6,17.91d-6,20.92d-6,26.41d-6/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Index
  REAL(r64)    :: NuD
  REAL(r64)    :: ReD
  REAL(r64)    :: Coef
  REAL(r64)    :: rExp
  REAL(r64)    :: AirVisc
  REAL(r64)    :: AirVel
  REAL(r64)    :: AirTemp
  REAL(r64)    :: MidTemp
  REAL(r64)    :: PipeOD
  LOGICAL :: ViscositySet
  LOGICAL :: CoefSet

          !Set environmental variables
  SELECT CASE (PipeHT(PipeHTNum)%TypeOf)

    CASE (TypeOf_PipeInterior)

      SELECT CASE (PipeHT(PipeHTNum)%EnvironmentPtr)
        CASE (ScheduleEnv)
          AirTemp = GetCurrentScheduleValue(PipeHT(PipeHTNum)%EnvrSchedPtr)
          AirVel = GetCurrentScheduleValue(PipeHT(PipeHTNum)%EnvrVelSchedPtr)

        CASE (ZoneEnv)
          AirTemp = MAT(PipeHT(PipeHTNum)%EnvrZonePtr)
          AirVel = RoomAirVel
      END SELECT

    CASE (TypeOf_PipeExterior)

      SELECT CASE (PipeHT(PipeHTNum)%EnvironmentPtr)
        CASE (OutsideAirEnv)
          AirTemp = Node(PipeHT(PipeHTNum)%EnvrAirNodeNum)%Temp
          AirVel = WindSpeed
      END SELECT

  END SELECT

  PipeOD = PipeHT(PipeHTNum)%InsulationOD

  ViscositySet = .FALSE.
  DO index = 1, NumOfPropDivisions
    IF(AirTemp <= Temperature(index))THEN
      AirVisc = DynVisc(index)
      ViscositySet = .TRUE.
      EXIT
    ENDIF
  ENDDO

  IF (.NOT. ViscositySet)THEN
      AirVisc = DynVisc(NumOfPropDivisions)
      IF(AirTemp > Temperature(NumOfPropDivisions))THEN
          CALL ShowWarningError('Heat Transfer Pipe = '//TRIM(PipeHT(PipeHTNum)%Name)// &
               'Viscosity out of range, air temperature too high, setting to upper limit.')
      ENDIF
  ENDIF

      ! Calculate the Reynold's number
  CoefSet = .FALSE.
  IF (AirVisc > 0.0d0)THEN
    ReD = AirVel* PipeOD / (AirVisc)
  ENDIF

  DO index = 1,NumOfParamDivisions
    IF(ReD <= Upperbound(index))THEN
      Coef = CCoef(index)
      rExp  = mExp(index)
      CoefSet = .TRUE.
      EXIT
    ENDIF
  ENDDO

  IF (.NOT. CoefSet)THEN
        Coef = CCoef(NumOfParamDivisions)
        rExp  = mExp(NumOfParamDivisions)
        IF(ReD > Upperbound(NumOfParamDivisions))THEN
            CALL ShowWarningError('Heat Transfer Pipe = '//TRIM(PipeHT(PipeHTNum)%Name)// &
               'Reynolds Number out of range, setting coefficients to upper limit.')
        ENDIF
  ENDIF

          ! Calculate the Nusselt number
    NuD = Coef*(ReD**(rExp))*(Pr**(1.d0/3.d0))

          ! If the wind speed is too small, we need to use natural convection behavior:
    NuD = MAX(NuD,NaturalConvNusselt)

          ! h = (k)(Nu)/D
    OutsidePipeHeatTransCoef = CondAir * NuD / PipeOD

  RETURN

END FUNCTION OutsidePipeHeatTransCoef

!==============================================================================

REAL(r64) FUNCTION TBND(z, DayOfSim, PipeHTNum)

          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   December 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS FUNCTION:
          ! Returns a temperature to be used on the boundary of the buried pipe model domain

          ! METHODOLOGY EMPLOYED:
          ! Kusuda and Achenbach correlation is used

          ! REFERENCES: See Module Level Description

  USE DataGlobals,     ONLY : Pi
  REAL(r64), INTENT(IN)    :: z                       !Current Depth
  REAL(r64), INTENT(IN)    :: DayOfSim                !Current Simulation Day
  INTEGER, INTENT(IN)      :: PipeHTNum               !Current Pipe Number

  !Kusuda and Achenbach
  TBND=PipeHT(PipeHTNum)%AvgGroundTemp-PipeHT(PipeHTNum)%AvgGndTempAmp* &
        Exp(-z*((Pi/(365.d0*PipeHT(PipeHTNum)%SoilDiffusivityPerDay))**(0.5d0)))* &
        Cos((2.d0*Pi/365.d0)*(DayOfSim-PipeHT(PipeHTNum)%PhaseShiftDays-(z/2.d0)* &
        ((365.d0/(Pi*PipeHT(PipeHTNum)%SoilDiffusivityPerDay))**(0.5d0))))

  RETURN

End Function

!===============================================================================


!===============================================================================

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


END MODULE PipeHeatTransfer
