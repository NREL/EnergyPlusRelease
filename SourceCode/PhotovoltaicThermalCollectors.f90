MODULE PhotovoltaicThermalCollectors

          ! Module containing the routines dealing with the photovoltaic thermal collectors

          ! MODULE INFORMATION:
          !       AUTHOR         Brent. Griffith
          !       DATE WRITTEN   June-August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! collect models related to PVT or hybrid, photovoltaic - thermal solar collectors

          ! METHODOLOGY EMPLOYED:
          ! The approach is to have one PVT structure that works with different models.
          !  the PVT modle reuses photovoltaic modeling in Photovoltaics.f90 for electricity generation.
          !  the electric load center and "generator" is all accessed thru PV objects and models.
          !  this module is for the thermal portion of PVT.
          !  the first model is a "simple" or "ideal" model useful for sizing, early design, or policy analyses
          !  Simple PV/T model just converts incoming solar to electricity and temperature rise of a working fluid.

          ! REFERENCES:


          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataSurfaces, ONLY: Surface, TotSurfaces, SurfSunlitArea, SurfSunlitFrac, SurfaceClass_Detached_F, SurfaceClass_Detached_B,  &
                        SurfaceClass_Shading
USE DataPhotovoltaics
USE DataInterfaces


IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: SimplePVTmodel       = 1001
INTEGER, PARAMETER :: LayerByLayerPVTmodel = 1002

INTEGER, PARAMETER :: ScheduledThermEffic = 15 ! mode for thermal efficiency is to use schedule
INTEGER, PARAMETER :: FixedThermEffic     = 16 ! mode for thermal efficiency is to use fixed value

INTEGER, PARAMETER :: LiquidWorkingFluid  = 1
INTEGER, PARAMETER :: AirWorkingFluid     = 2

INTEGER, PARAMETER , PUBLIC :: CalledFromPlantLoopEquipMgr  = 101
INTEGER, PARAMETER , PUBLIC :: CalledFromOutsideAirSystem = 102

REAL(r64), PARAMETER :: SimplePVTWaterSizeFactor = 1.905d-5  ! [ m3/s/m2 ] average of collectors in SolarCollectors.idf


          ! DERIVED TYPE DEFINITIONS:

TYPE SimplePVTModelStruct
  CHARACTER(len=MaxNameLength) :: Name               = '' !
  REAL(r64)                    :: ThermalActiveFract = 0.0D0 ! fraction of surface area with active thermal collection
  INTEGER                      :: ThermEfficMode     = 0  ! setting for how therm effic is determined
  REAL(r64)                    :: ThermEffic         = 0.0D0 ! fixed or current Therm efficiency
  INTEGER                      :: ThermEffSchedNum   = 0 ! pointer to schedule for therm effic (if any)
  REAL(r64)                    :: SurfEmissivity     = 0.0D0  ! surface emittance in long wave IR
  REAL(r64)                    :: LastCollectorTemp  = 0.0D0  ! store previous temperature
  REAL(r64)                    :: CollectorTemp      = 0.0D0  ! average solar collector temp.
END TYPE SimplePVTModelStruct

TYPE PVTReportStruct

  REAL(r64) :: ThermEfficiency       = 0.0D0 ! Thermal efficiency of solar energy conversion
  REAL(r64) :: ThermPower            = 0.0D0 ! Heat gain or loss to collector fluid (W)
  REAL(r64) :: ThermHeatGain         = 0.0D0 ! Heat gain to collector fluid (W)
  REAL(r64) :: ThermHeatLoss         = 0.0D0 ! Heat loss from collector fluid (W)
  REAL(r64) :: ThermEnergy           = 0.0D0 ! Energy gained (or lost) to collector fluid (J)
  REAL(r64) :: MdotWorkFluid         = 0.0D0 ! working fluid mass flow rate (kg/s)
  REAL(r64) :: TinletWorkFluid       = 0.0D0 ! working fluid inlet temp (C)
  REAL(r64) :: ToutletWorkFluid      = 0.0D0 ! working fluid outlet temp (C)
  REAL(r64) :: BypassStatus          = 0.0D0  ! 0 = no bypass, 1=full bypass
END TYPE PVTReportStruct

TYPE PVTCollectorStruct
  ! input
  CHARACTER(len=MaxNameLength) :: Name               = '' ! Name of PVT collector
  INTEGER                      :: TypeNum                  ! Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
  INTEGER                      :: WLoopNum     = 0         ! Water plant loop index number                      !DSU
  INTEGER                      :: WLoopSideNum = 0         ! Water plant loop side index                        !DSU
  INTEGER                      :: WLoopBranchNum   = 0     ! Water plant loop branch index                      !DSU
  INTEGER                      :: WLoopCompNum     = 0     ! Water plant loop component index                   !DSU
  LOGICAL                      :: EnvrnInit          = .TRUE. ! manage begin environmen inits
  LOGICAL                      :: SizingInit         = .TRUE. ! manage when sizing is complete
  CHARACTER(len=MaxNameLength) :: PVTModelName       = '' ! Name of PVT performance object
  INTEGER                      :: PVTModelType       = 0  ! model type indicator, only simple avail now
  INTEGER                      :: SurfNum            = 0  ! surface index
  CHARACTER(len=MaxNameLength) :: PVname             = '' ! named Generator:Photovoltaic object
  INTEGER                      :: PVNum              = 0  ! PV index
  LOGICAL                      :: PVfound            = .FALSE. ! init, need to delay get input until PV gotten
 ! INTEGER                      :: PlantLoopNum       = 0  ! needed for sizing and control
 ! INTEGER                      :: PlantLoopSide      = 0  ! needed for sizing, demand vs. supply sided
  Type(SimplePVTModelStruct)   :: Simple                  ! performance data structure.

  INTEGER                      :: WorkingFluidType   = 0  !
  INTEGER                      :: PlantInletNodeNum  = 0  !
  INTEGER                      :: PlantOutletNodeNum = 0
  INTEGER                      :: HVACInletNodeNum   = 0
  INTEGER                      :: HVACOutletNodeNum  = 0
  REAL(r64)                    :: DesignVolFlowRate  = 0.0D0
  REAL(r64)                    :: MaxMassFlowRate    = 0.0D0
  REAL(r64)                    :: MassFlowRate       = 0.0D0  !DSU
  REAL(r64)                    :: AreaCol            = 0.0D0
  LOGICAL                      :: BypassDamperOff    = .TRUE.
  LOGICAL                      :: CoolingUseful      = .FALSE.
  LOGICAL                      :: HeatingUseful      = .FALSE.
  TYPE(PVTReportStruct)        :: Report
END TYPE PVTCollectorStruct

          ! MODULE VARIABLE DECLARATIONS:
TYPE (PVTCollectorStruct), ALLOCATABLE, DIMENSION(:) :: PVT
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
INTEGER  :: NumPVT =0              ! count of all types of PVT in input file
INTEGER  :: NumSimplePVTPerform=0    ! count of simple PVT performance objects in input file

          ! SUBROUTINE SPECIFICATIONS FOR MODULE:
     ! Driver/Manager Routines
PUBLIC  SimPVTcollectors       !main entry point, called from non-zone equipment manager
PRIVATE GetPVTcollectorsInput  !
PRIVATE InitPVTcollectors
PRIVATE SizePVT
PRIVATE ControlPVTcollector
PRIVATE CalcPVTcollectors
PRIVATE UpdatePVTcollectors


     ! Utility routines for module
! these would be public such as:
PUBLIC  GetPVTThermalPowerProduction
!PUBLIC  GetPVTIncidentSolarForInternalPVLayer
!PUBLIC  GetPVTCellTemp


CONTAINS

SUBROUTINE SimPVTcollectors(PVTNum, FirstHVACIteration, CalledFrom, PVTName, InitLoopEquip )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY:FindItemInList
  USE General,        ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(INOUT) :: PVTnum  ! index to PVT array.
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  INTEGER, INTENT(IN) :: CalledFrom
  CHARACTER(len=*), OPTIONAL, INTENT (IN)   :: PVTName
  LOGICAL, OPTIONAL, INTENT(IN) :: InitLoopEquip

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE      :: GetInputFlag = .true.  ! First time, input is "gotten"

  IF (GetInputFlag) THEN
    CALL GetPVTcollectorsInput
    GetInputFlag=.false.
  ENDIF

  IF (PRESENT(PVTName)) THEN
    IF ( PVTNum == 0) THEN
      PVTnum =  FindItemInList(PVTName,PVT%Name,NumPVT)
      IF (PVTnum == 0) THEN
        CALL ShowFatalError('SimPVTcollectors: Unit not found='//TRIM(PVTName))
      ENDIF
    ELSE
      IF (PVTnum > NumPVT .OR. PVTnum < 1 ) THEN
        CALL ShowFatalError('SimPVTcollectors: Invalid PVT index passed = '// &
                              TRIM(TrimSigDigits(PVTnum))// &
                              ', Number of PVT units='//TRIM(TrimSigDigits(NumPVT))//  &
                              ', Entered Unit name='//TRIM(PVTName))
      ENDIF
      IF (CheckEquipName(PVTnum)) THEN
        IF (PVTName /= PVT(PVTnum)%Name) THEN
          CALL ShowFatalError('SimPVTcollectors: Invalid PVT index passed = '// &
                                TRIM(TrimSigDigits(PVTnum))// &
                                ', Unit name='//TRIM(PVTName)//  &
                                ', stored name for that index='//TRIM(PVT(PVTnum)%Name))
        ENDIF
        CheckEquipName(PVTnum)=.false.
      ENDIF
    ENDIF
  ELSE
    IF (PVTnum > NumPVT .OR. PVTnum < 1 ) THEN
      CALL ShowFatalError('SimPVTcollectors: Invalid PVT index passed = '// &
                            TRIM(TrimSigDigits(PVTnum))// &
                            ', Number of PVT units='//TRIM(TrimSigDigits(NumPVT))//  &
                            ', Entered Unit name='//TRIM(PVTName))
    ENDIF
  ENDIF ! compName present

  IF (PRESENT(InitLoopEquip)) THEN
    IF (InitLoopEquip) THEN
      CALL InitPVTcollectors( PVTnum, FirstHVACIteration )
      RETURN
    ENDIF
  ENDIF

  !check where called from and what type of collector this is, return if not right for calling order for speed
  IF ((PVT(PVTnum)%WorkingFluidType == AirWorkingFluid) .AND. (CalledFrom == CalledFromPlantLoopEquipMgr) ) RETURN
  IF ((PVT(PVTnum)%WorkingFluidType == LiquidWorkingFluid) .AND. (CalledFrom == CalledFromOutsideAirSystem) ) RETURN

  CALL InitPVTcollectors( PVTnum, FirstHVACIteration )

  CALL ControlPVTcollector( PVTnum )

  CALL CalcPVTcollectors( PVTnum )

  CALL UpdatePVTcollectors( PVTnum )



  RETURN

END SUBROUTINE SimPVTcollectors

SUBROUTINE GetPVTcollectorsInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get input for PVT objects

          ! METHODOLOGY EMPLOYED:
          ! usual E+ methods

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem , FindItemInList, SameString, &
                            VerifyName
  USE DataIPShortCuts
  USE DataHeatBalance
  USE DataLoopNode
  USE DataEnvironment, ONLY: StdRhoAir
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE DataInterfaces, ONLY: SetupOutputVariable
  USE DataSizing ,    ONLY: Autosize
  USE General,        ONLY: RoundSigDigits
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataPlant      !DSU

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: Item    ! Item to be "gotten"
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER    :: SurfNum ! local use only
  TYPE (SimplePVTModelStruct), ALLOCATABLE, DIMENSION(:) :: tmpSimplePVTperf
  INTEGER                        :: ThisParamObj !
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name


  ! first load the performance object info into temporary structure
  cCurrentModuleObject = 'SolarCollectorPerformance:PhotovoltaicThermal:Simple'
  NumSimplePVTPerform = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumSimplePVTPerform > 0) Then
    Allocate(tmpSimplePVTperf(NumSimplePVTPerform))
    DO Item=1, NumSimplePVTPerform
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNumbers,IOStatus, AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),tmpSimplePVTperf%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Names')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) THEN
          CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//', Name cannot be blank')
        ENDIF
        CYCLE
      ENDIF
      tmpSimplePVTperf(Item)%Name = TRIM(cAlphaArgs(1))
      If (SameString(cAlphaArgs(2), 'Fixed')) Then
        tmpSimplePVTperf(Item)%ThermEfficMode = FixedThermEffic
      ELSEIF (SameString(cAlphaArgs(2), 'Scheduled')) Then
        tmpSimplePVTperf(Item)%ThermEfficMode = ScheduledThermEffic
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        ErrorsFound=.true.
      ENDIF
      tmpSimplePVTperf(Item)%ThermalActiveFract = rNumericArgs(1)
      tmpSimplePVTperf(Item)%ThermEffic         = rNumericArgs(2)

      tmpSimplePVTperf(Item)%ThermEffSchedNum = GetScheduleIndex(cAlphaArgs(3))
      IF ( (tmpSimplePVTperf(Item)%ThermEffSchedNum == 0) &
           .AND. (tmpSimplePVTperf(Item)%ThermEfficMode == ScheduledThermEffic) ) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        ErrorsFound=.true.
      ENDIF
      tmpSimplePVTperf(Item)%SurfEmissivity     = rNumericArgs(3)

    ENDDO
  ENDIF !NumSimplePVTPerform > 0

  ! now get main PVT objects
  cCurrentModuleObject = 'SolarCollector:FlatPlate:PhotovoltaicThermal'
  NumPVT              = GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(PVT(NumPVT))
  ALLOCATE(CheckEquipName(NumPVT))
  CheckEquipName=.true.

  DO Item=1, NumPVT
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNumbers,IOStatus, AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    !check name
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1), PVT%Name, Item -1, IsNotOK, IsBlank, &
                     TRIM(cCurrentModuleObject) )
    If (IsNotOK) Then
      ErrorsFound = .true.
      IF (IsBlank) THEN
          CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//', Name cannot be blank')
      ENDIF
      CYCLE
    ENDIF
    PVT(Item)%Name = cAlphaArgs(1)
    PVT(Item)%TypeNum = TypeOf_PVTSolarCollectorFlatPlate !DSU, assigned in DataPlant

    PVT(Item)%SurfNum = FindItemInList(cAlphaArgs(2),Surface%Name,TotSurfaces)
    ! check surface
    IF (PVT(Item)%SurfNum == 0) THEN
      IF (lAlphaFieldBlanks(2)) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Surface name cannot be blank.')
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Surface was not found.')
      ENDIF
      ErrorsFound=.true.
    ELSE
!     ! Found one -- make sure has right parameters for PVT
      SurfNum = PVT(Item)%SurfNum

      IF (.NOT. Surface(PVT(Item)%SurfNum)%ExtSolar) THEN
         CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
         CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
         CALL ShowContinueError('Surface must be exposed to solar.' )
         ErrorsFound=.true.
      END IF
! check surface orientation, warn if upside down
      IF (( Surface(SurfNum)%Tilt < -95.0D0 ) .OR. (Surface(SurfNum)%Tilt > 95.0D0)) THEN
        CALL ShowWarningError('Suspected input problem with '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError( 'Surface used for solar collector faces down')
        CALL ShowContinueError('Surface tilt angle (degrees from ground outward normal) = ' &
                                   //TRIM(RoundSigDigits(Surface(SurfNum)%Tilt,2) ) )
      ENDIF

    ENDIF ! check surface

    If (lAlphaFieldBlanks(3)) Then
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError(TRIM(cAlphaFieldNames(3))//', name cannot be blank.')
        ErrorsFound=.true.
    ELSE
      PVT(Item)%PVTModelName = cAlphaArgs(3)
      ThisParamObj = FindItemInList( PVT(Item)%PVTModelName, tmpSimplePVTperf%Name, NumSimplePVTPerform)
      IF (ThisParamObj > 0) THEN
        PVT(Item)%Simple = tmpSimplePVTperf(ThisParamObj) ! entire structure assigned
         ! do one-time setups on input data
        PVT(Item)%AreaCol = Surface(PVT(Item)%SurfNum)%Area * PVT(Item)%Simple%ThermalActiveFract
        PVT(Item)%PVTModelType = SimplePVTmodel
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError(TRIM(cAlphaFieldNames(3))//', was not found.')
        ErrorsFound=.true.
      ENDIF

    ENDIF
    !
    IF (ALLOCATED(PVarray)) THEN ! then PV input gotten... but don't expect this to be true.
      PVT(Item)%PVnum = FindItemInList( cAlphaArgs(4) ,PVarray%name, NumPVs)
    ! check PV
      IF (PVT(Item)%PVnum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//' = '//TRIM(cAlphaArgs(4)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        ErrorsFound=.true.
      ELSE
        PVT(Item)%PVname = TRIM(cAlphaArgs(4))
        PVT(Item)%PVfound = .TRUE.
      endif
    ELSE ! no PV or not yet gotten.
      PVT(Item)%PVname = TRIM(cAlphaArgs(4))
      PVT(Item)%PVfound = .FALSE.
    ENDIF

    IF (SameString(cAlphaArgs(5), 'Water' )) THEN
      PVT(Item)%WorkingFluidType = LiquidWorkingFluid
    ELSEIF (SameString (cAlphaArgs(5), 'Air' )) THEN
      PVT(Item)%WorkingFluidType = AirWorkingFluid
    ELSE
      IF (lAlphaFieldBlanks(5)) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' field cannot be blank.')
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      ENDIF
      ErrorsFound=.true.
    ENDIF

    IF (PVT(Item)%WorkingFluidType == LiquidWorkingFluid) THEN
      PVT(Item)%PlantInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent )
      PVT(Item)%PlantOutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent )

      CALL TestCompSet(TRIM(cCurrentModuleObject), cAlphaArgs(1), cAlphaArgs(6), cAlphaArgs(7), 'Water Nodes')

      PVT(Item)%WLoopSideNum =  DemandSupply_No

    ENDIF

    IF (PVT(Item)%WorkingFluidType == AirWorkingFluid) THEN
      PVT(Item)%HVACInletNodeNum = &
          GetOnlySingleNode(cAlphaArgs(8), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1), &
          NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
      PVT(Item)%HVACOutletNodeNum = &
          GetOnlySingleNode(cAlphaArgs(9), ErrorsFound, TRIM(cCurrentModuleObject), cAlphaArgs(1), &
          NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet( TRIM(cCurrentModuleObject), cAlphaArgs(1), cAlphaArgs(8), cAlphaArgs(9), 'Air Nodes' )

    ENDIF

    PVT(Item)%DesignVolFlowRate = rNumericArgs(1)
    PVT(Item)%SizingInit = .TRUE.
    IF (PVT(Item)%DesignVolFlowRate /= Autosize) THEN

      IF (PVT(Item)%WorkingFluidType == LiquidWorkingFluid) Then
        CALL RegisterPlantCompDesignFlow( PVT(Item)%PlantInletNodeNum, PVT(Item)%DesignVolFlowRate )
      ELSEIF(PVT(Item)%WorkingFluidType == AirWorkingFluid) Then
        PVT(Item)%MaxMassFlowRate   = PVT(Item)%DesignVolFlowRate * StdRhoAir
      ENDIF
      PVT(Item)%SizingInit = .FALSE.
    ENDIF

  ENDDO

  DO Item=1, NumPVT
   ! electrical production reporting under generator:photovoltaic....
   !    only thermal side reported here,

    CALL SetupOutputVariable('Generator Produced Thermal Rate [W]', &
         PVT(Item)%Report%ThermPower, 'System', 'Average', PVT(Item)%name )
    IF (PVT(Item)%WorkingFluidType == LiquidWorkingFluid) THEN
      CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', &
           PVT(Item)%Report%ThermEnergy, 'System', 'Sum', PVT(Item)%name , &
           ResourceTypeKey='SolarWater', EndUseKey='HeatProduced', GroupKey='Plant')
    ELSEIF (PVT(Item)%WorkingFluidType == AirWorkingFluid) THEN
      CALL SetupOutputVariable('Generator Produced Thermal Energy [J]', &
           PVT(Item)%Report%ThermEnergy, 'System', 'Sum', PVT(Item)%name , &
           ResourceTypeKey='SolarAir', EndUseKey='HeatProduced', GroupKey='System')
      CALL SetupOutputVariable('Generator PVT Fluid Bypass Status []', &
           PVT(Item)%Report%BypassStatus, 'System', 'Average', PVT(Item)%name )
    ENDIF

    CALL SetupOutputVariable('Generator PVT Fluid Inlet Temperature [C]', &
           PVT(Item)%Report%TinletWorkFluid, 'System', 'Average', PVT(Item)%name )
    CALL SetupOutputVariable('Generator PVT Fluid Outlet Temperature [C]', &
           PVT(Item)%Report%ToutletWorkFluid, 'System', 'Average', PVT(Item)%name )
    CALL SetupOutputVariable('Generator PVT Fluid Mass Flow Rate [kg/s]', &
           PVT(Item)%Report%MdotWorkFluid, 'System', 'Average', PVT(Item)%name )
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for photovoltaic thermal collectors')
  ENDIF

  IF (ALLOCATED(tmpSimplePVTperf)) DEALLOCATE(tmpSimplePVTperf)

  RETURN

END SUBROUTINE GetPVTcollectorsInput

SUBROUTINE InitPVTcollectors(PVTnum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   June 2008
          !       MODIFIED       B. Griffith, May 2009, EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! init for PVT

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: SysSizingCalc, InitConvTemp, AnyEnergyManagementSystemInModel
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow
  USE DataLoopNode,    ONLY: Node, SensedNodeFlagValue
  USE FluidProperties, ONLY: GetDensityGlycol
  USE InputProcessor,  ONLY: FindItemInList
  USE DataHVACGlobals, ONLY: DoSetPointTest, SetPointErrorFlag
  USE DataHeatBalance, ONLY: QRadSWOutIncident
  USE General,         ONLY: RoundSigDigits
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataPlant,       ONLY: ScanPlantLoopsForObject, PlantLoop
  USE PlantUtilities,  ONLY: SetComponentFlowRate , InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PVTnum
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: PVTindex
  INTEGER :: SurfNum
  LOGICAL :: ErrorsFound = .FALSE.
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag       ! get loop number flag
  LOGICAL :: errFlag
  REAL(r64) :: rho ! local fluid density kg/s
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(SetLoopIndexFlag(NumPVT))
    SetLoopIndexFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF(SetLoopIndexFlag(PVTnum))THEN
    IF(ALLOCATED(PlantLoop) .AND. (PVT(PVTnum)%PlantInletNodeNum >0 ) )THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(PVT(PVTnum)%Name, &
                                   PVT(PVTnum)%TypeNum, &
                                   PVT(PVTnum)%WLoopNum, &
                                   PVT(PVTnum)%WLoopSideNum, &
                                   PVT(PVTnum)%WLoopBranchNum, &
                                   PVT(PVTnum)%WLoopCompNum,   &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitPVTcollectors: Program terminated for previous conditions.')
      ENDIF
      SetLoopIndexFlag(PVTnum) = .FALSE.
    ENDIF
  ENDIF


  ! finish set up of PV, becaues PV get-input follows PVT's get input.
  IF (.NOT. PVT(PVTnum)%PVfound) Then
    IF (ALLOCATED(PVarray)) THEN
      PVT(PVTnum)%PVnum = FindItemInList( PVT(PVTnum)%PVname ,PVarray%name, NumPVs)
      IF (PVT(PVTnum)%PVnum == 0) THEN
        CALL ShowSevereError('Invalid name for photovoltaic generator = '//TRIM(PVT(PVTnum)%PVname) )
        CALL ShowContinueError('Entered in flat plate photovoltaic-thermal collector = '//TRIM(PVT(PVTnum)%Name) )
        ErrorsFound=.TRUE.
      ELSE
        PVT(PVTnum)%PVfound = .TRUE.
      ENDIF
    ELSE
      IF ((.NOT. BeginEnvrnFlag) .AND. (.NOT. FirstHVACIteration)) THEN
        CALL ShowSevereError('Photovoltaic generators are missing for Photovoltaic Thermal modeling' )
        CALL ShowContinueError('Needed for flat plate photovoltaic-thermal collector = '//TRIM(PVT(PVTnum)%Name) )
        ErrorsFound=.true.
      ENDIF
    ENDIF
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
    DO PVTindex = 1, NumPVT
      IF (PVT(PVTindex)%WorkingFluidType == AirWorkingFluid) THEN
        IF (Node(PVT(PVTindex)%HVACOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            CALL ShowSevereError( 'Missing temperature setpoint for PVT outlet node  ')
            CALL ShowContinueError('Add a setpoint manager to outlet node of PVT named ' &
                                    //Trim(PVT(PVTindex)%Name) )
            SetPointErrorFlag = .TRUE.
          ELSE
           ! need call to EMS to check node
            CALL CheckIfNodeSetpointManagedByEMS(PVT(PVTindex)%HVACOutletNodeNum,iTemperatureSetpoint, SetPointErrorFlag)
            IF (SetPointErrorFlag) THEN
              CALL ShowSevereError( 'Missing temperature setpoint for PVT outlet node  ')
              CALL ShowContinueError('Add a setpoint manager to outlet node of PVT named ' &
                                    //Trim(PVT(PVTindex)%Name) )
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node of PVT')
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    MySetPointCheckFlag = .FALSE.
  END IF

  !Size design flow rate
  IF ( .NOT. SysSizingCalc .AND. PVT(PVTnum)%SizingInit) THEN
    CALL SizePVT(PVTnum)
  ENDIF

  SELECT CASE (PVT(PVTnum)%WorkingFluidType)
  CASE (LiquidWorkingFluid)
    InletNode  = PVT(PVTnum)%PlantInletNodeNum
    OutletNode = PVT(PVTnum)%PlantOutletNodeNum
  CASE (AirWorkingFluid)
    InletNode  = PVT(PVTnum)%HVACInletNodeNum
    OutletNode = PVT(PVTnum)%HVACOutletNodeNum
  END SELECT

  IF (BeginEnvrnFlag .AND. PVT(PVTnum)%EnvrnInit ) THEN

    PVT(PVTnum)%MassFlowRate                 = 0.d0
    PVT(PVTnum)%BypassDamperOff              = .TRUE.
    PVT(PVTnum)%CoolingUseful                = .FALSE.
    PVT(PVTnum)%HeatingUseful                = .FALSE.
    PVT(PVTnum)%Simple%LastCollectorTemp     = 0.d0
    PVT(PVTnum)%Simple%CollectorTemp         = 0.d0
    PVT(PVTnum)%Report%ThermEfficiency       = 0.d0
    PVT(PVTnum)%Report%ThermPower            = 0.d0
    PVT(PVTnum)%Report%ThermHeatGain         = 0.d0
    PVT(PVTnum)%Report%ThermHeatLoss         = 0.d0
    PVT(PVTnum)%Report%ThermEnergy           = 0.d0
    PVT(PVTnum)%Report%MdotWorkFluid         = 0.d0
    PVT(PVTnum)%Report%TinletWorkFluid       = 0.d0
    PVT(PVTnum)%Report%ToutletWorkFluid      = 0.d0
    PVT(PVTnum)%Report%BypassStatus          = 0.d0


    SELECT CASE (PVT(PVTnum)% WorkingFluidType)

    CASE (LiquidWorkingFluid)

      rho = GetDensityGlycol(PlantLoop(PVT(PVTnum)%WLoopNum)%FluidName, &
                               60.d0, &
                               PlantLoop(PVT(PVTnum)%WLoopNum)%FluidIndex, &
                               'InitPVTcollectors')

      PVT(PVTnum)%MaxMassFlowRate = PVT(PVTnum)%DesignVolFlowRate * rho

      CALL InitComponentNodes(0.d0, PVT(PVTnum)%MaxMassFlowRate, &
                                   InletNode, OutletNode,        &
                                   PVT(PVTnum)%WLoopNum,         &
                                   PVT(PVTnum)%WLoopSideNum,     &
                                   PVT(PVTnum)%WLoopBranchNum,   &
                                   PVT(PVTnum)%WLoopCompNum )

      PVT(PVTnum)%Simple%LastCollectorTemp  = 23.0D0

    CASE (AirWorkingFluid)
      PVT(PVTnum)%Simple%LastCollectorTemp  = 23.0D0

    END SELECT

    PVT(PVTnum)%EnvrnInit = .FALSE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) PVT(PVTnum)%EnvrnInit = .TRUE.

  SELECT CASE (PVT(PVTnum)% WorkingFluidType)

  CASE (LiquidWorkingFluid)
    ! heating only right now, so control flow requests based on incident solar
    SurfNum  = PVT(PVTnum)%SurfNum
    IF (QRadSWOutIncident(SurfNum) > MinIrradiance) THEN
      !IF (FirstHVACIteration) THEN
        PVT(PVTnum)%MassFlowRate        = PVT(PVTnum)%MaxMassFlowRate  !DSU
      !ENDIF
    ELSE
      !IF (FirstHVACIteration) THEN
        PVT(PVTnum)%MassFlowRate        = 0.0D0  !DSU
      !ENDIF
    ENDIF
    ! Should we declare a mass flow rate variable in the data structure instead of using node(outlet)%massflowrate ?  DSU
    CALL SetComponentFlowRate( PVT(PVTnum)%MassFlowRate,InletNode,OutletNode, &     !DSU
                               PVT(PVTnum)%WLoopNum,PVT(PVTnum)%WLoopSideNum , &
                               PVT(PVTnum)%WLoopBranchNum , PVT(PVTnum)%WLoopCompNum)      !DSU
  CASE (AirWorkingFluid)
    PVT(PVTnum)%MassFlowRate = Node(InletNode)%MassFlowRate

  END SELECT

  RETURN

END SUBROUTINE InitPVTcollectors

SUBROUTINE SizePVT(PVTnum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing PVT flow rates that
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains hot water flow rate from the plant sizing array.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,        ONLY: PlantLoop, SupplySide, DemandSide
  USE DataInterfaces,   ONLY: ShowFatalError, ShowSevereError, ShowContinueError, SetupOutputVariable
  USE DataHVACGlobals,  ONLY: SmallWaterVolFlow, SmallAirVolFlow, Main, Cooling, Heating, Other
  USE PlantUtilities,   ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE DataAirSystems,   ONLY: PrimaryAirSystem
  USE DataAirLoop,      ONLY: AirLoopControlInfo
  USE DataEnvironment , ONLY: StdRhoAir
  USE DataLoopNode  ,   ONLY: Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PVTnum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  LOGICAL                      :: ErrorsFound   ! If errors detected in input
!unused1208  CHARACTER(len=MaxNameLength) :: equipName     ! Name of boiler object
  REAL(r64)                    :: DesVolFlow
  REAL(r64)                    :: DesMassFlow

  PltSizNum = 0
  ErrorsFound = .FALSE.



  IF (PVT(PVTnum)%WorkingFluidType == LiquidWorkingFluid) THEN

    IF ( .NOT. ALLOCATED(PlantSizData)) RETURN
    If ( .NOT. ALLOCATED(PlantLoop  ) ) RETURN

    IF (PVT(PVTnum)%WLoopNum > 0) THEN
      PltSizNum = PlantLoop(PVT(PVTnum)%WLoopNum)%PlantSizNum
    END IF

    IF (PVT(PVTnum)%DesignVolFlowRate  == AutoSize) THEN
      IF (PVT(PVTnum)%WLoopSideNum == SupplySide) Then
        IF (PltSizNum > 0) THEN
          IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
            DesVolFlow = PlantSizData(PltSizNum)%DesVolFlowRate
          ELSE
            DesVolFlow = 0.0d0
          END IF
          PVT(PVTnum)%DesignVolFlowRate = DesVolFlow
        ELSE
          CALL ShowSevereError('Autosizing of PVT solar collector design flow rate requires a Sizing:Plant object')
          CALL ShowContinueError('Occurs in PVT object='//TRIM(PVT(PVTnum)%Name))
          ErrorsFound = .TRUE.
        END IF
      ELSEIF (PVT(PVTnum)%WLoopSideNum == DemandSide) THEN
        PVT(PVTnum)%DesignVolFlowRate = PVT(PVTnum)%AreaCol * SimplePVTWaterSizeFactor

      ENDIF
      CALL ReportSizingOutput('SolarCollector:FlatPlate:PhotovoltaicThermal', PVT(PVTnum)%Name, &
                              'Design Flow Rate [m3/s]', &
                              PVT(PVTnum)%DesignVolFlowRate)
      CALL RegisterPlantCompDesignFlow( PVT(PVTnum)%PlantInletNodeNum, PVT(PVTnum)%DesignVolFlowRate )
      PVT(PVTnum)%SizingInit = .FALSE.
    END IF

  ENDIF !plant component

  IF (PVT(PVTnum)%WorkingFluidType == AirWorkingFluid) THEN

    IF (PVT(PVTnum)%DesignVolFlowRate  == AutoSize) THEN

      IF (CurSysNum > 0) THEN
        CALL CheckSysSizing('SolarCollector:FlatPlate:PhotovoltaicThermal', PVT(PVTnum)%Name)
        IF (CurOASysNum > 0) THEN
          DesVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
        ELSE
          SELECT CASE(CurDuctType)
          CASE(Main)
            DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE(Cooling)
            DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesCoolVolFlow
          CASE(Heating)
            DesVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
          CASE(Other)
            DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE DEFAULT
            DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
          END SELECT

        END IF
        DesMassFlow = StdRhoAir *DesVolFlow
        PVT(PVTnum)%DesignVolFlowRate = DesVolFlow
        PVT(PVTnum)%MaxMassFlowRate   = DesMassFlow

        CALL ReportSizingOutput('SolarCollector:FlatPlate:PhotovoltaicThermal', PVT(PVTnum)%Name, &
                                'Design Flow Rate [m3/s]', &
                                PVT(PVTnum)%DesignVolFlowRate)
        PVT(PVTnum)%SizingInit = .FALSE.
      ELSE IF (CurZoneEqNum > 0) THEN
       ! PVT is not currently for zone equipment, should not come here.

      ENDIF

    ENDIF

  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizePVT

SUBROUTINE ControlPVTcollector(PVTnum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! make control decisions for PVT collector

          ! METHODOLOGY EMPLOYED:
          ! decide if PVT should be in cooling or heat mode and if it should be bypassed or not

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode   , ONLY: Node
  USE DataHeatBalance, ONLY: QRadSWOutIncident
  USE DataPlant ,      ONLY: PlantReport
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: PVTnum !

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: SurfNum      = 0
!  INTEGER   :: PlantLoopNum = 0
!  REAL(r64) :: mdot  = 0.0D0

  SurfNum     = PVT(PVTnum)%SurfNum

  IF ( PVT(PVTnum)%WorkingFluidType == AirWorkingFluid ) THEN

    IF (PVT(PVTnum)%PVTModelType ==  SimplePVTmodel) THEN
      IF (QRadSWOutIncident(SurfNum) > MinIrradiance) then
         ! is heating wanted?
         !  Outlet node is required to have a setpoint.
        IF ( Node(PVT(PVTnum)%HVACOutletNodeNum)%TempSetPoint &
              >  Node(PVT(PVTnum)%HVACInletNodeNum)%Temp )   THEN
          PVT(PVTnum)%HeatingUseful   = .TRUE.
          PVT(PVTnum)%CoolingUseful   = .FALSE.
          PVT(PVTnum)%BypassDamperOff = .TRUE.
        ELSE
          PVT(PVTnum)%HeatingUseful   = .FALSE.
          PVT(PVTnum)%CoolingUseful   = .TRUE.
          PVT(PVTnum)%BypassDamperOff = .FALSE.
        ENDIF
      ELSE
        ! is cooling wanted?
        IF (Node(PVT(PVTnum)%HVACOutletNodeNum)%TempSetPoint &
              < Node(PVT(PVTnum)%HVACInletNodeNum)%Temp ) THEN
          PVT(PVTnum)%CoolingUseful   = .TRUE.
          PVT(PVTnum)%HeatingUseful   = .FALSE.
          PVT(PVTnum)%BypassDamperOff = .TRUE.
        ELSE
          PVT(PVTnum)%CoolingUseful   = .FALSE.
          PVT(PVTnum)%HeatingUseful   = .TRUE.
          PVT(PVTnum)%BypassDamperOff = .FALSE.
        ENDIF
      ENDIF
    ENDIF

  ELSEIF ( PVT(PVTnum)%WorkingFluidType == LiquidWorkingFluid ) THEN
    !PlantLoopNum = PVT(PVTNum)%PlantLoopNum
!    mdot   = Node(PVT(PVTNum)%PlantInletNodeNum)%MassFlowRate
    !If (.NOT. Allocated(PlantReport)) RETURN ! this can happen early before plant is setup
    IF (PVT(PVTnum)%PVTModelType ==  SimplePVTmodel) THEN
      IF (QRadSWOutIncident(SurfNum) > MinIrradiance) THEN
        ! is heating wanted?

      !  IF (mdot > 0.0D0) THEN
      !  If (PlantReport(PlantLoopNum)%HeatingDemand > 0.0) THEN
          PVT(PVTnum)%HeatingUseful   = .TRUE.
!          PVT(PVTnum)%CoolingUseful   = .FALSE.
          PVT(PVTnum)%BypassDamperOff = .TRUE.
!        ELSE
!          PVT(PVTnum)%HeatingUseful   = .FALSE.
!          PVT(PVTnum)%CoolingUseful   = .TRUE.
!          PVT(PVTnum)%BypassDamperOff = .FALSE.
!        ENDIF

      ELSE
        ! is cooling wanted?
!        IF (mdot > 0.0D0) THEN
      !  If (PlantReport(PlantLoopNum)%CoolingDemand > 0.0) THEN
!          PVT(PVTnum)%CoolingUseful   = .TRUE.
!          PVT(PVTnum)%HeatingUseful   = .FALSE.
!          PVT(PVTnum)%BypassDamperOff = .TRUE.
!        ELSE
          PVT(PVTnum)%CoolingUseful   = .FALSE.
!          PVT(PVTnum)%HeatingUseful   = .TRUE.
          PVT(PVTnum)%BypassDamperOff = .FALSE.
!        ENDIF

      ENDIF
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE ControlPVTcollector

SUBROUTINE CalcPVTcollectors(PVTnum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate PVT collector thermal

          ! METHODOLOGY EMPLOYED:
          ! Current model is "simple" fixed efficiency and simple night sky balance for cooling

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance, ONLY: VerySmooth, QRadSWOutIncident
  USE Psychrometrics , ONLY: CPHW, PsyCpAirFnWTdb, PsyTwbFnTdbWPb, PsyTdpFnTdbTwbPb
  USE DataGlobals,     ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE DataLoopNode   , ONLY: Node
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataEnvironment, ONLY: OutDryBulbTemp, SkyTemp, OutBaroPress
  USE ConvectionCoefficients,  ONLY: InitExteriorConvectionCoeff

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: PVTnum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER   :: InletNode    = 0
  INTEGER   :: OutletNode   = 0
  REAL(r64) :: Eff          = 0.0D0
  INTEGER   :: SurfNum      = 0
  INTEGER   :: RoughSurf    = 0
  REAL(r64) :: HcExt        = 0.0D0
  REAL(r64) :: HrSky        = 0.0D0
  REAL(r64) :: HrGround     = 0.0D0
  REAL(r64) :: HrAir        = 0.0D0
  REAL(r64) :: Tcollector   = 0.0D0
  REAL(r64) :: mdot         = 0.0D0
  REAL(r64) :: Tinlet       = 0.0D0
  REAL(r64) :: Winlet       = 0.0D0
  REAL(r64) :: CpInlet      = 0.0D0
  REAL(r64) :: PotentialOutletTemp = 0.0D0
  REAL(r64) :: BypassFraction = 0.0D0
  REAL(r64) :: PotentialHeatGain = 0.0D0
  REAL(r64) :: WetBulbInlet = 0.0D0
  REAL(r64) :: DewPointInlet = 0.0D0

  ! flow
  SurfNum     = PVT(PVTnum)%SurfNum
  RoughSurf   = VerySmooth

  SELECT CASE (PVT(PVTnum)% WorkingFluidType)
  CASE (LiquidWorkingFluid)
    InletNode  = PVT(PVTnum)%PlantInletNodeNum
    OutletNode = PVT(PVTnum)%PlantOutletNodeNum
  CASE (AirWorkingFluid)
    InletNode  = PVT(PVTnum)%HVACInletNodeNum
    OutletNode = PVT(PVTnum)%HVACOutletNodeNum
  END SELECT

  mdot   = PVT(PVTnum)%MassFlowRate
  Tinlet = Node(InletNode)%Temp

  IF (PVT(PVTnum)%PVTModelType ==  SimplePVTmodel) THEN

    IF (PVT(PVTnum)%HeatingUseful .AND. PVT(PVTnum)%BypassDamperOff .AND. (mdot > 0.0D0)) THEN

      SELECT CASE (PVT(PVTnum)%Simple%ThermEfficMode)

      CASE (FixedThermEffic)
        Eff = PVT(PVTnum)%Simple%ThermEffic
      CASE (ScheduledThermEffic)
        Eff = GetCurrentScheduleValue(PVT(PVTnum)%Simple%ThermEffSchedNum)
        PVT(PVTnum)%Simple%ThermEffic = Eff

      END SELECT

      PotentialHeatGain  = QRadSWOutIncident(SurfNum) * Eff * PVT(PVTnum)%AreaCol

      IF ( PVT(PVTnum)%WorkingFluidType == AirWorkingFluid ) THEN
        Winlet = Node(InletNode)%HumRat
        CpInlet = PsyCpAirFnWTdb(Winlet,Tinlet, 'CalcPVTcollectors')
        IF (mdot*CpInlet > 0.0D0) THEN
          PotentialOutletTemp = Tinlet + PotentialHeatGain /(mdot * CpInlet)
        ELSE
          PotentialOutletTemp = Tinlet
        ENDIF
        !now compare heating potential to setpoint and figure bypass fraction
        If (PotentialOutletTemp > Node(PVT(PVTnum)%HVACOutletNodeNum)%TempSetPoint) Then ! need to modulate
          If (Tinlet /= PotentialOutletTemp) Then
            BypassFraction = (Node(PVT(PVTnum)%HVACOutletNodeNum)%TempSetPoint - PotentialOutletTemp)  &
                             /(Tinlet - PotentialOutletTemp)
          ELSE
            BypassFraction = 0.0D0
          ENDIF
          BypassFraction = MAX(0.0D0, BypassFraction)
          PotentialOutletTemp = Node(PVT(PVTnum)%HVACOutletNodeNum)%TempSetPoint
          PotentialHeatGain  = mdot * PsyCpAirFnWTdb(Winlet,Tinlet, 'CalcPVTcollectors') &
                              *(PotentialOutletTemp - Tinlet)

        ELSE
          BypassFraction = 0.0D0
        ENDIF
      ELSEIf ( PVT(PVTnum)%WorkingFluidType == LiquidWorkingFluid ) THEN
        CpInlet = CPHW(Tinlet)
        IF (mdot * CpInlet /= 0.0D0) THEN ! protect divide by zero
          PotentialOutletTemp = Tinlet + PotentialHeatGain/(mdot * CpInlet)
        ELSE
          PotentialOutletTemp = Tinlet
        ENDIF
        BypassFraction = 0.0D0

      ENDIF

      PVT(PVTnum)%Report%ThermEfficiency  = Eff
      PVT(PVTnum)%Report%ThermHeatGain    = PotentialHeatGain
      PVT(PVTnum)%Report%ThermPower       = PVT(PVTnum)%Report%ThermHeatGain
      PVT(PVTnum)%Report%ThermEnergy      = PVT(PVTnum)%Report%ThermPower * TimeStepSys * SecInHour
      PVT(PVTnum)%Report%ThermHeatLoss    = 0.0D0
      PVT(PVTnum)%Report%TinletWorkFluid  = Tinlet
      PVT(PVTnum)%Report%MdotWorkFluid    = mdot
      PVT(PVTnum)%Report%ToutletWorkFluid = PotentialOutletTemp
      PVT(PVTnum)%Report%BypassStatus     = BypassFraction

    ELSEIF (PVT(PVTnum)%CoolingUseful .AND. PVT(PVTnum)%BypassDamperOff .AND. (mdot > 0.0D0 ) ) THEN
         !calculate cooling using energy balance

      CALL InitExteriorConvectionCoeff(SurfNum,0.0D0,RoughSurf,PVT(PVTnum)%Simple%SurfEmissivity, &
            PVT(PVTnum)%Simple%LastCollectorTemp, &
            HcExt,HrSky,HrGround,HrAir)

      IF ( PVT(PVTnum)%WorkingFluidType == AirWorkingFluid ) THEN
        Winlet = Node(InletNode)%HumRat
        CpInlet = PsyCpAirFnWTdb(Winlet,Tinlet, 'CalcPVTcollectors')
        WetBulbInlet = PsyTwbFnTdbWPb(Tinlet, Winlet,  OutBaroPress, 'CalcPVTcollectors')
        DewPointInlet = PsyTdpFnTdbTwbPb(Tinlet, WetBulbInlet, OutBaroPress, 'CalcPVTcollectors')
      ELSEIf ( PVT(PVTnum)%WorkingFluidType == LiquidWorkingFluid ) THEN
        CpInlet = CPHW(Tinlet)
      ENDIF

      Tcollector = ( 2.0D0 * mdot * CpInlet * Tinlet                &
                 + PVT(PVTnum)%AreaCol *(                           &
                      HrGround * OutDryBulbTemp                     &
                      + HrSky * SkyTemp                             &
                      + HrAir * Surface(SurfNum)%OutDryBulbTemp     &
                      + HcExt * Surface(SurfNum)%OutDryBulbTemp) )  &
                 / (2.0D0 * mdot * CpInlet + PVT(PVTnum)%AreaCol *(HrGround + HrSky + HrAir + HcExt) )

      PotentialOutletTemp = 2.0D0 * Tcollector - Tinlet
      PVT(PVTnum)%Report%ToutletWorkFluid  =PotentialOutletTemp
      ! trap for air not being cooled below its wetbulb.
      IF ( PVT(PVTnum)%WorkingFluidType == AirWorkingFluid ) THEN
        IF (PotentialOutletTemp < DewPointInlet) THEN
        !  water removal would be needed.. not going to allow that for now.  limit cooling to dew point and model bypass
          IF (Tinlet /= PotentialOutletTemp) THEN
            BypassFraction = (DewPointInlet - PotentialOutletTemp)  &
                             /(Tinlet - PotentialOutletTemp)
          ELSE
            BypassFraction = 0.0D0
          ENDIF
          BypassFraction = MAX(0.0D0, BypassFraction)
          PotentialOutletTemp = DewPointInlet

        ENDIF
      ENDIF

      PVT(PVTnum)%Report%MdotWorkFluid     = mdot
      PVT(PVTnum)%Report%TinletWorkFluid   = Tinlet
      PVT(PVTnum)%Report%ToutletWorkFluid  = PotentialOutletTemp
      PVT(PVTnum)%Report%ThermHeatLoss     = mdot * CpInlet *(Tinlet - PVT(PVTnum)%Report%ToutletWorkFluid)
      PVT(PVTnum)%Report%ThermHeatGain     = 0.0D0
      PVT(PVTnum)%Report%ThermPower        = -1.0D0 * PVT(PVTnum)%Report%ThermHeatLoss
      PVT(PVTnum)%Report%ThermEnergy       = PVT(PVTnum)%Report%ThermPower * TimeStepSys * SecInHour
      PVT(PVTnum)%Report%ThermEfficiency   = 0.0D0
      PVT(PVTnum)%Simple%LastCollectorTemp = Tcollector
      PVT(PVTnum)%Report%BypassStatus      = 0.0D0

    ELSEIF (.NOT. PVT(PVTnum)%BypassDamperOff) THEN ! bypassed, zero things out

      PVT(PVTnum)%Report%TinletWorkFluid  = Tinlet
      PVT(PVTnum)%Report%ToutletWorkFluid = Tinlet
      PVT(PVTnum)%Report%ThermHeatLoss    = 0.0D0
      PVT(PVTnum)%Report%ThermHeatGain    = 0.0D0
      PVT(PVTnum)%Report%ThermPower       = 0.0D0
      PVT(PVTnum)%Report%ThermEfficiency  = 0.0D0
      PVT(PVTnum)%Report%ThermEnergy      = 0.0D0
      PVT(PVTnum)%Report%BypassStatus     = 1.0D0
      PVT(PVTnum)%Report%MdotWorkFluid    = mdot

    ELSE
      PVT(PVTnum)%Report%TinletWorkFluid  = Tinlet
      PVT(PVTnum)%Report%ToutletWorkFluid = Tinlet
      PVT(PVTnum)%Report%ThermHeatLoss    = 0.0D0
      PVT(PVTnum)%Report%ThermHeatGain    = 0.0D0
      PVT(PVTnum)%Report%ThermPower       = 0.0D0
      PVT(PVTnum)%Report%ThermEfficiency  = 0.0D0
      PVT(PVTnum)%Report%ThermEnergy      = 0.0D0
      PVT(PVTnum)%Report%BypassStatus     = 1.0D0
      PVT(PVTnum)%Report%MdotWorkFluid    = mdot
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CalcPVTcollectors

SUBROUTINE UpdatePVTcollectors(PVTnum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,   ONLY: Node
  USE Psychrometrics, ONLY: PsyHFnTdbW
  USE PlantUtilities, ONLY: SafeCopyPlantNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: PVTNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode

  SELECT CASE (PVT(PVTnum)% WorkingFluidType)
  CASE (LiquidWorkingFluid)
    InletNode  = PVT(PVTnum)%PlantInletNodeNum
    OutletNode = PVT(PVTnum)%PlantOutletNodeNum

    CALL SafeCopyPlantNode(InletNode, OutletNode)
    Node(OutletNode)%Temp = PVT(PVTnum)%Report%ToutletWorkFluid

  CASE (AirWorkingFluid)
    InletNode  = PVT(PVTnum)%HVACInletNodeNum
    OutletNode = PVT(PVTnum)%HVACOutletNodeNum

    ! Set the outlet nodes for properties that just pass through & not used
    Node(OutletNode)%Quality              = Node(InletNode)%Quality
    Node(OutletNode)%Press                = Node(InletNode)%Press
    Node(OutletNode)%MassFlowRate         = Node(InletNode)%MassFlowRate
    Node(OutletNode)%MassFlowRateMin      = Node(InletNode)%MassFlowRateMin
    Node(OutletNode)%MassFlowRateMax      = Node(InletNode)%MassFlowRateMax
    Node(OutletNode)%MassFlowRateMinAvail = Node(InletNode)%MassFlowRateMinAvail
    Node(OutletNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRateMaxAvail

    ! Set outlet node variables that are possibly changed
    Node(OutletNode)%Temp = PVT(PVTnum)%Report%ToutletWorkFluid
    Node(OutletNode)%HumRat = Node(InletNode)%HumRat ! assumes dewpoint bound on cooling ....
    Node(OutletNode)%Enthalpy = PsyHFnTdbW( PVT(PVTnum)%Report%ToutletWorkFluid  , &
                                            Node(OutletNode)%HumRat )
  END SELECT

  RETURN

END SUBROUTINE UpdatePVTcollectors

SUBROUTINE GetPVTThermalPowerProduction(PVindex, ThermalPower, ThermalEnergy)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

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
  INTEGER, INTENT(IN)  :: PVindex ! index of PV generator (not PVT collector)
  REAL(r64),  INTENT(OUT) :: ThermalPower
  REAL(r64),  INTENT(OUT) :: ThermalEnergy
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PVTnum = 0
  INTEGER :: Loop = 0

  ! first find PVT index that is associated with this PV generator
  DO loop = 1, NumPVT
    IF (.not. PVT(loop)%PVfound) CYCLE
    IF (PVT(loop)%PVnum == PVindex) THEN ! we found it
      PVTnum = loop
    ENDIF
  ENDDO

  IF (PVTnum > 0) THEN
    ThermalPower  = PVT(PVTnum)%report%ThermPower
    ThermalEnergy = PVT(PVTnum)%report%ThermEnergy
  ELSE
    ThermalPower  = 0.0D0
    ThermalEnergy = 0.0D0
  ENDIF

  RETURN

END SUBROUTINE GetPVTThermalPowerProduction

!=====================  Utility/Other routines for module.
! Insert as appropriate

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

END MODULE PhotovoltaicThermalCollectors
