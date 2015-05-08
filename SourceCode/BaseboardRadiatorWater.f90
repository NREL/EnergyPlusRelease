! Module HWBaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Water)
MODULE HWBaseboardRadiator
  ! Module containing the routines dealing with the hot water baseboard heaters

  ! MODULE INFORMATION:
  !       AUTHOR         Daeho Kang
  !       DATE WRITTEN   Aug 2007
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate hot water baseboard heaters.

  ! METHODOLOGY EMPLOYED:

  ! REFERENCES:
  ! 1. I=B=R Ratings for Baseboards, Baseboard Radiation,
  !   Finned Tube (Commercial) Radiation, and Indirect Fired Water Heaters, January 2007 Edition
  ! 2. Incropera and DeWitt, Fundamentals of Heat and Mass Transfer, Chapter 11.3 and 11.4,
  !   eq. 11.15, 11.17, and 11.33

  ! OTHER NOTES:
  ! na

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataPlant       ,   ONLY: PlantLoop, TypeOf_Baseboard_Rad_Conv_Water
USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig
USE DataHVACGlobals,    ONLY: SmallLoad, TimeStepSys, SysTimeElapsed
USE DataInterfaces
  ! Use statements for access to subroutines in other modules
USE Psychrometrics,     ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW
USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
USE ReportSizingManager, ONLY: ReportSizingOutput

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS

CHARACTER(len=*), PARAMETER :: cCMO_BBRadiator_Water='ZoneHVAC:Baseboard:RadiantConvective:Water'

  ! DERIVED TYPE DEFINITIONS
  TYPE HWBaseboardParams
    CHARACTER(len=MaxNameLength) :: EquipID   =' '
    INTEGER :: EquipType =0
    CHARACTER(len=MaxNameLength) :: Schedule  =' '
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfaceName
    INTEGER, ALLOCATABLE, DIMENSION(:) :: SurfacePtr
    INTEGER :: ZonePtr                  = 0
    INTEGER :: SchedPtr                 = 0
    INTEGER :: WaterInletNode           = 0
    INTEGER :: WaterOutletNode          = 0
    INTEGER :: TotSurfToDistrib         = 0
    INTEGER :: ControlCompTypeNum       = 0
    INTEGER :: CompErrIndex             = 0
    REAL(r64) :: AirMassFlowRate        =0.0d0
    REAL(r64) :: AirMassFlowRateStd     =0.0d0
    REAL(r64) :: WaterTempAvg           =0.0d0
    REAL(r64) :: RatedCapacity          =0.0d0
    REAL(r64) :: UA                     =0.0d0
    REAL(r64) :: Offset                 =0.0d0
    REAL(r64) :: WaterMassFlowRate      =0.0d0
    REAL(r64) :: WaterMassFlowRateMax   =0.0d0
    REAL(r64) :: WaterMassFlowRateStd   =0.0d0
    REAL(r64) :: WaterVolFlowRateMax    =0.0d0
    REAL(r64) :: WaterInletTempStd      =0.0d0
    REAL(r64) :: WaterInletTemp         =0.0d0
    REAL(r64) :: WaterInletEnthalpy     =0.0d0
    REAL(r64) :: WaterOutletTempStd     =0.0d0
    REAL(r64) :: WaterOutletTemp        =0.0d0
    REAL(r64) :: WaterOutletEnthalpy    =0.0d0
    REAL(r64) :: AirInletTempStd        =0.0d0
    REAL(r64) :: AirInletTemp           =0.0d0
    REAL(r64) :: AirOutletTemp          =0.0d0
    REAL(r64) :: AirInletHumRat         =0.0d0
    REAL(r64) :: AirOutletTempStd       =0.0d0
    REAL(r64) :: FracRadiant            =0.0d0
    REAL(r64) :: FracConvect            =0.0d0
    REAL(r64) :: FracDistribPerson      =0.0d0
    REAL(r64),   ALLOCATABLE, DIMENSION(:) :: FracDistribToSurf
    REAL(r64) :: TotPower               =0.0d0
    REAL(r64) :: Power                  =0.0d0
    REAL(r64) :: ConvPower              =0.0d0
    REAL(r64) :: RadPower               =0.0d0
    REAL(r64) :: TotEnergy              =0.0d0
    REAL(r64) :: Energy                 =0.0d0
    REAL(r64) :: ConvEnergy             =0.0d0
    REAL(r64) :: RadEnergy              =0.0d0
    INTEGER   :: LoopNum                =0  ! plant loop index
    INTEGER   :: LoopSideNum            =0  ! plant loop side index
    INTEGER   :: BranchNum              =0  ! plant loop branch index
    INTEGER   :: CompNum                =0  ! plant loop component index
    INTEGER   :: BBLoadReSimIndex       =0  !
    INTEGER   :: BBMassFlowReSimIndex   =0
    INTEGER   :: BBInletTempFlowReSimIndex =0

  END TYPE HWBaseboardParams

  !MODULE VARIABLE DECLARATIONS:
  TYPE (HWBaseboardParams), ALLOCATABLE, DIMENSION(:) :: HWBaseboard
  INTEGER :: NumHWBaseboards = 0
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: QBBRadSource ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: QBBRadSrcAvg ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZeroSourceSumHATsurf ! Equal to the SumHATsurf for all the walls in a zone with no source

  ! Record keeping variables used to calculate QBBRadSrcAvg locally
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastQBBRadSrc        ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastSysTimeElapsed   ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastTimeStepSys      ! Need to keep the last value in case we are still iterating
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: CheckEquipName
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag       ! get loop number flag

  !SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

  PUBLIC  :: SimHWBaseboard
  PRIVATE :: GetHWBaseboardInput
  PRIVATE :: InitHWBaseboard
  PRIVATE :: SizeHWBaseboard
  PUBLIC  :: CalcHWBaseboard
  PUBLIC  :: UpdateBBRadSourceValAvg
  PRIVATE :: UpdateHWBaseboard
  PRIVATE :: DistributeBBRadGains
  PRIVATE :: ReportHWBaseboard
  PRIVATE :: SumHATsurf
  PUBLIC  :: UpdateHWBaseboardPlantConnection

CONTAINS

  SUBROUTINE SimHWBaseboard(EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration,  &
                            PowerMet, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the Baseboard Radiators.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode,          ONLY: Node
    USE InputProcessor,        ONLY: FindItemInList,MakeUPPERCase
    USE General,               ONLY: TrimSigDigits
    USE ScheduleManager,       ONLY: GetCurrentScheduleValue
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
    USE DataInterfaces,        ONLY: ControlCompOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: EquipName
    INTEGER, INTENT(IN)          :: ActualZoneNum
    INTEGER, INTENT(IN)          :: ControlledZoneNum
    INTEGER, INTENT(INOUT)       :: CompIndex
    LOGICAL, INTENT(IN)          :: FirstHVACIteration
    REAL(r64),    INTENT(OUT)    :: PowerMet

          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: MaxIter = 30

          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER      :: BaseboardNum           ! Index of unit in baseboard array
    LOGICAL,SAVE :: GetInputFlag = .TRUE.  ! One time get input flag
    REAL(r64)    :: QZnReq                 ! Zone load not yet satisfied
    REAL(r64)    :: MaxWaterFlow
    REAL(r64)    :: MinWaterFlow

    IF (GetInputFlag) THEN
      CALL GetHWBaseboardInput
      GetInputFlag=.false.
    END IF

       ! Find the correct Baseboard Equipment
    IF (CompIndex == 0) THEN
      BaseboardNum = FindItemInList(EquipName, HWBaseboard%EquipID, NumHWBaseboards)
      IF (BaseboardNum == 0) THEN
        CALL ShowFatalError('SimHWBaseboard: Unit not found='//TRIM(EquipName))
      ENDIF
      CompIndex = BaseboardNum
    ELSE
      BaseboardNum = CompIndex
      IF (BaseboardNum > NumHWBaseboards .or. BaseboardNum < 1) THEN
        CALL ShowFatalError('SimHWBaseboard:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumHWBaseboards))//  &
                            ', Entered Unit name='//TRIM(EquipName))
      ENDIF
      IF (CheckEquipName(BaseboardNum)) THEN
        IF (EquipName /= HWBaseboard(BaseboardNum)%EquipID) THEN
          CALL ShowFatalError('SimHWBaseboard: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(BaseboardNum))// &
                              ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                              TRIM(HWBaseboard(BaseboardNum)%EquipID))
        ENDIF
        CheckEquipName(BaseboardNum)=.false.
      ENDIF
    ENDIF

    IF (CompIndex > 0) THEN

      CALL InitHWBaseboard (BaseboardNum, ControlledZoneNum, FirstHVACIteration)

      QZnReq = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToHeatSP

         ! On the first HVAC iteration the system values are given to the controller, but after that
         ! the demand limits are in place and there needs to be feedback to the Zone Equipment
      If(FirstHVACIteration)Then
         MaxWaterFlow = HWBaseboard(BaseboardNum)%WaterMassFlowRateMax
         MinWaterFlow = 0.0d0
      Else
         MaxWaterFlow = Node(HWBaseboard(BaseboardNum)%WaterInletNode)%MassFlowRateMaxAvail
         MinWaterFlow = Node(HWBaseboard(BaseboardNum)%WaterInletNode)%MassFlowRateMinAvail
      ENDIF

      SELECT CASE (HWBaseboard(BaseboardNum)%EquipType)

        CASE (TypeOf_Baseboard_Rad_Conv_Water)  ! 'ZoneHVAC:Baseboard:RadiantConvective:Water'
          CALL ControlCompOutput(CompName=HWBaseBoard(BaseboardNum)%EquipID,CompType=cCMO_BBRadiator_Water,  &
                                 CompNum=BaseboardNum, &
                                 FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                                 ActuatedNode=HWBaseboard(BaseboardNum)%WaterInletNode, &
                                 MaxFlow=MaxWaterFlow,MinFlow=MinWaterFlow, &
                                 ControlOffset=HWBaseboard(BaseboardNum)%Offset, &
                                 ControlCompTypeNum=HWBaseboard(BaseboardNum)%ControlCompTypeNum, &
                                 CompErrIndex=HWBaseboard(BaseboardNum)%CompErrIndex, &
                                 LoopNum = HWBaseboard(BaseboardNum)%LoopNum, &
                                 LoopSide = HWBaseboard(BaseboardNum)%LoopSideNum, &
                                 BranchIndex = HWBaseboard(BaseboardNum)%BranchNum)
        CASE DEFAULT
          CALL ShowSevereError('SimBaseboard: Errors in Baseboard='//TRIM(HWBaseboard(BaseboardNum)%EquipID))
          CALL ShowContinueError('Invalid or unimplemented equipment type='//  &
              TRIM(TrimSigDigits(HWBaseboard(BaseboardNum)%EquipType)))
          CALL ShowFatalError('Preceding condition causes termination.')

      END SELECT

      PowerMet = HWBaseboard(BaseboardNum)%TotPower

      CALL UpdateHWBaseboard(BaseboardNum)

      CALL ReportHWBaseboard(BaseboardNum)

    ELSE
        CALL ShowFatalError('SimHWBaseboard: Unit not found='//TRIM(EquipName))
    ENDIF

  END SUBROUTINE SimHWBaseboard

  SUBROUTINE GetHWBaseboardInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Aug 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! Standard input processor calls.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode, ONLY: Node, NodeType_Water, NodeConnectionType_Inlet, NodeConnectionType_Outlet, ObjectIsNotParent
!unused0909    USE DataGlobals,           ONLY: NumOfZones
    USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, SameString, VerifyName
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet
    USE DataSurfaces,          ONLY: Surface, TotSurfaces
    USE ScheduleManager,       ONLY: GetScheduleIndex, GetCurrentScheduleValue
    USE GlobalNames,           ONLY: VerifyUniqueBaseboardName
    USE General,               ONLY: RoundSigDigits
    USE DataSizing,            ONLY: Autosize, FinalZoneSizing
    USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='GetHWBaseboardInput:'
    REAL(r64), PARAMETER :: MaxFraction           = 1.0d0
    REAL(r64), PARAMETER :: MinFraction           = 0.0d0
    REAL(r64), PARAMETER :: MaxWaterTempAvg       = 150.0d0    ! Maximum limit of average water temperature in degree C
    REAL(r64), PARAMETER :: MinWaterTempAvg       = 20.0d0     ! Minimum limit of average water temperature in degree C
    REAL(r64), PARAMETER :: HighWaterMassFlowRate = 10.0d0     ! Maximum limit of water mass flow rate in kg/s
    REAL(r64), PARAMETER :: LowWaterMassFlowRate  = 0.00001d0  ! Minimum limit of water mass flow rate in kg/s
    REAL(r64), PARAMETER :: MaxWaterFlowRate      = 10.0d0     ! Maximum limit of water volume flow rate in m3/s
    REAL(r64), PARAMETER :: MinWaterFlowRate      = 0.00001d0  ! Minimum limit of water volume flow rate in m3/s
    REAL(r64), PARAMETER :: WaterTempAvgDefault   = 87.78d0    ! Default water mass flow rate in kg/s
    REAL(r64), PARAMETER :: WaterMassFlowDefault  = 0.063d0    ! Default water mass flow rate in kg/s
    REAL(r64), PARAMETER :: AirInletTempStd       = 18.0d0     ! Standard air inlet temperature in degree C
    REAL(r64), PARAMETER :: CPAirStd              = 1005.0d0   ! Average specific heat of air at between 25C and 40C in J/kg-k
!    INTEGER, PARAMETER   :: MaxDistribSurfaces    = 20         ! Maximum number of surfaces that a baseboard heater can radiate to
    INTEGER, PARAMETER   :: MinDistribSurfaces    = 1          ! Minimum number of surfaces that a baseboard heater can radiate to

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AllFracsSummed        ! Sum of the fractions radiant
    INTEGER   :: BaseboardNum          ! Baseboard number
    INTEGER   :: NumAlphas             ! Number of Alphas for each GetobjectItem call
    INTEGER   :: NumNumbers            ! Number of Numbers for each GetobjectItem call
    INTEGER   :: SurfNum               ! Surface number Do loop counter
    INTEGER   :: IOSTAT
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK                 ! Flag to verify name
    LOGICAL :: IsBlank                 ! Flag for blank name
    LOGICAL :: errflag

    NumHWBaseboards = GetNumObjectsFound(cCMO_BBRadiator_Water)

    ! Count total number of baseboard units

    ALLOCATE(HWBaseboard(NumHWBaseboards))
    ALLOCATE(CheckEquipName(NumHWBaseboards))
    CheckEquipName=.true.

       ! Get the data from the user input related to baseboard heaters
    DO BaseboardNum = 1, NumHWBaseboards

      CALL GetObjectItem(cCMO_BBRadiator_Water,BaseboardNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNumbers,IOSTAT,  &
                       AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                       AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),HWBaseboard%EquipID,BaseboardNum,IsNotOK,IsBlank,cCMO_BBRadiator_Water//' Name')

      IF (IsNotOK) THEN
         ErrorsFound=.true.
      ENDIF
      CALL VerifyUniqueBaseboardName(cCMO_BBRadiator_Water,cAlphaArgs(1),errflag,cCMO_BBRadiator_Water//' Name')
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF

      HWBaseboard(BaseboardNum)%EquipID   = cAlphaArgs(1)               ! Name of this baseboard
      HWBaseboard(BaseboardNum)%EquipType = TypeOf_Baseboard_Rad_Conv_Water !'ZoneHVAC:Baseboard:RadiantConvective:Water'

       ! Get schedule
      HWBaseboard(BaseboardNum)%Schedule  = cAlphaArgs(2)
      IF (lAlphaFieldBlanks(2)) THEN
        HWBaseboard(BaseboardNum)%SchedPtr  = ScheduleAlwaysOn
      ELSE
        HWBaseboard(BaseboardNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
        IF (HWBaseboard(BaseboardNum)%SchedPtr == 0) THEN
          CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
           '", '//TRIM(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
          ErrorsFound=.true.
        END IF
      ENDIF

         ! Get inlet node number
      HWBaseboard(BaseboardNum)%WaterInletNode = &
      GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,cCMO_BBRadiator_Water,cAlphaArgs(1), &
      NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)

         ! Get outlet node number
      HWBaseboard(BaseboardNum)%WaterOutletNode = &
      GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,cCMO_BBRadiator_Water,cAlphaArgs(1), &
      NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      CALL TestCompSet(cCMO_BBRadiator_Water,cAlphaArgs(1),cAlphaArgs(3), &
                       cAlphaArgs(4),'Hot Water Nodes')

      HWBaseboard(BaseboardNum)%WaterTempAvg = rNumericArgs(1)
      IF (HWBaseboard(BaseboardNum)%WaterTempAvg > MaxWaterTempAvg + 0.001d0) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(1))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxWaterTempAvg,2))//'].')
        HWBaseboard(BaseboardNum)%WaterTempAvg = MaxWaterTempAvg
      ELSE IF (HWBaseboard(BaseboardNum)%WaterTempAvg < MinWaterTempAvg - 0.001d0) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(1))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinWaterTempAvg,2))//'].')
        HWBaseboard(BaseboardNum)%WaterTempAvg = MinWaterTempAvg
      END IF

      HWBaseboard(BaseboardNum)%WaterMassFlowRateStd = rNumericArgs(2)
      IF (HWBaseboard(BaseboardNum)%WaterMassFlowRateStd < LowWaterMassFlowRate-0.0001d0 .OR. &
          HWBaseboard(BaseboardNum)%WaterMassFlowRateStd > HighWaterMassFlowRate+0.0001d0 ) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(2))//' is an invalid Standard Water mass flow rate.')
        CALL ShowContinueError('...reset to a default value=['//trim(RoundSigDigits(WaterMassFlowDefault,1))//'].')
        HWBaseboard(BaseboardNum)%WaterMassFlowRateStd = WaterMassFlowDefault
      END IF

      HWBaseboard(BaseboardNum)%RatedCapacity = rNumericArgs(3)

      HWBaseboard(BaseboardNum)%WaterVolFlowRateMax = rNumericArgs(4)
      IF (ABS(HWBaseboard(BaseboardNum)%WaterVolFlowRateMax) <= MinWaterFlowRate) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(4))//' was less than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinWaterFlowRate,2))//'].')
         HWBaseboard(BaseboardNum)%WaterVolFlowRateMax = MinWaterFlowRate
      ELSE IF(HWBaseboard(BaseboardNum)%WaterVolFlowRateMax > MaxWaterFlowRate) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(4))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxWaterFlowRate,2))//'].')
        HWBaseboard(BaseboardNum)%WaterVolFlowRateMax = MaxWaterFlowRate
      END IF

      HWBaseboard(BaseboardNum)%Offset = rNumericArgs(5)
         ! Set default convergence tolerance
      IF (HWBaseboard(BaseboardNum)%Offset .LE. 0.0d0) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(5))//' was less than the allowable minimum.')
        CALL ShowContinueError('...reset to a default value=['//trim(RoundSigDigits(MaxFraction,2))//'].')
        HWBaseboard(BaseboardNum)%Offset = 0.001d0
      END IF

      HWBaseboard(BaseboardNum)%FracRadiant = rNumericArgs(6)
      IF (HWBaseboard(BaseboardNum)%FracRadiant < MinFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(6))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinFraction,2))//'].')
        HWBaseboard(BaseboardNum)%FracRadiant = MinFraction
      END IF
      IF (HWBaseboard(BaseboardNum)%FracRadiant > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(6))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,2))//'].')
        HWBaseboard(BaseboardNum)%FracRadiant = MaxFraction
      END IF

         ! Remaining fraction is added to the zone as convective heat transfer
      AllFracsSummed = HWBaseboard(BaseboardNum)%FracRadiant
      IF (AllFracsSummed > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", Fraction Radiant was higher than the allowable maximum.')
        HWBaseboard(BaseboardNum)%FracRadiant = MaxFraction
        HWBaseboard(BaseboardNum)%FracConvect = 0.0d0
      ELSE
        HWBaseboard(BaseboardNum)%FracConvect = 1.0d0 - AllFracsSummed
      END IF

      HWBaseboard(BaseboardNum)%FracDistribPerson = rNumericArgs(7)
      IF (HWBaseboard(BaseboardNum)%FracDistribPerson < MinFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(7))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinFraction,3))//'].')
        HWBaseboard(BaseboardNum)%FracDistribPerson = MinFraction
      END IF
      IF (HWBaseboard(BaseboardNum)%FracDistribPerson > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(7))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,3))//'].')
        HWBaseboard(BaseboardNum)%FracDistribPerson = MaxFraction
      END IF

      HWBaseboard(BaseboardNum)%TotSurfToDistrib = NumNumbers - 7
!      IF (HWBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
!        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
!          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
!        CALL ShowContinueError('...only the maximum value=['//trim(RoundSigDigits(MaxDistribSurfaces))// &
!           '] will be processed.')
!        HWBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
!      END IF
      IF ( (HWBaseboard(BaseboardNum)%TotSurfToDistrib < MinDistribSurfaces) .AND. &
           (HWBaseboard(BaseboardNum)%FracRadiant > MinFraction) ) THEN
        CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", the number of surface/radiant fraction groups entered was less than the allowable minimum.')
        CALL ShowContinueError('...the minimum that must be entered=['//trim(RoundSigDigits(MinDistribSurfaces))//'].')
        ErrorsFound = .true.
        HWBaseboard(BaseboardNum)%TotSurfToDistrib = 0  ! error
      END IF

      ALLOCATE(HWBaseboard(BaseboardNum)%SurfaceName(HWBaseboard(BaseboardNum)%TotSurfToDistrib))
      HWBaseboard(BaseboardNum)%SurfaceName=' '
      ALLOCATE(HWBaseboard(BaseboardNum)%SurfacePtr(HWBaseboard(BaseboardNum)%TotSurfToDistrib))
      HWBaseboard(BaseboardNum)%SurfacePtr=0
      ALLOCATE(HWBaseboard(BaseboardNum)%FracDistribToSurf(HWBaseboard(BaseboardNum)%TotSurfToDistrib))
      HWBaseboard(BaseboardNum)%FracDistribToSurf=0.0d0

      AllFracsSummed = HWBaseboard(BaseboardNum)%FracDistribPerson
      Do SurfNum = 1,HWBaseboard(BaseboardNum)%TotSurfToDistrib
        HWBaseboard(BaseboardNum)%SurfaceName(SurfNum) = cAlphaArgs(SurfNum + 4)
        HWBaseboard(BaseboardNum)%SurfacePtr(SurfNum)  = FindItemInList(cAlphaArgs(SurfNum + 4),Surface%Name,TotSurfaces)
        HWBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) = rNumericArgs(SurfNum + 7)
        IF (HWBaseboard(BaseboardNum)%SurfacePtr(SurfNum) == 0 ) THEN
          CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cAlphaFieldNames(SurfNum+4))//'="'//trim(cAlphaArgs(SurfNum + 4))//'" invalid - not found.')
          ErrorsFound = .true.
        END IF
        IF (HWBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) > MaxFraction) THEN
          CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cNumericFieldNames(SurfNum+7))//'was greater than the allowable maximum.')
          CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,2))//'].')
          HWBaseboard(BaseboardNum)%TotSurfToDistrib = MaxFraction
        END IF
        IF (HWBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) < MinFraction) THEN
          CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cNumericFieldNames(SurfNum+7))//'was less than the allowable minimum.')
          CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MinFraction,2))//'].')
          HWBaseboard(BaseboardNum)%TotSurfToDistrib = MinFraction
        END IF
        IF (HWBaseboard(BaseboardNum)%SurfacePtr(SurfNum) /= 0 ) THEN
          Surface( HWBaseboard(BaseboardNum)%SurfacePtr(SurfNum) )%IntConvSurfGetsRadiantHeat = .TRUE.
        ENDIF

        AllFracsSummed = AllFracsSummed + HWBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum)
      End DO ! Surfaces

      IF (AllFracsSummed > (MaxFraction + 0.01d0) ) THEN
        CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", Summed radiant fractions for people + surface groups > 1.0')
        ErrorsFound = .TRUE.
      END IF
      IF ( (AllFracsSummed < (MaxFraction - 0.01d0)) .AND. &            ! User didn't distribute all of the
           (HWBaseboard(BaseboardNum)%FracRadiant > MinFraction) ) THEN ! radiation warn that some will be lost
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Water//'="'//trim(cAlphaArgs(1))// &
          '", Summed radiant fractions for people + surface groups < 1.0')
        CALL ShowContinueError('The rest of the radiant energy delivered by the baseboard heater will be lost')
      END IF
    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//cCMO_BBRadiator_Water//'Errors found getting input. Program terminates.')
    END IF

       ! Setup Report variables for the Coils
    DO BaseboardNum = 1, NumHWBaseboards
      ! CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Water'
      CALL SetupOutputVariable('Baseboard Total Heating Rate [W]', HWBaseboard(BaseboardNum)%TotPower, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Convective Heating Rate [W]', HWBaseboard(BaseboardNum)%ConvPower, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Radiant Heating Rate [W]', HWBaseboard(BaseboardNum)%RadPower, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Total Heating Energy [J]', HWBaseboard(BaseboardNum)%TotEnergy, &
                               'System','Sum',HWBaseboard(BaseboardNum)%EquipID, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BASEBOARD',GroupKey='System')

      CALL SetupOutputVariable('Baseboard Convective Heating Energy [J]', HWBaseboard(BaseboardNum)%ConvEnergy, &
                               'System','Sum',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Radiant Heating Energy [J]', HWBaseboard(BaseboardNum)%RadEnergy, &
                               'System','Sum',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Hot Water Energy [J]', HWBaseboard(BaseboardNum)%Energy, &
                               'System','Sum',HWBaseboard(BaseboardNum)%EquipID, &
                                ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='BASEBOARD',GroupKey='System')
      CALL SetupOutputVariable('Baseboard Hot Water Mass Flow Rate [kg/s]', HWBaseboard(BaseboardNum)%WaterMassFlowRate, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Air Mass Flow Rate [kg/s]', HWBaseboard(BaseboardNum)%AirMassFlowRate, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Air Inlet Temperature [C]', HWBaseboard(BaseboardNum)%AirInletTemp, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Air Outlet Temperature [C]', HWBaseboard(BaseboardNum)%AirOutletTemp, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Water Inlet Temperature [C]', HWBaseboard(BaseboardNum)%WaterInletTemp, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Water Outlet Temperature [C]', HWBaseboard(BaseboardNum)%WaterOutletTemp, &
                               'System','Average',HWBaseboard(BaseboardNum)%EquipID)
    END DO

    RETURN

  END SUBROUTINE GetHWBaseboardInput

SUBROUTINE InitHWBaseboard(BaseboardNum, ControlledZoneNumSub, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !                      Rick Strand
          !       DATE WRITTEN   Nov 1997
          !                      Feb 2001
          !       MODIFIED       Aug 2007 Daeho Kang (Add radiant component)
          !                      Sept 2010 Brent Griffith (plant interactions)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the baseboard units, and determines the UA values during simulation.

          ! METHODOLOGY EMPLOYED:
          ! The initialization subrotines both in high temperature radiant radiator
          ! and convective only baseboard radiator are combined and modified. In addition,
          ! an UA value calculation by LMTD method is added.
          ! The heater is assumed to be crossflow with both fluids unmixed.

          ! REFERENCES:
          ! 1. Incropera and DeWitt, Fundamentals of Heat and Mass Transfer
          ! Chapter 11.3, p. 510, eq. 11.15 and 11.17
          ! 2. I=B=R Ratings for Baseboards, Baseboard Radiation, Finned Tube (Commercial) Radiation,
          ! and Indirect Fired Water Heaters, January 2007 Edition

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY: BeginEnvrnFlag, NumofZones
  USE DataLoopNode,      ONLY: Node
  USE DataEnvironment,   ONLY: StdRhoAir
  USE PlantUtilities,    ONLY: InitComponentNodes
  USE DataPlant,         ONLY: ScanPlantLoopsForObject

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
  INTEGER, INTENT(IN)    :: BaseboardNum
  INTEGER, INTENT(IN)    :: ControlledZoneNumSub

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Constant         = 0.0062d0     ! Constant of linear equation for air mass flow rate
  REAL(r64), PARAMETER :: Coeff            = 0.0000275d0  ! Correlation coefficient to capacity

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: MyOneTimeFlag = .true.
  LOGICAL, SAVE :: ZoneEquipmentListChecked = .false.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  INTEGER    :: Loop
  INTEGER    :: WaterInletNode
  INTEGER    :: ZoneNode
  INTEGER    :: ZoneNum
  REAL(r64)  :: RhoAirStdInit
  REAL(r64)  :: rho !local fluid density
  REAL(r64)  :: Cp  ! local fluid specific heat
  LOGICAL    :: errFlag


       ! Do the one time initializations
    IF (MyOneTimeFlag) THEN

       ! Initialize the environment and sizing flags
      ALLOCATE(MyEnvrnFlag(NumHWBaseboards))
      ALLOCATE(MySizeFlag(NumHWBaseboards))
      ALLOCATE(ZeroSourceSumHATsurf(NumofZones))
               ZeroSourceSumHATsurf = 0.0D0
      ALLOCATE(QBBRadSource(NumHWBaseboards))
               QBBRadSource = 0.0D0
      ALLOCATE(QBBRadSrcAvg(NumHWBaseboards))
               QBBRadSrcAvg = 0.0D0
      ALLOCATE(LastQBBRadSrc(NumHWBaseboards))
               LastQBBRadSrc = 0.0D0
      ALLOCATE(LastSysTimeElapsed(NumHWBaseboards))
               LastSysTimeElapsed = 0.0D0
      ALLOCATE(LastTimeStepSys(NumHWBaseboards))
               LastTimeStepSys = 0.0D0
      ALLOCATE(SetLoopIndexFlag(NumHWBaseboards))
      MyEnvrnFlag = .TRUE.
      MySizeFlag  = .TRUE.
      MyOneTimeFlag = .false.
      SetLoopIndexFlag = .TRUE.
      DO Loop = 1, NumHWBaseboards
       ! Air mass flow rate is obtained from the following linear equation (reset if autosize is used)
       ! m_dot = 0.0062 + 2.75e-05*q
        HWBaseboard(Loop)%AirMassFlowRateStd = Constant + Coeff * HWBaseboard(Loop)%RatedCapacity
      END DO
    ENDIF

    IF (HWBaseboard(BaseboardNum)%ZonePtr <= 0) &
      HWBaseboard(BaseboardNum)%ZonePtr = ZoneEquipConfig(ControlledZoneNumSub)%ActualZoneNum


       ! Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
    IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
      ZoneEquipmentListChecked=.true.
      DO Loop=1,NumHWBaseboards
        IF (CheckZoneEquipmentList(cCMO_BBRadiator_Water,HWBaseboard(Loop)%EquipID)) CYCLE
        CALL ShowSevereError('InitBaseboard: Unit=['//TRIM(cCMO_BBRadiator_Water)//','//  &
                TRIM(HWBaseboard(Loop)%EquipID)//  &
                '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
      END DO
    ENDIF


    IF(SetLoopIndexFlag(BaseboardNum))THEN
      IF(ALLOCATED(PlantLoop))THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(HWBaseboard(BaseboardNum)%EquipID,     &
                                     HWBaseboard(BaseboardNum)%EquipType,   &
                                     HWBaseboard(BaseboardNum)%LoopNum,     &
                                     HWBaseboard(BaseboardNum)%LoopSideNum, &
                                     HWBaseboard(BaseboardNum)%BranchNum,   &
                                     HWBaseboard(BaseboardNum)%CompNum,     &
                                     errFlag=errFlag)
        IF (errFlag) THEN
          CALL ShowFatalError('InitHWBaseboard: Program terminated for previous conditions.')
        ENDIF
        SetLoopIndexFlag(BaseboardNum) = .FALSE.
      ENDIF
    ENDIF

    IF (.NOT. SysSizingCalc .AND. MySizeFlag(BaseboardNum) .AND. .NOT. SetLoopIndexFlag(BaseboardNum)) THEN
       ! For each coil, do the sizing once
      CALL SizeHWBaseboard(BaseboardNum)
      MySizeFlag(BaseboardNum) = .FALSE.
    END IF

       ! Do the Begin Environment initializations
    IF (BeginEnvrnFlag .and. MyEnvrnFlag(BaseboardNum)) THEN
          ! Initialize
      RhoAirStdInit = StdRhoAir
      WaterInletNode = HWBaseboard(BaseboardNum)%WaterInletNode

      rho = GetDensityGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex,&
                                  'BaseboardRadiatorWater:InitHWBaseboard')

      HWBaseboard(BaseboardNum)%WaterMassFlowRateMax =  rho * HWBaseboard(BaseboardNum)%WaterVolFlowRateMax

      CALL InitComponentNodes(0.d0,HWBaseboard(BaseboardNum)%WaterMassFlowRateMax, &
                                   HWBaseboard(BaseboardNum)%WaterInletNode,       &
                                   HWBaseboard(BaseboardNum)%WaterOutletNode,       &
                                   HWBaseboard(BaseboardNum)%LoopNum,              &
                                   HWBaseboard(BaseboardNum)%LoopSideNum,          &
                                   HWBaseboard(BaseboardNum)%BranchNum,            &
                                   HWBaseboard(BaseboardNum)%CompNum)

      Node(WaterInletNode)%Temp                      = 60.0d0

      Cp =  GetSpecificHeatGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                 Node(WaterInletNode)%Temp,                      &
                                 PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                                 'BaseboardRadiatorWater:InitHWBaseboard')

      Node(WaterInletNode)%Enthalpy                  = Cp * Node(WaterInletNode)%Temp
      Node(WaterInletNode)%Quality                   = 0.0d0
      Node(WaterInletNode)%Press                     = 0.0d0
      Node(WaterInletNode)%HumRat                    = 0.0d0

      ZeroSourceSumHATsurf      =0.0D0
      QBBRadSource              =0.0D0
      QBBRadSrcAvg              =0.0D0
      LastQBBRadSrc             =0.0D0
      LastSysTimeElapsed        =0.0D0
      LastTimeStepSys           =0.0D0

      MyEnvrnFlag(BaseboardNum) = .FALSE.
    END IF

    IF (.not. BeginEnvrnFlag) THEN
        MyEnvrnFlag(BaseboardNum) = .true.
    ENDIF

    IF (BeginTimeStepFlag .AND. FirstHVACIteration) THEN
      ZoneNum = HWBaseboard(BaseboardNum)%ZonePtr
      ZeroSourceSumHATsurf(ZoneNum)    = SumHATsurf(ZoneNum)
      QBBRadSrcAvg(BaseboardNum)       = 0.0D0
      LastQBBRadSrc(BaseboardNum)      = 0.0D0
      LastSysTimeElapsed(BaseboardNum) = 0.0D0
      LastTimeStepSys(BaseboardNum)    = 0.0D0
    END IF

       ! Do the every time step initializations
    WaterInletNode = HWBaseboard(BaseboardNum)%WaterInletNode
    ZoneNode       = ZoneEquipConfig(ControlledZoneNumSub)%ZoneNode
    HWBaseboard(BaseboardNum)%WaterMassFlowRate  = Node(WaterInletNode)%MassFlowRate
    HWBaseboard(BaseboardNum)%WaterInletTemp     = Node(WaterInletNode)%Temp
    HWBaseboard(BaseboardNum)%WaterInletEnthalpy = Node(WaterInletNode)%Enthalpy
    HWBaseboard(BaseboardNum)%AirInletTemp       = Node(ZoneNode)%Temp
    HWBaseboard(BaseboardNum)%AirInletHumRat     = Node(ZoneNode)%HumRat

    HWBaseboard(BaseboardNum)%TotPower   = 0.0d0
    HWBaseboard(BaseboardNum)%Power      = 0.0d0
    HWBaseboard(BaseboardNum)%ConvPower  = 0.0d0
    HWBaseboard(BaseboardNum)%RadPower   = 0.0d0
    HWBaseboard(BaseboardNum)%TotEnergy  = 0.0d0
    HWBaseboard(BaseboardNum)%Energy     = 0.0d0
    HWBaseboard(BaseboardNum)%ConvEnergy = 0.0d0
    HWBaseboard(BaseboardNum)%RadEnergy  = 0.0d0

  RETURN

END SUBROUTINE InitHWBaseboard

SUBROUTINE SizeHWBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2009 Daeho Kang (Add UA autosizing by LMTD)
          !                      Aug 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing hot water baseboard components

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataLoopNode,    ONLY: Node
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: AirInletTempStd  = 18.0d0     ! I=B=R rating document
  REAL(r64), PARAMETER :: CPAirStd         = 1005.0d0   ! Average specific heat of air at between 25C and 40C in J/kg-k
  REAL(r64), PARAMETER :: Constant         = 0.0062d0     ! Constant of linear equation for air mass flow rate
  REAL(r64), PARAMETER :: Coeff            = 0.0000275d0  ! Correlation coefficient to capacity

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: PltSizNum         ! do loop index for plant sizing
  INTEGER    :: PltSizHeatNum     ! index of plant sizing object for 1st heating loop
  REAL(r64)  :: DesCoilLoad
  REAL(r64)  :: WaterInletTempStd
  REAL(r64)  :: WaterOutletTempStd
  REAL(r64)  :: AirOutletTempStd
  REAL(r64)  :: DeltaT1
  REAL(r64)  :: DeltaT2
  REAL(r64)  :: LMTD
  REAL(r64)  :: AirMassFlowRate
  REAL(r64)  :: WaterMassFlowRateStd
  REAL(r64)  :: rho                      ! local fluid density
  REAL(r64)  :: Cp                       ! local fluid specific heat
  LOGICAL    :: ErrorsFound              ! If errors detected in input
  LOGICAL    :: FlowAutosize             ! Indicator to autosize for maximum water vloume flow
  LOGICAL    :: CapAutosize              ! Indicator to autosize for capacity
  REAL(r64)  :: WaterVolFlowRateMaxDes   ! Design maximum water volume flow for reproting
  REAL(r64)  :: WaterVolFlowRateMaxUser  ! User hard-sized maximum water volume flow for reproting
  !REAL(r64)  :: RatedCapacityDes         ! Design rated capacity for reproting
  !REAL(r64)  :: RatedCapacityUser        ! User hard-sized rated capacity for reproting

  PltSizHeatNum = 0
  PltSizNum = 0
  DesCoilLoad = 0.0d0
  ErrorsFound = .FALSE.
  FlowAutosize = .FALSE.
  CapAutosize = .FALSE.
  WaterVolFlowRateMaxDes = 0.0d0
  WaterVolFlowRateMaxUser = 0.0d0
  !RatedCapacityDes = 0.0d0
  !RatedCapacityUser = 0.0d0

  ! find the appropriate heating Plant Sizing object
  PltSizHeatNum = PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%PlantSizNum

  IF (PltSizHeatNum > 0) THEN
    IF (CurZoneEqNum > 0) THEN
      IF (HWBaseboard(BaseboardNum)%WaterVolFlowRateMax == AutoSize) THEN
        FlowAutosize = .TRUE.
      END IF
      IF (.NOT. FlowAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
        IF (HWBaseboard(BaseboardNum)%WaterVolFlowRateMax > 0.0d0) THEN
          CALL ReportSizingOutput(cCMO_BBRadiator_Water,HWBaseboard(BaseboardNum)%EquipID,&
                  'User-Specified Maximum Water Flow Rate [m3/s]',HWBaseboard(BaseboardNum)%WaterVolFlowRateMax)
        END IF
      ELSE
        CALL CheckZoneSizing(cCMO_BBRadiator_Water,HWBaseboard(BaseboardNum)%EquipID)
        DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
        IF (DesCoilLoad >= SmallLoad) THEN
          Cp =  GetSpecificHeatGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                         60.0d0,                      &
                                         PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                                         'SizeHWBaseboard')
          rho = GetDensityGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                          InitConvTemp, &
                                          PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex,&
                                          'SizeHWBaseboard')
          WaterVolFlowRateMaxDes = DesCoilLoad / (PlantSizData(PltSizHeatNum)%DeltaT * Cp * rho)
        ELSE
          WaterVolFlowRateMaxDes = 0.0d0
        END IF

        IF (FlowAutoSize) THEN
          HWBaseboard(BaseboardNum)%WaterVolFlowRateMax = WaterVolFlowRateMaxDes
          CALL ReportSizingOutput(cCMO_BBRadiator_Water,HWBaseboard(BaseboardNum)%EquipID,&
                               'Design Size Maximum Water Flow Rate [m3/s]',WaterVolFlowRateMaxDes)
        ELSE ! Hard-sized with sizing data
          IF (HWBaseboard(BaseboardNum)%WaterVolFlowRateMax > 0.0d0 .AND. WaterVolFlowRateMaxDes > 0.0d0) THEN
            WaterVolFlowRateMaxUser = HWBaseboard(BaseboardNum)%WaterVolFlowRateMax
            CALL ReportSizingOutput(cCMO_BBRadiator_Water,HWBaseboard(BaseboardNum)%EquipID,&
                               'Design Size Maximum Water Flow Rate [m3/s]',WaterVolFlowRateMaxDes, &
                               'User-Specified Maximum Water Flow Rate [m3/s]',WaterVolFlowRateMaxUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(WaterVolFlowRateMaxDes - WaterVolFlowRateMaxUser)/WaterVolFlowRateMaxUser) &
                                 > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeHWBaseboard: Potential issue with equipment sizing for ' &
                                             // 'ZoneHVAC:Baseboard:RadiantConvective:Water="'//  &
                                                      TRIM(HWBaseboard(BaseboardNum)%EquipID)//'".')
                CALL ShowContinueError('User-Specified Maximum Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(WaterVolFlowRateMaxUser,5))//' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(WaterVolFlowRateMaxDes,5))//' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
      IF (HWBaseboard(BaseboardNum)%WaterTempAvg > 0.0d0 .AND. HWBaseboard(BaseboardNum)%WaterMassFlowRateStd > 0.0d0 &
          .AND. HWBaseboard(BaseboardNum)%RatedCapacity > 0.0d0) THEN
        DesCoilLoad = HWBaseboard(BaseboardNum)%RatedCapacity
        WaterMassFlowRateStd = HWBaseboard(BaseboardNum)%WaterMassFlowRateStd
      Else IF (HWBaseboard(BaseboardNum)%RatedCapacity == AutoSize .OR. &
            HWBaseboard(BaseboardNum)%RatedCapacity == 0.0d0) THEN
        DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
        rho = GetDensityGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                          InitConvTemp, &
                          PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex,&
                          'BaseboardRadiatorWater:SizeHWBaseboard')
        WaterMassFlowRateStd = HWBaseboard(BaseboardNum)%WaterVolFlowRateMax * rho
      END IF
      IF (DesCoilLoad >= SmallLoad) THEN
            ! Calculate UA value
            ! Air mass flow rate is obtained from the following linear equation
            ! m_dot = 0.0062 + 2.75e-05*q
          AirMassFlowRate = Constant + Coeff * DesCoilLoad
          Cp =  GetSpecificHeatGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                         HWBaseboard(BaseboardNum)%WaterTempAvg,                      &
                         PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                         'SizeHWBaseboard')
          WaterInletTempStd  = (DesCoilLoad / (2.0d0 * WaterMassFlowRateStd * Cp)) &
                         + HWBaseboard(BaseboardNum)%WaterTempAvg
          WaterOutletTempStd = ABS((2.0d0 * HWBaseboard(BaseboardNum)%WaterTempAvg) - WaterInletTempStd)
          AirOutletTempStd = (DesCoilLoad / (AirMassFlowRate * CPAirStd)) + AirInletTempStd
          HWBaseboard(BaseboardNum)%AirMassFlowRateStd = AirMassFlowRate
                  ! Check Ta,out < Tw,in
          IF (AirOutletTempStd >= WaterInletTempStd) THEN
            CALL ShowSevereError('SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water="'//  &
                 TRIM(HWBaseboard(BaseboardNum)%EquipID)//'".')
            CALL ShowContinueError('...Air Outlet temperature must be below the Water Inlet temperature')
            CALL ShowContinueError('...Air Outlet Temperature=['//trim(RoundSigDigits(AirOutletTempStd,2))//  &
              '], Water Inlet Temperature=['//trim(RoundSigDigits(WaterInletTempStd,2))//'].')
            AirOutletTempStd = WaterInletTempStd-0.01d0
            CALL ShowContinueError('...Air Outlet Temperature set to ['//trim(RoundSigDigits(AirOutletTempStd,2))//'].')
          END IF
                  ! Check Tw,out < Ta,in
          IF (AirInletTempStd >= WaterOutletTempStd) THEN
            CALL ShowSevereError('SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water="'//  &
             TRIM(HWBaseboard(BaseboardNum)%EquipID)//'".')
            CALL ShowContinueError('...Water Outlet temperature must be below the Air Inlet temperature')
            CALL ShowContinueError('...Air Inlet Temperature=['//trim(RoundSigDigits(AirInletTempStd,2))//  &
              '], Water Outlet Temperature=['//trim(RoundSigDigits(WaterOutletTempStd,2))//'].')
            WaterOutletTempStd = AirInletTempStd+0.01d0
            CALL ShowContinueError('...Water Outlet Temperature set to ['//trim(RoundSigDigits(WaterOutletTempStd,2))//'].')
          END IF
                  ! LMTD calculation
          DeltaT1 = WaterInletTempStd  - AirOutletTempStd
          DeltaT2 = WaterOutletTempStd - AirInletTempStd
          LMTD = (DeltaT1 - DeltaT2) / (log(DeltaT1 / DeltaT2))
          HWBaseboard(BaseboardNum)%UA = DesCoilLoad / LMTD
      ELSE
          HWBaseboard(BaseboardNum)%UA = 0.0d0
      END IF
           ! Report an UA value
        CALL ReportSizingOutput(cCMO_BBRadiator_Water,HWBaseboard(BaseboardNum)%EquipID,&
                              'U-Factor times Area [W/C]',HWBaseboard(BaseboardNum)%UA)
    END IF
  ELSE
     ! if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
    IF (HWBaseboard(BaseboardNum)%WaterVolFlowRateMax == AutoSize .OR. &
      HWBaseboard(BaseboardNum)%RatedCapacity == AutoSize .OR. HWBaseboard(BaseboardNum)%RatedCapacity == 0.0d0 ) THEN
      CALL ShowSevereError('Autosizing of hot water baseboard requires a heating loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Hot Water Baseboard Heater='//TRIM(HWBaseboard(BaseboardNum)%EquipID))
           ErrorsFound = .TRUE.
    END IF
    ! calculate UA from rated capacities
    DesCoilLoad = HWBaseboard(BaseboardNum)%RatedCapacity
    IF (DesCoilLoad >= SmallLoad) THEN
      WaterMassFlowRateStd = HWBaseboard(BaseboardNum)%WaterMassFlowRateStd
      ! m_dot = 0.0062 + 2.75e-05*q
      AirMassFlowRate = Constant + Coeff * DesCoilLoad
      Cp =  GetSpecificHeatGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                       HWBaseboard(BaseboardNum)%WaterTempAvg,                      &
                       PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                       'SizeHWBaseboard')
      WaterInletTempStd  = (DesCoilLoad / (2.0d0 * WaterMassFlowRateStd &
                       * Cp)) &
                       + HWBaseboard(BaseboardNum)%WaterTempAvg
      WaterOutletTempStd = ABS((2.0d0 * HWBaseboard(BaseboardNum)%WaterTempAvg) - WaterInletTempStd)
      AirOutletTempStd = (DesCoilLoad / (AirMassFlowRate * CPAirStd)) + AirInletTempStd
      HWBaseboard(BaseboardNum)%AirMassFlowRateStd = AirMassFlowRate

                  ! Check Ta,out < Tw,in
        IF (AirOutletTempStd >= WaterInletTempStd) THEN
          CALL ShowSevereError('SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water="'//  &
             TRIM(HWBaseboard(BaseboardNum)%EquipID)//'".')
          CALL ShowContinueError('...Air Outlet temperature must be below the Water Inlet temperature')
          CALL ShowContinueError('...Air Outlet Temperature=['//trim(RoundSigDigits(AirOutletTempStd,2))//  &
              '], Water Inlet Temperature=['//trim(RoundSigDigits(WaterInletTempStd,2))//'].')
          AirOutletTempStd = WaterInletTempStd-0.01d0
          CALL ShowContinueError('...Air Outlet Temperature set to ['//trim(RoundSigDigits(AirOutletTempStd,2))//'].')
        END IF
                  ! Check Tw,out < Ta,in
        IF (AirInletTempStd >= WaterOutletTempStd) THEN
          CALL ShowSevereError('SizeHWBaseboard: ZoneHVAC:Baseboard:RadiantConvective:Water="'//  &
             TRIM(HWBaseboard(BaseboardNum)%EquipID)//'".')
          CALL ShowContinueError('...Water Outlet temperature must be below the Air Inlet temperature')
          CALL ShowContinueError('...Air Inlet Temperature=['//trim(RoundSigDigits(AirInletTempStd,2))//  &
              '], Water Outlet Temperature=['//trim(RoundSigDigits(WaterOutletTempStd,2))//'].')
          WaterOutletTempStd = AirInletTempStd+0.01d0
          CALL ShowContinueError('...Water Outlet Temperature set to ['//trim(RoundSigDigits(WaterOutletTempStd,2))//'].')
        END IF
                ! LMTD calculation
      DeltaT1 = WaterInletTempStd  - AirOutletTempStd
      DeltaT2 = WaterOutletTempStd - AirInletTempStd
      LMTD = (DeltaT1 - DeltaT2) / (log(DeltaT1 / DeltaT2))
      HWBaseboard(BaseboardNum)%UA = DesCoilLoad / LMTD
    ELSE
      HWBaseboard(BaseboardNum)%UA = 0.0d0
    END IF
              ! Report an UA value
    CALL ReportSizingOutput(cCMO_BBRadiator_Water,HWBaseboard(BaseboardNum)%EquipID,&
                              'U-Factor times Area [W/C]',HWBaseboard(BaseboardNum)%UA)

  END IF
     ! save the design water flow rate for use by the water loop sizing algorithms
  CALL RegisterPlantCompDesignFlow(HWBaseboard(BaseboardNum)%WaterInletNode,HWBaseboard(BaseboardNum)%WaterVolFlowRateMax)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeHWBaseboard

  SUBROUTINE CalcHWBaseboard(BaseboardNum, LoadMet)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       May 2000 Fred Buhl
          !                      Aug 2007 Daeho Kang (Add the calculation of radiant heat source)
          !                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates both the convective and radiant heat transfer rate
          ! in a hot water baseboard heater.  The heater is assumed to be crossflowwith
          ! both fluids unmixed.  The air flow is bouyancy driven and a constant airflow
          ! and a constant airflow velocity of 0.5m/s is assumed.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Incropera and DeWitt, Fundamentals of Heat and Mass Transfer
          ! Chapter 11.4, p. 523, eq. 11.33

          ! USE STATEMENTS:
    USE DataSizing
    USE DataLoopNode,          ONLY: Node
    USE ScheduleManager,       ONLY: GetCurrentScheduleValue
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
    USE DataInterfaces, ONLY: CalcHeatBalanceOutsideSurf, CalcHeatBalanceInsideSurf

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER                :: BaseboardNum
    REAL(r64), INTENT(OUT) :: LoadMet

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Constant = 0.0062d0     ! Constant of linear equation for air mass flow rate
  REAL(r64), PARAMETER :: Coeff  = 0.0000275d0    ! Correlation coefficient to capacity
  REAL(r64), PARAMETER :: MinFrac  = 0.0005d0     ! Minimum fraction that delivers radiant heats to surfaces


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER   :: ZoneNum
    REAL(r64) :: RadHeat
    REAL(r64) :: BBHeat
    REAL(r64) :: AirInletTemp
    REAL(r64) :: AirOutletTemp
    REAL(r64) :: WaterInletTemp
    REAL(r64) :: WaterOutletTemp
    REAL(r64) :: WaterMassFlowRate
    REAL(r64) :: AirMassFlowRate
    REAL(r64) :: CapacitanceAir
    REAL(r64) :: CapacitanceWater
    REAL(r64) :: CapacitanceMax
    REAL(r64) :: CapacitanceMin
    REAL(r64) :: CapacityRatio
    REAL(r64) :: NTU
    REAL(r64) :: Effectiveness
    REAL(r64) :: AA
    REAL(r64) :: BB
    REAL(r64) :: CC
    REAL(r64) :: QZnReq
    REAL(r64) :: Cp

    ZoneNum = HWBaseboard(BaseboardNum)%ZonePtr
    QZnReq  = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
    AirInletTemp      = HWBaseboard(BaseboardNum)%AirInletTemp
    AirOutletTemp     = AirInletTemp
    WaterInletTemp    = HWBaseboard(BaseboardNum)%WaterInletTemp
    WaterOutletTemp   = WaterInletTemp
    WaterMassFlowRate = Node(HWBaseboard(BaseboardNum)%WaterInletNode)%MassFlowRate

    IF (QZnReq > SmallLoad &
       .AND. .NOT. CurDeadbandOrSetback(ZoneNum) &
       .AND. (GetCurrentScheduleValue(HWBaseboard(BaseboardNum)%SchedPtr) > 0) &
       .AND. (WaterMassFlowRate > 0.0d0) ) THEN
        ! Assume the air mass flow rate is twice the water mass flow rate
        ! Calculate air mass flow rate
      AirMassFlowRate  = HWBaseboard(BaseboardNum)%AirMassFlowRateStd * &
                          (WaterMassFlowRate / HWBaseboard(BaseboardNum)%WaterMassFlowRateStd)
      CapacitanceAir   = PsyCpAirFnWTdb(HWBaseboard(BaseboardNum)%AirInletHumRat,AirInletTemp) * AirMassFlowRate
      Cp =  GetSpecificHeatGlycol(PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidName,  &
                               WaterInletTemp,                      &
                               PlantLoop(HWBaseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                               'CalcHWBaseboard')

      CapacitanceWater = Cp * WaterMassFlowRate
      CapacitanceMax   = MAX(CapacitanceAir,CapacitanceWater)
      CapacitanceMin   = MIN(CapacitanceAir,CapacitanceWater)
      CapacityRatio    = CapacitanceMin / CapacitanceMax
      NTU = HWBaseboard(BaseboardNum)%UA / CapacitanceMin

      ! The effectiveness is given by the following formula:
      ! Effectiveness = 1. - EXP((1./CapacityRatio)*(NTU)**0.22*(EXP(-CapacityRatio*(NTU)**0.78)-1.))
      ! To prevent possible underflows (numbers smaller than the computer can handle) we must break
      ! the calculation up into steps and check the size of the exponential arguments.
      AA = -CapacityRatio*(NTU)**0.78d0
      IF (AA.LT.-20.0d0) THEN
        BB = 0.0d0
      ELSE
        BB = EXP(AA)
      END IF
      CC = (1.d0/ CapacityRatio)*(NTU)**0.22d0 * (BB-1.d0)
      IF (CC.LT.-20.d0) THEN
        Effectiveness = 1.0d0
      ELSE
        Effectiveness  = 1.0d0 - EXP(CC)
      END IF

      AirOutletTemp = AirInletTemp + Effectiveness * CapacitanceMin * (WaterInletTemp-AirInletTemp) / CapacitanceAir
      WaterOutletTemp = WaterInletTemp - CapacitanceAir * (AirOutletTemp-AirInletTemp) / CapacitanceWater
      BBHeat = CapacitanceWater * (WaterInletTemp - WaterOutletTemp)
      RadHeat = BBHeat * HWBaseboard(BaseboardNum)%FracRadiant
      QBBRadSource(BaseboardNum) = RadHeat

      IF (HWBaseboard(BaseboardNum)%FracRadiant <= MinFrac) THEN
        LoadMet = BBHeat
      ELSE

       ! Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
        CALL DistributeBBRadGains
       ! Now "simulate" the system by recalculating the heat balances
        CALL CalcHeatBalanceOutsideSurf(ZoneNum)

        CALL CalcHeatBalanceInsideSurf(ZoneNum)

       ! Here an assumption is made regarding radiant heat transfer to people.
       ! While the radiant heat transfer to people array will be used by the thermal comfort
       ! routines, the energy transfer to people would get lost from the perspective
       ! of the heat balance.  So, to avoid this net loss of energy which clearly
       ! gets added to the zones, we must account for it somehow.  This assumption
       ! that all energy radiated to people is converted to convective energy is
       ! not very precise, but at least it conserves energy. The system impact to heat balance
       ! should include this.
        LoadMet = (SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)) &
              + (BBHeat *  HWBaseboard(BaseboardNum)%FracConvect) &
              + (RadHeat * HWBaseboard(BaseboardNum)%FracDistribPerson)
      END IF
      HWBaseboard(BaseboardNum)%WaterOutletEnthalpy = HWBaseboard(BaseboardNum)%WaterInletEnthalpy - BBHeat &
                                                    / WaterMassFlowRate
    ELSE
      CapacitanceWater  = 0.0d0
      CapacitanceMax    = 0.0d0
      CapacitanceMin    = 0.0d0
      NTU               = 0.0d0
      Effectiveness     = 0.0d0
      AirOutletTemp     = AirInletTemp
      WaterOutletTemp   = WaterInletTemp
      BBHeat            = 0.0d0
      LoadMet           = 0.0d0
      RadHeat           = 0.0d0
      WaterMassFlowRate = 0.0d0
      AirMassFlowRate   = 0.0d0
      QBBRadSource(BaseboardNum)   = 0.0d0
      HWBaseboard(BaseboardNum)%WaterOutletEnthalpy = HWBaseboard(BaseboardNum)%WaterInletEnthalpy
    END IF

    HWBaseboard(BaseboardNum)%WaterOutletTemp   = WaterOutletTemp
    HWBaseboard(BaseboardNum)%AirOutletTemp     = AirOutletTemp
    HWBaseboard(BaseboardNum)%WaterMassFlowRate = WaterMassFlowRate
    HWBaseboard(BaseboardNum)%AirMassFlowRate   = AirMassFlowRate
    HWBaseboard(BaseboardNum)%TotPower          = LoadMet
    HWBaseboard(BaseboardNum)%Power             = BBHeat
    HWBaseboard(BaseboardNum)%ConvPower         = BBHeat - RadHeat
    HWBaseboard(BaseboardNum)%RadPower          = RadHeat

    RETURN

  END SUBROUTINE CalcHWBaseboard

  SUBROUTINE UpdateHWBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !                      Rick Strand
          !       DATE WRITTEN   Nov 1997
          !                      February 2001
          !       MODIFIED       Aug 2007 Daeho Kang (Add the update of radiant source)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! The update subrotines both in high temperature radiant radiator
          ! and convective only baseboard radiator are combined and modified.
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode,      ONLY: Node
    USE DataGlobals,       ONLY: TimeStepZone, BeginEnvrnFlag
    USE PlantUtilities,    ONLY: SafeCopyPlantNode
    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)  :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER      :: WaterInletNode
    INTEGER      :: WaterOutletNode
    INTEGER,SAVE :: Iter = 0
    Logical,save :: MyEnvrnFlag = .true.

    IF (Beginenvrnflag .and. Myenvrnflag) then
        Iter=0
        Myenvrnflag = .false.
    END IF
    IF (.not. Beginenvrnflag) then
        Myenvrnflag=.true.
    END IF

          ! First, update the running average if necessary...
    IF (LastSysTimeElapsed(BaseboardNum) == SysTimeElapsed) THEN
        QBBRadSrcAvg(BaseboardNum) = QBBRadSrcAvg(BaseboardNum) - LastQBBRadSrc(BaseboardNum) * &
                                     LastTimeStepSys(BaseboardNum) / TimeStepZone
    END IF
          ! Update the running average and the "last" values with the current values of the appropriate variables
    QBBRadSrcAvg(BaseboardNum) = QBBRadSrcAvg(BaseboardNum) + &
                                 QBBRadSource(BaseboardNum) * TimeStepSys / TimeStepZone

    LastQBBRadSrc(BaseboardNum)      = QBBRadSource(BaseboardNum)
    LastSysTimeElapsed(BaseboardNum) = SysTimeElapsed
    LastTimeStepSys(BaseboardNum)    = TimeStepSys

    WaterInletNode  = HWBaseboard(BaseboardNum)%WaterInletNode
    WaterOutletNode = HWBaseboard(BaseboardNum)%WaterOutletNode

       ! Set the outlet air nodes of the Baseboard
       ! Set the outlet water nodes for the Coil
    CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)
    Node(WaterOutletNode)%Temp         = HWBaseboard(BaseboardNum)%WaterOutletTemp
    Node(WaterOutletNode)%Enthalpy     = HWBaseboard(BaseboardNum)%WaterOutletEnthalpy


    RETURN

  END SUBROUTINE UpdateHWBaseboard

  SUBROUTINE UpdateBBRadSourceValAvg(HWBaseboardSysOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       Aug 2007 Daeho Kang (Modification only for baseboard)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To transfer the average value of the heat source over the entire
          ! zone time step back to the heat balance routines so that the heat
          ! balance algorithms can simulate one last time with the average source
          ! to maintain some reasonable amount of continuity and energy balance
          ! in the temperature and flux histories.

          ! METHODOLOGY EMPLOYED:
          ! All of the record keeping for the average term is done in the Update
          ! routine so the only other thing that this subroutine does is check to
          ! see if the system was even on.  If any average term is non-zero, then
          ! one or more of the radiant systems was running.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(OUT) :: HWBaseboardSysOn   ! .TRUE. if the radiant system has run this zone time step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: BaseboardNum    ! DO loop counter for surface index

       ! FLOW:
    HWBaseboardSysOn = .FALSE.

       ! If this was never allocated, then there are no radiant systems in this input file (just RETURN)
    IF (.NOT.ALLOCATED(QBBRadSrcAvg)) RETURN

       ! If it was allocated, then we have to check to see if this was running at all...
    DO BaseboardNum = 1, NumHWBaseboards
      IF (QBBRadSrcAvg(BaseboardNum) /= 0.0D0) THEN
          HWBaseboardSysOn = .TRUE.
        EXIT !DO loop
      END IF
    END DO

       QBBRadSource = QBBRadSrcAvg

  CALL DistributeBBRadGains  ! QBBRadSource has been modified so we need to redistribute gains

  RETURN

END SUBROUTINE UpdateBBRadSourceValAvg

SUBROUTINE DistributeBBRadGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       Aug. 2007 Daeho Kang (Modification only for baseboard)
          !                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To distribute the gains from the hot water basebaord heater
          ! as specified in the user input file.  This includes distribution
          ! of long wavelength radiant gains to surfaces and "people."

          ! METHODOLOGY EMPLOYED:
          ! We must cycle through all of the radiant systems because each
          ! surface could feel the effect of more than one radiant system.
          ! Note that the energy radiated to people is assumed to affect them
          ! but them it is assumed to be convected to the air.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces,    ONLY : ShowContinueError, ShowWarningError, ShowSevereError, &
                                ShowFatalError
  USE General,           ONLY : RoundSigDigits
  USE DataHeatBalance,   ONLY : Zone
  USE DataHeatBalFanSys, ONLY : QHWBaseboardToPerson, QHWBaseboardSurf, MaxRadHeatFlux
  USE DataSurfaces,      ONLY : Surface, TotSurfaces

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SmallestArea = 0.001d0   ! Smallest area in meters squared (to avoid a divide by zero)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: RadSurfNum      ! Counter for surfaces receiving radiation from radiant heater
  INTEGER   :: BaseboardNum    ! Counter for the baseboard
  INTEGER   :: SurfNum         ! Pointer to the Surface derived type
  INTEGER   :: ZoneNum         ! Pointer to the Zone derived type
  REAL(R64) :: ThisSurfIntensity ! temporary for W/m2 term for rad on a surface

          ! FLOW:
          ! Initialize arrays
    QHWBaseboardSurf     = 0.0D0
    QHWBaseboardToPerson = 0.0D0

    DO BaseboardNum = 1, NumHWBaseboards

      ZoneNum = HWBaseboard(BaseboardNum)%ZonePtr
      IF (ZoneNum <= 0) CYCLE
      QHWBaseboardToPerson(ZoneNum) = QHWBaseboardToPerson(ZoneNum) + &
                                      QBBRadSource(BaseboardNum) * HWBaseboard(BaseboardNum)%FracDistribPerson

      DO RadSurfNum = 1, HWBaseboard(BaseboardNum)%TotSurfToDistrib
        SurfNum = HWBaseboard(BaseboardNum)%SurfacePtr(RadSurfNum)
        IF (Surface(SurfNum)%Area > SmallestArea)   THEN
          ThisSurfIntensity = (QBBRadSource(BaseboardNum) &
                                        * HWBaseboard(BaseboardNum)%FracDistribToSurf(RadSurfNum) &
                                        / Surface(SurfNum)%Area )
          QHWBaseboardSurf(SurfNum) = QHWBaseboardSurf(SurfNum)  + ThisSurfIntensity
           ! CR 8074, trap for excessive intensity (throws off surface balance )
          IF (ThisSurfIntensity > MaxRadHeatFlux) THEN
            CALL ShowSevereError('DistributeBBRadGains:  excessive thermal radiation heat flux intensity detected')
            CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
            CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
            CALL ShowContinueError('Occurs in '//cCMO_BBRadiator_Water//' = '//Trim(HWBaseboard(BaseboardNum)%EquipID))
            CALL ShowContinueError('Radiation intensity = '//Trim(RoundSigDigits(ThisSurfIntensity,2))//' [W/m2]')
            CALL ShowContinueError('Assign a larger surface area or more surfaces in '//cCMO_BBRadiator_Water )
            CALL ShowFatalError('DistributeBBRadGains:  excessive thermal radiation heat flux intensity detected')
          ENDIF
        ELSE
          CALL ShowSevereError('DistributeBBRadGains:  surface not large enough to receive thermal radiation heat flux')
          CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
          CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
          CALL ShowContinueError('Occurs in '//cCMO_BBRadiator_Water//' = '//Trim(HWBaseboard(BaseboardNum)%EquipID))
          CALL ShowContinueError('Assign a larger surface area or more surfaces in '//cCMO_BBRadiator_Water )
          CALL ShowFatalError('DistributeBBRadGains:  surface not large enough to receive thermal radiation heat flux')

        ENDIF
      END DO

    END DO

  RETURN

END SUBROUTINE DistributeBBRadGains


  SUBROUTINE ReportHWBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Aug 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    Use DataLoopNode,    ONLY: Node
    Use DataSurfaces,    ONLY: Surface

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    HWBaseboard(BaseboardNum)%TotEnergy  = HWBaseboard(BaseboardNum)%TotPower  * TimeStepSys*SecInHour
    HWBaseboard(BaseboardNum)%Energy     = HWBaseboard(BaseboardNum)%Power     * TimeStepSys*SecInHour
    HWBaseboard(BaseboardNum)%ConvEnergy = HWBaseboard(BaseboardNum)%ConvPower * TimeStepSys*SecInHour
    HWBaseboard(BaseboardNum)%RadEnergy  = HWBaseboard(BaseboardNum)%RadPower  * TimeStepSys*SecInHour

    RETURN

  END SUBROUTINE ReportHWBaseboard

REAL(r64) FUNCTION SumHATsurf(ZoneNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
          ! The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
          ! and should be updated accordingly.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces
  USE DataHeatBalance
  USE DataHeatBalSurface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum     ! Zone number

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: SurfNum     ! Surface number
  REAL(r64)           :: Area        ! Effective surface area

          ! FLOW:
  SumHATsurf = 0.0d0

  DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    Area = Surface(SurfNum)%Area

    IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        ! The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
        Area = Area + SurfaceWindow(SurfNum)%DividerArea
      END IF

      IF (SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
        ! Window frame contribution
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea &
          * (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn) * SurfaceWindow(SurfNum)%FrameTempSurfIn
      END IF

      IF (SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntShadeOn &
           .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntBlindOn) THEN
        ! Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea &
          * (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn) * SurfaceWindow(SurfNum)%DividerTempSurfIn
      END IF
    END IF

    SumHATsurf = SumHATsurf + HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum)
  END DO

  RETURN

END FUNCTION SumHATsurf

SUBROUTINE UpdateHWBaseboardPlantConnection(BaseboardTypeNum, &
                                            BaseboardName,&
                                            EquipFlowCtrl, &
                                            LoopNum,LoopSide,CompIndex,&
                                            FirstHVACIteration, InitLoopEquip)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept. 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update sim routine called from plant

          ! METHODOLOGY EMPLOYED:
          ! check input, provide comp index, call utility routines

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities , ONLY: PullCompInterconnectTrigger
  USE DataPlant,       ONLY: ccSimPlantEquipTypes, TypeOf_Baseboard_Rad_Conv_Water, &
                             CriteriaType_MassFlowRate, CriteriaType_Temperature, CriteriaType_HeatTransferRate
  USE InputProcessor,  ONLY: FindItemInList
  USE General,         ONLY: TrimSigDigits
  USE DataGlobals,     ONLY: KickOffSimulation

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
  INTEGER, INTENT(IN)    :: BaseboardTypeNum    ! type index
  CHARACTER(len=*), INTENT(IN) :: BaseboardName ! component name
  INTEGER, INTENT(IN)    :: EquipFlowCtrl       ! Flow control mode for the equipment
  INTEGER, INTENT(IN)    :: LoopNum             ! Plant loop index for where called from
  INTEGER, INTENT(IN)    :: LoopSide            ! Plant loop side index for where called from
  INTEGER, INTENT(INOUT) :: CompIndex           ! Chiller number pointer
  LOGICAL , INTENT(IN)   :: FirstHVACIteration
  LOGICAL, INTENT(INOUT) :: InitLoopEquip       ! If not zero, calculate the max load for operating conditions

  INTEGER :: BaseboardNum
  INTEGER :: InletNodeNum
  INTEGER :: OutletNodeNum

    ! Find the correct baseboard
  IF (CompIndex == 0) THEN
    BaseboardNum = FindItemInList(BaseboardName,HWBaseboard%EquipID,NumHWBaseboards)
    IF (BaseboardNum == 0) THEN
      CALL ShowFatalError('UpdateHWBaseboardPlantConnection: Specified baseboard not valid ='//TRIM(BaseboardName))
    ENDIF
    CompIndex=BaseboardNum
  ELSE
    BaseboardNum=CompIndex
    IF (BaseboardNum > NumHWBaseboards .or. BaseboardNum < 1) THEN
      CALL ShowFatalError('UpdateHWBaseboardPlantConnection:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(BaseboardNum))// &
                          ', Number of baseboards='//TRIM(TrimSigDigits(NumHWBaseboards))//  &
                          ', Entered baseboard name='//TRIM(BaseboardName))
    ENDIF
    IF (KickOffSimulation) THEN
      IF (BaseboardName /= HWBaseboard(BaseboardNum)%EquipID) THEN
        CALL ShowFatalError('UpdateHWBaseboardPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', baseboard name='//TRIM(BaseboardName)//', stored baseboard Name for that index='//  &
                            TRIM(HWBaseboard(BaseboardNum)%EquipID))
      ENDIF
      IF (BaseboardTypeNum /= TypeOf_Baseboard_Rad_Conv_Water) THEN
        CALL ShowFatalError('UpdateHWBaseboardPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', baseboard name='//TRIM(BaseboardName)//', stored baseboard Name for that index='//  &
                            TRIM(ccSimPlantEquipTypes(BaseboardTypeNum)) )
      ENDIF
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    RETURN
  END IF

  CALL PullCompInterconnectTrigger(HWBaseboard(BaseboardNum)%LoopNum,     &
                                   HWBaseboard(BaseboardNum)%LoopSideNum, &
                                   HWBaseboard(BaseboardNum)%BranchNum,   &
                                   HWBaseboard(BaseboardNum)%CompNum,     &
                                   HWBaseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   HWBaseboard(BaseboardNum)%LoopNum,     &
                                   HWBaseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_HeatTransferRate,         &
                                   HWBaseboard(BaseboardNum)%Power)

  CALL PullCompInterconnectTrigger(HWBaseboard(BaseboardNum)%LoopNum,     &
                                   HWBaseboard(BaseboardNum)%LoopSideNum, &
                                   HWBaseboard(BaseboardNum)%BranchNum,   &
                                   HWBaseboard(BaseboardNum)%CompNum,     &
                                   HWBaseboard(BaseboardNum)%BBMassFlowReSimIndex, &
                                   HWBaseboard(BaseboardNum)%LoopNum,     &
                                   HWBaseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_MassFlowRate,         &
                                   HWBaseboard(BaseboardNum)%WaterMassFlowRate)

  CALL PullCompInterconnectTrigger(HWBaseboard(BaseboardNum)%LoopNum,     &
                                   HWBaseboard(BaseboardNum)%LoopSideNum, &
                                   HWBaseboard(BaseboardNum)%BranchNum,   &
                                   HWBaseboard(BaseboardNum)%CompNum,     &
                                   HWBaseboard(BaseboardNum)%BBInletTempFlowReSimIndex, &
                                   HWBaseboard(BaseboardNum)%LoopNum,     &
                                   HWBaseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_Temperature,         &
                                   HWBaseboard(BaseboardNum)%WaterOutletTemp)
  RETURN

END SUBROUTINE UpdateHWBaseboardPlantConnection


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

END MODULE HWBaseboardRadiator