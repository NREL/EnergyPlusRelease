! Module SteamBaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Steam)
MODULE SteamBaseboardRadiator
  ! Module containing the routines dealing with the steam baseboard heaters

  ! MODULE INFORMATION:
  !       AUTHOR         Daeho Kang
  !       DATE WRITTEN   September 2009
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate steam baseboard heaters.

  ! METHODOLOGY EMPLOYED:

  ! REFERENCES:
  ! 1. HWBaseboardRadiator module (ZoneHVAC:Baseboard:RadiantConvective:Water)
  ! 2. SteamCoils module (Coil:Heating:Steam)

  ! OTHER NOTES:
  ! na

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig
USE DataHVACGlobals,   ONLY: SmallLoad, TimeStepSys, SysTimeElapsed
USE DataLoopNode,      ONLY: Node, NodeType_Steam, NodeConnectionType_Inlet, NodeConnectionType_Outlet, ObjectIsNotParent
USE DataInterfaces
USE DataPlant       ,   ONLY: PlantLoop, TypeOf_Baseboard_Rad_Conv_Steam

  ! Use statements for access to subroutines in other modules
USE Psychrometrics,     ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: cCMO_BBRadiator_Steam='ZoneHVAC:Baseboard:RadiantConvective:Steam'

  ! DERIVED TYPE DEFINITIONS
  TYPE SteamBaseboardParams
    CHARACTER(len=MaxNameLength) :: EquipID   =' '
    INTEGER :: EquipType =0
    CHARACTER(len=MaxNameLength) :: Schedule  =' '
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfaceName
    INTEGER, ALLOCATABLE, DIMENSION(:) :: SurfacePtr
    INTEGER :: ZonePtr                  = 0
    INTEGER :: SchedPtr                 = 0  ! Pointer to the correct schedule
    INTEGER :: SteamInletNode           = 0  ! Inlet steam baseboard node
    INTEGER :: SteamOutletNode          = 0  ! Outlet steam baseboard node
    INTEGER :: TotSurfToDistrib         = 0  ! Total numbers of the surfaces that the radiant heat gets distributed
    INTEGER :: FluidIndex               = 0  ! Fluid index for FluidProperties (Steam)
    INTEGER :: ControlCompTypeNum       = 0
    INTEGER :: CompErrIndex             = 0
    REAL(r64) :: DegofSubCooling        =0.0d0 ! Temperature differences due to subcooling of the condensate [C]
    REAL(r64) :: Offset                 =0.0d0 ! Control accuracy
    REAL(r64) :: SteamMassFlowRate      =0.0d0 ! Mass flow rate of steam passing through the heater [kg/s]
    REAL(r64) :: SteamMassFlowRateMax   =0.0d0 ! Maximum mass flow rate of steam [kg/s]
    REAL(r64) :: SteamVolFlowRateMax    =0.0d0 ! Maximum volumetric flow rate of steam [m3/s]
    REAL(r64) :: SteamOutletTemp        =0.0d0 ! Outlet steam temperature from the heater [C]
    REAL(r64) :: SteamInletTemp         =0.0d0 ! Inlet steam temperature [C]
    REAL(r64) :: SteamInletEnthalpy     =0.0d0 ! Enthalpy of the steam delivered from the boiler [J/kg]
    REAL(r64) :: SteamOutletEnthalpy    =0.0d0 ! Enthalpy of the steam leaving the heater [J/kg]
    REAL(r64) :: SteamInletPress        =0.0d0 ! Pressure of steam at the inlet of the heater [Pa]
    REAL(r64) :: SteamOutletPress       =0.0d0 ! Pressure of steam at the outlet of the heater [Pa]
    REAL(r64) :: SteamInletQuality      =0.0d0 ! Quality of steam at the inlet of the heater [Pa]
    REAL(r64) :: SteamOutletQuality     =0.0d0 ! Quality of steam at the outlet of the heater [Pa]
    REAL(r64) :: FracRadiant            =0.0d0 ! User defined fraction for radiant heat addition
    REAL(r64) :: FracConvect            =0.0d0 ! Fraction for convective heat addition
    REAL(r64) :: FracDistribPerson      =0.0d0 ! Fraction for radiant heat incident on people
    REAL(r64),   ALLOCATABLE, DIMENSION(:) :: FracDistribToSurf
    REAL(r64) :: TotPower               =0.0d0 ! Convective system impact rate that the heater actually meets [W]
    REAL(r64) :: Power                  =0.0d0 ! Maximum heating rate [W]
    REAL(r64) :: ConvPower              =0.0d0 ! Convective heating rate [W]
    REAL(r64) :: RadPower               =0.0d0 ! Radiant heating rate [W]
    REAL(r64) :: TotEnergy              =0.0d0 ! Convective system impact energy [J]
    REAL(r64) :: Energy                 =0.0d0 ! Maximum heating energy [J]
    REAL(r64) :: ConvEnergy             =0.0d0 ! Convective heating energy [J]
    REAL(r64) :: RadEnergy              =0.0d0 ! Radiant heating energy [J]
    INTEGER   :: LoopNum                =0   ! plant loop index
    INTEGER   :: LoopSideNum            =0   ! plant loop side index
    INTEGER   :: BranchNum              =0   ! plant loop branch index
    INTEGER   :: CompNum                =0   ! plant loop component index
    INTEGER   :: BBLoadReSimIndex       =0   !
    INTEGER   :: BBMassFlowReSimIndex   =0
    INTEGER   :: BBInletTempFlowReSimIndex =0

  END TYPE SteamBaseboardParams

  !MODULE VARIABLE DECLARATIONS:
  TYPE (SteamBaseboardParams), ALLOCATABLE, DIMENSION(:) :: SteamBaseboard
  INTEGER :: NumSteamBaseboards = 0
  INTEGER :: SteamIndex=0

  REAL(r64), ALLOCATABLE, DIMENSION(:) :: QBBSteamRadSource      ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: QBBSteamRadSrcAvg      ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZeroSourceSumHATsurf   ! Equal to the SumHATsurf for all the walls in a zone
                                                                 ! with no source

  ! Record keeping variables used to calculate QBBRadSrcAvg locally
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastQBBSteamRadSrc     ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastSysTimeElapsed     ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastTimeStepSys        ! Need to keep the last value in case we are still iterating
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: CheckEquipName
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag   ! get loop number flag

  !SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

  PUBLIC  :: SimSteamBaseboard
  PRIVATE :: GetSteamBaseboardInput
  PRIVATE :: InitSteamBaseboard
  PRIVATE :: SizeSteamBaseboard
  PUBLIC  :: CalcSteamBaseboard
  PUBLIC  :: UpdateBBSteamRadSourceValAvg
  PRIVATE :: UpdateSteamBaseboard
  PRIVATE :: DistributeBBSteamRadGains
  PRIVATE :: ReportSteamBaseboard
  PRIVATE :: SumHATsurf
  PUBLIC  :: UpdateSteamBaseboardPlantConnection

CONTAINS

  SUBROUTINE SimSteamBaseboard(EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration,  &
                               PowerMet, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the steam baseboards or radiators.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,        ONLY: FindItemInList,MakeUPPERCase
    USE General,               ONLY: TrimSigDigits
    USE ScheduleManager,       ONLY: GetCurrentScheduleValue
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
    USE DataInterfaces,        ONLY: ControlCompOutput
    USE PlantUtilities,        ONLY: SetComponentFlowRate

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
    INTEGER      :: BaseboardNum           ! index of unit in baseboard array
    LOGICAL,SAVE :: GetInputFlag = .TRUE.  ! one time get input flag
    REAL(r64)    :: QZnReq                 ! zone load not yet satisfied
    REAL(r64)    :: MaxSteamFlow
    REAL(r64)    :: MinSteamFlow
    REAL(r64)    :: mdot = 0.d0

    IF (GetInputFlag) THEN
      CALL GetSteamBaseboardInput
      GetInputFlag=.false.
    END IF

       ! Find the correct Baseboard Equipment
    IF (CompIndex == 0) THEN
      BaseboardNum = FindItemInList(EquipName, SteamBaseboard%EquipID, NumSteamBaseboards)
      IF (BaseboardNum == 0) THEN
        CALL ShowFatalError('SimSteamBaseboard: Unit not found='//TRIM(EquipName))
      ENDIF
      CompIndex = BaseboardNum
    ELSE
      BaseboardNum = CompIndex
      IF (BaseboardNum > NumSteamBaseboards .or. BaseboardNum < 1) THEN
        CALL ShowFatalError('SimSteamBaseboard:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumSteamBaseboards))//  &
                            ', Entered Unit name='//TRIM(EquipName))
      ENDIF
      IF (CheckEquipName(BaseboardNum)) THEN
        IF (EquipName /= SteamBaseboard(BaseboardNum)%EquipID) THEN
          CALL ShowFatalError('SimSteamBaseboard: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(BaseboardNum))// &
                              ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                              TRIM(SteamBaseboard(BaseboardNum)%EquipID))
        ENDIF
        CheckEquipName(BaseboardNum)=.false.
      ENDIF
    ENDIF

    IF (CompIndex > 0) THEN

      CALL InitSteamBaseboard (BaseboardNum, ControlledZoneNum, FirstHVACIteration)

      QZnReq = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToHeatSP

      IF (QZnReq > SmallLoad &
          .AND. .NOT. CurDeadbandOrSetback(ActualZoneNum) &
          .AND. (GetCurrentScheduleValue(SteamBaseboard(BaseboardNum)%SchedPtr) > 0.0d0) ) THEN

           ! On the first HVAC iteration the system values are given to the controller, but after that
           ! the demand limits are in place and there needs to be feedback to the Zone Equipment
        If(FirstHVACIteration)Then
           MaxSteamFlow = SteamBaseboard(BaseboardNum)%SteamMassFlowRateMax
           MinSteamFlow = 0.0d0
        Else
           MaxSteamFlow = Node(SteamBaseboard(BaseboardNum)%SteamInletNode)%MassFlowRateMaxAvail
           MinSteamFlow = Node(SteamBaseboard(BaseboardNum)%SteamInletNode)%MassFlowRateMinAvail
        ENDIF

        SELECT CASE (SteamBaseboard(BaseboardNum)%EquipType)

          CASE (TypeOf_Baseboard_Rad_Conv_Steam)  ! 'ZoneHVAC:Baseboard:RadiantConvective:Steam'
            CALL ControlCompOutput(CompName=SteamBaseBoard(BaseboardNum)%EquipID,CompType=cCMO_BBRadiator_Steam, &
                                   CompNum=BaseboardNum, &
                                   FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                                   ActuatedNode=SteamBaseboard(BaseboardNum)%SteamInletNode, &
                                   MaxFlow=MaxSteamFlow,MinFlow=MinSteamFlow, &
                                   ControlOffset=SteamBaseboard(BaseboardNum)%Offset, &
                                   ControlCompTypeNum=SteamBaseboard(BaseboardNum)%ControlCompTypeNum, &
                                   CompErrIndex=SteamBaseboard(BaseboardNum)%CompErrIndex, &
                                   LoopNum = SteamBaseboard(BaseboardNum)%LoopNum, &
                                   LoopSide = SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   BranchIndex = SteamBaseboard(BaseboardNum)%BranchNum)
          CASE DEFAULT
            CALL ShowSevereError('SimSteamBaseboard: Errors in Baseboard='//TRIM(SteamBaseboard(BaseboardNum)%EquipID))
            CALL ShowContinueError('Invalid or unimplemented equipment type='//  &
                TRIM(TrimSigDigits(SteamBaseboard(BaseboardNum)%EquipType)))
            CALL ShowFatalError('Preceding condition causes termination.')

        END SELECT

        PowerMet = SteamBaseboard(BaseboardNum)%TotPower
      ELSE
      ! baseboard is off, don't bother going into ControlCompOutput
        mdot = 0.d0
        CALL SetComponentFlowRate(mdot, &
                                   SteamBaseboard(BaseboardNum)%SteamInletNode, &
                                   SteamBaseboard(BaseboardNum)%SteamOutletNode, &
                                   SteamBaseboard(BaseboardNum)%LoopNum, &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   SteamBaseboard(BaseboardNum)%BranchNum, &
                                   SteamBaseboard(BaseboardNum)%CompNum )
        CALL CalcSteamBaseboard(BaseboardNum, PowerMet)

      ENDIF

      CALL UpdateSteamBaseboard(BaseboardNum)

      CALL ReportSteamBaseboard(BaseboardNum)

    ELSE
        CALL ShowFatalError('SimSteamBaseboard: Unit not found='//TRIM(EquipName))
    ENDIF

  END SUBROUTINE SimSteamBaseboard

  SUBROUTINE GetSteamBaseboardInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! Standard input processor calls.

          ! REFERENCES:
          ! HWBaseboardRadiator module

          ! USE STATEMENTS:
    USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, SameString, VerifyName
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet
    USE DataSurfaces,          ONLY: Surface, TotSurfaces
    USE ScheduleManager,       ONLY: GetScheduleIndex, GetCurrentScheduleValue
    USE GlobalNames,           ONLY: VerifyUniqueBaseboardName
    USE General,               ONLY: RoundSigDigits
    USE FluidProperties,       ONLY: FindRefrigerant
    USE DataIPShortCuts
    USE DataSizing
    USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='GetSteamBaseboardInput:'
    REAL(r64), PARAMETER :: MaxFraction       = 1.0d0       ! Maximum limit of fractional values
    REAL(r64), PARAMETER :: MinFraction       = 0.0d0       ! Minimum limit of fractional values
    REAL(r64), PARAMETER :: MaxSteamFlowRate  = 10.0d0      ! Maximum limit of steam volume flow rate in m3/s
    REAL(r64), PARAMETER :: MinSteamFlowRate  = 0.0d0       ! Minimum limit of steam volume flow rate in m3/s
!    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20          ! Maximum number of surfaces that a baseboard heater can radiate to
    INTEGER,PARAMETER :: MinDistribSurfaces   = 1           ! Minimum number of surfaces that a baseboard heater can radiate to
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
    LOGICAL   :: ErrorsFound = .false. ! If errors detected in input
    LOGICAL   :: IsNotOK               ! Flag to verify name
    LOGICAL   :: IsBlank               ! Flag for blank name
    LOGICAL   :: errflag
    LOGICAL   :: SteamMessageNeeded

    SteamMessageNeeded=.true.
    NumSteamBaseboards = GetNumObjectsFound(cCMO_BBRadiator_Steam)

    ! Count total number of baseboard units

    ALLOCATE(SteamBaseboard(NumSteamBaseboards))
    ALLOCATE(CheckEquipName(NumSteamBaseboards))
    CheckEquipName=.true.

       ! Get the data from the user input related to baseboard heaters
    DO BaseboardNum = 1, NumSteamBaseboards

      CALL GetObjectItem(cCMO_BBRadiator_Steam,BaseboardNum,cAlphaArgs,NumAlphas,&
                         rNumericArgs,NumNumbers,IOSTAT, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),SteamBaseboard%EquipID,BaseboardNum,IsNotOK,IsBlank,cCMO_BBRadiator_Steam//' Name')

        IF (IsNotOK) THEN
           ErrorsFound=.true.
        ENDIF
        CALL VerifyUniqueBaseboardName(cCMO_BBRadiator_Steam,cAlphaArgs(1),errflag,cCMO_BBRadiator_Steam//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

    SteamBaseboard(BaseboardNum)%EquipID   = cAlphaArgs(1)           ! Name of the baseboard
    SteamBaseboard(BaseboardNum)%EquipType = TypeOf_Baseboard_Rad_Conv_Steam !'ZoneHVAC:Baseboard:RadiantConvective:Steam'

       ! Get schedule
    SteamBaseboard(BaseboardNum)%Schedule  = cAlphaArgs(2)
    IF (lAlphaFieldBlanks(2)) THEN
      SteamBaseboard(BaseboardNum)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      SteamBaseboard(BaseboardNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
      IF (SteamBaseboard(BaseboardNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
           '", '//TRIM(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
        ErrorsFound=.true.
      ENDIF
    ENDIF

       ! Get inlet node number
    SteamBaseboard(BaseboardNum)%SteamInletNode = &
    GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,cCMO_BBRadiator_Steam,cAlphaArgs(1), &
                    NodeType_Steam, NodeConnectionType_Inlet,1,ObjectIsNotParent)

       ! Get outlet node number
    SteamBaseboard(BaseboardNum)%SteamOutletNode = &
    GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,cCMO_BBRadiator_Steam,cAlphaArgs(1), &
                    NodeType_Steam,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    CALL TestCompSet(cCMO_BBRadiator_Steam,cAlphaArgs(1),cAlphaArgs(3), &
                     cAlphaArgs(4),'Hot Steam Nodes')

        ! Desired degree of cooling
    SteamBaseboard(BaseboardNum)%DegofSubCooling = rNumericArgs(1)
        ! Maximum steam flow rate
    SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax = rNumericArgs(2)
      IF(SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax >= MaxSteamFlowRate) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(2))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxSteamFlowRate,2))//'].')
         SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax = MaxSteamFlowRate
      ELSE IF (SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax <= MinSteamFlowRate .AND. &
                SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax /= autosize) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(2))//' was less than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinSteamFlowRate,2))//'].')
          SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax = MinSteamFlowRate
      END IF

    SteamBaseboard(BaseboardNum)%Offset = rNumericArgs(3)
       ! Set default convergence tolerance
    IF (SteamBaseboard(BaseboardNum)%Offset .LE. 0.0d0) THEN
        SteamBaseboard(BaseboardNum)%Offset = 0.001d0
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(6))//' was less than the allowable minimum.')
        CALL ShowContinueError('...reset to default value=[0.001].')
      END IF
        ! Fraction of radiant heat out of the total heating rate of the unit
    SteamBaseboard(BaseboardNum)%FracRadiant = rNumericArgs(4)
      IF (SteamBaseboard(BaseboardNum)%FracRadiant < MinFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(7))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinFraction,3))//'].')
       SteamBaseboard(BaseboardNum)%FracRadiant = MinFraction
      ELSE IF (SteamBaseboard(BaseboardNum)%FracRadiant > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(7))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,3))//'].')
        SteamBaseboard(BaseboardNum)%FracRadiant = MaxFraction
      END IF

       ! Remaining fraction is added to the zone as convective heat transfer
    AllFracsSummed = SteamBaseboard(BaseboardNum)%FracRadiant
      IF (AllFracsSummed > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", Fraction Radiant was higher than the allowable maximum.')
        SteamBaseboard(BaseboardNum)%FracRadiant = MaxFraction
        SteamBaseboard(BaseboardNum)%FracConvect = 0.0d0
      ELSE
        SteamBaseboard(BaseboardNum)%FracConvect = 1.0d0 - AllFracsSummed
      END IF
        ! Fraction of radiant heat addition to the people within the radiant heating capacity specified by the user
    SteamBaseboard(BaseboardNum)%FracDistribPerson = rNumericArgs(5)
      IF (SteamBaseboard(BaseboardNum)%FracDistribPerson < MinFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(8))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinFraction,3))//'].')
        SteamBaseboard(BaseboardNum)%FracDistribPerson = MinFraction
      END IF
      IF (SteamBaseboard(BaseboardNum)%FracDistribPerson > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(8))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,3))//'].')
        SteamBaseboard(BaseboardNum)%FracDistribPerson = MaxFraction
      END IF
        !
    SteamBaseboard(BaseboardNum)%TotSurfToDistrib = NumNumbers - 5
!      IF (SteamBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
!        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
!          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
!        CALL ShowContinueError('...only the maximum value=['//trim(RoundSigDigits(MaxDistribSurfaces))//  &
!           '] will be processed.')
!        SteamBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
!      END IF
      IF ( (SteamBaseboard(BaseboardNum)%TotSurfToDistrib < MinDistribSurfaces) .AND. &
           (SteamBaseboard(BaseboardNum)%FracRadiant > MinFraction) ) THEN
        CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", the number of surface/radiant fraction groups entered was less than the allowable minimum.')
        CALL ShowContinueError('...the minimum that must be entered=['//trim(RoundSigDigits(MinDistribSurfaces))//'].')
        ErrorsFound = .true.
        SteamBaseboard(BaseboardNum)%TotSurfToDistrib = 0
      END IF
        ! Allocate the surfaces and fractions
     ALLOCATE(SteamBaseboard(BaseboardNum)%SurfaceName(SteamBaseboard(BaseboardNum)%TotSurfToDistrib))
     SteamBaseboard(BaseboardNum)%SurfaceName=' '
     ALLOCATE(SteamBaseboard(BaseboardNum)%SurfacePtr(SteamBaseboard(BaseboardNum)%TotSurfToDistrib))
     SteamBaseboard(BaseboardNum)%SurfacePtr=0
     ALLOCATE(SteamBaseboard(BaseboardNum)%FracDistribToSurf(SteamBaseboard(BaseboardNum)%TotSurfToDistrib))
     SteamBaseboard(BaseboardNum)%FracDistribToSurf=0.0d0

     AllFracsSummed = SteamBaseboard(BaseboardNum)%FracDistribPerson
     Do SurfNum = 1,SteamBaseboard(BaseboardNum)%TotSurfToDistrib
        SteamBaseboard(BaseboardNum)%SurfaceName(SurfNum) = cAlphaArgs(SurfNum + 4)
        SteamBaseboard(BaseboardNum)%SurfacePtr(SurfNum)  = FindItemInList(cAlphaArgs(SurfNum + 4),Surface%Name,TotSurfaces)
        SteamBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) = rNumericArgs(SurfNum + 5)
      IF (SteamBaseboard(BaseboardNum)%SurfacePtr(SurfNum) == 0 ) THEN
          CALL ShowSevereError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cAlphaFieldNames(SurfNum+4))//'="'//trim(cAlphaArgs(SurfNum + 4))//'" invalid - not found.')
          ErrorsFound = .true.
        END IF
      IF (SteamBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) > MaxFraction) THEN
          CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cNumericFieldNames(SurfNum+6))//'was greater than the allowable maximum.')
          CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,1))//'].')
          SteamBaseboard(BaseboardNum)%TotSurfToDistrib = MaxFraction
      END IF
      IF (SteamBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) < MinFraction) THEN
          CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cNumericFieldNames(SurfNum+6))//'was less than the allowable minimum.')
          CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MinFraction,1))//'].')
          SteamBaseboard(BaseboardNum)%TotSurfToDistrib = MinFraction
        END IF
      IF (SteamBaseboard(BaseboardNum)%SurfacePtr(SurfNum) /= 0 ) THEN
        Surface(SteamBaseboard(BaseboardNum)%SurfacePtr(SurfNum))%IntConvSurfGetsRadiantHeat = .TRUE.
      ENDIF

     AllFracsSummed = AllFracsSummed + SteamBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum)
     End DO ! surfaces

    IF (AllFracsSummed > (MaxFraction + 0.01d0) ) THEN
      CALL ShowSevereError('Fraction of radiation distributed to surfaces sums up to greater than 1 for '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Occurs in Baseboard Heater='//TRIM(cAlphaArgs(1)))
           ErrorsFound = .TRUE.
    END IF
      IF ( (AllFracsSummed < (MaxFraction - 0.01d0)) .AND. &               ! User didn't distribute all of the
           (SteamBaseboard(BaseboardNum)%FracRadiant > MinFraction) ) THEN ! radiation warn that some will be lost
        CALL ShowWarningError(RoutineName//cCMO_BBRadiator_Steam//'="'//trim(cAlphaArgs(1))// &
          '", Summed radiant fractions for people + surface groups < 1.0')
        CALL ShowContinueError('The rest of the radiant energy delivered by the baseboard heater will be lost')
      END IF

      IF (SteamIndex == 0 .and. BaseboardNum == 1) THEN
        SteamIndex=FindRefrigerant('Steam')
        IF (SteamIndex == 0) THEN
          CALL ShowSevereError(RoutineName//'Steam Properties for '//TRIM(cAlphaArgs(1))// &
                               ' not found.')
          IF (SteamMessageNeeded) CALL ShowContinueError('Steam Fluid Properties should have been included in the input file.')
          ErrorsFound=.true.
          SteamMessageNeeded=.false.
        ENDIF
      ENDIF

      SteamBaseboard(BaseBoardNum)%FluidIndex=SteamIndex

    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//cCMO_BBRadiator_Steam//'Errors found getting input. Program terminates.')
    END IF

       ! Setup Report variables for the Coils
    DO BaseboardNum = 1, NumSteamBaseboards
      ! CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Steam'
      CALL SetupOutputVariable('Baseboard Total Heating Rate [W]', SteamBaseboard(BaseboardNum)%TotPower, &
                               'System','Average',SteamBaseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Convective Heating Rate [W]', SteamBaseboard(BaseboardNum)%ConvPower, &
                               'System','Average',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Radiant Heating Rate [W]', SteamBaseboard(BaseboardNum)%RadPower, &
                               'System','Average',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Total Heating Energy [J]', SteamBaseboard(BaseboardNum)%TotEnergy, &
                               'System','Sum',SteamBaseboard(BaseboardNum)%EquipID, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BASEBOARD',GroupKey='System')
      CALL SetupOutputVariable('Baseboard Convective Heating Energy [J]', SteamBaseboard(BaseboardNum)%ConvEnergy, &
                               'System','Sum',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Radiant Heating Energy [J]', SteamBaseboard(BaseboardNum)%RadEnergy, &
                               'System','Sum',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Steam Energy [J]', SteamBaseboard(BaseboardNum)%Energy, &
                               'System','Sum',SteamBaseboard(BaseboardNum)%EquipID, &
                                ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='BASEBOARD',GroupKey='System')
      CALL SetupOutputVariable('Baseboard Steam Rate [W]', SteamBaseboard(BaseboardNum)%Power, &
                               'System','Average',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Steam Mass Flow Rate [kg/s]', SteamBaseboard(BaseboardNum)%SteamMassFlowRate, &
                               'System','Average',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Steam Inlet Temperature [C]', SteamBaseboard(BaseboardNum)%SteamInletTemp, &
                                'System','Average',SteamBaseboard(BaseboardNum)%EquipID)
      CALL SetupOutputVariable('Baseboard Steam Outlet Temperature [C]', SteamBaseboard(BaseboardNum)%SteamOutletTemp, &
                              'System','Average',SteamBaseboard(BaseboardNum)%EquipID)
    END DO

    RETURN

  END SUBROUTINE GetSteamBaseboardInput

SUBROUTINE InitSteamBaseboard(BaseboardNum, ControlledZoneNumSub, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !                      Rick Strand
          !       DATE WRITTEN   Nov 1997
          !                      Feb 2001
          !       MODIFIED       Sep 2009 Daeho Kang (Add Radiant Component)
          !                      Sept 2010 Chandan Sharma, FSEC (plant interactions)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! The initialization subrotines both in high temperature radiant radiator
          ! and convective only baseboard radiator are combined and modified.
          ! The heater is assumed to be crossflow with both fluids unmixed.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataEnvironment,   ONLY: StdBaroPress
  USE FluidProperties,   ONLY: GetSatEnthalpyRefrig, GetSatDensityRefrig
  USE PlantUtilities,    ONLY: InitComponentNodes
  USE DataPlant,         ONLY: ScanPlantLoopsForObject

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
  INTEGER, INTENT(IN)    :: BaseboardNum
  INTEGER, INTENT(IN)    :: ControlledZoneNumSub

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: MyOneTimeFlag = .true.
  LOGICAL, SAVE :: ZoneEquipmentListChecked = .false.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  INTEGER    :: Loop
  INTEGER    :: SteamInletNode
  INTEGER    :: ZoneNode
  INTEGER    :: ZoneNum
  REAL(r64)  :: StartEnthSteam
  REAL(r64)  :: SteamDensity
  LOGICAL    :: errFlag

       ! Do the one time initializations
    IF (MyOneTimeFlag) THEN

       ! initialize the environment and sizing flags
      ALLOCATE(MyEnvrnFlag(NumSteamBaseboards))
      ALLOCATE(MySizeFlag(NumSteamBaseboards))
      ALLOCATE(ZeroSourceSumHATsurf(NumofZones))
               ZeroSourceSumHATsurf = 0.0D0
      ALLOCATE(QBBSteamRadSource(NumSteamBaseboards))
               QBBSteamRadSource = 0.0D0
      ALLOCATE(QBBSteamRadSrcAvg(NumSteamBaseboards))
               QBBSteamRadSrcAvg = 0.0D0
      ALLOCATE(LastQBBSteamRadSrc(NumSteamBaseboards))
               LastQBBSteamRadSrc = 0.0D0
      ALLOCATE(LastSysTimeElapsed(NumSteamBaseboards))
               LastSysTimeElapsed = 0.0D0
      ALLOCATE(LastTimeStepSys(NumSteamBaseboards))
               LastTimeStepSys = 0.0D0
      ALLOCATE(SetLoopIndexFlag(NumSteamBaseboards))
      MyEnvrnFlag      = .TRUE.
      MySizeFlag       = .TRUE.
      MyOneTimeFlag    = .false.
      SetLoopIndexFlag = .TRUE.
    END IF

    IF (SteamBaseboard(BaseboardNum)%ZonePtr <= 0) &
      SteamBaseboard(BaseboardNum)%ZonePtr = ZoneEquipConfig(ControlledZoneNumSub)%ActualZoneNum

       ! Need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
    IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
      ZoneEquipmentListChecked=.true.
      DO Loop=1,NumSteamBaseboards
        IF (CheckZoneEquipmentList(cCMO_BBRadiator_Steam,SteamBaseboard(Loop)%EquipID)) CYCLE
        CALL ShowSevereError('InitBaseboard: Unit=['//TRIM(cCMO_BBRadiator_Steam)//','//  &
                TRIM(SteamBaseboard(Loop)%EquipID)//'] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
      END DO
    ENDIF

    IF(SetLoopIndexFlag(BaseboardNum))THEN
      IF(ALLOCATED(PlantLoop))THEN
        errFlag=.false.
        CALL ScanPlantLoopsForObject(SteamBaseboard(BaseboardNum)%EquipID,     &
                                     SteamBaseboard(BaseboardNum)%EquipType,   &
                                     SteamBaseboard(BaseboardNum)%LoopNum,     &
                                     SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                     SteamBaseboard(BaseboardNum)%BranchNum,   &
                                     SteamBaseboard(BaseboardNum)%CompNum,     &
                                     errFlag=errFlag)
        SetLoopIndexFlag(BaseboardNum) = .FALSE.
        IF (errFlag) THEN
          CALL ShowFatalError('InitSteamBaseboard: Program terminated for previous conditions.')
        ENDIF
      ENDIF
    ENDIF

    IF (.NOT. SysSizingCalc .AND. MySizeFlag(BaseboardNum) .AND. (.NOT. SetLoopIndexFlag(BaseboardNum))) THEN
       ! For each coil, do the sizing once
      CALL SizeSteamBaseboard(BaseboardNum)
      MySizeFlag(BaseboardNum) = .FALSE.
    END IF

       ! Do the Begin Environment initializations
    IF (BeginEnvrnFlag .and. MyEnvrnFlag(BaseboardNum)) THEN
       ! Initialize
      SteamInletNode = SteamBaseboard(BaseboardNum)%SteamInletNode
      Node(SteamInletNode)%Temp  = 100.0d0
      Node(SteamInletNode)%Press = 101325.0d0
      SteamDensity   = GetSatDensityRefrig ('STEAM',Node(SteamInletNode)%Temp,1.0d0,Node(SteamInletNode)%FluidIndex, &
                                          'InitSteamCoil')
      StartEnthSteam = GetSatEnthalpyRefrig('STEAM',Node(SteamInletNode)%Temp,1.0d0,Node(SteamInletNode)%FluidIndex, &
                                          'InitSteamCoil')
      SteamBaseboard(BaseboardNum)%SteamMassFlowRateMax = SteamDensity * SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax
      CALL InitComponentNodes(0.d0,SteamBaseboard(BaseboardNum)%SteamMassFlowRateMax, &
                                   SteamBaseboard(BaseboardNum)%SteamInletNode,       &
                                   SteamBaseboard(BaseboardNum)%SteamOutletNode,      &
                                   SteamBaseboard(BaseboardNum)%LoopNum,              &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum,          &
                                   SteamBaseboard(BaseboardNum)%BranchNum,            &
                                   SteamBaseboard(BaseboardNum)%CompNum)
      Node(SteamInletNode)%Enthalpy                     = StartEnthSteam
      Node(SteamInletNode)%Quality                      = 1.0d0
      Node(SteamInletNode)%HumRat                       = 0.0d0

          ! Initializes radiant sources
      ZeroSourceSumHATsurf      =0.0D0
      QBBSteamRadSource         =0.0D0
      QBBSteamRadSrcAvg         =0.0D0
      LastQBBSteamRadSrc        =0.0D0
      LastSysTimeElapsed        =0.0D0
      LastTimeStepSys           =0.0D0

      MyEnvrnFlag(BaseboardNum) = .FALSE.
    END IF

    IF (.not. BeginEnvrnFlag) THEN
        MyEnvrnFlag(BaseboardNum) = .true.
    ENDIF

    IF (BeginTimeStepFlag .AND. FirstHVACIteration) THEN
      ZoneNum = SteamBaseboard(BaseboardNum)%ZonePtr
      ZeroSourceSumHATsurf(ZoneNum)    = SumHATsurf(ZoneNum)
      QBBSteamRadSrcAvg(BaseboardNum)  = 0.0D0
      LastQBBSteamRadSrc(BaseboardNum) = 0.0D0
      LastSysTimeElapsed(BaseboardNum) = 0.0D0
      LastTimeStepSys(BaseboardNum)    = 0.0D0
    END IF

       ! Do the every time step initializations
    SteamInletNode = SteamBaseboard(BaseboardNum)%SteamInletNode
    ZoneNode       = ZoneEquipConfig(ControlledZoneNumSub)%ZoneNode
    SteamBaseboard(BaseboardNum)%SteamMassFlowRate  = Node(SteamInletNode)%MassFlowRate
    SteamBaseboard(BaseboardNum)%SteamInletTemp     = Node(SteamInletNode)%Temp
    SteamBaseboard(BaseboardNum)%SteamInletEnthalpy = Node(SteamInletNode)%Enthalpy
    SteamBaseboard(BaseboardNum)%SteamInletPress    = Node(SteamInletNode)%Press
    SteamBaseboard(BaseboardNum)%SteamInletQuality  = Node(SteamInletNode)%Quality

    SteamBaseboard(BaseboardNum)%TotPower   = 0.0d0
    SteamBaseboard(BaseboardNum)%Power      = 0.0d0
    SteamBaseboard(BaseboardNum)%ConvPower  = 0.0d0
    SteamBaseboard(BaseboardNum)%RadPower   = 0.0d0
    SteamBaseboard(BaseboardNum)%TotEnergy  = 0.0d0
    SteamBaseboard(BaseboardNum)%Energy     = 0.0d0
    SteamBaseboard(BaseboardNum)%ConvEnergy = 0.0d0
    SteamBaseboard(BaseboardNum)%RadEnergy  = 0.0d0

  RETURN

END SUBROUTINE InitSteamBaseboard

SUBROUTINE SizeSteamBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing steam baseboard components

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE PlantUtilities,      ONLY: RegisterPlantCompDesignFlow
  USE DataEnvironment,     ONLY: StdBaroPress
  USE FluidProperties,     ONLY: GetSatEnthalpyRefrig,GetSatDensityRefrig, GetSatSpecificHeatRefrig
!  USE BranchInputManager,  ONLY: MyPlantSizingIndex
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: PltSizSteamNum      ! Index of plant sizing object for 1st steam loop
  REAL(r64) :: DesCoilLoad         ! Design heating load in the zone
  REAL(r64) :: SteamInletTemp      ! Inlet steam temperature in C
  REAL(r64) :: EnthSteamInDry      ! Enthalpy of dry steam
  REAL(r64) :: EnthSteamOutWet     ! Enthalpy of wet steam
  REAL(r64) :: LatentHeatSteam     ! latent heat of steam
  REAL(r64) :: SteamDensity        ! Density of steam
  REAL(r64) :: Cp                  ! local fluid specific heat
  REAL(r64) :: tmpSteamVolFlowRateMax ! local temporary design steam flow
  LOGICAL   :: ErrorsFound         ! If errors detected in input
  LOGICAL   :: IsAutosize               ! Indicator to autosizing steam flow
  REAL(r64) :: SteamVolFlowRateMaxDes   ! Design maximum steam volume flow for reporting
  REAL(r64) :: SteamVolFlowRateMaxUser  ! User hard-sized maximum steam volume flow for reporting


  PltSizSteamNum = 0
  DesCoilLoad    = 0.0d0
  ErrorsFound    = .FALSE.
  IsAutosize = .FALSE.
  SteamVolFlowRateMaxDes = 0.0d0
  SteamVolFlowRateMaxUser = 0.0d0

        ! Find the appropriate steam plant sizing object
    PltSizSteamNum = PlantLoop(SteamBaseboard(BaseboardNum)%LoopNum)%PlantSizNum
!    PltSizSteamNum = MyPlantSizingIndex('Coil:Heating:Steam', SteamBaseboard(BaseboardNum)%EquipID, &
!                    SteamBaseboard(BaseboardNum)%SteamInletNode, &
!                    SteamBaseboard(BaseboardNum)%SteamOutletNode, ErrorsFound)

    IF (PltSizSteamNum > 0) THEN

      IF (CurZoneEqNum > 0) THEN

      IF (SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax == AutoSize) THEN
        IsAutosize = .TRUE.
      END IF
      IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
        IF (SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax > 0.0d0) THEN
          CALL ReportSizingOutput(cCMO_BBRadiator_Steam,SteamBaseboard(BaseboardNum)%EquipID,&
                               'User-Specified Maximum Water Flow Rate [m3/s]', &
                                SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax)
        END IF
      ELSE
        CALL CheckZoneSizing(cCMO_BBRadiator_Steam,SteamBaseboard(BaseboardNum)%EquipID)
        DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
        IF (DesCoilLoad >= SmallLoad) THEN
          SteamInletTemp  = 100.0d0
          EnthSteamInDry  = GetSatEnthalpyRefrig('STEAM',SteamInletTemp,1.0d0,  &
                            SteamBaseboard(BaseboardNum)%FluidIndex,'SizeSteamBaseboard')
          EnthSteamOutWet = GetSatEnthalpyRefrig('STEAM',SteamInletTemp,0.0d0,  &
                            SteamBaseboard(BaseboardNum)%FluidIndex,'SizeSteamBaseboard')
          LatentHeatSteam = EnthSteamInDry - EnthSteamOutWet
          SteamDensity    = GetSatDensityRefrig('STEAM',SteamInletTemp,1.0d0,  &
                            SteamBaseboard(BaseboardNum)%FluidIndex,'SizeSteamBaseboard')
          Cp = GetSatSpecificHeatRefrig('STEAM',SteamInletTemp,0.0d0,SteamBaseboard(BaseboardNum)%FluidIndex, &
                                            'SizeSteamBaseboard')

          SteamVolFlowRateMaxDes = DesCoilLoad / &
                                     (SteamDensity*(LatentHeatSteam + SteamBaseboard(BaseboardNum)%DegOfSubCooling * Cp))
        ELSE
          SteamVolFlowRateMaxDes = 0.0d0
        END IF

        IF (IsAutosize) THEN
          SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax = SteamVolFlowRateMaxDes
          CALL ReportSizingOutput(cCMO_BBRadiator_Steam,SteamBaseboard(BaseboardNum)%EquipID,&
                              'Design Size Maximum Steam Flow Rate [m3/s]',SteamVolFlowRateMaxDes)
        ELSE ! Hard size with sizing data
          IF (SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax > 0.0d0 .AND. SteamVolFlowRateMaxDes > 0.0d0) THEN
            SteamVolFlowRateMaxUser = SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax
            CALL ReportSizingOutput(cCMO_BBRadiator_Steam,SteamBaseboard(BaseboardNum)%EquipID,&
                                'Design Size Maximum Steam Flow Rate [m3/s]',SteamVolFlowRateMaxDes, &
                                'User-Speicified Maximum Steam Flow Rate [m3/s]',SteamVolFlowRateMaxUser)
            IF (DisplayExtraWarnings) THEN
                ! Report difference between design size and user-specified values
              IF ((ABS(SteamVolFlowRateMaxDes - SteamVolFlowRateMaxUser)/SteamVolFlowRateMaxUser) &
                          > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeSteamBaseboard: Potential issue with equipment sizing for ' &
                                    // 'ZoneHVAC:Baseboard:RadiantConvective:Steam="'//  &
                                    TRIM(SteamBaseboard(BaseboardNum)%EquipID)//'".')
                CALL ShowContinueError('User-Specified Maximum Steam Flow Rate of '// &
                                    TRIM(RoundSigDigits(SteamVolFlowRateMaxUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Steam Flow Rate of ' // &
                                    TRIM(RoundSigDigits(SteamVolFlowRateMaxDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    IF (IsAutosize) THEN
       ! if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
       ! first error will be issued by MyPlantSizingIndex
      CALL ShowSevereError('Autosizing of steam baseboard requires a heating loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Baseboard Heater='//TRIM(SteamBaseboard(BaseboardNum)%EquipID))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(SteamBaseboard(BaseboardNum)%SteamInletNode,SteamBaseboard(BaseboardNum)%SteamVolFlowRateMax)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeSteamBaseboard

  SUBROUTINE CalcSteamBaseboard(BaseboardNum, LoadMet)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   September 2009
          !       MODIFIED       Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates both the convective and radiant heat transfer rate
          ! of steam baseboard heaters. The heater is assumed to be crossflow with
          ! both fluids unmixed. The air flow is bouyancy driven and a constant airflow.

          ! METHODOLOGY EMPLOYED:
          ! Equations that calculates heating capacity of steam coils and outlet air and water temperatures
          ! of the zone control steam coil in steam coil module in EnergyPlus are employed.

          ! REFERENCES:

          ! USE STATEMENTS:
    USE ScheduleManager,       ONLY: GetCurrentScheduleValue
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
    USE FluidProperties,       ONLY: GetSatEnthalpyRefrig, GetSatDensityRefrig, GetSatSpecificHeatRefrig
    USE DataInterfaces, ONLY: CalcHeatBalanceOutsideSurf, CalcHeatBalanceInsideSurf
    USE DataHVACGlobals, ONLY: SmallLoad
    USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER                :: BaseboardNum
    REAL(r64), INTENT(OUT) :: LoadMet

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER   :: ZoneNum
    REAL(r64) :: RadHeat
    REAL(r64) :: SteamBBHeat
    REAL(r64) :: SteamInletTemp
    REAL(r64) :: SteamOutletTemp
    REAL(r64) :: SteamMassFlowRate
    REAL(r64) :: SubCoolDeltaT
    REAL(r64) :: QZnReq
    REAL(r64) :: EnthSteamInDry
    REAL(r64) :: EnthSteamOutWet
    REAL(r64) :: LatentHeatSteam
    REAL(r64) :: Cp

    ZoneNum = SteamBaseboard(BaseboardNum)%ZonePtr
    QZnReq  = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
    SteamInletTemp    = Node(SteamBaseboard(BaseboardNum)%SteamInletNode)%Temp
    SteamOutletTemp = SteamInletTemp
    SteamMassFlowRate = Node(SteamBaseboard(BaseboardNum)%SteamInletNode)%MassFlowRate
    SubCoolDeltaT     = SteamBaseboard(BaseboardNum)%DegOfSubCooling

    IF (QZnReq > SmallLoad &
        .AND. .NOT. CurDeadbandOrSetback(ZoneNum) &
        .AND. SteamMassFlowRate > 0.0d0 &
        .AND. GetCurrentScheduleValue(SteamBaseboard(BaseboardNum)%SchedPtr) > 0) THEN
       ! Unit is on
      EnthSteamInDry  = GetSatEnthalpyRefrig('STEAM',SteamInletTemp,1.0d0, &
                        SteamBaseboard(BaseboardNum)%FluidIndex,'CalcSteamBaseboard')
      EnthSteamOutWet = GetSatEnthalpyRefrig('STEAM',SteamInletTemp,0.0d0, &
                        SteamBaseboard(BaseboardNum)%FluidIndex,'CalcSteamBaseboard')
      LatentHeatSteam = EnthSteamInDry-EnthSteamOutWet
      Cp = GetSatSpecificHeatRefrig('STEAM',SteamInletTemp,0.0d0,SteamBaseboard(BaseboardNum)%FluidIndex, &
                                    'CalcSteamBaseboard')
      SteamBBHeat = SteamMassFlowRate*(LatentHeatSteam+SubCoolDeltaT*Cp) ! Baseboard heating rate
      SteamOutletTemp = SteamInletTemp - SubCoolDeltaT ! Outlet temperature of steam
        ! Estimate radiant heat addition
      RadHeat = SteamBBHeat * SteamBaseboard(BaseboardNum)%FracRadiant ! Radiant heating rate
      QBBSteamRadSource(BaseboardNum) = RadHeat ! Radiant heat source which will be distributed to surfaces and people

       ! Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
      CALL DistributeBBSteamRadGains
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

        ! Actual system load that the unit should meet
      LoadMet = (SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)) &
                + (SteamBBHeat * SteamBaseboard(BaseboardNum)%FracConvect) &
                + (RadHeat * SteamBaseboard(BaseboardNum)%FracDistribPerson)
      SteamBaseboard(BaseboardNum)%SteamOutletEnthalpy = SteamBaseboard(BaseboardNum)%SteamInletEnthalpy &
                                                          - SteamBBHeat / SteamMassFlowRate
      SteamBaseboard(BaseboardNum)%SteamOutletQuality  = 0.0d0
    ELSE
      SteamOutletTemp   = SteamInletTemp
      SteamBBHeat       = 0.0d0
      LoadMet           = 0.0d0
      RadHeat           = 0.0d0
      SteamMassFlowRate = 0.0d0
      QBBSteamRadSource(BaseboardNum) = 0.0d0
      SteamBaseboard(BaseboardNum)%SteamOutletQuality  = 0.0d0
      SteamBaseboard(BaseboardNum)%SteamOutletEnthalpy = SteamBaseboard(BaseboardNum)%SteamInletEnthalpy
    END IF

    SteamBaseboard(BaseboardNum)%SteamOutletTemp     = SteamOutletTemp
    SteamBaseboard(BaseboardNum)%SteamMassFlowRate   = SteamMassFlowRate
    SteamBaseboard(BaseboardNum)%SteamOutletEnthalpy = SteamBaseboard(BaseboardNum)%SteamOutletEnthalpy
    SteamBaseboard(BaseboardNum)%SteamOutletQuality  = SteamBaseboard(BaseboardNum)%SteamOutletQuality
    SteamBaseboard(BaseboardNum)%TotPower            = LoadMet
    SteamBaseboard(BaseboardNum)%Power               = SteamBBHeat
    SteamBaseboard(BaseboardNum)%ConvPower           = SteamBBHeat - RadHeat
    SteamBaseboard(BaseboardNum)%RadPower            = RadHeat

    RETURN

  END SUBROUTINE CalcSteamBaseboard


  SUBROUTINE UpdateSteamBaseboard(BaseboardNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !                      Rick Strand
          !       DATE WRITTEN   Nov 1997
          !                      February 2001
          !       MODIFIED       Sep 2009 Daeho Kang (add radiant component)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! The update subrotines both in high temperature radiant radiator
          ! and convective only baseboard radiator are combined and modified.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals,       ONLY: TimeStepZone, BeginEnvrnFlag
    USE DataEnvironment,   ONLY: StdBaroPress
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
    INTEGER      :: SteamInletNode
    INTEGER      :: SteamOutletNode
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
        QBBSteamRadSrcAvg(BaseboardNum) = QBBSteamRadSrcAvg(BaseboardNum) - LastQBBSteamRadSrc(BaseboardNum) * &
                                     LastTimeStepSys(BaseboardNum) / TimeStepZone
    END IF
          ! Update the running average and the "last" values with the current values of the appropriate variables
    QBBSteamRadSrcAvg(BaseboardNum) = QBBSteamRadSrcAvg(BaseboardNum) + &
                                    QBBSteamRadSource(BaseboardNum) * TimeStepSys / TimeStepZone

    LastQBBSteamRadSrc(BaseboardNum) = QBBSteamRadSource(BaseboardNum)
    LastSysTimeElapsed(BaseboardNum) = SysTimeElapsed
    LastTimeStepSys(BaseboardNum)    = TimeStepSys

    SteamInletNode  = SteamBaseboard(BaseboardNum)%SteamInletNode
    SteamOutletNode = SteamBaseboard(BaseboardNum)%SteamOutletNode

       ! Set the outlet air nodes of the Baseboard
       ! Set the outlet water nodes for the Coil
    CALL SafeCopyPlantNode(SteamInletNode, SteamOutletNode)
    Node(SteamOutletNode)%Temp         = SteamBaseboard(BaseboardNum)%SteamOutletTemp
    Node(SteamOutletNode)%Enthalpy     = SteamBaseboard(BaseboardNum)%SteamOutletEnthalpy


    RETURN

  END SUBROUTINE UpdateSteamBaseboard

  SUBROUTINE UpdateBBSteamRadSourceValAvg(SteamBaseboardSysOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       Aug 2009 Daeho Kang (modify only for baseboard)
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
  LOGICAL, INTENT(OUT) :: SteamBaseboardSysOn   ! .TRUE. if the radiant system has run this zone time step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: BaseboardNum    ! DO loop counter for surface index

       ! FLOW:
    SteamBaseboardSysOn = .FALSE.

       ! If this was never allocated, then there are no radiant systems in this input file (just RETURN)
    IF (.NOT.ALLOCATED(QBBSteamRadSrcAvg)) RETURN

       ! If it was allocated, then we have to check to see if this was running at all...
    DO BaseboardNum = 1, NumSteamBaseboards
      IF (QBBSteamRadSrcAvg(BaseboardNum) /= 0.0D0) THEN
          SteamBaseboardSysOn = .TRUE.
        EXIT !DO loop
      END IF
    END DO

       QBBSteamRadSource = QBBSteamRadSrcAvg

  CALL DistributeBBSteamRadGains  ! QBBRadSource has been modified so we need to redistribute gains

  RETURN

END SUBROUTINE UpdateBBSteamRadSourceValAvg

SUBROUTINE DistributeBBSteamRadGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       Aug. 2009 Daeho Kang (modify only for steam baseboard)
          !                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To distribute the gains from the steam basebaord heater
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
  USE DataHeatBalance,   ONLY : Zone
  USE DataHeatBalFanSys, ONLY : QSteamBaseboardToPerson, QSteamBaseboardSurf, MaxRadHeatFlux
  USE DataSurfaces,      ONLY : Surface, TotSurfaces
  USE General,           ONLY : RoundSigDigits
  USE DataInterfaces,    ONLY : ShowContinueError, ShowWarningError, ShowSevereError, &
                                ShowFatalError

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
    QSteamBaseboardSurf     = 0.0D0
    QSteamBaseboardToPerson = 0.0D0

    DO BaseboardNum = 1, NumSteamBaseboards

      ZoneNum = SteamBaseboard(BaseboardNum)%ZonePtr
      QSteamBaseboardToPerson(ZoneNum) = QSteamBaseboardToPerson(ZoneNum) + &
                                         QBBSteamRadSource(BaseboardNum) * SteamBaseboard(BaseboardNum)%FracDistribPerson

      DO RadSurfNum = 1, SteamBaseboard(BaseboardNum)%TotSurfToDistrib
         SurfNum = SteamBaseboard(BaseboardNum)%SurfacePtr(RadSurfNum)
        IF (Surface(SurfNum)%Area > SmallestArea) THEN
          ThisSurfIntensity =  (QBBSteamRadSource(BaseboardNum) &
                                        * SteamBaseboard(BaseboardNum)%FracDistribToSurf(RadSurfNum) &
                                        / Surface(SurfNum)%Area )
          QSteamBaseboardSurf(SurfNum) = QSteamBaseboardSurf(SurfNum)            &
                                        + ThisSurfIntensity

          IF (ThisSurfIntensity > MaxRadHeatFlux) THEN ! CR 8074, trap for excessive intensity (throws off surface balance )
            CALL ShowSevereError('DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected')
            CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
            CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
            CALL ShowContinueError('Occurs in '//cCMO_BBRadiator_Steam//' = '//Trim(SteamBaseboard(BaseboardNum)%EquipID))
            CALL ShowContinueError('Radiation intensity = '//Trim(RoundSigDigits(ThisSurfIntensity,2))//' [W/m2]')
            CALL ShowContinueError('Assign a larger surface area or more surfaces in '//cCMO_BBRadiator_Steam )
            CALL ShowFatalError('DistributeBBSteamRadGains:  excessive thermal radiation heat flux intensity detected')
          ENDIF
        ELSE ! small surface
          CALL ShowSevereError('DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux')
          CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
          CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
          CALL ShowContinueError('Occurs in '//cCMO_BBRadiator_Steam//' = '//Trim(SteamBaseboard(BaseboardNum)%EquipID))
          CALL ShowContinueError('Assign a larger surface area or more surfaces in '//cCMO_BBRadiator_Steam )
          CALL ShowFatalError('DistributeBBSteamRadGains:  surface not large enough to receive thermal radiation heat flux')

        ENDIF
      END DO

    END DO

  RETURN

END SUBROUTINE DistributeBBSteamRadGains


  SUBROUTINE ReportSteamBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
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

    SteamBaseboard(BaseboardNum)%TotEnergy  = SteamBaseboard(BaseboardNum)%TotPower  * TimeStepSys*SecInHour
    SteamBaseboard(BaseboardNum)%Energy     = SteamBaseboard(BaseboardNum)%Power     * TimeStepSys*SecInHour
    SteamBaseboard(BaseboardNum)%ConvEnergy = SteamBaseboard(BaseboardNum)%ConvPower * TimeStepSys*SecInHour
    SteamBaseboard(BaseboardNum)%RadEnergy  = SteamBaseboard(BaseboardNum)%RadPower  * TimeStepSys*SecInHour

    RETURN
  END SUBROUTINE ReportSteamBaseboard

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

SUBROUTINE UpdateSteamBaseboardPlantConnection(BaseboardTypeNum, &
                                               BaseboardName,&
                                               EquipFlowCtrl, &
                                               LoopNum,LoopSide,CompIndex,&
                                               FirstHVACIteration, InitLoopEquip)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   Sept. 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update sim routine called from plant

          ! METHODOLOGY EMPLOYED:
          ! check input, provide comp index, call utility routines

          ! REFERENCES:
          ! Based on UpdateBaseboardPlantConnection from Brent Griffith, Sept 2010

          ! USE STATEMENTS:
  USE PlantUtilities , ONLY: PullCompInterconnectTrigger
  USE DataPlant,       ONLY: ccSimPlantEquipTypes, TypeOf_Baseboard_Rad_Conv_Steam, &
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
  INTEGER, INTENT(IN)   :: BaseboardTypeNum  ! type index
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
    BaseboardNum = FindItemInList(BaseboardName,SteamBaseboard%EquipID,NumSteamBaseboards)
    IF (BaseboardNum == 0) THEN
      CALL ShowFatalError('UpdateSteamBaseboardPlantConnection: Specified baseboard not valid ='//TRIM(BaseboardName))
    ENDIF
    CompIndex=BaseboardNum
  ELSE
    BaseboardNum=CompIndex
    IF (BaseboardNum > NumSteamBaseboards .or. BaseboardNum < 1) THEN
      CALL ShowFatalError('UpdateSteamBaseboardPlantConnection:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(BaseboardNum))// &
                          ', Number of baseboards='//TRIM(TrimSigDigits(NumSteamBaseboards))//  &
                          ', Entered baseboard name='//TRIM(BaseboardName))
    ENDIF
    IF (KickOffSimulation) THEN
      IF (BaseboardName /= SteamBaseboard(BaseboardNum)%EquipID) THEN
        CALL ShowFatalError('UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', baseboard name='//TRIM(BaseboardName)//', stored baseboard Name for that index='//  &
                            TRIM(SteamBaseboard(BaseboardNum)%EquipID))
      ENDIF
      IF (BaseboardTypeNum /= TypeOf_Baseboard_Rad_Conv_Steam) THEN
        CALL ShowFatalError('UpdateSteamBaseboardPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', baseboard name='//TRIM(BaseboardName)//', stored baseboard Name for that index='//  &
                            TRIM(ccSimPlantEquipTypes(BaseboardTypeNum)) )
      ENDIF
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    RETURN
  END IF

  CALL PullCompInterconnectTrigger(SteamBaseboard(BaseboardNum)%LoopNum,     &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   SteamBaseboard(BaseboardNum)%BranchNum,   &
                                   SteamBaseboard(BaseboardNum)%CompNum,     &
                                   SteamBaseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   SteamBaseboard(BaseboardNum)%LoopNum,     &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_HeatTransferRate,         &
                                   SteamBaseboard(BaseboardNum)%Power)

  CALL PullCompInterconnectTrigger(SteamBaseboard(BaseboardNum)%LoopNum,     &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   SteamBaseboard(BaseboardNum)%BranchNum,   &
                                   SteamBaseboard(BaseboardNum)%CompNum,     &
                                   SteamBaseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   SteamBaseboard(BaseboardNum)%LoopNum,     &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_MassFlowRate,         &
                                   SteamBaseboard(BaseboardNum)%SteamMassFlowRate)

  CALL PullCompInterconnectTrigger(SteamBaseboard(BaseboardNum)%LoopNum,     &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   SteamBaseboard(BaseboardNum)%BranchNum,   &
                                   SteamBaseboard(BaseboardNum)%CompNum,     &
                                   SteamBaseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   SteamBaseboard(BaseboardNum)%LoopNum,     &
                                   SteamBaseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_Temperature,         &
                                   SteamBaseboard(BaseboardNum)%SteamOutletTemp)
  RETURN

END SUBROUTINE UpdateSteamBaseboardPlantConnection

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

END MODULE SteamBaseboardRadiator