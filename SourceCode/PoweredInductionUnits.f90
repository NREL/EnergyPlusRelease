MODULE PoweredInductionUnits

  ! Module containing routines dealing with Series and Parallel fan powered terminal boxes

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   August 2000
  !       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms needed to simulate Series and Parallel
  ! fan powered induction terminal boxes.

  ! METHODOLOGY EMPLOYED:
  ! The terminal boxes are modeled as a collection of components: air mixer,
  ! fan, and heating coil plus an integrated control
  ! algorithm that adjusts the primary air flow and the heating coil output
  ! to meet the zone load.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, SecInHour, NumOfZones, &
                           InitConvTemp, SysSizingCalc, ScheduleAlwaysOn, DisplayExtraWarnings
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, FanElecPower, SmallTempDiff, SmallAirVolFlow, SingleCoolingSetPoint, &
                           SingleHeatingSetPoint, PlenumInducedMassFlow
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHFnTdbW
USE SteamCoils, ONLY:SimulateSteamCoilComponents
USE FluidProperties
USE DataHeatBalFanSys, ONLY: TempControlType


IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: SingleDuct_SeriesPIU_Reheat=6
INTEGER, PARAMETER :: SingleDuct_ParallelPIU_Reheat=7
! coil types in this module
INTEGER, PARAMETER :: HCoilType_Gas = 1
INTEGER, PARAMETER :: HCoilType_Electric = 2
INTEGER, PARAMETER :: HCoilType_SimpleHeating = 3
INTEGER, PARAMETER :: HCoilType_SteamAirHeating = 4

  ! DERIVED TYPE DEFINITIONS
TYPE PowIndUnitData
  ! input data
  CHARACTER(len=MaxNameLength) :: Name               =' '  ! name of unit
  CHARACTER(len=MaxNameLength) :: UnitType           =' '  ! type of unit
  INTEGER                      :: UnitType_Num       = 0   ! index for type of unit
  CHARACTER(len=MaxNameLength) :: Sched              =' '  ! availability schedule
  INTEGER                      :: SchedPtr           =0    ! index to schedule
  REAL(r64)                    :: MaxTotAirVolFlow   =0.0d0  ! m3/s  (series)
  REAL(r64)                    :: MaxTotAirMassFlow  =0.0d0  ! kg/s  (series)
  REAL(r64)                    :: MaxPriAirVolFlow   =0.0d0  ! m3/s
  REAL(r64)                    :: MaxPriAirMassFlow  =0.0d0  ! kg/s
  REAL(r64)                    :: MinPriAirFlowFrac  =0.0d0  ! minimum primary air flow fraction
  REAL(r64)                    :: MinPriAirMassFlow  =0.0d0  ! kg/s
  REAL(r64)                    :: MaxSecAirVolFlow   =0.0d0  ! m3/s (parallel)
  REAL(r64)                    :: MaxSecAirMassFlow  =0.0d0  ! kg/s (parallel)
  REAL(r64)                    :: FanOnFlowFrac      =0.0d0  ! frac of primary air flow at which fan turns on (parallel)
  REAL(r64)                    :: FanOnAirMassFlow   =0.0d0  ! primary air mass flow rate at which fan turns on (parallel)
  INTEGER                      :: PriAirInNode       =0    ! unit primary air inlet node number
  INTEGER                      :: SecAirInNode       =0    ! unit secondary air inlet node number
  INTEGER                      :: OutAirNode         =0    ! unit air outlet node number
  INTEGER                      :: HCoilInAirNode     =0    ! unit mixed air node number
  INTEGER                      :: ControlCompTypeNum =0
  INTEGER                      :: CompErrIndex       =0
  CHARACTER(len=MaxNameLength) :: MixerName          =' '  ! name of air mixer component
  INTEGER                      :: Mixer_Num          =0    ! index for type of mixer
  CHARACTER(len=MaxNameLength) :: FanName            =' '  ! name of fan component
  INTEGER                      :: Fan_Num            =0    ! index for fan type
  INTEGER                      :: Fan_Index          =0    ! store index for this fan
  CHARACTER(len=MaxNameLength) :: HCoilType          =' '  ! type of heating coil component
  INTEGER                      :: HCoilType_Num      = 0   ! index for heating coil type
  INTEGER                      :: HCoil_PlantTypeNum =0  !
  CHARACTER(len=MaxNameLength) :: HCoil              =' '  ! name of heating coil component
  INTEGER                      :: HCoil_Index        =0    ! index to this heating coil
  INTEGER                      :: HCoil_FluidIndex   =0
  REAL(r64)                    :: MaxVolHotWaterFlow =0.0d0  ! m3/s
  REAL(r64)                    :: MaxVolHotSteamFlow =0.0d0   ! m3/s
  REAL(r64)                    :: MaxHotWaterFlow    =0.0d0   ! kg/s
  REAL(r64)                    :: MaxHotSteamFlow    =0.0d0   ! kg/s
  REAL(r64)                    :: MinVolHotWaterFlow =0.0d0   ! m3/s
  REAL(r64)                    :: MinHotSteamFlow    =0.0d0   ! kg/s
  REAL(r64)                    :: MinVolHotSteamFlow =0.0d0   ! m3/s
  REAL(r64)                    :: MinHotWaterFlow    =0.0d0   ! kg/s
  INTEGER                      :: HotControlNode     =0     ! hot water control node
  INTEGER                      :: HotCoilOutNodeNum  =0   ! outlet of coil
  REAL(r64)                    :: HotControlOffset   =0.0d0   ! control tolerance
  INTEGER                      :: HWLoopNum          =0   ! index for plant loop with hot plant coil
  INTEGER                      :: HWLoopSide         =0   ! index for plant loop side for hot plant coil
  INTEGER                      :: HWBranchNum        =0   ! index for plant branch for hot plant coil
  INTEGER                      :: HWCompNum          =0   ! index for plant component for hot plant coil

  INTEGER                      :: ADUNum             =0     ! index of corresponding air distribution unit
  LOGICAL                      :: InducesPlenumAir   =.FALSE.  ! True if secondary air comes from the plenum
  ! Report data
  REAL(r64)                    :: HeatingRate        =0.0d0   ! unit heat addition rate to zone [W]
  REAL(r64)                    :: HeatingEnergy      =0.0d0   ! unit heat addition to zone [J]
  REAL(r64)                    :: SensCoolRate       =0.0d0   ! unit sensible heat removal rate from zone [W]
  REAL(r64)                    :: SensCoolEnergy     =0.0d0   ! unit sensible heat removal from zone [J]
END TYPE PowIndUnitData

  ! MODULE VARIABLE DECLARATIONS:
TYPE (PowIndUnitData), ALLOCATABLE, DIMENSION(:)         :: PIU
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL :: GetPIUInputFlag = .TRUE.  ! First time, input is "gotten"

INTEGER :: NumPIUs=0
INTEGER :: NumSeriesPIUs=0
INTEGER :: NumParallelPIUs=0

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

PUBLIC  SimPIU
PRIVATE GetPIUs
PRIVATE InitPIU
PRIVATE SizePIU
PRIVATE CalcSeriesPIU
PRIVATE CalcParallelPIU
! PRIVATE UpdatePIU
PRIVATE ReportPIU
PUBLIC  PIUnitHasMixer
PUBLIC  PIUInducesPlenumAir

CONTAINS

SUBROUTINE SimPIU(CompName, FirstHVACIteration, ZoneNum, ZoneNodeNum,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a fan powered induction terminal unit.
          ! Called from SimZoneAirLoopEquipmentin module ZoneAirLoopEquipmentManager.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DataSizing, ONLY: TermUnitPIU
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName    ! name of the PIU
  LOGICAL, INTENT (IN)         :: FirstHVACIteration ! TRUE if first HVAC iteration in time step
  INTEGER, INTENT (IN)         :: ZoneNum     ! index of zone served by PIU
  INTEGER, INTENT (IN)         :: ZoneNodeNum ! zone node number of zone served by PIU
  INTEGER, INTENT (INOUT)      :: CompIndex   ! PIU Index in PIU names

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: PIUNum                 ! index of powered induction unit being simulated

          ! FLOW

! First time SimPIU is called, get the input for all the fan coil units
IF (GetPIUInputFlag) THEN
  CALL GetPIUs
  GetPIUInputFlag = .FALSE.
END IF

! Get the powered induction unit index
IF (CompIndex == 0) THEN
  PIUNum = FindItemInList(CompName,PIU%Name,NumPIUs)
  IF (PIUNum == 0) THEN
    CALL ShowFatalError('SimPIU: PIU Unit not found='//TRIM(CompName))
  ENDIF
  CompIndex=PIUNum
ELSE
  PIUNum=CompIndex
  IF (PIUNum > NumPIUs .or. PIUNum < 1) THEN
    CALL ShowFatalError('SimPIU: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                        ', Number of PIU Units='//TRIM(TrimSigDigits(NumPIUs))//', PIU Unit name='//TRIM(CompName))
  ENDIF
  IF (CheckEquipName(PIUNum)) THEN
    IF (CompName /= PIU(PIUNum)%Name) THEN
      CALL ShowFatalError('SimPIU: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                          ', PIU Unit name='//TRIM(CompName)//', stored PIU Unit Name for that index='//TRIM(PIU(PIUNum)%Name))
    ENDIF
    CheckEquipName(PIUNum)=.false.
  ENDIF
ENDIF

! initialize the unit
CALL InitPIU(PIUNum,FirstHVACIteration)

TermUnitPIU = .TRUE.

! Select the correct unit type
SELECT CASE(PIU(PIUNum)%UnitType_Num)

  CASE (SingleDuct_SeriesPIU_Reheat) !  'AirTerminal:SingleDuct:SeriesPIU:Reheat'

    CALL CalcSeriesPIU(PIUNum,ZoneNum,ZoneNodeNum,FirstHVACIteration)

  CASE (SingleDuct_ParallelPIU_Reheat) ! 'AirTerminal:SingleDuct:ParallelPIU:Reheat'

    CALL CalcParallelPIU(PIUNum,ZoneNum,ZoneNodeNum,FirstHVACIteration)

  CASE DEFAULT
    CALL ShowSevereError('Illegal PI Unit Type used='//TRIM(PIU(PIUNum)%UnitType))
    CALL ShowContinueError('Occurs in PI Unit='//TRIM(PIU(PIUNum)%Name))
    CALL ShowFatalError('Preceding condition causes termination.')


END SELECT

TermUnitPIU = .FALSE.

! Update the current unit's outlet nodes
  ! no update needed: reheat coil updates outlet node; inlet nodes' mass flow rate set by Calc routine

! Fill the report variables
CALL ReportPIU(PIUNum)

RETURN
END SUBROUTINE SimPIU

SUBROUTINE GetPIUs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for powered induction unit terminal boxes and stores it
          ! in PIU data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE FluidProperties, ONLY: FindRefrigerant
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
  USE DataDefineEquip,   ONLY: AirDistUnit, NumAirDistUnits
  USE DataIPShortCuts
  USE DataPlant,  ONLY: TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating
  USE WaterCoils, ONLY: GetCoilWaterInletNode
  USE SteamCoils, ONLY: GetCoilSteamInletNode

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
  INTEGER :: PIUIndex            ! loop index
  INTEGER :: PIUNum              ! current fan coil number
  INTEGER :: NumAlphas           ! Number of Alpha input fields for each GetObjectItem call
  INTEGER :: NumNumbers          ! Number of Numeric input fields for each GetObjectItem call
  INTEGER :: IOStatus            ! Used in GetObjectItem
  LOGICAL :: ErrorsFound=.false. ! Set to true if errors in input, fatal at end of routine
  LOGICAL :: IsNotOK             ! Flag to verify name
  LOGICAL :: IsBlank             ! Flag for blank name
  INTEGER :: CtrlZone            ! controlled zome do loop index
  INTEGER :: SupAirIn            ! controlled zone supply air inlet index
  LOGICAL :: AirNodeFound
  INTEGER :: ADUNum
  CHARACTER(len=*), PARAMETER :: RoutineName='GetPIUs: ' ! include trailing blank space
  LOGICAL :: SteamMessageNeeded

          ! FLOW
 ! find the number of each type of fan coil unit
SteamMessageNeeded=.true.
NumSeriesPIUs = GetNumObjectsFound('AirTerminal:SingleDuct:SeriesPIU:Reheat')
NumParallelPIUs = GetNumObjectsFound('AirTerminal:SingleDuct:ParallelPIU:Reheat')
NumPIUs = NumSeriesPIUs + NumParallelPIUs
! allocate the data structures
ALLOCATE(PIU(NumPIUs))
ALLOCATE(CheckEquipName(NumPIUs))
CheckEquipName=.true.

! loop over Series PIUs; get and load the input data
DO PIUIndex = 1,NumSeriesPIUs

  cCurrentModuleObject = 'AirTerminal:SingleDuct:SeriesPIU:Reheat'

  CALL GetObjectItem(cCurrentModuleObject,PIUIndex,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                     NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  PIUNum = PIUIndex
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(cAlphaArgs(1),PIU%Name,PIUNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  PIU(PIUNum)%Name     = cAlphaArgs(1)
  PIU(PIUNum)%UnitType = TRIM(cCurrentModuleObject)
  PIU(PIUNum)%UnitType_Num = SingleDuct_SeriesPIU_Reheat
  PIU(PIUNum)%Sched    = cAlphaArgs(2)
  IF (lAlphaFieldBlanks(2)) THEN
    PIU(PIUNum)%SchedPtr = ScheduleAlwaysOn
  ELSE
    PIU(PIUNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))  ! convert schedule name to pointer
    IF (PIU(PIUNum)%SchedPtr .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
                            ' entered ='//TRIM(cAlphaArgs(2))// &
                            ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.TRUE.
    END IF
  END IF

  PIU(PIUNum)%MaxTotAirVolFlow  = rNumericArgs(1)
  PIU(PIUNum)%MaxPriAirVolFlow  = rNumericArgs(2)
  PIU(PIUNum)%MinPriAirFlowFrac = rNumericArgs(3)

  PIU(PIUNum)%HCoilType = cAlphaArgs(9) ! type (key) of heating coil
  IF (SameString(cAlphaArgs(9),'COIL:HEATING:WATER')) THEN
    PIU(PIUNum)%HCoilType_Num = HCoilType_SimpleHeating
    PIU(PIUNum)%HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating
  ELSEIF (SameString(cAlphaArgs(9),'COIL:HEATING:GAS')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_Gas
  ELSEIF (SameString(cAlphaArgs(9),'COIL:HEATING:STEAM')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_SteamAirHeating
    PIU(PIUNum)%HCoil_PlantTypeNum=TypeOf_CoilSteamAirHeating
    PIU(PIUNum)%HCoil_FluidIndex=FindRefrigerant('Steam')
    IF (PIU(PIUNum)%HCoil_FluidIndex == 0) THEN
      CALL ShowSevereError(RoutineName//'Steam Properties for '//TRIM(cAlphaArgs(1))// &
                           ' not found.')
      IF (SteamMessageNeeded) CALL ShowContinueError('Steam Fluid Properties should have been included in the input file.')
      ErrorsFound=.true.
      SteamMessageNeeded=.false.
    ENDIF
  ELSEIF (SameString(cAlphaArgs(9),'COIL:HEATING:ELECTRIC')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_Electric
  ELSE
    CALL ShowSevereError('Illegal '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(cAlphaArgs(9)))
    CALL ShowContinueError('Occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(PIU(PIUNum)%Name))
    ErrorsFound=.TRUE.
  ENDIF

  PIU(PIUNum)%PriAirInNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,PIU(PIUNum)%UnitType,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFieldNames(3))

  PIU(PIUNum)%SecAirInNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,PIU(PIUNum)%UnitType,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFieldNames(4))

  PIU(PIUNum)%OutAirNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,PIU(PIUNum)%UnitType,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent,cAlphaFieldNames(5))

  PIU(PIUNum)%HCoilInAirNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,PIU(PIUNum)%UnitType,cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent,cAlphaFieldNames(6))
  ! The reheat coil control node is necessary for hot water reheat, but not necessary for
  ! electric or gas reheat.
  IF (PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Gas .OR. PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Electric) THEN
    IF(.NOT. lAlphaFieldBlanks(11)) THEN
      CALL ShowWarningError('In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
                             // ' the '//TRIM(cAlphaFieldNames(11))//' is not needed and will be ignored.')
      CALL ShowContinueError('  It is used for hot water reheat coils only.')
    END IF
  ELSE
    IF(lAlphaFieldBlanks(11)) THEN
      CALL ShowSevereError('In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
                           // ' the '//TRIM(cAlphaFieldNames(11))//' is undefined.')
      ErrorsFound=.TRUE.
    END IF
    PIU(PIUNum)%HotControlNode  = &
      GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent,cAlphaFieldNames(11))
  END IF
  PIU(PIUNum)%MixerName = cAlphaArgs(7)  ! name of zone mixer object
  PIU(PIUNum)%FanName   = cAlphaArgs(8)  ! name of fan object
  PIU(PIUNum)%HCoil     = cAlphaArgs(10) ! name of heating coil object
  CALL ValidateComponent(PIU(PIUNum)%HCoilType,PIU(PIUNum)%HCoil,IsNotOK,TRIM(cCurrentModuleObject)//' - Heating Coil')
  IF (IsNotOK) THEN
    CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//' = '//TRIM(PIU(PIUNum)%Name))
    ErrorsFound=.TRUE.
  ENDIF
  PIU(PIUNum)%MaxVolHotWaterFlow = rNumericArgs(4)
  PIU(PIUNum)%MinVolHotWaterFlow = rNumericArgs(5)
  PIU(PIUNum)%HotControlOffset   = rNumericArgs(6)
  ! Set default convergence tolerance
  IF (PIU(PIUNum)%HotControlOffset .LE. 0.0d0) THEN
    PIU(PIUNum)%HotControlOffset = 0.001d0
  END IF

  ! Add fan to component sets array
  CALL SetUpCompSets(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                     'UNDEFINED',cAlphaArgs(8),'UNDEFINED',cAlphaArgs(6))

  ! Add reheat coil to component sets array
  CALL SetUpCompSets(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                     cAlphaArgs(9),cAlphaArgs(10),cAlphaArgs(6),cAlphaArgs(5))

  ! Register component set data
  CALL TestCompSet(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name, &
                   NodeID(PIU(PIUNum)%PriAirInNode),NodeID(PIU(PIUNum)%OutAirNode),'Air Nodes')

  ! Fill the Zone Equipment data with the supply air inlet node number of this unit.
  AirNodeFound=.false.
  DO CtrlZone = 1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
    DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
      IF (PIU(PIUNum)%OutAirNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
        ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = PIU(PIUNum)%PriAirInNode
        ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = PIU(PIUNum)%OutAirNode
        AirNodeFound=.TRUE.
        EXIT
      END IF
    END DO
  END DO
  IF (.not. AirNodeFound) THEN
    CALL ShowSevereError('The outlet air node from the '//TRIM(cCurrentModuleObject)//' Unit = '//TRIM(PIU(PIUNum)%Name))
    CALL ShowContinueError('did not have a matching Zone Equipment Inlet Node, Node = '//TRIM(cAlphaArgs(5)))
    ErrorsFound=.TRUE.
  ENDIF

END DO

DO PIUIndex = 1,NumParallelPIUs

  cCurrentModuleObject = 'AirTerminal:SingleDuct:ParallelPIU:Reheat'

  CALL GetObjectItem(cCurrentModuleObject,PIUIndex,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                     NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  PIUNum = PIUIndex + NumSeriesPIUs
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(cAlphaArgs(1),PIU%Name,PIUNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  PIU(PIUNum)%Name     = cAlphaArgs(1)
  PIU(PIUNum)%UnitType = TRIM(cCurrentModuleObject)
  PIU(PIUNum)%UnitType_Num = SingleDuct_ParallelPIU_Reheat
  PIU(PIUNum)%Sched    = cAlphaArgs(2)
  IF (lAlphaFieldBlanks(2)) THEN
    PIU(PIUNum)%SchedPtr = ScheduleAlwaysOn
  ELSE
    PIU(PIUNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))  ! convert schedule name to pointer
    IF (PIU(PIUNum)%SchedPtr .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
                            ' entered ='//TRIM(cAlphaArgs(2))// &
                            ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.TRUE.
    END IF
  END IF
  PIU(PIUNum)%MaxPriAirVolFlow  = rNumericArgs(1)
  PIU(PIUNum)%MaxSecAirVolFlow  = rNumericArgs(2)
  PIU(PIUNum)%MinPriAirFlowFrac = rNumericArgs(3)
  PIU(PIUNum)%FanOnFlowFrac     = rNumericArgs(4)
  PIU(PIUNum)%HCoilType         = cAlphaArgs(9) ! type (key) of heating coil
  IF (SameString(cAlphaArgs(9),'COIL:HEATING:WATER')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_SimpleHeating
    PIU(PIUNum)%HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating
  ELSEIF (SameString(cAlphaArgs(9),'COIL:HEATING:GAS')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_Gas
  ELSEIF (SameString(cAlphaArgs(9),'COIL:HEATING:STEAM')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_SteamAirHeating
    PIU(PIUNum)%HCoil_PlantTypeNum=TypeOf_CoilSteamAirHeating
    PIU(PIUNum)%HCoil_FluidIndex=FindRefrigerant('Steam')
    IF (PIU(PIUNum)%HCoil_FluidIndex == 0) THEN
      CALL ShowSevereError(RoutineName//'Steam Properties for '//TRIM(cAlphaArgs(1))// &
                           ' not found.')
      IF (SteamMessageNeeded) CALL ShowContinueError('Steam Fluid Properties should have been included in the input file.')
      ErrorsFound=.true.
      SteamMessageNeeded=.false.
    ENDIF
  ELSEIF (SameString(cAlphaArgs(9),'COIL:HEATING:ELECTRIC')) THEN
    PIU(PIUNum)%HCoilType_Num=HCoilType_Electric
  ELSE
    CALL ShowSevereError('Illegal '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(cAlphaArgs(9)))
    CALL ShowContinueError('Occurs in '//TRIM(cCurrentModuleObject)//' = '//TRIM(PIU(PIUNum)%Name))
    ErrorsFound=.TRUE.
  ENDIF

  PIU(PIUNum)%PriAirInNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFieldNames(3))

  PIU(PIUNum)%SecAirInNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFieldNames(4))

  PIU(PIUNum)%OutAirNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent,cAlphaFieldNames(5))

  PIU(PIUNum)%HCoilInAirNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent,cAlphaFieldNames(6))
  ! The reheat coil control node is necessary for hot water reheat, but not necessary for
  ! electric or gas reheat.
!  IF (PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Gas .OR. PIU(PIUNum)%HCoilType_Num .EQ. HCoilType_Electric) THEN
!    IF(cAlphaArgs(11) /= '') THEN
!      CALL ShowWarningError('In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
!                             // ' the '//TRIM(cAlphaFieldNames(11))//' is not needed and will be ignored.')
!      CALL ShowContinueError('  It is used for hot water reheat coils only.')
!    END IF
!  ELSE
!    IF(cAlphaArgs(11) == '') THEN
!      CALL ShowSevereError('In '//TRIM(cCurrentModuleObject)//' = ' // TRIM(PIU(PIUNum)%Name) &
!                           // ' the '//TRIM(cAlphaFieldNames(11))//' is undefined.')
!      ErrorsFound=.true.
!    END IF
!    PIU(PIUNum)%HotControlNode  = &
!      GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
!                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
!  END IF
  IF (PIU(PIUNum)%HCoilType_Num == HCoilType_SimpleHeating) THEN
    PIU(PIUNum)%HotControlNode  = GetCoilWaterInletNode(cAlphaArgs(9),cAlphaArgs(10),ErrorsFound)
  ENDIF
  IF (PIU(PIUNum)%HCoilType_Num == HCoilType_SteamAirHeating) THEN
    PIU(PIUNum)%HotControlNode  = GetCoilSteamInletNode(cAlphaArgs(9),cAlphaArgs(10),ErrorsFound)
  ENDIF
  PIU(PIUNum)%MixerName = cAlphaArgs(7) ! name of zone mixer object
  PIU(PIUNum)%FanName = cAlphaArgs(8)   ! name of fan object
  PIU(PIUNum)%HCoil = cAlphaArgs(10)    ! name of heating coil object
  CALL ValidateComponent(PIU(PIUNum)%HCoilType,PIU(PIUNum)%HCoil,IsNotOK,TRIM(cCurrentModuleObject)//' - Heating Coil')
  IF (IsNotOK) THEN
    CALL ShowContinueError('In '//TRIM(cCurrentModuleObject)//' = '//TRIM(PIU(PIUNum)%Name))
    ErrorsFound=.true.
  ENDIF
  PIU(PIUNum)%MaxVolHotWaterFlow = rNumericArgs(5)
  PIU(PIUNum)%MinVolHotWaterFlow = rNumericArgs(6)
  PIU(PIUNum)%HotControlOffset   = rNumericArgs(7)
  ! Set default convergence tolerance
  IF (PIU(PIUNum)%HotControlOffset .LE. 0.0d0) THEN
    PIU(PIUNum)%HotControlOffset = 0.001d0
  END IF

  ! Add fan to component sets array
  CALL SetUpCompSets(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                     'UNDEFINED',cAlphaArgs(8),cAlphaArgs(4),'UNDEFINED')

  ! Add reheat coil to component sets array
  CALL SetUpCompSets(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                     cAlphaArgs(9),cAlphaArgs(10),cAlphaArgs(6),cAlphaArgs(5))

  ! Register component set data
  CALL TestCompSet(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name, &
                   NodeID(PIU(PIUNum)%PriAirInNode),NodeID(PIU(PIUNum)%OutAirNode),'Air Nodes')

  ! Fill the Zone Equipment data with the supply air inlet node number of this unit.
  AirNodeFound=.false.
  DO CtrlZone = 1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
    DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
      IF (PIU(PIUNum)%OutAirNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
        ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = PIU(PIUNum)%PriAirInNode
        ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = PIU(PIUNum)%OutAirNode
        AirNodeFound=.true.
      END IF
    END DO
  END DO
  IF (.not. AirNodeFound) THEN
    CALL ShowSevereError('The outlet air node from the '//TRIM(cCurrentModuleObject)//' Unit = '//TRIM(PIU(PIUNum)%Name))
    CALL ShowContinueError('did not have a matching Zone Equipment Inlet Node, Node = '//TRIM(cAlphaArgs(5)))
    ErrorsFound=.true.
  ENDIF

END DO

DO PIUNum=1,NumPIUs
  DO ADUNum = 1,NumAirDistUnits
    IF (PIU(PIUNum)%OutAirNode == AirDistUnit(ADUNum)%OutletNodeNum) THEN
!      AirDistUnit(ADUNum)%InletNodeNum = PIU(PIUNum)%InletNodeNum
      PIU(PIUNum)%ADUNum = ADUNum
    END IF
  END DO
  ! one assumes if there isn't one assigned, it's an error?
  IF (PIU(PIUNum)%ADUNum == 0) THEN
    CALL ShowSevereError(RoutineName//'No matching Air Distribution Unit, for PIU = ['//  &
       TRIM(PIU(PIUNum)%UnitType)//','//TRIM(PIU(PIUNum)%Name)//'].')
    CALL ShowContinueError('...should have outlet node = '//TRIM(NodeID(PIU(PIUNum)%OutAirNode)))
!          ErrorsFound=.true.
  ENDIF
END DO

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in getting input.  Preceding conditions cause termination.')
END IF

Do PIUNum=1,NumPIUs
  ! Setup Report variables for the Fan Coils
  CALL SetupOutputVariable('Zone Air Terminal Heating Rate [W]',PIU(PIUNum)%HeatingRate,'System','Average',&
                           PIU(PIUNum)%Name)
  CALL SetupOutputVariable('Zone Air Terminal Heating Energy [J]',PIU(PIUNum)%HeatingEnergy,'System','Sum',&
                           PIU(PIUNum)%Name)
  CALL SetupOutputVariable('Zone Air Terminal Sensible Cooling Rate [W]',PIU(PIUNum)%SensCoolRate,'System','Average',&
                           PIU(PIUNum)%Name)
  CALL SetupOutputVariable('Zone Air Terminal Sensible Cooling Energy [J]',PIU(PIUNum)%SensCoolEnergy,'System','Sum',&
                           PIU(PIUNum)%Name)

END DO

RETURN
END SUBROUTINE GetPIUs

SUBROUTINE InitPIU(PIUNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the powered induction unit
          ! terminal boxe.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE DataDefineEquip,   ONLY: AirDistUnit
  USE DataPlant,         ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                               TypeOf_CoilSteamAirHeating
  USE PlantUtilities,    ONLY: InitComponentNodes
  USE DataGlobals,       ONLY: AnyPlantInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: PIUNum ! number of the current fan coil unit being simulated
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! TRUE if first zone equip this HVAC step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PriNode         ! primary air inlet node number
  INTEGER             :: SecNode         ! secondary air inlet node number
  INTEGER             :: HotConNode      ! hot water control node number in PIU
  INTEGER             :: OutletNode      ! unit air outlet node number
  REAL(r64)           :: RhoAir          ! air density at outside pressure and standard temperature and humidity
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop  ! Loop checking control variable
  REAL(r64)           :: rho  !local plant fluid density
  LOGICAL             :: errFlag

          ! FLOW:
! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumPIUs))
    ALLOCATE(MySizeFlag(NumPIUs))
    ALLOCATE(MyPlantScanFlag(NumPIUs))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .false.

  END IF

  IF (MyPlantScanFlag(PIUNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ((PIU(PIUNum)%HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) .OR. &
        (PIU(PIUNum)%HCoil_PlantTypeNum == TypeOf_CoilSteamAirHeating) ) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(   PIU(PIUNum)%HCoil, &
                                      PIU(PIUNum)%HCoil_PlantTypeNum, &
                                      PIU(PIUNum)%HWLoopNum, &
                                      PIU(PIUNum)%HWLoopSide, &
                                      PIU(PIUNum)%HWBranchNum, &
                                      PIU(PIUNum)%HWCompNum,  &
                                      errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitPIU: Program terminated due to previous condition(s).')
      ENDIF
      PIU(PIUNum)%HotCoilOutNodeNum = &
            PlantLoop(PIU(PIUNum)%HWLoopNum)%LoopSide(PIU(PIUNum)%HWLoopSide) &
                         %Branch(PIU(PIUNum)%HWBranchNum)%Comp(PIU(PIUNum)%HWCompNum)%NodeNumOut
    ENDIF
    MyPlantScanFlag(PIUNum) = .FALSE.
  ELSEIF (MyPlantScanFlag(PIUNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(PIUNum) = .FALSE.
  ENDIF

  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    ! Check to see if there is a Air Distribution Unit on the Zone Equipment List
    DO Loop=1,NumPIUs
      IF (PIU(Loop)%ADUNum == 0) CYCLE
      IF (CheckZoneEquipmentList('ZoneHVAC:AirDistributionUnit',AirDistUnit(PIU(Loop)%ADUNum)%Name)) CYCLE
      CALL ShowSevereError('InitPIU: ADU=[Air Distribution Unit,'//  &
           TRIM(AirDistUnit(PIU(Loop)%ADUNum)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.')
      CALL ShowContinueError('...PIU=['//TRIM(PIU(Loop)%UnitType)//','//TRIM(PIU(Loop)%Name)//  &
           '] will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(PIUNum) .AND. .NOT. MyPlantScanFlag(PIUNum)) THEN

    CALL SizePIU(PIUNum)

    HotConNode = PIU(PIUNum)%HotControlNode
    IF (HotConNode.GT.0) THEN
     !plant upgrade note? why no separate handling of steam coil? add it ?
      rho = GetDensityGlycol( PlantLoop(PIU(PIUNum)%HWLoopNum)%FluidName, &
                             60.d0, &
                             PlantLoop(PIU(PIUNum)%HWLoopNum)%FluidIndex, &
                             'InitPIU')

      PIU(PIUNum)%MaxHotWaterFlow = rho * PIU(PIUNum)%MaxVolHotWaterFlow
      PIU(PIUNum)%MinHotWaterFlow = rho * PIU(PIUNum)%MinVolHotWaterFlow
      CALL InitComponentNodes ( PIU(PIUNum)%MinHotWaterFlow, &
                                PIU(PIUNum)%MaxHotWaterFlow, &
                                PIU(PIUNum)%HotControlNode, &
                                PIU(PIUNum)%HotCoilOutNodeNum, &
                                PIU(PIUNum)%HWLoopNum, &
                                PIU(PIUNum)%HWLoopSide, &
                                PIU(PIUNum)%HWBranchNum, &
                                PIU(PIUNum)%HWCompNum )

    END IF

    MySizeFlag(PIUNum) = .FALSE.
  END IF

! Do the Begin Environment initializations
IF (BeginEnvrnFlag .and. MyEnvrnFlag(PIUNum)) THEN
  RhoAir = StdRhoAir
  PriNode = PIU(PIUNum)%PriAirInNode
  SecNode = PIU(PIUNum)%SecAirInNode
  OutletNode = PIU(PIUNum)%OutAirNode
  ! set the mass flow rates from the input volume flow rates
  IF (PIU(PIUNum)%UnitType.EQ.'AirTerminal:SingleDuct:SeriesPIU:Reheat') THEN
    ! series
    PIU(PIUNum)%MaxTotAirMassFlow = RhoAir * PIU(PIUNum)%MaxTotAirVolFlow
    PIU(PIUNum)%MaxPriAirMassFlow = RhoAir * PIU(PIUNum)%MaxPriAirVolFlow
    PIU(PIUNum)%MinPriAirMassFlow = RhoAir * PIU(PIUNum)%MinPriAirFlowFrac * PIU(PIUNum)%MaxPriAirVolFlow
    Node(PriNode)%MassFlowRateMax = PIU(PIUNum)%MaxPriAirMassFlow
    Node(PriNode)%MassFlowRateMin = PIU(PIUNum)%MinPriAirMassFlow
    Node(OutletNode)%MassFlowRateMax = PIU(PIUNum)%MaxTotAirMassFlow
  ELSE
    ! parallel
    PIU(PIUNum)%MaxPriAirMassFlow = RhoAir * PIU(PIUNum)%MaxPriAirVolFlow
    PIU(PIUNum)%MinPriAirMassFlow = RhoAir * PIU(PIUNum)%MinPriAirFlowFrac * PIU(PIUNum)%MaxPriAirVolFlow
    PIU(PIUNum)%MaxSecAirMassFlow = RhoAir * PIU(PIUNum)%MaxSecAirVolFlow
    PIU(PIUNum)%FanOnAirMassFlow = RhoAir * PIU(PIUNum)%FanOnFlowFrac * PIU(PIUNum)%MaxPriAirVolFlow
    Node(PriNode)%MassFlowRateMax = PIU(PIUNum)%MaxPriAirMassFlow
    Node(PriNode)%MassFlowRateMin = PIU(PIUNum)%MinPriAirMassFlow
    Node(OutletNode)%MassFlowRateMax = PIU(PIUNum)%MaxPriAirMassFlow
  END IF

  IF ( ((PIU(PIUNum)%HCoilType_Num == HCoilType_SimpleHeating) .OR.  &
        (PIU(PIUNum)%HCoilType_Num == HCoilType_SteamAirHeating))    &
            .AND. .NOT. MyPlantScanFlag(PIUNum) ) THEN
    CALL InitComponentNodes ( PIU(PIUNum)%MinHotWaterFlow, &
                                PIU(PIUNum)%MaxHotWaterFlow, &
                                PIU(PIUNum)%HotControlNode, &
                                PIU(PIUNum)%HotCoilOutNodeNum, &
                                PIU(PIUNum)%HWLoopNum, &
                                PIU(PIUNum)%HWLoopSide, &
                                PIU(PIUNum)%HWBranchNum, &
                                PIU(PIUNum)%HWCompNum )
  ENDIF
  MyEnvrnFlag(PIUNum) = .FALSE.
END IF ! end one time inits

IF (.not. BeginEnvrnFlag) THEN
  MyEnvrnFlag(PIUNum) = .true.
ENDIF

PriNode = PIU(PIUNum)%PriAirInNode
SecNode = PIU(PIUNum)%SecAirInNode

! Do the start of HVAC time step initializations
IF (FirstHVACIteration) THEN
  ! check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
  IF (GetCurrentScheduleValue(PIU(PIUNum)%SchedPtr) .GT. 0.0d0 .AND. &
      Node(PriNode)%MassFlowRate .GT. 0.0d0) THEN
    IF (PIU(PIUNum)%UnitType.EQ.'AirTerminal:SingleDuct:SeriesPIU:Reheat') THEN
      Node(PriNode)%MassFlowRate = PIU(PIUNum)%MaxPriAirMassFlow
      Node(SecNode)%MassFlowRate = MAX( 0.0d0, PIU(PIUNum)%MaxTotAirMassFlow - PIU(PIUNum)%MaxPriAirMassFlow )
    ELSE
      Node(PriNode)%MassFlowRate = PIU(PIUNum)%MaxPriAirMassFlow
      Node(SecNode)%MassFlowRate = PIU(PIUNum)%MaxSecAirMassFlow
    END IF
  ELSE
    Node(PriNode)%MassFlowRate = 0.0d0
    Node(SecNode)%MassFlowRate = 0.0d0
  END IF
  ! reset the max and min avail flows
  IF (GetCurrentScheduleValue(PIU(PIUNum)%SchedPtr) .GT. 0.0d0 .AND. &
      Node(PriNode)%MassFlowRateMaxAvail .GT. 0.0d0) THEN
    IF (PIU(PIUNum)%UnitType.EQ.'AirTerminal:SingleDuct:SeriesPIU:Reheat') THEN
      Node(PriNode)%MassFlowRateMaxAvail = PIU(PIUNum)%MaxPriAirMassFlow
      Node(PriNode)%MassFlowRateMinAvail = PIU(PIUNum)%MinPriAirMassFlow
      Node(SecNode)%MassFlowRateMaxAvail = MAX( 0.0d0, PIU(PIUNum)%MaxTotAirMassFlow - PIU(PIUNum)%MinPriAirMassFlow )
      Node(SecNode)%MassFlowRateMinAvail = MAX( 0.0d0, PIU(PIUNum)%MaxTotAirMassFlow - PIU(PIUNum)%MaxPriAirMassFlow )
    ELSE
      Node(PriNode)%MassFlowRateMaxAvail = PIU(PIUNum)%MaxPriAirMassFlow
      Node(PriNode)%MassFlowRateMinAvail = PIU(PIUNum)%MinPriAirMassFlow
      Node(SecNode)%MassFlowRateMaxAvail = PIU(PIUNum)%MaxSecAirMassFlow
      Node(SecNode)%MassFlowRateMinAvail = 0.0d0
    END IF
  ELSE
    Node(PriNode)%MassFlowRateMaxAvail = 0.0d0
    Node(PriNode)%MassFlowRateMinAvail = 0.0d0
    Node(SecNode)%MassFlowRateMaxAvail = 0.0d0
    Node(SecNode)%MassFlowRateMinAvail = 0.0d0
  END IF
END IF

! Do the following initializations every time step

! None needed

RETURN
END SUBROUTINE InitPIU

SUBROUTINE SizePIU(PIUNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing PIU terminal units for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE WaterCoils,     ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
  USE SteamCoils,     ONLY: GetCoilSteamInletNode, GetCoilSteamOutletNode
!  USE BranchInputManager, ONLY: MyPlantSizingIndex
  USE DataPlant,          ONLY: PlantLoop, MyPlantSizingIndex
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: PIUNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum     ! do loop index for plant sizing
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  REAL(r64)           :: CoilInTemp
  REAL(r64)           :: CoilOutTemp
  REAL(r64)           :: CoilOutHumRat
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: DesMassFlow

  REAL(r64)           :: TempSteamIn
  REAL(r64)           :: EnthSteamInDry
  REAL(r64)           :: EnthSteamOutWet
  REAL(r64)           :: LatentHeatSteam
  REAL(r64)           :: SteamDensity
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  INTEGER             :: CoilSteamInletNode=0
  INTEGER             :: CoilSteamOutletNode=0
  LOGICAL             :: ErrorsFound
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  INTEGER             :: DummyWaterIndex = 1
  LOGICAL             :: IsAutosize             ! Indicator to autosize
  REAL(r64)           :: MaxPriAirVolFlowDes    ! Autosized maximum primary air flow for reporting
  REAL(r64)           :: MaxPriAirVolFlowUser   ! Hardsized maximum primary air flow for reporting
  REAL(r64)           :: MaxTotAirVolFlowDes    ! Autosized maximum air flow for reporting
  REAL(r64)           :: MaxTotAirVolFlowUser   ! Hardsized maximum air flow for reporting
  REAL(r64)           :: MaxSecAirVolFlowDes    ! Autosized maximum secondary air flow for reporting
  REAL(r64)           :: MaxSecAirVolFlowUser   ! Hardsized maximum secondary air flow for reporting
  REAL(r64)           :: MinPriAirFlowFracDes   ! Autosized minimum primary air flow fraction for reporting
  REAL(r64)           :: MinPriAirFlowFracUser  ! Hardsized minimum primary air flow fraction for reporting
  REAL(r64)           :: FanOnFlowFracDes       ! Autosized fan on flow fraction for reporting
  REAL(r64)           :: FanOnFlowFracUser      ! Hardsized fan on flow fraction for reporting
  REAL(r64)           :: MaxVolHotWaterFlowDes  ! Autosized maximum hot water flow for reporting
  REAL(r64)           :: MaxVolHotWaterFlowUser ! Hardsized maximum hot water flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowDes  ! Autosized maximum hot steam flow for reporting
  REAL(r64)           :: MaxVolHotSteamFlowUser ! Hardsized maximum hot steam flow for reporting

  PltSizHeatNum = 0
  DesMassFlow = 0.0d0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxPriAirVolFlowDes = 0.0d0
  MaxPriAirVolFlowUser = 0.0d0
  MaxTotAirVolFlowDes = 0.0d0
  MaxTotAirVolFlowUser = 0.0d0
  MaxSecAirVolFlowDes = 0.0d0
  MaxSecAirVolFlowUser = 0.0d0
  MinPriAirFlowFracDes = 0.0d0
  MinPriAirFlowFracUser = 0.0d0
  FanOnFlowFracDes = 0.0d0
  FanOnFlowFracUser = 0.0d0
  MaxVolHotWaterFlowDes = 0.0d0
  MaxVolHotWaterFlowUser = 0.0d0
  MaxVolHotSteamFlowDes = 0.0d0
  MaxVolHotSteamFlowUser = 0.0d0

  IF (PIU(PIUNum)%MaxPriAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%MaxPriAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'User-Specified Maximum Primary Air Flow Rate [m3/s]', PIU(PIUNum)%MaxPriAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name)
      MaxPriAirVolFlowDes = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                           TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxPriAirVolFlowDes < SmallAirVolFlow) THEN
        MaxPriAirVolFlowDes = 0.0d0
      END IF

      IF (IsAutosize) THEN
        PIU(PIUNum)%MaxPriAirVolFlow = MaxPriAirVolFlowDes
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Maximum Primary Air Flow Rate [m3/s]', MaxPriAirVolFlowDes)
      ELSE
        IF (PIU(PIUNum)%MaxPriAirVolFlow > 0.0d0 .AND. MaxPriAirVolFlowDes > 0.0d0) THEN
          MaxPriAirVolFlowUser = PIU(PIUNum)%MaxPriAirVolFlow
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Maximum Primary Air Flow Rate [m3/s]', MaxPriAirVolFlowDes, &
                              'User-Specified Maximum Primary Air Flow Rate [m3/s]', MaxPriAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxPriAirVolFlowDes - MaxPriAirVolFlowUser)/MaxPriAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                   //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
              CALL ShowContinueError('User-Specified Primary Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxPriAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Primary Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxPriAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PIU(PIUNum)%MaxTotAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%MaxTotAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'User-Specified Maximum Air Flow Rate [m3/s]', PIU(PIUNum)%MaxTotAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name)
      MaxTotAirVolFlowDes = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                           TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxTotAirVolFlowDes < SmallAirVolFlow) THEN
        MaxTotAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PIU(PIUNum)%MaxTotAirVolFlow = MaxTotAirVolFlowDes
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxTotAirVolFlowDes)
      ELSE
        IF (PIU(PIUNum)%MaxTotAirVolFlow > 0.0d0 .AND. MaxTotAirVolFlowDes> 0.0d0) THEN
          MaxTotAirVolFlowUser = PIU(PIUNum)%MaxTotAirVolFlow
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxTotAirVolFlowDes, &
                              'User-Specified Maximum Air Flow Rate [m3/s]', MaxTotAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxTotAirVolFlowDes - MaxTotAirVolFlowUser)/MaxTotAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                    //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxTotAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxTotAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PIU(PIUNum)%MaxSecAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%MaxSecAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'User-Specified Maximum Secondary Air Flow Rate [m3/s]', PIU(PIUNum)%MaxSecAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name)
      MaxSecAirVolFlowDes = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                           TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxSecAirVolFlowDes < SmallAirVolFlow) THEN
        MaxSecAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PIU(PIUNum)%MaxSecAirVolFlow = MaxSecAirVolFlowDes
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                                'Design Size Maximum Secondary Air Flow Rate [m3/s]', MaxSecAirVolFlowDes)
      ELSE
        IF (PIU(PIUNum)%MaxSecAirVolFlow > 0.0d0 .AND. MaxSecAirVolFlowDes > 0.0d0) THEN
          MaxSecAirVolFlowUser = PIU(PIUNum)%MaxSecAirVolFlow
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                                'Design Size Maximum Secondary Air Flow Rate [m3/s]', MaxSecAirVolFlowDes, &
                                'User-Specified Maximum Secondary Air Flow Rate [m3/s]', MaxSecAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxSecAirVolFlowDes - MaxSecAirVolFlowUser)/MaxSecAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                   //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Secondary Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxSecAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Secondary Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxSecAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PIU(PIUNum)%MinPriAirFlowFrac == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%MinPriAirFlowFrac > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                            'User-Specified Minimum Primary Air Flow Fraction', PIU(PIUNum)%MinPriAirFlowFrac)
      END IF
    ELSE
      CALL CheckZoneSizing(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name)
      IF (PIU(PIUNum)%MaxPriAirVolFlow >= SmallAirVolFlow .AND. &
          TermUnitFinalZoneSizing(CurZoneEqNum)%MinOA >= SmallAirVolFlow) THEN
        MinPriAirFlowFracDes = TermUnitFinalZoneSizing(CurZoneEqNum)%MinOA / PIU(PIUNum)%MaxPriAirVolFlow
      ELSE
        MinPriAirFlowFracDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        PIU(PIUNum)%MinPriAirFlowFrac = MinPriAirFlowFracDes
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Minimum Primary Air Flow Fraction', MinPriAirFlowFracDes)
      ELSE
        IF (PIU(PIUNum)%MinPriAirFlowFrac > 0.0d0 .AND. MinPriAirFlowFracDes > 0.0d0) THEN
          MinPriAirFlowFracUser = PIU(PIUNum)%MinPriAirFlowFrac
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Minimum Primary Air Flow Fraction', MinPriAirFlowFracDes, &
                              'User-Specified Minimum Primary Air Flow Fraction', MinPriAirFlowFracUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MinPriAirFlowFracDes - MinPriAirFlowFracUser)/MinPriAirFlowFracUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                   //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
              CALL ShowContinueError('User-Specified Minimum Primary Air Flow Fraction of '// &
                                      TRIM(RoundSigDigits(MinPriAirFlowFracUser,1)))
              CALL ShowContinueError('differs from Design Size Minimum Primary Air Flow Fraction of ' // &
                                      TRIM(RoundSigDigits(MinPriAirFlowFracDes,1)))
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IF (CurZoneEqNum > 0) THEN
    SELECT CASE(PIU(PIUNum)%UnitType_Num)
      CASE (SingleDuct_SeriesPIU_Reheat)
        TermUnitSizing(CurZoneEqNum)%AirVolFlow = PIU(PIUNum)%MaxTotAirVolFlow
      CASE (SingleDuct_ParallelPIU_Reheat)
        TermUnitSizing(CurZoneEqNum)%AirVolFlow = PIU(PIUNum)%MaxSecAirVolFlow + &
          PIU(PIUNum)%MinPriAirFlowFrac * PIU(PIUNum)%MaxPriAirVolFlow
    END SELECT
  END IF

  IsAutosize = .FALSE.
  IF (PIU(PIUNum)%FanOnFlowFrac == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%FanOnFlowFrac > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'User-Specified Fan On Flow Fraction', PIU(PIUNum)%FanOnFlowFrac)
      END IF
    ELSE
      CALL CheckZoneSizing(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name)
      FanOnFlowFracDes = PIU(PIUNum)%MinPriAirFlowFrac
      IF (IsAutosize) THEN
        PIU(PIUNum)%FanOnFlowFrac = FanOnFlowFracDes
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Fan On Flow Fraction', FanOnFlowFracDes)
      ELSE
        IF (PIU(PIUNum)%FanOnFlowFrac > 0.0d0 .AND. FanOnFlowFracDes > 0.0d0) THEN
          FanOnFlowFracUser = PIU(PIUNum)%FanOnFlowFrac
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'Design Size Fan On Flow Fraction', FanOnFlowFracDes, &
                              'User-Specified Fan On Flow Fraction', FanOnFlowFracUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(FanOnFlowFracDes - FanOnFlowFracUser)/FanOnFlowFracUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                   //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
              CALL ShowContinueError('User-Specified Fan On Flow Fraction of '// &
                                      TRIM(RoundSigDigits(FanOnFlowFracUser,1)))
              CALL ShowContinueError('differs from Design Size Fan On Flow Fraction of ' // &
                                      TRIM(RoundSigDigits(FanOnFlowFracDes,1)))
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PIU(PIUNum)%MaxVolHotWaterFlow == AutoSize) THEN !.or.()) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%MaxVolHotWaterFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                            'User-Specified Maximum Reheat Water Flow Rate [m3/s]', PIU(PIUNum)%MaxVolHotWaterFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(PIU(PIUNum)%UnitType,PIU(PIUNum)%Name)
      IF (SameString(PIU(PIUNum)%HCoilType,'Coil:Heating:Water')) THEN

        CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',PIU(PIUNum)%HCoil,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',PIU(PIUNum)%HCoil,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', PIU(PIUNum)%HCoil, CoilWaterInletNode, &
                                       CoilWaterOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN

            IF (TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              CoilInTemp = TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU * PIU(PIUNum)%MinPriAirFlowFrac + &
                           TermUnitFinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak * (1.0d0 - PIU(PIUNum)%MinPriAirFlowFrac)
              CoilOutTemp = TermUnitFinalZoneSizing(CurZoneEqNum)%HeatDesTemp
              CoilOutHumRat = TermUnitFinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
              DesMassFlow = StdRhoAir * TermUnitSizing(CurZoneEqNum)%AirVolFlow
              DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                            * DesMassFlow * (CoilOutTemp-CoilInTemp)

              rho = GetDensityGlycol(PlantLoop(PIU(PIUNum)%HWLoopNum)%FluidName, &
                                    60.d0, &
                                     PlantLoop(PIU(PIUNum)%HWLoopNum)%FluidIndex, &
                                     'SizePIU')
              Cp = GetSpecificHeatGlycol(PlantLoop(PIU(PIUNum)%HWLoopNum)%FluidName, &
                                    60.d0, &
                                     PlantLoop(PIU(PIUNum)%HWLoopNum)%FluidIndex, &
                                     'SizePIU')

              MaxVolHotWaterFlowDes = DesCoilLoad / &
                                    ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                    Cp * rho )
            ELSE
              MaxVolHotWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in' //  TRIM(PIU(PIUNum)%UnitType) // ' Object='//TRIM(PIU(PIUNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          PIU(PIUNum)%MaxVolHotWaterFlow = MaxVolHotWaterFlowDes
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                                  'Design Size Maximum Reheat Water Flow Rate [m3/s]', MaxVolHotWaterFlowDes)
        ELSE ! Hardsize with sizing data
          IF (PIU(PIUNum)%MaxVolHotWaterFlow > 0.0d0 .AND. MaxVolHotWaterFlowDes > 0.0d0) THEN
            MaxVolHotWaterFlowUser = PIU(PIUNum)%MaxVolHotWaterFlow
            CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                                  'Design Size Maximum Reheat Water Flow Rate [m3/s]', MaxVolHotWaterFlowDes, &
                                  'User-Specified Maximum Reheat Water Flow Rate [m3/s]', MaxVolHotWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser)/MaxVolHotWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                       //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
                CALL ShowContinueError('User-Specified Maximum Reheat Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxVolHotWaterFlowUser,5))//' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Reheat Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxVolHotWaterFlowDes,5))//' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      ELSE
        PIU(PIUNum)%MaxVolHotWaterFlow = 0.0d0
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (PIU(PIUNum)%MaxVolHotSteamFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (PIU(PIUNum)%MaxVolHotWaterFlow > 0.0d0) THEN
        CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                              'User-Specified Maximum Reheat Steam Flow Rate [m3/s]', PIU(PIUNum)%MaxVolHotWaterFlow)
      END IF
    ELSE
      IF (SameString(PIU(PIUNum)%HCoilType,'Coil:Heating:Steam')) THEN

        CoilSteamInletNode = GetCoilSteamInletNode('Coil:Heating:Steam',PIU(PIUNum)%HCoil,ErrorsFound)
        CoilSteamOutletNode = GetCoilSteamOutletNode('Coil:Heating:Steam',PIU(PIUNum)%HCoil,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Steam', PIU(PIUNum)%HCoil, CoilSteamInletNode, &
                                       CoilSteamOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN

            IF (TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              CoilInTemp = TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU * PIU(PIUNum)%MinPriAirFlowFrac + &
                           TermUnitFinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak * (1.0d0 - PIU(PIUNum)%MinPriAirFlowFrac)
              CoilOutTemp = TermUnitFinalZoneSizing(CurZoneEqNum)%HeatDesTemp
              CoilOutHumRat = TermUnitFinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
              DesMassFlow = StdRhoAir * TermUnitSizing(CurZoneEqNum)%AirVolFlow
              DesCoilLoad = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp)) &
                            * DesMassFlow * (CoilOutTemp-CoilInTemp)
              TempSteamIn= 100.00d0
              EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,PIU(PIUNum)%HCoil_FluidIndex,'SizePIU')
              EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,PIU(PIUNum)%HCoil_FluidIndex,'SizePIU')
              LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
              SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,PIU(PIUNum)%HCoil_FluidIndex,'SizePIU')
              Cp = GetSpecificHeatGlycol('WATER', PlantSizData(PltSizHeatNum)%ExitTemp, DummyWaterIndex, 'SizePIU')
              MaxVolHotSteamFlowDes = DesCoilLoad/(SteamDensity*(LatentHeatSteam + &
                                    PlantSizData(PltSizHeatNum)%DeltaT * Cp))
            ELSE
              MaxVolHotSteamFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of Steam flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in' //  TRIM(PIU(PIUNum)%UnitType) // ' Object='//TRIM(PIU(PIUNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          PIU(PIUNum)%MaxVolHotSteamFlow = MaxVolHotSteamFlowDes
          CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                                  'Design Size Maximum Reheat Steam Flow [m3/s]', MaxVolHotSteamFlowDes)
        ELSE
          IF (PIU(PIUNum)%MaxVolHotSteamFlow > 0.0d0 .AND. MaxVolHotSteamFlowDes > 0.0d0) THEN
            MaxVolHotSteamFlowUser = PIU(PIUNum)%MaxVolHotSteamFlow
            CALL ReportSizingOutput(PIU(PIUNum)%UnitType, PIU(PIUNum)%Name, &
                                  'Design Size Maximum Reheat Steam Flow [m3/s]', MaxVolHotSteamFlowDes, &
                                  'User-Specified Maximum Reheat Steam Flow [m3/s]', MaxVolHotSteamFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotSteamFlowDes - MaxVolHotSteamFlowUser)/MaxVolHotSteamFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizePIU: Potential issue with equipment sizing for ' &
                                      //TRIM(PIU(PIUNum)%UnitType)//' '//TRIM(PIU(PIUNum)%Name))
                CALL ShowContinueError('User-Specified Maximum Reheat Steam Flow of '// &
                                      TRIM(RoundSigDigits(MaxVolHotSteamFlowUser,5))//' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Reheat Steam Flow of ' // &
                                      TRIM(RoundSigDigits(MaxVolHotSteamFlowDes,5))//' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      ELSE
        PIU(PIUNum)%MaxVolHotSteamFlow = 0.0d0
      END IF
    END IF
  END IF

  IF (CurZoneEqNum > 0) THEN
    TermUnitSizing(CurZoneEqNum)%MinFlowFrac = PIU(PIUNum)%MinPriAirFlowFrac
    TermUnitSizing(CurZoneEqNum)%MaxHWVolFlow = PIU(PIUNum)%MaxVolHotWaterFlow
    TermUnitSizing(CurZoneEqNum)%MaxSTVolFlow = PIU(PIUNum)%MaxVolHotSteamFlow
    TermUnitSizing(CurZoneEqNum)%InducesPlenumAir = PIU(PIUNum)%InducesPlenumAir
    IF (PIU(PIUNum)%HCoilType_Num == HCoilType_SimpleHeating) THEN
      CALL SetCoilDesFlow(PIU(PIUNum)%HCoilType,PIU(PIUNum)%HCoil,TermUnitSizing(CurZoneEqNum)%AirVolFlow,&
                          ErrorsFound)
    END IF
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END  SUBROUTINE SizePIU

SUBROUTINE CalcSeriesPIU(PIUNum,ZoneNum,ZoneNode,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a series powered induction unit; adjust its primary air flow
          ! and reheat coil output to match the zone load.

          ! METHODOLOGY EMPLOYED:
          ! If unit is on and there is a cooling load:
          ! (1) simulates mixer and fan at max secondary air flow and heating coil
          !     off. Obtains fan temperature increase.
          ! (2) Calculates primary and secomdary air flow to meet zone load and
          !     resimulates mixer, fan, and (off) coil.
          ! If unit is on and there is a heating load
          ! (1) sets primary air flow to a minimum.
          ! (2) simulates mixer and fan
          ! (3) if reheat is hot water, calls ControlCompOutput to simulate hot
          !     water coil and adjust water flow to match coil output to the zone load.
          ! (4) if reheat is electric or gas calls SimulateHeatingCoilComponents to
          !     simulate coil at coil output that matches the zone load

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE MixerComponent, ONLY      : SimAirMixer
  Use HeatingCoils, Only: SimulateHeatingCoilComponents
  USE Fans, ONLY        : SimulateFanComponents
  USE WaterCoils, ONLY: SimulateWaterCoilComponents
  USE SteamCoils, Only: SimulateSteamCoilComponents
  USE DataInterfaces, ONLY: ControlCompOutput
  USE DataPlant,   ONLY: PlantLoop
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)  :: PIUNum             ! number of the current PIU being simulated
  INTEGER, INTENT (IN)  :: ZoneNum            ! number of zone being served
  INTEGER, INTENT (IN)  :: ZoneNode           ! zone node number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIter = 25 ! maximum number of iterations for controlling output

          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)    :: QZnReq            ! heating or cooling needed by zone [Watts]
REAL(r64)    :: QToHeatSetPt      ! [W]  remaining load to heating setpoint
REAL(r64)    :: QActualHeating    ! the heating load seen by the reheat coil [W]
REAL(r64)    :: PowerMet          ! power supplied
LOGICAL :: UnitOn            ! TRUE if unit is on
LOGICAL :: PriOn             ! TRUE if primary air available
LOGICAL :: HCoilOn           ! TRUE if heating coil is on
INTEGER :: ControlNode       ! the hot water or cold water inlet node
REAL(r64)    :: ControlOffset     ! tolerance for output control
REAL(r64)    :: MaxWaterFlow      ! maximum water flow for heating or cooling [kg/s]
REAL(r64)    :: MinWaterFlow      ! minimum water flow for heating or cooling [kg/s]
INTEGER :: OutletNode        ! unit air outlet node
INTEGER :: PriNode           ! unit primary air inlet node
INTEGER :: SecNode           ! unit secondary air inlet node
INTEGER :: HCoilInAirNode    ! air inlet node of reheat coil
REAL(r64)    :: QCoilReq          ! required heating coil outlet to meet zone load
REAL(r64)    :: PriAirMassFlow    ! primary air mass flow rate [kg/s]
REAL(r64)    :: PriAirMassFlowMax ! max primary air mass flow rate [kg/s]
REAL(r64)    :: PriAirMassFlowMin ! min primary air mass flow rate [kg/s]
REAL(r64)    :: SecAirMassFlow    ! secondary air mass flow rate [kg/s]
REAL(r64)    :: CpAirZn           ! zone air specific heat [J/kg-C]
REAL(r64)    :: FanDeltaTemp      ! fan temperature rise [C]
REAL(r64)    :: OutletTempNeeded  ! unit outlet temperature needed to meet cooling load
REAL(r64)    :: MixTempNeeded     ! mixer outlet temperature needed to meet cooling load
REAL(r64)    :: MinSteamFlow
REAL(r64)    :: MaxSteamFlow
REAL(r64)    :: rho !local plant fluid density
REAL(r64)    :: Cp  ! local plant specific Heat
REAL(r64)    :: mdot ! local plant fluid flow rate kg/s

          ! FLOW

FanElecPower = 0.0d0
! initialize local variables
FanDeltaTemp = 0.0d0
OutletTempNeeded = 0.0d0
MixTempNeeded = 0.0d0
UnitOn = .TRUE.
PriOn = .TRUE.
HCoilOn = .TRUE.
ControlNode = 0
ControlOffset = PIU(PIUNum)%HotControlOffset
OutletNode = PIU(PIUNum)%OutAirNode
PriNode = PIU(PIUNum)%PriAirInNode
SecNode = PIU(PIUNum)%SecAirInNode
HCoilInAirNode = PIU(PIUNum)%HCoilInAirNode
ControlNode = PIU(PIUNum)%HotControlNode
PriAirMassFlow = Node(PriNode)%MassFlowRate
PriAirMassFlowMax = Node(PriNode)%MassFlowRateMaxAvail
PriAirMassFlowMin = Node(PriNode)%MassFlowRateMinAvail
SecAirMassFlow = Node(SecNode)%MassFlowRate
QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
CpAirZn = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat,Node(ZoneNode)%Temp)

!On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment
IF (ControlNode.GT.0) THEN
  IF (FirstHVACIteration) THEN
     MaxWaterFlow = PIU(PIUNum)%MaxHotWaterFlow
     MinWaterFlow = PIU(PIUNum)%MinHotWaterFlow
     MaxSteamFlow = PIU(PIUNum)%MaxHotWaterFlow       ! Need TO change THESE******************************
     MinSteamFlow = PIU(PIUNum)%MinHotWaterFlow
  ELSE
     MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
     MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
     MaxSteamFlow = Node(ControlNode)%MassFlowRateMaxAvail
     MinSteamFlow = Node(ControlNode)%MassFlowRateMinAvail
  END IF
END IF
IF (GetCurrentScheduleValue(PIU(PIUNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
IF (PriAirMassFlow.LE.SmallMassFlow .OR. PriAirMassFlowMax.LE.SmallMassFlow) PriOn = .FALSE.
! Set the mass flow rates
IF (UnitOn) THEN
  ! unit is on
  IF (.NOT. PriOn) THEN
    ! no primary air flow
    PriAirMassFlow = 0.0d0
    SecAirMassFlow = PIU(PIUNum)%MaxTotAirMassFlow
  ELSE IF (CurDeadBandOrSetback(ZoneNum) .OR. ABS(QZnReq).LT.SmallLoad) THEN
    ! in deadband or very small load: set primary air flow to the minimum
    PriAirMassFlow = PriAirMassFlowMin
    SecAirMassFlow = MAX( 0.0d0, PIU(PIUNum)%MaxTotAirMassFlow - PriAirMassFlow )
  ELSE IF (QZnReq.GT.SmallLoad) THEN
    ! heating: set primary air flow to the minimum
    PriAirMassFlow = PriAirMassFlowMin
    SecAirMassFlow = MAX( 0.0d0, PIU(PIUNum)%MaxTotAirMassFlow - PriAirMassFlow )
  ELSE
    ! cooling: set the primary air flow rate to meet the load.
    ! First calculate the fan temperature rise
    ! use only secondary air for this calculation
    Node(PriNode)%MassFlowRate = 0.0d0
    Node(SecNode)%MassFlowRate = PIU(PIUNum)%MaxTotAirMassFlow
    CALL SimAirMixer(PIU(PIUNum)%MixerName,PIU(PIUNum)%Mixer_Num) ! fire the mixer
    CALL SimulateFanComponents(PIU(PIUNum)%FanName,FirstHVACIteration,PIU(PIUNum)%Fan_Index) ! fire the fan
    FanDeltaTemp = Node(HCoilInAirNode)%Temp - Node(SecNode)%Temp
    ! using the required zone load, calculate the air temperature needed to meet the load
    ! PIU(PIUNum)%MaxTotAirMassFlow * CpAirZn * (OutletTempNeeded - Node(ZoneNodeNum)%Temp) = QZnReq
    OutletTempNeeded = Node(ZoneNode)%Temp + QZnReq / (PIU(PIUNum)%MaxTotAirMassFlow * CpAirZn)
    MixTempNeeded = OutletTempNeeded - FanDeltaTemp
    IF (MixTempNeeded.LE.Node(PriNode)%Temp) THEN
      PriAirMassFlow = PriAirMassFlowMax
    ELSE IF (MixTempNeeded.GE.Node(PriNode)%Temp .AND. MixTempNeeded.GE.Node(SecNode)%Temp) THEN
      PriAirMassFlow = PriAirMassFlowMin
    ELSE
      PriAirMassFlow = PIU(PIUNum)%MaxTotAirMassFlow * (Node(SecNode)%Temp - MixTempNeeded) / &
                         MAX(SmallTempDiff, Node(SecNode)%Temp - Node(PriNode)%Temp)
      PriAirMassFlow = MIN(MAX(PriAirMassFlow,PriAirMassFlowMin),PriAirMassFlowMax)
    END IF
    SecAirMassFlow = MAX( 0.0d0, PIU(PIUNum)%MaxTotAirMassFlow - PriAirMassFlow )
  END IF
ELSE
  ! unit is off ; no flow
  PriAirMassFlow = 0.0d0
  SecAirMassFlow = 0.0d0
END IF
! Set inlet node flowrates
Node(PriNode)%MassFlowRate = PriAirMassFlow
Node(SecNode)%MassFlowRate = SecAirMassFlow
!now that inlet airflows have been set, the terminal bos components can be simulated.

! fire the mixer
CALL SimAirMixer(PIU(PIUNum)%MixerName,PIU(PIUNum)%Mixer_Num)
! fire the fan
CALL SimulateFanComponents(PIU(PIUNum)%FanName,FirstHVACIteration,PIU(PIUNum)%Fan_Index)
! check if heating coil is off
QActualHeating = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * &
                 (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
IF ( (.NOT. UnitOn) .OR. (QActualHeating .LT. SmallLoad) .OR. &
     (TempControlType(ZoneNum) == SingleCoolingSetPoint) .OR. &
     (PriAirMassFlow > PriAirMassFlowMin) ) THEN
  HCoilOn = .FALSE.
END IF
!fire the heating coil

SELECT CASE(PIU(PIUNum)%HCoilType_Num)

  CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
    IF ( .NOT. HCoilOn) THEN
      !call the reheat coil with the NO FLOW condition
      mdot = 0.d0
      Call SetComponentFlowRate(mdot, &
                                 PIU(PIUNum)%HotControlNode, &
                                 PIU(PIUNum)%HotCoilOutNodeNum, &
                                 PIU(PIUNum)%HWLoopNum, &
                                 PIU(PIUNum)%HWLoopSide, &
                                 PIU(PIUNum)%HWBranchNum, &
                                 PIU(PIUNum)%HWCompNum)

      CALL SimulateWaterCoilComponents(PIU(PIUNum)%HCoil,FirstHVACIteration,PIU(PIUNum)%HCoil_Index)
    ELSE
      ! control water flow to obtain output matching QZnReq
      CALL ControlCompOutput(CompType=PIU(PIUNum)%UnitType,CompName=PIU(PIUNum)%HCoil,  &
                             CompNum=PIU(PIUNum)%HCoil_Index, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QActualHeating, &
                              ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             TempInNode=HCoilInAirNode,TempOutNode=OutletNode, &
                             MinFlow=MinWaterFlow,ControlOffSet=ControlOffset, &
                             ControlCompTypeNum=PIU(PIUNum)%ControlCompTypeNum,&
                             CompErrIndex=PIU(PIUNum)%CompErrIndex, &
                             LoopNum     = PIU(PIUNum)%HWLoopNum,   &
                             LoopSide    = PIU(PIUNum)%HWLoopSide,  &
                             BranchIndex = PIU(PIUNum)%HWBranchNum)
    END IF
  CASE(HCoilType_SteamAirHeating) ! COIL:STEAM:AIRHEATING
    IF ( .NOT. HCoilOn) THEN
      QCoilReq = 0.0d0
    ELSE
      QCoilReq = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
    END IF
    CALL SimulateSteamCoilComponents(CompName=PIU(PIUNum)%HCoil,       &
                                       FirstHVACIteration=FirstHVACIteration, &
                                       QCoilReq=QCoilReq,CompIndex=PIU(PIUNum)%HCoil_Index)

  CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
    IF ( .NOT. HCoilOn) THEN
      QCoilReq = 0.0d0
    ELSE
      QCoilReq = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
    END IF
    CALL SimulateHeatingCoilComponents(CompName=PIU(PIUNum)%HCoil,       &
                                       FirstHVACIteration=FirstHVACIteration, &
                                       QCoilReq=QCoilReq,CompIndex=PIU(PIUNum)%HCoil_Index)

  CASE(HCoilType_Gas)  ! COIL:GAS:HEATING
    IF ( .NOT. HCoilOn) THEN
      QCoilReq = 0.0d0
    ELSE
      QCoilReq = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
    END IF
    CALL SimulateHeatingCoilComponents(CompName=PIU(PIUNum)%HCoil,       &
                                       FirstHVACIteration=FirstHVACIteration, &
                                       QCoilReq=QCoilReq,CompIndex=PIU(PIUNum)%HCoil_Index)

END SELECT

PowerMet = Node(OutletNode)%MassFlowRate * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(ZoneNode)%HumRat)  &
                                          - PsyHFnTdbW(Node(ZoneNode)%Temp,Node(ZoneNode)%HumRat))
PIU(PIUNum)%HeatingRate = MAX(0.0d0,PowerMet)
PIU(PIUNum)%SensCoolRate = ABS(MIN(constant_zero,PowerMet))
IF (Node(OutletNode)%MassFlowRate .EQ. 0.0d0) THEN
  Node(PriNode)%MassFlowRate = 0.0d0
  Node(SecNode)%MassFlowRate = 0.0d0
END IF
IF (PIU(PIUNum)%InducesPlenumAir) THEN
  PlenumInducedMassFlow = Node(SecNode)%MassFlowRate
ELSE
  PlenumInducedMassFlow = 0.0d0
END IF
Node(OutletNode)%MassFlowRateMax = PIU(PIUNum)%MaxTotAirMassFlow

RETURN
END SUBROUTINE CalcSeriesPIU

SUBROUTINE CalcParallelPIU(PIUNum,ZoneNum,ZoneNode,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a parallel powered induction unit; adjust its primary air flow
          ! and reheat coil output to match the zone load.

          ! METHODOLOGY EMPLOYED:
          ! If unit is on and there is a cooling load:
          ! (1) simulate fan at max secondary air flow and heating coil
          !     off. Obtains fan temperature increase.
          ! (2) Calculates primary and secomdary air flow to meet zone load.
          !     (a) Assume fan is off and calculate primary air flow to meet cooling load.
          !     (b) If calculated primary air flow is above the fan turn on ratio, fan is off.
          !         Otherwise fan is on; calculate mixed secondary and primary air flow that
          !         will meet the zone load
          !  (3) Simulate fan, mixer, and (off) heating coil to obtain zone inlet conditions.
          ! If unit is on and there is a heating load
          ! (1) sets primary air flow to a minimum.
          ! (2) simulates fan and mixer
          ! (3) if reheat is hot water, calls ControlCompOutput to simulate hot
          !     water coil and adjust water flow to match coil output to the zone load.
          ! (4) if reheat is electric or gas calls SimulateHeatingCoilComponents to
          !     simulate coil at coil output that matches the zone load

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE MixerComponent, ONLY      : SimAirMixer
  Use HeatingCoils, Only: SimulateHeatingCoilComponents
  USE Fans, ONLY        : SimulateFanComponents
  USE WaterCoils, ONLY: SimulateWaterCoilComponents
  USE SteamCoils, ONLY: SimulateSteamCoilComponents
  USE DataInterfaces, ONLY: ControlCompOutput
  USE PlantUtilities,  ONLY: SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)  :: PIUNum             ! number of the current PIU being simulated
  INTEGER, INTENT (IN)  :: ZoneNum            ! number of zone being served
  INTEGER, INTENT (IN)  :: ZoneNode           ! zone node number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: MaxIter = 25 ! maximum number of iterations for controlling output

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)    :: QZnReq            ! heating or cooling needed by zone [Watts]
REAL(r64)    :: QToHeatSetPt      ! [W]  remaining load to heating setpoint
REAL(r64)    :: QActualHeating    ! the heating load seen by the reheat coil [W]
REAL(r64)    :: PowerMet          ! power supplied
LOGICAL :: UnitOn            ! TRUE if unit is on
LOGICAL :: PriOn             ! TRUE if primary air available
LOGICAL :: HCoilOn           ! TRUE if heating coil is on
INTEGER :: ControlNode       ! the hot water or cold water inlet node
REAL(r64)    :: ControlOffset     ! tolerance for output control
REAL(r64)    :: MaxWaterFlow      ! maximum water flow for heating or cooling [kg/s]
REAL(r64)    :: MinWaterFlow      ! minimum water flow for heating or cooling [kg/s]
INTEGER :: OutletNode        ! unit air outlet node
INTEGER :: PriNode           ! unit primary air inlet node
INTEGER :: SecNode           ! unit secondary air inlet node
INTEGER :: HCoilInAirNode    ! air inlet node of reheat coil
REAL(r64)    :: QCoilReq          ! required heating coil outlet to meet zone load
REAL(r64)    :: PriAirMassFlow    ! primary air mass flow rate [kg/s]
REAL(r64)    :: PriAirMassFlowMax ! max primary air mass flow rate [kg/s]
REAL(r64)    :: PriAirMassFlowMin ! min primary air mass flow rate [kg/s]
REAL(r64)    :: SecAirMassFlow    ! secondary air mass flow rate [kg/s]
REAL(r64)    :: CpAirZn           ! zone air specific heat [J/kg-C]
REAL(r64)    :: FanDeltaTemp      ! fan temperature rise [C]
!unusedREAL(r64)    :: MaxSteamFlow
!unusedREAL(r64)    :: MinSteamFlow
REAL(r64)    :: mdot ! local fluid flow rate kg/s

          ! FLOW

FanElecPower = 0.0d0
! initialize local variables
FanDeltaTemp = 0.0d0
UnitOn = .TRUE.
PriOn = .TRUE.
HCoilOn = .TRUE.
ControlNode = 0
ControlOffset = PIU(PIUNum)%HotControlOffset
OutletNode = PIU(PIUNum)%OutAirNode
PriNode = PIU(PIUNum)%PriAirInNode
SecNode = PIU(PIUNum)%SecAirInNode
HCoilInAirNode = PIU(PIUNum)%HCoilInAirNode
ControlNode = PIU(PIUNum)%HotControlNode
PriAirMassFlow = Node(PriNode)%MassFlowRate
PriAirMassFlowMax = Node(PriNode)%MassFlowRateMaxAvail
PriAirMassFlowMin = Node(PriNode)%MassFlowRateMinAvail
SecAirMassFlow = Node(SecNode)%MassFlowRate
QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
CpAirZn = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat,Node(ZoneNode)%Temp)

!On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment
IF (ControlNode > 0) THEN
 If(FirstHVACIteration) Then
   MaxWaterFlow = PIU(PIUNum)%MaxHotWaterFlow
   MinWaterFlow = PIU(PIUNum)%MinHotWaterFlow
  Else
   MaxWaterFlow = Node(ControlNode)%MassFlowRateMaxAvail
   MinWaterFlow = Node(ControlNode)%MassFlowRateMinAvail
  End If
END IF
IF (GetCurrentScheduleValue(PIU(PIUNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
IF (PriAirMassFlow.LE.SmallMassFlow .OR. PriAirMassFlowMax.LE.SmallMassFlow) PriOn = .FALSE.
! Set the mass flow rates
IF (UnitOn) THEN
  ! unit is on
  IF (.NOT. PriOn) THEN
    ! no primary air flow
    PriAirMassFlow = 0.0d0
    SecAirMassFlow = PIU(PIUNum)%MaxSecAirMassFlow
  ELSE IF (CurDeadBandOrSetback(ZoneNum) .OR. ABS(QZnReq).LT.SmallLoad) THEN
    ! in deadband or very small load: set primary air flow to the minimum
    PriAirMassFlow = PriAirMassFlowMin
    SecAirMassFlow = PIU(PIUNum)%MaxSecAirMassFlow
  ELSE IF (QZnReq.GT.SmallLoad) THEN
    ! heating: set primary air flow to the minimum
    PriAirMassFlow = PriAirMassFlowMin
    SecAirMassFlow = PIU(PIUNum)%MaxSecAirMassFlow
  ELSE
    ! cooling: set the primary air flow rate to meet the load.
    ! First calculate the fan temperature rise
    Node(SecNode)%MassFlowRate = PIU(PIUNum)%MaxSecAirMassFlow
    Node(SecNode)%MassFlowRateMaxAvail = PIU(PIUNum)%MaxSecAirMassFlow
    Node(PriNode)%MassFlowRate = 0.0d0
    CALL SimulateFanComponents(PIU(PIUNum)%FanName,FirstHVACIteration,PIU(PIUNum)%Fan_Index) ! fire the fan
    CALL SimAirMixer(PIU(PIUNum)%MixerName,PIU(PIUNum)%Mixer_Num) ! fire the mixer
    FanDeltaTemp = Node(HCoilInAirNode)%Temp - Node(SecNode)%Temp
    ! Assuming the fan is off, calculate the primary air flow needed to meet the zone cooling demand.
    ! CpAir*PriAirMassFlow*(Node(PriNode)%Temp - Node(ZoneNodeNum)%Temp) = QZnReq
    PriAirMassFlow = QZnReq / (CpAirZn*MIN(-SmallTempDiff,(Node(PriNode)%Temp -Node(ZoneNode)%Temp)))
    PriAirMassFlow = MIN(MAX(PriAirMassFlow,PriAirMassFlowMin),PriAirMassFlowMax)
    ! check for fan on or off
    IF (PriAirMassFlow.GT.PIU(PIUNum)%FanOnAirMassFlow) THEN
      SecAirMassFlow = 0.0d0 ! Fan is off; no secondary air
    ELSE
      ! fan is on; recalc primary air flow
      ! CpAir*PriAirMassFlow*(Node(PriNode)%Temp - Node(ZoneNodeNum)%Temp) +
      !   CpAir*SecAirMassFlow*(Node(SecNode)%Temp + FanDeltaTemp - Node(ZoneNodeNum)%Temp) = QZnReq
      PriAirMassFlow = (QZnReq - CpAirZn*SecAirMassFlow*(Node(SecNode)%Temp + FanDeltaTemp - Node(ZoneNode)%Temp)) / &
                         (CpAirZn*MIN(-SmallTempDiff,(Node(PriNode)%Temp -Node(ZoneNode)%Temp)))
      PriAirMassFlow = MIN(MAX(PriAirMassFlow,PriAirMassFlowMin),PriAirMassFlowMax)
      SecAirMassFlow = PIU(PIUNum)%MaxSecAirMassFlow
    END IF
  END IF
ELSE
  ! unit is off; no flow
  PriAirMassFlow = 0.0d0
  SecAirMassFlow = 0.0d0
END IF
! Set inlet node flowrates
Node(PriNode)%MassFlowRate = PriAirMassFlow
Node(SecNode)%MassFlowRate = SecAirMassFlow
Node(SecNode)%MassFlowRateMaxAvail = SecAirMassFlow
!now that inlet airflows have been set, the terminal bos components can be simulated.
! fire the fan
CALL SimulateFanComponents(PIU(PIUNum)%FanName,FirstHVACIteration,PIU(PIUNum)%Fan_Index)
! fire the mixer
CALL SimAirMixer(PIU(PIUNum)%MixerName,PIU(PIUNum)%Mixer_Num)
! check if heating coil is off
QActualHeating = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * &
                 (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
IF ( (.NOT. UnitOn) .OR. (QActualHeating .LT. SmallLoad) .OR. &
     (TempControlType(ZoneNum) == SingleCoolingSetPoint) .OR. &
     (PriAirMassFlow > PriAirMassFlowMin) ) THEN
  HCoilOn = .FALSE.
END IF
!fire the heating coil
SELECT CASE(PIU(PIUNum)%HCoilType_Num)

  CASE(HCoilType_SimpleHeating)  ! COIL:WATER:SIMPLEHEATING
    IF ( .NOT. HCoilOn) THEN
      !call the reheat coil with the NO FLOW condition
      mdot = 0.d0
      Call SetComponentFlowRate(mdot, &
                                 PIU(PIUNum)%HotControlNode, &
                                 PIU(PIUNum)%HotCoilOutNodeNum, &
                                 PIU(PIUNum)%HWLoopNum, &
                                 PIU(PIUNum)%HWLoopSide, &
                                 PIU(PIUNum)%HWBranchNum, &
                                 PIU(PIUNum)%HWCompNum)
      CALL SimulateWaterCoilComponents(PIU(PIUNum)%HCoil,FirstHVACIteration,PIU(PIUNum)%HCoil_Index)
    ELSE
      ! control water flow to obtain output matching QZnReq
      CALL ControlCompOutput(CompType=PIU(PIUNum)%UnitType,CompName=PIU(PIUNum)%HCoil,CompNum=PIU(PIUNum)%HCoil_Index, &
                             FirstHVACIteration=FirstHVACIteration,QZnReq=QActualHeating, &
                              ActuatedNode=ControlNode,MaxFlow=MaxWaterFlow, &
                             TempInNode=HCoilInAirNode,TempOutNode=OutletNode, &
                             MinFlow=MinWaterFlow,ControlOffSet=ControlOffset, &
                             ControlCompTypeNum=PIU(PIUNum)%ControlCompTypeNum,&
                             CompErrIndex=PIU(PIUNum)%CompErrIndex,   &
                             LoopNum     = PIU(PIUNum)%HWLoopNum,   &
                             LoopSide    = PIU(PIUNum)%HWLoopSide,  &
                             BranchIndex = PIU(PIUNum)%HWBranchNum)
    END IF
  CASE(HCoilType_SteamAirHeating)  ! COIL:STEAM:AIRHEATING
    IF ( .NOT. HCoilOn) THEN
      QCoilReq = 0.0d0
    ELSE
      QCoilReq = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
    END IF
    CALL SimulateSteamCoilComponents(CompName=PIU(PIUNum)%HCoil,       &
                                       FirstHVACIteration=FirstHVACIteration, &
                                       QCoilReq=QCoilReq,CompIndex=PIU(PIUNum)%HCoil_Index)
  CASE(HCoilType_Electric)  ! COIL:ELECTRIC:HEATING
    IF ( .NOT. HCoilOn) THEN
      QCoilReq = 0.0d0
    ELSE
      QCoilReq = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
    END IF
    CALL SimulateHeatingCoilComponents(CompName=PIU(PIUNum)%HCoil,       &
                                       FirstHVACIteration=FirstHVACIteration, &
                                       QCoilReq=QCoilReq,CompIndex=PIU(PIUNum)%HCoil_Index)

  CASE(HCoilType_Gas)  ! COIL:GAS:HEATING
    IF ( .NOT. HCoilOn) THEN
      QCoilReq = 0.0d0
    ELSE
      QCoilReq = QToHeatSetPt - Node(HCoilInAirNode)%MassFlowRate * CpAirZn * (Node(HCoilInAirNode)%Temp-Node(ZoneNode)%Temp)
    END IF
    CALL SimulateHeatingCoilComponents(CompName=PIU(PIUNum)%HCoil,       &
                                       FirstHVACIteration=FirstHVACIteration, &
                                       QCoilReq=QCoilReq,CompIndex=PIU(PIUNum)%HCoil_Index)

END SELECT
PowerMet = Node(OutletNode)%MassFlowRate * (PsyHFnTdbW(Node(OutletNode)%Temp,Node(ZoneNode)%HumRat)  &
                                          - PsyHFnTdbW(Node(ZoneNode)%Temp,Node(ZoneNode)%HumRat))
PIU(PIUNum)%HeatingRate = MAX(0.0d0,PowerMet)
PIU(PIUNum)%SensCoolRate = ABS(MIN(constant_zero,PowerMet))
IF (Node(OutletNode)%MassFlowRate .EQ. 0.0d0) THEN
  Node(PriNode)%MassFlowRate = 0.0d0
  Node(SecNode)%MassFlowRate = 0.0d0
END IF
IF (PIU(PIUNum)%InducesPlenumAir) THEN
  PlenumInducedMassFlow = Node(SecNode)%MassFlowRate
ELSE
  PlenumInducedMassFlow = 0.0d0
END IF
Node(OutletNode)%MassFlowRateMax = PIU(PIUNum)%MaxPriAirMassFlow

RETURN
END SUBROUTINE CalcParallelPIU


SUBROUTINE ReportPIU(PIUNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fills some of the report variables for the PIU terminal boxes

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: PIUNum ! number of the current fan coil unit being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW

PIU(PIUNum)%HeatingEnergy = PIU(PIUNum)%HeatingRate * TimeStepSys * SecInHour
PIU(PIUNum)%SensCoolEnergy = PIU(PIUNum)%SensCoolRate * TimeStepSys * SecInHour

RETURN
END SUBROUTINE ReportPIU

! ===================== Utilities =====================================

FUNCTION PIUnitHasMixer(CompName) RESULT(YesNo)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given a mixer name, this routine determines if that mixer is found on
          ! PIUnits.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName  ! component (mixer) name
  LOGICAL :: YesNo  ! True if found

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ItemNum

  IF (GetPIUInputFlag) THEN
    CALL GetPIUs
    GetPIUInputFlag = .FALSE.
  END IF

  YesNo=.false.
  IF (NumPIUs > 0) THEN
    ItemNum=FindItemInList(CompName,PIU%MixerName,NumPIUs)
    IF (ItemNum > 0) YesNo=.true.
  ENDIF

  RETURN

END FUNCTION PIUnitHasMixer

SUBROUTINE PIUInducesPlenumAir(NodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Marks a PIU air terminal unit as obtaining its induced air from
          ! a plenum.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NodeNum  ! induced air node number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PIUIndex

  IF (GetPIUInputFlag) THEN
    CALL GetPIUs
    GetPIUInputFlag = .FALSE.
  END IF

  DO PIUIndex=1,NumPIUs
    IF (NodeNum == PIU(PIUIndex)%SecAirInNode) THEN
      PIU(PIUIndex)%InducesPlenumAir = .TRUE.
      EXIT
    END IF
  END DO

  RETURN

END SUBROUTINE PIUInducesPlenumAir


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

END MODULE PoweredInductionUnits
