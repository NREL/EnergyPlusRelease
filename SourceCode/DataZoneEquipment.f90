MODULE DataZoneEquipment      ! EnergyPlus Data-Only Module


          ! MODULE INFORMATION
          !             AUTHOR:  Russ Taylor
          !       DATE WRITTEN:  June 1998



          ! PURPOSE OF THIS MODULE:
          ! This module contains variable declarations for zone equipment configuration data


USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, NumOfZones
IMPLICIT NONE   ! Enforce explicit typing of all variables


PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.



          ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: PathInlet       = 1
  INTEGER, PARAMETER :: CompInlet       = 2
  INTEGER, PARAMETER :: Intermediate    = 3
  INTEGER, PARAMETER :: Outlet          = 4

  INTEGER, PARAMETER :: ZoneSplitter_Type     = 1
  INTEGER, PARAMETER :: ZoneSupplyPlenum_Type = 2
  INTEGER, PARAMETER :: ZoneMixer_Type = 3
  INTEGER, PARAMETER :: ZoneReturnPlenum_Type = 4

! Start zone equip objects
! list units that are valid for zone system availability managers first
  INTEGER, PARAMETER :: FanCoil4Pipe_Num         = 1
  INTEGER, PARAMETER :: PkgTermHPAirToAir_Num    = 2
  INTEGER, PARAMETER :: PkgTermACAirToAir_Num    = 3
  INTEGER, PARAMETER :: PkgTermHPWaterToAir_Num  = 4
  INTEGER, PARAMETER :: WindowAC_Num             = 5
  INTEGER, PARAMETER :: UnitHeater_Num           = 6
  INTEGER, PARAMETER :: UnitVentilator_Num       = 7
  INTEGER, PARAMETER :: ERVStandAlone_Num        = 8
  INTEGER, PARAMETER :: VentilatedSlab_Num       = 9
  INTEGER, PARAMETER :: OutdoorAirUnit_Num       = 10
  INTEGER, PARAMETER :: VRFTerminalUnit_Num      = 11
  INTEGER, PARAMETER :: PurchasedAir_Num         = 12
  INTEGER, PARAMETER :: ZoneEvaporativeCoolerUnit_Num = 13
  INTEGER, PARAMETER :: AirDistUnit_Num          = 14
  INTEGER, PARAMETER :: DirectAir_Num            = 15
  INTEGER, PARAMETER :: BBWaterConvective_Num    = 16
  INTEGER, PARAMETER :: BBElectricConvective_Num = 17
  INTEGER, PARAMETER :: HiTempRadiant_Num        = 18
  INTEGER, PARAMETER :: LoTempRadiant_Num        = 19
  INTEGER, PARAMETER :: ZoneExhaustFan_Num       = 20
  INTEGER, PARAMETER :: HeatXchngr_Num           = 21
  INTEGER, PARAMETER :: HPWaterHeater_Num        = 22
  INTEGER, PARAMETER :: BBWater_Num              = 23
  INTEGER, PARAMETER :: ZoneDXDehumidifier_Num   = 24
  INTEGER, PARAMETER :: BBSteam_Num              = 25
  INTEGER, PARAMETER :: BBElectric_Num           = 26
  INTEGER, PARAMETER :: RefrigerationAirChillerSet_Num   = 27
  INTEGER, PARAMETER :: UserDefinedZoneHVACForcedAir_Num = 28
  INTEGER, PARAMETER :: ZoneUnitarySystem_Num = 29 ! AirloopHVAC:UnitarySystem configured as zone equipment
!
  INTEGER, PARAMETER :: TotalNumZoneEquipType = 29
  ! **NOTE**... if you add another zone equipment object, then increment
  ! TotalNumZoneEquipType above to match the total number of zone equipment types
! End zone equip objects


  INTEGER, PARAMETER :: NumValidSysAvailZoneComponents = 13
  CHARACTER(len=*), PARAMETER, DIMENSION(NumValidSysAvailZoneComponents) :: cValidSysAvailManagerCompTypes=    &
                 (/'ZoneHVAC:FourPipeFanCoil                        ',     &
                   'ZoneHVAC:PackagedTerminalHeatPump               ',     &
                   'ZoneHVAC:PackagedTerminalAirConditioner         ',     &
                   'ZoneHVAC:WaterToAirHeatPump                     ',     &
                   'ZoneHVAC:WindowAirConditioner                   ',     &
                   'ZoneHVAC:UnitHeater                             ',     &
                   'ZoneHVAC:UnitVentilator                         ',     &
                   'ZoneHVAC:EnergyRecoveryVentilator               ',     &
                   'ZoneHVAC:VentilatedSlab                         ',     &
                   'ZoneHVAC:OutdoorAirUnit                         ',     &
                   'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow   ',     &
                   'ZoneHVAC:IdealLoadsAirSystem                    ',     &
                   'ZoneHVAC:EvaporativeCoolerUnit                  ' /)

          ! DERIVED TYPE DEFINITIONS:
  TYPE EquipMeterData
    CHARACTER(len=MaxNameLength)          :: ReportVarName      =''
    CHARACTER(len=MaxNameLength)          :: ReportVarUnits     =''
    INTEGER                               :: ResourceType       =0
    CHARACTER(len=MaxNameLength)          :: EndUse             =''
    INTEGER                               :: EndUse_CompMode    =0
    CHARACTER(len=MaxNameLength)          :: Group              =''
    INTEGER                               :: ReportVarIndex     =0
    INTEGER                               :: ReportVarIndexType =0
    INTEGER                               :: ReportVarType      =0
    REAL(r64)                             :: CurMeterReading    =0.0d0
  END TYPE EquipMeterData

  TYPE SubSubEquipmentData  !data for an individual component
    CHARACTER(len=MaxNameLength)   :: TypeOf            =''        ! The 'keyWord' identifying  component type
    CHARACTER(len=MaxNameLength)   :: Name              =''         ! Component name
    INTEGER                        :: EquipIndex        =0     ! Component Index for routines
    LOGICAL                        :: ON            =.true. ! When true, the designated component or operation scheme is available
    INTEGER                        :: InletNodeNum      =0
    INTEGER                        :: OutletNodeNum     =0
    INTEGER                        :: NumMeteredVars    =0
    TYPE(EquipMeterData),ALLOCATABLE, DIMENSION(:)  :: MeteredVar    !Index of energy output report data
    INTEGER                        :: EnergyTransComp  =0 !1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
    INTEGER                        :: ZoneEqToPlantPtr =0 !0=No plant loop connection, >=0 index to ZoneEqToPlant array
    INTEGER                        :: OpMode = 0
    REAL(r64)                      :: Capacity=0.0d0
    REAL(r64)                      :: Efficiency=0.0d0
    REAL(r64)                      :: TotPlantSupplyElec    =0.0d0
    REAL(r64)                      :: PlantSupplyElecEff    =0.0d0
    REAL(r64)                      :: PeakPlantSupplyElecEff    =0.0d0
    REAL(r64)                      :: TotPlantSupplyGas     =0.0d0
    REAL(r64)                      :: PlantSupplyGasEff     =0.0d0
    REAL(r64)                      :: PeakPlantSupplyGasEff     =0.0d0
    REAL(r64)                      :: TotPlantSupplyPurch   =0.0d0
    REAL(r64)                      :: PlantSupplyPurchEff   =0.0d0
    REAL(r64)                      :: PeakPlantSupplyPurchEff   =0.0d0
    REAL(r64)                      :: TotPlantSupplyOther   =0.0d0
    REAL(r64)                      :: PlantSupplyOtherEff   =0.0d0
    REAL(r64)                      :: PeakPlantSupplyOtherEff   =0.0d0
  END TYPE SubSubEquipmentData

  TYPE SubEquipmentData  !data for an individual component
    LOGICAL                        :: Parent            =.FALSE. ! When true, the designated component is made up of sub-components
    INTEGER                        :: NumSubSubEquip    =0
    CHARACTER(len=MaxNameLength)   :: TypeOf            =''        ! The 'keyWord' identifying  component type
    CHARACTER(len=MaxNameLength)   :: Name              =''        ! Component name
    INTEGER                        :: EquipIndex        =0     ! Component Index for routines
    LOGICAL                        :: ON            =.true. ! When true, the designated component or operation scheme is available
    INTEGER                        :: InletNodeNum      =0
    INTEGER                        :: OutletNodeNum     =0
    INTEGER                        :: NumMeteredVars    =0
    TYPE(EquipMeterData),ALLOCATABLE,DIMENSION(:)  :: MeteredVar    !Index of energy output report data
    TYPE(SubSubEquipmentData), ALLOCATABLE, DIMENSION(:)   :: SubSubEquipData             ! Component list
    INTEGER                        :: EnergyTransComp  =0 !1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
    INTEGER                        :: ZoneEqToPlantPtr =0 !0=No plant loop connection, >0 index to ZoneEqToPlant array
    INTEGER                        :: OpMode = 0
    REAL(r64)                      :: Capacity=0.0d0
    REAL(r64)                      :: Efficiency=0.0d0
    REAL(r64)                      :: TotPlantSupplyElec    =0.0d0
    REAL(r64)                      :: PlantSupplyElecEff    =0.0d0
    REAL(r64)                      :: PeakPlantSupplyElecEff    =0.0d0
    REAL(r64)                      :: TotPlantSupplyGas     =0.0d0
    REAL(r64)                      :: PlantSupplyGasEff     =0.0d0
    REAL(r64)                      :: PeakPlantSupplyGasEff     =0.0d0
    REAL(r64)                      :: TotPlantSupplyPurch   =0.0d0
    REAL(r64)                      :: PlantSupplyPurchEff   =0.0d0
    REAL(r64)                      :: PeakPlantSupplyPurchEff   =0.0d0
    REAL(r64)                      :: TotPlantSupplyOther   =0.0d0
    REAL(r64)                      :: PlantSupplyOtherEff   =0.0d0
    REAL(r64)                      :: PeakPlantSupplyOtherEff   =0.0d0
  END TYPE SubEquipmentData

  TYPE AirIn
    INTEGER   :: InNode              =0 ! Air distribution unit inlet node
    INTEGER   :: OutNode             =0 ! Air distribution unit Outlet node
    LOGICAL   :: SupplyAirPathExists =.FALSE.
    INTEGER   :: MainBranchIndex     =0
    INTEGER   :: SupplyBranchIndex   =0
    INTEGER   :: AirDistUnitIndex    =0    ! equipment number in EquipList
    INTEGER   :: SupplyAirPathIndex  =0
    REAL(r64) :: NetBranchCoilDemand =0.0d0
    TYPE (SubSubEquipmentData), ALLOCATABLE, DIMENSION(:) :: Coil
  END TYPE AirIn

  TYPE EquipConfiguration
    CHARACTER(len=MaxNameLength)           :: ZoneName         = 'Uncontrolled Zone' !
    INTEGER                                :: ActualZoneNum    = 0   ! index into the Zone data
    CHARACTER(len=MaxNameLength)           :: EquipListName    = ' ' !
    INTEGER                                :: EquipListIndex   = 0 !
    CHARACTER(len=MaxNameLength)           :: ControlListName  = ' ' !
    INTEGER                                :: ZoneNode         = 0   !
    INTEGER                                :: ReturnAirNode    = 0   !
    INTEGER                                :: NumInletNodes    = 0   !
    INTEGER                                :: NumExhaustNodes  = 0   !
    LOGICAL                                :: FlowError        = .FALSE. ! flow error flag
    INTEGER, DIMENSION(:), ALLOCATABLE     :: InletNode               ! zone supply air inlet nodes
    INTEGER, DIMENSION(:), ALLOCATABLE     :: ExhaustNode           ! zone air exhaust nodes
    INTEGER                                :: ReturnZonePlenumCondNum = 0   ! number of the zone's return air plenum
    INTEGER                                :: AirLoopNum       = 0    ! the air loop index for this controlled zone
    INTEGER                                :: FanOpMode        = 0    ! =0 if no central sys;
                                                                      ! -1 if central sys is in cycling fan mode;
                                                                      ! =2 if central sysis in constant fan mode.
    LOGICAL                                :: ZonalSystemOnly=.FALSE. ! TRUE if served by a zonal system (only)
    LOGICAL                                :: IsControlled =.false.  ! True when this is a controlled zone.
    REAL(r64)                              :: ZoneExh          = 0.d0 ! zone exhaust (unbalanced) mass flow rate [kg/s]
    REAL(r64)                              :: ZoneExhBalanced  = 0.d0 ! balanced zone exhaust mass flow rate [kg/s]
    REAL(r64)                              :: PlenumMassFlow   = 0.d0  ! zone air mass flow rate induced from plenum [kg/s]
                        ! AirDistUnitCool and AirDistUnitHeat
                        ! do not correspond with the AIR DISTRIBUTION UNIT object in the zone equipment list.
                        ! AirDistUnitCool/AirDistUnitHeat, may represent a DIRECT AIR object,
                        ! or the cold/hot side of AIR DISTRIBUTION
                        ! UNIT object.  That is both AirDistUnitHeat and AirDistUnitCool are required to describe a dual
                        ! duct AIR DISTRIBUTION object in the ZoneEquipList.  Although only one AIR DISTRIBUTION UNIT is
                        ! allowed in ZoneEquipList, two instances of that object may exist in this data structure
    TYPE (AirIn), DIMENSION(:),ALLOCATABLE :: AirDistUnitHeat   ! dimensioned to number of zone inlet nodes
    TYPE (AirIn), DIMENSION(:),ALLOCATABLE :: AirDistUnitCool   ! dimensioned to number of zone inlet nodes.
    LOGICAL                                :: SupLeakToRetPlen  = .FALSE.! True if there is supply duct leak to the
                                                                                   ! plenum (simple duct leakage model)
    LOGICAL                                :: InFloorActiveElement = .FALSE. !Convection adapation, true if zone has in-floor HVAC
    LOGICAL                                :: InWallActiveElement  = .FALSE. !Convection adapation, true if zone has in-wall HVAC
    LOGICAL                                :: InCeilingActiveElement = .FALSE. !Convection adapation,
                                                                               ! true when zone has in-ceiling HVAC
  END TYPE EquipConfiguration

  TYPE EquipmentData  !data for an individual component
    LOGICAL                        :: Parent            =.FALSE.  ! When true, the designated component is made up of sub-components
    INTEGER                        :: NumSubEquip       =0
    CHARACTER(len=MaxNameLength)   :: TypeOf            =''       ! The 'keyWord' identifying  component type
    CHARACTER(len=MaxNameLength)   :: Name              =''       ! Component name
    LOGICAL                        :: ON            =.true. ! When true, the designated component or operation scheme is available
    INTEGER                        :: NumInlets         =0
    INTEGER                        :: NumOutlets        =0
    INTEGER,ALLOCATABLE,  DIMENSION(:) :: InletNodeNums
    INTEGER,ALLOCATABLE,  DIMENSION(:) :: OutletNodeNums
    INTEGER                        :: NumMeteredVars    =0
    TYPE(EquipMeterData),ALLOCATABLE,  DIMENSION(:)    :: MeteredVar    !Index of energy output report data
    TYPE(SubEquipmentData), ALLOCATABLE, DIMENSION(:)   :: SubEquipData             ! Component list
    INTEGER                        :: EnergyTransComp  =0 !1=EnergyTransfer, 0=No EnergyTransfer  Flag needed for reporting
    INTEGER                        :: ZoneEqToPlantPtr =0 !0=No plant loop connection, >0 index to ZoneEqToPlant array
    REAL(r64)                      :: TotPlantSupplyElec    =0.0d0
    REAL(r64)                      :: PlantSupplyElecEff    =0.0d0
    REAL(r64)                      :: PeakPlantSupplyElecEff    =0.0d0
    REAL(r64)                      :: TotPlantSupplyGas     =0.0d0
    REAL(r64)                      :: PlantSupplyGasEff     =0.0d0
    REAL(r64)                      :: PeakPlantSupplyGasEff     =0.0d0
    REAL(r64)                      :: TotPlantSupplyPurch   =0.0d0
    REAL(r64)                      :: PlantSupplyPurchEff   =0.0d0
    REAL(r64)                      :: PeakPlantSupplyPurchEff   =0.0d0
    REAL(r64)                      :: TotPlantSupplyOther   =0.0d0
    REAL(r64)                      :: PlantSupplyOtherEff   =0.0d0
    REAL(r64)                      :: PeakPlantSupplyOtherEff   =0.0d0
    REAL(r64)                      :: Capacity=0.0d0
    INTEGER                        :: OpMode = 0
  END TYPE EquipmentData

  TYPE EquipList
    CHARACTER(len=MaxNameLength)                            :: Name               = ' '  ! Name of the equipment list
    INTEGER                                                 :: NumOfEquipTypes    = 0    ! Number of items on this list
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: EquipType
    INTEGER, DIMENSION(:), ALLOCATABLE                      :: EquipType_Num
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: EquipName
    INTEGER, DIMENSION(:), ALLOCATABLE                      :: EquipIndex
    INTEGER, DIMENSION(:), ALLOCATABLE                      :: CoolingPriority
    INTEGER, DIMENSION(:), ALLOCATABLE                      :: HeatingPriority
    TYPE(EquipmentData),ALLOCATABLE, DIMENSION(:)           :: EquipData !Index of energy output report data
  END TYPE EquipList

  TYPE ControlList
    CHARACTER(len=MaxNameLength) :: Name           = ' ' !
    INTEGER   :: NumOfControls                     = 0 !
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: ControlType
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: ControlName
  END TYPE ControlList

  TYPE SupplyAir
    CHARACTER(len=MaxNameLength)                                :: Name                 = ' ' !
    INTEGER                                                     :: NumOfComponents      = 0   !
    INTEGER                                                     :: InletNodeNum         = 0   !
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE     :: ComponentType
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: ComponentType_Num
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE     :: ComponentName
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: ComponentIndex
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: SplitterIndex
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: PlenumIndex
    INTEGER                                                     :: NumOutletNodes       = 0
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: OutletNode
    INTEGER                                                     :: NumNodes             = 0
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: Node
    INTEGER, DIMENSION(:), ALLOCATABLE                          :: NodeType
  END TYPE SupplyAir

  TYPE ReturnAir
    CHARACTER(len=MaxNameLength) :: Name              = ' ' !
    INTEGER   :: NumOfComponents                      = 0   !
    INTEGER   :: OutletNodeNum                        = 0   !
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE    :: ComponentType
    INTEGER, DIMENSION(:), ALLOCATABLE                         :: ComponentType_Num
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE    :: ComponentName
    INTEGER, DIMENSION(:), ALLOCATABLE :: ComponentIndex
  END TYPE ReturnAir

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumSupplyAirPaths      =0
  INTEGER :: NumReturnAirPaths      =0
  LOGICAL :: ZoneEquipInputsFilled = .FALSE.
  LOGICAL :: ZoneEquipSimulatedOnce = .FALSE.
  INTEGER :: NumofZoneEquipLists  = 0 ! The Number of Zone Equipment List objects
  INTEGER, ALLOCATABLE :: ZoneEquipAvail(:)

  TYPE (EquipConfiguration), ALLOCATABLE, DIMENSION(:) :: ZoneEquipConfig
  TYPE (EquipList), ALLOCATABLE, DIMENSION(:)          :: ZoneEquipList
  TYPE (ControlList), ALLOCATABLE, DIMENSION(:)        :: HeatingControlList
  TYPE (ControlList), ALLOCATABLE, DIMENSION(:)        :: CoolingControlList
  TYPE (SupplyAir), ALLOCATABLE, DIMENSION(:)          :: SupplyAirPath
  TYPE (ReturnAir), ALLOCATABLE, DIMENSION(:)          :: ReturnAirPath

          ! Utility routines for module
PUBLIC  GetZoneEquipmentData
PRIVATE GetZoneEquipmentData1
PUBLIC  CheckZoneEquipmentList
PUBLIC  GetControlledZoneIndex
PUBLIC  GetSystemNodeNumberForZone
PUBLIC  GetReturnAirNodeForZone
PUBLIC  CalcDesignSpecificationOutdoorAir
PUBLIC FindControlledZoneIndexFromSystemNodeNumberForZone

CONTAINS

SUBROUTINE GetZoneEquipmentData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is a stub routine to allow an outside module (ZoneEquipmentManager) to get input while
          ! allowing the routine itself to remain PRIVATE to this module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

  CALL GetZoneEquipmentData1

  RETURN

END SUBROUTINE GetZoneEquipmentData

SUBROUTINE GetZoneEquipmentData1

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1997
          !       MODIFIED       Aug 2003, FCW: set ZoneEquipConfig number for each zone
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get all the system related equipment which may be attached to
          ! a zone

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
     USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindIteminList, GetObjectItemNum, VerifyName,  &
                               GetObjectDefMaxArgs, MakeUPPERCase, SameString
     USE DataHeatBalance, ONLY: Zone
     USE NodeInputManager, ONLY: GetOnlySingleNode, GetNodeNums, InitUniqueNodeCheck, CheckUniqueNodes, EndUniqueNodeCheck
     USE DataHVACGlobals
     USE BranchNodeConnections, ONLY: SetUpCompSets
     USE DataLoopNode
     USE General, ONLY: TrimSigDigits, RoundSigDigits
     USE DataGlobals, ONLY: NumOfZones
     USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError, ShowFatalError, ShowWarningError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetZoneEquipmentData1: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
TYPE EquipListAudit
  CHARACTER(len=MaxNameLength) :: ObjectType=' '
  CHARACTER(len=MaxNameLength) :: ObjectName=' '
  INTEGER :: OnListNum=0
END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: NumAlphas
INTEGER :: NumNums
INTEGER :: NodeNum
INTEGER :: PathNum
INTEGER :: CompNum
INTEGER :: ControlledZoneNum
INTEGER :: ControlledZoneLoop
INTEGER :: ZoneEquipTypeNum
INTEGER :: ZoneEquipListNum
INTEGER :: IOSTAT
CHARACTER(len=MaxNameLength) :: InletNodeListName
CHARACTER(len=MaxNameLength) :: ExhaustNodeListName
CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: AlphArray
REAL(r64), DIMENSION(:), ALLOCATABLE :: NumArray
INTEGER MaxAlphas
INTEGER MaxNums
INTEGER NumParams
INTEGER :: NumNodes
INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNums
INTEGER :: Counter
LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
LOGICAL       :: IsNotOK               ! Flag to verify name
LOGICAL       :: IsBlank               ! Flag for blank name
LOGICAL :: NodeListError
LOGICAL :: UniqueNodeError
INTEGER :: NumOfControlledZones ! The number of Controlled Zone Equip Configuration objects
CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and error messages
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
LOGICAL :: IdealLoadsOnEquipmentList
INTEGER :: found = 0
INTEGER :: maxEquipCount
INTEGER :: numEquipCount
TYPE (EquipListAudit), ALLOCATABLE, DIMENSION(:) :: ZoneEquipListAcct
INTEGER :: overallEquipCount
INTEGER :: Loop1
INTEGER :: Loop2


ExhaustNodeListName = ' '
InletNodeListName = ' '

! Look in the input file for zones with air loop and zone equipment attached

NumOfControlledZones = GetNumObjectsFound('ZoneHVAC:EquipmentConnections')
NumOfZoneEquipLists = GetNumObjectsFound('ZoneHVAC:EquipmentList') ! Look for lists of equipment data - there should
                                                                ! be as many of these as there are controlled zones
CALL GetObjectDefMaxArgs('NodeList',NumParams,NumAlphas,NumNums)
ALLOCATE(NodeNums(NumParams))
NodeNums=0
CALL GetObjectDefMaxArgs('ZoneHVAC:EquipmentList',NumParams,NumAlphas,NumNums)
MaxAlphas=NumAlphas
MaxNums=NumNums
CALL GetObjectDefMaxArgs('ZoneHVAC:EquipmentConnections',NumParams,NumAlphas,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNums=MAX(MaxNums,NumNums)
CALL GetObjectDefMaxArgs('AirLoopHVAC:SupplyPath',NumParams,NumAlphas,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNums=MAX(MaxNums,NumNums)
CALL GetObjectDefMaxArgs('AirLoopHVAC:ReturnPath',NumParams,NumAlphas,NumNums)
MaxAlphas=MAX(MaxAlphas,NumAlphas)
MaxNums=MAX(MaxNums,NumNums)
ALLOCATE(AlphArray(MaxAlphas))
AlphArray=' '
ALLOCATE(NumArray(MaxNums))
NumArray=0.0d0
ALLOCATE(cAlphaFields(MaxAlphas))
cAlphaFields=' '
ALLOCATE(cNumericFields(MaxNums))
cNumericFields=' '
ALLOCATE(lAlphaBlanks(MaxAlphas))
lAlphaBlanks=.true.
ALLOCATE(lNumericBlanks(MaxNums))
lNumericBlanks=.true.

IF (.not. ALLOCATED(SupplyAirPath)) THEN
              ! Look for and read in the air supply path
              ! component (splitters) information for each zone
  NumSupplyAirPaths = GetNumObjectsFound('AirLoopHVAC:SupplyPath')
  ALLOCATE (SupplyAirPath(NumSupplyAirPaths))
ENDIF

IF (.not. ALLOCATED(ReturnAirPath)) THEN
              ! Look for and read in the air return path
              ! component (mixers & plenums) information for each zone
  NumReturnAirPaths = GetNumObjectsFound('AirLoopHVAC:ReturnPath')
  ALLOCATE (ReturnAirPath(NumReturnAirPaths))
ENDIF

ALLOCATE (ZoneEquipConfig(NumOfZones)) ! Allocate the array containing the configuration
                                                 ! data for each zone to the number of controlled zones
                                                 ! found in the input file.  This may or may not
                                                 ! be the same as the number of zones in the building
ALLOCATE (ZoneEquipList(NumOfZones))
ALLOCATE (ZoneEquipAvail(NumOfZones))
ZoneEquipAvail = NoAction

IF (NumOfZoneEquipLists /= NumOfControlledZones) THEN
  CALL ShowSevereError(RoutineName//'Number of Zone Equipment lists ['//TRIM(TrimSigDigits(NumOfZoneEquipLists))//  &
       '] not equal Number of Controlled Zones ['//TRIM(TrimSigDigits(NumOfControlledZones))//']')
  CALL ShowContinueError('..Each Controlled Zone [ZoneHVAC:EquipmentConnections] must have a corresponding'//  &
                     '(unique) ZoneHVAC:EquipmentList')
  CALL ShowFatalError('GetZoneEquipment: Incorrect number of zone equipment lists')
END IF

IF (NumOfControlledZones > NumOfZones) THEN
  CALL ShowSevereError(RoutineName//'Number of Controlled Zone objects ['//TRIM(TrimSigDigits(NumOfControlledZones))//  &
       '] greater than Number of Zones ['//TRIM(TrimSigDigits(NumOfZones))//']')
  CALL ShowFatalError(RoutineName//'Too many ZoneHVAC:EquipmentConnections objects.')
ENDIF


CALL InitUniqueNodeCheck('ZoneHVAC:EquipmentConnections')

  overallEquipCount=0

DO ControlledZoneLoop = 1,NumOfControlledZones

  CurrentModuleObject = 'ZoneHVAC:EquipmentConnections'

  CALL GetObjectItem(CurrentModuleObject,ControlledZoneLoop,AlphArray,NumAlphas, &  ! Get Equipment
                     NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields) !  data for one zone

  ControlledZoneNum    = FindItemInList(AlphArray(1),Zone%Name,NumOfZones)

  IF (ControlledZoneNum == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '//  &
                         TRIM(cAlphaFields(1))//'="'//TRIM(AlphArray(1))//'"')
    CALL ShowContinueError('..Requested Controlled Zone not among Zones, remaining items for this object not processed.')
    ErrorsFound=.true.
    CYCLE
  ELSE
!    Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%ZoneEquipConfigNum = ControlledZoneNum
    IF (Zone(ControlledZoneNum)%IsControlled) THEN
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '//  &
                         TRIM(cAlphaFields(1))//'="'//TRIM(AlphArray(1))//'"')
      CALL ShowContinueError('..Duplicate Controlled Zone entered, only one '//TRIM(CurrentModuleObject)//' per zone is allowed.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    Zone(ControlledZoneNum)%IsControlled = .true.
    ZoneEquipConfig(ControlledZoneNum)%IsControlled=.true.
    ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum=ControlledZoneNum
  ENDIF
  ZoneEquipConfig(ControlledZoneNum)%ZoneName = AlphArray(1) ! for x-referencing with the geometry data

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(AlphArray(2),ZoneEquipConfig%EquipListName,ControlledZoneLoop-1,IsNotOK,IsBlank,  &
                  TRIM(CurrentModuleObject)//TRIM(cAlphaFields(2)))
  IF (IsNotOK) THEN
    CALL ShowContinueError('..another Controlled Zone has been assigned that '//TRIM(cAlphaFields(2))//'.')
    ErrorsFound=.true.
    IF (IsBlank) AlphArray(2)='xxxxx'
  ENDIF
  ZoneEquipConfig(ControlledZoneNum)%EquipListName = AlphArray(2) ! the name of the list containing all the zone eq.
  InletNodeListName = AlphArray(3)
  ExhaustNodeListName = AlphArray(4)
  ZoneEquipConfig(ControlledZoneNum)%ZoneNode         = & ! all zone air state variables are
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_ZoneNode,1,ObjectIsNotParent)
  IF (ZoneEquipConfig(ControlledZoneNum)%ZoneNode == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '//  &
                         TRIM(cAlphaFields(1))//'="'//TRIM(AlphArray(1))//'", invalid')
    CALL ShowContinueError(TRIM(cAlphaFields(5))//' must be present.')
    ErrorsFound=.true.
  ELSE
    UniqueNodeError=.false.
    CALL CheckUniqueNodes(cAlphaFields(5),'NodeName',UniqueNodeError,CheckName=AlphArray(5),ObjectName=AlphArray(1))
    IF (UniqueNodeError) THEN
!      CALL ShowContinueError('Occurs for '//TRIM(cAlphaFields(1))//' = '//TRIM(AlphArray(1)))
      ErrorsFound=.true.
    ENDIF
  ENDIF
                                                                    ! assigned to this node
  IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum > 0) THEN
    Zone(ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum)%SystemZoneNodeNumber = ZoneEquipConfig(ControlledZoneNum)%ZoneNode
  ENDIF  ! This error already detected and program will be terminated.

  ZoneEquipConfig(ControlledZoneNum)%ReturnAirNode    = & ! all return air state variables are
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_ZoneReturn,1,ObjectIsNotParent)
                                                                    ! assigned to this node
  IF (ZoneEquipConfig(ControlledZoneNum)%ReturnAirNode /= 0) THEN
    UniqueNodeError=.false.
    CALL CheckUniqueNodes(cAlphaFields(6),'NodeName',UniqueNodeError,CheckName=AlphArray(6),ObjectName=AlphArray(1))
    IF (UniqueNodeError) THEN
!      CALL ShowContinueError('Occurs for '//TRIM(cAlphaFields(1))//' = '//TRIM(AlphArray(1)))
      ErrorsFound=.true.
    ENDIF
  ENDIF

  ! Read in the equipment type, name and sequence information
  ! for each equipment list

  CurrentModuleObject = 'ZoneHVAC:EquipmentList'

  ZoneEquipListNum = GetObjectItemNum(TRIM(CurrentModuleObject),ZoneEquipConfig(ControlledZoneNum)%EquipListName)
  IF (ZoneEquipListNum > 0) THEN

    CALL GetObjectItem(CurrentModuleObject,ZoneEquipListNum,AlphArray,NumAlphas, &
                       NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields) !  data for one zone
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(AlphArray(1),ZoneEquipList%Name,ControlledZoneNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      CALL ShowContinueError('Bad Zone Equipment name in '//trim(CurrentModuleObject)//'="'//  &
                             TRIM(ZoneEquipConfig(ControlledZoneNum)%EquipListName)//'"')
      CALL ShowContinueError('For Zone="'//TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName)//'".')
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    ZoneEquipList(ControlledZoneNum)%Name =AlphArray(1)

    maxEquipCount=0
    numEquipCount=(NumAlphas-1)/2
    if (numEquipCount*2 /= (NumAlphas-1)) numEquipCount=numEquipCount+1
    DO ZoneEquipTypeNum = 1,numEquipCount
      IF (.not. lAlphaBlanks(2*ZoneEquipTypeNum) .and. .not. lAlphaBlanks(2*ZoneEquipTypeNum + 1)) THEN
        maxEquipCount=maxEquipCount+1
        CYCLE
      ENDIF
      CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'//trim(ZoneEquipList(ControlledZoneNum)%Name)//  &
         '", truncated list at blank field; object count='//trim(RoundSigDigits(maxEquipCount)))
      EXIT
    ENDDO

    overallEquipCount=overallEquipCount+maxEquipCount
    ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes = maxEquipCount
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%EquipType(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%EquipName(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%EquipIndex(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%EquipData(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%CoolingPriority(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ALLOCATE(ZoneEquipList(ControlledZoneNum)%HeatingPriority(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))
    ZoneEquipList(ControlledZoneNum)%EquipType=' '
    ZoneEquipList(ControlledZoneNum)%EquipType_Num=0
    ZoneEquipList(ControlledZoneNum)%EquipName=' '
    ZoneEquipList(ControlledZoneNum)%EquipIndex=0
    ZoneEquipList(ControlledZoneNum)%CoolingPriority=0
    ZoneEquipList(ControlledZoneNum)%HeatingPriority=0

    IdealLoadsOnEquipmentList=.false.

    DO ZoneEquipTypeNum = 1, ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes
      ZoneEquipList(ControlledZoneNum)%EquipType(ZoneEquipTypeNum) = &
                    AlphArray(2*ZoneEquipTypeNum)
      ZoneEquipList(ControlledZoneNum)%EquipName(ZoneEquipTypeNum) = &
                    AlphArray(2*ZoneEquipTypeNum + 1)
      CALL ValidateComponent(ZoneEquipList(ControlledZoneNum)%EquipType(ZoneEquipTypeNum),  &
                             ZoneEquipList(ControlledZoneNum)%EquipName(ZoneEquipTypeNum),  &
                             IsNotOK,TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'='//TRIM(ZoneEquipList(ControlledZoneNum)%Name))
        ErrorsFound=.true.
      ENDIF
      ZoneEquipList(ControlledZoneNum)%CoolingPriority(ZoneEquipTypeNum) = &
                    NINT(NumArray(2*ZoneEquipTypeNum-1))
      IF ((ZoneEquipList(ControlledZoneNum)%CoolingPriority(ZoneEquipTypeNum) <= 0) .OR. &
          (ZoneEquipList(ControlledZoneNum)%CoolingPriority(ZoneEquipTypeNum) > &
            ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'".')
        CALL ShowContinueError('invalid '//TRIM(cNumericFields(2*ZoneEquipTypeNum-1))//'=['&
              //TRIM(RoundSigDigits(ZoneEquipList(ControlledZoneNum)%CoolingPriority(ZoneEquipTypeNum)))//'].')
        CALL ShowContinueError('equipment sequence must be > 0 and <= number of equipments in the list.')
        IF (ZoneEquipList(ControlledZoneNum)%CoolingPriority(ZoneEquipTypeNum) > 0) &
          CALL ShowContinueError('only '//trim(RoundSigDigits(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))//' in the list.')
        ErrorsFound=.true.
      ENDIF

      ZoneEquipList(ControlledZoneNum)%HeatingPriority(ZoneEquipTypeNum) = &
                    NINT(NumArray(2*ZoneEquipTypeNum))
      IF ((ZoneEquipList(ControlledZoneNum)%HeatingPriority(ZoneEquipTypeNum) <= 0) .OR. &
          (ZoneEquipList(ControlledZoneNum)%HeatingPriority(ZoneEquipTypeNum) > &
            ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes)) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(AlphArray(1))//'".')
        CALL ShowContinueError('invalid '//TRIM(cNumericFields(2*ZoneEquipTypeNum))//'=['&
              //TRIM(RoundSigDigits(ZoneEquipList(ControlledZoneNum)%HeatingPriority(ZoneEquipTypeNum)))//'].')
        CALL ShowContinueError('equipment sequence must be > 0 and <= number of equipments in the list.')
        IF (ZoneEquipList(ControlledZoneNum)%HeatingPriority(ZoneEquipTypeNum) > 0) &
          CALL ShowContinueError('only '//trim(RoundSigDigits(ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes))//' in the list.')
        ErrorsFound=.true.
      ENDIF

      SELECT CASE(TRIM(MakeUPPERCase(ZoneEquipList(ControlledZoneNum)%EquipType(ZoneEquipTypeNum))))

        CASE('ZONEHVAC:AIRDISTRIBUTIONUNIT')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=AirDistUnit_Num

        CASE('AIRTERMINAL:SINGLEDUCT:UNCONTROLLED')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=DirectAir_Num

        CASE ('ZONEHVAC:WINDOWAIRCONDITIONER') ! Window Air Conditioner
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=WindowAC_Num

        CASE ('ZONEHVAC:PACKAGEDTERMINALHEATPUMP') ! Packaged Terminal Heat Pump
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=PkgTermHPAirToAir_Num

        CASE ('ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER') ! Packaged Terminal Air Conditioner
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=PkgTermACAirToAir_Num

        CASE ('AIRLOOPHVAC:UNITARYSYSTEM') ! Unitary System
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=ZoneUnitarySystem_Num

        CASE ('ZONEHVAC:DEHUMIDIFIER:DX') ! Zone dehumidifier
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=ZoneDXDehumidifier_Num

        CASE ('ZONEHVAC:WATERTOAIRHEATPUMP') ! Zone Water to Air Heat Pump
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=PkgTermHPWaterToAir_Num

        CASE ('ZONEHVAC:FOURPIPEFANCOIL')! 4-Pipe Fan Coil
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=FanCoil4Pipe_Num

        CASE ('ZONEHVAC:UNITVENTILATOR') ! Unit Ventilator
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=UnitVentilator_Num

        CASE ('ZONEHVAC:UNITHEATER') ! Unit Heater
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=UnitHeater_Num

        CASE('ZONEHVAC:IDEALLOADSAIRSYSTEM')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=PurchasedAir_Num

        CASE('ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER') ! Hot Water Baseboard
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=BBWater_Num

        CASE('ZONEHVAC:BASEBOARD:CONVECTIVE:WATER') ! Baseboard
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=BBWaterConvective_Num

        CASE('ZONEHVAC:BASEBOARD:CONVECTIVE:ELECTRIC') ! Electric Baseboard
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=BBElectricConvective_Num

        CASE('ZONEHVAC:HIGHTEMPERATURERADIANT') ! High Temperature Radiators
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=HiTempRadiant_Num

        CASE ('ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW') ! Low temperature radiant system (hydronic)
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=LoTempRadiant_Num

        CASE ('ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW') ! Low temperature radiant system (hydronic, constant flow)
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=LoTempRadiant_Num

        CASE ('ZONEHVAC:LOWTEMPERATURERADIANT:ELECTRIC') ! Low temperature radiant system (electric)
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=LoTempRadiant_Num

        CASE('FAN:ZONEEXHAUST')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=ZoneExhaustFan_Num

        CASE ('HEATEXCHANGER:AIRTOAIR:FLATPLATE')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=HeatXchngr_Num

        CASE ('ZONEHVAC:ENERGYRECOVERYVENTILATOR')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=ERVStandAlone_Num

        CASE ('WATERHEATER:HEATPUMP')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=HPWaterHeater_Num

        CASE ('ZONEHVAC:VENTILATEDSLAB') ! Ventilated Slab
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=VentilatedSlab_Num

        CASE('ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM') ! Steam Baseboard
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=BBSteam_Num

        CASE ('ZONEHVAC:OUTDOORAIRUNIT') ! Outdoor Air Unit
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=OutdoorAirUnit_Num

        CASE('ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC') ! Radiant electric Baseboard
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=BBElectric_Num

        CASE('ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW') ! VRF AC System
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=VRFTerminalUnit_Num

        CASE('ZONEHVAC:REFRIGERATIONCHILLERSET') ! Refrigeration chiller designed for warehouse applications
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=RefrigerationAirChillerSet_Num

        CASE('ZONEHVAC:FORCEDAIR:USERDEFINED')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=UserDefinedZoneHVACForcedAir_Num

        CASE('ZONEHVAC:EVAPORATIVECOOLERUNIT')
          ZoneEquipList(ControlledZoneNum)%EquipType_Num(ZoneEquipTypeNum)=ZoneEvaporativeCoolerUnit_Num
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(ZoneEquipList(ControlledZoneNum)%Name))
          CALL ShowContinueError('..Invalid Equipment Type = '//TRIM(ZoneEquipList(ControlledZoneNum)%EquipType(ZoneEquipTypeNum)))
          ErrorsFound=.true.

      END SELECT
    END DO
    DO ZoneEquipTypeNum = 1, ZoneEquipList(ControlledZoneNum)%NumOfEquipTypes
      IF (COUNT(ZoneEquipList(ControlledZoneNum)%CoolingPriority==ZoneEquipTypeNum) > 1) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(ZoneEquipList(ControlledZoneNum)%Name))
        CALL ShowContinueError('...multiple assignments for Zone Equipment Cooling Sequence='//  &
           trim(RoundSigDigits(ZoneEquipTypeNum))//  &
           ', must be 1-1 correspondence between sequence assignments and number of equipments.')
        ErrorsFound=.true.
      ELSEIF (COUNT(ZoneEquipList(ControlledZoneNum)%CoolingPriority==ZoneEquipTypeNum) == 0) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(ZoneEquipList(ControlledZoneNum)%Name))
        CALL ShowContinueError('...zero assignments for Zone Equipment Cooling Sequence='//  &
           trim(RoundSigDigits(ZoneEquipTypeNum))//', apparent gap in sequence assignments in this equipment list.')
      ENDIF
      IF (COUNT(ZoneEquipList(ControlledZoneNum)%HeatingPriority==ZoneEquipTypeNum) > 1) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(ZoneEquipList(ControlledZoneNum)%Name))
        CALL ShowContinueError('...multiple assignments for Zone Equipment Heating or No-Load Sequence='//  &
           trim(RoundSigDigits(ZoneEquipTypeNum))//  &
           ', must be 1-1 correspondence between sequence assignments and number of equipments.')
        ErrorsFound=.true.
      ELSEIF (COUNT(ZoneEquipList(ControlledZoneNum)%HeatingPriority==ZoneEquipTypeNum) == 0) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(ZoneEquipList(ControlledZoneNum)%Name))
        CALL ShowContinueError('...zero assignments for Zone Equipment Heating or No-Load Sequence='//  &
           trim(RoundSigDigits(ZoneEquipTypeNum))//', apparent gap in sequence assignments in this equipment list.')
      ENDIF
    ENDDO

  ELSE
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' not found = '//  &
       TRIM(ZoneEquipConfig(ControlledZoneNum)%EquipListName))
    CALL ShowContinueError('In ZoneHVAC:EquipmentConnections object, for Zone = '//  &
                           TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName))
    ErrorsFound=.true.
  END IF

  ! End ZoneHVAC:EquipmentList


  NodeListError=.false.
  CALL GetNodeNums(InletNodeListName,NumNodes,NodeNums,NodeListError,NodeType_Air,'ZoneHVAC:EquipmentConnections', &
                   ZoneEquipConfig(ControlledZoneNum)%ZoneName,NodeConnectionType_ZoneInlet,1,ObjectIsNotParent)

  IF (.not. NodeListError) THEN
    ZoneEquipConfig(ControlledZoneNum)%NumInletNodes = NumNodes

    ALLOCATE(ZoneEquipConfig(ControlledZoneNum)%InletNode(NumNodes))
    ALLOCATE(ZoneEquipConfig(ControlledZoneNum)%AirDistUnitCool(NumNodes))
    ALLOCATE(ZoneEquipConfig(ControlledZoneNum)%AirDistUnitHeat(NumNodes))

    DO NodeNum = 1, NumNodes
      ZoneEquipConfig(ControlledZoneNum)%InletNode(NodeNum) = NodeNums(NodeNum)
      UniqueNodeError=.false.
      CALL CheckUniqueNodes('Zone Air Inlet Nodes','NodeNumber',UniqueNodeError,CheckNumber=NodeNums(NodeNum),   &
          ObjectName=ZoneEquipConfig(ControlledZoneNum)%ZoneName)
      IF (UniqueNodeError) THEN
!        CALL ShowContinueError('Occurs for Zone = '//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      ZoneEquipConfig(ControlledZoneNum)%AirDistUnitCool(NodeNum)%InNode = 0
      ZoneEquipConfig(ControlledZoneNum)%AirDistUnitHeat(NodeNum)%InNode = 0
      ZoneEquipConfig(ControlledZoneNum)%AirDistUnitCool(NodeNum)%OutNode = 0
      ZoneEquipConfig(ControlledZoneNum)%AirDistUnitHeat(NodeNum)%OutNode = 0
    END DO
  ELSE
    CALL ShowContinueError('Invalid inlet node or NodeList name in ZoneHVAC:EquipmentConnections object, for Zone = '// &
                           TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName))
    ErrorsFound=.true.
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(ExhaustNodeListName,NumNodes,NodeNums,NodeListError,NodeType_Air,'ZoneHVAC:EquipmentConnections', &
                   ZoneEquipConfig(ControlledZoneNum)%ZoneName,NodeConnectionType_ZoneExhaust,1,ObjectIsNotParent)

  IF (.not. NodeListError) THEN
    ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes = NumNodes

    ALLOCATE(ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(NumNodes))

    DO NodeNum = 1, NumNodes
      ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(NodeNum) = NodeNums(NodeNum)
      UniqueNodeError=.false.
      CALL CheckUniqueNodes('Zone Air Exhaust Nodes','NodeNumber',UniqueNodeError,CheckNumber=NodeNums(NodeNum),   &
          ObjectName=ZoneEquipConfig(ControlledZoneNum)%ZoneName)
      IF (UniqueNodeError) THEN
!        CALL ShowContinueError('Occurs for Zone = '//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
    END DO
  ELSE
    CALL ShowContinueError('Invalid exhaust node or NodeList name in ZoneHVAC:EquipmentConnections object, for Zone='// &
                           TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName))
    ErrorsFound=.true.
  ENDIF
END DO ! end loop over controlled zones

    IF (ErrorsFound) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//', duplicate items NOT CHECKED due to previous errors.')
      overallEquipCount=0
    ENDIF
    IF (overallEquipCount > 0) THEN
      ALLOCATE(ZoneEquipListAcct(overallEquipCount))
      overallEquipCount=0
      DO Loop1 = 1,NumOfControlledZones
        DO Loop2=1,ZoneEquipList(Loop1)%NumOfEquipTypes
          overallEquipCount=overallEquipCount+1
          ZoneEquipListAcct(overallEquipCount)%ObjectType=ZoneEquipList(Loop1)%EquipType(Loop2)
          ZoneEquipListAcct(overallEquipCount)%ObjectName=ZoneEquipList(Loop1)%EquipName(Loop2)
          ZoneEquipListAcct(overallEquipCount)%OnListNum=Loop1
        ENDDO
      ENDDO
      ! Now check for uniqueness
      DO Loop1=1,overallEquipCount
        DO Loop2=Loop1+1,overallEquipCount
          IF (ZoneEquipListAcct(Loop1)%ObjectType /= ZoneEquipListAcct(Loop2)%ObjectType  .or.  &
              ZoneEquipListAcct(Loop1)%ObjectName /= ZoneEquipListAcct(Loop2)%ObjectName) CYCLE
          ! Duplicated -- not allowed
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', duplicate items in ZoneHVAC:EquipmentList.')
          CALL ShowContinueError('Equipment: Type='//trim(ZoneEquipListAcct(Loop1)%ObjectType)//', Name='//  &
                              trim(ZoneEquipListAcct(Loop1)%ObjectName))
          CALL ShowContinueError('Found on List="'//trim(ZoneEquipList(ZoneEquipListAcct(Loop1)%OnListNum)%Name)//'".')
          CALL ShowContinueError('Equipment Duplicated on List="'//  &
             trim(ZoneEquipList(ZoneEquipListAcct(Loop2)%OnListNum)%Name)//'".')
          ErrorsFound=.true.
        ENDDO
      ENDDO
      DEALLOCATE(ZoneEquipListAcct)
    ENDIF


!map ZoneEquipConfig%EquipListIndex to ZoneEquipList%Name

DO ControlledZoneLoop = 1,NumOfZones
    found = FindItemInList(ZoneEquipList(ControlledZoneLoop)%Name, ZoneEquipConfig%EquipListName, NumOfZones)
    IF (Found > 0) ZoneEquipConfig(found)%EquipListIndex = ControlledZoneLoop
END DO ! end loop over controlled zones

CALL EndUniqueNodeCheck('ZoneHVAC:EquipmentConnections')

CurrentModuleObject = 'AirLoopHVAC:SupplyPath'
DO PathNum = 1,  NumSupplyAirPaths

  CALL GetObjectItem(CurrentModuleObject,PathNum,AlphArray,NumAlphas, &
                     NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields) !  data for one zone
  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(AlphArray(1),SupplyAirPath%Name,PathNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) AlphArray(1)='xxxxx'
  ENDIF
  SupplyAirPath(PathNum)%Name   = AlphArray(1)
  SupplyAirPath(PathNum)%NumOfComponents = NINT((REAL(NumAlphas,r64) - 2.0d0) / 2.0d0)

  SupplyAirPath(PathNum)%InletNodeNum   = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

  ALLOCATE(SupplyAirPath(PathNum)%ComponentType(SupplyAirPath(PathNum)%NumOfComponents))
  ALLOCATE(SupplyAirPath(PathNum)%ComponentType_Num(SupplyAirPath(PathNum)%NumOfComponents))
  SupplyAirPath(PathNum)%ComponentType_Num=0
  ALLOCATE(SupplyAirPath(PathNum)%ComponentName(SupplyAirPath(PathNum)%NumOfComponents))
  ALLOCATE(SupplyAirPath(PathNum)%ComponentIndex(SupplyAirPath(PathNum)%NumOfComponents))
  ALLOCATE(SupplyAirPath(PathNum)%SplitterIndex(SupplyAirPath(PathNum)%NumOfComponents))
  ALLOCATE(SupplyAirPath(PathNum)%PlenumIndex(SupplyAirPath(PathNum)%NumOfComponents))

  Counter=3

  DO CompNum = 1, SupplyAirPath(PathNum)%NumOfComponents

    IF ((AlphArray(Counter) .EQ. 'AIRLOOPHVAC:ZONESPLITTER') .OR. &
        (AlphArray(Counter) .EQ. 'AIRLOOPHVAC:SUPPLYPLENUM')) THEN

      SupplyAirPath(PathNum)%ComponentType(CompNum) = AlphArray(Counter)
      SupplyAirPath(PathNum)%ComponentName(CompNum) = AlphArray(Counter+1)
      CALL ValidateComponent(SupplyAirPath(PathNum)%ComponentType(CompNum),  &
                             SupplyAirPath(PathNum)%ComponentName(CompNum),  &
                             IsNotOK,TRIM(CurrentModuleObject))
      SupplyAirPath(PathNum)%ComponentIndex(CompNum) = 0
      SupplyAirPath(PathNum)%SplitterIndex(CompNum) = 0
      SupplyAirPath(PathNum)%PlenumIndex(CompNum) = 0
      IF (AlphArray(Counter) == 'AIRLOOPHVAC:ZONESPLITTER') SupplyAirPath(PathNum)%ComponentType_Num(CompNum)=ZoneSplitter_Type
      IF (AlphArray(Counter) == 'AIRLOOPHVAC:SUPPLYPLENUM') SupplyAirPath(PathNum)%ComponentType_Num(CompNum)=ZoneSupplyPlenum_Type

    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(1))//'="'//TRIM(SupplyAirPath(PathNum)%Name)//'"')
      CALL ShowContinueError('Unhandled component type ="'//AlphArray(Counter)//'".')
      CALL ShowContinueError('Must be "AirLoopHVAC:ZoneSplitter" or "AirLoopHVAC:SupplyPlenum"')
      ErrorsFound = .TRUE.
    ENDIF

    Counter=Counter+2

  END DO

  SupplyAirPath(PathNum)%NumOutletNodes = 0
  SupplyAirPath(PathNum)%NumNodes = 0

END DO  ! end loop over supply air paths

CurrentModuleObject = 'AirLoopHVAC:ReturnPath'
DO PathNum = 1,  NumReturnAirPaths

  CALL GetObjectItem(CurrentModuleObject,PathNum,AlphArray,NumAlphas, &
                     NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields) !  data for one zone

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(AlphArray(1),ReturnAirPath%Name,PathNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) AlphArray(1)='xxxxx'
  ENDIF
  ReturnAirPath(PathNum)%Name   = AlphArray(1)
  ReturnAirPath(PathNum)%NumOfComponents = NINT((REAL(NumAlphas,r64) - 2.0d0) / 2.0d0)

  ReturnAirPath(PathNum)%OutletNodeNum  = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

  ALLOCATE(ReturnAirPath(PathNum)%ComponentType(ReturnAirPath(PathNum)%NumOfComponents))
  ALLOCATE(ReturnAirPath(PathNum)%ComponentType_Num(ReturnAirPath(PathNum)%NumOfComponents))
  ReturnAirPath(PathNum)%ComponentType_Num=0
  ALLOCATE(ReturnAirPath(PathNum)%ComponentName(ReturnAirPath(PathNum)%NumOfComponents))
  ALLOCATE(ReturnAirPath(PathNum)%ComponentIndex(ReturnAirPath(PathNum)%NumOfComponents))

  Counter=3

  DO CompNum = 1, ReturnAirPath(PathNum)%NumOfComponents

    IF ((AlphArray(Counter) .EQ. 'AIRLOOPHVAC:ZONEMIXER') .OR. &
      (AlphArray(Counter) .EQ. 'AIRLOOPHVAC:RETURNPLENUM')) THEN

      ReturnAirPath(PathNum)%ComponentType(CompNum) = AlphArray(Counter)
      ReturnAirPath(PathNum)%ComponentName(CompNum) = AlphArray(Counter+1)
      ReturnAirPath(PathNum)%ComponentIndex(CompNum)=0
      CALL ValidateComponent(ReturnAirPath(PathNum)%ComponentType(CompNum),  &
                             ReturnAirPath(PathNum)%ComponentName(CompNum),  &
                             IsNotOK,TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
        CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = '//TRIM(ReturnAirPath(PathNum)%Name))
        ErrorsFound=.true.
      ENDIF
      IF (AlphArray(Counter) == 'AIRLOOPHVAC:ZONEMIXER') ReturnAirPath(PathNum)%ComponentType_Num(CompNum)=ZoneMixer_Type
      IF (AlphArray(Counter) == 'AIRLOOPHVAC:RETURNPLENUM') ReturnAirPath(PathNum)%ComponentType_Num(CompNum)=ZoneReturnPlenum_Type
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(1))//'="'//TRIM(ReturnAirPath(PathNum)%Name)//'"')
      CALL ShowContinueError('Unhandled component type ="'//AlphArray(Counter)//'".')
      CALL ShowContinueError('Must be "AirLoopHVAC:ZoneMixer" or "AirLoopHVAC:ReturnPlenum"')
      ErrorsFound = .TRUE.
    ENDIF

    Counter=Counter+2

  END DO

END DO ! end loop over return air paths

DEALLOCATE(AlphArray)
DEALLOCATE(NumArray)
DEALLOCATE(cAlphaFields)
DEALLOCATE(cNumericFields)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(lNumericBlanks)

!setup zone equipment info for convection correlations
CALL SetupZoneEquipmentForConvectionFlowRegime

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in getting Zone Equipment input.')
ENDIF

RETURN

END SUBROUTINE GetZoneEquipmentData1

SUBROUTINE SetupZoneEquipmentForConvectionFlowRegime

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Decide a few one-time things for later
          ! determination of flow regime for convection

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  INTEGER :: ZoneLoop

  DO ZoneLoop=1, NumOfZones


  ENDDO

  RETURN

END SUBROUTINE SetupZoneEquipmentForConvectionFlowRegime

FUNCTION CheckZoneEquipmentList(ComponentType,ComponentName) RESULT(IsOnList)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Provides a way to check if a component name is listed on a zone equipment list.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ComponentType  ! Type of component
  CHARACTER(len=*), INTENT(IN) :: ComponentName  ! Name of component
  LOGICAL                      :: IsOnList       ! True if item is on a list, false if not.

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
   INTEGER :: Loop
   INTEGER :: ListLoop

  IsOnList=.false.
  EquipList: DO Loop=1,NumOfZones  ! NumofZoneEquipLists
    IF (ZoneEquipList(Loop)%Name == ' ') CYCLE    ! dimensioned by NumOfZones.  Only valid ones have names.
    DO ListLoop=1,ZoneEquipList(Loop)%NumOfEquipTypes
      IF (.NOT. SameString(ZoneEquipList(Loop)%EquipType(ListLoop),ComponentType)) CYCLE
      IF (ComponentName == '*') THEN
        IsOnList=.true.
        EXIT EquipList
      ENDIF
      IF (.NOT. SameString(ZoneEquipList(Loop)%EquipName(ListLoop), ComponentName)) CYCLE
      IsOnList=.true.
      EXIT EquipList
    ENDDO
  ENDDO EquipList

  RETURN

END FUNCTION CheckZoneEquipmentList

FUNCTION GetControlledZoneIndex(ZoneName) RESULT (ControlledZoneIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the index into the Controlled Zone Equipment structure
          ! of the indicated zone.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneName ! Zone name to match into Controlled Zone structure
  INTEGER                      :: ControlledZoneIndex ! Index into Controlled Zone structure

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (.not. ZoneEquipInputsFilled) THEN
    CALL GetZoneEquipmentData1
    ZoneEquipInputsFilled=.true.
  END IF

  ControlledZoneIndex=FindItemInList(ZoneName,ZoneEquipConfig%ZoneName,NumOfZones)

  RETURN

END FUNCTION GetControlledZoneIndex

FUNCTION FindControlledZoneIndexFromSystemNodeNumberForZone(TrialZoneNodeNum) RESULT (ControlledZoneIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the zone number for the indicated
          ! zone node num.  Returns 0 if did not find zone node in any Zone

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: TrialZoneNodeNum ! Node number to match into Controlled Zone structure
  INTEGER             :: ControlledZoneIndex ! Index into Controlled Zone structure

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: FoundIt
  INTEGER :: ZoneNum

  FoundIt = .FALSE.

  IF (.not. ZoneEquipInputsFilled) THEN
    CALL GetZoneEquipmentData1
    ZoneEquipInputsFilled=.true.
  END IF
  ControlledZoneIndex = 0
  DO ZoneNum = 1 , NumOfZones
    IF (ZoneEquipConfig(ZoneNum)%ActualZoneNum > 0)  THEN
      IF ( TrialZoneNodeNum == ZoneEquipConfig(ZoneNum)%ZoneNode) THEN
        ! found it.
        FoundIt = .TRUE.
        ControlledZoneIndex = ZoneEquipConfig(ZoneNum)%ActualZoneNum
      ENDIF
    ENDIF
  ENDDO

  RETURN

END FUNCTION FindControlledZoneIndexFromSystemNodeNumberForZone

FUNCTION GetSystemNodeNumberForZone(ZoneName) RESULT (SystemZoneNodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the system node number for the indicated
          ! zone.  Returns 0 if the Zone is not a controlled zone.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneName ! Zone name to match into Controlled Zone structure
  INTEGER                      :: SystemZoneNodeNumber ! System node number for controlled zone

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ControlledZoneIndex

  IF (.not. ZoneEquipInputsFilled) THEN
    CALL GetZoneEquipmentData1
    ZoneEquipInputsFilled=.true.
  END IF

  ControlledZoneIndex=FindItemInList(ZoneName,ZoneEquipConfig%ZoneName,NumOfZones)
  SystemZoneNodeNumber = 0  ! default is not found
  IF (ControlledZoneIndex > 0) THEN
    IF (ZoneEquipConfig(ControlledZoneIndex)%ActualZoneNum > 0) THEN
      SystemZoneNodeNumber = ZoneEquipConfig(ControlledZoneIndex)%ZoneNode
    ENDIF
  ENDIF

  RETURN

END FUNCTION GetSystemNodeNumberForZone

FUNCTION GetReturnAirNodeForZone(ZoneName) RESULT (ReturnAirNodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the return air node number for the indicated
          ! zone.  Returns 0 if the Zone is not a controlled zone.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneName ! Zone name to match into Controlled Zone structure
  INTEGER                      :: ReturnAirNodeNumber ! Return Air node number for controlled zone

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ControlledZoneIndex

  IF (.not. ZoneEquipInputsFilled) THEN
    CALL GetZoneEquipmentData1
    ZoneEquipInputsFilled=.true.
  END IF

  ControlledZoneIndex=FindItemInList(ZoneName,ZoneEquipConfig%ZoneName,NumOfZones)
  ReturnAirNodeNumber = 0   ! default is not found
  IF (ControlledZoneIndex > 0) THEN
    IF (ZoneEquipConfig(ControlledZoneIndex)%ActualZoneNum > 0) THEN
      ReturnAirNodeNumber = ZoneEquipConfig(ControlledZoneIndex)%ReturnAirNode
    ENDIF
  ENDIF

  RETURN

END FUNCTION GetReturnAirNodeForZone

FUNCTION CalcDesignSpecificationOutdoorAir(DSOAPtr, ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, PerPersonNotSet, &
                                           MaxOAVolFlowFlag) RESULT (OAVolumeFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   October 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns the air volume flow rate based on DesignSpecification:OutdoorAir object.

          ! METHODOLOGY EMPLOYED:
          ! User inputs and zone index allows calculation of outdoor air quantity.
          ! Sizing does not use occupancy or min OA schedule and will call with flags set to FALSE
          ! Ventilation Rate Procedure uses occupancy schedule based on user input.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing, ONLY: OARequirements   ! to access DesignSpecification:OutdoorAir inputs
  USE DataSizing, ONLY: OAFlowNone, OAFlowPPer, OAFlow, OAFlowPerArea, OAFlowACH, OAFlowSum, OAFlowMax
  USE ScheduleManager, ONLY: GetCurrentScheduleValue, GetScheduleMaxValue
  USE DataHeatBalance, ONLY: Zone, ZoneIntGain, People, TotPeople

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DSOAPtr       ! Pointer to DesignSpecification:OutdoorAir object
  INTEGER, INTENT(IN) :: ActualZoneNum ! Zone index
  LOGICAL, INTENT(IN) :: UseOccSchFlag ! Zone occupancy schedule will be used instead of using total zone occupancy
  LOGICAL, INTENT(IN) :: UseMinOASchFlag  ! Use min OA schedule in DesignSpecification:OutdoorAir object
  LOGICAL, INTENT(IN), OPTIONAL :: PerPersonNotSet ! when calculation should not include occupants (e.g., dual duct)
  LOGICAL, INTENT(IN), OPTIONAL :: MaxOAVolFlowFlag ! TRUE when calculation uses occupancy schedule  (e.g., dual duct)
  REAL(r64)           :: OAVolumeFlowRate ! Return value for calculated outdoor air volume flow rate [m3/s]

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DSOAFlowPeople  ! Outdoor air volume flow rate based on occupancy (m3/s)
  REAL(r64) :: DSOAFlowPerZone ! Outdoor air volume flow rate (m3/s)
  REAL(r64) :: DSOAFLowPerArea ! Outdoor air volume flow rate based on zone floor area (m3/s)
  REAL(r64) :: DSOAFlowACH     ! Outdoor air volume flow rate based on air changes per hour (m3/s)
  REAL(r64) :: PeopleCount     ! total count of people in people objects
  INTEGER   :: Loop            ! index counter in LOOP
  LOGICAL   :: PerPersonModeNotSet
  LOGICAL   :: MaxOAFlag

  OAVolumeFlowRate = 0.d0
  IF(DSOAPtr .EQ. 0)RETURN

  IF(PRESENT(PerPersonNotSet))THEN
    PerPersonModeNotSet = PerPersonNotSet
  ELSE
    PerPersonModeNotSet = .FALSE.
  END IF

  IF(PRESENT(MaxOAVolFlowFlag))THEN
    MaxOAFlag = MaxOAVolFlowFlag
  ELSE
    MaxOAFlag = .FALSE.
  END IF

  ! Calculate people outdoor air flow rate as needed
  SELECT CASE(OARequirements(DSOAPtr)%OAFlowMethod)
    CASE(OAFlowPPer, OAFlowSum, OAFlowMax)
      IF(UseOccSchFlag)THEN
        IF(MaxOAFlag)THEN
          ! OAPerPersonMode == PerPersonDCVByCurrentLevel (UseOccSchFlag = TRUE)
          ! for dual duct, get max people according to max schedule value when requesting MaxOAFlow
          PeopleCount = 0.d0
          DO Loop = 1, TotPeople
            IF (ActualZoneNum /= People(Loop)%ZonePtr) CYCLE
            PeopleCount = PeopleCount + People(Loop)%NumberOfPeople &
                       * GetScheduleMaxValue(People(Loop)%NumberOfPeoplePtr )
          ENDDO
          DSOAFlowPeople = PeopleCount * OARequirements(DSOAPtr)%OAFlowPerPerson
        ELSE
          DSOAFlowPeople = ZoneIntGain(ActualZoneNum)%NOFOCC * OARequirements(DSOAPtr)%OAFlowPerPerson
        END IF
      ELSE
        IF(MaxOAFlag)THEN
          ! OAPerPersonMode == PerPersonByDesignLevel (UseOccSchFlag = FALSE)
          ! use total people when requesting MaxOAFlow
          DSOAFlowPeople = Zone(ActualZoneNum)%TotOccupants * OARequirements(DSOAPtr)%OAFlowPerPerson
        ELSE
          DSOAFlowPeople = Zone(ActualZoneNum)%TotOccupants * OARequirements(DSOAPtr)%OAFlowPerPerson
        END IF
      END IF
      IF(PerPersonModeNotSet)DSOAFlowPeople = 0.d0  ! for Dual Duct if Per Person Ventilation Rate Mode is not entered
    CASE DEFAULT
      DSOAFlowPeople = 0.d0
  END SELECT

  ! Calculate minimum outdoor air flow rate
  SELECT CASE(OARequirements(DSOAPtr)%OAFlowMethod)
    CASE(OAFlowNone)
      ! Special case for no DesignSpecification:OutdoorAir object in Sizing:Zone object
      ! probably won't get to this CASE statement since it will RETURN above (Ptr=0)
      ! See SizingManager GetZoneSizingInput for Sizing:Zone input field Design Specification Outdoor Air Object Name
      OAVolumeFlowRate = 0.d0
    CASE(OAFlowPPer)
      ! Multiplied by occupancy
      OAVolumeFlowRate = DSOAFlowPeople
    CASE(OAFlow)
      ! User input
      OAVolumeFlowRate = OARequirements(DSOAPtr)%OAFlowPerZone
    CASE(OAFlowPerArea)
      ! Multiplied by zone floor area
      OAVolumeFlowRate = OARequirements(DSOAPtr)%OAFlowPerArea * Zone(ActualZoneNum)%FloorArea
    CASE(OAFlowACH)
      ! Multiplied by zone volume
      OAVolumeFlowRate = OARequirements(DSOAPtr)%OAFlowACH * Zone(ActualZoneNum)%Volume / 3600.d0

    CASE(OAFlowSum, OAFlowMax)
      ! Use sum or max of per person and the following
      DSOAFlowPerZone = OARequirements(DSOAPtr)%OAFlowPerZone
      DSOAFLowPerArea = OARequirements(DSOAPtr)%OAFlowPerArea * Zone(ActualZoneNum)%FloorArea
      DSOAFlowACH     = OARequirements(DSOAPtr)%OAFlowACH * Zone(ActualZoneNum)%Volume / 3600.d0
      IF(OARequirements(DSOAPtr)%OAFlowMethod == OAFlowMax)THEN
        OAVolumeFlowRate = MAX(DSOAFlowPeople, DSOAFlowPerZone, DSOAFlowPerArea, DSOAFlowACH)
      ELSE
        OAVolumeFlowRate = DSOAFlowPeople + DSOAFlowPerZone + DSOAFlowPerArea + DSOAFlowACH
      END IF

    CASE DEFAULT
      ! Will never get here
      OAVolumeFlowRate = 0.d0
  END SELECT

  ! Apply zone multipliers and zone list multipliers
  OAVolumeFlowRate = OAVolumeFlowRate * Zone(ActualZoneNum)%Multiplier * Zone(ActualZoneNum)%ListMultiplier

  ! Apply schedule as needed. Sizing does not use schedule.
  IF(OARequirements(DSOAPtr)%OAFlowFracSchPtr .GT. 0 .AND. UseMinOASchFlag)THEN
    IF(MaxOAFlag)THEN
      OAVolumeFlowRate = OAVolumeFlowRate * GetScheduleMaxValue(OARequirements(DSOAPtr)%OAFlowFracSchPtr)
    ELSE
      OAVolumeFlowRate = OAVolumeFlowRate * GetCurrentScheduleValue(OARequirements(DSOAPtr)%OAFlowFracSchPtr)
    END IF
  END IF

  END FUNCTION CalcDesignSpecificationOutdoorAir

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


END MODULE DataZoneEquipment
