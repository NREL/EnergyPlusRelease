MODULE UserDefinedComponents

          ! Module containing the routines dealing with the User Defined HVAC and Plant component models

          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Collect component models for custom program with Erl.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataInterfaces
USE DataGlobals, ONLY: MaxNameLength, emsCallFromUserDefinedComponentModel, BeginEnvrnFlag, NumOfZones
USE DataPlant
USE DataLoopNode
USE DataRuntimeLanguage
USE DataWater,  ONLY: WaterStorage

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:





  TYPE PlantConnectionStruct
    INTEGER        :: ErlInitProgramMngr   = 0 ! points to an EMS:ProgramManager to run for setup and sizing
    INTEGER        :: ErlSimProgramMngr    = 0 ! points to an EMS:ProgramManager to run only when this connection is called
    INTEGER        :: LoopNum              = 0  ! plant loop connection index
    INTEGER        :: LoopSideNum          = 0  ! plant loop side connection index
    INTEGER        :: BranchNum            = 0  ! plant loop branch connection index
    INTEGER        :: CompNum              = 0  ! plant loop component connection index
    INTEGER        :: InletNodeNum         = 0  ! plant loop inlet node index
    INTEGER        :: OutletNodeNum        = 0  ! plant loop outlet node index
    INTEGER        :: FlowPriority         = LoopFlowStatus_Unknown ! how component affects overall loop flow determination
    INTEGER        :: HowLoadServed        = HowMet_Unknown  ! nature of component wrt to plant loop's loads
    REAL(r64)      :: LowOutTempLimit      = 0.d0 ! low limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT
    REAL(r64)      :: HiOutTempLimit       = 0.d0 ! hi limit for outlet temp if MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT
    REAL(r64)      :: MassFlowRateRequest  = 0.d0 ! request filled by actuator, might not be satisfied if plant constrained [kg/s]
    REAL(r64)      :: MassFlowRateMin      = 0.d0 ! filled by actuator, reports minimum (hardware) flow rate for component [kg/s]
    REAL(r64)      :: MassFlowRateMax      = 0.d0 ! filled by actuator, reports maximum (hardware) flow rate for component [kg/s]
    REAL(r64)      :: DesignVolumeFlowRate = 0.d0 ! filled by actuator,
    REAL(r64)      :: MyLoad  =0.d0   ! fills internal variable for user's model to know current load request of supply equip [W]
    REAL(r64)      :: MinLoad = 0.d0  ! filled by actuator, reports back size for load dispatch routines [W]
    REAL(r64)      :: MaxLoad = 0.d0  ! filled by actuator, reports back size for load dispatch [W]
    REAL(r64)      :: OptLoad = 0.d0  ! filled by actuator, reports back size for load dispatch [W]
    REAL(r64)      :: InletRho = 0.d0 ! fills internal variable, current density for fluid type and inlet temperature [kg/m3]
    REAL(r64)      :: InletCp  = 0.d0 ! fills internal Varaible, current specific heat for fluid type and inlet temperature [J/kg-C]
    REAL(r64)      :: InletTemp = 0.d0 ! fills internal variable, current inlet fluid temperature [C]
    REAL(r64)      :: InletMassFlowRate = 0.d0 ! fills internal variable, current inlet mass flow rate [kg/s]
    REAL(r64)      :: OutletTemp = 0.d0 ! filled by actuator, componenent outlet temperature [C]
  END TYPE PlantConnectionStruct

  TYPE AirConnectionStruct
    INTEGER        :: InletNodeNum       = 0     ! air inlet node index
    INTEGER        :: OutletNodeNum      = 0     ! air outlet node index
    REAL(r64)      :: InletRho           = 0.d0  ! fills internal variable, current inlet air density [kg/m3]
    REAL(r64)      :: InletCp            = 0.d0  ! fills internal variable, current inlet air specific heat [J/kg-c]
    REAL(r64)      :: InletTemp          = 0.d0  ! fills internal variable, current inlet air temperature [C]
    REAL(r64)      :: InletHumRat        = 0.d0  ! fills internal variable, current inlet air humidity ratio [kg/kg]
    REAL(r64)      :: InletMassFlowRate  = 0.d0  ! fills internal variable, current inlet air mass flow rate [kg/s]
    REAL(r64)      :: OutletTemp         = 0.d0  ! filled by actuator, component outlet temperature [C]
    REAL(r64)      :: OutletHumRat       = 0.d0  ! filled by actuator, component outlet humidity ratio [kg/kg]
    REAL(r64)      :: OutletMassFlowRate = 0.d0  ! filled by actuator, component outlet mass flow rate [kg/s]
  END TYPE AirConnectionStruct

  TYPE WaterUseTankConnectionStruct ! data for interacting with water use storage system
    LOGICAL    :: SuppliedByWaterSystem           = .FALSE.
    INTEGER    :: SupplyTankID                    = 0 ! index "pointer" to WaterStorage structure
    INTEGER    :: SupplyTankDemandARRID           = 0 ! index "pointer" to demand array inside WaterStorage structure
    REAL(r64)  :: SupplyVdotRequest               = 0.d0
    LOGICAL    :: CollectsToWaterSystem           = .FALSE.
    INTEGER    :: CollectionTankID                = 0 !index "pointer" to Storage TAnk array WaterStorage
    INTEGER    :: CollectionTankSupplyARRID       = 0 !index pointe to supply Vdot array in WaterStorage
    REAL(r64)  :: CollectedVdot                   = 0.d0 !
  END TYPE WaterUseTankConnectionStruct

  TYPE ZoneInternalGainsStruct
    LOGICAL   :: DeviceHasInternalGains      = .FALSE.
    INTEGER   :: ZoneNum                     = 0
    REAL(r64) :: ConvectionGainRate          = 0.d0
    REAL(r64) :: ReturnAirConvectionGainRate = 0.d0
    REAL(r64) :: ThermalRadiationGainRate    = 0.d0
    REAL(r64) :: LatentGainRate              = 0.d0
    REAL(r64) :: ReturnAirLatentGainRate     = 0.d0
    REAL(r64) :: CarbonDioxideGainRate       = 0.d0
    REAL(r64) :: GenericContamGainRate       = 0.d0
  END TYPE ZoneInternalGainsStruct

  TYPE UserPlantComponentStruct
    CHARACTER(len=MaxNameLength) :: Name   =' '    ! user identifier
    INTEGER    :: ErlSimProgramMngr = 0   ! EMS:ProgramManager to always run when this model is called
    INTEGER    :: NumPlantConnections = 0 ! count of how many plant loop connections there are
    TYPE(PlantConnectionStruct), DIMENSION(:), ALLOCATABLE :: Loop ! collect data for each plant loop connection
    TYPE(AirConnectionStruct)            :: Air
    TYPE(WaterUseTankConnectionStruct)   :: Water
    TYPE(ZoneInternalGainsStruct)        :: Zone

  END TYPE UserPlantComponentStruct

  TYPE UserCoilComponentStruct
    CHARACTER(len=MaxNameLength) :: Name   =' '    ! user identifier
    INTEGER    :: ErlSimProgramMngr = 0   ! EMS:ProgramManager to always run when this model is called
    INTEGER    :: ErlInitProgramMngr= 0   ! EMS:ProgramManager to  run when this model is initialized and setup
    INTEGER    :: NumAirConnections = 0   ! count of how many air connectiosn there are
    LOGICAL    :: PlantIsConnected = .FALSE.
    TYPE(AirConnectionStruct), DIMENSION(:), ALLOCATABLE :: Air
    TYPE(PlantConnectionStruct)          :: Loop
    TYPE(WaterUseTankConnectionStruct)   :: Water
    TYPE(ZoneInternalGainsStruct)        :: Zone
  END TYPE UserCoilComponentStruct

  TYPE UserZoneHVACForcedAirComponentStruct
    CHARACTER(len=MaxNameLength) :: Name   =' '    ! user identifier
    INTEGER    :: ErlSimProgramMngr = 0   ! EMS:ProgramManager to always run when this model is called
    INTEGER    :: ErlInitProgramMngr= 0   ! EMS:ProgramManager to  run when this model is initialized and setup
    TYPE(AirConnectionStruct)            :: ZoneAir
    TYPE(AirConnectionStruct)            :: SourceAir
    INTEGER    :: NumPlantConnections = 0 ! count of how many plant loop (demand) connections there are
    TYPE(PlantConnectionStruct), DIMENSION(:), ALLOCATABLE :: Loop ! collect data for each plant loop connection
    TYPE(WaterUseTankConnectionStruct)   :: Water
    TYPE(ZoneInternalGainsStruct)        :: Zone ! for skin losses
    REAL(r64)  :: RemainingOutputToHeatingSP    = 0.d0 ! sensible load remaining for device, to heating setpoint [W]
    REAL(r64)  :: RemainingOutputToCoolingSP    = 0.d0 ! sensible load remaining for device, negative means cooling [W]
    REAL(r64)  :: RemainingOutputReqToHumidSP   = 0.d0 ! latent load remaining for device, to humidification setpoint [kg/s]
    REAL(r64)  :: RemainingOutputReqToDehumidSP = 0.d0 ! latent load remaining for device, Negative means dehumidify [kg/s]
  END Type

  TYPE UserAirTerminalComponentStruct
    CHARACTER(len=MaxNameLength) :: Name   =' '    ! user identifier
    INTEGER    :: ActualCtrlZoneNum = 0
    INTEGER    :: ErlSimProgramMngr = 0   ! EMS:ProgramManager to always run when this model is called
    INTEGER    :: ErlInitProgramMngr= 0   ! EMS:ProgramManager to  run when this model is initialized and setup
    TYPE(AirConnectionStruct)     :: AirLoop
    TYPE(AirConnectionStruct)     :: SourceAir
    INTEGER    :: NumPlantConnections = 0 ! count of how many plant loop (demand) connections there are
    TYPE(PlantConnectionStruct), DIMENSION(:), ALLOCATABLE :: Loop ! collect data for each plant loop connection
    TYPE(WaterUseTankConnectionStruct)   :: Water
    TYPE(ZoneInternalGainsStruct)        :: Zone ! for skin losses
    REAL(r64)  :: RemainingOutputToHeatingSP    = 0.d0 ! sensible load remaining for device, to heating setpoint [W]
    REAL(r64)  :: RemainingOutputToCoolingSP    = 0.d0 ! sensible load remaining for device, negative means cooling [W]
    REAL(r64)  :: RemainingOutputReqToHumidSP   = 0.d0 ! latent load remaining for device, to humidification setpoint [kg/s]
    REAL(r64)  :: RemainingOutputReqToDehumidSP = 0.d0 ! latent load remaining for device, Negative means dehumidify [kg/s]
  END TYPE
          ! MODULE VARIABLE DECLARATIONS:
  TYPE(UserPlantComponentStruct),             DIMENSION(:), ALLOCATABLE :: UserPlantComp
  TYPE(UserCoilComponentStruct),              DIMENSION(:), ALLOCATABLE :: UserCoil
  TYPE(UserZoneHVACForcedAirComponentStruct), DIMENSION(:), ALLOCATABLE :: UserZoneAirHVAC
  TYPE(UserAirTerminalComponentStruct),       DIMENSION(:), ALLOCATABLE :: UserAirTerminal

  INTEGER   :: NumUserPlantComps = 0
  INTEGER   :: NumUserCoils = 0
  INTEGER   :: NumUserZoneAir = 0
  INTEGER   :: NumUserAirTerminals = 0

  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckUserPlantCompName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckUserCoilName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckUserZoneAirName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckUserAirTerminal
  LOGICAL :: GetInput = .TRUE.


          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:
PRIVATE GetUserDefinedComponents

PUBLIC SimUserDefinedPlantComponent
PRIVATE InitPlantUserComponent
PRIVATE ReportPlantUserComponent

PUBLIC SimCoilUserDefined
PRIVATE InitCoilUserDefined
PRIVATE ReportCoilUserDefined

PUBLIC SimZoneAirUserDefined
PRIVATE InitZoneAirUserDefined
PRIVATE ReportZoneAirUserDefined

PUBLIC SimAirTerminalUserDefined
PRIVATE InitAirTerminalUserDefined
PRIVATE ReportAirTerminalUserDefined

CONTAINS


SUBROUTINE SimUserDefinedPlantComponent(LoopNum, LoopSideNum, EquipType,EquipName, &
                                         CompIndex,InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! User Defined plant generic component

          ! METHODOLOGY EMPLOYED:
          ! This routine to be called from PlantLoopEquipment.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE EMSManager,     ONLY: ManageEMS
  USE PlantUtilities, ONLY: InitComponentNodes, RegisterPlantCompDesignFlow

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: LoopNum  ! plant loop sim call originated from
  INTEGER, INTENT(IN)          :: LoopSideNum  ! plant loop side sim call originated from
  CHARACTER(len=*), INTENT(IN) :: EquipType  ! type of equipment, 'PlantComponent:UserDefined'
  CHARACTER(len=*), INTENT(IN) :: EquipName  ! user name for component
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(IN)        :: MyLoad
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER       :: CompNum
  INTEGER       :: ThisLoop
  INTEGER       :: Loop

  IF (GetInput) THEN
    CALL GetUserDefinedComponents
    GetInput=.FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    CompNum = FindItemInList(EquipName, UserPlantComp%Name, NumUserPlantComps)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: User Defined Plant Component not found')
    ENDIF
    CompIndex = CompNum
  ELSE
    CompNum = CompIndex
    IF (CompNum < 1 .OR. CompNum > NumUserPlantComps) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Number of units ='//TRIM(TrimSigDigits(NumUserPlantComps))// &
                           ', Entered Unit name = '//TRIM(EquipName) )
    ENDIF
    IF(CheckUserPlantCompName(CompNum)) THEN
      IF (EquipName /= UserPlantComp(CompNum)%Name) THEN
        CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Unit name='//TRIM(EquipName)//', stored unit name for that index='// &
                           TRIM(UserPlantComp(CompNum)%Name) )
      ENDIF
      CheckUserPlantCompName(CompNum) = .FALSE.
    ENDIF
  ENDIF

  IF (InitLoopEquip .OR. BeginEnvrnFlag) THEN
    CALL InitPlantUserComponent(CompNum, LoopNum, MyLoad)
    ! find loop connection number from LoopNum and LoopSide
    ThisLoop = 0
    DO Loop = 1, UserPlantComp(CompNum)%NumPlantConnections
      IF (LoopNum  /= UserPlantComp(CompNum)%Loop(Loop)%LoopNum) CYCLE
      IF (LoopSideNum /= UserPlantComp(CompNum)%Loop(Loop)%LoopSideNum) CYCLE
      ThisLoop = loop
    ENDDO
    IF (ThisLoop > 0) THEN
      IF (UserPlantComp(CompNum)%Loop(ThisLoop)%ErlInitProgramMngr > 0) THEN
        CALL ManageEMS(emsCallFromUserDefinedComponentModel,  &
                       ProgramManagerToRun = UserPlantComp(CompNum)%Loop(ThisLoop)%ErlInitProgramMngr )
      ENDIF
      ! now interface sizing related values with rest of E+
      MinCap = UserPlantComp(CompNum)%Loop(ThisLoop)%MinLoad
      MaxCap = UserPlantComp(CompNum)%Loop(ThisLoop)%MaxLoad
      OptCap = UserPlantComp(CompNum)%Loop(ThisLoop)%OptLoad

      CALL InitComponentNodes(UserPlantComp(CompNum)%Loop(ThisLoop)%MassFlowRateMin, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%MassFlowRateMax, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%InletNodeNum, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%OutletNodeNum, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%LoopNum, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%LoopSideNum, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%BranchNum, &
                        UserPlantComp(CompNum)%Loop(ThisLoop)%CompNum )

      CALL RegisterPlantCompDesignFlow(UserPlantComp(CompNum)%Loop(ThisLoop)%InletNodeNum, &
                                       UserPlantComp(CompNum)%Loop(ThisLoop)%DesignVolumeFlowRate)

    ELSE
      ! throw warning
      CALL ShowFatalError('SimUserDefinedPlantComponent: did not find where called from' // &
                           ' loop number called from =' //TRIM(TrimSigDigits(LoopNum)) // &
                           ' , loop side called from =' //TRIM(TrimSigDigits(LoopSideNum)) )
    ENDIF
    RETURN
  ENDIF

  ThisLoop = 0
  DO Loop = 1, UserPlantComp(CompNum)%NumPlantConnections
    IF (LoopNum  /= UserPlantComp(CompNum)%Loop(Loop)%LoopNum) CYCLE
    IF (LoopSideNum /= UserPlantComp(CompNum)%Loop(Loop)%LoopSideNum) CYCLE
    ThisLoop = loop
  ENDDO

  CALL InitPlantUserComponent(CompNum, ThisLoop, MyLoad)

  IF (ThisLoop > 0) THEN
    IF (UserPlantComp(CompNum)%Loop(ThisLoop)%ErlSimProgramMngr > 0) THEN
      CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserPlantComp(CompNum)%Loop(ThisLoop)%ErlSimProgramMngr)
    ENDIF
  ENDIF

  IF (UserPlantComp(CompNum)%ErlSimProgramMngr > 0) THEN
    CALL ManageEMS(emsCallFromUserDefinedComponentModel,  &
                 ProgramManagerToRun = UserPlantComp(CompNum)%ErlSimProgramMngr )
  ENDIF

  CALL ReportPlantUserComponent(CompNum, ThisLoop)


  RETURN

END SUBROUTINE SimUserDefinedPlantComponent

SUBROUTINE SimCoilUserDefined(EquipName, CompIndex, AirLoopNum, HeatingActive, CoolingActive)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE EMSManager,     ONLY: ManageEMS
  USE PlantUtilities, ONLY: SetComponentFlowRate, InitComponentNodes, RegisterPlantCompDesignFlow
  USE Psychrometrics, ONLY: PsyHFnTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: EquipName  ! user name for component
  INTEGER, INTENT(INOUT)       :: CompIndex
  INTEGER, INTENT(IN)          :: AirLoopNum
  LOGICAL, INTENT(INOUT)       :: HeatingActive
  LOGICAL, INTENT(INOUT)       :: CoolingActive

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: EnthInlet
  REAL(r64)  :: EnthOutlet
  INTEGER    :: CompNum

  IF (GetInput) THEN
    CALL GetUserDefinedComponents
    GetInput=.FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    CompNum = FindItemInList(EquipName, UserCoil%Name, NumUserCoils)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: User Defined Coil not found')
    ENDIF
    CompIndex = CompNum
  ELSE
    CompNum = CompIndex
    IF (CompNum < 1 .OR. CompNum > NumUserCoils) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Number of units ='//TRIM(TrimSigDigits(NumUserCoils))// &
                           ', Entered Unit name = '//TRIM(EquipName) )
    ENDIF
    IF(CheckUserCoilName(CompNum)) THEN
      IF (EquipName /= UserCoil(CompNum)%Name) THEN
        CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Unit name='//TRIM(EquipName)//', stored unit name for that index='// &
                           TRIM(UserCoil(CompNum)%Name) )
      ENDIF
      CheckUserCoilName(CompNum) = .FALSE.
    ENDIF
  ENDIF

  IF (BeginEnvrnFlag) THEN
    IF (UserCoil(CompNum)%ErlInitProgramMngr > 0) THEN
      CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserCoil(CompNum)%ErlInitProgramMngr )
    ENDIF

    IF (UserCoil(CompNum)%PlantIsConnected) THEN

      CALL InitComponentNodes(UserCoil(CompNum)%Loop%MassFlowRateMin, &
                              UserCoil(CompNum)%Loop%MassFlowRateMax, &
                              UserCoil(CompNum)%Loop%InletNodeNum, &
                              UserCoil(CompNum)%Loop%OutletNodeNum, &
                              UserCoil(CompNum)%Loop%LoopNum, &
                              UserCoil(CompNum)%Loop%LoopSideNum, &
                              UserCoil(CompNum)%Loop%BranchNum, &
                              UserCoil(CompNum)%Loop%CompNum )

      CALL RegisterPlantCompDesignFlow(UserCoil(CompNum)%Loop%InletNodeNum, &
                                       UserCoil(CompNum)%Loop%DesignVolumeFlowRate)

    ENDIF
  ENDIF

  CALL InitCoilUserDefined(CompNum)

  IF (UserCoil(CompNum)%ErlSimProgramMngr > 0) THEN
    CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserCoil(CompNum)%ErlSimProgramMngr)
  ENDIF

  CALL ReportCoilUserDefined(CompNum)

  IF (AirLoopNum /=-1) THEN ! IF the sysem is not an equipment of outdoor air unit
    ! determine if heating or cooling on primary air stream
    IF (Node(UserCoil(CompNum)%Air(1)%InletNodeNum)%Temp < Node(UserCoil(CompNum)%Air(1)%InletNodeNum)%Temp ) THEN
      HeatingActive = .TRUE.
    ELSE
      HeatingActive = .FALSE.
    ENDIF

    EnthInlet = PSyHFnTdbW(Node(UserCoil(CompNum)%Air(1)%InletNodeNum)%Temp, Node(UserCoil(CompNum)%Air(1)%InletNodeNum)%HumRat)
    EnthOutlet = PSyHFnTdbW(Node(UserCoil(CompNum)%Air(1)%OutletNodeNum)%Temp, Node(UserCoil(CompNum)%Air(1)%OutletNodeNum)%HumRat)
    IF (EnthInlet > EnthOutlet) THEN
      CoolingActive = .TRUE.
    ELSE
      CoolingActive = .FALSE.
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE SimCoilUserDefined

SUBROUTINE SimZoneAirUserDefined(CompName,ZoneNum,SensibleOutputProvided,LatentOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   February, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE EMSManager,     ONLY: ManageEMS
  USE PlantUtilities, ONLY: SetComponentFlowRate, InitComponentNodes, RegisterPlantCompDesignFlow
  USE Psychrometrics, ONLY: PsyHFnTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT    (IN) :: CompName            ! name of the packaged terminal heat pump
  INTEGER,          INTENT    (IN) :: ZoneNum             ! number of zone being served
  REAL(r64),        INTENT   (OUT) :: SensibleOutputProvided   ! sensible capacity delivered to zone
  REAL(r64),        INTENT   (OUT) :: LatentOutputProvided   ! Latent add/removal  (kg/s), dehumid = negative
  INTEGER,          INTENT (INOUT) :: CompIndex           ! index to zone hvac unit
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: CompNum
  INTEGER    :: Loop
  REAL(r64)  :: AirMassFlow
  REAL(r64)  :: MinHumRat
  REAL(r64)  :: SpecHumOut
  REAL(r64)  :: SpecHumIn

  IF (GetInput) THEN
    CALL GetUserDefinedComponents
    GetInput=.FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    CompNum = FindItemInList(CompName, UserZoneAirHVAC%Name, NumUserZoneAir)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: User Defined Coil not found')
    ENDIF
    CompIndex = CompNum
  ELSE
    CompNum = CompIndex
    IF (CompNum < 1 .OR. CompNum > NumUserZoneAir) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Number of units ='//TRIM(TrimSigDigits(NumUserZoneAir))// &
                           ', Entered Unit name = '//TRIM(CompName) )
    ENDIF
    IF(CheckUserZoneAirName(CompNum)) THEN
      IF (CompName /= UserZoneAirHVAC(CompNum)%Name) THEN
        CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Unit name='//TRIM(CompName)//', stored unit name for that index='// &
                           TRIM(UserZoneAirHVAC(CompNum)%Name) )
      ENDIF
      CheckUserZoneAirName(CompNum) = .FALSE.
    ENDIF
  ENDIF

  IF (BeginEnvrnFlag) THEN
    CALL InitZoneAirUserDefined(CompNum, ZoneNum)

    IF (UserZoneAirHVAC(CompNum)%ErlInitProgramMngr > 0) THEN
      CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserZoneAirHVAC(CompNum)%ErlInitProgramMngr )
    ENDIF
    IF (UserZoneAirHVAC(CompNum)%NumPlantConnections > 0) THEN
      DO Loop = 1, UserZoneAirHVAC(CompNum)%NumPlantConnections

        CALL InitComponentNodes(UserZoneAirHVAC(CompNum)%Loop(Loop)%MassFlowRateMin, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%MassFlowRateMax, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%OutletNodeNum, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopNum, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopSideNum, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%BranchNum, &
                          UserZoneAirHVAC(CompNum)%Loop(Loop)%CompNum )

        CALL RegisterPlantCompDesignFlow(UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum, &
                                         UserZoneAirHVAC(CompNum)%Loop(Loop)%DesignVolumeFlowRate)
      ENDDO
    ENDIF

  ENDIF ! BeginEnvrnFlag

  CALL InitZoneAirUserDefined(CompNum, ZoneNum)

  IF (UserZoneAirHVAC(CompNum)%ErlSimProgramMngr > 0) THEN
    CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserZoneAirHVAC(CompNum)%ErlSimProgramMngr)
  ENDIF

  CALL ReportZoneAirUserDefined(CompNum)

   ! calculate delivered capacity
  AirMassFlow = MIN (Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%MassFlowRate, &
                     Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%MassFlowRate)
  ! calculate sensible load met using delta enthalpy at a constant (minimum) humidity ratio)
  MinHumRat = MIN(Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%HumRat, &
                  Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%HumRat)
  SensibleOutputProvided   = AirMassFlow * &
              (  PsyHFnTdbW(Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%Temp,MinHumRat, 'SimZoneAirUserDefined') &
                - PsyHFnTdbW(Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%Temp,MinHumRat, 'SimZoneAirUserDefined'))

! CR9155 Remove specific humidity calculations
  SpecHumOut = Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%HumRat
  SpecHumIn  = Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%HumRat
  LatentOutputProvided = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate, kg/s (dehumid = negative)


  RETURN

END SUBROUTINE SimZoneAirUserDefined

SUBROUTINE SimAirTerminalUserDefined(CompName,FirstHVACIteration, ZoneNum, ZoneNodeNum, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulation call for generic air terminal

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE EMSManager,     ONLY: ManageEMS
  USE PlantUtilities, ONLY: SetComponentFlowRate, InitComponentNodes, RegisterPlantCompDesignFlow

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  LOGICAL,      INTENT (IN):: FirstHVACIteration
  INTEGER,      INTENT (IN):: ZoneNum
  INTEGER,      INTENT (IN):: ZoneNodeNum
  INTEGER,      INTENT (INOUT):: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: CompNum
  INTEGER    :: Loop

  IF (GetInput) THEN
    CALL GetUserDefinedComponents
    GetInput=.FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    CompNum = FindItemInList(CompName, UserAirTerminal%Name, NumUserAirTerminals)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: User Defined Coil not found')
    ENDIF
    CompIndex = CompNum
  ELSE
    CompNum = CompIndex
    IF (CompNum < 1 .OR. CompNum > NumUserAirTerminals) THEN
      CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Number of units ='//TRIM(TrimSigDigits(NumUserAirTerminals))// &
                           ', Entered Unit name = '//TRIM(CompName) )
    ENDIF
    IF(CheckUserAirTerminal(CompNum)) THEN
      IF (CompName /= UserAirTerminal(CompNum)%Name) THEN
        CALL ShowFatalError('SimUserDefinedPlantComponent: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Unit name='//TRIM(CompName)//', stored unit name for that index='// &
                           TRIM(UserAirTerminal(CompNum)%Name) )
      ENDIF
      CheckUserAirTerminal(CompNum) = .FALSE.
    ENDIF
  ENDIF

  IF (BeginEnvrnFlag) THEN
    CALL InitAirTerminalUserDefined(CompNum, ZoneNum)

    IF (UserAirTerminal(CompNum)%ErlInitProgramMngr > 0) THEN
      CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserAirTerminal(CompNum)%ErlInitProgramMngr )
    ENDIF
    IF (UserAirTerminal(CompNum)%NumPlantConnections > 0) THEN
      DO Loop = 1, UserAirTerminal(CompNum)%NumPlantConnections

        CALL InitComponentNodes(UserAirTerminal(CompNum)%Loop(Loop)%MassFlowRateMin, &
                          UserAirTerminal(CompNum)%Loop(Loop)%MassFlowRateMax, &
                          UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum, &
                          UserAirTerminal(CompNum)%Loop(Loop)%OutletNodeNum, &
                          UserAirTerminal(CompNum)%Loop(Loop)%LoopNum, &
                          UserAirTerminal(CompNum)%Loop(Loop)%LoopSideNum, &
                          UserAirTerminal(CompNum)%Loop(Loop)%BranchNum, &
                          UserAirTerminal(CompNum)%Loop(Loop)%CompNum )

        CALL RegisterPlantCompDesignFlow(UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum, &
                                         UserAirTerminal(CompNum)%Loop(Loop)%DesignVolumeFlowRate)
      ENDDO


    ENDIF

  ENDIF ! BeginEnvrnFlag

  CALL InitAirTerminalUserDefined(CompNum, ZoneNum)

  IF (UserAirTerminal(CompNum)%ErlSimProgramMngr > 0) THEN
    CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                   ProgramManagerToRun = UserAirTerminal(CompNum)%ErlSimProgramMngr)
  ENDIF

  CALL ReportAirTerminalUserDefined(CompNum)

  RETURN

END SUBROUTINE SimAirTerminalUserDefined


SUBROUTINE GetUserDefinedComponents

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectDefMaxArgs, GetObjectItem, &
                                   FindItemInList, VerifyName
  USE General,               ONLY: RoundSigDigits
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataHeatBalance,       ONLY: Zone, IntGainTypeOf_PlantComponentUserDefined, &
                                   IntGainTypeOf_CoilUserDefined, IntGainTypeOf_ZoneHVACForcedAirUserDefined, &
                                   IntGainTypeOf_AirTerminalUserDefined
  USE WaterManager,          ONLY: SetupTankDemandComponent, SetupTankSupplyComponent
  USE DataZoneEquipment,     ONLY: ZoneEquipConfig
  USE GlobalNames,           ONLY: VerifyUniqueCoilName

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
  LOGICAL    :: ErrorsFound = .FALSE.
  INTEGER    :: NumAlphas ! Number of elements in the alpha array
  INTEGER    :: NumNums   ! Number of elements in the numeric array
  INTEGER    :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL    :: IsNotOK   ! Flag to verify name
  LOGICAL    :: IsBlank   ! Flag for blank name
  INTEGER    :: MaxNumAlphas = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER    :: MaxNumNumbers = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER    :: TotalArgs = 0 !argument for call to GetObjectDefMaxArgs
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  INTEGER  :: CompLoop
  INTEGER  :: ConnectionLoop
  INTEGER  :: NumPlantConnections
  INTEGER  :: NumAirConnections
  CHARACTER(len=20) :: LoopStr
  INTEGER  :: aArgCount
  INTEGER  :: StackMngrNum
  LOGICAL  :: lDummy
!  INTEGER  :: alphaNum
!  INTEGER  :: Loop
  INTEGER  :: MgrCountTest
  INTEGER  :: CtrlZone   ! controlled zone do loop index
  INTEGER  :: SupAirIn   ! controlled zone supply air inlet index
  LOGICAL  :: errflag

  cCurrentModuleObject = 'PlantComponent:UserDefined'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=NumNums
  MaxNumAlphas=NumAlphas


  ALLOCATE(cAlphaFieldNames(MaxNumAlphas))
  cAlphaFieldNames=' '
  ALLOCATE(cAlphaArgs(MaxNumAlphas))
  cAlphaArgs=' '
  ALLOCATE(lAlphaFieldBlanks(MaxNumAlphas))
  lAlphaFieldBlanks=.false.
  ALLOCATE(cNumericFieldNames(MaxNumNumbers))
  cNumericFieldNames=' '
  ALLOCATE(rNumericArgs(MaxNumNumbers))
  rNumericArgs=0.0d0
  ALLOCATE(lNumericFieldBlanks(MaxNumNumbers))
  lNumericFieldBlanks=.false.


  !need to make sure GetEMSInput has run...

  cCurrentModuleObject = 'PlantComponent:UserDefined'
  NumUserPlantComps = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumUserPlantComps > 0) THEN
    ALLOCATE(UserPlantComp(NumUserPlantComps))
    ALLOCATE(CheckUserPlantCompName(NumUserPlantComps))
    CheckUserPlantCompName = .TRUE.
    DO CompLoop =1, NumUserPlantComps
      CALL GetObjectItem(cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), UserPlantComp%Name, CompLoop - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      UserPlantComp(CompLoop)%Name =  cAlphaArgs(1)

      ! now get program manager for model simulations
      IF (.NOT. lAlphaFieldBlanks(2)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(2), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserPlantComp(CompLoop)%ErlSimProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      NumPlantConnections = FLOOR(rNumericArgs(1))

      IF ((NumPlantConnections >= 1) .AND. (NumPlantConnections <= 4)) THEN
        ALLOCATE(UserPlantComp(CompLoop)%Loop(NumPlantConnections))
        UserPlantComp(CompLoop)%NumPlantConnections = NumPlantConnections
        DO ConnectionLoop = 1, NumPlantConnections
          LoopStr=RoundSigDigits(ConnectionLoop)
          aArgCount = (ConnectionLoop-1) *  6 + 3
          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(aArgCount),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, ConnectionLoop, ObjectIsNotParent)
          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(aArgCount + 1),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, ConnectionLoop, ObjectIsNotParent)

          CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(aArgCount),cAlphaArgs(aArgCount + 1),  &
             'Plant Nodes '//LoopStr)

          SELECT CASE (TRIM(cAlphaArgs(aArgCount + 2)))
          CASE ('DEMANDSLOAD')
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_NoneDemand
          CASE ('MEETSLOADWITHPASSIVECAPACITY')
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_PassiveCap
          CASE ('MEETSLOADWITHNOMINALCAPACITY')
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_ByNominalCap
          CASE ('MEETSLOADWITHNOMINALCAPACITYLOWOUTLIMIT')
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_ByNominalCapLowOutLimit
            ! actuator for low out limit
            CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Low Outlet Temperature Limit', '[C]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%LowOutTempLimit)
          CASE ('MEETSLOADWITHNOMINALCAPACITYHIOUTLIMIT')
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_ByNominalCapHiOutLimit
            ! actuator for hi out limit
            CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'High Outlet Temperature Limit', '[C]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HiOutTempLimit)
          END SELECT

          SELECT CASE (TRIM(cAlphaArgs(aArgCount + 3)))
          CASE ('NEEDSFLOWIFLOOPON' )
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%FlowPriority = LoopFlowStatus_NeedyIfLoopOn
          CASE ('NEEDSFLOWANDTURNSLOOPON')
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn
          CASE ('RECEIVESWHATEVERFLOWAVAILABLE' )
            UserPlantComp(CompLoop)%Loop(ConnectionLoop)%FlowPriority = LoopFlowStatus_TakesWhatGets
          END SELECT

          ! find program manager for initial setup, begin environment and sizing of this plant connection
          IF (.NOT. lAlphaFieldBlanks(aArgCount + 4)) THEN
            StackMngrNum = FindItemInList(cAlphaArgs(aArgCount + 4), EMSProgramCallManager%Name, NumProgramCallManagers)
            IF (StackMngrNum > 0) THEN ! found it
              UserPlantComp(CompLoop)%Loop(ConnectionLoop)%ErlInitProgramMngr = StackMngrNum
            ELSE
              CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(aArgCount + 4))//'='//TRIM(cAlphaArgs(aArgCount + 4)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Program Manager Name not found.')
              ErrorsFound = .TRUE.
            ENDIF
          ENDIF

          ! find program to call for model simulations for just this plant connection
          IF (.NOT. lAlphaFieldBlanks(aArgCount + 5)) THEN
            StackMngrNum = FindItemInList(cAlphaArgs(aArgCount + 5), EMSProgramCallManager%Name, NumProgramCallManagers)
            IF (StackMngrNum > 0) THEN ! found it
              UserPlantComp(CompLoop)%Loop(ConnectionLoop)%ErlSimProgramMngr = StackMngrNum
            ELSE
              CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(aArgCount + 4))//'='//TRIM(cAlphaArgs(aArgCount + 4)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Program Manager Name not found.')
              ErrorsFound = .TRUE.
            ENDIF
          ENDIF
          !Setup Internal Variables
          !model input related internal variables
          CALL SetupEMSInternalVariable( 'Inlet Temperature for Plant Connection '//TRIM(LoopStr) , &
                                          UserPlantComp(CompLoop)%Name, '[C]', &
                                          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%InletTemp )
          CALL SetupEMSInternalVariable( 'Inlet Mass Flow Rate for Plant Connection '//TRIM(LoopStr) , &
                                          UserPlantComp(CompLoop)%Name, '[kg/s]', &
                                          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%InletMassFlowRate )
          IF (UserPlantComp(CompLoop)%Loop(ConnectionLoop)%HowLoadServed /= HowMet_NoneDemand) THEN
            CALL SetupEMSInternalVariable( 'Load Request for Plant Connection '//TRIM(LoopStr) , &
                                          UserPlantComp(CompLoop)%Name, '[W]', &
                                          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%MyLoad )
          ENDIF
          CALL SetupEMSInternalVariable( 'Inlet Density for Plant Connection '//TRIM(LoopStr) , &
                                          UserPlantComp(CompLoop)%Name, '[kg/m3]', &
                                          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%InletRho )
          CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Plant Connection '//TRIM(LoopStr) , &
                                          UserPlantComp(CompLoop)%Name, '[J/kg-C]', &
                                          UserPlantComp(CompLoop)%Loop(ConnectionLoop)%InletCp )
          ! model results related actuators
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Outlet Temperature', '[C]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%OutletTemp )
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%MassFlowRateRequest)
          ! model initialization and sizing related actuators
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Minimum Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%MassFlowRateMin)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Maximum Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%MassFlowRateMax)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Design Volume Flow Rate', '[m3/s]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%DesignVolumeFlowRate)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Minimum Loading Capacity', '[W]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%MinLoad )
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Maximum Loading Capacity', '[W]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%MaxLoad )
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserPlantComp(CompLoop)%Name, &
                                          'Optimal Loading Capacity', '[W]', lDummy, &
                                           UserPlantComp(CompLoop)%Loop(ConnectionLoop)%OptLoad )
        ENDDO
      ENDIF


      IF (.NOT. lAlphaFieldBlanks(27) ) THEN
        UserPlantComp(CompLoop)%Air%InletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(27),ErrorsFound,TRIM(cCurrentModuleObject),UserPlantComp(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
          !model input related internal variables
        CALL SetupEMSInternalVariable( 'Inlet Temperature for Air Connection' , UserPlantComp(CompLoop)%Name, '[C]', &
                                        UserPlantComp(CompLoop)%Air%InletTemp )
        CALL SetupEMSInternalVariable( 'Inlet Mass Flow Rate for Air Connection' , UserPlantComp(CompLoop)%Name, '[kg/s]', &
                                        UserPlantComp(CompLoop)%Air%InletMassFlowRate )
        CALL SetupEMSInternalVariable( 'Inlet Humidity Ratio for Air Connection' , UserPlantComp(CompLoop)%Name,  &
                                       '[kgWater/kgDryAir]', &
                                        UserPlantComp(CompLoop)%Air%InletHumRat )
        CALL SetupEMSInternalVariable( 'Inlet Density for Air Connection' , UserPlantComp(CompLoop)%Name, '[kg/m3]', &
                                        UserPlantComp(CompLoop)%Air%InletRho )
        CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Air Connection' , UserPlantComp(CompLoop)%Name, '[J/kg-C]', &
                                        UserPlantComp(CompLoop)%Air%InletCp )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(28) ) THEN
        UserPlantComp(CompLoop)%Air%OutletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(28),ErrorsFound,TRIM(cCurrentModuleObject),UserPlantComp(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_ReliefAir,1,ObjectIsNotParent)
          !outlet air node results
          CALL SetupEMSActuator('Air Connection', UserPlantComp(CompLoop)%Name, &
                                          'Outlet Temperature', '[C]', lDummy, &
                                          UserPlantComp(CompLoop)%Air%OutletTemp )
          CALL SetupEMSActuator('Air Connection', UserPlantComp(CompLoop)%Name, &
                                          'Outlet Humidity Ratio', '[kgWater/kgDryAir]', lDummy, &
                                          UserPlantComp(CompLoop)%Air%OutletHumRat )
          CALL SetupEMSActuator('Air Connection', UserPlantComp(CompLoop)%Name, &
                                          'Mass Flow Rate', '[kg/s]', lDummy, &
                                          UserPlantComp(CompLoop)%Air%OutletMassFlowRate)
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(29) ) THEN
        CALL SetupTankDemandComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(29), ErrorsFound, &
                              UserPlantComp(CompLoop)%Water%SupplyTankID, UserPlantComp(CompLoop)%Water%SupplyTankDemandARRID)

        UserPlantComp(CompLoop)%Water%SuppliedByWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserPlantComp(CompLoop)%Name, &
                                          'Supplied Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserPlantComp(CompLoop)%Water%SupplyVdotRequest )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(30) ) THEN
        CALL SetupTankSupplyComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(30), ErrorsFound, &
                              UserPlantComp(CompLoop)%Water%CollectionTankID,   &
                              UserPlantComp(CompLoop)%Water%CollectionTankSupplyARRID)
        UserPlantComp(CompLoop)%Water%CollectsToWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserPlantComp(CompLoop)%Name, &
                                          'Collected Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserPlantComp(CompLoop)%Water%CollectedVdot )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(31) ) THEN

        UserPlantComp(CompLoop)%Zone%ZoneNum = FindItemInList(cAlphaArgs(31),Zone%Name,NumOfZones)
        IF (UserPlantComp(CompLoop)%Zone%ZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Ambient Zone Name not found = '//TRIM(cAlphaArgs(31)))
          ErrorsFound = .TRUE.
        ELSE
          UserPlantComp(CompLoop)%Zone%DeviceHasInternalGains = .TRUE.
          CALL SetupZoneInternalGain(UserPlantComp(CompLoop)%Zone%ZoneNum, &
                                     TRIM(cCurrentModuleObject), &
                                     TRIM(cAlphaArgs(1)), &
                                     IntGainTypeOf_PlantComponentUserDefined, &
                                     ConvectionGainRate          = UserPlantComp(CompLoop)%Zone%ConvectionGainRate, &
                                     ReturnAirConvectionGainRate = UserPlantComp(CompLoop)%Zone%ReturnAirConvectionGainRate, &
                                     ThermalRadiationGainRate    = UserPlantComp(CompLoop)%Zone%ThermalRadiationGainRate, &
                                     LatentGainRate              = UserPlantComp(CompLoop)%Zone%LatentGainRate, &
                                     ReturnAirLatentGainRate     = UserPlantComp(CompLoop)%Zone%ReturnAirLatentGainRate, &
                                     CarbonDioxideGainRate       = UserPlantComp(CompLoop)%Zone%CarbonDioxideGainRate, &
                                     GenericContamGainRate       = UserPlantComp(CompLoop)%Zone%GenericContamGainRate )

          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Sensible Heat Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%ConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Return Air Heat Sensible Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%ReturnAirConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Thermal Radiation Heat Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%ThermalRadiationGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%LatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Return Air Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%ReturnAirLatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Carbon Dioxide Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%CarbonDioxideGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserPlantComp(CompLoop)%Name, &
                                'Gaseous Contaminant Gain Rate', '[W]', lDummy, &
                                 UserPlantComp(CompLoop)%Zone%GenericContamGainRate )
        ENDIF
      ENDIF


      ! make sure user has entered at least some erl program managers to actually calculate something
      MgrCountTest = 0
      IF (UserPlantComp(CompLoop)%ErlSimProgramMngr > 0) MgrCountTest = 1
      DO ConnectionLoop = 1, NumPlantConnections
        IF (UserPlantComp(CompLoop)%Loop(ConnectionLoop)%ErlInitProgramMngr > 0) MgrCountTest = MgrCountTest + 1
        IF (UserPlantComp(CompLoop)%Loop(ConnectionLoop)%ErlSimProgramMngr > 0)  MgrCountTest = MgrCountTest + 1
      ENDDO
      IF (MgrCountTest == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('At least one program calling manager is needed.')
        ErrorsFound = .TRUE.
      ENDIF

    ENDDO
  ENDIF !NumUserPlantComps > 0

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetUserDefinedComponents: Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
  ENDIF

  cCurrentModuleObject = 'Coil:UserDefined'
  NumUserCoils = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumUserCoils > 0) THEN
    ALLOCATE(UserCoil(NumUserCoils))
    ALLOCATE(CheckUserCoilName(NumUserCoils))
    CheckUserCoilName = .TRUE.
    DO CompLoop = 1, NumUserCoils
      CALL GetObjectItem(cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), UserCoil%Name, CompLoop - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      CALL VerifyUniqueCoilName(cCurrentModuleObject,cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF
      UserCoil(CompLoop)%Name =  cAlphaArgs(1)

      ! now get program manager for model simulations
      IF (.NOT. lAlphaFieldBlanks(2)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(2), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserCoil(CompLoop)%ErlSimProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      ! now get program manager for model initializations
      IF (.NOT. lAlphaFieldBlanks(3)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(3), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserCoil(CompLoop)%ErlInitProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      NumAirConnections = FLOOR(rNumericArgs(1))
      IF ((NumAirConnections >= 1) .AND. (NumAirConnections <= 2)) THEN
        ALLOCATE(UserCoil(CompLoop)%Air(NumAirConnections))
        UserCoil(CompLoop)%NumAirConnections = NumAirConnections
        DO ConnectionLoop = 1, NumAirConnections
          aArgCount = (ConnectionLoop - 1) * 2 + 4
          UserCoil(CompLoop)%Air(ConnectionLoop)%InletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(aArgCount),ErrorsFound,TRIM(cCurrentModuleObject),UserCoil(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

          LoopStr=RoundSigDigits(ConnectionLoop)
            !model input related internal variables
          CALL SetupEMSInternalVariable( 'Inlet Temperature for Air Connection '//TRIM(LoopStr) , UserCoil(CompLoop)%Name, &
                                         '[C]',      UserCoil(CompLoop)%Air(ConnectionLoop)%InletTemp )
          CALL SetupEMSInternalVariable( 'Inlet Mass Flow Rate for Air Connection '//TRIM(LoopStr) , UserCoil(CompLoop)%Name, &
                                         '[kg/s]',   UserCoil(CompLoop)%Air(ConnectionLoop)%InletMassFlowRate )
          CALL SetupEMSInternalVariable( 'Inlet Humidity Ratio for Air Connection '//TRIM(LoopStr) , UserCoil(CompLoop)%Name, &
                                         '[kgWater/kgDryAir]',  UserCoil(CompLoop)%Air(ConnectionLoop)%InletHumRat )
          CALL SetupEMSInternalVariable( 'Inlet Density for Air Connection '//TRIM(LoopStr) , UserCoil(CompLoop)%Name, &
                                         '[kg/m3]',  UserCoil(CompLoop)%Air(ConnectionLoop)%InletRho )
          CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Air Connection '//TRIM(LoopStr) , UserCoil(CompLoop)%Name, &
                                         '[J/kg-C]', UserCoil(CompLoop)%Air(ConnectionLoop)%InletCp )

          UserCoil(CompLoop)%Air(ConnectionLoop)%OutletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(aArgCount + 1),ErrorsFound,TRIM(cCurrentModuleObject),UserCoil(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
          CALL SetupEMSActuator('Air Connection '//TRIM(LoopStr), UserCoil(CompLoop)%Name, &
                                          'Outlet Temperature', '[C]', lDummy, &
                                          UserCoil(CompLoop)%Air(ConnectionLoop)%OutletTemp )
          CALL SetupEMSActuator('Air Connection '//TRIM(LoopStr), UserCoil(CompLoop)%Name, &
                                          'Outlet Humidity Ratio', '[kgWater/kgDryAir]', lDummy, &
                                          UserCoil(CompLoop)%Air(ConnectionLoop)%OutletHumRat )
          CALL SetupEMSActuator('Air Connection '//TRIM(LoopStr), UserCoil(CompLoop)%Name, &
                                          'Mass Flow Rate', '[kg/s]', lDummy, &
                                          UserCoil(CompLoop)%Air(ConnectionLoop)%OutletMassFlowRate)

          CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(aArgCount),cAlphaArgs(aArgCount + 1),  &
             'Air Nodes '//LoopStr)

        ENDDO

        IF (.NOT. lAlphaFieldBlanks(8) ) THEN
          SELECT CASE (cAlphaArgs(8) )

          CASE ('YES')
            UserCoil(CompLoop)%PlantIsConnected = .TRUE.
          CASE ('NO')
            UserCoil(CompLoop)%PlantIsConnected = .FALSE.
          END SELECT

        ELSE
          UserCoil(CompLoop)%PlantIsConnected = .FALSE.
        ENDIF

        IF (UserCoil(CompLoop)%PlantIsConnected) THEN ! get input
          UserCoil(CompLoop)%Loop%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 2, ObjectIsNotParent)
          UserCoil(CompLoop)%Loop%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 2, ObjectIsNotParent)

          CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(9),cAlphaArgs(10),'Plant Nodes')

          ! this model is only for plant connections that are "Demand"
          UserCoil(CompLoop)%Loop%HowLoadServed = HowMet_NoneDemand
          ! this model is only for plant connections that are needy and turn loop on
          UserCoil(CompLoop)%Loop%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn

          !Setup Internal Variables
          !model input related internal variables
          CALL SetupEMSInternalVariable( 'Inlet Temperature for Plant Connection' , UserCoil(CompLoop)%Name, '[C]', &
                                          UserCoil(CompLoop)%Loop%InletTemp )
          CALL SetupEMSInternalVariable( 'Inlet Mass Flow Rate for Plant Connection' , UserCoil(CompLoop)%Name, '[kg/s]', &
                                          UserCoil(CompLoop)%Loop%InletMassFlowRate )
          CALL SetupEMSInternalVariable( 'Inlet Density for Plant Connection' , UserCoil(CompLoop)%Name, '[kg/m3]', &
                                          UserCoil(CompLoop)%Loop%InletRho )
          CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Plant Connection' , UserCoil(CompLoop)%Name, '[J/kg-C]', &
                                          UserCoil(CompLoop)%Loop%InletCp )
          ! model results related actuators
          CALL SetupEMSActuator('Plant Connection', UserCoil(CompLoop)%Name, &
                                          'Outlet Temperature', '[C]', lDummy, &
                                          UserCoil(CompLoop)%Loop%OutletTemp )
          CALL SetupEMSActuator('Plant Connection', UserCoil(CompLoop)%Name, &
                                          'Mass Flow Rate', '[kg/s]', lDummy, &
                                          UserCoil(CompLoop)%Loop%MassFlowRateRequest)
          ! model initialization and sizing related actuators
          CALL SetupEMSActuator('Plant Connection ', UserCoil(CompLoop)%Name, &
                                          'Design Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserCoil(CompLoop)%Loop%DesignVolumeFlowRate)

          CALL SetupEMSActuator('Plant Connection', UserCoil(CompLoop)%Name, &
                                          'Minimum Mass Flow Rate', '[kg/s]', lDummy, &
                                          UserCoil(CompLoop)%Loop%MassFlowRateMin)
          CALL SetupEMSActuator('Plant Connection', UserCoil(CompLoop)%Name, &
                                          'Maximum Mass Flow Rate', '[kg/s]', lDummy, &
                                          UserCoil(CompLoop)%Loop%MassFlowRateMax)
        ENDIF

      IF (.NOT. lAlphaFieldBlanks(11) ) THEN
        CALL SetupTankDemandComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(11), ErrorsFound, &
                              UserCoil(CompLoop)%Water%SupplyTankID, UserCoil(CompLoop)%Water%SupplyTankDemandARRID)

        UserCoil(CompLoop)%Water%SuppliedByWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserCoil(CompLoop)%Name, &
                                          'Supplied Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserCoil(CompLoop)%Water%SupplyVdotRequest )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(12) ) THEN
        CALL SetupTankSupplyComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(12), ErrorsFound, &
                              UserCoil(CompLoop)%Water%CollectionTankID, UserCoil(CompLoop)%Water%CollectionTankSupplyARRID)
        UserCoil(CompLoop)%Water%CollectsToWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserCoil(CompLoop)%Name, &
                                          'Collected Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserCoil(CompLoop)%Water%CollectedVdot )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(13) ) THEN

        UserCoil(CompLoop)%Zone%ZoneNum = FindItemInList(cAlphaArgs(13),Zone%Name,NumOfZones)
        IF (UserCoil(CompLoop)%Zone%ZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Ambient Zone Name not found = '//TRIM(cAlphaArgs(13)))
          ErrorsFound = .TRUE.
        ELSE
          UserCoil(CompLoop)%Zone%DeviceHasInternalGains = .TRUE.
          CALL SetupZoneInternalGain(UserCoil(CompLoop)%Zone%ZoneNum, &
                                     TRIM(cCurrentModuleObject), &
                                     TRIM(cAlphaArgs(1)), &
                                     IntGainTypeOf_CoilUserDefined, &
                                     ConvectionGainRate          = UserCoil(CompLoop)%Zone%ConvectionGainRate, &
                                     ReturnAirConvectionGainRate = UserCoil(CompLoop)%Zone%ReturnAirConvectionGainRate, &
                                     ThermalRadiationGainRate    = UserCoil(CompLoop)%Zone%ThermalRadiationGainRate, &
                                     LatentGainRate              = UserCoil(CompLoop)%Zone%LatentGainRate, &
                                     ReturnAirLatentGainRate     = UserCoil(CompLoop)%Zone%ReturnAirLatentGainRate, &
                                     CarbonDioxideGainRate       = UserCoil(CompLoop)%Zone%CarbonDioxideGainRate, &
                                     GenericContamGainRate       = UserCoil(CompLoop)%Zone%GenericContamGainRate )

          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Sensible Heat Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%ConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Return Air Heat Sensible Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%ReturnAirConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Thermal Radiation Heat Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%ThermalRadiationGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%LatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Return Air Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%ReturnAirLatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Carbon Dioxide Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%CarbonDioxideGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserCoil(CompLoop)%Name, &
                                'Gaseous Contaminant Gain Rate', '[W]', lDummy, &
                                 UserCoil(CompLoop)%Zone%GenericContamGainRate )
        ENDIF
      ENDIF

      ENDIF
    ENDDO

  ENDIF !NumUserCoils > 0

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetUserDefinedComponents: Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
  ENDIF

  cCurrentModuleObject = 'ZoneHVAC:ForcedAir:UserDefined'
  NumUserZoneAir = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumUserZoneAir > 0) THEN
    ALLOCATE(UserZoneAirHVAC(NumUserZoneAir))
    ALLOCATE(CheckUserZoneAirName(NumUserZoneAir))
    CheckUserZoneAirName = .TRUE.
    DO  CompLoop=1, NumUserZoneAir
      CALL GetObjectItem(cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), UserZoneAirHVAC%Name, CompLoop - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      UserZoneAirHVAC(CompLoop)%Name =  cAlphaArgs(1)

      ! now get program manager for model simulations
      IF (.NOT. lAlphaFieldBlanks(2)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(2), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserZoneAirHVAC(CompLoop)%ErlSimProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      ! now get program manager for model initializations
      IF (.NOT. lAlphaFieldBlanks(3)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(3), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserZoneAirHVAC(CompLoop)%ErlInitProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      UserZoneAirHVAC(CompLoop)%ZoneAir%InletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),UserZoneAirHVAC(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        !model input related internal variables
      CALL SetupEMSInternalVariable( 'Inlet Temperature for Primary Air Connection' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[C]',      UserZoneAirHVAC(CompLoop)%ZoneAir%InletTemp )
      CALL SetupEMSInternalVariable( 'Inlet Humidity Ratio for Primary Air Connection' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[kgWater/kgDryAir]',  UserZoneAirHVAC(CompLoop)%ZoneAir%InletHumRat )
      CALL SetupEMSInternalVariable( 'Inlet Density for Primary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                     '[kg/m3]',  UserZoneAirHVAC(CompLoop)%ZoneAir%InletRho )
      CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Primary Air Connection' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[J/kg-C]', UserZoneAirHVAC(CompLoop)%ZoneAir%InletCp )

      CALL SetupEMSInternalVariable( 'Remaining Sensible Load to Heating Setpoint' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[W]', UserZoneAirHVAC(CompLoop)%RemainingOutputToHeatingSP )
      CALL SetupEMSInternalVariable( 'Remaining Sensible Load to Cooling Setpoint' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[W]', UserZoneAirHVAC(CompLoop)%RemainingOutputToCoolingSP )
      CALL SetupEMSInternalVariable( 'Remaining Latent Load to Humidifying Setpoint' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[kg/s]', UserZoneAirHVAC(CompLoop)%RemainingOutputReqToHumidSP )
      CALL SetupEMSInternalVariable( 'Remaining Latent Load to Dehumidifying Setpoint' , UserZoneAirHVAC(CompLoop)%Name, &
                                     '[kg/s]', UserZoneAirHVAC(CompLoop)%RemainingOutputReqToDehumidSP )

      CALL SetupEMSActuator('Primary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                      'Inlet Mass Flow Rate', '[kg/s]', lDummy, &
                                      UserZoneAirHVAC(CompLoop)%ZoneAir%InletMassFlowRate)
      UserZoneAirHVAC(CompLoop)%ZoneAir%OutletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),UserZoneAirHVAC(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
      CALL SetupEMSActuator('Primary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                      'Outlet Temperature', '[C]', lDummy, &
                                      UserZoneAirHVAC(CompLoop)%ZoneAir%OutletTemp )
      CALL SetupEMSActuator('Primary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                      'Outlet Humidity Ratio', '[kgWater/kgDryAir]', lDummy, &
                                      UserZoneAirHVAC(CompLoop)%ZoneAir%OutletHumRat )
      CALL SetupEMSActuator('Primary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                      'Outlet Mass Flow Rate', '[kg/s]', lDummy, &
                                      UserZoneAirHVAC(CompLoop)%ZoneAir%OutletMassFlowRate)

      IF (.NOT. lAlphaFieldBlanks(6) ) THEN
        UserZoneAirHVAC(CompLoop)%SourceAir%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),UserZoneAirHVAC(CompLoop)%Name, &
                           NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)
          !model input related internal variables
        CALL SetupEMSInternalVariable( 'Inlet Temperature for Secondary Air Connection' , UserZoneAirHVAC(CompLoop)%Name, &
                                       '[C]',      UserZoneAirHVAC(CompLoop)%SourceAir%InletTemp )

        CALL SetupEMSInternalVariable( 'Inlet Humidity Ratio for Secondary Air Connection' , UserZoneAirHVAC(CompLoop)%Name, &
                                       '[kgWater/kgDryAir]',  UserZoneAirHVAC(CompLoop)%SourceAir%InletHumRat )
        CALL SetupEMSInternalVariable( 'Inlet Density for Secondary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                       '[kg/m3]',  UserZoneAirHVAC(CompLoop)%SourceAir%InletRho )
        CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Secondary Air Connection' , UserZoneAirHVAC(CompLoop)%Name, &
                                       '[J/kg-C]', UserZoneAirHVAC(CompLoop)%SourceAir%InletCp )
        CALL SetupEMSActuator('Secondary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                       'Inlet Mass Flow Rate', '[kg/s]', lDummy, &
                                        UserZoneAirHVAC(CompLoop)%SourceAir%InletMassFlowRate)
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(7) ) THEN
        UserZoneAirHVAC(CompLoop)%SourceAir%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),UserZoneAirHVAC(CompLoop)%Name, &
                           NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        CALL SetupEMSActuator('Secondary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                        'Outlet Temperature', '[C]', lDummy, &
                                        UserZoneAirHVAC(CompLoop)%SourceAir%OutletTemp )
        CALL SetupEMSActuator('Secondary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                        'Outlet Humidity Ratio', '[kgWater/kgDryAir]', lDummy, &
                                        UserZoneAirHVAC(CompLoop)%SourceAir%OutletHumRat )
        CALL SetupEMSActuator('Secondary Air Connection', UserZoneAirHVAC(CompLoop)%Name, &
                                        'Mass Flow Rate', '[kg/s]', lDummy, &
                                        UserZoneAirHVAC(CompLoop)%SourceAir%OutletMassFlowRate)
      ENDIF

      IF ((UserZoneAirHVAC(CompLoop)%SourceAir%InletNodeNum > 0) .and. &
          (UserZoneAirHVAC(CompLoop)%SourceAir%OutletNodeNum > 0) ) THEN
        !  CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Air Nodes')
      ENDIF


      NumPlantConnections = FLOOR(rNumericArgs(1))
      UserZoneAirHVAC(CompLoop)%NumPlantConnections =NumPlantConnections
      IF ((NumPlantConnections >= 1) .AND. (NumPlantConnections <= 3)) THEN
        ALLOCATE(UserZoneAirHVAC(CompLoop)%Loop(NumPlantConnections))
        DO ConnectionLoop = 1, NumPlantConnections
          aArgCount = (ConnectionLoop-1) *  2 + 8
          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(aArgCount),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, (ConnectionLoop+2), ObjectIsNotParent)
          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(aArgCount + 1),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, (ConnectionLoop+2), ObjectIsNotParent)
          CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(aArgCount),cAlphaArgs(aArgCount + 1),'Plant Nodes')
          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_NoneDemand
          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn
          !Setup Internal Variables
          WRITE(LoopStr,*) ConnectionLoop
          LoopStr = ADJUSTL(LoopStr)
          !model input related internal variables
          CALL SetupEMSInternalVariable( 'Inlet Temperature for Plant Connection '//TRIM(LoopStr) , &
                                          UserZoneAirHVAC(CompLoop)%Name, '[C]', &
                                          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%InletTemp )
          CALL SetupEMSInternalVariable( 'Inlet Mass Flow Rate for Plant Connection '//TRIM(LoopStr) , &
                                          UserZoneAirHVAC(CompLoop)%Name, '[kg/s]', &
                                          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%InletMassFlowRate )
          CALL SetupEMSInternalVariable( 'Inlet Density for Plant Connection '//TRIM(LoopStr) , &
                                          UserZoneAirHVAC(CompLoop)%Name, '[kg/m3]', &
                                          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%InletRho )
          CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Plant Connection '//TRIM(LoopStr) , &
                                          UserZoneAirHVAC(CompLoop)%Name, '[J/kg-C]', &
                                          UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%InletCp )
          ! model results related actuators
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserZoneAirHVAC(CompLoop)%Name, &
                                          'Outlet Temperature', '[C]', lDummy, &
                                           UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%OutletTemp )
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserZoneAirHVAC(CompLoop)%Name, &
                                          'Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%MassFlowRateRequest)
          ! model initialization and sizing related actuators
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserZoneAirHVAC(CompLoop)%Name, &
                                          'Minimum Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%MassFlowRateMin)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserZoneAirHVAC(CompLoop)%Name, &
                                          'Maximum Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%MassFlowRateMax)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserZoneAirHVAC(CompLoop)%Name, &
                                          'Design Volume Flow Rate', '[m3/s]', lDummy, &
                                           UserZoneAirHVAC(CompLoop)%Loop(ConnectionLoop)%DesignVolumeFlowRate)

        ENDDO
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(14) ) THEN
        CALL SetupTankDemandComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(14), ErrorsFound, &
                              UserZoneAirHVAC(CompLoop)%Water%SupplyTankID, UserZoneAirHVAC(CompLoop)%Water%SupplyTankDemandARRID)

        UserZoneAirHVAC(CompLoop)%Water%SuppliedByWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserZoneAirHVAC(CompLoop)%Name, &
                                          'Supplied Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserZoneAirHVAC(CompLoop)%Water%SupplyVdotRequest )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(15) ) THEN
        CALL SetupTankSupplyComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(15), ErrorsFound, &
                              UserZoneAirHVAC(CompLoop)%Water%CollectionTankID,   &
                              UserZoneAirHVAC(CompLoop)%Water%CollectionTankSupplyARRID)
        UserZoneAirHVAC(CompLoop)%Water%CollectsToWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserZoneAirHVAC(CompLoop)%Name, &
                                          'Collected Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserZoneAirHVAC(CompLoop)%Water%CollectedVdot )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(16) ) THEN

        UserZoneAirHVAC(CompLoop)%Zone%ZoneNum = FindItemInList(cAlphaArgs(16),Zone%Name,NumOfZones)
        IF (UserZoneAirHVAC(CompLoop)%Zone%ZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Ambient Zone Name not found = '//TRIM(cAlphaArgs(16)))
          ErrorsFound = .TRUE.
        ELSE
          UserZoneAirHVAC(CompLoop)%Zone%DeviceHasInternalGains = .TRUE.
          CALL SetupZoneInternalGain(UserZoneAirHVAC(CompLoop)%Zone%ZoneNum, &
                                     TRIM(cCurrentModuleObject), &
                                     TRIM(cAlphaArgs(1)), &
                                     IntGainTypeOf_ZoneHVACForcedAirUserDefined, &
                                     ConvectionGainRate          = UserZoneAirHVAC(CompLoop)%Zone%ConvectionGainRate, &
                                     ReturnAirConvectionGainRate = UserZoneAirHVAC(CompLoop)%Zone%ReturnAirConvectionGainRate, &
                                     ThermalRadiationGainRate    = UserZoneAirHVAC(CompLoop)%Zone%ThermalRadiationGainRate, &
                                     LatentGainRate              = UserZoneAirHVAC(CompLoop)%Zone%LatentGainRate, &
                                     ReturnAirLatentGainRate     = UserZoneAirHVAC(CompLoop)%Zone%ReturnAirLatentGainRate, &
                                     CarbonDioxideGainRate       = UserZoneAirHVAC(CompLoop)%Zone%CarbonDioxideGainRate, &
                                     GenericContamGainRate       = UserZoneAirHVAC(CompLoop)%Zone%GenericContamGainRate )

          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Sensible Heat Gain Rate', '[W]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%ConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Return Air Heat Sensible Gain Rate', '[W]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%ReturnAirConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Thermal Radiation Heat Gain Rate', '[W]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%ThermalRadiationGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%LatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Return Air Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%ReturnAirLatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Carbon Dioxide Gain Rate', '[m3/s]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%CarbonDioxideGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserZoneAirHVAC(CompLoop)%Name, &
                                'Gaseous Contaminant Gain Rate', '[m3/s]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%GenericContamGainRate )
        ENDIF
      ENDIF


    ENDDO
  ENDIF !NumUserZoneAir > 0

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetUserDefinedComponents: Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
  ENDIF

  cCurrentModuleObject = 'AirTerminal:SingleDuct:UserDefined'
  NumUserAirTerminals = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumUserAirTerminals > 0) THEN
    ALLOCATE(UserAirTerminal(NumUserAirTerminals))
    ALLOCATE(CheckUserAirTerminal(NumUserAirTerminals))
    CheckUserAirTerminal = .TRUE.
    DO  CompLoop=1, NumUserAirTerminals
      CALL GetObjectItem(cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), UserAirTerminal%Name, CompLoop - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      UserAirTerminal(CompLoop)%Name =  cAlphaArgs(1)

      ! now get program manager for model simulations
      IF (.NOT. lAlphaFieldBlanks(2)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(2), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserAirTerminal(CompLoop)%ErlSimProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      ! now get program manager for model initializations
      IF (.NOT. lAlphaFieldBlanks(3)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(3), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          UserAirTerminal(CompLoop)%ErlInitProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      UserAirTerminal(CompLoop)%AirLoop%InletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),UserAirTerminal(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFieldNames(4))
        !model input related internal variables
      CALL SetupEMSInternalVariable( 'Inlet Temperature for Primary Air Connection' , UserAirTerminal(CompLoop)%Name, &
                                     '[C]',      UserAirTerminal(CompLoop)%AirLoop%InletTemp )
      CALL SetupEMSInternalVariable( 'Inlet Humidity Ratio for Primary Air Connection' , UserAirTerminal(CompLoop)%Name, &
                                     '[kgWater/kgDryAir]',  UserAirTerminal(CompLoop)%AirLoop%InletHumRat )
      CALL SetupEMSInternalVariable( 'Inlet Density for Primary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                     '[kg/m3]',  UserAirTerminal(CompLoop)%AirLoop%InletRho )
      CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Primary Air Connection' , UserAirTerminal(CompLoop)%Name, &
                                     '[J/kg-C]', UserAirTerminal(CompLoop)%AirLoop%InletCp )

      CALL SetupEMSInternalVariable( 'Remaining Sensible Load to Heating Setpoint' , UserAirTerminal(CompLoop)%Name, &
                                     '[W]', UserAirTerminal(CompLoop)%RemainingOutputToHeatingSP )
      CALL SetupEMSInternalVariable( 'Remaining Sensible Load to Cooling Setpoint' , UserAirTerminal(CompLoop)%Name, &
                                     '[W]', UserAirTerminal(CompLoop)%RemainingOutputToCoolingSP )
      CALL SetupEMSInternalVariable( 'Remaining Latent Load to Humidifying Setpoint' , UserAirTerminal(CompLoop)%Name, &
                                     '[kg/s]', UserAirTerminal(CompLoop)%RemainingOutputReqToHumidSP )
      CALL SetupEMSInternalVariable( 'Remaining Latent Load to Dehumidifying Setpoint' , UserAirTerminal(CompLoop)%Name, &
                                     '[kg/s]', UserAirTerminal(CompLoop)%RemainingOutputReqToDehumidSP )

      CALL SetupEMSActuator('Primary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                      'Inlet Mass Flow Rate', '[kg/s]', lDummy, &
                                      UserAirTerminal(CompLoop)%AirLoop%InletMassFlowRate)
      UserAirTerminal(CompLoop)%AirLoop%OutletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),UserAirTerminal(CompLoop)%Name, &
                         NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFieldNames(5))
      CALL SetupEMSActuator('Primary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                      'Outlet Temperature', '[C]', lDummy, &
                                      UserAirTerminal(CompLoop)%AirLoop%OutletTemp )
      CALL SetupEMSActuator('Primary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                      'Outlet Humidity Ratio', '[kgWater/kgDryAir]', lDummy, &
                                      UserAirTerminal(CompLoop)%AirLoop%OutletHumRat )
      CALL SetupEMSActuator('Primary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                      'Outlet Mass Flow Rate', '[kg/s]', lDummy, &
                                      UserAirTerminal(CompLoop)%AirLoop%OutletMassFlowRate)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Air Nodes')

      ! Fill the Zone Equipment data with the inlet node number of this unit.
      DO CtrlZone = 1,NumOfZones
        IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
        DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
          IF (UserAirTerminal(CompLoop)%AirLoop%OutletNodeNum == ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
            IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
              CALL ShowSevereError('Error in connecting a terminal unit to a zone')
              CALL ShowContinueError(TRIM(NodeID(UserAirTerminal(CompLoop)%AirLoop%OutletNodeNum)) &
                    //' already connects to another zone')
              CALL ShowContinueError('Occurs for terminal unit '//TRIM(cCurrentModuleObject)//' = ' &
                    //TRIM(UserAirTerminal(CompLoop)%Name))
              CALL ShowContinueError('Check terminal unit node names for errors')
              ErrorsFound = .true.
            ELSE
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode  = UserAirTerminal(CompLoop)%AirLoop%InletNodeNum
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = UserAirTerminal(CompLoop)%AirLoop%OutletNodeNum
            END IF

            UserAirTerminal(CompLoop)%ActualCtrlZoneNum   = CtrlZone

          END IF
        END DO
      END DO


      IF (.NOT. lAlphaFieldBlanks(6) ) THEN
        UserAirTerminal(CompLoop)%SourceAir%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),UserAirTerminal(CompLoop)%Name, &
                           NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent,cAlphaFieldNames(6))
          !model input related internal variables
        CALL SetupEMSInternalVariable( 'Inlet Temperature for Secondary Air Connection' , UserAirTerminal(CompLoop)%Name, &
                                       '[C]',      UserAirTerminal(CompLoop)%SourceAir%InletTemp )

        CALL SetupEMSInternalVariable( 'Inlet Humidity Ratio for Secondary Air Connection' , UserAirTerminal(CompLoop)%Name, &
                                       '[kgWater/kgDryAir]',  UserAirTerminal(CompLoop)%SourceAir%InletHumRat )
        CALL SetupEMSInternalVariable( 'Inlet Density for Secondary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                       '[kg/m3]',  UserAirTerminal(CompLoop)%SourceAir%InletRho )
        CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Secondary Air Connection' , UserAirTerminal(CompLoop)%Name, &
                                       '[J/kg-C]', UserAirTerminal(CompLoop)%SourceAir%InletCp )
        CALL SetupEMSActuator('Secondary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                       'Inlet Mass Flow Rate', '[kg/s]', lDummy, &
                                        UserAirTerminal(CompLoop)%SourceAir%InletMassFlowRate)
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(7) ) THEN
        UserAirTerminal(CompLoop)%SourceAir%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),UserAirTerminal(CompLoop)%Name, &
                           NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent,cAlphaFieldNames(7))
        CALL SetupEMSActuator('Secondary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                        'Outlet Temperature', '[C]', lDummy, &
                                        UserAirTerminal(CompLoop)%SourceAir%OutletTemp )
        CALL SetupEMSActuator('Secondary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                        'Outlet Humidity Ratio', '[kgWater/kgDryAir]', lDummy, &
                                        UserAirTerminal(CompLoop)%SourceAir%OutletHumRat )
        CALL SetupEMSActuator('Secondary Air Connection', UserAirTerminal(CompLoop)%Name, &
                                        'Mass Flow Rate', '[kg/s]', lDummy, &
                                        UserAirTerminal(CompLoop)%SourceAir%OutletMassFlowRate)
      ENDIF

      IF ((UserAirTerminal(CompLoop)%SourceAir%InletNodeNum > 0) .and. &
          (UserAirTerminal(CompLoop)%SourceAir%OutletNodeNum > 0) ) THEN
        !  CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Air Nodes')
      ENDIF


      NumPlantConnections = FLOOR(rNumericArgs(1))
      UserAirTerminal(CompLoop)%NumPlantConnections =NumPlantConnections
      IF ((NumPlantConnections >= 1) .AND. (NumPlantConnections <= 2)) THEN
        ALLOCATE(UserAirTerminal(CompLoop)%Loop(NumPlantConnections))
        DO ConnectionLoop = 1, NumPlantConnections
          aArgCount = (ConnectionLoop-1) *  2 + 8
          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%InletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(aArgCount),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, (ConnectionLoop+2), ObjectIsNotParent,cAlphaFieldNames(aArgCount))
          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(aArgCount + 1),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, (ConnectionLoop+2), ObjectIsNotParent,cAlphaFieldNames(aArgCount+1))
          CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(aArgCount),cAlphaArgs(aArgCount + 1),'Plant Nodes')
          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%HowLoadServed = HowMet_NoneDemand
          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn
          !Setup Internal Variables
          LoopStr = RoundSigDigits(ConnectionLoop)
          !model input related internal variables
          CALL SetupEMSInternalVariable( 'Inlet Temperature for Plant Connection '//TRIM(LoopStr) ,   &
             UserAirTerminal(CompLoop)%Name, '[C]', &
                                          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%InletTemp )
          CALL SetupEMSInternalVariable( 'Inlet Mass Flow Rate for Plant Connection '//TRIM(LoopStr) ,   &
             UserAirTerminal(CompLoop)%Name, '[kg/s]', &
                                          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%InletMassFlowRate )
          CALL SetupEMSInternalVariable( 'Inlet Density for Plant Connection '//TRIM(LoopStr) ,   &
             UserAirTerminal(CompLoop)%Name, '[kg/m3]', &
                                          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%InletRho )
          CALL SetupEMSInternalVariable( 'Inlet Specific Heat for Plant Connection '//TRIM(LoopStr) ,   &
             UserAirTerminal(CompLoop)%Name, '[J/kg-C]', &
                                          UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%InletCp )
          ! model results related actuators
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserAirTerminal(CompLoop)%Name, &
                                          'Outlet Temperature', '[C]', lDummy, &
                                           UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%OutletTemp )
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserAirTerminal(CompLoop)%Name, &
                                          'Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%MassFlowRateRequest)
          ! model initialization and sizing related actuators
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserAirTerminal(CompLoop)%Name, &
                                          'Minimum Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%MassFlowRateMin)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserAirTerminal(CompLoop)%Name, &
                                          'Maximum Mass Flow Rate', '[kg/s]', lDummy, &
                                           UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%MassFlowRateMax)
          CALL SetupEMSActuator('Plant Connection '//TRIM(LoopStr), UserAirTerminal(CompLoop)%Name, &
                                          'Design Volume Flow Rate', '[m3/s]', lDummy, &
                                           UserAirTerminal(CompLoop)%Loop(ConnectionLoop)%DesignVolumeFlowRate)

        ENDDO
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(12) ) THEN
        CALL SetupTankDemandComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(12), ErrorsFound, &
                              UserAirTerminal(CompLoop)%Water%SupplyTankID, UserAirTerminal(CompLoop)%Water%SupplyTankDemandARRID)

        UserAirTerminal(CompLoop)%Water%SuppliedByWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserAirTerminal(CompLoop)%Name, &
                                          'Supplied Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserAirTerminal(CompLoop)%Water%SupplyVdotRequest )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(13) ) THEN
        CALL SetupTankSupplyComponent(cAlphaArgs(1), TRIM(cCurrentModuleObject), cAlphaArgs(13), ErrorsFound, &
                              UserAirTerminal(CompLoop)%Water%CollectionTankID,   &
                              UserAirTerminal(CompLoop)%Water%CollectionTankSupplyARRID)
        UserAirTerminal(CompLoop)%Water%CollectsToWaterSystem = .TRUE.
        CALL SetupEMSActuator('Water System', UserAirTerminal(CompLoop)%Name, &
                                          'Collected Volume Flow Rate', '[m3/s]', lDummy, &
                                          UserAirTerminal(CompLoop)%Water%CollectedVdot )
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(14) ) THEN

        UserAirTerminal(CompLoop)%Zone%ZoneNum = FindItemInList(cAlphaArgs(14),Zone%Name,NumOfZones)
        IF (UserZoneAirHVAC(CompLoop)%Zone%ZoneNum == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Ambient Zone Name not found = '//TRIM(cAlphaArgs(16)))
          ErrorsFound = .TRUE.
        ELSE
          UserAirTerminal(CompLoop)%Zone%DeviceHasInternalGains = .TRUE.
          CALL SetupZoneInternalGain(UserAirTerminal(CompLoop)%Zone%ZoneNum, &
                                     TRIM(cCurrentModuleObject), &
                                     TRIM(cAlphaArgs(1)), &
                                     IntGainTypeOf_AirTerminalUserDefined, &
                                     ConvectionGainRate          = UserAirTerminal(CompLoop)%Zone%ConvectionGainRate, &
                                     ReturnAirConvectionGainRate = UserAirTerminal(CompLoop)%Zone%ReturnAirConvectionGainRate, &
                                     ThermalRadiationGainRate    = UserAirTerminal(CompLoop)%Zone%ThermalRadiationGainRate, &
                                     LatentGainRate              = UserAirTerminal(CompLoop)%Zone%LatentGainRate, &
                                     ReturnAirLatentGainRate     = UserAirTerminal(CompLoop)%Zone%ReturnAirLatentGainRate, &
                                     CarbonDioxideGainRate       = UserAirTerminal(CompLoop)%Zone%CarbonDioxideGainRate, &
                                     GenericContamGainRate       = UserAirTerminal(CompLoop)%Zone%GenericContamGainRate )

          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Sensible Heat Gain Rate', '[W]', lDummy, &
                                 UserAirTerminal(CompLoop)%Zone%ConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Return Air Heat Sensible Gain Rate', '[W]', lDummy, &
                                 UserZoneAirHVAC(CompLoop)%Zone%ReturnAirConvectionGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Thermal Radiation Heat Gain Rate', '[W]', lDummy, &
                                 UserAirTerminal(CompLoop)%Zone%ThermalRadiationGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserAirTerminal(CompLoop)%Zone%LatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Return Air Latent Heat Gain Rate', '[W]', lDummy, &
                                 UserAirTerminal(CompLoop)%Zone%ReturnAirLatentGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Carbon Dioxide Gain Rate', '[W]', lDummy, &
                                 UserAirTerminal(CompLoop)%Zone%CarbonDioxideGainRate )
          CALL SetupEMSActuator('Component Zone Internal Gain', UserAirTerminal(CompLoop)%Name, &
                                'Gaseous Contaminant Gain Rate', '[W]', lDummy, &
                                 UserAirTerminal(CompLoop)%Zone%GenericContamGainRate )
        ENDIF
      ENDIF

    ENDDO
  ENDIF !NumUserZoneAir > 0

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetUserDefinedComponents: Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
  ENDIF
  RETURN

END SUBROUTINE GetUserDefinedComponents


SUBROUTINE InitPlantUserComponent(CompNum, LoopNum, MyLoad)
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
  USE PlantUtilities,     ONLY: InitComponentNodes
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE Psychrometrics  ,   ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataEnvironment ,   ONLY: OutBaroPress

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum
  INTEGER, INTENT(IN) :: LoopNum
  REAL(r64), INTENT(IN) ::MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag            ! environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  INTEGER  :: ConnectionNum
  LOGICAL  :: errFlag
!  REAL(r64) :: rho
!  REAL(r64) :: Cp

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumUserPlantComps))
    ALLOCATE(MyEnvrnFlag(NumUserPlantComps))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyFlag(CompNum)) THEN
    ! locate the connections to the plant loops
    DO ConnectionNum = 1, UserPlantComp(CompNum)%NumPlantConnections
      errFlag = .false.
      CALL ScanPlantLoopsForObject(UserPlantComp(CompNum)%Name,    &
                                 Typeof_PlantComponentUserDefined, &
                                 UserPlantComp(CompNum)%Loop(ConnectionNum)%LoopNum, &
                                 UserPlantComp(CompNum)%Loop(ConnectionNum)%LoopSideNum, &
                                 UserPlantComp(CompNum)%Loop(ConnectionNum)%BranchNum, &
                                 UserPlantComp(CompNum)%Loop(ConnectionNum)%CompNum, &
                                 InletNodeNumber = UserPlantComp(CompNum)%Loop(ConnectionNum)%InletNodeNum )
      IF (errFlag) THEN
        CALL ShowFatalError('InitPlantUserComponent: Program terminated due to previous condition(s).')
      ENDIF

      !set user input for flow priority
      PlantLoop(UserPlantComp(CompNum)%Loop(ConnectionNum)%LoopNum)%  &
        LoopSide(UserPlantComp(CompNum)%Loop(ConnectionNum)%LoopSideNum)% &
          Branch(UserPlantComp(CompNum)%Loop(ConnectionNum)%BranchNum)%    &
           Comp(UserPlantComp(CompNum)%Loop(ConnectionNum)%CompNum)%FlowPriority &
             = UserPlantComp(CompNum)%Loop(ConnectionNum)%FlowPriority

      ! set user input for how loads served
      PlantLoop(UserPlantComp(CompNum)%Loop(ConnectionNum)%LoopNum)%  &
        LoopSide(UserPlantComp(CompNum)%Loop(ConnectionNum)%LoopSideNum)% &
          Branch(UserPlantComp(CompNum)%Loop(ConnectionNum)%BranchNum)%    &
           Comp(UserPlantComp(CompNum)%Loop(ConnectionNum)%CompNum)%HowLoadServed &
             = UserPlantComp(CompNum)%Loop(ConnectionNum)%HowLoadServed

    ENDDO

    MyFlag(CompNum) = .FALSE.
  ENDIF


  IF (LoopNum <= 0 .OR.  LoopNum > UserPlantComp(CompNum)%NumPlantConnections) RETURN

  ! fill internal variable targets
  UserPlantComp(CompNum)%Loop(LoopNum)%MyLoad = MyLoad
  UserPlantComp(CompNum)%Loop(LoopNum)%InletRho = GetDensityGlycol(  &
                           PlantLoop(UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum)%FluidName,  &
                           Node(UserPlantComp(CompNum)%Loop(LoopNum)%InletNodeNum)%Temp,       &
                           PlantLoop(UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum)%FluidIndex, &
                           'InitPlantUserComponent')
  UserPlantComp(CompNum)%Loop(LoopNum)%InletCp = GetSpecificHeatGlycol(  &
                           PlantLoop(UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum)%FluidName,  &
                           Node(UserPlantComp(CompNum)%Loop(LoopNum)%InletNodeNum)%Temp,       &
                           PlantLoop(UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum)%FluidIndex, &
                           'InitPlantUserComponent')
  UserPlantComp(CompNum)%Loop(LoopNum)%InletMassFlowRate = &
                           Node(UserPlantComp(CompNum)%Loop(LoopNum)%InletNodeNum)%MassFlowRate
  UserPlantComp(CompNum)%Loop(LoopNum)%InletTemp = &
                           Node(UserPlantComp(CompNum)%Loop(LoopNum)%InletNodeNum)%Temp
  IF (UserPlantComp(CompNum)%Air%InletNodeNum > 0) THEN
    UserPlantComp(CompNum)%Air%InletRho = PsyRhoAirFnPbTdbW(OutBaroPress, &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%Temp, &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%HumRat, &
                        'InitPlantUserComponent')
    UserPlantComp(CompNum)%Air%InletCp  = PsyCpAirFnWTdb( &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%HumRat, &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%Temp , &
                        'InitPlantUserComponent')
    UserPlantComp(CompNum)%Air%InletTemp = &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%Temp
    UserPlantComp(CompNum)%Air%InletMassFlowRate = &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%MassFlowRate
    UserPlantComp(CompNum)%Air%InletHumRat = &
                        Node(UserPlantComp(CompNum)%Air%InletNodeNum)%HumRat
  ENDIF

  RETURN

END SUBROUTINE InitPlantUserComponent

SUBROUTINE InitCoilUserDefined(CompNum)

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
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataEnvironment,    ONLY: OutBaroPress

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL  :: errFlag
  INTEGER :: Loop

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumUserCoils))
    MyFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyFlag(CompNum)) THEN
    IF (UserCoil(CompNum)%PlantIsConnected) THEN
      errFlag = .false.
      CALL ScanPlantLoopsForObject(UserCoil(CompNum)%Name,    &
                                 Typeof_CoilUserDefined, &
                                 UserCoil(CompNum)%Loop%LoopNum, &
                                 UserCoil(CompNum)%Loop%LoopSideNum, &
                                 UserCoil(CompNum)%Loop%BranchNum, &
                                 UserCoil(CompNum)%Loop%CompNum )
      IF (errFlag) THEN
        CALL ShowFatalError('InitPlantUserComponent: Program terminated due to previous condition(s).')
      ENDIF
      !set user input for flow priority
      PlantLoop(UserCoil(CompNum)%Loop%LoopNum)%  &
        LoopSide(UserCoil(CompNum)%Loop%LoopSideNum)% &
          Branch(UserCoil(CompNum)%Loop%BranchNum)%    &
           Comp(UserCoil(CompNum)%Loop%CompNum)%FlowPriority &
             = UserCoil(CompNum)%Loop%FlowPriority

      ! set user input for how loads served
      PlantLoop(UserCoil(CompNum)%Loop%LoopNum)%  &
        LoopSide(UserCoil(CompNum)%Loop%LoopSideNum)% &
          Branch(UserCoil(CompNum)%Loop%BranchNum)%    &
           Comp(UserCoil(CompNum)%Loop%CompNum)%HowLoadServed &
             = UserCoil(CompNum)%Loop%HowLoadServed

    ENDIF
    MyFlag(CompNum) = .FALSE.
  ENDIF

  ! fill internal variable targets
  DO loop = 1, UserCoil(CompNum)%NumAirConnections
    UserCoil(CompNum)%Air(loop)%InletRho = PsyRhoAirFnPbTdbW(OutBaroPress, &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%Temp, &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%HumRat, &
                              'InitCoilUserDefined')

    UserCoil(CompNum)%Air(loop)%InletCp  = PsyCpAirFnWTdb( &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%HumRat, &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%Temp , &
                              'InitCoilUserDefined')
    UserCoil(CompNum)%Air(loop)%InletTemp = &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%Temp
    UserCoil(CompNum)%Air(loop)%InletMassFlowRate = &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%MassFlowRate
    UserCoil(CompNum)%Air(loop)%InletHumRat = &
                              Node(UserCoil(CompNum)%Air(loop)%InletNodeNum)%HumRat
  ENDDO

  IF (UserCoil(CompNum)%PlantIsConnected) THEN
    UserCoil(CompNum)%Loop%InletRho = GetDensityGlycol(  &
                           PlantLoop(UserCoil(CompNum)%Loop%LoopNum)%FluidName,  &
                           Node(UserCoil(CompNum)%Loop%InletNodeNum)%Temp,       &
                           PlantLoop(UserCoil(CompNum)%Loop%LoopNum)%FluidIndex, &
                           'InitCoilUserDefined')
    UserCoil(CompNum)%Loop%InletCp = GetSpecificHeatGlycol(  &
                           PlantLoop(UserCoil(CompNum)%Loop%LoopNum)%FluidName,  &
                           Node(UserCoil(CompNum)%Loop%InletNodeNum)%Temp,       &
                           PlantLoop(UserCoil(CompNum)%Loop%LoopNum)%FluidIndex, &
                           'InitCoilUserDefined')
    UserCoil(CompNum)%Loop%InletTemp = &
                           Node(UserCoil(CompNum)%Loop%InletNodeNum)%Temp
    UserCoil(CompNum)%Loop%InletMassFlowRate = &
                           Node(UserCoil(CompNum)%Loop%InletNodeNum)%MassFlowRate
  ENDIF

  RETURN

END SUBROUTINE InitCoilUserDefined

SUBROUTINE InitZoneAirUserDefined(CompNum, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Feb. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! initialize data for user-defined zone HVAC forced air component model

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataEnvironment,    ONLY: OutBaroPress
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, ZoneSysMoistureDemand

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum
  INTEGER, INTENT(IN) :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL  :: errFlag
  INTEGER :: Loop


  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumUserZoneAir))
    MyFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyFlag(CompNum)) THEN
    IF (UserZoneAirHVAC(CompNum)%NumPlantConnections > 0) THEN
      DO Loop = 1, UserZoneAirHVAC(CompNum)%NumPlantConnections
        errFlag = .false.
        CALL ScanPlantLoopsForObject(UserZoneAirHVAC(CompNum)%Name,    &
                                   TypeOf_ZoneHVACAirUserDefined, &
                                   UserZoneAirHVAC(CompNum)%Loop(loop)%LoopNum, &
                                   UserZoneAirHVAC(CompNum)%Loop(loop)%LoopSideNum, &
                                   UserZoneAirHVAC(CompNum)%Loop(loop)%BranchNum, &
                                   UserZoneAirHVAC(CompNum)%Loop(loop)%CompNum , &
                                   InletNodeNumber = UserZoneAirHVAC(CompNum)%Loop(loop)%InletNodeNum)
        IF (errFlag) THEN
          CALL ShowFatalError('InitPlantUserComponent: Program terminated due to previous condition(s).')
        ENDIF
        !set user input for flow priority
        PlantLoop(UserZoneAirHVAC(CompNum)%Loop(loop)%LoopNum)%  &
          LoopSide(UserZoneAirHVAC(CompNum)%Loop(loop)%LoopSideNum)% &
            Branch(UserZoneAirHVAC(CompNum)%Loop(loop)%BranchNum)%    &
             Comp(UserZoneAirHVAC(CompNum)%Loop(loop)%CompNum)%FlowPriority &
               = UserZoneAirHVAC(CompNum)%Loop(loop)%FlowPriority

        ! set user input for how loads served
        PlantLoop(UserZoneAirHVAC(CompNum)%Loop(loop)%LoopNum)%  &
          LoopSide(UserZoneAirHVAC(CompNum)%Loop(loop)%LoopSideNum)% &
            Branch(UserZoneAirHVAC(CompNum)%Loop(loop)%BranchNum)%    &
             Comp(UserZoneAirHVAC(CompNum)%Loop(loop)%CompNum)%HowLoadServed &
               = UserZoneAirHVAC(CompNum)%Loop(loop)%HowLoadServed
      ENDDO

    ENDIF
  ENDIF
  ! fill internal variable targets
  UserZoneAirHVAC(CompNum)%RemainingOutputToHeatingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
  UserZoneAirHVAC(CompNum)%RemainingOutputToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
  UserZoneAirHVAC(CompNum)%RemainingOutputReqToDehumidSP = ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToDehumidSP
  UserZoneAirHVAC(CompNum)%RemainingOutputReqToHumidSP = ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToHumidSP

  UserZoneAirHVAC(CompNum)%ZoneAir%InletRho = PsyRhoAirFnPbTdbW(OutBaroPress, &
                            Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%Temp, &
                            Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%HumRat, &
                            'InitZoneAirUserDefined' )
  UserZoneAirHVAC(CompNum)%ZoneAir%InletCp  = PsyCpAirFnWTdb( &
                            Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%HumRat, &
                            Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%Temp , &
                            'InitZoneAirUserDefined')
  UserZoneAirHVAC(CompNum)%ZoneAir%InletTemp = &
                            Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%Temp
  UserZoneAirHVAC(CompNum)%ZoneAir%InletHumRat = &
                            Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%HumRat

  IF (UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum > 0) THEN
    UserZoneAirHVAC(CompNum)%SourceAir%InletRho = PsyRhoAirFnPbTdbW(OutBaroPress, &
                              Node(UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum)%Temp, &
                              Node(UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum)%HumRat, &
                              'InitZoneAirUserDefined')
    UserZoneAirHVAC(CompNum)%SourceAir%InletCp  = PsyCpAirFnWTdb( &
                              Node(UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum)%HumRat, &
                              Node(UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum)%Temp, &
                              'InitZoneAirUserDefined' )
    UserZoneAirHVAC(CompNum)%SourceAir%InletTemp = &
                              Node(UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum)%Temp
    UserZoneAirHVAC(CompNum)%SourceAir%InletHumRat = &
                              Node(UserZoneAirHVAC(CompNum)%SourceAir%InletNodeNum)%HumRat
  ENDIF

  IF (UserZoneAirHVAC(CompNum)%NumPlantConnections > 0) THEN
    Do Loop = 1, UserZoneAirHVAC(CompNum)%NumPlantConnections
      UserZoneAirHVAC(CompNum)%Loop(Loop)%InletRho = GetDensityGlycol(  &
                             PlantLoop(UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopNum)%FluidName,  &
                             Node(UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum)%Temp,       &
                             PlantLoop(UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopNum)%FluidIndex, &
                             'InitZoneAirUserDefined')
      UserZoneAirHVAC(CompNum)%Loop(Loop)%InletCp = GetSpecificHeatGlycol(  &
                             PlantLoop(UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopNum)%FluidName,  &
                             Node(UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum)%Temp,       &
                             PlantLoop(UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopNum)%FluidIndex, &
                             'InitZoneAirUserDefined')
      UserZoneAirHVAC(CompNum)%Loop(Loop)%InletTemp = &
                             Node(UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum)%Temp
      UserZoneAirHVAC(CompNum)%Loop(Loop)%InletMassFlowRate = &
                             Node(UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum)%MassFlowRate
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE InitZoneAirUserDefined

SUBROUTINE InitAirTerminalUserDefined(CompNum, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataEnvironment,    ONLY: OutBaroPress
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, ZoneSysMoistureDemand

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum
  INTEGER, INTENT(IN) :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL  :: errFlag
  INTEGER :: Loop

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumUserAirTerminals))
    MyFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyFlag(CompNum)) THEN
    IF (UserAirTerminal(CompNum)%NumPlantConnections > 0) THEN
      DO Loop = 1, UserAirTerminal(CompNum)%NumPlantConnections
        errFlag = .false.
        CALL ScanPlantLoopsForObject(UserAirTerminal(CompNum)%Name,    &
                                   TypeOf_AirTerminalUserDefined, &
                                   UserAirTerminal(CompNum)%Loop(loop)%LoopNum, &
                                   UserAirTerminal(CompNum)%Loop(loop)%LoopSideNum, &
                                   UserAirTerminal(CompNum)%Loop(loop)%BranchNum, &
                                   UserAirTerminal(CompNum)%Loop(loop)%CompNum , &
                                   InletNodeNumber = UserAirTerminal(CompNum)%Loop(loop)%InletNodeNum)
        IF (errFlag) THEN
          CALL ShowFatalError('InitPlantUserComponent: Program terminated due to previous condition(s).')
        ENDIF
        !set user input for flow priority
        PlantLoop(UserAirTerminal(CompNum)%Loop(loop)%LoopNum)%  &
          LoopSide(UserAirTerminal(CompNum)%Loop(loop)%LoopSideNum)% &
            Branch(UserAirTerminal(CompNum)%Loop(loop)%BranchNum)%    &
             Comp(UserAirTerminal(CompNum)%Loop(loop)%CompNum)%FlowPriority &
               = UserAirTerminal(CompNum)%Loop(loop)%FlowPriority

        ! set user input for how loads served
        PlantLoop(UserAirTerminal(CompNum)%Loop(loop)%LoopNum)%  &
          LoopSide(UserAirTerminal(CompNum)%Loop(loop)%LoopSideNum)% &
            Branch(UserAirTerminal(CompNum)%Loop(loop)%BranchNum)%    &
             Comp(UserAirTerminal(CompNum)%Loop(loop)%CompNum)%HowLoadServed &
               = UserAirTerminal(CompNum)%Loop(loop)%HowLoadServed
      ENDDO

    ENDIF
  ENDIF
  ! fill internal variable targets
  UserAirTerminal(CompNum)%RemainingOutputToHeatingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
  UserAirTerminal(CompNum)%RemainingOutputToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
  UserAirTerminal(CompNum)%RemainingOutputReqToDehumidSP = ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToDehumidSP
  UserAirTerminal(CompNum)%RemainingOutputReqToHumidSP = ZoneSysMoistureDemand(ZoneNum)%RemainingOutputReqToHumidSP

  UserAirTerminal(CompNum)%AirLoop%InletRho = PsyRhoAirFnPbTdbW(OutBaroPress, &
                            Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%Temp, &
                            Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%HumRat, &
                            'InitAirTerminalUserDefined' )
  UserAirTerminal(CompNum)%AirLoop%InletCp  = PsyCpAirFnWTdb( &
                            Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%HumRat, &
                            Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%Temp , &
                            'InitAirTerminalUserDefined')
  UserAirTerminal(CompNum)%AirLoop%InletTemp = &
                            Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%Temp
  UserAirTerminal(CompNum)%AirLoop%InletHumRat = &
                            Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%HumRat

  IF (UserAirTerminal(CompNum)%SourceAir%InletNodeNum > 0) THEN
    UserAirTerminal(CompNum)%SourceAir%InletRho = PsyRhoAirFnPbTdbW(OutBaroPress, &
                              Node(UserAirTerminal(CompNum)%SourceAir%InletNodeNum)%Temp, &
                              Node(UserAirTerminal(CompNum)%SourceAir%InletNodeNum)%HumRat, &
                              'InitAirTerminalUserDefined')
    UserAirTerminal(CompNum)%SourceAir%InletCp  = PsyCpAirFnWTdb( &
                              Node(UserAirTerminal(CompNum)%SourceAir%InletNodeNum)%HumRat, &
                              Node(UserAirTerminal(CompNum)%SourceAir%InletNodeNum)%Temp, &
                              'InitAirTerminalUserDefined' )
    UserAirTerminal(CompNum)%SourceAir%InletTemp = &
                              Node(UserAirTerminal(CompNum)%SourceAir%InletNodeNum)%Temp
    UserAirTerminal(CompNum)%SourceAir%InletHumRat = &
                              Node(UserAirTerminal(CompNum)%SourceAir%InletNodeNum)%HumRat
  ENDIF

  IF (UserAirTerminal(CompNum)%NumPlantConnections > 0) THEN
    Do Loop = 1, UserAirTerminal(CompNum)%NumPlantConnections
      UserAirTerminal(CompNum)%Loop(Loop)%InletRho = GetDensityGlycol(  &
                             PlantLoop(UserAirTerminal(CompNum)%Loop(Loop)%LoopNum)%FluidName,  &
                             Node(UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum)%Temp,       &
                             PlantLoop(UserAirTerminal(CompNum)%Loop(Loop)%LoopNum)%FluidIndex, &
                             'InitAirTerminalUserDefined')
      UserAirTerminal(CompNum)%Loop(Loop)%InletCp = GetSpecificHeatGlycol(  &
                             PlantLoop(UserAirTerminal(CompNum)%Loop(Loop)%LoopNum)%FluidName,  &
                             Node(UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum)%Temp,       &
                             PlantLoop(UserAirTerminal(CompNum)%Loop(Loop)%LoopNum)%FluidIndex, &
                             'InitAirTerminalUserDefined')
      UserAirTerminal(CompNum)%Loop(Loop)%InletTemp = &
                             Node(UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum)%Temp
      UserAirTerminal(CompNum)%Loop(Loop)%InletMassFlowRate = &
                             Node(UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum)%MassFlowRate
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE InitAirTerminalUserDefined


SUBROUTINE ReportPlantUserComponent(CompNum, LoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! report model results

          ! METHODOLOGY EMPLOYED:
          ! copy actuated values to structures elsewhere in program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SetComponentFlowRate, SafeCopyPlantNode
  USE Psychrometrics, ONLY: PsyHFnTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum
  INTEGER, INTENT(IN) :: LoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  CALL SafeCopyPlantNode(UserPlantComp(CompNum)%Loop(LoopNum)%InletNodeNum, &
                         UserPlantComp(CompNum)%Loop(LoopNum)%OutletNodeNum)

  !unload Actuators to node data structure

  Node(UserPlantComp(CompNum)%Loop(LoopNum)%OutletNodeNum)%Temp = &
                         UserPlantComp(CompNum)%Loop(LoopNum)%OutletTemp

  !make mass flow requests, just this loop
  CALL SetComponentFlowRate(  UserPlantComp(CompNum)%Loop(LoopNum)%MassFlowRateRequest, &
                              UserPlantComp(CompNum)%Loop(LoopNum)%InletNodeNum, &
                              UserPlantComp(CompNum)%Loop(LoopNum)%OutletNodeNum, &
                              UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum, &
                              UserPlantComp(CompNum)%Loop(LoopNum)%LoopSideNum, &
                              UserPlantComp(CompNum)%Loop(LoopNum)%BranchNum, &
                              UserPlantComp(CompNum)%Loop(LoopNum)%CompNum )

  IF (UserPlantComp(CompNum)%Air%OutletNodeNum > 0) THEN
    Node(UserPlantComp(CompNum)%Air%OutletNodeNum)%Temp         = UserPlantComp(CompNum)%Air%OutletTemp
    Node(UserPlantComp(CompNum)%Air%OutletNodeNum)%HumRat       = UserPlantComp(CompNum)%Air%OutletHumRat
    Node(UserPlantComp(CompNum)%Air%OutletNodeNum)%MassFlowRate = UserPlantComp(CompNum)%Air%OutletMassFlowRate
    Node(UserPlantComp(CompNum)%Air%OutletNodeNum)%Enthalpy     = PsyHFnTdbW(UserPlantComp(CompNum)%Air%OutletTemp, &
                                                                             UserPlantComp(CompNum)%Air%OutletHumRat, &
                                                                             'ReportPlantUserComponent')
  ENDIF

  IF (UserPlantComp(CompNum)%Water%SuppliedByWaterSystem) THEN
    WaterStorage(UserPlantComp(CompNum)%Water%SupplyTankID)% &
      VdotRequestDemand(UserPlantComp(CompNum)%Water%SupplyTankDemandARRID) &
        = UserPlantComp(CompNum)%Water%SupplyVdotRequest
  ENDIF

  IF (UserPlantComp(CompNum)%Water%CollectsToWaterSystem) THEN
    WaterStorage(UserPlantComp(CompNum)%Water%CollectionTankID)% &
      VdotAvailSupply(UserPlantComp(CompNum)%Water%CollectionTankSupplyARRID) &
        =  UserPlantComp(CompNum)%Water%CollectedVdot
  ENDIF

  IF (UserPlantComp(CompNum)%Loop(LoopNum)%HowLoadServed == HowMet_ByNominalCapLowOutLimit) THEN
    PlantLoop(UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum)%  &
        LoopSide(UserPlantComp(CompNum)%Loop(LoopNum)%LoopSideNum)% &
          Branch(UserPlantComp(CompNum)%Loop(LoopNum)%BranchNum)%    &
           Comp(UserPlantComp(CompNum)%Loop(LoopNum)%CompNum)%MinOutletTemp &
             = UserPlantComp(CompNum)%Loop(LoopNum)%LowOutTempLimit
  ENDIF

  IF (UserPlantComp(CompNum)%Loop(LoopNum)%HowLoadServed == HowMet_ByNominalCapHiOutLimit) THEN
    PlantLoop(UserPlantComp(CompNum)%Loop(LoopNum)%LoopNum)%  &
        LoopSide(UserPlantComp(CompNum)%Loop(LoopNum)%LoopSideNum)% &
          Branch(UserPlantComp(CompNum)%Loop(LoopNum)%BranchNum)%    &
           Comp(UserPlantComp(CompNum)%Loop(LoopNum)%CompNum)%MaxOutletTemp &
             = UserPlantComp(CompNum)%Loop(LoopNum)%HiOutTempLimit
  ENDIF

  RETURN

END SUBROUTINE ReportPlantUserComponent

SUBROUTINE ReportCoilUserDefined(CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! report model outputs

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SetComponentFlowRate, SafeCopyPlantNode
  USE Psychrometrics, ONLY: PsyHFnTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop

  DO Loop = 1, UserCoil(CompNum)%NumAirConnections
    IF (UserCoil(CompNum)%Air(Loop)%OutletNodeNum > 0) THEN
      Node(UserCoil(CompNum)%Air(Loop)%OutletNodeNum)%Temp         = UserCoil(CompNum)%Air(Loop)%OutletTemp
      Node(UserCoil(CompNum)%Air(Loop)%OutletNodeNum)%HumRat       = UserCoil(CompNum)%Air(Loop)%OutletHumRat
      Node(UserCoil(CompNum)%Air(Loop)%OutletNodeNum)%MassFlowRate = UserCoil(CompNum)%Air(Loop)%OutletMassFlowRate
      Node(UserCoil(CompNum)%Air(Loop)%OutletNodeNum)%Enthalpy     = PsyHFnTdbW(UserCoil(CompNum)%Air(Loop)%OutletTemp, &
                                                                                UserCoil(CompNum)%Air(Loop)%OutletHumRat, &
                                                                                'ReportCoilUserDefined')
    ENDIF
  ENDDO

  IF (UserCoil(CompNum)%PlantIsConnected) THEN
    !make mass flow requests
    CALL SetComponentFlowRate(UserCoil(CompNum)%Loop%MassFlowRateRequest, &
                              UserCoil(CompNum)%Loop%InletNodeNum, &
                              UserCoil(CompNum)%Loop%OutletNodeNum, &
                              UserCoil(CompNum)%Loop%LoopNum, &
                              UserCoil(CompNum)%Loop%LoopSideNum, &
                              UserCoil(CompNum)%Loop%BranchNum, &
                              UserCoil(CompNum)%Loop%CompNum )
    CALL SafeCopyPlantNode(UserCoil(CompNum)%Loop%InletNodeNum, &
                           UserCoil(CompNum)%Loop%OutletNodeNum)
    !unload Actuators to node data structure
    Node(UserCoil(CompNum)%Loop%OutletNodeNum)%Temp =  UserCoil(CompNum)%Loop%OutletTemp
  ENDIF

  IF (UserCoil(CompNum)%Water%SuppliedByWaterSystem) THEN
    WaterStorage(UserCoil(CompNum)%Water%SupplyTankID)% &
      VdotRequestDemand(UserCoil(CompNum)%Water%SupplyTankDemandARRID) &
        = UserCoil(CompNum)%Water%SupplyVdotRequest
  ENDIF

  IF (UserCoil(CompNum)%Water%CollectsToWaterSystem) THEN
    WaterStorage(UserCoil(CompNum)%Water%CollectionTankID)% &
      VdotAvailSupply(UserCoil(CompNum)%Water%CollectionTankSupplyARRID) &
        =  UserCoil(CompNum)%Water%CollectedVdot
  ENDIF

  RETURN

END SUBROUTINE ReportCoilUserDefined

SUBROUTINE ReportZoneAirUserDefined(CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! report model outputs

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SetComponentFlowRate, SafeCopyPlantNode
  USE Psychrometrics, ONLY: PsyHFnTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop

  Node(UserZoneAirHVAC(CompNum)%ZoneAir%InletNodeNum)%MassFlowRate  = UserZoneAirHVAC(CompNum)%ZoneAir%InletMassFlowRate

  Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%Temp         = UserZoneAirHVAC(CompNum)%ZoneAir%OutletTemp
  Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%HumRat       = UserZoneAirHVAC(CompNum)%ZoneAir%OutletHumRat
  Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%MassFlowRate = UserZoneAirHVAC(CompNum)%ZoneAir%OutletMassFlowRate
  Node(UserZoneAirHVAC(CompNum)%ZoneAir%OutletNodeNum)%Enthalpy     = PsyHFnTdbW(UserZoneAirHVAC(CompNum)%ZoneAir%OutletTemp, &
                                                                            UserZoneAirHVAC(CompNum)%ZoneAir%OutletHumRat, &
                                                                            'ReportZoneAirUserDefined')
  IF (UserZoneAirHVAC(CompNum)%SourceAir%OutletNodeNum > 0) THEN
    Node(UserZoneAirHVAC(CompNum)%SourceAir%OutletNodeNum)%Temp         = UserZoneAirHVAC(CompNum)%SourceAir%OutletTemp
    Node(UserZoneAirHVAC(CompNum)%SourceAir%OutletNodeNum)%HumRat       = UserZoneAirHVAC(CompNum)%SourceAir%OutletHumRat
    Node(UserZoneAirHVAC(CompNum)%SourceAir%OutletNodeNum)%MassFlowRate = UserZoneAirHVAC(CompNum)%SourceAir%OutletMassFlowRate
    Node(UserZoneAirHVAC(CompNum)%SourceAir%OutletNodeNum)%Enthalpy    = PsyHFnTdbW(UserZoneAirHVAC(CompNum)%SourceAir%OutletTemp,&
                                                                              UserZoneAirHVAC(CompNum)%SourceAir%OutletHumRat, &
                                                                              'ReportZoneAirUserDefined')
  ENDIF

  IF (UserZoneAirHVAC(CompNum)%NumPlantConnections > 0) THEN
    DO Loop = 1, UserZoneAirHVAC(CompNum)%NumPlantConnections
      !make mass flow requests
      CALL SetComponentFlowRate(UserZoneAirHVAC(CompNum)%Loop(Loop)%MassFlowRateRequest, &
                                UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum, &
                                UserZoneAirHVAC(CompNum)%Loop(Loop)%OutletNodeNum, &
                                UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopNum, &
                                UserZoneAirHVAC(CompNum)%Loop(Loop)%LoopSideNum, &
                                UserZoneAirHVAC(CompNum)%Loop(Loop)%BranchNum, &
                                UserZoneAirHVAC(CompNum)%Loop(Loop)%CompNum )
      CALL SafeCopyPlantNode(UserZoneAirHVAC(CompNum)%Loop(Loop)%InletNodeNum, &
                             UserZoneAirHVAC(CompNum)%Loop(Loop)%OutletNodeNum)
      !unload Actuators to node data structure
      Node(UserZoneAirHVAC(CompNum)%Loop(Loop)%OutletNodeNum)%Temp =  UserZoneAirHVAC(CompNum)%Loop(Loop)%OutletTemp
    ENDDO
  ENDIF

  IF (UserZoneAirHVAC(CompNum)%Water%SuppliedByWaterSystem) THEN
    WaterStorage(UserZoneAirHVAC(CompNum)%Water%SupplyTankID)% &
      VdotRequestDemand(UserZoneAirHVAC(CompNum)%Water%SupplyTankDemandARRID) &
        = UserZoneAirHVAC(CompNum)%Water%SupplyVdotRequest
  ENDIF

  IF (UserZoneAirHVAC(CompNum)%Water%CollectsToWaterSystem) THEN
    WaterStorage(UserZoneAirHVAC(CompNum)%Water%CollectionTankID)% &
      VdotAvailSupply(UserZoneAirHVAC(CompNum)%Water%CollectionTankSupplyARRID) &
        =  UserZoneAirHVAC(CompNum)%Water%CollectedVdot
  ENDIF

  RETURN

END SUBROUTINE ReportZoneAirUserDefined

SUBROUTINE ReportAirTerminalUserDefined(CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SetComponentFlowRate, SafeCopyPlantNode
  USE Psychrometrics, ONLY: PsyHFnTdbW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  INTEGER :: Loop

  Node(UserAirTerminal(CompNum)%AirLoop%InletNodeNum)%MassFlowRate  = UserAirTerminal(CompNum)%AirLoop%InletMassFlowRate

  Node(UserAirTerminal(CompNum)%AirLoop%OutletNodeNum)%Temp         = UserAirTerminal(CompNum)%AirLoop%OutletTemp
  Node(UserAirTerminal(CompNum)%AirLoop%OutletNodeNum)%HumRat       = UserAirTerminal(CompNum)%AirLoop%OutletHumRat
  Node(UserAirTerminal(CompNum)%AirLoop%OutletNodeNum)%MassFlowRate = UserAirTerminal(CompNum)%AirLoop%OutletMassFlowRate
  Node(UserAirTerminal(CompNum)%AirLoop%OutletNodeNum)%Enthalpy     = PsyHFnTdbW(UserAirTerminal(CompNum)%AirLoop%OutletTemp, &
                                                                            UserAirTerminal(CompNum)%AirLoop%OutletHumRat, &
                                                                            'ReportAirTerminalUserDefined')
  IF (UserAirTerminal(CompNum)%SourceAir%OutletNodeNum > 0) THEN
    Node(UserAirTerminal(CompNum)%SourceAir%OutletNodeNum)%Temp         = UserAirTerminal(CompNum)%SourceAir%OutletTemp
    Node(UserAirTerminal(CompNum)%SourceAir%OutletNodeNum)%HumRat       = UserAirTerminal(CompNum)%SourceAir%OutletHumRat
    Node(UserAirTerminal(CompNum)%SourceAir%OutletNodeNum)%MassFlowRate = UserAirTerminal(CompNum)%SourceAir%OutletMassFlowRate
    Node(UserAirTerminal(CompNum)%SourceAir%OutletNodeNum)%Enthalpy    = PsyHFnTdbW(UserAirTerminal(CompNum)%SourceAir%OutletTemp,&
                                                                              UserAirTerminal(CompNum)%SourceAir%OutletHumRat, &
                                                                              'ReportAirTerminalUserDefined')
  ENDIF

  IF (UserAirTerminal(CompNum)%NumPlantConnections > 0) THEN
    DO Loop = 1, UserAirTerminal(CompNum)%NumPlantConnections
      !make mass flow requests
      CALL SetComponentFlowRate(UserAirTerminal(CompNum)%Loop(Loop)%MassFlowRateRequest, &
                                UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum, &
                                UserAirTerminal(CompNum)%Loop(Loop)%OutletNodeNum, &
                                UserAirTerminal(CompNum)%Loop(Loop)%LoopNum, &
                                UserAirTerminal(CompNum)%Loop(Loop)%LoopSideNum, &
                                UserAirTerminal(CompNum)%Loop(Loop)%BranchNum, &
                                UserAirTerminal(CompNum)%Loop(Loop)%CompNum )
      CALL SafeCopyPlantNode(UserAirTerminal(CompNum)%Loop(Loop)%InletNodeNum, &
                             UserAirTerminal(CompNum)%Loop(Loop)%OutletNodeNum)
      !unload Actuators to node data structure
      Node(UserAirTerminal(CompNum)%Loop(Loop)%OutletNodeNum)%Temp =  UserAirTerminal(CompNum)%Loop(Loop)%OutletTemp
    ENDDO
  ENDIF

  IF (UserAirTerminal(CompNum)%Water%SuppliedByWaterSystem) THEN
    WaterStorage(UserAirTerminal(CompNum)%Water%SupplyTankID)% &
      VdotRequestDemand(UserAirTerminal(CompNum)%Water%SupplyTankDemandARRID) &
        = UserAirTerminal(CompNum)%Water%SupplyVdotRequest
  ENDIF

  IF (UserAirTerminal(CompNum)%Water%CollectsToWaterSystem) THEN
    WaterStorage(UserAirTerminal(CompNum)%Water%CollectionTankID)% &
      VdotAvailSupply(UserAirTerminal(CompNum)%Water%CollectionTankSupplyARRID) &
        =  UserAirTerminal(CompNum)%Water%CollectedVdot
  ENDIF

  RETURN

END SUBROUTINE ReportAirTerminalUserDefined

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

END MODULE UserDefinedComponents

