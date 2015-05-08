MODULE PlantLoopEquip

          ! MODULE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contains subroutine that calls the required component for simulation. The components are selected
          ! using a CASE statement.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES: none

          ! OTHER NOTES: none

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals,           ONLY : MaxNameLength
USE DataInterfaces,        ONLY : ShowSevereError, ShowWarningError,ShowContinueError,ShowFatalError
USE DataPlant
USE DataLoopNode,          ONLY : Node

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! SUBROUTINE SPECIFICATION
PUBLIC SimPlantEquip

CONTAINS
          ! MODULE SUBROUTINES

SUBROUTINE SimPlantEquip(LoopNum,LoopSideNum,BranchNum,Num,FirstHVACIteration,InitLoopEquip,GetCompSizFac)    !DSU

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       June 2000  -Brandon Anderson
          !                             Changed to Group Similar Components.  Components will
          !                         be defined by ComponentType:SpecificComponent.
          !                         The colon will act as the type delimeter, So all
          !                         components of one type will be grouped. ex.(Boilers,Chillers)
          !                       May 2003 - Simon Rees
          !                         Added initial loop to force free cooling chiller etc to be
          !                         simulated before other components.
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calls the appropriate routines to simulate
          ! the equipment on the plant.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine employs a rule-based
          ! scheme to operate the plant equipment simulation without
          ! requiring a detailed flow network solver.  The scheme is based
          ! on several restrictive assumptions which may be relaxed when
          ! a more detailed solution technique is developed.  The current
          ! assumptions are:
          !    1.   All loop cooling/heating equipment is connected
          !         in parallel.
          !    2.   Only one circulation pump may be specified per loop.
          !    3.   The circulation pump must be specified first in the
          !         simulation order and is assumed to be connected in
          !         series with the cooling/heating equipment.
          !    4.   The Circ. pump determines the maximum flow rate for
          !         the loop.
          ! The scheme is valid only for Part Load based plant equipment
          ! models (currently the only type implemented).  Each equipment
          ! simulation updates its outlet node temperature, estimates its
          ! flow rate and returns a remaining loop demand which is passed
          ! on to the other available equipment.

          ! NOTE: All Equipment return the index of their lists during "InitLoopEquip"
          ! as a time reduction measure.  Specific ifs are set to catch those modules that don't.
          ! If you add a module or new equipment type, you must set up this structure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager,               ONLY: GetCurrentScheduleValue
  USE Boilers,                       ONLY: SimBoiler
  USE WaterThermalTanks,             ONLY: SimWaterThermalTank
  USE ChillerAbsorption,             ONLY: SimBLASTAbsorber
  USE ChillerIndirectAbsorption,     ONLY: SimIndirectAbsorber
  USE ChillerGasAbsorption,          ONLY: SimGasAbsorber
  USE ChillerExhaustAbsorption,      ONLY: SimExhaustAbsorber
  USE PlantChillers,                 ONLY: SimChiller
  USE ChillerElectricEIR,            ONLY: SimElectricEIRChiller
  USE ChillerReformulatedEIR,        ONLY: SimReformulatedEIRChiller
  USE HeatPumpWaterToWaterHEATING,   ONLY: SimHPWatertoWaterHEATING
  USE HeatPumpWaterToWaterCOOLING,   ONLY: SimHPWatertoWaterCOOLING
  USE HeatPumpWaterToWaterSimple,    ONLY: SimHPWatertoWaterSimple
  USE OutsideEnergySources,          ONLY: SimOutsideEnergy
  USE Pipes,                         ONLY: SimPipes
  USE PipeHeatTransfer,              ONLY: SimPipesHeatTransfer
  USE Pumps,                         ONLY: SimPumps


  USE PlantHeatExchangerFluidToFluid,ONLY: SimFluidHeatExchanger
  USE CondenserLoopTowers,           ONLY: SimTowers
  USE FluidCoolers,                  ONLY: SimFluidCoolers
  USE EvaporativeFluidCoolers,       ONLY: SimEvapFluidCoolers
  USE BoilerSteam,                   ONLY: SimSteamBoiler
  USE IceThermalStorage,             ONLY: SimIceStorage
  USE FuelCellElectricGenerator ,    ONLY: SimFuelCellPlantHeatRecovery
  USE MicroCHPElectricGenerator,     ONLY: SimMicroCHPPlantHeatRecovery
  USE PlantValves,                   ONLY: SimPlantValves
  USE ICEngineElectricGenerator,     ONLY: SimICEPlantHeatRecovery
  USE CTElectricGenerator,           ONLY: SimCTPlantHeatRecovery
  USE MicroturbineElectricGenerator, ONLY: SimMTPlantHeatRecovery
  USE GroundHeatExchangers ,         ONLY: SimGroundHeatExchangers
  USE SurfaceGroundHeatExchanger,    ONLY: SimSurfaceGroundHeatExchanger
  USE PondGroundHeatExchanger,       ONLY: SimPondGroundHeatExchanger

  USE PlantLoadProfile,              ONLY: SimulatePlantProfile
  USE WaterCoils,                    ONLY: UpdateWaterToAirCoilPlantConnection
  USE WaterUse,                      ONLY: SimulateWaterUseConnection
  USE SolarCollectors,               ONLY: SimSolarCollector
  USE BaseboardRadiator,             ONLY: UpdateBaseboardPlantConnection
  USE HWBaseboardRadiator,           ONLY: UpdateHWBaseboardPlantConnection
  USE SteamBaseboardRadiator,        ONLY: UpdateSteamBaseboardPlantConnection
  USE RefrigeratedCase,              ONLY: SimRefrigCondenser
  USE PhotovoltaicThermalCollectors, ONLY: SimPVTcollectors, CalledFromPlantLoopEquipMgr
  USE PlantPipingSystemsManager,     ONLY: SimPipingSystemCircuit
  USE UserDefinedComponents,         ONLY: SimUserDefinedPlantComponent
  USE HVACVariableRefrigerantFlow,   ONLY: SimVRFCondenserPlant
  USE PlantComponentTemperatureSources, ONLY: SimWaterSource
  USE PlantCentralGSHP,              ONLY: SimCentralGroundSourceHeatPump

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)    :: LoopNum        ! loop counter
  INTEGER , INTENT(IN)    :: LoopSideNum        ! loop counter
  INTEGER , INTENT(IN)    :: BranchNum
  INTEGER,  INTENT(IN)    :: Num
  LOGICAL , INTENT(IN)    :: FirstHVACIteration ! TRUE if First iteration of simulation
  LOGICAL , INTENT(INOUT) :: InitLoopEquip
  LOGICAL , INTENT(IN)    :: GetCompSizFac ! Tells component routine to return the component sizing fraction

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: CompNum            ! Plant side component list equipment number
  INTEGER      :: BrnNum         ! Branch counter
  INTEGER      :: EquipNum       ! Plant side component list equipment number
  INTEGER      :: EquipTypeNum
  INTEGER      :: BranchInletNode
  INTEGER      :: LastNodeOnBranch
  INTEGER      :: PumpOutletNode
  INTEGER      :: LoopControl
  LOGICAL      :: RunFlag        ! TRUE if operating this iteration
  CHARACTER(len=MaxNameLength) :: EquipType !local equipment type
  CHARACTER(len=MaxNameLength) :: EquipName ! local equipment name
  INTEGER      :: EquipFlowCtrl
  REAL(r64)    :: CurLoad
  REAL(r64)    :: MaxLoad
  REAL(r64)    :: MinLoad
  REAL(r64)    :: OptLoad
  REAL(r64)    :: SizingFac    ! the component sizing fraction
  REAL(r64)    :: BranchFlowRequest = 0.0d0
  REAL(r64)    :: InitialBranchFlow = 0.0d0
  INTEGER      :: GeneralEquipType !Basic Equipment type from EquipType Used to help organize this routine
  LOGICAL      :: PumpPowerToLoop = .False.
  LOGICAL,SAVE :: RunLoopPumps = .False.
  REAL(r64)       :: TempCondInDesign        ! Design condenser inlet temp. C , or 25.d0
  REAL(r64)       :: TempEvapOutDesign

  ! Based on the general equip type and the GetCompSizFac value, see if we can just leave early
  GeneralEquipType = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%GeneralEquipType
  IF ( GetCompSizFac .AND. (GeneralEquipType .NE. GenEquipTypes_Chiller .AND. GeneralEquipType .NE. GenEquipTypes_Boiler) .AND. &
                            GeneralEquipType .NE. GenEquipTypes_CoolingTower) THEN
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac = 0.0d0
    RETURN
  END IF

       !set local variables
  EquipType = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TypeOf
  EquipTypeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TypeOf_Num
  EquipFlowCtrl = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%Flowctrl
  GeneralEquipType = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%GeneralEquipType
  EquipName = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%Name
  EquipNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum
  RunFlag   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%ON
  CurLoad   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MyLoad

          !select equipment and call equiment simulation
  TypeOfEquip:   &
  SELECT CASE (GeneralEquipType)
                  !PIPES
    !Pipe has no special types at the moment, so find it this way
    CASE (GenEquipTypes_Pipe)
      SELECT CASE (EquipTypeNum)

        CASE (TypeOf_Pipe)
          CALL SimPipes(TypeOf_Pipe,EquipName,PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%MaxVolFlowRate,InitLoopEquip,FirstHVACIteration)

        CASE (TypeOf_PipeSteam)
          CALL SimPipes(TypeOf_PipeSteam,EquipName,PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%MaxVolFlowRate,InitLoopEquip,FirstHVACIteration)

        CASE (TypeOf_PipeExterior)
            CALL SimPipesHeatTransfer(TypeOf_PipeExterior,EquipName,  &
                          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                          InitLoopEquip,FirstHVACIteration)

        CASE (TypeOf_PipeInterior)
            CALL SimPipesHeatTransfer(TypeOf_PipeInterior,EquipName,  &
                           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                           InitLoopEquip,FirstHVACIteration)

        CASE (TypeOf_PipeUnderground)
            CALL SimPipesHeatTransfer(TypeOf_PipeUnderground,EquipName,  &
                           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                           InitLoopEquip,FirstHVACIteration)

        CASE (TypeOf_PipingSystemPipeCircuit)
            CALL SimPipingSystemCircuit(EquipName, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                           InitLoopEquip, FirstHVACIteration)

       CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Pipe Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

    CASE (GenEquipTypes_Pump)
      !DSU? This is still called by the sizing routine, is that OK?


!      SELECT CASE(EquipTypeNum)
!
!       CASE (TypeOf_LoopPump)
!         ! Loop pumps are simulated before this routine
!         PumpHeat = 0.0
!
!       CASE (TypeOf_BranchPump)  ! This is the branch pump case
!        BranchInletNode = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn
!        LastNodeOnBranch  = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumOut
!        BranchFlowRequest = Node(LastNodeOnBranch)%MassFlowRate
!
!        IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Flowlock == 0) THEN    !DSU
!          PumpPowerToLoop = .FALSE.
!        ELSE
!          PumpPowerToLoop = .TRUE.
!        END IF
!        CALL SimPumps(EquipName,LoopNum,BranchInletNode,BranchFlowRequest, &
!                       RunLoopPumps,InitialBranchFlow,PumpPowerToLoop,     &
!                       PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PumpIndex,PumpOutletNode,PumpHeat)
!
!        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%ON = RunLoopPumps
!        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%PumpMassFlow = node(PumpOutletNode)%massflowrate
!        ! This will only stay TRUE for this branch
!        If(PumpPowerToLoop) AddPumpHeat = .True.
!        IF(PlantLoop(LoopNum)%LoopSide(SupplySide)%Flowlock == 0) Node(PumpOutletNode)%temp = &   !DSU
!           PlantLoop(LoopNum)%LoopSide(DemandSide)%FlowRequestTemperature
!    END SELECT

                  !CHILLERS
    CASE (GenEquipTypes_Chiller)
      SELECT CASE (EquipTypeNum)

        CASE (TypeOf_Chiller_EngineDriven, TypeOf_Chiller_Electric, TypeOf_Chiller_ConstCOP, TypeOf_Chiller_CombTurbine)
          CALL SimChiller(LoopNum, LoopSideNum, EquipTypeNum,EquipName,EquipFlowCtrl,EquipNum,RunFlag,FirstHVACIteration, &
                    InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac, TempCondInDesign, TempEvapOutDesign)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesCondIn  =  TempCondInDesign
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesEvapOut =  TempEvapOutDesign
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_Chiller_Absorption)
          CALL SimBLASTAbsorber(EquipType,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,RunFlag,FirstHVACIteration, &
                                InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac,TempCondInDesign)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesCondIn  =  TempCondInDesign
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_Chiller_Indirect_Absorption)
          CALL SimIndirectAbsorber(EquipType,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,RunFlag,FirstHVACIteration, &
                                InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac,TempCondInDesign)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesCondIn  =  TempCondInDesign
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_Chiller_ElectricEIR)
          CALL SimElectricEIRChiller(EquipType,EquipName,EquipFlowCtrl,EquipNum,LoopNum,RunFlag,FirstHVACIteration, &
                    InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac,TempCondInDesign,TempEvapOutDesign)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesCondIn  =  TempCondInDesign
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesEvapOut =  TempEvapOutDesign
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_Chiller_ElectricReformEIR)
          CALL SimReformulatedEIRChiller(EquipType,EquipName,EquipFlowCtrl,EquipNum,LoopNum,RunFlag,FirstHVACIteration, &
                    InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac,TempCondInDesign,TempEvapOutDesign)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesCondIn  =  TempCondInDesign
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesEvapOut =  TempEvapOutDesign
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        ! Chiller-Heater needs to know whether it is being called for heating or cooling
        ! Since loops are generic, pass the branch inlet nodenum
        CASE (TypeOf_Chiller_DFAbsorption)
          CALL SimGasAbsorber(EquipType,EquipName,EquipFlowCtrl,EquipNum,RunFlag,FirstHVACIteration,&
                              InitLoopEquip,CurLoad,                         &    !DSU
                              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn, &
                              Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac,TempCondInDesign,TempEvapOutDesign)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesCondIn  =  TempCondInDesign
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%TempDesEvapOut =  TempEvapOutDesign
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

! Exhaust Fired Absorption Chiller

        CASE (TypeOf_Chiller_ExhFiredAbsorption)
          CALL SimExhaustAbsorber(EquipType,EquipName,EquipFlowCtrl,EquipNum,RunFlag,FirstHVACIteration,&
                              InitLoopEquip,CurLoad,                         &    !DSU
                              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn, &
                              Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Chiller Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Chiller='//TRIM(EquipType))
        CALL ShowContinueError('..Chiller Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

                  !HEAT PUMPS
    CASE (GenEquipTypes_HeatPump)
      SELECT CASE (EquipTypeNum)

        CASE (TypeOf_HPWaterPECooling)
          CALL SimHPWatertoWaterCOOLING(EquipType,EquipName,EquipNum,FirstHVACIteration, &    !DSU
                                        InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,LoopNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE (TypeOf_HPWaterPEHeating)
          CALL SimHPWatertoWaterHEATING(EquipType,EquipName,EquipNum,FirstHVACIteration, &    !DSU
                                        InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,LoopNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE (TypeOf_HPWaterEFCooling)
          CALL SimHPWatertoWaterSimple(EquipType, EquipTypeNum, EquipName,EquipNum, FirstHVACIteration, &    !DSU
                                      InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,LoopNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE (TypeOf_HPWaterEFHeating)
          CALL SimHPWatertoWaterSimple(EquipType, EquipTypeNum, EquipName,EquipNum, FirstHVACIteration, &    !DSU
                                        InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,LoopNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE (TypeOf_HeatPumpVRF)

          CALL SimVRFCondenserPlant(EquipType, EquipTypeNum, EquipName,EquipNum, FirstHVACIteration, &    !DSU
                                        InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,LoopNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Heat Pump Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for HeatPump='//TRIM(EquipType))
        CALL ShowContinueError('..HeatPump Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

                  !TOWERS
    CASE (GenEquipTypes_CoolingTower)

      SELECT CASE (EquipTypeNum)

                  !TOWERS
        CASE (TypeOf_CoolingTower_SingleSpd)

          CALL SimTowers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip,CurLoad, &    !DSU
                         MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)
           IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
           END IF
           IF (GetCompSizFac) THEN
             PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
           END IF

        CASE (TypeOf_CoolingTower_TwoSpd)

          CALL SimTowers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip,CurLoad, &    !DSU
                         MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_CoolingTower_VarSpd) ! 'CoolingTower:VariableSpeed'

          CALL SimTowers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip,CurLoad , &    !DSU
                         MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_CoolingTower_VarSpdMerkel ) 

          CALL SimTowers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip, CurLoad,&
                         MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF
        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Tower Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Cooling Tower='//TRIM(EquipType))
        CALL ShowContinueError('..Tower Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

                  !FLUID COOLERS
    CASE (GenEquipTypes_FluidCooler)

      SELECT CASE (EquipTypeNum)

                  !FluidCoolerS
        CASE (TypeOf_FluidCooler_SingleSpd)

          CALL SimFluidCoolers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip, &    !DSU
                         MaxLoad,MinLoad,OptLoad)
           IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
           END IF

        CASE (TypeOf_FluidCooler_TwoSpd)

          CALL SimFluidCoolers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip, &    !DSU
                         MaxLoad,MinLoad,OptLoad)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid FluidCooler Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Fluid Cooler='//TRIM(EquipType))
        CALL ShowContinueError('..Fluid Cooler Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

    CASE (GenEquipTypes_EvapFluidCooler)

      SELECT CASE (EquipTypeNum)

                  !EvapFluidCoolers
        CASE (TypeOf_EvapFluidCooler_SingleSpd)

          CALL SimEvapFluidCoolers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip, &    !DSU
                         MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)
           IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
           END IF

        CASE (TypeOf_EvapFluidCooler_TwoSpd)

          CALL SimEvapFluidCoolers(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip, &    !DSU
                         MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid EvapFluidCooler Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Fluid Cooler='//TRIM(EquipType))
        CALL ShowContinueError('..Fluid Cooler Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

                  !BOILERS
    CASE (GenEquipTypes_Boiler)
      SELECT CASE (EquipTypeNum)
        CASE (TypeOf_Boiler_Simple)
          CALL SimBoiler(EquipType,EquipName,EquipFlowCtrl,EquipNum,RunFlag, &    !DSU
                         InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE (TypeOf_Boiler_Steam)
          CALL SimSteamBoiler(EquipType,EquipName,EquipFlowCtrl,EquipNum,RunFlag,FirstHVACIteration, &    !DSU
                         InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
            IF (GetCompSizFac) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Boiler Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Boiler='//TRIM(EquipType))
        CALL ShowContinueError('..Boiler Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

                   !WATER HEATER
    CASE (GenEquipTypes_WaterThermalTank)

      SELECT CASE (EquipTypeNum)
        CASE (TypeOf_WtrHeaterMixed, TypeOf_WtrHeaterStratified)
            CALL SimWaterThermalTank(EquipTypeNum,EquipName,EquipNum,RunFlag, &    !DSU
                              InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad, FirstHVACIteration,  &
                              LoopNum = LoopNum, LoopSideNum = LoopSideNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

                    ! HEAT PUMP WATER HEATER
        CASE (TypeOf_HeatPumpWtrHeater)
            CALL SimWaterThermalTank(EquipTypeNum,EquipName,EquipNum,RunFlag, &    !DSU
                              InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad, FirstHVACIteration, &
                              LoopNum = LoopNum, LoopSideNum = LoopSideNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Water Heater Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Water Heater='//TRIM(EquipType))
        CALL ShowContinueError('..Water Thermal Tank Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

                  !PURCHASED
    CASE (GenEquipTypes_Purchased)
      SELECT CASE (EquipTypeNum)
        CASE (TypeOf_PurchChilledWater)
            Call SimOutsideEnergy(EquipType,EquipName,EquipFlowCtrl,EquipNum,RunFlag, &    !DSU
                                 InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad, FirstHVACIteration)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE (TypeOf_PurchHotWater)
            Call SimOutsideEnergy(EquipType,EquipName,EquipFlowCtrl,EquipNum,RunFlag, &    !DSU
                                 InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad, FirstHVACIteration)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid District Energy Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for District Energy='//TRIM(EquipType))
        CALL ShowContinueError('..District Cooling/Heating Name='//TRIM(EquipName)//', in Plant Loop='//  &
           TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

    CASE (GenEquipTypes_HeatExchanger)

            SELECT CASE (EquipTypeNum)

            CASE(TypeOf_FluidToFluidPlantHtExchg)
              CALL SimFluidHeatExchanger(LoopNum, LoopSideNum, EquipType, EquipName, EquipNum, &
                                          InitLoopEquip, CurLoad, MaxLoad, MinLoad, OptLoad)
                IF (InitLoopEquip)THEN
                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
                ENDIF

              CASE DEFAULT
                CALL ShowSevereError('SimPlantEquip: Invalid Heat Exchanger Type='//TRIM(EquipType))
                CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
                CALL ShowFatalError('Preceding condition causes termination.')

            END SELECT

    CASE (GenEquipTypes_GroundHeatExchanger)
      SELECT CASE (EquipTypeNum)

        CASE (TypeOf_GrndHtExchgVertical) ! 'GROUND HEAT EXCHANGER:VERTICAL'
          CALL SimGroundHeatExchangers(EquipType,EquipName,EquipNum,RunFlag, &
                      FirstHVACIteration,InitLoopEquip)    !DSU

          IF (InitLoopEquip)THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
          END IF

        CASE (TypeOf_GrndHtExchgSurface) ! 'GROUND HEAT EXCHANGER:SURFACE'
          CALL SimSurfaceGroundHeatExchanger(EquipName,EquipNum,FirstHVACIteration,RunFlag, &    !DSU
                      InitLoopEquip)

          IF (InitLoopEquip)THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum = Equipnum
          END IF

        CASE (TypeOf_GrndHtExchgPond) ! 'GROUND HEAT EXCHANGER:POND'
          CALL SimPondGroundHeatExchanger(EquipName,EquipNum,FirstHVACIteration,RunFlag, &    !DSU
                      InitLoopEquip,Maxload,MinLoad,OptLoad)

          IF (InitLoopEquip)THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum

          END IF

        CASE (TypeOf_GrndHtExchgHorizTrench)
            CALL SimPipingSystemCircuit(EquipName, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum, &
                           InitLoopEquip, FirstHVACIteration)

          IF (InitLoopEquip)THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum = Equipnum
          END IF

      END SELECT
                  ! THERMAL STORAGE
    CASE (GenEquipTypes_ThermalStorage)

                  ! If component setpoint based control is active for this equipment
                  ! then reset CurLoad to original EquipDemand.
                  ! Allow negative CurLoad.  For cold storage this means the storage should
                  ! charge, for hot storage, this means the storage should discharge.
      IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CurOpSchemeType == CompSetPtBasedSchemeType) THEN
        CurLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%EquipDemand
        IF(CurLoad .NE. 0) RunFlag = .TRUE.
      ENDIF

      SELECT CASE (EquipTypeNum)

        CASE (TypeOf_TS_IceSimple)

          CALL SimIceStorage(EquipType,EquipName,EquipNum,RunFlag,FirstHVACIteration, &    !DSU
                             InitLoopEquip,CurLoad)   !,EquipFlowCtrl

            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  0.0d0
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  0.0d0
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  0.0d0
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE (TypeOf_TS_IceDetailed)

          CALL SimIceStorage(EquipType,EquipName,EquipNum,RunFlag,FirstHVACIteration, &    !DSU
                             InitLoopEquip,CurLoad)    !,EquipFlowCtrl

              ! Not sure what this really needs to do here...
            IF (InitLoopEquip) THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  0.0d0
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  0.0d0
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  0.0d0
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE ( TypeOf_ChilledWaterTankMixed, TypeOf_ChilledWaterTankStratified)
            CALL SimWaterThermalTank(EquipTypeNum,EquipName,EquipNum,RunFlag, &    !DSU
                              InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad, FirstHVACIteration, &
                              LoopNum = LoopNum, LoopSideNum = LoopSideNum)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Chilled/Ice Thermal Storage Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Thermal Storage='//TRIM(EquipType))
        CALL ShowContinueError('..Chilled/Ice Thermal Storage Name='//TRIM(EquipName)//  &
           ', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

    CASE (GenEquipTypes_Valve)
      SELECT CASE (EquipTypeNum)
        CASE (TypeOf_ValveTempering)
           Call SimPlantValves  (EquipTypeNum,EquipName,EquipNum,RunFlag, &    !DSU
                                 InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad, FirstHVACIteration)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF
        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Valve Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Valves='//TRIM(EquipType))
        CALL ShowContinueError('..Valve Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

    CASE(GenEquipTypes_Generator)
      ! for heat recovery plant interactions.
      SELECT CASE(EquipTypeNum)

        CASE(TypeOf_Generator_FCExhaust)
            CALL SimFuelCellPlantHeatRecovery(EquipType,EquipName,TypeOf_Generator_FCExhaust,EquipNum,  &
                        RunFlag,InitLoopEquip,  &    !DSU
                        Curload,MaxLoad,MinLoad,OptLoad,FirstHVACIteration)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE(TypeOf_Generator_FCStackCooler)
            CALL SimFuelCellPlantHeatRecovery(EquipType,EquipName,TypeOf_Generator_FCStackCooler,EquipNum,  &
                        RunFlag,InitLoopEquip,  &    !DSU
                        Curload,MaxLoad,MinLoad,OptLoad,FirstHVACIteration)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE(TypeOf_Generator_MicroCHP)
            CALL SimMicroCHPPlantHeatRecovery(EquipType,EquipName,EquipNum,RunFlag,InitLoopEquip,  &    !DSU
                        Curload,MaxLoad,MinLoad,OptLoad,FirstHVACIteration)
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE(TypeOf_Generator_MicroTurbine)
            CALL SimMTPlantHeatRecovery(EquipType,EquipName,TypeOf_Generator_MicroTurbine, EquipNum,RunFlag, &
                        InitLoopEquip,Curload,MaxLoad,MinLoad,OptLoad,FirstHVACIteration)    !DSU
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE(TypeOf_Generator_ICEngine)
            CALL SimICEPlantHeatRecovery(EquipType,EquipName,TypeOf_Generator_ICEngine, EquipNum,RunFlag, &
                        InitLoopEquip,Curload,MaxLoad,MinLoad,OptLoad,FirstHVACIteration)    !DSU
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE(TypeOf_Generator_CTurbine)
            CALL SimCTPlantHeatRecovery(EquipType,EquipName,TypeOf_Generator_CTurbine, EquipNum,RunFlag, &
                        InitLoopEquip,Curload,MaxLoad,MinLoad,OptLoad,FirstHVACIteration)    !DSU
            IF(InitLoopEquip)THEN
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
            END IF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Generator Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')

      END SELECT

      IF (InitLoopEquip .and. EquipNum == 0) THEN
        CALL ShowSevereError('InitLoop did not set Equipment Index for Generator='//TRIM(EquipType))
        CALL ShowContinueError('..Generator Name='//TRIM(EquipName)//', in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Previous condition causes termination.')
      ENDIF

    CASE (GenEquipTypes_LoadProfile)  ! DSU2 draft out InitLoopEquip on a demand side component
      SELECT CASE(EquipTypeNum)

        CASE(TypeOf_PlantLoadProfile)
          CALL SimulatePlantProfile(EquipType,EquipName,TypeOf_PlantLoadProfile,EquipNum,&
                                       FirstHVACIteration, InitLoopEquip)
          IF(InitLoopEquip)THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum

          ENDIF

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Load Profile Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

    CASE (GenEquipTypes_DemandCoil)   !DSU3
      ! for now these are place holders, the sim routines are called from other places, unclear if we need
      !  to call an update routine, or if air-side updates are sufficient.  this is where plant updates would be called from


      SELECT CASE (EquipTypeNum) !DSU3

        CASE ( TypeOf_CoilWaterCooling )
!          CALL UpdateWaterToAirCoilPlantConnection(EquipTypeNum,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,&
!                                                   FirstHVACIteration, InitLoopEquip)
!
!          IF(InitLoopEquip)THEN
!            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
!          ENDIF
        CASE ( TypeOf_CoilWaterDetailedFlatCooling )
!          CALL UpdateWaterToAirCoilPlantConnection(EquipTypeNum,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,&
!                                                   FirstHVACIteration, InitLoopEquip)
!
!          IF(InitLoopEquip)THEN
!            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
!          ENDIF
        CASE ( TypeOf_CoilWaterSimpleHeating )
!!          CALL UpdateWaterToAirCoilPlantConnection(EquipTypeNum,EquipName,EquipFlowCtrl,LoopNum,LoopSideNum,EquipNum,&
!!                                                   FirstHVACIteration, InitLoopEquip)
!!
!          IF(InitLoopEquip)THEN
!            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
!          ENDIF
        CASE ( TypeOf_CoilSteamAirHeating )
          !CALL UpdateSteamToAirCoilPlantConnection(  )

        CASE ( TypeOf_CoilWAHPHeatingEquationFit )

        CASE ( TypeOf_CoilWAHPCoolingEquationFit )

        CASE ( TypeOf_CoilVSWAHPHeatingEquationFit )

        CASE ( TypeOf_CoilVSWAHPCoolingEquationFit )


        CASE ( TypeOf_CoilWAHPHeatingParamEst )

        CASE ( TypeOf_CoilWAHPCoolingParamEst )

        CASE ( TypeOf_PackagedTESCoolingCoil )

        CASE DEFAULT
          CALL ShowSevereError('SimPlantEquip: Invalid Load Coil Type='//TRIM(EquipType))
          CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
          CALL ShowFatalError('Preceding condition causes termination.')

      END SELECT  !DSU3

    CASE (GenEquipTypes_WaterUse)

      SELECT CASE (EquipTypeNum)

      CASE (TypeOf_WaterUseConnection)

        CALL SimulateWaterUseConnection(EquipTypeNum, EquipName, EquipNum, InitLoopEquip, FirstHVACIteration)

        IF (InitLoopEquip) THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        ENDIF
      CASE DEFAULT
        CALL ShowSevereError('SimPlantEquip: Invalid Load Coil Type='//TRIM(EquipType))
        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

    CASE (GenEquipTypes_SolarCollector)

      SELECT CASE (EquipTypeNum)

      CASE (TypeOf_SolarCollectorFlatPlate, TypeOf_SolarCollectorICS)

        CALL SimSolarCollector(EquipTypeNum, EquipName, EquipNum, InitLoopEquip, FirstHVACIteration)

        IF (InitLoopEquip) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        ENDIF

      CASE (TypeOf_PVTSolarCollectorFlatPlate)

        CALL SimPVTcollectors(EquipNum, FirstHVACIteration, CalledFromPlantLoopEquipMgr, PVTName=EquipName, &
                                 InitLoopEquip=InitLoopEquip)

        IF (InitLoopEquip) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        ENDIF

      CASE DEFAULT
        CALL ShowSevereError('SimPlantEquip: Invalid Solar Collector Type='//TRIM(EquipType))
        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

    CASE (GenEquipTypes_ZoneHVACDemand) !ZoneHVAC and air terminal models with direct plant connections
      ! for now these are place holders, the sim routines are called from other places, unclear if we need
      !  to call an update routine, or if air-side updates are sufficient.  this is where plant updates would be called from

      SELECT CASE (EquipTypeNum)

      CASE (TypeOf_BASEBOARD_CONV_WATER)


      CASE (TypeOf_BASEBOARD_RAD_CONV_STEAM)


      CASE (TypeOf_BASEBOARD_RAD_CONV_WATER)

      CASE (TypeOf_LowTempRadiant_VarFlow)

      CASE (TypeOf_LowTempRadiant_ConstFlow)

      CASE (TypeOf_CooledBeamAirTerminal)

      CASE (TypeOf_MultiSpeedHeatPumpRecovery)

      CASE (TypeOf_UnitarySystemRecovery)

      CASE DEFAULT

        CALL ShowSevereError('SimPlantEquip: Invalid ZoneHVAC Type='//TRIM(EquipType))
        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')

      END SELECT

    CASE( GenEquipTypes_Refrigeration )

      SELECT CASE (EquipTypeNum)

      CASE (TypeOf_RefrigSystemWaterCondenser)
        CALL SimRefrigCondenser(EquipTypeNum, EquipName, EquipNum, FirstHVACIteration, InitLoopEquip)

        IF (InitLoopEquip) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        ENDIF


      CASE (TypeOf_RefrigerationWaterCoolRack)
        CALL SimRefrigCondenser(EquipTypeNum, EquipName, EquipNum, FirstHVACIteration, InitLoopEquip)

        IF (InitLoopEquip) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        ENDIF

      CASE DEFAULT
        CALL ShowSevereError('SimPlantEquip: Invalid Refrigeration Type='//TRIM(EquipType))
        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

    CASE (GenEquipTypes_PlantComponent)

      SELECT CASE (EquipTypeNum)

      CASE (Typeof_PlantComponentUserDefined)

        CALL SimUserDefinedPlantComponent(LoopNum, LoopSideNum, EquipType,EquipName, &
                                         EquipNum, InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad)
        IF(InitLoopEquip)THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        END IF

      CASE(TypeOf_WaterSource)

        CALL SimWaterSource(EquipName,EquipFlowCtrl,EquipNum,RunFlag, FirstHVACIteration, &
                                   InitLoopEquip,CurLoad,MaxLoad,MinLoad,OptLoad,GetCompSizFac,SizingFac)    !DSU
        IF (InitLoopEquip) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
        ENDIF

      CASE DEFAULT
!        CALL ShowSevereError('SimPlantEquip: Invalid Component Equipment Type='//TRIM(EquipType))
!        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
!        CALL ShowFatalError('Preceding condition causes termination.')

      END SELECT

    CASE (GenEquipTypes_CentralHeatPumpSystem)

      SELECT CASE(EquipTypeNum)

      CASE (TypeOf_CentralGroundSourceHeatPump)

         CALL SimCentralGroundSourceHeatPump(EquipName,EquipFlowCtrl,EquipNum,LoopNum, RunFlag,FirstHVACIteration, &
                         InitLoopEquip,CurLoad,Maxload,MinLoad,OptLoad,GetCompSizFac,SizingFac)
         IF (InitLoopEquip) THEN
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MaxLoad =  MaxLoad
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%MinLoad =  MinLoad
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%OptLoad =  OptLoad
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%CompNum =  EquipNum
         ENDIF

         IF (GetCompSizFac) THEN
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(Num)%SizFac =  SizingFac
         END IF

      CASE DEFAULT
        CALL ShowSevereError('SimPlantEquip: Invalid Central Heat Pump System Type='//TRIM(EquipType))
        CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
        CALL ShowFatalError('Preceding condition causes termination.')
      END SELECT

    CASE DEFAULT
      CALL ShowSevereError('SimPlantEquip: Invalid Equipment Type='//TRIM(EquipType))
      CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopNum)%Name))
      CALL ShowFatalError('Preceding condition causes termination.')
  END SELECT TypeOfEquip

  RETURN
END SUBROUTINE SimPlantEquip

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

END MODULE PlantLoopEquip
