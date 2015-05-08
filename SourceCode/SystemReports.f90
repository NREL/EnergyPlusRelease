MODULE SystemReports

          ! Module containing the routines dealing with Mechanical Ventilation Loads and Energy Reporting (Outside Air)

          ! MODULE INFORMATION:
          !       AUTHOR         Mike Witte, Linda Lawrie, Dan Fisher
          !       DATE WRITTEN   Apr-Jul 2005
          !       MODIFIED       22Aug2010 Craig Wray - added Fan:ComponentModel
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module embodies the scheme(s) for reporting ventilation loads and energy use.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataAirLoop
USE DataGlobals
USE DataHVACGlobals
USE DataSizing
USE DataZoneEquipment
USE DataAirSystems
USE DataInterfaces

IMPLICIT NONE


PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER    :: NoHeatNoCool = 0
INTEGER, PARAMETER    :: CoolingOnly = 1
INTEGER, PARAMETER    :: HeatingOnly = 2
INTEGER, PARAMETER    :: HeatAndCool = 3
INTEGER, PARAMETER    :: MaxSetBackCount = 3

          ! DERIVED TYPE DEFINITIONS:
TYPE Energy
  REAL(r64):: TotDemand  = 0.0d0
  REAL(r64):: Elec       = 0.0d0
  REAL(r64):: Gas        = 0.0d0
  REAL(r64):: Purch      = 0.0d0
  REAL(r64):: Other      = 0.0d0
END TYPE Energy

TYPE CoilType
  TYPE(Energy) :: DecreasedCC             ! LoadMetByVent
  TYPE(Energy) :: DecreasedHC             ! LoadMetByVent
  TYPE(Energy) :: IncreasedCC           ! LoadIncreasedVent
  TYPE(Energy) :: IncreasedHC           ! LoadAddedByVent
  TYPE(Energy) :: ReducedByCC           ! LoadAddedByVent
  TYPE(Energy) :: ReducedByHC           ! LoadAddedByVent
END TYPE CoilType

TYPE SummarizeLoads
  TYPE(Coiltype) :: Load             ! LoadMetByVent
  TYPE(Coiltype) :: NoLoad           ! LoadMetByVentNoLoad
  TYPE(Coiltype) :: ExcessLoad         ! LoadAddedByVentOvercool
  TYPE(Coiltype) :: PotentialSavings        ! LoadAddedByVentCoolLost
  TYPE(Coiltype) :: PotentialCost    ! LoadAddedByVentHeatLost
END TYPE SummarizeLoads

TYPE(SummarizeLoads) , ALLOCATABLE, DIMENSION(:)  :: Vent

          ! MODULE VARIABLE DECLARATIONS:
!Ventilation Report Variables
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxCoolingLoadMetByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxCoolingLoadAddedByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxOvercoolingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxHeatingLoadMetByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxHeatingLoadAddedByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxOverheatingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxNoLoadHeatingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MaxNoLoadCoolingByVent

REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxCoolingLoadMetByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxCoolingLoadAddedByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxOvercoolingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxHeatingLoadMetByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxHeatingLoadAddedByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxOverheatingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxNoLoadHeatingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RemMaxNoLoadCoolingByVent

REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxCoolingLoadMetByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxCoolingLoadAddedByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxOvercoolingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxHeatingLoadMetByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxHeatingLoadAddedByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxOverheatingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxNoLoadHeatingByVent
REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastMaxNoLoadCoolingByVent

REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotZoneLoadHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotZoneLoadCLNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysOALoadHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysOALoadCLNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotCLNG

REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotH2OHOT
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotH2OCOLD
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotGas
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysTotSteam


REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHumidHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHumidElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysEvapCLNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysEvapElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHeatExHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHeatExCLNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: DesDehumidCLNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: DesDehumidElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysSolarCollectHeating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysSolarCollectCooling
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysUserDefinedTerminalHeating
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysUserDefinedTerminalCooling

REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysFANCompHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysFANCompElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysCCCompCLNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysCCCompH2OCOLD
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysCCCompElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHCCompH2OHOT
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHCCompElec
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHCCompElecRes
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHCCompHTNG
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHCCompGas
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysHCCompSteam
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SysDomesticH20

REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOAMassFlow        ! zone mech vent mass flow rate {kg/s}
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOAMass            ! zone mech vent total mass for time {kg}
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOAVolFlowStdRho   ! zone mech vent volume flow rate at standard density {m3/s}
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOAVolStdRho       ! zone mech vent total volume OA at standard density {m3/s}
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOAVolFlowCrntRho  ! zone mech vent volume flow rate at current density {m3/s}
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneOAVolCrntRho      ! zone mech vent total volume OA at current density {m3/s}
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneMechACH           ! zone mech vent air changes per hour {ACH}


LOGICAL :: AirLoopLoadsReportEnabled=.true.
LOGICAL :: VentLoadsReportEnabled=.true.
LOGICAL :: VentEnergyReportEnabled=.false.
LOGICAL :: VentReportStructureCreated=.false.
INTEGER :: TotalLoopConnects=0  ! Total number of loop connections
INTEGER :: MaxLoopArraySize = 100
INTEGER :: MaxCompArraySize = 500
INTEGER :: DBFlag=0

INTEGER, ALLOCATABLE, DIMENSION(:) :: SetBackCounter
INTEGER, ALLOCATABLE, DIMENSION(:) :: HeatCoolFlag
INTEGER, ALLOCATABLE, DIMENSION(:) :: FirstHeatCoolFlag
INTEGER, ALLOCATABLE, DIMENSION(:) :: FirstHeatCoolHour
INTEGER, ALLOCATABLE, DIMENSION(:) :: LastHeatCoolFlag
INTEGER, ALLOCATABLE, DIMENSION(:) :: LastHeatCoolHour
LOGICAL, ALLOCATABLE, DIMENSION(:) :: AirLoopCalcDone
LOGICAL, ALLOCATABLE, DIMENSION(:) :: NoLoadFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: UnmetLoadFlag

          ! SUBROUTINE SPECIFICATIONS FOR MODULE SystemReports

          !Reporting Initialization
PUBLIC  InitEnergyReports
PUBLIC  CreateEnergyReportStructure
PRIVATE AllocateAndSetUpVentReports
PRIVATE UpdateZoneCompPtrArray
PRIVATE UpdateZoneSubCompPtrArray
PRIVATE UpdateZoneSubSubCompPtrArray
PRIVATE UpdateAirSysCompPtrArray
PRIVATE UpdateAirSysSubCompPtrArray
PRIVATE UpdateAirSysSubSubCompPtrArray

          ! Reporting routines for module
PUBLIC  ReportAirLoopConnections
PUBLIC  ReportMaxVentilationLoads
PUBLIC  ReportSystemEnergyUse
PRIVATE CalcSystemEnergyUse
PRIVATE FindDemandSideMatch     ! a routine that assists report initialization
PRIVATE FindFirstLastPtr
PRIVATE MatchPlantSys

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************

SUBROUTINE InitEnergyReports

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   April 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the energy components of the data structures

          ! METHODOLOGY EMPLOYED:
          ! Once all compsets have been established (second iteration) find all components
          ! subcomponents, etc.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataEnvironment, ONLY: StdBaroPress, OutHumRat
  USE SplitterComponent, ONLY: SplitterCond, NumSplitters
  USE InputProcessor, ONLY: FindItemInList
  USE Psychrometrics, ONLY: PsyHFnTdbW,PsyRhoAirFnPbTdbW
  USE ZonePlenum, ONLY: ZoneSupPlenCond, NumZoneSupplyPlenums
  USE DataConvergParams, ONLY: HVACFlowRateToler
  USE DataGlobalConstants

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER    :: TypeComp = 1
  INTEGER, PARAMETER    :: TypeSubComp = 2
  INTEGER, PARAMETER    :: TypeSubSubComp = 3
  INTEGER, PARAMETER    :: EnergyTransfer = 1


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


  INTEGER :: AirDistUnitNum
  INTEGER :: MatchLoop
  INTEGER :: MatchLoopType
  INTEGER :: MatchBranch
  INTEGER :: MatchComp
  INTEGER :: AirLoopNum
  INTEGER :: BranchNum
  INTEGER :: ZoneInletNodeNum
  INTEGER :: CompNum
  INTEGER :: VarNum
  INTEGER :: SubCompNum
  INTEGER :: SubSubCompNum
  INTEGER :: EquipNum
  INTEGER :: SubEquipNum
  INTEGER :: SubSubEquipNum
  INTEGER :: CtrlZoneNum
  INTEGER :: NodeIndex
  INTEGER :: Index
  INTEGER :: TempIndex
  INTEGER :: ListNum
  INTEGER :: SAPNum
  INTEGER :: SAPOutNode
  INTEGER :: MainBranchNum
  INTEGER :: SupplyCoolBranchNum
  INTEGER :: SupplyHeatBranchNum
  INTEGER :: VarType
  INTEGER :: VarIndex
  INTEGER :: OutNum
  INTEGER :: NodeCount
  INTEGER :: PlantLoopNum
  INTEGER :: NumZoneConnectComps
  INTEGER :: NumZoneConnectSubComps
  INTEGER :: NumZoneConnectSubSubComps
  INTEGER :: NumAirSysConnectComps
  INTEGER :: NumAirSysConnectSubComps
  INTEGER :: NumAirSysConnectSubSubComps
  INTEGER :: ArrayCount
  INTEGER :: LoopType
  INTEGER :: LoopNum
  INTEGER :: FirstIndex
  INTEGER :: LastIndex
  INTEGER :: LoopCount
  CHARACTER(len=MaxNameLength)  ::CompType
  CHARACTER(len=MaxNameLength)  ::CompName
  LOGICAL :: MatchFound
  LOGICAL,Save :: OneTimeFlag  = .True.  ! Flag set to make sure you initialize reports one time
  LOGICAL      :: Duplicate
  LOGICAL      :: ConnectionFlag

  IF (.not. VentReportStructureCreated) RETURN

  IF (OneTimeFlag) THEN

        ! ***I think we need to preprocess the main components on the branch to get them in order***
        ! This needs to be done before we start in on the component loop
        ! GetChildrenData will put all of the subcomponents in order for us

    DO CtrlZoneNum=1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
      AirLoopNum = ZoneEquipConfig(CtrlZoneNum)%AirLoopNum
      ZoneEquipConfig(CtrlZoneNum)%EquipListIndex =   &
         finditeminlist(ZoneEquipConfig(CtrlZoneNum)%EquipListName, ZoneEquipList%Name,NumofZones)
      ListNum = ZoneEquipConfig(CtrlZoneNum)%EquipListIndex
      DO ZoneInletNodeNum=1,ZoneEquipConfig(CtrlZoneNum)%NumInletNodes
        DO CompNum = 1, ZoneEquipList(ListNum)%NumOfEquipTypes
          DO NodeCount = 1, ZoneEquipList(ListNum)%EquipData(CompNum)%NumOutlets
            IF (ZoneEquipList(ListNum)%EquipData(CompNum)%OutletNodeNums(NodeCount) == &
                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%OutNode)THEN
              ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%AirDistUnitIndex = CompNum
              IF (ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyAirPathExists)THEN
                DO SAPNum =  1, numsupplyairpaths
                  DO SAPOutNode = 1, SupplyAirPath(SAPNum)%NumOutletNodes
                    IF (ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%InNode== &
                        SupplyAirPath(SAPNum)%OutletNode(SAPOutNode))THEN
                      ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyAirPathIndex = SAPNum
                      DO OutNum=1,AirToZoneNodeInfo(AirLoopNum)%NumSupplyNodes
                        IF (AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(OutNum) == &
                            SupplyAirPath(SAPNum)%InletNodeNum)THEN
                          ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyBranchIndex = &
                               PrimaryAirSystem(AirLoopNum)%OutletBranchNum(OutNum)
                          IF (PrimaryAirSystem(AirLoopNum)%Splitter%Exists)THEN
                            DO MainBranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
                              IF (PrimaryAirSystem(AirLoopNum)%Branch(MainBranchNum)%NodeNumOut ==   &
                                 PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumIn)THEN
                                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%MainBranchIndex = MainBranchNum
                              END IF
                            END DO
                          ELSE !no splitter
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%MainBranchIndex = &
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyBranchIndex
                          END IF
                        END IF
                      END DO
                    END IF
                  END DO
                END DO
              ELSE !no supply air path
                IF (AirLoopNum > 0) THEN
                  DO NodeIndex = 1, AirToZoneNodeInfo(AirLoopNum)%NumSupplyNodes
                    IF(AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(NodeIndex) == &
                       ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%InNode)THEN
                      DO BranchNum =1, PrimaryAirSystem(AirLoopNum)%NumBranches
                        IF(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut == &
                           AirToZoneNodeInfo(AirLoopNum)%AirLoopSupplyNodeNum(NodeIndex))THEN
                          ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyBranchIndex = BranchNum
                          IF(PrimaryAirSystem(AirLoopNum)%Splitter%Exists)THEN
                            DO MainBranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
                              IF(PrimaryAirSystem(AirLoopNum)%Branch(MainBranchNum)%NodeNumOut ==   &
                                 PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumIn)THEN
                                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%MainBranchIndex = MainBranchNum
                              END IF
                            END DO
                          ELSE !no splitter
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%MainBranchIndex = &
                               ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyAirPathIndex
                          END IF
                        END IF
                      END DO
                    END IF
                  END DO
                END IF
              END IF
            ELSE IF(ZoneEquipList(ListNum)%EquipData(CompNum)%OutletNodeNums(NodeCount) == &
                    ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%InNode)THEN
              ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%AirDistUnitIndex = CompNum
              IF(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyAirPathExists)THEN
                DO SAPNum =  1, numsupplyairpaths
                  DO NodeIndex = 1, AirToZoneNodeInfo(AirLoopNum)%NumSupplyNodes
                    IF(AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(NodeIndex) == &
                           SupplyAirPath(SAPNum)%InletNodeNum)THEN
                      DO BranchNum =1, PrimaryAirSystem(AirLoopNum)%NumBranches
                        IF(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut == &
                            AirToZoneNodeInfo(AirLoopNum)%AirLoopSupplyNodeNum(NodeIndex))THEN
                          ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyBranchIndex = BranchNum
                          IF(PrimaryAirSystem(AirLoopNum)%Splitter%Exists)THEN
                            DO MainBranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
                              IF(PrimaryAirSystem(AirLoopNum)%Branch(MainBranchNum)%NodeNumOut ==   &
                                 PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumIn)THEN
                                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%MainBranchIndex = MainBranchNum
                              END IF
                            END DO
                          ELSE !no splitter
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%MainBranchIndex = &
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyAirPathIndex
                          END IF
                        END IF
                      END DO
                    END IF
                  END DO

                  DO SAPOutNode = 1, SupplyAirPath(SAPNum)%NumOutletNodes
                    IF( ZoneInletNodeNum == SupplyAirPath(SAPNum)%OutletNode(SAPOutNode))THEN
                      ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyAirPathIndex = SAPNum
                    END IF
                  END DO
                END DO
              ELSE !no supply air path
                IF (AirLoopNum > 0) THEN
                  DO NodeIndex = 1, AirToZoneNodeInfo(AirLoopNum)%NumSupplyNodes
                    IF(AirToZoneNodeInfo(AirLoopNum)%ZoneEquipSupplyNodeNum(NodeIndex) == &
                        ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%InNode)THEN
                      DO BranchNum =1, PrimaryAirSystem(AirLoopNum)%NumBranches
                        IF(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut == &
                           AirToZoneNodeInfo(AirLoopNum)%AirLoopSupplyNodeNum(NodeIndex))THEN
                          ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyBranchIndex = BranchNum
                          IF(PrimaryAirSystem(AirLoopNum)%Splitter%Exists)THEN
                            DO MainBranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
                              IF(PrimaryAirSystem(AirLoopNum)%Branch(MainBranchNum)%NodeNumOut ==   &
                                 PrimaryAirSystem(AirLoopNum)%Splitter%NodeNumIn)THEN
                                ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%MainBranchIndex = MainBranchNum
                              END IF
                            END DO
                          ELSE !no splitter
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%MainBranchIndex = &
                            ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyAirPathIndex
                          END IF
                        END IF
                      END DO
                    END IF
                  END DO
                END IF
              END IF
            ELSE
              Continue
              !Can't tell if there's an error based on this code...need to check logical flags separately
            END IF
          END DO
        END DO
      END DO
    END DO


    DO CtrlZoneNum=1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
      AirLoopNum = ZoneEquipConfig(CtrlZoneNum)%AirLoopNum
      ZoneEquipConfig(CtrlZoneNum)%EquipListIndex =   &
         finditeminlist(ZoneEquipConfig(CtrlZoneNum)%EquipListName, ZoneEquipList%Name,NumofZones)
      ListNum = ZoneEquipConfig(CtrlZoneNum)%EquipListIndex
          !loop over the zone supply air path inlet nodes
      DO ZoneInletNodeNum=1,ZoneEquipConfig(CtrlZoneNum)%NumInletNodes

        ! 1. Find HVAC component plant loop connections
        MainBranchNum = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%MainBranchIndex
        MainBranchNum = MAX(MainBranchNum,ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%MainBranchIndex)
        IF(MainBranchNum > 0)CALL MatchPlantSys(AirLoopNum,MainBranchNum)
        SupplyCoolBranchNum = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%SupplyBranchIndex
        IF(SupplyCoolBranchNum > 0 .AND. (SupplyCoolBranchNum .NE. MainBranchNum))CALL MatchPlantSys(AirLoopNum,SupplyCoolBranchNum)
        SupplyHeatBranchNum = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%SupplyBranchIndex
        IF(SupplyHeatBranchNum > 0 .AND. (SupplyHeatBranchNum .NE. MainBranchNum))CALL MatchPlantSys(AirLoopNum,SupplyHeatBranchNum)

        AirDistUnitNum = ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInletNodeNum)%AirDistUnitIndex
        AirDistUnitNum = MAX(AirDistUnitNum,ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInletNodeNum)%AirDistUnitIndex)
        IF(ListNum > 0 .AND. AirDistUnitNum >0)THEN
          DO VarNum = 1, ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%NumMeteredVars
            IF(ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%MeteredVar(VarNum)%ResourceType == iRT_EnergyTransfer) THEN
              ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%EnergyTransComp = EnergyTransfer
              CompType = ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%TypeOf
              CompName = ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%Name
              Index = 0
              CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
              IF(MatchFound)CALL UpdateZoneCompPtrArray(Index,ListNum,AirDistUnitNum,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
              ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%ZoneEqToPlantPtr = Index
              EXIT
            END IF
          END DO
          DO SubEquipNum = 1, ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%NumSubEquip
            DO VarNum = 1, ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%NumMeteredVars
              IF(ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                 MeteredVar(VarNum)%ResourceType == iRT_EnergyTransfer) THEN
                ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%EnergyTransComp = EnergyTransfer
                CompType = ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%TypeOf
                CompName = ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%Name
                Index = 0
                CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
                IF(MatchFound)  &
                   CALL UpdateZoneSubCompPtrArray(Index,ListNum,AirDistUnitNum,SubEquipNum,MatchLoopType,  &
                      MatchLoop,MatchBranch,MatchComp)
                ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%ZoneEqToPlantPtr = Index
                EXIT
              END IF
            END DO
            DO SubSubEquipNum = 1, ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%NumSubSubEquip
              DO VarNum = 1, ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                 SubSubEquipData(SubSubEquipNum)%NumMeteredVars
                IF(ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                   SubSubEquipData(SubSubEquipNum)%MeteredVar(VarNum)%ResourceType == iRT_EnergyTransfer) THEN
                  ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                     SubSubEquipData(SubSubEquipNum)%EnergyTransComp = EnergyTransfer
                  CompType = ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                     SubSubEquipData(SubSubEquipNum)%TypeOf
                  CompName = ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                     SubSubEquipData(SubSubEquipNum)%Name
                  Index = 0
                  CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
                IF(MatchFound)  &
                   CALL UpdateZoneSubSubCompPtrArray(Index,ListNum,AirDistUnitNum,SubEquipNum,SubSubEquipNum,MatchLoopType,  &
                      MatchLoop,MatchBranch,MatchComp)
                  ZoneEquipList(ListNum)%EquipData(AirDistUnitNum)%SubEquipData(SubEquipNum)%  &
                     SubSubEquipData(SubSubEquipNum)%ZoneEqToPlantPtr = Index
                  EXIT
                END IF
              END DO
            END DO
          END DO
        END IF

            !Eliminate duplicates in the connection arrays
        IF (ALLOCATED(ZoneCompToPlant)) THEN
          EquipNum        = SIZE(ZoneCompToPlant)
        ELSE
          EquipNum        = 0
        ENDIF
        IF (ALLOCATED(ZoneSubCompToPlant)) THEN
          SubEquipNum     = SIZE(ZoneSubCompToPlant)
        ELSE
          SubEquipNum     = 0
        ENDIF
        IF (ALLOCATED(ZoneSubSubCompToPlant)) THEN
          SubSubEquipNum  = SIZE(ZoneSubSubCompToPlant)
        ELSE
          SubSubEquipNum  = 0
        ENDIF
        IF (ALLOCATED(AirSysCompToPlant)) THEN
          CompNum         = SIZE(AirSysCompToPlant)
        ELSE
          CompNum         = 0
        ENDIF
        IF (ALLOCATED(AirSysSubCompToPlant)) THEN
          SubCompNum      = SIZE(AirSysSubCompToPlant)
        ELSE
          SubCompNum      = 0
        ENDIF
        IF (ALLOCATED(AirSysSubSubCompToPlant)) THEN
          SubSubCompNum   = SIZE(AirSysSubSubCompToPlant)
        ELSE
          SubSubCompNum   = 0
        ENDIF

        IF (EquipNum > 0) THEN
          TempZoneCompToPlant%ZoneEqListNum       = 0
          TempZoneCompToPlant%ZoneEqCompNum       = 0
          TempZoneCompToPlant%PlantLoopType       = 0
          TempZoneCompToPlant%PlantLoopNum        = 0
          TempZoneCompToPlant%PlantLoopBranch     = 0
          TempZoneCompToPlant%PlantLoopComp       = 0
          TempZoneCompToPlant%FirstDemandSidePtr  = 0
          TempZoneCompToPlant%LastDemandSidePtr   = 0

          ArrayCount = 0
          DO Index = 1, EquipNum
            Duplicate = .FALSE.
            DO TempIndex = 1,Equipnum
              IF (ZoneCompToPlant(Index)%ZoneEqListNum == TempZoneCompToPlant(TempIndex)%ZoneEqListNum .AND. &
                  ZoneCompToPlant(Index)%ZoneEqCompNum == ZoneCompToPlant(TempIndex)%ZoneEqCompNum)THEN
                Duplicate = .TRUE.
                EXIT
              END IF
            END DO
            IF(.NOT. Duplicate)THEN
              ArrayCount = ArrayCount + 1
              TempZoneCompToPlant(ArrayCount)%ZoneEqListNum        = ZoneCompToPlant(Index)%ZoneEqListNum
              TempZoneCompToPlant(ArrayCount)%ZoneEqCompNum        = ZoneCompToPlant(Index)%ZoneEqCompNum
              TempZoneCompToPlant(ArrayCount)%PlantLoopType        = ZoneCompToPlant(Index)%PlantLoopType
              TempZoneCompToPlant(ArrayCount)%PlantLoopNum         = ZoneCompToPlant(Index)%PlantLoopNum
              TempZoneCompToPlant(ArrayCount)%PlantLoopBranch      = ZoneCompToPlant(Index)%PlantLoopBranch
              TempZoneCompToPlant(ArrayCount)%PlantLoopComp        = ZoneCompToPlant(Index)%PlantLoopComp
              TempZoneCompToPlant(ArrayCount)%FirstDemandSidePtr   = ZoneCompToPlant(Index)%FirstDemandSidePtr
              TempZoneCompToPlant(ArrayCount)%LastDemandSidePtr    = ZoneCompToPlant(Index)%LastDemandSidePtr
            ENDIF
          END DO

          ZoneCompToPlant%ZoneEqListNum       =        TempZoneCompToPlant%ZoneEqListNum
          ZoneCompToPlant%ZoneEqCompNum       =        TempZoneCompToPlant%ZoneEqCompNum
          ZoneCompToPlant%PlantLoopType       =        TempZoneCompToPlant%PlantLoopType
          ZoneCompToPlant%PlantLoopNum        =        TempZoneCompToPlant%PlantLoopNum
          ZoneCompToPlant%PlantLoopBranch     =        TempZoneCompToPlant%PlantLoopBranch
          ZoneCompToPlant%PlantLoopComp       =        TempZoneCompToPlant%PlantLoopComp
          ZoneCompToPlant%FirstDemandSidePtr  =        TempZoneCompToPlant%FirstDemandSidePtr
          ZoneCompToPlant%LastDemandSidePtr   =        TempZoneCompToPlant%LastDemandSidePtr

        ENDIF

        IF (SubEquipNum > 0) THEN
          TempZoneSubCompToPlant%ZoneEqListNum        = 0
          TempZoneSubCompToPlant%ZoneEqCompNum        = 0
          TempZoneSubCompToPlant%ZoneEqSubCompNum     = 0
          TempZoneSubCompToPlant%PlantLoopType        = 0
          TempZoneSubCompToPlant%PlantLoopNum         = 0
          TempZoneSubCompToPlant%PlantLoopBranch      = 0
          TempZoneSubCompToPlant%PlantLoopComp        = 0
          TempZoneSubCompToPlant%FirstDemandSidePtr   = 0
          TempZoneSubCompToPlant%LastDemandSidePtr    = 0

          ArrayCount = 0
          DO Index = 1, SubEquipNum
            Duplicate = .FALSE.
            DO TempIndex = 1,SubEquipNum
              IF (ZoneSubCompToPlant(Index)%ZoneEqListNum == TempZoneSubCompToPlant(TempIndex)%ZoneEqListNum   .AND. &
                  ZoneSubCompToPlant(Index)%ZoneEqCompNum == TempZoneSubCompToPlant(TempIndex)%ZoneEqCompNum .AND. &
                  ZoneSubCompToPlant(Index)%ZoneEqSubCompNum == TempZoneSubCompToPlant(TempIndex)%ZoneEqSubCompNum)THEN
                Duplicate = .TRUE.
                EXIT
              END IF
            END DO
            IF(.NOT. Duplicate)THEN
              ArrayCount = ArrayCount + 1
              TempZoneSubCompToPlant(ArrayCount)%ZoneEqListNum        = ZoneSubCompToPlant(Index)%ZoneEqListNum
              TempZoneSubCompToPlant(ArrayCount)%ZoneEqCompNum        = ZoneSubCompToPlant(Index)%ZoneEqCompNum
              TempZoneSubCompToPlant(ArrayCount)%ZoneEqSubCompNum     = ZoneSubCompToPlant(Index)%ZoneEqSubCompNum
              TempZoneSubCompToPlant(ArrayCount)%PlantLoopType        = ZoneSubCompToPlant(Index)%PlantLoopType
              TempZoneSubCompToPlant(ArrayCount)%PlantLoopNum         = ZoneSubCompToPlant(Index)%PlantLoopNum
              TempZoneSubCompToPlant(ArrayCount)%PlantLoopBranch      = ZoneSubCompToPlant(Index)%PlantLoopBranch
              TempZoneSubCompToPlant(ArrayCount)%PlantLoopComp        = ZoneSubCompToPlant(Index)%PlantLoopComp
              TempZoneSubCompToPlant(ArrayCount)%FirstDemandSidePtr   = ZoneSubCompToPlant(Index)%FirstDemandSidePtr
              TempZoneSubCompToPlant(ArrayCount)%LastDemandSidePtr    = ZoneSubCompToPlant(Index)%LastDemandSidePtr
            ENDIF
          END DO

          ZoneSubCompToPlant%ZoneEqListNum       =        TempZoneSubCompToPlant%ZoneEqListNum
          ZoneSubCompToPlant%ZoneEqCompNum       =        TempZoneSubCompToPlant%ZoneEqCompNum
          ZoneSubCompToPlant%ZoneEqSubCompNum    =        TempZoneSubCompToPlant%ZoneEqSubCompNum
          ZoneSubCompToPlant%PlantLoopType       =        TempZoneSubCompToPlant%PlantLoopType
          ZoneSubCompToPlant%PlantLoopNum        =        TempZoneSubCompToPlant%PlantLoopNum
          ZoneSubCompToPlant%PlantLoopBranch     =        TempZoneSubCompToPlant%PlantLoopBranch
          ZoneSubCompToPlant%PlantLoopComp       =        TempZoneSubCompToPlant%PlantLoopComp
          ZoneSubCompToPlant%FirstDemandSidePtr  =        TempZoneSubCompToPlant%FirstDemandSidePtr
          ZoneSubCompToPlant%LastDemandSidePtr   =        TempZoneSubCompToPlant%LastDemandSidePtr

        ENDIF

        IF (SubSubEquipNum > 0) THEN
          TempZoneSubSubCompToPlant%ZoneEqListNum         = 0
          TempZoneSubSubCompToPlant%ZoneEqCompNum         = 0
          TempZoneSubSubCompToPlant%ZoneEqSubCompNum      = 0
          TempZoneSubSubCompToPlant%ZoneEqSubSubCompNum   = 0
          TempZoneSubSubCompToPlant%PlantLoopType         = 0
          TempZoneSubSubCompToPlant%PlantLoopNum          = 0
          TempZoneSubSubCompToPlant%PlantLoopBranch       = 0
          TempZoneSubSubCompToPlant%PlantLoopComp         = 0
          TempZoneSubSubCompToPlant%FirstDemandSidePtr    = 0
          TempZoneSubSubCompToPlant%LastDemandSidePtr     = 0

          ArrayCount = 0
          DO Index = 1, SubSubEquipNum
            Duplicate = .FALSE.
            DO TempIndex = 1,SubSubEquipNum
              IF (ZoneSubSubCompToPlant(Index)%ZoneEqListNum == TempZoneSubSubCompToPlant(TempIndex)%ZoneEqListNum   .AND. &
                  ZoneSubSubCompToPlant(Index)%ZoneEqCompNum == TempZoneSubSubCompToPlant(TempIndex)%ZoneEqCompNum .AND. &
                  ZoneSubSubCompToPlant(Index)%ZoneEqSubCompNum == TempZoneSubSubCompToPlant(TempIndex)%ZoneEqSubCompNum.AND. &
                  ZoneSubSubCompToPlant(Index)%ZoneEqSubSubCompNum == TempZoneSubSubCompToPlant(TempIndex)%ZoneEqSubSubCompNum)THEN
                Duplicate = .TRUE.
                EXIT
              END IF
            END DO
            IF(.NOT. Duplicate)THEN
              ArrayCount = ArrayCount + 1
              TempZoneSubSubCompToPlant(ArrayCount)%ZoneEqListNum         = ZoneSubSubCompToPlant(Index)%ZoneEqListNum
              TempZoneSubSubCompToPlant(ArrayCount)%ZoneEqCompNum         = ZoneSubSubCompToPlant(Index)%ZoneEqCompNum
              TempZoneSubSubCompToPlant(ArrayCount)%ZoneEqSubCompNum      = ZoneSubSubCompToPlant(Index)%ZoneEqSubCompNum
              TempZoneSubSubCompToPlant(ArrayCount)%ZoneEqSubSubCompNum   = ZoneSubSubCompToPlant(Index)%ZoneEqSubSubCompNum
              TempZoneSubSubCompToPlant(ArrayCount)%PlantLoopType         = ZoneSubSubCompToPlant(Index)%PlantLoopType
              TempZoneSubSubCompToPlant(ArrayCount)%PlantLoopNum          = ZoneSubSubCompToPlant(Index)%PlantLoopNum
              TempZoneSubSubCompToPlant(ArrayCount)%PlantLoopBranch       = ZoneSubSubCompToPlant(Index)%PlantLoopBranch
              TempZoneSubSubCompToPlant(ArrayCount)%PlantLoopComp         = ZoneSubSubCompToPlant(Index)%PlantLoopComp
              TempZoneSubSubCompToPlant(ArrayCount)%FirstDemandSidePtr    = ZoneSubSubCompToPlant(Index)%FirstDemandSidePtr
              TempZoneSubSubCompToPlant(ArrayCount)%LastDemandSidePtr     = ZoneSubSubCompToPlant(Index)%LastDemandSidePtr
            ENDIF
          END DO

          ZoneSubSubCompToPlant%ZoneEqListNum       =        TempZoneSubSubCompToPlant%ZoneEqListNum
          ZoneSubSubCompToPlant%ZoneEqCompNum       =        TempZoneSubSubCompToPlant%ZoneEqCompNum
          ZoneSubSubCompToPlant%ZoneEqSubCompNum    =        TempZoneSubSubCompToPlant%ZoneEqSubCompNum
          ZoneSubSubCompToPlant%ZoneEqSubSubCompNum =        TempZoneSubSubCompToPlant%ZoneEqSubSubCompNum
          ZoneSubSubCompToPlant%PlantLoopType       =        TempZoneSubSubCompToPlant%PlantLoopType
          ZoneSubSubCompToPlant%PlantLoopNum        =        TempZoneSubSubCompToPlant%PlantLoopNum
          ZoneSubSubCompToPlant%PlantLoopBranch     =        TempZoneSubSubCompToPlant%PlantLoopBranch
          ZoneSubSubCompToPlant%PlantLoopComp       =        TempZoneSubSubCompToPlant%PlantLoopComp
          ZoneSubSubCompToPlant%FirstDemandSidePtr  =        TempZoneSubSubCompToPlant%FirstDemandSidePtr
          ZoneSubSubCompToPlant%LastDemandSidePtr   =        TempZoneSubSubCompToPlant%LastDemandSidePtr

        ENDIF

        IF (CompNum > 0) THEN
          TempAirSysCompToPlant%AirLoopNum                = 0
          TempAirSysCompToPlant%AirLoopBranch             = 0
          TempAirSysCompToPlant%AirLoopComp               = 0
          TempAirSysCompToPlant%PlantLoopType             = 0
          TempAirSysCompToPlant%PlantLoopNum              = 0
          TempAirSysCompToPlant%PlantLoopBranch           = 0
          TempAirSysCompToPlant%PlantLoopComp             = 0
          TempAirSysCompToPlant%FirstDemandSidePtr        = 0
          TempAirSysCompToPlant%LastDemandSidePtr         = 0

          ArrayCount = 0
          DO Index = 1, CompNum
            Duplicate = .FALSE.
            DO TempIndex = 1,CompNum
              IF (AirSysCompToPlant(Index)%AirLoopNum == TempAirSysCompToPlant(TempIndex)%AirLoopNum   .AND. &
                  AirSysCompToPlant(Index)%AirLoopBranch == TempAirSysCompToPlant(TempIndex)%AirLoopBranch .AND. &
                  AirSysCompToPlant(Index)%AirLoopComp == TempAirSysCompToPlant(TempIndex)%AirLoopComp)THEN
                Duplicate = .TRUE.
                EXIT
              END IF
            END DO
            IF(.NOT. Duplicate)THEN
              ArrayCount = ArrayCount + 1
              TempAirSysCompToPlant(ArrayCount)%AirLoopNum        = AirSysCompToPlant(Index)%AirLoopNum
              TempAirSysCompToPlant(ArrayCount)%AirLoopBranch     = AirSysCompToPlant(Index)%AirLoopBranch
              TempAirSysCompToPlant(ArrayCount)%AirLoopComp       = AirSysCompToPlant(Index)%AirLoopComp
              TempAirSysCompToPlant(ArrayCount)%PlantLoopType     = AirSysCompToPlant(Index)%PlantLoopType
              TempAirSysCompToPlant(ArrayCount)%PlantLoopNum      = AirSysCompToPlant(Index)%PlantLoopNum
              TempAirSysCompToPlant(ArrayCount)%PlantLoopBranch   = AirSysCompToPlant(Index)%PlantLoopBranch
              TempAirSysCompToPlant(ArrayCount)%PlantLoopComp     = AirSysCompToPlant(Index)%PlantLoopComp
              TempAirSysCompToPlant(ArrayCount)%FirstDemandSidePtr= AirSysCompToPlant(Index)%FirstDemandSidePtr
              TempAirSysCompToPlant(ArrayCount)%LastDemandSidePtr = AirSysCompToPlant(Index)%LastDemandSidePtr
            END IF
          END DO

          AirSysCompToPlant%AirLoopNum                = TempAirSysCompToPlant%AirLoopNum
          AirSysCompToPlant%AirLoopBranch             = TempAirSysCompToPlant%AirLoopBranch
          AirSysCompToPlant%AirLoopComp               = TempAirSysCompToPlant%AirLoopComp
          AirSysCompToPlant%PlantLoopType             = TempAirSysCompToPlant%PlantLoopType
          AirSysCompToPlant%PlantLoopNum              = TempAirSysCompToPlant%PlantLoopNum
          AirSysCompToPlant%PlantLoopBranch           = TempAirSysCompToPlant%PlantLoopBranch
          AirSysCompToPlant%PlantLoopComp             = TempAirSysCompToPlant%PlantLoopComp
          AirSysCompToPlant%FirstDemandSidePtr        = TempAirSysCompToPlant%FirstDemandSidePtr
          AirSysCompToPlant%LastDemandSidePtr         = TempAirSysCompToPlant%LastDemandSidePtr

        ENDIF

        IF (SubCompNum > 0) THEN
          TempAirSysSubCompToPlant%AirLoopNum             = 0
          TempAirSysSubCompToPlant%AirLoopBranch          = 0
          TempAirSysSubCompToPlant%AirLoopComp            = 0
          TempAirSysSubCompToPlant%AirLoopSubComp         = 0
          TempAirSysSubCompToPlant%PlantLoopType          = 0
          TempAirSysSubCompToPlant%PlantLoopNum           = 0
          TempAirSysSubCompToPlant%PlantLoopBranch        = 0
          TempAirSysSubCompToPlant%PlantLoopComp          = 0
          TempAirSysSubCompToPlant%FirstDemandSidePtr     = 0
          TempAirSysSubCompToPlant%LastDemandSidePtr      = 0

          ArrayCount = 0
          DO Index = 1, SubCompNum
            Duplicate = .FALSE.
            DO TempIndex = 1,SubCompNum
              IF (AirSysSubCompToPlant(Index)%AirLoopNum == TempAirSysSubCompToPlant(TempIndex)%AirLoopNum   .AND. &
                  AirSysSubCompToPlant(Index)%AirLoopBranch == TempAirSysSubCompToPlant(TempIndex)%AirLoopBranch .AND. &
                  AirSysSubCompToPlant(Index)%AirLoopComp == TempAirSysSubCompToPlant(TempIndex)%AirLoopComp .AND. &
                  AirSysSubCompToPlant(Index)%AirLoopSubComp == TempAirSysSubCompToPlant(TempIndex)%AirLoopSubComp)THEN
                Duplicate = .TRUE.
                EXIT
              END IF
            END DO
            IF(.NOT. Duplicate)THEN
              ArrayCount = ArrayCount + 1
              TempAirSysSubCompToPlant(ArrayCount)%AirLoopNum        = AirSysSubCompToPlant(Index)%AirLoopNum
              TempAirSysSubCompToPlant(ArrayCount)%AirLoopBranch     = AirSysSubCompToPlant(Index)%AirLoopBranch
              TempAirSysSubCompToPlant(ArrayCount)%AirLoopComp       = AirSysSubCompToPlant(Index)%AirLoopComp
              TempAirSysSubCompToPlant(ArrayCount)%AirLoopSubComp    = AirSysSubCompToPlant(Index)%AirLoopSubComp
              TempAirSysSubCompToPlant(ArrayCount)%PlantLoopType     = AirSysSubCompToPlant(Index)%PlantLoopType
              TempAirSysSubCompToPlant(ArrayCount)%PlantLoopNum      = AirSysSubCompToPlant(Index)%PlantLoopNum
              TempAirSysSubCompToPlant(ArrayCount)%PlantLoopBranch   = AirSysSubCompToPlant(Index)%PlantLoopBranch
              TempAirSysSubCompToPlant(ArrayCount)%PlantLoopComp     = AirSysSubCompToPlant(Index)%PlantLoopComp
              TempAirSysSubCompToPlant(ArrayCount)%FirstDemandSidePtr= AirSysSubCompToPlant(Index)%FirstDemandSidePtr
              TempAirSysSubCompToPlant(ArrayCount)%LastDemandSidePtr = AirSysSubCompToPlant(Index)%LastDemandSidePtr
            END IF
          END DO

          AirSysSubCompToPlant%AirLoopNum                = TempAirSysSubCompToPlant%AirLoopNum
          AirSysSubCompToPlant%AirLoopBranch             = TempAirSysSubCompToPlant%AirLoopBranch
          AirSysSubCompToPlant%AirLoopComp               = TempAirSysSubCompToPlant%AirLoopComp
          AirSysSubCompToPlant%AirLoopSubComp            = TempAirSysSubCompToPlant%AirLoopSubComp
          AirSysSubCompToPlant%PlantLoopType             = TempAirSysSubCompToPlant%PlantLoopType
          AirSysSubCompToPlant%PlantLoopNum              = TempAirSysSubCompToPlant%PlantLoopNum
          AirSysSubCompToPlant%PlantLoopBranch           = TempAirSysSubCompToPlant%PlantLoopBranch
          AirSysSubCompToPlant%PlantLoopComp             = TempAirSysSubCompToPlant%PlantLoopComp
          AirSysSubCompToPlant%FirstDemandSidePtr        = TempAirSysSubCompToPlant%FirstDemandSidePtr
          AirSysSubCompToPlant%LastDemandSidePtr         = TempAirSysSubCompToPlant%LastDemandSidePtr

        ENDIF

        IF (SubSubCompNum > 0) THEN
          TempAirSysSubSubCompToPlant%AirLoopNum          = 0
          TempAirSysSubSubCompToPlant%AirLoopBranch       = 0
          TempAirSysSubSubCompToPlant%AirLoopComp         = 0
          TempAirSysSubSubCompToPlant%AirLoopSubComp      = 0
          TempAirSysSubSubCompToPlant%AirLoopSubSubComp   = 0
          TempAirSysSubSubCompToPlant%PlantLoopType       = 0
          TempAirSysSubSubCompToPlant%PlantLoopNum        = 0
          TempAirSysSubSubCompToPlant%PlantLoopBranch     = 0
          TempAirSysSubSubCompToPlant%PlantLoopComp       = 0
          TempAirSysSubSubCompToPlant%FirstDemandSidePtr  = 0
          TempAirSysSubSubCompToPlant%LastDemandSidePtr   = 0

          ArrayCount = 0
          DO Index = 1, SubSubCompNum
            Duplicate = .FALSE.
            DO TempIndex = 1,SubSubCompNum
              IF (AirSysSubSubCompToPlant(Index)%AirLoopNum == TempAirSysSubSubCompToPlant(TempIndex)%AirLoopNum   .AND. &
                  AirSysSubSubCompToPlant(Index)%AirLoopBranch == TempAirSysSubSubCompToPlant(TempIndex)%AirLoopBranch .AND. &
                  AirSysSubSubCompToPlant(Index)%AirLoopComp == TempAirSysSubSubCompToPlant(TempIndex)%AirLoopComp .AND. &
                  AirSysSubSubCompToPlant(Index)%AirLoopSubComp == TempAirSysSubSubCompToPlant(TempIndex)%AirLoopSubComp .AND. &
                  AirSysSubSubCompToPlant(Index)%AirLoopSubSubComp == TempAirSysSubSubCompToPlant(TempIndex)%AirLoopSubSubComp)THEN
                Duplicate = .TRUE.
                EXIT
              END IF
            END DO
            IF(.NOT. Duplicate)THEN
              ArrayCount = ArrayCount + 1
              TempAirSysSubSubCompToPlant(ArrayCount)%AirLoopNum        = AirSysSubSubCompToPlant(Index)%AirLoopNum
              TempAirSysSubSubCompToPlant(ArrayCount)%AirLoopBranch     = AirSysSubSubCompToPlant(Index)%AirLoopBranch
              TempAirSysSubSubCompToPlant(ArrayCount)%AirLoopComp       = AirSysSubSubCompToPlant(Index)%AirLoopComp
              TempAirSysSubSubCompToPlant(ArrayCount)%AirLoopSubComp    = AirSysSubSubCompToPlant(Index)%AirLoopSubComp
              TempAirSysSubSubCompToPlant(ArrayCount)%AirLoopSubSubComp = AirSysSubSubCompToPlant(Index)%AirLoopSubSubComp
              TempAirSysSubSubCompToPlant(ArrayCount)%PlantLoopType     = AirSysSubSubCompToPlant(Index)%PlantLoopType
              TempAirSysSubSubCompToPlant(ArrayCount)%PlantLoopNum      = AirSysSubSubCompToPlant(Index)%PlantLoopNum
              TempAirSysSubSubCompToPlant(ArrayCount)%PlantLoopBranch   = AirSysSubSubCompToPlant(Index)%PlantLoopBranch
              TempAirSysSubSubCompToPlant(ArrayCount)%PlantLoopComp     = AirSysSubSubCompToPlant(Index)%PlantLoopComp
              TempAirSysSubSubCompToPlant(ArrayCount)%FirstDemandSidePtr= AirSysSubSubCompToPlant(Index)%FirstDemandSidePtr
              TempAirSysSubSubCompToPlant(ArrayCount)%LastDemandSidePtr = AirSysSubSubCompToPlant(Index)%LastDemandSidePtr
            END IF
          END DO

          AirSysSubSubCompToPlant%AirLoopNum                = TempAirSysSubSubCompToPlant%AirLoopNum
          AirSysSubSubCompToPlant%AirLoopBranch             = TempAirSysSubSubCompToPlant%AirLoopBranch
          AirSysSubSubCompToPlant%AirLoopComp               = TempAirSysSubSubCompToPlant%AirLoopComp
          AirSysSubSubCompToPlant%AirLoopSubComp            = TempAirSysSubSubCompToPlant%AirLoopSubComp
          AirSysSubSubCompToPlant%AirLoopSubSubComp         = TempAirSysSubSubCompToPlant%AirLoopSubSubComp
          AirSysSubSubCompToPlant%PlantLoopType             = TempAirSysSubSubCompToPlant%PlantLoopType
          AirSysSubSubCompToPlant%PlantLoopNum              = TempAirSysSubSubCompToPlant%PlantLoopNum
          AirSysSubSubCompToPlant%PlantLoopBranch           = TempAirSysSubSubCompToPlant%PlantLoopBranch
          AirSysSubSubCompToPlant%PlantLoopComp             = TempAirSysSubSubCompToPlant%PlantLoopComp
          AirSysSubSubCompToPlant%FirstDemandSidePtr        = TempAirSysSubSubCompToPlant%FirstDemandSidePtr
          AirSysSubSubCompToPlant%LastDemandSidePtr         = TempAirSysSubSubCompToPlant%LastDemandSidePtr

        ENDIF

        ! 2. Find Supply Side loop for every demand side component
                !The demand side components only need to know what supply side loop
                !they are connected to.  The input and plant data structure will
                !force the loop numbers to be the same.

        ! 3. Find Demand Side Component Corresponding to Supply Side Component
        DO PlantLoopNum = 1, NumPlantLoops
          DO BranchNum =1, VentRepPlantSupplySide(PlantLoopNum)%TotalBranches
            DO CompNum =1, VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%TotalComponents
              CompType = VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
              CompName = VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
              CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
              VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%LoopType  = MatchLoopType
              VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%LoopNum   = MatchLoop
              VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%BranchNum = MatchBranch
              VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%CompNum   = MatchComp
            END DO
          END DO
        END DO

        DO PlantLoopNum = 1, NumCondLoops
          DO BranchNum =1, VentRepCondSupplySide(PlantLoopNum)%TotalBranches
            DO CompNum =1, VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%TotalComponents
              CompType = VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
              CompName = VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
              CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
              VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%LoopType  = MatchLoopType
              VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%LoopNum   = MatchLoop
              VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%BranchNum = MatchBranch
              VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%ConnectPlant%CompNum   = MatchComp
            END DO
          END DO
        END DO
      END DO
    END DO  ! Controlled Zone Loop


        !4.  Now Load all of the plant supply/demand side connections in a single array with pointers from the
        !    connection arrays (ZoneCompToPlant, ZoneSubCompToPlant, ZoneSubSubCompToPlant, AirSysCompToPlant, etc.)
    IF (ALLOCATED(ZoneCompToPlant)) THEN
      NumZoneConnectComps         = SIZE(ZoneCompToPlant)
    ELSE
      NumZoneConnectComps         = 0
    ENDIF
    IF (ALLOCATED(ZoneSubCompToPlant)) THEN
      NumZoneConnectSubComps      = SIZE(ZoneSubCompToPlant)
    ELSE
      NumZoneConnectSubComps      = 0
    ENDIF
    IF (ALLOCATED(ZoneSubSubCompToPlant)) THEN
      NumZoneConnectSubSubComps   = SIZE(ZoneSubSubCompToPlant)
    ELSE
      NumZoneConnectSubSubComps   = 0
    ENDIF
    IF (ALLOCATED(AirSysCompToPlant)) THEN
      NumAirSysConnectComps       = SIZE(AirSysCompToPlant)
    ELSE
      NumAirSysConnectComps       = 0
    ENDIF
    IF (ALLOCATED(AirSysSubCompToPlant)) THEN
      NumAirSysConnectSubComps    = SIZE(AirSysSubCompToPlant)
    ELSE
      NumAirSysConnectSubComps    = 0
    ENDIF
    IF (ALLOCATED(AirSysSubSubCompToPlant)) THEN
      NumAirSysConnectSubSubComps = SIZE(AirSysSubSubCompToPlant)
    ELSE
      NumAirSysConnectSubSubComps = 0
    ENDIF
    OneTimeFlag = .FALSE.

    ArrayCount = 0
    DO CompNum = 1,NumZoneConnectComps
      LoopType      = ZoneCompToPlant(CompNum)%PlantLoopType
      LoopNum       = ZoneCompToPlant(CompNum)%PlantLoopNum
      FirstIndex = ArrayCount + 1
      LoopCount = 1

      IF(LoopType > 0 .AND. LoopNum > 0)THEN
        CALL FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
      END IF

      LastIndex = ArrayCount
      IF(FirstIndex > LastIndex) FirstIndex = LastIndex
      IF(ConnectionFlag)THEN
        ZoneCompToPlant(CompNum)%FirstDemandSidePtr = FirstIndex
        ZoneCompToPlant(CompNum)%LastDemandSidePtr = LastIndex
      ENDIF
    END DO

    DO SubCompNum = 1,NumZoneConnectSubComps
      LoopType      = ZoneSubCompToPlant(SubCompNum)%PlantLoopType
      LoopNum       = ZoneSubCompToPlant(SubCompNum)%PlantLoopNum
      FirstIndex = ArrayCount + 1
      LoopCount = 1

      IF(LoopType > 0 .AND. LoopNum > 0)THEN
        CALL FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
      END IF

      LastIndex = ArrayCount
      IF(FirstIndex > LastIndex) FirstIndex = LastIndex
      IF(ConnectionFlag)THEN
        ZoneSubCompToPlant(SubCompNum)%FirstDemandSidePtr = FirstIndex
        ZoneSubCompToPlant(SubCompNum)%LastDemandSidePtr = LastIndex
      ENDIF
    END DO

    DO SubSubCompNum = 1,NumZoneConnectSubSubComps
      LoopType      = ZoneSubSubCompToPlant(SubSubCompNum)%PlantLoopType
      LoopNum       = ZoneSubSubCompToPlant(SubSubCompNum)%PlantLoopNum
      FirstIndex = ArrayCount + 1
      LoopCount = 1

      IF(LoopType > 0 .AND. LoopNum > 0)THEN
        CALL FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
      END IF

      LastIndex = ArrayCount
      IF(FirstIndex > LastIndex) FirstIndex = LastIndex
      IF(ConnectionFlag)THEN
        ZoneSubSubCompToPlant(SubSubCompNum)%FirstDemandSidePtr = FirstIndex
        ZoneSubSubCompToPlant(SubSubCompNum)%LastDemandSidePtr = LastIndex
      ENDIF
    END DO
    DO CompNum = 1,NumAirSysConnectComps
      LoopType      = AirSysCompToPlant(CompNum)%PlantLoopType
      LoopNum       = AirSysCompToPlant(CompNum)%PlantLoopNum
      FirstIndex = ArrayCount + 1
      LoopCount = 1

      IF(LoopType > 0 .AND. LoopNum > 0)THEN
        CALL FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
      END IF

      LastIndex = ArrayCount
      IF(FirstIndex > LastIndex) FirstIndex = LastIndex
      IF(ConnectionFlag)THEN
        AirSysCompToPlant(CompNum)%FirstDemandSidePtr = FirstIndex
        AirSysCompToPlant(CompNum)%LastDemandSidePtr = LastIndex
      ENDIF
    END DO

    DO SubCompNum = 1,NumAirSysConnectSubComps
      LoopType      = AirSysSubCompToPlant(SubCompNum)%PlantLoopType
      LoopNum       = AirSysSubCompToPlant(SubCompNum)%PlantLoopNum
      FirstIndex = ArrayCount + 1
      LoopCount = 1

      IF(LoopType > 0 .AND. LoopNum > 0)THEN
        CALL FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
      END IF

      LastIndex = ArrayCount
      IF(FirstIndex > LastIndex) FirstIndex = LastIndex
      IF(ConnectionFlag)THEN
        AirSysSubCompToPlant(SubCompNum)%FirstDemandSidePtr = FirstIndex
        AirSysSubCompToPlant(SubCompNum)%LastDemandSidePtr = LastIndex
      ENDIF
    END DO

    DO SubSubCompNum = 1,NumAirSysConnectSubSubComps
      LoopType      = AirSysSubSubCompToPlant(SubSubCompNum)%PlantLoopType
      LoopNum       = AirSysSubSubCompToPlant(SubSubCompNum)%PlantLoopNum
      FirstIndex = ArrayCount + 1
      LoopCount = 1

      IF(LoopType > 0 .AND. LoopNum > 0)THEN
        CALL FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
      END IF

      LastIndex = ArrayCount
      IF(FirstIndex > LastIndex) FirstIndex = LastIndex
      IF(ConnectionFlag)THEN
        AirSysSubSubCompToPlant(SubSubCompNum)%FirstDemandSidePtr = FirstIndex
        AirSysSubSubCompToPlant(SubSubCompNum)%LastDemandSidePtr = LastIndex
      ENDIF
    END DO

    OneTimeFlag = .FALSE.

  END IF

! On every iteration, load the air loop energy data
  DO AirLoopNum =1, NumPrimaryAirSys
    DO BranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
      DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
        DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          VarType  = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType
          VarIndex = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%CurMeterReading = &
              GetInternalVariableValue(VarType,VarIndex)
        END DO
        DO SubCompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumSubcomps
          DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumMeteredVars
             VarType  = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                MeteredVar(VarNum)%ReportVarType
             VarIndex = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                MeteredVar(VarNum)%ReportVarIndex
             PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                MeteredVar(VarNum)%CurMeterReading = &
                 GetInternalVariableValue(VarType,VarIndex)
          END DO
          DO SubSubCompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumSubSubComps
            DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%NumMeteredVars
              VarType  = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarType
              VarIndex = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarIndex
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%CurMeterReading = &
                  GetInternalVariableValue(VarType,VarIndex)
            END DO
          END DO
        END DO
      END DO
    END DO
  END DO

! On every iteration, load the zone equipment energy data
  DO ListNum = 1, NumofZones
    IF (.not. ZoneEquipConfig(ListNum)%IsControlled) CYCLE
    DO CompNum = 1,ZoneEquipList(ListNum)%NumOfEquipTypes
      DO VarNum =1,ZoneEquipList(ListNum)%EquipData(CompNum)%NumMeteredVars
        VarType  = ZoneEquipList(ListNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarType
        VarIndex = ZoneEquipList(ListNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarIndex
        ZoneEquipList(ListNum)%EquipData(CompNum)%MeteredVar(VarNum)%CurMeterReading = &
            GetInternalVariableValue(VarType,VarIndex)
      END DO
      DO SubCompNum = 1, ZoneEquipList(ListNum)%EquipData(CompNum)%NumSubEquip
        DO VarNum = 1, ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%NumMeteredVars
           VarType  = ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarType
           VarIndex = ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarIndex
           ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%CurMeterReading = &
               GetInternalVariableValue(VarType,VarIndex)
        END DO
        DO SubSubCompNum = 1, ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%NumSubSubEquip
          DO VarNum = 1, ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
             SubSubEquipData(SubSubCompNum)%NumMeteredVars
            VarType  = ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%MeteredVar(VarNum)%ReportVarType
            VarIndex = ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%MeteredVar(VarNum)%ReportVarIndex
            ZoneEquipList(ListNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%MeteredVar(VarNum)%CurMeterReading = &
                GetInternalVariableValue(VarType,VarIndex)    !Sankar Corrected zone array
          END DO
        END DO
      END DO
    END DO
  END DO

! On every iteration, load the Plant Supply Side Data
  DO PlantLoopNum = 1, NumPlantLoops
    DO BranchNum =1, VentRepPlantSupplySide(PlantLoopNum)%TotalBranches
      DO CompNum =1, VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%TotalComponents
        DO VarNum = 1, VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          VarType  = VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType
          VarIndex = VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex
          VentRepPlantSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%CurMeterReading = &
              GetInternalVariableValue(VarType,VarIndex)
        END DO
      END DO
    END DO
  END DO

! On every iteration, load the Plant Demand Side Data
  DO PlantLoopNum = 1, NumPlantLoops
    DO BranchNum =1, VentRepPlantDemandSide(PlantLoopNum)%TotalBranches
      DO CompNum =1, VentRepPlantDemandSide(PlantLoopNum)%Branch(BranchNum)%TotalComponents
        DO VarNum = 1, VentRepPlantDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          VarType  = VentRepPlantDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType
          VarIndex = VentRepPlantDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex
          VentRepPlantDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%CurMeterReading = &
              GetInternalVariableValue(VarType,VarIndex)
        END DO
      END DO
    END DO
  END DO

! On every iteration, load the Condenser Supply Side Data
  DO PlantLoopNum = 1, NumCondLoops
    DO BranchNum =1, VentRepCondSupplySide(PlantLoopNum)%TotalBranches
      DO CompNum =1, VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%TotalComponents
        DO VarNum = 1, VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          VarType  = VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType
          VarIndex = VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex
          VentRepCondSupplySide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%CurMeterReading = &
              GetInternalVariableValue(VarType,VarIndex)
        END DO
      END DO
    END DO
  END DO

! On every iteration, load the Condenser Demand Side Data
  DO PlantLoopNum = 1, NumCondLoops
    DO BranchNum =1, VentRepCondDemandSide(PlantLoopNum)%TotalBranches
      DO CompNum =1, VentRepCondDemandSide(PlantLoopNum)%Branch(BranchNum)%TotalComponents
        DO VarNum = 1, VentRepCondDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          VarType  = VentRepCondDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType
          VarIndex = VentRepCondDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex
          VentRepCondDemandSide(PlantLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%CurMeterReading = &
              GetInternalVariableValue(VarType,VarIndex)
        END DO
      END DO
    END DO
  END DO

! initialize energy report variables

END SUBROUTINE InitEnergyReports

SUBROUTINE FindFirstLastPtr(LoopType,LoopNum,ArrayCount,LoopCount,ConnectionFlag)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the energy components of the data structures

          ! METHODOLOGY EMPLOYED:
          ! Once all compsets have been established (second iteration) find all components
          ! subcomponents, etc.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS

  INTEGER,INTENT(INOUT) :: ArrayCount
  INTEGER,INTENT(INOUT) :: LoopType
  INTEGER,INTENT(INOUT) :: LoopNum
  INTEGER,INTENT(INOUT) :: LoopCount
  LOGICAL,INTENT(INOUT) :: ConnectionFlag


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER    :: TypeComp = 1
  INTEGER, PARAMETER    :: TypeSubComp = 2
  INTEGER, PARAMETER    :: TypeSubSubComp = 3
  INTEGER, PARAMETER    :: EnergyTransfer = 1


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
  TYPE IdentifyLoop
    INTEGER :: LoopNum   = 0
    INTEGER :: LoopType  = 0
  END TYPE IdentifyLoop


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
TYPE (IdentifyLoop), SAVE, ALLOCATABLE, DIMENSION(:) :: LoopStack
TYPE (IdentifyLoop), SAVE, ALLOCATABLE, DIMENSION(:) :: TempLoopStack


  INTEGER :: BranchNum
  INTEGER :: Index
  INTEGER :: DemandSideLoopNum
  INTEGER :: DemandSideBranchNum
  INTEGER :: DemandSideCompNum
  INTEGER :: SupplySideCompNum
  INTEGER :: DemandSideLoopType
  LOGICAL,Save :: OneTimeFlag  = .True.  ! Flag set to make sure you initialize reports one time
  LOGICAL      :: Found
  integer :: countloop

  RETURN

  IF(OneTimeFlag)THEN
    ALLOCATE (LoopStack(MaxLoopArraySize))
    ALLOCATE (TempLoopStack(MaxLoopArraySize))
    ALLOCATE (DemandSideConnect(MaxCompArraySize))

    OneTimeFlag = .FALSE.

  END IF
    LoopStack%LoopNum = 0
    LoopStack%LoopType = 0

    TempLoopStack%LoopNum = 0
    TempLoopStack%LoopType = 0
    ConnectionFlag = .FALSE.
!    countloop=0
!    write(outputfiledebug,*) '1228=lt,lc,lnum,cflag,arrcnt',looptype,loopcount,loopnum,connectionflag,arraycount

      DO While (LoopCount > 0)
!        write(outputfiledebug,*) '1231==lt,lc,lnum,cflag,arrcnt',looptype,loopcount,loopnum,connectionflag,arraycount
!        write(outputfiledebug,*) 'loop=plname',trim(plantloop(loopnum)%name)
        LoopCount = LoopCount - 1
!        countloop=countloop+1
!        if (countloop > 100) exit
        IF (LoopType ==1)THEN
          DO BranchNum = 1, VentRepPlantSupplySide(LoopNum)%TotalBranches
            DO SupplySideCompNum = 1, VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents
              DemandSideLoopType    =   &
                 VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%LoopType
              DemandSideLoopNum     =   &
                 VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%LoopNum
              DemandSideBranchNum   =   &
                 VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%BranchNum
              DemandSideCompNum     =   &
                 VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%CompNum

                !If the connection is valid load the connection array
              IF(DemandSideLoopType == 1 .OR. DemandSideLoopType == 2)THEN
                ConnectionFlag = .TRUE.
                ArrayCount = ArrayCount + 1
                IF(ArrayCount > MaxCompArraySize) THEN
!                  ALLOCATE(TempDemandSideConnect(MaxCompArraySize*2))
                  ALLOCATE(TempDemandSideConnect(MaxCompArraySize+100))
                  TempDemandSideConnect(1:MaxCompArraySize) = DemandSideConnect(1:MaxCompArraySize)
                  DEALLOCATE(DemandSideConnect)
                  ALLOCATE(DemandSideConnect(MaxCompArraySize*2))
                  DemandSideConnect(1:MaxCompArraySize) = TempDemandSideConnect(1:MaxCompArraySize)
                  DEALLOCATE(TempDemandSideConnect)
!                  MaxCompArraySize=MaxCompArraySize*2
                  MaxCompArraySize=MaxCompArraySize+100
                END IF
                DemandSideConnect(ArrayCount)%LoopType = DemandSideLoopType
                DemandSideConnect(ArrayCount)%LoopNum  = DemandSideLoopNum
                DemandSideConnect(ArrayCount)%BranchNum = DemandSideBranchNum
                DemandSideConnect(ArrayCount)%CompNum = DemandSideCompNum

                found = .FALSE.
                write(outputfiledebug,*) '1271=lstacksize',size(loopstack)
                DO Index = 1, SIZE(LoopStack)
                  IF(DemandSideLoopNum ==  LoopStack(Index)%LoopNum .AND. &
                     DemandSideLoopType ==LoopStack(Index)%LoopType)THEN
                     found = .TRUE.
                     EXIT
                  END IF
                END DO
                IF(.NOT. found)THEN
                  LoopCount = LoopCount+1
 !       write(outputfiledebug,*) '1280=lc,mxsize',loopcount,maxlooparraysize
 !       write(outputfiledebug,*) '1281=dsloopnum,dslooptype',DemandSideLoopNum,DemandSideLoopType
                  IF(LoopCount > MaxLoopArraySize)THEN
!                    ALLOCATE(TempLoopStack(MaxLoopArraySize*2))
                    ALLOCATE(TempLoopStack(MaxLoopArraySize+100))
                    TempLoopStack(1:MaxLoopArraySize) = LoopStack(1:MaxLoopArraySize)
                    DEALLOCATE(LoopStack)
!                    ALLOCATE(LoopStack(MaxLoopArraySize*2))
                    ALLOCATE(LoopStack(MaxLoopArraySize+100))
                    LoopStack(1:MaxLoopArraySize) = TempLoopStack(1:MaxLoopArraySize)
                    DEALLOCATE(TempLoopStack)
!                    MaxLoopArraySize=MaxLoopArraySize*2
                    MaxLoopArraySize=MaxLoopArraySize+100
                  END IF
 !               write(outputfiledebug,*) '1294=lcnt,dsloopnum,dslooptype',loopcount,DemandSideLoopNum,DemandSideLoopType
                  LoopStack(LoopCount)%LoopNum = DemandSideLoopNum
                  LoopStack(LoopCount)%LoopType = DemandSideLoopType
                END IF
              END IF
            END DO
          END DO
        ELSEIF(LoopType == 2)THEN
          DO BranchNum = 1, VentRepCondSupplySide(LoopNum)%TotalBranches
            DO SupplySideCompNum = 1, VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents
              DemandSideLoopType =   &
                 VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%LoopType
              DemandSideLoopNum     =   &
                 VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%LoopNum
              DemandSideBranchNum   =   &
                 VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%BranchNum
              DemandSideCompNum     =   &
                 VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(SupplySideCompNum)%ConnectPlant%CompNum

                !If the connection is valid load the connection array
              IF(DemandSideLoopType == 1 .OR. DemandSideLoopType == 2)THEN
                 ConnectionFlag = .TRUE.
                 ArrayCount = ArrayCount + 1
                IF(ArrayCount > MaxCompArraySize)THEN
!                  ALLOCATE(TempDemandSideConnect(MaxCompArraySize*2))
                  ALLOCATE(TempDemandSideConnect(MaxCompArraySize+100))
                  TempDemandSideConnect(1:MaxCompArraySize) = DemandSideConnect(1:MaxCompArraySize)
                  DEALLOCATE(DemandSideConnect)
!                  ALLOCATE(DemandSideConnect(MaxCompArraySize*2))
                  ALLOCATE(DemandSideConnect(MaxCompArraySize+100))
                  DemandSideConnect(1:MaxCompArraySize) = TempDemandSideConnect(1:MaxCompArraySize)
                  DEALLOCATE(TempDemandSideConnect)
!                  MaxCompArraySize=MaxCompArraySize*2
                  MaxCompArraySize=MaxCompArraySize+100
                END IF
                DemandSideConnect(ArrayCount)%LoopType = DemandSideLoopType
                DemandSideConnect(ArrayCount)%LoopNum  = DemandSideLoopNum
                DemandSideConnect(ArrayCount)%BranchNum = DemandSideBranchNum
                DemandSideConnect(ArrayCount)%CompNum = DemandSideCompNum

                found = .FALSE.
                DO Index = 1, SIZE(LoopStack)
                  IF(DemandSideLoopNum ==  LoopStack(Index)%LoopNum .AND. &
                     DemandSideLoopType ==LoopStack(Index)%LoopType)THEN
                     found = .TRUE.
                     EXIT
                  END IF
                END DO
                IF(.NOT. found)THEN
                  LoopCount = LoopCount+1
 !       write(outputfiledebug,*) '1341=lcnt,arrsize',loopcount,maxlooparraysize
 !       write(outputfiledebug,*) '1342=lsloopnum,dslooptype',DemandSideLoopNum,DemandSideLoopType
                  IF(LoopCount > MaxLoopArraySize)THEN
!                    ALLOCATE(TempLoopStack(MaxLoopArraySize*2))
                    ALLOCATE(TempLoopStack(MaxLoopArraySize+100))
                    TempLoopStack(1:MaxLoopArraySize) = LoopStack(1:MaxLoopArraySize)
                    DEALLOCATE(LoopStack)
!                    ALLOCATE(LoopStack(MaxLoopArraySize*2))
                    ALLOCATE(LoopStack(MaxLoopArraySize+100))
                    LoopStack(1:MaxLoopArraySize) = TempLoopStack(1:MaxLoopArraySize)
                    DEALLOCATE(TempLoopStack)
!                    MaxLoopArraySize=MaxLoopArraySize*2
                    MaxLoopArraySize=MaxLoopArraySize+100
                  END IF
                  LoopStack(LoopCount)%LoopNum = DemandSideLoopNum
                  LoopStack(LoopCount)%LoopType = DemandSideLoopType
                END IF
              END IF
            END DO
          END DO
        ELSE
          write(outputfiledebug,*) '1361=error'
            !error
        END IF

            !now unload the LoopNum and LoopType arrays
        IF(LoopCount > 0)THEN
          LoopType  = LoopStack(LoopCount)%LoopType
          LoopNum   = LoopStack(LoopCount)%LoopNum
        END IF

      END DO !While loop
      RETURN

END SUBROUTINE FindFirstLastPtr

SUBROUTINE UpdateZoneCompPtrArray(Index,ListNum,AirDistUnitNum,PlantLoopType,PlantLoop,PlantBranch,PlantComp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update Zone Component pointers

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(OUT)        :: Index
    INTEGER, INTENT(IN)         :: ListNum
    INTEGER, INTENT(IN)         :: AirDistUnitNum
    INTEGER, INTENT(IN)         :: PlantLoopType
    INTEGER, INTENT(IN)         :: PlantLoop
    INTEGER, INTENT(IN)         :: PlantBranch
    INTEGER, INTENT(IN)         :: PlantComp
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
    LOGICAL, SAVE   :: OneTimeFlag = .TRUE.
    INTEGER         :: OldArrayLimit
    INTEGER, SAVE   :: ArrayLimit = 100
    INTEGER, SAVE   :: ArrayCounter = 1


    IF (OneTimeFlag)THEN
      ALLOCATE(ZoneCompToPlant(ArrayLimit))
      ZoneCompToPlant%ZoneEqListNum = 0
      ZoneCompToPlant%ZoneEqCompNum = 0
      ZoneCompToPlant%PlantLoopType = 0
      ZoneCompToPlant%PlantLoopNum = 0
      ZoneCompToPlant%PlantLoopBranch = 0
      ZoneCompToPlant%PlantLoopComp = 0
      ZoneCompToPlant%FirstDemandSidePtr = 0
      ZoneCompToPlant%LastDemandSidePtr = 0

      ALLOCATE(TempZoneCompToPlant(ArrayLimit))
      TempZoneCompToPlant%ZoneEqListNum = 0
      TempZoneCompToPlant%ZoneEqCompNum = 0
      TempZoneCompToPlant%PlantLoopType = 0
      TempZoneCompToPlant%PlantLoopNum = 0
      TempZoneCompToPlant%PlantLoopBranch = 0
      TempZoneCompToPlant%PlantLoopComp = 0
      TempZoneCompToPlant%FirstDemandSidePtr = 0
      TempZoneCompToPlant%LastDemandSidePtr = 0
      OneTimeFlag = .FALSE.
    END IF

    IF(ArrayCounter < ArrayLimit)THEN
      Index = ArrayCounter
      ZoneCompToPlant(Index)%ZoneEqListNum = ListNum
      ZoneCompToPlant(Index)%ZoneEqCompNum = AirDistUnitNum
      ZoneCompToPlant(Index)%PlantLoopType = PlantLoopType
      ZoneCompToPlant(Index)%PlantLoopNum = PlantLoop
      ZoneCompToPlant(Index)%PlantLoopBranch = PlantBranch
      ZoneCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    ELSE
      TempZoneCompToPlant = ZoneCompToPlant
      DEALLOCATE(ZoneCompToPlant)
      OldArrayLimit = ArrayLimit
      ArrayLimit = ArrayLimit*2
      ALLOCATE(ZoneCompToPlant(ArrayLimit))
      ZoneCompToPlant%ZoneEqListNum = 0
      ZoneCompToPlant%ZoneEqCompNum = 0
      ZoneCompToPlant%PlantLoopType = 0
      ZoneCompToPlant%PlantLoopNum = 0
      ZoneCompToPlant%PlantLoopBranch = 0
      ZoneCompToPlant%PlantLoopComp = 0
      ZoneCompToPlant%FirstDemandSidePtr = 0
      ZoneCompToPlant%LastDemandSidePtr = 0
      ZoneCompToPlant(1:OldArrayLimit) = TempZoneCompToPlant(1:OldArrayLimit)
      DEALLOCATE(TempZoneCompToPlant)
      ALLOCATE(TempZoneCompToPlant(ArrayLimit))
      TempZoneCompToPlant%ZoneEqListNum = 0
      TempZoneCompToPlant%ZoneEqCompNum = 0
      TempZoneCompToPlant%PlantLoopType = 0
      TempZoneCompToPlant%PlantLoopNum = 0
      TempZoneCompToPlant%PlantLoopBranch = 0
      TempZoneCompToPlant%PlantLoopComp = 0
      TempZoneCompToPlant%FirstDemandSidePtr = 0
      TempZoneCompToPlant%LastDemandSidePtr = 0

      Index = ArrayCounter
      ZoneCompToPlant(Index)%ZoneEqListNum = ListNum
      ZoneCompToPlant(Index)%ZoneEqCompNum = AirDistUnitNum
      ZoneCompToPlant(Index)%PlantLoopType = PlantLoopType
      ZoneCompToPlant(Index)%PlantLoopNum = PlantLoop
      ZoneCompToPlant(Index)%PlantLoopBranch = PlantBranch
      ZoneCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    END IF
  RETURN
END SUBROUTINE UpdateZoneCompPtrArray

SUBROUTINE UpdateZoneSubCompPtrArray(Index,ListNum,AirDistUnitNum,SubCompNum,PlantLoopType,PlantLoop,PlantBranch,PlantComp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update Zone Sub Component Pointer Array

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(OUT)        :: Index
    INTEGER, INTENT(IN)         :: ListNum
    INTEGER, INTENT(IN)         :: AirDistUnitNum
    INTEGER, INTENT(IN)         :: SubCompNum
    INTEGER, INTENT(IN)         :: PlantLoopType
    INTEGER, INTENT(IN)         :: PlantLoop
    INTEGER, INTENT(IN)         :: PlantBranch
    INTEGER, INTENT(IN)         :: PlantComp
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
    LOGICAL, SAVE   :: OneTimeFlag = .TRUE.
    INTEGER         :: OldArrayLimit
    INTEGER, SAVE   :: ArrayLimit = 100
    INTEGER, SAVE   :: ArrayCounter = 1


    IF (OneTimeFlag)THEN
      ALLOCATE(ZoneSubCompToPlant(ArrayLimit))
      ZoneSubCompToPlant%ZoneEqListNum = 0
      ZoneSubCompToPlant%ZoneEqCompNum = 0
      ZoneSubCompToPlant%ZoneEqSubCompNum = 0
      ZoneSubCompToPlant%PlantLoopType = 0
      ZoneSubCompToPlant%PlantLoopNum = 0
      ZoneSubCompToPlant%PlantLoopBranch = 0
      ZoneSubCompToPlant%PlantLoopComp = 0
      ZoneSubCompToPlant%FirstDemandSidePtr = 0
      ZoneSubCompToPlant%LastDemandSidePtr = 0
      ALLOCATE(TempZoneSubCompToPlant(ArrayLimit))
      TempZoneSubCompToPlant%ZoneEqListNum = 0
      TempZoneSubCompToPlant%ZoneEqCompNum = 0
      TempZoneSubCompToPlant%ZoneEqSubCompNum = 0
      TempZoneSubCompToPlant%PlantLoopType = 0
      TempZoneSubCompToPlant%PlantLoopNum = 0
      TempZoneSubCompToPlant%PlantLoopBranch = 0
      TempZoneSubCompToPlant%PlantLoopComp = 0
      TempZoneSubCompToPlant%FirstDemandSidePtr = 0
      TempZoneSubCompToPlant%LastDemandSidePtr = 0
      OneTimeFlag = .FALSE.
    END IF

    IF(ArrayCounter < ArrayLimit)THEN
      Index = ArrayCounter
      ZoneSubCompToPlant(Index)%ZoneEqListNum = ListNum
      ZoneSubCompToPlant(Index)%ZoneEqCompNum = AirDistUnitNum
      ZoneSubCompToPlant(Index)%ZoneEqSubCompNum = SubCompNum
      ZoneSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      ZoneSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      ZoneSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      ZoneSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    ELSE
      TempZoneSubCompToPlant = ZoneSubCompToPlant
      DEALLOCATE(ZoneSubCompToPlant)
      OldArrayLimit = ArrayLimit
      ArrayLimit = ArrayLimit*2
      ALLOCATE(ZoneSubCompToPlant(ArrayLimit))
      ZoneSubCompToPlant%ZoneEqListNum = 0
      ZoneSubCompToPlant%ZoneEqCompNum = 0
      ZoneSubCompToPlant%ZoneEqSubCompNum = 0
      ZoneSubCompToPlant%PlantLoopType = 0
      ZoneSubCompToPlant%PlantLoopNum = 0
      ZoneSubCompToPlant%PlantLoopBranch = 0
      ZoneSubCompToPlant%PlantLoopComp = 0
      ZoneSubCompToPlant%FirstDemandSidePtr = 0
      ZoneSubCompToPlant%LastDemandSidePtr = 0
      ZoneSubCompToPlant(1:OldArrayLimit) = TempZoneSubCompToPlant(1:OldArrayLimit)
      DEALLOCATE(TempZoneSubCompToPlant)
      ALLOCATE(TempZoneSubCompToPlant(ArrayLimit))
      TempZoneSubCompToPlant%ZoneEqListNum = 0
      TempZoneSubCompToPlant%ZoneEqCompNum = 0
      TempZoneSubCompToPlant%ZoneEqSubCompNum = 0
      TempZoneSubCompToPlant%PlantLoopType = 0
      TempZoneSubCompToPlant%PlantLoopNum = 0
      TempZoneSubCompToPlant%PlantLoopBranch = 0
      TempZoneSubCompToPlant%PlantLoopComp = 0
      TempZoneSubCompToPlant%FirstDemandSidePtr = 0
      TempZoneSubCompToPlant%LastDemandSidePtr = 0

      Index = ArrayCounter
      ZoneSubCompToPlant(Index)%ZoneEqListNum = ListNum
      ZoneSubCompToPlant(Index)%ZoneEqCompNum = AirDistUnitNum
      ZoneSubCompToPlant(Index)%ZoneEqSubCompNum = SubCompNum
      ZoneSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      ZoneSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      ZoneSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      ZoneSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    END IF
  RETURN
END SUBROUTINE UpdateZoneSubCompPtrArray

SUBROUTINE UpdateZoneSubSubCompPtrArray(Index,ListNum,AirDistUnitNum,SubCompNum,SubSubCompNum,PlantLoopType,  &
                                        PlantLoop,PlantBranch,PlantComp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update Zone Sub Component Pointer Array

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(OUT)        :: Index
    INTEGER, INTENT(IN)         :: ListNum
    INTEGER, INTENT(IN)         :: AirDistUnitNum
    INTEGER, INTENT(IN)         :: SubCompNum
    INTEGER, INTENT(IN)         :: SubSubCompNum
    INTEGER, INTENT(IN)         :: PlantLoopType
    INTEGER, INTENT(IN)         :: PlantLoop
    INTEGER, INTENT(IN)         :: PlantBranch
    INTEGER, INTENT(IN)         :: PlantComp
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
    LOGICAL, SAVE   :: OneTimeFlag = .TRUE.
    INTEGER         :: OldArrayLimit
    INTEGER, SAVE   :: ArrayLimit = 100
    INTEGER, SAVE   :: ArrayCounter = 1


    IF (OneTimeFlag)THEN
      ALLOCATE(ZoneSubSubCompToPlant(ArrayLimit))
      ZoneSubSubCompToPlant%ZoneEqListNum = 0
      ZoneSubSubCompToPlant%ZoneEqCompNum = 0
      ZoneSubSubCompToPlant%ZoneEqSubCompNum = 0
      ZoneSubSubCompToPlant%ZoneEqSubSubCompNum = 0
      ZoneSubSubCompToPlant%PlantLoopType = 0
      ZoneSubSubCompToPlant%PlantLoopNum = 0
      ZoneSubSubCompToPlant%PlantLoopBranch = 0
      ZoneSubSubCompToPlant%PlantLoopComp = 0
      ZoneSubSubCompToPlant%FirstDemandSidePtr = 0
      ZoneSubSubCompToPlant%LastDemandSidePtr = 0
      ALLOCATE(TempZoneSubSubCompToPlant(ArrayLimit))
      TempZoneSubSubCompToPlant%ZoneEqListNum = 0
      TempZoneSubSubCompToPlant%ZoneEqCompNum = 0
      TempZoneSubSubCompToPlant%ZoneEqSubCompNum = 0
      TempZoneSubSubCompToPlant%ZoneEqSubSubCompNum = 0
      TempZoneSubSubCompToPlant%PlantLoopType = 0
      TempZoneSubSubCompToPlant%PlantLoopNum = 0
      TempZoneSubSubCompToPlant%PlantLoopBranch = 0
      TempZoneSubSubCompToPlant%PlantLoopComp = 0
      TempZoneSubSubCompToPlant%FirstDemandSidePtr = 0
      TempZoneSubSubCompToPlant%LastDemandSidePtr = 0
      OneTimeFlag = .FALSE.
    END IF

    IF(ArrayCounter < ArrayLimit)THEN
      Index = ArrayCounter
      ZoneSubSubCompToPlant(Index)%ZoneEqListNum = ListNum
      ZoneSubSubCompToPlant(Index)%ZoneEqCompNum = AirDistUnitNum
      ZoneSubSubCompToPlant(Index)%ZoneEqSubCompNum = SubCompNum
      ZoneSubSubCompToPlant(Index)%ZoneEqSubSubCompNum = SubSubCompNum
      ZoneSubSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      ZoneSubSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      ZoneSubSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      ZoneSubSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    ELSE
      TempZoneSubSubCompToPlant = ZoneSubSubCompToPlant
      DEALLOCATE(ZoneSubSubCompToPlant)
      OldArrayLimit = ArrayLimit
      ArrayLimit = ArrayLimit*2
      ALLOCATE(ZoneSubSubCompToPlant(ArrayLimit))
      ZoneSubSubCompToPlant%ZoneEqListNum = 0
      ZoneSubSubCompToPlant%ZoneEqCompNum = 0
      ZoneSubSubCompToPlant%ZoneEqSubCompNum = 0
      ZoneSubSubCompToPlant%ZoneEqSubSubCompNum = 0
      ZoneSubSubCompToPlant%PlantLoopType = 0
      ZoneSubSubCompToPlant%PlantLoopNum = 0
      ZoneSubSubCompToPlant%PlantLoopBranch = 0
      ZoneSubSubCompToPlant%PlantLoopComp = 0
      ZoneSubSubCompToPlant%FirstDemandSidePtr = 0
      ZoneSubSubCompToPlant%LastDemandSidePtr = 0
      ZoneSubSubCompToPlant(1:OldArrayLimit) = TempZoneSubSubCompToPlant(1:OldArrayLimit)
      DEALLOCATE(TempZoneSubSubCompToPlant)
      ALLOCATE(TempZoneSubSubCompToPlant(ArrayLimit))
      TempZoneSubSubCompToPlant%ZoneEqListNum = 0
      TempZoneSubSubCompToPlant%ZoneEqCompNum = 0
      TempZoneSubSubCompToPlant%ZoneEqSubCompNum = 0
      TempZoneSubSubCompToPlant%ZoneEqSubSubCompNum = 0
      TempZoneSubSubCompToPlant%PlantLoopType = 0
      TempZoneSubSubCompToPlant%PlantLoopNum = 0
      TempZoneSubSubCompToPlant%PlantLoopBranch = 0
      TempZoneSubSubCompToPlant%PlantLoopComp = 0
      TempZoneSubSubCompToPlant%FirstDemandSidePtr = 0
      TempZoneSubSubCompToPlant%LastDemandSidePtr = 0

      Index = ArrayCounter
      ZoneSubSubCompToPlant(Index)%ZoneEqListNum = ListNum
      ZoneSubSubCompToPlant(Index)%ZoneEqCompNum = AirDistUnitNum
      ZoneSubSubCompToPlant(Index)%ZoneEqSubCompNum = SubCompNum
      ZoneSubSubCompToPlant(Index)%ZoneEqSubSubCompNum = SubSubCompNum
      ZoneSubSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      ZoneSubSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      ZoneSubSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      ZoneSubSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    END IF
  RETURN
END SUBROUTINE UpdateZoneSubSubCompPtrArray

SUBROUTINE UpdateAirSysCompPtrArray(Index,AirLoopNum,BranchNum,CompNum,PlantLoopType,PlantLoop,PlantBranch,PlantComp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update Air System Component Pointer Array

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(OUT)        :: Index
    INTEGER, INTENT(IN)         :: AirLoopNum
    INTEGER, INTENT(IN)         :: BranchNum
    INTEGER, INTENT(IN)         :: CompNum
    INTEGER, INTENT(IN)         :: PlantLoopType
    INTEGER, INTENT(IN)         :: PlantLoop
    INTEGER, INTENT(IN)         :: PlantBranch
    INTEGER, INTENT(IN)         :: PlantComp
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
    LOGICAL, SAVE   :: OneTimeFlag = .TRUE.
    INTEGER, SAVE   :: ArrayLimit = 100
    INTEGER         :: OldArrayLimit
    INTEGER, SAVE   :: ArrayCounter = 1


    IF (OneTimeFlag)THEN
      ALLOCATE(AirSysCompToPlant(ArrayLimit))
      ALLOCATE(TempAirSysCompToPlant(ArrayLimit))
      AirSysCompToPlant%AirLoopNum = 0
      AirSysCompToPlant%AirLoopBranch = 0
      AirSysCompToPlant%AirLoopComp = 0
      AirSysCompToPlant%PlantLoopType = 0
      AirSysCompToPlant%PlantLoopNum = 0
      AirSysCompToPlant%PlantLoopBranch = 0
      AirSysCompToPlant%PlantLoopComp = 0
      AirSysCompToPlant%FirstDemandSidePtr = 0
      AirSysCompToPlant%LastDemandSidePtr = 0
      TempAirSysCompToPlant%AirLoopNum = 0
      TempAirSysCompToPlant%AirLoopBranch = 0
      TempAirSysCompToPlant%AirLoopComp = 0
      TempAirSysCompToPlant%PlantLoopType = 0
      TempAirSysCompToPlant%PlantLoopNum = 0
      TempAirSysCompToPlant%PlantLoopBranch = 0
      TempAirSysCompToPlant%PlantLoopComp = 0
      TempAirSysCompToPlant%FirstDemandSidePtr = 0
      TempAirSysCompToPlant%LastDemandSidePtr = 0
      OneTimeFlag = .FALSE.
    END IF

    IF(ArrayCounter < ArrayLimit)THEN
      Index = ArrayCounter
      AirSysCompToPlant(Index)%AirLoopNum = AirLoopNum
      AirSysCompToPlant(Index)%AirLoopBranch = BranchNum
      AirSysCompToPlant(Index)%AirLoopComp = CompNum
      AirSysCompToPlant(Index)%PlantLoopType = PlantLoopType
      AirSysCompToPlant(Index)%PlantLoopNum = PlantLoop
      AirSysCompToPlant(Index)%PlantLoopBranch = PlantBranch
      AirSysCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    ELSE
      TempAirSysCompToPlant = AirSysCompToPlant
      DEALLOCATE(AirSysCompToPlant)
      OldArrayLimit = ArrayLimit
      ArrayLimit = ArrayLimit*2
      ALLOCATE(AirSysCompToPlant(ArrayLimit))
      AirSysCompToPlant%AirLoopNum = 0
      AirSysCompToPlant%AirLoopBranch = 0
      AirSysCompToPlant%AirLoopComp = 0
      AirSysCompToPlant%PlantLoopType = 0
      AirSysCompToPlant%PlantLoopNum = 0
      AirSysCompToPlant%PlantLoopBranch = 0
      AirSysCompToPlant%PlantLoopComp = 0
      AirSysCompToPlant%FirstDemandSidePtr = 0
      AirSysCompToPlant%LastDemandSidePtr = 0
      AirSysCompToPlant(1:OldArrayLimit) = TempAirSysCompToPlant(1:OldArrayLimit)
      DEALLOCATE(TempAirSysCompToPlant)
      ALLOCATE(TempAirSysCompToPlant(ArrayLimit))
      TempAirSysCompToPlant%AirLoopNum = 0
      TempAirSysCompToPlant%AirLoopBranch = 0
      TempAirSysCompToPlant%AirLoopComp = 0
      TempAirSysCompToPlant%PlantLoopType = 0
      TempAirSysCompToPlant%PlantLoopNum = 0
      TempAirSysCompToPlant%PlantLoopBranch = 0
      TempAirSysCompToPlant%PlantLoopComp = 0
      TempAirSysCompToPlant%FirstDemandSidePtr = 0
      TempAirSysCompToPlant%LastDemandSidePtr = 0

      Index = ArrayCounter
      AirSysCompToPlant(Index)%AirLoopNum = AirLoopNum
      AirSysCompToPlant(Index)%AirLoopBranch = BranchNum
      AirSysCompToPlant(Index)%AirLoopComp = CompNum
      AirSysCompToPlant(Index)%PlantLoopType = PlantLoopType
      AirSysCompToPlant(Index)%PlantLoopNum = PlantLoop
      AirSysCompToPlant(Index)%PlantLoopBranch = PlantBranch
      AirSysCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    END IF
  RETURN
END SUBROUTINE UpdateAirSysCompPtrArray

SUBROUTINE UpdateAirSysSubCompPtrArray(Index,AirLoopNum,BranchNum,CompNum,SubCompNum,PlantLoopType,PlantLoop,PlantBranch,PlantComp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update Air System Sub Component Pointer Array

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(OUT)        :: Index
    INTEGER, INTENT(IN)         :: AirLoopNum
    INTEGER, INTENT(IN)         :: BranchNum
    INTEGER, INTENT(IN)         :: CompNum
    INTEGER, INTENT(IN)         :: SubCompNum
    INTEGER, INTENT(IN)         :: PlantLoopType
    INTEGER, INTENT(IN)         :: PlantLoop
    INTEGER, INTENT(IN)         :: PlantBranch
    INTEGER, INTENT(IN)         :: PlantComp
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
    LOGICAL, SAVE   :: OneTimeFlag = .TRUE.
    INTEGER         :: OldArrayLimit
    INTEGER, SAVE   :: ArrayLimit = 100
    INTEGER, SAVE   :: ArrayCounter = 1


    IF (OneTimeFlag)THEN
      ALLOCATE(AirSysSubCompToPlant(ArrayLimit))
      ALLOCATE(TempAirSysSubCompToPlant(ArrayLimit))
      AirSysSubCompToPlant%AirLoopNum = 0
      AirSysSubCompToPlant%AirLoopBranch = 0
      AirSysSubCompToPlant%AirLoopComp = 0
      AirSysSubCompToPlant%AirLoopSubComp = 0
      AirSysSubCompToPlant%PlantLoopType = 0
      AirSysSubCompToPlant%PlantLoopNum = 0
      AirSysSubCompToPlant%PlantLoopBranch = 0
      AirSysSubCompToPlant%PlantLoopComp = 0
      AirSysSubCompToPlant%FirstDemandSidePtr = 0
      AirSysSubCompToPlant%LastDemandSidePtr = 0
      TempAirSysSubCompToPlant%AirLoopNum = 0
      TempAirSysSubCompToPlant%AirLoopBranch = 0
      TempAirSysSubCompToPlant%AirLoopComp = 0
      TempAirSysSubCompToPlant%AirLoopSubComp = 0
      TempAirSysSubCompToPlant%PlantLoopType = 0
      TempAirSysSubCompToPlant%PlantLoopNum = 0
      TempAirSysSubCompToPlant%PlantLoopBranch = 0
      TempAirSysSubCompToPlant%PlantLoopComp = 0
      TempAirSysSubCompToPlant%FirstDemandSidePtr = 0
      TempAirSysSubCompToPlant%LastDemandSidePtr = 0
      OneTimeFlag = .FALSE.
    END IF

    IF(ArrayCounter < ArrayLimit)THEN
      Index = ArrayCounter
      AirSysSubCompToPlant(Index)%AirLoopNum = AirLoopNum
      AirSysSubCompToPlant(Index)%AirLoopBranch = BranchNum
      AirSysSubCompToPlant(Index)%AirLoopComp = CompNum
      AirSysSubCompToPlant(Index)%AirLoopSubComp = SubCompNum
      AirSysSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      AirSysSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      AirSysSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      AirSysSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    ELSE
      TempAirSysSubCompToPlant = AirSysSubCompToPlant
      DEALLOCATE(AirSysSubCompToPlant)
      OldArrayLimit = ArrayLimit
      ArrayLimit = ArrayLimit*2
      ALLOCATE(AirSysSubCompToPlant(ArrayLimit))
      AirSysSubCompToPlant%AirLoopNum = 0
      AirSysSubCompToPlant%AirLoopBranch = 0
      AirSysSubCompToPlant%AirLoopComp = 0
      AirSysSubCompToPlant%AirLoopSubComp = 0
      AirSysSubCompToPlant%PlantLoopType = 0
      AirSysSubCompToPlant%PlantLoopNum = 0
      AirSysSubCompToPlant%PlantLoopBranch = 0
      AirSysSubCompToPlant%PlantLoopComp = 0
      AirSysSubCompToPlant%FirstDemandSidePtr = 0
      AirSysSubCompToPlant%LastDemandSidePtr = 0
      AirSysSubCompToPlant(1:OldArrayLimit) = TempAirSysSubCompToPlant(1:OldArrayLimit)
      DEALLOCATE(TempAirSysSubCompToPlant)
      ALLOCATE(TempAirSysSubCompToPlant(ArrayLimit))
      TempAirSysSubCompToPlant%AirLoopNum = 0
      TempAirSysSubCompToPlant%AirLoopBranch = 0
      TempAirSysSubCompToPlant%AirLoopComp = 0
      TempAirSysSubCompToPlant%AirLoopSubComp = 0
      TempAirSysSubCompToPlant%PlantLoopType = 0
      TempAirSysSubCompToPlant%PlantLoopNum = 0
      TempAirSysSubCompToPlant%PlantLoopBranch = 0
      TempAirSysSubCompToPlant%PlantLoopComp = 0
      TempAirSysSubCompToPlant%FirstDemandSidePtr = 0
      TempAirSysSubCompToPlant%LastDemandSidePtr = 0

      Index = ArrayCounter
      AirSysSubCompToPlant(Index)%AirLoopNum = AirLoopNum
      AirSysSubCompToPlant(Index)%AirLoopBranch = BranchNum
      AirSysSubCompToPlant(Index)%AirLoopComp = CompNum
      AirSysSubCompToPlant(Index)%AirLoopSubComp = SubCompNum
      AirSysSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      AirSysSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      AirSysSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      AirSysSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    END IF
  RETURN
END SUBROUTINE UpdateAirSysSubCompPtrArray

SUBROUTINE UpdateAirSysSubSubCompPtrArray(Index,AirLoopNum,BranchNum,CompNum,SubCompNum,SubSubCompNum,  &
                                          PlantLoopType,PlantLoop,PlantBranch,PlantComp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update Air System Sub Sub Component Pointer Array

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(OUT)        :: Index
    INTEGER, INTENT(IN)         :: AirLoopNum
    INTEGER, INTENT(IN)         :: BranchNum
    INTEGER, INTENT(IN)         :: CompNum
    INTEGER, INTENT(IN)         :: SubCompNum
    INTEGER, INTENT(IN)         :: SubSubCompNum
    INTEGER, INTENT(IN)         :: PlantLoopType
    INTEGER, INTENT(IN)         :: PlantLoop
    INTEGER, INTENT(IN)         :: PlantBranch
    INTEGER, INTENT(IN)         :: PlantComp
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
    LOGICAL, SAVE   :: OneTimeFlag = .TRUE.
    INTEGER, SAVE   :: ArrayLimit = 100
    INTEGER         :: OldArrayLimit
    INTEGER, SAVE   :: ArrayCounter = 1


    IF (OneTimeFlag)THEN
      ALLOCATE(AirSysSubSubCompToPlant(ArrayLimit))
      ALLOCATE(TempAirSysSubSubCompToPlant(ArrayLimit))
      AirSysSubSubCompToPlant%AirLoopNum = 0
      AirSysSubSubCompToPlant%AirLoopBranch = 0
      AirSysSubSubCompToPlant%AirLoopComp = 0
      AirSysSubSubCompToPlant%AirLoopSubComp = 0
      AirSysSubSubCompToPlant%AirLoopSubSubComp = 0
      AirSysSubSubCompToPlant%PlantLoopType = 0
      AirSysSubSubCompToPlant%PlantLoopNum = 0
      AirSysSubSubCompToPlant%PlantLoopBranch = 0
      AirSysSubSubCompToPlant%PlantLoopComp = 0
      AirSysSubSubCompToPlant%FirstDemandSidePtr = 0
      AirSysSubSubCompToPlant%LastDemandSidePtr = 0
      TempAirSysSubSubCompToPlant%AirLoopNum = 0
      TempAirSysSubSubCompToPlant%AirLoopBranch = 0
      TempAirSysSubSubCompToPlant%AirLoopComp = 0
      TempAirSysSubSubCompToPlant%AirLoopSubComp = 0
      TempAirSysSubSubCompToPlant%AirLoopSubSubComp = 0
      TempAirSysSubSubCompToPlant%PlantLoopType = 0
      TempAirSysSubSubCompToPlant%PlantLoopNum = 0
      TempAirSysSubSubCompToPlant%PlantLoopBranch = 0
      TempAirSysSubSubCompToPlant%PlantLoopComp = 0
      TempAirSysSubSubCompToPlant%FirstDemandSidePtr = 0
      TempAirSysSubSubCompToPlant%LastDemandSidePtr = 0
      OneTimeFlag = .FALSE.
    END IF

    IF(ArrayCounter < ArrayLimit)THEN
      Index = ArrayCounter
      AirSysSubSubCompToPlant(Index)%AirLoopNum = AirLoopNum
      AirSysSubSubCompToPlant(Index)%AirLoopBranch = BranchNum
      AirSysSubSubCompToPlant(Index)%AirLoopComp = CompNum
      AirSysSubSubCompToPlant(Index)%AirLoopSubComp = SubCompNum
      AirSysSubSubCompToPlant(Index)%AirLoopSubSubComp = SubSubCompNum
      AirSysSubSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      AirSysSubSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      AirSysSubSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      AirSysSubSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    ELSE
      TempAirSysSubSubCompToPlant = AirSysSubSubCompToPlant
      DEALLOCATE(AirSysSubSubCompToPlant)
      OldArrayLimit = ArrayLimit
      ArrayLimit = ArrayLimit*2
      ALLOCATE(AirSysSubSubCompToPlant(ArrayLimit))
      AirSysSubSubCompToPlant%AirLoopNum = 0
      AirSysSubSubCompToPlant%AirLoopBranch = 0
      AirSysSubSubCompToPlant%AirLoopComp = 0
      AirSysSubSubCompToPlant%AirLoopSubComp = 0
      AirSysSubSubCompToPlant%AirLoopSubSubComp = 0
      AirSysSubSubCompToPlant%PlantLoopType = 0
      AirSysSubSubCompToPlant%PlantLoopNum = 0
      AirSysSubSubCompToPlant%PlantLoopBranch = 0
      AirSysSubSubCompToPlant%PlantLoopComp = 0
      AirSysSubSubCompToPlant%FirstDemandSidePtr = 0
      AirSysSubSubCompToPlant%LastDemandSidePtr = 0
      AirSysSubSubCompToPlant(1:OldArrayLimit) = TempAirSysSubSubCompToPlant(1:OldArrayLimit)
      DEALLOCATE(TempAirSysSubSubCompToPlant)
      ALLOCATE(TempAirSysSubSubCompToPlant(ArrayLimit))
      TempAirSysSubSubCompToPlant%AirLoopNum = 0
      TempAirSysSubSubCompToPlant%AirLoopBranch = 0
      TempAirSysSubSubCompToPlant%AirLoopComp = 0
      TempAirSysSubSubCompToPlant%AirLoopSubComp = 0
      TempAirSysSubSubCompToPlant%AirLoopSubSubComp = 0
      TempAirSysSubSubCompToPlant%PlantLoopType = 0
      TempAirSysSubSubCompToPlant%PlantLoopNum = 0
      TempAirSysSubSubCompToPlant%PlantLoopBranch = 0
      TempAirSysSubSubCompToPlant%PlantLoopComp = 0
      TempAirSysSubSubCompToPlant%FirstDemandSidePtr = 0
      TempAirSysSubSubCompToPlant%LastDemandSidePtr = 0

      Index = ArrayCounter
      AirSysSubSubCompToPlant(Index)%AirLoopNum = AirLoopNum
      AirSysSubSubCompToPlant(Index)%AirLoopBranch = BranchNum
      AirSysSubSubCompToPlant(Index)%AirLoopComp = CompNum
      AirSysSubSubCompToPlant(Index)%AirLoopSubComp = SubCompNum
      AirSysSubSubCompToPlant(Index)%AirLoopSubSubComp = SubSubCompNum
      AirSysSubSubCompToPlant(Index)%PlantLoopType = PlantLoopType
      AirSysSubSubCompToPlant(Index)%PlantLoopNum = PlantLoop
      AirSysSubSubCompToPlant(Index)%PlantLoopBranch = PlantBranch
      AirSysSubSubCompToPlant(Index)%PlantLoopComp = PlantComp
      ArrayCounter = ArrayCounter + 1
    END IF
  RETURN
END SUBROUTINE UpdateAirSysSubSubCompPtrArray

SUBROUTINE AllocateAndSetUpVentReports

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Allocates Arrays and setup output variables related to Ventilation reports.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
 USE DataZoneEquipment, ONLY: ZoneEquipConfig, NumOfZones


          ! Subroutine Variable Declaration
  INTEGER ZoneIndex
  INTEGER SysIndex

  ALLOCATE(MaxCoolingLoadMetByVent(NumOfZones))
  ALLOCATE(MaxCoolingLoadAddedByVent(NumOfZones))
  ALLOCATE(MaxOvercoolingByVent(NumOfZones))
  ALLOCATE(MaxHeatingLoadMetByVent(NumOfZones))
  ALLOCATE(MaxHeatingLoadAddedByVent(NumOfZones))
  ALLOCATE(MaxOverheatingByVent(NumOfZones))
  ALLOCATE(MaxNoLoadHeatingByVent(NumOfZones))
  ALLOCATE(MaxNoLoadCoolingByVent(NumOfZones))

  ALLOCATE(ZoneOAMassFlow(NumOfZones))
  ALLOCATE(ZoneOAMass(NumOfZones))
  ALLOCATE(ZoneOAVolFlowStdRho(NumOfZones))
  ALLOCATE(ZoneOAVolStdRho(NumOfZones))
  ALLOCATE(ZoneOAVolFlowCrntRho(NumOfZones))
  ALLOCATE(ZoneOAVolCrntRho(NumOfZones))
  ALLOCATE(ZoneMechACH(NumOfZones))

  ALLOCATE(SysTotZoneLoadHTNG(NumPrimaryAirSys))
  ALLOCATE(SysTotZoneLoadCLNG(NumPrimaryAirSys))
  ALLOCATE(SysOALoadHTNG(NumPrimaryAirSys))
  ALLOCATE(SysOALoadCLNG(NumPrimaryAirSys))
  ALLOCATE(SysTotHTNG(NumPrimaryAirSys))
  ALLOCATE(SysTotCLNG(NumPrimaryAirSys))

  ALLOCATE(SysTotElec(NumPrimaryAirSys))
  ALLOCATE(SystotGas(NumPrimaryAirSys))
  ALLOCATE(SysTotSteam(NumPrimaryAirSys))
  ALLOCATE(SysTotH2OCOLD(NumPrimaryAirSys))
  ALLOCATE(SysTotH2OHOT(NumPrimaryAirSys))

  ALLOCATE(SysHumidHTNG(NumPrimaryAirSys))
  ALLOCATE(SysHumidElec(NumPrimaryAirSys))
  ALLOCATE(DesDehumidCLNG(NumPrimaryAirSys))
  ALLOCATE(DesDehumidElec(NumPrimaryAirSys))
  ALLOCATE(SysEvapCLNG(NumPrimaryAirSys))
  ALLOCATE(SysEvapElec(NumPrimaryAirSys))
  ALLOCATE(SysHeatExHTNG(NumPrimaryAirSys))
  ALLOCATE(SysHeatExCLNG(NumPrimaryAirSys))
  ALLOCATE(SysSolarCollectHeating(NumPrimaryAirSys))
  ALLOCATE(SysSolarCollectCooling(NumPrimaryAirSys))
  ALLOCATE(SysUserDefinedTerminalHeating(NumPrimaryAirSys))
  ALLOCATE(SysUserDefinedTerminalCooling(NumPrimaryAirSys))
  ALLOCATE(SysFANCompHTNG(NumPrimaryAirSys))
  ALLOCATE(SysFANCompElec(NumPrimaryAirSys))
  ALLOCATE(SysCCCompCLNG(NumPrimaryAirSys))
  ALLOCATE(SysCCCompH2OCOLD(NumPrimaryAirSys))
  ALLOCATE(SysCCCompElec(NumPrimaryAirSys))
  ALLOCATE(SysHCCompH2OHOT(NumPrimaryAirSys))
  ALLOCATE(SysHCCompElec(NumPrimaryAirSys))
  ALLOCATE(SysHCCompElecRes(NumPrimaryAirSys))
  ALLOCATE(SysHCCompHTNG(NumPrimaryAirSys))
  ALLOCATE(SysHCCompGas(NumPrimaryAirSys))
  ALLOCATE(SysHCCompSteam(NumPrimaryAirSys))
  ALLOCATE(SysDomesticH20(NumPrimaryAirSys))

  ALLOCATE(SetBackCounter(NumOfZones))
  ALLOCATE(HeatCoolFlag(NumOfZones))
  ALLOCATE(LastHeatCoolFlag(NumOfZones))
  ALLOCATE(FirstHeatCoolFlag(NumOfZones))
  ALLOCATE(LastHeatCoolHour(NumOfZones))
  ALLOCATE(FirstHeatCoolHour(NumOfZones))
  ALLOCATE(NoLoadFlag(NumOfZones))
  ALLOCATE(UnmetLoadFlag(NumOfZones))

  UnmetLoadFlag = .FALSE.
  SetBackCounter = 0
  HeatCoolFlag = 0
  LastHeatCoolFlag = 0
  FirstHeatCoolFlag = 0
  LastHeatCoolHour = 0
  FirstHeatCoolHour = 0
  NoLoadFlag = .FALSE.

  MaxCoolingLoadMetByVent   = 0.0d0
  MaxCoolingLoadAddedByVent = 0.0d0
  MaxOvercoolingByVent      = 0.0d0
  MaxHeatingLoadMetByVent   = 0.0d0
  MaxHeatingLoadAddedByVent = 0.0d0
  MaxOverheatingByVent      = 0.0d0
  MaxNoLoadHeatingByVent    = 0.0d0
  MaxNoLoadCoolingByVent    = 0.0d0

ZoneOAMassFlow       = 0.0d0
ZoneOAMass           = 0.0d0
ZoneOAVolFlowStdRho  = 0.0d0
ZoneOAVolStdRho      = 0.0d0
ZoneOAVolFlowCrntRho = 0.0d0
ZoneOAVolCrntRho     = 0.0d0
ZoneMechACH          = 0.0d0

  !SYSTEM LOADS REPORT
SysTotZoneLoadHTNG  = 0.0d0
SysTotZoneLoadCLNG  = 0.0d0
SysOALoadHTNG       = 0.0d0
SysOALoadCLNG       = 0.0d0
SysTotHTNG          = 0.0d0
SysTotCLNG          = 0.0d0

  !SYSTEM ENERGY USE REPORT
SysTotElec          = 0.0d0
SystotGas           = 0.0d0
SysTotSteam         = 0.0d0
SysTotH2OCOLD       = 0.0d0
SysTotH2OHOT        = 0.0d0

  !SYSTEM COMPONENT LOADS REPORT
SysFANCompHTNG      = 0.0d0
SysCCCompCLNG       = 0.0d0
SysHCCompHTNG       = 0.0d0
SysHeatExHTNG       = 0.0d0
SysHeatExCLNG       = 0.0d0
SysSolarCollectHeating = 0.d0
SysSolarCollectCooling = 0.d0
SysUserDefinedTerminalHeating = 0.d0
SysUserDefinedTerminalCooling = 0.d0
SysHumidHTNG        = 0.0d0
SysEvapCLNG         = 0.0d0
DesDehumidCLNG      = 0.0d0
SysDomesticH20      = 0.0d0

  !SYSTEM COMPONENT ENERGY REPORT
SysFANCompElec      = 0.0d0
SysHCCompH2OHOT     = 0.0d0
SysCCCompH2OCOLD    = 0.0d0
SysHCCompElec       = 0.0d0
SysCCCompElec       = 0.0d0
SysHCCompElecRes    = 0.0d0
SysHCCompGas        = 0.0d0
SysHCCompSteam      = 0.0d0
SysHumidElec        = 0.0d0
DesDehumidElec      = 0.0d0
SysEvapElec         = 0.0d0

IF (AirLoopLoadsReportEnabled) THEN
  DO SysIndex=1,NumPrimaryAirSys

  !CurrentModuleObject='AirloopHVAC'
  !SYSTEM LOADS REPORT
    CALL SetupOutputVariable('Air System Total Heating Energy [J]', SysTotHTNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Total Cooling Energy [J]', SysTotCLNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)


  !SYSTEM ENERGY USE REPORT
    CALL SetupOutputVariable('Air System Hot Water Energy [J]', SysTotH2OHOT(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Steam Energy [J]', SysTotSteam(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Chilled Water Energy [J]', SysTotH2OCOLD(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Electric Energy [J]', SysTotElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Gas Energy [J]', SysTotGas(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Water Volume [m3]', SysDomesticH20(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)


  !SYSTEM COMPONENT LOAD REPORT
    CALL SetupOutputVariable('Air System Fan Air Heating Energy [J]', SysFANCompHTNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Cooling Coil Total Cooling Energy [J]', SysCCCompCLNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heating Coil Total Heating Energy [J]', SysHCCompHTNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heat Exchanger Total Heating Energy [J]', SysHeatExHTNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heat Exchanger Total Cooling Energy [J]', SysHeatExCLNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Solar Collector Total Heating Energy [J]', SysSolarCollectHeating(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Solar Collector Total Cooling Energy [J]', SysSolarCollectCooling(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System User Defined Air Terminal Total Heating Energy [J]', &
                              SysUserDefinedTerminalHeating(SysIndex), &
                             'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System User Defined Air Terminal Total Cooling Energy [J]', &
                              SysUserDefinedTerminalCooling(SysIndex), &
                             'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Humidifier Total Heating Energy [J]', SysHumidHTNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Evaporative Cooler Total Cooling Energy [J]', SysEvapCLNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Desiccant Dehumidifier Total Cooling Energy [J]', DesDehumidCLNG(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)


  !SYSTEM COMPONENT ENERGY REPORT
    CALL SetupOutputVariable('Air System Fan Electric Energy [J]', SysFANCompElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heating Coil Hot Water Energy [J]', SysHCCompH2OHOT(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Cooling Coil Chilled Water Energy [J]', SysCCCompH2OCOLD(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System DX Heating Coil Electric Energy [J]', SysHCCompElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System DX Cooling Coil Electric Energy [J]', SysCCCompElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heating Coil Electric Energy [J]', SysHCCompElecRes(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heating Coil Gas Energy [J]', SysHCCompGas(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Heating Coil Steam Energy [J]', SysHCCompSteam(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Humidifier Electric Energy [J]', SysHumidElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Evaporative Cooler Electric Energy [J]', SysEvapElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

    CALL SetupOutputVariable('Air System Desiccant Dehumidifier Electric Energy [J]', DesDehumidElec(SysIndex), &
                           'HVAC','Sum', PrimaryAirSystem(SysIndex)%Name)

  ENDDO
ENDIF

  DO ZoneIndex=1,NumOfZones
    IF (.not. ZoneEquipConfig(ZoneIndex)%IsControlled) CYCLE
    ! CurrentModuleObject='Zones(Controlled)'
    IF (VentLoadsReportEnabled) THEN
!Cooling Loads
      CALL SetupOutputVariable('Zone Mechanical Ventilation No Load Heat Removal Energy [J]', MaxNoLoadCoolingByVent(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

      CALL SetupOutputVariable('Zone Mechanical Ventilation Cooling Load Increase Energy [J]', &
                                MaxCoolingLoadAddedByVent(ZoneIndex), &
                               'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

      CALL SetupOutputVariable('Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy [J]', &
                               MaxOverheatingByVent(ZoneIndex), &
                              'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

      CALL SetupOutputVariable('Zone Mechanical Ventilation Cooling Load Decrease Energy [J]', MaxCoolingLoadMetByVent(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)
!Heating Loads
      CALL SetupOutputVariable('Zone Mechanical Ventilation No Load Heat Addition Energy [J]', MaxNoLoadHeatingByVent(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

      CALL SetupOutputVariable('Zone Mechanical Ventilation Heating Load Increase Energy [J]', &
                                MaxHeatingLoadAddedByVent(ZoneIndex), &
                               'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

      CALL SetupOutputVariable('Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy [J]', &
                                MaxOvercoolingByVent(ZoneIndex), &
                               'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

      CALL SetupOutputVariable('Zone Mechanical Ventilation Heating Load Decrease Energy [J]', MaxHeatingLoadMetByVent(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)
    ENDIF

    CALL SetupOutputVariable('Zone Mechanical Ventilation Mass Flow Rate [kg/s]', ZoneOAMassFlow(ZoneIndex), &
                           'HVAC','Average', ZoneEquipConfig(ZoneIndex)%ZoneName)

    CALL SetupOutputVariable('Zone Mechanical Ventilation Mass [kg]', ZoneOAMass(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

    CALL SetupOutputVariable('Zone Mechanical Ventilation Standard Density Volume Flow Rate [m3/s]', &
                            ZoneOAVolFlowStdRho(ZoneIndex), &
                           'HVAC','Average', ZoneEquipConfig(ZoneIndex)%ZoneName)

    CALL SetupOutputVariable('Zone Mechanical Ventilation Standard Density Volume [m3]', &
                            ZoneOAVolStdRho(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

    CALL SetupOutputVariable('Zone Mechanical Ventilation Current Density Volume Flow Rate [m3/s]', &
                            ZoneOAVolFlowCrntRho(ZoneIndex), &
                           'HVAC','Average', ZoneEquipConfig(ZoneIndex)%ZoneName)

    CALL SetupOutputVariable('Zone Mechanical Ventilation Current Density Volume [m3]', &
                            ZoneOAVolCrntRho(ZoneIndex), &
                           'HVAC','Sum', ZoneEquipConfig(ZoneIndex)%ZoneName)

    CALL SetupOutputVariable('Zone Mechanical Ventilation Air Changes per Hour [ach]', ZoneMechACH(ZoneIndex), &
                           'HVAC','Average', ZoneEquipConfig(ZoneIndex)%ZoneName)
  END DO

  RETURN

END SUBROUTINE AllocateAndSetUpVentReports

SUBROUTINE CreateEnergyReportStructure

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher/Linda Lawrie
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Creates the Energy Reporting Structure.  This routine is only called once --
          ! so string compares have been left in.

          ! METHODOLOGY EMPLOYED:
          ! Once all compsets/nodes/connections have been established find all components
          ! subcomponents, etc.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE BranchNodeConnections, ONLY: GetComponentData, GetChildrenData, GetNumChildren, IsParentObject
  USE DataGlobalConstants

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE GetNumMeteredVariables
    FUNCTION GetNumMeteredVariables(ComponentType,ComponentName) RESULT(NumVariables)
      CHARACTER(len=*), INTENT(IN) :: ComponentType  ! Given Component Type
      CHARACTER(len=*), INTENT(IN) :: ComponentName  ! Given Component Name (user defined)
      INTEGER                      :: NumVariables
    END FUNCTION
  END INTERFACE

  INTERFACE GetMeteredVariables
    SUBROUTINE GetMeteredVariables(ComponentType,ComponentName,VarIndexes,VarTypes,IndexTypes,  &
                                   UnitsStrings,ResourceTypes,EndUses,Groups,Names,NumFound,VarIDs)
      CHARACTER(len=*),      INTENT(IN)            :: ComponentType  ! Given Component Type
      CHARACTER(len=*),      INTENT(IN)            :: ComponentName  ! Given Component Name (user defined)
      INTEGER, DIMENSION(:), INTENT(OUT)           :: VarIndexes     ! Variable Numbers
      INTEGER, DIMENSION(:), INTENT(OUT)           :: VarTypes       ! Variable Types (1=integer, 2=real, 3=meter)
      INTEGER, DIMENSION(:), INTENT(OUT)           :: IndexTypes     ! Variable Index Types (1=Zone,2=HVAC)
      CHARACTER(len=*), DIMENSION(:), INTENT(OUT)  :: UnitsStrings   ! UnitsStrings for each variable
      INTEGER, DIMENSION(:), INTENT(OUT)  :: ResourceTypes  ! ResourceTypes for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: EndUses        ! EndUses for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: Groups         ! Groups for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: Names          ! Variable Names for each variable
      INTEGER, OPTIONAL, INTENT(OUT)               :: NumFound       ! Number Found
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: VarIDs         ! Variable Report Numbers
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: AirLoopNum
  INTEGER :: BranchNum
  INTEGER :: CompNum
  INTEGER :: SubCompNum
  INTEGER :: SubSubCompNum
  INTEGER :: VarNum
  INTEGER :: VarNum1
  INTEGER :: CtrlZoneNum
  CHARACTER(len=MaxNameLength) :: TypeOfComp
  CHARACTER(len=MaxNameLength) :: NameOfComp
  LOGICAL :: ErrorsFound
  LOGICAL :: ModeFlagOn
  INTEGER :: NumInlets
  INTEGER :: NumOutlets
  INTEGER :: PlantLoopNum


    !Dimension GetChildrenData arrays
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SubCompTypes
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SubCompNames
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: InletNodeNames
  INTEGER,                      ALLOCATABLE, DIMENSION(:) :: InletNodeNumbers
  INTEGER,                      ALLOCATABLE, DIMENSION(:) :: InletFluidStreams
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: OutletNodeNames
  INTEGER,                      ALLOCATABLE, DIMENSION(:) :: OutletNodeNumbers
  INTEGER,                      ALLOCATABLE, DIMENSION(:) :: OutletFluidStreams
  INTEGER                                                 :: NumChildren
  INTEGER                                                 :: NumGrandChildren
  LOGICAL                                                 :: IsParent

    !Dimension GetMeteredVariables arrays
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: VarIndexes     ! Variable Numbers
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: VarTypes       ! Variable Types (1=integer, 2=real, 3=meter)
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: IndexTypes     ! Variable Index Types (1=Zone,2=HVAC)
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: UnitsStrings   ! UnitsStrings for each variable
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ResourceTypes  ! ResourceTypes for each variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: EndUses        ! EndUses for each variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Groups         ! Groups for each variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Names          ! Variable Names for each variable
  INTEGER         :: NumFound       ! Number Found
  INTEGER         :: NumVariables
  INTEGER         :: NumLeft     ! Counter for deeper components

    ! some variables for setting up the plant data structures
  INTEGER :: LoopSideNum
  TYPE (ReportLoopData), POINTER :: ThisReportData

  VentReportStructureCreated=.true.

  CALL AllocateAndSetUpVentReports

  DO AirLoopNum = 1, NumPrimaryAirSys
    DO BranchNum =1, PrimaryAirSystem(AirLoopNum)%NumBranches
      DO CompNum =1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
        TypeOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        NameOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
      ! Get complete list of components for complex branches
        IF (IsParentObject(TypeOfComp,NameOfComp))THEN

          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Parent = .TRUE.
          NumChildren = GetNumChildren(TypeOfComp, NameOfComp)

          ALLOCATE (SubCompTypes(NumChildren))
          ALLOCATE (SubCompNames(NumChildren))
          ALLOCATE (InletNodeNames(NumChildren))
          ALLOCATE (InletNodeNumbers(NumChildren))
          ALLOCATE (OutletNodeNames(NumChildren))
          ALLOCATE (OutletNodeNumbers(NumChildren))
          ALLOCATE (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(NumChildren))

          CALL GetChildrenData(TypeOfComp, NameOfComp, &
                               NumChildren, &
                               SubCompTypes,SubCompNames, &
                               InletNodeNames,InletNodeNumbers, &
                               OutletNodeNames,OutletNodeNumbers,ErrorsFound)

          DO SubCompNum = 1, NumChildren
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf      =   &
               SubCompTypes(SubCompNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name        =   &
               SubCompNames(SubCompNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNameIn  =   &
               InletNodeNames(SubCompNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNameOut =   &
               OutletNodeNames(SubCompNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNumIn   =   &
               InletNodeNumbers(SubCompNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNumOut  =   &
               OutletNodeNumbers(SubCompNum)
          END DO

          DEALLOCATE (SubCompTypes)
          DEALLOCATE (SubCompNames)
          DEALLOCATE (InletNodeNames)
          DEALLOCATE (InletNodeNumbers)
          DEALLOCATE (OutletNodeNames)
          DEALLOCATE (OutletNodeNumbers)

        ELSE
          NumChildren =0
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Parent = .FALSE.
        END IF
        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumSubComps = NumChildren



                  !check for 'grandchildren'
        DO SubCompNum = 1, NumChildren
          TypeOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf
          NameOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name
          IF (IsParentObject(TypeOfComp, NameOfComp))THEN
            NumGrandChildren = GetNumChildren(TypeOfComp, NameOfComp)
            ALLOCATE (SubCompTypes(NumGrandChildren))
            ALLOCATE (SubCompNames(NumGrandChildren))
            ALLOCATE (InletNodeNames(NumGrandChildren))
            ALLOCATE (InletNodeNumbers(NumGrandChildren))
            ALLOCATE (OutletNodeNames(NumGrandChildren))
            ALLOCATE (OutletNodeNumbers(NumGrandChildren))
            ALLOCATE   &
               (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%SubSubComp(NumGrandChildren))

            CALL GetChildrenData(TypeOfComp, NameOfComp, &
                             NumGrandChildren, &
                             SubCompTypes,SubCompNames, &
                             InletNodeNames,InletNodeNumbers, &
                             OutletNodeNames,OutletNodeNumbers,ErrorsFound)

            DO SubSubCompNum = 1, NumGrandChildren
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%TypeOf        = SubCompTypes(SubSubCompNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%Name        = SubCompNames(SubSubCompNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%NodeNameIn  = InletNodeNames(SubSubCompNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%NodeNameOut = OutletNodeNames(SubSubCompNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%NodeNumIn   = InletNodeNumbers(SubSubCompNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%NodeNumOut  = OutletNodeNumbers(SubSubCompNum)
              NumLeft=GetNumChildren(SubCompTypes(SubSubCompNum),SubCompNames(SubSubCompNum))
              IF (NumLeft > 0) THEN
                CALL ShowSevereError('Hanging Children for component='//TRIM(SubCompTypes(SubSubCompNum))//':'//  &
                   TRIM(SubCompNames(SubSubCompNum)))
              ENDIF
            END DO

            DEALLOCATE (SubCompTypes)
            DEALLOCATE (SubCompNames)
            DEALLOCATE (InletNodeNames)
            DEALLOCATE (InletNodeNumbers)
            DEALLOCATE (OutletNodeNames)
            DEALLOCATE (OutletNodeNumbers)
          ELSE
            NumGrandChildren =0
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Parent = .FALSE.
          END IF

          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumSubSubComps = NumGrandChildren

        END DO
      END DO
    END DO
  END DO



  DO AirLoopNum = 1, NumPrimaryAirSys
    DO BranchNum =1, PrimaryAirSystem(AirLoopNum)%NumBranches
      DO CompNum =1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
      ! Get complete list of components for complex branches
        TypeOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        NameOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
        NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
        ALLOCATE (VarIndexes(NumVariables))
        ALLOCATE (VarTypes(NumVariables))
        ALLOCATE (IndexTypes(NumVariables))
        ALLOCATE (UnitsStrings(NumVariables))
        ALLOCATE (ResourceTypes(NumVariables))
        ALLOCATE (EndUses(NumVariables))
        ALLOCATE (Groups(NumVariables))
        ALLOCATE (Names(NumVariables))
        ALLOCATE (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(NumVariables))

        PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars = NumVariables
        CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                 VarIndexes, VarTypes, &
                                 IndexTypes, UnitsStrings, &
                                 ResourceTypes, EndUses, Groups, Names, NumFound)
        ModeFlagOn = .TRUE.
        DO VarNum = 1, NumVariables
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarName   = Names(VarNum)
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarUnits  = UnitsStrings(VarNum)
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex  = VarIndexes(VarNum)
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndexType  = IndexTypes(VarNum)
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType   = VarTypes(VarNum)
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ResourceType    = ResourceTypes(VarNum)
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse          = EndUses(VarNum)
          IF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse == 'HEATINGCOILS'   &
             .AND. ModeFlagOn) THEN
            DO VarNum1 = 1, NumVariables
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
            END DO
            ModeFlagOn = .FALSE.
          ELSEIF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse == 'COOLINGCOILS'   &
             .AND. ModeFlagOn) THEN
            DO VarNum1 = 1, NumVariables
             PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
            END DO
            ModeFlagOn = .FALSE.
          ELSEIF(ModeFlagOn)THEN
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
          ENDIF
          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%Group           = Groups(VarNum)
        END DO

        DEALLOCATE (VarIndexes)
        DEALLOCATE (VarTypes)
        DEALLOCATE (IndexTypes)
        DEALLOCATE (UnitsStrings)
        DEALLOCATE (ResourceTypes)
        DEALLOCATE (EndUses)
        DEALLOCATE (Groups)
        DEALLOCATE (Names)


        DO SubCompNum =1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumSubComps
          ! Get complete list of components for complex branches
          TypeOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf
          NameOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name
          NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
          ALLOCATE (VarIndexes(NumVariables))
          ALLOCATE (VarTypes(NumVariables))
          ALLOCATE (IndexTypes(NumVariables))
          ALLOCATE (UnitsStrings(NumVariables))
          ALLOCATE (ResourceTypes(NumVariables))
          ALLOCATE (EndUses(NumVariables))
          ALLOCATE (Groups(NumVariables))
          ALLOCATE (Names(NumVariables))
          ALLOCATE (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%MeteredVar(NumVariables))

          CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                   VarIndexes, VarTypes, &
                                   IndexTypes, UnitsStrings, &
                                   ResourceTypes, EndUses, Groups, Names, NumFound)

          ModeFlagOn = .TRUE.
          DO VarNum = 1, NumVariables
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ReportVarName   = Names(VarNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ReportVarUnits  = UnitsStrings(VarNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ReportVarIndex  = VarIndexes(VarNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ReportVarIndexType  = IndexTypes(VarNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ReportVarType   = VarTypes(VarNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ResourceType    = ResourceTypes(VarNum)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%EndUse          = EndUses(VarNum)
            IF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
              DO VarNum1 = 1, NumVariables
                PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
              END DO
              ModeFlagOn = .FALSE.
            ELSEIF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
              DO VarNum1 = 1, NumVariables
                PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
              END DO
              ModeFlagOn = .FALSE.
            ELSEIF(ModeFlagOn)THEN
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
            ENDIF
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%Group           = Groups(VarNum)
          END DO

          DEALLOCATE (VarIndexes)
          DEALLOCATE (VarTypes)
          DEALLOCATE (IndexTypes)
          DEALLOCATE (UnitsStrings)
          DEALLOCATE (ResourceTypes)
          DEALLOCATE (EndUses)
          DEALLOCATE (Groups)
          DEALLOCATE (Names)

          PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumMeteredVars = NumVariables

          DO SubSubCompNum = 1,PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumSubSubComps
            ! Get complete list of components for complex branches
            TypeOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%TypeOf
            NameOfComp = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%Name
            NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
            ALLOCATE (VarIndexes(NumVariables))
            ALLOCATE (VarTypes(NumVariables))
            ALLOCATE (IndexTypes(NumVariables))
            ALLOCATE (UnitsStrings(NumVariables))
            ALLOCATE (ResourceTypes(NumVariables))
            ALLOCATE (EndUses(NumVariables))
            ALLOCATE (Groups(NumVariables))
            ALLOCATE (Names(NumVariables))
            ALLOCATE (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%MeteredVar(NumVariables))

            CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                     VarIndexes, VarTypes, &
                                     IndexTypes, UnitsStrings, &
                                     ResourceTypes, EndUses, Groups, Names, NumFound)

            ModeFlagOn = .TRUE.
            DO VarNum = 1, NumVariables
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarName = Names(VarNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarUnits    = UnitsStrings(VarNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarIndex  = VarIndexes(VarNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarIndexType  = IndexTypes(VarNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ReportVarType = VarTypes(VarNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ResourceType  = ResourceTypes(VarNum)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%EndUse            = EndUses(VarNum)
              IF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
                DO VarNum1 = 1, NumVariables
                  PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                     SubSubComp(SubSubCompNum)%MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
                END DO
                ModeFlagOn = .FALSE.
              ELSEIF (PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
                DO VarNum1 = 1, NumVariables
                  PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                     SubSubComp(SubSubCompNum)%MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
                END DO
                ModeFlagOn = .FALSE.
              ELSEIF(ModeFlagOn)THEN
                PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
              ENDIF
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%Group         = Groups(VarNum)
            END DO

            DEALLOCATE (VarIndexes)
            DEALLOCATE (VarTypes)
            DEALLOCATE (IndexTypes)
            DEALLOCATE (UnitsStrings)
            DEALLOCATE (ResourceTypes)
            DEALLOCATE (EndUses)
            DEALLOCATE (Groups)
            DEALLOCATE (Names)

            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%NumMeteredVars = NumVariables
          END DO
        END DO

      END DO
    END DO
  END DO

              ! Allocate the system serving zone equipment component arrays
  DO CtrlZoneNum=1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
      ! Set index of air loop serving zone
    DO CompNum = 1, ZoneEquipList(CtrlZoneNum)%NumOfEquipTypes
      TypeOfComp = ZoneEquipList(CtrlZoneNum)%EquipType(CompNum)
      NameOfComp = ZoneEquipList(CtrlZoneNum)%EquipName(CompNum)
      CALL GetComponentData(TypeOfComp, NameOfComp, IsParent, NumInlets,       &
                            InletNodeNames, InletNodeNumbers, InletFluidStreams,  &
                            NumOutlets, OutletNodeNames, OutletNodeNumbers, OutletFluidStreams,  &
                            ErrorsFound)
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%TypeOf         = TypeOfComp
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%Name           = NameOfComp
      ALLOCATE(ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%OutletNodeNums(NumOutlets))
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%NumOutlets     = NumOutlets
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%OutletNodeNums = OutletNodeNumbers
      ALLOCATE(ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%InletNodeNums(NumInlets))
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%NumInlets      = NumInlets
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%InletNodeNums  = InletNodeNumbers
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%Parent         = IsParent
      NumVariables = GetNumMeteredVariables(TypeOfComp,NameOfComp)
      ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%NumMeteredVars = NumVariables
      DEALLOCATE(InletNodeNames)
      DEALLOCATE(InletNodeNumbers)
      DEALLOCATE(InletFluidStreams)
      DEALLOCATE(OutletNodeNames)
      DEALLOCATE(OutletNodeNumbers)
      DEALLOCATE(OutletFluidStreams)

      ALLOCATE (VarIndexes(NumVariables))
      ALLOCATE (VarTypes(NumVariables))
      ALLOCATE (IndexTypes(NumVariables))
      ALLOCATE (UnitsStrings(NumVariables))
      ALLOCATE (ResourceTypes(NumVariables))
      ALLOCATE (EndUses(NumVariables))
      ALLOCATE (Groups(NumVariables))
      ALLOCATE (Names(NumVariables))
      ALLOCATE (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(NumVariables))

      CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                               VarIndexes, VarTypes, &
                               IndexTypes, UnitsStrings, &
                               ResourceTypes, EndUses, Groups, Names, NumFound)

      ModeFlagOn = .TRUE.
      DO VarNum = 1, NumVariables
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarName        = Names(VarNum)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarUnits       = UnitsStrings(VarNum)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarIndex       = VarIndexes(VarNum)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarIndexType   = IndexTypes(VarNum)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%ReportVarType        = VarTypes(VarNum)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%ResourceType         = ResourceTypes(VarNum)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%EndUse               = EndUses(VarNum)
        IF (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
          DO VarNum1 = 1, NumVariables
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
          END DO
          ModeFlagOn = .FALSE.
        ELSEIF (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
          DO VarNum1 = 1, NumVariables
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
          END DO
          ModeFlagOn = .FALSE.
        ELSEIF(ModeFlagOn)THEN
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
        ENDIF
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%MeteredVar(VarNum)%Group                = Groups(VarNum)
      END DO

      DEALLOCATE (VarIndexes)
      DEALLOCATE (VarTypes)
      DEALLOCATE (IndexTypes)
      DEALLOCATE (UnitsStrings)
      DEALLOCATE (ResourceTypes)
      DEALLOCATE (EndUses)
      DEALLOCATE (Groups)
      DEALLOCATE (Names)

      IF (IsParentObject(TypeOfComp,NameOfComp))THEN
        NumChildren = GetNumChildren(TypeOfComp, NameOfComp)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%NumSubEquip=NumChildren

        ALLOCATE (SubCompTypes(NumChildren))
        ALLOCATE (SubCompNames(NumChildren))
        ALLOCATE (InletNodeNames(NumChildren))
        ALLOCATE (InletNodeNumbers(NumChildren))
        ALLOCATE (OutletNodeNames(NumChildren))
        ALLOCATE (OutletNodeNumbers(NumChildren))
        ALLOCATE (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(NumChildren))

        CALL GetChildrenData(TypeOfComp, NameOfComp, &
                             NumChildren, &
                             SubCompTypes,SubCompNames, &
                             InletNodeNames,InletNodeNumbers, &
                             OutletNodeNames,OutletNodeNumbers,ErrorsFound)

        DO SubCompNum = 1, NumChildren
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%TypeOf         = SubCompTypes(SubCompNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%Name           = SubCompNames(SubCompNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%OutletNodeNum  = OutletNodeNumbers(SubCompNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%InletNodeNum   = InletNodeNumbers(SubCompNum)
        END DO

        DEALLOCATE (SubCompTypes)
        DEALLOCATE (SubCompNames)
        DEALLOCATE (InletNodeNames)
        DEALLOCATE (InletNodeNumbers)
        DEALLOCATE (OutletNodeNames)
        DEALLOCATE (OutletNodeNumbers)
      ELSE
        NumChildren =0
      END IF


      DO SubCompNum = 1, NumChildren
        TypeOfComp = ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%TypeOf
        NameOfComp = ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%Name
        IF (IsParentObject(TypeOfComp,NameOfComp))THEN
          NumGrandChildren = GetNumChildren(TypeOfComp, NameOfComp)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%NumSubSubEquip=NumGrandChildren
          ALLOCATE (SubCompTypes(NumGrandChildren))
          ALLOCATE (SubCompNames(NumGrandChildren))
          ALLOCATE (InletNodeNames(NumGrandChildren))
          ALLOCATE (InletNodeNumbers(NumGrandChildren))
          ALLOCATE (OutletNodeNames(NumGrandChildren))
          ALLOCATE (OutletNodeNumbers(NumGrandChildren))
          ALLOCATE (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(NumGrandChildren))
              !Sankar added the array number for EquipData
          CALL GetChildrenData(TypeOfComp, NameOfComp, &
                             NumGrandChildren, &
                             SubCompTypes,SubCompNames, &
                             InletNodeNames,InletNodeNumbers, &
                             OutletNodeNames,OutletNodeNumbers,ErrorsFound)

          DO SubSubCompNum = 1, NumGrandChildren
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%TypeOf         = SubCompTypes(SubSubCompNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%Name           = SubCompNames(SubSubCompNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%OutletNodeNum  = OutletNodeNumbers(SubSubCompNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%InletNodeNum   = InletNodeNumbers(SubSubCompNum)
          END DO
          DEALLOCATE (SubCompTypes)
          DEALLOCATE (SubCompNames)
          DEALLOCATE (InletNodeNames)
          DEALLOCATE (InletNodeNumbers)
          DEALLOCATE (OutletNodeNames)
          DEALLOCATE (OutletNodeNumbers)
        ELSE
          NumGrandChildren =0
        END IF
      END DO
    END DO
  END DO

  DO CtrlZoneNum=1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
    DO CompNum = 1,ZoneEquipList(CtrlZoneNum)%NumOfEquipTypes
      DO SubCompNum = 1, ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%NumSubEquip
        TypeOfComp = ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%TypeOf
        NameOfComp = ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%Name

        NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
        ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%NumMeteredVars = NumVariables !Sankar added this line
        ALLOCATE (VarIndexes(NumVariables))
        ALLOCATE (VarTypes(NumVariables))
        ALLOCATE (IndexTypes(NumVariables))
        ALLOCATE (UnitsStrings(NumVariables))
        ALLOCATE (ResourceTypes(NumVariables))
        ALLOCATE (EndUses(NumVariables))
        ALLOCATE (Groups(NumVariables))
        ALLOCATE (Names(NumVariables))
        ALLOCATE (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(NumVariables))

        CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                 VarIndexes, VarTypes, &
                                 IndexTypes, UnitsStrings, &
                                 ResourceTypes, EndUses, Groups, Names, NumFound)

        ModeFlagOn = .TRUE.
        DO VarNum = 1, NumVariables
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarName       =   &
             Names(VarNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarUnits      =   &
             UnitsStrings(VarNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarIndex      =   &
             VarIndexes(VarNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarIndexType  =   &
             IndexTypes(VarNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ReportVarType       =   &
             VarTypes(VarNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%ResourceType        =   &
             ResourceTypes(VarNum)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%EndUse              =   &
             EndUses(VarNum)
          IF (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
             MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
            DO VarNum1 = 1, NumVariables
              ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum1)%EndUse_CompMode =   &
                 HeatingOnly
            END DO
            ModeFlagOn = .FALSE.
          ELSEIF (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
             MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
            DO VarNum1 = 1, NumVariables
              ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum1)%EndUse_CompMode =   &
                 CoolingOnly
            END DO
            ModeFlagOn = .FALSE.
          ELSEIF(ModeFlagOn)THEN
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%EndUse_CompMode =   &
               NoHeatNoCool
          ENDIF
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%MeteredVar(VarNum)%Group               =   &
             Groups(VarNum)
        END DO

        DEALLOCATE (VarIndexes)
        DEALLOCATE (VarTypes)
        DEALLOCATE (IndexTypes)
        DEALLOCATE (UnitsStrings)
        DEALLOCATE (ResourceTypes)
        DEALLOCATE (EndUses)
        DEALLOCATE (Groups)
        DEALLOCATE (Names)

        DO SubSubCompNum = 1, ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%NumSubSubEquip
          TypeOfComp = ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%TypeOf
          NameOfComp = ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%Name

          NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
          ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%NumMeteredvars =   &
             NumVariables !Sankar added this line
          ALLOCATE (VarIndexes(NumVariables))
          ALLOCATE (VarTypes(NumVariables))
          ALLOCATE (IndexTypes(NumVariables))
          ALLOCATE (UnitsStrings(NumVariables))
          ALLOCATE (ResourceTypes(NumVariables))
          ALLOCATE (EndUses(NumVariables))
          ALLOCATE (Groups(NumVariables))
          ALLOCATE (Names(NumVariables))
          ALLOCATE (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%  &
             SubSubEquipData(SubSubCompNum)%MeteredVar(NumVariables))

          CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                       VarIndexes, VarTypes, &
                                       IndexTypes, UnitsStrings, &
                                       ResourceTypes, EndUses, Groups, Names, NumFound)

          ModeFlagOn = .TRUE.
          DO VarNum = 1, NumVariables
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%ReportVarName       = Names(VarNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%ReportVarUnits      = UnitsStrings(VarNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%ReportVarIndex      = VarIndexes(VarNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%ReportVarIndexType  = IndexTypes(VarNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%ReportVarType       = VarTypes(VarNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%ResourceType        = ResourceTypes(VarNum)
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%EndUse              = EndUses(VarNum)
            IF (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
              DO VarNum1 = 1, NumVariables
                ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
                   MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
              END DO
              ModeFlagOn = .FALSE.
            ELSEIF (ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
              DO VarNum1 = 1, NumVariables
                ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
                   MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
              END DO
              ModeFlagOn = .FALSE.
            ELSEIF(ModeFlagOn)THEN
              ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
                 MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
            ENDIF
            ZoneEquipList(CtrlZoneNum)%EquipData(CompNum)%SubEquipData(SubCompNum)%SubSubEquipData(SubSubCompNum)%  &
               MeteredVar(VarNum)%Group               = Groups(VarNum)
          END DO

          DEALLOCATE (VarIndexes)
          DEALLOCATE (VarTypes)
          DEALLOCATE (IndexTypes)
          DEALLOCATE (UnitsStrings)
          DEALLOCATE (ResourceTypes)
          DEALLOCATE (EndUses)
          DEALLOCATE (Groups)
          DEALLOCATE (Names)


        END DO
      END DO
    END DO
  END DO

!***Plant Loops

  ! previously, four separate huge DO loops all looking very very similar were used here
  ! each individual block would operate on a single type of loop-side (plant demand, cond supply, etc.)
  ! now, a bigger DO loop is applied iterating over all loops
  ! a pointer (ThisReportData) is then directed to a particular item in the appropriate array
  ! by operating on the pointer directly, we are actually operating on the item in the TARGET array item
  ! in making this change, over 700 lines of code were dropped down to a single block

  DO PlantLoopNum = 1, NumPlantLoops+NumCondLoops
    DO LoopSideNum = DemandSide, SupplySide

        IF (PlantLoopNum <= NumPlantLoops) THEN
            SELECT CASE (LoopSideNum)
            CASE (DemandSide)
                ThisReportData => VentRepPlantDemandSide(PlantLoopNum)
            CASE (SupplySide)
                ThisReportData => VentRepPlantSupplySide(PlantLoopNum)
            END SELECT
        ELSE ! CondLoop
            SELECT CASE (LoopSideNum)
            CASE (DemandSide)
                ThisReportData => VentRepCondDemandSide(PlantLoopNum-NumPlantLoops)
            CASE (SupplySide)
                ThisReportData => VentRepCondSupplySide(PlantLoopNum-NumPlantLoops)
            END SELECT
        END IF

        DO BranchNum =1, ThisReportData%TotalBranches
          DO CompNum =1, ThisReportData%Branch(BranchNum)%TotalComponents
            TypeOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%TypeOf
            NameOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%Name
          ! Get complete list of components for complex branches
            IF (IsParentObject(TypeOfComp,NameOfComp))THEN

              ThisReportData%Branch(BranchNum)%Comp(CompNum)%Parent = .TRUE.
              NumChildren = GetNumChildren(TypeOfComp, NameOfComp)

              ALLOCATE (SubCompTypes(NumChildren))
              ALLOCATE (SubCompNames(NumChildren))
              ALLOCATE (InletNodeNames(NumChildren))
              ALLOCATE (InletNodeNumbers(NumChildren))
              ALLOCATE (OutletNodeNames(NumChildren))
              ALLOCATE (OutletNodeNumbers(NumChildren))
              ALLOCATE (ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(NumChildren))

              CALL GetChildrenData(TypeOfComp, NameOfComp, &
                                   NumChildren, &
                                   SubCompTypes,SubCompNames, &
                                   InletNodeNames,InletNodeNumbers, &
                                   OutletNodeNames,OutletNodeNumbers,ErrorsFound)

              DO SubCompNum = 1, NumChildren
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf      =   &
                   SubCompTypes(SubCompNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name        =   &
                   SubCompNames(SubCompNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNameIn  =   &
                   InletNodeNames(SubCompNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNameOut =   &
                   OutletNodeNames(SubCompNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNumIn   =   &
                   InletNodeNumbers(SubCompNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNumOut  =   &
                   OutletNodeNumbers(SubCompNum)
              END DO

              DEALLOCATE (SubCompTypes)
              DEALLOCATE (SubCompNames)
              DEALLOCATE (InletNodeNames)
              DEALLOCATE (InletNodeNumbers)
              DEALLOCATE (OutletNodeNames)
              DEALLOCATE (OutletNodeNumbers)

           ELSE
             NumChildren =0
             ThisReportData%Branch(BranchNum)%Comp(CompNum)%Parent = .FALSE.
           END IF
           ThisReportData%Branch(BranchNum)%Comp(CompNum)%NumSubComps = NumChildren



                  !check for 'grandchildren'
           DO SubCompNum = 1, NumChildren
             TypeOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf
             NameOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name
             IF (IsParentObject(TypeOfComp, NameOfComp))THEN
               NumGrandChildren = GetNumChildren(TypeOfComp, NameOfComp)
               ALLOCATE (SubCompTypes(NumGrandChildren))
               ALLOCATE (SubCompNames(NumGrandChildren))
               ALLOCATE (InletNodeNames(NumGrandChildren))
               ALLOCATE (InletNodeNumbers(NumGrandChildren))
               ALLOCATE (OutletNodeNames(NumGrandChildren))
               ALLOCATE (OutletNodeNumbers(NumGrandChildren))
               ALLOCATE (ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                  SubSubComp(NumGrandChildren))

               CALL GetChildrenData(TypeOfComp, NameOfComp, &
                              NumGrandChildren, &
                              SubCompTypes,SubCompNames, &
                              InletNodeNames,InletNodeNumbers, &
                              OutletNodeNames,OutletNodeNumbers,ErrorsFound)

               DO SubSubCompNum = 1, NumGrandChildren
                   ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                      SubSubComp(SubSubCompNum)%TypeOf        = SubCompTypes(SubSubCompNum)
                   ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                      SubSubComp(SubSubCompNum)%Name        = SubCompNames(SubSubCompNum)
                   ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                      SubSubComp(SubSubCompNum)%NodeNameIn  = InletNodeNames(SubSubCompNum)
                   ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                      SubSubComp(SubSubCompNum)%NodeNameOut = OutletNodeNames(SubSubCompNum)
                   ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                      SubSubComp(SubSubCompNum)%NodeNumIn   = InletNodeNumbers(SubSubCompNum)
                   ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                      SubSubComp(SubSubCompNum)%NodeNumOut  = OutletNodeNumbers(SubSubCompNum)
               END DO

               DEALLOCATE (SubCompTypes)
               DEALLOCATE (SubCompNames)
               DEALLOCATE (InletNodeNames)
               DEALLOCATE (InletNodeNumbers)
               DEALLOCATE (OutletNodeNames)
               DEALLOCATE (OutletNodeNumbers)
             ELSE
               NumGrandChildren =0
               ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Parent = .FALSE.
             END IF

             ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumSubSubComps = NumGrandChildren

           END DO
         END DO
       END DO
   END DO
 END DO

  DO PlantLoopNum = 1, NumPlantLoops + NumCondLoops

    DO LoopSideNum = DemandSide, SupplySide

        IF (PlantLoopNum <= NumPlantLoops) THEN
            SELECT CASE (LoopSideNum)
            CASE (DemandSide)
                ThisReportData => VentRepPlantDemandSide(PlantLoopNum)
            CASE (SupplySide)
                ThisReportData => VentRepPlantSupplySide(PlantLoopNum)
            END SELECT
        ELSE ! CondLoop
            SELECT CASE (LoopSideNum)
            CASE (DemandSide)
                ThisReportData => VentRepCondDemandSide(PlantLoopNum - NumPlantLoops)
            CASE (SupplySide)
                ThisReportData => VentRepCondSupplySide(PlantLoopNum - NumPlantLoops)
            END SELECT
        END IF

        DO BranchNum =1, ThisReportData%TotalBranches
          DO CompNum =1, ThisReportData%Branch(BranchNum)%TotalComponents
          ! Get complete list of components for complex branches
            TypeOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%TypeOf
            NameOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%Name
            NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
            ALLOCATE (VarIndexes(NumVariables))
            ALLOCATE (VarTypes(NumVariables))
            ALLOCATE (IndexTypes(NumVariables))
            ALLOCATE (UnitsStrings(NumVariables))
            ALLOCATE (ResourceTypes(NumVariables))
            ALLOCATE (EndUses(NumVariables))
            ALLOCATE (Groups(NumVariables))
            ALLOCATE (Names(NumVariables))
            ALLOCATE (ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(NumVariables))

            ThisReportData%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars = NumVariables
            CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                     VarIndexes, VarTypes, &
                                     IndexTypes, UnitsStrings, &
                                     ResourceTypes, EndUses, Groups, Names, NumFound)

            ModeFlagOn = .TRUE.
            DO VarNum = 1, NumVariables
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarName   =   &
                 Names(VarNum)
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarUnits  =   &
                 UnitsStrings(VarNum)
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndex  =   &
                 VarIndexes(VarNum)
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarIndexType  =   &
                 IndexTypes(VarNum)
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ReportVarType   =   &
                 VarTypes(VarNum)
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ResourceType    =   &
                 ResourceTypes(VarNum)
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse          =   &
                 EndUses(VarNum)
              IF (ThisReportData%Branch(BranchNum)%Comp(CompNum)%  &
                 MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
                DO VarNum1 = 1, NumVariables
                  ThisReportData%Branch(BranchNum)%Comp(CompNum)%  &
                     MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
                END DO
                ModeFlagOn = .FALSE.
              ELSEIF (ThisReportData%Branch(BranchNum)%Comp(CompNum)%  &
                 MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
                DO VarNum1 = 1, NumVariables
                  ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
                END DO
                ModeFlagOn = .FALSE.
              ELSEIF(ModeFlagOn)THEN
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
              ENDIF
              ThisReportData%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%Group           = Groups(VarNum)
            END DO

            DEALLOCATE (VarIndexes)
            DEALLOCATE (VarTypes)
            DEALLOCATE (IndexTypes)
            DEALLOCATE (UnitsStrings)
            DEALLOCATE (ResourceTypes)
            DEALLOCATE (EndUses)
            DEALLOCATE (Groups)
            DEALLOCATE (Names)


            DO SubCompNum =1, ThisReportData%Branch(BranchNum)%Comp(CompNum)%NumSubComps
                  ! Get complete list of components for complex branches
              TypeOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf
              NameOfComp = ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name
              NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
              ALLOCATE (VarIndexes(NumVariables))
              ALLOCATE (VarTypes(NumVariables))
              ALLOCATE (IndexTypes(NumVariables))
              ALLOCATE (UnitsStrings(NumVariables))
              ALLOCATE (ResourceTypes(NumVariables))
              ALLOCATE (EndUses(NumVariables))
              ALLOCATE (Groups(NumVariables))
              ALLOCATE (Names(NumVariables))
              ALLOCATE   &
                 (ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%MeteredVar(NumVariables))

              CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                                       VarIndexes, VarTypes, &
                                       IndexTypes, UnitsStrings, &
                                       ResourceTypes, EndUses, Groups, Names, NumFound)

             ModeFlagOn = .TRUE.
              DO VarNum = 1, NumVariables
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%ReportVarName   = Names(VarNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%ReportVarUnits  = UnitsStrings(VarNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%ReportVarIndex  = VarIndexes(VarNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%ReportVarIndexType  = IndexTypes(VarNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%ReportVarType   = VarTypes(VarNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%ResourceType    = ResourceTypes(VarNum)
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%EndUse          = EndUses(VarNum)
                IF (ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%EndUse == 'HEATINGCOILS' .AND. ModeFlagOn) THEN
                  DO VarNum1 = 1, NumVariables
                    ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                       MeteredVar(VarNum1)%EndUse_CompMode = HeatingOnly
                  END DO
                  ModeFlagOn = .FALSE.
                ELSEIF (ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%EndUse == 'COOLINGCOILS' .AND. ModeFlagOn) THEN
                  DO VarNum1 = 1, NumVariables
                    ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                       MeteredVar(VarNum1)%EndUse_CompMode = CoolingOnly
                  END DO
                  ModeFlagOn = .FALSE.
                ELSEIF(ModeFlagOn)THEN
                  ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                     MeteredVar(VarNum)%EndUse_CompMode = NoHeatNoCool
                ENDIF
                ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   MeteredVar(VarNum)%Group           = Groups(VarNum)
              END DO

              DEALLOCATE (VarIndexes)
              DEALLOCATE (VarTypes)
              DEALLOCATE (IndexTypes)
              DEALLOCATE (UnitsStrings)
              DEALLOCATE (ResourceTypes)
              DEALLOCATE (EndUses)
              DEALLOCATE (Groups)
              DEALLOCATE (Names)

              ThisReportData%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumMeteredVars = NumVariables
            END DO
          END DO
        END DO
    END DO
  END DO

  RETURN

END SUBROUTINE CreateEnergyReportStructure

 ! End Initialization Section of the Module
!******************************************************************************



! Beginning of Reporting subroutines for the SimAir Module
! *****************************************************************************

SUBROUTINE ReportSystemEnergyUse
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   November 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate and report system loads and energy

          ! METHODOLOGY EMPLOYED:
                !Accumulate meter data to appropriate report variables


          ! REFERENCES:
          ! na

         ! USE STATEMENTS:
  USE  DataZoneEnergyDemands
  USE  Psychrometrics, ONLY: PsyHFnTdbW
  USE  DataGlobalConstants
  USE  DataEnvironment, ONLY: OutDryBulbTemp

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
    CHARACTER(len=MaxNameLength)    :: CompType
    CHARACTER(len=MaxNameLength)    :: CompName
    INTEGER     ::  Index    !loop counter
    INTEGER     ::  nodes    !loop counter
    INTEGER     ::  CtrlZoneNum         !ZONE counter
    INTEGER     ::  ZoneInNum           !counter for zone air distribution inlets
    INTEGER     ::  AirLoopNum           !counter for zone air distribution inlets
    INTEGER     ::  BranchNum           !counter for zone air distribution inlets
    INTEGER     ::  EquipListNum           !counter for zone air distribution inlets
    INTEGER     ::  VarNum           !counter for zone air distribution inlets
    INTEGER     ::  CompNum
    INTEGER     ::  SubCompNum
    INTEGER     ::  SubSubCompNum
    INTEGER     ::  CompMode
    INTEGER     ::  InletNodeNum
    INTEGER     ::  OutletNodeNum
    INTEGER     ::  ADUNum
    INTEGER     ::  ADUCoolNum
    INTEGER     ::  ADUHeatNum
    INTEGER     ::  AirDistCoolInletNodeNum
    INTEGER     ::  AirDistHeatInletNodeNum
    INTEGER     ::  EnergyType
    INTEGER     ::  ActualZoneNum
    REAL(r64)   ::  CompEnergyUse
    REAL(r64)   ::  ZoneLoad
    REAL(r64)   ::  CompLoad
    REAL(r64)   ::  ADUCoolFlowrate
    REAL(r64)   ::  ADUHeatFlowrate
    LOGICAL     ::  CompLoadFlag

IF (.not. AirLoopLoadsReportEnabled) RETURN

  !SYSTEM LOADS REPORT
SysTotZoneLoadHTNG  = 0.0d0
SysTotZoneLoadCLNG  = 0.0d0
SysOALoadHTNG       = 0.0d0
SysOALoadCLNG       = 0.0d0
SysTotHTNG          = 0.0d0
SysTotCLNG          = 0.0d0

  !SYSTEM ENERGY USE REPORT
SysTotElec          = 0.0d0
SystotGas           = 0.0d0
SysTotSteam         = 0.0d0
SysTotH2OCOLD       = 0.0d0
SysTotH2OHOT        = 0.0d0

  !SYSTEM COMPONENT LOADS REPORT
SysFANCompHTNG      = 0.0d0
SysCCCompCLNG       = 0.0d0
SysHCCompHTNG       = 0.0d0
SysHeatExHTNG       = 0.0d0
SysHeatExCLNG       = 0.0d0
SysSolarCollectHeating = 0.d0
SysSolarCollectCooling = 0.d0
SysUserDefinedTerminalHeating = 0.d0
SysUserDefinedTerminalCooling = 0.d0
SysHumidHTNG        = 0.0d0
SysEvapCLNG         = 0.0d0
DesDehumidCLNG      = 0.0d0
SysDomesticH20      = 0.0d0

  !SYSTEM COMPONENT ENERGY REPORT
SysFANCompElec      = 0.0d0
SysHCCompH2OHOT     = 0.0d0
SysCCCompH2OCOLD    = 0.0d0
SysHCCompElec       = 0.0d0
SysCCCompElec       = 0.0d0
SysHCCompElecRes    = 0.0d0
SysHCCompGas        = 0.0d0
SysHCCompSteam      = 0.0d0
SysHumidElec        = 0.0d0
DesDehumidElec      = 0.0d0
SysEvapElec         = 0.0d0


  DO AirLoopNum = 1, NumPrimaryAirSys
    DO BranchNum = 1, PrimaryAirSystem(AirLoopNum)%NumBranches
      IF(Node(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%NodeNumOut)%massflowrate <= 0.0d0)CYCLE
      DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
        CompName = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
        CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        InletNodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
        OutletNodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut
        IF (InletNodeNum <= 0 .OR. OutletNodeNum <= 0) CYCLE
        CompLoad = Node(OutletNodeNum)%massflowrate*  &
                   (PsyHFnTdbW(Node(InletNodeNum)%Temp, Node(InletNodeNum)%HumRat) -   &
                       PsyHFnTdbW(Node(outletNodeNum)%Temp, Node(outletNodeNum)%HumRat))
        CompLoad = CompLoad * TimeStepSys * SecInHour
        CompEnergyUse = 0.0d0
        EnergyType = iRT_None
        CompLoadFlag=.TRUE.
        CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
        CompLoadFlag = .FALSE.
        DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          CompMode = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%EndUse_CompMode
          CompEnergyUse = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%CurMeterReading
          EnergyType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ResourceType
          CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
        END DO

        DO SubCompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumSubComps
          CompName = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name
          CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf
          InletNodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNumIn
          IF (InletNodeNum <= 0 .OR. OutletNodeNum <= 0) CYCLE
          OutletNodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NodeNumOut
          CompLoad = Node(OutletNodeNum)%massflowrate*  &
             (PsyHFnTdbW(Node(InletNodeNum)%Temp, Node(InletNodeNum)%HumRat) -   &
                 PsyHFnTdbW(Node(outletNodeNum)%Temp, Node(outletNodeNum)%HumRat))
          CompLoad = CompLoad * TimeStepSys * SecInHour
          CompEnergyUse = 0.0d0
          EnergyType = iRT_None
          CompLoadFlag=.TRUE.
          CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
          CompLoadFlag = .FALSE.
          DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumMeteredVars
            CompMode = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%EndUse_CompMode
            CompEnergyUse = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%CurMeterReading
            EnergyType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               MeteredVar(VarNum)%ResourceType
            CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
          END DO

          DO SubSubCompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumSubSubcomps
            CompName = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%Name
            CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%TypeOf
            InletNodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%NodeNumIn
            OutletNodeNum = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%NodeNumOut
            IF (InletNodeNum <= 0 .OR. OutletNodeNum <= 0) CYCLE
            CompLoad = Node(OutletNodeNum)%massflowrate*(PsyHFnTdbW(Node(InletNodeNum)%Temp,   &
               Node(InletNodeNum)%HumRat) - PsyHFnTdbW(Node(outletNodeNum)%Temp, Node(outletNodeNum)%HumRat))
            CompLoad = CompLoad * TimeStepSys * SecInHour
            CompEnergyUse = 0.0d0
            EnergyType = iRT_None
            CompLoadFlag=.TRUE.
            CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
            CompLoadFlag = .FALSE.
            DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
               SubSubComp(SubSubCompNum)%NumMeteredVars
              CompMode = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%EndUse_CompMode
              CompEnergyUse = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%CurMeterReading
              EnergyType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                 SubSubComp(SubSubCompNum)%MeteredVar(VarNum)%ResourceType
              CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
            END DO

          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  DO CtrlZoneNum=1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE

          !retrieve the zone load for each zone
    ActualZoneNum=ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    ZoneLoad= ZoneSysEnergyDemand(ActualZoneNum)%TotalOutputRequired

        !if system operating in deadband reset zone load
    IF (DeadbandOrSetback(ActualZoneNum)) ZoneLoad = 0.0d0

        ! retrieve air loop indexes
    AirLoopNum = ZoneEquipConfig(CtrlZoneNum)%AirLoopNum
    IF(AirLoopNum == 0 ) CYCLE

        !Zone cooling load
    IF(ZoneLoad < -SmallLoad)THEN
      SysTotZoneLoadCLNG(AirLoopNum) = SysTotZoneLoadCLNG(AirLoopNum) + ABS(ZoneLoad)

        !Zone heating load
    ELSEIF(ZoneLoad > SmallLoad)THEN
      SysTotZoneLoadHTNG(AirLoopNum) = SysTotZoneLoadHTNG(AirLoopNum) + ABS(ZoneLoad)
    ENDIF

        !loop over the zone supply air path inlet nodes
    DO ZoneInNum=1,ZoneEquipConfig(CtrlZoneNum)%NumInletNodes
      AirDistCoolInletNodeNum  = MAX(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode,0)
      AirDistHeatInletNodeNum  = MAX(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode,0)

        ! Set for cooling or heating path
      IF(AirDistCoolInletNodeNum > 0 .AND. AirDistHeatInletNodeNum == 0)THEN
        ADUCoolFlowrate = MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
      ELSEIF(AirDistHeatInletNodeNum > 0 .AND. AirDistCoolInletNodeNum == 0)THEN
        ADUHeatFlowrate = MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
      ELSE
        ADUCoolFlowrate = 0.0d0
        ADUHeatFlowrate = 0.0d0
      END IF



      DO Index =1,2
          EquipListNum = ZoneEquipConfig(CtrlZoneNum)%EquipListIndex

        IF (Index ==1)THEN
          ADUCoolNum = MAX(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%AirDistUnitIndex,0)
          IF (ADUCoolNum == 0) CYCLE
          ADUNum=ADUCoolNum
        ELSE !(Index =2)THEN
          ADUHeatNum = MAX(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%AirDistUnitIndex,0)
          IF (ADUHeatNum == 0) CYCLE
          ADUNum=ADUHeatNum
        ENDIF

        CompLoad = 0.0d0
        IF(ZoneEquipList(EquipListNum)%EquipData(ADUNum)%NumInlets > 0)THEN
          DO nodes = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%NumInlets
            InletNodeNum = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%InletNodeNums(index)
            CompLoad = CompLoad + (PsyHFnTdbW(Node(InletNodeNum)%Temp,   &
               Node(InletNodeNum)%HumRat)*Node(InletNodeNum)%massflowrate)
          END DO
          DO nodes = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%NumOutlets
            OutletNodeNum = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%OutletNodeNums(index)
            CompLoad = CompLoad - (PsyHFnTdbW(Node(OutletNodeNum)%Temp,   &
               Node(OutletNodeNum)%HumRat)*Node(OutletNodeNum)%massflowrate)
          END DO
        ENDIF
        CompLoad = CompLoad * TimeStepSys * SecInHour
        CompName = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%Name
        CompType = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%TypeOf
        CompEnergyUse =0.0d0
        EnergyType = iRT_None
        CompLoadFlag = .TRUE.
        CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
        CompLoadFlag = .FALSE.
        DO VarNum = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%NumMeteredVars
          CompEnergyUse = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%MeteredVar(VarNum)%CurMeterReading
          EnergyType = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%MeteredVar(VarNum)%ResourceType
          CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
        ENDDO

        DO SubCompNum = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%NumSubEquip
          CompName = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%Name
          CompType = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%TypeOf
          InletNodeNum = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%InletNodeNum
          OutletNodeNum = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%OutletNodeNum
          IF (InletNodeNum <= 0 .OR. OutletNodeNum <= 0) CYCLE
          CompLoad = Node(InletNodeNum)%massflowrate*(PsyHFnTdbW(Node(InletNodeNum)%Temp, Node(InletNodeNum)%HumRat) &
                       - PsyHFnTdbW(Node(OutletNodeNum)%Temp, Node(OutletNodeNum)%HumRat))
          CompLoad = CompLoad * TimeStepSys * SecInHour
          CompEnergyUse =0.0d0
          EnergyType = iRT_None
          CompLoadFlag = .TRUE.
          CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
          CompLoadFlag = .FALSE.
          DO VarNum = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%NumMeteredVars
            CompEnergyUse = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               MeteredVar(VarNum)%CurMeterReading
            CompMode = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               MeteredVar(VarNum)%EndUse_CompMode
            EnergyType = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               MeteredVar(VarNum)%ResourceType
            CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
          ENDDO

          DO SubSubCompNum = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%NumSubSubEquip
            CompName = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%Name
            CompType = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%TypeOf
            InletNodeNum = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%InletNodeNum
            OutletNodeNum = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%OutletNodeNum
            IF (InletNodeNum <= 0 .OR. OutletNodeNum <= 0) CYCLE
            CompLoad = Node(InletNodeNum)%massflowrate*(PsyHFnTdbW(Node(InletNodeNum)%Temp, Node(InletNodeNum)%HumRat) &
                         - PsyHFnTdbW(Node(OutletNodeNum)%Temp, Node(OutletNodeNum)%HumRat))
            CompLoad = CompLoad * TimeStepSys * SecInHour
            CompEnergyUse =0.0d0
            EnergyType = iRT_None
            CompLoadFlag = .TRUE.
            CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
            CompLoadFlag = .FALSE.
            DO VarNum = 1, ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
               SubSubEquipData(SubSubCompNum)%NumMeteredVars
              CompEnergyUse = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
                 SubSubEquipData(SubSubCompNum)%MeteredVar(VarNum)%CurMeterReading
              CompMode = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
                 SubSubEquipData(SubSubCompNum)%MeteredVar(VarNum)%EndUse_CompMode
              EnergyType = ZoneEquipList(EquipListNum)%EquipData(ADUNum)%SubEquipData(SubCompNum)%  &
                 SubSubEquipData(SubSubCompNum)%MeteredVar(VarNum)%ResourceType
              CALL CalcSystemEnergyUse(CompLoadFlag, AirLoopNum,CompType,EnergyType,CompLoad,CompEnergyUse)
            ENDDO
          ENDDO !SubSubCompNum
        ENDDO !SubCompNum
      ENDDO !Index
     END DO ! ZoneInNum
   END DO  ! Controlled Zone Loop

  DO AirLoopNum = 1, NumPrimaryAirSys
    SysTotHTNG(AirLoopNum)    = SysFANCompHTNG(AirLoopNum) + SysHCCompHTNG(AirLoopNum) +   &
                                SysHeatExHTNG(AirLoopNum) + SysHumidHTNG(AirLoopNum)   +   &
                                SysSolarCollectHeating(AirLoopNum) + SysUserDefinedTerminalHeating(AirLoopNum)
    SysTotCLNG(AirLoopNum)    = SysCCCompCLNG(AirLoopNum) + SysHeatExCLNG(AirLoopNum) +   &
                                SysEvapCLNG(AirLoopNum) + DesDehumidCLNG(AirLoopNum)  +   &
                                SysSolarCollectCooling(AirLoopNum) + SysUserDefinedTerminalCooling(AirLoopNum)
    SysTotElec(AirLoopNum)    = SysFANCompElec(AirLoopNum) +  SysHCCompElec(AirLoopNum) +   &
                                SysCCCompElec(AirLoopNum) + SysHCCompElecRes(AirLoopNum) + &
                                SysHumidElec(AirLoopNum) + DesDehumidElec(AirLoopNum) +   &
                                SysEvapElec(AirLoopNum)
    SystotGas(AirLoopNum)     = SysHCCompGas(AirLoopNum)
    SysTotSteam(AirLoopNum)   = SysHCCompSteam(AirLoopNum)
    SysTotH2OCOLD(AirLoopNum) = SysCCCompH2OCOLD(AirLoopNum)
    SysTotH2OHOT(AirLoopNum)  = SysHCCompH2OHOT(AirLoopNum)
  END DO

RETURN

END SUBROUTINE ReportSystemEnergyUse


SUBROUTINE CalcSystemEnergyUse(CompLoadFlag,AirLoopNum,CompType,EnergyType,CompLoad,CompEnergy)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Nov. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! accumulate system loads and energy to report variables

          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE  Psychrometrics, ONLY: PsyHFnTdbW
  USE  DataZoneEnergyDemands
  USE  DataGlobalConstants
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN)    :: CompType
    INTEGER, INTENT(IN)     ::  AirLoopNum
    INTEGER, INTENT(IN)     ::  EnergyType
    REAL(r64), INTENT(IN)        ::  CompLoad
    REAL(r64), INTENT(IN)        ::  CompEnergy
    LOGICAL, INTENT(IN)     ::  CompLoadFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: SmallLoad = 0.1d0  !(W)
    REAL(r64), PARAMETER :: KJperJ = 0.001d0   !kilojoules per joules

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
    TYPE CompTypeError
      CHARACTER(len=MaxNameLength) :: CompType=' '
      INTEGER                      :: CompErrIndex
    END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER,SAVE :: NumCompTypes=0
    TYPE (CompTypeError), ALLOCATABLE, SAVE, DIMENSION(:) :: CompTypeErrors
    LOGICAL, SAVE :: firsttime=.true.
    INTEGER :: Found

IF (.not. AirLoopLoadsReportEnabled) RETURN

        ! following for debug
!    CHARACTER(len=60) :: cEnergyType

!    cEnergyType=cRT_ValidTypes(EnergyType-ResourceTypeInitialOffset)

IF (firsttime) THEN
  ALLOCATE(CompTypeErrors(100))
  firsttime=.false.
ENDIF

SELECT CASE(CompType)

! Outside Air System
  CASE('AIRLOOPHVAC:OUTDOORAIRSYSTEM')
    IF (CompLoadFlag) THEN
      IF(CompLoad > 0.0d0)THEN
        SysOALoadCLNG(AirLoopNum) =  SysOALoadCLNG(AirLoopNum) + ABS(CompLoad)
      ELSE
        SysOALoadHTNG(AirLoopNum) =  SysOALoadHTNG(AirLoopNum) + ABS(CompLoad)
      ENDIF
    ENDIF
! Outdoor Air Mixer
  CASE('OUTDOORAIR:MIXER')
       CONTINUE !No energy transfers to account for

  CASE('AIRTERMINAL:SINGLEDUCT:INLETSIDEMIXER')
       CONTINUE !No energy transfers to account for

  CASE('AIRTERMINAL:SINGLEDUCT:SUPPLYSIDEMIXER')
       CONTINUE !No energy transfers to account for

! Fan Types for the air sys simulation
  CASE('FAN:CONSTANTVOLUME', &
       'FAN:VARIABLEVOLUME', &
       'FAN:ONOFF','FAN:COMPONENTMODEL') !cpw22Aug2010 Add 'FAN:COMPONENTMODEL')

        IF(CompLoadFlag)SysFANCompHTNG(AirLoopNum) = SysFANCompHTNG(AirLoopNum) + ABS(CompLoad)
        SysFANCompElec(AirLoopNum) = SysFANCompElec(AirLoopNum) + CompEnergy


! Cooling Coil Types for the air sys simulation
  CASE('COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED', &
       'COIL:COOLING:DX:SINGLESPEED', &
       'COIL:COOLING:DX:TWOSPEED', &
       'COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE', &
       'COIL:COOLING:DX:MULTISPEED', &
       'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT', &
       'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION', &
       'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
       'COIL:COOLING:DX:VARIABLESPEED', &
       'COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED', &
       'COIL:COOLING:WATER:DETAILEDGEOMETRY', &
       'COIL:COOLING:WATER', &
       'COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE')

       IF(CompLoadFlag)SysCCCompCLNG(AirLoopNum) = SysCCCompCLNG(AirLoopNum) + ABS(CompLoad)
       SELECT CASE(EnergyType)
         CASE (iRT_PlantLoopCoolingDemand, iRT_DistrictCooling)
               SysCCCompH2OCOLD(AirLoopNum) = SysCCCompH2OCOLD(AirLoopNum) + CompEnergy
         CASE (iRT_Electricity)
               SysCCCompElec(AirLoopNum) = SysCCCompElec(AirLoopNum) + CompEnergy
       END SELECT

! Heating Coil Types for the air sys simulation
  CASE('COIL:HEATING:WATER', &
       'COIL:HEATING:DX:SINGLESPEED', &
       'COIL:HEATING:DX:MULTISPEED', &
       'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT', &
       'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION', &
       'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
       'COIL:HEATING:DX:VARIABLESPEED', &
       'COIL:HEATING:STEAM', &
       'COIL:HEATING:GAS', &
       'COIL:HEATING:GAS:MULTISTAGE', &
       'COIL:HEATING:DESUPERHEATER')

        IF(CompLoadFlag)SysHCCompHTNG(AirLoopNum) = SysHCCompHTNG(AirLoopNum) + ABS(CompLoad)
        SELECT CASE(EnergyType)
          CASE (iRT_PlantLoopHeatingDemand, iRT_DistrictHeating)
                SysHCCompH2OHOT(AirLoopNum)    = SysHCCompH2OHOT(AirLoopNum) + CompEnergy
          CASE (iRT_Steam)
                SysHCCompSteam(AirLoopNum)     = SysHCCompSteam(AirLoopNum) + CompEnergy
          CASE (iRT_Electricity)
                SysHCCompElec(AirLoopNum)      = SysHCCompElec(AirLoopNum) + CompEnergy
          CASE (iRT_Natural_Gas, iRT_Propane)
                 SysHCCompGas(AirLoopNum)       = SysHCCompGas(AirLoopNum) + CompEnergy
        END SELECT

  CASE('COIL:HEATING:ELECTRIC', &
       'COIL:HEATING:ELECTRIC:MULTISTAGE')

       IF(CompLoadFlag)SysHCCompHTNG(AirLoopNum) = SysHCCompHTNG(AirLoopNum) + ABS(CompLoad)
       SELECT CASE(EnergyType)
         CASE (iRT_Electricity)
           SysHCCompElecRes(AirLoopNum)      = SysHCCompElecRes(AirLoopNum) + CompEnergy
         CASE DEFAULT
       END SELECT

  CASE ('COIL:USERDEFINED')

    IF(CompLoadFlag) THEN
      IF (CompLoad > 0.d0) THEN
        SysCCCompCLNG(AirLoopNum) = SysCCCompCLNG(AirLoopNum) + ABS(CompLoad)
      ELSE
        SysHCCompHTNG(AirLoopNum) = SysHCCompHTNG(AirLoopNum) + ABS(CompLoad)
      ENDIF
    ENDIF
    SELECT CASE(EnergyType)
      CASE (iRT_PlantLoopHeatingDemand, iRT_DistrictHeating)
            SysHCCompH2OHOT(AirLoopNum)    = SysHCCompH2OHOT(AirLoopNum) + CompEnergy
      CASE (iRT_PlantLoopCoolingDemand, iRT_DistrictCooling)
            SysCCCompH2OCOLD(AirLoopNum) = SysCCCompH2OCOLD(AirLoopNum) + CompEnergy
      CASE (iRT_Steam)
            SysHCCompSteam(AirLoopNum)     = SysHCCompSteam(AirLoopNum) + CompEnergy
      CASE (iRT_Electricity)
        IF (CompLoad > 0.d0) THEN
          SysCCCompElec(AirLoopNum) = SysCCCompElec(AirLoopNum) + CompEnergy
        ELSE
          SysHCCompElec(AirLoopNum)      = SysHCCompElec(AirLoopNum) + CompEnergy
        ENDIF
      CASE (iRT_Natural_Gas, iRT_Propane)
             SysHCCompGas(AirLoopNum)       = SysHCCompGas(AirLoopNum) + CompEnergy
    END SELECT

!DX Systems
  CASE('AIRLOOPHVAC:UNITARYSYSTEM')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('COILSYSTEM:COOLING:DX')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('COILSYSTEM:HEATING:DX')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARYHEATONLY')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARYHEATCOOL')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS')
       CONTINUE !All energy transfers accounted for in subcomponent models
  CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED')
       CONTINUE !All energy transfers accounted for in subcomponent models

! Humidifier Types for the air system simulation
  CASE('HUMIDIFIER:STEAM:ELECTRIC')
        IF(CompLoadFlag)SysHumidHTNG(AirLoopNum) = SysHumidHTNG(AirLoopNum) + ABS(CompLoad)
        SELECT CASE(EnergyType)
          CASE (iRT_Water)
                SysDomesticH20(AirLoopNum) = SysDomesticH20(AirLoopNum) + ABS(CompEnergy)
          CASE (iRT_Electricity)
                SysHumidElec(AirLoopNum) = SysHumidElec(AirLoopNum) + CompEnergy
        END SELECT

! Evap Cooler Types for the air system simulation
  CASE('EVAPORATIVECOOLER:DIRECT:CELDEKPAD', &
       'EVAPORATIVECOOLER:INDIRECT:CELDEKPAD', &
       'EVAPORATIVECOOLER:INDIRECT:WETCOIL', &
       'EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL', &
       'EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL')
        IF(CompLoadFlag)SysEvapCLNG(AirLoopNum) = SysEvapCLNG(AirLoopNum) + ABS(CompLoad)
        SELECT CASE(EnergyType)
          CASE (iRT_Water)
                SysDomesticH20(AirLoopNum) = SysDomesticH20(AirLoopNum) + ABS(CompEnergy)
          CASE (iRT_Electricity)
                SysEvapElec(AirLoopNum) = SysEvapElec(AirLoopNum) + CompEnergy
        END SELECT

! Desiccant Dehumidifier Types for the air system simulation
  CASE('DEHUMIDIFIER:DESICCANT:NOFANS', &
       'DEHUMIDIFIER:DESICCANT:SYSTEM')
        IF(CompLoadFlag)DesDehumidCLNG(AirLoopNum) = DesDehumidCLNG(AirLoopNum) + ABS(CompLoad)
        SELECT CASE(EnergyType)
          CASE (iRT_Electricity)
                DesDehumidElec(AirLoopNum) = DesDehumidElec(AirLoopNum) + CompEnergy
        END SELECT

! Heat Exchanger Types
  CASE('HEATEXCHANGER:AIRTOAIR:FLATPLATE', &
       'HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT', &
       'HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
    IF (CompLoadFlag) THEN
      IF(CompLoad > 0.0d0 )THEN
        SysHeatExCLNG(AirLoopNum) =  SysHeatExCLNG(AirLoopNum) + ABS(CompLoad)
      ELSE
        SysHeatExHTNG(AirLoopNum) =  SysHeatExHTNG(AirLoopNum) + ABS(CompLoad)
      ENDIF
    ENDIF

! Air Terminal Types
  CASE('AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:COOL', &
       'AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:HEAT', &
       'AIRTERMINAL:DUALDUCT:VAV:COOL', &
       'AIRTERMINAL:DUALDUCT:VAV:HEAT', &
       'AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:OUTDOORAIR', &
       'AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:RECIRCULATEDAIR', &
       'AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEINDUCTION', &
       'AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:REHEAT', &
       'AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT', &
       'AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT', &
       'AIRTERMINAL:SINGLEDUCT:UNCONTROLLED', &
       'AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:NOREHEAT', &
       'AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:REHEAT', &
       'AIRTERMINAL:SINGLEDUCT:VAV:NOREHEAT', &
       'AIRTERMINAL:SINGLEDUCT:VAV:REHEAT', &
       'AIRTERMINAL:SINGLEDUCT:VAV:REHEAT:VARIABLESPEEDFAN', &
       'AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM', &
       'ZONEHVAC:AIRDISTRIBUTIONUNIT')
       CONTINUE !All energy transfers accounted for in component models

! Duct Types
  CASE('DUCT')
       CONTINUE ! duct losses should be accounted for here ???
                ! requires addition of a new variable to sum duct losses
! Example:
!      IF(CompLoad > 0.0d0)THEN
!        SysDuctHTNG(AirLoopNum) =  SysDuctHTNG(AirLoopNum) + ABS(CompLoad)
!      ELSE
!        SysDuctCLNG(AirLoopNum) =  SysDuctCLNG(AirLoopNum) + ABS(CompLoad)
!      ENDIF


! Solar Collector Types
  CASE('SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL', &
       'SOLARCOLLECTOR:UNGLAZEDTRANSPIRED')
    IF (CompLoadFlag) THEN
      IF(CompLoad > 0.0d0 ) THEN
        SysSolarCollectCooling(AirLoopNum) =  SysSolarCollectCooling(AirLoopNum) + ABS(CompLoad)
      ELSE
        SysSolarCollectHeating(AirLoopNum) =  SysSolarCollectHeating(AirLoopNum) + ABS(CompLoad)
      ENDIF
    ENDIF

  CASE('AIRTERMINAL:SINGLEDUCT:USERDEFINED')
  ! User component model energy use should be accounted for here
    IF (CompLoadFlag) THEN
      IF(CompLoad > 0.0d0 )THEN
        SysUserDefinedTerminalCooling(AirLoopNum) =  SysUserDefinedTerminalCooling(AirLoopNum) + ABS(CompLoad)
      ELSE
        SysUserDefinedTerminalHeating(AirLoopNum) =  SysUserDefinedTerminalHeating(AirLoopNum) + ABS(CompLoad)
      ENDIF
    ENDIF
    SELECT CASE(EnergyType)
      CASE (iRT_PlantLoopHeatingDemand, iRT_DistrictHeating)
            SysHCCompH2OHOT(AirLoopNum)    = SysHCCompH2OHOT(AirLoopNum) + CompEnergy
      CASE (iRT_PlantLoopCoolingDemand, iRT_DistrictCooling)
            SysCCCompH2OCOLD(AirLoopNum) = SysCCCompH2OCOLD(AirLoopNum) + CompEnergy
      CASE (iRT_Steam)
            SysHCCompSteam(AirLoopNum)     = SysHCCompSteam(AirLoopNum) + CompEnergy
      CASE (iRT_Electricity)
        IF (CompLoad > 0.d0) THEN
          SysCCCompElec(AirLoopNum) = SysCCCompElec(AirLoopNum) + CompEnergy
        ELSE
          SysHCCompElec(AirLoopNum)      = SysHCCompElec(AirLoopNum) + CompEnergy
        ENDIF
      CASE (iRT_Natural_Gas, iRT_Propane)
             SysHCCompGas(AirLoopNum)       = SysHCCompGas(AirLoopNum) + CompEnergy
    END SELECT
! Recurring warning for unaccounted equipment types
! (should never happen, when this does happen enter appropriate equipment CASE statement above)
  CASE DEFAULT
    Found=0
    IF (NumCompTypes > 0) THEN
      Found=FindItemInList(CompType,CompTypeErrors%CompType,NumCompTypes)
    ENDIF
    IF (Found == 0) THEN
      NumCompTypes=NumCompTypes+1
      CompTypeErrors(NumCompTypes)%CompType=CompType
      Found=NumCompTypes
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd('CalcSystemEnergyUse: Component Type='//trim(CompType)//  &
      ' not logged as one of allowable Component Types.',CompTypeErrors(Found)%CompErrIndex)

END SELECT


RETURN
END SUBROUTINE CalcSystemEnergyUse

SUBROUTINE ReportMaxVentilationLoads
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher (with minor assistance from RKS)
          !       DATE WRITTEN   July 2004
          !       MODIFIED       Dec. 2006, BG. reengineered to add zone forced air units to vent rates and loads
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate and report zone ventilation loads

          ! METHODOLOGY EMPLOYED:
          ! calculate energy contribution of outside air through mixing box and pro-rate to
          ! zones according to zone mass flow rates.

          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE  Psychrometrics, ONLY: PsyHFnTdbW, PsyRhoAirFnPbTdbW
  USE  DataZoneEnergyDemands
  USE  DataGlobalConstants
  USE  DataHeatBalance, ONLY: Zone, ZnAirRpt, ZonePreDefRep
  USE  DataHeatBalFanSys, ONLY: MAT, ZoneAirHumRatAvg
  USE  DataEnvironment, ONLY: StdBaroPress, StdRhoAir, OutAirDensity, OutBaroPress

  USE WindowAC,                  Only : GetWindowACOutAirNode, GetWindowACMixedAirNode, &
                                        GetWindowACZoneInletAirNode, GetWindowACReturnAirNode
  USE PackagedTerminalHeatPump,  Only : GetPTUnitOutAirNode, GetPTUnitMixedAirNode, &
                                        GetPTUnitZoneInletAirNode, GetPTUnitReturnAirNode
  USE FanCoilUnits,              Only : GetFanCoilOutAirNode, GetFanCoilMixedAirNode, &
                                        GetFanCoilZoneInletAirNode, GetFanCoilReturnAirNode
  USE UnitVentilator ,           Only : GetUnitVentilatorOutAirNode, GetUnitVentilatorMixedAirNode, &
                                        GetUnitVentilatorZoneInletAirNode, GetUnitVentilatorReturnAirNode
  USE PurchasedAirManager,       Only : GetPurchasedAirOutAirMassFlow, GetPurchasedAirZoneInletAirNode, &
                                        GetPurchasedAirMixedAirTemp, GetPurchasedAirMixedAirHumRat, GetPurchasedAirReturnAirNode
  USE HVACStandAloneERV ,        Only : GetStandAloneERVOutAirNode, GetStandAloneERVReturnAirNode, &
                                        GetStandAloneERVZoneInletAirNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64), PARAMETER :: SmallLoad = 0.1d0  !(W)
    REAL(r64), PARAMETER :: KJperJ = 0.001d0   !kilojoules per joules

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER     ::  CtrlZoneNum         !ZONE counter
    INTEGER     ::  ZoneInNum           !counter for zone air distribution inlets
    INTEGER     ::  ReturnAirNode       !node number for return node on primary air loop
    INTEGER     ::  MixedAirNode        !mixed air node number (right after the mixing box) on primary air loop
    INTEGER     ::  AirLoopNum
    INTEGER     ::  AirDistCoolInletNodeNum
    INTEGER     ::  AirDistHeatInletNodeNum

    REAL(r64)   ::  AirSysEnthReturnAir      !enthalpy of the return air (mixing box inlet node, return side)
    REAL(r64)   ::  AirSysEnthMixedAir       !enthalpy of the mixed air (mixing box outlet node, mixed air side)
    REAL(r64)   ::  AirSysZoneVentLoad       !ventilation load attributed to a particular zone from primary air system
    REAL(r64)   ::  ADUCoolFlowrate
    REAL(r64)   ::  ADUHeatFlowrate
    REAL(r64)   ::  AirSysTotalMixFlowRate   !Mixed air flow
    REAL(r64)   ::  AirSysOutAirFlow         ! outside air flow rate for zone from primary air system

    REAL(r64)   ::  ZFAUEnthReturnAir !Zone forced Air unit enthalpy of the return air
    REAL(r64)   ::  ZFAUTempMixedAir  !Zone forced Air unit dry-bulb temperature of the mixed air
    REAL(r64)   ::  ZFAUHumRatMixedAir  !Zone forced Air unit humidity ratio of the mixed air
    REAL(r64)   ::  ZFAUEnthMixedAir  !Zone forced Air unit enthalpy of the mixed air
    REAL(r64)   ::  ZFAUFlowRate
    REAL(r64)   ::  ZFAUZoneVentLoad !ventilation load attributed to a particular zone from zone forced air units
    REAL(r64)   ::  ZFAUOutAirFlow   !outside air flow rate for zone from zone forced air units.
    INTEGER     ::  ZoneInletAirNode

    REAL(r64)   ::  ZoneVentLoad        !ventilation load attributed to a particular zone
    REAL(r64)   ::  ZoneLoad            !ventilation load attributed to a particular zone
    REAL(r64)   ::  OutAirFlow        !Total outside air flow
    REAL(r64)   ::  ZoneFlowFrac      !fraction of mixed air flowing to a zone
    REAL(r64)   ::  ZoneVolume        !Volume of zone
    REAL(r64)   ::  currentZoneAirDensity ! current zone air density (outside barometric pressure)

    INTEGER     ::  ActualZoneNum
    INTEGER     ::  OutAirNode
    INTEGER     ::  thisZoneEquipNum  ! loop counter

!  CALL GetComponentEnergyUse
    IF (.not. VentReportStructureCreated) RETURN
    IF (.not. VentLoadsReportEnabled) RETURN
    !following inits are array assignments across all controlled zones.
    ZoneOAMassFlow               = 0.0d0
    ZoneOAMass                   = 0.0d0
    ZoneOAVolFlowStdRho          = 0.0d0
    ZoneOAVolStdRho              = 0.0d0
    ZoneOAVolFlowCrntRho         = 0.0d0
    ZoneOAVolCrntRho             = 0.0d0
    ZoneMechACH                  = 0.0d0
    MaxCoolingLoadMetByVent      = 0.0d0
    MaxCoolingLoadAddedByVent    = 0.0d0
    MaxOvercoolingByVent         = 0.0d0
    MaxHeatingLoadMetByVent      = 0.0d0
    MaxHeatingLoadAddedByVent    = 0.0d0
    MaxOverheatingByVent         = 0.0d0
    MaxNoLoadHeatingByVent       = 0.0d0
    MaxNoLoadCoolingByVent       = 0.0d0

  DO CtrlZoneNum=1,NumOfZones
    IF (.not. ZoneEquipConfig(CtrlZoneNum)%IsControlled) CYCLE
    ! first clear out working variables from previous zone.
    AirDistCoolInletNodeNum      = 0
    AirDistHeatInletNodeNum      = 0
    ADUCoolFlowrate              = 0.0d0
    ADUHeatFlowrate              = 0.0d0
    AirSysTotalMixFlowRate       = 0.0d0
    AirSysZoneVentLoad           = 0.0d0
    AirSysOutAirFlow             = 0.0d0
    ZFAUFlowRate                 = 0.0d0
    ZFAUZoneVentLoad             = 0.0d0
    ZFAUOutAirFlow               = 0.0d0
    OutAirFlow                   = 0.0d0
    ZoneFlowFrac                 = 0.0d0
    ZoneVolume                   = 0.0d0

    !retrieve the zone load for each zone
    ActualZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    ZoneLoad      = ZoneSysEnergyDemand(ActualZoneNum)%TotalOutputRequired
    ZoneVolume     = Zone(ActualZoneNum)%Volume * Zone(ActualZoneNum)%Multiplier * Zone(ActualZoneNum)%ListMultiplier  !CR 7170

        !if system operating in deadband reset zone load
    IF (DeadbandOrSetback(ActualZoneNum)) ZoneLoad = 0.0d0
    IF (DeadbandOrSetback(ActualZoneNum))THEN
     DBFlag = 1
    ELSE
     DBFlag = 0
    ENDIF

  !  IF(AirLoopNum == 0 ) CYCLE   !orig line (BG 12-8-06 changed, zone forced air equipment seems to get excluded here...)

    ! first deal with any (and all) Zone Forced Air Units that might have outside air.
    DO thisZoneEquipNum = 1, ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%NumOfEquipTypes
      SELECT CASE (ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipType_Num(thisZoneEquipNum))
      ! case statement to cover all possible zone forced air units that could have outside air
      CASE (WindowAC_Num)    ! Window Air Conditioner
        OutAirNode =   &
           GetWindowACOutAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (OutAirNode > 0)  ZFAUOutAirFlow = ZFAUOutAirFlow + Node(OutAirNode)%MassFlowRate

        ZoneInletAirNode =   &
           GetWindowACZoneInletAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (ZoneInletAirNode > 0) ZFAUFlowRate =  MAX(Node(ZoneInletAirNode)%MassFlowRate,0.0d0)
        MixedAirNode  =   &
           GetWindowACMixedAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        ReturnAirNode =   &
           GetWindowACReturnAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If ((MixedAirNode > 0) .AND. (ReturnAirNode > 0)) then
          ZFAUEnthMixedAir  = PsyHFnTdbW(Node(MixedAirNode)%Temp, Node(MixedAirNode)%HumRat)
          ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
           !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
          ZFAUZoneVentLoad = ZFAUZoneVentLoad +   &
             (ZFAUFlowRate)*(ZFAUEnthMixedAir-ZFAUEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
        ELSE
          ZFAUZoneVentLoad =  ZFAUZoneVentLoad +  0.0d0
        ENDIF

      CASE (PkgTermHPAirToAir_Num, PkgTermACAirToAir_Num, PkgTermHPWaterToAir_Num)
        OutAirNode = GetPTUnitOutAirNode(ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum), &
                                ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipType_Num(thisZoneEquipNum) )
        If (OutAirNode > 0)  ZFAUOutAirFlow = ZFAUOutAirFlow + Node(OutAirNode)%MassFlowRate

        ZoneInletAirNode =   &
           GetPTUnitZoneInletAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) , &
                                      ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipType_Num(thisZoneEquipNum))
        If (ZoneInletAirNode > 0) ZFAUFlowRate =  MAX(Node(ZoneInletAirNode)%MassFlowRate,0.0d0)
        MixedAirNode  =   &
           GetPTUnitMixedAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum), &
                                  ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipType_Num(thisZoneEquipNum) )
        ReturnAirNode =   &
           GetPTUnitReturnAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum), &
                                   ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipType_Num(thisZoneEquipNum) )
        If ((MixedAirNode > 0) .AND. (ReturnAirNode > 0)) then
          ZFAUEnthMixedAir  = PsyHFnTdbW(Node(MixedAirNode)%Temp, Node(MixedAirNode)%HumRat)
          ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
           !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
          ZFAUZoneVentLoad = ZFAUZoneVentLoad +   &
             (ZFAUFlowRate)*(ZFAUEnthMixedAir-ZFAUEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
        ELSE
          ZFAUZoneVentLoad =  ZFAUZoneVentLoad +  0.0d0
        ENDIF

      CASE (FanCoil4Pipe_Num)
        OutAirNode =   &
           GetFanCoilOutAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (OutAirNode > 0)  ZFAUOutAirFlow = ZFAUOutAirFlow + Node(OutAirNode)%MassFlowRate

        ZoneInletAirNode =   &
           GetFanCoilZoneInletAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (ZoneInletAirNode > 0) ZFAUFlowRate =  MAX(Node(ZoneInletAirNode)%MassFlowRate,0.0d0)
        MixedAirNode  =   &
           GetFanCoilMixedAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        ReturnAirNode =   &
           GetFanCoilReturnAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If ((MixedAirNode > 0) .AND. (ReturnAirNode > 0)) then
          ZFAUEnthMixedAir  = PsyHFnTdbW(Node(MixedAirNode)%Temp, Node(MixedAirNode)%HumRat)
          ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
           !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
          ZFAUZoneVentLoad = ZFAUZoneVentLoad +   &
             (ZFAUFlowRate)*(ZFAUEnthMixedAir-ZFAUEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
        ELSE
          ZFAUZoneVentLoad =  ZFAUZoneVentLoad +  0.0d0
        ENDIF

      CASE (UnitVentilator_Num)
        OutAirNode =   &
           GetUnitVentilatorOutAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (OutAirNode > 0)  ZFAUOutAirFlow = ZFAUOutAirFlow + Node(OutAirNode)%MassFlowRate

        ZoneInletAirNode =   &
           GetUnitVentilatorZoneInletAirNode(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (ZoneInletAirNode > 0) ZFAUFlowRate =  MAX(Node(ZoneInletAirNode)%MassFlowRate,0.0d0)
        MixedAirNode  =   &
           GetUnitVentilatorMixedAirNode(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        ReturnAirNode =   &
           GetUnitVentilatorReturnAirNode(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If ((MixedAirNode > 0) .AND. (ReturnAirNode > 0)) then
          ZFAUEnthMixedAir  = PsyHFnTdbW(Node(MixedAirNode)%Temp, Node(MixedAirNode)%HumRat)
          ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
           !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
          ZFAUZoneVentLoad = ZFAUZoneVentLoad +   &
             (ZFAUFlowRate)*(ZFAUEnthMixedAir-ZFAUEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
        ELSE
          ZFAUZoneVentLoad =  ZFAUZoneVentLoad +  0.0d0
        ENDIF
      CASE (PurchasedAir_Num)
        ZFAUOutAirFlow = ZFAUOutAirFlow +   &
          GetPurchasedAirOutAirMassFlow( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        ZoneInletAirNode =   &
           GetPurchasedAirZoneInletAirNode(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (ZoneInletAirNode > 0) ZFAUFlowRate =  MAX(Node(ZoneInletAirNode)%MassFlowRate,0.0d0)
        ZFAUTempMixedAir  =  &
           GetPurchasedAirMixedAirTemp(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        ZFAUHumRatMixedAir  =  &
           GetPurchasedAirMixedAirHumRat(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        ReturnAirNode =   &
           GetPurchasedAirReturnAirNode(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If ((ZFAUFlowRate > 0) .AND. (ReturnAirNode > 0)) then
          ZFAUEnthMixedAir  = PsyHFnTdbW(ZFAUTempMixedAir, ZFAUHumRatMixedAir)
          ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
           !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
          ZFAUZoneVentLoad = ZFAUZoneVentLoad +   &
             (ZFAUFlowRate)*(ZFAUEnthMixedAir-ZFAUEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
        ELSE
          ZFAUZoneVentLoad =  ZFAUZoneVentLoad +  0.0d0
        ENDIF
      CASE (ERVStandAlone_Num)
        OutAirNode =   &
           GetStandAloneERVOutAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (OutAirNode > 0)  ZFAUOutAirFlow = ZFAUOutAirFlow + Node(OutAirNode)%MassFlowRate

        ZoneInletAirNode =   &
           GetStandAloneERVZoneInletAirNode(   &
              ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If (ZoneInletAirNode > 0) ZFAUFlowRate =  MAX(Node(ZoneInletAirNode)%MassFlowRate,0.0d0)
        MixedAirNode  = ZoneInletAirNode
        ReturnAirNode =   &
           GetStandAloneERVReturnAirNode( ZoneEquipList(ZoneEquipConfig(CtrlZoneNum)%EquipListIndex)%EquipIndex(thisZoneEquipNum) )
        If ((MixedAirNode > 0) .AND. (ReturnAirNode > 0)) then
          ZFAUEnthMixedAir  = PsyHFnTdbW(Node(MixedAirNode)%Temp, Node(MixedAirNode)%HumRat)
          ZFAUEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
           !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
          ZFAUZoneVentLoad = ZFAUZoneVentLoad +   &
             (ZFAUFlowRate)*(ZFAUEnthMixedAir-ZFAUEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
        ELSE
          ZFAUZoneVentLoad =  ZFAUZoneVentLoad +  0.0d0
        ENDIF

      END SELECT

    ENDDO


        ! retrieve air loop indexes
    AirLoopNum = ZoneEquipConfig(CtrlZoneNum)%AirLoopNum
    If (AirLoopNum /= 0 ) then  ! deal with primary air system
        !loop over the zone supply air path inlet nodes

      DO ZoneInNum=1,ZoneEquipConfig(CtrlZoneNum)%NumInletNodes
        AirDistCoolInletNodeNum  = MAX(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode,0)
        AirDistHeatInletNodeNum  = MAX(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode,0)
         ! Set for cooling or heating path
        IF(AirDistCoolInletNodeNum > 0 .AND. AirDistHeatInletNodeNum == 0)THEN
          ADUCoolFlowrate = ADUCoolFlowrate & ! CR7244 need to accumulate flow across multiple inlets
                            + MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
        ELSEIF(AirDistHeatInletNodeNum > 0 .AND. AirDistCoolInletNodeNum == 0)THEN
          ADUHeatFlowrate = ADUHeatFlowrate & ! CR7244 need to accumulate flow across multiple inlets
                            + MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
        ELSEIF(AirDistCoolInletNodeNum > 0 .AND. AirDistHeatInletNodeNum > 0 .AND. &
               AirDistCoolInletNodeNum /= AirDistHeatInletNodeNum) THEN
           ! dual ducts! CR7244 need to accumulate flow across multiple inlets (don't count same inlet twice)
          ADUHeatFlowrate = ADUHeatFlowrate & ! CR7244 need to accumulate flow across multiple inlets
                            + MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitHeat(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
          ADUCoolFlowrate = ADUCoolFlowrate & ! CR7244 need to accumulate flow across multiple inlets
                            + MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
        ELSEIF(AirDistCoolInletNodeNum > 0 .AND. AirDistHeatInletNodeNum > 0) THEN
           ! dual ducts! CR7244 need to accumulate flow across multiple inlets (don't count same inlet twice)
          ADUCoolFlowrate = ADUCoolFlowrate & ! CR7244 need to accumulate flow across multiple inlets
                            + MAX(Node(ZoneEquipConfig(CtrlZoneNum)%AirDistUnitCool(ZoneInNum)%InNode)%MassFlowRate,0.0d0)
        ELSE
          ! do nothing (already inits)
        END IF
      END DO


        !Find the mixed air node and return air node of the system that supplies the zone
      MixedAirNode     = PrimaryAirSystem(AirLoopNum)%OASysOutletNodeNum
      ReturnAirNode    = PrimaryAirSystem(AirLoopNum)%OASysInletNodeNum
      IF(MixedAirNode == 0 .OR. ReturnAirNode == 0)  then
        AirSysZoneVentLoad = 0.0d0
        AirSysOutAirFlow   = 0.0d0
      ELSE
        !Calculate return and mixed air ethalpies
        AirSysEnthReturnAir = PsyHFnTdbW(Node(ReturnAirNode)%Temp, Node(ReturnAirNode)%HumRat)
        AirSysEnthMixedAir  = PsyHFnTdbW(Node(MixedAirNode)%Temp, Node(MixedAirNode)%HumRat)

        IF(PrimaryAirSystem(AirLoopNum)%OASysExists) THEN
          OutAirNode = PrimaryAirSystem(AirLoopNum)%OAMixOAInNodeNum
          AirSysOutAirFlow = Node(OutAirNode)%MassFlowRate
        ELSE
          AirSysOutAirFlow = 0.0d0
        END IF

        AirSysTotalMixFlowRate = Node(MixedAirNode)%MassFlowRate

        IF(AirSysTotalMixFlowRate .NE. 0.0d0) THEN
          ZoneFlowFrac = (ADUCoolFlowrate+ADUHeatFlowrate)/AirSysTotalMixFlowRate
          AirSysOutAirFlow  = ZoneFlowFrac * AirSysOutAirFlow
        ELSE
          ZoneFlowfrac = 0.0d0
          AirSysOutAirFlow  = 0.0d0
        END IF
        !Calculate the zone ventilation load for this supply air path (i.e. zone inlet)
        AirSysZoneVentLoad =   &
           (ADUCoolFlowrate+ADUHeatFlowrate)*(AirSysEnthMixedAir-AirSysEnthReturnAir)* TimeStepSys * SecInHour !*KJperJ
      ENDIF

    END IF ! primary air system present


    !now combine OA flow from zone forced air units with primary air system
    OutAirFlow  = AirSysOutAirFlow + ZFAUOutAirFlow
      ! assign report variables
    ZoneOAMassFlow(CtrlZoneNum) = OutAirFlow
    ZoneOAMass(CtrlZoneNum)     = ZoneOAMassFlow(CtrlZoneNum)* TimeStepSys* SecInHour

    ! determine volumetric values from mass flow using standard density (adjusted for elevation)
    ZoneOAVolFlowStdRho(CtrlZoneNum)  = ZoneOAMassFlow(CtrlZoneNum) / StdRhoAir
    ZoneOAVolStdRho(CtrlZoneNum)      = ZoneOAVolFlowStdRho(CtrlZoneNum) * TimeStepSys* SecInHour

    ! determine volumetric values from mass flow using current air density for zone (adjusted for elevation)
    currentZoneAirDensity =   PsyRhoAirFnPbTdbW(OutBaroPress, MAT(ActualZoneNum), ZoneAirHumRatAvg(ActualZoneNum))
    IF (currentZoneAirDensity > 0.0D0) ZoneOAVolFlowCrntRho(CtrlZoneNum)  = ZoneOAMassFlow(CtrlZoneNum) / currentZoneAirDensity
    ZoneOAVolCrntRho(CtrlZoneNum)  = ZoneOAVolFlowCrntRho(CtrlZoneNum) * TimeStepSys* SecInHour
    if (ZoneVolume > 0.0d0)  ZoneMechACH(CtrlZoneNum)    = (ZoneOAVolCrntRho(CtrlZoneNum) / TimeStepSys)/ZoneVolume

    !store data for predefined tabular report on outside air
    IF (ZonePreDefRep(ActualZoneNum)%isOccupied) THEN
      !accumulate the occupied time
      ZonePreDefRep(ActualZoneNum)%TotTimeOcc = ZonePreDefRep(ActualZoneNum)%TotTimeOcc + TimeStepSys
      !mechnical ventilation
      ZonePreDefRep(ActualZoneNum)%MechVentVolTotal = ZonePreDefRep(ActualZoneNum)%MechVentVolTotal + &
        ZoneOAVolCrntRho(CtrlZoneNum)
      IF ((ZoneOAVolCrntRho(CtrlZoneNum) / TimeStepSys) .LT. ZonePreDefRep(ActualZoneNum)%MechVentVolMin) THEN
        ZonePreDefRep(ActualZoneNum)%MechVentVolMin = ZoneOAVolCrntRho(CtrlZoneNum) / TimeStepSys
      END IF
      !infiltration
      ZonePreDefRep(ActualZoneNum)%InfilVolTotal = ZonePreDefRep(ActualZoneNum)%InfilVolTotal + &
        ZnAirRpt(ActualZoneNum)%InfilVolumeCurDensity
      IF (ZnAirRpt(ActualZoneNum)%InfilVolumeCurDensity .LT. ZonePreDefRep(ActualZoneNum)%InfilVolMin) THEN
        ZonePreDefRep(ActualZoneNum)%InfilVolMin = ZnAirRpt(ActualZoneNum)%InfilVolumeCurDensity
      END IF
      !'simple' mechanical ventilation
      ZonePreDefRep(ActualZoneNum)%SimpVentVolTotal = ZonePreDefRep(ActualZoneNum)%SimpVentVolTotal + &
        ZnAirRpt(ActualZoneNum)%VentilVolumeCurDensity
      IF (ZnAirRpt(ActualZoneNum)%VentilVolumeCurDensity .LT. ZonePreDefRep(ActualZoneNum)%SimpVentVolMin) THEN
        ZonePreDefRep(ActualZoneNum)%SimpVentVolMin = ZnAirRpt(ActualZoneNum)%VentilVolumeCurDensity
      END IF
    END IF

    !now combine Vent load from zone forced air units with primary air system
    ZoneVentLoad  = AirSysZoneVentLoad + ZFAUZoneVentLoad
    !cycle if ZoneVentLoad is small
    IF(ABS(ZoneVentLoad) < SmallLoad) CYCLE  ! orig. had RETURN here, BG changed to CYCLE for next controlled zone in do loop.

    !Ventilation Heating
    IF (ZoneVentLoad > SmallLoad)THEN
           !Zone cooling load
      IF(ZoneLoad < -SmallLoad)THEN
        MaxCoolingLoadAddedByVent(CtrlZoneNum) = MaxCoolingLoadAddedByVent(CtrlZoneNum) + ABS(ZoneVentLoad)
          !Zone heating load
      ELSEIF(ZoneLoad > SmallLoad)THEN
        IF(ZoneVentLoad > ZoneLoad)THEN
            MaxHeatingLoadMetByVent(CtrlZoneNum) = MaxHeatingLoadMetByVent(CtrlZoneNum) + ABS(ZoneLoad)
            MaxOverheatingByVent(CtrlZoneNum) = MaxOverheatingByVent(CtrlZoneNum) + &
            (ZoneVentLoad - ZoneLoad )
        ELSE
            MaxHeatingLoadMetByVent(CtrlZoneNum) = MaxHeatingLoadMetByVent(CtrlZoneNum) + ABS(ZoneVentLoad)
        ENDIF
            !No Zone Load
      ELSE
          MaxNoLoadHeatingByVent(CtrlZoneNum) = MaxNoLoadHeatingByVent(CtrlZoneNum) + ABS(ZoneVentLoad)
      ENDIF

        !Ventilation Cooling
    ELSEIF (ZoneVentLoad < -SmallLoad)THEN
            !Zone cooling load
      IF(ZoneLoad < -SmallLoad)THEN
        IF(ZoneVentLoad < ZoneLoad)THEN
          MaxCoolingLoadMetByVent(CtrlZoneNum) = MaxCoolingLoadMetByVent(CtrlZoneNum) + ABS(ZoneLoad)
          MaxOvercoolingByVent(CtrlZoneNum) = MaxOvercoolingByVent(CtrlZoneNum) + &
            ABS(ZoneVentLoad - ZoneLoad )
        ELSE
          MaxCoolingLoadMetByVent(CtrlZoneNum) = MaxCoolingLoadMetByVent(CtrlZoneNum) + ABS(ZoneVentLoad)
        ENDIF
             !Zone heating load
      ELSEIF(ZoneLoad > SmallLoad)THEN
        MaxHeatingLoadAddedByVent(CtrlZoneNum) = MaxHeatingLoadAddedByVent(CtrlZoneNum) + ABS(ZoneVentLoad)
        !No Zone Load
      ELSE
        MaxNoLoadCoolingByVent(CtrlZoneNum) = MaxNoLoadCoolingByVent(CtrlZoneNum) + ABS(ZoneVentLoad)
      ENDIF

   !Ventilation No Load
    ELSE
    ENDIF
  END DO  ! loop over controlled zones
  RETURN
END Subroutine ReportMaxVentilationLoads


SUBROUTINE MatchPlantSys(AirLoopNum,BranchNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   May 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate and report zone ventilation loads

          ! METHODOLOGY EMPLOYED:
          ! calculate energy contribution of outside air through mixing box and pro-rate to
          ! zones according to zone mass flow rates.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobalConstants

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)     ::  AirLoopNum           !counter for zone air distribution inlets
    INTEGER, INTENT(IN)     ::  BranchNum           !counter for zone air distribution inlets

          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER         :: EnergyTrans = 1
    INTEGER, PARAMETER         :: PrimaryAirLoop = 1

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    CHARACTER(len=MaxNameLength)  :: CompType
    CHARACTER(len=MaxNameLength)  :: CompName
    INTEGER     ::  CompNum             !counter for components on air loop branch connected to air distribution unit
    INTEGER     ::  VarNum
    INTEGER     ::  SubCompNum             !counter for components on air loop branch connected to air distribution unit
    INTEGER     ::  SubSubCompNum             !counter for components on air loop branch connected to air distribution unit
    LOGICAL     :: MatchFound    ! Set to .TRUE. when a match is found
    INTEGER     :: MatchLoop     ! Loop number of the match
    INTEGER     :: MatchBranch   ! Branch number of the match
    INTEGER     :: MatchComp     ! Component number of the match
    INTEGER     :: MatchLoopType
    INTEGER     :: Index


      DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
        DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumMeteredVars
          IF(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%MeteredVar(VarNum)%ResourceType ==   &
               iRT_EnergyTransfer)THEN
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%EnergyTransComp = EnergyTrans
            CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
            CompName = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%Name
            Index = 0
            CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
            IF(MatchFound)  &
               CALL UpdateAirSysCompPtrArray(Index,AirLoopNum,BranchNum,CompNum,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
            PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%AirSysToPlantPtr = Index
            EXIT
          END IF
        END DO
        DO SubCompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NumSubcomps
!!!!!          IF(SysVentLoad == 0.0d0)EXIT
          DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumMeteredVars
            IF(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%MeteredVar(VarNum)%ResourceType  &
                 == iRT_EnergyTransfer)THEN
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%EnergyTransComp = EnergyTrans
              CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%TypeOf
              CompName = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%Name
              Index = 0
              CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
              IF(MatchFound)  &
                 CALL UpdateAirSysSubCompPtrArray(Index,AirLoopNum,BranchNum,CompNum,SubCompNum,MatchLoopType,  &
                    MatchLoop,MatchBranch,MatchComp)
              PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%AirSysToPlantPtr = Index
            EXIT
            END IF
          END DO
          DO SubSubCompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%NumSubSubcomps
!!!!!            IF(SysVentLoad == 0.0d0)EXIT
            DO VarNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                                                   SubSubComp(SubSubCompNum)%NumMeteredVars
              IF(PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%SubSubComp(SubSubCompNum)%  &
                 MeteredVar(VarNum)%ResourceType == iRT_EnergyTransfer)THEN
                PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   SubSubComp(SubSubCompNum)%EnergyTransComp = EnergyTrans
                CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   SubSubComp(SubSubCompNum)%TypeOf
                CompName = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   SubSubComp(SubSubCompNum)%Name
                Index = 0
                CALL FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)
                IF(MatchFound)  &
                   CALL UpdateAirSysSubSubCompPtrArray(Index,AirLoopNum,BranchNum,CompNum,SubCompNum,SubSubCompNum,MatchLoopType,  &
                      MatchLoop,MatchBranch,MatchComp)
                PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%SubComp(SubCompNum)%  &
                   SubSubComp(SubSubCompNum)%AirSysToPlantPtr = Index
                EXIT
              END IF
            END DO
          END DO
        END DO
      END DO

END SUBROUTINE MatchPlantSys



SUBROUTINE FindDemandSideMatch(CompType,CompName,MatchFound,MatchLoopType,MatchLoop,MatchBranch,MatchComp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   September 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine intializes the connections between various loops.
          ! Due to the fact that this requires numerous string compares, it
          ! is much more efficient to find this information once and then
          ! store it in module level variables (LoopConnect derived type).

          ! METHODOLOGY EMPLOYED:
          ! Simply cycles through the plant and condenser demand sides until
          ! a component is found that matches the component type and name

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompType      ! Inlet node of the component to find the match of
  CHARACTER(len=*), INTENT(IN)  :: CompName      ! Outlet node of the component to find the match of
  LOGICAL, INTENT(OUT) :: MatchFound    ! Set to .TRUE. when a match is found
  INTEGER, INTENT(OUT) :: MatchLoopType     ! Loop number of the match
  INTEGER, INTENT(OUT) :: MatchLoop     ! Loop number of the match
  INTEGER, INTENT(OUT) :: MatchBranch   ! Branch number of the match
  INTEGER, INTENT(OUT) :: MatchComp     ! Component number of the match

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na



          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PassBranchNum      ! DO loop counter for branches
  INTEGER :: PassCompNum        ! DO loop counter for components
  INTEGER :: PassLoopNum        ! DO loop counter for loops or the top level of the hierarchy
          ! FLOW:
          ! Initialize all of the output variables
  MatchFound  = .FALSE.
  MatchLoopType = 0
  MatchLoop     = 0
  MatchLoop     = 0
  MatchBranch   = 0
  MatchComp     = 0

          ! Now cycle through all of the demand side loops to see if we can find
          ! a match for the component type and name.  Once a match is found,
          ! record the type of loop and the loop, branch, and component numbers.
  IF (.NOT. MatchFound) THEN ! Go through the plant demand side loops
    DO PassLoopNum = 1, NumPlantLoops
      DO PassBranchNum = 1, VentRepPlantDemandSide(PassLoopNum)%TotalBranches
        DO PassCompNum = 1, VentRepPlantDemandSide(PassLoopNum)%Branch(PassBranchNum)%TotalComponents
          IF ( SameString(CompType,VentRepPlantDemandSide(PassLoopNum)%Branch(PassBranchNum)%Comp(PassCompNum)%TypeOf) .AND. &
               SameString(CompName,VentRepPlantDemandSide(PassLoopNum)%Branch(PassBranchNum)%Comp(PassCompNum)%Name) ) THEN
            ! Found a match on the plant demand side--increment the counter
            MatchFound  = .TRUE.
            MatchLoopType = 1
            MatchLoop   = PassLoopNum
            MatchBranch = PassBranchNum
            MatchComp   = PassCompNum
            EXIT    ! PassCompNum DO loop
          END IF
        END DO
        IF (MatchFound) EXIT ! PassBranchNum DO loop
      END DO
      IF (MatchFound) EXIT   ! PassLoopNum DO loop
    END DO
  END IF

  IF (.NOT. MatchFound) THEN ! Go through the condenser demand side loops
    DO PassLoopNum = 1, NumCondLoops
      DO PassBranchNum = 1, VentRepCondDemandSide(PassLoopNum)%TotalBranches
        DO PassCompNum = 1, VentRepCondDemandSide(PassLoopNum)%Branch(PassBranchNum)%TotalComponents
          IF ( SameString(CompType,VentRepCondDemandSide(PassLoopNum)%Branch(PassBranchNum)%Comp(PassCompNum)%TypeOf) .AND. &
               SameString(CompName,VentRepCondDemandSide(PassLoopNum)%Branch(PassBranchNum)%Comp(PassCompNum)%Name) ) THEN
            ! Found a match on the plant demand side--increment the counter
            MatchFound  = .TRUE.
            MatchLoopType = 2
            MatchLoop   = PassLoopNum
            MatchBranch = PassBranchNum
            MatchComp   = PassCompNum
            EXIT    ! PassCompNum DO loop
          END IF
        END DO
        IF (MatchFound) EXIT ! PassBranchNum DO loop
      END DO
      IF (MatchFound) EXIT   ! PassLoopNum DO loop
    END DO
  END IF

  RETURN

END SUBROUTINE FindDemandSideMatch

SUBROUTINE ReportAirLoopConnections

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, Linda K. Lawrie
          !       DATE WRITTEN   February 2004 (moved from BranchInputManager ReportLoopConnections)
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Report air loop splitter connections to the BND file.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: OutputFileBNDetails
  USE DataHVACGlobals, ONLY: NumPrimaryAirSys
  USE DataHeatBalance, ONLY: Zone


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: errstring='**error**'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Count
  INTEGER Count1
  INTEGER CtrldZoneNum
  INTEGER ZoneNum
  CHARACTER(len=20) ChrOut
  CHARACTER(len=20) ChrOut2
  CHARACTER(len=20) ChrOut3
  CHARACTER(len=20) ChrOut4
  CHARACTER(len=20) ChrOut5

 701 FORMAT(A)
 706 FORMAT('! <#AirLoopHVACs>,<Number of AirLoopHVACs>')
 707 FORMAT(1X,A)
 708 FORMAT('! <AirLoopHVAC>,<Air Loop Name>,<# Return Nodes>,<# Supply Nodes>,',  &
            '<# Zones Cooled>,<# Zones Heated>,<Outdoor Air Used>')
 709 FORMAT('! <AirLoop Return Connections>,<Connection Count>,<AirLoopHVAC Name>,', &
            '<Zn Eqp Return Node #>,<Zn Eqp Return Node Name>,',  &
            '<AirLoop Return Node #>,<Air Loop Return Node Name>')
 710 FORMAT('! <AirLoop Supply Connections>,<Connection Count>,<AirLoopHVAC Name>,', &
            '<Zn Eqp Supply Node #>,<Zn Eqp Supply Node Name>,',  &
            '<AirLoop Supply Node #>,<Air Loop Supply Node Name>')
 711 FORMAT('! <Cooled Zone Info>,<Cooled Zone Count>,<Cooled Zone Name>,',  &
            '<Cooled Zone Inlet Node #>,<Cooled Zone Inlet Node Name>,<AirLoopHVAC Name>')
 712 FORMAT('! <Heated Zone Info>,<Heated Zone Count>,<Heated Zone Name>,',  &
            '<Heated Zone Inlet Node #>,<Heated Zone Inlet Node Name>,<AirLoopHVAC Name>')
 714 FORMAT('! <Outdoor Air Connections>,<OA Inlet Node #>,<OA Return Air Inlet Node Name>,',  &
            '<OA Outlet Node #>,<OA Mixed Air Outlet Node Name>,<AirLoopHVAC Name>')
 713 FORMAT(A)
  WRITE(OutputFileBNDetails,701) '! ==============================================================='
  WRITE(OutputFileBNDetails,706)
  WRITE(ChrOut,*) NumPrimaryAirSys
  WRITE(OutputFileBNDetails,707) '#AirLoopHVACs,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,708)
  WRITE(OutputFileBNDetails,709)
  WRITE(OutputFileBNDetails,710)
  WRITE(OutputFileBNDetails,711)
  WRITE(OutputFileBNDetails,712)
  WRITE(OutputFileBNDetails,714)
  WRITE(OutputFileBNDetails,713) '! <AirLoopHVAC Connector>,<Connector Type>,<Connector Name>,'// &
                                 '<Loop Name>,<Loop Type>,<Number of Inlets/Outlets>'
  WRITE(OutputFileBNDetails,713) '! <AirLoopHVAC Connector Branches>,<Connector Node Count>,<Connector Type>,'// &
                                 '<Connector Name>,<Inlet Branch>,<Outlet Branch>,'// &
                                 '<Loop Name>,<Loop Type>'
  WRITE(OutputFileBNDetails,713) '! <AirLoopHVAC Connector Nodes>,<Connector Node Count>,<Connector Type>,'// &
                                 '<Connector Name>,<Inlet Node>,<Outlet Node>,'// &
                                 '<Loop Name>,<Loop Type>'
  DO Count=1,NumPrimaryAirSys
    WRITE(ChrOut,*) AirToZoneNodeInfo(Count)%NumReturnNodes
    WRITE(ChrOut2,*) AirToZoneNodeInfo(Count)%NumSupplyNodes
    WRITE(ChrOut3,*) AirToZoneNodeInfo(Count)%NumZonesCooled
    WRITE(ChrOut4,*) AirToZoneNodeInfo(Count)%NumZonesHeated
    ChrOut=ADJUSTL(ChrOut)
    ChrOut2=ADJUSTL(ChrOut2)
    ChrOut3=ADJUSTL(ChrOut3)
    ChrOut4=ADJUSTL(ChrOut4)
    IF (AirToOANodeInfo(Count)%OASysExists) THEN
      ChrOut5='Yes'
    ELSE
      ChrOut5='No'
    ENDIF
    WRITE(OutputFileBNDetails,701) ' AirLoopHVAC,'//TRIM(AirToZoneNodeInfo(Count)%AirLoopName)//','// &
                      TRIM(ChrOut)//','//TRIM(ChrOut2)//','//TRIM(ChrOut3)//','//TRIM(ChrOut4)//','//TRIM(ChrOut5)
    DO Count1=1,AirToZoneNodeInfo(Count)%NumReturnNodes
      WRITE(ChrOut,*) Count1
      IF (AirToZoneNodeInfo(Count)%ZoneEquipReturnNodeNum(Count1) > 0) THEN
        WRITE(ChrOut2,*) AirToZoneNodeInfo(Count)%ZoneEquipReturnNodeNum(Count1)
      ELSE
        ChrOut2=errstring
      ENDIF
      IF (AirToZoneNodeInfo(Count)%AirLoopReturnNodeNum(Count1) > 0) THEN
        WRITE(ChrOut3,*) AirToZoneNodeInfo(Count)%AirLoopReturnNodeNum(Count1)
      ELSE
        ChrOut3=errstring
      ENDIF
      ChrOut=ADJUSTL(ChrOut)
      ChrOut2=ADJUSTL(ChrOut2)
      ChrOut3=ADJUSTL(ChrOut3)
      WRITE(OutputFileBNDetails,707,advance='No') '  AirLoop Return Connections,'//TRIM(ChrOut)//','//  &
            TRIM(AirToZoneNodeInfo(Count)%AirLoopName)//','
      IF (ChrOut2 /= errstring) then
        WRITE(OutputFileBNDetails,701,advance='No') TRIM(ChrOut2)//','//  &
                    TRIM(NodeID(AirToZoneNodeInfo(Count)%ZoneEquipReturnNodeNum(Count1)))//','
      ELSE
        WRITE(OutputFileBNDetails,701,advance='No') TRIM(errstring)//','//TRIM(errstring)//','
      ENDIF
      IF (ChrOut3 /= errstring) then
        WRITE(OutputFileBNDetails,701) TRIM(ChrOut3)//','//  &
                    TRIM(NodeID(AirToZoneNodeInfo(Count)%AirLoopReturnNodeNum(Count1)))
      ELSE
        WRITE(OutputFileBNDetails,701) TRIM(errstring)//','//TRIM(errstring)
      ENDIF
    ENDDO
    DO Count1=1,AirToZoneNodeInfo(Count)%NumSupplyNodes
      WRITE(ChrOut,*) Count1
      IF (AirToZoneNodeInfo(Count)%ZoneEquipSupplyNodeNum(Count1) > 0) THEN
        WRITE(ChrOut2,*) AirToZoneNodeInfo(Count)%ZoneEquipSupplyNodeNum(Count1)
      ELSE
        ChrOut2=errstring
      ENDIF
      IF (AirToZoneNodeInfo(Count)%AirLoopSupplyNodeNum(Count1) > 0) THEN
        WRITE(ChrOut3,*) AirToZoneNodeInfo(Count)%AirLoopSupplyNodeNum(Count1)
      ELSE
        ChrOut3=errstring
      ENDIF
      ChrOut=ADJUSTL(ChrOut)
      ChrOut2=ADJUSTL(ChrOut2)
      ChrOut3=ADJUSTL(ChrOut3)
      WRITE(OutputFileBNDetails,707,advance='No') '  AirLoop Supply Connections,'//TRIM(ChrOut)//','//  &
            TRIM(AirToZoneNodeInfo(Count)%AirLoopName)//','
      IF (ChrOut2 /= errstring) then
        WRITE(OutputFileBNDetails,701,advance='No') TRIM(ChrOut2)//','//  &
                    TRIM(NodeID(AirToZoneNodeInfo(Count)%ZoneEquipSupplyNodeNum(Count1)))//','
      ELSE
        WRITE(OutputFileBNDetails,701,advance='No') TRIM(errstring)//','//TRIM(errstring)//','
      ENDIF
      IF (ChrOut3 /= errstring) then
        WRITE(OutputFileBNDetails,701) TRIM(ChrOut3)//','//  &
                    TRIM(NodeID(AirToZoneNodeInfo(Count)%AirLoopSupplyNodeNum(Count1)))
      ELSE
        WRITE(OutputFileBNDetails,701) TRIM(errstring)//','//TRIM(errstring)
      ENDIF
    ENDDO

    DO Count1=1,AirToZoneNodeInfo(Count)%NumZonesCooled
      WRITE(ChrOut,*) Count1
      IF (AirToZoneNodeInfo(Count)%CoolZoneInletNodes(Count1) > 0) THEN
        WRITE(ChrOut2,*) AirToZoneNodeInfo(Count)%CoolZoneInletNodes(Count1)
      ELSE
        ChrOut2=errstring
      ENDIF
      ChrOut=ADJUSTL(ChrOut)
      ChrOut2=ADJUSTL(ChrOut2)
      CtrldZoneNum = AirToZoneNodeInfo(Count)%CoolCtrlZoneNums(Count1)
      ZoneNum = ZoneEquipConfig(CtrldZoneNum)%ActualZoneNum
      WRITE(OutputFileBNDetails,707,advance='No') '  Cooled Zone Info,'//TRIM(ChrOut)//','//  &
            TRIM(Zone(ZoneNum)%Name)//','
      IF (ChrOut2 /= errstring) then
        WRITE(OutputFileBNDetails,701) TRIM(ChrOut2)//','//  &
                    TRIM(NodeID(AirToZoneNodeInfo(Count)%CoolZoneInletNodes(Count1)))//','//  &
                    TRIM(AirToZoneNodeInfo(Count)%AirLoopName)
      ELSE
        WRITE(OutputFileBNDetails,701) TRIM(errstring)//','//TRIM(errstring)//','//TRIM(AirToZoneNodeInfo(Count)%AirLoopName)
      ENDIF
    ENDDO
    DO Count1=1,AirToZoneNodeInfo(Count)%NumZonesHeated
      WRITE(ChrOut,*) Count1
      IF (AirToZoneNodeInfo(Count)%HeatZoneInletNodes(Count1) > 0) THEN
        WRITE(ChrOut2,*) AirToZoneNodeInfo(Count)%HeatZoneInletNodes(Count1)
      ELSE
        ChrOut2=errstring
      ENDIF
      ChrOut=ADJUSTL(ChrOut)
      ChrOut2=ADJUSTL(ChrOut2)
      CtrldZoneNum = AirToZoneNodeInfo(Count)%HeatCtrlZoneNums(Count1)
      ZoneNum = ZoneEquipConfig(CtrldZoneNum)%ActualZoneNum
      WRITE(OutputFileBNDetails,707,advance='No') '  Heated Zone Info,'//TRIM(ChrOut)//','//  &
            TRIM(Zone(ZoneNum)%Name)//','
      IF (ChrOut2 /= errstring) THEN
        WRITE(OutputFileBNDetails,701) TRIM(ChrOut2)//','//  &
                    TRIM(NodeID(AirToZoneNodeInfo(Count)%HeatZoneInletNodes(Count1)))//','//  &
                    TRIM(AirToZoneNodeInfo(Count)%AirLoopName)
      ELSE
        WRITE(OutputFileBNDetails,701) TRIM(errstring)//','//TRIM(errstring)//','//TRIM(AirToZoneNodeInfo(Count)%AirLoopName)
      ENDIF
    ENDDO
    IF (AirToOANodeInfo(Count)%OASysExists) THEN
      IF (AirToOANodeInfo(Count)%OASysInletNodeNum > 0) THEN
        WRITE(ChrOut,*) AirToOANodeInfo(Count)%OASysInletNodeNum
      ELSE
        ChrOut=errstring
      ENDIF
      IF (AirToOANodeInfo(Count)%OASysOutletNodeNum > 0) THEN
        WRITE(ChrOut2,*) AirToOANodeInfo(Count)%OASysOutletNodeNum
      ELSE
        ChrOut2=errstring
      ENDIF
      ChrOut=ADJUSTL(ChrOut)
      ChrOut2=ADJUSTL(ChrOut2)
      WRITE(OutputFileBNDetails,707,advance='No') '  Outdoor Air Connections,'//TRIM(ChrOut)//','
      IF (ChrOut /= errstring) THEN
        WRITE(OutputFileBNDetails,701,advance='No') TRIM(NodeID(AirToOANodeInfo(Count)%OASysInletNodeNum))//','
      ELSE
        WRITE(OutputFileBNDetails,701,advance='No') TRIM(errstring)//','
      ENDIF
      IF (ChrOut2 /= errstring) THEN
        WRITE(OutputFIleBNDetails,701)  TRIM(ChrOut2)//','// &
            TRIM(NodeID(AirToOANodeInfo(Count)%OASysOutletNodeNum))//','// &
            TRIM(AirToZoneNodeInfo(Count)%AirLoopName)
      ELSE
        WRITE(OutputFileBNDetails,701) TRIM(errstring)//','//TRIM(errstring)//','//TRIM(AirToZoneNodeInfo(Count)%AirLoopName)
      ENDIF
    ENDIF
          !  Report HVAC Air Loop Splitter to BND file
    IF (PrimaryAirSystem(Count)%Splitter%Exists) THEN
      WRITE(ChrOut,*) PrimaryAirSystem(Count)%Splitter%TotalOutletNodes
      WRITE(OutputFileBNDetails,701) '   AirLoopHVAC Connector,Splitter,'// &
            TRIM(PrimaryAirSystem(Count)%Splitter%Name)//','//  &
            TRIM(PrimaryAirSystem(Count)%Name)//',Air,'//  &
            TRIM(ADJUSTL(ChrOut))
      DO Count1=1,PrimaryAirSystem(Count)%Splitter%TotalOutletNodes
        WRITE(ChrOut,*) Count1
        IF (PrimaryAirSystem(Count)%Splitter%BranchNumIn <= 0) THEN
          ChrOut2=errstring
        ENDIF
        IF (PrimaryAirSystem(Count)%Splitter%BranchNumOut(Count1) <= 0) THEN
          ChrOut3=errstring
        ENDIF
        WRITE(OutputFileBNDetails,701,advance='No') '     AirLoopHVAC Connector Branches,'//TRIM(ADJUSTL(ChrOut))//  &
                                                    ',Splitter,'//TRIM(PrimaryAirSystem(Count)%Splitter%Name)//','
        IF (ChrOut2 /= errstring) THEN
          WRITE(OutputFileBNDetails,701,advance='No')   &
             TRIM(PrimaryAirSystem(Count)%Branch(PrimaryAirSystem(Count)%Splitter%BranchNumIn)%Name)//','
        ELSE
          WRITE(OutputFileBNDetails,701,advance='No') TRIM(ChrOut2)//','
        ENDIF
        IF (ChrOut3 /= errstring) THEN
          WRITE(OutputFileBNDetails,701)   &
              TRIM(PrimaryAirSystem(Count)%Branch(PrimaryAirSystem(Count)%Splitter%BranchNumOut(Count1))%Name)//','// &
              TRIM(PrimaryAirSystem(Count)%Name)//',Air'
        ELSE
          WRITE(OutputFileBNDetails,701)   &
              TRIM(ChrOut3)//','// &
              TRIM(PrimaryAirSystem(Count)%Name)//',Air'
        ENDIF
        WRITE(OutputFileBNDetails,701) '     AirLoopHVAC Connector Nodes,   '//TRIM(ADJUSTL(ChrOut))//',Splitter,'// &
              TRIM(PrimaryAirSystem(Count)%Splitter%Name)//','//  &
              TRIM(PrimaryAirSystem(Count)%Splitter%NodeNameIn)//','//  &
              TRIM(PrimaryAirSystem(Count)%Splitter%NodeNameOut(Count1))//','//  &
              TRIM(PrimaryAirSystem(Count)%Name)//',Air'
      ENDDO
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportAirLoopConnections

!        End of Reporting subroutines for the SimAir Module
! *****************************************************************************

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

END MODULE SystemReports
