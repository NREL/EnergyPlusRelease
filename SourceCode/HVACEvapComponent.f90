Module EvaporativeCoolers
  ! Module containing the EvaporativeCoolers simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   Oct 2000
  !       MODIFIED       BG July 2003 ResearchSpecial Indirect
  !                      BG Febraury 2007 outside air nodes
  !                      BG March 2009 ResearchSpecial Direct
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required for
  ! Evaporative Coolers Components for use in mechanical air systems

  ! METHODOLOGY EMPLOYED:
  ! various evaporative component models in this module
  !   different models share common module level data structure.
  !

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: BeginEnvrnFlag,BeginDayFlag, SysSizingCalc, SecInHour, ScheduleAlwaysOn
USE DataInterfaces, ONLY: ShowContinueError, ShowSevereError,ShowFatalError, SetupOutputVariable, &
                          ShowWarningError
USE DataLoopNode
Use DataEnvironment, ONLY: OUTBAROPRESS
USE ScheduleManager
Use Psychrometrics
USE DataGlobalConstants
  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: WaterSupplyFromMains = 101
INTEGER, PARAMETER :: WaterSupplyFromTank  = 102

  ! DERIVED TYPE DEFINITIONS
TYPE EvapConditions
  CHARACTER(len=MaxNameLength) :: EvapCoolerName  = ' ' ! Name of the EvapCooler
  INTEGER                      :: EquipIndex      = 0
  INTEGER                      :: EvapCoolerType  = 0 ! Type of the EvapCooler (parameters in DataGlobalConstants.f90
  CHARACTER(len=MaxNameLength) :: EvapControlType = ' ' ! Type of Control for the EvapCooler
  CHARACTER(len=MaxNameLength) :: Schedule        = ' ' ! HeatingCoil Operation Schedule
  Integer      :: SchedPtr                        = 0   ! Pointer to the correct schedule
  REAL(r64)    :: VolFlowRate                     = 0.0D0 !Volume Flow Rate in Evap Cooler needed for calculating SatEff
  REAL(r64)    :: OutletTemp                      = 0.0D0 !
  REAL(r64)    :: OuletWetBulbTemp                = 0.0D0 !
  REAL(r64)    :: OutletHumRat                    = 0.0D0 !
  REAL(r64)    :: OutletEnthalpy                  = 0.0D0 !
  REAL(r64)    :: OutletPressure                  = 0.0D0 !
  REAL(r64)    :: OutletMassFlowRate              = 0.0D0 !MassFlow through the EvapCooler being Simulated [kg/Sec]
  REAL(r64)    :: OutletMassFlowRateMaxAvail      = 0.0D0 ! [kg/Sec]
  REAL(r64)    :: OutletMassFlowRateMinAvail      = 0.0D0 ! [kg/Sec]
  LOGICAL      :: InitFlag                        = .FALSE. !
  INTEGER      :: InletNode                       = 0   !
  INTEGER      :: OutletNode                      = 0   !
  INTEGER      :: SecondaryInletNode              = 0   ! This is usually OA node feeding into the purge/secondary side
  INTEGER      :: TertiaryInletNode               = 0   ! This node is used to run building exhaust into purge side.
  REAL(r64)    :: InletMassFlowRate               = 0.0D0 ! Inlet is primary process air node at inlet to cooler
  REAL(r64)    :: InletMassFlowRateMaxAvail       = 0.0D0 !
  REAL(r64)    :: InletMassFlowRateMinAvail       = 0.0D0 !
  REAL(r64)    :: InletTemp                       = 0.0D0 !
  REAL(r64)    :: InletWetBulbTemp                = 0.0D0 !
  REAL(r64)    :: InletHumRat                     = 0.0D0 !
  REAL(r64)    :: InletEnthalpy                   = 0.0D0 !
  REAL(r64)    :: InletPressure                   = 0.0D0 !
  REAL(r64)    :: SecInletMassFlowRate            = 0.0D0 ! Secondary inlet is for indirect coolers
  REAL(r64)    :: SecInletMassFlowRateMaxAvail    = 0.0D0 !
  REAL(r64)    :: SecInletMassFlowRateMinAvail    = 0.0D0 !
  REAL(r64)    :: SecInletTemp                    = 0.0D0 !
  REAL(r64)    :: SecInletWetBulbTemp             = 0.0D0 !
  REAL(r64)    :: SecInletHumRat                  = 0.0D0 !
  REAL(r64)    :: SecInletEnthalpy                = 0.0D0 !
  REAL(r64)    :: SecInletPressure                = 0.0D0 !
  REAL(r64)    :: PadDepth                        = 0.0D0 !
  REAL(r64)    :: PadArea                         = 0.0D0 !
  REAL(r64)    :: RecircPumpPower                 = 0.0D0 !
  REAL(r64)    :: IndirectRecircPumpPower         = 0.0D0 !
  REAL(r64)    :: IndirectPadDepth                = 0.0D0 !
  REAL(r64)    :: IndirectPadArea                 = 0.0D0 !
  REAL(r64)    :: IndirectVolFlowRate             = 0.0D0 !
  REAL(r64)    :: IndirectFanEff                  = 0.0D0 !
  REAL(r64)    :: IndirectFanDeltaPress           = 0.0D0 !
  REAL(r64)    :: IndirectHXEffectiveness         = 0.0D0 !
  REAL(r64)    :: DirectEffectiveness             = 0.0D0 ! input saturation effectiveness for constant effectiveness model
  REAL(r64)    :: WetCoilMaxEfficiency            = 0.0D0 !
  REAL(r64)    :: WetCoilFlowRatio                = 0.0D0 !
  REAL(r64)    :: EvapCoolerEnergy                = 0.0D0 !
  REAL(r64)    :: EvapCoolerPower                 = 0.0D0 !

  INTEGER      :: EvapWaterSupplyMode             = WaterSupplyFromMains !  where does water come from
  CHARACTER(len=MaxNameLength) :: EvapWaterSupplyName = ' ' ! name of water source e.g. water storage tank
  INTEGER      :: EvapWaterSupTankID              = 0 !
  INTEGER      :: EvapWaterTankDemandARRID        = 0 !
  REAL(r64)    :: DriftFraction                   = 0.0D0 ! excess water from drift as fraction of Evap Water Consumption rate
  REAL(r64)    :: BlowDownRatio                   = 0.0D0 ! excess water use for blowdown as solids ratio to be maintained

  REAL(r64)    :: EvapWaterConsumpRate            = 0.0D0 ! Evap Water Consumption rate in m3/sec
  REAL(r64)    :: EvapWaterConsump                = 0.0D0 ! Evap Water Consumption in m3
  REAL(r64)    :: EvapWaterStarvMakup             = 0.0D0 ! Evap water consumed but not really available from tank m3
  REAL(r64)    :: SatEff                          = 0.0D0 !Reporting for Direct Stage and Ind Dry Saturation Efficiency
  REAL(r64)    :: StageEff                        = 0.0D0 !Reporting for Indirect Total Stage Efficiency
  REAL(r64)    :: DPBoundFactor                   = 0.0D0 ! in RDDSpecial efficency w.r.t. dewpoint
  Integer      :: EvapControlNodeNum              = 0   ! need to control to avoid over cooling
  REAL(r64)    :: DesiredOutletTemp               = 0.0D0 ! setpoint manager should set this
  REAL(r64)    :: PartLoadFract                   = 0.0D0 ! reduces cooling performance and associated fan power
  Integer      :: DewPointBoundFlag               = 0   ! report when indirect research special cooler is bound by dewpoint
                                                        ! rather than wetbulb-depression approach
END TYPE EvapConditions

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER     :: NumEvapCool=0             ! The Number of Evap Coolers found in the Input
  TYPE (EvapConditions), ALLOCATABLE, DIMENSION(:) :: EvapCond
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE EvapCoolers
PUBLIC  SimEvapCooler
PRIVATE GetEvapInput
PRIVATE InitEvapCooler
PRIVATE SizeEvapCooler
PRIVATE CalcDirectEvapCooler
PRIVATE CalcDryIndirectEvapCooler
PRIVATE CalcWetIndirectEvapCooler
PRIVATE CalcResearchSpecialPartLoad
PRIVATE CalcIndirectResearchSpecialEvapCooler
PRIVATE CalcDirectResearchSpecialEvapCooler
PRIVATE UpdateEvapCooler
PRIVATE ReportEvapCooler

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimEvapCooler(CompName,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages EvapCooler component simulation.
          ! It is called from the SimAirLoopComponent
          ! at the system time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER, INTENT(INOUT)       :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: EvapCoolNum     ! The EvapCooler that you are currently loading input into
  LOGICAL,SAVE      :: GetInputFlag = .True. ! Flag set to make sure you get input once

          ! FLOW:

  ! Obtains and Allocates EvapCooler related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetEvapInput
    GetInputFlag=.false.
  End If


  ! Find the correct EvapCoolNumber
  IF (CompIndex == 0) THEN
    EvapCoolNum=FindItemInList(CompName,EvapCond%EvapCoolerName,NumEvapCool)
    IF (EvapCoolNum == 0) THEN
      CALL ShowFatalError('SimEvapCooler: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=EvapCoolNum
  ELSE
    EvapCoolNum=CompIndex
    IF (EvapCoolNum > NumEvapCool .or. EvapCoolNum < 1) THEN
      CALL ShowFatalError('SimEvapCooler:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(EvapCoolNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumEvapCool))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(EvapCoolNum)) THEN
      IF (CompName /= EvapCond(EvapCoolNum)%EvapCoolerName) THEN
        CALL ShowFatalError('SimEvapCooler: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(EvapCoolNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(EvapCond(EvapCoolNum)%EvapCoolerName))
      ENDIF
      CheckEquipName(EvapCoolNum)=.false.
    ENDIF
  ENDIF

  ! With the correct EvapCoolNum Initialize
  CALL InitEvapCooler(EvapCoolNum)  ! Initialize all related parameters

  SELECT CASE (EvapCond(EvapCoolNum)%EvapCoolerType)

  CASE (iEvapCoolerDirectCELDEKPAD)
    CALL CalcDirectEvapCooler(EvapCoolNum)
  CASE (iEvapCoolerInDirectCELDEKPAD)
    CALL CalcDryIndirectEvapCooler(EvapCoolNum)
  CASE ( iEvapCoolerInDirectWETCOIL)
    CALL CalcWetIndirectEvapCooler(EvapCoolNum)
  CASE (iEvapCoolerInDirectRDDSpecial)
    CALL CalcResearchSpecialPartLoad(EvapCoolNum)
    CALL CalcIndirectResearchSpecialEvapCooler(EvapCoolNum)
  CASE (iEvapCoolerDirectResearchSpecial)
    CALL CalcResearchSpecialPartLoad(EvapCoolNum)
    CALL CalcDirectResearchSpecialEvapCooler(EvapCoolNum)
  END SELECT
  ! Update the current Evap Cooler to the outlet nodes
  CALL UpdateEvapCooler(EvapCoolNum)

  ! Report the current Evap Cooler
  CALL ReportEvapCooler(EvapCoolNum)

  RETURN

END SUBROUTINE SimEvapCooler


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetEvapInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       BTG,  adding in EVAPCOOLER:INDIRECT:RDDSPECIAL
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,VerifyName
    USE DataIPShortCuts  ! Data for field names, blank numerics
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet
    USE WaterManager, ONLY: SetupTankDemandComponent
    USE OutAirNodeManager,  ONLY: CheckOutAirNodeNumber

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: EvapCoolNum       ! The EvapCooler that you are currently loading input into
    INTEGER :: NumDirectEvapCool ! The number of Direct CelDek EvapCooler in this simulation
    INTEGER :: NumDryInDirectEvapCool ! The number of dry indirect evap coolers
    INTEGER :: NumWetInDirectEvapCool ! The number of wet indirect evap coolers
    INTEGER :: NumRDDEvapCool   ! the number of special research indirect evap coolers
    INTEGER :: NumDirectResearchSpecialEvapCool ! the number of special research direct evap coolers

    INTEGER :: IndEvapCoolNum    !Do Loop Counter for indirect evap coolers
    INTEGER :: DirectEvapCoolNum ! Do loop counter for direct evap cooler
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound=.false.
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name

   ! Start getting the input data
    NumDirectEvapCool      = GetNumObjectsFound('EvaporativeCooler:Direct:CelDekPad')
    NumDryInDirectEvapCool = GetNumObjectsFound('EvaporativeCooler:Indirect:CelDekPad')
    NumWetInDirectEvapCool = GetNumObjectsFound('EvaporativeCooler:Indirect:WetCoil')
    NumRDDEvapCool         = GetNumObjectsFound('EvaporativeCooler:Indirect:ResearchSpecial')
    NumDirectResearchSpecialEvapCool = GetNumObjectsFound('EvaporativeCooler:Direct:ResearchSpecial')

    !Sum up all of the Evap Cooler Types
    NumEvapCool = NumDirectEvapCool + NumDryInDirectEvapCool + NumWetInDirectEvapCool + NumRDDEvapCool &
                  + NumDirectResearchSpecialEvapCool

    IF (NumEvapCool.GT.0) ALLOCATE(EvapCond(NumEvapCool))
    ALLOCATE(CheckEquipName(NumEvapCool))
    CheckEquipName=.true.

    cCurrentModuleObject = 'EvaporativeCooler:Direct:CelDekPad'

    DO EvapCoolNum = 1,  NumDirectEvapCool
      CALL GetObjectItem(cCurrentModuleObject,EvapCoolNum,cAlphaArgs,NumAlphas, &
                      rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EvapCond%EvapCoolerName,EvapCoolNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      EvapCond(EvapCoolNum)%EvapCoolerName = Trim(cAlphaArgs(1))
      EvapCond(EvapCoolNum)%EvapCoolerType = iEvapCoolerDirectCELDEKPAD

      EvapCond(EvapCoolNum)%Schedule = cAlphaArgs(2)
      IF (lAlphaFieldBlanks(2)) THEN
        EvapCond(EvapCoolNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        EvapCond(EvapCoolNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (EvapCond(EvapCoolNum)%SchedPtr == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      EvapCond(EvapCoolNum)%InletNode  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%OutletNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4), &
                       'Evap Air Nodes')

      EvapCond(EvapCoolNum)%EvapControlType = TRIM(cAlphaArgs(5))

      !input the numerical data
      EvapCond(EvapCoolNum)%PadArea         = rNumericArgs(1)
      EvapCond(EvapCoolNum)%PadDepth        = rNumericArgs(2)
      EvapCond(EvapCoolNum)%RecircPumpPower = rNumericArgs(3)

      CALL SetupOutputVariable('Evaporative Cooler Wet Bulb Effectiveness []',EvapCond(EvapCoolNum)%Sateff, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)

           ! A6 ; \Field Name of Water Supply Storage Tank
      EvapCond(EvapCoolNum)%EvapWaterSupplyName = cAlphaArgs(6)
      IF (lAlphaFieldBlanks(6)) THEN
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromTank
         CALL SetupTankDemandComponent(EvapCond(EvapCoolNum)%EvapCoolerName, TRIM(cCurrentModuleObject), &
                    EvapCond(EvapCoolNum)%EvapWaterSupplyName, ErrorsFound, EvapCond(EvapCoolNum)%EvapWaterSupTankID, &
                    EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID )

      ENDIF

    END DO   ! end Number of EvapCooler Loop

    !**************************************************************
    !This is the start of the Dry Indirect Evap Cooler Loop
    cCurrentModuleObject = 'EvaporativeCooler:Indirect:CelDekPad'

    DO IndEvapCoolNum = 1,  NumDryInDirectEvapCool
      EvapCoolNum = NumDirectEvapCool + IndEvapCoolNum

      CALL GetObjectItem(cCurrentModuleObject,IndEvapCoolNum,cAlphaArgs,NumAlphas, &
                      rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EvapCond%EvapCoolerName,EvapCoolNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      EvapCond(EvapCoolNum)%EvapCoolerName = Trim(cAlphaArgs(1))
      EvapCond(EvapCoolNum)%EvapCoolerType = iEvapCoolerInDirectCELDEKPAD !'EvaporativeCooler:Indirect:CelDekPad'

      EvapCond(EvapCoolNum)%Schedule = cAlphaArgs(2)
      IF (lAlphaFieldBlanks(2)) THEN
        EvapCond(EvapCoolNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        EvapCond(EvapCoolNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (EvapCond(EvapCoolNum)%SchedPtr == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      EvapCond(EvapCoolNum)%InletNode  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%OutletNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4), &
                       'Evap Air Nodes')

      EvapCond(EvapCoolNum)%EvapControlType = TRIM(cAlphaArgs(5))

      !input the numerical data
      EvapCond(EvapCoolNum)%IndirectPadArea         = rNumericArgs(1)
      EvapCond(EvapCoolNum)%IndirectPadDepth        = rNumericArgs(2)
      EvapCond(EvapCoolNum)%IndirectRecircPumpPower = rNumericArgs(3)
      EvapCond(EvapCoolNum)%IndirectVolFlowRate     = rNumericArgs(4)
      EvapCond(EvapCoolNum)%IndirectFanEff          = rNumericArgs(5)
      EvapCond(EvapCoolNum)%IndirectFanDeltaPress   = rNumericArgs(6)
      EvapCond(EvapCoolNum)%IndirectHXEffectiveness = rNumericArgs(7)

      CALL SetupOutputVariable('Evaporative Cooler Wetbulb Effectiveness []',EvapCond(EvapCoolNum)%Sateff, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)
      CALL SetupOutputVariable('Evaporative Cooler Total Stage Effectiveness []',EvapCond(EvapCoolNum)%StageEff, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)

      ! A6 ; \Field Name of Water Supply Storage Tank
      EvapCond(EvapCoolNum)%EvapWaterSupplyName = cAlphaArgs(6)
      IF ( lAlphaFieldBlanks(6) ) THEN
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromTank
         CALL SetupTankDemandComponent(EvapCond(EvapCoolNum)%EvapCoolerName, TRIM(cCurrentModuleObject), &
                    EvapCond(EvapCoolNum)%EvapWaterSupplyName, ErrorsFound, EvapCond(EvapCoolNum)%EvapWaterSupTankID, &
                    EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID )

      ENDIF

      !A7 ; \field Secondary Outside Air Inlet node.
      IF ( lAlphaFieldBlanks(7) ) THEN
         EvapCond(EvapCoolNum)%SecondaryInletNode  =  0
      ELSE
         EvapCond(EvapCoolNum)%SecondaryInletNode  = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
         IF ( .not. CheckOutAirNodeNumber(EvapCond(EvapCoolNum)%SecondaryInletNode)) THEN
           CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
           CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            !TODO rename point
           CALL ShowContinueError('Node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
           ErrorsFound=.true.
         ENDIF

      ENDIF

    END DO   ! end Number of Dry Indirect EvapCooler Loop

    !**************************************************************
    !This is the start of the WetIndirect Evap Cooler Loop
    cCurrentModuleObject = 'EvaporativeCooler:Indirect:WetCoil'
    DO IndEvapCoolNum = 1,  NumWetInDirectEvapCool
      EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + IndEvapCoolNum

      CALL GetObjectItem(cCurrentModuleObject,IndEvapCoolNum,cAlphaArgs,NumAlphas, &
                      rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EvapCond%EvapCoolerName,EvapCoolNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      EvapCond(EvapCoolNum)%EvapCoolerName = Trim(cAlphaArgs(1))
      EvapCond(EvapCoolNum)%EvapCoolerType = iEvapCoolerInDirectWETCOIL !'EvaporativeCooler:Indirect:WetCoil'

      EvapCond(EvapCoolNum)%Schedule = cAlphaArgs(2)
      IF (lAlphaFieldBlanks(2)) THEN
        EvapCond(EvapCoolNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        EvapCond(EvapCoolNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (EvapCond(EvapCoolNum)%SchedPtr == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      EvapCond(EvapCoolNum)%InletNode  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%OutletNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4), &
                       'Evap Air Nodes')

      EvapCond(EvapCoolNum)%EvapControlType = TRIM(cAlphaArgs(5))

      !input the numerical data
      EvapCond(EvapCoolNum)%WetCoilMaxEfficiency    = rNumericArgs(1)
      EvapCond(EvapCoolNum)%WetCoilFlowRatio        = rNumericArgs(2)
      EvapCond(EvapCoolNum)%IndirectRecircPumpPower = rNumericArgs(3)
      EvapCond(EvapCoolNum)%IndirectVolFlowRate     = rNumericArgs(4)
      EvapCond(EvapCoolNum)%IndirectFanEff          = rNumericArgs(5)
      EvapCond(EvapCoolNum)%IndirectFanDeltaPress   = rNumericArgs(6)

      CALL SetupOutputVariable('Evaporative Cooler Total Stage Effectiveness []',EvapCond(EvapCoolNum)%StageEff, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)

     !  A6 ; \Field Name of Water Supply Storage Tank
      EvapCond(EvapCoolNum)%EvapWaterSupplyName = cAlphaArgs(6)
      IF (lAlphaFieldBlanks(6)) THEN
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromTank
         CALL SetupTankDemandComponent(EvapCond(EvapCoolNum)%EvapCoolerName, TRIM(cCurrentModuleObject), &
                    EvapCond(EvapCoolNum)%EvapWaterSupplyName, ErrorsFound, EvapCond(EvapCoolNum)%EvapWaterSupTankID, &
                    EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID )

      ENDIF

     ! A7 ; \field Secondary Outside Air Inlet node.
      IF (lAlphaFieldBlanks(7)) THEN
         EvapCond(EvapCoolNum)%SecondaryInletNode  =  0
      ELSE
         EvapCond(EvapCoolNum)%SecondaryInletNode  = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_OutsideAirReference,1,ObjectIsNotParent)
         IF ( .not. CheckOutAirNodeNumber(EvapCond(EvapCoolNum)%SecondaryInletNode)) THEN
           CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
           CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            !TODO rename point
           CALL ShowContinueError('Node does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
           ErrorsFound=.true.
         ENDIF

      ENDIF

    END DO   ! end Number of Wet Coil Indirect EvapCooler Loop
    !**************************************************************
    !This is the start of the Indirect Research Special Evap Cooler
    cCurrentModuleObject = 'EvaporativeCooler:Indirect:ResearchSpecial'
    DO IndEvapCoolNum = 1,  NumRDDEvapCool
      EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + &
                    NumWetInDirectEvapCool + IndEvapCoolNum
     CALL GetObjectItem(cCurrentModuleObject,IndEvapCoolNum,cAlphaArgs,NumAlphas, &
                      rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EvapCond%EvapCoolerName,EvapCoolNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      EvapCond(EvapCoolNum)%EvapCoolerName = Trim(cAlphaArgs(1))
      EvapCond(EvapCoolNum)%EvapCoolerType = iEvapCoolerInDirectRDDSpecial !'EvaporativeCooler:Indirect:ResearchSpecial'

      EvapCond(EvapCoolNum)%Schedule = cAlphaArgs(2)
      IF (lAlphaFieldBlanks(2)) THEN
        EvapCond(EvapCoolNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        EvapCond(EvapCoolNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (EvapCond(EvapCoolNum)%SchedPtr == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      EvapCond(EvapCoolNum)%InletNode  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%OutletNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4), &
                       'Evap Air Nodes')

      EvapCond(EvapCoolNum)%EvapControlType = TRIM(cAlphaArgs(5))

     ! A6 ; \field Secondary Air Inlet Node Name
      IF (lAlphaFieldBlanks(6)) THEN
         EvapCond(EvapCoolNum)%SecondaryInletNode  =  0
      ELSE
      EvapCond(EvapCoolNum)%SecondaryInletNode = &
        GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)
      END IF

      EvapCond(EvapCoolNum)%EvapControlNodeNum = &
      GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%TertiaryInletNode  = &
      GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,3,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%EvapWaterSupplyName = cAlphaArgs(9)
      IF (lAlphaFieldBlanks(9)) THEN
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromTank
         CALL SetupTankDemandComponent(EvapCond(EvapCoolNum)%EvapCoolerName, TRIM(cCurrentModuleObject), &
                    EvapCond(EvapCoolNum)%EvapWaterSupplyName, ErrorsFound, EvapCond(EvapCoolNum)%EvapWaterSupTankID, &
                    EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID )

      ENDIF

      !input the numerical data
      EvapCond(EvapCoolNum)%WetCoilMaxEfficiency    = rNumericArgs(1)
      EvapCond(EvapCoolNum)%WetCoilFlowRatio        = rNumericArgs(2)
      EvapCond(EvapCoolNum)%IndirectRecircPumpPower = rNumericArgs(3)
      EvapCond(EvapCoolNum)%IndirectVolFlowRate     = rNumericArgs(4)
      EvapCond(EvapCoolNum)%IndirectFanEff          = rNumericArgs(5)
      EvapCond(EvapCoolNum)%IndirectFanDeltaPress   = rNumericArgs(6)
      EvapCond(EvapCoolNum)%DPBoundFactor           = rNumericArgs(7)
      IF (lNumericFieldBlanks(8)) THEN
        EvapCond(EvapCoolNum)%DriftFraction   = 0.0D0
      ELSE
        EvapCond(EvapCoolNum)%DriftFraction   = rNumericArgs(8)
      ENDIF

      IF (lNumericFieldBlanks(9)) THEN
        EvapCond(EvapCoolNum)%BlowDownRatio   = 0.0D0
      ELSE
        EvapCond(EvapCoolNum)%BlowDownRatio   = rNumericArgs(9)
      ENDIF


      CALL SetupOutputVariable('Evaporative Cooler Total Stage Effectiveness []',EvapCond(EvapCoolNum)%StageEff, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)
      CALL SetupOutputVariable('Evaporative Cooler Part Load Ratio []',EvapCond(EvapCoolNum)%PartLoadFract, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)

      CALL SetupOutputVariable('Evaporative Cooler Dewpoint Bound Status []', EvapCond(EvapCoolNum)%DewPointBoundFlag, &
                               'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)

    END Do  ! end of Indirect Research Special cooler input loop

    cCurrentModuleObject = 'EvaporativeCooler:Direct:ResearchSpecial'
    Do DirectEvapCoolNum =  1, NumDirectResearchSpecialEvapCool
      EvapCoolNum = NumDirectEvapCool + NumDryInDirectEvapCool + &
                    NumWetInDirectEvapCool + NumRDDEvapCool + DirectEvapCoolNum
      CALL GetObjectItem(cCurrentModuleObject,DirectEvapCoolNum,cAlphaArgs,NumAlphas, &
                      rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks,&
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EvapCond%EvapCoolerName,EvapCoolNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      EvapCond(EvapCoolNum)%EvapCoolerName = Trim(cAlphaArgs(1))
      EvapCond(EvapCoolNum)%EvapCoolerType = iEvapCoolerDirectResearchSpecial

      EvapCond(EvapCoolNum)%Schedule       = cAlphaArgs(2)
      IF (lAlphaFieldBlanks(2)) THEN
        EvapCond(EvapCoolNum)%SchedPtr = ScheduleAlwaysOn
      ELSE
        EvapCond(EvapCoolNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
        IF (EvapCond(EvapCoolNum)%SchedPtr == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF

      EvapCond(EvapCoolNum)%InletNode  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%OutletNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4), &
                       'Evap Air Nodes')

      EvapCond(EvapCoolNum)%EvapControlNodeNum = &
         GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

      EvapCond(EvapCoolNum)%EvapWaterSupplyName = cAlphaArgs(6)

      IF (lAlphaFieldBlanks(6)) THEN
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromMains
      ELSE
         EvapCond(EvapCoolNum)%EvapWaterSupplyMode = WaterSupplyFromTank
         CALL SetupTankDemandComponent(EvapCond(EvapCoolNum)%EvapCoolerName, TRIM(cCurrentModuleObject), &
                    EvapCond(EvapCoolNum)%EvapWaterSupplyName, ErrorsFound, EvapCond(EvapCoolNum)%EvapWaterSupTankID, &
                    EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID )
      ENDIF
      EvapCond(EvapCoolNum)%DirectEffectiveness = rNumericArgs(1)
      EvapCond(EvapCoolNum)%RecircPumpPower     = rNumericArgs(2)

      IF (lNumericFieldBlanks(3)) THEN
        EvapCond(EvapCoolNum)%DriftFraction   = 0.0D0
      ELSE
        EvapCond(EvapCoolNum)%DriftFraction   = rNumericArgs(3)
      ENDIF

      IF (lNumericFieldBlanks(4)) THEN
        EvapCond(EvapCoolNum)%BlowDownRatio   = 0.0D0
      ELSE
        EvapCond(EvapCoolNum)%BlowDownRatio   = rNumericArgs(4)
      ENDIF

    ENDDO


    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in processing input for evaporative coolers')
    ENDIF



    DO EvapCoolNum=1,NumEvapCool
      ! Setup Report variables for the Evap Coolers
      CALL SetupOutputVariable('Evaporative Cooler Electric Energy [J]',EvapCond(EvapCoolNum)%EvapCoolerEnergy, &
                            'System','Sum',EvapCond(EvapCoolNum)%EvapCoolerName, &
                             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')
      CALL SetupOutputVariable('Evaporative Cooler Electric Power [W]',EvapCond(EvapCoolNum)%EvapCoolerPower, &
                            'System','Average',EvapCond(EvapCoolNum)%EvapCoolerName)
      ! this next report variable is setup differently depending on how the water should be metered here.
      IF (EvapCond(EvapCoolNum)%EvapWaterSupplyMode == WaterSupplyFromMains) Then
        CALL SetupOutputVariable('Evaporative Cooler Water Volume [m3]',EvapCond(EvapCoolNum)%EvapWaterConsump, &
                            'System','Sum',EvapCond(EvapCoolNum)%EvapCoolerName, &
                             ResourceTypeKey='Water',EndUseKey='Cooling',GroupKey='System')
        CALL SetupOutputVariable('Evaporative Cooler Mains Water Volume [m3]',EvapCond(EvapCoolNum)%EvapWaterConsump, &
                            'System','Sum',EvapCond(EvapCoolNum)%EvapCoolerName, &
                             ResourceTypeKey='MainsWater',EndUseKey='Cooling',GroupKey='System')

      ELSEIF  (EvapCond(EvapCoolNum)%EvapWaterSupplyMode == WaterSupplyFromTank) THEN
        CALL SetupOutputVariable('Evaporative Cooler Storage Tank Water Volume [m3]',EvapCond(EvapCoolNum)%EvapWaterConsump, &
                            'System','Sum',EvapCond(EvapCoolNum)%EvapCoolerName, &
                             ResourceTypeKey='Water',EndUseKey='Cooling' , GroupKey='System')
        CALL SetupOutputVariable('Evaporative Cooler Starved Water Volume [m3]',EvapCond(EvapCoolNum)%EvapWaterStarvMakup, &
                            'System','Sum',EvapCond(EvapCoolNum)%EvapCoolerName, &
                             ResourceTypeKey='Water',EndUseKey='Cooling', GroupKey='System')
        CALL SetupOutputVariable('Evaporative Cooler Starved Mains Water Volume [m3]',EvapCond(EvapCoolNum)%EvapWaterStarvMakup, &
                            'System','Sum',EvapCond(EvapCoolNum)%EvapCoolerName, &
                             ResourceTypeKey='MainsWater',EndUseKey='Cooling', GroupKey='System')
      ENDIF

    END DO



  RETURN

END SUBROUTINE GetEvapInput

! End of Get Input subroutines for the HB Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitEvapCooler(EvapCoolNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       B. Griffith, May 2009, added EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations of the EvapCooler Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: DoSetPointTest, SetPointErrorFlag
  USE DataEnvironment, ONLY: OutAirDensity, OutDryBulbTemp, OutEnthalpy, OutWetBulbTemp
  USE DataGlobals,     ONLY: AnyEnergyManagementSystemInModel
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: InletNode
  Integer             :: SecInletNode   ! local index for secondary inlet node.
  REAL(r64)           :: RhoAir !Air Density
  Integer             :: ControlNode
  Integer             :: OutNode
  Integer             :: EvapUnitNum
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL             :: localSetpointCheck = .FALSE.

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MySizeFlag(NumEvapCool))
    MySizeFlag = .TRUE.
    MyOneTimeFlag = .false.
  ENDIF

  ! FLOW:
  !Check that setpoint is active
  IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
    DO EvapUnitNum = 1, NumEvapCool
      ControlNode = EvapCond(EvapUnitNum)%EvapControlNodeNum
      IF (ControlNode > 0) THEN
        IF (Node(ControlNode)%TempSetPoint == SensedNodeFlagValue) THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            CALL ShowSevereError('Missing temperature setpoint for Evap Cooler unit ' // &
                                  TRIM(EvapCond(EvapCoolNum)%EvapCoolerName))
            CALL ShowContinueError(' use a Setpoint Manager to establish a setpoint at the unit control node.')
          ELSE
            localSetpointCheck = .FALSE.
            CALL CheckIfNodeSetpointManagedByEMS(ControlNode, iTemperatureSetpoint, localSetpointCheck)
            IF (localSetpointCheck) THEN
              CALL ShowSevereError('Missing temperature setpoint for Evap Cooler unit ' // &
                                    TRIM(EvapCond(EvapCoolNum)%EvapCoolerName))
              CALL ShowContinueError(' use a Setpoint Manager to establish a setpoint at the unit control node.')
              CALL ShowContinueError(' or use an EMS actuator to establish a setpoint at the unit control node.')
            ENDIF
          ENDIF
        END IF
      END IF
    END DO
    MySetPointCheckFlag = .FALSE.
  END IF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(EvapCoolNum)) THEN
    ! for each cooler, do the sizing once.
    CALL SizeEvapCooler(EvapCoolNum)

    MySizeFlag(EvapCoolNum) = .FALSE.
  END IF

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.

  !Transfer the node data to EvapCond data structure
  InletNode = EvapCond(EvapCoolNum)%InletNode

  RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,Node(InletNode)%Temp,Node(InletNode)%HumRat)

   ! set the volume flow rates from the input mass flow rates
  EvapCond(EvapCoolNum)%VolFlowRate = Node(InletNode)%MassFlowRate/RhoAir

   ! Calculate the entering wet bulb temperature for inlet conditions
  EvapCond(EvapCoolNum)%InletWetBulbTemp=PsyTwbFnTdbWPb(Node(InletNode)%Temp,Node(InletNode)%HumRat,OutBaroPress)

    !Set all of the inlet mass flow variables from the nodes
  EvapCond(EvapCoolNum)%InletMassFlowRate         = Node(InletNode)%MassFlowRate
  EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail = Node(InletNode)%MassFlowRateMaxAvail
  EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail = Node(InletNode)%MassFlowRateMinAvail
    !Set all of the inlet state variables from the inlet nodes
  EvapCond(EvapCoolNum)%InletTemp        = Node(InletNode)%Temp
  EvapCond(EvapCoolNum)%InletHumRat      = Node(InletNode)%HumRat
  EvapCond(EvapCoolNum)%InletEnthalpy    = Node(InletNode)%Enthalpy
  EvapCond(EvapCoolNum)%InletPressure    = Node(InletNode)%Press
    !Set default outlet state to inlet states(?)
  EvapCond(EvapCoolNum)%OutletTemp       = EvapCond(EvapCoolNum)%InletTemp
  EvapCond(EvapCoolNum)%OutletHumRat     = EvapCond(EvapCoolNum)%InletHumRat
  EvapCond(EvapCoolNum)%OutletEnthalpy   = EvapCond(EvapCoolNum)%InletEnthalpy
  EvapCond(EvapCoolNum)%OutletPressure   = EvapCond(EvapCoolNum)%InletPressure

  EvapCond(EvapCoolNum)%OutletMassFlowRate         = EvapCond(EvapCoolNum)%InletMassFlowRate
  EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail
  EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail

    !Set all of the secondary inlet mass flow variables from the nodes
  SecInletNode = EvapCond(EvapCoolNum)%SecondaryInletNode
  IF (SecInletNode /= 0) THEN
    EvapCond(EvapCoolNum)%SecInletMassFlowRate         = Node(SecInletNode)%MassFlowRate
    EvapCond(EvapCoolNum)%SecInletMassFlowRateMaxAvail = Node(SecInletNode)%MassFlowRateMaxAvail
    EvapCond(EvapCoolNum)%SecInletMassFlowRateMinAvail = Node(SecInletNode)%MassFlowRateMinAvail
    EvapCond(EvapCoolNum)%SecInletTemp                 = Node(SecInletNode)%Temp
    EvapCond(EvapCoolNum)%SecInletHumRat               = Node(SecInletNode)%HumRat
    EvapCond(EvapCoolNum)%SecInletEnthalpy             = Node(SecInletNode)%Enthalpy
    EvapCond(EvapCoolNum)%SecInletPressure             = Node(SecInletNode)%Press
  ELSE
    EvapCond(EvapCoolNum)%SecInletMassFlowRate         = EvapCond(EvapCoolNum)%IndirectVolFlowRate * OutAirDensity
    EvapCond(EvapCoolNum)%SecInletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%IndirectVolFlowRate * OutAirDensity
    EvapCond(EvapCoolNum)%SecInletMassFlowRateMinAvail = 0.0
    EvapCond(EvapCoolNum)%SecInletTemp                 = OutDryBulbTemp
    EvapCond(EvapCoolNum)%SecInletHumRat               = PsyWFnTdbTwbPb(OutDryBulbTemp,OutWetBulbTemp,OutBaroPress)
    EvapCond(EvapCoolNum)%SecInletEnthalpy             = OutEnthalpy
    EvapCond(EvapCoolNum)%SecInletPressure             = OutBaroPress
  ENDIF
!Set the energy consumption to zero each time through for reporting
  EvapCond(EvapCoolNum)%EvapCoolerEnergy = 0.0
  EvapCond(EvapCoolNum)%EvapCoolerPower  = 0.0
  EvapCond(EvapCoolNum)%DewPointBoundFlag = 0
!Set the water consumption to zero each time through for reporting
  EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0
  EvapCond(EvapCoolNum)%EvapWaterConsump     = 0.0
  EvapCond(EvapCoolNum)%EvapWaterStarvMakup  = 0.0

!Set the Saturation and Stage Efficiency to zero each time through for reporting
  EvapCond(EvapCoolNum)%StageEff  = 0.0
  EvapCond(EvapCoolNum)%SatEff    = 0.0

! These initializations are done every iteration
  OutNode = EvapCond(EvapCoolNum)%OutletNode
  ControlNode = EvapCond(EvapCoolNum)%EvapControlNodeNum

  IF (ControlNode.EQ.0) THEN
    EvapCond(EvapCoolNum)%DesiredOutletTemp = 0.0
  ELSE IF (ControlNode.EQ.OutNode) THEN
    EvapCond(EvapCoolNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint
  ELSE
    EvapCond(EvapCoolNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint - &
      (Node(ControlNode)%Temp - Node(OutNode)%Temp)
  END IF

  RETURN

END SUBROUTINE InitEvapCooler

SUBROUTINE SizeEvapCooler(EvapCoolNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Size calculations for Evap coolers
          !  currently just for secondary side of Research Special Indirect evap cooler

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataAirSystems, ONLY: PrimaryAirSystem
  USE InputProcessor, ONLY: SameString
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: CoolerOnOApath      = .FALSE.
  LOGICAL :: CoolerOnMainAirLoop = .FALSE.
!unused0509  INTEGER :: OAsysIndex = 0
  INTEGER :: AirSysBranchLoop = 0
!unused0509  INTEGER :: OAsysLoop = 0
!unuse0509  INTEGER :: OAcompLoop = 0
  INTEGER :: BranchComp = 0

  !inits
  CoolerOnOApath      = .FALSE.
  CoolerOnMainAirLoop = .FALSE.

  IF (EvapCond(EvapCoolNum)%IndirectVolFlowRate == Autosize) Then
    IF (CurSysNum > 0) THEN !central system
      !where is this cooler located, is it on OA system or main loop?
      ! search for this component in Air loop branches.
      DO AirSysBranchLoop =1,  PrimaryAirSystem(CurSysNum)%NumBranches
        DO BranchComp  =1, PrimaryAirSystem(CurSysNum)%Branch(AirSysBranchLoop)%TotalComponents

          IF (SameString(PrimaryAirSystem(CurSysNum)%Branch(AirSysBranchLoop)%comp(BranchComp)%Name, &
                          EvapCond(EvapCoolNum)%EvapCoolerName )) THEN
            CoolerOnMainAirLoop = .TRUE.
          ENDIF

        ENDDO
      ENDDO

      ! would like search for this componenent in some OutsideAirSys structure
       ! but thats not so easy becuase of circular USE with MixedAir.f90
       !  So assume if its not on main air path, its on OA path (for now)
      IF (.NOT. CoolerOnMainAirLoop) CoolerOnOApath = .TRUE.

      IF (CoolerOnMainAirLoop) THEN
        EvapCond(EvapCoolNum)%IndirectVolFlowRate =FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSEIF (CoolerOnOApath) THEN
        EvapCond(EvapCoolNum)%IndirectVolFlowRate =MAX(FinalSysSizing(CurSysNum)%DesOutAirVolFlow, &
                                                      0.5D0*FinalSysSizing(CurSysNum)%DesMainVolFlow)
      ENDIF

    ELSE !zone equipment
      ! we have no zone equipment evap coolers yet

    ENDIF

    CALL ReportSizingOutput('EvaporativeCooler:Indirect:ResearchSpecial', EvapCond(EvapCoolNum)%EvapCoolerName, &
                              'Secondary Fan Flow Rate [m3/s]', EvapCond(EvapCoolNum)%IndirectVolFlowRate)
  ENDIF

  RETURN

END SUBROUTINE SizeEvapCooler


 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE CalcDirectEvapCooler(EvapCoolNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY : RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      Integer :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      !REAL(r64) Variables
      REAL(r64)  :: PadDepth    ! EvapCooler Pad Depth in Meters as input by the User
      REAL(r64)  :: SatEff      ! Saturation Efficiency of the CelDek Pad
      REAL(r64)  :: AirVel      ! The Calculated Air Velocity through the Pad
      REAL(r64)  :: TEDB        ! Entering Dry Bulb Temperature
      REAL(r64)  :: TEWB        ! Entering Wet Bulb Temperature
      REAL(r64)  :: RhoWater


  ! If the Evaporative Cooler  is operating there should be some mass flow rate
  !  Also the evap cooler has to be scheduled to be available
  IF((EvapCond(EvapCoolNum)%InletMassFlowRate .GT. 0.0) .and. &
     (GetCurrentScheduleValue(EvapCond(EvapCoolNum)%SchedPtr) .gt. 0.0)) THEN


   PadDepth = EvapCond(EvapCoolNum)%PadDepth
!******************************************************************************
!   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
!   FOR A DIRECT EVAPORATIVE AIR COOLER SUPPLIED WITH CFMAIR,DIRPAD,TEWB,TEDB,
!   AND PB (ATM. PRESS.) FOR AIR DENSITY CALCULATIONS.
!******************************************************************************


  AirVel=EvapCond(EvapCoolNum)%VolFlowRate/EvapCond(EvapCoolNum)%PadArea

!******************************************************************************
!   SAT EFF IS FOR DIFFERENT THICKNESS CELDEK PAD (CURVE FIT FROM DATA)
!******************************************************************************
  SatEff=0.792714+0.958569*PadDepth - 0.25193*AirVel                             &
         - 1.03215*PadDepth**2 + 2.62659E-2*AirVel**2 + 0.914869*PadDepth*AirVel &
         - 1.48241*AirVel*PadDepth**2 - 1.89919E-2*AirVel**3*PadDepth            &
         + 1.13137*PadDepth**3*AirVel + 3.27622E-2*AirVel**3*PadDepth**2         &
         - 0.145384*PadDepth**3*AirVel**2

  IF(SatEff.GE.1.0) SatEff=1.0
  IF(SatEff < 0.0) THEN ! we have a serious problem.  Pad Area and/or depth not suitable for system air flow rates
    Call ShowSevereError('EVAPCOOLER:DIRECT:CELDEKPAD: '//trim(EvapCond(EvapCoolNum)%EvapCoolerName)//' has a problem')
    Call ShowContinueError('Check size of Pad Area and/or Pad Depth in input')
    Call ShowContinueError('Cooler Effectiveness calculated as: '//trim(RoundSigDigits(SatEff,2)) )
    Call ShowContinueError('Air velocity (m/s) through pads calculated as: '//trim(RoundSigDigits(AirVel,2)) )
    CALL showFatalError('Program Terminates due to previous error condition')

  ENDIF
  EvapCond(EvapCoolNum)%SatEff = SatEff
!***************************************************************************
!   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
!   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
!   ACROSS A DIRECT EVAPORATION COOLER.
      TEWB = EvapCond(EvapCoolNum)%InletWetBulbTemp
      TEDB = EvapCond(EvapCoolNum)%InletTemp

      EvapCond(EvapCoolNum)%OutletTemp = TEDB-((TEDB-TEWB)*SatEff)

      EvapCond(EvapCoolNum)%OuletWetBulbTemp = EvapCond(EvapCoolNum)%InletWetBulbTemp


      EvapCond(EvapCoolNum)%OutletHumRat = PsyWFnTdbTwbPb(EvapCond(EvapCoolNum)%OutletTemp,TEWB,OutBaroPress)

      EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                             EvapCond(EvapCoolNum)%OutletHumRat)

!***************************************************************************
!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!Add the pump energy to the total Evap Cooler energy comsumption
      EvapCond(EvapCoolNum)%EvapCoolerPower = EvapCond(EvapCoolNum)%EvapCoolerPower +  &
                                              EvapCond(EvapCoolNum)%RecircPumpPower


!******************
!             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
!             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
!                                /RhoWater [kg H2O/m3 H2O]
!******************
      RhoWater = RhoH2O(EvapCond(EvapCoolNum)%OutletTemp)
      EvapCond(EvapCoolNum)%EvapWaterConsumpRate =  &
                 (EvapCond(EvapCoolNum)%OutletHumRat - EvapCond(EvapCoolNum)%InletHumRat) *  &
                  EvapCond(EvapCoolNum)%InletMassFlowRate/Rhowater
      ! A numerical check to keep from having very tiny negative water consumption values being reported
      If(EvapCond(EvapCoolNum)%EvapWaterConsumpRate < 0.0) EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0


 Else
     ! The evap cooler is not running and does not change conditions from inlet to outlet
      EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%InletTemp

      EvapCond(EvapCoolNum)%OuletWetBulbTemp = EvapCond(EvapCoolNum)%InletWetBulbTemp

      EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

      EvapCond(EvapCoolNum)%OutletEnthalpy = EvapCond(EvapCoolNum)%InletEnthalpy

      EvapCond(EvapCoolNum)%EvapCoolerEnergy = 0.0

      EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0

 End IF
 ! all of the mass flowrates are not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletMassFlowRate         = EvapCond(EvapCoolNum)%InletMassFlowRate
 EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail
 EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail

 ! the pressure is not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletPressure = EvapCond(EvapCoolNum)%InletPressure

 RETURN

END SUBROUTINE CalcDirectEvapCooler


SUBROUTINE CalcDryIndirectEvapCooler(EvapCoolNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       BG Feb. 2007 secondary air inlet node
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      Integer :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      !REAL(r64) Variables
      REAL(r64)  :: PadDepth    ! EvapCooler Pad Depth in Meters as input by the User
      REAL(r64)  :: SatEff      ! Saturation Efficiency of the CelDek Pad
      REAL(r64)  :: AirVel      ! The Calculated Air Velocity through the Pad
      REAL(r64)  :: TDBSec      ! Secondary leaving dry bulb
      REAL(r64)  :: TWBSec      ! Secondary Leaving Wet Bulb
      REAL(r64)  :: HumRatSec   ! Secondary leaving Humidity Ratio
      REAL(r64)  :: EffHX       ! Effectiveness of Secondary Heat Exchanger
      REAL(r64)  :: QHX         ! Q Across Sec HX
      REAL(r64)  :: RhoWater
      REAL(r64)  :: RhoAir      ! Density of the primary side air
      REAL(r64)  :: CpAir       ! Cp of the primary side air
      REAL(r64)  :: CFMAir
      REAL(r64)  :: CFMSec


  ! If the Evaporative Cooler  is operating there should be some mass flow rate
  !  Also the evap cooler has to be scheduled to be available
  IF((EvapCond(EvapCoolNum)%InletMassFlowRate .GT. 0.0) .and. &
     (GetCurrentScheduleValue(EvapCond(EvapCoolNum)%SchedPtr) .gt. 0.0)) THEN


   PadDepth = EvapCond(EvapCoolNum)%IndirectPadDepth
!******************************************************************************
!   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
!   FOR A DIRECT EVAPORATIVE AIR COOLER SUPPLIED WITH CFMAIR,DIRPAD,TEWB,TEDB,
!   AND PB (ATM. PRESS.) FOR AIR DENSITY CALCULATIONS.
!******************************************************************************


  AirVel=EvapCond(EvapCoolNum)%IndirectVolFlowRate/EvapCond(EvapCoolNum)%IndirectPadArea

!******************************************************************************
!   SAT EFF IS FOR DIFFERENT THICKNESS CELDEK PAD (CURVE FIT FROM DATA)
!******************************************************************************
  SatEff=0.792714+0.958569*PadDepth - 0.25193*AirVel                             &
         - 1.03215*PadDepth**2 + 2.62659E-2*AirVel**2 + 0.914869*PadDepth*AirVel &
         - 1.48241*AirVel*PadDepth**2 - 1.89919E-2*AirVel**3*PadDepth            &
         + 1.13137*PadDepth**3*AirVel + 3.27622E-2*AirVel**3*PadDepth**2         &
         - 0.145384*PadDepth**3*AirVel**2

  IF(SatEff.GE.1.0) SatEff=1.0
  EvapCond(EvapCoolNum)%SatEff = SatEff
!***************************************************************************
!   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
!   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE PAD BEFORE THE HX.
!***************************************************************************
!***** FIRST CHECK IF THIS TEWB IS A FEASIBLE POINT ON PSYCH CHART**********

! BG Feb 2007 mods for oa node (eg. height-dependent outside air model)
      TWBSec =  PsyTwbFnTdbWPb(EvapCond(EvapCoolNum)%SecInletTemp  &
                              ,EvapCond(EvapCoolNum)%SecInletHumRat &
                              ,EvapCond(EvapCoolNum)%SecInletPressure )  !  OutWetBulbTemp
      TDBSec = EvapCond(EvapCoolNum)%SecInletTemp-((EvapCond(EvapCoolNum)%SecInletTemp-TWBSec)*SatEff)

      HumratSec = PsyWFnTdbTwbPb(TDBSec,TWBSec,EvapCond(EvapCoolNum)%SecInletPressure)

!***************************************************************************
!                  CALCULATE THE TLDB FROM HX EQUATIONS GIVEN AN EFFICIENCY
!***************************************************************************
      EffHX  = EvapCond(EvapCoolNum)%IndirectHXEffectiveness
      CpAir  = PsyCpAirFnWTdb(EvapCond(EvapCoolNum)%InletHumRat,EvapCond(EvapCoolNum)%InletTemp)
      RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,EvapCond(EvapCoolNum)%InletTemp,EvapCond(EvapCoolNum)%InletHumRat)
      CFMAir = EvapCond(EvapCoolNum)%VolFlowRate            !Volume Flow Rate Primary Side
      CFMSec = EvapCond(EvapCoolNum)%IndirectVolFlowRate    !Volume Flolw Rate Secondary Side

      QHX=EFFHX*MIN(CFMSEC,CFMAIR)*RhoAir*CpAir*(EvapCond(EvapCoolNum)%InletTemp-TDBSec)
      EvapCond(EvapCoolNum)%OutletTemp=EvapCond(EvapCoolNum)%InletTemp-QHX/(RhoAir*CFMAIR*CpAir)
! This is a rough approximation of the Total Indirect Stage Efficiency for the Dry stage which
!   is a 2 step process the first being teh pad efficiency and then the HX Effectiveness.  I think that
!   this would mainly be used for evap sizing purposes.
      EvapCond(EvapCoolNum)%StageEff = SatEff * EFFHX
!***************************************************************************
!                  CALCULATE THE WET BULB TEMP in the primary system air USING PSYCH ROUTINES
! There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
      EvapCond(EvapCoolNum)%OuletWetBulbTemp=PsyTwbFnTdbWPb(EvapCond(EvapCoolNum)%OutletTemp, &
                                             EvapCond(EvapCoolNum)%InletHumRat,OutBaroPress)
!***************************************************************************
!   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
!   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
!   ACROSS A DIRECT EVAPORATION COOLER.

      EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

      EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                             EvapCond(EvapCoolNum)%OutletHumRat)

!***************************************************************************
!                  POWER OF THE SECONDARY AIR FAN
      IF (EvapCond(EvapCoolNum)%IndirectFanEff > 0.0D0) THEN
        EvapCond(EvapCoolNum)%EvapCoolerPower=EvapCond(EvapCoolNum)%EvapCoolerPower +      &
                                              EvapCond(EvapCoolNum)%IndirectFanDeltaPress* &
                                              EvapCond(EvapCoolNum)%IndirectVolFlowRate/   &
                                              EvapCond(EvapCoolNum)%IndirectFanEff
      ENDIF

!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!Add the pump energy to the total Evap Cooler energy comsumption
      EvapCond(EvapCoolNum)%EvapCoolerPower = EvapCond(EvapCoolNum)%EvapCoolerPower +  &
                                              EvapCond(EvapCoolNum)%IndirectRecircPumpPower


!******************
!             WATER CONSUMPTION IN LITERS OF WATER FOR DIRECT
!             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
!                                /RhoWater [kg H2O/m3 H2O]
!******************
      RhoWater = RhoH2O(TDBSec)
      RhoAir = ( PsyRhoAirFnPbTdbW(EvapCond(EvapCoolNum)%SecInletPressure  &
                                  , EvapCond(EvapCoolNum)%SecInletTemp     &
                                  , EvapCond(EvapCoolNum)%SecInletHumRat ) &
                + PsyRhoAirFnPbTdbW(EvapCond(EvapCoolNum)%SecInletPressure  &
                                  , TDBSec     &
                                  , HumratSec ) ) / 2.0
      EvapCond(EvapCoolNum)%EvapWaterConsumpRate =  &
                 (HumratSec - EvapCond(EvapCoolNum)%SecInletHumRat) *  &
                 EvapCond(EvapCoolNum)%IndirectVolFlowRate*RhoAir/RhoWater
      ! A numerical check to keep from having very tiny negative water consumption values being reported
      If(EvapCond(EvapCoolNum)%EvapWaterConsumpRate < 0.0) EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0

 Else
     ! The evap cooler is not running and does not change conditions from inlet to outlet
      EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%InletTemp

      EvapCond(EvapCoolNum)%OuletWetBulbTemp = EvapCond(EvapCoolNum)%InletWetBulbTemp

      EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

      EvapCond(EvapCoolNum)%OutletEnthalpy = EvapCond(EvapCoolNum)%InletEnthalpy

      EvapCond(EvapCoolNum)%EvapCoolerEnergy = 0.0

      EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0

 End IF
 ! all of the mass flowrates are not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletMassFlowRate         = EvapCond(EvapCoolNum)%InletMassFlowRate
 EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail
 EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail

 ! the pressure is not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletPressure = EvapCond(EvapCoolNum)%InletPressure

 RETURN

END SUBROUTINE CalcDryIndirectEvapCooler


SUBROUTINE CalcWetIndirectEvapCooler(EvapCoolNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
 !     Use DataEnvironment, ONLY: OutDryBulbTemp, OutWetBulbTemp, OutHumRat, OutBaroPress


      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      Integer :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      !REAL(r64) Variables
      REAL(r64)  :: StageEff    ! Stage Efficiency of the Heat Exchanger
      REAL(r64)  :: TEDB        ! Entering Dry Bulb Temperature
      REAL(r64)  :: TEWB        ! Entering Wet Bulb Temperature
      REAL(r64)  :: QHX         ! Q Across Sec HX in Watts or J/sec
      REAL(r64)  :: RhoWater
      REAL(r64)  :: RhoAir      ! Density of the primary side air
      REAL(r64)  :: CFMAir
      REAL(r64)  :: CFMSec
      REAL(r64)  :: TWBSec  ! wet bulb of secondary air


  ! If the Evaporative Cooler  is operating there should be some mass flow rate
  !  Also the evap cooler has to be scheduled to be available
  IF((EvapCond(EvapCoolNum)%InletMassFlowRate .GT. 0.0) .and. &
     (GetCurrentScheduleValue(EvapCond(EvapCoolNum)%SchedPtr) .gt. 0.0)) THEN


!******************************************************************************
!   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
!   FOR A WET COIL EVAPORATIVE COOLER
!******************************************************************************
!  INDIRECT STAGE EFFICIENCY FOR WET COIL INDIRECT EVAP COOLERS
      CFMAir = EvapCond(EvapCoolNum)%VolFlowRate            !Volume Flow Rate Primary Side
      CFMSec = EvapCond(EvapCoolNum)%IndirectVolFlowRate    !Volume Flolw Rate Secondary Side

      StageEff = EvapCond(EvapCoolNum)%WetCoilMaxEfficiency - &
              MIN(EvapCond(EvapCoolNum)%WetCoilFlowRatio*CFMAIR/CFMSEC, &
                  EvapCond(EvapCoolNum)%WetCoilMaxEfficiency)

      IF(StageEff.GE.1.0) StageEff=1.0
! This is a rough approximation of the Total Indirect Stage Efficiency.  I think that
!   this would mainly be used for evap sizing purposes.
      EvapCond(EvapCoolNum)%StageEff = StageEff
!***************************************************************************
!   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
!   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
!   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
!***************************************************************************
!                  CALCULATE THE TLDB
      TEWB = EvapCond(EvapCoolNum)%InletWetBulbTemp
      TEDB = EvapCond(EvapCoolNum)%InletTemp
      TWBSec =  PsyTwbFnTdbWPb(EvapCond(EvapCoolNum)%SecInletTemp  &
                              ,EvapCond(EvapCoolNum)%SecInletHumRat &
                              ,EvapCond(EvapCoolNum)%SecInletPressure )
      EvapCond(EvapCoolNum)%OutletTemp = TEDB - StageEff*(TEDB-TWBSec)

!***************************************************************************
!                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
! There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
      EvapCond(EvapCoolNum)%OuletWetBulbTemp = PsyTwbFnTdbWPb(EvapCond(EvapCoolNum)%OutletTemp, &
                                               EvapCond(EvapCoolNum)%InletHumRat,OutBaroPress)
!***************************************************************************
!                  CALCULATE other outlet properties using PSYCH ROUTINES
      EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

      EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                             EvapCond(EvapCoolNum)%OutletHumRat)

!***************************************************************************
!                  POWER OF THE SECONDARY AIR FAN
      IF (EvapCond(EvapCoolNum)%IndirectFanEff > 0.0D0) THEN
        EvapCond(EvapCoolNum)%EvapCoolerPower=EvapCond(EvapCoolNum)%EvapCoolerPower +      &
                                              EvapCond(EvapCoolNum)%IndirectFanDeltaPress* &
                                              EvapCond(EvapCoolNum)%IndirectVolFlowRate/   &
                                              EvapCond(EvapCoolNum)%IndirectFanEff
      ENDIF

!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!Add the pump energy to the total Evap Cooler energy comsumption
      EvapCond(EvapCoolNum)%EvapCoolerPower = EvapCond(EvapCoolNum)%EvapCoolerPower +  &
                                              EvapCond(EvapCoolNum)%IndirectRecircPumpPower


!******************
!             WATER CONSUMPTION IN LITERS OF WATER FOR Wet InDIRECT
!             H2O [m3/sec] = (QHx [J/s])/(2,500,000 [J/kg H2O] * RhoWater [kg H2O/m3 H2O])
!******************
!***** FIRST calculate the heat exchange on the primary air side**********
      RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,EvapCond(EvapCoolNum)%InletTemp,EvapCond(EvapCoolNum)%InletHumRat)
      QHX = CFMAir*RhoAir*(EvapCond(EvapCoolNum)%InletEnthalpy - EvapCond(EvapCoolNum)%OutletEnthalpy)

      RhoWater = RhoH2O(EvapCond(EvapCoolNum)%SecInletTemp)
      EvapCond(EvapCoolNum)%EvapWaterConsumpRate =  (QHx/StageEff)/(2500000.0 * RhoWater)
      ! A numerical check to keep from having very tiny negative water consumption values being reported
      If(EvapCond(EvapCoolNum)%EvapWaterConsumpRate < 0.0) EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0

 Else
     ! The evap cooler is not running and does not change conditions from inlet to outlet
      EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%InletTemp

      EvapCond(EvapCoolNum)%OuletWetBulbTemp = EvapCond(EvapCoolNum)%InletWetBulbTemp

      EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

      EvapCond(EvapCoolNum)%OutletEnthalpy = EvapCond(EvapCoolNum)%InletEnthalpy

      EvapCond(EvapCoolNum)%EvapCoolerEnergy = 0.0

      EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0

 End IF
 ! all of the mass flowrates are not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletMassFlowRate         = EvapCond(EvapCoolNum)%InletMassFlowRate
 EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail
 EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail

 ! the pressure is not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletPressure = EvapCond(EvapCoolNum)%InletPressure

 RETURN

END SUBROUTINE CalcWetIndirectEvapCooler

Subroutine CalcResearchSpecialPartLoad(EvapCoolNum)
      ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Subroutine models a "special" cooler.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! copied CalcWetIndirectEvapCooler as template for new cooler

                   ! USE STATEMENTS:
      USE DataHVACGlobals, only:  TempControlTol

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      Integer :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
  REAL(r64), PARAMETER :: MinAirMassFlow = 0.001d0
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  CHARACTER(len=MaxNameLength)  :: CompName
  REAL(r64)           :: FullOutput
  REAL(r64)           :: ReqOutput
  Integer             :: InletNode
  Integer             :: OutletNode
  Integer             :: ControlNode
  REAL(r64)           :: PartLoadFrac
  REAL(r64)           :: DesOutTemp
      ! Set local variables
      ! Retrieve the load on the controlled zone
  OutletNode = EvapCond(EvapCoolNum)%OutletNode
  InletNode = EvapCond(EvapCoolNum)%InletNode
  ControlNode = EvapCond(EvapCoolNum)%EvapControlNodeNum
  DesOutTemp = EvapCond(EvapCoolNum)%DesiredOutletTemp
  PartLoadFrac = 0.0
  CompName=EvapCond(EvapCoolNum)%EvapCoolerName

  ! If Evap Cooler runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
  If((GetCurrentScheduleValue(EvapCond(EvapCoolNum)%SchedPtr) .gt. 0.0) .and. &
       (Node(InletNode)%MassFlowRate .gt. MinAirMassFlow) .and. &
       (Node(InletNode)%Temp > Node(ControlNode)%TempSetPoint) .and. &
       (ABS(Node(InletNode)%Temp - DesOutTemp) .gt. TempControlTol) ) Then

        ! Get full load result, depending on model
        EvapCond(EvapCoolNum)%PartLoadFract = 1.0
        SELECT CASE (EvapCond(EvapCoolNum)%EvapCoolerType)
        CASE (iEvapCoolerInDirectRDDSpecial)
          CALL CalcIndirectResearchSpecialEvapCooler(EvapCoolNum)
          CALL UpdateEvapCooler(EvapCoolNum)
          FullOutput = Node(InletNode)%MassFlowRate *  &
                     (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                      - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

          ReqOutput = Node(InletNode)%MassFlowRate *  &
                     (PsyHFnTdbW(EvapCond(EvapCoolNum)%DesiredOutletTemp,Node(InletNode)%HumRat) - &
                      PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

          ! now reinit after test call
          CALL InitEvapCooler(EvapCoolNum)

        CASE (iEvapCoolerDirectResearchSpecial)
          CALL CalcDirectResearchSpecialEvapCooler(EvapCoolNum)
          CALL UpdateEvapCooler(EvapCoolNum)
          FullOutput = Node(OutletNode)%Temp - Node(InletNode)%Temp
          ReqOutput = EvapCond(EvapCoolNum)%DesiredOutletTemp - Node(InletNode)%Temp

          ! now reinit after test call
          CALL InitEvapCooler(EvapCoolNum)

        END SELECT

        ! Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
        ! Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
        ! Calculate the part load fraction
        If (FullOutput .EQ. 0.0D0) then
          FullOutput = 0.00001D0
        ENDIF
          PartLoadFrac = ReqOutput/FullOutput
        IF(PartLoadFrac.GT.1.0D0) THEN
          PartLoadFrac = 1.0D0
        ELSEIF(PartLoadFrac < 0.0D0) THEN
          PartLoadFrac = 0.0D0
        END IF

  ELSE ! No cooling
    PartLoadFrac = 0.0D0

  ENDIF   ! End of the cooler running If block
  !Set the final results
  EvapCond(EvapCoolNum)%PartLoadFract = PartLoadFrac

  RETURN
END SUBROUTINE CalcResearchSpecialPartLoad

SUBROUTINE CalcIndirectResearchSpecialEvapCooler(EvapCoolNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Subroutine models a "special" cooler that allows high effectiveness and controls

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! copied CalcWetIndirectEvapCooler as template for new cooler

          ! USE STATEMENTS:
      Use DataEnvironment, ONLY: OutDryBulbTemp, OutWetBulbTemp, OutHumRat, OutBaroPress
      USE DataWater ,      ONLY: WaterStorage
      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER, INTENT(IN) :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  !REAL(r64) Variables
  REAL(r64)  :: SecondaryInletWetBulbTemp ! entering wet bulb for secondary/purge side
  REAL(r64)  :: SecondaryInletDewpointTemp ! entering dewpoint for secondary/purge side
  REAL(r64)  :: StageEff    ! Stage Efficiency of the Heat Exchanger
  REAL(r64)  :: TEDB        ! Entering Dry Bulb Temperature
  REAL(r64)  :: TEWB        ! Entering Wet Bulb Temperature
  REAL(r64)  :: QHX         ! Q Across Sec HX in Watts or J/sec
  REAL(r64)  :: RhoWater
  REAL(r64)  :: RhoAir      ! Density of the primary side air
  REAL(r64)  :: CFMAir
  Integer    :: TertNode  ! inlet node for relief (from bulding) to mix for purge
  REAL(r64)  :: BoundTemp   ! temperature limit for outlet
  REAL(r64)  :: PartLoad
  REAL(r64)  :: TotalVolFlow
  REAL(r64)  :: TertMdot
  REAL(r64)  :: TertHumRate
  REAL(r64)  :: TertTemp
  REAL(r64)  :: TertRho
  REAL(r64)  :: TertVdot
  REAL(r64)  :: SecVdot
  REAL(r64)  :: SecRho
  REAL(r64)  :: SecMdot
  REAL(r64)  :: PurgeMdot
  REAL(r64)  :: PurgeHumRat
  REAL(r64)  :: PurgeEnthalpy
  REAL(r64)  :: PurgeTemp
  REAL(r64)  :: BlowDownVdot      =0.0
  REAL(r64)  :: DriftVdot         =0.0
  REAL(r64)  :: EvapVdot          =0.0

  ! If the Evaporative Cooler  is operating there should be some mass flow rate
  !  Also the evap cooler has to be scheduled to be available
  IF((EvapCond(EvapCoolNum)%InletMassFlowRate .GT. 0.0) .and. &
     (GetCurrentScheduleValue(EvapCond(EvapCoolNum)%SchedPtr) .gt. 0.0)) THEN


!******************************************************************************
!   THIS SUBROUTINE WILL CACULATE THE TEMPERATURE OF THE LEAVING AIR DRY BULB
!   FOR A WET COIL EVAPORATIVE COOLER
!******************************************************************************
!  INDIRECT STAGE EFFICIENCY FOR WET COIL INDIRECT EVAP COOLERS
      CFMAir = EvapCond(EvapCoolNum)%VolFlowRate            !Volume Flow Rate Primary Side
!      CFMSec = EvapCond(EvapCoolNum)%IndirectVolFlowRate    !Volume Flolw Rate Secondary Side

      StageEff = EvapCond(EvapCoolNum)%WetCoilMaxEfficiency

! This is model is for special indirect cooler with efficiency greater than 1.0

      IF(StageEff.GE.1.5) StageEff=1.5

      EvapCond(EvapCoolNum)%StageEff = StageEff

!***********************************************
!  Unit is allowed to mix relief air that would otherwise be exhausted outdoors for ventilation
!  If tertiary node is set >0 then it assumed that this node is the exhaust out of the building
!  and the remainder will be made up with outside air from the secondary node
!*********************************************

  TertNode = EvapCond(EvapCoolNum)%TertiaryInletNode
  If (tertNode .EQ. 0) then

    SecondaryInletWetBulbTemp  = PsyTwbFnTdbWPb(EvapCond(EvapCoolNum)%SecInletTemp, &
                                                EvapCond(EvapCoolNum)%SecInletHumRat,OutBaroPress)
    SecondaryInletDewpointTemp = PsyTdpFnTdbTwbPb(EvapCond(EvapCoolNum)%SecInletTemp, SecondaryInletWetBulbTemp, OutBaroPress)

  Else

    TotalVolFlow = EvapCond(EvapCoolNum)%IndirectVolFlowRate
    TertMdot    = node(TertNode)%MassFlowRate
    TertHumRate = node(TertNode)%HumRat
    TertTemp    = node(TertNode)%Temp
    ! is Node pressure available or better? using outdoor pressure for now
    TertRho     = PsyRhoAirFnPbTdbW(OutBaroPress, TertTemp , TertHumRate)
    TertVdot    = TertMdot/TertRho

    SecVdot     = TotalVolFlow - TertVdot

    IF (SecVdot .LT. 0.0) then ! all tertiary/releif air e.g. econonizer wide open
      SecVdot =  0.0
      SecondaryInletWetBulbTemp  = PsyTwbFnTdbWPb(TertTemp, TertHumRate , OutBaroPress)
      SecondaryInletDewpointTemp = PsyTdpFnTdbTwbPb(TertTemp, SecondaryInletWetBulbTemp, OutBaroPress)

    Else

      ! First determine mass flow of OA,  in secondary
      SecRho = PsyRhoAirFnPbTdbW(OutBaroPress, EvapCond(EvapCoolNum)%SecInletTemp,EvapCond(EvapCoolNum)%SecInletHumRat)
      SecMdot = SecRho * SecVdot
      ! Mass balance on moisture to get outlet air humidity ratio
      ! this mixing takes place before wet media.
      PurgeMdot   = SecMdot + TertMdot
      PurgeHumRat = (SecMdot * EvapCond(EvapCoolNum)%SecInletHumRat + TertMdot * TertHumRate) / PurgeMdot

      ! Energy balance to get outlet air enthalpy

      PurgeEnthalpy = (SecMdot * PsyHFnTdbW(EvapCond(EvapCoolNum)%SecInletTemp,EvapCond(EvapCoolNum)%SecInletHumRat) &
                 +   TertMdot * PsyHFnTdbW(TertTemp, TertHumRate)) / PurgeMdot

      ! Use Enthalpy and humidity ratio to get outlet temperature from psych chart

      PurgeTemp = PsyTdbFnHW(PurgeEnthalpy, PurgeHumRat)
      SecondaryInletWetBulbTemp  = PsyTwbFnTdbWPb(PurgeTemp, PurgeHumRat, OutBaroPress)
      SecondaryInletDewpointTemp = PsyTdpFnTdbTwbPb(PurgeTemp,SecondaryInletWetBulbTemp, OutBaroPress)
    Endif
 endif
 !***************************************************************************
!   TEMP LEAVING DRY BULB IS CALCULATED FROM A SIMPLE WET BULB APPROACH
!   MODEL GIVEN THE INDIRECT STAGE EFFICIENCY.
!   DRY BULB TEMP APPROACHES THE WET BULB TEMP ACROSS THE INDIRECT STAGE.
!***************************************************************************
      TEWB = EvapCond(EvapCoolNum)%InletWetBulbTemp
      TEDB = EvapCond(EvapCoolNum)%InletTemp
      PartLoad = EvapCond(EvapCoolNum)%PartLoadFract

  IF (PartLoad .EQ. 1.0D0) THEN
  !                                 Tout = Tin -  (   0.7    (Tin  - Tpurge,wb,in)
        EvapCond(EvapCoolNum)%OutletTemp = TEDB - StageEff*(TEDB - SecondaryInletWetBulbTemp )
  !  now bound with secondary dewpoint.
  ! unless the resulting Tout<=Tpurge,dp,in ; in which case Tout = Tin - 0.9(Tin-Tpurge,dp,in)

     BoundTemp = TEDB - EvapCond(EvapCoolNum)%DPBoundFactor *(TEDB - SecondaryInletDewpointTemp )
     IF (EvapCond(EvapCoolNum)%OutletTemp .LT. BoundTemp) THEN
       EvapCond(EvapCoolNum)%OutletTemp = BoundTemp
       EvapCond(EvapCoolNum)%DewPointBoundFlag = 1
     ENDIF
  ELSEIF ((partLoad .LT. 1.0D0) .AND. (partLoad .GT. 0.0D0)) THEN
    ! assume perfect control Use PLF for energy consumption
    IF (EvapCond(EvapCoolNum)%DesiredOutletTemp .LT. TEDB ) THEN
      EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%DesiredOutletTemp
    ENDIF
  ELSE
    !part load set to zero so no cooling
    EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%InletTemp
  ENDIF


!***************************************************************************
!                  CALCULATE THE WET BULB TEMP in the primary system air using PSYCH ROUTINES
! There is a constant humidity ratio across the primary side but a reduction in the dry bulb temp
      EvapCond(EvapCoolNum)%OuletWetBulbTemp = PsyTwbFnTdbWPb(EvapCond(EvapCoolNum)%OutletTemp, &
                                               EvapCond(EvapCoolNum)%InletHumRat,OutBaroPress)
!***************************************************************************
!                  CALCULATE other outlet propertiesusing PSYCH ROUTINES
      EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

      EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                             EvapCond(EvapCoolNum)%OutletHumRat)

!***************************************************************************
!                  POWER OF THE SECONDARY AIR FAN with part load factor applied (assumes const efficiency)
      IF (EvapCond(EvapCoolNum)%IndirectFanEff > 0.0D0) THEN
        EvapCond(EvapCoolNum)%EvapCoolerPower=EvapCond(EvapCoolNum)%EvapCoolerPower +      &
                                              EvapCond(EvapCoolNum)%IndirectFanDeltaPress* &
                                              EvapCond(EvapCoolNum)%IndirectVolFlowRate/   &
                                              EvapCond(EvapCoolNum)%IndirectFanEff* PartLoad
      ENDIF
!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!Add the pump energy to the total Evap Cooler energy comsumption
      EvapCond(EvapCoolNum)%EvapCoolerPower = EvapCond(EvapCoolNum)%EvapCoolerPower +  &
                                              EvapCond(EvapCoolNum)%IndirectRecircPumpPower* PartLoad


!******************
!             WATER CONSUMPTION IN LITERS OF WATER FOR Wet InDIRECT
!             H2O [m3/sec] = (QHx [J/s])/(2,500,000 [J/kg H2O] * RhoWater [kg H2O/m3 H2O])
!******************
!***** FIRST calculate the heat exchange on the primary air side**********
      RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,EvapCond(EvapCoolNum)%InletTemp,EvapCond(EvapCoolNum)%InletHumRat)
      QHX = CFMAir*RhoAir*(EvapCond(EvapCoolNum)%InletEnthalpy - EvapCond(EvapCoolNum)%OutletEnthalpy)

      RhoWater = RhoH2O(OutDryBulbTemp)
      EvapVdot=  (QHx)/(2500000.0 * RhoWater)
      DriftVdot = EvapVdot * EvapCond(EvapCoolNum)%DriftFraction
      IF (EvapCond(EvapCoolNum)%BlowDownRatio > 0.0D0) THEN
        BlowDownVdot =  EvapVdot / (EvapCond(EvapCoolNum)%BlowDownRatio - 1) - DriftVdot
        IF ( BlowDownVdot < 0.0 ) BlowDownVdot = 0.0
      ELSE
        BlowDownVdot = 0.0D0
      ENDIF
      EvapCond(EvapCoolNum)%EvapWaterConsumpRate =  EvapVdot + DriftVdot + BlowDownVdot
      ! A numerical check to keep from having very tiny negative water consumption values being reported
      IF(EvapCond(EvapCoolNum)%EvapWaterConsumpRate < 0.0) EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0

 ELSE
   ! The evap cooler is not running and does not change conditions from inlet to outlet
    EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%InletTemp

    EvapCond(EvapCoolNum)%OuletWetBulbTemp = EvapCond(EvapCoolNum)%InletWetBulbTemp

    EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

    EvapCond(EvapCoolNum)%OutletEnthalpy = EvapCond(EvapCoolNum)%InletEnthalpy

    EvapCond(EvapCoolNum)%EvapCoolerEnergy     = 0.0D0
    EvapCond(EvapCoolNum)%EvapCoolerPower      = 0.0D0
    EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0D0

 End IF

 ! all of the mass flowrates are not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletMassFlowRate         = EvapCond(EvapCoolNum)%InletMassFlowRate
 EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail
 EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail

 ! the pressure is not changed across the evap cooler
 EvapCond(EvapCoolNum)%OutletPressure = EvapCond(EvapCoolNum)%InletPressure

 RETURN

END SUBROUTINE CalcIndirectResearchSpecialEvapCooler

SUBROUTINE CalcDirectResearchSpecialEvapCooler(EvapCoolNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   March 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate model for direct evaporative cooler that is simple and controllable

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: SatEff      ! Saturation Efficiency of the CelDek Pad
  REAL(r64)  :: TEDB        ! Entering Dry Bulb Temperature
  REAL(r64)  :: TEWB        ! Entering Wet Bulb Temperature
  REAL(r64)  :: RhoWater
  REAL(r64)  :: PartLoad
  REAL(r64)  :: BlowDownVdot      =0.0
  REAL(r64)  :: DriftVdot         =0.0
  REAL(r64)  :: EvapVdot          =0.0

  ! If the Evaporative Cooler  is operating there should be some mass flow rate
  !  Also the evap cooler has to be scheduled to be available
  IF((EvapCond(EvapCoolNum)%InletMassFlowRate .GT. 0.0) .and. &
     (GetCurrentScheduleValue(EvapCond(EvapCoolNum)%SchedPtr) .gt. 0.0)) THEN

!***************************************************************************
!   TEMP LEAVING DRY BULB IS CALCULATED FROM SATURATION EFFICIENCY AS THE
!   DRY BULB TEMP APPROACHES THE WET BULB TEMP. WET BULB TEMP IS CONSTANT
!   ACROSS A DIRECT EVAPORATION COOLER.
      TEWB   = EvapCond(EvapCoolNum)%InletWetBulbTemp
      TEDB   = EvapCond(EvapCoolNum)%InletTemp
      SatEff = EvapCond(EvapCoolNum)%DirectEffectiveness
      PartLoad = EvapCond(EvapCoolNum)%PartLoadFract
      IF (PartLoad .EQ. 1.0) THEN
        EvapCond(EvapCoolNum)%OutletTemp = TEDB-((TEDB-TEWB)*SatEff)
        EvapCond(EvapCoolNum)%OuletWetBulbTemp = TEWB
        EvapCond(EvapCoolNum)%OutletHumRat = PsyWFnTdbTwbPb(EvapCond(EvapCoolNum)%OutletTemp,TEWB,OutBaroPress)
        EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                               EvapCond(EvapCoolNum)%OutletHumRat)
      ELSEIF ((partLoad .LT. 1.0D0) .AND. (partLoad .GT. 0.0D0)) THEN
        ! assume perfect control Use PLF for energy consumption
        IF (EvapCond(EvapCoolNum)%DesiredOutletTemp .LT. TEDB ) THEN
          EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%DesiredOutletTemp
          EvapCond(EvapCoolNum)%OuletWetBulbTemp = TEWB
          EvapCond(EvapCoolNum)%OutletHumRat = PsyWFnTdbTwbPb(EvapCond(EvapCoolNum)%OutletTemp,TEWB,OutBaroPress)

          EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                               EvapCond(EvapCoolNum)%OutletHumRat)
        ELSE !do no cooling
          EvapCond(EvapCoolNum)%OutletTemp = TEDB
          EvapCond(EvapCoolNum)%OuletWetBulbTemp = TEWB
          EvapCond(EvapCoolNum)%OutletHumRat = PsyWFnTdbTwbPb(EvapCond(EvapCoolNum)%OutletTemp,TEWB,OutBaroPress)
          EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                               EvapCond(EvapCoolNum)%OutletHumRat)
        ENDIF
      ELSE
        !part load set to zero so no cooling
        EvapCond(EvapCoolNum)%OutletTemp = TEDB
        EvapCond(EvapCoolNum)%OuletWetBulbTemp = TEWB
        EvapCond(EvapCoolNum)%OutletHumRat = PsyWFnTdbTwbPb(EvapCond(EvapCoolNum)%OutletTemp,TEWB,OutBaroPress)
        EvapCond(EvapCoolNum)%OutletEnthalpy = PsyHFnTdbW(EvapCond(EvapCoolNum)%OutletTemp,  &
                                             EvapCond(EvapCoolNum)%OutletHumRat)
      ENDIF

!***************************************************************************
!                  ENERGY CONSUMED BY THE RECIRCULATING PUMP
!Add the pump energy to the total Evap Cooler energy comsumption
      EvapCond(EvapCoolNum)%EvapCoolerPower = EvapCond(EvapCoolNum)%RecircPumpPower * PartLoad

!******************
!             WATER CONSUMPTION IN m3 OF WATER FOR DIRECT
!             H2O [m3/sec] = Delta W[KgH2O/Kg air]*Mass Flow Air[Kg air]
!                                /RhoWater [kg H2O/m3 H2O]
!******************
      RhoWater = RhoH2O(EvapCond(EvapCoolNum)%OutletTemp)
      EvapVdot =  (EvapCond(EvapCoolNum)%OutletHumRat - EvapCond(EvapCoolNum)%InletHumRat) *  &
                  EvapCond(EvapCoolNum)%InletMassFlowRate/Rhowater
      DriftVdot = EvapVdot * EvapCond(EvapCoolNum)%DriftFraction

      IF (EvapCond(EvapCoolNum)%BlowDownRatio > 0.0D0) THEN
        BlowDownVdot =  EvapVdot / (EvapCond(EvapCoolNum)%BlowDownRatio - 1.0D0) - DriftVdot
        IF ( BlowDownVdot < 0.0D0 ) BlowDownVdot = 0.0D0
      ELSE
        BlowDownVdot = 0.0D0
      ENDIF

      EvapCond(EvapCoolNum)%EvapWaterConsumpRate =  EvapVdot + DriftVdot + BlowDownVdot

      ! A numerical check to keep from having very tiny negative water consumption values being reported
      If(EvapCond(EvapCoolNum)%EvapWaterConsumpRate < 0.0) EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0


  Else
   ! The evap cooler is not running and does not change conditions from inlet to outlet
    EvapCond(EvapCoolNum)%OutletTemp = EvapCond(EvapCoolNum)%InletTemp

    EvapCond(EvapCoolNum)%OuletWetBulbTemp = EvapCond(EvapCoolNum)%InletWetBulbTemp

    EvapCond(EvapCoolNum)%OutletHumRat = EvapCond(EvapCoolNum)%InletHumRat

    EvapCond(EvapCoolNum)%OutletEnthalpy = EvapCond(EvapCoolNum)%InletEnthalpy
    EvapCond(EvapCoolNum)%EvapCoolerPower  = 0.0D0
    EvapCond(EvapCoolNum)%EvapCoolerEnergy = 0.0D0

    EvapCond(EvapCoolNum)%EvapWaterConsumpRate = 0.0D0

  End IF
 ! all of the mass flowrates are not changed across the evap cooler
  EvapCond(EvapCoolNum)%OutletMassFlowRate         = EvapCond(EvapCoolNum)%InletMassFlowRate
  EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMaxAvail
  EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail = EvapCond(EvapCoolNum)%InletMassFlowRateMinAvail

  ! the pressure is not changed across the evap cooler
  EvapCond(EvapCoolNum)%OutletPressure = EvapCond(EvapCoolNum)%InletPressure

  RETURN

END SUBROUTINE CalcDirectResearchSpecialEvapCooler


! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the EvapCooler Module
! *****************************************************************************

SUBROUTINE UpdateEvapCooler(EvapCoolNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   October 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataWater
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutletNode
  Integer             :: InletNode
  REAL(r64)           :: AvailWaterRate = 0.0

   OutletNode = EvapCond(EvapCoolNum)%OutletNode
   InletNode  = EvapCond(EvapCoolNum)%InletNode

   ! Set the outlet air nodes of the EvapCooler
   Node(OutletNode)%MassFlowRate  = EvapCond(EvapCoolNum)%OutletMassFlowRate
   Node(OutletNode)%MassFlowRateMaxAvail  = EvapCond(EvapCoolNum)%OutletMassFlowRateMaxAvail
   Node(OutletNode)%MassFlowRateMinAvail  = EvapCond(EvapCoolNum)%OutletMassFlowRateMinAvail
   Node(OutletNode)%Temp          = EvapCond(EvapCoolNum)%OutletTemp
   Node(OutletNode)%HumRat        = EvapCond(EvapCoolNum)%OutletHumRat
   Node(OutletNode)%Enthalpy      = EvapCond(EvapCoolNum)%OutletEnthalpy
   Node(OutletNode)%Press         = EvapCond(EvapCoolNum)%OutletPressure
   ! Set the outlet nodes for properties that just pass through & not used
   Node(OutletNode)%Quality         = Node(InletNode)%Quality

    ! Set the demand request for supply water from water storage tank (if needed)
    If (EvapCond(EvapCoolNum)%EvapWaterSupplyMode == WaterSupplyFromTank) Then
      WaterStorage(EvapCond(EvapCoolNum)%EvapWaterSupTankID)%VdotRequestDemand(EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID) &
       = EvapCond(EvapCoolNum)%EvapWaterConsumpRate
    endif

   !check if should be starved by restricted flow from tank
    IF (EvapCond(EvapCoolNum)%EvapWaterSupplyMode == WaterSupplyFromTank) Then
      AvailWaterRate = &
      WaterStorage(EvapCond(EvapCoolNum)%EvapWaterSupTankID)%VdotAvailDemand(EvapCond(EvapCoolNum)%EvapWaterTankDemandARRID)
      If (AvailWaterRate < EvapCond(EvapCoolNum)%EvapWaterConsumpRate) Then
        EvapCond(EvapCoolNum)%EvapWaterStarvMakup = EvapCond(EvapCoolNum)%EvapWaterConsumpRate - AvailWaterRate
        EvapCond(EvapCoolNum)%EvapWaterConsumpRate = AvailWaterRate
      endif
    ENDIF

  IF (Contaminant%CO2Simulation) Then
    Node(OutletNode)%CO2 = Node(InletNode)%CO2
  End If

  IF (Contaminant%GenericContamSimulation) Then
    Node(OutletNode)%GenContam = Node(InletNode)%GenContam
  End If

  RETURN
END Subroutine UpdateEvapCooler


!        End of Update subroutines for the EvapCooler Module
! *****************************************************************************


! Beginning of Reporting subroutines for the EvapCooler Module
! *****************************************************************************

SUBROUTINE ReportEvapCooler(EvapCoolNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: EvapCoolNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
 ! report the Evap Cooler energy from this component
   EvapCond(EvapCoolNum)%EvapCoolerPower= EvapCond(EvapCoolNum)%EvapCoolerPower
   EvapCond(EvapCoolNum)%EvapCoolerEnergy= EvapCond(EvapCoolNum)%EvapCoolerPower*TimeStepSys*SecInHour

   ! Report Water comsumption in liters per timestep
   EvapCond(EvapCoolNum)%EvapWaterConsump=EvapCond(EvapCoolNum)%EvapWaterConsumpRate*TimeStepSys*SecInHour


  RETURN
END Subroutine ReportEvapCooler

!        End of Reporting subroutines for the EvaporativeCoolers Module
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

End Module EvaporativeCoolers

