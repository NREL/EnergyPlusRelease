!Note: Multiple Chiller Modules included in this file.
!      Find the chiller you are looking for by searching for "ChillerModule"

!      The Electric, Diesel, and Gas Turbine chiller models are very similar.
!         They only differ in the unit that drives the chiller.  Therefore,
!         the Blast curve fit portion for each chiller is the same.
! Contents:
! ChillerElectric
! ChillerEngineDriven
! ChillerGasTurbine
! ChillerCONSTCop

MODULE ChillerElectric  !Electric ChillerModule

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the Electric vapor
          ! compression Chillers.

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the Electric chiller
          ! is available to meet a loop cooling demand, it calls SimElectric
          ! Chiller which in turn calls the Electric chiller model.
          ! The Electric chiller model is based on a polynomial fit of chiller
          ! performance data, just like BLAST.

          ! REFERENCES:
          ! 1. BLAST Users Manual


          ! OTHER NOTES:
          ! The CHILLER program from the Electric family of software can be used
          ! to generate the coefficients for the model.

          ! The Electric, Diesel, and Gas Turbine chiller models are very similar.
          ! They only differ in the unit that drives the chiller.  Therefore,
          ! the Blast curve fit portion for each chiller is the same and is in the
          ! general routines module under CalcChillerPower.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,    ONLY : MaxNameLength, InitConvTemp, WarmupFlag
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE DataPlant,       ONLY: DeltaTemptol, TypeOf_Chiller_Electric
USE General,         ONLY: TrimSigDigits


IMPLICIT NONE
PRIVATE

          !MODULE PARAMETER DEFINITIONS:
! Parameters for use in Chillers
INTEGER, PARAMETER :: AirCooled = 1
INTEGER, PARAMETER :: WaterCooled = 2
INTEGER, PARAMETER :: EvapCooled = 3

          ! MODULE VARIABLE DECLARATIONS:
INTEGER, SAVE ,PUBLIC    :: NumElectricChillers    =0   ! number of Electric chillers specified in input
REAL(r64)                :: CondMassFlowRate       =0.0 ! Kg/s - condenser mass flow rate, water side
REAL(r64)                :: EvapMassFlowRate       =0.0 ! Kg/s - evaporator mass flow rate, water side
REAL(r64)                :: CondOutletTemp         =0.0 ! C - condenser outlet temperature, water side
REAL(r64)                :: EvapOutletTemp         =0.0 ! C - evaporator outlet temperature, water side
REAL(r64)                :: Power                  =0.0 ! W - rate of chiller energy use
REAL(r64)                :: QEvaporator            =0.0 ! W - rate of heat transfer to the evaporator coil
REAL(r64)                :: QCondenser             =0.0 ! W - rate of heat transfer to the condenser coil
REAL(r64)                :: Energy                 =0.0 ! J - chiller energy use
REAL(r64)                :: EvaporatorEnergy       =0.0 ! J - rate of heat transfer to the evaporator coil
REAL(r64)                :: CondenserEnergy        =0.0 ! J - rate of heat transfer to the condenser coil
REAL(r64)                :: QHeatRecovered         =0.0 ! W - rate of heat transfer to the Heat Recovery coil
REAL(r64)                :: HeatRecOutletTemp      =0.0 ! C - Heat Rec outlet temperature, water side
REAL(r64)                :: ChillerCyclingRatio    =0.0 ! Cycling ratio for chiller when load is below MinPLR
REAL(r64)                :: BasinHeaterPower       =0.0 ! Basin heater power (W)

TYPE, PUBLIC             :: ElectricChillerSpecs
       CHARACTER(len=MaxNameLength) :: Name     =' ' ! user identifier
       INTEGER           :: CondenserType       = 0  ! Type of Condenser - Air or Water Cooled

       REAL(r64)         :: NomCap              =0.0 ! W - design nominal capacity of chiller
       REAL(r64)         :: COP                 =0.0 ! coefficient of performance
       LOGICAL           :: ConstantFlow     =.false.! True if this is a Constant Flow Chiller
       LOGICAL           :: VariableFlow     =.false.! True if this is a Variable Flow Chiller
       LOGICAL           :: VariableFlowSetToLoop= .FALSE. ! True if the setpoint is missing at the outlet node
       LOGICAL           :: VariableFlowErrDone  = .FALSE.  ! true if setpoint warning issued
       REAL(r64)         :: EvapVolFlowRate     =0.0 ! m**3/s - design nominal water volumetric flow rate through the evaporator
       REAL(r64)         :: EvapMassFlowRateMax =0.0 ! kg/s - design water mass flow rate through evaporator
       REAL(r64)         :: CondVolFlowRate     =0.0 ! m**3/s - design nominal water volumetric flow rate through the condenser
       REAL(r64)         :: CondMassFlowRateMax =0.0 ! kg/s - design water mass flow rate through condenser
       INTEGER           :: EvapInletNodeNum    =0   ! Node number on the inlet side of the plant
       INTEGER           :: EvapOutletNodeNum   =0   ! Node number on the outlet side of the plant
       INTEGER           :: CondInletNodeNum    =0   ! Node number on the inlet side of the condenser
       INTEGER           :: CondOutletNodeNum   =0   ! Node number on the outlet side of the condenser
       REAL(r64)         :: MinPartLoadRat      =0.0 ! (Electric MIN) min allowed operating frac full load
       REAL(r64)         :: MaxPartLoadRat      =0.0 ! (Electric MAX) max allowed operating frac full load
       REAL(r64)         :: OptPartLoadRat      =0.0 ! (Electric BEST) optimal operating frac full load
       REAL(r64)         :: TempDesCondIn       =0.0 ! C - (Electric ADJTC(1)The design secondary loop fluid
                                                     ! temperature at the chiller condenser side inlet
       REAL(r64)         :: TempRiseCoef        =0.0 ! (Electric ADJTC(2)) correction factor for off ChillDesign oper.
       REAL(r64)         :: TempDesEvapOut      =0.0 ! C - (Electric ADJTC(3)The design primary loop fluid
                                                     ! temperature at the chiller evaporator side outlet
       REAL(r64),DIMENSION(3) :: CapRatCoef          =0.0 ! (Electric RCAVC() ) coeff of cap ratio poly fit
       REAL(r64),DIMENSION(3) :: PowerRatCoef        =0.0 ! (Electric ADJEC() ) coeff of power rat poly fit
       REAL(r64),DIMENSION(3) :: FullLoadCoef        =0.0 ! (Electric RPWRC() ) coeff of full load poly. fit
       REAL(r64)         :: TempLowLimitEvapOut =0.0 ! C - low temperature shut off
       REAL(r64)         :: DesignHeatRecVolFlowRate = 0.0 ! m3/s, Design Water mass flow rate through heat recovery loop
       REAL(r64)         :: DesignHeatRecMassFlowRate = 0.0 ! kg/s, Design Water mass flow rate through heat recovery loop
       REAL(r64)         :: SizFac                    = 0.0 ! sizing factor
       REAL(r64)         :: BasinHeaterPowerFTempDiff = 0.0 ! Basin heater capacity per degree C below set point (W/C)
       REAL(r64)         :: BasinHeaterSetPointTemp   = 0.0 ! Set point temperature for basin heater operation (C)
       LOGICAL           :: HeatRecActive = .False.    ! True entered Heat Rec Vol Flow Rate >0
       INTEGER           :: HeatRecInletNodeNum = 0    ! Node number on the heat recovery inlet side of the condenser
       INTEGER           :: HeatRecOutletNodeNum = 0   ! Node number on the heat recovery outlet side of the condenser
       INTEGER           :: ErrCount1 = 0      ! for recurring error messages
       INTEGER           :: ErrCount2 = 0      ! for recurring error messages
       INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
       INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
       INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
       INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
       INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
       INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
       INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
       INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
       INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop side index
       INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
       INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
       INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
       INTEGER           :: BasinHeaterSchedulePtr  = 0   ! Pointer to basin heater schedule
       INTEGER           :: CondMassFlowIndex = 0
       CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
       CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
       REAL(r64)         :: MsgDataLast   = 0.0 ! value of data when warning occurred (passed to Recurring Warn)
       LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
       INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
       LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE ElectricChillerSpecs

TYPE ReportVars
  REAL(r64)    :: Power              = 0.0 ! reporting: power
  REAL(r64)    :: QEvap              = 0.0 ! reporting: evaporator heat transfer
  REAL(r64)    :: QCond              = 0.0 ! reporting: condenser heat transfer
  REAL(r64)    :: Energy             = 0.0 ! reporting: Energy
  REAL(r64)    :: EvapEnergy         = 0.0 ! reporting: evaporator heat transfer Energy
  REAL(r64)    :: CondEnergy         = 0.0 ! reporting: condenser heat transfer Energy
  REAL(r64)    :: CondInletTemp      = 0.0 ! reporting: condenser inlet temperature
  REAL(r64)    :: EvapInletTemp      = 0.0 ! reporting: evaporator inlet temperature
  REAL(r64)    :: CondOutletTemp     = 0.0 ! reporting: condenser outlet temperature
  REAL(r64)    :: EvapOutletTemp     = 0.0 ! reporting: evaporator outlet temperature
  REAL(r64)    :: Evapmdot           = 0.0 ! reporting: evaporator mass flow rate
  REAL(r64)    :: Condmdot           = 0.0 ! reporting: condenser mass flow rate
  REAL(r64)    :: ActualCOP          = 0.0 ! reporting: coefficient of performance
  REAL(r64)    :: QHeatRecovery      = 0.0
  REAL(r64)    :: EnergyHeatRecovery = 0.0
  REAL(r64)    :: HeatRecInletTemp   = 0.0
  REAL(r64)    :: HeatRecOutletTemp  = 0.0
  REAL(r64)    :: HeatRecMassFlow    = 0.0
  REAL(r64)    :: BasinHeaterPower       = 0.0  ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0  ! Basin heater energy consumption (J)
END TYPE ReportVars

TYPE (ElectricChillerSpecs), ALLOCATABLE, PUBLIC, DIMENSION(:)  :: ElectricChiller  !dimension to number of machines

TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::ElectricChillerReport
LOGICAL     :: GetInput = .TRUE.! then TRUE, calls subroutine to read input file.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PRIVATE    CalcElectricChillerModel
PRIVATE    GetElectricChillerInput
PRIVATE    InitElectricChiller
PRIVATE    SizeElectricChiller
PRIVATE    UpdateElectricChillerRecords
PRIVATE    ChillerHeatRecovery
PUBLIC     SimElectricChiller
!PUBLIC     SimElectricChillerHeatRecovery


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Electric Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimElectricChiller(LoopNum, LoopSide, ChillerType,ChillerName,EquipFlowCtrl,CompIndex,RunFlag,FirstHVACIteration, &
                              InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       April 1999, May 200-Taecheol Kim
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the Electric chiller model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide, UpdateComponentHeatRecoverySide

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: LoopNum  ! Flow control mode for the equipment
  INTEGER, INTENT(IN)      :: LoopSide            ! chiller number pointer
  CHARACTER(len=*), INTENT(IN)  :: ChillerType   ! type of chiller !unused1208
  CHARACTER(len=*), INTENT(IN)  :: ChillerName   ! user specified name of chiller
  INTEGER, INTENT(IN)       :: EquipFlowCtrl  ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)    :: CompIndex            ! chiller number pointer
  LOGICAL , INTENT(IN)      :: RunFlag             ! simulate chiller when TRUE
  LOGICAL , INTENT(IN)      :: FirstHVACIteration      ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT)    :: InitLoopEquip            ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)    :: MyLoad              ! loop demand component will meet
  REAL(r64)         :: MinCap           ! W - minimum operating capacity of chiller
  REAL(r64)         :: MaxCap           ! W - maximum operating capacity of chiller
  REAL(r64)         :: OptCap           ! W - optimal operating capacity of chiller
  LOGICAL, INTENT(IN)         :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(OUT)      :: SizingFactor     ! sizing factor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ChillNum            ! chiller number pointer

          !Get chiller data from input file
  IF (GetInput) THEN
    CALL GetElectricChillerInput
    GetInput = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(ChillerName,ElectricChiller%Name,NumElectricChillers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimElectricChiller: Specified Chiller not one of Valid Electric Chillers='//TRIM(ChillerName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumElectricChillers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimElectricChiller:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumElectricChillers))//  &
                          ', Entered Unit name='//TRIM(ChillerName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (ChillerName /= ElectricChiller(ChillNum)%Name) THEN
        CALL ShowFatalError('SimElectricChiller: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                            TRIM(ElectricChiller(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    CALL InitElectricChiller(ChillNum,RunFlag,MyLoad,FirstHVACIteration)
    CALL SizeElectricChiller(ChillNum)
    IF (LoopNum == ElectricChiller(ChillNum)%CWLoopNum) THEN ! chilled water loop

      MinCap = ElectricChiller(ChillNum)%NomCap*ElectricChiller(ChillNum)%MinPartLoadRat
      MaxCap = ElectricChiller(ChillNum)%NomCap*ElectricChiller(ChillNum)%MaxPartLoadRat
      OptCap = ElectricChiller(ChillNum)%NomCap*ElectricChiller(ChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = ElectricChiller(ChillNum)%SizFac
    END IF
    RETURN
  END IF

    ! calculate model depending on where called from
  IF (LoopNum == ElectricChiller(ChillNum)%CWLoopNum) THEN ! chilled water loop

    CALL InitElectricChiller(ChillNum,RunFlag,MyLoad, FirstHVACIteration)
    CALL CalcElectricChillerModel(ChillNum,MyLoad,EquipFlowCtrl,Runflag,FirstHVACIteration)
    CALL UpdateElectricChillerRecords(MyLoad,RunFlag,ChillNum)

  ELSEIF (LoopNum == ElectricChiller(ChillNum)%CDLoopNum) THEN ! condenser loop
    CALL UpdateChillerComponentCondenserSide(ElectricChiller(ChillNum)%CDLoopNum, &
                                     ElectricChiller(ChillNum)%CDLoopSideNum,     &
                                     TypeOf_Chiller_Electric,                     &
                                     ElectricChiller(ChillNum)%CondInletNodeNum,  &
                                     ElectricChiller(ChillNum)%CondOutletNodeNum, &
                                     ElectricChillerReport(ChillNum)%QCond,             &
                                     ElectricChillerReport(ChillNum)%CondInletTemp,     &
                                     ElectricChillerReport(ChillNum)%CondOutletTemp,    &
                                     ElectricChillerReport(ChillNum)%Condmdot,          &
                                     FirstHVACIteration)
  ELSEIF (LoopNum == ElectricChiller(ChillNum)%HRLoopNum) THEN  ! heat recovery loop
    CALL UpdateComponentHeatRecoverySide(ElectricChiller(ChillNum)%HRLoopNum,               &
                                    ElectricChiller(ChillNum)%HRLoopSideNum,           &
                                    TypeOf_Chiller_Electric,                           &
                                    ElectricChiller(ChillNum)%HeatRecInletNodeNum,     &
                                    ElectricChiller(ChillNum)%HeatRecOutletNodeNum,    &
                                    ElectricChillerReport(ChillNum)%QHeatRecovery,     &
                                    ElectricChillerReport(ChillNum)%HeatRecInletTemp,  &
                                    ElectricChillerReport(ChillNum)%HeatRecOutletTemp, &
                                    ElectricChillerReport(ChillNum)%HeatRecMassFlow ,  &
                                    FirstHVACIteration)
  ENDIF

RETURN
END SUBROUTINE SimElectricChiller

! End Electric Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of Electric Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetElectricChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the Electric Chiller model.


            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General,           ONLY: RoundSigDigits
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,    ONLY: GetScheduleIndex

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: ChillerNum !chiller counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
!  CHARACTER(len=MaxNameLength),DIMENSION(9)   :: AlphArray !character string data
!  REAL(r64),                        DIMENSION(22)  :: NumArray  !numeric data
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay
!  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject  ! for ease in renaming.

         !FLOW
  cCurrentModuleObject = 'Chiller:Electric'
  NumElectricChillers = GetNumObjectsFound(TRIM(cCurrentModuleObject))

  IF (NumElectricChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' Equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

            !See if load distribution manager has already gotten the input
    IF (ALLOCATED(ElectricChiller))RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (ElectricChiller(NumElectricChillers))

  ALLOCATE (ElectricChillerReport(NumElectricChillers))
  ALLOCATE(CheckEquipName(NumElectricChillers))
  CheckEquipName=.true.

         !LOAD ARRAYS WITH Electric CURVE FIT CHILLER DATA
  DO ChillerNum = 1 , NumElectricChillers
    CALL GetObjectItem(TRIM(cCurrentModuleObject),ChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    NumericFieldNames=cNumericFieldNames,AlphaFieldnames=cAlphaFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ElectricChiller%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%Name                = cAlphaArgs(1)

    IF (cAlphaArgs(2) == 'AIRCOOLED' ) THEN
      ElectricChiller(ChillerNum)%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(2) == 'WATERCOOLED' ) THEN
      ElectricChiller(ChillerNum)%CondenserType       = WaterCooled
    ELSEIF (cAlphaArgs(2) == 'EVAPORATIVELYCOOLED' ) THEN
      ElectricChiller(ChillerNum)%CondenserType       = EvapCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF


    ElectricChiller(ChillerNum)%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%EvapInletNodeNum    = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ElectricChiller(ChillerNum)%EvapOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    IF (ElectricChiller(ChillerNum)%CondenserType == AirCooled .or. ElectricChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
       !If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      !  since it is not used elsewhere for connection
      ! for transition purposes, add this node if not there.
      IF(lAlphaFieldBlanks(5))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(6) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(6) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(6) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      ElectricChiller(ChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(ElectricChiller(ChillerNum)%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(5)))
      ENDIF

      ElectricChiller(ChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSEIF (ElectricChiller(ChillerNum)%CondenserType == WaterCooled) THEN
      ElectricChiller(ChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ElectricChiller(ChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser Water Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ElseIf ( lAlphaFieldBlanks(6) ) Then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      ElectricChiller(ChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ElectricChiller(ChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ElseIf ( lAlphaFieldBlanks(6) ) Then
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF


    ElectricChiller(ChillerNum)%MinPartLoadRat      = rNumericArgs(3)
    ElectricChiller(ChillerNum)%MaxPartLoadRat      = rNumericArgs(4)
    ElectricChiller(ChillerNum)%OptPartLoadRat      = rNumericArgs(5)
    ElectricChiller(ChillerNum)%TempDesCondIn       = rNumericArgs(6)
    ElectricChiller(ChillerNum)%TempRiseCoef        = rNumericArgs(7)
    ElectricChiller(ChillerNum)%TempDesEvapOut      = rNumericArgs(8)
    ElectricChiller(ChillerNum)%EvapVolFlowRate     = rNumericArgs(9)
    ElectricChiller(ChillerNum)%CondVolFlowRate     = rNumericArgs(10)
    ElectricChiller(ChillerNum)%CapRatCoef(1)       = rNumericArgs(11)
    ElectricChiller(ChillerNum)%CapRatCoef(2)       = rNumericArgs(12)
    ElectricChiller(ChillerNum)%CapRatCoef(3)       = rNumericArgs(13)
    IF ((rNumericArgs(11)+rNumericArgs(12)+rNumericArgs(13)) == 0.0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Sum of Capacity Ratio Coef = 0.0, chiller='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ElectricChiller(ChillerNum)%PowerRatCoef(1)     = rNumericArgs(14)
    ElectricChiller(ChillerNum)%PowerRatCoef(2)     = rNumericArgs(15)
    ElectricChiller(ChillerNum)%PowerRatCoef(3)     = rNumericArgs(16)
    ElectricChiller(ChillerNum)%FullLoadCoef(1)     = rNumericArgs(17)
    ElectricChiller(ChillerNum)%FullLoadCoef(2)     = rNumericArgs(18)
    ElectricChiller(ChillerNum)%FullLoadCoef(3)     = rNumericArgs(19)
    ElectricChiller(ChillerNum)%TempLowLimitEvapOut = rNumericArgs(20)
    ElectricChiller(ChillerNum)%SizFac              = rNumericArgs(22)
    IF (ElectricChiller(ChillerNum)%SizFac <= 0.0) ElectricChiller(ChillerNum)%SizFac = 1.0d0

    If(cAlphaArgs(7) .eq. 'CONSTANTFLOW') Then
       ElectricChiller(ChillerNum)%ConstantFlow = .True.
       ElectricChiller(ChillerNum)%VariableFlow = .False.
    Else If(cAlphaArgs(7) .eq. 'VARIABLEFLOW') Then
       ElectricChiller(ChillerNum)%ConstantFlow = .False.
       ElectricChiller(ChillerNum)%VariableFlow = .True.
    Else  ! We will assume a variable flow chiller is none specified
       ElectricChiller(ChillerNum)%ConstantFlow = .False.
       ElectricChiller(ChillerNum)%VariableFlow = .True.
    End If

   ! These are the Heat Recovery Inputs
    ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(21)
    IF (ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate > 0.0) THEN
      ElectricChiller(ChillerNum)%HeatRecActive=.true.
      ElectricChiller(ChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (ElectricChiller(ChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      ElectricChiller(ChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (ElectricChiller(ChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(8),cAlphaArgs(9),'Heat Recovery Nodes')
      CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillerNum)%HeatRecInletNodeNum, &
                                ElectricChiller(ChillerNum)%DesignHeatRecVolFlowRate )

      ! Condenser flow rate must be specified for heat reclaim
      IF (ElectricChiller(ChillerNum)%CondenserType == AirCooled .OR. &
          ElectricChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        IF(ElectricChiller(ChillerNum)%CondVolFlowRate .LE. 0.0)THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(rNumericArgs(10),6)))
          CALL ShowSevereError('Condenser fluid flow rate must be specified for Heat Reclaim applications.')
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        END IF
      END IF
    ELSE
      ElectricChiller(ChillerNum)%HeatRecActive=.false.
      ElectricChiller(ChillerNum)%DesignHeatRecMassFlowRate = 0.0
      ElectricChiller(ChillerNum)%HeatRecInletNodeNum   = 0
      ElectricChiller(ChillerNum)%HeatRecOutletNodeNum   = 0
      ! if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
      IF (ElectricChiller(ChillerNum)%CondenserType == AirCooled .OR. &
          ElectricChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        ElectricChiller(ChillerNum)%CondVolFlowRate = 0.0011d0  ! set to avoid errors in calc routine
      END IF
      IF ((.NOT. lAlphaFieldBlanks(8))  .OR. (.NOT. lAlphaFieldBlanks(9))) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '//  &
                             TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('However, Node names were specified for Heat Recovery inlet or outlet nodes')
      END IF

    END IF
    !   Basin heater power as a function of temperature must be greater than or equal to 0
    ElectricChiller(ChillerNum)%BasinHeaterPowerFTempDiff = rNumericArgs(23)
    IF(rNumericArgs(23) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(ElectricChiller(ChillerNum)%Name)//&
                     '" TRIM(cNumericFieldNames(23)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    ElectricChiller(ChillerNum)%BasinHeaterSetPointTemp = rNumericArgs(24)

    IF(ElectricChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 24) THEN
        ElectricChiller(ChillerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(ElectricChiller(ChillerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(ElectricChiller(ChillerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(24))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(10))THEN
      ElectricChiller(ChillerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(10))
      IF(ElectricChiller(ChillerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(ElectricChiller(ChillerNum)%Name)//&
                       '" TRIM(cAlphaFieldNames(10)) "'//TRIM(cAlphaArgs(10)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO ChillerNum = 1, NumElectricChillers
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          ElectricChillerReport(ChillerNum)%Power,'System','Average',ElectricChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Consumption [J]', &
          ElectricChillerReport(ChillerNum)%Energy,'System','Sum',ElectricChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Evap Heat Trans Rate [W]', &
          ElectricChillerReport(ChillerNum)%QEvap,'System','Average',ElectricChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Heat Trans [J]', &
          ElectricChillerReport(ChillerNum)%EvapEnergy,'System','Sum',ElectricChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evap Water Inlet Temp [C]', &
          ElectricChillerReport(ChillerNum)%EvapInletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Outlet Temp [C]', &
          ElectricChillerReport(ChillerNum)%EvapOutletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Mass Flow Rate [kg/s]', &
          ElectricChillerReport(ChillerNum)%Evapmdot,'System','Average',ElectricChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Cond Heat Trans Rate [W]', &
          ElectricChillerReport(ChillerNum)%QCond,'System','Average',ElectricChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cond Heat Trans [J]', &
          ElectricChillerReport(ChillerNum)%CondEnergy,'System','Sum',ElectricChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller COP [W/W]', &
          ElectricChillerReport(ChillerNum)%ActualCOP,'System','Average',ElectricChiller(ChillerNum)%Name)

        !Condenser mass flow and outlet temp are valid for water cooled
     IF (ElectricChiller(ChillerNum)%CondenserType == WaterCooled)THEN
       CALL SetupOutputVariable('Chiller Cond Water Inlet Temp [C]', &
            ElectricChillerReport(ChillerNum)%CondInletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)
       CALL SetupOutputVariable('Chiller Cond Water Outlet Temp [C]', &
            ElectricChillerReport(ChillerNum)%CondOutletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)
       CALL SetupOutputVariable('Chiller Cond Water Mass Flow Rate [kg/s]', &
            ElectricChillerReport(ChillerNum)%Condmdot,'System','Average',ElectricChiller(ChillerNum)%Name)
     ELSEIF (ElectricChiller(ChillerNum)%CondenserType == AirCooled) THEN
       CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
            ElectricChillerReport(ChillerNum)%CondInletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)
     ELSEIF (ElectricChiller(ChillerNum)%CondenserType == EvapCooled) THEN
       CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
            ElectricChillerReport(ChillerNum)%CondInletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)
       IF(ElectricChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
         CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
          ElectricChillerReport(ChillerNum)%BasinHeaterPower,'System','Average',ElectricChiller(ChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Basin Heater Electric Consumption [J]', &
          ElectricChillerReport(ChillerNum)%BasinHeaterConsumption,'System','Sum',ElectricChiller(ChillerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
       END IF
     ENDIF

     !If heat recovery is active then setup report variables
     IF (ElectricChiller(ChillerNum)%HeatRecActive) THEN
         CALL SetupOutputVariable('Chiller Heat Recovery Rate [W]', &
           ElectricChillerReport(ChillerNum)%QHeatRecovery,'System','Average',ElectricChiller(ChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Heat Recovery [J]', &
           ElectricChillerReport(ChillerNum)%EnergyHeatRecovery,'System','Sum',ElectricChiller(ChillerNum)%Name,  &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')
         CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temp[C]', &
           ElectricChillerReport(ChillerNum)%HeatRecInletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)

         CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temp[C]', &
           ElectricChillerReport(ChillerNum)%HeatRecOutletTemp,'System','Average',ElectricChiller(ChillerNum)%Name)

         CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
           ElectricChillerReport(ChillerNum)%HeatRecMassFlow,'System','Average',ElectricChiller(ChillerNum)%Name)
     ENDIF

  END DO



RETURN
END SUBROUTINE GetElectricChillerInput

! End of Get Input subroutines for the Electric Chiller Module
!******************************************************************************

SUBROUTINE InitElectricChiller(ChillNum,RunFlag, MyLoad, FirstHVACIteration)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Electric Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_Electric, ScanPlantLoopsForObject, PlantSizesOkayToFinalize, &
                              PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current electric chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad
  LOGICAL, INTENT(IN)  :: FirstHVACIteration      ! initialize variables when TRUE

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: SupplySideLoopNum = 2
  CHARACTER(len=*), PARAMETER :: RoutineName='InitElectricChiller'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER :: EvapInletNode
  INTEGER :: EvapOutletNode
  INTEGER :: HeatRecInNode
  INTEGER :: HeatRecOutNode
  LOGICAL :: errFlag
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: mdot ! local mass flow rate
  REAL(r64) :: mdotCond ! local mass flow rate for condenser

  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  INTEGER :: BranchIndex
  INTEGER :: CompIndex
  LOGICAL :: FatalError
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumElectricChillers))
    ALLOCATE(MyEnvrnFlag(NumElectricChillers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  CondInletNode  = ElectricChiller(ChillNum)%CondInletNodeNum
  CondOutletNode = ElectricChiller(ChillNum)%CondOutletNodeNum
  EvapInletNode  = ElectricChiller(ChillNum)%EvapInletNodeNum
  EvapOutletNode = ElectricChiller(ChillNum)%EvapOutletNodeNum

  IF (ElectricChiller(ChillNum)%HeatRecActive ) THEN
    HeatRecInNode = ElectricChiller(ChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = ElectricChiller(ChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(ElectricChiller(ChillNum)%Name, &
                                 TypeOf_Chiller_Electric, &
                                 ElectricChiller(ChillNum)%CWLoopNum, &
                                 ElectricChiller(ChillNum)%CWLoopSideNum, &
                                 ElectricChiller(ChillNum)%CWBranchNum, &
                                 ElectricChiller(ChillNum)%CWCompNum, &
                                 LowLimitTemp = ElectricChiller(ChillNum)%TempLowLimitEvapOut, &
                                 InletNodeNumber = ElectricChiller(ChillNum)%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (ElectricChiller(ChillNum)%CondenserType /= AirCooled .AND. &
        ElectricChiller(ChillNum)%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(ElectricChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_Electric, &
                                   ElectricChiller(ChillNum)%CDLoopNum, &
                                   ElectricChiller(ChillNum)%CDLoopSideNum, &
                                   ElectricChiller(ChillNum)%CDBranchNum, &
                                   ElectricChiller(ChillNum)%CDCompNum, &
                                   InletNodeNumber = ElectricChiller(ChillNum)%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElectricChiller(ChillNum)%CWLoopNum,      &
                                          ElectricChiller(ChillNum)%CWLoopSideNum,  &
                                          ElectricChiller(ChillNum)%CDLoopNum,      &
                                          ElectricChiller(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_Electric, .TRUE. )
    ENDIF
    IF (ElectricChiller(ChillNum)%HeatRecActive) THEN
      CALL ScanPlantLoopsForObject(ElectricChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_Electric, &
                                   ElectricChiller(ChillNum)%HRLoopNum, &
                                   ElectricChiller(ChillNum)%HRLoopSideNum, &
                                   ElectricChiller(ChillNum)%HRBranchNum, &
                                   ElectricChiller(ChillNum)%HRCompNum, &
                                   InletNodeNumber = ElectricChiller(ChillNum)%HeatRecInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElectricChiller(ChillNum)%CWLoopNum,      &
                                          ElectricChiller(ChillNum)%CWLoopSideNum,  &
                                          ElectricChiller(ChillNum)%HRLoopNum,      &
                                          ElectricChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_Electric, .TRUE.  )
    ENDIF

    IF (ElectricChiller(ChillNum)%CondenserType /= AirCooled  .AND. &
        ElectricChiller(ChillNum)%CondenserType /= EvapCooled .AND. &
        ElectricChiller(ChillNum)%HeatRecActive) THEN
      CALL InterConnectTwoPlantLoopSides( ElectricChiller(ChillNum)%CDLoopNum,      &
                                          ElectricChiller(ChillNum)%CDLoopSideNum,  &
                                          ElectricChiller(ChillNum)%HRLoopNum,      &
                                          ElectricChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_Electric, .FALSE. )
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('InitElectricChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (ElectricChiller(ChillNum)%VariableFlow) Then 
      ! reset flow priority
      PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%LoopSide(ElectricChiller(ChillNum)%CWLoopSideNum)% &
          Branch(ElectricChiller(ChillNum)%CWBranchNum)%Comp(ElectricChiller(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn

      ! check if setpoint on outlet node
      IF (Node(ElectricChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. ElectricChiller(ChillNum)%VariableFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(ElectricChiller(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ElectricChiller(ChillNum)%VariableFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(ElectricChiller(ChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. ElectricChiller(ChillNum)%VariableFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(ElectricChiller(ChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Set Point Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              ElectricChiller(ChillNum)%VariableFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        ElectricChiller(ChillNum)%VariableFlowSetToLoop = .TRUE.
        Node(ElectricChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
    ENDIF
    MyFlag(ChillNum)=.FALSE.
  ENDIF

  IF( MyEnvrnFlag(ChillNum) .AND. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeElectricChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                RoutineName)

    ElectricChiller(ChillNum)%EvapMassFlowRateMax = rho * ElectricChiller(ChillNum)%EvapVolFlowRate
    CALL InitComponentNodes(0.0D0,ElectricChiller(ChillNum)%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         ElectricChiller(ChillNum)%CWLoopNum,               &
                         ElectricChiller(ChillNum)%CWLoopSideNum,           &
                         ElectricChiller(ChillNum)%CWBranchNum,             &
                         ElectricChiller(ChillNum)%CWCompNum)

          !init maximum available condenser flow rate
    IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = ElectricChiller(ChillNum)%TempDesCondIn  !DSU? old behavior, still want?

      rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      ElectricChiller(ChillNum)%CondMassFlowRateMax = rho * ElectricChiller(ChillNum)%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  ElectricChiller(ChillNum)%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         ElectricChiller(ChillNum)%CDLoopNum,               &
                         ElectricChiller(ChillNum)%CDLoopSideNum,           &
                         ElectricChiller(ChillNum)%CDBranchNum,             &
                         ElectricChiller(ChillNum)%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = ElectricChiller(ChillNum)%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,ElectricChiller(ChillNum)%TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0
      Node(CondInletNode)%MassFlowRateMin       = 0.0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0
    END IF

    IF (ElectricChiller(ChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidIndex,&
                                  RoutineName)
      ElectricChiller(ChillNum)%DesignHeatRecMassFlowRate = rho * &
                                         ElectricChiller(ChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, ElectricChiller(ChillNum)%DesignHeatRecMassFlowRate ,  &
                         ElectricChiller(ChillNum)%HeatRecInletNodeNum,        &
                         ElectricChiller(ChillNum)%HeatRecOutletNodeNum,       &
                         ElectricChiller(ChillNum)%HRLoopNum,               &
                         ElectricChiller(ChillNum)%HRLoopSideNum,           &
                         ElectricChiller(ChillNum)%HRBranchNum,             &
                         ElectricChiller(ChillNum)%HRCompNum)
    ENDIF

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  IF (ElectricChiller(ChillNum)%VariableFlow .AND. ElectricChiller(ChillNum)%VariableFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(ElectricChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
  ENDIF

!

  IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
    ! request full then take what can get
    mdot     = ElectricChiller(ChillNum)%EvapMassFlowRateMax
    mdotCond = ElectricChiller(ChillNum)%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              ElectricChiller(ChillNum)%CWLoopNum,     &
                              ElectricChiller(ChillNum)%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%CWBranchNum,   &
                              ElectricChiller(ChillNum)%CWCompNum)
  IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,         &
                                ElectricChiller(ChillNum)%CDLoopNum,     &
                                ElectricChiller(ChillNum)%CDLoopSideNum, &
                                ElectricChiller(ChillNum)%CDBranchNum,   &
                                ElectricChiller(ChillNum)%CDCompNum)
  ENDIF


  ! Initialize heat recovery flow rates at node
  IF (ElectricChiller(ChillNum)%HeatRecActive ) THEN

    InletNode    =  ElectricChiller(ChillNum)%HeatRecInletNodeNum
    OutletNode   =  ElectricChiller(ChillNum)%HeatRecOutletNodeNum
    LoopNum      =  ElectricChiller(ChillNum)%HRLoopNum
    LoopSideNum  =  ElectricChiller(ChillNum)%HRLoopSideNum
    BranchIndex  =  ElectricChiller(ChillNum)%HRBranchNum
    CompIndex    =  ElectricChiller(ChillNum)%HRCompNum

    If (FirstHVACIteration .AND. RunFlag) Then
      mdot = ElectricChiller(ChillNum)%DesignHeatRecMassFlowRate
    ELSEIF (FirstHVACIteration .AND. (.NOT. RunFlag)) Then
      mdot = 0.d0
    ELSEIF ((.NOT. FirstHVACIteration) .AND. RunFlag) THEN
      mdot = ElectricChillerReport(ChillNum)%HeatRecMassFlow
    ELSEIF ((.NOT. FirstHVACIteration) .AND. (.NOT. RunFlag)) THEN
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF

  IF (ElectricChiller(ChillNum)%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF

  RETURN

END SUBROUTINE InitElectricChiller

SUBROUTINE SizeElectricChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, April 2011, allow repeated sizing calls, finish when ready to do so

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Electric Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined

  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho ! local fluid density
  REAL(r64)           ::  Cp  ! local fluid specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  ! init local temporary version in case of partial/mixed autosizing
  tmpNomCap          = ElectricChiller(ChillNum)%NomCap
  tmpEvapVolFlowRate = ElectricChiller(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = ElectricChiller(ChillNum)%CondVolFlowRate

  IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
    IF (ElectricChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%PlantSizNum

  IF (ElectricChiller(ChillNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                  'SizeElectricChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                 InitConvTemp,                      &
                                 PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidIndex, &
                                 'SizeElectricChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate * ElectricChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%NomCap = tmpNomCap

      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Name, &
                                                        'Nominal Capacity [W]', ElectricChiller(ChillNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ElectricChiller(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * ElectricChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize)  ElectricChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              ElectricChiller(ChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (ElectricChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  ElectricChiller(ChillNum)%TempDesCondIn, &
                                  PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  'SizeElectricChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                 ElectricChiller(ChillNum)%TempDesCondIn,                      &
                                 PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                 'SizeElectricChiller')
        tmpCondVolFlowRate = tmpNomCap *   (1.d0 + 1.d0/ElectricChiller(ChillNum)%COP) / &
                                             ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) ElectricChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.0d0
        IF (PlantSizesOkayToFinalize)  ElectricChiller(ChillNum)%CondVolFlowRate = 0.d0
      END IF
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput('Chiller:Electric', ElectricChiller(ChillNum)%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              ElectricChiller(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowContinueError('Autosizing of Electric Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CALL RegisterPlantCompDesignFlow(ElectricChiller(ChillNum)%CondInletNodeNum,tmpCondVolFlowRate)
  ENDIF
  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = ElectricChiller(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:Electric')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ElectricChiller(ChillNum)%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ElectricChiller(ChillNum)%NomCap)
  ENDIF

  RETURN
END SUBROUTINE SizeElectricChiller

! Beginning of Chiller model Subroutines
! *****************************************************************************

SUBROUTINE CalcElectricChillerModel(ChillNum,MyLoad,EquipFlowCtrl,Runflag,FirstIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression chiller using the Electric model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1. BLAST Users Manual
          ! 2. CHILLER User Manual

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, outputfiledebug, CurrentTime
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys, SysTimeElapsed
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : ControlType_SeriesActive, MassFlowTol, PlantLoop, &
                              TypeOf_Chiller_Electric, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillNum        ! chiller number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep !unused1208
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
  !INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow
  INTEGER, INTENT(IN)    :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: CapacityRat           ! intermediate result:  capacity ratio
  REAL(r64), DIMENSION(3)     :: PowerRat              ! intermediate result:  power ratio
  REAL(r64), DIMENSION(3)     :: FullLoadFactor        ! intermediate result:  full load factor
  REAL(r64)              :: MinPartLoadRat        ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat        ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn            ! C - (Electric ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempCondInDesign      ! C - (Electric ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempRiseRat           ! intermediate result:  temperature rise ratio
  REAL(r64)              :: EvapInletTemp         ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp         ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut           ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: TempEvapOutDesign     ! design evaporator outlet temperature, water side
  REAL(r64)              :: ChillerNomCap         ! chiller nominal capacity
  REAL(r64)              :: AvailChillerCap       ! chiller available capacity
  REAL(r64)              :: RatedCOP              ! rated coefficient of performance, from user input
  REAL(r64)              :: FracFullLoadPower     ! fraction of full load power
  REAL(r64)              :: EvapDeltaTemp         ! C - evaporator temperature difference, water side
  REAL(r64)              :: DeltaTemp             ! C - intermediate result: condenser/evaporator temp diff
  REAL(r64)              :: AvailNomCapRat        ! intermediate result: available nominal capacity ratio
  REAL(r64)              :: FullLoadPowerRat      ! intermediate result: full load power ratio
  REAL(r64)              :: PartLoadRat           ! part load ratio for efficiency calculation
  REAL(r64)              :: OperPartLoadRat       ! Actual Operating PLR
  REAL(r64)              :: TempLowLimitEout      ! C - Evaporator low temp. limit cut off
  REAL(r64)              :: EvapMassFlowRateMax ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side
  REAL(r64)              :: FRAC
!  LOGICAL,SAVE           :: PossibleSubCooling=.false.
  INTEGER                :: PlantLoopNum
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  INTEGER                :: BranchNum
  INTEGER                :: CompNum
  REAL(r64),SAVE  :: TimeStepSysLast=0.0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages
  REAL(r64)       :: Cp ! local for fluid specif heat, for evaporator
  REAL(r64)       :: CpCond ! local for fluid specif heat, for condenser

          !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0
  CondMassFlowRate           = 0.0
  Power                      = 0.0
  Energy                     = 0.0
  QCondenser                 = 0.0
  QEvaporator                = 0.0
  CondenserEnergy            = 0.0
  EvaporatorEnergy           = 0.0
  QHeatRecovered             = 0.0
  EvapInletNode  = ElectricChiller(ChillNum)%EvapInletNodeNum
  EvapOutletNode = ElectricChiller(ChillNum)%EvapOutletNodeNum
  CondInletNode  = ElectricChiller(ChillNum)%CondInletNodeNum
  CondOutletNode = ElectricChiller(ChillNum)%CondOutletNodeNum
  FRAC               = 1.0
  LoopNum            = ElectricChiller(ChillNum)%CWLoopNum
  LoopSideNum        = ElectricChiller(ChillNum)%CWLoopSideNum
  BranchNum          = ElectricChiller(ChillNum)%CWBranchNum
  CompNum            = ElectricChiller(ChillNum)%CWCompNum
  EvapInletTemp      = Node(EvapInletNode)%Temp

!   calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(ElectricChiller(ChillNum)%PrintMessage)THEN
        ElectricChiller(ChillNum)%MsgErrorCount = &
                         ElectricChiller(ChillNum)%MsgErrorCount + 1
!       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (ElectricChiller(ChillNum)%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(ElectricChiller(ChillNum)%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(ElectricChiller(ChillNum)%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ElectricChiller(ChillNum)%MsgBuffer1)//' error continues.', &
           ElectricChiller(ChillNum)%ErrCount1,ReportMaxOf=ElectricChiller(ChillNum)%MsgDataLast,  &
           ReportMinOf=ElectricChiller(ChillNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

!   save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

          !If no loop demand or chiller OFF, return
   !If Chiller load is 0 or chiller is not running then leave the subroutine.
  IF(MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
     ! call for zero flow before leaving
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE 
      EvapMassFlowRate           = 0.0
      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%CWLoopNum,     &
                              ElectricChiller(ChillNum)%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%CWBranchNum,   &
                              ElectricChiller(ChillNum)%CWCompNum)
    ENDIF
    IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)% &
            LoopSide(ElectricChiller(ChillNum)%CDLoopSideNum)% &
              Branch(ElectricChiller(ChillNum)%CDBranchNum)%  &
                Comp(ElectricChiller(ChillNum)%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate           = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate           = 0.0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              ElectricChiller(ChillNum)%CDLoopNum, &
                              ElectricChiller(ChillNum)%CDLoopSideNum, &
                              ElectricChiller(ChillNum)%CDBranchNum, &
                              ElectricChiller(ChillNum)%CDCompNum)
      ENDIF
    ENDIF

    IF (ElectricChiller(ChillNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ElectricChiller(ChillNum)%BasinHeaterPowerFTempDiff,&
                                ElectricChiller(ChillNum)%BasinHeaterSchedulePtr,&
                                ElectricChiller(ChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    ElectricChiller(ChillNum)%PrintMessage = .FALSE.
    RETURN
  END IF

  IF (ElectricChiller(ChillNum)%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
      !  Warn user if entering condenser temperature falls below 0C
      IF(Node(CondInletNode)%Temp .LT. 0.0 .and. .not. WarmupFlag) THEN
        ElectricChiller(ChillNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ElectricChiller(ChillNum)%MsgBuffer1 = 'CalcElectricChillerModel - Chiller:Electric "' &
                             //TRIM(ElectricChiller(ChillNum)%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        ElectricChiller(ChillNum)%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ElectricChiller(ChillNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ElectricChiller(ChillNum)%PrintMessage = .FALSE.
      ENDIF
  Else IF (ElectricChiller(ChillNum)%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
    IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
      ElectricChiller(ChillNum)%PrintMessage = .TRUE.
      WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
      ElectricChiller(ChillNum)%MsgBuffer1 = 'CalcElectricChillerModel - Chiller:Electric "' &
                           //TRIM(ElectricChiller(ChillNum)%Name)// &
                           '" - Evap Cooled Condenser Inlet Temperature below 10C'
      ElectricChiller(ChillNum)%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                 ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                 //TRIM(CreateSysTimeIntervalString())
      ElectricChiller(ChillNum)%MsgDataLast = Node(CondInletNode)%Temp
    ELSE
      ElectricChiller(ChillNum)%PrintMessage = .FALSE.
    ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

   ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

        !Set mass flow rates
IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
  CondMassFlowRate = ElectricChiller(ChillNum)%CondMassFlowRateMax
  CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                            ElectricChiller(ChillNum)%CDLoopNum, &
                            ElectricChiller(ChillNum)%CDLoopSideNum, &
                            ElectricChiller(ChillNum)%CDBranchNum, &
                            ElectricChiller(ChillNum)%CDCompNum)
  CALL PullCompInterconnectTrigger(ElectricChiller(ChillNum)%CWLoopNum, &
                                   ElectricChiller(ChillNum)%CWLoopSideNum, &
                                   ElectricChiller(ChillNum)%CWBranchNum, &
                                   ElectricChiller(ChillNum)%CWCompNum, &
                                   ElectricChiller(ChillNum)%CondMassFlowIndex,              &
                                   ElectricChiller(ChillNum)%CDLoopNum, &
                                   ElectricChiller(ChillNum)%CDLoopSideNum,   &
                                   CriteriaType_MassFlowRate, &
                                   CondMassFlowRate)
  IF (CondMassFlowRate < MassFlowTol) RETURN
END IF

      !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  CapacityRat        = ElectricChiller(ChillNum)%CapRatCoef
  PowerRat           = ElectricChiller(ChillNum)%PowerRatCoef
  FullLoadFactor     = ElectricChiller(ChillNum)%FullLoadCoef
  MinPartLoadRat     = ElectricChiller(ChillNum)%MinPartLoadRat
  PartLoadRat        = MinPartLoadRat
  MaxPartLoadRat     = ElectricChiller(ChillNum)%MaxPartLoadRat
  TempCondInDesign   = ElectricChiller(ChillNum)%TempDesCondIn
  TempRiseRat        = ElectricChiller(ChillNum)%TempRiseCoef
  TempEvapOutDesign  = ElectricChiller(ChillNum)%TempDesEvapOut
  ChillerNomCap      = ElectricChiller(ChillNum)%NomCap
  RatedCOP           = ElectricChiller(ChillNum)%COP
  TempEvapOut        = Node(ElectricChiller(ChillNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = ElectricChiller(ChillNum)%TempLowLimitEvapOut
  EvapMassFlowRateMax    = ElectricChiller(ChillNum)%EvapMassFlowRateMax
  PlantLoopNum       = ElectricChiller(ChillNum)%CWLoopNum
  TempCondIn         = Node(ElectricChiller(ChillNum)%CondInletNodeNum)%Temp
  LoopNum            = ElectricChiller(ChillNum)%CWLoopNum
  LoopSideNum        = ElectricChiller(ChillNum)%CWLoopSideNum

  !Calculate chiller performance from this set of performance equations.
  !  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
  DeltaTemp= (TempCondIn   -  TempCondInDesign) / TempRiseRat &
         - (TempEvapOut -  TempEvapOutDesign)

  ! model should have bounds on DeltaTemp and check them (also needs engineering ref content)
  !  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
  AvailNomCapRat =   CapacityRat(1)                   &
                   + CapacityRat(2) * DeltaTemp       &
                   + CapacityRat(3) * DeltaTemp ** 2.d0

  AvailChillerCap = ChillerNomCap*AvailNomCapRat

  ! from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
  FullLoadPowerRat=   PowerRat(1)                         &
                    + PowerRat(2) * AvailNomCapRat      &
                    + PowerRat(3) * AvailNomCapRat ** 2.d0

  !  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
  !         /RCAV,MAXCHFR(I,IPLCTR)))

  !Calculate the PLR. When there is Min PLR and the load is less than Min PLR then the Frac Full load Power
  !is calculated at Min PLR, while all other calculations are based on the actual PLR. So in that case once
  !FracFullLoadPower is calculated the PLR should be recalculated
  IF (AvailChillerCap > 0.0) THEN
    PartLoadRat = MAX(MinPartLoadRat, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
  ENDIF

 ! from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
  FracFullLoadPower = FullLoadFactor(1)                      &
                    + FullLoadFactor(2) * PartLoadRat      &
                    + FullLoadFactor(3) * PartLoadRat ** 2.d0

  !If the PLR is less than Min PLR calculate the actual PLR for calculations. The power will then adjust for
  !the cycling.
  IF (AvailChillerCap > 0.0) THEN
    IF(ABS(MyLoad)/AvailChillerCap .LT. MinPartLoadRat) THEN
     OperPartLoadRat = ABS(MyLoad)/AvailChillerCap
    ELSE
     OperPartLoadRat = PartLoadRat
    END IF
  ELSE
    OperPartLoadRat = 0.0
  ENDIF

  Cp = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(ElectricChiller(ChillNum)%CWLoopNum)%FluidIndex, &
                         'CalcElectricChillerModel')

        ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
        ! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN

       !ElectricChiller(ChillNum)%PossibleSubCooling = .FALSE.
       !PossibleSubCooling = .NOT. PlantLoop(PlantLoopNum)%TempSetPtCtrl
       IF(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                    == CompSetPtBasedSchemeType)THEN
         ElectricChiller(ChillNum)%PossibleSubCooling = .FALSE.
       ELSE
         ElectricChiller(ChillNum)%PossibleSubCooling = .TRUE.
       ENDIF
       QEvaporator = AvailChillerCap * OperPartLoadRat
       IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
        FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
       ELSE
        FRAC = 1.0
       END IF
       Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap/RatedCOP * FRAC

       ! Either set the flow to the Constant value or caluclate the flow for the variable volume
       If(ElectricChiller(ChillNum)%ConstantFlow)Then

          ! Start by assuming max (design) flow
          EvapMassFlowRate = EvapMassFlowRateMax
          ! Use SetComponentFlowRate to decide actual flow
          CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%CWLoopNum,     &
                              ElectricChiller(ChillNum)%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%CWBranchNum,   &
                              ElectricChiller(ChillNum)%CWCompNum)
          ! Evaluate delta temp based on actual flow rate
          IF (EvapMassFlowRate /= 0.0D0) THEN
            EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
          ELSE
            EvapDeltaTemp = 0.0D0
          ENDIF
          ! Evaluate outlet temp based on delta
          EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

       Else IF(ElectricChiller(ChillNum)%VariableFlow)Then

          ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
          EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint

          IF (EvapDeltaTemp /= 0.0d0) THEN

            ! Calculate desired flow to request based on load
            EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
            !Check to see if the Maximum is exceeded, if so set to maximum
            IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTol) &
                     ElectricChiller(ChillNum)%PossibleSubCooling = .TRUE.
            EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%CWLoopNum,     &
                              ElectricChiller(ChillNum)%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%CWBranchNum,   &
                              ElectricChiller(ChillNum)%CWCompNum)

            EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint

          ELSE

            ! Try to request zero flow
            EvapMassFlowRate=0.0
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%CWLoopNum,     &
                              ElectricChiller(ChillNum)%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%CWBranchNum,   &
                              ElectricChiller(ChillNum)%CWCompNum)
            ! No deltaT since component is not running
            EvapOutletTemp = Node(EvapInletNode)%Temp

          END IF

       End If  !End of Constant Variable Flow If Block

  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElectricChiller(ChillNum)%CWLoopNum,     &
                              ElectricChiller(ChillNum)%CWLoopSideNum, &
                              ElectricChiller(ChillNum)%CWBranchNum,   &
                              ElectricChiller(ChillNum)%CWCompNum)

!       Some other component set the flow to 0. No reason to continue with calculations.
    IF(EvapMassFlowRate == 0.0d0)THEN
      MyLoad = 0.0d0
      IF (ElectricChiller(ChillNum)%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(ElectricChiller(ChillNum)%BasinHeaterPowerFTempDiff,&
                            ElectricChiller(ChillNum)%BasinHeaterSchedulePtr,&
                            ElectricChiller(ChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      ENDIF
      ElectricChiller(ChillNum)%PrintMessage = .FALSE.
      RETURN
    END IF
    !Flow resolver might have given less flow or control scheme have provided more load, which may
    !result in subcooling.
    IF(ElectricChiller(ChillNum)%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE  !No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
      
      IF ((ElectricChiller(ChillNum)%VariableFlow) .OR. &
          (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
               == CompSetPtBasedSchemeType)          .OR. &
          (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
        TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
      ELSE
        TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint  
    END IF
    !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTemptol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTemptol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF

    ! If load exceeds the distributed load set to the distributed load
    IF(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTol) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      ELSE
        QEvaporator = 0.0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End IF

    ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > (AvailChillerCap * MaxPartLoadRat))Then
      If(EvapMassFlowRate > MassFlowTol) THEN
        QEvaporator = AvailChillerCap * OperPartLoadRat
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
      FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
      FRAC = 1.0d0
    END IF

    ! set the module level variable used for reporting FRAC
    ChillerCyclingRatio = FRAC

    ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap /RatedCOP * FRAC

    IF(EvapMassFlowRate == 0.0) THEN
     QEvaporator = 0.0
     EvapOutletTemp = Node(EvapInletNode)%Temp
     Power = 0.0
     ElectricChiller(ChillNum)%PrintMessage = .FALSE.
    END IF
    IF(QEvaporator == 0.0d0 .AND. ElectricChiller(ChillNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ElectricChiller(ChillNum)%BasinHeaterPowerFTempDiff,&
                                  ElectricChiller(ChillNum)%BasinHeaterSchedulePtr,&
                                  ElectricChiller(ChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    END IF
  END IF  !This is the end of the FlowLock Block

  !QCondenser is calculated the same for each type, but the power consumption should be different
  !  depending on the performance coefficients used for the chiller model.
  QCondenser = Power + QEvaporator

  IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
    IF (CondMassFlowRate > MassFlowTol) THEN
      ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
      If(ElectricChiller(ChillNum)%HeatRecActive) Call ChillerHeatRecovery(ChillNum,QCondenser, &
                                                               CondMassFlowRate,CondInletTemp,QHeatRecovered)
      CpCond = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                         CondInletTemp,                      &
                                         PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                         'CalcElectricChillerModel')
       CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
    ELSE
      CALL ShowSevereError('CalcElectricChillerModel: Condenser flow = 0, for ElectricChiller='//  &
                            TRIM(ElectricChiller(ChillNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')

    END IF
  ELSE !Air Cooled or Evap Cooled

    ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
    If(ElectricChiller(ChillNum)%HeatRecActive) Call ChillerHeatRecovery(ChillNum,QCondenser, &
                                                 CondMassFlowRate,CondInletTemp,QHeatRecovered)
    !don't care about outlet temp for Air-Cooled or Evap Cooled and there is no CondMassFlowRate and would divide by zero
    CondOutletTemp = CondInletTemp
  END IF


    !Calculate Energy
  CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
  Energy           = Power*TimeStepSys*SecInHour
  EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem

    IF (ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcElectricChillerModel: Condenser loop inlet temperatures over 70.0 C for ElectricChiller='//  &
                            TRIM(ElectricChiller(ChillNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    IF(.NOT.WarmupFlag)THEN
      If (AvailNomCapRat < 0.0 ) then     ! apparently the real reason energy goes negative
        CALL ShowSevereError('CalcElectricChillerModel: Capacity ratio below zero for ElectricChiller='//  &
                              TRIM(ElectricChiller(ChillNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Check input for Capacity Ratio Curve')
        CALL showContinueError('Condenser inlet temperature: '//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )
        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0
    Energy = 0.0
  ENDIF
  RETURN
END SUBROUTINE CalcElectricChillerModel


SUBROUTINE ChillerHeatRecovery(ChillNum,QCond,CondMassFlow,CondInletTemp,QHeatRec)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Richard Liesen
            !       DATE WRITTEN:    January 2004

            ! PURPOSE OF THIS SUBROUTINE:
            ! Calculate the heat recovered from the chiller condenser


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:
USE Psychrometrics, ONLY: PsyCpAirFnWTdb
USE FluidProperties, ONLY: GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)     :: ChillNum      ! number of the current electric chiller being simulated
  REAL(r64),INTENT(INOut)       :: QCond         ! current condenser load
  REAL(r64),INTENT(Out)         :: QHeatRec      ! amount of heat recovered
  REAL(r64),INTENT(IN)          :: CondMassFlow  ! current condenser Mass Flow
  REAL(r64),INTENT(IN)          :: CondInletTemp ! current condenser Inlet Temp

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CondInletNode     ! condenser inlet node number, water side
  INTEGER :: CondOutletNode    ! condenser outlet node number, water side
  INTEGER :: HeatRecInNode
  INTEGER :: HeatRecOutNode
  REAL(r64)    :: QTotal
  REAL(r64)    :: QCondTmp
  REAL(r64)    :: HeatRecInletTemp
  REAL(r64)    :: HeatRecMassFlowRate
  REAL(r64)    :: FracHeatRec
  REAL(r64)    :: TAvgIn
  REAL(r64)    :: TAvgOut
  REAL(r64)    :: CpHeatRec
  REAL(r64)    :: CpCond


  ! Begin routine
  HeatRecInNode  = ElectricChiller(ChillNum)%HeatRecInletNodeNum
  HeatRecOutNode = ElectricChiller(ChillNum)%HeatRecOutletNodeNum
  CondInletNode  = ElectricChiller(ChillNum)%CondInletNodeNum
  CondOutletNode = ElectricChiller(ChillNum)%CondOutletNodeNum
  HeatRecInletTemp  = Node(HeatRecInNode)%Temp
  HeatRecMassFlowRate = Node(HeatRecInNode)%MassFlowRate

  CpHeatRec = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                   HeatRecInletTemp,                      &
                                   PlantLoop(ElectricChiller(ChillNum)%HRLoopNum)%FluidIndex, &
                                   'ChillerHeatRecovery')

  IF(ElectricChiller(ChillNum)%CondenserType == WaterCooled) THEN
     CpCond = GetSpecificHeatGlycol(PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                   CondInletTemp,                      &
                                   PlantLoop(ElectricChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'ChillerHeatRecovery')
  ELSE
    CpCond = PsyCpAirFnWTdb(Node(CondInletNode)%HumRat,CondInletTemp,'ElecChillerHeatRecovery')
  END IF

  ! Before we modify the QCondenser, the total or original value is transferred to QTot
  QTotal = QCond

  TAvgIn = (HeatRecMassFlowRate*CpHeatRec*HeatRecInletTemp + CondMassFlow*CpCond*CondInletTemp)/  &
             (HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond)

  TAvgOut = QTotal/(HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond) + TAvgIn

  QCondTmp = CondMassFlow*CpCond*(TAvgOut-CondInletTemp)

  If(QCondTmp <= 0.0)Then
    FracHeatRec = 1.0d0
  Else
    FracHeatRec = (HeatRecMassFlowRate*CpHeatRec*(TAvgOut-HeatRecInletTemp))/QCondTmp
  End If

  If(FracHeatRec <= 0.0d0) FracHeatRec = 0.0d0
  If(FracHeatRec > 1.0d0) FracHeatRec = 1.0d0

  QCond = QTotal*(1.0d0 - FracHeatRec)

  If(FracHeatRec == 0.0) Then
    QHeatRec = 0.0
  Else
    QHeatRec = QTotal*FracHeatRec
  End If

  ! Calculate a new Heat Recovery Coil Outlet Temp
  IF (HeatRecMassFlowRate > 0.0) THEN
    HeatRecOutletTemp = QHeatRec/(HeatRecMassFlowRate*CpHeatRec) + HeatRecInletTemp
  Else
    HeatRecOutletTemp = HeatRecInletTemp
  End If


 RETURN
END SUBROUTINE ChillerHeatRecovery

! End of Electric Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the Electric Chiller Module
! *****************************************************************************

SUBROUTINE UpdateElectricChillerRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE PlantUtilities,  ONLY : SetComponentFlowRate, SafeCopyPlantNode


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)          :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! chiller number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side
  INTEGER                :: HeatRecInNode
  INTEGER                :: HeatRecOutNode
  REAL(r64)              :: ReportingConstant     ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode  = ElectricChiller(Num)%EvapInletNodeNum
  EvapOutletNode = ElectricChiller(Num)%EvapOutletNodeNum
  CondInletNode  = ElectricChiller(Num)%CondInletNodeNum
  CondOutletNode = ElectricChiller(Num)%CondOutletNodeNum
  HeatRecInNode  = ElectricChiller(Num)%HeatRecInletNodeNum
  HeatRecOutNode = ElectricChiller(Num)%HeatRecOutletNodeNum

  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running so pass inlet states to outlet states
          !set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp


    ElectricChillerReport(Num)%Power            = 0.0
    ElectricChillerReport(Num)%QEvap            = 0.0
    ElectricChillerReport(Num)%QCond            = 0.0
    ElectricChillerReport(Num)%Energy           = 0.0
    ElectricChillerReport(Num)%EvapEnergy       = 0.0
    ElectricChillerReport(Num)%CondEnergy       = 0.0
    ElectricChillerReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    ElectricChillerReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    ElectricChillerReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    ElectricChillerReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    ElectricChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    ElectricChillerReport(Num)%Condmdot         = CondMassFlowRate
    ElectricChillerReport(Num)%ActualCOP        = 0.0
    IF (ElectricChiller(Num)%CondenserType == EvapCooled) THEN
      ElectricChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      ElectricChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

    If(ElectricChiller(Num)%HeatRecActive) Then

      CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)

      ElectricChillerReport(Num)%QHeatRecovery = 0.0
      ElectricChillerReport(Num)%EnergyHeatRecovery = 0.0
      ElectricChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
      ElectricChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
      ElectricChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate
    End If

  ELSE !Chiller is running, so pass calculated values
          !set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp
          !set node flow rates;  for these load based models
          !assume that the sufficient evaporator flow rate available
    ElectricChillerReport(Num)%Power            = Power
    ElectricChillerReport(Num)%QEvap            = QEvaporator
    ElectricChillerReport(Num)%QCond            = QCondenser
    ElectricChillerReport(Num)%Energy           = Energy
    ElectricChillerReport(Num)%EvapEnergy       = EvaporatorEnergy
    ElectricChillerReport(Num)%CondEnergy       = CondenserEnergy
    ElectricChillerReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    ElectricChillerReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    ElectricChillerReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    ElectricChillerReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    ElectricChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    ElectricChillerReport(Num)%Condmdot         = CondMassFlowRate
    IF (ElectricChiller(Num)%CondenserType == EvapCooled) THEN
      ElectricChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      ElectricChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
    IF (Power .NE. 0.0) THEN
       ElectricChillerReport(Num)%ActualCOP     = QEvaporator/Power
    ELSE
       ElectricChillerReport(Num)%ActualCOP     = 0.0
    END IF

    If(ElectricChiller(Num)%HeatRecActive) Then

       CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)
       ElectricChillerReport(Num)%QHeatRecovery = QHeatRecovered
       ElectricChillerReport(Num)%EnergyHeatRecovery = QHeatRecovered*TimeStepSys*SecInHour
       Node(HeatRecOutNode)%Temp = HeatRecOutletTemp
       ElectricChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
       ElectricChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
       ElectricChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate
    End If

  END IF
RETURN
END SUBROUTINE UpdateElectricChillerRecords




! End of Record Keeping subroutines for the Electric Chiller Module
! *****************************************************************************


END MODULE ChillerElectric


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


MODULE ChillerEngineDriven  !EngineDriven ChillerModule

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the EngineDriven engine driven vapor
          ! compression Chillers.

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the EngineDriven chiller
          ! is available to meet a loop cooling demand, it calls SimEngineDriven
          ! Chiller which in turn calls the EngineDriven chiller model.
          ! The EngineDriven chiller model is based on a polynomial fit of chiller
          ! performance data.

          ! REFERENCES:
          ! 1. BLAST Users Manual


          ! OTHER NOTES:
          ! The CHILLER program from the EngineDriven family of software can be used
          ! to generate the coefficients for the model.

          ! The Electric, EngineDriven, and Gas Turbine chiller models are very similar.
          ! They only differ in the unit that drives the chiller.  Therefore,
          ! the Blast curve fit portion for each chiller is the same and is in the
          ! general routines module under CalcChillerPower.


          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals ,    ONLY : MaxNameLength, NumOfTimeStepInHour, InitConvTemp, WarmupFlag
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE DataPlant,       ONLY: DeltaTemptol, TypeOf_Chiller_EngineDriven
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE
PRIVATE

          !MODULE PARAMETER DEFINITIONS:
! Parameters for use in Chillers
INTEGER, PARAMETER :: AirCooled = 1
INTEGER, PARAMETER :: WaterCooled = 2
INTEGER, PARAMETER :: EvapCooled = 3

          ! MODULE VARIABLE DECLARATIONS:
INTEGER                  :: NumEngineDrivenChillers =0   ! number of EngineDriven chillers specified in input
REAL(r64)                :: CondMassFlowRate        =0.0 ! Kg/s - condenser mass flow rate, water side
REAL(r64)                :: EvapMassFlowRate        =0.0 ! Kg/s - evaporator mass flow rate, water side
REAL(r64)                :: CondOutletTemp          =0.0 ! C - condenser outlet temperature, water side
REAL(r64)                :: EvapOutletTemp          =0.0 ! C - evaporator outlet temperature, water side
REAL(r64)                :: Power                   =0.0 ! W - rate of chiller energy use
REAL(r64)                :: QEvaporator             =0.0 ! W - rate of heat transfer to the evaporator coil
REAL(r64)                :: QCondenser              =0.0 ! W - rate of heat transfer to the condenser coil
REAL(r64)                :: Energy                  =0.0 ! W - rate of chiller energy use
REAL(r64)                :: EvaporatorEnergy        =0.0 ! W - rate of heat transfer to the evaporator coil
REAL(r64)                :: CondenserEnergy         =0.0 ! W - rate of heat transfer to the condenser coil
REAL(r64)                :: HeatRecInletTemp        =0.0 ! Inlet Temperature of the heat recovery fluid
REAL(r64)                :: HeatRecOutletTemp       =0.0 ! Outlet Temperature of the heat recovery fluid
REAL(r64)                :: HeatRecMdotActual       =0.0 ! reporting: Heat Recovery Loop Mass flow rate
REAL(r64)                :: HeatRecMdotDesign       =0.0
REAL(r64)                :: QTotalHeatRecovered     =0.0 ! total heat recovered (W)
REAL(r64)                :: QJacketRecovered        =0.0 ! heat recovered from jacket (W)
REAL(r64)                :: QLubeOilRecovered       =0.0 ! heat recovered from lube (W)
REAL(r64)                :: QExhaustRecovered       =0.0 ! exhaust gas heat recovered (W)
REAL(r64)                :: FuelEnergyUseRate       =0.0 ! Fuel Energy used (W)
REAL(r64)                :: TotalHeatEnergyRec      =0.0 ! total heat recovered (J)
REAL(r64)                :: JacketEnergyRec         =0.0 ! heat recovered from jacket (J)
REAL(r64)                :: LubeOilEnergyRec        =0.0 ! heat recovered from lube (J)
REAL(r64)                :: ExhaustEnergyRec        =0.0 ! exhaust gas heat recovered (J)
REAL(r64)                :: FuelEnergy              =0.0 ! Fuel Energy used (J)
REAL(r64)                :: FuelMdot                =0.0 ! Fuel Amount used (Kg/s)
REAL(r64)                :: ExhaustStackTemp        =0.0 ! Exhaust Stack Temperature (C)
REAL(r64)                :: ChillerCyclingRatio     =0.0 ! Cycling ratio for chiller when load is below MinPLR
REAL(r64)                :: BasinHeaterPower        =0.0 ! Basin heater power (W)

TYPE EngineDrivenChillerSpecs
  CHARACTER(len=MaxNameLength) :: Name      =' '  ! user identifier
  INTEGER           :: CondenserType        = 0   ! Type of Condenser - Air or Water Cooled
  CHARACTER(len=MaxNameLength) :: FuelType  =' '  ! Type of Fuel - DIESEL, GASOLINE, GAS
  REAL(r64)         :: NomCap               =0.0 ! W - design nominal capacity of chiller
  REAL(r64)         :: COP                  =0.0 ! coefficient of performance
  LOGICAL           :: ConstantFlow         =.false. ! True if this is a Constant Flow Chiller
  LOGICAL           :: VariableFlow         =.false.  ! True if this is a Variable Flow Chiller
  LOGICAL           :: VariableFlowSetToLoop= .FALSE. ! True if the setpoint is missing at the outlet node
  LOGICAL           :: VariableFlowErrDone  = .FALSE.  ! true if setpoint warning issued
  REAL(r64)         :: EvapVolFlowRate      =0.0 ! m**3/s - design nominal water volumetric flow rate through the evaporator
  REAL(r64)         :: EvapMassFlowRateMax  =0.0 ! kg/s - design water mass flow rate through evaporator
  REAL(r64)         :: CondVolFlowRate      =0.0 ! m**3/s - design nominal water volumetric flow rate through the condenser
  REAL(r64)         :: CondMassFlowRateMax  =0.0 ! kg/s - design water mass flow rate through condenser
  INTEGER           :: EvapInletNodeNum     =0   ! Node number on the inlet side of the plant
  INTEGER           :: EvapOutletNodeNum    =0   ! Node number on the outlet side of the plant
  INTEGER           :: CondInletNodeNum     =0   ! Node number on the inlet side of the condenser
  INTEGER           :: CondOutletNodeNum    =0   ! Node number on the outlet side of the condenser

  REAL(r64)         :: MinPartLoadRat       =0.0 ! (EngineDriven MIN) min allowed operating frac full load
  REAL(r64)         :: MaxPartLoadRat       =0.0 ! (EngineDriven MAX) max allowed operating frac full load
  REAL(r64)         :: OptPartLoadRat       =0.0 ! (EngineDriven BEST) optimal operating frac full load
  REAL(r64)         :: TempDesCondIn        =0.0 ! C - (EngineDriven ADJTC(1)The design secondary loop fluid
                                                 ! temperature at the chiller condenser side inlet
  REAL(r64)         :: TempRiseCoef         =0.0 ! (EngineDriven ADJTC(2)) correction factor for off ChillDesign oper.
  REAL(r64)         :: TempDesEvapOut       =0.0 ! C - (EngineDriven ADJTC(3)The design primary loop fluid
                                                 ! temperature at the chiller evaporator side outlet
  REAL(r64),DIMENSION(3) :: CapRatCoef           =0.0 ! (EngineDriven RCAVC() ) coeff of cap ratio poly fit
  REAL(r64),DIMENSION(3) :: PowerRatCoef         =0.0 ! (EngineDriven ADJEC() ) coeff of power rat poly fit
  REAL(r64),DIMENSION(3) :: FullLoadCoef         =0.0 ! (EngineDriven RPWRC() ) coeff of full load poly. fit
  REAL(r64)         :: TempLowLimitEvapOut  =0.0 ! C - low temperature shut off

  INTEGER           :: ClngLoadtoFuelCurve   =0   !Coeff of Shaft Power to Fuel Energy Input Coeff Poly Fit
  INTEGER           :: RecJacHeattoFuelCurve =0   !Curve Index for Ratio of Recoverable Jacket Heat to
  INTEGER           :: RecLubeHeattoFuelCurve=0   !Curve Index for Ratio of Recoverable Lube Oil Heat to
  INTEGER           :: TotExhausttoFuelCurve =0   !Curve Index for Total Exhaust heat Input to Fuel Energy Input Coeffs Poly Fit
  REAL(r64)         :: ExhaustTemp           =0.0 !(TEXDC) Exhaust Gas Temp to Fuel Energy Input
  INTEGER           :: ExhaustTempCurve      =0   !Curve Index for Exhaust Gas Temp to Fuel Energy Input Coeffs Poly Fit
  REAL(r64)         :: UA                    =0.0 !(UACDC) exhaust gas Heat Exchanger UA to Capacity
  REAL(r64),DIMENSION(2) :: UACoef                =0.0   !Heat Exchanger UA Coeffs Poly Fit
  REAL(r64)         :: MaxExhaustperPowerOutput =0.0 !MAX EXHAUST FLOW PER W DSL POWER OUTPUT COEFF
  REAL(r64)         :: DesignMinExitGasTemp     =0.0 !Steam Saturation Temperature
  REAL(r64)         :: FuelHeatingValue         =0.0 ! Heating Value of Fuel in kJ/kg
  REAL(r64)         :: DesignHeatRecVolFlowRate =0.0 ! m3/s, Design Water mass flow rate through heat recovery loop
  REAL(r64)         :: DesignHeatRecMassFlowRate=0.0 ! kg/s, Design Water mass flow rate through heat recovery loop
  REAL(r64)         :: SizFac                   =0.0 ! sizing factor
  REAL(r64)         :: BasinHeaterPowerFTempDiff = 0.0 ! Basin heater capacity per degree C below set point (W/C)
  REAL(r64)         :: BasinHeaterSetPointTemp   = 0.0 ! Set point temperature for basin heater operation (C)
  LOGICAL           :: HeatRecActive        =.false. ! True entered Heat Rec Vol Flow Rate >0
  INTEGER           :: HeatRecInletNodeNum  =0  ! Node number on the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum =0  ! Node number on the heat recovery outlet side of the condenser
  REAL(r64)         :: HeatRecMaxTemp       =0.0 !Max Temp that can be produced in heat recovery
  INTEGER           :: ErrCount1            =0  ! error counter for recurring messages
  INTEGER           :: ErrCount2            =0  ! error counter for recurring messages
  INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
  INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
  INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
  INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
  INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
  INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
  INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
  INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
  INTEGER           :: BasinHeaterSchedulePtr  = 0   ! Pointer to basin heater schedule
  INTEGER           :: CondMassFlowIndex = 0
  CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
  CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
  REAL(r64)         :: MsgDataLast   = 0.0 ! value of data when warning occurred (passed to Recurring Warn)
  LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
  INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
  LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE EngineDrivenChillerSpecs


TYPE ReportVars
  REAL(r64)    :: Power                  = 0.0 ! reporting: power
  REAL(r64)    :: QEvap                  = 0.0 ! reporting: evaporator heat transfer
  REAL(r64)    :: QCond                  = 0.0 ! reporting: condensor heat transfer
  REAL(r64)    :: Energy                 = 0.0 ! reporting: power
  REAL(r64)    :: EvapEnergy             = 0.0 ! reporting: evaporator heat transfer
  REAL(r64)    :: CondEnergy             = 0.0 ! reporting: condensor heat transfer
  REAL(r64)    :: CondInletTemp          = 0.0 ! reporting: condenser inlet temperature
  REAL(r64)    :: EvapInletTemp          = 0.0 ! reporting: evaporator inlet temperature
  REAL(r64)    :: CondOutletTemp         = 0.0 ! reporting: condenser outlet temperature
  REAL(r64)    :: EvapOutletTemp         = 0.0 ! reporting: evaporator outlet temperature
  REAL(r64)    :: Evapmdot               = 0.0 ! reporting: evaporator mass flow rate
  REAL(r64)    :: Condmdot               = 0.0 ! reporting: condenser mass flow rate
  REAL(r64)    :: QJacketRecovered       = 0.0 ! reporting: Heat Recovered from Jacket (W)
  REAL(r64)    :: QLubeOilRecovered      = 0.0 ! reporting: Heat Recovered from Lubricant (W)
  REAL(r64)    :: QExhaustRecovered      = 0.0 ! reporting: exhaust gas heat recovered (W)
  REAL(r64)    :: QTotalHeatRecovered    = 0.0 ! reporting: Total Heat Recovered (W)
  REAL(r64)    :: TotalHeatEnergyRec     = 0.0 ! reporting: total heat recovered (J)
  REAL(r64)    :: JacketEnergyRec        = 0.0 ! reporting: heat recovered from jacket (J)
  REAL(r64)    :: LubeOilEnergyRec       = 0.0 ! reporting: heat recovered from lube (J)
  REAL(r64)    :: ExhaustEnergyRec       = 0.0 ! reporting: exhaust gas heat recovered (J)
  REAL(r64)    :: FuelEnergy             = 0.0 ! reporting: Fuel Energy used (J)
  REAL(r64)    :: FuelEnergyUseRate      = 0.0 ! reporting: Fuel Energy used (W)
  REAL(r64)    :: FuelMdot               = 0.0 ! reporting: Fuel used (Kg/s)
  REAL(r64)    :: ExhaustStackTemp       = 0.0 ! reporting: Exhaust Stack Temperature (C)
  REAL(r64)    :: HeatRecInletTemp       = 0.0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
  REAL(r64)    :: HeatRecOutletTemp      = 0.0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
  REAL(r64)    :: HeatRecMdot            = 0.0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)
  REAL(r64)    :: FuelCOP                = 0.0 ! reporting: Fuel COP [delivered cooling rate/fuel energy input rate] (W/W)
  REAL(r64)    :: BasinHeaterPower       = 0.0 ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0 ! Basin heater energy consumption (J)
END TYPE ReportVars



TYPE (EngineDrivenChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: EngineDrivenChiller  !dimension to number of machines

TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::EngineDrivenChillerReport
LOGICAL     :: GetInput = .TRUE.! then TRUE, calls subroutine to read input file.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PRIVATE    CalcEngineDrivenChillerModel
PRIVATE    CalcEngineChillerHeatRec
PRIVATE    GetEngineDrivenChillerInput
PRIVATE    InitEngineDrivenChiller
PRIVATE    SizeEngineDrivenChiller
PRIVATE    UpdateEngineDrivenChiller
PUBLIC     SimEngineDrivenChiller
!PUBLIC     SimEngineDrivenChillerHeatRecovery


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of EngineDriven Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimEngineDrivenChiller(LoopNum, LoopSide, ChillerType,ChillerName, EquipFlowCtrl, CompIndex,RunFlag,FirstHVACIteration, &
                                  InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the EngineDriven chiller model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : FindItemInList
  USE PlantUtilities, ONLY : UpdateChillerComponentCondenserSide , UpdateComponentHeatRecoverySide

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: LoopNum  ! Flow control mode for the equipment
  INTEGER, INTENT(IN)      :: LoopSide            ! chiller number pointer
  CHARACTER(len=*), INTENT(IN) :: ChillerType      ! type of chiller !unused1208
  CHARACTER(len=*), INTENT(IN) :: ChillerName      ! user specified name of chiller
  INTEGER, INTENT(IN)          :: EquipFlowCtrl    ! Flow control mode for the equipment
  LOGICAL, INTENT(IN)          :: RunFlag          ! simulate chiller when TRUE
  LOGICAL, INTENT(IN)          :: FirstHVACIteration   ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip    ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad           ! loop demand component will meet
  INTEGER, INTENT(INOUT)       :: CompIndex        ! chiller number pointer
  REAL(r64)                    :: MinCap           ! W - minimum operating capacity of chiller
  REAL(r64)                    :: MaxCap           ! W - maximum operating capacity of chiller
  REAL(r64)                    :: OptCap           ! W - optimal operating capacity of chiller
  LOGICAL, INTENT(IN)          :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(OUT)       :: SizingFactor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ChillNum            ! chiller number pointer


          !Get chiller data from input file
  IF (GetInput) THEN
    CALL GetEngineDrivenChillerInput
    GetInput = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(ChillerName,EngineDrivenChiller%Name,NumEngineDrivenChillers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimEngineDrivenChiller: Specified Chiller not one of Valid EngineDriven Chillers='//TRIM(ChillerName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumEngineDrivenChillers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimEngineDrivenChiller:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumEngineDrivenChillers))//  &
                          ', Entered Unit name='//TRIM(ChillerName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (ChillerName /= EngineDrivenChiller(ChillNum)%Name) THEN
        CALL ShowFatalError('SimEngineDrivenChiller: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                            TRIM(EngineDrivenChiller(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    CALL InitEngineDrivenChiller(ChillNum, RunFlag, MyLoad, FirstHVACIteration)
    CALL SizeEngineDrivenChiller(ChillNum)
    IF (LoopNum == EngineDrivenChiller(ChillNum)%CWLoopNum) THEN
      MinCap = EngineDrivenChiller(ChillNum)%NomCap*EngineDrivenChiller(ChillNum)%MinPartLoadRat
      MaxCap = EngineDrivenChiller(ChillNum)%NomCap*EngineDrivenChiller(ChillNum)%MaxPartLoadRat
      OptCap = EngineDrivenChiller(ChillNum)%NomCap*EngineDrivenChiller(ChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = EngineDrivenChiller(ChillNum)%SizFac
    END IF
    RETURN
  END IF

  ! calculate model depending on where called from
  IF (LoopNum == EngineDrivenChiller(ChillNum)%CWLoopNum) THEN ! chilled water loop
    CALL InitEngineDrivenChiller(ChillNum, RunFlag, MyLoad, FirstHVACIteration)
    CALL CalcEngineDrivenChillerModel(ChillNum,MyLoad,Runflag,FirstHVACIteration,EquipFlowCtrl)
    CALL UpdateEngineDrivenChiller(MyLoad,RunFlag,ChillNum)
  ELSEIF (LoopNum == EngineDrivenChiller(ChillNum)%CDLoopNum) THEN ! condenser loop
    CALL UpdateChillerComponentCondenserSide(EngineDrivenChiller(ChillNum)%CDLoopNum, &
                                     EngineDrivenChiller(ChillNum)%CDLoopSideNum,     &
                                     TypeOf_Chiller_EngineDriven,                     &
                                     EngineDrivenChiller(ChillNum)%CondInletNodeNum,  &
                                     EngineDrivenChiller(ChillNum)%CondOutletNodeNum, &
                                     EngineDrivenChillerReport(ChillNum)%QCond,             &
                                     EngineDrivenChillerReport(ChillNum)%CondInletTemp,     &
                                     EngineDrivenChillerReport(ChillNum)%CondOutletTemp,    &
                                     EngineDrivenChillerReport(ChillNum)%Condmdot,          &
                                     FirstHVACIteration)
  ELSEIF (LoopNum == EngineDrivenChiller(ChillNum)%HRLoopNum) THEN  ! heat recovery loop
    CALL UpdateComponentHeatRecoverySide(EngineDrivenChiller(ChillNum)%HRLoopNum,             &
                                    EngineDrivenChiller(ChillNum)%HRLoopSideNum,              &
                                    TypeOf_Chiller_EngineDriven,                              &
                                    EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum,        &
                                    EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum,       &
                                    EngineDrivenChillerReport(ChillNum)%QTotalHeatRecovered,  &
                                    EngineDrivenChillerReport(ChillNum)%HeatRecInletTemp,     &
                                    EngineDrivenChillerReport(ChillNum)%HeatRecOutletTemp,    &
                                    EngineDrivenChillerReport(ChillNum)%HeatRecMdot ,         &
                                    FirstHVACIteration)
  ENDIF

RETURN
END SUBROUTINE SimEngineDrivenChiller

! End EngineDriven Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of EngineDriven Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetEngineDrivenChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000
            !
            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the EngineDriven Chiller model.


            ! METHODOLOGY EMPLOYED:

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE CurveManager,   ONLY : GetCurveIndex
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General,           ONLY: RoundSigDigits
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,    ONLY: GetScheduleIndex

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: ChillerNum !chiller counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay

         !FLOW
  cCurrentModuleObject = 'Chiller:EngineDriven'
  NumEngineDrivenChillers = GetNumObjectsFound(TRIM(cCurrentModuleObject))

  IF (NumEngineDrivenChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF
            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(EngineDrivenChiller))RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (EngineDrivenChiller(NumEngineDrivenChillers))

  ALLOCATE (EngineDrivenChillerReport(NumEngineDrivenChillers))
  ALLOCATE(CheckEquipName(NumEngineDrivenChillers))
  CheckEquipName=.true.

         !LOAD ARRAYS WITH EngineDriven CURVE FIT CHILLER DATA
  DO ChillerNum = 1 , NumEngineDrivenChillers
    CALL GetObjectItem(TRIM(cCurrentModuleObject),ChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),EngineDrivenChiller%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    EngineDrivenChiller(ChillerNum)%Name                = cAlphaArgs(1)

    EngineDrivenChiller(ChillerNum)%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    EngineDrivenChiller(ChillerNum)%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    IF (cAlphaArgs(2) == 'AIRCOOLED' ) THEN
      EngineDrivenChiller(ChillerNum)%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(2) == 'WATERCOOLED' ) THEN
      EngineDrivenChiller(ChillerNum)%CondenserType       = WaterCooled
    ELSEIF (cAlphaArgs(2) == 'EVAPORATIVELYCOOLED' ) THEN
      EngineDrivenChiller(ChillerNum)%CondenserType       = EvapCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF


    EngineDrivenChiller(ChillerNum)%EvapInletNodeNum    =   &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    EngineDrivenChiller(ChillerNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    IF (EngineDrivenChiller(ChillerNum)%CondenserType == AirCooled .or.  &
        EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
       !If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      !  since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(5))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(6) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(6) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(6) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      EngineDrivenChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(EngineDrivenChiller(ChillerNum)%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(5)))
      ENDIF

      EngineDrivenChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      !CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (Air) Nodes')
    ELSEIF (EngineDrivenChiller(ChillerNum)%CondenserType == WaterCooled) THEN
      EngineDrivenChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      EngineDrivenChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser Water Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(6) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      EngineDrivenChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      EngineDrivenChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
     CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(6) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF


    EngineDrivenChiller(ChillerNum)%MinPartLoadRat      = rNumericArgs(3)
    EngineDrivenChiller(ChillerNum)%MaxPartLoadRat      = rNumericArgs(4)
    EngineDrivenChiller(ChillerNum)%OptPartLoadRat      = rNumericArgs(5)
    EngineDrivenChiller(ChillerNum)%TempDesCondIn       = rNumericArgs(6)
    EngineDrivenChiller(ChillerNum)%TempRiseCoef        = rNumericArgs(7)
    EngineDrivenChiller(ChillerNum)%TempDesEvapOut      = rNumericArgs(8)
    EngineDrivenChiller(ChillerNum)%EvapVolFlowRate     = rNumericArgs(9)
    EngineDrivenChiller(ChillerNum)%CondVolFlowRate     = rNumericArgs(10)
    EngineDrivenChiller(ChillerNum)%CapRatCoef(1)       = rNumericArgs(11)
    EngineDrivenChiller(ChillerNum)%CapRatCoef(2)       = rNumericArgs(12)
    EngineDrivenChiller(ChillerNum)%CapRatCoef(3)       = rNumericArgs(13)
    IF ((rNumericArgs(11)+rNumericArgs(12)+rNumericArgs(13)) == 0.0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Sum of Capacity Ratio Coef = 0.0, chiller='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    EngineDrivenChiller(ChillerNum)%PowerRatCoef(1)     = rNumericArgs(14)
    EngineDrivenChiller(ChillerNum)%PowerRatCoef(2)     = rNumericArgs(15)
    EngineDrivenChiller(ChillerNum)%PowerRatCoef(3)     = rNumericArgs(16)
    EngineDrivenChiller(ChillerNum)%FullLoadCoef(1)     = rNumericArgs(17)
    EngineDrivenChiller(ChillerNum)%FullLoadCoef(2)     = rNumericArgs(18)
    EngineDrivenChiller(ChillerNum)%FullLoadCoef(3)     = rNumericArgs(19)
    EngineDrivenChiller(ChillerNum)%TempLowLimitEvapOut = rNumericArgs(20)


!Load Special EngineDriven Chiller Curve Fit Inputs
    EngineDrivenChiller(ChillerNum)%ClngLoadtoFuelCurve = GetCurveIndex(cAlphaArgs(7)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%ClngLoadtoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%RecJacHeattoFuelCurve = GetCurveIndex(cAlphaArgs(8)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%RecJacHeattoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%RecLubeHeattoFuelCurve = GetCurveIndex(cAlphaArgs(9)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%RecLubeHeattoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%TotExhausttoFuelCurve = GetCurveIndex(cAlphaArgs(10)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%TotExhausttoFuelCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%ExhaustTempCurve = GetCurveIndex(cAlphaArgs(11)) ! convert curve name to number
    IF (EngineDrivenChiller(ChillerNum)%ExhaustTempCurve .EQ. 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%UACoef(1) = rNumericArgs(21)
    EngineDrivenChiller(ChillerNum)%UACoef(2) = rNumericArgs(22)

    EngineDrivenChiller(ChillerNum)%MaxExhaustperPowerOutput = rNumericArgs(23)
    EngineDrivenChiller(ChillerNum)%DesignMinExitGasTemp = rNumericArgs(24)

    EngineDrivenChiller(ChillerNum)%FuelType = TRIM(cAlphaArgs(12))

    SELECT CASE (cAlphaArgs(12))

    CASE ('Gas','NATURALGAS','NATURAL GAS')
      EngineDrivenChiller(ChillerNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      EngineDrivenChiller(ChillerNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      EngineDrivenChiller(ChillerNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       EngineDrivenChiller(ChillerNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       EngineDrivenChiller(ChillerNum)%FuelType = 'FuelOil#2'

    CASE ('Propane','LPG','PROPANEGAS','PROPANE GAS')
       EngineDrivenChiller(ChillerNum)%FuelType = 'Propane'

    CASE DEFAULT
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(12))//'='//TRIM(cAlphaArgs(12)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END SELECT


    EngineDrivenChiller(ChillerNum)%FuelHeatingValue = rNumericArgs(25)
    EngineDrivenChiller(ChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(26)
    IF (EngineDrivenChiller(ChillerNum)%DesignHeatRecVolFlowRate > 0.0) THEN
      EngineDrivenChiller(ChillerNum)%HeatRecActive=.true.
      EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(13),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(13))//'='//TRIM(cAlphaArgs(13)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(14),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(14))//'='//TRIM(cAlphaArgs(14)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(13),cAlphaArgs(14),'Heat Recovery Nodes')
      CALL RegisterPlantCompDesignFlow(EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum, &
                                EngineDrivenChiller(ChillerNum)%DesignHeatRecVolFlowRate)
      ! Condenser flow rate must be specified for heat reclaim
      IF (EngineDrivenChiller(ChillerNum)%CondenserType == AirCooled .OR. &
          EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        IF(EngineDrivenChiller(ChillerNum)%CondVolFlowRate .LE. 0.0)THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(rNumericArgs(10),6)))
          CALL ShowSevereError('Condenser fluid flow rate must be specified for Heat Reclaim applications.')
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        END IF
      END IF
    ELSE
      EngineDrivenChiller(ChillerNum)%HeatRecActive=.false.
      EngineDrivenChiller(ChillerNum)%DesignHeatRecMassFlowRate = 0.0
      EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum   = 0
      EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum   = 0
      ! if heat recovery is not used, don't care about condenser flow rate for air/evap-cooled equip.
      IF (EngineDrivenChiller(ChillerNum)%CondenserType == AirCooled .OR. &
          EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        EngineDrivenChiller(ChillerNum)%CondVolFlowRate = 0.0011d0  ! set to avoid errors in calc routine
      END IF
      IF ((.NOT. lAlphaFieldBlanks(13))  .OR. (.NOT. lAlphaFieldBlanks(14))) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '// &
                    TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('However, Node names were specified for Heat Recovery inlet or outlet nodes')
      ENDIF
    ENDIF


    If(cAlphaArgs(15) .eq. 'CONSTANTFLOW') Then
       EngineDrivenChiller(ChillerNum)%ConstantFlow = .True.
       EngineDrivenChiller(ChillerNum)%VariableFlow = .False.
    Else If(cAlphaArgs(15) .eq. 'VARIABLEFLOW') Then
       EngineDrivenChiller(ChillerNum)%ConstantFlow = .False.
       EngineDrivenChiller(ChillerNum)%VariableFlow = .True.
    Else  ! We will assume a variable flow chiller is none specified
       EngineDrivenChiller(ChillerNum)%ConstantFlow = .False.
       EngineDrivenChiller(ChillerNum)%VariableFlow = .True.
    End If


    EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp = rNumericArgs(27)
    EngineDrivenChiller(ChillerNum)%SizFac = rNumericArgs(28)
    IF (EngineDrivenChiller(ChillerNum)%SizFac <= 0.0) EngineDrivenChiller(ChillerNum)%SizFac = 1.0d0

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    EngineDrivenChiller(ChillerNum)%BasinHeaterPowerFTempDiff = rNumericArgs(29)
    IF(rNumericArgs(29) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(EngineDrivenChiller(ChillerNum)%Name)//&
                     '" TRIM(cNumericFieldNames(29)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    EngineDrivenChiller(ChillerNum)%BasinHeaterSetPointTemp = rNumericArgs(30)

    IF(EngineDrivenChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 30) THEN
        EngineDrivenChiller(ChillerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(EngineDrivenChiller(ChillerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(EngineDrivenChiller(ChillerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(30))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(16))THEN
      EngineDrivenChiller(ChillerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(16))
      IF(EngineDrivenChiller(ChillerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(EngineDrivenChiller(ChillerNum)%Name)//&
                       '" TRIM(cAlphaFieldNames(16)) "'//TRIM(cAlphaArgs(16)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO ChillerNum = 1, NumEngineDrivenChillers
     CALL SetupOutputVariable('Chiller Shaft Power [W]', &
          EngineDrivenChillerReport(ChillerNum)%Power,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Shaft Energy [J]', &
          EngineDrivenChillerReport(ChillerNum)%Energy,'System','Sum',EngineDrivenChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Evap Heat Trans Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%QEvap,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Heat Trans [J]', &
          EngineDrivenChillerReport(ChillerNum)%EvapEnergy,'System','Sum',EngineDrivenChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evap Water Inlet Temp [C]', &
          EngineDrivenChillerReport(ChillerNum)%EvapInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Outlet Temp [C]', &
          EngineDrivenChillerReport(ChillerNum)%EvapOutletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Mass Flow Rate [kg/s]', &
          EngineDrivenChillerReport(ChillerNum)%Evapmdot,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cond Heat Trans Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%QCond,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cond Heat Trans [J]', &
          EngineDrivenChillerReport(ChillerNum)%CondEnergy,'System','Sum',EngineDrivenChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

       !Condenser mass flow and outlet temp are valid for Water Cooled
     IF (EngineDrivenChiller(ChillerNum)%CondenserType == WaterCooled)THEN
       CALL SetupOutputVariable('Chiller Cond Water Inlet Temp [C]', &
            EngineDrivenChillerReport(ChillerNum)%CondInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
       CALL SetupOutputVariable('Chiller Cond Water Outlet Temp [C]', &
            EngineDrivenChillerReport(ChillerNum)%CondOutletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
       CALL SetupOutputVariable('Chiller Cond Water Mass Flow Rate [kg/s]', &
            EngineDrivenChillerReport(ChillerNum)%Condmdot,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     ELSEIF (EngineDrivenChiller(ChillerNum)%CondenserType == AirCooled) THEN
       CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
            EngineDrivenChillerReport(ChillerNum)%CondInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     ELSEIF (EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
       CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
            EngineDrivenChillerReport(ChillerNum)%CondInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
       IF(EngineDrivenChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
         CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
          EngineDrivenChillerReport(ChillerNum)%BasinHeaterPower,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Basin Heater Electric Consumption [J]', &
          EngineDrivenChillerReport(ChillerNum)%BasinHeaterConsumption,'System','Sum',EngineDrivenChiller(ChillerNum)%Name, &
          ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
       END IF
     End IF

     CALL SetupOutputVariable('Chiller Jacket Heat Recovery Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%QJacketRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Jacket Heat Recovery [J]', &
          EngineDrivenChillerReport(ChillerNum)%JacketEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Lube Heat Recovery Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%QLubeOilRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Lube Heat Recovery [J]', &
          EngineDrivenChillerReport(ChillerNum)%LubeOilEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Exhaust Heat Recovery Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%QExhaustRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Exhaust Heat Recovery [J]', &
          EngineDrivenChillerReport(ChillerNum)%ExhaustEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Total Heat Recovery Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%QTotalHEatRecovered,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Total Heat Recovery [J]', &
          EngineDrivenChillerReport(ChillerNum)%TotalHeatEnergyRec,'System','Sum',EngineDrivenChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller ' // TRIM(EngineDrivenChiller(ChillerNum)%FuelType) //' Consumption Rate [W]', &
          EngineDrivenChillerReport(ChillerNum)%FuelEnergyUseRate,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller ' // TRIM(EngineDrivenChiller(ChillerNum)%FuelType) //' Consumption [J]', &
          EngineDrivenChillerReport(ChillerNum)%FuelEnergy,'System','Sum',EngineDrivenChiller(ChillerNum)%Name,  &
                              ResourceTypeKey=EngineDrivenChiller(ChillerNum)%FuelType,EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Fuel COP [W/W]', &
          EngineDrivenChillerReport(ChillerNum)%FuelCOP,'System','Average',EngineDrivenChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller ' // TRIM(EngineDrivenChiller(ChillerNum)%FuelType) //' Mass Flow Rate [kg/s]', &
          EngineDrivenChillerReport(ChillerNum)%FuelMdot,'System','Average',EngineDrivenChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Exhaust Stack Temp[C]', &
          EngineDrivenChillerReport(ChillerNum)%ExhaustStackTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
          EngineDrivenChillerReport(ChillerNum)%HeatRecMdot,'System','Average',EngineDrivenChiller(ChillerNum)%Name)

     IF (EngineDrivenChiller(ChillerNum)%HeatRecActive) THEN
       CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temp[C]', &
          EngineDrivenChillerReport(ChillerNum)%HeatRecInletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)

       CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temp[C]', &
          EngineDrivenChillerReport(ChillerNum)%HeatRecOutletTemp,'System','Average',EngineDrivenChiller(ChillerNum)%Name)
     ENDIF
  END DO




RETURN
END SUBROUTINE GetEngineDrivenChillerInput

! End of Get Input subroutines for the EngineDriven Chiller Module
!******************************************************************************

SUBROUTINE InitEngineDrivenChiller(ChillNum,RunFlag,MyLoad,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Engine Driven Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_EngineDriven, ScanPlantLoopsForObject, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current engine driven chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad
  LOGICAL, INTENT(IN)  :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitEngineDrivenChiller'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode
  INTEGER :: EvapInletNode
  INTEGER :: EvapOutletNode
  INTEGER :: HeatRecInNode
  INTEGER :: HeatRecOutNode
  REAL(r64) :: rho       ! local fluid density
  REAL(r64) :: mdot      ! local mass flow rate
  REAL(r64) :: mdotCond  ! local mass flow rate for condenser
  LOGICAL :: errFlag
  INTEGER :: InletNode
  INTEGER :: OutletNode
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  INTEGER :: BranchIndex
  INTEGER :: CompIndex
  LOGICAL :: FatalError

          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumEngineDrivenChillers))
    ALLOCATE(MyEnvrnFlag(NumEngineDrivenChillers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  !Load inputs to local structure
  CondInletNode  = EngineDrivenChiller(ChillNum)%CondInletNodeNum
  CondOutletNode = EngineDrivenChiller(ChillNum)%CondOutletNodeNum
  EvapInletNode  = EngineDrivenChiller(ChillNum)%EvapInletNodeNum
  EvapOutletNode = EngineDrivenChiller(ChillNum)%EvapOutletNodeNum

  IF (EngineDrivenChiller(ChillNum)%HeatRecActive) THEN
    HeatRecInNode = EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(EngineDrivenChiller(ChillNum)%Name, &
                                 TypeOf_Chiller_EngineDriven, &
                                 EngineDrivenChiller(ChillNum)%CWLoopNum, &
                                 EngineDrivenChiller(ChillNum)%CWLoopSideNum, &
                                 EngineDrivenChiller(ChillNum)%CWBranchNum, &
                                 EngineDrivenChiller(ChillNum)%CWCompNum, &
                                 LowLimitTemp = EngineDrivenChiller(ChillNum)%TempLowLimitEvapOut, &
                                 InletNodeNumber = EngineDrivenChiller(ChillNum)%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (EngineDrivenChiller(ChillNum)%CondenserType /= AirCooled .AND. &
        EngineDrivenChiller(ChillNum)%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(EngineDrivenChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_EngineDriven, &
                                   EngineDrivenChiller(ChillNum)%CDLoopNum, &
                                   EngineDrivenChiller(ChillNum)%CDLoopSideNum, &
                                   EngineDrivenChiller(ChillNum)%CDBranchNum, &
                                   EngineDrivenChiller(ChillNum)%CDCompNum, &
                                   InletNodeNumber = EngineDrivenChiller(ChillNum)%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( EngineDrivenChiller(ChillNum)%CWLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%CWLoopSideNum,  &
                                          EngineDrivenChiller(ChillNum)%CDLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_EngineDriven, .TRUE. )
    ENDIF
    IF (EngineDrivenChiller(ChillNum)%HeatRecActive ) THEN
      CALL ScanPlantLoopsForObject(EngineDrivenChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_EngineDriven, &
                                   EngineDrivenChiller(ChillNum)%HRLoopNum, &
                                   EngineDrivenChiller(ChillNum)%HRLoopSideNum, &
                                   EngineDrivenChiller(ChillNum)%HRBranchNum, &
                                   EngineDrivenChiller(ChillNum)%HRCompNum, &
                                   InletNodeNumber = EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( EngineDrivenChiller(ChillNum)%CWLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%CWLoopSideNum,  &
                                          EngineDrivenChiller(ChillNum)%HRLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_EngineDriven , .TRUE. )
    ENDIF
    MyFlag(ChillNum)=.FALSE.
    IF (EngineDrivenChiller(ChillNum)%CondenserType /= AirCooled .AND. &
        EngineDrivenChiller(ChillNum)%CondenserType /= EvapCooled .AND. &
        EngineDrivenChiller(ChillNum)%HeatRecActive)  THEN
      CALL InterConnectTwoPlantLoopSides( EngineDrivenChiller(ChillNum)%CDLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%CDLoopSideNum,  &
                                          EngineDrivenChiller(ChillNum)%HRLoopNum,      &
                                          EngineDrivenChiller(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_EngineDriven, .FALSE. )
    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('InitEngineDrivenChiller: Program terminated due to previous condition(s).')
    ENDIF


    IF (EngineDrivenChiller(ChillNum)%VariableFlow) Then 
      ! reset flow priority
      PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%LoopSide(EngineDrivenChiller(ChillNum)%CWLoopSideNum)% &
          Branch(EngineDrivenChiller(ChillNum)%CWBranchNum)%Comp(EngineDrivenChiller(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
      ! check if setpoint on outlet node
      IF (Node(EngineDrivenChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. EngineDrivenChiller(ChillNum)%VariableFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(EngineDrivenChiller(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            EngineDrivenChiller(ChillNum)%VariableFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(EngineDrivenChiller(ChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. EngineDrivenChiller(ChillNum)%VariableFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(EngineDrivenChiller(ChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Set Point Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              EngineDrivenChiller(ChillNum)%VariableFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        EngineDrivenChiller(ChillNum)%VariableFlowSetToLoop = .TRUE.
        Node(EngineDrivenChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
    ENDIF

    MyFlag(ChillNum)=.FALSE.
  ENDIF

          !Initialize critical Demand Side Variables
!  IF((MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag) &
!     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN
  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete)  CALL SizeEngineDrivenChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                RoutineName)

    EngineDrivenChiller(ChillNum)%EvapMassFlowRateMax = rho * EngineDrivenChiller(ChillNum)%EvapVolFlowRate
    CALL InitComponentNodes(0.0D0,EngineDrivenChiller(ChillNum)%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         EngineDrivenChiller(ChillNum)%CWLoopNum,               &
                         EngineDrivenChiller(ChillNum)%CWLoopSideNum,           &
                         EngineDrivenChiller(ChillNum)%CWBranchNum,             &
                         EngineDrivenChiller(ChillNum)%CWCompNum)

          !init maximum available condenser flow rate

    IF (EngineDrivenChiller(ChillNum)%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = EngineDrivenChiller(ChillNum)%TempDesCondIn

      rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      EngineDrivenChiller(ChillNum)%CondMassFlowRateMax = rho * EngineDrivenChiller(ChillNum)%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  EngineDrivenChiller(ChillNum)%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         EngineDrivenChiller(ChillNum)%CDLoopNum,               &
                         EngineDrivenChiller(ChillNum)%CDLoopSideNum,           &
                         EngineDrivenChiller(ChillNum)%CDBranchNum,             &
                         EngineDrivenChiller(ChillNum)%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = EngineDrivenChiller(ChillNum)%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,EngineDrivenChiller(ChillNum)%TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0
      Node(CondInletNode)%MassFlowRateMin       = 0.0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0
    END IF

    IF (EngineDrivenChiller(ChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%HRLoopNum)%FluidIndex,&
                                  RoutineName)
      EngineDrivenChiller(ChillNum)%DesignHeatRecMassFlowRate = rho * &
                                         EngineDrivenChiller(ChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, EngineDrivenChiller(ChillNum)%DesignHeatRecMassFlowRate ,  &
                         EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum,        &
                         EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum,       &
                         EngineDrivenChiller(ChillNum)%HRLoopNum,               &
                         EngineDrivenChiller(ChillNum)%HRLoopSideNum,           &
                         EngineDrivenChiller(ChillNum)%HRBranchNum,             &
                         EngineDrivenChiller(ChillNum)%HRCompNum)
    ENDIF

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  IF (EngineDrivenChiller(ChillNum)%VariableFlow .AND. EngineDrivenChiller(ChillNum)%VariableFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(EngineDrivenChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
  ENDIF
  IF (FirstHVACIteration) THEN
    IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
      mdot     = EngineDrivenChiller(ChillNum)%EvapMassFlowRateMax
      mdotCond = EngineDrivenChiller(ChillNum)%CondMassFlowRateMax
    ELSE
      mdot     = 0.d0
      mdotCond = 0.d0
    ENDIF
  ELSE
    IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
      mdot     = EngineDrivenChillerReport(ChillNum)%Evapmdot
      mdotCond = EngineDrivenChillerReport(ChillNum)%Condmdot
    ELSE
      mdot     = 0.d0
      mdotCond = 0.d0
    ENDIF
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              EngineDrivenChiller(ChillNum)%CWLoopNum,     &
                              EngineDrivenChiller(ChillNum)%CWLoopSideNum, &
                              EngineDrivenChiller(ChillNum)%CWBranchNum,   &
                              EngineDrivenChiller(ChillNum)%CWCompNum)
  IF (EngineDrivenChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,        &
                              EngineDrivenChiller(ChillNum)%CDLoopNum,     &
                              EngineDrivenChiller(ChillNum)%CDLoopSideNum, &
                              EngineDrivenChiller(ChillNum)%CDBranchNum,   &
                              EngineDrivenChiller(ChillNum)%CDCompNum)
  ENDIF

  ! Initialize heat recovery flow rates at node
  IF (EngineDrivenChiller(ChillNum)%HeatRecActive ) THEN
    InletNode    =  EngineDrivenChiller(ChillNum)%HeatRecInletNodeNum
    OutletNode   =  EngineDrivenChiller(ChillNum)%HeatRecOutletNodeNum
    LoopNum      =  EngineDrivenChiller(ChillNum)%HRLoopNum
    LoopSideNum  =  EngineDrivenChiller(ChillNum)%HRLoopSideNum
    BranchIndex  =  EngineDrivenChiller(ChillNum)%HRBranchNum
    CompIndex    =  EngineDrivenChiller(ChillNum)%HRCompNum

    If (FirstHVACIteration .AND. RunFlag) Then
      mdot = EngineDrivenChiller(ChillNum)%DesignHeatRecMassFlowRate
    ELSEIF (FirstHVACIteration .AND. (.NOT. RunFlag)) Then
      mdot = 0.d0
    ELSEIF ((.NOT. FirstHVACIteration) .AND. RunFlag) THEN
      mdot = EngineDrivenChillerReport(ChillNum)%HeatRecMdot
    ELSEIF ((.NOT. FirstHVACIteration) .AND. (.NOT. RunFlag)) THEN
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF
  IF (EngineDrivenChiller(ChillNum)%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF

  RETURN

END SUBROUTINE InitEngineDrivenChiller

SUBROUTINE SizeEngineDrivenChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Engine Driven Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho ! local fluid density
  REAL(r64)           ::  Cp  ! local fluid specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap          = EngineDrivenChiller(ChillNum)%NomCap
  tmpEvapVolFlowRate = EngineDrivenChiller(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = EngineDrivenChiller(ChillNum)%CondVolFlowRate

  IF (EngineDrivenChiller(ChillNum)%CondenserType == WaterCooled) THEN
    IF (EngineDrivenChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%PlantSizNum

  IF (EngineDrivenChiller(ChillNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                  'SizeEngineDrivenChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                 InitConvTemp,                      &
                                 PlantLoop(EngineDrivenChiller(ChillNum)%CWLoopNum)%FluidIndex, &
                                 'SizeEngineDrivenChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                              * PlantSizData(PltSizNum)%DesVolFlowRate * EngineDrivenChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) EngineDrivenChiller(ChillNum)%NomCap =  tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize)  EngineDrivenChiller(ChillNum)%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:EngineDriven', EngineDrivenChiller(ChillNum)%Name, &
                              'Nominal Capacity [W]', EngineDrivenChiller(ChillNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Engine Driven Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Engine Driven Chiller object='//TRIM(EngineDrivenChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (EngineDrivenChiller(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate *   &
                                    EngineDrivenChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) EngineDrivenChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) EngineDrivenChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:EngineDriven', EngineDrivenChiller(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              EngineDrivenChiller(ChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Engine Driven Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Engine Driven Chiller object='//TRIM(EngineDrivenChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(EngineDrivenChiller(ChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (EngineDrivenChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  EngineDrivenChiller(ChillNum)%TempDesCondIn, &
                                  PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  'SizeEngineDrivenChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                 EngineDrivenChiller(ChillNum)%TempDesCondIn,                      &
                                 PlantLoop(EngineDrivenChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                 'SizeEngineDrivenChiller')
        tmpCondVolFlowRate = tmpNomCap *   (1.d0 + 1.d0/EngineDrivenChiller(ChillNum)%COP) / &
                                                 ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize)  EngineDrivenChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.0d0
        IF (PlantSizesOkayToFinalize)  EngineDrivenChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)  CALL ReportSizingOutput('Chiller:EngineDriven', EngineDrivenChiller(ChillNum)%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              EngineDrivenChiller(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowContinueError('Autosizing of EngineDriven Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in EngineDriven Chiller object='//TRIM(EngineDrivenChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (EngineDrivenChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CALL RegisterPlantCompDesignFlow(EngineDrivenChiller(ChillNum)%CondInletNodeNum, tmpCondVolFlowRate)
  ENDIF

  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = EngineDrivenChiller(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:EngineDriven')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,EngineDrivenChiller(ChillNum)%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,EngineDrivenChiller(ChillNum)%NomCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeEngineDrivenChiller

! Beginning of Chiller model Subroutines
! *****************************************************************************

SUBROUTINE CalcEngineDrivenChillerModel(ChillerNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression chiller using the EngineDriven model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1. BLAST Users Manual
          ! 2. CHILLER User Manual

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, CurrentTime
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys, SysTimeElapsed
  USE CurveManager,    ONLY : CurveValue
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : ControlType_SeriesActive, MassFlowTol, PlantLoop, &
                              TypeOf_Chiller_EngineDriven, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillerNum      ! chiller number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
 ! INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER   :: ExhaustCP = 1.047d0    !Exhaust Gas Specific Heat (J/kg-K)
  REAL(r64), PARAMETER   :: KJtoJ = 1000.d0        !convert Kjoules to joules
  REAL(r64), PARAMETER   :: ReferenceTemp = 25.0d0 !Reference temperature by which lower heating
                                                   ! value is reported.  This should be subtracted
                                                   ! off of when calculated exhaust energies.
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: CapacityRat         ! intermediate result:  capacity ratio
  REAL(r64), DIMENSION(3)     :: PowerRat            ! intermediate result:  power ratio
  REAL(r64), DIMENSION(3)     :: FullLoadFactor      ! intermediate result:  full load factor
  REAL(r64)              :: MinPartLoadRat      ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat      ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn          ! C - (EngineDriven ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempCondInDesign    ! C - (EngineDriven ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempRiseRat         ! intermediate result:  temperature rise ratio
  REAL(r64)              :: EvapInletTemp       ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp       ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut         ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: TempEvapOutDesign   ! design evaporator outlet temperature, water side
  REAL(r64)              :: ChillerNomCap       ! chiller nominal capacity
  REAL(r64)              :: AvailChillerCap     ! chiller available capacity
  REAL(r64)              :: COP                 ! coefficient of performance
  REAL(r64)              :: FracFullLoadPower   ! fraction of full load power
  REAL(r64)              :: EvapDeltaTemp       ! C - evaporator temperature difference, water side
  REAL(r64)              :: DeltaTemp             ! C - intermediate result: condenser/evaporator temp diff
  REAL(r64)              :: AvailNomCapRat        ! intermediate result: available nominal capacity ratio
  REAL(r64)              :: FullLoadPowerRat      ! intermediate result: full load power ratio
  REAL(r64)              :: PartLoadRat           ! part load ratio for efficiency
  REAL(r64)              :: OperPartLoadRat     ! Actual operating PLR
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  REAL(r64)              :: EvapMassFlowRateMax ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
  REAL(r64)              :: TempLowLimitEout    ! C - Evaporator low temp. limit cut off
  REAL(r64)              :: FRAC
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  REAL(r64),SAVE  :: TimeStepSysLast=0.0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages
  REAL(r64)       :: Cp                      ! local for fluid specif heat, for evaporator
  REAL(r64)       :: CpCond                  ! local for fluid specif heat, for condenser

! Special variables for EngineDriven Chiller
  REAL(r64)    :: MaxExhaustperPowerOutput !curve fit parameter
  REAL(r64)    :: ClngLoadFuelRat      !(RELDC) Ratio of Shaft Power to Fuel Energy Input
  REAL(r64)    :: RecJacHeattoFuelRat  !(RJACDC) Ratio of Recoverable Jacket Heat to Fuel Energy Input
  REAL(r64)    :: RecLubeHeattoFuelRat !(RLUBDC) Ratio of Recoverable Lube Oil Heat to Fuel Energy Input
  REAL(r64)    :: TotExhausttoFuelRat  !(REXDC) Total Exhaust Energy Input to Fuel Energy Input
  REAL(r64)    :: TotalExhaustEnergy
  REAL(r64)    :: ExhaustTemp          !(TEX) Exhaust Gas Temp
  REAL(r64)    :: ExhaustGasFlow       !exhaust gas mass flow rate
  REAL(r64)    :: DesignMinExitGasTemp
  REAL(r64)    :: UA                   !(UACDC) exhaust gas Heat Exchanger UA
  REAL(r64)    :: HeatRecCp            !Specific Heat of the Heat Recovery Fluid (J/kg-K)
  REAL(r64)    :: EngineDrivenFuelEnergy
  REAL(r64)    :: HeatRecRatio              !When Max Temp is reached the amount of recovered heat has to be reduced.
!  LOGICAL,SAVE :: PossibleSubCooling=.FALSE.

      !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0
  CondMassFlowRate           = 0.0
  Power                      = 0.0
  QCondenser                 = 0.0
  QEvaporator                = 0.0
  Energy                     = 0.0
  CondenserEnergy            = 0.0
  EvaporatorEnergy           = 0.0
  HeatRecCp                  = 0.0
  HeatRecMdotActual          = 0.0
  QTotalHeatRecovered        = 0.0
  QJacketRecovered           = 0.0
  QLubeOilRecovered          = 0.0
  QExhaustRecovered          = 0.0
  EngineDrivenFuelEnergy     = 0.0
  FuelEnergyUseRate          = 0.0
  TotalHeatEnergyRec         = 0.0
  JacketEnergyRec            = 0.0
  LubeOilEnergyRec           = 0.0
  ExhaustEnergyRec           = 0.0
  FuelEnergy                 = 0.0
  FuelMdot                   = 0.0
  ExhaustStackTemp           = 0.0
  FRAC                       = 1.0

  IF (EngineDrivenChiller(ChillerNum)%HeatRecActive) THEN
     HeatRecInletTemp           = Node(EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum)%Temp
     HeatRecOutletTemp          = Node(EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum)%Temp
     HeatRecMdotDesign          = EngineDrivenChiller(ChillerNum)%DesignHeatRecMassFlowRate
  ENDIF

  EvapInletNode  = EngineDrivenChiller(ChillerNum)%EvapInletNodeNum
  EvapOutletNode = EngineDrivenChiller(ChillerNum)%EvapOutletNodeNum
  CondInletNode  = EngineDrivenChiller(ChillerNum)%CondInletNodeNum
  CondOutletNode = EngineDrivenChiller(ChillerNum)%CondOutletNodeNum
  LoopNum        = EngineDrivenChiller(ChillerNum)%CWLoopNum
  LoopSideNum    = EngineDrivenChiller(ChillerNum)%CWLoopSideNum
  EvapInletTemp  = Node(EvapInletNode)%Temp

!   calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(EngineDrivenChiller(ChillerNum)%PrintMessage)THEN
        EngineDrivenChiller(ChillerNum)%MsgErrorCount = &
                         EngineDrivenChiller(ChillerNum)%MsgErrorCount + 1
!     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (EngineDrivenChiller(ChillerNum)%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(EngineDrivenChiller(ChillerNum)%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(EngineDrivenChiller(ChillerNum)%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(EngineDrivenChiller(ChillerNum)%MsgBuffer1)//' error continues.', &
           EngineDrivenChiller(ChillerNum)%ErrCount1,ReportMaxOf=EngineDrivenChiller(ChillerNum)%MsgDataLast,  &
           ReportMinOf=EngineDrivenChiller(ChillerNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

! save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

   !If Chiller load is 0 or chiller is not running then leave the subroutine.
  IF(MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate           = 0.d0

      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%CWCompNum)
    ENDIF
    
    IF (EngineDrivenChiller(ChillerNum)%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(EngineDrivenChiller(ChillerNum)%CDLoopNum)% &
            LoopSide(EngineDrivenChiller(ChillerNum)%CDLoopSideNum)% &
              Branch(EngineDrivenChiller(ChillerNum)%CDBranchNum)%  &
                Comp(EngineDrivenChiller(ChillerNum)%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate           = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate           = 0.d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                                EngineDrivenChiller(ChillerNum)%CDLoopNum, &
                                EngineDrivenChiller(ChillerNum)%CDLoopSideNum, &
                                EngineDrivenChiller(ChillerNum)%CDBranchNum, &
                                EngineDrivenChiller(ChillerNum)%CDCompNum)
      ENDIF
    ENDIF

    IF (EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(EngineDrivenChiller(ChillerNum)%BasinHeaterPowerFTempDiff,&
                                EngineDrivenChiller(ChillerNum)%BasinHeaterSchedulePtr,&
                                EngineDrivenChiller(ChillerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    EngineDrivenChiller(ChillerNum)%PrintMessage = .FALSE.
    RETURN
  END IF

  IF (EngineDrivenChiller(ChillerNum)%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
!  Warn user if entering condenser temperature falls below 0C
      IF(Node(CondInletNode)%Temp .LT. 0.0 .and. .not. WarmupFlag) THEN
        EngineDrivenChiller(ChillerNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        EngineDrivenChiller(ChillerNum)%MsgBuffer1 = 'CalcEngineDrivenChillerModel - Chiller:EngineDriven "' &
                             //TRIM(EngineDrivenChiller(ChillerNum)%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        EngineDrivenChiller(ChillerNum)%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        EngineDrivenChiller(ChillerNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        EngineDrivenChiller(ChillerNum)%PrintMessage = .FALSE.
      ENDIF
  Else IF (EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
      IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
        EngineDrivenChiller(ChillerNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        EngineDrivenChiller(ChillerNum)%MsgBuffer1 = 'CalcEngineDrivenChillerModel - Chiller:EngineDriven "' &
                             //TRIM(EngineDrivenChiller(ChillerNum)%Name)// &
                             '" - Evap Cooled Condenser Inlet Temperature below 10C'
        EngineDrivenChiller(ChillerNum)%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        EngineDrivenChiller(ChillerNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        EngineDrivenChiller(ChillerNum)%PrintMessage = .FALSE.
      ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

  ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

        !Set mass flow rates
  IF (EngineDrivenChiller(ChillerNum)%CondenserType == WaterCooled) THEN
    CondMassFlowRate = EngineDrivenChiller(ChillerNum)%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              EngineDrivenChiller(ChillerNum)%CDLoopNum, &
                              EngineDrivenChiller(ChillerNum)%CDLoopSideNum, &
                              EngineDrivenChiller(ChillerNum)%CDBranchNum, &
                              EngineDrivenChiller(ChillerNum)%CDCompNum)
    CALL PullCompInterconnectTrigger(EngineDrivenChiller(ChillerNum)%CWLoopNum, &
                                     EngineDrivenChiller(ChillerNum)%CWLoopSideNum, &
                                     EngineDrivenChiller(ChillerNum)%CWBranchNum, &
                                     EngineDrivenChiller(ChillerNum)%CWCompNum, &
                                     EngineDrivenChiller(ChillerNum)%CondMassFlowIndex,              &
                                     EngineDrivenChiller(ChillerNum)%CDLoopNum, &
                                     EngineDrivenChiller(ChillerNum)%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)
    IF (CondMassFlowRate < MassFlowTol) RETURN

  END IF

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  CapacityRat        = EngineDrivenChiller(ChillerNum)%CapRatCoef
  PowerRat           = EngineDrivenChiller(ChillerNum)%PowerRatCoef
  FullLoadFactor     = EngineDrivenChiller(ChillerNum)%FullLoadCoef
  MinPartLoadRat     = EngineDrivenChiller(ChillerNum)%MinPartLoadRat
  MaxPartLoadRat     = EngineDrivenChiller(ChillerNum)%MaxPartLoadRat
  TempCondInDesign   = EngineDrivenChiller(ChillerNum)%TempDesCondIn
  TempRiseRat        = EngineDrivenChiller(ChillerNum)%TempRiseCoef
  TempEvapOutDesign  = EngineDrivenChiller(ChillerNum)%TempDesEvapOut
  ChillerNomCap      = EngineDrivenChiller(ChillerNum)%NomCap
  COP                = EngineDrivenChiller(ChillerNum)%COP
  TempCondIn         = Node(EngineDrivenChiller(ChillerNum)%CondInletNodeNum)%Temp
  TempEvapOut        = Node(EngineDrivenChiller(ChillerNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = EngineDrivenChiller(ChillerNum)%TempLowLimitEvapOut
  MaxExhaustperPowerOutput  = EngineDrivenChiller(ChillerNum)%MaxExhaustperPowerOutput
  LoopNum            = EngineDrivenChiller(ChillerNum)%CWLoopNum
  LoopSideNum        = EngineDrivenChiller(ChillerNum)%CWLoopSideNum
  EvapMassFlowRateMax    = EngineDrivenChiller(ChillerNum)%EvapMassFlowRateMax

!*********************************
  !Calculate chiller performance from this set of performance equations.
  !  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
    DeltaTemp= (TempCondIn   -  TempCondInDesign) / TempRiseRat &
           - (TempEvapOut -  TempEvapOutDesign)

  !  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
    AvailNomCapRat =   CapacityRat(1)                   &
                     + CapacityRat(2) * DeltaTemp       &
                     + CapacityRat(3) * DeltaTemp ** 2

    AvailChillerCap = ChillerNomCap*AvailNomCapRat

   ! from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
    FullLoadPowerRat=   PowerRat(1)                         &
                      + PowerRat(2) * AvailNomCapRat      &
                      + PowerRat(3) * AvailNomCapRat ** 2

  !  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
  !         /RCAV,MAXCHFR(I,IPLCTR)))
 IF (AvailChillerCap > 0.0) THEN
   PartLoadRat = MAX(MinPartLoadRat, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
 ENDIF
   ! from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
    FracFullLoadPower = FullLoadFactor(1)                      &
                      + FullLoadFactor(2) * PartLoadRat      &
                      + FullLoadFactor(3) * PartLoadRat ** 2

 IF (AvailChillerCap > 0.0) THEN
   IF(ABS(MyLoad)/AvailChillerCap .LT. MinPartLoadRat) THEN
     OperPartLoadRat = ABS(MyLoad)/AvailChillerCap
   ELSE
     OperPartLoadRat = PartLoadRat
   END IF
 ELSE
   OperPartLoadRat = 0.0d0
 ENDIF
!*********************************
  Cp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillerNum)%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(EngineDrivenChiller(ChillerNum)%CWLoopNum)%FluidIndex, &
                         'CalcEngineDrivenChillerModel')

  ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
  ! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN
    EngineDrivenChiller(ChillerNum)%PossibleSubCooling = .FALSE.
    QEvaporator = AvailChillerCap * OperPartLoadRat
    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
     FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
     FRAC = 1.0d0
    END IF
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap/COP * FRAC

    ! Either set the flow to the Constant value or caluclate the flow for the variable volume
    If(EngineDrivenChiller(ChillerNum)%ConstantFlow)Then
      ! Start by assuming max (design) flow
      EvapMassFlowRate = EvapMassFlowRateMax
      ! Use SetComponentFlowRate to decide actual flow
      Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%CWCompNum)
      ! Evaluate delta temp based on actual flow rate
      IF (EvapMassFlowRate /= 0.0D0) THEN
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      ELSE
        EvapDeltaTemp = 0.0D0
      ENDIF
      ! Evaluate outlet temp based on delta
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

    Else IF(EngineDrivenChiller(ChillerNum)%VariableFlow)Then

      ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
      EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint

      IF (EvapDeltaTemp /= 0) THEN
        EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
        IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTol) &
             EngineDrivenChiller(ChillerNum)%PossibleSubCooling = .TRUE.
        !Check to see if the Maximum is exceeded, if so set to maximum
        EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%CWCompNum)
        EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
      ELSE
        ! Try to request zero flow
        EvapMassFlowRate=0.0
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          EngineDrivenChiller(ChillerNum)%CWLoopNum,     &
                          EngineDrivenChiller(ChillerNum)%CWLoopSideNum, &
                          EngineDrivenChiller(ChillerNum)%CWBranchNum,   &
                          EngineDrivenChiller(ChillerNum)%CWCompNum)
        ! No deltaT since component is not running
        EvapOutletTemp = Node(EvapInletNode)%Temp

      END IF
    End If  !End of Constant Variable Flow If Block
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              EngineDrivenChiller(ChillerNum)%CWLoopNum,     &
                              EngineDrivenChiller(ChillerNum)%CWLoopSideNum, &
                              EngineDrivenChiller(ChillerNum)%CWBranchNum,   &
                              EngineDrivenChiller(ChillerNum)%CWCompNum)
    ! Some other component set the flow to 0. No reason to continue with calculations.
    IF(EvapMassFlowRate == 0.0d0)THEN
      MyLoad = 0.0d0
      IF (EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(EngineDrivenChiller(ChillerNum)%BasinHeaterPowerFTempDiff,&
                            EngineDrivenChiller(ChillerNum)%BasinHeaterSchedulePtr,&
                            EngineDrivenChiller(ChillerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      ENDIF
      EngineDrivenChiller(ChillerNum)%PrintMessage = .FALSE.
      RETURN
    END IF

    IF(EngineDrivenChiller(ChillerNum)%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      END IF
    ELSE !No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
      IF ((EngineDrivenChiller(ChillerNum)%VariableFlow) .OR. &
          (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(EngineDrivenChiller(ChillerNum)%CWBranchNum) &
            %Comp(EngineDrivenChiller(ChillerNum)%CWCompNum)%CurOpSchemeType &
               == CompSetPtBasedSchemeType)          .OR. &
          (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
        TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
      ELSE
        TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint
    END IF
    !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTemptol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTemptol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    ! If load exceeds the distributed load set to the distributed load
    If(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTol) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End IF

    ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > (AvailChillerCap * MaxPartLoadRat))Then
      If(EvapMassFlowRate > MassFlowTol) THEN
        QEvaporator = AvailChillerCap * OperPartLoadRat
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

   IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
     FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
   ELSE
     FRAC = 1.0d0
   END IF

  ! set the module level variable used for reporting FRAC
   ChillerCyclingRatio = FRAC

  ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap /COP * FRAC

    IF(EvapMassFlowRate == 0.0) THEN
      QEvaporator = 0.0
      EvapOutletTemp = Node(EvapInletNode)%Temp
      Power = 0.0
      EngineDrivenChiller(ChillerNum)%PrintMessage = .FALSE.
    END IF
    IF(QEvaporator == 0.0d0 .AND. EngineDrivenChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(EngineDrivenChiller(ChillerNum)%BasinHeaterPowerFTempDiff,&
                                EngineDrivenChiller(ChillerNum)%BasinHeaterSchedulePtr,&
                                EngineDrivenChiller(ChillerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    END IF
  END IF  !This is the end of the FlowLock Block


!Now determine Cooling
    !QCondenser is calculated the same for each type, but the power consumption should be different
    !  depending on the performance coefficients used for the chiller model.
  QCondenser = Power + QEvaporator

  IF (EngineDrivenChiller(ChillerNum)%CondenserType == WaterCooled) THEN

    IF (CondMassFlowRate > MassFlowTol) THEN
      CpCond = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillerNum)%CDLoopNum)%FluidName,  &
                                         CondInletTemp,                      &
                                         PlantLoop(EngineDrivenChiller(ChillerNum)%CDLoopNum)%FluidIndex, &
                                         'CalcEngineDrivenChillerModel')
      CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
    ELSE
      CALL ShowSevereError('CalcEngineDrivenChillerModel: Condenser flow = 0, for EngineDrivenChiller='//  &
                           TRIM(EngineDrivenChiller(ChillerNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
    END IF

  ELSE !Air Cooled or Evap Cooled

    !don't care about outlet temp for Air-Cooled or Evap Cooled
    CondOutletTemp = CondInletTemp
  END IF

! EngineDriven Portion of the Engine Driven Chiller:

!DETERMINE FUEL CONSUMED AND AVAILABLE WASTE HEAT

!Use Curve fit to determine Fuel Energy Input.  For electric power generated in Watts, the fuel
!energy input is calculated in J/s.  The PLBasedFuelInputCurve selects ratio of fuel flow (J/s)/cooling load (J/s).
  IF (PartLoadRat == 0)THEN
    EngineDrivenFuelEnergy = 0
  ELSE
    PartLoadRat = MAX(MinPartLoadRat,PartLoadRat)
    ClngLoadFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%ClngLoadtoFuelCurve, PartLoadRat)
    EngineDrivenFuelEnergy = QEvaporator / ClngLoadFuelRat
  END IF
!Use Curve fit to determine energy recovered in the water jacket.  This curve calculates the water jacket energy recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the water jacket at that
!particular part load.

  RecJacHeattoFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%RecJacHeattoFuelCurve, PartLoadRat)
  QJacketRecovered = EngineDrivenFuelEnergy * RecJacHeattoFuelRat

!Use Curve fit to determine Heat Recovered Lubricant Energy.  This curve calculates the lube energy recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the lube oil at that
!particular part load.
  RecLubeHeattoFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%RecLubeHeattoFuelCurve, PartLoadRat)
  QLubeOilRecovered = EngineDrivenFuelEnergy * RecLubeHeattoFuelRat

!Use Curve fit to determine Heat Recovered from the exhaust.  This curve calculates the  energy recovered (J/s) by
!multiplying the total fuel input (J/s) by the fraction of that power that could be recovered in the exhaust at that
!particular part load.
  TotExhausttoFuelRat = CurveValue(EngineDrivenChiller(ChillerNum)%TotExhausttoFuelCurve, PartLoadRat)
  TotalExhaustEnergy = EngineDrivenFuelEnergy * TotExhausttoFuelRat


!Use Curve fit to determine Exhaust Temperature in C.  The temperature is simply a curve fit
!of the exhaust temperature in C to the part load ratio.
  IF (PartLoadRat /= 0)THEN
    ExhaustTemp = CurveValue(EngineDrivenChiller(ChillerNum)%ExhaustTempCurve, PartLoadRat)
    ExhaustGasFlow = TotalExhaustEnergy / (ExhaustCP*(ExhaustTemp-ReferenceTemp))


!Use Curve fit to determine stack temp after heat recovery
    UA = EngineDrivenChiller(ChillerNum)%UACoef(1) * ChillerNomCap **  &
                   EngineDrivenChiller(ChillerNum)%UACoef(2)

    DesignMinExitGasTemp = EngineDrivenChiller(ChillerNum)%DesignMinExitGasTemp
    ExhaustStackTemp = DesignMinExitGasTemp + (ExhaustTemp - DesignMinExitGasTemp) / &
                         EXP(UA/(MAX(ExhaustGasFlow, MaxExhaustperPowerOutput * ChillerNomCap) * ExhaustCP))

    QExhaustRecovered = MAX(ExhaustGasFlow*ExhaustCP*(ExhaustTemp-ExhaustStackTemp),0.0d0)
  ELSE
    QExhaustRecovered = 0
  END IF


  QTotalHeatRecovered = QExhaustRecovered + QLubeOilRecovered + QJacketRecovered

  !Update Heat Recovery temperatures
  IF (EngineDrivenChiller(ChillerNum)%HeatRecActive) THEN
    CALL CalcEngineChillerHeatRec(ChillerNum,QTotalHeatRecovered,FirstIteration,HeatRecRatio)
    QExhaustRecovered = QExhaustRecovered*HeatRecRatio
    QLubeOilRecovered = QLubeOilRecovered*HeatRecRatio
    QJacketRecovered  = QJacketRecovered*HeatRecRatio

  ENDIF

      !Calculate Energy
  CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
  Energy           = Power*TimeStepSys*SecInHour
  EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour
  FuelEnergyUseRate = EngineDrivenFuelEnergy
  FuelEnergy       = FuelEnergyUseRate*TimeStepSys*SecInHour
  JacketEnergyRec      = QJacketRecovered*TimeStepSys*SecInHour
  LubeOilEnergyRec     = QLubeOilRecovered*TimeStepSys*SecInHour
  ExhaustEnergyRec     = QExhaustRecovered*TimeStepSys*SecInHour
  QTotalHeatRecovered = QExhaustRecovered + QLubeOilRecovered + QJacketRecovered
  TotalHeatEnergyRec  = ExhaustEnergyRec + LubeOilEnergyRec + JacketEnergyRec
  FuelEnergyUseRate   = ABS(FuelEnergyUseRate)
  FuelEnergy          = ABS(FuelEnergy)
  FuelMdot      =  ABS(FuelEnergyUseRate)/(EngineDrivenChiller(ChillerNum)%FuelHeatingValue * KJtoJ)

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem
    IF (EngineDrivenChiller(ChillerNum)%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcEngineDrivenChillerModel: Condenser loop inlet temperatures '//  &
           '> 70.0 C for EngineDrivenChiller='//  &
           TRIM(EngineDrivenChiller(ChillerNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    IF(.NOT.WarmupFlag)THEN
      If (AvailNomCapRat < 0.0 ) then     ! apparently the real reason energy goes negative
        CALL ShowSevereError('CalcEngineDrivenChillerModel: Capacity ratio below zero for EngineDrivenChiller='//  &
                              TRIM(EngineDrivenChiller(ChillerNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Check input for Capacity Ratio Curve')
        CALL showContinueError('Condenser inlet temperature: '//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )
        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0
    Energy = 0.0
  ENDIF

  RETURN
END SUBROUTINE CalcEngineDrivenChillerModel

SUBROUTINE CalcEngineChillerHeatRec(ChillerNum, EnergyRecovered, FirstHVACIteration,HeatRecRatio)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Brandon Anderson
            !       DATE WRITTEN:    November 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! To perform heat recovery calculations and node updates


            ! METHODOLOGY EMPLOYED: This routine is required for the heat recovery loop.
            ! It works in conjunction with the Heat Recovery Manager, and the PlantWaterHeater.
            ! The chiller sets the flow on the loop first by the input design flowrate and then
            ! performs a check to verify that

            ! REFERENCES: na

            ! USE STATEMENTS: na
USE Psychrometrics,  ONLY: PsyCpAirFnWTdb
USE FluidProperties, ONLY: GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT(IN)          :: ChillerNum          ! Chiller number
  REAL(r64), INTENT(IN)       :: EnergyRecovered     ! Amount of heat recovered
  LOGICAL                     :: FirstHVACIteration  ! TRUE when first iteration of timestep !unused1208
  REAL(r64),INTENT(INOUT)     :: HeatRecRatio        ! Max Heat recovery ratio


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: HeatRecInNode
  INTEGER                  :: HeatRecOutNode
  REAL(r64)                :: HeatRecMdot
  REAL(r64)                :: MinHeatRecMdot
  REAL(r64)                :: HeatRecInTemp
  REAL(r64)                :: HeatRecOutTemp
  REAL(r64)                :: HeatRecCp

  !Load inputs to local structure
  HeatRecInNode  = EngineDrivenChiller(ChillerNum)%HeatRecInletNodeNum
  HeatRecOutNode = EngineDrivenChiller(ChillerNum)%HeatRecOutletNodeNum

  !Need to set the HeatRecRatio to 1.0 if it is not modified
  HeatRecRatio= 1.0

!  !This mdot is input specified mdot "Desired Flowrate", already set in init routine
  HeatRecMdot = Node(HeatRecInNode)%MassFlowRate

  HeatRecInTemp = Node(HeatRecInNode)%Temp
  HeatRecCp = GetSpecificHeatGlycol(PlantLoop(EngineDrivenChiller(ChillerNum)%HRLoopNum)%FluidName,  &
                                   HeatRecInletTemp,                      &
                                   PlantLoop(EngineDrivenChiller(ChillerNum)%HRLoopNum)%FluidIndex, &
                                   'ChillerHeatRecovery')


  !Don't divide by zero - Note This also results in no heat recovery when
  !  design Mdot for Heat Recovery - Specified on Chiller Input - is zero
  !  In order to see what minimum heat recovery flow rate is for the design temperature
  !  The design heat recovery flow rate can be set very small, but greater than zero.
  IF ((HeatRecMdot .GT. 0) .AND. (HeatRecCp .GT. 0)) THEN
    HeatRecOutTemp = (EnergyRecovered)/(HeatRecMdot * HeatRecCp) + HeatRecInTemp
  ELSE
    HeatRecOutTemp = HeatRecInTemp
  END IF

  !Now verify that the design flowrate was large enough to prevent phase change
  IF(HeatRecOutTemp > EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp) THEN
   IF(EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp /= HeatRecInTemp)THEN
      MinHeatRecMdot = (EnergyRecovered)/(HeatRecCp * (EngineDrivenChiller(ChillerNum)%HeatRecMaxTemp - HeatRecInTemp))
      If(MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0
    END IF

    !Recalculate Outlet Temperature, with adjusted flowrate
    IF ((MinHeatRecMdot .GT. 0.0) .AND. (HeatRecCp .GT. 0.0)) THEN
      HeatRecOutTemp = (EnergyRecovered)/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
      HeatRecRatio = HeatRecMdot/MinHeatRecMdot
    ELSE
      HeatRecOutTemp = HeatRecInTemp
      HeatRecRatio = 0.0
    END IF

  END IF


  !Update global variables for reporting later
  HeatRecInletTemp  = HeatRecInTemp
  HeatRecOutletTemp = HeatRecOutTemp
  HeatRecMdotActual = HeatRecMdot

END SUBROUTINE CalcEngineChillerHeatRec


! End of EngineDriven Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the EngineDriven Chiller Module
! *****************************************************************************

SUBROUTINE UpdateEngineDrivenChiller(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na

USE DataGlobals,     ONLY: SecInHour
USE DataHVACGlobals, ONLY: TimeStepSys
USE PlantUtilities,  ONLY: SafeCopyPlantNode

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)     :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! chiller number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode       ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode      ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode       ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode      ! condenser outlet node number, water side
  INTEGER                :: HeatRecInletNode
  INTEGER                :: HeatRecOutletNode
  REAL(r64)              :: ReportingConstant   ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

    EvapInletNode  = EngineDrivenChiller(Num)%EvapInletNodeNum
    EvapOutletNode = EngineDrivenChiller(Num)%EvapOutletNodeNum
    CondInletNode  = EngineDrivenChiller(Num)%CondInletNodeNum
    CondOutletNode = EngineDrivenChiller(Num)%CondOutletNodeNum

    HeatRecInletNode = EngineDrivenChiller(Num)%HeatRecInletNodeNum
    HeatRecOutletNode = EngineDrivenChiller(Num)%HeatRecOutletNodeNum

  IF (MyLoad >=0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running
          !set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp

    EngineDrivenChillerReport(Num)%Power            = 0
    EngineDrivenChillerReport(Num)%QEvap            = 0
    EngineDrivenChillerReport(Num)%QCond            = 0
    EngineDrivenChillerReport(Num)%Energy           = 0
    EngineDrivenChillerReport(Num)%EvapEnergy       = 0
    EngineDrivenChillerReport(Num)%CondEnergy       = 0
    EngineDrivenChillerReport(Num)%EvapInletTemp  = Node(EvapInletNode)%Temp
    EngineDrivenChillerReport(Num)%CondInletTemp  = Node(CondInletNode)%Temp
    EngineDrivenChillerReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    EngineDrivenChillerReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    EngineDrivenChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    EngineDrivenChillerReport(Num)%Condmdot         = CondMassFlowRate
    EngineDrivenChillerReport(Num)%FuelCOP          = 0
    IF (EngineDrivenChiller(Num)%CondenserType == EvapCooled) THEN
      EngineDrivenChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      EngineDrivenChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
  ELSE !Chiller is running
          !set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp

    EngineDrivenChillerReport(Num)%Power            = Power
    EngineDrivenChillerReport(Num)%QEvap            = QEvaporator
    EngineDrivenChillerReport(Num)%QCond            = QCondenser
    EngineDrivenChillerReport(Num)%Energy           = Energy
    EngineDrivenChillerReport(Num)%EvapEnergy       = EvaporatorEnergy
    EngineDrivenChillerReport(Num)%CondEnergy       = CondenserEnergy
    EngineDrivenChillerReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    EngineDrivenChillerReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    EngineDrivenChillerReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    EngineDrivenChillerReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    EngineDrivenChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    EngineDrivenChillerReport(Num)%Condmdot         = CondMassFlowRate
    IF (FuelEnergyUseRate .NE. 0.0) THEN
      EngineDrivenChillerReport(Num)%FuelCOP          = QEvaporator/FuelEnergyUseRate
    ELSE
      EngineDrivenChillerReport(Num)%FuelCOP          = 0.0
    END IF
    IF (EngineDrivenChiller(Num)%CondenserType == EvapCooled) THEN
      EngineDrivenChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      EngineDrivenChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
  END IF

! Update Heat Recovery Stuff whether running or not, variables should be set correctly
  EngineDrivenChillerReport(Num)%QJacketRecovered     = QJacketRecovered
  EngineDrivenChillerReport(Num)%QLubeOilRecovered    = QLubeOilRecovered
  EngineDrivenChillerReport(Num)%QExhaustRecovered    = QExhaustRecovered
  EngineDrivenChillerReport(Num)%QTotalHeatRecovered  = QTotalHeatRecovered
  EngineDrivenChillerReport(Num)%FuelEnergyUseRate    = FuelEnergyUseRate
  EngineDrivenChillerReport(Num)%JacketEnergyRec      = JacketEnergyRec
  EngineDrivenChillerReport(Num)%LubeOilEnergyRec     = LubeOilEnergyRec
  EngineDrivenChillerReport(Num)%ExhaustEnergyRec     = ExhaustEnergyRec
  EngineDrivenChillerReport(Num)%TotalHeatEnergyRec   = TotalHeatEnergyRec
  EngineDrivenChillerReport(Num)%FuelEnergy           = FuelEnergy
  EngineDrivenChillerReport(Num)%FuelMdot             = FuelMdot
  EngineDrivenChillerReport(Num)%ExhaustStackTemp     = ExhaustStackTemp
  EngineDrivenChillerReport(Num)%HeatRecInletTemp     = HeatRecInletTemp
  EngineDrivenChillerReport(Num)%HeatRecOutletTemp    = HeatRecOutletTemp
  EngineDrivenChillerReport(Num)%HeatRecMdot          = HeatRecMdotActual


  !Update the Heat Recovery outlet
  IF (EngineDrivenChiller(Num)%HeatRecActive) THEN
    CALL SafeCopyPlantNode(HeatRecInletNode, HeatRecOutletNode)
    Node(HeatRecOutletNode)%Temp = HeatRecOutletTemp
  ENDIF

RETURN
END SUBROUTINE UpdateEngineDrivenChiller

END MODULE ChillerEngineDriven

!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


MODULE ChillerGasTurbine  !Gas Turbine ChillerModule

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   September 2000
          !       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of the GT vapor
          ! compression Chillers.

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the GT chiller
          ! is available to meet a loop cooling demand, it calls SimGT
          ! Chiller which in turn calls the GT chiller model.
          ! The GT chiller model is based on a polynomial fit of chiller
          ! performance data.

          ! REFERENCES:
          ! 1. BLAST Users Manual


          ! OTHER NOTES:
          ! The CHILLER program from the GT family of software can be used
          ! to generate the coefficients for the model.

          ! The Electric, Diesel, and Gas Turbine chiller models are very similar.
          ! They only differ in the unit that drives the chiller.  Therefore,
          ! the Blast curve fit portion for each chiller is the same and is in the
          ! general routines module under CalcChillerPower.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,   ONLY : MaxNameLength, NumOfTimeStepInHour, InitConvTemp, WarmupFlag
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE DataPlant,       ONLY: DeltaTemptol,TypeOf_Chiller_CombTurbine
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE
PRIVATE

          !MODULE PARAMETER DEFINITIONS:
! Parameters for use in Chillers
INTEGER, PARAMETER :: AirCooled = 1
INTEGER, PARAMETER :: WaterCooled = 2
INTEGER, PARAMETER :: EvapCooled = 3
REAL(r64),    PARAMETER :: KJtoJ = 1000.d0        !convert Kjoules to joules

          ! MODULE VARIABLE DECLARATIONS:
INTEGER                  :: NumGTChillers    =0   ! number of GT chillers specified in input
REAL(r64)                :: CondMassFlowRate =0.0 ! Kg/s - condenser mass flow rate, water side
REAL(r64)                :: EvapMassFlowRate =0.0 ! Kg/s - evaporator mass flow rate, water side
REAL(r64)                :: CondOutletTemp   =0.0 ! C - condenser outlet temperature, water side
REAL(r64)                :: EvapOutletTemp   =0.0 ! C - evaporator outlet temperature, water side
REAL(r64)                :: Power            =0.0 ! W - rate of chiller energy use
REAL(r64)                :: QEvaporator      =0.0 ! W - rate of heat transfer to the evaporator coil
REAL(r64)                :: QCondenser       =0.0 ! W - rate of heat transfer to the condenser coil
REAL(r64)                :: Energy           =0.0 ! J - chiller energy use
REAL(r64)                :: EvaporatorEnergy =0.0 ! J - heat transfer to the evaporator coil
REAL(r64)                :: CondenserEnergy  =0.0 ! N - heat transfer to the condenser coil
REAL(r64)                :: ChillerCyclingRatio =0.0 ! Cycling ratio for chiller when load is below MinPLR
REAL(r64)                :: BasinHeaterPower       =0.0 ! Basin heater power (W)

TYPE GTChillerSpecs
  CHARACTER(len=MaxNameLength) :: Name     =' '  ! user identifier
  CHARACTER(len=MaxNameLength) :: FuelType =' '  ! Type of Fuel - DIESEL, GASOLINE, GAS
  INTEGER           :: CondenserType       =0    ! Type of Condenser - Air or Water Cooled
  REAL(r64)         :: NomCap              =0.0  ! W - design nominal capacity of chiller
  REAL(r64)         :: COP                 =0.0  ! coefficient of performance
  LOGICAL           :: ConstantFlow     =.false. ! True if this is a Constant Flow Chiller
  LOGICAL           :: VariableFlow     =.false. ! True if this is a Variable Flow Chiller
  LOGICAL           :: VariableFlowSetToLoop = .FALSE.  ! True if the setpoint is missing at the outlet node
  LOGICAL           :: VariableFlowErrDone   = .FALSE.  ! true if setpoint warning issued
  REAL(r64)         :: EvapVolFlowRate     =0.0  ! m**3/s - design nominal water volumetric flow rate through the evaporator
  REAL(r64)         :: EvapMassFlowRateMax =0.0  ! kg/s - design water mass flow rate through evaporator
  REAL(r64)         :: CondVolFlowRate     =0.0  ! m**3/s - design nominal water volumetric flow rate through the condenser
  REAL(r64)         :: CondMassFlowRateMax =0.0  ! kg/s - design water mass flow rate through condenser
  INTEGER           :: EvapInletNodeNum    =0    ! Node number on the inlet side of the plant
  INTEGER           :: EvapOutletNodeNum   =0    ! Node number on the outlet side of the plant
  INTEGER           :: CondInletNodeNum    =0    ! Node number on the inlet side of the condenser
  INTEGER           :: CondOutletNodeNum   =0    ! Node number on the outlet side of the condenser

  REAL(r64)         :: MinPartLoadRat      =0.0  ! (GT MIN) min allowed operating frac full load
  REAL(r64)         :: MaxPartLoadRat      =0.0  ! (GT MAX) max allowed operating frac full load
  REAL(r64)         :: OptPartLoadRat      =0.0  ! (GT BEST) optimal operating frac full load
  REAL(r64)         :: TempDesCondIn       =0.0  ! C - (GT ADJTC(1)The design secondary loop fluid
                                                 ! temperature at the chiller condenser side inlet
  REAL(r64)         :: TempRiseCoef        =0.0  ! (GT ADJTC(2)) correction factor for off ChillDesign oper.
  REAL(r64)         :: TempDesEvapOut      =0.0  ! C - (GT ADJTC(3)The design primary loop fluid
                                                 ! temperature at the chiller evaporator side outlet
  REAL(r64),DIMENSION(3) :: CapRatCoef     =0.0  ! (GT RCAVC() ) coeff of cap ratio poly fit
  REAL(r64),DIMENSION(3) :: PowerRatCoef   =0.0  ! (GT ADJEC() ) coeff of power rat poly fit
  REAL(r64),DIMENSION(3) :: FullLoadCoef   =0.0  ! (GT RPWRC() ) coeff of full load poly. fit
  REAL(r64)         :: TempLowLimitEvapOut =0.0  ! C - low temperature shut off
  REAL(r64)         :: SizFac              =0.0  ! sizing factor
  REAL(r64)         :: BasinHeaterPowerFTempDiff = 0.0 ! Basin heater capacity per degree C below set point (W/C)
  REAL(r64)         :: BasinHeaterSetPointTemp   = 0.0 ! Set point temperature for basin heater operation (C)
  INTEGER           :: ErrCount1           =0    ! error counter
  INTEGER           :: ErrCount2           =0    ! error counter
  INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
  INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
  INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
  INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
  INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
  INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
  INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
  INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
  INTEGER           :: BasinHeaterSchedulePtr  = 0   ! Pointer to basin heater schedule
  INTEGER           :: CondMassFlowIndex = 0
  CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
  CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
  REAL(r64)         :: MsgDataLast   = 0.0 ! value of data when warning occurred (passed to Recurring Warn)
  LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
  INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
  LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested

END TYPE GTChillerSpecs


TYPE SpecialGTChillerSpecs
! Special GT Chiller Input Parameters

  REAL(r64)         :: FuelEnergyIn            =0.0 !(EFUEL) Amount of Fuel Energy Required to run gas turbine
  REAL(r64),DIMENSION(3) :: PLBasedFuelInputCoef    =0.0 !(FUL1GC) Part Load Ratio Based Fuel Input Coefficients Poly Fit
  REAL(r64),DIMENSION(3) :: TempBasedFuelInputCoef  =0.0 !(FUL2GC) Ambient Temperature Based Fuel Input Coeff Poly Fit

  REAL(r64)         :: ExhaustFlow             =0.0  !(FEX) Exhaust Gas Flow Rate cubic meters per second
  REAL(r64),DIMENSION(3) :: ExhaustFlowCoef    =0.0  !(FEXGC) Exhaust Gas Flow Rate Input Coef Poly Fit

  REAL(r64)         :: ExhaustTemp             =0.0  !(TEX) Exhaust Gas Temperature in C
  REAL(r64),DIMENSION(3) :: PLBasedExhaustTempCoef  =0.0  !(TEX1GC) Part Load Ratio Based Exhaust Temperature Input Coeffs Poly Fit
  REAL(r64),DIMENSION(3) :: TempBasedExhaustTempCoef=0.0  !(TEX2GC) Ambient Temperature Based Exhaust Gas Temp to
                                                          ! Fuel Energy Input Coeffs Poly Fit

  REAL(r64)         :: HeatRecLubeEnergy       =0.0  !(ELUBE) Recoverable Lube Oil Energy
  REAL(r64)         :: HeatRecLubeRate         =0.0  !(ELUBE) Recoverable Lube Oil Rate of Rwecovery (W)
  REAL(r64),DIMENSION(3) :: HeatRecLubeEnergyCoef   =0.0  !(ELUBEGC)  Recoverable Lube Oil Energy Input Coef Poly Fit

  REAL(r64)         :: UAtoCapRat              =0.0  !(UACGC) Heat Exchanger UA to Capacity
  REAL(r64),DIMENSION(3) :: UAtoCapCoef        =0.0  !Heat Exchanger UA to Capacity Coeffs Poly Fit

  REAL(r64)         :: GTEngineCapacity        =0.0  ! Capacity of GT Unit attached to Chiller
  REAL(r64)         :: MaxExhaustperGTPower    =0.0  !Max Exhaust Flow per KW Power Out
  REAL(r64)         :: DesignSteamSatTemp      =0.0  !Steam Saturation Temperature
  REAL(r64)         :: ExhaustStackTemp        =0.0  !Temperature of Exhaust Gases

  INTEGER           :: HeatRecInletNodeNum     =0    ! Node number on the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum    =0    ! Node number on the heat recovery outlet side of the condenser

  REAL(r64)         :: HeatRecInletTemp        =0.0  !Inlet Temperature of the heat recovery fluid
  REAL(r64)         :: HeatRecOutletTemp       =0.0  !Outlet Temperature of the heat recovery fluid
  REAL(r64)         :: HeatRecMdot             =0.0  ! reporting: Heat Recovery Loop Mass flow rate
  REAL(r64)         :: DesignHeatRecVolFlowRate=0.0    ! m3/s, Design Water mass flow rate through heat recovery loop
  REAL(r64)         :: DesignHeatRecMassFlowRate=0.0   ! kg/s, Design Water mass flow rate through heat recovery loop
  LOGICAL           :: HeatRecActive        =.false. ! True entered Heat Rec Vol Flow Rate >0
  REAL(r64)         :: FuelHeatingValue        =0.0   !Heating Value of Fuel in kJ/kg
  REAL(r64)         :: HeatRecMaxTemp          =0.0  !Max Temp that can be produced in heat recovery
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index

END TYPE SpecialGTChillerSpecs



TYPE ReportVars
  REAL(r64)    :: Power                = 0.0 ! reporting: power
  REAL(r64)    :: QEvap                = 0.0 ! reporting: evaporator heat transfer
  REAL(r64)    :: QCond                = 0.0 ! reporting: condensor heat transfer
  REAL(r64)    :: Energy               = 0.0 ! reporting: power
  REAL(r64)    :: EvapEnergy           = 0.0 ! reporting: evaporator heat transfer
  REAL(r64)    :: CondEnergy           = 0.0 ! reporting: condensor heat transfer
  REAL(r64)    :: CondInletTemp        = 0.0 ! reporting: condenser inlet temperature
  REAL(r64)    :: EvapInletTemp        = 0.0 ! reporting: evaporator inlet temperature
  REAL(r64)    :: CondOutletTemp       = 0.0 ! reporting: condenser outlet temperature
  REAL(r64)    :: EvapOutletTemp       = 0.0 ! reporting: evaporator outlet temperature
  REAL(r64)    :: Evapmdot             = 0.0 ! reporting: evaporator mass flow rate
  REAL(r64)    :: Condmdot             = 0.0 ! reporting: condenser mass flow rate

  REAL(r64)    :: HeatRecLubeEnergy    = 0.0 ! reporting: Heat Recovered from Lubricant(J)
  REAL(r64)    :: HeatRecLubeRate      = 0.0 ! reporting: Recoverable Lube Oil Rate of Rwecovery (W)
  REAL(r64)    :: FuelEnergyUsed       = 0.0 ! reporting: Fuel Energy used
  REAL(r64)    :: FuelEnergyUsedRate   = 0.0 ! reporting: Fuel energy used rate (fuel consumption rate)
  REAL(r64)    :: FuelMassUsed         = 0.0 ! reporting: Fuel Amount used
  REAL(r64)    :: FuelMassUsedRate     = 0.0 ! reporting: Fuel amount used (fuel Mass consumption rate)
  REAL(r64)    :: ExhaustStackTemp     = 0.0 ! reporting: Exhaust Stack Temperature

  REAL(r64)    :: HeatRecInletTemp     = 0.0 ! reporting: Heat Recovery Loop Inlet Temperature
  REAL(r64)    :: HeatRecOutletTemp    = 0.0 ! reporting: Heat Recovery Loop Outlet Temperature
  REAL(r64)    :: HeatRecMdot          = 0.0 ! reporting: Heat Recovery Loop Mass flow rate

  REAL(r64)    :: FuelCOP              = 0.0 ! reporting: Fuel coefficient of performance (Qevap/FuelEnergyUsedRate)
  REAL(r64)    :: BasinHeaterPower       = 0.0  ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0  ! Basin heater energy consumption (J)


END TYPE ReportVars

TYPE (GTChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: GTChiller  !dimension to number of machines

TYPE (SpecialGTChillerSpecs), ALLOCATABLE, DIMENSION(:) :: GTChillerSpecial

TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::GTChillerReport

LOGICAL     :: GetInput = .TRUE.! then TRUE, calls subroutine to read input file.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
PRIVATE    CalcGTChillerModel
PRIVATE    GetGTChillerInput
PRIVATE    InitGTChiller
PRIVATE    SizeGTChiller
PRIVATE    UpdateGTChillerRecords
PUBLIC     SimGTChiller
!PUBLIC     SimGTChillerHeatRecovery

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of GT Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimGTChiller(LoopNum, LoopSide, ChillerType,ChillerName,EquipFlowCtrl, CompIndex,RunFlag,FirstHVACIteration, &
                        InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the GT chiller model driver.  It
               ! gets the input for the models, initializes simulation variables, call
               ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide, UpdateComponentHeatRecoverySide

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: LoopNum             ! Flow control mode for the equipment
  INTEGER, INTENT(IN)          :: LoopSide            ! chiller number pointer
  CHARACTER(len=*), INTENT(IN) :: ChillerType         ! type of chiller !unused1208
  CHARACTER(len=*), INTENT(IN) :: ChillerName         ! user specified name of chiller
  INTEGER, INTENT(IN)          :: EquipFlowCtrl       ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)       :: CompIndex           ! chiller number pointer
  LOGICAL , INTENT(IN)         :: RunFlag             ! simulate chiller when TRUE
  LOGICAL , INTENT(IN)         :: FirstHVACIteration  ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip       ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad              ! loop demand component will meet
  REAL(r64)                    :: MinCap              ! W - minimum operating capacity of chiller
  REAL(r64)                    :: MaxCap              ! W - maximum operating capacity of chiller
  REAL(r64)                    :: OptCap              ! W - optimal operating capacity of chiller
  LOGICAL, INTENT(IN)          :: GetSizingFactor     ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(OUT)       :: SizingFactor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ChillNum            ! chiller number pointer

          !Get chiller data from input file
  IF (GetInput) THEN
    CALL GetGTChillerInput
    GetInput = .FALSE.
  END IF

  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(ChillerName,GTChiller%Name,NumGTChillers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimGTChiller: Specified Chiller not one of Valid Gas Turbine Chillers='//TRIM(ChillerName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumGTChillers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimGTChiller:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumGTChillers))//  &
                          ', Entered Unit name='//TRIM(ChillerName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (ChillerName /= GTChiller(ChillNum)%Name) THEN
        CALL ShowFatalError('SimGTChiller: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                            TRIM(GTChiller(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF



  IF (InitLoopEquip) THEN
    CALL InitGTChiller(ChillNum,RunFlag, MyLoad,FirstHVACIteration)
    CALL SizeGTChiller(ChillNum)
    IF (LoopNum == GTChiller(ChillNum)%CWLoopNum) THEN
      MinCap = GTChiller(ChillNum)%NomCap*GTChiller(ChillNum)%MinPartLoadRat
      MaxCap = GTChiller(ChillNum)%NomCap*GTChiller(ChillNum)%MaxPartLoadRat
      OptCap = GTChiller(ChillNum)%NomCap*GTChiller(ChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = GTChiller(ChillNum)%SizFac
    END IF
    RETURN
  END IF

    ! calculate model depending on where called from
  IF (LoopNum == GTChiller(ChillNum)%CWLoopNum) THEN ! chilled water loop

    CALL InitGTChiller(ChillNum,RunFlag, MyLoad,FirstHVACIteration)
    CALL CalcGTChillerModel(ChillNum,MyLoad,Runflag,FirstHVACIteration,EquipFlowCtrl)
    CALL UpdateGTChillerRecords(MyLoad,RunFlag,ChillNum)

  ELSEIF (LoopNum == GTChiller(ChillNum)%CDLoopNum) THEN ! condenser loop
    CALL UpdateChillerComponentCondenserSide(GTChiller(ChillNum)%CDLoopNum, &
                                     GTChiller(ChillNum)%CDLoopSideNum,     &
                                     TypeOf_Chiller_CombTurbine,                     &
                                     GTChiller(ChillNum)%CondInletNodeNum,  &
                                     GTChiller(ChillNum)%CondOutletNodeNum, &
                                     GTChillerReport(ChillNum)%QCond,             &
                                     GTChillerReport(ChillNum)%CondInletTemp,     &
                                     GTChillerReport(ChillNum)%CondOutletTemp,    &
                                     GTChillerReport(ChillNum)%Condmdot,          &
                                     FirstHVACIteration)
  ELSEIF (LoopNum == GTChillerSpecial(ChillNum)%HRLoopNum) THEN  ! heat recovery loop
    CALL UpdateComponentHeatRecoverySide(GTChillerSpecial(ChillNum)%HRLoopNum,               &
                                    GTChillerSpecial(ChillNum)%HRLoopSideNum,           &
                                    TypeOf_Chiller_CombTurbine,                           &
                                    GTChillerSpecial(ChillNum)%HeatRecInletNodeNum,     &
                                    GTChillerSpecial(ChillNum)%HeatRecOutletNodeNum,    &
                                    GTChillerReport(ChillNum)%HeatRecLubeRate,      &
                                    GTChillerReport(ChillNum)%HeatRecInletTemp,  &
                                    GTChillerReport(ChillNum)%HeatRecOutletTemp, &
                                    GTChillerReport(ChillNum)%HeatRecMdot ,  &
                                    FirstHVACIteration)
  ENDIF

RETURN
END SUBROUTINE SimGTChiller

! End GT Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of GT Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetGTChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the GT Chiller model.


            ! METHODOLOGY EMPLOYED:
            ! EnergyPlus input processor

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General,           ONLY: RoundSigDigits
  USE PlantUtilities,    ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,    ONLY: GetScheduleIndex

  IMPLICIT NONE !

            ! PARAMETERS

            !LOCAL VARIABLES
  INTEGER                     :: ChillerNum !chiller counter
  INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
  INTEGER                     :: NumNums    ! Number of elements in the numeric array
  INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay

         !FLOW
  cCurrentModuleObject = 'Chiller:CombustionTurbine'
  NumGTChillers = GetNumObjectsFound(TRIM(cCurrentModuleObject))

  IF (NumGTChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF
            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(GTChiller))RETURN

         !ALLOCATE ARRAYS
  ALLOCATE (GTChiller(NumGTChillers))

  ALLOCATE (GTChillerSpecial(NumGTChillers))

  ALLOCATE (GTChillerReport(NumGTChillers))
  ALLOCATE(CheckEquipName(NumGTChillers))
  CheckEquipName=.true.

  DO ChillerNum = 1 , NumGTChillers
    CALL GetObjectItem(TRIM(cCurrentModuleObject),ChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),GTChiller%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    GTChiller(ChillerNum)%Name                = cAlphaArgs(1)

    GTChiller(ChillerNum)%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    GTChiller(ChillerNum)%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    IF (cAlphaArgs(2) == 'AIRCOOLED' ) THEN
      GTChiller(ChillerNum)%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(2) == 'WATERCOOLED') THEN
      GTChiller(ChillerNum)%CondenserType       = WaterCooled
    ELSEIF (cAlphaArgs(2) == 'EVAPORATIVELYCOOLED') THEN
      GTChiller(ChillerNum)%CondenserType       = EvapCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF


    GTChiller(ChillerNum)%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    GTChiller(ChillerNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

    IF (GTChiller(ChillerNum)%CondenserType == AirCooled .or. GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
      ! If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      ! since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(5))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(6) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(6) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(6) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      GTChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(GTChiller(ChillerNum)%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(5)))
      ENDIF

      GTChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSE ! WaterCooled CondenserType
      GTChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      GTChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(6) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(6))//' is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    GTChiller(ChillerNum)%MinPartLoadRat      = rNumericArgs(3)
    GTChiller(ChillerNum)%MaxPartLoadRat      = rNumericArgs(4)
    GTChiller(ChillerNum)%OptPartLoadRat      = rNumericArgs(5)
    GTChiller(ChillerNum)%TempDesCondIn       = rNumericArgs(6)
    GTChiller(ChillerNum)%TempRiseCoef        = rNumericArgs(7)
    GTChiller(ChillerNum)%TempDesEvapOut      = rNumericArgs(8)
    GTChiller(ChillerNum)%EvapVolFlowRate     = rNumericArgs(9)
    GTChiller(ChillerNum)%CondVolFlowRate     = rNumericArgs(10)
    GTChiller(ChillerNum)%CapRatCoef(1)       = rNumericArgs(11)
    GTChiller(ChillerNum)%CapRatCoef(2)       = rNumericArgs(12)
    GTChiller(ChillerNum)%CapRatCoef(3)       = rNumericArgs(13)
    IF ((rNumericArgs(11)+rNumericArgs(12)+rNumericArgs(13)) == 0.0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Sum of Capacity Ratio Coef = 0.0, chiller='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    GTChiller(ChillerNum)%PowerRatCoef(1)     = rNumericArgs(14)
    GTChiller(ChillerNum)%PowerRatCoef(2)     = rNumericArgs(15)
    GTChiller(ChillerNum)%PowerRatCoef(3)     = rNumericArgs(16)
    GTChiller(ChillerNum)%FullLoadCoef(1)     = rNumericArgs(17)
    GTChiller(ChillerNum)%FullLoadCoef(2)     = rNumericArgs(18)
    GTChiller(ChillerNum)%FullLoadCoef(3)     = rNumericArgs(19)
    GTChiller(ChillerNum)%TempLowLimitEvapOut = rNumericArgs(20)


    !Load Special GT Chiller Input

    GTChillerSpecial(ChillerNum)%PLBasedFuelInputCoef(1) = rNumericArgs(21)
    GTChillerSpecial(ChillerNum)%PLBasedFuelInputCoef(2) = rNumericArgs(22)
    GTChillerSpecial(ChillerNum)%PLBasedFuelInputCoef(3) = rNumericArgs(23)

    GTChillerSpecial(ChillerNum)%TempBasedFuelInputCoef(1) = rNumericArgs(24)
    GTChillerSpecial(ChillerNum)%TempBasedFuelInputCoef(2) = rNumericArgs(25)
    GTChillerSpecial(ChillerNum)%TempBasedFuelInputCoef(3) = rNumericArgs(26)

    GTChillerSpecial(ChillerNum)%ExhaustFlowCoef(1) = rNumericArgs(27)
    GTChillerSpecial(ChillerNum)%ExhaustFlowCoef(2) = rNumericArgs(28)
    GTChillerSpecial(ChillerNum)%ExhaustFlowCoef(3) = rNumericArgs(29)

    GTChillerSpecial(ChillerNum)%PLBasedExhaustTempCoef(1) = rNumericArgs(30)
    GTChillerSpecial(ChillerNum)%PLBasedExhaustTempCoef(2) = rNumericArgs(31)
    GTChillerSpecial(ChillerNum)%PLBasedExhaustTempCoef(3) = rNumericArgs(32)

    GTChillerSpecial(ChillerNum)%TempBasedExhaustTempCoef(1) = rNumericArgs(33)
    GTChillerSpecial(ChillerNum)%TempBasedExhaustTempCoef(2) = rNumericArgs(34)
    GTChillerSpecial(ChillerNum)%TempBasedExhaustTempCoef(3) = rNumericArgs(35)

    GTChillerSpecial(ChillerNum)%HeatRecLubeEnergyCoef(1) = rNumericArgs(36)
    GTChillerSpecial(ChillerNum)%HeatRecLubeEnergyCoef(2) = rNumericArgs(37)
    GTChillerSpecial(ChillerNum)%HeatRecLubeEnergyCoef(3) = rNumericArgs(38)

    GTChillerSpecial(ChillerNum)%UAtoCapCoef(1) = rNumericArgs(39)
    GTChillerSpecial(ChillerNum)%UAtoCapCoef(2) = rNumericArgs(40)

    GTChillerSpecial(ChillerNum)%GTEngineCapacity = rNumericArgs(41)
    GTChillerSpecial(ChillerNum)%MaxExhaustperGTPower = rNumericArgs(42)
    GTChillerSpecial(ChillerNum)%DesignSteamSatTemp = rNumericArgs(43)
    GTChillerSpecial(ChillerNum)%FuelHeatingValue = rNumericArgs(44)

    !Get the Heat Recovery information
    GTChillerSpecial(ChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(45)
    IF (GTChillerSpecial(ChillerNum)%DesignHeatRecVolFlowRate > 0.0) THEN
      GTChillerSpecial(ChillerNum)%HeatRecActive=.true.
      GTChillerSpecial(ChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (GTChillerSpecial(ChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      GTChillerSpecial(ChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (GTChillerSpecial(ChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(7),cAlphaArgs(8),'Heat Recovery Nodes')
      CALL RegisterPlantCompDesignFlow(GTChillerSpecial(ChillerNum)%HeatRecInletNodeNum, &
                                GTChillerSpecial(ChillerNum)%DesignHeatRecVolFlowRate )
      ! Condenser flow rate must be specified for heat reclaim
      IF (GTChiller(ChillerNum)%CondenserType == AirCooled .OR. &
          GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        IF(GTChiller(ChillerNum)%CondVolFlowRate .LE. 0.0)THEN
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(10))//'='//TRIM(RoundSigDigits(rNumericArgs(10),6)))
          CALL ShowSevereError('Condenser fluid flow rate must be specified for Heat Reclaim applications.')
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        END IF
      END IF
    ELSE
      GTChillerSpecial(ChillerNum)%HeatRecActive=.false.
      GTChillerSpecial(ChillerNum)%DesignHeatRecMassFlowRate = 0.0
      GTChillerSpecial(ChillerNum)%HeatRecInletNodeNum    = 0
      GTChillerSpecial(ChillerNum)%HeatRecOutletNodeNum   = 0
      IF ((.NOT. lAlphaFieldBlanks(7))  .OR. (.NOT. lAlphaFieldBlanks(8))) THEN
        CALL ShowWarningError('Since Design Heat Flow Rate = 0.0, Heat Recovery inactive for '// &
                    TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('However, Node names were specified for heat recovery inlet or outlet nodes')
      ENDIF
      IF (GTChiller(ChillerNum)%CondenserType == AirCooled .OR. &
          GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        GTChiller(ChillerNum)%CondVolFlowRate = 0.0011d0  ! set to avoid errors in calc routine
      END IF
    ENDIF

    If(cAlphaArgs(9) .eq. 'CONSTANTFLOW') Then
       GTChiller(ChillerNum)%ConstantFlow = .True.
       GTChiller(ChillerNum)%VariableFlow = .False.
    Else If(cAlphaArgs(9) .eq. 'VARIABLEFLOW') Then
       GTChiller(ChillerNum)%ConstantFlow = .False.
       GTChiller(ChillerNum)%VariableFlow = .True.
    Else  ! We will assume a variable flow chiller is none specified
       GTChiller(ChillerNum)%ConstantFlow = .False.
       GTChiller(ChillerNum)%VariableFlow = .True.
    End If

    !Fuel Type Case Statement
    SELECT CASE (cAlphaArgs(10))
    CASE ('GAS','NATURALGAS','NATURAL GAS')
      GTChiller(ChillerNum)%FuelType = 'Gas'

    CASE ('DIESEL')
      GTChiller(ChillerNum)%FuelType = 'Diesel'

    CASE ('GASOLINE')
      GTChiller(ChillerNum)%FuelType = 'Gasoline'

    CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
       GTChiller(ChillerNum)%FuelType = 'FuelOil#1'

    CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
       GTChiller(ChillerNum)%FuelType = 'FuelOil#2'

    CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
       GTChiller(ChillerNum)%FuelType = 'Propane'

    CASE DEFAULT
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END SELECT


    GTChillerSpecial(ChillerNum)%HeatRecMaxTemp = rNumericArgs(46)
    GTChiller(ChillerNum)%SizFac = rNumericArgs(47)
    IF (GTChiller(ChillerNum)%SizFac <= 0.0) GTChiller(ChillerNum)%SizFac = 1.0d0

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    GTChiller(ChillerNum)%BasinHeaterPowerFTempDiff = rNumericArgs(48)
    IF(rNumericArgs(48) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(GTChiller(ChillerNum)%Name)//&
                     '" TRIM(cNumericFieldNames(48)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    GTChiller(ChillerNum)%BasinHeaterSetPointTemp = rNumericArgs(49)

    IF(GTChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 49) THEN
        GTChiller(ChillerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(GTChiller(ChillerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(GTChiller(ChillerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(49))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(11))THEN
      GTChiller(ChillerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(11))
      IF(GTChiller(ChillerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(GTChiller(ChillerNum)%Name)//&
                       '" TRIM(cAlphaFieldNames(11)) "'//TRIM(cAlphaArgs(11)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '// TRIM(cCurrentModuleObject) )
  ENDIF

  DO ChillerNum = 1, NumGTChillers
     CALL SetupOutputVariable('Chiller Shaft Power [W]', &
          GTChillerReport(ChillerNum)%Power,'System','Average',GTChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Shaft Energy [J]', &
          GTChillerReport(ChillerNum)%Energy,'System','Sum',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Evap Heat Trans Rate [W]', &
          GTChillerReport(ChillerNum)%QEvap,'System','Average',GTChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Heat Trans [J]', &
          GTChillerReport(ChillerNum)%EvapEnergy,'System','Sum',GTChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evap Water Inlet Temp [C]', &
          GTChillerReport(ChillerNum)%EvapInletTemp,'System','Average',GTChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Outlet Temp [C]', &
          GTChillerReport(ChillerNum)%EvapOutletTemp,'System','Average',GTChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Mass Flow Rate [kg/s]', &
          GTChillerReport(ChillerNum)%Evapmdot,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Cond Heat Trans Rate [W]', &
          GTChillerReport(ChillerNum)%QCond,'System','Average',GTChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cond Heat Trans [J]', &
          GTChillerReport(ChillerNum)%CondEnergy,'System','Sum',GTChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

        !Condenser mass flow and outlet temp are valid for water cooled
     IF (GTChiller(ChillerNum)%CondenserType == WaterCooled)THEN
        CALL SetupOutputVariable('Chiller Cond Water Inlet Temp [C]', &
             GTChillerReport(ChillerNum)%CondInletTemp,'System','Average',GTChiller(ChillerNum)%Name)
        CALL SetupOutputVariable('Chiller Cond Water Outlet Temp [C]', &
             GTChillerReport(ChillerNum)%CondOutletTemp,'System','Average',GTChiller(ChillerNum)%Name)
        CALL SetupOutputVariable('Chiller Cond Water Mass Flow Rate [kg/s]', &
             GTChillerReport(ChillerNum)%Condmdot,'System','Average',GTChiller(ChillerNum)%Name)
     ELSEIF (GTChiller(ChillerNum)%CondenserType == AirCooled) THEN
        CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
             GTChillerReport(ChillerNum)%CondInletTemp,'System','Average',GTChiller(ChillerNum)%Name)
     ELSEIF (GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
             GTChillerReport(ChillerNum)%CondInletTemp,'System','Average',GTChiller(ChillerNum)%Name)
        IF(GTChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
          CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
               GTChillerReport(ChillerNum)%BasinHeaterPower,'System','Average',GTChiller(ChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Basin Heater Electric Consumption [J]', &
               GTChillerReport(ChillerNum)%BasinHeaterConsumption,'System','Sum',GTChiller(ChillerNum)%Name, &
               ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
        END IF
     ENDIF

     CALL SetupOutputVariable('Chiller Lube Heat Recovery Rate [W]', &
          GTChillerReport(ChillerNum)%HeatRecLubeRate,'System','Average',GTChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Lube Heat Recovery [J]', &
          GTChillerReport(ChillerNum)%HeatRecLubeEnergy,'System','Sum',GTChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HeatRecovery',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Consumption Rate [W]', &
          GTChillerReport(ChillerNum)%FuelEnergyUsedRate,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Consumption [J]', &
          GTChillerReport(ChillerNum)%FuelEnergyUsed,'System','Sum',GTChiller(ChillerNum)%Name,  &
                              ResourceTypeKey=GTChiller(ChillerNum)%FuelType,EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Mass Flow Rate [kg/s]', &
          GTChillerReport(ChillerNum)%FuelMassUsedRate,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller '// TRIM(GTChiller(ChillerNum)%FuelType)//' Mass Consumption [kg]', &
          GTChillerReport(ChillerNum)%FuelMassUsed,'System','Sum',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Exhaust Stack Temp[C]', &
          GTChillerReport(ChillerNum)%ExhaustStackTemp,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temp [C]', &
          GTChillerReport(ChillerNum)%HeatRecInletTemp,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temp [C]', &
          GTChillerReport(ChillerNum)%HeatRecOutletTemp,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
          GTChillerReport(ChillerNum)%HeatRecMdot,'System','Average',GTChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Fuel COP [W/W]', &
          GTChillerReport(ChillerNum)%FuelCOP,'System','Average',GTChiller(ChillerNum)%Name)

  END DO



RETURN
END SUBROUTINE GetGTChillerInput

! End of Get Input subroutines for the GT Chiller Module
!******************************************************************************

SUBROUTINE InitGTChiller(ChillNum,RunFlag, MyLoad, FirstHVACIteration )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Gas Turbine Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_CombTurbine, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current engine driven chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad
  LOGICAL, INTENT(IN)  :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitGTChiller'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyFlag
  INTEGER   :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER   :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER   :: EvapInletNode
  INTEGER   :: EvapOutletNode
  INTEGER   :: HeatRecInNode
  INTEGER   :: HeatRecOutNode
  REAL(r64) :: rho       ! local fluid density
  REAL(r64) :: mdot      ! local mass flow rate
  REAL(r64) :: mdotCond  ! local mass flow rate for condenser
  INTEGER   :: InletNode
  INTEGER   :: OutletNode
  INTEGER   :: LoopNum
  INTEGER   :: LoopSideNum
  INTEGER   :: BranchIndex
  INTEGER   :: CompIndex
  LOGICAL   :: FatalError
  LOGICAL :: errFlag
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumGTChillers))
    ALLOCATE(MyEnvrnFlag(NumGTChillers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  CondInletNode  = GTChiller(ChillNum)%CondInletNodeNum
  CondOutletNode = GTChiller(ChillNum)%CondOutletNodeNum
  EvapInletNode  = GTChiller(ChillNum)%EvapInletNodeNum
  EvapOutletNode = GTChiller(ChillNum)%EvapOutletNodeNum

  IF (GTChillerSpecial(ChillNum)%HeatRecActive) THEN
    HeatRecInNode  = GTChillerSpecial(ChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = GTChillerSpecial(ChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(GTChiller(ChillNum)%Name, &
                                 TypeOf_Chiller_CombTurbine, &
                                 GTChiller(ChillNum)%CWLoopNum, &
                                 GTChiller(ChillNum)%CWLoopSideNum, &
                                 GTChiller(ChillNum)%CWBranchNum, &
                                 GTChiller(ChillNum)%CWCompNum, &
                                 LowLimitTemp = GTChiller(ChillNum)%TempLowLimitEvapOut , &
                                 InletNodeNumber = GTChiller(ChillNum)%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (GTChiller(ChillNum)%CondenserType /= AirCooled .AND. &
        GTChiller(ChillNum)%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(GTChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_CombTurbine, &
                                   GTChiller(ChillNum)%CDLoopNum, &
                                   GTChiller(ChillNum)%CDLoopSideNum, &
                                   GTChiller(ChillNum)%CDBranchNum, &
                                   GTChiller(ChillNum)%CDCompNum, &
                                   InletNodeNumber = GTChiller(ChillNum)%CondInletNodeNum,  &
                                   errFlag=errFlag)
       CALL InterConnectTwoPlantLoopSides( GTChiller(ChillNum)%CWLoopNum,     &
                                          GTChiller(ChillNum)%CWLoopSideNum,  &
                                          GTChiller(ChillNum)%CDLoopNum,      &
                                          GTChiller(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_CombTurbine, .TRUE. )
    ENDIF
    IF (GTChillerSpecial(ChillNum)%HeatRecActive) THEN
      CALL ScanPlantLoopsForObject(GTChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_CombTurbine, &
                                   GTChillerSpecial(ChillNum)%HRLoopNum, &
                                   GTChillerSpecial(ChillNum)%HRLoopSideNum, &
                                   GTChillerSpecial(ChillNum)%HRBranchNum, &
                                   GTChillerSpecial(ChillNum)%HRCompNum, &
                                   InletNodeNumber = GTChillerSpecial(ChillNum)%HeatRecInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( GTChiller(ChillNum)%CWLoopNum,      &
                                          GTChiller(ChillNum)%CWLoopSideNum,  &
                                          GTChillerSpecial(ChillNum)%HRLoopNum,     &
                                          GTChillerSpecial(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_CombTurbine , .TRUE. )
    ENDIF

    IF (GTChiller(ChillNum)%CondenserType /= AirCooled  .AND. &
        GTChiller(ChillNum)%CondenserType /= EvapCooled .AND. &
        GTChillerSpecial(ChillNum)%HeatRecActive) THEN
      CALL InterConnectTwoPlantLoopSides( GTChiller(ChillNum)%CDLoopNum,      &
                                          GTChiller(ChillNum)%CDLoopSideNum,  &
                                          GTChillerSpecial(ChillNum)%HRLoopNum,     &
                                          GTChillerSpecial(ChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_CombTurbine, .FALSE. )
    ENDIF
    IF (errFlag) THEN
      CALL ShowFatalError('InitGTChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (GTChiller(ChillNum)%VariableFlow) Then 
      ! reset flow priority
      PlantLoop(GTChiller(ChillNum)%CWLoopNum)%LoopSide(GTChiller(ChillNum)%CWLoopSideNum)% &
          Branch(GTChiller(ChillNum)%CWBranchNum)%Comp(GTChiller(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    
    ! check if setpoint on outlet node
      IF (Node(GTChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. GTChiller(ChillNum)%VariableFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(GTChiller(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            GTChiller(ChillNum)%VariableFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(GTChiller(ChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. GTChiller(ChillNum)%VariableFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(GTChiller(ChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Set Point Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              GTChiller(ChillNum)%VariableFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        GTChiller(ChillNum)%VariableFlowSetToLoop = .TRUE.
        Node(GTChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(GTChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
    ENDIF
    MyFlag(ChillNum)=.FALSE.
  ENDIF

  IF(MyEnvrnFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeGTChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(GTChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                RoutineName)

    GTChiller(ChillNum)%EvapMassFlowRateMax = rho * GTChiller(ChillNum)%EvapVolFlowRate
    CALL InitComponentNodes(0.0D0,GTChiller(ChillNum)%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         GTChiller(ChillNum)%CWLoopNum,               &
                         GTChiller(ChillNum)%CWLoopSideNum,           &
                         GTChiller(ChillNum)%CWBranchNum,             &
                         GTChiller(ChillNum)%CWCompNum)

          !init maximum available condenser flow rate
    IF (GTChiller(ChillNum)%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = GTChiller(ChillNum)%TempDesCondIn

      rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(GTChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      GTChiller(ChillNum)%CondMassFlowRateMax = rho * GTChiller(ChillNum)%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  GTChiller(ChillNum)%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         GTChiller(ChillNum)%CDLoopNum,               &
                         GTChiller(ChillNum)%CDLoopSideNum,           &
                         GTChiller(ChillNum)%CDBranchNum,             &
                         GTChiller(ChillNum)%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = GTChiller(ChillNum)%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,GTChiller(ChillNum)%TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0
      Node(CondInletNode)%MassFlowRateMin       = 0.0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0
    END IF

    IF (GTChillerSpecial(ChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(GTChillerSpecial(ChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(GTChillerSpecial(ChillNum)%HRLoopNum)%FluidIndex,&
                                  RoutineName)
      GTChillerSpecial(ChillNum)%DesignHeatRecMassFlowRate = rho * &
                                         GTChillerSpecial(ChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, GTChillerSpecial(ChillNum)%DesignHeatRecMassFlowRate ,  &
                         GTChillerSpecial(ChillNum)%HeatRecInletNodeNum,        &
                         GTChillerSpecial(ChillNum)%HeatRecOutletNodeNum,       &
                         GTChillerSpecial(ChillNum)%HRLoopNum,                  &
                         GTChillerSpecial(ChillNum)%HRLoopSideNum,              &
                         GTChillerSpecial(ChillNum)%HRBranchNum,                &
                         GTChillerSpecial(ChillNum)%HRCompNum)
    ENDIF

    MyEnvrnFlag(ChillNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ChillNum)=.true.
  ENDIF

  IF (GTChiller(ChillNum)%VariableFlow .AND. GTChiller(ChillNum)%VariableFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(GTChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(GTChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
  ENDIF

  IF (FirstHVACIteration) THEN
    IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
      mdot     = GTChiller(ChillNum)%EvapMassFlowRateMax
      mdotCond = GTChiller(ChillNum)%CondMassFlowRateMax
    ELSE
      mdot     = 0.d0
      mdotCond = 0.d0
    ENDIF
  ELSE
    IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
      mdot     = GTChillerReport(ChillNum)%Evapmdot
      mdotCond = GTChillerReport(ChillNum)%Condmdot
    ELSE
      mdot     = 0.d0
      mdotCond = 0.d0
    ENDIF
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              GTChiller(ChillNum)%CWLoopNum,     &
                              GTChiller(ChillNum)%CWLoopSideNum, &
                              GTChiller(ChillNum)%CWBranchNum,   &
                              GTChiller(ChillNum)%CWCompNum)
  IF (GTChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,         &
                              GTChiller(ChillNum)%CDLoopNum,     &
                              GTChiller(ChillNum)%CDLoopSideNum, &
                              GTChiller(ChillNum)%CDBranchNum,   &
                              GTChiller(ChillNum)%CDCompNum)
  ENDIF

  ! Initialize heat recovery flow rates at node
  IF (GTChillerSpecial(ChillNum)%HeatRecActive ) THEN

    InletNode    =  GTChillerSpecial(ChillNum)%HeatRecInletNodeNum
    OutletNode   =  GTChillerSpecial(ChillNum)%HeatRecOutletNodeNum
    LoopNum      =  GTChillerSpecial(ChillNum)%HRLoopNum
    LoopSideNum  =  GTChillerSpecial(ChillNum)%HRLoopSideNum
    BranchIndex  =  GTChillerSpecial(ChillNum)%HRBranchNum
    CompIndex    =  GTChillerSpecial(ChillNum)%HRCompNum

    If (FirstHVACIteration .AND. RunFlag) Then
      mdot = GTChillerSpecial(ChillNum)%DesignHeatRecMassFlowRate
    ELSEIF (FirstHVACIteration .AND. (.NOT. RunFlag)) Then
      mdot = 0.d0
    ELSEIF ((.NOT. FirstHVACIteration) .AND. RunFlag) THEN
      mdot = GTChillerReport(ChillNum)%HeatRecMdot
    ELSEIF ((.NOT. FirstHVACIteration) .AND. (.NOT. RunFlag)) THEN
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF
  IF (GTChiller(ChillNum)%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF
  RETURN

END SUBROUTINE InitGTChiller

SUBROUTINE SizeGTChiller(ChillNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Gas Turbine Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  REAL(r64)           :: EngineEff     ! this should be an input! needed to autosize the engine capacity.
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho  ! local fluid density
  REAL(r64)           ::  Cp   ! local fluid specific heat
  REAL(r64)           ::  tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           ::  tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           ::  tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  EngineEff = 0.35d0
  ErrorsFound = .FALSE.
  tmpNomCap          = GTChiller(ChillNum)%NomCap
  tmpEvapVolFlowRate = GTChiller(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = GTChiller(ChillNum)%CondVolFlowRate

  IF (GTChiller(ChillNum)%CondenserType == WaterCooled) THEN
    IF (GTChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(GTChiller(ChillNum)%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(GTChiller(ChillNum)%CWLoopNum)%PlantSizNum

  IF (GTChiller(ChillNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(GTChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                  'SizeGTChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                   InitConvTemp,                      &
                                   PlantLoop(GTChiller(ChillNum)%CWLoopNum)%FluidIndex, &
                                   'SizeGTChiller')
        tmpNomCap = Cp * Rho * PlantSizData(PltSizNum)%DeltaT &
                                        * PlantSizData(PltSizNum)%DesVolFlowRate * GTChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize)  GTChiller(ChillNum)%NomCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%NomCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Name, &
                              'Nominal Capacity [W]', GTChiller(ChillNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Gas Turbine Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Gas Turbine Chiller object='//TRIM(GTChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (GTChiller(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * GTChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              GTChiller(ChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Gas Turbine Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Gas Turbine Chiller object='//TRIM(GTChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(GTChiller(ChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (GTChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(GTChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  GTChiller(ChillNum)%TempDesCondIn, &
                                  PlantLoop(GTChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  'SizeGTChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                 GTChiller(ChillNum)%TempDesCondIn,                      &
                                 PlantLoop(GTChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                 'SizeGTChiller')
        tmpCondVolFlowRate = tmpNomCap *  (1. + 1./GTChiller(ChillNum)%COP) / &
                                ( PlantSizData(PltSizCondNum)%DeltaT * Cp * Rho)
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) GTChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              GTChiller(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowContinueError('Autosizing of Gas Turbine Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Gas Turbine Chiller object='//TRIM(GTChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF
  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (GTChiller(ChillNum)%CondenserType == WaterCooled) &
    CALL RegisterPlantCompDesignFlow(GTChiller(ChillNum)%CondInletNodeNum,tmpCondVolFlowRate)

  IF (GTChillerSpecial(ChillNum)%GTEngineCapacity  == AutoSize .and. PlantSizesOkayToFinalize ) THEN
    GTChillerSpecial(ChillNum)%GTEngineCapacity = GTChiller(ChillNum)%NomCap * EngineEff &
                                                    / GTChiller(ChillNum)%COP
  IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:CombustionTurbine', GTChiller(ChillNum)%Name, &
                            'Gas Turbine Engine Capacity [W]', GTChillerSpecial(ChillNum)%GTEngineCapacity)
  END IF


  IF (PlantSizesOkayToFinalize) Then
    !create predefined report
    equipName = GTChiller(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:CombustionTurbine')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,GTChiller(ChillNum)%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,GTChiller(ChillNum)%NomCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeGTChiller


! Beginning of Chiller model Subroutines
! *****************************************************************************

SUBROUTINE CalcGTChillerModel(ChillerNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher / Brandon Anderson
          !       DATE WRITTEN   Sept. 2000
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! simulate a vapor compression chiller using the GT model

          ! METHODOLOGY EMPLOYED:
          ! curve fit of performance data:

          ! REFERENCES:
          ! 1. BLAST Users Manual
          ! 2. CHILLER User Manual

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, SecInHour, CurrentTime
  USE DataHVACGlobals, ONLY : FirstTimeStepSysFlag, TimeStepSys, SysTimeElapsed
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : ControlType_SeriesActive, MassFlowTol, PlantLoop, &
                              TypeOf_Chiller_CombTurbine, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate
  USE DataEnvironment, ONLY : OutDryBulbTemp, EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillerNum        ! chiller number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep !unused1208
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
 ! INTEGER, INTENT(IN)    :: FlowLock        ! TRUE when flow resolver has calculated branch flow
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER        :: ExhaustCP = 1.047d0 !Exhaust Gas Specific Heat
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), DIMENSION(3)     :: CapacityRat           ! intermediate result:  capacity ratio
  REAL(r64), DIMENSION(3)     :: PowerRat              ! intermediate result:  power ratio
  REAL(r64), DIMENSION(3)     :: FullLoadFactor        ! intermediate result:  full load factor
  REAL(r64)              :: MinPartLoadRat        ! min allowed operating frac full load
  REAL(r64)              :: MaxPartLoadRat        ! max allowed operating frac full load
  REAL(r64)              :: TempCondIn            ! C - (GT ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempCondInDesign      ! C - (GT ADJTC(1)The design secondary loop fluid
  REAL(r64)              :: TempRiseRat           ! intermediate result:  temperature rise ratio
  REAL(r64)              :: EvapInletTemp         ! C - evaporator inlet temperature, water side
  REAL(r64)              :: CondInletTemp         ! C - condenser inlet temperature, water side
  REAL(r64)              :: TempEvapOut           ! C - evaporator outlet temperature, water side
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  REAL(r64)              :: TempEvapOutDesign     ! design evaporator outlet temperature, water side
  REAL(r64)              :: ChillerNomCap         ! chiller nominal capacity
  REAL(r64)              :: AvailChillerCap       ! chiller available capacity
  REAL(r64)              :: COP                   ! coefficient of performance
  REAL(r64)              :: FracFullLoadPower     ! fraction of full load power
  REAL(r64)              :: EvapDeltaTemp         ! C - evaporator temperature difference, water side
  REAL(r64)              :: DeltaTemp             ! C - intermediate result: condenser/evaporator temp diff
  REAL(r64)              :: AvailNomCapRat        ! intermediate result: available nominal capacity ratio
  REAL(r64)              :: FullLoadPowerRat      ! intermediate result: full load power ratio
  REAL(r64)              :: PartLoadRat           ! part load ratio for efficiency calculations
  REAL(r64)              :: OperPartLoadRat       ! Actual Operating PLR
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side
  REAL(r64), SAVE             :: EvapMassFlowRateMax=0.0   ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
  REAL(r64)              :: TempLowLimitEout      ! C - Evaporator low temp. limit cut off
! Special variables for GT Chiller
  REAL(r64)              :: RPLoad
  REAL(r64)              :: PLoad
  REAL(r64)              :: GTEngineCapacity      ! Capacity of GT Unit attached to Chiller
  REAL(r64)              :: MaxExhaustperGTPower  ! Maximum Exhaust Flow per KW Power Out
  REAL(r64)              :: RL
  REAL(r64)              :: RL2

  REAL(r64)              :: FuelEnergyIn          !(EFUEL) Amount of Fuel Energy Required to run gas turbine
  REAL(r64)              :: ExhaustFlow           !(FEX) Exhaust Gas Flow Rate cubic meters per second
  REAL(r64)              :: ExhaustTemp           !(TEX) Exhaust Gas Temperature in C
  REAL(r64)              :: QHeatRecLube          !(ELUBE) Recoverable Lube Oil Energy (W)
  REAL(r64)              :: UAtoCapRat            !(UACGC) Heat Exchanger UA to Capacity
  REAL(r64)              :: AmbientDeltaT         !(ATAIR) Difference between ambient actual and ambient design temperatures
  REAL(r64)         :: DesignSteamSatTemp         !Saturization Temperature of Steam in Stack
  REAL(r64)         :: ExhaustStackTemp           !Temperature of Exhaust Gases
  REAL(r64),SAVE  :: TimeStepSysLast=0.0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages

  INTEGER           :: HeatRecInNode     !Heat Recovery Fluid Inlet Node Num
  INTEGER           :: HeatRecOutNode    !Heat Recovery Fluid Outlet Node Num
  REAL(r64)         :: HeatRecInTemp     !Heat Recovery Fluid Inlet Temperature
  REAL(r64)         :: HeatRecOutTemp    !Heat Recovery Fluid Outlet Temperature
  REAL(r64)         :: HeatRecMdot       !Heat Recovery Fluid Mass FlowRate
  REAL(r64)         :: HeatRecCp         !Specific Heat of the Heat Recovery Fluid
  REAL(r64)         :: FuelHeatingValue  !Heating Value of Fuel in kJ/kg
  REAL(r64)         :: MinHeatRecMdot    !Mass Flow rate that keeps from exceeding max temp
  REAL(r64)         :: HeatRecRatio      !Reduced ratio to multiply recovered heat terms by
  REAL(r64)         :: FRAC
!  LOGICAL,SAVE      :: PossibleSubCooling=.FALSE.

  INTEGER     :: LoopNum
  INTEGER     :: LoopSideNum
  REAL(r64)   :: Cp      ! local for fluid specif heat, for evaporator
  REAL(r64)   :: CpCond  ! local for fluid specif heat, for condenser

          !set module level inlet and outlet nodes
  EvapMassFlowRate           = 0.0
  CondMassFlowRate           = 0.0
  Power                      = 0.0
  QCondenser                 = 0.0
  QEvaporator                = 0.0
  Energy                     = 0.0
  CondenserEnergy            = 0.0
  EvaporatorEnergy           = 0.0
  EvapInletNode  = GTChiller(ChillerNum)%EvapInletNodeNum
  EvapOutletNode = GTChiller(ChillerNum)%EvapOutletNodeNum
  CondInletNode  = GTChiller(ChillerNum)%CondInletNodeNum
  CondOutletNode = GTChiller(ChillerNum)%CondOutletNodeNum
  HeatRecInNode  = GTChillerSpecial(ChillerNum)%HeatRecInletNodeNum
  HeatRecOutNode = GTChillerSpecial(ChillerNum)%HeatRecOutletNodeNum
  QHeatRecLube   = 0.0
  FRAC           = 1.0
  LoopNum        = GTChiller(ChillerNum)%CWLoopNum
  LoopSideNum    = GTChiller(ChillerNum)%CWLoopSideNum
  EvapInletTemp  = Node(EvapInletNode)%Temp

! calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

! Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
! Wait for next time step to print warnings. If simulation iterates, print out
! the warning for the last iteration only. Must wait for next time step to accomplish this.
! If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(GTChiller(ChillerNum)%PrintMessage)THEN
      GTChiller(ChillerNum)%MsgErrorCount = &
                         GTChiller(ChillerNum)%MsgErrorCount + 1
     ! Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (GTChiller(ChillerNum)%MsgErrorCount < 2) THEN
        CALL ShowWarningError(TRIM(GTChiller(ChillerNum)%MsgBuffer1)//'.')
        CALL ShowContinueError(TRIM(GTChiller(ChillerNum)%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(GTChiller(ChillerNum)%MsgBuffer1)//' error continues.', &
          GTChiller(ChillerNum)%ErrCount1,ReportMaxOf=GTChiller(ChillerNum)%MsgDataLast,  &
          ReportMinOf=GTChiller(ChillerNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

! save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

! If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
! if the component control is SERIESACTIVE we set the component flow to inlet flow so that
! flow resolver will not shut down the branch
  IF(MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate           = 0.d0

      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%CWLoopNum,     &
                          GTChiller(ChillerNum)%CWLoopSideNum, &
                          GTChiller(ChillerNum)%CWBranchNum,   &
                          GTChiller(ChillerNum)%CWCompNum)
    ENDIF
    IF (GTChiller(ChillerNum)%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(GTChiller(ChillerNum)%CDLoopNum)% &
            LoopSide(GTChiller(ChillerNum)%CDLoopSideNum)% &
              Branch(GTChiller(ChillerNum)%CDBranchNum)%  &
                Comp(GTChiller(ChillerNum)%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate         = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate           = 0.d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                                GTChiller(ChillerNum)%CDLoopNum, &
                                GTChiller(ChillerNum)%CDLoopSideNum, &
                                GTChiller(ChillerNum)%CDBranchNum, &
                                GTChiller(ChillerNum)%CDCompNum)
      ENDIF
    ENDIF

    IF (GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(GTChiller(ChillerNum)%BasinHeaterPowerFTempDiff,&
                                GTChiller(ChillerNum)%BasinHeaterSchedulePtr,&
                                GTChiller(ChillerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    GTChiller(ChillerNum)%PrintMessage = .FALSE.
    RETURN
  END IF

  IF (GTChiller(ChillerNum)%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
!  Warn user if entering condenser temperature falls below 0C
    IF(Node(CondInletNode)%Temp .LT. 0.0 .and. .not. WarmupFlag) THEN
      GTChiller(ChillerNum)%PrintMessage = .TRUE.
      WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
      GTChiller(ChillerNum)%MsgBuffer1 = 'CalcGasTurbineChillerModel - Chiller:CombustionTurbine "' &
                           //TRIM(GTChiller(ChillerNum)%Name)// &
                           '" - Air Cooled Condenser Inlet Temperature below 0C'
      GTChiller(ChillerNum)%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                 ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                 //TRIM(CreateSysTimeIntervalString())
      GTChiller(ChillerNum)%MsgDataLast = Node(CondInletNode)%Temp
    ELSE
      GTChiller(ChillerNum)%PrintMessage = .FALSE.
    ENDIF
  Else IF (GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
    IF(Node(CondInletNode)%Temp .LT. 10.0d0 .and. .not. WarmupFlag) THEN
      GTChiller(ChillerNum)%PrintMessage = .TRUE.
      WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
      GTChiller(ChillerNum)%MsgBuffer1 = 'CalcGasTurbineChillerModel - Chiller:CombustionTurbine "' &
                           //TRIM(GTChiller(ChillerNum)%Name)// &
                           '" - Evap Cooled Condenser Inlet Temperature below 10C'
      GTChiller(ChillerNum)%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                 ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                 //TRIM(CreateSysTimeIntervalString())
      GTChiller(ChillerNum)%MsgDataLast = Node(CondInletNode)%Temp
    ELSE
      GTChiller(ChillerNum)%PrintMessage = .FALSE.
    ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

  ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

  !Set mass flow rates
  IF (GTChiller(ChillerNum)%CondenserType == WaterCooled) THEN
    CondMassFlowRate = GTChiller(ChillerNum)%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              GTChiller(ChillerNum)%CDLoopNum, &
                              GTChiller(ChillerNum)%CDLoopSideNum, &
                              GTChiller(ChillerNum)%CDBranchNum, &
                              GTChiller(ChillerNum)%CDCompNum)
    CALL PullCompInterconnectTrigger(GTChiller(ChillerNum)%CWLoopNum, &
                                     GTChiller(ChillerNum)%CWLoopSideNum, &
                                     GTChiller(ChillerNum)%CWBranchNum, &
                                     GTChiller(ChillerNum)%CWCompNum, &
                                     GTChiller(ChillerNum)%CondMassFlowIndex,              &
                                     GTChiller(ChillerNum)%CDLoopNum, &
                                     GTChiller(ChillerNum)%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)

    IF (CondMassFlowRate < MassFlowTol) RETURN

  END IF

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  CapacityRat        = GTChiller(ChillerNum)%CapRatCoef
  PowerRat           = GTChiller(ChillerNum)%PowerRatCoef
  FullLoadFactor     = GTChiller(ChillerNum)%FullLoadCoef
  MinPartLoadRat     = GTChiller(ChillerNum)%MinPartLoadRat
  MaxPartLoadRat     = GTChiller(ChillerNum)%MaxPartLoadRat
  TempCondInDesign   = GTChiller(ChillerNum)%TempDesCondIn
  TempRiseRat        = GTChiller(ChillerNum)%TempRiseCoef
  TempEvapOutDesign  = GTChiller(ChillerNum)%TempDesEvapOut
  ChillerNomCap      = GTChiller(ChillerNum)%NomCap
  COP                = GTChiller(ChillerNum)%COP
  TempCondIn         = Node(GTChiller(ChillerNum)%CondInletNodeNum)%Temp
  TempEvapOut        = Node(GTChiller(ChillerNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = GTChiller(ChillerNum)%TempLowLimitEvapOut
  EvapMassFlowRateMax = GTChiller(ChillerNum)%EvapMassFlowRateMax
  LoopNum            = GTChiller(ChillerNum)%CWLoopNum
  LoopSideNum        = GTChiller(ChillerNum)%CWLoopSideNum

!*********************************
  !Calculate chiller performance from this set of performance equations.
  !  from BLAST...Z=(TECONDW-ADJTC(1))/ADJTC(2)-(TLCHLRW-ADJTC(3))
  DeltaTemp= (TempCondIn   -  TempCondInDesign) / TempRiseRat &
           - (TempEvapOut -  TempEvapOutDesign)

  !  from BLAST...RCAV=RCAVC(1)+RCAVC(2)*Z+RCAVC(3)*Z**2
  AvailNomCapRat =   CapacityRat(1)                   &
                     + CapacityRat(2) * DeltaTemp       &
                     + CapacityRat(3) * DeltaTemp ** 2

  AvailChillerCap = ChillerNomCap*AvailNomCapRat

  ! from BLAST...G=ADJEC(1)+ADJEC(2)*RCAV+ADJEC(3)*RCAV**2.
  FullLoadPowerRat=   PowerRat(1)                         &
                      + PowerRat(2) * AvailNomCapRat      &
                      + PowerRat(3) * AvailNomCapRat ** 2

  !  from BLAST...RCLOAD=AMAX1(MINCHFR(I,IPLCTR),AMIN1(CHLRLOAD(I)/CHLROCAP(I) &
  !         /RCAV,MAXCHFR(I,IPLCTR)))
  IF (AvailChillerCap > 0.0) THEN
    PartLoadRat = MAX(MinPartLoadRat, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
  ENDIF

  ! from BLAST...RPOWER=RPWRC(1)+RPWRC(2)*RCLOAD+RPWRC(3)*RCLOAD**2
  FracFullLoadPower = FullLoadFactor(1)                      &
                      + FullLoadFactor(2) * PartLoadRat      &
                      + FullLoadFactor(3) * PartLoadRat ** 2

  IF (AvailChillerCap > 0.0) THEN
    IF(ABS(MyLoad)/AvailChillerCap .LT. MinPartLoadRat) THEN
     OperPartLoadRat = ABS(MyLoad)/AvailChillerCap
    ELSE
     OperPartLoadRat = PartLoadRat
    END IF
  ELSE
    OperPartLoadRat = 0.0d0
  ENDIF
!*********************************
  Cp = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillerNum)%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(GTChiller(ChillerNum)%CWLoopNum)%FluidIndex, &
                         'CalcGTChillerModel')
! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
! condenser side outlet temperature.
  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN
    GTChiller(ChillerNum)%PossibleSubCooling  =.FALSE.
    QEvaporator = AvailChillerCap * OperPartLoadRat
    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
     FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
     FRAC = 1.0
    END IF
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap/COP * FRAC

    ! Either set the flow to the Constant value or caluclate the flow for the variable volume
    If(GTChiller(ChillerNum)%ConstantFlow)Then
      ! Start by assuming max (design) flow
      EvapMassFlowRate = EvapMassFlowRateMax
      ! Use SetComponentFlowRate to decide actual flow
      Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%CWLoopNum,     &
                          GTChiller(ChillerNum)%CWLoopSideNum, &
                          GTChiller(ChillerNum)%CWBranchNum,   &
                          GTChiller(ChillerNum)%CWCompNum)
      ! Evaluate delta temp based on actual flow rate
      IF (EvapMassFlowRate /= 0.0D0) THEN
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      ELSE
        EvapDeltaTemp = 0.0D0
      ENDIF
      ! Evaluate outlet temp based on delta
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    Else IF(GTChiller(ChillerNum)%VariableFlow)Then
      ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
      EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
      IF (EvapDeltaTemp /= 0) THEN
        ! Calculate desired flow to request based on load
        EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
        IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTol) &
               GTChiller(ChillerNum)%PossibleSubCooling = .TRUE.
        !Check to see if the Maximum is exceeded, if so set to maximum
        EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%CWLoopNum,     &
                          GTChiller(ChillerNum)%CWLoopSideNum, &
                          GTChiller(ChillerNum)%CWBranchNum,   &
                          GTChiller(ChillerNum)%CWCompNum)

        EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
      ELSE
        ! Try to request zero flow
        EvapMassFlowRate=0.0
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          GTChiller(ChillerNum)%CWLoopNum,     &
                          GTChiller(ChillerNum)%CWLoopSideNum, &
                          GTChiller(ChillerNum)%CWBranchNum,   &
                          GTChiller(ChillerNum)%CWCompNum)
        ! No deltaT since component is not running
        EvapOutletTemp = Node(EvapInletNode)%Temp

      END IF
    End If  !End of Constant Variable Flow If Block
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              GTChiller(ChillerNum)%CWLoopNum,     &
                              GTChiller(ChillerNum)%CWLoopSideNum, &
                              GTChiller(ChillerNum)%CWBranchNum,   &
                              GTChiller(ChillerNum)%CWCompNum)
!       Some other component set the flow to 0. No reason to continue with calculations.
    IF(EvapMassFlowRate == 0.0d0)THEN
      MyLoad = 0.0d0
      IF (GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(GTChiller(ChillerNum)%BasinHeaterPowerFTempDiff,&
                            GTChiller(ChillerNum)%BasinHeaterSchedulePtr,&
                            GTChiller(ChillerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
      ENDIF
      GTChiller(ChillerNum)%PrintMessage = .FALSE.
      RETURN
    END IF

    IF(GTChiller(ChillerNum)%PossibleSubCooling) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE  !No subcooling in this case.No recalculation required.Still need to check chiller low temp limit
      IF ((GTChiller(ChillerNum)%VariableFlow) .OR. &
          (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(GTChiller(ChillerNum)%CWBranchNum) &
            %Comp(GTChiller(ChillerNum)%CWCompNum)%CurOpSchemeType &
               == CompSetPtBasedSchemeType)          .OR. &
          (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
        TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
      ELSE
        TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF

      EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
      QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
      EvapOutletTemp = TempEvapOutSetpoint
    END IF
    !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
    IF(EvapOutletTemp .LT. TempLowLimitEout) THEN
      IF((Node(EvapInletNode)%Temp - TempLowLimitEout) .GT. DeltaTempTol) THEN
        EvapOutletTemp = TempLowLimitEout
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
      IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTempTol) THEN
        EvapOutletTemp = Node(EvapOutletNode)%TempMin
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      ELSE
        EvapOutletTemp = Node(EvapInletNode)%Temp
        EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
        QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
      END IF
    END IF
    ! If load exceeds the distributed load set to the distributed load
    If(QEvaporator > ABS(MyLoad)) Then
      If(EvapMassFlowRate > MassFlowTol) THEN
        QEvaporator = ABS(MyLoad)
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End IF

    ! Checks QEvaporator on the basis of the machine limits.
    If(QEvaporator > (AvailChillerCap * MaxPartLoadRat))Then
      If(EvapMassFlowRate > MassFlowTol) THEN
        QEvaporator = AvailChillerCap * PartLoadRat
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      Else
        QEvaporator = 0.0
        EvapOutletTemp = Node(EvapInletNode)%Temp
      End If
    End If

    IF (OperPartLoadRat .LT. MinPartLoadRat) THEN
      FRAC = MIN(1.0d0,(OperPartLoadRat/MinPartLoadRat))
    ELSE
      FRAC = 1.0
    END IF

    ! set the module level variable used for reporting FRAC
    ChillerCyclingRatio = FRAC

    ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
    Power = FracFullLoadPower * FullLoadPowerRat * AvailChillerCap /COP * FRAC

    IF(EvapMassFlowRate == 0.0) THEN
     QEvaporator = 0.0
     EvapOutletTemp = Node(EvapInletNode)%Temp
     Power = 0.0
     GTChiller(ChillerNum)%PrintMessage = .FALSE.
    END IF
    IF(QEvaporator == 0.0d0 .AND. GTChiller(ChillerNum)%CondenserType == EvapCooled) THEN
       CALL CalcBasinHeaterPower(GTChiller(ChillerNum)%BasinHeaterPowerFTempDiff,&
                                 GTChiller(ChillerNum)%BasinHeaterSchedulePtr,&
                                 GTChiller(ChillerNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    END IF

    END IF  !This is the end of the FlowLock Block

    !Now determine Cooling
    !QCondenser is calculated the same for each type, but the power consumption should be different
    !  depending on the performance coefficients used for the chiller model.
    QCondenser = Power + QEvaporator

    IF (GTChiller(ChillerNum)%CondenserType == WaterCooled) THEN

      IF (CondMassFlowRate > MassFlowTol) THEN
        CpCond = GetSpecificHeatGlycol(PlantLoop(GTChiller(ChillerNum)%CDLoopNum)%FluidName,  &
                                       CondInletTemp,                      &
                                       PlantLoop(GTChiller(ChillerNum)%CDLoopNum)%FluidIndex, &
                                       'CalcGTChillerModel')
        CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
      ELSE
        CALL ShowSevereError('CalcGasTurbineChillerModel: Condenser flow = 0, for GasTurbineChiller='//  &
                             TRIM(GTChiller(ChillerNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')

      END IF

    ELSE !Air Cooled or Evap Cooled

      !don't care about outlet temp for Air-Cooled or Evap Cooled and there is no CondMassFlowRate and would divide by zero
      CondOutletTemp = CondInletTemp
    END IF



    !Special GT Chiller Variables
    ! Gas Turbine Driven Portion of the Chiller:

    GTEngineCapacity = GTChillerSpecial(ChillerNum)%GTEngineCapacity
    MaxExhaustperGTPower = GTChillerSpecial(ChillerNum)%MaxExhaustperGTPower


!Note: All Old Blast Code comments begin at left.

!D                                   COMPUTE TOWER CLOAD
!               ETOWER(TypeIndex) = PREQD + CHLRLOAD(TypeIndex)
!               RPLOAD = PREQD/CHLROCAP(TypeIndex)
!
!               IF (RFLAGS(81)) WRITE (OUTPUT,703) PREQD,ETOWER(TypeIndex),RPLOAD
!               IF (PREQD .GT. 0.) THEN
    IF (AvailChillerCap >0)THEN
      RPLoad = Power / AvailChillerCap
    ELSE
      RPLoad = 0.0
    END IF

    IF (Power > 0) THEN
!D$                               FOR EACH CHILLER OPERATING
!                  MAXSZ = NUMCHSIZ(TypeIndex,IPLCTR)
!                  DO 100 IS = 1,MAXSZ
!
!                     NUMOPR = CHLRIOPR(IS,TypeIndex)
!                     IF (NUMOPR.GT.0) THEN
!
!                        PLOAD = CHNOMCAP(IS,TypeIndex,IPLCTR) * RPLOAD

    PLoad = ChillerNomCap * RPLoad

!
!D$                                COMPUTE FUEL AND WASTE HEAT
!
!     TEX IS CALCULATED USING COEFFICIENTS TEX2GC( ) TO RESULT IN TEMP.
!     DEGREES ACTUAL, HENCE THE NECESSARY CONVERSION ?-273.?
!
!                        RLOAD=AMAX1(PLOAD/CHLROCAP(TypeIndex),MINCHFR(TypeIndex,IPLCTR))
!                        RLD2 = RLOAD**2

     ! RL = MAX(PLoad/GTEngineCapacity, MinPartLoadRat * ChillerNomCap)
    RL = MAX(PLoad/ChillerNomCap, MinPartLoadRat)
    RL2 = RL**2

!     ATAIR = DELTA TEMPERATURE. ACTUAL - 25 DEG.C (77 DEG.F)
!                                RATING POINT
!                        ATAIR = ODB - 25.
!                        TAR2=ATAIR**2

    ! ??? Not sure about this Ambient Actual Temp - also do we need to have design ambient as input?

    IF (GTChiller(ChillerNum)%CondenserType == WaterCooled) THEN
      AmbientDeltaT = OutDryBulbTemp - 25.d0
    ELSE  ! air or evap cooled
      AmbientDeltaT = Node(CondInletNode)%OutAirDryBulb - 25.d0
    ENDIF


!                        EFUEL=PLOAD*(FUL1GC(1,IPLCTR)+FUL1GC(2,IPLCTR)*  &
!                              RLOAD+FUL1GC(3,IPLCTR)*RLD2)*              &
!                              (FUL2GC(1,IPLCTR)+FUL2GC(2,IPLCTR)*ATAIR+  &
!                              FUL2GC(3,IPLCTR)*TAR2)

    FuelEnergyIn = PLoad * (GTChillerSpecial(ChillerNum)%PLBasedFuelInputCoef(1) +         &
                               GTChillerSpecial(ChillerNum)%PLBasedFuelInputCoef(2)*RL +    &
                                  GTChillerSpecial(ChillerNum)%PLBasedFuelInputCoef(3)*RL2)  &
                           * (GTChillerSpecial(ChillerNum)%TempBasedFuelInputCoef(1) +       &
                                GTChillerSpecial(ChillerNum)%TempBasedFuelInputCoef(2)*AmbientDeltaT + &
                                  GTChillerSpecial(ChillerNum)%TempBasedFuelInputCoef(3)*AmbientDeltaT**2)


!                        FEX=GTDSLCAP(IS,TypeIndex,IPLCTR)*(FEXGC(1,IPLCTR)+      &
!                            FEXGC(2,IPLCTR)*ATAIR+FEXGC(3,IPLCTR)*TAR2)

    ExhaustFlow = GTEngineCapacity * (GTChillerSpecial(ChillerNum)%ExhaustFlowCoef(1) +      &
                                          GTChillerSpecial(ChillerNum)%ExhaustFlowCoef(2) * AmbientDeltaT  + &
                                            GTChillerSpecial(ChillerNum)%ExhaustFlowCoef(3) * AmbientDeltaT**2)

!                        TEX=(TEX1GC(1,IPLCTR)+TEX1GC(2,IPLCTR)*RLOAD+    &
!                            TEX1GC(3,IPLCTR)*RLD2)*(TEX2GC(1,IPLCTR)+    &
!                            TEX2GC(2,IPLCTR)*ATAIR+TEX2GC(3,IPLCTR)*     &
!                            TAR2)-273.

    ExhaustTemp = (GTChillerSpecial(ChillerNum)%PLBasedExhaustTempCoef(1) +          &
                       GTChillerSpecial(ChillerNum)%PLBasedExhaustTempCoef(2)*RL +     &
                         GTChillerSpecial(ChillerNum)%PLBasedExhaustTempCoef(3)*RL2)   &
                  * (GTChillerSpecial(ChillerNum)%TempBasedExhaustTempCoef(1) +        &
                       GTChillerSpecial(ChillerNum)%TempBasedExhaustTempCoef(2)*AmbientDeltaT + &
                         GTChillerSpecial(ChillerNum)%TempBasedExhaustTempCoef(3)*AmbientDeltaT**2) - 273



!                        UAG=UACGC(1,IPLCTR)*GTDSLCAP(IS,TypeIndex,IPLCTR)**      &
!                            UACGC(2,IPLCTR)
    IF (PLoad /= 0.0)THEN
      UAtoCapRat = GTChillerSpecial(ChillerNum)%UAtoCapCoef(1) * GTEngineCapacity **  &
                       GTChillerSpecial(ChillerNum)%UAtoCapCoef(2)

!     TSTACK = EXHAUST STACK TEMPERATURE, C.
!
!                        TSTACK=TSATUR(IPLCTR)+(TEX-TSATUR(IPLCTR))/      &
!                               EXP(UAG/(AMAX1(FEX,RMXKGC(IPLCTR)*        &
!                               GTDSLCAP(IS,TypeIndex,IPLCTR)) * 1.047))

      DesignSteamSatTemp = GTChillerSpecial(ChillerNum)%DesignSteamSatTemp
      ExhaustStackTemp = DesignSteamSatTemp + (ExhaustTemp - DesignSteamSatTemp) / &
                           EXP(UAtoCapRat/(MAX(ExhaustFlow, MaxExhaustperGTPower * GTEngineCapacity) * ExhaustCP))


!                        EEX = AMAX1 ( FEX*1.047*(TEX-TSTACK),0.)
!                        ELUBE=PLOAD*(ELUBEGC(1,IPLCTR)+ELUBEGC(2,IPLCTR) &
!                              *RLOAD+ELUBEGC(3,IPLCTR)*RLD2 )
    END IF

    IF (GTChillerSpecial(ChillerNum)%HeatRecActive) THEN
      QHeatRecLube = PLoad * (GTChillerSpecial(ChillerNum)%HeatRecLubeEnergyCoef(1) +      &
                              GTChillerSpecial(ChillerNum)%HeatRecLubeEnergyCoef(2)*RL + &
                              GTChillerSpecial(ChillerNum)%HeatRecLubeEnergyCoef(3)*RL2)

    ELSE
      QHeatRecLube = 0.0
    End If

!                        CHLRFUEL(TypeIndex) = CHLRFUEL(TypeIndex) + EFUEL * NUMOPR
!                        EEXGC = EEXGC + EEX * NUMOPR
!                        ELBEGC = ELBEGC + ELUBE * NUMOPR
!


!Heat Recovery Loop -  lube recovered heat
!   If lube is not present, then the energy should be 0 at this point
! Thigh = Energy / (Mdot*Cp) + Tlow

    !Need to set the HeatRecRatio to 1.0 if it is not modified
    HeatRecRatio= 1.0

    IF (GTChillerSpecial(ChillerNum)%HeatRecActive) THEN
       !This mdot is input specified mdot "Desired Flowrate", already set at node in init routine
      HeatRecMdot = Node(HeatRecInNode)%MassFlowRate
      HeatRecInTemp = Node(HeatRecInNode)%Temp
      HeatRecCp = GetSpecificHeatGlycol(PlantLoop(GTChillerSpecial(ChillerNum)%HRLoopNum)%FluidName,  &
                                        HeatRecInTemp,                      &
                                        PlantLoop(GTChillerSpecial(ChillerNum)%HRLoopNum)%FluidIndex, &
                                        'ChillerHeatRecovery')

      !Don't divide by zero
      IF ((HeatRecMdot .GT. 0) .AND. (HeatRecCp .GT. 0)) THEN
        HeatRecOutTemp = (QHeatRecLube)/(HeatRecMdot * HeatRecCp) + HeatRecInTemp
      ELSE
        HeatRecOutTemp = HeatRecInTemp
      END IF

      !Now verify that the design flowrate was large enough to prevent phase change
      IF(HeatRecOutTemp > GTChillerSpecial(ChillerNum)%HeatRecMaxTemp) THEN
        IF(GTChillerSpecial(ChillerNum)%HeatRecMaxTemp /= HeatRecInTemp)THEN
          MinHeatRecMdot = (QHeatRecLube)/(HeatRecCp * (GTChillerSpecial(ChillerNum)%HeatRecMaxTemp - HeatRecInTemp))
          If(MinHeatRecMdot < 0.0) MinHeatRecMdot = 0.0
        END IF

        !Recalculate Outlet Temperature, with adjusted flowrate
        IF ((MinHeatRecMdot .GT. 0.0) .AND. (HeatRecCp .GT. 0.0)) THEN
          HeatRecOutTemp = (QHeatRecLube)/(MinHeatRecMdot * HeatRecCp) + HeatRecInTemp
          HeatRecRatio = HeatRecMdot/MinHeatRecMdot
        ELSE
          HeatRecOutTemp = HeatRecInTemp
          HeatRecRatio = 0.0
        END IF
      End If

      QHeatRecLube = QHeatRecLube*HeatRecRatio
    ELSE
      HeatRecInTemp=0.0
      HeatRecMDot=0.0
      HeatRecCp=0.0
      HeatRecOutTemp=0.0
    ENDIF

  END IF

   GTChillerSpecial(ChillerNum)%HeatRecInletTemp = HeatRecInTemp
   GTChillerSpecial(ChillerNum)%HeatRecOutletTemp = HeatRecOutTemp
   GTChillerSpecial(ChillerNum)%HeatRecMdot = HeatRecMdot
   GTChillerSpecial(ChillerNum)%HeatRecLubeEnergy = QHeatRecLube*(TimeStepSys*SecInHour)
   GTChillerSpecial(ChillerNum)%HeatRecLubeRate = QHeatRecLube
   GTChillerSpecial(ChillerNum)%FuelEnergyIn = ABS(FuelEnergyIn)

   FuelHeatingValue = GTChillerSpecial(ChillerNum)%FuelHeatingValue

   GTChillerReport(ChillerNum)%FuelMassUsedRate =  ABS(FuelEnergyIn)/(FuelHeatingValue * KJtoJ)

   GTChillerSpecial(ChillerNum)%ExhaustStackTemp = ExhaustStackTemp

      !Calculate Energy
   CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
   Energy           = Power*TimeStepSys*SecInHour
   EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem

    IF (GTChiller(ChillerNum)%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcGTChillerModel: Condenser loop inlet temperatures over 70.0 C for GTChiller='//  &
                            TRIM(GTChiller(ChillerNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    IF(.NOT.WarmupFlag)THEN
      If (AvailNomCapRat < 0.0 ) then     ! apparently the real reason energy goes negative
        CALL ShowSevereError('CalcGTChillerModel: Capacity ratio below zero for GTChiller='//  &
                              TRIM(GTChiller(ChillerNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Check input for Capacity Ratio Curve')
        CALL showContinueError('Condenser inlet temperature: '//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )
        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0
    Energy = 0.0
  ENDIF
  RETURN
END SUBROUTINE CalcGTChillerModel

! End of GT Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the GT Chiller Module
! *****************************************************************************

SUBROUTINE UpdateGTChillerRecords(MyLoad,RunFlag, Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher / Brandon Anderson
            !       DATE WRITTEN:    September 2000

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS: na
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE PlantUtilities,  ONLY : SafeCopyPlantNode

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)     :: MyLoad    ! current load
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! chiller number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode         ! evaporator inlet node number, water side
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number, water side
  INTEGER                :: CondInletNode         ! condenser inlet node number, water side
  INTEGER                :: CondOutletNode        ! condenser outlet node number, water side

  INTEGER                :: HeatRecInletNode
  INTEGER                :: HeatRecOutletNode
  REAL(r64)              :: ReportingConstant     ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode  = GTChiller(Num)%EvapInletNodeNum
  EvapOutletNode = GTChiller(Num)%EvapOutletNodeNum
  CondInletNode  = GTChiller(Num)%CondInletNodeNum
  CondOutletNode = GTChiller(Num)%CondOutletNodeNum
  IF (GTChillerSpecial(Num)%HeatRecActive) THEN
    HeatRecInletNode = GTChillerSpecial(Num)%HeatRecInletNodeNum
    HeatRecOutletNode = GTChillerSpecial(Num)%HeatRecOutletNodeNum
  ENDIF

  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running so pass inlet states to outlet states
          !set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp


    IF (GTChillerSpecial(Num)%HeatRecActive) THEN
      CALL SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode)
      GTChillerReport(Num)%HeatRecInletTemp     = Node(HeatRecInletNode)%Temp
      GTChillerReport(Num)%HeatRecOutletTemp    = Node(HeatRecOutletNode)%Temp
    ENDIF


    GTChillerReport(Num)%Power            = 0
    GTChillerReport(Num)%QEvap            = 0
    GTChillerReport(Num)%QCond            = 0
    GTChillerReport(Num)%Energy           = 0
    GTChillerReport(Num)%EvapEnergy       = 0
    GTChillerReport(Num)%CondEnergy       = 0
    GTChillerReport(Num)%EvapInletTemp  = Node(EvapInletNode)%Temp
    GTChillerReport(Num)%CondInletTemp  = Node(CondInletNode)%Temp
    GTChillerReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    GTChillerReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    GTChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    GTChillerReport(Num)%Condmdot         = CondMassFlowRate
    GTChillerReport(Num)%FuelEnergyUsedRate = 0.0
    GTChillerReport(Num)%FuelMassUsedRate   = 0.0
    GTChillerReport(Num)%FuelEnergyUsed     = 0.0
    GTChillerReport(Num)%FuelMassUsed       = 0.0

    GTChillerReport(Num)%HeatRecLubeEnergy    = 0.0
    GTChillerReport(Num)%HeatRecLubeRate      = 0.0
    GTChillerReport(Num)%ExhaustStackTemp     = 0.0
    GTChillerReport(Num)%HeatRecMdot          = GTChillerSpecial(Num)%HeatRecMdot
    GTChillerReport(Num)%FuelCOP              = 0.0
    IF (GTChiller(Num)%CondenserType == EvapCooled) THEN
      GTChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      GTChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

  ELSE !Chiller is running so report calculated values
          !set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp

    IF (GTChillerSpecial(Num)%HeatRecActive) THEN
      CALL SafeCopyPlantNode( HeatRecOutletNode, HeatRecInletNode)
      Node(HeatRecOutletNode)%Temp = GTChillerSpecial(Num)%HeatRecOutletTemp
    ENDIF

    GTChillerReport(Num)%Power            = Power
    GTChillerReport(Num)%QEvap            = QEvaporator
    GTChillerReport(Num)%QCond            = QCondenser
    GTChillerReport(Num)%Energy           = Energy
    GTChillerReport(Num)%EvapEnergy       = EvaporatorEnergy
    GTChillerReport(Num)%CondEnergy        = CondenserEnergy
    GTChillerReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    GTChillerReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    GTChillerReport(Num)%CondOutletTemp   = Node(CondOutletNode)%Temp
    GTChillerReport(Num)%EvapOutletTemp   = Node(EvapOutletNode)%Temp
    GTChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    GTChillerReport(Num)%Condmdot         = CondMassFlowRate

    GTChillerReport(Num)%HeatRecLubeEnergy    = GTChillerSpecial(Num)%HeatRecLubeEnergy
    GTChillerReport(Num)%HeatRecLubeRate      = GTChillerSpecial(Num)%HeatRecLubeRate
    GTChillerReport(Num)%FuelEnergyUsedRate   = GTChillerSpecial(Num)%FuelEnergyIn
    GTChillerReport(Num)%FuelMassUsedRate     = GTChillerReport(Num)%FuelMassUsedRate
    GTChillerReport(Num)%FuelEnergyUsed       = GTChillerReport(Num)%FuelEnergyUsedRate*TimeStepSys*SecInHour
    GTChillerReport(Num)%FuelMassUsed         = GTChillerReport(Num)%FuelMassUsedRate*TimeStepSys*SecInHour
    GTChillerReport(Num)%ExhaustStackTemp     = GTChillerSpecial(Num)%ExhaustStackTemp
    GTChillerReport(Num)%HeatRecInletTemp     = GTChillerSpecial(Num)%HeatRecInletTemp
    GTChillerReport(Num)%HeatRecOutletTemp    = GTChillerSpecial(Num)%HeatRecOutletTemp
    GTChillerReport(Num)%HeatRecMdot          = GTChillerSpecial(Num)%HeatRecMdot
    IF (GTChillerReport(Num)%FuelEnergyUsedRate .NE. 0.0) THEN
      GTChillerReport(Num)%FuelCOP              = GTChillerReport(Num)%QEvap/GTChillerReport(Num)%FuelEnergyUsedRate
    ELSE
      GTChillerReport(Num)%FuelCOP              = 0.0
    END IF
    IF (GTChiller(Num)%CondenserType == EvapCooled) THEN
      GTChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      GTChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF
  END IF
RETURN
END SUBROUTINE UpdateGTChillerRecords

! End of Record Keeping subroutines for the GT Chiller Module
! *****************************************************************************


END MODULE ChillerGasTurbine


!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE ChillerConstCOP  !ConstCop  ChillerModule
          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   April 1999
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of constant COP vapor
          ! compression Chillers.

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the Const. COP chiller
          ! is available to meet a loop cooling demand, it calls SimConstCOPChiller
          ! which in turn calls the appropriate Const COP chiller model.

          ! REFERENCES: na

          ! OTHER NOTES: na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals, ONLY: MaxNameLength, BeginFullSimFlag, InitConvTemp, WarmupFlag
USE DataInterfaces
USE DataPlant,       ONLY: DeltaTemptol
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE General,         ONLY: TrimSigDigits

IMPLICIT NONE

PRIVATE

  ! MODULE PARAMETER DEFINITIONS
! Parameters for use in Chillers
INTEGER, PARAMETER :: AirCooled = 1
INTEGER, PARAMETER :: WaterCooled = 2
INTEGER, PARAMETER :: EvapCooled = 3
REAL(r64), PARAMETER    :: zeroLoadtol = 1.0d0      ! zero load tolerance for operating equipment in W

  ! DERIVED TYPE DEFINITIONS
TYPE ChillerSpecs
   CHARACTER(len=MaxNameLength) :: Name   =' ' ! user identifier
   REAL(r64)         :: NomCap            =0.0 ! design nominal capacity of chiller
   REAL(r64)         :: COP               =0.0 ! (BLAST ELECTRICAL) 1/COP at nominal (design) conditions
   LOGICAL           :: ConstantFlow      =.false. ! True if this is a Constant Flow Chiller
   LOGICAL           :: VariableFlow      =.false. ! True if this is a Variable Flow Chiller
   LOGICAL           :: VariableFlowSetToLoop = .FALSE.  ! True if the setpoint is missing at the outlet node
   LOGICAL           :: VariableFlowErrDone   = .FALSE.  ! true if setpoint warning issued
   REAL(r64)         :: EvapVolFlowRate   =0.0 ! m**3/s - design nominal water volumetric flow rate through the evaporator
   REAL(r64)         :: EvapMassFlowRateMax=0.0    ! Max Design Evaporator Mass Flow Rate converted from Volume Flow Rate
   REAL(r64)         :: CondVolFlowRate   =0.0  ! design nominal water volumetric flow rate through the condenser
   REAL(r64)         :: CondMassFlowRateMax =0.0 ! kg/s - design water mass flow rate through condenser
   REAL(r64)         :: SizFac            = 0.0 ! sizing factor
   REAL(r64)         :: BasinHeaterPowerFTempDiff = 0.0 ! Basin heater capacity per degree C below set point (W/C)
   REAL(r64)         :: BasinHeaterSetPointTemp   = 0.0 ! Set point temperature for basin heater operation (C)
   INTEGER           :: EvapInletNodeNum  =0   ! Node number on the inlet side of the plant
   INTEGER           :: EvapOutletNodeNum =0   ! Node number on the outlet side of the plant
   INTEGER           :: CondInletNodeNum  =0   ! Node number on the inlet side of the condenser
   INTEGER           :: CondOutletNodeNum =0   ! Node number on the outlet side of the condenser
   INTEGER           :: CondenserType     =WaterCooled   ! Type of Condenser - Air or Water or Evap Cooled
   INTEGER           :: ErrCount1         =0   ! error counter
   INTEGER           :: ErrCount2         =0   ! error counter
   INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
   INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
   INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
   INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
   INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
   INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
   INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
   INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
   INTEGER           :: BasinHeaterSchedulePtr  = 0   ! Pointer to basin heater schedule
   INTEGER           :: CondMassFlowIndex = 0
   CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
   CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
   REAL(r64)         :: MsgDataLast   = 0.0 ! value of data when warning occurred (passed to Recurring Warn)
   LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
   INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
   LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE ChillerSpecs

TYPE ReportVars
  REAL(r64)    :: Power          = 0.0 !
  REAL(r64)    :: QEvap          = 0.0 !
  REAL(r64)    :: QCond          = 0.0 !
  REAL(r64)    :: Energy         = 0.0 !
  REAL(r64)    :: EvapEnergy     = 0.0 !
  REAL(r64)    :: CondEnergy     = 0.0 !
  REAL(r64)    :: CondInletTemp  = 0.0 !
  REAL(r64)    :: EvapInletTemp  = 0.0 !
  REAL(r64)    :: CondOutletTemp = 0.0 !
  REAL(r64)    :: EvapOutletTemp = 0.0 !
  REAL(r64)    :: Evapmdot       = 0.0 !
  REAL(r64)    :: Condmdot       = 0.0 !
  REAL(r64)    :: ActualCOP      = 0.0 !
  REAL(r64)    :: BasinHeaterPower       = 0.0  ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0  ! Basin heater energy consumption (J)
END TYPE ReportVars


  ! MODULE VARIABLE DECLARATIONS:
INTEGER                  :: NumConstCOPChillers =0
REAL(r64)                :: EvapInletTemp       =0.0
REAL(r64)                :: CondInletTemp       =0.0
REAL(r64)                :: CondMassFlowRate    =0.0
REAL(r64)                :: EvapMassFlowRate    =0.0
REAL(r64)                :: CondOutletTemp      =0.0
REAL(r64)                :: EvapOutletTemp      =0.0
REAL(r64)                :: QEvaporator         =0.0
REAL(r64)                :: QCondenser          =0.0
REAL(r64)                :: Power               =0.0
REAL(r64)                :: EvaporatorEnergy    =0.0
REAL(r64)                :: CondenserEnergy     =0.0
REAL(r64)                :: Energy              =0.0
REAL(r64)                :: BasinHeaterPower       =0.0 ! Basin heater power (W)

TYPE (ChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: ConstCOPChiller         !dimension to number of machines

TYPE (ReportVars), ALLOCATABLE,DIMENSION(:) :: ConstCOPChillerReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE ChillerConstCOP
PUBLIC     SimConstCOPChiller
PRIVATE    CalcConstCOPChillerModel
PRIVATE    GetConstCOPChillerInput
PRIVATE    InitConstCOPChiller
PRIVATE    UpdateConstCOPChillerRecords
PRIVATE    SizeConstCOPChiller


CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Const COP Chiller Module Driver Subroutines
!*************************************************************************

SUBROUTINE SimConstCOPChiller(LoopNum, ChillerType,ChillerName,EquipFlowCtrl, CompIndex,RunFlag,FirstIteration, &
                              InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : FindItemInList
  USE DataPlant,      ONLY : PlantLoop, TypeOf_Chiller_ConstCOP
  USE PlantUtilities, ONLY : UpdateChillerComponentCondenserSide

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER ,         INTENT(IN) :: LoopNum
  CHARACTER(len=*), INTENT(IN) :: ChillerType   ! type of chiller !unused1208
  CHARACTER(len=*), INTENT(IN) :: ChillerName   ! user specified name of chiller
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT) :: CompIndex       ! chiller number pointer
  LOGICAL, INTENT(IN)    :: RunFlag
  LOGICAL, INTENT(IN)    :: FirstIteration
 ! INTEGER, INTENT(IN)    :: FlowLock
  LOGICAL, INTENT(INOUT) :: InitLoopEquip
  REAL(r64), INTENT(INOUT)    :: MyLoad
  REAL(r64),  INTENT(INOUT)   :: MinCap
  REAL(r64), INTENT(INOUT)    :: MaxCap
  REAL(r64), INTENT(INOUT)    :: OptCap
  LOGICAL, INTENT(IN)         :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(OUT)      :: SizingFactor     ! sizing factor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                 :: GetCCCOPInput = .TRUE.
  INTEGER :: ChillNum       ! chiller number pointer

         !FLOW


    !Initialize simulation variables

          !GET INPUT
  IF (GetCCCOPInput)  THEN
    CALL GetConstCOPChillerInput
    GetCCCOPInput = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    ChillNum = FindItemInList(ChillerName,ConstCOPChiller%Name,NumConstCOPChillers)
    IF (ChillNum == 0) THEN
      CALL ShowFatalError('SimConstCOPChiller: Specified Chiller not one of Valid Constant COP Chillers='//TRIM(ChillerName))
    ENDIF
    CompIndex=ChillNum
  ELSE
    ChillNum=CompIndex
    IF (ChillNum > NumConstCOPChillers .or. ChillNum < 1) THEN
      CALL ShowFatalError('SimConstCOPChiller:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(ChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumConstCOPChillers))//  &
                          ', Entered Unit name='//TRIM(ChillerName))
    ENDIF
    IF (CheckEquipName(ChillNum)) THEN
      IF (ChillerName /= ConstCOPChiller(ChillNum)%Name) THEN
        CALL ShowFatalError('SimConstCOPChiller: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(ChillNum))// &
                            ', Unit name='//TRIM(ChillerName)//', stored Unit Name for that index='//  &
                            TRIM(ConstCOPChiller(ChillNum)%Name))
      ENDIF
      CheckEquipName(ChillNum)=.false.
    ENDIF
  ENDIF



  IF (InitLoopEquip) THEN
    CALL InitConstCOPChiller(ChillNum,RunFlag,MyLoad,FirstIteration)
    CALL SizeConstCOPChiller(ChillNum)
    IF (LoopNum == ConstCOPChiller(ChillNum)%CWLoopNum) THEN
      MinCap = 0
      MaxCap = ConstCOPChiller(ChillNum)%NomCap
      OptCap = ConstCOPChiller(ChillNum)%NomCap
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = ConstCOPChiller(ChillNum)%SizFac
    END IF
    RETURN
  END IF

  IF (LoopNum == ConstCOPChiller(ChillNum)%CWLoopNum) THEN
 ! Calculate Load
   ! IF MinPlr, MaxPlr, OptPlr are not defined, assume min = 0, max=opt=Nomcap
    CALL InitConstCOPChiller(ChillNum,RunFlag,MyLoad,FirstIteration)
    CALL CalcConstCOPChillerModel(ChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
    CALL UpdateConstCOPChillerRecords(MyLoad,RunFlag,ChillNum)
  ELSEIF (LoopNum == ConstCOPChiller(ChillNum)%CDLoopNum) THEN
    CALL UpdateChillerComponentCondenserSide(ConstCOPChiller(ChillNum)%CDLoopNum, &
                                     ConstCOPChiller(ChillNum)%CDLoopSideNum,     &
                                     TypeOf_Chiller_ConstCOP,                     &
                                     ConstCOPChiller(ChillNum)%CondInletNodeNum,  &
                                     ConstCOPChiller(ChillNum)%CondOutletNodeNum, &
                                     ConstCOPChillerReport(ChillNum)%QCond,        &
                                     ConstCOPChillerReport(ChillNum)%CondInletTemp,     &
                                     ConstCOPChillerReport(ChillNum)%CondOutletTemp,    &
                                     ConstCOPChillerReport(ChillNum)%Condmdot,  &
                                     FirstIteration)
  ENDIF
RETURN
END SUBROUTINE SimConstCOPChiller

! End Const COP Chiller Module Driver Subroutines
!******************************************************************************


! Beginning of Const COP Chiller Module Get Input subroutines
!******************************************************************************


SUBROUTINE GetConstCOPChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    April 1998

            ! PURPOSE OF THIS SUBROUTINE:!This routine will get the input
                         !required by the PrimaryPlantLoopManager.  As such
                         !it will interact with the Input Scanner to retrieve
                         !information from the input file, count the number of
                         !heating and cooling loops and begin to fill the
                         !arrays associated with the type PlantLoopProps.


            ! METHODOLOGY EMPLOYED: to be determined...
            ! REFERENCES:

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE GlobalNames, ONLY: VerifyUniqueChillerName
  USE OutputReportPredefined
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE General, ONLY: RoundSigDigits
  USE ScheduleManager,    ONLY: GetScheduleIndex

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: ChillerNum
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: errflag
  LOGICAL       :: Okay

            !GET NUMBER OF ALL EQUIPMENT TYPES
  cCurrentModuleObject = 'Chiller:ConstantCOP'
  NumConstCOPChillers = GetNumObjectsFound(TRIM(cCurrentModuleObject))

  IF (NumConstCOPChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(ConstCOPChiller))RETURN

  ALLOCATE (ConstCOPChiller(NumConstCOPChillers))

  ALLOCATE (ConstCOPChillerReport(NumConstCOPChillers))
  ALLOCATE(CheckEquipName(NumConstCOPChillers))
  CheckEquipName=.true.

             !LOAD ARRAYS WITH BLAST ConstCOP CHILLER DATA
  DO ChillerNum = 1 , NumConstCOPChillers
    CALL GetObjectItem(TRIM(cCurrentModuleObject),ChillerNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ConstCOPChiller%Name,ChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    ENDIF
    ConstCOPChiller(ChillerNum)%Name                = cAlphaArgs(1)
    ConstCOPChiller(ChillerNum)%NomCap              = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF
    ConstCOPChiller(ChillerNum)%COP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    !Set the Condenser Type from input
    IF (cAlphaArgs(6) == 'AIRCOOLED' ) THEN
      ConstCOPChiller(ChillerNum)%CondenserType       = AirCooled
    ELSEIF (cAlphaArgs(6) == 'EVAPORATIVELYCOOLED') THEN
      ConstCOPChiller(ChillerNum)%CondenserType       = EvapCooled
    ELSEIF (cAlphaArgs(6) == 'WATERCOOLED' ) THEN
      ConstCOPChiller(ChillerNum)%CondenserType       = WaterCooled
    ELSE
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

    ConstCOPChiller(ChillerNum)%EvapVolFlowRate     = rNumericArgs(3)
    IF (ConstCOPChiller(ChillerNum)%CondenserType == AirCooled .OR. &
        ConstCOPChiller(ChillerNum)%CondenserType == EvapCooled) THEN ! Condenser flow rate not used for these cond types
      ConstCOPChiller(ChillerNum)%CondVolFlowRate   = 0.0011
    ELSE
      ConstCOPChiller(ChillerNum)%CondVolFlowRate   = rNumericArgs(4)
    ENDIF
    ConstCOPChiller(ChillerNum)%SizFac              = rNumericArgs(5)

    ConstCOPChiller(ChillerNum)%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ConstCOPChiller(ChillerNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')

    IF (ConstCOPChiller(ChillerNum)%CondenserType == AirCooled .or. ConstCOPChiller(ChillerNum)%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
       !If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      !  since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(4))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 21) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(4) = TRIM(cAlphaArgs(1))//' CONDENSER INLET NODE'
        ELSE
          cAlphaArgs(4) = TRIM(cAlphaArgs(1)(1:79))//' CONDENSER INLET NODE'
        ENDIF
      End If
      IF(lAlphaFieldBlanks(5) )THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 22) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(5) = TRIM(cAlphaArgs(1))//' CONDENSER OUTLET NODE'
        ELSE
          cAlphaArgs(5) = TRIM(cAlphaArgs(1)(1:78))//' CONDENSER OUTLET NODE'
        ENDIF
      END IF

      ConstCOPChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(ConstCOPChiller(ChillerNum)%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', Adding OutdoorAir:Node='//TRIM(cAlphaArgs(4)))
      ENDIF

      ConstCOPChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
    ELSEIF (ConstCOPChiller(ChillerNum)%CondenserType == WaterCooled) THEN
      ConstCOPChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ConstCOPChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser Water Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(4) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      ConstCOPChiller(ChillerNum)%CondInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      ConstCOPChiller(ChillerNum)%CondOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Condenser (unknown?) Nodes')
      !Condenser Inlet node name is necessary for Water Cooled
      IF (lAlphaFieldBlanks(4) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(4))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ELSEIF ( lAlphaFieldBlanks(5) ) THEN
        CALL ShowSevereError('Invalid, '//TRIM(cAlphaFieldNames(5))//'is blank ')
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    If(cAlphaArgs(7) .eq. 'CONSTANTFLOW') Then
       ConstCOPChiller(ChillerNum)%ConstantFlow = .True.
       ConstCOPChiller(ChillerNum)%VariableFlow = .False.
    Else If(cAlphaArgs(7) .eq. 'VARIABLEFLOW') Then
       ConstCOPChiller(ChillerNum)%ConstantFlow = .False.
       ConstCOPChiller(ChillerNum)%VariableFlow = .True.
    Else  ! We will assume a variable flow chiller is none specified
       ConstCOPChiller(ChillerNum)%ConstantFlow = .False.
       ConstCOPChiller(ChillerNum)%VariableFlow = .True.
    End If

    !   Basin heater power as a function of temperature must be greater than or equal to 0
    ConstCOPChiller(ChillerNum)%BasinHeaterPowerFTempDiff = rNumericArgs(6)
    IF(rNumericArgs(6) .LT. 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', "'//TRIM(ConstCOPChiller(ChillerNum)%Name)//&
                     '" TRIM(cNumericFieldNames(6)) must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    ConstCOPChiller(ChillerNum)%BasinHeaterSetPointTemp = rNumericArgs(7)

    IF(ConstCOPChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 7) THEN
        ConstCOPChiller(ChillerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(ConstCOPChiller(ChillerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//':"'//TRIM(ConstCOPChiller(ChillerNum)%Name)//&
           '", '//TRIM(cNumericFieldNames(7))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(8))THEN
      ConstCOPChiller(ChillerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(8))
      IF(ConstCOPChiller(ChillerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//', "'//TRIM(ConstCOPChiller(ChillerNum)%Name)//&
                       '" TRIM(cAlphaFieldNames(8)) "'//TRIM(cAlphaArgs(8)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF


  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO ChillerNum = 1, NumConstCOPChillers
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          ConstCOPChillerReport(ChillerNum)%Power,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Consumption [J]', &
          ConstCOPChillerReport(ChillerNum)%Energy,'System','Sum',ConstCOPChiller(ChillerNum)%Name, &
          ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Evap Heat Trans Rate [W]', &
          ConstCOPChillerReport(ChillerNum)%QEvap,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Heat Trans [J]', &
          ConstCOPChillerReport(ChillerNum)%EvapEnergy,'System','Sum',ConstCOPChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller Evap Water Inlet Temp [C]', &
          ConstCOPChillerReport(ChillerNum)%EvapInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Outlet Temp [C]', &
          ConstCOPChillerReport(ChillerNum)%EvapOutletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evap Water Mass Flow Rate [kg/s]', &
          ConstCOPChillerReport(ChillerNum)%Evapmdot,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller COP [W/W]', &
          ConstCOPChillerReport(ChillerNum)%ActualCOP,'System','Average',ConstCOPChiller(ChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Cond Heat Trans Rate [W]', &
          ConstCOPChillerReport(ChillerNum)%QCond,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cond Heat Trans [J]', &
          ConstCOPChillerReport(ChillerNum)%CondEnergy,'System','Sum',ConstCOPChiller(ChillerNum)%Name,  &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')

        !Condenser mass flow and outlet temp are valid for water cooled
     IF (ConstCOPChiller(ChillerNum)%CondenserType == WaterCooled)THEN
        CALL SetupOutputVariable('Chiller Cond Water Inlet Temp [C]', &
             ConstCOPChillerReport(ChillerNum)%CondInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Name)
        CALL SetupOutputVariable('Chiller Cond Water Outlet Temp [C]', &
             ConstCOPChillerReport(ChillerNum)%CondOutletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Name)
        CALL SetupOutputVariable('Chiller Cond Water Mass Flow Rate [kg/s]', &
             ConstCOPChillerReport(ChillerNum)%Condmdot,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     ELSEIF (ConstCOPChiller(ChillerNum)%CondenserType == AirCooled) THEN
        CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
             ConstCOPChillerReport(ChillerNum)%CondInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Name)
     ELSEIF (ConstCOPChiller(ChillerNum)%CondenserType == EvapCooled) THEN
        CALL SetupOutputVariable('Chiller Cond Air Inlet Temp [C]', &
             ConstCOPChillerReport(ChillerNum)%CondInletTemp,'System','Average',ConstCOPChiller(ChillerNum)%Name)
        IF(ConstCOPChiller(ChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
          CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
               ConstCOPChillerReport(ChillerNum)%BasinHeaterPower,'System','Average',ConstCOPChiller(ChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Basin Heater Electric Consumption [J]', &
               ConstCOPChillerReport(ChillerNum)%BasinHeaterConsumption,'System','Sum',ConstCOPChiller(ChillerNum)%Name, &
               ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
        END IF
     ENDIF
  END DO

RETURN
END SUBROUTINE GetConstCOPChillerInput

! End of Get Input subroutines for the Const COP Chiller Module
!******************************************************************************

SUBROUTINE InitConstCOPChiller(ChillNum,RunFlag, MyLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   September 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Electric Chiller components

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! Based on InitElectricChiller from Fred Buhl

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_ConstCOP, ScanPlantLoopsForObject, &
                              PlantSizeNotComplete, PlantSizesOkayToFinalize, LoopFlowStatus_NeedyIfLoopOn
  USE DataEnvironment, ONLY : StdBaroPress
  USE Psychrometrics,  ONLY : PsyRhoAirFnPbTdbW
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE FluidProperties, ONLY : GetDensityGlycol
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ChillNum     ! number of the current electric chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag      ! TRUE when chiller operating
  REAL(r64), INTENT(IN):: MyLoad
  LOGICAL, INTENT(IN)  :: FirstHVACIteration      ! initialize variables when TRUE

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitConstCOPChiller'
  REAL(r64), parameter        :: TempDesCondIn = 25.d0        ! Design condenser inlet temp. C


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: OneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvironFlag
  INTEGER :: CondInletNode      ! node number of water inlet node to the condenser
  INTEGER :: CondOutletNode     ! node number of water outlet node from the condenser
  INTEGER :: EvapInletNode
  INTEGER :: EvapOutletNode
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: mdot ! local mass flow rate
  REAL(r64) :: mdotCond ! local mass flow rate for condenser
  LOGICAL :: FatalError
  LOGICAL :: errFlag
          !FLOW
  ! Do the one time initializations
  IF (OneTimeFlag) THEN
    ALLOCATE(MyFlag(NumConstCOPChillers))
    ALLOCATE(MyEnvironFlag(NumConstCOPChillers))
    MyFlag = .TRUE.
    MyEnvironFlag = .TRUE.
    OneTimeFlag = .false.
  END IF

  EvapInletNode            = ConstCOPChiller(ChillNum)%EvapInletNodeNum
  EvapOutletNode           = ConstCOPChiller(ChillNum)%EvapOutletNodeNum
  CondInletNode            = ConstCOPChiller(ChillNum)%CondInletNodeNum
  CondOutletNode           = ConstCOPChiller(ChillNum)%CondOutletNodeNum

  ! Init more variables
  IF (MyFlag(ChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(ConstCOPChiller(ChillNum)%Name, &
                                 TypeOf_Chiller_ConstCOP, &
                                 ConstCOPChiller(ChillNum)%CWLoopNum, &
                                 ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                                 ConstCOPChiller(ChillNum)%CWBranchNum, &
                                 ConstCOPChiller(ChillNum)%CWCompNum,  &
                                 InletNodeNumber = ConstCOPChiller(ChillNum)%EvapInletNodeNum,  &
                                 errFlag=errFlag)
    IF (ConstCOPChiller(ChillNum)%CondenserType /= AirCooled .AND. &
        ConstCOPChiller(ChillNum)%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(ConstCOPChiller(ChillNum)%Name, &
                                   TypeOf_Chiller_ConstCOP, &
                                   ConstCOPChiller(ChillNum)%CDLoopNum, &
                                   ConstCOPChiller(ChillNum)%CDLoopSideNum, &
                                   ConstCOPChiller(ChillNum)%CDBranchNum, &
                                   ConstCOPChiller(ChillNum)%CDCompNum,  &
                                   InletNodeNumber = ConstCOPChiller(ChillNum)%CondInletNodeNum,  &
                                   errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ConstCOPChiller(ChillNum)%CWLoopNum,      &
                                          ConstCOPChiller(ChillNum)%CWLoopSideNum,  &
                                          ConstCOPChiller(ChillNum)%CDLoopNum,      &
                                          ConstCOPChiller(ChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_ConstCOP, .TRUE. )
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('CalcConstCOPChillerModel: Program terminated due to previous condition(s).')
    ENDIF


    IF (ConstCOPChiller(ChillNum)%VariableFlow) Then 
      ! reset flow priority
      PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%LoopSide(ConstCOPChiller(ChillNum)%CWLoopSideNum)% &
          Branch(ConstCOPChiller(ChillNum)%CWBranchNum)%Comp(ConstCOPChiller(ChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    
      ! check if setpoint on outlet node
      IF (Node(ConstCOPChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. ConstCOPChiller(ChillNum)%VariableFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(ConstCOPChiller(ChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ConstCOPChiller(ChillNum)%VariableFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(ConstCOPChiller(ChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. ConstCOPChiller(ChillNum)%VariableFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode chiller named ' // &
                                          TRIM(ConstCOPChiller(ChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Set Point Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              ConstCOPChiller(ChillNum)%VariableFlowErrDone = .TRUE.
            ENDIF
          ENDIF
        ENDIF
        ConstCOPChiller(ChillNum)%VariableFlowSetToLoop = .TRUE.
        Node(ConstCOPChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
      ENDIF
    ENDIF
    MyFlag(ChillNum)=.FALSE.
  ENDIF

     !Initialize critical Demand Side Variables at the beginning of each environment
  IF(MyEnvironFlag(ChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))Then
    IF (PlantSizeNotComplete) CALL SizeConstCOPChiller(ChillNum)
    rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                RoutineName)
    ConstCOPChiller(ChillNum)%EvapMassFlowRateMax = ConstCOPChiller(ChillNum)%EvapVolFlowRate * rho
    CALL InitComponentNodes(0.0D0,ConstCOPChiller(ChillNum)%EvapMassFlowRateMax,  &
                         EvapInletNode,        &
                         EvapOutletNode,       &
                         ConstCOPChiller(ChillNum)%CWLoopNum,               &
                         ConstCOPChiller(ChillNum)%CWLoopSideNum,           &
                         ConstCOPChiller(ChillNum)%CWBranchNum,             &
                         ConstCOPChiller(ChillNum)%CWCompNum)

          !init maximum available condenser flow rate
    IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN

      Node(CondInletNode)%Temp = TempDesCondIn

      rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                                  RoutineName)

      ConstCOPChiller(ChillNum)%CondMassFlowRateMax = rho * ConstCOPChiller(ChillNum)%CondVolFlowRate

      CALL InitComponentNodes(0.0D0,  ConstCOPChiller(ChillNum)%CondMassFlowRateMax,  &
                         CondInletNode,        &
                         CondOutletNode,       &
                         ConstCOPChiller(ChillNum)%CDLoopNum,               &
                         ConstCOPChiller(ChillNum)%CDLoopSideNum,           &
                         ConstCOPChiller(ChillNum)%CDBranchNum,             &
                         ConstCOPChiller(ChillNum)%CDCompNum)
    ELSE ! air or evap-air
      Node(CondInletNode)%MassFlowRate        = ConstCOPChiller(ChillNum)%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,TempDesCondIn,0.0D0,RoutineName)

      Node(CondOutletNode)%MassFlowrate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0
      Node(CondInletNode)%MassFlowRateMin       = 0.0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0
    END IF
    MyEnvironFlag(ChillNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvironFlag(ChillNum)=.true.
  ENDIF
  IF (ConstCOPChiller(ChillNum)%VariableFlow .AND. ConstCOPChiller(ChillNum)%VariableFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(ConstCOPChiller(ChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
  ENDIF

  IF (FirstHVACIteration) THEN
    IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
      mdot     = ConstCOPChiller(ChillNum)%EvapMassFlowRateMax
      mdotCond = ConstCOPChiller(ChillNum)%CondMassFlowRateMax
    ELSE
      mdot     = 0.d0
      mdotCond = 0.d0
    ENDIF
  ELSE
    IF ((MyLoad < 0.d0) .AND. RunFlag)  THEN
      mdot     = ConstCOPChillerReport(ChillNum)%Evapmdot
      mdotCond = ConstCOPChillerReport(ChillNum)%Condmdot
    ELSE
      mdot     = 0.d0
      mdotCond = 0.d0
    ENDIF
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              ConstCOPChiller(ChillNum)%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%CWCompNum)
  IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,         &
                                ConstCOPChiller(ChillNum)%CDLoopNum,     &
                                ConstCOPChiller(ChillNum)%CDLoopSideNum, &
                                ConstCOPChiller(ChillNum)%CDBranchNum,   &
                                ConstCOPChiller(ChillNum)%CDCompNum)
  ENDIF

  IF (ConstCOPChiller(ChillNum)%CondenserType == EvapCooled) THEN
    BasinHeaterPower       = 0.0d0
  ENDIF

END SUBROUTINE InitConstCOPChiller

SUBROUTINE SizeConstCOPChiller(ChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Constabt COP Chiller Components for which capacities and flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains evaporator flow rate from the plant sizing array. Calculates nominal capacity from
          ! the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          ! is calculated from the nominal capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant, ONLY : PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           ::  rho ! local fluid density
  REAL(r64)           ::  Cp  ! local fluid specific heat
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap          = ConstCOPChiller(ChillNum)%NomCap
  tmpEvapVolFlowRate = ConstCOPChiller(ChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = ConstCOPChiller(ChillNum)%CondVolFlowRate

  IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN
    IF (ConstCOPChiller(ChillNum)%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%PlantSizNum
    END IF
  END IF

  PltSizNum = PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%PlantSizNum

  IF (ConstCOPChiller(ChillNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidIndex,&
                                  'SizeConstCOPChiller')
        Cp = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidName,  &
                                   InitConvTemp,                      &
                                   PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidIndex, &
                                 'SizeConstCOPChiller')
        tmpNomCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                              * PlantSizData(PltSizNum)%DesVolFlowRate * ConstCOPChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%NomCap = tmpNomCap

      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%NomCap = tmpNomCap
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:ConstantCOP', ConstCOPChiller(ChillNum)%Name, &
                              'Nominal Capacity [W]', ConstCOPChiller(ChillNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Constant COP Chiller nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:ConstantCOP object='//TRIM(ConstCOPChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ConstCOPChiller(ChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate * ConstCOPChiller(ChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize)  ConstCOPChiller(ChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:ConstantCOP', ConstCOPChiller(ChillNum)%Name, &
                              'Design Chilled Water Flow Rate [m3/s]', &
                              ConstCOPChiller(ChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Constant COP Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:ConstantCOP object='//TRIM(ConstCOPChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ConstCOPChiller(ChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF ((ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) .AND. &
      (ConstCOPChiller(ChillNum)%CondVolFlowRate == AutoSize)) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho = GetDensityGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidName,  &
                               29.44d0, &
                               PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidIndex,&
                               'SizeConstCOPChiller')

        Cp = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                   29.44d0,                      &
                                   PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                   'SizeConstCOPChiller')
        tmpCondVolFlowRate  = tmpNomCap *  (1.d0 + 1.d0/ConstCOPChiller(ChillNum)%COP) &
                                            /( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ConstCOPChiller(ChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:ConstantCOP', ConstCOPChiller(ChillNum)%Name, &
                              'Design Condenser Water Flow Rate [m3/s]', &
                              ConstCOPChiller(ChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowContinueError('Autosizing of Constant COP Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Chiller:ConstantCOP object='//TRIM(ConstCOPChiller(ChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) &
      CALL RegisterPlantCompDesignFlow(ConstCOPChiller(ChillNum)%CondInletNodeNum, tmpCondVolFlowRate)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  !create predefined report
  IF (PlantSizesOkayToFinalize) THEN
    equipName = ConstCOPChiller(ChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:ConstantCOP')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ConstCOPChiller(ChillNum)%COP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ConstCOPChiller(ChillNum)%NomCap)
  ENDIF

  RETURN
END SUBROUTINE SizeConstCOPChiller


! Beginning of Const COP Chiller Model Subroutines
! *****************************************************************************

SUBROUTINE CalcConstCOPChillerModel(ChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       Richard Liesen Nov-Dec 2001; Jan 2002,
          !                      Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour, CurrentTime
  USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, &
                         ShowContinueErrorTimeStamp, ShowContinueError
  USE DataHVACGlobals, ONLY : TimeStepSys, SysTimeElapsed
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE DataPlant,       ONLY : ControlType_SeriesActive, MassFlowTol, PlantLoop, &
                              SimPlantEquipTypes, TypeOf_Chiller_ConstCOP, CompSetPtBasedSchemeType, &
                              CriteriaType_MassFlowRate
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: ChillNum
  REAL(r64)              :: MyLoad
  LOGICAL                :: RunFlag
  LOGICAL                :: FirstIteration  !unused1208
 ! INTEGER, INTENT(IN)    :: FlowLock
  INTEGER, INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), parameter        :: DeltaTemptol=0.0001d0          ! C - minimum significant mass flow rate
  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: EvapDeltaTemp
  REAL(r64)              :: TempEvapOutSetpoint   ! C - evaporator outlet temperature setpoint
  INTEGER                :: EvapInletNode
  INTEGER                :: EvapOutletNode
  INTEGER                :: CondInletNode
  INTEGER                :: CondOutletNode
!  LOGICAL,SAVE           :: PossibleSubCooling=.FALSE.
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  REAL(r64),SAVE  :: TimeStepSysLast=0.0     ! last system time step (used to check for downshifting)
  REAL(r64)       :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE  :: CurrentEndTimeLast=0.0  ! end time of time step for last simulation time step
  CHARACTER(len=6):: OutputChar = ' '        ! character string for warning messages
  REAL(r64)       :: Cp      ! local for fluid specif heat, for evaporator
  REAL(r64)       :: CpCond  ! local for fluid specif heat, for condenser

  EvapInletNode            = ConstCOPChiller(ChillNum)%EvapInletNodeNum
  EvapOutletNode           = ConstCOPChiller(ChillNum)%EvapOutletNodeNum
  CondInletNode            = ConstCOPChiller(ChillNum)%CondInletNodeNum
  CondOutletNode           = ConstCOPChiller(ChillNum)%CondOutletNodeNum

          !set module level chiller inlet and temperature variables
  LoopNum                  = ConstCOPChiller(ChillNum)%CWLoopNum
  LoopSideNum              = ConstCOPChiller(ChillNum)%CWLoopSideNum
  IF ((ConstCOPChiller(ChillNum)%VariableFlow) .OR. &
      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(ConstCOPChiller(ChillNum)%CWBranchNum) &
        %Comp(ConstCOPChiller(ChillNum)%CWCompNum)%CurOpSchemeType &
           == CompSetPtBasedSchemeType)          .OR. &
      (Node(EvapOutletNode)%TempSetPoint /= SensedNodeFlagValue) ) THEN
    TempEvapOutSetpoint = Node(EvapOutletNode)%TempSetPoint
  ELSE
    TempEvapOutSetpoint = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint
  ENDIF
  EvapDeltaTemp            = ABS(Node(EvapInletNode)%Temp - TempEvapOutSetpoint)
  EvapInletTemp            = Node(EvapInletNode)%Temp
  

          !If no component demand, or chiller OFF, or Chiller type set to 'Passive' by free
          !cooling heat exchanger, then set condenser side flow and heat transfer rates set to zero
  IF (MyLoad >= 0.d0 .OR. .NOT. Runflag) THEN

   !If Chiller load is 0 or greater or chiller is not running then leave the subroutine.Before leaving
   !if the component control is SERIESACTIVE we set the component flow to inlet flow so that
   !flow resolver will not shut down the branch
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    ELSE
      EvapMassFlowRate = 0.d0
      CALL SetComponentFlowRate( EvapMassFlowRate,  &
                                EvapInletNode , EvapOutletNode  , &
                                ConstCOPChiller(ChillNum)%CWLoopNum,     &
                                ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                                ConstCOPChiller(ChillNum)%CWBranchNum,   &
                                ConstCOPChiller(ChillNum)%CWCompNum)
    ENDIF
    IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN
      IF ( PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)% &
            LoopSide(ConstCOPChiller(ChillNum)%CDLoopSideNum)% &
              Branch(ConstCOPChiller(ChillNum)%CDBranchNum)%  &
                Comp(ConstCOPChiller(ChillNum)%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate           = Node(CondInletNode)%MassFlowrate
      ELSE
        CondMassFlowRate = 0.d0
        CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                                ConstCOPChiller(ChillNum)%CDLoopNum, &
                                ConstCOPChiller(ChillNum)%CDLoopSideNum, &
                                ConstCOPChiller(ChillNum)%CDBranchNum, &
                                ConstCOPChiller(ChillNum)%CDCompNum)
      ENDIF
    ENDIF

    EvapOutletTemp       = Node(EvapInletNode)%Temp
    CondOutletTemp       = Node(CondInletNode)%Temp

    Power                = 0.d0
    QEvaporator          = 0.d0
    QCondenser           = 0.d0
    Energy               = 0.d0
    EvaporatorEnergy     = 0.d0
    CondenserEnergy      = 0.d0

    IF (ConstCOPChiller(ChillNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ConstCOPChiller(ChillNum)%BasinHeaterPowerFTempDiff,&
                            ConstCOPChiller(ChillNum)%BasinHeaterSchedulePtr,&
                            ConstCOPChiller(ChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    ConstCOPChiller(ChillNum)%PrintMessage = .FALSE.
    RETURN
  END IF

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(ConstCOPChiller(ChillNum)%PrintMessage)THEN
        ConstCOPChiller(ChillNum)%MsgErrorCount = &
                         ConstCOPChiller(ChillNum)%MsgErrorCount + 1
!       Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (ConstCOPChiller(ChillNum)%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(ConstCOPChiller(ChillNum)%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(ConstCOPChiller(ChillNum)%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ConstCOPChiller(ChillNum)%MsgBuffer1)//' error continues.', &
           ConstCOPChiller(ChillNum)%ErrCount1,ReportMaxOf=ConstCOPChiller(ChillNum)%MsgDataLast,  &
           ReportMinOf=ConstCOPChiller(ChillNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

!   save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime



!otherwise the chiller is running...

  IF (ConstCOPChiller(ChillNum)%CondenserType == AirCooled) THEN !Condenser inlet temp = outdoor temp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb
!  Warn user if entering condenser temperature falls below 0C
      IF(Node(CondInletNode)%Temp .LT. 0.0 .and. .not. WarmupFlag) THEN
        ConstCOPChiller(ChillNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ConstCOPChiller(ChillNum)%MsgBuffer1 = 'CalcConstCOPChillerModel - Chiller:ConstantCOP "' &
                             //TRIM(ConstCOPChiller(ChillNum)%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        ConstCOPChiller(ChillNum)%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ConstCOPChiller(ChillNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ConstCOPChiller(ChillNum)%PrintMessage = .FALSE.
      ENDIF
  Else IF (ConstCOPChiller(ChillNum)%CondenserType == EvapCooled) THEN !Condenser inlet temp = (outdoor wet bulb)
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  Warn user if evap condenser wet bulb temperature falls below 10C
      IF(Node(CondInletNode)%Temp .LT. 10.0 .and. .not. WarmupFlag) THEN
        ConstCOPChiller(ChillNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ConstCOPChiller(ChillNum)%MsgBuffer1 = 'CalcConstCOPChillerModel - Chiller:ConstantCOP "' &
                             //TRIM(ConstCOPChiller(ChillNum)%Name)// &
                             '" - Evap Cooled Condenser Inlet Temperature below 10C'
        ConstCOPChiller(ChillNum)%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ConstCOPChiller(ChillNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ConstCOPChiller(ChillNum)%PrintMessage = .FALSE.
      ENDIF
  ENDIF ! End of the Air Cooled/Evap Cooled Logic block

     ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp            = Node(CondInletNode)%Temp

        !Set condenser flow rate
  IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN
    CondMassFlowRate = ConstCOPChiller(ChillNum)%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              ConstCOPChiller(ChillNum)%CDLoopNum, &
                              ConstCOPChiller(ChillNum)%CDLoopSideNum, &
                              ConstCOPChiller(ChillNum)%CDBranchNum, &
                              ConstCOPChiller(ChillNum)%CDCompNum)
    CALL PullCompInterconnectTrigger(ConstCOPChiller(ChillNum)%CWLoopNum, &
                                     ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                                     ConstCOPChiller(ChillNum)%CWBranchNum, &
                                     ConstCOPChiller(ChillNum)%CWCompNum, &
                                     ConstCOPChiller(ChillNum)%CondMassFlowIndex,              &
                                     ConstCOPChiller(ChillNum)%CDLoopNum, &
                                     ConstCOPChiller(ChillNum)%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)

    IF (CondMassFlowRate < MassFlowTol) RETURN

  END IF

    ! If FlowLock is True, the new resolved mdot is used to update Power, QEvap, Qcond, and
    ! condenser side outlet temperature.

  Cp = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidName,  &
                         Node(EvapInletNode)%Temp,                      &
                         PlantLoop(ConstCOPChiller(ChillNum)%CWLoopNum)%FluidIndex, &
                         'CalcConstCOPChillerModel')

  IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock==0) THEN
     ConstCOPChiller(ChillNum)%PossibleSubCooling = .FALSE.
     QEvaporator = ABS(MyLoad)
     Power = ABS(MyLoad) / ConstCOPChiller(ChillNum)%COP

     ! Either set the flow to the Constant value or caluclate the flow for the variable volume
     If(ConstCOPChiller(ChillNum)%ConstantFlow)Then

          ! Start by assuming max (design) flow
        EvapMassFlowRate = ConstCOPChiller(ChillNum)%EvapMassFlowRateMax
          ! Use SetComponentFlowRate to decide actual flow
          Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%CWCompNum)
          ! Evaluate delta temp based on actual flow rate
        IF (EvapMassFlowRate /= 0.0D0) THEN
          EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
        ELSE
          EvapDeltaTemp = 0.0D0
        ENDIF
          ! Evaluate outlet temp based on delta
        EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

     Else IF(ConstCOPChiller(ChillNum)%VariableFlow)Then

        ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
        EvapDeltaTemp = ABS(Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint)

        IF (EvapDeltaTemp > DeltaTemptol) THEN
          EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
          IF((EvapMassFlowRate - ConstCOPChiller(ChillNum)%EvapMassFlowRateMax) .GT. MassFlowTol) &
                    ConstCOPChiller(ChillNum)%PossibleSubCooling = .TRUE.
          !Check to see if the Maximum is exceeded, if so set to maximum
          EvapMassFlowRate = MIN(ConstCOPChiller(ChillNum)%EvapMassFlowRateMax, EvapMassFlowRate)
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%CWCompNum)

          EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
        ELSE
            ! Try to request zero flow
          EvapMassFlowRate=0.0
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%CWCompNum)
            ! No deltaT since component is not running
          EvapOutletTemp = Node(EvapInletNode)%Temp

        END IF
     End If  !End of Constant or Variable Flow If Block for FlowLock = 0 (or making a flow request)
  ELSE  ! If FlowLock is True

    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ConstCOPChiller(ChillNum)%CWLoopNum,     &
                              ConstCOPChiller(ChillNum)%CWLoopSideNum, &
                              ConstCOPChiller(ChillNum)%CWBranchNum,   &
                              ConstCOPChiller(ChillNum)%CWCompNum)
!   Some other component set the flow to 0. No reason to continue with calculations.
     IF(EvapMassFlowRate == 0.0d0)THEN
       MyLoad = 0.0d0
       IF (ConstCOPChiller(ChillNum)%CondenserType == EvapCooled) THEN
         CALL CalcBasinHeaterPower(ConstCOPChiller(ChillNum)%BasinHeaterPowerFTempDiff,&
                              ConstCOPChiller(ChillNum)%BasinHeaterSchedulePtr,&
                              ConstCOPChiller(ChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
       ENDIF
       ConstCOPChiller(ChillNum)%PrintMessage = .FALSE.
       RETURN
     END IF

     !Recalculate the Delts Temp
        IF(ConstCOPChiller(ChillNum)%PossibleSubCooling) THEN
         QEvaporator = ABS(MyLoad)
         EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
         EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
         IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
          EvapOutletTemp = Node(EvapOutletNode)%TempMin
          EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
          QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
         END IF
        ELSE
         EvapDeltaTemp = Node(EvapInletNode)%Temp - TempEvapOutSetpoint
          !Calculate the evaporator heat transfer at the specified flow which could have changed
          !  in the Flow Resolution step.
         QEvaporator = ABS(EvapMassFlowRate*Cp*EvapDeltaTemp)
         EvapOutletTemp = TempEvapOutSetpoint
       END IF
        !Check that the Evap outlet temp honors both plant loop temp low limit and also the chiller low limit
         IF(EvapOutletTemp .LT. Node(EvapOutletNode)%TempMin) THEN
          IF((Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempMin) .GT. DeltaTempTol) THEN
           EvapOutletTemp = Node(EvapOutletNode)%TempMin
           EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
           QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          ELSE
           EvapOutletTemp = Node(EvapInletNode)%Temp
           EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTemp
           QEvaporator = EvapMassFlowRate*Cp*EvapDeltaTemp
          END IF
         END IF
     ! If load exceeds the distributed load set to the distributed load
     If(QEvaporator > ABS(MyLoad)) Then
       If(EvapMassFlowRate > MassFlowTol) THEN
           QEvaporator = ABS(MyLoad)
           EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
           EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
       Else
           QEvaporator = 0.0
           EvapOutletTemp = Node(EvapInletNode)%Temp
       End If
     End IF

     ! Checks QEvaporator on the basis of the machine limits.
     If(QEvaporator > ConstCOPChiller(ChillNum)%NomCap)Then
       If(EvapMassFlowRate > MassFlowTol) THEN
           QEvaporator = ConstCOPChiller(ChillNum)%NomCap
           EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
           EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
       Else
           QEvaporator = 0.0
           EvapOutletTemp = Node(EvapInletNode)%Temp
       End If
     End If
      !Calculate the Power consumption of the Const COP chiller which is a simplified calculation
     Power = QEvaporator / ConstCOPChiller(ChillNum)%COP
     IF(EvapMassFlowRate == 0.0) THEN
      QEvaporator = 0.0
      EvapOutletTemp = Node(EvapInletNode)%Temp
      Power = 0.0
      ConstCOPChiller(ChillNum)%PrintMessage = .FALSE.
     END IF
     IF(QEvaporator == 0.0d0 .AND. ConstCOPChiller(ChillNum)%CondenserType == EvapCooled) THEN
        CALL CalcBasinHeaterPower(ConstCOPChiller(ChillNum)%BasinHeaterPowerFTempDiff,&
                                  ConstCOPChiller(ChillNum)%BasinHeaterSchedulePtr,&
                                  ConstCOPChiller(ChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
     END IF

  END IF !This is the end of the FlowLock Block

    !QCondenser is calculated the same for each type, but the power consumption should be different
    !  depending on the performance coefficients used for the chiller model.
    QCondenser = Power + QEvaporator

    IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN
       CpCond = GetSpecificHeatGlycol(PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidName,  &
                                      CondInletTemp,                      &
                                      PlantLoop(ConstCOPChiller(ChillNum)%CDLoopNum)%FluidIndex, &
                                      'CalcConstCOPChillerModel')
       IF (CondMassFlowRate > MassFlowTol) THEN
         CondOutletTemp = QCondenser/CondMassFlowRate/CpCond + CondInletTemp
       ELSE
         CALL ShowSevereError('CalcConstCOPChillerModel: Condenser flow = 0, for CONST COP Chiller='//  &
                              TRIM(ConstCOPChiller(ChillNum)%Name))
         CALL ShowContinueErrorTimeStamp(' ')

       END IF
    ELSE ! Air Cooled or Evap Cooled
         !  Set condenser outlet temp to condenser inlet temp for Air Cooled or Evap Cooled
         !  since there is no CondMassFlowRate and would divide by zero
      CondOutletTemp = CondInletTemp
    END IF

        !Calculate Energy
   CondenserEnergy  = QCondenser*TimeStepSys*SecInHour
   Energy           = Power*TimeStepSys*SecInHour
   EvaporatorEnergy = QEvaporator*TimeStepSys*SecInHour

 !check for problems BG 9/12/06 (deal with observed negative energy results)
  IF (Energy < 0.0d0) then  ! there is a serious problem

    IF (ConstCOPChiller(ChillNum)%CondenserType == WaterCooled) THEN
     ! first check for run away condenser loop temps (only reason yet to be observed for this?)
      IF (CondInletTemp > 70.0d0 )  then
        CALL ShowSevereError('CalcConstCOPChillerModel: Condenser loop inlet temperatures over 70.0 C for ConstCOPChiller='//  &
                            TRIM(ConstCOPChiller(ChillNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Condenser loop water temperatures are too high at'//trim(RoundSigDigits(CondInletTemp,2)) )
        CALL ShowContinueError('Check input for condenser plant loop, especially cooling tower')
        CALL showContinueError('Evaporator inlet temperature: '//trim(RoundSigDigits(Node(EvapInletNode)%Temp,2)) )

        CALL ShowFatalError('Program Terminates due to previous error condition')
      ENDIF
    ENDIF
    ! If makes it here, set limits, chiller can't have negative energy/power
    ! proceeding silently for now but may want to throw error here
    Power = 0.0
    Energy = 0.0

  ENDIF
RETURN
END SUBROUTINE CalcConstCOPChillerModel


! End of Const COP Chiller Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the Const COP Chiller Module
! *****************************************************************************

SUBROUTINE UpdateConstCOPChillerRecords(MyLoad,RunFlag,Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:


            ! METHODOLOGY EMPLOYED:
            ! REFERENCES:

            ! USE STATEMENTS:
USE DataGlobals,     ONLY: SecInHour
USE DataHVACGlobals, ONLY: TimeStepSys

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)            :: MyLoad !unused1208
  LOGICAL              :: RunFlag !unused1208
  INTEGER              :: Num

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: EvapInletNode
  INTEGER                  :: EvapOutletNode
  INTEGER                  :: CondInletNode
  INTEGER                  :: CondOutletNode
  REAL(r64)                :: ReportingConstant  ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode            = ConstCOPChiller(Num)%EvapInletNodeNum
  EvapOutletNode           = ConstCOPChiller(Num)%EvapOutletNodeNum
  CondInletNode            = ConstCOPChiller(Num)%CondInletNodeNum
  CondOutletNode           = ConstCOPChiller(Num)%CondOutletNodeNum


  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag)THEN !Chiller not running so pass inlet states to outlet states
    ConstCOPChillerReport(Num)%Power            = 0.0
    ConstCOPChillerReport(Num)%QEvap            = 0.0
    ConstCOPChillerReport(Num)%QCond            = 0.0
    ConstCOPChillerReport(Num)%Energy           = 0.0
    ConstCOPChillerReport(Num)%EvapEnergy       = 0.0
    ConstCOPChillerReport(Num)%CondEnergy       = 0.0
    ConstCOPChillerReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    ConstCOPChillerReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    ConstCOPChillerReport(Num)%CondOutletTemp   = Node(CondInletNode)%Temp
    ConstCOPChillerReport(Num)%EvapOutletTemp   = Node(EvapInletNode)%Temp
    ConstCOPChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    ConstCOPChillerReport(Num)%Condmdot         = CondMassFlowRate
    ConstCOPChillerReport(Num)%ActualCOP        = 0.0
    IF (ConstCOPChiller(Num)%CondenserType == EvapCooled) THEN
      ConstCOPChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      ConstCOPChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF


          !set outlet node temperatures
    Node(EvapOutletNode)%Temp                   = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp                   = Node(CondInletNode)%Temp

  ELSE
    ConstCOPChillerReport(Num)%Power            = Power
    ConstCOPChillerReport(Num)%QEvap            = QEvaporator
    ConstCOPChillerReport(Num)%QCond            = QCondenser
    ConstCOPChillerReport(Num)%Energy           = Energy
    ConstCOPChillerReport(Num)%EvapEnergy       = EvaporatorEnergy
    ConstCOPChillerReport(Num)%CondEnergy       = CondenserEnergy
    ConstCOPChillerReport(Num)%CondInletTemp    = Node(CondInletNode)%Temp
    ConstCOPChillerReport(Num)%EvapInletTemp    = Node(EvapInletNode)%Temp
    ConstCOPChillerReport(Num)%CondOutletTemp   = CondOutletTemp
    ConstCOPChillerReport(Num)%EvapOutletTemp   = EvapOutletTemp
    ConstCOPChillerReport(Num)%Evapmdot         = EvapMassFlowRate
    ConstCOPChillerReport(Num)%Condmdot         = CondMassFlowRate
    IF (Power .ne. 0.0) THEN
       ConstCOPChillerReport(Num)%ActualCOP = QEvaporator/Power
    ELSE
       ConstCOPChillerReport(Num)%ActualCOP = 0.0
    END IF
    IF (ConstCOPChiller(Num)%CondenserType == EvapCooled) THEN
      ConstCOPChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      ConstCOPChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

          !set outlet node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp
  END IF

RETURN
END SUBROUTINE UpdateConstCOPChillerRecords

! End of Record Keeping subroutines for the Const COP Chiller Module
! *****************************************************************************


END MODULE ChillerConstCOP



!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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
