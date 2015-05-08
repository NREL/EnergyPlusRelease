!Note: Multiple Chiller Modules included in this file.
!      Find the chiller you are looking for by searching for "ChillerModule"

!      The Electric EIR and Reformulated EIR chiller models are similar.
!      They only differ in the independent variable used to evaluate the performance curves.
!      Since the Reformulated EIR chiller uses outlet condenser water temperature as an
!      independent variable, iteration is required to converge on a solution.
!
! Contents:
! ChillerElectricEIR
! ChillerReformulatedEIR
!
MODULE ChillerElectricEIR  ! DOE-2 Electric ChillerModule

          ! MODULE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2004
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !                      Brent Griffith, NREL, Sept 2010, revised for plant changes
          !                      generalized fluid properties
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !  This module simulates the performance of the electric vapor
          !  compression chiller used in DOE-2.

          ! METHODOLOGY EMPLOYED:
          !  Once the PlantLoopManager determines that the Electric EIR chiller
          !  is available to meet a loop cooling demand, it calls SimElectricEIRChiller
          !  which in turn calls the electric EIR model. The EIR chiller model is based on
          !  polynomial fits of chiller performance data.

          ! REFERENCES:
          ! 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: MaxNameLength, InitConvTemp
USE DataInterfaces
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE Psychrometrics,  ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW
USE General,         ONLY: TrimSigDigits
USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! MODULE PARAMETER DEFINITIONS:
! Chiller type parameters
INTEGER, PARAMETER :: AirCooled         = 1
INTEGER, PARAMETER :: WaterCooled       = 2
INTEGER, PARAMETER :: EvapCooled        = 3

!chiller flow modes
INTEGER, PARAMETER :: FlowModeNotSet           = 200
INTEGER, PARAMETER :: ConstantFlow             = 201
INTEGER, PARAMETER :: NotModulated             = 202
INTEGER, PARAMETER :: LeavingSetpointModulated = 203

          ! MODULE VARIABLE DECLARATIONS:
PRIVATE
INTEGER        :: NumElectricEIRChillers         = 0   ! Number of electric EIR chillers specified in input
REAL(r64)      :: CondMassFlowRate               = 0.0d0 ! Condenser mass flow rate [kg/s]
REAL(r64)      :: EvapMassFlowRate               = 0.0d0 ! Evaporator mass flow rate [kg/s]
REAL(r64)      :: CondOutletTemp                 = 0.0d0 ! Condenser outlet temperature [C]
REAL(r64)      :: CondOutletHumRat               = 0.0d0 ! Condenser outlet humidity ratio [kg/kg]
REAL(r64)      :: EvapOutletTemp                 = 0.0d0 ! Evaporator outlet temperature [C]
REAL(r64)      :: Power                          = 0.0d0 ! Rate of chiller electric energy use [W]
REAL(r64)      :: QEvaporator                    = 0.0d0 ! Rate of heat transfer to the evaporator coil [W]
REAL(r64)      :: QCondenser                     = 0.0d0 ! Rate of heat transfer to the condenser coil [W]
REAL(r64)      :: QHeatRecovered                 = 0.0d0 ! Rate of heat transfer to the heat recovery coil [W]
REAL(r64)      :: HeatRecOutletTemp              = 0.0d0 ! Heat recovery outlet temperature [C]
REAL(r64)      :: CondenserFanPower              = 0.0d0 ! Condenser Fan Power (fan cycles with compressor) [W]
REAL(r64)      :: ChillerCapFT                   = 0.0d0 ! Chiller capacity fraction (evaluated as a function of temperature)
REAL(r64)      :: ChillerEIRFT                   = 0.0d0 ! Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
REAL(r64)      :: ChillerEIRFPLR                 = 0.0d0 ! Chiller EIR as a function of part-load ratio (PLR)
REAL(r64)      :: ChillerPartLoadRatio           = 0.0d0 ! Chiller part-load ratio (PLR)
REAL(r64)      :: ChillerCyclingRatio            = 0.0d0 ! Chiller cycling ratio
REAL(r64)      :: BasinHeaterPower               = 0.0d0 ! Basin heater power (W)
REAL(r64)      :: ChillerFalseLoadRate           = 0.0d0 ! Chiller false load over and above the water-side load [W]
REAL(r64)      :: AvgCondSinkTemp                = 0.d0!  condenser temperature value for use in curves [C]

TYPE ElectricEIRChillerSpecs
  CHARACTER(len=MaxNameLength) :: Name           =' '  ! User identifier
  INTEGER           :: TypeNum = 0                     ! plant loop type identifier
  CHARACTER(len=MaxNameLength) :: EIRFPLRName    =' '  ! EIRPLR curve name
  INTEGER           :: CondenserType             = 0   ! Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
  REAL(r64)         :: RefCap                    = 0.0d0 ! Reference capacity of chiller [W]
  REAL(r64)         :: RefCOP                    = 0.0d0 ! Reference coefficient of performance [W/W]
  INTEGER           :: FlowMode          = FlowModeNotSet ! one of 3 modes for componet flow during operation
  LOGICAL           :: ModulatedFlowSetToLoop  =.FALSE. ! True if the setpoint is missing at the outlet node
  LOGICAL           :: ModulatedFlowErrDone    =.FALSE. ! true if setpoint warning issued
  LOGICAL           :: HRSPErrDone            =.FALSE. ! TRUE if set point warning issued for heat recovery loop
  REAL(r64)         :: EvapVolFlowRate           = 0.0d0 ! Reference water volumetric flow rate through the evaporator [m3/s]
  REAL(r64)         :: EvapMassFlowRateMax       = 0.0d0 ! Reference water mass flow rate through evaporator [kg/s]
  REAL(r64)         :: CondVolFlowRate           = 0.0d0 ! Reference water volumetric flow rate through the condenser [m3/s]
  REAL(r64)         :: CondMassFlowRateMax       = 0.0d0 ! Reference water mass flow rate through condenser [kg/s]
  REAL(r64)         :: CondenserFanPowerRatio    = 0.0d0 ! Reference power of condenser fan to capacity ratio, W/W
  REAL(r64)         :: CompPowerToCondenserFrac  = 0.0d0 ! Fraction of compressor electric power rejected by condenser [0 to 1]
  INTEGER           :: EvapInletNodeNum          = 0   ! Node number on the inlet side of the plant (evaporator side)
  INTEGER           :: EvapOutletNodeNum         = 0   ! Node number on the outlet side of the plant (evaporator side)
  INTEGER           :: CondInletNodeNum          = 0   ! Node number on the inlet side of the condenser
  INTEGER           :: CondOutletNodeNum         = 0   ! Node number on the outlet side of the condenser
  REAL(r64)         :: MinPartLoadRat            = 0.0d0 ! Minimum allowed operating fraction of full load
  REAL(r64)         :: MaxPartLoadRat            = 0.0d0 ! Maximum allowed operating fraction of full load
  REAL(r64)         :: OptPartLoadRat            = 0.0d0 ! Optimal operating fraction of full load
  REAL(r64)         :: MinUnLoadRat              = 0.0d0 ! Minimum unloading ratio
  REAL(r64)         :: TempRefCondIn             = 0.0d0 ! The reference secondary loop fluid temperature
                                                       ! at the chiller condenser side inlet [C]
  REAL(r64)         :: TempRefEvapOut            = 0.0d0 ! The reference primary loop fluid temperature
                                                       ! at the chiller evaporator side outlet [C]
  REAL(r64)         :: TempLowLimitEvapOut       = 0.0d0 ! Low temperature shut off [C]
  REAL(r64)         :: DesignHeatRecVolFlowRate  = 0.0d0 ! Design water volumetric flow rate through heat recovery loop [m3/s]
  REAL(r64)         :: DesignHeatRecMassFlowRate = 0.0d0 ! Design water mass flow rate through heat recovery loop [kg/s]
  REAL(r64)         :: SizFac                    = 0.0d0 ! sizing factor
  REAL(r64)         :: BasinHeaterPowerFTempDiff = 0.0d0 ! Basin heater capacity per degree C below setpoint (W/C)
  REAL(r64)         :: BasinHeaterSetPointTemp   = 0.0d0 ! setpoint temperature for basin heater operation (C)
  LOGICAL           :: HeatRecActive         = .False. ! True when entered Heat Rec Vol Flow Rate > 0
  INTEGER           :: HeatRecInletNodeNum       = 0   ! Node number for the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum      = 0   ! Node number for the heat recovery outlet side of the condenser
  REAL(r64)         :: HeatRecCapacityFraction   = 0.d0 ! user input for heat recovery capacity fraction []
  REAL(r64)         :: HeatRecMaxCapacityLimit   = 0.d0 ! Capacity limit for Heat recovery, one time calc [W]
  INTEGER           :: HeatRecSetpointNodeNum    = 0    ! index for system node with the heat recover leaving setpoint
  INTEGER           :: HeatRecInletLimitSchedNum = 0    ! index for schedule for the inlet high limit for heat recovery operation
  INTEGER           :: ChillerCapFT              = 0   ! Index for the total cooling capacity modifier curve
                                                       ! (function of leaving chilled water temperature and
                                                       !  entering condenser fluid temperature)
  INTEGER           :: ChillerEIRFT              = 0   ! Index for the energy input ratio modifier curve
                                                       ! (function of leaving chilled water temperature and
                                                       !  entering condenser fluid temperature)
  INTEGER           :: ChillerEIRFPLR            = 0   ! Index for the EIR vs part-load ratio curve
!  INTEGER           :: CondFanPowerFCap          = 0   ! Condenser fan capacity as a function of chiller capacity
  INTEGER           :: ChillerCapFTError         = 0   ! Used for negative capacity as a function of temp warnings
  INTEGER           :: ChillerCapFTErrorIndex    = 0   ! Used for negative capacity as a function of temp warnings
  INTEGER           :: ChillerEIRFTError         = 0   ! Used for negative EIR as a function of temp warnings
  INTEGER           :: ChillerEIRFTErrorIndex    = 0   ! Used for negative EIR as a function of temp warnings
  INTEGER           :: ChillerEIRFPLRError       = 0   ! Used for negative EIR as a function of PLR warnings
  INTEGER           :: ChillerEIRFPLRErrorIndex  = 0   ! Used for negative EIR as a function of PLR warnings
  REAL(r64)         :: ChillerEIRFPLRMin         = 0.0d0 ! Minimum value of PLR from EIRFPLR curve
  REAL(r64)         :: ChillerEIRFPLRMax         = 0.0d0 ! Maximum value of PLR from EIRFPLR curve
  INTEGER           :: DeltaTErrCount            = 0   ! Evaporator delta T equals 0 for variable flow chiller warning messages
  INTEGER           :: DeltaTErrCountIndex       = 0   ! Index to evaporator delta T = 0 for variable flow chiller warning messages
  INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
  INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
  INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
  INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
  INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
  INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
  INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
  INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
  INTEGER           :: BasinHeaterSchedulePtr  = 0   ! Pointer to basin heater schedule
  INTEGER           :: CondMassFlowIndex = 0
  CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
  CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
  REAL(r64)         :: MsgDataLast   = 0.0d0 ! value of data when warning occurred (passed to Recurring Warn)
  LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
  INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
  INTEGER           :: ErrCount1     = 0   ! for recurring error messages
  LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE ElectricEIRChillerSpecs

TYPE ReportEIRVars
  REAL(r64)    :: ChillerPartLoadRatio  = 0.0d0 ! reporting: Chiller PLR (Load/Capacity)
  REAL(r64)    :: ChillerCyclingRatio   = 0.0d0 ! reporting: Chiller cycling ratio (time on/time step)
  REAL(r64)    :: ChillerFalseLoadRate  = 0.0d0 ! reporting: Chiller false load over and above water side load [J]
  REAL(r64)    :: ChillerFalseLoad      = 0.0d0 ! reporting: Chiller false load over and above water side load [W]
  REAL(r64)    :: Power                 = 0.0d0 ! reporting: Chiller power, W
  REAL(r64)    :: QEvap                 = 0.0d0 ! reporting: Evaporator heat transfer rate [W]
  REAL(r64)    :: QCond                 = 0.0d0 ! reporting: Condenser heat transfer rate [W]
  REAL(r64)    :: Energy                = 0.0d0 ! reporting: Chiller electric consumption [J]
  REAL(r64)    :: EvapEnergy            = 0.0d0 ! reporting: Evaporator heat transfer energy [J]
  REAL(r64)    :: CondEnergy            = 0.0d0 ! reporting: Condenser heat transfer energy [J]
  REAL(r64)    :: CondInletTemp         = 0.0d0 ! reporting: Condenser inlet temperature [C]
  REAL(r64)    :: EvapInletTemp         = 0.0d0 ! reporting: Evaporator inlet temperature [C]
  REAL(r64)    :: CondOutletTemp        = 0.0d0 ! reporting: Condenser outlet temperature [C]
  REAL(r64)    :: EvapOutletTemp        = 0.0d0 ! reporting: Evaporator outlet temperature [C]
  REAL(r64)    :: Evapmdot              = 0.0d0 ! reporting: Evaporator mass flow rate [kg/s]
  REAL(r64)    :: Condmdot              = 0.0d0 ! reporting: Condenser mass flow rate [kg/s]
  REAL(r64)    :: ActualCOP             = 0.0d0 ! reporting: Coefficient of performance
  REAL(r64)    :: QHeatRecovery         = 0.0d0 ! reporting: Heat recovered from water-cooled condenser [W]
  REAL(r64)    :: EnergyHeatRecovery    = 0.0d0 ! reporting: Energy recovered from water-cooled condenser [J]
  REAL(r64)    :: HeatRecInletTemp      = 0.0d0 ! reporting: Heat reclaim inlet temperature [C]
  REAL(r64)    :: HeatRecOutletTemp     = 0.0d0 ! reporting: Heat reclaim outlet temperature [C]
  REAL(r64)    :: HeatRecMassFlow       = 0.0d0 ! reporting: Heat reclaim mass flow rate [kg/s]
  REAL(r64)    :: ChillerCondAvgTemp    = 0.d0  ! reporting: average condenser temp for curves with Heat recovery [C]
  REAL(r64)    :: ChillerCapFT          = 0.0d0 ! reporting: Chiller capacity curve output value
  REAL(r64)    :: ChillerEIRFT          = 0.0d0 ! reporting: Chiller EIRFT curve output value
  REAL(r64)    :: ChillerEIRFPLR        = 0.0d0 ! reporting: Chiller EIRFPLR curve output value
  REAL(r64)    :: CondenserFanPowerUse  = 0.0d0 ! reporting: Air-cooled condenser fan power [W]
  REAL(r64)    :: CondenserFanEnergyConsumption = 0.0d0 ! reporting: Air-cooled condenser fan energy [J]
  REAL(r64)    :: BasinHeaterPower       = 0.0d0  ! Basin heater power (W)
  REAL(r64)    :: BasinHeaterConsumption = 0.0d0  ! Basin heater energy consumption (J)
END TYPE ReportEIRVars

TYPE (ElectricEIRChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: ElectricEIRChiller  ! Dimension to number of machines

TYPE(ReportEIRVars), ALLOCATABLE, DIMENSION(:) ::ElectricEIRChillerReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

LOGICAL     :: GetInputEIR = .TRUE. ! When TRUE, calls subroutine to read input file.

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ChillerElectricEIR
PRIVATE    CalcElectricEIRChillerModel
PRIVATE    GetElectricEIRChillerInput
PRIVATE    InitElectricEIRChiller
PRIVATE    SizeElectricEIRChiller
PRIVATE    UpdateElectricEIRChillerRecords
PRIVATE    EIRChillerHeatRecovery
PUBLIC     SimElectricEIRChiller
!PUBLIC     SimEIRChillerHeatRecovery

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Electric EIR Chiller Module Driver Subroutine
!*************************************************************************

SUBROUTINE SimElectricEIRChiller(EIRChillerType,EIRChillerName,EquipFlowCtrl, CompIndex,LoopNum, RunFlag,FirstIteration, &
                                 InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor,  &
                                 TempCondInDesign,TempEvapOutDesign)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This is the electric EIR chiller model driver. It gets the input for the
          !  model, initializes simulation variables, calls the appropriate model and sets
          !  up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY : FindItemInList
  USE PlantUtilities, ONLY : UpdateChillerComponentCondenserSide, UpdateComponentHeatRecoverySide
  USE DataPlant,      ONLY : TypeOf_Chiller_ElectricEIR
  USE DataSizing,     ONLY : CurLoopNum

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: EIRChillerType   ! Type of chiller
  CHARACTER(len=*), INTENT(IN) :: EIRChillerName   ! User specified name of chiller
  INTEGER, INTENT(IN)          :: EquipFlowCtrl    ! Flow control mode for the equipment
  LOGICAL, INTENT(IN)          :: RunFlag          ! Simulate chiller when TRUE
  LOGICAL, INTENT(IN)          :: FirstIteration   ! Initialize variables when TRUE
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip    ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad           ! Loop demand component will meet
  REAL(r64), INTENT(INOUT)     :: MinCap           ! Minimum operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: MaxCap           ! Maximum operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: OptCap           ! Optimal operating capacity of chiller [W]
  INTEGER, INTENT(INOUT)       :: CompIndex        ! Chiller number pointer
  INTEGER, INTENT(IN)          :: LoopNum          ! plant loop index pointer
  LOGICAL, INTENT(IN)          :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)     :: SizingFactor     ! sizing factor
  REAL(r64), INTENT(INOUT)     :: TempCondInDesign
  REAL(r64), INTENT(INOUT)     :: TempEvapOutDesign

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: EIRChillNum      ! Chiller number pointer
  INTEGER   :: LoopSide

  IF (GetInputEIR) THEN
    CALL GetElectricEIRChillerInput
    GetInputEIR = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    EIRChillNum = FindItemInList(EIRChillerName,ElectricEIRChiller%Name,NumElectricEIRChillers)
    IF (EIRChillNum == 0) THEN
      CALL ShowFatalError('SimElectricEIRChiller: Specified Chiller not one of Valid EIR Electric Chillers='//TRIM(EIRChillerName))
    ENDIF
    CompIndex=EIRChillNum
  ELSE
    EIRChillNum=CompIndex
    IF (EIRChillNum > NumElectricEIRChillers .or. EIRChillNum < 1) THEN
      CALL ShowFatalError('SimElectricEIRChiller:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(EIRChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumElectricEIRChillers))//  &
                          ', Entered Unit name='//TRIM(EIRChillerName))
    ENDIF
    IF (CheckEquipName(EIRChillNum)) THEN
      IF (EIRChillerName /= ElectricEIRChiller(EIRChillNum)%Name) THEN
        CALL ShowFatalError('SimElectricEIRChiller: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(EIRChillNum))// &
                            ', Unit name='//TRIM(EIRChillerName)//', stored Unit Name for that index='//  &
                            TRIM(ElectricEIRChiller(EIRChillNum)%Name))
      ENDIF
      CheckEquipName(EIRChillNum)=.false.
    ENDIF
  ENDIF


  IF (InitLoopEquip) THEN
    TempEvapOutDesign  = ElectricEIRChiller(EIRChillNum)%TempRefEvapOut
    TempCondInDesign   = ElectricEIRChiller(EIRChillNum)%TempRefCondIn
    CALL InitElectricEIRChiller(EIRChillNum,RunFlag,MyLoad)
    CALL SizeElectricEIRChiller(EIRChillNum)
    IF (LoopNum == ElectricEIRChiller(EIRChillNum)%CWLoopNum) THEN
      MinCap = ElectricEIRChiller(EIRChillNum)%RefCap*ElectricEIRChiller(EIRChillNum)%MinPartLoadRat
      MaxCap = ElectricEIRChiller(EIRChillNum)%RefCap*ElectricEIRChiller(EIRChillNum)%MaxPartLoadRat
      OptCap = ElectricEIRChiller(EIRChillNum)%RefCap*ElectricEIRChiller(EIRChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = ElectricEIRChiller(EIRChillNum)%SizFac
    END IF
    RETURN
  END IF

  IF (LoopNum == ElectricEIRChiller(EIRChillNum)%CWLoopNum) THEN
    CALL InitElectricEIRChiller(EIRChillNum,RunFlag,MyLoad)
    CALL CalcElectricEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
    CALL UpdateElectricEIRChillerRecords(MyLoad,RunFlag,EIRChillNum)

  ELSEIF (LoopNum == ElectricEIRChiller(EIRChillNum)%CDLoopNum) THEN
    LoopSide = ElectricEIRChiller(EIRChillNum)%CDLoopSideNum
    CALL UpdateChillerComponentCondenserSide(LoopNum, LoopSide, TypeOf_Chiller_ElectricEIR, &
                                     ElectricEIRChiller(EIRChillNum)%CondInletNodeNum,     &
                                     ElectricEIRChiller(EIRChillNum)%CondOutletNodeNum,    &
                                     ElectricEIRChillerReport(EIRChillNum)%QCond,          &
                                     ElectricEIRChillerReport(EIRChillNum)%CondInletTemp,  &
                                     ElectricEIRChillerReport(EIRChillNum)%CondOutletTemp, &
                                     ElectricEIRChillerReport(EIRChillNum)%Condmdot, FirstIteration)

  ELSEIF (LoopNum == ElectricEIRChiller(EIRChillNum)%HRLoopNum) THEN
    CALL UpdateComponentHeatRecoverySide(ElectricEIRChiller(EIRChillNum)%HRLoopNum,               &
                                    ElectricEIRChiller(EIRChillNum)%HRLoopSideNum,           &
                                    TypeOf_Chiller_ElectricEIR,                           &
                                    ElectricEIRChiller(EIRChillNum)%HeatRecInletNodeNum,     &
                                    ElectricEIRChiller(EIRChillNum)%HeatRecOutletNodeNum,    &
                                    ElectricEIRChillerReport(EIRChillNum)%QHeatRecovery,     &
                                    ElectricEIRChillerReport(EIRChillNum)%HeatRecInletTemp,  &
                                    ElectricEIRChillerReport(EIRChillNum)%HeatRecOutletTemp, &
                                    ElectricEIRChillerReport(EIRChillNum)%HeatRecMassFlow ,  &
                                    FirstIteration)
  ENDIF

  RETURN

END SUBROUTINE SimElectricEIRChiller

! End Electric EIR Chiller Module Driver Subroutine
!******************************************************************************

SUBROUTINE GetElectricEIRChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Richard Raustad, FSEC
            !       DATE WRITTEN:    June 2004

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will get the input required by the Electric EIR Chiller model.

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE CurveManager,          ONLY: GetCurveIndex
  USE FluidProperties,       ONLY: FindGlycol
  USE CurveManager,          ONLY: CurveValue
  USE General,               ONLY: TrimSigDigits, RoundSigDigits
  USE GlobalNames,           ONLY: VerifyUniqueChillerName
  USE OutAirNodeManager,     ONLY: CheckAndAddAirNodeNumber
  USE PlantUtilities,        ONLY: RegisterPlantCompDesignFlow
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE DataSizing,            ONLY: Autosize
  USE DataGlobals,           ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! PARAMETERS
  CHARACTER(len=*), PARAMETER :: RoutineName='GetElectricEIRChillerInput: ' ! include trailing blank space

            ! LOCAL VARIABLES
  INTEGER             :: EIRChillerNum           ! Chiller counter
  INTEGER             :: NumAlphas               ! Number of elements in the alpha array
  INTEGER             :: NumNums                 ! Number of elements in the numeric array
  INTEGER             :: IOStat                  ! IO Status when calling get input subroutine
  LOGICAL, SAVE       :: ErrorsFound=.false.     ! True when input errors are found
  LOGICAL             :: IsNotOK                 ! Flag to verify name
  LOGICAL             :: IsBlank                 ! Flag for blank name
  REAL(r64)           :: CurveVal                ! Used to verify EIR-FT and CAP-FT curves equal 1 at reference conditions
  LOGICAL             :: FoundNegValue = .FALSE. ! Used to evaluate PLFFPLR curve objects
  INTEGER             :: CurveCheck = 0          ! Used to evaluate PLFFPLR curve objects
  REAL(r64), DIMENSION(11) :: CurveValArray           ! Used to evaluate PLFFPLR curve objects
  REAL(r64)           :: CurveValTmp             ! Used to evaluate PLFFPLR curve objects
  LOGICAL             :: errflag                 ! Used to tell if a unique chiller name has been specified
  CHARACTER(len=132)  :: StringVar               ! Used for EIRFPLR warning messages
  INTEGER             :: CurveValPtr             ! Index to EIRFPLR curve output
  LOGICAL, SAVE       :: AllocatedFlag =.FALSE. ! True when arrays are allocated
  LOGICAL             :: Okay


            ! FLOW

  IF (AllocatedFlag) RETURN
  cCurrentModuleObject = 'Chiller:Electric:EIR'
  NumElectricEIRChillers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumElectricEIRChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  END IF


  ! ALLOCATE ARRAYS
  ALLOCATE (ElectricEIRChiller(NumElectricEIRChillers))
  ALLOCATE (ElectricEIRChillerReport(NumElectricEIRChillers))
  ALLOCATE(CheckEquipName(NumElectricEIRChillers))
  CheckEquipName=.true.
  AllocatedFlag = .TRUE.

  ! Load arrays with electric EIR chiller data
  DO EIRChillerNum = 1 , NumElectricEIRChillers
    CALL GetObjectItem(cCurrentModuleObject,EIRChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ElectricEIRChiller%Name,EIRChillerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    END IF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    END IF
    ElectricEIRChiller(EIRChillerNum)%Name                  = cAlphaArgs(1)

!   Performance curves
    ElectricEIRChiller(EIRChillerNum)%ChillerCapFT          = GetCurveIndex(cAlphaArgs(2))
    IF (ElectricEIRChiller(EIRChillerNum)%ChillerCapFT .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound = .TRUE.
    END IF

    ElectricEIRChiller(EIRChillerNum)%ChillerEIRFT          = GetCurveIndex(cAlphaArgs(3))
    IF (ElectricEIRChiller(EIRChillerNum)%ChillerEIRFT .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      ErrorsFound = .TRUE.
    END IF

    ElectricEIRChiller(EIRChillerNum)%ChillerEIRFPLR        = GetCurveIndex(cAlphaArgs(4))
    IF (ElectricEIRChiller(EIRChillerNum)%ChillerEIRFPLR .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      ErrorsFound = .TRUE.
    END IF

    ElectricEIRChiller(EIRChillerNum)%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ElectricEIRChiller(EIRChillerNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Chilled Water Nodes')

    IF (SameString(cAlphaArgs(9),'WaterCooled')) THEN
        ElectricEIRChiller(EIRChillerNum)%CondenserType = WaterCooled
    ELSEIF(SameString(cAlphaArgs(9),'AirCooled')) THEN
            ElectricEIRChiller(EIRChillerNum)%CondenserType = AirCooled
    ELSEIF(SameString(cAlphaArgs(9),'EvaporativelyCooled')) THEN
            ElectricEIRChiller(EIRChillerNum)%CondenserType = EvapCooled
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Valid entries are AirCooled, WaterCooled, or EvaporativelyCooled')
      ErrorsFound=.true.
    END IF

    IF (ElectricEIRChiller(EIRChillerNum)%CondenserType == AirCooled .or. &
        ElectricEIRChiller(EIRChillerNum)%CondenserType == EvapCooled) THEN
      ! Connection not required for air or evap cooled condenser
      ! If the condenser inlet is blank for air cooled and evap cooled condensers then supply a generic name
      ! since it is not used elsewhere for connection
      IF(lAlphaFieldBlanks(7))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 25) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(7) = TRIM(cAlphaArgs(1))//' INLET NODE FOR CONDENSER'
        ELSE
          cAlphaArgs(7) = TRIM(cAlphaArgs(1)(1:75))//' INLET NODE FOR CONDENSER'
        ENDIF
      END IF
      IF(lAlphaFieldBlanks(8))THEN
        IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 26) ) THEN ! protect against long name leading to > 100 chars
          cAlphaArgs(8) = TRIM(cAlphaArgs(1))//' OUTLET NODE FOR CONDENSER'
        ELSE
          cAlphaArgs(8) = TRIM(cAlphaArgs(1)(1:74))//' OUTLET NODE FOR CONDENSER'
        ENDIF
      END IF

      ElectricEIRChiller(EIRChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
      CALL CheckAndAddAirNodeNumber(ElectricEIRChiller(EIRChillerNum)%CondInletNodeNum,Okay)
      IF (.not. Okay) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Adding OutdoorAir:Node='//TRIM(cAlphaArgs(7)))
      ENDIF

      ElectricEIRChiller(EIRChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Air,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

    ELSEIF (ElectricEIRChiller(EIRChillerNum)%CondenserType == WaterCooled) THEN
      ! Condenser inlet node name is necessary for water-cooled condenser
      IF (lAlphaFieldBlanks(7)  .or. lAlphaFieldBlanks(8) ) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Condenser Inlet or Outlet Node Name is blank.')
        ErrorsFound=.true.
      END IF

      ElectricEIRChiller(EIRChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

      ElectricEIRChiller(EIRChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(7),cAlphaArgs(8),'Condenser Water Nodes')

    ELSE
      ! Condenser inlet node name is necessary (never should reach this part of code)
      IF (lAlphaFieldBlanks(7) .or. lAlphaFieldBlanks(8) ) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Condenser Inlet or Outlet Node Name is blank.')
        ErrorsFound=.true.
      END IF
      ElectricEIRChiller(EIRChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Unknown,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

      ElectricEIRChiller(EIRChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,  &
               TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Unknown,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(7),cAlphaArgs(8),'Condenser (unknown?) Nodes')

    END IF

    SELECT CASE (TRIM(cAlphaArgs(10)))
    CASE ( 'CONSTANTFLOW' )
      ElectricEIRChiller(EIRChillerNum)%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      ElectricEIRChiller(EIRChillerNum)%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      ElectricEIRChiller(EIRChillerNum)%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      ElectricEIRChiller(EIRChillerNum)%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      ElectricEIRChiller(EIRChillerNum)%FlowMode = NotModulated
    END SELECT

!   Chiller rated performance data
    ElectricEIRChiller(EIRChillerNum)%RefCap                 = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      ErrorsFound=.true.
    END IF
    ElectricEIRChiller(EIRChillerNum)%RefCOP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      ErrorsFound=.true.
    END IF
    ElectricEIRChiller(EIRChillerNum)%TempRefEvapOut         = rNumericArgs(3)
    ElectricEIRChiller(EIRChillerNum)%TempRefCondIn          = rNumericArgs(4)
    ElectricEIRChiller(EIRChillerNum)%EvapVolFlowRate        = rNumericArgs(5)

    IF (ElectricEIRChiller(EIRChillerNum)%CondenserType == AirCooled .OR. &
        ElectricEIRChiller(EIRChillerNum)%CondenserType == EvapCooled) THEN ! Condenser flow rate not used for these cond types
      ElectricEIRChiller(EIRChillerNum)%CondVolFlowRate      = 0.0011d0
    ELSE
      ElectricEIRChiller(EIRChillerNum)%CondVolFlowRate      = rNumericArgs(6)
    END IF

    ElectricEIRChiller(EIRChillerNum)%MinPartLoadRat         = rNumericArgs(7)
    ElectricEIRChiller(EIRChillerNum)%MaxPartLoadRat         = rNumericArgs(8)
    ElectricEIRChiller(EIRChillerNum)%OptPartLoadRat         = rNumericArgs(9)
    ElectricEIRChiller(EIRChillerNum)%MinUnLoadRat           = rNumericArgs(10)
    ElectricEIRChiller(EIRChillerNum)%SizFac                 = rNumericArgs(15)
    IF (ElectricEIRChiller(EIRChillerNum)%SizFac <= 0.0d0) ElectricEIRChiller(EIRChillerNum)%SizFac = 1.0d0

    IF(ElectricEIRChiller(EIRChillerNum)%MinPartLoadRat .GT. ElectricEIRChiller(EIRChillerNum)%MaxPartLoadRat) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(rNumericArgs(7),3))//'] > '//  &
                              TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(rNumericArgs(8),3))//']')
       CALL ShowContinueError('Minimum part load ratio must be less than or equal to the '// &
                            'maximum part load ratio ')
       ErrorsFound=.true.
    END IF

    IF(ElectricEIRChiller(EIRChillerNum)%MinUnLoadRat .LT. ElectricEIRChiller(EIRChillerNum)%MinPartLoadRat .OR. &
       ElectricEIRChiller(EIRChillerNum)%MinUnLoadRat .GT. ElectricEIRChiller(EIRChillerNum)%MaxPartLoadRat) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' = '//TRIM(RoundSigDigits(rNumericArgs(10),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' must be greater than or equal to the '// &
                              TRIM(cNumericFieldNames(7)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' must be less than or equal to the '// &
                              TRIM(cNumericFieldNames(8)))
       ErrorsFound=.true.
    END IF

    IF(ElectricEIRChiller(EIRChillerNum)%OptPartLoadRat .LT. ElectricEIRChiller(EIRChillerNum)%MinPartLoadRat .OR. &
       ElectricEIRChiller(EIRChillerNum)%OptPartLoadRat .GT. ElectricEIRChiller(EIRChillerNum)%MaxPartLoadRat) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' = '//TRIM(RoundSigDigits(rNumericArgs(9),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' must be greater than or equal to the '// &
                              TRIM(cNumericFieldNames(7)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' must be less than or equal to the '// &
                              TRIM(cNumericFieldNames(8)))
       ErrorsFound=.true.
    END IF

    ElectricEIRChiller(EIRChillerNum)%CondenserFanPowerRatio = rNumericArgs(11)
    ElectricEIRChiller(EIRChillerNum)%CompPowerToCondenserFrac = rNumericArgs(12)

    IF(ElectricEIRChiller(EIRChillerNum)%CompPowerToCondenserFrac .LT. 0.0d0 .OR. &
       ElectricEIRChiller(EIRChillerNum)%CompPowerToCondenserFrac .GT. 1.0d0) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(12))//' = '//TRIM(RoundSigDigits(rNumericArgs(12),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(12))//' must be greater than or equal to zero' )
       CALL ShowContinueError(TRIM(cNumericFieldNames(12))//' must be less than or equal to one' )
       ErrorsFound=.true.
    END IF

    ElectricEIRChiller(EIRChillerNum)%TempLowLimitEvapOut    = rNumericArgs(13)

   ! These are the heat recovery inputs
    ElectricEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(14)
    IF ((ElectricEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate > 0.0d0)  &
        .OR. (ElectricEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate == Autosize) )THEN
      ElectricEIRChiller(EIRChillerNum)%HeatRecActive=.True.
      ElectricEIRChiller(EIRChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (ElectricEIRChiller(EIRChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
        ErrorsFound=.True.
      END IF
      ElectricEIRChiller(EIRChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(12),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (ElectricEIRChiller(EIRChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(12))//'='//TRIM(cAlphaArgs(12)))
        ErrorsFound=.True.
      END IF
      IF (ElectricEIRChiller(EIRChillerNum)%CondenserType .NE. WaterCooled) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Heat Recovery requires a Water Cooled Condenser.')
        ErrorsFound=.True.
      END IF

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(11),cAlphaArgs(12),'Heat Recovery Nodes')
      !store heat recovery volume flow for plant sizing
      IF (ElectricEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate > 0.d0) THEN
        Call RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillerNum)%HeatRecInletNodeNum, &  !CR 6953
                               ElectricEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate)
      ENDIF
      IF (NumNums > 17 ) THEN
        IF (.NOT. lNumericFieldBlanks(18)) THEN
          ElectricEIRChiller(EIRChillerNum)%HeatRecCapacityFraction =  rNumericArgs(18)
        ELSE
          ElectricEIRChiller(EIRChillerNum)%HeatRecCapacityFraction = 1.d0
        ENDIF
      ELSE
        ElectricEIRChiller(EIRChillerNum)%HeatRecCapacityFraction = 1.d0
      ENDIF

      IF (NumAlphas > 13) THEN
        IF ( .NOT. lAlphaFieldBlanks(14)) THEN
          ElectricEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum = GetScheduleIndex(cAlphaArgs(14))
          IF (ElectricEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(14))//'='//TRIM(cAlphaArgs(14)))
            ErrorsFound=.True.
          ENDIF
        ELSE
          ElectricEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum = 0
        ENDIF
      ELSE
        ElectricEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum = 0
      ENDIF

      If (NumAlphas > 14) THEN
        IF ( .NOT. lAlphaFieldBlanks(15)) THEN
          ElectricEIRChiller(EIRChillerNum)%HeatRecSetpointNodeNum = &
              GetOnlySingleNode(cAlphaArgs(15), ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                             NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
        ELSE
          ElectricEIRChiller(EIRChillerNum)%HeatRecSetpointNodeNum = 0
        ENDIF
      ELSE
        ElectricEIRChiller(EIRChillerNum)%HeatRecSetpointNodeNum = 0
      ENDIF

    ELSE
      ElectricEIRChiller(EIRChillerNum)%HeatRecActive=.False.
      ElectricEIRChiller(EIRChillerNum)%DesignHeatRecMassFlowRate = 0.0d0
      ElectricEIRChiller(EIRChillerNum)%HeatRecInletNodeNum   = 0
      ElectricEIRChiller(EIRChillerNum)%HeatRecOutletNodeNum   = 0
      IF (.not. lAlphaFieldBlanks(11) .or. .not. lAlphaFieldBlanks(12) ) THEN
    !  IF (cAlphaArgs(11) /= ' ' .or. cAlphaArgs(12) /= ' ') THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive.')
        CALL ShowContinueError('However, node names were specified for heat recovery inlet or outlet nodes.')
      END IF
    END IF


!   Check the CAP-FT, EIR-FT, and PLR curves and warn user if different from 1.0 by more than +-10%
    IF (ElectricEIRChiller(EIRChillerNum)%ChillerCAPFT > 0) THEN
      CurveVal = CurveValue(ElectricEIRChiller(EIRChillerNum)%ChillerCAPFT, &
                          ElectricEIRChiller(EIRChillerNum)%TempRefEvapOut,ElectricEIRChiller(EIRChillerNum)%TempRefCondIn)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Capacity ratio as a function of temperature curve output is not equal to 1.0' // &
                              ' (+ or - 10%) at reference conditions.')
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (ElectricEIRChiller(EIRChillerNum)%ChillerEIRFT > 0) THEN
      CurveVal = CurveValue(ElectricEIRChiller(EIRChillerNum)%ChillerEIRFT, &
                          ElectricEIRChiller(EIRChillerNum)%TempRefEvapOut,ElectricEIRChiller(EIRChillerNum)%TempRefCondIn)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Energy input ratio as a function of temperature curve output is not equal to 1.0' // &
                              ' (+ or - 10%) at reference conditions.')
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (ElectricEIRChiller(EIRChillerNum)%ChillerEIRFPLR > 0) THEN
      CurveVal = CurveValue(ElectricEIRChiller(EIRChillerNum)%ChillerEIRFPLR, 1.0d0)

      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Energy input ratio as a function of part-load ratio curve output is not equal to 1.0' // &
                              ' (+ or - 10%) at reference conditions.')
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
    END IF

    IF (ElectricEIRChiller(EIRChillerNum)%ChillerEIRFPLR > 0) THEN
      FoundNegValue = .FALSE.
      DO CurveCheck = 0, 10, 1
        CurveValTmp = CurveValue(ElectricEIRChiller(EIRChillerNum)%ChillerEIRFPLR, REAL(CurveCheck/10.0d0,r64))
        IF(CurveValTmp .LT. 0.0d0) FoundNegValue = .TRUE.
        CurveValArray(CurveCheck+1) = INT(CurveValTmp*100.0d0)/100.0d0
      END DO
      IF(FoundNegValue)THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Energy input ratio as a function of part-load ratio curve shows negative values.')
        CALL ShowContinueError('EIR as a function of PLR curve output at various part-load ratios shown below:')
        CALL ShowContinueError('PLR          =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00')
        WRITE(StringVar,530)(CurveValArray(CurveValPtr), CurveValPtr = 1, 11)
530     FORMAT('Curve Output = ',11(F7.2))
        CALL ShowContinueError(TRIM(StringVar))
        ErrorsFound = .TRUE.
      END IF
    END IF
    !   Basin heater power as a function of temperature must be greater than or equal to 0
    ElectricEIRChiller(EIRChillerNum)%BasinHeaterPowerFTempDiff = rNumericArgs(16)
    IF(rNumericArgs(16) .LT. 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cNumericFieldNames(16))//' must be >= 0')
      ErrorsFound = .TRUE.
    END IF

    ElectricEIRChiller(EIRChillerNum)%BasinHeaterSetPointTemp = rNumericArgs(17)

    IF(ElectricEIRChiller(EIRChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0) THEN
      IF(NumNums .LT. 17) THEN
        ElectricEIRChiller(EIRChillerNum)%BasinHeaterSetPointTemp = 2.0d0
      ENDIF
      IF(ElectricEIRChiller(EIRChillerNum)%BasinHeaterSetPointTemp < 2.0d0) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//' "'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(17))//' is less than 2 deg C. Freezing could occur.')
      END IF
    END IF

    IF(.NOT. lAlphaFieldBlanks(13))THEN
      ElectricEIRChiller(EIRChillerNum)%BasinHeaterSchedulePtr   = GetScheduleIndex(cAlphaArgs(13))
      IF(ElectricEIRChiller(EIRChillerNum)%BasinHeaterSchedulePtr .EQ. 0)THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowWarningError(TRIM(cAlphaFieldNames(13))//' "'//TRIM(cAlphaArgs(13)) &
                       //'" was not found. Basin heater operation will not be modeled and the simulation continues')
      END IF
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  END IF

  DO EIRChillerNum = 1, NumElectricEIRChillers
     CALL SetupOutputVariable('Chiller Part Load Ratio []', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerPartLoadRatio,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cycling Ratio []', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerCyclingRatio,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          ElectricEIRChillerReport(EIRChillerNum)%Power,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Energy [J]', &
          ElectricEIRChillerReport(EIRChillerNum)%Energy,'System','Sum',ElectricEIRChiller(EIRChillerNum)%Name,  &
                               ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          ElectricEIRChillerReport(EIRChillerNum)%QEvap,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          ElectricEIRChillerReport(EIRChillerNum)%EvapEnergy,'System','Sum',ElectricEIRChiller(EIRChillerNum)%Name,  &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller False Load Heat Transfer Rate [W]', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerFalseLoadRate,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller False Load Heat Transfer Energy [J]', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerFalseLoad,'System','Sum',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          ElectricEIRChillerReport(EIRChillerNum)%EvapInletTemp,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          ElectricEIRChillerReport(EIRChillerNum)%EvapOutletTemp,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          ElectricEIRChillerReport(EIRChillerNum)%Evapmdot,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          ElectricEIRChillerReport(EIRChillerNum)%QCond,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          ElectricEIRChillerReport(EIRChillerNum)%CondEnergy,'System','Sum',ElectricEIRChiller(EIRChillerNum)%Name,  &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller COP [W/W]', &
          ElectricEIRChillerReport(EIRChillerNum)%ActualCOP,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Capacity Temperature Modifier Multiplier []', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerCapFT,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller EIR Temperature Modifier Multiplier []', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerEIRFT,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller EIR Part Load Modifier Multiplier []', &
          ElectricEIRChillerReport(EIRChillerNum)%ChillerEIRFPLR,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)

     ! Condenser mass flow and outlet temp are valid for water cooled
     IF (ElectricEIRChiller(EIRChillerNum)%CondenserType == WaterCooled)THEN
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            ElectricEIRChillerReport(EIRChillerNum)%CondInletTemp,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
       CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
            ElectricEIRChillerReport(EIRChillerNum)%CondOutletTemp,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
       CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
            ElectricEIRChillerReport(EIRChillerNum)%Condmdot,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)

       ! If heat recovery is active then setup report variables
       IF (ElectricEIRChiller(EIRChillerNum)%HeatRecActive) THEN
          CALL SetupOutputVariable('Chiller Total Recovered Heat Rate [W]', &
            ElectricEIRChillerReport(EIRChillerNum)%QHeatRecovery,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Total Recovered Heat Energy [J]', &
            ElectricEIRChillerReport(EIRChillerNum)%EnergyHeatRecovery,'System','Sum', &
            ElectricEIRChiller(EIRChillerNum)%Name, &
            ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')
          CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temperature [C]', &
            ElectricEIRChillerReport(EIRChillerNum)%HeatRecInletTemp,'System','Average', &
            ElectricEIRChiller(EIRChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temperature [C]', &
            ElectricEIRChillerReport(EIRChillerNum)%HeatRecOutletTemp,'System','Average', &
            ElectricEIRChiller(EIRChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
            ElectricEIRChillerReport(EIRChillerNum)%HeatRecMassFlow,'System','Average', &
            ElectricEIRChiller(EIRChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Effective Heat Rejection Temperature [C]', &
            ElectricEIRChillerReport(EIRChillerNum)%ChillerCondAvgTemp,'System','Average', &
            ElectricEIRChiller(EIRChillerNum)%Name)
       END IF

     ELSE
       CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
            ElectricEIRChillerReport(EIRChillerNum)%CondInletTemp,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
       IF(ElectricEIRChiller(EIRChillerNum)%CondenserFanPowerRatio > 0) THEN
          CALL SetupOutputVariable('Chiller Condenser Fan Electric Power [W]', &
               ElectricEIRChillerReport(EIRChillerNum)%CondenserFanPowerUse,'System','Average' &
               ,ElectricEIRChiller(EIRChillerNum)%Name)
          CALL SetupOutputVariable('Chiller Condenser Fan Electric Energy [J]', &
               ElectricEIRChillerReport(EIRChillerNum)%CondenserFanEnergyConsumption,'System','Sum', &
               ElectricEIRChiller(EIRChillerNum)%Name, &
               ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')
       END IF
       IF (ElectricEIRChiller(EIRChillerNum)%CondenserType == EvapCooled) THEN
         IF(ElectricEIRChiller(EIRChillerNum)%BasinHeaterPowerFTempDiff .GT. 0.0d0)THEN
           CALL SetupOutputVariable('Chiller Basin Heater Electric Power [W]', &
                ElectricEIRChillerReport(EIRChillerNum)%BasinHeaterPower,'System','Average',ElectricEIRChiller(EIRChillerNum)%Name)
           CALL SetupOutputVariable('Chiller Basin Heater Electric Energy [J]', &
                ElectricEIRChillerReport(EIRChillerNum)%BasinHeaterConsumption,'System','Sum',  &
                ElectricEIRChiller(EIRChillerNum)%Name, &
                ResourceTypeKey='Electric',EndUseKey='CHILLERS',GroupKey='Plant')
         END IF
       ENDIF
     END IF
     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', ElectricEIRChiller(EIRChillerNum)%Name, '[W]', &
                                     ElectricEIRChiller(EIRChillerNum)%RefCap  )
     ENDIF

  END DO

  RETURN

END SUBROUTINE GetElectricEIRChillerInput

SUBROUTINE InitElectricEIRChiller(EIRChillNum,RunFlag, MyLoad)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for initializations of the Electric EIR Chiller variables

          ! METHODOLOGY EMPLOYED:
          !  Uses the status flags to trigger initializations.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_ElectricEIR, ScanPlantLoopsForObject, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn, &
                              SingleSetpoint, DualSetpointDeadband
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE DataEnvironment, ONLY : StdBaroPress
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: EIRChillNum         ! Number of the current electric EIR chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag             ! TRUE when chiller operating
  REAL(r64),INTENT(IN) :: MyLoad              ! current load put on chiller

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE                             :: MyOneTimeFlag = .true. ! Flag used to execute code only once
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag                ! TRUE in order to set component location
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag           ! TRUE when new environment is started
  INTEGER                                 :: EvapInletNode          ! Node number for evaporator water inlet node
  INTEGER                                 :: EvapOutletNode         ! Node number for evaporator water outlet node
  INTEGER                                 :: CondInletNode          ! Node number for condenser water inlet node
  INTEGER                                 :: CondOutletNode         ! Node number for condenser water outlet node
  INTEGER                                 :: HeatRecInNode          ! Node number for heat recovery water inlet node
  INTEGER                                 :: HeatRecOutNode         ! Node number for heat recovery water outlet node
  REAL(r64)                               :: rho                    ! local fluid density
  REAL(r64)                               :: mdot                   ! local fluid mass flow rate
  REAL(r64)                               :: mdotCond               ! local fluid mass flow rate for condenser
  REAL(r64)                               :: THeatRecSetpoint       ! tests set point node for proper set point value
  INTEGER                                 :: LoopNum
  INTEGER                                 :: LoopSideNum
  INTEGER                                 :: BranchIndex
  INTEGER                                 :: CompIndex
  LOGICAL                                 :: FatalError
  LOGICAL                                 :: errFlag
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumElectricEIRChillers))
    ALLOCATE(MyFlag(NumElectricEIRChillers))
    MyEnvrnFlag = .TRUE.
    MyFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  EvapInletNode  = ElectricEIRChiller(EIRChillNum)%EvapInletNodeNum
  EvapOutletNode = ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum
  CondInletNode  = ElectricEIRChiller(EIRChillNum)%CondInletNodeNum
  CondOutletNode = ElectricEIRChiller(EIRChillNum)%CondOutletNodeNum

  IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive ) THEN
    HeatRecInNode  = ElectricEIRChiller(EIRChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = ElectricEIRChiller(EIRChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(EIRChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(ElectricEIRChiller(EIRChillNum)%Name, &
                             TypeOf_Chiller_ElectricEIR, &
                             ElectricEIRChiller(EIRChillNum)%CWLoopNum, &
                             ElectricEIRChiller(EIRChillNum)%CWLoopSideNum, &
                             ElectricEIRChiller(EIRChillNum)%CWBranchNum,&
                             ElectricEIRChiller(EIRChillNum)%CWCompNum, &
                             LowLimitTemp = ElectricEIRChiller(EIRChillNum)%TempLowLimitEvapOut, &
                             InletNodeNumber = ElectricEIRChiller(EIRChillNum)%EvapInletNodeNum,  &
                             errFlag=errFlag)
    IF (ElectricEIRChiller(EIRChillNum)%CondenserType /= AirCooled .AND. &
        ElectricEIRChiller(EIRChillNum)%CondenserType /= EvapCooled) THEN
      CALL ScanPlantLoopsForObject(ElectricEIRChiller(EIRChillNum)%Name, &
                               TypeOf_Chiller_ElectricEIR, &
                               ElectricEIRChiller(EIRChillNum)%CDLoopNum, &
                               ElectricEIRChiller(EIRChillNum)%CDLoopSideNum, &
                               ElectricEIRChiller(EIRChillNum)%CDBranchNum,&
                               ElectricEIRChiller(EIRChillNum)%CDCompNum, &
                               InletNodeNumber = ElectricEIRChiller(EIRChillNum)%CondInletNodeNum,  &
                               errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElectricEIRChiller(EIRChillNum)%CWLoopNum,      &
                                          ElectricEIRChiller(EIRChillNum)%CWLoopSideNum,  &
                                          ElectricEIRChiller(EIRChillNum)%CDLoopNum,      &
                                          ElectricEIRChiller(EIRChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_ElectricEIR, .TRUE. )
    ENDIF
    IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive) THEN
      CALL ScanPlantLoopsForObject(ElectricEIRChiller(EIRChillNum)%Name, &
                               TypeOf_Chiller_ElectricEIR, &
                               ElectricEIRChiller(EIRChillNum)%HRLoopNum, &
                               ElectricEIRChiller(EIRChillNum)%HRLoopSideNum, &
                               ElectricEIRChiller(EIRChillNum)%HRBranchNum,&
                               ElectricEIRChiller(EIRChillNum)%HRCompNum, &
                               InletNodeNumber = ElectricEIRChiller(EIRChillNum)%HeatRecInletNodeNum,  &
                               errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElectricEIRChiller(EIRChillNum)%CWLoopNum,      &
                                          ElectricEIRChiller(EIRChillNum)%CWLoopSideNum,  &
                                          ElectricEIRChiller(EIRChillNum)%HRLoopNum,     &
                                          ElectricEIRChiller(EIRChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_ElectricEIR, .TRUE. )
    ENDIF

    IF ( ElectricEIRChiller(EIRChillNum)%CondenserType /= AirCooled .AND. &
         ElectricEIRChiller(EIRChillNum)%CondenserType /= EvapCooled  .AND.  &
         ElectricEIRChiller(EIRChillNum)%HeatRecActive )  THEN
      CALL InterConnectTwoPlantLoopSides( ElectricEIRChiller(EIRChillNum)%CDLoopNum,      &
                                          ElectricEIRChiller(EIRChillNum)%CDLoopSideNum,  &
                                          ElectricEIRChiller(EIRChillNum)%HRLoopNum,     &
                                          ElectricEIRChiller(EIRChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_ElectricEIR , .FALSE.)
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('InitElectricEIRChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (ElectricEIRChiller(EIRChillNum)%FlowMode == ConstantFlow) THEN
      ! reset flow priority
      PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%LoopSide(ElectricEIRChiller(EIRChillNum)%CWLoopSideNum)% &
          Branch(ElectricEIRChiller(EIRChillNum)%CWBranchNum)%Comp(ElectricEIRChiller(EIRChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (ElectricEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) THEN
      ! reset flow priority
      PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%LoopSide(ElectricEIRChiller(EIRChillNum)%CWLoopSideNum)% &
          Branch(ElectricEIRChiller(EIRChillNum)%CWBranchNum)%Comp(ElectricEIRChiller(EIRChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
      ! check if setpoint on outlet node
      IF ((Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue) ) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. ElectricEIRChiller(EIRChillNum)%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ElectricEIRChiller(EIRChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ElectricEIRChiller(EIRChillNum)%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. ElectricEIRChiller(EIRChillNum)%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ElectricEIRChiller(EIRChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              ElectricEIRChiller(EIRChillNum)%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        ElectricEIRChiller(EIRChillNum)%ModulatedFlowSetToLoop = .TRUE.
        Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi =                        &
          Node(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF
    MyFlag(EIRChillNum)=.FALSE.
  ENDIF

  IF (MyEnvrnFlag(EIRChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN

    IF (PlantSizeNotComplete) CALL SizeElectricEIRChiller(EIRChillNum)
    rho = GetDensityGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex,&
                                  'InitElectricEIRChiller')

    ElectricEIRChiller(EIRChillNum)%EvapMassFlowRateMax = ElectricEIRChiller(EIRChillNum)%EvapVolFlowRate * rho

    CALL InitComponentNodes(0.d0,  ElectricEIRChiller(EIRChillNum)%EvapMassFlowRateMax,  &
                          EvapInletNode,        &
                          EvapOutletNode,       &
                          ElectricEIRChiller(EIRChillNum)%CWLoopNum,               &
                          ElectricEIRChiller(EIRChillNum)%CWLoopSideNum,           &
                          ElectricEIRChiller(EIRChillNum)%CWBranchNum,             &
                          ElectricEIRChiller(EIRChillNum)%CWCompNum)

    IF (ElectricEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN

      rho = GetDensityGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                  ElectricEIRChiller(EIRChillNum)%TempRefCondIn, &
                                  PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex,&
                                  'InitElectricEIRChiller')
      ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax = rho * ElectricEIRChiller(EIRChillNum)%CondVolFlowRate
      CALL InitComponentNodes(0.d0, ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax ,&
                          CondInletNode,        &
                          CondOutletNode,       &
                          ElectricEIRChiller(EIRChillNum)%CDLoopNum,               &
                          ElectricEIRChiller(EIRChillNum)%CDLoopSideNum,           &
                          ElectricEIRChiller(EIRChillNum)%CDBranchNum,             &
                          ElectricEIRChiller(EIRChillNum)%CDCompNum)
      Node(CondInletNode)%Temp = ElectricEIRChiller(EIRChillNum)%TempRefCondIn
    ELSE ! air or evap air condenser
     ! Initialize maximum available condenser flow rate
      rho = PsyRhoAirFnPbTdbW(StdBaroPress,ElectricEIRChiller(EIRChillNum)%TempRefCondIn,0.0D0,'InitElectricEIRChiller')
      ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax = rho * ElectricEIRChiller(EIRChillNum)%CondVolFlowRate

      Node(CondInletNode)%MassFlowRate          = ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax
      Node(CondOutletNode)%MassFlowRate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0d0
      Node(CondInletNode)%MassFlowRateMin       = 0.0d0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0d0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0d0
      Node(CondInletNode)%Temp = ElectricEIRChiller(EIRChillNum)%TempRefCondIn

    ENDIF

    IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%FluidIndex,&
                                  'InitElectricEIRChiller')
      ElectricEIRChiller(EIRChillNum)%DesignHeatRecMassFlowRate = rho * &
                                      ElectricEIRChiller(EIRChillNum)%DesignHeatRecVolFlowRate

      CALL InitComponentNodes(0.0D0, ElectricEIRChiller(EIRChillNum)%DesignHeatRecMassFlowRate ,  &
                         ElectricEIRChiller(EIRChillNum)%HeatRecInletNodeNum,        &
                         ElectricEIRChiller(EIRChillNum)%HeatRecOutletNodeNum,       &
                         ElectricEIRChiller(EIRChillNum)%HRLoopNum,               &
                         ElectricEIRChiller(EIRChillNum)%HRLoopSideNum,           &
                         ElectricEIRChiller(EIRChillNum)%HRBranchNum,             &
                         ElectricEIRChiller(EIRChillNum)%HRCompNum)
      ! overall capacity limit
      ElectricEIRChiller(EIRChillNum)%HeatRecMaxCapacityLimit = ElectricEIRChiller(EIRChillNum)%HeatRecCapacityFraction &
                              *  (ElectricEIRChiller(EIRChillNum)%RefCap + ElectricEIRChiller(EIRChillNum)%RefCap &
                                                                              /ElectricEIRChiller(EIRChillNum)%RefCOP)

      IF(ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum > 0)THEN
        SELECT CASE (PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetPoint)
            THeatRecSetpoint = Node(ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum)%TempSetPoint
          CASE (DualSetPointDeadBand)
            THeatRecSetpoint = Node(ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum)%TempSetPointHi
        END SELECT
        IF(THeatRecSetpoint == SensedNodeFlagValue)THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            IF (.NOT. ElectricEIRChiller(EIRChillNum)%HRSPErrDone) THEN
              CALL ShowWarningError('Missing heat recovery temperature setpoint for chiller named ' // &
                                          TRIM(ElectricEIRChiller(EIRChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the heat recovery leaving temperature ' // &
                                             'setpoint node specified, use a SetpointManager')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for heat recovery. '//  &
                 'The simulation continues ...')
              ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum =   &
                 PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%TempSetPointNodeNum
              ElectricEIRChiller(EIRChillNum)%HRSPErrDone = .TRUE.
            ENDIF
          ELSE
           ! need call to EMS to check node
            FatalError = .FALSE. ! but not really fatal yet, but should be.
            CALL CheckIfNodeSetpointManagedByEMS(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
            IF (FatalError) THEN
              IF (.NOT. ElectricEIRChiller(EIRChillNum)%HRSPErrDone) THEN
                CALL ShowWarningError('Missing heat recovery temperature setpoint for chiller named ' // &
                                          TRIM(ElectricEIRChiller(EIRChillNum)%Name) )
                CALL ShowContinueError('  A temperature setpoint is needed at the heat recovery leaving temperature ' // &
                                     'setpoint node specified, use a SetpointManager to establish a setpoint')
                CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at this node ')
                CALL ShowContinueError('  The overall loop setpoint will be assumed for heat recovery. '//  &
                   'The simulation continues ...')
                ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum =   &
                   PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%TempSetPointNodeNum
                ElectricEIRChiller(EIRChillNum)%HRSPErrDone = .TRUE.
              ENDIF
            ENDIF
          END IF ! IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        END IF ! IF(THeatRecSetpoint == SensedNodeFlagValue)THEN
      END IF ! IF(ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum > 0)THEN
    ENDIF ! IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive) THEN

    MyEnvrnFlag(EIRChillNum) = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(EIRChillNum)=.true.
  END IF

  IF ((ElectricEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) &
      .AND. ElectricEIRChiller(EIRChillNum)%ModulatedFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
    mdot     = ElectricEIRChiller(EIRChillNum)%EvapMassFlowRateMax
    mdotCond = ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              ElectricEIRChiller(EIRChillNum)%CWLoopNum,     &
                              ElectricEIRChiller(EIRChillNum)%CWLoopSideNum, &
                              ElectricEIRChiller(EIRChillNum)%CWBranchNum,   &
                              ElectricEIRChiller(EIRChillNum)%CWCompNum)

  IF (ElectricEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,        &
                                ElectricEIRChiller(EIRChillNum)%CDLoopNum,     &
                                ElectricEIRChiller(EIRChillNum)%CDLoopSideNum, &
                                ElectricEIRChiller(EIRChillNum)%CDBranchNum,   &
                                ElectricEIRChiller(EIRChillNum)%CDCompNum)
  ENDIF
  ! Initialize heat recovery flow rates at node
  IF (ElectricEIRChiller(EIRChillNum)%HeatRecActive ) THEN
    LoopNum      =  ElectricEIRChiller(EIRChillNum)%HRLoopNum
    LoopSideNum  =  ElectricEIRChiller(EIRChillNum)%HRLoopSideNum
    BranchIndex  =  ElectricEIRChiller(EIRChillNum)%HRBranchNum
    CompIndex    =  ElectricEIRChiller(EIRChillNum)%HRCompNum
    If (RunFlag) Then
      mdot          = ElectricEIRChiller(EIRChillNum)%DesignHeatRecMassFlowRate
    ELSE
      mdot          = 0.0D0
    ENDIF

    CALL SetComponentFlowRate(mdot,HeatRecInNode,HeatRecOutNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF

  IF (ElectricEIRChiller(EIRChillNum)%CondenserType == EvapCooled) THEN
      BasinHeaterPower       = 0.0d0
  ENDIF
  RETURN

END SUBROUTINE InitElectricEIRChiller

SUBROUTINE SizeElectricEIRChiller(EIRChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for sizing Electric EIR Chiller Components for which capacities and flow rates
          !  have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          !  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
          !  the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          !  is calculated from the reference capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,   ONLY: PlantLoop, PlantSizesOkayToFinalize, TypeOf_Chiller_ElectricEIR
  USE PlantUtilities,     ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE StandardRatings,   ONLY: CalcChillerIPLV

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: EIRChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  LOGICAL             :: LoopErrorsFound
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate
  LOGICAL, SAVE       :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag   ! TRUE in order to calculate IPLV

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumElectricEIRChillers))
    MyFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap          = ElectricEIRChiller(EIRChillNum)%RefCap
  tmpEvapVolFlowRate = ElectricEIRChiller(EIRChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = ElectricEIRChiller(EIRChillNum)%CondVolFlowRate

  IF (ElectricEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
    IF (ElectricEIRChiller(EIRChillNum)%CondVolFlowRate == AutoSize) THEN
      PltSizCondNum = PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%PlantSizNum
    END IF
  END IF

  ! find the appropriate Plant Sizing object
  PltSizNum = PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%PlantSizNum

  IF (ElectricEIRChiller(EIRChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate *   &
                                    ElectricEIRChiller(EIRChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ElectricEIRChiller(EIRChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ElectricEIRChiller(EIRChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Electric:EIR', ElectricEIRChiller(EIRChillNum)%Name, &
                              'Reference Chilled Water Flow Rate [m3/s]', &
                              ElectricEIRChiller(EIRChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricEIRChiller(EIRChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (ElectricEIRChiller(EIRChillNum)%RefCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        Cp  = GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                                 InitConvTemp,                      &
                                 PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex, &
                                 'SizeElectricEIRChiller')

        rho  = GetDensityGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex,&
                                  'SizeElectricEIRChiller')
        tmpNomCap =  Cp * rho * PlantSizData(PltSizNum)%DeltaT  * tmpEvapVolFlowRate
        IF (PlantSizesOkayToFinalize) ElectricEIRChiller(EIRChillNum)%RefCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ElectricEIRChiller(EIRChillNum)%RefCap = tmpNomCap
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Electric:EIR', ElectricEIRChiller(EIRChillNum)%Name, &
                              'Reference Capacity [W]', ElectricEIRChiller(EIRChillNum)%RefCap)
    ELSE
      CALL ShowSevereError('Autosizing of Electric Chiller reference capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric Chiller object='//TRIM(ElectricEIRChiller(EIRChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ElectricEIRChiller(EIRChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

        rho  = GetDensityGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex,&
                                  'SizeElectricEIRChiller')

        Cp  = GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                 ElectricEIRChiller(EIRChillNum)%TempRefCondIn,                      &
                                 PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                 'SizeElectricEIRChiller')
        tmpCondVolFlowRate = tmpNomCap * &
          (1.0d0 + (1.0d0/ElectricEIRChiller(EIRChillNum)%RefCOP) * ElectricEIRChiller(EIRChillNum)%CompPowerToCondenserFrac) / &
          ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho )
        IF (PlantSizesOkayToFinalize) ElectricEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ElectricEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Electric:EIR', ElectricEIRChiller(EIRChillNum)%Name, &
                              'Reference Condenser Water Flow Rate [m3/s]', &
                              ElectricEIRChiller(EIRChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Electric EIR Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Electric EIR Chiller object='//TRIM(ElectricEIRChiller(EIRChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  CALL RegisterPlantCompDesignFlow(ElectricEIRChiller(EIRChillNum)%CondInletNodeNum,tmpCondVolFlowRate)

  IF (PlantSizesOkayToFinalize) THEN
    IF (MyFlag(EIRChillNum)) THEN
      CALL CalcChillerIPLV(ElectricEIRChiller(EIRChillNum)%Name, &
                           TypeOf_Chiller_ElectricEIR,           &
                           ElectricEIRChiller(EIRChillNum)%RefCap, &
                           ElectricEIRChiller(EIRChillNum)%RefCOP, &
                           ElectricEIRChiller(EIRChillNum)%CondenserType, &
                           ElectricEIRChiller(EIRChillNum)%ChillerCapFT, &
                           ElectricEIRChiller(EIRChillNum)%ChillerEIRFT, &
                           ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLR, &
                           ElectricEIRChiller(EIRChillNum)%MinUnLoadRat)
      MyFlag(EIRChillNum) = .FALSE.
    ENDIF
    !create predefined report
    equipName = ElectricEIRChiller(EIRChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:Electric:EIR')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ElectricEIRChiller(EIRChillNum)%RefCOP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ElectricEIRChiller(EIRChillNum)%RefCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeElectricEIRChiller

SUBROUTINE CalcElectricEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   July 2004
          !       MODIFIED       Chandan Sharma, FSEC, February 2010, Added basin heater
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate a vapor compression chiller using the DOE-2 model

          ! METHODOLOGY EMPLOYED:
          !  Use empirical curve fits to model performance at off-reference conditions

          ! REFERENCES:
          ! 1. DOE-2 Engineers Manual, Version 2.1A, November 1982, LBL-11353

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : WarmupFlag, CurrentTime
  USE DataHVACGlobals, ONLY : SmallLoad, SysTimeElapsed, TimeStepSys
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : DeltaTemptol, PlantLoop, SimPlantEquipTypes, TypeOf_Chiller_ElectricEIR,  &
                              CompSetPtBasedSchemeType, CriteriaType_MassFlowRate, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY: ControlType_SeriesActive, MassFlowTolerance
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger
  USE Psychrometrics,  ONLY : PsyCpAirFnWTdb, PsyWFnTdbTwbPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: EIRChillNum     ! Chiller number
  REAL(r64)              :: MyLoad          ! Operating load
  LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
  INTEGER, INTENT(IN)    :: EquipFlowCtrl   ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:

  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: FRAC                  ! Chiller cycling ratio
  REAL(r64)              :: MinPartLoadRat        ! Min allowed operating fraction of full load
  REAL(r64)              :: MinUnloadRat          ! Min allowed unloading fraction of full load
  REAL(r64)              :: MaxPartLoadRat        ! Max allowed operating fraction of full load
  REAL(r64)              :: EvapInletTemp         ! Evaporator inlet temperature [C]
  REAL(r64)              :: CondInletTemp         ! Condenser inlet temperature [C]
  REAL(r64)              :: EvapOutletTempSetpoint ! Evaporator outlet temperature setpoint [C]
  REAL(r64)              :: AvailChillerCap       ! Chiller available capacity at current operating conditions [W]
  REAL(r64)              :: ChillerRefCap         ! Chiller reference capacity
  REAL(r64)              :: EvapDeltaTemp         ! Evaporator temperature difference [C]
  REAL(r64)              :: ReferenceCOP          ! Reference coefficient of performance, from user input
  REAL(r64)              :: PartLoadRat           ! Operating part load ratio
  REAL(r64)              :: TempLowLimitEout      ! Evaporator low temp. limit cut off [C]
  REAL(r64)              :: EvapMassFlowRateMax   ! Max reference evaporator mass flow rate converted from volume flow rate [kg/s]
  INTEGER                :: EvapInletNode         ! Evaporator inlet node number
  INTEGER                :: EvapOutletNode        ! Evaporator outlet node number
  INTEGER                :: CondInletNode         ! Condenser inlet node number
  INTEGER                :: CondOutletNode        ! Condenser outlet node number
!  LOGICAL,SAVE           :: PossibleSubCooling
  REAL(r64)              :: TempLoad              ! Actual load to be met by chiller. This value is compared to MyLoad
                                                  ! and reset when necessary since this chiller can cycle, the load passed
                                                  ! should be the actual load. Instead the minimum PLR * RefCap is
                                                  ! passed in. [W]
  INTEGER                :: PlantLoopNum          ! Plant loop which contains the current chiller
  INTEGER                :: LoopSideNum           ! Plant loop side which contains the current chiller (usually supply side)
  INTEGER                :: BranchNum
  INTEGER                :: CompNum
  REAL(r64),SAVE         :: TimeStepSysLast=0.0d0     ! last system time step (used to check for downshifting)
  REAL(r64)              :: CurrentEndTime          ! end time of time step for current simulation time step
  REAL(r64),SAVE         :: CurrentEndTimeLast=0.0d0  ! end time of time step for last simulation time step
  CHARACTER(len=6)       :: OutputChar = ' '        ! character string for warning messages
  REAL(r64)              :: Cp !local fluid specific heat

! Set module level inlet and outlet nodes and initialize other local variables
  ChillerPartLoadRatio       = 0.0d0
  ChillerCyclingRatio        = 0.0d0
  ChillerFalseLoadRate       = 0.0d0
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  Power                      = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  QHeatRecovered             = 0.0d0
  CondenserFanPower          = 0.0d0
  EvapInletNode  = ElectricEIRChiller(EIRChillNum)%EvapInletNodeNum
  EvapOutletNode = ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum
  CondInletNode  = ElectricEIRChiller(EIRChillNum)%CondInletNodeNum
  CondOutletNode = ElectricEIRChiller(EIRChillNum)%CondOutletNodeNum
  PlantLoopNum   = ElectricEIRChiller(EIRChillNum)%CWLoopNum
  LoopSideNum    = ElectricEIRChiller(EIRChillNum)%CWLoopSideNum
  BranchNum      = ElectricEIRChiller(EIRChillNum)%CWBranchNum
  CompNum        = ElectricEIRChiller(EIRChillNum)%CWCompNum
  EvapInletTemp  = Node(EvapInletNode)%Temp
  FRAC    = 1.0d0

! Set performance curve outputs to 0.0 when chiller is off
  ChillerCapFT = 0.0d0
  ChillerEIRFT = 0.0d0
  ChillerEIRFPLR = 0.0d0

! calculate end time of current time step
  CurrentEndTime = CurrentTime + SysTimeElapsed

! Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
! Wait for next time step to print warnings. If simulation iterates, print out
! the warning for the last iteration only. Must wait for next time step to accomplish this.
! If a warning occurs and the simulation down shifts, the warning is not valid.
  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
    IF(ElectricEIRChiller(EIRChillNum)%PrintMessage)THEN
          ElectricEIRChiller(EIRChillNum)%MsgErrorCount = &
                         ElectricEIRChiller(EIRChillNum)%MsgErrorCount + 1
!     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
      IF (ElectricEIRChiller(EIRChillNum)%MsgErrorCount < 2) THEN
         CALL ShowWarningError(TRIM(ElectricEIRChiller(EIRChillNum)%MsgBuffer1)//'.')
         CALL ShowContinueError(TRIM(ElectricEIRChiller(EIRChillNum)%MsgBuffer2))
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(ElectricEIRChiller(EIRChillNum)%MsgBuffer1)//' error continues.', &
           ElectricEIRChiller(EIRChillNum)%ErrCount1,ReportMaxOf=ElectricEIRChiller(EIRChillNum)%MsgDataLast,  &
           ReportMinOf=ElectricEIRChiller(EIRChillNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
      END IF
    END IF
  END IF

! save last system time step and last end time of current time step (used to determine if warning is valid)
  TimeStepSysLast    = TimeStepSys
  CurrentEndTimeLast = CurrentEndTime

  ! If no loop demand or chiller OFF, return
   !If Chiller load is 0 or chiller is not running then leave the subroutine.Before leaving
   !if the component control is SERIESACTIVE we set the component flow to inlet flow so that
   !flow resolver will not shut down the branch
   IF(MyLoad >= 0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    END IF
    IF (ElectricEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
      IF (PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)% &
            LoopSide(ElectricEIRChiller(EIRChillNum)%CDLoopSideNum)% &
              Branch(ElectricEIRChiller(EIRChillNum)%CDBranchNum)%  &
                Comp(ElectricEIRChiller(EIRChillNum)%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate = Node(CondInletNode)%MassFlowrate
      ENDIF
    ENDIF
    IF (ElectricEIRChiller(EIRChillNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ElectricEIRChiller(EIRChillNum)%BasinHeaterPowerFTempDiff,&
                                ElectricEIRChiller(EIRChillNum)%BasinHeaterSchedulePtr,&
                                ElectricEIRChiller(EIRChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    ElectricEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
    RETURN
   END IF

! initialize outlet air humidity ratio of air or evap cooled chillers
  CondOutletHumRat = Node(CondInletNode)%HumRat

  IF (ElectricEIRChiller(EIRChillNum)%CondenserType == AirCooled) THEN ! Condenser inlet temp = outdoor temp
!    Node(CondInletNode)%Temp = OutDryBulbTemp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirDryBulb

! Warn user if entering condenser dry-bulb temperature falls below 0 C
      IF(Node(CondInletNode)%Temp .LT. 0.0d0 .AND. ABS(MyLoad) .GT. 0 .AND. RunFlag .AND. .NOT. WarmupFlag) THEN
        ElectricEIRChiller(EIRChillNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ElectricEIRChiller(EIRChillNum)%MsgBuffer1 = 'ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR "' &
                             //TRIM(ElectricEIRChiller(EIRChillNum)%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 0C'
        ElectricEIRChiller(EIRChillNum)%MsgBuffer2 = '... Outdoor Dry-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ElectricEIRChiller(EIRChillNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ElectricEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
      END IF
  ELSE IF (ElectricEIRChiller(EIRChillNum)%CondenserType == EvapCooled) THEN ! Condenser inlet temp = (outdoor wet bulb)
!    Node(CondInletNode)%Temp = OutWetBulbTemp
    Node(CondInletNode)%Temp = Node(CondInletNode)%OutAirWetBulb
!  line above assumes evaporation pushes condenser inlet air humidity ratio to saturation
    CondOutletHumRat = PsyWFnTdbTwbPb(Node(CondInletNode)%Temp,Node(CondInletNode)%Temp,Node(CondInletNode)%Press)

! Warn user if evap condenser wet-bulb temperature falls below 10 C
      IF(Node(CondInletNode)%Temp .LT. 10.0d0 .AND. ABS(MyLoad) .GT. 0 .AND. RunFlag .AND. .NOT. WarmupFlag) THEN
        ElectricEIRChiller(EIRChillNum)%PrintMessage = .TRUE.
        WRITE(OutputChar,OutputFormat)Node(CondInletNode)%Temp
        ElectricEIRChiller(EIRChillNum)%MsgBuffer1 = 'ElectricEIRChillerModel - CHILLER:ELECTRIC:EIR "' &
                             //TRIM(ElectricEIRChiller(EIRChillNum)%Name)// &
                             '" - Air Cooled Condenser Inlet Temperature below 10C'
        ElectricEIRChiller(EIRChillNum)%MsgBuffer2 = '... Outdoor Wet-bulb Condition = '//TRIM(OutputChar)// &
                   ' C. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                   //TRIM(CreateSysTimeIntervalString())
        ElectricEIRChiller(EIRChillNum)%MsgDataLast = Node(CondInletNode)%Temp
      ELSE
        ElectricEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
      END IF
  END IF ! End of the Air Cooled/Evap Cooled Logic block

  ! If not air or evap cooled then set to the condenser node that is attached to a cooling tower
  CondInletTemp  = Node(CondInletNode)%Temp

  ! LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  MinPartLoadRat     = ElectricEIRChiller(EIRChillNum)%MinPartLoadRat
  MaxPartLoadRat     = ElectricEIRChiller(EIRChillNum)%MaxPartLoadRat
  MinUnloadRat       = ElectricEIRChiller(EIRChillNum)%MinUnLoadRat
  ChillerRefCap      = ElectricEIRChiller(EIRChillNum)%RefCap
  ReferenceCOP       = ElectricEIRChiller(EIRChillNum)%RefCOP
  EvapOutletTemp     = Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout   = ElectricEIRChiller(EIRChillNum)%TempLowLimitEvapOut
  EvapMassFlowRateMax  = ElectricEIRChiller(EIRChillNum)%EvapMassFlowRateMax

    ! Set mass flow rates
  IF (ElectricEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
    CondMassFlowRate = ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              ElectricEIRChiller(EIRChillNum)%CDLoopNum, &
                              ElectricEIRChiller(EIRChillNum)%CDLoopSideNum, &
                              ElectricEIRChiller(EIRChillNum)%CDBranchNum, &
                              ElectricEIRChiller(EIRChillNum)%CDCompNum)
    CALL PullCompInterconnectTrigger(ElectricEIRChiller(EIRChillNum)%CWLoopNum, &
                                     ElectricEIRChiller(EIRChillNum)%CWLoopSideNum, &
                                     ElectricEIRChiller(EIRChillNum)%CWBranchNum, &
                                     ElectricEIRChiller(EIRChillNum)%CWCompNum, &
                                     ElectricEIRChiller(EIRChillNum)%CondMassFlowIndex,              &
                                     ElectricEIRChiller(EIRChillNum)%CDLoopNum, &
                                     ElectricEIRChiller(EIRChillNum)%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)

    IF (CondMassFlowRate < MassFlowTolerance) RETURN

  END IF

  SELECT CASE (PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    IF ((ElectricEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
        (Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint /= SensedNodeFlagValue) ) THEN
         ! there will be a valid setpoint on outlet
       EvapOutletTempSetpoint = Node(EvapOutletNode)%TempSetPoint
    ELSE ! use plant loop overall setpoint
      EvapOutletTempSetpoint= Node(PlantLoop(PlantLoopNum)%TempSetPointNodeNum)%TempSetPoint
    ENDIF
  CASE (DualSetpointDeadband)
    IF ((ElectricEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
        (Node(ElectricEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
         ! there will be a valid setpoint on outlet
       EvapOutletTempSetpoint = Node(EvapOutletNode)%TempSetPointHi
    ELSE ! use plant loop overall setpoint
      EvapOutletTempSetpoint= Node(PlantLoop(PlantLoopNum)%TempSetPointNodeNum)%TempSetPointHi
    ENDIF
  END SELECT

  ! correct temperature if using heat recovery
  ! use report values for latest valid calculation, lagged somewhat
  IF ( ElectricEIRChiller(EIRChillNum)%HeatRecActive ) THEN
    If ( (ElectricEIRChillerReport(EIRChillNum)%QHeatRecovery &
           +  ElectricEIRChillerReport(EIRChillNum)%QCond) > 0.d0) THEN ! protect div by zero
      AvgCondSinkTemp = (ElectricEIRChillerReport(EIRChillNum)%QHeatRecovery &
                            * ElectricEIRChillerReport(EIRChillNum)%HeatRecInletTemp &
                          + ElectricEIRChillerReport(EIRChillNum)%QCond  &
                            * ElectricEIRChillerReport(EIRChillNum)%CondInletTemp) &
                          / (ElectricEIRChillerReport(EIRChillNum)%QHeatRecovery   &
                                 + ElectricEIRChillerReport(EIRChillNum)%QCond)
    ELSE
      AvgCondSinkTemp = CondInletTemp
    ENDIF
  ELSE
    AvgCondSinkTemp = CondInletTemp
  ENDIF

  ! Get capacity curve info with respect to CW setpoint and entering condenser water temps
  ChillerCapFT = CurveValue(ElectricEIRChiller(EIRChillNum)%ChillerCapFT, &
                            EvapOutletTempSetpoint,AvgCondSinkTemp)

  IF(ChillerCapFT .LT. 0)THEN
    IF(ElectricEIRChiller(EIRChillNum)%ChillerCapFTError .LT. 1 .AND. &
      PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
      .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElectricEIRChiller(EIRChillNum)%ChillerCapFTError = ElectricEIRChiller(EIRChillNum)%ChillerCapFTError + 1
      CALL ShowWarningError('CHILLER:ELECTRIC:EIR "'//TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":')
      CALL ShowContinueError(' Chiller Capacity as a Function of Temperature curve output is negative ('// &
                               TRIM(RoundSigDigits(ChillerCapFT,3))//').')
      CALL ShowContinueError(' Negative value occurs using an Evaporator Outlet Temp of ' &
                              //TRIM(RoundSigDigits(EvapOutletTempSetpoint,1))// &
                             ' and a Condenser Inlet Temp of '//TRIM(RoundSigDigits(CondInletTemp,1))//'.')
      CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
    ELSE IF(PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
            .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElectricEIRChiller(EIRChillNum)%ChillerCapFTError = ElectricEIRChiller(EIRChillNum)%ChillerCapFTError + 1
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:EIR "' &
                                        //TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":'//&
          ' Chiller Capacity as a Function of Temperature curve output is negative warning continues...' &
          , ElectricEIRChiller(EIRChillNum)%ChillerCapFTErrorIndex, ChillerCapFT, ChillerCapFT)
    END IF
    ChillerCapFT = 0.0d0
  END IF

  ! Available chiller capacity as a function of temperature
  AvailChillerCap = ChillerRefCap*ChillerCapFT

 !Only perform this check for temperature setpoint control
  IF (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpschemeType ==   &
        CompSetPtBasedSchemeType) THEN
    ! Calculate water side load

    Cp  = GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                             Node(EvapInletNode)%Temp,                      &
                             PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex, &
                             'CalcElectricEIRChillerModel')
    EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
    SELECT CASE (PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetpoint)
      TempLoad = EvapMassFlowRate * Cp * &
                 (Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint)
    CASE (DualSetpointDeadband)
      TempLoad = EvapMassFlowRate * Cp * &
                 (Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi)
    END SELECT
    TempLoad = MAX(0.0d0,TempLoad)

    ! MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
    IF (ABS(MyLoad) .GT. TempLoad) THEN
      MyLoad = SIGN(TempLoad, MyLoad)
    END IF
  END IF

  ! Part load ratio based on load and available chiller capacity, cap at max part load ratio
  IF(AvailChillerCap .GT. 0)THEN
    PartLoadRat = MAX(0.0d0, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
  ELSE
    PartLoadRat = 0.0d0
  END IF


  Cp  = GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                              Node(EvapInletNode)%Temp,                      &
                              PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex, &
                             'CalcElectricEIRChillerModel')


   IF(PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType ==   &
        CompSetPtBasedSchemeType)THEN
     ElectricEIRChiller(EIRChillNum)%PossibleSubCooling = .FALSE.
   ELSE
     ElectricEIRChiller(EIRChillNum)%PossibleSubCooling = .TRUE.
   ENDIF
   ! Set evaporator heat transfer rate
   QEvaporator = AvailChillerCap * PartLoadRat

   ! Either set the flow to the Constant value or caluclate the flow for the variable volume
   IF ((ElectricEIRChiller(EIRChillNum)%FlowMode == ConstantFlow)   &
      .OR. (ElectricEIRChiller(EIRChillNum)%FlowMode == NotModulated )) THEN
      ! Set the evaporator mass flow rate to design
      ! Start by assuming max (design) flow
      EvapMassFlowRate = EvapMassFlowRateMax
      ! Use SetComponentFlowRate to decide actual flow
      Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          ElectricEIRChiller(EIRChillNum)%CWLoopNum,     &
                          ElectricEIRChiller(EIRChillNum)%CWLoopSideNum, &
                          ElectricEIRChiller(EIRChillNum)%CWBranchNum,   &
                          ElectricEIRChiller(EIRChillNum)%CWCompNum)
      IF (EvapMassFlowRate /= 0.0D0) THEN
        EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      ELSE
        EvapDeltaTemp = 0.0D0
      ENDIF
      ! Evaluate outlet temp based on delta
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp

   ELSEIF(ElectricEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) THEN

      ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
      SELECT CASE (PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetpoint)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
      CASE (DualSetpointDeadband)
        EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
      END SELECT

      IF (EvapDeltaTemp /= 0) THEN
        ! Calculate desired flow to request based on load
        EvapMassFlowRate = ABS(QEvaporator/Cp/EvapDeltaTemp)
        IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTolerance) &
              ElectricEIRChiller(EIRChillNum)%PossibleSubCooling = .TRUE.
        !Check to see if the Maximum is exceeded, if so set to maximum
        EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          ElectricEIRChiller(EIRChillNum)%CWLoopNum,     &
                          ElectricEIRChiller(EIRChillNum)%CWLoopSideNum, &
                          ElectricEIRChiller(EIRChillNum)%CWBranchNum,   &
                          ElectricEIRChiller(EIRChillNum)%CWCompNum)
        ! Should we recalculate this with the corrected setpoint?
        SELECT CASE (PlantLoop(ElectricEIRChiller(EIRChillNum)%CWLoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetpoint)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
        CASE (DualSetpointDeadband)
          EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
        END SELECT
        QEvaporator = MAX(0.0d0,(EvapMassFlowRate*Cp*EvapDeltaTemp))
      ELSE
        ! Try to request zero flow
        EvapMassFlowRate=0.0d0
        ! Use SetComponentFlowRate to decide actual flow
        Call SetComponentFlowRate( EvapMassFlowRate,  &
                          EvapInletNode , EvapOutletNode  , &
                          ElectricEIRChiller(EIRChillNum)%CWLoopNum,     &
                          ElectricEIRChiller(EIRChillNum)%CWLoopSideNum, &
                          ElectricEIRChiller(EIRChillNum)%CWBranchNum,   &
                          ElectricEIRChiller(EIRChillNum)%CWCompNum)
        ! No deltaT since component is not running
        EvapOutletTemp = Node(EvapInletNode)%Temp
        QEvaporator = 0.0d0
        PartLoadRat = 0.0d0
        ChillerPartLoadRatio = PartLoadRat

        ! DSU? so what if the delta T is zero?  On FlowLock==0, the inlet temp could = setpoint, right?
        IF (ElectricEIRChiller(EIRChillNum)%DeltaTErrCount < 1 .AND. .NOT. WarmupFlag) THEN
          ElectricEIRChiller(EIRChillNum)%DeltaTErrCount=ElectricEIRChiller(EIRChillNum)%DeltaTErrCount+1
          CALL ShowWarningError('Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tsetpoint).')
          CALL ShowContinueErrorTimeStamp(' ')
        ELSE IF (.NOT. WarmupFlag) THEN
          ElectricEIRChiller(EIRChillNum)%ChillerCapFTError = ElectricEIRChiller(EIRChillNum)%ChillerCapFTError + 1
          CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:EIR "' &
                                    //TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":'//&
              ' Evaporator DeltaTemp = 0 in mass flow calculation warning continues...' &
              , ElectricEIRChiller(EIRChillNum)%DeltaTErrCountIndex, EvapDeltaTemp, EvapDeltaTemp)
        END IF

      END IF
   END IF  !End of Constant Variable Flow If Block

  IF(EvapMassFlowRate == 0.0d0)THEN
    MyLoad = 0.0d0
    IF (ElectricEIRChiller(EIRChillNum)%CondenserType == EvapCooled) THEN
      CALL CalcBasinHeaterPower(ElectricEIRChiller(EIRChillNum)%BasinHeaterPowerFTempDiff,&
                          ElectricEIRChiller(EIRChillNum)%BasinHeaterSchedulePtr,&
                          ElectricEIRChiller(EIRChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
    ENDIF
    ElectricEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
    RETURN
  END IF
  IF(ElectricEIRChiller(EIRChillNum)%PossibleSubCooling) THEN
    QEvaporator = ABS(MyLoad)
    EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
    EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
  ELSE
    EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTempSetpoint
    QEvaporator = MAX(0.0d0,(EvapMassFlowRate*Cp*EvapDeltaTemp))
    EvapOutletTemp = EvapOutletTempSetpoint
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
  IF(QEvaporator > ABS(MyLoad)) THEN
    IF(EvapMassFlowRate > MassFlowTolerance) THEN
      QEvaporator = ABS(MyLoad)
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE
      QEvaporator = 0.0d0
      EvapOutletTemp = Node(EvapInletNode)%Temp
    END IF
  END IF

  ! Checks QEvaporator on the basis of the machine limits.
  IF(QEvaporator > (AvailChillerCap * MaxPartLoadRat))THEN
    IF(EvapMassFlowRate > MassFlowTolerance) THEN
      QEvaporator = AvailChillerCap * MaxPartLoadRat
      EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
       ! evaporator outlet temperature is allowed to float upwards (recalculate AvailChillerCap? iterate?)
      EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
    ELSE
      QEvaporator = 0.0d0
      EvapOutletTemp = Node(EvapInletNode)%Temp
    END IF

  END IF

  IF(AvailChillerCap .GT. 0.0d0)THEN
    PartLoadRat = MAX(0.0d0,MIN((QEvaporator/AvailChillerCap),MaxPartLoadRat))
  ELSE
    PartLoadRat = 0.0d0
  END IF

  ! Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
  IF (PartLoadRat .LT. MinPartLoadRat) FRAC = MIN(1.0d0,(PartLoadRat/MinPartLoadRat))

  ! set the module level variable used for reporting FRAC
  ChillerCyclingRatio = FRAC

  ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
  IF(AvailChillerCap .GT. 0.0d0)THEN
    PartLoadRat = MAX(PartLoadRat,MinUnLoadRat)
  ELSE
    PartLoadRat = 0.0d0
  END IF

   ! set the module level variable used for reporting PLR
  ChillerPartLoadRatio = PartLoadRat

  ! calculate the load due to false loading on chiller over and above water side load
  ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - QEvaporator
  IF(ChillerFalseLoadRate .LT. SmallLoad) THEN
    ChillerFalseLoadRate = 0.0d0
  END IF
  IF(QEvaporator == 0.0d0 .AND. ElectricEIRChiller(EIRChillNum)%CondenserType == EvapCooled) THEN
    CALL CalcBasinHeaterPower(ElectricEIRChiller(EIRChillNum)%BasinHeaterPowerFTempDiff,&
                               ElectricEIRChiller(EIRChillNum)%BasinHeaterSchedulePtr,&
                               ElectricEIRChiller(EIRChillNum)%BasinHeaterSetPointTemp,BasinHeaterPower)
  END IF

  ChillerEIRFT   = CurveValue(ElectricEIRChiller(EIRChillNum)%ChillerEIRFT,EvapOutletTemp,AvgCondSinkTemp)
  IF(ChillerEIRFT .LT. 0.0d0)THEN
    IF(ElectricEIRChiller(EIRChillNum)%ChillerEIRFTError .LT. 1 .AND. PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElectricEIRChiller(EIRChillNum)%ChillerEIRFTError = ElectricEIRChiller(EIRChillNum)%ChillerEIRFTError + 1
      CALL ShowWarningError('CHILLER:ELECTRIC:EIR "'//TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":')
      CALL ShowContinueError(' Chiller EIR as a Function of Temperature curve output is negative (' &
                           //TRIM(RoundSigDigits(ChillerEIRFT,3))//').')
      CALL ShowContinueError(' Negative value occurs using an Evaporator Outlet Temp of ' &
                            //TRIM(RoundSigDigits(EvapOutletTemp,1))// &
                           ' and a Condenser Inlet Temp of '//TRIM(RoundSigDigits(CondInletTemp,1))//'.')
      CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
    ELSE IF(PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElectricEIRChiller(EIRChillNum)%ChillerEIRFTError = ElectricEIRChiller(EIRChillNum)%ChillerEIRFTError + 1
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:EIR "' &
                                        //TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":'//&
          ' Chiller EIR as a Function of Temperature curve output is negative warning continues...' &
          , ElectricEIRChiller(EIRChillNum)%ChillerEIRFTErrorIndex, ChillerEIRFT, ChillerEIRFT)
    END IF
    ChillerEIRFT = 0.0d0
  END IF

  ChillerEIRFPLR  = CurveValue(ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLR,PartLoadRat)
  IF(ChillerEIRFPLR .LT. 0.0d0)THEN
    IF(ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLRError .LT. 1 .AND. PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLRError = ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLRError + 1
      CALL ShowWarningError('CHILLER:ELECTRIC:EIR "'//TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":')
      CALL ShowContinueError(' Chiller EIR as a function of PLR curve output is negative (' &
                           //TRIM(RoundSigDigits(ChillerEIRFPLR,3))//').')
      CALL ShowContinueError(' Negative value occurs using a part-load ratio of '//TRIM(RoundSigDigits(PartLoadRat,3))//'.')
      CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
    ELSE IF(PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLRError = ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLRError + 1
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:EIR "' &
                                        //TRIM(ElectricEIRChiller(EIRChillNum)%Name)//'":'//&
          ' Chiller EIR as a function of PLR curve output is negative warning continues...' &
          , ElectricEIRChiller(EIRChillNum)%ChillerEIRFPLRErrorIndex, ChillerEIRFPLR, ChillerEIRFPLR)
    END IF
    ChillerEIRFPLR = 0.0d0
  END IF

  Power = (AvailChillerCap/ReferenceCOP) * ChillerEIRFPLR * ChillerEIRFT * FRAC

  QCondenser = Power*ElectricEIRChiller(EIRChillNum)%CompPowerToCondenserFrac + QEvaporator + ChillerFalseLoadRate

  IF (ElectricEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
    IF (CondMassFlowRate > MassFlowTolerance) THEN
       ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
      IF(ElectricEIRChiller(EIRChillNum)%HeatRecActive) CALL EIRChillerHeatRecovery(EIRChillNum,QCondenser, &
                                                               CondMassFlowRate,CondInletTemp,QHeatRecovered)
        Cp  = GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                 CondInletTemp,                      &
                                 PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                 'CalcElectricEIRChillerModel')

        CondOutletTemp = QCondenser/CondMassFlowRate/Cp + CondInletTemp
    ELSE
      CALL ShowSevereError('CalcElectricEIRChillerModel: Condenser flow = 0, for ElectricEIRChiller='//  &
                            TRIM(ElectricEIRChiller(EIRChillNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
       !DSU? maybe this could be handled earlier, check if this component has a load and an evap flow rate
       ! then if cond flow is zero, just make a request to the condenser,
       ! then just say it couldn't run until condenser loop wakes up.
       !CALL ShowFatalError('Program Terminates due to previous error condition.')
    END IF
  ELSE !Air Cooled or Evap Cooled

    IF(QCondenser > 0.0d0) THEN
      CondMassFlowRate = ElectricEIRChiller(EIRChillNum)%CondMassFlowRateMax * PartLoadRat
    ELSE
      CondMassFlowRate = 0.0d0
    END IF

     ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
    IF(ElectricEIRChiller(EIRChillNum)%HeatRecActive) CALL EIRChillerHeatRecovery(EIRChillNum,QCondenser, &
                                                             CondMassFlowRate,CondInletTemp,QHeatRecovered)

    IF(CondMassFlowRate .GT. 0.0d0)THEN
      Cp = PsyCpAirFnWTdb(Node(CondInletNode)%HumRat,CondInletTemp,'CalcElectricEIRChillerModel')
      CondOutletTemp = CondInletTemp + QCondenser/CondMassFlowRate/Cp
    ELSE
      CondOutletTemp = CondInletTemp
    END IF
  END IF

  ! Calculate condenser fan power
  IF(ChillerCapFT .GT. 0.0d0)THEN
    CondenserFanPower = ChillerRefCap*ElectricEIRChiller(EIRChillNum)%CondenserFanPowerRatio*FRAC
  ELSE
    CondenserFanPower = 0.0d0
  END IF

  RETURN

END SUBROUTINE CalcElectricEIRChillerModel

SUBROUTINE EIRChillerHeatRecovery(EIRChillNum,QCond,CondMassFlow,CondInletTemp,QHeatRec)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Richard Liesen
            !       DATE WRITTEN:    January 2004
            !       MODIFIED:        Richard Raustad, FSEC (occurrences of EIR only, calcs are identical to electric chiller)

            ! PURPOSE OF THIS SUBROUTINE:
            !  Calculate the heat recovered from the chiller condenser


            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE DataPlant,       ONLY: SingleSetpoint, DualSetpointDeadband, PlantLoop
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)     :: EIRChillNum   ! Number of the current electric EIR chiller being simulated
  REAL(r64),INTENT(INOut)       :: QCond         ! Current condenser load [W]
  REAL(r64),INTENT(Out)         :: QHeatRec      ! Amount of heat recovered [W]
  REAL(r64),INTENT(IN)          :: CondMassFlow  ! Current condenser mass flow [kg/s]
  REAL(r64),INTENT(IN)          :: CondInletTemp ! Current condenser inlet temp [C]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CondInletNode       ! Condenser inlet node number
  INTEGER :: CondOutletNode      ! Condenser outlet node number
  INTEGER :: HeatRecInNode       ! Node number of heat recovery water inlet node
  INTEGER :: HeatRecOutNode      ! Node number of heat recovery water outlet node
  REAL(r64)    :: QTotal              ! Total condenser heat [W]
  REAL(r64)    :: QCondTmp            ! Total condenser heat based on average temperatures [W]
  REAL(r64)    :: HeatRecInletTemp    ! Heat reclaim inlet temp [C]
  REAL(r64)    :: HeatRecMassFlowRate ! Heat reclaim mass flow rate [m3/s]
  REAL(r64)    :: FracHeatRec         ! Fraction of condenser heat reclaimed
  REAL(r64)    :: TAvgIn              ! Average inlet temperature of heat reclaim inlet and condenser inlet [C]
  REAL(r64)    :: TAvgOut             ! Average outlet temperature [C]
  REAL(r64)    :: CpHeatRec           ! Heat reclaim water inlet specific heat [J/kg-K]
  REAL(r64)    :: CpCond              ! Condenser water inlet specific heat [J/kg-K]
  REAL(r64)    :: THeatRecSetpoint    ! local value for heat recovery leaving setpoint [C]
  REAL(r64)    :: QHeatRecToSetpoint  ! load to heat recovery setpoint
  REAL(r64)    :: HeatRecHighInletLimit ! local value for inlet limit for heat recovery [C]


  ! Begin routine
  HeatRecInNode  = ElectricEIRChiller(EIRChillNum)%HeatRecInletNodeNum
  HeatRecOutNode = ElectricEIRChiller(EIRChillNum)%HeatRecOutletNodeNum
  CondInletNode  = ElectricEIRChiller(EIRChillNum)%CondInletNodeNum
  CondOutletNode = ElectricEIRChiller(EIRChillNum)%CondOutletNodeNum

   ! Inlet node to the heat recovery heat exchanger
  HeatRecInletTemp  = Node(HeatRecInNode)%Temp
  HeatRecMassFlowRate = Node(HeatRecInNode)%MassFlowRate

  CpHeatRec =  GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%FluidName,  &
                                 HeatRecInletTemp,                      &
                                 PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%FluidIndex, &
                                 'EIRChillerHeatRecovery')
  CpCond =  GetSpecificHeatGlycol(PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                 CondInletTemp,                      &
                                 PlantLoop(ElectricEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                 'EIRChillerHeatRecovery')

  ! Before we modify the QCondenser, the total or original value is transferred to QTot
  QTotal = QCond

  IF (ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum == 0) THEN ! use original algorithm that blends temps
    TAvgIn = (HeatRecMassFlowRate*CpHeatRec*HeatRecInletTemp + CondMassFlow*CpCond*CondInletTemp)/  &
               (HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond)

    TAvgOut = QTotal/(HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond) + TAvgIn

    QHeatRec = HeatRecMassFlowRate * CpHeatRec * (TAvgOut - HeatRecInletTemp)
    QHeatRec = MAX(QHeatRec, 0.d0) ! ensure non negative
   !check if heat flow too large for physical size of bundle
    QHeatRec = MIN(QHeatRec, ElectricEIRChiller(EIRChillNum)%HeatRecMaxCapacityLimit)
  ELSE ! use new algorithm to meet setpoint
    SELECT CASE (PlantLoop(ElectricEIRChiller(EIRChillNum)%HRLoopNum)%LoopDemandCalcScheme)

    CASE (SingleSetPoint)
      THeatRecSetpoint = Node(ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum)%TempSetPoint
    CASE (DualSetPointDeadBand)
      THeatRecSetpoint = Node(ElectricEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum)%TempSetPointHi
    END SELECT

    QHeatRecToSetpoint = HeatRecMassFlowRate *  CpHeatRec * (THeatRecSetpoint - HeatRecInletTemp)
    QHeatRecToSetpoint = MAX(QHeatRecToSetpoint, 0.d0)
    QHeatRec = MIN(QTotal,QHeatRecToSetpoint)
     !check if heat flow too large for physical size of bundle
    QHeatRec = MIN(QHeatRec, ElectricEIRChiller(EIRChillNum)%HeatRecMaxCapacityLimit)
  ENDIF

   ! check if limit on inlet is present and exceeded.
  IF (ElectricEIRChiller(EIRChillNum)%HeatRecInletLimitSchedNum > 0) THEN
    HeatRecHighInletLimit =  GetCurrentScheduleValue(ElectricEIRChiller(EIRChillNum)%HeatRecInletLimitSchedNum)
    IF ( HeatRecInletTemp > HeatRecHighInletLimit) THEN ! shut down heat recovery
      QHeatRec = 0.d0
    ENDIF
  ENDIF

  QCond = QTotal - QHeatRec

  ! Calculate a new Heat Recovery Coil Outlet Temp
  IF (HeatRecMassFlowRate > 0.0d0) THEN
    HeatRecOutletTemp = QHeatRec/(HeatRecMassFlowRate*CpHeatRec) + HeatRecInletTemp
  ELSE
    HeatRecOutletTemp = HeatRecInletTemp
  END IF

  RETURN

END SUBROUTINE EIRChillerHeatRecovery

SUBROUTINE UpdateElectricEIRChillerRecords(MyLoad,RunFlag,Num)

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Richard Raustad, FSEC
            !       DATE WRITTEN:    June 2004

            ! PURPOSE OF THIS SUBROUTINE:
            !  Reporting

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE PlantUtilities,  ONLY : SafeCopyPlantNode
  USE Psychrometrics,  ONLY : PsyHFnTdbW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)     :: MyLoad    ! Current load [W]
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! Chiller number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: EvapInletNode         ! Evaporator inlet node number
  INTEGER                :: EvapOutletNode        ! Evaporator outlet node number
  INTEGER                :: CondInletNode         ! Condenser inlet node number
  INTEGER                :: CondOutletNode        ! Condenser outlet node number
  INTEGER                :: HeatRecInNode         ! Node number of heat recovery water inlet node
  INTEGER                :: HeatRecOutNode        ! Node number of heat recovery water outlet node
  REAL(r64)              :: ReportingConstant     ! Number of seconds per HVAC system time step, to convert from W (J/s) to J

  ReportingConstant = TimeStepSys*SecInHour

  EvapInletNode  = ElectricEIRChiller(Num)%EvapInletNodeNum
  EvapOutletNode = ElectricEIRChiller(Num)%EvapOutletNodeNum
  CondInletNode  = ElectricEIRChiller(Num)%CondInletNodeNum
  CondOutletNode = ElectricEIRChiller(Num)%CondOutletNodeNum
  HeatRecInNode  = ElectricEIRChiller(Num)%HeatRecInletNodeNum
  HeatRecOutNode = ElectricEIRChiller(Num)%HeatRecOutletNodeNum

  IF (MyLoad>=0 .OR. .NOT. RunFlag) THEN ! Chiller not running so pass inlet states to outlet states
    ! Set node conditions
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp
    IF(ElectricEIRChiller(Num)%CondenserType /= WaterCooled) THEN
      Node(CondOutletNode)%HumRat   = Node(CondInletNode)%HumRat
      Node(CondOutletNode)%Enthalpy = Node(CondInletNode)%Enthalpy
    END IF


    ElectricEIRChillerReport(Num)%ChillerPartLoadRatio  = 0.0d0
    ElectricEIRChillerReport(Num)%ChillerCyclingRatio   = 0.0d0
    ElectricEIRChillerReport(Num)%ChillerFalseLoadRate  = 0.0d0
    ElectricEIRChillerReport(Num)%ChillerFalseLoad      = 0.0d0
    ElectricEIRChillerReport(Num)%Power                 = 0.0d0
    ElectricEIRChillerReport(Num)%QEvap                 = 0.0d0
    ElectricEIRChillerReport(Num)%QCond                 = 0.0d0
    ElectricEIRChillerReport(Num)%Energy                = 0.0d0
    ElectricEIRChillerReport(Num)%EvapEnergy            = 0.0d0
    ElectricEIRChillerReport(Num)%CondEnergy            = 0.0d0
    ElectricEIRChillerReport(Num)%EvapInletTemp         = Node(EvapInletNode)%Temp
    ElectricEIRChillerReport(Num)%CondInletTemp         = Node(CondInletNode)%Temp
    ElectricEIRChillerReport(Num)%CondOutletTemp        = Node(CondOutletNode)%Temp
    ElectricEIRChillerReport(Num)%EvapOutletTemp        = Node(EvapOutletNode)%Temp
    ElectricEIRChillerReport(Num)%Evapmdot              = EvapMassFlowRate  ! could still be flow if in series
    ElectricEIRChillerReport(Num)%Condmdot              = CondMassFlowRate  ! could still be flow if in series
    ElectricEIRChillerReport(Num)%ActualCOP             = 0.0d0
    ElectricEIRChillerReport(Num)%CondenserFanPowerUse  = 0.0d0
    ElectricEIRChillerReport(Num)%CondenserFanEnergyConsumption = 0.0d0
    IF (ElectricEIRChiller(Num)%CondenserType == EvapCooled) THEN
      ElectricEIRChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      ElectricEIRChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

    IF (ElectricEIRChiller(Num)%HeatRecActive) THEN

       CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)

       ElectricEIRChillerReport(Num)%QHeatRecovery      = 0.0d0
       ElectricEIRChillerReport(Num)%EnergyHeatRecovery = 0.0d0
       ElectricEIRChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
       ElectricEIRChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
       ElectricEIRChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate
    END IF

  ELSE ! Chiller is running, so pass calculated values
    ! Set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp
    IF(ElectricEIRChiller(Num)%CondenserType /= WaterCooled) THEN
      Node(CondOutletNode)%HumRat   = CondOutletHumRat
      Node(CondOutletNode)%Enthalpy = PsyHFnTdbW(CondOutletTemp, CondOutletHumRat)
    END IF

    ! Set node flow rates;  for these load based models
    ! assume that sufficient evaporator flow rate is available
    ElectricEIRChillerReport(Num)%ChillerPartLoadRatio  = ChillerPartLoadRatio
    ElectricEIRChillerReport(Num)%ChillerCyclingRatio   = ChillerCyclingRatio
    ElectricEIRChillerReport(Num)%ChillerFalseLoadRate  = ChillerFalseLoadRate
    ElectricEIRChillerReport(Num)%ChillerFalseLoad      = ChillerFalseLoadRate*TimeStepSys*SecInHour
    ElectricEIRChillerReport(Num)%Power                 = Power
    ElectricEIRChillerReport(Num)%QEvap                 = QEvaporator
    ElectricEIRChillerReport(Num)%QCond                 = QCondenser
    ElectricEIRChillerReport(Num)%Energy                = Power*TimeStepSys*SecInHour
    ElectricEIRChillerReport(Num)%EvapEnergy            = QEvaporator*TimeStepSys*SecInHour
    ElectricEIRChillerReport(Num)%CondEnergy            = QCondenser*TimeStepSys*SecInHour
    ElectricEIRChillerReport(Num)%EvapInletTemp         = Node(EvapInletNode)%Temp
    ElectricEIRChillerReport(Num)%CondInletTemp         = Node(CondInletNode)%Temp
    ElectricEIRChillerReport(Num)%CondOutletTemp        = Node(CondOutletNode)%Temp
    ElectricEIRChillerReport(Num)%EvapOutletTemp        = Node(EvapOutletNode)%Temp
    ElectricEIRChillerReport(Num)%Evapmdot              = EvapMassFlowRate
    ElectricEIRChillerReport(Num)%Condmdot              = CondMassFlowRate
    ElectricEIRChillerReport(Num)%CondenserFanPowerUse  = CondenserFanPower
    ElectricEIRChillerReport(Num)%CondenserFanEnergyConsumption = CondenserFanPower*TimeStepSys*SecInHour
    IF (Power .NE. 0.0d0) THEN
       ElectricEIRChillerReport(Num)%ActualCOP          = (QEvaporator+ChillerFalseLoadRate)/Power
    ELSE
       ElectricEIRChillerReport(Num)%ActualCOP          = 0.0d0
    END IF
    IF (ElectricEIRChiller(Num)%CondenserType == EvapCooled) THEN
      ElectricEIRChillerReport(Num)%BasinHeaterPower       = BasinHeaterPower
      ElectricEIRChillerReport(Num)%BasinHeaterConsumption = BasinHeaterPower*ReportingConstant
    ENDIF

    IF(ElectricEIRChiller(Num)%HeatRecActive) THEN

       CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)
       ElectricEIRChillerReport(Num)%QHeatRecovery      = QHeatRecovered
       ElectricEIRChillerReport(Num)%EnergyHeatRecovery = QHeatRecovered*TimeStepSys*SecInHour
       Node(HeatRecOutNode)%Temp                        = HeatRecOutletTemp
       ElectricEIRChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
       ElectricEIRChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
       ElectricEIRChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate
    END IF

  END IF

  ElectricEIRChillerReport(Num)%ChillerCapFT          = ChillerCapFT
  ElectricEIRChillerReport(Num)%ChillerEIRFT          = ChillerEIRFT
  ElectricEIRChillerReport(Num)%ChillerEIRFPLR        = ChillerEIRFPLR


RETURN

END SUBROUTINE UpdateElectricEIRChillerRecords


END MODULE ChillerElectricEIR


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!*******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!*******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


MODULE ChillerReformulatedEIR  ! DOE-2 Reformulated EIR ChillerModule (by Mark Hydeman)

          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !  This module simulates the performance of the electric vapor compression
          !  chiller using a reformulated model based on the DOE-2 EIR chiller.

          ! METHODOLOGY EMPLOYED:
          !  Once the PlantLoopManager determines that the Reformulated EIR chiller
          !  is available to meet a loop cooling demand, it calls SimReformulatedEIRChiller
          !  which in turn calls the reformulated EIR chiller model.
          !  The ReformulatedEIR chiller model is based on polynomial fits of chiller
          !  performance data.

          ! REFERENCES:
          ! 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
          !    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

          ! USE STATEMENTS:
USE DataInterfaces
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: MaxNameLength, InitConvTemp
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE Psychrometrics,  ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW
USE General,         ONLY: TrimSigDigits
USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
USE DataPlant,       ONLY: PlantLoop

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! MODULE PARAMETER DEFINITIONS:
! Chiller type parameters
INTEGER, PARAMETER :: AirCooled         = 1 ! Air-cooled condenser currently not allowed
INTEGER, PARAMETER :: WaterCooled       = 2 ! Only water-cooled condensers are currently allowed
INTEGER, PARAMETER :: EvapCooled        = 3 ! Evap-cooled condenser currently not allowed
! Performance curve variable parameters
INTEGER, PARAMETER :: LeavingCondenser  = 5


!chiller flow modes
INTEGER, PARAMETER :: FlowModeNotSet           = 200
INTEGER, PARAMETER :: ConstantFlow             = 201
INTEGER, PARAMETER :: NotModulated             = 202
INTEGER, PARAMETER :: LeavingSetpointModulated = 203

          ! MODULE VARIABLE DECLARATIONS:
PRIVATE
INTEGER        :: NumElecReformEIRChillers =0   ! Number of electric reformulated EIR chillers specified in input
REAL(r64)      :: CondMassFlowRate         =0.0d0 ! Condenser mass flow rate [kg/s]
REAL(r64)      :: EvapMassFlowRate         =0.0d0 ! Evaporator mass flow rate [kg/s]
REAL(r64)      :: CondOutletTemp           =0.0d0 ! Condenser outlet temperature [C]
REAL(r64)      :: EvapOutletTemp           =0.0d0 ! Evaporator outlet temperature [C]
REAL(r64)      :: Power                    =0.0d0 ! Rate of chiller electric energy use [W]
REAL(r64)      :: QEvaporator              =0.0d0 ! Rate of heat transfer to the evaporator coil [W]
REAL(r64)      :: QCondenser               =0.0d0 ! Rate of heat transfer to the condenser coil [W]
REAL(r64)      :: QHeatRecovered           =0.0d0 ! Rate of heat transfer to the heat recovery coil [W]
REAL(r64)      :: HeatRecOutletTemp        =0.0d0 ! Heat recovery outlet temperature [C]
!REAL(r64)      :: CondenserFanPower       =0.0d0 ! Condenser Fan Power (fan cycles with compressor) [W]
REAL(r64)      :: ChillerCapFT             =0.0d0 ! Chiller capacity fraction (evaluated as a function of temperature)
REAL(r64)      :: ChillerEIRFT             =0.0d0 ! Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
REAL(r64)      :: ChillerEIRFPLR           =0.0d0 ! Chiller EIR as a function of part-load ratio (PLR)
REAL(r64)      :: ChillerPartLoadRatio     =0.0d0 ! Chiller part-load ratio (PLR)
REAL(r64)      :: ChillerCyclingRatio      =0.0d0 ! Chiller cycling ratio
REAL(r64)      :: ChillerFalseLoadRate     =0.0d0 ! Chiller false load over and above the water-side load [W]
REAL(r64)      :: AvgCondSinkTemp          =0.d0!  condenser temperature value for use in curves [C]

TYPE ReformulatedEIRChillerSpecs
  CHARACTER(len=MaxNameLength) :: Name           = ' ' ! User identifier
  INTEGER           :: TypeNum                   = 0   ! plant loop type identifier
  CHARACTER(len=MaxNameLength) :: CAPFTName      = ' ' ! CAPFT curve name
  CHARACTER(len=MaxNameLength) :: EIRFTName      = ' ' ! EIRFT curve name
  CHARACTER(len=MaxNameLength) :: EIRFPLRName    = ' ' ! EIRPLR curve name
  INTEGER           :: CondenserType             = 0   ! Type of Condenser. Water Cooled is the only available option for now
  REAL(r64)         :: RefCap                    = 0.0d0 ! Reference capacity of the chiller [W]
  REAL(r64)         :: RefCOP                    = 0.0d0 ! Reference coefficient of performance [W/W]
  INTEGER           :: FlowMode          = FlowModeNotSet ! one of 3 modes for componet flow during operation
  LOGICAL           :: ModulatedFlowSetToLoop  =.FALSE. ! True if the setpoint is missing at the outlet node
  LOGICAL           :: ModulatedFlowErrDone    =.FALSE. ! true if setpoint warning issued
  REAL(r64)         :: EvapVolFlowRate           = 0.0d0 ! Reference water volumetric flow rate through the evaporator [m3/s]
  REAL(r64)         :: EvapMassFlowRateMax       = 0.0d0 ! Reference water mass flow rate through evaporator [kg/s]
  REAL(r64)         :: CondVolFlowRate           = 0.0d0 ! Reference water volumetric flow rate through the condenser [m3/s]
  REAL(r64)         :: CondMassFlowRateMax       = 0.0d0 ! Reference water mass flow rate through condenser [kg/s]
  REAL(r64)         :: CompPowerToCondenserFrac  = 0.0d0 ! Fraction of compressor electric power rejected by condenser [0 to 1]
  INTEGER           :: EvapInletNodeNum          = 0   ! Node number on the inlet side of the plant (evaporator side)
  INTEGER           :: EvapOutletNodeNum         = 0   ! Node number on the outlet side of the plant (evaporator side)
  INTEGER           :: CondInletNodeNum          = 0   ! Node number on the inlet side of the condenser
  INTEGER           :: CondOutletNodeNum         = 0   ! Node number on the outlet side of the condenser
  REAL(r64)         :: MinPartLoadRat            = 0.0d0 ! Minimum allowed operating fraction of full load
  REAL(r64)         :: MaxPartLoadRat            = 0.0d0 ! Maximum allowed operating fraction of full load
  REAL(r64)         :: OptPartLoadRat            = 0.0d0 ! Optimal operating fraction of full load
  REAL(r64)         :: MinUnLoadRat              = 0.0d0 ! Minimum unloading ratio
  REAL(r64)         :: TempRefCondIn             = 0.0d0 ! The reference secondary loop fluid temperature at the
                                                       ! chiller condenser side inlet for the reformulated chiller [C]
  REAL(r64)         :: TempRefCondOut            = 0.0d0 ! The reference secondary loop fluid temperature at the
                                                       ! chiller condenser side outlet for the reformulated chiller [C]
  REAL(r64)         :: TempRefEvapOut            = 0.0d0 ! The reference primary loop fluid
                                                       ! temperature at the chiller evaporator side outlet [C]
  REAL(r64)         :: TempLowLimitEvapOut       = 0.0d0 ! Low temperature shut off [C]
  REAL(r64)         :: DesignHeatRecVolFlowRate  = 0.0d0 ! Design water volumetric flow rate through heat recovery loop [m3/s]
  REAL(r64)         :: DesignHeatRecMassFlowRate = 0.0d0 ! Design water mass flow rate through heat recovery loop [kg/s]
  REAL(r64)         :: SizFac                    = 0.0d0 ! sizing factor
  LOGICAL           :: HeatRecActive         = .False. ! True when entered Heat Rec Vol Flow Rate > 0
  INTEGER           :: HeatRecInletNodeNum       = 0   ! Node number for the heat recovery inlet side of the condenser
  INTEGER           :: HeatRecOutletNodeNum      = 0   ! Node number for the heat recovery outlet side of the condenser
  REAL(r64)         :: HeatRecCapacityFraction   = 0.d0 ! user input for heat recovery capacity fraction []
  REAL(r64)         :: HeatRecMaxCapacityLimit   = 0.d0 ! Capacity limit for Heat recovery, one time calc [W]
  INTEGER           :: HeatRecSetpointNodeNum    = 0    ! index for system node with the heat recover leaving setpoint
  INTEGER           :: HeatRecInletLimitSchedNum = 0    ! index for schedule for the inlet high limit for heat recovery operation
  INTEGER           :: ChillerCapFT              = 0   ! Index for the total cooling capacity modifier curve
                                                       ! (function of leaving evaporator and condenser water temperatures)
  INTEGER           :: ChillerEIRFT              = 0   ! Index for the energy input ratio modifier curve
                                                       ! (function of leaving evaporator and condenser water temperatures)
  INTEGER           :: ChillerEIRFPLR            = 0   ! Index for the energy input ratio vs part-load ratio curve
                                                       ! (function of leaving condenser water temperature and part-load ratio)
!  INTEGER           :: CondFanPowerFCap         = 0   ! Condenser fan capacity as a function of chiller capacity
  INTEGER           :: ChillerCapFTError         = 0   ! Used for negative capacity as a function of temp warnings
  INTEGER           :: ChillerCapFTErrorIndex    = 0   ! Used for negative capacity as a function of temp warnings
  INTEGER           :: ChillerEIRFTError         = 0   ! Used for negative EIR as a function of temp warnings
  INTEGER           :: ChillerEIRFTErrorIndex    = 0   ! Used for negative EIR as a function of temp warnings
  INTEGER           :: ChillerEIRFPLRError       = 0   ! Used for negative EIR as a function of PLR warnings
  INTEGER           :: ChillerEIRFPLRErrorIndex  = 0   ! Used for negative EIR as a function of PLR warnings
  REAL(r64)         :: ChillerCAPFTXTempMin      = 0.0d0 ! Minimum value of CAPFT curve X variable [C]
  REAL(r64)         :: ChillerCAPFTXTempMax      = 0.0d0 ! Maximum value of CAPFT curve X variable [C]
  REAL(r64)         :: ChillerCAPFTYTempMin      = 0.0d0 ! Minimum value of CAPFT curve Y variable [C]
  REAL(r64)         :: ChillerCAPFTYTempMax      = 0.0d0 ! Maximum value of CAPFT curve Y variable [C]
  REAL(r64)         :: ChillerEIRFTXTempMin      = 0.0d0 ! Minimum value of EIRFT curve X variable [C]
  REAL(r64)         :: ChillerEIRFTXTempMax      = 0.0d0 ! Maximum value of EIRFT curve X variable [C]
  REAL(r64)         :: ChillerEIRFTYTempMin      = 0.0d0 ! Minimum value of EIRFT curve Y variable [C]
  REAL(r64)         :: ChillerEIRFTYTempMax      = 0.0d0 ! Maximum value of EIRFT curve Y variable [C]
  REAL(r64)         :: ChillerEIRFPLRTempMin     = 0.0d0 ! Minimum value of EIRFPLR curve condenser outlet temperature [C]
  REAL(r64)         :: ChillerEIRFPLRTempMax     = 0.0d0 ! Maximum value of EIRFPLR curve condenser outlet temperature [C]
  REAL(r64)         :: ChillerEIRFPLRPLRMin      = 0.0d0 ! Minimum value of EIRFPLR curve part-load ratio
  REAL(r64)         :: ChillerEIRFPLRPLRMax      = 0.0d0 ! Maximum value of EIRFPLR curve part-load ratio
  INTEGER           :: CAPFTXIter                = 0   ! Iteration counter for evaporator outlet temperature CAPFT warning messages
  INTEGER           :: CAPFTXIterIndex           = 0   ! Index for evaporator outlet temperature CAPFT warning messages
  INTEGER           :: CAPFTYIter                = 0   ! Iteration counter for condenser outlet temperature CAPFT warning messages
  INTEGER           :: CAPFTYIterIndex           = 0   ! Index for condenser outlet temperature CAPFT warning messages
  INTEGER           :: EIRFTXIter                = 0   ! Iteration counter for evaporator outlet temperature EIRFT warning messages
  INTEGER           :: EIRFTXIterIndex           = 0   ! Index for evaporator outlet temperature EIRFT warning messages
  INTEGER           :: EIRFTYIter                = 0   ! Iteration counter for condenser outlet temperature EIRFT warning messages
  INTEGER           :: EIRFTYIterIndex           = 0   ! Index for condenser outlet temperature EIRFT warning messages
  INTEGER           :: EIRFPLRTIter              = 0   ! Iteration counter for condenser outlet temperature EIRFPLR warning messages
  INTEGER           :: EIRFPLRTIterIndex         = 0   ! Index for condenser outlet temperature EIRFPLR warning messages
  INTEGER           :: EIRFPLRPLRIter            = 0   ! Iteration counter for part-load ratio EIRFPLR warning messages
  INTEGER           :: EIRFPLRPLRIterIndex       = 0   ! Index for part-load ratio EIRFPLR warning messages
  INTEGER           :: IterLimitExceededNum      = 0   ! Iteration limit exceeded for RegulaFalsi routine
  INTEGER           :: IterLimitErrIndex         = 0   ! Index to iteration limit warning for RegulaFalsi routine
  INTEGER           :: IterFailed                = 0   ! Iteration limit failed for RegulaFalsi routine
  INTEGER           :: IterFailedIndex           = 0   ! Index to iteration limit failed for RegulaFalsi routine
  INTEGER           :: DeltaTErrCount            = 0   ! Evaporator delta T equals 0 for variable flow chiller warning messages
  INTEGER           :: DeltaTErrCountIndex       = 0   ! Index to evaporator delta T = 0 for variable flow chiller warning messages
  INTEGER           :: CWLoopNum     = 0  ! chilled water plant loop index number
  INTEGER           :: CWLoopSideNum = 0  ! chilled water plant loop side index
  INTEGER           :: CWBranchNum   = 0  ! chilled water plant loop branch index
  INTEGER           :: CWCompNum     = 0  ! chilled water plant loop component index
  INTEGER           :: CDLoopNum     = 0  ! condenser water plant loop index number
  INTEGER           :: CDLoopSideNum = 0  ! condenser water plant loop side index
  INTEGER           :: CDBranchNum   = 0  ! condenser water plant loop branch index
  INTEGER           :: CDCompNum     = 0  ! condenser water plant loop component index
  INTEGER           :: HRLoopNum     = 0  ! heat recovery water plant loop index
  INTEGER           :: HRLoopSideNum = 0  ! heat recovery water plant loop side index
  INTEGER           :: HRBranchNum   = 0  ! heat recovery water plant loop branch index
  INTEGER           :: HRCompNum     = 0  ! heat recovery water plant loop component index
  INTEGER           :: CondMassFlowIndex = 0
!  CHARACTER(len=220):: MsgBuffer1    = ' ' !- buffer to print warning messages on following time step
!  CHARACTER(len=300):: MsgBuffer2    = ' ' !- buffer to print warning messages on following time step
!  REAL(r64)         :: MsgDataLast   = 0.0d0 ! value of data when warning occurred (passed to Recurring Warn)
!  LOGICAL           :: PrintMessage  = .FALSE. ! logical to determine if message is valid
!  INTEGER           :: MsgErrorCount = 0   ! number of occurrences of warning
!  INTEGER           :: ErrCount1     = 0   ! for recurring error messages
    LOGICAL           :: PossibleSubCooling = .FALSE. ! flag to indicate chiller is doing less cooling that requested
END TYPE ReformulatedEIRChillerSpecs


TYPE ReportVars
  REAL(r64)    :: ChillerPartLoadRatio  = 0.0d0 ! reporting: Chiller PLR (Load/Capacity)
  REAL(r64)    :: ChillerCyclingRatio   = 0.0d0 ! reporting: Chiller cycling ratio (time on/time step)
  REAL(r64)    :: ChillerFalseLoadRate  = 0.0d0 ! reporting: Chiller false load over and above water side load [J]
  REAL(r64)    :: ChillerFalseLoad      = 0.0d0 ! reporting: Chiller false load over and above water side load [W]
  REAL(r64)    :: Power                 = 0.0d0 ! reporting: Chiller power [W]
  REAL(r64)    :: QEvap                 = 0.0d0 ! reporting: Evaporator heat transfer rate [W]
  REAL(r64)    :: QCond                 = 0.0d0 ! reporting: Condenser heat transfer rate [W]
  REAL(r64)    :: Energy                = 0.0d0 ! reporting: Chiller electric consumption [J]
  REAL(r64)    :: EvapEnergy            = 0.0d0 ! reporting: Evaporator heat transfer energy [J]
  REAL(r64)    :: CondEnergy            = 0.0d0 ! reporting: Condenser heat transfer energy [J]
  REAL(r64)    :: CondInletTemp         = 0.0d0 ! reporting: Condenser inlet temperature [C]
  REAL(r64)    :: EvapInletTemp         = 0.0d0 ! reporting: Evaporator inlet temperature [C]
  REAL(r64)    :: CondOutletTemp        = 0.0d0 ! reporting: Condenser outlet temperature [C]
  REAL(r64)    :: EvapOutletTemp        = 0.0d0 ! reporting: Evaporator outlet temperature [C]
  REAL(r64)    :: Evapmdot              = 0.0d0 ! reporting: Evaporator mass flow rate [kg/s]
  REAL(r64)    :: Condmdot              = 0.0d0 ! reporting: Condenser mass flow rate [kg/s]
  REAL(r64)    :: ActualCOP             = 0.0d0 ! reporting: Coefficient of performance
  REAL(r64)    :: QHeatRecovery         = 0.0d0 ! reporting: Heat recovered from water-cooled condenser [W]
  REAL(r64)    :: EnergyHeatRecovery    = 0.0d0 ! reporting: Energy recovered from water-cooled condenser [J]
  REAL(r64)    :: HeatRecInletTemp      = 0.0d0 ! reporting: Heat reclaim inlet temperature [C]
  REAL(r64)    :: HeatRecOutletTemp     = 0.0d0 ! reporting: Heat reclaim outlet temperature [C]
  REAL(r64)    :: HeatRecMassFlow       = 0.0d0 ! reporting: Heat reclaim mass flow rate [kg/s]
  REAL(r64)    :: ChillerCapFT          = 0.0d0 ! reporting: Chiller capacity curve output value
  REAL(r64)    :: ChillerEIRFT          = 0.0d0 ! reporting: Chiller EIRFT curve output value
  REAL(r64)    :: ChillerEIRFPLR        = 0.0d0 ! reporting: Chiller EIRFPLR curve output value
!  REAL(r64)    :: CondenserFanPowerUse  = 0.0d0 ! reporting: Air-cooled condenser fan power [W]
!  REAL(r64)    :: CondenserFanEnergyConsumption = 0.0d0 ! reporting: Air-cooled condenser fan energy [J]
  REAL(r64)    :: ChillerCondAvgTemp    = 0.d0  ! reporting: average condenser temp for curves with Heat recovery [C]
END TYPE ReportVars


TYPE (ReformulatedEIRChillerSpecs), ALLOCATABLE, DIMENSION(:)  :: ElecReformEIRChiller  ! dimension to number of machines

TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) ::ElecReformEIRChillerReport
LOGICAL   :: GetInputREIR = .TRUE.! When TRUE, calls subroutine to read input file

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ChillerReformulatedEIR
PUBLIC   SimReformulatedEIRChiller
PRIVATE  GetElecReformEIRChillerInput
PRIVATE  InitElecReformEIRChiller
PRIVATE  SizeElecReformEIRChiller
PRIVATE  ControlReformEIRChillerModel
PRIVATE  CalcReformEIRChillerModel
PRIVATE  UpdateReformEIRChillerRecords
PRIVATE  ReformEIRChillerHeatRecovery
PRIVATE  CondOutTempResidual
PRIVATE  CheckMinMaxCurveBoundaries

CONTAINS
          ! MODULE SUBROUTINES:

! Beginning of Reformulated EIR Chiller Module Driver Subroutine
!*************************************************************************

SUBROUTINE SimReformulatedEIRChiller(EIRChillerType,EIRChillerName,EquipFlowCtrl, CompIndex,LoopNum,RunFlag,FirstIteration, &
                              InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor,  &
                              TempCondInDesign,TempEvapOutDesign )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This is the reformulated EIR chiller model driver. It gets the input for the
          !  models, initializes simulation variables, calls the appropriate model and sets
          !  up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY : ShowFatalError
  USE InputProcessor, ONLY : FindItemInList
  USE PlantUtilities, ONLY : UpdateChillerComponentCondenserSide, UpdateComponentHeatRecoverySide
  USE DataPlant,      ONLY : TypeOf_Chiller_ElectricReformEIR
  USE DataSizing,     ONLY : CurLoopNum

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: EIRChillerType   ! Type of chiller !unused1208
  CHARACTER(len=*), INTENT(IN) :: EIRChillerName   ! User specified name of chiller
  INTEGER, INTENT(IN)          :: EquipFlowCtrl    ! Flow control mode for the equipment
  LOGICAL, INTENT(IN)          :: RunFlag          ! Simulate chiller when TRUE
  LOGICAL, INTENT(IN)          :: FirstIteration   ! Initialize variables when TRUE
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip    ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad           ! Loop demand component will meet [W]
  REAL(r64), INTENT(INOUT)     :: MinCap           ! Minimum operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: MaxCap           ! Maximum operating capacity of chiller [W]
  REAL(r64), INTENT(INOUT)     :: OptCap           ! Optimal operating capacity of chiller [W]
  INTEGER, INTENT(INOUT)       :: CompIndex        ! Chiller number pointer
  INTEGER, INTENT(IN)          :: LoopNum          ! plant loop index pointer
  LOGICAL, INTENT(IN)          :: GetSizingFactor  ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)     :: SizingFactor     ! sizing factor
  REAL(r64), INTENT(INOUT)     :: TempCondInDesign
  REAL(r64), INTENT(INOUT)     :: TempEvapOutDesign


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: EIRChillNum
  INTEGER :: LoopSide

  IF (GetInputREIR) THEN
    CALL GetElecReformEIRChillerInput
    GetInputREIR = .FALSE.
  END IF

    ! Find the correct Chiller
  IF (CompIndex == 0) THEN
    EIRChillNum = FindItemInList(EIRChillerName,ElecReformEIRChiller%Name,NumElecReformEIRChillers)
    IF (EIRChillNum == 0) THEN
      CALL ShowFatalError('SimReformulatedEIRChiller: Specified Chiller not one of Valid Reformulated EIR Electric Chillers='//  &
         TRIM(EIRChillerName))
    ENDIF
    CompIndex=EIRChillNum
  ELSE
    EIRChillNum=CompIndex
    IF (EIRChillNum > NumElecReformEIRChillers .or. EIRChillNum < 1) THEN
      CALL ShowFatalError('SimReformulatedEIRChiller:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(EIRChillNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumElecReformEIRChillers))//  &
                          ', Entered Unit name='//TRIM(EIRChillerName))
    ENDIF
    IF (EIRChillerName /= ElecReformEIRChiller(EIRChillNum)%Name) THEN
      CALL ShowFatalError('SimReformulatedEIRChiller: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(EIRChillNum))// &
                          ', Unit name='//TRIM(EIRChillerName)//', stored Unit Name for that index='//  &
                          TRIM(ElecReformEIRChiller(EIRChillNum)%Name))
    ENDIF
  ENDIF



  IF (InitLoopEquip) THEN
    TempEvapOutDesign  = ElecReformEIRChiller(EIRChillNum)%TempRefEvapOut
    TempCondInDesign   = ElecReformEIRChiller(EIRChillNum)%TempRefCondIn
    CALL InitElecReformEIRChiller(EIRChillNum,RunFlag,MyLoad)
    CALL SizeElecReformEIRChiller(EIRChillNum)
    IF (LoopNum == ElecReformEIRChiller(EIRChillNum)%CWLoopNum) THEN
      MinCap = ElecReformEIRChiller(EIRChillNum)%RefCap*ElecReformEIRChiller(EIRChillNum)%MinPartLoadRat
      MaxCap = ElecReformEIRChiller(EIRChillNum)%RefCap*ElecReformEIRChiller(EIRChillNum)%MaxPartLoadRat
      OptCap = ElecReformEIRChiller(EIRChillNum)%RefCap*ElecReformEIRChiller(EIRChillNum)%OptPartLoadRat
    ELSE
      MinCap = 0.d0
      MaxCap = 0.d0
      OptCap = 0.d0
    ENDIF
    IF (GetSizingFactor) THEN
      SizingFactor = ElecReformEIRChiller(EIRChillNum)%SizFac
    END IF
    RETURN
  END IF

  IF (LoopNum == ElecReformEIRChiller(EIRChillNum)%CWLoopNum) THEN
    CALL InitElecReformEIRChiller(EIRChillNum,RunFlag,MyLoad)
    CALL ControlReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)
    CALL UpdateReformEIRChillerRecords(MyLoad,RunFlag,EIRChillNum)
  ELSEIF (LoopNum == ElecReformEIRChiller(EIRChillNum)%CDLoopNum) THEN
    LoopSide = ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum
    CALL UpdateChillerComponentCondenserSide(LoopNum, LoopSide, TypeOf_Chiller_ElectricReformEIR, &
                                     ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum,     &
                                     ElecReformEIRChiller(EIRChillNum)%CondOutletNodeNum,    &
                                     ElecReformEIRChillerReport(EIRChillNum)%QCond,          &
                                     ElecReformEIRChillerReport(EIRChillNum)%CondInletTemp,  &
                                     ElecReformEIRChillerReport(EIRChillNum)%CondOutletTemp, &
                                     ElecReformEIRChillerReport(EIRChillNum)%Condmdot, FirstIteration)
  ELSEIF (LoopNum == ElecReformEIRChiller(EIRChillNum)%HRLoopNum) THEN
    CALL UpdateComponentHeatRecoverySide(ElecReformEIRChiller(EIRChillNum)%HRLoopNum,               &
                                    ElecReformEIRChiller(EIRChillNum)%HRLoopSideNum,           &
                                    TypeOf_Chiller_ElectricReformEIR,                           &
                                    ElecReformEIRChiller(EIRChillNum)%HeatRecInletNodeNum,     &
                                    ElecReformEIRChiller(EIRChillNum)%HeatRecOutletNodeNum,    &
                                    ElecReformEIRChillerReport(EIRChillNum)%QHeatRecovery,     &
                                    ElecReformEIRChillerReport(EIRChillNum)%HeatRecInletTemp,  &
                                    ElecReformEIRChillerReport(EIRChillNum)%HeatRecOutletTemp, &
                                    ElecReformEIRChillerReport(EIRChillNum)%HeatRecMassFlow ,  &
                                    FirstIteration)
  ENDIF
  RETURN

END SUBROUTINE SimReformulatedEIRChiller

! End Reformulated EIR Chiller Module Driver Subroutine
!******************************************************************************

SUBROUTINE GetElecReformEIRChillerInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    July 2006

            ! PURPOSE OF THIS SUBROUTINE:
            !  This routine will get the input required by the Reformulated Electric EIR Chiller model

            ! METHODOLOGY EMPLOYED:
            !

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataInterfaces,        ONLY: ShowSevereError, ShowWarningError, ShowFatalError, SetupOutputVariable, ShowContinueError
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE CurveManager,          ONLY: GetCurveIndex
  USE FluidProperties,       ONLY: FindGlycol
  USE General,               ONLY: TrimSigDigits, RoundSigDigits
  USE PlantUtilities,        ONLY: RegisterPlantCompDesignFlow
  USE GlobalNames,           ONLY: VerifyUniqueChillerName
  USE DataSizing,            ONLY: Autosize
  USE DataGlobals,           ONLY: AnyEnergyManagementSystemInModel
  USE ScheduleManager,       ONLY: GetScheduleIndex

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! PARAMETERS
  CHARACTER(len=*), PARAMETER :: RoutineName='GetElecReformEIRChillerInput: ' ! include trailing blank space

            ! LOCAL VARIABLES
  INTEGER                     :: EIRChillerNum             ! Chiller counter
  INTEGER                     :: NumAlphas                 ! Number of elements in the alpha array
  INTEGER                     :: NumNums                   ! Number of elements in the numeric array
  INTEGER                     :: IOStat                    ! IO Status when calling get input subroutine
  LOGICAL, SAVE               :: ErrorsFound=.false.       ! True when input errors found
  LOGICAL                     :: IsNotOK                   ! Flag to verify name
  LOGICAL                     :: IsBlank                   ! Flag for blank name
  LOGICAL                     :: errflag                   ! Error flag, used to tell if a unique chiller name has been specified
  LOGICAL, SAVE       :: AllocatedFlag =.FALSE. ! True when arrays are allocated

            ! FLOW

  If (AllocatedFlag) RETURN

  cCurrentModuleObject =  'Chiller:Electric:ReformulatedEIR'
  NumElecReformEIRChillers = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumElecReformEIRChillers <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  END IF

  ! ALLOCATE ARRAYS
  ALLOCATE (ElecReformEIRChiller(NumElecReformEIRChillers))
  ALLOCATE (ElecReformEIRChillerReport(NumElecReformEIRChillers))
  AllocatedFlag = .TRUE.

  ! Load arrays with reformulated electric EIR chiller data
  DO EIRChillerNum = 1 , NumElecReformEIRChillers
    CALL GetObjectItem(cCurrentModuleObject,EIRChillerNum,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                    AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames, &
                    NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ElecReformEIRChiller%Name,EIRChillerNum-1,IsNotOK,IsBlank, &
                                TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    END IF
    CALL VerifyUniqueChillerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
    IF (errflag) THEN
      ErrorsFound=.true.
    END IF
    ElecReformEIRChiller(EIRChillerNum)%Name                = cAlphaArgs(1)
   ! Performance curves
    ElecReformEIRChiller(EIRChillerNum)%ChillerCapFT        = GetCurveIndex(cAlphaArgs(2))
    ElecReformEIRChiller(EIRChillerNum)%CAPFTName           = cAlphaArgs(2)
    IF (ElecReformEIRChiller(EIRChillerNum)%ChillerCapFT .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound = .TRUE.
    END IF

    ElecReformEIRChiller(EIRChillerNum)%ChillerEIRFT        = GetCurveIndex(cAlphaArgs(3))
    ElecReformEIRChiller(EIRChillerNum)%EIRFTName           = cAlphaArgs(3)
    IF (ElecReformEIRChiller(EIRChillerNum)%ChillerEIRFT .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      ErrorsFound = .TRUE.
    END IF

    ElecReformEIRChiller(EIRChillerNum)%EIRFPLRName         = cAlphaArgs(4)
    ElecReformEIRChiller(EIRChillerNum)%ChillerEIRFPLR      = GetCurveIndex(cAlphaArgs(4))
    IF (ElecReformEIRChiller(EIRChillerNum)%ChillerEIRFPLR .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      ErrorsFound = .TRUE.
    END IF

   ! Chilled water inlet/outlet node names are necessary
    IF (lAlphaFieldBlanks(5) ) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' is blank.')
      ErrorsFound=.true.
    END IF
    IF (lAlphaFieldBlanks(6) ) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(6))//' is blank.')
      ErrorsFound=.true.
    END IF

    ElecReformEIRChiller(EIRChillerNum)%EvapInletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    ElecReformEIRChiller(EIRChillerNum)%EvapOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),'Chilled Water Nodes')

    ElecReformEIRChiller(EIRChillerNum)%CondenserType = WaterCooled

    ! Condenser inlet/outlet node names are necessary
    IF (lAlphaFieldBlanks(7) ) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(7))//' is blank.')
      ErrorsFound=.true.
    END IF
    IF (lAlphaFieldBlanks(8) ) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(8))//' is blank.')
      ErrorsFound=.true.
    END IF

    ElecReformEIRChiller(EIRChillerNum)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,  &
             TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
    ElecReformEIRChiller(EIRChillerNum)%CondOutletNodeNum   = GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,  &
             TRIM(cCurrentModuleObject),cAlphaArgs(1), NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(7),cAlphaArgs(8),'Condenser Water Nodes')

    SELECT CASE (TRIM(cAlphaArgs(9)))
    CASE ( 'CONSTANTFLOW' )
      ElecReformEIRChiller(EIRChillerNum)%FlowMode = ConstantFlow
    CASE ( 'VARIABLEFLOW' )
      ElecReformEIRChiller(EIRChillerNum)%FlowMode = LeavingSetpointModulated
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Key choice is now called "LeavingSetpointModulated" and the simulation continues')
    CASE ('LEAVINGSETPOINTMODULATED')
      ElecReformEIRChiller(EIRChillerNum)%FlowMode = LeavingSetpointModulated
    CASE ('NOTMODULATED')
      ElecReformEIRChiller(EIRChillerNum)%FlowMode = NotModulated
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
      CALL ShowContinueError('Available choices are ConstantFlow, NotModulated, or LeavingSetpointModulated')
      CALL ShowContinueError('Flow mode NotModulated is assumed and the simulation continues.')
      ElecReformEIRChiller(EIRChillerNum)%FlowMode = NotModulated
    END SELECT


!   Chiller rated performance data
    ElecReformEIRChiller(EIRChillerNum)%RefCap                 = rNumericArgs(1)
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      ErrorsFound=.true.
    END IF
    ElecReformEIRChiller(EIRChillerNum)%RefCOP                 = rNumericArgs(2)
    IF (rNumericArgs(2) == 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      ErrorsFound=.true.
    END IF
    ElecReformEIRChiller(EIRChillerNum)%TempRefEvapOut         = rNumericArgs(3)
    ElecReformEIRChiller(EIRChillerNum)%TempRefCondOut         = rNumericArgs(4)
    ElecReformEIRChiller(EIRChillerNum)%EvapVolFlowRate        = rNumericArgs(5)
    ElecReformEIRChiller(EIRChillerNum)%CondVolFlowRate        = rNumericArgs(6)
    ElecReformEIRChiller(EIRChillerNum)%MinPartLoadRat         = rNumericArgs(7)
    ElecReformEIRChiller(EIRChillerNum)%MaxPartLoadRat         = rNumericArgs(8)
    ElecReformEIRChiller(EIRChillerNum)%OptPartLoadRat         = rNumericArgs(9)
    ElecReformEIRChiller(EIRChillerNum)%MinUnLoadRat           = rNumericArgs(10)
    ElecReformEIRChiller(EIRChillerNum)%SizFac                 = rNumericArgs(14)
    IF (ElecReformEIRChiller(EIRChillerNum)%SizFac <= 0.0d0) ElecReformEIRChiller(EIRChillerNum)%SizFac = 1.0d0

    IF(ElecReformEIRChiller(EIRChillerNum)%MinPartLoadRat .GT. ElecReformEIRChiller(EIRChillerNum)%MaxPartLoadRat) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(7))//' ['//TRIM(RoundSigDigits(rNumericArgs(7),3))//'] > '//  &
                              TRIM(cNumericFieldNames(8))//' ['//TRIM(RoundSigDigits(rNumericArgs(8),3))//']')
       CALL ShowContinueError('Minimum part load ratio must be less than or equal to the '// &
                              'maximum part load ratio ')
       ErrorsFound=.true.
    END IF

    IF(ElecReformEIRChiller(EIRChillerNum)%MinUnLoadRat .LT. ElecReformEIRChiller(EIRChillerNum)%MinPartLoadRat .OR. &
       ElecReformEIRChiller(EIRChillerNum)%MinUnLoadRat .GT. ElecReformEIRChiller(EIRChillerNum)%MaxPartLoadRat) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' = '//TRIM(RoundSigDigits(rNumericArgs(10),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' must be greater than or equal to the '// &
                              TRIM(cNumericFieldNames(7)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(10))//' must be less than or equal to the '// &
                              TRIM(cNumericFieldNames(8)))
       ErrorsFound=.true.
    END IF

    IF(ElecReformEIRChiller(EIRChillerNum)%OptPartLoadRat .LT. ElecReformEIRChiller(EIRChillerNum)%MinPartLoadRat .OR. &
       ElecReformEIRChiller(EIRChillerNum)%OptPartLoadRat .GT. ElecReformEIRChiller(EIRChillerNum)%MaxPartLoadRat) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' = '//TRIM(RoundSigDigits(rNumericArgs(9),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' must be greater than or equal to the '// &
                              TRIM(cNumericFieldNames(7)))
       CALL ShowContinueError(TRIM(cNumericFieldNames(9))//' must be less than or equal to the '// &
                              TRIM(cNumericFieldNames(8)))
       ErrorsFound=.true.
    END IF

    ElecReformEIRChiller(EIRChillerNum)%CompPowerToCondenserFrac = rNumericArgs(11)

    IF(ElecReformEIRChiller(EIRChillerNum)%CompPowerToCondenserFrac .LT. 0.0d0 .OR. &
       ElecReformEIRChiller(EIRChillerNum)%CompPowerToCondenserFrac .GT. 1.0d0) THEN
       CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
       CALL ShowContinueError(TRIM(cNumericFieldNames(11))//' = '//TRIM(RoundSigDigits(rNumericArgs(11),3)) )
       CALL ShowContinueError(TRIM(cNumericFieldNames(11))//' must be greater than or equal to zero' )
       CALL ShowContinueError(TRIM(cNumericFieldNames(11))//' must be less than or equal to one' )
       ErrorsFound=.true.
    END IF

    ElecReformEIRChiller(EIRChillerNum)%TempLowLimitEvapOut    = rNumericArgs(12)

   ! These are the optional heat recovery inputs
    ElecReformEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate = rNumericArgs(13)
    IF ((ElecReformEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate > 0.0d0) &
        .OR. (ElecReformEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate == Autosize)) THEN
      ElecReformEIRChiller(EIRChillerNum)%HeatRecActive=.True.
      ElecReformEIRChiller(EIRChillerNum)%HeatRecInletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(10),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 3, ObjectIsNotParent)
      IF (ElecReformEIRChiller(EIRChillerNum)%HeatRecInletNodeNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
        ErrorsFound=.True.
      END IF
      ElecReformEIRChiller(EIRChillerNum)%HeatRecOutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 3, ObjectIsNotParent)
      IF (ElecReformEIRChiller(EIRChillerNum)%HeatRecOutletNodeNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(11))//'='//TRIM(cAlphaArgs(11)))
        ErrorsFound=.True.
      END IF
      IF (ElecReformEIRChiller(EIRChillerNum)%CondenserType .NE. WaterCooled) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('Heat Recovery requires a Water Cooled Condenser.')
        ErrorsFound=.True.
      END IF

      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(10),cAlphaArgs(11),'Heat Recovery Nodes')

      If (ElecReformEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate > 0.d0) THEN
        Call RegisterPlantCompDesignFlow(ElecReformEIRChiller(EIRChillerNum)%HeatRecInletNodeNum, &
                                  ElecReformEIRChiller(EIRChillerNum)%DesignHeatRecVolFlowRate )
      ENDIF
      IF (NumNums > 14) THEN
        IF (.NOT. lNumericFieldBlanks(15)) THEN
          ElecReformEIRChiller(EIRChillerNum)%HeatRecCapacityFraction =  rNumericArgs(15)
        ELSE
          ElecReformEIRChiller(EIRChillerNum)%HeatRecCapacityFraction = 1.d0
        ENDIf
      ELSE
        ElecReformEIRChiller(EIRChillerNum)%HeatRecCapacityFraction = 1.d0
      ENDIF

      If (NumAlphas > 11) THEN
        IF (.NOT. lAlphaFieldBlanks(12)) THEN
          ElecReformEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum = GetScheduleIndex(cAlphaArgs(12))
          IF (ElecReformEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
            CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(12))//'='//TRIM(cAlphaArgs(12)))
            ErrorsFound=.True.
          ENDIF
        ELSE
          ElecReformEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum = 0
        ENDIF
      ELSE
        ElecReformEIRChiller(EIRChillerNum)%HeatRecInletLimitSchedNum = 0
      ENDIF

      IF (NumAlphas > 12) THEN
        IF ( .NOT. lAlphaFieldBlanks(13)) THEN
          ElecReformEIRChiller(EIRChillerNum)%HeatRecSetpointNodeNum = &
              GetOnlySingleNode(cAlphaArgs(13), ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                             NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
        ELSE
          ElecReformEIRChiller(EIRChillerNum)%HeatRecSetpointNodeNum = 0
        ENDIF
      ELSE
        ElecReformEIRChiller(EIRChillerNum)%HeatRecSetpointNodeNum = 0
      ENDIF

    ELSE
      ElecReformEIRChiller(EIRChillerNum)%HeatRecActive=.False.
      ElecReformEIRChiller(EIRChillerNum)%DesignHeatRecMassFlowRate = 0.0d0
      ElecReformEIRChiller(EIRChillerNum)%HeatRecInletNodeNum   = 0
      ElecReformEIRChiller(EIRChillerNum)%HeatRecOutletNodeNum   = 0
      IF ((.NOT. lAlphaFieldBlanks(10)) .OR. (.NOT. lAlphaFieldBlanks(11)) ) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowWarningError('Since Reference Heat Reclaim Volume Flow Rate = 0.0, heat recovery is inactive.')
        CALL ShowContinueError('However, node names were specified for heat recovery inlet or outlet nodes.')
      END IF
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  END IF

  DO EIRChillerNum = 1, NumElecReformEIRChillers
     CALL SetupOutputVariable('Chiller Part Load Ratio []', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerPartLoadRatio,'System','Average', &
                ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Cycling Ratio []', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerCyclingRatio,'System','Average', &
                ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Power [W]', &
          ElecReformEIRChillerReport(EIRChillerNum)%Power,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Electric Energy [J]', &
          ElecReformEIRChillerReport(EIRChillerNum)%Energy,'System','Sum',ElecReformEIRChiller(EIRChillerNum)%Name,  &
                               ResourceTypeKey='ELECTRICITY',EndUseKey='Cooling',GroupKey='Plant')

     CALL SetupOutputVariable('Chiller Evaporator Cooling Rate [W]', &
          ElecReformEIRChillerReport(EIRChillerNum)%QEvap,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Cooling Energy [J]', &
          ElecReformEIRChillerReport(EIRChillerNum)%EvapEnergy,'System','Sum',ElecReformEIRChiller(EIRChillerNum)%Name,  &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='CHILLERS',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller False Load Heat Transfer Rate [W]', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerFalseLoadRate,'System','Average', &
                ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller False Load Heat Transfer Energy [J]', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerFalseLoad,'System','Sum',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Inlet Temperature [C]', &
          ElecReformEIRChillerReport(EIRChillerNum)%EvapInletTemp,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Outlet Temperature [C]', &
          ElecReformEIRChillerReport(EIRChillerNum)%EvapOutletTemp,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Evaporator Mass Flow Rate [kg/s]', &
          ElecReformEIRChillerReport(EIRChillerNum)%Evapmdot,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Rate [W]', &
          ElecReformEIRChillerReport(EIRChillerNum)%QCond,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Heat Transfer Energy [J]', &
          ElecReformEIRChillerReport(EIRChillerNum)%CondEnergy,'System','Sum',ElecReformEIRChiller(EIRChillerNum)%Name,  &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATREJECTION',GroupKey='Plant')
     CALL SetupOutputVariable('Chiller COP [W/W]', &
          ElecReformEIRChillerReport(EIRChillerNum)%ActualCOP,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Capacity Temperature Modifier Multiplier []', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerCapFT,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller EIR Temperature Modifier Multiplier []', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerEIRFT,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller EIR Part Load Modifier Multiplier []', &
          ElecReformEIRChillerReport(EIRChillerNum)%ChillerEIRFPLR,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)

     CALL SetupOutputVariable('Chiller Condenser Inlet Temperature [C]', &
          ElecReformEIRChillerReport(EIRChillerNum)%CondInletTemp,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Outlet Temperature [C]', &
          ElecReformEIRChillerReport(EIRChillerNum)%CondOutletTemp,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
     CALL SetupOutputVariable('Chiller Condenser Mass Flow Rate [kg/s]', &
          ElecReformEIRChillerReport(EIRChillerNum)%Condmdot,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)

     ! If heat recovery is active then setup report variables
     IF (ElecReformEIRChiller(EIRChillerNum)%HeatRecActive) THEN
         CALL SetupOutputVariable('Chiller Total Recovered Heat Rate [W]', &
           ElecReformEIRChillerReport(EIRChillerNum)%QHeatRecovery,'System','Average',ElecReformEIRChiller(EIRChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Total Recovered Heat Energy [J]', &
           ElecReformEIRChillerReport(EIRChillerNum)%EnergyHeatRecovery,'System','Sum', &
           ElecReformEIRChiller(EIRChillerNum)%Name, &
           ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATRECOVERY',GroupKey='Plant')
         CALL SetupOutputVariable('Chiller Heat Recovery Inlet Temperature [C]', &
           ElecReformEIRChillerReport(EIRChillerNum)%HeatRecInletTemp,'System','Average', &
           ElecReformEIRChiller(EIRChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Heat Recovery Outlet Temperature [C]', &
           ElecReformEIRChillerReport(EIRChillerNum)%HeatRecOutletTemp,'System','Average', &
           ElecReformEIRChiller(EIRChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Heat Recovery Mass Flow Rate [kg/s]', &
           ElecReformEIRChillerReport(EIRChillerNum)%HeatRecMassFlow,'System','Average', &
           ElecReformEIRChiller(EIRChillerNum)%Name)
         CALL SetupOutputVariable('Chiller Effective Heat Rejection Temperature [C]', &
           ElecReformEIRChillerReport(EIRChillerNum)%ChillerCondAvgTemp,'System','Average', &
           ElecReformEIRChiller(EIRChillerNum)%Name)
     END IF

     IF (AnyEnergyManagementSystemInModel) THEN
       CALL SetupEMSInternalVariable('Chiller Nominal Capacity', ElecReformEIRChiller(EIRChillerNum)%Name, '[W]', &
                                     ElecReformEIRChiller(EIRChillerNum)%RefCap  )
     ENDIF

  END DO

RETURN

END SUBROUTINE GetElecReformEIRChillerInput

SUBROUTINE InitElecReformEIRChiller(EIRChillNum, RunFlag, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu, FSEC
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for initializations of the Reformulated Electric EIR Chiller variables

          ! METHODOLOGY EMPLOYED:
          !  Uses the status flags to trigger initializations.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : BeginEnvrnFlag, AnyEnergyManagementSystemInModel
  USE DataPlant,       ONLY : PlantLoop, TypeOf_Chiller_ElectricReformEIR, ScanPlantLoopsForObject, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete, LoopFlowStatus_NeedyIfLoopOn
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes, SetComponentFlowRate
  USE DataEnvironment, ONLY : StdBaroPress
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowContinueError
  USE ScheduleManager, ONLY : GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: EIRChillNum         ! Number of the current electric EIR chiller being simulated
  LOGICAL, INTENT(IN)  :: RunFlag             ! TRUE when chiller operating
  REAL(r64),INTENT(IN) :: MyLoad              ! Current load put on chiller

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE                             :: MyOneTimeFlag = .true. ! One time logic flag for allocating MyEnvrnFlag array
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag            ! Logical array to initialize when appropriate
  INTEGER                                  :: EvapInletNode          ! Node number for evaporator water inlet node
  INTEGER                                  :: EvapOutletNode         ! Node number for evaporator water outlet node
  INTEGER                                  :: CondInletNode          ! Node number for condenser water inlet node
  INTEGER                                  :: CondOutletNode         ! Node number for condenser water outlet node
  INTEGER                                  :: HeatRecInNode          ! Node number for heat recovery water inlet node
  INTEGER                                  :: HeatRecOutNode         ! Node number for heat recovery water outlet node
  REAL(r64)                                :: rho                    ! local fluid density
  REAL(r64)                                :: mdot                   ! local fluid mass flow rate
  REAL(r64)                                :: mdotCond               ! local fluid mass flow rate for condenser
  INTEGER                                  :: LoopNum
  INTEGER                                  :: LoopSideNum
  INTEGER                                  :: BranchIndex
  INTEGER                                  :: CompIndex
  LOGICAL                                  :: FatalError
  LOGICAL                                  :: errFlag
  LOGICAL                                  :: HeatRecRunFlag
  REAL(r64)                                :: HeatRecHighInletLimit
          ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumElecReformEIRChillers))
    ALLOCATE(MyFlag(NumElecReformEIRChillers))
    MyEnvrnFlag = .TRUE.
    MyFlag=.TRUE.
    MyOneTimeFlag = .false.
  END IF

  ! Initialize condenser nodes
  EvapInletNode  = ElecReformEIRChiller(EIRChillNum)%EvapInletNodeNum
  EvapOutletNode = ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum
  CondInletNode  = ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum
  CondOutletNode = ElecReformEIRChiller(EIRChillNum)%CondOutletNodeNum

  IF (ElecReformEIRChiller(EIRChillNum)%HeatRecActive ) THEN
    HeatRecInNode  = ElecReformEIRChiller(EIRChillNum)%HeatRecInletNodeNum
    HeatRecOutNode = ElecReformEIRChiller(EIRChillNum)%HeatRecOutletNodeNum
  ENDIF

  ! Init more variables
  IF (MyFlag(EIRChillNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag = .FALSE.
    CALL ScanPlantLoopsForObject(ElecReformEIRChiller(EIRChillNum)%Name, &
                                   TypeOf_Chiller_ElectricReformEIR, &
                                   ElecReformEIRChiller(EIRChillNum)%CWLoopNum, &
                                   ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                                   ElecReformEIRChiller(EIRChillNum)%CWBranchNum,&
                                   ElecReformEIRChiller(EIRChillNum)%CWCompNum, &
                                   LowLimitTemp = ElecReformEIRChiller(EIRChillNum)%TempLowLimitEvapOut, &
                                   InletNodeNumber = ElecReformEIRChiller(EIRChillNum)%EvapInletNodeNum,  &
                                   errFlag=errFlag)
    IF (ElecReformEIRChiller(EIRChillNum)%CondenserType /= AirCooled) THEN
      CALL ScanPlantLoopsForObject(ElecReformEIRChiller(EIRChillNum)%Name, &
                                     TypeOf_Chiller_ElectricReformEIR, &
                                     ElecReformEIRChiller(EIRChillNum)%CDLoopNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CDBranchNum,&
                                     ElecReformEIRChiller(EIRChillNum)%CDCompNum, &
                                     InletNodeNumber = ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum,  &
                                     errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElecReformEIRChiller(EIRChillNum)%CWLoopNum,      &
                                          ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum,  &
                                          ElecReformEIRChiller(EIRChillNum)%CDLoopNum,      &
                                          ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum,  &
                                          TypeOf_Chiller_ElectricReformEIR, .TRUE. )
    ENDIF
    IF (ElecReformEIRChiller(EIRChillNum)%HeatRecActive) THEN
      CALL ScanPlantLoopsForObject(ElecReformEIRChiller(EIRChillNum)%Name, &
                                     TypeOf_Chiller_ElectricReformEIR, &
                                     ElecReformEIRChiller(EIRChillNum)%HRLoopNum, &
                                     ElecReformEIRChiller(EIRChillNum)%HRLoopSideNum, &
                                     ElecReformEIRChiller(EIRChillNum)%HRBranchNum,&
                                     ElecReformEIRChiller(EIRChillNum)%HRCompNum, &
                                     InletNodeNumber = ElecReformEIRChiller(EIRChillNum)%HeatRecInletNodeNum,  &
                                     errFlag=errFlag)
      CALL InterConnectTwoPlantLoopSides( ElecReformEIRChiller(EIRChillNum)%CWLoopNum,      &
                                          ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum,  &
                                          ElecReformEIRChiller(EIRChillNum)%HRLoopNum,      &
                                          ElecReformEIRChiller(EIRChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_ElectricReformEIR , .TRUE.)
    ENDIF

    IF ((ElecReformEIRChiller(EIRChillNum)%CondenserType /= AirCooled) .AND. &
        (ElecReformEIRChiller(EIRChillNum)%HeatRecActive)  ) THEN
      CALL InterConnectTwoPlantLoopSides( ElecReformEIRChiller(EIRChillNum)%CDLoopNum,      &
                                          ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum,  &
                                          ElecReformEIRChiller(EIRChillNum)%HRLoopNum,      &
                                          ElecReformEIRChiller(EIRChillNum)%HRLoopSideNum,  &
                                          TypeOf_Chiller_ElectricReformEIR , .FALSE. )
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('InitElecReformEIRChiller: Program terminated due to previous condition(s).')
    ENDIF

    IF (ElecReformEIRChiller(EIRChillNum)%FlowMode == ConstantFlow) Then
      ! reset flow priority
      PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%LoopSide(ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum)% &
          Branch(ElecReformEIRChiller(EIRChillNum)%CWBranchNum)%Comp(ElecReformEIRChiller(EIRChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
    ENDIF

    IF (ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) Then
      ! reset flow priority
      PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%LoopSide(ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum)% &
          Branch(ElecReformEIRChiller(EIRChillNum)%CWBranchNum)%Comp(ElecReformEIRChiller(EIRChillNum)%CWCompNum)%FlowPriority &
              = LoopFlowStatus_NeedyIfLoopOn
      ! check if setpoint on outlet node
      IF ((Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
          (Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          IF (.NOT. ElecReformEIRChiller(EIRChillNum)%ModulatedFlowErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ElecReformEIRChiller(EIRChillNum)%Name) )
            CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller ' // &
                                             'in variable flow mode, use a SetpointManager')
            CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
            ElecReformEIRChiller(EIRChillNum)%ModulatedFlowErrDone = .TRUE.
          ENDIF
        ELSE
         ! need call to EMS to check node
          FatalError = .FALSE. ! but not really fatal yet, but should be.
          CALL CheckIfNodeSetpointManagedByEMS(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum,iTemperatureSetpoint, FatalError)
          IF (FatalError) THEN
            IF (.NOT. ElecReformEIRChiller(EIRChillNum)%ModulatedFlowErrDone) THEN
              CALL ShowWarningError('Missing temperature setpoint for LeavingSetpointModulated mode chiller named ' // &
                                          TRIM(ElecReformEIRChiller(EIRChillNum)%Name) )
              CALL ShowContinueError('  A temperature setpoint is needed at the outlet node of a chiller evaporator ' // &
                                             'in variable flow mode')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the chiller evaporator outlet node ')
              CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the outlet node ')
              CALL ShowContinueError('  The overall loop setpoint will be assumed for chiller. The simulation continues ... ')
              ElecReformEIRChiller(EIRChillNum)%ModulatedFlowErrDone = .TRUE.
            ENDIF
          ENDIF


        ENDIF
        ElecReformEIRChiller(EIRChillNum)%ModulatedFlowSetToLoop = .TRUE.
        Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
          Node(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
        Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi =                        &
          Node(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
      ENDIF
    ENDIF
    MyFlag(EIRChillNum)=.FALSE.
  ENDIF

  ! Initialize Demand Side Variables
!  IF((MyEnvrnFlag(EIRChillNum) .and. BeginEnvrnFlag) &
!     .OR. (Node(CondInletNode)%MassFlowrate <= 0.0 .AND. RunFlag)) THEN

  IF(MyEnvrnFlag(EIRChillNum) .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeElecReformEIRChiller(EIRChillNum)
    rho = GetDensityGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex,&
                                  'InitElecReformEIRChiller')

    ElecReformEIRChiller(EIRChillNum)%EvapMassFlowRateMax = ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate * rho

    CALL InitComponentNodes(0.d0,  ElecReformEIRChiller(EIRChillNum)%EvapMassFlowRateMax,  &
                          EvapInletNode,        &
                          EvapOutletNode,       &
                          ElecReformEIRChiller(EIRChillNum)%CWLoopNum,               &
                          ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum,           &
                          ElecReformEIRChiller(EIRChillNum)%CWBranchNum,             &
                          ElecReformEIRChiller(EIRChillNum)%CWCompNum)

    IF (ElecReformEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN

      rho = GetDensityGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                  ElecReformEIRChiller(EIRChillNum)%TempRefCondIn, &
                                  PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex,&
                                  'InitElecReformEIRChiller')
      ElecReformEIRChiller(EIRChillNum)%CondMassFlowRateMax = rho * ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate
      CALL InitComponentNodes(0.d0, ElecReformEIRChiller(EIRChillNum)%CondMassFlowRateMax ,&
                          CondInletNode,        &
                          CondOutletNode,       &
                          ElecReformEIRChiller(EIRChillNum)%CDLoopNum,               &
                          ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum,           &
                          ElecReformEIRChiller(EIRChillNum)%CDBranchNum,             &
                          ElecReformEIRChiller(EIRChillNum)%CDCompNum)
      Node(CondInletNode)%Temp = ElecReformEIRChiller(EIRChillNum)%TempRefCondIn
    ELSE ! air or evap air condenser
     ! Initialize maximum available condenser flow rate
      Node(CondInletNode)%MassFlowRate          = ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate * &
        PsyRhoAirFnPbTdbW(StdBaroPress,ElecReformEIRChiller(EIRChillNum)%TempRefCondIn,0.0D0,'InitElecReformEIRChiller')
      Node(CondOutletNode)%MassFlowRate         = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMaxAvail  = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMax       = Node(CondInletNode)%MassFlowrate
      Node(CondOutletNode)%MassFlowRateMax      = Node(CondInletNode)%MassFlowrate
      Node(CondInletNode)%MassFlowRateMinAvail  = 0.0d0
      Node(CondInletNode)%MassFlowRateMin       = 0.0d0
      Node(CondOutletNode)%MassFlowRateMinAvail = 0.0d0
      Node(CondOutletNode)%MassFlowRateMin      = 0.0d0
      Node(CondInletNode)%Temp = ElecReformEIRChiller(EIRChillNum)%TempRefCondIn

    ENDIF

    IF (ElecReformEIRChiller(EIRChillNum)%HeatRecActive) THEN
      rho = GetDensityGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%HRLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(ElecReformEIRChiller(EIRChillNum)%HRLoopNum)%FluidIndex,&
                                  'InitElecReformEIRChiller')
      ElecReformEIRChiller(EIRChillNum)%DesignHeatRecMassFlowRate = rho * &
                                      ElecReformEIRChiller(EIRChillNum)%DesignHeatRecVolFlowRate
      CALL InitComponentNodes(0.0D0, ElecReformEIRChiller(EIRChillNum)%DesignHeatRecMassFlowRate ,  &
                         ElecReformEIRChiller(EIRChillNum)%HeatRecInletNodeNum,        &
                         ElecReformEIRChiller(EIRChillNum)%HeatRecOutletNodeNum,       &
                         ElecReformEIRChiller(EIRChillNum)%HRLoopNum,               &
                         ElecReformEIRChiller(EIRChillNum)%HRLoopSideNum,           &
                         ElecReformEIRChiller(EIRChillNum)%HRBranchNum,             &
                         ElecReformEIRChiller(EIRChillNum)%HRCompNum)
      ! overall capacity limit
      ElecReformEIRChiller(EIRChillNum)%HeatRecMaxCapacityLimit = ElecReformEIRChiller(EIRChillNum)%HeatRecCapacityFraction &
                              *  (ElecReformEIRChiller(EIRChillNum)%RefCap + ElecReformEIRChiller(EIRChillNum)%RefCap &
                                                                              /ElecReformEIRChiller(EIRChillNum)%RefCOP)
    ENDIF

    MyEnvrnFlag(EIRChillNum) = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(EIRChillNum)=.true.
  END IF

  IF ((ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) &
       .AND. ElecReformEIRChiller(EIRChillNum)%ModulatedFlowSetToLoop) THEN
  ! fix for clumsy old input that worked because loop setpoint was spread.
  !  could be removed with transition, testing , model change, period of being obsolete.
    Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint =                        &
         Node(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPoint
    Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi =                        &
         Node(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%TempSetPointNodeNum)%TempSetPointHi
  ENDIF

  IF ((ABS(MyLoad) > 0.d0) .AND. RunFlag)  THEN
    mdot     = ElecReformEIRChiller(EIRChillNum)%EvapMassFlowRateMax
    mdotCond = ElecReformEIRChiller(EIRChillNum)%CondMassFlowRateMax
  ELSE
    mdot     = 0.d0
    mdotCond = 0.d0
  ENDIF

  CALL SetComponentFlowRate( mdot, EvapInletNode, EvapOutletNode,            &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopNum,     &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                              ElecReformEIRChiller(EIRChillNum)%CWBranchNum,   &
                              ElecReformEIRChiller(EIRChillNum)%CWCompNum)

  IF (ElecReformEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
    CALL SetComponentFlowRate( mdotCond, CondInletNode, CondOutletNode,            &
                                ElecReformEIRChiller(EIRChillNum)%CDLoopNum,     &
                                ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum, &
                                ElecReformEIRChiller(EIRChillNum)%CDBranchNum,   &
                                ElecReformEIRChiller(EIRChillNum)%CDCompNum)
  ENDIF
  ! Initialize heat recovery flow rates at node
  IF (ElecReformEIRChiller(EIRChillNum)%HeatRecActive ) THEN
    LoopNum      =  ElecReformEIRChiller(EIRChillNum)%HRLoopNum
    LoopSideNum  =  ElecReformEIRChiller(EIRChillNum)%HRLoopSideNum
    BranchIndex  =  ElecReformEIRChiller(EIRChillNum)%HRBranchNum
    CompIndex    =  ElecReformEIRChiller(EIRChillNum)%HRCompNum

    ! check if inlet limit active and if exceeded.
    IF (ElecReformEIRChiller(EIRChillNum)%HeatRecInletLimitSchedNum > 0) THEN
      HeatRecHighInletLimit =  GetCurrentScheduleValue(ElecReformEIRChiller(EIRChillNum)%HeatRecInletLimitSchedNum)
      IF ( Node(HeatRecInNode)%Temp > HeatRecHighInletLimit) THEN ! shut down heat recovery
        HeatRecRunFlag = .FALSE.
      ELSE
        HeatRecRunFlag = RunFlag
      ENDIF
    ELSE
        HeatRecRunFlag = RunFlag
    ENDIF

    If ( HeatRecRunFlag) Then
      mdot = ElecReformEIRChiller(EIRChillNum)%DesignHeatRecMassFlowRate
    ELSE
      mdot = 0.d0
    ENDIF

    CALL SetComponentFlowRate(mdot,HeatRecInNode,HeatRecOutNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)

  END IF

  RETURN

END SUBROUTINE InitElecReformEIRChiller

SUBROUTINE SizeElecReformEIRChiller(EIRChillNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2004
          !       MODIFIED       July 2006, L. Gu, modified for reformulated EIR chiller
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for sizing Reformulated Electric EIR Chiller Components for which capacities and flow rates
          !  have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          !  Obtains evaporator flow rate from the plant sizing array. Calculates reference capacity from
          !  the evaporator flow rate and the chilled water loop design delta T. The condenser flow rate
          !  is calculated from the reference capacity, the COP, and the condenser loop design delta T.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,    ONLY: PlantLoop, PlantSizesOkayToFinalize, TypeOf_Chiller_ElectricReformEIR
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE CurveManager, ONLY: CurveValue, GetCurveMinMaxValues
  USE OutputReportPredefined
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE StandardRatings,   ONLY: CalcChillerIPLV

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: EIRChillNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum            ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum        ! Plant Sizing index for condenser loop
  LOGICAL             :: ErrorsFound          ! If errors detected in input
  LOGICAL             :: LoopErrorsFound      ! Plant loop errors found
  REAL(r64)           :: SizingEvapOutletTemp ! Plant Sizing outlet temperature for CurLoopNum [C]
  REAL(r64)           :: SizingCondOutletTemp ! Plant Sizing outlet temperature for condenser loop [C]
  REAL(r64)           :: RefCAPFT             ! Capacity as a function of temperature curve output used for sizing
  CHARACTER(len=MaxNameLength) :: equipName                ! Name of chiller
  REAL(r64)                   :: CurveVal                  ! Used to verify EIR-FT/CAP-FT curves = 1 at reference conditions
  REAL(r64)                   :: CondTemp                  ! Used to verify EIRFPLR curve is > than 0 at reference conditions
  LOGICAL                     :: FoundNegValue = .FALSE.   ! Used to evaluate EIRFPLR curve objects
  INTEGER                     :: CurveCheck = 0            ! Used to evaluate EIRFPLR curve objects
  REAL(r64), DIMENSION(11)    :: CurveValArray             ! Used to evaluate EIRFPLR curve objects
  REAL(r64), DIMENSION(11)    :: CondTempArray             ! Used to evaluate EIRFPLR curve objects
  REAL(r64)                   :: CurveValTmp               ! Used to evaluate EIRFPLR curve objects
  REAL(r64)                   :: Density                   ! Density of condenser water used in warning messages
  REAL(r64)                   :: SpecificHeat              ! Specific heat of condenser water used in warning messages
  REAL(r64)                   :: CondenserCapacity         ! Full load (reference) condenser capacity used in warning messages
  CHARACTER(len=132)          :: StringVar                 ! Used for EIRFPLR warning messages
  INTEGER                     :: CurveValPtr               ! Index to EIRFPLR curve output
  REAL(r64)                   :: DeltaTCond                ! Full load delta T at condenser, used for checking curve objects
  REAL(r64)                   :: PLRTemp                   ! Temporary variable used for warning messages
  REAL(r64)                   :: rho
  REAL(r64)                   :: Cp
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power
  REAL(r64)           :: tmpEvapVolFlowRate ! local evaporator design volume flow rate
  REAL(r64)           :: tmpCondVolFlowRate ! local condenser design volume flow rate
  REAL(r64)           :: tmpHeatRecVolFlowRate ! local heat recovery design volume flow rate
  LOGICAL, SAVE       :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag   ! TRUE in order to calculate IPLV

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumElecReformEIRChillers))
    MyFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  PltSizNum = 0
  PltSizCondNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap          = ElecReformEIRChiller(EIRChillNum)%RefCap
  tmpEvapVolFlowRate = ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate
  tmpCondVolFlowRate = ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate


  IF (ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate == AutoSize) THEN
    PltSizCondNum = PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%PlantSizNum
  END IF

  ! find the appropriate Plant Sizing object
  PltSizNum = PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%PlantSizNum

  IF (ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpEvapVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate *   &
                               ElecReformEIRChiller(EIRChillNum)%SizFac
        IF (PlantSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      ELSE
        tmpEvapVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate = tmpEvapVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Chiller:Electric:ReformulatedEIR', &
                              ElecReformEIRChiller(EIRChillNum)%Name, &
                              'Reference Chilled Water Flow Rate [m3/s]', &
                              ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Reformulated Electric Chiller evap flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Reformulated Electric Chiller object='//TRIM(ElecReformEIRChiller(EIRChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(ElecReformEIRChiller(EIRChillNum)%EvapInletNodeNum,tmpEvapVolFlowRate)

  IF (ElecReformEIRChiller(EIRChillNum)%RefCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        IF(PltSizCondNum > 0)THEN
          SizingEvapOutletTemp = PlantSizData(PltSizNum)%ExitTemp
          SizingCondOutletTemp = PlantSizData(PltSizCondNum)%ExitTemp + PlantSizData(PltSizCondNum)%DeltaT
        ELSE
          SizingEvapOutletTemp = ElecReformEIRChiller(EIRChillNum)%TempRefEvapOut
          SizingCondOutletTemp = ElecReformEIRChiller(EIRChillNum)%TempRefCondOut
        END IF
        Cp  = GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                                    InitConvTemp,                      &
                                    PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex, &
                                    'SizeElecReformEIRChiller')

        rho  = GetDensityGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex,&
                                'SizeElecReformEIRChiller')

        RefCapFT = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerCapFT, SizingEvapOutletTemp,SizingCondOutletTemp)
        tmpNomCap = (Cp * rho * PlantSizData(PltSizNum)%DeltaT  * tmpEvapVolFlowRate) / RefCapFT
        IF (PlantSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%RefCap = tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%RefCap = tmpNomCap
      END IF
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput('Chiller:Electric:ReformulatedEIR', ElecReformEIRChiller(EIRChillNum)%Name, &
                              'Reference Capacity [W]', ElecReformEIRChiller(EIRChillNum)%RefCap)
    ELSE
      CALL ShowSevereError('Autosizing of Reformulated Electric Chiller reference capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Reformulated Electric Chiller object='//TRIM(ElecReformEIRChiller(EIRChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  IF (ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        rho  = GetDensityGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                'SizeElecReformEIRChiller')

        Cp  = GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                    ElecReformEIRChiller(EIRChillNum)%TempRefCondIn,                   &
                                    PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                    'SizeElecReformEIRChiller')
        tmpCondVolFlowRate = tmpNomCap * &
            (1.0d0 + (1.0d0/ElecReformEIRChiller(EIRChillNum)%RefCOP) *   &
               ElecReformEIRChiller(EIRChillNum)%CompPowerToCondenserFrac) / &
            ( PlantSizData(PltSizCondNum)%DeltaT * Cp * rho)
        IF (PlantSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate

      ELSE
        tmpCondVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate = tmpCondVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize)   &
         CALL ReportSizingOutput('Chiller:Electric:ReformulatedEIR', ElecReformEIRChiller(EIRChillNum)%Name, &
                              'Reference Condenser Water Flow Rate [m3/s]', &
                              ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Reformulated Electric EIR Chiller condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Reformulated Electric EIR Chiller object='//TRIM(ElecReformEIRChiller(EIRChillNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the reference condenser water volumetric flow rate for use by the condenser water loop sizing algorithms
  CALL RegisterPlantCompDesignFlow(ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum,tmpCondVolFlowRate)

  IF (ElecReformEIRChiller(EIRChillNum)%HeatRecActive) THEN
    tmpHeatRecVolFlowRate = ElecReformEIRChiller(EIRChillNum)%DesignHeatRecVolFlowRate
    IF (ElecReformEIRChiller(EIRChillNum)%DesignHeatRecVolFlowRate == Autosize) THEN
      tmpHeatRecVolFlowRate = tmpCondVolFlowRate * ElecReformEIRChiller(EIRChillNum)%HeatRecCapacityFraction
      IF (PlantSizesOkayToFinalize) THEN
        ElecReformEIRChiller(EIRChillNum)%DesignHeatRecVolFlowRate = tmpHeatRecVolFlowRate
        CALL ReportSizingOutput('Chiller:Electric:ReformulatedEIR', ElecReformEIRChiller(EIRChillNum)%Name, &
                              'Design Heat Recovery Fluid Flow Rate [m3/s]', &
                              ElecReformEIRChiller(EIRChillNum)%DesignHeatRecVolFlowRate)
      ENDIF
    ENDIF

    ! save the reference heat recovery fluid volumetric flow rate
    CALL RegisterPlantCompDesignFlow(ElecReformEIRChiller(EIRChillNum)%HeatRecInletNodeNum,tmpHeatRecVolFlowRate)
  ENDIF


  IF (PlantSizesOkayToFinalize) THEN
    IF (MyFlag(EIRChillNum)) THEN
      CALL CalcChillerIPLV(ElecReformEIRChiller(EIRChillNum)%Name, &
                           TypeOf_Chiller_ElectricReformEIR,           &
                           ElecReformEIRChiller(EIRChillNum)%RefCap, &
                           ElecReformEIRChiller(EIRChillNum)%RefCOP, &
                           ElecReformEIRChiller(EIRChillNum)%CondenserType, &
                           ElecReformEIRChiller(EIRChillNum)%ChillerCapFT, &
                           ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT, &
                           ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR, &
                           ElecReformEIRChiller(EIRChillNum)%MinUnLoadRat, &
                           ElecReformEIRChiller(EIRChillNum)%EvapVolFlowRate, &
                           ElecReformEIRChiller(EIRChillNum)%CDLoopNum, &
                           ElecReformEIRChiller(EIRChillNum)%CompPowerToCondenserFrac)
     MyFlag(EIRChillNum) = .FALSE.
   ENDIF
   !create predefined report
    equipName = ElecReformEIRChiller(EIRChillNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Chiller:Electric:ReformulatedEIR')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,ElecReformEIRChiller(EIRChillNum)%RefCOP)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,ElecReformEIRChiller(EIRChillNum)%RefCap)
  ENDIF

! Only check performance curves if Capacity and volumetric flow rate are greater than 0
  IF(ElecReformEIRChiller(EIRChillNum)%RefCap .GT. 0.0D0 .AND. &
    ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate .GT. 0.0D0) THEN
!   Check the CAP-FT, EIR-FT, and PLR curves at reference conditions and warn user if different from 1.0 by more than +-10%
    IF (ElecReformEIRChiller(EIRChillNum)%ChillerCAPFT > 0)THEN
      CurveVal = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerCAPFT, &
                            ElecReformEIRChiller(EIRChillNum)%TempRefEvapOut,ElecReformEIRChiller(EIRChillNum)%TempRefCondOut)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
        CALL ShowWarningError('Capacity ratio as a function of temperature curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = ' &
                                //TRIM(equipName))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
      CALL GetCurveMinMaxValues(ElecReformEIRChiller(EIRChillNum)%ChillerCAPFT, &
             ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTXTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTXTempMax, &
             ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMax)
    END IF

    IF (ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT > 0) THEN
      CurveVal = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT, &
                            ElecReformEIRChiller(EIRChillNum)%TempRefEvapOut,ElecReformEIRChiller(EIRChillNum)%TempRefCondOut)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Energy input ratio as a function of temperature curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = ' &
                               //TRIM(equipName))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
      CALL GetCurveMinMaxValues(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT, &
             ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTXTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTXTempMax, &
             ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMax)
    END IF

    IF (ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR > 0) THEN
      CurveVal = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR, &
                 ElecReformEIRChiller(EIRChillNum)%TempRefCondOut,1.0d0)
      IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0)THEN
        CALL ShowWarningError('Energy input ratio as a function of part-load ratio curve output is not equal to 1.0')
        CALL ShowContinueError('(+ or - 10%) at reference conditions for Chiller:Electric:ReformulatedEIR = ' &
                              //TRIM(equipName))
        CALL ShowContinueError('Curve output at reference conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
      END IF
      CALL GetCurveMinMaxValues(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR, &
             ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMax, &
             ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax)
      IF (ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin < 0 .OR. &
            ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin .GE. &
            ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax .OR. &
            ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin > 1) THEN
        CALL ShowSevereError('Invalid minimum value of PLR = ' &
                             //TRIM(TrimSigDigits(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin,3)) &
                             //' in bicubic curve = '//TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFPLRName)//' which is used')
        CALL ShowContinueError('by Chiller:Electric:ReformulatedEIR = '// TRIM(equipName)//'.')
        CALL ShowContinueError('The minimum value of PLR [y] must be from zero to 1, and less than the maximum value of PLR.')
        ErrorsFound=.True.
      END IF
      IF (ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax > 1.1 .OR. &
            ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax .LE. &
              ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin .OR. &
              ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax < 0) THEN
        CALL ShowSevereError('Invalid maximum value of PLR = ' &
                             //TRIM(TrimSigDigits(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax,3)) &
                             //' in bicubic curve = '//TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFPLRName)//' which is used')
        CALL ShowContinueError('by Chiller:Electric:ReformulatedEIR = '//TRIM(equipName)//'.')
        CALL ShowContinueError('The maximum value of PLR [y] must be from zero to 1.1, and greater than the minimum value ' &
                                 //'of PLR.')
        ErrorsFound=.True.
      END IF
    END IF

!  Initialize condenser reference inlet temperature (not a user input)
    Density  = GetDensityGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                  ElecReformEIRChiller(EIRChillNum)%TempRefCondOut, &
                                  PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex,&
                                  'SizeElecReformEIRChiller')

    SpecificHeat  = GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                 ElecReformEIRChiller(EIRChillNum)%TempRefCondOut,                      &
                                 PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                 'SizeElecReformEIRChiller')
    CondenserCapacity = ElecReformEIRChiller(EIRChillNum)%RefCap * &
           (1.0d0 + (1.0d0/ElecReformEIRChiller(EIRChillNum)%RefCOP)*ElecReformEIRChiller(EIRChillNum)%CompPowerToCondenserFrac)
    DeltaTCond = (CondenserCapacity) / &
                (ElecReformEIRChiller(EIRChillNum)%CondVolFlowRate * Density * SpecificHeat)
    ElecReformEIRChiller(EIRChillNum)%TempRefCondIn = ElecReformEIRChiller(EIRChillNum)%TempRefCondOut - DeltaTCond

!     Check EIRFPLR curve output. Calculate condenser inlet temp based on reference condenser outlet temp,
!     chiller capacity, and mass flow rate. Starting with the calculated condenser inlet temp and PLR = 0,
!     calculate the condenser outlet temp proportional to PLR and test the EIRFPLR curve output for negative numbers.
    FoundNegValue = .FALSE.
    IF (ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR > 0) THEN
      CurveValArray = 0.0d0
      CondTempArray = 0.0d0
      DO CurveCheck = 0, 10, 1
        PLRTemp = CurveCheck/10.0d0
        CondTemp    = ElecReformEIRChiller(EIRChillNum)%TempRefCondIn + (DeltaTCond*PLRTemp)
        CondTemp    = MIN(CondTemp, ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMax)
        CondTemp    = MAX(CondTemp, ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMin)
        IF(PLRTemp .LT. ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin)THEN
          CurveValTmp = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR, &
                       CondTemp, ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin)
        ELSE
          CurveValTmp = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR, CondTemp, PLRTemp)
        END IF
        IF(CurveValTmp .LT. 0.0d0) FoundNegValue = .TRUE.
        CurveValArray(CurveCheck+1) = INT(CurveValTmp*100.0d0)/100.0d0
        CondTempArray(CurveCheck+1) = INT(CondTemp*100.0d0)/100.0d0
      END DO
    END IF

!     Output warning message if negative values are found in the EIRFPLR curve output. Results in Fatal error.
    IF(FoundNegValue)THEN
      CALL ShowWarningError('Energy input to cooing output ratio function of part-load ratio curve shows negative values ')
      CALL ShowContinueError('for  Chiller:Electric:ReformulatedEIR = '// TRIM(equipName)//'.')
      CALL ShowContinueError('EIR as a function of PLR curve output at various part-load ratios and condenser '// &
                            'water temperatures shown below:')
      CALL ShowContinueError('PLR           =    0.00   0.10   0.20   0.30   0.40   0.50   0.60   0.70   0.80   0.90   1.00')
      WRITE(StringVar,530)(CondTempArray(CurveValPtr), CurveValPtr = 1, 11)
530     FORMAT('Cond Temp (C) = ',11(F7.2))
      CALL ShowContinueError(TRIM(StringVar))
      WRITE(StringVar,531)(CurveValArray(CurveValPtr), CurveValPtr = 1, 11)
531     FORMAT('Curve Output  = ',11(F7.2))
      CALL ShowContinueError(TRIM(StringVar))
      ErrorsFound = .TRUE.
    END IF
  ELSE ! just get curve min/max values if capacity or cond volume flow rate = 0
    CALL GetCurveMinMaxValues(ElecReformEIRChiller(EIRChillNum)%ChillerCAPFT, &
           ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTXTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTXTempMax, &
           ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMax)
    CALL GetCurveMinMaxValues(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT, &
           ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTXTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTXTempMax, &
           ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMax)
    CALL GetCurveMinMaxValues(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR, &
           ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMax, &
           ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin,ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax)
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeElecReformEIRChiller

SUBROUTINE ControlReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu, FSEC
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a vapor compression chiller using the reformulated model developed by Mark Hydeman

          ! METHODOLOGY EMPLOYED:
          ! Use empirical curve fits to model performance at off-design conditions. This subroutine
          ! calls Subroutines CalcReformEIRChillerModel and SolveRegulaFalsi to obtain solution.
          ! The actual chiller performance calculations are in Subroutine CalcReformEIRChillerModel.

          ! REFERENCES:
          ! 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
          !    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

          ! USE STATEMENTS:

 USE DataGlobals,     ONLY : WarmupFlag
 USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowWarningError, ShowContinueErrorTimeStamp, &
                             ShowRecurringWarningErrorAtEnd, ShowContinueError
 USE DataHVACGlobals, ONLY : SmallLoad
 USE CurveManager,    ONLY : GetCurveMinMaxValues
 USE DataBranchAirLoopPlant, ONLY: ControlType_SeriesActive
 USE General,         ONLY : SolveRegulaFalsi

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:

 INTEGER                :: EIRChillNum     ! Chiller number
 REAL(r64)              :: MyLoad          ! Operating load [W]
 LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep
 LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when chiller operating
 INTEGER, INTENT(IN)    :: EquipFlowCtrl   ! Flow control mode for the equipment

         ! SUBROUTINE PARAMETER DEFINITIONS:

 REAL(r64),PARAMETER :: Acc = 0.0001D0    ! Accuracy control for SolveRegulaFalsi
 INTEGER,PARAMETER          :: MaxIter = 500   ! Iteration control for SolveRegulaFalsi

         ! INTERFACE BLOCK SPECIFICATIONS:
         !  na

         ! DERIVED TYPE DEFINITIONS:
         !  na

         ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

 REAL(r64)              :: CAPFTYTmin            ! Minimum condenser leaving temperature allowed by CAPFT curve [C]
 REAL(r64)              :: CAPFTYTmax            ! Maximum condenser leaving temperature allowed by CAPFT curve [C]
 REAL(r64)              :: EIRFTYTmin            ! Minimum condenser leaving temperature allowed by EIRFT curve [C]
 REAL(r64)              :: EIRFTYTmax            ! Maximum condenser leaving temperature allowed by EIRFT curve [C]
 REAL(r64)              :: EIRFPLRTmin           ! Minimum condenser leaving temperature allowed by EIRFPLR curve [C]
 REAL(r64)              :: EIRFPLRTmax           ! Maximum condenser leaving temperature allowed by EIRFPLR curve [C]
 REAL(r64)              :: Tmin                  ! Minimum condenser leaving temperature allowed by curve objects [C]
 REAL(r64)              :: Tmax                  ! Maximum condenser leaving temperature allowed by curve objects [C]
 REAL(r64)              :: Par(6)                ! Pass parameters for RegulaFalsi solver
 REAL(r64)              :: FalsiCondOutTemp      ! RegulaFalsi condenser outlet temperature result [C]
 INTEGER                :: SolFla                ! Feedback flag from SolveRegulaFalsi
 REAL(r64)              :: CondTempMin     ! Condenser outlet temperature when using Tmin as input to CalcReformEIRChillerModel [C]
 REAL(r64)              :: CondTempMax     ! Condenser outlet temperature when using Tmax as input to CalcReformEIRChillerModel [C]

 IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN
   CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl, &
                                  Node(ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum)%Temp)
 ELSE

!  Find min/max condenser outlet temperature used by curve objects
   CAPFTYTmin = ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMin
   EIRFTYTmin = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMin
   EIRFPLRTmin = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMin
   Tmin = MIN(CAPFTYTmin, EIRFTYTmin, EIRFPLRTmin)

   CAPFTYTmax = ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMax
   EIRFTYTmax = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMax
   EIRFPLRTmax = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMax
   Tmax = MAX(CAPFTYTmax, EIRFTYTmax, EIRFPLRTmax)

!  Check that condenser outlet temperature is within curve object limits prior to calling RegulaFalsi
   CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,Tmin)
   CondTempMin = CondOutletTemp
   CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,Tmax)
   CondTempMax = CondOutletTemp

   IF(CondTempMin .GT. Tmin .AND. CondTempMax .LT. Tmax)THEN

!    Initialize iteration parameters for RegulaFalsi function
     Par(1) = EIRChillNum
     Par(2) = MyLoad
     IF (Runflag) THEN
       Par(3) = 1.0d0
     ELSE
       Par(3) = 0.0d0
     END IF
     IF (FirstIteration) THEN
       Par(4) = 1.0d0
     ELSE
       Par(4) = 0.0d0
     END IF
     !Par(5) = FlowLock !DSU
     Par(6) = EquipFlowCtrl

     CALL SolveRegulaFalsi(Acc, MaxIter, SolFla, FalsiCondOutTemp, CondOutTempResidual, Tmin, Tmax, Par)

     IF (SolFla == -1) THEN
       IF (.NOT. WarmupFlag) THEN
         ElecReformEIRChiller(EIRChillNum)%IterLimitExceededNum = ElecReformEIRChiller(EIRChillNum)%IterLimitExceededNum + 1
         IF (ElecReformEIRChiller(EIRChillNum)%IterLimitExceededNum .EQ. 1) THEN
           CALL ShowWarningError(TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
             ': Iteration limit exceeded calculating condenser outlet temperature and non-converged temperature is used')
         ELSE
           CALL ShowRecurringWarningErrorAtEnd(TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
              ': Iteration limit exceeded calculating condenser outlet temperature.', &
              ElecReformEIRChiller(EIRChillNum)%IterLimitErrIndex, CondOutletTemp, CondOutletTemp)
         END IF
       END IF
     ELSE IF (SolFla == -2) THEN
       IF (.NOT. WarmupFlag) THEN
         ElecReformEIRChiller(EIRChillNum)%IterFailed = ElecReformEIRChiller(EIRChillNum)%IterFailed + 1
         IF (ElecReformEIRChiller(EIRChillNum)%IterFailed .EQ. 1) THEN
           CALL ShowWarningError(TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
             ': Solution found when calculating condenser outlet temperature.'// &
             ' The inlet temperature will used and the simulation continues...')
           CALL ShowContinueError('Please check minimum and maximum values of x in EIRFPLR Curve ' &
                                  //TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFPLRName))
         ELSE
           CALL ShowRecurringWarningErrorAtEnd(TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
             ': Solution is not found in calculating condenser outlet temperature.', &
             ElecReformEIRChiller(EIRChillNum)%IterFailedIndex, CondOutletTemp, CondOutletTemp)
         END IF
       END IF
       CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl, &
            Node(ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum)%Temp)
     END IF
   ELSE
!    If iteration is not possible, average the min/max condenser outlet temperature and manually determine solution
     CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,(CondTempMin+CondTempMax)/2.0d0)
     CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl, CondOutletTemp)
   END IF

!  Call subroutine to evaluate all performance curve min/max values against evaporator/condenser outlet temps and PLR
   CALL CheckMinMaxCurveBoundaries(EIRChillNum, FirstIteration)

 END IF

 RETURN

END SUBROUTINE ControlReformEIRChillerModel

SUBROUTINE ReformEIRChillerHeatRecovery(EIRChillNum,QCond,CondMassFlow,CondInletTemp,QHeatRec)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    July 2006
            !       MODIFIED:        na

            ! PURPOSE OF THIS SUBROUTINE:
            !  Calculate the heat recovered from the chiller condenser

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE DataPlant, ONLY: SingleSetPoint, DualSetPointDeadBand, PlantLoop
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)     :: EIRChillNum   ! Number of the current electric EIR chiller being simulated
  REAL(r64),INTENT(INOUT)       :: QCond         ! Current condenser load [W]
  REAL(r64),INTENT(OUT)         :: QHeatRec      ! Amount of heat recovered [W]
  REAL(r64),INTENT(IN)          :: CondMassFlow  ! Current condenser mass flow [kg/s]
  REAL(r64),INTENT(IN)          :: CondInletTemp ! Current condenser inlet temp [C]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CondInletNode       ! Condenser inlet node number
  INTEGER :: CondOutletNode      ! Condenser outlet node number
  INTEGER :: HeatRecInNode       ! Node number for heat recovery water inlet node
  INTEGER :: HeatRecOutNode      ! Node number for heat recovery water outlet node
  REAL(r64)    :: QTotal              ! Total condenser heat [W]
!  REAL(r64)    :: QCondTmp            ! Total condenser heat based on average temperatures [W]
  REAL(r64)    :: HeatRecInletTemp    ! Heat reclaim inlet temp [C]
  REAL(r64)    :: HeatRecMassFlowRate ! Heat reclaim mass flow rate [m3/s]
  REAL(r64)    :: FracHeatRec         ! Fraction of condenser heat reclaimed
  REAL(r64)    :: TAvgIn              ! Average inlet temperature of heat reclaim inlet and condenser inlet [C]
  REAL(r64)    :: TAvgOut             ! Average outlet temperature [C]
  REAL(r64)    :: CpHeatRec           ! Heat reclaim water inlet specific heat [J/kg-K]
  REAL(r64)    :: CpCond              ! Condenser water inlet specific heat [J/kg-K]
  REAL(r64)    :: QHeatRecToSetpoint
  REAL(r64)    :: THeatRecSetpoint
  REAL(r64)    :: HeatRecHighInletLimit


  ! Begin routine
  HeatRecInNode  = ElecReformEIRChiller(EIRChillNum)%HeatRecInletNodeNum
  HeatRecOutNode = ElecReformEIRChiller(EIRChillNum)%HeatRecOutletNodeNum
  CondInletNode  = ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum
  CondOutletNode = ElecReformEIRChiller(EIRChillNum)%CondOutletNodeNum

   ! inlet node to the heat recovery heat exchanger
  HeatRecInletTemp  = Node(HeatRecInNode)%Temp
  HeatRecMassFlowRate = Node(HeatRecInNode)%MassFlowRate

  CpHeatRec =  GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%HRLoopNum)%FluidName,  &
                                     HeatRecInletTemp,                      &
                                     PlantLoop(ElecReformEIRChiller(EIRChillNum)%HRLoopNum)%FluidIndex, &
                                     'EIRChillerHeatRecovery')
  CpCond =  GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                  CondInletTemp,                      &
                                  PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                  'EIRChillerHeatRecovery')


  ! Before we modify the QCondenser, the total or original value is transferred to QTot
  QTotal = QCond


  IF (ElecReformEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum == 0) THEN ! use original algorithm that blends temps
    TAvgIn = (HeatRecMassFlowRate*CpHeatRec*HeatRecInletTemp + CondMassFlow*CpCond*CondInletTemp)/  &
               (HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond)

    TAvgOut = QTotal/(HeatRecMassFlowRate*CpHeatRec + CondMassFlow*CpCond) + TAvgIn

    QHeatRec = HeatRecMassFlowRate * CpHeatRec * (TAvgOut - HeatRecInletTemp)
    QHeatRec = MAX(QHeatRec, 0.d0) ! ensure non negative
   !check if heat flow too large for physical size of bundle
    QHeatRec = MIN(QHeatRec, ElecReformEIRChiller(EIRChillNum)%HeatRecMaxCapacityLimit)
  ELSE ! use new algorithm to meet setpoint
    SELECT CASE (PlantLoop(ElecReformEIRChiller(EIRChillNum)%HRLoopNum)%LoopDemandCalcScheme)

    CASE (SingleSetPoint)
      THeatRecSetpoint = Node(ElecReformEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum)%TempSetPoint
    CASE (DualSetPointDeadBand)
      THeatRecSetpoint = Node(ElecReformEIRChiller(EIRChillNum)%HeatRecSetpointNodeNum)%TempSetPointHi
    END SELECT

    QHeatRecToSetpoint = HeatRecMassFlowRate *  CpHeatRec * (THeatRecSetpoint - HeatRecInletTemp)
    QHeatRecToSetpoint = MAX(QHeatRecToSetpoint, 0.d0)
    QHeatRec = MIN(QTotal, QHeatRecToSetpoint)
     !check if heat flow too large for physical size of bundle
    QHeatRec = MIN(QHeatRec, ElecReformEIRChiller(EIRChillNum)%HeatRecMaxCapacityLimit)
  ENDIF

   ! check if limit on inlet is present and exceeded.
  IF (ElecReformEIRChiller(EIRChillNum)%HeatRecInletLimitSchedNum > 0) THEN
    HeatRecHighInletLimit =  GetCurrentScheduleValue(ElecReformEIRChiller(EIRChillNum)%HeatRecInletLimitSchedNum)
    IF ( HeatRecInletTemp > HeatRecHighInletLimit) THEN ! shut down heat recovery
      QHeatRec = 0.d0
    ENDIF
  ENDIF

  QCond = QTotal - QHeatRec

  ! Calculate a new Heat Recovery Coil Outlet Temp
  IF (HeatRecMassFlowRate > 0.0d0) THEN
    HeatRecOutletTemp = QHeatRec/(HeatRecMassFlowRate*CpHeatRec) + HeatRecInletTemp
  ELSE
    HeatRecOutletTemp = HeatRecInletTemp
  END IF

  RETURN

END SUBROUTINE ReformEIRChillerHeatRecovery

SUBROUTINE UpdateReformEIRChillerRecords(MyLoad,RunFlag,Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Lixing Gu, FSEC
            !       DATE WRITTEN:    July 2006

            ! PURPOSE OF THIS SUBROUTINE:
            !  Reporting

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:

  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE PlantUtilities,  ONLY : SafeCopyPlantNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)     :: MyLoad    ! Current load [W]
  LOGICAL, INTENT(IN)      :: RunFlag   ! TRUE if chiller operating
  INTEGER, INTENT(IN)      :: Num       ! Chiller number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: EvapInletNode         ! Evaporator inlet node number
  INTEGER  :: EvapOutletNode        ! Evaporator outlet node number
  INTEGER  :: CondInletNode         ! Condenser inlet node number
  INTEGER  :: CondOutletNode        ! Condenser outlet node number
  INTEGER  :: HeatRecInNode         ! Node number for the heat recovery water inlet node
  INTEGER  :: HeatRecOutNode        ! Node number for the heat recovery water outlet node


  EvapInletNode  = ElecReformEIRChiller(Num)%EvapInletNodeNum
  EvapOutletNode = ElecReformEIRChiller(Num)%EvapOutletNodeNum
  CondInletNode  = ElecReformEIRChiller(Num)%CondInletNodeNum
  CondOutletNode = ElecReformEIRChiller(Num)%CondOutletNodeNum
  HeatRecInNode  = ElecReformEIRChiller(Num)%HeatRecInletNodeNum
  HeatRecOutNode = ElecReformEIRChiller(Num)%HeatRecOutletNodeNum

  IF (MyLoad >= 0.d0 .OR. .NOT. RunFlag) THEN ! Chiller not running so pass inlet states to outlet states
    ! Set node temperatures
    Node(EvapOutletNode)%Temp     = Node(EvapInletNode)%Temp
    Node(CondOutletNode)%Temp     = Node(CondInletNode)%Temp


    ElecReformEIRChillerReport(Num)%ChillerPartLoadRatio  = 0.0d0
    ElecReformEIRChillerReport(Num)%ChillerCyclingRatio   = 0.0d0
    ElecReformEIRChillerReport(Num)%ChillerFalseLoadRate  = 0.0d0
    ElecReformEIRChillerReport(Num)%ChillerFalseLoad      = 0.0d0
    ElecReformEIRChillerReport(Num)%Power                 = 0.0d0
    ElecReformEIRChillerReport(Num)%QEvap                 = 0.0d0
    ElecReformEIRChillerReport(Num)%QCond                 = 0.0d0
    ElecReformEIRChillerReport(Num)%Energy                = 0.0d0
    ElecReformEIRChillerReport(Num)%EvapEnergy            = 0.0d0
    ElecReformEIRChillerReport(Num)%CondEnergy            = 0.0d0
    ElecReformEIRChillerReport(Num)%EvapInletTemp         = Node(EvapInletNode)%Temp
    ElecReformEIRChillerReport(Num)%CondInletTemp         = Node(CondInletNode)%Temp
    ElecReformEIRChillerReport(Num)%CondOutletTemp        = Node(CondOutletNode)%Temp
    ElecReformEIRChillerReport(Num)%EvapOutletTemp        = Node(EvapOutletNode)%Temp
    ElecReformEIRChillerReport(Num)%Evapmdot              = EvapMassFlowRate  ! could still be flow if in series
    ElecReformEIRChillerReport(Num)%Condmdot              = CondMassFlowRate  ! could still be flow if in series
    ElecReformEIRChillerReport(Num)%ActualCOP             = 0.0d0

    IF (ElecReformEIRChiller(Num)%HeatRecActive) THEN

       CALL SafeCopyPlantNode(HeatRecInNode, HeatRecOutNode)
       ElecReformEIRChillerReport(Num)%QHeatRecovery      = 0.0d0
       ElecReformEIRChillerReport(Num)%EnergyHeatRecovery = 0.0d0
       ElecReformEIRChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
       ElecReformEIRChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
       ElecReformEIRChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate

       ElecReformEIRChillerReport(Num)%ChillerCondAvgTemp = AvgCondSinkTemp
    END IF

  ELSE ! Chiller is running, so pass calculated values
    ! Set node temperatures
    Node(EvapOutletNode)%Temp     = EvapOutletTemp
    Node(CondOutletNode)%Temp     = CondOutletTemp
    ! Set node flow rates;  for these load based models
    ! assume that sufficient evaporator flow rate is available
    ElecReformEIRChillerReport(Num)%ChillerPartLoadRatio  = ChillerPartLoadRatio
    ElecReformEIRChillerReport(Num)%ChillerCyclingRatio   = ChillerCyclingRatio
    ElecReformEIRChillerReport(Num)%ChillerFalseLoadRate  = ChillerFalseLoadRate
    ElecReformEIRChillerReport(Num)%ChillerFalseLoad      = ChillerFalseLoadRate*TimeStepSys*SecInHour
    ElecReformEIRChillerReport(Num)%Power                 = Power
    ElecReformEIRChillerReport(Num)%QEvap                 = QEvaporator
    ElecReformEIRChillerReport(Num)%QCond                 = QCondenser
    ElecReformEIRChillerReport(Num)%Energy                = Power*TimeStepSys*SecInHour
    ElecReformEIRChillerReport(Num)%EvapEnergy            = QEvaporator*TimeStepSys*SecInHour
    ElecReformEIRChillerReport(Num)%CondEnergy            = QCondenser*TimeStepSys*SecInHour
    ElecReformEIRChillerReport(Num)%EvapInletTemp         = Node(EvapInletNode)%Temp
    ElecReformEIRChillerReport(Num)%CondInletTemp         = Node(CondInletNode)%Temp
    ElecReformEIRChillerReport(Num)%CondOutletTemp        = Node(CondOutletNode)%Temp
    ElecReformEIRChillerReport(Num)%EvapOutletTemp        = Node(EvapOutletNode)%Temp
    ElecReformEIRChillerReport(Num)%Evapmdot              = EvapMassFlowRate
    ElecReformEIRChillerReport(Num)%Condmdot              = CondMassFlowRate
    IF (Power .NE. 0.0d0) THEN
       ElecReformEIRChillerReport(Num)%ActualCOP          = (QEvaporator+ChillerFalseLoadRate)/Power
    ELSE
       ElecReformEIRChillerReport(Num)%ActualCOP          = 0.0d0
    END IF

    IF (ElecReformEIRChiller(Num)%HeatRecActive) THEN

       CALL SafeCopyPlantNode( HeatRecInNode, HeatRecOutNode)
       ElecReformEIRChillerReport(Num)%QHeatRecovery      = QHeatRecovered
       ElecReformEIRChillerReport(Num)%EnergyHeatRecovery = QHeatRecovered*TimeStepSys*SecInHour
       Node(HeatRecOutNode)%Temp                          = HeatRecOutletTemp
       ElecReformEIRChillerReport(Num)%HeatRecInletTemp   = Node(HeatRecInNode)%Temp
       ElecReformEIRChillerReport(Num)%HeatRecOutletTemp  = Node(HeatRecOutNode)%Temp
       ElecReformEIRChillerReport(Num)%HeatRecMassFlow    = Node(HeatRecInNode)%MassFlowRate

       ElecReformEIRChillerReport(Num)%ChillerCondAvgTemp = AvgCondSinkTemp
    END IF

  END IF

  ElecReformEIRChillerReport(Num)%ChillerCapFT        = ChillerCapFT
  ElecReformEIRChillerReport(Num)%ChillerEIRFT        = ChillerEIRFT
  ElecReformEIRChillerReport(Num)%ChillerEIRFPLR      = ChillerEIRFPLR


  RETURN

END SUBROUTINE UpdateReformEIRChillerRecords

REAL(r64) FUNCTION CondOutTempResidual(FalsiCondOutTemp, Par)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   May 2006
          !       MODIFIED       L.Gu, May 2006
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired condenser outlet temperature)
          !  Reformulated EIR chiller requires condenser outlet temperature to calculate capacity and power.

          ! METHODOLOGY EMPLOYED:
          !  Regula Falsi solver is used to calculate condenser outlet temperature.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
          !  na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: FalsiCondOutTemp        ! RegulaFalsi condenser outlet temperature result [C]
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Parameter array used to interface with RegulaFalsi solver

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !  na

          ! DERIVED TYPE DEFINITIONS:
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER              :: EIRChillNum         ! Chiller number
  REAL(r64)            :: MyLoad              ! Operating load [W]
  LOGICAL              :: FirstIteration      ! TRUE when first iteration of timestep
  LOGICAL              :: RunFlag             ! TRUE when chiller operating
  INTEGER              :: EquipFlowCtrl       ! Flow control mode for the equipment

! FalsiCondOutTemp = Value used by RegulaFalsi during iteration (used to evaluate CAPFT, EIRFT, and EIRFPLR curves)
!
! CondOutletTemp = Value calculated by CalcReformEIRChillerModel subroutine as shown below
! CondOutletTemp = QCondenser/CondMassFlowRate/CPCW(CondInletTemp) + CondInletTemp


  EIRChillNum = INT(Par(1))
  MyLoad = Par(2)
  IF (INT(Par(3)) == 1) THEN
    Runflag = .True.
  ELSE
    Runflag = .False.
  END IF
  IF (INT(Par(4)) == 1) THEN
    FirstIteration = .True.
  ELSE
    FirstIteration = .False.
  END IF
  !FlowLock = INT(Par(5))   !DSU
  EquipFlowCtrl = INT(Par(6))

  CALL CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,FalsiCondOutTemp)
  CondOutTempResidual = FalsiCondOutTemp - CondOutletTemp ! CondOutletTemp is module level variable, final value used for reporting

  RETURN

END FUNCTION CondOutTempResidual

SUBROUTINE CalcReformEIRChillerModel(EIRChillNum,MyLoad,Runflag,FirstIteration,EquipFlowCtrl,FalsiCondOutTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu, FSEC
          !       DATE WRITTEN   July 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Simulate a vapor compression chiller using the reformulated model developed by Mark Hydeman

          ! METHODOLOGY EMPLOYED:
          !  Use empirical curve fits to model performance at off-design conditions

          ! REFERENCES:
          ! 1. Hydeman, M., P. Sreedharan, N. Webb, and S. Blanc. 2002. "Development and Testing of a Reformulated
          !    Regression-Based Electric Chiller Model". ASHRAE Transactions, HI-02-18-2, Vol 108, Part 2, pp. 1118-1127.

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : WarmupFlag, CurrentTime
  USE DataInterfaces,  ONLY : ShowFatalError, ShowSevereError, ShowWarningError, ShowContinueErrorTimeStamp, &
                              ShowRecurringWarningErrorAtEnd, ShowContinueError
  USE DataHVACGlobals, ONLY : SmallLoad, SysTimeElapsed, TimeStepSys
  USE General,         ONLY : RoundSigDigits, CreateSysTimeIntervalString
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : DeltaTemptol,PlantLoop, SimPlantEquipTypes, TypeOf_Chiller_ElectricReformEIR,  &
                              CompSetPtBasedSchemeType, CriteriaType_MassFlowRate, SingleSetpoint, DualSetpointDeadband
  USE DataBranchAirLoopPlant, ONLY: ControlType_SeriesActive, MassFlowTolerance
  USE DataEnvironment, ONLY : EnvironmentName, CurMnDy
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: EIRChillNum      ! Chiller number
  REAL(r64)              :: MyLoad           ! Operating load [W]
  LOGICAL                :: FirstIteration   ! TRUE when first iteration of timestep !unused1208
  LOGICAL, INTENT(IN)    :: RunFlag          ! TRUE when chiller operating
  INTEGER, INTENT(IN)    :: EquipFlowCtrl    ! Flow control mode for the equipment
  REAL(r64), INTENT(IN)  :: FalsiCondOutTemp ! RegulaFalsi condenser outlet temperature result [C]

          ! SUBROUTINE PARAMETER DEFINITIONS:

  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F6.2)'

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: FRAC                  ! Chiller cycling ratio
  REAL(r64)              :: MinPartLoadRat        ! Minimum allowed operating fraction of full load
  REAL(r64)              :: MinUnloadRat          ! Minimum allowed unloading fraction of full load
  REAL(r64)              :: MaxPartLoadRat        ! Maximum allowed operating fraction of full load
  REAL(r64)              :: EvapInletTemp         ! Evaporator inlet temperature [C]
  REAL(r64)              :: CondInletTemp         ! Condenser inlet temperature [C]
  REAL(r64)              :: EvapOutletTempSetpoint ! Evaporator outlet temperature setpoint [C]
  REAL(r64)              :: AvailChillerCap       ! Chiller available capacity [W]
  REAL(r64)              :: ChillerRefCap         ! Chiller reference capacity [W]
  REAL(r64)              :: EvapDeltaTemp         ! Evaporator temperature difference [C]
  REAL(r64)              :: ReferenceCOP          ! Reference coefficient of performance, from user input
  REAL(r64)              :: PartLoadRat           ! Operating part load ratio
  REAL(r64)              :: TempLowLimitEout      ! Evaporator low temp. limit cut off [C]
  REAL(r64)              :: EvapMassFlowRateMax   ! Maximum evaporator mass flow rate converted from volume flow rate [kg/s]
  INTEGER                :: EvapInletNode         ! evaporator inlet node number
  INTEGER                :: EvapOutletNode        ! evaporator outlet node number
  INTEGER                :: CondInletNode         ! condenser inlet node number
  INTEGER                :: CondOutletNode        ! condenser outlet node number
!  LOGICAL, SAVE          :: PossibleSubCooling
  REAL(r64)              :: TempLoad              ! actual load to be met by chiller. This value is compared to MyLoad
                                                  ! and reset when necessary since this chiller can cycle, the load passed
                                                  ! should be the actual load.  Instead the minimum PLR * RefCap is
                                                  ! passed in.
  INTEGER                :: PlantLoopNum          ! Plant loop which contains the current chiller
  INTEGER                :: LoopSideNum           ! Plant loop side which contains the current chiller (usually supply side)
  INTEGER                :: BranchNum
  INTEGER                :: CompNum
  REAL(r64)              :: Cp                    ! Local fluid specific heat


!  REAL(r64),SAVE         :: TimeStepSysLast=0.0     ! last system time step (used to check for downshifting)
!  REAL(r64)              :: CurrentEndTime          ! end time of time step for current simulation time step
!  REAL(r64),SAVE         :: CurrentEndTimeLast=0.0  ! end time of time step for last simulation time step
!  CHARACTER(len=6)       :: OutputChar = ' '        ! character string for warning messages

! Set module level inlet and outlet nodes and initialize other local variables
  ChillerPartLoadRatio       = 0.0d0
  ChillerCyclingRatio        = 0.0d0
  ChillerFalseLoadRate       = 0.0d0
  EvapMassFlowRate           = 0.0d0
  CondMassFlowRate           = 0.0d0
  Power                      = 0.0d0
  QCondenser                 = 0.0d0
  QEvaporator                = 0.0d0
  QHeatRecovered             = 0.0d0
!  CondenserFanPower          = 0.0
  EvapInletNode  = ElecReformEIRChiller(EIRChillNum)%EvapInletNodeNum
  EvapOutletNode = ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum
  CondInletNode  = ElecReformEIRChiller(EIRChillNum)%CondInletNodeNum
  CondOutletNode = ElecReformEIRChiller(EIRChillNum)%CondOutletNodeNum
  PlantLoopNum        = ElecReformEIRChiller(EIRChillNum)%CWLoopNum
  LoopSideNum         = ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum
  BranchNum         = ElecReformEIRChiller(EIRChillNum)%CWBranchNum
  CompNum         = ElecReformEIRChiller(EIRChillNum)%CWCompNum

! Set performance curve outputs to 0.0 when chiller is off
  ChillerCapFT = 0.0d0
  ChillerEIRFT = 0.0d0
  ChillerEIRFPLR = 0.0d0

! Set module-level chiller evap and condenser inlet temperature variables
  EvapInletTemp  = Node(EvapInletNode)%Temp
  CondInletTemp  = Node(CondInletNode)%Temp

! This chiller is currenlty has only a water-cooled condenser
!
!! calculate end time of current time step
!  CurrentEndTime = CurrentTime + SysTimeElapsed
!
!! Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!! Wait for next time step to print warnings. If simulation iterates, print out
!! the warning for the last iteration only. Must wait for next time step to accomplish this.
!! If a warning occurs and the simulation down shifts, the warning is not valid.
!  IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN
!    IF(ElecReformEIRChiller(EIRChillNum)%PrintMessage)THEN
!          ElecReformEIRChiller(EIRChillNum)%MsgErrorCount = &
!                         ElecReformEIRChiller(EIRChillNum)%MsgErrorCount + 1
!!     Show single warning and pass additional info to ShowRecurringWarningErrorAtEnd
!      IF (ElecReformEIRChiller(EIRChillNum)%MsgErrorCount < 2) THEN
!         CALL ShowWarningError(TRIM(ElecReformEIRChiller(EIRChillNum)%MsgBuffer1)//'.')
!         CALL ShowContinueError(TRIM(ElecReformEIRChiller(EIRChillNum)%MsgBuffer2))
!      ELSE
!        CALL ShowRecurringWarningErrorAtEnd(TRIM(ElecReformEIRChiller(EIRChillNum)%MsgBuffer1)//' error continues.', &
!           ElecReformEIRChiller(EIRChillNum)%ErrCount1,ReportMaxOf=ElecReformEIRChiller(EIRChillNum)%MsgDataLast,  &
!           ReportMinOf=ElecReformEIRChiller(EIRChillNum)%MsgDataLast,ReportMaxUnits='[C]',ReportMinUnits='[C]')
!      END IF
!    END IF
!  END IF
!
!! save last system time step and last end time of current time step (used to determine if warning is valid)
!  TimeStepSysLast    = TimeStepSys
!  CurrentEndTimeLast = CurrentEndTime

   ! If no loop demand or chiller OFF, return
   ! If chiller load is 0 or chiller is not running then leave the subroutine. Before leaving
   !  if the component control is SERIESACTIVE we set the component flow to inlet flow so that
   !  flow resolver will not shut down the branch
   IF(MyLoad >= 0 .OR. .NOT. RunFlag) THEN
    IF(EquipFlowCtrl == ControlType_SeriesActive .OR. PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%FlowLock==1) THEN
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowrate
    END IF
    IF (ElecReformEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
      IF (PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)% &
            LoopSide(ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum)% &
              Branch(ElecReformEIRChiller(EIRChillNum)%CDBranchNum)%  &
                Comp(ElecReformEIRChiller(EIRChillNum)%CDCompNum)%FlowCtrl == ControlType_SeriesActive) THEN
        CondMassFlowRate = Node(CondInletNode)%MassFlowrate
      ENDIF
    ENDIF


    RETURN
   END IF

  ! LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  MinPartLoadRat      = ElecReformEIRChiller(EIRChillNum)%MinPartLoadRat
  MaxPartLoadRat      = ElecReformEIRChiller(EIRChillNum)%MaxPartLoadRat
  MinUnloadRat        = ElecReformEIRChiller(EIRChillNum)%MinUnLoadRat
  ChillerRefCap       = ElecReformEIRChiller(EIRChillNum)%RefCap
  ReferenceCOP        = ElecReformEIRChiller(EIRChillNum)%RefCOP
  EvapOutletTemp      = Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%Temp
  TempLowLimitEout    = ElecReformEIRChiller(EIRChillNum)%TempLowLimitEvapOut
  EvapMassFlowRateMax = ElecReformEIRChiller(EIRChillNum)%EvapMassFlowRateMax

  ! Set mass flow rates

  IF (ElecReformEIRChiller(EIRChillNum)%CondenserType == WaterCooled) THEN
    CondMassFlowRate = ElecReformEIRChiller(EIRChillNum)%CondMassFlowRateMax
    CALL SetComponentFlowRate(CondMassFlowRate, CondInletNode, CondOutletNode, &
                              ElecReformEIRChiller(EIRChillNum)%CDLoopNum, &
                              ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum, &
                              ElecReformEIRChiller(EIRChillNum)%CDBranchNum, &
                              ElecReformEIRChiller(EIRChillNum)%CDCompNum)
    CALL PullCompInterconnectTrigger(ElecReformEIRChiller(EIRChillNum)%CWLoopNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CWBranchNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CWCompNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CondMassFlowIndex,              &
                                     ElecReformEIRChiller(EIRChillNum)%CDLoopNum, &
                                     ElecReformEIRChiller(EIRChillNum)%CDLoopSideNum,   &
                                     CriteriaType_MassFlowRate, &
                                     CondMassFlowRate)

    IF (CondMassFlowRate < MassFlowTolerance) RETURN

  END IF
  FRAC    = 1.0d0

  SELECT CASE (PlantLoop(PlantLoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    IF ((ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
        (Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint /= SensedNodeFlagValue) ) THEN
         ! there will be a valid setpoint on outlet
      EvapOutletTempSetpoint = Node(EvapOutletNode)%TempSetPoint
    ELSE ! use plant loop overall setpoint
      EvapOutletTempSetpoint= Node(PlantLoop(PlantLoopNum)%TempSetPointNodeNum)%TempSetPoint
    ENDIF
  CASE (DualSetpointDeadband)
    IF ((ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
        (Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
         ! there will be a valid setpoint on outlet
      EvapOutletTempSetpoint = Node(EvapOutletNode)%TempSetPointHi
    ELSE ! use plant loop overall setpoint
      EvapOutletTempSetpoint= Node(PlantLoop(PlantLoopNum)%TempSetPointNodeNum)%TempSetPointHi
    ENDIF
  END SELECT

  ! correct temperature if using heat recovery
  ! use report values for latest valid calculation, lagged somewhat
  IF ( ElecReformEIRChiller(EIRChillNum)%HeatRecActive ) THEN
    If ( (ElecReformEIRChillerReport(EIRChillNum)%QHeatRecovery &
           +  ElecReformEIRChillerReport(EIRChillNum)%QCond) > 0.d0) THEN ! protect div by zero
      AvgCondSinkTemp = (ElecReformEIRChillerReport(EIRChillNum)%QHeatRecovery &
                            * ElecReformEIRChillerReport(EIRChillNum)%HeatRecOutletTemp &
                          + ElecReformEIRChillerReport(EIRChillNum)%QCond  &
                            * ElecReformEIRChillerReport(EIRChillNum)%CondOutletTemp) &
                          / (ElecReformEIRChillerReport(EIRChillNum)%QHeatRecovery   &
                                 + ElecReformEIRChillerReport(EIRChillNum)%QCond)
    ELSE
      AvgCondSinkTemp = FalsiCondOutTemp
    ENDIF
  ELSE
    AvgCondSinkTemp = FalsiCondOutTemp
  ENDIF

  ! Get capacity curve info with respect to CW setpoint and leaving condenser water temps
  ChillerCapFT = MAX(0.0d0, CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerCapFT, &
                            EvapOutletTempSetpoint,AvgCondSinkTemp))

  ! Available chiller capacity as a function of temperature
  AvailChillerCap = ChillerRefCap*ChillerCapFT

!  IF (PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0) THEN
!    EvapMassFlowRate = MIN(EvapMassFlowRateMax,Node(EvapInletNode)%MassFlowRateMaxAvail)    !CRBranchPump
!    EvapMassFlowRate = MAX(EvapMassFlowRate,Node(EvapInletNode)%MassFlowRateMinAvail)       !CRBranchPump
!!   Some other component set the flow to 0. No reason to continue with calculations.
!    IF(EvapMassFlowRate == 0.0d0)THEN
!      MyLoad = 0.0d0
!!      ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
!      RETURN
!    END IF
!  ELSE
  EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
!   Some other component set the flow to 0. No reason to continue with calculations.
  IF(EvapMassFlowRate == 0.0d0)THEN
    MyLoad = 0.0d0
!      ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
    RETURN
  END IF
!  END IF

! This chiller is currenlty has only a water-cooled condenser


  ! Calculate water side load
  Cp  = GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidName,  &
                              Node(EvapInletNode)%Temp,                      &
                              PlantLoop(ElecReformEIRChiller(EIRChillNum)%CWLoopNum)%FluidIndex, &
                              'CalcElecReformEIRChillerModel')
! problem here if no setpoint on outlet
! CR 9132 changed from actual node flow rate to maximum available to avoid issue of limiting capacity

  TempLoad = Node(EvapInletNode)%MassFlowRateMaxAvail * Cp * (Node(EvapInletNode)%Temp - EvapOutletTempSetpoint)

  TempLoad = MAX(0.0d0,TempLoad)

  ! MyLoad is capped at minimum PLR * RefCap, adjust load to actual water side load because this chiller can cycle
  IF (ABS(MyLoad) .GT. TempLoad) THEN
    MyLoad = SIGN(TempLoad, MyLoad)
  END IF

  ! Part load ratio based on load and available chiller capacity, cap at max part load ratio
  IF(AvailChillerCap .GT. 0)THEN
    PartLoadRat = MAX(0.0d0, MIN(ABS(MyLoad)/AvailChillerCap,MaxPartLoadRat))
  ELSE
    PartLoadRat = 0.0d0
  END IF

  ! Set evaporator heat transfer rate
  QEvaporator = AvailChillerCap * PartLoadRat
  ChillerPartLoadRatio = PartLoadRat
  ! If FlowLock is False (0), the chiller sets the plant loop mdot
  ! If FlowLock is True (1),  the new resolved plant loop mdot is used
  IF (PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock==0) THEN
    IF (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType ==   &
           CompSetPtBasedSchemeType) THEN
      ElecReformEIRChiller(EIRChillNum)%PossibleSubCooling = .FALSE.
    ELSE
      ElecReformEIRChiller(EIRChillNum)%PossibleSubCooling = .TRUE.
    ENDIF
       ! Either set the flow to the Constant value or caluclate the flow for the variable volume case
       IF(     (ElecReformEIRChiller(EIRChillNum)%FlowMode == ConstantFlow)  &
          .OR. (ElecReformEIRChiller(EIRChillNum)%FlowMode == NotModulated )) THEN
          ! Set the evaporator mass flow rate to design
          ! Start by assuming max (design) flow
          EvapMassFlowRate = EvapMassFlowRateMax
          ! Use SetComponentFlowRate to decide actual flow
          Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopNum,     &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                              ElecReformEIRChiller(EIRChillNum)%CWBranchNum,   &
                              ElecReformEIRChiller(EIRChillNum)%CWCompNum)
          IF (EvapMassFlowRate /= 0.0D0) THEN
            EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
          ELSE
            EvapDeltaTemp = 0.0D0
          ENDIF
          EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
       ELSEIF (ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) THEN
          SELECT CASE (PlantLoop(PlantLoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetpoint)
            ! Calculate the Delta Temp from the inlet temp to the chiller outlet setpoint
            EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPoint
          CASE (DualSetpointDeadband)
            EvapDeltaTemp = Node(EvapInletNode)%Temp - Node(EvapOutletNode)%TempSetPointHi
          END SELECT

          IF (EvapDeltaTemp /= 0) THEN
            EvapMassFlowRate = MAX(0.0d0,(QEvaporator/Cp/EvapDeltaTemp))
            IF((EvapMassFlowRate - EvapMassFlowRateMax) .GT. MassFlowTolerance) &
                  ElecReformEIRChiller(EIRChillNum)%PossibleSubCooling = .TRUE.
            !Check to see if the Maximum is exceeded, if so set to maximum
            EvapMassFlowRate = MIN(EvapMassFlowRateMax, EvapMassFlowRate)
            ! Use SetComponentFlowRate to decide actual flow
            Call SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopNum,     &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                              ElecReformEIRChiller(EIRChillNum)%CWBranchNum,   &
                              ElecReformEIRChiller(EIRChillNum)%CWCompNum)
            ! Should we recalculate this with the corrected setpoint?
            SELECT CASE (PlantLoop(PlantLoopNum)%LoopDemandCalcScheme)
            CASE (SingleSetpoint)
              EvapOutletTemp = Node(EvapOutletNode)%TempSetPoint
            CASE (DualSetpointDeadband)
              EvapOutletTemp = Node(EvapOutletNode)%TempSetPointHi
            END SELECT
            QEvaporator = MAX(0.0d0,(EvapMassFlowRate*Cp*EvapDeltaTemp))
          ELSE
            ! Try to request zero flow
            EvapMassFlowRate=0.0d0
            ! Use SetComponentFlowRate to decide actual flow
            CALL SetComponentFlowRate( EvapMassFlowRate,  &
                              EvapInletNode , EvapOutletNode  , &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopNum,     &
                              ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                              ElecReformEIRChiller(EIRChillNum)%CWBranchNum,   &
                              ElecReformEIRChiller(EIRChillNum)%CWCompNum)
            ! No deltaT since component is not running
            EvapOutletTemp = Node(EvapInletNode)%Temp
            QEvaporator = 0.0d0
            PartLoadRat = 0.0d0
            ChillerPartLoadRatio = PartLoadRat

            IF (ElecReformEIRChiller(EIRChillNum)%DeltaTErrCount < 1 .AND. .NOT. WarmupFlag) THEN
              ElecReformEIRChiller(EIRChillNum)%DeltaTErrCount=ElecReformEIRChiller(EIRChillNum)%DeltaTErrCount+1
              CALL ShowWarningError('Evaporator DeltaTemp = 0 in mass flow calculation (Tevapin = Tevapout setpoint temp).')
              CALL ShowContinueErrorTimeStamp(' ')
            ELSE IF (.NOT. WarmupFlag) THEN
              ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError = ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError + 1
              CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "' &
                                        //TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":'//&
                  ' Evaporator DeltaTemp = 0 in mass flow calculation warning continues...' &
                  , ElecReformEIRChiller(EIRChillNum)%DeltaTErrCountIndex, EvapDeltaTemp, EvapDeltaTemp)
            END IF

          END IF
       END IF  !End of Constant Variable Flow If Block

  ELSE  ! If FlowLock is True
      EvapMassFlowRate = Node(EvapInletNode)%MassFlowRate
      Call SetComponentFlowRate( EvapMassFlowRate,  &
                                 EvapInletNode , EvapOutletNode  , &
                                 ElecReformEIRChiller(EIRChillNum)%CWLoopNum,     &
                                 ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum, &
                                 ElecReformEIRChiller(EIRChillNum)%CWBranchNum,   &
                                 ElecReformEIRChiller(EIRChillNum)%CWCompNum)
!       Some other component set the flow to 0. No reason to continue with calculations.
      IF(EvapMassFlowRate == 0.0d0)THEN
        MyLoad = 0.0d0
!        ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
        RETURN
      END IF
      IF(ElecReformEIRChiller(EIRChillNum)%PossibleSubCooling) THEN
       QEvaporator = ABS(MyLoad)
       EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
       EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
      ELSE
       EvapDeltaTemp = Node(EvapInletNode)%Temp - EvapOutletTempSetpoint
       QEvaporator = MAX(0.0d0,(EvapMassFlowRate*Cp*EvapDeltaTemp))
       EvapOutletTemp = EvapOutletTempSetpoint
      END IF
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
      IF(QEvaporator > ABS(MyLoad)) THEN
        IF(EvapMassFlowRate > MassFlowTolerance) THEN
            QEvaporator = ABS(MyLoad)
            EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
            EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
        ELSE
            QEvaporator = 0.0d0
            EvapOutletTemp = Node(EvapInletNode)%Temp
        END IF
      END IF

      ! Checks QEvaporator on the basis of the machine limits.
      IF(QEvaporator > (AvailChillerCap * MaxPartLoadRat))THEN
        IF(EvapMassFlowRate > MassFlowTolerance) THEN
           QEvaporator = AvailChillerCap * MaxPartLoadRat
           EvapDeltaTemp = QEvaporator/EvapMassFlowRate/Cp
           ! evaporator outlet temperature is allowed to float upwards (recalculate AvailChillerCap? iterate?)
           EvapOutletTemp = Node(EvapInletNode)%Temp - EvapDeltaTemp
        ELSE
           QEvaporator = 0.0d0
           EvapOutletTemp = Node(EvapInletNode)%Temp
!           ElecReformEIRChiller(EIRChillNum)%PrintMessage = .FALSE.
        END IF

      END IF

      IF(AvailChillerCap .GT. 0.0d0)THEN
        PartLoadRat = MAX(0.0d0,MIN((QEvaporator/AvailChillerCap),MaxPartLoadRat))
      ELSE
        PartLoadRat = 0.0d0
      END IF

      ! Chiller cycles below minimum part load ratio, FRAC = amount of time chiller is ON during this time step
      IF (PartLoadRat .LT. MinPartLoadRat) FRAC = MIN(1.0d0,(PartLoadRat/MinPartLoadRat))

      ! set the module level variable used for reporting FRAC
      ChillerCyclingRatio = FRAC

      ! Chiller is false loading below PLR = minimum unloading ratio, find PLR used for energy calculation
      IF(AvailChillerCap .GT. 0.0d0)THEN
        PartLoadRat = MAX(PartLoadRat,MinUnLoadRat)
      ELSE
        PartLoadRat = 0.0d0
      END IF

       ! set the module level variable used for reporting PLR
      ChillerPartLoadRatio = PartLoadRat

      ! calculate the load due to false loading on chiller over and above water side load
      ChillerFalseLoadRate = (AvailChillerCap * PartLoadRat * FRAC) - QEvaporator
      IF(ChillerFalseLoadRate .LT. SmallLoad) THEN
       ChillerFalseLoadRate = 0.0d0
      END IF

  END IF  !This is the end of the FlowLock Block

  ChillerEIRFT   = MAX(0.0d0, CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT,EvapOutletTemp,AvgCondSinkTemp))

  ChillerEIRFPLR  = MAX(0.0d0, CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR,AvgCondSinkTemp,PartLoadRat))

  Power = (AvailChillerCap/ReferenceCOP) * ChillerEIRFPLR * ChillerEIRFT * FRAC

  QCondenser = Power*ElecReformEIRChiller(EIRChillNum)%CompPowerToCondenserFrac + QEvaporator + ChillerFalseLoadRate

!  Currently only water cooled chillers are allowed for the reformulated EIR chiller model
  IF (CondMassFlowRate > MassFlowTolerance) THEN
    ! If Heat Recovery specified for this vapor compression chiller, then Qcondenser will be adjusted by this subroutine
    IF(ElecReformEIRChiller(EIRChillNum)%HeatRecActive) CALL ReformEIRChillerHeatRecovery(EIRChillNum,QCondenser, &
                                                             CondMassFlowRate,CondInletTemp,QHeatRecovered)
    Cp  = GetSpecificHeatGlycol(PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidName,  &
                                CondInletTemp,                      &
                                PlantLoop(ElecReformEIRChiller(EIRChillNum)%CDLoopNum)%FluidIndex, &
                                'CalcElecReformEIRChillerModel')
    CondOutletTemp = QCondenser/CondMassFlowRate/Cp + CondInletTemp
  ELSE
    CALL ShowSevereError('ControlReformEIRChillerModel: Condenser flow = 0, for ElecReformEIRChiller='//  &
                         TRIM(ElecReformEIRChiller(EIRChillNum)%Name))
    CALL ShowContinueErrorTimeStamp(' ')

  END IF

  RETURN

END SUBROUTINE CalcReformEIRChillerModel

SUBROUTINE CheckMinMaxCurveBoundaries(EIRChillNum, FirstIteration)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          R Raustad, FSEC
            !       DATE WRITTEN:    August 2006

            ! PURPOSE OF THIS SUBROUTINE:
            !  To compare the evaporator/condenser outlet temperatures to curve object min/max values

            ! METHODOLOGY EMPLOYED:
            !  na

            ! REFERENCES:
            !  na

            ! USE STATEMENTS:
  USE General,         ONLY : TrimSigDigits, RoundSigDigits
  USE DataGlobals,     ONLY : WarmupFlag
  USE DataInterfaces,  ONLY : ShowWarningError, ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd, &
                              ShowContinueError
  USE CurveManager,    ONLY : CurveValue
  USE DataPlant,       ONLY : PlantLoop, CompSetPtBasedSchemeType, SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: EIRChillNum     ! Number of the current electric EIR chiller being simulated
  LOGICAL, INTENT(IN)   :: FirstIteration  ! TRUE when first iteration of timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: EvapOutletNode        ! Chiller evaporator outlet node number
  REAL(r64)  :: EvapOutletTempSetpoint ! Evaporator outlet temperature setpoint [C]
  REAL(r64)  :: CAPFTXTmin            ! Minimum evaporator leaving temperature allowed by CAPFT curve [C]
  REAL(r64)  :: CAPFTXTmax            ! Maximum evaporator leaving temperature allowed by CAPFT curve [C]
  REAL(r64)  :: CAPFTYTmin            ! Minimum condenser  leaving temperature allowed by CAPFT curve [C]
  REAL(r64)  :: CAPFTYTmax            ! Maximum condenser  leaving temperature allowed by CAPFT curve [C]
  REAL(r64)  :: EIRFTXTmin            ! Minimum evaporator leaving temperature allowed by EIRFT curve [C]
  REAL(r64)  :: EIRFTXTmax            ! Maximum evaporator leaving temperature allowed by EIRFT curve [C]
  REAL(r64)  :: EIRFTYTmin            ! Minimum condenser  leaving temperature allowed by EIRFT curve [C]
  REAL(r64)  :: EIRFTYTmax            ! Maximum condenser  leaving temperature allowed by EIRFT curve [C]
  REAL(r64)  :: EIRFPLRTmin           ! Minimum condenser  leaving temperature allowed by EIRFPLR curve [C]
  REAL(r64)  :: EIRFPLRTmax           ! Maximum condenser  leaving temperature allowed by EIRFPLR curve [C]
  REAL(r64)  :: EIRFPLRPLRmin         ! Minimum PLR allowed by EIRFPLR curve
  REAL(r64)  :: EIRFPLRPLRmax         ! Maximum PLR allowed by EIRFPLR curve
  INTEGER    :: PlantLoopNum          ! Plant loop which contains the current chiller
  INTEGER    :: LoopSideNum           ! Plant loop side which contains the current chiller (usually supply side)
  INTEGER    :: BranchNum
  INTEGER    :: CompNum

! Do not print out warnings if chiller not operating or FirstIteration/WarmupFlag/FlowLock
  PlantLoopNum   = ElecReformEIRChiller(EIRChillNum)%CWLoopNum
  LoopSideNum    = ElecReformEIRChiller(EIRChillNum)%CWLoopSideNum
  BranchNum      = ElecReformEIRChiller(EIRChillNum)%CWBranchNum
  CompNum        = ElecReformEIRChiller(EIRChillNum)%CWCompNum

  IF (FirstIteration .OR. WarmupFlag .OR. PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock .EQ. 0) RETURN

  EvapOutletNode = ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum

! Move CAPFT and EIRFT min/max values for evaporator outlet temperature to local variables
  CAPFTXTmin = ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTXTempMin
  CAPFTXTmax = ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTXTempMax

  EIRFTXTmin = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTXTempMin
  EIRFTXTmax = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTXTempMax

! Check bounds for curves, lump min/max into same check since min/max values are reported in recurring warning messages
  IF(EvapOutletTemp .LT. CAPFTXTmin .OR. EvapOutletTemp .GT. CAPFTXTmax) THEN
    ElecReformEIRChiller(EIRChillNum)%CAPFTXIter = ElecReformEIRChiller(EIRChillNum)%CAPFTXIter + 1
    IF (ElecReformEIRChiller(EIRChillNum)%CAPFTXIter .EQ. 1) THEN
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
         '": The evaporator outlet temperature ('//TRIM(TrimSigDigits(EvapOutletTemp,2))//' C) is outside the range of ' &
         //'evaporator outlet temperatures (X var) given in Cooling Capacity Function of Temperature biquadratic curve = ' &
         //TRIM(ElecReformEIRChiller(EIRChillNum)%CAPFTName))
      CALL ShowContinueErrorTimeStamp('The range specified = '//TRIM(TrimSigDigits(CAPFTXTmin,2)) &
                                      //' C to '//TRIM(TrimSigDigits(CAPFTXTmax,2))//' C. ')
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The evap outlet temp range in Cooling Capacity Function of Temp curve error continues.', &
        ElecReformEIRChiller(EIRChillNum)%CAPFTXIterIndex, EvapOutletTemp, EvapOutletTemp)
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The evap outlet temp range in Cooling Capacity Function of Temp curve error continues.', &
        ElecReformEIRChiller(EIRChillNum)%CAPFTXIterIndex, EvapOutletTemp, EvapOutletTemp)
    END IF
  END IF

  IF(EvapOutletTemp .LT. EIRFTXTmin .OR. EvapOutletTemp .GT. EIRFTXTmax) THEN
    ElecReformEIRChiller(EIRChillNum)%EIRFTXIter = ElecReformEIRChiller(EIRChillNum)%EIRFTXIter + 1
    IF (ElecReformEIRChiller(EIRChillNum)%EIRFTXIter .EQ. 1) THEN
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
         '": The evaporator outlet temperature ('//TRIM(TrimSigDigits(EvapOutletTemp,2))//' C) is outside the range of ' &
         //'evaporator outlet temperatures (X var) given in Electric Input to Cooling Output Ratio Function of ' &
         //'Temperature biquadratic curve = '//TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFTName))
      CALL ShowContinueErrorTimeStamp('The range specified = '//TRIM(TrimSigDigits(EIRFTXTmin,2)) &
                                      //' C to '//TRIM(TrimSigDigits(EIRFTXTmax,2))//' C. ')
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The evap outlet temp range in Electric Input to Cooling Output Ratio Function of Temp curve error' &
        //' continues.',ElecReformEIRChiller(EIRChillNum)%EIRFTXIterIndex, EvapOutletTemp, EvapOutletTemp)
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The evap outlet temp range in Electric Input to Cooling Output Ratio Function of Temp curve error' &
        //' continues.',ElecReformEIRChiller(EIRChillNum)%EIRFTXIterIndex, EvapOutletTemp, EvapOutletTemp)
    END IF
  END IF

! Move CAPFT, EIRFT, and EIRFPLR min/max condenser outlet temperature values to local variables
  CAPFTYTmin = ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMin
  CAPFTYTmax = ElecReformEIRChiller(EIRChillNum)%ChillerCAPFTYTempMax

  EIRFTYTmin = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMin
  EIRFTYTmax = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTYTempMax

  EIRFPLRTmin = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMin
  EIRFPLRTmax = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRTempMax

! Move EIRFPLR min/max part-load ratio values to local variables
  EIRFPLRPLRmin = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMin
  EIRFPLRPLRmax = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRPLRMax

! Check bounds for curves, lump min/max into same check since min/max values are reported in recurring warning messages
  IF(CondOutletTemp .LT. CAPFTYTmin .OR. CondOutletTemp .GT. CAPFTYTmax) THEN
    ElecReformEIRChiller(EIRChillNum)%CAPFTYIter = ElecReformEIRChiller(EIRChillNum)%CAPFTYIter + 1
    IF (ElecReformEIRChiller(EIRChillNum)%CAPFTYIter .EQ. 1) THEN
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
         '": The condenser outlet temperature ('//TRIM(TrimSigDigits(CondOutletTemp,2))//' C) is outside the range of ' &
         //'condenser outlet temperatures (Y var) given in Cooling Capacity Function of Temperature biquadratic curve = ' &
         //TRIM(ElecReformEIRChiller(EIRChillNum)%CAPFTName))
      CALL ShowContinueErrorTimeStamp('The range specified = '//TRIM(TrimSigDigits(CAPFTYTmin,2)) &
                                      //' C to '//TRIM(TrimSigDigits(CAPFTYTmax,2))//' C. ')
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The cond outlet temp range in Cooling Capacity Function of Temp curve error continues.', &
        ElecReformEIRChiller(EIRChillNum)%CAPFTYIterIndex, CondOutletTemp, CondOutletTemp)
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The cond outlet temp range in Cooling Capacity Function of Temp curve error continues.', &
        ElecReformEIRChiller(EIRChillNum)%CAPFTYIterIndex, CondOutletTemp, CondOutletTemp)
    END IF
  END IF

  IF(CondOutletTemp .LT. EIRFTYTmin .OR. CondOutletTemp .GT. EIRFTYTmax) THEN
    ElecReformEIRChiller(EIRChillNum)%EIRFTYIter = ElecReformEIRChiller(EIRChillNum)%EIRFTYIter + 1
    IF (ElecReformEIRChiller(EIRChillNum)%EIRFTYIter .EQ. 1) THEN
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
         '": The condenser outlet temperature ('//TRIM(TrimSigDigits(CondOutletTemp,2))//' C) is outside the range of ' &
         //'condenser outlet temperatures (Y var) given in Electric Input to Cooling Output Ratio Function of ' &
         //'Temperature biquadratic curve = '//TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFTName))
      CALL ShowContinueErrorTimeStamp('The range specified = '//TRIM(TrimSigDigits(EIRFTYTmin,2)) &
                                      //' C to '//TRIM(TrimSigDigits(EIRFTYTmax,2))//' C. ')
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The cond outlet temp range in Electric Input to Cooling Output Ratio as a Function of Temp ' &
        //'curve error continues.', ElecReformEIRChiller(EIRChillNum)%EIRFTYIterIndex, CondOutletTemp, CondOutletTemp)
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The cond outlet temp range in Electric Input to Cooling Output Ratio as a Function of Temp ' &
        //'curve error continues.', ElecReformEIRChiller(EIRChillNum)%EIRFTYIterIndex, CondOutletTemp, CondOutletTemp)
    END IF
  END IF

  IF(CondOutletTemp .LT. EIRFPLRTmin .OR. CondOutletTemp .GT. EIRFPLRTmax) THEN
    ElecReformEIRChiller(EIRChillNum)%EIRFPLRTIter = ElecReformEIRChiller(EIRChillNum)%EIRFPLRTIter + 1
    IF (ElecReformEIRChiller(EIRChillNum)%EIRFPLRTIter .EQ. 1) THEN
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
         '": The condenser outlet temperature ('//TRIM(TrimSigDigits(CondOutletTemp,2))//' C) is outside the range of ' &
         //'condenser outlet temperatures (X var) given in Electric Input to Cooling Output Ratio Function of Part-load Ratio ' &
         //'bicubic curve = '//TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFPLRName))
      CALL ShowContinueErrorTimeStamp('The range specified = '//TRIM(TrimSigDigits(EIRFPLRTmin,2)) &
                                      //' C to '//TRIM(TrimSigDigits(EIRFPLRTmax,2))//' C. ')
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The cond outlet temp range in Electric Input to Cooling Output Ratio Function of PLR ' &
        //'curve error continues.', ElecReformEIRChiller(EIRChillNum)%EIRFPLRTIterIndex, CondOutletTemp, CondOutletTemp)
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The cond outlet temp range in Electric Input to Cooling Output Ratio Function of PLR ' &
        //'curve error continues.', ElecReformEIRChiller(EIRChillNum)%EIRFPLRTIterIndex, CondOutletTemp, CondOutletTemp)
    END IF
  END IF

  IF(ChillerPartLoadRatio .LT. EIRFPLRPLRmin .OR. ChillerPartLoadRatio .GT. EIRFPLRPLRmax) THEN
    ElecReformEIRChiller(EIRChillNum)%EIRFPLRPLRIter = ElecReformEIRChiller(EIRChillNum)%EIRFPLRPLRIter + 1
    IF (ElecReformEIRChiller(EIRChillNum)%EIRFPLRPLRIter .EQ. 1) THEN
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name) //  &
         '": The part-load ratio ('//TRIM(TrimSigDigits(ChillerPartLoadRatio,3))//') is outside the range of ' &
         //'part-load ratios (Y var) given in Electric Input to Cooling Output Ratio Function of Part-load Ratio ' &
         //'bicubic curve = '//TRIM(ElecReformEIRChiller(EIRChillNum)%EIRFPLRName))
      CALL ShowContinueErrorTimeStamp('The range specified = '//TRIM(TrimSigDigits(EIRFPLRPLRmin,3)) &
                                      //' to '//TRIM(TrimSigDigits(EIRFPLRPLRmax,3))//'. ')
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The part-load ratio range in Electric Input to Cooling Output Ratio Function of PLRatio curve ' &
        //'error continues.', ElecReformEIRChiller(EIRChillNum)%EIRFPLRPLRIterIndex, ChillerPartLoadRatio, ChillerPartLoadRatio)
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)// &
        '": The part-load ratio range in Electric Input to Cooling Output Ratio Function of PLRatio curve ' &
        //'error continues.', ElecReformEIRChiller(EIRChillNum)%EIRFPLRPLRIterIndex, ChillerPartLoadRatio, ChillerPartLoadRatio)
    END IF
  END IF

  SELECT CASE (PlantLoop(PlantLoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    IF ((ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
        (Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPoint /= SensedNodeFlagValue) ) THEN
         ! there will be a valid setpoint on outlet
      EvapOutletTempSetpoint = Node(EvapOutletNode)%TempSetPoint
    ELSE ! use plant loop overall setpoint
      EvapOutletTempSetpoint= Node(PlantLoop(PlantLoopNum)%TempSetPointNodeNum)%TempSetPoint
    ENDIF
  CASE (DualSetpointDeadband)
    IF ((ElecReformEIRChiller(EIRChillNum)%FlowMode == LeavingSetpointModulated) .OR. &
        (PlantLoop(PlantLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType &
                 == CompSetPtBasedSchemeType)          .OR. &
        (Node(ElecReformEIRChiller(EIRChillNum)%EvapOutletNodeNum)%TempSetPointHi /= SensedNodeFlagValue) ) THEN
         ! there will be a valid setpoint on outlet
      EvapOutletTempSetpoint = Node(EvapOutletNode)%TempSetPointHi
    ELSE ! use plant loop overall setpoint
      EvapOutletTempSetpoint= Node(PlantLoop(PlantLoopNum)%TempSetPointNodeNum)%TempSetPointHi
    ENDIF
  END SELECT
  ChillerCapFT = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerCapFT, &
                            EvapOutletTempSetpoint,CondOutletTemp)

  IF(ChillerCapFT .LT. 0)THEN
    IF(ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError .LT. 1 .AND. PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError = ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError + 1
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":')
      CALL ShowContinueError(' Chiller Capacity as a Function of Temperature curve output is negative ('// &
                               TRIM(RoundSigDigits(ChillerCapFT,3))//').')
      CALL ShowContinueError(' Negative value occurs using an Evaporator Leaving Temp of ' &
                              //TRIM(RoundSigDigits(EvapOutletTempSetpoint,1))// &
                             ' and a Condenser Leaving Temp of '//TRIM(RoundSigDigits(CondOutletTemp,1))//'.')
      CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
    ELSE IF(PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError = ElecReformEIRChiller(EIRChillNum)%ChillerCapFTError + 1
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "' &
                                        //TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":'//&
          ' Chiller Capacity as a Function of Temperature curve output is negative warning continues...' &
          , ElecReformEIRChiller(EIRChillNum)%ChillerCapFTErrorIndex, ChillerCapFT, ChillerCapFT)
    END IF
  END IF

  ChillerEIRFT   = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFT,EvapOutletTemp,CondOutletTemp)

  IF(ChillerEIRFT .LT. 0.0d0)THEN
    IF(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTError .LT. 1 .AND. PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTError = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTError + 1
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":')
      CALL ShowContinueError(' Reformulated Chiller EIR as a Function of Temperature curve output is negative (' &
                           //TRIM(RoundSigDigits(ChillerEIRFT,3))//').')
      CALL ShowContinueError(' Negative value occurs using an Evaporator Leaving Temp of ' &
                            //TRIM(RoundSigDigits(EvapOutletTemp,1))// &
                           ' and a Condenser Leaving Temp of '//TRIM(RoundSigDigits(CondOutletTemp,1))//'.')
      CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
    ELSE IF (PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock .NE. 0 .AND. .NOT. WarmupFlag) THEN
      ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTError = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTError + 1
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "' &
                                        //TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":'//&
          ' Chiller EIR as a Function of Temperature curve output is negative warning continues...' &
          , ElecReformEIRChiller(EIRChillNum)%ChillerEIRFTErrorIndex, ChillerEIRFT, ChillerEIRFT)
    END IF
  END IF

  ChillerEIRFPLR  = CurveValue(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLR,CondOutletTemp,ChillerPartLoadRatio)
  IF(ChillerEIRFPLR .LT. 0.0d0)THEN
    IF(ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRError .LT. 1 .AND. PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock &
     .NE. 0 .AND. .NOT. WarmupFlag)THEN
      ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRError = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRError + 1
      CALL ShowWarningError('CHILLER:ELECTRIC:REFORMULATEDEIR "'//TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":')
      CALL ShowContinueError(' Chiller EIR as a function of PLR and condenser water temperature curve output is negative (' &
                           //TRIM(RoundSigDigits(ChillerEIRFPLR,3))//').')
      CALL ShowContinueError(' Negative value occurs using a part-load ratio of '//TRIM(RoundSigDigits(ChillerPartLoadRatio,3))// &
                           ' and a Condenser Leaving Temp of '//TRIM(RoundSigDigits(CondOutletTemp,1))//' C.')
      CALL ShowContinueErrorTimeStamp(' Resetting curve output to zero and continuing simulation.')
    ELSE IF (PlantLoop(PlantLoopNum)%Loopside(LoopSideNum)%FlowLock .NE. 0 .AND. .NOT. WarmupFlag) THEN
      ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRError = ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRError + 1
      CALL ShowRecurringWarningErrorAtEnd('CHILLER:ELECTRIC:REFORMULATEDEIR "' &
                                        //TRIM(ElecReformEIRChiller(EIRChillNum)%Name)//'":'//&
          ' Chiller EIR as a function of PLR curve output is negative warning continues...' &
          , ElecReformEIRChiller(EIRChillNum)%ChillerEIRFPLRErrorIndex, ChillerEIRFPLR, ChillerEIRFPLR)
    END IF
  END IF

  RETURN

END SUBROUTINE CheckMinMaxCurveBoundaries

END MODULE ChillerReformulatedEIR

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
