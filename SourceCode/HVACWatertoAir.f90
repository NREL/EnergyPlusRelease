! Two modules contained in this file:
! WatertoAirHeatPump
! WatertoAirHeatPumpSimple

MODULE WatertoAirHeatPump
  ! Module containing the Water to Air Heat Pump simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Hui Jin
  !       DATE WRITTEN   Oct 2000
  !       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
  !                      Brent Griffith, plant upgrades, fluid props
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the Water to Air Heat Pump Component

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:
  ! Jin, H. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd Thesis.
  ! Oklahoma State University.

  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals, ONLY: CycFanCycCoil, ContFanCycCoil, TimeStepSys
USE DataInterfaces
USE DataPlant, ONLY: TypeOf_CoilWAHPCoolingParamEst,  TypeOf_CoilWAHPHeatingParamEst, PlantLoop


  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: CompressorType_Reciprocating =1
INTEGER, PARAMETER :: CompressorType_Rotary        =2
INTEGER, PARAMETER :: CompressorType_Scroll        =3


  ! DERIVED TYPE DEFINITIONS
TYPE WatertoAirHPEquipConditions
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the Water to Air Heat pump
  CHARACTER(len=MaxNameLength) :: WatertoAirHPType     =' '  ! Type of WatertoAirHP ie. Heating or Cooling
  INTEGER                      :: WAHPPlantTypeOfNum   = 0   ! type of component in plant
  CHARACTER(len=MaxNameLength) :: Refrigerant          =' '  ! Refrigerant name
  LOGICAL                      :: Simflag              = .false.
  REAL(r64) :: InletAirMassFlowRate  =0.0d0 ! Inlet Air Mass Flow through the Water to Air Heat Pump being Simulated [kg/s]
  REAL(r64) :: OutletAirMassFlowRate =0.0d0 ! Outlet Air Mass Flow through the Water to Air Heat Pump being Simulated [kg/s]
  REAL(r64) :: InletAirDBTemp        =0.0d0 ! Inlet Air Dry Bulb Temperature [C]
  REAL(r64) :: InletAirHumRat        =0.0d0 ! Inlet Air Humidity Ratio [kg/kg]
  REAL(r64) :: OutletAirDBTemp       =0.0d0 ! Outlet Air Dry Bulb Temperature [C]
  REAL(r64) :: OutletAirHumRat       =0.0d0 ! Outlet Air Humidity Ratio [kg/kg]
  REAL(r64) :: InletAirEnthalpy      =0.0d0 ! Inlet Air Enthalpy [J/kg]
  REAL(r64) :: OutletAirEnthalpy     =0.0d0 ! Outlet Air Enthalpy [J/kg]

  REAL(r64) :: InletWaterTemp        =0.0d0 ! Inlet Water Temperature [C]
  REAL(r64) :: OutletWaterTemp       =0.0d0 ! Outlet Water Temperature [C]

  REAL(r64) :: InletWaterMassFlowRate=0.0d0 ! Inlet Water Mass Flow Rate [kg/s]
  REAL(r64) :: OutletWaterMassFlowRate=0.0d0 ! Outlet Water Mass Flow Rate [kg/s]
  REAL(r64) :: DesignWaterMassFlowRate=0.0d0  ! Design Water Mass Flow Rate [kg/s]
  REAL(r64) :: DesignWaterVolFlowRate =0.0d0  ! Design Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: InletWaterEnthalpy     =0.0d0  ! Inlet Water Enthalpy [J/kg]
  REAL(r64) :: OutletWaterEnthalpy    =0.0d0  ! Outlet Water Enthalpy [J/kg]

  REAL(r64) :: Power                  =0.0d0  ! Power Consumption [W]
  REAL(r64) :: Energy                 =0.0d0  ! Energy Consumption [J]
  REAL(r64) :: QSensible              =0.0d0 ! Sensible Load Side Heat Transfer Rate [W]
  REAL(r64) :: QLatent                =0.0d0 ! Latent Load Side Heat Transfer Rate [W]
  REAL(r64) :: QSource                =0.0d0 ! Source Side Heat Transfer Rate [W]
  REAL(r64) :: EnergySensible = 0.0d0        ! Sensible Load Side Heat Transferred [J]
  REAL(r64) :: EnergyLatent =0.0d0          ! Latent Load Side Heat Transferred [J]
  REAL(r64) :: EnergySource =0.0d0          ! Source Side Heat Transferred [J]
  REAL(r64) :: RunFrac           =0.0d0 ! Duty Factor
  REAL(r64) :: PartLoadRatio     =0.0d0 ! Part Load Ratio
  REAL(r64) :: HeatingCapacity   =0.0d0 ! Nominal Heating Capacity
  REAL(r64) :: CoolingCapacity   =0.0d0 ! Nominal Cooling Capacity
  REAL(r64) :: QLoadTotal        =0.0d0 ! Load Side Total Heat Transfer Rate [W]
  REAL(r64) :: EnergyLoadTotal =0.0d0       ! Load Side Total Heat Transferred [J]
  REAL(r64) :: Twet_Rated        =0.0d0 ! Nominal Time for Condensate Removal to Begin [s]
  REAL(r64) :: Gamma_Rated       =0.0d0 ! Ratio of Initial Moisture Evaporation Rate and Steady-state Latent Capacity
  REAL(r64) :: MaxONOFFCyclesperHour =0.0d0 ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64) :: HPTimeConstant        =0.0d0 ! Heat pump time constant [s]
  REAL(r64) :: FanDelayTime          =0.0d0 ! Fan delay time, time delay for the HP's fan to
                                             ! shut off after compressor cycle off [s]

  REAL(r64) :: SourceSideUACoeff     =0.0d0 ! Source Side Heat Transfer coefficient [W/C]
  REAL(r64) :: LoadSideTotalUACoeff  =0.0d0 ! Load Side Total Heat Transfer coefficient [W/C]
  REAL(r64) :: LoadSideOutsideUACoeff=0.0d0     ! Load Side Outside Heat Transfer coefficient [W/C]
  REAL(r64) :: CompPistonDisp        =0.0d0     ! Compressor Piston Displacement [m3/s]
  REAL(r64) :: CompClearanceFactor   =0.0d0     ! Compressor Clearance Factor
  REAL(r64) :: CompSucPressDrop      =0.0d0 ! Suction Pressure Drop [Pa]
  REAL(r64) :: SuperheatTemp         =0.0d0 ! Superheat Temperature [C]
  REAL(r64) :: PowerLosses           =0.0d0 ! Constant Part of the Compressor Power Losses [W]
  REAL(r64) :: LossFactor        =0.0d0 ! Compressor Power Loss Factor
  REAL(r64) :: RefVolFlowRate    =0.0d0 ! Refrigerant Volume Flow rate at the beginning
                                    ! of the Compression [m3/s]
  REAL(r64) :: VolumeRatio       =0.0d0 ! Built-in-volume ratio [~]
  REAL(r64) :: LeakRateCoeff     =0.0d0 ! Coefficient for the relationship between
                                    ! Pressure Ratio and Leakage Rate [~]
  REAL(r64) :: SourceSideHTR1    =0.0d0 ! Source Side Heat Transfer Resistance coefficient 1 [~]
  REAL(r64) :: SourceSideHTR2    =0.0d0 ! Source Side Heat Transfer Resistance coefficient 2 [k/kW]
  REAL(r64) :: HighPressCutOff   =0.0d0 ! High Pressure Cut-off [Pa]
  REAL(r64) :: LowPressCutOff    =0.0d0 ! Low Pressure Cut-off [Pa]
  INTEGER   :: CompressorType    =0   ! Type of Compressor ie. Reciprocating,Rotary or Scroll
  INTEGER   :: AirInletNodeNum   =0   ! air side coil inlet node number
  INTEGER   :: AirOutletNodeNum  =0   ! air side coil outlet node number
  INTEGER   :: WaterInletNodeNum =0   ! water side coil inlet node number
  INTEGER   :: WaterOutletNodeNum=0   ! water side coil outlet node number
  INTEGER   :: LowPressClgError  =0   ! count for low pressure errors (cooling)
  INTEGER   :: HighPressClgError =0   ! count for high pressure errors (cooling)
  INTEGER   :: LowPressHtgError  =0   ! count for low pressure errors (heating)
  INTEGER   :: HighPressHtgError =0   ! count for high pressure errors (heating)

  INTEGER   :: LoopNum           =0    ! plant loop index for water side
  INTEGER   :: LoopSide          =0    ! plant loop side index
  INTEGER   :: BranchNum         =0    ! plant branch index
  INTEGER   :: CompNum           =0    ! plant component index

END TYPE WatertoAirHPEquipConditions


! Output Variables Type definition

  !MODULE VARIABLE DECLARATIONS:
INTEGER :: NumWatertoAirHPs  =0 ! The Number of Water to Air Heat Pumps found in the Input
TYPE (WatertoAirHPEquipConditions), ALLOCATABLE, DIMENSION(:) :: WatertoAirHP
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


INTEGER            :: RefrigIndex = 0   ! Refrigerant index
INTEGER            :: WaterIndex = 0    ! Water index
LOGICAL :: GetCoilsInputFlag = .True. ! Flag set to make sure you get input once
! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  SimWatertoAirHP

          ! Get Input routines for module
PRIVATE GetWatertoAirHPInput

          ! Initialization routines for module
PRIVATE InitWatertoAirHP

          ! Computational routines
PRIVATE CalcWatertoAirHPCooling
PRIVATE CalcWatertoAirHPHeating

          ! Update routine to check convergence and update nodes
PRIVATE UpdateWatertoAirHP

          ! Utility routines
PUBLIC  GetCoilIndex
PUBLIC  GetCoilCapacity
PUBLIC  GetCoilInletNode
PUBLIC  GetCoilOutletNode

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************


SUBROUTINE SimWatertoAirHP(CompName,CompIndex,DesignAirflow,CyclingScheme, &
           FirstHVACIteration,RuntimeFrac,MaxONOFFCyclesperHour,HPTimeConstant, &
           FanDelayTime,InitFlag,SensLoad,LatentLoad,CompOp,PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hui Jin
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Water to Air Heat Pump component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits
  USE FluidProperties, ONLY: FindGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  REAL(r64), INTENT (IN):: DesignAirflow         !design air flow rate
  REAL(r64), INTENT (IN):: RuntimeFrac           !compressor run time fraction

  REAL(r64), INTENT (INOUT) :: MaxONOFFCyclesperHour !Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64), INTENT (INOUT) :: HPTimeConstant        !Heat pump time constant [s]
  REAL(r64), INTENT (INOUT) :: FanDelayTime          !Fan delay time, time delay for the HP's fan to
                                                !shut off after compressor cycle off  [s]
  REAL(r64), INTENT (IN):: SensLoad              !sensible load
  REAL(r64), INTENT (IN):: LatentLoad            !latent load
  LOGICAL, INTENT (IN):: FirstHVACIteration !first iteration flag
  LOGICAL, INTENT (IN):: Initflag      !initialization flag used to suppress property routine errors
  CHARACTER(len=*), INTENT(IN) :: CompName  !component name
  INTEGER, INTENT(INOUT)       :: CompIndex ! Index for Component name
  INTEGER, INTENT(IN) :: CyclingScheme      !cycling scheme--either continuous fan/cycling compressor or
                                            !cycling fan/cycling compressor
  INTEGER, INTENT(IN) :: CompOp
  REAL(r64), INTENT(IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: HPNum     ! The WatertoAirHP that you are currently loading input into

          ! FLOW:

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    CALL GetWatertoAirHPInput
    GetCoilsInputFlag=.false.
  End If


 IF (CompIndex == 0) THEN
    HPNum = FindItemInList(CompName,WatertoAirHP%Name,NumWatertoAirHPs)
    IF (HPNum == 0) THEN
      CALL ShowFatalError('WaterToAir HP not found='//TRIM(CompName))
    ENDIF
    CompIndex=HPNum
  ELSE
    HPNum=CompIndex
    IF (HPNum > NumWatertoAirHPs .or. HPNum < 1) THEN
      CALL ShowFatalError('SimWatertoAirHP: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HPNum))// &
                          ', Number of Water to Air HPs='//TRIM(TrimSigDigits(NumWatertoAirHPs))//  &
                          ', WaterToAir HP name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(HPNum)) THEN
      IF (CompName /= Blank .AND. CompName /= WatertoAirHP(HPNum)%Name) THEN
        CALL ShowFatalError('SimWatertoAirHP: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(HPNum))// &
                            ', WaterToAir HP name='//TRIM(CompName)//', stored WaterToAir HP Name for that index='//  &
                            TRIM(WatertoAirHP(HPNum)%Name))
      ENDIF
      CheckEquipName(HPNum)=.false.
    ENDIF
  ENDIF
  ! Calculate the Correct Water to Air HP Model with the current HPNum

  IF(WatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPCoolingParamEst)THEN
    CALL InitWatertoAirHP(HPNum, InitFlag,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime, &
                          SensLoad,LatentLoad,DesignAirflow,PartLoadRatio)
    CALL CalcWatertoAirHPCooling(HPNum,CyclingScheme,FirstHVACIteration,RuntimeFrac,initflag,SensLoad,CompOp,PartLoadRatio)

    CALL UpdateWatertoAirHP(HPNum)

  ELSEIF(WatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPHeatingParamEst)THEN
    CALL InitWatertoAirHP(HPNum, InitFlag,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime, &
                          SensLoad,LatentLoad,DesignAirflow,PartLoadRatio)
    CALL CalcWatertoAirHPHeating(HPNum,CyclingScheme,FirstHVACIteration,RuntimeFrac,initflag,SensLoad,CompOp,PartLoadRatio)

    CALL UpdateWatertoAirHP(HPNum)

  ELSE
    CALL ShowFatalError ('SimWatertoAirHP: AirtoAir heatpump not in either HEATING or COOLING')
  ENDIF


  RETURN

END SUBROUTINE SimWatertoAirHP


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetWatertoAirHPInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hui Jin
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for HPs and stores it in HP data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE NodeInputManager
  USE BranchNodeConnections, ONLY: TestCompSet
  USE FluidProperties,    ONLY : CheckFluidPropertyName, FindGlycol
  USE GlobalNames, ONLY: VerifyUniqueCoilName
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER (len=*), PARAMETER   :: RoutineName='GetWatertoAirHPInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HPNum      ! The Water to Air HP that you are currently loading input into
  INTEGER :: NumCool
  INTEGER :: NumHeat
  INTEGER :: WatertoAirHPNum
  INTEGER :: NumFluids
  INTEGER :: NumAlphas
  INTEGER :: NumParams
  INTEGER :: NumNums
  INTEGER :: MaxNums=0     ! Maximum number of numeric input fields
  INTEGER :: MaxAlphas=0   ! Maximum number of alpha input fields
  INTEGER :: IOSTAT
  LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
  LOGICAL :: IsNotOK                 ! Flag to verify name
  LOGICAL :: IsBlank                 ! Flag for blank name
  LOGICAL :: errflag
  CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.


          ! FLOW

    NumCool   = GetNumObjectsFound('Coil:Cooling:WaterToAirHeatPump:ParameterEstimation')
    NumHeat   = GetNumObjectsFound('Coil:Heating:WaterToAirHeatPump:ParameterEstimation')
    NumWatertoAirHPs = NumCool+NumHeat
    HPNum= 0

    IF(NumWatertoAirHPs <= 0) THEN
      CALL ShowSevereError('No Equipment found in SimWatertoAirHP')
      ErrorsFound=.true.
    END IF

   ! Allocate Arrays
    IF (NumWatertoAirHPs.GT.0) THEN
      ALLOCATE(WatertoAirHP(NumWatertoAirHPs))
      ALLOCATE(CheckEquipName(NumWatertoAirHPs))
      CheckEquipName=.true.
    ENDIF

    CALL GetObjectDefMaxArgs('Coil:Cooling:WaterToAirHeatPump:ParameterEstimation',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('Coil:Heating:WaterToAirHeatPump:ParameterEstimation',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    ALLOCATE(AlphArray(MaxAlphas))
    AlphArray=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.TRUE.
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0d0

      ! Get the data for detailed cooling Heat Pump
    CurrentModuleObject = 'Coil:Cooling:WaterToAirHeatPump:ParameterEstimation'

    DO WatertoAirHPNum = 1, NumCool

        HPNum= HPNum + 1

        CALL GetObjectItem(CurrentModuleObject,HPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT,&
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.

        CALL VerifyName(AlphArray(1),WatertoAirHP%Name,HPNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        WatertoAirHP(HPNum)%Name     = TRIM(AlphArray(1))
        WatertoAirHP(HPNum)%WatertoAirHPType  = 'COOLING'
        WatertoAirHP(HPNum)%WAHPPlantTypeOfNum = TypeOf_CoilWAHPCoolingParamEst
        WatertoAirHP(HPNum)%Refrigerant = TRIM(AlphArray(3))
        WatertoAirHP(HPNum)%DesignWaterVolFlowRate = NumArray(1)
        WatertoAirHP(HPNum)%CoolingCapacity = NumArray(2)
        WatertoAirHP(HPNum)%Twet_Rated=NumArray(3)
        WatertoAirHP(HPNum)%Gamma_Rated=NumArray(4)


        WatertoAirHP(HPNum)%HighPressCutOff=NumArray(5)
        WatertoAirHP(HPNum)%LowPressCutOff=NumArray(6)


        WatertoAirHP(HPNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WatertoAirHP(HPNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WatertoAirHP(HPNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                             NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WatertoAirHP(HPNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(7),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)


        !2010-01-13 ESL: Jason Glazer noted that these were out of order previously, but they are good now
        WatertoAirHP(HPNum)%LoadSideTotalUACoeff=NumArray(7)
        WatertoAirHP(HPNum)%LoadSideOutsideUACoeff=NumArray(8)

        IF ((WatertoAirHP(HPNum)%LoadSideOutsideUACoeff .LT. rTinyValue) .OR. &
            (WatertoAirHP(HPNum)%LoadSideTotalUACoeff   .LT. rTinyValue)) THEN
          CALL ShowSevereError('Input problem for '//TRIM(CurrentModuleObject)//'='//TRIM(WatertoAirHP(HPNum)%Name))
          CALL ShowContinueError(' One or both load side UA values entered are below tolerance, likely zero or blank.')
          CALL ShowContinueError(' Verify inputs, as the parameter syntax for this object went through a change with')
          CALL ShowContinueError('  the release of EnergyPlus version 5.')
          ErrorsFound = .TRUE.
        END IF

        WatertoAirHP(HPNum)%SuperheatTemp=NumArray(9)
        WatertoAirHP(HPNum)%PowerLosses=NumArray(10)
        WatertoAirHP(HPNum)%LossFactor=NumArray(11)

        SELECT CASE  (AlphArray(2))

          CASE ('RECIPROCATING')
            WaterToAirHP(HPNum)%CompressorType=CompressorType_Reciprocating
            WatertoAirHP(HPNum)%CompPistonDisp=NumArray(12)
            WatertoAirHP(HPNum)%CompSucPressDrop=NumArray(13)
            WatertoAirHP(HPNum)%CompClearanceFactor=NumArray(14)

          CASE ('ROTARY')
            WaterToAirHP(HPNum)%CompressorType=CompressorType_Rotary
            WatertoAirHP(HPNum)%CompPistonDisp=NumArray(12)
            WatertoAirHP(HPNum)%CompSucPressDrop=NumArray(13)

          CASE ('SCROLL')
            WaterToAirHP(HPNum)%CompressorType=CompressorType_Scroll
            WatertoAirHP(HPNum)%RefVolFlowRate=NumArray(15)
            WatertoAirHP(HPNum)%VolumeRatio=NumArray(16)
            WatertoAirHP(HPNum)%LeakRateCoeff=NumArray(17)

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(2))//' ('//  &
              TRIM(AlphArray(2))//') entered.'//TRIM(CurrentModuleObject)//'='//TRIM(WatertoAirHP(HPNum)%Name))
            ErrorsFound=.true.

        END SELECT

        WatertoAirHP(HPNum)%SourceSideUACoeff=NumArray(18)
        WatertoAirHP(HPNum)%SourceSideHTR1=NumArray(19)
        WatertoAirHP(HPNum)%SourceSideHTR2=NumArray(20)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(6),AlphArray(7),'Air Nodes')

        CALL SetupOutputVariable('Cooling Coil Electric Energy [J]', &
             WatertoAirHP(HPNum)%Energy,'System','Summed',WatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')

        CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]', &
             WatertoAirHP(HPNum)%EnergyLoadTotal,'System','Summed',WatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]', &
             WatertoAirHP(HPNum)%EnergySensible,'System','Summed',WatertoAirHP(HPNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Latent Cooling Energy [J]', &
             WatertoAirHP(HPNum)%EnergyLatent,'System','Summed',WatertoAirHP(HPNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Energy [J]', &
             WatertoAirHP(HPNum)%EnergySource,'System','Summed',WatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')

        ! save the design source side flow rate for use by plant loop sizing algorithms
        CALL RegisterPlantCompDesignFlow(WatertoAirHP(HPNum)%WaterInletNodeNum,0.5d0*WatertoAirHP(HPNum)%DesignWaterVolFlowRate)

        !create predefined report entries
        CALL PreDefTableEntry(pdchCoolCoilType,WatertoAirHP(HPNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchCoolCoilTotCap,WatertoAirHP(HPNum)%Name,WatertoAirHP(HPNum)%CoolingCapacity)
        CALL PreDefTableEntry(pdchCoolCoilSensCap,WatertoAirHP(HPNum)%Name,'-')
        CALL PreDefTableEntry(pdchCoolCoilLatCap,WatertoAirHP(HPNum)%Name,'-')
        CALL PreDefTableEntry(pdchCoolCoilSHR,WatertoAirHP(HPNum)%Name,'-')
        CALL PreDefTableEntry(pdchCoolCoilNomEff,WatertoAirHP(HPNum)%Name,'-')

   END DO


   CurrentModuleObject = 'Coil:Heating:WaterToAirHeatPump:ParameterEstimation'

   DO WatertoAirHPNum = 1, NumHeat

        HPNum= HPNum + 1

        CALL GetObjectItem(CurrentModuleObject,WatertoAirHPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT,&
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.

        CALL VerifyName(AlphArray(1),WatertoAirHP%Name,HPNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        WatertoAirHP(HPNum)%Name     = TRIM(AlphArray(1))
        WatertoAirHP(HPNum)%WatertoAirHPType  = 'HEATING'
        WatertoAirHP(HPNum)%WAHPPlantTypeOfNum = TypeOf_CoilWAHPHeatingParamEst
        WatertoAirHP(HPNum)%Refrigerant = TRIM(AlphArray(3))
        WatertoAirHP(HPNum)%DesignWaterVolFlowRate = NumArray(1)
        WatertoAirHP(HPNum)%HeatingCapacity = NumArray(2)

        WatertoAirHP(HPNum)%HighPressCutOff=NumArray(3)
        WatertoAirHP(HPNum)%LowPressCutOff=NumArray(4)


        WatertoAirHP(HPNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                            NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WatertoAirHP(HPNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                            NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WatertoAirHP(HPNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(6),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WatertoAirHP(HPNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(7),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        WatertoAirHP(HPNum)%LoadSideTotalUACoeff=NumArray(5)
        IF (WatertoAirHP(HPNum)%LoadSideTotalUACoeff .LT. rTinyValue) THEN
          CALL ShowSevereError('Input problem for '//TRIM(CurrentModuleObject)//'='//TRIM(WatertoAirHP(HPNum)%Name))
          CALL ShowContinueError(' Load side UA value is less than tolerance, likely zero or blank.')
          CALL ShowContinueError(' Verify inputs, as the parameter syntax for this object went through a change with')
          CALL ShowContinueError('  the release of EnergyPlus version 5.')
          ErrorsFound = .TRUE.
        END IF

        WatertoAirHP(HPNum)%SuperheatTemp=NumArray(6)
        WatertoAirHP(HPNum)%PowerLosses=NumArray(7)
        WatertoAirHP(HPNum)%LossFactor=NumArray(8)

        SELECT CASE  (AlphArray(2))

          CASE ('RECIPROCATING')
            WaterToAirHP(HPNum)%CompressorType=CompressorType_Reciprocating
            WatertoAirHP(HPNum)%CompPistonDisp=NumArray(9)
            WatertoAirHP(HPNum)%CompSucPressDrop=NumArray(10)
            WatertoAirHP(HPNum)%CompClearanceFactor=NumArray(11)

          CASE ('ROTARY')
            WaterToAirHP(HPNum)%CompressorType=CompressorType_Rotary
            WatertoAirHP(HPNum)%CompPistonDisp=NumArray(9)
            WatertoAirHP(HPNum)%CompSucPressDrop=NumArray(10)

          CASE ('SCROLL')
            WaterToAirHP(HPNum)%CompressorType=CompressorType_Scroll
            WatertoAirHP(HPNum)%RefVolFlowRate=NumArray(12)
            WatertoAirHP(HPNum)%VolumeRatio=NumArray(13)
            WatertoAirHP(HPNum)%LeakRateCoeff=NumArray(14)

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(2))//' ('//  &
              TRIM(AlphArray(2))//') entered.'//TRIM(CurrentModuleObject)//'='//TRIM(WatertoAirHP(HPNum)%Name))
            ErrorsFound=.true.

        END SELECT

        WatertoAirHP(HPNum)%SourceSideUACoeff = NumArray(15)
        WatertoAirHP(HPNum)%SourceSideHTR1    = NumArray(16)
        WatertoAirHP(HPNum)%SourceSideHTR2    = NumArray(17)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(6),AlphArray(7),'Air Nodes')

        CALL SetupOutputVariable('Heating Coil Electric Energy [J]', &
             WatertoAirHP(HPNum)%Energy,'System','Summed',WatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')

        CALL SetupOutputVariable('Heating Coil Heating Energy [J]', &
             WatertoAirHP(HPNum)%EnergyLoadTotal,'System','Summed',WatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Energy [J]', &
             WatertoAirHP(HPNum)%EnergySource,'System','Summed',WatertoAirHP(HPNum)%Name, &
             ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')

        ! save the design source side flow rate for use by plant loop sizing algorithms
        CALL RegisterPlantCompDesignFlow(WatertoAirHP(HPNum)%WaterInletNodeNum,0.5d0*WatertoAirHP(HPNum)%DesignWaterVolFlowRate)

        !create predefined report entries
        CALL PreDefTableEntry(pdchHeatCoilType,WatertoAirHP(HPNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchHeatCoilNomCap,WatertoAirHP(HPNum)%Name,WatertoAirHP(HPNum)%HeatingCapacity)
        CALL PreDefTableEntry(pdchHeatCoilNomEff,WatertoAirHP(HPNum)%Name,'-')

    END DO

    DEALLOCATE(AlphArray)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(lNumericBlanks)
    DEALLOCATE(NumArray)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found getting input. Program terminates.')
  ENDIF


  DO HPNum=1,NumWatertoAirHPs

    IF ( WatertoAirHP(HPNum)%WAHPPlantTypeOfNum == TypeOf_CoilWAHPCoolingParamEst) THEN
        ! COOLING COIL: Setup Report variables for the Heat Pump
      CALL SetupOutputVariable('Cooling Coil Electric Power [W]', &
           WatertoAirHP(HPNum)%Power,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', &
           WatertoAirHP(HPNum)%QLoadTotal,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', &
           WatertoAirHP(HPNum)%QSensible,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Latent Cooling Rate [W]', &
           WatertoAirHP(HPNum)%QLatent,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Rate [W]', &
           WatertoAirHP(HPNum)%QSource,'System','Average',WatertoAirHP(HPNum)%Name)


      CALL SetupOutputVariable('Cooling Coil Part Load Ratio []', &
           WatertoAirHP(HPNum)%PartLoadRatio,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Runtime Fraction []', &
           WatertoAirHP(HPNum)%RunFrac,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Air Mass Flow Rate [kg/s]', &
           WatertoAirHP(HPNum)%OutletAirMassFlowRate,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Inlet Temperature [C]', &
           WatertoAirHP(HPNum)%InletAirDBTemp,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
           WatertoAirHP(HPNum)%InletAirHumRat,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Outlet Temperature [C]', &
           WatertoAirHP(HPNum)%OutletAirDBTemp,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
           WatertoAirHP(HPNum)%OutletAirHumRat,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Source Side Mass Flow Rate [kg/s]', &
           WatertoAirHP(HPNum)%OutletWaterMassFlowRate,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Source Side Inlet Temperature [C]', &
           WatertoAirHP(HPNum)%InletWaterTemp,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Source Side Outlet Temperature [C]', &
           WatertoAirHP(HPNum)%OutletWaterTemp,'System','Average',WatertoAirHP(HPNum)%Name)
    ELSEIF (WatertoAirHP(HPNum)%WAHPPlantTypeOfNum == TypeOf_CoilWAHPHeatingParamEst) THEN
        ! HEATING COIL Setup Report variables for the Heat Pump
      CALL SetupOutputVariable('Heating Coil Electric Power [W]', &
           WatertoAirHP(HPNum)%Power,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Heating Coil Heating Rate [W]', &
           WatertoAirHP(HPNum)%QLoadTotal,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Heating Coil Sensible Heating Rate [W]', &
           WatertoAirHP(HPNum)%QSensible,'System','Average',WatertoAirHP(HPNum)%Name)


      CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Rate [W]', &
           WatertoAirHP(HPNum)%QSource,'System','Average',WatertoAirHP(HPNum)%Name)


      CALL SetupOutputVariable('Heating Coil Part Load Ratio []', &
           WatertoAirHP(HPNum)%PartLoadRatio,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Runtime Fraction []', &
           WatertoAirHP(HPNum)%RunFrac,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Heating Coil Air Mass Flow Rate [kg/s]', &
           WatertoAirHP(HPNum)%OutletAirMassFlowRate,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Inlet Temperature [C]', &
           WatertoAirHP(HPNum)%InletAirDBTemp,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
           WatertoAirHP(HPNum)%InletAirHumRat,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Outlet Temperature [C]', &
           WatertoAirHP(HPNum)%OutletAirDBTemp,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
           WatertoAirHP(HPNum)%OutletAirHumRat,'System','Average',WatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Heating Coil Source Side Mass Flow Rate [kg/s]', &
           WatertoAirHP(HPNum)%OutletWaterMassFlowRate,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Source Side Inlet Temperature [C]', &
           WatertoAirHP(HPNum)%InletWaterTemp,'System','Average',WatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Source Side Outlet Temperature [C]', &
           WatertoAirHP(HPNum)%OutletWaterTemp,'System','Average',WatertoAirHP(HPNum)%Name)

    ENDIF



  END DO


  RETURN

END SUBROUTINE GetWatertoAirHPInput
! End of Get Input subroutines for the HB Module
!******************************************************************************


 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitWatertoAirHP(HPNum,InitFlag,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime, &
                            SensLoad,LatentLoad,DesignAirFlow,PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hui Jin
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004)
          !                      Brent Griffith, Sept 2010, plant upgrades, general fluid properties

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Water to Air HP Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:

          ! USE STATEMENTS:

  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : ScanPlantLoopsForObject, PlantLoop
  USE PlantUtilities,  ONLY : InitComponentNodes, SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: HPNum  ! index to main heat pump data structure
  REAL(r64),    INTENT(IN) :: MaxONOFFCyclesperHour !Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64),    INTENT(IN) :: HPTimeConstant        !Heat pump time constant [s]
  REAL(r64),    INTENT(IN) :: FanDelayTime          !Fan delay time, time delay for the HP's fan to
                                               !shut off after compressor cycle off  [s]
  REAL(r64),    INTENT(IN) :: SensLoad
  REAL(r64),    INTENT(IN) :: LatentLoad
  REAL(r64),    INTENT(IN) :: DesignAirFlow
  LOGICAL,      INTENT(IN) :: InitFlag
  REAL(r64),    INTENT(IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
! REAL(r64), PARAMETER        :: CpWater=4210.d0          ! Specific heat of water J/kg_C
  REAL(r64), PARAMETER        :: TempTOL=0.2d0            ! air temperature tolerance to trigger resimulation
  REAL(r64), PARAMETER        :: EnthTOL=0.2d0            ! air enthalpy tolerance to trigger resimulation
  REAL(r64), PARAMETER        :: HumRatTOL=0.2d0          ! air humidity ratio tolerance

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER             :: WatertoAirHPNum          ! heat pump number
  INTEGER             :: AirInletNode             ! air inlet node number
  INTEGER             :: WaterInletNode           ! water inlet node number
  INTEGER             :: PlantOutletNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:)  :: MyEnvrnFlag
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: Cp  ! local fluid specific heat
  REAL(r64) :: Temptemp
  LOGICAL   :: errFlag

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumWatertoAirHPs))
    ALLOCATE(MyPlantScanFlag(NumWatertoAirHPs))
    MyEnvrnFlag = .TRUE.
    MyPlantScanFlag   = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  IF (MyPlantScanFlag(HPNum) .AND. ALLOCATED(PlantLoop)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(WatertoAirHP(HPNum)%Name,  &
                                 WatertoAirHP(HPNum)%WAHPPlantTypeOfNum, &
                                 WatertoAirHP(HPNum)%LoopNum, &
                                 WatertoAirHP(HPNum)%LoopSide, &
                                 WatertoAirHP(HPNum)%BranchNum, &
                                 WatertoAirHP(HPNum)%CompNum,   &
                                 errFlag=errFlag)

    IF(PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidName=='WATER') THEN
      IF (WatertoAirHP(HPNum)%SourceSideUACoeff .LT. rTinyValue) THEN
        CALL ShowSevereError('Input problem for water to air heat pump, "'//TRIM(WatertoAirHP(HPNum)%Name) //'".')
        CALL ShowContinueError(' Source side UA value is less than tolerance, likely zero or blank.')
        CALL ShowContinueError(' Verify inputs, as the parameter syntax for this object went through a change with')
        CALL ShowContinueError('  the release of EnergyPlus version 5.')
        errFlag = .TRUE.
      END IF
    ELSE
      IF ((WatertoAirHP(HPNum)%SourceSideHTR1 .LT. rTinyValue) .OR. (WatertoAirHP(HPNum)%SourceSideHTR2 .LT. rTinyValue)) THEN
        CALL ShowSevereError('Input problem for water to air heat pump, "'//TRIM(WatertoAirHP(HPNum)%Name)//'".')
        CALL ShowContinueError(' A source side heat transfer resistance value is less than tolerance, likely zero or blank.')
        CALL ShowContinueError(' Verify inputs, as the parameter syntax for this object went through a change with')
        CALL ShowContinueError('  the release of EnergyPlus version 5.')
        errFlag = .TRUE.
      END IF
    END IF

    IF (errFlag) THEN
      CALL ShowFatalError('InitWatertoAirHP: Program terminated for previous conditions.')
    ENDIF

    MyPlantScanFlag(HPNum) = .FALSE.
  ENDIF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(HPNum) .AND. .NOT. MyPlantScanFlag(HPNum)) THEN
    ! Do the initializations to start simulation
    ! Set water and air inlet nodes
    AirInletNode   = WatertoAirHP(HPNum)%AirInletNodeNum
    WaterInletNode = WatertoAirHP(HPNum)%WaterInletNodeNum

      !Initialize all report variables to a known state at beginning of simulation
    WatertoAirHP(HPNum)%Power=0.d0
    WatertoAirHP(HPNum)%Energy=0.d0
    WatertoAirHP(HPNum)%QLoadTotal=0.d0
    WatertoAirHP(HPNum)%QSensible=0.d0
    WatertoAirHP(HPNum)%QLatent=0.d0
    WatertoAirHP(HPNum)%QSource=0.d0
    WatertoAirHP(HPNum)%EnergyLoadTotal=0.d0
    WatertoAirHP(HPNum)%EnergySensible=0.d0
    WatertoAirHP(HPNum)%EnergyLatent=0.d0
    WatertoAirHP(HPNum)%EnergySource=0.d0
    WatertoAirHP(HPNum)%RunFrac=0.d0
    WatertoAirHP(HPNum)%PartLoadRatio=0.d0
    WatertoAirHP(HPNum)%OutletAirDBTemp=0.d0
    WatertoAirHP(HPNum)%OutletAirHumRat=0.d0
    WatertoAirHP(HPNum)%InletAirDBTemp=0.d0
    WatertoAirHP(HPNum)%InletAirHumRat=0.d0
    WatertoAirHP(HPNum)%OutletWaterTemp=0.d0
    WatertoAirHP(HPNum)%InletWaterTemp=0.d0
    WatertoAirHP(HPNum)%InletAirMassFlowRate=0.d0
    WatertoAirHP(HPNum)%InletWaterMassFlowRate=0.d0
    WatertoAirHP(HPNum)%OutletAirEnthalpy = 0.d0
    WatertoAirHP(HPNum)%OutletWaterEnthalpy = 0.d0

      ! The rest of the one time initializations
    rho = GetDensityGlycol(PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidName, &
                              InitConvTemp, &
                              PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidIndex, &
                              'InitWatertoAirHP')
    Cp  = GetSpecificHeatGlycol(PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidName, &
                              InitConvTemp, &
                              PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidIndex, &
                              'InitWatertoAirHP')

    WatertoAirHP(HPNum)%DesignWaterMassFlowRate= rho * WatertoAirHP(HPNum)%DesignWaterVolFlowRate
    WatertoAirHP(HPNum)%MaxONOFFCyclesperHour=MaxONOFFCyclesperHour
    WatertoAirHP(HPNum)%HPTimeConstant=HPTimeConstant
    WatertoAirHP(HPNum)%FanDelayTime=FanDelayTime

    PlantOutletNode = PlantLoop(WatertoAirHP(HPNum)%LoopNum)%LoopSide(WatertoAirHP(HPNum)%LoopSide) &
                         %Branch(WatertoAirHP(HPNum)%BranchNum)%Comp(WatertoAirHP(HPNum)%CompNum)%NodeNumOut
    Call InitComponentNodes(0.d0, WatertoAirHP(HPNum)%DesignWaterMassFlowRate, &
                                 WaterInletNode, PlantOutletNode , &
                                 WatertoAirHP(HPNum)%LoopNum, &
                                 WatertoAirHP(HPNum)%LoopSide, &
                                 WatertoAirHP(HPNum)%BranchNum, &
                                 WatertoAirHP(HPNum)%CompNum )

    Node(WaterInletNode)%Temp          = 5.0d0
    Node(WaterInletNode)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
    Node(WaterInletNode)%Quality       = 0.0d0
    Node(WaterInletNode)%Press         = 0.0d0
    Node(WaterInletNode)%HumRat        = 0.0d0

    Node(PlantOutletNode)%Temp          = 5.0d0
    Node(PlantOutletNode)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
    Node(PlantOutletNode)%Quality       = 0.0d0
    Node(PlantOutletNode)%Press         = 0.0d0
    Node(PlantOutletNode)%HumRat        = 0.0d0

    WatertoAirHP(HPNum)%SimFlag = .TRUE.

    MyEnvrnFlag(HPNum) = .FALSE.
  END IF  ! End If for the Begin Environment initializations

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(HPNum)=.TRUE.
  ENDIF


  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! First set the conditions for the air into the heat pump model

  ! Set water and air inlet nodes
   AirInletNode = WatertoAirHP(HPNum)%AirInletNodeNum
   WaterInletNode = WatertoAirHP(HPNum)%WaterInletNodeNum

!  ! Set heat pump simulation flag to false if the air loop and water loop conditions have not changed
!  IF( .NOT. (BeginEnvrnFlag .and. MyEnvrnFlag) .AND. (&
!  WatertoAirHP(HPNum)%InletWaterTemp      >= (Node(WaterInletNode)%Temp + TempTOL) .OR. &
!  WatertoAirHP(HPNum)%InletWaterTemp      <= (Node(WaterInletNode)%Temp - TempTOL) .OR. &
!  WatertoAirHP(HPNum)%InletWaterEnthalpy  >= (Node(WaterInletNode)%Enthalpy + EnthTOL) .OR. &
!  WatertoAirHP(HPNum)%InletWaterEnthalpy  <= (Node(WaterInletNode)%Enthalpy - EnthTOL) .OR. &!!

!  WatertoAirHP(HPNum)%InletAirDBTemp      >= (Node(AirInletNode)%Temp + TempTOL) .OR. &
!  WatertoAirHP(HPNum)%InletAirDBTemp      <= (Node(AirInletNode)%Temp - TempTOL) .OR. &
!  WatertoAirHP(HPNum)%InletAirHumRat      >= (Node(AirInletNode)%HumRat + HumRatTOL) .OR. &
!  WatertoAirHP(HPNum)%InletAirHumRat      <= (Node(AirInletNode)%HumRat - HumRatTOL) .OR. &
!  WatertoAirHP(HPNum)%InletAirEnthalpy    >= (Node(AirInletNode)%Enthalpy + EnthTOL) .OR. &
!  WatertoAirHP(HPNum)%InletAirEnthalpy    <= (Node(AirInletNode)%Enthalpy - EnthTOL) .OR. &
!  WatertoAirHP(HPNum)%InletAirMassFlowRate > 0.0))THEN
!    WatertoAirHP(HPNum)%SimFlag =.TRUE.
!  ELSE
!    WatertoAirHP(HPNum)%SimFlag =.FALSE.
!  ENDIF


  IF(((SensLoad .NE. 0.0d0 .OR. LatentLoad .NE. 0.0d0) .OR. (SensLoad .EQ. 0.0d0 .AND. InitFlag)) &
     .AND. Node(AirInletNode)%MassFlowRate > 0.0d0 .AND. PartLoadRatio > 0.0d0) THEN
    !set the water side flow rate to the design flow rate unless constrained by
    !the demand side manager (MIN/MAX available). now done by call to setcomponentFlowRate
    WatertoAirHP(HPNum)%InletWaterMassFlowRate = WatertoAirHP(HPNum)%DesignWaterMassFlowRate
    WatertoAirHP(HPNum)%InletAirMassFlowRate = DesignAirFlow !This is required instead of the node temperature
                                                             !because the air loop operates handles part load for
                                                             !cycling equipment by modulating the air flow rate
                                                             !the heat pump model requires an accurate (i.e. full load
                                                             !flow rate for accurate simulation.
  ELSE !heat pump is off
    WatertoAirHP(HPNum)%InletWaterMassFlowRate = 0.d0

    WatertoAirHP(HPNum)%InletAirMassFlowRate = 0.0d0
  ENDIF
  !constrain water flow provided by plant
  CALL SetComponentFlowRate(WatertoAirHP(HPNum)%InletWaterMassFlowRate, &
                                 WatertoAirHP(HPNum)%WaterInletNodeNum , &
                                 WatertoAirHP(HPNum)%WaterOutletNodeNum, &
                                 WatertoAirHP(HPNum)%LoopNum, &
                                 WatertoAirHP(HPNum)%LoopSide, &
                                 WatertoAirHP(HPNum)%BranchNum, &
                                 WatertoAirHP(HPNum)%CompNum )

  WatertoAirHP(HPNum)%InletWaterTemp         = Node(WaterInletNode)%Temp
!  IF (WatertoAirHP(HPNum)%InletWaterTemp < 0.0) THEN  ! Debug trap
!    Temptemp         = Node(WaterInletNode)%Temp
!  ENDIF
  WatertoAirHP(HPNum)%InletWaterEnthalpy     = Node(WaterInletNode)%Enthalpy

  WatertoAirHP(HPNum)%InletAirDBTemp       = Node(AirInletNode)%Temp
  WatertoAirHP(HPNum)%InletAirHumRat       = Node(AirInletNode)%HumRat
  WatertoAirHP(HPNum)%InletAirEnthalpy     = Node(AirInletNode)%Enthalpy


  WatertoAirHP(HPNum)%Power=0.0d0
  WatertoAirHP(HPNum)%Energy=0.0d0
  WatertoAirHP(HPNum)%QLoadTotal=0.0d0
  WatertoAirHP(HPNum)%QSensible=0.0d0
  WatertoAirHP(HPNum)%QLatent=0.0d0
  WatertoAirHP(HPNum)%QSource=0.0d0
  WatertoAirHP(HPNum)%EnergyLoadTotal=0.0d0
  WatertoAirHP(HPNum)%EnergySensible=0.0d0
  WatertoAirHP(HPNum)%EnergyLatent=0.0d0
  WatertoAirHP(HPNum)%EnergySource=0.0d0
  WatertoAirHP(HPNum)%RunFrac=0.0d0
  WatertoAirHP(HPNum)%OutletAirDBTemp=0.0d0
  WatertoAirHP(HPNum)%OutletAirHumRat=0.0d0
  WatertoAirHP(HPNum)%OutletWaterTemp=0.0d0
  WatertoAirHP(HPNum)%OutletAirEnthalpy = 0.0d0
  WatertoAirHP(HPNum)%OutletWaterEnthalpy = 0.0d0

  RETURN

END SUBROUTINE InitWatertoAirHP

 ! End Initialization Section of the Module
!******************************************************************************


! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE CalcWatertoAirHPCooling(HPNum,CyclingScheme,FirstHVACIteration,RuntimeFrac,initflag,SensDemand,CompOp,PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hui Jin
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       Dan Fisher, Kenneth Tang (Jan 2004), R. Raustad (Oct 2006) Revised iteration technique
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a parameter estimation based water to air heat pump model

          ! USE STATEMENTS:
          ! na

  USE FluidProperties
!  USE DataZoneEnergyDemands
  USE CurveManager,         ONLY: CurveValue,GetCurveIndex
  USE Psychrometrics,       ONLY: PsyHFnTdbW,PsyTdbFnHW,PsyWFnTdbH,PsyTwbFnTdbWPb, & !,PsyHFnTdbRhPb,PsyWFnTdpPb
                                  PsyCpAirFnWTdb, PsyTsatFnHPb
  USE General,              ONLY: RoundSigDigits, SolveRegulaFalsi
  USE InputProcessor,       ONLY: SameString
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN) ::   HPNum               !heat pump number
   REAL(r64)   , INTENT(IN) ::   RuntimeFrac
   REAL(r64)   , INTENT(IN) ::   SensDemand
   INTEGER, INTENT(IN) ::   CyclingScheme       !fan/compressor cycling scheme indicator
   LOGICAL, INTENT (IN)::   FirstHVACIteration  !first iteration flag
   LOGICAL, INTENT (IN)::   initflag            !suppress property errors if true
   INTEGER, INTENT(IN) ::   CompOp
   REAL(r64), INTENT(IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
      REAL(r64), PARAMETER        :: CpWater=4210.d0             ! Specific heat of water J/kg_C
      REAL(r64), PARAMETER        :: DegreeofSuperheat=80.d0    ! Initial guess of degree of superheat
      REAL(r64), PARAMETER        :: gamma= 1.114d0             ! Expansion Coefficient
      REAL(r64), PARAMETER        :: RelaxParam = .5d0          ! Relaxation Parameter
      REAL(r64), PARAMETER        :: ERR=0.01d0                 ! Error Value
      REAL(r64), PARAMETER        :: ERR1=0.001d0               ! Error Value
      REAL(r64), PARAMETER        :: PB=1.013d5               ! Barometric Pressure (Pa)


      INTEGER, PARAMETER     :: STOP1=100000             ! Iteration stopper1
      INTEGER, PARAMETER     :: STOP2=100000             ! Iteration stopper2
      INTEGER, PARAMETER     :: STOP3=100000             ! Iteration stopper3


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!      INTEGER                :: NumIteration1            ! Number of Iteration1
      INTEGER                :: NumIteration2            ! Number of Iteration2
      INTEGER                :: NumIteration3            ! Number of Iteration3
      INTEGER                :: NumIteration4            ! Number of Iteration4 (use of latent degradation model ONLY)
      INTEGER                :: SourceSideFluidIndex     ! Source Side Fluid Index

      INTEGER :: CompressorType           ! Type of Compressor ie. Reciprocating,Rotary or Scroll
      CHARACTER(len=MaxNameLength) :: SourceSideFluidName   ! Name of source side fluid
      CHARACTER(len=MaxNameLength) :: Refrigerant           ! Name of refrigerant
!      CHARACTER(len=25) :: CErrCount
      REAL(r64)        :: NominalCoolingCapacity   ! Nominal Cooling Capacity (W)
      REAL(r64)        :: LoadSideTotalUA          ! Load Side Total Heat Transfer coefficient [W/C]
      REAL(r64)        :: LoadSideoutsideUA        ! Load Side Outside Heat Transfer coefficient [W/C]
      REAL(r64)        :: SourceSideUA             ! Source Side Heat Transfer coefficient [W/C]
      REAL(r64)        :: PressureDrop             ! Suction or Discharge Pressure Drop [Pa]
      REAL(r64)        :: ClearanceFactor          ! Compressor Clearance Factor
      REAL(r64)        :: PistonDisp               ! Compressor Piston Displacement [m3/s]
      REAL(r64)        :: ShTemp                   ! Superheat Temperature [C]
      REAL(r64)        :: LosFac                   ! Compressor Power Loss Factor
      REAL(r64)        :: PowerLos                 ! Constant Part of Power Losses [kW]
      REAL(r64)        :: RefVolFlowRate           ! Refrigerant Volume Flow rate at the beginning
      REAL(r64)        :: VolumeRatio              ! Built-in-volume ratio [~]
      REAL(r64)        :: LeakRateCoeff            ! Coefficient for the relationship between
                                                   ! Pressure Ratio and Leakage Rate [~]
      REAL(r64)        :: SourceSideHTRes1         ! Source Side Heat Transfer Resistance coefficient 1 [~]
      REAL(r64)        :: SourceSideHTRes2         ! Source Side Heat Transfer Resistance coefficient 2 [K/kW]
      REAL(r64)        :: HighPressCutOff          ! High Pressure Cut-off [Pa]
      REAL(r64)        :: LowPressCutOff           ! Low Pressure Cut-off [Pa]

      REAL(r64)        :: Quality                  ! Quality of Refrigerant
      REAL(r64)        :: SourceSideMassFlowRate   ! Source Side Mass Flow Rate [kg/s]
      REAL(r64)        :: SourceSideInletTemp      ! Source Side Inlet Temperature [C]
      REAL(r64)        :: SourceSideWaterInletEnth ! Source Side Outlet Enthalpy [J/kg]
      REAL(r64)        :: SourceSideoutletTemp     ! Source Side Outlet Temperature [C]
      REAL(r64)        :: SourceSideVolFlowRate    ! Source Side Volumetric Flow Rate [m3/s]
      REAL(r64)        :: DegradFactor             ! Degradation Factor [~]
      REAL(r64)        :: CpFluid                  ! Specific heat of source side fluid(J/kg)
      REAL(r64)        :: LoadSideMassFlowRate     ! Load Side Mass Flow Rate [kg/s]
      REAL(r64)        :: LoadSideInletWBTemp      ! Wet-bulb temperature of indoor inlet air [C]
      REAL(r64)        :: LoadSideInletDBTemp      ! Load Side Inlet Dry Bulb Temp [C]
      REAL(r64)        :: LoadSideInletHumRat      ! Load Side Inlet Humidity Ratio [kg/kg]
      REAL(r64)        :: LoadSideoutletDBTemp     ! Load Side Outlet Dry Bulb Temperature [C]
      REAL(r64)        :: LoadsideOutletHumRat     ! Load Side Outlet Humidity Ratio [kg/kg]
      REAL(r64)        :: LoadSideAirInletEnth     ! Load Side Inlet Enthalpy [J/kg]
      REAL(r64)        :: LoadSideAirOutletEnth    ! Load Side Outlet Enthalpy [J/kg]
!      REAL(r64)        :: EffectiveSurfaceTemp1    ! Effective Surface Temperature Guess #1 [C]
!      REAL(r64)        :: EffectiveSurfaceTemp2    ! Effective Surface Temperature Guess #2 [C]
      REAL(r64), SAVE  :: EffectiveSurfaceTemp     ! Effective Surface Temperature [C]
      REAL(r64)        :: EffectiveSatEnth         ! Saturated Enthalpy of Air Corresponding to the Effective Surface
                                                   ! Temperature [J/kg]
!      REAL(r64)        :: EffectiveSatEnth1        ! Guess of the Saturated Enthalpy of Air Corresponding to the
!                                                   ! Effective Surface Temperature [J/kg]
      REAL(r64)        :: QSource                  ! Source Side Heat Transfer Rate [W]
      REAL(r64)        :: QLoadTotal               ! Load Side Total Heat Transfer Rate [W]
      REAL(r64)        :: QSensible                ! Load Side Sensible Heat Transfer Rate [W]
      REAL(r64)        :: Power                    ! Power Consumption [W]
!      REAL(r64)        :: EvapTemp1                ! Evaporating Temperature Guess #1 [C]
!      REAL(r64)        :: EvapTemp2                ! Evaporating Temperature Guess #2 [C]
      REAL(r64) ,SAVE  :: EvapTemp                 ! Evaporating Temperature [C]
      REAL(r64)        :: ANTUWET                  ! Number of Transfer Unit for Wet Condition
      REAL(r64)        :: EffectWET                ! Load Side Heat Exchanger Effectiveness
      REAL(r64)        :: EvapSatEnth              ! Saturated Enthalpy of Air Corresponding to the Evaporating
                                                   ! Temperature [J/kg]
!      REAL(r64)        :: EvapSatEnth1             ! Guess of the Saturated Enthalpy of Air Corresponding to the
!                                                   ! Evaporating Temperature [J/kg]
      REAL(r64)        :: SourceSideEffect         ! Source Side Heat Exchanger Effectiveness
      REAL(r64)        :: LoadSideEffec            ! Load Side Effectiveness based on Outside Heat Transfer Coefficient
      REAL(r64)        :: SourceSideTemp           ! Source Side Saturated Refrigerant Temperature [C]
      REAL(r64)        :: LoadSideTemp             ! Load Side Saturated Refrigerant Temperature [C]
      REAL(r64)        :: SourceSidePressure       ! Source Side Saturated Refrigerant Pressure [Pa]
      REAL(r64)        :: LoadSidePressure         ! Load Side Saturated Refrigerant Pressure [Pa]
      REAL(r64)        :: SuctionPr                ! Compressor Suction Pressure [Pa]
      REAL(r64)        :: DischargePr              ! Compressor Discharge Pressure [Pa]
      REAL(r64)        :: CompressInletTemp        ! Temperature of the Refrigerant Entering the Compressor [C]
      REAL(r64)        :: MassRef                  ! Mass Flow Rate of Refrigerant [kg/s]
      REAL(r64)        :: SourceSideOutletEnth     ! Enthalpy of Refrigerant leaving the Source Side Heat Exchanger [J/kg]
      REAL(r64)        :: LoadSideOutletEnth       ! Enthalpy of Refrigerant leaving the Load Side Heat Exchanger [J/kg]
      REAL(r64)        :: CpAir                    ! Specific Heat of Air [J/kg_C]
      REAL(r64), SAVE       :: initialQSource           ! Guess Source Side Heat Transfer Rate [W]
      REAL(r64), SAVE       :: initialQLoadTotal        ! Guess Load Side Heat Transfer rate [W]
      REAL(r64)        :: SuperHeatEnth            ! Enthalpy of the Superheated Refrigerant [J/kg]
      REAL(r64)        :: CompSuctionTemp1         ! Guess of the Temperature of the Refrigerant Entering the
                                                   ! Compressor #1 [C]
      REAL(r64)        :: CompSuctionTemp2         ! Guess of the Temperature of the Refrigerant Entering the
                                                   ! Compressor #2 [C]
      REAL(r64), SAVE       :: CompSuctionTemp          ! Temperature of the Refrigerant Entering the Compressor [C]
      REAL(r64)        :: CompSuctionEnth          ! Enthalpy of the Refrigerant Entering the Compressor [J/kg]
      REAL(r64)        :: CompSuctionDensity       ! Density of the Refrigerant Entering the Compressorkg/m3
      REAL(r64)        :: CompSuctionSatTemp       ! Temperature of Saturated Refrigerant at Compressor Suction Pressure [C]
      REAL(r64)        :: Twet_rated               ! Twet at rated conditions (coil air flow rate and air temperatures), sec
      REAL(r64)        :: Gamma_rated              ! Gamma at rated conditions (coil air flow rate and air temperatures)
      LOGICAL          :: LatDegradModelSimFlag    ! Latent degradation model simulation flag
      LOGICAL          :: FinalSimFlag             ! Final Simulation Flag
      LOGICAL          :: Converged                ! overall convergence Flag

      REAL(r64)        :: QLatRated                ! Qlatent at rated conditions of indoor(TDB,TWB)=(26.7C,19.4C)
      REAL(r64)        :: QLatActual               ! Qlatent at actual operating conditions
      REAL(r64)        :: SHRss                    ! Sensible heat ratio at steady state
      REAL(r64)        :: SHReff                   ! Effective sensible heat ratio at part-load condition
      REAL(r64)        :: Par(4)                   ! Parameter array passed to RegulaFalsi function
      INTEGER          :: SolFlag                  ! Solution flag returned from RegulaFalsi function
      LOGICAL          :: ErrorsFound = .FALSE.
      LOGICAL, SAVE    :: FirstTime = .true.
      REAL(r64), SAVE  :: LoadSideInletDBTemp_Init ! rated conditions
      REAL(r64), SAVE  :: LoadSideInletHumRat_Init ! rated conditions
      REAL(r64), SAVE  :: LoadSideAirInletEnth_Init ! rated conditions
      REAL(r64)        :: LoadSideInletDBTemp_Unit ! calc conditions for unit
      REAL(r64)        :: LoadSideInletHumRat_Unit ! calc conditions for unit
      REAL(r64)        :: LoadSideAirInletEnth_Unit ! calc conditions for unit

    IF (FirstTime) THEN
      !Set indoor air conditions to the rated condition
      LoadSideInletDBTemp_Init = 26.7d0
      LoadSideInletHumRat_Init = 0.0111d0
      LoadSideAirInletEnth_Init = PsyHFnTdbW(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init)
      FirstTime=.false.
    ENDIF

 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
    NominalCoolingCapacity = WatertoAirHP(HPNum)%CoolingCapacity
    CompressorType = WatertoAirHP(HPNum)%CompressorType
    Refrigerant = WatertoAirHP(HPNum)%Refrigerant
    LoadSideTotalUA   = WatertoAirHP(HPNum)%LoadSideTotalUACoeff
    LoadSideoutsideUA = WatertoAirHP(HPNum)%LoadSideOutsideUACoeff
    PistonDisp        = WatertoAirHP(HPNum)%CompPistonDisp
    ClearanceFactor   = WatertoAirHP(HPNum)%CompClearanceFactor
    PressureDrop      = WatertoAirHP(HPNum)%CompSucPressDrop
    ShTemp            = WatertoAirHP(HPNum)%SuperheatTemp
    PowerLos          = WatertoAirHP(HPNum)%PowerLosses
    LosFac            = WatertoAirHP(HPNum)%LossFactor
    RefVolFlowRate    = WatertoAirHP(HPNum)%RefVolFlowRate
    VolumeRatio       = WatertoAirHP(HPNum)%VolumeRatio
    LeakRateCoeff     = WatertoAirHP(HPNum)%LeakRateCoeff
    SourceSideUA      = WatertoAirHP(HPNum)%SourceSideUACoeff
    SourceSideHTRes1  = WatertoAirHP(HPNum)%SourceSideHTR1
    SourceSideHTRes2  = WatertoAirHP(HPNum)%SourceSideHTR2
    HighPressCutOff   = WatertoAirHP(HPNum)%HighPressCutOff
    LowPressCutOff    = WatertoAirHP(HPNum)%LowPressCutOff

    LoadSideMassFlowRate    = WatertoAirHP(HPNum)%InletAirMassFlowRate
                               !Set indoor air conditions to the actual condition
    LoadSideInletDBTemp_Unit= WatertoAirHP(HPNum)%InletAirDBTemp
    LoadSideInletHumRat_Unit= WatertoAirHP(HPNum)%InletAirHumRat
    cpair = PsyCpAirFnWTdb(LoadSideInletHumRat_Unit,LoadSideInletDBTemp_Unit)
    LoadSideAirInletEnth_Unit = PsyHFnTdbW(LoadSideInletDBTemp_Unit,LoadSideInletHumRat_Unit)

    SourceSideInletTemp      = WatertoAirHP(HPNum)%InletWaterTemp
    SourceSideWaterInletEnth = WatertoAirHP(HPNum)%InletWaterEnthalpy
    SourceSideFluidName      = PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidName
    SourceSideFluidIndex     = PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidIndex
    SourceSideMassFlowRate = WatertoAirHP(HPNum)%InletWaterMassFlowRate
    SourceSideVolFlowRate  = SourceSideMassFlowRate / GetDensityGlycol(SourceSideFluidName,        &
                                                        SourceSideInletTemp,SourceSideFluidIndex,  &
                                                       'CalcWatertoAirHPCooling:SourceSideInletTemp')

    Twet_rated  = WatertoAirHP(HPNum)%Twet_Rated
    Gamma_rated = WatertoAirHP(HPNum)%Gamma_Rated

    FinalSimFlag= .FALSE.

! If heat pump is not operating, return
IF (SensDemand == 0.0d0 .OR. LoadSideMassFlowRate <= 0.0d0 .OR. SourceSideMassFlowRate <= 0.0d0)THEN
   WatertoAirHP(HPNum)%SimFlag = .FALSE.
   RETURN
ELSE
   WatertoAirHP(HPNum)%SimFlag = .TRUE.
ENDIF

IF (CompOp .EQ. 0) THEN
   WaterToAirHP(HPNum)%SimFlag = .FALSE.
   RETURN
ENDIF

IF(FirstHVACIteration) THEN
  initialQSource = NominalCoolingCapacity
  initialQLoadTotal = NominalCoolingCapacity
ENDIF

IF(initialQLoadTotal==0.0d0)  initialQLoadTotal = NominalCoolingCapacity
IF(initialQSource==0.0d0)  initialQSource = NominalCoolingCapacity


!Loop the calculation at least twice depending whether the latent degradation model
!is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
!and 2nd iteration to calculate the  QLatent(actual)

QLatRated=0.0d0
QLatActual=0.0d0
!IF((RuntimeFrac .GE. 1.0) .OR. (Twet_rated .LE. 0.0) .OR. (Gamma_rated .LE. 0.0)) THEN
! Cycling fan does not required latent degradation model, only the constant fan case
IF((RuntimeFrac .GE. 1.0d0) .OR. (Twet_rated .LE. 0.0d0) .OR. (Gamma_rated .LE. 0.0d0) .OR. (CyclingScheme .EQ. CycFanCycCoil)) THEN
  LatDegradModelSimFlag = .FALSE.
  !Set NumIteration4=1 so that latent model would quit after 1 simulation with the actual condition
  NumIteration4=1
ELSE
  LatDegradModelSimFlag = .TRUE.
  !Set NumIteration4=0 so that latent model would simulate twice with rated and actual condition
  NumIteration4=0
END IF

LOOPLatentDegradationModel: DO
NumIteration4=NumIteration4+1
IF (NumIteration4.EQ.1) THEN
  !Set indoor air conditions to the rated condition
  LoadSideInletDBTemp  = LoadSideInletDBTemp_Init
  LoadSideInletHumRat  = LoadSideInletHumRat_Init
  LoadSideAirInletEnth = LoadSideAirInletEnth_Init
ELSE
  !Set indoor air conditions to the actual condition
  LoadSideInletDBTemp  = LoadSideInletDBTemp_Unit
  LoadSideInletHumRat  = LoadSideInletHumRat_Unit
  LoadSideAirInletEnth = LoadSideAirInletEnth_Unit
END IF

  !Outerloop: Calculate source side heat transfer
  NumIteration2=0
  Converged = .FALSE.
  FinalSimFlag = .FALSE.
  LOOPSourceEnth: DO
    IF (Converged) FinalSimFlag = .TRUE.

    NumIteration2=NumIteration2+1

    IF (NumIteration2.GT.STOP2) THEN
      WatertoAirHP(HPNum)%SimFlag = .FALSE.
      RETURN
    END IF

    !Innerloop: Calculate load side heat transfer
    NumIteration3=0
    LOOPLoadEnth: DO

      NumIteration3=NumIteration3+1

      IF (NumIteration3.GT.STOP3) THEN
        WatertoAirHP(HPNum)%SimFlag = .FALSE.
        RETURN
      END IF

      ! Determine Effectiveness of Source Side
      CpFluid=GetSpecificHeatGlycol(SourceSideFluidName,SourceSideInletTemp,SourceSideFluidIndex,  &
                                    'CalcWatertoAirHPCooling:SourceSideInletTemp')

!      IF (SourceSideFluidName=='WATER') THEN
      IF (SourceSideFluidIndex == WaterIndex) THEN  ! SourceSideFluidName=='Water'
        SourceSideEffect = 1.0d0 - EXP( -SourceSideUA / (CpFluid * SourceSideMassFlowRate))
      ELSE
        DegradFactor=DegradF(SourceSideFluidName,SourceSideInletTemp,SourceSideFluidIndex)
        SourceSideEffect = 1.0d0 / ((SourceSideHTRes1 * SourceSideVolFlowRate**(-0.8d0)) / DegradFactor + SourceSideHTRes2)
      END IF

      ! Determine Source Side Tempertaure (Condensing Temp in this case)
      SourceSideTemp = SourceSideInletTemp +initialQSource/(SourceSideEffect * CpFluid * SourceSideMassFlowRate)

      ! Compute the Effective Surface Temperature
      LoadSideEffec=1.0d0-EXP(-LoadSideoutsideUA/(LoadSideMassFlowRate*CpAir))
      EffectiveSatEnth=LoadSideAirInletEnth-initialQloadTotal/(LoadSideEffec*LoadSideMassFlowRate)

!      ! Set up the Initial Range of Effective Surface Temperature
!      IF(.NOT. Converged)THEN
!        EffectiveSurfaceTemp1=-100.
!        EffectiveSurfaceTemp2=200.
!      END IF
!
!      ! Iterate to calculate the effective surface temp from the corresponding enthalpy
!      NumIteration1=0
!      LOOP1: DO
!
!        NumIteration1=NumIteration1+1
!        IF (NumIteration1.GT.STOP1) THEN
!          WatertoAirHP(HPNum)%SimFlag = .FALSE.
!          RETURN
!        END IF
!
!        EffectiveSurfaceTemp=0.5d0*(EffectiveSurfaceTemp1+EffectiveSurfaceTemp2)
!        EffectiveSatEnth1=PsyHFnTdbRhPb(EffectiveSurfaceTemp,1.0,PB)
!
!        IF(ABS(EffectiveSatEnth-EffectiveSatEnth1).LT.0.01 .OR. &
!          ABS(EffectiveSurfaceTemp1-EffectiveSurfaceTemp2).LT.0.001) THEN
!          EXIT LOOP1
!        END IF
!
!        IF(EffectiveSatEnth1.LT.EffectiveSatEnth) THEN
!          EffectiveSurfaceTemp1=EffectiveSurfaceTemp
!        ELSE
!          EffectiveSurfaceTemp2=EffectiveSurfaceTemp
!        END IF
!      END DO LOOP1

      EffectiveSurfaceTemp = PsyTsatFnHPb(EffectiveSatEnth,PB)

      QSensible=LoadSideMassFlowRate*CpAir*(LoadSideInletDBTemp-EffectiveSurfaceTemp)*LoadSideEffec
      ANTUWET=LoadSideTotalUA/(LoadSideMassFlowRate*CpAir)
      EffectWET = 1.0d0 - EXP(-ANTUWET)
      EvapSatEnth=LoadsideAirInletEnth-initialQloadTotal/(EffectWET*LoadSideMassFlowRate)

!      ! Iterate to compute Evaporating Temperature
!      IF(.NOT. Converged)THEN
!        EvapTemp1=-150
!        EvapTemp2=100
!      END IF
!      NumIteration1=0
!      LOOP2: DO
!        NumIteration1=NumIteration1+1
!
!        IF (NumIteration1.GT.STOP1) THEN
!          WatertoAirHP(HPNum)%SimFlag = .FALSE.
!          RETURN
!        END IF
!        EvapTemp=0.5d0*(EvapTemp1+EvapTemp2)
!        EvapSatEnth1=PsyHFnTdbRhPb(EvapTemp,1.0,PB)
!        IF(ABS((EvapSatEnth-EvapSatEnth1)/EvapSatEnth).LT.ERR1) THEN
!         EXIT LOOP2
!        END IF
!        IF(EvapSatEnth1.LT.EvapSatEnth) THEN
!         EvapTemp1=EvapTemp
!        ELSE
!         EvapTemp2=EvapTemp
!        END IF
!      END DO LOOP2

      EvapTemp = PsyTsatFnHPb(EvapSatEnth,PB)

      ! Load Side Saturated Temperature (Evaporating Temp in this case)
      LoadSideTemp=EvapTemp

      ! Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
      SourceSidePressure = GetSatPressureRefrig(Refrigerant,SourceSideTemp,RefrigIndex,'CalcWatertoAirHPCooling:SourceSideTemp')
      LoadSidePressure = GetSatPressureRefrig(Refrigerant,LoadSideTemp,RefrigIndex,'CalcWatertoAirHPCooling:LoadSideTemp')

      IF (LoadSidePressure < LowPressCutOff.AND. .NOT. FirstHVACIteration) THEN
          IF (.not. WarmupFlag) THEN
            CALL ShowRecurringWarningErrorAtEnd('WaterToAir Heat pump:cooling ['//TRIM(WaterToAirHP(HPNum)%Name)//  &
                 '] shut off on low pressure < '//TRIM(RoundSigDigits(LowPressCutoff,0)),  &
                 WaterToAirHP(HPNum)%LowPressClgError,LoadSidePressure,LoadSidePressure,  &
                 ReportMinUnits='[Pa]',ReportMaxUnits='[Pa]')
          ENDIF
          WatertoAirHP(HPNum)%SimFlag = .FALSE.
          RETURN
      END IF

      IF (SourceSidePressure > HighPressCutOff .AND. .NOT. FirstHVACIteration)THEN
          IF (.not. WarmUpFlag) THEN
            CALL ShowRecurringWarningErrorAtEnd('WaterToAir Heat pump:cooling ['//TRIM(WaterToAirHP(HPNum)%Name)//  &
                 '] shut off on high pressure > '//TRIM(RoundSigDigits(HighPressCutOff,0)),  &
                 WaterToAirHP(HPNum)%HighPressClgError,SourceSideInletTemp,SourceSideInletTemp,  &
                 ReportMinUnits='SourceSideInletTemp[C]',ReportMaxUnits='SourceSideInletTemp[C]')
          ENDIF
          WatertoAirHP(HPNum)%SimFlag = .FALSE.
         RETURN
      END IF

      ! Determine Suction Pressure & Discharge Pressure at Compressor Exit
      SELECT CASE  (CompressorType)
        CASE (CompressorType_Reciprocating) ! RECIPROCATING
          SuctionPr = LoadSidePressure - PressureDrop
          DischargePr = SourceSidePressure + PressureDrop
        CASE (CompressorType_Rotary) ! ROTARY
          SuctionPr = LoadSidePressure
          DischargePr = SourceSidePressure + PressureDrop
        CASE (CompressorType_Scroll) ! SCROLL
          SuctionPr = LoadSidePressure
          DischargePr = SourceSidePressure
      END SELECT

      ! Determine the Load Side Outlet Enthalpy (Saturated Gas)
      Quality = 1.0d0
      LoadSideOutletEnth = GetSatEnthalpyRefrig(Refrigerant, LoadSideTemp, Quality,   &
         RefrigIndex,'CalcWatertoAirHPCooling:LoadSideTemp')

      ! Determine Source Side Outlet Enthalpy (Saturated Liquid)
      Quality=0.0d0
      SourceSideOutletEnth = GetSatEnthalpyRefrig(Refrigerant, SourceSideTemp, Quality,   &
         RefrigIndex,'CalcWatertoAirHPCooling:SourceSideTemp')
      ! Determine Superheated Temperature of the Load Side outlet/compressor Inlet
      CompressInletTemp = LoadSideTemp + ShTemp

      ! Determine the Enthalpy of the Superheated Fluid at Load Side Outlet/Compressor Inlet
      SuperHeatEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompressInletTemp, LoadSidePressure,   &
         RefrigIndex,'CalcWatertoAirHPCooling:CompressInletTemp')

      ! Determining the suction state of the fluid from inlet state involves interation
      ! Method employed...
      ! Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
      ! check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached
    IF(.NOT. Converged)THEN
      CompSuctionSatTemp = GetSatTemperatureRefrig(Refrigerant, SuctionPr,   &
         RefrigIndex,'CalcWatertoAirHPCooling:SuctionPr')
      CompSuctionTemp1 = CompSuctionSatTemp

      ! Shoot into the Superheated Region
      CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat
    END IF
      ! Iterate to find the Suction State
!      NumIteration1=0
!
!       LOOP: DO
!
!           NumIteration1=NumIteration1+1
!
!           IF (NumIteration1.GT.STOP1) THEN
!             WatertoAirHP(HPNum)%SimFlag = .FALSE.
!             RETURN
!           END IF
!
!               CompSuctionTemp = 0.5d0 * ( CompSuctionTemp1 + CompSuctionTemp2 )
!               CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
!               CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
!
!               IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < ERR)  THEN
!                   EXIT LOOP
!               END IF
!
!               IF ( CompsuctionEnth < SuperHeatEnth ) THEN
!                   CompSuctionTemp1 = CompSuctionTemp
!               ELSE
!                   CompSuctionTemp2 = CompSuctionTemp
!               END IF
!        END DO LOOP

!  Do not need the name of the refrigerant if we already have the index (from above CALLs)
      Par(1) = SuctionPr
      Par(2) = REAL(RefrigIndex,r64)
      Par(3) = SuperHeatEnth

      CALL SolveRegulaFalsi(ERR, STOP1, SolFlag, CompSuctionTemp, CalcCompSuctionTempResidual, &
                            CompSuctionTemp1, CompSuctionTemp2, Par)
      IF(SolFlag == -1)THEN
        WatertoAirHP(HPNum)%SimFlag = .FALSE.
        RETURN
      END IF
      CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr,   &
         RefrigIndex,'CalcWatertoAirHPCooling:CompSuctionTemp')
      CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr,   &
         RefrigIndex,'CalcWatertoAirHPCooling:CompSuctionTemp')

      ! Find Refrigerant Flow Rate
      SELECT CASE  (CompressorType)
        CASE (CompressorType_Reciprocating) ! RECIPROCATING
          MassRef = PistonDisp * CompSuctionDensity * &
                (1.0d0+ClearanceFactor-ClearanceFactor*((DischargePr/SuctionPr)**(1.0d0/gamma)))
        CASE (CompressorType_Rotary) ! ROTARY
          MassRef = PistonDisp * CompSuctionDensity
        CASE (CompressorType_Scroll) ! SCROLL
          MassRef = RefVolFlowRate * CompSuctionDensity - LeakRateCoeff * (DischargePr/SuctionPr)
      END SELECT

      ! Find the Load Side Heat Transfer
      QloadTotal = MassRef * ( LoadSideOutletEnth - SourceSideOutletEnth )

      IF(ABS(QloadTotal - initialQloadTotal)/initialQloadTotal.LT. ERR ) THEN
          EXIT LOOPLoadEnth
      ELSE
          initialQLoadTotal = initialQLoadTotal+ RelaxParam*(QloadTotal-initialQLoadTotal)
      END IF


    END DO LOOPLoadEnth

    ! Determine the Power Consumption
      SELECT CASE  (CompressorType)
        CASE (CompressorType_Reciprocating) ! RECIPROCATING
          Power = PowerLos+(1.0d0/LosFac)*(MassRef*gamma/(gamma-1.0d0) * &
                SuctionPr /CompSuctionDensity  &
                *(((DischargePr/SuctionPr)**((gamma-1.0d0)/gamma)) - 1.0d0))
        CASE (CompressorType_Rotary) ! ROTARY
          Power = PowerLos+(1.0d0/LosFac)*(MassRef*gamma/(gamma-1.0d0) * &
                SuctionPr /CompSuctionDensity  &
                *(((DischargePr/SuctionPr)**((gamma-1.0d0)/gamma)) - 1.0d0))
        CASE (CompressorType_Scroll) ! SCROLL
          Power = PowerLos+(1.0d0/LosFac)*(gamma/(gamma-1.0d0)) * SuctionPr * &
                  RefVolFlowRate * (((gamma-1.0d0) / gamma) * &
                 ((DischargePr / SuctionPr) / VolumeRatio) + ((1.0d0/gamma) * &
                 VolumeRatio **(gamma-1.0d0)) - 1.0d0)
      END SELECT

    ! Determine the Sourceside Heat Rate
    QSource = Power + QLoadTotal

    IF(ABS(QSource - initialQSource)/initialQSource.LT. ERR) THEN
      Converged = .TRUE.
    ELSE
      initialQSource= initialQSource+ RelaxParam*(QSource-initialQSource)
    END IF

    IF (FinalSimFlag)  EXIT LOOPSourceEnth

  END DO LOOPSourceEnth


  IF (SuctionPr < LowPressCutOff) THEN
    CALL ShowWarningError('Heat pump:cooling shut down on low pressure')
    WatertoAirHP(HPNum)%SimFlag = .FALSE.
  END IF

  IF (DischargePr > HighPressCutOff.AND. .NOT. FirstHVACIteration)THEN
    CALL ShowWarningError('Heat pump:cooling shut down on high pressure')
    WatertoAirHP(HPNum)%SimFlag = .FALSE.
  END IF


  IF(QSensible.GT.QLoadTotal) THEN
     QSensible = QLoadTotal
  END IF

  IF(LatDegradModelSimFlag) THEN
    IF(NumIteration4.EQ.1) THEN
    QLatRated=QLoadTotal-QSensible

    ELSEIF(NumIteration4.EQ.2) THEN
    QLatActual=QLoadTotal-QSensible
    SHRss=QSensible/QLoadTotal
    LoadSideInletWBTemp=PsyTwbFnTdbWPb(LoadSideInletDBTemp,LoadSideInletHumRat,PB)
    SHReff = CalcEffectiveSHR(HPNum, SHRss,CyclingScheme, RuntimeFrac, &
                          QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp)
!   Update sensible capacity based on effective SHR
    QSensible = QLoadTotal * SHReff
    EXIT LOOPLatentDegradationModel
    END IF
  ELSE

    SHReff = QSensible/QLoadTotal
    EXIT LOOPLatentDegradationModel
  END IF
  END DO LOOPLatentDegradationModel

   !calculate coil outlet state variables
   LoadSideAirOutletEnth= LoadSideAirInletEnth - QLoadTotal/LoadSideMassFlowRate
   LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible/(LoadSideMassFlowRate * CpAir)
   LoadsideOutletHumRat =  PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideAirOutletEnth)
   SourceSideOutletTemp = SourceSideInletTemp + QSource/(SourceSideMassFlowRate * CpWater)

    ! Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    WatertoAirHP(HPNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideAirOutletEnth + &
                                          (1.0d0-PartLoadRatio)*LoadSideAirInletEnth
    WatertoAirHP(HPNum)%OutletAirHumRat =PartLoadRatio*LoadsideOutletHumRat + &
                                          (1.0d0-PartLoadRatio)*LoadSideInletHumRat
    WatertoAirHP(HPNum)%OutletAirDBTemp = PsyTdbFnHW(WatertoAirHP(HPNum)%OutletAirEnthalpy,WatertoAirHP(HPNum)%OutletAirHumRat)
  ELSE
    ! default to cycling fan, cycling compressor
    WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth
    WatertoAirHP(HPNum)%OutletAirHumRat = LoadsideOutletHumRat
    WatertoAirHP(HPNum)%OutletAirDBTemp = LoadSideOutletDBTemp
  END IF

   !scale heat transfer rates and power to run time
   QLoadTotal= QLoadTotal*PartLoadRatio
   QSensible = QSensible*PartLoadRatio
   Power = Power*RuntimeFrac
   QSource = QSource*PartLoadRatio

 !Update heat pump data structure
  WatertoAirHP(HPNum)%Power=Power
  WatertoAirHP(HPNum)%QLoadTotal=QLoadTotal
  WatertoAirHP(HPNum)%QSensible=QSensible
  WatertoAirHP(HPNum)%QLatent=QLoadTotal - QSensible
  WatertoAirHP(HPNum)%QSource=QSource
  WatertoAirHP(HPNum)%RunFrac = RuntimeFrac
  WatertoAirHP(HPNum)%PartLoadRatio = PartLoadRatio

!  Air-side outlet conditions are already calculated above
!  WatertoAirHP(HPNum)%OutletAirDBTemp=LoadSideOutletDBTemp
!  WatertoAirHP(HPNum)%OutletAirHumRat=LoadsideOutletHumRat
!  WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth

  WatertoAirHP(HPNum)%OutletAirMassFlowRate=LoadSideMassFlowRate
  WatertoAirHP(HPNum)%OutletWaterTemp=SourceSideOutletTemp
  WatertoAirHP(HPNum)%OutletWaterMassFlowRate=SourceSideMassFlowRate
  WatertoAirHP(HPNum)%OutletWaterEnthalpy = SourceSideWaterInletEnth + &
        QSource/SourceSideMassFlowRate

  RETURN
  END SUBROUTINE CalcWatertoAirHPCooling

FUNCTION CalcCompSuctionTempResidual(CompSuctionTemp, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the compressor suction temperature for water to air HP's

          ! METHODOLOGY EMPLOYED:
          ! Use SolveRegulaFalsi to call this Function to converge on a solution

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: GetSupHeatEnthalpyRefrig, GetSupHeatDensityRefrig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                             :: CompSuctionTemp  ! HP compressor suction temperature (C)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL     :: Par              ! Function parameters
  REAL(r64)                                    :: Residuum         ! Result (force to 0)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: Refrigerant           ! Name of refrigerant
  REAL(r64)  :: SuctionPR
  INTEGER    :: RefrigIndex
  REAL(r64)  :: CompSuctionEnth
  REAL(r64)  :: SuperHeatEnth

! Convert parameters to usable variables
  Refrigerant  = ''
  SuctionPr    = Par(1)
  RefrigIndex  = INT(Par(2))
  SuperHeatEnth = Par(3)

  CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr,   &
          RefrigIndex,'CalcWaterToAirHPHeating:CalcCompSuctionTemp')

! Calculate residual based on output calculation flag
  Residuum = (CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth

RETURN
END FUNCTION CalcCompSuctionTempResidual

SUBROUTINE CalcWatertoAirHPHeating(HPNum,CyclingScheme,FirstHVACIteration,RuntimeFrac,InitFlag, SensDemand, CompOp, PartLoadRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hui Jin
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       R. Raustad (Oct 2006) Revised iteration technique
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a parameter estimation based water to air heat pump model

          ! USE STATEMENTS:
          ! na

  USE FluidProperties
  USE Psychrometrics,       ONLY:PsyCpAirFnWTdb,PsyTdbFnHW,PsyWFnTdbH !,PsyHFnTdbRhPb,PsyWFnTdpPb
!  USE DataZoneEnergyDemands
  USE General,              ONLY: RoundSigDigits, SolveRegulaFalsi
  USE InputProcessor,       ONLY: SameString
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN) :: HPNum                 !heat pump number
   REAL(r64)   , INTENT(IN) :: RuntimeFrac
   REAL(r64)   , INTENT(IN) :: SensDemand
   INTEGER             :: CyclingScheme         !fan/compressor cycling scheme indicator
   LOGICAL, INTENT(IN) :: FirstHVACIteration    !first iteration flag
   LOGICAL, INTENT(IN) :: InitFlag              !first iteration flag
   INTEGER, INTENT(IN) :: CompOp
   REAL(r64), INTENT(IN) :: PartLoadRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
      REAL(r64), PARAMETER        :: CpWater=4210.d0             ! Specific heat of water J/kg_C
      REAL(r64), PARAMETER        :: DegreeofSuperheat=80.d0    ! Initial guess of degree of superheat
      REAL(r64), PARAMETER        :: gamma= 1.114d0             ! Expnasion Coefficient
      REAL(r64), PARAMETER        :: RelaxParam = .5d0          ! Relaxation Parameter
      REAL(r64), PARAMETER        :: ERR=0.01d0                 ! Error Value
      REAL(r64), PARAMETER        :: ERR1=0.01d0                ! Error Value
      REAL(r64), PARAMETER        :: PB=1.013d5               ! Barometric Pressure (Pa)
      INTEGER, PARAMETER     :: STOP1=10000              ! Iteration stopper1
      INTEGER, PARAMETER     :: STOP2=100000             ! Iteration stopper2
      INTEGER, PARAMETER     :: STOP3=100000             ! Iteration stopper3

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!      INTEGER                :: NumIteration1            ! Number of Iteration1
      INTEGER                :: NumIteration2            ! Number of Iteration2
      INTEGER                :: NumIteration3            ! Number of Iteration3
      INTEGER                :: SourceSideFluidIndex     ! Source Side Fluid Index

      INTEGER :: CompressorType           ! Type of Compressor ie. Reciprocating,Rotary or Scroll
      CHARACTER(len=MaxNameLength) :: SourceSideFluidName   ! Name of source side fluid
      CHARACTER(len=MaxNameLength) :: Refrigerant           ! Name of refrigerant
!      CHARACTER(len=25) :: CErrCount
      REAL(r64)              :: NominalHeatingCapacity   ! Nominal Heating Capacity(W)
      REAL(r64)              :: LoadSideUA               ! Load Side Heat Transfer coefficient [W/C]
      REAL(r64)              :: SourceSideUA             ! Source Side Heat Transfer coefficient [W/C]
      REAL(r64)              :: PressureDrop             ! Suction or Discharge Pressure Drop [Pa]
      REAL(r64)              :: ClearanceFactor          ! Compressor Clearance Factor
      REAL(r64)              :: PistonDisp               ! Compressor Piston Displacement [m3/s]
      REAL(r64)              :: ShTemp                   ! Superheat Temperature [C]
      REAL(r64)              :: LosFac                   ! Compressor Power Loss Factor
      REAL(r64)              :: PowerLos                 ! Constant Part of Power Losses [kW]
      REAL(r64)              :: RefVolFlowRate           ! Refrigerant Volume Flow rate at the beginning
      REAL(r64)              :: VolumeRatio              ! Built-in-volume ratio [~]
      REAL(r64)              :: LeakRateCoeff            ! Coefficient for the relationship between
                                                         ! Pressure Ratio and Leakage Rate [~]
      REAL(r64)              :: SourceSideHTRes1         ! Source Side Heat Transfer Resistance coefficient 1 [~]
      REAL(r64)              :: SourceSideHTRes2         ! Source Side Heat Transfer Resistance coefficient 2 [K/kW]
      REAL(r64)              :: HighPressCutOff          ! High Pressure Cut-off [Pa]
      REAL(r64)              :: LowPressCutOff           ! Low Pressure Cut-off [Pa]

      REAL(r64)              :: Quality
      REAL(r64)              :: SourceSideMassFlowRate   ! Source Side Mass Flow Rate [kg/s]
      REAL(r64)              :: SourceSideInletTemp      ! Source Side Inlet Temperature [C]
      REAL(r64)              :: SourceSideWaterInletEnth ! Source Side Inlet Water Enthalpy [J/kg]
      REAL(r64)              :: SourceSideoutletTemp     ! Source Side Outlet Temperature [C]
      REAL(r64)              :: SourceSideVolFlowRate    ! Source Side Volumetric Flow Rate [m3/s]
      REAL(r64)              :: CpFluid                  ! Specific heat of source side fluid(J/kg)
      REAL(r64)              :: LoadSideMassFlowRate     ! Load Side Mass Flow Rate [kg/s]
      REAL(r64)              :: LoadSideInletDBTemp      ! Load Side Inlet Dry Bulb Temp [C]
      REAL(r64)              :: LoadSideInletHumRat      ! Load Side Inlet Humidity Ratio [kg/kg]
      REAL(r64)              :: LoadSideoutletDBTemp     ! Load Side Outlet Dry Bulb Temperature [C]
      REAL(r64)              :: LoadsideOutletHumRat     ! Load Side Outlet Humidity Ratio [kg/kg]
      REAL(r64)              :: LoadSideAirInletEnth     ! Load Side Inlet Enthalpy [J/kg]
      REAL(r64)              :: LoadSideAirOutletEnth    ! Load Side Outlet Enthalpy [J/kg]
      REAL(r64)              :: CpAir                    ! Specific Heat of Air [J/kg_C]
      REAL(r64)              :: DegradFactor             ! Degradation Factor [~]
      REAL(r64)              :: QSource                  ! Source Side Heat Transfer Rate [W]
      REAL(r64)              :: QLoadTotal               ! Load Side Heat Transfer Rate [W]
      REAL(r64)              :: Power                    ! Power Consumption [W]

      REAL(r64)              :: SourceSideEffect         ! Source Side Heat Exchanger Effectiveness
      REAL(r64)              :: LoadSideEffect           ! Load Side Effectiveness based on Outside Heat Transfer Coefficient
      REAL(r64)              :: SourceSideTemp           ! Source Side Saturated Refrigerant Temperature [C]
      REAL(r64)              :: LoadSideTemp             ! Load Side Saturated Refrigerant Temperature [C]
      REAL(r64)              :: SourceSidePressure       ! Source Side Saturated Refrigerant Pressure [Pa]
      REAL(r64)              :: LoadSidePressure         ! Load Side Saturated Refrigerant Pressure [Pa]
      REAL(r64)              :: SuctionPr                ! Compressor Suction Pressure [Pa]
      REAL(r64)              :: DischargePr              ! Compressor Discharge Pressure [Pa]
      REAL(r64)              :: CompressInletTemp        ! Temperature of the Refrigerant Entering the Compressor [C]
      REAL(r64)              :: MassRef                  ! Mass Flow Rate of Refrigerant [kg/s]
      REAL(r64)              :: SourceSideOutletEnth     ! Enthalpy of Refrigerant leaving the Source Side Heat Exchanger [J/kg]
      REAL(r64)              :: LoadSideOutletEnth       ! Enthalpy of Refrigerant leaving the Load Side Heat Exchanger [J/kg]
      REAL(r64),SAVE              :: initialQSource           ! Guess Source Side Heat Transfer Rate [W]
      REAL(r64),SAVE              :: initialQLoad             ! Guess Load Side Heat Transfer rate [W]
      REAL(r64)              :: SuperHeatEnth            ! Enthalpy of the Superheated Refrigerant [J/kg]
      REAL(r64)              :: CompSuctionTemp1         ! Guess of the Temperature of the Refrigerant Entering the
                                                         ! Compressor #1 [C]
      REAL(r64)              :: CompSuctionTemp2         ! Guess of the Temperature of the Refrigerant Entering the
                                                         ! Compressor #2 [C]
      REAL(r64)              :: CompSuctionTemp          ! Temperature of the Refrigerant Entering the Compressor [C]
      REAL(r64)              :: CompSuctionEnth          ! Enthalpy of the Refrigerant Entering the Compressor [J/kg]
      REAL(r64)              :: CompSuctionDensity       ! Density of the Refrigerant Entering the Compressorkg/m3
      REAL(r64)              :: CompSuctionSatTemp       ! Temperature of Saturated Refrigerant at Compressor Suction Pressure [C]
      LOGICAL                :: FinalSimFlag             ! Final Simulation Flag
      LOGICAL                :: Converged                ! Overall convergence Flag
      REAL(r64)              :: Par(4)                   ! Parameter array passed to RegulaFalsi function
      INTEGER                :: SolFlag                  ! Solution flag returned from RegulaFalsi function


 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

    NominalHeatingCapacity = WatertoAirHP(HPNum)%HeatingCapacity
    CompressorType = WatertoAirHP(HPNum)%CompressorType
    Refrigerant = WatertoAirHP(HPNum)%Refrigerant

    LoadSideUA        = WatertoAirHP(HPNum)%LoadSideTotalUACoeff
    PistonDisp        = WatertoAirHP(HPNum)%CompPistonDisp
    ClearanceFactor   = WatertoAirHP(HPNum)%CompClearanceFactor
    PressureDrop      = WatertoAirHP(HPNum)%CompSucPressDrop
    ShTemp            = WatertoAirHP(HPNum)%SuperheatTemp
    PowerLos          = WatertoAirHP(HPNum)%PowerLosses
    LosFac            = WatertoAirHP(HPNum)%LossFactor
    RefVolFlowRate    = WatertoAirHP(HPNum)%RefVolFlowRate
    VolumeRatio       = WatertoAirHP(HPNum)%VolumeRatio
    LeakRateCoeff     = WatertoAirHP(HPNum)%LeakRateCoeff
    HighPressCutOff   = WatertoAirHP(HPNum)%HighPressCutOff
    LowPressCutOff    = WatertoAirHP(HPNum)%LowPressCutOff
    SourceSideUA             = WatertoAirHP(HPNum)%SourceSideUACoeff
    SourceSideHTRes1         = WatertoAirHP(HPNum)%SourceSideHTR1
    SourceSideHTRes2         = WatertoAirHP(HPNum)%SourceSideHTR2

    LoadSideMassFlowRate    = WatertoAirHP(HPNum)%InletAirMassFlowRate
    LoadsideInletDBTemp     = WatertoAirHP(HPNum)%InletAirDBTemp
    LoadsideInletHumRat     = WatertoAirHP(HPNum)%InletAirHumRat
    cpair = PsyCpAirFnWTdb(LoadSideInletHumRat,LoadSideInletDBTemp)

    SourceSideInletTemp      = WatertoAirHP(HPNum)%InletWaterTemp
    SourceSideWaterInletEnth = WatertoAirHP(HPNum)%InletWaterEnthalpy
    SourceSideFluidName      = PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidName
    SourceSideFluidIndex     = PlantLoop(WatertoAirHP(HPNum)%LoopNum)%FluidIndex
    SourceSideMassFlowRate   = WatertoAirHP(HPNum)%InletWaterMassFlowRate
    SourceSideVolFlowRate    = WatertoAirHP(HPNum)%InletWaterMassFlowRate / GetDensityGlycol(SourceSideFluidName, &
                                                                             SourceSideInletTemp,SourceSideFluidIndex,  &
                                                                            'CalcWatertoAirHPHeating:SourceSideInletTemp')

! Load Side Inlet Air Enthalpy
    LoadSideAirInletEnth=WatertoAirHP(HPNum)%InletAirEnthalpy

! If heat pump is not operating, return
IF (SensDemand == 0.0d0 .OR. LoadSideMassFlowRate <= 0.0d0 .OR. SourceSideMassFlowRate <= 0.0d0)THEN
   WatertoAirHP(HPNum)%SimFlag = .FALSE.
   RETURN
ELSE
   WatertoAirHP(HPNum)%SimFlag = .TRUE.
ENDIF

IF (CompOp .EQ. 0) THEN
   WaterToAirHP(HPNum)%SimFlag = .FALSE.
   RETURN
ENDIF

IF(FirstHVACIteration) THEN
  initialQLoad = NominalHeatingCapacity
  initialQSource = NominalHeatingCapacity
END IF

IF(initialQLoad==0.0d0)  initialQLoad = NominalHeatingCapacity
IF(initialQSource==0.0d0)  initialQSource = NominalHeatingCapacity

 !Outerloop: calculate load side heat transfer
  NumIteration3=0
  Converged = .FALSE.
  FinalSimFlag = .FALSE.
  LOOPLoadEnth: DO
    IF (Converged) FinalSimFlag = .TRUE.

    NumIteration3=NumIteration3+1

    IF (NumIteration3.GT.STOP3) THEN
      WatertoAirHP(HPNum)%SimFlag = .FALSE.
      RETURN
    END IF

    !Innerloop: calculate load side heat transfer
    NumIteration2=0
    LOOPSourceEnth: DO

       NumIteration2=NumIteration2+1

       IF (NumIteration2.GT.STOP2) THEN
         WatertoAirHP(HPNum)%SimFlag = .FALSE.
         RETURN
       END IF

      ! Determine Effectiveness of Source Side
      CpFluid=GetSpecificHeatGlycol(SourceSideFluidName,SourceSideInletTemp,SourceSideFluidIndex,  &
                                   'CalcWatertoAirHPHeating:SourceSideInletTemp')

!      IF (SourceSideFluidName=='WATER') THEN
      IF (SourceSideFluidIndex== WaterIndex) THEN
        SourceSideEffect = 1.0d0- EXP( -SourceSideUA / (CpFluid * SourceSideMassFlowRate))  ! SourceSideFluidName=='Water'
      ELSE
        DegradFactor=DegradF(SourceSideFluidName,SourceSideInletTemp,SourceSideFluidIndex)
        SourceSideEffect = 1 / ((SourceSideHTRes1 * SourceSideVolFlowRate**(-0.8d0)) / DegradFactor + SourceSideHTRes2)
      END IF

       ! Determine Load Side Effectiveness
       LoadSideEffect = 1.0d0- EXP( -LoadSideUA / (CpAir * LoadSideMassFlowRate))


       ! Determine Source Side Tempertaure (Evap. Temp for this mode)
       SourceSideTemp = SourceSideInletTemp - initialQSource/(SourceSideEffect * CpFluid * SourceSideMassFlowRate)

       ! Determine Load Side Tempertaure (Condensing Temp for this mode)
       LoadSideTemp = LoadSideInletDBTemp + initialQLoad/(LoadSideEffect * CpAir * LoadSideMassFlowRate)

       ! Determine the Load Side and Source Side Saturated Temp (evaporating and condensing pressures)
       SourceSidePressure = GetSatPressureRefrig(Refrigerant,SourceSideTemp,RefrigIndex,'CalcWatertoAirHPHeating:SourceSideTemp')
       LoadSidePressure = GetSatPressureRefrig(Refrigerant,LoadSideTemp,RefrigIndex,'CalcWatertoAirHPHeating:LoadSideTemp')
       IF (SourceSidePressure < LowPressCutOff.AND. .NOT. FirstHVACIteration) THEN
         IF (.not. WarmupFlag) THEN
           CALL ShowRecurringWarningErrorAtEnd('WaterToAir Heat pump:heating ['//TRIM(WaterToAirHP(HPNum)%Name)//  &
                '] shut off on low pressure < '//TRIM(RoundSigDigits(LowPressCutoff,0)),  &
                WaterToAirHP(HPNum)%LowPressHtgError,SourceSidePressure,SourceSidePressure,  &
                 ReportMinUnits='[Pa]',ReportMaxUnits='[Pa]' )
         ENDIF
         WatertoAirHP(HPNum)%SimFlag = .FALSE.
         RETURN
       END IF

       IF (LoadSidePressure > HighPressCutOff.AND. .NOT. FirstHVACIteration)THEN
          IF (.not. WarmUpFlag) THEN
            CALL ShowRecurringWarningErrorAtEnd('WaterToAir Heat pump:heating ['//TRIM(WaterToAirHP(HPNum)%Name)//  &
                 '] shut off on high pressure > '//TRIM(RoundSigDigits(HighPressCutOff,0)),  &
                 WaterToAirHP(HPNum)%HighPressHtgError,SourceSideInletTemp,SourceSideInletTemp,  &
                 ReportMinUnits='SourceSideInletTemp[C]',ReportMaxUnits='SourceSideInletTemp[C]')
          ENDIF
!         CALL ShowWarningError('Heat pump:heating shut off on high pressure')
!         WRITE(CErrCount,*) SourceSideInletTemp
!         CErrCount=ADJUSTL(CErrCount)
!         CALL ShowContinueError('Source side inlet temperature too low, T='//TRIM(CErrCount))
!         CALL ShowContinueError('Heat pump heating demand not met by plant side')
         WatertoAirHP(HPNum)%SimFlag = .FALSE.
         RETURN
       END IF

       ! Determine Suction Pressure at Compressor Entrance & Discharge Pressure at Compressor Exit
      SELECT CASE  (CompressorType)
        CASE (CompressorType_Reciprocating) ! RECIPROCATING
          SuctionPr = SourceSidePressure - PressureDrop
          DischargePr = LoadSidePressure + PressureDrop
        CASE (CompressorType_Rotary) ! ROTARY
          SuctionPr = SourceSidePressure
           DischargePr = LoadSidePressure + PressureDrop
        CASE (CompressorType_Scroll) ! SCROLL
          SuctionPr = SourceSidePressure
           DischargePr = LoadSidePressure
       END SELECT

       ! Determine the Source Side Outlet Enthalpy
       ! Quality of the refrigerant leaving the evaporator is saturated gas
       Quality = 1.0d0
       SourceSideOutletEnth = GetSatEnthalpyRefrig(Refrigerant, SourceSideTemp, Quality,   &
            RefrigIndex,'CalcWatertoAirHPHeating:SourceSideTemp')

       ! Determine Load Side Outlet Enthalpy
       ! Quality of the refrigerant leaving the condenser is saturated liguid
       Quality = 0.0d0
       LoadSideOutletEnth = GetSatEnthalpyRefrig(Refrigerant, LoadSideTemp, Quality,    &
            RefrigIndex,'CalcWatertoAirHPHeating:LoadSideTemp')


       ! Determine Superheated Temperature of the Source Side outlet/compressor Inlet
       CompressInletTemp = SourceSideTemp + ShTemp

       ! Determine the Enathalpy of the Superheated Fluid at Source Side Outlet/Compressor Inlet
       SuperHeatEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompressInletTemp, SourceSidePressure,   &
            RefrigIndex,'CalcWatertoAirHPHeating:CompressInletTemp')

       ! Determining the suction state of the fluid from inlet state involves interation
       ! Method employed...
       ! Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
       ! check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

       IF(.NOT. converged)THEN
         CompSuctionSatTemp = GetSatTemperatureRefrig(Refrigerant, SuctionPr, RefrigIndex,'CalcWatertoAirHPHeating:SuctionPr')
         CompSuctionTemp1 = CompSuctionSatTemp

         ! Shoot into the Superheated Region
         CompSuctionTemp2 = CompSuctionSatTemp + DegreeofSuperheat
       END IF

!       ! Iterate to find the Suction State
!       NumIteration1=0
!
!       LOOP: DO
!
!           NumIteration1=NumIteration1+1
!
!           IF (NumIteration1.GT.STOP1) THEN
!             WatertoAirHP(HPNum)%SimFlag = .FALSE.
!             RETURN
!           END IF
!
!               CompSuctionTemp = 0.5d0 * ( CompSuctionTemp1 + CompSuctionTemp2 )
!               CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
!               CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr, RefrigIndex)
!
!               IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < ERR)  THEN
!                   EXIT LOOP
!               END IF
!
!               IF ( CompsuctionEnth < SuperHeatEnth ) THEN
!                   CompSuctionTemp1 = CompSuctionTemp
!               ELSE
!                   CompSuctionTemp2 = CompSuctionTemp
!               END IF
!        END DO LOOP

!       Do not need the name of the refrigerant if we already have the index (from above CALLs)
        Par(1) = SuctionPr
        Par(2) = REAL(RefrigIndex,r64)
        Par(3) = SuperHeatEnth

        CALL SolveRegulaFalsi(ERR, STOP1, SolFlag, CompSuctionTemp, CalcCompSuctionTempResidual, &
                              CompSuctionTemp1, CompSuctionTemp2, Par)
        IF(SolFlag == -1)THEN
          WatertoAirHP(HPNum)%SimFlag = .FALSE.
          RETURN
        END IF
        CompSuctionEnth = GetSupHeatEnthalpyRefrig(Refrigerant, CompSuctionTemp, SuctionPr,   &
           RefrigIndex,'CalcWatertoAirHPHeating:CompSuctionTemp')
        CompSuctionDensity = GetSupHeatDensityRefrig(Refrigerant, CompSuctionTemp, SuctionPr,   &
           RefrigIndex,'CalcWatertoAirHPHeating:CompSuctionTemp')

        ! Find Refrigerant Flow Rate
        SELECT CASE  (CompressorType)
          CASE (CompressorType_Reciprocating) ! RECIPROCATING
            MassRef = PistonDisp * CompSuctionDensity * &
                (1+ClearanceFactor-ClearanceFactor*((DischargePr/SuctionPr)**(1/gamma)))
          CASE (CompressorType_Rotary) ! ROTARY
            MassRef = PistonDisp * CompSuctionDensity
          CASE (CompressorType_Scroll) ! SCROLL
            MassRef = RefVolFlowRate * CompSuctionDensity - LeakRateCoeff * (DischargePr/SuctionPr)
        END SELECT

        ! Find the Source Side Heat Transfer
        QSource = MassRef * ( SourceSideOutletEnth - LoadSideOutletEnth )

        IF(ABS(QSource - initialQSource)/initialQSource.LT. ERR) THEN
            EXIT LOOPSourceEnth
        ELSE
            initialQSource= initialQSource+ RelaxParam*(QSource-initialQSource)
        END IF

    END DO LOOPSourceEnth

    ! Determine the Power Consumption
      SELECT CASE  (CompressorType)
        CASE (CompressorType_Reciprocating) ! RECIPROCATING
          Power = PowerLos+(1/LosFac)*(MassRef*gamma/(gamma-1) * &
                SuctionPr /CompSuctionDensity  &
                *(((DischargePr/SuctionPr)**((gamma-1)/gamma)) - 1))
        CASE (CompressorType_Rotary) ! ROTARY
          Power = PowerLos+(1/LosFac)*(MassRef*gamma/(gamma-1) * &
              SuctionPr /CompSuctionDensity  &
              *(((DischargePr/SuctionPr)**((gamma-1)/gamma)) - 1))
        CASE (CompressorType_Scroll) ! SCROLL
          Power = PowerLos+(1/LosFac)*(gamma/(gamma-1)) * SuctionPr * &
              RefVolFlowRate * (((gamma - 1) / gamma) * &
             ((DischargePr / SuctionPr) / VolumeRatio) + ((1 / gamma) * &
             VolumeRatio **(gamma - 1)) - 1)
      END SELECT

    ! Determine the Load Side Heat Rate
    QLoadTotal = Power + QSource

    IF(ABS(QLoadTotal - initialQLoad)/initialQLoad.LT. ERR) THEN
      Converged = .TRUE.
    ELSE
        initialQLoad= initialQLoad+ RelaxParam*(QLoadTotal-initialQLoad)
    END IF

    IF (FinalSimFlag)  EXIT LOOPLoadEnth
  END DO LOOPLoadEnth

  IF (SuctionPr < LowPressCutOff.AND. .NOT. FirstHVACIteration) THEN
              CALL ShowWarningError('Heat pump:heating shut down on low pressure')
              WatertoAirHP(HPNum)%SimFlag = .FALSE.
              RETURN
  END IF


  IF (DischargePr > HighPressCutOff.AND. .NOT. FirstHVACIteration)THEN
              CALL ShowWarningError('Heat pump:heating shut down on high pressure')
              WatertoAirHP(HPNum)%SimFlag = .FALSE.
              RETURN
   END IF

   !calculate coil outlet state variables
   LoadSideAirOutletEnth= LoadSideAirInletEnth + QLoadTotal/LoadSideMassFlowRate
   LoadSideOutletDBTemp = LoadSideInletDBTemp + QLoadTotal/(LoadSideMassFlowRate * CpAir)
   LoadsideOutletHumRat =  PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideAirOutletEnth)
   SourceSideOutletTemp = SourceSideInletTemp - QSource/(SourceSideMassFlowRate * CpWater)

    ! Calculate actual outlet conditions for the run time fraction
    ! Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    WatertoAirHP(HPNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideAirOutletEnth + &
                                          (1.0d0-PartLoadRatio)*LoadSideAirInletEnth
    WatertoAirHP(HPNum)%OutletAirHumRat = PartLoadRatio*LoadsideOutletHumRat + &
                                          (1.0d0-PartLoadRatio)*LoadsideInletHumRat
    WatertoAirHP(HPNum)%OutletAirDBTemp = PsyTdbFnHW(WatertoAirHP(HPNum)%OutletAirEnthalpy,WatertoAirHP(HPNum)%OutletAirHumRat)
  ELSE
    ! default to cycling fan, cycling compressor
    WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth
    WatertoAirHP(HPNum)%OutletAirHumRat = LoadsideOutletHumRat
    WatertoAirHP(HPNum)%OutletAirDBTemp = LoadSideOutletDBTemp
  END IF
   !scale heat transfer rates and power to run time
   QLoadTotal= QLoadTotal*PartLoadRatio
   Power = Power*RuntimeFrac
   QSource = QSource*PartLoadRatio

 !Update heat pump data structure
  WatertoAirHP(HPNum)%Power=Power
  WatertoAirHP(HPNum)%QLoadTotal=QLoadTotal
  WatertoAirHP(HPNum)%QSensible=QLoadTotal

  WatertoAirHP(HPNum)%QSource=QSource
  WatertoAirHP(HPNum)%RunFrac = RuntimeFrac
  WatertoAirHP(HPNum)%PartLoadRatio = PartLoadRatio

!  Air-side outlet conditions are already calculated above
!  WatertoAirHP(HPNum)%OutletAirDBTemp=LoadSideOutletDBTemp
!  WatertoAirHP(HPNum)%OutletAirHumRat=LoadsideOutletHumRat
!  WatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideAirOutletEnth

  WatertoAirHP(HPNum)%OutletAirMassFlowRate=LoadSideMassFlowRate
  WatertoAirHP(HPNum)%OutletWaterTemp=SourceSideOutletTemp
  WatertoAirHP(HPNum)%OutletWaterMassFlowRate=SourceSideMassFlowRate
  WatertoAirHP(HPNum)%OutletWaterEnthalpy = SourceSideWaterInletEnth- &
        QSource/SourceSideMassFlowRate

  RETURN
  END SUBROUTINE CalcWatertoAirHPHeating

! End Algorithm Section of the Module
! *****************************************************************************


! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the WatertoAirHP Module
! *****************************************************************************

SUBROUTINE UpdateWatertoAirHP(HPNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Hui Jin
          !       DATE WRITTEN   Oct 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the Water to Air Heat Pump outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the HP data structure to the HP outlet nodes.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE PlantUtilities,         ONLY: SafeCopyPlantNode
  USE DataHVACGlobals,        ONLY: TimeStepSys
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(In) :: HPNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER              :: AirInletNode
  INTEGER              :: WaterInletNode
  INTEGER              :: AirOutletNode
  INTEGER              :: WaterOutletNode
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour
 !WatertoAirHP(HPNum)%Simflag=.FALSE.
  IF(.NOT. WatertoAirHP(HPNum)%Simflag)THEN
    ! Heatpump is off; just pass through conditions
    WatertoAirHP(HPNum)%Power=0.0d0
    WatertoAirHP(HPNum)%Energy=0.0d0
    WatertoAirHP(HPNum)%QLoadTotal=0.0d0
    WatertoAirHP(HPNum)%QSensible=0.0d0
    WatertoAirHP(HPNum)%QLatent=0.0d0
    WatertoAirHP(HPNum)%QSource=0.0d0
  ! These will be overwritten below based on variables above that are already set to 0.
  !  WatertoAirHP(HPNum)%EnergyLoadTotal=0.0
  !  WatertoAirHP(HPNum)%EnergySensible=0.0
  !  WatertoAirHP(HPNum)%EnergySource=0.0
  !  WatertoAirHP(HPNum)%EnergyLatent=0.0
    WatertoAirHP(HPNum)%RunFrac=0.0d0
    WatertoAirHP(HPNum)%PartLoadRatio=0.0d0
    WatertoAirHP(HPNum)%OutletAirDBTemp=WatertoAirHP(HPNum)%InletAirDBTemp
    WatertoAirHP(HPNum)%OutletAirHumRat=WatertoAirHP(HPNum)%InletAirHumRat
    WatertoAirHP(HPNum)%OutletWaterTemp=WatertoAirHP(HPNum)%InletWaterTemp
    WatertoAirHP(HPNum)%OutletAirMassFlowRate=WatertoAirHP(HPNum)%InletAirMassFlowRate
    WatertoAirHP(HPNum)%OutletWaterMassFlowRate=WatertoAirHP(HPNum)%InletWaterMassFlowRate
    WatertoAirHP(HPNum)%OutletAirEnthalpy = WatertoAirHP(HPNum)%InletAirEnthalpy
    WatertoAirHP(HPNum)%OutletWaterEnthalpy = WatertoAirHP(HPNum)%InletWaterEnthalpy
  END IF

  AirInletNode    = WatertoAirHP(HPNum)%AirInletNodeNum
  WaterInletNode  = WatertoAirHP(HPNum)%WaterInletNodeNum
  AirOutletNode   = WatertoAirHP(HPNum)%AirOutletNodeNum
  WaterOutletNode = WatertoAirHP(HPNum)%WaterOutletNodeNum


   ! Set the outlet air nodes of the WatertoAirHP
  Node(AirOutletNode)%MassFlowRate = Node(AirInletNode)%MassFlowRate
  Node(AirOutletNode)%Temp         = WatertoAirHP(HPNum)%OutletAirDBTemp
  Node(AirOutletNode)%HumRat       = WatertoAirHP(HPNum)%OutletAirHumRat
  Node(AirOutletNode)%Enthalpy     = WatertoAirHP(HPNum)%OutletAirEnthalpy

   ! Set the outlet nodes for properties that just pass through & not used
  CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)
   ! Set the outlet water nodes for the heat pump
  Node(WaterOutletNode)%Temp         = WatertoAirHP(HPNum)%OutletWaterTemp
  Node(WaterOutletNode)%Enthalpy     = WatertoAirHP(HPNum)%OutletWaterEnthalpy

     ! Set the outlet nodes for properties that just pass through & not used
  Node(AirOutletNode)%Quality             = Node(AirInletNode)%Quality
  Node(AirOutletNode)%Press               = Node(AirInletNode)%Press
  Node(AirOutletNode)%MassFlowRateMin     = Node(AirInletNode)%MassFlowRateMin
  Node(AirOutletNode)%MassFlowRateMax     = Node(AirInletNode)%MassFlowRateMax
  Node(AirOutletNode)%MassFlowRateMinAvail= Node(AirInletNode)%MassFlowRateMinAvail
  Node(AirOutletNode)%MassFlowRateMaxAvail= Node(AirInletNode)%MassFlowRateMaxAvail

     ! Pass through the load side mass flow rates
  WaterToAirHP(HPNum)%InletAirMassFlowRate = Node(AirInletNode)%MassFlowRate
  WaterToAirHP(HPNum)%OutletAirMassFlowRate = WaterToAirHP(HPNum)%InletAirMassFlowRate

  WatertoAirHP(HPNum)%Energy          = WatertoAirHP(HPNum)%Power*ReportingConstant
  WatertoAirHP(HPNum)%EnergyLoadTotal = WatertoAirHP(HPNum)%QLoadTotal*ReportingConstant
  WatertoAirHP(HPNum)%EnergySensible  = WatertoAirHP(HPNum)%QSensible*ReportingConstant
  WatertoAirHP(HPNum)%EnergyLatent    = WatertoAirHP(HPNum)%QLatent*ReportingConstant
  WatertoAirHP(HPNum)%EnergySource    = WatertoAirHP(HPNum)%QSource*ReportingConstant

  IF (Contaminant%CO2Simulation) Then
     Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
  End If
  IF (Contaminant%GenericContamSimulation) Then
     Node(AirOutletNode)%GenContam = Node(AirInletNode)%GenContam
  End If

  RETURN
END SUBROUTINE UpdateWatertoAirHP
!        End of Update subroutines for the WatertoAirHP Module
! *****************************************************************************

FUNCTION CalcEffectiveSHR(HPNum,SHRss, CyclingScheme, RTF, QLatRated, QLatActual, EnteringDB, EnteringWB) RESULT(SHReff)

        ! FUNCTION INFORMATION:
        !    AUTHOR         Richard Raustad, FSEC
        !    DATE WRITTEN   September 2003
        !    MODIFIED       Kenneth Tang (Aug 2004) Added capability for simulating CycFanCycCoil
        !    RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        !    Adjust sensible heat ratio to account for degradation of DX coil latent
        !    capacity at part-load (cycling) conditions.

        ! METHODOLOGY EMPLOYED:
        !    With model parameters entered by the user, the part-load latent performance
        !    of a DX cooling coil is determined for a constant air flow system with
        !    a cooling coil that cycles on/off. The model calculates the time
        !    required for condensate to begin falling from the cooling coil.
        !    Runtimes greater than this are integrated to a "part-load" latent
        !    capacity which is used to determine the "part-load" sensible heat ratio.
        !    See reference below for additional details (linear decay model, Eq. 8b).
        !
        ! REFERENCES:
        !   "A Model to Predict the Latent Capacity of Air Conditioners and
        !    Heat Pumps at Part-Load Conditions with Constant Fan Operation"
        !    1996 ASHRAE Transactions, Volume 102, Part 1, Pp. 266 - 274,
        !    Hugh I. Henderson, Jr., P.E., Kannan Rengarajan, P.E.

        ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HPNum   ! Index number for cooling coil
  INTEGER, INTENT (IN) :: CyclingScheme !fan/compressor cycling scheme indicator
  REAL(r64), INTENT (IN) :: SHRss       ! Steady-state sensible heat ratio
  REAL(r64), INTENT (IN) :: RTF         ! Compressor run-time fraction
  REAL(r64), INTENT (IN) :: QLatRated   ! Rated latent capacity
  REAL(r64), INTENT (IN) :: QLatActual  ! Actual latent capacity
  REAL(r64), INTENT (IN) :: EnteringDB  ! Entering air dry-bulb temperature
  REAL(r64), INTENT (IN) :: EnteringWB  ! Entering air wet-bulb temperature
  REAL(r64)            :: SHReff      ! Effective sensible heat ratio, includes degradation due to cycling effects

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Twet        ! Nominal time for condensate to begin leaving the coil's condensate drain line
                         !   at the current operating conditions (sec)
  REAL(r64) :: Gamma       ! Initial moisture evaporation rate divided by steady-state AC latent capacity
                         !   at the current operating conditions
  REAL(r64) :: Twet_rated  ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated ! Gamma at rated conditions (coil air flow rate and air temperatures)
  REAL(r64) :: Twet_max    ! Maximum allowed value for Twet
  REAL(r64) :: MaxONOFFCyclesperHour  !Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64) :: HPTimeConstant         !Heat pump time constant [s]
  REAL(r64) :: FanDelayTime           !Fan delay time, time delay for the HP's fan to
                                    !shut off after compressor cycle off  [s]

  REAL(r64) :: Ton         ! Coil on time (sec)
  REAL(r64) :: Toff        ! Coil off time (sec)
  REAL(r64) :: Toffa       ! Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
  REAL(r64) :: aa          ! Intermediate variable
  REAL(r64) :: To1         ! Intermediate variable (first guess at To). To = time to the start of moisture removal
  REAL(r64) :: To2         ! Intermediate variable (second guess at To). To = time to the start of moisture removal
  REAL(r64) :: Error       ! Error for iteration (DO) loop
  REAL(r64) :: LHRmult     ! Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

   Twet_rated  = WatertoAirHP(HPNum)%Twet_Rated
   Gamma_rated = WatertoAirHP(HPNum)%Gamma_Rated
   MaxONOFFCyclesperHour = WatertoAirHP(HPNum)%MaxONOFFCyclesperHour
   HPTimeConstant = WatertoAirHP(HPNum)%HPTimeConstant
   FanDelayTime = WatertoAirHP(HPNum)%FanDelayTime



!  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
!  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
!  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
   IF((RTF.GE.1.0d0) .OR. (QLatRated.EQ.0.0d0) .OR. (QLatActual.EQ.0.0d0) .OR. (Twet_rated.LE.0.0d0) .OR. &
      (Gamma_rated.LE.0.0d0) .OR. (MaxONOFFCyclesperHour.LE.0.0d0) .OR. (HPTimeConstant.LE.0.0d0) .OR. (RTF.LE. 0.0d0)) THEN
     SHReff = SHRss
     RETURN
   ENDIF

   Twet_max   = 9999.0d0 ! high limit for Twet

!  Calculate the model parameters at the actual operating conditions
   Twet    = MIN(Twet_rated*QLatRated /(QLatActual+1.d-10),Twet_max)
   Gamma   = Gamma_rated*QLatRated*(EnteringDB-EnteringWB)/((26.7d0-19.4d0)*QLatActual+1.d-10)

!  Calculate the compressor on and off times using a converntional thermostat curve
   Ton  = 3600.d0/(4.d0*MaxONOFFCyclesperHour*(1.d0-RTF))   ! duration of cooling coil on-cycle (sec)

   IF ((CyclingScheme .EQ. CycFanCycCoil).AND.(FanDelayTime.NE.0.0d0)) THEN
!  For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
!  until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
   Toff = FanDelayTime
   ELSE
!  For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
!  for the entire heat pump off-cycle.
   Toff = 3600.d0/(4.d0*MaxONOFFCyclesperHour*RTF)        ! duration of cooling coil off-cycle (sec)
   END IF

!  Cap Toff to meet the equation restriction
   IF(Gamma .GT. 0.0d0)THEN
     Toffa = MIN(Toff, 2.d0*Twet/Gamma)
   ELSE
     Toffa = Toff
   END IF

!  Use sucessive substitution to solve for To
   aa = (Gamma*Toffa) - (0.25d0/Twet)*(Gamma**2)*(Toffa**2)

   To1 = aa+HPTimeConstant
   Error = 1.0d0
   DO WHILE (Error .gt. 0.001d0)
       To2 = aa-HPTimeConstant*(EXP(-To1/HPTimeConstant)-1.0d0)
       Error = ABS((To2-To1)/To1)
       To1 = To2
   END DO

!  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
!  Floating underflow errors occur when -Ton/HPTimeConstant is a large negative number.
!  Cap lower limit at -700 to avoid the underflow errors.
   aa = EXP(MAX(-700.0d0,-Ton/HPTimeConstant))
!  Calculate latent heat ratio multiplier
   LHRmult = MAX(((Ton-To2)/(Ton+HPTimeConstant*(aa-1.0d0))),0.0d0)

!  Calculate part-load or "effective" sensible heat ratio
   SHReff = 1.0d0-(1.0d0-SHRss)*LHRmult

   IF (SHReff .LT. SHRss) SHReff = SHRss ! Effective SHR can be less than the steady-state SHR
   IF (SHReff .GT. 1.0d0) SHReff=1.0d0 ! Effective sensible heat ratio can't be greater than 1.0

 RETURN

END FUNCTION CalcEffectiveSHR

REAL(r64) FUNCTION DegradF(FluidName,Temp,FluidIndex)
! FUNCTION INFORMATION:
        !    AUTHOR         Kenneth Tang
        !    DATE WRITTEN   October 2004
        !    MODIFIED       na
        !    RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        !    Calculate the degradation factor to predict the heat pump performance
        !    when antifreeze is used.
        ! METHODOLOGY EMPLOYED:
        !    Use FluidProperties to calculate the properties of water and glycol
        !    at the given temperature. Then substitute the properties into the equation.
        !
        ! REFERENCES:
        !    Jin, H. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd Thesis.
        !    Oklahoma State University.

        ! USE STATEMENTS:
  USE FluidProperties

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT (INOUT) :: FluidName  !Name of glycol used in source side
  REAL(r64), INTENT (INOUT) :: Temp           ! Temperature of the fluid
  INTEGER, INTENT (INOUT) :: FluidIndex    ! Index number for the fluid

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CalledFrom='HVACWaterToAir:DegradF'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: VisWater          !Viscosity of water [mPa-s]
  REAL(r64) :: DensityWater      !Density of water [kg/m3]
  REAL(r64) :: CpWater           !Specific heat of water [J/kg-K]
  REAL(r64) :: CondWater         !Conductivity of water [W/m-K]
  REAL(r64) :: VisCoolant        !Viscosity of water [mPa-s]
  REAL(r64) :: DensityCoolant    !Density of water [kg/m3]
  REAL(r64) :: CpCoolant         !Specific heat of water [J/kg-K]
  REAL(r64) :: CondCoolant       !Conductivity of water [W/m-K]

 VisWater = GetViscosityGlycol('WATER',Temp,WaterIndex,CalledFrom)
 DensityWater = GetDensityGlycol('WATER',Temp,WaterIndex,CalledFrom)
 CpWater = GetSpecificHeatGlycol('WATER',Temp,WaterIndex,CalledFrom)
 CondWater =  GetConductivityGlycol('WATER',Temp,WaterIndex,CalledFrom)
 VisCoolant = GetViscosityGlycol(FluidName,Temp,FluidIndex,CalledFrom)
 DensityCoolant = GetDensityGlycol(FluidName,Temp,FluidIndex,CalledFrom)
 CpCoolant = GetSpecificHeatGlycol(FluidName,Temp,FluidIndex,CalledFrom)
 CondCoolant =  GetConductivityGlycol(FluidName,Temp,FluidIndex,CalledFrom)

 DegradF = (VisCoolant / VisWater)**(-0.47d0) *   &
           (DensityCoolant / DensityWater)**0.8d0 * (CpCoolant / CpWater)**0.33d0 *   &
             (CondCoolant / CondWater)**0.67d0

END FUNCTION DegradF

FUNCTION GetCoilIndex(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the index.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType      ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName      ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound   ! set to true if problem
  INTEGER                      :: IndexNum      ! returned index of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWatertoAirHPInput
    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IndexNum=FindItemInList(CoilName,WatertoAirHP%Name,NumWatertoAirHPs)

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetCoilIndex

FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil capacity for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilCapacity ! returned capacity of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    CALL GetWatertoAirHPInput
    GetCoilsInputFlag=.false.
  End If

  IF (SameString(CoilType,'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') .or.   &
      SameString(CoilType,'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION')) THEN
    WhichCoil=FindItemInList(CoilName,WaterToAirHP%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      IF (SameString(CoilType,'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION')) THEN
        CoilCapacity=WaterToAirHP(WhichCoil)%HeatingCapacity
      ELSE
        CoilCapacity=WaterToAirHP(WhichCoil)%CoolingCapacity
      ENDIF
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilCapacity=-1000.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilCapacity

FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWatertoAirHPInput
    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,WatertoAirHP%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=WatertoAirHP(WhichCoil)%AirInletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNode

FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the outlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetWatertoAirHPInput
    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,WatertoAirHP%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=WatertoAirHP(WhichCoil)%AirOutletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNode

END MODULE WatertoAirHeatPump

!********************************************************************************************
!END First module for parameter estimation heat pumps

! **********************************************!! BREAK IN MODULES  !!****************************************

!begin second module for equation fit model.
!********************************************************************************************
MODULE WatertoAirHeatPumpSimple

! Module containing the Water to Air Heat Pump simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Arun Shenoy
  !       DATE WRITTEN   Nov 2003
  !       MODIFIED       Brent Griffith, Sept 2010 plant upgrades
  !
  !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the Water to Air Heat Pump Simple Component

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:
  ! (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
  ! M.S. Thesis, University of Illinois at Urbana Champaign.
  ! (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
  ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
  ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)
  ! (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
  ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
  ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)

  !
  ! OTHER NOTES:

  ! USE STATEMENTS:
  ! Use statements for data only modules
  ! Use statements for access to subroutines in other modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataSizing
USE DataEnvironment, ONLY: StdBaroPress, OutBaroPress
USE DataHVACGlobals, ONLY: CycFanCycCoil, ContFanCycCoil, WaterCycling, WaterConstant, WaterConstantOnDemand, Heating, Cooling,  &
                           TimestepSys
USE DataInterfaces
USE DataPlant,       ONLY: TypeOf_CoilWAHPHeatingEquationFit, TypeOf_CoilWAHPCoolingEquationFit

IMPLICIT NONE         ! Enforce explicit typing of all variables
PRIVATE               ! Everything private unless explicitly made public

 !MODULE PARAMETER DEFINITIONS
  REAL(r64), PARAMETER    :: CelsiustoKelvin=KelvinConv     ! Conversion from Celsius to Kelvin

! DERIVED TYPE DEFINITIONS
TYPE SimpleWatertoAirHPConditions
  CHARACTER(len=MaxNameLength) :: Name            =' ' ! Name of the Water to Air Heat pump
  CHARACTER(len=MaxNameLength) :: WatertoAirHPType=' ' ! Type of WatertoAirHP ie. Heating or Cooling
  INTEGER   :: WAHPPlantTypeOfNum              = 0     ! type of component in plant
  LOGICAL   :: Simflag                         =.false. ! Heat Pump Simulation Flag
  REAL(r64) :: AirVolFlowRate                  =0.0d0  ! Air Volumetric Flow Rate[m3/s]
  REAL(r64) :: AirMassFlowRate                 =0.0d0  ! Air Mass Flow Rate[kg/s]
  REAL(r64) :: InletAirDBTemp                  =0.0d0  ! Inlet Air Dry Bulb Temperature [C]
  REAL(r64) :: InletAirHumRat                  =0.0d0  ! Inlet Air Humidity Ratio [kg/kg]
  REAL(r64) :: InletAirEnthalpy                =0.0d0  ! Inlet Air Enthalpy [J/kg]
  REAL(r64) :: OutletAirDBTemp                 =0.0d0  ! Outlet Air Dry Bulb Temperature [C]
  REAL(r64) :: OutletAirHumRat                 =0.0d0  ! Outlet Air Humidity Ratio [kg/kg]
  REAL(r64) :: OutletAirEnthalpy               =0.0d0  ! Outlet Air Enthalpy [J/kg]
  REAL(r64) :: WaterVolFlowRate                =0.0d0  ! Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: WaterMassFlowRate               =0.0d0  ! Water Mass Flow Rate [kg/s]
  REAL(r64) :: DesignWaterMassFlowRate         =0.0d0
  REAL(r64) :: InletWaterTemp                  =0.0d0  ! Inlet Water Temperature [C]
  REAL(r64) :: InletWaterEnthalpy              =0.0d0  ! Inlet Water Enthalpy [J/kg]
  REAL(r64) :: OutletWaterTemp                 =0.0d0  ! Outlet Water Temperature [C]
  REAL(r64) :: OutletWaterEnthalpy             =0.0d0  ! Outlet Water Enthalpy [J/kg]
  REAL(r64) :: Power                           =0.0d0  ! Power Consumption [W]
  REAL(r64) :: QLoadTotal                      =0.0d0  ! Load Side Total Heat Transfer Rate [W]
  REAL(r64) :: QSensible                       =0.0d0  ! Sensible Load Side Heat Transfer Rate [W]
  REAL(r64) :: QLatent                         =0.0d0  ! Latent Load Side Heat Transfer Rate [W]
  REAL(r64) :: QSource                         =0.0d0  ! Source Side Heat Transfer Rate [W]
  REAL(r64) :: Energy                          =0.0d0  ! Energy Consumption [J]
  REAL(r64) :: EnergyLoadTotal                 =0.0d0  ! Load Side Total Heat Transferred [J]
  REAL(r64) :: EnergySensible                  =0.0d0  ! Sensible Load Side Heat Transferred [J]
  REAL(r64) :: EnergyLatent                    =0.0d0  ! Latent Load Side Heat Transferred [J]
  REAL(r64) :: EnergySource                    =0.0d0  ! Source Side Heat Transferred [J]
  REAL(r64) :: COP                             =0.0d0  ! Heat Pump Coefficient of Performance [-]
  REAL(r64) :: RunFrac                         =0.0d0  ! Duty Factor
  REAL(r64) :: PartLoadRatio                   =0.0d0  ! Part Load Ratio

  REAL(r64) :: RatedWaterVolFlowRate           =0.0d0  ! Rated/Ref Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedAirVolFlowRate             =0.0d0  ! Rated/Ref Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedCapHeat                    =0.0d0  ! Rated/Ref Heating Capacity [W]
  REAL(r64) :: RatedPowerHeat                  =0.0d0  ! Rated/Ref Heating Power Consumption[W]
  REAL(r64) :: RatedCOPHeat                    =0.0d0  ! Rated/Ref Heating COP [W/W]
  REAL(r64) :: RatedCapCoolTotal               =0.0d0  ! Rated/Ref Total Cooling Capacity [W]
  REAL(r64) :: RatedCapCoolSens                =0.0d0  ! Rated/Ref Sensible Cooling Capacity [W]
  REAL(r64) :: RatedPowerCool                  =0.0d0  ! Rated/Ref Cooling Power Consumption[W]
  REAL(r64) :: RatedCOPCool                    =0.0d0  ! Rated/Ref Cooling COP [W/W]
  REAL(r64) :: HeatCap1                        =0.0d0  ! 1st coefficient of the Heating capacity performance curve
  REAL(r64) :: HeatCap2                        =0.0d0  ! 2nd coefficient of the Heating capacity performance curve
  REAL(r64) :: HeatCap3                        =0.0d0  ! 3rd coefficient of the Heating capacity performance curve
  REAL(r64) :: HeatCap4                        =0.0d0  ! 4th coefficient of the Heating capacity performance curve
  REAL(r64) :: HeatCap5                        =0.0d0  ! 5th coefficient of the Heating capacity performance curve
  REAL(r64) :: HeatPower1                      =0.0d0  ! 1st coefficient of the Heating power consumption curve
  REAL(r64) :: HeatPower2                      =0.0d0  ! 2nd coefficient of the Heating power consumption curve
  REAL(r64) :: HeatPower3                      =0.0d0  ! 3rd coefficient of the Heating power consumption curve
  REAL(r64) :: HeatPower4                      =0.0d0  ! 4th coefficient of the Heating power consumption curve
  REAL(r64) :: HeatPower5                      =0.0d0  ! 5th coefficient of the Heating power consumption curve
  REAL(r64) :: TotalCoolCap1                   =0.0d0  ! 1st coefficient of the Total Cooling capacity performance curve
  REAL(r64) :: TotalCoolCap2                   =0.0d0  ! 2nd coefficient of the Total Cooling capacity performance curve
  REAL(r64) :: TotalCoolCap3                   =0.0d0  ! 3rd coefficient of the Total Cooling capacity performance curve
  REAL(r64) :: TotalCoolCap4                   =0.0d0  ! 4th coefficient of the Total Cooling capacity performance curve
  REAL(r64) :: TotalCoolCap5                   =0.0d0  ! 5th coefficient of the Total Cooling capacity performance curve
  REAL(r64) :: SensCoolCap1                    =0.0d0  ! 1st coefficient of the Sensible Cooling capacity performance curve
  REAL(r64) :: SensCoolCap2                    =0.0d0  ! 2nd coefficient of the Sensible Cooling capacity performance curve
  REAL(r64) :: SensCoolCap3                    =0.0d0  ! 3rd coefficient of the Sensible Cooling capacity performance curve
  REAL(r64) :: SensCoolCap4                    =0.0d0  ! 4th coefficient of the Sensible Cooling capacity performance curve
  REAL(r64) :: SensCoolCap5                    =0.0d0  ! 5th coefficient of the Sensible Cooling capacity performance curve
  REAL(r64) :: SensCoolCap6                    =0.0d0  ! 6th coefficient of the Sensible Cooling capacity performance curve
  REAL(r64) :: CoolPower1                      =0.0d0  ! 1st coefficient of the Cooling power consumption curve
  REAL(r64) :: CoolPower2                      =0.0d0  ! 2nd coefficient of the Cooling power consumption curve
  REAL(r64) :: CoolPower3                      =0.0d0  ! 3rd coefficient of the Cooling power consumption curve
  REAL(r64) :: CoolPower4                      =0.0d0  ! 4th coefficient of the Cooling power consumption curve
  REAL(r64) :: CoolPower5                      =0.0d0  ! 5th coefficient of the Cooling power consumption curve

  INTEGER      :: AirInletNodeNum              =0    ! Node Number of the Air Inlet
  INTEGER      :: AirOutletNodeNum             =0     ! Node Number of the Air Outlet
  INTEGER      :: WaterInletNodeNum            =0     ! Node Number of the Water Onlet
  INTEGER      :: WaterOutletNodeNum           =0     ! Node Number of the Water Outlet
  INTEGER      :: LoopNum                      =0    ! plant loop index for water side
  INTEGER      :: LoopSide                     =0    ! plant loop side index
  INTEGER      :: BranchNum                    =0    ! plant branch index
  INTEGER      :: CompNum                      =0    ! plant component index

  INTEGER   :: WaterCyclingMode                = 0     ! Heat Pump Coil water flow mode; See definitions in DataHVACGlobals,
                                                       ! 1=water cycling, 2=water constant, 3=water constant on demand (old mode)
  INTEGER   :: LastOperatingMode               = WaterCycling  ! type of coil calling for water flow, either heating or cooling,
                                                               ! start it at 1 so there will be water flow from the start,
                                                               ! even if there is no load.
                                                               ! Gets updated only during the first iteration of each timestep
  LOGICAL   :: WaterFlowMode                   = .FALSE.       ! whether the water flow through the coil is called
                                                               ! because there is a load on the coil, or not.
                                                               ! Gets updated each iteration

! set by parent object and "pushed" to this structure in SetSimpleWSHPData subroutine
  INTEGER      :: CompanionCoolingCoilNum      =0    ! Heating coil companion cooling coil index
  INTEGER      :: CompanionHeatingCoilNum      =0    ! Cooling coil companion heating coil index

  REAL(r64) :: Twet_Rated                      =0.0d0  ! Nominal Time for Condensate Removal to Begin [s]
  REAL(r64) :: Gamma_Rated                     =0.0d0  ! Ratio of Initial Moisture Evaporation Rate
                                                   ! and Steady-state Latent Capacity
  REAL(r64) :: MaxONOFFCyclesperHour           =0.0d0  ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64) :: HPTimeConstant                  =0.0d0  ! Heat pump time constant [s]
  REAL(r64) :: FanDelayTime                    =0.0d0  ! Fan delay time, time delay for the HP's fan to
 END TYPE SimpleWatertoAirHPConditions

  ! MODULE VARIABLE DECLARATIONS:
TYPE (SimpleWatertoAirHPConditions), ALLOCATABLE, DIMENSION(:) :: SimpleWatertoAirHP

INTEGER        :: NumWatertoAirHPs  = 0        ! The Number of Water to Air Heat Pumps found in the Input
!INTEGER        :: WaterIndex = 0                   ! Water index
!INTEGER        :: Count = 0
LOGICAL        :: GetCoilsInputFlag = .TRUE.       ! Flag set to make sure you get input once
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: SimpleHPTimeStepFlag  ! determines whether the previous operating mode for the coil and it's partner has been initialized

REAL(r64) :: SourceSideMassFlowRate =0.0d0 ! Source Side Mass flow rate [Kg/s]
REAL(r64) :: SourceSideInletTemp    =0.0d0 ! Source Side Inlet Temperature [C]
REAL(r64) :: SourceSideInletEnth    =0.0d0 ! Source Side Inlet Enthalpy [J/kg]
REAL(r64) :: LoadSideMassFlowRate   =0.0d0 ! Load Side Mass flow rate [Kg/s]
REAL(r64) :: LoadSideInletDBTemp    =0.0d0 ! Load Side Inlet Dry Bulb Temp [C]
REAL(r64) :: LoadSideInletWBTemp    =0.0d0 ! Load Side Inlet Wet Bulb Temp [C]
REAL(r64) :: LoadSideInletHumRat    =0.0d0 ! Load Side Outlet Humidity ratio
REAL(r64) :: LoadSideInletEnth      =0.0d0 ! Load Side Inlet Enthalpy [J/kg]
REAL(r64) :: LoadSideOutletDBTemp   =0.0d0 ! Load Side Outlet Dry Bulb Temp [C]
REAL(r64) :: LoadSideOutletHumRat   =0.0d0 ! Load Side Outlet Humidity ratio
REAL(r64) :: LoadSideOutletEnth     =0.0d0 ! Load Side Outlet Enthalpy [J/kg]
REAL(r64) :: QSensible              =0.0d0 ! Load side sensible heat transfer rate [W]
REAL(r64) :: QLoadTotal             =0.0d0 ! Load side total heat transfer rate [W]
REAL(r64) :: QLatRated              =0.0d0 ! Latent Capacity [W] rated at entering air conditions [Tdb=26.7C Twb=19.4C]
REAL(r64) :: QLatActual             =0.0d0 ! Actual Latent Capacity [W]
REAL(r64) :: QSource                =0.0d0 ! Source side heat transfer rate [W]
REAL(r64) :: Winput                 =0.0d0 ! Power Consumption [W]
REAL(r64) :: PLRCorrLoadSideMdot    =0.0d0 ! Load Side Mdot corrected for Part Load Ratio of the unit

          ! Subroutine Specifications for the Module
          ! Driver/Manager Routines

PUBLIC  SimWatertoAirHPSimple

          ! Get Input routines for module
PRIVATE GetSimpleWatertoAirHPInput

          ! Initialization routines for module
PRIVATE InitSimpleWatertoAirHP
PRIVATE SizeHVACWaterToAir

          ! Algorithms for the module
PRIVATE CalcHPCoolingSimple
PRIVATE CalcHPHeatingSimple

          ! Update routine
PRIVATE UpdateSimpleWatertoAirHP

          ! Utility routines
PUBLIC  GetCoilIndex
PUBLIC  GetCoilCapacity
PUBLIC  GetCoilInletNode
PUBLIC  GetCoilOutletNode
PUBLIC  GetCoilAirFlowRate
PUBLIC  SetSimpleWSHPData

CONTAINS



! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimWatertoAirHPSimple(CompName,CompIndex,SensLoad,LatentLoad, &
           CyclingScheme,RuntimeFrac,MaxONOFFCyclesperHour, &
           HPTimeConstant,FanDelayTime,CompOp, PartLoadRatio, FirstHVACIteration, OnOffAirFlowRat)

          !       AUTHOR         Arun Shenoy
          !       DATE WRITTEN   Nov 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Simple Water to Air Heat Pump component simulation.

          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:
          ! (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
          ! M.S. Thesis, University of Illinois at Urbana Champaign.
          ! (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)
          ! (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)


          ! USE STATEMENTS:
  USE InputProcessor,       ONLY: FindItemInList
  USE FluidProperties,      ONLY: FindGlycol
  USE General,              ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  CHARACTER(len=*), INTENT(IN) :: CompName                ! Coil Name
  INTEGER, INTENT(INOUT)       :: CompIndex               ! Index for Component name
  REAL(r64), INTENT(IN)        :: SensLoad                ! Sensible demand load [W]
  REAL(r64), INTENT(IN)        :: LatentLoad              ! Latent demand load [W]
  INTEGER, INTENT(IN)          :: CyclingScheme           ! Continuous fan OR cycling compressor
  REAL(r64), INTENT (IN)       :: RuntimeFrac             ! Compressor run time fraction  or
                                                          ! percent on-time (on-time/cycle time)
  REAL(r64), INTENT (INOUT)    :: MaxONOFFCyclesperHour   ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64), INTENT (INOUT)   :: HPTimeConstant           ! Heat pump time constant [s]
  REAL(r64), INTENT (INOUT)   :: FanDelayTime             ! Fan delay time, time delay for the HP's fan to
                                                          ! shut off after compressor cycle off  [s]
  INTEGER, INTENT(IN)         :: CompOp
  REAL(r64), INTENT(IN)       :: PartLoadRatio
  LOGICAL, INTENT (IN)        :: FirstHVACIteration
  REAL(r64), OPTIONAL, INTENT(IN) :: OnOffAirFlowRat      ! ratio of comp on to comp off air flow rate

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: HPNum                       ! The WatertoAirHP that you are currently loading input into
  REAL(r64) :: OnOffAirFlowRatio           ! ratio of comp on to comp off air flow rate
  REAL(r64) :: WaterPartLoad               ! The part load ratio of water

           ! FLOW:

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (CompIndex == 0) THEN
    HPNum = FindItemInList(CompName,SimpleWatertoAirHP%Name,NumWatertoAirHPs )
    IF (HPNum == 0) THEN
      CALL ShowFatalError('WaterToAirHPSimple not found='//TRIM(CompName))
    ENDIF
    CompIndex=HPNum
  ELSE
    HPNum=CompIndex
    IF (HPNum > NumWatertoAirHPs  .or. HPNum < 1) THEN
      CALL ShowFatalError('SimWatertoAirHPSimple: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HPNum))// &
                          ', Number of Water to Air HPs='//TRIM(TrimSigDigits(NumWatertoAirHPs))//  &
                          ', WaterToAir HP name='//TRIM(CompName))
    ENDIF
    IF (CompName /= Blank .AND. CompName /= SimpleWatertoAirHP(HPNum)%Name) THEN
      CALL ShowFatalError('SimWatertoAirHPSimple: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HPNum))// &
                          ', WaterToAir HP name='//TRIM(CompName)//', stored WaterToAir HP Name for that index='//  &
                          TRIM(SimpleWatertoAirHP(HPNum)%Name))
    ENDIF
  ENDIF

  IF(PRESENT(OnOffAirFlowRat))THEN
    OnOffAirFlowRatio = OnOffAirFlowRat
  ELSE
    OnOffAirFlowRatio = 1.0d0
  END IF

  ! Calculate the Correct Water to Air HP Model with the current HPNum
  IF((SimpleWatertoAirHP(HPNum)%WaterCyclingMode)==WaterCycling)THEN
    WaterPartLoad = RuntimeFrac
        !IF (WaterPartLoad < 0.1d0)THEN
  ! WaterPartLoad = 0.1d0
  !ENDIF
  ELSE
    WaterPartLoad = 1.0d0
  ENDIF

  IF(SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPCoolingEquationFit)THEN
    ! Cooling mode
    CALL InitSimpleWatertoAirHP(HPNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                OnOffAirFlowRatio,WaterPartLoad,FirstHVACIteration)
    CALL CalcHPCoolingSimple(HPNum,CyclingScheme,RuntimeFrac,SensLoad,LatentLoad,CompOp, PartLoadRatio,   &
                                OnOffAirFlowRatio,WaterPartLoad)
    CALL UpdateSimpleWatertoAirHP(HPNum)
  ELSEIF(SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPHeatingEquationFit)THEN
    ! Heating mode
    CALL InitSimpleWatertoAirHP(HPNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,constant_zero,CyclingScheme, &
                                OnOffAirFlowRatio,WaterPartLoad,FirstHVACIteration)
    CALL CalcHPHeatingSimple(HPNum,CyclingScheme,RuntimeFrac,SensLoad,CompOp,PartLoadRatio, OnOffAirFlowRatio,WaterPartLoad)
    CALL UpdateSimpleWatertoAirHP(HPNum)
  ELSE
    CALL ShowFatalError ('SimWatertoAirHPSimple: WatertoAir heatpump not in either HEATING or COOLING mode')
  ENDIF

  RETURN

END SUBROUTINE SimWatertoAirHPSimple

! MODULE SUBROUTINES:
!*************************************************************************

SUBROUTINE GetSimpleWatertoAirHPInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Arun Shenoy
          !       DATE WRITTEN   Nov 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for HPs and stores it in HP data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! (1) Lash.T.A.,1992.Simulation and Analysis of a Water loop Heat Pump System.
          ! M.S. Thesis, University of Illinois at Urbana Champaign.
          ! (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)
          ! (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager
    USE BranchNodeConnections, ONLY: TestCompSet
    USE GlobalNames, ONLY: VerifyUniqueCoilName
    USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER (len=*), PARAMETER   :: RoutineName='GetSimpleWatertoAirHPInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: HPNum                    ! The Water to Air HP that you are currently loading input into
    INTEGER :: NumCool                  ! Counter for cooling coil
    INTEGER :: NumHeat                  ! Counter for heating coil
    INTEGER :: WatertoAirHPNum          ! Counter
    INTEGER :: NumAlphas                ! Number of variables in String format
    INTEGER :: NumNums                  ! Number of variables in Numeric format
    INTEGER :: NumParams                ! Total number of input fields
    INTEGER :: MaxNums=0                ! Maximum number of numeric input fields
    INTEGER :: MaxAlphas=0              ! Maximum number of alpha input fields
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound = .FALSE.     ! If errors detected in input
    LOGICAL       :: IsNotOK             ! Flag to verify name
    LOGICAL       :: IsBlank             ! Flag for blank name
    LOGICAL       :: errflag
    CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.

    NumCool   = GetNumObjectsFound('Coil:Cooling:WaterToAirHeatPump:EquationFit')
    NumHeat   = GetNumObjectsFound('Coil:Heating:WaterToAirHeatPump:EquationFit')
    NumWatertoAirHPs = NumCool+NumHeat
    HPNum=0

    IF(NumWatertoAirHPs <= 0) THEN
      CALL ShowSevereError('No Equipment found in SimWatertoAirHPSimple')
      ErrorsFound=.TRUE.
    END IF

   ! Allocate Arrays
    IF (NumWatertoAirHPs.GT.0) THEN
      ALLOCATE(SimpleWatertoAirHP(NumWatertoAirHPs))
      ALLOCATE(SimpleHPTimeStepFlag(NumWatertoAirHPs))
      SimpleHPTimeStepFlag = .TRUE.
    ENDIF

    CALL GetObjectDefMaxArgs('Coil:Cooling:WaterToAirHeatPump:EquationFit',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('Coil:Heating:WaterToAirHeatPump:EquationFit',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    ALLOCATE(AlphArray(MaxAlphas))
    AlphArray=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.TRUE.
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0d0


      ! Get the data for cooling coil
    CurrentModuleObject = 'Coil:Cooling:WaterToAirHeatPump:EquationFit'

    DO WatertoAirHPNum = 1, NumCool

        HPNum= HPNum + 1

        CALL GetObjectItem(CurrentModuleObject,HPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),SimpleWatertoAirHP%Name,HPNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        SimpleWatertoAirHP(HPNum)%Name     = TRIM(AlphArray(1))
        SimpleWatertoAirHP(HPNum)%WatertoAirHPType  = 'COOLING'
        SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum  = TypeOf_CoilWAHPCoolingEquationFit
        SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate = NumArray(1)
        SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate=NumArray(2)
        SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal=NumArray(3)
        SimpleWatertoAirHP(HPNum)%RatedCapCoolSens=NumArray(4)
        SimpleWatertoAirHP(HPNum)%RatedCOPCool=NumArray(5)
        SimpleWatertoAirHP(HPNum)%TotalCoolCap1=NumArray(6)
        SimpleWatertoAirHP(HPNum)%TotalCoolCap2=NumArray(7)
        SimpleWatertoAirHP(HPNum)%TotalCoolCap3=NumArray(8)
        SimpleWatertoAirHP(HPNum)%TotalCoolCap4=NumArray(9)
        SimpleWatertoAirHP(HPNum)%TotalCoolCap5=NumArray(10)
        SimpleWatertoAirHP(HPNum)%SensCoolCap1=NumArray(11)
        SimpleWatertoAirHP(HPNum)%SensCoolCap2=NumArray(12)
        SimpleWatertoAirHP(HPNum)%SensCoolCap3=NumArray(13)
        SimpleWatertoAirHP(HPNum)%SensCoolCap4=NumArray(14)
        SimpleWatertoAirHP(HPNum)%SensCoolCap5=NumArray(15)
        SimpleWatertoAirHP(HPNum)%SensCoolCap6=NumArray(16)
        SimpleWatertoAirHP(HPNum)%CoolPower1=NumArray(17)
        SimpleWatertoAirHP(HPNum)%CoolPower2=NumArray(18)
        SimpleWatertoAirHP(HPNum)%CoolPower3=NumArray(19)
        SimpleWatertoAirHP(HPNum)%CoolPower4=NumArray(20)
        SimpleWatertoAirHP(HPNum)%CoolPower5=NumArray(21)
        SimpleWatertoAirHP(HPNum)%Twet_Rated=NumArray(22)
        SimpleWatertoAirHP(HPNum)%Gamma_Rated=NumArray(23)

        SimpleWatertoAirHP(HPNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        SimpleWatertoAirHP(HPNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        SimpleWatertoAirHP(HPNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Air Nodes')

        CALL SetupOutputVariable('Cooling Coil Electric Energy [J]', &
             SimpleWatertoAirHP(HPNum)%Energy,'System','Summed',SimpleWatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')

        CALL SetupOutputVariable('Cooling Coil Total Cooling Energy [J]', &
             SimpleWatertoAirHP(HPNum)%EnergyLoadTotal,'System','Summed',SimpleWatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('Cooling Coil Sensible Cooling Energy [J]', &
             SimpleWatertoAirHP(HPNum)%EnergySensible,'System','Summed',SimpleWatertoAirHP(HPNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Latent Cooling Energy [J]', &
             SimpleWatertoAirHP(HPNum)%EnergyLatent,'System','Summed',SimpleWatertoAirHP(HPNum)%Name)

        CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Energy [J]', &
             SimpleWatertoAirHP(HPNum)%EnergySource,'System','Summed',SimpleWatertoAirHP(HPNum)%Name,   &
             ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')

        !create predefined report entries
        CALL PreDefTableEntry(pdchCoolCoilType,SimpleWatertoAirHP(HPNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchCoolCoilTotCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
        CALL PreDefTableEntry(pdchCoolCoilSensCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
        CALL PreDefTableEntry(pdchCoolCoilLatCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal &
                                 - SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
        CALL PreDefTableEntry(pdchCoolCoilSHR,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolSens &
                                 / SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
        CALL PreDefTableEntry(pdchCoolCoilNomEff,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedPowerCool &
                                 / SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)

   END DO

      ! Get the data for heating coil
   CurrentModuleObject = 'Coil:Heating:WaterToAirHeatPump:EquationFit'

   DO WatertoAirHPNum = 1, NumHeat

        HPNum= HPNum + 1

        CALL GetObjectItem(CurrentModuleObject,WatertoAirHPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),SimpleWatertoAirHP%Name,HPNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,AlphArray(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        SimpleWatertoAirHP(HPNum)%Name     = TRIM(AlphArray(1))
        SimpleWatertoAirHP(HPNum)%WatertoAirHPType  = 'HEATING'
        SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum  = TypeOf_CoilWAHPHeatingEquationFit
        SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate = NumArray(1)
        SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate=NumArray(2)
        SimpleWatertoAirHP(HPNum)%RatedCapHeat=NumArray(3)
        SimpleWatertoAirHP(HPNum)%RatedCOPHeat=NumArray(4)
        SimpleWatertoAirHP(HPNum)%HeatCap1=NumArray(5)
        SimpleWatertoAirHP(HPNum)%HeatCap2=NumArray(6)
        SimpleWatertoAirHP(HPNum)%HeatCap3=NumArray(7)
        SimpleWatertoAirHP(HPNum)%HeatCap4=NumArray(8)
        SimpleWatertoAirHP(HPNum)%HeatCap5=NumArray(9)
        SimpleWatertoAirHP(HPNum)%HeatPower1=NumArray(10)
        SimpleWatertoAirHP(HPNum)%HeatPower2=NumArray(11)
        SimpleWatertoAirHP(HPNum)%HeatPower3=NumArray(12)
        SimpleWatertoAirHP(HPNum)%HeatPower4=NumArray(13)
        SimpleWatertoAirHP(HPNum)%HeatPower5=NumArray(14)

        SimpleWatertoAirHP(HPNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        SimpleWatertoAirHP(HPNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        SimpleWatertoAirHP(HPNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Air Nodes')

        CALL SetupOutputVariable('Heating Coil Electric Energy [J]', &
             SimpleWatertoAirHP(HPNum)%Energy,'System','Summed',SimpleWatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')

        CALL SetupOutputVariable('Heating Coil Heating Energy [J]', &
             SimpleWatertoAirHP(HPNum)%EnergyLoadTotal,'System','Summed',SimpleWatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Energy [J]', &
             SimpleWatertoAirHP(HPNum)%EnergySource,'System','Summed',SimpleWatertoAirHP(HPNum)%Name,  &
             ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')

        !create predefined report entries
        CALL PreDefTableEntry(pdchHeatCoilType,SimpleWatertoAirHP(HPNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchHeatCoilNomCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapHeat)
        CALL PreDefTableEntry(pdchHeatCoilNomEff,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedPowerHeat &
                                 / SimpleWatertoAirHP(HPNum)%RatedCapHeat)

   END DO

  DEALLOCATE(AlphArray)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lNumericBlanks)
  DEALLOCATE(NumArray)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found getting input. Program terminates.')
  ENDIF

  DO HPNum=1,NumWatertoAirHPs

    IF (    SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum== TypeOf_CoilWAHPCoolingEquationFit ) THEN
          ! COOLING COIL  Setup Report variables for the Heat Pump
      CALL SetupOutputVariable('Cooling Coil Electric Power [W]', &
           SimpleWatertoAirHP(HPNum)%Power,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Total Cooling Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QLoadTotal,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Sensible Cooling Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QSensible,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Latent Cooling Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QLatent,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Source Side Heat Transfer Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QSource,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Part Load Ratio []', &
           SimpleWatertoAirHP(HPNum)%PartLoadRatio,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Runtime Fraction []', &
           SimpleWatertoAirHP(HPNum)%RunFrac,'System','Average',SimpleWatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Cooling Coil Air Mass Flow Rate [kg/s]', &
           SimpleWatertoAirHP(HPNum)%AirMassFlowRate,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Inlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%InletAirDBTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
           SimpleWatertoAirHP(HPNum)%InletAirHumRat,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Outlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%OutletAirDBTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
           SimpleWatertoAirHP(HPNum)%OutletAirHumRat,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Source Side Mass Flow Rate [kg/s]', &
           SimpleWatertoAirHP(HPNum)%WaterMassFlowRate,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Source Side Inlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%InletWaterTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Cooling Coil Source Side Outlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%OutletWaterTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)

    ELSEIF ( SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum== TypeOf_CoilWAHPHeatingEquationFit) THEN
          ! HEATING COIL Setup Report variables for the Heat Pump
      CALL SetupOutputVariable('Heating Coil Electric Power [W]', &
           SimpleWatertoAirHP(HPNum)%Power,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Heating Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QLoadTotal,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Sensible Heating Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QSensible,'System','Average',SimpleWatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Heating Coil Source Side Heat Transfer Rate [W]', &
           SimpleWatertoAirHP(HPNum)%QSource,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Part Load Ratio []', &
           SimpleWatertoAirHP(HPNum)%PartLoadRatio,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Runtime Fraction []', &
           SimpleWatertoAirHP(HPNum)%RunFrac,'System','Average',SimpleWatertoAirHP(HPNum)%Name)

      CALL SetupOutputVariable('Heating Coil Air Mass Flow Rate [kg/s]', &
           SimpleWatertoAirHP(HPNum)%AirMassFlowRate,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Inlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%InletAirDBTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Inlet Humidity Ratio [kgWater/kgDryAir]', &
           SimpleWatertoAirHP(HPNum)%InletAirHumRat,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Outlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%OutletAirDBTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Air Outlet Humidity Ratio [kgWater/kgDryAir]', &
           SimpleWatertoAirHP(HPNum)%OutletAirHumRat,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Source Side Mass Flow Rate [kg/s]', &
           SimpleWatertoAirHP(HPNum)%WaterMassFlowRate,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Source Side Inlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%InletWaterTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)
      CALL SetupOutputVariable('Heating Coil Source Side Outlet Temperature [C]', &
           SimpleWatertoAirHP(HPNum)%OutletWaterTemp,'System','Average',SimpleWatertoAirHP(HPNum)%Name)

    ENDIF



   END DO

  RETURN

END SUBROUTINE GetSimpleWatertoAirHPInput

 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitSimpleWatertoAirHP(HPNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio,WaterPartLoad,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Arun Shenoy
          !       DATE WRITTEN   Nov 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Simple Water to Air HP Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE Psychrometrics,       ONLY:  PsyRhoAirFnPbTdbW
  USE DataGlobals,          ONLY: SysSizingCalc
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : ScanPlantLoopsForObject, PlantLoop
  USE PlantUtilities,  ONLY : InitComponentNodes, SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN) :: HPNum                 ! Current HPNum under simulation
  REAL(r64),    INTENT(IN) :: MaxONOFFCyclesperHour ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64),    INTENT(IN) :: HPTimeConstant        ! Heat pump time constant [s]
  REAL(r64),    INTENT(IN) :: FanDelayTime          ! Fan delay time, time delay for the HP's fan to
                                               ! shut off after compressor cycle off  [s]
  REAL(r64), INTENT(IN) :: SensLoad              ! Control zone sensible load[W]
  REAL(r64), INTENT(IN) :: LatentLoad            ! Control zone latent load[W]
  INTEGER,   INTENT(IN) :: CyclingScheme         ! fan operating mode
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio     ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: WaterPartLoad
  LOGICAL,   INTENT(IN) :: FirstHVACIteration  ! Iteration flag


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: AirInletNode        ! Node Number of the air inlet
  INTEGER                :: WaterInletNode      ! Node Number of the Water inlet
  LOGICAL, SAVE          :: MyOneTimeFlag = .TRUE.  ! one time allocation flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! used for sizing PTHP inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  REAL(r64)   :: rho ! local fluid density
  REAL(r64)   :: Cp  ! local fluid specific heat
  LOGICAL     :: errFlag


  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MySizeFlag(NumWatertoAirHPs))
    ALLOCATE(MyEnvrnFlag(NumWatertoAirHPs))
    ALLOCATE(MyPlantScanFlag(NumWatertoAirHPs))
    MySizeFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .FALSE.

  END IF

  IF (MyPlantScanFlag(HPNum) .AND. ALLOCATED(PlantLoop)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(SimpleWatertoAirHP(HPNum)%Name,  &
                                 SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum, &
                                 SimpleWatertoAirHP(HPNum)%LoopNum, &
                                 SimpleWatertoAirHP(HPNum)%LoopSide, &
                                 SimpleWatertoAirHP(HPNum)%BranchNum, &
                                 SimpleWatertoAirHP(HPNum)%CompNum,   &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitSimpleWatertoAirHP: Program terminated for previous conditions.')
    ENDIF
    MyPlantScanFlag(HPNum) = .FALSE.
  ENDIF


  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(HPNum) .AND. .NOT. MyPlantScanFlag(HPNum) ) THEN
    ! for each furnace, do the sizing once.
    CALL SizeHVACWaterToAir(HPNum)

    MySizeFlag(HPNum) = .FALSE.

  END IF

  IF(FirstHVACIteration)THEN
    IF(SimpleHPTimeStepFlag(HPNum))THEN
      IF(SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPCoolingEquationFit)THEN
        IF (SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum .GT. 0) THEN
          IF(SimpleWatertoAirHP(HPNum)%WaterFlowMode)THEN
            SimpleWatertoAirHP(HPNum)%LastOperatingMode = Cooling
            SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum)%LastOperatingMode = Cooling
          ELSEIF(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum)%WaterFlowMode)THEN
            SimpleWatertoAirHP(HPNum)%LastOperatingMode = Heating
            SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum)%LastOperatingMode = Heating
          END IF
          SimpleHPTimeStepFlag(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum) = .FALSE.
        ELSE
          IF(SimpleWatertoAirHP(HPNum)%WaterFlowMode)THEN
            SimpleWatertoAirHP(HPNum)%LastOperatingMode = Cooling
          ENDIF
        ENDIF
        SimpleHPTimeStepFlag(HPNum) = .FALSE.
      ELSE
        ! it is a heating coil
        IF(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum .GT. 0) THEN
          IF(SimpleWatertoAirHP(HPNum)%WaterFlowMode)THEN
            SimpleWatertoAirHP(HPNum)%LastOperatingMode = Heating
            SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%LastOperatingMode = Heating
          ELSEIF(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%WaterFlowMode)THEN
            SimpleWatertoAirHP(HPNum)%LastOperatingMode = Cooling
            SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%LastOperatingMode = Cooling
          END IF
          SimpleHPTimeStepFlag(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum) = .FALSE.
        ELSE
          IF(SimpleWatertoAirHP(HPNum)%WaterFlowMode)THEN
            SimpleWatertoAirHP(HPNum)%LastOperatingMode = Heating
          ENDIF
        ENDIF
        SimpleHPTimeStepFlag(HPNum) = .FALSE.
      END IF
    END IF
  ELSE
    SimpleHPTimeStepFlag(HPNum) = .TRUE.
    IF(SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPCoolingEquationFit)THEN
      IF(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum .GT. 0) &
      SimpleHPTimeStepFlag(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum) = .TRUE.
    ELSE
      IF(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum .GT. 0) &
      SimpleHPTimeStepFlag(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum) = .TRUE.
    END IF
  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(HPNum) .AND. .NOT. MyPlantScanFlag(HPNum)) THEN
      ! Do the initializations to start simulation

    AirInletNode   = SimpleWatertoAirHP(HPNum)%AirInletNodeNum
    WaterInletNode = SimpleWatertoAirHP(HPNum)%WaterInletNodeNum

    !Initialize all report variables to a known state at beginning of simulation
    SimpleWatertoAirHP(HPNum)%AirVolFlowRate=0.0d0
    SimpleWatertoAirHP(HPNum)%InletAirDBTemp=0.0d0
    SimpleWatertoAirHP(HPNum)%InletAirHumRat=0.0d0
    SimpleWatertoAirHP(HPNum)%OutletAirDBTemp=0.0d0
    SimpleWatertoAirHP(HPNum)%OutletAirHumRat=0.0d0
    SimpleWatertoAirHP(HPNum)%WaterVolFlowRate=0.0d0
    SimpleWatertoAirHP(HPNum)%WaterMassFlowRate=0.0d0
    SimpleWatertoAirHP(HPNum)%InletWaterTemp=0.0d0
    SimpleWatertoAirHP(HPNum)%InletWaterEnthalpy = 0.0d0
    SimpleWatertoAirHP(HPNum)%OutletWaterEnthalpy = 0.0d0
    SimpleWatertoAirHP(HPNum)%OutletWaterTemp=0.0d0
    SimpleWatertoAirHP(HPNum)%Power=0.0d0
    SimpleWatertoAirHP(HPNum)%QLoadTotal=0.0d0
    SimpleWatertoAirHP(HPNum)%QSensible=0.0d0
    SimpleWatertoAirHP(HPNum)%QLatent=0.0d0
    SimpleWatertoAirHP(HPNum)%QSource=0.0d0
    SimpleWatertoAirHP(HPNum)%Energy=0.0d0
    SimpleWatertoAirHP(HPNum)%EnergyLoadTotal=0.0d0
    SimpleWatertoAirHP(HPNum)%EnergySensible=0.0d0
    SimpleWatertoAirHP(HPNum)%EnergyLatent=0.0d0
    SimpleWatertoAirHP(HPNum)%EnergySource=0.0d0
    SimpleWatertoAirHP(HPNum)%COP=0.0d0
    SimpleWatertoAirHP(HPNum)%RunFrac=0.0d0
    SimpleWatertoAirHP(HPNum)%PartLoadRatio=0.0d0

    rho = GetDensityGlycol(PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidName, &
                              InitConvTemp, &
                              PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidIndex, &
                              'InitSimpleWatertoAirHP')
    Cp  = GetSpecificHeatGlycol(PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidName, &
                              InitConvTemp, &
                              PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidIndex, &
                              'InitSimpleWatertoAirHP')

    SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate= &
                             rho * SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate
    SimpleWatertoAirHP(HPNum)%MaxONOFFCyclesperHour=MaxONOFFCyclesperHour
    SimpleWatertoAirHP(HPNum)%HPTimeConstant=HPTimeConstant
    SimpleWatertoAirHP(HPNum)%FanDelayTime=FanDelayTime

    CALL InitComponentNodes(0.d0, SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate, &
                                  SimpleWatertoAirHP(HPNum)%WaterInletNodeNum,  &
                                  SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum , &
                                  SimpleWatertoAirHP(HPNum)%LoopNum, &
                                  SimpleWatertoAirHP(HPNum)%LoopSide, &
                                  SimpleWatertoAirHP(HPNum)%BranchNum, &
                                  SimpleWatertoAirHP(HPNum)%CompNum )

    Node(WaterInletNode)%Temp          = 5.0d0
    Node(WaterInletNode)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
    Node(WaterInletNode)%Quality       = 0.0d0
    Node(WaterInletNode)%Press         = 0.0d0
    Node(WaterInletNode)%HumRat        = 0.0d0

    Node(SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum)%Temp          = 5.0d0
    Node(SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
    Node(SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum)%Quality       = 0.0d0
    Node(SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum)%Press         = 0.0d0
    Node(SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum)%HumRat        = 0.0d0

    SimpleWatertoAirHP(HPNum)%SimFlag = .TRUE.

    MyEnvrnFlag(HPNum) = .FALSE.

  END IF  ! End If for the Begin Environment initializations

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(HPNum)=.TRUE.
  ENDIF

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! First set the conditions for the air into the heat pump model

  ! Set water and air inlet nodes

  AirInletNode = SimpleWatertoAirHP(HPNum)%AirInletNodeNum
  WaterInletNode = SimpleWatertoAirHP(HPNum)%WaterInletNodeNum

  IF ((SensLoad .NE. 0.0d0 .OR. LatentLoad .NE. 0.0d0).AND.(Node(AirInletNode)%MassFlowRate > 0.0d0)) THEN

   ! changed the water mass flow rate to be equal to the design times run time fraction in order to account for
   ! cycling of equipment
    SimpleWatertoAirHP(HPNum)%WaterMassFlowRate =    SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate*WaterPartLoad

!    SimpleWatertoAirHP(HPNum)%WaterMassFlowRate =    SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate

! Model requires the values to be calculated at full design flow rate for air and then scaled to part load ratio.
! So always start the calculations by setting the air flow rate to design flow rate.

!    SimpleWatertoAirHP(HPNum)%AirMassFlowRate   = Node(AirInletNode)%MassFlowRate
    SimpleWatertoAirHP(HPNum)%AirMassFlowRate   = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate*  &
             PsyRhoAirFnPbTdbW(StdBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
    !If air flow is less than 25% rated flow. Then set air flow to the 25% of rated conditions
    IF(SimpleWatertoAirHP(HPNum)%AirMassFlowRate.LT.  &
         0.25d0*SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate*  &
             PsyRhoAirFnPbTdbW(StdBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)) THEN
        SimpleWatertoAirHP(HPNum)%AirMassFlowRate =   &
          0.25d0*SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate* &
              PsyRhoAirFnPbTdbW(StdBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
    END IF
    SimpleWatertoAirHP(HPNum)%WaterFlowMode = .TRUE.
  ELSE !heat pump is off
    SimpleWatertoAirHP(HPNum)%WaterFlowMode = .FALSE.
    SimpleWatertoAirHP(HPNum)%WaterMassFlowRate = 0.d0
    SimpleWatertoAirHP(HPNum)%AirMassFlowRate   = 0.d0
    IF((SimpleWatertoAirHP(HPNum)%WaterCyclingMode)==WaterConstant)THEN
      IF(SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPCoolingEquationFit)THEN
        IF (SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum .GT. 0) THEN
          IF(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionHeatingCoilNum)%QLoadTotal .GT. 0.0d0)THEN
            ! do nothing, there will be flow through this coil
          ELSEIF(SimpleWatertoAirHP(HPNum)%LastOperatingMode==Cooling)THEN
            ! set the flow rate to full design flow
            SimpleWatertoAirHP(HPNum)%WaterMassFlowRate = SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate
          END IF
        ELSE
          IF(SimpleWatertoAirHP(HPNum)%LastOperatingMode==Cooling)THEN
            ! set the flow rate to full design flow
            SimpleWatertoAirHP(HPNum)%WaterMassFlowRate = SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate
          END IF
        ENDIF
      ELSEIF(SimpleWatertoAirHP(HPNum)%WAHPPlantTypeOfNum==TypeOf_CoilWAHPHeatingEquationFit)THEN
        ! It's a heating coil
        IF(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum .GT. 0) THEN
          IF(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%QLoadTotal .GT. 0.0d0)THEN
            ! do nothing, there will be flow through this coil
          ELSEIF(SimpleWatertoAirHP(HPNum)%LastOperatingMode==Heating)THEN
            ! set the flow rate to full design flow
            SimpleWatertoAirHP(HPNum)%WaterMassFlowRate = SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate
          END IF
        ELSE
          IF(SimpleWatertoAirHP(HPNum)%LastOperatingMode==Heating)THEN
            ! set the flow rate to full design flow
            SimpleWatertoAirHP(HPNum)%WaterMassFlowRate = SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate
          END IF
        ENDIF
      END IF
    ENDIF
  ENDIF

  CALL SetComponentFlowRate(SimpleWatertoAirHP(HPNum)%WaterMassFlowRate, &
                                 SimpleWatertoAirHP(HPNum)%WaterInletNodeNum , &
                                 SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum, &
                                 SimpleWatertoAirHP(HPNum)%LoopNum, &
                                 SimpleWatertoAirHP(HPNum)%LoopSide, &
                                 SimpleWatertoAirHP(HPNum)%BranchNum, &
                                 SimpleWatertoAirHP(HPNum)%CompNum )


   SimpleWatertoAirHP(HPNum)%InletAirDBTemp       = Node(AirInletNode)%Temp
   SimpleWatertoAirHP(HPNum)%InletAirHumRat       = Node(AirInletNode)%HumRat
   SimpleWatertoAirHP(HPNum)%InletAirEnthalpy     = Node(AirInletNode)%Enthalpy
   SimpleWatertoAirHP(HPNum)%InletWaterTemp       = Node(WaterInletNode)%Temp
   SimpleWatertoAirHP(HPNum)%InletWaterEnthalpy   = Node(WaterInletNode)%Enthalpy

   SimpleWatertoAirHP(HPNum)%MaxONOFFCyclesperHour= MaxONOFFCyclesperHour
   SimpleWatertoAirHP(HPNum)%HPTimeConstant       = HPTimeConstant
   SimpleWatertoAirHP(HPNum)%FanDelayTime         = FanDelayTime

   ! Outlet variables
   SimpleWatertoAirHP(HPNum)%Power=0.0d0
   SimpleWatertoAirHP(HPNum)%QLoadTotal=0.0d0
   SimpleWatertoAirHP(HPNum)%QSensible=0.0d0
   SimpleWatertoAirHP(HPNum)%QLatent=0.0d0
   SimpleWatertoAirHP(HPNum)%QSource=0.0d0
   SimpleWatertoAirHP(HPNum)%Energy=0.0d0
   SimpleWatertoAirHP(HPNum)%EnergyLoadTotal=0.0d0
   SimpleWatertoAirHP(HPNum)%EnergySensible=0.0d0
   SimpleWatertoAirHP(HPNum)%EnergyLatent=0.0d0
   SimpleWatertoAirHP(HPNum)%EnergySource=0.0d0
   SimpleWatertoAirHP(HPNum)%COP=0.0d0

   SimpleWatertoAirHP(HPNum)%OutletAirDBTemp=0.0d0
   SimpleWatertoAirHP(HPNum)%OutletWaterTemp=0.0d0
   SimpleWatertoAirHP(HPNum)%OutletAirHumRat=0.0d0
   SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy = 0.0d0
   SimpleWatertoAirHP(HPNum)%OutletWaterEnthalpy = 0.0d0

  RETURN

END SUBROUTINE InitSimpleWatertoAirHP

SUBROUTINE SizeHVACWaterToAir(HPNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2009
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing WSHP Components for which nominal capacities
          ! and flow rates have not been specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Obtains heating capacities and flow rates from the zone or system sizing arrays.
          !
          ! NOTE: For WSHP's we are sizing the heating capacity to be
          ! equal to the cooling capacity.  Thus the cooling and
          ! and heating capacities of a DX heat pump system will be identical. In real life the ARI
          ! heating and cooling capacities are close but not identical.



          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics
  USE DataPlant,          ONLY: PlantLoop, MyPlantSizingIndex
  USE DataHVACGlobals,    ONLY: SmallAirVolFlow, SmallLoad
  USE General,            ONLY: TrimSigDigits, RoundSigDigits
  USE PlantUtilities,     ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataAirSystems,     ONLY: PrimaryAirSystem
  USE OutputReportPredefined
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: HPNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='SizeWaterToAirCoil'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: rhoair
  REAL(r64) :: CpAir
  REAL(r64) :: MixTemp
  REAL(r64) :: MixHumRat
  REAL(r64) :: MixEnth
  REAL(r64) :: MixWetBulb
  REAL(r64) :: SupTemp
  REAL(r64) :: SupHumRat
  REAL(r64) :: SupEnth
  REAL(r64) :: OutTemp
  REAL(r64) :: ratioTDB
  REAL(r64) :: ratioTWB
  REAL(r64) :: ratioTS
  REAL(r64) :: OutAirFrac
  REAL(r64) :: VolFlowRate
  REAL(r64) :: CoolCapAtPeak
  REAL(r64) :: TotCapTempModFac
  REAL(r64) :: SensCapAtPeak
  REAL(r64) :: SensCapTempModFac
  REAL(r64) :: TotalCapCoeff1         ! 1st coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff2         ! 2nd coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff3         ! 3rd coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff4         ! 4th coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff5         ! 5th coefficient of the total cooling capacity performance curve
  REAL(r64) :: SensCapCoeff1          ! 1st coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff2          ! 2nd coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff3          ! 3rd coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff4          ! 4th coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff5          ! 5th coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff6          ! 6th coefficient of the sensible cooling capacity performance curve
  INTEGER   :: TimeStepNumAtMax
  INTEGER   :: DDNum
  INTEGER   :: PltSizNum
  LOGICAL   :: RatedCapCoolTotalAutosized
  LOGICAL   :: RatedCapCoolSensAutosized
  LOGICAL   :: ErrorsFound
  REAL(r64) :: SystemCapacity
  REAL(r64) :: rho
  REAL(r64) :: cp
  LOGICAL   :: IsAutosize                ! Indicator to autosize
  LOGICAL   :: HardSizeNoDesRun          ! Indicator to hardsize and no sizing run
  REAL(r64) :: RatedAirVolFlowRateDes    ! Autosized rated air flow for reporting
  REAL(r64) :: RatedAirVolFlowRateUser   ! Hardsized rated air flow for reporting
  REAL(r64) :: RatedCapCoolTotalDes      ! Autosized rated cooling capacity for reporting
  REAL(r64) :: RatedCapCoolTotalUser     ! Hardsized rated cooling capacity for reporting
  REAL(r64) :: RatedCapCoolSensDes       ! Autosized rated sensible cooling capacity for reporting
  REAL(r64) :: RatedCapCoolSensUser      ! Hardsized rated sensible cooling capacity for reporting
  REAL(r64) :: RatedCapHeatDes           ! Autosized rated heating capacity for reporting
  REAL(r64) :: RatedCapHeatUser          ! Hardsized rated heating capacity for reporting
  REAL(r64) :: RatedWaterVolFlowRateDes  ! Autosized rated water flow rate for reporting
  REAL(r64) :: RatedWaterVolFlowRateUser ! Hardsized rated water flow rate for reporting
  LOGICAL :: SizingDesRunThisAirSys            ! true if a particular air system had a Sizing:System object and system sizing done
  LOGICAL :: SizingDesRunThisZone              ! true if a particular zone had a Sizing:Zone object and zone sizing was done

  PltSizNum = 0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  IF (SysSizingRunDone .OR. ZoneSizingRunDone) THEN
    HardSizeNoDesRun = .FALSE.
  ELSE
    HardSizeNoDesRun = .TRUE.
  ENDIF
  IF (CurSysNum > 0) THEN
    CALL CheckThisAirSystemForSizing(CurSysNum, SizingDesRunThisAirSys )
  ELSE
    SizingDesRunThisAirSys =  .FALSE.
  ENDIF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
  ELSE
    SizingDesRunThisZone =  .FALSE.
  ENDIF
  RatedAirVolFlowRateDes = 0.0d0
  RatedAirVolFlowRateUser = 0.0d0
  RatedCapCoolTotalDes = 0.0d0
  RatedCapCoolTotalUser = 0.0d0
  RatedCapCoolSensDes = 0.0d0
  RatedCapCoolSensUser = 0.0d0
  RatedCapHeatDes = 0.0d0
  RatedCapHeatUser = 0.0d0
  RatedWaterVolFlowRateDes = 0.0d0
  RatedWaterVolFlowRateUser = 0.0d0

  IF (SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurSysNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'User-Specified Rated Air Flow Rate [m3/s]', &
                             SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate)
      END IF
    ELSE
      CALL CheckSysSizing('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                          SimpleWatertoAirHP(HPNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        RatedAirVolFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        RatedAirVolFlowRateDes = 0.0d0
      END IF
    END IF
  ELSE IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'User-Specified Rated Air Flow Rate [m3/s]', &
                             SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate)
      END IF
    ELSE
      CALL CheckZoneSizing('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                          SimpleWatertoAirHP(HPNum)%Name)
      RatedAirVolFlowRateDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (RatedAirVolFlowRateDes < SmallAirVolFlow) THEN
        RatedAirVolFlowRateDes = 0.0d0
      END IF
    END IF
  END IF
  IF (.NOT. HardSizeNoDesRun) THEN
    IF (IsAutosize) THEN
      SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate = RatedAirVolFlowRateDes
      CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Air Flow Rate [m3/s]', &
                             RatedAirVolFlowRateDes)
    ELSE
      IF (SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate > 0.0d0 .AND. RatedAirVolFlowRateDes > 0.0d0 &
              .AND. .NOT. HardSizeNoDesRun) THEN
        RatedAirVolFlowRateUser = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Air Flow Rate [m3/s]', &
                             RatedAirVolFlowRateDes, &
                            'User-Specified Rated Air Flow Rate [m3/s]', &
                             RatedAirVolFlowRateUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(RatedAirVolFlowRateDes - RatedAirVolFlowRateUser)/RatedAirVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeHVACWaterToAir: Potential issue with equipment sizing for coil ' &
                                  //TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)// &
                                  ':WATERTOAIRHEATPUMP:EQUATIONFIT "'// TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
            CALL ShowContinueError('User-Specified Rated Air Volume Flow Rate of '// &
                                      TRIM(RoundSigDigits(RatedAirVolFlowRateUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Rated Air Volume Flow Rate of ' // &
                                      TRIM(RoundSigDigits(RatedAirVolFlowRateDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

  RatedCapCoolTotalAutosized = .FALSE.
  RatedCapCoolSensAutosized  = .FALSE.

IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'COOLING') THEN
! size rated total cooling capacity
  IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal == AutoSize .AND. &
      SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'COOLING') THEN
    RatedCapCoolTotalAutosized = .TRUE.
  END IF
  IF (SizingDesRunThisAirSys .OR. SizingDesRunThisZone) HardSizeNoDesRun = .FALSE.
  IF (CurSysNum > 0) THEN
    IF (.NOT. RatedCapCoolTotalAutosized .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'User-Specified Rated Total Cooling Capacity [W]', &
                             SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
      END IF
    ELSE
      CALL CheckSysSizing('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                          SimpleWatertoAirHP(HPNum)%Name)
      VolFlowRate = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF (CurOASysNum > 0) THEN ! coil is in the OA stream
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
        ELSE ! coil is on the main air loop
          SupTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
          SupHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
          IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
            MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
            MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
          ELSE ! there is precooling of OA stream
            IF (VolFlowRate > 0.0d0) THEN
              OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                        (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
            MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
          END IF
        END IF
        OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
        rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
        TotalCapCoeff1   = SimpleWatertoAirHP(HPNum)%TotalCoolCap1
        TotalCapCoeff2   = SimpleWatertoAirHP(HPNum)%TotalCoolCap2
        TotalCapCoeff3   = SimpleWatertoAirHP(HPNum)%TotalCoolCap3
        TotalCapCoeff4   = SimpleWatertoAirHP(HPNum)%TotalCoolCap4
        TotalCapCoeff5   = SimpleWatertoAirHP(HPNum)%TotalCoolCap5
        ratioTWB         = (MixWetBulb+273.15d0)/283.15d0
        ! rated condenser water inlet temperature of 85F
        ratioTS          = (((85.0d0 - 32.0d0)/1.8d0)+273.15d0)/283.15d0
        TotCapTempModFac = TotalCapCoeff1 + (ratioTWB * TotalCapCoeff2) + (ratioTS * TotalCapCoeff3) +   &
                             (1.0d0 * TotalCapCoeff4) + (1.0d0 * TotalCapCoeff5)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixEnth .GT. SupEnth)THEN
          CoolCapAtPeak = rhoair * VolFlowRate * (MixEnth-SupEnth)
        ELSE
          CoolCapAtPeak = rhoair * VolFlowRate * (48000.0d0-SupEnth)
        END IF
        CoolCapAtPeak = MAX(0.0d0, CoolCapAtPeak)
        IF(TotCapTempModFac .GT. 0.0d0)THEN
          RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac
        ELSE
          RatedCapCoolTotalDes = CoolCapAtPeak
        END IF
      ELSE
        RatedCapCoolTotalDes = 0.0d0
      END IF
    END IF
  ELSE IF (CurZoneEqNum > 0) THEN
    IF (.NOT. RatedCapCoolTotalAutosized .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'User-Specified Rated Total Cooling Capacity [W]', &
                             SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
      END IF
    ELSE
      CALL CheckZoneSizing('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                          SimpleWatertoAirHP(HPNum)%Name)
      VolFlowRate = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF(ZoneEqDXCoil)THEN
          IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
            MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
          ELSE
            MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
          END IF
        ELSE
          MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
          MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
        END IF
        SupTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
        SupHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
        TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
        DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
        IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
          OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
        ELSE
          OutTemp = 0.0d0
        ENDIF
        rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
        TotalCapCoeff1   = SimpleWatertoAirHP(HPNum)%TotalCoolCap1
        TotalCapCoeff2   = SimpleWatertoAirHP(HPNum)%TotalCoolCap2
        TotalCapCoeff3   = SimpleWatertoAirHP(HPNum)%TotalCoolCap3
        TotalCapCoeff4   = SimpleWatertoAirHP(HPNum)%TotalCoolCap4
        TotalCapCoeff5   = SimpleWatertoAirHP(HPNum)%TotalCoolCap5
        ratioTWB         = (MixWetBulb+273.15d0)/283.15d0
        ! rated condenser water inlet temperature of 85F
        ratioTS          = (((85.0d0 - 32.0d0)/1.8d0)+273.15d0)/283.15d0
        TotCapTempModFac = TotalCapCoeff1 + (ratioTWB * TotalCapCoeff2) + (ratioTS * TotalCapCoeff3) +   &
                             (1.0d0 * TotalCapCoeff4) + (1.0d0 * TotalCapCoeff5)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixEnth .GT. SupEnth)THEN
          CoolCapAtPeak = rhoair * VolFlowRate * (MixEnth-SupEnth)
        ELSE
          CoolCapAtPeak = rhoair * VolFlowRate * (48000.0d0-SupEnth)
        END IF
        CoolCapAtPeak = MAX(0.0d0, CoolCapAtPeak)
        IF(TotCapTempModFac .GT. 0.0d0)THEN
          RatedCapCoolTotalDes = CoolCapAtPeak / TotCapTempModFac
        ELSE
          RatedCapCoolTotalDes = CoolCapAtPeak
        END IF
      ELSE
        RatedCapCoolTotalDes = 0.0d0
      END IF
    END IF
    IF (RatedCapCoolTotalDes < SmallLoad) THEN
      RatedCapCoolTotalDes = 0.0d0
    END IF
  END IF
  IF (.NOT. HardSizeNoDesRun) THEN
    IF (RatedCapCoolTotalAutosized) THEN
      SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal = RatedCapCoolTotalDes
      CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Total Cooling Capacity [W]', &
                             RatedCapCoolTotalDes)
      CALL PreDefTableEntry(pdchCoolCoilTotCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
      CALL PreDefTableEntry(pdchCoolCoilLatCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal &
                                 - SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal /= 0.0d0) THEN
        CALL PreDefTableEntry(pdchCoolCoilSHR,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolSens &
                                   / SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
        CALL PreDefTableEntry(pdchCoolCoilNomEff,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedPowerCool &
                                   / SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
      ELSE
        CALL PreDefTableEntry(pdchCoolCoilSHR,SimpleWatertoAirHP(HPNum)%Name,0.0d0)
        CALL PreDefTableEntry(pdchCoolCoilNomEff,SimpleWatertoAirHP(HPNum)%Name,0.0d0)
      ENDIF
    ELSE ! Hardsized with sizing data
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal > 0.0d0 .AND. RatedCapCoolTotalDes > 0.0d0 &
                     .AND. .NOT. HardSizeNoDesRun) THEN
        RatedCapCoolTotalUser = SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Total Cooling Capacity [W]', &
                             RatedCapCoolTotalDes, &
                            'User-Specified Rated Total Cooling Capacity [W]', &
                             RatedCapCoolTotalUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(RatedCapCoolTotalDes - RatedCapCoolTotalUser)/RatedCapCoolTotalUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeHVACWaterToAir: Potential issue with equipment sizing for coil ' &
                                  //TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)// &
                                  ':WATERTOAIRHEATPUMP:EQUATIONFIT "'// TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
            CALL ShowContinueError('User-Specified Rated Total Cooling Capacity of '// &
                                      TRIM(RoundSigDigits(RatedCapCoolTotalUser,2))// ' [W]')
            CALL ShowContinueError('differs from Design Size Rated Total Cooling Capacity of ' // &
                                      TRIM(RoundSigDigits(RatedCapCoolTotalDes,2))// ' [W]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

! Set the global DX cooling coil capacity variable for use by other objects
  IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'COOLING') THEN
    DXCoolCap = SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal
  END IF

! size rated sensible cooling capacity
  IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolSens == AutoSize .AND. &
      SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'COOLING') THEN
    RatedCapCoolSensAutosized  = .TRUE.
  END IF
  IF (SizingDesRunThisAirSys .OR. SizingDesRunThisZone)  HardSizeNoDesRun = .FALSE.
  IF (CurSysNum > 0) THEN
    IF (.NOT. RatedCapCoolSensAutosized .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolSens > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'User-Specified Rated Sensible Cooling Capacity [W]', &
                             SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
      END IF
    ELSE
      CALL CheckSysSizing('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                          SimpleWatertoAirHP(HPNum)%Name)
      VolFlowRate = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF (CurOASysNum > 0) THEN ! coil is in the OA stream
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
        ELSE ! coil is on the main air loop
          SupTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
          SupHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
          IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
            MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
            MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
          ELSE ! there is precooling of OA stream
            IF (VolFlowRate > 0.0d0) THEN
              OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                        (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
            MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
          END IF
        END IF
        OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
        rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
        SensCapCoeff1    = SimpleWatertoAirHP(HPNum)%SensCoolCap1
        SensCapCoeff2    = SimpleWatertoAirHP(HPNum)%SensCoolCap2
        SensCapCoeff3    = SimpleWatertoAirHP(HPNum)%SensCoolCap3
        SensCapCoeff4    = SimpleWatertoAirHP(HPNum)%SensCoolCap4
        SensCapCoeff5    = SimpleWatertoAirHP(HPNum)%SensCoolCap5
        SensCapCoeff6    = SimpleWatertoAirHP(HPNum)%SensCoolCap6
        ratioTDB         = (MixTemp+273.15d0)/283.15d0
        ratioTWB         = (MixWetBulb+273.15d0)/283.15d0
        ! rated condenser water inlet temperature of 85F
        ratioTS          = (((85.0d0 - 32.0d0)/1.8d0)+273.15d0)/283.15d0
        CpAir = PsyCpAirFnWTdb(SupHumRat,SupTemp,RoutineName)
        SensCapTempModFac = SensCapCoeff1 + (ratioTDB * SensCapCoeff2) + (ratioTWB * SensCapCoeff3) +   &
                            (ratioTS * SensCapCoeff4) + (1.0d0 * SensCapCoeff5) + (1.0d0 * SensCapCoeff6)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixTemp .GT. SupTemp)THEN
          SensCapAtPeak = rhoair * VolFlowRate * CpAir * (MixTemp-SupTemp)
        ELSE
          SensCapAtPeak = rhoair * VolFlowRate * CpAir * (24.0d0-SupTemp)
        END IF
        SensCapAtPeak = MAX(0.0d0, SensCapAtPeak)
        RatedCapCoolSensDes = SensCapAtPeak / SensCapTempModFac
      ELSE
        RatedCapCoolSensDes = 0.0d0
      END IF
    END IF
  ELSE IF (CurZoneEqNum > 0) THEN
    IF (.NOT. RatedCapCoolSensAutosized .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolSens > 0.0d0) THEN
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'User-Specified Rated Sensible Cooling Capacity [W]', &
                             SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
      END IF
    ELSE
      CALL CheckZoneSizing('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                          SimpleWatertoAirHP(HPNum)%Name)
      VolFlowRate = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF(ZoneEqDXCoil)THEN
          IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0d0) THEN
            MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
          ELSE
            MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
          END IF
        ELSE
          MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
          MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
        END IF
        SupTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
        SupHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
        TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
        DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
        IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
          OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
        ELSE
          OutTemp = 0.0d0
        ENDIF
        rhoair = PsyRhoAirFnPbTdbW(StdBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,StdBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)
        SensCapCoeff1    = SimpleWatertoAirHP(HPNum)%SensCoolCap1
        SensCapCoeff2    = SimpleWatertoAirHP(HPNum)%SensCoolCap2
        SensCapCoeff3    = SimpleWatertoAirHP(HPNum)%SensCoolCap3
        SensCapCoeff4    = SimpleWatertoAirHP(HPNum)%SensCoolCap4
        SensCapCoeff5    = SimpleWatertoAirHP(HPNum)%SensCoolCap5
        SensCapCoeff6    = SimpleWatertoAirHP(HPNum)%SensCoolCap6
        ratioTDB         = (MixTemp+273.15d0)/283.15d0
        ratioTWB         = (MixWetBulb+273.15d0)/283.15d0
        ! rated condenser water inlet temperature of 85F
        ratioTS          = (((85.0d0 - 32.0d0)/1.8d0)+273.15d0)/283.15d0
        CpAir = PsyCpAirFnWTdb(SupHumRat,SupTemp,RoutineName)
        SensCapTempModFac = SensCapCoeff1 + (ratioTDB * SensCapCoeff2) + (ratioTWB * SensCapCoeff3) +   &
                            (ratioTS * SensCapCoeff4) + (1.0d0 * SensCapCoeff5) + (1.0d0 * SensCapCoeff6)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixTemp .GT. SupTemp)THEN
          SensCapAtPeak = rhoair * VolFlowRate * CpAir * (MixTemp-SupTemp)
        ELSE
          SensCapAtPeak = rhoair * VolFlowRate * CpAir * (24.0d0-SupTemp)
        END IF
        SensCapAtPeak = MAX(0.0d0, SensCapAtPeak)
        IF(SensCapTempModFac .GT. 0.0d0)THEN
          RatedCapCoolSensDes = SensCapAtPeak / SensCapTempModFac
        ELSE
          RatedCapCoolSensDes = SensCapAtPeak
        END IF
      ELSE
        RatedCapCoolSensDes = 0.0d0
      END IF
    END IF
  END IF
  IF (RatedCapCoolSensDes < SmallLoad) THEN
    RatedCapCoolSensDes = 0.0d0
  END IF
  IF (.NOT. HardSizeNoDesRun) THEN
    IF (RatedCapCoolSensAutosized) THEN
      SimpleWatertoAirHP(HPNum)%RatedCapCoolSens = RatedCapCoolSensDes
      CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Sensible Cooling Capacity [W]', &
                             RatedCapCoolSensDes)
      CALL PreDefTableEntry(pdchCoolCoilSensCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
      CALL PreDefTableEntry(pdchCoolCoilLatCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal &
                                 - SimpleWatertoAirHP(HPNum)%RatedCapCoolSens)
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal /= 0.0d0) THEN
        CALL PreDefTableEntry(pdchCoolCoilSHR,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapCoolSens &
                                 / SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)
      ELSE
        CALL PreDefTableEntry(pdchCoolCoilSHR,SimpleWatertoAirHP(HPNum)%Name,0.0d0)
      ENDIF
    ELSE
      IF (SimpleWatertoAirHP(HPNum)%RatedCapCoolSens > 0.0d0 .AND. RatedCapCoolSensDes > 0.0d0) THEN
        RatedCapCoolSensUser = SimpleWatertoAirHP(HPNum)%RatedCapCoolSens
        CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Sensible Cooling Capacity [W]', &
                             RatedCapCoolSensDes, &
                            'User-Specified Rated Sensible Cooling Capacity [W]', &
                             RatedCapCoolSensUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(RatedCapCoolSensDes - RatedCapCoolSensUser)/RatedCapCoolSensUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeHVACWaterToAir: Potential issue with equipment sizing for coil ' &
                                    //TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)// &
                                  ':WATERTOAIRHEATPUMP:EQUATIONFIT "'// TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
            CALL ShowContinueError('User-Specified Rated Sensible Cooling Capacity of '// &
                                      TRIM(RoundSigDigits(RatedCapCoolSensUser,2))// ' [W]')
            CALL ShowContinueError('differs from Design Size Rated Sensible Cooling Capacity of ' // &
                                      TRIM(RoundSigDigits(RatedCapCoolSensDes,2))// ' [W]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

! test autosized sensible and total cooling capacity for total > sensible
  IF(RatedCapCoolSensAutosized .AND. RatedCapCoolTotalAutosized .OR. &
     RatedCapCoolSensAutosized)THEN
    IF(SimpleWatertoAirHP(HPNum)%RatedCapCoolSens .GT. SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)THEN
        CALL ShowWarningError('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT "'// &
                               TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Rated Sensible Cooling Capacity > Rated Total Cooling Capacity')
        CALL ShowContinueError('Each of these capacity inputs have been autosized.')
        CALL ShowContinueError('Rated Sensible Cooling Capacity = '// &
                                TRIM(TrimSigDigits(SimpleWatertoAirHP(HPNum)%RatedCapCoolSens,2))//' W')
        CALL ShowContinueError('Rated Total Cooling Capacity    = '// &
                                TRIM(TrimSigDigits(SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal,2))//' W')
        CALL ShowContinueError('See eio file for further details.')
        CALL ShowContinueError('Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.')
        CALL ShowContinueError('Check Zone and System Sizing objects to verify sizing inputs.')
        CALL ShowContinueError('Sizing statistics:')
        CALL ShowContinueError('Entering Air Dry-Bulb Temperature = '//TRIM(TrimSigDigits(MixTemp,3))//' C')
        CALL ShowContinueError('Entering Air Wet-Bulb Temperature = '//TRIM(TrimSigDigits(MixWetBulb,3))//' C')
        CALL ShowContinueError('Entering Condenser Water Temperature used = 24.4444 C')
        CALL ShowContinueError('Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)')
        CALL ShowContinueError('ratioTDB = '//TRIM(TrimSigDigits(((MixTemp+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTWB = '//TRIM(TrimSigDigits(((MixWetBulb+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTS  = '//TRIM(TrimSigDigits(((85.0d0+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('Sensible Cooling Capacity Modifier = '//TRIM(TrimSigDigits(SensCapTempModFac,5)))
        CALL ShowContinueError('...Rated Sensible Cooling Capacity = Sensible Design Load / Sensible Cooling Capacity Modifier')
        CALL ShowContinueError('Total Cooling Capacity Modifier = '//TRIM(TrimSigDigits(TotCapTempModFac,5)))
        CALL ShowContinueError('...Rated Total Cooling Capacity = Total Design Load / Total Cooling Capacity Modifier')
        CALL ShowContinueError('Carefully review the Load Side Total, Sensible, and Latent heat transfer rates')
        CALL ShowContinueError('... to ensure they meet the expected manufacturers performance specifications.')
    END IF
  ELSE If(RatedCapCoolTotalAutosized)THEN
    IF(SimpleWatertoAirHP(HPNum)%RatedCapCoolSens .GT. SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal)THEN
        CALL ShowWarningError('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT "'// &
                               TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Rated Sensible Cooling Capacity > Rated Total Cooling Capacity')
        CALL ShowContinueError('Only the rated total capacity input is autosized, consider autosizing both inputs.')
        CALL ShowContinueError('Rated Sensible Cooling Capacity = '// &
                                TRIM(TrimSigDigits(SimpleWatertoAirHP(HPNum)%RatedCapCoolSens,2))//' W')
        CALL ShowContinueError('Rated Total Cooling Capacity    = '// &
                                TRIM(TrimSigDigits(SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal,2))//' W')
        CALL ShowContinueError('See eio file for further details.')
        CALL ShowContinueError('Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.')
        CALL ShowContinueError('Check Zone and System Sizing objects to verify sizing inputs.')
        CALL ShowContinueError('Sizing statistics for Total Cooling Capacity:')
        CALL ShowContinueError('Entering Air Wet-Bulb Temperature = '//TRIM(TrimSigDigits(MixWetBulb,3))//' C')
        CALL ShowContinueError('Entering Condenser Water Temperature used = 24.4444 C')
        CALL ShowContinueError('Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)')
        CALL ShowContinueError('ratioTWB = '//TRIM(TrimSigDigits(((MixWetBulb+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTS  = '//TRIM(TrimSigDigits(((85.0d0+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('Sensible Cooling Capacity Modifier = '//TRIM(TrimSigDigits(SensCapTempModFac,5)))
        CALL ShowContinueError('...Rated Sensible Cooling Capacity = Sensible Design Load / Sensible Cooling Capacity Modifier')
        CALL ShowContinueError('Carefully review the Load Side Total, Sensible, and Latent heat transfer rates')
        CALL ShowContinueError('... to ensure they meet the expected manufacturers performance specifications.')
    END IF
  END IF

ENDIF ! Cooling Coild

IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'HEATING') THEN
! size rated heating capacity
  IsAutosize = .FALSE.
  IF (SimpleWatertoAirHP(HPNum)%RatedCapHeat == AutoSize .AND. &
      SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'HEATING') THEN
    IsAutosize = .TRUE.
  END IF
!   simply set heating capacity equal to the cooling capacity
  IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'HEATING') THEN
    RatedCapHeatDes = DXCoolCap
    IF (RatedCapHeatDes ==  autosize) THEN
      CALL ShowWarningError('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT "'// &
                            TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
      CALL ShowContinueError(RoutineName//': Heating coil could not be autosized since cooling coil was not previously sized.')
      CALL ShowContinueError('... Cooling coil must be upstream of heating coil.')
      CALL ShowContinueError('... Manually sizing this heating coil will be required.')
    END IF
    IF (RatedCapHeatDes < SmallLoad) THEN
      RatedCapHeatDes = 0.0d0
    END IF
  END IF
  IF (IsAutosize) THEN
    SimpleWatertoAirHP(HPNum)%RatedCapHeat = RatedCapHeatDes
    CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Heating Capacity [W]', &
                             RatedCapHeatDes)
    CALL PreDefTableEntry(pdchHeatCoilNomCap,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedCapHeat)
    IF (SimpleWatertoAirHP(HPNum)%RatedCapHeat /= 0.0d0) THEN
      CALL PreDefTableEntry(pdchHeatCoilNomEff,SimpleWatertoAirHP(HPNum)%Name,SimpleWatertoAirHP(HPNum)%RatedPowerHeat &
                             / SimpleWatertoAirHP(HPNum)%RatedCapHeat)
    ELSE
      CALL PreDefTableEntry(pdchHeatCoilNomEff,SimpleWatertoAirHP(HPNum)%Name,0.0d0)
    ENDIF
  ELSE
    IF (SimpleWatertoAirHP(HPNum)%RatedCapHeat > 0.0d0 .AND. RatedCapHeatDes > 0.0d0 .AND. .NOT. HardSizeNoDesRun) THEN
      RatedCapHeatUser = SimpleWatertoAirHP(HPNum)%RatedCapHeat
      CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                             SimpleWatertoAirHP(HPNum)%Name, &
                            'Design Size Rated Heating Capacity [W]', &
                             RatedCapHeatDes, &
                            'User-Specified Rated Heating Capacity [W]', &
                             RatedCapHeatUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(RatedCapHeatDes - RatedCapHeatUser)/RatedCapHeatUser) > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeHVACWaterToAir: Potential issue with equipment sizing for coil ' &
                                    //TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)// &
                                  ':WATERTOAIRHEATPUMP:EQUATIONFIT "'// TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
          CALL ShowContinueError('User-Specified Rated Heating Capacity of '// &
                                      TRIM(RoundSigDigits(RatedCapHeatUser,2))// ' [W]')
          CALL ShowContinueError('differs from Design Size Rated Heating Capacity of ' // &
                                      TRIM(RoundSigDigits(RatedCapHeatDes,2))// ' [W]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

  ! Check that heat pump heating capacity is within 20% of cooling capacity. Check only for heating coil and report both.
  IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'HEATING' .AND. &
      SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum .GT. 0)THEN

    IF(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal .GT. 0.0D0)THEN

      IF(ABS(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal-&
           SimpleWatertoAirHP(HPNum)%RatedCapHeat)/&
           SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal .GT. 0.2d0) THEN

        CALL ShowWarningError('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)// &
                            ':WATERTOAIRHEATPUMP:EQUATIONFIT "'//TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
        CALL ShowContinueError('...used with COIL:'// &
               TRIM(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%WaterToAirHPType)// &
                            ':WATERTOAIRHEATPUMP:EQUATIONFIT "'// &
               TRIM(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%Name)//'"')
        CALL ShowContinueError('...heating capacity is disproportionate (> 20% different) to total cooling capacity')
        CALL ShowContinueError('...heating capacity = '//TRIM(TrimSigDigits(SimpleWatertoAirHP(HPNum)%RatedCapHeat,3))//' W')
        CALL ShowContinueError('...cooling capacity = '// &
           TRIM(TrimSigDigits(SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal,3))//' W')

      END IF

    END IF

  END IF

ENDIF ! Heating

! size rated power
  IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'COOLING') THEN

    SimpleWatertoAirHP(HPNum)%RatedPowerCool = SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal/SimpleWatertoAirHP(HPNum)%RatedCOPCool

  ELSE IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'HEATING') THEN

    SimpleWatertoAirHP(HPNum)%RatedPowerHeat = SimpleWatertoAirHP(HPNum)%RatedCapHeat/SimpleWatertoAirHP(HPNum)%RatedCOPHeat

  END IF

! Size water volumetric flow rate
  IsAutosize = .FALSE.
  IF (SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate == AutoSize)THEN
    IsAutosize = .TRUE.
  END IF

!   WSHP condenser can be on either a plant loop or condenser loop. Test each to find plant sizing number.
!   first check to see if coil is connected to a plant loop, no warning on this CALL
  IF (IsAutosize) THEN
    PltSizNum = &
        MyPlantSizingIndex('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                           SimpleWatertoAirHP(HPNum)%Name, &
                           SimpleWatertoAirHP(HPNum)%WaterInletNodeNum, &
                           SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum, ErrorsFound, .FALSE.)

!!   if not found on a plant loop, check condenser loop and warn user if not found
!    IF(PltSizNum == 0) THEN
!
!      PltSizNum = &
!          MyCondPlantSizingIndex('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
!                                 SimpleWatertoAirHP(HPNum)%Name, &
!                                 SimpleWatertoAirHP(HPNum)%WaterInletNodeNum, &
!                                 SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum, ErrorsFound)
!    END IF

    IF (PltSizNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidName, &
                             PlantSizData(PltSizNum)%ExitTemp, &
                             PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidIndex, &
                             'SizeHVACWaterToAir')
      Cp  = GetSpecificHeatGlycol(PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidName, &
                                  PlantSizData(PltSizNum)%ExitTemp, &
                                  PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidIndex, &
                                 'SizeHVACWaterToAir')

      IF (SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'HEATING') THEN

        RatedWaterVolFlowRateDes = SimpleWatertoAirHP(HPNum)%RatedCapHeat / &
                                   ( PlantSizData(PltSizNum)%DeltaT * Cp * rho )
      ELSEIF(SimpleWatertoAirHP(HPNum)%WaterToAirHPType == 'COOLING') THEN

!       use companion heating coil capacity to calculate volumetric flow rate
        IF(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum .GT. 0)THEN
          SystemCapacity = SimpleWatertoAirHP(SimpleWatertoAirHP(HPNum)%CompanionCoolingCoilNum)%RatedCapHeat
        ELSE
          SystemCapacity = SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal
        END IF

        RatedWaterVolFlowRateDes = &
                                SystemCapacity / &
                                ( PlantSizData(PltSizNum)%DeltaT * Cp * rho )
      END IF
    ELSE
      CALL ShowSevereError('Autosizing of water flow requires a loop Sizing:Plant object')
      CALL ShowContinueError('Autosizing also requires physical connection to a plant or condenser loop.')
      CALL ShowContinueError('Occurs in ' // &
             'COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT' // ' Object=' &
                //TRIM(SimpleWatertoAirHP(HPNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF
  IF (IsAutosize) THEN
    SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate = RatedWaterVolFlowRateDes
    CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                                  SimpleWatertoAirHP(HPNum)%Name, &
                                  'Design Size Rated Water Flow Rate [m3/s]', RatedWaterVolFlowRateDes)
  ELSE
    IF (SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate > 0.0d0 .AND. RatedWaterVolFlowRateDes > 0.0d0) THEN
      RatedWaterVolFlowRateUser = SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate
      CALL ReportSizingOutput('COIL:'//TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:EQUATIONFIT', &
                                  SimpleWatertoAirHP(HPNum)%Name, &
                                  'Design Size Rated Water Flow Rate [m3/s]', RatedWaterVolFlowRateDes, &
                                  'User-Specified Rated Water Flow Rate [m3/s]', RatedWaterVolFlowRateUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(RatedWaterVolFlowRateDes - RatedWaterVolFlowRateUser)/RatedWaterVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeHVACWaterToAir: Potential issue with equipment sizing for coil ' &
                                  //TRIM(SimpleWatertoAirHP(HPNum)%WaterToAirHPType)// &
                                  ':WATERTOAIRHEATPUMP:EQUATIONFIT "'// TRIM(SimpleWatertoAirHP(HPNum)%Name)//'"')
          CALL ShowContinueError('User-Specified Rated Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(RatedWaterVolFlowRateUser,5))// ' [m3/s]')
          CALL ShowContinueError('differs from Design Size Rated Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(RatedWaterVolFlowRateDes,5))// ' [m3/s]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

! Save component design water volumetric flow rate.
! Use 1/2 flow since both cooling and heating coil will save flow yet only 1 will operate at a time
  IF(SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate .GT. 0.0d0)THEN
    CALL RegisterPlantCompDesignFlow(SimpleWatertoAirHP(HPNum)%WaterInletNodeNum,  &
       0.5d0*SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate)
  END IF

  RETURN

END SUBROUTINE SizeHVACWaterToAir


SUBROUTINE CalcHPCoolingSimple(HPNum,CyclingScheme,RuntimeFrac,SensDemand,LatentDemand,CompOp,PartLoadRatio,  &
                               OnOffAirFlowRatio,WaterPartLoad)


          !       AUTHOR         Arun Shenoy
          !       DATE WRITTEN   Jan 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for simulating the cooling mode of the Water to Air HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Simulate the heat pump performance using the coefficients and rated conditions
          !
          ! If the LatDegradModelSimFlag is enabled, the coil will be simulated twice:
          ! (1)first simulation at the rated conditions (2) second simulation at the
          ! actual operating conditions. Then call CalcEffectiveSHR and the effective SHR
          ! is adjusted.
          !
          ! If the LatDegradModelSimFlag is disabled, the cooling coil is only simulated
          ! once at the actual operating conditions.
          !
          ! Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
          ! and RuntimeFrac.

          ! REFERENCES:
          ! (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
          ! M.S. Thesis, University of Illinois at Urbana Champaign.
          ! (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)
          ! (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)
          ! (4) Henderson, H.I., K. Rengarajan.1996. A Model to Predict the Latent
          ! Capacity of Air Conditioners and Heat Pumps at Part-Load Conditions
          ! with Constant Fan Operation ASHRAE Transactions 102 (1), pp. 266-274.

          ! USE STATEMENTS:
  USE DataHVACGlobals,      ONLY: TimeStepSys, DXElecCoolingPower
  USE Psychrometrics,       ONLY: PsyWFnTdbTwbPb,PsyCpAirFnWTdb,PsyHFnTdbW,PsyRhoAirFnPbTdbW,  &
                                  PsyTwbFnTdbWPb,PsyTdbFnHW,PsyWFnTdbH
  USE FluidProperties,      ONLY: GetSpecificHeatGlycol
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER,   INTENT(IN) :: HPNum              ! Heat Pump Number
  INTEGER,   INTENT(IN) :: CyclingScheme      ! Fan/Compressor cycling scheme indicator
  REAL(r64), INTENT(IN) :: RuntimeFrac        ! Runtime Fraction of compressor or percent on time (on-time/cycle time)
  REAL(r64), INTENT(IN) :: SensDemand         ! Cooling Sensible Demand [W] !unused1208
  REAL(r64), INTENT(IN) :: LatentDemand       ! Cooling Latent Demand [W]
  INTEGER,   INTENT(IN) :: CompOp             ! compressor operation flag
  REAL(r64), INTENT(IN) :: PartLoadRatio      ! compressor part load ratio
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio  ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: WaterPartLoad      ! water part load ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Tref=283.15d0      ! Reference Temperature for performance curves,10C [K]
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcHPCoolingSimple'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: TotalCapRated          ! Rated Total Cooling Capacity [W]
  REAL(r64) :: SensCapRated           ! Rated Sensible Cooling Capacity [W]
  REAL(r64) :: CoolPowerRated         ! Rated Cooling Power Input[W]
  REAL(r64) :: AirVolFlowRateRated    ! Rated Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: WaterVolFlowRateRated  ! Rated Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: TotalCapCoeff1         ! 1st coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff2         ! 2nd coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff3         ! 3rd coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff4         ! 4th coefficient of the total cooling capacity performance curve
  REAL(r64) :: TotalCapCoeff5         ! 5th coefficient of the total cooling capacity performance curve
  REAL(r64) :: SensCapCoeff1          ! 1st coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff2          ! 2nd coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff3          ! 3rd coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff4          ! 4th coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff5          ! 5th coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: SensCapCoeff6          ! 6th coefficient of the sensible cooling capacity performance curve
  REAL(r64) :: CoolPowerCoeff1        ! 1st coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff2        ! 2nd coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff3        ! 3rd coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff4        ! 4th coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff5        ! 5th coefficient of the cooling power consumption curve
  REAL(r64) :: Twet_rated             ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated            ! Gamma at rated conditions (coil air flow rate and air temperatures)

  REAL(r64) :: SHRss                  ! Sensible heat ratio at steady state
  REAL(r64) :: SHReff                 ! Effective sensible heat ratio at part-load condition
!  REAL(r64) :: PartLoadRatio          ! Part load ratio

  REAL(r64) :: ratioTDB               ! Ratio of the inlet air dry bulb temperature to the rated conditions
  REAL(r64) :: ratioTWB               ! Ratio of the inlet air wet bulb temperature to the rated conditions
  REAL(r64) :: ratioTS                ! Ratio of the source side(water) inlet temperature to the rated conditions
  REAL(r64) :: ratioVL                ! Ratio of the air flow rate to the rated conditions
  REAL(r64) :: ratioVS                ! Ratio of the water flow rate to the rated conditions
  REAL(r64) :: CpWater                ! Specific heat of water [J/kg_C]
  REAL(r64) :: CpAir                  ! Specific heat of air [J/kg_C]
  REAL(r64) :: ReportingConstant

  LOGICAL :: LatDegradModelSimFlag  ! Latent degradation model simulation flag
  INTEGER :: NumIteration           ! Iteration Counter
  INTEGER, SAVE :: Count=0          ! No idea what this is for.
  LOGICAL, SAVE    :: FirstTime = .true.
  REAL(r64), SAVE  :: LoadSideInletDBTemp_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletWBTemp_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletHumRat_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletEnth_Init ! rated conditions
  REAL(r64), SAVE  :: CpAir_Init                ! rated conditions
  REAL(r64)        :: LoadSideInletDBTemp_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletWBTemp_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletHumRat_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletEnth_Unit ! calc conditions for unit
  REAL(r64)        :: CpAir_Unit                ! calc conditions for unit

  IF (FirstTime) THEN
    !Set indoor air conditions to the rated condition
    LoadSideInletDBTemp_Init = 26.7d0
    LoadSideInletHumRat_Init = 0.0111d0
    LoadSideInletEnth_Init = PsyHFnTdbW(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init,RoutineName//':Init')
    CpAir_Init = PsyCpAirFnWTdb(LoadSideInletHumRat_Init,LoadSideInletDBTemp_Init,RoutineName//':Init')
    FirstTime=.false.
  ENDIF
  LoadSideInletWBTemp_Init = PsyTwbFnTdbWPb(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init,OutBaroPress,RoutineName)



 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

  TotalCapRated          = SimpleWatertoAirHP(HPNum)%RatedCapCoolTotal
  SensCapRated           = SimpleWatertoAirHP(HPNum)%RatedCapCoolSens
  CoolPowerRated         = SimpleWatertoAirHP(HPNum)%RatedPowerCool
  AirVolFlowRateRated    = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
  WaterVolFlowRateRated  = SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate

  TotalCapCoeff1         = SimpleWatertoAirHP(HPNum)%TotalCoolCap1
  TotalCapCoeff2         = SimpleWatertoAirHP(HPNum)%TotalCoolCap2
  TotalCapCoeff3         = SimpleWatertoAirHP(HPNum)%TotalCoolCap3
  TotalCapCoeff4         = SimpleWatertoAirHP(HPNum)%TotalCoolCap4
  TotalCapCoeff5         = SimpleWatertoAirHP(HPNum)%TotalCoolCap5
  SensCapCoeff1          = SimpleWatertoAirHP(HPNum)%SensCoolCap1
  SensCapCoeff2          = SimpleWatertoAirHP(HPNum)%SensCoolCap2
  SensCapCoeff3          = SimpleWatertoAirHP(HPNum)%SensCoolCap3
  SensCapCoeff4          = SimpleWatertoAirHP(HPNum)%SensCoolCap4
  SensCapCoeff5          = SimpleWatertoAirHP(HPNum)%SensCoolCap5
  SensCapCoeff6          = SimpleWatertoAirHP(HPNum)%SensCoolCap6
  CoolPowerCoeff1        = SimpleWatertoAirHP(HPNum)%CoolPower1
  CoolPowerCoeff2        = SimpleWatertoAirHP(HPNum)%CoolPower2
  CoolPowerCoeff3        = SimpleWatertoAirHP(HPNum)%CoolPower3
  CoolPowerCoeff4        = SimpleWatertoAirHP(HPNum)%CoolPower4
  CoolPowerCoeff5        = SimpleWatertoAirHP(HPNum)%CoolPower5
  Twet_rated             = SimpleWatertoAirHP(HPNum)%Twet_rated
  Gamma_rated            = SimpleWatertoAirHP(HPNum)%Gamma_rated

  LoadSideMassFlowRate   = SimpleWatertoAirHP(HPNum)%AirMassFlowRate
  SourceSideMassFlowRate = SimpleWatertoAirHP(HPNum)%WaterMassFlowRate
  SourceSideInletTemp    = SimpleWatertoAirHP(HPNum)%InletWaterTemp
  SourceSideInletEnth    = SimpleWatertoAirHP(HPNum)%InletWaterEnthalpy
  CpWater = GetSpecificHeatGlycol(PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidName, &
                                   SourceSideInletTemp, &
                                   PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidIndex,  &
                                  'CalcHPCoolingSimple:SourceSideInletTemp')

   !Check for flows, do not perform simulation if no flow in load side or source side.
  IF (SourceSideMassFlowRate <= 0.0d0 .OR. LoadSideMassFlowRate <= 0.0d0)THEN
     SimpleWatertoAirHP(HPNum)%SimFlag = .FALSE.
     RETURN
  ELSE
     SimpleWatertoAirHP(HPNum)%SimFlag = .TRUE.
  ENDIF

  IF (CompOp .EQ. 0) THEN
     SimpleWaterToAirHP(HPNum)%SimFlag = .FALSE.
     RETURN
  ENDIF

  !Loop the calculation at least once depending whether the latent degradation model
  !is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
  !and 2nd iteration to calculate the  QLatent(actual)
  IF((RuntimeFrac .GE. 1.0d0) .OR. (Twet_rated .LE. 0.0d0) .OR. (Gamma_rated .LE. 0.0d0)) THEN
    LatDegradModelSimFlag = .FALSE.
    !Set NumIteration=1 so that latent model would quit after 1 simulation with the actual condition
    NumIteration=1
  ELSE
    LatDegradModelSimFlag = .TRUE.
    !Set NumIteration=0 so that latent model would simulate twice with rated and actual condition
    NumIteration=0
  END IF


  !Set indoor air conditions to the actual condition
  LoadSideInletDBTemp_Unit = SimpleWatertoAirHP(HPNum)%InletAirDBTemp
  LoadSideInletHumRat_Unit = SimpleWatertoAirHP(HPNum)%InletAirHumRat
  LoadSideInletWBTemp_Unit = PsyTwbFnTdbWPb(LoadSideInletDBTemp_Unit,LoadSideInletHumRat_Unit,OutBaroPress,RoutineName)
  LoadSideInletEnth_Unit = SimpleWatertoAirHP(HPNum)%InletAirEnthalpy
  CpAir_Unit = PsyCpAirFnWTdb(LoadSideInletHumRat_Unit,LoadSideInletDBTemp_Unit)

LOOP: DO
    NumIteration=NumIteration+1
    IF (NumIteration.EQ.1) THEN
    !Set indoor air conditions to the rated conditions
        LoadSideInletDBTemp = LoadSideInletDBTemp_Init
        LoadSideInletHumRat = LoadSideInletHumRat_Init
        LoadSideInletWBTemp = LoadSideInletWBTemp_Init
        LoadSideInletEnth = LoadSideInletEnth_Init
        CpAir = CpAir_Init
    ELSE
    !Set indoor air conditions to the actual condition
        LoadSideInletDBTemp = LoadSideInletDBTemp_Unit
        LoadSideInletHumRat = LoadSideInletHumRat_Unit
        LoadSideInletWBTemp = LoadSideInletWBTemp_Unit
        LoadSideInletEnth = LoadSideInletEnth_Unit
        CpAir = CpAir_Unit
    END IF

    ratioTDB = ((LoadSideInletDBTemp+CelsiustoKelvin)/Tref)
    ratioTWB = ((LoadSideInletWBTemp+CelsiustoKelvin)/Tref)
    ratioTS = ((SourceSideInletTemp+CelsiustoKelvin)/Tref)
    ratioVL = (LoadSideMassFlowRate/(AirVolFlowRateRated*PsyRhoAirFnPbTdbW(StdBaroPress,LoadSideInletDBTemp,LoadSideInletHumRat)))

    IF (WaterPartLoad > 0.0d0 .and. SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate > 0.0d0) THEN
      ratioVS = (SourceSideMassFlowRate)/(SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate*WaterPartLoad)
    ELSE
      ratioVS = 0.0d0
    ENDIF

    QLoadTotal = TotalCapRated*(TotalCapCoeff1 + (ratioTWB * TotalCapCoeff2) + (ratioTS * TotalCapCoeff3) +   &
                                (ratioVL * TotalCapCoeff4) + (ratioVS * TotalCapCoeff5))
    QSensible = SensCapRated*(SensCapCoeff1 + (ratioTDB * SensCapCoeff2) + (ratioTWB * SensCapCoeff3) +   &
                                (ratioTS * SensCapCoeff4) + (ratioVL * SensCapCoeff5) + (ratioVS * SensCapCoeff6))
    Winput = CoolPowerRated*(CoolPowerCoeff1 + (ratioTWB * CoolPowerCoeff2) + (ratioTS * CoolPowerCoeff3)+   &
                                (ratioVL * CoolPowerCoeff4) + (ratioVS * CoolPowerCoeff5))
    Qsource =  QLoadTotal + Winput

  !Check if the Sensible Load is greater than the Total Cooling Load
  IF(QSensible.GT.QLoadTotal) THEN
     QSensible = QLoadTotal
  END IF

  IF(LatDegradModelSimFlag) THEN
  !Calculate for SHReff using the Latent Degradation Model
    IF(NumIteration.EQ.1) THEN
        QLatRated=QLoadTotal-QSensible
    ELSEIF(NumIteration.EQ.2) THEN
        QLatActual=QLoadTotal-QSensible
        SHRss=QSensible/QLoadTotal
        SHReff = CalcEffectiveSHR(HPNum, SHRss,CyclingScheme, RuntimeFrac, &
                 QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp)
!       Update sensible capacity based on effective SHR
        QSensible = QLoadTotal * SHReff
        EXIT LOOP
    END IF
  ELSE
  !Assume SHReff=SHRss
    SHReff = QSensible/QLoadTotal
    EXIT LOOP
  END IF
  END DO LOOP

  !calculate coil outlet state variables
  LoadSideOutletEnth   = LoadSideInletEnth - QLoadTotal/LoadSideMassFlowRate
  LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible/(LoadSideMassFlowRate * CpAir)
  LoadsideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideOutletEnth,RoutineName)
  Count = Count + 1
  !Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideOutletEnth + &
                                                  (1.0d0-PartLoadRatio)*LoadSideInletEnth
    SimpleWatertoAirHP(HPNum)%OutletAirHumRat   = PartLoadRatio*LoadsideOutletHumRat + &
                                                  (1.0d0-PartLoadRatio)*LoadSideInletHumRat
    SimpleWatertoAirHP(HPNum)%OutletAirDBTemp   = PsyTdbFnHW(SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy,  &
                                                             SimpleWatertoAirHP(HPNum)%OutletAirHumRat,    &
                                                             RoutineName)
    PLRCorrLoadSideMdot = LoadSideMassFlowRate
  ELSE
    ! default to cycling fan, cycling compressor
    SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideOutletEnth
    SimpleWatertoAirHP(HPNum)%OutletAirHumRat   = LoadsideOutletHumRat
    SimpleWatertoAirHP(HPNum)%OutletAirDBTemp   = LoadSideOutletDBTemp
    PLRCorrLoadSideMdot = LoadSideMassFlowRate*PartLoadRatio
  END IF

   ! scale heat transfer rates to PLR and power to RTF
  QLoadTotal = QLoadTotal*PartLoadRatio
  QSensible  = QSensible*PartLoadRatio
  Winput     = Winput*RuntimeFrac
  QSource    = QSource*PartLoadRatio

!  Add power to global variable so power can be summed by parent object
  DXElecCoolingPower = Winput

  ReportingConstant=TimeStepSys*SecInHour
  !Update heat pump data structure
  SimpleWatertoAirHP(HPNum)%Power               = Winput
  SimpleWatertoAirHP(HPNum)%QLoadTotal          = QLoadTotal
  SimpleWatertoAirHP(HPNum)%QSensible           = QSensible
  SimpleWatertoAirHP(HPNum)%QLatent             = QLoadTotal - QSensible
  SimpleWatertoAirHP(HPNum)%QSource             = QSource
  SimpleWatertoAirHP(HPNum)%Energy=Winput*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergyLoadTotal=QLoadTotal*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergySensible=QSensible*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergyLatent=(QLoadTotal - QSensible)*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergySource=QSource*ReportingConstant
  IF(RunTimeFrac == 0.0d0) THEN
    SimpleWatertoAirHP(HPNum)%COP = 0.0d0
  ELSE
    SimpleWatertoAirHP(HPNum)%COP = QLoadTotal/Winput
  END IF
  SimpleWatertoAirHP(HPNum)%RunFrac             = RuntimeFrac
  SimpleWatertoAirHP(HPNum)%PartLoadRatio       = PartLoadRatio
  SimpleWatertoAirHP(HPNum)%AirMassFlowRate     = PLRCorrLoadSideMdot

  SimpleWatertoAirHP(HPNum)%WaterMassFlowRate   = SourceSideMassFlowRate
  SimpleWatertoAirHP(HPNum)%OutletWaterTemp     = SourceSideInletTemp + QSource/(SourceSideMassFlowRate * CpWater)
  SimpleWatertoAirHP(HPNum)%OutletWaterEnthalpy = SourceSideInletEnth + QSource/SourceSideMassFlowRate

END SUBROUTINE CalcHPCoolingSimple

SUBROUTINE CalcHPHeatingSimple(HPNum,CyclingScheme,RuntimeFrac,SensDemand,CompOp,PartLoadRatio,OnOffAirFlowRatio,WaterPartLoad)


          !       AUTHOR         Arun Shenoy
          !       DATE WRITTEN   Jan 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for simulating the heating mode of the Water to Air HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Simulate the heat pump performance using the coefficients and rated conditions
          !
          ! Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
          ! and RuntimeFrac.

          ! REFERENCES:
          ! (1) Lash.T.A.,1992.Simulation and Analysis of a Water Loop Heat Pump System.
          ! M.S. Thesis, University of Illinois at Urbana Champaign.
          ! (2) Shenoy, Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump.
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)
          ! (3) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from www.hvac.okstate.edu)

          ! USE STATEMENTS:
  USE DataHVACGlobals,      ONLY:TimeStepSys, DXElecHeatingPower
  USE Psychrometrics,       ONLY:PsyWFnTdbTwbPb,PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyTwbFnTdbWPb,  &
                                 PsyTdbFnHW,PsyWFnTdbH
  USE FluidProperties,      ONLY:GetSpecificHeatGlycol
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER,   INTENT(IN) :: HPNum              ! Heat Pump Number
  INTEGER,   INTENT(IN) :: CyclingScheme      ! Fan/Compressor cycling scheme indicator
  REAL(r64), INTENT(IN) :: RuntimeFrac        ! Runtime Fraction of compressor
  REAL(r64), INTENT(IN) :: SensDemand         ! Cooling Sensible Demand [W] !unused1208
  INTEGER,   INTENT(IN) :: CompOp             ! compressor operation flag
  REAL(r64), INTENT(IN) :: PartLoadRatio      ! compressor part load ratio
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio  ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: WaterPartLoad      ! water part load ratio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Tref=283.15d0      ! Reference Temperature for performance curves,10C [K]
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcHPHeatingSimple'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  REAL(r64) :: HeatCapRated           ! Rated Heating Capacity [W]
  REAL(r64) :: HeatPowerRated         ! Rated Heating Power Input[W]
  REAL(r64) :: AirVolFlowRateRated    ! Rated Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: WaterVolFlowRateRated  ! Rated Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: HeatCapCoeff1          ! 1st coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff2          ! 2nd coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff3          ! 3rd coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff4          ! 4th coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff5          ! 5th coefficient of the heating capacity performance curve
  REAL(r64) :: HeatPowerCoeff1        ! 1st coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff2        ! 2nd coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff3        ! 3rd coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff4        ! 4th coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff5        ! 5th coefficient of the heating power consumption curve

!  REAL(r64) :: PartLoadRatio          ! Part load ratio
  REAL(r64) :: ratioTDB               ! Ratio of the inlet air dry bulb temperature to the rated conditions
  REAL(r64) :: ratioTS                ! Ratio of the source side (water) inlet temperature to the rated conditions
  REAL(r64) :: ratioVL                ! Ratio of the load side flow rate to the rated conditions
  REAL(r64) :: ratioVS                ! Ratio of the source side flow rate to the rated conditions
  REAL(r64) :: CpWater                ! Specific heat of water [J/kg_C]
  REAL(r64) :: CpAir                  ! Specific heat of air [J/kg_C]
  REAL(r64) :: ReportingConstant

 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)

  HeatCapRated           = SimpleWatertoAirHP(HPNum)%RatedCapHeat
  HeatPowerRated         = SimpleWatertoAirHP(HPNum)%RatedPowerHeat
  AirVolFlowRateRated    = SimpleWatertoAirHP(HPNum)%RatedAirVolFlowRate
  WaterVolFlowRateRated  = SimpleWatertoAirHP(HPNum)%RatedWaterVolFlowRate
  HeatCapCoeff1          = SimpleWatertoAirHP(HPNum)%HeatCap1
  HeatCapCoeff2          = SimpleWatertoAirHP(HPNum)%HeatCap2
  HeatCapCoeff3          = SimpleWatertoAirHP(HPNum)%HeatCap3
  HeatCapCoeff4          = SimpleWatertoAirHP(HPNum)%HeatCap4
  HeatCapCoeff5          = SimpleWatertoAirHP(HPNum)%HeatCap5
  HeatPowerCoeff1        = SimpleWatertoAirHP(HPNum)%HeatPower1
  HeatPowerCoeff2        = SimpleWatertoAirHP(HPNum)%HeatPower2
  HeatPowerCoeff3        = SimpleWatertoAirHP(HPNum)%HeatPower3
  HeatPowerCoeff4        = SimpleWatertoAirHP(HPNum)%HeatPower4
  HeatPowerCoeff5        = SimpleWatertoAirHP(HPNum)%HeatPower5

  LoadSideMassFlowRate   = SimpleWatertoAirHP(HPNum)%AirMassFlowRate
  LoadSideInletDBTemp    = SimpleWatertoAirHP(HPNum)%InletAirDBTemp
  LoadSideInletHumRat    = SimpleWatertoAirHP(HPNum)%InletAirHumRat

  LoadSideInletWBTemp    = PsyTwbFnTdbWPb(LoadSideInletDBTemp,LoadSideInletHumRat,OutBaroPress,RoutineName)
  LoadSideInletEnth      = SimpleWatertoAirHP(HPNum)%InletAirEnthalpy
  CpAir                  = PsyCpAirFnWTdb(LoadSideInletHumRat,LoadSideInletDBTemp,RoutineName)
  SourceSideMassFlowRate = SimpleWatertoAirHP(HPNum)%WaterMassFlowRate
  SourceSideInletTemp    = SimpleWatertoAirHP(HPNum)%InletWaterTemp
  SourceSideInletEnth    = SimpleWatertoAirHP(HPNum)%InletWaterEnthalpy
  CpWater                = GetSpecificHeatGlycol(PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidName, &
                                   SourceSideInletTemp, &
                                   PlantLoop(SimpleWatertoAirHP(HPNum)%LoopNum)%FluidIndex,  &
                                   RoutineName//':SourceSideInletTemp')

 !Check for flows, do not perform simulation if no flow in load side or source side.
  IF (SourceSideMassFlowRate <= 0.0d0 .OR. LoadSideMassFlowRate <= 0.0d0)THEN
    SimpleWatertoAirHP(HPNum)%SimFlag = .FALSE.
    RETURN
  ELSE
    SimpleWatertoAirHP(HPNum)%SimFlag = .TRUE.
  ENDIF

  IF (CompOp .EQ. 0) THEN
    SimpleWaterToAirHP(HPNum)%SimFlag = .FALSE.
    RETURN
  ENDIF

  ratioTDB = ((LoadSideInletDBTemp+CelsiustoKelvin)/Tref)
  ratioTS = ((SourceSideInletTemp+CelsiustoKelvin)/Tref)
  ratioVL = (LoadSideMassFlowRate/  &
     (AirVolFlowRateRated*PsyRhoAirFnPbTdbW(StdBaroPress,LoadSideInletDBTemp,LoadSideInletHumRat,RoutineName)))
  IF (WaterPartLoad > 0.0d0 .and. SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate > 0.0d0) THEN
    ratioVS = (SourceSideMassFlowRate)/(SimpleWatertoAirHP(HPNum)%DesignWaterMassFlowRate*WaterPartLoad)
  ELSE
    ratioVS = 0.0d0
  ENDIF

  QLoadTotal = HeatCapRated*(HeatCapCoeff1 + (ratioTDB * HeatCapCoeff2) + (ratioTS * HeatCapCoeff3) +   &
                                  (ratioVL * HeatCapCoeff4) + (ratioVS * HeatCapCoeff5))
  QSensible = QLoadTotal
  Winput = HeatPowerRated*(HeatPowerCoeff1 + (ratioTDB * HeatPowerCoeff2) + (ratioTS * HeatPowerCoeff3) +   &
                             (ratioVL * HeatPowerCoeff4) + (ratioVS * HeatPowerCoeff5))
  Qsource = QLoadTotal-Winput

  ! calculate coil outlet state variables
  LoadSideOutletEnth   = LoadSideInletEnth + QLoadTotal/LoadSideMassFlowRate
  LoadSideOutletDBTemp = LoadSideInletDBTemp + QSensible/(LoadSideMassFlowRate * CpAir)
  LoadsideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideOutletEnth,RoutineName)

  ! Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideOutletEnth + &
                                                  (1.d0-PartLoadRatio)*LoadSideInletEnth
    SimpleWatertoAirHP(HPNum)%OutletAirHumRat   = PartLoadRatio*LoadsideOutletHumRat + &
                                                  (1.d0-PartLoadRatio)*LoadSideInletHumRat
    SimpleWatertoAirHP(HPNum)%OutletAirDBTemp   = PsyTdbFnHW(SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy,  &
                                                               SimpleWatertoAirHP(HPNum)%OutletAirHumRat,RoutineName)
    PLRCorrLoadSideMdot = LoadSideMassFlowRate
  ELSE
    ! default to cycling fan, cycling compressor
    SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy = LoadSideOutletEnth
    SimpleWatertoAirHP(HPNum)%OutletAirHumRat   = LoadsideOutletHumRat
    SimpleWatertoAirHP(HPNum)%OutletAirDBTemp   = LoadSideOutletDBTemp
    PLRCorrLoadSideMdot = LoadSideMassFlowRate*PartLoadRatio
  END IF


   ! scale heat transfer rates to PLR and power to RTF
  QLoadTotal = QLoadTotal*PartLoadRatio
  QSensible  = QSensible*PartLoadRatio
  Winput     = Winput*RuntimeFrac
  QSource    = QSource*PartLoadRatio

!  Add power to global variable so power can be summed by parent object
  DXElecHeatingPower = Winput

  ReportingConstant=TimeStepSys*SecInHour
  !Update heat pump data structure
  SimpleWatertoAirHP(HPNum)%Power               = Winput
  SimpleWatertoAirHP(HPNum)%QLoadTotal          = QLoadTotal
  SimpleWatertoAirHP(HPNum)%QSensible           = QSensible
  SimpleWatertoAirHP(HPNum)%QSource             = QSource
  SimpleWatertoAirHP(HPNum)%Energy=Winput*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergyLoadTotal=QLoadTotal*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergySensible=QSensible*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergyLatent=0.0d0
  SimpleWatertoAirHP(HPNum)%EnergySource=QSource*ReportingConstant
  IF(RunTimeFrac == 0.0d0) THEN
    SimpleWatertoAirHP(HPNum)%COP = 0.0d0
  ELSE
    SimpleWatertoAirHP(HPNum)%COP = QLoadTotal/Winput
  END IF
  SimpleWatertoAirHP(HPNum)%RunFrac             = RuntimeFrac
  SimpleWatertoAirHP(HPNum)%PartLoadRatio       = PartLoadRatio
  SimpleWatertoAirHP(HPNum)%AirMassFlowRate     = PLRCorrLoadSideMdot

  SimpleWatertoAirHP(HPNum)%WaterMassFlowRate   = SourceSideMassFlowRate
  SimpleWatertoAirHP(HPNum)%OutletWaterTemp     = SourceSideInletTemp - QSource/(SourceSideMassFlowRate * CpWater)
  SimpleWatertoAirHP(HPNum)%OutletWaterEnthalpy = SourceSideInletEnth - QSource/SourceSideMassFlowRate

END SUBROUTINE CalcHPHeatingSimple


SUBROUTINE UpdateSimpleWatertoAirHP(HPNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Arun Shenoy
          !       DATE WRITTEN   Jan 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  Kenneth Tang (Jan 2005)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the Water to Air Heat Pump outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the HP data structure to the HP outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys
  USe PlantUtilities,  ONLY: SafeCopyPlantNode
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(In) :: HPNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER              :: AirInletNode
  INTEGER              :: WaterInletNode
  INTEGER              :: AirOutletNode
  INTEGER              :: WaterOutletNode
  REAL(r64)            :: ReportingConstant


  !WatertoAirHP(HPNum)%Simflag=.FALSE.
  IF(.NOT. SimpleWatertoAirHP(HPNum)%Simflag)THEN
    ! Heatpump is off; just pass through conditions
    SimpleWatertoAirHP(HPNum)%Power               = 0.0d0
    SimpleWatertoAirHP(HPNum)%QLoadTotal          = 0.0d0
    SimpleWatertoAirHP(HPNum)%QSensible           = 0.0d0
    SimpleWatertoAirHP(HPNum)%QLatent             = 0.0d0
    SimpleWatertoAirHP(HPNum)%QSource             = 0.0d0
    SimpleWatertoAirHP(HPNum)%Energy              = 0.0d0
    SimpleWatertoAirHP(HPNum)%EnergyLoadTotal     = 0.0d0
    SimpleWatertoAirHP(HPNum)%EnergySensible      = 0.0d0
    SimpleWatertoAirHP(HPNum)%EnergyLatent        = 0.0d0
    SimpleWatertoAirHP(HPNum)%EnergySource        = 0.0d0
    SimpleWatertoAirHP(HPNum)%COP                 = 0.0d0
    SimpleWatertoAirHP(HPNum)%RunFrac             = 0.0d0
    SimpleWatertoAirHP(HPNum)%PartLoadRatio       = 0.0d0

    SimpleWatertoAirHP(HPNum)%OutletAirDBTemp     = SimpleWatertoAirHP(HPNum)%InletAirDBTemp
    SimpleWatertoAirHP(HPNum)%OutletAirHumRat     = SimpleWatertoAirHP(HPNum)%InletAirHumRat
    SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy   = SimpleWatertoAirHP(HPNum)%InletAirEnthalpy
    SimpleWatertoAirHP(HPNum)%OutletWaterTemp     = SimpleWatertoAirHP(HPNum)%InletWaterTemp
    SimpleWatertoAirHP(HPNum)%OutletWaterEnthalpy = SimpleWatertoAirHP(HPNum)%InletWaterEnthalpy
  END IF

  AirInletNode    = SimpleWatertoAirHP(HPNum)%AirInletNodeNum
  WaterInletNode  = SimpleWatertoAirHP(HPNum)%WaterInletNodeNum
  AirOutletNode   = SimpleWatertoAirHP(HPNum)%AirOutletNodeNum
  WaterOutletNode = SimpleWatertoAirHP(HPNum)%WaterOutletNodeNum


  ! Set the air outlet  nodes of the WatertoAirHPSimple
  Node(AirOutletNode)%MassFlowRate          = Node(AirInletNode)%MassFlowRate     !LoadSideMassFlowRate
  Node(AirOutletNode)%Temp                  = SimpleWatertoAirHP(HPNum)%OutletAirDBTemp
  Node(AirOutletNode)%HumRat                = SimpleWatertoAirHP(HPNum)%OutletAirHumRat
  Node(AirOutletNode)%Enthalpy              = SimpleWatertoAirHP(HPNum)%OutletAirEnthalpy

   ! Set the air outlet nodes for properties that just pass through & not used
  Node(AirOutletNode)%Quality               = Node(AirInletNode)%Quality
  Node(AirOutletNode)%Press                 = Node(AirInletNode)%Press
  Node(AirOutletNode)%MassFlowRateMin       = Node(AirInletNode)%MassFlowRateMin
  Node(AirOutletNode)%MassFlowRateMax       = Node(AirInletNode)%MassFlowRateMax    !LoadSideMassFlowRate
  Node(AirOutletNode)%MassFlowRateMinAvail  = Node(AirInletNode)%MassFlowRateMinAvail
  Node(AirOutletNode)%MassFlowRateMaxAvail  = Node(AirInletNode)%MassFlowRateMaxAvail     !LoadSideMassFlowRate

   ! Set the water outlet node of the WatertoAirHPSimple
   ! Set the water outlet nodes for properties that just pass through & not used
  CALL SafeCopyPlantNode(WaterInletNode , WaterOutletNode)

  Node(WaterOutletNode)%Temp                = SimpleWatertoAirHP(HPNum)%OutletWaterTemp
  Node(WaterOutletNode)%Enthalpy            = SimpleWatertoAirHP(HPNum)%OutletWaterEnthalpy

  ReportingConstant                         = TimeStepSys*SecInHour
  SimpleWatertoAirHP(HPNum)%Energy          = SimpleWatertoAirHP(HPNum)%Power*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergyLoadTotal = SimpleWatertoAirHP(HPNum)%QLoadTotal*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergySensible  = SimpleWatertoAirHP(HPNum)%QSensible*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergyLatent    = SimpleWatertoAirHP(HPNum)%QLatent*ReportingConstant
  SimpleWatertoAirHP(HPNum)%EnergySource    = SimpleWatertoAirHP(HPNum)%QSource*ReportingConstant

   IF (Contaminant%CO2Simulation) Then
     Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
   End If
   IF (Contaminant%GenericContamSimulation) Then
     Node(AirOutletNode)%GenContam = Node(AirInletNode)%GenContam
   End If

  RETURN
END SUBROUTINE UpdateSimpleWatertoAirHP

!        End of Update subroutines for the WatertoAirHP Module
! *****************************************************************************

FUNCTION CalcEffectiveSHR(HPNum,SHRss, CyclingScheme, RTF, QLatRated, QLatActual, EnteringDB, EnteringWB) RESULT(SHReff)

        ! FUNCTION INFORMATION:
        !    AUTHOR         Richard Raustad, FSEC
        !    DATE WRITTEN   September 2003
        !    MODIFIED       Kenneth Tang (Aug 2004) Added capability for simulating CycFanCycCoil
        !    RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        !    Adjust sensible heat ratio to account for degradation of DX coil latent
        !    capacity at part-load (cycling) conditions.

        ! METHODOLOGY EMPLOYED:
        !    With model parameters entered by the user, the part-load latent performance
        !    of a DX cooling coil is determined for a constant air flow system with
        !    a cooling coil that cycles on/off. The model calculates the time
        !    required for condensate to begin falling from the cooling coil.
        !    Runtimes greater than this are integrated to a "part-load" latent
        !    capacity which is used to determine the "part-load" sensible heat ratio.
        !    See reference below for additional details (linear decay model, Eq. 8b).

        !    For cycling fan operation, a modified version of Henderson and Rengarajan (1996)
        !    model is used by ultilizing the fan delay time as the time-off (or time duration
        !    for the re-evaporation of moisture from time coil). Refer to Tang, C.C. (2005)

        ! REFERENCES:
        !    (1) Henderson, H.I., K. Rengarajan.1996. A Model to Predict the Latent
        !    Capacity of Air Conditioners and Heat Pumps at Part-Load Conditions
        !    with Constant Fan Operation ASHRAE Transactions 102 (1), pp. 266-274.
        !    (2) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
        !    State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
        !    Oklahoma State University. (downloadable from www.hvac.okstate.edu)


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HPNum         ! Index number for cooling coil
  INTEGER, INTENT (IN) :: CyclingScheme ! Fan/compressor cycling scheme indicator
  REAL(r64), INTENT (IN) :: SHRss         ! Steady-state sensible heat ratio
  REAL(r64), INTENT (IN) :: RTF           ! Compressor run-time fraction
  REAL(r64), INTENT (IN) :: QLatRated     ! Rated latent capacity
  REAL(r64), INTENT (IN) :: QLatActual    ! Actual latent capacity
  REAL(r64), INTENT (IN) :: EnteringDB    ! Entering air dry-bulb temperature
  REAL(r64), INTENT (IN) :: EnteringWB    ! Entering air wet-bulb temperature
  REAL(r64)            :: SHReff        ! Effective sensible heat ratio, includes degradation due to cycling effects

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Twet                 ! Nominal time for condensate to begin leaving the coil's condensate drain line
                                    ! at the current operating conditions (sec)
  REAL(r64) :: Gamma                ! Initial moisture evaporation rate divided by steady-state AC latent capacity
                                    ! at the current operating conditions
  REAL(r64) :: Twet_rated           ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated          ! Gamma at rated conditions (coil air flow rate and air temperatures)
  REAL(r64) :: Twet_max             ! Maximum allowed value for Twet
  REAL(r64) :: MaxONOFFCyclesperHour  ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64) :: HPTimeConstant       ! Heat pump time constant [s]
  REAL(r64) :: FanDelayTime         ! Fan delay time, time delay for the HP's fan to
                                    ! shut off after compressor cycle off  [s]
  REAL(r64) :: Ton                  ! Coil on time (sec)
  REAL(r64) :: Toff                 ! Coil off time (sec)
  REAL(r64) :: Toffa                ! Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
  REAL(r64) :: aa                   ! Intermediate variable
  REAL(r64) :: To1                  ! Intermediate variable (first guess at To). To = time to the start of moisture removal
  REAL(r64) :: To2                  ! Intermediate variable (second guess at To). To = time to the start of moisture removal
  REAL(r64) :: Error                ! Error for iteration (DO) loop
  REAL(r64) :: LHRmult              ! Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

   Twet_rated               = SimpleWatertoAirHP(HPNum)%Twet_Rated
   Gamma_rated              = SimpleWatertoAirHP(HPNum)%Gamma_Rated
   MaxONOFFCyclesperHour    = SimpleWatertoAirHP(HPNum)%MaxONOFFCyclesperHour
   HPTimeConstant           = SimpleWatertoAirHP(HPNum)%HPTimeConstant
   FanDelayTime             = SimpleWatertoAirHP(HPNum)%FanDelayTime

!  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
!  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
!  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
   IF((RTF.GE.1.0d0) .OR. (QLatRated.EQ.0.0d0) .OR. (QLatActual.EQ.0.0d0) .OR. (Twet_rated.LE.0.0d0) .OR. &
      (Gamma_rated.LE.0.0d0) .OR. (MaxONOFFCyclesperHour.LE.0.0d0) .OR. (HPTimeConstant.LE.0.0d0) .OR. (RTF.LE. 0.0d0)) THEN
     SHReff = SHRss
     RETURN
   ENDIF

   Twet_max   = 9999.0d0 ! high limit for Twet

!  Calculate the model parameters at the actual operating conditions
   Twet    = MIN(Twet_rated*QLatRated /(QLatActual+1.d-10),Twet_max)
   Gamma   = Gamma_rated*QLatRated*(EnteringDB-EnteringWB)/((26.7d0-19.4d0)*QLatActual+1.d-10)

!  Calculate the compressor on and off times using a converntional thermostat curve
   Ton  = 3600.d0/(4.d0*MaxONOFFCyclesperHour*(1.d0-RTF))   ! duration of cooling coil on-cycle (sec)

   IF ((CyclingScheme .EQ. CycFanCycCoil).AND.(FanDelayTime.NE.0.0d0)) THEN
    ! For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
    ! until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
        Toff = FanDelayTime
   ELSE
    ! For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
    ! for the entire heat pump off-cycle.
        Toff = 3600.d0/(4.d0*MaxONOFFCyclesperHour*RTF)        ! duration of cooling coil off-cycle (sec)
   END IF

!  Cap Toff to meet the equation restriction
   IF(Gamma .GT. 0.0d0)THEN
     Toffa = MIN(Toff, 2.d0*Twet/Gamma)
   ELSE
     Toffa = Toff
   END IF

!  Use sucessive substitution to solve for To
   aa = (Gamma*Toffa) - (0.25d0/Twet)*(Gamma**2)*(Toffa**2)

   To1 = aa+HPTimeConstant
   Error = 1.0d0
   DO WHILE (Error .gt. 0.001d0)
       To2 = aa-HPTimeConstant*(EXP(-To1/HPTimeConstant)-1.0d0)
       Error = ABS((To2-To1)/To1)
       To1 = To2
   END DO

!  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
!  Floating underflow errors occur when -Ton/HPTimeConstant is a large negative number.
!  Cap lower limit at -700 to avoid the underflow errors.
   aa = EXP(MAX(-700.0d0,-Ton/HPTimeConstant))
!  Calculate latent heat ratio multiplier
   LHRmult = MAX(((Ton-To2)/(Ton+HPTimeConstant*(aa-1.0d0))),0.0d0)

!  Calculate part-load or "effective" sensible heat ratio
   SHReff = 1.0d0-(1.0d0-SHRss)*LHRmult

   IF (SHReff .LT. SHRss) SHReff = SHRss ! Effective SHR can be less than the steady-state SHR
   IF (SHReff .GT. 1.0d0) SHReff=1.0d0 ! Effective sensible heat ratio can't be greater than 1.0

 RETURN

END FUNCTION CalcEffectiveSHR

FUNCTION GetCoilIndex(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil capacity for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: IndexNum     ! returned index of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IndexNum=FindItemInList(CoilName,SimpleWaterToAirHP%Name,NumWaterToAirHPs)

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetCoilIndex

FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil capacity for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilCapacity ! returned capacity of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (SameString(CoilType,'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT') .or.   &
      SameString(CoilType,'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT')) THEN
    WhichCoil=FindItemInList(CoilName,SimpleWaterToAirHP%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      IF (SameString(CoilType,'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT')) THEN
        CoilCapacity=SimpleWaterToAirHP(WhichCoil)%RatedCapHeat
      ELSE
        CoilCapacity=SimpleWaterToAirHP(WhichCoil)%RatedCapCoolTotal
      ENDIF
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilCapacity=-1000.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilCapacity

FUNCTION GetCoilAirFlowRate(CoilType,CoilName,ErrorsFound) RESULT(CoilAirFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   October 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil air flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilAirFlowRate ! returned air volume flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (CoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT' .or.   &
      CoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT') THEN
    WhichCoil=FindItemInList(CoilName,SimpleWaterToAirHP%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      CoilAirFlowRate=SimpleWatertoAirHP(WhichCoil)%RatedAirVolFlowRate
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilAirFlowRate=-1000.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilAirFlowRate

FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,SimpleWatertoAirHP%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=SimpleWatertoAirHP(WhichCoil)%AirInletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNode

FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   July 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the outlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,SimpleWatertoAirHP%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=SimpleWatertoAirHP(WhichCoil)%AirOutletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNode

 SUBROUTINE SetSimpleWSHPData(SimpleWSHPNum,ErrorsFound,WaterCyclingMode,CompanionCoolingCoilNum,CompanionHeatingCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine was designed to "push" information from a parent object to
          ! this WSHP coil object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: TrimSigDigits
  USE InputProcessor,  ONLY: FindItemInList, SameString
  USE FluidProperties, ONLY: FindGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: SimpleWSHPNum  ! Number of OA Controller
  LOGICAL, INTENT(INOUT) :: ErrorsFound    ! Set to true if certain errors found
  INTEGER, INTENT(IN)    :: WaterCyclingMode  ! the coil water flow mode (cycling, constant or constantondemand)
  INTEGER, OPTIONAL      :: CompanionCoolingCoilNum  ! Index to cooling coil for heating coil = SimpleWSHPNum
  INTEGER, OPTIONAL      :: CompanionHeatingCoilNum  ! Index to heating coil for cooling coil = SimpleWSHPNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetSimpleWatertoAirHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (SimpleWSHPNum <= 0 .or. SimpleWSHPNum > NumWatertoAirHPs) THEN
    CALL ShowSevereError('SetSimpleWSHPData: called with WSHP Coil Number out of range='//  &
         TRIM(TrimSigDigits(SimpleWSHPNum))//' should be >0 and <'//TRIM(TrimSigDigits(NumWatertoAirHPs)))
    ErrorsFound=.true.
    RETURN
  ENDIF

  SimpleWatertoAirHP(SimpleWSHPNum)%WaterCyclingMode = WaterCyclingMode
  IF (PRESENT(CompanionCoolingCoilNum)) THEN
    SimpleWatertoAirHP(SimpleWSHPNum)%CompanionCoolingCoilNum=CompanionCoolingCoilNum
    SimpleWatertoAirHP(CompanionCoolingCoilNum)%CompanionHeatingCoilNum=SimpleWSHPNum
    SimpleWatertoAirHP(CompanionCoolingCoilNum)%WaterCyclingMode = WaterCyclingMode
  ENDIF

  IF (PRESENT(CompanionHeatingCoilNum)) THEN
    SimpleWatertoAirHP(SimpleWSHPNum)%CompanionHeatingCoilNum=CompanionHeatingCoilNum
    SimpleWatertoAirHP(CompanionHeatingCoilNum)%CompanionCoolingCoilNum=SimpleWSHPNum
    SimpleWatertoAirHP(CompanionHeatingCoilNum)%WaterCyclingMode = WaterCyclingMode
  ENDIF

  RETURN

END SUBROUTINE SetSimpleWSHPData

END MODULE WatertoAirHeatPumpSimple

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

