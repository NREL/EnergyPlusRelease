!  Three modules are contained in this file:
! HeatPumpWaterToWaterHEATING
! HeatPumpWaterToWaterCOOLING
! HeatPumpWaterToWaterSimple
!************************************************************************************
!
!==================================== MODULE HeatPumpWaterToWaterHEATING ======================
!
!************************************************************************************
MODULE HeatPumpWaterToWaterHEATING
  ! Module containing the routines dealing with the Water to Water Heat Pump (Heating)

  ! MODULE INFORMATION:
  !       AUTHOR         ARUN
  !       DATE WRITTEN   7/18/2000
  !       MODIFIED       ARUN: 6/27/2002: Cycle Time
  !                      L Lawrie: V1.1.1 (5/20/2003) add meters and energy to several reporting variables
  !                      L Lawrie: V1.1.1 (5/20/2003) restructure modules to comply with standard templates
  !                      B. Griffith, Sept 2010, plant upgrades, generalize fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module simulates a water to Water Heat Pump (Heating)

  ! METHODOLOGY EMPLOYED:
  ! This simulation is based on a set of selected parameters,
  ! Which are obtained using Parameter Estimation technique.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BeginSimFlag,InitconvTemp,BeginEnvrnFlag, HourOfDay, KelvinConv,  &
                              TimeStep,TimeStepZone,DayOfSim,WarmupFlag,SecInHour
USE DataInterfaces
USE DataLoopNode

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: ModuleCompName='HeatPump:WaterToWater:ParameterEstimation:Heating'
CHARACTER(len=*), PARAMETER :: ModuleCompNameUC='HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING'

  ! DERIVED TYPE DEFINITIONS

    ! Type Description of Heat Pump
TYPE GshpSpecs
  CHARACTER(len=MaxNameLength) :: Name          = ' ' ! user identifier
  INTEGER            :: WWHPPlantTypeOfNum      = 0       ! equipment type num
  LOGICAL            :: Available               = .false. ! need an array of logicals--load identifiers of available equipment
  LOGICAL            :: ON                      = .false. ! simulate the machine at it's operating part load ratio
  REAL(r64)          :: COP                     = 0.0d0 ! Coefficient of Performance of the machine
  REAL(r64)          :: NomCap                  = 0.0d0 ! Nominal Capcity of the HeatPump
  REAL(r64)          :: MinPartLoadRat          = 0.0d0 ! Minimum operating Part Load Ratio
  REAL(r64)          :: MaxPartLoadRat          = 0.0d0 ! Maximum operating Part Load Ratio
  REAL(r64)          :: OptPartLoadRat          = 0.0d0 ! Optimal operating Part Load Ratio
  REAL(r64)          :: LoadSideVolFlowRate     = 0.0d0 ! Design Flow Rate on the Load side m3/sec
  REAL(r64)          :: LoadSideDesignMassFlow  = 0.d0 ! Design flow rate (kg/s)
  REAL(r64)          :: SourceSideVolFlowRate   = 0.0d0 ! Design Flow Rate on th Source Side m3/sec
  REAL(r64)          :: SourceSideDesignMassFlow = 0.d0 ! Design flow rate (kg/s)
  INTEGER            :: SourceSideInletNodeNum  = 0   ! Node number on the inlet side of the plant
  INTEGER            :: SourceSideOutletNodeNum = 0   ! Node number on the outlet side of the plant
  INTEGER            :: LoadSideInletNodeNum    = 0   ! Node number on the inlet side of the Load Side
  INTEGER            :: LoadSideOutletNodeNum   = 0   ! Node number on the outlet side of the Load Side
  REAL(r64)          :: SourceSideUACoeff       = 0.0d0 ! Source Side heat transfer coeff W/K
  REAL(r64)          :: LoadSideUACoeff         = 0.0d0 ! Load Side heat transfer coeff  W/K
  REAL(r64)          :: CompPistonDisp          = 0.0d0 ! compressor piston displacement m3
  REAL(r64)          :: CompClearanceFactor     = 0.0d0 ! compressor clearance factor
  REAL(r64)          :: CompSucPressDrop        = 0.0d0 ! deltap ,  compressor suction and discharge pressure drop Pascals
  REAL(r64)          :: SuperheatTemp           = 0.0d0 ! deltatsh , super heating  °C
  REAL(r64)          :: PowerLosses             = 0.0d0 ! constant part of electro mechanical power losses  watts Joules/sec
  REAL(r64)          :: LossFactor              = 0.0d0 ! loss factor used ot define the electro mechanical
                                                      ! loss that is supposed to be proportional to the theoretical power
  REAL(r64)          :: HighPressCutOff         = 0.0d0 ! Maximum Design Pressure on the Load Side Pascals
  REAL(r64)          :: LowPressCutOff          = 0.0d0 ! Minimum Design Pressure on the Source Side Pascals

  ! Added by Arun 6-27-02
  ! to implement cycletime - removed 9/10/2013 LKL
  LOGICAL            :: IsOn                    = .false.
  LOGICAL            :: MustRun                 = .false.
  !loop topology variables
  INTEGER            :: SourceLoopNum           = 0 ! source side plant loop index number
  INTEGER            :: SourceLoopSideNum       = 0 ! source side plant loop side index
  INTEGER            :: SourceBranchNum         = 0 ! source side plant loop branch index
  INTEGER            :: SourceCompNum           = 0 ! source side plant loop component index
  INTEGER            :: LoadLoopNum             = 0 ! load side plant loop index number
  INTEGER            :: LoadLoopSideNum         = 0 ! load side plant loop side index
  INTEGER            :: LoadBranchNum           = 0 ! load side plant loop branch index
  INTEGER            :: LoadCompNum             = 0 ! load side plant loop component index
END TYPE GshpSpecs


    ! Output Variables Type definition
TYPE ReportVars
  REAL(r64)    :: Power                              = 0.0d0 ! Power Consumption Watts
  REAL(r64)    :: Energy                             = 0.0d0 ! Energy Consumption Joules
  REAL(r64)    :: QLoad                              = 0.0d0 ! Load Side heat transfer rate Watts
  REAL(r64)    :: QLoadEnergy                        = 0.0d0 ! Load Side heat transfer Joules
  REAL(r64)    :: QSource                            = 0.0d0 ! Source Side heat transfer rate Watts
  REAL(r64)    :: QSourceEnergy                      = 0.0d0 ! Source Side heat transfer Joules
  REAL(r64)    :: LoadSideWaterInletTemp             = 0.0d0 ! Load Side outlet temperature °C
  REAL(r64)    :: SourceSideWaterInletTemp           = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)    :: LoadSideWaterOutletTemp            = 0.0d0 ! Load Side outlet temperature °C
  REAL(r64)    :: SourceSideWaterOutletTemp          = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)    :: LoadSidemdot                       = 0.0d0 ! Mass flow rate of the cooling water in Load Side Kg/s
  REAL(r64)    :: SourceSidemdot                     = 0.0d0 ! Mass flow rate of chilled water in Eavporator Kg/s
  INTEGER   :: Running                             = 0   ! On reporting Flag
END TYPE ReportVars


  ! MODULE VARIABLE DECLARATIONS:

  TYPE (GSHPSpecs), ALLOCATABLE, DIMENSION(:)  ::GSHP   !dimension to number of machines
  TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) :: GSHPReport

  CHARACTER(len=3)   :: GSHPRefrigerant='R22'            ! Refrigerent name and index
  INTEGER            :: GSHPRefrigIndex=0

  INTEGER         :: NumGSHPs                      = 0   ! number of Gshps specified in input
  REAL(r64)       :: LoadSideWaterMassFlowRate     = 0.0d0 ! Load Side mass flow rate, water side Kg/s
  REAL(r64)       :: SourceSideWaterMassFlowRate   = 0.0d0 ! Source Side mass flow rate, water side Kg/s
  REAL(r64)       :: Power                         = 0.0d0 ! power consumption Watts Joules/sec
  REAL(r64)       :: QLoad                         = 0.0d0 ! heat rejection from Load Side coil Joules
  REAL(r64)       :: QSource                       = 0.0d0 ! cooling capacity Joules
  REAL(r64)       :: SourceSideWaterOutletTemp     = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)       :: SourceSideWaterInletTemp      = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)       :: LoadSideWaterOutletTemp       = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)       :: LoadSideWaterInletTemp        = 0.0d0 ! Source Side outlet temperature °C
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE

  ! Name Public routines, optionally name Private routines within this module
PUBLIC     SimHPWatertoWaterHEATING
PRIVATE    CalcGshpModel
PRIVATE    GetGshpInput
PRIVATE    InitGshp
PRIVATE    UpdateGSHPRecords



CONTAINS
         ! MODULE SUBROUTINES:

SUBROUTINE SimHPWatertoWaterHEATING(GSHPType, GSHPName, CompIndex,FirstHVACIteration, &
                                    InitLoopEquip, MyLoad, MaxCap, MinCap, OptCap, LoopNum)
          !       SUBROUTINE INFORMATION:
          !       AUTHOR    Arun
          !       DATE WRITTEN   Feb 2000
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the  water to water Heat Pump driver.
          ! It gets the input for the models, initializes simulation variables, calls
          ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide
  USE DataPlant, ONLY: TypeOf_HPWaterEFHeating
  USE InputProcessor, ONLY: FindItemInList
  USE DataEnvironment
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: GshpType  ! type ofGSHP
  CHARACTER(len=*), INTENT(IN) :: GshpName  ! user specified name ofGSHP
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  INTEGER, INTENT(IN)          :: LoopNum
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(INOUT) :: InitLoopEquip   ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)    :: MyLoad          ! loop demand component will meet

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE     :: GetInput = .TRUE.    ! then TRUE, calls subroutine to read input file.
  REAL(r64)         :: MinCap               ! W - minimum operating capacity of GSHP
  REAL(r64)         :: MaxCap               ! W - maximum operating capacity of GSHP
  REAL(r64)         :: OptCap               ! W - optimal operating capacity of GSHP
  INTEGER                      :: GSHPNum


    !Get input from IDF

  IF (GetInput) THEN
    CALL GetGshpInput
    GetInput = .FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    GSHPNum = FindItemInList( GSHPName, GSHP%Name, NumGSHPs )
    IF (GSHPNum == 0) THEN
      CALL ShowFatalError('SimHPWatertoWaterHEATING: Unit not found='//TRIM(GSHPName))
    ENDIF
    CompIndex=GSHPNum
  ELSE
    GSHPNum=CompIndex
    IF (GSHPNum > NumGSHPs .or. GSHPNum < 1) THEN
      CALL ShowFatalError('SimHPWatertoWaterHEATING:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(GSHPNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumGSHPs))//  &
                          ', Entered Unit name='//TRIM(GSHPName))
    ENDIF
    IF (CheckEquipName(GSHPNum)) THEN
      IF (GSHPName /= GSHP(GSHPNum)%Name) THEN
        CALL ShowFatalError('SimHPWatertoWaterHEATING: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(GSHPNum))// &
                            ', Unit name='//TRIM(GSHPName)//', stored Unit Name for that index='//  &
                            TRIM(GSHP(GSHPNum)%Name))
      ENDIF
      CheckEquipName(GSHPNum)=.false.
    ENDIF
  ENDIF

     ! Calculate Demand on heat pump
  IF (InitLoopEquip) THEN
    MinCap = GSHP(GSHPNum)%NomCap*GSHP(GSHPNum)%MinPartLoadRat
    MaxCap = GSHP(GSHPNum)%NomCap*GSHP(GSHPNum)%MaxPartLoadRat
    OptCap = GSHP(GSHPNum)%NomCap*GSHP(GSHPNum)%OptPartLoadRat
    Return
  END IF

 ! Simulate the model for the Demand "MyLoad"

 IF (LoopNum == GSHP(GSHPNum)%LoadLoopNum) THEN ! chilled water loop
   CALL InitGshp(GSHPNum)
   CALL CalcGshpModel( GSHPType, GSHPName, GSHPNum, MyLoad,FirstHVACIteration)
   CALL UpdateGshpRecords(GSHPNum)
 ELSEIF (LoopNum == GSHP(GSHPNum)%SourceLoopNum) THEN ! condenser loop
   CALL UpdateChillerComponentCondenserSide(GSHP(GSHPNum)%SourceLoopNum, &
                                     GSHP(GSHPNum)%SourceLoopSideNum,     &
                                     TypeOf_HPWaterEFHeating,                     &
                                     GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                     GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                     - GSHPReport(GSHPNum)%QSource,             &
                                     GSHPReport(GSHPNum)%SourceSideWaterInletTemp,     &
                                     GSHPReport(GSHPNum)%SourceSideWaterOutletTemp,    &
                                     GSHPReport(GSHPNum)%SourceSidemdot,          &
                                     FirstHVACIteration)
 ELSE
   CALL ShowFatalError ('SimHPWatertoWaterHEATING:: Invalid loop connection '//ModuleCompName//', Requested Unit='//TRIM(GSHPName))
 ENDIF

RETURN
END SUBROUTINE SimHPWatertoWaterHEATING

SUBROUTINE GetGshpInput
            !       SUBROUTINE INFORMATION:
            !       AUTHOR:
            !       DATE WRITTEN:    April 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the GSHP models.  As such
            ! it will interact with the Input Scanner to retrieve
            ! information from the input file, count the number of
            ! GSHPs and begin to fill the
            ! arrays associated with the type GSHP.


            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:
  USE DataPlant, ONLY: TypeOf_HPWaterPEHeating, ScanPlantLoopsForObject
  USE InputProcessor,        ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE NodeInputManager,      ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY : TestCompSet
  USE FluidProperties,       ONLY : FindRefrigerant
  USE PlantUtilities,  ONLY: RegisterPlantCompDesignFlow

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: GshpNum                    !Gshp counter
  INTEGER                     :: NumAlphas                 ! Number of elements in the alpha array
  INTEGER                     :: NumNums                   ! Number of elements in the numeric array
  INTEGER                     :: IOStat                    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(5)   :: AlphArray !character string data
  REAL(r64),                        DIMENSION(23)  :: NumArray  !numeric data

  LOGICAL, SAVE :: ErrorsFound = .false.
  LOGICAL       :: IsNotOk          ! Flag to verify name
  LOGICAL       :: IsBlank          ! Flag for blank name
  LOGICAL :: errFlag

  NumGshps = GetNumObjectsFound(ModuleCompName)

  IF(NumGshps <= 0) THEN
    CALL ShowSevereError(ModuleCompName//': No Equipment found')
    ErrorsFound=.true.
 END IF

   ! Allocate Arrays
  ALLOCATE (GSHP(NumGshps))
  ALLOCATE (GshpReport(NumGshps))
  ALLOCATE(CheckEquipName(NumGshps))
  CheckEquipName=.true.


  DO GshpNum = 1, NumGshps
   CALL GetObjectItem(ModuleCompNameUC,GshpNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)
        IsNotOk=.false.
        IsBlank=.true.
   CALL VerifyName(AlphArray(1),GSHP%Name,GSHPNum-1, ISNotOK,ISBlank,'GHSP Name')

   IF (ISNotOK) THEN
       ErrorsFound=.true.
       IF(ISBlank) AlphArray(1)='xxxxx'
   END IF
    GSHP(GSHPNum)%Name                = AlphArray(1)

    GSHP(GSHPNum)%WWHPPlantTypeOfNum    = TypeOf_HPWaterPEHeating

    GSHP(GSHPNum)%COP                 = NumArray(1)
    IF(NumArray(1) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':COP = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    ! zero values for NumArray 3 - 6 checked in input - idd
    GSHP(GSHPNum)%NomCap    = NumArray(2)

    GSHP(GSHPNum)%MinPartLoadRat    = NumArray(3)

    GSHP(GSHPNum)%MaxPartLoadRat    = NumArray(4)

    GSHP(GSHPNum)%OptPartLoadRat    = NumArray(5)

    GSHP(GSHPNum)%LoadSideVolFlowRate    = NumArray(6)
    IF(NumArray(6) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Load Side Flow Rate = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%SourceSideVolFlowRate    = NumArray(7)
    IF(NumArray(7) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Source Side Flow Rate = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%LoadSideUACoeff          = NumArray(8)
    IF(NumArray(8) == 0.0d0) THEN
      CALL ShowSevereError(ModuleCompName//':Load Side Heat Transfer Coeffcient = 0.0, Heatpump='//TRIM(AlphArray(1)))
      ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%SourceSideUACoeff          = NumArray(9)
    IF(NumArray(9) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Source Side Heat Transfer Coeffcient = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%CompPistonDisp       = NumArray(10)
    IF(NumArray(10) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Compressor Piston displacement/Storke = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%CompClearanceFactor  = NumArray(11)
    IF(NumArray(11) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Compressor Clearance Factor = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%CompSucPressDrop     = NumArray(12)
    IF(NumArray(12)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//': Pressure Drop = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%SuperheatTemp        = NumArray(13)
    IF(NumArray(13) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Source Side SuperHeat = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%PowerLosses          = NumArray(14)
    IF(NumArray(14) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Compressor Power Loss = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF
    GSHP(GSHPNum)%LossFactor           = NumArray(15)
    IF(NumArray(15) == 0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Efficiency = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%HighPressCutOff        = NumArray(16)
    IF(NumArray(16) == 0.0d0) THEN
       GSHP(GSHPNum)%HighPressCutOff        = 500000000.0d0
       !CALL ShowWarningError(ModuleCompName//': High Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
    END IF

    GSHP(GSHPNum)%LowPressCutOff        = NumArray(17)
    IF(NumArray(17) == 0.0d0) THEN
       GSHP(GSHPNum)%LowPressCutOff        = 0.0d0
       !CALL ShowWarningError(ModuleCompName//': Low Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
    END IF

    GSHP(GSHPNum)%SourceSideInletNodeNum   =   &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,ModuleCompName,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    GSHP(GSHPNum)%SourceSideOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,ModuleCompName,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

    GSHP(GSHPNum)%LoadSideInletNodeNum    =  &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,ModuleCompName,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

    GSHP(GSHPNum)%LoadSideOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,ModuleCompName,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)


    ! Test node sets
    CALL TestCompSet(ModuleCompNameUC,AlphArray(1),AlphArray(2),AlphArray(3),'Condenser Water Nodes')
    CALL TestCompSet(ModuleCompNameUC,AlphArray(1),AlphArray(4),AlphArray(5),'Hot Water Nodes')

    ! save the design source side flow rate for use by plant loop sizing algorithms
    CALL RegisterPlantCompDesignFlow(GSHP(GSHPNum)%SourceSideInletNodeNum,0.5d0*GSHP(GSHPNum)%SourceSideVolFlowRate)

  END DO

  IF (ErrorsFound)THEN
    CALL ShowFatalError('Errors Found in getting '//ModuleCompNameUC//' Input')
  END IF

  GSHPRefrigIndex=FindRefrigerant(GSHPRefrigerant)
  IF (GSHPRefrigIndex == 0) THEN
    CALL ShowFatalError('Refrigerant for HeatPump:WaterToWater Heating not found, should have been='//TRIM(GSHPRefrigerant))
  ENDIF

  ! CurrentModuleObject='HeatPump:WaterToWater:ParameterEstimation:Heating'
  DO GSHPNum = 1,NumGshps
   CALL SetupOutputVariable('Water to Water Heat Pump Electric Power [W]', &
        GshpReport(GSHPNum)%Power,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Electric Energy [J]', &
        GshpReport(GSHPNum)%Energy,'System','Sum',GSHP(GSHPNum)%Name,  &
        ResourceTypeKey='Electricity',EndUseKey='Heating',GroupKey='Plant')

   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Rate [W]', &
        GshpReport(GSHPNum)%QLoad,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Energy [J]', &
        GshpReport(GSHPNum)%QLoadEnergy,'System','Sum',GSHP(GSHPNum)%Name)

   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Rate [W]', &
        GshpReport(GSHPNum)%QSource,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Energy [J]', &
        GshpReport(GSHPNum)%QSourceEnergy,'System','Sum',GSHP(GSHPNum)%Name)

   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Outlet Temperature [C]', &
        GshpReport(GSHPNum)%LoadSideWaterOutletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Inlet Temperature [C]', &
        GshpReport(GSHPNum)%LoadSideWaterInletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Outlet Temperature [C]', &
        GshpReport(GSHPNum)%SourceSideWaterOutletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Inlet Temperature [C]', &
        GshpReport(GSHPNum)%SourceSideWaterInletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Mass Flow Rate [kg/s]', &
        GshpReport(GSHPNum)%LoadSidemdot,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Mass Flow Rate [kg/s]', &
        GshpReport(GSHPNum)%SourceSidemdot,'System','Average',GSHP(GSHPNum)%Name)

    !scan for loop connection data
   errFlag=.false.
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 GSHP(GSHPNum)%WWHPPlantTypeOfNum, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum, &
                                 inletNodeNumber = GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                 errflag=errFlag)
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 GSHP(GSHPNum)%WWHPPlantTypeOfNum, &
                                 GSHP(GSHPNum)%LoadLoopNum, &
                                 GSHP(GSHPNum)%LoadLoopSideNum, &
                                 GSHP(GSHPNum)%LoadBranchNum, &
                                 GSHP(GSHPNum)%LoadCompNum, &
                                 inletNodeNumber = GSHP(GSHPNum)%LoadSideInletNodeNum,  &
                                 errflag=errFlag)


    IF (errFlag) THEN
      CALL ShowFatalError('GetWatertoWaterHPInput: Program terminated on scan for loop data')
    ENDIF


  END DO

RETURN
END SUBROUTINE GetGshpInput

SUBROUTINE InitGshp(GSHPNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    July 2007

            ! PURPOSE OF THIS SUBROUTINE:
            ! initialization


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataPlant,       ONLY: PlantLoop
  USE FluidProperties, ONLY: GetDensityGlycol
  USE PlantUtilities,  ONLY: InitComponentNodes

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: GSHPNum       ! GSHP number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:)  :: MyEnvrnFlag
  LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:)  :: MyPlanScanFlag
  LOGICAL, SAVE    :: MyOneTimeFlag = .TRUE.
  REAL(r64)        :: rho  ! local fluid density

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyPlanScanFlag(NumGSHPs))
    ALLOCATE(MyEnvrnFlag(NumGSHPs))
    MyOneTimeFlag = .false.
    MyEnvrnFlag  = .TRUE.
    MyPlanScanFlag = .TRUE.
  END IF


  !For each new environment
  IF(BeginEnvrnFlag .AND. MyEnvrnFlag(GSHPNum))Then
    GshpReport(GSHPNum)%QLoad = 0.0d0
    GshpReport(GSHPNum)%QSource = 0.0d0
    GshpReport(GSHPNum)%Power = 0.0d0
    GshpReport(GSHPNum)%QLoadEnergy = 0.0d0
    GshpReport(GSHPNum)%QSourceEnergy = 0.0d0
    GshpReport(GSHPNum)%Energy = 0.0d0
    GshpReport(GSHPNum)%LoadSideWaterInletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSideWaterInletTemp = 0.0d0
    GshpReport(GSHPNum)%LoadSideWaterOutletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSideWaterOutletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSidemdot=0.0d0
    GshpReport(GSHPNum)%LoadSidemdot=0.0d0
    GSHP(GSHPNum)%isOn = .FALSE.
    GSHP(GSHPNum)%MustRun = .TRUE.

    MyEnvrnFlag(GSHPNum) = .FALSE.

    rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                         InitconvTemp, &
                         PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                         'InitGshp')
    GSHP(GSHPNum)%LoadSideDesignMassFlow   = GSHP(GSHPNum)%LoadSideVolFlowRate * rho

    CALL InitComponentNodes( 0.d0, GSHP(GSHPNum)%LoadSideDesignMassFlow, &
                                    GSHP(GSHPNum)%LoadSideInletNodeNum, &
                                    GSHP(GSHPNum)%LoadSideOutletNodeNum, &
                                    GSHP(GSHPNum)%LoadLoopNum, &
                                    GSHP(GSHPNum)%LoadLoopSideNum, &
                                    GSHP(GSHPNum)%LoadBranchNum, &
                                    GSHP(GSHPNum)%LoadCompNum)

    rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                         InitconvTemp, &
                         PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                         'InitGshp')
    GSHP(GSHPNum)%SourceSideDesignMassFlow = GSHP(GSHPNum)%SourceSideVolFlowRate * rho

    CALL InitComponentNodes( 0.d0,GSHP(GSHPNum)%SourceSideDesignMassFlow, &
                                 GSHP(GSHPNum)%SourceSideInletNodeNum, &
                                 GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum)

    IF (Node(GSHP(GSHPNum)%SourceSideOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) &
                       Node(GSHP(GSHPNum)%SourceSideOutletNodeNum)%TempSetPoint=0.0d0
    Node(GSHP(GSHPNum)%SourceSideInletNodeNum)%Temp = Node(GSHP(GSHPNum)%SourceSideOutletNodeNum)%TempSetPoint+30

  END IF

  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(GSHPNum)= .TRUE.

  !On every call
  GSHPReport(GSHPNum)%Running = 0

  GSHP(GSHPNum)%MustRun = .TRUE.        ! Reset MustRun Flag to TRUE

  LoadSideWaterMassFlowRate   = 0.0d0     ! Load Side mass flow rate, water side
  SourceSideWaterMassFlowRate = 0.0d0     ! Source Side mass flow rate, water side
  Power   = 0.0d0                         ! power consumption
  QLoad   = 0.0d0                         ! heat rejection from Load Side coil
  QSource = 0.0d0

RETURN
END SUBROUTINE InitGshp


SUBROUTINE CalcGshpModel( GSHPType, GSHPName, GSHPNum, MyLoad,FirstHVACIteration )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       April 1999
          !                      September 2002, SJR
          !       RE-ENGINEERED  Mar2000

          ! PURPOSE OF THIS SUBROUTINE: This routine performs

          ! METHODOLOGY EMPLOYED: under development

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys ,SysTimeElapsed, FirstTimeStepSysFlag
  USE FluidProperties
  USE General,         ONLY: TrimSigDigits
  USE DataPlant,       ONLY: PlantLoop
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE PlantUtilities,  ONLY: SetComponentFlowRate

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: GshpType  ! type ofGSHP
  CHARACTER(len=*), INTENT(IN) :: GshpName  ! user specified name ofGSHP
  INTEGER  , INTENT(IN)     :: GSHPNum      ! GSHP Number
  REAL(r64)             :: MyLoad           ! Operating Load
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER        :: gamma            = 1.114d0             ! Expnasion Coefficient
  REAL(r64), PARAMETER        :: HeatBalTol       = 0.0005d0
  REAL(r64), PARAMETER        :: RelaxParam       = 0.6d0
  REAL(r64), PARAMETER        :: SmallNum         = 1.0d-20
  INTEGER, PARAMETER     :: IterationLimit   = 500


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)              :: SourceSideEffect         ! Source Side effectiveness
  REAL(r64)              :: LoadSideEffect           ! Load Side effectiveness
  REAL(r64)              :: SourceSideTemp           ! Source Side temperature °C
  REAL(r64)              :: LoadSideTemp             ! Load Side temperature °C
  REAL(r64)              :: SourceSideUA             ! Source Side heat transfer coefficient    w/k
  REAL(r64)              :: LoadSideUA               ! Load Side heat transfer coefficient W/k
  REAL(r64)              :: SourceSidePressure       ! Source Side pressure Pascals
  REAL(r64)              :: LoadSidePressure         ! Load Side pressure Pascals
  REAL(r64)              :: SuctionPr                ! Suction Pressure  pascals
  REAL(r64)              :: DischargePr              ! Discharge Pressure pascals
  REAL(r64)              :: CompressInletTemp        ! Compressor inlet temperature  °C
  REAL(r64)              :: PressureDrop             ! Suction Pressure drop °C
  REAL(r64)              :: ClearanceFactor          ! Clearance factor
  REAL(r64)              :: PistonDisp               ! Compressor piston displacement  m3
  REAL(r64)              :: ShTemp                   ! Superheat temperature °C
  REAL(r64)              :: LosFac                   ! Loss factor used to define the electromechanical loss for compressor
  REAL(r64)              :: MassRef                  ! mass flow rate of refrigerant Kg/s
  REAL(r64)              :: SourceSideOutletEnth     ! Enthalpy at Source Side pressure Joules
  REAL(r64)              :: LoadSideOutletEnth       ! Enthalpy at Condensor Pressure  Joules
  REAL(r64)              :: initialQSource           ! Guess Source Side Heat rate Joules
  REAL(r64)              :: initialQLoad             ! Guess Load Side Heat rate Joules
  REAL(r64)              :: qual                     ! quality
  REAL(r64)              :: SuperHeatEnth
  REAL(r64)              :: T110
  REAL(r64)              :: T111
  REAL(r64)              :: CompSuctionTemp
  REAL(r64)              :: CompSuctionEnth
  REAL(r64)              :: CompSuctionDensity
  REAL(r64)              :: PowerLosses
  REAL(r64)              :: CompSuctionSatTemp
  REAL(r64)              :: HighPressCutOff
  REAL(r64)              :: LowPressCutOff
  CHARACTER(len=25)      :: ErrString
  REAL(r64)              :: DutyFactor
  INTEGER                :: IterationCount

  REAL(r64), SAVE :: CurrentSimTime = 0.0d0
  REAL(r64), SAVE :: PrevSimTime = 0.0d0
  LOGICAL, SAVE          :: OneTimeFlag = .TRUE.
  ! Nodes
  INTEGER                :: SourceSideInletNode      ! Source Side inlet node number, water side
  INTEGER                :: SourceSideOutletNode     ! Source Side outlet node number, water side
  INTEGER                :: LoadSideInletNode        ! Load Side inlet node number, water side
  INTEGER                :: LoadSideOutletNode       ! Load Side outlet node number, water side
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64) :: CpSourceSide ! local temporary for fluid specific heat
  REAL(r64) :: CpLoadSide ! local temporary for fluid specific heat

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  PressureDrop      = GSHP(GSHPNum)%CompSucPressDrop
  ClearanceFactor   = GSHP(GSHPNum)%CompClearanceFactor
  PistonDisp        = GSHP(GSHPNum)%CompPistonDisp
  ShTemp            = GSHP(GSHPNum)%SuperheatTemp
  LosFac            = GSHP(GSHPNum)%LossFactor
  SourceSideUA      = GSHP(GSHPNum)%SourceSideUACoeff
  LoadSideUA        = GSHP(GSHPNum)%LoadSideUACoeff
  PowerLosses       = GSHP(GSHPNum)%PowerLosses
  HighPressCutOff   = GSHP(GSHPNum)%HighPressCutOff
  LowPressCutOff    = GSHP(GSHPNum)%LowPressCutOff
  ! REPORT VAR
  GSHPReport(GSHPNum)%Running = 0

  ! Init Module level Variables
  GSHP(GSHPNum)%MustRun = .TRUE.      ! Reset MustRun Flag to TRUE
  LoadSideWaterMassFlowRate  = 0.0d0    ! Load Side mass flow rate, water side
  SourceSideWaterMassFlowRate = 0.0d0   ! Source Side mass flow rate, water side
  Power = 0.0d0                         ! power consumption
  QLoad = 0.0d0                         ! heat rejection from Load Side coil
  QSource = 0.0d0

  LoadSideInletNode    = GSHP(GSHPNum)%LoadSideInletNodeNum
  LoadSideOutletNode   = GSHP(GSHPNum)%LoadSideOutletNodeNum
  SourceSideInletNode  = GSHP(GSHPNum)%SourceSideInletNodeNum
  SourceSideOutletNode = GSHP(GSHPNum)%SourceSideOutletNodeNum
  LoopNum              = GSHP(GSHPNum)%LoadLoopNum
  LoopSideNum          = GSHP(GSHPNum)%LoadLoopSideNum

  IF(PrevSimTime .NE. CurrentSimTime)THEN
     PrevSimTime = CurrentSimTime
  END IF

  ! CALCULATE THE SIMULATION TIME
  CurrentSimTime = (dayofSim-1)*24 + hourofday-1 + (timestep-1)*timestepZone + SysTimeElapsed

  ! initialize event time array when the environment simulation begins
  IF(CurrentSimTime == 0.0d0 .AND. OneTimeFlag)THEN
    OneTimeFlag = .FALSE.
  END IF

  IF(CurrentSimTime > 0.0d0 )OneTimeFlag = .TRUE.

  IF(MyLoad > 0.0d0)THEN
    GSHP(GSHPNum)%MustRun = .TRUE.
    GSHP(GSHPNum)%IsOn = .TRUE.
  ELSE
    GSHP(GSHPNum)%MustRun = .FALSE.
    GSHP(GSHPNum)%IsOn = .FALSE.
  END IF


!*******Set flow based on "run" flags**********
! Set flows if the heat pump is not running
  IF( .NOT. GSHP(GSHPNum)%MustRun )THEN
    LoadSideWaterMassFlowRate = 0.d0
    Call SetComponentFlowRate(LoadSideWaterMassFlowRate, &
          LoadSideInletNode, LoadSideOutletNode, &
          GSHP(GSHPNum)%LoadLoopNum, GSHP(GSHPNum)%LoadLoopSideNum, &
          GSHP(GSHPNum)%LoadBranchNum, GSHP(GSHPNum)%LoadCompNum)
    SourceSideWaterMassFlowRate = 0.d0
    Call SetComponentFlowRate(SourceSideWaterMassFlowRate, &
          SourceSideInletNode, SourceSideOutletNode, &
          GSHP(GSHPNum)%SourceLoopNum, GSHP(GSHPNum)%SourceLoopSideNum, &
          GSHP(GSHPNum)%SourceBranchNum, GSHP(GSHPNum)%SourceCompNum)
        !now initialize simulation variables for "heat pump off"
    QLoad   = 0.0d0
    QSource = 0.0d0
    Power   = 0.0d0
    LoadSideWaterInletTemp      = Node(LoadSideInletNode)%Temp
    LoadSideWaterOutletTemp     = LoadSideWaterInletTemp
    SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
    SourceSideWaterOutletTemp   = SourceSideWaterInletTemp
    RETURN !if heat pump is not running return without simulation

! Set flows if the heat pump is running
  ELSE ! the heat pump must run, request design flow

    LoadSideWaterMassFlowRate = GSHP(GSHPNum)%LoadSideDesignMassFlow
    CALL SetComponentFlowRate(LoadSideWaterMassFlowRate, &
          LoadSideInletNode, LoadSideOutletNode, &
          GSHP(GSHPNum)%LoadLoopNum, GSHP(GSHPNum)%LoadLoopSideNum, &
          GSHP(GSHPNum)%LoadBranchNum, GSHP(GSHPNum)%LoadCompNum)

    SourceSideWaterMassFlowRate = GSHP(GSHPNum)%SourceSideDesignMassFlow
    CALL SetComponentFlowRate(SourceSideWaterMassFlowRate, &
          SourceSideInletNode, SourceSideOutletNode, &
          GSHP(GSHPNum)%SourceLoopNum, GSHP(GSHPNum)%SourceLoopSideNum, &
          GSHP(GSHPNum)%SourceBranchNum, GSHP(GSHPNum)%SourceCompNum)
    ! get inlet temps
    LoadSideWaterInletTemp      = Node(LoadSideInletNode)%Temp
    SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
    !if there's no flow, turn the "heat pump off"
    IF(LoadSideWaterMassFlowRate < MassFlowTolerance .OR. &
         SourceSideWaterMassFlowRate < MassFlowTolerance)THEN
        LoadSideWaterMassFlowRate = 0.d0
        Call SetComponentFlowRate(LoadSideWaterMassFlowRate, &
              LoadSideInletNode, LoadSideOutletNode, &
              GSHP(GSHPNum)%LoadLoopNum, GSHP(GSHPNum)%LoadLoopSideNum, &
              GSHP(GSHPNum)%LoadBranchNum, GSHP(GSHPNum)%LoadCompNum)
        SourceSideWaterMassFlowRate = 0.d0
        Call SetComponentFlowRate(SourceSideWaterMassFlowRate, &
              SourceSideInletNode, SourceSideOutletNode, &
              GSHP(GSHPNum)%SourceLoopNum, GSHP(GSHPNum)%SourceLoopSideNum, &
              GSHP(GSHPNum)%SourceBranchNum, GSHP(GSHPNum)%SourceCompNum)
        QLoad = 0.0d0
        QSource = 0.0d0
        Power = 0.0d0
        LoadSideWaterInletTemp      = Node(LoadSideInletNode)%Temp
        LoadSideWaterOutletTemp     = LoadSideWaterInletTemp
        SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
        SourceSideWaterOutletTemp   = SourceSideWaterInletTemp
      RETURN
    END IF
  END IF

!***********BEGIN CALCULATION****************
! initialize the source and load side heat transfer rates for the simulation
  initialQSource = 0.0d0
  initialQLoad   = 0.0d0
  IterationCount = 0

  CpSourceSide = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                                       SourceSideWaterInletTemp, &
                                       PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                                       'CalcGshpModel')

  CpLoadSide = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                                       LoadSideWaterInletTemp, &
                                       PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                                       'CalcGshpModel')

  ! Determine effectiveness of Source Side (the Evaporator in heating mode)
  SourceSideEffect = 1.0d0- EXP( -SourceSideUA / &
                            (CpSourceSide * SourceSideWaterMassFlowRate))
  !Determine effectiveness of Load Side the condenser in heating mode
  LoadSideEffect = 1.0d0- EXP ( -LoadSideUA / &
                           (CpLoadSide * LoadSideWaterMassFlowRate))

  LOOPLoadEnth: DO  ! main loop to solve model equations
    IterationCount = IterationCount+1
    ! Determine Source Side tempertaure
    SourceSideTemp = SourceSideWaterInletTemp - initialQSource/ &
                     (SourceSideEffect * CpSourceSide * SourceSideWaterMassFlowRate)

    ! To determine Load Side temperature condenser
    LoadSideTemp = LoadSideWaterInletTemp + initialQLoad/ &
                   (LoadSideEffect * CpLoadSide * LoadSideWaterMassFlowRate)

    ! Determine the evaporating and condensing pressures
    SourceSidePressure = GetSatPressureRefrig(GSHPRefrigerant,SourceSideTemp,GSHPRefrigIndex,'CalcGSHPModel:SourceSideTemp')
    LoadSidePressure = GetSatPressureRefrig(GSHPRefrigerant,LoadSideTemp,GSHPRefrigIndex,'CalcGSHPModel:LoadSideTemp')

    ! check cutoff pressures
    IF (SourceSidePressure < LowPressCutOff) THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Heating Source Side Pressure Less than the Design Minimum')
      CALL ShowContinueError('Source Side Pressure='//TRIM(TrimSigDigits(SourceSidePressure,2))//  &
                             ' and user specified Design Minimum Pressure='//TRIM(TrimSigDigits(LowPressCutoff,2)))
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF
    IF (LoadSidePressure > HighPressCutOff)THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Heating Load Side Pressure greater than the Design Maximum')
      CALL ShowContinueError('Load Side Pressure='//TRIM(TrimSigDigits(LoadSidePressure,2))//  &
                             ' and user specified Design Maximum Pressure='//TRIM(TrimSigDigits(HighPressCutOff,2)))
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF

    ! Determine Suction Pressure at compressor inlet
    SuctionPr = SourceSidePressure - PressureDrop
    ! Determine Discharge Pressure at compressor exit
    DischargePr = LoadSidePressure + PressureDrop
    ! check cutoff pressures
    IF (SuctionPr < LowPressCutOff) THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Heating Suction Pressure Less than the Design Minimum')
      CALL ShowContinueError('Heating Suction Pressure='//TRIM(TrimSigDigits(SuctionPr,2))//  &
                             ' and user specified Design Minimum Pressure='//TRIM(TrimSigDigits(LowPressCutoff,2)))
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF
    IF (DischargePr > HighPressCutOff)THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Heating Discharge Pressure greater than the Design Maximum')
      CALL ShowContinueError('Heating Discharge Pressure='//TRIM(TrimSigDigits(DischargePr,2))//  &
                             ' and user specified Design Maximum Pressure='//TRIM(TrimSigDigits(HighPressCutOff,2)))
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF

    ! Determine the Source Side Outlet Enthalpy
    qual=1.0d0
    SourceSideOutletEnth = GetSatEnthalpyRefrig(GSHPRefrigerant, SourceSideTemp, qual,   &
       GSHPRefrigIndex,'CalcGSHPModel:SourceSideTemp')

    ! Determine Load Side Outlet Enthalpy
    qual= 0.0d0
    LoadSideOutletEnth = GetSatEnthalpyRefrig(GSHPRefrigerant,LoadSideTemp,qual,  &
       GSHPRefrigIndex,'CalcGSHPModel:LoadSideTemp')

    ! Determine superheated temperature of the Source Side outlet/compressor inlet
    CompressInletTemp = SourceSideTemp + ShTemp
    ! Determine the enathalpy of the super heated fluid at Source Side outlet
    SuperHeatEnth = GetSupHeatEnthalpyRefrig(GSHPRefrigerant,CompressInletTemp,SourceSidePressure,  &
       GSHPRefrigIndex,'CalcGSHPModel:CompressInletTemp')

    ! Determining the suction state of the fluid from inlet state involves interation
    ! Method employed...
    ! Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
    ! check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

    CompSuctionSatTemp = GetSatTemperatureRefrig(GSHPRefrigerant,SuctionPr,GSHPRefrigIndex,'CalcGSHPModel:SuctionPr')

    T110 = CompSuctionSatTemp
    !Shoot into the super heated region
    T111 = CompSuctionSatTemp + 80

    ! Iterate to find the Suction State - given suction pressure and superheat enthalpy
    LOOP: DO
      CompSuctionTemp = 0.5d0 * ( T110 + T111 )

      CompSuctionEnth = GetSupHeatEnthalpyRefrig(GSHPRefrigerant,CompSuctionTemp,SuctionPr,  &
         GSHPRefrigIndex,'CalcGSHPModel:CompSuctionTemp')
      IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < .0001d0)  THEN
          EXIT LOOP
      END IF

      IF ( CompsuctionEnth < SuperHeatEnth ) THEN
        T110 = CompSuctionTemp
      ELSE
        T111 = CompSuctionTemp
      END IF
    END DO LOOP

    ! Determine the Mass flow rate of refrigerant
    CompSuctionDensity = GetSupHeatDensityRefrig(GSHPRefrigerant, CompSuctionTemp, SuctionPr,   &
       GSHPRefrigIndex,'CalcGSHPModel:CompSuctionTemp')
    MassRef = PistonDisp * CompSuctionDensity * (1+ClearanceFactor-ClearanceFactor* &
              ((DischargePr/SuctionPr)**(1.d0/gamma)))

    ! Find the  Source Side Heat Transfer
    QSource = MassRef * ( SourceSideOutletEnth - LoadSideOutletEnth )

    ! Determine the theoretical power
    Power = PowerLosses+(MassRef*gamma/(gamma-1) * SuctionPr /CompSuctionDensity/LosFac * &
            ((DischargePr/SuctionPr)**((gamma-1)/gamma) - 1))

    ! Determine the Loadside HeatRate (QLoad)
    QLoad = Power + QSource

    ! convergence and iteration limit check
    IF(ABS((QLoad - initialQLoad)/(initialQLoad+SmallNum)) < HeatBalTol .OR. IterationCount>IterationLimit) THEN
      IF(IterationCount>IterationLimit)then
        CALL ShowWarningError(ModuleCompName//' did not converge')
        CALL ShowContinueErrorTimeStamp('  ')
        CALL ShowContinueError('Heatpump Name = '//TRIM(GSHP(GSHPNum)%Name))
        WRITE(ErrString,*) ABS(100.0d0*(QLoad - initialQLoad)/(initialQLoad+SmallNum))
        CALL ShowContinueError('Heat Inbalance (%)             = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) QLoad
        CALL ShowContinueError('Load-side heat transfer rate   = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) Qsource
        CALL ShowContinueError('Source-side heat transfer rate = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) SourceSideWaterMassFlowRate
        CALL ShowContinueError('Source-side mass flow rate     = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) LoadSideWaterMassFlowRate
        CALL ShowContinueError('Load-side mass flow rate       = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) SourceSideWaterInletTemp
        CALL ShowContinueError('Source-side inlet temperature  = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) LoadSideWaterInletTemp
        CALL ShowContinueError('Load-side inlet temperature    = '//TRIM(ADJUSTL(ErrString)))
      END IF
      EXIT LOOPLoadEnth

    ELSE ! update load
      initialQLoad= initialQLoad+ RelaxParam*(QLoad-initialQLoad)
      initialQSource = initialQSource + RelaxParam*(QSource - initialQSource)
    END IF

  END DO LOOPLoadEnth

  !Control Strategy
  IF(ABS(MyLoad) < QLoad) THEN
    DutyFactor = ABS(MyLoad)/QLoad
    QLoad = ABS(MyLoad)
    Power = DutyFactor * Power
    QSource = QSource * DutyFactor

    ! Determine the Exterior fluid temperature at the Load Side oulet and eveporator outlet...
    ! Refrigerant = "Steam"
    LoadSideWaterOutletTemp   = LoadSideWaterInletTemp + QLoad/(LoadSideWaterMassFlowRate * &
                                CpLoadSide)
    SourceSideWaterOutletTemp = SourceSideWaterInletTemp - QSource/(SourceSideWaterMassFlowRate * &
                                CpSourceSide)
    RETURN
  END IF

  LoadSideWaterOutletTemp   = LoadSideWaterInletTemp + QLoad/(LoadSideWaterMassFlowRate * &
                              CpLoadSide)
  SourceSideWaterOutletTemp = SourceSideWaterInletTemp - QSource/(SourceSideWaterMassFlowRate * &
                              CpSourceSide )
  ! REPORT VAR
  GSHPReport(GSHPNum)%Running = 1

  RETURN

END SUBROUTINE CalcGshpModel

SUBROUTINE UpdateGSHPRecords(GSHPNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Arun
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys


  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: GSHPNum       ! GSHP number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: SourceSideInletNode      ! Source Side inlet node number, water side
  INTEGER     :: SourceSideOutletNode     ! Source Side outlet node number, water side
  INTEGER     :: LoadSideInletNode        ! Load Side inlet node number, water side
  INTEGER     :: LoadSideOutletNode       ! Load Side outlet node number, water side
  REAL(r64) :: ReportingConstant

    LoadSideInletNode    = GSHP(GSHPNum)%LoadSideInletNodeNum
    LoadSideOutletNode   = GSHP(GSHPNum)%LoadSideOutletNodeNum
    SourceSideInletNode  = GSHP(GSHPNum)%SourceSideInletNodeNum
    SourceSideOutletNode = GSHP(GSHPNum)%SourceSideOutletNodeNum

  IF (.NOT. GSHP(GSHPNum)%MustRun )THEN
          !set node temperatures
    Node(SourceSideOutletNode)%Temp     = Node(SourceSideInletNode)%Temp
    Node(LoadSideOutletNode)%Temp       = Node(LoadSideInletNode)%Temp

    GSHPReport(GSHPNum)%Power                = 0.0d0
    GSHPReport(GSHPNum)%Energy               = 0.0d0
    GSHPReport(GSHPNum)%QSource              = 0.0d0
    GSHPReport(GSHPNum)%QSourceEnergy        = 0.0d0
    GSHPReport(GSHPNum)%QLoad                = 0.0d0
    GSHPReport(GSHPNum)%QLoadEnergy          = 0.0d0
    GSHPReport(GSHPNum)%SourceSideWaterInletTemp  = Node(SourceSideInletNode)%Temp
    GSHPReport(GSHPNum)%SourceSideWaterOutletTemp = Node(SourceSideOutletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterInletTemp   = Node(LoadSideInletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterOutletTemp  = Node(LoadSideOutletNode)%Temp
    GSHPReport(GSHPNum)%SourceSidemdot       = SourceSideWaterMassFlowRate
    GSHPReport(GSHPNum)%LoadSidemdot         = LoadSideWaterMassFlowRate

  ELSE
         !set node temperatures
    Node(LoadSideOutletNode)%Temp      = LoadSideWaterOutletTemp
    Node(SourceSideOutletNode)%Temp    = SourceSideWaterOutletTemp

    ReportingConstant = TimeStepSys*SecInHour
    GSHPReport(GSHPNum)%Power       = Power
    GSHPReport(GSHPNum)%Energy      = Power*ReportingConstant
    GSHPReport(GSHPNum)%QSource     = QSource
    GSHPReport(GSHPNum)%QLoad       = QLoad
    GSHPReport(GSHPNum)%QSourceEnergy = QSource*ReportingConstant
    GSHPReport(GSHPNum)%QLoadEnergy   = QLoad*ReportingConstant
    GSHPReport(GSHPNum)%LoadSideWaterInletTemp    = Node(LoadSideInletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterOutletTemp   = Node(LoadSideOutletNode)%Temp
    GSHPReport(GSHPNum)%SourceSideWaterInletTemp = Node(SourceSideInletNode)%Temp
    GSHPReport(GSHPNum)%SourceSideWaterOutletTemp= Node(SourceSideOutletNode)%Temp
    GSHPReport(GSHPNum)%SourceSidemdot       = SourceSideWaterMassFlowRate
    GSHPReport(GSHPNum)%LoadSidemdot         = LoadSideWaterMassFlowRate

  END IF
RETURN
END SUBROUTINE UpdateGSHPRecords

END MODULE HeatPumpWaterToWaterHEATING
!************************************************************************************
!
!==================================== MODULE HeatPumpWaterToWaterCOOLING ======================
!
!************************************************************************************
MODULE HeatPumpWaterToWaterCOOLING
  ! Module containing the routines dealing with the Water to Water Heat Pump (Cooling)

  ! MODULE INFORMATION:
  !       AUTHOR         ARUN
  !       DATE WRITTEN   7/18/2000
  !       MODIFIED       ARUN: 6/27/2002: Cycle Time
  !                      L Lawrie: V1.1.1 (5/20/2003) add meters and energy to several reporting variables
  !                      L Lawrie: V1.1.1 (5/20/2003) restructure modules to comply with standard templates
  !                      B Griffith, Sept 2010, plant upgrades, general fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module simulates a water to Water Heat Pump (Cooling)

  ! METHODOLOGY EMPLOYED:
  ! This simulation is based on a set of selected parameters,
  ! Which are obtained using Parameter Estimation technique.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BeginSimFlag,InitconvTemp,BeginEnvrnFlag, HourOfDay,   &
                              TimeStep,TimeStepZone,DayOfSim,WarmupFlag,SecInHour
USE DataInterfaces
USE DataLoopNode

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: ModuleCompName='HeatPump:WaterToWater:ParameterEstimation:Cooling'
CHARACTER(len=*), PARAMETER :: ModuleCompNameUC='HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING'

  ! DERIVED TYPE DEFINITIONS

    ! Type Description of Heat Pump
TYPE GshpSpecs   ! Needs Some Modifications talk with Dr.Fisher and decide....
  CHARACTER(len=MaxNameLength) :: Name          = ' ' ! user identifier
  INTEGER            :: WWHPPlantTypeOfNum
  LOGICAL            :: Available           = .false. ! need an array of logicals--load identifiers of available equipment
  LOGICAL            :: ON                  = .false. ! simulate the machine at it's operating part load ratio
  REAL(r64)          :: COP                     = 0.0d0 ! Coefficeint of Performance of the machine
  REAL(r64)          :: NomCap                  = 0.0d0 ! Nomial Capcity of the HeatPump
  REAL(r64)          :: MinPartLoadRat          = 0.0d0 ! Minimum operating Part Load Ratio
  REAL(r64)          :: MaxPartLoadRat          = 0.0d0 ! Maximum operating Part Load Ratio
  REAL(r64)          :: OptPartLoadRat          = 0.0d0 ! Optimal operating Part Load Ratio
  REAL(r64)          :: LoadSideVolFlowRate     = 0.0d0 ! Design Flow Rate on the Load side
  REAL(r64)          :: LoadSideDesignMassFlow  = 0.d0 ! Design flow rate (kg/s)
  REAL(r64)          :: SourceSideVolFlowRate   = 0.0d0 ! Design Flow Rate on th Source Side
  REAL(r64)          :: SourceSideDesignMassFlow = 0.d0 ! Design flow rate (kg/s)
  INTEGER            :: SourceSideInletNodeNum  = 0   ! Node number on the inlet side of the plant
  INTEGER            :: SourceSideOutletNodeNum = 0   ! Node number on the outlet side of the plant
  INTEGER            :: LoadSideInletNodeNum    = 0   ! Node number on the inlet side of the Load Side
  INTEGER            :: LoadSideOutletNodeNum   = 0   ! Node number on the outlet side of the Load Side
  REAL(r64)          :: SourceSideUACoeff       = 0.0d0 ! Source Side heat transfer coeff
  REAL(r64)          :: LoadSideUACoeff         = 0.0d0 ! Load Side heat transfer coeff
  REAL(r64)          :: CompPistonDisp          = 0.0d0 ! compressor piston displacement
  REAL(r64)          :: CompClearanceFactor     = 0.0d0 ! compressor clearance factor
  REAL(r64)          :: CompSucPressDrop        = 0.0d0 ! deltap ,  compressor suction and discharge pressure drop
  REAL(r64)          :: SuperheatTemp           = 0.0d0 ! deltatsh , super heating
  REAL(r64)          :: PowerLosses             = 0.0d0 ! constant part of electro mechanical power losses
  REAL(r64)          :: LossFactor              = 0.0d0 ! loss factor used ot define the electro mechanical loss
                                                      !  that is supposed to be proportional to the theoretical power
  REAL(r64)          :: HighPressCutOff         = 0.0d0 ! Maximum Design Pressure on the Load Side
  REAL(r64)          :: LowPressCutOff          = 0.0d0 ! Minimum Design Pressure on the Source Side

      ! Added by Arun 6-27-02
  ! to implement cycletime - removed 9/10/2013 LKL
  LOGICAL            :: IsOn                      = .false.
  LOGICAL      :: MustRun                         = .false.
  !loop topology variables
  INTEGER            :: SourceLoopNum           = 0 ! source side plant loop index number
  INTEGER            :: SourceLoopSideNum       = 0 ! source side plant loop side index
  INTEGER            :: SourceBranchNum         = 0 ! source side plant loop branch index
  INTEGER            :: SourceCompNum           = 0 ! source side plant loop component index
  INTEGER            :: LoadLoopNum             = 0 ! load side plant loop index number
  INTEGER            :: LoadLoopSideNum         = 0 ! load side plant loop side index
  INTEGER            :: LoadBranchNum           = 0 ! load side plant loop branch index
  INTEGER            :: LoadCompNum             = 0 ! load side plant loop component index
END TYPE GshpSpecs


    ! Output Variables Type definition
TYPE ReportVars
  REAL(r64)    :: Power                       = 0.0d0 ! Power Consumption Watts
  REAL(r64)    :: Energy                      = 0.0d0 ! Energy Consumption Joules
  REAL(r64)    :: QLoad                       = 0.0d0 ! Load Side heat transfer rate Watts
  REAL(r64)    :: QLoadEnergy                 = 0.0d0 ! Load Side heat transfer Joules
  REAL(r64)    :: QSource                     = 0.0d0 ! Source Side heat transfer rate Watts
  REAL(r64)    :: QSourceEnergy               = 0.0d0 ! Source Side heat transfer Joules
  REAL(r64)    :: LoadSideWaterInletTemp      = 0.0d0 ! Load Side outlet temperature °C
  REAL(r64)    :: SourceSideWaterInletTemp    = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)    :: LoadSideWaterOutletTemp     = 0.0d0 ! Load Side outlet temperature °C
  REAL(r64)    :: SourceSideWaterOutletTemp   = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)    :: LoadSidemdot                = 0.0d0 ! Mass flow rate of the cooling water in Load Side kg/s
  REAL(r64)    :: SourceSidemdot              = 0.0d0 ! Mass flow rate of chilled water in Eavporator kg/s
  INTEGER :: Running                       = 0   ! On reporting Flag
END TYPE ReportVars


    ! MODULE VARIABLE DECLARATIONS:
TYPE (GSHPSpecs), ALLOCATABLE, DIMENSION(:)  ::GSHP          !dimension to number of machines
TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) :: GSHPReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

    CHARACTER(len=3)   :: GSHPRefrigerant='R22'        ! refrigerent name and index
    INTEGER            :: GSHPRefrigIndex=0

    INTEGER     :: NumGSHPs                  = 0   ! number of Gshps specified in input
    REAL(r64)   :: LoadSideWaterMassFlowRate     = 0.0d0 ! Load Side mass flow rate, water side kg/s
    REAL(r64)   :: SourceSideWaterMassFlowRate   = 0.0d0 ! Source Side mass flow rate, water side kg/s
    REAL(r64)   :: Power                         = 0.0d0 ! power consumption Watts
    REAL(r64)   :: QLoad                         = 0.0d0 ! heat rejection from Load Side coil Watts
    REAL(r64)   :: QSource                       = 0.0d0 ! cooling capacity Watts
    REAL(r64)   :: SourceSideWaterOutletTemp     = 0.0d0 ! Source Side outlet temperature °C
    REAL(r64)   :: SourceSideWaterInletTemp      = 0.0d0 ! Source Side outlet temperature °C
    REAL(r64)   :: LoadSideWaterOutletTemp       = 0.0d0 ! Source Side outlet temperature °C
    REAL(r64)   :: LoadSidewaterInletTemp        = 0.0d0 ! Source Side outlet temperature °C



  ! SUBROUTINE SPECIFICATIONS FOR MODULE

  ! Name Public routines, optionally name Private routines within this module
PUBLIC     SimHPWatertoWaterCOOLING
PRIVATE    CalcGshpModel
PRIVATE    GetGshpInput
PRIVATE    InitGshp
PRIVATE    UpdateGSHPRecords

CONTAINS
         ! MODULE SUBROUTINES:

SUBROUTINE SimHPWatertoWaterCOOLING(GSHPType,GSHPName,CompIndex, FirstHVACIteration, &
                                    InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,LoopNum)
          !       SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   Feb 2000
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the  water to water Heat Pump driver.
          ! It gets the input for the models, initializes simulation variables, calls
          ! the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: UpdateChillerComponentCondenserSide
  USE DataPlant, ONLY: TypeOf_HPWaterEFCooling
  USE InputProcessor, ONLY: FindItemInList
  USE DataEnvironment
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: GshpType  ! type ofGSHP
  CHARACTER(len=*), INTENT(IN) :: GshpName  ! user specified name ofGSHP
  INTEGER, INTENT(IN)          :: LoopNum
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip  ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad         ! loop demand component will meet

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE     :: GetInput = .TRUE.  ! then TRUE, calls subroutine to read input file.
  REAL(r64)         :: MinCap             ! W - minimum operating capacity of GSHP
  REAL(r64)         :: MaxCap             ! W - maximum operating capacity of GSHP
  REAL(r64)         :: OptCap             ! W - optimal operating capacity of GSHP
  INTEGER           :: GSHPNum

    !Get input from IDF

  IF (GetInput) THEN
    CALL GetGshpInput
    GetInput = .FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    GSHPNum = FindItemInList( GSHPName, GSHP%Name, NumGSHPs )
    IF (GSHPNum == 0) THEN
      CALL ShowFatalError('SimHPWatertoWaterCOOLING: Unit not found='//TRIM(GSHPName))
    ENDIF
    CompIndex=GSHPNum
  ELSE
    GSHPNum=CompIndex
    IF (GSHPNum > NumGSHPs .or. GSHPNum < 1) THEN
      CALL ShowFatalError('SimHPWatertoWaterCOOLING:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(GSHPNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumGSHPs))//  &
                          ', Entered Unit name='//TRIM(GSHPName))
    ENDIF
    IF (CheckEquipName(GSHPNum)) THEN
      IF (GSHPName /= GSHP(GSHPNum)%Name) THEN
        CALL ShowFatalError('SimHPWatertoWaterCOOLING: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(GSHPNum))// &
                            ', Unit name='//TRIM(GSHPName)//', stored Unit Name for that index='//  &
                            TRIM(GSHP(GSHPNum)%Name))
      ENDIF
      CheckEquipName(GSHPNum)=.false.
    ENDIF
  ENDIF

     ! Calculate Demand on heat pump
  IF (InitLoopEquip) THEN
    MinCap = GSHP(GSHPNum)%NomCap*GSHP(GSHPNum)%MinPartLoadRat
    MaxCap = GSHP(GSHPNum)%NomCap*GSHP(GSHPNum)%MaxPartLoadRat
    OptCap = GSHP(GSHPNum)%NomCap*GSHP(GSHPNum)%OptPartLoadRat
    Return
  END IF

  ! Simulate the model for the Demand "MyLoad"
 IF (LoopNum == GSHP(GSHPNum)%LoadLoopNum) THEN ! chilled water loop
   CALL InitGshp(GSHPNum)
   CALL CalcGshpModel( GSHPType, GSHPName, GSHPNum, MyLoad,FirstHVACIteration)
   CALL UpdateGshpRecords(GSHPNum)
 ELSEIF (LoopNum == GSHP(GSHPNum)%SourceLoopNum) THEN ! condenser loop
   CALL UpdateChillerComponentCondenserSide(GSHP(GSHPNum)%SourceLoopNum, &
                                     GSHP(GSHPNum)%SourceLoopSideNum,     &
                                     TypeOf_HPWaterEFCooling,                     &
                                     GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                     GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                     GSHPReport(GSHPNum)%QSource,             &
                                     GSHPReport(GSHPNum)%SourceSideWaterInletTemp,     &
                                     GSHPReport(GSHPNum)%SourceSideWaterOutletTemp,    &
                                     GSHPReport(GSHPNum)%SourceSidemdot,          &
                                     FirstHVACIteration)
 ELSE
   CALL ShowFatalError ('SimHPWatertoWaterCOOLING:: Invalid loop connection '//ModuleCompName//', Requested Unit='//TRIM(GSHPName))
 ENDIF

RETURN
END SUBROUTINE SimHPWatertoWaterCOOLING

SUBROUTINE GetGshpInput
            !       SUBROUTINE INFORMATION:
            !       AUTHOR:
            !       DATE WRITTEN:    April 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the GSHP models.  As such
            ! it will interact with the Input Scanner to retrieve
            ! information from the input file, count the number of
            ! GSHPs and begin to fill the
            ! arrays associated with the typeGSHP.


            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:
  USE DataPlant, ONLY: TypeOf_HPWaterPECooling, ScanPlantLoopsForObject
  USE InputProcessor,        ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE NodeInputManager,      ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY : TestCompSet
  USE FluidProperties,       ONLY : FindRefrigerant
  USE PlantUtilities,        ONLY : RegisterPlantCompDesignFlow

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: GshpNum                    !Gshp counter
  INTEGER                     :: NumAlphas                 ! Number of elements in the alpha array
  INTEGER                     :: NumNums                   ! Number of elements in the numeric array
  INTEGER                     :: IOStat                    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(5)   :: AlphArray !character string data
  REAL(r64),                        DIMENSION(23)  :: NumArray  !numeric data

  LOGICAL, SAVE :: ErrorsFound = .false.
  LOGICAL       :: IsNotOk          ! Flag to verify name
  LOGICAL       :: IsBlank          ! Flag for blank name
  LOGICAL :: errFlag


  NumGshps = GetNumObjectsFound(ModuleCompNameUC)

  IF(NumGshps <= 0) THEN
    CALL ShowSevereError('No Equipment found in SimGshp')
    ErrorsFound=.true.
  END IF

   ! Allocate Arrays
  ALLOCATE (GSHP(NumGshps))
  ALLOCATE (GshpReport(NumGshps))
  ALLOCATE(CheckEquipName(NumGshps))
  CheckEquipName=.true.


  DO GshpNum = 1, NumGshps
   CALL GetObjectItem(ModuleCompNameUC,GshpNum,AlphArray,NumAlphas, &
                   NumArray,NumNums,IOSTAT)
        IsNotOk=.false.
        IsBlank=.true.
   CALL VerifyName(AlphArray(1),GSHP%Name,GSHPNum-1, ISNotOK,ISBlank,'GHSP Name')

   IF (ISNotOK) THEN
       ErrorsFound=.true.
       IF(ISBlank) AlphArray(1)='xxxxx'
   END IF
    GSHP(GSHPNum)%Name                = AlphArray(1)

    GSHP(GSHPNum)%WWHPPlantTypeOfNum    = TypeOf_HPWaterPECooling

    GSHP(GSHPNum)%COP                 = NumArray(1)
    IF(NumArray(1)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':COP = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    ! zero values for NumArray 3 - 6 checked in input - idd

    GSHP(GSHPNum)%NomCap    = NumArray(2)

    GSHP(GSHPNum)%MinPartLoadRat    = NumArray(3)

    GSHP(GSHPNum)%MaxPartLoadRat    = NumArray(4)

    GSHP(GSHPNum)%OptPartLoadRat    = NumArray(5)

    GSHP(GSHPNum)%LoadSideVolFlowRate    = NumArray(6)
    IF(NumArray(6)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Load Side Vol Flow Rate = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%SourceSideVolFlowRate    = NumArray(7)
    IF(NumArray(7)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Source Side Vol Flow Rate = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%LoadSideUACoeff          = NumArray(8)
    IF(NumArray(9)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Load Side Heat Transfer Coeffcient = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%SourceSideUACoeff          = NumArray(9)
    IF(NumArray(8)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Source Side Heat Transfer Coeffcient = 0.0, Heatpump=' &
                             //TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%CompPistonDisp       = NumArray(10)
    IF(NumArray(10)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Compressor Piston displacement/Storke = 0.0, Heatpump=' &
                             //TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%CompClearanceFactor  = NumArray(11)
    IF(NumArray(11)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Compressor Clearance Factor = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%CompSucPressDrop     = NumArray(12)
    IF(NumArray(12)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//': Pressure Drop = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%SuperheatTemp        = NumArray(13)
    IF(NumArray(13)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Source Side SuperHeat = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%PowerLosses          = NumArray(14)
    IF(NumArray(14)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Compressor Power Loss = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF
    GSHP(GSHPNum)%LossFactor           = NumArray(15)
    IF(NumArray(15)==0.0d0) THEN
       CALL ShowSevereError(ModuleCompName//':Efficiency = 0.0, Heatpump='//TRIM(AlphArray(1)))
       ErrorsFound = .true.
    END IF

    GSHP(GSHPNum)%HighPressCutOff        = NumArray(16)
    IF(NumArray(16)==0.0d0) THEN
       GSHP(GSHPNum)%HighPressCutOff        = 500000000.0d0
       !CALL ShowWarningError(ModuleCompName//': High Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
    END IF

    GSHP(GSHPNum)%LowPressCutOff        = NumArray(17)
    IF(NumArray(17)==0.0d0) THEN
       GSHP(GSHPNum)%LowPressCutOff        = 0.0d0
       !CALL ShowWarningError(ModuleCompName//': Low Pressure Cut Off= 0.0 Heat Pump'//TRIM(AlphArray(1)))
    END IF

    GSHP(GSHPNum)%SourceSideInletNodeNum   =   &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,'HeatPump:WaterToWater Cooling',AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    GSHP(GSHPNum)%SourceSideOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,'HeatPump:WaterToWater Cooling',AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

    GSHP(GSHPNum)%LoadSideInletNodeNum    =  &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,'HeatPump:WaterToWater Cooling',AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

    GSHP(GSHPNum)%LoadSideOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,'HeatPump:WaterToWater Cooling',AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

    ! Test node sets
    CALL TestCompSet(ModuleCompNameUC,AlphArray(1),AlphArray(2),AlphArray(3),'Condenser Water Nodes')
    CALL TestCompSet(ModuleCompNameUC,AlphArray(1),AlphArray(4),AlphArray(5),'Chilled Water Nodes')

    ! save the design source side flow rate for use by plant loop sizing algorithms
    CALL RegisterPlantCompDesignFlow(GSHP(GSHPNum)%SourceSideInletNodeNum,0.5d0*GSHP(GSHPNum)%SourceSideVolFlowRate)

    GshpReport(GSHPNum)%QLoad = 0.0d0
    GshpReport(GSHPNum)%QSource = 0.0d0
    GshpReport(GSHPNum)%Power = 0.0d0
    GshpReport(GSHPNum)%LoadSideWaterInletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSideWaterInletTemp = 0.0d0
    GshpReport(GSHPNum)%LoadSideWaterOutletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSideWaterOutletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSidemdot=0.0d0
    GshpReport(GSHPNum)%LoadSidemdot=0.0d0
    GSHP(GSHPNum)%isOn = .FALSE.
    GSHP(GSHPNum)%MustRun = .TRUE.

  END DO

  IF (ErrorsFound)THEN
    CALL ShowFatalError('Errors Found in getting Gshp input')
  END IF

  GSHPRefrigIndex=FindRefrigerant(GSHPRefrigerant)
  IF (GSHPRefrigIndex == 0) THEN
    CALL ShowFatalError('Refrigerant for HeatPump:WaterToWater Heating not found, should have been='//TRIM(GSHPRefrigerant))
  ENDIF

   !CurrentModuleObject='HeatPump:WaterToWater:ParameterEstimation:Cooling'
  DO GSHPNum = 1,NumGshps
   CALL SetupOutputVariable('Water to Water Heat Pump Electric Power [W]', &
        GshpReport(GSHPNum)%Power,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Electric Energy [J]', &
        GshpReport(GSHPNum)%Energy,'System','Sum',GSHP(GSHPNum)%Name,  &
        ResourceTypeKey='Electricity',EndUseKey='Cooling',GroupKey='Plant')

   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Rate [W]', &
        GshpReport(GSHPNum)%QLoad,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Energy [J]', &
        GshpReport(GSHPNum)%QLoadEnergy,'System','Sum',GSHP(GSHPNum)%Name)

   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Rate [W]', &
        GshpReport(GSHPNum)%QSource,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Energy [J]', &
        GshpReport(GSHPNum)%QSourceEnergy,'System','Sum',GSHP(GSHPNum)%Name)

   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Outlet Temperature [C]', &
        GshpReport(GSHPNum)%LoadSideWaterOutletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Inlet Temperature [C]', &
        GshpReport(GSHPNum)%LoadSideWaterInletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Outlet Temperature [C]', &
        GshpReport(GSHPNum)%SourceSideWaterOutletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Inlet Temperature [C]', &
        GshpReport(GSHPNum)%SourceSideWaterInletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Mass Flow Rate [kg/s]', &
        GshpReport(GSHPNum)%LoadSidemdot,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Mass Flow Rate [kg/s]', &
        GshpReport(GSHPNum)%SourceSidemdot,'System','Average',GSHP(GSHPNum)%Name)

    !scan for loop connection data
   errFlag=.false.
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 GSHP(GSHPNum)%WWHPPlantTypeOfNum, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum, &
                                 inletNodeNumber = GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                 errflag=errFlag)
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 GSHP(GSHPNum)%WWHPPlantTypeOfNum, &
                                 GSHP(GSHPNum)%LoadLoopNum, &
                                 GSHP(GSHPNum)%LoadLoopSideNum, &
                                 GSHP(GSHPNum)%LoadBranchNum, &
                                 GSHP(GSHPNum)%LoadCompNum, &
                                 inletNodeNumber = GSHP(GSHPNum)%LoadSideInletNodeNum,  &
                                 errflag=errFlag)


    IF (errFlag) THEN
      CALL ShowFatalError('GetWatertoWaterHPInput: Program terminated on scan for loop data')
    ENDIF

  END DO


RETURN
END SUBROUTINE GetGshpInput


SUBROUTINE InitGshp(GSHPNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    July 2007

            ! PURPOSE OF THIS SUBROUTINE:
            ! initialization


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataPlant, ONLY : TypeOf_HPWaterPECooling, ScanPlantLoopsForObject, PlantLoop
  USE FluidProperties, ONLY: GetDensityGlycol
  USE PlantUtilities,  ONLY: InitComponentNodes
  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: GSHPNum       ! GSHP number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:)  :: MyEnvrnFlag
  LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:)  :: MyPlanScanFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  REAL(r64)  :: rho ! local fluid density
    LOGICAL :: errFlag
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyPlanScanFlag(NumGSHPs))
    ALLOCATE(MyEnvrnFlag(NumGSHPs))
    MyOneTimeFlag = .false.
    MyEnvrnFlag  = .TRUE.
    MyPlanScanFlag = .TRUE.
  END IF

  IF (MyPlanScanFlag(GSHPNum)) THEN
    ! Locate the heating on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 TypeOf_HPWaterPECooling, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum, &
                                 InletNodeNumber = GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                 errFlag=errFlag)
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 TypeOf_HPWaterPECooling, &
                                 GSHP(GSHPNum)%LoadLoopNum, &
                                 GSHP(GSHPNum)%LoadLoopSideNum, &
                                 GSHP(GSHPNum)%LoadBranchNum, &
                                 GSHP(GSHPNum)%LoadCompNum, &
                                 InletNodeNumber = GSHP(GSHPNum)%LoadSideInletNodeNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitGshp: Program terminated due to previous condition(s).')
    ENDIF
    MyPlanScanFlag(GSHPNum)=.FALSE.
  ENDIF

  !For each new environment
  IF(BeginEnvrnFlag .AND. MyEnvrnFlag(GSHPNum))Then
    GshpReport(GSHPNum)%QLoad = 0.0d0
    GshpReport(GSHPNum)%QSource = 0.0d0
    GshpReport(GSHPNum)%Power = 0.0d0
    GshpReport(GSHPNum)%QLoadEnergy = 0.0d0
    GshpReport(GSHPNum)%QSourceEnergy = 0.0d0
    GshpReport(GSHPNum)%Energy = 0.0d0
    GshpReport(GSHPNum)%LoadSideWaterInletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSideWaterInletTemp = 0.0d0
    GshpReport(GSHPNum)%LoadSideWaterOutletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSideWaterOutletTemp = 0.0d0
    GshpReport(GSHPNum)%SourceSidemdot=0.0d0
    GshpReport(GSHPNum)%LoadSidemdot=0.0d0
    GSHP(GSHPNum)%isOn = .FALSE.
    GSHP(GSHPNum)%MustRun = .TRUE.

    MyEnvrnFlag(GSHPNum) = .FALSE.
    rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                         InitconvTemp, &
                         PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                         'InitGshp')
    GSHP(GSHPNum)%LoadSideDesignMassFlow   = GSHP(GSHPNum)%LoadSideVolFlowRate * rho

    CALL InitComponentNodes( 0.d0, GSHP(GSHPNum)%LoadSideDesignMassFlow, &
                                    GSHP(GSHPNum)%LoadSideInletNodeNum, &
                                    GSHP(GSHPNum)%LoadSideOutletNodeNum, &
                                    GSHP(GSHPNum)%LoadLoopNum, &
                                    GSHP(GSHPNum)%LoadLoopSideNum, &
                                    GSHP(GSHPNum)%LoadBranchNum, &
                                    GSHP(GSHPNum)%LoadCompNum)

    rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                         InitconvTemp, &
                         PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                         'InitGshp')
    GSHP(GSHPNum)%SourceSideDesignMassFlow = GSHP(GSHPNum)%SourceSideVolFlowRate * rho

    CALL InitComponentNodes( 0.d0,GSHP(GSHPNum)%SourceSideDesignMassFlow, &
                                 GSHP(GSHPNum)%SourceSideInletNodeNum, &
                                 GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum)


     Node(GSHP(GSHPNum)%SourceSideInletNodeNum)%Temp =  35.0d0
  END IF

  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(GSHPNum) = .TRUE.


! Init more variables

  !On every call
  GSHPReport(GSHPNum)%Running = 0

  GSHP(GSHPNum)%MustRun = .TRUE.        ! Reset MustRun Flag to TRUE

  LoadSideWaterMassFlowRate   = 0.0d0     ! Load Side mass flow rate, water side
  SourceSideWaterMassFlowRate = 0.0d0     ! Source Side mass flow rate, water side
  Power   = 0.0d0                         ! power consumption
  QLoad   = 0.0d0                         ! heat rejection from Load Side coil
  QSource = 0.0d0

RETURN
END SUBROUTINE InitGshp

SUBROUTINE CalcGshpModel(GSHPType,GSHPName,GSHPNum, MyLoad,FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       April 1999
          !                      September 2002, SJR
          !       RE-ENGINEERED  Mar2000

          ! PURPOSE OF THIS SUBROUTINE: This routine performs

          ! METHODOLOGY EMPLOYED: under development

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys ,SysTimeElapsed,FirstTimeStepSysFlag
  USE FluidProperties
  USE General,         ONLY : TrimSigDigits
  USE DataPlant,       ONLY : PlantLoop
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance
  USE PlantUtilities,  ONLY : SetComponentFlowRate

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: GshpType  ! type ofGSHP
  CHARACTER(len=*), INTENT(IN) :: GshpName  ! user specified name ofGSHP
  INTEGER, INTENT(IN)   :: GSHPNum                      ! GSHP Number
  REAL(r64)             :: MyLoad                       ! Operating Load
  LOGICAL, INTENT(IN)   :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER        :: gamma            = 1.114d0    ! Expnasion Coefficient
  REAL(r64), PARAMETER        :: HeatBalTol       = 0.0005d0
  REAL(r64), PARAMETER        :: RelaxParam       = 0.6d0
  REAL(r64), PARAMETER        :: SmallNum         = 1.0d-20
  INTEGER, PARAMETER          :: IterationLimit   = 500

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64)              :: SourceSideEffect         ! Source Side effectiveness
   REAL(r64)              :: LoadSideEffect           ! Load Side effectiveness
   REAL(r64)              :: SourceSideRefridgTemp    ! Source Side temperature
   REAL(r64)              :: LoadSideRefridgTemp      ! Load Side temperature
   REAL(r64)              :: SourceSideUA             ! Source Side heat transfer coefficient
   REAL(r64)              :: LoadSideUA               ! Load Side heat transfer coefficient
   REAL(r64)              :: SourceSidePressure       ! Source Side pressure
   REAL(r64)              :: LoadSidePressure         ! Load Side pressure
   REAL(r64)              :: SuctionPr                ! Suction Pressure
   REAL(r64)              :: DischargePr              ! Discharge Pressure
   REAL(r64)              :: CompressInletTemp        ! Compressor inlet temperature
   REAL(r64)              :: PressureDrop             ! Suction Pressure drop
   REAL(r64)              :: ClearanceFactor          ! Clearance factor
   REAL(r64)              :: PistonDisp               ! Compressor piston displacement
   REAL(r64)              :: ShTemp                   ! Superheat temperature
   REAL(r64)              :: LosFac                   ! Loss factor used to define the electromechanical loss for compressor
   REAL(r64)              :: MassRef                  ! mass flow rate of refrigerant
   REAL(r64)              :: SourceSideOutletEnth     ! Enthalpy at Source Side pressure
   REAL(r64)              :: LoadSideOutletEnth       ! Enthalpy at Condensor Pressure
   REAL(r64)              :: initialQSource           ! Guess Source Side Heat rate
   REAL(r64)              :: initialQLoad             ! Guess Load Side Heat rate
   REAL(r64)              :: qual                     ! quality
   REAL(r64)              :: COP
   REAL(r64)              :: SuperHeatEnth
   REAL(r64)              :: T110
   REAL(r64)              :: T111
   REAL(r64)              :: CompSuctionTemp
   REAL(r64)              :: CompSuctionEnth
   REAL(r64)              :: CompSuctionDensity
   REAL(r64)              :: PowerLosses
   REAL(r64)              :: CompSuctionSatTemp
   REAL(r64)              :: HighPressCutOff
   REAL(r64)              :: LowPressCutOff
   CHARACTER(len=25)      :: ErrString
   REAL(r64)              :: DutyFactor
   INTEGER                :: IterationCount

  REAL(r64), SAVE :: CurrentSimTime = 0.0d0
  REAL(r64), SAVE :: PrevSimTime = 0.0d0
  LOGICAL, SAVE          :: OneTimeFlag = .TRUE.
  ! Nodes
  INTEGER                :: SourceSideInletNode      ! Source Side inlet node number, water side
  INTEGER                :: SourceSideOutletNode     ! Source Side outlet node number, water side
  INTEGER                :: LoadSideInletNode        ! Load Side inlet node number, water side
  INTEGER                :: LoadSideOutletNode       ! Load Side outlet node number, water side
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64) :: CpSourceSide ! local temporary for fluid specific heat
  REAL(r64) :: CpLoadSide ! local temporary for fluid specific heat

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  PressureDrop      = GSHP(GSHPNum)%CompSucPressDrop
  ClearanceFactor   = GSHP(GSHPNum)%CompClearanceFactor
  PistonDisp        = GSHP(GSHPNum)%CompPistonDisp
  ShTemp            = GSHP(GSHPNum)%SuperheatTemp
  LosFac            = GSHP(GSHPNum)%LossFactor
  SourceSideUA      = GSHP(GSHPNum)%SourceSideUACoeff
  LoadSideUA        = GSHP(GSHPNum)%LoadSideUACoeff
  PowerLosses       = GSHP(GSHPNum)%PowerLosses
  COP               = GSHP(GSHPNum)%COP
  HighPressCutOff   = GSHP(GSHPNum)%HighPressCutOff
  LowPressCutOff    = GSHP(GSHPNum)%LowPressCutOff
  LoadSideInletNode    = GSHP(GSHPNum)%LoadSideInletNodeNum
  LoadSideOutletNode   = GSHP(GSHPNum)%LoadSideOutletNodeNum
  SourceSideInletNode  = GSHP(GSHPNum)%SourceSideInletNodeNum
  SourceSideOutletNode = GSHP(GSHPNum)%SourceSideOutletNodeNum
  LoopNum              = GSHP(GSHPNum)%LoadLoopNum
  LoopSideNum          = GSHP(GSHPNum)%LoadLoopSideNum

  IF(PrevSimTime .NE. CurrentSimTime)THEN
     PrevSimTime = CurrentSimTime
  END IF

  ! CALCULATE THE SIMULATION TIME
  CurrentSimTime = (dayofSim-1)*24 + hourofday-1 + (timestep-1)*timestepZone + SysTimeElapsed

  ! initialize event time array when the environment simulation begins
  IF(CurrentSimTime == 0.0d0 .AND. OneTimeFlag)THEN
    OneTimeFlag = .FALSE.
  END IF

  IF(CurrentSimTime > 0.0d0 ) OneTimeFlag = .TRUE.

  IF(MyLoad < 0.d0)THEN
    GSHP(GSHPNum)%MustRun = .TRUE.
    GSHP(GSHPNum)%IsOn = .TRUE.
  ELSE
    GSHP(GSHPNum)%MustRun = .FALSE.
    GSHP(GSHPNum)%IsOn = .FALSE.
  END IF

!*******Set flow based on "flowlock" and "run" flags**********
! Set flows if the heat pump is not running
  IF( .NOT. GSHP(GSHPNum)%MustRun )THEN
    LoadSideWaterMassFlowRate = 0.d0
    Call SetComponentFlowRate(LoadSideWaterMassFlowRate, &
          LoadSideInletNode, LoadSideOutletNode, &
          GSHP(GSHPNum)%LoadLoopNum, GSHP(GSHPNum)%LoadLoopSideNum, &
          GSHP(GSHPNum)%LoadBranchNum, GSHP(GSHPNum)%LoadCompNum)
    SourceSideWaterMassFlowRate = 0.d0
    Call SetComponentFlowRate(SourceSideWaterMassFlowRate, &
          SourceSideInletNode, SourceSideOutletNode, &
          GSHP(GSHPNum)%SourceLoopNum, GSHP(GSHPNum)%SourceLoopSideNum, &
          GSHP(GSHPNum)%SourceBranchNum, GSHP(GSHPNum)%SourceCompNum)
        !now initialize simulation variables for "heat pump off"
    QLoad   = 0.0d0
    QSource = 0.0d0
    Power   = 0.0d0
    LoadSideWaterInletTemp      = Node(LoadSideInletNode)%Temp
    LoadSideWaterOutletTemp     = LoadSideWaterInletTemp
    SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
    SourceSideWaterOutletTemp   = SourceSideWaterInletTemp
    RETURN !if heat pump is not running return without simulation

! Set flows if the heat pump is running
  ELSE ! the heat pump must run
    LoadSideWaterMassFlowRate = GSHP(GSHPNum)%LoadSideDesignMassFlow
    CALL SetComponentFlowRate(LoadSideWaterMassFlowRate, &
          LoadSideInletNode, LoadSideOutletNode, &
          GSHP(GSHPNum)%LoadLoopNum, GSHP(GSHPNum)%LoadLoopSideNum, &
          GSHP(GSHPNum)%LoadBranchNum, GSHP(GSHPNum)%LoadCompNum)

    SourceSideWaterMassFlowRate = GSHP(GSHPNum)%SourceSideDesignMassFlow
    CALL SetComponentFlowRate(SourceSideWaterMassFlowRate, &
          SourceSideInletNode, SourceSideOutletNode, &
          GSHP(GSHPNum)%SourceLoopNum, GSHP(GSHPNum)%SourceLoopSideNum, &
          GSHP(GSHPNum)%SourceBranchNum, GSHP(GSHPNum)%SourceCompNum)
    ! get inlet temps
    LoadSideWaterInletTemp      = Node(LoadSideInletNode)%Temp
    SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
    !if there's no flow, turn the "heat pump off"
    IF(LoadSideWaterMassFlowRate < MassFlowTolerance .OR. &
      SourceSideWaterMassFlowRate < MassFlowTolerance)THEN
        LoadSideWaterMassFlowRate = 0.d0
        Call SetComponentFlowRate(LoadSideWaterMassFlowRate, &
              LoadSideInletNode, LoadSideOutletNode, &
              GSHP(GSHPNum)%LoadLoopNum, GSHP(GSHPNum)%LoadLoopSideNum, &
              GSHP(GSHPNum)%LoadBranchNum, GSHP(GSHPNum)%LoadCompNum)
        SourceSideWaterMassFlowRate = 0.d0
        Call SetComponentFlowRate(SourceSideWaterMassFlowRate, &
              SourceSideInletNode, SourceSideOutletNode, &
              GSHP(GSHPNum)%SourceLoopNum, GSHP(GSHPNum)%SourceLoopSideNum, &
              GSHP(GSHPNum)%SourceBranchNum, GSHP(GSHPNum)%SourceCompNum)
        QLoad = 0.0d0
        QSource = 0.0d0
        Power = 0.0d0
        LoadSideWaterInletTemp      = Node(LoadSideInletNode)%Temp
        LoadSideWaterOutletTemp     = LoadSideWaterInletTemp
        SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
        SourceSideWaterOutletTemp   = SourceSideWaterInletTemp
      RETURN
    END IF
  END IF


!**********BEGIN THE CALCULATION**************

! initialize the source and load side heat transfer rates for the simulation
  initialQSource = 0.0d0
  initialQLoad   = 0.0d0
  IterationCount = 0

  CpSourceSide = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                                       SourceSideWaterInletTemp, &
                                       PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                                       'CalcGshpModel')

  CpLoadSide = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                                       LoadSideWaterInletTemp, &
                                       PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                                       'CalcGshpModel')


  !Determine effectiveness of Load Side
  LoadSideEffect   = 1.0d0 - EXP ( -LoadSideUA / (CpLoadSide * LoadSidewaterMassFlowRate))
  ! Determine effectiveness of Source Side
  SourceSideEffect = 1.0d0 - EXP( -SourceSideUA / (CpSourceSide * SourceSideWaterMassFlowRate ))

  ! main iteration loop to solve model equations
  LOOPSourceEnth: DO
    IterationCount = IterationCount +1

    ! To determine Load Side temperature
    LoadSideRefridgTemp = LoadSideWaterInletTemp - initialQLoad/ &
    (LoadSideEffect * CpLoadSide * LoadSideWaterMassFlowRate)

    ! Determine Source Side tempertaure
    SourceSideRefridgTemp = SourceSideWaterInletTemp + initialQSource/&
    (SourceSideEffect * CpSourceSide * SourceSideWaterMassFlowRate )

    ! Determine the evaporating and condensing pressures
    SourceSidePressure  = GetSatPressureRefrig(GSHPRefrigerant,SourceSideRefridgTemp,GSHPRefrigIndex,'CalcGSHPModel')
    LoadSidePressure  = GetSatPressureRefrig(GSHPRefrigerant,LoadSideRefridgTemp,GSHPRefrigIndex,'CalcGSHPModel')

    IF (SourceSidePressure < LowPressCutOff) THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Cooling Source Side Pressure Less than the Design Minimum')
      CALL ShowContinueError('Cooling Source Side Pressure='//TRIM(TrimSigDigits(SourceSidePressure,2))//  &
                             ' and user specified Design Minimum Pressure='//TRIM(TrimSigDigits(LowPressCutOff,2)))
      CALL ShowContinueErrorTimeStamp('  ')
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF

    IF (LoadSidePressure > HighPressCutOff)THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Cooling Load Side Pressure greater than the Design Maximum')
      CALL ShowContinueError('Cooling Load Side Pressure='//TRIM(TrimSigDigits(LoadSidePressure,2))//  &
                             ' and user specified Design Maximum Pressure='//TRIM(TrimSigDigits(HighPressCutOff,2)))
      CALL ShowContinueErrorTimeStamp('  ')
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF
    ! Determine Suction Pressure at compressor inlet
    SuctionPr = LoadSidePressure - PressureDrop
    ! Determine Discharge Pressure at compressor exit
    DischargePr = SourceSidePressure + PressureDrop

    IF (SuctionPr < LowPressCutOff) THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Cooling Suction Pressure Less than the Design Minimum')
      CALL ShowContinueError('Cooling Suction Pressure='//TRIM(TrimSigDigits(SuctionPr,2))//  &
                             ' and user specified Design Minimum Pressure='//TRIM(TrimSigDigits(LowPressCutOff,2)))
      CALL ShowContinueErrorTimeStamp('  ')
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF

    IF (DischargePr > HighPressCutOff)THEN
      CALL ShowSevereError(ModuleCompName//'="'//trim(GSHPName)//'" Cooling Discharge Pressure greater than the Design Maximum')
      CALL ShowContinueError('Cooling Discharge Pressure='//TRIM(TrimSigDigits(DischargePr,2))//  &
                             ' and user specified Design Maximum Pressure='//TRIM(TrimSigDigits(HighPressCutOff,2)))
      CALL ShowContinueErrorTimeStamp('  ')
      CALL ShowFatalError('Preceding Conditions cause termination.')
    END IF

    ! Determine the Source Side Outlet Enthalpy

    qual= 1.0d0
    LoadSideOutletEnth = GetSatEnthalpyRefrig(GSHPRefrigerant,LoadSideRefridgTemp,qual,  &
       GSHPRefrigIndex,'CalcGSHPModel:LoadSideRefridgTemp')

    qual=0.0d0
    SourceSideOutletEnth = GetSatEnthalpyRefrig(GSHPRefrigerant, SourceSideRefridgTemp, qual,   &
       GSHPRefrigIndex,'CalcGSHPModel:SourceSideRefridgTemp')

    ! Determine Load Side Outlet Enthalpy
    ! Determine superheated temperature of the LoadSide outlet/compressor inlet
    CompressInletTemp = LoadSideRefridgTemp + ShTemp

    ! Determine the enathalpy of the super heated fluid at Source Side outlet
    SuperHeatEnth = GetSupHeatEnthalpyRefrig(GSHPRefrigerant,CompressInletTemp,LoadSidePressure,  &
       GSHPRefrigIndex,'CalcGSHPModel:CompressInletTemp')

    ! Determining the suction state of the fluid from inlet state involves interation
    ! Method employed...
    ! Determine the saturated temp at suction pressure, shoot out into the superheated region find the enthalpy
    ! check that with the inlet enthalpy ( as suction loss is isenthalpic). Iterate till desired accuracy is reached

    !this routine was reenginerred from HVACsim + takes pressure in Pascals, tolrance, refrgerant # R22 =6
    CompSuctionSatTemp = GetSatTemperatureRefrig(GSHPRefrigerant,SuctionPr,GSHPRefrigIndex,'CalcGSHPModel:SuctionPr')

    T110 = CompSuctionSatTemp
    !            Shoot into the super heated region
    T111 = CompSuctionSatTemp + 100.d0
    ! Iterate to find the Suction State
    LOOP: DO
      CompSuctionTemp = 0.5d0 * ( T110 + T111 )

      CompSuctionEnth = GetSupHeatEnthalpyRefrig(GSHPRefrigerant,CompSuctionTemp,SuctionPr,  &
         GSHPRefrigIndex,'CalcGSHPModel:CompSuctionTemp')

      IF (ABS(CompsuctionEnth-SuperHeatEnth)/SuperHeatEnth < .0001d0)  THEN
        EXIT LOOP
      END IF

      IF ( CompsuctionEnth < SuperHeatEnth ) THEN
        T110 = CompSuctionTemp
      ELSE
        T111 = CompSuctionTemp
      END IF
    END DO LOOP

    ! Determine the Mass flow rate of refrigerant
    CompSuctionDensity = GetSupHeatDensityRefrig(GSHPRefrigerant, CompSuctionTemp, SuctionPr,   &
          GSHPRefrigIndex ,'CalcGSHPModel:CompSuctionTemp')
    MassRef = PistonDisp * CompSuctionDensity * (1+ClearanceFactor-ClearanceFactor*((DischargePr/SuctionPr)**(1/gamma)))

    ! Find the Source Side Heat Transfer

    QLoad = MassRef * ( LoadSideOutletEnth - SourceSideOutletEnth )

    ! Determine the theoretical power
    Power = PowerLosses+(MassRef*gamma/(gamma-1) * SuctionPr /CompSuctionDensity/LosFac * &
    ((DischargePr/SuctionPr)**((gamma-1)/gamma) - 1))

    ! Determine the Loadside HeatRate (QLoad)
    QSource = Power + QLoad
    ! convergence and iteration limit check
    IF(ABS((QSource - initialQSource)/(initialQSource+SmallNum)) < HeatBalTol .OR. IterationCount>IterationLimit) THEN
      IF(IterationCount>IterationLimit)then
        CALL ShowWarningError('HeatPump:WaterToWater:ParameterEstimation, Cooling did not converge')
        CALL ShowContinueErrorTimeStamp('  ')
        CALL ShowContinueError('Heatpump Name = '//TRIM(GSHP(GSHPNum)%Name))
        WRITE(ErrString,*) ABS(100.0d0*(QSource - initialQSource)/(initialQSource+SmallNum))
        CALL ShowContinueError('Heat Inbalance (%)             = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) QLoad
        CALL ShowContinueError('Load-side heat transfer rate   = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) Qsource
        CALL ShowContinueError('Source-side heat transfer rate = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) SourceSideWaterMassFlowRate
        CALL ShowContinueError('Source-side mass flow rate     = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) LoadSideWaterMassFlowRate
        CALL ShowContinueError('Load-side mass flow rate       = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) SourceSideWaterInletTemp
        CALL ShowContinueError('Source-side inlet temperature  = '//TRIM(ADJUSTL(ErrString)))
        WRITE(ErrString,*) LoadSideWaterInletTemp
        CALL ShowContinueError('Load-side inlet temperature    = '//TRIM(ADJUSTL(ErrString)))

      END IF
      EXIT LOOPSourceEnth

    ELSE ! update load
      initialQSource = initialQSource+ RelaxParam*(QSource-initialQSource)
      initialQLoad = initialQLoad + RelaxParam*(QLoad - initialQLoad)

    END IF

  END DO LOOPSourceEnth

  !Control Strategy
  IF(ABS(MyLoad) < QLoad) THEN
    DutyFactor = ABS(MyLoad)/QLoad
    QLoad      = ABS(MyLoad)
    Power      = DutyFactor * Power
    QSource    = QSource * DutyFactor
    ! Determine the Exterior fluid temperature at the Load Side oulet and eveporator outlet...
    LoadSideWaterOutletTemp   = LoadSideWaterInletTemp - QLoad/& ! Chilled water
                                (LoadSideWaterMassFlowRate * CpLoadSide)
    SourceSideWaterOutletTemp = SourceSideWaterInletTemp + QSource/ & ! cooling water
                                (SourceSideWaterMassFlowRate  * CpSourceSide)
    RETURN
  END IF

  LoadSideWaterOutletTemp   = LoadSideWaterInletTemp - QLoad/& ! Chilled water
                              (LoadSideWaterMassFlowRate * CpLoadSide)
  SourceSideWaterOutletTemp = SourceSideWaterInletTemp + QSource/&
                              (SourceSideWaterMassFlowRate  * CpSourceSide)
  GSHPReport(GSHPNum)%Running = 1

 RETURN

END SUBROUTINE CalcGshpModel

SUBROUTINE UpdateGSHPRecords(GSHPNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: GSHPNum       ! GSHP number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: SourceSideInletNode      ! Source Side inlet node number, water side
  INTEGER     :: SourceSideOutletNode     ! Source Side outlet node number, water side
  INTEGER     :: LoadSideInletNode        ! Load Side inlet node number, water side
  INTEGER     :: LoadSideOutletNode       ! Load Side outlet node number, water side
  REAL(r64) :: ReportingConstant

    LoadSideInletNode    = GSHP(GSHPNum)%LoadSideInletNodeNum
    LoadSideOutletNode   = GSHP(GSHPNum)%LoadSideOutletNodeNum
    SourceSideInletNode  = GSHP(GSHPNum)%SourceSideInletNodeNum
    SourceSideOutletNode = GSHP(GSHPNum)%SourceSideOutletNodeNum

  IF (.NOT. GSHP(GSHPNum)%MustRun )THEN
          !set node temperatures
    Node(SourceSideOutletNode)%Temp     = Node(SourceSideInletNode)%Temp
    Node(LoadSideOutletNode)%Temp       = Node(LoadSideInletNode)%Temp
    GSHPReport(GSHPNum)%Power                = 0.0d0
    GSHPReport(GSHPNum)%Energy               = 0.0d0
    GSHPReport(GSHPNum)%QSource              = 0.0d0
    GSHPReport(GSHPNum)%QLoad                = 0.0d0
    GSHPReport(GSHPNum)%QSourceEnergy        = 0.0d0
    GSHPReport(GSHPNum)%QLoadEnergy          = 0.0d0
    GSHPReport(GSHPNum)%SourceSidemdot       = SourceSideWaterMassFlowRate
    GSHPReport(GSHPNum)%LoadSidemdot         = LoadSideWaterMassFlowRate
    GSHPReport(GSHPNum)%SourceSideWaterInletTemp  = Node(SourceSideInletNode)%Temp
    GSHPReport(GSHPNum)%SourceSideWaterOutletTemp = Node(SourceSideOutletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterInletTemp    = Node(LoadSideInletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterOutletTemp   = Node(LoadSideOutletNode)%Temp

  ELSE
          !set node temperatures
    Node(LoadSideOutletNode)%Temp      = LoadSideWaterOutletTemp
    Node(SourceSideOutletNode)%Temp    = SourceSideWaterOutletTemp

      !set node flow rates;  for these load based models
          !assume that the sufficient Source Side flow rate available

    ReportingConstant = TimeStepSys*SecInHour

    GSHPReport(GSHPNum)%Power       = Power
    GSHPReport(GSHPNum)%QSource     = QSource
    GSHPReport(GSHPNum)%QLoad       = QLoad
    GSHPReport(GSHPNum)%Energy      = Power*ReportingConstant
    GSHPReport(GSHPNum)%QSourceEnergy = QSource*ReportingConstant
    GSHPReport(GSHPNum)%QLoadEnergy   = QLoad*ReportingConstant
    GSHPReport(GSHPNum)%SourceSidemdot       = SourceSideWaterMassFlowRate
    GSHPReport(GSHPNum)%LoadSidemdot         = LoadSideWaterMassFlowRate
    GSHPReport(GSHPNum)%SourceSideWaterInletTemp    = Node(SourceSideInletNode)%Temp
    GSHPReport(GSHPNum)%SourceSideWaterOutletTemp   = Node(SourceSideOutletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterInletTemp  = Node(LoadSideInletNode)%Temp
    GSHPReport(GSHPNum)%LoadSideWaterOutletTemp   = Node(LoadSideOutletNode)%Temp

  END IF
!    Node(SourceSideInletNode)%MassFlowRate = SourceSideWaterMassFlowRate
!    Node(SourceSideOutletNode)%MassFlowRate = SourceSideWaterMassFlowRate
!
!    Node(LoadSideInletNode)%MassFlowRate = LoadSideWaterMassFlowRate
!    Node(LoadSideOutletNode)%MassFlowRate = LoadSideWaterMassFlowRate
!
!    Node(SourceSideOutletNode)%MassFlowRateMaxAvail = Node(SourceSideInletNode)%MassFlowRateMaxAvail
!    Node(SourceSideOutletNode)%MassFlowRateMinAvail = Node(SourceSideInletNode)%MassFlowRateMinAvail
!    Node(LoadSideOutletNode)%MassFlowRateMaxAvail = Node(LoadSideInletNode)%MassFlowRateMaxAvail
!    Node(LoadSideOutletNode)%MassFlowRateMinAvail = Node(LoadSideInletNode)%MassFlowRateMinAvail
RETURN
END SUBROUTINE UpdateGSHPRecords

END MODULE HeatPumpWaterToWaterCOOLING
!************************************************************************************
!
!==================================== MODULE HeatPumpWaterToWaterSimple ======================
!
!************************************************************************************
MODULE HeatPumpWaterToWaterSimple

  ! MODULE INFORMATION:
  !       AUTHOR         Kenneth Tang
  !       DATE WRITTEN   March 2005
  !       MODIFIED       Brent Griffith, plant upgrades, fluid properties
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module simulates a Water-to-Water Heat Pump Simple (Equation-Fit Model)

  ! METHODOLOGY EMPLOYED:
  ! This simulation is based on a set of coefficients generated from
  ! the manufacturer catalog data using the generalized least square method

  ! REFERENCES:
  ! (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
  ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
  ! Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
  ! (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
  ! Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
  ! M.S. Thesis, Department of Mechanical and Aerospace Engineering,
  ! Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)


  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BeginSimFlag,InitconvTemp,BeginEnvrnFlag, HourOfDay, KelvinConv,  &
                              TimeStep,TimeStepZone,DayOfSim,WarmupFlag,SecInHour
USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, ShowContinueError, SetupOutputVariable, &
                              ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
USE General,         ONLY : TrimSigDigits
USE DataLoopNode

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: HPEqFitHeating='HeatPump:WatertoWater:EquationFit:Heating'
CHARACTER(len=*), PARAMETER :: HPEqFitHeatingUC='HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING'
CHARACTER(len=*), PARAMETER :: HPEqFitCooling='HeatPump:WatertoWater:EquationFit:Cooling'
CHARACTER(len=*), PARAMETER :: HPEqFitCoolingUC='HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING'

  ! DERIVED TYPE DEFINITIONS

  ! Type Description of Heat Pump
TYPE GshpSpecs
  CHARACTER(len=MaxNameLength) :: Name               = ' ' ! user identifier
  CHARACTER(len=MaxNameLength) :: WatertoWaterHPType = ' ' ! Type of WatertoAirHP ie. Heating or Cooling
  INTEGER      :: WWHPPlantTypeOfNum  = 0       ! equipment type num
  LOGICAL      :: Available           = .false. ! need an array of logicals--load identifiers of available equipment
  LOGICAL      :: ON                  = .false. ! simulate the machine at it's operating part load ratio
  LOGICAL      :: IsOn                = .false. ! flag that the heat pump is ON during current time step
  LOGICAL      :: MustRun             = .false. ! flag that the heat pump is MUST RUN during current time step
  REAL(r64)    :: SourceSideDesignMassFlow = 0.d0 ! Design flow rate (kg/s)
  REAL(r64)    :: LoadSideDesignMassFlow  = 0.d0 ! Design flow rate (kg/s)
  REAL(r64)    :: RatedLoadVolFlowCool    = 0.0d0 ! Rated Cooling Load Side Volumetric Flow Rate [m3/s]
  REAL(r64)    :: RatedSourceVolFlowCool  = 0.0d0 ! Rated Cooling Source Side Volumetric Flow Rate [m3/s]
  REAL(r64)    :: RatedCapCool            = 0.0d0 ! Rated Cooling Capacity [W]
  REAL(r64)    :: RatedPowerCool          = 0.0d0 ! Rated Cooling Power Consumption[W]
  REAL(r64)    :: CoolCap1                = 0.0d0 ! 1st coefficient of the Cooling capacity performance curve
  REAL(r64)    :: CoolCap2                = 0.0d0 ! 2nd coefficient of the Cooling capacity performance curve
  REAL(r64)    :: CoolCap3                = 0.0d0 ! 3rd coefficient of the Cooling capacity performance curve
  REAL(r64)    :: CoolCap4                = 0.0d0 ! 4th coefficient of the Cooling capacity performance curve
  REAL(r64)    :: CoolCap5                = 0.0d0 ! 5th coefficient of the Cooling capacity performance curve
  REAL(r64)    :: CoolPower1              = 0.0d0 ! 1st coefficient of the Cooling power consumption curve
  REAL(r64)    :: CoolPower2              = 0.0d0 ! 2nd coefficient of the Cooling power consumption curve
  REAL(r64)    :: CoolPower3              = 0.0d0 ! 3rd coefficient of the Cooling power consumption curve
  REAL(r64)    :: CoolPower4              = 0.0d0 ! 4th coefficient of the Cooling power consumption curve
  REAL(r64)    :: CoolPower5              = 0.0d0 ! 5th coefficient of the Cooling power consumption curve
  INTEGER      :: CoolCapNegativeCounter  = 0   ! Counter for number of times cooling capacity curve is <= 0.0
  INTEGER      :: CoolCapNegativeIndex    = 0   ! Index for recurring warning message regarding cooling capacity curve is <= 0.0
  INTEGER      :: CoolPowerNegativeCounter  = 0 ! Counter for number of times cooling power curve is <= 0.0
  INTEGER      :: CoolPowerNegativeIndex  = 0   ! Index for recurring warning message regarding cooling power curve is <= 0.0

  REAL(r64)    :: RatedLoadVolFlowHeat    = 0.0d0 ! Rated Heating Load Side Volumetric Flow Rate [m3/s]
  REAL(r64)    :: RatedSourceVolFlowHeat  = 0.0d0 ! Rated Heating Source Side Volumetric Flow Rate [m3/s]
  REAL(r64)    :: RatedCapHeat            = 0.0d0 ! Rated Heating Capacity [W]
  REAL(r64)    :: RatedPowerHeat          = 0.0d0 ! Rated Heating Compressor Power[W]
  REAL(r64)    :: HeatCap1                = 0.0d0 ! 1st coefficient of the Heating capacity performance curve
  REAL(r64)    :: HeatCap2                = 0.0d0 ! 2nd coefficient of the Heating capacity performance curve
  REAL(r64)    :: HeatCap3                = 0.0d0 ! 3rd coefficient of the Heating capacity performance curve
  REAL(r64)    :: HeatCap4                = 0.0d0 ! 4th coefficient of the Heating capacity performance curve
  REAL(r64)    :: HeatCap5                = 0.0d0 ! 5th coefficient of the Heating capacity performance curve
  REAL(r64)    :: HeatPower1              = 0.0d0 ! 1st coefficient of the Heating power consumption curve
  REAL(r64)    :: HeatPower2              = 0.0d0 ! 2nd coefficient of the Heating power consumption curve
  REAL(r64)    :: HeatPower3              = 0.0d0 ! 3rd coefficient of the Heating power consumption curve
  REAL(r64)    :: HeatPower4              = 0.0d0 ! 4th coefficient of the Heating power consumption curve
  REAL(r64)    :: HeatPower5              = 0.0d0 ! 5th coefficient of the Heating power consumption curve
  INTEGER      :: LoadSideInletNodeNum    = 0   ! Load Side Inlet Node
  INTEGER      :: LoadSideOutletNodeNum   = 0   ! Load Side Outlet Node
  INTEGER      :: SourceSideInletNodeNum  = 0   ! Source Side Inlet Node
  INTEGER      :: SourceSideOutletNodeNum = 0   ! Source Side Outlet Node
  INTEGER      :: HeatCapNegativeCounter  = 0   ! Counter for number of times heating capacity curve is <= 0.0
  INTEGER      :: HeatCapNegativeIndex    = 0   ! Index for recurring warning message regarding heating capacity curve is <= 0.0
  INTEGER      :: HeatPowerNegativeCounter  = 0 ! Counter for number of times heating power curve is <= 0.0
  INTEGER      :: HeatPowerNegativeIndex  = 0   ! Index for recurring warning message regarding heating power curve is <= 0.0
  !loop topology variables
  INTEGER      :: SourceLoopNum           = 0 ! source side plant loop index number
  INTEGER      :: SourceLoopSideNum       = 0 ! source side plant loop side index
  INTEGER      :: SourceBranchNum         = 0 ! source side plant loop branch index
  INTEGER      :: SourceCompNum           = 0 ! source side plant loop component index
  INTEGER      :: LoadLoopNum             = 0 ! load side plant loop index number
  INTEGER      :: LoadLoopSideNum         = 0 ! load side plant loop side index
  INTEGER      :: LoadBranchNum           = 0 ! load side plant loop branch index
  INTEGER      :: LoadCompNum             = 0 ! load side plant loop component index
END TYPE GshpSpecs

    ! Output Variables Type definition
TYPE ReportVars
  REAL(r64)    :: Power                   = 0.0d0 ! Power Consumption [W]
  REAL(r64)    :: Energy                  = 0.0d0 ! Energy Consumption [J]
  REAL(r64)    :: QLoad                   = 0.0d0 ! Load Side Heat Transfer Rate [W]
  REAL(r64)    :: QLoadEnergy             = 0.0d0 ! Load Side Heat Transfer [J]
  REAL(r64)    :: QSource                 = 0.0d0 ! Source Side Heat Transfer Rate [W]
  REAL(r64)    :: QSourceEnergy           = 0.0d0 ! Source Side Heat Transfer [J]
  REAL(r64)    :: LoadSideMassFlowRate    = 0.0d0 ! Load side volumetric flow rate m3/s
  REAL(r64)    :: LoadSideInletTemp       = 0.0d0 ! Load Side outlet temperature °C
  REAL(r64)    :: LoadSideOutletTemp      = 0.0d0 ! Load Side outlet temperature °C
  REAL(r64)    :: SourceSideMassFlowRate  = 0.0d0 ! Source side volumetric flow rate m3/s
  REAL(r64)    :: SourceSideInletTemp     = 0.0d0 ! Source Side outlet temperature °C
  REAL(r64)    :: SourceSideOutletTemp    = 0.0d0 ! Source Side outlet temperature °C
END TYPE ReportVars

TYPE (GSHPSpecs), ALLOCATABLE, DIMENSION(:)  ::GSHP
TYPE (ReportVars), ALLOCATABLE, DIMENSION(:) :: GSHPReport

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER  :: NumGSHPs                      = 0   ! Number of GSHPs specified in input

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

          ! Driver/Manager Routines
PUBLIC SimHPWatertoWaterSimple

          ! Get Input routines for module
PRIVATE GetWatertoWaterHPInput

          ! Initialization routines for module
PRIVATE InitWatertoWaterHP

          ! Computational routines
PRIVATE CalcWatertoWaterHPCooling
PRIVATE CalcWatertoWaterHPHeating

          ! Update routine to check convergence and update nodes
PRIVATE UpdateGSHPRecords

          ! Other routines

CONTAINS

         ! MODULE SUBROUTINES:

SUBROUTINE SimHPWatertoWaterSimple(GSHPType, GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, &
                                    InitLoopEquip, MyLoad, MaxCap, MinCap, OptCap, LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Water-to-Water Heat Pump Simple (Equation-Fit Model)

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
   USE PlantUtilities, ONLY:UpdateChillerComponentCondenserSide
  USE DataEnvironment
  USE General, ONLY: TrimSigDigits
  USE DataPlant,  ONLY: TypeOf_HPWaterEFCooling, TypeOf_HPWaterEFHeating

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: GSHPType         ! Type of GSHP
  INTEGER         , INTENT(IN)  :: GSHPTypeNum      ! Type of GSHP in Plant equipment
  INTEGER         , INTENT(IN)  :: LoopNum          ! The calling loop number
  CHARACTER(len=*), INTENT(IN)  :: GSHPName         ! User Specified Name of GSHP
  INTEGER, INTENT(INOUT)        :: GSHPNum          ! Index of Equipment
  LOGICAL, INTENT(IN)           :: FirstHVACIteration
  LOGICAL, INTENT(INOUT)        :: InitLoopEquip    ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(IN)              :: MyLoad           ! Loop demand component will meet
  REAL(r64), INTENT(OUT)             :: MinCap           ! Minimum operating capacity of GSHP [W]
  REAL(r64), INTENT(OUT)             :: MaxCap           ! Maximum operating capacity of GSHP [W]
  REAL(r64), INTENT(OUT)             :: OptCap           ! Optimal operating capacity of GSHP [W]


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE     :: GetInputFlag = .TRUE.    ! then TRUE, calls subroutine to read input file.

  !Get input from IDF
  IF (GetInputFlag) THEN
    CALL GetWatertoWaterHPInput
    GetInputFlag = .FALSE.
  END IF

  IF (InitLoopEquip) THEN
     GSHPNum = FindItemInList( GSHPName, GSHP%Name, NumGSHPs )
     IF (GSHPNum /= 0) THEN  ! if 0, fall through to next
       SELECT CASE (GSHPTypeNum)
       CASE (TypeOf_HPWaterEFCooling)
            MinCap = 0.0d0
            MaxCap = GSHP(GSHPNum)%RatedCapCool
            OptCap = GSHP(GSHPNum)%RatedCapCool
       CASE (TypeOf_HPWaterEFHeating)
            MinCap = 0.0d0
            MaxCap = GSHP(GSHPNum)%RatedCapHeat
            OptCap = GSHP(GSHPNum)%RatedCapHeat
       CASE DEFAULT
            CALL ShowFatalError('SimHPWatertoWaterSimple: Module called with incorrect GSHPType='//TRIM(GSHPTYpe))
       END SELECT
       Return
     ENDIF
  END IF

  ! Calculate Demand on heat pump
  TypeOfEquip: SELECT CASE (GSHPTypeNum)
    CASE (TypeOf_HPWaterEFCooling)
      IF (GSHPNum /= 0) THEN
        IF (LoopNum == GSHP(GSHPNum)%LoadLoopNum) THEN ! chilled water loop

          CALL InitWatertoWaterHP(GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad)
          CALL CalcWatertoWaterHPCooling(GSHPNum, MyLoad)
          CALL UpdateGshpRecords(GSHPNum)

        ELSEIF (LoopNum == GSHP(GSHPNum)%SourceLoopNum) THEN ! condenser loop
          CALL UpdateChillerComponentCondenserSide(GSHP(GSHPNum)%SourceLoopNum, &
                                     GSHP(GSHPNum)%SourceLoopSideNum,     &
                                     TypeOf_HPWaterEFCooling,                     &
                                     GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                     GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                     GSHPReport(GSHPNum)%QSource,             &
                                     GSHPReport(GSHPNum)%SourceSideInletTemp,     &
                                     GSHPReport(GSHPNum)%SourceSideOutletTemp,    &
                                     GSHPReport(GSHPNum)%SourceSideMassFlowRate,          &
                                     FirstHVACIteration)
        ELSE
          CALL ShowFatalError ('SimHPWatertoWaterSimple:: Invalid loop connection '//  &
             HPEqFitCooling//', Requested Unit='//TRIM(GSHPName))
        ENDIF
      ELSE
        CALL ShowFatalError ('SimHPWatertoWaterSimple:: Invalid '//HPEqFitCooling//  &
           ', Requested Unit='//TRIM(GSHPName))
      ENDIF
    CASE (TypeOf_HPWaterEFHeating)
      IF (GSHPNum /= 0) THEN
        IF (LoopNum == GSHP(GSHPNum)%LoadLoopNum) THEN ! chilled water loop

          CALL InitWatertoWaterHP(GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad)
          CALL CalcWatertoWaterHPHeating(GSHPNum, MyLoad)
          CALL UpdateGshpRecords(GSHPNum)
        ELSEIF (LoopNum == GSHP(GSHPNum)%SourceLoopNum) THEN ! condenser loop
          CALL UpdateChillerComponentCondenserSide(GSHP(GSHPNum)%SourceLoopNum, &
                                     GSHP(GSHPNum)%SourceLoopSideNum,     &
                                     TypeOf_HPWaterEFHeating,                     &
                                     GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                     GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                     - GSHPReport(GSHPNum)%QSource,             &
                                     GSHPReport(GSHPNum)%SourceSideInletTemp,     &
                                     GSHPReport(GSHPNum)%SourceSideOutletTemp,    &
                                     GSHPReport(GSHPNum)%SourceSideMassFlowRate,          &
                                     FirstHVACIteration)
        ELSE
          CALL ShowFatalError ('SimHPWatertoWaterSimple:: Invalid loop connection '//  &
             HPEqFitCooling//', Requested Unit='//TRIM(GSHPName))
        ENDIF
      ELSE
        CALL ShowFatalError ('SimHPWatertoWaterSimple:: Invalid '//HPEqFitHeating//  &
           ', Requested Unit='//TRIM(GSHPName))
      ENDIF
    CASE DEFAULT
      CALL ShowFatalError('SimHPWatertoWaterSimple: Module called with incorrect GSHPType='//TRIM(GSHPTYpe))
    END SELECT TypeOfEquip


RETURN
END SUBROUTINE SimHPWatertoWaterSimple

SUBROUTINE GetWatertoWaterHPInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtain input from IDF and store them in data structures

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE NodeInputManager,      ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY : TestCompSet
  USE PlantUtilities,        ONLY : RegisterPlantCompDesignFlow
  USE DataPlant,  ONLY: TypeOf_HPWaterEFCooling, TypeOf_HPWaterEFHeating,ScanPlantLoopsForObject

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                     :: GSHPNum                   ! GSHP number
  INTEGER                     :: HPNum                     ! Counter
  INTEGER                     :: NumCoolCoil               ! Number of Cooling Coils
  INTEGER                     :: NumHeatCoil               ! Number of Heating Coils
  INTEGER                     :: NumAlphas                 ! Number of elements in the alpha array
  INTEGER                     :: NumNums                   ! Number of elements in the numeric array
  INTEGER                     :: IOStat                    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(5)   :: AlphArray ! character string data
  REAL(r64),                   DIMENSION(15)  :: NumArray  ! numeric data

  LOGICAL, SAVE :: ErrorsFound = .false.
  LOGICAL       :: IsNotOk          ! Flag to verify name
  LOGICAL       :: IsBlank          ! Flag for blank name
  LOGICAL       :: errFlag

  NumCoolCoil = GetNumObjectsFound(HPEqFitCoolingUC)
  NumHeatCoil = GetNumObjectsFound(HPEqFitHeatingUC)
  NumGSHPs = NumCoolCoil + NumHeatCoil

  IF(NumGSHPs <= 0) THEN
    CALL ShowSevereError('GetEquationFitWaterToWater Input: No Equipment found')
    ErrorsFound=.true.
  END IF

  IF(NumGSHPs > 0) THEN
    ALLOCATE (GSHP(NumGSHPs))
    ALLOCATE (GSHPReport(NumGSHPs))
    ! initialize the data structures
  END IF

  !Load data structure for cooling coil
  DO HPNum = 1, NumCoolCoil

        GSHPNum = HPNum

        CALL GetObjectItem(HPEqFitCoolingUC,HPNum,AlphArray,NumAlphas, &
                   NumArray,NumNums,IOSTAT)
        IsNotOk=.false.
        IsBlank=.true.
        CALL VerifyName(AlphArray(1),GSHP%Name,HPNum-1, ISNotOK,ISBlank,'GHSP Name')

        IF (ISNotOK) THEN
            ErrorsFound=.true.
            IF(ISBlank) AlphArray(1)='xxxxx'
        END IF
        GSHP(GSHPNum)%WWHPPlantTypeOfNum    = TypeOf_HPWaterEFCooling
        GSHP(GSHPNum)%Name                  = AlphArray(1)
        GSHP(GSHPNum)%RatedLoadVolFlowCool  = NumArray(1)
        GSHP(GSHPNum)%RatedSourceVolFlowCool= NumArray(2)
        GSHP(GSHPNum)%RatedCapCool          = NumArray(3)
        GSHP(GSHPNum)%RatedPowerCool        = NumArray(4)
        GSHP(GSHPNum)%CoolCap1              = NumArray(5)
        GSHP(GSHPNum)%CoolCap2              = NumArray(6)
        GSHP(GSHPNum)%CoolCap3              = NumArray(7)
        GSHP(GSHPNum)%CoolCap4              = NumArray(8)
        GSHP(GSHPNum)%CoolCap5              = NumArray(9)
        GSHP(GSHPNum)%CoolPower1            = NumArray(10)
        GSHP(GSHPNum)%CoolPower2            = NumArray(11)
        GSHP(GSHPNum)%CoolPower3            = NumArray(12)
        GSHP(GSHPNum)%CoolPower4            = NumArray(13)
        GSHP(GSHPNum)%CoolPower5            = NumArray(14)

        GSHP(GSHPNum)%SourceSideInletNodeNum   =   &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,HPEqFitCoolingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

        GSHP(GSHPNum)%SourceSideOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,HPEqFitCoolingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

        GSHP(GSHPNum)%LoadSideInletNodeNum    =  &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,HPEqFitCoolingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

        GSHP(GSHPNum)%LoadSideOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,HPEqFitCoolingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

        ! Test node sets
        CALL TestCompSet(HPEqFitCoolingUC,AlphArray(1),AlphArray(2),AlphArray(3),'Condenser Water Nodes')
        CALL TestCompSet(HPEqFitCoolingUC,AlphArray(1),AlphArray(4),AlphArray(5),'Hot Water Nodes')

        ! save the design source side flow rate for use by plant loop sizing algorithms
        CALL RegisterPlantCompDesignFlow(GSHP(GSHPNum)%SourceSideInletNodeNum,0.5d0*GSHP(GSHPNum)%RatedSourceVolFlowCool)

        ! CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Cooling'
        CALL SetupOutputVariable('Water to Water Heat Pump Electric Energy [J]', &
             GSHPReport(GSHPNum)%Energy,'System','Sum',GSHP(GSHPNum)%Name,  &
             ResourceTypeKey='Electricity',EndUseKey='Cooling',GroupKey='Plant')
        CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Energy [J]', &
             GSHPReport(GSHPNum)%QLoadEnergy,'System','Sum',GSHP(GSHPNum)%Name)
        CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Energy [J]', &
             GSHPReport(GSHPNum)%QSourceEnergy,'System','Sum',GSHP(GSHPNum)%Name)
  END DO

  !Load data structure for heating coil
  DO HPNum = 1, NumHeatCoil

        GSHPNum = NumCoolCoil + HPNum

        CALL GetObjectItem(HPEqFitHeatingUC,HPNum,AlphArray,NumAlphas, &
                   NumArray,NumNums,IOSTAT)
        IsNotOk=.false.
        IsBlank=.true.
        CALL VerifyName(AlphArray(1),GSHP%Name,HPNum-1, ISNotOK,ISBlank,'GHSP Name')

        IF (ISNotOK) THEN
            ErrorsFound=.true.
            IF(ISBlank) AlphArray(1)='xxxxx'
        END IF
        GSHP(GSHPNum)%WWHPPlantTypeOfNum    = TypeOf_HPWaterEFHeating
        GSHP(GSHPNum)%Name                  = AlphArray(1)
        GSHP(GSHPNum)%RatedLoadVolFlowHeat  = NumArray(1)
        GSHP(GSHPNum)%RatedSourceVolFlowHeat= NumArray(2)
        GSHP(GSHPNum)%RatedCapHeat          = NumArray(3)
        GSHP(GSHPNum)%RatedPowerHeat        = NumArray(4)
        GSHP(GSHPNum)%HeatCap1              = NumArray(5)
        GSHP(GSHPNum)%HeatCap2              = NumArray(6)
        GSHP(GSHPNum)%HeatCap3              = NumArray(7)
        GSHP(GSHPNum)%HeatCap4              = NumArray(8)
        GSHP(GSHPNum)%HeatCap5              = NumArray(9)
        GSHP(GSHPNum)%HeatPower1            = NumArray(10)
        GSHP(GSHPNum)%HeatPower2            = NumArray(11)
        GSHP(GSHPNum)%HeatPower3            = NumArray(12)
        GSHP(GSHPNum)%HeatPower4            = NumArray(13)
        GSHP(GSHPNum)%HeatPower5            = NumArray(14)

        GSHP(GSHPNum)%SourceSideInletNodeNum   =   &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,HPEqFitHeatingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

        GSHP(GSHPNum)%SourceSideOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,HPEqFitHeatingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

        GSHP(GSHPNum)%LoadSideInletNodeNum    =  &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,HPEqFitHeatingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)

        GSHP(GSHPNum)%LoadSideOutletNodeNum    = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,HPEqFitHeatingUC,AlphArray(1), &
               NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

        ! Test node sets
        CALL TestCompSet(HPEqFitHeatingUC,AlphArray(1),AlphArray(2),AlphArray(3),'Condenser Water Nodes')
        CALL TestCompSet(HPEqFitHeatingUC,AlphArray(1),AlphArray(4),AlphArray(5),'Hot Water Nodes')

        ! save the design source side flow rate for use by plant loop sizing algorithms
        CALL RegisterPlantCompDesignFlow(GSHP(GSHPNum)%SourceSideInletNodeNum,0.5d0*GSHP(GSHPNum)%RatedSourceVolFlowHeat)

        ! CurrentModuleObject='HeatPump:WatertoWater:EquationFit:Heating'
        CALL SetupOutputVariable('Water to Water Heat Pump Electric Energy [J]', &
             GSHPReport(GSHPNum)%Energy,'System','Sum',GSHP(GSHPNum)%Name,  &
             ResourceTypeKey='Electricity',EndUseKey='Heating',GroupKey='Plant')
        CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Energy [J]', &
             GSHPReport(GSHPNum)%QLoadEnergy,'System','Sum',GSHP(GSHPNum)%Name)
        CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Energy [J]', &
             GSHPReport(GSHPNum)%QSourceEnergy,'System','Sum',GSHP(GSHPNum)%Name)
  END DO

  DO GSHPNum = 1,NumGSHPs
    !setup output variables
   CALL SetupOutputVariable('Water to Water Heat Pump Electric Power [W]', &
        GSHPReport(GSHPNum)%Power,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Heat Transfer Rate [W]', &
        GSHPReport(GSHPNum)%QLoad,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Heat Transfer Rate [W]', &
        GSHPReport(GSHPNum)%QSource,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Outlet Temperature [C]', &
        GSHPReport(GSHPNum)%LoadSideOutletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Inlet Temperature [C]', &
        GSHPReport(GSHPNum)%LoadSideInletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Outlet Temperature [C]', &
        GSHPReport(GSHPNum)%SourceSideOutletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Inlet Temperature [C]', &
        GSHPReport(GSHPNum)%SourceSideInletTemp,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Load Side Mass Flow Rate [kg/s]', &
        GSHPReport(GSHPNum)%LoadSideMassFlowRate,'System','Average',GSHP(GSHPNum)%Name)
   CALL SetupOutputVariable('Water to Water Heat Pump Source Side Mass Flow Rate [kg/s]', &
        GSHPReport(GSHPNum)%SourceSideMassFlowRate,'System','Average',GSHP(GSHPNum)%Name)

    !scan for loop connection data
   errFlag=.false.
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 GSHP(GSHPNum)%WWHPPlantTypeOfNum, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum, &
                                 inletNodeNumber = GSHP(GSHPNum)%SourceSideInletNodeNum,  &
                                 errflag=errFlag)
    CALL ScanPlantLoopsForObject(GSHP(GSHPNum)%Name, &
                                 GSHP(GSHPNum)%WWHPPlantTypeOfNum, &
                                 GSHP(GSHPNum)%LoadLoopNum, &
                                 GSHP(GSHPNum)%LoadLoopSideNum, &
                                 GSHP(GSHPNum)%LoadBranchNum, &
                                 GSHP(GSHPNum)%LoadCompNum, &
                                 inletNodeNumber = GSHP(GSHPNum)%LoadSideInletNodeNum,  &
                                 errflag=errFlag)


    IF (errFlag) THEN
      CALL ShowFatalError('GetWatertoWaterHPInput: Program terminated on scan for loop data')
    ENDIF

  END DO


RETURN
END SUBROUTINE GetWatertoWaterHPInput


SUBROUTINE InitWatertoWaterHP(GSHPTypeNum, GSHPName, GSHPNum, FirstHVACIteration, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Water-to-Water HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)
          ! (2) Murugappan, Arun. 2002. Implementing Ground Source Heat Pump and Ground
          ! Loop Heat Exchanger Models in the EnergyPlus Simulation Environment,
          ! M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

          ! USE STATEMENTS:
  USE DataHVACGlobals,   ONLY : TimeStepSys,SysTimeElapsed
  USE DataPlant,         ONLY : TypeOf_HPWaterEFCooling, TypeOf_HPWaterEFHeating, PlantLoop
  USE InputProcessor,    ONLY : SameString
  USE FluidProperties, ONLY : GetDensityGlycol
  USE PlantUtilities,    ONLY : InitComponentNodes, SetComponentFlowRate

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: GSHPTypeNum  ! Type of GSHP
  CHARACTER(len=*), INTENT(IN) :: GSHPName  ! User Specified Name of GSHP
  INTEGER, INTENT(IN)          :: GSHPNum   ! GSHP Number
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  REAL(r64), INTENT(IN)        :: MyLoad    ! Demand Load

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: LoadSideInletNode       ! Load Side Inlet Node
  INTEGER                :: LoadSideOutletNode      ! Load Side Outlet Node
  INTEGER                :: SourceSideInletNode     ! Source Side Inlet Node
  INTEGER                :: SourceSideOutletNode    ! Source Side Outlet Node
  LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:)     :: MyEnvrnFlag   ! Flag required to keep track of initialization
  LOGICAL, SAVE          :: OneTimeFlag = .TRUE.    ! One Time Flag
  REAL(r64), SAVE :: CurrentSimTime = 0.0d0    ! Current Simulation Time
  REAL(r64), SAVE :: PrevSimTime = 0.0d0       ! Previous Simulation Time
  LOGICAL,  ALLOCATABLE, SAVE, DIMENSION(:)  :: MyPlanScanFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64) :: rho ! local fluid density

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyPlanScanFlag(NumGSHPs))
    ALLOCATE(MyEnvrnFlag(NumGSHPs))
    MyOneTimeFlag = .false.
    MyEnvrnFlag  = .TRUE.
    MyPlanScanFlag = .TRUE.
  END IF




  GSHP(GSHPNum)%MustRun = .TRUE. ! Reset MustRun flag to TRUE
  LoadSideInletNode    = GSHP(GSHPNum)%LoadSideInletNodeNum
  LoadSideOutletNode   = GSHP(GSHPNum)%LoadSideOutletNodeNum
  SourceSideInletNode  = GSHP(GSHPNum)%SourceSideInletNodeNum
  SourceSideOutletNode = GSHP(GSHPNum)%SourceSideOutletNodeNum


  IF(MyEnvrnFlag(GSHPNum) .AND. BeginEnvrnFlag)THEN
    !Initialize all report variables to a known state at beginning of simulation

    GSHPReport(GSHPNum)%Power=0.0d0
    GSHPReport(GSHPNum)%Energy = 0.0d0
    GSHPReport(GSHPNum)%QLoad = 0.0d0
    GSHPReport(GSHPNum)%QLoadEnergy = 0.0d0
    GSHPReport(GSHPNum)%QSource = 0.0d0
    GSHPReport(GSHPNum)%QSourceEnergy = 0.0d0
    GSHPReport(GSHPNum)%LoadSideMassFlowRate = 0.0d0
    GSHPReport(GSHPNum)%LoadSideInletTemp = 0.0d0
    GSHPReport(GSHPNum)%LoadSideOutletTemp = 0.0d0
    GSHPReport(GSHPNum)%SourceSideMassFlowRate = 0.0d0
    GSHPReport(GSHPNum)%SourceSideInletTemp = 0.0d0
    GSHPReport(GSHPNum)%SourceSideOutletTemp = 0.0d0
    GSHP(GSHPNum)%IsOn = .FALSE.
    GSHP(GSHPNum)%MustRun = .TRUE.


    IF (GSHP(GSHPNum)%WWHPPlantTypeOfNum  == TypeOf_HPWaterEFHeating) THEN
      rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                           InitconvTemp, &
                           PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                           'InitGshp')
      GSHP(GSHPNum)%LoadSideDesignMassFlow   = GSHP(GSHPNum)%RatedLoadVolFlowHeat * rho
      rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                           InitconvTemp, &
                           PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                           'InitGshp')
      GSHP(GSHPNum)%SourceSideDesignMassFlow = GSHP(GSHPNum)%RatedSourceVolFlowHeat * rho
    ELSEIF  (GSHP(GSHPNum)%WWHPPlantTypeOfNum  == TypeOf_HPWaterEFCooling) THEN
      rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                           InitconvTemp, &
                           PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                           'InitGshp')
      GSHP(GSHPNum)%LoadSideDesignMassFlow   = GSHP(GSHPNum)%RatedLoadVolFlowCool * rho
      rho = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                           InitconvTemp, &
                           PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                           'InitGshp')
      GSHP(GSHPNum)%SourceSideDesignMassFlow = GSHP(GSHPNum)%RatedSourceVolFlowCool * rho
    ENDIF

    CALL InitComponentNodes( 0.d0, GSHP(GSHPNum)%LoadSideDesignMassFlow, &
                                    GSHP(GSHPNum)%LoadSideInletNodeNum, &
                                    GSHP(GSHPNum)%LoadSideOutletNodeNum, &
                                    GSHP(GSHPNum)%LoadLoopNum, &
                                    GSHP(GSHPNum)%LoadLoopSideNum, &
                                    GSHP(GSHPNum)%LoadBranchNum, &
                                    GSHP(GSHPNum)%LoadCompNum)

    CALL InitComponentNodes( 0.d0,GSHP(GSHPNum)%SourceSideDesignMassFlow, &
                                 GSHP(GSHPNum)%SourceSideInletNodeNum, &
                                 GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                 GSHP(GSHPNum)%SourceLoopNum, &
                                 GSHP(GSHPNum)%SourceLoopSideNum, &
                                 GSHP(GSHPNum)%SourceBranchNum, &
                                 GSHP(GSHPNum)%SourceCompNum)

     IF (Node(GSHP(GSHPNum)%SourceSideOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) &
                            Node(GSHP(GSHPNum)%SourceSideOutletNodeNum)%TempSetPoint=0.0d0
     Node(GSHP(GSHPNum)%SourceSideInletNodeNum)%Temp  &
                = Node(GSHP(GSHPNum)%SourceSideOutletNodeNum)%TempSetPoint+30

     MyEnvrnFlag(GSHPNum) = .FALSE.
  END IF
! Reset the environment flag
  IF(.NOT. BeginEnvrnFlag)MyEnvrnFlag(GSHPNum) = .TRUE.

  IF(PrevSimTime .NE. CurrentSimTime)THEN
     PrevSimTime = CurrentSimTime
  END IF

  ! Calculate the simulation time
  CurrentSimTime = (DayOfSim-1)*24 + (HourOfDay-1) + (TimeStep-1)*TimeStepZone + SysTimeElapsed

  ! Initialize event time array when the environment simulation begins
  IF(CurrentSimTime == 0.0d0 .AND. OneTimeFlag)THEN
    OneTimeFlag = .FALSE.
  END IF

  LoopNum              = GSHP(GSHPNum)%LoadLoopNum
  LoopSideNum          = GSHP(GSHPNum)%LoadLoopSideNum

  IF(CurrentSimTime > 0.0d0 )OneTimeFlag = .TRUE.

  IF(MyLoad > 0.0d0 .AND. GSHPTypeNum == TypeOf_HPWaterEFHeating )THEN
    GSHP(GSHPNum)%MustRun = .TRUE.
    GSHP(GSHPNum)%IsOn    = .TRUE.
  ELSEIF (MyLoad < 0.0d0 .AND. GSHPTypeNum == TypeOf_HPWaterEFCooling) THEN
    GSHP(GSHPNum)%MustRun = .TRUE.
    GSHP(GSHPNum)%IsOn    = .TRUE.
  ELSE
    GSHP(GSHPNum)%MustRun = .FALSE.
    GSHP(GSHPNum)%IsOn    = .FALSE.
  END IF

!*******Set flow based on "flowlock" and "run" flags**********
! Set flows if the heat pump is not running
 IF( .NOT. GSHP(GSHPNum)%MustRun )THEN
    GSHPReport(GSHPNum)%LoadSideMassFlowRate  = 0.0d0
    GSHPReport(GSHPNum)%SourceSideMassFlowRate = 0.0d0

    CALL SetComponentFlowRate(GSHPReport(GSHPNum)%LoadSideMassFlowRate, &
                                    GSHP(GSHPNum)%LoadSideInletNodeNum, &
                                    GSHP(GSHPNum)%LoadSideOutletNodeNum, &
                                    GSHP(GSHPNum)%LoadLoopNum, &
                                    GSHP(GSHPNum)%LoadLoopSideNum, &
                                    GSHP(GSHPNum)%LoadBranchNum, &
                                    GSHP(GSHPNum)%LoadCompNum)
    CALL SetComponentFlowRate(GSHPReport(GSHPNum)%SourceSideMassFlowRate, &
                                    GSHP(GSHPNum)%SourceSideInletNodeNum, &
                                    GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                    GSHP(GSHPNum)%SourceLoopNum, &
                                    GSHP(GSHPNum)%SourceLoopSideNum, &
                                    GSHP(GSHPNum)%SourceBranchNum, &
                                    GSHP(GSHPNum)%SourceCompNum)

! Set flows if the heat pump is running
  ELSE ! the heat pump must run

    GSHPReport(GSHPNum)%LoadSideMassFlowRate     = GSHP(GSHPNum)%LoadSideDesignMassFlow
    GSHPReport(GSHPNum)%SourceSideMassFlowRate   = GSHP(GSHPNum)%SourceSideDesignMassFlow
  ! now check against and request in plant
    CALL SetComponentFlowRate(GSHPReport(GSHPNum)%LoadSideMassFlowRate, &
                                    GSHP(GSHPNum)%LoadSideInletNodeNum, &
                                    GSHP(GSHPNum)%LoadSideOutletNodeNum, &
                                    GSHP(GSHPNum)%LoadLoopNum, &
                                    GSHP(GSHPNum)%LoadLoopSideNum, &
                                    GSHP(GSHPNum)%LoadBranchNum, &
                                    GSHP(GSHPNum)%LoadCompNum)
    CALL SetComponentFlowRate(GSHPReport(GSHPNum)%SourceSideMassFlowRate, &
                                    GSHP(GSHPNum)%SourceSideInletNodeNum, &
                                    GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                    GSHP(GSHPNum)%SourceLoopNum, &
                                    GSHP(GSHPNum)%SourceLoopSideNum, &
                                    GSHP(GSHPNum)%SourceBranchNum, &
                                    GSHP(GSHPNum)%SourceCompNum)
     !if there's no flowin one, turn the entire "heat pump off"
    IF(GSHPReport(GSHPNum)%LoadSideMassFlowRate  <= 0.0d0 .OR. &
       GSHPReport(GSHPNum)%SourceSideMassFlowRate  <= 0.0d0)THEN

      GSHPReport(GSHPNum)%LoadSideMassFlowRate = 0.0d0
      GSHPReport(GSHPNum)%SourceSideMassFlowRate = 0.0d0
      GSHP(GSHPNum)%MustRun = .FALSE.

      CALL SetComponentFlowRate(GSHPReport(GSHPNum)%LoadSideMassFlowRate, &
                                      GSHP(GSHPNum)%LoadSideInletNodeNum, &
                                      GSHP(GSHPNum)%LoadSideOutletNodeNum, &
                                      GSHP(GSHPNum)%LoadLoopNum, &
                                      GSHP(GSHPNum)%LoadLoopSideNum, &
                                      GSHP(GSHPNum)%LoadBranchNum, &
                                      GSHP(GSHPNum)%LoadCompNum)
      CALL SetComponentFlowRate(GSHPReport(GSHPNum)%SourceSideMassFlowRate, &
                                      GSHP(GSHPNum)%SourceSideInletNodeNum, &
                                      GSHP(GSHPNum)%SourceSideOutletNodeNum, &
                                      GSHP(GSHPNum)%SourceLoopNum, &
                                      GSHP(GSHPNum)%SourceLoopSideNum, &
                                      GSHP(GSHPNum)%SourceBranchNum, &
                                      GSHP(GSHPNum)%SourceCompNum)

      RETURN
    END IF
  END IF


    ! Get inlet temps
  GSHPReport(GSHPNum)%LoadSideInletTemp   = Node(LoadSideInletNode)%Temp
  GSHPReport(GSHPNum)%SourceSideInletTemp = Node(SourceSideInletNode)%Temp

  ! Outlet variables
  GSHPReport(GSHPNum)%Power=0.0d0
  GSHPReport(GSHPNum)%Energy = 0.0d0
  GSHPReport(GSHPNum)%QLoad = 0.0d0
  GSHPReport(GSHPNum)%QLoadEnergy = 0.0d0
  GSHPReport(GSHPNum)%QSource = 0.0d0
  GSHPReport(GSHPNum)%QSourceEnergy = 0.0d0
  GSHPReport(GSHPNum)%LoadSideOutletTemp = 0.0d0
  GSHPReport(GSHPNum)%SourceSideOutletTemp = 0.0d0

RETURN
END SUBROUTINE InitWatertoWaterHP

SUBROUTINE CalcWatertoWaterHPCooling(GSHPNum, MyLoad)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine simulate the heat pump peformance in cooling mode

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop
  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: GSHPNum   ! GSHP Number
  REAL(r64), INTENT(IN)             :: MyLoad    ! Operating Load

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER   :: CelsiustoKelvin  = KelvinConv  ! Conversion from Celsius to Kelvin
  REAL(r64), PARAMETER   :: Tref             = 283.15d0  ! Reference Temperature for performance curves,10C [K]


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: CoolCapRated                 ! Rated Cooling Capacity [W]
  REAL(r64) :: CoolPowerRated               ! Rated Cooling Power Consumption[W]
  REAL(r64) :: LoadSideVolFlowRateRated     ! Rated Load Side Volumetric Flow Rate [m3/s]
  REAL(r64) :: SourceSideVolFlowRateRated   ! Rated Source Side Volumetric Flow Rate [m3/s]
  REAL(r64) :: CoolCapCoeff1                ! 1st coefficient of the cooling capacity performance curve
  REAL(r64) :: CoolCapCoeff2                ! 2nd coefficient of the cooling capacity performance curve
  REAL(r64) :: CoolCapCoeff3                ! 3rd coefficient of the cooling capacity performance curve
  REAL(r64) :: CoolCapCoeff4                ! 4th coefficient of the cooling capacity performance curve
  REAL(r64) :: CoolCapCoeff5                ! 5th coefficient of the cooling capacity performance curve
  REAL(r64) :: CoolPowerCoeff1              ! 1st coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff2              ! 2nd coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff3              ! 3rd coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff4              ! 4th coefficient of the cooling power consumption curve
  REAL(r64) :: CoolPowerCoeff5              ! 5th coefficient of the cooling power consumption curve

  REAL(r64) :: LoadSideMassFlowRate         ! Load Side Mass Flow Rate [kg/s]
  REAL(r64) :: LoadSideInletTemp            ! Load Side Inlet Temperature [C]
  REAL(r64) :: LoadSideOutletTemp           ! Load side Outlet Temperature [C]
  REAL(r64) :: SourceSideMassFlowRate       ! Source Side Mass Flow Rate [kg/s]
  REAL(r64) :: SourceSideInletTemp          ! Source Side Inlet Temperature [C]
  REAL(r64) :: SourceSideOutletTemp         ! Source Side Outlet Temperature [C]

  REAL(r64) :: func1                        ! Portion of the heat transfer and power equation
  REAL(r64) :: func2                        ! Portion of the heat transfer and power equation
  REAL(r64) :: func3                        ! Portion of the heat transfer and power equation
  REAL(r64) :: func4                        ! Portion of the heat transfer and power equation
  REAL(r64) :: Power                        ! Power Consumption [W]
  REAL(r64) :: QLoad                        ! Cooling Capacity [W]
  REAL(r64) :: QSource                      ! Source Side Heat Transfer Rate [W]
  REAL(r64) :: PartLoadRatio                ! Part-Load Ratio
  REAL(r64) :: ReportingConstant
  REAL(r64) :: rhoLoadSide
  REAL(r64) :: rhoSourceSide
  REAL(r64) :: CpLoadSide
  REAL(r64) :: CpSourceSide

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
  LoadSideVolFlowRateRated  = GSHP(GSHPNum)%RatedLoadVolFlowCool
  SourceSideVolFlowRateRated= GSHP(GSHPNum)%RatedSourceVolFlowCool
  CoolCapRated              = GSHP(GSHPNum)%RatedCapCool
  CoolPowerRated            = GSHP(GSHPNum)%RatedPowerCool
  CoolCapCoeff1             = GSHP(GSHPNum)%CoolCap1
  CoolCapCoeff2             = GSHP(GSHPNum)%CoolCap2
  CoolCapCoeff3             = GSHP(GSHPNum)%CoolCap3
  CoolCapCoeff4             = GSHP(GSHPNum)%CoolCap4
  CoolCapCoeff5             = GSHP(GSHPNum)%CoolCap5
  CoolPowerCoeff1           = GSHP(GSHPNum)%CoolPower1
  CoolPowerCoeff2           = GSHP(GSHPNum)%CoolPower2
  CoolPowerCoeff3           = GSHP(GSHPNum)%CoolPower3
  CoolPowerCoeff4           = GSHP(GSHPNum)%CoolPower4
  CoolPowerCoeff5           = GSHP(GSHPNum)%CoolPower5

  LoadSideMassFlowRate      = GSHPReport(GSHPNum)%LoadSideMassFlowRate
  LoadSideInletTemp         = GSHPReport(GSHPNum)%LoadSideInletTemp
  SourceSideMassFlowRate    = GSHPReport(GSHPNum)%SourceSideMassFlowRate
  SourceSideInletTemp       = GSHPReport(GSHPNum)%SourceSideInletTemp

  ! If heat pump is not operating, THEN return
  IF(.NOT. GSHP(GSHPNum)%MustRun) THEN
    RETURN
  ENDIF

  rhoLoadSide =  GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                           LoadSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPCooling')

  rhoSourceSide = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                           SourceSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPCooling')

  func1 = ((LoadSideInletTemp+CelsiustoKelvin)/Tref)
  func2 = ((SourceSideInletTemp+CelsiustoKelvin)/Tref)
  func3 = (LoadSideMassFlowRate/(LoadSideVolFlowRateRated * rhoLoadSide))
  func4 = (SourceSideMassFlowRate/(SourceSideVolFlowRateRated * rhoSourceSide))

  QLoad = CoolCapRated*(CoolCapCoeff1 + (func1 * CoolCapCoeff2) + (func2 * CoolCapCoeff3) + (func3 * CoolCapCoeff4)+   &
                                          (func4 * CoolCapCoeff5))
  Power = CoolPowerRated*(CoolPowerCoeff1 + (func1 * CoolPowerCoeff2) + (func2 * CoolPowerCoeff3) +   &
                                          (func3 * CoolPowerCoeff4) + (func4 * CoolPowerCoeff5))

  IF ( (Qload .LE. 0.0d0 .OR. Power .LE. 0.0d0) .AND. .NOT. WarmupFlag) THEN
    IF (Qload .LE. 0.0d0) THEN
      IF (GSHP(GSHPNum)%CoolCapNegativeCounter .LT. 1) THEN
        GSHP(GSHPNum)%CoolCapNegativeCounter = GSHP(GSHPNum)%CoolCapNegativeCounter + 1
        CALL ShowWarningError(TRIM(HPEqFitCooling)//' "'//TRIM(GSHP(GSHPNum)%Name)//'":')
        CALL ShowContinueError(' Cooling capacity curve output is <= 0.0 ('//TRIM(TrimSigDigits(QLoad,4))//').')
        CALL ShowContinueError(' Zero or negative value occurs with a load-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(LoadSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a source-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(SourceSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a load-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(LoadSideMassFlowRate,3))//' kg/s,')
        CALL ShowContinueError(' and a source-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(SourceSideMassFlowRate,3))//' kg/s.')
        CALL ShowContinueErrorTimeStamp(' The heat pump is turned off for this time step but simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(HPEqFitCooling)//' "'// TRIM(GSHP(GSHPNum)%Name)//'":'//&
                     ' Cooling capacity curve output is <= 0.0 warning continues...' &
                     , GSHP(GSHPNum)%CoolCapNegativeIndex, Qload, Qload)
      END IF
    END IF
    IF (Power .LE. 0.0d0) THEN
      IF (GSHP(GSHPNum)%CoolPowerNegativeCounter .LT. 1) THEN
        GSHP(GSHPNum)%CoolPowerNegativeCounter = GSHP(GSHPNum)%CoolPowerNegativeCounter + 1
        CALL ShowWarningError(TRIM(HPEqFitCooling)//' "'//TRIM(GSHP(GSHPNum)%Name)//'":')
        CALL ShowContinueError(' Cooling compressor power curve output is <= 0.0 ('//TRIM(TrimSigDigits(Power,4))//').')
        CALL ShowContinueError(' Zero or negative value occurs with a load-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(LoadSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a source-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(SourceSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a load-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(LoadSideMassFlowRate,3))//' kg/s,')
        CALL ShowContinueError(' and a source-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(SourceSideMassFlowRate,3))//' kg/s.')
        CALL ShowContinueErrorTimeStamp(' The heat pump is turned off for this time step but simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(HPEqFitCooling)//' "'// TRIM(GSHP(GSHPNum)%Name)//'":'//&
                     ' Cooling compressor power curve output is <= 0.0 warning continues...' &
                     , GSHP(GSHPNum)%CoolPowerNegativeIndex, Power, Power)
      END IF
    END IF

    Qload = 0.0d0
    Power = 0.0d0

  END IF

  QSource = QLoad+Power   !assume no losses

  !Control Strategy
  IF(ABS(MyLoad) < QLoad .AND. Qload .NE. 0.0d0) THEN
    PartLoadRatio        = ABS(MyLoad)/QLoad
    QLoad                = ABS(MyLoad)
    Power                = Power * PartLoadRatio
    QSource              = QSource * PartLoadRatio
  END IF

  CpLoadSide   = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                           LoadSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPCooling')

  CpSourceSide = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                           SourceSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPCooling')

  LoadSideOutletTemp   = LoadSideInletTemp - QLoad/(LoadSideMassFlowRate * CpLoadSide)
  SourceSideOutletTemp = SourceSideInletTemp + QSource/(SourceSideMassFlowRate * CpSourceSide )

  ReportingConstant = TimeStepSys*SecInHour

  GSHPReport(GSHPNum)%Power                 = Power
  GSHPReport(GSHPNum)%Energy                = Power*ReportingConstant
  GSHPReport(GSHPNum)%QSource               = QSource
  GSHPReport(GSHPNum)%QLoad                 = QLoad
  GSHPReport(GSHPNum)%QSourceEnergy         = QSource*ReportingConstant
  GSHPReport(GSHPNum)%QLoadEnergy           = QLoad*ReportingConstant
  GSHPReport(GSHPNum)%LoadSideOutletTemp    = LoadSideOutletTemp
  GSHPReport(GSHPNum)%SourceSideOutletTemp  = SourceSideOutletTemp
  RETURN
END SUBROUTINE CalcWatertoWaterHPCooling


SUBROUTINE CalcWatertoWaterHPHeating(GSHPNum, MyLoad)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kenneth Tang
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine simulate the heat pump peformance in heating mode

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! (1) Tang,C.C.. 2005. Modeling Packaged Heat Pumps in a Quasi-Steady
          ! State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering,
          ! Oklahoma State University. (downloadable from http://www.hvac.okstate.edu/)


          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop
  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: GSHPNum   ! GSHP Number
  REAL(r64), INTENT(IN)             :: MyLoad    ! Operating Load

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER   :: CelsiustoKelvin  = KelvinConv  ! Conversion from Celsius to Kelvin
  REAL(r64), PARAMETER   :: Tref             = 283.15d0  ! Reference Temperature for performance curves,10C [K]


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: HeatCapRated                 ! Rated Heating Capacity [W]
  REAL(r64) :: HeatPowerRated               ! Rated Heating Compressor Power[W]
  REAL(r64) :: LoadSideVolFlowRateRated     ! Rated Load Side Volumetric Flow Rate [m3/s]
  REAL(r64) :: SourceSideVolFlowRateRated   ! Rated Source Side Volumetric Flow Rate [m3/s]
  REAL(r64) :: HeatCapCoeff1                ! 1st coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff2                ! 2nd coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff3                ! 3rd coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff4                ! 4th coefficient of the heating capacity performance curve
  REAL(r64) :: HeatCapCoeff5                ! 5th coefficient of the heating capacity performance curve
  REAL(r64) :: HeatPowerCoeff1              ! 1st coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff2              ! 2nd coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff3              ! 3rd coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff4              ! 4th coefficient of the heating power consumption curve
  REAL(r64) :: HeatPowerCoeff5              ! 5th coefficient of the heating power consumption curve
  REAL(r64) :: LoadSideMassFlowRate         ! Load Side Mass Flow Rate [kg/s]
  REAL(r64) :: LoadSideInletTemp            ! Load Side Inlet Temperature [C]
  REAL(r64) :: LoadSideOutletTemp           ! Load side Outlet Temperature [C]
  REAL(r64) :: SourceSideMassFlowRate       ! Source Side Mass Flow Rate [kg/s]
  REAL(r64) :: SourceSideInletTemp          ! Source Side Inlet Temperature [C]
  REAL(r64) :: SourceSideOutletTemp         ! Source Side Outlet Temperature [C]
  REAL(r64) :: func1                        ! Portion of the heat transfer and power equation
  REAL(r64) :: func2                        ! Portion of the heat transfer and power equation
  REAL(r64) :: func3                        ! Portion of the heat transfer and power equation
  REAL(r64) :: func4                        ! Portion of the heat transfer and power equation
  REAL(r64) :: Power                        ! Power Consumption [W]
  REAL(r64) :: QLoad                        ! Cooling Capacity [W]
  REAL(r64) :: QSource                      ! Source Side Heat Transfer Rate [W]
  REAL(r64) :: PartLoadRatio                ! Part Load Ratio
  REAL(r64) :: ReportingConstant
  REAL(r64) :: rhoLoadSide
  REAL(r64) :: rhoSourceSide
  REAL(r64) :: CpLoadSide
  REAL(r64) :: CpSourceSide

  !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE
  LoadSideVolFlowRateRated  = GSHP(GSHPNum)%RatedLoadVolFlowHeat
  SourceSideVolFlowRateRated= GSHP(GSHPNum)%RatedSourceVolFlowHeat
  HeatCapRated              = GSHP(GSHPNum)%RatedCapHeat
  HeatPowerRated            = GSHP(GSHPNum)%RatedPowerHeat
  HeatCapCoeff1             = GSHP(GSHPNum)%HeatCap1
  HeatCapCoeff2             = GSHP(GSHPNum)%HeatCap2
  HeatCapCoeff3             = GSHP(GSHPNum)%HeatCap3
  HeatCapCoeff4             = GSHP(GSHPNum)%HeatCap4
  HeatCapCoeff5             = GSHP(GSHPNum)%HeatCap5
  HeatPowerCoeff1           = GSHP(GSHPNum)%HeatPower1
  HeatPowerCoeff2           = GSHP(GSHPNum)%HeatPower2
  HeatPowerCoeff3           = GSHP(GSHPNum)%HeatPower3
  HeatPowerCoeff4           = GSHP(GSHPNum)%HeatPower4
  HeatPowerCoeff5           = GSHP(GSHPNum)%HeatPower5

  LoadSideMassFlowRate      = GSHPReport(GSHPNum)%LoadSideMassFlowRate
  LoadSideInletTemp         = GSHPReport(GSHPNum)%LoadSideInletTemp
  SourceSideMassFlowRate    = GSHPReport(GSHPNum)%SourceSideMassFlowRate
  SourceSideInletTemp       = GSHPReport(GSHPNum)%SourceSideInletTemp


  ! If heat pump is not operating, THEN return
  IF(.NOT. GSHP(GSHPNum)%MustRun) THEN
    RETURN
  ENDIF
  rhoLoadSide =  GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                           LoadSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPHeating')

  rhoSourceSide = GetDensityGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                           SourceSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPHeating')

  func1 = ((LoadSideInletTemp+CelsiustoKelvin)/Tref)
  func2 = ((SourceSideInletTemp+CelsiustoKelvin)/Tref)
  func3 = (LoadSideMassFlowRate/(LoadSideVolFlowRateRated * rhoLoadSide ))
  func4 = (SourceSideMassFlowRate/(SourceSideVolFlowRateRated * rhoSourceSide ))

  QLoad = HeatCapRated*(HeatCapCoeff1 + (func1 * HeatCapCoeff2) + (func2 * HeatCapCoeff3) + (func3 * HeatCapCoeff4)+   &
                                        (func4 * HeatCapCoeff5))
  Power = HeatPowerRated*(HeatPowerCoeff1 + (func1 * HeatPowerCoeff2) + (func2 * HeatPowerCoeff3) +   &
                                         (func3 * HeatPowerCoeff4) + (func4 * HeatPowerCoeff5))

  IF ( (Qload .LE. 0.0d0 .OR. Power .LE. 0.0d0) .AND. .NOT. WarmupFlag) THEN
    IF (Qload .LE. 0.0d0) THEN
      IF (GSHP(GSHPNum)%HeatCapNegativeCounter .LT. 1) THEN
        GSHP(GSHPNum)%HeatCapNegativeCounter = GSHP(GSHPNum)%HeatCapNegativeCounter + 1
        CALL ShowWarningError(TRIM(HPEqFitHeating)//' "'//TRIM(GSHP(GSHPNum)%Name)//'":')
        CALL ShowContinueError(' Heating capacity curve output is <= 0.0 ('//TRIM(TrimSigDigits(QLoad,4))//').')
        CALL ShowContinueError(' Zero or negative value occurs with a load-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(LoadSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a source-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(SourceSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a load-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(LoadSideMassFlowRate,3))//' kg/s,')
        CALL ShowContinueError(' and a source-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(SourceSideMassFlowRate,3))//' kg/s.')
        CALL ShowContinueErrorTimeStamp(' The heat pump is turned off for this time step but simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(HPEqFitHeating)//' "'// TRIM(GSHP(GSHPNum)%Name)//'":'//&
                     ' Heating capacity curve output is <= 0.0 warning continues...' &
                     , GSHP(GSHPNum)%HeatCapNegativeIndex, Qload, Qload)
      END IF
    END IF
    IF (Power .LE. 0.0d0) THEN
      IF (GSHP(GSHPNum)%HeatPowerNegativeCounter .LT. 1) THEN
        GSHP(GSHPNum)%HeatPowerNegativeCounter = GSHP(GSHPNum)%HeatPowerNegativeCounter + 1
        CALL ShowWarningError(TRIM(HPEqFitHeating)//' "'//TRIM(GSHP(GSHPNum)%Name)//'":')
        CALL ShowContinueError(' Heating compressor power curve output is <= 0.0 ('//TRIM(TrimSigDigits(Power,4))//').')
        CALL ShowContinueError(' Zero or negative value occurs with a load-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(LoadSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a source-side inlet temperature of ' &
                                //TRIM(TrimSigDigits(SourceSideInletTemp,2))//' C,')
        CALL ShowContinueError(' a load-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(LoadSideMassFlowRate,3))//' kg/s,')
        CALL ShowContinueError(' and a source-side mass flow rate of ' &
                                //TRIM(TrimSigDigits(SourceSideMassFlowRate,3))//' kg/s.')
        CALL ShowContinueErrorTimeStamp(' The heat pump is turned off for this time step but simulation continues.')
      ELSE
        CALL ShowRecurringWarningErrorAtEnd(TRIM(HPEqFitHeating)//' "'// TRIM(GSHP(GSHPNum)%Name)//'":'//&
                     ' Heating compressor power curve output is <= 0.0 warning continues...' &
                     , GSHP(GSHPNum)%HeatPowerNegativeIndex, Power, Power)
      END IF
    END IF

    Qload = 0.0d0
    Power = 0.0d0

  END IF

  QSource = QLoad-Power   !assume no losses

  !Control Strategy
  IF(ABS(MyLoad) < QLoad .AND. Qload .NE. 0.0d0) THEN
    PartLoadRatio        = ABS(MyLoad)/QLoad
    QLoad                = ABS(MyLoad)
    Power                = Power * PartLoadRatio
    QSource              = QSource * PartLoadRatio
  END IF

  CpLoadSide   = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidName, &
                           LoadSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%LoadLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPHeating')

  CpSourceSide = GetSpecificHeatGlycol(PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidName, &
                           SourceSideInletTemp, &
                           PlantLoop(GSHP(GSHPNum)%SourceLoopNum)%FluidIndex, &
                           'CalcWatertoWaterHPHeating')

  LoadSideOutletTemp   = LoadSideInletTemp + QLoad/(LoadSideMassFlowRate * CpLoadSide)
  SourceSideOutletTemp = SourceSideInletTemp - QSource/(SourceSideMassFlowRate * CpSourceSide )

  ReportingConstant = TimeStepSys*SecInHour

  GSHPReport(GSHPNum)%Power                 = Power
  GSHPReport(GSHPNum)%Energy                = Power*ReportingConstant
  GSHPReport(GSHPNum)%QSource               = QSource
  GSHPReport(GSHPNum)%QLoad                 = QLoad
  GSHPReport(GSHPNum)%QSourceEnergy         = QSource*ReportingConstant
  GSHPReport(GSHPNum)%QLoadEnergy           = QLoad*ReportingConstant
  GSHPReport(GSHPNum)%LoadSideOutletTemp    = LoadSideOutletTemp
  GSHPReport(GSHPNum)%SourceSideOutletTemp  = SourceSideOutletTemp
  RETURN
END SUBROUTINE CalcWatertoWaterHPHeating


SUBROUTINE UpdateGSHPRecords(GSHPNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Kenneth Tang
            !       DATE WRITTEN:    March 2005

            ! PURPOSE OF THIS SUBROUTINE:
            ! reporting


            ! METHODOLOGY EMPLOYED: na

            ! REFERENCES: na

            ! USE STATEMENTS:

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: GSHPNum       ! GSHP number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: SourceSideInletNode      ! Source Side inlet node number, water side
  INTEGER     :: SourceSideOutletNode     ! Source Side outlet node number, water side
  INTEGER     :: LoadSideInletNode        ! Load Side inlet node number, water side
  INTEGER     :: LoadSideOutletNode       ! Load Side outlet node number, water side

    LoadSideInletNode    = GSHP(GSHPNum)%LoadSideInletNodeNum
    LoadSideOutletNode   = GSHP(GSHPNum)%LoadSideOutletNodeNum
    SourceSideInletNode  = GSHP(GSHPNum)%SourceSideInletNodeNum
    SourceSideOutletNode = GSHP(GSHPNum)%SourceSideOutletNodeNum

  IF (.NOT. GSHP(GSHPNum)%MustRun )THEN
  ! Heatpump is off; just pass through conditions
    GSHPReport(GSHPNum)%Power                = 0.0d0
    GSHPReport(GSHPNum)%Energy               = 0.0d0
    GSHPReport(GSHPNum)%QSource              = 0.0d0
    GSHPReport(GSHPNum)%QSourceEnergy        = 0.0d0
    GSHPReport(GSHPNum)%QLoad                = 0.0d0
    GSHPReport(GSHPNum)%QLoadEnergy          = 0.0d0
    GSHPReport(GSHPNum)%LoadSideOutletTemp   = GSHPReport(GSHPNum)%LoadSideInletTemp
    GSHPReport(GSHPNum)%SourceSideOutletTemp = GSHPReport(GSHPNum)%SourceSideInletTemp
  END IF

  Node(SourceSideOutletNode)%Temp          = GSHPReport(GSHPNum)%SourceSideOutletTemp
  Node(LoadSideOutletNode)%Temp            = GSHPReport(GSHPNum)%LoadSideOutletTemp
RETURN
END SUBROUTINE UpdateGSHPRecords

END MODULE HeatPumpWaterToWaterSimple

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

