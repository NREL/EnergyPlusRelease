
MODULE PlantComponentTemperatureSources

          ! MODULE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates plant supply components which operate against a
          !  predefined (but variable) boundary temperature.

          ! METHODOLOGY EMPLOYED:
          ! Called by plantloopequipment, model accepts inputs, and calculates a
          ! thermal response using new plant routines such as SetComponentFlowRate

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals ,    ONLY: MaxNameLength, InitConvTemp
USE DataHVACGlobals, ONLY: SmallWaterVolFlow
USE DataPlant,       ONLY: TypeOf_WaterSource, PlantLocation
USE General,         ONLY: TrimSigDigits
USE DataInterfaces

IMPLICIT NONE

PRIVATE

          !MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: TempSpecType_Constant = -1
INTEGER, PARAMETER :: TempSpecType_Schedule = -2

          !MODULE DERIVED TYPE DEFINITIONS:
TYPE WaterSourceSpecs
  CHARACTER(len=MaxNameLength) :: Name     =' ' ! user identifier
  INTEGER           :: InletNodeNum  =0   ! Node number on the inlet side of the plant
  INTEGER           :: OutletNodeNum =0   ! Node number on the outlet side of the plant
  REAL(r64)         :: DesVolFlowRate     =0.0d0 ! m**3/s - design nominal volumetric flow rate
  REAL(r64)         :: MassFlowRateMax =0.0d0 ! kg/s - design mass flow rate
  LOGICAL           :: EMSOverrideOnMassFlowRateMax = .FALSE. ! if true EMS is calling to override maximum mass flow
  REAL(r64)         :: EMSOverrideValueMassFlowRateMax = 0.d0 ! value to use if EMS is overriding max mass flow
  REAL(r64)         :: MassFlowRate = 0.0d0
  INTEGER           :: TempSpecType = 0 ! temperature specification type
  CHARACTER(len=MaxNameLength) :: TempSpecScheduleName = ''
  INTEGER           :: TempSpecScheduleNum = 0
  REAL(r64)         :: BoundaryTemp = 0.0d0
  REAL(r64)         :: OutletTemp = 0.0d0 !may be different if the flow is off
  REAL(r64)         :: InletTemp = 0.0d0
  REAL(r64)         :: HeatRate = 0.0d0
  REAL(r64)         :: HeatEnergy = 0.0d0
  TYPE(PlantLocation) :: Location = PlantLocation(0,0,0,0)
  REAL(r64)         :: SizFac                    = 0.0d0 ! sizing factor
  LOGICAL           :: CheckEquipName = .TRUE.
  LOGICAL           :: MyFlag = .TRUE.
  LOGICAL           :: MyEnvironFlag = .TRUE.
END TYPE WaterSourceSpecs

          !MODULE VARIABLES
TYPE (WaterSourceSpecs), ALLOCATABLE, DIMENSION(:)  :: WaterSource         !dimension to number of machines
INTEGER     :: NumSources = 0
LOGICAL     :: GetInput = .TRUE.! then TRUE, calls subroutine to read input file.

          !MODULE ROUTINES
PUBLIC     SimWaterSource
PRIVATE    GetWaterSource
PRIVATE    InitWaterSource
PRIVATE    SizeWaterSource
PRIVATE    CalcWaterSource
PRIVATE    UpdateWaterSource

CONTAINS

SUBROUTINE SimWaterSource(SourceName,EquipFlowCtrl,CompIndex,RunFlag,FirstHVACIteration, &
                              InitLoopEquip,MyLoad,MaxLoad,MinLoad,OptLoad,GetSizingFactor,SizingFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   October 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This is the water source model driver.  It
          !  gets the input for the models, initializes simulation variables, call
          !  the appropriate model and sets up reporting variables.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DataGlobals,    ONLY: BigNumber

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SourceName             ! user-specified name for this component
  INTEGER, INTENT(IN)          :: EquipFlowCtrl      ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)       :: CompIndex          ! HX number pointer
  LOGICAL , INTENT(IN)         :: RunFlag            ! simulate HX when TRUE
  LOGICAL , INTENT(IN)         :: FirstHVACIteration ! initialize variables when TRUE
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip      ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)     :: MyLoad             ! loop demand component will meet
  REAL(r64), INTENT(INOUT)     :: MaxLoad
  REAL(r64), INTENT(INOUT)     :: MinLoad
  REAL(r64), INTENT(INOUT)     :: OptLoad
  LOGICAL, INTENT(IN)          :: GetSizingFactor    ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(INOUT)     :: SizingFactor       ! sizing factor

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SourceNum            ! HX number pointer

            !GET INPUT
      IF (GetInput)  THEN
        CALL GetWaterSource()
        GetInput = .FALSE.
      END IF

        ! Find the correct Chiller
      IF (CompIndex == 0) THEN
        SourceNum = FindItemInList(SourceName,WaterSource%Name,NumSources)
        IF (SourceNum == 0) THEN
          CALL ShowFatalError('SimWaterSource: Specified heat exchanger not one of Valid heat exchangers='//TRIM(SourceName))
        ENDIF
        CompIndex=SourceNum
      ELSE
        SourceNum=CompIndex
        IF (SourceNum > NumSources .or. SourceNum < 1) THEN
          CALL ShowFatalError('SimWaterSource:  Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(SourceNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumSources))//  &
                              ', Entered Unit name='//TRIM(SourceName))
        ENDIF
        IF (WaterSource(SourceNum)%CheckEquipName) THEN
          IF (SourceName /= WaterSource(SourceNum)%Name) THEN
            CALL ShowFatalError('SimWaterSource: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(SourceNum))// &
                                ', Unit name='//TRIM(SourceName)//', stored Unit Name for that index='//  &
                                TRIM(WaterSource(SourceNum)%Name))
          ENDIF
          WaterSource(SourceNum)%CheckEquipName=.false.
        ENDIF
      ENDIF

      IF (InitLoopEquip) THEN
        CALL InitWaterSource(SourceNum,RunFlag,MyLoad,FirstHVACIteration)
        CALL SizeWaterSource(SourceNum)
        IF (GetSizingFactor) THEN
          SizingFactor = WaterSource(SourceNum)%SizFac
        END IF
        MaxLoad = BigNumber
        MinLoad = 0.0d0
        OptLoad = BigNumber
        RETURN
      END IF

    CALL InitWaterSource(SourceNum,RunFlag,MyLoad,FirstHVACIteration)
    CALL CalcWaterSource(SourceNum,MyLoad,Runflag,EquipFlowCtrl)
    CALL UpdateWaterSource(SourceNum)

  RETURN

END SUBROUTINE SimWaterSource

SUBROUTINE GetWaterSource

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Edwin Lee
            !       DATE WRITTEN:    October 2012

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine gets the inputs and processes them into local data structures

            ! METHODOLOGY EMPLOYED:
            ! Standard E+ input processor interaction

            ! REFERENCES:
            !WaterSource,
            !  A1 , \field Name
            !  A2 , \field Inlet Node
            !  A3 , \field Outlet Node
            !  N1 , \field Design Volume Flow Rate
            !  A4 , \field Temperature Specification Type
            !  N2 , \field Boundary Temperature
            !  A5 ; \field Source Temperature Schedule Name

            ! USE STATEMENTS:
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE ScheduleManager,    ONLY: GetScheduleIndex
  USE DataInterfaces, ONLY: SetupOutputVariable, SetupEMSActuator
  USE DataGLobals, ONLY: AnyEnergyManagementSystemInModel

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: SourceNum
  INTEGER       :: NumAlphas ! Number of elements in the alpha array
  INTEGER       :: NumNums   ! Number of elements in the numeric array
  INTEGER       :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL, SAVE :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name

            !GET NUMBER OF ALL EQUIPMENT TYPES
  cCurrentModuleObject = 'PlantComponent:TemperatureSource'
  NumSources = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumSources <= 0) THEN
    CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
    ErrorsFound=.true.
  ENDIF

            !See if load distribution manager has already gotten the input
  IF (ALLOCATED(WaterSource)) RETURN ! probably not possible, and probably should throw error
  ALLOCATE (WaterSource(NumSources))

  ! fill arrays
  DO SourceNum = 1 , NumSources
    CALL GetObjectItem(cCurrentModuleObject,SourceNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),WaterSource%Name,SourceNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    WaterSource(SourceNum)%Name = cAlphaArgs(1)

    WaterSource(SourceNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    WaterSource(SourceNum)%OutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')

    WaterSource(SourceNum)%DesVolFlowRate = rNumericArgs(1)

    IF (cAlphaArgs(4) .EQ. 'CONSTANT') THEN
        WaterSource(SourceNum)%TempSpecType = TempSpecType_Constant
        WaterSource(SourceNum)%BoundaryTemp = rNumericArgs(2)
    ELSE IF (cAlphaArgs(4) .EQ. 'SCHEDULED') THEN
        WaterSource(SourceNum)%TempSpecType = TempSpecType_Schedule
        WaterSource(SourceNum)%TempSpecScheduleName = cAlphaArgs(5)
        WaterSource(SourceNum)%TempSpecScheduleNum = GetScheduleIndex(cAlphaArgs(5))
        IF (WaterSource(SourceNum)%TempSpecScheduleNum == 0) THEN
            CALL ShowSevereError('Input error for '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Invalid schedule name in field '//TRIM(cAlphaFieldNames(5))//'='//cAlphaArgs(5))
            ErrorsFound = .TRUE.
        END IF
    ELSE
        CALL ShowSevereError('Input error for '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('Invalid temperature specification type.  Expected either "Constant" or "Scheduled". Encountered "' &
                               //TRIM(cAlphaArgs(4))//'"')
        ErrorsFound = .TRUE.
    END IF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF

  DO SourceNum = 1, NumSources
     CALL SetupOutputVariable('Plant Temperature Source Component Mass Flow Rate [kg/s]', &
          WaterSource(SourceNum)%MassFlowRate,'System','Average',WaterSource(SourceNum)%Name)
     CALL SetupOutputVariable('Plant Temperature Source Component Inlet Temperature [C]', &
          WaterSource(SourceNum)%InletTemp,'System','Average',WaterSource(SourceNum)%Name)
     CALL SetupOutputVariable('Plant Temperature Source Component Outlet Temperature [C]', &
          WaterSource(SourceNum)%OutletTemp,'System','Average',WaterSource(SourceNum)%Name)
     CALL SetupOutputVariable('Plant Temperature Source Component Source Temperature [C]', &
          WaterSource(SourceNum)%BoundaryTemp,'System','Average',WaterSource(SourceNum)%Name)
     CALL SetupOutputVariable('Plant Temperature Source Component Heat Transfer Rate [W]', &
          WaterSource(SourceNum)%HeatRate,'System','Average',WaterSource(SourceNum)%Name)
     CALL SetupOutputVariable('Plant Temperature Source Component Heat Transfer Energy [J]', &
          WaterSource(SourceNum)%HeatEnergy,'System','Sum',WaterSource(SourceNum)%Name)
     IF ( AnyEnergyManagementSystemInModel ) THEN
       CALL SetupEMSActuator('PlantComponent:TemperatureSource', WaterSource(SourceNum)%Name, &
                             'Maximum Mass Flow Rate', '[kg/s]', &
                             WaterSource(SourceNum)%EMSOverrideOnMassFlowRateMax, &
                             WaterSource(SourceNum)%EMSOverrideValueMassFlowRateMax)
     ENDIF
  END DO

  RETURN

END SUBROUTINE GetWaterSource

SUBROUTINE InitWaterSource(SourceNum ,RunFlag, MyLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the water source objects

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY : BeginEnvrnFlag, WarmupFlag
  USE DataPlant,         ONLY : PlantLoop, ScanPlantLoopsForObject,PlantSizeNotComplete, PlantSizesOkayToFinalize
  USE PlantUtilities,    ONLY : InitComponentNodes, SetComponentFlowRate
  USE FluidProperties,   ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE ScheduleManager,   ONLY : GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN) :: SourceNum     ! number of the current component being simulated
  LOGICAL,   INTENT(IN)  :: RunFlag      ! TRUE when component operating
  REAL(r64), INTENT(IN)  :: MyLoad
  LOGICAL,   INTENT(IN)  :: FirstHVACIteration      ! initialize variables when TRUE

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='InitWaterSource'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: cp ! local specific heat
  LOGICAL :: errFlag

          !FLOW

  ! Init more variables
  IF (WaterSource(SourceNum)%MyFlag) THEN
    ! Locate the component on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(WaterSource(SourceNum)%Name, &
                                 TypeOf_WaterSource, &
                                 WaterSource(SourceNum)%Location%LoopNum, &
                                 WaterSource(SourceNum)%Location%LoopSideNum, &
                                 WaterSource(SourceNum)%Location%BranchNum, &
                                 WaterSource(SourceNum)%Location%CompNum,  &
                                 InletNodeNumber = WaterSource(SourceNum)%InletNodeNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError(RoutineName//': Program terminated due to previous condition(s).')
    ENDIF
    WaterSource(SourceNum)%MyFlag=.FALSE.
  ENDIF

     !Initialize critical Demand Side Variables at the beginning of each environment
  IF(WaterSource(SourceNum)%MyEnvironFlag .and. BeginEnvrnFlag .AND. (PlantSizesOkayToFinalize))Then
    IF (PlantSizeNotComplete) CALL SizeWaterSource(SourceNum)
    rho = GetDensityGlycol(PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%FluidName,  &
                                InitConvTemp, &
                                PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%FluidIndex,&
                                RoutineName)
    WaterSource(SourceNum)%MassFlowRateMax = WaterSource(SourceNum)%DesVolFlowRate * rho
    CALL InitComponentNodes(0.0D0,WaterSource(SourceNum)%MassFlowRateMax,  &
                         WaterSource(SourceNum)%InletNodeNum,        &
                         WaterSource(SourceNum)%OutletNodeNum,       &
                         WaterSource(SourceNum)%Location%LoopNum,               &
                         WaterSource(SourceNum)%Location%LoopSideNum,           &
                         WaterSource(SourceNum)%Location%BranchNum,             &
                         WaterSource(SourceNum)%Location%CompNum)

    WaterSource(SourceNum)%MyEnvironFlag = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    WaterSource(SourceNum)%MyEnvironFlag=.true.
  ENDIF

  ! OK, so we can set up the inlet and boundary temperatures now
  WaterSource(SourceNum)%InletTemp = Node(WaterSource(SourceNum)%InletNodeNum)%Temp
  IF (WaterSource(SourceNum)%TempSpecType == TempSpecType_Schedule) THEN
    WaterSource(SourceNum)%BoundaryTemp = GetCurrentScheduleValue(WaterSource(SourceNum)%TempSpecScheduleNum)
  END IF

  ! Calculate specific heat
  Cp = GetSpecificHeatGlycol(PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%FluidName,  &
                             WaterSource(SourceNum)%BoundaryTemp,                           &
                             PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%FluidIndex, &
                             RoutineName)

  ! if myload is > 0 then we want to heat the loop
  ! if myload is < 0 then we want to cool the loop
  ! thus, given a fixed outlet temperature (the boundary temp, Tbnd), the eq is:
  !  myload = mdot * cp * (Tbnd - Tin)
  ! re-arranging:
  !  mdot = myload / [cp * (Tbnd - Tin)]
  ! if mdot
  WaterSource(SourceNum)%MassFlowRate=MyLoad/(Cp*(WaterSource(SourceNum)%BoundaryTemp-WaterSource(SourceNum)%InletTemp))

  ! If the mdot is negative it means we can't help the load so we will want to just go to zero.
  ! If the mdot is already zero, then well, we still want to go to zero
  ! If the mdot is positive, just make sure we constrain it to the design value
  IF (WaterSource(SourceNum)%MassFlowRate < 0) THEN
    WaterSource(SourceNum)%MassFlowRate = 0.0d0
  ELSE
    IF (.NOT. WaterSource(SourceNum)%EMSOverrideOnMassFlowRateMax) THEN
      WaterSource(SourceNum)%MassFlowRate = MIN(WaterSource(SourceNum)%MassFlowRate, WaterSource(SourceNum)%MassFlowRateMax)
    ELSE
      WaterSource(SourceNum)%MassFlowRate = MIN(WaterSource(SourceNum)%MassFlowRate, &
                                                WaterSource(SourceNum)%EMSOverrideValueMassFlowRateMax)
    ENDIF
  END IF

  CALL SetComponentFlowRate( WaterSource(SourceNum)%MassFlowRate,         &
                             WaterSource(SourceNum)%InletNodeNum,         &
                             WaterSource(SourceNum)%OutletNodeNum,        &
                             WaterSource(SourceNum)%Location%LoopNum,     &
                             WaterSource(SourceNum)%Location%LoopSideNum, &
                             WaterSource(SourceNum)%Location%BranchNum,   &
                             WaterSource(SourceNum)%Location%CompNum)

   ! at this point the mass flow rate, inlet temp, and boundary temp structure vars have been updated
   ! the calc routine will update the outlet temp and heat transfer rate/energies

END SUBROUTINE InitWaterSource

SUBROUTINE SizeWaterSource(SourceNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing water source design flow rate

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rate from the plant sizing array.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,             ONLY: PlantLoop, PlantSizesOkayToFinalize
  USE PlantUtilities,        ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager,   ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties,       ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SourceNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  REAL(r64)           :: tmpVolFlowRate ! local design volume flow rate

  PltSizNum = 0
  ErrorsFound = .FALSE.
  tmpVolFlowRate = WaterSource(SourceNum)%DesVolFlowRate

  PltSizNum = PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%PlantSizNum

  IF (WaterSource(SourceNum)%DesVolFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpVolFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate !* WaterSource(SourceNum)%SizFac
        IF (PlantSizesOkayToFinalize) WaterSource(SourceNum)%DesVolFlowRate = tmpVolFlowRate
      ELSE
        tmpVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize)  WaterSource(SourceNum)%DesVolFlowRate = tmpVolFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('PlantComponent:TemperatureSource', WaterSource(SourceNum)%Name, &
                              'Design Fluid Flow Rate [m3/s]', WaterSource(SourceNum)%DesVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of plant component temperature source flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in PlantComponent:TemperatureSource object='//TRIM(WaterSource(SourceNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(WaterSource(SourceNum)%InletNodeNum,WaterSource(SourceNum)%DesVolFlowRate)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeWaterSource

SUBROUTINE CalcWaterSource(SourceNum,MyLoad,Runflag,EquipFlowCtrl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   October 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals,            ONLY : SecInHour
  USE DataHVACGlobals,        ONLY : TimeStepSys
  USE DataPlant,              ONLY : PlantLoop
  USE FluidProperties,        ONLY : GetSpecificHeatGlycol

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: SourceNum
  REAL(r64), INTENT(IN) :: MyLoad
  LOGICAL,   INTENT(IN) :: RunFlag
  INTEGER,   INTENT(IN) :: EquipFlowCtrl  ! Flow control mode for the equipment

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CalcWaterSource'

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: InletNode
  INTEGER                :: OutletNode
  INTEGER                :: LoopNum
  INTEGER                :: LoopSideNum
  REAL(r64)              :: Cp
  REAL(r64) :: rDummy
  INTEGER :: iDummy
  LOGICAL :: lDummy

  rDummy = MyLoad
  iDummy = EquipFlowCtrl
  lDummy = RunFlag

    IF (WaterSource(SourceNum)%MassFlowRate > 0.0d0) THEN
        WaterSource(SourceNum)%OutletTemp = WaterSource(SourceNum)%BoundaryTemp
        Cp = GetSpecificHeatGlycol(PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%FluidName,  &
                                   WaterSource(SourceNum)%BoundaryTemp,                           &
                                   PlantLoop(WaterSource(SourceNum)%Location%LoopNum)%FluidIndex, &
                                   RoutineName)
        WaterSource(SourceNum)%HeatRate   = WaterSource(SourceNum)%MassFlowRate * Cp *   &
           (WaterSource(SourceNum)%OutletTemp - WaterSource(SourceNum)%InletTemp)
        WaterSource(SourceNum)%HeatEnergy = WaterSource(SourceNum)%HeatRate*TimeStepSys*SecInHour
    ELSE
        WaterSource(SourceNum)%OutletTemp = WaterSource(SourceNum)%BoundaryTemp
        WaterSource(SourceNum)%HeatRate   = 0.0d0
        WaterSource(SourceNum)%HeatEnergy = 0.0d0
    END IF

RETURN
END SUBROUTINE CalcWaterSource

SUBROUTINE UpdateWaterSource(SourceNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:


            ! METHODOLOGY EMPLOYED:
            ! REFERENCES:

            ! USE STATEMENTS:
!USE DataGlobals,     ONLY: SecInHour
!USE DataHVACGlobals, ONLY: TimeStepSys

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: SourceNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: InletNode
  INTEGER                  :: OutletNode

  InletNode            =  WaterSource(SourceNum)%InletNodeNum
  OutletNode           =  WaterSource(SourceNum)%OutletNodeNum


          !set outlet node temperatures
    Node(OutletNode)%Temp     = WaterSource(SourceNum)%OutletTemp

RETURN
END SUBROUTINE UpdateWaterSource

! End of Record Keeping subroutines for the Const COP Chiller Module
! *****************************************************************************


END MODULE PlantComponentTemperatureSources



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
