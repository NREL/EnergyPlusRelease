!note there are routines that lie outside of the Module at the end of this file

MODULE EMSManager

        ! MODULE INFORMATION:
        !       AUTHOR         Peter Graham Ellis
        !       DATE WRITTEN   June 2006
        !       MODIFIED       Brent Griffith
        !                      May - August 2009
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS MODULE:
        ! This module manages the programmable energy management system(EMS).

        ! METHODOLOGY EMPLOYED:
        !

        ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataInterfaces, ONLY: GetInternalVariableValue, ShowFatalError, ShowWarningError, ShowContinueError
USE DataRuntimeLanguage

IMPLICIT NONE ! Enforce explicit typing of all variables

          ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER, PUBLIC :: iTemperatureSetpoint      = 101 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iTemperatureMinSetpoint   = 102 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iTemperatureMaxSetpoint   = 103 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iHumidityRatioSetpoint    = 104 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iHumidityRatioMinSetpoint = 105 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iHumidityRatioMaxSetpoint = 106 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iMassFlowRateSetpoint     = 107 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iMassFlowRateMinSetpoint  = 108 ! integer for node setpoint control type
INTEGER, PARAMETER, PUBLIC :: iMassFlowRateMaxSetpoint  = 109 ! integer for node setpoint control type

PRIVATE ! Everything private unless explicitly made public

          ! DERIVED TYPE DEFINITIONS:

          ! MODULE VARIABLE TYPE DECLARATIONS:

          ! MODULE VARIABLE DECLARATIONS:
LOGICAL, SAVE :: GetEMSUserInput = .TRUE.  ! Flag to prevent input from being read multiple times
Logical, SAVE :: ZoneThermostatActuatorsHaveBeenSetup = .FALSE. 
LOGICAL, SAVE :: FinishProcessingUserInput = .TRUE. ! Flag to indicate still need to process input

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC CheckIfAnyEMS
PUBLIC ManageEMS
PRIVATE ReportEMS
PRIVATE GetEMSInput
PRIVATE InitEMS
PRIVATE ProcessEMSInput
PRIVATE SetupPrimaryAirSystemAvailMgrAsActuators
PRIVATE SetupNodeSetpointsAsActuators
PRIVATE SetupZoneInfoAsInternalDataAvail
PRIVATE SetupWindowShadingControlActuators
PRIVATE SetupThermostatActuators
PRIVATE SetupSurfaceConvectionActuators
PRIVATE SetupSurfaceOutdoorBoundaryConditionActuators
PRIVATE GetVariableTypeAndIndex
PRIVATE EchoOutActuatorKeyChoices
PRIVATE EchoOutInternalVariableChoices
PUBLIC UpdateEMSTrendVariables
PUBLIC CheckIfNodeSetpointManagedByEMS

CONTAINS

SUBROUTINE CheckIFAnyEMS

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2009
          !       MODIFIED       Rui Zhang February 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determine if EMS is used in model and set flag
          ! This needs to be checked early so calls to SetupEMSActuator
          ! can be avoided if there is no EMS in model.
          ! We cannot do error checking during the full get input until later in the simulation.

          ! METHODOLOGY EMPLOYED:
          ! Get number of EMS-related input objects and set
          ! global logical AnyEnergyManagementSystemInModel

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE DataIPShortcuts
  USE InputProcessor , ONLY: GetNumObjectsFound
  USE DataGlobals ,    ONLY: AnyEnergyManagementSystemInModel
  USE General,         ONLY: ScanForReports

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

  INTEGER, EXTERNAL :: GetNewUnitNumber
  INTEGER :: write_stat
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject

  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
  NumSensors = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:Actuator'
  numActuatorsUsed = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:ProgramCallingManager'
  NumProgramCallManagers = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:Program'
  NumErlPrograms = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:Subroutine'
  NumErlSubroutines = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:GlobalVariable'
  NumUserGlobalVariables = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:OutputVariable'
  NumEMSOutputVariables = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:MeteredOutputVariable'
  NumEMSMeteredOutputVariables= GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:CurveOrTableIndexVariable'
  NumEMSCurveIndices   = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'ExternalInterface:Variable'
  NumExternalInterfaceGlobalVariables = GetNumObjectsFound(cCurrentModuleObject)

  ! added for FMUImport
  cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitImport:To:Variable'
  NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables = GetNumObjectsFound(cCurrentModuleObject)

  ! added for FMUExport
  cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitExport:To:Variable'
  NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'ExternalInterface:Actuator'
  NumExternalInterfaceActuatorsUsed = GetNumObjectsFound(cCurrentModuleObject)

  ! added for FMUImport
  cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitImport:To:Actuator'
  NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed = GetNumObjectsFound(cCurrentModuleObject)

  ! added for FMUExport
  cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitExport:To:Actuator'
  NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'EnergyManagementSystem:ConstructionIndexVariable'
  NumEMSConstructionIndices = GetNumObjectsFound(cCurrentModuleObject)

  ! added for FMU
  IF ((NumSensors + numActuatorsUsed + NumProgramCallManagers + NumErlPrograms + NumErlSubroutines &
      + NumUserGlobalVariables + NumEMSOutputVariables + NumEMSCurveIndices &
      + NumExternalInterfaceGlobalVariables + NumExternalInterfaceActuatorsUsed &
      + NumEMSConstructionIndices + NumEMSMeteredOutputVariables + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
      + NumExternalInterfaceFunctionalMockupUnitImportGlobalVariables   &
      + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed &
      + NumExternalInterfaceFunctionalMockupUnitExportGlobalVariables) > 0 ) THEN
    AnyEnergyManagementSystemInModel = .TRUE.
  ELSE
    AnyEnergyManagementSystemInModel = .FALSE.
  ENDIF

  IF (AnyEnergyManagementSystemInModel)  THEN

    CALL ScanForReports('EnergyManagementSystem', OutputEDDFile)
    IF (OutputEDDFile) THEN
      ! open up output file for EMS EDD file  EMS Data and Debug
      OutputEMSFileUnitNum=GetNewUnitNumber()
      OPEN (OutputEMSFileUnitNum,FILE='eplusout.edd',ACTION='write',IOSTAT=write_stat)
      IF (write_stat /= 0) THEN
       CALL ShowFatalError('CheckIFAnyEMS: Could not open file "eplusout.edd" for output (write).')
      ENDIF
    ENDIF
  ELSE
    CALL ScanForReports('EnergyManagementSystem', OutputEDDFile)
    IF (OutputEDDFile) THEN
      CALL ShowWarningError('CheckIFAnyEMS: No EnergyManagementSystem has been set up in the input file but'//  &
         ' output is requested.')
      CALL ShowContinueError('No EDD file will be produced. Refer to EMS Application Guide and/or InputOutput Reference'//  &
         ' to set up your EnergyManagementSystem.')
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckIFAnyEMS

          ! MODULE SUBROUTINES:
SUBROUTINE ManageEMS(iCalledFrom, ProgramManagerToRun)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  Brent Griffith, April 2009
          !                      added calling point argument and logic.
          !                      Collapsed SimulateEMS into this routine

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: WarmupFlag, DoingSizing, ZoneTSReporting, HVACTSReporting, &
                         KickOffSimulation, AnyEnergyManagementSystemInModel, BeginEnvrnFlag, &
                         emsCallFromSetupSimulation, emsCallFromExternalInterface, emsCallFromBeginNewEvironment, &
                         emsCallFromUserDefinedComponentModel
  USE DataInterfaces, ONLY: ShowFatalError

  USE RuntimeLanguageProcessor, ONLY: EvaluateStack, BeginEnvrnInitializeRuntimeLanguage
  USE OutputProcessor, ONLY: MeterType, NumEnergyMeters, EnergyMeters, RealVariables, RealVariableType, NumOfRVariable, RVar, &
    RVariableTypes

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: iCalledFrom ! indicates where subroutine was called from, parameters in DataGlobals.
  INTEGER, INTENT (IN), OPTIONAL :: ProgramManagerToRun  ! specific program manager to run

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER  :: ErlVariableNum     ! local index
  INTEGER  :: ProgramManagerNum  ! local index and loop
  INTEGER  :: ErlProgramNum      ! local index
  INTEGER  :: ActuatorUsedLoop   ! local loop
  INTEGER  :: EMSActuatorVariableNum
  TYPE(ErlValueType)        :: ReturnValue  ! local Erl value structure
  LOGICAL  :: AnyProgramRan  ! local logical
  INTEGER  :: tmpInteger
!  INTEGER  :: ProgramNum

  ! FLOW:
  IF ( .NOT. AnyEnergyManagementSystemInModel) RETURN ! quick return if nothing to do

  IF (iCalledFrom == emsCallFromBeginNewEvironment) CALL BeginEnvrnInitializeRuntimeLanguage

  CALL InitEMS(iCalledFrom)

  IF (iCalledFrom == emsCallFromSetupSimulation) THEN
    Call ProcessEMSInput(.TRUE.)
    RETURN
  ENDIF

 ! Run the Erl programs depending on calling point.
  AnyProgramRan = .FALSE.
  IF (iCalledFrom /= emsCallFromUserDefinedComponentModel) THEN
    DO ProgramManagerNum = 1, NumProgramCallManagers

      IF (EMSProgramCallManager(ProgramManagerNum)%CallingPoint == iCalledFrom) THEN
        DO ErlProgramNum = 1, EMSProgramCallManager(ProgramManagerNum)%NumErlPrograms
          ReturnValue = EvaluateStack(EMSProgramCallManager(ProgramManagerNum)%ErlProgramARR(ErlProgramNum))
          AnyProgramRan = .TRUE.
        ENDDO
      ENDIF
    ENDDO
  ELSE ! call specific program manager
    IF (PRESENT(ProgramManagerToRun)) THEN
      DO ErlProgramNum = 1, EMSProgramCallManager(ProgramManagerToRun)%NumErlPrograms
          ReturnValue = EvaluateStack(EMSProgramCallManager(ProgramManagerToRun)%ErlProgramARR(ErlProgramNum))
          AnyProgramRan = .TRUE.
      ENDDO
    ENDIF
  ENDIF

  IF (iCalledFrom == emsCallFromExternalInterface) THEN
     AnyProgramRan = .TRUE.
  ENDIF

  IF (.NOT. AnyProgramRan) RETURN

    ! Set actuated variables with new values
  DO ActuatorUsedLoop = 1, numActuatorsUsed + NumExternalInterfaceActuatorsUsed &
                              + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
                              + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed
    ErlVariableNum = EMSActuatorUsed(ActuatorUsedLoop)%ErlVariableNum
    IF (.NOT. (ErlVariableNum >0)) CYCLE ! this can happen for good reason during sizing

    EMSActuatorVariableNum = EMSActuatorUsed(ActuatorUsedLoop)%ActuatorVariableNum
    IF (.NOT. (EMSActuatorVariableNum >0)) CYCLE ! this can happen for good reason during sizing

    IF (ErlVariable(ErlVariableNum)%Value%Type == ValueNull) THEN
      EMSActuatorAvailable(EMSActuatorVariableNum)%Actuated = .FALSE.
    ELSE
      ! Set the value and the actuated flag remotely on the actuated object via the pointer
      SELECT CASE (EMSActuatorAvailable(EMSActuatorVariableNum)%PntrVarTypeUsed)

      CASE (PntrReal)
        EMSActuatorAvailable(EMSActuatorVariableNum)%Actuated = .TRUE.
        EMSActuatorAvailable(EMSActuatorVariableNum)%RealValue = ErlVariable(ErlVariableNum)%Value%Number
      CASE (PntrInteger)
        EMSActuatorAvailable(EMSActuatorVariableNum)%Actuated = .TRUE.
        tmpInteger = FLOOR(ErlVariable(ErlVariableNum)%Value%Number)
        EMSActuatorAvailable(EMSActuatorVariableNum)%IntValue = tmpInteger
      CASE (PntrLogical)
        EMSActuatorAvailable(EMSActuatorVariableNum)%Actuated = .TRUE.
        IF (ErlVariable(ErlVariableNum)%Value%Number == 0.0D0) THEN
          EMSActuatorAvailable(EMSActuatorVariableNum)%LogValue = .FALSE.
        ELSEIF (ErlVariable(ErlVariableNum)%Value%Number == 1.0D0) THEN
          EMSActuatorAvailable(EMSActuatorVariableNum)%LogValue = .TRUE.
        ELSE
          EMSActuatorAvailable(EMSActuatorVariableNum)%LogValue = .FALSE.
        ENDIF

      CASE DEFAULT

      END SELECT
    END IF

  END DO

  CALL ReportEMS

  RETURN

END SUBROUTINE ManageEMS

SUBROUTINE InitEMS (iCalledFrom)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! collect routines needed to initialize EMS

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: WarmupFlag, DoingSizing, KickOffSimulation, BeginEnvrnFlag,  &
                         emsCallFromZoneSizing, emsCallFromSystemSizing, emsCallFromUserDefinedComponentModel
  USE DataInterfaces, ONLY: ShowFatalError
  USE RuntimeLanguageProcessor, ONLY: InitializeRuntimeLanguage, SetErlValueNumber
  USE ScheduleManager,  ONLY : GetCurrentScheduleValue
  USE DataZoneControls, ONLY: GetZoneAirStatsInputFlag

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: iCalledFrom ! indicates where subroutine was called from, parameters in DataGlobals.


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER  :: InternalVarUsedNum ! local index and loop
  INTEGER  :: InternVarAvailNum  ! local index
  INTEGER  :: SensorNum ! local loop and index
  INTEGER  :: ErlVariableNum ! local index
  REAL(r64)  :: tmpReal ! temporary local integer

  IF (GetEMSUserInput) THEN
    CALL SetupZoneInfoAsInternalDataAvail
    CALL SetupWindowShadingControlActuators
    CALL SetupSurfaceConvectionActuators
    CALL SetupSurfaceConstructionActuators
    CALL SetupSurfaceOutdoorBoundaryConditionActuators
    CALL GetEMSInput
    GetEMSUserInput = .FALSE.
  ENDIF

  IF (.NOT. GetZoneAirStatsInputFlag .AND. .NOT. ZoneThermostatActuatorsHaveBeenSetup) THEN
    CALL SetupThermostatActuators
    ZoneThermostatActuatorsHaveBeenSetup = .TRUE.
  ENDIF

  ! need to delay setup of HVAC actuator until after the systems input has been processed (if present)
  IF (FinishProcessingUserInput .AND. .NOT. DoingSizing .AND. .NOT. KickOffSimulation) THEN !
    CALL SetupNodeSetpointsAsActuators
    CALL SetupPrimaryAirSystemAvailMgrAsActuators
!    CALL SetupWindowShadingControlActuators !this is too late for including in sizing, moved to GetEMSUserInput
!    CALL SetupThermostatActuators !this is too late for including in sizing, moved to GetEMSUserInput
!    CALL SetupSurfaceConvectionActuators !this is too late for including in sizing, moved to GetEMSUserInput
    FinishProcessingUserInput = .FALSE.
  END IF

  CALL InitializeRuntimeLanguage

  IF ((BeginEnvrnFlag) .OR. (iCalledFrom == emsCallFromZoneSizing) .OR. (iCalledFrom == emsCallFromSystemSizing) &
      .OR. (iCalledFrom == emsCallFromUserDefinedComponentModel) )  THEN

    ! another pass at trying to setup input data.
    IF (FinishProcessingUserInput) CALL ProcessEMSInput(.FALSE.)

    ! update internal data variables being used by Erl
    DO InternalVarUsedNum = 1, NumInternalVariablesUsed
      ErlVariableNum = EMSInternalVarsUsed(InternalVarUsedNum)%ErlVariableNum
      InternVarAvailNum  =  EMSInternalVarsUsed(InternalVarUsedNum)%InternVarNum
      IF (.NOT. (InternVarAvailNum > 0 )) CYCLE ! sometimes executes before completely finished setting up.
      IF (.NOT. (ErlVariableNum > 0 )) CYCLE

      SELECT CASE (EMSInternalVarsAvailable(InternVarAvailNum)%PntrVarTypeUsed)

      CASE (PntrReal)

          ErlVariable(ErlVariableNum)%Value = SetErlValueNumber(EMSInternalVarsAvailable(InternVarAvailNum)%RealValue)

      CASE (PntrInteger)

          tmpReal = REAL(EMSInternalVarsAvailable(InternVarAvailNum)%IntValue, r64 )
          ErlVariable(ErlVariableNum)%Value = SetErlValueNumber(tmpReal)

      END SELECT

    ENDDO

  ENDIF

  ! Update sensors with current data
  DO SensorNum = 1, NumSensors
    ErlVariableNum = Sensor(SensorNum)%VariableNum
    IF ((ErlVariableNum > 0) .AND. (Sensor(SensorNum)%Index > 0)) THEN
      IF (Sensor(SensorNum)%SchedNum == 0) THEN ! not a schedule so get from output processor

        ErlVariable(ErlVariableNum)%Value =   &
           SetErlValueNumber(GetInternalVariableValue(Sensor(SensorNum)%Type, Sensor(SensorNum)%Index) , &
                                                    OrigValue = ErlVariable(ErlVariableNum)%Value )
      ELSE ! schedule so use schedule service

        ErlVariable(ErlVariableNum)%Value = &
           SetErlValueNumber(GetCurrentScheduleValue(Sensor(SensorNum)%SchedNum), &
                                                    OrigValue = ErlVariable(ErlVariableNum)%Value )
      ENDIF
    ENDIF
  END DO

  RETURN

END SUBROUTINE InitEMS

SUBROUTINE ReportEMS

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE RuntimeLanguageProcessor, ONLY: ReportRuntimeLanguage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  !INTEGER, INTENT(IN) :: ListNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW:
  CALL ReportRuntimeLanguage

  RETURN

END SUBROUTINE ReportEMS


SUBROUTINE GetEMSInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       BG April 2009, finishing, renaming, etc.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the EMS input from the input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength, AnyEnergyManagementSystemInModel, &
                         emsCallFromZoneSizing , emsCallFromSystemSizing , emsCallFromBeginNewEvironment, &
                         emsCallFromBeginNewEvironmentAfterWarmUp , emsCallFromBeginTimestepBeforePredictor, &
                         emsCallFromBeforeHVACManagers, emsCallFromAfterHVACManagers,  emsCallFromHVACIterationLoop, &
                         emsCallFromEndZoneTimestepBeforeZoneReporting, emsCallFromEndZoneTimestepAfterZoneReporting, &
                         emsCallFromEndSystemTimestepBeforeHVACReporting, emsCallFromEndSystemTimestepAfterHVACReporting, &
                         emsCallFromComponentGetInput, emsCallFromUserDefinedComponentModel, emsCallFromUnitarySystemSizing
  USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowFatalError, SetupOutputVariable, ShowContinueError
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, MakeUpperCase, SameString,   &
     GetObjectDefMaxArgs
!  USE OutputProcessor, ONLY: GetReportVarPointerForEMS
!  USE DataIPShortCuts

  USE RuntimeLanguageProcessor, ONLY: InitializeRuntimeLanguage, FindEMSVariable, NewEMSVariable, &
       ExternalInterfaceInitializeErlVariable, SetErlValueNumber

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER                     :: StackNum
  INTEGER                     :: SensorNum
  INTEGER                     :: ActuatorNum
  INTEGER                     :: ActuatorVariableNum
!  INTEGER                     :: ProgramNum
  INTEGER                     :: VariableNum  ! local do loop index
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: AlphaNum
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
!  CHARACTER(len=MaxNameLength), DIMENSION(99) :: AlphArray  ! Character string data  ! 99 should really be some kind of constant
!  REAL(r64), DIMENSION(1)          :: NumArray  ! Numeric data
  LOGICAL                     :: IsNotOK   ! Flag to verify name
  LOGICAL                     :: IsBlank   ! Flag for blank name
  LOGICAL                     :: ErrorsFound = .FALSE.
  INTEGER, EXTERNAL           :: GetMeterIndex
!  CHARACTER(len=MaxNameLength)   :: objNameMsg = ' '
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  INTEGER :: VarType
  INTEGER :: VarIndex
  LOGICAL :: FoundObjectType
  LOGICAL :: FoundObjectName
  LOGICAL :: FoundActuatorName
  INTEGER :: NumErlProgramsThisManager ! temporary size of Erl programs in EMSProgramCallManager
  INTEGER :: ManagerProgramNum ! index counter for Erl programs inside EMSProgramCallManager
  INTEGER :: CallManagerNum ! loop counter for EMSProgramCallManager structure
  INTEGER :: InternVarNum ! do loop counter for internal variables used (outer)
  INTEGER :: InternalVarAvailNum ! do loop counter for internal variables available (inner)
  INTEGER :: Loop ! do loop counter
  INTEGER :: MaxNumAlphas = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER :: MaxNumNumbers = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER :: TotalArgs = 0 !argument for call to GetObjectDefMaxArgs
  LOGICAL :: errFlag

          ! FLOW:
  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=NumNums
  MaxNumAlphas=NumAlphas
  cCurrentModuleObject = 'EnergyManagementSystem:Actuator'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:ProgramCallingManager'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:Program'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:Subroutine'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:OutputVariable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'ExternalInterface:Variable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'ExternalInterface:Actuator'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
!  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
!  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
!  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
!  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'EnergyManagementSystem:GlobalVariable'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

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

  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
  IF (NumSensors > 0) THEN
    ALLOCATE(Sensor(NumSensors))

    DO SensorNum = 1, NumSensors
      CALL GetObjectItem(cCurrentModuleObject, SensorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOSTAT ,&
                      AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), Sensor%Name, SensorNum - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF

      CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),errFlag,ErrorsFound)
      IF (.not. errFlag) THEN
        Sensor(SensorNum)%Name = Trim(cAlphaArgs(1))

        ! really needs to check for conflicts with program and function names too...done later
        VariableNum = FindEMSVariable(cAlphaArgs(1), 0)

        IF (VariableNum > 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Object name conflicts with a global variable name in EMS')
          ErrorsFound = .TRUE.
        ELSE
          VariableNum = NewEMSVariable(cAlphaArgs(1), 0)
          Sensor(SensorNum)%VariableNum = VariableNum
        END IF
      END IF

      IF (cAlphaArgs(2) == '*') cAlphaArgs(2)=blank
      Sensor(SensorNum)%UniqueKeyName = cAlphaArgs(2)
      Sensor(SensorNum)%OutputVarName = cAlphaArgs(3)

      VarIndex = GetMeterIndex(cAlphaArgs(3))
      IF (VarIndex > 0) THEN
        IF (.NOT. lAlphaFieldBlanks(2) ) THEN
          CALL ShowWarningError('Unused'//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Meter Name found; Key Name will be ignored')  ! why meters have no keys..
        ELSE
          Sensor(SensorNum)%Type = 3
          Sensor(SensorNum)%Index = VarIndex
          Sensor(SensorNum)%CheckedOkay = .TRUE.
        END IF
      ELSE
        ! Search for variable names
        CALL GetVariableTypeAndIndex(cAlphaArgs(3), cAlphaArgs(2), VarType, VarIndex)
        IF (VarType /= 0) THEN
          Sensor(SensorNum)%Type = VarType
          IF (VarIndex /= 0) THEN
            Sensor(SensorNum)%Index = VarIndex
            Sensor(SensorNum)%CheckedOkay = .TRUE.
          ENDIF
        END IF
      END IF

    END DO  ! SensorNum
  END IF

  cCurrentModuleObject = 'EnergyManagementSystem:Actuator'

  IF (numActuatorsUsed + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
                                                    + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed> 0) THEN
    ALLOCATE(EMSActuatorUsed(numActuatorsUsed + NumExternalInterfaceActuatorsUsed &
                              + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
                              + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed))
    DO ActuatorNum = 1, numActuatorsUsed + NumExternalInterfaceActuatorsUsed &
                            + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
                            + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed
       ! If we process the ExternalInterface actuators, all we need to do is to change the
       ! name of the module object, and shift the ActuatorNum in GetObjectItem
       IF ( ActuatorNum <= numActuatorsUsed ) THEN
         CALL GetObjectItem(cCurrentModuleObject, ActuatorNum, cAlphaArgs, NumAlphas, rNumericArgs, &
               NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
               AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
       ELSE IF ( ActuatorNum > numActuatorsUsed .AND. ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed) THEN
         cCurrentModuleObject = 'ExternalInterface:Actuator'
         CALL GetObjectItem(cCurrentModuleObject, ActuatorNum-numActuatorsUsed, cAlphaArgs, NumAlphas, rNumericArgs, &
               NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
               AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
       ELSE IF ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed .AND.   &
                 ActuatorNum <= (numActuatorsUsed + NumExternalInterfaceActuatorsUsed +   &
                                 NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed) ) THEN
         cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitImport:To:Actuator'
         CALL GetObjectItem(cCurrentModuleObject, ActuatorNum-numActuatorsUsed-NumExternalInterfaceActuatorsUsed, &
                           cAlphaArgs, NumAlphas, rNumericArgs, &
                           NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                           AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        ELSE IF ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed &
                              + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed .AND. ActuatorNum <= numActuatorsUsed &
                              + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
                              + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed) THEN
         cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitExport:To:Actuator'
         CALL GetObjectItem(cCurrentModuleObject, ActuatorNum-numActuatorsUsed-NumExternalInterfaceActuatorsUsed &
                           -NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed, &
                           cAlphaArgs, NumAlphas, rNumericArgs, &
                           NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                           AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
       END IF

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), EMSActuatorUsed%Name, ActuatorNum - 1, IsNotOK, IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF

      CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),errFlag,ErrorsFound)
      IF (.not. errFlag) THEN
        EMSActuatorUsed(ActuatorNum)%Name = cAlphaArgs(1)

        ! really needs to check for conflicts with program and function names too...
        VariableNum = FindEMSVariable(cAlphaArgs(1), 0)

        IF (VariableNum > 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Object name conflicts with a global variable name in EMS')
          ErrorsFound = .TRUE.
        ELSE
          VariableNum = NewEMSVariable(cAlphaArgs(1), 0)
          EMSActuatorUsed(ActuatorNum)%ErlVariableNum = VariableNum
          IF ( ActuatorNum > numActuatorsUsed ) THEN
             ! Initialize variables for the ExternalInterface variables
             CALL ExternalInterfaceInitializeErlVariable( VariableNum, &
                  SetErlValueNumber(rNumericArgs(1)), lNumericFieldBlanks(1) )
          ENDIF
        END IF
      END IF

      ! need to store characters to finish processing later (once available Actuators have all been setup)
      EMSActuatorUsed(ActuatorNum)%ComponentTypeName = cAlphaArgs(3)
      EMSActuatorUsed(ActuatorNum)%UniqueIDName      = cAlphaArgs(2)
      EMSActuatorUsed(ActuatorNum)%ControlTypeName   = cAlphaArgs(4)

      FoundObjectType = .FALSE.
      FoundObjectName = .FALSE.
      FoundActuatorName = .FALSE.
      DO ActuatorVariableNum = 1, numEMSActuatorsAvailable
        IF (SameString(EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName , cAlphaArgs(3))) THEN
          FoundObjectType = .TRUE.
          IF (SameString(EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName , cAlphaArgs(2))) THEN
            FoundObjectName = .TRUE.
            IF (SameString(EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName , cAlphaArgs(4))) THEN
              FoundActuatorName = .TRUE.
              EXIT
            END IF
          END IF
        END IF
      END DO

      IF (FoundActuatorName) THEN
        EMSActuatorUsed(ActuatorNum)%ActuatorVariableNum = ActuatorVariableNum
        EMSActuatorUsed(ActuatorNum)%CheckedOkay = .TRUE.
      END IF
    END DO  ! ActuatorNum
  END IF

  cCurrentModuleObject = 'EnergyManagementSystem:InternalVariable'
  NumInternalVariablesUsed  = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumInternalVariablesUsed > 0 ) THEN
    ALLOCATE(EMSInternalVarsUsed(NumInternalVariablesUsed))

    DO InternVarNum = 1, NumInternalVariablesUsed
      CALL GetObjectItem(cCurrentModuleObject, InternVarNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, &
                      IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), EMSInternalVarsUsed%Name, InternVarNum - 1, IsNotOK, &
                  IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF

      CALL ValidateEMSVariableName(cCurrentModuleObject,cAlphaArgs(1),cAlphaFieldNames(1),errFlag,ErrorsFound)
      IF (.not. errFlag) THEN
        EMSInternalVarsUsed(InternVarNum)%Name = cAlphaArgs(1)
        VariableNum = FindEMSVariable(cAlphaArgs(1), 0)
        IF (VariableNum > 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Object name conflicts with a global variable name in EMS')
          ErrorsFound = .TRUE.
        ELSE
          VariableNum = NewEMSVariable(cAlphaArgs(1), 0)
          EMSInternalVarsUsed(InternVarNum)%ErlVariableNum = VariableNum
        END IF

        EMSInternalVarsUsed(InternVarNum)%UniqueIDName         = cAlphaArgs(2)
        EMSInternalVarsUsed(InternVarNum)%InternalDataTypeName = cAlphaArgs(3)

        FoundObjectType = .FALSE.
        FoundObjectName = .FALSE.
        DO InternalVarAvailNum = 1, numEMSInternalVarsAvailable
          IF (SameString(EMSInternalVarsAvailable(InternalVarAvailNum)%DataTypeName , cAlphaArgs(3))) THEN
            FoundObjectType = .TRUE.
            IF (SameString(EMSInternalVarsAvailable(InternalVarAvailNum)%UniqueIDName , cAlphaArgs(2))) THEN
              FoundObjectName = .TRUE.
              EXIT ! InternalVarAvailNum now holds needed index pointer
            END IF
          END IF
        END DO

        IF (FoundObjectName) THEN
          EMSInternalVarsUsed(InternVarNum)%InternVarNum = InternalVarAvailNum
          EMSInternalVarsUsed(InternVarNum)%CheckedOkay  = .TRUE.
        ENDIF

      ENDIF
    ENDDO
  ENDIF

  CALL InitializeRuntimeLanguage  ! Loads built-in globals and functions, then performs GetInput for runtime language objects

  IF (NumProgramCallManagers > 0) THEN
    cCurrentModuleObject = 'EnergyManagementSystem:ProgramCallingManager'
    ALLOCATE(EMSProgramCallManager(NumProgramCallManagers))

    DO CallManagerNum = 1, NumProgramCallManagers

      CALL GetObjectItem(cCurrentModuleObject, CallManagerNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, &
                      IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
                      AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), EMSProgramCallManager%Name, CallManagerNum - 1, &
                        IsNotOK, IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      EMSProgramCallManager(CallManagerNum)%Name = cAlphaArgs(1)

      SELECT CASE (TRIM(cAlphaArgs(2)))

      CASE ('BEGINNEWENVIRONMENT')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromBeginNewEvironment
      CASE ('AFTERNEWENVIRONMENTWARMUPISCOMPLETE')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromBeginNewEvironmentAfterWarmUp
      CASE ('BEGINTIMESTEPBEFOREPREDICTOR')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromBeginTimestepBeforePredictor
      CASE ('AFTERPREDICTORBEFOREHVACMANAGERS')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromBeforeHVACManagers
      CASE ('AFTERPREDICTORAFTERHVACMANAGERS')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromAfterHVACManagers
      CASE ('INSIDEHVACSYSTEMITERATIONLOOP')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromHVACIterationLoop
      CASE ('ENDOFZONETIMESTEPBEFOREZONEREPORTING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromEndZoneTimestepBeforeZoneReporting
      CASE ('ENDOFZONETIMESTEPAFTERZONEREPORTING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromEndZoneTimestepAfterZoneReporting
      CASE ('ENDOFSYSTEMTIMESTEPBEFOREHVACREPORTING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromEndSystemTimestepBeforeHVACReporting
      CASE ('ENDOFSYSTEMTIMESTEPAFTERHVACREPORTING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromEndSystemTimestepAfterHVACReporting
      CASE ('ENDOFZONESIZING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromZoneSizing
      CASE ('ENDOFSYSTEMSIZING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromSystemSizing
      CASE ('AFTERCOMPONENTINPUTREADIN')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromComponentGetInput
      CASE ('USERDEFINEDCOMPONENTMODEL')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromUserDefinedComponentModel
      CASE ('UNITARYSYSTEMSIZING')
        EMSProgramCallManager(CallManagerNum)%CallingPoint = emsCallFromUnitarySystemSizing
      CASE DEFAULT
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound = .TRUE.
      END SELECT

      NumErlProgramsThisManager = NumAlphas - 2
      EMSProgramCallManager(CallManagerNum)%NumErlPrograms = NumErlProgramsThisManager
      ALLOCATE(EMSProgramCallManager(CallManagerNum)%ErlProgramARR(NumErlProgramsThisManager) )
      ManagerProgramNum = 0
      DO AlphaNum = 3, NumAlphas
        ! find program name in Stack structure
        IF (lAlphaFieldBlanks(AlphaNum)) THEN ! throw error
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(AlphaNum))//'='//TRIM(cAlphaArgs(AlphaNum)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program names cannot be blank')
          ErrorsFound = .TRUE.
        ENDIF

        StackNum = FindItemInList(cAlphaArgs(AlphaNum), ErlStack%Name, NumErlStacks)

        IF (StackNum > 0) THEN ! found it
          ! check for duplicate and warn.
          DO Loop = 1, ManagerProgramNum
            IF (EMSProgramCallManager(CallManagerNum)%ErlProgramARR(Loop) == StackNum) THEN
              CALL ShowWarningError('Duplicate '//TRIM(cAlphaFieldNames(AlphaNum))//'='//TRIM(cAlphaArgs(AlphaNum)))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Erl program appears more than once, and the simulation continues.')
            END IF
          END DO

          ManagerProgramNum = ManagerProgramNum + 1

          EMSProgramCallManager(CallManagerNum)%ErlProgramARR(ManagerProgramNum) = StackNum

        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(AlphaNum))//'='//TRIM(cAlphaArgs(AlphaNum)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('Program Name not found.')
          ErrorsFound = .TRUE.
        END IF
      END DO  ! AlphaNum

    ENDDO

  ELSE ! no program calling manager in input
    IF (NumErlPrograms > 0) THEN
      cCurrentModuleObject = 'EnergyManagementSystem:ProgramCallingManager'
      CALL ShowWarningError('Energy Management System is missing input object '//TRIM(cCurrentModuleObject))
      CALL ShowContinueError('EnergyPlus Runtime Language programs need a calling manager to control when they get executed')

    ENDIF

  END IF

  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lNumericFieldBlanks)


  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting Energy Management System input. Preceding condition causes termination.')
  END IF

  RETURN

END SUBROUTINE GetEMSInput

SUBROUTINE ProcessEMSInput(reportErrors)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! contains Some input checks that need to be deferred until later in the simulation

          ! METHODOLOGY EMPLOYED:
          ! Loop over objects doing input checks.
          ! Had to break up get user input into two phases because
          ! the actuators can't be set up until all the HVAC systems are read in, sized, etc.
          ! but we also want to allow customizing sizing calcs which occur much earlier in the simulation.
          !  so here we do a final pass and throw the errors that would usually occur during get input.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE DataIPShortcuts, ONLY: cCurrentModuleObject
  USE InputProcessor,  ONLY: SameString
  USE DataInterfaces,  ONLY: ShowSevereError, ShowWarningError, ShowContinueError, ShowFatalError
  USE RuntimeLanguageProcessor, ONLY: BeginEnvrnInitializeRuntimeLanguage
  USE ScheduleManager,   ONLY: GetScheduleIndex
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)  :: reportErrors !.  If true, then report out errors ,otherwise setup what we can

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SensorNum ! local loop
!  INTEGER :: VariableNum  ! local do loop index
  INTEGER :: VarIndex
  INTEGER, EXTERNAL           :: GetMeterIndex
  INTEGER :: VarType
  LOGICAL                     :: ErrorsFound = .FALSE.
  INTEGER                     :: ActuatorNum
  LOGICAL :: FoundObjectType
  LOGICAL :: FoundObjectName
  LOGICAL :: FoundActuatorName
  INTEGER :: ActuatorVariableNum
  INTEGER :: InternVarNum  ! local do loop index
  INTEGER :: InternalVarAvailNum ! local do loop index
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject

  cCurrentModuleObject = 'EnergyManagementSystem:Sensor'
  DO SensorNum = 1, NumSensors
    IF (Sensor(SensorNum)%CheckedOkay) Cycle

    ! try again to process sensor.
    VarIndex = GetMeterIndex(Sensor(SensorNum)%OutputVarName)
    IF (VarIndex > 0) THEN

      Sensor(SensorNum)%Type = 3
      Sensor(SensorNum)%Index = VarIndex

    ELSE
      ! Search for variable names
      CALL GetVariableTypeAndIndex(Sensor(SensorNum)%OutputVarName, Sensor(SensorNum)%UniqueKeyName, VarType, VarIndex)
      IF (VarType == 0) THEN
        IF (reportErrors) THEN
          CALL ShowSevereError('Invalid Output:Variable or Output:Meter Name ='//TRIM(Sensor(SensorNum)%OutputVarName) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(Sensor(SensorNum)%Name) )
          CALL ShowContinueError('Output:Variable Name not found')
          ErrorsFound = .TRUE.
        ENDIF
      ELSE IF  (VarIndex == 0) THEN
        IF (reportErrors) THEN
          CALL ShowSevereError('Invalid Output:Variable or Output:Meter Index Key Name ='//TRIM(Sensor(SensorNum)%UniqueKeyName))
          CALL ShowContinueError('For Output:Variable or Output:Meter = '//TRIM(Sensor(SensorNum)%OutputVarName))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(Sensor(SensorNum)%Name))
          CALL ShowContinueError('Unique Key Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ELSE
        Sensor(SensorNum)%Type = VarType
        Sensor(SensorNum)%Index = VarIndex
        Sensor(SensorNum)%CheckedOkay = .TRUE.
        ! If variable is Schedule Value, then get the schedule id to register it as being used
        IF (SameString(Sensor(SensorNum)%OutputVarName, 'Schedule Value')) THEN
          Sensor(SensorNum)%SchedNum = GetScheduleIndex(Sensor(SensorNum)%UniqueKeyName)
          IF (Sensor(SensorNum)%SchedNum == 0) THEN
            Sensor(SensorNum)%CheckedOkay = .FALSE.
            IF (reportErrors) THEN
              CALL ShowSevereError('Invalid Output:Variable or Output:Meter Index Key Name ='//  &
                 TRIM(Sensor(SensorNum)%UniqueKeyName))
              CALL ShowContinueError('For Output:Variable or Output:Meter = '//TRIM(Sensor(SensorNum)%OutputVarName))
              CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(Sensor(SensorNum)%Name))
              CALL ShowContinueError('Schedule Name not found.')
              ErrorsFound = .TRUE.
            ENDIF
          ENDIF
        ENDIF

      END IF
    END IF

  END DO  ! SensorNum

  ! added for FMU
  DO ActuatorNum = 1, numActuatorsUsed + NumExternalInterfaceActuatorsUsed &
      + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed
     ! If we process the ExternalInterface actuators, all we need to do is to change the

    IF ( ActuatorNum <= numActuatorsUsed ) THEN
      cCurrentModuleObject = 'EnergyManagementSystem:Actuator'
    ELSE IF ( ActuatorNum > numActuatorsUsed .AND. ActuatorNum <= numActuatorsUsed + NumExternalInterfaceActuatorsUsed) THEN
      cCurrentModuleObject = 'ExternalInterface:Actuator'
    ELSE IF ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed .AND. ActuatorNum <= numActuatorsUsed &
                           + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed) THEN
      cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitImport:To:Actuator'
    ELSE IF ( ActuatorNum > numActuatorsUsed + NumExternalInterfaceActuatorsUsed + &
      NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed.AND. ActuatorNum <= numActuatorsUsed &
      + NumExternalInterfaceActuatorsUsed + NumExternalInterfaceFunctionalMockupUnitImportActuatorsUsed &
      + NumExternalInterfaceFunctionalMockupUnitExportActuatorsUsed) THEN
      cCurrentModuleObject = 'ExternalInterface:FunctionalMockupUnitExport:To:Actuator'
    END IF

    IF (EMSActuatorUsed(ActuatorNum)%CheckedOkay) CYCLE
    FoundObjectType = .FALSE.
    FoundObjectName = .FALSE.
    FoundActuatorName = .FALSE.
    DO ActuatorVariableNum = 1, numEMSActuatorsAvailable
      IF (SameString(EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName , &
                     EMSActuatorUsed(ActuatorNum)%ComponentTypeName )) THEN
        FoundObjectType = .TRUE.
        IF (SameString(EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName , &
                       EMSActuatorUsed(ActuatorNum)%UniqueIDName )) THEN
          FoundObjectName = .TRUE.
          IF (SameString(EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName , &
                         EMSActuatorUsed(ActuatorNum)%ControlTypeName )) THEN
            FoundActuatorName = .TRUE.
            EXIT
          END IF
        END IF
      END IF
    END DO

    IF (.NOT. FoundObjectType) THEN
      IF (reportErrors) THEN
        CALL ShowSevereError('Invalid Actuated Component Type ='//TRIM(EMSActuatorUsed(ActuatorNum)%ComponentTypeName))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(EMSActuatorUsed(ActuatorNum)%Name))
        CALL ShowContinueError('Component Type not found')
        IF (OutputEDDFile) THEN
          CALL ShowContinueError('Review .edd file for valid component types.')
        ELSE
          CALL ShowContinueError('Use Output:EnergyManagementSystem object to create .edd file for valid component types.')
        ENDIF
        ErrorsFound = .TRUE.
      ENDIF
    END IF

    IF (.NOT. FoundObjectName) THEN
      IF (reportErrors) THEN
        CALL ShowSevereError('Invalid Actuated Component Unique Name ='//TRIM(EMSActuatorUsed(ActuatorNum)%UniqueIDName))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(EMSActuatorUsed(ActuatorNum)%Name))
        CALL ShowContinueError('Component Unique key name not found ')
        IF (OutputEDDFile) THEN
          CALL ShowContinueError('Review edd file for valid component names.')
        ELSE
          CALL ShowContinueError('Use Output:EnergyManagementSystem object to create .edd file for valid component names.')
        ENDIF
        ErrorsFound = .TRUE.
      ENDIF
    END IF

    IF (.NOT. FoundActuatorName) THEN
      IF (reportErrors) THEN
        CALL ShowSevereError('Invalid Actuated Component Control Type ='//TRIM(EMSActuatorUsed(ActuatorNum)%ControlTypeName))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(EMSActuatorUsed(ActuatorNum)%Name))
        CALL ShowContinueError('Control Type not found')
        IF (OutputEDDFile) THEN
          CALL ShowContinueError('Review edd file for valid component control types.')
        ELSE
          CALL ShowContinueError('Use Output:EnergyManagementSystem object to create '//  &
             '.edd file for valid component control types.')
        ENDIF
        ErrorsFound = .TRUE.
      ENDIF
    ELSE
      EMSActuatorUsed(ActuatorNum)%ActuatorVariableNum = ActuatorVariableNum
      EMSActuatorUsed(ActuatorNum)%CheckedOkay = .TRUE.
    END IF
  END DO  ! ActuatorNum

  cCurrentModuleObject = 'EnergyManagementSystem:InternalVariable'
  DO InternVarNum = 1, NumInternalVariablesUsed
    IF (EMSInternalVarsUsed(InternVarNum)%CheckedOkay) CYCLE
    FoundObjectType = .FALSE.
    FoundObjectName = .FALSE.
    DO InternalVarAvailNum = 1, numEMSInternalVarsAvailable
      IF (SameString(EMSInternalVarsAvailable(InternalVarAvailNum)%DataTypeName , &
                     EMSInternalVarsUsed(InternVarNum)%InternalDataTypeName)) THEN
        FoundObjectType = .TRUE.
        IF (SameString(EMSInternalVarsAvailable(InternalVarAvailNum)%UniqueIDName , &
                       EMSInternalVarsUsed(InternVarNum)%UniqueIDName)) THEN
          FoundObjectName = .TRUE.
          EXIT ! InternalVarAvailNum now holds needed index pointer
        END IF
      END IF
    ENDDO

    IF (.not. FoundObjectType) THEN
      IF (reportErrors) THEN
        CALL ShowSevereError('Invalid Internal Data Type ='//TRIM(EMSInternalVarsUsed(InternVarNum)%InternalDataTypeName))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(EMSInternalVarsUsed(InternVarNum)%Name))
        CALL ShowContinueError('Internal data type name not found')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    IF (.NOT. FoundObjectName) THEN
      IF (reportErrors) THEN
        CALL ShowSevereError('Invalid Internal Data Index Key Name ='//TRIM(EMSInternalVarsUsed(InternVarNum)%UniqueIDName))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(EMSInternalVarsUsed(InternVarNum)%Name))
        CALL ShowContinueError('Internal data unique identifier not found')
        ErrorsFound = .TRUE.
      ENDIF
    ELSE
      EMSInternalVarsUsed(InternVarNum)%InternVarNum = InternalVarAvailNum
      EMSInternalVarsUsed(InternVarNum)%CheckedOkay  = .TRUE.
    ENDIF

  ENDDO
  IF (reportErrors) THEN
    CALL EchoOutActuatorKeyChoices
    CALL EchoOutInternalVariableChoices
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing Energy Management System input. Preceding condition causes termination.')
  END IF

  IF (reportErrors) THEN
    CALL BeginEnvrnInitializeRuntimeLanguage
  ENDIF

  RETURN

END SUBROUTINE ProcessEMSInput


SUBROUTINE GetVariableTypeAndIndex(VarName, VarKeyName, VarType, VarIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! local helper routine intended to lookup report variables only.
          !    Use GetMeterIndex for meters.

          ! METHODOLOGY EMPLOYED:
          ! make calls to OutputProcessor methods GetVariableKeyCountandType and GetVariableKeys

          ! USE STATEMENTS:

  USE RuntimeLanguageProcessor, ONLY: EvaluateStack
  USE DataInterfaces, ONLY:GetVariableKeyCountandType, GetVariableKeys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: VarName
  CHARACTER(len=*), INTENT(IN) :: VarKeyName
  INTEGER, INTENT(OUT) :: VarType
  INTEGER, INTENT(OUT) :: VarIndex

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: NumKeys
  INTEGER                :: KeyNum
  INTEGER                :: AvgOrSum
  INTEGER                :: StepType
  CHARACTER(len=MaxNameLength) :: Units
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE   :: KeyName
  INTEGER, DIMENSION(:), ALLOCATABLE   :: KeyIndex
  LOGICAL :: Found

          ! FLOW:
  VarType = 0
  VarIndex = 0
  Found = .FALSE.
  CALL GetVariableKeyCountandType(VarName, NumKeys, VarType, AvgOrSum, StepType, Units)

  ! note that schedules are not getting VarType set right...

  IF (NumKeys > 0) THEN
    ALLOCATE(KeyName(NumKeys))
    ALLOCATE(KeyIndex(NumKeys))
    CALL GetVariableKeys(VarName, VarType, KeyName, KeyIndex)

    IF (KeyName(1) == 'ENVIRONMENT') THEN
      VarIndex = KeyIndex(1)
    ELSE
      DO KeyNum = 1, NumKeys
        IF (KeyName(KeyNum) == VarKeyName) THEN
          Found = .TRUE.
          EXIT
        END IF
      END DO
      IF (Found) VarIndex = KeyIndex(KeyNum)
    END IF

    DEALLOCATE(KeyName)
    DEALLOCATE(KeyIndex)
  END IF

  RETURN

END SUBROUTINE GetVariableTypeAndIndex

SUBROUTINE EchoOutActuatorKeyChoices

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! echo out actuators registered with SetupEMSActuator for user access

          ! METHODOLOGY EMPLOYED:
          ! mine structure and write to edd file
          ! note this executes after final processing and sizing-related calling points may already execute Erl programs

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

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
  INTEGER :: ActuatorLoop
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: TempTypeName
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: TempCntrlType
  LOGICAL, DIMENSION(:), ALLOCATABLE                      :: NonUniqueARRflag
  INTEGER :: FoundTypeName
  INTEGER :: FoundControlType

  IF (OutputEMSActuatorAvailFull) THEN

    WRITE(OutputEMSFileUnitNum, '(A)') '! <EnergyManagementSystem:Actuator Available>, Component Unique Name,' &
                                           //' Component Type,  Control Type, Units'
    Do ActuatorLoop =1, numEMSActuatorsAvailable
      WRITE(OutputEMSFileUnitNum, '(A)') 'EnergyManagementSystem:Actuator Available,' &
                                              //Trim(EMSActuatorAvailable(ActuatorLoop)%UniqueIDName) &
                                         //','//Trim(EMSActuatorAvailable(ActuatorLoop)%ComponentTypeName)  &
                                         //','//Trim(EMSActuatorAvailable(ActuatorLoop)%ControlTypeName) &
                                         //','//Trim(EMSActuatorAvailable(ActuatorLoop)%Units)
    ENDDO
  ELSE IF (OutputEMSActuatorAvailSmall) THEN
    WRITE(OutputEMSFileUnitNum, '(A)') '! <EnergyManagementSystem:Actuator Available>, *, Component Type, '// &
                                          'Control Type, Units'

    ALLOCATE(TempTypeName(numEMSActuatorsAvailable))
    TempTypeName = EMSActuatorAvailable%ComponentTypeName
    ALLOCATE(TempCntrlType(numEMSActuatorsAvailable))
    TempCntrlType = EMSActuatorAvailable%ControlTypeName
    ALLOCATE(NonUniqueARRflag(numEMSActuatorsAvailable))
    NonUniqueARRflag = .FALSE.
    DO ActuatorLoop =1, numEMSActuatorsAvailable
      IF (ActuatorLoop+1 <= numEMSActuatorsAvailable) THEN
        FoundTypeName = FindItemInList(TempTypeName(ActuatorLoop), TempTypeName(ActuatorLoop+1:numEMSActuatorsAvailable), &
                                 (numEMSActuatorsAvailable - (ActuatorLoop+1)) )
        FoundControlType = FindItemInList(TempCntrlType(ActuatorLoop), TempCntrlType(ActuatorLoop+1:numEMSActuatorsAvailable), &
                                 (numEMSActuatorsAvailable - (ActuatorLoop+1)) )
      ELSE
        FoundTypeName = 1
        FoundControlType = 1
      ENDIF
      IF ((FoundTypeName /= 0) .AND. (FoundControlType /= 0)) THEN
        NonUniqueARRflag(ActuatorLoop) = .TRUE.
      ENDIF
    ENDDO
    DO ActuatorLoop =1, numEMSActuatorsAvailable
      IF (.NOT. NonUniqueARRflag(ActuatorLoop)) Then
        WRITE(OutputEMSFileUnitNum, '(A)') 'EnergyManagementSystem:Actuator Available,' &
                                              //' *' &
                                         //','//Trim(EMSActuatorAvailable(ActuatorLoop)%ComponentTypeName)  &
                                         //','//Trim(EMSActuatorAvailable(ActuatorLoop)%ControlTypeName) &
                                         //','//Trim(EMSActuatorAvailable(ActuatorLoop)%Units)
      ENDIF
    ENDDO

    DEALLOCATE(TempTypeName)
    DEALLOCATE(TempCntrlType)
    DEALLOCATE(NonUniqueARRflag)

  ENDIF
  RETURN

END SUBROUTINE EchoOutActuatorKeyChoices

SUBROUTINE EchoOutInternalVariableChoices

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! echo out actuators registered with SetupEMSActuator for user access

          ! METHODOLOGY EMPLOYED:
          ! mine structure and write to eio file

          ! REFERENCES:
          ! na

          ! USE STATEMENTS
  USE InputProcessor, ONLY: FindItemInList
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
  INTEGER :: InternalDataLoop
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: TempTypeName
  LOGICAL, DIMENSION(:), ALLOCATABLE                      :: UniqueARRflag
  INTEGER :: Found

  IF (OutputEMSInternalVarsFull) THen

    WRITE(OutputEMSFileUnitNum, '(A)') '! <EnergyManagementSystem:InternalVariable Available>, Unique Name, Internal Data Type' &
                     //', Units '
    DO InternalDataLoop =1, numEMSInternalVarsAvailable
      WRITE(OutputEMSFileUnitNum, '(A)') 'EnergyManagementSystem:InternalVariable Available,'&
                                         //Trim(EMSInternalVarsAvailable(InternalDataLoop)%UniqueIDName) &
                                         //','//Trim(EMSInternalVarsAvailable(InternalDataLoop)%DataTypeName) &
                                         //','//Trim(EMSInternalVarsAvailable(InternalDataLoop)%Units )
    ENDDO

  ELSE IF (OutputEMSInternalVarsSmall) THEN
    WRITE(OutputEMSFileUnitNum, '(A)') '! <EnergyManagementSystem:InternalVariable Available>, *, Internal Data Type'
    ALLOCATE(TempTypeName(numEMSInternalVarsAvailable))
    TempTypeName = EMSInternalVarsAvailable%DataTypeName
    ALLOCATE(UniqueARRflag(numEMSInternalVarsAvailable))
    UniqueARRflag = .FALSE.
    DO InternalDataLoop =1, numEMSInternalVarsAvailable
      IF (InternalDataLoop+1 <= numEMSInternalVarsAvailable) THEN
        Found = FindItemInList(TempTypeName(InternalDataLoop), TempTypeName(InternalDataLoop+1:numEMSInternalVarsAvailable), &
                                 (numEMSInternalVarsAvailable - (InternalDataLoop+1)) )
      ELSE
        Found = 0
      ENDIF
      IF (Found == 0) UniqueARRflag(InternalDataLoop) = .TRUE.
    ENDDO
    DO InternalDataLoop =1, numEMSInternalVarsAvailable
      IF (UniqueARRflag(InternalDataLoop)) Then
        WRITE(OutputEMSFileUnitNum, '(A)') 'EnergyManagementSystem:InternalVariable Available,'&
                                         //' *' &
                                         //','//Trim(EMSInternalVarsAvailable(InternalDataLoop)%DataTypeName) &
                                         //','//Trim(EMSInternalVarsAvailable(InternalDataLoop)%Units )
      ENDIF
    ENDDO

    DEALLOCATE(TempTypeName)
    DEALLOCATE(UniqueARRflag)
  ENDIF
  RETURN

END SUBROUTINE EchoOutInternalVariableChoices


SUBROUTINE SetupNodeSetpointsAsActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! make system nodes in model available for EMS control

          ! METHODOLOGY EMPLOYED:
          ! Loop over node structures and make calls to SetupEMSActuator
          ! the pattern for the basic node setpoints is a little different in that the actuators directly
          ! affect the node variables, rather than using seperate logical override flag and ems values


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,      ONLY: Node, NodeID, NumOfNodes
  USE DataInterfaces,    ONLY: SetupEMSActuator
  USE OutAirNodeManager, ONLY: NumOutsideAirNodes, OutsideAirNodeList

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
  INTEGER  :: LoopNode  !local do loop index
  LOGICAL  :: lDummy ! not going to setup a pointer to logical control
                              ! (could this ever cause a fault?)
                              ! make it optional in Setup call?
  INTEGER  :: OutsideAirNodeNum ! local do loop index
  INTEGER  :: NodeNum ! local index.

  lDummy = .FALSE.

  IF (NumOfNodes > 0 ) THEN

    DO LoopNode = 1, NumOfNodes
    ! setup the setpoint for each type of variable that can be controlled
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Temperature Setpoint', '[C]', lDummy, Node(LoopNode)%TempSetPoint )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Temperature Minimum Setpoint', '[C]', lDummy, Node(LoopNode)%TempMin )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Temperature Maximum Setpoint', '[C]', lDummy, Node(LoopNode)%TempMax )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Humidity Ratio Setpoint', '[kgWater/kgDryAir]', lDummy, Node(LoopNode)%HumRatSetPoint )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Humidity Ratio Maximum Setpoint', '[kgWater/kgDryAir]', lDummy, Node(LoopNode)%HumRatMax )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Humidity Ratio Minimum Setpoint', '[kgWater/kgDryAir]', lDummy, Node(LoopNode)%HumRatMin )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Mass Flow Rate Setpoint', '[kg/s]', lDummy, Node(LoopNode)%MassFlowRateSetPoint )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Mass Flow Rate Maximum Available Setpoint', '[kg/s]', lDummy, Node(LoopNode)%MassFlowRateMaxAvail )
      CALL SetupEMSActuator ('System Node Setpoint', NodeID(LoopNode), &
                              'Mass Flow Rate Minimum Available Setpoint', '[kg/s]', lDummy, Node(LoopNode)%MassFlowRateMinAvail )
    ENDDO

  ENDIF ! NumOfNodes > 0

  IF (NumOutsideAirNodes > 0) THEN
    DO OutsideAirNodeNum = 1, NumOutsideAirNodes
      NodeNum = OutsideAirNodeList(OutsideAirNodeNum)
      CALL SetupEMSActuator ('Outdoor Air System Node', NodeID(NodeNum), &
                                'Drybulb Temperature' , '[C]', Node(NodeNum)%EMSOverrideOutAirDryBulb, &
                                Node(NodeNum)%EMSValueForOutAirDryBulb )
      CALL SetupEMSActuator ('Outdoor Air System Node', NodeID(NodeNum), &
                                'Wetbulb Temperature' , '[C]', Node(NodeNum)%EMSOverrideOutAirWetBulb, &
                                Node(NodeNum)%EMSValueForOutAirWetBulb )
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE SetupNodeSetpointsAsActuators

SUBROUTINE UpdateEMSTrendVariables

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Logged trend data

          ! METHODOLOGY EMPLOYED:
          ! Store current value of Erl Variable in Trend stack
          ! Trend arrays are pushed so that the latest value is
          !  always at index 1.  old values get lost.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataGlobals, ONLY: AnyEnergyManagementSystemInModel

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
  INTEGER   :: TrendNum = 0 ! local loop counter
  INTEGER   :: ErlVarNum = 0 !
  INTEGER   :: TrendDepth = 0
  REAL(r64) :: currentVal = 0.0D0


  ! checks with quick return if no updates needed.
  IF (.NOT. AnyEnergyManagementSystemInModel) RETURN
  IF (NumErlTrendVariables == 0) RETURN

  DO TrendNum = 1, NumErlTrendVariables
    ErlVarNum = TrendVariable(TrendNum)%ErlVariablePointer
    TrendDepth = TrendVariable(TrendNum)%LogDepth
    IF ((ErlVarNum > 0) .AND. (TrendDepth > 0)) THEN
      currentVal = ErlVariable(ErlVarNum)%Value%Number
      ! push into trend
      TrendVariable(TrendNum)%tempTrendARR = TrendVariable(TrendNum)%TrendValARR
      TrendVariable(TrendNum)%TrendValARR(1) = currentVal
      TrendVariable(TrendNum)%TrendValARR(2:TrendDepth) = &
                        TrendVariable(TrendNum)%tempTrendARR(1:TrendDepth -1)

    ENDIF
  ENDDO

  RETURN

END SUBROUTINE UpdateEMSTrendVariables

SUBROUTINE CheckIfNodeSetpointManagedByEMS(NodeNum, SetpointType, ErrorFlag )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Provide method to verify that a specific node is (probably) managed by EMS

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataLoopNode,   ONLY: NodeID

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: NodeNum ! index of node being checked.
  INTEGER, INTENT(IN)    :: SetpointType
  LOGICAL, INTENT(INOUT) :: ErrorFlag
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop = 0 ! local do loop index
  CHARACTER(len=MaxNameLength) :: cControlTypeName
  CHARACTER(len=MaxNameLength) :: cComponentTypeName
  CHARACTER(len=MaxNameLength) :: cNodeName
  LOGICAL  :: FoundControl = .FALSE.

  FoundControl = .FALSE.

  cNodeName = NodeID(NodeNum)
  cComponentTypeName = 'System Node Setpoint'
  SELECT CASE (SetpointType)

  CASE (iTemperatureSetpoint)
    cControlTypeName = 'Temperature Setpoint'
  CASE (iTemperatureMinSetpoint)
    cControlTypeName = 'Temperature Minimum Setpoint'
  CASE (iTemperatureMaxSetpoint)
    cControlTypeName = 'Temperature Maximum Setpoint'
  CASE (iHumidityRatioSetpoint)
    cControlTypeName = 'Humidity Ratio Setpoint'
  CASE (iHumidityRatioMinSetpoint)
    cControlTypeName = 'Humidity Ratio Minimum Setpoint'
  CASE (iHumidityRatioMaxSetpoint)
    cControlTypeName = 'Humidity Ratio Maximum Setpoint'
  CASE (iMassFlowRateSetpoint)
    cControlTypeName = 'Mass Flow Rate Setpoint'
  CASE (iMassFlowRateMinSetpoint)
    cControlTypeName = 'Mass Flow Rate Minimum Available Setpoint'
  CASE (iMassFlowRateMaxSetpoint)
    cControlTypeName = 'Mass Flow Rate Maximum Available Setpoint'
  END SELECT

  DO loop = 1, numActuatorsUsed + NumExternalInterfaceActuatorsUsed
    IF ( (SameString( EMSActuatorUsed(loop)%ComponentTypeName, cComponentTypeName)) &
        .AND. (SameString( EMSActuatorUsed(loop)%UniqueIDName, cNodeName ))  &
        .AND. (SameString( EMSActuatorUsed(loop)%ControlTypeName, cControlTypeName)) ) THEN
      FoundControl = .TRUE.
    ENDIF

  ENDDO

  IF ((.NOT. ErrorFlag) .AND. (.NOT. FoundControl)) ErrorFlag = .TRUE.

  RETURN

END SUBROUTINE CheckIfNodeSetpointManagedByEMS

SUBROUTINE SetupPrimaryAirSystemAvailMgrAsActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! make air system status available as EMS actuator

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop    , ONLY: PriAirSysAvailMgr
  USE DataAirSystems , ONLY: PrimaryAirSystem
  USE DataInterfaces , ONLY: SetupEMSActuator

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
  INTEGER :: numAirLoops = 0
  INTEGER :: loop = 0
  LOGICAL  :: lDummy
  lDummy = .FALSE.

  IF (ALLOCATED(PriAirSysAvailMgr)) THEN
    numAirLoops = SIZE(PriAirSysAvailMgr)
    DO loop = 1, numAirLoops
          CALL SetupEMSActuator('AirLoopHVAC', PrimaryAirSystem(loop)%Name,  &
                 'Availability Status', '[ ]', lDummy, PriAirSysAvailMgr(loop)%AvailStatus )

    ENDDO

  ELSE

  ENDIF

  RETURN

END SUBROUTINE SetupPrimaryAirSystemAvailMgrAsActuators

SUBROUTINE SetupWindowShadingControlActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! make calls to SetupEMSactuator for public data for Window Shades

          ! METHODOLOGY EMPLOYED:
          ! Loop thru SurfaceWindow and register any shading controls

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces, Only: Surface, SurfaceWindow, TotSurfaces, SurfaceClass_Window, &
                           ExternalEnvironment
  Use DataInterfaces, Only: SetupEMSActuator

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
  INTEGER :: loopSurfNum = 0 ! local do loop index

  Do loopSurfNum  =1, TotSurfaces

    IF(Surface(loopSurfNum)%Class /= SurfaceClass_Window) CYCLE
    IF(Surface(loopSurfNum)%ExtBoundCond /= ExternalEnvironment) CYCLE
    IF(Surface(loopSurfNum)%WindowShadingControlPtr == 0) CYCLE

    CALL SetupEMSActuator('Window Shading Control',   Surface(loopSurfNum)%Name, &
                           'Control Status', '[ShadeStatus]', &
      SurfaceWindow(loopSurfNum)%ShadingFlagEMSOn, SurfaceWindow(loopSurfNum)%ShadingFlagEMSValue)

    IF (SurfaceWindow(loopSurfNum)%MovableSlats) THEN
      CALL SetupEMSActuator('Window Shading Control',   Surface(loopSurfNum)%Name, &
                           'Slat Angle', '[degrees]', &
        SurfaceWindow(loopSurfNum)%SlatAngThisTSDegEMSon, SurfaceWindow(loopSurfNum)%SlatAngThisTSDegEMSValue)

    ENDIF

  ENDDO
  RETURN

END SUBROUTINE SetupWindowShadingControlActuators

SUBROUTINE SetupThermostatActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Make zone thermostats, humidistats, and comfort controls available to EMS

          ! METHODOLOGY EMPLOYED:
          ! Loop over structures and call SetupEMSactuator for public data in DataZoneControls.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneControls
  USE DataInterfaces,   ONLY: SetupEMSActuator

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
  INTEGER :: Loop = 0 ! local do loop index

  DO Loop = 1, NumTempControlledZones
    CALL SetupEMSActuator('Zone Temperature Control',  TempControlledZone(loop)%ZoneName, &
                           'Heating Setpoint', '[C]', &
                           TempControlledZone(loop)%EMSOverrideHeatingSetpointOn, &
                           TempControlledZone(loop)%EMSOverrideHeatingSetpointValue)
    CALL SetupEMSActuator('Zone Temperature Control',  TempControlledZone(loop)%ZoneName, &
                           'Cooling Setpoint', '[C]', &
                           TempControlledZone(loop)%EMSOverrideCoolingSetpointOn, &
                           TempControlledZone(loop)%EMSOverrideCoolingSetpointValue)
  ENDDO

  DO Loop = 1, NumHumidityControlZones
    CALL SetupEMSActuator('Zone Humidity Control',  HumidityControlZone(loop)%ZoneName, &
                           'Relative Humidity Humidifying Setpoint', '[%]', &
                           HumidityControlZone(loop)%EMSOverrideHumidifySetpointOn, &
                           HumidityControlZone(loop)%EMSOverrideHumidifySetpointValue)
    CALL SetupEMSActuator('Zone Humidity Control',  HumidityControlZone(loop)%ZoneName, &
                           'Relative Humidity Dehumidifying Setpoint', '[%]', &
                           HumidityControlZone(loop)%EMSOverrideDehumidifySetpointOn, &
                           HumidityControlZone(loop)%EMSOverrideDehumidifySetpointValue)
  ENDDO

  DO Loop = 1, NumComfortControlledZones
    CALL SetupEMSActuator('Zone Comfort Control',  ComfortControlledZone(loop)%ZoneName, &
                           'Heating Setpoint', '[]', &
                           ComfortControlledZone(loop)%EMSOverrideHeatingSetpointOn, &
                           ComfortControlledZone(loop)%EMSOverrideHeatingSetpointValue)
    CALL SetupEMSActuator('Zone Comfort Control',  ComfortControlledZone(loop)%ZoneName, &
                           'Cooling Setpoint', '[]', &
                           ComfortControlledZone(loop)%EMSOverrideCoolingSetpointOn, &
                           ComfortControlledZone(loop)%EMSOverrideCoolingSetpointValue)
  ENDDO

  RETURN

END SUBROUTINE SetupThermostatActuators

SUBROUTINE SetupSurfaceConvectionActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Setup EMS actuators available for surface convection coefficients

          ! METHODOLOGY EMPLOYED:
          ! access public data and loop over it.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces,ONLY: SetupEMSActuator
  USE DataSurfaces,  ONLY: Surface, TotSurfaces

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
  INTEGER :: SurfNum ! local loop index.


  DO SurfNum = 1, TotSurfaces
    CALL SetupEMSActuator('Surface',  Surface(SurfNum)%Name, &
                           'Interior Surface Convection Heat Transfer Coefficient', '[W/m2-K]', &
                           Surface(SurfNum)%EMSOverrideIntConvCoef, &
                           Surface(SurfNum)%EMSValueForIntConvCoef)
    CALL SetupEMSActuator('Surface',  Surface(SurfNum)%Name, &
                           'Exterior Surface Convection Heat Transfer Coefficient', '[W/m2-K]', &
                           Surface(SurfNum)%EMSOverrideExtConvCoef, &
                           Surface(SurfNum)%EMSValueForExtConvCoef)
  ENDDO

  RETURN

END SUBROUTINE SetupSurfaceConvectionActuators

SUBROUTINE SetupSurfaceConstructionActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! setup EMS actuators available for surface construction

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces,  ONLY: SetupEMSActuator
  USE DataSurfaces,    ONLY: Surface, TotSurfaces
  USE DataHeatBalance, ONLY: TotConstructs

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
  INTEGER :: SurfNum ! local loop index.
  DO SurfNum = 1, TotSurfaces

    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE

    CALL SetupEMSActuator('Surface',  Surface(SurfNum)%Name, &
                           'Construction State', '[ ]', &
                           Surface(SurfNum)%EMSConstructionOverrideON, &
                           Surface(SurfNum)%EMSConstructionOverrideValue)
  ENDDO

  !Setup error checking storage

  IF (.NOT. ALLOCATED(EMSConstructActuatorChecked)) &
    ALLOCATE(EMSConstructActuatorChecked(TotSurfaces, TotConstructs))
  EMSConstructActuatorChecked = .FALSE.

  IF (.NOT. ALLOCATED(EMSConstructActuatorIsOkay)) &
    ALLOCATE(EMSConstructActuatorIsOkay(TotSurfaces, TotConstructs))
  EMSConstructActuatorIsOkay = .FALSE.

  RETURN

END SUBROUTINE SetupSurfaceConstructionActuators

SUBROUTINE SetupSurfaceOutdoorBoundaryConditionActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   May 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! setup EMS actuators for outside boundary conditions by surface

          ! METHODOLOGY EMPLOYED:
          ! loop through all surfaces, cycle if not heat transfer or outdoors BC

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces,  ONLY: SetupEMSActuator
  USE DataSurfaces,    ONLY: Surface, TotSurfaces, ExternalEnvironment

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
  INTEGER :: SurfNum ! local loop index.

  DO SurfNum = 1, TotSurfaces

    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
    IF (.NOT. Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) CYCLE

    CALL SetupEMSActuator('Surface',  Surface(SurfNum)%Name, &
                           'Outdoor Air Dryblub Temperature', '[C]', &
                           Surface(SurfNum)%OutDryBulbTempEMSOverrideOn, &
                           Surface(SurfNum)%OutDryBulbTempEMSOverrideValue)

    CALL SetupEMSActuator('Surface',  Surface(SurfNum)%Name, &
                           'Outdoor Air Wetblub Temperature', '[C]', &
                           Surface(SurfNum)%OutWetBulbTempEMSOverrideOn, &
                           Surface(SurfNum)%OutWetBulbTempEMSOverrideValue)
    IF (Surface(SurfNum)%ExtWind) THEN
      CALL SetupEMSActuator('Surface',  Surface(SurfNum)%Name, &
                             'Outdoor Air Wind Speed', '[m/s]', &
                             Surface(SurfNum)%WindSpeedEMSOverrideOn, &
                             Surface(SurfNum)%WindSpeedEMSOverrideValue)
    ENDIF
  ENDDO
  RETURN

END SUBROUTINE SetupSurfaceOutdoorBoundaryConditionActuators


SUBROUTINE SetupZoneInfoAsInternalDataAvail

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! set up zone-related info as internal data

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: NumOfZones
  USE DataInterfaces, ONLY: SetupEMSInternalVariable
  USE DataHeatBalance, ONLY: Zone

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

  INTEGER :: ZoneNum

  IF (Allocated(Zone)) THEN
    DO ZoneNum = 1, NumOfZones

      CALL SetupEMSInternalVariable('Zone Floor Area', Zone(ZoneNum)%Name, '[m2]', &
                                    Zone(ZoneNum)%FloorArea )
      CALL SetupEMSInternalVariable('Zone Air Volume', Zone(ZoneNum)%Name, '[m3]', &
                                    Zone(ZoneNum)%Volume )
      CALL SetupEMSInternalVariable('Zone Multiplier', Zone(ZoneNum)%Name, '[ ]', &
                                    Zone(ZoneNum)%Multiplier )
      CALL SetupEMSInternalVariable('Zone List Multiplier', Zone(ZoneNum)%Name, '[ ]', &
                                    Zone(ZoneNum)%ListMultiplier )
    ENDDO

  ENDIF

  RETURN

END SUBROUTINE SetupZoneInfoAsInternalDataAvail





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

END MODULE EMSManager
!
!Moved these setup EMS actuator routines out of module to solve circular use problems between
!  ScheduleManager and OutputProcessor. Followed pattern used for SetupOutputVariable
!
SUBROUTINE SetupEMSRealActuator(cComponentTypeName, cUniqueIDName, cControlTypeName, cUnits, lEMSActuated, rValue )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   June 2006
          !       MODIFIED       Brent Griffith April 2009,
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! register a new actuator for EMS
          !   set  up pointer to logical and real value
          !

          ! METHODOLOGY EMPLOYED:
          ! push size of ActuatorVariable and add a new one.
          !  check for duplicates.

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError
  USE InputProcessor, ONLY: MakeUpperCase
  USE DataPrecisionGlobals
  USE DataRuntimeLanguage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: cComponentTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName
  CHARACTER(len=*), INTENT(IN)  :: cControlTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUnits
  LOGICAL, TARGET, INTENT(IN)   :: lEMSActuated
  REAL(r64), TARGET, INTENT(IN) :: rValue

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ActuatorVariableNum
  LOGICAL :: FoundActuatorType
  LOGICAL :: FoundDuplicate
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectType
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectName
  CHARACTER(len=MaxNameLength) :: UpperCaseActuatorName
  TYPE(EMSActuatorAvailableType), DIMENSION(:), ALLOCATABLE :: TempEMSActuatorAvailable

          ! FLOW:

  FoundActuatorType = .FALSE.
  FoundDuplicate = .FALSE.

  UpperCaseObjectType   = MakeUpperCase(cComponentTypeName)
  UpperCaseObjectName   = MakeUpperCase(cUniqueIDName)
  UpperCaseActuatorName = MakeUpperCase(cControlTypeName)

  DO ActuatorVariableNum = 1, numEMSActuatorsAvailable
    IF ((EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName == UpperCaseObjectType)  &
      .AND. (EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName == UpperCaseActuatorName)) THEN

      FoundActuatorType = .TRUE.

      IF (EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName == UpperCaseObjectName) THEN
        FoundDuplicate = .TRUE.
        EXIT
      END IF
    END IF
  END DO

  IF (FoundDuplicate) THEN
    CALL ShowSevereError('Duplicate actuator was sent to SetupEMSActuator.')
  ELSE
    ! Add new actuator
    IF (numEMSActuatorsAvailable == 0) THEN
      ALLOCATE(EMSActuatorAvailable(VarsAvailableAllocInc))
      numEMSActuatorsAvailable = 1
      maxEMSActuatorsAvailable = VarsAvailableAllocInc
    ELSE
      IF (numEMSActuatorsAvailable+1 > maxEMSActuatorsAvailable) THEN
        ALLOCATE(TempEMSActuatorAvailable(maxEMSActuatorsAvailable+VarsAvailableAllocInc))
        TempEMSActuatorAvailable(1:numEMSActuatorsAvailable) = EMSActuatorAvailable(1:numEMSActuatorsAvailable)
        DEALLOCATE(EMSActuatorAvailable)
        ALLOCATE(EMSActuatorAvailable(maxEMSActuatorsAvailable+VarsAvailableAllocInc))
        EMSActuatorAvailable(1:numEMSActuatorsAvailable) = TempEMSActuatorAvailable(1:numEMSActuatorsAvailable)
        DEALLOCATE(TempEMSActuatorAvailable)
        maxEMSActuatorsAvailable=maxEMSActuatorsAvailable+VarsAvailableAllocInc
      ENDIF
      numEMSActuatorsAvailable = numEMSActuatorsAvailable + 1
    END IF

    ActuatorVariableNum = numEMSActuatorsAvailable
    EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName = cComponentTypeName
    EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName      = cUniqueIDName
    EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName   = cControlTypeName
    EMSActuatorAvailable(ActuatorVariableNum)%Units             = cUnits
    EMSActuatorAvailable(ActuatorVariableNum)%Actuated         => lEMSActuated  ! Pointer assigment
    EMSActuatorAvailable(ActuatorVariableNum)%RealValue        => rValue        ! Pointer assigment
    EMSActuatorAvailable(ActuatorVariableNum)%PntrVarTypeUsed  = PntrReal

  END IF

  RETURN

END SUBROUTINE SetupEMSRealActuator

SUBROUTINE SetupEMSIntegerActuator(cComponentTypeName, cUniqueIDName, cControlTypeName, cUnits, lEMSActuated, iValue )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! register a new actuator for EMS
          !   set  up pointer to logical and integer value
          !

          ! METHODOLOGY EMPLOYED:
          ! push size of ActuatorVariable and add a new one.
          !  check for duplicates.

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError
  USE InputProcessor, ONLY: MakeUpperCase
  USE DataPrecisionGlobals
  USE DataRuntimeLanguage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: cComponentTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName
  CHARACTER(len=*), INTENT(IN)  :: cControlTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUnits
  LOGICAL, TARGET, INTENT(IN)   :: lEMSActuated
  INTEGER, TARGET, INTENT(IN)   :: iValue

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ActuatorVariableNum
  LOGICAL :: FoundActuatorType
  LOGICAL :: FoundDuplicate
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectType
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectName
  CHARACTER(len=MaxNameLength) :: UpperCaseActuatorName
  TYPE(EMSActuatorAvailableType), DIMENSION(:), ALLOCATABLE :: TempEMSActuatorAvailable

          ! FLOW:
!  IF (.NOT. ActuatorFileOpen) THEN
!    !OPEN(88,file='eplusout.add')
!    !WRITE(88, '(A)') 'Object Type,Actuator Name'
!    ActuatorFileOpen = .TRUE.
!  END IF

  FoundActuatorType = .FALSE.
  FoundDuplicate = .FALSE.

  UpperCaseObjectType   = MakeUpperCase(cComponentTypeName)
  UpperCaseObjectName   = MakeUpperCase(cUniqueIDName)
  UpperCaseActuatorName = MakeUpperCase(cControlTypeName)

  DO ActuatorVariableNum = 1, numEMSActuatorsAvailable
    IF ((EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName == UpperCaseObjectType)  &
      .AND. (EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName == UpperCaseActuatorName)) THEN

      FoundActuatorType = .TRUE.

      IF (EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName == UpperCaseObjectName) THEN
        FoundDuplicate = .TRUE.
        EXIT
      END IF
    END IF
  END DO

  IF (FoundDuplicate) THEN
    CALL ShowSevereError('Duplicate actuator was sent to SetupEMSIntegerActuator.')
  ELSE
    ! Add new actuator
    IF (numEMSActuatorsAvailable == 0) THEN
      ALLOCATE(EMSActuatorAvailable(VarsAvailableAllocInc))
      numEMSActuatorsAvailable = 1
      maxEMSActuatorsAvailable = VarsAvailableAllocInc
    ELSE
      IF (numEMSActuatorsAvailable+1 > maxEMSActuatorsAvailable) THEN
        ALLOCATE(TempEMSActuatorAvailable(maxEMSActuatorsAvailable+VarsAvailableAllocInc))
        TempEMSActuatorAvailable(1:numEMSActuatorsAvailable) = EMSActuatorAvailable(1:numEMSActuatorsAvailable)
        DEALLOCATE(EMSActuatorAvailable)
        ALLOCATE(EMSActuatorAvailable(maxEMSActuatorsAvailable+VarsAvailableAllocInc))
        EMSActuatorAvailable(1:numEMSActuatorsAvailable) = TempEMSActuatorAvailable(1:numEMSActuatorsAvailable)
        DEALLOCATE(TempEMSActuatorAvailable)
        maxEMSActuatorsAvailable=maxEMSActuatorsAvailable+VarsAvailableAllocInc
      ENDIF
      numEMSActuatorsAvailable = numEMSActuatorsAvailable + 1
    END IF

    ActuatorVariableNum = numEMSActuatorsAvailable
    EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName = cComponentTypeName
    EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName      = cUniqueIDName
    EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName   = cControlTypeName
    EMSActuatorAvailable(ActuatorVariableNum)%Units             = cUnits
    EMSActuatorAvailable(ActuatorVariableNum)%Actuated         => lEMSActuated  ! Pointer assigment
    EMSActuatorAvailable(ActuatorVariableNum)%IntValue         => iValue        ! Pointer assigment
    EMSActuatorAvailable(ActuatorVariableNum)%PntrVarTypeUsed   = PntrInteger

  END IF

  RETURN

END SUBROUTINE SetupEMSIntegerActuator

SUBROUTINE SetupEMSLogicalActuator(cComponentTypeName, cUniqueIDName, cControlTypeName, cUnits, lEMSActuated, lValue )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2009
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! register a new actuator for EMS
          !   set  up pointer to logical and logical value
          !

          ! METHODOLOGY EMPLOYED:
          ! push size of ActuatorVariable and add a new one.
          !  check for duplicates.

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError, ShowContinueError
  USE InputProcessor, ONLY: MakeUpperCase
  USE DataPrecisionGlobals
  USE DataRuntimeLanguage

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: cComponentTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName
  CHARACTER(len=*), INTENT(IN)  :: cControlTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUnits
  LOGICAL, TARGET, INTENT(IN)   :: lEMSActuated
  LOGICAL, TARGET, INTENT(IN)   :: lValue

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ActuatorVariableNum
  LOGICAL :: FoundActuatorType
  LOGICAL :: FoundDuplicate
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectType
  CHARACTER(len=MaxNameLength) :: UpperCaseObjectName
  CHARACTER(len=MaxNameLength) :: UpperCaseActuatorName
  TYPE(EMSActuatorAvailableType), DIMENSION(:), ALLOCATABLE :: TempEMSActuatorAvailable

          ! FLOW:
  FoundActuatorType = .FALSE.
  FoundDuplicate = .FALSE.

  UpperCaseObjectType   = MakeUpperCase(cComponentTypeName)
  UpperCaseObjectName   = MakeUpperCase(cUniqueIDName)
  UpperCaseActuatorName = MakeUpperCase(cControlTypeName)

  DO ActuatorVariableNum = 1, numEMSActuatorsAvailable
    IF ((EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName == UpperCaseObjectType)  &
      .AND. (EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName == UpperCaseActuatorName)) THEN

      FoundActuatorType = .TRUE.

      IF (EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName == UpperCaseObjectName) THEN
        FoundDuplicate = .TRUE.
        EXIT
      END IF
    END IF
  END DO

  IF (FoundDuplicate) THEN
    CALL ShowSevereError('Duplicate actuator was sent to SetupEMSLogicalActuator.')
  ELSE
    ! Add new actuator
    IF (numEMSActuatorsAvailable == 0) THEN
      ALLOCATE(EMSActuatorAvailable(VarsAvailableAllocInc))
      numEMSActuatorsAvailable = 1
      maxEMSActuatorsAvailable = VarsAvailableAllocInc
    ELSE
      IF (numEMSActuatorsAvailable+1 > maxEMSActuatorsAvailable) THEN
        ALLOCATE(TempEMSActuatorAvailable(maxEMSActuatorsAvailable+VarsAvailableAllocInc))
        TempEMSActuatorAvailable(1:numEMSActuatorsAvailable) = EMSActuatorAvailable(1:numEMSActuatorsAvailable)
        DEALLOCATE(EMSActuatorAvailable)
        ALLOCATE(EMSActuatorAvailable(maxEMSActuatorsAvailable+VarsAvailableAllocInc))
        EMSActuatorAvailable(1:numEMSActuatorsAvailable) = TempEMSActuatorAvailable(1:numEMSActuatorsAvailable)
        DEALLOCATE(TempEMSActuatorAvailable)
        maxEMSActuatorsAvailable=maxEMSActuatorsAvailable+VarsAvailableAllocInc
      ENDIF
      numEMSActuatorsAvailable = numEMSActuatorsAvailable + 1
    END IF

    ActuatorVariableNum = numEMSActuatorsAvailable
    EMSActuatorAvailable(ActuatorVariableNum)%ComponentTypeName = cComponentTypeName
    EMSActuatorAvailable(ActuatorVariableNum)%UniqueIDName      = cUniqueIDName
    EMSActuatorAvailable(ActuatorVariableNum)%ControlTypeName   = cControlTypeName
    EMSActuatorAvailable(ActuatorVariableNum)%Units             = cUnits
    EMSActuatorAvailable(ActuatorVariableNum)%Actuated         => lEMSActuated  ! Pointer assigment
    EMSActuatorAvailable(ActuatorVariableNum)%LogValue         => lValue        ! Pointer assigment
    EMSActuatorAvailable(ActuatorVariableNum)%PntrVarTypeUsed   = PntrLogical

  END IF

  RETURN

END SUBROUTINE SetupEMSLogicalActuator

SUBROUTINE SetupEMSRealInternalVariable(cDataTypeName, cUniqueIDName, cUnits,  rValue )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Setup internal data source and make available to EMS

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataPrecisionGlobals
  USE DataRuntimeLanguage
  USE DataInterfaces, ONLY :ShowSevereError, ShowContinueError


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  CHARACTER(len=*), INTENT(IN)  :: cDataTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName
  CHARACTER(len=*), INTENT(IN)  :: cUnits
  REAL(r64), TARGET, INTENT(IN) :: rValue
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InternalVarAvailNum ! loop index
  LOGICAL :: FoundInternalDataType
  LOGICAL :: FoundDuplicate
  TYPE(InternalVarsAvailableType), DIMENSION(:), ALLOCATABLE :: tmpEMSInternalVarsAvailable

  FoundInternalDataType = .FALSE.
  FoundDuplicate        = .FALSE.

  DO InternalVarAvailNum = 1, numEMSInternalVarsAvailable
    IF ((SameString(cDataTypeName, EMSInternalVarsAvailable(InternalVarAvailNum)%DataTypeName )) &
         .AND. (SameString(cUniqueIDName, EMSInternalVarsAvailable(InternalVarAvailNum)%UniqueIDName)) ) THEN
      FoundDuplicate = .TRUE.
      EXIT
    ENDIF
  ENDDO

  IF (FoundDuplicate) THEN
    CALL ShowSevereError('Duplicate internal variable was sent to SetupEMSInternalVariable.')
    CALL ShowContinueError('Internal variable type = '//TRIM(cDataTypeName)//' ; name = '//TRIM(cUniqueIDName) )
    CALL ShowContinueError('Called from SetupEMSRealInternalVariable.')
  ELSE
    ! add new internal data variable
    IF (numEMSInternalVarsAvailable == 0) THEN
      ALLOCATE(EMSInternalVarsAvailable(varsAvailableAllocInc))
      numEMSInternalVarsAvailable = 1
      maxEMSInternalVarsAvailable = varsAvailableAllocInc
    ELSE
      IF (numEMSInternalVarsAvailable+1 > maxEMSInternalVarsAvailable) THEN
        ALLOCATE(tmpEMSInternalVarsAvailable(maxEMSInternalVarsAvailable+varsAvailableAllocInc))
        tmpEMSInternalVarsAvailable(1:numEMSInternalVarsAvailable) = EMSInternalVarsAvailable(1:numEMSInternalVarsAvailable)
        DEALLOCATE(EMSInternalVarsAvailable)
        ALLOCATE(EMSInternalVarsAvailable(maxEMSInternalVarsAvailable+varsAvailableAllocInc))
        EMSInternalVarsAvailable(1:numEMSInternalVarsAvailable) = tmpEMSInternalVarsAvailable(1:numEMSInternalVarsAvailable)
        DEALLOCATE(tmpEMSInternalVarsAvailable)
        maxEMSInternalVarsAvailable=maxEMSInternalVarsAvailable+varsAvailableAllocInc
      ENDIF
      numEMSInternalVarsAvailable = numEMSInternalVarsAvailable + 1
    ENDIF

    InternalVarAvailNum = numEMSInternalVarsAvailable
    EMSInternalVarsAvailable(InternalVarAvailNum)%DataTypeName = cDataTypeName
    EMSInternalVarsAvailable(InternalVarAvailNum)%UniqueIDName = cUniqueIDName
    EMSInternalVarsAvailable(InternalVarAvailNum)%Units        = cUnits
    EMSInternalVarsAvailable(InternalVarAvailNum)%RealValue    => rValue
    EMSInternalVarsAvailable(InternalVarAvailNum)%PntrVarTypeUsed = PntrReal
  ENDIF

  RETURN

END SUBROUTINE SetupEMSRealInternalVariable

SUBROUTINE SetupEMSIntegerInternalVariable(cDataTypeName, cUniqueIDName, cUnits,  iValue )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Setup internal data source and make available to EMS

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataPrecisionGlobals
  USE DataRuntimeLanguage
  USE DataInterfaces, ONLY :ShowSevereError, ShowContinueError

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  CHARACTER(len=*), INTENT(IN)  :: cDataTypeName
  CHARACTER(len=*), INTENT(IN)  :: cUniqueIDName
  CHARACTER(len=*), INTENT(IN)  :: cUnits
  INTEGER, TARGET, INTENT(IN)   :: iValue
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InternalVarAvailNum ! loop index
  LOGICAL :: FoundInternalDataType
  LOGICAL :: FoundDuplicate
  TYPE(InternalVarsAvailableType), DIMENSION(:), ALLOCATABLE :: tmpEMSInternalVarsAvailable

  FoundInternalDataType = .FALSE.
  FoundDuplicate        = .FALSE.

  DO InternalVarAvailNum = 1, numEMSInternalVarsAvailable
    IF ((SameString(cDataTypeName, EMSInternalVarsAvailable(InternalVarAvailNum)%DataTypeName )) &
         .AND. (SameString(cUniqueIDName, EMSInternalVarsAvailable(InternalVarAvailNum)%UniqueIDName)) ) THEN
      FoundDuplicate = .TRUE.
      EXIT
    ENDIF
  ENDDO

  IF (FoundDuplicate) THEN
    CALL ShowSevereError('Duplicate internal variable was sent to SetupEMSInternalVariable.')
    CALL ShowContinueError('Internal variable type = '//TRIM(cDataTypeName)//' ; name = '//TRIM(cUniqueIDName) )
    CALL ShowContinueError('called from SetupEMSIntegerInternalVariable')
  ELSE
    ! add new internal data variable
    IF (numEMSInternalVarsAvailable == 0) THEN
      ALLOCATE(EMSInternalVarsAvailable(varsAvailableAllocInc))
      numEMSInternalVarsAvailable = 1
      maxEMSInternalVarsAvailable = varsAvailableAllocInc
    ELSE
      IF (numEMSInternalVarsAvailable+1 > maxEMSInternalVarsAvailable) THEN
        ALLOCATE(tmpEMSInternalVarsAvailable(maxEMSInternalVarsAvailable+varsAvailableAllocInc))
        tmpEMSInternalVarsAvailable(1:numEMSInternalVarsAvailable) = EMSInternalVarsAvailable(1:numEMSInternalVarsAvailable)
        DEALLOCATE(EMSInternalVarsAvailable)
        ALLOCATE(EMSInternalVarsAvailable(maxEMSInternalVarsAvailable+varsAvailableAllocInc))
        EMSInternalVarsAvailable(1:numEMSInternalVarsAvailable) = tmpEMSInternalVarsAvailable(1:numEMSInternalVarsAvailable)
        DEALLOCATE(tmpEMSInternalVarsAvailable)
        maxEMSInternalVarsAvailable=maxEMSInternalVarsAvailable+varsAvailableAllocInc
      ENDIF
      numEMSInternalVarsAvailable = numEMSInternalVarsAvailable + 1
    ENDIF

    InternalVarAvailNum = numEMSInternalVarsAvailable
    EMSInternalVarsAvailable(InternalVarAvailNum)%DataTypeName = cDataTypeName
    EMSInternalVarsAvailable(InternalVarAvailNum)%UniqueIDName = cUniqueIDName
    EMSInternalVarsAvailable(InternalVarAvailNum)%Units        = cUnits
    EMSInternalVarsAvailable(InternalVarAvailNum)%IntValue    => iValue
    EMSInternalVarsAvailable(InternalVarAvailNum)%PntrVarTypeUsed = PntrInteger
  ENDIF

  RETURN

END SUBROUTINE SetupEMSIntegerInternalVariable
