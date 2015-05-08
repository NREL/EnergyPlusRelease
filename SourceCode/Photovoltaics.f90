MODULE Photovoltaics
  !       MODULE INFORMATION:
  !       AUTHOR         David Bradley
  !       DATE WRITTEN   January 2003
  !       MODIFIED       B. Griffith, dec2003 - Jan2004
  !                      added Sandia PV model loosely based on G. Barker's implementation for TRNSYS type
  !                      added Simple PV efficiency model for early design phases
  !       RE-ENGINEERED  added case statement to allow selecting and mixing between different models
  !                      moved derived types to DataPhotovoltaics
  !                      B. Griffith, Aug. 2008, refactored PV data structures and input objects to
  !                       so that there is one Generator:Photovoltaics object with 3 different model options.


  ! PURPOSE OF THIS MODULE:
  ! This module collects routines used to simulate the timestep by timestep performance of a
  ! photovoltaic arrays.  The user can select between different models by choosing an a model and performance input object
  !
  ! Using the input object "PhotovoltaicPerformance:Simple" will lead to modeling the PV system using
  ! crude model that just applies a power conversion efficiency factor, much simpler to specify
  !
  ! Using the input object "PhotovoltaicPerformance:EquivalentOne-Diode" will lead to modeling the PV system using
  ! The PV model used as the basis for this module is Type180 from the HYDROGEMS library developed by
  ! Oystein Ulleberg at the IFE Institute for Energy Technology in Norway and also work by Eckstein

  ! Using the input object, "PhotovoltaicPerformance:SANDIA"  will lead to modeling a PV array
  !  using models developed by David King, Sandia National lab.  These models appear to provide
  !  improved prediction of PV performance at low radiance and incident angles.

  ! METHODOLOGY EMPLOYED: This module contains routines to manage PV system models.
  !  There are two options for what model to use and this duality of modeling approaches is
  !  reflected in there being two groups of routines for each PV model, The original model is
  !  referred to as Equivalent one-diode model and has origins as a TRNSYS type180 from the Hydrogems library
  !  A newer model with more involved input has been developed by Sandia National Lab (SNL) by David King.
  !
  !  The TRNSYS type180 model include the use of numerical routines to minimize a multivariate function


  ! REFERENCES:
  !

  ! OTHER NOTES: none

  ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataPhotovoltaics
    USE DataGlobals, ONLY: BeginSimFlag, BeginEnvrnFlag, BeginDayFlag, EndEnvrnFlag, MaxNameLength, SecInHour, KelvinConv
    USE DataInterfaces, ONLY: ShowFatalError, ShowSevereError, ShowWarningError, ShowContinueError, SetupOutputVariable
    USE DataHVACGlobals, ONLY: TimeStepSys

    IMPLICIT NONE         ! Enforce explicit typing of all variables

    PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS:
  !   see DataPhotovoltaics.f90

  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  !SUBROUTINE SPECIFICATIONS FOR MODULE Photovoltaics

    PUBLIC  :: SimPVGenerator  ! common entry point, splits to multiple types of modeling options
    PRIVATE :: GetPVInput      ! common input processing for multiple models.
    PUBLIC  :: GetPVGeneratorResults
    PRIVATE :: ReportPV

  ! The following subroutines are used for the SIMPLE model
    PRIVATE :: CalcSimplePV

  ! The following subroutines and functions are used for only the EQUIVALENT ONE-DIODE model
    PRIVATE :: InitTRNSYSPV
    PRIVATE :: CalcTRNSYSPV
    PRIVATE :: POWER
    PRIVATE :: NEWTON
    PRIVATE :: SEARCH
    PRIVATE :: FUN
    PRIVATE :: FI
    PRIVATE :: FV

  ! The following subroutines and functions are used for the Sandia model.
    PRIVATE :: CalcSandiaPV
    PRIVATE :: SandiaModuleTemperature
    PRIVATE :: SandiaTcellFromTmodule
    PRIVATE :: SandiaCellTemperature
    PRIVATE :: SandiaEffectiveIrradiance
    PRIVATE :: AbsoluteAirMass
    PRIVATE :: SandiaF1
    PRIVATE :: SandiaF2
    PRIVATE :: SandiaImp
    PRIVATE :: SandiaIsc
    PRIVATE :: SandiaIx
    PRIVATE :: SandiaIxx
    PRIVATE :: SandiaVmp
    PRIVATE :: SandiaVoc

   !  OO get set methods for coupling to exterior vented baffle cavity mounting configurations
    PRIVATE SetVentedModuleQdotSource
    PRIVATE GetExtVentedCavityIndex
    PRIVATE GetExtVentedCavityTsColl

    CONTAINS

! *************

SUBROUTINE SimPVGenerator(GeneratorType,GeneratorName,GeneratorIndex,RunFlag,PVLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         David Bradley
          !       DATE WRITTEN   April 2003
          !       MODIFIED       B. Griffith Jan 2004
          !                      B. Griffith Aug. 2008 Rework for new structure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is in charge of all the rest of the subroutines contained
          ! in this module. provides common entry point for all the models

          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
!unused0909  USE DataEnvironment, ONLY : EnvironmentName, DayOfYear
!unused0909  USE DataGlobals, ONLY: BeginEnvrnFlag, EndEnvrnFlag
  USE DataGlobalConstants, ONLY: iGeneratorPV
  USE General, ONLY: TrimSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator !unused1208
  CHARACTER(len=*), INTENT(IN)  :: GeneratorName   ! user specified name of Generator
  INTEGER, INTENT(INOUT)        :: GeneratorIndex
  LOGICAL ,         INTENT(IN)  :: RunFlag         ! is PV ON or OFF as determined by schedules in ElecLoadCenter
  REAL(r64)  ,      INTENT(IN)  :: PVLoad  ! electrical load on the PV (not really used... PV models assume "full on" !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: PVNum ! index of unit in PV array for Equivalent one-diode model
  LOGICAL,SAVE   :: GetInputFlag = .TRUE. ! one time get input flag

      !Get PV data from input file
  IF (GetInputFlag) THEN
    CALL GetPVInput  ! for all three types of models
    GetInputFlag=.FALSE.
  ENDIF

  IF (GeneratorIndex == 0) THEN
    PVNum = FindItemInList(GeneratorName, PVarray%Name, NumPVs)
    IF (PVNum == 0) THEN
       CALL ShowFatalError('SimPhotovoltaicGenerator: Specified PV not one of valid Photovoltaic Generators '//  &
                                       TRIM(GeneratorName))
    ENDIF
    GeneratorIndex=PVNum
  ELSE
    PVNum = GeneratorIndex
    IF (PVNum > NumPVs .or. PVNum < 1) THEN
      CALL ShowFatalError('SimPhotovoltaicGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(PVNum))// &
                          ', Number of PVs='//TRIM(TrimSigDigits(NumPVs))//  &
                          ', Generator name='//TRIM(GeneratorName))
    ENDIF
    IF (CheckEquipName(PVNum)) THEN
      IF (GeneratorName /= PVarray(PVNum)%Name) THEN
        CALL ShowFatalError('SimPhotovoltaicGenerator: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(PVNum))// &
                            ', Generator name='//TRIM(GeneratorName)//', stored PV Name for that index='//  &
                            TRIM(PVarray(PVNum)%Name))
      ENDIF
      CheckEquipName(PVNum)=.false.
    ENDIF
  ENDIF

  SELECT CASE (PVarray(PVNum)%PVModelType)    !SELECT and CALL MODELS based on model type

  CASE (iSimplePVModel)  !

    CALL CalcSimplePV(PVNum, RunFlag)

  CASE (iTRNSYSPVModel)
  ! 'PhotovoltaicPeformance:EquivalentOne-Diode' (aka. 5-parameter TRNSYS type 180 model)

    CALL InitTRNSYSPV(PVNum)

    CALL CalcTRNSYSPV(PVNum,RunFlag)

  CASE (iSandiaPVModel)
    ! 'PhotovoltaicPerformance:Sandia' (aka. King model, Sandia Nat. Labs.  )

    CALL CalcSandiaPV(PVNum, RunFlag)

  CASE DEFAULT

    CALL ShowFatalError('Specified generator model type not found for PV generator = '//TRIM(GeneratorName))

  END SELECT

  CALL ReportPV(PVNum)

  RETURN

END SUBROUTINE SimPVGenerator

SUBROUTINE GetPVGeneratorResults(GeneratorType,GeneratorIndex, &
                                 GeneratorPower, GeneratorEnergy, &
                                 ThermalPower  , ThermalEnergy)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         B. Griffith
    !       DATE WRITTEN   Aug. 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! provide a "get" method to collect results for individual electic load centers.

    ! METHODOLOGY EMPLOYED:
    !

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
  USE PhotovoltaicThermalCollectors  , ONLY: GetPVTThermalPowerProduction
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: GeneratorType   ! type of Generator !unused1208
  INTEGER, INTENT(IN)           :: GeneratorIndex
  REAL(r64), INTENT(OUT)        :: GeneratorPower  ! electrical power
  REAL(r64), INTENT(OUT)        :: GeneratorEnergy ! electrical energy
  REAL(r64), INTENT(OUT)        :: ThermalPower
  REAL(r64), INTENT(OUT)        :: ThermalEnergy


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  GeneratorPower  = PVarray(GeneratorIndex)%Report%DCPower
  GeneratorEnergy = PVarray(GeneratorIndex)%Report%DCEnergy
  ! PVT may add thermal
  If (PVArray(GeneratorIndex)%CellIntegrationMode == iPVTSolarCollectorCellIntegration) Then
    ! get result for thermal power generation
    Call GetPVTThermalPowerProduction(GeneratorIndex, ThermalPower, ThermalEnergy)
  ELSE
    ThermalPower  = 0.0D0
    ThermalEnergy = 0.0D0
  ENDIF

  RETURN

END SUBROUTINE GetPVGeneratorResults

! *************

SUBROUTINE GetPVInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         David Bradley
          !       DATE WRITTEN   January 2003
          !       MODIFIED       B.Griffith Dec. 2003 - Jan 2004 added input for Simple and Sandia PV model
          !                      B. Griffith Feb. 2008 - revised input for TRNSYS pv model for BIPV and inverter
          !                      B. Griffith Aug. 2008 - revised input for new organization and naming convention
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the Photovoltaic units saving it in
          ! the data structures defined in DataPhotovoltaics.f90.

          ! METHODOLOGY EMPLOYED:
          ! subroutine structure taken from Beta2 BaseboardRadiator.f90

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, &
                            SameString
  USE DataIPShortCuts
  USE DataGlobals,     ONLY: DegToRadians, KelvinConv
!unused0909  USE DataEnvironment, ONLY: Longitude, TimeZoneMeridian
  USE DataSurfaces,    ONLY: Surface, TotSurfaces, ExternalEnvironment,  &
                             SurfaceClass_Shading, SurfaceClass_Detached_F, SurfaceClass_Detached_B
  USE DataHeatBalance
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE TranspiredCollector, ONLY: GetTranspiredCollectorIndex
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PVNum   ! working variable for do loop through pv arrays
  INTEGER :: SurfNum ! working variable for surface id in Heat Balance domain
  INTEGER :: ModNum  ! working variable for do loop through Sandia model parameter input
  INTEGER :: NumAlphas ! Number of PV Array parameter alpha names being passed
  INTEGER :: NumNums   ! Number of PV Array numeric parameters are being passed
  INTEGER :: IOSTAT
  LOGICAL :: ErrorsFound = .FALSE.   ! if errors detected in input
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
  INTEGER :: ThisParamObj !
  INTEGER :: dupPtr

  TYPE(SimplePVParamsStruct),        ALLOCATABLE, DIMENSION(:) :: tmpSimpleModuleParams ! temporary, for processing input data
  TYPE(TRNSYSPVModuleParamsStruct),  ALLOCATABLE, DIMENSION(:) :: tmpTNRSYSModuleParams ! temporary, for processing input data
  TYPE(SNLModuleParamsStuct),        ALLOCATABLE, DIMENSION(:) :: tmpSNLModuleParams  ! temporary, for processing input data

  ! count how many photovoltaic arrays of different types are in the .idf
  NumPVs                 = GetNumObjectsFound(cPVGeneratorObjectName       )
  NumSimplePVModuleTypes = GetNumObjectsFound(cPVSimplePerfObjectName      )
  Num1DiodePVModuleTypes = GetNumObjectsFound(cPVEquiv1DiodePerfObjectName )
  NumSNLPVModuleTypes    = GetNumObjectsFound(cPVSandiaPerfObjectName      )

  IF (NumPVs <= 0) THEN
    CALL ShowSevereError('Did not find any '//Trim(cPVGeneratorObjectName) )
    RETURN
  ENDIF

  IF (.NOT. ALLOCATED(PVarray)) ALLOCATE(PVarray(NumPVs))
  ALLOCATE(CheckEquipName(NumPVs))
  CheckEquipName=.true.

  cCurrentModuleObject = cPVGeneratorObjectName
  Do PVnum = 1, NumPVs
    CALL GetObjectItem(cCurrentModuleObject,PVnum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOSTAT, &
                      AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames )
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1), PVarray%Name, PVnum -1, IsNotOK, IsBlank,  TRIM(cCurrentModuleObject)//' Name' )
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    ENDIF
    PVarray(PVNum)%Name = cAlphaArgs(1)

    PVarray(PVNum)%SurfaceName = cAlphaArgs(2)
    PVarray(PVNum)%SurfacePtr  = FindItemInList(cAlphaArgs(2),Surface%Name,TotSurfaces)
    ! required-surface
    IF (lAlphaFieldBlanks(2)) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      CALL ShowContinueError('Surface name cannot be blank')
      ErrorsFound=.TRUE.
    ENDIF
    IF (PVarray(PVNum)%SurfacePtr == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
      ErrorsFound=.TRUE.
    ELSE
      ! Found one -- make sure has right parameters for PV
      SurfNum = PVarray(PVNum)%SurfacePtr
      Surface(SurfNum)%isPV = .TRUE.

      IF (.NOT. Surface(SurfNum)%ExtSolar) THEN
        CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError( 'Surface is not exposed to solar, check surface bounday condition')
      END IF

      ! check surface orientation, warn if upside down
      IF (( Surface(SurfNum)%Tilt < -95.0D0 ) .OR. (Surface(SurfNum)%Tilt > 95.0D0)) THEN
        CALL ShowWarningError('Suspected input problem with '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError( 'Surface used for solar collector faces down')
        CALL ShowContinueError('Surface tilt angle (degrees from ground outward normal) = ' &
                                   //TRIM(RoundSigDigits(Surface(SurfNum)%Tilt,2) ) )
      ENDIF
    ENDIF

    PVArray(PVNum)%PVModelType = iNotYetSetPVModel
    IF (SameString(cAlphaArgs(3), cPVSimplePerfObjectName)) THEN
      PVArray(PVNum)%PVModelType = iSimplePVModel
    ELSEIF( SameString(cAlphaArgs(3), cPVEquiv1DiodePerfObjectName)) Then
      PVArray(PVNum)%PVModelType =  iTRNSYSPVModel
    ELSEIF ( SameString(cAlphaArgs(3), cPVSandiaPerfObjectName)) THEN
      PVArray(PVNum)%PVModelType = iSandiaPVModel
    ELSE ! throw error, did not find module performance type
      If (lAlphaFieldBlanks(3)) then
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Field cannot be blank')
        ErrorsFound=.TRUE.
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Did not recognize entry')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF
    PVArray(PVNum)%PerfObjName = cAlphaArgs(4) ! check later once perf objects are loaded

    PVArray(PVNum)%CellIntegrationMode = iNotYetSetCellIntegration
    If (SameString(cAlphaArgs(5), 'Decoupled')) Then
      PVArray(PVNum)%CellIntegrationMode = iDecoupledCellIntegration
    ELSEIF(SameString(cAlphaArgs(5), 'DecoupledUllebergDynamic')) Then
      PVArray(PVNum)%CellIntegrationMode = iDecoupledUllebergDynamicCellIntegration
    ELSEIF(SameString(cAlphaArgs(5), 'IntegratedSurfaceOutsideFace')) Then
      PVArray(PVNum)%CellIntegrationMode = iSurfaceOutsideFaceCellIntegration
    ELSEIF(SameString(cAlphaArgs(5), 'IntegratedTranspiredCollector')) Then
      PVArray(PVNum)%CellIntegrationMode = iTranspiredCollectorCellIntegration
    ELSEIF(SameString(cAlphaArgs(5), 'IntegratedExteriorVentedCavity')) Then
      PVArray(PVNum)%CellIntegrationMode = iExteriorVentedCavityCellIntegration
    ELSEIF(SameString(cAlphaArgs(5), 'PhotovoltaicThermalSolarCollector')) Then
      PVArray(PVNum)%CellIntegrationMode = iPVTSolarCollectorCellIntegration
    ELSE
      IF (lAlphaFieldBlanks(5)) then
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Field cannot be blank')
        ErrorsFound=.TRUE.
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//' = '//TRIM(cAlphaArgs(5)) )
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
        CALL ShowContinueError('Did not recognize entry')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

    PVArray(PVNum)%NumSeriesNParall = rNumericArgs(1)
    PVArray(PVNum)%NumModNSeries    = rNumericArgs(2)

  ENDDO ! main PV array objects

  ! search for duplicate PV arrays on integrated heat transfer surfaces, accumulating source terms across arrays is not supported
  DO PVnum = 1, NumPVs
    IsNotOK = .FALSE.
    SELECT CASE (PVArray(PVNum)%CellIntegrationMode)

    CASE (iSurfaceOutsideFaceCellIntegration, iTranspiredCollectorCellIntegration, iExteriorVentedCavityCellIntegration )
      dupPtr = FindItemInList(PVarray(PVNum)%SurfaceName, PVarray(PVNum+1:NumPVs)%SurfaceName, (NumPVs - PVnum) )
      IF (dupPtr /= 0) dupPtr = dupPtr + PVNum ! to correct for shortened array in find item
      IF (dupPtr /= 0) THEN
        IF (PVArray(dupPtr)%CellIntegrationMode == iSurfaceOutsideFaceCellIntegration) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': problem detected with multiple PV arrays.')
          CALL ShowContinueError('When using IntegratedSurfaceOutsideFace heat transfer mode, only one PV array can be coupled')
          CALL ShowContinueError('Both '//TRIM(PVarray(PVNum)%Name)//' and '//TRIM(PVarray(dupPtr)%Name)//' are using surface ' &
                                    //TRIM(PVarray(PVNum)%SurfaceName) )
           ErrorsFound=.TRUE.
        ELSEIF (PVArray(dupPtr)%CellIntegrationMode == iTranspiredCollectorCellIntegration) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': problem detected with multiple PV arrays.')
          CALL ShowContinueError('When using IntegratedTranspiredCollector heat transfer mode, only one PV array can be coupled')
          CALL ShowContinueError('Both '//TRIM(PVarray(PVNum)%Name)//' and '//TRIM(PVarray(dupPtr)%Name)//' are using UTSC ' &
                                   //'surface = ' //TRIM(PVarray(PVNum)%SurfaceName) )
           ErrorsFound=.TRUE.
        ELSEIF (PVArray(dupPtr)%CellIntegrationMode ==iExteriorVentedCavityCellIntegration) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': problem detected with multiple PV arrays.')
          CALL ShowContinueError('When using IntegratedExteriorVentedCavity heat transfer mode, only one PV array can be coupled')
          CALL ShowContinueError('Both '//TRIM(PVarray(PVNum)%Name)//' and '//TRIM(PVarray(dupPtr)%Name)//' are using exterior ' &
                                   //'vented surface = '   //TRIM(PVarray(PVNum)%SurfaceName) )
           ErrorsFound=.TRUE.
        ENDIF
      ENDIF
    END select
  ENDDO

  IF (NumSimplePVModuleTypes > 0) THEN
    ALLOCATE(tmpSimpleModuleParams(NumSimplePVModuleTypes))
    cCurrentModuleObject = cPVSimplePerfObjectName
    Do ModNum=1,NumSimplePVModuleTypes
      CALL GetObjectItem(cCurrentModuleObject, ModNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, &
                      AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),tmpSimpleModuleParams%name,ModNum-1,IsNotOK,IsBlank, &
                      Trim(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN !repeat or blank name so don't add
        ErrorsFound=.TRUE.
        CYCLE
      ENDIF
      tmpSimpleModuleParams(ModNum)%Name            = cAlphaArgs(1)
      tmpSimpleModuleParams(ModNum)%ActiveFraction  = rNumericArgs(1)

      If (SameString(cAlphaArgs(2), 'Fixed')) Then
        tmpSimpleModuleParams(ModNum)%EfficencyInputMode = FixedEfficiency
      ELSEIF( SameString(cAlphaArgs(2), 'Scheduled')) Then
        tmpSimpleModuleParams(ModNum)%EfficencyInputMode = ScheduledEfficiency
      ELSE
        IF (lAlphaFieldBlanks(2)) then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError('Field cannot be blank')
          ErrorsFound=.TRUE.
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError('Did not recognize entry')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF
      tmpSimpleModuleParams(ModNum)%PVEfficiency = rNumericArgs(2)

      tmpSimpleModuleParams(ModNum)%EffSchedPtr = GetScheduleIndex(cAlphaArgs(3))
      If ( (tmpSimpleModuleParams(ModNum)%EffSchedPtr == 0) .AND.  &
         ( tmpSimpleModuleParams(ModNum)%EfficencyInputMode == ScheduledEfficiency) ) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//' = '//TRIM(cAlphaArgs(3)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError('Did not find schedule')
          ErrorsFound=.TRUE.
      ENDIF
    ENDDO
  ENDIF !

  IF (Num1DiodePVModuleTypes > 0) Then
    ALLOCATE(tmpTNRSYSModuleParams(Num1DiodePVModuleTypes) )
    cCurrentModuleObject =cPVEquiv1DiodePerfObjectName
    Do ModNum=1,Num1DiodePVModuleTypes
      CALL GetObjectItem(cCurrentModuleObject, ModNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, &
                      AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames )

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),tmpTNRSYSModuleParams%name,ModNum-1,IsNotOK,IsBlank, &
                      Trim(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN !repeat or blank name so don't add
        ErrorsFound=.TRUE.
        CYCLE
      ENDIF
      tmpTNRSYSModuleParams(ModNum)%Name                 = cAlphaArgs(1)
      IF (SameString(cAlphaArgs(2), 'CrystallineSilicon')) THEN
        tmpTNRSYSModuleParams(ModNum)%CellType = CrystallineSiPVCells
      ELSEIF (SameString(cAlphaArgs(2), 'AmorphousSilicon')) THEN
        tmpTNRSYSModuleParams(ModNum)%CellType = AmorphousSiPVCells
      ELSE
        IF (lAlphaFieldBlanks(2)) then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError('Field cannot be blank')
          ErrorsFound=.TRUE.
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
          CALL ShowContinueError('Did not recognize entry')
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF

      tmpTNRSYSModuleParams(ModNum)%CellsInSeries        = INT(rNumericArgs(1))
      tmpTNRSYSModuleParams(ModNum)%Area                 = rNumericArgs(2)
      tmpTNRSYSModuleParams(ModNum)%TauAlpha             = rNumericArgs(3)
      tmpTNRSYSModuleParams(ModNum)%SemiConductorBandgap = rNumericArgs(4)
      tmpTNRSYSModuleParams(ModNum)%ShuntResistance      = rNumericArgs(5)
      tmpTNRSYSModuleParams(ModNum)%RefIsc               = rNumericArgs(6)
      tmpTNRSYSModuleParams(ModNum)%RefVoc               = rNumericArgs(7)
      tmpTNRSYSModuleParams(ModNum)%RefTemperature       = rNumericArgs(8) + KelvinConv
      tmpTNRSYSModuleParams(ModNum)%RefInsolation        = rNumericArgs(9)
      tmpTNRSYSModuleParams(ModNum)%Imp                  = rNumericArgs(10)
      tmpTNRSYSModuleParams(ModNum)%Vmp                  = rNumericArgs(11)
      tmpTNRSYSModuleParams(ModNum)%TempCoefIsc          = rNumericArgs(12)
      tmpTNRSYSModuleParams(ModNum)%TempCoefVoc          = rNumericArgs(13)
      tmpTNRSYSModuleParams(ModNum)%NOCTAmbTemp          = rNumericArgs(14)+ KelvinConv
      tmpTNRSYSModuleParams(ModNum)%NOCTCellTemp         = rNumericArgs(15)+ KelvinConv
      tmpTNRSYSModuleParams(ModNum)%NOCTInsolation       = rNumericArgs(16)
      tmpTNRSYSModuleParams(ModNum)%HeatLossCoef         = rNumericArgs(17)
      tmpTNRSYSModuleParams(ModNum)%HeatCapacity         = rNumericArgs(18)

    ENDDO
  ENDIF

  IF(NumSNLPVModuleTypes > 0) THEN
    ALLOCATE(tmpSNLModuleParams(NumSNLPVModuleTypes))
    cCurrentModuleObject = cPVSandiaPerfObjectName
    Do ModNum=1, NumSNLPVModuleTypes

      CALL GetObjectItem(cCurrentModuleObject, ModNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT, &
                      AlphaBlank=lAlphaFieldBlanks, AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames )

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),tmpSNLModuleParams%name,ModNum-1,IsNotOK,IsBlank, &
                      Trim(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN !repeat or blank name so don't add
        ErrorsFound=.TRUE.
        CYCLE
      ENDIF

      tmpSNLModuleParams(ModNum)%Name         = cAlphaArgs(1)
      tmpSNLModuleParams(ModNum)%Acoll        = rNumericArgs(1)
      tmpSNLModuleParams(ModNum)%NcellSer     = rNumericArgs(2)
      tmpSNLModuleParams(ModNum)%NparSerCells = rNumericArgs(3)
      tmpSNLModuleParams(ModNum)%Isc0         = rNumericArgs(4)
      tmpSNLModuleParams(ModNum)%Voc0         = rNumericArgs(5)
      tmpSNLModuleParams(ModNum)%Imp0         = rNumericArgs(6)
      tmpSNLModuleParams(ModNum)%Vmp0         = rNumericArgs(7)
      tmpSNLModuleParams(ModNum)%aIsc         = rNumericArgs(8)
      tmpSNLModuleParams(ModNum)%aImp         = rNumericArgs(9)
      tmpSNLModuleParams(ModNum)%c_0          = rNumericArgs(10)
      tmpSNLModuleParams(ModNum)%c_1          = rNumericArgs(11)
      tmpSNLModuleParams(ModNum)%BVoc0        = rNumericArgs(12)
      tmpSNLModuleParams(ModNum)%mBVoc        = rNumericArgs(13)
      tmpSNLModuleParams(ModNum)%BVmp0        = rNumericArgs(14)
      tmpSNLModuleParams(ModNum)%mBVmp        = rNumericArgs(15)
      tmpSNLModuleParams(ModNum)%DiodeFactor  = rNumericArgs(16)
      tmpSNLModuleParams(ModNum)%c_2          = rNumericArgs(17)
      tmpSNLModuleParams(ModNum)%c_3          = rNumericArgs(18)
      tmpSNLModuleParams(ModNum)%a_0          = rNumericArgs(19)
      tmpSNLModuleParams(ModNum)%a_1          = rNumericArgs(20)
      tmpSNLModuleParams(ModNum)%a_2          = rNumericArgs(21)
      tmpSNLModuleParams(ModNum)%a_3          = rNumericArgs(22)
      tmpSNLModuleParams(ModNum)%a_4          = rNumericArgs(23)
      tmpSNLModuleParams(ModNum)%b_0          = rNumericArgs(24)
      tmpSNLModuleParams(ModNum)%b_1          = rNumericArgs(25)
      tmpSNLModuleParams(ModNum)%b_2          = rNumericArgs(26)
      tmpSNLModuleParams(ModNum)%b_3          = rNumericArgs(27)
      tmpSNLModuleParams(ModNum)%b_4          = rNumericArgs(28)
      tmpSNLModuleParams(ModNum)%b_5          = rNumericArgs(29)
      tmpSNLModuleParams(ModNum)%DT0          = rNumericArgs(30)
      tmpSNLModuleParams(ModNum)%fd           = rNumericArgs(31)
      tmpSNLModuleParams(ModNum)%a            = rNumericArgs(32)
      tmpSNLModuleParams(ModNum)%b            = rNumericArgs(33)
      tmpSNLModuleParams(ModNum)%c_4          = rNumericArgs(34)
      tmpSNLModuleParams(ModNum)%c_5          = rNumericArgs(35)
      tmpSNLModuleParams(ModNum)%Ix0          = rNumericArgs(36)
      tmpSNLModuleParams(ModNum)%Ixx0         = rNumericArgs(37)
      tmpSNLModuleParams(ModNum)%c_6          = rNumericArgs(38)
      tmpSNLModuleParams(ModNum)%c_7          = rNumericArgs(39)

    ENDDO
  ENDIF

  ! now fill collector performance data into main PV structure
  DO PVnum = 1, NumPVs

    SELECT CASE (PVArray(PVNum)%PVModelType)

    CASE (iSimplePVModel)

       ThisParamObj = FindItemInList(PVArray(PVNum)%PerfObjName, tmpSimpleModuleParams%Name, NumSimplePVModuleTypes)
       IF (ThisParamObj > 0) THEN
         PVArray(PVNum)%SimplePVModule = tmpSimpleModuleParams(ThisParamObj)  !entire structure assignment

         ! do one-time setups on input data
         PVArray(PVNum)%SimplePVModule%AreaCol = Surface(PVArray(PVNum)%SurfacePtr)%Area &
                                          * PVArray(PVNum)%SimplePVModule%ActiveFraction
       ELSE
         CALL ShowSevereError('Invalid PV performance object name of '//TRIM(PVArray(PVNum)%PerfObjName) )
         CALL ShowContinueError('Entered in '//TRIM(cPVGeneratorObjectName)//' = '//TRIM(PVArray(PVNum)%Name) )
         ErrorsFound = .TRUE.
       ENDIF

    CASE (iTRNSYSPVModel)

       ThisParamObj = FindItemInList(PVArray(PVNum)%PerfObjName, tmpTNRSYSModuleParams%Name, Num1DiodePVModuleTypes)
       IF (ThisParamObj > 0) THEN
         PVArray(PVNum)%TRNSYSPVModule = tmpTNRSYSModuleParams(ThisParamObj)  !entire structure assignment
       ELSE
         CALL ShowSevereError('Invalid PV performance object name of '//TRIM(PVArray(PVNum)%PerfObjName) )
         CALL ShowContinueError('Entered in '//TRIM(cPVGeneratorObjectName)//' = '//TRIM(PVArray(PVNum)%Name) )
         ErrorsFound = .TRUE.
       ENDIF

    CASE (iSandiaPVModel)

       ThisParamObj = FindItemInList(PVArray(PVNum)%PerfObjName, tmpSNLModuleParams%Name, NumSNLPVModuleTypes)
       IF (ThisParamObj > 0) THEN
         PVArray(PVNum)%SNLPVModule = tmpSNLModuleParams(ThisParamObj)  !entire structure assignment
       ELSE
         CALL ShowSevereError('Invalid PV performance object name of '//TRIM(PVArray(PVNum)%PerfObjName) )
         CALL ShowContinueError('Entered in '//TRIM(cPVGeneratorObjectName)//' = '//TRIM(PVArray(PVNum)%Name) )
         ErrorsFound = .TRUE.
       ENDIF
    END SELECT

    !set up report variables CurrentModuleObject='Photovoltaics'
    CALL SetupOutputVariable('Generator Produced DC Electric Power [W]', &
                 PVArray(PVNum)%Report%DCPower,'System','Average', &
                 PVarray(PVNum)%Name)
    CALL SetupOutputVariable('Generator Produced DC Electric Energy [J]', &
                 PVArray(PVNum)%Report%DCEnergy,'System','Sum', &
                 PVarray(PVNum)%Name)
    CALL SetupOutputVariable('Generator PV Array Efficiency []', &
                 PVArray(PVNum)%Report%ArrayEfficiency,'System','Average', &
                 PVarray(PVNum)%Name)

    ! CurrentModuleObject='Equiv1Diode or Sandia Photovoltaics'
    IF ((PVArray(PVNum)%PVModelType == iTRNSYSPVModel) .OR. (PVArray(PVNum)%PVModelType == iSandiaPVModel)) THEN
      CALL SetupOutputVariable('Generator PV Cell Temperature [C]', &
             PVArray(PVNum)%Report%CellTemp,'System','Average', &
             PVarray(PVNum)%Name)
      CALL SetupOutputVariable('Generator PV Short Circuit Current [A]', &
             PVArray(PVNum)%Report%ArrayIsc,'System','Average', &
             PVarray(PVNum)%Name)
      CALL SetupOutputVariable('Generator PV Open Circuit Voltage [V]', &
             PVArray(PVNum)%Report%ArrayVoc,'System','Average', &
             PVarray(PVNum)%Name)
    ENDIF

    ! do some checks and setup
    IF ( PVArray(PVNum)%PVModelType == iSurfaceOutsideFaceCellIntegration ) THEN
           !check that surface is HeatTransfer and a Construction with Internal Source was used
          IF (.NOT. Surface(PVArray(PVNum)%SurfacePTR)%HeatTransSurf) THEN
            CALL ShowSevereError('Must use a surface with heat transfer for IntegratedSurfaceOutsideFace mode in '&
                        //TRIM(PVArray(PVNum)%Name))
            ErrorsFound = .TRUE.
          ELSEIF (.NOT. Construct(Surface(PVArray(PVNum)%SurfacePTR)%Construction)%SourceSinkPresent) THEN
            CALL ShowSevereError('Must use a surface with internal source construction for IntegratedSurfaceOutsideFace mode in '&
                        //TRIM(PVArray(PVNum)%Name))
            ErrorsFound = .TRUE.
          ENDIF
    ENDIF

    IF (PVArray(PVNum)%CellIntegrationMode == iTranspiredCollectorCellIntegration) THEN
      CALL GetTranspiredCollectorIndex( PVArray(PVNum)%SurfacePtr , PVArray(PVNum)%UTSCPtr )
    ENDIF

    IF (PVArray(PVNum)%CellIntegrationMode == iExteriorVentedCavityCellIntegration) THEN
      CALL GetExtVentedCavityIndex( PVArray(PVNum)%SurfacePtr , PVArray(PVNum)%ExtVentCavPtr)
    ENDIF

    IF (PVArray(PVNum)%CellIntegrationMode == iPVTSolarCollectorCellIntegration) THEN
      ! Call GetPVTmodelIndex(  PVArray(PVNum)%SurfacePtr , PVArray(PVNum)%PVTPtr )
    ENDIF

  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting photovoltaic input')
  ENDIF

  RETURN
END SUBROUTINE GetPVInput
! **************************************
SUBROUTINE CalcSimplePV(thisPV, RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan. 2004
          !       MODIFIED       B. Griffith, Aug. 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate the electricity production using a simple PV model

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    USE DataHeatBalance, ONLY: QRadSWOutIncident
    USE ScheduleManager, ONLY: GetCurrentScheduleValue
    USE DataHVACGlobals, ONLY: TimeStepSys
    USE DataGlobals,     ONLY: SecInHour
    USE DataSurfaces,    ONLY: Surface


    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: thisPV
    LOGICAL, INTENT(IN) :: RunFlag !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER   :: thisSurf ! working index ptr to Surface arrays
    REAL(r64) :: Eff      ! working variable for solar electric efficiency
!unused1208    REAL(r64) :: ArrayEnergy !working variable for PV energy this system time step
    !first get surface index to use as a pointer
    thisSurf = PVArray(thisPV)%SurfacePtr

    IF (QRadSWOutIncident(thisSurf) > MinIrradiance) then

       !get efficiency
      SELECT CASE (PVArray(thisPV)%SimplePVModule%EfficencyInputMode)

      CASE (FixedEfficiency)

        Eff = PVArray(thisPV)%SimplePVModule%PVEfficiency

      CASE (ScheduledEfficiency) ! get from schedule

        Eff = GetCurrentScheduleValue(PVArray(thisPV)%SimplePVModule%EffSchedPtr )
        PVArray(thisPV)%SimplePVModule%PVEfficiency = Eff

      CASE DEFAULT
        call showSevereError('caught bad Mode in Generator:Photovoltaic:Simple use FIXED or SCHEDULED efficiency mode')
      END SELECT

      PVArray(thisPV)%Report%DCPower =         &
         PVArray(thisPV)%SimplePVModule%AreaCol        & ! active solar cellsurface net area
         * Eff                            & ! solar conversion efficiency
         * QRadSWOutIncident(thisSurf)      ! solar incident

       ! store sink term in appropriate place for surface heat transfer itegration
      PVArray(thisPV)%SurfaceSink = PVArray(thisPV)%Report%DCPower

        ! array energy, power * timestep
      PVArray(thisPV)%Report%DCEnergy = PVArray(thisPV)%Report%DCPower * (TimeStepSys * SecInHour)
      PVArray(thisPV)%Report%ArrayEfficiency = Eff
    ELSE  !not enough incident solar, zero things out

      PVArray(thisPV)%SurfaceSink    = 0.0d0
      PVArray(thisPV)%Report%DCEnergy = 0.0d0
      PVArray(thisPV)%Report%DCPower  = 0.0d0
      PVArray(thisPV)%Report%ArrayEfficiency = 0.0d0

    ENDIF

  RETURN

END SUBROUTINE CalcSimplePV

SUBROUTINE ReportPV(PVnum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan. 2004
          !       MODIFIED       B. Griffith, Aug. 2008
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! collect statements that assign to variables tied to output variables

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataHeatBalance, ONLY: zone
  USE DataSurfaces   , ONLY: surface
  USE DataHeatBalFanSys, only: QPVSysSource
  USE TranspiredCollector, ONLY: SetUTSCQdotSource

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PVNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: thisZone ! working index for zones

  PVArray(PVNum)%Report%DCEnergy  = PVArray(PVNum)%Report%DCPower*(TimeStepSys*SecInHour)

    ! add check for multiplier.  if surface is attached to a zone that is on a multiplier
    ! then PV production should be multiplied out as well

  IF (surface(PVArray(PVNum)%SurfacePtr)%Zone /= 0) then ! might need to apply multiplier
     thisZone =  surface(PVArray(PVNum)%SurfacePtr)%Zone
     PVArray(PVNum)%Report%DCEnergy = PVArray(PVNum)%Report%DCEnergy &
                  * (Zone(thisZone)%Multiplier * Zone(thisZone)%ListMultiplier)
     PVArray(PVNum)%Report%DCPower = PVArray(PVNum)%Report%DCPower  &
                  * (Zone(thisZone)%Multiplier * Zone(thisZone)%ListMultiplier)
  ENDIF

  SELECT CASE (PVArray(PVNum)%CellIntegrationMode)
  ! SurfaceSink is not multiplied...
  CASE (iSurfaceOutsideFaceCellIntegration)
    QPVSysSource(PVArray(PVNum)%SurfacePtr) = -1.0D0 * PVArray(PVNum)%SurfaceSink

  CASE (iTranspiredCollectorCellIntegration)
    CALL SetUTSCQdotSource(PVArray(PVNum)%UTSCPtr, -1.0D0 * PVArray(PVNum)%SurfaceSink )

  CASE ( iExteriorVentedCavityCellIntegration)
    CALL SetVentedModuleQdotSource(PVArray(PVNum)%ExtVentCavPtr, -1.0D0 * PVArray(PVNum)%SurfaceSink )

  CASE (iPVTSolarCollectorCellIntegration)
    ! CALL SetPVTQdotSource(PVArray(PVNum)%ExtVentCavPtr,  -1 * PVArray(PVNum)%SurfaceSink )

  END SELECT

  RETURN

END SUBROUTINE ReportPV


! *************
SUBROUTINE CalcSandiaPV(PVNum, runFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith , (derived from Greg Barker's TRNSYS type101 for SANDIA PV model)
          !       DATE WRITTEN   Jan 2004
          !       MODIFIED       B. Griffith, Aug. 2008 reworked for new, single-PV-generator data structure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate various PV system peformance indicies at the current timestep


          ! METHODOLOGY EMPLOYED:
          !  adapted code from a set of F77 routines by G. Barker that implement the model
          !  This routines works on a single photovoltaic object of the type 'GENERATOR:PV:SANDIA'
          !  Each major model equation has its own function (in this module)

          ! REFERENCES:
          ! King, David L. . Photovoltaic module and array performance characterization methods for all
          !   system operating conditions. Pro. NREL/SNL Photovoltaics Program Review, AIP Press, Lakewood CO
          !   Sandia National Laboratories

          ! Davis, M.W., A.H. Fanney, and B.P. Dougherty. Measured versus predicted performance of Building
          !    integrated photovoltaics. Solar 2002, Sunrise on the Reliable Energy Economy, June 15-19, 2002 Reno, NV

          ! USE STATEMENTS:
          !
    USE DataGlobals,     ONLY: DegToRadians
    USE DataEnvironment, ONLY: Elevation, SOLCOS
    USE DataHeatBalance, ONLY: CosIncidenceAngle, QRadSWOutIncidentBeam, QRadSWOutIncident
    USE DataHeatBalSurface, ONLY: TempSurfOut
    USE DataSurfaces, ONLY: Surface
    USE TranspiredCollector, ONLY: GetUTSCTsColl

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: PVNum ! ptr to current PV system
    LOGICAL, INTENT(IN) :: RunFlag  ! controls if generator is scheduled *ON*

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: thisSurf ! working variable for indexing surfaces
!unused1208    INTEGER :: thisMod  ! working variable for indexing module parameters
    REAL(r64)    :: Ee

    thisSurf = PVarray(PVNum)%SurfacePtr

    !   get input from elsewhere in Energyplus for the current point in the simulation
    PVArray(PVNum)%SNLPVinto%IcBeam = QRadSWOutIncidentBeam(thisSurf)  !(W/m2)from DataHeatBalance
    PVArray(PVNum)%SNLPVinto%IcDiffuse        = QRadSWOutIncident(thisSurf) - QRadSWOutIncidentBeam(thisSurf) !(W/ m2)(was kJ/hr m2)
    PVArray(PVNum)%SNLPVinto%IncidenceAngle   = ACOS(CosIncidenceAngle(thisSurf) )/DegToRadians ! (deg) from dataHeatBalance
    PVArray(PVNum)%SNLPVinto%ZenithAngle      = ACOS (SOLCOS(3))/DegToRadians !(degrees),
    PVArray(PVNum)%SNLPVinto%Tamb             = Surface(thisSurf)%OutDryBulbTemp  !(deg. C)
    PVArray(PVNum)%SNLPVinto%WindSpeed        = Surface(thisSurf)%WindSpeed  ! (m/s)
    PVArray(PVNum)%SNLPVinto%Altitude         = Elevation   ! from DataEnvironment via USE

    IF (((PVArray(PVNum)%SNLPVinto%IcBeam+ PVArray(PVNum)%SNLPVinto%IcDiffuse) > MinIrradiance) &
           .AND. (RunFlag) ) THEN

      ! first determine PV cell temperatures depending on model
      SELECT CASE (PVarray(PVNum)%CellIntegrationMode)

      CASE (iDecoupledCellIntegration)  ! Sandia module temperature model for rack mounted PVs
        ! Calculate back-of-module temperature:
        PVarray(PVNum)%SNLPVCalc%Tback = SandiaModuleTemperature(PVArray(PVNum)%SNLPVinto%IcBeam, &
                                                                 PVArray(PVNum)%SNLPVinto%IcDiffuse, &
                                                                 PVArray(PVNum)%SNLPVinto%WindSpeed, &
                                                                 PVArray(PVNum)%SNLPVinto%Tamb, &
                                                                 PVArray(PVNum)%SNLPVModule%fd, &
                                                                 PVArray(PVNum)%SNLPVmodule%a, &
                                                                 PVArray(PVNum)%SNLPVmodule%b  )

        ! Calculate cell temperature:
        PVarray(PVNum)%SNLPVCalc%Tcell = SandiaTcellFromTmodule(PVarray(PVNum)%SNLPVCalc%Tback,PVArray(PVNum)%SNLPVinto%IcBeam, &
                                        PVArray(PVNum)%SNLPVinto%IcDiffuse, PVArray(PVNum)%SNLPVmodule%fd, &
                                        PVArray(PVNum)%SNLPVmodule%DT0)

      CASE (iSurfaceOutsideFaceCellIntegration)
        ! get back-of-module temperature from elsewhere in EnergyPlus
        PVarray(PVNum)%SNLPVCalc%Tback = TempSurfOut(PVArray(PVNum)%SurfacePtr)

        PVarray(PVNum)%SNLPVCalc%Tcell = SandiaTcellFromTmodule(PVarray(PVNum)%SNLPVCalc%Tback,PVArray(PVNum)%SNLPVinto%IcBeam, &
                                        PVArray(PVNum)%SNLPVinto%IcDiffuse, PVArray(PVNum)%SNLPVmodule%fd, &
                                        PVArray(PVNum)%SNLPVmodule%DT0)

      CASE (iTranspiredCollectorCellIntegration)
        CAll GetUTSCTsColl(PVArray(PVNum)%UTSCPtr, PVarray(PVNum)%SNLPVCalc%Tback)

        PVarray(PVNum)%SNLPVCalc%Tcell = SandiaTcellFromTmodule(PVarray(PVNum)%SNLPVCalc%Tback,PVArray(PVNum)%SNLPVinto%IcBeam, &
                                        PVArray(PVNum)%SNLPVinto%IcDiffuse, PVArray(PVNum)%SNLPVmodule%fd, &
                                        PVArray(PVNum)%SNLPVmodule%DT0)

      CASE (iExteriorVentedCavityCellIntegration)
        CALL GetExtVentedCavityTsColl(PVArray(PVNum)%ExtVentCavPtr, &
                           PVarray(PVNum)%SNLPVCalc%Tback)

        PVarray(PVNum)%SNLPVCalc%Tcell = SandiaTcellFromTmodule(PVarray(PVNum)%SNLPVCalc%Tback,PVArray(PVNum)%SNLPVinto%IcBeam, &
                                        PVArray(PVNum)%SNLPVinto%IcDiffuse, PVArray(PVNum)%SNLPVmodule%fd, &
                                        PVArray(PVNum)%SNLPVmodule%DT0)

      CASE (iPVTSolarCollectorCellIntegration)
        ! add calls to PVT models here

      CASE DEFAULT
        Call ShowSevereError('Sandia PV Simulation Temperature Modeling Mode Error in ' &
                                       //TRIM(PVArray(PVNum)%Name) )

      END SELECT

      ! Calculate Air Mass function
      PVarray(PVNum)%SNLPVCalc%AMa = AbsoluteAirMass(PVArray(PVNum)%SNLPVinto%ZenithAngle, PVArray(PVNum)%SNLPVinto%Altitude)

      ! Calculate F1 polynomial function:
      PVarray(PVNum)%SNLPVCalc%F1 = SandiaF1(PVarray(PVNum)%SNLPVCalc%AMa,PVArray(PVNum)%SNLPVmodule%a_0,   &
                           PVArray(PVNum)%SNLPVmodule%a_1,PVArray(PVNum)%SNLPVmodule%a_2,   &
                           PVArray(PVNum)%SNLPVmodule%a_3,PVArray(PVNum)%SNLPVmodule%a_4)

     ! Calculate F2 polynomial function:
      PVarray(PVNum)%SNLPVCalc%F2 = SandiaF2(PVArray(PVNum)%SNLPVinto%IncidenceAngle,PVArray(PVNum)%SNLPVmodule%b_0, &
                            PVArray(PVNum)%SNLPVmodule%b_1,PVArray(PVNum)%SNLPVmodule%b_2,             &
                            PVArray(PVNum)%SNLPVmodule%b_3,PVArray(PVNum)%SNLPVmodule%b_4,             &
                            PVArray(PVNum)%SNLPVmodule%b_5)

     ! Calculate short-circuit current function:
      PVarray(PVNum)%SNLPVCalc%Isc = SandiaIsc( PVarray(PVNum)%SNLPVCalc%Tcell, PVArray(PVNum)%SNLPVmodule%Isc0,            &
                           PVArray(PVNum)%SNLPVinto%IcBeam, PVArray(PVNum)%SNLPVinto%IcDiffuse, PVarray(PVNum)%SNLPVCalc%F1, &
                           PVarray(PVNum)%SNLPVCalc%F2, PVArray(PVNum)%SNLPVmodule%fd, PVArray(PVNum)%SNLPVmodule%aIsc )

     ! Calculate effective irradiance function:
      Ee = SandiaEffectiveIrradiance(PVarray(PVNum)%SNLPVCalc%Tcell,PVarray(PVNum)%SNLPVCalc%Isc,   &
                        PVArray(PVNum)%SNLPVmodule%Isc0, PVArray(PVNum)%SNLPVmodule%aIsc)
     ! Calculate Imp function:
      PVarray(PVNum)%SNLPVCalc%Imp = SandiaImp(PVarray(PVNum)%SNLPVCalc%Tcell, Ee, PVArray(PVNum)%SNLPVmodule%Imp0,   &
                        PVArray(PVNum)%SNLPVmodule%aImp,PVArray(PVNum)%SNLPVmodule%c_0,               &
                        PVArray(PVNum)%SNLPVmodule%c_1)

     ! Calculate Voc function:
      PVarray(PVNum)%SNLPVCalc%Voc = SandiaVoc(PVarray(PVNum)%SNLPVCalc%Tcell, Ee, PVArray(PVNum)%SNLPVmodule%Voc0, &
                        PVArray(PVNum)%SNLPVmodule%NcellSer,PVArray(PVNum)%SNLPVmodule%DiodeFactor, &
                        PVArray(PVNum)%SNLPVmodule%BVoc0,PVArray(PVNum)%SNLPVmodule%mBVoc)

     ! Calculate Vmp: voltagea at maximum powerpoint
      PVarray(PVNum)%SNLPVCalc%Vmp = SandiaVmp(PVarray(PVNum)%SNLPVCalc%Tcell,Ee,PVArray(PVNum)%SNLPVmodule%Vmp0,       &
                            PVArray(PVNum)%SNLPVmodule%NcellSer,PVArray(PVNum)%SNLPVmodule%DiodeFactor, &
                            PVArray(PVNum)%SNLPVmodule%BVmp0,PVArray(PVNum)%SNLPVmodule%mBVmp,          &
                            PVArray(PVNum)%SNLPVmodule%c_2,PVArray(PVNum)%SNLPVmodule%c_3)

     ! Calculate Ix function:
      PVarray(PVNum)%SNLPVCalc%Ix  = SandiaIx(PVarray(PVNum)%SNLPVCalc%Tcell,Ee,PVArray(PVNum)%SNLPVmodule%Ix0, &
                            PVArray(PVNum)%SNLPVmodule%aIsc,PVArray(PVNum)%SNLPVmodule%aImp,    &
                            PVArray(PVNum)%SNLPVmodule%c_4,PVArray(PVNum)%SNLPVmodule%c_5)


     ! Calculate Vx function:
      PVarray(PVNum)%SNLPVCalc%Vx = PVarray(PVNum)%SNLPVCalc%Voc/2.0d0

     ! Calculate Ixx function:
      PVarray(PVNum)%SNLPVCalc%Ixx = SandiaIxx(PVarray(PVNum)%SNLPVCalc%Tcell,Ee,PVArray(PVNum)%SNLPVmodule%Ixx0, &
                            PVArray(PVNum)%SNLPVmodule%aImp,PVArray(PVNum)%SNLPVmodule%c_6, &
                            PVArray(PVNum)%SNLPVmodule%c_7)
     ! Calculate Vxx :
      PVarray(PVNum)%SNLPVCalc%Vxx = 0.5d0*(PVarray(PVNum)%SNLPVCalc%Voc+PVarray(PVNum)%SNLPVCalc%Vmp)

     ! Calculate Pmp, single module: power at maximum powerpoint
      PVarray(PVNum)%SNLPVCalc%Pmp = PVarray(PVNum)%SNLPVCalc%Imp * PVarray(PVNum)%SNLPVCalc%Vmp ! W

     ! Calculate PV efficiency at maximum power point
      PVarray(PVNum)%SNLPVCalc%EffMax = PVarray(PVNum)%SNLPVCalc%Pmp/  &
         (PVArray(PVNum)%SNLPVinto%IcBeam+PVArray(PVNum)%SNLPVinto%IcDiffuse)/PVArray(PVNum)%SNLPVmodule%Acoll

     ! Scale to NumStrings and NumSeries:
      PVarray(PVNum)%SNLPVCalc%Pmp = PVarray(PVNum)%SNLPVCalc%Pmp * PVarray(PVNum)%NumSeriesNParall &
                                            * PVarray(PVNum)%NumModNSeries
      PVarray(PVNum)%SNLPVCalc%Imp = PVarray(PVNum)%SNLPVCalc%Imp * PVarray(PVNum)%NumModNSeries
      PVarray(PVNum)%SNLPVCalc%Vmp = PVarray(PVNum)%SNLPVCalc%Vmp * PVarray(PVNum)%NumModNSeries
      PVarray(PVNum)%SNLPVCalc%Isc = PVarray(PVNum)%SNLPVCalc%Isc * PVarray(PVNum)%NumSeriesNParall
      PVarray(PVNum)%SNLPVCalc%Voc = PVarray(PVNum)%SNLPVCalc%Voc * PVarray(PVNum)%NumModNSeries
      PVarray(PVNum)%SNLPVCalc%Ix  = PVarray(PVNum)%SNLPVCalc%Ix  * PVarray(PVNum)%NumSeriesNParall
      PVarray(PVNum)%SNLPVCalc%Ixx = PVarray(PVNum)%SNLPVCalc%Ixx * PVarray(PVNum)%NumSeriesNParall
      PVarray(PVNum)%SNLPVCalc%Vx  = PVarray(PVNum)%SNLPVCalc%Vx  * PVarray(PVNum)%NumModNSeries
      PVarray(PVNum)%SNLPVCalc%Vxx = PVarray(PVNum)%SNLPVCalc%Vxx * PVarray(PVNum)%NumModNSeries
      PVarray(PVNum)%SNLPVCalc%SurfaceSink = PVarray(PVNum)%SNLPVCalc%Pmp
    ELSE ! Ibeam+Idiff < MaxIrradiance or not RunFlag
      ! so zero things.
      PVarray(PVNum)%SNLPVCalc%Vmp=0.0d0
      PVarray(PVNum)%SNLPVCalc%Imp=0.0d0
      PVarray(PVNum)%SNLPVCalc%Pmp=0.0d0
      PVarray(PVNum)%SNLPVCalc%EffMax=0.0d0
      PVarray(PVNum)%SNLPVCalc%Isc=0.0d0
      PVarray(PVNum)%SNLPVCalc%Voc=0.0d0
      PVarray(PVNum)%SNLPVCalc%Tcell=PVArray(PVNum)%SNLPVinto%Tamb
      PVarray(PVNum)%SNLPVCalc%Tback=PVArray(PVNum)%SNLPVinto%Tamb
      PVarray(PVNum)%SNLPVCalc%AMa=999.0d0
      PVarray(PVNum)%SNLPVCalc%F1=0.0d0
      PVarray(PVNum)%SNLPVCalc%F2=0.0d0
      PVarray(PVNum)%SNLPVCalc%Ix=0.0d0
      PVarray(PVNum)%SNLPVCalc%Vx=0.0d0
      PVarray(PVNum)%SNLPVCalc%Ixx=0.0d0
      PVarray(PVNum)%SNLPVCalc%Vxx=0.0d0
      PVarray(PVNum)%SNLPVCalc%SurfaceSink = 0.0d0
    ENDIF !Ibeam+Idiff > MinIrradiance and runflag

    ! update calculations to report variables
    PVarray(PVNum)%Report%DCPower = PVarray(PVNum)%SNLPVCalc%Pmp
    PVarray(PVNum)%Report%ArrayIsc = PVarray(PVNum)%SNLPVCalc%Isc
    PVarray(PVNum)%Report%ArrayVoc = PVarray(PVNum)%SNLPVCalc%Voc
    PVarray(PVNum)%Report%CellTemp = PVarray(PVNum)%SNLPVCalc%Tcell
    PVarray(PVNum)%Report%ArrayEfficiency = PVarray(PVNum)%SNLPVCalc%EffMax
    PVarray(PVNum)%SurfaceSink  = PVarray(PVNum)%SNLPVCalc%SurfaceSink

    RETURN

END SUBROUTINE CalcSandiaPV


! ********************
! begin routines for Equivalent one-diode model by Bradley/Ulleberg

SUBROUTINE InitTRNSYSPV(PVNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         David Bradley
          !       DATE WRITTEN   April 2003
          !       MODIFIED       BG March 2007 reworked for CR7109 (reverse DD testing)
          !                      B. Griffith, Aug. 2008 reworked for new, single-PV-generator data structure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the PV arrays during simulation. It performs both start of
          ! simulation initializations and start of timestep initializations. The structure of the
          ! subroutine was taken from InitBaseboard.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE DataPhotovoltaics, ONLY:CellTemp,LastCellTemp
  USE DataHeatBalance, ONLY:QRadSWOutIncident
  USE DataSurfaces, ONLY: Surface, TotSurfaces
  Use DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE DataGlobals    , ONLY: TimeStep, TimeStepZone, SecInHour, BeginEnvrnFlag, HourOfDay
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PVNum              !the number of the GENERATOR:PHOTOVOLTAICS (passed in)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  REAL(r64)    :: TimeElapsed         ! Fraction of the current hour that has elapsed (h)

  ! perform the one time initializations
  IF (MyOneTimeFlag) THEN
  ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumPVs))
    MyEnvrnFlag = .true.
    MyOneTimeFlag = .false.
  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(PVNum)) THEN

    PVarray(PVNum)%TRNSYSPVcalc%CellTempK     = Surface(PVarray(PVNum)%SurfacePtr)%OutDryBulbTemp + KelvinConv
    PVarray(PVNum)%TRNSYSPVcalc%LastCellTempK = Surface(PVarray(PVNum)%SurfacePtr)%OutDryBulbTemp + KelvinConv
    MyEnvrnFlag(PVNum) = .FALSE.
  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(PVNum) = .TRUE.
  END IF

  ! Do the beginning of every time step initializations
  TimeElapsed = HourOfDay + TimeStep * TimeStepZone + SysTimeElapsed
  IF (PVarray(PVNum)%TRNSYSPVcalc%TimeElapsed /= TimeElapsed) THEN
   ! The simulation has advanced to the next system timestep.  Save conditions from the end of the previous system
    PVarray(PVNum)%TRNSYSPVcalc%LastCellTempK = PVarray(PVNum)%TRNSYSPVcalc%CellTempK
    PVarray(PVNum)%TRNSYSPVcalc%TimeElapsed = TimeElapsed
  END IF

  IF (ANY(QRadSWOutIncident > 0.0D0)) THEN
   !  Determine the amount of radiation incident on each PV
    PVarray(PVNum)%TRNSYSPVcalc%Insolation = QRadSWOutIncident(PVarray(PVNum)%SurfacePtr)  ![W/m2]
  ELSE
    PVarray(PVNum)%TRNSYSPVcalc%Insolation = 0.0D0
  ENDIF

  RETURN
END SUBROUTINE InitTRNSYSPV

! *************

SUBROUTINE CalcTRNSYSPV(PVNum,RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D. Bradley
          !       DATE WRITTEN   April 2003
          !       MODIFIED       B. Griffith, February 2008-- added support for inverter
          !                      multipliers, and building integrated heat transfer
          !                      B. Griffith, Aug. 2008 reworked for new, single-PV-generator data structure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the PV performance.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY:SecInHour,MinutesPerTimeStep
  USE DataSurfaces, ONLY: Surface
!  USE DataPhotovoltaics, ONLY:CellTemp,LastCellTemp
  USE DataHeatBalSurface, ONLY: TempSurfOut
  USE TranspiredCollector, ONLY: GetUTSCTsColl
  USE DataHeatBalance , ONLY: Zone
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE FUNCTION DECLARATIONS:


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PVNum   !BTG added intent
  LOGICAL, INTENT(IN) :: RunFlag !BTG added intent    !flag tells whether the PV is ON or OFF

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: EPS=0.001d0
  REAL(r64), PARAMETER :: ERR=0.001d0
  REAL(r64), PARAMETER :: MinInsolation=30.0d0
  INTEGER, PARAMETER :: CCMAX=10
  INTEGER, PARAMETER :: KMAX=100
  REAL(r64), PARAMETER :: EtaIni = 0.10d0   !initial value of eta

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64),SAVE :: PVTimeStep      !internal timestep (in seconds) for cell temperature mode 3
  REAL(r64) :: DummyErr        !
  REAL(r64) :: ETA,Tambient,EtaOld,ILRef,AARef,IORef,SeriesResistance,IL,AA,IO,ISCG1,ISC,VOCG1,VOC
  REAL(r64) :: VLEFT,VRIGHT,VM,IM,PM,IA,ISCA,VA,VOCA,PA
  INTEGER :: CC,K
  REAL(r64) :: CellTemp  ! cell temperature in Kelvin
  REAL(r64) :: CellTempC       !cell temperature in degrees C
  LOGICAL, SAVE :: FirstTime=.true.
!unused1208  INTEGER :: thisZone

! if the cell temperature mode is 2, convert the timestep to seconds
  IF(FirstTime .and. PVarray(PVNum)%CellIntegrationMode == iDecoupledUllebergDynamicCellIntegration) THEN
    PVTimeStep = REAL(MinutesPerTimeStep,r64)*60.0d0   !Seconds per time step
  ENDIF
  FirstTime=.false.

! place the shunt resistance into its common block
  ShuntResistance = PVarray(PVNum)%TRNSYSPVModule%ShuntResistance

! convert ambient temperature from C to K
  Tambient = Surface(PVarray(PVNum)%SurfacePtr)%OutDryBulbTemp + KelvinConv

  IF ((PVarray(PVNum)%TRNSYSPVcalc%Insolation .GT. MinInsolation).AND.(RunFlag)) THEN

! set initial values for eta iteration loop
    DummyErr = 2.0d0*ERR
    CC = 1
    EtaOld = EtaIni

! Begin DO WHILE loop - until the error tolerance is reached.
    ETA = 0.0d0
    DO WHILE (DummyErr .GT. ERR)

      SELECT CASE (PVarray(PVNum)%CellIntegrationMode)

      CASE (iDecoupledCellIntegration)
        !  cell temperature based on energy balance
        PVarray(PVNum)%TRNSYSPVModule%HeatLossCoef = PVarray(PVNum)%TRNSYSPVModule%TauAlpha*  &
           PVarray(PVNum)%TRNSYSPVModule%NOCTInsolation/    &
           (PVarray(PVNum)%TRNSYSPVModule%NOCTCellTemp-PVarray(PVNum)%TRNSYSPVModule%NOCTAmbTemp)
        CellTemp = Tambient+(PVarray(PVNum)%TRNSYSPVcalc%Insolation*PVarray(PVNum)%TRNSYSPVModule%TauAlpha/  &
           PVarray(PVNum)%TRNSYSPVModule%HeatLossCoef)*(1.0d0-ETA/PVarray(PVNum)%TRNSYSPVModule%TauAlpha)
      CASE (iDecoupledUllebergDynamicCellIntegration)
         !  cell temperature based on energy balance with thermal capacity effects
        CellTemp = Tambient+(PVarray(PVNum)%TRNSYSPVcalc%LastCellTempK-Tambient)*  &
           EXP(-PVarray(PVNum)%TRNSYSPVModule%HeatLossCoef/PVarray(PVNum)%TRNSYSPVModule%HeatCapacity*PVTimeStep)  + &
           (PVarray(PVNum)%TRNSYSPVModule%TauAlpha-ETA)*PVarray(PVNum)%TRNSYSPVcalc%Insolation /    &
           PVarray(PVNum)%TRNSYSPVModule%HeatLossCoef*(1.0d0-EXP(-PVarray(PVNum)%TRNSYSPVModule%HeatLossCoef /   &
              PVarray(PVNum)%TRNSYSPVModule%HeatCapacity*PVTimeStep))
      CASE (iSurfaceOutsideFaceCellIntegration)
        CellTemp = TempSurfOut(PVArray(PVNum)%SurfacePtr) + KelvinConv
      CASE (iTranspiredCollectorCellIntegration)
        CAll GetUTSCTsColl(PVArray(PVNum)%UTSCPtr, CellTemp)
        CellTemp = CellTemp + KelvinConv
      CASE (iExteriorVentedCavityCellIntegration)
        CALL GetExtVentedCavityTsColl(PVArray(PVNum)%ExtVentCavPtr, CellTemp )
        CellTemp = CellTemp + KelvinConv
      CASE (iPVTSolarCollectorCellIntegration)
        ! get PVT model result for cell temp..

      END SELECT

!  reference parameters
      ILRef = PVarray(PVNum)%TRNSYSPVModule%RefIsc
      AARef = (PVarray(PVNum)%TRNSYSPVModule%TempCoefVoc*PVarray(PVNum)%TRNSYSPVModule%RefTemperature-  &
         PVarray(PVNum)%TRNSYSPVModule%RefVoc+PVarray(PVNum)%TRNSYSPVModule%SemiConductorBandgap*    &
         PVarray(PVNum)%TRNSYSPVModule%CellsInSeries)/(PVarray(PVNum)%TRNSYSPVModule%TempCoefIsc *   &
         PVarray(PVNum)%TRNSYSPVModule%RefTemperature/ILRef-3.0d0)
      IORef = ILRef*EXP(-PVarray(PVNum)%TRNSYSPVModule%RefVoc/AARef)

!  series resistance
      SeriesResistance = (AARef*LOG(1.0d0-PVarray(PVNum)%TRNSYSPVModule%Imp/ILRef)-  &
                          PVarray(PVNum)%TRNSYSPVModule%Vmp+PVarray(PVNum)%TRNSYSPVModule%RefVoc) /   &
                             PVarray(PVNum)%TRNSYSPVModule%Imp

!  temperature depencence
      IL = PVarray(PVNum)%TRNSYSPVcalc%Insolation/PVarray(PVNum)%TRNSYSPVModule%RefInsolation*  &
         (ILRef+PVarray(PVNum)%TRNSYSPVModule%TempCoefIsc*(CellTemp-PVarray(PVNum)%TRNSYSPVModule%RefTemperature))
      AA = AARef*CellTemp/PVarray(PVNum)%TRNSYSPVModule%RefTemperature
      IO = IORef*(CellTemp/PVarray(PVNum)%TRNSYSPVModule%RefTemperature)**3*  &
         EXP(PVarray(PVNum)%TRNSYSPVModule%SemiConductorBandgap*    &
         PVarray(PVNum)%TRNSYSPVModule%CellsInSeries/AARef*(1.0d0-PVarray(PVNum)%TRNSYSPVModule%RefTemperature/CellTemp))


!  compute short curcuit current and open circuit voltage

!   NEWTON --> ISC  (STARTVALUE: ISCG1 - BASED ON IL=ISC)
      ISCG1 = IL
      CALL NEWTON(ISC,FUN,FI,ISC,constant_zero,IO,IL,SeriesResistance,AA,ISCG1,EPS)

!   NEWTON --> VOC  (STARTVALUE: VOCG1 - BASED ON IM=0.0)
      VOCG1 = (LOG(IL/IO)+1.0d0)*AA
      CALL NEWTON(VOC,FUN,FV,constant_zero,VOC,IO,IL,SeriesResistance,AA,VOCG1,EPS)

!  maximum power point tracking

!   SEARCH --> VM AT MAXIMUM POWER POINT
      VLEFT  = 0.0d0
      VRIGHT = VOC
      CALL SEARCH(VLEFT,VRIGHT,VM,K,IO,IL,SeriesResistance,AA,EPS,KMAX)

!   POWER --> IM & PM AT MAXIMUM POWER POINT
      CALL POWER(IO,IL,SeriesResistance,AA,EPS,IM,VM,PM)

! calculate overall PV module efficiency
      ETA = PM/PVarray(PVNum)%TRNSYSPVcalc%Insolation/PVarray(PVNum)%TRNSYSPVModule%Area
      DummyErr = ABS((ETA-EtaOld)/EtaOld)
      ETAOLD = ETA
      CC=CC+1

    END DO

! end of DO WHILE loop.

  ELSE
! if there is no incident radiation or if the control switch is 'Off'
    Select Case (PVarray(PVNum)%CellIntegrationMode)
    Case (iDecoupledCellIntegration)
      CellTemp = Tambient
    CASE (iDecoupledUllebergDynamicCellIntegration)
      CellTemp = Tambient+(PVarray(PVNum)%TRNSYSPVcalc%LastCellTempK-Tambient) &
               *EXP(-PVarray(PVNum)%TRNSYSPVModule%HeatLossCoef/PVarray(PVNum)%TRNSYSPVModule%HeatCapacity*PVTimeStep)
    CASE (iSurfaceOutsideFaceCellIntegration)
      CellTemp = TempSurfOut(PVarray(PVNum)%SurfacePtr)+ KelvinConv
    CASE (iTranspiredCollectorCellIntegration)
      CAll GetUTSCTsColl(PVarray(PVNum)%UTSCPtr, CellTemp)
      CellTemp = CellTemp + KelvinConv
    CASE (iExteriorVentedCavityCellIntegration)
      CALL GetExtVentedCavityTsColl(PVarray(PVNum)%ExtVentCavPtr, CellTemp )
      CellTemp = CellTemp + KelvinConv
    CASE (iPVTSolarCollectorCellIntegration)
        ! get PVT model result for cell temp..
    END SELECT

    PVarray(PVNum)%TRNSYSPVcalc%Insolation = 0.0d0
    IM = 0.0d0   !module current
    VM = 0.0d0   !module voltage
    PM = 0.0d0   !module power
    ETA = 0.0d0  !module efficiency
    ISC = 0.0d0
    VOC = 0.0d0

  END IF

! convert cell temperature back to C
  CellTempC = CellTemp - KelvinConv

! calculate array based outputs (so far, the outputs are module based
  IA   = PVarray(PVNum)%NumSeriesNParall*IM
  ISCA = PVarray(PVNum)%NumSeriesNParall*ISC
  VA   = PVarray(PVNum)%NumModNSeries*VM
  VOCA = PVarray(PVNum)%NumModNSeries*VOC
  PA   = IA*VA

! Place local variables into the reporting structure
  PVarray(PVNum)%TRNSYSPVcalc%ArrayCurrent = IA
  PVarray(PVNum)%TRNSYSPVcalc%ArrayVoltage = VA
  PVarray(PVNum)%TRNSYSPVcalc%ArrayPower   = PA
  PVarray(PVNum)%Report%DCPower            = PA
  PVarray(PVNum)%TRNSYSPVcalc%ArrayEfficiency = ETA
  PVarray(PVNum)%Report%ArrayEfficiency    = ETA
  PVarray(PVNum)%TRNSYSPVcalc%CellTemp     = CellTempC
  PVarray(PVNum)%Report%CellTemp           = CellTempC
  PVarray(PVNum)%TRNSYSPVcalc%CellTempK    = CellTemp
  PVarray(PVNum)%TRNSYSPVcalc%ArrayIsc     = ISCA
  PVarray(PVNum)%Report%ArrayIsc           = ISCA
  PVarray(PVNum)%TRNSYSPVcalc%ArrayVoc     = VOCA
  PVarray(PVNum)%Report%ArrayVoc           = VOCA
  PVarray(PVNum)%SurfaceSink               = PA


  CONTINUE

  RETURN
END SUBROUTINE CalcTRNSYSPV


SUBROUTINE POWER(IO,IL,RSER,AA,EPS,II,VV,PP)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
          !       DATE WRITTEN   March 2001
          !       MODIFIED       D. Bradley for use with EnergyPlus
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the power produced by the PV.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE FUNCTION DECLARATIONS:
          ! na

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) :: IO    !passed in from CalcPV
  REAL(r64) :: IL    !passed in from CalcPV
  REAL(r64) :: RSer  !passed in from CalcPV
  REAL(r64) :: AA    !passed in from CalcPV
  REAL(r64) :: EPS   !passed in from CalcPV

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: II    !current [A]
  REAL(r64) :: VV    !voltage [V]
  REAL(r64) :: PP    !power [W]
  REAL(r64) :: IG1

! NEWTON --> II (STARTVALUE: IG1 BASED ON SIMPLIFIED I(I,V) EQUATION)
  IG1 = IL-IO*EXP(VV/AA-1.0d0)
  CALL NEWTON(II,FUN,FI,II,VV,IO,IL,RSER,AA,IG1,EPS)
  PP = II*VV
  RETURN

END SUBROUTINE POWER


SUBROUTINE NEWTON(XX,FXX,DER,II,VV,IO,IL,RSER,AA,XS,EPS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
          !       DATE WRITTEN   March 2001
          !       MODIFIED       D. Bradley for use with EnergyPlus
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine uses the Newton-Raphson method to solve a non linear equation with one variable.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), EXTERNAL :: FXX
  REAL(r64), EXTERNAL :: DER
  REAL(r64) XX,II,VV,IO,IL,RSER,AA,XS,EPS

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: CCMAX=10

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER COUNT
  REAL(r64) ERR,X0

  COUNT = 0
  XX  = XS
  ERR = 1.0d0
  DO WHILE ((ERR .GT. EPS) .AND. (COUNT .LE. 10))
    X0  = XX
    XX  = XX-FXX(II,VV,IL,IO,RSER,AA)/DER(II,VV,IO,RSER,AA)
    COUNT = COUNT + 1
    ERR = ABS((XX-X0)/X0)
  ENDDO
  RETURN

END SUBROUTINE NEWTON


SUBROUTINE SEARCH(A,B,P,K,IO,IL,RSER,AA,EPS,KMAX)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
          !       DATE WRITTEN   March 2001
          !       MODIFIED       D. Bradley for use with EnergyPlus
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine minimum of an unimodal function with one variable. The algorithm was
          ! adapted to find the maximum power point of a PV module. The changes to the original
          ! algorithm are the following:
          ! 1. a subroutine "POWER" is called in order to calculate the power output of the PV module
          ! 2. the negative of the power of the PV module is taken so that the optimum can be found.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          !   /1/ MATHEWS, JOHN H.  NUMERICAL METHODS:  FORTRAN PROGRAMS. 1992, PP 413.
          !   /2/ NUMERICAL METHODS FOR MATHEMATICS, SCIENCE AND ENGINEERING, 2ND EDITION,
          !       PRENTICE HALL, NEW JERSEY, 1992.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER K, KMAX

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: DELTA=1.d-3
  REAL(r64), PARAMETER :: EPSILON=1.d-3

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) A,B,C,D,H,P,RONE,RTWO,YP,YA,YB,YC,YD
  REAL(r64) IO,IL,RSER,AA,EPS,IM,PM

  RONE=(SQRT(5.0d0)-1.d0)/2.d0
  RTWO=RONE*RONE
  H=B-A
  CALL POWER(IO,IL,RSER,AA,EPS,IM,A,PM)
  YA=-1.0d0*PM
  CALL POWER(IO,IL,RSER,AA,EPS,IM,B,PM)
  YB=-1.0d0*PM
  C=A+RTWO*H
  D=A+RONE*H
  CALL POWER(IO,IL,RSER,AA,EPS,IM,C,PM)
  YC=-1.0d0*PM
  CALL POWER(IO,IL,RSER,AA,EPS,IM,D,PM)
  YD=-1.0d0*PM
  K=1
  DO WHILE (ABS(YB-YA).GT.EPSILON .OR. H.GT.DELTA)
    IF (YC.LT.YD) THEN
      B=D
      YB=YD
      D=C
      YD=YC
      H=B-A
      C=A+RTWO*H
      CALL POWER(IO,IL,RSER,AA,EPS,IM,C,PM)
      YC=-1.0d0*PM
    ELSE
      A=C
      YA=YC
      C=D
      YC=YD
      H=B-A
      D=A+RONE*H
      CALL POWER(IO,IL,RSER,AA,EPS,IM,D,PM)
      YD=-1.0d0*PM
    ENDIF
    K=K+1
  END DO
  IF (K.LT.KMAX) THEN
    P=A
    YP=YA
    IF (YB.LT.YA) THEN
      P=B
      YP=YB
    ENDIF
    RETURN
  ELSE
    RETURN
  ENDIF
  END SUBROUTINE SEARCH


REAL(r64) FUNCTION FUN(II,VV,IL,IO,RSER,AA)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
          !       DATE WRITTEN   March 2001
          !       MODIFIED       D. Bradley for EnergyPlus
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! This function is based on the current-voltage characteristic of the PV module and is of the
          ! form f(I,V)=0

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY:RoundSigDigits
  USE DataInterfaces, ONLY:ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) II,VV,IL,IO,RSER,AA

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (((VV+II*RSER)/AA) < 700.0D0) THEN

    FUN = II-IL+IO*(EXP((VV+II*RSER)/AA)-1.0d0)-((VV+II*RSER)/ShuntResistance)
  ELSE

    Call ShowSevereError('EquivalentOneDiode Photovoltaic model failed to find maximum power point')
    Call ShowContinueError('Numerical solver failed trying to take exponential of too large a number')
    Call ShowContinueError('Check input data in '//cPVEquiv1DiodePerfObjectName)
    Call ShowContinueError('VV (voltage) = '//Trim(RoundSigDigits(VV, 5)) )
    Call ShowContinueError('II (current) = '//Trim(RoundSigDigits(II, 5)) )
    Call ShowFatalError('FUN: EnergyPlus terminates because of numerical problem in EquivalentOne-Diode PV model')

  ENDIF

  RETURN
END FUNCTION FUN


REAL(r64) FUNCTION FI(II,VV,IO,RSER,AA)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
          !       DATE WRITTEN   March 2001
          !       MODIFIED       D. Bradley for EnergyPlus
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! partial differential of I=I(I,V)

          ! METHODOLOGY EMPLOYED:
          ! the function is based on the current voltage characteristic of the PV module and is of
          ! the form dF(I,V)/dI=0

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY:RoundSigDigits
  USE DataInterfaces, ONLY:ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) II,VV,IO,RSER,AA

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (((VV+II*RSER)/AA) < 700.0D0) THEN

    FI = 1.0d0+IO*EXP((VV+II*RSER)/AA)*RSER/AA+(RSER/ShuntResistance)

  ELSE

    Call ShowSevereError('EquivalentOneDiode Photovoltaic model failed to find maximum power point')
    Call ShowContinueError('Numerical solver failed trying to take exponential of too large a number')
    Call ShowContinueError('Check input data in '//cPVEquiv1DiodePerfObjectName)
    Call ShowContinueError('VV (voltage) = '//Trim(RoundSigDigits(VV, 5)) )
    Call ShowContinueError('II (current) = '//Trim(RoundSigDigits(II, 5)) )
    Call ShowFatalError('FI: EnergyPlus terminates because of numerical problem in EquivalentOne-Diode PV model')

  ENDIF

  RETURN

END FUNCTION FI


REAL(r64) FUNCTION FV(II,VV,IO,RSER,AA)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Ø. Ulleberg, IFE Norway for Hydrogems
          !       DATE WRITTEN   March 2001
          !       MODIFIED       D. Bradley for EnergyPlus
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! partial differential of V=I(I,V)

          ! METHODOLOGY EMPLOYED:
          ! the function is based on the current voltage characteristic of the PV module and is of
          ! the form dF(I,V)/dV=0

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY:RoundSigDigits
  USE DataInterfaces, ONLY:ShowFatalError, ShowSevereError, ShowContinueError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64) II,VV,IO,RSER,AA

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (((VV+II*RSER)/AA) < 700.0D0) THEN

    FV = IO*EXP((VV+II*RSER)/AA)/AA+(1.0d0/ShuntResistance)

  ELSE

    Call ShowSevereError('EquivalentOneDiode Photovoltaic model failed to find maximum power point')
    Call ShowContinueError('Numerical solver failed trying to take exponential of too large a number')
    Call ShowContinueError('Check input data in '//cPVEquiv1DiodePerfObjectName)
    Call ShowContinueError('VV (voltage) = '//Trim(RoundSigDigits(VV, 5)) )
    Call ShowContinueError('II (current) = '//Trim(RoundSigDigits(II, 5)) )
    Call ShowFatalError('FI: EnergyPlus terminates because of numerical problem in EquivalentOne-Diode PV model')


  ENDIF
  RETURN

  END FUNCTION FV


! End routines for Equivalent One-Diode model as implemented by Bradley
!************************************************************************
!
! Begin supporting routines for Sandia PV model
! -------------------------------------------------------------------------------
  REAL(r64) FUNCTION SandiaModuleTemperature(Ibc,Idc,Ws,Ta,fd,a,b)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  B.Griffith December 2003

          ! PURPOSE OF THIS FUNCTION:
          ! Returns back-of-module temperature, deg C

          ! METHODOLOGY EMPLOYED:
          ! apply sandia temperature model, This is module temp or back of
          ! of the panel.  A seperate correction handles delta T for actual cell

          ! REFERENCES:
          ! from G. Barker's TRNSYS implementation
          ! Equations (10)  in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Ibc ! beam radiation on collector plane, W/m2
    REAL(r64), INTENT(IN) :: Idc ! Diffuse radiation on collector plane, W/m2
    REAL(r64), INTENT(IN) :: Ws  ! wind speed, m/s
    REAL(r64), INTENT(IN) :: Ta  ! ambient temperature, degC
    REAL(r64), INTENT(IN) :: fd  ! fraction of Idc used (empirical constant)
    REAL(r64), INTENT(IN) :: a   ! empirical constant
    REAL(r64), INTENT(IN) :: b   ! empirical constant

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:


     REAL(r64)  :: E ! total irradiance working variable

     E = Ibc + fd * Idc

     SandiaModuleTemperature = E * Exp(a + b * Ws) + Ta

  END Function SandiaModuleTemperature
    ! -------------------------------------------------------------------------------
    ! -------------------------------------------------------------------------------
  REAL(r64) Function SandiaTcellFromTmodule(Tm,Ibc,Idc,fd,DT0)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Jan 2004 F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns cell temperature, deg C

          ! METHODOLOGY EMPLOYED:
          ! This is for the Sandia model method of determining cell temperatures
          ! module temperature differs from solar cell temperature
          ! because panel temperatures are not uniform

          ! REFERENCES:
          !Equations (11) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tm  ! module temperature (deg C)
    REAL(r64), INTENT(IN) :: Ibc ! beam radiation on collector plane, W/m2
    REAL(r64), INTENT(IN) :: Idc ! Diffuse radiation on collector plane, W/m2
    REAL(r64), INTENT(IN) :: fd  ! fraction of Idc used (empirical constant)
    REAL(r64), INTENT(IN) :: DT0 ! (Tc-Tm) at E=1000 W/m2 (empirical constant known as delta T), deg C

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

     REAL(r64)  :: E ! total irradiance working variable

     E = Ibc + fd * Idc

     SandiaTcellFromTmodule = Tm + (E / 1000.0d0) * DT0

     RETURN
  END FUNCTION SandiaTcellFromTmodule
! -------------------------------------------------------------------------------

  REAL(r64) Function SandiaCellTemperature(Ibc,Idc,Ws,Ta,fd,a,b,DT0)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   unknown
          !       MODIFIED
          !       RE-ENGINEERED  B. Griffith, Jan 2004 F77-> f90

          ! PURPOSE OF THIS FUNCTION:
          !  Returns cell temperature, deg C
          !
          ! METHODOLOGY EMPLOYED:
          ! is this even used?  duplicates separate functions above.
          ! combines function SandiaTcellFromTmodule with
          !  SandiaModuleTemperature

          ! REFERENCES:
          ! Equations (10) and (11) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Ibc  ! beam radiation on collector plane W/m2
    REAL(r64), INTENT(IN) :: Idc  ! Diffuse radiation on collector plane W/m2
    REAL(r64), INTENT(IN) :: Ws   ! wind speed, m/s
    REAL(r64), INTENT(IN) :: Ta   ! ambient temperature, degC
    REAL(r64), INTENT(IN) :: fd   ! fraction of Idc used (empirical constant)
    REAL(r64), INTENT(IN) :: a    ! empirical constant
    REAL(r64), INTENT(IN) :: b    ! empirical constant
    REAL(r64), INTENT(IN) :: DT0  ! (Tc-Tm) at E=1000 W/m2 (empirical constant known as dTc), deg C

           ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
    REAL(r64) ::  E  ! irradiance working variable
    REAL(r64) ::  Tm

    E  = Ibc + fd * Idc

    Tm = E * Exp(a + b * Ws) + Ta

    SandiaCellTemperature = Tm + (E / 1000.0d0) * DT0 ! E0=1000.0 W/m2

  END FUNCTION SandiaCellTemperature
! -------------------------------------------------------------------------------

  REAL(r64) Function SandiaEffectiveIrradiance(Tc,Isc,Isc0,aIsc)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Jan 2004, F77 to f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns "effective irradiance", used in calculation of Imp, Voc, Ix, Ixx

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:

    REAL(r64), INTENT(IN) ::  Tc   ! cell temperature (deg C)
    REAL(r64), INTENT(IN) ::  Isc  ! short-circuit current under operating conditions (A)
    REAL(r64), INTENT(IN) ::  Isc0 ! reference Isc at Tc=25 C, Ic=1000 W/m2 (A)
    REAL(r64), INTENT(IN) ::  aIsc ! Isc temperature coefficient (degC^-1)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

        SandiaEffectiveIrradiance = Isc / (1.0d0+aIsc*(Tc - 25.0d0))/Isc0

  END FUNCTION SandiaEffectiveIrradiance

! -------------------------------------------------------------------------------
  REAL(r64) Function AbsoluteAirMass(SolZen, Altitude)
                ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Jan 2004 F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns absolute air mass

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals, ONLY: DegToRadians

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: SolZen   ! solar zenith angle (deg)
    REAL(r64), INTENT(IN) :: Altitude ! site altitude (m)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
    REAL(r64) :: AM ! air mass working variable

    IF (SolZen.LT.89.9d0) THEN
        AM = (Cos(SolZen * DegToRadians) + 0.5057d0 &
              * (96.08d0 - SolZen)**(-1.634d0))**(-1.0d0)

        AbsoluteAirMass = Exp(-0.0001184d0 * Altitude) * AM
    ELSE
        AbsoluteAirMass = 999.d0
        ! should maybe add a show warning msg.
    ENDIF

  END FUNCTION AbsoluteAirMass
! -------------------------------------------------------------------------------

  REAL(r64) Function SandiaF1(AMa,a0,a1,a2,a3,a4)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffit F77-> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns the result of Sandia Air Mass function
          !  "AMa-Function" for solar spectral influence

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation (8) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: AMa  ! absolute air mass
    REAL(r64), INTENT(IN) :: a0   ! empirical constant, module-specific
    REAL(r64), INTENT(IN) :: a1   ! empirical constant, module-specific
    REAL(r64), INTENT(IN) :: a2   ! empirical constant, module-specific
    REAL(r64), INTENT(IN) :: a3   ! empirical constant, module-specific
    REAL(r64), INTENT(IN) :: a4   ! empirical constant, module-specific

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

    REAL(r64) :: F1 !working variable for function result

    F1 = a0 + a1*AMa + a2*AMa**2 + a3*AMa**3 + a4*AMa**4

    IF (F1.GT.0.0d0) THEN
        SandiaF1 = F1
    ELSE
        SandiaF1 = 0.0d0
    ENDIF

  END FUNCTION SandiaF1
! -------------------------------------------------------------------------------
  REAL(r64) FUNCTION SandiaF2(IncAng,b0,b1,b2,b3,b4,b5)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Jan 2004 F77-> f90

          ! PURPOSE OF THIS FUNCTION:
          ! C Returns Sandia F2 function

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation (9) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: IncAng ! incidence angle (deg)
    REAL(r64), INTENT(IN) :: b0 ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: b1 ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: b2 ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: b3 ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: b4 ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: b5 ! empirical module-specific constants

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: F2 ! working variable for function result

    F2 = b0+b1*IncAng+b2*IncAng**2+b3*IncAng**3+b4*IncAng**4 &
             + b5*IncAng**5

    IF (F2.GT.0.0d0) THEN
          SandiaF2 = F2
    ELSE
          SandiaF2 = 0.0d0
    ENDIF

  END FUNCTION SandiaF2
! -------------------------------------------------------------------------------

  REAL(r64) Function SandiaImp(Tc,Ee,Imp0,aImp,C0,C1)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns current at maximum power point (A)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation (3) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tc   ! cell temperature (degC)
    REAL(r64), INTENT(IN) :: Ee   ! effective irradiance (W/m2)
    REAL(r64), INTENT(IN) :: Imp0 ! current at MPP at SRC (1000 W/m2, 25 C) (A)
    REAL(r64), INTENT(IN) :: aImp ! Imp temperature coefficient (degC^-1)
    REAL(r64), INTENT(IN) :: c0   ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: c1   ! empirical module-specific constants

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

     SandiaImp = Imp0*(C0*Ee+C1*Ee**2)*(1.0d0+aImp*(Tc-25))
     ! why hardwire T0 at 25.0?  can this change? seems okay, fewer args
   End Function
! -------------------------------------------------------------------------------

   REAL(r64) Function SandiaIsc(Tc,Isc0,Ibc,Idc,F1,F2,fd,aIsc)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Jan 2004 F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns Short-Circuit Current

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation (1) in Davis, M.W., A.H. Fanney, B.P. Dougherty. Measured versus
          !   predicted performance of building integrated photovoltaics,
          !   Solar 2002, Sunrise on the Reliable Energy Economy,
          !   June 15-19, 2002, Reno, NV.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tc   ! cell temperature (deg C)
    REAL(r64), INTENT(IN) :: Isc0 ! Isc at Tc=25 C, Ic=1000 W/m2 (A)
    REAL(r64), INTENT(IN) :: Ibc  ! beam radiation on collector plane (W/m2)
    REAL(r64), INTENT(IN) :: Idc  ! Diffuse radiation on collector plane (W/m2)
    REAL(r64), INTENT(IN) :: F1   ! Sandia F1 function for air mass effects
    REAL(r64), INTENT(IN) :: F2   ! Sandia F2 function of incidence angle
    REAL(r64), INTENT(IN) :: fd   ! module-specific empirical constant
    REAL(r64), INTENT(IN) :: aIsc ! Isc temperature coefficient (degC^-1)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    ! SandiaIsc=Isc0*((Ibc*F1*F2+fd*Idc)/1000.0)*(1.0+aIsc*(Tc-25.0))
    ! Barkers original (above) changed to match publish eq. (1) in reference
    SandiaIsc=Isc0*F1*((Ibc*F2+fd*Idc)/1000.0d0)*(1.0d0+aIsc*(Tc-25.0d0))

    ! why hardwire E0 at 1000.0 ?, can this change? seems okay

  End Function SandiaIsc
! -------------------------------------------------------------------------------

  REAL(r64) Function SandiaIx(Tc,Ee,Ix0,aIsc,aImp,C4,C5)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, Jan 2004 F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns current "Ix" at V=0.5*Voc (A)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation 9 in King et al. nov 20003

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tc   ! cell temperature (deg C)
    REAL(r64), INTENT(IN) :: Ee   ! effective irradiance
    REAL(r64), INTENT(IN) :: Ix0  ! Ix at SRC (1000 W/m2, 25 C) (A)
    REAL(r64), INTENT(IN) :: aIsc ! Isc temp coefficient (/C)
    REAL(r64), INTENT(IN) :: aImp ! Imp temp coefficient (/C)
    REAL(r64), INTENT(IN) :: c4   ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: c5   ! empirical module-specific constants

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

    SandiaIx=Ix0*(C4*Ee+C5*Ee**2) &
                 *(1.0d0+((aIsc+aImp)/2.0d0*(Tc-25.0d0)))

  END FUNCTION SandiaIx
! -------------------------------------------------------------------------------

  REAL(r64) FUNCTION SandiaIxx(Tc,Ee,Ixx0,aImp,C6,C7)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith Jan2004 F77 to f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns current "Ix" at V=0.5*(Voc+Vmp) (A)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation 10 in King et al nov. 2003

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tc   ! cell temperature (deg C)
    REAL(r64), INTENT(IN) :: Ee   ! effective irradiance (W/m2 ?)
    REAL(r64), INTENT(IN) :: Ixx0 ! Ixx at SRC (1000 W/m2, 25 C) (A)
    REAL(r64), INTENT(IN) :: aImp ! Imp temp coefficient (/C)
    REAL(r64), INTENT(IN) :: c6   ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: c7   ! empirical module-specific constants

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    SandiaIxx=Ixx0*(C6*Ee+C7*Ee**2)*(1.0d0+aImp*(Tc-25.0d0))

  END FUNCTION SandiaIxx
! -------------------------------------------------------------------------------
  REAL(r64) Function SandiaVmp(Tc,Ee,Vmp0,NcellSer,DiodeFactor, &
                         BVmp0,mBVmp,C2,C3)
              ! FUNCTION INFORMATION:
          !       AUTHOR         G. Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, Jan 2004, F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns Voltage at Max. Power Point (V)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! Equation 4 in King et al Nov. 2003

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tc ! cell temperature (deg C)
    REAL(r64), INTENT(IN) :: Ee ! effective irradiance
    REAL(r64), INTENT(IN) :: Vmp0 ! Vmp at SRC (1000 W/m2, 25 C) (V)
    REAL(r64), INTENT(IN) :: NcellSer ! # cells in series
    REAL(r64), INTENT(IN) :: DiodeFactor ! module-specIFic empirical constant
    REAL(r64), INTENT(IN) :: BVmp0  ! Vmp temperature coefficient (V/C)
    REAL(r64), INTENT(IN) :: mBVmp  ! change in BVmp with irradiance
    REAL(r64), INTENT(IN) :: c2    ! empirical module-specific constants
    REAL(r64), INTENT(IN) :: c3    ! empirical module-specific constants

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

    REAL(r64) :: dTc
    REAL(r64) :: BVmpEe

    IF (Ee.GT.0.0d0) THEN
      ! following is equation 8 in King et al. nov. 2003
      dTc = DiodeFactor*((1.38066d-23*(Tc+KelvinConv))/1.60218d-19)

      BVmpEe = BVmp0 + mBVmp * (1.0d0 - Ee)

      SandiaVmp = Vmp0+C2*NcellSer*dTc*Log(Ee)+ &
                    C3*NcellSer*(dTc*Log(Ee))**2+BVmpEe*(Tc-25.0d0)
    ELSE
      SandiaVmp = 0.0d0
    ENDIF

  END FUNCTION SandiaVmp
! -------------------------------------------------------------------------------

  REAL(r64) FUNCTION SandiaVoc(Tc,Ee,Voc0,NcellSer,DiodeFactor,  &
                        BVoc0,mBVoc)
          ! FUNCTION INFORMATION:
          !       AUTHOR         G Barker
          !       DATE WRITTEN   <unknown>
          !       MODIFIED       na
          !       RE-ENGINEERED  B Griffith Jan 2004 F77 -> f90

          ! PURPOSE OF THIS FUNCTION:
          ! Returns Open-Circuit Voltage (V)

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: Tc  ! cell temperature (deg C)
    REAL(r64), INTENT(IN) :: Ee  ! effective irradiance
    REAL(r64), INTENT(IN) :: Voc0  ! Voc at SRC (1000 W/m2, 25 C) (V)
    REAL(r64), INTENT(IN) :: NcellSer ! # cells in series
    REAL(r64), INTENT(IN) :: DiodeFactor ! module-specIFic empirical constant
    REAL(r64), INTENT(IN) :: BVoc0  ! Voc temperature coefficient (V/C)
    REAL(r64), INTENT(IN) :: mBVoc  ! change in BVoc with irradiance

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

     REAL(r64) :: dTc !working variable
     REAL(r64) :: BVocEe !working variable

     IF (Ee.GT.0.0d0) THEN
        dTc=DiodeFactor*((1.38066d-23*(Tc + KelvinConv))/1.60218d-19)
        BVocEe = BVoc0 + mBVoc * (1.0d0 - Ee)

        SandiaVoc=Voc0+NcellSer*dTc*Log(Ee)+BVocEe*(Tc-25.0d0)
     ELSE
        SandiaVoc = 0.0d0
     ENDIF

  END FUNCTION SandiaVoc


SUBROUTINE SetVentedModuleQdotSource(VentModNum, QSource)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Janauray 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Set" routine for updating sink term without exposing variables

          ! METHODOLOGY EMPLOYED:
          ! update derived type with new data , turn power into W/m2
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataSurfaces

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)  :: VentModNum
  REAL(r64)    , INTENT(IN)  :: QSource  ! source term in Watts


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ExtVentedCavity(VentModNum)%QdotSource = QSource / ExtVentedCavity(VentModNum)%ProjArea

  RETURN

END SUBROUTINE SetVentedModuleQdotSource

SUBROUTINE GetExtVentedCavityIndex(SurfacePtr, VentCavIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Get" routine for establishing correct integer index from outside this module

          ! METHODOLOGY EMPLOYED:
          ! mine Surface derived type for correct index/number of surface
          ! mine  ExtVentedCavity derived type that has the surface.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor , ONLY: FindItemInList
  USE DataSurfaces   , ONLY: Surface, TotSurfaces, ExtVentedCavity, TotExtVentCav

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER     , INTENT(IN)     :: SurfacePtr
  INTEGER     , INTENT(OUT)    :: VentCavIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: CavNum ! temporary
  INTEGER     :: thisSurf ! temporary
  INTEGER     :: thisCav
  Logical     :: Found

  IF (SurfacePtr == 0) THEN
     ! should be trapped already
     CALL ShowFatalError('Invalid surface passed to GetExtVentedCavityIndex' )
  ENDIF

  CavNum = 0
  Found = .false.
  Do thisCav=1, TotExtVentCav
     Do thisSurf =1, ExtVentedCavity(thisCav)%NumSurfs
        IF (SurfacePtr == ExtVentedCavity(thisCav)%SurfPtrs(thisSurf)) then
          Found = .TRUE.
          CavNum = thisCav
        ENDIF
     ENDDO
  ENDDO

  IF (.NOT. Found) THEN
    CALL ShowFatalError('Did not find surface in Exterior Vented Cavity description in GetExtVentedCavityIndex, '// &
         'Surface name = ' //TRIM(Surface(SurfacePtr)%Name))
  ELSE

    VentCavIndex = CavNum

  ENDIF

  RETURN

END SUBROUTINE GetExtVentedCavityIndex

SUBROUTINE GetExtVentedCavityTsColl(VentModNum, TsColl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! object oriented "Get" routine for collector surface temperature

          ! METHODOLOGY EMPLOYED:
          ! access derived type

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataSurfaces ,   only :ExtVentedCavity
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: VentModNum
  REAL(r64)   , INTENT(OUT) :: TsColl

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TsColl = ExtVentedCavity(VentModNum)%Tbaffle

  RETURN

  END SUBROUTINE GetExtVentedCavityTsColl
! -------------------------------------------------------------------------------

!     EnergyPlus V1.2 and beyond include models for photovoltaic calculations called
!     Generator:Photovoltaic:Simple and Generator:PV:Sandia implemented by the Center for
!     Buildings and Thermal Systems, National Renewable Energy Laboratory, 1617 Cole Blvd
!     MS 2722, Golden, CO, 80401
!
!     EnergyPlus v1.1.1 and beyond includes model for Photovoltaic calculations, now
!     referred to as the Generator:PV:Equivalent One-Diode model developed by Thermal Energy
!     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719;
!     Tel: (608) 274-2577
!
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

END MODULE Photovoltaics
