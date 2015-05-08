MODULE PlantLoadProfile

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       Brent Griffith, plant rewrite, general fluid types
          !                      allow flow requests with out load requests
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates a scheduled load profile on the demand side of the plant loop.

          ! METHODOLOGY EMPLOYED:
          ! The plant load profile object provides a scheduled load on the plant loop.  Unlike most plant equipment
          ! on the demand side, i.e. zone equipment, this object does not have a zone associated with it.
          ! For this reason the plant load profile can only be called for simulation by the non-zone equipment
          ! manager (see NonZoneEquipmentManager.f90).

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BeginEnvrnFlag, InitConvTemp
USE DataInterfaces, ONLY: ShowWarningError, ShowFatalError, ShowSevereError, ShowContinueError, SetupOutputVariable
USE DataPlant  ,    ONLY: PlantLoop, TypeOf_PlantLoadProfile, ScanPlantLoopsForObject
USE PlantUtilities, ONLY: SetComponentFlowRate, InitComponentNodes

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! DERIVED TYPE DEFINITIONS:
TYPE PlantProfileData
  CHARACTER(len=MaxNameLength) :: Name = ''                ! Name of Plant Load Profile object
  INTEGER                      :: TypeNum                  ! Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
  INTEGER                      :: WLoopNum     = 0         ! water plant loop index number                      !DSU
  INTEGER                      :: WLoopSideNum = 0         ! water plant loop side index                        !DSU
  INTEGER                      :: WLoopBranchNum   = 0     ! water plant loop branch index                      !DSU
  INTEGER                      :: WLoopCompNum     = 0     ! water plant loop component index                   !DSU
  LOGICAL                      :: Init = .TRUE.            ! Flag for initialization:  TRUE means do the init
  LOGICAL                      :: InitSizing = .TRUE.      ! Flag for initialization of plant sizing
  INTEGER                      :: InletNode = 0
  REAL(r64)                    :: InletTemp = 0.0d0          ! Inlet temperature (C)
  INTEGER                      :: OutletNode = 0
  REAL(r64)                    :: OutletTemp = 0.0d0         ! Outlet temperature (C)
  INTEGER                      :: LoadSchedule = 0         ! Pointer to schedule object
  LOGICAL                      :: EMSOverridePower  = .FALSE. ! if true, then EMS is calling to override power level
  REAL(r64)                    :: EMSPowerValue  = 0.0D0  ! value EMS is directing to use for power [W]
  REAL(r64)                    :: PeakVolFlowRate = 0.0d0    ! Peak volumetric flow rate, also water consumption rate (m3/s)
  INTEGER                      :: FlowRateFracSchedule = 0 ! Pointer to schedule object
  REAL(r64)                    :: VolFlowRate = 0.0d0        ! Volumetric flow rate (m3/s)
  REAL(r64)                    :: MassFlowRate = 0.0d0       ! Mass flow rate (kg/s)
  LOGICAL                      :: EMSOverrideMassFlow = .FALSE. !
  REAL(r64)                    :: EMSMassFlowValue    = 0.0D0 !

  ! Report variables
  REAL(r64)                    :: Power = 0.0d0              ! Power required to meet the load (W)
  REAL(r64)                    :: Energy = 0.0d0             ! Energy required to meet the load (J)
  REAL(r64)                    :: HeatingEnergy = 0.0d0      ! Heating Energy required to meet the load (J)
  REAL(r64)                    :: CoolingEnergy = 0.0d0      ! Cooling Energy required to meet the load (J)
  LOGICAL                      :: SetLoopIndexFlag = .TRUE.
END TYPE PlantProfileData

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE (PlantProfileData), ALLOCATABLE, DIMENSION(:) :: PlantProfile

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumOfPlantProfile

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC SimulatePlantProfile
PRIVATE GetPlantProfileInput
PRIVATE InitPlantProfile
PRIVATE UpdatePlantProfile
PRIVATE ReportPlantProfile

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE SimulatePlantProfile(EquipTypeName,EquipName,EquipTypeNum,ProfileNum, FirstHVACIteration,InitLoopEquip )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       Brent Griffith, generalize fluid cp
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates the plant load profile object.

          ! METHODOLOGY EMPLOYED:
          ! This is a very simple simulation.  InitPlantProfile does the work of getting the scheduled load and flow rate.
          ! Flow is requested and the actual available flow is set.  The outlet temperature is calculated.

          ! USE STATEMENTS:

  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataInterfaces, ONLY: ShowFatalError
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: EquipTypeName  ! description of model (not used until different types of profiles)
  CHARACTER(len=*), INTENT(IN) :: EquipName ! the user-defined name
  INTEGER, INTENT(IN)          :: EquipTypeNum ! the plant parameter ID for equipment model
  INTEGER, INTENT(INOUT)       :: ProfileNum ! the index for specific load profile
  LOGICAL, INTENT(IN)          :: FirstHVACIteration
  LOGICAL, INTENT(IN)          :: InitLoopEquip ! flag indicating if called in special initialization mode.


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DeltaTemp
  LOGICAL, SAVE :: GetInput = .TRUE.
  REAL(r64) :: Cp ! local fluid specific heat

          ! FLOW:
  IF (GetInput) THEN
    CALL GetPlantProfileInput
    GetInput = .FALSE.
  END IF

  IF (InitLoopEquip) THEN
    ProfileNum = FindItemInList(EquipName, PlantProfile%Name, NumOfPlantProfile)
    IF (ProfileNum /= 0) THEN
      CALL InitPlantProfile(ProfileNum)
      RETURN
    ENDIF

  ENDIF

  IF (ProfileNum /= 0) THEN

    CALL InitPlantProfile(ProfileNum)

    IF (PlantProfile(ProfileNum)%MassFlowRate > 0.d0) THEN

      Cp = GetSpecificHeatGlycol(PlantLoop(PlantProfile(ProfileNum)%WLoopNum)%FluidName,  &
                                 PlantProfile(ProfileNum)%InletTemp,                      &
                                 PlantLoop(PlantProfile(ProfileNum)%WLoopNum)%FluidIndex, &
                                 'SimulatePlantProfile')

      DeltaTemp = PlantProfile(ProfileNum)%Power &
                    / (PlantProfile(ProfileNum)%MassFlowRate * Cp)
    ELSE
      PlantProfile(ProfileNum)%Power = 0.d0
      DeltaTemp = 0.d0
    END IF

    PlantProfile(ProfileNum)%OutletTemp = PlantProfile(ProfileNum)%InletTemp - DeltaTemp

    CALL UpdatePlantProfile(ProfileNum)
    CALL ReportPlantProfile(ProfileNum)

  ELSE
    CALL ShowFatalError('SimulatePlantProfile: plant load profile not found ='//Trim(EquipName))

  ENDIF


  RETURN

END SUBROUTINE SimulatePlantProfile


SUBROUTINE GetPlantProfileInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the plant load profile input from the input file and sets up the objects.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataGlobals,           ONLY: AnyEnergyManagementSystemInModel
  USE DataInterfaces,        ONLY: SetupEMSActuator
  USE DataLoopNode
  USE DataIPShortCuts  ! Data for field names, blank numerics

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound = .FALSE. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus              ! Used in GetObjectItem
  LOGICAL                        :: IsBlank               ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk               ! TRUE if there was a problem with a list name
  INTEGER                        :: NumAlphas             ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers            ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: ProfileNum            ! PLANT LOAD PROFILE (PlantProfile) object number
!  CHARACTER(len=MaxNameLength)   :: FoundBranchName
!  INTEGER                        :: BranchControlType

          ! FLOW:
  cCurrentModuleObject = 'LoadProfile:Plant'
  NumOfPlantProfile = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumOfPlantProfile > 0) THEN
    ALLOCATE(PlantProfile(NumOfPlantProfile))

    DO ProfileNum = 1, NumOfPlantProfile
      CALL GetObjectItem(cCurrentModuleObject,ProfileNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,    &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      ! PlantProfile name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),PlantProfile%Name,ProfileNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject))
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      PlantProfile(ProfileNum)%Name = cAlphaArgs(1)
      PlantProfile(ProfileNum)%TypeNum = TypeOf_PlantLoadProfile    ! parameter assigned in DataPlant !DSU

      PlantProfile(ProfileNum)%InletNode = GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
      PlantProfile(ProfileNum)%OutletNode = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

      PlantProfile(ProfileNum)%LoadSchedule = GetScheduleIndex(cAlphaArgs(4))

      IF (PlantProfile(ProfileNum)%LoadSchedule == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
          '"  The Schedule for '//Trim(cAlphaFieldNames(4))//' called '//Trim(cAlphaArgs(4))//' was not found.')
        ErrorsFound = .TRUE.
      END IF

      PlantProfile(ProfileNum)%PeakVolFlowRate = rNumericArgs(1)

      PlantProfile(ProfileNum)%FlowRateFracSchedule = GetScheduleIndex(cAlphaArgs(5))

      IF (PlantProfile(ProfileNum)%FlowRateFracSchedule == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
          '"  The Schedule for '//Trim(cAlphaFieldNames(5))//' called '//Trim(cAlphaArgs(5))//' was not found.')

        ErrorsFound = .TRUE.
      END IF

      ! Check plant connections
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),TRIM(cCurrentModuleObject)//' Nodes')

      ! Setup report variables
      CALL SetupOutputVariable('Plant Load Profile Mass Flow Rate [kg/s]', PlantProfile(ProfileNum)%MassFlowRate, &
                               'System','Average',PlantProfile(ProfileNum)%Name)

      CALL SetupOutputVariable('Plant Load Profile Heat Transfer Rate [W]', PlantProfile(ProfileNum)%Power, &
                               'System','Average',PlantProfile(ProfileNum)%Name)

      CALL SetupOutputVariable('Plant Load Profile Heat Transfer Energy [J]', PlantProfile(ProfileNum)%Energy, &
                               'System','Sum',PlantProfile(ProfileNum)%Name, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='Heating',GroupKey='Plant')  ! is EndUseKey right?

      CALL SetupOutputVariable('Plant Load Profile Heating Energy [J]', PlantProfile(ProfileNum)%HeatingEnergy, &
                               'System','Sum',PlantProfile(ProfileNum)%Name, &
                                ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='Heating',GroupKey='Plant')

      CALL SetupOutputVariable('Plant Load Profile Cooling Energy [J]', PlantProfile(ProfileNum)%CoolingEnergy, &
                               'System','Sum',PlantProfile(ProfileNum)%Name, &
                                ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='Cooling',GroupKey='Plant')

      IF (AnyEnergyManagementSystemInModel) THEN
        CALL SetupEMSActuator('Plant Load Profile', PlantProfile(ProfileNum)%Name, 'Mass Flow Rate' , '[kg/s]', &
                 PlantProfile(ProfileNum)%EMSOverrideMassFlow, PlantProfile(ProfileNum)%EMSMassFlowValue )
        CALL SetupEMSActuator('Plant Load Profile', PlantProfile(ProfileNum)%Name, 'Power' , '[W]', &
                 PlantProfile(ProfileNum)%EMSOverridePower, PlantProfile(ProfileNum)%EMSPowerValue )
      ENDIF

      IF (ErrorsFound) CALL ShowFatalError('Errors in '//TRIM(cCurrentModuleObject)//' input.')

    END DO ! ProfileNum
  END IF

  RETURN

END SUBROUTINE GetPlantProfileInput


SUBROUTINE InitPlantProfile(ProfileNum )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the plant load profile object during the plant simulation.

          ! METHODOLOGY EMPLOYED:
          ! Inlet and outlet nodes are initialized.  The scheduled load and flow rate is obtained, flow is requested, and the
          ! actual available flow is set.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SysSizingCalc
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE DataLoopNode, ONLY: Node
  USE ScheduleManager, ONLY: GetCurrentScheduleValue, GetScheduleMaxValue
  USE FluidProperties, ONLY: GetDensityGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ProfileNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode
  REAL(r64) :: MaxFlowMultiplier
  REAL(R64) :: FluidDensityInit
  LOGICAL   :: errFlag

          ! FLOW:

  ! Do the one time initializations
  IF(PlantProfile(ProfileNum)%SetLoopIndexFlag)THEN
    IF(ALLOCATED(PlantLoop))THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(PlantProfile(ProfileNum)%Name, &
                                   PlantProfile(ProfileNum)%TypeNum, &
                                   PlantProfile(ProfileNum)%WLoopNum, &
                                   PlantProfile(ProfileNum)%WLoopSideNum, &
                                   PlantProfile(ProfileNum)%WLoopBranchNum, &
                                   PlantProfile(ProfileNum)%WLoopCompNum,   &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitPlantProfile: Program terminated for previous conditions.')
      ENDIF

      PlantProfile(ProfileNum)%SetLoopIndexFlag = .FALSE.
    ENDIF
  ENDIF

          ! FLOW:
  InletNode  = PlantProfile(ProfileNum)%InletNode
  OutletNode = PlantProfile(ProfileNum)%OutletNode

  IF (.NOT. SysSizingCalc .AND. PlantProfile(ProfileNum)%InitSizing) THEN
    CALL RegisterPlantCompDesignFlow(InletNode, PlantProfile(ProfileNum)%PeakVolFlowRate)
    PlantProfile(ProfileNum)%InitSizing = .FALSE.
  END IF

  IF (BeginEnvrnFlag .AND. PlantProfile(ProfileNum)%Init) THEN
    ! Clear node initial conditions
    !DSU? can we centralize these temperature inits
!    Node(InletNode)%Temp = 0.0
    Node(OutletNode)%Temp = 0.0d0

    FluidDensityInit = GetDensityGlycol(PlantLoop(PlantProfile(ProfileNum)%WLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(PlantProfile(ProfileNum)%WLoopNum)%FluidIndex,'InitPlantProfile')

    MaxFlowMultiplier = GetScheduleMaxValue(PlantProfile(ProfileNum)%FlowRateFracSchedule)

    CALL InitComponentNodes(0.d0, PlantProfile(ProfileNum)%PeakVolFlowRate*FluidDensityInit*MaxFlowMultiplier,  &
                            InletNode,OutletNode, &
                            PlantProfile(ProfileNum)%WLoopNum,PlantProfile(ProfileNum)%WLoopSideNum, &
                            PlantProfile(ProfileNum)%WLoopBranchNum, PlantProfile(ProfileNum)%WLoopCompNum)

    PlantProfile(ProfileNum)%EMSOverrideMassFlow = .FALSE.
    PlantProfile(ProfileNum)%EMSMassFlowValue    = 0.d0
    PlantProfile(ProfileNum)%EMSOverridePower    = .FALSE.
    PlantProfile(ProfileNum)%EMSPowerValue       = 0.d0
    PlantProfile(ProfileNum)%Init = .FALSE.

  END IF

  IF (.NOT. BeginEnvrnFlag) PlantProfile(ProfileNum)%Init = .TRUE.

  PlantProfile(ProfileNum)%InletTemp = Node(InletNode)%Temp
  PlantProfile(ProfileNum)%Power = GetCurrentScheduleValue(PlantProfile(ProfileNum)%LoadSchedule)

  IF (PlantProfile(ProfileNum)%EMSOverridePower) PlantProfile(ProfileNum)%Power = PlantProfile(ProfileNum)%EMSPowerValue

  FluidDensityInit = GetDensityGlycol(PlantLoop(PlantProfile(ProfileNum)%WLoopNum)%FluidName,  &
                                  PlantProfile(ProfileNum)%InletTemp, &
                                  PlantLoop(PlantProfile(ProfileNum)%WLoopNum)%FluidIndex,'InitPlantProfile')

  ! Get the scheduled mass flow rate
  PlantProfile(ProfileNum)%VolFlowRate = PlantProfile(ProfileNum)%PeakVolFlowRate *   &
                                         GetCurrentScheduleValue(PlantProfile(ProfileNum)%FlowRateFracSchedule)


  PlantProfile(ProfileNum)%MassFlowRate = PlantProfile(ProfileNum)%VolFlowRate * FluidDensityInit

  IF (PlantProfile(ProfileNum)%EMSOverrideMassFlow) &
           PlantProfile(ProfileNum)%MassFlowRate =  PlantProfile(ProfileNum)%EMSMassFlowValue

    ! Request the mass flow rate from the plant component flow utility routine
  CALL SetComponentFlowRate(PlantProfile(ProfileNum)%MassFlowRate,InletNode,OutletNode, &
            PlantProfile(ProfileNum)%WLoopNum,PlantProfile(ProfileNum)%WLoopSideNum, &
            PlantProfile(ProfileNum)%WLoopBranchNum, PlantProfile(ProfileNum)%WLoopCompNum)

  PlantProfile(ProfileNum)%VolFlowRate = PlantProfile(ProfileNum)%MassFlowRate / FluidDensityInit

  RETURN

END SUBROUTINE InitPlantProfile


SUBROUTINE UpdatePlantProfile(ProfileNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the node variables with local variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ProfileNum

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OutletNode

          ! FLOW:

  OutletNode = PlantProfile(ProfileNum)%OutletNode

  ! Set outlet node variables that are possibly changed
  Node(OutletNode)%Temp = PlantProfile(ProfileNum)%OutletTemp

  !DSU? enthalpy? quality etc? central routine? given inlet node, fluid type, delta T, properly fill all node vars?

  RETURN

END SUBROUTINE UpdatePlantProfile


SUBROUTINE ReportPlantProfile(ProfileNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates report variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SecInHour
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ProfileNum

          ! FLOW:
  PlantProfile(ProfileNum)%Energy = PlantProfile(ProfileNum)%Power * TimeStepSys * SecInHour

  IF (PlantProfile(ProfileNum)%Energy .GE. 0.0d0) THEN
    PlantProfile(ProfileNum)%HeatingEnergy = PlantProfile(ProfileNum)%Energy
    PlantProfile(ProfileNum)%CoolingEnergy = 0.0d0
  ELSE
    PlantProfile(ProfileNum)%HeatingEnergy = 0.0d0
    PlantProfile(ProfileNum)%CoolingEnergy = ABS(PlantProfile(ProfileNum)%Energy)
  ENDIF

  RETURN

END SUBROUTINE ReportPlantProfile

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

END MODULE PlantLoadProfile
