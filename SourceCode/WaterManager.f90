MODULE WaterManager

          ! Module containing the routines dealing with the management of water

          ! MODULE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       DJS to add ecoroof irrigation Jan 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BigNumber
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError, SetupOutputVariable

USE DataWater

          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE WaterManager:
PUBLIC  ManageWater
PUBLIC  ManageWaterInits
PRIVATE GetWaterManagerInput
PRIVATE UpdatePrecipitation
PRIVATE UpdateIrrigation
PRIVATE CalcWaterStorageTank
PUBLIC  SetupTankSupplyComponent  !call for water providing component models to obtain index
                                  ! pointers for water storage tanks and their supply arrays
PUBLIC  SetupTankDemandComponent  !call for water using component models to obtain index
                                  ! pointers for water storage tanks and their demand arrays
PRIVATE CalcRainCollector
PRIVATE CalcGroundwaterWell

PRIVATE UpdateWaterManager ! also does key initializations for envivronment and begin new timestep
PRIVATE ReportWaterManager

CONTAINS

SUBROUTINE ManageWater

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the top-level driver subroutine for managine water systems in the building
          ! Routine is called at the system timestep level from ManageHVAC
          !  (somewhat analogous to SimHVAC)

          ! METHODOLOGY EMPLOYED:
          ! State variables are continually recalculated each system iteration
          ! except when appropriate to update them.  IF this module is moved up
          ! to a different timestep (with less iteration), then numerical solution
          ! may need to be added.  Iteration is being used to solve interdependecies
          ! of storage, supply, and demand modeling of water system.
          !
          ! Most data are declared in data-only module DataWater.f90
          !
          !
          ! Calling order,
          !   storage tanks
          !   supply
          !   demands
          !  IF first/last timestep, then do an update.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:


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
  LOGICAL,SAVE      :: GetInputFlag = .true.  ! First time, input is "gotten"
  INTEGER           :: RainColNum = 0
  INTEGER           :: TankNum = 0
  INTEGER           :: WellNum = 0

  IF (GetInputFlag) THEN
    CALL GetWaterManagerInput
    GetInputFlag=.false.
  ENDIF

  If ( .NOT. (AnyWaterSystemsInModel) ) RETURN


  ! this is the main water manager
  ! first call all the water storage tanks
  !    (these called first to make control decisions)
  Do TankNum=1, NumWaterStorageTanks
     CALL  CalcWaterStorageTank(TankNum)
  ENDDO !tank loop

  Do RainColNum=1, NumRainCollectors
     CALL CalcRainCollector(RainColNum)
  ENDDO

  Do WellNum=1, NumGroundWaterWells
     CALL CalcGroundwaterWell(WellNum)
  ENDDO

  !call the tanks again to get updated rain and well activity
  Do TankNum=1, NumWaterStorageTanks
     CALL  CalcWaterStorageTank(TankNum)
  ENDDO !tank loop

  CALL ReportWaterManager

  RETURN

END SUBROUTINE ManageWater

SUBROUTINE ManageWaterInits

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,  only: BeginTimeStepFlag
  USE DataHVACGlobals, ONLY: FirstTimeStepSysFlag

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
  If ( .NOT. (AnyWaterSystemsInModel) ) RETURN

!  IF (BeginTimeStepFlag .OR. FirstTimeStepSysFlag) Then
  ! calls do updating that would be needed at end of final iteration
  ! and at the beginning of the timestep.

    call UpdateWaterManager

    Call UpdatePrecipitation
    Call UpdateIrrigation

!  ENDIF
  RETURN

END SUBROUTINE ManageWaterInits


SUBROUTINE GetWaterManagerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList ,&
                            SameString, GetObjectDefMaxArgs, VerifyName
  USE DataSurfaces,   ONLY: Surface , TotSurfaces
  USE DataHeatBalance, ONLY: Zone
  Use DataGlobals , ONLY:    NumOfZones
  Use DataInterfaces, ONLY:    ShowSevereError, SetupOutputVariable
  USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax, GetScheduleMinValue, GetScheduleMaxValue,  &
                             CheckScheduleValue
  USE General,      ONLY: RoundSigDigits

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
  INTEGER                        :: Item    ! Item to be "gotten"
  INTEGER                        :: NumAlphas  = 0 ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers = 0 ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus = 0  ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL, SAVE                  :: MyOneTimeFlag = .true.
  INTEGER                        :: MaxNumAlphas = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: MaxNumNumbers = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: TotalArgs = 0 !argument for call to GetObjectDefMaxArgs
  LOGICAL                        :: IsNotOK = .false.
  LOGICAL                        :: IsBlank = .false.
  INTEGER                        :: alphaOffset = 0 !
  INTEGER                        :: surfNum = 0
  CHARACTER(len=MaxNameLength)   :: objNameMsg = ' '
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  REAL(r64)                      :: tmpMax = 0.0d0
  REAL(r64)                      :: tmpMin = 0.0d0
  REAL(r64)                      :: tmpNumerator = 0.0d0
  REAL(r64)                      :: tmpArea = 0.0d0
  REAL(r64)                      :: tmpDenominator = 0.0d0
  INTEGER                        :: thisSurf = 0
  INTEGER                        :: NumIrrigation
  INTEGER                        :: Dummy


If( (MyOneTimeFlag).AND. (.NOT.( WaterSystemGetInputCalled)) ) THEN  !big block for entire subroutine

  cCurrentModuleObject  = 'WaterUse:Storage'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  MaxNumNumbers=NumNumbers
  MaxNumAlphas=NumAlphas
  cCurrentModuleObject = 'WaterUse:RainCollector'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNumbers)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'WaterUse:Well'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNumbers)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'Site:Precipitation'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNumbers)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  cCurrentModuleObject = 'RoofIrrigation'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNumbers)
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


  MyOneTimeFlag = .false.
  cCurrentModuleObject = 'WaterUse:Storage'
  NumWaterStorageTanks=GetNumObjectsFound(cCurrentModuleObject)
  IF (NumWaterStorageTanks > 0) THen
    AnyWaterSystemsInModel=.true.
    IF (.NOT.(Allocated(WaterStorage))) Allocate(WaterStorage(NumWaterStorageTanks))

    DO Item=1,NumWaterStorageTanks
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      AnyWaterSystemsInModel=.true.
      WaterStorage(Item)%Name = cAlphaArgs(1)
      Call VerifyName( cAlphaArgs(1), WaterStorage%Name, Item -1, IsNotOK,IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      objNameMsg = trim(TRIM(cCurrentModuleObject)//' = '//trim(cAlphaArgs(1)))

      WaterStorage(Item)%QualitySubCategoryName = cAlphaArgs(2)
!    If (SameString(cAlphaArgs(2), 'Mains')) Then
!      WaterStorage(Item)%QualitySubCategory = MainsWater
!    ELSEIF (SameString(cAlphaArgs(2), 'RAINWATER')) Then
!      WaterStorage(Item)%QualitySubCategory = RainWater
!
!    ELSEIF (SameString(cAlphaArgs(2), 'GREYWATER')) Then
!      WaterStorage(Item)%QualitySubCategory = GreyWater
!
!    ELSEIF (SameString(cAlphaArgs(2), 'WELLWATER')) Then
!      WaterStorage(Item)%QualitySubCategory =  WellWater
!
!    ELSEIF (SameString(cAlphaArgs(2), 'BLACKWATER')) Then
!      WaterStorage(Item)%QualitySubCategory = BlackWater

!    ELSE
!          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
!          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
!      ErrorsFound = .true.
!    ENDIF

      WaterStorage(Item)%MaxCapacity    = rNumericArgs(1)
      IF (WaterStorage(Item)%MaxCapacity  == 0.0d0 ) Then !default
        WaterStorage(Item)%MaxCapacity    = BigNumber
      endif

      WaterStorage(Item)%InitialVolume  = rNumericArgs(2)
      WaterStorage(Item)%MaxInFlowRate  = rNumericArgs(3)
      IF (WaterStorage(Item)%MaxInFlowRate  == 0.0d0 ) Then !default
        WaterStorage(Item)%MaxInFlowRate    = BigNumber
      endif

      WaterStorage(Item)%MaxOutFlowRate = rNumericArgs(4)
      IF (WaterStorage(Item)%MaxOutFlowRate   == 0.0d0 ) Then !default
        WaterStorage(Item)%MaxOutFlowRate    = BigNumber
      endif

      WaterStorage(Item)%OverflowTankName = cAlphaArgs(3) ! setup later

      If (SameString(cAlphaArgs(4), 'None')) Then
        WaterStorage(Item)%ControlSupplyType = NoControlLevel
      elseif (SameString(cAlphaArgs(4), 'Mains')) Then
        WaterStorage(Item)%ControlSupplyType = MainsFloatValve
      elseif (SameString(cAlphaArgs(4), 'GroundwaterWell')) Then
        WaterStorage(Item)%ControlSupplyType = WellFloatValve
      elseif (SameString(cAlphaArgs(4), 'OtherTank')) THEN
        WaterStorage(Item)%ControlSupplyType = OtherTankFloatValve
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound = .true.
      endif
      WaterStorage(Item)%ValveOnCapacity = rNumericArgs(5)
      WaterStorage(Item)%ValveOffCapacity = rNumericArgs(6)
      IF (WaterStorage(Item)%ControlSupplyType /= NoControlLevel) THEN
        IF (WaterStorage(Item)%ValveOffCapacity < WaterStorage(Item)%ValveOnCapacity) THEN 
          CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(5))//' and/or '// TRIM(cNumericFieldNames(6)) )
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError( TRIM(cNumericFieldNames(6)) //' must be greater than '//TRIM(cNumericFieldNames(5)) )
          CALL ShowContinueError('Check value for '//TRIM(cNumericFieldNames(5))//' = ' &
                               //TRIM(RoundSIgDigits(WaterStorage(Item)%ValveOnCapacity, 5)) )
          CALL ShowContinueError('which must be lower than '//TRIM(cNumericFieldNames(6))//' = ' &
                               //TRIM(RoundSIgDigits(WaterStorage(Item)%ValveOffCapacity, 5)) )
          ErrorsFound = .true.
        ENDIF
      ENDIF

      WaterStorage(Item)%BackupMainsCapacity = rNumericArgs(7)
      If (WaterStorage(Item)%BackupMainsCapacity > 0.0d0) Then !add backup to well and other thank supply
        If (WaterStorage(Item)%ControlSupplyType == WellFloatValve) Then
          WaterStorage(Item)%ControlSupplyType = WellFloatMainsBackup
        endif
        If (WaterStorage(Item)%ControlSupplyType == OtherTankFloatValve) Then
          WaterStorage(Item)%ControlSupplyType = TankMainsBackup
        endif
      endif

      WaterStorage(Item)%SupplyTankName = cAlphaArgs(5) !set up later

      If     (SameString(cAlphaArgs(6), 'ScheduledTemperature')) Then
        WaterStorage(item)%ThermalMode = ScheduledTankTemp
      ELSEIF (SameString(cAlphaArgs(6), 'ThermalModel')) Then
        WaterStorage(item)%ThermalMode = TankZoneThermalCoupled
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ENDIF

      If (WaterStorage(Item)%ThermalMode == ScheduledTankTemp) Then
          WaterStorage(item)%TempSchedID = GetScheduleIndex(cAlphaArgs(7))
        If (WaterStorage(item)%TempSchedID == 0) Then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          errorsfound = .true.
        ENDIF
        tmpMin = GetScheduleMinValue(WaterStorage(item)%TempSchedID)
        IF (tmpMin < 0.0d0) Then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          Call ShowContinueError('Found storage tank temperature schedule value less than 0.0 in '//trim(objNameMsg))
          errorsfound = .true.
        ENDIF
        tmpMax = GetScheduleMaxValue(WaterStorage(item)%TempSchedID)
        If (tmpMax > 100.0d0) Then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(7))//'='//TRIM(cAlphaArgs(7)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          Call ShowContinueError('found storage tank temperature schedule value greater than 100.0 in '//trim(objNameMsg))
            errorsfound = .true.
        ENDIF

      ENDIF

      If (WaterStorage(Item)%ThermalMode == TankZoneThermalCoupled) THEN
        If (SameString(cAlphaArgs(8), 'Schedule')) THEN
          WaterStorage(item)%AmbientTempIndicator  = AmbientTempSchedule
        ELSEIF (SameString(cAlphaArgs(8), 'Zone')) THEN
          WaterStorage(item)%AmbientTempIndicator  = AmbientTempZone
        ELSEIF (SameString(cAlphaArgs(8), 'Outdoors')) THEN
          WaterStorage(item)%AmbientTempIndicator  = AmbientTempExterior
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          errorsfound = .true.
        ENDIF
        WaterStorage(item)%AmbientTempSchedule = GetScheduleIndex(cAlphaArgs(9))
        If ((WaterStorage(item)%AmbientTempSchedule == 0) .AND.   &
            (WaterStorage(item)%AmbientTempIndicator == AmbientTempSchedule)) then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(9))//'='//TRIM(cAlphaArgs(9)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          errorsfound = .true.
        endif
        WaterStorage(Item)%ZoneID =  FindItemInList(cAlphaArgs(10), Zone%Name, NumOfZones)
        If ((WaterStorage(Item)%ZoneID == 0) .AND. (WaterStorage(item)%AmbientTempIndicator == AmbientTempZone)) Then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(10))//'='//TRIM(cAlphaArgs(10)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          ErrorsFound = .true.
        ENDIF
        WaterStorage(Item)%SurfArea = rNumericArgs(8)
        WaterStorage(Item)%UValue   =  rNumericArgs(9)
        WaterStorage(Item)%SurfMaterialName = cAlphaArgs(11)
        ! todo verify material collect and store useful data from it.
      ENDIF
    ENDDO
  ENDIF ! num water storage tanks > 0

  cCurrentModuleObject = 'WaterUse:RainCollector'
  NumRainCollectors = GetNumObjectsFound(cCurrentModuleObject)
  If (NumRainCollectors > 0) then
    IF (.NOT.(Allocated(RainCollector))) Allocate(RainCollector(NumRainCollectors))
    ! allow exensible reference to surfaces.
    AnyWaterSystemsInModel=.true.

    DO Item=1,NumRainCollectors
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      RainCollector(Item)%Name = cAlphaArgs(1)
      Call VerifyName( cAlphaArgs(1), RainCollector%Name, Item -1, IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Named ')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      ObjNameMsg = TRIM(cCurrentModuleObject)//' Named '//trim(cAlphaArgs(1))

      RainCollector(Item)%StorageTankName =  cAlphaArgs(2)
      RainCollector(Item)%StorageTankID = FindItemInList(cAlphaArgs(2), WaterStorage%Name, NumWaterStorageTanks)
      IF (RainCollector(Item)%StorageTankID == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        errorsfound = .true.
      ENDIF

      IF (SameString(cAlphaArgs(3), 'Constant')) THEN
        RainCollector(Item)%LossFactorMode = ConstantRainLossFactor
      ELSEIF (SameString(cAlphaArgs(3), 'Scheduled')) THEN
        RainCollector(Item)%LossFactorMode = ScheduledRainLossFactor
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        errorsfound = .true.
      ENDIF
      RainCollector(Item)%LossFactor = rNumericArgs(1)
      If (RainCollector(Item)%LossFactor > 1.0d0) then
        CALL ShowWarningError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        Call ShowContinueError('found rain water collection loss factor greater than 1.0, simulation continues')
      endif
      If (RainCollector(Item)%LossFactor < 0.0d0) then
        CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        Call ShowContinueError('found rain water collection loss factor less than 0.0')
        errorsfound = .true.
      endif

      If (RainCollector(Item)%LossFactorMode == ScheduledRainLossFactor) THEN
        RainCollector(Item)%LossFactorSchedID = GetScheduleIndex(cAlphaArgs(4))
        If (RainCollector(Item)%LossFactorSchedID == 0) Then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          errorsfound = .true.
        ENDIF
        IF (GetScheduleMinValue(RainCollector(Item)%LossFactorSchedID) < 0.0d0) Then
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          Call ShowContinueError('found rain water collection loss factor schedule value less than 0.0 in '//trim(objNameMsg))
          errorsfound = .true.
        ENDIF
        If (GetScheduleMaxValue(RainCollector(Item)%LossFactorSchedID) > 1.0d0) Then
          CALL showWarningError('Potentially invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          Call ShowContinueError('found rain water collection loss factor schedule value greater than 1.0, '// &
                                  'simulation continues' )
        ! allowing it to continue
        ENDIF
      ENDIF
      RainCollector(Item)%MaxCollectRate = rNumericArgs(1)
      If (RainCollector(Item)%MaxCollectRate == 0.0d0) RainCollector(Item)%MaxCollectRate = 100000000000.0d0

      !number of surfaces is extensible and = NumAlphas - alphaOffset
      alphaOffset = 4 !update this if more alphas inserted ahead of extensible surface listing
      RainCollector(Item)%NumCollectSurfs = NumAlphas - alphaOffset
      Allocate(RainCollector(Item)%SurfName(RainCollector(Item)%NumCollectSurfs))
      Allocate(RainCollector(Item)%SurfID(RainCollector(Item)%NumCollectSurfs))
      Do surfNum=1, RainCollector(Item)%NumCollectSurfs
        RainCollector(Item)%SurfName(surfNum) = cAlphaArgs(surfNum + alphaOffset)
        RainCollector(Item)%SurfID(surfNum) = FindItemInList(cAlphaArgs(surfNum + alphaOffset), Surface%Name, TotSurfaces)
        IF ( RainCollector(Item)%SurfID(surfNum) == 0) THEN
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(surfNum+alphaOffset))//'='//TRIM(cAlphaArgs(surfNum+alphaOffset)))
          CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          errorsfound = .true.
        ENDIF
      ENDDO

      ! now setup horizontal surface area
      tmpArea = 0.0d0
      tmpNumerator = 0.0d0
      tmpDenominator = 0.0d0
      Do surfNum=1, RainCollector(Item)%NumCollectSurfs
        thisSurf = RainCollector(Item)%SurfID(surfNum)
        tmpArea = tmpArea + Surface(thisSurf)%GrossArea * Surface(thisSurf)%CosTilt
        tmpNumerator = tmpNumerator + Surface(thisSurf)%Centroid%z * Surface(thisSurf)%GrossArea
        tmpDenominator = tmpDenominator + Surface(thisSurf)%GrossArea
      ENDDO
      RainCollector(Item)%HorizArea = tmpArea
      !now setup vertical hieght above ground for height dependent outdoor temps
      RainCollector(Item)%MeanHeight = tmpNumerator / tmpDenominator

      ! now set up tank supply connection
      Call InternalSetupTankSupplyComponent(RainCollector(Item)%Name, TRIM(cCurrentModuleObject),   &
                                      RainCollector(Item)%StorageTankName, &
                                      errorsFound, RainCollector(Item)%StorageTankID, RainCollector(Item)%StorageTankSupplyARRID)
    ENDDO
  ENDIF  ! (NumRainCollectors > 0)

  cCurrentModuleObject = 'WaterUse:Well'
  NumGroundWaterWells=GetNumObjectsFound(cCurrentModuleObject)
  If (NumGroundWaterWells > 0) Then
    AnyWaterSystemsInModel=.true.
    Allocate(GroundwaterWell(NumGroundWaterWells))
    DO Item=1,NumGroundWaterWells
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                    AlphaBlank=lAlphaFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      GroundwaterWell(Item)%Name = cAlphaArgs(1)
      Call VerifyName( cAlphaArgs(1), GroundwaterWell%Name, Item -1, IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      objNameMsg = TRIM(cCurrentModuleObject)//' Named '//TRIM(cAlphaArgs(1))
      GroundwaterWell(Item)%StorageTankName = cAlphaArgs(2)

      Call InternalSetupTankSupplyComponent(GroundwaterWell(Item)%Name, TRIM(cCurrentModuleObject),   &
                                GroundwaterWell(Item)%StorageTankName, &
                                errorsFound, GroundwaterWell(Item)%StorageTankID, GroundwaterWell(Item)%StorageTankSupplyARRID)

      If (allocated(WaterStorage)) WaterStorage(GroundwaterWell(Item)%StorageTankID)%GroundWellID = Item

      GroundwaterWell(Item)%PumpDepth          = rNumericArgs(1)
      GroundwaterWell(Item)%PumpNomVolFlowRate = rNumericArgs(2)
      GroundwaterWell(Item)%PumpNomHead        = rNumericArgs(3)
      GroundwaterWell(Item)%PumpNomPowerUse    = rNumericArgs(4)
      GroundwaterWell(Item)%PumpEfficiency     = rNumericArgs(5)
      GroundwaterWell(Item)%WellRecoveryRate   = rNumericArgs(6)
      GroundwaterWell(Item)%NomWellStorageVol  = rNumericArgs(7)
      If (SameString(cAlphaArgs(3), 'Constant')) THEN
        GroundwaterWell(Item)%GroundwaterTableMode = ConstantWaterTable
      ELSEIF (SameString(cAlphaArgs(3), 'Scheduled') ) THEN
        GroundwaterWell(Item)%GroundwaterTableMode = ScheduledWaterTable
      ELseIF (lAlphaFieldBlanks(3) ) then
        GroundwaterWell(Item)%GroundwaterTableMode = 0
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        errorsfound = .true.
      ENDIF

      !  N8, \field water table depth
      GroundwaterWell(Item)%WaterTableDepth = rNumericArgs(8)
      ! A4; \field water table depth schedule
      GroundwaterWell(Item)%WaterTableDepthSchedID =  GetScheduleIndex(cAlphaArgs(4))
      If ((GroundwaterWell(Item)%GroundwaterTableMode == ScheduledWaterTable) .AND. &
          ( GroundwaterWell(Item)%WaterTableDepthSchedID == 0) ) Then
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        errorsfound  = .true.
      endif

    ENDDO
  ENDIF !(NumGroundWaterWells > 0)

! do some water tank setup
  cCurrentModuleObject  = 'WaterUse:Storage'
  IF (NumWaterStorageTanks > 0) THen
    DO Item=1,NumWaterStorageTanks
      ! check that all storage tanks with ground well controls actually had wells pointing to them
      If(( WaterStorage(Item)%ControlSupplyType == WellFloatValve) &
         .or. (WaterStorage(Item)%ControlSupplyType == WellFloatMainsBackup)) THEN
         If (WaterStorage(Item)%GroundWellID == 0) Then
           Call ShowSevereError(TRIM(cCurrentModuleObject)//'= "'//trim(WaterStorage(Item)%Name) &
               //'" does not have a WaterUse:Well (groundwater well) that names it.')
            errorsFound = .true.
         ENDIF
      ENDIF

      ! setup tanks whose level is controlled by supply from another tank
      If(( WaterStorage(Item)%ControlSupplyType == OtherTankFloatValve) &
         .or. (WaterStorage(Item)%ControlSupplyType == TankMainsBackup)) THEN
        WaterStorage(Item)%SupplyTankID =   &
            FindItemInList(WaterStorage(Item)%SupplyTankName, WaterStorage%Name, NumWaterStorageTanks)
        If (WaterStorage(Item)%SupplyTankID == 0) Then
           Call ShowSevereError('Other tank called '//trim(WaterStorage(Item)%SupplyTankName) & ! TODO rename point
                                //' not found for '//TRIM(cCurrentModuleObject)//' Named '//trim(WaterStorage(Item)%Name) )
           errorsFound = .true.
        ENDIF
        CALL InternalSetupTankDemandComponent(WaterStorage(Item)%Name, TRIM(cCurrentModuleObject), &
                   WaterStorage(Item)%SupplyTankName, ErrorsFound, WaterStorage(Item)%SupplyTankID, &
                   WaterStorage(Item)%SupplyTankDemandARRID)
        !call to setup tank supply as well
        Call InternalSetupTankSupplyComponent(WaterStorage(Item)%SupplyTankName, TRIM(cCurrentModuleObject),   &
                                      WaterStorage(Item)%Name, &
                                      errorsFound, dummy, dummy)
      ENDIF
      ! setup overflow inputs
      WaterStorage(Item)%OverflowTankID = &
         FindItemInList(WaterStorage(Item)%OverflowTankName, WaterStorage%Name, NumWaterStorageTanks)
      If (WaterStorage(Item)%OverflowTankID == 0) Then
         ! if blank, then okay it is discarded.  but if not blank then error
         IF (WaterStorage(Item)%OverflowTankName == '     ') THEN
            WaterStorage(Item)%OverflowMode = OverflowDiscarded
         ELSE
           Call ShowSevereError('Overflow tank name of '//trim(WaterStorage(Item)%OverflowTankName)// &
                                ' not found for '//TRIM(cCurrentModuleObject)//' Named '//trim(WaterStorage(Item)%Name) )
           errorsfound = .true.
         ENDIF
      ELSE
      WaterStorage(Item)%OverflowMode = OverflowToTank
      ENDIf
      If (WaterStorage(Item)%OverflowMode == OverflowToTank) Then
        Call InternalSetupTankSupplyComponent(WaterStorage(Item)%Name, TRIM(cCurrentModuleObject),   &
                                      WaterStorage(Item)%OverflowTankName, &
                                      errorsFound, WaterStorage(Item)%OverflowTankID, WaterStorage(Item)%OverflowTankSupplyARRID)
      ENDIF


    ENDDO
  ENDIF

  cCurrentModuleObject = 'Site:Precipitation'
  NumSiteRainFall=GetNumObjectsFound(cCurrentModuleObject)
  IF (NumsiteRainFall > 1) THEN ! throw error
    Call ShowSevereError('Only one '//TRIM(cCurrentModuleObject)//' object is allowed')
    errorsfound = .true.
  ENDIF

  If (NumSiteRainFall == 1) then
    AnyWaterSystemsInModel=.true.
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)

    If (SameString(cAlphaArgs(1), 'ScheduleAndDesignLevel') ) then
      RainFall%ModeID = RainSchedDesign
    ELSE
      Call ShowSevereError('Precipitation Model Type of '//TRIM(cCurrentModuleObject)//' is incorrect.')
      Call ShowContinueError('Only available option is ScheduleAndDesignLevel.')
      errorsFound = .true.
    ENDIF
    RainFall%RainSchedID = GetScheduleIndex(cAlphaArgs(2))
    If ((RainFall%RainSchedID == 0) .AND. (RainFall%ModeID == RainSchedDesign)) then
      Call ShowSevereError('Schedule not found for '//TRIM(cCurrentModuleObject)//' object')
      errorsFound = .true.
    ElseIf ((RainFall%RainSchedID == 0) .AND. (RainFall%ModeID == RainSchedDesign)) then
      If (.not. CheckScheduleValueMinMax(RainFall%RainSchedID,'>=',0.0d0) ) then
        CALL ShowSevereError('Schedule='//trim(cAlphaArgs(2))//' for '//TRIM(cCurrentModuleObject)//' object has values < 0.')
        errorsFound = .true.
      ENDIF
    ENDIF

    RainFall%DesignAnnualRain = rNumericArgs(1)
    RainFall%NomAnnualRain    = rNumericArgs(2)

  ENDIF

  cCurrentModuleObject = 'RoofIrrigation'
  NumIrrigation = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumIrrigation > 1) THEN
    Call ShowSevereError('Only one '//TRIM(cCurrentModuleObject)//' object is allowed')
    errorsFound = .true.
  Endif

  IF (NumIrrigation == 1) THEN
    AnyIrrigationInModel = .true.
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
    IF (SameString(cAlphaArgs(1), 'Schedule') ) THEN
      Irrigation%ModeID = IrrSchedDesign
    ELSEIF ( SameString(cAlphaArgs(1), 'SmartSchedule')) THEN
      Irrigation%ModeID = IrrSmartSched
    ELSE
      CALL ShowSevereError('Type of '//TRIM(cCurrentModuleObject)//' is incorrect. Options are '// &
                           'Schedule or SmartSchedule')
      errorsFound = .true.
    ENDIF
    Irrigation%IrrSchedID = GetScheduleIndex(cAlphaArgs(2))
    IF ((Irrigation%IrrSchedID == 0) .AND. ((Irrigation%ModeID == IrrSchedDesign) .OR. Irrigation%ModeID == IrrSmartSched) )then
      CALL ShowSevereError ('Schedule not found for '//TRIM(cCurrentModuleObject)//' object')
      errorsFound = .true.
    ELSEIF ((Irrigation%IrrSchedID == 0) .AND. (Irrigation%ModeID == IrrSchedDesign)) THEN
      IF (.not. CheckScheduleValueMinMax(Irrigation%IrrSchedID,'>=',0.0d0) ) THEN
        CALL ShowSevereError('Schedule='//TRIM(cAlphaArgs(2))//' for '//TRIM(cCurrentModuleObject)//' object has values < 0.')
        errorsFound = .true.
      ENDIF
    ENDIF

    ! If we later add a designannualirrigation and a nominalannualirrigation variable (for scaling) those
    ! would be assigned here... as with the Rainfall...
    Irrigation%IrrigationThreshold=0.4d0
    IF (Irrigation%ModeID == IrrSmartSched .and. NumNumbers > 0) THEN
      IF (rNumericArgs(1) > 100.d0 .or. rNumericArgs(1) < 0.0d0) THEN
        CALL ShowSevereError('Irrigation threshold for '//TRIM(cCurrentModuleObject)//' object has values > 100 or < 0.')
        errorsFound = .true.
      ELSE
        Irrigation%IrrigationThreshold=rNumericArgs(1)/100.d0
      endif
    ENDIF

  ENDIF ! NumIrrigation ==1


  AnyWaterSystemsInModel=.true.
  WaterSystemGetInputCalled = .true.
  MyOneTimeFlag = .false.

  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lNumericFieldBlanks)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for water manager objects')
  ENDIF
 ! <SetupOutputVariables here...>, CurrentModuleObject='WaterUse:Storage'
  DO Item=1,NumWaterStorageTanks
        ! this next one is a measure of the state of water in the tank, not a flux of m3 that needs to be summed
    CALL SetupOutputVariable('Water System Storage Tank Volume [m3]', &
        WaterStorage(item)%ThisTimeStepVolume,'System','Average',WaterStorage(item)%Name)
    CALL SetupOutputVariable('Water System Storage Tank Net Volume Flow Rate [m3/s]', &
        WaterStorage(item)%NetVdot,'System','Average',WaterStorage(item)%Name)
    CALL SetupOutputVariable('Water System Storage Tank Inlet Volume Flow Rate [m3/s]', &
        WaterStorage(item)%VdotToTank,'System','Average',WaterStorage(item)%Name)
    CALL SetupOutputVariable('Water System Storage Tank Outlet Volume Flow Rate [m3/s]', &
        WaterStorage(item)%VdotFromTank,'System','Average',WaterStorage(item)%Name)
    CALL SetupOutputVariable('Water System Storage Tank Mains Water Volume [m3]', &
        WaterStorage(item)%MainsDrawVol,'System','Sum',WaterStorage(item)%Name, &
          ResourceTypeKey='MainsWater', &
          EndUseKey='WaterSystem', &
          EndUseSubKey=WaterStorage(item)%QualitySubCategoryName, &
          GroupKey='System')
    CALL SetupOutputVariable('Water System Storage Tank Mains Water Volume Flow Rate [m3/s]', &
        WaterStorage(item)%MainsDrawVdot,'System','Average',WaterStorage(item)%Name)
    CALL SetupOutputVariable('Water System Storage Tank Water Temperature [C]', &
        WaterStorage(item)%Twater,'System','Average',WaterStorage(item)%Name)
    CALL SetupOutputVariable('Water System Storage Tank Overflow Volume Flow Rate [m3/s]', &
        WaterStorage(item)%VdotOverflow,'System','Average',WaterStorage(item)%Name)
    If (WaterStorage(item)%OverflowMode  == OverflowDiscarded) Then
      CALL SetupOutputVariable('Water System Storage Tank Overflow Water Volume [m3]', &
        WaterStorage(item)%VolOverflow,'System','Sum',WaterStorage(item)%Name)
     !     ResourceTypeKey='Water',  &
     !     EndUseKey='WaterSystems', &
     !     EndUseSubkey=WaterStorage(item)%QualitySubCategoryName ,&
     !     GroupKey='System')
    ELSE
      CALL SetupOutputVariable('Water System Storage Tank Overflow Water Volume [m3]', &
        WaterStorage(item)%VolOverflow,'System','Sum',WaterStorage(item)%Name)

    ENDIF
    CALL SetupOutputVariable('Water System Storage Tank Overflow Temperature [C]', &
        WaterStorage(item)%TwaterOverflow,'System','Average',WaterStorage(item)%Name)

  ENDDO

  If (NumSiteRainFall ==1) Then  ! CurrentModuleObject='Site:Precipitation'
    CALL SetupOutputVariable('Site Precipitation Rate [m/s]', &
         RainFall%CurrentRate,'System','Average','Site:Precipitation')
    CALL SetupOutputVariable('Site Precipitation Depth [m]', &
         RainFall%CurrentAmount,'System','Sum','Site:Precipitation')
  endif

  If (NumIrrigation ==1) Then   ! CurrentModuleObject='RoofIrrigation'
    CALL SetupOutputVariable('Water System Roof Irrigation Scheduled Depth [m]', &
         Irrigation%ScheduledAmount,'System','Sum','RoofIrrigation')
    CALL SetupOutputVariable('Water System Roof Irrigation Actual Depth [m]', &
         Irrigation%ActualAmount,'System','Sum','RoofIrrigation')
  endif

  DO Item =1, NumRainCollectors  ! CurrentModuleObject='WaterUse:RainCollector'
    CALL SetupOutputVariable('Water System Rainwater Collector Volume Flow Rate [m3/s]', &
       RainCollector(item)%VdotAvail,'System','Average',RainCollector(item)%Name)
    CALL SetupOutputVariable('Water System Rainwater Collector Volume [m3]', &
       RainCollector(item)%VolCollected, 'System', 'Sum', RainCollector(item)%Name , &
       ResourceTypeKey='OnSiteWater', EndUseKey='Rainwater', GroupKey='System')

  ENDDO


  Do Item =1, NumGroundWaterWells  ! CurrentModuleObject='WaterUse:Well'
    CALL SetupOutputVariable('Water System Groundwater Well Requested Volume Flow Rate [m3/s]', &
       GroundwaterWell(item)%VdotRequest,'System','Average',GroundwaterWell(item)%Name)
    CALL SetupOutputVariable('Water System Groundwater Well Volume Flow Rate [m3/s]', &
       GroundwaterWell(item)%VdotDelivered,'System','Average',GroundwaterWell(item)%Name)
    CALL SetupOutputVariable('Water System Groundwater Well Volume [m3]', &
       GroundwaterWell(item)%VolDelivered,'System','Sum',GroundwaterWell(item)%Name, &
       ResourceTypeKey='OnSiteWater', EndUseKey='Wellwater', GroupKey='System')
    CALL SetupOutputVariable('Water System Groundwater Well Pump Electric Power [W]', &
       GroundwaterWell(item)%PumpPower,'System','Average',GroundwaterWell(item)%Name)
    CALL SetupOutputVariable('Water System Groundwater Well Pump Electric Energy [J]', &
       GroundwaterWell(item)%PumpEnergy,'System','Sum',GroundwaterWell(item)%Name, &
       ResourceTypeKey='Electricity', EndUseKey='WaterSystems', GroupKey='System')

  ENDDO

ENDIF ! my one time flag block

RETURN

END SUBROUTINE GetWaterManagerInput

SUBROUTINE UpdatePrecipitation

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !update the current rate of precipitation
          !

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataGlobals    , ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys

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
  REAL(r64)  :: schedRate
  REAL(r64)  :: ScaleFactor

  If (RainFall%ModeID == RainSchedDesign) then
    schedRate = GetCurrentScheduleValue(RainFall%RainSchedID) ! m/hr
    ScaleFactor = RainFall%DesignAnnualRain / RainFall%NomAnnualRain
    RainFall%CurrentRate = schedRate * ScaleFactor / SecInHour  !convert to m/s
    RainFall%CurrentAmount =  RainFall%CurrentRate* (TimeStepSys * SecInHour)
  ENDIF

  RETURN

END SUBROUTINE UpdatePrecipitation

SUBROUTINE UpdateIrrigation

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D. Sailor
          !       DATE WRITTEN   Dec 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !update the current rate of irrigation
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE ScheduleManager, ONLY: GetCurrentScheduleValue
USE DataGlobals    , ONLY: SecInHour
USE DataHVACGlobals, ONLY: TimeStepSys

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
REAL(r64)  :: schedRate
!REAL(r64)  :: ScaleFactor

Irrigation%ScheduledAmount =  0.0d0

If (Irrigation%ModeID == IrrSchedDesign) then
schedRate = GetCurrentScheduleValue(Irrigation%IrrSchedID) ! m/hr
Irrigation%ScheduledAmount =  schedRate* (TimeStepSys * SecInHour)/SecInHour ! convert to m/timestep

ElseIf (Irrigation%ModeID == IrrSmartSched) then
schedRate = GetCurrentScheduleValue(Irrigation%IrrSchedID) ! m/hr
Irrigation%ScheduledAmount =  schedRate* (TimeStepSys * SecInHour)/SecInHour ! convert to m/timestep
ENDIF

RETURN

END SUBROUTINE UpdateIrrigation

SUBROUTINE SizeWaterManager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

  RETURN

END SUBROUTINE SizeWaterManager


SUBROUTINE CalcWaterStorageTank(TankNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Collect the calculations used to update the modeled values
          ! for the storage tanks at each system timestep
          !

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: SecInHour, BeginTimeStepFlag
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, Intent(IN)  :: TankNum ! Index of storage tank

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! see DataWater.f90


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: OrigVdotDemandRequest = 0.0d0
  REAL(r64)    :: TotVdotDemandAvail = 0.0d0
  REAL(r64)    :: OrigVolDemandRequest = 0.0d0
  REAL(r64)    :: TotVolDemandAvail = 0.0d0
  REAL(r64)    :: OrigVdotSupplyAvail = 0.0d0
  REAL(r64)    :: TotVdotSupplyAvail = 0.0d0
  REAL(r64)    :: TotVolSupplyAvail = 0.0d0
!  REAL(r64)    :: TotVolSupplyAllow = 0.0d0
  REAL(r64)    :: overflowVdot = 0.0d0
  REAL(r64)    :: overflowVol = 0.0d0
  REAL(r64)    :: overflowTwater = 0.0d0
  REAL(r64)    :: NetVdotAdd = 0.0d0
  REAL(r64)    :: NetVolAdd = 0.0d0
  REAL(r64)    :: FillVolRequest = 0.0d0
  REAL(r64)    :: TotVolAllowed = 0.0d0
  REAL(r64)    :: AvailVolume = 0.0d0
  REAL(r64)    :: underflowVdot = 0.0d0
  REAL(r64)    :: VolumePredict = 0.0d0
  REAL(r64)    :: OverFillVolume = 0.0d0

  If (BeginTimeStepFlag) then
    ! initializations are done in UpdateWaterManager
  endif

  overflowVdot = 0.0d0
  IF (WaterStorage(TankNum)%NumWaterSupplies > 0) THEN
    OrigVdotSupplyAvail = Sum(WaterStorage(TankNum)%VdotAvailSupply)
  ELSE
    OrigVdotSupplyAvail = 0.0d0
  ENDIF
  TotVdotSupplyAvail = OrigVdotSupplyAvail ! Init
  If (TotVdotSupplyAvail > WaterStorage(TankNum)%MaxInFlowRate) THen
    ! pipe/filter rate constraints on inlet
    overflowVdot = TotVdotSupplyAvail - WaterStorage(TankNum)%MaxInFlowRate
    overflowTwater = Sum(WaterStorage(TankNum)%VdotAvailSupply * WaterStorage(TankNum)%TwaterSupply) &
                     / Sum(WaterStorage(TankNum)%VdotAvailSupply)
    TotVdotSupplyAvail = WaterStorage(TankNum)%MaxInFlowRate
  endif
  TotVolSupplyAvail = TotVdotSupplyAvail *  TimeStepSys * SecInHour
  overflowVol = overflowVdot *  TimeStepSys * SecInHour

  underflowVdot = 0.0d0
  IF (WaterStorage(TankNum)%NumWaterDemands > 0) THEN
    OrigVdotDemandRequest = Sum(WaterStorage(TankNum)%VdotRequestDemand)
  ELSE
    OrigVdotDemandRequest = 0.0d0
  ENDIF
  OrigVolDemandRequest  = OrigVdotDemandRequest *  TimeStepSys * SecInHour
  TotVdotDemandAvail =  OrigVdotDemandRequest ! initialize to satisfied then modify if needed
  If (TotVdotDemandAvail > WaterStorage(TankNum)%MaxOutFlowRate) THEN
    ! pipe/filter rate constraints on outlet
    underflowVdot = OrigVdotDemandRequest - WaterStorage(TankNum)%MaxOutFlowRate
    TotVdotDemandAvail = WaterStorage(TankNum)%MaxOutFlowRate
  ENDIF
  TotVolDemandAvail  = TotVdotDemandAvail* (TimeStepSys * SecInHour)

  NetVdotAdd = TotVdotSupplyAvail - TotVdotDemandAvail
  NetVolAdd  = NetVdotAdd * (TimeStepSys * SecInHour)

  VolumePredict = WaterStorage(TankNum)%LastTimeStepVolume + NetVolAdd

  ! would tank capacity be exceeded?
  TotVolAllowed = WaterStorage(TankNum)%MaxCapacity - WaterStorage(TankNum)%LastTimeStepVolume
  If (VolumePredict > WaterStorage(TankNum)%MaxCapacity) THEN ! too much
     ! added overflow to inlet rate limit, new temperature model
     OverFillVolume =  (VolumePredict - WaterStorage(TankNum)%MaxCapacity)
     overflowTwater =  (overflowTwater * overflowVol + OverFillVolume * WaterStorage(TankNum)%Twater) &
                        / (overflowVol + OverFillVolume)
     overflowVol = overflowVol + OverFillVolume
     NetVolAdd   = NetVolAdd - OverFillVolume
     NetVdotAdd  = NetVolAdd / (TimeStepSys * SecInHour)
     VolumePredict = WaterStorage(TankNum)%MaxCapacity
  endif

  !Is tank too low to meet the request?
  IF (VolumePredict < 0.0d0) THEN
    AvailVolume = WaterStorage(TankNum)%LastTimeStepVolume + TotVolSupplyAvail
    AvailVolume = MAX(0.d0, AvailVolume)
    TotVolDemandAvail = AvailVolume
    TotVdotDemandAvail = AvailVolume / (TimeStepSys * SecInHour)
    underflowVdot = OrigVdotDemandRequest - TotVdotDemandAvail
    NetVdotAdd = TotVdotSupplyAvail - TotVdotDemandAvail
    NetVolAdd  = NetVdotAdd * (TimeStepSys * SecInHour)
    VolumePredict = 0.0d0
  ENDIF

  If (TotVdotDemandAvail < OrigVdotDemandRequest) Then ! starvation
  ! even distribution
    IF (OrigVdotDemandRequest > 0.d0) THEN
      WaterStorage(TankNum)%VdotAvailDemand = (TotVdotDemandAvail/OrigVdotDemandRequest) &
                                           * WaterStorage(TankNum)%VdotRequestDemand
    ELSE
      WaterStorage(TankNum)%VdotAvailDemand = 0.d0
    ENDIF
  ELSE ! requested demand can be served
    IF (WaterStorage(TankNum)%NumWaterDemands > 0) THEN
      WaterStorage(TankNum)%VdotAvailDemand = WaterStorage(TankNum)%VdotRequestDemand
    ENDIF
  ENDIF


  ! is tank lower than float valve on capacity and requesting fill from controlled supplier?
  FillVolRequest = 0.0d0
  If ((VolumePredict)< WaterStorage(TankNum)%ValveOnCapacity) THen !turn on supply to fill tank
    FillVolRequest = WaterStorage(TankNum)%ValveOffCapacity - VolumePredict

    ! set mains draws for float on (all the way to Float off)
    IF (WaterStorage(TankNum)%ControlSupplyType == MainsFloatValve) then

      WaterStorage(TankNum)%MainsDrawVdot = FillVolRequest   / (TimeStepSys * SecInHour)
      NetVolAdd = FillVolRequest

    endif
    ! set demand request in supplying tank if needed
    IF ((WaterStorage(TankNum)%ControlSupplyType == OtherTankFloatValve) &
          .OR. (WaterStorage(TankNum)%ControlSupplyType == TankMainsBackup)) THEN
      WaterStorage(WaterStorage(TankNum)%SupplyTankID)%VdotRequestDemand(WaterStorage(TankNum)%SupplyTankDemandARRID) &
            = FillVolRequest  / (TimeStepSys * SecInHour)
      !

    ENDIF

    ! set demand request in groundwater well if needed
    IF ((WaterStorage(TankNum)%ControlSupplyType == WellFloatValve) &
          .OR. (WaterStorage(TankNum)%ControlSupplyType == WellFloatMainsBackup)) THEN
      GroundwaterWell(WaterStorage(TankNum)%GroundWellID)%VdotRequest = FillVolRequest  / (TimeStepSys * SecInHour)
    ENDIF


  ENDIF


  ! set mains flow if mains backup active
  If ((VolumePredict)< WaterStorage(TankNum)%BackupMainsCapacity) THen !turn on supply
    IF ((WaterStorage(TankNum)%ControlSupplyType == WellFloatMainsBackup) &
        .OR. (WaterStorage(TankNum)%ControlSupplyType == TankMainsBackup)) THEN
      FillVolRequest = WaterStorage(TankNum)%ValveOffCapacity - VolumePredict
      WaterStorage(TankNum)%MainsDrawVdot = FillVolRequest   / (TimeStepSys * SecInHour)
      NetVolAdd = FillVolRequest

    ENDIF
  ENDIF

  WaterStorage(TankNum)%ThisTimeStepVolume =  WaterStorage(TankNum)%LastTimeStepVolume + NetVolAdd
  WaterStorage(TankNum)%VdotOverflow  = overflowVol / (TimeStepSys * SecInHour)
  WaterStorage(TankNum)%VolOverflow   = overflowVol
  WaterStorage(TankNum)%TwaterOverflow = overflowTwater
  WaterStorage(TankNum)%NetVdot = NetVolAdd  / (TimeStepSys * SecInHour)
  WaterStorage(TankNum)%MainsDrawVol =  WaterStorage(TankNum)%MainsDrawVdot * (TimeStepSys * SecInHour)
  WaterStorage(TankNum)%VdotToTank   = TotVdotSupplyAvail
  WaterStorage(TankNum)%VdotFromTank =  TotVdotDemandAvail

  Select Case (WaterStorage(TankNum)%ThermalMode)
  Case(ScheduledTankTemp)
    WaterStorage(TankNum)%Twater = GetCurrentScheduleValue(WaterStorage(TankNum)%TempSchedID)
    WaterStorage(TankNum)%TouterSkin =  WaterStorage(TankNum)%Twater
  Case(TankZoneThermalCoupled)
    CAll ShowFatalError('WaterUse:Storage (Water Storage Tank) zone thermal model incomplete')
  End Select

  !set supply avail data from overflows in Receiving tank
  IF (WaterStorage(TankNum)%OverflowMode == OverflowToTank) THEN
    WaterStorage(WaterStorage(TankNum)%OverflowTankID)%VdotAvailSupply(WaterStorage(TankNum)%OverflowTankSupplyARRID) &
          = WaterStorage(TankNum)%VdotOverflow
    WaterStorage(WaterStorage(TankNum)%OverflowTankID)%TwaterSupply(WaterStorage(TankNum)%OverflowTankSupplyARRID) &
          = WaterStorage(TankNum)%TwaterOverflow
  ENDIF


  RETURN

END SUBROUTINE CalcWaterStorageTank

SUBROUTINE SetupTankSupplyComponent(CompName, CompType, TankName, ErrorsFound, TankIndex, WaterSupplyIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Each simulated component that can supply water to a tank
          ! makes one call to this subroutine to obtain the data
          ! array index it should use to set values in the
          ! VdotAvailSupply

          ! METHODOLOGY EMPLOYED:
          ! push the VdotAvailToTank array and return

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(Len=*), INTENT(IN) :: CompName
  CHARACTER(Len=*), INTENT(IN) :: CompType
  CHARACTER(Len=*), INTENT(IN) :: TankName
  LOGICAL ,INTENT(INOUT)                   :: ErrorsFound
  INTEGER , INTENT(OUT)                    :: TankIndex
  INTEGER , INTENT(OUT)                    :: WaterSupplyIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  If (.NOT. (WaterSystemGetInputCalled)) Then
    CALL GetWaterManagerInput
  endif

  CALL InternalSetupTankSupplyComponent(CompName,CompType,TankName,ErrorsFound,TankIndex,WaterSupplyIndex)

  RETURN
END SUBROUTINE SetupTankSupplyComponent

SUBROUTINE InternalSetupTankSupplyComponent(CompName, CompType, TankName, ErrorsFound, TankIndex, WaterSupplyIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Each simulated component that can supply water to a tank
          ! makes one call to this subroutine to obtain the data
          ! array index it should use to set values in the
          ! VdotAvailSupply

          ! METHODOLOGY EMPLOYED:
          ! push the VdotAvailToTank array and return

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor , only: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(Len=*), INTENT(IN) :: CompName
  CHARACTER(Len=*), INTENT(IN) :: CompType
  CHARACTER(Len=*), INTENT(IN) :: TankName
  LOGICAL ,INTENT(INOUT)                   :: ErrorsFound
  INTEGER , INTENT(OUT)                    :: TankIndex
  INTEGER , INTENT(OUT)                    :: WaterSupplyIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: oldNumSupply
  CHARACTER(Len=MaxNameLength), Allocatable :: oldSupplyCompNames(:)
  CHARACTER(Len=MaxNameLength), Allocatable :: oldSupplyCompTypes(:)
!  LOGICAL , SAVE    :: MyOneTimeFlag = .true.

  TankIndex = FindItemInList(TankName, WaterStorage%Name, NumWaterStorageTanks)
  If (TankIndex == 0) Then
    Call ShowSevereError('WaterUse:Storage (Water Storage Tank) ="'//trim(TankName)//'" not found in '  &
           //trim(CompType)//' called '//trim(CompName) )
    errorsFound = .true.
    RETURN ! So we don't pass TankIndex=0
  ENDIF
  oldNumSupply = WaterStorage(TankIndex)%NumWaterSupplies
  If (oldNumSupply > 0) Then ! do array push
    IF( ALLOCATED(oldSupplyCompNames)) DEALLOCATE(oldSupplyCompNames)
    Allocate(oldSupplyCompNames(oldNumSupply))
    IF( ALLOCATED(oldSupplyCompTypes)) DEALLOCATE(oldSupplyCompTypes)
    ALLOCATE(oldSupplyCompTypes(oldNumSupply))
    If (ALLOCATED(WaterStorage(TankIndex)%SupplyCompNames)) THEN
      oldSupplyCompNames = WaterStorage(TankIndex)%SupplyCompNames
      DEALLOCATE(WaterStorage(TankIndex)%SupplyCompNames)
      ALLOCATE(WaterStorage(TankIndex)%SupplyCompNames(oldNumSupply + 1))
      WaterStorage(TankIndex)%SupplyCompNames(1:oldNumSupply) = oldSupplyCompNames !array assignment
      WaterStorage(TankIndex)%SupplyCompNames(oldNumSupply + 1) = CompName
    ENDIF
    If (ALLOCATED(WaterStorage(TankIndex)%SupplyCompTypes)) THEN
      oldSupplyCompTypes = WaterStorage(TankIndex)%SupplyCompTypes
      DEALLOCATE(WaterStorage(TankIndex)%SupplyCompTypes)
      ALLOCATE(WaterStorage(TankIndex)%SupplyCompTypes(oldNumSupply + 1))
      WaterStorage(TankIndex)%SupplyCompTypes(1:oldNumSupply) = oldSupplyCompTypes !array assignment
      WaterStorage(TankIndex)%SupplyCompTypes(oldNumSupply + 1) = CompType
    ENDIF
    DEALLOCATE(WaterStorage(TankIndex)%VdotAvailSupply)
    ALLOCATE(WaterStorage(TankIndex)%VdotAvailSupply(oldNumSupply + 1))
    WaterStorage(TankIndex)%VdotAvailSupply = 0.0d0 !initialize
    DEALLOCATE(WaterStorage(TankIndex)%TwaterSupply)
    ALLOCATE(WaterStorage(TankIndex)%TwaterSupply(oldNumSupply + 1))
    WaterStorage(TankIndex)%TwaterSupply = 0.0d0 !initialize
    WaterSupplyIndex = oldNumSupply + 1
    WaterStorage(TankIndex)%NumWaterSupplies = WaterStorage(TankIndex)%NumWaterSupplies + 1
  ELSE ! first time (no push)

    ALLOCATE(WaterStorage(TankIndex)%VdotAvailSupply(1))
    WaterStorage(TankIndex)%VdotAvailSupply = 0.0d0 !initialize
    ALLOCATE(WaterStorage(TankIndex)%TwaterSupply(1))
    WaterStorage(TankIndex)%TwaterSupply = 0.0d0 !initialize
    ALLOCATE(WaterStorage(TankIndex)%SupplyCompNames(1))
    WaterStorage(TankIndex)%SupplyCompNames(1) = CompName
    ALLOCATE(WaterStorage(TankIndex)%SupplyCompTypes(1))
    WaterStorage(TankIndex)%SupplyCompTypes(1) = CompType
    WaterSupplyIndex = 1
    WaterStorage(TankIndex)%NumWaterSupplies = 1
  ENDIF

  RETURN

END SUBROUTINE InternalSetupTankSupplyComponent

SUBROUTINE SetupTankDemandComponent(CompName, CompType, TankName, ErrorsFound, TankIndex, WaterDemandIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Each simulated component that can supply water to a tank
          ! makes one call to this subroutine to obtain the data
          ! array index it should use to set values in the
          ! VdotAvailSupply

          ! METHODOLOGY EMPLOYED:
          ! push the VdotAvailToTank array and return

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(Len=*), INTENT(IN) :: CompName
  CHARACTER(Len=*), INTENT(IN) :: CompType
  CHARACTER(Len=*), INTENT(IN) :: TankName
  LOGICAL ,INTENT(INOUT)                   :: ErrorsFound
  INTEGER , INTENT(OUT)                    :: TankIndex
  INTEGER , INTENT(OUT)                    :: WaterDemandIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  If (.NOT. (WaterSystemGetInputCalled)) Then
    CALL GetWaterManagerInput
  endif

  CALL InternalSetupTankDemandComponent(CompName,CompType,TankName,ErrorsFound,TankIndex,WaterDemandIndex)

  RETURN
END SUBROUTINE SetupTankDemandComponent

SUBROUTINE InternalSetupTankDemandComponent(CompName, CompType, TankName, ErrorsFound, TankIndex, WaterDemandIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Each simulated component that can supply water to a tank
          ! makes one call to this subroutine to obtain the data
          ! array index it should use to set values in the
          ! VdotAvailSupply

          ! METHODOLOGY EMPLOYED:
          ! push the VdotAvailToTank array and return

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE InputProcessor, only: FindItemInList
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(Len=*), INTENT(IN) :: CompName
  CHARACTER(Len=*), INTENT(IN) :: CompType
  CHARACTER(Len=*), INTENT(IN) :: TankName
  LOGICAL ,INTENT(INOUT)                   :: ErrorsFound
  INTEGER , INTENT(OUT)                    :: TankIndex
  INTEGER , INTENT(OUT)                    :: WaterDemandIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: oldNumDemand
  CHARACTER(Len=MaxNameLength),  Allocatable :: oldDemandCompNames(:)
  CHARACTER(Len=MaxNameLength),  Allocatable :: oldDemandCompTypes(:)
!  LOGICAL , SAVE    :: MyOneTimeFlag = .true.

  TankIndex = FindItemInList(TankName, WaterStorage%Name, NumWaterStorageTanks)
  If (TankIndex == 0) Then
    Call ShowSevereError('WaterUse:Storage (Water Storage Tank) ="'//trim(TankName)//'" not found in '  &
           //trim(CompType)//' called '//trim(CompName) )
    errorsFound = .true.
    return
  ENDIF
  oldNumDemand = WaterStorage(TankIndex)%NumWaterDemands
  If (oldNumDemand > 0) Then ! do array push
    IF( ALLOCATED(oldDemandCompNames)) DEALLOCATE(oldDemandCompNames)
    Allocate(oldDemandCompNames(oldNumDemand))
    IF( ALLOCATED(oldDemandCompTypes)) DEALLOCATE(oldDemandCompTypes)
    ALLOCATE(oldDemandCompTypes(oldNumDemand))
    If (ALLOCATED(WaterStorage(TankIndex)%DemandCompNames)) THEN
      oldDemandCompNames = WaterStorage(TankIndex)%DemandCompNames
      DEALLOCATE(WaterStorage(TankIndex)%DemandCompNames)
      ALLOCATE(WaterStorage(TankIndex)%DemandCompNames(oldNumDemand + 1))
      WaterStorage(TankIndex)%DemandCompNames(1:oldNumDemand) = oldDemandCompNames !array assignment
      WaterStorage(TankIndex)%DemandCompNames(oldNumDemand + 1) = CompName
    ENDIF
    If (ALLOCATED(WaterStorage(TankIndex)%DemandCompTypes)) THEN
      oldDemandCompTypes = WaterStorage(TankIndex)%DemandCompTypes
      DEALLOCATE(WaterStorage(TankIndex)%DemandCompTypes)
      ALLOCATE(WaterStorage(TankIndex)%DemandCompTypes(oldNumDemand + 1))
      WaterStorage(TankIndex)%DemandCompTypes(1:oldNumDemand) = oldDemandCompTypes !array assignment
      WaterStorage(TankIndex)%DemandCompTypes(oldNumDemand + 1) = CompType
    ENDIF

    DEALLOCATE(WaterStorage(TankIndex)%VdotRequestDemand)
    ALLOCATE(WaterStorage(TankIndex)%VdotRequestDemand(oldNumDemand + 1))
    WaterStorage(TankIndex)%VdotRequestDemand = 0.0d0 !initialize

    DEALLOCATE(WaterStorage(TankIndex)%VdotAvailDemand)
    ALLOCATE(WaterStorage(TankIndex)%VdotAvailDemand(oldNumDemand + 1))
    WaterStorage(TankIndex)%VdotAvailDemand = 0.0d0 !initialize

    WaterDemandIndex = oldNumDemand + 1
    WaterStorage(TankIndex)%NumWaterDemands = WaterStorage(TankIndex)%NumWaterDemands + 1
  ELSE ! first time (no push)

    ALLOCATE(WaterStorage(TankIndex)%VdotRequestDemand(1))
    WaterStorage(TankIndex)%VdotRequestDemand = 0.0d0 !initialize
    ALLOCATE(WaterStorage(TankIndex)%VdotAvailDemand(1))
    WaterStorage(TankIndex)%VdotAvailDemand = 0.0d0 !initialize
    ALLOCATE(WaterStorage(TankIndex)%DemandCompNames(1))
    WaterStorage(TankIndex)%DemandCompNames(1) = CompName
    ALLOCATE(WaterStorage(TankIndex)%DemandCompTypes(1))
    WaterStorage(TankIndex)%DemandCompTypes(1) = CompType
    WaterStorage(TankIndex)%NumWaterDemands = 1
    WaterDemandIndex = 1
  ENDIF

  RETURN

END SUBROUTINE InternalSetupTankDemandComponent


SUBROUTINE CalcRainCollector(RainColNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Collect the calculations used to update the modeled values
          ! for the rain collector at each system timestep
          !

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataGlobals,     ONLY: SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE DataEnvironment,     ONLY: OutWetBulbTempAt
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, Intent(IN)  :: RainColNum ! Index of rain collector

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! see DataWater.f90


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: LossFactor
  REAL(r64)    :: VdotAvail


 !If (.NOT.(IsRain)) Then ! is it raining now? No don't use this flag since precip schedule might differ from weather file
  IF (RainFall%CurrentRate <= 0.0d0) Then
  ! set available supply rate in WaterStorage
    WaterStorage(RainCollector(RainColNum)%StorageTankID)%VdotAvailSupply(RainCollector(RainColNum)%StorageTankSupplyARRID) = 0.0d0
    ! temperature of water supply is modeled as the same as outdoor drybulb.
    WaterStorage(RainCollector(RainColNum)%StorageTankID)%TwaterSupply(RainCollector(RainColNum)%StorageTankSupplyARRID)    = 0.0d0

    RainCollector(RainColNum)%VdotAvail = 0.0d0
    RainCollector(RainColNum)%VolCollected =  0.0d0
  ELSE

    SELECT CASE (RainCollector(RainColNum)%LossFactorMode)

    CASE(ConstantRainLossFactor)
      LossFactor = RainCollector(RainColNum)%LossFactor
    CASE(ScheduledRainLossFactor)
      LossFactor = GetCurrentScheduleValue(RainCollector(RainColNum)%LossFactorSchedID)
    END SELECT

    VdotAvail =  RainFall%CurrentRate * RainCollector(RainColNum)%HorizArea *(1.0d0 - LossFactor )

    If (VdotAvail > RainCollector(RainColNum)%MaxCollectRate) Then
      VdotAvail =  RainCollector(RainColNum)%MaxCollectRate
    endif

    ! set available supply rate in WaterStorage
    WaterStorage(RainCollector(RainColNum)%StorageTankID)%VdotAvailSupply(RainCollector(RainColNum)%StorageTankSupplyARRID) = &
      VdotAVail

    ! temperature of water supply is modeled as the same as outdoor drybulb.
    WaterStorage(RainCollector(RainColNum)%StorageTankID)%TwaterSupply(RainCollector(RainColNum)%StorageTankSupplyARRID) &
       = OutWetBulbTempAt(RainCollector(RainColNum)%MeanHeight)

    RainCollector(RainColNum)%VdotAvail    = VdotAvail
    RainCollector(RainColNum)%VolCollected =  VdotAvail * TimeStepSys * SecInHour

  ENDIF !

  RETURN

END SUBROUTINE CalcRainCollector

SUBROUTINE CalcGroundwaterWell(WellNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Collect the calculations used to update the modeled values
          ! for the groundwater wells at each system timestep
          !

          ! METHODOLOGY EMPLOYED:
          ! starting simple and ignoring well storage and complex rate restrictions.
          ! just uses nominal pump rate and power (assuming well designed well).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, only: BeginTimeStepFlag, SecInHour
  USE DataHVACGlobals, ONLY: TimeStepSys
  USE DataEnvironment , only:GroundTemp_Deep
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, Intent(IN)  :: WellNum ! Index of well

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! see DataWater.f90


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: VdotDelivered
!  REAL(r64) :: VdotRequest
  REAL(r64) :: PumpPower


  If (BeginTimeStepFlag) then
    ! do any updating needed
   ! GroundwaterWell(WellNum)%VdotRequest = 0.0

  endif

  VdotDelivered = 0.0d0
  PumpPower = 0.0d0
  If (GroundwaterWell(WellNum)%VdotRequest > 0.0d0) Then

    IF (GroundwaterWell(WellNum)%VdotRequest >= GroundwaterWell(WellNum)%PumpNomVolFlowRate) Then ! run flat out
      WaterStorage(GroundwaterWell(WellNum)%StorageTankID)%VdotAvailSupply(GroundwaterWell(WellNum)%StorageTankSupplyARRID) &
           =  GroundwaterWell(WellNum)%PumpNomVolFlowRate
      WaterStorage(GroundwaterWell(WellNum)%StorageTankID)%TwaterSupply(GroundwaterWell(WellNum)%StorageTankSupplyARRID) &
           = GroundTemp_Deep
      VdotDelivered = GroundwaterWell(WellNum)%PumpNomVolFlowRate
      PumpPower =  GroundwaterWell(WellNum)%PumpNomPowerUse
    endif

    ! the run at part load to just meet request
    If (GroundwaterWell(WellNum)%VdotRequest < GroundwaterWell(WellNum)%PumpNomVolFlowRate) Then
      WaterStorage(GroundwaterWell(WellNum)%StorageTankID)%VdotAvailSupply(GroundwaterWell(WellNum)%StorageTankSupplyARRID) &
           =  GroundwaterWell(WellNum)%VdotRequest
      WaterStorage(GroundwaterWell(WellNum)%StorageTankID)%TwaterSupply(GroundwaterWell(WellNum)%StorageTankSupplyARRID) &
           = GroundTemp_Deep

      VdotDelivered = GroundwaterWell(WellNum)%VdotRequest
      PumpPower =  GroundwaterWell(WellNum)%PumpNomPowerUse *   &
                      GroundwaterWell(WellNum)%VdotRequest / GroundwaterWell(WellNum)%PumpNomVolFlowRate

    ENDIF
  ENDIF

  GroundwaterWell(WellNum)%VdotDelivered  = VdotDelivered
  GroundwaterWell(WellNum)%VolDelivered   = VdotDelivered * TimeStepSys * SecInHour
  GroundwaterWell(WellNum)%PumpPower      = PumpPower
  GroundwaterWell(WellNum)%PumpEnergy     = PumpPower * TimeStepSys * SecInHour


  RETURN

END SUBROUTINE CalcGroundwaterWell


SUBROUTINE UpdateWaterManager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The water manger is iterating and
          ! we need to do the timestep record keeping
          ! for tracking state variables.
          !  this routine updates variables
          ! that hold the value of the Last Timestep

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY:BeginEnvrnFlag, InitConvTemp, WarmUpFlag, KickOffSimulation, DoingSizing

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
  INTEGER  :: TankNum
  INTEGER  :: RainColNum
  INTEGER  :: WellNum
  LOGICAL,SAVE        :: MyEnvrnFlag = .TRUE. ! flag for init once at start of environment
  LOGICAL,SAVE        :: MyWarmupFlag = .False. ! flag for init after warmup complete
  LOGICAL,SAVE        :: MyTankDemandCheckFlag = .TRUE.

  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN !
    Do TankNum=1, NumWaterStorageTanks

     WaterStorage(TankNum)%LastTimeStepVolume = WaterStorage(TankNum)%InitialVolume
     WaterStorage(TankNum)%ThisTimeStepVolume = WaterStorage(TankNum)%InitialVolume
    ENDDO
    IF ((.NOT. DoingSizing) .and. (.NOT. KickOffSimulation) .AND. MyTankDemandCheckFlag) THEN
      IF (NumWaterStorageTanks > 0) THEN
        DO TankNum=1,NumWaterStorageTanks
          IF (WaterStorage(TankNum)%NumWaterDemands == 0) THEN
            CALL ShowWarningError('Found WaterUse:Tank that has nothing connected to draw water from it.')
            CALL ShowContinueError('Occurs for WaterUse:Tank = '//TRIM(WaterStorage(TankNum)%Name) )
            Call ShowContinueError('Check that input for water consuming components specifies a water supply tank.')
          ENDIF
        ENDDO
      ENDIF
      MyTankDemandCheckFlag = .FALSE.
    ENDIF

    MyEnvrnFlag = .FALSE.
    MyWarmupFlag = .TRUE.
  ENDIF ! end environmental inits
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF

  IF (MyWarmupFlag .AND. (.NOT. WarmUpFlag) ) THEN ! do environment inits.  just went out of warmup mode
    Do TankNum=1, NumWaterStorageTanks
     WaterStorage(TankNum)%LastTimeStepVolume = WaterStorage(TankNum)%InitialVolume
     WaterStorage(TankNum)%ThisTimeStepVolume = WaterStorage(TankNum)%InitialVolume
     WaterStorage(TankNum)%LastTimeStepTemp   = WaterStorage(TankNum)%InitialTankTemp
    ENDDO
    MyWarmupFlag = .FALSE.
  ENDIF

  Do TankNum=1, NumWaterStorageTanks
     ! main location for inits for new timestep.
     WaterStorage(TankNum)%LastTimeStepVolume = MAX(WaterStorage(TankNum)%ThisTimeStepVolume, 0.d0)
     WaterStorage(TankNum)%MainsDrawVdot = 0.0d0
     WaterStorage(TankNum)%MainsDrawVol  = 0.0d0
     WaterStorage(TankNum)%NetVdot       = 0.0d0
     WaterStorage(TankNum)%VdotFromTank  = 0.0d0
     WaterStorage(TankNum)%VdotToTank    = 0.0d0
     IF (WaterStorage(TankNum)%NumWaterDemands > 0) THEN
       WaterStorage(TankNum)%VdotRequestDemand = 0.0d0
       WaterStorage(TankNum)%VdotAvailDemand   = 0.0d0
     ENDIF
     WaterStorage(TankNum)%VdotOverflow      = 0.0d0
     IF (WaterStorage(TankNum)%NumWaterSupplies > 0) THEN
       WaterStorage(TankNum)%VdotAvailSupply   = 0.0d0
     ENDIF
     IF ((WaterStorage(TankNum)%ControlSupplyType == WellFloatValve) &
          .OR. (WaterStorage(TankNum)%ControlSupplyType == WellFloatMainsBackup)) THEN
       If (allocated(GroundWaterWell))  GroundwaterWell(WaterStorage(TankNum)%GroundWellID)%VdotRequest = 0.0d0
     ENDIF
  ENDDO !tank loop

  Do RainColNum=1, NumRainCollectors

    RainCollector(RainColNum)%VdotAvail    = 0.0d0
    RainCollector(RainColNum)%VolCollected = 0.0d0
  ENDDO

  Do WellNum=1, NumGroundWaterWells
    ! re init calculated vars
    GroundwaterWell(WellNum)%VdotRequest   = 0.0d0
    GroundwaterWell(WellNum)%VdotDelivered = 0.0d0
    GroundwaterWell(WellNum)%VolDelivered  = 0.0d0
    GroundwaterWell(WellNum)%PumpPower     = 0.0d0
    GroundwaterWell(WellNum)%PumpEnergy    = 0.0d0
  ENDDO


  RETURN

END SUBROUTINE UpdateWaterManager

SUBROUTINE ReportWaterManager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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

 ! <this routine is typically needed only for those cases where you must transform the internal data to a reportable form>

  RETURN

END SUBROUTINE ReportWaterManager

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

END MODULE WaterManager

