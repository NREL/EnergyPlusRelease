! Module ElectricBaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:RadiantConvective:Electric)
MODULE ElectricBaseboardRadiator
  ! Module containing the routines dealing with the electric baseboard heater

  ! MODULE INFORMATION:
  !       AUTHOR         Daeho Kang
  !       DATE WRITTEN   Feb 2010
  !       MODIFIED
  !       RE-ENGINEERED

  ! PURPOSE OF THIS MODULE:
  ! This module is to calculate the actual convective heat addition that an electrical baseboard heater
  ! deliveres to a space.

  ! METHODOLOGY EMPLOYED:
  ! Based on the convective-only electric baseboard module (Object: ZoneHVAC:Baseboard:Convective:Electric)
  ! written by Richard Liesen in Nov 2001, this new electric baseboard module is to add the existing calculation
  ! algorithm of radiant heat transfer in the high temperature radiant system module.

  ! REFERENCES:
  ! HighTempRadiantSystem module (ZoneHVAC:HighTemperatureRadiant)
  ! Convective electric baseboard module (ZoneHVAC:Baseboard:Convective:Electric)

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataGlobals
USE DataInterfaces
USE DataPrecisionGlobals

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: BaseboardRadiator_Electric = 1
CHARACTER(len=*), PARAMETER :: cCMO_BBRadiator_Electric = 'ZoneHVAC:Baseboard:RadiantConvective:Electric'

  ! DERIVED TYPE DEFINITIONS
TYPE ElecBaseboardParams
    CHARACTER(len=MaxNameLength) :: EquipName =' '
    INTEGER :: EquipType = 0
    CHARACTER(len=MaxNameLength) :: Schedule  =' '
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfaceName
    INTEGER, ALLOCATABLE, DIMENSION(:) :: SurfacePtr
    INTEGER :: ZonePtr                 = 0
    INTEGER :: SchedPtr                = 0
    INTEGER :: TotSurfToDistrib        = 0
    REAL(r64) :: NominalCapacity       =0.0d0
    REAL(r64) :: BaseBoardEfficiency   =0.0d0
    REAL(r64) :: AirInletTemp          =0.0d0
    REAL(r64) :: AirInletHumRat        =0.0d0
    REAL(r64) :: AirOutletTemp         =0.0d0
    REAL(r64) :: ElecUseLoad           =0.0d0
    REAL(r64) :: ElecUseRate           =0.0d0
    REAL(r64) :: FracRadiant           =0.0d0
    REAL(r64) :: FracConvect           =0.0d0
    REAL(r64) :: FracDistribPerson     =0.0d0
    REAL(r64) :: TotPower              =0.0d0
    REAL(r64) :: Power                 =0.0d0
    REAL(r64) :: ConvPower             =0.0d0
    REAL(r64) :: RadPower              =0.0d0
    REAL(r64) :: TotEnergy             =0.0d0
    REAL(r64) :: Energy                =0.0d0
    REAL(r64) :: ConvEnergy            =0.0d0
    REAL(r64) :: RadEnergy             =0.0d0
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: FracDistribToSurf
END TYPE ElecBaseboardParams

  !MODULE VARIABLE DECLARATIONS:
  TYPE (ElecBaseboardParams), ALLOCATABLE, DIMENSION(:) :: ElecBaseboard
  INTEGER :: NumElecBaseboards=0
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: QBBElecRadSource ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: QBBElecRadSrcAvg ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZeroSourceSumHATsurf ! Equal to the SumHATsurf for all the walls in a zone with no source
  ! Record keeping variables used to calculate QBBRadSrcAvg locally
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastQBBElecRadSrc    ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastSysTimeElapsed   ! Need to keep the last value in case we are still iterating
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: LastTimeStepSys      ! Need to keep the last value in case we are still iterating
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: CheckEquipName
  !SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

  PUBLIC  :: SimElecBaseBoard
  PUBLIC  :: CalcElectricBaseboard
  PRIVATE :: GetElectricBaseboardInput
  PRIVATE :: InitElectricBaseboard
  PRIVATE :: SizeElectricBaseboard
  PRIVATE :: ReportElectricBaseboard
  PRIVATE :: UpdateElectricBaseboard
  PUBLIC  :: UpdateBBElecRadSourceValAvg
  PRIVATE :: DistributeBBElecRadGains
  PRIVATE :: SumHATsurf

CONTAINS

  SUBROUTINE SimElecBaseBoard (EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, PowerMet, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       Feb 2010 Daeho Kang for radiant component
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the Electric Baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Water baseboard module

          ! USE STATEMENTS:
    USE InputProcessor,        ONLY: FindItemInList
    USE General,               ONLY: TrimSigDigits
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: EquipName
    INTEGER, INTENT(IN)    :: ActualZoneNum
    INTEGER, INTENT(IN)    :: ControlledZoneNum
    REAL(r64), INTENT(OUT) :: PowerMet
    INTEGER, INTENT(INOUT) :: CompIndex
    LOGICAL, INTENT(IN)    :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER       :: BaseboardNum  ! Index of unit in baseboard array
    LOGICAL,SAVE  :: GetInputFlag = .TRUE. ! One time get input flag

    IF (GetInputFlag) THEN
      CALL GetElectricBaseboardInput
      GetInputFlag=.false.
    END IF

    ! Find the correct Baseboard Equipment
    IF (CompIndex == 0) THEN
      BaseboardNum = FindItemInList(EquipName, ElecBaseboard%EquipName, NumElecBaseboards)
      IF (BaseboardNum == 0) THEN
        CALL ShowFatalError('SimElectricBaseboard: Unit not found='//TRIM(EquipName))
      ENDIF
      CompIndex=BaseboardNum
    ELSE
      BaseboardNum=CompIndex
      IF (BaseboardNum > NumElecBaseboards .or. BaseboardNum < 1) THEN
        CALL ShowFatalError('SimElectricBaseboard:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumElecBaseboards))//  &
                            ', Entered Unit name='//TRIM(EquipName))
      ENDIF
      IF (CheckEquipName(BaseboardNum)) THEN
        IF (EquipName /= ElecBaseboard(BaseboardNum)%EquipName) THEN
          CALL ShowFatalError('SimElectricBaseboard: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(BaseboardNum))// &
                              ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                              TRIM(ElecBaseboard(BaseboardNum)%EquipName))
        ENDIF
      CheckEquipName(BaseboardNum)=.false.
      ENDIF
    ENDIF

    CALL InitElectricBaseboard(BaseboardNum, ControlledZoneNum, FirstHVACIteration)


    SELECT CASE (ElecBaseboard(BaseboardNum)%EquipType)

     CASE (BaseboardRadiator_Electric)  ! 'ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC'
           ! Simulate baseboard
           CALL CalcElectricBaseboard(BaseboardNum, ControlledZoneNum)

      CASE DEFAULT
        CALL ShowSevereError('SimElecBaseBoard: Errors in Baseboard='//TRIM(ElecBaseboard(BaseboardNum)%EquipName))
        CALL ShowContinueError('Invalid or unimplemented equipment type='//  &
           cCMO_BBRadiator_Electric)
        CALL ShowFatalError('Preceding condition causes termination.')

    END SELECT

    PowerMet = ElecBaseboard(BaseboardNum)%TotPower

    CALL UpdateElectricBaseboard(BaseboardNum)
    CALL ReportElectricBaseboard(BaseboardNum)

  END SUBROUTINE SimElecBaseBoard

  SUBROUTINE GetElectricBaseboardInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       Feb 2010 Daeho Kang for radiant component
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the Baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! Standard input processor calls.

          ! REFERENCES:
          ! Hot water baseboard module

          ! USE STATEMENTS:
    USE InputProcessor,    ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, MakeUPPERCase
    USE DataSurfaces,      ONLY: Surface, TotSurfaces
    USE GlobalNames,       ONLY: VerifyUniqueBaseboardName
    USE General,           ONLY: RoundSigDigits
    USE ScheduleManager,   ONLY: GetScheduleIndex
    USE DataIPShortCuts

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER  :: RoutineName='GetBaseboardInput: ' ! include trailing blank space
    REAL(r64), PARAMETER :: MaxFraction       = 1.0d0   ! Maximum limit of fractional values
    REAL(r64), PARAMETER :: MinFraction       = 0.0d0   ! Minimum limit of fractional values
!    INTEGER,PARAMETER :: MaxDistribSurfaces   = 20      ! Maximum number of surfaces that a baseboard heater can radiate to
    INTEGER,PARAMETER :: MinDistribSurfaces   = 1       ! Minimum number of surfaces that a baseboard heater can radiate to

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AllFracsSummed         ! Sum of the fractions radiant
    INTEGER   :: BaseboardNum
    INTEGER   :: ElecBBNum
    INTEGER   :: NumAlphas
    INTEGER   :: NumNumbers
    INTEGER   :: SurfNum                ! surface number that radiant heat delivered
    INTEGER   :: IOSTAT
    LOGICAL   :: ErrorsFound = .false.  ! If errors detected in input
    LOGICAL   :: IsNotOK                ! Flag to verify name
    LOGICAL   :: IsBlank                ! Flag for blank name
    LOGICAL   :: errflag

    cCurrentModuleObject = cCMO_BBRadiator_Electric

    NumElecBaseboards = GetNumObjectsFound(cCurrentModuleObject)

    ! object is extensible, no max args needed as IPShortCuts being used

    ALLOCATE(ElecBaseboard(NumElecBaseboards))
    ALLOCATE(CheckEquipName(NumElecBaseboards))
    CheckEquipName=.true.

      DO BaseboardNum = 1,  NumElecBaseboards

        CALL GetObjectItem(cCurrentModuleObject,BaseboardNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),ElecBaseboard%EquipName,BaseboardNum,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          CYCLE
        ENDIF
        CALL VerifyUniqueBaseboardName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        ElecBaseboard(BaseboardNum)%EquipName = cAlphaArgs(1) ! name of this baseboard
        ElecBaseboard(BaseboardNum)%EquipType = BaseboardRadiator_Electric
        ElecBaseboard(BaseboardNum)%Schedule  = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          ElecBaseboard(BaseboardNum)%SchedPtr  = ScheduleAlwaysOn
        ELSE
          ElecBaseboard(BaseboardNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
          IF (ElecBaseboard(BaseboardNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
                                  ' entered ='//TRIM(cAlphaArgs(2))// &
                                  ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        ENDIF
        ! get inlet node number
        ElecBaseboard(BaseboardNum)%NominalCapacity      = rNumericArgs(1)
        ElecBaseboard(BaseboardNum)%BaseBoardEfficiency  = rNumericArgs(2)

      ElecBaseboard(BaseboardNum)%FracRadiant = rNumericArgs(3)
      IF (ElecBaseboard(BaseboardNum)%FracRadiant < MinFraction) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(3))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinFraction,2))//'].')
        ElecBaseboard(BaseboardNum)%FracRadiant = MinFraction
      END IF
      IF (ElecBaseboard(BaseboardNum)%FracRadiant > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(3))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,2))//'].')
        ElecBaseboard(BaseboardNum)%FracRadiant = MaxFraction
      END IF

         ! Remaining fraction is added to the zone as convective heat transfer
      AllFracsSummed = ElecBaseboard(BaseboardNum)%FracRadiant
      IF (AllFracsSummed > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", Fraction Radiant was higher than the allowable maximum.')
        ElecBaseboard(BaseboardNum)%FracRadiant = MaxFraction
        ElecBaseboard(BaseboardNum)%FracConvect = 0.0d0
      ELSE
        ElecBaseboard(BaseboardNum)%FracConvect = 1.0d0 - AllFracsSummed
      END IF

      ElecBaseboard(BaseboardNum)%FracDistribPerson = rNumericArgs(4)
      IF (ElecBaseboard(BaseboardNum)%FracDistribPerson < MinFraction) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(4))//' was lower than the allowable minimum.')
        CALL ShowContinueError('...reset to minimum value=['//trim(RoundSigDigits(MinFraction,2))//'].')
        ElecBaseboard(BaseboardNum)%FracDistribPerson = MinFraction
      END IF
      IF (ElecBaseboard(BaseboardNum)%FracDistribPerson > MaxFraction) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", '//trim(cNumericFieldNames(4))//' was higher than the allowable maximum.')
        CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,2))//'].')
        ElecBaseboard(BaseboardNum)%FracDistribPerson = MaxFraction
      END IF

      ElecBaseboard(BaseboardNum)%TotSurfToDistrib = NumNumbers - 4
!      IF (ElecBaseboard(BaseboardNum)%TotSurfToDistrib > MaxDistribSurfaces) THEN
!        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
!          '", the number of surface/radiant fraction groups entered was higher than the allowable maximum.')
!        CALL ShowContinueError('...only the maximum value=['//trim(RoundSigDigits(MaxDistribSurfaces))// &
!           '] will be processed.')
!        ElecBaseboard(BaseboardNum)%TotSurfToDistrib = MaxDistribSurfaces
!      END IF
      IF ( (ElecBaseboard(BaseboardNum)%TotSurfToDistrib < MinDistribSurfaces) .AND. &
           (ElecBaseboard(BaseboardNum)%FracRadiant > MinFraction) ) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", the number of surface/radiant fraction groups entered was less than the allowable minimum.')
        CALL ShowContinueError('...the minimum that must be entered=['//trim(RoundSigDigits(MinDistribSurfaces))//'].')
        ErrorsFound = .true.
        ElecBaseboard(BaseboardNum)%TotSurfToDistrib = 0  ! error
      END IF

      ALLOCATE(ElecBaseboard(BaseboardNum)%SurfaceName(ElecBaseboard(BaseboardNum)%TotSurfToDistrib))
      ElecBaseboard(BaseboardNum)%SurfaceName=' '
      ALLOCATE(ElecBaseboard(BaseboardNum)%SurfacePtr(ElecBaseboard(BaseboardNum)%TotSurfToDistrib))
      ElecBaseboard(BaseboardNum)%SurfacePtr=0
      ALLOCATE(ElecBaseboard(BaseboardNum)%FracDistribToSurf(ElecBaseboard(BaseboardNum)%TotSurfToDistrib))
      ElecBaseboard(BaseboardNum)%FracDistribToSurf=0.0d0

      AllFracsSummed = ElecBaseboard(BaseboardNum)%FracDistribPerson
      Do SurfNum = 1,ElecBaseboard(BaseboardNum)%TotSurfToDistrib
        ElecBaseboard(BaseboardNum)%SurfaceName(SurfNum) = cAlphaArgs(SurfNum + 2)
        ElecBaseboard(BaseboardNum)%SurfacePtr(SurfNum)  = FindItemInList(cAlphaArgs(SurfNum + 2),Surface%Name,TotSurfaces)
        ElecBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) = rNumericArgs(SurfNum + 4)
        IF (ElecBaseboard(BaseboardNum)%SurfacePtr(SurfNum) == 0 ) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cAlphaFieldNames(SurfNum+2))//'="'//trim(cAlphaArgs(SurfNum + 2))//'" invalid - not found.')
          ErrorsFound = .true.
        END IF
        IF (ElecBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) > MaxFraction) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cNumericFieldNames(SurfNum+4))//'was greater than the allowable maximum.')
          CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MaxFraction,2))//'].')
          ElecBaseboard(BaseboardNum)%TotSurfToDistrib = MaxFraction
        END IF
        IF (ElecBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum) < MinFraction) THEN
          CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
            '", '//trim(cNumericFieldNames(SurfNum+4))//'was less than the allowable minimum.')
          CALL ShowContinueError('...reset to maximum value=['//trim(RoundSigDigits(MinFraction,2))//'].')
          ElecBaseboard(BaseboardNum)%TotSurfToDistrib = MinFraction
        END IF
        IF (ElecBaseboard(BaseboardNum)%SurfacePtr(SurfNum) /= 0 ) THEN
          Surface(ElecBaseboard(BaseboardNum)%SurfacePtr(SurfNum))%IntConvSurfGetsRadiantHeat = .TRUE.
        ENDIF

        AllFracsSummed = AllFracsSummed + ElecBaseboard(BaseboardNum)%FracDistribToSurf(SurfNum)
      End DO ! Surfaces

      IF (AllFracsSummed > (MaxFraction + 0.01d0) ) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", Summed radiant fractions for people + surface groups > 1.0')
        ErrorsFound = .TRUE.
      END IF
      IF ( (AllFracsSummed < (MaxFraction - 0.01d0)) .AND. &              ! User didn't distribute all of the
           (ElecBaseboard(BaseboardNum)%FracRadiant > MinFraction) ) THEN ! radiation warn that some will be lost
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))// &
          '", Summed radiant fractions for people + surface groups < 1.0')
        CALL ShowContinueError('The rest of the radiant energy delivered by the baseboard heater will be lost')
      END IF
    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//TRIM(cCurrentModuleObject)//'Errors found getting input. Program terminates.')
    END IF

    DO BaseboardNum = 1,NumElecBaseboards

      ! Setup Report variables for the Electric BaseBoards
      ! CurrentModuleObject='ZoneHVAC:Baseboard:RadiantConvective:Electric'
      CALL SetupOutputVariable('Baseboard Total Heating Rate [W]', ElecBaseboard(BaseboardNum)%TotPower, &
                               'System','Average',ElecBaseboard(BaseboardNum)%EquipName)

      CALL SetupOutputVariable('Baseboard Convective Heating Rate [W]', ElecBaseboard(BaseboardNum)%ConvPower, &
                               'System','Average',ElecBaseboard(BaseboardNum)%EquipName)
      CALL SetupOutputVariable('Baseboard Radiant Heating Rate [W]', ElecBaseboard(BaseboardNum)%RadPower, &
                               'System','Average',ElecBaseboard(BaseboardNum)%EquipName)

      CALL SetupOutputVariable('Baseboard Electric Energy [J]',ElecBaseboard(BaseboardNum)%ElecUseLoad, &
                              'System','Sum',ElecBaseboard(BaseboardNum)%EquipName,  &
                                ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')
      CALL SetupOutputVariable('Baseboard Electric Power [W]',ElecBaseboard(BaseboardNum)%ElecUseRate, &
                              'System','Average',ElecBaseboard(BaseboardNum)%EquipName)
      CALL SetupOutputVariable('Baseboard Total Heating Energy [J]', ElecBaseboard(BaseboardNum)%TotEnergy, &
                               'System','Sum',ElecBaseboard(BaseboardNum)%EquipName, &
                                ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BASEBOARD',GroupKey='System')

      CALL SetupOutputVariable('Baseboard Convective Heating Energy [J]', ElecBaseboard(BaseboardNum)%ConvEnergy, &
                               'System','Sum',ElecBaseboard(BaseboardNum)%EquipName)
      CALL SetupOutputVariable('Baseboard Radiant Heating Energy [J]', ElecBaseboard(BaseboardNum)%RadEnergy, &
                               'System','Sum',ElecBaseboard(BaseboardNum)%EquipName)
    END DO

    RETURN
  END SUBROUTINE GetElectricBaseboardInput

SUBROUTINE InitElectricBaseboard(BaseboardNum, ControlledZoneNumSub, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       Feb 2010 Daeho Kang for radiant component
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the Baseboard units during simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode,       ONLY: Node
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BaseboardNum
  INTEGER, INTENT(IN) :: ControlledZoneNumSub
  LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: ZoneNode
  LOGICAL,SAVE  :: MyOneTimeFlag = .true.
  LOGICAL,SAVE  :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  INTEGER       :: ZoneNum
  INTEGER       :: Loop
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumElecBaseboards))
    ALLOCATE(MySizeFlag(NumElecBaseboards))
    ALLOCATE(ZeroSourceSumHATsurf(NumofZones))
             ZeroSourceSumHATsurf = 0.0D0
    ALLOCATE(QBBElecRadSource(NumElecBaseboards))
             QBBElecRadSource = 0.0D0
    ALLOCATE(QBBElecRadSrcAvg(NumElecBaseboards))
             QBBElecRadSrcAvg = 0.0D0
    ALLOCATE(LastQBBElecRadSrc(NumElecBaseboards))
             LastQBBElecRadSrc = 0.0D0
    ALLOCATE(LastSysTimeElapsed(NumElecBaseboards))
             LastSysTimeElapsed = 0.0D0
    ALLOCATE(LastTimeStepSys(NumElecBaseboards))
             LastTimeStepSys = 0.0D0
    MyEnvrnFlag = .TRUE.
    MySizeFlag  = .TRUE.

    MyOneTimeFlag = .false.

  END IF

    IF (ElecBaseboard(BaseboardNum)%ZonePtr <= 0) &
      ElecBaseboard(BaseboardNum)%ZonePtr = ZoneEquipConfig(ControlledZoneNumSub)%ActualZoneNum

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(BaseboardNum)) THEN
    ! for each coil, do the sizing once.
    CALL SizeElectricBaseboard(BaseboardNum)
        MySizeFlag(BaseboardNum) = .FALSE.
  END IF

  ! need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumElecBaseboards
      IF (CheckZoneEquipmentList(cCMO_BBRadiator_Electric,ElecBaseboard(Loop)%EquipName)) CYCLE
      CALL ShowSevereError('InitBaseboard: Unit=['//cCMO_BBRadiator_Electric//','//  &
         TRIM(ElecBaseboard(Loop)%EquipName)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

       ! Do the Begin Environment initializations
    IF (BeginEnvrnFlag .and. MyEnvrnFlag(BaseboardNum)) THEN
        ! Initialize
    ZeroSourceSumHATsurf   =0.0D0
    QBBElecRadSource       =0.0D0
    QBBElecRadSrcAvg       =0.0D0
    LastQBBElecRadSrc      =0.0D0
    LastSysTimeElapsed     =0.0D0
    LastTimeStepSys        =0.0D0

    MyEnvrnFlag(BaseboardNum) = .FALSE.
    END IF

    IF (.not. BeginEnvrnFlag) THEN
        MyEnvrnFlag(BaseboardNum) = .true.
    ENDIF

       ! Do the Begin Day initializations
    IF (BeginDayFlag) THEN

    END IF

    IF (BeginTimeStepFlag .AND. FirstHVACIteration) THEN
      ZoneNum = ElecBaseboard(BaseboardNum)%ZonePtr
      ZeroSourceSumHATsurf(ZoneNum)    = SumHATsurf(ZoneNum)
      QBBElecRadSrcAvg(BaseboardNum)   = 0.0D0
      LastQBBElecRadSrc(BaseboardNum)  = 0.0D0
      LastSysTimeElapsed(BaseboardNum) = 0.0D0
      LastTimeStepSys(BaseboardNum)    = 0.0D0
    END IF

      ! Do the every time step initializations
  ZoneNode = ZoneEquipConfig(ControlledZoneNumSub)%ZoneNode
  ElecBaseboard(BaseboardNum)%AirInletTemp = Node(ZoneNode)%Temp
  ElecBaseboard(BaseboardNum)%AirInletHumRat = Node(ZoneNode)%HumRat

     ! Set the reporting variables to zero at each timestep.
  ElecBaseboard(BaseboardNum)%TotPower    = 0.0d0
  ElecBaseboard(BaseboardNum)%Power       = 0.0d0
  ElecBaseboard(BaseboardNum)%ConvPower   = 0.0d0
  ElecBaseboard(BaseboardNum)%RadPower    = 0.0d0
  ElecBaseboard(BaseboardNum)%TotEnergy   = 0.0d0
  ElecBaseboard(BaseboardNum)%Energy      = 0.0d0
  ElecBaseboard(BaseboardNum)%ConvEnergy  = 0.0d0
  ElecBaseboard(BaseboardNum)%RadEnergy   = 0.0d0
  ElecBaseboard(BaseboardNum)%ElecUseLoad = 0.0d0
  ElecBaseboard(BaseboardNum)%ElecUseRate = 0.0d0

 RETURN
END SUBROUTINE InitElectricBaseboard

SUBROUTINE SizeElectricBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing electric baseboard components for which nominal capacities have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
          ! calculated by numerically inverting the baseboard calculation routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: NominalCapacityDes    ! Design capacity value for reproting
  REAL(r64) :: NominalCapacityUser   ! User hard-sized UA value for reproting
  LOGICAL   :: IsAutosize            ! Indicator to autosizing nominal capacity

  IsAutosize = .FALSE.
  NominalCapacityDes = 0.0d0
  NominalCapacityUser = 0.0d0

  IF (CurZoneEqNum > 0) THEN

    IF (ElecBaseboard(BaseboardNum)%NominalCapacity == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF
        ! Check if all are hard-sized
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation should continue
      IF (ElecBaseboard(BaseboardNum)%NominalCapacity > 0.0d0) THEN
        CALL ReportSizingOutput(cCMO_BBRadiator_Electric,ElecBaseboard(BaseboardNum)%EquipName,&
                                'User-Specified Nominal Capacity [W]',ElecBaseboard(BaseboardNum)%NominalCapacity)
      END IF
    ELSE ! Autosize or hard-size with sizing run
      CALL CheckZoneSizing(cCMO_BBRadiator_Electric,ElecBaseboard(BaseboardNum)%EquipName)
         NominalCapacityDes = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad *   &
                              CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
      IF (IsAutoSize) THEN
        ElecBaseboard(BaseboardNum)%NominalCapacity = NominalCapacityDes
        CALL ReportSizingOutput(cCMO_BBRadiator_Electric,ElecBaseboard(BaseboardNum)%EquipName,&
                                'Design Size Nominal Capacity [W]',NominalCapacityDes)
      ELSE ! Hard-size with sizing data
        IF (ElecBaseboard(BaseboardNum)%NominalCapacity > 0.0d0 .AND. NominalCapacityDes > 0.0d0) THEN
          NominalCapacityUser = ElecBaseboard(BaseboardNum)%NominalCapacity
          CALL ReportSizingOutput(cCMO_BBRadiator_Electric,ElecBaseboard(BaseboardNum)%EquipName,&
                                'Design Size Nominal Capacity [W]',NominalCapacityDes, &
                                'User-Specified Nominal Capacity [W]',NominalCapacityUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(NominalCapacityDes - NominalCapacityUser)/NominalCapacityUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeElecBaseboard: Potential issue with equipment sizing for ' &
                                     //'ZoneHVAC:Baseboard:RadiantConvective:Electric="'//  &
                                     TRIM(ElecBaseboard(BaseboardNum)%EquipName)//'".')
              CALL ShowContinueError('User-Specified Nominal Capacity of '// &
                                      TRIM(RoundSigDigits(NominalCapacityUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Nominal Capacity of ' // &
                                      TRIM(RoundSigDigits(NominalCapacityDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  RETURN
END SUBROUTINE SizeElectricBaseboard

SUBROUTINE CalcElectricBaseboard(BaseboardNum, ControlledZoneNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       Feb 2010 Daeho Kang for radiant component
          !                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the heat exchange rate in a Electric baseboard heater.
          ! It includes radiant heat transfer to people and surfaces in a space, and the actual convective
          ! system impact of a electric baseboard heater is determined after the radiant heat distribution.

          !
          ! METHODOLOGY EMPLOYED:
          ! This is primarily modified from Convective Electric Baseboard. An existing algorithm of radiant
          ! heat transfer calculation in the High Tmeperature Radiant System module is implemented.

          ! REFERENCES:

          ! USE STATEMENTS:
    USE Psychrometrics,     ONLY: PsyCpAirFnWTdb
    USE ScheduleManager,    ONLY: GetScheduleIndex, GetCurrentScheduleValue
    USE DataZoneEquipment,  ONLY: ZoneEquipConfig
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
    USE DataInterfaces, ONLY: CalcHeatBalanceOutsideSurf, CalcHeatBalanceInsideSurf
    USE DataHVACGlobals, ONLY: SmallLoad
    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)    :: BaseboardNum
    INTEGER, INTENT(IN)    :: ControlledZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER :: SimpConvAirFlowSpeed = 0.5d0 ! m/s
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER   :: ZoneNum
    REAL(r64) :: AirInletTemp
    REAL(r64) :: CpAir
    REAL(r64) :: AirMassFlowRate
    REAL(r64) :: CapacitanceAir
    REAL(r64) :: Effic
    REAL(r64) :: AirOutletTemp
    REAL(r64) :: QBBCap
    REAL(r64) :: RadHeat
    REAL(r64) :: QZnReq
    REAL(r64) :: LoadMet

    ZoneNum = ElecBaseboard(BaseboardNum)%ZonePtr
    QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
    AirInletTemp = ElecBaseboard(BaseboardNum)%AirInletTemp
    AirOutletTemp = AirInletTemp
    CpAir = PsyCpAirFnWTdb(ElecBaseboard(BaseboardNum)%AirInletHumRat,AirInletTemp)
    AirMassFlowRate = SimpConvAirFlowSpeed
    CapacitanceAir = CpAir * AirMassFlowRate

    ! Currently only the efficiency is used to calculate the electric consumption.  There could be some
    ! thermal loss that could be accounted for with this efficiency input.
    Effic = ElecBaseboard(BaseboardNum)%BaseBoardEfficiency

    IF (QZnReq > SmallLoad &
        .AND. .NOT. CurDeadBandOrSetback(ZoneNum) &
        .AND. GetCurrentScheduleValue(ElecBaseboard(BaseboardNum)%SchedPtr) > 0.0d0) THEN

            ! If the load exceeds the capacity than the capacity is set to the BB limit.
      IF(QZnReq > ElecBaseboard(BaseboardNum)%NominalCapacity) THEN
        QBBCap = ElecBaseboard(BaseboardNum)%NominalCapacity
      Else
        QBBCap = QZnReq
      End IF
      RadHeat = QBBCap * ElecBaseboard(BaseboardNum)%FracRadiant
      QBBElecRadSource(BaseboardNum) = RadHeat

      IF (ElecBaseboard(BaseboardNum)%FracRadiant > 0.0d0) THEN  ! User defines radiant heat addition
            ! Now, distribute the radiant energy of all systems to the appropriate surfaces, to people, and the air
        CALL DistributeBBElecRadGains
            ! Now "simulate" the system by recalculating the heat balances
        CALL CalcHeatBalanceOutsideSurf(ZoneNum)
        CALL CalcHeatBalanceInsideSurf(ZoneNum)
            ! Here an assumption is made regarding radiant heat transfer to people.
            ! While the radiant heat transfer to people array will be used by the thermal comfort
            ! routines, the energy transfer to people would get lost from the perspective
            ! of the heat balance.  So, to avoid this net loss of energy which clearly
            ! gets added to the zones, we must account for it somehow.  This assumption
            ! that all energy radiated to people is converted to convective energy is
            ! not very precise, but at least it conserves energy. The system impact to heat balance
            ! should include this.
        LoadMet = (SumHATsurf(ZoneNum) - ZeroSourceSumHATsurf(ZoneNum)) &
                 + (QBBCap *  ElecBaseboard(BaseboardNum)%FracConvect) &
                 + (RadHeat * ElecBaseboard(BaseboardNum)%FracDistribPerson)

        IF (LoadMet < 0.0d0) THEN ! No cooling output
          AirOutletTemp = AirInletTemp
          ElecBaseboard(BaseboardNum)%ElecUseRate = 0.0d0
        ELSE
          AirOutletTemp = AirInletTemp + QBBCap/CapacitanceAir
            ! This could be utilized somehow or even reported so the data structures are left in place
            ! The BaseBoard electric Load is calculated using the efficiency
          ElecBaseboard(BaseboardNum)%ElecUseRate = QBBCap/Effic
        END IF

      ELSE ! zero radiant fraction, no need of recalculation of heat balances

        LoadMet = QBBCap
        ElecBaseboard(BaseboardNum)%ElecUseRate = QBBCap/Effic

      END IF

    ELSE  ! If there is an off condition the BB does nothing.

      QBBCap  = 0.0d0
      LoadMet = 0.0d0
      RadHeat = 0.0d0
      AirOutletTemp = AirInletTemp
      QBBElecRadSource(BaseboardNum) = 0.0d0
      ElecBaseboard(BaseboardNum)%ElecUseRate = 0.0d0
    END IF

        ! Assign calculated ones
    ElecBaseboard(BaseboardNum)%AirOutletTemp = AirOutletTemp
    ElecBaseboard(BaseboardNum)%Power     = QBBCap
    ElecBaseboard(BaseboardNum)%TotPower  = LoadMet
    ElecBaseboard(BaseboardNum)%RadPower  = RadHeat
    ElecBaseboard(BaseboardNum)%ConvPower = QBBCap - RadHeat

    RETURN
  END SUBROUTINE CalcElectricBaseboard

  SUBROUTINE UpdateElectricBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !                      Rick Strand
          !       DATE WRITTEN   Nov 1997
          !                      February 2001
          !       MODIFIED       Feb 2010 Daeho Kang for radiant component
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! The update subrotines in water baseboard radiator is modified.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals,       ONLY: TimeStepZone, BeginEnvrnFlag
    USE DataHVACGlobals,   ONLY: TimeStepSys, SysTimeElapsed

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)  :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER,SAVE :: Iter = 0
    Logical,save :: MyEnvrnFlag = .true.

    IF (Beginenvrnflag .and. Myenvrnflag) then
        Iter=0
        Myenvrnflag = .false.
    END IF
    IF (.not. Beginenvrnflag) then
        Myenvrnflag=.true.
    END IF

          ! First, update the running average if necessary...
    IF (LastSysTimeElapsed(BaseboardNum) == SysTimeElapsed) THEN
        QBBElecRadSrcAvg(BaseboardNum) = QBBElecRadSrcAvg(BaseboardNum) - LastQBBElecRadSrc(BaseboardNum) * &
                                     LastTimeStepSys(BaseboardNum) / TimeStepZone
    END IF
          ! Update the running average and the "last" values with the current values of the appropriate variables
    QBBElecRadSrcAvg(BaseboardNum) = QBBElecRadSrcAvg(BaseboardNum) + &
                                     QBBElecRadSource(BaseboardNum) * TimeStepSys / TimeStepZone

    LastQBBElecRadSrc(BaseboardNum)  = QBBElecRadSource(BaseboardNum)
    LastSysTimeElapsed(BaseboardNum) = SysTimeElapsed
    LastTimeStepSys(BaseboardNum)    = TimeStepSys

    RETURN

  END SUBROUTINE UpdateElectricBaseboard

  SUBROUTINE UpdateBBElecRadSourceValAvg(ElecBaseboardSysOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       Feb 2010 Daeho Kang for baseboard
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To transfer the average value of the heat source over the entire
          ! zone time step back to the heat balance routines so that the heat
          ! balance algorithms can simulate one last time with the average source
          ! to maintain some reasonable amount of continuity and energy balance
          ! in the temperature and flux histories.

          ! METHODOLOGY EMPLOYED:
          ! All of the record keeping for the average term is done in the Update
          ! routine so the only other thing that this subroutine does is check to
          ! see if the system was even on.  If any average term is non-zero, then
          ! one or more of the radiant systems was running.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(OUT) :: ElecBaseboardSysOn   ! .TRUE. if the radiant system has run this zone time step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: BaseboardNum    ! DO loop counter for surface index
  INTEGER :: ZoneNum    ! DO loop counter for surface index

       ! FLOW:
    ElecBaseboardSysOn = .FALSE.

       ! If this was never allocated, then there are no radiant systems in this input file (just RETURN)
    IF (.NOT.ALLOCATED(QBBElecRadSrcAvg)) RETURN

       ! If it was allocated, then we have to check to see if this was running at all...
    DO BaseboardNum = 1, NumElecBaseboards
      IF (QBBElecRadSrcAvg(BaseboardNum) /= 0.0D0) THEN
          ElecBaseboardSysOn = .TRUE.
        EXIT !DO loop
      END IF
    END DO

       QBBElecRadSource = QBBElecRadSrcAvg

  ! QBBElecRadSource has been modified so we need to redistribute gains

  CALL DistributeBBElecRadGains

  RETURN

END SUBROUTINE UpdateBBElecRadSourceValAvg

  SUBROUTINE DistributeBBElecRadGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2001
          !       MODIFIED       Feb 2010 Daeho Kang for baseboard
          !                      April 2010 Brent Griffith, max limit to protect surface temperature calcs
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! To distribute the gains from the electric basebaord heater
          ! as specified in the user input file.  This includes distribution
          ! of long wavelength radiant gains to surfaces and "people."

          ! METHODOLOGY EMPLOYED:
          ! We must cycle through all of the radiant systems because each
          ! surface could feel the effect of more than one radiant system.
          ! Note that the energy radiated to people is assumed to affect them
          ! but them it is assumed to be convected to the air.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys, ONLY : QElecBaseboardToPerson, QElecBaseboardSurf, MaxRadHeatFlux
  USE General,           ONLY : RoundSigDigits
  USE DataSurfaces,      ONLY : Surface, TotSurfaces
  USE DataZoneEquipment, ONLY : ZoneEquipConfig
  USE DataInterfaces,    ONLY : ShowContinueError, ShowWarningError, ShowSevereError, &
                                ShowFatalError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: SmallestArea = 0.001d0   ! Smallest area in meters squared (to avoid a divide by zero)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: RadSurfNum      ! Counter for surfaces receiving radiation from radiant heater
  INTEGER   :: BaseboardNum    ! Counter for the baseboard
  INTEGER   :: SurfNum         ! Pointer to the Surface derived type
  INTEGER   :: ZoneNum        ! Pointer to the Zone derived type
  REAL(R64) :: ThisSurfIntensity ! temporary for W/m2 term for rad on a surface

          ! FLOW:
          ! Initialize arrays
    QElecBaseboardSurf     = 0.0D0
    QElecBaseboardToPerson = 0.0D0

    DO BaseboardNum = 1, NumElecBaseboards

      ZoneNum = ElecBaseboard(BaseboardNum)%ZonePtr
      QElecBaseboardToPerson(ZoneNum) = QElecBaseboardToPerson(ZoneNum) + &
                                        QBBElecRadSource(BaseboardNum) * ElecBaseboard(BaseboardNum)%FracDistribPerson

      DO RadSurfNum = 1, ElecBaseboard(BaseboardNum)%TotSurfToDistrib
         SurfNum = ElecBaseboard(BaseboardNum)%SurfacePtr(RadSurfNum)
        IF (Surface(SurfNum)%Area > SmallestArea) THEN
          ThisSurfIntensity = (QBBElecRadSource(BaseboardNum) &
                                        * ElecBaseboard(BaseboardNum)%FracDistribToSurf(RadSurfNum) &
                                        / Surface(SurfNum)%Area)
          QElecBaseboardSurf(SurfNum) = QElecBaseboardSurf(SurfNum) + ThisSurfIntensity
          IF (ThisSurfIntensity > MaxRadHeatFlux) THEN
            CALL ShowSevereError('DistributeBBElecRadGains:  excessive thermal radiation heat flux intensity detected')
            CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
            CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
            CALL ShowContinueError('Occurs in '//cCMO_BBRadiator_Electric//' = '//Trim(ElecBaseboard(BaseboardNum)%EquipName))
            CALL ShowContinueError('Radiation intensity = '//Trim(RoundSigDigits(ThisSurfIntensity,2))//' [W/m2]')
            CALL ShowContinueError('Assign a larger surface area or more surfaces in '//cCMO_BBRadiator_Electric )
            CALL ShowFatalError('DistributeBBElecRadGains:  excessive thermal radiation heat flux intensity detected')
          ENDIF
        ELSE
          CALL ShowSevereError('DistributeBBElecRadGains:  surface not large enough to receive thermal radiation heat flux')
          CALL ShowContinueError('Surface = '//TRIM(Surface(SurfNum)%Name) )
          CALL ShowContinueError('Surface area = '//TRIM(RoundSigDigits(Surface(SurfNum)%Area,3))//' [m2]')
          CALL ShowContinueError('Occurs in '//cCMO_BBRadiator_Electric//' = '//Trim(ElecBaseboard(BaseboardNum)%EquipName))
          CALL ShowContinueError('Assign a larger surface area or more surfaces in '//cCMO_BBRadiator_Electric )
          CALL ShowFatalError('DistributeBBElecRadGains:  surface not large enough to receive thermal radiation heat flux')
        ENDIF
      END DO

    END DO

  RETURN

END SUBROUTINE DistributeBBElecRadGains

  SUBROUTINE ReportElectricBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       Feb 2010 Daeho Kang for additional variables
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    Use DataHVACGlobals, ONLY: TimeStepSys

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    ElecBaseboard(BaseboardNum)%ElecUseLoad = ElecBaseboard(BaseboardNum)%ElecUseRate * TimeStepSys*SecInHour
    ElecBaseboard(BaseboardNum)%TotEnergy   = ElecBaseboard(BaseboardNum)%TotPower    * TimeStepSys*SecInHour
    ElecBaseboard(BaseboardNum)%Energy      = ElecBaseboard(BaseboardNum)%Power       * TimeStepSys*SecInHour
    ElecBaseboard(BaseboardNum)%ConvEnergy  = ElecBaseboard(BaseboardNum)%ConvPower   * TimeStepSys*SecInHour
    ElecBaseboard(BaseboardNum)%RadEnergy   = ElecBaseboard(BaseboardNum)%RadPower    * TimeStepSys*SecInHour

    RETURN
  END SUBROUTINE ReportElectricBaseboard

REAL(r64) FUNCTION SumHATsurf(ZoneNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
          ! The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
          ! and should be updated accordingly.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSurfaces
  USE DataHeatBalance
  USE DataHeatBalSurface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum     ! Zone number

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: SurfNum     ! Surface number
  REAL(r64)           :: Area        ! Effective surface area

          ! FLOW:
  SumHATsurf = 0.0d0

  DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

    Area = Surface(SurfNum)%Area

    IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN
      IF (SurfaceWindow(SurfNum)%ShadingFlag == IntShadeOn .OR. SurfaceWindow(SurfNum)%ShadingFlag == IntBlindOn) THEN
        ! The area is the shade or blind area = the sum of the glazing area and the divider area (which is zero if no divider)
        Area = Area + SurfaceWindow(SurfNum)%DividerArea
      END IF

      IF (SurfaceWindow(SurfNum)%FrameArea > 0.0d0) THEN
        ! Window frame contribution
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%FrameArea &
          * (1.0d0 + SurfaceWindow(SurfNum)%ProjCorrFrIn) * SurfaceWindow(SurfNum)%FrameTempSurfIn
      END IF

      IF (SurfaceWindow(SurfNum)%DividerArea > 0.0d0 .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntShadeOn &
           .AND. SurfaceWindow(SurfNum)%ShadingFlag /= IntBlindOn) THEN
        ! Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
        SumHATsurf = SumHATsurf + HConvIn(SurfNum) * SurfaceWindow(SurfNum)%DividerArea &
          * (1.0d0 + 2.0d0 * SurfaceWindow(SurfNum)%ProjCorrDivIn) * SurfaceWindow(SurfNum)%DividerTempSurfIn
      END IF
    END IF

    SumHATsurf = SumHATsurf + HConvIn(SurfNum) * Area * TempSurfInTmp(SurfNum)
  END DO

  RETURN

END FUNCTION SumHATsurf

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
END MODULE ElectricBaseboardRadiator