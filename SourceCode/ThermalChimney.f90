MODULE ThermalChimney
  ! Module containing the data for Thermal Chimney system

  ! MODULE INFORMATION:
  !       AUTHOR         Kwang Ho Lee
  !       DATE WRITTEN   April 2008
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithyms required to manage the ThermalChimney System Component

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! 1. N. K. Bansal, R. Mathur and M. S. Bhandari, "Solar Chimney for Enhanced Stack Ventilation",
  ! Building and Environment, 28, pp. 373-377, 1993
  ! 2. K. S. Ong, "A Mathematical Model of a Solar Chimney", Renewable Energy, 28, pp. 1047-1060, 2003
  ! 3. J. Marti-Herrero and M. R. Heras-Celemin, "Dynamic Physical Model for a Solar Chimney",
  ! Solar Energy, 81, pp. 614-622, 2007

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataGlobals
USE DataEnvironment
USE DataHeatBalFanSys
USE DataHeatBalance
USE DataSurfaces
USE DataHeatBalSurface
USE DataInterfaces

          ! Use statements for access to subroutines in other modules
Use Psychrometrics

IMPLICIT NONE

! DERIVED TYPE DEFINITIONS
TYPE ThermalChimneyData
    CHARACTER(len=MaxNameLength) :: Name           =' '
    INTEGER :: RealZonePtr                         =0
    CHARACTER(len=MaxNameLength) :: RealZoneName   =' '
    INTEGER :: SchedPtr                            =0
    CHARACTER(len=MaxNameLength) :: SchedName      =' '
    REAL(r64)                   :: AbsorberWallWidth    =0.0d0
    REAL(r64)                   :: AirOutletCrossArea   =0.0d0
    REAL(r64)                    :: DischargeCoeff       =0.0d0
    INTEGER                :: TotZoneToDistrib     =0
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ZonePtr        !
    CHARACTER(len=MaxNameLength), &
             ALLOCATABLE, DIMENSION(:) :: ZoneName       !
    REAL(r64) ,    ALLOCATABLE, DIMENSION(:) :: DistanceThermChimInlet !
    REAL(r64) ,    ALLOCATABLE, DIMENSION(:) :: RatioThermChimAirFlow
    REAL(r64) ,    ALLOCATABLE, DIMENSION(:) :: EachAirInletCrossArea
END TYPE ThermalChimneyData

Type ThermChimZnReportVars
  REAL(r64)    :: ThermalChimneyHeatLoss      =0.0d0 ! Heat Gain {Joules} due to ThermalChimney
  REAL(r64)    :: ThermalChimneyHeatGain      =0.0d0 ! Heat Loss {Joules} due to ThermalChimney
  REAL(r64)    :: ThermalChimneyVolume        =0.0d0 ! Volume of Air {m3} due to ThermalChimney
  REAL(r64)    :: ThermalChimneyMass          =0.0d0 ! Mass of Air {kg} due to ThermalChimney
END TYPE ThermChimZnReportVars

Type ThermChimReportVars
  REAL(r64)    :: OverallTCVolumeFlow        =0.0d0 ! Volume of Air {m3/s} due to ThermalChimney
  REAL(r64)    :: OverallTCMassFlow          =0.0d0 ! Mass of Air {kg/s} due to ThermalChimney
  REAL(r64)    :: OutletAirTempThermalChim   =0.0d0 ! Air Temp {C} of ThermalChimney
END TYPE ThermChimReportVars

TYPE (ThermalChimneyData), ALLOCATABLE, DIMENSION(:) :: ThermalChimneySys
TYPE (ThermChimZnReportVars), ALLOCATABLE, DIMENSION(:) :: ZnRptThermChim
TYPE (ThermChimReportVars), ALLOCATABLE, DIMENSION(:) :: ThermalChimneyReport
INTEGER :: TotThermalChimney  =0 ! Total ThermalChimney Statements in input

 ! Subroutine Specifications for the Heat Balance Module
          ! Driver Routines
PUBLIC  ManageThermalChimney
          ! Get Input routines for module
PRIVATE GetThermalChimney
          ! Algorithms for the module
PRIVATE CalcThermalChimney
          ! Reporting routines for module
PRIVATE ReportThermalChimney
          ! Utility routines for module
PRIVATE GaussElimination

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE ManageThermalChimney

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   April 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the simulation of ThermalChimney unit.
          ! This driver manages the calls to all of
          ! the other drivers and simulation algorithms.

          ! METHODOLOGY EMPLOYED:
          ! na

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
  LOGICAL, SAVE :: GetInputFlag=.true.
  LOGICAL :: ErrorsFound=.false.


  ! Obtains and Allocates heat balance related parameters from input file
  IF (GetInputFlag) THEN
    CALL GetThermalChimney(ErrorsFound)
    GetInputFlag=.false.
  ENDIF

  IF (TotThermalChimney == 0) RETURN

  CALL CalcThermalChimney

  CALL ReportThermalChimney

  RETURN

END SUBROUTINE ManageThermalChimney



SUBROUTINE GetThermalChimney(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   April 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine obtains input data for ThermalChimney units and
          ! stores it in the ThermalChimney data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList,VerifyName
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE General, ONLY: RoundSigDigits
  USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '
  REAL(r64),        PARAMETER :: FlowFractionTolerance = 0.0001d0 ! Smallest deviation from unity for the sum of all fractions

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    CHARACTER(len=MaxNameLength), DIMENSION(23) :: AlphaName
!    REAL(r64) , DIMENSION(63)              :: IHGNumbers
    INTEGER :: NumAlpha
    INTEGER :: NumNumber
    REAL(r64) :: AllRatiosSummed  !
    INTEGER :: TCZoneNum    ! Thermal chimney zone counter
    INTEGER :: TCZoneNum1    ! Thermal chimney zone counter
    INTEGER :: IOStat
    INTEGER :: Loop
    INTEGER :: Loop1
    LOGICAL :: IsNotOK
    LOGICAL :: IsBlank

!  ALLOCATE(MCPTThermChim(NumOfZones))
!  MCPTThermChim=0.0
!  ALLOCATE(MCPThermChim(NumOfZones))
!  MCPThermChim=0.0
!  ALLOCATE(ThermChimAMFL(NumOfZones))
!  ThermChimAMFL=0.0

! Following used for reporting
ALLOCATE (ZnRptThermChim (NumOfZones))

cCurrentModuleObject='ZoneThermalChimney'
TotThermalChimney=GetNumObjectsFound(cCurrentModuleObject)

ALLOCATE (ThermalChimneySys(TotThermalChimney))
ALLOCATE (ThermalChimneyReport(TotThermalChimney))

DO Loop=1, TotThermalChimney

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

          ! First Alpha is Thermal Chimney Name
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ThermalChimneySys%Name,Loop,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) THEN
        CYCLE
      ELSE
        cAlphaArgs(1)=TRIM(cAlphaArgs(1))//'--dup'
      ENDIF
    ENDIF
    ThermalChimneySys(Loop)%Name = cAlphaArgs(1)

          ! Second Alpha is Zone Name
    ThermalChimneySys(Loop)%RealZonePtr = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (ThermalChimneySys(Loop)%RealZonePtr == 0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid Zone')
      CALL ShowContinueError('invalid - not found '//trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'".')
      ErrorsFound = .TRUE.
    ELSEIF (.not. Zone(ThermalChimneySys(Loop)%RealZonePtr)%HasWindow) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid Zone')
      CALL ShowContinueError('...invalid - no window(s) in '//trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'".')
      CALL ShowContinueError('...thermal chimney zones must have window(s).')
      ErrorsFound = .TRUE.
    ENDIF
    ThermalChimneySys(Loop)%RealZoneName = cAlphaArgs(2)

    ThermalChimneySys(Loop)%SchedName = cAlphaArgs(3)
    IF (lAlphaFieldBlanks(3)) THEN
      ThermalChimneySys(Loop)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      ThermalChimneySys(Loop)%SchedPtr  = GetScheduleIndex(cAlphaArgs(3))
      IF (ThermalChimneySys(Loop)%SchedPtr == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
        CALL ShowContinueError('Invalid-not found '//trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
      END IF
    END IF

    ThermalChimneySys(Loop)%AbsorberWallWidth = rNumericArgs(1)
    IF (ThermalChimneySys(Loop)%AbsorberWallWidth < 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
           trim(cNumericFieldNames(1))//' must be >= 0, entered value=['//trim(RoundSigDigits(rNumericArgs(1),2))//'].')
      ErrorsFound = .TRUE.
    END IF

    ThermalChimneySys(Loop)%AirOutletCrossArea = rNumericArgs(2)
    IF (ThermalChimneySys(Loop)%AirOutletCrossArea < 0.0d0) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
           trim(cNumericFieldNames(2))//' must be >= 0, entered value=['//trim(RoundSigDigits(rNumericArgs(2),2))//'].')
      ErrorsFound = .TRUE.
    END IF

    ThermalChimneySys(Loop)%DischargeCoeff = rNumericArgs(3)
    IF ((ThermalChimneySys(Loop)%DischargeCoeff <= 0.0d0) .OR. &
        (ThermalChimneySys(Loop)%DischargeCoeff >  1.0d0)) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
           trim(cNumericFieldNames(3))//' must be > 0 and <=1.0, entered value=['//trim(RoundSigDigits(rNumericArgs(3),2))//'].')
      ErrorsFound = .TRUE.
    END IF

    ThermalChimneySys(Loop)%TotZoneToDistrib = NumAlpha - 3
    ALLOCATE(ThermalChimneySys(Loop)%ZonePtr(ThermalChimneySys(Loop)%TotZoneToDistrib))
    ALLOCATE(ThermalChimneySys(Loop)%ZoneName(ThermalChimneySys(Loop)%TotZoneToDistrib))
    ALLOCATE(ThermalChimneySys(Loop)%DistanceThermChimInlet(ThermalChimneySys(Loop)%TotZoneToDistrib))
    ALLOCATE(ThermalChimneySys(Loop)%RatioThermChimAirFlow(ThermalChimneySys(Loop)%TotZoneToDistrib))
    ALLOCATE(ThermalChimneySys(Loop)%EachAirInletCrossArea(ThermalChimneySys(Loop)%TotZoneToDistrib))

    AllRatiosSummed = 0.0d0
    DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
      ThermalChimneySys(Loop)%ZoneName(TCZoneNum)  = cAlphaArgs(TCZoneNum+3)
      ThermalChimneySys(Loop)%ZonePtr(TCZoneNum)   = FindIteminList(cAlphaArgs(TCZoneNum+3),Zone%Name,NumOfZones)
      ThermalChimneySys(Loop)%DistanceThermChimInlet(TCZoneNum) = rNumericArgs(3*TCZoneNum+1)
      ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum) = rNumericArgs(3*TCZoneNum+2)
      IF (lNumericFieldBlanks(3*TCZoneNum+2)) ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum) = 1.0d0
      ThermalChimneySys(Loop)%EachAirInletCrossArea(TCZoneNum) = rNumericArgs(3*TCZoneNum+3)

          !!! Error trap for zones that do not exist or zones not in the zone the thermal chimney is in
      IF (ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
             trim(cAlphaFieldNames(TCZoneNum+3))//'="'//trim(cAlphaArgs(TCZoneNum+3))//'" not found.')
        ErrorsFound=.true.
      ELSE IF (ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop)%RealZonePtr) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid reference '//   &
            trim(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2)))
        CALL ShowContinueError('...must not have same zone as reference= '//  &
             trim(cAlphaFieldNames(TCZoneNum+3))//'="'//trim(cAlphaArgs(TCZoneNum+3))//'".')
        ErrorsFound=.true.
      END IF

      IF (ThermalChimneySys(Loop)%DistanceThermChimInlet(TCZoneNum) < 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
              trim(cNumericFieldNames(3*TCZoneNum+1))//' must be >= 0, entered value=['//  &
              trim(RoundSigDigits(rNumericArgs(3*TCZoneNum+1),2))//'].')
        ErrorsFound = .TRUE.
      END IF

      IF ((ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum) <= 0.0d0) .OR. &
          (ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum) >  1.0d0)) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
                trim(cNumericFieldNames(3*TCZoneNum+2))//' must be > 0 and <=1.0, entered value=['//  &
                trim(RoundSigDigits(rNumericArgs(3*TCZoneNum+2),2))//'].')
        ErrorsFound = .TRUE.
      END IF

      IF (ThermalChimneySys(Loop)%EachAirInletCrossArea(TCZoneNum) < 0.0d0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
              trim(cNumericFieldNames(3*TCZoneNum+3))//' must be >= 0, entered value=['//  &
              trim(RoundSigDigits(rNumericArgs(3*TCZoneNum+3),2))//'].')
        ErrorsFound = .TRUE.
      END IF

      AllRatiosSummed = AllRatiosSummed + ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum)

    END DO  ! DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib

          ! Error trap if the sum of fractions is not equal to 1.0
    IF (ABS(AllRatiosSummed-1.0d0) > FlowFractionTolerance) THEN
      CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid '//   &
                'sum of fractions, must be =1.0, entered value (summed from entries)=['//  &
                trim(RoundSigDigits(AllRatiosSummed,4))//'].')
      ErrorsFound = .TRUE.
    END IF

END DO     ! DO Loop=1, TotThermalChimney

! Set up the output variables for thermal chimneys
  DO Loop=1, TotThermalChimney
    CALL SetupOutputVariable('Zone Thermal Chimney Volume Flow Rate [m3/s]',   &
                              ThermalChimneyReport(Loop)%OverallTCVolumeFlow,'System','Average',  &
                              ThermalChimneySys(Loop)%Name)
    CALL SetupOutputVariable('Zone Thermal Chimney Mass Flow Rate [kg/s]',   &
                              ThermalChimneyReport(Loop)%OverallTCMassFlow,'System','Average',  &
                              ThermalChimneySys(Loop)%Name)
    CALL SetupOutputVariable('Zone Thermal Chimney Outlet Temperature [C]',   &
                              ThermalChimneyReport(Loop)%OutletAirTempThermalChim,'System','Average',  &
                              ThermalChimneySys(Loop)%Name)

    DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
      CALL SetupOutputVariable('Zone Thermal Chimney Heat Loss Energy [J]',   &
                               ZnRptThermChim(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%ThermalChimneyHeatLoss,   &
                              'System','Sum',Zone(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%Name)
      CALL SetupOutputVariable('Zone Thermal Chimney Heat Gain Energy [J]',   &
                               ZnRptThermChim(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%ThermalChimneyHeatGain,   &
                              'System','Sum',Zone(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%Name)
      CALL SetupOutputVariable('Zone Thermal Chimney Volume [m3]',   &
                               ZnRptThermChim(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%ThermalChimneyVolume,   &
                              'System','Sum',Zone(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%Name)
      CALL SetupOutputVariable('Zone Thermal Chimney Mass [kg]',   &
                               ZnRptThermChim(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%ThermalChimneyMass,   &
                              'System','Sum',Zone(ThermalChimneySys(Loop)%ZonePtr(TCZoneNum))%Name)
    END DO     ! DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
  END DO     ! DO Loop=1, TotThermalChimney


  !! LKL-more renaming effort and code review might be possible here
          ! Check to make sure there is only one thermal chimney statement per zone
  DO Loop = 1, TotThermalChimney
     IF (ThermalChimneySys(Loop)%TotZoneToDistrib > 1) THEN
        DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib

          IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN
            DO TCZoneNum1 = TCZoneNum+1, ThermalChimneySys(Loop)%TotZoneToDistrib
              IF ( ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop)%ZonePtr(TCZoneNum1) ) THEN
                CALL ShowSevereError('Only one ZoneThermalChimney object allowed per zone but zone '  &
                                     //TRIM(ThermalChimneySys(Loop)%ZoneName(TCZoneNum))// &
                                     ' has two ZoneThermalChimney objects associated with it')
                     ErrorsFound = .TRUE.
              END IF
            END DO
            DO TCZoneNum1 = 1, TCZoneNum-1
              IF ( ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop)%ZonePtr(TCZoneNum1) ) THEN
                CALL ShowSevereError('Only one ZoneThermalChimney object allowed per zone but zone '  &
                                     //TRIM(ThermalChimneySys(Loop)%ZoneName(TCZoneNum))// &
                                     ' has two ZoneThermalChimney objects associated with it')
                     ErrorsFound = .TRUE.
              END IF
            END DO
          ELSE     ! IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN
            DO TCZoneNum1 = 1, TCZoneNum-1
              IF ( ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop)%ZonePtr(TCZoneNum1) ) THEN
                CALL ShowSevereError('Only one ZoneThermalChimney object allowed per zone but zone '  &
                                     //TRIM(ThermalChimneySys(Loop)%ZoneName(TCZoneNum))// &
                                     ' has two ZoneThermalChimney objects associated with it')
                     ErrorsFound = .TRUE.
              END IF
            END DO
          END IF     ! IF ( ThermalChimneySys(Loop)%TotZoneToDistrib >= (TCZoneNum+1) ) THEN

        END DO     ! DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
     END IF     ! IF (ThermalChimneySys(Loop)%TotZoneToDistrib > 1) THEN
  END DO     ! DO Loop = 1, TotThermalChimney


          ! Check to make sure there is only one thermal chimney statement per zone
 IF (TotThermalChimney > 1) THEN
  DO Loop = 1, TotThermalChimney

     IF ( TotThermalChimney >= (Loop+1) ) THEN
        DO Loop1 = Loop+1, TotThermalChimney
          DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
            DO TCZoneNum1 = 1, ThermalChimneySys(Loop1)%TotZoneToDistrib
              IF ( ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop1)%ZonePtr(TCZoneNum1) ) THEN
                 CALL ShowSevereError('Only one ZoneThermalChimney object allowed per zone but zone '  &
                                      //TRIM(ThermalChimneySys(Loop)%ZoneName(TCZoneNum))// &
                                      ' has two ZoneThermalChimney objects associated with it')
                      ErrorsFound = .TRUE.
              END IF
            END DO
          END DO
        END DO
        DO Loop1 = 1, Loop-1
          DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
            DO TCZoneNum1 = 1, ThermalChimneySys(Loop1)%TotZoneToDistrib
              IF ( ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop1)%ZonePtr(TCZoneNum1) ) THEN
                 CALL ShowSevereError('Only one ZoneThermalChimney object allowed per zone but zone '  &
                                      //TRIM(ThermalChimneySys(Loop)%ZoneName(TCZoneNum))// &
                                      ' has two ZoneThermalChimney objects associated with it')
                      ErrorsFound = .TRUE.
              END IF
            END DO
          END DO
        END DO
     ELSE     ! IF ( TotThermalChimney >= (Loop+1) ) THEN
        DO Loop1 = 1, Loop-1
          DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
            DO TCZoneNum1 = 1, ThermalChimneySys(Loop1)%TotZoneToDistrib
              IF ( ThermalChimneySys(Loop)%ZonePtr(TCZoneNum) == ThermalChimneySys(Loop1)%ZonePtr(TCZoneNum1) ) THEN
                 CALL ShowSevereError('Only one ZoneThermalChimney object allowed per zone but zone '  &
                                      //TRIM(ThermalChimneySys(Loop)%ZoneName(TCZoneNum))// &
                                      ' has two ZoneThermalChimney objects associated with it')
                      ErrorsFound = .TRUE.
              END IF
            END DO
          END DO
        END DO
     END IF     ! IF ( TotThermalChimney >= (Loop+1) ) THEN

  END DO     ! DO Loop = 1, TotThermalChimney
 END IF     ! IF (TotThermalChimney > 1) THEN


  IF (ErrorsFound) THEN
    CALL ShowFatalError(trim(cCurrentModuleObject)//' Errors found in input.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE GetThermalChimney


  SUBROUTINE CalcThermalChimney

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   April 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the components making up the ThermalChimney.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER     :: NTC=15                ! Number of subregions in thermal chimney air channel for FINITE DIFFERENCE

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        ! To be obtained from other modules and subroutines
  REAL(r64)                    :: SurfTempAbsorberWall                ! Absorber wall surface temperature (K)
  REAL(r64)                    :: SurfTempGlassCover                  ! Glass cover surface temperature (K)
  REAL(r64)                    :: ConvTransCoeffWallFluid             ! Absorber wall convection trasnfer coefficient
  REAL(r64)                    :: ConvTransCoeffGlassFluid            ! Glass cover convection trasnfer coefficient

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        ! Real local vaiables
  INTEGER   :: Loop            ! DO loop counter
  INTEGER   :: SurfNum            ! DO loop counter for surfaces
  INTEGER   :: ZoneNum            ! DO loop counter for zones
  INTEGER   :: TCZoneNumCounter
  INTEGER   :: TCZoneNum
  REAL(r64) :: minorW             ! width of enclosure (narrow dimension)
  REAL(r64) :: majorW             ! width of major surface
  REAL(r64) :: TempmajorW

  REAL(r64) :: RoomAirTemp
  REAL(r64) :: AirSpecHeatThermalChim              ! (J/kg*C) or (J/kg*K)
  REAL(r64) :: AbsorberWallWidthTC
  REAL(r64) :: TCVolumeAirFlowRate                 ! (m^3/s)
  REAL(r64) :: TCMassAirFlowRate                   ! (kg/s)
  REAL(r64) :: DischargeCoeffTC
  REAL(r64) :: AirOutletCrossAreaTC
  REAL(r64) :: AirInletCrossArea
  REAL(r64) :: AirRelativeCrossArea
  ! REAL(r64)                    :: OutletAirTempThermalChim
  REAL(r64) :: OverallThermalChimLength
  REAL(r64) :: ThermChimTolerance
  REAL(r64), DIMENSION(10) :: TempTCMassAirFlowRate                  ! Temporary Value of Thermal Chimney Mass Flow Rate ()
  REAL(r64), DIMENSION(10) :: TempTCVolumeAirFlowRate                ! Temporary Value of Thermal Chimney Volume Flow Rate ()
  INTEGER   :: IterationLoop
  REAL(r64) :: Process1                            ! Temporary Variable Used in the Middle of the Calculation
  REAL(r64) :: Process2                            ! Temporary Variable Used in the Middle of the Calculation
  REAL(r64) :: Process3                            ! Temporary Variable Used in the Middle of the Calculation
!unused1208  REAL(r64)   :: Process4                            ! Temporary Variable Used in the Middle of the Calculation
  REAL(r64) :: AirDensityThermalChim               ! (kg/m^3)
  REAL(r64) :: AirDensity                          ! (kg/m^3)
  REAL(r64) :: CpAir
  REAL(r64) :: TemporaryWallSurfTemp

  REAL(r64) :: DeltaL                              ! OverallThermalChimLength / NTC
  INTEGER   :: ThermChimLoop1
  INTEGER   :: ThermChimLoop2
  REAL(r64), DIMENSION(NTC, NTC) :: EquaCoef               ! Coefficients in Linear Algebraic Euqation for FINITE DIFFERENCE
  REAL(r64), DIMENSION(NTC)     :: EquaConst               ! Constants in Linear Algebraic Equation for FINITE DIFFERENCE
  REAL(r64), DIMENSION(NTC)     :: ThermChimSubTemp        ! Air temperature of each thermal chimney air channel subregion


DO Loop=1, TotThermalChimney


  ZoneNum = ThermalChimneySys(Loop)%RealZonePtr
  ! start off with first surface in zone widths
  majorW = Surface(Zone(ZoneNum)%SurfaceFirst)%Width
  minorW = majorW
  TempmajorW = 0.0d0
  TemporaryWallSurfTemp = -10000.0d0


  ! determine major width and minor width
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst+1,Zone(ZoneNum)%SurfaceLast
    IF (Surface(SurfNum)%Class .NE. SurfaceClass_Wall) CYCLE

    IF (Surface(SurfNum)%Width > majorW) THEN
      majorW = Surface(SurfNum)%Width
    END IF

    IF (Surface(SurfNum)%Width < minorW) THEN
      minorW = Surface(SurfNum)%Width
    END IF
  END DO

  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (Surface(SurfNum)%Width == majorW) THEN
       IF (TempSurfIn(SurfNum) > TemporaryWallSurfTemp) THEN
         TemporaryWallSurfTemp = TempSurfIn(SurfNum)
         ConvTransCoeffWallFluid = HConvIn(SurfNum)
         SurfTempAbsorberWall = TempSurfIn(SurfNum) + KelvinConv
       END IF
    END IF
  END DO


  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast

     IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces

     IF (Surface(SurfNum)%Class == SurfaceClass_Window) THEN

       IF (Surface(SurfNum)%Width > TempmajorW) THEN
         TempmajorW = Surface(SurfNum)%Width
         ConvTransCoeffGlassFluid = HConvIn(SurfNum)
         SurfTempGlassCover = TempSurfIn(SurfNum) + KelvinConv
       END IF

     END IF

  END DO

  AbsorberWallWidthTC = majorW
  IF (ThermalChimneySys(Loop)%AbsorberWallWidth /= majorW) THEN
    AbsorberWallWidthTC = ThermalChimneySys(Loop)%AbsorberWallWidth
  END IF

  AirDensityThermalChim          = PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))
  AirSpecHeatThermalChim         = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MAT(ZoneNum))
  AirOutletCrossAreaTC           = ThermalChimneySys(Loop)%AirOutletCrossArea
  DischargeCoeffTC               = ThermalChimneySys(Loop)%DischargeCoeff

  AirInletCrossArea = 0.0d0
  DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
     AirInletCrossArea = AirInletCrossArea + ThermalChimneySys(Loop)%EachAirInletCrossArea(TCZoneNum)
  END DO

  RoomAirTemp = 0.0d0
  DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
     TCZoneNumCounter = ThermalChimneySys(Loop)%ZonePtr(TCZoneNum)
     RoomAirTemp = RoomAirTemp + ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum) * MAT(TCZoneNumCounter)
  END DO
  RoomAirTemp = RoomAirTemp + KelvinConv

  Process1 = 0.0d0
  Process2 = 0.0d0
  DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
     TCZoneNumCounter = ThermalChimneySys(Loop)%ZonePtr(TCZoneNum)
     Process1 = Process1 + PsyHFnTdbW(MAT(TCZoneNumCounter),ZoneAirHumRat(TCZoneNumCounter)) *   &
           ThermalChimneySys(Loop)%DistanceThermChimInlet(TCZoneNum) * ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum)
     Process2 = Process2 + ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum) *   &
             PsyHFnTdbW(MAT(TCZoneNumCounter),ZoneAirHumRat(TCZoneNumCounter))
  END DO
  OverallThermalChimLength = Process1 / Process2

  DeltaL = OverallThermalChimLength / NTC


 ! Starting the iteration for mass and volumetric flow rate calculation
 ThermChimTolerance = 10000000.0d0      ! An impossibly big tolerance
 DO IterationLoop = 1,10

  IF (IterationLoop == 1) THEN
    TempTCMassAirFlowRate(IterationLoop) = 0.05d0     ! Inital Guess

  ELSE
    TempTCMassAirFlowRate(IterationLoop) = TempTCVolumeAirFlowRate(IterationLoop-1) * AirDensityThermalChim

      IF (ABS( TempTCMassAirFlowRate(IterationLoop)-TempTCMassAirFlowRate(IterationLoop-1) ) < ThermChimTolerance) THEN
         ThermChimTolerance = ABS( TempTCMassAirFlowRate(IterationLoop)-TempTCMassAirFlowRate(IterationLoop-1) )
         TCMassAirFlowRate = TempTCMassAirFlowRate(IterationLoop)
         TCVolumeAirFlowRate = TempTCVolumeAirFlowRate(IterationLoop)
      END IF

  END IF     ! IF (IterationLoop == 1) THEN


   ! Calculation of Thermal Chimney Discharge Air Temperature
   Process1= AbsorberWallWidthTC*DeltaL*ConvTransCoeffGlassFluid + AbsorberWallWidthTC*DeltaL*ConvTransCoeffWallFluid -   &
             2.d0*TempTCMassAirFlowRate(IterationLoop)*AirSpecHeatThermalChim
   Process2= AbsorberWallWidthTC*DeltaL*ConvTransCoeffGlassFluid + AbsorberWallWidthTC*DeltaL*ConvTransCoeffWallFluid +   &
             2.d0*TempTCMassAirFlowRate(IterationLoop)*AirSpecHeatThermalChim
   Process3= 2.d0*AbsorberWallWidthTC*DeltaL*ConvTransCoeffGlassFluid*SurfTempGlassCover  &
             + 2.d0*AbsorberWallWidthTC*DeltaL*ConvTransCoeffWallFluid*SurfTempAbsorberWall

   DO ThermChimLoop1=1, NTC
    DO ThermChimLoop2=1, NTC
     EquaCoef(ThermChimLoop1, ThermChimLoop2)=0.0d0
    END DO
   END DO

   EquaCoef(1,1)=Process2
   EquaConst(1)=Process3-Process1*RoomAirTemp
   DO ThermChimLoop1=2, NTC
    EquaCoef(ThermChimLoop1, (ThermChimLoop1-1))=Process1
    EquaCoef(ThermChimLoop1, ThermChimLoop1)=Process2
    EquaConst(ThermChimLoop1)=PRocess3
   END DO

   CALL GaussElimination(EquaCoef, EquaConst, ThermChimSubTemp, NTC)

    AirRelativeCrossArea = AirOutletCrossAreaTC / AirInletCrossArea
    IF (ThermChimSubTemp(NTC) <= RoomAirTemp) THEN
       TempTCVolumeAirFlowRate(IterationLoop) = 0.0d0
    ELSE
       TempTCVolumeAirFlowRate(IterationLoop) = DischargeCoeffTC * AirOutletCrossAreaTC *    &
                            ((  2.d0*((ThermChimSubTemp(NTC)-RoomAirTemp)/RoomAirTemp)  &
                            * 9.8d0 * OverallThermalChimLength / ((1.d0+AirRelativeCrossArea)**2)  )**0.5d0)
    END IF

 END DO     ! DO IterationLoop = 1,10


   ! Calculation of Thermal Chimney Discharge Temperature
   Process1= AbsorberWallWidthTC*DeltaL*ConvTransCoeffGlassFluid + AbsorberWallWidthTC*DeltaL*ConvTransCoeffWallFluid -   &
             2.d0*TCMassAirFlowRate*AirSpecHeatThermalChim
   Process2= AbsorberWallWidthTC*DeltaL*ConvTransCoeffGlassFluid + AbsorberWallWidthTC*DeltaL*ConvTransCoeffWallFluid +   &
             2.d0*TCMassAirFlowRate*AirSpecHeatThermalChim
   Process3= 2.d0*AbsorberWallWidthTC*DeltaL*ConvTransCoeffGlassFluid*SurfTempGlassCover  &
             + 2.d0*AbsorberWallWidthTC*DeltaL*ConvTransCoeffWallFluid*SurfTempAbsorberWall

   DO ThermChimLoop1=1, NTC
    DO ThermChimLoop2=1, NTC
     EquaCoef(ThermChimLoop1, ThermChimLoop2)=0.0d0
    END DO
   END DO

   EquaCoef(1,1)=Process2
   EquaConst(1)=Process3-Process1*RoomAirTemp
   DO ThermChimLoop1=2, NTC
    EquaCoef(ThermChimLoop1, (ThermChimLoop1-1))=Process1
    EquaCoef(ThermChimLoop1, ThermChimLoop1)=Process2
    EquaConst(ThermChimLoop1)=PRocess3
   END DO

   CALL GaussElimination(EquaCoef, EquaConst, ThermChimSubTemp, NTC)

    AirRelativeCrossArea = AirOutletCrossAreaTC / AirInletCrossArea
    IF (ThermChimSubTemp(NTC) <= RoomAirTemp) THEN
       TCVolumeAirFlowRate = 0.0d0
    ELSE
       TCVolumeAirFlowRate = DischargeCoeffTC * AirOutletCrossAreaTC *    &
                            ((  2.d0*((ThermChimSubTemp(NTC)-RoomAirTemp)/RoomAirTemp)  &
                            * 9.8d0 * OverallThermalChimLength / ((1.d0+AirRelativeCrossArea)**2)  )**0.5d0)
    END IF

   ! Now assignment of the overall mass flow rate into each zone
  DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
     TCZoneNumCounter = ThermalChimneySys(Loop)%ZonePtr(TCZoneNum)
     AirDensity       = PsyRhoAirFnPbTdbW(OutBaroPress,MAT(TCZoneNumCounter),ZoneAirHumRat(TCZoneNumCounter))
     CpAir            = PsyCpAirFnWTdb(ZoneAirHumRat(TCZoneNumCounter),MAT(TCZoneNumCounter))
     MCPThermChim(TCZoneNumCounter) = TCVolumeAirFlowRate * AirDensity * CpAir *   &
                                      ThermalChimneySys(Loop)%RatioThermChimAirFlow(TCZoneNum)
     IF (MCPThermChim(TCZoneNumCounter) <= 0.0d0) THEN
        MCPThermChim(TCZoneNumCounter) = 0.0d0
     END IF
     ThermChimAMFL(TCZoneNumCounter) = MCPThermChim(TCZoneNumCounter) / CpAir
     MCPTThermChim(TCZoneNumCounter) = MCPThermChim(TCZoneNumCounter) * Zone(TCZoneNumCounter)%OutDryBulbTemp
  END DO

  MCPThermChim(ZoneNum) = TCVolumeAirFlowRate * AirDensity * CpAir
  IF (MCPThermChim(ZoneNum) <= 0.0d0) THEN
     MCPThermChim(ZoneNum) = 0.0d0
  END IF
  ThermChimAMFL(ZoneNum) = MCPThermChim(ZoneNum) / CpAir
  MCPTThermChim(ZoneNum) = MCPThermChim(ZoneNum) * Zone(ZoneNum)%OutDryBulbTemp

  ThermalChimneyReport(Loop)%OverallTCVolumeFlow = TCVolumeAirFlowRate
  ThermalChimneyReport(Loop)%OverallTCMassFlow = TCMassAirFlowRate
     IF (  ThermalChimneyReport(Loop)%OverallTCMassFlow /= (TCVolumeAirFlowRate*AirDensityThermalChim)  ) THEN
        ThermalChimneyReport(Loop)%OverallTCMassFlow = ThermalChimneyReport(Loop)%OverallTCVolumeFlow * AirDensityThermalChim
     END IF
  ThermalChimneyReport(Loop)%OutletAirTempThermalChim = ThermChimSubTemp(NTC) - KelvinConv

  IF (GetCurrentScheduleValue(ThermalChimneySys(Loop)%SchedPtr) <= 0.0d0) THEN
     DO TCZoneNum = 1, ThermalChimneySys(Loop)%TotZoneToDistrib
        TCZoneNumCounter = ThermalChimneySys(Loop)%ZonePtr(TCZoneNum)
        MCPThermChim(TCZoneNumCounter) = 0.0d0
        ThermChimAMFL(TCZoneNumCounter) = 0.0d0
        MCPTThermChim(TCZoneNumCounter) = 0.0d0
     END DO
     MCPThermChim(ZoneNum) = 0.0d0
     ThermChimAMFL(ZoneNum) = 0.0d0
     MCPTThermChim(ZoneNum) = 0.0d0
     ThermalChimneyReport(Loop)%OverallTCVolumeFlow = 0.0d0
     ThermalChimneyReport(Loop)%OverallTCMassFlow = 0.0d0
     ThermalChimneyReport(Loop)%OutletAirTempThermalChim = MAT(ZoneNum)
  END IF

END DO     ! DO Loop=1, TotThermalChimney

RETURN

END SUBROUTINE CalcThermalChimney


SUBROUTINE ReportThermalChimney

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   April 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine fills remaining report variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
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
  INTEGER  :: ZoneLoop                ! Counter for the # of zones (nz)
  REAL(r64)  ::  AirDensity
  REAL(r64)  :: CpAir
  REAL(r64)  :: TSMult

  TSMult=TimeStepSys*SecInHour

  DO ZoneLoop = 1, NumOfZones   ! Start of zone loads report variable update loop ...

          ! Break the infiltration load into heat gain and loss components.
    AirDensity                     = PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneLoop),ZoneAirHumRat(ZoneLoop))
    CpAir                          = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneLoop),MAT(ZoneLoop))
    ZnRptThermChim(ZoneLoop)%ThermalChimneyVolume   = (MCPThermChim(ZoneLoop)/CpAir/AirDensity)*TSMult
    ZnRptThermChim(ZoneLoop)%ThermalChimneyMass     = (MCPThermChim(ZoneLoop)/CpAir)*TSMult

    ZnRptThermChim(ZoneLoop)%ThermalChimneyHeatLoss = 0.0d0
    ZnRptThermChim(ZoneLoop)%ThermalChimneyHeatGain = 0.0d0

      IF (ZT(ZoneLoop) > Zone(ZoneLoop)%OutDryBulbTemp) THEN

        ZnRptThermChim(ZoneLoop)%ThermalChimneyHeatLoss = MCPThermChim(ZoneLoop)*(ZT(ZoneLoop)-Zone(ZoneLoop)%OutDryBulbTemp)* &
                                         TSMult
        ZnRptThermChim(ZoneLoop)%ThermalChimneyHeatGain=0.0d0

      ELSE IF (ZT(ZoneLoop) <= Zone(ZoneLoop)%OutDryBulbTemp) THEN

        ZnRptThermChim(ZoneLoop)%ThermalChimneyHeatGain = MCPThermChim(ZoneLoop)*(Zone(ZoneLoop)%OutDryBulbTemp-ZT(ZoneLoop))* &
                                         TSMult
        ZnRptThermChim(ZoneLoop)%ThermalChimneyHeatLoss =0.0d0

      END IF

  END DO     ! ... end of zone loads report variable update loop.


RETURN

END SUBROUTINE ReportThermalChimney


 SUBROUTINE GaussElimination(EquaCoef, EquaConst, ThermChimSubTemp, NTC)
          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sovles linear algebraic equations using Gauss Elimination Method.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

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

  INTEGER, INTENT(IN) :: NTC
  REAL(r64) , DIMENSION(NTC, NTC), INTENT(INOUT) :: EquaCoef
  REAL(r64) , DIMENSION(NTC), INTENT(INOUT) :: EquaConst
  REAL(r64) , DIMENSION(NTC), INTENT(OUT) :: ThermChimSubTemp

  REAL(r64) , DIMENSION(NTC) :: tempor
  REAL(r64)  :: tempb
  REAL(r64)  :: TCvalue
  REAL(r64)  :: TCcoefficient
  INTEGER :: PIVOT
  REAL(r64)  :: ThermalChimSum
  INTEGER                :: ThermChimLoop1
  INTEGER                :: ThermChimLoop2
  INTEGER                :: ThermChimLoop3


  DO ThermChimLoop1=1, NTC

   TCvalue=ABS(EquaCoef(ThermChimLoop1,ThermChimLoop1))
   pivot=ThermChimLoop1
   DO ThermChimLoop2=ThermChimLoop1+1, NTC
    IF (ABS(EquaCoef(ThermChimLoop2,ThermChimLoop1))>TCvalue) THEN
     TCvalue=ABS(EquaCoef(ThermChimLoop2,ThermChimLoop1))
     pivot=ThermChimLoop2
    END IF
   END DO


   IF (pivot /= ThermChimLoop1) THEN
    tempor(ThermChimLoop1:NTC)=EquaCoef(ThermChimLoop1, ThermChimLoop1:NTC)
    tempb=EquaConst(ThermChimLoop1)
    EquaCoef(ThermChimLoop1, ThermChimLoop1:NTC)=EquaCoef(pivot, ThermChimLoop1:NTC)
    EquaConst(ThermChimLoop1)=EquaConst(pivot)
    EquaCoef(pivot, ThermChimLoop1:NTC)=tempor(ThermChimLoop1:NTC)
    EquaConst(pivot)=tempb
   END IF

   DO ThermChimLoop2=ThermChimLoop1+1, NTC
    TCcoefficient=-EquaCoef(ThermChimLoop2,ThermChimLoop1)/EquaCoef(ThermChimLoop1,ThermChimLoop1)
    EquaCoef(ThermChimLoop2, ThermChimLoop1:NTC)=EquaCoef(ThermChimLoop2, ThermChimLoop1:NTC)   &
                                               +TCcoefficient*EquaCoef(ThermChimLoop1, ThermChimLoop1:NTC)
    EquaConst(ThermChimLoop2)=EquaConst(ThermChimLoop2)+TCcoefficient*EquaConst(ThermChimLoop1)
   END DO

  END DO

  ThermChimSubTemp(NTC)=EquaConst(NTC)/EquaCoef(NTC, NTC)
  DO ThermChimLoop2=NTC-1, 1, -1
   ThermalChimSum=0.0d0
   DO ThermChimLoop3=ThermChimLoop2+1, NTC
    ThermalChimSum=ThermalChimSum+EquaCoef(ThermChimLoop2, ThermChimLoop3)*ThermChimSubTemp(ThermChimLoop3)
   END DO
   ThermChimSubTemp(ThermChimLoop2)=(EquaConst(ThermChimLoop2)-ThermalChimSum)/EquaCoef(ThermChimLoop2,ThermChimLoop2)
  END DO

 END SUBROUTINE GaussElimination

!        End of Module Subroutines for ThermalChimney

!*****************************************************************************************
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

END MODULE ThermalChimney