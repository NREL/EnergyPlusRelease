MODULE EarthTube
  ! Module containing the data for Earth Tube system

  ! MODULE INFORMATION:
  !       AUTHOR         Kwang Ho Lee
  !       DATE WRITTEN   November 2005
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithyms required to manage the EarthTube System Component

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! 1. M. Krarti, "Analytical Model to Predict Annual Soil Surface Temperature Variation",
  ! Journal of Solar Energy Engineering 117, 1995, pp 91-99
  ! 2. K. Labs In: J. Cook, editor, "Passive Cooling",
  ! Cambridge Massachusetts, MIT Press, 1989, pp 206-212

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalFanSys
USE DataHeatBalance         ! This is the heat balance super block data module
USE DataSurfaces
USE DataInterfaces

          ! Use statements for access to subroutines in other modules
Use Psychrometrics

IMPLICIT NONE

! DERIVED TYPE DEFINITIONS
TYPE EarthTubeData
    INTEGER :: ZonePtr                        =0
    INTEGER :: SchedPtr                       =0
    CHARACTER(len=MaxNameLength) :: SchedName =' '
    REAL(r64) :: DesignLevel                       =0.0D0
    REAL(r64) :: MinTemperature                    =0.0D0
    REAL(r64) :: MaxTemperature                    =0.0D0
    REAL(r64) :: DelTemperature                    =0.0D0
    INTEGER :: FanType                             =0
    REAL(r64) :: FanPressure                       =0.0D0
    REAL(r64) :: FanEfficiency                     =0.0D0
    REAL(r64) :: FanPower                          =0.0D0
    REAL(r64) :: GroundTempz1z2t                   =0.0D0   ! ground temp between z1 and z2 at time t
    REAL(r64) :: InsideAirTemp                     =0.0D0
    REAL(r64) :: AirTemp                           =0.0D0
    REAL(r64) :: r1                                =0.0D0   ! Inner Pipe Radius (m)
    REAL(r64) :: r2                                =0.0D0   ! Pipe Thickness (m)
    REAL(r64) :: r3                                =0.0D0   ! Distance between Pipe Outer Surface and Undistubed Soil (m)
    REAL(r64) :: PipeLength                        =0.0D0   ! Entire Pipe Length
    REAL(r64) :: PipeThermCond                     =0.0D0   ! Pipe Thermal Conductivity
    REAL(r64) :: z                                 =0.0D0   ! Depth under the Ground Surface (m)
    REAL(r64) :: SoilThermDiff                     =0.0D0   ! Soil Thermal Diffusivity
    REAL(r64) :: SoilThermCond                     =0.0D0   ! Soil Thermal Conductivity
    REAL(r64) :: AverSoilSurTemp     ! Average Soil Surface Temperature
    REAL(r64) :: ApmlSoilSurTemp     ! Amplitude of Soil Surface Temperature
    INTEGER :: SoilSurPhaseConst     ! Phase constant of Soil Surface
    REAL(r64) :: ConstantTermCoef                  =0.0D0
    REAL(r64) :: TemperatureTermCoef               =0.0D0
    REAL(r64) :: VelocityTermCoef                  =0.0D0
    REAL(r64) :: VelocitySQTermCoef                =0.0D0
END TYPE EarthTubeData

Type EarthTubeZoneReportVars
  REAL(r64) :: EarthTubeHeatLoss      = 0.0D0 ! [J] Heat loss or cooling to zone from air delivered by earth tube
  REAL(r64) :: EarthTubeHeatLossRate  = 0.0D0 ! [W] Heat loss or cooling rate to zone from air delivered by earth tube
  REAL(r64) :: EarthTubeHeatGain      = 0.0D0 ! [J] Heat Gain to zone from air delivered by earth tube
  REAL(r64) :: EarthTubeHeatGainRate  = 0.0D0 ! [W] Heat Gain rate to zone from air delivered by earth tube
  REAL(r64) :: EarthTubeOATreatmentPower  = 0.0D0 ! [W] rate of heat transfer to/from air.  positive is heating OA to higher temp
  REAL(r64) :: EarthTubeVolume        =0.0D0 ! Volume of Air {m3} due to EarthTube
  REAL(r64) :: EarthTubeVolFlowRate   =0.0D0 ! Volume flow rate of air (m3/s) due to EarthTube
  REAL(r64) :: EarthTubeMass          =0.0D0 ! Mass of Air {kg} due to EarthTube
  REAL(r64) :: EarthTubeMassFlowRate  =0.0D0 ! Mass flow rate of air (kg/s) due to EarthTube
  REAL(r64) :: EarthTubeFanElec       =0.0D0 ! [J] Fan Electricity consumed by EarthTube
  REAL(r64) :: EarthTubeFanElecPower  =0.0D0 ! [W] Fan Electric power for EarthTube
  REAL(r64) :: EarthTubeAirTemp       =0.0D0 ! Air Temp {C} of EarthTube, air leaving tube and entering zone
END TYPE

! MODULE VARIABLES DECLARATIONS:
TYPE (EarthTubeData), ALLOCATABLE, DIMENSION(:) :: EarthTubeSys
TYPE (EarthTubeZoneReportVars), ALLOCATABLE, DIMENSION(:) :: ZnRptET
INTEGER :: TotEarthTube  =0 ! Total EarthTube Statements in input
          ! Parameters for Ventilation
INTEGER, PARAMETER :: NaturalEarthTube = 0
INTEGER, PARAMETER :: IntakeEarthTube  = 1
INTEGER, PARAMETER :: ExhaustEarthTube = 2

!         Subroutine Specifications for the Heat Balance Module
          ! Driver Routines
PUBLIC  ManageEarthTube

          ! Get Input routines for module
PRIVATE GetEarthTube

          ! Algorithms for the module
PRIVATE CalcEarthTube

          ! Reporting routines for module
PRIVATE ReportEarthTube


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE ManageEarthTube

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   November 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the simulation of EarthTube unit.
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
    CALL GetEarthTube(ErrorsFound)
    GetInputFlag=.false.
  ENDIF

  IF (TotEarthTube == 0) RETURN

  CALL CalcEarthTube

  CALL ReportEarthTube

  RETURN

END SUBROUTINE ManageEarthTube


SUBROUTINE GetEarthTube(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   November 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine obtains input data for EarthTube units and
          ! stores it in the EarthTube data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList,VerifyName
  USE ScheduleManager, ONLY: GetScheduleIndex,GetScheduleValuesForDay
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: Blank=' '
    REAL(r64),        PARAMETER :: EarthTubeTempLimit = 100.d0 ! degrees Celsius

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208    CHARACTER(len=MaxNameLength), DIMENSION(10) :: AlphaName
!unused1208    REAL(r64), DIMENSION(20)              :: IHGNumbers
    INTEGER                         :: NumAlpha, NumNumber
    INTEGER                         :: IOStat
    INTEGER                         :: Loop
    INTEGER                         :: Loop1
    LOGICAL, DIMENSION(:), ALLOCATABLE :: RepVarSet

  ALLOCATE(RepVarSet(NumOfZones))
  RepVarSet=.true.

! Following used for reporting
ALLOCATE (ZnRptET (NumOfZones))

cCurrentModuleObject='ZoneEarthtube'
TotEarthTube=GetNumObjectsFound(cCurrentModuleObject)

ALLOCATE (EarthTubeSys(TotEarthTube))

DO Loop=1, TotEarthTube
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

          ! First Alpha is Zone Name
    EarthTubeSys(Loop)%ZonePtr = FindIteminList(cAlphaArgs(1),Zone%Name,NumOfZones)
    IF (EarthTubeSys(Loop)%ZonePtr == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//' not found='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    ENDIF

          ! Second Alpha is Schedule Name
    EarthTubeSys(Loop)%SchedName = cAlphaArgs(2)
    EarthTubeSys(Loop)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
    IF (EarthTubeSys(Loop)%SchedPtr == 0) THEN
      IF (lAlphaFieldBlanks(2)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(2))//  &
           ' is required, missing for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           ' entered='//TRIM(cAlphaArgs(2))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

          ! Overall parameters and their limits
    EarthTubeSys(Loop)%DesignLevel    = rNumericArgs(1)

    EarthTubeSys(Loop)%MinTemperature = rNumericArgs(2)
    IF ((EarthTubeSys(Loop)%MinTemperature < -EarthTubeTempLimit) .OR. &
        (EarthTubeSys(Loop)%MinTemperature >  EarthTubeTempLimit))  THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ' must have a minimum temperature between -'//TRIM(RoundSigDigits(EarthTubeTempLimit,0))//  &
         'C and '//TRIM(RoundSigDigits(EarthTubeTempLimit,0))//'C')
      CALL ShowContinueError('Entered value='//TRIM(RoundSigDigits(EarthTubeSys(Loop)%MinTemperature,0)))
      ErrorsFound = .TRUE.
    END IF

    EarthTubeSys(Loop)%MaxTemperature = rNumericArgs(3)
    IF ((EarthTubeSys(Loop)%MaxTemperature < -EarthTubeTempLimit) .OR. &
        (EarthTubeSys(Loop)%MaxTemperature >  EarthTubeTempLimit))  THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ' must have a maximum temperature between -'//TRIM(RoundSigDigits(EarthTubeTempLimit,0))//   &
         'C and '//TRIM(RoundSigDigits(EarthTubeTempLimit,0))//'C')
      CALL ShowContinueError('Entered value='//TRIM(RoundSigDigits(EarthTubeSys(Loop)%MaxTemperature,0)))
      ErrorsFound = .TRUE.
    END IF

    EarthTubeSys(Loop)%DelTemperature = rNumericArgs(4)  !  3/12/03  Negative del temp now allowed COP

    SELECT CASE (cAlphaArgs(3))  ! Fan type character input-->convert to integer
      CASE ('EXHAUST')
        EarthTubeSys(Loop)%FanType = ExhaustEarthTube
      CASE ('INTAKE')
        EarthTubeSys(Loop)%FanType = IntakeEarthTube
      CASE ('NATURAL','NONE',Blank)
        EarthTubeSys(Loop)%FanType = NaturalEarthTube
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
           '='//TRIM(cAlphaArgs(1)) // &
           ', '//TRIM(cAlphaFieldNames(3))//' invalid='//TRIM(cAlphaArgs(3)))
        ErrorsFound = .TRUE.
    END SELECT

    EarthTubeSys(Loop)%FanPressure   = rNumericArgs(5)
    IF (EarthTubeSys(Loop)%FanPressure < 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(5))//' must be positive, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%FanPressure,2)))
      ErrorsFound = .TRUE.
    END IF

    EarthTubeSys(Loop)%FanEfficiency = rNumericArgs(6)
    IF ((EarthTubeSys(Loop)%FanEfficiency <= 0.0d0) .OR. &
        (EarthTubeSys(Loop)%FanEfficiency >  1.0d0)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(6))//' must be greater than zero and less than or equal to one, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%FanEfficiency,2)))
      ErrorsFound = .TRUE.
    END IF


    EarthTubeSys(Loop)%r1                 = rNumericArgs(7)
    IF (EarthTubeSys(Loop)%r1 <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(7))//' must be positive, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%r1,2)))
      ErrorsFound = .TRUE.
    END IF

    EarthTubeSys(Loop)%r2                 = rNumericArgs(8)
    IF (EarthTubeSys(Loop)%r2 <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(8))//' must be positive, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%r2,2)))
      ErrorsFound = .TRUE.
    END IF

    EarthTubeSys(Loop)%r3                 = 2.d0*EarthTubeSys(Loop)%r1

    EarthTubeSys(Loop)%PipeLength         = rNumericArgs(9)
    IF (EarthTubeSys(Loop)%PipeLength <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(9))//' must be positive, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%PipeLength,2)))
      ErrorsFound = .TRUE.
    END IF

    EarthTubeSys(Loop)%PipeThermCond      = rNumericArgs(10)
    IF (EarthTubeSys(Loop)%PipeThermCond <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(10))//' must be positive, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%PipeThermCond,2)))
      ErrorsFound = .TRUE.
    END IF


    EarthTubeSys(Loop)%z                  = rNumericArgs(11)
    IF (EarthTubeSys(Loop)%z <= 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(11))//' must be positive, entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%z,2)))
      ErrorsFound = .TRUE.
    END IF
    IF (EarthTubeSys(Loop)%z <= (EarthTubeSys(Loop)%r1+EarthTubeSys(Loop)%r2+EarthTubeSys(Loop)%r3)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)) // &
         ', '//TRIM(cNumericFieldNames(11))//' must be greater than 3*'//  &
         TRIM(cNumericFieldNames(7))//' + '//TRIM(cNumericFieldNames(8))//' entered value='//  &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%z,2))//' ref sum='// &
         TRIM(RoundSigDigits(EarthTubeSys(Loop)%r1+EarthTubeSys(Loop)%r2+EarthTubeSys(Loop)%r3,2)))
      ErrorsFound = .TRUE.
    END IF

    SELECT CASE (cAlphaArgs(4))   ! Soil type character input --> convert to number
      CASE ('HEAVYANDSATURATED')
        EarthTubeSys(Loop)%SoilThermDiff = 0.0781056d0
        EarthTubeSys(Loop)%SoilThermCond = 2.42d0
      CASE ('HEAVYANDDAMP')
        EarthTubeSys(Loop)%SoilThermDiff = 0.055728d0
        EarthTubeSys(Loop)%SoilThermCond = 1.3d0
      CASE ('HEAVYANDDRY')
        EarthTubeSys(Loop)%SoilThermDiff = 0.0445824d0
        EarthTubeSys(Loop)%SoilThermCond = 0.865d0
      CASE ('LIGHTANDDRY')
        EarthTubeSys(Loop)%SoilThermDiff = 0.024192d0
        EarthTubeSys(Loop)%SoilThermCond = 0.346d0
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaFieldNames(1))//  &
           '='//TRIM(cAlphaArgs(1)) // &
           ', '//TRIM(cAlphaFieldNames(4))//' invalid='//TRIM(cAlphaArgs(4)))
        ErrorsFound = .TRUE.
     END SELECT

    EarthTubeSys(Loop)%AverSoilSurTemp    = rNumericArgs(12)
    EarthTubeSys(Loop)%ApmlSoilSurTemp    = rNumericArgs(13)
    EarthTubeSys(Loop)%SoilSurPhaseConst  = INT(rNumericArgs(14))

          ! Override any user input for cases where natural ventilation is being used
    IF (EarthTubeSys(Loop)%FanType == NaturalEarthTube) THEN
      EarthTubeSys(Loop)%FanPressure   = 0.0d0
      EarthTubeSys(Loop)%FanEfficiency = 1.0d0
    END IF

    EarthTubeSys(Loop)%ConstantTermCoef    = rNumericArgs(15)
    EarthTubeSys(Loop)%TemperatureTermCoef = rNumericArgs(16)
    EarthTubeSys(Loop)%VelocityTermCoef    = rNumericArgs(17)
    EarthTubeSys(Loop)%VelocitySQTermCoef  = rNumericArgs(18)

    IF (EarthTubeSys(Loop)%ZonePtr > 0) THEN
      IF (RepVarSet(EarthTubeSys(Loop)%ZonePtr)) THEN
        RepVarSet(EarthTubeSys(Loop)%ZonePtr)=.false.
        CALL SetupOutputVariable('Earth Tube Zone Sensible Cooling Energy [J]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeHeatLoss,  &
          'System','NonState',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Zone Sensible Cooling Rate [W]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeHeatLossRate,  &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Zone Sensible Heating Energy [J]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeHeatGain,  &
           'System','NonState',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Zone Sensible Heating Rate [W]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeHeatGainRate,  &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Air Flow Volume [m3]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeVolume,  &
           'System','NonState',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Air Volume Flow Rate [m3/s]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeVolFlowRate, &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Air Flow Mass [kg]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeMass,  &
           'System','NonState',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Air Mass Flow Rate [kg/s]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeMassFlowRate, &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Fan Electric Energy [J]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeFanElec,  &
           'System','NonState',Zone(EarthTubeSys(Loop)%ZonePtr)%Name,  &
           ResourceTypeKey='Electricity',GroupKey='Building')
        CALL SetupOutputVariable('Earth Tube Fan Electric Power [W]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeFanElecPower,  &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Zone Inlet Air Temperature [C]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeAirTemp,  &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Ground Interface Temperature [C]',  &
           EarthTubeSys(Loop)%GroundTempz1z2t,  &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
        CALL SetupOutputVariable('Earth Tube Outdoor Air Heat Transfer Rate [W]',  &
           ZnRptET(EarthTubeSys(Loop)%ZonePtr)%EarthTubeOATreatmentPower,  &
           'System','State',Zone(EarthTubeSys(Loop)%ZonePtr)%Name)
      END IF
    END IF
  END DO

          ! Check to make sure there is only on ventilation statement per zone
  DO Loop = 1, TotEarthTube
    DO Loop1 = Loop+1, TotEarthTube-1
      IF (EarthTubeSys(Loop)%ZonePtr == EarthTubeSys(Loop1)%ZonePtr) THEN
        CALL ShowSevereError(TRIM(cAlphaArgs(1))//' is assigned to more than one '//TRIM(cCurrentModuleObject))
        CALL ShowContinueError('Only one such assignment is allowed.')
        ErrorsFound = .TRUE.
      END IF
    END DO
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(TRIM(cCurrentModuleObject)//': Errors getting input.  Program terminates.')
  ENDIF

END SUBROUTINE GetEarthTube


SUBROUTINE CalcEarthTube

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   November 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the components making up the EarthTube unit.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE ScheduleManager, ONLY: GetCurrentScheduleValue, GetScheduleIndex

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
INTEGER :: Loop, NZ
REAL(r64) :: Process1     ! Variable Used in the Middle of the Calculation
REAL(r64) :: GroundTempz1z2t     ! Average Ground Temperature between Depth z1 and z2 at time t

REAL(r64) :: AirThermCond     ! Thermal Conductivity of Air (W/mC)
REAL(r64) :: AirKinemVisco     ! Kinematic Viscosity of Air (m2/s)
REAL(r64) :: AirThermDiffus     ! Thermal Diffusivity of Air (m2/s)
REAL(r64) :: Re     ! Reynolds Number for Flow Inside Pipe
REAL(r64) :: Pr     ! Prandtl Number for Flow Inside Pipe
REAL(r64) :: Nu     ! Nusselt Number for Flow Inside Pipe
REAL(r64) :: fa     ! Friction Factor of Pipe
REAL(r64) :: PipeHeatTransCoef     ! Convective Heat Transfer Coefficient at Inner Pipe Surface
REAL(r64) :: Rc     ! Thermal Resistance due to Convection between Air and Pipe Inner Surface
REAL(r64) :: Rp     ! Thermal Resistance due to Conduction between Pipe Inner and Outer Surface
REAL(r64) :: Rs     ! Thermal Resistance due to Conduction between Pipe Outer Surface and Soil
REAL(r64) :: Rt     ! Total Thermal Resistance between Pipe Air and Soil
REAL(r64) :: OverallHeatTransCoef     ! Overall Heat Transfer Coefficient of Earth Tube
REAL(r64) :: AverPipeAirVel     ! Average Pipe Air Velocity (m/s)
REAL(r64) :: AirMassFlowRate     ! Actual Mass Flow Rate of Air inside Pipe
REAL(r64) :: AirSpecHeat     ! Specific Heat of Air
REAL(r64) :: AirDensity     ! Density of Air
REAL(r64) :: InsideEnthalpy
REAL(r64) :: OutletAirEnthalpy
REAL(r64) :: InsideDewPointTemp
REAL(r64) :: InsideHumRat
REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: EVF  ! DESIGN EARTHTUBE FLOW RATE (M**3/SEC)


   ! Allocate the EVF array
   IF (.NOT. ALLOCATED(EVF)) ALLOCATE(EVF(NumOfZones))

   EVF = 0.0d0
   MCPTE = 0.0d0
   MCPE = 0.0d0
   EAMFL = 0.0d0


DO Loop=1, TotEarthTube

 NZ = EarthTubeSys(Loop)%ZonePtr
 EarthTubeSys(Loop)%FanPower = 0.0d0
         ! Skip this if the zone is below the minimum temperature limit
 IF (MAT(NZ) < EarthTubeSys(Loop)%MinTemperature) CYCLE
         ! Skip this if the zone is above the maximum temperature limit
 IF (MAT(NZ) > EarthTubeSys(Loop)%MaxTemperature) CYCLE
         ! Skip if below the temperature difference limit
 IF (ABS(MAT(NZ)-OutDryBulbTemp) < EarthTubeSys(Loop)%DelTemperature) CYCLE

 AirDensity                    = PsyRhoAirFnPbTdbW(OutBaroPress,OutDryBulbTemp,OutHumRat)
 AirSpecHeat                   = PsyCpAirFnWTdb(OutHumRat,OutDryBulbTemp)
 EVF(NZ)  = EarthTubeSys(Loop)%DesignLevel*GetCurrentScheduleValue(EarthTubeSys(Loop)%SchedPtr)
 MCPE(NZ) = EVF(NZ)*AirDensity*AirSpecHeat*( EarthTubeSys(Loop)%ConstantTermCoef      &
           + ABS(OutDryBulbTemp-MAT(NZ))*EarthTubeSys(Loop)%TemperatureTermCoef &
           + WindSpeed*(EarthTubeSys(Loop)%VelocityTermCoef + WindSpeed*EarthTubeSys(Loop)%VelocitySQTermCoef) )

 EAMFL(NZ) = MCPE(NZ)/AirSpecHeat
 IF (EarthTubeSys(Loop)%FanEfficiency > 0.0d0) THEN
   EarthTubeSys(Loop)%FanPower = EAMFL(NZ)*EarthTubeSys(Loop)%FanPressure/(EarthTubeSys(Loop)%FanEfficiency*AirDensity)
 END IF

 AverPipeAirVel=EVF(NZ)/PI/(EarthTubeSys(Loop)%r1**2)
 AirMassFlowRate=EVF(NZ)*AirDensity

! Calculation of Average Ground Temperature between Depth z1 and z2 at time t
GroundTempz1z2t=EarthTubeSys(Loop)%AverSoilSurTemp-EarthTubeSys(Loop)%ApmlSoilSurTemp*   &
               exp(-EarthTubeSys(Loop)%z*SQRT(pi/365.0d0/EarthTubeSys(Loop)%SoilThermDiff))*   &
               cos(2.0d0*pi/365.0d0*(DayOfYear-EarthTubeSys(Loop)%SoilSurPhaseConst-EarthTubeSys(Loop)%z/2.0d0*   &
               SQRT(365.0d0/pi/EarthTubeSys(Loop)%SoilThermDiff)))
EarthTubeSys(Loop)%GroundTempz1z2t = GroundTempz1z2t

! Calculation of Convective Heat Transfer Coefficient at Inner Pipe Surface
AirThermCond=0.02442d0+0.6992d0*OutDryBulbTemp/10000.d0
AirKinemVisco=(0.1335d0+0.000925d0*OutDryBulbTemp)/10000.d0
AirThermDiffus=(0.0014d0*OutDryBulbTemp+0.1872d0)/10000.d0
Re=2.d0*EarthTubeSys(Loop)%r1*AverPipeAirVel/AirKinemVisco
Pr=AirKinemVisco/AirThermDiffus
IF (Re<=2300.d0) THEN
 Nu=3.66d0
ELSE IF (Re<=4000.d0) THEN
 fa=(1.58d0*LOG(Re)-3.28d0)**(-2.d0)
 Process1=(fa/2.d0)*(Re-1000.d0)*Pr/(1.d0+12.7d0*((fa/2.d0)**0.5d0)*(Pr**(2.d0/3.d0)-1.d0))
 Nu=(Process1-3.66d0)/(1700.d0)*Re+(4000.d0*3.66d0-2300.d0*Process1)/1700.d0
ELSE
 fa=(1.58d0*LOG(Re)-3.28d0)**(-2.d0)
 Nu=(fa/2.d0)*(Re-1000.d0)*Pr/(1.d0+12.7d0*((fa/2.d0)**0.5d0)*(Pr**(2.d0/3.d0)-1.d0))
END IF
PipeHeatTransCoef=Nu*AirThermCond/2.d0/EarthTubeSys(Loop)%r1


! Claculation of Thermal Resistance and Overall Heat Transger Coefficient
Rc=1.d0/2.d0/PI/EarthTubeSys(Loop)%r1/PipeHeatTransCoef
Rp=LOG((EarthTubeSys(Loop)%r1+EarthTubeSys(Loop)%r2)/EarthTubeSys(Loop)%r1)/2.d0/PI/  &
    EarthTubeSys(Loop)%PipeThermCond
Rs=LOG((EarthTubeSys(Loop)%r1+EarthTubeSys(Loop)%r2+EarthTubeSys(Loop)%r3)/  &
   (EarthTubeSys(Loop)%r1+EarthTubeSys(Loop)%r2))/2.d0/PI/EarthTubeSys(Loop)%SoilThermCond
Rt=Rc+Rp+Rs
OverallHeatTransCoef=1.d0/Rt

IF (AirMassFlowRate*AirSpecHeat == 0.0d0) THEN
 EarthTubeSys(Loop)%InsideAirTemp=GroundTempz1z2t

ELSE

!Calculation of Pipe Outlet Air Temperature
IF (OutDryBulbTemp>GroundTempz1z2t) THEN
  Process1=(LOG(ABS(OutDryBulbTemp-GroundTempz1z2t))*AirMassFlowRate*AirSpecHeat-OverallHeatTransCoef*  &
           EarthTubeSys(Loop)%PipeLength)/(AirMassFlowRate*AirSpecHeat)
  EarthTubeSys(Loop)%InsideAirTemp=EXP(Process1)+GroundTempz1z2t
ELSE IF (OutDryBulbTemp==GroundTempz1z2t) THEN
  EarthTubeSys(Loop)%InsideAirTemp=GroundTempz1z2t
ELSE
  Process1=(LOG(ABS(OutDryBulbTemp-GroundTempz1z2t))*AirMassFlowRate*AirSpecHeat-OverallHeatTransCoef*  &
           EarthTubeSys(Loop)%PipeLength)/(AirMassFlowRate*AirSpecHeat)
  EarthTubeSys(Loop)%InsideAirTemp=GroundTempz1z2t-EXP(Process1)
END IF

END IF

InsideDewPointTemp = PsyTdpFnWPb(OutHumRat, OutBaroPress)

IF (EarthTubeSys(Loop)%InsideAirTemp>=InsideDewPointTemp) THEN
 InsideEnthalpy  = PsyHFnTdbW(EarthTubeSys(Loop)%InsideAirTemp,OutHumRat)
      ! Intake fans will add some heat to the air, raising the temperature for an intake fan...
 IF (EarthTubeSys(Loop)%FanType == IntakeEarthTube) THEN
   IF (EAMFL(NZ) == 0.0d0) Then
      OutletAirEnthalpy         = InsideEnthalpy
   ELSE
      OutletAirEnthalpy         = InsideEnthalpy + EarthTubeSys(Loop)%FanPower/EAMFL(NZ)
   END IF
   EarthTubeSys(Loop)%AirTemp = PsyTdbFnHW(OutletAirEnthalpy,OutHumRat)
 ELSE
   EarthTubeSys(Loop)%AirTemp = EarthTubeSys(Loop)%InsideAirTemp
 END IF
 MCPTE(NZ) = MCPE(NZ)*EarthTubeSys(Loop)%AirTemp

ELSE
 InsideHumRat = PsyWFnTdpPb (EarthTubeSys(Loop)%InsideAirTemp, OutBaroPress)
 InsideEnthalpy  = PsyHFnTdbW(EarthTubeSys(Loop)%InsideAirTemp,InsideHumRat)
      ! Intake fans will add some heat to the air, raising the temperature for an intake fan...
 IF (EarthTubeSys(Loop)%FanType == IntakeEarthTube) THEN
   IF (EAMFL(NZ) == 0.0d0) Then
      OutletAirEnthalpy         = InsideEnthalpy
   ELSE
      OutletAirEnthalpy         = InsideEnthalpy + EarthTubeSys(Loop)%FanPower/EAMFL(NZ)
   END IF
   EarthTubeSys(Loop)%AirTemp = PsyTdbFnHW(OutletAirEnthalpy,InsideHumRat)
 ELSE
   EarthTubeSys(Loop)%AirTemp = EarthTubeSys(Loop)%InsideAirTemp
 END IF
 MCPTE(NZ) = MCPE(NZ)*EarthTubeSys(Loop)%AirTemp
END IF

END DO

RETURN

END SUBROUTINE CalcEarthTube



SUBROUTINE ReportEarthTube

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   November 2005
          !       MODIFIED       B. Griffith April 2010 added output reports
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
  INTEGER  :: EarthTubeNum           ! Counter for EarthTube statements
  REAL(r64) ::  AirDensity
  REAL(r64) :: CpAir
  REAL(r64) :: ReportingConstant   ! reporting constant for this module

  ReportingConstant = TimeStepSys*SecInHour

  DO ZoneLoop = 1, NumOfZones   ! Start of zone loads report variable update loop ...

          ! Break the infiltration load into heat gain and loss components.
    AirDensity                     = PsyRhoAirFnPbTdbW(OutBaroPress,OutDryBulbTemp,OutHumRat)
    CpAir                          = PsyCpAirFnWTdb(OutHumRat,OutDryBulbTemp)
    ZnRptET(ZoneLoop)%EarthTubeVolume   = (MCPE(ZoneLoop)/CpAir/AirDensity)*ReportingConstant
    ZnRptET(ZoneLoop)%EarthTubeMass     = (MCPE(ZoneLoop)/CpAir)*ReportingConstant
    ZnRptET(ZoneLoop)%EarthTubeVolFlowRate  = MCPE(ZoneLoop)/CpAir/AirDensity
    ZnRptET(ZoneLoop)%EarthTubeMassFlowRate = MCPE(ZoneLoop)/CpAir

    ZnRptET(ZoneLoop)%EarthTubeFanElec = 0.0d0
    ZnRptET(ZoneLoop)%EarthTubeAirTemp = 0.0d0
    DO EarthTubeNum = 1, TotEarthTube
      IF (EarthTubeSys(EarthTubeNum)%ZonePtr == ZoneLoop) THEN
        ZnRptET(ZoneLoop)%EarthTubeFanElec       = EarthTubeSys(EarthTubeNum)%FanPower*ReportingConstant
        ZnRptET(ZoneLoop)%EarthTubeFanElecPower  = EarthTubeSys(EarthTubeNum)%FanPower

            ! Break the EarthTube load into heat gain and loss components.

        IF (ZT(ZoneLoop) > EarthTubeSys(EarthTubeNum)%AirTemp) THEN

          ZnRptET(ZoneLoop)%EarthTubeHeatLoss     = MCPE(ZoneLoop)*(ZT(ZoneLoop)-EarthTubeSys(EarthTubeNum)%AirTemp)* &
                                                       ReportingConstant
          ZnRptET(ZoneLoop)%EarthTubeHeatLossRate = MCPE(ZoneLoop)*(ZT(ZoneLoop)-EarthTubeSys(EarthTubeNum)%AirTemp)
          ZnRptET(ZoneLoop)%EarthTubeHeatGain     = 0.0D0
          ZnRptET(ZoneLoop)%EarthTubeHeatGainRate = 0.0D0

        ELSE IF (ZT(ZoneLoop) <= EarthTubeSys(EarthTubeNum)%AirTemp) THEN

          ZnRptET(ZoneLoop)%EarthTubeHeatGain     = MCPE(ZoneLoop)*(EarthTubeSys(EarthTubeNum)%AirTemp-ZT(ZoneLoop))* &
                                                       ReportingConstant
          ZnRptET(ZoneLoop)%EarthTubeHeatGainRate = MCPE(ZoneLoop)*(EarthTubeSys(EarthTubeNum)%AirTemp-ZT(ZoneLoop))
          ZnRptET(ZoneLoop)%EarthTubeHeatLoss     = 0.0D0
          ZnRptET(ZoneLoop)%EarthTubeHeatLossRate = 0.0D0

        END IF

        ZnRptET(ZoneLoop)%EarthTubeAirTemp  = EarthTubeSys(EarthTubeNum)%AirTemp
        ZnRptET(ZoneLoop)%EarthTubeOATreatmentPower =  MCPE(ZoneLoop)*(EarthTubeSys(EarthTubeNum)%AirTemp - OutDryBulbTemp)
        EXIT ! DO loop
      END IF
    END DO

  END DO    ! ... end of zone loads report variable update loop.


RETURN

END SUBROUTINE ReportEarthTube

!        End of Module Subroutines for EarthTube

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

END MODULE EarthTube