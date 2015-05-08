! (ref: Object: Generator:WindTurbine)
MODULE WindTurbine
  ! Module containing the data for wind turbine system

  ! MODULE INFORMATION:
  !       AUTHOR         Daeho Kang
  !       DATE WRITTEN   October 2009
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module is to calculate the electrical power output that wind turbine systems produce.
  ! Both horizontal and vertical axis wind turbine systems are modeled.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! Sathyajith Mathew. 2006. Wind Energy: Fundamental, Resource Analysis and Economics. Springer,
  !     Chap. 2, pp. 11-15
  ! Mazharul Islam, David S.K. Ting, and Amir Fartaj. 2008. Aerodynamic Models for Darrieus-type sSraight-bladed
  !     Vertical Axis Wind Turbines. Renewable & Sustainable Energy Reviews, Volume 12, pp.1087-1109

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGenerators
USE DataGlobals,   ONLY : MaxNameLength, Pi, SecInHour, BeginEnvrnFlag, DegToRadians, ScheduleAlwaysOn
USE DataInterfaces,   ONLY : ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError, SetupOutputVariable

IMPLICIT NONE

  ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: HAWT = 1 ! 'HorizontalAxisWindTurbine'
INTEGER, PARAMETER :: VAWT = 2 ! 'VerticalAxisWindTurbine'

INTEGER, PARAMETER :: FSFP = 1 ! 'FixedSpeedFixedPitch'
INTEGER, PARAMETER :: FSVP = 2 ! 'FixedSpeedVariablePitch'
INTEGER, PARAMETER :: VSFP = 3 ! 'VariableSpeedFixedPitch'
INTEGER, PARAMETER :: VSVP = 4 ! 'VariableSpeedVariablePitch'

! DERIVED TYPE DEFINITIONS
TYPE WindTurbineParams
    CHARACTER(len=MaxNameLength) :: Name        =' '  ! The component name
    CHARACTER(len=MaxNameLength) :: Schedule    =' '  ! Available schedule
    INTEGER   :: RotorType           = 0   ! Rotor type (HAWT or VAWT)
    INTEGER   :: ControlType         = 0   ! Control type
    INTEGER   :: SchedPtr            = 0   ! Schedule
    INTEGER   :: NumOfBlade          = 0   ! Blade number
    REAL(r64) :: RatedRotorSpeed     = 0.0d0 ! Rated rotor speed in m/s
    REAL(r64) :: RotorDiameter       = 0.0d0 ! Diameter of rotor in m
    REAL(r64) :: RotorHeight         = 0.0d0 ! Overall height of the rotor in m
    REAL(r64) :: RatedPower          = 0.0d0 ! Nominal average power outpout at the rated wind speed in Watts
    REAL(r64) :: RatedWindSpeed      = 0.0d0 ! Rated wind speed showing maximum power output in Watts
    REAL(r64) :: CutInSpeed          = 0.0d0 ! Minimum wind speed for system operation in m/s
    REAL(r64) :: CutOutSpeed         = 0.0d0 ! Maximum wind speed for system operation in m/s
    REAL(r64) :: SysEfficiency       = 0.0d0 ! Overall system efficiency including subsystems and losses
    REAL(r64) :: MaxTipSpeedRatio    = 0.0d0 ! Maximum tip speed ratio
    REAL(r64) :: MaxPowerCoeff       = 0.0d0 ! Maximum power coefficient
    REAL(r64) :: LocalAnnualAvgWS    = 0.0d0 ! Annual average wind speed locally measured in m/s
    REAL(r64) :: AnnualTMYWS         = 0.0d0 ! Annual average wind speed from stat file in m/s
    REAL(r64) :: HeightForLocalWS    = 0.0d0 ! Height of the local station in m
    REAL(r64) :: ChordArea           = 0.0d0 ! Chord area of a single blade for VAWTs in m2
    REAL(r64) :: DragCoeff           = 0.0d0 ! Empirical blade drag coefficient for VAWTs
    REAL(r64) :: LiftCoeff           = 0.0d0 ! Empirical blade lift coefficient for VAWTs
    REAL(r64) :: PowerCoeffC1        = 0.0d0 ! Empirical power coefficient 1 for analytical calculation
    REAL(r64) :: PowerCoeffC2        = 0.0d0 ! Empirical power coefficient 2 for analytical calculation
    REAL(r64) :: PowerCoeffC3        = 0.0d0 ! Empirical power coefficient 3 for analytical calculation
    REAL(r64) :: PowerCoeffC4        = 0.0d0 ! Empirical power coefficient 4 for analytical calculation
    REAL(r64) :: PowerCoeffC5        = 0.0d0 ! Empirical power coefficient 5 for analytical calculation
    REAL(r64) :: PowerCoeffC6        = 0.0d0 ! Empirical power coefficient 6 for analytical calculation
    REAL(r64) :: TotPower            = 0.0d0 ! Maximum power produced from the wind in Watts
    REAL(r64) :: Power               = 0.0d0 ! Actual power wind turbine supplies to the building in Watts
    REAL(r64) :: TotEnergy           = 0.0d0 ! Maximum energy produced from the wind in Joules
    REAL(r64) :: Energy              = 0.0d0 ! Actual energy wind turbine supplies to the building in Joules
    REAL(r64) :: LocalWindSpeed      = 0.0d0 ! Local wind speed estimated at the particular height in m/s
    REAL(r64) :: LocalAirDensity     = 0.0d0 ! Local air density estimated at the particular height kg/m3
    REAL(r64) :: PowerCoeff          = 0.0d0 ! Power coefficient determined
    REAL(r64) :: ChordalVel          = 0.0d0 ! Chordal velocity for VAWTs in m/s
    REAL(r64) :: NormalVel           = 0.0d0 ! Normal velocity for VAWTs in m/s
    REAL(r64) :: RelFlowVel          = 0.0d0 ! Relative flow velocity for VAWTs in m/s
    REAL(r64) :: TipSpeedRatio       = 0.0d0 ! Relative flow velocity for VAWTs in m/s
    REAL(r64) :: WSFactor            = 0.0d0 ! Relative flow velocity for VAWTs in m/s
    REAL(r64) :: AngOfAttack         = 0.0d0 ! Angle of attack in degree
    REAL(r64) :: IntRelFlowVel       = 0.0d0 ! Integral of relative flow velocity
    REAL(r64) :: TanForce            = 0.0d0 ! Tnagential force
    REAL(r64) :: NorForce            = 0.0d0 ! Normal force in N.m
    REAL(r64) :: TotTorque           = 0.0d0 ! Total torque in N.m
    REAL(r64) :: AzimuthAng          = 0.0d0 ! Azimuth angle between blades
END TYPE WindTurbineParams

! MODULE VARIABLES DECLARATIONS:
TYPE (WindTurbineParams), ALLOCATABLE, DIMENSION(:) :: WindTurbineSys
INTEGER :: NumWindTurbines = 0      ! Total wind turbine statements in inputs

! Subroutine Specifications for the Heat Balance Module
PUBLIC  SimWindTurbine
PUBLIC  GetWTGeneratorResults
PRIVATE GetWindTurbineInput
PRIVATE InitWindTurbine
PRIVATE CalcWindTurbine
PRIVATE ReportWindTurbine

CONTAINS

SUBROUTINE SimWindTurbine(GeneratorType,GeneratorName,GeneratorIndex,RunFlag,WTLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   October 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the simulation of wind turbine component.
          ! This drivers manages the calls to all of the other drivers and simulation algorithms.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE InputProcessor,      ONLY: FindItemInList
USE DataGlobalConstants, ONLY: iGeneratorWindTurbine
USE General,             ONLY: TrimSigDigits
       ! na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, INTENT(IN)          :: GeneratorType   ! Type of Generator
INTEGER, INTENT(INOUT)       :: GeneratorIndex  ! Generator index
CHARACTER(len=*), INTENT(IN) :: GeneratorName   ! User specified name of Generator
LOGICAL, INTENT(IN)          :: RunFlag         ! ON or OFF
REAL(r64), INTENT(IN)        :: WTLoad          ! Electrical load on WT (not used)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL, SAVE :: GetInputFlag=.true.
INTEGER       :: WindTurbineNum
        ! Obtains and allocates heat balance related parameters from input
IF (GetInputFlag) THEN
  CALL GetWindTurbineInput
       GetInputFlag=.false.
ENDIF

IF (GeneratorIndex == 0) THEN
    WindTurbineNum = FindItemInList(GeneratorName,WindTurbineSys%Name,NumWindTurbines)
   IF (WindTurbineNum == 0) THEN
      CALL ShowFatalError('SimWindTurbine: Specified Generator not one of Valid Wind Turbine Generators '//  &
                          TRIM(GeneratorName))
   END IF
   GeneratorIndex=WindTurbineNum
ELSE
   WindTurbineNum = GeneratorIndex
  IF (WindTurbineNum > NumWindTurbines .or. WindTurbineNum < 1) THEN
    CALL ShowFatalError('SimWindTurbine: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(WindTurbineNum))// &
                        ', Number of Wind Turbine Generators='//TRIM(TrimSigDigits(NumWindTurbines))//  &
                        ', Generator name='//TRIM(GeneratorName))
  ENDIF
    IF (GeneratorName /= WindTurbineSys(WindTurbineNum)%Name) THEN
      CALL ShowFatalError('SimMWindTurbine: Invalid GeneratorIndex passed='//TRIM(TrimSigDigits(WindTurbineNum))// &
                          ', Generator name='//TRIM(GeneratorName)//', stored Generator Name for that index='//  &
                          TRIM(WindTurbineSys(WindTurbineNum)%Name))
    ENDIF
  ENDIF

  CALL InitWindTurbine (WindTurbineNum)

  CALL CalcWindTurbine (WindTurbineNum, RunFlag)

  CALL ReportWindTurbine (WindTurbineNum)

RETURN

END SUBROUTINE SimWindTurbine

SUBROUTINE GetWTGeneratorResults(GeneratorType,GeneratorIndex, GeneratorPower, GeneratorEnergy, &
                                 ThermalPower, ThermalEnergy)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         B. Griffith
    !       DATE WRITTEN   Aug. 2008
    !       MODIFIED       D Kang, October 2009 for Wind Turbine
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine provides a "get" method to collect results for individual electic load centers.

    ! METHODOLOGY EMPLOYED:
    !

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:

IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)      :: GeneratorType    ! Type of Generator
INTEGER, INTENT(IN)      :: GeneratorIndex   ! Generator number
REAL(r64), INTENT(OUT)   :: GeneratorPower   ! Electrical power
REAL(r64), INTENT(OUT)   :: GeneratorEnergy  ! Electrical energy
REAL(r64), INTENT(OUT)   :: ThermalPower
REAL(r64), INTENT(OUT)   :: ThermalEnergy

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

GeneratorPower  = WindTurbineSys(GeneratorIndex)%Power
GeneratorEnergy = WindTurbineSys(GeneratorIndex)%Energy

    ! Thermal energy is ignored
ThermalPower  = 0.0D0
ThermalEnergy = 0.0D0

RETURN

END SUBROUTINE GetWTGeneratorResults

SUBROUTINE GetWindTurbineInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   October 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets input data for wind turbine components
          ! and stores it in the wind turbine data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE InputProcessor,   ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList,SameString,VerifyName,GetObjectDefMaxArgs
USE ScheduleManager,  ONLY: GetScheduleIndex
USE General,          ONLY: RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: Blank = ' '
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='Generator:WindTurbine'
REAL(r64), PARAMETER :: SysEffDefault  = 0.835d0  ! Default value of overall system efficiency
REAL(r64), PARAMETER :: MaxTSR         = 12.0d0   ! Maximum tip speed ratio
REAL(r64), PARAMETER :: DefaultPC      = 0.25d0   ! Default power coefficient
REAL(r64), PARAMETER :: MaxPowerCoeff  = 0.59d0   ! Maximum power coefficient
REAL(r64), PARAMETER :: DefaultH       = 50.0d0   ! Default of height for local wind speed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: ErrorsFound = .false.    ! If errors detected in input
  LOGICAL :: IsNotOK                  ! Flag to verify name
  LOGICAL :: IsBlank                  ! Flag for blank name
  INTEGER :: WindTurbineNum           ! Wind turbine number
  INTEGER :: NumAlphas                ! Number of Alphas for each GetobjectItem call
  INTEGER :: NumNumbers               ! Number of Numbers for each GetobjectItem call
  INTEGER :: NumArgs
  INTEGER :: IOStat
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaArgs     ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: rNumericArgs      ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.

            ! Initializations and allocations
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumArgs,NumAlphas,NumNumbers)
  ALLOCATE(cAlphaArgs(NumAlphas))
  cAlphaArgs=' '
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(rNumericArgs(NumNumbers))
  rNumericArgs=0.0d0
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.true.

NumWindTurbines = GetNumObjectsFound(CurrentModuleObject)

ALLOCATE (WindTurbineSys(NumWindTurbines))

            ! Flow
DO WindTurbineNum = 1, NumWindTurbines

  CALL GetObjectItem(CurrentModuleObject,WindTurbineNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStat,  &
                    AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                    AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
       CALL VerifyName(cAlphaArgs(1),WindTurbineSys%Name,WindTurbineNum,IsNotOK,IsBlank,CurrentModuleObject//' Name')

        IF (IsNotOK) THEN
           ErrorsFound = .true.
        ENDIF

    WindTurbineSys(WindTurbineNum)%Name = cAlphaArgs(1)       ! Name of wind turbine

    WindTurbineSys(WindTurbineNum)%Schedule = cAlphaArgs(2)   ! Get schedule
    IF (lAlphaBlanks(2)) THEN
      WindTurbineSys(WindTurbineNum)%SchedPtr  = ScheduleAlwaysOn
    ELSE
      WindTurbineSys(WindTurbineNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
      IF (WindTurbineSys(WindTurbineNum)%SchedPtr == 0) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cAlphaFields(2))//'="'//trim(cAlphaArgs(2))//'" not found.')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF
        ! Select rotor type
    SELECT CASE (cAlphaArgs(3))
      CASE ('HORIZONTALAXISWINDTURBINE','HAWT', 'NONE', ' ')
        WindTurbineSys(WindTurbineNum)%RotorType = HAWT
      CASE ('VERTICALAXISWINDTURBINE','VAWT')
        WindTurbineSys(WindTurbineNum)%RotorType = VAWT
      CASE DEFAULT
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
          trim(cAlphaFields(3))//'="'//trim(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
    END SELECT

        ! Select control type
    SELECT CASE (cAlphaArgs(4))
      CASE ('FIXEDSPEEDFIXEDPITCH','FSFP')
        WindTurbineSys(WindTurbineNum)%ControlType = FSFP
      CASE ('FIXEDSPEEDVARIABLEPITCH','FSVP')
        WindTurbineSys(WindTurbineNum)%ControlType = FSVP
      CASE ('VARIABLESPEEDFIXEDPITCH','VSFP')
        WindTurbineSys(WindTurbineNum)%ControlType = VSFP
      CASE ('VARIABLESPEEDVARIABLEPITCH','VSVP', 'NONE', ' ')
        WindTurbineSys(WindTurbineNum)%ControlType = VSVP
      CASE DEFAULT
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//&
          trim(cAlphaFields(4))//'="'//trim(cAlphaArgs(4))//'".')
        ErrorsFound = .TRUE.
    END SELECT

    WindTurbineSys(WindTurbineNum)%RatedRotorSpeed = rNumericArgs(1) ! Maximum rotor speed in rpm
    IF (WindTurbineSys(WindTurbineNum)%RatedRotorSpeed <= 0.0d0) THEN
      IF (lNumericBlanks(1)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(1))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(1))//'=['//trim(RoundSigDigits(rNumericArgs(1),2))//  &
           '] must be greater than zero.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%RotorDiameter = rNumericArgs(2) ! Rotor diameter in m
    IF (WindTurbineSys(WindTurbineNum)%RotorDiameter <= 0.0d0) THEN
      IF (lNumericBlanks(2)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(2))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(2))//'=['//trim(RoundSigDigits(rNumericArgs(2),1))//  &
           '] must be greater than zero.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%RotorHeight = rNumericArgs(3)     ! Overall height of the rotor
    IF (WindTurbineSys(WindTurbineNum)%RotorHeight <= 0.0d0) THEN
      IF (lNumericBlanks(3)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(3))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(3))//'=['//trim(RoundSigDigits(rNumericArgs(3),1))//  &
           '] must be greater than zero.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%NumOfBlade = rNumericArgs(4)     ! Total number of blade
    IF (WindTurbineSys(WindTurbineNum)%NumOfBlade == 0) THEN
      CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(4))//'=['//trim(RoundSigDigits(rNumericArgs(4),0))//  &
           '] must be greater than zero.')
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%RatedPower = rNumericArgs(5)     ! Rated average power
    IF (WindTurbineSys(WindTurbineNum)%RatedPower == 0.0d0) THEN
      IF (lNumericBlanks(5)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(5))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(5))//'=['//trim(RoundSigDigits(rNumericArgs(5),2))//  &
           '] must be greater than zero.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%RatedWindSpeed = rNumericArgs(6)     ! Rated wind speed
    IF (WindTurbineSys(WindTurbineNum)%RatedWindSpeed == 0.0d0) THEN
      IF (lNumericBlanks(6)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(6))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(6))//'=['//trim(RoundSigDigits(rNumericArgs(6),2))//  &
           '] must be greater than zero.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%CutInSpeed = rNumericArgs(7)      ! Minimum wind speed for system operation
    IF (WindTurbineSys(WindTurbineNum)%CutInSpeed == 0.0d0) THEN
      IF (lNumericBlanks(7)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(7))//' is required but input is blank.')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(7))//'=['//trim(RoundSigDigits(rNumericArgs(7),2))//  &
           '] must be greater than zero.')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%CutOutSpeed = rNumericArgs(8)      ! Minimum wind speed for system operation
    IF (WindTurbineSys(WindTurbineNum)%CutOutSpeed == 0.0d0) THEN
      IF (lNumericBlanks(8)) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(8))//' is required but input is blank.')
      ELSE IF (WindTurbineSys(WindTurbineNum)%CutOutSpeed <= WindTurbineSys(WindTurbineNum)%RatedWindSpeed) THEN
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(8))//'=['//trim(RoundSigDigits(rNumericArgs(8),2))//'] must be greater than '// &
           trim(cNumericFields(6))//'=['//trim(RoundSigDigits(rNumericArgs(6),2))//'].')
      ELSE
        CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(8))//'=['//trim(RoundSigDigits(rNumericArgs(8),2))//  &
           '] must be greater than zero')
      ENDIF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%SysEfficiency = rNumericArgs(9)   ! Overall wind turbine system efficiency
    IF (lNumericBlanks(9) .OR. WindTurbineSys(WindTurbineNum)%SysEfficiency == 0.0d0 .OR. &
        WindTurbineSys(WindTurbineNum)%SysEfficiency > 1.0d0) THEN
        WindTurbineSys(WindTurbineNum)%SysEfficiency = SysEffDefault
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(9))//'=['//trim(RoundSigDigits(rNumericArgs(9),2))//'].')
        CALL ShowContinueError('...The default value of '//trim(RoundSigDigits(SysEffDefault,3))//' for '// &
            trim(cNumericFields(9))//' was assumed.')
    END IF

    WindTurbineSys(WindTurbineNum)%MaxTipSpeedRatio = rNumericArgs(10)   ! Maximum tip speed ratio
    IF (WindTurbineSys(WindTurbineNum)%MaxTipSpeedRatio == 0.0d0) THEN
        IF (lNumericBlanks(10)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(10))//' is required but input is blank.')
        ELSE
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(10))//'=['//trim(RoundSigDigits(rNumericArgs(10),2))//  &
             '] must be greater than zero.')
        END IF
      ErrorsFound = .TRUE.
    ENDIF
    IF (WindTurbineSys(WindTurbineNum)%SysEfficiency > MaxTSR) THEN
        WindTurbineSys(WindTurbineNum)%SysEfficiency = MaxTSR
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(10))//'=['//trim(RoundSigDigits(rNumericArgs(10),2))//'].')
        CALL ShowContinueError('...The default value of '//trim(RoundSigDigits(MaxTSR,1))//' for '// &
            trim(cNumericFields(10))//' was assumed.')
    END IF

    WindTurbineSys(WindTurbineNum)%MaxPowerCoeff = rNumericArgs(11)   ! Maximum power coefficient
    IF (WindTurbineSys(WindTurbineNum)%RotorType == HAWT .AND. &
        WindTurbineSys(WindTurbineNum)%MaxPowerCoeff == 0.0d0) THEN
        IF (lNumericBlanks(11)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(11))//' is required but input is blank.')
        ELSE
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(11))//'=['//trim(RoundSigDigits(rNumericArgs(11),2))//  &
             '] must be greater than zero.')
        END IF
      ErrorsFound = .TRUE.
    ENDIF
    IF (WindTurbineSys(WindTurbineNum)%MaxPowerCoeff > MaxPowerCoeff) THEN
        WindTurbineSys(WindTurbineNum)%MaxPowerCoeff = DefaultPC
        CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
           trim(cNumericFields(11))//'=['//trim(RoundSigDigits(rNumericArgs(11),2))//'].')
        CALL ShowContinueError('...The default value of '//trim(RoundSigDigits(DefaultPC,2))//' for '// &
            trim(cNumericFields(11))//' will be used.')
    END IF

    WindTurbineSys(WindTurbineNum)%LocalAnnualAvgWS = rNumericArgs(12)   ! Local wind speed annually averaged
    IF (WindTurbineSys(WindTurbineNum)%LocalAnnualAvgWS == 0.0d0) THEN
        IF (lNumericBlanks(12)) THEN
          CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(12))//' is necessary for accurate prediction but input is blank.')
        ELSE
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(12))//'=['//trim(RoundSigDigits(rNumericArgs(12),2))//  &
             '] must be greater than zero.')
          ErrorsFound=.true.
        END IF
    ENDIF

    WindTurbineSys(WindTurbineNum)%HeightForLocalWS = rNumericArgs(13)   ! Height of local meteorological station
    IF (WindTurbineSys(WindTurbineNum)%HeightForLocalWS == 0.0d0) THEN
       IF (WindTurbineSys(WindTurbineNum)%LocalAnnualAvgWS == 0.0d0) THEN
         WindTurbineSys(WindTurbineNum)%HeightForLocalWS = 0.0d0
       ELSE
         WindTurbineSys(WindTurbineNum)%HeightForLocalWS = DefaultH
         IF (lNumericBlanks(13)) THEN
           CALL ShowWarningError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
                trim(cNumericFields(13))//' is necessary for accurate prediction but input is blank.')
           CALL ShowContinueError('...The default value of '//trim(RoundSigDigits(DefaultH,2))//' for '// &
            trim(cNumericFields(13))//' will be used.')
         ELSE
           CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
               trim(cNumericFields(13))//'=['//trim(RoundSigDigits(rNumericArgs(13),2))//  &
               '] must be greater than zero.')
           ErrorsFound=.true.
         END IF
      END IF
    END IF

    WindTurbineSys(WindTurbineNum)%ChordArea = rNumericArgs(14)      ! Chord area of a single blade for VAWTs
    IF (WindTurbineSys(WindTurbineNum)%RotorType == VAWT .AND. &
        WindTurbineSys(WindTurbineNum)%ChordArea == 0.0d0) THEN
        IF (lNumericBlanks(14)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(14))//' is required but input is blank.')
        ELSE
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(14))//'=['//trim(RoundSigDigits(rNumericArgs(14),2))//  &
             '] must be greater than zero.')
        END IF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%DragCoeff = rNumericArgs(15)      ! Blade drag coefficient
    IF (WindTurbineSys(WindTurbineNum)%RotorType == VAWT .AND. &
        WindTurbineSys(WindTurbineNum)%DragCoeff == 0.0d0) THEN
        IF (lNumericBlanks(15)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(15))//' is required but input is blank.')
        ELSE
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(15))//'=['//trim(RoundSigDigits(rNumericArgs(15),2))//  &
             '] must be greater than zero.')
        END IF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%LiftCoeff = rNumericArgs(16)      ! Blade lift coefficient
    IF (WindTurbineSys(WindTurbineNum)%RotorType == VAWT .AND. &
        WindTurbineSys(WindTurbineNum)%LiftCoeff == 0.0d0) THEN
        IF (lNumericBlanks(16)) THEN
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(16))//' is required but input is blank.')
        ELSE
          CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//   &
             trim(cNumericFields(16))//'=['//trim(RoundSigDigits(rNumericArgs(16),2))//  &
             '] must be greater than zero.')
        END IF
      ErrorsFound = .TRUE.
    ENDIF

    WindTurbineSys(WindTurbineNum)%PowerCoeffC1 = rNumericArgs(17)      ! Empirical power coefficient C1
    IF (lNumericBlanks(17)) THEN
      WindTurbineSys(WindTurbineNum)%PowerCoeffC1 = 0.0d0
    END IF
    WindTurbineSys(WindTurbineNum)%PowerCoeffC2 = rNumericArgs(18)      ! Empirical power coefficient C2
    IF (lNumericBlanks(18)) THEN
      WindTurbineSys(WindTurbineNum)%PowerCoeffC2 = 0.0d0
    END IF
    WindTurbineSys(WindTurbineNum)%PowerCoeffC3 = rNumericArgs(19)      ! Empirical power coefficient C3
    IF (lNumericBlanks(19)) THEN
      WindTurbineSys(WindTurbineNum)%PowerCoeffC3 = 0.0d0
    END IF
    WindTurbineSys(WindTurbineNum)%PowerCoeffC4 = rNumericArgs(20)      ! Empirical power coefficient C4
    IF (lNumericBlanks(20)) THEN
      WindTurbineSys(WindTurbineNum)%PowerCoeffC4 = 0.0d0
    END IF
    WindTurbineSys(WindTurbineNum)%PowerCoeffC5 = rNumericArgs(21)      ! Empirical power coefficient C5
    IF (lNumericBlanks(21)) THEN
      WindTurbineSys(WindTurbineNum)%PowerCoeffC5 = 0.0d0
    END IF
    WindTurbineSys(WindTurbineNum)%PowerCoeffC6 = rNumericArgs(22)      ! Empirical power coefficient C6
    IF (lNumericBlanks(22)) THEN
      WindTurbineSys(WindTurbineNum)%PowerCoeffC6 = 0.0d0
    END IF

END DO

DEALLOCATE(cAlphaArgs)
DEALLOCATE(cAlphaFields)
DEALLOCATE(cNumericFields)
DEALLOCATE(rNumericArgs)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(lNumericBlanks)

IF (ErrorsFound) Call ShowFatalError(CurrentModuleObject//' errors occurred in input.  Program terminates.')

DO WindTurbineNum = 1, NumWindTurbines
    CALL SetupOutputVariable('Generator Produced Electric Power [W]',WindTurbineSys(WindTurbineNum)%Power, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
    CALL SetupOutputVariable('Generator Produced Electric Energy [J]',WindTurbineSys(WindTurbineNum)%Energy, &
                              'System','Sum',WindTurbineSys(WindTurbineNum)%Name, &
                              ResourceTypeKey='ElectricityProduced',EndUseKey='WINDTURBINE',GroupKey='Plant')
    CALL SetupOutputVariable('Generator Turbine Local Wind Speed [m/s]',WindTurbineSys(WindTurbineNum)%LocalWindSpeed, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
    CALL SetupOutputVariable('Generator Turbine Local Air Density [kg/m3]',WindTurbineSys(WindTurbineNum)%LocalAirDensity, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
    CALL SetupOutputVariable('Generator Turbine Tip Speed Ratio []',WindTurbineSys(WindTurbineNum)%TipSpeedRatio, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
    SELECT CASE (WindTurbineSys(WindTurbineNum)%RotorType)
      CASE (HAWT)
        CALL SetupOutputVariable('Generator Turbine Power Coefficient []',WindTurbineSys(WindTurbineNum)%PowerCoeff, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
      CASE (VAWT)
        CALL SetupOutputVariable('Generator Turbine Chordal Component Velocity [m/s]',WindTurbineSys(WindTurbineNum)%ChordalVel, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
        CALL SetupOutputVariable('Generator Turbine Normal Component Velocity [m/s]',WindTurbineSys(WindTurbineNum)%NormalVel, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
        CALL SetupOutputVariable('Generator Turbine Relative Flow Velocity [m/s]',WindTurbineSys(WindTurbineNum)%RelFlowVel, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
        CALL SetupOutputVariable('Generator Turbine Attack Angle [deg]',WindTurbineSys(WindTurbineNum)%AngOfAttack, &
                             'System','Average',WindTurbineSys(WindTurbineNum)%Name)
    END SELECT
END DO

RETURN

END SUBROUTINE GetWindTurbineInput

SUBROUTINE InitWindTurbine (WindTurbineNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Oct 2009
          !       MODIFIED       Linda K. Lawrie, December 2009 for reading stat file
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads monthly average wind speed from stat file and then
          ! determines annual average wind speed. Differences between this TMY wind speed
          ! and local wind speed that the user inputs are then factored.
          ! IF the user has no local wind data and does not enter the local wind speed to be factored,
          ! then the factor of 1 is assigned, so that wind speed estimated
          ! at the particular rotor height is used with no factorization.
          ! It also initializes module variables at each time step.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
USE DataEnvironment,   ONLY : WeatherFileWindModCoeff, SiteWindBLHeight, SiteWindExp

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)    :: WindTurbineNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: Blank = ' '
CHARACTER(len=1), PARAMETER :: TabChr=CHAR(9)   ! Tab character

         ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL, SAVE     :: MyOneTimeFlag = .true.
INTEGER, EXTERNAL :: GetNewUnitNumber         ! External  function to "get" a unit number
INTEGER   :: OpenStatus    ! Open status of stat file
INTEGER   :: ReadStatus    ! Reading status of stat file
INTEGER   :: StatFile      ! Weather Stat File
INTEGER   :: lnPtr         ! scan pointer for Line input
INTEGER   :: mon           ! loop counter
LOGICAL   :: wsStatFound   ! logical noting that wind stats were found
LOGICAL   :: fileExists    ! true if in.stat file exists
LOGICAL   :: warningShown  ! true if the <365 warning has already been shown
CHARACTER(len=200) :: lineIn
REAL(r64), DIMENSION(12) :: MonthWS
REAL(r64),SAVE :: AnnualTMYWS=0.0d0   ! Annual average wind speed in stat file
REAL(r64) :: LocalTMYWS    ! Annual average wind speed at the rotor height

        ! Estimate average annual wind speed once
 IF (MyOneTimeFlag) THEN
   wsStatFound=.false.
   INQUIRE(file='in.stat',EXIST=fileExists)
   IF (fileExists) THEN
     StatFile = GetNewUnitNumber()
     readstatus=0
     OPEN (unit=statFile, file='in.stat', action='READ',iostat=readStatus)
     IF (readstatus /= 0) THEN
       CALL ShowFatalError('InitWindTurbine: Could not open file "in.stat" for input (read).')
     ENDIF
     DO WHILE (readStatus == 0) !end of file
       READ(UNIT=statFile,FMT='(A)',IOSTAT=readStatus) lineIn
       ! reconcile line with different versions of stat file
       lnPtr=INDEX(lineIn,'Wind Speed')
       if (lnPtr == 0) CYCLE
       ! have hit correct section.
       DO WHILE (readStatus == 0)  ! find daily avg line
         READ(UNIT=statFile,FMT='(A)',IOSTAT=readStatus) lineIn
         lnPtr=INDEX(lineIn,'Daily Avg')
         if (lnPtr == 0) CYCLE
         ! tab delimited file
         lineIn=lineIn(lnptr+10:)
         MonthWS=0.0d0
         wsStatFound=.true.
         warningShown=.false.
         DO mon=1,12
           lnPtr=INDEX(lineIn,TabChr)
           IF (lnPtr /= 1) THEN
             IF (lineIn(1:lnPtr-1) /= Blank) THEN
               IF (lnPtr /= 0) THEN
                 READ(lineIn(1:lnPtr-1),*) MonthWS(mon)
                 lineIn=lineIn(lnPtr+1:)
               ENDIF
             ELSE ! blank field
               IF (.not. warningShown) THEN
                 CALL ShowWarningError('InitWindTurbine: read from in.stat file shows <365 days in weather file. '// &
                    'Annual average wind speed used will be inaccurate.')
                 lineIn=lineIn(lnPtr+1:)
                 warningShown=.true.
               ENDIF
             ENDIF
           ELSE  ! two tabs in succession
             IF (.not. warningShown) THEN
               CALL ShowWarningError('InitWindTurbine: read from in.stat file shows <365 days in weather file. '// &
                  'Annual average wind speed used will be inaccurate.')
               lineIn=lineIn(lnPtr+1:)
               warningShown=.true.
             ENDIF
           ENDIF
         ENDDO
         EXIT
       ENDDO
       if (wsStatFound) EXIT
     ENDDO
     CLOSE(UNIT=statFile)
     IF (wsStatFound) THEN
       AnnualTMYWS = SUM(MonthWS)/12.0d0
     ELSE
       CALL ShowWarningError('InitWindTurbine: stat file did not include Wind Speed statistics. '//  &
           'TMY Wind Speed adjusted at the height is used.')
     ENDIF
   ELSE  ! No stat file
     CALL ShowWarningError('InitWindTurbine: stat file missing. '//  &
          'TMY Wind Speed adjusted at the height is used.')
   END IF

   MyOneTimeFlag = .false.

 END IF

WindTurbineSys(WindTurbineNum)%AnnualTMYWS=AnnualTMYWS

    ! Factor differences between TMY wind data and local wind data once
IF (AnnualTMYWS > 0.0d0 .AND. WindTurbineSys(WindTurbineNum)%WSFactor == 0.0d0 .AND. &
    WindTurbineSys(WindTurbineNum)%LocalAnnualAvgWS > 0) THEN
    ! Convert the annual wind speed to the local wind speed at the height of the local station, then factor
    LocalTMYWS = AnnualTMYWS * WeatherFileWindModCoeff * &
                (WindTurbineSys(WindTurbineNum)%HeightForLocalWS / SiteWindBLHeight) ** SiteWindExp
    WindTurbineSys(WindTurbineNum)%WSFactor = LocalTMYWS / WindTurbineSys(WindTurbineNum)%LocalAnnualAvgWS
END IF
    ! Assign factor of 1.0 if no stat file or no input of local average wind speed
IF (WindTurbineSys(WindTurbineNum)%WSFactor == 0.0d0) WindTurbineSys(WindTurbineNum)%WSFactor = 1.0d0

    ! Do every time step initialization
 WindTurbineSys(WindTurbineNum)%Power          = 0.0d0
 WindTurbineSys(WindTurbineNum)%TotPower       = 0.0d0
 WindTurbineSys(WindTurbineNum)%PowerCoeff     = 0.0d0
 WindTurbineSys(WindTurbineNum)%TipSpeedRatio  = 0.0d0
 WindTurbineSys(WindTurbineNum)%ChordalVel     = 0.0d0
 WindTurbineSys(WindTurbineNum)%NormalVel      = 0.0d0
 WindTurbineSys(WindTurbineNum)%RelFlowVel     = 0.0d0
 WindTurbineSys(WindTurbineNum)%AngOfAttack    = 0.0d0
 WindTurbineSys(WindTurbineNum)%TanForce       = 0.0d0
 WindTurbineSys(WindTurbineNum)%NorForce       = 0.0d0
 WindTurbineSys(WindTurbineNum)%TotTorque      = 0.0d0

RETURN

END SUBROUTINE InitWindTurbine


SUBROUTINE CalcWindTurbine (WindTurbineNum, RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   Octorber 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
! Sathyajith Mathew. 2006. Wind Energy: Fundamental, Resource Analysis and Economics. Springer,
!     Chap. 2, pp. 11-15
! Mazharul Islam, David S.K. Ting, and Amir Fartaj. 2008. Aerodynamic Models for Darrieus-type sSraight-bladed
!     Vertical Axis Wind Turbines. Renewable & Sustainable Energy Reviews, Volume 12, pp.1087-1109

          ! USE STATEMENTS:
USE ScheduleManager,   ONLY: GetCurrentScheduleValue
USE Psychrometrics,    ONLY: PsyWFnTdbTwbPb, PsyRhoAirFnPbTdbW
USE DataEnvironment,   ONLY: OutBaroPress, WindSpeed, WindSpeedAt, OutDryBulbTempAt, OutWetBulbTempAt, OutBaroPressAt

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)           :: WindTurbineNum   ! System is on
LOGICAL, INTENT(IN)           :: RunFlag          ! System is on

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER :: MaxTheta     = 90.0d0     ! Maximum of theta
REAL(r64), PARAMETER :: MaxDegree    = 360.0d0    ! Maximum limit of outdoor air wind speed in m/s
REAL(r64), PARAMETER :: PitchAngle   = 0.0d0      ! No pitch control, i.e. maximum rotor speed
REAL(r64), PARAMETER :: SecInMin     = 60.0d0
REAL(r64), PARAMETER :: MaxTSR       = 12.0d0     ! Maximum of tip speed ratio

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: LocalWindSpeed    ! Ambient wind speed at the specific height in m/s
REAL(r64) :: RotorH            ! Height of the rotor in m
REAL(r64) :: RotorD            ! Diameter of the rotor in m
REAL(r64) :: LocalHumRat       ! Local humidity ratio of the air at the specific height
REAL(r64) :: LocalAirDensity   ! Local density at the specific height in m
REAL(r64) :: PowerCoeff        ! Power coefficient
REAL(r64) :: SweptArea         ! Swept area of the rotor in m2
REAL(r64) :: WTPower           ! Total Power generated by the turbine in the quasi-steady state in Watts
REAL(r64) :: Power             ! Actual power of the turbine in Watts
REAL(r64) :: TipSpeedRatio     ! Tip speed ratio (TSR)
REAL(r64) :: TipSpeedRatioAtI  ! Tip speed ratio at the ith time step
REAL(r64) :: AzimuthAng        ! Azimuth angle of blades in degree
REAL(r64) :: ChordalVel        ! Chordal velocity component in m/s
REAL(r64) :: NormalVel         ! Normal velocity component in m/s
REAL(r64) :: AngOfAttack       ! Angle of attack of a single blade in degree
REAL(r64) :: RelFlowVel        ! Relative flow velocity component in m/s
REAL(r64) :: TanForce          ! Tangential force in N.m
REAL(r64) :: NorForce          ! Normal force in N.m
REAL(r64) :: RotorVel          ! Rotor velocity in m/s
REAL(r64) :: AvgTanForce       ! Average tangential force in N.m
REAL(r64) :: Constant          ! Constants within integrand of tangential force
REAL(r64) :: IntRelFlowVel     ! Integration of relative flow velocity
REAL(r64) :: TotTorque         ! Total torque for the number of blades
REAL(r64) :: Omega             ! Angular velocity of rotor in rad/s
REAL(r64) :: TanForceCoeff     ! Tnagential force coefficient
REAL(r64) :: NorForceCoeff     ! Normal force coefficient
REAL(r64) :: Period            ! Period of sine and cosine functions
REAL(r64) :: Integrand         ! Integrand of tangential force
REAL(r64) :: C1                ! Empirical power coefficient C1
REAL(r64) :: C2                ! Empirical power coefficient C2
REAL(r64) :: C3                ! Empirical power coefficient C3
REAL(r64) :: C4                ! Empirical power coefficient C4
REAL(r64) :: C5                ! Empirical power coefficient C5
REAL(r64) :: C6                ! Empirical power coefficient C6
REAL(r64) :: LocalTemp         ! Ambient air temperature at the height in degree C
REAL(r64) :: LocalPress        ! Local atmospheric pressure at the particular height in Pa
REAL(r64) :: InducedVel        ! Induced velocity on the rotor in m/s
!unused REAL(r64) :: SysEfficiency     ! Overall wind turbine system efficiency including generator and inverter
REAL(r64) :: MaxPowerCoeff     ! Maximum power coefficient
REAL(r64) :: RotorSpeed        ! Speed of rotors

        ! Estimate local velocity and density
RotorH = WindTurbineSys(WindTurbineNum)%RotorHeight
RotorD = WindTurbineSys(WindTurbineNum)%RotorDiameter
RotorSpeed = WindTurbineSys(WindTurbineNum)%RatedRotorSpeed
LocalTemp       = OutDryBulbTempAt(RotorH)
LocalPress      = OutBaroPressAt(RotorH)
LocalHumRat     = PsyWFnTdbTwbPb(LocalTemp,OutWetBulbTempAt(RotorH),LocalPress)
LocalAirDensity = PsyRhoAirFnPbTdbW(LocalPress,LocalTemp,LocalHumRat)
LocalWindSpeed  = WindSpeedAt(RotorH)
LocalWindSpeed  = LocalWindSpeed / WindTurbineSys(WindTurbineNum)%WSFactor

        ! Flow
        ! Check wind conditions for system operation
IF (GetCurrentScheduleValue(WindTurbineSys(WindTurbineNum)%SchedPtr) > 0 .AND. &
    LocalWindSpeed > WindTurbineSys(WindTurbineNum)%CutInSpeed .AND. &
    LocalWindSpeed < WindTurbineSys(WindTurbineNum)%CutOutSpeed) THEN

        ! System is on
  Period    = 2.0d0 * pi
  Omega     = (RotorSpeed * Period) / SecInMin
  SweptArea = (Pi * RotorD**2.0d0) / 4
  TipSpeedRatio = (Omega * (RotorD/2.0d0)) / LocalWindSpeed

        ! Limit maximum tip speed ratio
  IF (TipSpeedRatio > WindTurbineSys(WindTurbineNum)%MaxTipSpeedRatio) THEN
    TipSpeedRatio = WindTurbineSys(WindTurbineNum)%MaxTipSpeedRatio
  END IF

  SELECT CASE (WindTurbineSys(WindTurbineNum)%RotorType)
    CASE (HAWT) ! Horizontal axis wind turbine

       MaxPowerCoeff = WindTurbineSys(WindTurbineNum)%MaxPowerCoeff
            ! Check if empirical constants are available
       C1 = WindTurbineSys(WindTurbineNum)%PowerCoeffC1
       C2 = WindTurbineSys(WindTurbineNum)%PowerCoeffC2
       C3 = WindTurbineSys(WindTurbineNum)%PowerCoeffC3
       C4 = WindTurbineSys(WindTurbineNum)%PowerCoeffC4
       C5 = WindTurbineSys(WindTurbineNum)%PowerCoeffC5
       C6 = WindTurbineSys(WindTurbineNum)%PowerCoeffC6

       IF (C1 > 0.0d0 .AND. C2 > 0.0d0 .AND. C3 > 0.0d0 .AND. C4 >= 0.0d0 .AND. C5 > 0.0d0 .AND. C6 > 0.0d0) THEN
            ! Analytical approximation
            ! Maximum power, i.e., rotor speed is at maximum, and pitch angle is zero
         TipSpeedRatioAtI = 1.0d0 / ( (1.0d0 / (TipSpeedRatio + 0.08d0 * PitchAngle)) - (0.035d0 / (PitchAngle**3 + 1.0d0)))
         PowerCoeff = C1 * ((C2 / TipSpeedRatioAtI) - (C3 * PitchAngle) - (C4 * PitchAngle**1.5d0) - C5) * &
                    (EXP(-(C6 / TipSpeedRatioAtI)))
         IF (PowerCoeff > MaxPowerCoeff) THEN
           PowerCoeff = MaxPowerCoeff
         END IF
         WTPower = 0.5d0 * LocalAirDensity * PowerCoeff * SweptArea * LocalWindSpeed**3
       ELSE   ! Simple approximation
         WTPower = 0.5d0 * LocalAirDensity * SweptArea * LocalWindSpeed**3 * MaxPowerCoeff
         PowerCoeff = MaxPowerCoeff
       END IF
            ! Maximum of rated power
       IF (LocalWindSpeed >= WindTurbineSys(WindTurbineNum)%RatedWindSpeed .OR. &
           WTPower > WindTurbineSys(WindTurbineNum)%RatedPower) THEN
         WTPower    = WindTurbineSys(WindTurbineNum)%RatedPower
         PowerCoeff = WTPower / (0.5d0 * LocalAirDensity * SweptArea * LocalWindSpeed**3.0d0)
       END IF
            ! Recalculated Cp at the rated power
       WindTurbineSys(WindTurbineNum)%PowerCoeff  = PowerCoeff

    CASE (VAWT)  ! Vertical axis wind turbine
       RotorVel   = Omega * (RotorD / 2.0d0)
            ! Recalculated omega, if TSR is greater than the maximum
       IF (TipSpeedRatio >= WindTurbineSys(WindTurbineNum)%MaxTipSpeedRatio) THEN
       RotorVel = LocalWindSpeed * WindTurbineSys(WindTurbineNum)%MaxTipSpeedRatio
       Omega    = RotorVel / (RotorD / 2.0d0)
       END IF

       AzimuthAng = MaxDegree / WindTurbineSys(WindTurbineNum)%NumOfBlade
            ! Azimuth angle between zero and 90 degree
       IF (AzimuthAng > MaxTheta ) THEN  ! Number of blades is 2 or 3
       AzimuthAng = AzimuthAng - MaxTheta
          IF (AzimuthAng == MaxTheta) THEN  ! 2 blades
              AzimuthAng = 0.0d0
          END IF
       ELSE IF (AzimuthAng == MaxTheta) THEN ! 4 blades
       AzimuthAng = 0.0d0
       END IF

       InducedVel = LocalWindSpeed * 2.0d0/3.0d0
            ! Velocity components
       ChordalVel = RotorVel + InducedVel * COS(AzimuthAng * DegToRadians)
       NormalVel  = InducedVel * SIN(AzimuthAng * DegToRadians)
       RelFlowVel = SQRT(ChordalVel**2 + NormalVel**2)

            ! Angle of attack
       AngOfAttack = ATAN((SIN(AzimuthAng * DegToRadians) / ((RotorVel / LocalWindSpeed) / (InducedVel / LocalWindSpeed) + &
                        COS(AzimuthAng * DegToRadians))))

            ! Force coefficients
       TanForceCoeff = ABS(WindTurbineSys(WindTurbineNum)%LiftCoeff * SIN(AngOfAttack * DegToRadians) - &
                       WindTurbineSys(WindTurbineNum)%DragCoeff * COS(AngOfAttack * DegToRadians))
       NorForceCoeff = WindTurbineSys(WindTurbineNum)%LiftCoeff * COS(AngOfAttack * DegToRadians) + &
                       WindTurbineSys(WindTurbineNum)%DragCoeff * SIN(AngOfAttack * DegToRadians)

            ! Net tangential and normal forces
       TanForce = TanForceCoeff * 0.5d0 * LocalAirDensity * WindTurbineSys(WindTurbineNum)%ChordArea * RelFlowVel**2
       NorForce = NorForceCoeff * 0.5d0 * LocalAirDensity * WindTurbineSys(WindTurbineNum)%ChordArea * RelFlowVel**2
       Constant = (1.0d0 / Period) * (TanForce / RelFlowVel**2)

            ! Relative flow velocity is the only function of theta in net tangential force
            ! Integral of cos(theta) on zero to 2pi goes to zero
            ! Integrate constants only
       IntRelFlowVel = RotorVel**2 * Period + InducedVel**2 * Period

            ! Average tangential force on a single blade
       AvgTanForce = Constant * IntRelFlowVel
       TotTorque = WindTurbineSys(WindTurbineNum)%NumOfBlade * AvgTanForce * (RotorD / 2.0d0)
       WTPower = TotTorque * Omega

            ! Check if power produced is greater than maximum or rated power
       IF (WTPower > WindTurbineSys(WindTurbineNum)%RatedPower) THEN
         WTPower = WindTurbineSys(WindTurbineNum)%RatedPower
       END IF

       WindTurbineSys(WindTurbineNum)%ChordalVel = ChordalVel
       WindTurbineSys(WindTurbineNum)%NormalVel  = NormalVel
       WindTurbineSys(WindTurbineNum)%RelFlowVel = RelFlowVel
       WindTurbineSys(WindTurbineNum)%TanForce   = TanForce
       WindTurbineSys(WindTurbineNum)%NorForce   = NorForce
       WindTurbineSys(WindTurbineNum)%TotTorque  = TotTorque

  END SELECT

    IF (WTPower > WindTurbineSys(WindTurbineNum)%RatedPower) THEN
      WTPower = WindTurbineSys(WindTurbineNum)%RatedPower
    END IF

        ! Actual power generated by the wind turbine system
    Power = WTPower * WindTurbineSys(WindTurbineNum)%SysEfficiency

    WindTurbineSys(WindTurbineNum)%Power           = Power
    WindTurbineSys(WindTurbineNum)%TotPower        = WTPower
    WindTurbineSys(WindTurbineNum)%LocalWindSpeed  = LocalWindSpeed
    WindTurbineSys(WindTurbineNum)%LocalAirDensity = LocalAirDensity
    WindTurbineSys(WindTurbineNum)%TipSpeedRatio   = TipSpeedRatio

  ELSE  ! System is off
    WindTurbineSys(WindTurbineNum)%Power           = 0.0d0
    WindTurbineSys(WindTurbineNum)%TotPower        = 0.0d0
    WindTurbineSys(WindTurbineNum)%PowerCoeff      = 0.0d0
    WindTurbineSys(WindTurbineNum)%LocalWindSpeed  = LocalWindSpeed
    WindTurbineSys(WindTurbineNum)%LocalAirDensity = LocalAirDensity
    WindTurbineSys(WindTurbineNum)%TipSpeedRatio   = 0.0d0
    WindTurbineSys(WindTurbineNum)%ChordalVel      = 0.0d0
    WindTurbineSys(WindTurbineNum)%NormalVel       = 0.0d0
    WindTurbineSys(WindTurbineNum)%RelFlowVel      = 0.0d0
    WindTurbineSys(WindTurbineNum)%AngOfAttack     = 0.0d0
    WindTurbineSys(WindTurbineNum)%TanForce        = 0.0d0
    WindTurbineSys(WindTurbineNum)%NorForce        = 0.0d0
    WindTurbineSys(WindTurbineNum)%TotTorque       = 0.0d0
END IF

RETURN

END SUBROUTINE CalcWindTurbine


SUBROUTINE ReportWindTurbine (WindTurbineNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   October 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine fills remaining report variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataHVACGlobals,  ONLY : TimeStepSys

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)           :: WindTurbineNum
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

WindTurbineSys(WindTurbineNum)%Energy = WindTurbineSys(WindTurbineNum)%Power * TimeStepSys*SecInHour

RETURN

END SUBROUTINE ReportWindTurbine

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

END MODULE WindTurbine
