MODULE HVACControllers
  ! Module containing the controller simulation routines for the air loop

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   July 1998
  !       MODIFIED       Feb 2006, Dimitri Curtil (LBNL)
  !                      - Added tracing mechanism for debugging convergence process.
  !                        - Trace operation of each individual controller in a file named
  !                          'controller.<Controller Name>.csv'
  !                        - Trace operation of all controllers per air loop in a file named
  !                          'controller.<Air Loop Name>.csv'
  !                      - Added operations to enable cold start/speculative warm restart
  !                        and final check.
  !       MODIFIED       March 2006, Dimitri Curtil (LBNL)
  !                      - Added mechanism to track runtime performance statistics.
  !                      - Added routine to dump controller statistics to a file named
  !                        'statistics.HVACControllers.csv'
  !                      - Integrated smart root finder from MODULE RootFinder implemented in
  !                        file RootFinder.f90.
  !       MODIFIED       April 2006, Dimitri Curtil (LBNL)
  !                      - Added speedup optimization scheme to reuse air loop solution
  !                        obtained at the current HVAC iteration from solving the previous controller
  !                        on the loop (see ReuseIntermediateSolutionFlag). Of course this works only
  !                        if there are 2 or more controllers on the same air loop.
  !                      - Added speedup optimization scheme to reuse solution obtained
  !                        at the previous HVAC iteration for this controller during the
  !                        bracketing phase (see ReusePreviousSolutionFlag).
  !       MODIFIED       May 2006, Dimitri Curtil (LBNL)
  !                      - Added mechanism to monitor min/max bounds to ensure that they remain invariant
  !                        between successive controller iterations.
  !                      - Modified setpoint calculation to force the setpoint to be computed only once.
  !                      - Modified setpoint calculation for TEMPandHUMRAT control strategy to
  !                        force the setpoint to be computed once the air loop has been evaluated with
  !                        the max actuated value.
  !       MODIFIED       June 2006, Dimitri Curtil (LBNL)
  !                      - Renamed parameter variables so as to use lower caps.
  !                      - Replaced $ edit descriptor in WRITE statements with ADVANCE='No'
  !                      - Replaced the preprocessing directives TRACK_AIRLOOP, TRACE_AIRLOOP,
  !                        TRACE_CONTROLLER with corresponding environment variables defined
  !                        in DataSystemVariables.f90.
  !
  !       MODIFIED       Feb. 2010, Brent Griffith (NREL)
  !                       - changed plant loop interactions, Demand Side Update Phase 3
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the Controller System Component.
  !

  ! METHODOLOGY EMPLOYED:
  ! The main entry point if the SUBROUTINE ManageControllers().
  !
  ! 1. For proper operation, the subroutine must first be called with either the
  !    iControllerOpColdStart or iControllerOpWarmRestart operation code to initialize
  !    the various controllers.
  ! 2. Then the actuated variable for each controller is computed iteratively using
  !    root finding techniques that aim at forcing the sensed variable to be
  !    "equal" (within the user-specified tolerance) to the desired setpoint.
  !    This step is achieved by calling ManageController() with the iControllerOpIterate
  !    operation code.
  ! 3. Finally, after all controllers have been successfully simulated,  the subroutine has
  !    to be called one last time with the iControllerOpEnd operation code to ensure that
  !    the sequential solution indeed represents a valid global solution across all controllers
  !    simultaneously.
  !
  ! The following pseudo-code shows the typical calling sequence for the SUBROUTINE
  ! ManageControllers :
  !
  ! - for each controller on air loop
  !   - CALL ManageControllers( Operation=<iControllerOpColdStart or iControllerOpWarmRestart> )
  ! - simulate air loop components with the initial values for all actuated variables
  ! - for each controller on air loop
  !   - CALL ManageControllers( Operation=iControllerOpIterate, IsConvergedFlag )
  !   - if NOT IsConvergedFlag then
  !     - exit loop with error if too many iterations performed
  !     - simulate air loop components with the new candidate value for the actuated variable of
  !       the current controller
  ! - simulate air loop components with the final values for all actuated variables
  ! - for each controller on air loop
  !   - CALL ManageControllers( Operation=iControllerOpEnd, IsConvergedFlag )
  !   - if NOT IsConvergedFlag then
  !     - exit loop with error indicating no "global" convergence with final solution.
  !
  ! Check the subroutines SolveAirLoopControllers() and ReSolveAirLoopControllers()
  ! invoked in the subroutine SimAirLoop() for the actual calling sequences.
  !

  ! REFERENCES:
  ! na

  ! OTHER NOTES:
  ! To enable runtime statistics tracking for each air loop, define the environment variable
  ! TRACK_AIRLOOP=YES or TRACK_AIRLOOP=Y.
  !
  ! To enable generating a trace file with the converged solution for all controllers on each air loop,
  ! define the environment variable TRACE_AIRLOOP=YES or TRACE_AIRLOOP=Y.
  !
  ! To enable generating an individual, detailed trace file for each controller, define the
  ! environment variable TRACE_CONTROLLER=YES or TRACE_CONTROLLER=Y.
  !
  ! See DataSystemVariables.f90 for the definitions of the environment variables used to debug
  ! the air loop simulation.
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals, ONLY: SmallWaterVolFlow, SetPointErrorFlag
USE DataHVACControllers
USE DataRootFinder
USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables
PRIVATE

! MODULE PARAMETER DEFINITIONS
  ! Number of significant digits to display in error messages for floating-point numbers
  REAL(r64), PARAMETER    ::  SomeFloatingPoint  = 1.0d0
  INTEGER, PARAMETER ::  NumSigDigits   = PRECISION(SomeFloatingPoint)

  ! Parameters for controls used here
  INTEGER, PARAMETER :: iNoControlVariable           = 0
  INTEGER, PUBLIC, PARAMETER :: iTemperature         = 1
  INTEGER, PUBLIC, PARAMETER :: iHumidityRatio       = 2
  INTEGER, PUBLIC, PARAMETER :: iTemperatureAndHumidityRatio = 3
  INTEGER, PARAMETER :: iFlow                        = 4

  INTEGER, PARAMETER :: CoilType_Cooling=1
  INTEGER, PARAMETER :: CoilType_Heating=2

  CHARACTER(LEN=*), PARAMETER, DIMENSION(0:4) :: ControlVariableTypes =  &
                     (/'No control variable           ',  &
                       'Temperature                   ',  &
                       'Humidity ratio                ',  &
                       'Temperature and humidity ratio',  &
                       'Flow rate                     '/)


! DERIVED TYPE DEFINITIONS
  TYPE SolutionTrackerType
    LOGICAL      :: DefinedFlag   = .TRUE.    ! Flag set to TRUE when tracker is up-to-date. FALSE otherwise.
    REAL(r64)    :: ActuatedValue = 0.0d0       ! Actuated value
    INTEGER      :: Mode          = iModeNone ! Operational model of controller
  END TYPE SolutionTrackerType

  ! Type describing a controller's properties
  TYPE ControllerPropsType
    CHARACTER(LEN=MaxNameLength) :: ControllerName = ' '  ! Name of the Controller
    CHARACTER(LEN=MaxNameLength) :: ControllerType = ' '  ! Type of Controller
    INTEGER      :: ControllerType_Num = ControllerSimple_Type
    INTEGER      :: ControlVar = iNoControlVariable       ! The type of control variable being sensed
    INTEGER      :: ActuatorVar = 0                       ! The variable that the controller will act on ie. flow
    INTEGER      :: Action = iNoAction                    ! Controller Action - Reverse or Normal

    ! Controller must be initialized to set MinActuated and MaxActuated
    LOGICAL      :: InitFirstPass = .TRUE.

    ! --------------------
    ! Internal data used for optimal restart across successive calls to SimAirLoop()
    ! --------------------
    INTEGER      :: NumCalcCalls = 0  ! Number of Calc() calls since last call to Reset()
    INTEGER      :: Mode = iModeNone  ! Operational model of controller at current iteration

    ! Flag indicating whether the current controller simulation was performed from a cold start
    ! or following a speculative warm restart. Set in the ResetController() routine.
    ! Used in the CheckController() routine.
    LOGICAL      :: DoWarmRestartFlag              = .FALSE.
    ! Flag used to decide whether or not it is allowed to reuse the intermediate solution from
    ! solving the previous controller on the air loop (COLD_START mode only) as the initial guess for
    ! the current controller.
    LOGICAL      :: ReuseIntermediateSolutionFlag  = .FALSE.
    ! Flag used to decide whether or not it is possible to reuse the solution from
    ! the last call to SimAirLoop() as a possible candidate.
    LOGICAL      :: ReusePreviousSolutionFlag      = .FALSE.
    ! Array of solution trackers. Saved at last call to SimAirLoop() in ManageControllers(iControllerOpEnd)
    ! The first tracker is used to track the solution when FirstHVACIteration is TRUE.
    ! The second tracker is used to track the solution at FirstHVACIteration is FALSE.
    TYPE(SolutionTrackerType), DIMENSION(2) :: SolutionTrackers

    ! --------------------
    ! Operational limits at min/max avail values for actuated variable and the corresponding sensed values
    ! --------------------
    REAL(r64)    :: MaxAvailActuated = 0.0d0 ! kg/s, The maximum actuated variable currently available.
                                           ! Reset by simulation at each HVAC iteration
    REAL(r64)    :: MaxAvailSensed   = 0.0d0 ! Sensed value at maximum available actuated variable
    REAL(r64)    :: MinAvailActuated = 0.0d0 ! kg/s, The minimum actuated variable currently available.
                                           ! Reset by simulation at each HVAC iteration
    REAL(r64)    :: MinAvailSensed   = 0.0d0 ! Sensed value at maximum available actuated variable

    ! --------------------
    ! User input min/max values for actuated variable
    ! --------------------
    REAL(r64)    :: MaxVolFlowActuated = 0.0d0 ! m3/s, From User input the Max amount for the actuated variable
    REAL(r64)    :: MinVolFlowActuated = 0.0d0 ! m3/s, From User input the Min amount for the actuated variable
    REAL(r64)    :: MaxActuated = 0.0d0  ! kg/s, From User input the Max amount for the actuated variable
    REAL(r64)    :: MinActuated = 0.0d0  ! kg/s, From User input the Min amount for the actuated variable

    ! --------------------
    ! Actuated variable
    ! --------------------
    INTEGER      :: ActuatedNode       = 0   ! The node that is acted upon by the controller
    REAL(r64)    :: ActuatedValue      = 0.0d0 ! Value of actuated variable before change by the controller
    REAL(r64)    :: NextActuatedValue  = 0.0d0 ! The new control actuated value
    INTEGER      :: ActuatedNodePlantLoopNum  = 0 ! the plant loop index for the actuated node DSU3
    INTEGER      :: ActuatedNodePlantLoopSide = 0 ! the plant loop side for the actuated node DSU3
    INTEGER      :: ActuatedNodePlantLoopBranchNum = 0 ! the plant loop branch num for actuated node DSU3

    ! --------------------
    ! Sensed variable
    ! --------------------
    INTEGER      :: SensedNode = 0    ! The sensed node number from the grid
    LOGICAL      :: IsSetPointDefinedFlag = .FALSE. ! If TRUE indicates that the setpoint has been defined and can
                                                    ! be used to compute DeltaSensed
    REAL(r64)    :: SetPointValue = 0.0d0 ! Desired setpoint; set in the SetPoint Manager or computed in Init() routine
    REAL(r64)    :: SensedValue = 0.0d0 ! The sensed control variable of any type
    REAL(r64)    :: DeltaSensed = 0.0d0 ! Difference of sensed to setpoint value for calculating proportional gain
    REAL(r64)    :: Offset = 0.0d0      ! This is the tolerance or droop from the error

    ! --------------------
    ! Other controller inputs, not yet used
    ! --------------------
    CHARACTER(LEN=MaxNameLength) :: LimitType = ' '  ! Limit type as in HIGH or LOW
    REAL(r64)    :: Range = 0.0d0  ! The range or hysteresis of the control limit
    REAL(r64)    :: Limit = 0.0d0  ! The Limit value for a Limit Controller

    ! --------------------
    ! Trace mechanism
    ! --------------------
    INTEGER      :: TraceFileUnit    = 0    ! File unit for individual controller trace file to use if > 0
    LOGICAL      :: FirstTraceFlag = .TRUE. ! To detect first individual write operation to individual controller trace file
    INTEGER      :: BadActionErrCount = 0   ! Counts number of incorrect action errors
    INTEGER      :: BadActionErrIndex = 0   ! index to recurring error structure for bad action error
  END TYPE ControllerPropsType


  ! Type describing a controller's runtime statistics over the course of the simulation
  TYPE ControllerStatsType
    INTEGER, DIMENSION(iFirstMode:iLastMode)  :: NumCalls = 0      ! Number of times this controller operated in each mode
    INTEGER, DIMENSION(iFirstMode:iLastMode)  :: TotIterations = 0 ! Total number of iterations required to solve this controller
    INTEGER, DIMENSION(iFirstMode:iLastMode)  :: MaxIterations = 0 ! Maximum number of iterations required to solve this controller
  END TYPE ControllerStatsType

  ! Type describing an air loop's runtime statistics over the course of the simulation
  TYPE AirLoopStatsType
    INTEGER      :: TraceFileUnit                 ! File unit for trace file for all controllers on each air loop.
                                                  ! Used only if > 0. Same size as NumPrimaryAirSys
    LOGICAL      :: FirstTraceFlag = .TRUE.       ! To detect first trace to air loop trace file
    INTEGER      :: NumCalls = 0                  ! Number of times air loop is simulated (number of calls to SimAirLoop)
    INTEGER      :: NumFailedWarmRestarts = 0     ! Number of times speculative warm restart was attempted and failed
    INTEGER      :: NumSuccessfulWarmRestarts = 0 ! Number of times speculative warm restart was attempted and succeeded
    INTEGER      :: TotSimAirLoopComponents = 0   ! Total number of times the SimAirLoopComponents() routine has been invoked
    INTEGER      :: MaxSimAirLoopComponents = 0   ! Maximum number of times the SimAirLoopComponents() routine has been invoked
    INTEGER      :: TotIterations  = 0            ! Total number of iterations required to solve the controllers on this air loop
    INTEGER      :: MaxIterations  = 0            ! Maximum number of iterations required to solve the controllers on this air loop
    TYPE (ControllerStatsType), DIMENSION(:), ALLOCATABLE :: ControllerStats  ! Array of statistics for each controller
                                                                              ! on this air loop
  END TYPE AirLoopStatsType


! MODULE VARIABLE DECLARATIONS:
  INTEGER                                               :: NumControllers  = 0 ! The number of controllers found in the Input
  TYPE (ControllerPropsType), ALLOCATABLE, DIMENSION(:) :: ControllerProps
  TYPE (RootFinderDataType), ALLOCATABLE, DIMENSION(:)  :: RootFinders
  INTEGER                                               :: NumAirLoopStats = 0 ! Same size as NumPrimaryAirSys if controllers
                                                                               ! are defined, 0 otherwise.
  TYPE (AirLoopStatsType), ALLOCATABLE, DIMENSION(:)    :: AirLoopStats ! Statistics array to analyze computational profile for
                                                                               ! all controllers per air loop
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! Flag set to make sure you get input once
  LOGICAL                :: GetControllerInputFlag = .TRUE.

! SUBROUTINE Specifications for the Module
          ! Driver/Manager Routines
  PUBLIC  ManageControllers

          ! Get Input routines for module
  PRIVATE GetControllerInput

          ! Initialization routines for module
  PRIVATE InitController
  PRIVATE SizeController
  PRIVATE ResetController

          ! Algorithms for the module
  PRIVATE LimitController
  PRIVATE ExitCalcController

          ! Update routine to check convergence and update nodes
  PRIVATE UpdateController

          ! Reporting routines for module
  PRIVATE ReportController

          ! Algorithms for the Simple Controller
  PRIVATE FindRootSimpleController
  PRIVATE CalcSimpleController
  PRIVATE CheckSimpleController
  PRIVATE CheckMinActiveController
  PRIVATE CheckMaxActiveController
  PRIVATE SaveSimpleController

          ! Statistics routines
  PUBLIC  TrackAirLoopControllers
  PRIVATE TrackAirLoopController
  PUBLIC  DumpAirLoopStatistics
  PRIVATE WriteAirLoopStatistics

          ! Trace routines for all controllers on each air loop
  PUBLIC  TraceAirLoopControllers
  PRIVATE SetupAirLoopControllersTracer
  PRIVATE TraceIterationStamp
  PRIVATE TraceAirLoopController

          ! Trace routines for each individual controller
  PRIVATE TraceIndividualController
  PRIVATE SetupIndividualControllerTracer

          ! Misc routines
  PUBLIC  CreateHVACTimeString
  PUBLIC  MakeHVACTimeIntervalString
  PUBLIC  CreateHVACStepFullString
  PRIVATE CheckControllerListOrder
  PUBLIC  CheckCoilWaterInletNode
  PUBLIC  GetControllerActuatorNodeNum

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************

SUBROUTINE ManageControllers( &
    ControllerName, ControllerIndex, &
    FirstHVACIteration, AirLoopNum, AirLoopPass, &
    Operation, IsConvergedFlag, IsUpToDateFlag, AllowWarmRestartFlag )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   July 1998
          !       MODIFIED       Dimitri Curtil, February 2006
          !                      - Added air loop information
          !                      - Added tracing to csv files
          !                      - Added primitive operations to replace mixed
          !                        bag of ResetController, FirstCallConvergenceTest, ...
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Controller component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSystemVariables
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE DataPlant,      ONLY: PlantLoop, FlowLocked

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(LEN=*), INTENT(IN)    :: ControllerName
  INTEGER, INTENT(INOUT)          :: ControllerIndex
  ! TRUE if first full HVAC iteration in an HVAC time step
  LOGICAL, INTENT(IN)             :: FirstHVACIteration
  ! Current air loop num 1...NumPrimaryAirSys
  INTEGER, INTENT(IN)             :: AirLoopNum !unused1208
  ! Current pass counter in SimAirLoop()
  INTEGER, INTENT(IN)             :: AirLoopPass
  ! Operation to execute
  INTEGER, INTENT(IN)             :: Operation
  ! TRUE if controller is converged
  LOGICAL, INTENT(OUT)            :: IsConvergedFlag
  ! TRUE if air loop is up-to-date meaning that the current node values are consistent (air loop evaluated)
  ! Only used within the Calc routines
  LOGICAL, INTENT(INOUT)          :: IsUpToDateFlag
  ! TRUE if speculative warm restart is supported by this controller
  LOGICAL, INTENT(OUT), OPTIONAL  :: AllowWarmRestartFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  ! The Controller that you are currently loading input into
  INTEGER                      :: ControlNum
  INTEGER                      :: ControllerType

          ! FLOW:

  ! Obtains and Allocates Controller related parameters from input file
  IF (GetControllerInputFlag) THEN  !First time subroutine has been entered
    CALL GetControllerInput
    GetControllerInputFlag = .FALSE.
  END IF

  IF (ControllerIndex == 0) THEN
    ControlNum = FindItemInList(ControllerName, ControllerProps%ControllerName, NumControllers)
    IF (ControlNum == 0) THEN
      CALL ShowFatalError( &
        'ManageControllers: Invalid controller='//TRIM(ControllerName)// &
        '. The only valid controller type'//  &
        ' for an AirLoopHVAC is Controller:WaterCoil.' &
      )
    ENDIF
    ControllerIndex = ControlNum
  ELSE
    ControlNum = ControllerIndex
    IF (ControlNum > NumControllers .OR. ControlNum < 1) THEN
      CALL ShowFatalError( &
        'ManageControllers: Invalid ControllerIndex passed='// &
        TRIM(TrimSigDigits(ControlNum))// &
        ', Number of controllers='//TRIM(TrimSigDigits(NumControllers))//  &
        ', Controller name='//TRIM(ControllerName) &
      )
    ENDIF
    IF (CheckEquipName(ControlNum)) THEN
      IF (ControllerName /= ControllerProps(ControlNum)%ControllerName) THEN
        CALL ShowFatalError( &
          'ManageControllers: Invalid ControllerIndex passed='// &
          TRIM(TrimSigDigits(ControlNum))// &
          ', Controller name='//TRIM(ControllerName)// &
          ', stored Controller Name for that index='//  &
          TRIM(ControllerProps(ControlNum)%ControllerName) &
        )
      ENDIF
      CheckEquipName(ControlNum)=.false.
    ENDIF
  ENDIF
  ! Find the correct ControllerNumber with the AirLoop & CompNum from AirLoop Derived Type
  !ControlNum = AirLoopEquip(AirLoopNum)%ComponentOfTypeNum(CompNum)

  ! detect if plant is locked and flow cannot change
  IF (ControllerProps(ControlNum)%ActuatedNodePlantLoopNum > 0) THEN

    IF (PlantLoop(ControllerProps(ControlNum)%ActuatedNodePlantLoopNum)% &
       LoopSide(ControllerProps(ControlNum)%ActuatedNodePlantLoopSide)%Flowlock == FlowLocked) THEN
    ! plant is rigid so controller cannot change anything.
           ! Update the current Controller to the outlet nodes
      CALL UpdateController(ControlNum)

      IsConvergedFlag = .TRUE.
      RETURN
    ENDIF

  ENDIF

  ! Detect if speculative warm restart is supported by this computer
  IF ( PRESENT(AllowWarmRestartFlag) ) THEN
    ! NOTE: Never allow speculative warm restart with dual humidity ratio and temperature control
    !       because the actual setpoint depends on the current temperature and max hum ratio at
    !       the sensed node, and therefore might not be known until after one air loop simulation.
    IF ( ControllerProps(ControlNum)%ControlVar == iTemperatureAndHumidityRatio ) THEN
      AllowWarmRestartFlag = .FALSE.
    ELSE
      AllowWarmRestartFlag = .TRUE.
    END IF
  END IF

  IF(ControllerProps(ControlNum)%InitFirstPass)THEN
    ! Coil must first be sized to:
    ! Initialize ControllerProps(ControlNum)%MinActuated and ControllerProps(ControlNum)%MaxActuated
    CALL InitController(ControlNum, FirstHVACIteration, IsConvergedFlag)
    ControllerProps(ControlNum)%InitFirstPass = .FALSE.
  END IF

  ! Perform requested operation
  ! Note that InitController() is not called upon START/RESTART ops in order to avoid
  ! side-effects on the calculation of Node(ActuatedNode)%MassFlowRateMaxAvail used to
  ! determine ControllerProps(ControlNum)%MaxAvailActuated.
  ! Plant upgrades for V7 added init to these cases because MassFlowRateMaxAvail is better controlled
  ControllerOp : SELECT CASE (Operation)
    CASE (iControllerOpColdStart)
      ! If a iControllerOpColdStart call, reset the actuator inlet flows
      CALL ResetController(ControlNum, FirstHVACIteration, .FALSE., IsConvergedFlag)
  !    CALL InitController(ControlNum, FirstHVACIteration, IsConvergedFlag)
      ! Update the current Controller to the outlet nodes
      CALL UpdateController(ControlNum)

      ! Report the current Controller
      CALL ReportController(ControlNum)


    CASE (iControllerOpWarmRestart)
      ! If a iControllerOpWarmRestart call, set the actuator inlet flows to previous solution
      CALL ResetController(ControlNum, FirstHVACIteration,.TRUE., IsConvergedFlag)
   !   CALL InitController(ControlNum, FirstHVACIteration, IsConvergedFlag)
      ! Update the current Controller to the outlet nodes
      CALL UpdateController(ControlNum)

      ! Report the current Controller
      CALL ReportController(ControlNum)


    CASE (iControllerOpIterate)
      ! With the correct ControlNum Initialize all Controller related parameters
      CALL InitController(ControlNum, FirstHVACIteration, IsConvergedFlag)

      ! No initialization needed: should have been done before
      ! Simulate the correct Controller with the current ControlNum
      ControllerType = ControllerProps(ControlNum)%ControllerType_Num

      ControllerCalc: SELECT CASE (ControllerType)
      CASE (ControllerSimple_Type)  ! 'Controller:WaterCoil'
          CALL CalcSimpleController(ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName)
        CASE DEFAULT
          CALL ShowFatalError( &
            'Invalid controller type in ManageControllers='// &
            TRIM(ControllerProps(ControlNum)%ControllerType) &
          )
      END SELECT ControllerCalc

      ! Update the current Controller to the outlet nodes
      CALL UpdateController(ControlNum)

      ! Report the current Controller
      CALL ReportController(ControlNum)


    CASE (iControllerOpEnd)
      ! With the correct ControlNum Initialize all Controller related parameters
      CALL InitController(ControlNum, FirstHVACIteration, IsConvergedFlag)

      ! No initialization needed: should have been done before
      ! Check convergence for the correct Controller with the current ControlNum
      ControllerType = ControllerProps(ControlNum)%ControllerType_Num

      ControllerCheck: SELECT CASE (ControllerType)
        CASE (ControllerSimple_Type)  ! 'Controller:WaterCoil'
          CALL CheckSimpleController(ControlNum, IsConvergedFlag)
          CALL SaveSimpleController(ControlNum, FirstHVACIteration, IsConvergedFlag)

        CASE DEFAULT
          CALL ShowFatalError( &
            'Invalid controller type in ManageControllers='// &
            TRIM(ControllerProps(ControlNum)%ControllerType) &
          )
      END SELECT ControllerCheck

      ! Report the current Controller
      CALL ReportController(ControlNum)

    CASE DEFAULT
      CALL ShowFatalError( &
        'ManageControllers: Invalid Operation passed='//TRIM(TrimSigDigits(Operation))// &
        ', Controller name='//TRIM(ControllerName) &
      )
  END SELECT ControllerOp


  ! Write detailed diagnostic for individual controller
  !
  ! To enable generating an individual, detailed trace file for each controller on each air loop,
  ! define the environment variable TRACE_CONTROLLER=YES or TRACE_CONTROLLER=Y
  IF ( TraceHVACControllerEnvFlag ) THEN
    CALL TraceIndividualController( &
      ControlNum, &
      FirstHVACIteration, &
      AirLoopPass, &
      Operation, &
      IsConvergedFlag )
  END IF

  RETURN
END SUBROUTINE ManageControllers


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetControllerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   July 1998
          !       MODIFIED       February 2006, Dimitri Curtil
          !                      - Added processing for air loop controller stats
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! Gets the object:
          ! Controller:WaterCoil,
          !   \min-fields 9
          !   A1 , \field Name
          !        \type alpha
          !        \required-field
          !        \reference AirLoopControllers
          !   A2 , \field Control Variable
          !        \type choice
          !        \key Temperature
          !        \key HumidityRatio
          !        \key TemperatureAndHumidityRatio
          !        \key Flow
          !        \note TemperatureAndHumidityRatio requires a SetpointManager:SingleZone:Humidity:Maximum object
          !   A3 , \field Action
          !        \type choice
          !        \key Normal
          !        \key Reverse
          !   A4 , \field Actuator Variable
          !        \type choice
          !        \key Flow
          !   A5 , \field Sensor Node Name
          !        \type alpha
          !   A6 , \field Actuator Node Name
          !        \type alpha
          !   N1 , \field Controller Convergence Tolerance
          !        \units deltaC
          !        \type real
          !        \default Autosize
          !        \autosizable
          !   N2 , \field Maximum Actuated Flow
          !        \type real
          !        \units m3/s
          !        \autosizable
          !   N3 ; \field Minimum Actuated Flow
          !        \type real
          !        \default 0.0000001
          !        \units m3/s


          ! USE STATEMENTS:
  USE DataSystemVariables, ONLY : TrackAirLoopEnvFlag, TraceAirLoopEnvFlag, TraceHVACControllerEnvFlag
  USE InputProcessor,      ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs, SameString, MakeUPPERCase
  USE NodeInputManager,    ONLY : GetOnlySingleNode
  USE DataHVACGlobals,     ONLY : NumPrimaryAirSys
  USE DataAirSystems,      ONLY : PrimaryAirSystem
  USE WaterCoils,          ONLY : CheckActuatorNode, CheckForSensorAndSetpointNode
  USE MixedAir,            ONLY : CheckForControllerWaterCoil
  USE SetPointManager,     ONLY : NodeHasSPMCtrlVarType, iCtrlVarType_Temp, iCtrlVarType_MaxHumRat
  USE EMSManager,          ONLY : CheckIfNodeSetpointManagedByEMS, iTemperatureSetpoint, iHumidityRatioMaxSetpoint

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='HVACControllers: GetControllerInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: Num      ! The Controller that you are currently loading input into
  INTEGER       :: NumSimpleControllers
  INTEGER       :: NumAlphas
  INTEGER       :: NumNums
  INTEGER       :: NumArgs
  INTEGER       :: IOSTAT
  INTEGER       :: AirLoopNum            ! DO index for each air loop
  LOGICAL       :: ActuatorNodeNotFound  ! true if no water coil inlet node match for actuator node
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray
  CHARACTER(LEN=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  CHARACTER(LEN=MaxNameLength)  :: CurrentModuleObject      ! for ease in getting objects
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  LOGICAL       :: ErrorsFound=.FALSE.
  INTEGER       :: iNodeType  ! for checking actuator node type
  INTEGER       :: WaterCoilNum          ! water coil index
  LOGICAL       :: NodeNotFound          ! flag true if the sensor node is on the coil air outlet node
  LOGICAL       :: NotHeatingCoil        ! flag true if the water coil is a cooling coil
  LOGICAL       :: EMSSetpointErrorFlag  ! flag true is EMS is used to set node setpoints

  ! All the controllers are loaded into the same derived type, both the PI and Limit
  ! These controllers are separate objects and loaded sequentially, but will
  ! be retrieved by name as they are needed.

  CurrentModuleObject = 'Controller:WaterCoil'
  NumSimpleControllers = GetNumObjectsFound(CurrentModuleObject)
  NumControllers = NumSimpleControllers

  ! Allocate stats data structure for each air loop and controller if needed
  IF ( TrackAirLoopEnvFlag .OR. TraceAirLoopEnvFlag .OR. TraceHVACControllerEnvFlag ) THEN
    IF ( NumPrimaryAirSys > 0 ) THEN
      NumAirLoopStats = NumPrimaryAirSys
      ALLOCATE(AirLoopStats(NumAirLoopStats))

      ! Allocate controller statistics data for each controller on each air loop
      DO AirLoopNum=1,NumPrimaryAirSys
        ALLOCATE(AirLoopStats(AirLoopNum)%ControllerStats(PrimaryAirSystem(AirLoopNum)%NumControllers))
      END DO
    END IF
  END IF


  IF (NumControllers == 0) RETURN
  ! Condition of no controllers will be taken care of elsewhere, if necessary

  ALLOCATE(ControllerProps(NumControllers))
  ALLOCATE(RootFinders(NumControllers))
  ALLOCATE(CheckEquipName(NumControllers))
  CheckEquipName=.true.

  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumArgs,NumAlphas,NumNums)
  ALLOCATE(AlphArray(NumAlphas))
  AlphArray=' '
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNums))
  cNumericFields=' '
  ALLOCATE(NumArray(NumNums))
  NumArray=0.0d0
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNums))
  lNumericBlanks=.true.

  ! Now find and load all of the simple controllers.
  IF (NumSimpleControllers .GT. 0) THEN
    DO Num = 1, NumSimpleControllers
      CALL GetObjectItem(CurrentModuleObject,Num,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(AlphArray(1),ControllerProps%ControllerName,Num-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      ControllerProps(Num)%ControllerName = AlphArray(1)
      ControllerProps(Num)%ControllerType = TRIM(CurrentModuleObject)
      SELECT CASE (AlphArray(2))
        CASE ('TEMPERATURE')
          ControllerProps(Num)%ControlVar  = iTemperature
        CASE ('HUMIDITYRATIO')
          ControllerProps(Num)%ControlVar  = iHumidityRatio
        CASE ('TEMPERATUREANDHUMIDITYRATIO')
          ControllerProps(Num)%ControlVar  = iTemperatureAndHumidityRatio
!        CASE ('FLOW')
!          ControllerProps(Num)%ControlVar  = iFlow
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(AlphArray(1))//'".')
          CALL ShowSevereError('...Invalid '//TRIM(cAlphaFields(2))//'="'//TRIM(AlphArray(2))//  &
             '", must be Temperature, HumidityRatio, or TemperatureAndHumidityRatio.')
          ErrorsFound=.true.
      END SELECT
      IF (SameString(AlphArray(3) , 'Normal')) THEN
        ControllerProps(Num)%Action = iNormalAction
      ELSEIF  (SameString(AlphArray(3) , 'Reverse')) THEN
        ControllerProps(Num)%Action = iReverseAction
      ELSEIF  (lAlphaBlanks(3)) THEN
        ControllerProps(Num)%Action = 0
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
           TRIM(AlphArray(1))//'".')
        CALL ShowSevereError('...Invalid '//TRIM(cAlphaFields(3))//'="'//TRIM(AlphArray(3))//  &
           '", must be "Normal", "Reverse" or blank.')
        ErrorsFound=.true.
      ENDIF
      IF (AlphArray(4) == 'FLOW') THEN
        ControllerProps(Num)%ActuatorVar = iFlow
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
           TRIM(AlphArray(1))//'".')
        CALL ShowContinueError('...Invalid '//TRIM(cAlphaFields(4))//'="'//TRIM(AlphArray(4))//  &
           '", only FLOW is allowed.')
        ErrorsFound=.true.
      ENDIF
      ControllerProps(Num)%SensedNode    = &
           GetOnlySingleNode(AlphArray(5),ErrorsFound,CurrentModuleObject,AlphArray(1),  &
                        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      ControllerProps(Num)%ActuatedNode  = &
           GetOnlySingleNode(AlphArray(6),ErrorsFound,CurrentModuleObject,AlphArray(1),  &
                        NodeType_Unknown,NodeConnectionType_Actuator,1,ObjectIsNotParent)
      ControllerProps(Num)%Offset             = NumArray(1)
      ControllerProps(Num)%MaxVolFlowActuated = NumArray(2)
      ControllerProps(Num)%MinVolFlowActuated = NumArray(3)

      IF (.not. CheckForControllerWaterCoil(CurrentModuleObject,AlphArray(1))) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
           TRIM(AlphArray(1))//'" not found on any AirLoopHVAC:ControllerList.')
        ErrorsFound = .TRUE.
      ENDIF

      IF ( ControllerProps(Num)%SensedNode > 0) THEN
        CALL CheckForSensorAndSetpointNode(ControllerProps(Num)%SensedNode,ControllerProps(Num)%ControlVar,NodeNotFound)

        IF (NodeNotFound) THEN
            ! the sensor node is not on the water coil air outlet node
            CALL ShowWarningError(RoutineName//TRIM(ControllerProps(Num)%ControllerType)//'="'//  &
                                    TRIM(ControllerProps(Num)%ControllerName)//'". ')
            CALL ShowContinueError(' ..Sensor node not found on water coil air outlet node.')
            CALL ShowContinueError(' ..The sensor node may have been placed on a node downstream of the coil'// &
                                    ' or on an airloop outlet node.')
        ELSE
            ! check if the setpoint is also on the same node where the sensor is placed on
            EMSSetpointErrorFlag = .FALSE.
            SELECT CASE (ControllerProps(Num)%ControlVar)
            CASE (iTemperature)
                CALL CheckIfNodeSetpointManagedByEMS(ControllerProps(Num)%SensedNode,iTemperatureSetpoint, EMSSetpointErrorFlag)
                IF (EMSSetpointErrorFlag) THEN
                    IF(.NOT. NodeHasSPMCtrlVarType(ControllerProps(Num)%SensedNode, iCtrlVarType_Temp)) THEN
                    CALL ShowContinueError(' ..Temperature setpoint not found on coil air outlet node.')
                    CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                            ' or on an airloop outlet node.')
                    CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
                    END IF
                ENDIF
            CASE (iHumidityRatio)
                CALL CheckIfNodeSetpointManagedByEMS(ControllerProps(Num)%SensedNode,iHumidityRatioMaxSetpoint,   &
                   EMSSetpointErrorFlag)
                IF (EMSSetpointErrorFlag) THEN
                    IF(.NOT. NodeHasSPMCtrlVarType(ControllerProps(Num)%SensedNode, iCtrlVarType_MaxHumRat)) THEN
                    CALL ShowContinueError(' ..Humidity ratio setpoint not found on coil air outlet node.')
                    CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                            ' or on an airloop outlet node.')
                    CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
                    END IF
                ENDIF
            CASE (iTemperatureAndHumidityRatio)
                CALL CheckIfNodeSetpointManagedByEMS(ControllerProps(Num)%SensedNode,iTemperatureSetpoint, EMSSetpointErrorFlag)
                IF (EMSSetpointErrorFlag) THEN
                    IF(.NOT. NodeHasSPMCtrlVarType(ControllerProps(Num)%SensedNode, iCtrlVarType_Temp)) THEN
                    CALL ShowContinueError(' ..Temperature setpoint not found on coil air outlet node.')
                    CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                            ' or on an airloop outlet node.')
                    CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
                    END IF
                ENDIF
                EMSSetpointErrorFlag = .FALSE.
                CALL CheckIfNodeSetpointManagedByEMS(ControllerProps(Num)%SensedNode,iHumidityRatioMaxSetpoint,   &
                   EMSSetpointErrorFlag)
                IF (EMSSetpointErrorFlag) THEN
                    IF(.NOT. NodeHasSPMCtrlVarType(ControllerProps(Num)%SensedNode, iCtrlVarType_MaxHumRat)) THEN
                    CALL ShowContinueError(' ..Humidity ratio setpoint not found on coil air outlet node.')
                    CALL ShowContinueError(' ..The setpoint may have been placed on a node downstream of the coil'// &
                                            ' or on an airloop outlet node.')
                    CALL ShowContinueError(' ..Specify the setpoint and the sensor on the coil air outlet node when possible.')
                    END IF
                ENDIF
            END SELECT
        ENDIF
      ENDIF
    END DO
  END IF

 ! check that actuator nodes are matched by a water coil inlet node
  DO Num = 1, NumSimpleControllers
    CALL CheckActuatorNode(ControllerProps(Num)%ActuatedNode, iNodeType, ActuatorNodeNotFound)
    IF (ActuatorNodeNotFound) THEN
      ErrorsFound=.true.
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
         TRIM(ControllerProps(Num)%ControllerName)//'":')
      CALL ShowContinueError('...the actuator node must also be a water inlet node of a water coil')
    ELSE  ! Node found, check type and action
      IF (iNodeType == CoilType_Cooling) THEN
        IF (ControllerProps(Num)%Action == 0) THEN
          ControllerProps(Num)%Action = iReverseAction
        ELSEIF (ControllerProps(Num)%Action == iNormalAction) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(ControllerProps(Num)%ControllerName)//'":')
          CALL ShowContinueError('...Normal action has been specified for a cooling coil - should be Reverse.')
          CALL ShowContinueError('...overriding user input action with Reverse Action.')
          ControllerProps(Num)%Action = iReverseAction
        ENDIF
      ELSEIF (iNodeType == CoilType_Heating) THEN
        IF (ControllerProps(Num)%Action == 0) THEN
          ControllerProps(Num)%Action = iNormalAction
        ELSEIF (ControllerProps(Num)%Action == iReverseAction) THEN
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(ControllerProps(Num)%ControllerName)//'":')
          CALL ShowContinueError('...Reverse action has been specified for a heating coil - should be Normal.')
          CALL ShowContinueError('...overriding user input action with Normal Action.')
          ControllerProps(Num)%Action = iNormalAction
        ENDIF
      END IF
    END IF
  END DO

  DEALLOCATE(AlphArray)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(NumArray)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)


  !CR 8253 check that the sensed nodes in the controllers are in flow order in controller List
  CALL CheckControllerListOrder


  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input.')
  ENDIF

  RETURN
END SUBROUTINE GetControllerInput

! End of Get Input subroutines for the Module
!******************************************************************************


! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE ResetController(ControlNum, FirstHVACIteration, DoWarmRestartFlag, IsConvergedFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2004
          !       MODIFIED       Dimitri Curtil (LBNL), Feb 2006
          !                      - Added capability for speculative warm restart
          !                      Brent Griffith (NREL), Feb 2010
          !                      - use SetActuatedBranchFlowRate in Plant Utilities (honor hardware min > 0.0)
          !                      - add FirstHVACIteration logic, don't reset if false,
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine resets the actuator inlet flows.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY : SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN)       :: ControlNum
  LOGICAL, INTENT(IN)       :: FirstHVACIteration
  LOGICAL, INTENT(IN)       :: DoWarmRestartFlag
  LOGICAL, INTENT(OUT)      :: IsConvergedFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ActuatedNode
  INTEGER             :: SensedNode
  REAL(r64)           :: NoFlowResetValue

  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode

  ! Set again in ReportController() to ControllerProps(ControlNum)%NextActuatedValue
!  IF (FirstHVACIteration) THEN
!DSU3    Node(ActuatedNode)%MassFlowRate = 0.0d0
    NoFlowResetValue = 0.0d0
    CALL SetActuatedBranchFlowRate(NoFlowResetValue,  &
                         ControllerProps(ControlNum)%ActuatedNode ,      &
                         ControllerProps(ControlNum)%ActuatedNodePlantLoopNum, &
                         ControllerProps(ControlNum)%ActuatedNodePlantLoopSide, &
                         ControllerProps(ControlNum)%ActuatedNodePlantLoopBranchNum, &
                         .TRUE.)

!  ENDIF

  ! Reset iteration counter and internal variables
  ControllerProps(ControlNum)%NumCalcCalls = 0

  ControllerProps(ControlNum)%DeltaSensed   = 0.0d0
  ControllerProps(ControlNum)%SensedValue   = 0.0d0
  ControllerProps(ControlNum)%ActuatedValue = 0.0d0

  ! Reset setpoint-related quantities
  ControllerProps(ControlNum)%SetPointValue = 0.0d0
  ControllerProps(ControlNum)%IsSetPointDefinedFlag = .FALSE.

  ! MinAvailActuated and MaxAvailActuated set in InitController()
  ControllerProps(ControlNum)%MinAvailActuated = 0.0d0
  ControllerProps(ControlNum)%MinAvailSensed   = 0.0d0
  ControllerProps(ControlNum)%MaxAvailActuated = 0.0d0
  ControllerProps(ControlNum)%MaxAvailSensed   = 0.0d0

  ! Restart from previous solution if speculative warm restart flag set
  ! Keep same mode and next actuated value unchanged from last controller simulation.
  IF ( DoWarmRestartFlag ) THEN
    ControllerProps(ControlNum)%DoWarmRestartFlag = .TRUE.
  ELSE
    ControllerProps(ControlNum)%DoWarmRestartFlag = .FALSE.
    ! If no speculative warm restart then reset stored mode and actucated value
    ControllerProps(ControlNum)%Mode = iModeNone
    ControllerProps(ControlNum)%NextActuatedValue = 0.0d0
  END IF

  ! Only set once per HVAC iteration.
  ! Might be overwritten in the InitController() routine.
  !
  ! Allow reusing the previous solution while identifying brackets if
  ! this is not the first HVAC step of the environment
  ControllerProps(ControlNum)%ReusePreviousSolutionFlag  = .TRUE.
  ! Always reset to false by default. Set in CalcSimpleController() on the first controller iteration.
  ControllerProps(ControlNum)%ReuseIntermediateSolutionFlag = .FALSE.
  ! By default not converged
  IsConvergedFlag = .FALSE.


  ! Reset root finder
  ! This is independent of the processing in InitializeRootFinder() performed in Calc() routine.
  RootFinders(ControlNum)%StatusFlag = iStatusNone
  RootFinders(ControlNum)%CurrentMethodType = iMethodNone

  RootFinders(ControlNum)%CurrentPoint%DefinedFlag = .FALSE.
  RootFinders(ControlNum)%CurrentPoint%X           = 0.0d0
  RootFinders(ControlNum)%CurrentPoint%Y           = 0.0d0

  RootFinders(ControlNum)%MinPoint%DefinedFlag   = .FALSE.
  RootFinders(ControlNum)%MaxPoint%DefinedFlag   = .FALSE.
  RootFinders(ControlNum)%LowerPoint%DefinedFlag = .FALSE.
  RootFinders(ControlNum)%UpperPoint%DefinedFlag = .FALSE.

  RETURN
END SUBROUTINE ResetController


SUBROUTINE InitController(ControlNum,FirstHVACIteration,IsConvergedFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   July 1998
          !       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
          !       MODIFIED       Dimitri Curtil (LBNL), Feb 2006
          !                      - Moved first call convergence test code to ResetController()
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations of the Controller Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics,   ONLY: PsyTdpFnWPb
  USE FluidProperties,  ONLY: GetDensityGlycol
  USE DataEnvironment,  ONLY: OutBaroPress
  USE DataHVACGlobals,  ONLY: DoSetPointTest
  USE RootFinder,       ONLY: SetupRootFinder
  USE EMSManager,       ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS , &
                              iHumidityRatioSetpoint, iHumidityRatioMaxSetpoint, iMassFlowRateSetpoint
  USE DataPlant,        ONLY: PlantLoop, ScanPlantLoopsForNodeNum
  USE PlantUtilities,   ONLY: SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: ControlNum
  LOGICAL, INTENT(INOUT)   :: IsConvergedFlag
  LOGICAL, INTENT(IN)      :: FirstHVACIteration ! TRUE if first full HVAC iteration in an HVAC timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ActuatedNode
  INTEGER             :: SensedNode
  INTEGER             :: ControllerIndex
  LOGICAL, SAVE       :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantIndexsFlag
  LOGICAL, SAVE       :: MySetPointCheckFlag = .TRUE.
  ! Supply Air Temp Setpoint when 'TemperatureAndHumidityRatio' control is used
  REAL(r64), SAVE          :: HumidityControlTempSetPoint
  ! Difference between SA dry-bulb and dew-point temperatures
  REAL(r64)           :: ApproachTemp
  ! Desired dew point temperature setpoint for 'TemperatureAndHumidityRatio' control
  REAL(r64)           :: DesiredDewPoint
  REAL(r64)           :: rho !local fluid density


  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumControllers))
    ALLOCATE(MySizeFlag(NumControllers))
    ALLOCATE(MyPlantIndexsFlag(NumControllers))
    MyEnvrnFlag = .TRUE.
    MySizeFlag  = .TRUE.
    MyPlantIndexsFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
    ! check for missing setpoints
    DO ControllerIndex=1,NumControllers
      SensedNode = ControllerProps(ControllerIndex)%SensedNode
      SELECT CASE(ControllerProps(ControllerIndex)%ControlVar)
        CASE(iTemperature) ! 'Temperature'
          IF (Node(SensedNode)%TempSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('HVACControllers: Missing temperature setpoint for controller type='//  &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'// &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
              CALL ShowContinueError('Node Referenced (by Controller)='//TRIM(NodeID(SensedNode)))
              CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "Temperature" to establish '//  &
                                     'a setpoint at the controller sensed node.')
              SetPointErrorFlag = .TRUE.
            ELSE
              ! call to check node is actuated by EMS
              CALL CheckIfNodeSetpointManagedByEMS(SensedNode,iTemperatureSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError ('HVACControllers: Missing temperature setpoint for controller type='//  &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'// &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
                CALL ShowContinueError('Node Referenced (by Controller)='//TRIM(NodeID(SensedNode)))
                CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "Temperature" to establish '//  &
                                     'a setpoint at the controller sensed node.')
                CALL ShowContinueError('Or add EMS Actuator to provide temperature setpoint at this node')
              ENDIF
            ENDIF
          ELSE
!           Warn if humidity setpoint is detected (only for cooling coils) and control varible is TEMP.
            IF (Node(SensedNode)%HumRatMax /= SensedNodeFlagValue .AND.   &
                           ControllerProps(ControllerIndex)%Action==iReverseAction) THEN
              CALL ShowWarningError('HVACControllers: controller type='//TRIM(ControllerProps(ControllerIndex)%ControllerType)// &
                                    ' Name="'//      &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"'//     &
                                    ' has detected a maximum humidity ratio setpoint at the control node.')
              CALL ShowContinueError('Node referenced (by controller)='//TRIM(NodeID(SensedNode)))
              CALL ShowContinueError('  set the controller control variable to TemperatureAndHumidityRatio' &
                                     //' if humidity control is desired.')
!              SetPointErrorFlag = .TRUE.
            END IF
          END IF
        CASE(iHumidityRatio)  ! 'HumidityRatio'
          IF (Node(SensedNode)%HumRatSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('HVACControllers: Missing humidity ratio setpoint for controller type=' // &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'//      &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
              CALL ShowContinueError('Node referenced (by controller)='//TRIM(NodeID(SensedNode)))
              CALL ShowContinueError('  use a SetpointManager with the field Control Variable = "HumidityRatio" to establish '//  &
                                     'a setpoint at the controller sensed node.')
              SetPointErrorFlag = .TRUE.
            ELSE
              CALL CheckIfNodeSetpointManagedByEMS(SensedNode,iHumidityRatioSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError('HVACControllers: Missing humidity ratio setpoint for controller type=' // &
                                      TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'//      &
                                      TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
                CALL ShowContinueError('Node referenced (by controller)='//TRIM(NodeID(SensedNode)))
                CALL ShowContinueError('  use a SetpointManager with the field Control Variable = '//  &
                   '"HumidityRatio" to establish a setpoint at the controller sensed node.')
                CALL ShowContinueError('Or add EMS Actuator to provide Humidity Ratio setpoint at this node')

              ENDIF
            ENDIF
          END IF
        CASE(iTemperatureAndHumidityRatio) ! 'TemperatureAndHumidityRatio'
          IF (Node(SensedNode)%TempSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('HVACControllers: Missing temperature setpoint for controller type='//  &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'// &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
              CALL ShowContinueError('Node Referenced (by Controller)='//TRIM(NodeID(SensedNode)))
              CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "Temperature" to establish '//  &
                                     'a setpoint at the controller sensed node.')
              SetPointErrorFlag = .TRUE.
            ELSE
              ! call to check node is actuated by EMS
              CALL CheckIfNodeSetpointManagedByEMS(SensedNode,iTemperatureSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError ('HVACControllers: Missing temperature setpoint for controller type='//  &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'// &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
                CALL ShowContinueError('Node Referenced (by Controller)='//TRIM(NodeID(SensedNode)))
                CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "Temperature" to establish '//  &
                                     'a setpoint at the controller sensed node.')
                CALL ShowContinueError('Or add EMS Actuator to provide temperature setpoint at this node')
              ENDIF
            ENDIF
          END IF
          IF (Node(SensedNode)%HumRatMax == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('HVACControllers: Missing maximum humidity ratio setpoint for controller type=' // &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'//      &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
              CALL ShowContinueError('Node Referenced (by Controller)='//TRIM(NodeID(SensedNode)))
              CALL ShowContinueError('  use a SetpointManager with the field Control Variable = '//  &
                 '"MaximumHumidityRatio" to establish a setpoint at the controller sensed node.')
              SetPointErrorFlag = .TRUE.
            ELSE
              ! call to check node is actuated by EMS
              CALL CheckIfNodeSetpointManagedByEMS(SensedNode,iHumidityRatioMaxSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError('HVACControllers: Missing maximum humidity ratio setpoint for controller type=' // &
                                      TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'//      &
                                      TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
                CALL ShowContinueError('Node Referenced (by Controller)='//TRIM(NodeID(SensedNode)))
                CALL ShowContinueError('  use a SetpointManager with the field Control Variable = '//  &
                   '"MaximumHumidityRatio" to establish a setpoint at the controller sensed node.')
                CALL ShowContinueError('Or add EMS Actuator to provide maximum Humidity Ratio setpoint at this node')
              ENDIF
            ENDIF
          END IF
        CASE(iFlow) ! 'Flow'
          IF (Node(SensedNode)%MassFlowRateSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('HVACControllers: Missing mass flow rate setpoint for controller type=' // &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'//      &
                                    TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
              CALL ShowContinueError('Node Referenced (in Controller)='//TRIM(NodeID(SensedNode)))
              CALL ShowContinueError('  use a SetpointManager with the field Control Variable = "MassFlowRate" to establish '//  &
                                     'a setpoint at the controller sensed node.')
              SetPointErrorFlag = .TRUE.
            ELSE
              ! call to check node is actuated by EMS
              CALL CheckIfNodeSetpointManagedByEMS(SensedNode,iMassFlowRateSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError('HVACControllers: Missing mass flow rate setpoint for controller type=' // &
                                      TRIM(ControllerProps(ControllerIndex)%ControllerType)//' Name="'//      &
                                      TRIM(ControllerProps(ControllerIndex)%ControllerName) // '"')
                CALL ShowContinueError('Node Referenced (in Controller)='//TRIM(NodeID(SensedNode)))
                CALL ShowContinueError('  use a SetpointManager with the field Control Variable = "MassFlowRate" to establish '//  &
                                       'a setpoint at the controller sensed node.')
                CALL ShowContinueError('Or add EMS Actuator to provide Mass Flow Rate setpoint at this node')
              ENDIF
            ENDIF
          END IF
      END SELECT
    END DO

    MySetPointCheckFlag = .FALSE.
  END IF

  IF (ALLOCATED(PlantLoop) .AND. MyPlantIndexsFlag(ControlNum)) THEN
    CALL  ScanPlantLoopsForNodeNum(ControllerProps(ControlNum)%ControllerName, &
                                   ControllerProps(ControlNum)%ActuatedNode, &
                                   ControllerProps(ControlNum)%ActuatedNodePlantLoopNum, &
                                   ControllerProps(ControlNum)%ActuatedNodePlantLoopSide, &
                                   ControllerProps(ControlNum)%ActuatedNodePlantLoopBranchNum)
    MyPlantIndexsFlag(ControlNum) = .FALSE.

  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(ControlNum)) THEN

    CALL SizeController(ControlNum)

    !Check to make sure that the Minimum Flow rate is less than the max.
    IF (ControllerProps(ControlNum)%MaxVolFlowActuated == 0.0d0) THEN
      ControllerProps(ControlNum)%MinVolFlowActuated = 0.0d0
    ELSE IF (ControllerProps(ControlNum)%MinVolFlowActuated .GE. ControllerProps(ControlNum)%MaxVolFlowActuated) THEN
      CALL ShowFatalError( &
        'Controller:WaterCoil, Minimum control flow is > or = Maximum control flow; '// &
        TRIM(ControllerProps(ControlNum)%ControllerName) &
      )
    END IF

    ! Setup root finder after sizing calculation
    SelectAction: SELECT CASE ( ControllerProps(ControlNum)%Action )
      CASE ( iNormalAction )
        CALL SetupRootFinder(                 &
          RootFinders(ControlNum),            &
          iSlopeIncreasing,                   & ! Slope type
          iMethodBrent,                       & ! Method type
          constant_zero,                      & ! TolX: no relative tolerance for X variables
          1.0d-6,                             & ! ATolX: absolute tolerance for X variables
          ControllerProps(ControlNum)%Offset  & ! ATolY: absolute tolerance for Y variables
        )

      CASE ( iReverseAction )
        CALL SetupRootFinder(                 &
          RootFinders(ControlNum),            &
          iSlopeDecreasing,                   & ! Slope type
          iMethodBrent,                       & ! Method type
          constant_zero,                      & ! TolX: no relative tolerance for X variables
          1.0d-6,                             & ! ATolX: absolute tolerance for X variables
          ControllerProps(ControlNum)%Offset  & ! ATolY: absolute tolerance for Y variables
        )
      CASE DEFAULT
        CALL ShowFatalError( &
          'InitController: Invalid controller action. '// &
          'Valid choices are "Normal" or "Reverse"' &
        )
    END SELECT SelectAction

    MySizeFlag(ControlNum) = .FALSE.
  END IF

  ! Set the sensed and actuated node numbers
  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(ControlNum)) THEN

    rho = GetDensityGlycol( PlantLoop(ControllerProps(ControlNum)%ActuatedNodePlantLoopNum)%FluidName,  &
                               InitConvTemp, &
                               PlantLoop(ControllerProps(ControlNum)%ActuatedNodePlantLoopNum)%FluidIndex,&
                               'InitController')

    ControllerProps(ControlNum)%MinActuated = rho*ControllerProps(ControlNum)%MinVolFlowActuated
    ControllerProps(ControlNum)%MaxActuated = rho*ControllerProps(ControlNum)%MaxVolFlowActuated

    ! Turn off scheme to reuse previous solution obtained at last SimAirLoop() call
    ControllerProps(ControlNum)%ReusePreviousSolutionFlag      = .FALSE.
    ! Reset solution trackers
    ControllerProps(ControlNum)%SolutionTrackers%DefinedFlag   = .FALSE.
    ControllerProps(ControlNum)%SolutionTrackers%Mode          = iModeNone
    ControllerProps(ControlNum)%SolutionTrackers%ActuatedValue = 0.0d0

    MyEnvrnFlag(ControlNum) = .FALSE.
  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ControlNum)=.TRUE.
  ENDIF

  Call SetActuatedBranchFlowRate(ControllerProps(ControlNum)%NextActuatedValue, &
                                       ActuatedNode, &
                                       ControllerProps(ControlNum)%ActuatedNodePlantLoopNum,&
                                       ControllerProps(ControlNum)%ActuatedNodePlantLoopSide, &
                                       ControllerProps(ControlNum)%ActuatedNodePlantLoopBranchNum, &
                                       .FALSE.)

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! Load the node data in this section for the component simulation
  IsConvergedFlag = .FALSE.

  SELECT CASE(ControllerProps(ControlNum)%ControlVar)
    CASE (iTemperature)  ! 'Temperature'
      ControllerProps(ControlNum)%SensedValue = Node(SensedNode)%Temp
      ! Done once per HVAC step
      IF ( .NOT.ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
        ControllerProps(ControlNum)%SetPointValue = Node(SensedNode)%TempSetPoint
        ControllerProps(ControlNum)%IsSetPointDefinedFlag = .TRUE.
      END IF

    CASE (iTemperatureAndHumidityRatio)  ! 'TemperatureAndHumidityRatio'
      ControllerProps(ControlNum)%SensedValue = Node(SensedNode)%Temp
      ! Done once per HVAC step
      !
      ! WARNING: The scheme for computing the setpoint for the dual temperature and humidity ratio
      !          control strategy breaks down whenever the sensed node temperature is modified by
      !          a controller fired after the current one. Indeed the final sensed node temperature
      !          is likely to have changed in the meantime if the other controller is active,
      !          thereby invalidating the setpoint calculation for the other controller performed
      !          earlier on the air loop.
      !
      IF ( .NOT.ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
        ! NOTE: For TEMPANDHUMRAT control the computed value ControllerProps(ControlNum)%SetPointValue
        !       depends on:
        !       - Node(SensedNode)%HumRatMax
        !       - Node(SensedNode)%Temp
        !       - Node(SensedNode)%HumRat
        IF (Node(SensedNode)%HumRatMax .GT. 0) THEN
          ! Setpoint can only be computed when the sensed node temperature is evaluated at the max
          ! actuated value for the dual humidity ratio / temperature strategy.
          ! See routine CalcSimpleController() for the sequence of operations.
          IF (ControllerProps(ControlNum)%NextActuatedValue == RootFinders(ControlNum)%MaxPoint%X) THEN
            ! Calculate the approach temperature (difference between SA dry-bulb temp and SA dew point temp)
            ApproachTemp = Node(SensedNode)%Temp - PsyTdpFnWPb(Node(SensedNode)%HumRat,OutBaroPress)
            ! Calculate the dew point temperature at the SA humidity ratio setpoint
            DesiredDewPoint = PsyTdpFnWPb(Node(SensedNode)%HumRatMax,OutBaroPress)
            ! Adjust the calculated dew point temperature by the approach temp
            HumidityControlTempSetPoint = DesiredDewPoint + ApproachTemp
            ! NOTE: The next line introduces a potential discontinuity into the residual function
            !       which could prevent the root finder from finding the root it if were done at each
            !       controller iteration. For this reason we perform the setpoint calculation only
            !       once when the air loop has been evaluated with the max actuated value.
            !       See routine CalcSimpleController() for the sequence of operations.
            ControllerProps(ControlNum)%SetPointValue = MIN( &
              Node(SensedNode)%TempSetPoint, &   ! Pure temperature setpoint
              HumidityControlTempSetPoint &      ! Temperature setpoint to achieve the humidity ratio setpoint
            )
            ! Overwrite the "pure" temperature setpoint with the actual setpoint that takes into
            ! account the humidity ratio setpoint.
            !
            ! NOTE: Check that this does not create side-effects somewhere else in the code.
            Node(SensedNode)%TempSetPoint = ControllerProps(ControlNum)%SetPointValue
            ! Finally indicate thate the setpoint has been computed
            ControllerProps(ControlNum)%IsSetPointDefinedFlag = .TRUE.
          END IF
        ELSE
          ! Pure temperature setpoint control strategy
          ControllerProps(ControlNum)%SetPointValue = Node(SensedNode)%TempSetPoint
          ! Finally indicate thate the setpoint has been computed
          ControllerProps(ControlNum)%IsSetPointDefinedFlag = .TRUE.
        END IF
      END IF

    CASE(iHumidityRatio)  ! 'HumidityRatio'
      ControllerProps(ControlNum)%SensedValue = Node(SensedNode)%HumRat
      ! Done once per HVAC step
      IF ( .NOT.ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
        ControllerProps(ControlNum)%SetPointValue = Node(SensedNode)%HumRatSetPoint
        ControllerProps(ControlNum)%IsSetPointDefinedFlag = .TRUE.
      END IF

    CASE(iFlow)   ! 'Flow'
      ControllerProps(ControlNum)%SensedValue = Node(SensedNode)%MassFlowRate
      ! Done once per HVAC step
      IF ( .NOT.ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
        ControllerProps(ControlNum)%SetPointValue = Node(SensedNode)%MassFlowRateSetPoint
        ControllerProps(ControlNum)%IsSetPointDefinedFlag = .TRUE.
      END IF

    CASE DEFAULT
      CALL ShowFatalError( &
        'Invalid Controller Variable Type='// &
        TRIM(ControlVariableTypes(ControllerProps(ControlNum)%ControlVar)) &
      )
  END SELECT

  SELECT CASE(ControllerProps(ControlNum)%ActuatorVar)
    CASE(iFlow)  ! 'Flow'
      ! At the beginning of every time step the value is reset to the User Input
      ! The interface managers can reset the Max or Min to available values during the time step
      ! and these will then be the new setpoint limits for the controller to work within.
      ControllerProps(ControlNum)%ActuatedValue = Node(ActuatedNode)%MassFlowRate
      ! Compute the currently available min and max bounds for controller.
      ! Done only once per HVAC step, as it would not make any sense to modify the min/max
      ! bounds during successive iterations of the root finder.
      IF ( ControllerProps(ControlNum)%NumCalcCalls == 0 ) THEN
        ControllerProps(ControlNum)%MinAvailActuated  = MAX( &
          Node(ActuatedNode)%MassFlowRateMinAvail, &
          ControllerProps(ControlNum)%MinActuated)
        ControllerProps(ControlNum)%MaxAvailActuated  = MIN( &
          Node(ActuatedNode)%MassFlowRateMaxAvail, &
          ControllerProps(ControlNum)%MaxActuated)
        ! MinActuated is user input for minimum actuated flow, use that value if allowed
        ! (i.e., reset MinAvailActuated based on Node%MassFlowRateMaxAvail)
        ControllerProps(ControlNum)%MinAvailActuated = MIN( &
          ControllerProps(ControlNum)%MinAvailActuated, &
          ControllerProps(ControlNum)%MaxAvailActuated)
       END IF

    CASE DEFAULT
      CALL ShowFatalError( &
        'Invalid Actuator Variable Type='// &
        TRIM(ControlVariableTypes(ControllerProps(ControlNum)%ActuatorVar)) &
      )
  END SELECT

  ! Compute residual for control function using desired setpoint value and current sensed value
  !
  ! NOTE: The delta sensed value might be wrong if the setpoint has not yet been computed.
  !       Make sure not to use it until the setpoint has been computed.
  IF ( ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
    ControllerProps(ControlNum)%DeltaSensed = ControllerProps(ControlNum)%SensedValue &
                                              - ControllerProps(ControlNum)%SetPointValue
  ELSE
    ControllerProps(ControlNum)%DeltaSensed = 0.0d0
  ENDIF

  RETURN
END SUBROUTINE InitController


SUBROUTINE SizeController(ControlNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   November 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Controller Components for which max flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the actuated node. Should have been set by the water coils.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataConvergParams, ONLY: HVACEnergyToler, HVACTemperatureToler
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ControlNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ActuatedNode              ! node number of actuated node
  INTEGER :: WaterCompNum

  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode

  IF (ControllerProps(ControlNum)%MaxVolFlowActuated == AutoSize) THEN
    DO WaterCompNum=1,SaveNumPlantComps
      IF (CompDesWaterFlow(WaterCompNum)%SupNode == ActuatedNode) THEN
        ControllerProps(ControlNum)%MaxVolFlowActuated = CompDesWaterFlow(WaterCompNum)%DesVolFlowRate
      ENDIF
    ENDDO

    IF (ControllerProps(ControlNum)%MaxVolFlowActuated < SmallWaterVolFlow) THEN
      ControllerProps(ControlNum)%MaxVolFlowActuated = 0.0d0
    END IF
    CALL ReportSizingOutput(ControllerProps(ControlNum)%ControllerType, ControllerProps(ControlNum)%ControllerName, &
                            'Maximum Actuated Flow [m3/s]', ControllerProps(ControlNum)%MaxVolFlowActuated)
  END IF

  IF (ControllerProps(ControlNum)%Offset == AutoSize) THEN
    ! 2100 = 0.5 * 4.2 * 1000/1.2 * 1.2 where 0.5 is the ratio of chilled water delta T to supply air delta T,
    !   4.2 is the ratio of water density to air density, 1000/1.2 is the ratio of water specific heat to
    !   air specific heat, and 1.2 converts the result from air volumetric flow rate to air mass flow rate.
    !   The assumption is that a temperatute tolerance of 0.001 C is good for an air mass flow rate of 1 kg/s.
    !   So we divide .001 by the air mass flow rate estimated from the water volumetric flow rate to come up
    !   with a temperature tolerance that won't exceed the loop energy error tolerance (10 W).
    ! Finally we need to take into account the fact that somebody might change the energy tolerance.
    ControllerProps(ControlNum)%Offset = &
      ( 0.001d0 / (2100.d0 * MAX(ControllerProps(ControlNum)%MaxVolFlowActuated,SmallWaterVolFlow)) ) &
        * (HVACEnergyToler/10.0d0)
    ! do not let the controller tolerance exceed 1/10 of the loop temperature tolerance.
    ControllerProps(ControlNum)%Offset =  MIN(0.1d0*HVACTemperatureToler, ControllerProps(ControlNum)%Offset)
    CALL ReportSizingOutput( &
      ControllerProps(ControlNum)%ControllerType, ControllerProps(ControlNum)%ControllerName, &
      'Controller Convergence Tolerance', ControllerProps(ControlNum)%Offset )
  END IF

  RETURN
END SUBROUTINE SizeController

 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************

SUBROUTINE CalcSimpleController(ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   May 2006
          !       MODIFIED       Dimitri Curtil (LBNL), May 2006
          !                      - Added IsPointFlagDefinedFlag to control when the setpoiont should be
          !                        computed depending on the control strategy. This was needed to
          !                        trigger the setpoint calculation for the dual temperature and
          !                        humidity ratio control strategy only once the air loop has been
          !                        evaluated with the max actuated flow.
          !                        See the routine InitController() for more details on the setpoint
          !                        calculation.
          !       MODIFIED       Dimitri Curtil (LBNL), March 2006
          !                      - Added IsUpToDateFlag to detect whether or not the air loop
          !                        has been evaluated prior the first iteration, which allows
          !                        to use the current node values as the first iterate for the root
          !                        finder (for COLD RESTART ONLY).
          !       MODIFIED       Dimitri Curtil (LBNL), Feb 2006
          !                      - Added mode detection capability.
          !                      - Now trying min actuated variable first to
          !                        detect min-constrained cases in 1 iteration.
          !                      - Trying max actuated variable second.
          !                        Checks for max-constrained here instead of in
          !                        NormActuatedCalc mode.
          !                      - Checking for inactive mode as soon as min and max
          !                        support points are known instead of in NormActuatedCalc
          !                        mode.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,        ONLY : TrimSigDigits
  USE RootFinder,     ONLY : InitializeRootFinder, CheckRootFinderCandidate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)             :: ControlNum
  LOGICAL, INTENT(IN)             :: FirstHVACIteration
  ! Set to TRUE if current controller is converged; FALSE if more iteration are needed.
  ! Note that an error in the root finding process can be mapped onto IsConvergedFlag=TRUE
  ! to avoid continue iterating.
  LOGICAL, INTENT(OUT)            :: IsConvergedFlag
  ! TRUE if air loop is up-to-date meaning that the current node values are consistent (air loop evaluated)
  ! Only used within the Calc routines
  LOGICAL, INTENT(INOUT)          :: IsUpToDateFlag
  CHARACTER(len=*), INTENT(IN)    :: ControllerName ! used when errors occur

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ActuatedNode
  INTEGER             :: SensedNode


  ! Increment counter
  ControllerProps(ControlNum)%NumCalcCalls = ControllerProps(ControlNum)%NumCalcCalls + 1

  ! Obtain actuated and sensed nodes
  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode


  ! Check to see if the component is running; if not converged and return.  This check will be done
  ! by looking at the component mass flow rate at the sensed node.
  IF (Node(SensedNode)%MassFlowRate == 0.0d0) THEN
    CALL ExitCalcController( &
      ControlNum, &
      constant_zero, &
      iModeOff, &
      IsConvergedFlag, &
      IsUpToDateFlag &
    )
    RETURN
  END IF


  ! Intialize root finder
  IF ( ControllerProps(ControlNum)%NumCalcCalls == 1 ) THEN
    ! Set min/max boundaries for root finder on first iteration
    CALL InitializeRootFinder(                           &
      RootFinders(ControlNum),                      & !
      ControllerProps(ControlNum)%MinAvailActuated, & ! XMin
      ControllerProps(ControlNum)%MaxAvailActuated  & ! XMax
    )

    ControllerProps(ControlNum)%ReuseIntermediateSolutionFlag = &
      ! Only allow to reuse initial evaluation if the air loop is up-to-date.
      ! Set in SolveAirLoopControllers()
      IsUpToDateFlag .AND.                                    &
      ! Only reuse initial evaluation if setpoint is already available for the current controller
      !
      ! Note that in the case of dual temperature and humidity ratio control strategy since the
      ! setpoint at a later iteration, the initial solution cannot be reused.
      ControllerProps(ControlNum)%IsSetPointDefinedFlag .AND. &
      ! Make sure that the initial candidate value lies within range
      CheckRootFinderCandidate( RootFinders(ControlNum), ControllerProps(ControlNum)%ActuatedValue )

    IF ( ControllerProps(ControlNum)%ReuseIntermediateSolutionFlag ) THEN

      ! Reuse intermediate solution obtained with previous controller for the current HVAC step
      ! and fire root finder to get next root candidate
      CALL FindRootSimpleController( ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName)

    ELSE

      ! We need to evaluate the sensed node temperature with the max actuated value before
      ! we can compute the actual setpoint for the dual humidity ratio / temperature strategy.
      SelectController: SELECT CASE ( ControllerProps(ControlNum)%ControlVar )
      CASE (iTemperature,iHumidityRatio,iFlow)
        ! Always start with min point by default for the other control strategies
        ControllerProps(ControlNum)%NextActuatedValue = RootFinders(ControlNum)%MinPoint%X

      CASE (iTemperatureAndHumidityRatio)
        IF ( .NOT.ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
          ! Always start with max point if setpoint not yet computed. See routine InitController().
          ControllerProps(ControlNum)%NextActuatedValue = RootFinders(ControlNum)%MaxPoint%X
        ELSE
          ! If setpoint already exists (i.e., HumRatMax <= 0) then try min point first as in simple
          ! temperature control case.
          ControllerProps(ControlNum)%NextActuatedValue = RootFinders(ControlNum)%MinPoint%X
        END IF

      CASE DEFAULT
        ! Should never happen
        CALL ShowSevereError('CalcSimpleController: HVAC controller failed at '//TRIM(CreateHVACStepFullString()))
        CALL ShowContinueError( &
          ' Controller name='//TRIM(ControllerProps(ControlNum)%ControllerName) &
        )
        CALL ShowContinueError( &
          ' Unrecognized control variable type='// &
          TRIM(TrimSigDigits(ControllerProps(ControlNum)%ControlVar)) &
        )
        CALL ShowFatalError('Preceding error causes program termination.')
      END SELECT SelectController

    END IF


  ! Process current iterate and compute next candidate if needed
  ! We assume that after the first controller iteration:
  ! - the setpoint is defined
  ! - the min and max available bounds are defined
  !
  ! NOTE: Not explicitly checked but the air mass flow rate must remain constant across successive
  !       controller iterations to ensure that the root finder converges.
  ELSE
    ! Check that the setpoint is defined
    IF ( .NOT.ControllerProps(ControlNum)%IsSetPointDefinedFlag ) THEN
      CALL ShowSevereError('CalcSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      CALL ShowContinueError( &
        ' Controller name="'//TRIM(ControllerName)//'"' &
      )
      CALL ShowContinueError(' Setpoint is not available/defined.')
      CALL ShowFatalError('Preceding error causes program termination.')
    END IF
    ! Monitor invariants across successive controller iterations
    ! - min bound
    ! - max bound
    !
    IF ( RootFinders(ControlNum)%MinPoint%X /= ControllerProps(ControlNum)%MinAvailActuated ) THEN
      CALL ShowSevereError('CalcSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      CALL ShowContinueError( &
        ' Controller name="'//TRIM(ControllerName)//'"' &
      )
      CALL ShowContinueError(' Minimum bound must remain invariant during successive iterations.')
      CALL ShowContinueError(' Minimum root finder point='//  &
         trim(TrimSigDigits(RootFinders(ControlNum)%MinPoint%X,NumSigDigits)))
      CALL ShowContinueError(' Minimum avail actuated='//  &
         trim(TrimSigDigits(ControllerProps(ControlNum)%MinAvailActuated,NumSigDigits)))
      CALL ShowFatalError('Preceding error causes program termination.')
    END IF
    IF ( RootFinders(ControlNum)%MaxPoint%X /= ControllerProps(ControlNum)%MaxAvailActuated ) THEN
      CALL ShowSevereError('CalcSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      CALL ShowContinueError( &
        ' Controller name="'//TRIM(ControllerName)//'"' &
      )
      CALL ShowContinueError(' Maximum bound must remain invariant during successive iterations.')
      CALL ShowContinueError(' Maximum root finder point='//  &
         trim(TrimSigDigits(RootFinders(ControlNum)%MaxPoint%X,NumSigDigits)))
      CALL ShowContinueError(' Maximum avail actuated='//  &
         trim(TrimSigDigits(ControllerProps(ControlNum)%MaxAvailActuated,NumSigDigits)))
      CALL ShowFatalError('Preceding error causes program termination.')
    END IF

    ! Updates root finder with current iterate and computes next one if needed
    CALL FindRootSimpleController( ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName )

  END IF

  RETURN
END SUBROUTINE CalcSimpleController


SUBROUTINE FindRootSimpleController(ControlNum, FirstHVACIteration, IsConvergedFlag, IsUpToDateFlag, ControllerName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   March 2006
          !       MODIFIED       na
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! New routine to fire the root finder using the current actuated and sensed values.
          ! - Updates IsConvergedFlag depending ou iteration status.
          ! - Sets next actuated value to try in ControllerProps(ControlNum)%NextActuatedValue
          !

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE General,        ONLY : TrimSigDigits
  USE RootFinder,     ONLY : IterateRootFinder, CheckRootFinderCandidate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: ControlNum
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
  LOGICAL, INTENT(OUT)   :: IsConvergedFlag
  LOGICAL, INTENT(OUT)   :: IsUpToDateFlag
  CHARACTER(len=*), INTENT(IN)    :: ControllerName ! used when errors occur

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ActuatedNode
  INTEGER             :: SensedNode
  ! TRUE if root finder needs to continue iterating, FALSE otherwise.
  LOGICAL             :: IsDoneFlag
  LOGICAL             :: ReusePreviousSolutionFlag
  INTEGER             :: PreviousSolutionIndex
  LOGICAL             :: PreviousSolutionDefinedFlag
  INTEGER             :: PreviousSolutionMode
  REAL(r64)           :: PreviousSolutionValue


  ! Obtain actuated and sensed nodes
  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode


  ! Update root finder with latest solution point
  ! Check for unconstrained/constrained convergence
  ! Compute next candidate if not converged yet.
  CALL IterateRootFinder( &
    RootFinders(ControlNum),                       & ! root finder's data
    ControllerProps(ControlNum)%ActuatedValue,     & ! X
    ControllerProps(ControlNum)%DeltaSensed,       & ! Y
    IsDoneFlag                                     & ! not used
  )


  ! Process root finder if converged or error
  ! Map root finder status onto controller mode
  SelectStatus: SELECT CASE ( RootFinders(ControlNum)%StatusFlag )
    CASE (iStatusNone, iStatusWarningNonMonotonic, iStatusWarningSingular)
      ! We need to keep iterating...
      IsConvergedFlag = .FALSE.

      IF ( FirstHVACIteration ) THEN
        PreviousSolutionIndex = 1
      ELSE
        PreviousSolutionIndex = 2
      END IF

      PreviousSolutionDefinedFlag = ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%DefinedFlag
      PreviousSolutionMode  = ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%Mode
      PreviousSolutionValue = ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%ActuatedValue

      ! Attempt to use root at previous HVAC step in place of the candidate produced by the
      ! root finder.
      ReusePreviousSolutionFlag = &
        ! Set in InitController() depending on controller mode at previous HVAC step iteration
        ControllerProps(ControlNum)%ReusePreviousSolutionFlag .AND.  &
        ! Only attempted during bracketing phase of root finder.
        (RootFinders(ControlNum)%CurrentMethodType == iMethodBracket) .AND.  &
        ! Check that a previous solution is available
        PreviousSolutionDefinedFlag  .AND.  &
        ! Make sure that mode of previous solution was active
        (PreviousSolutionMode == iModeActive)  .AND.  &
        ! Make sure that proposed candidate does not conflict with current min/max range and lower/upper brackets
        CheckRootFinderCandidate( RootFinders(ControlNum), PreviousSolutionValue )

      IF ( ReusePreviousSolutionFlag ) THEN
        ! Try to reuse saved solution from previous call to SolveAirLoopControllers()
        ! instead of candidate proposed by the root finder
        ControllerProps(ControlNum)%NextActuatedValue = PreviousSolutionValue

        ! Turn off flag since we can only use the previous solution once per HVAC iteration
        ControllerProps(ControlNum)%ReusePreviousSolutionFlag = .FALSE.
      ELSE
        ! By default, use candidate value computed by root finder
        ControllerProps(ControlNum)%NextActuatedValue = RootFinders(ControlNum)%XCandidate
      END IF


    CASE ( iStatusOK, iStatusOKRoundOff )
      ! Indicate convergence with base value (used to obtain DeltaSensed!)
      CALL ExitCalcController( &
        ControlNum, &
        RootFinders(ControlNum)%XCandidate, &
        iModeActive,     &
        IsConvergedFlag, &
        IsUpToDateFlag   &
      )


    CASE (iStatusOKMin)
      ! Indicate convergence with min value
      CALL ExitCalcController( &
        ControlNum, &
        ! Should be the same as ControllerProps(ControlNum)%MinAvailActuated
        RootFinders(ControlNum)%MinPoint%X, &
        iModeMinActive, &
        IsConvergedFlag, &
        IsUpToDateFlag   &
      )


    CASE (iStatusOKMax)
      ! Indicate convergence with max value
      CALL ExitCalcController( &
        ControlNum, &
        ! Should be the same as ControllerProps(ControlNum)%MaxAvailActuated
        RootFinders(ControlNum)%MaxPoint%X, &
        iModeMaxActive, &
        IsConvergedFlag, &
        IsUpToDateFlag   &
      )


    CASE (iStatusErrorSingular)
      ! Indicate inactive mode with min actuated value
      CALL ExitCalcController( &
        ControlNum, &
        ! NOTE: Original code returned Node(ActuatedNode)%MassFlowRateMinAvail
        !       This was not portable in case the actuated variable was NOT a mass flow rate!
        !
        !       Replaced   Node(ActuatedNode)%MassFlowRateMinAvail
        !       with       RootFinders(ControlNum)%MinPoint%X
        !       which is the same as (see SUBROUTINE InitController)
        !                  ControllerProps(ControlNum)%MinAvailActuated
        RootFinders(ControlNum)%MinPoint%X, &
        iModeInactive,   &
        IsConvergedFlag, &
        IsUpToDateFlag   &
      )


    ! Abnormal case: should never happen
    CASE (iStatusErrorRange)
      CALL ShowSevereError('FindRootSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      CALL ShowContinueError( &
        ' Controller name="'//TRIM(ControllerName)//'"' &
      )
      CALL ShowContinueError( &
        ' Root candidate x='// &
        TRIM(TrimSigDigits(ControllerProps(ControlNum)%ActuatedValue,NumSigDigits))// &
        ' does not lie within the min/max bounds.' &
      )
      CALL ShowContinueError( &
        ' Min bound is x='// &
        TRIM(TrimSigDigits(RootFinders(ControlNum)%MinPoint%X,NumSigDigits)) &
      )
      CALL ShowContinueError( &
        ' Max bound is x='// &
        TRIM(TrimSigDigits(RootFinders(ControlNum)%MaxPoint%X,NumSigDigits)) &
      )
      CALL ShowFatalError('Preceding error causes program termination.')


    ! Abnormal case: should never happen
    CASE (iStatusErrorBracket)
      CALL ShowSevereError('FindRootSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      CALL ShowContinueError( &
        ' Controller name='//TRIM(ControllerProps(ControlNum)%ControllerName) &
      )
      CALL ShowContinueError( &
        ' Controller action='//TRIM(ActionTypes(ControllerProps(ControlNum)%Action)) &
      )
      CALL ShowContinueError( &
        ' Root candidate x='// &
        TRIM(TrimSigDigits(ControllerProps(ControlNum)%ActuatedValue,NumSigDigits))// &
        ' does not lie within the lower/upper brackets.' &
      )
      IF ( RootFinders(ControlNum)%LowerPoint%DefinedFlag ) THEN
        CALL ShowContinueError( &
          ' Lower bracket is x='// &
          TRIM(TrimSigDigits(RootFinders(ControlNum)%LowerPoint%X,NumSigDigits)) &
        )
      END IF
      IF ( RootFinders(ControlNum)%UpperPoint%DefinedFlag ) THEN
        CALL ShowContinueError( &
          ' Upper bracket is x='// &
          TRIM(TrimSigDigits(RootFinders(ControlNum)%UpperPoint%X,NumSigDigits)) &
        )
      END IF
      CALL ShowFatalError('Preceding error causes program termination.')


    ! Detected control function with wrong action between the min and max points.
    ! Should never happen: probably indicative of some serious problems in IDFs
    !
    ! NOTE: This approach is more robust and consistent than what was done in version 1.3.
    !       Indeed, such a function with the wrong action characteristic would have silently returned
    !       either of the following values depending on the specified action:
    !       - NORMAL ACTION:
    !         - If y(xMin) > ySetPoint && y(xMax) < y(xMin), then  x = xMin
    !         - If y(xMin) < ySetPoint && y(xMax) < y(xMin), then  x = xMax
    !       - REVERSE ACTION:
    !         - If y(xMin) < ySetPoint && y(xMax) > y(xMin), then  x = xMin
    !         - If y(xMin) > ySetPoint && y(xMax) > y(xMin), then  x = xMax
    CASE (iStatusErrorSlope)
      ! CALL ShowSevereError('FindRootSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      ! CALL ShowContinueError( &
      !   'FindRootSimpleController: Controller name='//TRIM(ControllerProps(ControlNum)%ControllerName) &
     !  )
     !  CALL ShowContinueError( &
     !    'FindRootSimpleController: Controller action='//TRIM(ActionTypes(ControllerProps(ControlNum)%Action)) &
     !  )
     !  CALL ShowContinueError( &
     !    'FindRootSimpleController: Controller setpoint='// &
     !    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,NumSigDigits)) &
     !  )
     !  CALL ShowContinueError( &
     !    'FindRootSimpleController: Controller function is inconsistent with the specified action.' &
     !  )
     !  CALL ShowContinueError( &
     !    'FindRootSimpleController: Min bound is '// &
     !    'x='//TRIM(TrimSigDigits(RootFinders(ControlNum)%MinPoint%X,NumSigDigits))//','// &
     !    'y='//TRIM(TrimSigDigits(RootFinders(ControlNum)%MinPoint%Y,NumSigDigits)) &
     !  )
     !  CALL ShowContinueError( &
     !    'FindRootSimpleController: Max bound is '// &
     !    'x='//TRIM(TrimSigDigits(RootFinders(ControlNum)%MaxPoint%X,NumSigDigits))//','// &
     !    'y='//TRIM(TrimSigDigits(RootFinders(ControlNum)%MaxPoint%Y,NumSigDigits)) &
     !  )
     !  CALL ShowFatalError('FindRootSimpleController: Preceding error causes program termination.')
      IF (.NOT. WarmupFlag .AND. ControllerProps(ControlNum)%BadActionErrCount == 0) THEN
        ControllerProps(ControlNum)%BadActionErrCount = ControllerProps(ControlNum)%BadActionErrCount + 1
        CALL ShowSevereError('FindRootSimpleController: Controller error for controller = "'//  &
                              TRIM(ControllerName)//'"')
        CALL ShowContinueErrorTimeStamp('  ')
        CALL ShowContinueError('  Controller function is inconsistent with user specified controller action = ' // &
                               TRIM(ActionTypes(ControllerProps(ControlNum)%Action)))
        CALL ShowContinueError('  Actuator will be set to maximum action')
        CALL ShowContinueError( 'Controller control type='// TRIM(ControlVariableTypes(ControllerProps(ControlNum)%ControlVar)) )
        IF (ControllerProps(ControlNum)%ControlVar == iTemperature) THEN
          CALL ShowContinueError('Controller temperature setpoint = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,2))//' [C]')
          CALL ShowContinueError('Controller sensed temperature = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SensedValue,2))//' [C]')
        ELSE IF (ControllerProps(ControlNum)%ControlVar == iHumidityRatio) THEN
          CALL ShowContinueError('Controller humidity ratio setpoint = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,2))// &
                                 ' [kg-H2O/kg-air]')
          CALL ShowContinueError('Controller sensed humidity ratio = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SensedValue,2))// &
                                 ' [kg-H2O/kg-air]')
        ELSE IF (ControllerProps(ControlNum)%ControlVar == iTemperatureAndHumidityRatio) THEN
          CALL ShowContinueError('Controller temperature setpoint = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,2))//' [C]')
          CALL ShowContinueError('Controller sensed temperature = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SensedValue,2))//' [C]')
          CALL ShowContinueError('Controller humidity ratio setpoint = '//  &
                TRIM(TrimSigDigits(Node(ControllerProps(ControlNum)%SensedNode)%HumRatMax,2))//' [kg-H2O/kg-air]')
          CALL ShowContinueError('Controller sensed humidity ratio = '//  &
                TRIM(TrimSigDigits(Node(ControllerProps(ControlNum)%SensedNode)%HumRat,2))//' [kg-H2O/kg-air]')
        ELSE IF (ControllerProps(ControlNum)%ControlVar == iFlow) THEN
          CALL ShowContinueError('Controller mass flow rate setpoint = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,2))//' [kg/s]')
          CALL ShowContinueError('Controller sensed mass flow rate = '//  &
                    TRIM(TrimSigDigits(ControllerProps(ControlNum)%SensedValue,2))//' [kg/s]')
        ELSE
          ! bad control variable input checked in input routine
        END IF
        IF (ControllerProps(ControlNum)%ActuatorVar == iFlow) THEN
          CALL ShowContinueError('Controller actuator mass flow rate set to '//  &
                  TRIM(TrimSigDigits(ControllerProps(ControlNum)%MaxAvailActuated,2))//' [kg/s]')
          IF (ControllerProps(ControlNum)%ControlVar == iTemperature) THEN
            CALL ShowContinueError('Controller actuator temperature = '//  &
                  TRIM(TrimSigDigits(Node(ControllerProps(ControlNum)%ActuatedNode)%Temp,2))//' [C]')
            CALL ShowContinueError('  Note: Chilled water coils should be reverse action and the entering chilled')
            CALL ShowContinueError('        water temperature (controller actuator temperature) should be '//  &
                                            'below the setpoint temperature')
            CALL ShowContinueError('  Note: Hot water coils should be normal action and the entering hot')
            CALL ShowContinueError('        water temperature (controller actuator temperature) should be '//  &
                                            'above the setpoint temperature')
          END IF
        ELSE
          ! bad actuator variable input checked in input routine
        END IF
      ELSE IF (.NOT. WarmupFlag) THEN
        ControllerProps(ControlNum)%BadActionErrCount = ControllerProps(ControlNum)%BadActionErrCount + 1
        CALL ShowRecurringSevereErrorAtEnd('FindRootSimpleController: Previous controller action error'//  &
                                           ' continues for controller = '// &
                                            TRIM(ControllerName), &
                                            ControllerProps(ControlNum)%BadActionErrIndex)
      ELSE
      ! do nothing
      END IF
      ! Indicate convergence with min value
      CALL ExitCalcController( &
        ControlNum, &
        ! Should be the same as ControllerProps(ControlNum)%MaxAvailActuated
        RootFinders(ControlNum)%MaxPoint%X, &
        iModeMaxActive, &
        IsConvergedFlag, &
        IsUpToDateFlag   &
      )

    CASE DEFAULT
      ! Should never happen
      CALL ShowSevereError('FindRootSimpleController: Root finder failed at '//TRIM(CreateHVACStepFullString()))
      CALL ShowContinueError( &
        ' Controller name='//TRIM(ControllerName) &
      )
      CALL ShowContinueError( &
        ' Unrecognized root finder status flag='// &
        TRIM(TrimSigDigits(RootFinders(ControlNum)%StatusFlag)) &
      )
      CALL ShowFatalError('Preceding error causes program termination.')


  END SELECT SelectStatus

  RETURN
END SUBROUTINE FindRootSimpleController


SUBROUTINE CheckSimpleController(ControlNum, IsConvergedFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil (LBNL)
          !       DATE WRITTEN   Feb 2006
          !       MODIFIED       na
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! New routine used to detect whether controller can be considered converged
          ! depending on its mode of operation.
          ! Used after all controllers on an air loop have been solved in order
          ! to make sure that final air loop state still represents a converged
          ! state.
          !
          ! PRECONDITION: Setpoint must be known. See ControllerProps%IsSetPointDefinedFlag
          !

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE General,        ONLY : TrimSigDigits
  USE RootFinder,     ONLY : CheckRootFinderConvergence

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: ControlNum
  LOGICAL, INTENT(OUT)   :: IsConvergedFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ActuatedNode
  INTEGER             :: SensedNode


  ! Obtain actuated and sensed nodes
  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode

  ! Default initialization: assuming no convergence unless detected in the following code!
  IsConvergedFlag = .FALSE.


  SelectMode: SELECT CASE (ControllerProps(ControlNum)%Mode)
    CASE (iModeOff)
      ! Check whether the component is running
      !
      ! This check is perfomed by looking at the component mass flow rate at the sensed node.
      ! Since the components have been simulated before getting here, if they are zero they should be OFF.
      IF (Node(SensedNode)%MassFlowRate == 0.0d0) THEN
        IF ( ControllerProps(ControlNum)%ActuatedValue == 0.0d0 ) THEN
          IsConvergedFlag = .TRUE.
          RETURN
        END IF
      END IF


    CASE (iModeInactive)
      ! Controller component NOT available (ie, inactive)
      ! Make sure that the actuated variable is still equal to the node min avail
      !
      ! NOTE: Replaced Node(ActuatedNode)%MassFlowRateMinAvail         in release 1.3
      !       with     ControllerProps(ControlNum)%MinAvailActuated    in release 1.4
      IF ( ControllerProps(ControlNum)%ActuatedValue == ControllerProps(ControlNum)%MinAvailActuated) THEN
        IsConvergedFlag = .TRUE.
        RETURN
      END IF


    CASE (iModeMinActive)
      ! Check for min constrained convergence
      IF ( CheckMinActiveController(ControlNum) ) THEN
        IsConvergedFlag = .TRUE.
        RETURN
      END IF
      ! Check for unconstrained convergence assuming that there is more than one controller controlling
      ! the same sensed node and that the other controller was able to meet the setpoint although this one
      ! was min-constrained.
      IF ( CheckRootFinderConvergence( RootFinders(ControlNum), ControllerProps(ControlNum)%DeltaSensed ) ) THEN
        ! Indicate convergence with base value (used to compute DeltaSensed!)
        IsConvergedFlag = .TRUE.
        RETURN
      END IF


    CASE (iModeMaxActive)
      ! Check for max constrained convergence
      IF ( CheckMaxActiveController(ControlNum) ) THEN
        IsConvergedFlag = .TRUE.
        RETURN
      END IF
      ! Check for unconstrained convergence assuming that there is more than one controller controlling
      ! the same sensed node and that the other controller was able to meet the setpoint although this one
      ! was max-constrained.
      IF ( CheckRootFinderConvergence( RootFinders(ControlNum), ControllerProps(ControlNum)%DeltaSensed ) ) THEN
        ! Indicate convergence with base value (used to compute DeltaSensed!)
        IsConvergedFlag = .TRUE.
        RETURN
      END IF


    CASE (iModeActive)
      ! Check min constraint on actuated variable
      IF ( ControllerProps(ControlNum)%ActuatedValue < ControllerProps(ControlNum)%MinAvailActuated ) THEN
        IsConvergedFlag = .FALSE.
        RETURN
      END IF
      ! Check max constraint on actuated variable
      IF ( ControllerProps(ControlNum)%ActuatedValue > ControllerProps(ControlNum)%MaxAvailActuated ) THEN
        IsConvergedFlag = .FALSE.
        RETURN
      END IF

      ! Check for unconstrained convergence
      !
      ! Equivalent to:
      ! IF ((ABS(ControllerProps(ControlNum)%DeltaSensed) .LE. ControllerProps(ControlNum)%Offset)) THEN
      !
      ! NOTE: If setpoint has changed since last call, then the following test will most likely fail.
      IF ( CheckRootFinderConvergence( RootFinders(ControlNum), ControllerProps(ControlNum)%DeltaSensed ) ) THEN
        ! Indicate convergence with base value (used to compute DeltaSensed!)
        IsConvergedFlag = .TRUE.
        RETURN
      END IF
      ! Check for min constrained convergence
      IF ( CheckMinActiveController(ControlNum) ) THEN
        IsConvergedFlag = .TRUE.
        RETURN
      END IF
      ! Check for max constrained convergence
      IF ( CheckMaxActiveController(ControlNum) ) THEN
        IsConvergedFlag = .TRUE.
        RETURN
      END IF


    CASE DEFAULT
      ! Can only happen if controller is not converged after MaxIter in SolveAirLoopControllers()
      ! which will produce ControllerProps(ControlNum)%Mode = iModeNone
      IsConvergedFlag = .FALSE.

  END SELECT SelectMode

  RETURN
END SUBROUTINE CheckSimpleController


LOGICAL FUNCTION CheckMinActiveController( ControlNum )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns true if controller is min-constrained. false otherwise.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: ControlNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  CheckMinActiveController = .FALSE.

  ! Check that actuated value is the min avail actuated value
  IF ( ControllerProps(ControlNum)%ActuatedValue /= ControllerProps(ControlNum)%MinAvailActuated ) THEN
    CheckMinActiveController = .FALSE.
    RETURN
  END IF

  SelectAction: SELECT CASE (ControllerProps(ControlNum)%Action)
  CASE (iNormalAction)  ! "NORMAL"
    ! Check for min constrained convergence
    IF (ControllerProps(ControlNum)%SetPointValue <= ControllerProps(ControlNum)%SensedValue) THEN
      CheckMinActiveController = .TRUE.
      RETURN
    END IF

  CASE (iReverseAction)  ! "REVERSE"
    ! Check for min constrained convergence
    IF (ControllerProps(ControlNum)%SetPointValue >= ControllerProps(ControlNum)%SensedValue) THEN
      CheckMinActiveController = .TRUE.
      RETURN
    END IF

  CASE DEFAULT
    ! Should never happen
    CALL ShowSevereError( &
      'CheckMinActiveController: Invalid controller action during '//TRIM(CreateHVACStepFullString())//'.' &
    )
    CALL ShowContinueError( &
      'CheckMinActiveController: Controller name='//TRIM(ControllerProps(ControlNum)%ControllerName) &
    )
    CALL ShowContinueError('CheckMinActiveController: Valid choices are "NORMAL" or "REVERSE"')
    CALL ShowFatalError('CheckMinActiveController: Preceding error causes program termination.')

  END SELECT SelectAction

  RETURN
END FUNCTION CheckMinActiveController


LOGICAL FUNCTION CheckMaxActiveController( ControlNum )
          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns true if controller is max-constrained. false otherwise.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: ControlNum

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  CheckMaxActiveController = .FALSE.

  ! Check that actuated value is the max avail actuated value
  IF ( ControllerProps(ControlNum)%ActuatedValue /= ControllerProps(ControlNum)%MaxAvailActuated ) THEN
    CheckMaxActiveController = .FALSE.
    RETURN
  END IF

  SelectAction: SELECT CASE (ControllerProps(ControlNum)%Action)
  CASE (iNormalAction)  ! "NORMAL"
    ! Check for max constrained convergence
    IF (ControllerProps(ControlNum)%SetPointValue >= ControllerProps(ControlNum)%SensedValue) THEN
      CheckMaxActiveController = .TRUE.
      RETURN
    END IF

  CASE (iReverseAction)  ! "REVERSE"
    ! Check for max constrained convergence
    IF (ControllerProps(ControlNum)%SetPointValue <= ControllerProps(ControlNum)%SensedValue) THEN
      CheckMaxActiveController = .TRUE.
      RETURN
    END IF

  CASE DEFAULT
    ! Should never happen
    CALL ShowSevereError( &
      'CheckMaxActiveController: Invalid controller action during '//TRIM(CreateHVACStepFullString())//'.' &
    )
    CALL ShowContinueError( &
      'CheckMaxActiveController: Controller name='//TRIM(ControllerProps(ControlNum)%ControllerName) &
    )
    CALL ShowContinueError('CheckMaxActiveController: Valid choices are "NORMAL" or "REVERSE"')
    CALL ShowFatalError('CheckMaxActiveController: Preceding error causes program termination.')

  END SELECT SelectAction

  RETURN
END FUNCTION CheckMaxActiveController


SUBROUTINE SaveSimpleController(ControlNum, FirstHVACIteration, IsConvergedFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates solution trackers if simple controller is converged.
          !

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: ControlNum
  LOGICAL, INTENT(IN)   :: FirstHVACIteration
  LOGICAL, INTENT(IN)   :: IsConvergedFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PreviousSolutionIndex

          ! FLOW

  ! Save solution and mode for next call only if converged
  IF ( IsConvergedFlag ) THEN
    IF ( FirstHVACIteration ) THEN
      PreviousSolutionIndex = 1
    ELSE
      PreviousSolutionIndex = 2
    END IF

    IF ( ControllerProps(ControlNum)%Mode == iModeActive ) THEN
      ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%DefinedFlag   = .TRUE.
      ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%Mode          = ControllerProps(ControlNum)%Mode
      ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%ActuatedValue =   &
                                                                              ControllerProps(ControlNum)%NextActuatedValue
    ELSE
      ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%DefinedFlag   = .FALSE.
      ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%Mode          = ControllerProps(ControlNum)%Mode
      ControllerProps(ControlNum)%SolutionTrackers(PreviousSolutionIndex)%ActuatedValue =   &
                                                                              ControllerProps(ControlNum)%NextActuatedValue
     END IF
  END IF

  RETURN
END SUBROUTINE SaveSimpleController


SUBROUTINE LimitController(ControlNum, IsConvergedFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   July 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER   :: ControlNum !unused1208
  LOGICAL   :: IsConvergedFlag !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  RETURN
END SUBROUTINE LimitController

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the Controller Module
! *****************************************************************************

SUBROUTINE UpdateController(ControlNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the actuated node with the next candidate value.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities,  ONLY : SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ControlNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ActuatedNode
  INTEGER             :: SensedNode

   ! Set the sensed and actuated node numbers
   ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
   SensedNode   = ControllerProps(ControlNum)%SensedNode

   ! Set the actuated node of the Controller
  SELECT CASE(ControllerProps(ControlNum)%ActuatorVar)
    CASE (iFlow)  ! 'Flow'
      Call SetActuatedBranchFlowRate(ControllerProps(ControlNum)%NextActuatedValue,  &
                                   ControllerProps(ControlNum)%ActuatedNode ,      &
                                   ControllerProps(ControlNum)%ActuatedNodePlantLoopNum, &
                                   ControllerProps(ControlNum)%ActuatedNodePlantLoopSide, &
                                   ControllerProps(ControlNum)%ActuatedNodePlantLoopBranchNum, &
                                   .FALSE.)
!     Node(ActuatedNode)%MassFlowRate = ControllerProps(ControlNum)%NextActuatedValue

    CASE DEFAULT
      CALL ShowFatalError( &
        'UpdateController: Invalid Actuator Variable Type='// &
        TRIM(ControlVariableTypes(ControllerProps(ControlNum)%ActuatorVar)) &
      )
  END SELECT

  RETURN
END SUBROUTINE UpdateController

!        End of Update subroutines for the Controller Module
! *****************************************************************************


! Beginning of Reporting subroutines for the Controller Module
! *****************************************************************************

SUBROUTINE ReportController(ControlNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ControlNum !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

 ! Still needs to report the Controller power from this component
 ! Write(*,*)=ControllerProps(ControlNum)%ControllerPower

  RETURN
END SUBROUTINE ReportController

!        End of Reporting subroutines for the Controller Module
! *****************************************************************************

SUBROUTINE ExitCalcController(ControlNum, NextActuatedValue, Mode, IsConvergedFlag, IsUpToDateFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   February 06
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Only called when controller is considered as "converged", meaning that we do no longer
          ! need to continue iterating.
          !

          ! METHODOLOGY EMPLOYED:
          ! Updates:
          ! - next actuated value
          ! - controller mode
          ! - IsConvergedFlag
          ! - IsUpToDateFlag
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: ControlNum
  REAL(r64), INTENT(IN)        :: NextActuatedValue
  INTEGER, INTENT(IN)     :: Mode
  LOGICAL, INTENT(OUT)    :: IsConvergedFlag
  LOGICAL, INTENT(OUT)    :: IsUpToDateFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ControllerProps(ControlNum)%NextActuatedValue = NextActuatedValue
  ControllerProps(ControlNum)%Mode = Mode
  IsConvergedFlag = .TRUE.

  ! Set IsUpToDateFlag upon exiting to indicate caller whether or not the air loop needs to be
  ! re-simulated with the current candidate value, ie ControllerProps(ControlNum)%NextActuatedValue
  IF ( ControllerProps(ControlNum)%ActuatedValue /= ControllerProps(ControlNum)%NextActuatedValue ) THEN
    IsUpToDateFlag = .FALSE.
  ELSE
    IsUpToDateFlag = .TRUE.
  END IF

  RETURN
END SUBROUTINE ExitCalcController


! Beginning of Statistics subroutines for the Controller Module
! *****************************************************************************

SUBROUTINE TrackAirLoopControllers( &
              AirLoopNum, &
              WarmRestartStatus, &
              AirLoopIterMax, AirLoopIterTot, AirLoopNumCalls )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates runtime statistics for controllers on the specified air loop.
          ! Used to produce objective metrics when analyzing runtime performance
          ! of HVAC controllers for different implementations.
          !

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,  ONLY : NumPrimaryAirSys
  USE DataAirSystems,   ONLY : PrimaryAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT(IN)          :: AirLoopNum
  ! See CONTROLLER_WARM_RESTART_<> parameters in DataHVACControllers.f90
  ! If Status<0, no speculative warm restart.
  ! If Status==0, speculative warm restart failed.
  ! If Status>0, speculative warm restart succeeded.
  INTEGER, INTENT(IN)          :: WarmRestartStatus
  ! Max number of iterations performed by controllers on this air loop (per call to SimAirLoop)
  INTEGER, INTENT(IN)          :: AirLoopIterMax
  ! Aggregated number of iterations performed by controllers on this air loop (per call to SimAirLoop)
  INTEGER, INTENT(IN)          :: AirLoopIterTot
  ! Number of times SimAirLoopComponents() has been invoked
  INTEGER, INTENT(IN)          :: AirLoopNumCalls

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: ControllerNum

          ! FLOW

  ! If no controllers on this air loop then we have nothig to do
  IF ( PrimaryAirSystem(AirLoopNum)%NumControllers == 0 ) RETURN
  ! To avoid tracking statistics in case of no air loop or no HVAC controllers are defined
  IF ( NumAirLoopStats == 0 ) RETURN

  ! Update performance statistics for air loop
  AirLoopStats(AirLoopNum)%NumCalls = AirLoopStats(AirLoopNum)%NumCalls + 1

  SELECT CASE (WarmRestartStatus)
  CASE (iControllerWarmRestartSuccess)
    AirLoopStats(AirLoopNum)%NumSuccessfulWarmRestarts = &
      AirLoopStats(AirLoopNum)%NumSuccessfulWarmRestarts + 1
  CASE (iControllerWarmRestartFail)
    AirLoopStats(AirLoopNum)%NumFailedWarmRestarts = &
      AirLoopStats(AirLoopNum)%NumFailedWarmRestarts + 1
  CASE DEFAULT
    ! Nothing to do if no speculative warm restart used
  END SELECT

  AirLoopStats(AirLoopNum)%TotSimAirLoopComponents = &
    AirLoopStats(AirLoopNum)%TotSimAirLoopComponents + AirLoopNumCalls

  AirLoopStats(AirLoopNum)%MaxSimAirLoopComponents = MAX( &
    AirLoopStats(AirLoopNum)%MaxSimAirLoopComponents, &
    AirLoopNumCalls &
  )

  AirLoopStats(AirLoopNum)%TotIterations = &
    AirLoopStats(AirLoopNum)%TotIterations + AirLoopIterTot

  AirLoopStats(AirLoopNum)%MaxIterations = MAX( &
    AirLoopStats(AirLoopNum)%MaxIterations, &
    AirLoopIterMax &
  )

  ! Update performance statistics for each controller on air loop
  DO ControllerNum=1,PrimaryAirSystem(AirLoopNum)%NumControllers
    CALL TrackAirLoopController( AirLoopNum, ControllerNum )
  END DO

  RETURN
END SUBROUTINE TrackAirLoopControllers


SUBROUTINE TrackAirLoopController( AirLoopNum, AirLoopControlNum )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates runtime statistics for the specified controller.
          ! Used to produce objective metrics when analyzing runtime performance
          ! of HVAC controllers for different implementations.
          !

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals,  ONLY : NumPrimaryAirSys
  USE DataAirSystems,   ONLY : PrimaryAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT(IN)     :: AirLoopNum         ! Air loop index
  INTEGER, INTENT(IN)     :: AirLoopControlNum  ! Controller index on this air loop

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  ! Corresponding index in ControllerProps array
  INTEGER                      :: ControlIndex
  ! Number of iterations needed to solve this controller
  INTEGER                      :: IterationCount
  ! Current operating mode
  INTEGER                      :: Mode

          ! FLOW

  ControlIndex = PrimaryAirSystem(AirLoopNum)%ControllerIndex(AirLoopControlNum)

  ! We use NumCalcCalls instead of the iteration counter used in SolveAirLoopControllers()
  ! to avoid having to call TrackAirLoopController() directly from SolveAirLoopControllers().
  !
  ! The 2 counters should be the same anyway as NumCalcCalls is first reset to zero and
  ! incremented each time ManageControllers() is invoked with iControllerOpIterate
  IterationCount = ControllerProps(ControlIndex)%NumCalcCalls
  Mode = ControllerProps(ControlIndex)%Mode

  IF ( Mode /= iModeNone ) THEN

    AirLoopStats(AirLoopNum)%ControllerStats(AirLoopControlNum)%NumCalls(Mode) = &
      AirLoopStats(AirLoopNum)%ControllerStats(AirLoopControlNum)%NumCalls(Mode) + 1

    AirLoopStats(AirLoopNum)%ControllerStats(AirLoopControlNum)%TotIterations(Mode) = &
      AirLoopStats(AirLoopNum)%ControllerStats(AirLoopControlNum)%TotIterations(Mode) + &
      IterationCount

    AirLoopStats(AirLoopNum)%ControllerStats(AirLoopControlNum)%MaxIterations(Mode) = MAX( &
      AirLoopStats(AirLoopNum)%ControllerStats(AirLoopControlNum)%MaxIterations(Mode), &
      IterationCount &
    )

  END IF

  RETURN
END SUBROUTINE TrackAirLoopController


SUBROUTINE DumpAirLoopStatistics

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Writes runtime statistics for controllers on all air loops
          ! to a CSV file named "statistics.HVACControllers.csv".

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSystemVariables, ONLY : TrackAirLoopEnvFlag
  USE DataHVACGlobals,     ONLY : NumPrimaryAirSys
  USE DataAirSystems,      ONLY : PrimaryAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTEGER, EXTERNAL            :: GetNewUnitNumber

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(LEN=MaxNameLength) :: StatisticsFileName
  INTEGER                      :: FileUnit
  INTEGER                      :: AirLoopNum

          ! FLOW

  ! Detect if statistics have been generated or not for this run
  IF ( .NOT.TrackAirLoopEnvFlag ) THEN
    RETURN
  END IF

  StatisticsFileName(:) = ' '
  StatisticsFileName = 'statistics.HVACControllers.csv'
  StatisticsFileName = ADJUSTL(StatisticsFileName)

  FileUnit = GetNewUnitNumber()

  IF ( FileUnit <= 0 ) THEN
    CALL ShowWarningError( &
      'DumpAirLoopStatistics: Invalid unit for air loop statistics file="'// &
      TRIM(StatisticsFileName)//'"')
    RETURN
  END IF

  OPEN(UNIT=FileUnit, FILE=StatisticsFileName, Action='write', ERR=100)

  DO AirLoopNum=1,NumPrimaryAirSys
    CALL WriteAirLoopStatistics( FileUnit, PrimaryAirSystem(AirLoopNum), AirLoopStats(AirLoopNum) )
  END DO

  CLOSE(FileUnit)

  RETURN

100 CONTINUE
  CALL ShowFatalError( &
    'DumpAirLoopStatistics: Failed to open statistics file "'//TRIM(StatisticsFileName)//'" for output (write).' &
  )

  RETURN
END SUBROUTINE DumpAirLoopStatistics


SUBROUTINE WriteAirLoopStatistics( FileUnit, ThisPrimaryAirSystem, ThisAirLoopStats )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Writes runtime statistics for controllers on the specified air loop
          ! to the specified file.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirSystems
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT(IN)                      :: FileUnit
  TYPE(DefinePrimaryAirSystem), INTENT(IN) :: ThisPrimaryAirSystem
  TYPE(AirLoopStatsType), INTENT(IN)       :: ThisAirLoopStats

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: AirLoopControlNum
  INTEGER                      :: NumWarmRestarts
  REAL(r64)                    :: WarmRestartSuccessRatio
  INTEGER                      :: NumCalls
  INTEGER                      :: TotIterations
  INTEGER                      :: MaxIterations
  REAL(r64)                    :: AvgIterations
  INTEGER                      :: iModeNum

          ! FLOW

  WRITE(FileUnit,'(A,A)') TRIM(ThisPrimaryAirSystem%Name), ','

  ! Number of calls to SimAirLoop() has been invoked over the course of the simulation
  ! to simulate the specified air loop
  WRITE(FileUnit,'(A,A,A)')  'NumCalls', ',', TRIM(TrimSigDigits(ThisAirLoopStats%NumCalls))

  ! Warm restart success ratio
  NumWarmRestarts = ThisAirLoopStats%NumSuccessfulWarmRestarts + ThisAirLoopStats%NumFailedWarmRestarts
  IF ( NumWarmRestarts == 0 ) THEN
    WarmRestartSuccessRatio = 0.0d0
  ELSE
    WarmRestartSuccessRatio = REAL(ThisAirLoopStats%NumSuccessfulWarmRestarts,r64) / REAL(NumWarmRestarts,r64)
  END IF

  WRITE(FileUnit,'(A,A,A)') 'NumWarmRestarts', ',', TRIM(TrimSigDigits(NumWarmRestarts))
  WRITE(FileUnit,'(A,A,A)') 'NumSuccessfulWarmRestarts', ',', TRIM(TrimSigDigits(ThisAirLoopStats%NumSuccessfulWarmRestarts))
  WRITE(FileUnit,'(A,A,A)') 'NumFailedWarmRestarts', ',', TRIM(TrimSigDigits(ThisAirLoopStats%NumFailedWarmRestarts))
  WRITE(FileUnit,'(A,A,A)') 'WarmRestartSuccessRatio', ',', TRIM(TrimSigDigits(WarmRestartSuccessRatio,10))

  ! Total number of times SimAirLoopComponents() has been invoked over the course of the simulation
  ! to simulate the specified air loop
  WRITE(FileUnit,'(A,A,A)')  'TotSimAirLoopComponents', ',', TRIM(TrimSigDigits(ThisAirLoopStats%TotSimAirLoopComponents))
  ! Maximum number of times SimAirLoopComponents() has been invoked over the course of the simulation
  ! to simulate the specified air loop
  WRITE(FileUnit,'(A,A,A)')  'MaxSimAirLoopComponents', ',', TRIM(TrimSigDigits(ThisAirLoopStats%MaxSimAirLoopComponents))

  ! Aggregated number of iterations needed by all controllers to simulate the specified air loop
  WRITE(FileUnit,'(A,A,A)')  'TotIterations', ',', TRIM(TrimSigDigits(ThisAirLoopStats%TotIterations))
  ! Maximum number of iterations needed by controllers to simulate the specified air loop
  WRITE(FileUnit,'(A,A,A)')  'MaxIterations', ',', TRIM(TrimSigDigits(ThisAirLoopStats%MaxIterations))

  ! Average number of iterations needed by controllers to simulate the specified air loop
  IF ( ThisAirLoopStats%NumCalls == 0 ) THEN
    AvgIterations = 0.0d0
  ELSE
    AvgIterations = REAL(ThisAirLoopStats%TotIterations,r64)/REAL(ThisAirLoopStats%NumCalls,r64)
  END IF

  WRITE(FileUnit,'(A,A,A)')  'AvgIterations', ',', TRIM(TrimSigDigits(AvgIterations,10))


  ! Dump statistics for each controller on this air loop
  DO AirLoopControlNum=1,ThisPrimaryAirSystem%NumControllers

    WRITE(FileUnit,'(A,A)') TRIM(ThisPrimaryAirSystem%ControllerName(AirLoopControlNum)), ','

    ! Aggregate iteration trackers across all operating modes
    NumCalls = 0
    TotIterations = 0
    MaxIterations = 0

    DO iModeNum=iFirstMode,iLastMode
      NumCalls = NumCalls + ThisAirLoopStats%ControllerStats(AirLoopControlNum)%NumCalls(iModeNum)

      TotIterations = TotIterations + ThisAirLoopStats%ControllerStats(AirLoopControlNum)%TotIterations(iModeNum)

      MaxIterations = MAX( &
        MaxIterations, &
        ThisAirLoopStats%ControllerStats(AirLoopControlNum)%MaxIterations(iModeNum) &
      )
    END DO

    ! Number of times this controller was simulated (should match air loop num calls)
    WRITE(FileUnit,'(A,A,A)')  'NumCalls', ',', TRIM(TrimSigDigits(NumCalls))
    ! Aggregated number of iterations needed by this controller
    WRITE(FileUnit,'(A,A,A)')  'TotIterations', ',', TRIM(TrimSigDigits(TotIterations))
    ! Aggregated number of iterations needed by this controller
    WRITE(FileUnit,'(A,A,A)')  'MaxIterations', ',', TRIM(TrimSigDigits(MaxIterations))

    ! Average number of iterations needed by controllers to simulate the specified air loop
    IF ( NumCalls == 0 ) THEN
      AvgIterations = 0.0d0
    ELSE
      AvgIterations = REAL(TotIterations,r64) / REAL(NumCalls,r64)
    END IF
    WRITE(FileUnit,'(A,A,A)')  'AvgIterations', ',', TRIM(TrimSigDigits(AvgIterations,10))


    ! Dump iteration trackers for each operating mode
    DO iModeNum=iFirstMode,iLastMode

      WRITE(FileUnit,'(A,A)') ControllerModeTypes(iModeNum), ','

      ! Number of times this controller operated in this mode
      WRITE(FileUnit,'(A,A,A)')  'NumCalls', ',', &
        TRIM(TrimSigDigits(ThisAirLoopStats%ControllerStats(AirLoopControlNum)%NumCalls(iModeNum)))

      ! Aggregated number of iterations needed by this controller
      WRITE(FileUnit,'(A,A,A)')  'TotIterations', ',', &
        TRIM(TrimSigDigits(ThisAirLoopStats%ControllerStats(AirLoopControlNum)%TotIterations(iModeNum)))
      ! Aggregated number of iterations needed by this controller
      WRITE(FileUnit,'(A,A,A)')  'MaxIterations', ',', &
        TRIM(TrimSigDigits(ThisAirLoopStats%ControllerStats(AirLoopControlNum)%MaxIterations(iModeNum)))

      ! Average number of iterations needed by controllers to simulate the specified air loop
      IF ( ThisAirLoopStats%ControllerStats(AirLoopControlNum)%NumCalls(iModeNum) == 0 ) THEN
        AvgIterations = 0.0d0
      ELSE
        AvgIterations = &
          REAL(ThisAirLoopStats%ControllerStats(AirLoopControlNum)%TotIterations(iModeNum),r64) / &
          REAL(ThisAirLoopStats%ControllerStats(AirLoopControlNum)%NumCalls(iModeNum),r64)
      END IF
      WRITE(FileUnit,'(A,A,A)')  'AvgIterations', ',', TRIM(TrimSigDigits(AvgIterations,10))

    END DO

  END DO

  RETURN
END SUBROUTINE WriteAirLoopStatistics


! Beginning of Tracing subroutines for the Controller Module
! *****************************************************************************

SUBROUTINE SetupAirLoopControllersTracer( AirLoopNum )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Opens main trace file for controllers on specific air loop
          ! and writes header row with titles.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirSystems, ONLY : PrimaryAirSystem
  USE General, ONLY: TrimSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT(IN)             :: AirLoopNum

          ! INTERFACE BLOCK SPECIFICATIONS
  INTEGER, EXTERNAL               :: GetNewUnitNumber

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(LEN=MaxNameLength)    :: TraceFileName
  INTEGER                         :: TraceFileUnit
  INTEGER                         :: ControllerNum


  ! Open main controller trace file for each air loop
  TraceFileName(:) = ' '
  TraceFileName = 'controller.'//TRIM(PrimaryAirSystem(AirLoopNum)%Name)//'.csv'
  TraceFileName = ADJUSTL(TraceFileName)

  TraceFileUnit = GetNewUnitNumber()

  IF ( TraceFileUnit <= 0 ) THEN
    CALL ShowWarningError( &
      'SetupAirLoopControllersTracer: Invalid unit for air loop controllers trace file="'// &
      TRIM(TraceFileName)//'"')
    RETURN
  END IF

  ! Store file unit in air loop stats
  AirLoopStats(AirLoopNum)%TraceFileUnit = TraceFileUnit

  OPEN(UNIT=TraceFileUnit, FILE=TraceFileName, Action='write', ERR=100)


  ! List all controllers and their corrresponding handles into main trace file
  WRITE(TraceFileUnit,'(2(A,A))') &
    'Num', ',', &
    'Name', ','

  DO ControllerNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers
    WRITE(TraceFileUnit,'(1(A,A),1(A,A))') &
      TRIM(TrimSigDigits(ControllerNum)), ',', &
      PrimaryAirSystem(AirLoopNum)%ControllerName(ControllerNum), ','
      ! SAME AS ControllerProps(ControllerIndex)%ControllerName BUT NOT YET AVAILABLE
  END DO

  ! Skip a bunch of lines
  WRITE(TraceFileUnit,*)
  WRITE(TraceFileUnit,*)
  WRITE(TraceFileUnit,*)


  ! Write column header in main contoller trace file
  WRITE(TraceFileUnit,'(12(A,A))',ADVANCE='No') &
    'ZoneSizingCalc', ',', &
    'SysSizingCalc', ',', &
    'EnvironmentNum', ',', &
    'WarmupFlag', ',', &
    'SysTimeStamp', ',', &
    'SysTimeInterval', ',', &
    'BeginTimeStepFlag', ',', &
    'FirstTimeStepSysFlag', ',', &
    'FirstHVACIteration', ',', &
    'AirLoopPass', ',', &
    'AirLoopNumCallsTot', ',', &
    'AirLoopConverged', ','

  ! Write headers for final state
  DO ControllerNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers
    WRITE(TraceFileUnit,'(5(A,A,A))',ADVANCE='No') &
      'Mode', TRIM(TrimSigDigits(ControllerNum)), ',', &
      'IterMax', TRIM(TrimSigDigits(ControllerNum)), ',', &
      'XRoot', TRIM(TrimSigDigits(ControllerNum)), ',', &
      'YRoot', TRIM(TrimSigDigits(ControllerNum)), ',', &
      'YSetPoint', TRIM(TrimSigDigits(ControllerNum)), ','
  END DO

  ! Finally goto next line
  WRITE(TraceFileUnit,*)

  RETURN

100 CONTINUE
  CALL ShowFatalError( &
    'SetupAirLoopControllersTracer: Failed to open air loop trace file "'//TRIM(TraceFileName)//'" for output (write).' &
  )

  RETURN
END SUBROUTINE SetupAirLoopControllersTracer


SUBROUTINE TraceAirLoopControllers( &
              FirstHVACIteration, AirLoopNum, AirLoopPass, AirLoopConverged, &
              AirLoopNumCalls )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes diagnostic to the trace file attached to each air loop.
          !

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirSystems, ONLY : PrimaryAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)     :: FirstHVACIteration
  INTEGER, INTENT(IN)     :: AirLoopNum
  INTEGER, INTENT(IN)     :: AirLoopPass
  ! TRUE when primary air system & controllers simulation has converged;
  LOGICAL, INTENT(IN)     :: AirLoopConverged
  ! Number of times SimAirLoopComponents() has been invoked
  INTEGER, INTENT(IN)     :: AirLoopNumCalls

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                 :: ControllerNum
  INTEGER                 :: TraceFileUnit

          ! FLOW

  ! IF no controllers on this air loop then we have nothig to do
  IF ( PrimaryAirSystem(AirLoopNum)%NumControllers == 0 ) RETURN
  ! To avoid tracking statistics in case of no air loop or no HVAC controllers are defined
  IF ( NumAirLoopStats == 0 ) RETURN

  ! Setup trace file on first call only
  IF ( AirLoopStats(AirLoopNum)%FirstTraceFlag ) THEN
    CALL SetupAirLoopControllersTracer( AirLoopNum )

    AirLoopStats(AirLoopNum)%FirstTraceFlag = .FALSE.
  END IF

  TraceFileUnit = AirLoopStats(AirLoopNum)%TraceFileUnit

  IF ( TraceFileUnit <= 0 ) RETURN

  ! Write iteration stamp first
  CALL TraceIterationStamp( &
    TraceFileUnit, &
    FirstHVACIteration, AirLoopPass, AirLoopConverged, &
    AirLoopNumCalls )

  ! Loop over the air sys controllers and write diagnostic to trace file
  DO ControllerNum = 1,PrimaryAirSystem(AirLoopNum)%NumControllers

    CALL TraceAirLoopController( &
      TraceFileUnit, &
      PrimaryAirSystem(AirLoopNum)%ControllerIndex(ControllerNum) )

  END DO

  ! Go to next line
  WRITE(TraceFileUnit,*)

  RETURN
END SUBROUTINE TraceAirLoopControllers


SUBROUTINE TraceIterationStamp( &
            TraceFileUnit, &
            FirstHVACIteration, AirLoopPass, AirLoopConverged, &
            AirLoopNumCalls )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Writes current iteration time stamp to specified trace file.
          !

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,        ONLY : CurEnvirNum, CurMnDy
  USE DataGlobals,            ONLY : ZoneSizingCalc, SysSizingCalc, WarmupFlag, BeginTimeStepFlag
  USE DataHVACGlobals,        ONLY : FirstTimeStepSysFlag
  USE General,                ONLY: TrimSigDigits, LogicalToInteger

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, INTENT(IN)     :: TraceFileUnit
  LOGICAL, INTENT(IN)     :: FirstHVACIteration
  INTEGER, INTENT(IN)     :: AirLoopPass
  ! TRUE when primary air system and controllers simulation has converged;
  LOGICAL, INTENT(IN)     :: AirLoopConverged
  ! Number of times SimAirLoopComponents() has been invoked
  INTEGER, INTENT(IN)     :: AirLoopNumCalls

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  ! Write step stamp to air loop trace file after reset
  ! Note that we do not go to the next line
  WRITE(TraceFileUnit,'(4(A,A),2(A,A),6(A,A))',ADVANCE='No') &
      TRIM(TrimSigDigits(LogicalToInteger(ZoneSizingCalc))), ',', &
      TRIM(TrimSigDigits(LogicalToInteger(SysSizingCalc))), ',', &
      TRIM(TrimSigDigits(CurEnvirNum)), ',', &
      TRIM(TrimSigDigits(LogicalToInteger(WarmupFlag))), ',', &
      TRIM(CreateHVACTimeString()), ',', &
      TRIM(MakeHVACTimeIntervalString()), ',', &
      TRIM(TrimSigDigits(LogicalToInteger(BeginTimeStepFlag))), ',', &
      TRIM(TrimSigDigits(LogicalToInteger(FirstTimeStepSysFlag))), ',', &
      TRIM(TrimSigDigits(LogicalToInteger(FirstHVACIteration))), ',', &
      TRIM(TrimSigDigits(AirLoopPass)), ',', &
      TRIM(TrimSigDigits(AirLoopNumCalls)), ',', &
      TRIM(TrimSigDigits(LogicalToInteger(AirLoopConverged))), ','

  RETURN
END SUBROUTINE TraceIterationStamp


SUBROUTINE TraceAirLoopController( TraceFileUnit, ControlNum )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes convergence diagnostic to the air loop trace file
          ! for the specified controller index.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: TraceFileUnit
  INTEGER, INTENT(IN)   :: ControlNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: ActuatedNode
  INTEGER               :: SensedNode


  ! Set the sensed and actuated node numbers
  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode

  WRITE(TraceFileUnit,'(2(A,A),3(A,A))',ADVANCE='No')  &
    TRIM(TrimSigDigits(ControllerProps(ControlNum)%Mode)), ',', &         ! controller mode for current step
    TRIM(TrimSigDigits(ControllerProps(ControlNum)%NumCalcCalls)), ',', & ! number of Sim() calls since last reset
    TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRate,10)), ',', &          ! X = actuated variable
    TRIM(TrimSigDigits(Node(SensedNode)%Temp,10)), ',', &                    ! Y = sensed variable
    TRIM(TrimSigDigits(Node(SensedNode)%TempSetPoint,10)), ','               ! desired setpoint

  RETURN
END SUBROUTINE  TraceAirLoopController


SUBROUTINE SetupIndividualControllerTracer(ControlNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Opens individual controller trace file for the specified controller
          ! and writes header row.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE RootFinder,           ONLY:  WriteRootFinderTraceHeader

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)             :: ControlNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTEGER, EXTERNAL               :: GetNewUnitNumber

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(LEN=MaxNameLength)    :: TraceFileName
  INTEGER                         :: TraceFileUnit = 0


  ! Open and write column header in trace file for each individual controller
  TraceFileUnit = GetNewUnitNumber()

  IF ( TraceFileUnit <= 0 ) THEN
    CALL ShowFatalError( &
      'SetupIndividualControllerTracer: Invalid unit (<=0) for setting up controller trace file')
    RETURN
  END IF

  TraceFileName(:) = ' '
  TraceFileName = 'controller.'//TRIM(ControllerProps(ControlNum)%ControllerName)//'.csv'
  TraceFileName = ADJUSTL(TraceFileName)

  !WRITE(*,*) 'Trace file name="', TRIM(TraceFileName) , '"'
  OPEN(UNIT=TraceFileUnit, FILE=TraceFileName, Action='write', ERR=100)

  ! Store trace file unit
  ControllerProps(ControlNum)%TraceFileUnit = TraceFileUnit

  ! Write header row
  WRITE(TraceFileUnit,'(19(A,A))',ADVANCE='No') &
    'EnvironmentNum', ',',                    &
    'WarmupFlag', ',',                        &
    'SysTimeStamp', ',',                      &
    'SysTimeInterval', ',',                   &
    'AirLoopPass', ',',                       &
    'FirstHVACIteration', ',',                &
    'Operation', ',',                         &
    'NumCalcCalls', ',',                      &
    ! Masss flow rate
    'SensedNode%MassFlowRate', ',',           &
    'ActuatedNode%MassFlowRateMinAvail', ',', &
    'ActuatedNode%MassFlowRateMaxAvail', ',', &
    ! Convergence analysis
    'X', ',',                                 &
    'Y', ',',                                 &
    'Setpoint', ',',                          &
    'DeltaSensed', ',',                       &
    'Offset', ',',                            &
    'Mode', ',',                              &
    'IsConvergedFlag', ',',                   &
    'NextActuatedValue', ','

  CALL WriteRootFinderTraceHeader( TraceFileUnit )

  ! Finally skip line
  WRITE(TraceFileUnit,*)

  RETURN

100 CONTINUE
  CALL ShowFatalError( &
    'SetupIndividualControllerTracer: Failed to open controller trace file "'//TRIM(TraceFileName)//'" for output (write).' &
  )

  RETURN
END SUBROUTINE SetupIndividualControllerTracer


SUBROUTINE TraceIndividualController( &
            ControlNum, &
            FirstHVACIteration, AirLoopPass, &
            Operation, IsConvergedFlag )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine writes convergence diagnostic to the trace file for the specified
          ! controller.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,        ONLY : CurEnvirNum
  USE General,                ONLY : TrimSigDigits,LogicalToInteger
  USE RootFinder,             ONLY : WriteRootFinderTrace

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: ControlNum
  LOGICAL, INTENT(IN)   :: FirstHVACIteration
  INTEGER, INTENT(IN)   :: AirLoopPass
  INTEGER, INTENT(IN)   :: Operation         ! Operation to execute
  LOGICAL, INTENT(IN)   :: IsConvergedFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: TraceFileUnit
  INTEGER               :: ActuatedNode
  INTEGER               :: SensedNode
  LOGICAL               :: SkipLineFlag

  ! Setup individual trace file on first trace only
  IF ( ControllerProps(ControlNum)%FirstTraceFlag ) THEN
    CALL SetupIndividualControllerTracer(ControlNum)

    ControllerProps(ControlNum)%FirstTraceFlag = .FALSE.
    SkipLineFlag = .FALSE.
  ELSE
    SkipLineFlag = FirstHVACIteration .AND. (ControllerProps(ControlNum)%NumCalcCalls==0)
  END IF

  TraceFileUnit = ControllerProps(ControlNum)%TraceFileUnit

  ! Nothing to do if trace file not registered
  IF ( TraceFileUnit <= 0 ) RETURN

  ! Skip a line before each new HVAC step
  IF ( SkipLineFlag ) THEN
    WRITE(TraceFileUnit,*)
  END IF

  ! Set the sensed and actuated node numbers
  ActuatedNode = ControllerProps(ControlNum)%ActuatedNode
  SensedNode   = ControllerProps(ControlNum)%SensedNode

  ! Write iteration stamp
  WRITE(TraceFileUnit,'(2(A,A),2(A,A),4(A,A))',ADVANCE='No')   &
    TRIM(TrimSigDigits(CurEnvirNum)), ',',                             &
    TRIM(TrimSigDigits(LogicalToInteger(WarmupFlag))), ',',           &
    TRIM(CreateHVACTimeString()), ',',            &
    TRIM(MakeHVACTimeIntervalString()), ',',    &
    TRIM(TrimSigDigits(AirLoopPass)), ',',                             &
    TRIM(TrimSigDigits(LogicalToInteger(FirstHVACIteration))), ',',   &
    TRIM(TrimSigDigits(Operation)), ',',                               &
    TRIM(TrimSigDigits(ControllerProps(ControlNum)%NumCalcCalls)), ','

  ! Write detailed diagnostic
  SelectOperation: SELECT CASE (Operation)
  CASE (iControllerOpColdStart, iControllerOpWarmRestart)

    WRITE(TraceFileUnit, '(3(A,A),3(A,A),2(A,A),2(A,A),1(A,A))',ADVANCE='No') &
      ! Masss flow rate
      TRIM(TrimSigDigits(Node(SensedNode)%MassFlowRate,10)), ',',                    &
      TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRateMinAvail,10)), ',',          &
      TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRateMaxAvail,10)), ',',          &
      ! Convergence analysis
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%ActuatedValue,10)), ',',        & ! X
      TRIM(TrimSigDigits(Node(SensedNode)%Temp,10)), ',',                            & ! Y
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,10)), ',',        & ! setpoint
      ' ', ',',                                              & ! DeltaSensed = Y - YRoot
      ' ', ',',                                              & ! Offset
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%Mode)), ',',                 & ! Mode
      TRIM(TrimSigDigits(LogicalToInteger(IsConvergedFlag))), ',',               & ! IsConvergedFlag
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%NextActuatedValue,10)), ','

    ! No trace available for root finder yet
    ! Skip call to WriteRootFinderTrace()

    ! Finally skip line
    WRITE(TraceFileUnit,*)

  CASE (iControllerOpIterate)
    WRITE(TraceFileUnit, '(8(A,A),2(A,A),1(A,A))',ADVANCE='No') &
      ! Masss flow rate
      TRIM(TrimSigDigits(Node(SensedNode)%MassFlowRate,10)), ',',                    &
      TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRateMinAvail,10)), ',',          &
      TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRateMaxAvail,10)), ',',          &
      ! Convergence analysis
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%ActuatedValue,10)), ',',        & ! X
      TRIM(TrimSigDigits(Node(SensedNode)%Temp,10)), ',',                            & ! Y
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,10)), ',',        & ! setpoint
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%DeltaSensed,10)), ',',          & ! DeltaSensed = Y - YRoot
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%Offset,10)), ',',               & ! Offset
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%Mode)), ',',                 & ! Mode
      TRIM(TrimSigDigits(LogicalToInteger(IsConvergedFlag))), ',',               & ! IsConvergedFlag
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%NextActuatedValue,10)), ','

    ! Append trace for root finder
    CALL WriteRootFinderTrace( TraceFileUnit, RootFinders(ControlNum) )

    ! Finally skip line
    WRITE(TraceFileUnit,*)

  CASE (iControllerOpEnd)
    WRITE(TraceFileUnit, '(3(A,A),5(A,A),2(A,A),1(A,A))',ADVANCE='No') &
      ! Masss flow rate
      TRIM(TrimSigDigits(Node(SensedNode)%MassFlowRate,10)), ',',                    &
      TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRateMinAvail,10)), ',',          &
      TRIM(TrimSigDigits(Node(ActuatedNode)%MassFlowRateMaxAvail,10)), ',',          &
      ! Convergence analysis
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%ActuatedValue,10)), ',',        & ! X
      TRIM(TrimSigDigits(Node(SensedNode)%Temp,10)), ',',                            & ! Y
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%SetPointValue,10)), ',',        & ! setpoint
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%DeltaSensed,10)), ',',          & ! DeltaSensed = Y - YRoot
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%Offset,10)), ',',               & ! Offset
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%Mode)), ',',                 & ! Mode
      TRIM(TrimSigDigits(LogicalToInteger(IsConvergedFlag))), ',',               & ! IsConvergedFlag
      TRIM(TrimSigDigits(ControllerProps(ControlNum)%NextActuatedValue,10)), ','

    ! No trace available for root finder yet
    ! Skip call to WriteRootFinderTrace()

    ! Finally skip line
    WRITE(TraceFileUnit,*)

    ! Skip an additional line to indicate end of current HVAC step
    WRITE(TraceFileUnit,*)

  CASE DEFAULT
    ! Should never happen
    CALL ShowFatalError( &
      'TraceIndividualController: Invalid Operation passed='//TRIM(TrimSigDigits(Operation))// &
      ', Controller name='//TRIM(ControllerProps(ControlNum)%ControllerName) &
    )

  END SELECT SelectOperation

  RETURN
END SUBROUTINE  TraceIndividualController


FUNCTION CreateHVACTimeString() RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function creates a string describing the current time stamp of the system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,        ONLY : CurMnDy
  USE General,   ONLY : CreateTimeString, &
                        GetCurrentHVACTime

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=32) :: OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=32) Buffer


  Buffer = CreateTimeString(GetCurrentHVACTime())
  OutputString = TRIM(CurMnDy)//' '//TRIM(ADJUSTL(Buffer))

  RETURN
END FUNCTION CreateHVACTimeString


FUNCTION CreateHVACStepFullString() RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   April 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function creates a string describing the current HVAC step.
          ! It includes the environment name, the current day/month and the current
          ! time stamp for the system time step.
          !
          ! It is used in error messages only.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,        ONLY : EnvironmentName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(LEN=200) :: OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  OutputString = TRIM(EnvironmentName)//', '//MakeHVACTimeIntervalString()

  RETURN
END FUNCTION CreateHVACStepFullString


FUNCTION MakeHVACTimeIntervalString() RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Dimitri Curtil
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function creates a string describing the current time interval of the system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,   ONLY : CreateHVACTimeIntervalString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(LEN=52) :: OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  OutputString = ADJUSTL(CreateHVACTimeIntervalString())

  RETURN
END FUNCTION MakeHVACTimeIntervalString
!        End of Tracing subroutines for the Controller Module

SUBROUTINE CheckControllerListOrder

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Oct 10.
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! check that if multiple controllers on an air loop, that they aren't listed in a bad order
          ! CR 8253

          ! METHODOLOGY EMPLOYED:
          ! setup data for sensed nodes and compare positions if on the same branch

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirSystems,  ONLY: PrimaryAirSystem
  USE DataHVACGlobals, ONLY: NumPrimaryAirSys
  USE InputProcessor,  ONLY: SameString, FindItemInList
  USE DataInterfaces,  ONLY: ShowFatalError, ShowContinueError, ShowSevereError

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
  INTEGER :: AirSysNum
  INTEGER :: ContrlNum
  INTEGER :: WaterCoilContrlCount
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ContrlSensedNodeNums !array for storing sense node info
  INTEGER :: SensedNodeIndex
  INTEGER :: BranchNodeIndex
  INTEGER :: BranchNum
  INTEGER :: foundControl

  Do AirSysNum = 1, NumPrimaryAirSys

    IF (PrimaryAirSystem(AirSysNum)%NumControllers > 1) THEN
      ! first see how many are water coil controllers
      WaterCoilContrlCount = 0 !init
      DO ContrlNum =1, PrimaryAirSystem(AirSysNum)%NumControllers
        If (SameString(PrimaryAirSystem(AirSysNum)%ControllerType(ContrlNum), 'CONTROLLER:WATERCOIL')) THEN
          WaterCoilContrlCount = WaterCoilContrlCount + 1
        ENDIF
      ENDDO

      IF (WaterCoilContrlCount > 1) THEN
        ALLOCATE(ContrlSensedNodeNums(WaterCoilContrlCount, 3))
        ContrlSensedNodeNums=0
        SensedNodeIndex = 0
        DO ContrlNum =1, PrimaryAirSystem(AirSysNum)%NumControllers
          IF (SameString(PrimaryAirSystem(AirSysNum)%ControllerType(ContrlNum), 'CONTROLLER:WATERCOIL')) THEN
            SensedNodeIndex = SensedNodeIndex + 1
            foundControl = FindItemInList(PrimaryAirSystem(AirSysNum)%ControllerName(ContrlNum), &
                                             ControllerProps%ControllerName, NumControllers)
            IF (foundControl > 0) THEN
              ContrlSensedNodeNums(SensedNodeIndex, 1) = ControllerProps(foundControl)%SensedNode
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      !fill branch index for sensed nodes
      IF (ALLOCATED(ContrlSensedNodeNums)) THEN
        DO BranchNum = 1,PrimaryAirSystem(AirSysNum)%NumBranches
          DO SensedNodeIndex =1, WaterCoilContrlCount
            DO BranchNodeIndex = 1, PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%TotalNodes
              IF ( ContrlSensedNodeNums(SensedNodeIndex, 1) &
                   == PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%NodeNum(BranchNodeIndex)) THen
                ContrlSensedNodeNums(SensedNodeIndex, 2) = BranchNodeIndex
                ContrlSensedNodeNums(SensedNodeIndex, 3) = BranchNum
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      ! check if flow order doesn't agree with controller order
      IF (ALLOCATED(ContrlSensedNodeNums)) THEN
        DO SensedNodeIndex =1, WaterCoilContrlCount
          IF (SensedNodeIndex == 1) CYCLE
          IF (ContrlSensedNodeNums(SensedNodeIndex, 2) < ContrlSensedNodeNums(SensedNodeIndex-1, 2)) THEN
            !now see if on the same branch
            IF (ContrlSensedNodeNums(SensedNodeIndex, 3) == ContrlSensedNodeNums(SensedNodeIndex-1, 3)) THEN
              ! we have a flow order problem with water coil controllers
              CALL ShowSevereError('CheckControllerListOrder: A water coil controller list has the wrong order')
              CALL ShowContinueError('Check the AirLoopHVAC:ControllerList for the air loop called "' &
                                     //Trim(PrimaryAirSystem(AirSysNum)%Name)//'"')
              CALL ShowContinueError('When there are multiple Controller:WaterCoil objects for the same air loop, ' &
                                    //'they need to be listed in the proper order.')
              CALL ShowContinueError('The controllers should be listed in natural flow order with those for upstream' &
                                   //' coils listed before those for downstream coils.')
              CALL ShowContinueError('The sensed nodes specified for the respective controllers should also reflect this order.')

            ENDIF

          ENDIF
        ENDDO
      ENDIF

      IF (ALLOCATED(ContrlSensedNodeNums)) DEALLOCATE(ContrlSensedNodeNums)

    ENDIF ! controllers > 1
  ENDDO

  RETURN

END SUBROUTINE CheckControllerListOrder

SUBROUTINE CheckCoilWaterInletNode(WaterInletNodeNum, NodeNotFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Heejin Cho
          !       DATE WRITTEN   November 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine checks that the water inlet node number is matched by
          ! the actuator node number of some water coil

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: WaterInletNodeNum   ! input actuator node number
  LOGICAL, INTENT(OUT) :: NodeNotFound      ! true if matching actuator node not found

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ControlNum

  IF (GetControllerInputFlag) THEN
    CALL GetControllerInput
    GetControllerInputFlag=.false.
  ENDIF

  NodeNotFound = .TRUE.
  DO ControlNum = 1,NumControllers
    IF (ControllerProps(ControlNum)%ActuatedNode == WaterInletNodeNum) THEN
        NodeNotFound = .FALSE.
    END IF
  END DO

  RETURN

END SUBROUTINE CheckCoilWaterInletNode

SUBROUTINE GetControllerActuatorNodeNum(ControllerName, WaterInletNodeNum, NodeNotFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   September 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the controllers actuator node number

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ControllerName    ! name of coil controller
  INTEGER, INTENT(OUT)         :: WaterInletNodeNum   ! input actuator node number
  LOGICAL, INTENT(OUT)         :: NodeNotFound      ! true if matching actuator node not found

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ControlNum

  IF (GetControllerInputFlag) THEN
    CALL GetControllerInput
    GetControllerInputFlag=.false.
  ENDIF

  NodeNotFound = .TRUE.
  ControlNum = FindItemInList(ControllerName,ControllerProps%ControllerName,NumControllers)
  IF( ControlNum > 0 .AND. ControlNum <= NumControllers) THEN
    WaterInletNodeNum = ControllerProps(ControlNum)%ActuatedNode
    NodeNotFound = .FALSE.
  END IF

  RETURN

END SUBROUTINE GetControllerActuatorNodeNum

! *****************************************************************************

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

END MODULE HVACControllers

