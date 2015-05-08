MODULE GroundHeatExchangers
          ! MODULE INFORMATION:
          !       AUTHOR         Arun Murugappan, Dan Fisher
          !       DATE WRITTEN   September 2000
          !       MODIFIED       B. Griffith, Sept 2010,plant upgrades
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! The module contains the data structures and routines to simulate the
          ! operation of vertical closed-loop ground heat exchangers (GLHE) typically
          ! used in low temperature geothermal heat pump systems.

          ! METHODOLOGY EMPLOYED:
          ! The borehole and fluid temperatures are calculated from the response to
          ! the current heat transfer rate and the response to the history of past
          ! applied heat pulses. The response to each pulse is calculated from a non-
          ! dimensionalized response function, or G-function, that is specific to the
          ! given borehole field arrangement, depth and spacing. The data defining
          ! this function is read from input.
          !
          ! The heat pulse histories need to be recorded over an extended period (months).
          ! To aid computational efficiency past pulses are continuously agregated into
          ! equivalent heat pulses of longer duration, as each pulse becomes less recent.


          ! REFERENCES:
          ! Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
          !   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
          ! Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
          !   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, BeginSimFlag, BeginEnvrnFlag, BeginTimeStepFlag, BeginHourFlag, HourOfDay, &
                       TimeStep,TimeStepZone,DayOfSim, PI, InitConvTemp, WarmUpFlag, SecInHour
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, ShowContinueError, &
                       SetupOutputVariable
USE DataHVACGlobals, ONLY: TimeStepSys ,SysTimeElapsed
USE DataLoopNode
USE General,  ONLY: TrimSigDigits

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public



  ! DERIVED TYPE DEFINITIONS
TYPE GlheSpecs
  CHARACTER(len=MaxNameLength)  :: Name               =' ' ! user identifier
  LOGICAL                       :: Available          =.false. ! need an array of logicals--load identifiers of available equipment
  LOGICAL                       :: ON                 =.false. ! simulate the machine at it's operating part load ratio
  REAL(r64)                     :: MaxGlheFlowRate    =0.0d0 ! design nominal capacity of Pump
  INTEGER                       :: MaxSimYears        =0   ! maximum length of simulation (years)
  INTEGER                       :: GlheInletNodeNum   =0   ! Node number on the inlet side of the plant
  INTEGER                       :: GlheOutletNodeNum  =0   ! Node number on the outlet side of the plant
  INTEGER                       :: NumBoreholes       =0
  REAL(r64)                     :: BoreholeLength     =0.0d0
  REAL(r64)                     :: BoreholeRadius     =0.0d0
  REAL(r64)                     :: KGround            =0.0d0 ! Thermal conductivity of the ground        [W/(mK)]
  REAL(r64)                     :: CpRhoGround        =0.0d0 ! Specific heat capacity of ground      [J/Kg/K]
  REAL(r64)                     :: TempGround         =0.0d0 ! The far feild temperature of the ground   [°C]
  REAL(r64)                     :: DesignFlow         =0.0d0 ! Design volumetric flow rate               [m3/S]
  REAL(r64)                     :: DesignMassFlow     =0.0d0 ! Design mass flow rate                    [kg/S]
  REAL(r64)                     :: KGrout             =0.0d0 ! Grout thermal conductivity                [W/(mK)]
  REAL(r64)                     :: KPipe              =0.0d0 ! Thermal Conductivity of the U tube        [W/(mK)]
  REAL(r64)                     :: PipeOutDia         =0.0d0 ! Outer diameter of the Pipe                [m]
  REAL(r64)                     :: UtubeDist          =0.0d0 ! Distance between the legs of the Utube    [m]
  REAL(r64)                     :: PipeThick          =0.0d0 ! Thickness of the pipe wall
  REAL(r64)                     :: gReferenceRatio    =0.0d0 ! Reference ratio for developing g-functions [-]
  INTEGER                       :: NPairs             =0   ! Number of pairs of Lntts and Gfunc
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: QnMonthlyAgg       ! Monthly aggregated normalised heat extraction/rejection rate [W/m]
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: QnHr               ! Hourly aggregated normalised heat extraction/rejection rate [W/m]
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: QnSubHr            ! Contains the subhourly heat extraction/rejection rate normalised
                                                      ! by the total active length of bore holes  [W/m]
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: LNTTS              ! natural log of Non Dimensional Time Ln(t/ts)
  REAL(r64),ALLOCATABLE,DIMENSION(:) :: GFNC               ! G-function ( Non Dimensional temperature response factors)
  INTEGER                       :: AGG                =0   ! Minimum Hourly Histroy required
  INTEGER                       :: SubAGG             =0   ! Minimum subhourly History
  INTEGER,ALLOCATABLE,DIMENSION(:) :: LastHourN            ! Stores the Previous hour's N for past hours
                                                           ! until the minimum subhourly history
  !loop topology variables
  INTEGER                       :: LoopNum            =0
  INTEGER                       :: LoopSideNum        =0
  INTEGER                       :: BranchNum          =0
  INTEGER                       :: CompNum            =0
END TYPE GlheSpecs


TYPE ReportVars
  REAL(r64)                     :: GlheBoreholeTemp   =0.0d0 ! [°C]
  REAL(r64)                     :: GlheMassFlowRate   =0.0d0 ! [kg/s]
  REAL(r64)                     :: GlheOutletTemp     =0.0d0 ! [°C]
  REAL(r64)                     :: GlheInletTemp      =0.0d0 ! [°C]
  REAL(r64)                     :: GlheAveFluidTemp   =0.0d0 ! [°C]
  REAL(r64)                     :: QGlhe              =0.0d0 ! [W] heat transfer rate
END TYPE ReportVars

  ! MODULE PARAMETER DEFINITIONS
REAL(r64) , PARAMETER    :: HrsPerDay   = 24.d0   ! Number of hours in a day
REAL(r64) , PARAMETER    :: HrsPerMonth =730.0d0 ! Number of hours in month
INTEGER, PARAMETER       :: MaxTSinHr   = 60   ! Max number of time step in a hour

  ! MODULE VARIABLE DECLARATIONS:
INTEGER                         :: NumVerticalGlhes=0
INTEGER                         :: N               =1    ! COUNTER OF TIME STEP
REAL(r64)                       :: CurrentSimTime  =0.0d0  ! Current simulation time in hours
REAL(r64)                       :: GlheOutletTemp  =0.0d0  ! Outlet temperature of the fluid  [°C]
REAL(r64)                       :: GlheInletTemp   =0.0d0  ! Inlet temperature of the fluid   [°C]
REAL(r64)                       :: GlheMassFlowRate=0.0d0  ! Mass flowrate of the fluid       [Kg/s]
REAL(r64)                       :: QGlhe           =0.0d0  ! The normalised heat transfer rate[W/m]
REAL(r64)                       :: GlheRB          =0.0d0  ! [K per W/m] Just for Analyis will be removed later
REAL(r64)                       :: GlheAveFluidTemp=0.0d0  ! The average fluid temperature    [°C]
REAL(r64)                       :: GlheBoreholeTemp=0.0d0  ! The average borehole tempreature [°C]
INTEGER                         :: LocHourofDay    =0
INTEGER                         :: LocDayofSim     =0
REAL(r64),SAVE, ALLOCATABLE,DIMENSION(:):: LastQnSubHr   ! Previous time step Qn subhourly value
REAL(r64)                               :: MDotActual

REAL(r64), ALLOCATABLE,DIMENSION(:)  :: PrevTimeSteps  ! This is used to store only the Last Few time step's time
                                                      ! to enable the calculation of the subhouly contribution..
                                                      ! Recommended size, the product of Minimum subhourly history required and
                                                      ! the maximum no of system time steps in an hour

TYPE(GlheSpecs), ALLOCATABLE, DIMENSION(:)  :: VerticalGlhe         !dimension to number of machines
TYPE(ReportVars), ALLOCATABLE,DIMENSION(:)        :: VerticalGlheReport
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


          ! SUBROUTINE SPECIFICATIONS FOR MODULE CondenserTowers
PUBLIC     SimGroundHeatExchangers
PRIVATE    CalcVerticalGroundHeatExchanger
PRIVATE    GetGroundheatExchangerInput
PRIVATE    BoreholeResistance
PRIVATE    InitBoreholeHXSimVars
PRIVATE    CalcAggregateLoad
PRIVATE    UpdateVerticalGroundHeatExchanger

CONTAINS
          ! MODULE SUBROUTINES:

!******************************************************************************

SUBROUTINE SimGroundHeatExchangers(GlheType,GlheName, CompIndex,RunFlag, FirstIteration, InitLoopEquip)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    August, 2000
          !       MODIFIED         Arun Murugappan
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! mananges the simulation of the vertical closed-loop ground heat
          ! exchangers (GLHE) model

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
          !   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
          ! Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
          !   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

          ! USE STATEMENTS:

  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),INTENT(IN) :: GlheType
  CHARACTER(len=*),INTENT(IN) :: GlheName
  INTEGER,INTENT(INOUT)       :: CompIndex
  LOGICAL,INTENT(IN)          :: RunFlag
  LOGICAL, INTENT(IN)         :: FirstIteration
  LOGICAL,INTENT(IN)          :: InitLoopEquip

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE          :: GetInput = .TRUE.
  INTEGER                :: GlheNum


          !GET INPUT
  IF (GetInput) THEN
    CALL GetGroundheatExchangerInput
    GetInput = .FALSE.
  END IF

    ! Find the correct Furnace
  IF (CompIndex == 0) THEN
    GlheNum=FindItemInList(GlheName,VerticalGlhe%Name,NumVerticalGlhes)
    IF (GlheNum == 0) THEN
      CALL ShowFatalError('SimGroundHeatExchangers: Unit not found='//TRIM(GlheName))
    ENDIF
    CompIndex=GlheNum
  ELSE
    GlheNum=CompIndex
    IF (GlheNum > NumVerticalGlhes .or. GlheNum < 1) THEN
      CALL ShowFatalError('SimGroundHeatExchangers:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(GlheNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumVerticalGlhes))//  &
                          ', Entered Unit name='//TRIM(GlheName))
    ENDIF
    IF (CheckEquipName(GlheNum)) THEN
      IF (GlheName /= VerticalGlhe(GlheNum)%Name) THEN
        CALL ShowFatalError('SimGroundHeatExchangers: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(NumVerticalGlhes))// &
                            ', Unit name='//TRIM(GlheName)//', stored Unit Name for that index='//  &
                            TRIM(VerticalGlhe(GlheNum)%Name))
      ENDIF
      CheckEquipName(GlheNum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    CALL InitBoreholeHXSimVars(GlheNum,Runflag)
    RETURN
  ENDIF

          !INITIALIZE
  CALL InitBoreholeHXSimVars(GlheNum,Runflag)


           !SIMULATE HEAT EXCHANGER
  CALL CalcVerticalGroundHeatExchanger(GlheNum)
  CALL UpdateVerticalGroundHeatExchanger(RunFlag,GlheNum)

  RETURN
END SUBROUTINE SimGroundHeatExchangers

!******************************************************************************

SUBROUTINE CalcVerticalGroundHeatExchanger(GlheNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    August, 2000
          !       MODIFIED         Arun Murugappan
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the main routine to simulate the operation of vertical
          ! closed-loop ground heat exchangers (GLHE).

          ! METHODOLOGY EMPLOYED:
          ! The borehole and fluid temperatures are calculated from the response to
          ! the current heat transfer rate and the response to the history of past
          ! applied heat pulses. The response to each pulse is calculated from a non-
          ! dimensionalized response function, or G-function, that is specific to the
          ! given borehole field arrangement, depth and spacing. The data defining
          ! this function is read from input.
          !
          ! The heat pulse histories need to be recorded over an extended period (months).
          ! To aid computational efficiency past pulses are continuously agregated into
          ! equivalent heat pulses of longer duration, as each pulse becomes less recent.


          ! REFERENCES:
          ! Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
          !   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
          ! Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
          !   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

          ! USE STATEMENTS:
    USE DataPlant, ONLY : PlantLoop
    USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER                :: GlheNum


    !LOCAL BHORE HOLE PARAMETERS
    INTEGER                :: NumBholes
    REAL(r64)              :: FluidDensity
    REAL(r64)              :: BholeLength
    REAL(r64)              :: K_Ground
    REAL(r64)              :: K_Ground_Factor
    REAL(r64)              :: Cp_Fluid
    REAL(r64)              :: Tground
    REAL(r64)              :: ResistanceBhole           ! The thermal resistance of the borehole, (K per W/m]
    REAL(r64)              :: Gfuncval                  ! Interpolated G function value at a sub-hour
    REAL(r64)              :: ToutNew = 19.375d0
    REAL(r64)              :: FluidAveTemp
    REAL(r64)              :: GroundDiffusivity
    REAL(r64)              :: TimeSS                    !Steady state time
    REAL(r64)              :: TimeSS_Factor             !Steady state time factor for calculation
    REAL(r64)              :: XI
    REAL(r64)              :: C_1
    INTEGER                :: NumOfMonths               ! the number of months of simulation elapsed
    INTEGER                :: CurrentMonth              ! The Month upto which the Montly blocks are superposed
    REAL(r64)              :: SumQnMonthly              ! tmp variable which holds the sum of the Temperature diffrence
                                                        ! due to Aggregated heat extraction/rejection step
    REAL(r64)              :: SumQnHourly               ! same as above for hourly
    REAL(r64)              :: SumQnSubHourly            ! same as above for subhourly( with no aggreation]
    REAL(r64)              :: RQMonth
    REAL(r64)              :: RQHour
    REAL(r64)              :: RQSubHr
    INTEGER                :: I
    REAL(r64)              :: tmpQnSubHourly            ! current Qn subhourly value
    INTEGER                :: HourlyLimit               ! number of hours to be taken into account in superposition
    INTEGER                :: SubHourlyLimit            ! number of subhourlys to be taken into account in subhourly superposition
    REAL(r64)              :: SumTotal                  ! sum of all the Qn (load) blocks
    REAL(r64)              :: C0                        ! **Intermediate constants used
    REAL(r64)              :: C1                        ! **Intermediate constants used
    REAL(r64)              :: C2                        ! **in explicit  calcualtion of the
    REAL(r64)              :: C3                        ! **temperature at the U tube outlet.
    INTEGER, SAVE          :: PrevN =1                  ! The saved value of N at previous time step
    INTEGER                :: IndexN                    ! Used to index the LastHourN array
    LOGICAL,SAVE           :: UpdateCurSimTime = .TRUE. ! Used to reset the CurSimtime to reset after Warmupflag
    LOGICAL, SAVE          :: TriggerDesignDayReset = .FALSE.
    INTEGER                :: GlheInletNode             ! Inlet node number of the Glhe
    INTEGER                :: GlheOutletNode            ! Outlet node number of the Glhe
!    LOGICAL, SAVE      :: Allocated = .FALSE.
    INTEGER          :: AGG
    INTEGER          :: SubAGG
    INTEGER          :: LoopNum
    INTEGER          :: LoopSideNum


          !set local glhe parameters

  NumBholes    = VerticalGlhe(GlheNum)%NumBoreholes
  BholeLength  = VerticalGlhe(GlheNum)%BoreholeLength
  GlheInletNode = VerticalGlhe(GlheNum)%GlheInletNodeNum
  GlheInletTemp = Node(GlheInletnode)%Temp
  Cp_fluid     = GetSpecificHeatGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                       'CalcVerticalGroundHeatExchanger')

  Tground      = VerticalGlhe(GlheNum)%TempGround
  FluidDensity = GetDensityGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                       'CalcVerticalGroundHeatExchanger')
  K_Ground     = VerticalGlhe(GlheNum)%KGround
  K_Ground_Factor = 2.0d0 * PI * K_Ground
  AGG      = VerticalGlhe(GlheNum)%AGG
  SubAGG     = VerticalGlhe(GlheNum)%SubAGG
  GroundDiffusivity = VerticalGlhe(GlheNum)%KGround/VerticalGlhe(GlheNum)%CpRhoGround

  ! calculate annual time constant for ground conduction
  TimeSS = (VerticalGlhe(GlheNum)%BoreholeLength**2/(9.d0*GroundDiffusivity)) /SecInHour/8760.0d0
  TimeSS_Factor = TimeSS*8760.d0

  GlheOutletNode      = VerticalGlhe(GlheNum)%GlheOutletNodeNum
  LoopNum = VerticalGlhe(GlheNum)%LoopNum
  LoopSideNum = VerticalGlhe(GlheNum)%LoopSideNum

  GlheMassFlowRate = MDotActual

  IF(TriggerDesignDayReset.AND. Warmupflag)UpdateCurSimTime=.TRUE.
  IF(DayofSim .EQ. 1 .AND. UpdateCurSimTime)THEN
    CurrentSimTime = 0.0d0
    PrevTimeSteps = 0.0d0
    DO I = 1,NumVerticalGlhes
      VerticalGlhe(I)%QnHr           = 0.0d0
      VerticalGlhe(I)%QnMonthlyAgg   = 0.0d0
      VerticalGlhe(I)%QnSubHr   = 0.0d0
      VerticalGlhe(I)%LastHourN = 1
    END DO
    N = 1
    UpdateCurSimTime = .FALSE.
    TriggerDesignDayReset = .FALSE.
  END IF

  CurrentSimTime = (dayofSim-1)*24 + hourofday-1 + (timestep-1)*timestepZone + SysTimeElapsed  !+ TimeStepsys
  LocHourOfDay = MOD(CurrentSimTime,HrsPerDay)+1
  LocDayOfSim = CurrentSimTime/24 + 1

  IF(DayofSim .GT. 1) THEN
    UpdateCurSimTime = .TRUE.
  END IF

  IF(.NOT. Warmupflag)THEN
    TriggerDesignDayReset = .TRUE.
  ENDIF

  IF (CurrentSimTime .LE. 0.0d0)THEN
    PrevTimeSteps = 0.0d0 ! this resets history when rounding 24:00 hours during warmup avoids hard crash later
    GlheOutletTemp = GlheInletTemp
    GlheMassFlowRate = MDotActual
    CALL CalcAggregateLoad(GlheNum)     !Just allocates and initializes PrevHour array
    RETURN
  END IF

  ! Store currentsimtime in PrevTimeSteps only if a time step occurs

  IF( PrevTimeSteps (1) /= CurrentSimTime) THEN
    PrevTimeSteps = EOSHIFT(PrevTimeSteps,-1, CurrentSimTime)
    N = N + 1
  END IF
  IF(N /= PrevN) THEN
    PrevN = N
    DO I = 1,NumVerticalGlhes
      VerticalGlhe(I)%QnSubHr = EOSHIFT(VerticalGlhe(I)%QnSubHr,-1,LastQnSubHr(I))
    END DO
  END IF

  CALL CalcAggregateLoad(GlheNum)

  ! Update the borehole resistance each time
  CALL BoreholeResistance(GlheNum, ResistanceBhole)

  SumTotal = 0.0d0 !Objexx:Uninit Line added to assure SumTotal initialized when used in: GlheBoreholeTemp = TGround - SumTotal
  IF(N .EQ. 1) THEN
    IF(MDotActual .LE. 0.0d0) THEN
      tmpQnSubHourly = 0.0d0
      FluidAveTemp = Tground
      ToutNew = GlheInletTemp
    ELSE
      XI = LOG( CurrentSimTime / (TimeSS_Factor)  )
      CALL INTERP(GlheNum,XI,GfuncVal)

      C_1 = (BholeLength * NumBholes )/( 2.d0 * MDotActual * Cp_Fluid)
      tmpQnSubHourly = (Tground - GlheInletTemp)/ &
                           (GfuncVal/(K_Ground_Factor ) + ResistanceBhole + C_1)
      FluidAveTemp = Tground - tmpQnSubHourly * ResistanceBhole
      ToutNew = Tground - tmpQnsubHourly * (GfuncVal /  &
                                          (K_Ground_Factor  ) + ResistanceBhole - C_1)
    END IF
  ELSE
      ! no monthly super position
    IF(CurrentSimTime .LT.(HrsPerMonth + AGG + SubAGG)) THEN

      ! Calculate the Sub Hourly Superposition
      SumQnSubHourly = 0.0d0
      IF(INT(CurrentSimTime).LT.SubAGG)THEN
        IndexN = INT(CurrentSimTime)+1
      ELSE
        IndexN = SubAGG+1
      END IF
      SubHourlyLimit = N - VerticalGlhe(GlheNum)%LastHourN(IndexN) !Check this when running simulation

      SUBHRLY_LOOP:DO I = 1, SubHourlyLimit
        IF(I.EQ.SubHourlyLimit) THEN
          IF( INT(CurrentSimTime)>=SubAGG)THEN
            XI = LOG( (CurrentSimTime - PrevTimeSteps(I+1)) / (TimeSS_Factor) )
            CALL INTERP(GlheNum,XI,GfuncVal)
            RQSubHr =  GfuncVal/(K_Ground_Factor )
            SumQnSubHourly = SumQnSubHourly + (VerticalGlhe(GlheNum)%QnSubHr(I)-&
                                      VerticalGlhe(GlheNum)%QnHr(IndexN)) * RQSubHr
          ELSE
            XI = LOG( (CurrentSimTime - PrevTimeSteps(I+1)) / (TimeSS_Factor) )
            CALL INTERP(GlheNum,XI,GfuncVal)
            RQSubHr =  GfuncVal/(K_Ground_Factor)
            SumQnSubHourly = SumQnSubHourly + VerticalGlhe(GlheNum)%QnSubHr(I)* RQSubHr
          END IF
          EXIT SUBHRLY_LOOP
        END IF
          !PrevTimeSteps(I+1) This is "I+1" because PrevTimeSteps(1) = CurrentTimestep
        XI = LOG( (CurrentSimTime - PrevTimeSteps(I+1)) / (TimeSS_Factor) )
        CALL INTERP(GlheNum,XI,GfuncVal)
        RQSubHr = GfuncVal / (K_Ground_Factor)
        SumQnSubHourly = SumQnSubHourly + &
                           (VerticalGlhe(GlheNum)%QnSubHr(I)-&
                           VerticalGlhe(GlheNum)%QnSubHr(I+1)) * RQSubHr
      END DO SUBHRLY_LOOP

          ! Calculate the Hourly Superposition

      HourlyLimit =  INT(CurrentSimTime)
      SumQnHourly = 0.0d0
      HOURLY_LOOP:DO I = SubAGG+1, HourlyLimit
        IF(I.EQ.HourlyLimit) THEN
          XI = LOG( CurrentSimTime / (TimeSS_Factor) )
          CALL INTERP(GlheNum,XI,GfuncVal)
          RQHour =  GfuncVal/(K_Ground_Factor)
          SumQnHourly = SumQnHourly + VerticalGlhe(GlheNum)%QnHr(I)*RQHour
          EXIT HOURLY_LOOP
        END IF
        XI = LOG( (CurrentSimTime - INT(CurrentSimTime) + I) / (TimeSS_Factor))
        CALL INTERP(GlheNum,XI,GfuncVal)
        RQHour  = GfuncVal/(K_Ground_Factor )
        SumQnHourly = SumQnHourly + &
                            (VerticalGlhe(GlheNum)%QnHr(I)-&
                            VerticalGlhe(GlheNum)%QnHr(I+1)) * RQHour
      END DO HOURLY_LOOP


      ! Find the total Sum of the Temperature difference due to all load blocks
      SumTotal = SumQnSubHourly + SumQnHourly

       !Calulate the subhourly temperature due the Last Time steps Load
      XI = LOG( (CurrentSimTime - PrevTimeSteps(2)) / (TimeSS_Factor) )
      CALL INTERP(GlheNum,XI,GfuncVal)
      RQSubHr = GfuncVal / (K_Ground_Factor )

      IF(MDotActual .LE. 0.0d0)THEN
        tmpQnsubHourly = 0.0d0
        FluidAveTemp = Tground - SumTotal ! Q(N)*RB = 0
        ToutNew = GlheInletTemp
      ELSE
         !Dr.Spitler's Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
        C0 = RQSubHr
        C1 = Tground - (SumTotal - VerticalGlhe(GlheNum)%QnSubHr(1)*RQSubHr)
        C2 = BholeLength * NumBholes / ( 2.d0 * MDotActual *Cp_Fluid)
        C3 = MDotActual * Cp_Fluid / ( BholeLength * NumBholes )
        tmpQnsubHourly = (C1 - GlheInletTemp)/( ResistanceBhole + C0 - C2 + ( 1 / C3 ))
        FluidAveTemp = C1 - (C0 + ResistanceBhole) * tmpQnsubHourly
        ToutNew = C1 + (C2 - C0 - ResistanceBhole) * tmpQnsubHourly
      END IF

    ELSE ! Monthly Aggregation and super position

      NumOfMonths = (CurrentSimTime+1)/HrsPerMonth

      IF(CurrentSimTime .LT. ((NumOfMonths)*HrsPerMonth)+AGG+SubAGG)THEN
        CurrentMonth = NumOfMonths - 1
      ELSE
        CurrentMonth = NumOfMonths
      END IF

         !monthly superposition
      SumQnMonthly = 0.0d0
      SUMMONTHLY :DO I = 1,CurrentMonth
        IF(I.EQ.1) THEN
          XI = LOG(CurrentSimTime/ (TimeSS_Factor))
          CALL INTERP(GlheNum,XI,GfuncVal)
          RQMonth =  GfuncVal/(K_Ground_Factor )
          SumQnMonthly = SumQnMonthly + VerticalGlhe(GlheNum)%QnMonthlyAgg(I) * RQMonth
          CYCLE SUMMONTHLY
        END IF
        XI = LOG((CurrentSimTime - (I-1)*HrsPerMonth )/ (TimeSS_Factor))
        CALL INTERP(GlheNum,XI,GfuncVal)
        RQMonth =  GfuncVal/(K_Ground_Factor )
        SumQnMonthly = SumQnMonthly + &
                         (VerticalGlhe(GlheNum)%QnMonthlyAgg(I)- &
                         VerticalGlhe(GlheNum)%QnMonthlyAgg(I-1))* RQMonth
      END DO SUMMONTHLY

       ! Hourly Supr position
      HourlyLimit = INT( CurrentSimTime - CurrentMonth* HrsPerMonth)
      SumQnHourly = 0.0d0
      HOURLYLOOP: DO I = 1 + SubAGG, HourlyLimit
        IF(I.EQ.HourlyLimit) THEN
          XI = LOG( (CurrentSimTime - INT(CurrentSimTime) + I) / (TimeSS_Factor) )
          CALL INTERP(GlheNum,XI,GfuncVal)
          RQHour =  GfuncVal/(K_Ground_Factor )
          SumQnHourly = SumQnHourly + (VerticalGlhe(GlheNum)%QnHr(I) -&
                                 VerticalGlhe(GlheNum)%QnMonthlyAgg(CurrentMonth)) * RQHour
          EXIT HOURLYLOOP
        END IF
        XI = LOG( (CurrentSimTime - INT(CurrentSimTime) + I) / (TimeSS_Factor) )
        CALL INTERP(GlheNum,XI,GfuncVal)
        RQHour  = GfuncVal / (K_Ground_Factor )
        SumQnHourly = SumQnHourly + &
                          (VerticalGlhe(GlheNum)%QnHr(I)-&
                          VerticalGlhe(GlheNum)%QnHr(I+1)) * RQHour
      END DO HOURLYLOOP

          ! Subhourly Superposition
      SubHourlyLimit = N - VerticalGlhe(GlheNum)%LastHourN(SubAGG+1)
      SumQnSubHourly = 0.0d0
      SUBHRLOOP:  DO I = 1 ,SubHourlyLimit
        IF(I.EQ.SubHourlyLimit) THEN
          XI = LOG( (CurrentSimTime - PrevTimeSteps(I+1)) / (TimeSS_Factor) )
          CALL INTERP(GlheNum,XI,GfuncVal)
          RQSubHr =  GfuncVal/(K_Ground_Factor )
          SumQnSubHourly = SumQnSubHourly + (VerticalGlhe(GlheNum)%QnSubHr(I)-&
                                      VerticalGlhe(GlheNum)%QnHr(SubAgg+1)) * RQSubHr
          EXIT SUBHRLOOP
        END IF
        XI = LOG( (CurrentSimTime - PrevTimeSteps(I+1)) / (TimeSS_Factor) )
        CALL INTERP(GlheNum,XI,GfuncVal)
            RQSubHr = GfuncVal / (K_Ground_Factor )
            SumQnSubHourly = SumQnSubHourly + &
                                 (VerticalGlhe(GlheNum)%QnSubHr(I)-&
                                  VerticalGlhe(GlheNum)%QnSubHr(I+1)) * RQSubHr
      END DO SUBHRLOOP

      SumTotal = SumQnMonthly + SumQnHourly + SumQnSubHourly

      !Calulate the subhourly temperature due the Last Time steps Load

      XI = LOG( (CurrentSimTime - PrevTimeSteps(2)) / (TimeSS_Factor) )
      CALL INTERP(GlheNum,XI,GfuncVal)
      RQSubHr = GfuncVal / (K_Ground_Factor )

      IF(MDotActual .LE. 0.0d0)THEN
        tmpQnsubHourly = 0.0d0
        FluidAveTemp = Tground - SumTotal ! Q(N)*RB = 0
        ToutNew = GlheInletTemp
      ELSE
      ! Explicit set of equations to calculate the New Outlet Temperature of the U-Tube
        C0 = RQSubHr
        C1 = Tground - (SumTotal - VerticalGlhe(GlheNum)%QnSubHr(1)*RQSubHr)
        C2 = BholeLength * NumBholes / ( 2 * MDotActual *Cp_Fluid)
        C3 = MDotActual * Cp_Fluid / ( BholeLength * NumBholes )
        tmpQnsubHourly = (C1 - GlheInletTemp)/( ResistanceBhole + C0 - C2 + ( 1 / C3 ))
        FluidAveTemp = C1 - (C0 + ResistanceBhole) * tmpQnsubHourly
        ToutNew = C1 + (C2 - C0 - ResistanceBhole) * tmpQnsubHourly
      END IF
    END IF !  end of AGG OR NO AGG
  END IF ! end of N  = 1 branch
  GlheBoreholeTemp = TGround - SumTotal
  !Load the QnSubHourly Array with a new value at end of every timestep

  !Load the report vars
  LastQnSubHr(GlheNum)  = tmpQnsubHourly
  GlheOutletTemp = ToutNew
  QGlhe = tmpQnsubHourly
  GlheAveFluidTemp = FluidAveTemp
  GlheRB = ResistanceBhole
  GlheMassFlowRate = MDotActual

END SUBROUTINE CalcVerticalGroundHeatExchanger

!******************************************************************************

SUBROUTINE CalcAggregateLoad(GlheNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Arun Murugappan
          !       DATE WRITTEN:    August, 2000
          !       MODIFIED:        na
          !       RE-ENGINEERED:   na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the heat transfer history.

          ! METHODOLOGY EMPLOYED:
          ! The heat pulse histories need to be recorded over an extended period (months).
          ! To aid computational efficiency past pulses are continuously agregated into
          ! equivalent heat pulses of longer duration, as each pulse becomes less recent.
          ! Past sub-hourly loads are re-aggregated into equivalent hourly and monthly loads.

          ! REFERENCES:
          ! Eskilson, P. 'Thermal Analysis of Heat Extraction Boreholes' Ph.D. Thesis:
          !   Dept. of Mathematical Physics, University of Lund, Sweden, June 1987.
          ! Yavuzturk, C., J.D. Spitler. 1999. 'A Short Time Step Response Factor Model
          !   for Vertical Ground Loop Heat Exchangers. ASHRAE Transactions. 105(2): 475-485.

          ! USE STATEMENTS:
          ! na

    INTEGER, INTENT(IN):: GlheNum

!LOCAL VARIABLES
    REAL(r64)   :: SumQnMonth                           ! intermediate variable to store the Montly heat rejection/
    REAL(r64)   :: SumQnHr
    INTEGER     :: MonthNum
    INTEGER     :: J                                    ! Loop counter
    INTEGER,SAVE, ALLOCATABLE,DIMENSION(:):: PrevHour   ! Saved Var to store the previous hour
!    LOGICAL,SAVE :: Allocated = .FALSE.
    LOGICAL, SAVE      :: MyEnvrnFlag = .TRUE.


    IF(MyEnvrnFlag .AND. BeginEnvrnFlag)THEN
!    IF(.not.Allocated)THEN
!      ALLOCATE(PrevHour(NumVerticalGlhes))
      IF(.NOT. ALLOCATED(PrevHour)) ALLOCATE(PrevHour(NumVerticalGlhes))
!      Allocated = .TRUE.
      MyEnvrnFlag = .FALSE.
      PrevHour = 1
    END IF
    IF (CurrentSimTime .LE. 0.0d0) RETURN

    IF(.NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

    !FOR EVERY HOUR UPDATE THE HOURLY QN QnHr(J)
    !THIS IS DONE BY AGGREGATING THE SUBHOURLY QN FROM THE PREVIOUS HOUR TO UNTIL THE CURRNET HOUR
    !AND STORING IT IN  VerticalGlhe(GlheNum)%QnHr(J)

    !SUBHOURLY Qn IS NOT AGGREGATED . IT IS THE BASIC LOAD
    IF(PrevHour(GlheNum) /= LocHourOfDay )THEN
        SumQnHr=0.0d0
        DO J = 1, (N - VerticalGlhe(GlheNum)%LastHourN(1)) ! Check during debugging if we need a +1
            SumQnHr = SumQnHr + VerticalGlhe(GlheNum)%QnSubHr(J)*ABS((PrevTimeSteps(J)-PrevTimeSteps(J+1)))
        END DO
        SumQnHr = SumQnHr/ABS(PrevTimeSteps(1)-PrevTimeSteps(J))
        VerticalGlhe(GlheNum)%QnHr = EOSHIFT(VerticalGlhe(GlheNum)%QnHr,-1,SumQnHr)
        VerticalGlhe(GlheNum)%LastHourN = EOSHIFT(VerticalGlhe(GlheNum)%LastHourN,-1,N)
    END IF

    !CHECK IF A MONTH PASSES...
    IF(MOD(((LocDayOfSim-1)*HrsPerDay+(LocHourOfDay)),HrsPerMonth).EQ.0 .AND. PrevHour(GlheNum) /= LocHourOfDay)THEN
        MonthNum = (LocDayOfSim*HrsPerDay+LocHourOfDay)/HrsPerMonth
        SumQnMonth=0.0d0
        DO J = 1, Int(HrsPerMonth)
            SumQnMonth = SumQnMonth+VerticalGlhe(GlheNum)%QnHr(J)
        END DO
        SumQnMonth = SumQnMonth/HrsPerMonth
        VerticalGlhe(GlheNum)%QnMonthlyAgg(MonthNum) = SumQnMonth
    END IF
    IF(PrevHour(GlheNum) /= LocHourOfDay)THEN
        PrevHour(GlheNum) = LocHourOfDay
    END IF

END SUBROUTINE CalcAggregateLoad

!******************************************************************************

SUBROUTINE GetGroundheatExchangerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    August, 2000
          !       MODIFIED         Arun Murugappan
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
  USE DataIPShortCuts
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE General, ONLY: TrimSigDigits,RoundSigDigits
  USE DataEnvironment, ONLY: MaxNumberSimYears
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow

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
    INTEGER                     :: GlheNum
    INTEGER                     :: NumAlphas                ! Number of elements in the alpha array
    INTEGER                     :: NumNums                  ! Number of elements in the numeric array
    INTEGER                     :: IOStat                   ! IO Status when calling get input subroutine
    LOGICAL, SAVE               :: ErrorsFound=.FALSE.
    LOGICAL                     :: IsNotOK                  ! Flag to verify name
    LOGICAL                     :: IsBlank                  ! Flag for blank name
    INTEGER                     :: IndexNum
    INTEGER                     :: PairNum
    LOGICAL                     :: Allocated

            !GET NUMBER OF ALL EQUIPMENT TYPES
    cCurrentModuleObject = 'GroundHeatExchanger:Vertical'
    NumVerticalGlhes = GetNumObjectsFound(cCurrentModuleObject)

    Allocated = .FALSE.

    IF (NumVerticalGlhes <= 0 ) THEN
      CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment found in input file')
      ErrorsFound = .true.
    ENDIF

    ALLOCATE (VerticalGlhe(NumVerticalGlhes))

    ALLOCATE (VerticalGlheReport(NumVerticalGlhes))
    ALLOCATE(CheckEquipName(NumVerticalGlhes))
    CheckEquipName=.true.

    DO GlheNum = 1 , NumVerticalGlhes
        CALL GetObjectItem(cCurrentModuleObject,GlheNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.

            !get object name
    CALL VerifyName(cAlphaArgs(1),VerticalGlhe%Name,GlheNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    VerticalGlhe(GlheNum)%Name               = cAlphaArgs(1)

        !get inlet node num
    VerticalGlhe(GlheNum)%GlheInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

        !get outlet node num
    VerticalGlhe(GlheNum)%GlheOutletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    verticalglhe(GlheNum)%Available =   .TRUE.
    verticalglhe(GlheNum)%On            =   .TRUE.

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3), &
                     'Condenser Water Nodes')

        !load borehole data
    VerticalGlhe(GlheNum)%DesignFlow        = rNumericArgs(1)
    CALL RegisterPlantCompDesignFlow (VerticalGlhe(GlheNum)%GlheInletNodeNum, VerticalGlhe(GlheNum)%DesignFlow)

    VerticalGlhe(GlheNum)%NumBoreholes      = rNumericArgs(2)
    VerticalGlhe(GlheNum)%BoreholeLength    = rNumericArgs(3)
    VerticalGlhe(GlheNum)%BoreholeRadius    = rNumericArgs(4)
    VerticalGlhe(GlheNum)%KGround           = rNumericArgs(5)
    VerticalGlhe(GlheNum)%CpRhoGround       = rNumericArgs(6)
    VerticalGlhe(GlheNum)%TempGround        = rNumericArgs(7)
    VerticalGlhe(GlheNum)%MaxGlheFlowRate   = rNumericArgs(8)
    VerticalGlhe(GlheNum)%KGrout            = rNumericArgs(9)
    VerticalGlhe(GlheNum)%KPipe             = rNumericArgs(10)
    VerticalGlhe(GlheNum)%PipeOutDia        = rNumericArgs(11)
    VerticalGlhe(GlheNum)%UtubeDist         = rNumericArgs(12)
    VerticalGlhe(GlheNum)%PipeThick         = rNumericArgs(13)
    VerticalGlhe(GlheNum)%MaxSimYears       = rNumericArgs(14)
    VerticalGlhe(GlheNum)%gReferenceRatio   = rNumericArgs(15)

!   Not many checks
    IF (VerticalGlhe(GlheNum)%PipeThick >= VerticalGlhe(GlheNum)%PipeOutDia/2.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//trim(VerticalGlhe(GlheNum)%Name)//  &
         '", invalid value in field.')
      CALL ShowContinueError('...'//trim(cNumericFieldNames(13))//'=['//  &
         trim(RoundSigDigits(VerticalGlhe(GlheNum)%PipeThick,3))//'].')
      CALL ShowContinueError('...'//trim(cNumericFieldNames(11))//'=['//  &
         trim(RoundSigDigits(VerticalGlhe(GlheNum)%PipeOutDia,3))//'].')
      CALL ShowContinueError('...Radius will be <=0.')
      ErrorsFound=.true.
    ENDIF

    IF (VerticalGlhe(GlheNum)%MaxSimYears < MaxNumberSimYears) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//trim(VerticalGlhe(GlheNum)%Name)//  &
         '", invalid value in field.')
      CALL ShowContinueError('...'//trim(cNumericFieldNames(14))//' less than RunPeriod Request')
      CALL ShowContinueError('Requested input='//TRIM(TrimSigDigits(VerticalGlhe(GlheNum)%MaxSimYears))// &
                             ' will be set to '//TRIM(TrimSigDigits(MaxNumberSimYears)))
      VerticalGlhe(GlheNum)%MaxSimYears=MaxNumberSimYears
    ENDIF


        ! Get Gfunction data
    VerticalGlhe(GlheNum)%NPairs    = rNumericArgs(16)
    VerticalGlhe(GlheNum)%SubAGG    = 15
    VerticalGlhe(GlheNum)%AGG       = 192


    ! Allocation of all the dynamic arrays
    ALLOCATE (VerticalGlhe(GlheNum)%LNTTS(VerticalGlhe(GlheNum)%NPairs))
    VerticalGlhe(GlheNum)%LNTTS=0.0d0
    ALLOCATE (VerticalGlhe(GlheNum)%GFNC(VerticalGlhe(GlheNum)%NPairs))
    VerticalGlhe(GlheNum)%GFNC=0.0d0
    ALLOCATE (VerticalGlhe(GlheNum)%QnMonthlyAgg(VerticalGlhe(GlheNum)%MaxSimYears*12))
    VerticalGlhe(GlheNum)%QnMonthlyAgg=0.0d0
    ALLOCATE (VerticalGlhe(GlheNum)%QnHr(730+ VerticalGlhe(GlheNum)%AGG+ &
                                            VerticalGlhe(GlheNum)%SubAGG))
    VerticalGlhe(GlheNum)%QnHr=0.0d0
    ALLOCATE (VerticalGlhe(GlheNum)%QnSubHr((VerticalGlhe(GlheNum)%SubAGG+1)*MaxTSinHr+1))
    VerticalGlhe(GlheNum)%QnSubHr=0.0d0
    ALLOCATE (VerticalGlhe(GlheNum)%LastHourN(VerticalGlhe(GlheNum)%SubAGG+1))
    VerticalGlhe(GlheNum)%LastHourN=0

    IF(.NOT.Allocated)THEN
      ALLOCATE (PrevTimeSteps((VerticalGlhe(GlheNum)%SubAGG+1)*MaxTSinHr+1))
      PrevTimeSteps=0.0d0
      Allocated = .TRUE.
    END IF

    IndexNum = 17
    Do PairNum = 1, VerticalGlhe(GlheNum)%NPairs
        VerticalGlhe(GlheNum)%LNTTS(PairNum) = rNumericArgs(IndexNum)
        VerticalGlhe(GlheNum)%GFNC(PairNum) =  rNumericArgs(IndexNum+1)
        IndexNum=IndexNum+2
    End Do
         !Check for Errors
    IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
    ENDIF
  END DO

        !Set up report variables
  DO GlheNum = 1, NumVerticalGlhes
    CALL SetupOutputVariable('Ground Heat Exchanger Average Borehole Temperature [C]', &
          VerticalGlheReport(GlheNum)%GlheBoreholeTemp,'System','Average',VerticalGlhe(GlheNum)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Heat Transfer Rate [W]', &
          VerticalGlheReport(GlheNum)%QGlhe,'System','Average',VerticalGlhe(GlheNum)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Inlet Temperature [C]', &
          VerticalGlheReport(GlheNum)%GlheInletTemp,'System','Average',VerticalGlhe(GlheNum)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Outlet Temperature [C]', &
          VerticalGlheReport(GlheNum)%GlheOutletTemp,'System','Average',VerticalGlhe(GlheNum)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Mass Flow Rate [kg/s]', &
          VerticalGlheReport(GlheNum)%GlheMassFlowRate,'System','Average',VerticalGlhe(GlheNum)%Name)
    CALL SetupOutputVariable('Ground Heat Exchanger Average Fluid Temperature [C]', &
          VerticalGlheReport(GlheNum)%GlheAveFluidTemp,'System','Average',VerticalGlhe(GlheNum)%Name)
  END DO

RETURN
END SUBROUTINE GetGroundheatExchangerInput

!******************************************************************************

    SUBROUTINE BoreholeResistance(GlheNum,ResistanceBhole)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Cenk Yavuzturk
          !       DATE WRITTEN   1998
          !       MODIFIED       August, 2000
          !       RE-ENGINEERED Dan Fisher

          ! PURPOSE OF THIS SUBROUTINE:
          !    Calculates the resistance of a vertical borehole
          !    with a U-tube inserted into it.
          !

          ! METHODOLOGY EMPLOYED:
          !

          !  REFERENCE:          Thermal Analysis of Heat Extraction
          !                      Boreholes.  Per Eskilson, Dept. of
          !                      Mathematical Physics, University of
          !                      Lund, Sweden, June 1987.
          !
          ! USE STATEMENTS: na
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol, GetViscosityGlycol, &
                             GetConductivityGlycol
  USE DataPlant,    ONLY: PlantLoop
    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)    :: GlheNum
    REAL(r64), INTENT(OUT)      :: ResistanceBhole
            ! SUBROUTINE PARAMETER DEFINITIONS:
            ! na

            ! INTERFACE BLOCK SPECIFICATIONS
            ! na

            ! DERIVED TYPE DEFINITIONS
            ! na

            ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER                :: NumBholes           !number of boreholes
    REAL(r64)              :: BholeLength
    REAL(r64)              :: BholeRadius
    REAL(r64)              :: K_Ground
    REAL(r64)              :: Cp_Ground
    REAL(r64)              :: Cp_Fluid
    REAL(r64)              :: Tground
    REAL(r64)              :: K_Grout
    REAL(r64)              :: K_Fluid
    REAL(r64)              :: K_Pipe
    REAL(r64)              :: FluidDensity
    REAL(r64)              :: FluidViscosity
    REAL(r64)              :: PipeOuterDia
    REAL(r64)              :: PipeInnerDia
    REAL(r64)              :: DistUtube
    REAL(r64)              :: ThickPipe
    REAL(r64)              :: BholeMdot
    REAL(r64)              :: PipeOuterRad
    REAL(r64)              :: PipeInnerRad
    REAL(r64)              :: NusseltNum
    REAL(r64)              :: ReynoldsNum
    REAL(r64)              :: PrandlNum
    REAL(r64)              :: hci
    REAL(r64)              :: Rcond
    REAL(r64)              :: Rconv
    REAL(r64)              :: Rgrout
    REAL(r64)              :: B0, B1              !grout resistance curve fit coefficients
    REAL(r64)              :: MaxDistance
    REAL(r64)              :: DistanceRatio

        !assign local variables
    NumBholes   = VerticalGlhe(GlheNum)%NumBoreholes
    BholeLength = VerticalGlhe(GlheNum)%BoreholeLength
    BholeRadius = VerticalGlhe(GlheNum)%BoreholeRadius
    K_Ground    = VerticalGlhe(GlheNum)%KGround
    Cp_Ground   = VerticalGlhe(GlheNum)%CpRhoGround

    Cp_Fluid    = GetSpecificHeatGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                       'CalcVerticalGroundHeatExchanger')

    Tground     = VerticalGlhe(GlheNum)%TempGround
    K_Grout     = VerticalGlhe(GlheNum)%KGrout
    K_Pipe      = VerticalGlhe(GlheNum)%KPipe
    K_Fluid     = GetConductivityGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                       'CalcVerticalGroundHeatExchanger')
    FluidDensity = GetDensityGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                       'CalcVerticalGroundHeatExchanger')

    FluidViscosity = GetViscosityGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                       'CalcVerticalGroundHeatExchanger')

    PipeOuterDia    = VerticalGlhe(GlheNum)%PipeOutDia
    DistUtube   = VerticalGlhe(GlheNum)%UtubeDist
    ThickPipe   = VerticalGlhe(GlheNum)%PipeThick


        !calculate mass flow rate
    BholeMdot = GlheMassFlowRate/NumBholes !VerticalGlhe(GlheNum)%DesignFlow*FluidDensity /NumBholes

    PipeOuterRad = PipeOuterDia / 2.0d0
    PipeInnerRad = PipeOuterRad-ThickPipe
    PipeInnerDia = 2.0d0 * PipeInnerRad
                    !Re=Rho*V*D/Mu
    ReynoldsNum = FluidDensity*PipeInnerDia*(BholeMdot/FluidDensity/(PI*PipeInnerRad**2))/FluidViscosity
    PrandlNum=(Cp_Fluid*FluidViscosity)/(K_Fluid)
!   Convection Resistance
    NusseltNum = 0.023d0 * (ReynoldsNum**0.8d0) * (PrandlNum**0.35d0)
    hci = NusseltNum * K_Fluid / PipeInnerDia
    IF(BholeMdot == 0.0d0)THEN
        RCONV=0.0d0
    ELSE
        RCONV = 1.0d0 / (2.0d0*PI*PipeInnerDia*hci)
    ENDIF

!   Conduction Resistance
    RCOND = LOG(PipeOuterRad/PipeInnerRad) / (2.0d0*PI*K_Pipe)/2.d0 ! pipe in parallel so /2

!   Resistance Due to the grout.
    MaxDistance=2.d0*BholeRadius-(2.d0*PipeOuterDia)
    DistanceRatio=DistUtube/MaxDistance


    IF(DistanceRatio >= 0.0d0 .AND. DistanceRatio <= 0.25d0) THEN
        B0=14.450872d0
        B1=-0.8176d0
    ELSE IF(DistanceRatio > 0.25d0 .AND. DistanceRatio < 0.5d0) THEN
        B0=20.100377d0
        B1=-0.94467d0
    ELSE IF(DistanceRatio >= 0.5d0.and.DistanceRatio <= 0.75d0) THEN
        B0=17.44268d0
        B1=-0.605154d0
    ELSE
        B0=21.90587d0
        B1=-0.3796d0
    END IF

    RGROUT=1.d0/(K_Grout*(B0*(BholeRadius/PipeOuterRad)**B1))
    ResistanceBhole = RCOND+RCONV+RGROUT
    RETURN
END SUBROUTINE BoreholeResistance

!******************************************************************************

SUBROUTINE INTERP(GlheNum,LnTTsVal,GfuncVal)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chris L. Marshall, Jeffrey D. Spitler
          !       DATE WRITTEN   1993
          !       MODIFIED       August, 2000
          !       RE-ENGINEERED Dan Fisher

          ! PURPOSE OF THIS SUBROUTINE:
          !    To interpolate or extrapolate data in GFILE
          !    to find the correct g-function value for a
          !    known value of the natural log of (T/Ts)

          ! METHODOLOGY EMPLOYED:
          !

          !  REFERENCE:          Thermal Analysis of Heat Extraction
          !                      Boreholes.  Per Eskilson, Dept. of
          !                      Mathematical Physics, University of
          !                      Lund, Sweden, June 1987.
          !
          ! USE STATEMENTS: na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)     :: GlheNum      ! Ground loop heat exchanger ID number
    REAL(r64),    INTENT(IN)     :: LnTTsVal     ! The value of LN(t/TimeSS) that a g-function
                                            !          needs to be found for.
    REAL(r64), INTENT(OUT)       :: GfuncVal     ! The value of the g-function at LnTTsVal; found by
                                            !          either extrapolation or interpolation
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER         :: NumPairs
    REAL(r64)       :: RATIO
    REAL(r64)       :: ReferenceRatio

    !Binary Search Algorithms Variables
    ! REFERENCE      :  DATA STRUCTURES AND ALGORITHM ANALYSIS IN C BY MARK ALLEN WEISS
    INTEGER         :: Mid
    INTEGER         :: Low
    INTEGER         :: High
    LOGICAL         :: Found

    NumPairs =VerticalGlhe(GlheNum)%NPairs
    RATIO=VerticalGlhe(GlheNum)%BoreholeRadius/VerticalGlhe(GlheNum)%BoreholeLength
    ReferenceRatio = VerticalGlhe(GlheNum)%gReferenceRatio

    ! The following IF loop determines the g-function for the case
    ! when LnTTsVal is less than the first element of the LnTTs array.
    ! In this case, the g-function must be found by extrapolation.

    IF(LnTTsVal <= VerticalGlhe(GlheNum)%LNTTS(1)) THEN
        GfuncVal=((          LnTTsVal             - VerticalGlhe(GlheNum)%LNTTS(1))  &
                 /(VerticalGlhe(GlheNum)%LNTTS(2) - VerticalGlhe(GlheNum)%LNTTS(1))) &
                 *(VerticalGlhe(GlheNum)%GFNC(2)  - VerticalGlhe(GlheNum)%GFNC(1))   &
                 + VerticalGlhe(GlheNum)%GFNC(1)

    ! The following IF statement determines the condition of the ratio
    ! between the borehole radius and the active borehole length.
    ! If RATIO does not equal 0.0005 then a correction factor for
    ! the g-function must be used.

        IF(RATIO /= ReferenceRatio) THEN
          GfuncVal=GfuncVal-LOG(VerticalGlhe(GlheNum)%BoreholeRadius/(VerticalGlhe(GlheNum)%BoreholeLength*ReferenceRatio))
        ENDIF

        RETURN
    ENDIF

    ! The following IF loop determines the g-function for the case
    ! when LnTTsVal is greater than the last element of the LnTTs array.
    ! In this case, the g-function must be found by extrapolation.

    IF(LnTTsVal > VerticalGlhe(GlheNum)%LNTTS(NumPairs)) THEN
        GfuncVal=((           LnTTsVal                     - VerticalGlhe(GlheNum)%LNTTS(NumPairs))  &
                 /(VerticalGlhe(GlheNum)%LNTTS(NumPairs-1) - VerticalGlhe(GlheNum)%LNTTS(NumPairs))) &
                 *(VerticalGlhe(GlheNum)%GFNC(NumPairs-1)  - VerticalGlhe(GlheNum)%GFNC(NumPairs))   &
                 + VerticalGlhe(GlheNum)%GFNC(NumPairs)

            ! Apply correction factor if necessary
          IF(RATIO /= ReferenceRatio) THEN
            GfuncVal=GfuncVal-LOG(VerticalGlhe(GlheNum)%BoreholeRadius/(VerticalGlhe(GlheNum)%BoreholeLength*ReferenceRatio))
          ENDIF

          RETURN
      ENDIF

    ! The following DO loop is for the case when LnTTsVal falls within
    ! the first and last elements of the LnTTs array, or is identically
    ! equal to one of the LnTTs elements.  In this case the g-function
    ! must be found by interpolation.
! USING BINARY SEARCH TO FIND THE ELEMENET
    Found = .FALSE.
    Low = 1
    High = NumPairs
LOOP:   DO WHILE(Low<=High)
        Mid = (Low+high)/2
        IF(VerticalGlhe(GlheNum)%LNTTS(Mid)<LnTTsVal)THEN
            Low = Mid+1
        ELSE
        IF( VerticalGlhe(GlheNum)%LNTTS(Mid)>LnTTsVal)THEN
            High = Mid-1
        ELSE
            Found=.true.
            EXIT LOOP
        END IF
        END IF
    END DO LOOP
    !LnTTsVal is identical to one of the LnTTS array elements return
    !the GfuncVal after applying the correction
    IF(Found)THEN
      GfuncVal  = VerticalGlhe(GlheNum)%GFNC(Mid)
      ! Apply correction factor if necessary
      IF(RATIO.NE.ReferenceRatio) THEN
           GfuncVal=GfuncVal-LOG(VerticalGlhe(GlheNum)%BoreholeRadius/(VerticalGlhe(GlheNum)%BoreholeLength*ReferenceRatio))
      ENDIF
      RETURN
    END IF

    !LnTTsVal is in between any of the two LnTTS array elements find the
    ! gfunction value by interplation and apply the correction and return
    IF(.NOT.Found)THEN
        IF(VerticalGlhe(GlheNum)%LNTTS(Mid)<LnTTsVal)Mid=Mid+1

           GfuncVal=((          LnTTsVal                 - VerticalGlhe(GlheNum)%LNTTS(Mid))  &
                         /(VerticalGlhe(GlheNum)%LNTTS(Mid-1) - VerticalGlhe(GlheNum)%LNTTS(Mid))) &
                         *(VerticalGlhe(GlheNum)%GFNC(Mid-1)  - VerticalGlhe(GlheNum)%GFNC(Mid))   &
                         + VerticalGlhe(GlheNum)%GFNC(Mid)

           ! Apply correction factor if necessary
           IF(RATIO /= ReferenceRatio) THEN
              GfuncVal=GfuncVal-LOG(VerticalGlhe(GlheNum)%BoreholeRadius/(VerticalGlhe(GlheNum)%BoreholeLength*ReferenceRatio))
           ENDIF
       RETURN
    END IF
RETURN
END SUBROUTINE INTERP

!******************************************************************************

SUBROUTINE InitBoreholeHXSimVars(GlheNum,Runflag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    August, 2000
          !       MODIFIED         Arun Murugappan
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: InitComponentNodes, SetComponentFlowRate, RegulateCondenserCompFlowReqOp
  USE DataPlant,      ONLY: PlantLoop, TypeOf_GrndHtExchgVertical, ScanPlantLoopsForObject
  USE FluidProperties, ONLY: GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN) :: RunFlag
  INTEGER, INTENT (IN) :: GlheNum


          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: MyEnvironFlag = .TRUE.
  REAL(r64)      :: FluidDensity
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL          :: errFlag

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumVerticalGlhes))
    ALLOCATE(MyFlag(NumVerticalGlhes))
    MyOneTimeFlag = .false.
    MyEnvrnFlag = .TRUE.
    MyFlag = .TRUE.
  END IF

  ! Init more variables
    IF (MyFlag(GlheNum)) THEN
      ! Locate the hx on the plant loops for later usage
      errFlag=.false.
      CALL ScanPlantLoopsForObject(VerticalGlhe(GlheNum)%Name, &
                                   TypeOf_GrndHtExchgVertical, &
                                   VerticalGlhe(GlheNum)%LoopNum, &
                                   VerticalGlhe(GlheNum)%LoopSideNum, &
                                   VerticalGlhe(GlheNum)%BranchNum, &
                                   VerticalGlhe(GlheNum)%CompNum,  &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitBoreholeHXSimVars: Program terminated due to previous condition(s).')
      ENDIF
      MyFlag(GlheNum)=.FALSE.
    ENDIF

    IF(MyEnvrnFlag(GlheNum) .AND. BeginEnvrnFlag)THEN
      MyEnvrnFlag(GlheNum) = .FALSE.

      IF(.NOT. ALLOCATED(LastQnSubHr)) ALLOCATE(LastQnSubHr(NumVerticalGlhes))
      FluidDensity = GetDensityGlycol(PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidName, &
                                      20.d0 , &
                                      PlantLoop(VerticalGlhe(GlheNum)%LoopNum)%FluidIndex, &
                                      'InitBoreholeHXSimVars')
      VerticalGlhe(GlheNum)%DesignMassFlow = VerticalGlhe(GlheNum)%DesignFlow * FluidDensity
      Call InitComponentNodes(0.d0, VerticalGlhe(GlheNum)%DesignMassFlow, &
                                 VerticalGlhe(GlheNum)%GlheInletNodeNum, &
                                 VerticalGlhe(GlheNum)%GlheOutletNodeNum, &
                                 VerticalGlhe(GlheNum)%LoopNum, &
                                 VerticalGlhe(GlheNum)%LoopSideNum, &
                                 VerticalGlhe(GlheNum)%BranchNum, &
                                 VerticalGlhe(GlheNum)%CompNum)

      LastQnSubHr = 0.0d0
      Node(VerticalGlhe(GlheNum)%GlheInletNodeNum)%Temp                  = VerticalGlhe(GlheNum)%TempGround
      Node(VerticalGlhe(GlheNum)%GlheOutletNodeNum)%Temp                 = VerticalGlhe(GlheNum)%TempGround

      ! zero out all history arrays

      VerticalGlhe(GlheNum)%QnHr           = 0.0d0
      VerticalGlhe(GlheNum)%QnMonthlyAgg   = 0.0d0
      VerticalGlhe(GlheNum)%QnSubHr        = 0.0d0
      VerticalGlhe(GlheNum)%LastHourN      = 0
      PrevTimeSteps  = 0.0d0
      CurrentSimtime = 0.0d0
    END IF

  MDotActual = RegulateCondenserCompFlowReqOp(VerticalGlhe(GlheNum)%LoopNum,     &
                                              VerticalGlhe(GlheNum)%LoopSideNum, &
                                              VerticalGlhe(GlheNum)%BranchNum,   &
                                              VerticalGlhe(GlheNum)%CompNum,     &
                                              VerticalGlhe(GlheNum)%DesignMassFlow)

  CALL SetComponentFlowRate( MDotActual, &
                               VerticalGlhe(GlheNum)%GlheInletNodeNum, &
                               VerticalGlhe(GlheNum)%GlheOutletNodeNum, &
                               VerticalGlhe(GlheNum)%LoopNum, &
                               VerticalGlhe(GlheNum)%LoopSideNum, &
                               VerticalGlhe(GlheNum)%BranchNum, &
                               VerticalGlhe(GlheNum)%CompNum)

  ! Resent local environment init flag
  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.


  RETURN
END SUBROUTINE InitBoreholeHXSimVars

!******************************************************************************

SUBROUTINE UpdateVerticalGroundHeatExchanger(RunFlag, Num)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    August, 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the GLHE report variable data structure

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY : TrimSigDigits
  USE PlantUtilities, ONLY: SafeCopyPlantNode
  USE DataPlant,  ONLY: PlantLoop
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL              :: RunFlag
  INTEGER              :: Num


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER      :: DeltaTempLimit = 100.d0    ! temp limit for warnings

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  INTEGER                :: GlheInletNode      ! Inlet node number of the Glhe
  INTEGER                :: GlheOutletNode     ! Outlet node number of the Glhe
  REAL(r64)              :: GlhedeltaTemp      ! ABS(Outlet temp -inlet temp)
  INTEGER, SAVE          :: NumErrorCalls = 0
  REAL(r64)              :: DesignMassFlow
  REAL(r64)              :: FluidDensity

        !set node temperatures
  GlheInletNode                             = VerticalGlhe(Num)%GlheInletNodeNum
  GlheOutletNode                            = VerticalGlhe(Num)%GlheOutletNodeNum

  CALL SafeCopyPlantNode(GlheInletNode, GlheOutletNode)

  Node(GlheOutletNode)%Temp                 = GlheOutletTemp
  Node(GlheOutletNode)%Enthalpy             = GlheOutletTemp * GetSpecificHeatGlycol( &
                                                     PlantLoop(VerticalGlhe(Num)%LoopNum)%FluidName, &
                                                     GlheOutletTemp, &
                                                     PlantLoop(VerticalGlhe(Num)%LoopNum)%FluidIndex, &
                                                     'UpdateVerticalGroundHeatExchanger')
  GlhedeltaTemp                             = ABS(GlheOutletTemp-GlheInletTemp)
  VerticalGlheReport(Num)%GlheBoreholeTemp  = GlheBoreholeTemp
  VerticalGlheReport(Num)%GlheOutletTemp    = GlheOutletTemp
  ! calc load from load per unit length.
  VerticalGlheReport(Num)%QGlhe             = QGlhe * VerticalGlhe(Num)%BoreholeLength * &
                                                      VerticalGlhe(Num)%NumBoreholes
  VerticalGlheReport(Num)%GlheInletTemp     = GlheInletTemp
  VerticalGlheReport(Num)%GlheMassFlowRate  = GlheMassFlowRate
  VerticalGlheReport(Num)%GlheAveFluidTemp  = GlheAveFluidTemp

    IF (GlhedeltaTemp > DeltaTempLimit .AND. NumErrorCalls < NumVerticalGlhes .AND. .NOT. Warmupflag) THEN
      FluidDensity = GetDensityGlycol(PlantLoop(VerticalGlhe(Num)%LoopNum)%FluidName, &
                                       GlheInletTemp, &
                                       PlantLoop(VerticalGlhe(Num)%LoopNum)%FluidIndex, &
                                       'UpdateVerticalGroundHeatExchanger')
      DesignMassFlow = VerticalGlhe(Num)%DesignFlow* FluidDensity
      CALL ShowWarningError('Check GLHE design inputs & g-functions for consistency')
      CALL ShowContinueError('For GroundHeatExchanger:Vertical ' //TRIM(VerticalGlhe(Num)%Name)//'GLHE delta Temp > 100C.')
      CALL ShowContinueError('This can be encountered in cases where the GLHE mass flow rate is either significantly')
      CALL ShowContinueError(' lower than the design value, or cases where the mass flow rate rapidly changes.')
      CALL ShowContinueError('Glhe Current Flow Rate='//TRIM(TrimSigDigits(GlheMassFlowRate,3))// &
                           '; Glhe Design Flow Rate='//TRIM(TrimSigDigits(DesignMassFlow,3)))
      NumErrorCalls = NumErrorCalls + 1
    ENDIF

  RETURN

END SUBROUTINE UpdateVerticalGroundHeatExchanger


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

END MODULE GroundHeatExchangers


