MODULE WatertoAirMulSpeeddHP

 ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE Psychrometrics
Use DataEnvironment, ONLY: StdBaroPress, EnvironmentName, CurMnDy, OutDryBulbTemp, OutHumRat, OutBaroPress, OutWetBulbTemp
USE DataInterfaces
USE DataSizing
USE DataPlant,       ONLY: TypeOf_CoilVSWAHPHeatingEquationFit, TypeOf_CoilVSWAHPCoolingEquationFit

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS

REAL(r64), PARAMETER ::    RatedInletAirTemp   = 26.6667d0   ! 26.6667C or 80F
REAL(r64), PARAMETER ::    RatedInletWetbulbTemp = 19.44d0   ! 19.44 or 67F, cooling mode
REAL(r64), PARAMETER ::    RatedInletAirHumRat = 0.01125d0   ! Humidity ratio corresponding to 80F dry bulb/67F wet bulb
REAL(r64), PARAMETER ::    RatedInletWaterTemp = 29.4d0      ! 85 F cooling mode
REAL(r64), PARAMETER ::    RatedInletAirTempHeat = 21.11d0   ! 21.11C or 70F, heating mode
REAL(r64), PARAMETER ::    RatedInletWaterTempHeat = 21.11d0  ! 21.11C or 70F, heating mode

! Airflow per total capacity range
REAL(r64), PARAMETER ::    MaxRatedVolFlowPerRatedTotCap = 0.00006041d0 ! m3/s per watt = 450 cfm/ton
REAL(r64), PARAMETER ::    MinRatedVolFlowPerRatedTotCap = 0.00004027d0 ! m3/s per watt = 300 cfm/ton
REAL(r64), PARAMETER ::    MaxHeatVolFlowPerRatedTotCap  = 0.00008056d0 ! m3/s per watt = 600 cfm/ton
REAL(r64), PARAMETER ::    MaxCoolVolFlowPerRatedTotCap  = 0.00006713d0 ! m3/s per watt = 500 cfm/ton
REAL(r64), PARAMETER ::    MinOperVolFlowPerRatedTotCap  = 0.00002684d0 ! m3/s per watt = 200 cfm/ton

! Curve Types
INTEGER, PARAMETER :: Linear      = 1
INTEGER, PARAMETER :: Bilinear    = 2
INTEGER, PARAMETER :: Quadratic   = 3
INTEGER, PARAMETER :: Biquadratic = 4
INTEGER, PARAMETER :: Cubic       = 5

INTEGER, PUBLIC, PARAMETER :: MaxSpedLevels = 10  ! Maximum number of speed that supports

  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC :: WatertoAirMulSpedCoilData ! water-to-air variable speed coil
  CHARACTER(len=MaxNameLength) :: Name           =' '    ! Name of the  Coil
  CHARACTER(len=MaxNameLength) :: WtoADXCoilType     =' '    ! type of coil

  INTEGER :: NumOfSpeeds    =2   ! Number of speeds
  INTEGER :: NormSpedLevel    =MaxSpedLevels   ! Nominal speed level

  REAL(r64) :: RatedWaterVolFlowRate           =AUTOSIZE  ! Rated/Ref Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedWaterMassFlowRate          =AUTOSIZE  ! Rated/Ref Water Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedAirVolFlowRate             =AUTOSIZE  ! Rated/Ref Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: RatedCapHeat                    =AUTOSIZE  ! Rated/Ref Heating Capacity [W]
  REAL(r64) :: RatedCapCoolTotal               =AUTOSIZE  ! Rated/Ref Total Cooling Capacity [W]

  REAL(r64):: MaxONOFFCyclesperHour = 0.0 ! Maximum ON/OFF cycles per hour for the compressor (cycles/hour)

  REAL(r64):: Twet_Rated = 0.0      ! Nominal time for condensate to begin leaving the coil's
                                                   ! condensate drain line (sec)
  REAL(r64):: Gamma_Rated = 0.0     ! Initial moisture evaporation rate divided by steady-state
                                                   ! AC latent capacity (dimensionless)
  INTEGER :: HOTGASREHEATFLG = 0    !whether to use hot gas reheat
  REAL(r64) :: HPTimeConstant                  =0.0  ! Heat pump time constant [s]

  INTEGER:: PLFFPLR = 0     ! index of part load curve as a function of part load ratio

  CHARACTER(len=MaxNameLength) :: WatertoAirHPType=' ' ! Type of WatertoAirHP ie. Heating or Cooling
  INTEGER   :: WAHPPlantTypeOfNum              = 0     ! type of component in plant
  LOGICAL   :: Simflag                         =.false. ! Heat Pump Simulation Flag
  REAL(r64) :: DesignWaterMassFlowRate         =0.0  ! design water mass flow rate [kg/s]
  REAL(r64) :: DesignWaterVolFlowRate          =0.0  ! design water volumetric flow rate [m3/s]
  REAL(r64) :: DesignAirMassFlowRate           =0.0  ! Design Air Mass Flow Rate [kg/s]
  REAL(r64) :: DesignAirVolFlowRate            =0.0  ! Design Air Volumetric Flow Rate [m3/s]
  REAL(r64) :: AirVolFlowRate                  =0.0  ! Air Volumetric Flow Rate[m3/s], real time
  REAL(r64) :: AirMassFlowRate                 =0.0  ! Air Mass Flow Rate[kg/s], real time
  REAL(r64) :: InletAirPressure                =0.0 !air inlet pressure [pa]
  REAL(r64) :: InletAirDBTemp                  =0.0  ! Inlet Air Dry Bulb Temperature [C], real time
  REAL(r64) :: InletAirHumRat                  =0.0  ! Inlet Air Humidity Ratio [kg/kg], real time
  REAL(r64) :: InletAirEnthalpy                =0.0  ! Inlet Air Enthalpy [J/kg], real time
  REAL(r64) :: OutletAirDBTemp                 =0.0  ! Outlet Air Dry Bulb Temperature [C], real time
  REAL(r64) :: OutletAirHumRat                 =0.0  ! Outlet Air Humidity Ratio [kg/kg], real time
  REAL(r64) :: OutletAirEnthalpy               =0.0  ! Outlet Air Enthalpy [J/kg], real time
  REAL(r64) :: WaterVolFlowRate                =0.0  ! Water Volumetric Flow Rate [m3/s], real time
  REAL(r64) :: WaterMassFlowRate               =0.0  ! Water Mass Flow Rate [kg/s], real time
  REAL(r64) :: InletWaterTemp                  =0.0  ! Inlet Water Temperature [C]
  REAL(r64) :: InletWaterEnthalpy              =0.0  ! Inlet Water Enthalpy [J/kg]
  REAL(r64) :: OutletWaterTemp                 =0.0  ! Outlet Water Temperature [C]
  REAL(r64) :: OutletWaterEnthalpy             =0.0  ! Outlet Water Enthalpy [J/kg]
  REAL(r64) :: Power                           =0.0  ! Power Consumption [W]
  REAL(r64) :: QLoadTotal                      =0.0  ! Load Side Total Heat Transfer Rate [W]
  REAL(r64) :: QSensible                       =0.0  ! Sensible Load Side Heat Transfer Rate [W]
  REAL(r64) :: QLatent                         =0.0  ! Latent Load Side Heat Transfer Rate [W]
  REAL(r64) :: QSource                         =0.0  ! Source Side Heat Transfer Rate [W]
  REAL(r64) :: QWasteHeat                      =0.0  ! Recoverable waste Heat Transfer Rate [W]
  REAL(r64) :: Energy                          =0.0  ! Energy Consumption [J]
  REAL(r64) :: EnergyLoadTotal                 =0.0  ! Load Side Total Heat Transferred [J]
  REAL(r64) :: EnergySensible                  =0.0  ! Sensible Load Side Heat Transferred [J]
  REAL(r64) :: EnergyLatent                    =0.0  ! Latent Load Side Heat Transferred [J]
  REAL(r64) :: EnergySource                    =0.0  ! Source Side Heat Transferred [J]
  REAL(r64) :: COP                             =0.0  ! Heat Pump Coefficient of Performance [-]
  REAL(r64) :: RunFrac                         =0.0  ! Duty Factor
  REAL(r64) :: PartLoadRatio                   =0.0  ! Part Load Ratio

  REAL(r64) :: RatedPowerHeat                  =0.0  ! Rated/Ref Heating Power Consumption[W]
  REAL(r64) :: RatedCOPHeat                    =0.0  ! Rated/Ref Heating COP [W/W]
  REAL(r64) :: RatedCapCoolSens                =0.0  ! Rated/Ref Sensible Cooling Capacity [W]
  REAL(r64) :: RatedPowerCool                  =0.0  ! Rated/Ref Cooling Power Consumption[W]
  REAL(r64) :: RatedCOPCool                    =0.0  ! Rated/Ref Cooling COP [W/W]

  INTEGER      :: AirInletNodeNum              =0    ! Node Number of the Air Inlet
  INTEGER      :: AirOutletNodeNum             =0     ! Node Number of the Air Outlet
  INTEGER      :: WaterInletNodeNum            =0     ! Node Number of the Water Onlet
  INTEGER      :: WaterOutletNodeNum           =0     ! Node Number of the Water Outlet
  INTEGER      :: LoopNum                      =0    ! plant loop index for water side
  INTEGER      :: LoopSide                     =0    ! plant loop side index
  INTEGER      :: BranchNum                    =0    ! plant branch index
  INTEGER      :: CompNum                      =0    ! plant component index
! set by parent object and "pushed" to this structure in SetVSWSHPData subroutine

  LOGICAL      :: FindCompanionUpStreamCoil    = .TRUE.    ! Flag to get the companion coil in Init.
  INTEGER      :: CompanionCoolingCoilNum      =0           ! Heating coil companion cooling coil index
  INTEGER      :: CompanionHeatingCoilNum      =0           ! Cooling coil companion heating coil index

  REAL(r64) :: FanDelayTime                    =0.0  ! Fan delay time, time delay for the HP's fan to

  ! beginning for multispeed coil type
  INTEGER       :: MSErrIndex(MaxSpedLevels) = 0                ! index flag for num speeds/recurring messages
  REAL(r64)     :: MSRatedPercentTotCap(MaxSpedLevels) = 0.0    ! Percentage to the total cooling capacity for MS heat pump at the highest speed [dimensionless]
  REAL(r64)     :: MSRatedTotCap(MaxSpedLevels) = 0.0           ! Rated cooling capacity for MS heat pump [W]
  REAL(r64)     :: MSRatedSHR(MaxSpedLevels) = 0.0              ! Rated SHR for MS heat pump [dimensionless]
  REAL(r64)     :: MSRatedCOP(MaxSpedLevels) = 0.0              ! Rated COP for MS heat pump [dimensionless]

  REAL(r64)     :: MSRatedAirVolFlowPerRatedTotCap(MaxSpedLevels) = 0.0
  ! Rated Air volume flow rate per total capacity through unit at rated conditions [m^3/w]
  REAL(r64)     :: MSRatedAirVolFlowRate(MaxSpedLevels) = 0.0
  ! Air volume flow rate through unit at rated conditions [m3/s]
  REAL(r64)     :: MSRatedAirMassFlowRate(MaxSpedLevels) = 0.0
  ! Air mass flow rate through unit at rated conditions [kg/s]

  REAL(r64)     :: MSRatedWaterVolFlowPerRatedTotCap(MaxSpedLevels) = 0.0
  ! Rated water volume flow rate per total  capacity through unit at rated conditions [m^3/w]
  REAL(r64)     :: MSRatedWaterVolFlowRate(MaxSpedLevels) = 0.0
  ! Water volume flow rate through unit at rated conditions [m3/s]
  REAL(r64)     :: MSRatedWaterMassFlowRate(MaxSpedLevels) = 0.0
  ! Water mass flow rate through unit at rated conditions [kg/s]

  REAL(r64)     :: MSRatedCBF(MaxSpedLevels) = 0.0
  ! rated coil bypass factor
  REAL(r64)     :: MSEffectiveAo(MaxSpedLevels) = 0.0
  ! effective heat transfer surface at each speed

  INTEGER       :: MSCCapFTemp(MaxSpedLevels) = 0
  ! index of total capacity modifier curve
  INTEGER       :: MSCCapAirFFlow(MaxSpedLevels) = 0
  ! index of total capacity modifier curve as a function of air flow
  INTEGER       :: MSCCapWaterFFlow(MaxSpedLevels) = 0
  ! index of total capacity modifier curve as a function of water flow
  INTEGER       :: MSEIRFTemp(MaxSpedLevels) = 0
  ! index of energy input ratio modifier curve as a function of temperature
  INTEGER       :: MSEIRAirFFlow(MaxSpedLevels) = 0
  ! index of energy input ratio modifier curve as a function of air flow fraction
  INTEGER       :: MSEIRWaterFFlow(MaxSpedLevels) = 0
  ! index of energy input ratio modifier curve as a function of water flow fraction
  INTEGER       :: MSWasteHeat(MaxSpedLevels) = 0
  ! index of waste heat as a function of temperature
  REAL(r64)     :: MSWasteHeatFrac(MaxSpedLevels) = 0.0
  ! Waste heat fraction

  REAL(r64):: SpeedNumReport = 0.0
  !speed number for output
  REAL(r64):: SpeedRatioReport = 0.0
  !speed ratio for output between two neighboring speeds
  ! End of multispeed DX coil input

END TYPE WatertoAirMulSpedCoilData


  ! MODULE VARIABLE DECLARATIONS:
  ! Identifier is WtoADXCoil
INTEGER        :: NumWatertoAirHPs  = 0        ! The Number of Water to Air Heat Pumps found in the Input

LOGICAL        :: GetCoilsInputFlag = .TRUE.       ! Flag set to make sure you get input once
TYPE (WatertoAirMulSpedCoilData) , PUBLIC, ALLOCATABLE, DIMENSION(:) :: WtoADXCoil
! LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag

REAL(r64) :: SourceSideMassFlowRate =0.0 ! Source Side Mass flow rate [Kg/s]
REAL(r64) :: SourceSideInletTemp    =0.0 ! Source Side Inlet Temperature [C]
REAL(r64) :: SourceSideInletEnth    =0.0 ! Source Side Inlet Enthalpy [J/kg]
REAL(r64) :: LoadSideMassFlowRate   =0.0 ! Load Side Mass flow rate [Kg/s]
REAL(r64) :: LoadSideInletDBTemp    =0.0 ! Load Side Inlet Dry Bulb Temp [C]
REAL(r64) :: LoadSideInletWBTemp    =0.0 ! Load Side Inlet Wet Bulb Temp [C]
REAL(r64) :: LoadSideInletHumRat    =0.0 ! Load Side Outlet Humidity ratio
REAL(r64) :: LoadSideInletEnth      =0.0 ! Load Side Inlet Enthalpy [J/kg]
REAL(r64) :: LoadSideOutletDBTemp   =0.0 ! Load Side Outlet Dry Bulb Temp [C]
REAL(r64) :: LoadSideOutletHumRat   =0.0 ! Load Side Outlet Humidity ratio
REAL(r64) :: LoadSideOutletEnth     =0.0 ! Load Side Outlet Enthalpy [J/kg]
REAL(r64) :: QSensible              =0.0 ! Load side sensible heat transfer rate [W]
REAL(r64) :: QLoadTotal             =0.0 ! Load side total heat transfer rate [W]
REAL(r64) :: QLatRated              =0.0 ! Latent Capacity [W] rated at entering air conditions [Tdb=26.7C Twb=19.4C]
REAL(r64) :: QLatActual             =0.0 ! Actual Latent Capacity [W]
REAL(r64) :: QSource                =0.0 ! Source side heat transfer rate [W]
REAL(r64) :: Winput                 =0.0 ! Power Consumption [W]
REAL(r64) :: PLRCorrLoadSideMdot    =0.0 ! Load Side Mdot corrected for Part Load Ratio of the unit


  ! SUBROUTINE SPECIFICATIONS FOR MODULE

          ! Driver/Manager Routines
PUBLIC SimWatertoAirHPMulSpeed

          ! Get Input routines for module
PRIVATE GetMulSpeedWSHPInput

          ! Initialization routines for module
PRIVATE InitMulSpeedWSHPCoil
PRIVATE SizeMulSpeedWSHPCoil

          ! Update routines to check convergence and update nodes
PUBLIC  CalcMulSpeedWSHPCoilCooling
PUBLIC  CalcMulSpeedWSHPCoilHeating

          ! Update routine
PRIVATE UpdateMulSpeedWSHP

          ! Utility routines
PUBLIC  GetCoilIndexMulSpeedWSHP
PUBLIC  GetCoilCapacityMulSpeedWSHP
PUBLIC  GetCoilInletNodeMulSpeedWSHP
PUBLIC  GetCoilOutletNodeMulSpeedWSHP
PUBLIC  GetCoilAirFlowRateMulSpeedWSHP
PUBLIC  SetMulSpeedWSHPData
        !SHR, bypass factor routines
PRIVATE CalcEffectiveSHR
PRIVATE CalcTotCapSHR_VSWSHP
PRIVATE CalcCBF
PRIVATE AdjustCBF

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimWatertoAirHPMulSpeed(CompName,CompIndex,&
           CyclingScheme,MaxONOFFCyclesperHour, &
           HPTimeConstant,FanDelayTime,CompOp, PartLoadFrac, OnOffAirFlowRat,SpeedNum, SpeedRatio, &
           SensLoad, LatentLoad)

          !       AUTHOR         Bo Shen, ORNL
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages variable-speed Water to Air Heat Pump component simulation.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! N/A



          ! USE STATEMENTS:
  USE InputProcessor,       ONLY: FindItemInList
  USE DataHVACGlobals,      ONLY: TimestepSys
  USE FluidProperties,      ONLY: FindGlycol
  USE General,              ONLY: TrimSigDigits, SolveRegulaFalsi

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  CHARACTER(len=*), INTENT(IN)      :: CompName                     ! Coil Name
  INTEGER, INTENT(INOUT)            :: CompIndex                    ! Index for Component name
  INTEGER, INTENT(IN)               :: CyclingScheme                ! Continuous fan OR cycling compressor
  REAL(r64), INTENT (INOUT)         :: MaxONOFFCyclesperHour        ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64), INTENT (INOUT)         :: HPTimeConstant               ! Heat pump time constant [s]
  REAL(r64), INTENT (INOUT)         :: FanDelayTime                 ! Fan delay time, time delay for the HP's fan to
                                                                    ! shut off after compressor cycle off  [s]
  INTEGER, INTENT(IN)               :: CompOp                       ! compressor on/off. 0 = off; 1= on
  REAL(r64), INTENT(IN)             :: PartLoadFrac
  ! part-load ratio = load/total capacity, passed in by the parent object
  REAL(r64), OPTIONAL, INTENT(IN)   :: OnOffAirFlowRat              ! ratio of comp on to comp off air flow rate
  REAL(r64), INTENT(IN)             :: SensLoad                     ! Sensible demand load [W]
  REAL(r64), INTENT(IN)             :: LatentLoad                   ! Latent demand load [W]
  REAL(r64), INTENT(IN)             :: SpeedRatio                   ! compressor speed ratio
  INTEGER, INTENT(IN)               :: SpeedNum                     ! compressor speed number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: DXCoilNum                         ! The WatertoAirHP that you are currently loading input into
  REAL(r64)                    :: OnOffAirFlowRatio                 ! ratio of comp on to comp off air flow rate
  REAL(r64)                    :: RuntimeFrac                       ! run time fraction
  INTEGER                      :: SpeedCal                          ! variable for error proof speed input

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (CompIndex == 0) THEN
    DXCoilNum = FindItemInList(CompName,WtoADXCoil%Name,NumWatertoAirHPs )
    IF (DXCoilNum == 0) THEN
      CALL ShowFatalError('WaterToAirHPVSWEquationFit not found='//TRIM(CompName))
    ENDIF
    CompIndex=DXCoilNum
  ELSE
    DXCoilNum=CompIndex
    IF (DXCoilNum > NumWatertoAirHPs  .or. DXCoilNum < 1) THEN
      CALL ShowFatalError('SimWatertoAirHPMulSpeed: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DXCoilNum))// &
                          ', Number of Water to Air HPs='//TRIM(TrimSigDigits(NumWatertoAirHPs))//  &
                          ', WaterToAir HP name='//TRIM(CompName))
    ENDIF
    IF (CompName /= Blank .AND. CompName /= WtoADXCoil(DXCoilNum)%Name) THEN
      CALL ShowFatalError('SimWatertoAirHPMulSpeed: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DXCoilNum))// &
                          ', WaterToAir HP name='//TRIM(CompName)//', stored WaterToAir HP Name for that index='//  &
                          TRIM(WtoADXCoil(DXCoilNum)%Name))
    ENDIF
  ENDIF

  IF(PRESENT(OnOffAirFlowRat))THEN
    OnOffAirFlowRatio = OnOffAirFlowRat
  ELSE
    OnOffAirFlowRatio = 1.0d0
  END IF

 !ERROR PROOF
 IF(SpeedNum < 1) THEN
    SpeedCal = 1
 ELSE
    SpeedCal = SpeedNum
 END IF

 IF(WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit)THEN
    ! Cooling mode
    CALL InitMulSpeedWSHPCoil(DXCoilNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL CalcMulSpeedWSHPCoilCooling(DXCoilNum,CyclingScheme,RuntimeFrac,&
                SensLoad,LatentLoad,CompOp,PartLoadFrac,OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL UpdateMulSpeedWSHP(DXCoilNum)
 ELSEIF(WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit)THEN
    ! Heating mode
    CALL InitMulSpeedWSHPCoil(DXCoilNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL CalcMulSpeedWSHPCoilHeating(DXCoilNum,CyclingScheme,RuntimeFrac,&
                SensLoad,CompOp,PartLoadFrac,OnOffAirFlowRatio, SpeedRatio, SpeedCal)
    CALL UpdateMulSpeedWSHP(DXCoilNum)
 ELSE
    CALL ShowFatalError ('SimWatertoAirHPMulSpeed: WatertoAir heatpump not in either HEATING or COOLING mode')
 ENDIF

 ! two additional output variables
 WtoADXCoil(DXCoilNum)%SpeedNumReport = SpeedCal
 WtoADXCoil(DXCoilNum)%SpeedRatioReport = SpeedRatio

 RETURN

END SUBROUTINE SimWatertoAirHPMulSpeed

SUBROUTINE GetMulSpeedWSHPInput

       ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for HPs and stores it in HP data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! n/a

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager
    USE BranchNodeConnections, ONLY: TestCompSet
    USE GlobalNames,     ONLY: VerifyUniqueWaterToAirHPName
    USE OutputReportPredefined
    USE General,               ONLY: TrimSigDigits
    USE CurveManager,          ONLY: GetCurveIndex, GetCurveType, CurveValue, SetCurveOutputMinMaxValues

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER (len=*), PARAMETER   :: RoutineName='GetMulSpeedWSHPInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: DXCoilNum                    ! The Water to Air HP that you are currently loading input into
    INTEGER :: NumCool                      ! Counter for cooling coil
    INTEGER :: NumHeat                      ! Counter for heating coil
    INTEGER :: WatertoAirHPNum              ! Counter
    INTEGER :: I                            ! Loop index increment
    INTEGER :: NumAlphas                    ! Number of variables in String format
    INTEGER :: NumNums                      ! Number of variables in Numeric format
    INTEGER :: NumParams                    ! Total number of input fields
    INTEGER :: MaxNums=0                    ! Maximum number of numeric input fields
    INTEGER :: MaxAlphas=0                  ! Maximum number of alpha input fields
    INTEGER :: IOSTAT
    INTEGER :: AlfaFieldIncre               !increment number of Alfa field
    LOGICAL :: ErrorsFound = .FALSE.        ! If errors detected in input
    LOGICAL       :: IsNotOK                ! Flag to verify name
    LOGICAL       :: IsBlank                ! Flag for blank name
    LOGICAL       :: errflag
    REAL(r64) :: CurveVal                   ! Used to verify modifier curves equal 1 at rated conditions
    CHARACTER (len=MaxNameLength)  :: CurrentModuleObject                       ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray        ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields     ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields   ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray                            ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks                        ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks                      ! Logical array, numeric field input BLANK = .true.

    NumCool   = GetNumObjectsFound('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT')
    NumHeat   = GetNumObjectsFound('COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT')
    NumWatertoAirHPs = NumCool+NumHeat
    DXCoilNum=0

    IF(NumWatertoAirHPs <= 0) THEN
      CALL ShowSevereError('No Equipment found in SimWatertoAirHPSimple')
      ErrorsFound=.TRUE.
    END IF

   ! Allocate Arrays
    IF (NumWatertoAirHPs.GT.0) THEN
      ALLOCATE(WtoADXCoil(NumWatertoAirHPs))
    ENDIF

    CALL GetObjectDefMaxArgs('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT',NumParams,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    ALLOCATE(AlphArray(MaxAlphas))
    AlphArray=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.TRUE.
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0

      ! Get the data for cooling coil
    CurrentModuleObject = 'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit' !for reporting

    DO WatertoAirHPNum = 1, NumCool

        DXCoilNum= DXCoilNum + 1
        AlfaFieldIncre = 1

        CALL GetObjectItem(TRIM(CurrentModuleObject),DXCoilNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),WtoADXCoil%Name,DXCoilNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueWaterToAirHPName(TRIM(CurrentModuleObject),AlphArray(1),errflag,  &
                                          TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        WtoADXCoil(DXCoilNum)%Name     = TRIM(AlphArray(1))
        WtoADXCoil(DXCoilNum)%WatertoAirHPType  = 'COOLING'
        WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum  = TypeOf_CoilVSWAHPCoolingEquationFit
        WtoADXCoil(DXCoilNum)%NumOfSpeeds = INT(NumArray(1))
        WtoADXCoil(DXCoilNum)%NormSpedLevel = INT(NumArray(2))
        WtoADXCoil(DXCoilNum)%RatedCapCoolTotal=NumArray(3)
        WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate = NumArray(4)
        WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate=NumArray(5)
        WtoADXCoil(DXCoilNum)%Twet_Rated=NumArray(6)
        WtoADXCoil(DXCoilNum)%Gamma_Rated=NumArray(7)
        WtoADXCoil(DXCoilNum)%HOTGASREHEATFLG=INT(NumArray(8))

        WtoADXCoil(DXCoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WtoADXCoil(DXCoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WtoADXCoil(DXCoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WtoADXCoil(DXCoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1),  &
                                   NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Air Nodes')


        If (WtoADXCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(1))//' must be >= 2.'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(1),0)))
          ErrorsFound=.TRUE.
        End If

        If ((WtoADXCoil(DXCoilNum)%NormSpedLevel > WtoADXCoil(DXCoilNum)%NumOfSpeeds) &
            .OR. (WtoADXCoil(DXCoilNum)%NormSpedLevel <= 0)) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' must be valid speed level'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(2),0)))
          ErrorsFound=.TRUE.
        End If

        !part load curve
        WtoADXCoil(DXCoilNum)%PLFFPLR = GetCurveIndex(AlphArray(6)) ! convert curve name to number
        IF (WtoADXCoil(DXCoilNum)%PLFFPLR .EQ. 0) THEN
          IF (lAlphaBlanks(6)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(6))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
            CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%PLFFPLR,1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)&
                        //'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(6))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF
        END IF

        Do I=1,WtoADXCoil(DXCoilNum)%NumOfSpeeds
            WtoADXCoil(DXCoilNum)%MSRatedTotCap(I) = NumArray(9+(I-1)*6)
            WtoADXCoil(DXCoilNum)%MSRatedSHR(I)    = NumArray(10+(I-1)*6)
            WtoADXCoil(DXCoilNum)%MSRatedCOP(I)    = NumArray(11+(I-1)*6)
            WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(I) = NumArray(12+(I-1)*6)
            WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I) = NumArray(13+(I-1)*6)
            WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(I) = NumArray(14+(I-1)*6)

            AlfaFieldIncre = 7+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSCCapFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))&
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is BiQuadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(I),RatedInletWetbulbTemp,RatedInletWaterTemp)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                            //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapFTemp(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 8+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre ))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre ))//'="' &
                        //TRIM(AlphArray(AlfaFieldIncre ))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)// &
                        '="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre ))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName// &
                        trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre ))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 9+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre =10+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSEIRFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//&
                        TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Biquadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(I),RatedInletWetbulbTemp,RatedInletWaterTemp)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRFTemp(1))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre =11+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))&
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                            //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject) &
                        //'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

           AlfaFieldIncre =12+(I-1)*7
           WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre =13+(I-1)*7
            ! Read waste heat modifier curve name
            WtoADXCoil(DXCoilNum)%MSWasteHeat(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSWasteHeat(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal types are BiQuadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSWasteHeat(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(I),RatedInletWaterTemp,RatedInletAirTemp)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSWasteHeat(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

        END DO

        Do I=1,WtoADXCoil(DXCoilNum)%NumOfSpeeds
            WtoADXCoil(DXCoilNum)%MSRatedPercentTotCap(I) =WtoADXCoil(DXCoilNum)%MSRatedTotCap(I)/ &
                    WtoADXCoil(DXCoilNum)%MSRatedTotCap(WtoADXCoil(DXCoilNum)%NumOfSpeeds)
            WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(I) = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(I)/ &
                    WtoADXCoil(DXCoilNum)%MSRatedTotCap(I)
            WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(I) = WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I)/ &
                    WtoADXCoil(DXCoilNum)%MSRatedTotCap(I)
        END DO

        CALL SetupOutputVariable('VSWatertoAirHP Cooling Electric Consumption [J]', &
             WtoADXCoil(DXCoilNum)%Energy,'System','Summed',WtoADXCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Cooling',GroupKey='System')

        CALL SetupOutputVariable('VSWatertoAirHP Load Side Total Cooling Energy [J]', &
             WtoADXCoil(DXCoilNum)%EnergyLoadTotal,'System','Summed',WtoADXCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('VSWatertoAirHP Load Side Sensible Cooling Energy [J]', &
             WtoADXCoil(DXCoilNum)%EnergySensible,'System','Summed',WtoADXCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('VSWatertoAirHP Load Side Latent Cooling Energy [J]', &
             WtoADXCoil(DXCoilNum)%EnergyLatent,'System','Summed',WtoADXCoil(DXCoilNum)%Name)

        CALL SetupOutputVariable('VSWatertoAirHP Source Side Cooling Energy [J]', &
             WtoADXCoil(DXCoilNum)%EnergySource,'System','Summed',WtoADXCoil(DXCoilNum)%Name,   &
             ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')

        !for table output, being consistent with outher water-to-air coils
!        IF (WtoADXCoil(DXCoilNum)%RatedCapCoolTotal /= AutoSize) THEN
!            WtoADXCoil(DXCoilNum)%RatedCapCoolSens = WtoADXCoil(DXCoilNum)%RatedCapCoolTotal &
!                *WtoADXCoil(DXCoilNum)%MSRatedSHR(WtoADXCoil(DXCoilNum)%NormSpedLevel)
!        ELSE
!            WtoADXCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE
!        END IF

        WtoADXCoil(DXCoilNum)%RatedCapCoolSens = AUTOSIZE !always auto-sized, to be determined in the sizing calculation

        !create predefined report entries
        CALL PreDefTableEntry(pdchCoolCoilType,WtoADXCoil(DXCoilNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchCoolCoilTotCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
        CALL PreDefTableEntry(pdchCoolCoilSensCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolSens)
        CALL PreDefTableEntry(pdchCoolCoilLatCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolTotal &
                                 - WtoADXCoil(DXCoilNum)%RatedCapCoolSens)
        CALL PreDefTableEntry(pdchCoolCoilSHR,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolSens &
                                 / WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
        CALL PreDefTableEntry(pdchCoolCoilNomEff,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedPowerCool &
                                 / WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)

   END DO

      ! Get the data for heating coil
   CurrentModuleObject = 'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit'

   DO WatertoAirHPNum = 1, NumHeat

        DXCoilNum= DXCoilNum + 1

        CALL GetObjectItem(TRIM(CurrentModuleObject),WatertoAirHPNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.FALSE.
        IsBlank=.FALSE.

        CALL VerifyName(AlphArray(1),WtoADXCoil%Name,DXCoilNum-1, ISNotOK,ISBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.TRUE.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueWaterToAirHPName(TRIM(CurrentModuleObject),AlphArray(1),errflag,  &
                                          TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF

        WtoADXCoil(DXCoilNum)%Name     = TRIM(AlphArray(1))
        WtoADXCoil(DXCoilNum)%WatertoAirHPType  = 'HEATING'
        WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum  = TypeOf_CoilVSWAHPHeatingEquationFit
        WtoADXCoil(DXCoilNum)%NumOfSpeeds = INT(NumArray(1))
        WtoADXCoil(DXCoilNum)%NormSpedLevel = INT(NumArray(2))
        WtoADXCoil(DXCoilNum)%RatedCapHeat=NumArray(3)
        WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate = NumArray(4)
        WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate=NumArray(5)

        WtoADXCoil(DXCoilNum)%WaterInletNodeNum    = &
               GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent)
        WtoADXCoil(DXCoilNum)%WaterOutletNodeNum   = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent)
        WtoADXCoil(DXCoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        WtoADXCoil(DXCoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),  &
                         AlphArray(1),NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(2),AlphArray(3),'Water Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject),AlphArray(1),AlphArray(4),AlphArray(5),'Air Nodes')


        If (WtoADXCoil(DXCoilNum)%NumOfSpeeds .LT. 2) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(1))//' must be >= 2.'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(1),0)))
          ErrorsFound=.TRUE.
        End If

        If ((WtoADXCoil(DXCoilNum)%NormSpedLevel > WtoADXCoil(DXCoilNum)%NumOfSpeeds) &
            .OR. (WtoADXCoil(DXCoilNum)%NormSpedLevel <= 0)) Then
          CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
          CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' must be valid speed level'//  &
                                                 ' entered number is '//TRIM(TrimSigDigits(NumArray(2),0)))
          ErrorsFound=.TRUE.
        End If

        !part load curve
        WtoADXCoil(DXCoilNum)%PLFFPLR = GetCurveIndex(AlphArray(6)) ! convert curve name to number
        IF (WtoADXCoil(DXCoilNum)%PLFFPLR .EQ. 0) THEN
          IF (lAlphaBlanks(6)) THEN
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
            CALL ShowContinueError('...required '//trim(cAlphaFields(6))//' is blank.')
          ELSE
            CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
            CALL ShowContinueError('...not found '//TRIM(cAlphaFields(6))//'="'//TRIM(AlphArray(6))//'".')
          END IF
          ErrorsFound = .TRUE.
        ELSE
            CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%PLFFPLR,1.0d0)
            IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
              CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="'&
                    //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
              CALL ShowContinueError('...'//TRIM(cAlphaFields(6))//' output is not equal to 1.0 '//  &
                                                 '(+ or - 10%) at rated conditions.')
              CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
            END IF
        END IF

        Do I=1,WtoADXCoil(DXCoilNum)%NumOfSpeeds
            WtoADXCoil(DXCoilNum)%MSRatedTotCap(I) = NumArray(6+(I-1)*5)
            WtoADXCoil(DXCoilNum)%MSRatedCOP(I)    = NumArray(7+(I-1)*5)
            WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(I) = NumArray(8+(I-1)*5)
            WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I) = NumArray(9+(I-1)*5)
            WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(I) = NumArray(10+(I-1)*5)

            AlfaFieldIncre = 7+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSCCapFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSCCapFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                            //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is BiQuadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(I),RatedInletAirTempHeat,RatedInletWaterTempHeat)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapFTemp(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 8+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="' &
                        //TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 9+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(14+(I-1)*6))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                            //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 10+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSEIRFTemp(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSEIRFTemp(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Biquadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRFTemp(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(I),RatedInletAirTempHeat,RatedInletWaterTempHeat)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRFTemp(1))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 11+(I-1)*7
            WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))//'="'//TRIM(AlphArray(16+(I-1)*6))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject) &
                        //'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

           AlfaFieldIncre = 12+(I-1)*7
           WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre))// &
                        '="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal type is Quadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I)))

              CASE('QUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)&
                        //'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE('CUBIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I),1.0d0)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                      TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(I))))
                CALL ShowContinueError('Curve type must be Quadratic or Cubic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

            AlfaFieldIncre = 13+(I-1)*7
            ! Read waste heat modifier curve name
            WtoADXCoil(DXCoilNum)%MSWasteHeat(I) = GetCurveIndex(AlphArray(AlfaFieldIncre)) ! convert curve name to number
            IF (WtoADXCoil(DXCoilNum)%MSWasteHeat(I) .EQ. 0) THEN
              IF (lAlphaBlanks(AlfaFieldIncre)) THEN
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", missing')
                CALL ShowContinueError('...required '//trim(cAlphaFields(AlfaFieldIncre))//' is blank.')
              ELSE
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...not found '//TRIM(cAlphaFields(AlfaFieldIncre)) &
                        //'="'//TRIM(AlphArray(AlfaFieldIncre))//'".')
              END IF
              ErrorsFound = .TRUE.
            ELSE
              ! Verify Curve Object, only legal types are BiQuadratic
              SELECT CASE(GetCurveType(WtoADXCoil(DXCoilNum)%MSWasteHeat(I)))

              CASE('BIQUADRATIC')
                CurveVal = CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(I),RatedInletAirTempHeat,RatedInletWaterTempHeat)
                IF(CurveVal .GT. 1.10d0 .OR. CurveVal .LT. 0.90d0) THEN
                  CALL ShowWarningError(RoutineName//trim(CurrentModuleObject)//'="' &
                        //trim(WtoADXCoil(DXCoilNum)%Name)//'", curve values')
                  CALL ShowContinueError('...'//TRIM(cAlphaFields(AlfaFieldIncre))//' output is not equal to 1.0 '//  &
                                                     '(+ or - 10%) at rated conditions.')
                  CALL ShowContinueError('...Curve output at rated conditions = '//TRIM(TrimSigDigits(CurveVal,3)))
                END IF

              CASE DEFAULT
                CALL ShowSevereError(RoutineName//trim(CurrentModuleObject)//'="'//trim(WtoADXCoil(DXCoilNum)%Name)//'", invalid')
                CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(AlfaFieldIncre))//' type for this object = '// &
                                     TRIM(GetCurveType(WtoADXCoil(DXCoilNum)%MSWasteHeat(I))))
                CALL ShowContinueError('Curve type must be BiQuadratic.')
                ErrorsFound=.TRUE.
              END SELECT
            END IF

        END DO

        Do I=1,WtoADXCoil(DXCoilNum)%NumOfSpeeds
            WtoADXCoil(DXCoilNum)%MSRatedPercentTotCap(I) =WtoADXCoil(DXCoilNum)%MSRatedTotCap(I)/ &
                    WtoADXCoil(DXCoilNum)%MSRatedTotCap(WtoADXCoil(DXCoilNum)%NumOfSpeeds)
            WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(I) = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(I)/ &
                        WtoADXCoil(DXCoilNum)%MSRatedTotCap(I)
            WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(I) = WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(I)/ &
                        WtoADXCoil(DXCoilNum)%MSRatedTotCap(I)
        END DO

        CALL SetupOutputVariable('VSWatertoAirHP Heating Electric Consumption [J]', &
             WtoADXCoil(DXCoilNum)%Energy,'System','Summed',WtoADXCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')

        CALL SetupOutputVariable('VSWatertoAirHP Load Side Total Heating Energy [J]', &
             WtoADXCoil(DXCoilNum)%EnergyLoadTotal,'System','Summed',WtoADXCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')

        CALL SetupOutputVariable('VSWatertoAirHP Source Side Heating Energy [J]', &
             WtoADXCoil(DXCoilNum)%EnergySource,'System','Summed',WtoADXCoil(DXCoilNum)%Name,  &
             ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='HEATINGCOILS',GroupKey='System')

        !create predefined report entries
        CALL PreDefTableEntry(pdchHeatCoilType,WtoADXCoil(DXCoilNum)%Name,CurrentModuleObject)
        CALL PreDefTableEntry(pdchHeatCoilNomCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapHeat)
        CALL PreDefTableEntry(pdchHeatCoilNomEff,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedPowerHeat &
                                 / WtoADXCoil(DXCoilNum)%RatedCapHeat)

   END DO

   DEALLOCATE(AlphArray)
   DEALLOCATE(cAlphaFields)
   DEALLOCATE(lAlphaBlanks)
   DEALLOCATE(cNumericFields)
   DEALLOCATE(lNumericBlanks)
   DEALLOCATE(NumArray)

   IF (ErrorsFound) THEN
     CALL ShowFatalError(RoutineName//'Errors found getting input. Program terminates.')
   ENDIF

   DO DXCoilNum=1,NumWatertoAirHPs
            ! Setup Report variables for the Heat Pump
       CALL SetupOutputVariable('VSWatertoAirHP Power [W]', &
            WtoADXCoil(DXCoilNum)%Power,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Total Heat Transfer Rate [W]', &
            WtoADXCoil(DXCoilNum)%QLoadTotal,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Sensible Heat Transfer Rate [W]', &
            WtoADXCoil(DXCoilNum)%QSensible,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Latent Heat Transfer Rate [W]', &
            WtoADXCoil(DXCoilNum)%QLatent,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Source Side Heat Transfer Rate [W]', &
            WtoADXCoil(DXCoilNum)%QSource,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Part Load Ratio', &
            WtoADXCoil(DXCoilNum)%PartLoadRatio,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Run Time Fraction', &
            WtoADXCoil(DXCoilNum)%RunFrac,'System','Average',WtoADXCoil(DXCoilNum)%Name)

       CALL SetupOutputVariable('VSWatertoAirHP Air Mass Flow Rate [kg/s]', &
            WtoADXCoil(DXCoilNum)%AirMassFlowRate,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Inlet Dry Bulb Temperature [C]', &
            WtoADXCoil(DXCoilNum)%InletAirDBTemp,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Inlet Humidity Ratio [kgWater/kgDryAir]', &
            WtoADXCoil(DXCoilNum)%InletAirHumRat,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Outlet Dry Bulb Temperature [C]', &
            WtoADXCoil(DXCoilNum)%OutletAirDBTemp,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Load Side Outlet Humidity Ratio [kgWater/kgDryAir]', &
            WtoADXCoil(DXCoilNum)%OutletAirHumRat,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Water Mass Flow Rate [kg/s]', &
            WtoADXCoil(DXCoilNum)%WaterMassFlowRate,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Source Side Inlet Temperature [C]', &
            WtoADXCoil(DXCoilNum)%InletWaterTemp,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Source Side Outlet Temperature [C]', &
            WtoADXCoil(DXCoilNum)%OutletWaterTemp,'System','Average',WtoADXCoil(DXCoilNum)%Name)

       !starting newly added output variables
       CALL SetupOutputVariable('VSWatertoAirHP Upper Speed Level', &
            WtoADXCoil(DXCoilNum)%SpeedNumReport,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Speed Ratio between Two Neighboring Speeds', &
            WtoADXCoil(DXCoilNum)%SpeedRatioReport,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       CALL SetupOutputVariable('VSWatertoAirHP Recoverable Waste Heat [W]', &
            WtoADXCoil(DXCoilNum)%QWasteHeat,'System','Average',WtoADXCoil(DXCoilNum)%Name)
       !end newly added output variables

   END DO

  IF (ErrorsFound) THEN
     CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' input.  '//&
                      'Preceding condition(s) causes termination.')
  END IF

  RETURN

END SUBROUTINE GetMulSpeedWSHPInput

 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitMulSpeedWSHPCoil(DXCoilNum,MaxONOFFCyclesperHour,HPTimeConstant,FanDelayTime,SensLoad,LatentLoad,CyclingScheme, &
                                  OnOffAirFlowRatio, SpeedRatio, SpeedNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on  MODULE WatertoAirHeatPumpSimple:InitSimpleWatertoAirHP
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the variable speed Water to Air HP Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE Psychrometrics,       ONLY:  PsyRhoAirFnPbTdbW
  USE DataGlobals,          ONLY: SysSizingCalc
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,       ONLY : ScanPlantLoopsForObject, PlantLoop
  USE PlantUtilities,  ONLY : InitComponentNodes, SetComponentFlowRate
  USE General,         ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN) :: DXCoilNum                  ! Current DXCoilNum under simulation
  REAL(r64),    INTENT(IN) :: MaxONOFFCyclesperHour ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64),    INTENT(IN) :: HPTimeConstant        ! Heat pump time constant [s]
  REAL(r64),    INTENT(IN) :: FanDelayTime          ! Fan delay time, time delay for the HP's fan to
                                                    ! shut off after compressor cycle off  [s]
  REAL(r64), INTENT(IN) :: SensLoad                 ! Control zone sensible load[W]
  REAL(r64), INTENT(IN) :: LatentLoad               ! Control zone latent load[W]
  INTEGER,   INTENT(IN) :: CyclingScheme            ! fan operating mode
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio        ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: SpeedRatio               ! compressor speed ratio
  INTEGER, INTENT(IN)   :: SpeedNum                 ! compressor speed number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: AirInletNode        ! Node Number of the air inlet
  INTEGER                :: WaterInletNode      ! Node Number of the Water inlet
  LOGICAL, SAVE          :: MyOneTimeFlag = .TRUE.  ! one time allocation flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag ! used for initializations each begin environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySizeFlag  ! used for sizing PTHP inputs one time
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  REAL(r64)   :: rho                                    ! local fluid density
  REAL(r64)   :: Cp                                     ! local fluid specific heat
  INTEGER :: SpeedCal                                   ! calculated speed level
  LOGICAL     :: errFlag
  LOGICAL :: ErrorsFound=.FALSE.                        ! TRUE when errors found, air loop initialization error
  REAL(r64) :: RatedVolFlowPerRatedTotCap               ! Rated Air Volume Flow Rate divided by Rated Total Capacity [m3/s-W)
  INTEGER :: Mode                                       ! Performance mode for MultiMode DX coil; Always 1 for other coil types
  REAL(r64) :: RatedHeatPumpIndoorAirTemp
  ! Indoor dry-bulb temperature to heat pump evaporator at rated conditions [C]
  REAL(r64) :: RatedHeatPumpIndoorHumRat                ! Inlet humidity ratio to heat pump evaporator at rated conditions [kg/kg]
  REAL(r64) :: WaterFlowScale                           ! water flow scaling factor match rated flow rate

            ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64) :: SmallDifferenceTest=0.00000001d0
  CHARACTER(len=*), PARAMETER :: RoutineName='InitMulSpeedWSHPCoil'


  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MySizeFlag(NumWatertoAirHPs))
    ALLOCATE(MyEnvrnFlag(NumWatertoAirHPs))
    ALLOCATE(MyPlantScanFlag(NumWatertoAirHPs))
    MySizeFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyPlantScanFlag = .TRUE.
    MyOneTimeFlag = .FALSE.

  END IF

  IF (MyPlantScanFlag(DXCoilNum) .AND. ALLOCATED(PlantLoop)) THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(WtoADXCoil(DXCoilNum)%Name,  &
                                 WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum, &
                                 WtoADXCoil(DXCoilNum)%LoopNum, &
                                 WtoADXCoil(DXCoilNum)%LoopSide, &
                                 WtoADXCoil(DXCoilNum)%BranchNum, &
                                 WtoADXCoil(DXCoilNum)%CompNum,   &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitMulSpeedWSHPCoil: Program terminated for previous conditions.')
    ENDIF
    MyPlantScanFlag(DXCoilNum) = .FALSE.
  ENDIF


  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(DXCoilNum) .AND. .NOT. MyPlantScanFlag(DXCoilNum) ) THEN
    ! for each furnace, do the sizing once.
    CALL SizeMulSpeedWSHPCoil(DXCoilNum)

    MySizeFlag(DXCoilNum) = .FALSE.

    ! Multispeed Cooling
    IF (WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit ) THEN
      Do Mode = 1, WtoADXCoil(DXCoilNum)%NumOfSpeeds
        ! Check for zero capacity or zero max flow rate
        IF (WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode) <= 0.0) THEN
          CALL ShowSevereError('Sizing: '//TRIM(WtoADXCoil(DXCoilNum)%WtoADXCoilType)//' '//TRIM(WtoADXCoil(DXCoilNum)%Name)// &
                            ' has zero rated total capacity at speed '//Trim(TrimSigDigits(Mode)))
          ErrorsFound=.TRUE.
        END IF
        IF (WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode) <= 0.0) THEN
          CALL ShowSevereError('Sizing: '//TRIM(WtoADXCoil(DXCoilNum)%WtoADXCoilType)//' '//TRIM(WtoADXCoil(DXCoilNum)%Name)// &
                            ' has zero rated air flow rate at speed '//Trim(TrimSigDigits(Mode)))
          ErrorsFound=.TRUE.
        END IF
        IF (ErrorsFound) THEN
          CALL ShowFatalError('Preceding condition causes termination.')
        ENDIF
        !
        ! Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
        !
        RatedVolFlowPerRatedTotCap = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)/  &
                                                 WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode)
       !note: variable-speed HP can exceed the flow rate restrictions at low speed levels
!        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
!           ((RatedVolFlowPerRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
!          CALL ShowSevereError ('Sizing: '//TRIM(WtoADXCoil(DXCoilNum)%WtoADXCoilType) &
!           // ' "'//TRIM(WtoADXCoil(DXCoilNum)%Name)//  &
!                '": Rated air volume flow rate per watt of rated total '// &
!                'cooling capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
!          CALL ShowContinueError &
!           ('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
!           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
!           Max Rated Vol Flow Per Watt=['// &
!           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference Manual for valid range.')
!        END IF
!        WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
!          PsyRhoAirFnPbTdbW(OutBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
!        ! get high speed rated coil bypass factor
!        WtoADXCoil(DXCoilNum)%MSRatedCBF(Mode) = CalcCBF(WtoADXCoil(DXCoilNum)%WtoADXCoilType, &
!               WtoADXCoil(DXCoilNum)%Name,&
!                                           RatedInletAirTemp,RatedInletAirHumRat,WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode),&
!                                           WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode), &
!                           WtoADXCoil(DXCoilNum)%MSRatedSHR(Mode))
      END DO
    END IF

    ! Multispeed Heating
    IF (WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit) THEN
      RatedHeatPumpIndoorAirTemp = 21.11d0  ! 21.11C or 70F
      RatedHeatPumpIndoorHumRat = 0.00881d0 ! Humidity ratio corresponding to 70F dry bulb/60F wet bulb
      Do Mode = 1, WtoADXCoil(DXCoilNum)%NumOfSpeeds

        WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
         PsyRhoAirFnPbTdbW(OutBaroPress,RatedHeatPumpIndoorAirTemp,RatedHeatPumpIndoorHumRat,RoutineName)
        ! Check for valid range of (Rated Air Volume Flow Rate / Rated Total Capacity)
        !
        RatedVolFlowPerRatedTotCap = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)/  &
                                                  WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode)
        !note: variable-speed HP can exceed the flow rate restrictions at low speed levels
!        IF (((MinRatedAirVolFlowPerRatedTotCap - RatedVolFlowPerRatedTotCap) > SmallDifferenceTest).OR. &
!            ((RatedVolFlowperRatedTotCap - MaxRatedAirVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
!          CALL ShowSevereError ('Coil:Heating:DX:MultiSpeed '//TRIM(WtoADXCoil(DXCoilNum)%Name)//  &
!                              ': Rated air volume flow rate per watt of rated total '// &
!                'heating capacity is out of range at speed '//TRIM(TrimSigDigits(Mode)))
!          CALL ShowContinueError('Min Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits &
!           (MinRatedAirVolFlowPerRatedTotCap,3))//'], '// &
!           'Rated Vol Flow Per Watt=['//TRIM(TrimSigDigits(RatedVolFlowPerRatedTotCap,3))//'],  &
!               Max Rated Vol Flow Per Watt=['// &
!           TRIM(TrimSigDigits(MaxRatedAirVolFlowPerRatedTotCap,3))//']. See Input-Output Reference  &
!                Manual for valid range.')
!        END IF
      End Do
    END IF

  END IF

  IF(SpeedNum > WtoADXCoil(DXCoilNum)%NumOfSpeeds) THEN
      SpeedCal = WtoADXCoil(DXCoilNum)%NumOfSpeeds
  ELSE IF(SpeedNum < 1) THEN
     SpeedCal = 1
  ELSE
     SpeedCal = SpeedNum
  END IF

  IF((SpeedNum <= 1) .OR.(SpeedNum > WtoADXCoil(DXCoilNum)%NumOfSpeeds) ) THEN
    WtoADXCoil(DXCoilNum)%DesignAirMassFlowRate = WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal)
    WtoADXCoil(DXCoilNum)%DesignAirVolFlowRate = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(SpeedCal)
    WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate = WtoADXCoil(DXCoilNum)%MSRatedWaterMassFlowRate(SpeedCal)
    WtoADXCoil(DXCoilNum)%DesignWaterVolFlowRate = WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(SpeedCal)
  ELSE
    WtoADXCoil(DXCoilNum)%DesignAirMassFlowRate = WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal)*SpeedRatio &
            + (1.0 - SpeedRatio ) * WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal - 1)
    WtoADXCoil(DXCoilNum)%DesignAirVolFlowRate = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(SpeedCal)*SpeedRatio &
            + (1.0 - SpeedRatio ) * WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(SpeedCal - 1)
    WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate = WtoADXCoil(DXCoilNum)%MSRatedWaterMassFlowRate(SpeedCal)*SpeedRatio &
            + (1.0 - SpeedRatio ) * WtoADXCoil(DXCoilNum)%MSRatedWaterMassFlowRate(SpeedCal - 1)
    WtoADXCoil(DXCoilNum)%DesignWaterVolFlowRate = WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(SpeedCal)*SpeedRatio &
            + (1.0 - SpeedRatio ) * WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(SpeedCal - 1)
  END IF


  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(DXCoilNum) .AND. .NOT. MyPlantScanFlag(DXCoilNum)) THEN
      ! Do the initializations to start simulation

    AirInletNode   = WtoADXCoil(DXCoilNum)%AirInletNodeNum
    WaterInletNode = WtoADXCoil(DXCoilNum)%WaterInletNodeNum

    !Initialize all report variables to a known state at beginning of simulation
    WtoADXCoil(DXCoilNum)%AirVolFlowRate=0.0
    WtoADXCoil(DXCoilNum)%InletAirDBTemp=0.0
    WtoADXCoil(DXCoilNum)%InletAirHumRat=0.0
    WtoADXCoil(DXCoilNum)%OutletAirDBTemp=0.0
    WtoADXCoil(DXCoilNum)%OutletAirHumRat=0.0
    WtoADXCoil(DXCoilNum)%WaterVolFlowRate=0.0
    WtoADXCoil(DXCoilNum)%WaterMassFlowRate=0.0
    WtoADXCoil(DXCoilNum)%InletWaterTemp=0.0
    WtoADXCoil(DXCoilNum)%InletWaterEnthalpy = 0.0
    WtoADXCoil(DXCoilNum)%OutletWaterEnthalpy = 0.0
    WtoADXCoil(DXCoilNum)%OutletWaterTemp=0.0
    WtoADXCoil(DXCoilNum)%Power=0.0
    WtoADXCoil(DXCoilNum)%QLoadTotal=0.0
    WtoADXCoil(DXCoilNum)%QSensible=0.0
    WtoADXCoil(DXCoilNum)%QLatent=0.0
    WtoADXCoil(DXCoilNum)%QSource=0.0
    WtoADXCoil(DXCoilNum)%Energy=0.0
    WtoADXCoil(DXCoilNum)%EnergyLoadTotal=0.0
    WtoADXCoil(DXCoilNum)%EnergySensible=0.0
    WtoADXCoil(DXCoilNum)%EnergyLatent=0.0
    WtoADXCoil(DXCoilNum)%EnergySource=0.0
    WtoADXCoil(DXCoilNum)%COP=0.0
    WtoADXCoil(DXCoilNum)%RunFrac=0.0
    WtoADXCoil(DXCoilNum)%PartLoadRatio=0.0

    rho = GetDensityGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                              InitConvTemp, &
                              PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                              'InitSimpleWatertoAirHP')
    Cp  = GetSpecificHeatGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                              InitConvTemp, &
                              PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                              'InitSimpleWatertoAirHP')

!    WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate= &
!                             rho * WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate

    WtoADXCoil(DXCoilNum)%MaxONOFFCyclesperHour=MaxONOFFCyclesperHour
    WtoADXCoil(DXCoilNum)%HPTimeConstant=HPTimeConstant
    WtoADXCoil(DXCoilNum)%FanDelayTime=FanDelayTime

    CALL InitComponentNodes(0.d0, WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate, &
                                  WtoADXCoil(DXCoilNum)%WaterInletNodeNum,  &
                                  WtoADXCoil(DXCoilNum)%WaterOutletNodeNum , &
                                  WtoADXCoil(DXCoilNum)%LoopNum, &
                                  WtoADXCoil(DXCoilNum)%LoopSide, &
                                  WtoADXCoil(DXCoilNum)%BranchNum, &
                                  WtoADXCoil(DXCoilNum)%CompNum )

    Node(WaterInletNode)%Temp          = 5.0
    Node(WaterInletNode)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
    Node(WaterInletNode)%Quality       = 0.0
    Node(WaterInletNode)%Press         = 0.0
    Node(WaterInletNode)%HumRat        = 0.0

    Node(WtoADXCoil(DXCoilNum)%WaterOutletNodeNum)%Temp          = 5.0
    Node(WtoADXCoil(DXCoilNum)%WaterOutletNodeNum)%Enthalpy      = Cp* Node(WaterInletNode)%Temp
    Node(WtoADXCoil(DXCoilNum)%WaterOutletNodeNum)%Quality       = 0.0
    Node(WtoADXCoil(DXCoilNum)%WaterOutletNodeNum)%Press         = 0.0
    Node(WtoADXCoil(DXCoilNum)%WaterOutletNodeNum)%HumRat        = 0.0

    WtoADXCoil(DXCoilNum)%SimFlag = .TRUE.

    MyEnvrnFlag(DXCoilNum) = .FALSE.

  END IF  ! End If for the Begin Environment initializations

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(DXCoilNum)=.TRUE.
  ENDIF

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! First set the conditions for the air into the heat pump model

    ! Set water and air inlet nodes

    AirInletNode = WtoADXCoil(DXCoilNum)%AirInletNodeNum
    WaterInletNode = WtoADXCoil(DXCoilNum)%WaterInletNodeNum

  IF ((SensLoad .NE. 0.0 .OR. LatentLoad .NE. 0.0).AND.(Node(AirInletNode)%MassFlowRate > 0.0)) THEN

    WaterFlowScale = WtoADXCoil(DXCoilNum)%RatedWaterMassFlowRate/ &
    WtoADXCoil(DXCoilNum)%MSRatedWaterMassFlowRate(WtoADXCoil(DXCoilNum)%NormSpedLevel)
    WtoADXCoil(DXCoilNum)%WaterMassFlowRate =   WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate*WaterFlowScale

    IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
       WtoADXCoil(DXCoilNum)%AirMassFlowRate   = Node(AirInletNode)%MassFlowRate
!    WtoADXCoil(DXCoilNum)%AirMassFlowRate   = WtoADXCoil(DXCoilNum)%DesignAirVolFlowRate*  &
!             PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
        !If air flow is less than 25% rated flow. Then set air flow to the 25% of rated conditions
        IF(WtoADXCoil(DXCoilNum)%AirMassFlowRate.LT.  &
             0.25d0*WtoADXCoil(DXCoilNum)%DesignAirVolFlowRate*  &
                 PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)) THEN
            WtoADXCoil(DXCoilNum)%AirMassFlowRate =   &
              0.25d0*WtoADXCoil(DXCoilNum)%DesignAirVolFlowRate* &
                  PsyRhoAirFnPbTdbW(OutBaroPress,Node(AirInletNode)%Temp,Node(AirInletNode)%HumRat)
        END IF
    ELSE !CYCLIC FAN, NOT CORRECTION, WILL BE PROCESSED IN THE FOLLOWING SUBROUTINES
      WtoADXCoil(DXCoilNum)%AirMassFlowRate   = Node(AirInletNode)%MassFlowRate
    END IF

  ELSE !heat pump is off
    WtoADXCoil(DXCoilNum)%WaterMassFlowRate = 0.d0
    WtoADXCoil(DXCoilNum)%AirMassFlowRate   = 0.d0
  ENDIF

  CALL SetComponentFlowRate(WtoADXCoil(DXCoilNum)%WaterMassFlowRate, &
                                 WtoADXCoil(DXCoilNum)%WaterInletNodeNum , &
                                 WtoADXCoil(DXCoilNum)%WaterOutletNodeNum, &
                                 WtoADXCoil(DXCoilNum)%LoopNum, &
                                 WtoADXCoil(DXCoilNum)%LoopSide, &
                                 WtoADXCoil(DXCoilNum)%BranchNum, &
                                 WtoADXCoil(DXCoilNum)%CompNum )

   WtoADXCoil(DXCoilNum)%InletAirDBTemp       = Node(AirInletNode)%Temp
   WtoADXCoil(DXCoilNum)%InletAirHumRat       = Node(AirInletNode)%HumRat
   WtoADXCoil(DXCoilNum)%InletAirEnthalpy     = Node(AirInletNode)%Enthalpy
   WtoADXCoil(DXCoilNum)%InletWaterTemp       = Node(WaterInletNode)%Temp
   WtoADXCoil(DXCoilNum)%InletWaterEnthalpy   = Node(WaterInletNode)%Enthalpy

   WtoADXCoil(DXCoilNum)%MaxONOFFCyclesperHour= MaxONOFFCyclesperHour
   WtoADXCoil(DXCoilNum)%HPTimeConstant       = HPTimeConstant
   WtoADXCoil(DXCoilNum)%FanDelayTime         = FanDelayTime


   WtoADXCoil(DXCoilNum)%InletAirPressure    = OutBaroPress !temporary
   ! Outlet variables
   WtoADXCoil(DXCoilNum)%Power=0.0
   WtoADXCoil(DXCoilNum)%QLoadTotal=0.0
   WtoADXCoil(DXCoilNum)%QSensible=0.0
   WtoADXCoil(DXCoilNum)%QLatent=0.0
   WtoADXCoil(DXCoilNum)%QSource=0.0
   WtoADXCoil(DXCoilNum)%QWasteHeat = 0.0
   WtoADXCoil(DXCoilNum)%Energy=0.0
   WtoADXCoil(DXCoilNum)%EnergyLoadTotal=0.0
   WtoADXCoil(DXCoilNum)%EnergySensible=0.0
   WtoADXCoil(DXCoilNum)%EnergyLatent=0.0
   WtoADXCoil(DXCoilNum)%EnergySource=0.0
   WtoADXCoil(DXCoilNum)%COP=0.0

   WtoADXCoil(DXCoilNum)%OutletAirDBTemp=0.0
   WtoADXCoil(DXCoilNum)%OutletWaterTemp=0.0
   WtoADXCoil(DXCoilNum)%OutletAirHumRat=0.0
   WtoADXCoil(DXCoilNum)%OutletAirEnthalpy = 0.0
   WtoADXCoil(DXCoilNum)%OutletWaterEnthalpy = 0.0

  RETURN

END SUBROUTINE InitMulSpeedWSHPCoil


SUBROUTINE SizeMulSpeedWSHPCoil(DXCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:SizeHVACWaterToAir
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing WSHP Components for which nominal capacities
          ! and flow rates have not been specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Obtains heating capacities and flow rates from the zone or system sizing arrays.
          !
          ! NOTE: For WSHP's we are sizing the heating capacity to be
          ! equal to the cooling capacity.  Thus the cooling and
          ! and heating capacities of a DX heat pump system will be identical. In real life the ARI
          ! heating and cooling capacities are close but not identical.



          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics
  USE DataPlant,          ONLY: PlantLoop, MyPlantSizingIndex
  USE DataHVACGlobals,    ONLY: SmallAirVolFlow, SmallLoad
  USE General,            ONLY: RoundSigDigits, TrimSigDigits
  USE PlantUtilities,     ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataAirSystems,     ONLY: PrimaryAirSystem
  USE OutputReportPredefined
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: DXCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='SizeMulSpeedWSHPCoil'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: rhoair
  REAL(r64) :: CpAir
  REAL(r64) :: MixTemp
  REAL(r64) :: MixHumRat
  REAL(r64) :: MixEnth
  REAL(r64) :: MixWetBulb
  REAL(r64) :: SupTemp
  REAL(r64) :: SupHumRat
  REAL(r64) :: SupEnth
  REAL(r64) :: OutTemp
  REAL(r64) :: OutAirFrac
  REAL(r64) :: VolFlowRate
  REAL(r64) :: CoolCapAtPeak
  REAL(r64) :: TotCapTempModFac
  REAL(r64) :: SensCapAtPeak
  REAL(r64) :: SensCapTempModFac
  INTEGER   :: TimeStepNumAtMax
  INTEGER   :: DDNum
  INTEGER   :: PltSizNum
  LOGICAL   :: RatedCapCoolTotalAutosized
  LOGICAL   :: RatedCapCoolSensAutosized
  LOGICAL   :: ErrorsFound
  REAL(r64) :: SystemCapacity
  REAL(r64) :: rho
  REAL(r64) :: cp
  INTEGER   :: NormSpeed !norminal speed level
  INTEGER   :: UpperSpeed !highest speed level
  INTEGER   :: Mode !speed level
  REAL(r64) :: rhoW !water density
  REAL(r64) :: rhoA !air density
  REAL(r64) :: SHR  !sensible heat transfer ratio
  REAL(r64) :: RatedAirMassFlowRate  !rated air mass flow rate
  REAL(r64) :: CBFRated  !bypass factor at the rated condition, considering difference in flow rates
  REAL(r64) :: RatedInletEnth !rated inlet air enthalpy
  REAL(r64) :: QLoadTotal1 !placeholder for calculating SHR
  REAL(r64) :: QLoadTotal2 !placeholder for calculating SHR
  REAL(r64) :: QLoadTotal  !placeholder for calculating SHR
  REAL(r64) :: AirMassFlowRatio !air mass flow ratio
  REAL(r64) :: WaterMassFlowRatio !water mass flow rate

  UpperSpeed = WtoADXCoil(DXCoilNum)%NumOfSpeeds
  NormSpeed = WtoADXCoil(DXCoilNum)%NormSpedLevel
  PltSizNum = 0
  ErrorsFound = .FALSE.

  IF (WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate == AutoSize) THEN

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                          WtoADXCoil(DXCoilNum)%Name)
      IF (FinalSysSizing(CurSysNum)%DesMainVolFlow >= SmallAirVolFlow) THEN
        WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate = FinalSysSizing(CurSysNum)%DesMainVolFlow
      ELSE
        WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate = 0.0
      END IF

    END IF

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                          WtoADXCoil(DXCoilNum)%Name)
      WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                            FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate < SmallAirVolFlow) THEN
        WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate = 0.0
      END IF

    END IF

    CALL ReportSizingOutput('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                             WtoADXCoil(DXCoilNum)%Name, &
                            'Rated Air Flow Rate [m3/s]', &
                             WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate)

  END IF

  RatedCapCoolTotalAutosized = .FALSE.
  RatedCapCoolSensAutosized  = .FALSE.

! size rated total cooling capacity
  IF (WtoADXCoil(DXCoilNum)%RatedCapCoolTotal == AutoSize .AND. &
      WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'COOLING') THEN

    RatedCapCoolTotalAutosized = .TRUE.

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                          WtoADXCoil(DXCoilNum)%Name)
      VolFlowRate = WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF (CurOASysNum > 0) THEN ! coil is in the OA stream
          MixTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
          MixHumRat = FinalSysSizing(CurSysNum)%CoolOutHumRat
          SupTemp = FinalSysSizing(CurSysNum)%PrecoolTemp
          SupHumRat = FinalSysSizing(CurSysNum)%PrecoolHumRat
        ELSE ! coil is on the main air loop
          SupTemp = FinalSysSizing(CurSysNum)%CoolSupTemp
          SupHumRat = FinalSysSizing(CurSysNum)%CoolSupHumRat
          IF (PrimaryAirSystem(CurSysNum)%NumOACoolCoils == 0) THEN ! there is no precooling of the OA stream
            MixTemp = FinalSysSizing(CurSysNum)%CoolMixTemp
            MixHumRat = FinalSysSizing(CurSysNum)%CoolMixHumRat
          ELSE ! there is precooling of OA stream
            IF (VolFlowRate > 0.0) THEN
              OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / VolFlowRate
            ELSE
              OutAirFrac = 1.0d0
            END IF
            OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
            MixTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolTemp + &
                        (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetTemp
            MixHumRat = OutAirFrac*FinalSysSizing(CurSysNum)%PrecoolHumRat + &
                          (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%CoolRetHumRat
          END IF
        END IF
        OutTemp = FinalSysSizing(CurSysNum)%CoolOutTemp
        rhoair = PsyRhoAirFnPbTdbW(OutBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,OutBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)

        TotCapTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(WtoADXCoil(DXCoilNum)%NormSpedLevel), &
                    MixWetBulb,RatedInletWaterTemp)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixEnth .GT. SupEnth)THEN
          CoolCapAtPeak = rhoair * VolFlowRate * (MixEnth-SupEnth)
        ELSE
          CoolCapAtPeak = rhoair * VolFlowRate * (48000.0d0-SupEnth)
        END IF
        CoolCapAtPeak = MAX(0.0d0, CoolCapAtPeak)
        IF(TotCapTempModFac .GT. 0.0d0)THEN
          WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = CoolCapAtPeak / TotCapTempModFac
        ELSE
          WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = CoolCapAtPeak
        END IF
      ELSE
        WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = 0.0
      END IF

    ELSE IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                          WtoADXCoil(DXCoilNum)%Name)
      VolFlowRate = WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate
      IF (VolFlowRate >= SmallAirVolFlow) THEN
        IF(ZoneEqDXCoil)THEN
          IF (ZoneEqSizing(CurZoneEqNum)%OAVolFlow > 0.0) THEN
            MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
          ELSE
            MixTemp = FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtCoolPeak
            MixHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtCoolPeak
          END IF
        ELSE
          MixTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
          MixHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
        END IF
        SupTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
        SupHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
        TimeStepNumAtMax = FinalZoneSizing(CurZoneEqNum)%TimeStepNumAtCoolMax
        DDNum = FinalZoneSizing(CurZoneEqNum)%CoolDDNum
        IF (DDNum > 0 .and. TimeStepNumAtMax > 0) THEN
          OutTemp = DesDayWeath(DDNum)%Temp(TimeStepNumAtMax)
        ELSE
          OutTemp = 0.0
        ENDIF
        rhoair = PsyRhoAirFnPbTdbW(OutBaroPress,MixTemp,MixHumRat,RoutineName)
        MixEnth = PsyHFnTdbW(MixTemp,MixHumRat,RoutineName)
        MixWetBulb = PsyTwbFnTdbWPb(MixTemp,MixHumRat,OutBaroPress,RoutineName)
        SupEnth = PsyHFnTdbW(SupTemp,SupHumRat,RoutineName)

        TotCapTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(WtoADXCoil(DXCoilNum)%NormSpedLevel), &
                MixWetBulb,RatedInletWaterTemp)
!       The mixed air temp for zone equipment without an OA mixer is 0.
!       This test avoids a negative capacity until a solution can be found.
        IF(MixEnth .GT. SupEnth)THEN
          CoolCapAtPeak = rhoair * VolFlowRate * (MixEnth-SupEnth)
        ELSE
          CoolCapAtPeak = rhoair * VolFlowRate * (48000.0d0-SupEnth)
        END IF
        CoolCapAtPeak = MAX(0.0d0, CoolCapAtPeak)
        IF(TotCapTempModFac .GT. 0.0d0)THEN
          WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = CoolCapAtPeak / TotCapTempModFac
        ELSE
          WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = CoolCapAtPeak
        END IF
      ELSE
        WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = 0.0
      END IF

    END IF

    IF (WtoADXCoil(DXCoilNum)%RatedCapCoolTotal < SmallLoad) THEN
      WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = 0.0
    END IF

    CALL ReportSizingOutput('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                             WtoADXCoil(DXCoilNum)%Name, &
                            'Rated Total Cooling Capacity [W]', &
                             WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
    CALL PreDefTableEntry(pdchCoolCoilTotCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
    CALL PreDefTableEntry(pdchCoolCoilLatCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolTotal &
                                 - WtoADXCoil(DXCoilNum)%RatedCapCoolSens)
    IF (WtoADXCoil(DXCoilNum)%RatedCapCoolTotal /= 0.0d0) THEN
      CALL PreDefTableEntry(pdchCoolCoilSHR,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolSens &
                                   / WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
      CALL PreDefTableEntry(pdchCoolCoilNomEff,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedPowerCool &
                                   / WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
    ELSE
      CALL PreDefTableEntry(pdchCoolCoilSHR,WtoADXCoil(DXCoilNum)%Name,0.0d0)
      CALL PreDefTableEntry(pdchCoolCoilNomEff,WtoADXCoil(DXCoilNum)%Name,0.0d0)
    ENDIF

  END IF

! Set the global DX cooling coil capacity variable for use by other objects
  IF (WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'COOLING') THEN
    DXCoolCap = WtoADXCoil(DXCoilNum)%RatedCapCoolTotal
  END IF

! size rated heating capacity
  IF (WtoADXCoil(DXCoilNum)%RatedCapHeat == AutoSize .AND. &
      WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'HEATING') THEN

!   simply set heating capacity equal to the cooling capacity
    !WtoADXCoil(DXCoilNum)%RatedCapHeat = DXCoolCap
    IF(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum > 0) THEN
        WtoADXCoil(DXCoilNum)%RatedCapHeat = WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal
        WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = WtoADXCoil(DXCoilNum)%RatedCapHeat !AVOID BEING ZERO
    ELSE
        WtoADXCoil(DXCoilNum)%RatedCapHeat = DXCoolCap !previous code, can be risky
    END IF

    IF(WtoADXCoil(DXCoilNum)%RatedCapHeat == Autosize)THEN
      CALL ShowWarningError('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType) &
                    //':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
                            TRIM(WtoADXCoil(DXCoilNum)%Name)//'"')
      CALL ShowContinueError(RoutineName//': Heating coil could not be autosized since cooling coil was not previously sized.')
      CALL ShowContinueError('... Cooling coil must be upstream of heating coil.')
      CALL ShowContinueError('... Manually sizing this heating coil will be required.')
    END IF

    IF (WtoADXCoil(DXCoilNum)%RatedCapHeat < SmallLoad) THEN
      WtoADXCoil(DXCoilNum)%RatedCapHeat = 0.0
    END IF

    CALL ReportSizingOutput('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                             WtoADXCoil(DXCoilNum)%Name, &
                            'Nominal Heating Capacity [W]', &
                             WtoADXCoil(DXCoilNum)%RatedCapHeat)
    CALL PreDefTableEntry(pdchHeatCoilNomCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapHeat)
    IF (WtoADXCoil(DXCoilNum)%RatedCapHeat /= 0.0d0) THEN
      CALL PreDefTableEntry(pdchHeatCoilNomEff,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedPowerHeat &
                             / WtoADXCoil(DXCoilNum)%RatedCapHeat)
    ELSE
      CALL PreDefTableEntry(pdchHeatCoilNomEff,WtoADXCoil(DXCoilNum)%Name,0.0d0)
    ENDIF
  END IF


  ! Check that heat pump heating capacity is within 20% of cooling capacity. Check only for heating coil and report both.
  IF (WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'HEATING' .AND. &
      WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum .GT. 0)THEN

    IF(WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal .GT. 0.0D0)THEN

      IF(ABS(WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal-&
           WtoADXCoil(DXCoilNum)%RatedCapHeat)/&
           WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal .GT. 0.2d0) THEN

        CALL ShowWarningError('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)// &
                            ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'//TRIM(WtoADXCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError('...used with COIL:'// &
               TRIM(WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%WaterToAirHPType)// &
                            ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
               TRIM(WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%Name)//'"')
        CALL ShowContinueError('...heating capacity is disproportionate (> 20% different) to total cooling capacity')
        CALL ShowContinueError('...heating capacity = '//TRIM(TrimSigDigits(WtoADXCoil(DXCoilNum)%RatedCapHeat,3))//' W')
        CALL ShowContinueError('...cooling capacity = '// &
           TRIM(TrimSigDigits(WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapCoolTotal,3))//' W')

      END IF

    END IF

  END IF


! size rated power
  IF (WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'COOLING') THEN

    WtoADXCoil(DXCoilNum)%RatedCOPCool = WtoADXCoil(DXCoilNum)%MSRatedCOP(WtoADXCoil(DXCoilNum)%NormSpedLevel)
    WtoADXCoil(DXCoilNum)%RatedPowerCool = WtoADXCoil(DXCoilNum)%RatedCapCoolTotal/WtoADXCoil(DXCoilNum)%RatedCOPCool

  ELSE IF (WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'HEATING') THEN
    WtoADXCoil(DXCoilNum)%RatedCOPHeat = WtoADXCoil(DXCoilNum)%MSRatedCOP(WtoADXCoil(DXCoilNum)%NormSpedLevel)
    WtoADXCoil(DXCoilNum)%RatedPowerHeat = WtoADXCoil(DXCoilNum)%RatedCapHeat/WtoADXCoil(DXCoilNum)%RatedCOPHeat
    WtoADXCoil(DXCoilNum)%RatedCapCoolTotal = WtoADXCoil(DXCoilNum)%RatedCapHeat
  END IF

! Size water volumetric flow rate
  IF (WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate == AutoSize)THEN

!!   if not found on a plant loop, check condenser loop and warn user if not found
!    IF(PltSizNum == 0) THEN
!
!      PltSizNum = &
!          MyCondPlantSizingIndex('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)// &
!               ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
!                                 WtoADXCoil(DXCoilNum)%Name, &
!                                 WtoADXCoil(DXCoilNum)%WaterInletNodeNum, &
!                                 WtoADXCoil(DXCoilNum)%WaterOutletNodeNum, ErrorsFound)
!    END IF

  !   WSHP condenser can be on either a plant loop or condenser loop. Test each to find plant sizing number.
!   first check to see if coil is connected to a plant loop, no warning on this CALL
   PltSizNum = &
        MyPlantSizingIndex('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                           WtoADXCoil(DXCoilNum)%Name, &
                           WtoADXCoil(DXCoilNum)%WaterInletNodeNum, &
                           WtoADXCoil(DXCoilNum)%WaterOutletNodeNum, ErrorsFound, .FALSE.)

    IF (PltSizNum > 0) THEN
      rho = GetDensityGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                             PlantSizData(PltSizNum)%ExitTemp, &
                             PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                             'SizeHVACWaterToAir')
      Cp  = GetSpecificHeatGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                                  PlantSizData(PltSizNum)%ExitTemp, &
                                  PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                 'SizeHVACWaterToAir')

      IF (WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'HEATING') THEN

        WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate = WtoADXCoil(DXCoilNum)%RatedCapHeat / &
                                                       ( PlantSizData(PltSizNum)%DeltaT * Cp * rho )

        CALL ReportSizingOutput('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//&
                                ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                                  WtoADXCoil(DXCoilNum)%Name, &
                                  'Rated Water Flow Rate [m3/s]', WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate)

      ELSEIF(WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'COOLING') THEN

!       use companion heating coil capacity to calculate volumetric flow rate
        IF(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum .GT. 0)THEN
          SystemCapacity = WtoADXCoil(WtoADXCoil(DXCoilNum)%CompanionCoolingCoilNum)%RatedCapHeat
        ELSE
          SystemCapacity = WtoADXCoil(DXCoilNum)%RatedCapCoolTotal
        END IF

          WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate = &
                      SystemCapacity / &
                    ( PlantSizData(PltSizNum)%DeltaT * Cp * rho )

        CALL ReportSizingOutput('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)&
                                //':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                                  WtoADXCoil(DXCoilNum)%Name, &
                                  'Rated Water Flow Rate [m3/s]', WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate)

      END IF

    ELSE
        CALL ShowContinueError('Autosizing of water flow requires a loop Sizing:Plant object')
        CALL ShowContinueError('Autosizing also requires physical connection to a plant or condenser loop.')
        CALL ShowContinueError('Occurs in ' // &
                 'COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//&
                        ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' // ' Object=' &
                //TRIM(WtoADXCoil(DXCoilNum)%Name))
        ErrorsFound = .TRUE.

    END IF

  END IF

! Save component design water volumetric flow rate.
! Use 1/2 flow since both cooling and heating coil will save flow yet only 1 will operate at a time
  IF(WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate .GT. 0.0)THEN
    CALL RegisterPlantCompDesignFlow(WtoADXCoil(DXCoilNum)%WaterInletNodeNum,  &
       0.5d0*WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate)
  END IF

  IF (WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit .OR. &
  WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit ) THEN

   IF (PltSizNum > 0) THEN
     rhoW = rho
   ELSE IF(WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit) THEN
     rhoW = GetDensityGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                                 RatedInletWaterTemp, &
                                 PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                 'SizeMulSpeedWSHPCoil')
   ELSE
     rhoW = GetDensityGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                                 RatedInletWaterTempHeat, &
                                 PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex, &
                                 'SizeMulSpeedWSHPCoil')
   END IF

   IF (WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit) THEN
    WtoADXCoil(DXCoilNum)%MSRatedTotCap(UpperSpeed) = WtoADXCoil(DXCoilNum)%RatedCapCoolTotal/ &
            WtoADXCoil(DXCoilNum)%MSRatedPercentTotCap(NormSpeed)
   ELSE IF (WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPHeatingEquationFit) THEN
    WtoADXCoil(DXCoilNum)%MSRatedTotCap(UpperSpeed) = WtoADXCoil(DXCoilNum)%RatedCapHeat/ &
            WtoADXCoil(DXCoilNum)%MSRatedPercentTotCap(NormSpeed)
   END IF

   rhoA = PsyRhoAirFnPbTdbW(OutBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
   WtoADXCoil(DXCoilNum)%RatedWaterMassFlowRate = WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate * rhoW
   Do Mode = WtoADXCoil(DXCoilNum)%NumOfSpeeds,1,-1
     WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode) = WtoADXCoil(DXCoilNum)%MSRatedTotCap(UpperSpeed) * &
                WtoADXCoil(DXCoilNum)%MSRatedPercentTotCap(Mode)
     WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode) = WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode) * &
                WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowPerRatedTotCap(Mode)
     WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode) = WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode) * &
                WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowPerRatedTotCap(Mode)
     WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode) = WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode)* &
                rhoA
     WtoADXCoil(DXCoilNum)%MSRatedWaterMassFlowRate(Mode) =&
            WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode) * rhoW
   END DO

    ! Ensure air flow rate at lower speed must be lower or
    ! equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
    Do Mode = 1,WtoADXCoil(DXCoilNum)%NumOfSpeeds-1
      If (WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode) .GT. WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode+1)) Then
        CALL ShowWarningError('SizeDXCoil: '//TRIM(WtoADXCoil(DXCoilNum)%WtoADXCoilType) &
                //' '//TRIM(WtoADXCoil(DXCoilNum)%Name)//', '// &
          'Speed '//Trim(TrimSigDigits(Mode))//' Rated Air Flow Rate must be less than or equal to '//&
          'Speed '//Trim(TrimSigDigits(Mode+1))//' Rated Air Flow Rate.')
        CALL ShowContinueError('Instead, '//TRIM(RoundSigDigits(WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode),2))//' > '//  &
                  TRIM(RoundSigDigits(WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode+1),2)))
        CALL ShowFatalError('Preceding conditions cause termination.')
      End If
    End Do

    ! Ensure water flow rate at lower speed must be lower or
    ! equal to the flow rate at higher speed. Otherwise, a severe error is isssued.
    Do Mode = 1,WtoADXCoil(DXCoilNum)%NumOfSpeeds-1
      If (WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode) .GT. &
      WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(Mode+1) * 1.05) Then
        CALL ShowWarningError('SizeDXCoil: '//TRIM(WtoADXCoil(DXCoilNum)%WtoADXCoilType) &
                //' '//TRIM(WtoADXCoil(DXCoilNum)%Name)//', '// &
          'Speed '//Trim(TrimSigDigits(Mode))//' Rated Air Flow Rate must be less than or equal to '//&
          'Speed '//Trim(TrimSigDigits(Mode+1))//' Rated Air Flow Rate.')
        CALL ShowContinueError('Instead, '//TRIM(RoundSigDigits(WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode),2))//' > '//  &
                  TRIM(RoundSigDigits(WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(Mode+1),2)))
        CALL ShowFatalError('Preceding conditions cause termination.')
      End If
    End Do

   ! Ensure capacity at lower speed must be lower or equal to the capacity at higher speed.
    Do Mode = 1,WtoADXCoil(DXCoilNum)%NumOfSpeeds-1
      If (WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode) .GT. WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode+1)) Then
        CALL ShowWarningError('SizeDXCoil: '//TRIM(WtoADXCoil(DXCoilNum)%WtoADXCoilType)&
                //' '//TRIM(WtoADXCoil(DXCoilNum)%Name)//', '// &
          'Speed '//Trim(TrimSigDigits(Mode))//' Rated Total Cooling Capacity must be less than or equal to '//&
          'Speed '//Trim(TrimSigDigits(Mode+1))//' Rated Total Cooling Capacity.')
        CALL ShowContinueError('Instead, '//TRIM(RoundSigDigits(WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode),2))//' > '//  &
                  TRIM(RoundSigDigits(WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode+1),2)))
        CALL ShowFatalError('Preceding conditions cause termination.')
      End If
    End Do
  END IF

  !convert SHR to rated Bypass factor and effective air side surface area
  IF (WtoADXCoil(DXCoilNum)%WAHPPlantTypeOfNum==TypeOf_CoilVSWAHPCoolingEquationFit) THEN
    Do Mode = 1,WtoADXCoil(DXCoilNum)%NumOfSpeeds
        WtoADXCoil(DXCoilNum)%MSRatedCBF(Mode) = &
        CalcCBF(WtoADXCoil(DXCoilNum)%WtoADXCoilType,WtoADXCoil(DXCoilNum)%Name,&
                                           RatedInletAirTemp,RatedInletAirHumRat,WtoADXCoil(DXCoilNum)%MSRatedTotCap(Mode),&
                                           WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode),&
                                           WtoADXCoil(DXCoilNum)%MSRatedSHR(Mode))
        IF ( WtoADXCoil(DXCoilNum)%MSRatedCBF(Mode) .gt. 0.0) THEN
            WtoADXCoil(DXCoilNum)%MSEffectiveAo(Mode) = -log( WtoADXCoil(DXCoilNum)%MSRatedCBF(Mode))* &
            WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(Mode)
        ELSE
            WtoADXCoil(DXCoilNum)%MSEffectiveAo(Mode) = 0.
        END IF
    End Do
  END IF

! size rated sensible cooling capacity
 RatedCapCoolSensAutosized  = .TRUE.  !always do that

 IF (WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate >= SmallAirVolFloW .AND. WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'COOLING') THEN
     RatedAirMassFlowRate = WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate* &
           PsyRhoAirFnPbTdbW(StdBaroPress,RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
     RatedInletEnth = PsyHFnTdbW(RatedInletAirTemp,RatedInletAirHumRat,RoutineName)
     CBFRated  = AdjustCBF(WtoADXCoil(DXCoilNum)%MSRatedCBF(NormSpeed),WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(NormSpeed), &
                RatedAirMassFlowRate)
     IF(CBFRated > 0.999) CBFRated = 0.999
     AirMassFlowRatio = WtoADXCoil(DXCoilNum)%RatedAirVolFlowRate/ WtoADXCoil(DXCoilNum)%MSRatedAirVolFlowRate(NormSpeed)
     WaterMassFlowRatio = WtoADXCoil(DXCoilNum)%RatedWaterVolFlowRate/WtoADXCoil(DXCoilNum)%MSRatedWaterVolFlowRate(NormSpeed)

     CALL CalcTotCapSHR_VSWSHP(RatedInletAirTemp,RatedInletAirHumRat,RatedInletEnth,RatedInletWetbulbTemp, &
                     AirMassFlowRatio, WaterMassFlowRatio, &
                     RatedAirMassFlowRate,CBFRated, &
                     WtoADXCoil(DXCoilNum)%MSRatedTotCap(NormSpeed),WtoADXCoil(DXCoilNum)%MSCCapFTemp(NormSpeed), &
                     WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(NormSpeed), WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(NormSpeed),&
                     0.0d0,0,0, 0,&
                     QLoadTotal1, QLoadTotal2, QLoadTotal,SHR,RatedInletWaterTemp, &
                     StdBaroPress, 0.0d0, 1)

     WtoADXCoil(DXCoilNum)%RatedCapCoolSens = WtoADXCoil(DXCoilNum)%RatedCapCoolTotal * SHR
 ELSE
     WtoADXCoil(DXCoilNum)%RatedCapCoolSens = 0.0
 END IF

 IF (WtoADXCoil(DXCoilNum)%RatedCapCoolSens < SmallLoad) THEN
    WtoADXCoil(DXCoilNum)%RatedCapCoolSens = 0.0
 END IF

 IF (WtoADXCoil(DXCoilNum)%WaterToAirHPType == 'COOLING') THEN !always report for cooling mode
    CALL ReportSizingOutput('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)//':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT', &
                             WtoADXCoil(DXCoilNum)%Name, &
                            'Rated Sensible Cooling Capacity [W]', &
                             WtoADXCoil(DXCoilNum)%RatedCapCoolSens)
    CALL PreDefTableEntry(pdchCoolCoilSensCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolSens)
    CALL PreDefTableEntry(pdchCoolCoilLatCap,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolTotal &
                                 - WtoADXCoil(DXCoilNum)%RatedCapCoolSens)
    IF (WtoADXCoil(DXCoilNum)%RatedCapCoolTotal /= 0.0d0) THEN
      CALL PreDefTableEntry(pdchCoolCoilSHR,WtoADXCoil(DXCoilNum)%Name,WtoADXCoil(DXCoilNum)%RatedCapCoolSens &
                                 / WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)
    ELSE
      CALL PreDefTableEntry(pdchCoolCoilSHR,WtoADXCoil(DXCoilNum)%Name,0.0d0)
    ENDIF

  END IF

! test autosized sensible and total cooling capacity for total > sensible
  IF(RatedCapCoolSensAutosized .AND. RatedCapCoolTotalAutosized .OR. &
     RatedCapCoolSensAutosized)THEN
    IF(WtoADXCoil(DXCoilNum)%RatedCapCoolSens .GT. WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)THEN
        CALL ShowWarningError('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)// &
                ':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
                               TRIM(WtoADXCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Rated Sensible Cooling Capacity > Rated Total Cooling Capacity')
        CALL ShowContinueError('Each of these capacity inputs have been autosized.')
        CALL ShowContinueError('Rated Sensible Cooling Capacity = '// &
                                TRIM(TrimSigDigits(WtoADXCoil(DXCoilNum)%RatedCapCoolSens,2))//' W')
        CALL ShowContinueError('Rated Total Cooling Capacity    = '// &
                                TRIM(TrimSigDigits(WtoADXCoil(DXCoilNum)%RatedCapCoolTotal,2))//' W')
        CALL ShowContinueError('See eio file for further details.')
        CALL ShowContinueError('Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.')
        CALL ShowContinueError('Check Zone and System Sizing objects to verify sizing inputs.')
        CALL ShowContinueError('Sizing statistics:')
        CALL ShowContinueError('Entering Air Dry-Bulb Temperature = '//TRIM(TrimSigDigits(MixTemp,3))//' C')
        CALL ShowContinueError('Entering Air Wet-Bulb Temperature = '//TRIM(TrimSigDigits(MixWetBulb,3))//' C')
        CALL ShowContinueError('Entering Condenser Water Temperature used = 24.4444 C')
        CALL ShowContinueError('Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)')
        CALL ShowContinueError('ratioTDB = '//TRIM(TrimSigDigits(((MixTemp+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTWB = '//TRIM(TrimSigDigits(((MixWetBulb+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTS  = '//TRIM(TrimSigDigits(((85.0d0+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('Sensible Cooling Capacity Modifier = '//TRIM(TrimSigDigits(SensCapTempModFac,5)))
        CALL ShowContinueError('...Rated Sensible Cooling Capacity = Sensible Design Load / Sensible Cooling Capacity Modifier')
        CALL ShowContinueError('Total Cooling Capacity Modifier = '//TRIM(TrimSigDigits(TotCapTempModFac,5)))
        CALL ShowContinueError('...Rated Total Cooling Capacity = Total Design Load / Total Cooling Capacity Modifier')
        CALL ShowContinueError('Carefully review the Load Side Total, Sensible, and Latent heat transfer rates')
        CALL ShowContinueError('... to ensure they meet the expected manufacturers performance specifications.')
    END IF
  ELSE If(RatedCapCoolTotalAutosized)THEN
    IF(WtoADXCoil(DXCoilNum)%RatedCapCoolSens .GT. WtoADXCoil(DXCoilNum)%RatedCapCoolTotal)THEN
        CALL ShowWarningError('COIL:'//TRIM(WtoADXCoil(DXCoilNum)%WaterToAirHPType)&
                        //':WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT "'// &
                               TRIM(WtoADXCoil(DXCoilNum)%Name)//'"')
        CALL ShowContinueError(RoutineName//': Rated Sensible Cooling Capacity > Rated Total Cooling Capacity')
        CALL ShowContinueError('Only the rated total capacity input is autosized, consider autosizing both inputs.')
        CALL ShowContinueError('Rated Sensible Cooling Capacity = '// &
                                TRIM(TrimSigDigits(WtoADXCoil(DXCoilNum)%RatedCapCoolSens,2))//' W')
        CALL ShowContinueError('Rated Total Cooling Capacity    = '// &
                                TRIM(TrimSigDigits(WtoADXCoil(DXCoilNum)%RatedCapCoolTotal,2))//' W')
        CALL ShowContinueError('See eio file for further details.')
        CALL ShowContinueError('Check Total and Sensible Cooling Capacity Coefficients to ensure they are accurate.')
        CALL ShowContinueError('Check Zone and System Sizing objects to verify sizing inputs.')
        CALL ShowContinueError('Sizing statistics for Total Cooling Capacity:')
        CALL ShowContinueError('Entering Air Wet-Bulb Temperature = '//TRIM(TrimSigDigits(MixWetBulb,3))//' C')
        CALL ShowContinueError('Entering Condenser Water Temperature used = 24.4444 C')
        CALL ShowContinueError('Used design air and water flow rates (i.e., used 1 for ratioVL and ratioVS)')
        CALL ShowContinueError('ratioTWB = '//TRIM(TrimSigDigits(((MixWetBulb+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('ratioTS  = '//TRIM(TrimSigDigits(((85.0d0+283.15d0)/273.15d0),3)))
        CALL ShowContinueError('Sensible Cooling Capacity Modifier = '//TRIM(TrimSigDigits(SensCapTempModFac,5)))
        CALL ShowContinueError('...Rated Sensible Cooling Capacity = Sensible Design Load / Sensible Cooling Capacity Modifier')
        CALL ShowContinueError('Carefully review the Load Side Total, Sensible, and Latent heat transfer rates')
        CALL ShowContinueError('... to ensure they meet the expected manufacturers performance specifications.')
    END IF
  END IF

  RETURN

END SUBROUTINE SizeMulSpeedWSHPCoil


SUBROUTINE  CalcMulSpeedWSHPCoilCooling(DXCoilNum,CyclingScheme, &
            RuntimeFrac,SensDemand,LatentDemand,CompOp,PartLoadRatio,OnOffAirFlowRatio, &
            SpeedRatio, SpeedNum)


          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:CalcHPCoolingSimple
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for simulating the cooling mode of the Variable-Speed Water to Air HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Simulate the heat pump performance using the coefficients and rated conditions, interpolating between speed levels
          !
          ! If the LatDegradModelSimFlag is enabled, the coil will be simulated twice:
          ! (1)first simulation at the rated conditions (2) second simulation at the
          ! actual operating conditions. Then call CalcEffectiveSHR and the effective SHR
          ! is adjusted.
          !
          ! If the LatDegradModelSimFlag is disabled, the cooling coil is only simulated
          ! once at the actual operating conditions.
          !
          ! Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
          ! and RuntimeFrac.

          ! REFERENCES:
          ! n/a

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals,      ONLY: TimeStepSys, DXElecCoolingPower
  USE Psychrometrics,       ONLY: PsyWFnTdbTwbPb,PsyCpAirFnWTdb,PsyHFnTdbW,PsyRhoAirFnPbTdbW,  &
                                  PsyTwbFnTdbWPb,PsyTdbFnHW,PsyWFnTdbH
  USE FluidProperties,      ONLY: GetSpecificHeatGlycol
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER,   INTENT(IN) :: DXCoilNum              ! Heat Pump Number
  INTEGER,   INTENT(IN) :: CyclingScheme      ! Fan/Compressor cycling scheme indicator
  REAL(r64), INTENT(IN) :: SensDemand         ! Cooling Sensible Demand [W] !unused1208
  REAL(r64), INTENT(IN) :: LatentDemand       ! Cooling Latent Demand [W]
  INTEGER,   INTENT(IN) :: CompOp             ! compressor operation flag
  REAL(r64), INTENT(IN) :: PartLoadRatio      ! compressor part load ratio
  REAL(r64), INTENT(INOUT) :: RuntimeFrac     ! Runtime Fraction of compressor or percent on time (on-time/cycle time)
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio  ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: SpeedRatio         ! SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
  INTEGER, INTENT(IN)  :: SpeedNum            ! Speed number, high bound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Tref=283.15d0      ! Reference Temperature for performance curves,10C [K]
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcMultiSpeedWtoADXCoilCooling'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: Twet_rated             ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated            ! Gamma at rated conditions (coil air flow rate and air temperatures)

  REAL(r64) :: SHRss                  ! Sensible heat ratio at steady state
  REAL(r64) :: SHReff                 ! Effective sensible heat ratio at part-load condition
  REAL(r64) :: CpWater                ! Specific heat of water [J/kg_C]
  REAL(r64) :: CpAir                  ! Specific heat of air [J/kg_C]
  REAL(r64) :: ReportingConstant

  LOGICAL :: LatDegradModelSimFlag  ! Latent degradation model simulation flag
  INTEGER :: NumIteration           ! Iteration Counter
  INTEGER, SAVE :: Count=0          ! No idea what this is for.
  LOGICAL, SAVE    :: FirstTime = .true.
  REAL(r64), SAVE  :: LoadSideInletDBTemp_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletWBTemp_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletHumRat_Init ! rated conditions
  REAL(r64), SAVE  :: LoadSideInletEnth_Init ! rated conditions
  REAL(r64), SAVE  :: CpAir_Init                ! rated conditions
  REAL(r64)        :: LoadSideInletDBTemp_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletWBTemp_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletHumRat_Unit ! calc conditions for unit
  REAL(r64)        :: LoadSideInletEnth_Unit ! calc conditions for unit
  REAL(r64)        :: CpAir_Unit                ! calc conditions for unit
  REAL(r64) :: AirMassFlowRatio   ! airflow ratio at low speed
  REAL(r64) :: WaterMassFlowRatio   ! airflow ratio at high speed
  REAL(r64) :: TotCapAirFFModFac !air flow fraction modification
  REAL(r64) :: TotCapWaterFFModFac !water flow fraction modification
  REAL(r64) :: TotCapTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: EIRAirFFModFac !air flow fraction modification
  REAL(r64) :: EIRWaterFFModFac !water flow fraction modification
  REAL(r64) :: EIRTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: CBFSpeed !total capacity temperature correctio fraction
  REAL(r64) :: SHR !total capacity temperature correctio fraction
  REAL(r64) :: EIR !total capacity temperature correctio fraction
  INTEGER :: MaxSpeed           ! maximum speed level
  INTEGER :: SpeedCal           ! calculated speed level
  REAL(r64) :: AoEff !effective air side surface area
  REAL(r64) :: QLoadTotal1 !total capacity at low speed
  REAL(r64) :: QLoadTotal2 !total capacity at high speed
  REAL(r64) :: Winput1 !power consumption at low speed
  REAL(r64) :: Winput2 !power consumption at high speed
  REAL(r64) :: QWasteHeat !recoverable waste heat
  REAL(r64) :: QWasteHeat1 !recoverable waste heat at low speed
  REAL(r64) :: QWasteHeat2 !recoverable waste heat at high speed
  REAL(r64) :: PLF  !part-load function
  REAL(r64) :: MaxHumRat  !max possible humidity
  REAL(r64) :: MaxOutletEnth !max possible outlet enthalpy

  IF (FirstTime) THEN
    !Set indoor air conditions to the rated condition
    LoadSideInletDBTemp_Init = 26.7d0
    LoadSideInletHumRat_Init = 0.0111d0
    LoadSideInletEnth_Init = PsyHFnTdbW(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init,RoutineName//':Init')
    CpAir_Init = PsyCpAirFnWTdb(LoadSideInletHumRat_Init,LoadSideInletDBTemp_Init,RoutineName//':Init')
    FirstTime=.false.
  ENDIF
  LoadSideInletWBTemp_Init = PsyTwbFnTdbWPb(LoadSideInletDBTemp_Init,LoadSideInletHumRat_Init,OutBaroPress,RoutineName)

  MaxSpeed = WtoADXCoil(DXCoilNum)%NumofSpeeds


 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  IF (.NOT. (CyclingScheme .EQ. ContFanCycCoil) .AND. PartLoadRatio > 0.0) THEN
     WtoADXCoil(DXCoilNum)%AirMassFlowRate   = Node(WtoADXCoil(DXCoilNum)%AirInletNodeNum)%MassFlowRate/PartLoadRatio
  END IF

  Twet_rated             = WtoADXCoil(DXCoilNum)%Twet_rated
  Gamma_rated            = WtoADXCoil(DXCoilNum)%Gamma_rated

  LoadSideMassFlowRate   = WtoADXCoil(DXCoilNum)%AirMassFlowRate
  SourceSideMassFlowRate = WtoADXCoil(DXCoilNum)%WaterMassFlowRate
  SourceSideInletTemp    = WtoADXCoil(DXCoilNum)%InletWaterTemp
  SourceSideInletEnth    = WtoADXCoil(DXCoilNum)%InletWaterEnthalpy
  CpWater = GetSpecificHeatGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                                   SourceSideInletTemp, &
                                   PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex,  &
                                  'CalcVSHPCoolingSimple:SourceSideInletTemp')

   !Check for flows, do not perform simulation if no flow in load side or source side.
  IF (SourceSideMassFlowRate <= 0.0 .OR. LoadSideMassFlowRate <= 0.0)THEN
     WtoADXCoil(DXCoilNum)%SimFlag = .FALSE.
     RETURN
  ELSE
     WtoADXCoil(DXCoilNum)%SimFlag = .TRUE.
  ENDIF

  IF (CompOp .EQ. 0) THEN
     WtoADXCoil(DXCoilNum)%SimFlag = .FALSE.
     RETURN
  ENDIF

  !Loop the calculation at least once depending whether the latent degradation model
  !is enabled. 1st iteration to calculate the QLatent(rated) at (TDB,TWB)indoorair=(26.7C,19.4C)
  !and 2nd iteration to calculate the  QLatent(actual)
  IF((PartLoadRatio < 1d-10) .OR. (Twet_rated .LE. 0.0) .OR. (Gamma_rated .LE. 0.0) &
     .OR. (SpeedNum > 1.0)) THEN
    LatDegradModelSimFlag = .FALSE.
    !Set NumIteration=1 so that latent model would quit after 1 simulation with the actual condition
    NumIteration=1
  ELSE
    LatDegradModelSimFlag = .TRUE.
    !Set NumIteration=0 so that latent model would simulate twice with rated and actual condition
    NumIteration=0
  END IF


  !Set indoor air conditions to the actual condition
  LoadSideInletDBTemp_Unit = WtoADXCoil(DXCoilNum)%InletAirDBTemp
  LoadSideInletHumRat_Unit = WtoADXCoil(DXCoilNum)%InletAirHumRat
  LoadSideInletWBTemp_Unit = PsyTwbFnTdbWPb(LoadSideInletDBTemp_Unit,LoadSideInletHumRat_Unit,OutBaroPress,RoutineName)
  LoadSideInletEnth_Unit = WtoADXCoil(DXCoilNum)%InletAirEnthalpy
  CpAir_Unit = PsyCpAirFnWTdb(LoadSideInletHumRat_Unit,LoadSideInletDBTemp_Unit)

  RuntimeFrac = 1.0
  WtoADXCoil(DXCoilNum)%RunFrac = 1.0
  IF((SpeedNum == 1) .AND. (PartLoadRatio < 1.0)) THEN
    PLF = CurveValue(WtoADXCoil(DXCoilNum)%PLFFPLR,PartLoadRatio)
    IF (PLF < 0.7d0) THEN
     PLF = 0.7d0
    END IF
    ! calculate the run time fraction
    WtoADXCoil(DXCoilNum)%RunFrac = PartLoadRatio / PLF
    WtoADXCoil(DXCoilNum)%PartLoadRatio    = PartLoadRatio

    IF ( WtoADXCoil(DXCoilNum)%RunFrac > 1. ) THEN
      WtoADXCoil(DXCoilNum)%RunFrac = 1.0d0 ! Reset coil runtime fraction to 1.0
    ELSE IF ( WtoADXCoil(DXCoilNum)%RunFrac < 0.0 ) THEN
      WtoADXCoil(DXCoilNum)%RunFrac = 0.0
    END IF

    RuntimeFrac = WtoADXCoil(DXCoilNum)%RunFrac
  END IF


LOOP: DO
    NumIteration=NumIteration+1
    IF (NumIteration.EQ.1) THEN
    !Set indoor air conditions to the rated conditions
        LoadSideInletDBTemp = LoadSideInletDBTemp_Init
        LoadSideInletHumRat = LoadSideInletHumRat_Init
        LoadSideInletWBTemp = LoadSideInletWBTemp_Init
        LoadSideInletEnth = LoadSideInletEnth_Init
        CpAir = CpAir_Init
    ELSE
    !Set indoor air conditions to the actual condition
        LoadSideInletDBTemp = LoadSideInletDBTemp_Unit
        LoadSideInletHumRat = LoadSideInletHumRat_Unit
        LoadSideInletWBTemp = LoadSideInletWBTemp_Unit
        LoadSideInletEnth = LoadSideInletEnth_Unit
        CpAir = CpAir_Unit
    END IF

    IF(SpeedNum > MaxSpeed) THEN
        SpeedCal = MaxSpeed
    ELSE
        SpeedCal = SpeedNum
    END IF

    IF((SpeedNum == 1) .OR.(SpeedNum > MaxSpeed).OR. (SpeedRatio == 1.0)) THEN
        AirMassFlowRatio = LoadSideMassFlowRate/ WtoADXCoil(DXCoilNum)%DesignAirMassFlowRate
        WaterMassFlowRatio = SourceSideMassFlowRate/WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate

        EIRTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)
        EIRAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)
        EIRWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
        EIR = (1.0/WtoADXCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac

        CBFSpeed  = AdjustCBF(WtoADXCoil(DXCoilNum)%MSRatedCBF(SpeedCal),&
                WtoADXCoil(DXCoilNum)%MSRatedAirMassFlowRate(SpeedCal),LoadSideMassFlowRate)

        IF(CBFSpeed > 0.999) CBFSpeed = 0.999

        CALL CalcTotCapSHR_VSWSHP(LoadSideInletDBTemp,LoadSideInletHumRat,LoadSideInletEnth,LoadSideInletWBTemp, &
                         AirMassFlowRatio, WaterMassFlowRatio, &
                         LoadSideMassFlowRate,CBFSpeed, &
                         WtoADXCoil(DXCoilNum)%MSRatedTotCap(SpeedCal),WtoADXCoil(DXCoilNum)%MSCCapFTemp(SpeedCal), &
                         WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal), WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),&
                         0.0d0,0,0, 0,&
                         QLoadTotal1, QLoadTotal2, QLoadTotal,SHR,SourceSideInletTemp, &
                         WtoADXCoil(DXCoilNum)%InletAirPressure, 0.0d0, 1)

        Winput = QLoadTotal * EIR

        QWasteHeat =  Winput * WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
        QWasteHeat = QWasteHeat * CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)

    ELSE
        AirMassFlowRatio = LoadSideMassFlowRate/ WtoADXCoil(DXCoilNum)%DesignAirMassFlowRate
        WaterMassFlowRatio = SourceSideMassFlowRate/WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate
        AoEff = WtoADXCoil(DXCoilNum)%MSEffectiveAo(SpeedCal)*SpeedRatio &
            + (1.0 - SpeedRatio ) * WtoADXCoil(DXCoilNum)%MSEffectiveAo(SpeedCal - 1)

        CBFSpeed  = exp(-AoEff/LoadSideMassFlowRate)

        IF(CBFSpeed > 0.999) CBFSpeed = 0.999

        CALL CalcTotCapSHR_VSWSHP(LoadSideInletDBTemp,LoadSideInletHumRat,LoadSideInletEnth,LoadSideInletWBTemp, &
                 AirMassFlowRatio, WaterMassFlowRatio, &
                 LoadSideMassFlowRate,CBFSpeed, &
                 WtoADXCoil(DXCoilNum)%MSRatedTotCap(SpeedCal - 1),WtoADXCoil(DXCoilNum)%MSCCapFTemp(SpeedCal - 1), &
                 WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal - 1), WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal - 1),&
                 WtoADXCoil(DXCoilNum)%MSRatedTotCap(SpeedCal),WtoADXCoil(DXCoilNum)%MSCCapFTemp(SpeedCal), &
                 WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal), WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),&
                 QLoadTotal1, QLoadTotal2, QLoadTotal,SHR,SourceSideInletTemp, &
                 WtoADXCoil(DXCoilNum)%InletAirPressure, SpeedRatio, 2)

        SpeedCal =  SpeedNum - 1
        EIRTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)
        EIRAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)
        EIRWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
        EIR = (1.0/WtoADXCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
        Winput1 = QLoadTotal1 * EIR

        QWasteHeat1 =  Winput1 * WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
        QWasteHeat1 = QWasteHeat1 * CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)

        SpeedCal =  SpeedNum
        EIRTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)
        EIRAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)
        EIRWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
        EIR = (1.0/WtoADXCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
        Winput2 = QLoadTotal2 * EIR

        QWasteHeat2 =  Winput2 * WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
        QWasteHeat2 = QWasteHeat2 * CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletWBTemp,SourceSideInletTemp)


        Winput = Winput2*SpeedRatio + (1.0 - SpeedRatio ) * Winput1
        QWasteHeat = QWasteHeat2*SpeedRatio + (1.0 - SpeedRatio ) * QWasteHeat1
    END IF

    QSensible = QLoadTotal * SHR

    Qsource =  QLoadTotal + Winput - QWasteHeat

    IF(Qsource < 0) THEN
      Qsource = 0.0
      QWasteHeat =  QLoadTotal + Winput
    END IF

  !Check if the Sensible Load is greater than the Total Cooling Load
    IF(QSensible.GT.QLoadTotal) THEN
      QSensible = QLoadTotal
    END IF

    IF(LatDegradModelSimFlag) THEN
  !Calculate for SHReff using the Latent Degradation Model
      IF(NumIteration.EQ.1) THEN
          QLatRated=QLoadTotal-QSensible
      ELSEIF(NumIteration.EQ.2) THEN
          QLatActual=QLoadTotal-QSensible
          SHRss=QSensible/QLoadTotal
          SHReff = CalcEffectiveSHR(DXCoilNum, SHRss,CyclingScheme, RuntimeFrac, &
                 QLatRated, QLatActual, LoadSideInletDBTemp, LoadSideInletWBTemp)
!       Update sensible capacity based on effective SHR
          QSensible = QLoadTotal * SHReff
          EXIT LOOP
      END IF
    ELSE
  !Assume SHReff=SHRss
      SHReff = QSensible/QLoadTotal
      EXIT LOOP
    END IF
  END DO LOOP

  ! considering hot gas reheat here
  IF(WtoADXCoil(DXCoilNum)%HOTGASREHEATFLG > 0) THEN
    QLoadTotal = QLoadTotal - QWasteHeat
    QSensible = QSensible -QWasteHeat
    SHReff = QSensible/QLoadTotal
  END IF

  !calculate coil outlet state variables
  LoadSideOutletEnth   = LoadSideInletEnth - QLoadTotal/LoadSideMassFlowRate
  LoadSideOutletDBTemp = LoadSideInletDBTemp - QSensible/(LoadSideMassFlowRate * CpAir)

  MaxHumRat = PsyWFnTdbRhPb(LoadSideOutletDBTemp,0.9999d0,WtoADXCoil(DXCoilNum)%InletAirPressure,RoutineName)
  MaxOutletEnth = PsyHFnTdbW(LoadSideOutletDBTemp,MaxHumRat,RoutineName)
  IF(LoadSideOutletEnth > MaxOutletEnth) THEN
    LoadSideOutletEnth = MaxOutletEnth
    !QLoadTotal = LoadSideMassFlowRate * (LoadSideInletEnth - LoadSideOutletEnth)
  END IF
  LoadsideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideOutletEnth,RoutineName)
  IF(LoadsideOutletHumRat > MaxHumRat) THEN
    LoadsideOutletHumRat = MaxHumRat
  END IF

  Count = Count + 1
  !Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    WtoADXCoil(DXCoilNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideOutletEnth + &
                                                  (1.-PartLoadRatio)*LoadSideInletEnth
    WtoADXCoil(DXCoilNum)%OutletAirHumRat   = PartLoadRatio*LoadsideOutletHumRat + &
                                                  (1.-PartLoadRatio)*LoadSideInletHumRat
    WtoADXCoil(DXCoilNum)%OutletAirDBTemp   = PsyTdbFnHW(WtoADXCoil(DXCoilNum)%OutletAirEnthalpy,  &
                                                             WtoADXCoil(DXCoilNum)%OutletAirHumRat,    &
                                                             RoutineName)
    PLRCorrLoadSideMdot = LoadSideMassFlowRate
  ELSE
    ! default to cycling fan, cycling compressor
    WtoADXCoil(DXCoilNum)%OutletAirEnthalpy = LoadSideOutletEnth
    WtoADXCoil(DXCoilNum)%OutletAirHumRat   = LoadsideOutletHumRat
    WtoADXCoil(DXCoilNum)%OutletAirDBTemp   = LoadSideOutletDBTemp
    PLRCorrLoadSideMdot = LoadSideMassFlowRate*PartLoadRatio
  END IF

   ! scale heat transfer rates to PLR and power to RTF
  QLoadTotal = QLoadTotal*PartLoadRatio
  QSensible  = QSensible*PartLoadRatio
  Winput     = Winput*RuntimeFrac
  QSource    = QSource*PartLoadRatio
  QWasteHeat = QWasteHeat * PartLoadRatio

!  Add power to global variable so power can be summed by parent object
  DXElecCoolingPower = Winput

  ReportingConstant=TimeStepSys*SecInHour
  !Update heat pump data structure
  WtoADXCoil(DXCoilNum)%Power               = Winput
  WtoADXCoil(DXCoilNum)%QLoadTotal          = QLoadTotal
  WtoADXCoil(DXCoilNum)%QSensible           = QSensible
  WtoADXCoil(DXCoilNum)%QLatent             = QLoadTotal - QSensible
  WtoADXCoil(DXCoilNum)%QSource             = QSource
  WtoADXCoil(DXCoilNum)%Energy=Winput*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergyLoadTotal=QLoadTotal*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergySensible=QSensible*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergyLatent=(QLoadTotal - QSensible)*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergySource=QSource*ReportingConstant
  IF(RunTimeFrac == 0.0) THEN
    WtoADXCoil(DXCoilNum)%COP = 0.0
  ELSE
    WtoADXCoil(DXCoilNum)%COP = QLoadTotal/Winput
  END IF
  WtoADXCoil(DXCoilNum)%RunFrac             = RuntimeFrac
  WtoADXCoil(DXCoilNum)%PartLoadRatio       = PartLoadRatio
  WtoADXCoil(DXCoilNum)%AirMassFlowRate     = PLRCorrLoadSideMdot

  WtoADXCoil(DXCoilNum)%WaterMassFlowRate   = SourceSideMassFlowRate
  WtoADXCoil(DXCoilNum)%OutletWaterTemp     = SourceSideInletTemp + QSource/(SourceSideMassFlowRate * CpWater)
  WtoADXCoil(DXCoilNum)%OutletWaterEnthalpy = SourceSideInletEnth + QSource/SourceSideMassFlowRate

  WtoADXCoil(DXCoilNum)%QWasteHeat =  QWasteHeat

  RETURN
END SUBROUTINE CalcMulSpeedWSHPCoilCooling

SUBROUTINE  CalcMulSpeedWSHPCoilHeating(DXCoilNum,CyclingScheme,RuntimeFrac, &
        SensDemand,CompOp,PartLoadRatio,OnOffAirFlowRatio, SpeedRatio, SpeedNum)


          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:CalcHPHeatingSimple
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for simulating the heating mode of the Variable Speed Water to Air HP Simple

          ! METHODOLOGY EMPLOYED:
          ! Simulate the heat pump performance using the coefficients and rated conditions
          !
          ! Finally, adjust the heat pump outlet conditions based on the PartLoadRatio
          ! and RuntimeFrac.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE DataHVACGlobals,      ONLY:TimeStepSys, DXElecHeatingPower
  USE Psychrometrics,       ONLY:PsyWFnTdbTwbPb,PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyTwbFnTdbWPb,  &
                                 PsyTdbFnHW,PsyWFnTdbH
  USE FluidProperties,      ONLY:GetSpecificHeatGlycol
  USE DataPlant,            ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER,   INTENT(IN) :: DXCoilNum              ! Heat Pump Number
  INTEGER,   INTENT(IN) :: CyclingScheme      ! Fan/Compressor cycling scheme indicator
  REAL(r64), INTENT(IN) :: SensDemand         ! Cooling Sensible Demand [W] !unused1208
  INTEGER,   INTENT(IN) :: CompOp             ! compressor operation flag
  REAL(r64), INTENT(IN) :: PartLoadRatio      ! compressor part load ratio
  REAL(r64), INTENT(INOUT) :: RuntimeFrac        ! Runtime Fraction of compressor or percent on time (on-time/cycle time)
  REAL(r64), INTENT(IN) :: OnOffAirFlowRatio  ! ratio of compressor on flow to average flow over time step
  REAL(r64), INTENT(IN) :: SpeedRatio        ! SpeedRatio varies between 1.0 (higher speed) and 0.0 (lower speed)
  INTEGER, INTENT(IN)  :: SpeedNum      ! Speed number, high bound, i.e. SpeedNum - 1 is the other side

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Tref=283.15d0      ! Reference Temperature for performance curves,10C [K]
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcMulSpeedWSHPCoilHeating'


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CpWater                ! Specific heat of water [J/kg_C]
  REAL(r64) :: CpAir                  ! Specific heat of air [J/kg_C]

  REAL(r64) :: AirMassFlowRatio   ! airflow ratio at low speed
  REAL(r64) :: WaterMassFlowRatio   ! airflow ratio at high speed
  REAL(r64) :: TotCapAirFFModFac !air flow fraction modification
  REAL(r64) :: TotCapWaterFFModFac !water flow fraction modification
  REAL(r64) :: TotCapTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: EIRAirFFModFac !air flow fraction modification
  REAL(r64) :: EIRWaterFFModFac !water flow fraction modification
  REAL(r64) :: EIRTempModFac !total capacity temperature correctio fraction
  REAL(r64) :: EIR !total capacity temperature correctio fraction
  INTEGER :: MaxSpeed           ! maximum speed level
  INTEGER :: SpeedCal           ! calculated speed level
  REAL(r64) :: QLoadTotal1      ! heating capacit at low speed
  REAL(r64) :: QLoadTotal2      ! heating capacity at high speed
  REAL(r64) :: Winput1          ! power consumption at low speed
  REAL(r64) :: Winput2          ! power consumption at high speed
  REAL(r64) :: QWasteHeat       !recoverable waste heat
  REAL(r64) :: QWasteHeat1      !recoverable waste heat at low speed
  REAL(r64) :: QWasteHeat2      !recoverable waste heat at high speed
  REAL(r64) :: PLF  !part-load function
  REAL(r64) :: ReportingConstant

  MaxSpeed = WtoADXCoil(DXCoilNum)%NumofSpeeds

 !  LOAD LOCAL VARIABLES FROM DATA STRUCTURE (for code readability)
  IF (.NOT. (CyclingScheme .EQ. ContFanCycCoil) .AND. PartLoadRatio > 0.0) THEN
     WtoADXCoil(DXCoilNum)%AirMassFlowRate   = Node(WtoADXCoil(DXCoilNum)%AirInletNodeNum)%MassFlowRate/PartLoadRatio
  END IF

  LoadSideMassFlowRate   = WtoADXCoil(DXCoilNum)%AirMassFlowRate
  LoadSideInletDBTemp    = WtoADXCoil(DXCoilNum)%InletAirDBTemp
  LoadSideInletHumRat    = WtoADXCoil(DXCoilNum)%InletAirHumRat

  LoadSideInletWBTemp    = PsyTwbFnTdbWPb(LoadSideInletDBTemp,LoadSideInletHumRat,OutBaroPress,RoutineName)
  LoadSideInletEnth      = WtoADXCoil(DXCoilNum)%InletAirEnthalpy
  CpAir                  = PsyCpAirFnWTdb(LoadSideInletHumRat,LoadSideInletDBTemp,RoutineName)
  SourceSideMassFlowRate = WtoADXCoil(DXCoilNum)%WaterMassFlowRate
  SourceSideInletTemp    = WtoADXCoil(DXCoilNum)%InletWaterTemp
  SourceSideInletEnth    = WtoADXCoil(DXCoilNum)%InletWaterEnthalpy
  CpWater                = GetSpecificHeatGlycol(PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidName, &
                                   SourceSideInletTemp, &
                                   PlantLoop(WtoADXCoil(DXCoilNum)%LoopNum)%FluidIndex,  &
                                   RoutineName//':SourceSideInletTemp')

 !Check for flows, do not perform simulation if no flow in load side or source side.
  IF (SourceSideMassFlowRate <= 0.0 .OR. LoadSideMassFlowRate <= 0.0)THEN
    WtoADXCoil(DXCoilNum)%SimFlag = .FALSE.
    RETURN
  ELSE
    WtoADXCoil(DXCoilNum)%SimFlag = .TRUE.
  ENDIF

  IF (CompOp .EQ. 0) THEN
    WtoADXCoil(DXCoilNum)%SimFlag = .FALSE.
    RETURN
  ENDIF

  IF(SpeedNum > MaxSpeed) THEN
      SpeedCal = MaxSpeed
  ELSE
      SpeedCal = SpeedNum
  END IF

  RuntimeFrac = 1.0
  WtoADXCoil(DXCoilNum)%RunFrac = 1.0
  IF((SpeedNum == 1) .AND. (PartLoadRatio < 1.0)) THEN
    PLF = CurveValue(WtoADXCoil(DXCoilNum)%PLFFPLR,PartLoadRatio)
    IF (PLF < 0.7d0) THEN
     PLF = 0.7d0
    END IF
    ! calculate the run time fraction
    WtoADXCoil(DXCoilNum)%RunFrac = PartLoadRatio / PLF
    WtoADXCoil(DXCoilNum)%PartLoadRatio    = PartLoadRatio

    IF ( WtoADXCoil(DXCoilNum)%RunFrac > 1. ) THEN
      WtoADXCoil(DXCoilNum)%RunFrac = 1.0d0 ! Reset coil runtime fraction to 1.0
    ELSE IF( WtoADXCoil(DXCoilNum)%RunFrac < 0.0 ) THEN
      WtoADXCoil(DXCoilNum)%RunFrac = 0.0
    END IF

    RuntimeFrac = WtoADXCoil(DXCoilNum)%RunFrac
  END IF

  IF((SpeedNum == 1) .OR.(SpeedNum > MaxSpeed) .OR. (SpeedRatio == 1.0)) THEN
    AirMassFlowRatio = LoadSideMassFlowRate/ WtoADXCoil(DXCoilNum)%DesignAirMassFlowRate
    WaterMassFlowRatio = SourceSideMassFlowRate/WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate

    TotCapTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    TotCapAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal),AirMassFlowRatio)
    TotCapWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),WaterMassFlowRatio)

    QLoadTotal = WtoADXCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac

    EIRTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    EIRAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)
    EIRWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
    EIR = (1.0/WtoADXCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
    Winput = QLoadTotal * EIR

    QWasteHeat =  Winput * WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
    QWasteHeat = QWasteHeat * CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)

  ELSE
    AirMassFlowRatio = LoadSideMassFlowRate/ WtoADXCoil(DXCoilNum)%DesignAirMassFlowRate
    WaterMassFlowRatio = SourceSideMassFlowRate/WtoADXCoil(DXCoilNum)%DesignWaterMassFlowRate

    SpeedCal =  SpeedNum - 1
    TotCapTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    TotCapAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal),AirMassFlowRatio)
    TotCapWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),WaterMassFlowRatio)

    QLoadTotal1 = WtoADXCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac

    EIRTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    EIRAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)
    EIRWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
    EIR = (1.0/WtoADXCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
    Winput1 = QLoadTotal1 * EIR

    QWasteHeat1 =  Winput1 * WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
    QWasteHeat1 = QWasteHeat1 * CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)


    SpeedCal =  SpeedNum
    TotCapTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    TotCapAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapAirFFlow(SpeedCal),AirMassFlowRatio)
    TotCapWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSCCapWaterFFlow(SpeedCal),WaterMassFlowRatio)

    QLoadTotal2 = WtoADXCoil(DXCoilNum)%MSRatedTotCap(SpeedCal) * TotCapTempModFac * TotCapAirFFModFac * TotCapWaterFFModFac

    EIRTempModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRFTemp(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)
    EIRAirFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRAirFFlow(SpeedCal),AirMassFlowRatio)
    EIRWaterFFModFac = CurveValue(WtoADXCoil(DXCoilNum)%MSEIRWaterFFlow(SpeedCal),WaterMassFlowRatio)
    EIR = (1.0/WtoADXCoil(DXCoilNum)%MSRatedCOP(SpeedCal)) * EIRTempModFac * EIRAirFFModFac * EIRWaterFFModFac
    Winput2 = QLoadTotal2 * EIR

    QWasteHeat2 =  Winput2 * WtoADXCoil(DXCoilNum)%MSWasteHeatFrac(SpeedCal)
    QWasteHeat2 = QWasteHeat2 * CurveValue(WtoADXCoil(DXCoilNum)%MSWasteHeat(SpeedCal),LoadSideInletDBTemp,SourceSideInletTemp)


    QLoadTotal = QLoadTotal2*SpeedRatio + (1.0 - SpeedRatio ) * QLoadTotal1
    Winput = Winput2*SpeedRatio + (1.0 - SpeedRatio ) * Winput1
    QWasteHeat = QWasteHeat2*SpeedRatio + (1.0 - SpeedRatio ) * QWasteHeat1
  END IF


  Qsource = QLoadTotal+ QWasteHeat -Winput
  QSensible = QLoadTotal

  IF(Qsource < 0) THEN
    Qsource = 0.0
    QWasteHeat =  Winput - QLoadTotal
  END IF

  ! calculate coil outlet state variables
  LoadSideOutletEnth   = LoadSideInletEnth + QLoadTotal/LoadSideMassFlowRate
  LoadSideOutletDBTemp = LoadSideInletDBTemp + QSensible/(LoadSideMassFlowRate * CpAir)
  LoadsideOutletHumRat = PsyWFnTdbH(LoadSideOutletDBTemp,LoadSideOutletEnth,RoutineName)

  ! Actual outlet conditions are "average" for time step
  IF (CyclingScheme .EQ. ContFanCycCoil) THEN
    ! continuous fan, cycling compressor
    WtoADXCoil(DXCoilNum)%OutletAirEnthalpy = PartLoadRatio*LoadSideOutletEnth + &
                                                  (1.d0-PartLoadRatio)*LoadSideInletEnth
    WtoADXCoil(DXCoilNum)%OutletAirHumRat   = PartLoadRatio*LoadsideOutletHumRat + &
                                                  (1.d0-PartLoadRatio)*LoadSideInletHumRat
    WtoADXCoil(DXCoilNum)%OutletAirDBTemp   = PsyTdbFnHW(WtoADXCoil(DXCoilNum)%OutletAirEnthalpy,  &
                                                               WtoADXCoil(DXCoilNum)%OutletAirHumRat,RoutineName)
    PLRCorrLoadSideMdot = LoadSideMassFlowRate
  ELSE
    ! default to cycling fan, cycling compressor
    WtoADXCoil(DXCoilNum)%OutletAirEnthalpy = LoadSideOutletEnth
    WtoADXCoil(DXCoilNum)%OutletAirHumRat   = LoadsideOutletHumRat
    WtoADXCoil(DXCoilNum)%OutletAirDBTemp   = LoadSideOutletDBTemp
    PLRCorrLoadSideMdot = LoadSideMassFlowRate*PartLoadRatio
  END IF


   ! scale heat transfer rates to PLR and power to RTF
  QLoadTotal = QLoadTotal*PartLoadRatio
  QSensible  = QSensible*PartLoadRatio
  Winput     = Winput*RuntimeFrac
  QSource    = QSource*PartLoadRatio
  QWasteHeat = QWasteHeat*PartLoadRatio

!  Add power to global variable so power can be summed by parent object
  DXElecHeatingPower = Winput

  ReportingConstant=TimeStepSys*SecInHour
  !Update heat pump data structure
  WtoADXCoil(DXCoilNum)%Power               = Winput
  WtoADXCoil(DXCoilNum)%QLoadTotal          = QLoadTotal
  WtoADXCoil(DXCoilNum)%QSensible           = QSensible
  WtoADXCoil(DXCoilNum)%QSource             = QSource
  WtoADXCoil(DXCoilNum)%Energy=Winput*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergyLoadTotal=QLoadTotal*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergySensible=QSensible*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergyLatent=0.0
  WtoADXCoil(DXCoilNum)%EnergySource=QSource*ReportingConstant
  IF(RunTimeFrac == 0.0) THEN
    WtoADXCoil(DXCoilNum)%COP = 0.0
  ELSE
    WtoADXCoil(DXCoilNum)%COP = QLoadTotal/Winput
  END IF
  WtoADXCoil(DXCoilNum)%RunFrac             = RuntimeFrac
  WtoADXCoil(DXCoilNum)%PartLoadRatio       = PartLoadRatio
  WtoADXCoil(DXCoilNum)%AirMassFlowRate     = PLRCorrLoadSideMdot

  WtoADXCoil(DXCoilNum)%WaterMassFlowRate   = SourceSideMassFlowRate
  WtoADXCoil(DXCoilNum)%OutletWaterTemp     = SourceSideInletTemp - QSource/(SourceSideMassFlowRate * CpWater)
  WtoADXCoil(DXCoilNum)%OutletWaterEnthalpy = SourceSideInletEnth - QSource/SourceSideMassFlowRate

  WtoADXCoil(DXCoilNum)%QWasteHeat =  QWasteHeat
  RETURN
END SUBROUTINE CalcMulSpeedWSHPCoilHeating

FUNCTION GetCoilCapacityMulSpeedWSHP(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilCapacity
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the rated coil capacity at the nominal speed level for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilCapacity ! returned capacity of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (CoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' .or.   &
      CoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') THEN
    WhichCoil=FindItemInList(CoilName,WtoADXCoil%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      IF (CoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') THEN
        CoilCapacity=WtoADXCoil(WhichCoil)%RatedCapHeat
      ELSE
        CoilCapacity=WtoADXCoil(WhichCoil)%RatedCapCoolTotal
      ENDIF
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilCapacity=-1000.
  ENDIF

  RETURN

END FUNCTION GetCoilCapacityMulSpeedWSHP

FUNCTION GetCoilIndexMulSpeedWSHP(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilIndex
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil index for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: IndexNum     ! returned index of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IndexNum=FindItemInList(CoilName,WtoADXCoil%Name,NumWaterToAirHPs)

  IF (IndexNum == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetCoilIndexMulSpeedWSHP

FUNCTION GetCoilAirFlowRateMulSpeedWSHP(CoilType,CoilName,ErrorsFound) RESULT(CoilAirFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilAirFlowRate
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max coil air flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilAirFlowRate ! returned air volume flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (CoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' .or.   &
      CoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') THEN
    WhichCoil=FindItemInList(CoilName,WtoADXCoil%Name,NumWaterToAirHPs)
    IF (WhichCoil /= 0) THEN
      !CoilAirFlowRate=WtoADXCoil(WhichCoil)%RatedAirVolFlowRate
      IF(WtoADXCoil(WhichCoil)%RatedAirVolFlowRate == AUTOSIZE) THEN !means autosize
        CoilAirFlowRate=WtoADXCoil(WhichCoil)%RatedAirVolFlowRate
      ELSE
        CoilAirFlowRate=WtoADXCoil(WhichCoil)%MSRatedAirVolFlowRate(WtoADXCoil(WhichCoil)%NumOfSpeeds)/ &
            WtoADXCoil(WhichCoil)%MSRatedAirVolFlowRate(WtoADXCoil(WhichCoil)%NormSpedLevel) * &
             WtoADXCoil(WhichCoil)%RatedAirVolFlowRate
      END IF ! use largest air flow rate
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoilAirFlowRate=-1000.
  ENDIF

  RETURN

END FUNCTION GetCoilAirFlowRateMulSpeedWSHP


FUNCTION GetCoilInletNodeMulSpeedWSHP(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilInletNode
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,WtoADXCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=WtoADXCoil(WhichCoil)%AirInletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNodeMulSpeedWSHP


FUNCTION GetCoilOutletNodeMulSpeedWSHP(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:GetCoilOutletNode
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the outlet node.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindGlycol
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  WhichCoil=FindItemInList(CoilName,WtoADXCoil%Name,NumWatertoAirHPs)
  IF (WhichCoil /= 0) THEN
    NodeNumber=WtoADXCoil(WhichCoil)%AirOutletNodeNum
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('Could not find CoilType="'//TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNodeMulSpeedWSHP

SUBROUTINE SetMulSpeedWSHPData(WSHPNum,ErrorsFound,CompanionCoolingCoilNum,CompanionHeatingCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:SetWSHPData
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine was designed to "push" information from a parent object to
          ! this WSHP coil object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: TrimSigDigits
  USE InputProcessor,  ONLY: FindItemInList, SameString
  USE FluidProperties, ONLY: FindGlycol

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: WSHPNum  ! Number of OA Controller
  LOGICAL, INTENT(INOUT) :: ErrorsFound    ! Set to true if certain errors found
  INTEGER, OPTIONAL      :: CompanionCoolingCoilNum  ! Index to cooling coil for heating coil = SimpleWSHPNum
  INTEGER, OPTIONAL      :: CompanionHeatingCoilNum  ! Index to heating coil for cooling coil = SimpleWSHPNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and Allocates WatertoAirHP related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetMulSpeedWSHPInput
!    WaterIndex=FindGlycol('WATER') !Initialize the WaterIndex once
    GetCoilsInputFlag=.FALSE.
  End If

  IF (WSHPNum <= 0 .or. WSHPNum > NumWatertoAirHPs) THEN
    CALL ShowSevereError('SetMulSpeedWSHPData: called with VS WSHP Coil Number out of range='//  &
         TRIM(TrimSigDigits(WSHPNum))//' should be >0 and <'//TRIM(TrimSigDigits(NumWatertoAirHPs)))
    ErrorsFound=.true.
    RETURN
  ENDIF

  IF (PRESENT(CompanionCoolingCoilNum)) THEN
    WtoADXCoil(WSHPNum)%CompanionCoolingCoilNum=CompanionCoolingCoilNum
    WtoADXCoil(WSHPNum)%FindCompanionUpStreamCoil = .TRUE.
    WtoADXCoil(CompanionCoolingCoilNum)%CompanionHeatingCoilNum=WSHPNum
  ENDIF

  IF (PRESENT(CompanionHeatingCoilNum)) THEN
    WtoADXCoil(WSHPNum)%CompanionHeatingCoilNum=CompanionHeatingCoilNum
    WtoADXCoil(CompanionHeatingCoilNum)%CompanionCoolingCoilNum=WSHPNum
  ENDIF

  RETURN

END SUBROUTINE SetMulSpeedWSHPData

SUBROUTINE UpdateMulSpeedWSHP(DXCoilNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:UpdateSimpleWSHP
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the Water to Air Heat Pump outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the HP data structure to the HP outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys
  USe PlantUtilities,  ONLY: SafeCopyPlantNode
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(In) :: DXCoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER              :: AirInletNode
  INTEGER              :: WaterInletNode
  INTEGER              :: AirOutletNode
  INTEGER              :: WaterOutletNode
  REAL(r64)            :: ReportingConstant


  !WatertoAirHP(DXCoilNum)%Simflag=.FALSE.
  IF(.NOT. WtoADXCoil(DXCoilNum)%Simflag)THEN
    ! Heatpump is off; just pass through conditions
    WtoADXCoil(DXCoilNum)%Power               = 0.0
    WtoADXCoil(DXCoilNum)%QLoadTotal          = 0.0
    WtoADXCoil(DXCoilNum)%QSensible           = 0.0
    WtoADXCoil(DXCoilNum)%QLatent             = 0.0
    WtoADXCoil(DXCoilNum)%QSource             = 0.0
    WtoADXCoil(DXCoilNum)%Energy              = 0.0
    WtoADXCoil(DXCoilNum)%EnergyLoadTotal     = 0.0
    WtoADXCoil(DXCoilNum)%EnergySensible      = 0.0
    WtoADXCoil(DXCoilNum)%EnergyLatent        = 0.0
    WtoADXCoil(DXCoilNum)%EnergySource        = 0.0
    WtoADXCoil(DXCoilNum)%COP                 = 0.0
    WtoADXCoil(DXCoilNum)%RunFrac             = 0.0
    WtoADXCoil(DXCoilNum)%PartLoadRatio       = 0.0

    WtoADXCoil(DXCoilNum)%OutletAirDBTemp     = WtoADXCoil(DXCoilNum)%InletAirDBTemp
    WtoADXCoil(DXCoilNum)%OutletAirHumRat     = WtoADXCoil(DXCoilNum)%InletAirHumRat
    WtoADXCoil(DXCoilNum)%OutletAirEnthalpy   = WtoADXCoil(DXCoilNum)%InletAirEnthalpy
    WtoADXCoil(DXCoilNum)%OutletWaterTemp     = WtoADXCoil(DXCoilNum)%InletWaterTemp
    WtoADXCoil(DXCoilNum)%OutletWaterEnthalpy = WtoADXCoil(DXCoilNum)%InletWaterEnthalpy
  END IF

  AirInletNode    = WtoADXCoil(DXCoilNum)%AirInletNodeNum
  WaterInletNode  = WtoADXCoil(DXCoilNum)%WaterInletNodeNum
  AirOutletNode   = WtoADXCoil(DXCoilNum)%AirOutletNodeNum
  WaterOutletNode = WtoADXCoil(DXCoilNum)%WaterOutletNodeNum


  ! Set the air outlet  nodes of the WatertoAirHPSimple
  Node(AirOutletNode)%MassFlowRate          = Node(AirInletNode)%MassFlowRate     !LoadSideMassFlowRate
  Node(AirOutletNode)%Temp                  = WtoADXCoil(DXCoilNum)%OutletAirDBTemp
  Node(AirOutletNode)%HumRat                = WtoADXCoil(DXCoilNum)%OutletAirHumRat
  Node(AirOutletNode)%Enthalpy              = WtoADXCoil(DXCoilNum)%OutletAirEnthalpy

   ! Set the air outlet nodes for properties that just pass through & not used
  Node(AirOutletNode)%Quality               = Node(AirInletNode)%Quality
  Node(AirOutletNode)%Press                 = Node(AirInletNode)%Press
  Node(AirOutletNode)%MassFlowRateMin       = Node(AirInletNode)%MassFlowRateMin
  Node(AirOutletNode)%MassFlowRateMax       = Node(AirInletNode)%MassFlowRateMax    !LoadSideMassFlowRate
  Node(AirOutletNode)%MassFlowRateMinAvail  = Node(AirInletNode)%MassFlowRateMinAvail
  Node(AirOutletNode)%MassFlowRateMaxAvail  = Node(AirInletNode)%MassFlowRateMaxAvail     !LoadSideMassFlowRate

   ! Set the water outlet node of the WatertoAirHPSimple
   ! Set the water outlet nodes for properties that just pass through & not used
  CALL SafeCopyPlantNode(WaterInletNode , WaterOutletNode)

  Node(WaterOutletNode)%Temp                = WtoADXCoil(DXCoilNum)%OutletWaterTemp
  Node(WaterOutletNode)%Enthalpy            = WtoADXCoil(DXCoilNum)%OutletWaterEnthalpy

  ReportingConstant                         = TimeStepSys*SecInHour
  WtoADXCoil(DXCoilNum)%Energy          = WtoADXCoil(DXCoilNum)%Power*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergyLoadTotal = WtoADXCoil(DXCoilNum)%QLoadTotal*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergySensible  = WtoADXCoil(DXCoilNum)%QSensible*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergyLatent    = WtoADXCoil(DXCoilNum)%QLatent*ReportingConstant
  WtoADXCoil(DXCoilNum)%EnergySource    = WtoADXCoil(DXCoilNum)%QSource*ReportingConstant

   IF (Contaminant%CO2Simulation) Then
     Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
   End If

  RETURN
END SUBROUTINE UpdateMulSpeedWSHP

FUNCTION CalcEffectiveSHR(DXCoilNum,SHRss, CyclingScheme, RTF, QLatRated, QLatActual, EnteringDB, EnteringWB) RESULT(SHReff)

        ! FUNCTION INFORMATION:
        !    AUTHOR         Bo Shen, based on WatertoAirHeatPumpSimple:CalcEffectiveSHR
        !    DATE WRITTEN   March 2012
        !    MODIFIED       na
        !    RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        !    Adjust sensible heat ratio to account for degradation of DX coil latent
        !    capacity at part-load (cycling) conditions.

        ! METHODOLOGY EMPLOYED:
        !    With model parameters entered by the user, the part-load latent performance
        !    of a DX cooling coil is determined for a constant air flow system with
        !    a cooling coil that cycles on/off. The model calculates the time
        !    required for condensate to begin falling from the cooling coil.
        !    Runtimes greater than this are integrated to a "part-load" latent
        !    capacity which is used to determine the "part-load" sensible heat ratio.
        !    See reference below for additional details (linear decay model, Eq. 8b).

        !    For cycling fan operation, a modified version of Henderson and Rengarajan (1996)
        !    model is used by ultilizing the fan delay time as the time-off (or time duration
        !    for the re-evaporation of moisture from time coil). Refer to Tang, C.C. (2005)

        ! REFERENCES:
        ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DXCoilNum         ! Index number for cooling coil
  INTEGER, INTENT (IN) :: CyclingScheme ! Fan/compressor cycling scheme indicator
  REAL(r64), INTENT (IN) :: SHRss         ! Steady-state sensible heat ratio
  REAL(r64), INTENT (IN) :: RTF           ! Compressor run-time fraction
  REAL(r64), INTENT (IN) :: QLatRated     ! Rated latent capacity
  REAL(r64), INTENT (IN) :: QLatActual    ! Actual latent capacity
  REAL(r64), INTENT (IN) :: EnteringDB    ! Entering air dry-bulb temperature
  REAL(r64), INTENT (IN) :: EnteringWB    ! Entering air wet-bulb temperature
  REAL(r64)            :: SHReff        ! Effective sensible heat ratio, includes degradation due to cycling effects

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Twet                 ! Nominal time for condensate to begin leaving the coil's condensate drain line
                                    ! at the current operating conditions (sec)
  REAL(r64) :: Gamma                ! Initial moisture evaporation rate divided by steady-state AC latent capacity
                                    ! at the current operating conditions
  REAL(r64) :: Twet_rated           ! Twet at rated conditions (coil air flow rate and air temperatures), sec
  REAL(r64) :: Gamma_rated          ! Gamma at rated conditions (coil air flow rate and air temperatures)
  REAL(r64) :: Twet_max             ! Maximum allowed value for Twet
  REAL(r64) :: MaxONOFFCyclesperHour  ! Maximum cycling rate of heat pump [cycles/hr]
  REAL(r64) :: HPTimeConstant       ! Heat pump time constant [s]
  REAL(r64) :: FanDelayTime         ! Fan delay time, time delay for the HP's fan to
                                    ! shut off after compressor cycle off  [s]
  REAL(r64) :: Ton                  ! Coil on time (sec)
  REAL(r64) :: Toff                 ! Coil off time (sec)
  REAL(r64) :: Toffa                ! Actual coil off time (sec). Equations valid for Toff <= (2.0 * Twet/Gamma)
  REAL(r64) :: aa                   ! Intermediate variable
  REAL(r64) :: To1                  ! Intermediate variable (first guess at To). To = time to the start of moisture removal
  REAL(r64) :: To2                  ! Intermediate variable (second guess at To). To = time to the start of moisture removal
  REAL(r64) :: Error                ! Error for iteration (DO) loop
  REAL(r64) :: LHRmult              ! Latent Heat Ratio (LHR) multiplier. The effective latent heat ratio LHR = (1-SHRss)*LHRmult

   Twet_rated               = WtoADXCoil(DXCoilNum)%Twet_Rated
   Gamma_rated              = WtoADXCoil(DXCoilNum)%Gamma_Rated
   MaxONOFFCyclesperHour    = WtoADXCoil(DXCoilNum)%MaxONOFFCyclesperHour
   HPTimeConstant           = WtoADXCoil(DXCoilNum)%HPTimeConstant
   FanDelayTime             = WtoADXCoil(DXCoilNum)%FanDelayTime

!  No moisture evaporation (latent degradation) occurs for runtime fraction of 1.0
!  All latent degradation model parameters cause divide by 0.0 if not greater than 0.0
!  Latent degradation model parameters initialize to 0.0 meaning no evaporation model used.
   IF((RTF.GE.1.0) .OR. (QLatRated.EQ.0.0) .OR. (QLatActual.EQ.0.0) .OR. (Twet_rated.LE.0.0) .OR. &
      (Gamma_rated.LE.0.0) .OR. (MaxONOFFCyclesperHour.LE.0.0) .OR. (HPTimeConstant.LE.0.0) .OR. (RTF.LE. 0.0)) THEN
     SHReff = SHRss
     RETURN
   ENDIF

   Twet_max   = 9999.0 ! high limit for Twet

!  Calculate the model parameters at the actual operating conditions
   Twet    = MIN(Twet_rated*QLatRated /(QLatActual+1.d-10),Twet_max)
   Gamma   = Gamma_rated*QLatRated*(EnteringDB-EnteringWB)/((26.7d0-19.4d0)*QLatActual+1.d-10)

!  Calculate the compressor on and off times using a converntional thermostat curve
   Ton  = 3600.d0/(4.d0*MaxONOFFCyclesperHour*(1.d0-RTF))   ! duration of cooling coil on-cycle (sec)

   IF ((CyclingScheme .EQ. CycFanCycCoil).AND.(FanDelayTime.NE.0.0)) THEN
    ! For CycFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
    ! until the fan cycle off. Assume no evaporation from the coil after the fan shuts off.
        Toff = FanDelayTime
   ELSE
    ! For ContFanCycCoil, moisture is evaporated from the cooling coil back to the air stream
    ! for the entire heat pump off-cycle.
        Toff = 3600.d0/(4.d0*MaxONOFFCyclesperHour*RTF)        ! duration of cooling coil off-cycle (sec)
   END IF

!  Cap Toff to meet the equation restriction
   IF(Gamma .GT. 0.0)THEN
     Toffa = MIN(Toff, 2.d0*Twet/Gamma)
   ELSE
     Toffa = Toff
   END IF

!  Use sucessive substitution to solve for To
   aa = (Gamma*Toffa) - (0.25d0/Twet)*(Gamma**2)*(Toffa**2)

   To1 = aa+HPTimeConstant
   Error = 1.0
   DO WHILE (Error .gt. 0.001d0)
       To2 = aa-HPTimeConstant*(EXP(-To1/HPTimeConstant)-1.0d0)
       Error = ABS((To2-To1)/To1)
       To1 = To2
   END DO

!  Adjust Sensible Heat Ratio (SHR) using Latent Heat Ratio (LHR) multiplier
!  Floating underflow errors occur when -Ton/HPTimeConstant is a large negative number.
!  Cap lower limit at -700 to avoid the underflow errors.
   aa = EXP(MAX(-700.0d0,-Ton/HPTimeConstant))
!  Calculate latent heat ratio multiplier
   LHRmult = MAX(((Ton-To2)/(Ton+HPTimeConstant*(aa-1.0d0))),0.0d0)

!  Calculate part-load or "effective" sensible heat ratio
   SHReff = 1.0-(1.0-SHRss)*LHRmult

   IF (SHReff .LT. SHRss) SHReff = SHRss ! Effective SHR can be less than the steady-state SHR
   IF (SHReff .GT. 1.0) SHReff=1.0 ! Effective sensible heat ratio can't be greater than 1.0

 RETURN

END FUNCTION CalcEffectiveSHR

SUBROUTINE CalcTotCapSHR_VSWSHP(InletDryBulb,InletHumRat,InletEnthalpy,InletWetBulb,AirMassFlowRatio, WaterMassFlowRatio, &
                         AirMassFlow,CBF, TotCapNom1,CCapFTemp1,CCapAirFFlow1, CCapWaterFFlow1,&
                          TotCapNom2,CCapFTemp2,CCapAirFFlow2, CCapWaterFFlow2,&
                         TotCap1, TotCap2, TotCapSpeed,SHR,CondInletTemp, Pressure, SpeedRatio, NumSpeeds)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bo Shen, , based on DX:CalcTotCapSHR, introducing two speed levels
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates total capacity and sensible heat ratio of a DX coil at the specified conditions

          ! METHODOLOGY EMPLOYED:
          ! With the rated performance data entered by the user, the model employs some of the
          ! DOE-2.1E curve fits to adjust the capacity and SHR of the unit as a function
          ! of entering air temperatures and supply air flow rate (actual vs rated flow). The model
          ! does NOT employ the exact same methodology to calculate performance as DOE-2, although
          ! some of the DOE-2 curve fits are employed by this model.

          ! The model checks for coil dryout conditions, and adjusts the calculated performance
          ! appropriately.

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit page 4-81.
          !
          ! Henderson, H.I. Jr., K. Rengarajan and D.B. Shirey, III. 1992.The impact of comfort
          ! control on air conditioner energy use in humid climates. ASHRAE Transactions 98(2):
          ! 104-113.
          !
          ! Henderson, H.I. Jr., Danny Parker and Y.J. Huang. 2000.Improving DOE-2's RESYS routine:
          ! User Defined Functions to Provide More Accurate Part Load Energy Use and Humidity
          ! Predictions. Proceedings of ACEEE Conference.


          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT (IN) :: InletDryBulb       ! inlet air dry bulb temperature [C]
  REAL(r64), INTENT (IN) :: InletHumRat        ! inlet air humidity ratio [kg water / kg dry air]
  REAL(r64), INTENT (IN) :: InletEnthalpy      ! inlet air specific enthalpy [J/kg]
  REAL(r64), INTENT (IN) :: InletWetBulb       ! inlet air wet bulb temperature [C]
  REAL(r64), INTENT (IN) :: AirMassFlowRatio   ! Ratio of actual air mass flow to nominal air mass flow
  REAL(r64), INTENT (IN) :: WaterMassFlowRatio   ! Ratio of actual water mass flow to nominal water mass flow
  REAL(r64), INTENT (IN) :: AirMassFlow        ! actual mass flow for capacity and SHR calculation
  REAL(r64), INTENT (IN) :: CBF                ! coil bypass factor
  INTEGER, INTENT (IN) :: NumSpeeds            ! number of speeds for input

  REAL(r64), INTENT (IN) :: TotCapNom1         ! nominal total capacity at low speed [W]
  INTEGER, INTENT (IN) :: CCapFTemp1           ! capacity modifier curve index, function of entering wetbulb at low speed
  INTEGER, INTENT (IN) :: CCapAirFFlow1        ! capacity modifier curve, function of actual air flow vs rated flow at low speed
  INTEGER, INTENT (IN) :: CCapWaterFFlow1      ! capacity modifier curve, function of actual water flow vs rated flow at low speed

  REAL(r64), INTENT (IN) :: TotCapNom2         ! nominal total capacity at high speed [W]
  INTEGER, INTENT (IN) :: CCapFTemp2           ! capacity modifier curve index, function of entering wetbulb at high speed
  INTEGER, INTENT (IN) :: CCapAirFFlow2        ! capacity modifier curve, function of actual air flow vs rated flow at high speed
  INTEGER, INTENT (IN) :: CCapWaterFFlow2      ! capacity modifier curve, function of actual water flow vs rated flow at high speed


  REAL(r64), INTENT (OUT)   :: TotCap1           ! total capacity at the given conditions [W] at low speed
  REAL(r64), INTENT (OUT)   :: TotCap2           ! total capacity at the given conditions [W] at high speed
  REAL(r64), INTENT (OUT)   :: TotCapSpeed       ! integrated total capacity corresponding to the speed ratio

  REAL(r64), INTENT (OUT)   :: SHR               ! sensible heat ratio at the given conditions
  REAL(r64), INTENT (IN) :: CondInletTemp      ! Condenser inlet temperature [C]
  REAL(r64), INTENT (IN) :: Pressure           ! air pressure [Pa]
  REAL(r64), INTENT (IN) :: SpeedRatio         ! from 0.0 to 1.0


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='CalcTotCapSHR_VSWSHP'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: InletWetBulbCalc    ! calculated inlet wetbulb temperature used for finding dry coil point [C]
  REAL(r64) :: InletHumRatCalc     ! calculated inlet humidity ratio used for finding dry coil point [kg water / kg dry air]
  REAL(r64) :: TotCapTempModFac1
  ! Total capacity modifier (function of entering wetbulb, outside water inlet temp) at low speed
  REAL(r64) :: TotCapAirFlowModFac1
  ! Total capacity modifier (function of actual supply air flow vs nominal flow) at low speed
  REAL(r64) :: TotCapWaterFlowModFac1
  ! Total capacity modifier (function of actual supply water flow vs nominal flow) at low speed
  REAL(r64) :: TotCapTempModFac2
  ! Total capacity modifier (function of entering wetbulb, outside water inlet temp) at high speed
  REAL(r64) :: TotCapAirFlowModFac2
  ! Total capacity modifier (function of actual supply air flow vs nominal flow) at high speed
  REAL(r64) :: TotCapWaterFlowModFac2
  ! Total capacity modifier (function of actual supply water flow vs nominal flow) at high speed
  REAL(r64) :: hDelta              ! Change in air enthalpy across the cooling coil [J/kg]
  REAL(r64) :: hADP                ! Apparatus dew point enthalpy [J/kg]
  REAL(r64) :: tADP                ! Apparatus dew point temperature [C]
  REAL(r64) :: wADP                ! Apparatus dew point humidity ratio [kg/kg]
  REAL(r64) :: hTinwADP            ! Enthalpy at inlet dry-bulb and wADP [J/kg]
  REAL(r64) :: SHRCalc             ! temporary calculated value of SHR
  REAL(r64) :: TotCapCalc          ! temporary calculated value of total capacity [W]
  REAL(r64) :: TotCapCalc1          ! temporary calculated value of total capacity [W] at low speed
  REAL(r64) :: TotCapCalc2          ! temporary calculated value of total capacity [W] at high speed
  INTEGER :: Counter             ! Counter for dry evaporator iterations
  INTEGER :: MaxIter             ! Maximum number of iterations for dry evaporator calculations
  REAL(r64) :: RF                  ! Relaxation factor for dry evaporator iterations
  REAL(r64) :: Tolerance           ! Error tolerance for dry evaporator iterations
  REAL(r64) :: werror              ! Deviation of humidity ratio in dry evaporator iteration loop
  LOGICAL   :: LoopOn = .TRUE.     ! flag to control the loop iteration

  MaxIter = 30
  RF = 0.4d0
  Counter = 0
  Tolerance = 0.01d0
  werror = 0.0

  InletWetBulbCalc = InletWetBulb
  InletHumRatCalc = InletHumRat
  LoopOn = .TRUE.

!  DO WHILE (ABS(werror) .gt. Tolerance .OR. Counter == 0)
!   Get capacity modifying factor (function of inlet wetbulb & outside drybulb) for off-rated conditions
  DO WHILE(LoopOn)
    TotCapTempModFac1 = CurveValue(CCapFTemp1,InletWetBulbCalc,CondInletTemp)
!   Get capacity modifying factor (function of mass flow) for off-rated conditions
    TotCapAirFlowModFac1 = CurveValue(CCapAirFFlow1,AirMassFlowRatio)
    !Get capacity modifying factor (function of mass flow) for off-rated conditions
    TotCapWaterFlowModFac1 = CurveValue(CCapWaterFFlow1,WaterMassFlowRatio)

!   Get total capacity
    IF( NumSpeeds  < 2 ) THEN !ONLY ONE SPEED
        TotCapCalc = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1
        TotCapCalc1 = TotCapCalc
        TotCapCalc2 = 0.0
    ELSE
        TotCapTempModFac2 = CurveValue(CCapFTemp2,InletWetBulbCalc,CondInletTemp)
        TotCapAirFlowModFac2 = CurveValue(CCapAirFFlow2,AirMassFlowRatio)
        TotCapWaterFlowModFac2 = CurveValue(CCapWaterFFlow2,WaterMassFlowRatio)
        TotCapCalc1 = TotCapNom1 * TotCapAirFlowModFac1 * TotCapWaterFlowModFac1 * TotCapTempModFac1
        TotCapCalc2 = TotCapNom2 * TotCapAirFlowModFac2 * TotCapWaterFlowModFac2 * TotCapTempModFac2

        TotCapCalc = TotCapCalc2*SpeedRatio + (1.0 - SpeedRatio ) * TotCapCalc1

    END IF

!   Calculate apparatus dew point conditions using TotCap and CBF
    hDelta = TotCapCalc/AirMassFlow
    hADP = InletEnthalpy - hDelta/(1.d0-CBF)
    tADP = PsyTsatFnHPb(hADP,Pressure)
    wADP = PsyWFnTdbH(tADP,hADP)
    hTinwADP = PsyHFnTdbW(InletDryBulb,wADP)
    SHRCalc = MIN((hTinwADP-hADP)/(InletEnthalpy-hADP),1.d0)
!
!   Check for dry evaporator conditions (win < wadp)
!
    IF (wADP .gt. InletHumRatCalc .or. (Counter .ge. 1 .and. Counter .lt. MaxIter)) THEN
      If(InletHumRatCalc == 0.0)InletHumRatCalc=0.00001d0
      werror = (InletHumRatCalc - wADP)/InletHumRatCalc
!
!     Increase InletHumRatCalc at constant inlet air temp to find coil dry-out point. Then use the
!     capacity at the dry-out point to determine exiting conditions from coil. This is required
!     since the TotCapTempModFac doesn't work properly with dry-coil conditions.
!
      InletHumRatCalc = RF*wADP + (1.d0-RF)*InletHumRatCalc
      InletWetBulbCalc = PsyTwbFnTdbWPb(InletDryBulb,InletHumRatCalc,Pressure)
      Counter = Counter + 1
      IF (ABS(werror) .gt. Tolerance) THEN
        LoopOn = .TRUE. !go to 50   ! Recalculate with modified inlet conditions
      ELSE
        LoopOn = .FALSE.
      END IF
    ELSE
      LoopOn = .FALSE.
    END IF
  END DO

! END DO

!  Calculate full load output conditions
  IF (SHRCalc .gt. 1.d0 .OR. Counter .gt. 0) SHRCalc = 1.d0

  SHR = SHRCalc
  TotCap1 = TotCapCalc1
  TotCap2 = TotCapCalc2
  TotCapSpeed = TotCapCalc

 ! IF(SHR < 0.3) SHR = 0.3

  RETURN
END SUBROUTINE CalcTotCapSHR_VSWSHP

FUNCTION AdjustCBF(CBFNom,AirMassFlowRateNom,AirMassFlowRate) RESULT(CBFAdj)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl using Don Shirey's code
          !       DATE WRITTEN   September 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !    Adjust coil bypass factor for actual air flow rate.

          ! METHODOLOGY EMPLOYED:
          ! Uses relation CBF = exp(-NTU) whereNTU = A0/(m*cp). Relationship models the cooling coil
          ! as a heat exchanger with Cmin/Cmax = 0.
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT (IN) :: CBFNom                ! nominal coil bypass factor
  REAL(r64), INTENT (IN) :: AirMassFlowRateNom    ! nominal air mass flow rate [kg/s]
  REAL(r64), INTENT (IN) :: AirMassFlowRate       ! actual air mass flow rate [kg/s]
  REAL(r64)         :: CBFAdj                ! the result - the adjusted coil bypass factor


          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: A0  ! intermediate variable
  REAL(r64) :: ADiff  ! intermediate variable

  IF (CBFNom .gt. 0.0) THEN
     A0 = -log(CBFNom)*AirMassFlowRateNom
  ELSE
     A0 = 0.
  END IF
  ADiff=-A0/AirMassFlowRate
  IF (ADiff >= EXP_LowerLimit) THEN
     CBFAdj = exp(ADiff)
  ELSE
     CBFAdj = 0.0
  END IF

  RETURN
END FUNCTION AdjustCBF

FUNCTION CalcCBF(UnitType,UnitName,InletAirTemp,InletAirHumRat,TotCap,AirMassFlowRate,SHR) RESULT(CBF)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl using Don Shirey's code
          !       DATE WRITTEN   September 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculate the coil bypass factor for a coil given the total capacity at the entering conditions,
          ! air mass flow rate at the entering conditions, and the sensible heat ratio (SHR) at the
          ! entering conditions.

          ! METHODOLOGY EMPLOYED:
          ! calculate SlopeRated (deltahumrat/deltaT) using rated unit information provided by
          ! user. Then hunt along saturation curve of psychrometric chart until the slope of the line
          ! between the saturation point and rated inlet air humidity ratio and T is the same as SlopeRated.
          ! When the slopes are equal, then we have located the apparatus dewpoint of the coil at rated
          ! conditions. From this information, coil bypass factor is calculated.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataEnvironment, ONLY: StdRhoAir

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN) :: UnitType
  CHARACTER(len=*), INTENT (IN) :: UnitName
  REAL(r64), INTENT (IN) :: InletAirTemp          ! inlet air temperature [C]
  REAL(r64), INTENT (IN) :: InletAirHumRat        ! inlet air humidity ratio [kg water / kg dry air]
  REAL(r64), INTENT (IN) :: TotCap                ! total cooling  capacity [Watts]
  REAL(r64), INTENT (IN) :: AirMassFlowRate       ! the air mass flow rate at the given capacity [kg/s]
  REAL(r64), INTENT (IN) :: SHR                   ! sensible heat ratio at the given capacity and flow rate
  REAL(r64)         :: CBF                   ! the result - the coil bypass factor


          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER ::  RoutineName='CalcCBF'
  REAL(r64) :: SmallDifferenceTest=0.00000001d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: InletAirEnthalpy  ! Enthalpy of inlet air to evaporator at given conditions [J/kg]
  REAL(r64) :: DeltaH            ! Enthalpy drop across evaporator at given conditions [J/kg]
  REAL(r64) :: DeltaT            ! Temperature drop across evaporator at given conditions [C]
  REAL(r64) :: DeltaHumRat       ! Humidity ratio drop across evaporator at given conditions [kg/kg]
  REAL(r64) :: OutletAirTemp     ! Outlet dry-bulb temperature from evaporator at given conditions [C]
  REAL(r64) :: OutletAirEnthalpy ! Enthalpy of outlet air at given conditions [J/kg]
  REAL(r64) :: OutletAirHumRat   ! Outlet humidity ratio from evaporator at given conditions [kg/kg]
  REAL(r64) :: OutletAirRH       ! relative humidity of the outlet air
  REAL(r64) :: Error                ! Error term used in given coil bypass factor (CBF) calculations
  REAL(r64) :: ErrorLast            ! Error term, from previous iteration
  INTEGER :: Iter                 ! Iteration loop counter in CBF calculations
  INTEGER :: IterMax              ! Maximum number of iterations in CBF calculations
  REAL(r64) :: ADPTemp              ! Apparatus dewpoint temperature used in CBF calculations [C]
  REAL(r64) :: ADPHumRat            ! Apparatus dewpoint humidity used in CBF calculations [kg/kg]
  REAL(r64) :: ADPEnthalpy          ! Air enthalpy at apparatus dew point [J/kg]
  REAL(r64) :: DeltaADPTemp         ! Change in Apparatus Dew Point used in CBF calculations [C]
  REAL(r64) :: SlopeAtConds          ! Slope (DeltaHumRat/DeltaT) at given conditions
  REAL(r64) :: Slope                ! Calculated Slope used while hunting for Tadp
  REAL(r64) :: Tolerance            ! Convergence tolerance for CBF calculations
  REAL(r64) :: HTinHumRatOut        ! Air enthalpy at inlet air temp and outlet air humidity ratio [J/kg]
  LOGICAL :: CBFErrors=.false.    ! Set to true if errors in CBF calculation, fatal at end of routine

  DeltaH = 0.0
  DeltaT = 0.0
  DeltaHumRat = 0.0
  OutletAirTemp =  InletAirTemp
  OutletAirHumRat = InletAirHumRat
  SlopeAtConds = 0.0
  Slope = 0.0
  IterMax = 50
  CBFErrors=.false.

  DeltaH = TotCap/AirMassFlowRate
  InletAirEnthalpy = PsyHFnTdbW(InletAirTemp,InletAirHumRat)
  HTinHumRatOut = InletAirEnthalpy - (1.0d0-SHR)*DeltaH
  OutletAirHumRat = PsyWFnTdbH(InletAirTemp,HTinHumRatOut)
  DeltaHumRat = InletAirHumRat - OutletAirHumRat
  OutletAirEnthalpy = InletAirEnthalpy - DeltaH
  OutletAirTemp = PsyTdbFnHW(OutletAirEnthalpy,OutletAirHumRat)
!  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
!  Pressure will have to be pass into this subroutine to fix this one
  OutletAirRH = PsyRhFnTdbWPb(OutletAirTemp,OutletAirHumRat,StdBaroPress,'CalcCBF')
  IF (OutletAirRH .ge. 1.0d0) THEN
     CALL ShowSevereError ('For object = '//TRIM(UnitType)// ', name = "'//TRIM(UnitName)// '"')
     CALL ShowContinueError ('Calculated outlet air relative humidity greater than 1. The combination of')
     CALL ShowContinueError ('rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting')
     CALL ShowContinueError ('air conditions above the saturation curve. Possible fixes are to reduce the rated total cooling')
     CALL ShowContinueError ('capacity, increase the rated air volume flow rate, or reduce the rated sensible heat'// &
                             ' ratio for this coil.')
     CALL ShowContinueError ('If autosizing, it is recommended that all three of these values be autosized.')
     CALL ShowContinueError('...Inputs used for calculating cooling coil bypass factor.')
     CALL ShowContinueError('...Inlet Air Temperature     = '//TRIM(RoundSigDigits(InletAirTemp,2))//' C')
     CALL ShowContinueError('...Outlet Air Temperature    = '//TRIM(RoundSigDigits(OutletAirTemp,2))//' C')
     CALL ShowContinueError('...Inlet Air Humidity Ratio  = '//TRIM(RoundSigDigits(InletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Outlet Air Humidity Ratio = '//TRIM(RoundSigDigits(OutletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Total Cooling Capacity used in calculation = '//TRIM(RoundSigDigits(TotCap,2))//' W')
     CALL ShowContinueError('...Air Mass Flow Rate used in calculation     = '//TRIM(RoundSigDigits(AirMassFlowRate,6))//' kg/s')
     CALL ShowContinueError('...Air Volume Flow Rate used in calculation   = '// &
       TRIM(RoundSigDigits(AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName),6))//' m3/s')
     IF(TotCap .GT. 0.d0)THEN
       IF (((MinRatedVolFlowPerRatedTotCap - AirMassFlowRate/ &
            PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap) > SmallDifferenceTest).OR. &
           ((AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap &
             - MaxRatedVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
         CALL ShowContinueError('...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = '// &
                                TRIM(RoundSigDigits(AirMassFlowRate/ &
                                PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap,7))//' m3/s/W')
       END IF
     END IF
     CALL ShowContinueErrorTimeStamp(' ')
     CALL ShowFatalError ('Check and revise the input data for this coil before rerunning the simulation.')
  END IF
  DeltaT = InletAirTemp - OutletAirTemp
  IF (DeltaT .LE. 0.0d0) THEN
     CALL ShowSevereError ('For object = '//TRIM(UnitType)// ', name = "'//TRIM(UnitName)// '"')
     CALL ShowContinueError ('Calculated coil delta T is less than or equal to 0. The combination of')
     CALL ShowContinueError ('rated air volume flow rate, total cooling capacity and sensible heat ratio yields coil exiting')
     CALL ShowContinueError ('air conditions that are not reasonable. Possible fixes are to adjust the rated total cooling')
     CALL ShowContinueError ('capacity, rated air volume flow rate, or rated sensible heat'// &
                             ' ratio for this coil.')
     CALL ShowContinueError ('If autosizing, it is recommended that all three of these values be autosized.')
     CALL ShowContinueError('...Inputs used for calculating cooling coil bypass factor.')
     CALL ShowContinueError('...Inlet Air Temperature     = '//TRIM(RoundSigDigits(InletAirTemp,2))//' C')
     CALL ShowContinueError('...Outlet Air Temperature    = '//TRIM(RoundSigDigits(OutletAirTemp,2))//' C')
     CALL ShowContinueError('...Inlet Air Humidity Ratio  = '//TRIM(RoundSigDigits(InletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Outlet Air Humidity Ratio = '//TRIM(RoundSigDigits(OutletAirHumRat,6))//' kgWater/kgDryAir')
     CALL ShowContinueError('...Total Cooling Capacity used in calculation = '//TRIM(RoundSigDigits(TotCap,2))//' W')
     CALL ShowContinueError('...Air Mass Flow Rate used in calculation     = '//TRIM(RoundSigDigits(AirMassFlowRate,6))//' kg/s')
     CALL ShowContinueError('...Air Volume Flow Rate used in calculation   = '// &
       TRIM(RoundSigDigits(AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName),6))//' m3/s')
     IF(TotCap .GT. 0.d0)THEN
       IF (((MinRatedVolFlowPerRatedTotCap - AirMassFlowRate/ &
            PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap) > SmallDifferenceTest).OR. &
           ((AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap &
             - MaxRatedVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
         CALL ShowContinueError('...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = '// &
                                TRIM(RoundSigDigits(AirMassFlowRate/ &
                                PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap,7))//' m3/s/W')
       END IF
     END IF
     CALL ShowContinueErrorTimeStamp(' ')
     CALL ShowFatalError ('Check and revise the input data for this coil before rerunning the simulation.')
  END IF
  ! Calculate slope at given conditions
  IF (DeltaT .gt. 0.0) SlopeAtConds = DeltaHumRat/DeltaT

!  IF (SlopeAtConds .le. .0000001d0 .or. OutletAirHumRat .le. 0.) THEN
  IF (SlopeAtConds .lt. 0.0d0 .or. OutletAirHumRat .le. 0.) THEN
!   Invalid conditions, slope can't be less than zero (SHR > 1) or
!   outlet air humidity ratio can't be less than zero.
    CALL ShowSevereError(TRIM(UnitType)//' "'//TRIM(UnitName)//'"')
    CALL ShowContinueError('...Invalid slope or outlet air condition when calculating cooling coil bypass factor.')
    CALL ShowContinueError('...Slope = '//TRIM(RoundSigDigits(SlopeAtConds,8)))
    CALL ShowContinueError('...Inlet Air Temperature     = '//TRIM(RoundSigDigits(InletAirTemp,2))//' C')
    CALL ShowContinueError('...Outlet Air Temperature    = '//TRIM(RoundSigDigits(OutletAirTemp,2))//' C')
    CALL ShowContinueError('...Inlet Air Humidity Ratio  = '//TRIM(RoundSigDigits(InletAirHumRat,6))//' kgWater/kgDryAir')
    CALL ShowContinueError('...Outlet Air Humidity Ratio = '//TRIM(RoundSigDigits(OutletAirHumRat,6))//' kgWater/kgDryAir')
    CALL ShowContinueError('...Total Cooling Capacity used in calculation = '//TRIM(RoundSigDigits(TotCap,2))//' W')
    CALL ShowContinueError('...Air Mass Flow Rate used in calculation     = '//TRIM(RoundSigDigits(AirMassFlowRate,6))//' kg/s')
     CALL ShowContinueError('...Air Volume Flow Rate used in calculation   = '// &
       TRIM(RoundSigDigits(AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName),6))//' m3/s')
    IF(TotCap .GT. 0.d0)THEN
       IF (((MinRatedVolFlowPerRatedTotCap - AirMassFlowRate/ &
            PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap) > SmallDifferenceTest).OR. &
           ((AirMassFlowRate/PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap &
             - MaxRatedVolFlowPerRatedTotCap) > SmallDifferenceTest)) THEN
         CALL ShowContinueError('...Air Volume Flow Rate per Watt of Rated Cooling Capacity is also out of bounds at = '// &
                                TRIM(RoundSigDigits(AirMassFlowRate/ &
                                PsyRhoAirFnPbTdbW(StdBaroPress,InletAirTemp,InletAirHumRat,RoutineName)/TotCap,7))//' m3/s/W')
       END IF
    END IF
    CALL ShowContinueErrorTimeStamp(' ')
    CBFErrors=.true.
  ELSE

!   First guess for Tadp is outlet air dew point
!  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
!  Pressure will have to be pass into this subroutine to fix this one
    ADPTemp = PsyTdpFnWPb(OutletAirHumRat,StdBaroPress)

    Tolerance = 1.         ! initial conditions for iteration
    ErrorLast = 100.d0
    Iter = 0
    DeltaADPTemp = 5.0d0
    DO WHILE ((Iter .le. IterMax).and.(Tolerance .gt. .001d0))
!     Do for IterMax iterations or until the error gets below .1%
      IF (Iter .gt. 0) ADPTemp = ADPTemp + DeltaADPTemp
      Iter = Iter + 1

!     Find new slope using guessed Tadp

!  Eventually inlet air conditions will be used in DX Coil, these lines are commented out and marked with this comment line
!  Pressure will have to be pass into this subroutine to fix this one
      ADPHumRat = PsyWFnTdpPb(ADPTemp,StdBaroPress)
      Slope     = (InletAirHumRat-ADPHumRat)/(InletAirTemp-ADPTemp)

!     check for convergence (slopes are equal to within error tolerance)

      Error     = (Slope-SlopeAtConds)/SlopeAtConds
      IF ((Error .gt. 0.).and.(ErrorLast .lt. 0.)) DeltaADPTemp = -DeltaADPTemp/2.d0
      IF ((Error .lt. 0.).and.(ErrorLast .gt. 0.)) DeltaADPTemp = -DeltaADPTemp/2.d0
      ErrorLast = Error

      Tolerance = ABS(Error)

    END DO

!   Calculate Bypass Factor from Enthalpies

    InletAirEnthalpy=PsyHFnTdbW(InletAirTemp,InletAirHumRat)
    OutletAirEnthalpy=PsyHFnTdbW(OutletAirTemp,OutletAirHumRat)
    ADPEnthalpy=PsyHFnTdbW(ADPTemp,ADPHumRat)
    CBF = (OutletAirEnthalpy-ADPEnthalpy)/(InletAirEnthalpy-ADPEnthalpy)
    IF (Iter .gt. IterMax) THEN
      CALL ShowSevereError(TRIM(UnitType)//' "'//TRIM(UnitName)//&
                          '" -- coil bypass factor calculation did not converge after max iterations.')
      CALL ShowContinueError('The RatedSHR of ['//TRIM(RoundSigDigits(SHR,3))//  &
         '], entered by the user or autosized (see *.eio file),')
      CALL ShowContinueError('may be causing this. The line defined by the coil rated inlet air conditions')
      CALL ShowContinueError('(26.7C drybulb and 19.4C wetbulb) and the RatedSHR (i.e., slope of the line) must intersect')
      CALL ShowContinueError('the saturation curve of the psychrometric chart. If the RatedSHR is too low, then this')
      CALL ShowContinueError('intersection may not occur and the coil bypass factor calculation will not converge.')
      CALL ShowContinueError('If autosizing the SHR, recheck the design supply air humidity ratio and design supply air')
      CALL ShowContinueError('temperature values in the Sizing:System and Sizing:Zone objects. In general, the temperatures')
      CALL ShowContinueError('and humidity ratios specified in these two objects should be the same for each system')
      CALL ShowContinueError('and the zones that it serves.')
      CALL ShowContinueErrorTimeStamp(' ')
      CBFErrors=.true.  ! Didn't converge within MaxIter iterations
    ENDIF
    IF (CBF .lt. 0.) THEN
      CALL ShowSevereError(TRIM(UnitType)//' "'//TRIM(UnitName)//'" -- negative coil bypass factor calculated.')
      CALL ShowContinueErrorTimeStamp(' ')
      CBFErrors=.true. ! Negative CBF not valid
    ENDIF
  END IF

! Show fatal error for specific coil that caused a CBF error
  IF (CBFErrors) THEN
    CALL ShowFatalError(TRIM(UnitType)//' "'//TRIM(UnitName)//&
                        '" Errors found in calculating coil bypass factors')
  END IF

  RETURN
END FUNCTION CalcCBF

!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
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

END MODULE WatertoAirMulSpeeddHP


