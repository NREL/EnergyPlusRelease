MODULE IceThermalStorage  ! Ice Storage Module

          ! MODULE INFORMATION:
          !       AUTHOR         Pyeongchan Ihm
          !       DATE WRITTEN   April 2002
          !       MODIFIED       Modified Refined model, added Simple model, by Guo Zhou, Oct 2002
          !                      Remove chiller, make just a storage tank, Michael J. Witte, Sep 2005
          !                      Added detailed ice storage model, Rick Strand, Feb 2006
          !                      B. Griffith, Sept 2010, plant upgrades, fluid properties
          !                      Enhancements to detailed ice storage model, Rick Strand, Aug 2012
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module simulates the performance of Ice Thermal Storage

          ! METHODOLOGY EMPLOYED:
          ! Once the PlantLoopManager determines that the Ice Thermal Storage
          ! is available to meet a loop cooling demand, it calls SimIceStorage
          ! which in turn calls the appropriate Ice Thermal Storage model.

          ! REFERENCES: Dion J. King, ASHRAE Transactions v104, pt1, 1998.

          ! OTHER NOTES: na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE CurveManager
  USE DataLoopNode
  USE DataGlobals,     ONLY: WarmupFlag, BeginDayFlag, EndDayFlag, TimeStepZone, SecInHour, &
                             HourOfDay,TimeStep,ScheduleAlwaysOn
  USE DataInterfaces,  ONLY: ShowWarningError, ShowContinueError, ShowSevereError, ShowFatalError, &
                             ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
  USE DataEnvironment, ONLY : OutWetBulbTemp,OutDryBulbTemp  ! This value is used to model Cooling Tower.  Twb + 2[degF]
  USE DataHVACGlobals
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE

  PRIVATE

          ! MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER :: cIceStorageSimple   = 'ThermalStorage:Ice:Simple'
  CHARACTER(len=*), PARAMETER :: cIceStorageDetailed = 'ThermalStorage:Ice:Detailed'

  INTEGER, PARAMETER :: IceStorageType_Simple   = 1
  INTEGER, PARAMETER :: IceStorageType_Detailed = 2

  INTEGER, PARAMETER :: DetIceInsideMelt  = 1    ! Inside melt system--charge starting with bare coil
  INTEGER, PARAMETER :: DetIceOutsideMelt = 2    ! Outside melt system--charge from existing ice layer on coil

       ! ITS parameter
  REAL(r64), PARAMETER :: FreezTemp   = 0.0d0      ! Water freezing Temperature, 0[C]
  REAL(r64), PARAMETER :: FreezTempIP  = 32.0d0    ! Water freezing Temperature, 32[F]
  REAL(r64), PARAMETER :: TimeInterval = 3600.0d0  ! Time Interval (1 hr) [s]
  INTEGER, PARAMETER :: ITSType_IceOnCoilInternal = 1
  INTEGER, PARAMETER :: ITSType_IceOnCoilExternal = 2
       ! Conversion parameter
  REAL(r64), PARAMETER :: EpsLimitForX         = 0.0d0 !0.02  ! See Dion's code as eps1
  REAL(r64), PARAMETER :: EpsLimitForDisCharge = 0.0d0 !0.20  ! See Dion's code as eps2
  REAL(r64), PARAMETER :: EpsLimitForCharge    = 0.0d0 !0.20  ! See Dion's code as eps3

      !variable used by simple model
  REAL(r64), PARAMETER :: Delta = 0.005d0
  REAL(r64), PARAMETER :: PLRmin = 0.1d0
  REAL(r64), PARAMETER :: Pa =0.088065d0
  REAL(r64), PARAMETER :: Pb =1.137742d0
  REAL(r64), PARAMETER :: Pc =-0.225806d0
  REAL(r64), PARAMETER :: Tref = 85.d0                !F
  REAL(r64), PARAMETER :: Tcharge = 1.d0             !F
  REAL(r64), PARAMETER :: Tdischarge = 5.d0          !F

          ! Parameter used by the Detailed Ice Storage Model
  REAL(r64), PARAMETER :: DeltaTofMin = 0.5d0  ! Minimum allowed outlet side temperature difference [C]
                                        ! This is (Tout - Tfreezing)
  REAL(r64), PARAMETER :: DeltaTifMin = 1.0d0  ! Minimum allowed inlet side temperature difference [C]
                                        ! This is (Tin - Tfreezing)

          ! DERIVED TYPE DEFINITIONS
     ! TYPE ITSSetCap is used for information of ITS plant in Loop, Brach, and Components.
!  TYPE ITSSetCapData
!    LOGICAL :: ITSFlag    = .FALSE.
!    INTEGER :: LoopNum    =0
!    INTEGER :: BranchNum  =0
!    INTEGER :: CompNum    =0
!  END TYPE ITSSetCapData

  TYPE IceStorageMapping
         ! Input data
    CHARACTER(len=MaxNameLength) :: Name       =' '   ! User identifier
    CHARACTER(len=MaxNameLength) :: StorageType=' '
    INTEGER                      :: StorageType_Num = 0
    INTEGER                      :: LocalEqNum      = 0
  END TYPE


  TYPE IceStorageSpecs
         ! Input data
    CHARACTER(len=MaxNameLength) :: Name     =' '   ! User identifier
    CHARACTER(len=MaxNameLength) :: ITSType  =' '   ! Ice Thermal Storage Type
    INTEGER                      :: ITSType_Num = 0 ! Storage Type as number (IceOnCoilInternal,IceOnCoilExternal)
    INTEGER                      :: MapNum   = 0    ! Number to Map structure
    INTEGER :: UratePtr             =0    ! Charging/Discharging SchedulePtr: u value schedule
    REAL(r64)    :: ITSNomCap            =0.0d0  ! Design nominal capacity of Ice Thermal Storage [J] (user input in GJ)
    INTEGER :: PltInletNodeNum      =0    ! Node number on the inlet side of the plant
    INTEGER :: PltOutletNodeNum     =0     ! Node number on the outlet side of the plant
    !loop topology variables
    INTEGER                      :: LoopNum            =0
    INTEGER                      :: LoopSideNum        =0
    INTEGER                      :: BranchNum          =0
    INTEGER                      :: CompNum            =0
    REAL(r64)                    :: DesignMassFlowRate = 0.d0
  END TYPE IceStorageSpecs

  TYPE DetailedIceStorageData
          ! Input data
    CHARACTER(len=MaxNameLength) :: Name = ' '                  ! User identifier
    CHARACTER(len=MaxNameLength) :: ScheduleName = ' '          ! User identifier
    INTEGER                      :: ScheduleIndex = 0           ! Plant inlet node number for ice storage unit
    REAL(r64)                    :: NomCapacity = 0.0d0           ! Design storage capacity of Ice Thermal Storage system [W-hr]
                                                                ! (User input for this parameter in GJ--need to convert to W-hr)
    INTEGER                      :: PlantInNodeNum = 0          ! Plant inlet node number for ice storage unit
    INTEGER                      :: PlantOutNodeNum = 0         ! Plant outlet node number for ice storage unit
    INTEGER                      :: PlantLoopNum       = 0
    INTEGER                      :: PlantLoopSideNum   = 0
    INTEGER                      :: PlantBranchNum     = 0
    INTEGER                      :: PlantCompNum       = 0
    REAL(r64)                    :: DesignMassFlowRate = 0.d0
    INTEGER                      :: MapNum   = 0                ! Number to Map structure
    CHARACTER(len=MaxNameLength) :: DischargeCurveType = ' '    ! Type of discharging equation entered by user (QuadraticLinear)
    CHARACTER(len=MaxNameLength) :: DischargeCurveName = ' '    ! Curve name for discharging (used to find the curve index)
    INTEGER                      :: DischargeCurveNum = 0       ! Curve index for discharging
    CHARACTER(len=MaxNameLength) :: ChargeCurveType = ' '       ! Type of charging equation entered by user (QuadraticLinear)
    CHARACTER(len=MaxNameLength) :: ChargeCurveName = ' '       ! Curve name for charging (used to find the curve index)
    INTEGER                      :: ChargeCurveNum = 0          ! Curve index for charging
    REAL(r64)                    :: CurveFitTimeStep = 1.0d0      ! Time step used to generate performance data [hours]
    REAL(r64)                    :: DischargeParaElecLoad = 0.0d0 ! Parasitic electric load duing discharging [dimensionless]
                                                                ! (This is multiplied by the tank capacity to obtain elec consump)
    REAL(r64)                    :: ChargeParaElecLoad = 0.0d0    ! Parasitic electric load duing charging [dimensionless]
                                                                ! (This is multiplied by the tank capacity to obtain elec consump)
    REAL(r64)                    :: TankLossCoeff = 0.0d0         ! Fraction of total storage capacity lost per hour [1/hours]
    REAL(r64)                    :: FreezingTemp = 0.0d0          ! Freezing/melting temperature of ice storage unit [C]
          ! Reporting data
    REAL(r64)                    :: CompLoad = 0.0d0              ! load requested by plant [W]
    REAL(r64)                    :: IceFracChange = 0.0d0         ! Change in fraction of ice stored during the time step [fraction]
    REAL(r64)                    :: IceFracRemaining = 1.0d0      ! Fraction of ice remaining in storage [fraction]
    CHARACTER(len=MaxNameLength) :: ThawProcessIndicator = ' '  ! User input determining whether system is inside or outside melt
    INTEGER                      :: ThawProcessIndex = 0        ! Conversion of thaw process indicator to integer index
    REAL(r64)                    :: IceFracOnCoil = 1.0d0       ! Fraction of ice on the coil (affects charging) [fraction]
    REAL(r64)                    :: DischargingRate = 0.0d0       ! Rate at which energy is being added (thawing) to ice unit [W]
    REAL(r64)                    :: DischargingEnergy = 0.0d0     ! Total energy added to the ice storage unit [J]
    REAL(r64)                    :: ChargingRate = 0.0d0          ! Rate at which energy is removed (freezing) to ice unit [W]
    REAL(r64)                    :: ChargingEnergy = 0.0d0        ! Total energy removed from ice storage unit [J]
    REAL(r64)                    :: MassFlowRate = 0.0d0          ! Total mass flow rate to component [kg/s]
    REAL(r64)                    :: BypassMassFlowRate = 0.0d0    ! Mass flow rate that bypasses the ice unit locally [kg/s]
    REAL(r64)                    :: TankMassFlowRate = 0.0d0      ! Mass flow rate through the ice storage unit [kg/s]
    REAL(r64)                    :: InletTemp = 0.0d0             ! Component inlet temperature (same as bypass temperature) [C]
    REAL(r64)                    :: OutletTemp = 0.0d0            ! Component outlet temperature (blended) [C]
    REAL(r64)                    :: TankOutletTemp = 0.0d0        ! Ice storage unit outlet temperature [C]
    REAL(r64)                    :: ParasiticElecRate = 0.0d0     ! Parasitic electrical energy rate consumed by ice storage [W]
    REAL(r64)                    :: ParasiticElecEnergy = 0.0d0   ! Total parasitic electrical energy consumed by ice storage [J]
    INTEGER                      :: DischargeIterErrors = 0     ! Number of max iterations exceeded errors during discharging
    INTEGER                      :: DischargeErrorCount = 0     ! Index for error counting routine
    INTEGER                      :: ChargeIterErrors = 0        ! Number of max iterations exceeded errors during charging
    INTEGER                      :: ChargeErrorCount = 0        ! Index for error counting routine
  END TYPE DetailedIceStorageData

  TYPE ReportVars
    REAL(r64)    :: MyLoad           =0.0d0 ! load requested by plant [W]
    REAL(r64)    :: U                =0.0d0  ! [fraction]
    REAL(r64)    :: Urate            =0.0d0  ! [fraction]
    REAL(r64)    :: IceFracRemain    =0.0d0  ! Fraction of ice remaining in storage [fraction]
    REAL(r64)    :: ITSCoolingRate   =0.0d0  ! [W]
    REAL(r64)    :: ITSCoolingEnergy =0.0d0  ! [J]
    REAL(r64)    :: ITSChargingRate  =0.0d0  ! [W]
    REAL(r64)    :: ITSChargingEnergy=0.0d0  ! [J]
    REAL(r64)    :: ITSmdot          =0.0d0  ! [kg/s]
    REAL(r64)    :: ITSInletTemp     =0.0d0  ! [C]
    REAL(r64)    :: ITSOutletTemp    =0.0d0  ! [C]
  END TYPE ReportVars

 ! TYPE (ITSSetCapData), SAVE                   :: ITSSetCap=ITSSetCapData(.false.,0,0,0)
  TYPE (IceStorageSpecs), ALLOCATABLE, DIMENSION(:) :: IceStorage         ! dimension to number of machines
  TYPE (ReportVars), ALLOCATABLE,DIMENSION(:)        :: IceStorageReport  ! dimension to number of machines
  TYPE (DetailedIceStorageData), ALLOCATABLE, DIMENSION(:) :: DetIceStor  ! Derived type for detailed ice storage model
  TYPE (IceStorageMapping), ALLOCATABLE, DIMENSION(:) :: IceStorageTypeMap


          ! MODULE VARIABLE DECLARATIONS:
  LOGICAL :: ResetXForITSFlag =.false.

       ! Input data
  REAL(r64)    :: ITSNomCap        =0.0d0 ! Design nominal capacity of Ice Thermal Storage [J] (user input in GJ)
  INTEGER :: InletNodeNum     =0   ! Node number on the inlet side of the plant
  INTEGER :: OutletNodeNum    =0   ! Node number on the inlet side of the plant

       ! ITS numbers and FoundOrNot
  INTEGER :: IceNum     =0
  INTEGER :: NumIceStorages=0
  LOGICAL :: IceStorageNotFound=.false.
  INTEGER :: NumDetIceStorages = 0
  INTEGER :: TotalIceStorages = 0
       ! ITS UAice and HLoss
  REAL(r64)    :: UAIceCh       =0.0d0     ! Charging Ice Thermal Storage overall heat transfer coefficient [W/C]
  REAL(r64)    :: UAIceDisCh    =0.0d0     ! Discharging Ice Thermal Storage overall heat transfer coefficient [W/C]
  REAL(r64)    :: HLoss         =0.0d0     ! ITS Heat Loss
       ! ITS State
  REAL(r64)    :: XCurIceFrac   =0.0d0     ! Current Fraction of Ice Thermal Storage remaining [fraction]
  REAL(r64)    :: U             =0.0d0     ! Adjusted input U after reading U Schedule [fraction]
  REAL(r64)    :: Urate   =0.0d0 ! Final Urate adjusted Urate based on Error protection (I) [fraction] by HOUR
       ! ITS status information
  REAL(r64)    :: ITSMassFlowRate =0.0d0       ! ITS water mass flow rate [kg/s]
  REAL(r64)    :: ITSInletTemp    =0.0d0       ! ITS inlet water temperature [C]
  REAL(r64)    :: ITSOutletTemp   =0.0d0       ! ITS outlet water temperature [C]
  REAL(r64)    :: ITSOutletSetPointTemp =0.0d0 ! ITS outlet water temperature setpoint [C]
  REAL(r64)    :: ITSCoolingRate  =0.0d0       ! ITS Discharge(-)/Charge(+) rate [W]
  REAL(r64)    :: ITSCoolingEnergy=0.0d0
  REAL(r64)    :: ChillerOutletTemp=0.0d0     ! Chiller outlet brine temperature [C]
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE
       ! General routine
  PUBLIC  SimIceStorage
  PUBLIC  UpdateIceFractions
  PRIVATE CalcIceStorageCapacity
  PRIVATE GetIceStorageInput
  PRIVATE UpdateNode
  PRIVATE RecordOutput
  PRIVATE CalcIceStorageDormant
  PRIVATE CalcIceStorageCharge
  PRIVATE CalcQiceChargeMaxByChiller
  PRIVATE CalcQiceChargeMaxByITS
  PRIVATE CalcIceStorageDischarge
  PRIVATE CalcQiceDischageMax
  PRIVATE CalcUAIce
  PRIVATE InitDetailedIceStorage
  PRIVATE InitSimpleIceStorage
  PRIVATE SimDetailedIceStorage
  PRIVATE CalcDetIceStorLMTDstar
  PRIVATE UpdateDetailedIceStorage
  PRIVATE ReportDetailedIceStorage


  CONTAINS

!*************************************************************************
SUBROUTINE SimIceStorage(IceStorageType,IceStorageName,CompIndex,RunFlag,FirstIteration, &
                           InitLoopEquip,MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TimeStepSys           ! [hr]
  USE InputProcessor,  ONLY: FindItemInList
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataGlobals,     ONLY: BeginEnvrnFlag, WarmupFlag
  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataPlant,       ONLY: PlantLoop, SingleSetpoint, DualSetpointDeadband


  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*)       :: IceStorageType
  CHARACTER(len=*)       :: IceStorageName
  INTEGER, INTENT(INOUT) :: CompIndex
  LOGICAL, INTENT(IN)    :: RunFlag
  LOGICAL                :: FirstIteration
  LOGICAL, INTENT(INOUT) :: InitLoopEquip
  REAL(r64), INTENT(INOUT)    :: MyLoad
  REAL(r64)                :: DemandMdot
  REAL(r64)                :: TempIn
  REAL(r64)                :: TempSetPt
  REAL(r64)                :: MyLoad2
  REAL(r64)                :: MaxCap
  REAL(r64)                :: MinCap
  REAL(r64)                :: OptCap
  REAL(r64)                :: Cp !local plant fluid specific heat

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: FirstTime = .TRUE.
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.
  INTEGER :: IceStorageNum

          ! FLOW

!  Set initialization flags
!  Allow ice to build up during warmup?
!  IF( (BeginEnvrnFlag) .OR. (WarmupFlag) ) THEN
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    ResetXForITSFlag = .TRUE.
    MyEnvrnFlag = .FALSE.
  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag = .TRUE.
  END IF

  IF (FirstTime) THEN
    CALL GetIceStorageInput
    FirstTime = .FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    IceStorageNum = FindItemInList(IceStorageName,IceStorageTypeMap%Name,TotalIceStorages)
    IF (IceStorageNum == 0) THEN
      CALL ShowFatalError('SimIceStorage: Unit not found='//TRIM(IceStorageName))
    ENDIF
    CompIndex=IceStorageNum
  ELSE
    IceStorageNum=CompIndex
    IF (IceStorageNum > TotalIceStorages .or. IceStorageNum < 1) THEN
      CALL ShowFatalError('SimIceStorage:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(IceStorageNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(TotalIceStorages))//  &
                          ', Entered Unit name='//TRIM(IceStorageName))
    ENDIF
    IF (CheckEquipName(IceStorageNum)) THEN
      IF (IceStorageName /= IceStorageTypeMap(IceStorageNum)%Name) THEN
        CALL ShowFatalError('SimIceStorage: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(IceStorageNum))// &
                            ', Unit name='//TRIM(IceStorageName)//', stored Unit Name for that index='//  &
                            TRIM(IceStorageTypeMap(IceStorageNum)%Name))
      ENDIF
      CheckEquipName(IceStorageNum)=.false.
    ENDIF
  ENDIF


  SELECT CASE (IceStorageTypeMap(IceStorageNum)%StorageType_Num)

    CASE (IceStorageType_Simple)

      !------------------------------------------------------------------------
      ! READING INPUT when first calling SimIceStorage
      !------------------------------------------------------------------------
      IceNum=IceStorageTypeMap(IceStorageNum)%LocalEqNum

      CALL InitSimpleIceStorage

      IF (InitLoopEquip) THEN

             ! Find IceStorage Number
             ! Assign ice thermal storage data to Module variables by each Ice thermal storage
        ITSNomCap            = IceStorage(IceNum)%ITSNomCap
        InletNodeNum         = IceStorage(IceNum)%PltInletNodeNum
        OutletNodeNum        = IceStorage(IceNum)%PltOutletNodeNum

        RETURN
      END IF  ! End Of InitLoopEquip

      !------------------------------------------------------------------------
      ! FIRST PROCESS (MyLoad = 0.0 as IN)
      ! At this moment as first calling of ITS, ITS provide ONLY MaxCap/OptCap/MinCap.
      !------------------------------------------------------------------------
      ! First process is in subroutine CalcIceStorageCapacity(MaxCap,MinCap,OptCap) shown bellow.

      !------------------------------------------------------------------------
      ! SECOND PROCESS (MyLoad is provided by E+ based on MaxCap/OptCap/MinCap)
      !------------------------------------------------------------------------
           ! Below routines are starting when second calling.
           ! After previous return, MyLoad is calculated based on MaxCap, OptCap, and MinCap.
           ! Then PlandSupplySideManager provides MyLoad to simulate Ice Thermal Storage.
           ! The process will be decided based on sign(+,-,0) of input U.

! MJW 19 Sep 2005 - New approach - calculate MyLoad locally from inlet node temp
!                   and outlet node setpoint until MyLoad that is passed in behaves well

!DSU? can we now use MyLoad? lets not yet to try to avoid scope creep

      TempIn     = Node(InletNodeNum)%Temp
      SELECT CASE (PlantLoop(IceStorage(IceNum)%LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetPoint)
        TempSetPt  = Node(OutletNodeNum)%TempSetPoint
      CASE (DualSetPointDeadBand)
        TempSetPt  = Node(OutletNodeNum)%TempSetPointHi
      END SELECT
      DemandMdot = IceStorage(IceNum)%DesignMassFlowRate

      Cp = GetSpecificHeatGlycol(PlantLoop(IceStorage(IceNum)%LoopNum)%FluidName, &
                                 TempIn, &
                                 PlantLoop(IceStorage(IceNum)%LoopNum)%FluidIndex, &
                                 'SimIceStorage')

      MyLoad2    = (DemandMdot* Cp *(TempIn - TempSetPt))
      MyLoad     = MyLoad2


!     Set fraction of ice remaining in storage
      XCurIceFrac = IceStorageReport(IceNum)%IceFracRemain

      !***** Dormant Process for ITS *****************************************
      !************************************************************************
!        IF( U .EQ. 0.0 ) THEN
      IF(( MyLoad2 .EQ. 0.0d0) .OR. (DemandMdot .EQ. 0.0d0)) THEN
        CALL CalcIceStorageDormant(IceStorageType_Simple,IceNum)

      !***** Charging Process for ITS *****************************************
      !************************************************************************
!        ELSE IF( U .GT. 0.0 ) THEN
      ELSE IF( MyLoad2 .LT. 0.0d0 ) THEN

!             Call CalcIceStorageCapacity from here - MJW - 19 Sep 2005
        CALL CalcIceStorageCapacity(IceStorageType_Simple,MaxCap,MinCap,OptCap)

        CALL CalcIceStorageCharge(IceStorageType_Simple,IceNum)

      !***** Discharging Process for ITS *****************************************
      !************************************************************************
!        ELSE IF( U .LT. 0.0 ) THEN
      ELSE IF( MyLoad2 .GT. 0.0d0 ) THEN
!             Call CalcIceStorageCapacity from here - MJW - 19 Sep 2005
        CALL CalcIceStorageCapacity(IceStorageType_Simple,MaxCap,MinCap,OptCap)

        CALL CalcIceStorageDischarge(IceStorageType_Simple,IceNum,MyLoad,Runflag,FirstIteration,MaxCap)
      END IF  ! Based on input of U value, deciding Dormant/Charge/Discharge process

           ! Update Node properties: mdot and Temperature
      CALL UpdateNode(MyLoad2,RunFlag,IceNum)

           ! Update report variables.
      CALL RecordOutput(IceNum,MyLoad2,RunFlag)
    !--------------------------------------------------------------------------
    !        Ali's TES modle   Itegrated by ZG  Oct. 2002
    !---------------------------------------------------------------------------

    CASE (IceStorageType_Detailed)

      IceNum=IceStorageTypeMap(IceStorageNum)%LocalEqNum
          ! Read input when first calling SimIceStorage
      IF (InitLoopEquip) THEN
        RETURN
      END IF  ! End Of InitLoopEquip

      CALL InitDetailedIceStorage   ! Initialize detailed ice storage

      CALL SimDetailedIceStorage    ! Simulate detailed ice storage

      CALL UpdateDetailedIceStorage ! Update detailed ice storage

      CALL ReportDetailedIceStorage ! Report detailed ice storage

    CASE DEFAULT
      CALL ShowFatalError('Specified IceStorage not found in SimIceStorage'//TRIM(IceStorageType))
  END SELECT


  RETURN
END SUBROUTINE SimIceStorage

SUBROUTINE SimDetailedIceStorage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main simulation subroutine for the detailed
          ! ice storage model.

          ! METHODOLOGY EMPLOYED:
          ! Based on whether the unit is dormant, in charging mode, or in discharging
          ! mode, the code either passes the flow through the bypass, through the tank,
          ! or both.  This depends on the temperature relative to the setpoint temperature
          ! and other features of the model.  The model itself is a LMTD model that uses
          ! performance curve fits that are quadratic in fraction charged/discharged and
          ! linear in LMTD for the calculation of Q.  The equations are actually non-
          ! dimensionalized.

          ! REFERENCES:
          ! Ice Storage Component Model Proposal (Revised).doc by Rick Strand (Dec 2005/Jan 2006)

          ! USE STATEMENTS:
  USE CurveManager,    ONLY : CurveValue
  USE ScheduleManager, ONLY : GetCurrentScheduleValue
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE DataPlant,       ONLY : PlantLoop, CommonPipe_TwoWay,  SingleSetpoint, DualSetpointDeadband
  USE PlantUtilities,  ONLY : SetComponentFlowRate
  USE DataBranchAirLoopPlant, ONLY : MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: MaxIterNum = 100            ! Maximum number of internal iterations for ice storage solution
  REAL(r64), PARAMETER :: SmallestLoad       = 0.1d0    ! Smallest load to actually run the ice storage unit [Watts]
  REAL(r64), PARAMETER :: TankDischargeToler = 0.001d0  ! Below this fraction, there is nothing left to discharge
  REAL(r64), PARAMETER :: TankChargeToler    = 0.999d0  ! Above this fraction, we don't have anything left to charge
  REAL(r64), PARAMETER :: TemperatureToler   = 0.1d0    ! Temperature difference between iterations that indicates convergence [C]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: ActualLoad     ! Actual load on the ice storage unit [W]
  REAL(r64)    :: AvgFracCharged ! Average fraction charged for the current time step
  REAL(r64)    :: ChargeFrac     ! Fraction of tank to be charged in the current time step
  INTEGER :: IterNum        ! Iteration number
  REAL(r64)    :: LMTDstar       ! Non-dimensional log mean temperature difference of ice storage unit [non-dimensional]
  REAL(r64)    :: LocalLoad      ! Estimated load on the ice storage unit [W]
  INTEGER :: NodeNumIn      ! Plant loop inlet node number for component
  INTEGER :: NodeNumOut     ! Plant loop outlet node number for component
  REAL(r64)    :: Qstar          ! Current load on the ice storage unit [non-dimensional]
  REAL(r64)    :: TempIn         ! Inlet temperature to component (from plant loop) [C]
  REAL(r64)    :: TempSetPt      ! Setpoint temperature defined by loop controls [C]
  REAL(r64)    :: ToutNew        ! Updated outlet temperature from the tank [C]
  REAL(r64)    :: ToutOld        ! Tank outlet temperature from the last iteration [C]
  REAL(r64)    :: Cp             ! local plant fluid specific heat
  REAL(r64)    :: mdot           ! local mass flow rate for plant connection

          ! FLOW:
          ! Set local variables
  NodeNumIn  = DetIceStor(IceNum)%PlantInNodeNum
  NodeNumOut = DetIceStor(IceNum)%PlantOutNodeNum
  TempIn     = Node(NodeNumIn)%Temp
  SELECT CASE (PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    TempSetPt  = Node(NodeNumOut)%TempSetPoint
  CASE (DualSetPointDeadBand)
    TempSetPt  = Node(NodeNumOut)%TempSetPointHi
  END SELECT

  IterNum    = 0

          ! Set derived type variables
  DetIceStor(IceNum)%InletTemp     = TempIn
  DetIceStor(IceNum)%MassFlowRate  = Node(NodeNumIn)%MassFlowRate

  !if two-way common pipe and no mass flow and tank is not full, then use design flow rate
  IF ((PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%CommonPipeType == CommonPipe_TwoWay) .AND. &
       (ABS(DetIceStor(IceNum)%MassFlowRate) < MassFlowTolerance) .AND. &
       (DetIceStor(IceNum)%IceFracRemaining < TankChargeToler) ) THEN
     DetIceStor(IceNum)%MassFlowRate  = DetIceStor(IceNum)%DesignMassFlowRate
  ENDIF

          ! Calculate the current load on the ice storage unit
  Cp = GetSpecificHeatGlycol(PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%FluidName, &
                             TempIn, &
                             PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%FluidIndex, &
                             'SimDetailedIceStorage')

  LocalLoad = DetIceStor(IceNum)%MassFlowRate * Cp * (TempIn - TempSetPt)

          ! Determine what the status is regarding the ice storage unit and the loop level flow
  IF ( (ABS(LocalLoad) <= SmallestLoad) .OR. (GetCurrentScheduleValue(DetIceStor(IceNum)%ScheduleIndex) <= 0) ) THEN
          ! No real load on the ice storage device or ice storage OFF--bypass all of the flow and leave the tank alone
    DetIceStor(IceNum)%CompLoad           = 0.0d0
    DetIceStor(IceNum)%OutletTemp         = TempIn
    DetIceStor(IceNum)%TankOutletTemp     = TempIn
    mdot = 0.d0
    CALL SetComponentFlowRate(mdot, &
                            DetIceStor(IceNum)%PlantInNodeNum, &
                            DetIceStor(IceNum)%PlantOutNodeNum, &
                            DetIceStor(IceNum)%PlantLoopNum, &
                            DetIceStor(IceNum)%PlantLoopSideNum, &
                            DetIceStor(IceNum)%PlantBranchNum, &
                            DetIceStor(IceNum)%PlantCompNum)

    DetIceStor(IceNum)%BypassMassFlowRate = mdot
    DetIceStor(IceNum)%TankMassFlowRate   = 0.0d0
    DetIceStor(IceNum)%MassFlowRate       = mdot

  ELSEIF (LocalLoad < 0.0d0) THEN
          ! The load is less than zero so we should be charging
          ! Before we do anything, we should check to make sure that we will actually be charging the unit

    IF ( (TempIn > (DetIceStor(IceNum)%FreezingTemp-DeltaTifMin)) .OR. &
         (DetIceStor(IceNum)%IceFracRemaining >= TankChargeToler) ) THEN
          ! If the inlet temperature is not below the freezing temperature of the
          ! device, then we cannot actually do any charging.  Bypass all of the flow.
          ! Also, if the tank is already sufficiently charged, we don't need to
          ! do any further charging.  So, bypass all of the flow.
      DetIceStor(IceNum)%CompLoad           = 0.0d0
      DetIceStor(IceNum)%OutletTemp         = TempIn
      DetIceStor(IceNum)%TankOutletTemp     = TempIn
      mdot = 0.d0
      CALL SetComponentFlowRate(mdot, &
                              DetIceStor(IceNum)%PlantInNodeNum, &
                              DetIceStor(IceNum)%PlantOutNodeNum, &
                              DetIceStor(IceNum)%PlantLoopNum, &
                              DetIceStor(IceNum)%PlantLoopSideNum, &
                              DetIceStor(IceNum)%PlantBranchNum, &
                              DetIceStor(IceNum)%PlantCompNum)

      DetIceStor(IceNum)%BypassMassFlowRate = mdot
      DetIceStor(IceNum)%TankMassFlowRate   = 0.0d0
      DetIceStor(IceNum)%MassFlowRate       = mdot

    ELSE
      !make flow request so tank will get flow
      mdot = DetIceStor(IceNum)%DesignMassFlowRate
      CALL SetComponentFlowRate(mdot, &
                              DetIceStor(IceNum)%PlantInNodeNum, &
                              DetIceStor(IceNum)%PlantOutNodeNum, &
                              DetIceStor(IceNum)%PlantLoopNum, &
                              DetIceStor(IceNum)%PlantLoopSideNum, &
                              DetIceStor(IceNum)%PlantBranchNum, &
                              DetIceStor(IceNum)%PlantCompNum)

          ! We are in charging mode, the temperatures are low enough to charge
          ! the tank, and we have some charging left to do.
          ! Make first guess at Qstar based on the current ice fraction remaining
          ! and LMTDstar that is based on the freezing or TempSetPt temperature.
      IF (TempSetPt > (DetIceStor(IceNum)%FreezingTemp-DeltaTofMin)) THEN
          ! Outlet temperature cannot be above the freezing temperature so set
          ! the outlet temperature to the freezing temperature and calculate
          ! LMTDstar based on that assumption.
        TempSetPt  = DetIceStor(IceNum)%FreezingTemp-DeltaTofMin
      END IF

      ToutOld  = TempSetPt
      LMTDstar = CalcDetIceStorLMTDstar(TempIn,ToutOld,DetIceStor(IceNum)%FreezingTemp)

          ! Find initial guess at average fraction charged during time step
      ChargeFrac = LocalLoad * TimeStepSys / DetIceStor(IceNum)%NomCapacity
      IF ((DetIceStor(IceNum)%IceFracRemaining+ChargeFrac) > 1.0d0) THEN
        ChargeFrac = 1.0d0 - DetIceStor(IceNum)%IceFracRemaining
      END IF
      IF (DetIceStor(IceNum)%ThawProcessIndex == DetIceInsideMelt) THEN
        AvgFracCharged = DetIceStor(IceNum)%IceFracOnCoil + (ChargeFrac/2.0d0)
      ELSE ! (DetIceStor(IceNum)%ThawProcessIndex == DetIceOutsideMelt)
        AvgFracCharged = DetIceStor(IceNum)%IceFracRemaining + (ChargeFrac/2.0d0)
      END IF

      Qstar    = ABS(CurveValue(DetIceStor(IceNum)%ChargeCurveNum,AvgFracCharged,LMTDstar))

      ActualLoad = Qstar * DetIceStor(IceNum)%NomCapacity / DetIceStor(IceNum)%CurveFitTimeStep

      ToutNew = TempIn + (ActualLoad/(DetIceStor(IceNum)%MassFlowRate * Cp ))
          ! Again, the outlet temperature cannot be above the freezing temperature (factoring in the tolerance)
      IF (ToutNew > (DetIceStor(IceNum)%FreezingTemp-DeltaTofMin)) ToutNew = DetIceStor(IceNum)%FreezingTemp-DeltaTofMin

      IF (ActualLoad > ABS(LocalLoad)) THEN
          ! We have more than enough capacity to meet the load so no need to iterate to find a solution
        DetIceStor(IceNum)%OutletTemp         = TempSetPt
        DetIceStor(IceNum)%TankOutletTemp     = ToutNew
        DetIceStor(IceNum)%CompLoad           = DetIceStor(IceNum)%MassFlowRate * Cp * ABS(TempIn - TempSetPt)
        DetIceStor(IceNum)%TankMassFlowRate   = DetIceStor(IceNum)%CompLoad / Cp / ABS(TempIn - ToutNew)
        DetIceStor(IceNum)%BypassMassFlowRate = DetIceStor(IceNum)%MassFlowRate - DetIceStor(IceNum)%TankMassFlowRate

      ELSE

        DO WHILE (IterNum < MaxIterNum)
          IF (ABS(ToutOld-ToutNew) > TemperatureToler) THEN
          ! Not converged yet so recalculated what is needed and keep iterating
          ! Calculate new values for LMTDstar and Qstar based on updated outlet temperature
            ToutOld  = ToutNew
            LMTDstar = CalcDetIceStorLMTDstar(TempIn,ToutOld,DetIceStor(IceNum)%FreezingTemp)
            Qstar    = ABS(CurveValue(DetIceStor(IceNum)%ChargeCurveNum,AvgFracCharged,LMTDstar))

          ! Now make sure that we don't go above 100% charged and calculate the new average fraction
            ChargeFrac = Qstar * (TimeStepSys/DetIceStor(IceNum)%CurveFitTimeStep)
            IF ((DetIceStor(IceNum)%IceFracRemaining+ChargeFrac) > 1.0d0) THEN
              ChargeFrac = 1.0d0 - DetIceStor(IceNum)%IceFracRemaining
              Qstar      = ChargeFrac
            END IF
            IF (DetIceStor(IceNum)%ThawProcessIndex == DetIceInsideMelt) THEN
              AvgFracCharged = DetIceStor(IceNum)%IceFracOnCoil + (ChargeFrac/2.0d0)
            ELSE ! (DetIceStor(IceNum)%ThawProcessIndex == DetIceOutsideMelt)
              AvgFracCharged = DetIceStor(IceNum)%IceFracRemaining + (ChargeFrac/2.0d0)
            END IF

          ! Finally, update the actual load and calculate the new outlet temperature; increment iteration counter
            ActualLoad = Qstar * DetIceStor(IceNum)%NomCapacity / DetIceStor(IceNum)%CurveFitTimeStep
            ToutNew    = TempIn + (ActualLoad/(DetIceStor(IceNum)%MassFlowRate * Cp ))
          ! Again, the outlet temperature cannot be above the freezing temperature (factoring in the tolerance)
            IF (ToutNew < (DetIceStor(IceNum)%FreezingTemp-DeltaTofMin)) ToutNew = DetIceStor(IceNum)%FreezingTemp-DeltaTofMin
            IterNum    = IterNum + 1

          ELSE
          ! Converged to acceptable tolerance so set output variables and exit DO WHILE loop
            EXIT

          END IF

        END DO    ! ...loop iterating for the ice storage outlet temperature

          ! Keep track of times that the iterations got excessive and report if necessary
        IF (IterNum >= MaxIterNum) THEN
          DetIceStor(IceNum)%ChargeIterErrors = DetIceStor(IceNum)%ChargeIterErrors + 1
          IF (DetIceStor(IceNum)%ChargeIterErrors <= 25) THEN
            CALL ShowWarningError('Detailed Ice Storage model exceeded its internal charging maximum iteration limit')
            CALL ShowContinueError('Detailed Ice Storage System Name = '//TRIM(DetIceStor(IceNum)%Name))
            CALL ShowContinueErrorTimeStamp(' ')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd('Detailed Ice Storage system ['//TRIM(DetIceStor(IceNum)%Name)//  &
                         ']  charging maximum iteration limit exceeded occurrence continues.',  &
                         DetIceStor(IceNum)%ChargeErrorCount)
          END IF
        END IF

          ! Set the values for the key outlet parameters
          ! Note that in REAL(r64)ity the tank will probably bypass some flow when it
          ! gets close to full charge.  This is a simplification that assumes
          ! all flow through the tank during charging and a lower delta T near
          ! the full charge level.  From an energy perspective, this is a reasonable
          ! approximation.
        DetIceStor(IceNum)%OutletTemp         = ToutNew
        DetIceStor(IceNum)%TankOutletTemp     = ToutNew
        DetIceStor(IceNum)%BypassMassFlowRate = 0.0d0
        DetIceStor(IceNum)%TankMassFlowRate   = DetIceStor(IceNum)%MassFlowRate
        DetIceStor(IceNum)%CompLoad           = DetIceStor(IceNum)%MassFlowRate * Cp * ABS(TempIn - ToutNew)

      END IF

    END IF

  ELSEIF (LocalLoad > 0.0d0) THEN
          ! The load is greater than zero so we should be discharging
          ! Before we do anything, we should check to make sure that we will actually be discharging the unit

    IF ( (DetIceStor(IceNum)%InletTemp < (DetIceStor(IceNum)%FreezingTemp+DeltaTifMin)) .OR. &
         (DetIceStor(IceNum)%IceFracRemaining <= TankDischargeToler) ) THEN
          ! If the inlet temperature is below the freezing temperature of the
          ! device, then we cannot actually do any discharging.  Bypass all of the flow.
          ! Also, if the tank is already discharged, we can't to do any further
          ! discharging.  So, bypass all of the flow.
      DetIceStor(IceNum)%CompLoad           = 0.0d0
      DetIceStor(IceNum)%OutletTemp         = DetIceStor(IceNum)%InletTemp
      DetIceStor(IceNum)%TankOutletTemp     = DetIceStor(IceNum)%InletTemp
      mdot = 0.d0
      CALL SetComponentFlowRate(mdot, &
                              DetIceStor(IceNum)%PlantInNodeNum, &
                              DetIceStor(IceNum)%PlantOutNodeNum, &
                              DetIceStor(IceNum)%PlantLoopNum, &
                              DetIceStor(IceNum)%PlantLoopSideNum, &
                              DetIceStor(IceNum)%PlantBranchNum, &
                              DetIceStor(IceNum)%PlantCompNum)

      DetIceStor(IceNum)%BypassMassFlowRate = mdot
      DetIceStor(IceNum)%TankMassFlowRate   = 0.0d0
      DetIceStor(IceNum)%MassFlowRate       = mdot

    ELSE

          !make flow request so tank will get flow
      mdot = DetIceStor(IceNum)%DesignMassFlowRate
      CALL SetComponentFlowRate(mdot, &
                              DetIceStor(IceNum)%PlantInNodeNum, &
                              DetIceStor(IceNum)%PlantOutNodeNum, &
                              DetIceStor(IceNum)%PlantLoopNum, &
                              DetIceStor(IceNum)%PlantLoopSideNum, &
                              DetIceStor(IceNum)%PlantBranchNum, &
                              DetIceStor(IceNum)%PlantCompNum)

          ! We are in discharging mode, the temperatures are high enough to discharge
          ! the tank, and we have some discharging left to do.
      IF (TempSetPt < (DetIceStor(IceNum)%FreezingTemp+DeltaTofMin)) THEN
          ! Outlet temperature cannot be below the freezing temperature so set
          ! the outlet temperature to the freezing temperature and calculate
          ! LMTDstar based on that assumption.
        TempSetPt  = DetIceStor(IceNum)%FreezingTemp+DeltaTofMin
      END IF

      ToutOld  = TempSetPt
      LMTDstar = CalcDetIceStorLMTDstar(TempIn,ToutOld,DetIceStor(IceNum)%FreezingTemp)

          ! Find initial guess at average fraction charged during time step
      ChargeFrac = LocalLoad * TimeStepSys / DetIceStor(IceNum)%NomCapacity
      IF ((DetIceStor(IceNum)%IceFracRemaining-ChargeFrac) < 0.0d0) ChargeFrac = DetIceStor(IceNum)%IceFracRemaining
      AvgFracCharged = DetIceStor(IceNum)%IceFracRemaining - (ChargeFrac/2.0d0)

      Qstar = ABS(CurveValue(DetIceStor(IceNum)%DischargeCurveNum,(1.0d0-AvgFracCharged),LMTDstar))

      ActualLoad = Qstar * DetIceStor(IceNum)%NomCapacity / DetIceStor(IceNum)%CurveFitTimeStep

      ToutNew = TempIn - (ActualLoad/(DetIceStor(IceNum)%MassFlowRate * Cp ))
          ! Again, the outlet temperature cannot be below the freezing temperature (factoring in the tolerance)
      IF (ToutNew < (DetIceStor(IceNum)%FreezingTemp+DeltaTofMin)) ToutNew = DetIceStor(IceNum)%FreezingTemp+DeltaTofMin

      IF (ActualLoad > LocalLoad) THEN
          ! We have more than enough storage to meet the load so no need to iterate to find a solution
        DetIceStor(IceNum)%OutletTemp         = TempSetPt
        DetIceStor(IceNum)%TankOutletTemp     = ToutNew
        DetIceStor(IceNum)%CompLoad           = DetIceStor(IceNum)%MassFlowRate * Cp * ABS(TempIn - TempSetPt)
        DetIceStor(IceNum)%TankMassFlowRate   = DetIceStor(IceNum)%CompLoad / Cp / ABS(TempIn - ToutNew)
        DetIceStor(IceNum)%BypassMassFlowRate = DetIceStor(IceNum)%MassFlowRate - DetIceStor(IceNum)%TankMassFlowRate

      ELSE

        DO WHILE (IterNum < MaxIterNum)
          IF (ABS(ToutOld-ToutNew) > TemperatureToler) THEN
          ! Not converged yet so recalculated what is needed and keep iterating
          ! Calculate new values for LMTDstar and Qstar based on updated outlet temperature
            ToutOld  = ToutNew
            LMTDstar = CalcDetIceStorLMTDstar(TempIn,ToutOld,DetIceStor(IceNum)%FreezingTemp)
            Qstar    = ABS(CurveValue(DetIceStor(IceNum)%DischargeCurveNum,(1.0d0-AvgFracCharged),LMTDstar))

          ! Now make sure that we don't go below 100% discharged and calculate the new average fraction
            ChargeFrac = Qstar * (TimeStepSys/DetIceStor(IceNum)%CurveFitTimeStep)
            IF ((DetIceStor(IceNum)%IceFracRemaining-ChargeFrac) < 0.0d0) THEN
              ChargeFrac = DetIceStor(IceNum)%IceFracRemaining
              Qstar      = ChargeFrac
            END IF
            AvgFracCharged = DetIceStor(IceNum)%IceFracRemaining - (ChargeFrac/2.0d0)

          ! Finally, update the actual load and calculate the new outlet temperature; increment iteration counter
            ActualLoad = Qstar * DetIceStor(IceNum)%NomCapacity / DetIceStor(IceNum)%CurveFitTimeStep
            ToutNew    = TempIn - (ActualLoad/(DetIceStor(IceNum)%MassFlowRate * Cp))
          ! Again, the outlet temperature cannot be below the freezing temperature (factoring in the tolerance)
            IF (ToutNew < (DetIceStor(IceNum)%FreezingTemp+DeltaTofMin)) ToutNew = DetIceStor(IceNum)%FreezingTemp+DeltaTofMin
            IterNum    = IterNum + 1

          ELSE
          ! Converged to acceptable tolerance so set output variables and exit DO WHILE loop
            EXIT

          END IF

        END DO    ! ...loop iterating for the ice storage outlet temperature

          ! Keep track of times that the iterations got excessive
        IF (IterNum >= MaxIterNum) THEN
          DetIceStor(IceNum)%DischargeIterErrors = DetIceStor(IceNum)%DischargeIterErrors + 1
          IF (DetIceStor(IceNum)%DischargeIterErrors <= 25) THEN
            CALL ShowWarningError('Detailed Ice Storage model exceeded its internal discharging maximum iteration limit')
            CALL ShowContinueError('Detailed Ice Storage System Name = '//TRIM(DetIceStor(IceNum)%Name))
            CALL ShowContinueErrorTimeStamp(' ')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd('Detailed Ice Storage system ['//TRIM(DetIceStor(IceNum)%Name)//  &
                         ']  discharging maximum iteration limit exceeded occurrence continues.',  &
                         DetIceStor(IceNum)%DischargeErrorCount)
          END IF
        END IF

          ! We are now done finding the outlet temperature of the tank.  We need
          ! to compare the outlet temperature to the setpoint temperature again
          ! to see where we are at and then we can set the values for the key
          ! outlet parameters.  If outlet temperature is greater than or equal
          ! to the setpoint temperature, then send all flow through the tank.
          ! Otherwise, we have more capacity than needed so let's bypass some
          ! flow and meet the setpoint temperautre.
        IF (ToutNew >= TempSetPt) THEN
          DetIceStor(IceNum)%OutletTemp         = ToutNew
          DetIceStor(IceNum)%TankOutletTemp     = ToutNew
          DetIceStor(IceNum)%BypassMassFlowRate = 0.0d0
          DetIceStor(IceNum)%TankMassFlowRate   = DetIceStor(IceNum)%MassFlowRate
          DetIceStor(IceNum)%CompLoad           = DetIceStor(IceNum)%MassFlowRate * Cp * ABS(TempIn - ToutNew)
        ELSE
          DetIceStor(IceNum)%OutletTemp         = TempSetPt
          DetIceStor(IceNum)%TankOutletTemp     = ToutNew
          DetIceStor(IceNum)%CompLoad           = DetIceStor(IceNum)%MassFlowRate * Cp * ABS(TempIn - TempSetPt)
          DetIceStor(IceNum)%TankMassFlowRate   = DetIceStor(IceNum)%CompLoad/(Cp * ABS(TempIn - ToutNew))
          DetIceStor(IceNum)%BypassMassFlowRate = DetIceStor(IceNum)%MassFlowRate - DetIceStor(IceNum)%TankMassFlowRate
        END IF

      END IF

    END IF

  ELSE    ! Shouldn't get here ever (print error if we do)

    CALL ShowFatalError('Detailed Ice Storage systemic code error--contact EnergyPlus support')

  END IF

  RETURN

END SUBROUTINE SimDetailedIceStorage


!******************************************************************************
SUBROUTINE GetIceStorageInput
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:
            !       DATE WRITTEN:

            ! PURPOSE OF THIS SUBROUTINE:!This routine will get the input
                         !required by the PrimaryPlantLoopManager.  As such
                         !it will interact with the Input Scanner to retrieve
                         !information from the input file, count the number of
                         !heating and cooling loops and begin to fill the
                         !arrays associated with the type PlantLoopProps.


            ! METHODOLOGY EMPLOYED: to be determined...
            ! REFERENCES:

            ! USE STATEMENTS:
  USE DataInterfaces, ONLY :  ShowSevereError, ShowWarningError, ShowFatalError, SetupOutputVariable, ShowContinueError
  USE InputProcessor, ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, SameString
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE ScheduleManager
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,   ONLY: GetOnlySingleNode
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IceNum
  INTEGER :: NumAlphas ! Number of elements in the alpha array
  INTEGER :: NumNums   ! Number of elements in the numeric array
  INTEGER :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL :: ErrorsFound
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
          ! FLOW:

  ErrorsFound = .FALSE. ! Always need to reset this since there are multiple types of ice storage systems

  !LOAD ARRAYS WITH IceStorage DATA
  NumIceStorages    = GetNumObjectsFound(cIceStorageSimple)      ! by ZG
  NumDetIceStorages = GetNumObjectsFound(cIceStorageDetailed)

  ALLOCATE (IceStorageTypeMap(NumIceStorages+NumDetIceStorages))
  ALLOCATE(CheckEquipName(NumIceStorages+NumDetIceStorages))
  CheckEquipName=.true.

  ! Allocate IceStorage based on NumOfIceStorage
  ALLOCATE (IceStorage(NumIceStorages))
  ALLOCATE (IceStorageReport(NumIceStorages))

  cCurrentModuleObject = cIceStorageSimple
  DO IceNum = 1 , NumIceStorages

    CALL GetObjectItem(cCurrentModuleObject,IceNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                    NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),IceStorage%Name,IceNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF

    TotalIceStorages=TotalIceStorages+1
    IceStorageTypeMap(TotalIceStorages)%StorageType=cCurrentModuleObject
    IceStorageTypeMap(TotalIceStorages)%StorageType_Num=IceStorageType_Simple
    IceStorageTypeMap(TotalIceStorages)%Name=cAlphaArgs(1)
    IceStorageTypeMap(TotalIceStorages)%LocalEqNum=IceNum
    IceStorage(IceNum)%MapNum      = TotalIceStorages

    ! ITS name
    IceStorage(IceNum)%Name      = cAlphaArgs(1)

    ! Get Ice Thermal Storage Type
    IceStorage(IceNum)%ITSType   = cAlphaArgs(2)
    IF (SameString(IceStorage(IceNum)%ITSType,'IceOnCoilInternal')) THEN
      IceStorage(IceNum)%ITSType_Num=ITSType_IceOnCoilInternal
    ELSEIF (SameString(IceStorage(IceNum)%ITSType,'IceOnCoilExternal')) THEN
      IceStorage(IceNum)%ITSType_Num=ITSType_IceOnCoilExternal
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    ENDIF

    ! Get and Verify ITS nominal Capacity (user input is in GJ, internal value in in J)
    IceStorage(IceNum)%ITSNomCap = rNumericArgs(1)*1.d+09
    IF (rNumericArgs(1) == 0.0d0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      ErrorsFound=.true.
    ENDIF

    ! Get Plant Inlet Node Num
    IceStorage(IceNum)%PltInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    ! Get Plant Outlet Node Num
    IceStorage(IceNum)%PltOutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

         ! Test InletNode and OutletNode
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

         ! Initialize Report Variables
    IceStorageReport(IceNum)%MyLoad = 0.0d0
    IceStorageReport(IceNum)%U = 0.0d0
    IceStorageReport(IceNum)%Urate            = 0.0d0
    IceStorageReport(IceNum)%IceFracRemain    = 1.0d0
    IceStorageReport(IceNum)%ITSCoolingRate   = 0.0d0
    IceStorageReport(IceNum)%ITSCoolingEnergy = 0.0d0
    IceStorageReport(IceNum)%ITSChargingRate   = 0.0d0
    IceStorageReport(IceNum)%ITSChargingEnergy = 0.0d0
    IceStorageReport(IceNum)%ITSmdot          = 0.0d0
    IceStorageReport(IceNum)%ITSInletTemp     = 0.0d0
    IceStorageReport(IceNum)%ITSOutletTemp    = 0.0d0

  END DO  ! IceNum

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject) )
  ENDIF


  ! Setup Output Variables to Report  CurrentModuleObject='ThermalStorage:Ice:Simple'
  !********************************************
  DO IceNum = 1, NumIceStorages

     CALL SetupOutputVariable('Ice Thermal Storage Requested Load [W]', &
          IceStorageReport(IceNum)%MyLoad,'System','Average',IceStorage(IceNum)%Name)

          ! Ice fraction
     CALL SetupOutputVariable('Ice Thermal Storage End Fraction []', &
          IceStorageReport(IceNum)%IceFracRemain,'Zone','Average',IceStorage(IceNum)%Name)

          ! Discharge: ITS Information
     CALL SetupOutputVariable('Ice Thermal Storage Mass Flow Rate [kg/s]', &
          IceStorageReport(IceNum)%ITSmdot,'System','Average',IceStorage(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Inlet Temperature [C]', &
          IceStorageReport(IceNum)%ITSInletTemp,'System','Average',IceStorage(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Outlet Temperature [C]', &
          IceStorageReport(IceNum)%ITSOutletTemp,'System','Average',IceStorage(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Discharge Rate [W]', &
          IceStorageReport(IceNum)%ITSCoolingRate,'System','Average',IceStorage(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Discharge Energy [J]', &
          IceStorageReport(IceNum)%ITSCoolingEnergy,'System','Sum',IceStorage(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Charge Rate [W]', &
          IceStorageReport(IceNum)%ITSChargingRate,'System','Average',IceStorage(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Charge Energy [J]', &
          IceStorageReport(IceNum)%ITSChargingEnergy,'System','Sum',IceStorage(IceNum)%Name)

  END DO  ! IceNum

  ErrorsFound = .FALSE. ! Always need to reset this since there are multiple types of ice storage systems

          ! Determine the number of detailed ice storage devices are in the input file and allocate appropriately
  cCurrentModuleObject = cIceStorageDetailed

  ALLOCATE (DetIceStor(NumDetIceStorages))   ! Allocate DetIceStorage based on NumDetIceStorages

  DO IceNum = 1, NumDetIceStorages

    CALL GetObjectItem(cCurrentModuleObject,IceNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                    AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),DetIceStor%Name,IceNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    END IF

    TotalIceStorages=TotalIceStorages+1
    IceStorageTypeMap(TotalIceStorages)%StorageType=cCurrentModuleObject
    IceStorageTypeMap(TotalIceStorages)%StorageType_Num=IceStorageType_Detailed
    IceStorageTypeMap(TotalIceStorages)%Name=cAlphaArgs(1)
    IceStorageTypeMap(TotalIceStorages)%LocalEqNum=IceNum

    DetIceStor(IceNum)%MapNum = TotalIceStorages
    DetIceStor(IceNum)%Name = cAlphaArgs(1)  ! Detailed ice storage name

          ! Get and verify availability schedule
    DetIceStor(IceNum)%ScheduleName  = cAlphaArgs(2)  ! Detailed ice storage availability schedule name
    IF (lAlphaFieldBlanks(2)) THEN
      DetIceStor(IceNum)%ScheduleIndex = ScheduleAlwaysOn
    ELSE
      DetIceStor(IceNum)%ScheduleIndex = GetScheduleIndex(DetIceStor(IceNum)%ScheduleName)
      IF (DetIceStor(IceNum)%ScheduleIndex == 0) THEN
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound = .TRUE.
      END IF
    END IF

          ! Get and Verify ITS nominal Capacity (user input is in GJ, internal value is in W-hr)
          ! Convert GJ to J by multiplying by 10^9
          ! Convert J to W-hr by dividing by number of seconds in an hour (3600)
    DetIceStor(IceNum)%NomCapacity = rNumericArgs(1)*(1.d+09)/(SecInHour)

    IF (rNumericArgs(1) <= 0.0d0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

          ! Get Plant Inlet Node Num
    DetIceStor(IceNum)%PlantInNodeNum = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                                          NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

          ! Get Plant Outlet Node Num
    DetIceStor(IceNum)%PlantOutNodeNum = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                                           NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

          ! Test InletNode and OutletNode
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Chilled Water Nodes')

          ! Obtain the Charging and Discharging Curve types and names
    DetIceStor(IceNum)%DischargeCurveName = cAlphaArgs(6)
    DetIceStor(IceNum)%DischargeCurveNum  = GetCurveIndex(cAlphaArgs(6))
    IF (DetIceStor(IceNum)%DischargeCurveNum <= 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(6))//'='//TRIM(cAlphaArgs(6)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    ELSE
    DetIceStor(IceNum)%DischargeCurveType = GetCurveType(DetIceStor(IceNum)%DischargeCurveNum)
    END IF
    IF ( (DetIceStor(IceNum)%DischargeCurveType /= cAlphaArgs(5)) .OR. &
         (DetIceStor(IceNum)%DischargeCurveType /= 'QUADRATICLINEAR') ) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Discharge curve type not valid, type='//TRIM(cAlphaArgs(5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Type does not match type for curve name or type does not equal QuadraticLinear')
      ErrorsFound = .TRUE.
    END IF

    DetIceStor(IceNum)%ChargeCurveName = cAlphaArgs(8)
    DetIceStor(IceNum)%ChargeCurveNum  = GetCurveIndex(cAlphaArgs(8))
    IF (DetIceStor(IceNum)%ChargeCurveNum <= 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(8))//'='//TRIM(cAlphaArgs(8)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    ELSE
    DetIceStor(IceNum)%ChargeCurveType = GetCurveType(DetIceStor(IceNum)%ChargeCurveNum)
    END IF
    IF ( (DetIceStor(IceNum)%ChargeCurveType /= cAlphaArgs(7)) .OR. &
         (DetIceStor(IceNum)%ChargeCurveType /= 'QUADRATICLINEAR') ) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Charge curve type not valid, type='//TRIM(cAlphaArgs(7)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Type does not match type for curve name or type does not equal QuadraticLinear')
      ErrorsFound = .TRUE.
    END IF

    DetIceStor(IceNum)%CurveFitTimeStep = rNumericArgs(2)
    IF ( (DetIceStor(IceNum)%CurveFitTimeStep <= 0.0d0) .OR. (DetIceStor(IceNum)%CurveFitTimeStep > 1.0d0) ) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Curve fit time step invalid, less than zero or greater than 1 for ' &
                           //TRIM(cAlphaArgs(1)))
      ErrorsFound = .TRUE.
    END IF

    DetIceStor(IceNum)%ThawProcessIndicator = cAlphaArgs(9)
    IF (SameString(DetIceStor(IceNum)%ThawProcessIndicator,'INSIDEMELT')) THEN
      DetIceStor(IceNum)%ThawProcessIndex = DetIceInsideMelt
    ELSEIF ( (SameString(DetIceStor(IceNum)%ThawProcessIndicator,'OUTSIDEMELT')) .OR. &
             (SameString(DetIceStor(IceNum)%ThawProcessIndicator,Blank)) ) THEN
      DetIceStor(IceNum)%ThawProcessIndex = DetIceOutsideMelt
    ELSE
      CALL ShowSevereError('Invalid thaw process indicator of '//TRIM(cAlphaArgs(9))//' was entered')
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value should either be "InsideMelt" or "OutsideMelt"')
      DetIceStor(IceNum)%ThawProcessIndex = DetIceInsideMelt ! Severe error will end simulation, but just in case...
      ErrorsFound = .TRUE.
    END IF

          ! Get the other ice storage parameters (electric, heat loss, freezing temperature) and stupidity check each one
    DetIceStor(IceNum)%DischargeParaElecLoad = rNumericArgs(3)
    DetIceStor(IceNum)%ChargeParaElecLoad    = rNumericArgs(4)
    DetIceStor(IceNum)%TankLossCoeff         = rNumericArgs(5)
    DetIceStor(IceNum)%FreezingTemp          = rNumericArgs(6)

    IF ( (DetIceStor(IceNum)%DischargeParaElecLoad < 0.0d0) .OR. (DetIceStor(IceNum)%DischargeParaElecLoad > 1.0d0) ) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(3))//'='//TRIM(RoundSigDigits(rNumericArgs(3),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value is either less than/equal to zero or greater than 1')
      ErrorsFound = .TRUE.
    END IF

    IF ( (DetIceStor(IceNum)%ChargeParaElecLoad < 0.0d0) .OR. (DetIceStor(IceNum)%ChargeParaElecLoad > 1.0d0) ) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(4))//'='//TRIM(RoundSigDigits(rNumericArgs(4),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value is either less than/equal to zero or greater than 1')
      ErrorsFound = .TRUE.
    END IF

    IF ( (DetIceStor(IceNum)%TankLossCoeff < 0.0d0) .OR. (DetIceStor(IceNum)%TankLossCoeff > 0.1d0) ) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(5))//'='//TRIM(RoundSigDigits(rNumericArgs(5),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value is either less than/equal to zero or greater than 0.1 (10%)')
      ErrorsFound = .TRUE.
    END IF

    IF ( (DetIceStor(IceNum)%FreezingTemp < -10.0d0) .OR. (DetIceStor(IceNum)%FreezingTemp > 10.0d0) ) THEN
      CALL ShowWarningError('Potentially invalid '//TRIM(cNumericFieldNames(6))//'='//TRIM(RoundSigDigits(rNumericArgs(6),3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value is either less than -10.0C or greater than 10.0C')
      CALL ShowContinueError('This value will be allowed but the user should verify that this temperature is correct')
    END IF

          ! Initialize Report Variables
    DetIceStor(IceNum)%CompLoad            = 0.0d0
    DetIceStor(IceNum)%IceFracChange       = 0.0d0
    DetIceStor(IceNum)%IceFracRemaining    = 1.0d0
    DetIceStor(IceNum)%IceFracOnCoil       = 1.0d0
    DetIceStor(IceNum)%DischargingRate     = 0.0d0
    DetIceStor(IceNum)%DischargingEnergy   = 0.0d0
    DetIceStor(IceNum)%ChargingRate        = 0.0d0
    DetIceStor(IceNum)%ChargingEnergy      = 0.0d0
    DetIceStor(IceNum)%MassFlowRate        = 0.0d0
    DetIceStor(IceNum)%BypassMassFlowRate  = 0.0d0
    DetIceStor(IceNum)%TankMassFlowRate    = 0.0d0
    DetIceStor(IceNum)%InletTemp           = 0.0d0
    DetIceStor(IceNum)%OutletTemp          = 0.0d0
    DetIceStor(IceNum)%TankOutletTemp      = 0.0d0
    DetIceStor(IceNum)%ParasiticElecRate   = 0.0d0
    DetIceStor(IceNum)%ParasiticElecEnergy = 0.0d0

  END DO  ! ...over detailed ice storage units

  IF ((NumIceStorages+NumDetIceStorages) <= 0) THEN
    CALL ShowSevereError('No Ice Storage Equipment found in GetIceStorage')
    ErrorsFound=.true.
  ENDIF


  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject))
  ENDIF


  ! Setup Output Variables to Report CurrentModuleObject='ThermalStorage:Ice:Detailed'
  !********************************************
  DO IceNum = 1, NumDetIceStorages

     CALL SetupOutputVariable('Ice Thermal Storage Cooling Rate [W]', &
                               DetIceStor(IceNum)%CompLoad,'System','Average',DetIceStor(IceNum)%Name)

          ! Ice fraction
     CALL SetupOutputVariable('Ice Thermal Storage Change Fraction []', &
                               DetIceStor(IceNum)%IceFracChange,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage End Fraction []', &
                               DetIceStor(IceNum)%IceFracRemaining,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage On Coil Fraction []', &
                               DetIceStor(IceNum)%IceFracOnCoil,'System','Average',DetIceStor(IceNum)%Name)

          ! Discharge: ITS Information
     CALL SetupOutputVariable('Ice Thermal Storage Mass Flow Rate [kg/s]', &
                               DetIceStor(IceNum)%MassFlowRate,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Bypass Mass Flow Rate [kg/s]', &
                               DetIceStor(IceNum)%BypassMassFlowRate,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Tank Mass Flow Rate [kg/s]', &
                               DetIceStor(IceNum)%TankMassFlowRate,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Fluid Inlet Temperature [C]', &
                               DetIceStor(IceNum)%InletTemp,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Blended Outlet Temperature [C]', &
                               DetIceStor(IceNum)%OutletTemp,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Tank Outlet Temperature [C]', &
                               DetIceStor(IceNum)%TankOutletTemp,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Discharge Rate [W]', &
                               DetIceStor(IceNum)%DischargingRate,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Discharge Energy [J]', &
                               DetIceStor(IceNum)%DischargingEnergy,'System','Sum',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Charge Rate [W]', &
                               DetIceStor(IceNum)%ChargingRate,'System','Average',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Cooling Charge Energy [J]', &
                               DetIceStor(IceNum)%ChargingEnergy,'System','Sum',DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Ancillary Electric Power [W]', &
                               DetIceStor(IceNum)%ParasiticElecRate,'System','Average', DetIceStor(IceNum)%Name)
     CALL SetupOutputVariable('Ice Thermal Storage Ancillary Electric Energy [J]', &
                               DetIceStor(IceNum)%ParasiticElecEnergy,'System','Sum',DetIceStor(IceNum)%Name, &
                               ResourceTypeKey='ELECTRICITY',GroupKey='System')

  END DO  ! ...over detailed ice storage units

  RETURN

END SUBROUTINE GetIceStorageInput


!******************************************************************************
SUBROUTINE InitDetailedIceStorage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes variables for the detailed ice storage model.

          ! METHODOLOGY EMPLOYED:
          ! Initializes parameters based on current status flag values.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY : BeginEnvrnFlag
  USE DataPlant,   ONLY : ScanPlantLoopsForObject, PlantLoop, TypeOf_TS_IceDetailed, CommonPipe_TwoWay, &
                          SupplySide, LoopFlowStatus_NeedyAndTurnsLoopOn
  USE PlantUtilities, ONLY: InitComponentNodes

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
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  INTEGER :: CompNum ! local do loop index
          ! FLOW:
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyPlantScanFlag(NumDetIceStorages))
    ALLOCATE(MyEnvrnFlag(NumDetIceStorages))
    MyPlantScanFlag = .TRUE.
    MyEnvrnFlag     = .TRUE.
    MyOneTimeFlag   = .FALSE.
  ENDIF

  IF (MyPlantScanFlag(IceNum)) THEN
    CALL ScanPlantLoopsForObject( DetIceStor(IceNum)%Name, &
                                  TypeOf_TS_IceDetailed, &
                                  DetIceStor(IceNum)%PlantLoopNum, &
                                  DetIceStor(IceNum)%PlantLoopSideNum, &
                                  DetIceStor(IceNum)%PlantBranchNum, &
                                  DetIceStor(IceNum)%PlantCompNum)


    MyPlantScanFlag(IceNum) = .FALSE.
  ENDIF

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(IceNum)) THEN  ! Beginning of environment initializations
          ! Make sure all state variables are reset at the beginning of every environment to avoid problems.
          ! The storage unit is assumed to be fully charged at the start of any environment.
          ! The IceNum variable is a module level variable that is already set before this subroutine is called.
    DetIceStor(IceNum)%IceFracChange       = 0.0d0
    DetIceStor(IceNum)%IceFracRemaining    = 1.0d0
    DetIceStor(IceNum)%IceFracOnCoil       = 1.0d0
    DetIceStor(IceNum)%InletTemp           = 0.0d0
    DetIceStor(IceNum)%OutletTemp          = 0.0d0
    DetIceStor(IceNum)%TankOutletTemp      = 0.0d0
    DetIceStor(IceNum)%DischargeIterErrors = 0
    DetIceStor(IceNum)%ChargeIterErrors    = 0
    DetIceStor(IceNum)%DesignMassFlowRate  = PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%MaxMassFlowRate
    !no design flow rates for model, assume min is zero and max is plant loop's max
    CALL InitComponentNodes( 0.0d0, DetIceStor(IceNum)%DesignMassFlowRate, &
                                  DetIceStor(IceNum)%PlantInNodeNum, &
                                  DetIceStor(IceNum)%PlantOutNodeNum, &
                                  DetIceStor(IceNum)%PlantLoopNum, &
                                  DetIceStor(IceNum)%PlantLoopSideNum, &
                                  DetIceStor(IceNum)%PlantBranchNum, &
                                  DetIceStor(IceNum)%PlantCompNum)

    IF ((PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%CommonPipeType == CommonPipe_TwoWay) .AND. &
        (DetIceStor(IceNum)%PlantLoopSideNum == SupplySide)) THEN
      ! up flow priority of other components on the same branch as the Ice tank
      DO CompNum = 1, PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%LoopSide(SupplySide)% &
                           Branch(DetIceStor(IceNum)%PlantBranchNum)%TotalComponents
        PlantLoop(DetIceStor(IceNum)%PlantLoopNum)%LoopSide(SupplySide)% &
           Branch(DetIceStor(IceNum)%PlantBranchNum)%Comp(CompNum)%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn
      ENDDO

    ENDIF

    MyEnvrnFlag(IceNum) = .FALSE.
  END IF
  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(IceNum) = .TRUE.

          ! Initializations that are done every iteration
          ! Make sure all of the reporting variables are always reset at the start of any iteration
  DetIceStor(IceNum)%CompLoad            = 0.0d0
  DetIceStor(IceNum)%IceFracChange       = 0.0d0
  DetIceStor(IceNum)%DischargingRate     = 0.0d0
  DetIceStor(IceNum)%DischargingEnergy   = 0.0d0
  DetIceStor(IceNum)%ChargingRate        = 0.0d0
  DetIceStor(IceNum)%ChargingEnergy      = 0.0d0
  DetIceStor(IceNum)%MassFlowRate        = 0.0d0
  DetIceStor(IceNum)%BypassMassFlowRate  = 0.0d0
  DetIceStor(IceNum)%TankMassFlowRate    = 0.0d0
  DetIceStor(IceNum)%ParasiticElecRate   = 0.0d0
  DetIceStor(IceNum)%ParasiticElecEnergy = 0.0d0

  RETURN

END SUBROUTINE InitDetailedIceStorage


SUBROUTINE InitSimpleIceStorage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jan 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! do initializations for simple ice storage

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: BeginEnvrnFlag
  USE DataPlant,      ONLY: TypeOf_TS_IceSimple, PlantLoop, ScanPlantLoopsForObject,CommonPipe_TwoWay, &
                          SupplySide, LoopFlowStatus_NeedyAndTurnsLoopOn
  USE PlantUtilities, ONLY: InitComponentNodes

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
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL :: errFlag
  INTEGER :: CompNum ! local do loop counter

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyPlantScanFlag(NumIceStorages))
    ALLOCATE(MyEnvrnFlag(NumIceStorages))
    MyOneTimeFlag = .false.
    MyPlantScanFlag = .TRUE.
    MyEnvrnFlag     = .TRUE.
  END IF


  IF (MyPlantScanFlag(IceNum)) THEN
    ! Locate the storage on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(IceStorage(IceNum)%Name, &
                                 TypeOf_TS_IceSimple, &
                                 IceStorage(IceNum)%LoopNum, &
                                 IceStorage(IceNum)%LoopSideNum, &
                                 IceStorage(IceNum)%BranchNum, &
                                 IceStorage(IceNum)%CompNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitSimpleIceStorage: Program terminated due to previous condition(s).')
    ENDIF
    MyPlantScanFlag(IceNum)=.FALSE.
  ENDIF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag(IceNum)) THEN
    IceStorage(IceNum)%DesignMassFlowRate = PlantLoop(IceStorage(IceNum)%LoopNum)%MaxMassFlowRate
    !no design flow rates for model, assume min is zero and max is plant loop's max
    CALL InitComponentNodes( 0.0d0, IceStorage(IceNum)%DesignMassFlowRate, &
                                 IceStorage(IceNum)%PltInletNodeNum, &
                                 IceStorage(IceNum)%PltOutletNodeNum, &
                                 IceStorage(IceNum)%LoopNum, &
                                 IceStorage(IceNum)%LoopSideNum, &
                                 IceStorage(IceNum)%BranchNum, &
                                 IceStorage(IceNum)%CompNum)
    IF ((PlantLoop(IceStorage(IceNum)%LoopNum)%CommonPipeType == CommonPipe_TwoWay) .AND. &
        (IceStorage(IceNum)%LoopSideNum == SupplySide)) THEN
      ! up flow priority of other components on the same branch as the Ice tank
      DO CompNum = 1, PlantLoop(IceStorage(IceNum)%LoopNum)%LoopSide(SupplySide)% &
                           Branch(IceStorage(IceNum)%BranchNum)%TotalComponents
        PlantLoop(IceStorage(IceNum)%LoopNum)%LoopSide(SupplySide)% &
           Branch(IceStorage(IceNum)%BranchNum)%Comp(CompNum)%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn
      ENDDO

    ENDIF
    IceStorageReport(IceNum)%MyLoad = 0.0d0
    IceStorageReport(IceNum)%U = 0.0d0
    IceStorageReport(IceNum)%Urate            = 0.0d0
    IceStorageReport(IceNum)%IceFracRemain    = 1.0d0
    IceStorageReport(IceNum)%ITSCoolingRate   = 0.0d0
    IceStorageReport(IceNum)%ITSCoolingEnergy = 0.0d0
    IceStorageReport(IceNum)%ITSChargingRate   = 0.0d0
    IceStorageReport(IceNum)%ITSChargingEnergy = 0.0d0
    IceStorageReport(IceNum)%ITSmdot          = 0.0d0
    IceStorageReport(IceNum)%ITSInletTemp     = 0.0d0
    IceStorageReport(IceNum)%ITSOutletTemp    = 0.0d0

    MyEnvrnFlag(IceNum) = .FALSE.
  ENDIF

  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag(IceNum) = .TRUE.

  ITSNomCap            = IceStorage(IceNum)%ITSNomCap
  InletNodeNum         = IceStorage(IceNum)%PltInletNodeNum
  OutletNodeNum        = IceStorage(IceNum)%PltOutletNodeNum

  RETURN

END SUBROUTINE InitSimpleIceStorage


!******************************************************************************
SUBROUTINE CalcIceStorageCapacity(IceStorageType,MaxCap,MinCap,OptCap)
          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals,      ONLY: WarmupFlag, NumOfTimeStepInHour, HourOfDay, TimeStep
  USE ScheduleManager,  ONLY: GetCurrentScheduleValue

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     ::IceStorageType

  REAL(r64), INTENT(OUT)      :: MaxCap
  REAL(r64), INTENT(OUT)      :: MinCap
  REAL(r64), INTENT(OUT)      :: OptCap

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: Umax               ! Max Urate  [fraction]
  REAL(r64)    :: Umin               ! Min Urate  [fraction]
  REAL(r64)    :: Uact               ! Acting between Umax and Umin [fraction]
  REAL(r64)    :: ITSCoolingRateMax
  REAL(r64)    :: ITSCoolingRateOpt
  REAL(r64)    :: ITSCoolingRateMin
  REAL(r64)    :: QiceMin
! unused  REAL(r64)    :: Tdb


          ! FLOW
SELECT CASE(IceStorageType)
CASE(IceStorageType_Simple)

  !------------------------------------------------------------------------
  ! FIRST PROCESS (MyLoad = 0.0 as IN)
  ! At this moment as first calling of ITS, ITS provide ONLY MaxCap/OptCap/MinCap.
  !------------------------------------------------------------------------

       ! Initialize Capacity
  MaxCap = 0.0d0
  MinCap = 0.0d0
  OptCap = 0.0d0

       ! Initialize processed Usys values
  Umax = 0.0d0
  Umin = 0.0d0
  Uact = 0.0d0

       ! XCurIceFrac is reset to 1.0 when first hour of day.
       ! Starting full is assumed, because most ice systems are fully charged overnight
  IF(ResetXForITSFlag) THEN
    XCurIceFrac = 1.0d0
    IceStorageReport(IceNum)%IceFracRemain = 1.0d0
    Urate = 0.0d0
    ResetXForITSFlag = .FALSE.
  END IF

       ! Calculate UAIceDisch[W/C] and UAIceCh[W/F] based on ONLY XCurIceFrac
  CALL CalcUAIce(IceNum,XCurIceFrac,  UAIceCh,UAIceDisCh,HLoss)

         ! Calculate QiceMin by UAiceDisCh*deltaTlm
         !   with UAiceDisCh(function of XCurIceFrac), ITSInletTemp and ITSOutletTemp(=Node(OutletNodeNum)%TempSetPoint by E+[C])
         ! QiceMin is REAL(r64) ITS capacity.
  CALL CalcQiceDischageMax(QiceMin)

         ! Check Umax and Umin to verify the input U value.
  Umax = 0.0d0
         ! At the first call of ITS model, MyLoad is 0. After that proper MyLoad will be provided by E+.
         ! Therefore, Umin is decided between input U and ITS REAL(r64) capacity.
  Umin = MIN( MAX( (-(1.0d0-EpsLimitForDisCharge)*QiceMin*TimeInterval/ITSNomCap), (-XCurIceFrac+EpsLimitForX) ), 0.0d0 )

        ! Calculate CoolingRate with Uact to provide E+.
  Uact = Umin
  ITSCoolingRateMax = ABS(Uact*ITSNomCap/TimeInterval)
  ITSCoolingRateOpt = ITSCoolingRateMax
  ITSCoolingRateMin = 0.0d0

       ! Define MaxCap, OptCap, and MinCap
  MaxCap = ITSCoolingRateMax
  OptCap = ITSCoolingRateOpt
  MinCap = ITSCoolingRateMin

CASE DEFAULT

END SELECT


RETURN
END SUBROUTINE CalcIceStorageCapacity


!******************************************************************************
SUBROUTINE CalcIceStorageDormant(IceStorageType,IceNum)
          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE PlantUtilities, ONLY: SetComponentFlowRate
  USE DataPlant,      ONLY : PlantLoop, SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IceStorageType  !BY ZG
  INTEGER :: IceNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Umax   ! Max Urate  [fraction]
  REAL(r64) :: Umin   ! Min Urate  [fraction]
  REAL(r64) :: Uact   ! Acting between Umax and Umin [fraction]

          ! FLOW
SELECT CASE (IceStorageType)   !by ZG

CASE(IceStorageType_Simple)   !by ZG

       ! Initialize processed Usys values
  Umax = 0.0d0
  Umin = 0.0d0
  Uact = 0.0d0

       ! Provide output results for ITS.
  ITSMassFlowRate  = 0.d0                        ![kg/s]

  CALL SetComponentFlowRate(ITSMassFlowRate, &
                            IceStorage(IceNum)%PltInletNodeNum, &
                            IceStorage(IceNum)%PltOutletNodeNum, &
                            IceStorage(IceNum)%LoopNum, &
                            IceStorage(IceNum)%LoopSideNum, &
                            IceStorage(IceNum)%BranchNum, &
                            IceStorage(IceNum)%CompNum )

  ITSInletTemp     = Node(InletNodeNum)%Temp  ![C]
  ITSOutletTemp    = ITSInletTemp             ![C]
  SELECT CASE (PlantLoop(IceStorage(IceNum)%LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    ITSOutletSetPointTemp  = Node(OutletNodeNum)%TempSetPoint
  CASE (DualSetPointDeadBand)
    ITSOutletSetPointTemp  = Node(OutletNodeNum)%TempSetPointHi
  END SELECT
  ITSCoolingRate   = 0.0d0                      ![W]
  ITSCoolingEnergy = 0.0d0                      ![J]

  Urate = 0.0d0  ![n/a]

CASE DEFAULT

END SELECT


RETURN
END SUBROUTINE CalcIceStorageDormant


!******************************************************************************
SUBROUTINE CalcIceStorageCharge(IceStorageType,IceNum)
          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE Psychrometrics, ONLY:CPCW
  USE PlantUtilities, ONLY: SetComponentFlowRate
  USE DataPlant,      ONLY: PlantLoop, SingleSetpoint, DualSetpointDeadband

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IceStorageType  !BY ZG
  INTEGER :: IceNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Umax               ! Max Urate adjusted Urate based on Error protection (I) [fraction]
  REAL(r64) :: Umin               ! Min Urate adjusted Urate based on Error protection (I) [fraction]
  REAL(r64) :: Uact               ! Acting between Usys and UsysLow Urate adjusted Urate based on Error protection (I) [fraction]
  REAL(r64) :: QiceMax            ![W]
  REAL(r64) :: QiceMaxByChiller   ![W]
  REAL(r64) :: QiceMaxByITS       ![W]
  REAL(r64) :: Qice               ![W]
  REAL(r64) :: DeltaTemp          ![C]

          ! FLOW
SELECT CASE (IceStorageType)
CASE(IceStorageType_Simple)

  !--------------------------------------------------------
  ! Initialize
  !--------------------------------------------------------
       ! Below values for ITS are reported forCharging process.
  ITSMassFlowRate  = IceStorage(IceNum)%DesignMassFlowRate ![kg/s]

  CALL SetComponentFlowRate(ITSMassFlowRate, &
                            IceStorage(IceNum)%PltInletNodeNum, &
                            IceStorage(IceNum)%PltOutletNodeNum, &
                            IceStorage(IceNum)%LoopNum, &
                            IceStorage(IceNum)%LoopSideNum, &
                            IceStorage(IceNum)%BranchNum, &
                            IceStorage(IceNum)%CompNum )

  ITSInletTemp     = Node(InletNodeNum)%Temp  ![C]
  ITSOutletTemp    = ITSInletTemp             ![C]
  SELECT CASE (PlantLoop(IceStorage(IceNum)%LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    ITSOutletSetPointTemp  = Node(OutletNodeNum)%TempSetPoint
  CASE (DualSetPointDeadBand)
    ITSOutletSetPointTemp  = Node(OutletNodeNum)%TempSetPointHi
  END SELECT
  ITSCoolingRate   = 0.0d0                      ![W]
  ITSCoolingEnergy = 0.0d0                      ![J]

       ! Initialize processed U values
  Umax = 0.0d0
  Umin = 0.0d0
  Uact = 0.0d0
  Urate = 0.0d0

       ! Calculate QiceMax which is REAL(r64) ITS capacity.
       ! There are three possible to calculate QiceMax
       !   with ChillerCapacity(Chiller+ITS), ITS capacity(ITS), and QchillerMax(Chiller).
  !--------------------------------------------------------
  ! Calcualte QiceMax with QiceMaxByChiller, QiceMaxByITS, QchillerMax
  !--------------------------------------------------------
       ! Calculate Qice charge max by Chiller with Twb and UAiceCh
  CALL CalcQiceChargeMaxByChiller(IceNum,QiceMaxByChiller)  ![W]

         ! Chiller is remote now, so chiller out is inlet node temp
  ChillerOutletTemp = Node(IceStorage(IceNum)%PltInletNodeNum)%Temp
       ! Calculate Qice charge max by ITS with ChillerOutletTemp
  CALL CalcQiceChargeMaxByITS(IceNum,ChillerOutletTemp,QiceMaxByITS)  ![W]

       ! Select minimum as QiceMax
       ! Because It is uncertain that QiceMax by chiller is same as QiceMax by ITS.
  QiceMax = MIN(QiceMaxByChiller,QiceMaxByITS)

  !--------------------------------------------------------
  ! Calculate Umin,Umax,Uact
  !--------------------------------------------------------
       ! Set Umin
  Umin = 0.0d0
       ! Calculate Umax based on real ITS Max Capacity and remained XCurIceFrac.
       ! Umax should be equal or larger than 0.02 for realistic purpose by Dion.
  Umax = MAX( MIN( ((1.0d0-EpsLimitForCharge)*QiceMax*TimeInterval/ITSNomCap), (1.0d0-XCurIceFrac-EpsLimitForX) ), 0.0d0 )

         ! Cannot charge more than the fraction that is left uncharged
  Umax = MIN(Umax,(1.d0-IceStorageReport(IceNum)%IceFracRemain)/TimeStepSys)
       ! First, check input U value.
       ! Based on Umax and Umin, if necessary to run E+, calculate proper Uact.
  IF( Umax == 0.0d0 ) THEN  !(No Capacity of ITS), ITS is OFF.
    Uact = 0.0d0

  ELSE  ! Umax non-zero
    Uact = Umax
  END IF  ! Check Uact for Discharging Process

  !--------------------------------------------------------
  ! Calcualte possible ITSChargingRate with Uact, Then error check
  !--------------------------------------------------------
       ! Calculate possible ITSChargingRate with Uact
  Qice = Uact*ITSNomCap/TimeInterval  ![W]
       ! If Qice is equal or less than 0.0, no need to calculate anymore.
  IF( Qice <= 0.0d0 ) THEN
    Urate               = 0.0d0                ![ratio]
  END IF

  !--------------------------------------------------------
  ! Find ChillerOutlet Temperature
  !--------------------------------------------------------
         ! Chiller is remote now, so chiller out is inlet node temp
  ChillerOutletTemp = Node(IceStorage(IceNum)%PltInletNodeNum)%Temp

        ! Calculate leaving water temperature
  IF((Qice .LE. 0.0d0) .OR. (XCurIceFrac .GE. 1.0d0)) THEN
    ITSOutletTemp = ITSInletTemp
    DeltaTemp     = 0.0d0
    Qice          = 0.0d0
    Uact          = 0.0d0
  ELSE
    DeltaTemp = Qice/CPCW(ITSInletTemp)/ITSMassFlowRate
    ITSOutletTemp = ITSInletTemp + DeltaTemp
     ! Limit leaving temp to be no greater than setpoint or freezing temp minus 1C
    ITSOutletTemp = MIN(ITSOutletTemp, ITSOutletSetPointTemp, (FreezTemp-1))
     ! Limit leaving temp to be no less than inlet temp
    ITSOutletTemp = MAX(ITSOutletTemp, ITSInletTemp)
    DeltaTemp     = ITSOutletTemp - ITSInletTemp
    Qice          = DeltaTemp*CPCW(ITSInletTemp)*ITSMassFlowRate
    Uact          = Qice/(ITSNomCap/TimeInterval)
  END IF  ! End of leaving temp checks

  Urate = Uact
  ITSCoolingRate   = -Qice
  ITSCoolingEnergy = ITSCoolingRate*TimeStepSys*SecInHour

CASE DEFAULT

END SELECT


RETURN
END SUBROUTINE CalcIceStorageCharge


!******************************************************************************
SUBROUTINE CalcQiceChargeMaxByChiller(IceNum,QiceMaxByChiller)

          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! Calculation inside is IP unit, then return QiceMaxByChiller as SI [W] unit.

          ! REFERENCES:

          ! USE STATEMENTS:

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER                :: IceNum
  REAL(r64), INTENT(OUT)      :: QiceMaxByChiller

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: TchillerOut

          ! FLOW

         ! Chiller is remote now, so chiller out is inlet node temp
  TchillerOut = Node(IceStorage(IceNum)%PltInletNodeNum)%Temp
  QiceMaxByChiller = UAiceCh*( FreezTemp-TchillerOut )  ![W] = [W/degC]*[degC]

       ! If it happened, it is occurred at the Discharing or Dormant process.
  IF( QiceMaxByChiller <= 0.0d0 ) THEN
    QiceMaxByChiller = 0.0d0
  END IF

RETURN
END SUBROUTINE CalcQiceChargeMaxByChiller


!******************************************************************************
SUBROUTINE CalcQiceChargeMaxByITS(IceNum,ChillerOutletTemp,QiceMaxByITS)

          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: IceNum
  REAL(r64), INTENT(IN)  :: ChillerOutletTemp  ![degC]
  REAL(r64), INTENT(OUT) :: QiceMaxByITS       ![W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tfr
  REAL(r64) :: ChillerInletTemp
  REAL(r64) :: ChOutletTemp
  REAL(r64) :: LogTerm

          ! FLOW
  ! Qice is maximized when ChillerInletTemp and ChillerOutletTemp(input data) is almost same due to LMTD method.
  ! Qice is minimized(=0) when ChillerInletTemp is almost same as FreezTemp(=0).

       ! Initilize
  Tfr = FreezTempIP
  ChOutletTemp = TempSItoIP(ChillerOutletTemp)  ![degF] = ConvertSItoIP[degC]
       ! Chiller outlet temp must be below freeze temp, or else no charge
  IF (ChOutletTemp .GE. Tfr) THEN
    ChillerInletTemp = ChOutletTemp
    QiceMaxByITS     = 0.0d0
  ELSE
       ! Make ChillerInletTemp as almost same as ChillerOutletTemp(input data)
    ChillerInletTemp = ChOutletTemp + 0.01
       ! ChillerInletTemp cannot be greater than or equal to freeze temp
    IF (ChillerInletTemp .GE. Tfr) THEN
      ChillerInletTemp = ChOutletTemp + (Tfr - ChOutletTemp)/2
    ENDIF

    LogTerm      = (Tfr - ChOutletTemp) / (Tfr - ChillerInletTemp)
       ! Need to protect this from LogTerm <= 0 - not sure what it should do then
    IF (LogTerm <= 0.0d0) THEN
      ChillerInletTemp = ChOutletTemp
      QiceMaxByITS     = 0.0d0
    ENDIF
    QiceMaxByITS = UAiceCh * ( TempIPtoSI(ChillerInletTemp) - TempIPtoSI(ChOutletTemp) ) / LOG(LogTerm)
  ENDIF

RETURN
END SUBROUTINE CalcQiceChargeMaxByITS


!******************************************************************************
SUBROUTINE CalcIceStorageDischarge(IceStorageType,IceNum,MyLoad,Runflag,FirstIteration,MaxCap)

          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: HourOfDay,TimeStep,NumOfTimeStepInHour
  USE DataInterfaces, ONLY: ShowFatalError
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE DataPlant, ONLY:  PlantLoop, SingleSetpoint, DualSetpointDeadband
  USE FluidProperties, ONLY: GetDensityGlycol
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT(IN)    :: IceStorageType  !by ZG

  INTEGER                :: IceNum          ! ice storage number
  REAL(r64)              :: MyLoad          ! operating load
  LOGICAL                :: FirstIteration  ! TRUE when first iteration of timestep
  LOGICAL, INTENT(IN)    :: RunFlag         ! TRUE when ice storage operating
  REAL(r64),    INTENT(IN)    :: MaxCap          ! Max possible discharge rate (positive value)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: TempTol = 0.0001d0          ! C - minimum significant mass flow rate

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
       ! External function
       ! Local
  REAL(r64)    :: Umax      =0.0d0     ! Max Urate adjusted Urate based on Error protection (I) [fraction]
  REAL(r64)    :: Umin      =0.0d0     ! Min Urate adjusted Urate based on Error protection (I) [fraction]
  REAL(r64)    :: Uact      =0.0d0     ! Acting between Usys and UsysLow Urate adjusted Urate based on Error protection (I) [fraction]
  REAL(r64)    :: Umyload   =0.0d0
! unused  REAL(r64)    :: QiceMin
  REAL(r64)    :: Qice      =0.0d0
  REAL(r64)    :: DeltaTemp =0.0d0


  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL(r64)  :: CpFluid ! local temporary for plant loop's fluid specific heat



          ! FLOW
SELECT CASE (IceStorageType)
CASE(IceStorageType_Simple)

       ! Initialize processed Rate and Energy
  ITSMassFlowRate  = 0.0d0
  ITSCoolingRate   = 0.0d0
  ITSCoolingEnergy = 0.0d0

  SELECT CASE (PlantLoop(IceStorage(IceNum)%LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    ITSOutletSetPointTemp  = Node(OutletNodeNum)%TempSetPoint
  CASE (DualSetPointDeadBand)
    ITSOutletSetPointTemp  = Node(OutletNodeNum)%TempSetPointHi
  END SELECT

       ! Initialize processed U values
  Umax = 0.0d0
  Umin = 0.0d0
  Uact = 0.0d0
  Umyload = 0.0d0
  Urate   = 0.0d0

       ! If no component demand or ITS OFF, then RETURN.
  IF( MyLoad == 0 .OR. .NOT.RunFlag ) THEN
    ITSMassFlowRate  = 0.0d0
    ITSInletTemp     = Node(InletNodeNum)%Temp
    ITSOutletTemp    = ITSInletTemp
    ITSCoolingRate   = 0.0d0
    ITSCoolingEnergy = 0.0d0
    RETURN
  END IF

       ! If FlowLock(provided by PlantSupplyManager) is False(=0), that is, MyLoad is not changed.
       ! then based on MyLoad, new ITSMassFlowRate will be calculated.


  !----------------------------
  LoopNum = IceStorage(IceNum)%LoopNum
  LoopSideNum = IceStorage(IceNum)%LoopSideNum

  CpFluid = GetDensityGlycol(PlantLoop(LoopNum)%FluidName, &
                             Node(InletNodeNum)%Temp, &
                             PlantLoop(LoopNum)%FluidIndex, &
                             'CalcIceStorageDischarge')

       ! Calculate Umyload based on MyLoad from E+
  Umyload = -MyLoad*TimeInterval/ITSNomCap
       ! Calculate Umax and Umin
       ! Cannot discharge more than the fraction that is left
  Umax = -IceStorageReport(IceNum)%IceFracRemain/TimeStepSys
       ! Calculate Umin based on returned Myload from E+.
  Umin = MIN( Umyload, 0.0d0 )
       ! Based on Umax and Umin, if necessary to run E+, calculate proper Uact
       ! U is negative here.
  Uact = MAX(Umin,Umax)

       ! Set ITSInletTemp provided by E+
  ITSInletTemp  = Node(InletNodeNum)%Temp
       !The first thing is to set the ITSMassFlowRate
  ITSMassFlowRate = IceStorage(IceNum)%DesignMassFlowRate ![kg/s]

  CALL SetComponentFlowRate(ITSMassFlowRate, &
                          IceStorage(IceNum)%PltInletNodeNum, &
                          IceStorage(IceNum)%PltOutletNodeNum, &
                          IceStorage(IceNum)%LoopNum, &
                          IceStorage(IceNum)%LoopSideNum, &
                          IceStorage(IceNum)%BranchNum, &
                          IceStorage(IceNum)%CompNum )

       ! Qice is calculate input U which is within boundary between Umin and Umax.
  Qice = Uact*ITSNomCap/TimeInterval
       ! Qice cannot exceed MaxCap calulated by CalcIceStorageCapacity
       ! Note Qice is negative here, MaxCap is positive
  Qice = MAX(Qice, -MaxCap)

        ! Calculate leaving water temperature
  IF((Qice .GE. 0.0d0) .OR. (XCurIceFrac .LE. 0.0d0)) THEN
    ITSOutletTemp = ITSInletTemp
    DeltaTemp     = 0.0d0
    Qice          = 0.0d0
    Uact          = 0.0d0
  ELSE
    DeltaTemp = Qice/CpFluid/ITSMassFlowRate
    ITSOutletTemp = ITSInletTemp + DeltaTemp
     ! Limit leaving temp to be no less than setpoint or freezing temp plus 1C
    ITSOutletTemp = MAX(ITSOutletTemp, ITSOutletSetPointTemp, (FreezTemp+1))
     ! Limit leaving temp to be no greater than inlet temp
    ITSOutletTemp = MIN(ITSOutletTemp, ITSInletTemp)
    DeltaTemp     = ITSOutletTemp - ITSInletTemp
    Qice          = DeltaTemp*CpFluid*ITSMassFlowRate
    Uact          = Qice/(ITSNomCap/TimeInterval)
  END IF  ! End of leaving temp checks

       ! Calculate reported U value
  Urate = Uact
       ! Calculate ITSCoolingEnergy [J]
  ITSCoolingRate   = -Qice
  ITSCoolingEnergy = ITSCoolingRate*TimeStepSys*SecInHour

CASE DEFAULT
END SELECT

RETURN
END SUBROUTINE CalcIceStorageDischarge


!******************************************************************************
SUBROUTINE CalcQiceDischageMax(QiceMin)

          ! SUBROUTINE INFORMATION:

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataPlant, ONLY : PlantLoop, SingleSetpoint, DualSetpointDeadband
  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(OUT) :: QiceMin

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ITSInletTemp
  REAL(r64) :: ITSOutletTemp
  REAL(r64) :: LogTerm

          ! FLOW
  ! Qice is minimized when ITSInletTemp and ITSOutletTemp is almost same due to LMTD method.
  ! Qice is maximized(=0) when ITSOutletTemp is almost same as FreezTemp(=0).

       ! Set ITSInletTemp from E+.
  ITSInletTemp  = Node(InletNodeNum)%Temp

       ! Make ITSOutletTemp as almost same as ITSInletTemp
  SELECT CASE (PlantLoop(IceStorage(IceNum)%LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetPoint)
    ITSOutletTemp  = Node(OutletNodeNum)%TempSetPoint
  CASE (DualSetPointDeadBand)
    ITSOutletTemp  = Node(OutletNodeNum)%TempSetPointHi
  END SELECT

    LogTerm       = (ITSInletTemp - FreezTemp) / (ITSOutletTemp - FreezTemp)

  IF(LogTerm <= 1) THEN
    QiceMin = 0.0d0
  ELSE
    QiceMin = UAiceDisCh * (ITSInletTemp-ITSOutletTemp) / LOG(LogTerm)
  ENDIF

RETURN
END SUBROUTINE CalcQiceDischageMax


!******************************************************************************
SUBROUTINE CalcUAIce(IceNum,XCurIceFrac,UAIceCh,UAIceDisCh,HLoss)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
  ! This routine is funtion of XCurIceFrac, and UA vaule is based on 1 hour.

          ! REFERENCES:

          ! USE STATEMENTS:

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IceNum
  REAL(r64), INTENT(IN)    :: XCurIceFrac
  REAL(r64), INTENT(OUT)   :: UAIceCh
  REAL(r64), INTENT(OUT)   :: UAIceDisCh
  REAL(r64), INTENT(OUT)   :: HLoss

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: y
!  REAL(r64)  :: dTlmic
!  REAL(r64)  :: Tfr     ! IP freezing temperature

  ! Flow

  SELECT CASE( IceStorage(IceNum)%ITSType_Num )
    CASE(ITSType_IceOnCoilInternal)
      y = XCurIceFrac
      UAIceCh = ( 1.3879d0 - 7.6333d0*y + 26.3423d0*y**2 - 47.6084d0*y**3 + 41.8498d0*y**4 - 14.2948d0*y**5 )  &     ! [W/C]
               * IceStorage(IceNum)%ITSNomCap / TimeInterval / 10.0d0
      y=1.0d0-XCurIceFrac
      UAIceDisCh = ( 1.3879d0 - 7.6333d0*y + 26.3423d0*y**2 - 47.6084d0*y**3 + 41.8498d0*y**4 - 14.2948d0*y**5 )  &  ! [W/C]
                  * IceStorage(IceNum)%ITSNomCap / TimeInterval / 10.0d0
      HLoss = 0.0d0
    CASE(ITSType_IceOnCoilExternal)
      y = XCurIceFrac
      UAIceCh = ( 1.3879d0 - 7.6333d0*y + 26.3423d0*y**2 - 47.6084d0*y**3 + 41.8498d0*y**4 - 14.2948d0*y**5 )  &     ! [W/C]
               * IceStorage(IceNum)%ITSNomCap / TimeInterval / 10.0d0
      y = 1.0d0-XCurIceFrac
      UAIceDisCh = ( 1.1756d0 - 5.3689d0*y + 17.3602d0*y**2 - 30.1077d0*y**3 + 25.6387d0*y**4 - 8.5102d0*y**5 )  &   ! [W/C]
                  * IceStorage(IceNum)%ITSNomCap / TimeInterval / 10.0d0
      HLoss = 0.0d0

  END SELECT

RETURN
END SUBROUTINE CalcUAIce


REAL(r64) FUNCTION CalcDetIceStorLMTDstar(Tin, Tout, Tfr)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the log mean temperature difference for
          ! the detailed ice storage unit.  The temperature difference is non-
          ! dimensionalized using a nominal temperature difference of 10C.
          ! This value must be used when obtaining the curve fit coefficients.

          ! METHODOLOGY EMPLOYED:
          ! Straight-forward calculation where:
          ! LMTD* = LMTD/Tnom
          ! LMTD = (Tin-Tout)/ln((Tin-Tfr)/(Tout-Tfr))

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: Tin   ! ice storage unit inlet temperature
  REAL(r64), INTENT(IN) :: Tout  ! ice storage unit outlet (setpoint) temperature
  REAL(r64), INTENT(IN) :: Tfr   ! freezing temperature

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Tnom =10.0d0         ! Nominal temperature difference across the ice storage unit [C]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DeltaTio  ! Inlet to outlet temperature difference
  REAL(r64) :: DeltaTif  ! Inlet to freezing temperature difference
  REAL(r64) :: DeltaTof  ! Outlet to freezing temperature difference

          ! FLOW:
          ! First set the temperature differences and avoid problems with the LOG
          ! term by setting some reasonable minimums
  DeltaTio = ABS(Tin-Tout)
  DeltaTif = ABS(Tin-Tfr)
  DeltaTof = ABS(Tout-Tfr)

  IF (DeltaTif < DeltaTifMin) DeltaTif = DeltaTifMin
  IF (DeltaTof < DeltaTofMin) DeltaTof = DeltaTofMin

  CalcDetIceStorLMTDstar = ( DeltaTio / LOG(DeltaTif/DeltaTof) ) / Tnom

  RETURN

END FUNCTION CalcDetIceStorLMTDstar


! *****************************************************************************
REAL(r64) FUNCTION TempSItoIP(Temp)
  IMPLICIT NONE
  REAL(r64), INTENT(IN) :: Temp

  TempSItoIP = (Temp*9.0d0/5.0d0)+32.0d0
  RETURN
END FUNCTION

! *****************************************************************************
REAL(r64) FUNCTION TempIPtoSI(Temp)
  IMPLICIT NONE
  REAL(r64), INTENT(IN) :: Temp

  TempIPtoSI = (Temp-32.0d0)*5.0d0/9.0d0
  RETURN
END FUNCTION


! *****************************************************************************
SUBROUTINE UpdateNode(MyLoad,RunFlag,Num)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    October 1998

            ! PURPOSE OF THIS SUBROUTINE:

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:
  USE PlantUtilities, ONLY : SafeCopyPlantNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)            :: MyLoad
  LOGICAL              :: RunFlag
  INTEGER              :: Num

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW

       ! Update Node Inlet & Outlet MassFlowRat
  CALL SafeCopyPlantNode(InletNodeNum, OutletNodeNum)
  IF (MyLoad ==0  .OR. .NOT. RunFlag ) THEN
         ! Update Outlet Conditions so that same as Inlet, so component can be bypassed if necessary
    Node(OutletNodeNum)%Temp          = Node(InletNodeNum)%Temp
  ELSE
    Node(OutletNodeNum)%Temp          = ITSOutletTemp
  END IF

!! ??? For now, always set outletnode mass flow equal to inletnode mass flow
!! ??? Node(InletNodeNum)%MassFlowRate   = ITSMassFlowRate
!! ???  Node(OutletNodeNum)%MassFlowRate  = ITSMassFlowRate
!
!!  IF (Node(InletNodeNum)%MassFlowRate > 0.0) THEN
!    Node(OutletNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRate
!!  ELSE
!!    Node(InletNodeNum)%MassFlowRate  = Node(InletNodeNum)%MassFlowRateMaxAvail
!!    Node(OutletNodeNum)%MassFlowRate = Node(InletNodeNum)%MassFlowRateMaxAvail
!!  ENDIF
!
!  Node(OutletNodeNum)%MassFlowRateMax = Node(InletNodeNum)%MassFlowRateMax
!  Node(OutletNodeNum)%MassFlowRateMin = Node(InletNodeNum)%MassFlowRateMin
!  Node(OutletNodeNum)%MassFlowRateMaxAvail = Node(InletNodeNum)%MassFlowRateMaxAvail
!  Node(OutletNodeNum)%MassFlowRateMinAvail = Node(InletNodeNum)%MassFlowRateMinAvail

RETURN
END SUBROUTINE UpdateNode


! *****************************************************************************
SUBROUTINE RecordOutput(IceNum,MyLoad,RunFlag)

            ! SUBROUTINE INFORMATION:

            ! PURPOSE OF THIS SUBROUTINE:

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: IceNum
  REAL(r64), INTENT(IN) :: MyLoad
  LOGICAL,   INTENT(IN) :: RunFlag

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW

  IF (MyLoad ==0  .OR. .NOT. RunFlag ) THEN
    IceStorageReport(IceNum)%MyLoad = MyLoad
    IceStorageReport(IceNum)%U = U
    IceStorageReport(IceNum)%Urate = Urate
    IceStorageReport(IceNum)%ITSCoolingRate   = 0.0d0
    IceStorageReport(IceNum)%ITSCoolingEnergy = 0.0d0
    IceStorageReport(IceNum)%ITSChargingRate   = 0.0d0
    IceStorageReport(IceNum)%ITSChargingEnergy = 0.0d0
    IceStorageReport(IceNum)%ITSmdot          = 0.0d0
    IceStorageReport(IceNum)%ITSInletTemp     = ITSInletTemp
    IceStorageReport(IceNum)%ITSOutletTemp    = ITSOutletTemp

  ELSE
    IceStorageReport(IceNum)%MyLoad = MyLoad
    IceStorageReport(IceNum)%U = U
    IceStorageReport(IceNum)%Urate = Urate
    IF(ITSCoolingRate > 0.0d0) THEN
      IceStorageReport(IceNum)%ITSCoolingRate   = ITSCoolingRate
      IceStorageReport(IceNum)%ITSCoolingEnergy = ITSCoolingEnergy
      IceStorageReport(IceNum)%ITSChargingRate   = 0.0d0
      IceStorageReport(IceNum)%ITSChargingEnergy = 0.0d0
    ELSE
      IceStorageReport(IceNum)%ITSCoolingRate   = 0.0d0
      IceStorageReport(IceNum)%ITSCoolingEnergy = 0.0d0
      IceStorageReport(IceNum)%ITSChargingRate   = -ITSCoolingRate
      IceStorageReport(IceNum)%ITSChargingEnergy = -ITSCoolingEnergy
    ENDIF
    IceStorageReport(IceNum)%ITSmdot          = ITSMassFlowRate
    IceStorageReport(IceNum)%ITSInletTemp     = ITSInletTemp
    IceStorageReport(IceNum)%ITSOutletTemp    = ITSOutletTemp

  END IF


RETURN
END SUBROUTINE RecordOutput

! *****************************************************************************
SUBROUTINE UpdateIceFractions

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mike Witte
          !       DATE WRITTEN   September 2005
          !       MODIFIED       Rick Strand (Feb 2006, for detailed ice storage model)
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Update all ice fractions at end of system time step.

          ! METHODOLOGY EMPLOYED:
          ! This is called from HVACManager once we have actually stepped forward
          ! a system time step.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IceNum2

          ! FLOW

  DO IceNum2 = 1, NumIceStorages
    IceStorageReport(IceNum2)%IceFracRemain = IceStorageReport(IceNum2)%IceFracRemain + &
        IceStorageReport(IceNum2)%Urate*TimeStepSys
    IF( IceStorageReport(IceNum2)%IceFracRemain .LE. 0.001d0) IceStorageReport(IceNum2)%IceFracRemain = 0.0d0
    IF( IceStorageReport(IceNum2)%IceFracRemain .GT. 1.0d0  ) IceStorageReport(IceNum2)%IceFracRemain = 1.0d0
  END DO

  DO IceNum2 = 1, NumDetIceStorages
    DetIceStor(IceNum2)%IceFracRemaining = DetIceStor(IceNum2)%IceFracRemaining &
                                          +DetIceStor(IceNum2)%IceFracChange &
                                          -(DetIceStor(IceNum2)%TankLossCoeff*TimeStepSys)
    IF( DetIceStor(IceNum2)%IceFracRemaining < 0.001d0) DetIceStor(IceNum2)%IceFracRemaining = 0.0d0
    IF( DetIceStor(IceNum2)%IceFracRemaining > 1.000d0) DetIceStor(IceNum2)%IceFracRemaining = 1.0d0
         ! Reset the ice on the coil to zero for inside melt whenever discharging takes place.
         ! This assumes that any remaining ice floats away from the coil and resettles perfectly.
         ! While this is not exactly what happens and it is possible theoretically to have multiple
         ! freeze thaw cycles that are not complete, this is the best we can do.
    IF (DetIceStor(IceNum2)%ThawProcessIndex == DetIceInsideMelt) THEN
      IF (DetIceStor(IceNum2)%IceFracChange < 0.0d0) THEN
        DetIceStor(IceNum2)%IceFracOnCoil = 0.0d0
      ELSE
         ! Assume loss term does not impact ice on the coil but what is remaining
        DetIceStor(IceNum2)%IceFracOnCoil = DetIceStor(IceNum2)%IceFracOnCoil &
                                           +DetIceStor(IceNum2)%IceFracChange
         ! If the ice remaining has run out because of tank losses, reset ice fraction on coil so that it keeps track of losses
        IF (DetIceStor(IceNum2)%IceFracOnCoil > DetIceStor(IceNum2)%IceFracRemaining) &
            DetIceStor(IceNum2)%IceFracOnCoil = DetIceStor(IceNum2)%IceFracRemaining
      END IF
    ELSE ! Outside melt system so IceFracOnCoil is always the same as IceFracRemaining (needs to be done for reporting only)
      DetIceStor(IceNum2)%IceFracOnCoil = DetIceStor(IceNum2)%IceFracRemaining
    END IF
  END DO

  RETURN

END SUBROUTINE UpdateIceFractions


SUBROUTINE UpdateDetailedIceStorage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine takes the necessary information from the local data
          ! structure and moves it back to the loop node data structure.

          ! METHODOLOGY EMPLOYED:
          ! Not much mystery here--just move the data to the appropriate place
          ! for the detailed ice storage system in question.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY : SafeCopyPlantNode

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
  INTEGER :: InNodeNum  ! Plant inlet node number for component
  INTEGER :: OutNodeNum ! Plant outlet node number for component

          ! FLOW:
          ! Set the temperature and flow rate for the component outlet node
  InNodeNum  = DetIceStor(IceNum)%PlantInNodeNum
  OutNodeNum = DetIceStor(IceNum)%PlantOutNodeNum

  CALL SafeCopyPlantNode(InNodeNum, OutNodeNum)

  Node(OutNodeNum)%Temp                 = DetIceStor(IceNum)%OutletTemp


  RETURN

END SUBROUTINE UpdateDetailedIceStorage


SUBROUTINE ReportDetailedIceStorage

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports all of the output necessary for the model.

          ! METHODOLOGY EMPLOYED:
          ! Just take what has already been calculated or calculate the appropriate
          ! output value based on simulation data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), PARAMETER :: LowLoadLimit = 0.1d0 ! Load below which device can be assumed off [W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
          ! Determine what is going on  based on load and the inlet and outlet temperature comparison

  IF (DetIceStor(IceNum)%CompLoad < LowLoadLimit) THEN  ! No load condition

    DetIceStor(IceNum)%IceFracChange       = 0.0d0
    DetIceStor(IceNum)%DischargingRate     = 0.0d0
    DetIceStor(IceNum)%DischargingEnergy   = 0.0d0
    DetIceStor(IceNum)%ChargingRate        = 0.0d0
    DetIceStor(IceNum)%ChargingEnergy      = 0.0d0
    DetIceStor(IceNum)%ParasiticElecRate   = 0.0d0
    DetIceStor(IceNum)%ParasiticElecEnergy = 0.0d0

  ELSE  ! There is a load, determine whether we are charging or discharging based on inlet and outlet temperature

    IF (DetIceStor(IceNum)%InletTemp < DetIceStor(IceNum)%OutletTemp) THEN  ! Charging Mode

      DetIceStor(IceNum)%ChargingRate        = DetIceStor(IceNum)%CompLoad
      DetIceStor(IceNum)%ChargingEnergy      = DetIceStor(IceNum)%CompLoad * (TimeStepSys*SecInHour)
      DetIceStor(IceNum)%IceFracChange       = DetIceStor(IceNum)%CompLoad * TimeStepSys / DetIceStor(IceNum)%NomCapacity
      DetIceStor(IceNum)%DischargingRate     = 0.0d0
      DetIceStor(IceNum)%DischargingEnergy   = 0.0d0
      DetIceStor(IceNum)%ParasiticElecRate   = DetIceStor(IceNum)%ChargeParaElecLoad * DetIceStor(IceNum)%CompLoad
      DetIceStor(IceNum)%ParasiticElecEnergy = DetIceStor(IceNum)%ChargeParaElecLoad * DetIceStor(IceNum)%ChargingEnergy

    ELSE    ! (DetIceStor(IceNum)%InletTemp < DetIceStor(IceNum)%OutletTemp) Discharging Mode

      DetIceStor(IceNum)%DischargingRate     = DetIceStor(IceNum)%CompLoad
      DetIceStor(IceNum)%DischargingEnergy   = DetIceStor(IceNum)%CompLoad * (TimeStepSys*SecInHour)
      DetIceStor(IceNum)%IceFracChange       =-DetIceStor(IceNum)%CompLoad * TimeStepSys / DetIceStor(IceNum)%NomCapacity
      DetIceStor(IceNum)%ChargingRate        = 0.0d0
      DetIceStor(IceNum)%ChargingEnergy      = 0.0d0
      DetIceStor(IceNum)%ParasiticElecRate   = DetIceStor(IceNum)%DischargeParaElecLoad * DetIceStor(IceNum)%CompLoad
      DetIceStor(IceNum)%ParasiticElecEnergy = DetIceStor(IceNum)%DischargeParaElecLoad * DetIceStor(IceNum)%ChargingEnergy

    END IF

  END IF

  RETURN

END SUBROUTINE ReportDetailedIceStorage

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

END MODULE IceThermalStorage

