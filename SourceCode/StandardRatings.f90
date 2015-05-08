MODULE StandardRatings

  ! MODULE INFORMATION:
  !       AUTHOR         Chandan Sharma
  !       DATE WRITTEN   February 2012
  !       MODIFIED       February 2013, Bereket Nigusse
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module contains the subroutines required to calculate the following standard ratings of HVAC equipment
  ! 1) Integrated Part Load Value (IPLV) rating for EIR and Reformulated EIR chillers
  ! 2) a) Standard Rated (net) Cooling Capacity
  !    b) Seasonal Energy Efficiency Ratio (SEER)
  !    c) Energy Efficiency Ratio (EER),
  !    d) Integrated Energy Efficiency Ratio (IEER)
  !       for Air-to-Air Direct Expansion Air Conditioner and Heat Pumps having a single-speed compressor,
  !       fixed speed indoor supply air fan, and air-cooled condensers.
  ! 3) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion Heat Pumps having a single-speed compressor,
  !       fixed speed indoor supply air fan
  ! 4) Seasonal Energy Efficiency Ratio (SEER) for Air-Source Direct Expansion multi-speed compressor Heat Pumps
  ! 5) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion multi-speed compressor Heat Pumps
  !
  ! METHODOLOGY EMPLOYED:
  ! Using the user specified reference capacity, reference COP and performance curves, the chiller or DX coil models are executed
  ! for standard test conditions as specified in ANSI/AHRI 550/590, 210/240 and 340/360. Then results of the simulated test points
  ! are processed into standard ratings according to standard's procedures.

  ! REFERENCES:
  ! (1) AHRI Standard 550/590-2011:  Standard for Performance Rating of Water-Chilling Packages using the Vapor
  !                                  Compression Cycle. Arlington, VA:  Air-Conditioning, Heating,
  !                                  and Refrigeration Institute.
  ! (2) ANSI/AHRI Standard 210/240-2008:  Standard for Performance Rating of Unitary Air-Conditioning and
  !                                       Air-Source Heat Pumps. Arlington, VA:  Air-Conditioning, Heating
  !                                       , and Refrigeration Institute.
  ! (3) ANSI/AHRI Standard 340/360-2007:  Standard for Performance Rating of Commercial and Industrial
  !                                       Unitary Air-Conditioning and Heat Pump Equipment.  Arlington,
  !                                       VA:  Air-Conditioning, Heating, and Refrigeration Institute.

  ! OTHER NOTES: none

  ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE

REAL(r64), PARAMETER ::    IndoorCoilInletAirWetbulbTempRated   = 19.44d0  ! 19.44C (67F)  Tests A2, B2, B1, and F1
REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempRated  = 35.0d0   ! 35.00C (95F)  Tests A2, B2, B1, and F1
REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestA2 = 35.0d0   ! 35.00C (95F)  Test A2 (high speed)
REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestB2 = 27.78d0  ! 27.78C (82F)  Test B2 (high speed)
REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestB1 = 27.78d0  ! 27.78C (82F)  Test B1 (Low speed)
REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestF1 = 19.44d0  ! 19.44C (67F)  Test B1 (Low speed)

  ! AHRI Standard 210/240-2008 Performance Test Conditions for Unitary Air-to-Air Air-Conditioning and Heat Pump Equipment
REAL(r64), PARAMETER ::    CoolingCoilInletAirWetbulbTempRated = 19.44d0   ! 19.44C (67F)  Tests A and B
REAL(r64), PARAMETER ::    OutdoorUnitInletAirDrybulbTemp = 27.78d0        ! 27.78C (82F)  Test B (for SEER)
REAL(r64), PARAMETER ::    OutdoorUnitInletAirDrybulbTempRated = 35.0d0    ! 35.00C (95F)  Test A (rated capacity)
REAL(r64), PARAMETER ::    AirMassFlowRatioRated = 1.0d0                   ! AHRI test is at the design flow rate
                                                                           ! and hence AirMassFlowRatio is 1.0
REAL(r64), PARAMETER ::    ConvFromSIToIP = 3.412141633D0                  ! Conversion from SI to IP [3.412 Btu/hr-W]
REAL(r64), PARAMETER ::    DefaultFanPowerPerEvapAirFlowRate = 773.3D0     ! 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
                                                      ! specifies a nominal/default fan electric power consumption per rated air
                                                      ! volume flow rate to account for indoor fan electric power consumption
                                                      ! when the standard tests are conducted on units that do not have an
                                                      ! indoor air circulting fan. Used if user doesn't enter a specific value.
REAL(r64), PARAMETER               :: PLRforSEER = 0.5d0  ! Part-load ratio for SEER calculation (single speed DX cooling coils)
REAL(r64), PARAMETER, DIMENSION(4) :: ReducedPLR = (/1.0D0, 0.75d0,0.50d0,0.25d0/)  ! Reduced Capacity part-load conditions
REAL(r64), PARAMETER, DIMENSION(4) :: IEERWeightingFactor = (/0.020D0, 0.617D0, 0.238D0, 0.125D0/) ! EER Weighting factors (IEER)
REAL(r64), PARAMETER               :: OADBTempLowReducedCapacityTest = 18.3D0 ! Outdoor air dry-bulb temp in degrees C (65F)
                                                      ! Std. AHRI AHRI 340/360 Dry-bulb Temp at reduced capacity, <= 0.444

! Defrost control  (heat pump only)
INTEGER,   PARAMETER                :: Timed                     = 1      ! defrost cycle is timed
INTEGER,   PARAMETER                :: OnDemand                  = 2      ! defrost cycle occurs only when required
INTEGER,   PARAMETER                :: TotalNumOfStandardDHRs    = 16     ! Total number of standard design heating requirements
INTEGER,   PARAMETER, DIMENSION(6)  :: TotalNumOfTemperatureBins = (/9, 10, 13, 15, 18, 9/) ! Total number of temperature
                                                                                            ! bins for a region
REAL(r64), PARAMETER, DIMENSION(16) :: StandardDesignHeatingRequirement = &
                                                            (/1465.36D0, 2930.71D0, 4396.07D0, 5861.42D0, &
                                                              7326.78D0, 8792.14D0, 10257.49D0, 11722.85D0, &
                                                             14653.56D0, 17584.27D0, 20514.98D0, 23445.70D0, &
                                                             26376.41D0, 29307.12D0, 32237.83D0, 38099.26D0/)
                                                                 ! Standardized DHRs from ANSI/AHRI 210/240
REAL(r64), PARAMETER                :: CorrectionFactor = 0.77D0 ! A correction factor which tends to improve the agreement
                                                                 ! between calculated and measured building loads, dimensionless.
REAL(r64), PARAMETER                :: CyclicDegradationCoeff = 0.25D0
REAL(r64), PARAMETER, DIMENSION(6)  :: OutdoorDesignTemperature = (/2.78D0, -2.78D0, -8.33D0, -15.0D0, -23.33D0, -1.11D0/)
                                    ! Outdoor design temperature for a region from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::OutdoorBinTemperature = (/16.67D0, 13.89D0, 11.11D0, 8.33D0, 5.56D0, 2.78D0, 0.00D0, &
                                                                -2.78D0, -5.56D0, -8.33D0, -11.11D0, -13.89D0, -16.67D0, &
                                                                -19.44D0, -22.22D0, -25.00D0, -27.78D0, -30.56D0 /)
                                    ! Fractional bin hours for different bin temperatures for region one, from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::RegionOneFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.291D0, 0.239D0, 0.194D0, 0.129D0, 0.081D0, 0.041D0, &
                                                              0.019D0, 0.005D0, 0.001D0, 0.0D0, 0.0D0, 0.0D0, &
                                                              0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
                                    ! Fractional bin hours for different bin temperatures for region two, from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::RegionTwoFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.215D0, 0.189D0, 0.163D0, 0.143D0, 0.112D0, 0.088D0, &
                                                              0.056D0, 0.024D0, 0.008D0, 0.002D0, 0.0D0, 0.0D0, 0.0D0, &
                                                              0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
                                    ! Fractional bin hours for different bin temperatures for region three, from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::RegionThreeFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.153D0, 0.142D0, 0.138D0, 0.137D0, 0.135D0, 0.118D0, &
                                                              0.092D0, 0.047D0, 0.021D0, 0.009D0, 0.005D0, 0.002D0, &
                                                              0.001D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0/)
                                    ! Fractional bin hours for different bin temperatures for region four, from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::RegionFourFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.132D0, 0.111D0, 0.103D0, 0.093D0, 0.1D0, 0.109D0,   &
                                                              0.126D0, 0.087D0, 0.055D0, 0.036D0, 0.026D0, 0.013D0, &
                                                              0.006D0, 0.002D0, 0.001D0, 0.0D0, 0.0D0, 0.0D0/)
                                    ! Fractional bin hours for different bin temperatures for region five, from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::RegionFiveFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.106D0, 0.092D0, 0.086D0, 0.076D0, 0.078D0, 0.087D0, &
                                                              0.102D0, 0.094D0, 0.074D0, 0.055D0, 0.047D0, 0.038D0, &
                                                              0.029D0, 0.018D0, 0.01D0, 0.005D0, 0.002D0, 0.001D0/)
                                    ! Fractional bin hours for different bin temperatures for region six, from ANSI/AHRI 210/240
REAL(r64), PARAMETER, DIMENSION(18) ::RegionSixFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.113D0, 0.206D0, 0.215D0, 0.204D0, 0.141D0, 0.076D0,  &
                                                              0.034D0, 0.008D0, 0.003D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                                                              0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0/)

! Representative cooling season Outdoor air temperature bin from ANSI/AHRI 210/240-2008
INTEGER,   PARAMETER                             :: NumOfOATempBins = 8  ! number of outdoor temperature bins for cooling season
REAL(r64), PARAMETER, DIMENSION(NumOfOATempBins) :: OutdoorBinTemperatureSEER = &
                                                            (/19.44D0, 22.22D0, 25.00D0, 27.78D0, 30.56D0, 33.33D0, &
                                                              36.11D0, 38.89D0/)
! Fractional bin hours for different bin temperatures for cooling, from ANSI/AHRI 210/240 - 2008
REAL(r64), PARAMETER, DIMENSION(NumOfOATempBins) :: CoolFracBinHoursAtOutdoorBinTemp = &
                                                            (/0.214D0, 0.231D0, 0.216D0, 0.161D0, 0.104D0, 0.052D0, &
                                                              0.018D0, 0.004D0/)

REAL(r64), PARAMETER :: HeatingIndoorCoilInletAirDBTempRated   = 21.11d0 ! Heating coil entering air dry-bulb temperature in
                                                                         ! degrees C (70F) Test H1, H2 and H3
                                                                         ! (low and High Speed) Std. AHRI 210/240
REAL(r64), PARAMETER :: HeatingOutdoorCoilInletAirDBTempH0Test = 16.67d0 ! Outdoor air dry-bulb temp in degrees C (47F)
                                                                         ! Test H0 (low and High Speed) Std. AHRI 210/240
REAL(r64), PARAMETER :: HeatingOutdoorCoilInletAirDBTempRated  = 8.33d0  ! Outdoor air dry-bulb temp in degrees C (47F)
                                                                         ! Test H1 or rated (low and High Speed) Std. AHRI 210/240
REAL(r64), PARAMETER :: HeatingOutdoorCoilInletAirDBTempH2Test = 1.67d0  ! Outdoor air dry-bulb temp in degrees C (35F)
                                                                         ! Test H2 (low and High Speed) Std. AHRI 210/240
REAL(r64), PARAMETER :: HeatingOutdoorCoilInletAirDBTempH3Test = -8.33d0 ! Outdoor air dry-bulb temp in degrees C (17F)
                                                                         ! Test H3 (low and High Speed) Std. AHRI 210/240

PUBLIC  CalcChillerIPLV
PRIVATE ReportChillerIPLV
PRIVATE ReformEIRChillerCondInletTempResidual
PRIVATE CheckCurveLimitsForIPLV

PUBLIC  CalcDXCoilStandardRating
PRIVATE SingelSpeedDXCoolingCoilStandardRatings
PRIVATE SingleSpeedDXHeatingCoilStandardRatings
PRIVATE MultiSpeedDXCoolingCoilStandardRatings
PRIVATE MultiSpeedDXHeatingCoilStandardRatings
PRIVATE CheckCurveLimitsForStandardRatings
PRIVATE ReportDXCoilRating

CONTAINS

SUBROUTINE CalcChillerIPLV(ChillerName, ChillerType, RefCap, RefCOP, CondenserType, CapFTempCurveIndex, &
                           EIRFTempCurveIndex, EIRFPLRCurveIndex, MinUnLoadRat, EvapVolFlowRate, &
                           CondLoopNum, OpenMotorEff)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2012
          !       Modified       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     Calculates Integrated Part Load Value (IPLV) for EIR and reformulated EIR chillers.
          !     Writes the result to EIO file.
          !
          ! METHODOLOGY EMPLOYED:
          ! (1) Obtains the reference cooling capacity, reference COP and performance curves of the chiller
          !
          ! (2) Evaluates the cooling capacity at AHRI test conditions (Per AHRI 551/591,2011 Table 3)
          !
          ! (3) Evaluates the EIR at AHRI test conditions (Per AHRI 551/591,2011 Table 3)
          !
          ! (4) The EER is evaluated from the total cooling capacity and total electric power
          !     evaluated at the standard rated test conditions.  The IPLV is a weighted value of the COP evaluated
          !     at four different capacities of 100%, 75%, 50% and 25%.  The reduced capacity COPs are evaluated
          !     at different outdoor coil entering temperatures.
          !
          ! REFERENCES:
          ! (1) AHRI Standard 551/591-2011:  Standard for Performance Rating of Water-Chilling Packages using the Vapor
          !                                  Compression Cycle. Arlington, VA:  Air-Conditioning, Heating,
          !                                  and Refrigeration Institute.

          ! USE STATEMENTS:

  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE DataPlant,       ONLY: PlantLoop,TypeOf_Chiller_ElectricEIR, TypeOf_Chiller_ElectricReformEIR
  USE CurveManager,    ONLY: CurveValue, GetCurveType, GetCurveName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: ChillerName         ! Name of Chiller for which IPLV is calculated
  INTEGER, INTENT(IN)              :: ChillerType         ! Type of Chiller - EIR or Reformulated EIR
  INTEGER, INTENT(IN)              :: CondenserType       ! Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
  INTEGER, INTENT(IN)              :: CapFTempCurveIndex  ! Index for the total cooling capacity modifier curve
                                                          ! (function of leaving chilled water temperature and
                                                          !  entering condenser fluid temperature)
  INTEGER, INTENT(IN)              :: EIRFTempCurveIndex  ! Index for the energy input ratio modifier curve
                                                          ! (function of leaving chilled water temperature and
                                                          !  entering condenser fluid temperature)
  INTEGER, INTENT(IN)              :: EIRFPLRCurveIndex   ! Index for the EIR vs part-load ratio curve
  REAL(r64), INTENT(IN)            :: RefCap              ! Reference capacity of chiller [W]
  REAL(r64), INTENT(IN)            :: RefCOP              ! Reference coefficient of performance [W/W]
  REAL(r64), INTENT(IN)            :: MinUnLoadRat        ! Minimum unloading ratio
  REAL(r64), INTENT(IN), OPTIONAL  :: EvapVolFlowRate     ! Reference water volumetric flow rate through the evaporator [m3/s]
  REAL(r64), INTENT(IN), OPTIONAL  :: OpenMotorEff        ! Open chiller motor efficiency [fraction, 0 to 1]
  INTEGER,   INTENT(IN), OPTIONAL  :: CondLoopNum         ! condenser water plant loop index number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER :: AirCooled       = 1
  INTEGER,   PARAMETER :: WaterCooled     = 2
  INTEGER,   PARAMETER :: EvapCooled      = 3

  REAL(r64), PARAMETER :: EvapOutletTemp  = 6.67d0        ! (44F)
  REAL(r64), PARAMETER :: Acc             = 0.0001d0      ! Accuracy of result
  REAL(r64), PARAMETER :: ConvFromSIToIP  = 3.412141633D0 ! Conversion from SI to IP [3.412 Btu/hr-W]
  INTEGER,   PARAMETER :: NumOfReducedCap = 4             ! Number of reduced capacity test conditions (100%,75%,50%,and 25%)
  INTEGER,   PARAMETER :: IterMax         = 500           ! Maximum number of iterations
  REAL(r64), PARAMETER, DIMENSION(4) :: ReducedPLR = (/1.0D0, 0.75d0,0.50d0,0.25d0/)  ! Reduced Capacity part-load conditions
  REAL(r64), PARAMETER, DIMENSION(4) :: IPLVWeightingFactor = (/0.010D0, 0.42D0, 0.45D0, 0.12D0/) ! EER Weighting factors (IPLV)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: AvailChillerCap                = 0.0D0   ! Chiller available capacity at current operating conditions [W]
  REAL(r64) :: EnteringWaterTempReduced       = 0.0D0   ! Entering Condenser Water Temperature at reduced conditions [C]
  REAL(r64) :: EnteringAirDrybulbTempReduced  = 0.0D0   ! Outdoor unit entering air dry-bulb temperature
                                                        ! at reduced capacity [C]
  REAL(r64) :: EnteringAirWetbulbTempReduced  = 0.0D0   ! Outdoor unit entering air wet-bulb temperature
                                                        ! at reduced capacity [C]
  REAL(r64) :: CondenserInletTemp        = 0.0D0   ! Entering Condenser Temperature at reduced conditions [C]
  REAL(r64) :: CondenserOutletTemp0      = 0.0D0   ! Lower bound for condenser outlet temperature [C]
  REAL(r64) :: CondenserOutletTemp1      = 0.0D0   ! Upper bound for condenser outlet temperature [C]
  REAL(r64) :: CondenserOutletTemp       = 0.0D0   ! Calculated condenser outlet temperature which corresponds
                                                   ! to EnteringWaterTempReduced above [C]
  REAL(r64) :: Cp                        = 0.0D0   ! Water specific heat [J/(kg*C)]
  REAL(r64) :: Rho                       = 0.0D0   ! Water density [kg/m3]
  REAL(r64) :: IPLV                      = 0.0D0   ! Integerated Part Load Value in SI [W/W]
  REAL(r64) :: EIR                       = 0.0D0   ! Inverse of COP at reduced capacity test conditions (100%, 75%, 50%, and 25%)
  REAL(r64) :: Power                     = 0.0D0   ! Power at reduced capacity test conditions (100%, 75%, 50%, and 25%)
  REAL(r64) :: COPReduced                = 0.0D0   ! COP at reduced capacity test conditions (100%, 75%, 50%, and 25%)
  REAL(r64) :: LoadFactor                = 0.0D0   ! Fractional "on" time for last stage at the desired reduced capacity,
                                                   ! (dimensionless)
  REAL(r64) :: DegradationCoeff          = 0.0D0   ! Degradation coeficient, (dimenssionless)
  REAL(r64) :: ChillerCapFT              = 0.0D0   ! Chiller capacity fraction (evaluated as a function of temperature)
  REAL(r64) :: ChillerEIRFT              = 0.0D0   ! Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
  REAL(r64) :: ChillerEIRFPLR            = 0.0D0   ! Chiller EIR as a function of part-load ratio (PLR)
  REAL(r64) :: PartLoadRatio             = 0.0D0   ! Part load ratio (PLR) at which chiller is operatign at reduced capacity
  INTEGER   :: RedCapNum                           ! Integer counter for reduced capacity
  INTEGER   :: SolFla                              ! Flag of solver
  REAL(r64), DIMENSION(11)  :: Par                 ! Parameter array need for RegulaFalsi routine

! Initialize local variables
  AvailChillerCap                = 0.0D0
  EnteringWaterTempReduced       = 0.0D0
  EnteringAirDrybulbTempReduced  = 0.0D0
  EnteringAirWetbulbTempReduced  = 0.0D0
  CondenserInletTemp             = 0.0D0
  CondenserOutletTemp0           = 0.0D0
  CondenserOutletTemp1           = 0.0D0
  CondenserOutletTemp            = 0.0D0
  Cp                             = 0.0D0
  Rho                            = 0.0D0
  IPLV                           = 0.0D0
  EIR                            = 0.0D0
  Power                          = 0.0D0
  COPReduced                     = 0.0D0
  LoadFactor                     = 0.0D0
  DegradationCoeff               = 0.0D0
  ChillerCapFT                   = 0.0D0
  ChillerEIRFT                   = 0.0D0
  ChillerEIRFPLR                 = 0.0D0
  PartLoadRatio                  = 0.0D0


  CALL CheckCurveLimitsForIPLV(ChillerName, ChillerType, CondenserType, CapFTempCurveIndex, EIRFTempCurveIndex)

  ! IPLV calculations:
  DO RedCapNum = 1, NumOfReducedCap
    IF (CondenserType == WaterCooled) THEN
        ! get the entering water temperature for the reduced capacity test conditions
        IF (ReducedPLR(RedCapNum) > 0.50D0 ) THEN
            EnteringWaterTempReduced = 8.0D0 + 22.0D0 * ReducedPLR(RedCapNum)
        ELSE
            EnteringWaterTempReduced = 19.0D0
        ENDIF
        CondenserInletTemp = EnteringWaterTempReduced
    ELSEIF (CondenserType == AirCooled) THEN
        ! get the outdoor air dry bulb temperature for the reduced capacity test conditions
        IF (ReducedPLR(RedCapNum) > 0.3125D0 ) THEN
            EnteringAirDrybulbTempReduced = 3.0D0 + 32.0D0 * ReducedPLR(RedCapNum)
        ELSE
            EnteringAirDrybulbTempReduced = 13.0D0
        ENDIF
        CondenserInletTemp = EnteringAirDrybulbTempReduced
    ELSE ! EvaporativelyCooled Condenser
        ! get the outdoor air wet bulb temperature for the reduced capacity test conditions
        EnteringAirWetbulbTempReduced = 10.0D0 + 14.0D0 * ReducedPLR(RedCapNum)
        CondenserInletTemp = EnteringAirWetbulbTempReduced
    ENDIF

    SELECT CASE (ChillerType)

        CASE (TypeOf_Chiller_ElectricEIR)
        ! Get capacity curve info with respect to CW setpoint and entering condenser temps
        ChillerCapFT = CurveValue(CapFTempCurveIndex, EvapOutletTemp,CondenserInletTemp)

        ChillerEIRFT   = CurveValue(EIRFTempCurveIndex,EvapOutletTemp,CondenserInletTemp)

        IF (ReducedPLR(RedCapNum) .GE. MinUnLoadRat) THEN
            ChillerEIRFPLR  = CurveValue(EIRFPLRCurveIndex,ReducedPLR(RedCapNum))
            PartLoadRatio   = ReducedPLR(RedCapNum)
        ELSE
            ChillerEIRFPLR  = CurveValue(EIRFPLRCurveIndex,MinUnLoadRat)
            PartLoadRatio   = MinUnLoadRat
        ENDIF

        CASE (TypeOf_Chiller_ElectricReformEIR)
        Cp  = GetSpecificHeatGlycol(PlantLoop(CondLoopNum)%FluidName,  &
                                    EnteringWaterTempReduced,          &
                                    PlantLoop(CondLoopNum)%FluidIndex, &
                                    'CalcChillerIPLV')

        Rho  = GetDensityGlycol(PlantLoop(CondLoopNum)%FluidName,  &
                                EnteringWaterTempReduced,          &
                                PlantLoop(CondLoopNum)%FluidIndex, &
                                'CalcChillerIPLV')

        Par(1) = EnteringWaterTempReduced
        Par(2) = EvapOutletTemp
        Par(3) = Cp
        Par(4) = ReducedPLR(RedCapNum)
        Par(5) = EvapVolFlowRate * Rho
        Par(6) = CapFTempCurveIndex
        Par(7) = EIRFTempCurveIndex
        Par(8) = EIRFPLRCurveIndex
        Par(9) = RefCap
        Par(10) = RefCOP
        Par(11) = OpenMotorEff
        CondenserOutletTemp0 = EnteringWaterTempReduced + 0.1D0
        CondenserOutletTemp1 = EnteringWaterTempReduced + 10.0D0
        CALL SolveRegulaFalsi(Acc, IterMax, SolFla, CondenserOutletTemp, ReformEIRChillerCondInletTempResidual, &
                                CondenserOutletTemp0, CondenserOutletTemp1, Par)
        IF (SolFla == -1) THEN
            CALL ShowWarningError('Iteration limit exceeded in calculating Reform Chiller IPLV')
            CALL ShowContinueError('Reformulated Chiller IPLV calculation failed for '// TRIM(ChillerName))
        ELSE IF (SolFla == -2) THEN
            CALL ShowWarningError('Bad starting values for calculating Reform Chiller IPLV')
            CALL ShowContinueError('Reformulated Chiller IPLV calculation failed for '// TRIM(ChillerName))
        ENDIF

        ChillerCapFT = CurveValue(CapFTempCurveIndex, EvapOutletTemp,CondenserOutletTemp)

        ChillerEIRFT   = CurveValue(EIRFTempCurveIndex,EvapOutletTemp, CondenserOutletTemp)

        IF (ReducedPLR(RedCapNum) .GE. MinUnLoadRat) THEN
            ChillerEIRFPLR  = CurveValue(EIRFPLRCurveIndex,CondenserOutletTemp, ReducedPLR(RedCapNum))
            PartLoadRatio   = ReducedPLR(RedCapNum)
        ELSE
            ChillerEIRFPLR  = CurveValue(EIRFPLRCurveIndex,CondenserOutletTemp, MinUnLoadRat)
            PartLoadRatio   = MinUnLoadRat
        ENDIF
        CASE DEFAULT
        ! should not come here, do nothing
    END SELECT

    ! Available chiller capacity as a function of temperature
    IF ( RefCap > 0.0d0 .AND. RefCOP > 0.0d0 .AND. ChillerCapFT > 0.0d0 .AND. ChillerEIRFT > 0.0d0) THEN
        AvailChillerCap = RefCap * ChillerCapFT
        Power = (AvailChillerCap / RefCOP) * ChillerEIRFPLR * ChillerEIRFT
        EIR = Power / (PartLoadRatio * AvailChillerCap)

        IF (ReducedPLR(RedCapNum) .GE. MinUnLoadRat) THEN
            COPReduced = 1.0d0 / EIR
        ELSE
            LoadFactor = (ReducedPLR(RedCapNum) * RefCap) / (MinUnLoadRat * AvailChillerCap)
            DegradationCoeff = 1.130D0 - 0.130D0 * LoadFactor
            COPReduced = 1.0d0 / (DegradationCoeff * EIR)
        ENDIF
        IPLV = IPLV + IPLVWeightingFactor(RedCapNum) * COPReduced
    ELSE
        SELECT CASE (ChillerType)
        CASE (TypeOf_Chiller_ElectricEIR)
          CALL ShowWarningError('Chiller:Electric:EIR = '// &
                                TRIM(ChillerName)//': '//&
                                ' Integrated Part Load Value (IPLV) cannot be calculated.')
        CASE (TypeOf_Chiller_ElectricReformEIR)

          CALL ShowWarningError('Chiller:Electric:ReformulatedEIR = '// &
                                TRIM(ChillerName)//': '//&
                                ' Integrated Part Load Value (IPLV) cannot be calculated.')
        END SELECT
        IF ( RefCap <= 0.0d0) THEN
            CALL ShowContinueError(' Check the chiller autosized or user specified capacity. ' &
            //'Autosized or specified chiller capacity = '//TRIM(RoundSigDigits(RefCap,2)))
        ENDIF
        IF ( RefCOP <= 0.0d0) THEN
            CALL ShowContinueError(' Check the chiller reference or rated COP specified. ' &
            //'Specified COP = '//TRIM(RoundSigDigits(RefCOP,2)))
        ENDIF
        IF (ChillerCapFT <= 0.0d0) THEN
            CALL ShowContinueError(' Check limits in Cooling Capacity Function of Temperature Curve, '  &
            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))      &
            //', Curve Name = '//TRIM(GetCurveName(CapFTempCurveIndex))//'.')
            CALL ShowContinueError(' ..ChillerCapFT value at standard test condition = '//TRIM(RoundSigDigits(ChillerCapFT,2)))
        END IF
        IF (ChillerEIRFT <= 0.0d0) THEN
            CALL ShowContinueError(' Check limits in EIR Function of Temperature Curve, ' &
                //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex)) &
                //', Curve Name = '//TRIM(GetCurveName(EIRFTempCurveIndex))//'.')
            CALL ShowContinueError(' ..ChillerEIRFT value at standard test condition = '//TRIM(RoundSigDigits(ChillerEIRFT,2)))
        END IF
        IPLV = 0.0d0
        EXIT
    ENDIF
  END DO

  ! Writes the IPLV value to the EIO file and standard tabular output tables
  CALL ReportChillerIPLV( ChillerName, ChillerType, IPLV,IPLV * ConvFromSIToIP)

RETURN

END SUBROUTINE CalcChillerIPLV

FUNCTION ReformEIRChillerCondInletTempResidual(CondenserOutletTemp, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   February 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function as described below
          ! Residuum = (CondenserInletTempAtAHRIConditions - CondenserInletTemp) / CondenserInletTempAtAHRIConditions.
          ! CondenserInletTemp here depends on the CondenserOutletTemp which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Varies CondenserOutletTemp until a balance point exists where the model output corresponds to the desired
          ! independent variable (i.e. CondenserInletTemp is within tolerance of CondenserInletTempAtAHRIConditions)

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE CurveManager, ONLY: CurveValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: CondenserOutletTemp        ! Condenser outlet temperature (boundary condition or guess value) [C]
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1)  = Condenser inlet temperature at AHRI Standard
                                                       !           551/591 conditons[C]
                                                       ! par(2)  = Evaporator outlet temperature [C]
                                                       ! par(3)  = Water specific heat [J/(kg*C)]
                                                       ! par(4)  = Part load ratio
                                                       ! par(5)  = Evaporator mass flow rate [kg/s]
                                                       ! par(6)  = Index for the total cooling capacity modifier curve
                                                       ! par(7)  = Index for the energy input ratio modifier curve
                                                       ! par(8)  = Index for the EIR vs part-load ratio curve
                                                       ! par(9)  = Reference capacity of chiller [W]
                                                       ! par(10) = Reference coefficient of performance [W/W]
                                                       ! par(11) = Open chiller motor efficiency [fraction, 0 to 1]

  REAL(r64)              :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: AvailChillerCap          = 0.0D0  ! Chiller available capacity at current operating conditions [W]
  REAL(r64)    :: CondenserInletTemp       = 0.0D0  ! Calculated condenser inlet temperature [C]
  REAL(r64)    :: EvapOutletTemp           = 0.0D0  ! Evaporator outlet temperature temperature [C]
  REAL(r64)    :: QEvap                    = 0.0D0  ! Rate of heat transfer to the evaporator coil [W]
  REAL(r64)    :: QCond                    = 0.0D0  ! Rate of heat transfer to the condenser coil [W]
  REAL(r64)    :: Power                    = 0.0D0  ! Power at reduced capacity test conditions (100%, 75%, 50%, and 25%)
  REAL(r64)    :: ReformEIRChillerCapFT    = 0.0D0  ! Chiller capacity fraction (evaluated as a function of temperature)
  REAL(r64)    :: ReformEIRChillerEIRFT    = 0.0D0  ! Chiller electric input ratio (EIR = 1 / COP) as a function of temperature
  REAL(r64)    :: ReformEIRChillerEIRFPLR  = 0.0D0  ! Chiller EIR as a function of part-load ratio (PLR)

    EvapOutletTemp = Par(2)

    ReformEIRChillerCapFT = CurveValue(INT(Par(6)), EvapOutletTemp,CondenserOutletTemp)

    ReformEIRChillerEIRFT   = CurveValue(INT(Par(7)),EvapOutletTemp, CondenserOutletTemp)

    ! Available chiller capacity as a function of temperature
    AvailChillerCap = Par(9) * ReformEIRChillerCapFT

    ReformEIRChillerEIRFPLR  = CurveValue(INT(Par(8)),CondenserOutletTemp,Par(4))

    Power = (AvailChillerCap / Par(10)) * ReformEIRChillerEIRFPLR * ReformEIRChillerEIRFT

    QEvap = AvailChillerCap * Par(4)

    QCond = Power*Par(11) + QEvap

    IF (Par(6) .GT. MassFlowTolerance) THEN
      CondenserInletTemp = CondenserOutletTemp - QCond/Par(5)/Par(3)
    ENDIF

    Residuum = (Par(1) - CondenserInletTemp) / Par(1)

  RETURN
END FUNCTION ReformEIRChillerCondInletTempResidual

SUBROUTINE ReportChillerIPLV(ChillerName,ChillerType, IPLVValueSI,IPLVValueIP)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Chandan Sharma
    !       DATE WRITTEN   January 2012
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the IPLV values in SI and IP units to
    ! the "eio" and tabular output files for EIR Chillers.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE OutputReportPredefined
    USE DataGlobals,    ONLY : OutputFileInits
    USE General,        ONLY : RoundSigDigits
    USE DataPlant,      ONLY: TypeOf_Chiller_ElectricEIR, TypeOf_Chiller_ElectricReformEIR

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN)   :: ChillerName  ! Name of Chiller for which IPLV is calculated
    INTEGER, INTENT(IN)            :: ChillerType  ! Type of Chiller - EIR or Reformulated EIR
    REAL(r64), INTENT(IN)          :: IPLVValueSI  ! IPLV value in SI units {W/W}
    REAL(r64), INTENT(IN)          :: IPLVValueIP  ! IPLV value in IP units {Btu/W-h}

    ! SUBROUTINE PARAMETER DEFINITIONS:


    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.

    IF (MyOneTimeFlag) THEN
        WRITE(OutputFileInits, 990)
        MyOneTimeFlag = .FALSE.
    END IF

    SELECT CASE (ChillerType)
      CASE (TypeOf_Chiller_ElectricEIR)

        WRITE (OutputFileInits, 991) 'Chiller:Electric:EIR', TRIM(ChillerName),  &
                                 TRIM(RoundSigDigits(IPLVValueSI,2)),TRIM(RoundSigDigits(IPLVValueIP,2))
        CALL PreDefTableEntry(pdchMechType,TRIM(ChillerName),'Chiller:Electric:EIR')

      CASE (TypeOf_Chiller_ElectricReformEIR)

        WRITE (OutputFileInits, 991) 'Chiller:Electric:ReformulatedEIR', TRIM(ChillerName),  &
                                 TRIM(RoundSigDigits(IPLVValueSI,2)),TRIM(RoundSigDigits(IPLVValueIP,2))
        CALL PreDefTableEntry(pdchMechType,TRIM(ChillerName),'Chiller:Electric:ReformulatedEIR')

    END SELECT

    CALL PreDefTableEntry(pdchMechIPLVSI,TRIM(ChillerName),TRIM(RoundSigDigits(IPLVValueSI,2)))
    CALL PreDefTableEntry(pdchMechIPLVIP,TRIM(ChillerName),TRIM(RoundSigDigits(IPLVValueIP,2)))

    990 FORMAT('! <Chiller Standard Rating Information>, Component Type, Component Name, ',    &
               'IPLV in SI Units {W/W}, ', 'IPLV in IP Units {Btu/W-h}')
    991 FORMAT(' Chiller Standard Rating Information, ',A,', ',A,', ',A,', ',A)

    RETURN

END SUBROUTINE ReportChillerIPLV

SUBROUTINE CheckCurveLimitsForIPLV(ChillerName, ChillerType, CondenserType, CapFTempCurveIndex, EIRFTempCurveIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR            Chandan Sharma, FSEC
          !       DATE WRITTEN      January 2012
          !       MODIFIED          na
          !       RE-ENGINEERED     na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Checks the limits of the various curves used in EIR chiller and returns .FALSE. if the limits do not include
          ! the standard test condition(s).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: DisplayExtraWarnings
  USE CurveManager,   ONLY: GetCurveMinMaxValues, GetCurveType, GetCurveName
  USE DataPlant,      ONLY: TypeOf_Chiller_ElectricEIR, TypeOf_Chiller_ElectricReformEIR

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ChillerName         ! Name of Chiller
  INTEGER, INTENT(IN)          :: ChillerType         ! Type of Chiller - EIR or ReformulatedEIR
  INTEGER, INTENT(IN)          :: CondenserType       ! Type of Condenser - Air Cooled, Water Cooled or Evap Cooled
  INTEGER, INTENT(IN)          :: CapFTempCurveIndex  ! Index for the total cooling capacity modifier curve
                                                      ! (function of leaving chilled water temperature and
                                                      !  entering condenser fluid temperature)
  INTEGER, INTENT(IN)          :: EIRFTempCurveIndex  ! Index for the energy input ratio modifier curve
                                                      ! (function of leaving chilled water temperature and
                                                      !  entering condenser fluid temperature)

          ! SUBROUTINE PARAMETER DEFINITIONS:

  INTEGER, PARAMETER   :: AirCooled     = 1
  INTEGER, PARAMETER   :: WaterCooled   = 2
  INTEGER, PARAMETER   :: EvapCooled    = 3

  ! Following parameters are taken from AHRI 551/591,2011 Table 3
  REAL(r64), PARAMETER :: HighEWTemp    = 30.0d0   ! Entering water temp in degrees C at full load capacity (85F)
  REAL(r64), PARAMETER :: LowEWTemp     = 19.0d0   ! Entering water temp in degrees C at minimum reduced capacity (65F)
  REAL(r64), PARAMETER :: OAHighEDBTemp = 35.0d0   ! Outdoor air dry-bulb temp in degrees C at full load capacity (95F)
  REAL(r64), PARAMETER :: OALowEDBTemp  = 13d0     ! Outdoor air dry-bulb temp in degrees C at minimum reduced capacity (55F)
  REAL(r64), PARAMETER :: OAHighEWBTemp = 24.0d0   ! Outdoor air wet-bulb temp in degrees C at full load capacity (75F)
  REAL(r64), PARAMETER :: OALowEWBTemp  = 13.50d0  ! Outdoor wet dry-bulb temp in degrees C at minimum reduced capacity (56.25F)
  REAL(r64), PARAMETER :: LeavingWaterTemp   = 6.67d0 ! Evaporator leaving water temperature in degrees C [44 F]

  CHARACTER(len=*), PARAMETER :: RoutineName = 'CheckCurveLimitsForIPLV: ' ! Include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

 !  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
  REAL(r64) :: CapacityLWTempMin  = 0.0d0   ! Capacity modifier Min value (leaving water temp), from the Curve:Biquadratic object
  REAL(r64) :: CapacityLWTempMax  = 0.0d0   ! Capacity modifier Max value (leaving water temp), from the Curve:Biquadratic object
  REAL(r64) :: CapacityEnteringCondTempMin = 0.0d0   ! Capacity modifier Min value (entering cond temp),
                                                     ! from the Curve:Biquadratic object
  REAL(r64) :: CapacityEnteringCondTempMax = 0.0d0   ! Capacity modifier Max value (entering cond temp),
                                                     ! from the Curve:Biquadratic object

!  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
  REAL(r64) :: EIRLWTempMin  = 0.0d0   ! EIR modifier Min value (leaving water temp), from the Curve:Biquadratic object
  REAL(r64) :: EIRLWTempMax  = 0.0d0   ! EIR modifier Max value (leaving water temp), from the Curve:Biquadratic object
  REAL(r64) :: EIREnteringCondTempMin = 0.0d0   ! EIR modifier Min value (entering cond temp),
                                                ! from the Curve:Biquadratic object
  REAL(r64) :: EIREnteringCondTempMax = 0.0d0   ! EIR modifier Max value (entering cond temp),
                                                ! from the Curve:Biquadratic object

  REAL(r64) :: HighCondenserEnteringTempLimit = 0.0d0  ! High limit of entering condenser temperature
  REAL(r64) :: LowCondenserEnteringTempLimit  = 0.0d0  ! Low limit of entering condenser temperature

  LOGICAL :: CapCurveIPLVLimitsExceeded = .FALSE.  ! Logical for capacity curve temperature limits being exceeded (IPLV calcs)
  LOGICAL :: EIRCurveIPLVLimitsExceeded = .FALSE.  ! Logical for EIR temperature limits being exceeded (IPLV calcs)

  CALL GetCurveMinMaxValues(CapFTempCurveIndex,CapacityLWTempMin,CapacityLWTempMax, &
                            CapacityEnteringCondTempMin,CapacityEnteringCondTempMax)
  CALL GetCurveMinMaxValues(EIRFTempCurveIndex,EIRLWTempMin,EIRLWTempMax, &
                            EIREnteringCondTempMin,EIREnteringCondTempMax)

  IF (CondenserType == WaterCooled) THEN
    HighCondenserEnteringTempLimit = HighEWTemp
    LowCondenserEnteringTempLimit  = LowEWTemp
  ELSE IF (CondenserType == AirCooled) THEN
    HighCondenserEnteringTempLimit = OAHighEDBTemp
    LowCondenserEnteringTempLimit  = OAHighEDBTemp
  ELSE ! Evaporatively Cooled Condenser
    HighCondenserEnteringTempLimit = OAHighEWBTemp
    LowCondenserEnteringTempLimit  = OAHighEWBTemp
  ENDIF

  ! Checking the limits of capacity modifying curve for temperatures (IPLV high and low test conditions)
  IF ( CapacityEnteringCondTempMax < HighCondenserEnteringTempLimit .OR. &
       CapacityEnteringCondTempMin > LowCondenserEnteringTempLimit .OR.  &
       CapacityLWTempMax < LeavingWaterTemp .OR. CapacityLWTempMin > LeavingWaterTemp ) THEN
       CapCurveIPLVLimitsExceeded = .TRUE.
  END IF
  ! Checking the limits of EIR modifying curve for temperatures (IPLV high and low test conditions)
  IF ( EIREnteringCondTempMax < HighCondenserEnteringTempLimit .OR. &
       EIREnteringCondTempMin > LowCondenserEnteringTempLimit .OR.            &
       EIRLWTempMax < LeavingWaterTemp .OR. EIRLWTempMin > LeavingWaterTemp ) THEN
       EIRCurveIPLVLimitsExceeded = .TRUE.
  END IF

  ! For IPLV:
  IF ( CapCurveIPLVLimitsExceeded .OR. EIRCurveIPLVLimitsExceeded) THEN
      IF (DisplayExtraWarnings) THEN
        SELECT CASE (ChillerType)

        CASE (TypeOf_Chiller_ElectricEIR)

          CALL ShowWarningError('Chiller:Electric:EIR = '// &
                                TRIM(ChillerName)//': '//&
                                ' Integrated Part Load Value (IPLV) calculated is not at the AHRI test condition.')
        CASE (TypeOf_Chiller_ElectricReformEIR)

          CALL ShowWarningError('Chiller:Electric:ReformulatedEIR = '// &
                                TRIM(ChillerName)//': '//&
                                ' Integrated Part Load Value (IPLV) calculated is not at the AHRI test condition.')
        END SELECT
        IF (CapCurveIPLVLimitsExceeded) THEN
            CALL ShowContinueError(' Check limits in Cooling Capacity Function of Temperature Curve, '  &
            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))      &
            //', Curve Name = '//TRIM(GetCurveName(CapFTempCurveIndex)))
        END IF
        IF (EIRCurveIPLVLimitsExceeded) THEN
            CALL ShowContinueError(' Check limits in EIR Function of Temperature Curve, ' &
                //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex)) &
                //', Curve Name = '//TRIM(GetCurveName(EIRFTempCurveIndex)))
        END IF
      END IF
  END IF

  RETURN

END SUBROUTINE CheckCurveLimitsForIPLV

SUBROUTINE CalcDXCoilStandardRating(DXCoilName, DXCoilType, DXCoilType_Num, ns, RatedTotalCapacity, RatedCOP, CapFFlowCurveIndex, &
                                    CapFTempCurveIndex, EIRFFlowCurveIndex, EIRFTempCurveIndex, PLFFPLRCurveIndex, &
                                    RatedAirVolFlowRate, FanPowerPerEvapAirFlowRateFromInput, RegionNum, MinOATCompressor, &
                                    OATempCompressorOn, OATempCompressorOnOffBlank, DefrostControl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, Chandan Sharma FSEC
          !       DATE WRITTEN   February 2010,
          !                      B. Nigusse, May 2010  Added EER and IEER Calculation
          !                      C. Sharma, March 2012  Added HSPF Calculation for single speed HP
          !                      B. Nigusse, August 2012 Added SEER Calculation for Multi-speed HP
          !                      B. Nigusse, November 2012 Added HSPF Calculation for Multi-speed HP

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !     Calculates:
          !                 (1) Standard Rated (net) Cooling Capacity
          !                 (2) Seasonal Energy Efficiency Ratio (SEER)
          !                 (3) Energy Efficiency Ratio (EER),
          !                 (4) Integrated Energy Efficiency Ratio (IEER)
          !                 for Air-to-Air Direct Expansion Air Conditioner and Heat Pumps having a single-speed compressor,
          !                 fixed speed indoor supply air fan, and air-cooled condensers. Writes the result to EIO file.
          !                 (5) Heating Seasonal Performance Factor (HSPF) for Air-Source Direct Expansion Heat Pumps having
          !                  a single-speed compressor, fixed speed indoor supply air fan
          !                 (6) Standard Rated (net) Cooling Capacity; and
          !                 (7) Seasonal Energy Efficiency Ratio (SEER) for Air-to-Air Heat Pumps having multi-speed
          !                     compressor.
          !                 (8) Heating Seasonal Performance Factor (HSPF) for Air-to-Air Heat Pumps having multi-speed
          !                     compressor.
          !
          ! METHODOLOGY EMPLOYED:
          ! (A) Methodology for calculating standard ratings for DX air conditioners
          !     (1) Obtains the rated condition parameters:
          !         Cooling capacity (User specified or Autosized Value)
          !         Rated Air volume flow rate through the DX Cooling Coil (User specified or autosized value)
          !
          !     (2) Evaluates the total cooling coil capacity at AHRI test conditions 26.7C/19.4C/27.8C. Then net
          !         cooling capacity is determined from the total cooling capacity of the DX coil at the AHRI test
          !         conditions and accounting for the INDOOR supply air fan heat.
          !
          !     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
          !         Evaluates the EIR capacity and flow fraction modifiers at 26.7C/19.4C/27.8C. The net electric
          !         power consumption is determined by adding the indoor fan electric power to the electric power
          !         consumption by the DX Coil Condenser Fan and Compressor at the AHRI test conditions.
          !
          !     (4) The EER is evaluated from the total net cooling capacity and total electric power
          !         evaluated at the standard rated test conditions.  The IEER is a weighted value of the EER evaluated
          !         at four different capacities of 100%, 75%, 50% and 25%.  The reduced capacity EERs are evaluated
          !         at different outdoor coil entering air dry-bulb temperatures.
          !
          ! (B) Methodology for calculating standard ratings for DX air air source heat pumps
          !     (1) Obtains the rated condition parameters:
          !         heating capacity (User specified or Autosized Value), COP,  Rated Air volume flow rate through the
          !         DX Cooling Coil (User specified or autosized value) and Fan power per rated air flow rate
          !
          !     (2) Evaluates the heating coil capacities for AHRI tests H1, H2 and H3 using the performance cuves and
          !         input values specified at (1) above. Then net heating capacity is determined from the total heating capacity
          !         of the DX coil at the AHRI test conditions and accounting for the INDOOR supply air fan heat.
          !
          !     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
          !         The net electric power consumption is determined by adding the indoor fan electric power to the
          !         electric power consumption by the DX Coil Condenser Fan and Compressor at the AHRI test conditions.
          !
          !     (4) High Temperature Heating Standard (Net) Rating Capacity and Low Temperature Heating Standard (Net)
          !         Rating Capacity capacity are determined using tests H1 adn H3 per ANSI/AHRI 210/240 2008.
          !
          !     (5) The HSPF is evaluated from the total net heating capacity and total electric power
          !         evaluated at the standard rated test conditions. For user specified region number, the outdoor temperatures
          !         are Binned (grouped) and fractioanl bin hours for each bin over the entire heating season are taken
          !         from AHRI 210/240. Then for each bin, building load, heat pump energy adn resistance space heating enegry are
          !         calculated. The sum of building load divided by sum of heat pump and resistance space heating over the
          !         entire heating season gives the HSPF. The detailed calculation algorithms of calculating HSPF
          !         are described in Engineering Reference.
          !
          ! (C) Methodology for calculating standard ratings for Multi-Speed Heat Pumps
          !     Net Total Cooling Capacity and SEER
          !     (1) Obtains the rated condition parameters:
          !         Cooling capacity (User specified or Autosized Value)
          !         Rated Air volume flow rate through the DX Cooling Coil (User specified or autosized value)
          !
          !     (2) Evaluates the total cooling coil capacity at AHRI A2 test conditions 26.7C/19.4C/35.0C. Then net
          !         cooling capacity is determined from the total cooling capacity of the DX coil at the AHRI A2 test
          !         conditions and accounting for the INDOOR supply air fan effect.  The net total cooling capacity
          !         is reported at the high (maximum) speed only.
          !
          !     (3) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
          !         Evaluates the EIR capacity and flow fraction modifiers at A2, B2, B1, and F1 test coditions per
          !         AHRI/ANSI Std. 210/240 test procedure for multi-speed compressor.  For any inter-
          !         mediate operating conditions (speed), the successive lower and the higher speed performnace are
          !         weighed per the standard.  Electric Power consumption is determined by adding the indoor fan
          !         electric power to the electric power consumption by the outdoor DX Coil Fan and Compressor Power
          !         at the AHRI test conditions.  The net total cooling capacity is also corrected for the fan heat
          !         effect for SEER calculation.
          !
          !     Net Heatingg Capacity and HSPF
          !     (4) Obtains the rated condition parameters:
          !         Heating capacity (User specified or Autosized Value)
          !         Rated Air volume flow rate through the DX Heating Coil (User specified or autosized value)
          !
          !     (5) Evaluates the heating coil capacity at AHRI H12 test conditions 21.1C/15.6C/8.33C. Then net
          !         heating capacity is determined from the total heating capacity of the DX coil at the AHRI H12
          !         test conditions and accounting for the supply supply air fan effect.  The net heating capacity
          !         is reported at the high (maximum) speed only.
          !
          !     (6) Calculates the electric power consumed by the DX Coil Unit (compressor + outdoor condenser fan).
          !         Evaluates the EIR capacity and flow fraction modifiers per AHRI/ANSI Std. 210/240 test procedures
          !         for two speed compressor (H01, H11, H21, H31, H12, H22, and H32 ). This procedure was modified
          !         for multispeed heat pumps. For any inter-mediate operating conditions (speed), the successive
          !         lower and the higher speed performnace are weighed per the standard.
          !         Electric Power consumption is determined by adding the supply fan electric power to the electric
          !         power consumption by the outdoor DX Coil Fan and Compressor Power at the AHRI test conditions.
          !         The net heating capacity is also corrected for the fan heat effect for SEER calculation.
          !
          ! REFERENCES:
          ! (1) ANSI/AHRI Standard 210/240-2008:  Standard for Performance Rating of Unitary Air-Conditioning and
          !                                       Air-Source Heat Pumps. Arlington, VA:  Air-Conditioning, Heating
          !                                       , and Refrigeration Institute.
          !
          ! (2) ANSI/AHRI Standard 340/360-2007:  Standard for Performance Rating of Commercial and Industrial
          !                                       Unitary Air-Conditioning and Heat Pump Equipment.  Arlington,
          !                                       VA:  Air-Conditioning, Heating, and Refrigeration Institute.

          ! USE STATEMENTS:

  USE DataHVACGlobals, ONLY: CoilDX_CoolingSingleSpeed, CoilDX_HeatingEmpirical,CoilDX_MultiSpeedCooling,CoilDX_MultiSpeedHeating
  USE CurveManager,    ONLY: CurveValue, GetCurveMinMaxValues, GetCurveType
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: DXCoilName              ! Name of DX coil for which HSPF is calculated
  CHARACTER(len=*), INTENT(IN)     :: DXCoilType              ! Type of DX coil for which HSPF is calculated
  INTEGER,   INTENT(IN)            :: DXCoilType_Num          ! Integer Type of DX coil - heating or cooling
  INTEGER,   INTENT(IN)            :: ns                      ! Number of compressor speeds
  INTEGER,   INTENT(IN)            :: CapFTempCurveIndex(ns)  ! Index for the capacity as a function of temperature modifier curve
  INTEGER,   INTENT(IN)            :: CapFFlowCurveIndex(ns)  ! Index for the capacity as a function of flow fraction modifier curve
  INTEGER,   INTENT(IN)            :: EIRFTempCurveIndex(ns)  ! Index for the EIR as a function of temperature modifier curve
  INTEGER,   INTENT(IN)            :: EIRFFlowCurveIndex(ns)  ! Index for the EIR as a function of flow fraction modifier curve
  INTEGER,   INTENT(IN)            :: PLFFPLRCurveIndex(ns)   ! Index for the PLF vs part-load ratio curve
  REAL(r64), INTENT(IN)            :: RatedTotalCapacity(ns)  ! Reference capacity of DX coil [W]
  REAL(r64), INTENT(IN)            :: RatedCOP(ns)            ! Reference coefficient of performance [W/W]
  REAL(r64), INTENT(IN)            :: RatedAirVolFlowRate(ns) ! Reference air flow rate of DX coil [m3/s]
  REAL(r64), INTENT(IN)            :: FanPowerPerEvapAirFlowRateFromInput(ns)  ! Reference fan power per evap air flow rate [W/(m3/s)]

  INTEGER, OPTIONAL, INTENT(IN)    :: RegionNum            ! Region number for calculating HSPF of single speed DX heating coil !Objexx:OPTIONAL Used without PRESENT check
  INTEGER, OPTIONAL, INTENT(IN)    :: DefrostControl       ! defrost control; 1=timed, 2=on-demand !Objexx:OPTIONAL Used without PRESENT check
  REAL(r64), OPTIONAL, INTENT(IN)  :: MinOATCompressor     ! Minimum OAT for heat pump compressor operation [C] !Objexx:OPTIONAL Used without PRESENT check
  REAL(r64), OPTIONAL, INTENT(IN)  :: OATempCompressorOn   ! The outdoor temperature when the compressor is automatically turned !Objexx:OPTIONAL Used without PRESENT check
                                                           ! back on, if applicable, following automatic shut off. This field is
                                                           ! used only for HSPF calculation. [C]
  LOGICAL, OPTIONAL, INTENT(IN)    :: OATempCompressorOnOffBlank  ! Flag used to determine low temperature cut out factor !Objexx:OPTIONAL Used without PRESENT check

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: FanPowerPerEvapAirFlowRate(ns) ! Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

! Intermediate values calculated from the inputs in the idf file
  REAL(r64) :: TotCapTempModFacH0 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H0 Test [-]
  REAL(r64) :: TotCapTempModFacH1 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H1 Test [-]
  REAL(r64) :: TotCapTempModFacH2 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H2 Test [-]
  REAL(r64) :: TotCapTempModFacH3 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H3 Test [-]
  REAL(r64) :: EIRTempModFacH1 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H1 Test[-]
  REAL(r64) :: EIRTempModFacH0 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H0 Test[-]
  REAL(r64) :: EIRTempModFacH2 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H2 Test[-]
  REAL(r64) :: EIRTempModFacH3 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H3 Test[-]
  REAL(r64) :: PartLoadFactor              = 0.0D0 ! Part load factor
  REAL(r64) :: PartLoadRatio               = 0.0d0 ! compressor cycling ratio between successive speeds, [-]
  REAL(r64) :: PartLoadFraction            = 0.0d0 ! part-load fraction that account for the cyclic degradation, [-]
  REAL(r64) :: NetHeatingCapWeighted       = 0.0d0 ! net total heating cap weighted by the fraction of the binned cooling hours [W]
  REAL(r64) :: TotHeatingElecPowerWeighted = 0.0d0 ! net total heat pump and resistance heating electric Energy input weighted by
                                                   ! the fraction of the binned cooling hours
  REAL(r64) :: BuildingHeatingLoad         = 0.0d0 ! Building space heating load corresponding to an outdoor bin temperature [W]
  REAL(r64) :: NetTotHeatCapBinned         = 0.0d0 ! Net tot heatinging cap corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotHeatElecPowerBinnedHP    = 0.0d0 ! Total Heat Pump heating electric power consumption at outdoor bin temp [W]
  REAL(r64) :: TotHeatElecPowerBinnedRH    = 0.0d0 ! Total Resistance heating electric power consumption at outdoor bin temp [W]
  !
  INTEGER   :: BinNum                              ! bin number counter
  INTEGER   :: spnum                               ! compressor speed number
  INTEGER   :: StandardDHRNum                      ! Integer counter for standardized DHRs

! Calculated and reported to the EIO file
  REAL(r64) :: SEER = 0.0d0                        ! Seasonal Energy Efficiency Ratio in SI [W/W]
  REAL(r64) :: EER  = 0.0d0                        ! Energy Efficiency Ratio in SI [W/W]
  REAL(r64) :: IEER = 0.0d0                        ! Integerated Energy Efficiency Ratio in SI [W/W]
  REAL(r64) :: HSPF = 0.0d0                        ! Heating Seasonal Performance Factor in SI [W/W]
  REAL(r64) :: NetHeatingCapRatedHighTemp = 0.0d0  ! Net Rated heating capacity at high temp [W]
  REAL(r64) :: NetHeatingCapRatedLowTemp  = 0.0d0  ! Net Rated heating capacity at low temp [W]
  REAL(r64) :: NetCoolingCapRated(ns)              ! Net Cooling Coil capacity at Rated conditions, accounting for supply fan heat [W]

  NetCoolingCapRated = 0.0d0

SELECT CASE(DXCoilType_Num)

 CASE (CoilDX_CoolingSingleSpeed) ! Coil:Cooling:DX:SingleSpeed

    CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex(1),  &
                                            CapFFlowCurveIndex(1), EIRFTempCurveIndex(1), EIRFFlowCurveIndex(1), &
                                            PLFFPLRCurveIndex(1))

    ! Calculated Net Cooling Capacity, SEER, EER, and IEER of single speed DX cooling coils
    CALL SingelSpeedDXCoolingCoilStandardRatings(DXCoilName, DXCoilType, CapFTempCurveIndex(1), CapFFlowCurveIndex(1), &
                                                EIRFTempCurveIndex(1), EIRFFlowCurveIndex(1), PLFFPLRCurveIndex(1), &
                                                RatedTotalCapacity(1), RatedCOP(1), RatedAirVolFlowRate(1), &
                                                FanPowerPerEvapAirFlowRateFromInput(1), NetCoolingCapRated(1), SEER, EER, IEER)

    ! Writes the net rated cooling capacity, SEER, EER and IEER values to the EIO file and standard tabular output tables
    CALL ReportDXCoilRating(DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(1),      &
                            SEER * ConvFromSIToIP,EER,EER * ConvFromSIToIP,IEER * ConvFromSIToIP, &
                            NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF * ConvFromSIToIP, RegionNum)

 CASE (CoilDX_HeatingEmpirical)  ! Coil:Heating:DX:SingleSpeed

    CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex(1),  &
                                            CapFFlowCurveIndex(1),EIRFTempCurveIndex(1), EIRFFlowCurveIndex(1), &
                                            PLFFPLRCurveIndex(1))
    ! Calculate the standard ratings for single speed DX heating coil
    CALL SingleSpeedDXHeatingCoilStandardRatings(RatedTotalCapacity(1), RatedCOP(1), CapFFlowCurveIndex(1), &
                                                 CapFTempCurveIndex(1), EIRFFlowCurveIndex(1), EIRFTempCurveIndex(1), &
                                                 RatedAirVolFlowRate(1), FanPowerPerEvapAirFlowRateFromInput(1), &
                                                 RegionNum, MinOATCompressor, OATempCompressorOn, OATempCompressorOnOffBlank, &
                                                 DefrostControl, NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF)

    ! Writes the HSPF value to the EIO file and standard tabular output tables
    CALL ReportDXCoilRating(DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(1),      &
                            SEER * ConvFromSIToIP,EER,EER * ConvFromSIToIP,IEER * ConvFromSIToIP, &
                            NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF * ConvFromSIToIP, RegionNum)


 CASE (CoilDX_MultiSpeedCooling)  ! Coil:Cooling:DX:MultiSpeed,

    DO spnum = 1, ns
      CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, &
                                              CapFTempCurveIndex(spnum), CapFFlowCurveIndex(spnum), &
                                              EIRFTempCurveIndex(spnum), EIRFFlowCurveIndex(spnum), &
                                              PLFFPLRCurveIndex(spnum))
    END DO
    ! Calculate the standard ratings for multispeed DX cooling coil
    CALL MultiSpeedDXCoolingCoilStandardRatings(DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                                EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, &
                                                RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, &
                                                FanPowerPerEvapAirFlowRateFromInput, ns, &
                                                NetCoolingCapRated(ns), SEER)
    ! Writes the SEER value to the EIO file and standard tabular output tables
    CALL ReportDXCoilRating(DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(ns),      &
                            SEER * ConvFromSIToIP, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0)


CASE (CoilDX_MultiSpeedHeating)  ! Coil:Heating:DX:MultiSpeed

    DO spnum = 1, ns
        CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, &
                                                CapFTempCurveIndex(spnum), CapFFlowCurveIndex(spnum), &
                                                EIRFTempCurveIndex(spnum), EIRFFlowCurveIndex(spnum), &
                                                PLFFPLRCurveIndex(spnum))
    END DO
    ! Calculate Net heatig capacity and HSPF of multispeed DX heating coils
    CALL MultiSpeedDXHeatingCoilStandardRatings(DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                                EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, &
                                                RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, &
                                                FanPowerPerEvapAirFlowRateFromInput, ns, RegionNum, MinOATCompressor, &
                                                OATempCompressorOn, OATempCompressorOnOffBlank, DefrostControl, &
                                                NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF)
    ! Writes the HSPF value to the EIO file and standard tabular output tables
    CALL ReportDXCoilRating(DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(ns),      &
                            SEER * ConvFromSIToIP,EER,EER * ConvFromSIToIP,IEER * ConvFromSIToIP, &
                            NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF * ConvFromSIToIP, RegionNum)

 CASE DEFAULT
    !... other DX Coil types will follow here


END SELECT

RETURN

END SUBROUTINE CalcDXCoilStandardRating

SUBROUTINE SingleSpeedDXHeatingCoilStandardRatings(RatedTotalCapacity, RatedCOP, CapFFlowCurveIndex, CapFTempCurveIndex, &
                                                   EIRFFlowCurveIndex, EIRFTempCurveIndex, RatedAirVolFlowRate, &
                                                   FanPowerPerEvapAirFlowRateFromInput, RegionNum, MinOATCompressor, &
                                                   OATempCompressorOn, OATempCompressorOnOffBlank, DefrostControl, &
                                                   NetHeatingCapRated, NetHeatingCapH3Test, HSPF)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Chandan Sharma
    !       DATE WRITTEN   February 2012
    !       MODIFIED       B Nigusse, December 2012
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! na

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue, GetCurveMinMaxValues, GetCurveType

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: CapFTempCurveIndex  ! Index for the capacity as a function of temperature modifier curve
  INTEGER, INTENT(IN)    :: CapFFlowCurveIndex  ! Index for the capacity as a function of flow fraction modifier curve
  INTEGER, INTENT(IN)    :: EIRFTempCurveIndex  ! Index for the EIR as a function of temperature modifier curve
  INTEGER, INTENT(IN)    :: EIRFFlowCurveIndex  ! Index for the EIR as a function of flow fraction modifier curve
  REAL(r64), INTENT(IN)  :: RatedTotalCapacity  ! Reference capacity of DX coil [W]
  REAL(r64), INTENT(IN)  :: RatedCOP            ! Reference coefficient of performance [W/W]
  REAL(r64), INTENT(IN)  :: RatedAirVolFlowRate ! Rated air volume flow rate [m3/s]
  REAL(r64), INTENT(IN)  :: FanPowerPerEvapAirFlowRateFromInput  ! Fan power per air volume flow rate [W/(m3/s)]


  INTEGER, OPTIONAL, INTENT(IN)    :: RegionNum            ! Region number for calculating HSPF of single speed DX heating coil
  INTEGER, OPTIONAL, INTENT(IN)    :: DefrostControl       ! defrost control; 1=timed, 2=on-demand
  REAL(r64), OPTIONAL, INTENT(IN)  :: MinOATCompressor     ! Minimum OAT for heat pump compressor operation [C]
  REAL(r64), OPTIONAL, INTENT(IN)  :: OATempCompressorOn   ! The outdoor tempearture when the compressor is automatically turned
                                                           ! back on, if applicable, following automatic shut off. This field is
                                                           ! used only for HSPF calculation. [C]
  LOGICAL, OPTIONAL, INTENT(IN)    :: OATempCompressorOnOffBlank ! Flag used to determine low temperature cut out factor
  REAL(r64), INTENT(OUT)           :: NetHeatingCapRated   ! Net Heating Coil capacity at Rated conditions,
                                                           ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT)           :: NetHeatingCapH3Test  ! Net Heating Coil capacity at H3 test conditions
                                                           ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT)           :: HSPF                 ! seasonale energy efficiency ratio of multi speed DX cooling coil


    ! SUBROUTINE PARAMETER DEFINITIONS:
    ! na
    !
    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: TotalHeatingCapRated = 0.0d0   ! Heating Coil capacity at Rated conditions, without accounting supply fan heat [W]
  REAL(r64) :: EIRRated = 0.0D0               ! EIR at Rated conditions [-]
  REAL(r64) :: TotCapTempModFacRated = 0.0D0  ! Total capacity as a function of temerature modifier at rated conditions [-]
  REAL(r64) :: EIRTempModFacRated = 0.0D0     ! EIR as a function of temerature modifier at rated conditions [-]
  REAL(r64) :: TotalHeatingCapH2Test = 0.0d0  ! Heating Coil capacity at H2 test conditions, without accounting supply fan heat [W]
  REAL(r64) :: TotalHeatingCapH3Test = 0.0d0  ! Heating Coil capacity at H3 test conditions, without accounting supply fan heat [W]
  REAL(r64) :: CapTempModFacH2Test = 0.0D0    ! Total capacity as a function of temerature modifier at H2 test conditions [-]
  REAL(r64) :: EIRTempModFacH2Test = 0.0d0    ! EIR as a function of temerature modifier at H2 test conditions [-]
  REAL(r64) :: EIRH2Test = 0.0d0              ! EIR at H2 test conditions [-]
  REAL(r64) :: CapTempModFacH3Test = 0.0d0    ! Total capacity as a function of temerature modifier at H3 test conditions [-]
  REAL(r64) :: EIRTempModFacH3Test = 0.0d0    ! EIR as a function of temerature modifier at H3 test conditions [-]
  REAL(r64) :: EIRH3Test = 0.0d0              ! EIR at H3 test conditions [-]
  REAL(r64) :: TotCapFlowModFac = 0.0d0       ! Total capacity modifier (function of actual supply air flow vs rated flow)
  REAL(r64) :: EIRFlowModFac = 0.0d0          ! EIR modifier (function of actual supply air flow vs rated flow)
  REAL(r64) :: FanPowerPerEvapAirFlowRate = 0.0d0 ! Fan power per air volume flow rate [W/(m3/s)]

  REAL(r64) :: ElecPowerRated                 ! Total system power at Rated conditions accounting for supply fan heat [W]
  REAL(r64) :: ElecPowerH2Test                ! Total system power at H2 test conditions accounting for supply fan heat [W]
  REAL(r64) :: ElecPowerH3Test                ! Total system power at H3 test conditions accounting for supply fan heat [W]
  REAL(r64) :: NetHeatingCapH2Test            ! Net Heating Coil capacity at H2 test conditions accounting for supply fan heat [W]

  REAL(r64) :: PartLoadFactor
  REAL(r64) :: LoadFactor                     ! Frac. "on" time for last stage at the desired reduced capacity, (dimensionless)
  REAL(r64) :: LowTempCutOutFactor = 0.0d0    ! Factor which corresponds to compressor operation depending on outdoor temperature
  REAL(r64) :: OATempCompressorOff = 0.0D0    ! Minimum outdoor air temperature to turn the commpressor off, [C]

  REAL(r64) :: FractionalBinHours     = 0.0d0 ! Fractional bin hours for the heating season  [-]
  REAL(r64) :: BuildingLoad           = 0.0d0 ! Building space conditioning load corresponding to an outdoor bin temperature [W]
  REAL(r64) :: HeatingModeLoadFactor  = 0.0d0 ! Heating mode load factor corresponding to an outdoor bin temperature  [-]
  REAL(r64) :: NetHeatingCapReduced   = 0.0d0 ! Net Heating Coil capacity corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotalBuildingLoad      = 0.0d0 ! Sum of building load over the entire heating season [W]
  REAL(r64) :: TotalElectricalEnergy  = 0.0d0 ! Sum of electrical energy consumed by the heatpump over the heating season [W]
  REAL(r64) :: DemandDeforstCredit    = 1.0d0 ! A factor to adjust HSPF if coil has demand defrost control  [-]
  REAL(r64) :: CheckCOP               = 0.0d0 ! Checking COP at an outdoor bin temperature against unity [-]
  REAL(r64) :: DesignHeatingRequirement      = 0.0d0 ! The amount of heating required to maintain a given indoor temperature
                                                     ! at a particular outdoor design temperature.  [W]
  REAL(r64) :: DesignHeatingRequirementMin   = 0.0d0 ! minimum design heating requirement [W]
  REAL(r64) :: DesignHeatingRequirementMax   = 0.0d0 ! maximum design heating requirement [W]
  REAL(r64) :: ElectricalPowerConsumption    = 0.0d0 ! Electrical power corresponding to an outdoor bin temperature [W]
  REAL(r64) :: HeatPumpElectricalEnergy      = 0.0d0 ! Heatpump electrical energy corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotalHeatPumpElectricalEnergy = 0.0d0 ! Sum of Heatpump electrical energy over the entire heating season [W]
  REAL(r64) :: ResistiveSpaceHeatingElectricalEnergy = 0.0d0      ! resistance heating electrical energy corresponding to an
                                                                  ! outdoor bin temperature [W]
  REAL(r64) :: TotalResistiveSpaceHeatingElectricalEnergy = 0.0d0 ! Sum of resistance heating electrical energy over the
                                                                  ! entire heating season [W]

  INTEGER   :: BinNum                         ! bin number counter
  INTEGER   :: spnum                          ! compressor speed number
  INTEGER   :: StandardDHRNum                 ! Integer counter for standardized DHRs

  
  TotalBuildingLoad = 0.0d0
  TotalHeatPumpElectricalEnergy = 0.0d0
  TotalResistiveSpaceHeatingElectricalEnergy = 0.0d0
  
  ! Calculate the supply air fan electric power consumption.  The electric power consumption is estimated
  ! using either user supplied or AHRI default value for fan power per air volume flow rate
  IF( FanPowerPerEvapAirFlowRateFromInput <= 0.0D0) THEN
      FanPowerPerEvapAirFlowRate=DefaultFanPowerPerEvapAirFlowRate
  ELSE
      FanPowerPerEvapAirFlowRate=FanPowerPerEvapAirFlowRateFromInput
  ENDIF

  TotCapFlowModFac = CurveValue(CapFFlowCurveIndex,AirMassFlowRatioRated)
  EIRFlowModFac = CurveValue(EIRFFlowCurveIndex,AirMassFlowRatioRated)

  SELECT CASE(GetCurveType(CapFTempCurveIndex))

    CASE('QUADRATIC', 'CUBIC')
        TotCapTempModFacRated = CurveValue(CapFTempCurveIndex,HeatingOutdoorCoilInletAirDBTempRated)

        CapTempModFacH2Test = CurveValue(CapFTempCurveIndex,HeatingOutdoorCoilInletAirDBTempH2Test)

        CapTempModFacH3Test = CurveValue(CapFTempCurveIndex,HeatingOutdoorCoilInletAirDBTempH3Test)
    CASE('BIQUADRATIC')
        TotCapTempModFacRated = CurveValue(CapFTempCurveIndex,HeatingIndoorCoilInletAirDBTempRated, &
                                           HeatingOutdoorCoilInletAirDBTempRated)

        CapTempModFacH2Test = CurveValue(CapFTempCurveIndex,HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH2Test)

        CapTempModFacH3Test = CurveValue(CapFTempCurveIndex,HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH3Test)

  END SELECT

  SELECT CASE(GetCurveType(EIRFTempCurveIndex))

    CASE('QUADRATIC', 'CUBIC')
        EIRTempModFacRated = CurveValue(EIRFTempCurveIndex,HeatingOutdoorCoilInletAirDBTempRated)

        EIRTempModFacH2Test = CurveValue(EIRFTempCurveIndex,HeatingOutdoorCoilInletAirDBTempH2Test)

        EIRTempModFacH3Test = CurveValue(EIRFTempCurveIndex,HeatingOutdoorCoilInletAirDBTempH3Test)
    CASE('BIQUADRATIC')
        EIRTempModFacRated = CurveValue(EIRFTempCurveIndex,HeatingIndoorCoilInletAirDBTempRated, &
                                        HeatingOutdoorCoilInletAirDBTempRated)

        EIRTempModFacH2Test = CurveValue(EIRFTempCurveIndex,HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH2Test)

        EIRTempModFacH3Test = CurveValue(EIRFTempCurveIndex,HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH3Test)
  END SELECT

  TotalHeatingCapRated = RatedTotalCapacity * TotCapTempModFacRated * TotCapFlowModFac
  NetHeatingCapRated = TotalHeatingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate

  TotalHeatingCapH2Test = RatedTotalCapacity * CapTempModFacH2Test * TotCapFlowModFac
  NetHeatingCapH2Test = TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate

  TotalHeatingCapH3Test = RatedTotalCapacity * CapTempModFacH3Test * TotCapFlowModFac
  NetHeatingCapH3Test = TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate

  IF ( RatedCOP > 0.0D0 ) THEN ! RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"

    EIRRated = EIRTempModFacRated * EIRFlowModFac / RatedCOP
    EIRH2Test = EIRTempModFacH2Test * EIRFlowModFac / RatedCOP
    EIRH3Test = EIRTempModFacH3Test * EIRFlowModFac / RatedCOP

  ENDIF

  ElecPowerRated = EIRRated * TotalHeatingCapRated + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
  ElecPowerH2Test = EIRH2Test * TotalHeatingCapH2Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
  ElecPowerH3Test = EIRH3Test * TotalHeatingCapH3Test + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate

  IF (RegionNum .EQ. 5) THEN
        DesignHeatingRequirementMin = NetHeatingCapRated
  ELSE
        DesignHeatingRequirementMin = NetHeatingCapRated * 1.8D0* (18.33D0 - OutdoorDesignTemperature(RegionNum)) / (60.0D0)
  ENDIF

  DO StandardDHRNum = 1, TotalNumOfStandardDHRs - 1
        IF (StandardDesignHeatingRequirement(StandardDHRNum) .LE. DesignHeatingRequirementMin .AND. &
            StandardDesignHeatingRequirement(StandardDHRNum + 1) .GE. DesignHeatingRequirementMin) THEN
        IF ((DesignHeatingRequirementMin - StandardDesignHeatingRequirement(StandardDHRNum)) .GT. &
            (StandardDesignHeatingRequirement(StandardDHRNum + 1) - DesignHeatingRequirementMin)) THEN
            DesignHeatingRequirementMin = StandardDesignHeatingRequirement(StandardDHRNum + 1)
        ELSE
            DesignHeatingRequirementMin = StandardDesignHeatingRequirement(StandardDHRNum)
        ENDIF
        ENDIF
    END DO
    IF (StandardDesignHeatingRequirement(1) .GE. DesignHeatingRequirementMin) THEN
        DesignHeatingRequirement = StandardDesignHeatingRequirement(1)
    ELSEIF (StandardDesignHeatingRequirement(TotalNumOfStandardDHRs) .LE. DesignHeatingRequirementMin) THEN
        DesignHeatingRequirement = StandardDesignHeatingRequirement(TotalNumOfStandardDHRs)
    ELSE
        DesignHeatingRequirement = DesignHeatingRequirementMin
    ENDIF

    DO BinNum = 1, TotalNumOfTemperatureBins(RegionNum)

        IF (RegionNum .EQ. 1) THEN
        FractionalBinHours = RegionOneFracBinHoursAtOutdoorBinTemp(BinNum)
        ELSEIF (RegionNum .EQ. 2) THEN
        FractionalBinHours = RegionTwoFracBinHoursAtOutdoorBinTemp(BinNum)
        ELSEIF (RegionNum .EQ. 3) THEN
        FractionalBinHours = RegionThreeFracBinHoursAtOutdoorBinTemp(BinNum)
        ELSEIF (RegionNum .EQ. 4) THEN
        FractionalBinHours = RegionFourFracBinHoursAtOutdoorBinTemp(BinNum)
        ELSEIF (RegionNum .EQ. 5) THEN
        FractionalBinHours = RegionFiveFracBinHoursAtOutdoorBinTemp(BinNum)
        ELSEIF (RegionNum .EQ. 6) THEN
        FractionalBinHours = RegionSixFracBinHoursAtOutdoorBinTemp(BinNum)
        ENDIF

        BuildingLoad = (18.33D0 - OutdoorBinTemperature(BinNum)) / (18.33D0 - OutdoorDesignTemperature(RegionNum)) &
                                * CorrectionFactor * DesignHeatingRequirement

        IF ((OutdoorBinTemperature(BinNum) .LE. -8.33D0) .OR. (OutdoorBinTemperature(BinNum) .GE. 7.22D0)) THEN
        NetHeatingCapReduced = NetHeatingCapH3Test + (NetHeatingCapRated - NetHeatingCapH3Test) * &
                                                        (OutdoorBinTemperature(BinNum) + 8.33D0)/ (16.67D0)
        ElectricalPowerConsumption = ElecPowerH3Test + (ElecPowerRated - ElecPowerH3Test) * &
                                                        (OutdoorBinTemperature(BinNum) + 8.33D0)/ (16.67D0)
        ELSE
        NetHeatingCapReduced = NetHeatingCapH3Test + (NetHeatingCapH2Test - NetHeatingCapH3Test) * &
                                                        (OutdoorBinTemperature(BinNum) + 8.33D0)/ (10.0D0)
        ElectricalPowerConsumption = ElecPowerH3Test + (ElecPowerH2Test - ElecPowerH3Test) * &
                                                        (OutdoorBinTemperature(BinNum) + 8.33D0)/ (10.0D0)
        ENDIF

        IF (NetHeatingCapReduced .NE. 0.0D0) THEN
        HeatingModeLoadFactor = BuildingLoad / NetHeatingCapReduced
        ENDIF

        IF (HeatingModeLoadFactor .GT. 1.0D0) THEN
        HeatingModeLoadFactor = 1.0D0
        ENDIF

        PartLoadFactor = 1 - CyclicDegradationCoeff * (1 - HeatingModeLoadFactor)

        IF  (ElectricalPowerConsumption .NE. 0.0D0) THEN
        CheckCOP = NetHeatingCapReduced/ElectricalPowerConsumption
        ENDIF

        OATempCompressorOff = MinOATCompressor

        IF (CheckCOP .LT. 1.0D0) THEN
        LowTempCutOutFactor = 0.0D0
        ELSE
        IF(.NOT. OATempCompressorOnOffBlank) THEN
            IF (OutdoorBinTemperature(BinNum) .LE. OATempCompressorOff) THEN
            LowTempCutOutFactor = 0.0D0
            ELSEIF (OutdoorBinTemperature(BinNum) .GT. OATempCompressorOff .and. &
                    OutdoorBinTemperature(BinNum) .LE. OATempCompressorOn) THEN
            LowTempCutOutFactor = 0.5D0
            ELSE
            LowTempCutOutFactor = 1.0D0
            ENDIF
        ELSE
            LowTempCutOutFactor = 1.0D0
        ENDIF
        ENDIF

        IF (PartLoadFactor .NE. 0.0D0) THEN
        HeatPumpElectricalEnergy = (HeatingModeLoadFactor * ElectricalPowerConsumption * LowTempCutOutFactor) &
                                        * FractionalBinHours / PartLoadFactor
        ENDIF

        ResistiveSpaceHeatingElectricalEnergy = (BuildingLoad - HeatingModeLoadFactor * NetHeatingCapReduced &
                                                                * LowTempCutOutFactor)  * FractionalBinHours

        TotalBuildingLoad = TotalBuildingLoad + (BuildingLoad * FractionalBinHours)

        TotalHeatPumpElectricalEnergy = TotalHeatPumpElectricalEnergy + HeatPumpElectricalEnergy

        TotalResistiveSpaceHeatingElectricalEnergy = TotalResistiveSpaceHeatingElectricalEnergy + &
                                                        ResistiveSpaceHeatingElectricalEnergy
  END DO

  TotalElectricalEnergy = TotalHeatPumpElectricalEnergy + TotalResistiveSpaceHeatingElectricalEnergy

  IF (DefrostControl .EQ. Timed) THEN
      DemandDeforstCredit = 1.0D0    ! Timed defrost control
  ELSE
      DemandDeforstCredit = 1.03D0   ! Demand defrost control
  ENDIF

  IF (TotalElectricalEnergy .NE. 0.0D0) THEN
      HSPF = TotalBuildingLoad * DemandDeforstCredit / TotalElectricalEnergy
  ENDIF


RETURN
END SUBROUTINE SingleSpeedDXHeatingCoilStandardRatings

SUBROUTINE SingelSpeedDXCoolingCoilStandardRatings(DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                                   EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, &
                                                   RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, &
                                                   FanPowerPerEvapAirFlowRateFromInput, &
                                                   NetCoolingCapRated, SEER, EER, IEER)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Nigusse, FSEC
          !       DATE WRITTEN   December 2012
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the standard ratings net cooling capacity and, SEER, EER and IEER values for single speed
          ! DX cooling coils at the AHRI standard test condition(s).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,    ONLY: CurveValue
  USE DataInterfaces,  ONLY: ShowSevereError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: DXCoilName          ! Name of DX coil for which HSPF is calculated
  CHARACTER(len=*), INTENT(IN)     :: DXCoilType          ! Type of DX coil - heating or cooling
  INTEGER, INTENT(IN)              :: CapFTempCurveIndex  ! Index for the capacity as a function of temperature modifier curve
  INTEGER, INTENT(IN)              :: CapFFlowCurveIndex  ! Index for the capacity as a function of flow fraction modifier curve
  INTEGER, INTENT(IN)              :: EIRFTempCurveIndex  ! Index for the EIR as a function of temperature modifier curve
  INTEGER, INTENT(IN)              :: EIRFFlowCurveIndex  ! Index for the EIR as a function of flow fraction modifier curve
  INTEGER, INTENT(IN)              :: PLFFPLRCurveIndex   ! Index for the EIR vs part-load ratio curve
  REAL(r64), INTENT(IN)            :: RatedTotalCapacity  ! Rated gross total cooling capacity
  REAL(r64), INTENT(IN)            :: RatedCOP            ! Rated gross COP
  REAL(r64), INTENT(IN)            :: RatedAirVolFlowRate ! air flow rate through the coil at rated condition
  REAL(r64), INTENT(IN) :: FanPowerPerEvapAirFlowRateFromInput ! Fan power per air volume flow rate through the evaporator coil
  REAL(r64), INTENT(OUT)           :: NetCoolingCapRated  ! net cooling capacity of single speed DX cooling coil
  REAL(r64), INTENT(OUT)           :: SEER                ! seasonale energy efficiency ratio of single speed DX cooling coil
  REAL(r64), INTENT(OUT)           :: EER                 ! energy efficiency ratio of single speed DX cooling coil
  REAL(r64), INTENT(OUT)           :: IEER                ! Integareted energy efficiency ratio of single speed DX cooling coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,          PARAMETER      :: NumOfReducedCap = 4 ! Number of reduced capacity test conditions (100%,75%,50%,and 25%)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: TotCapFlowModFac    = 0.0d0 ! Total capacity modifier f(actual flow vs rated flow) for each speed [-]
  REAL(r64) :: EIRFlowModFac       = 0.0d0 ! EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
  REAL(r64) :: TotCapTempModFac    = 0.0D0 ! Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
  REAL(r64) :: EIRTempModFac       = 0.0D0 ! EIR modifier (function of entering wetbulb, outside drybulb) [-]
  REAL(r64) :: TotCoolingCapAHRI   = 0.0D0 ! Total Cooling Coil capacity (gross) at AHRI test conditions [W]
  REAL(r64) :: NetCoolingCapAHRI   = 0.0D0 ! Net Cooling Coil capacity at AHRI TestB conditions, accounting for fan heat [W]
  REAL(r64) :: TotalElecPower      = 0.0D0 ! Net power consumption (Cond Fan+Compressor+Indoor Fan) at AHRI test conditions [W]
  REAL(r64) :: TotalElecPowerRated = 0.0D0 ! Net power consumption (Cond Fan+Compressor+Indoor Fan) at Rated test conditions [W]
  REAL(r64) :: EIR                 = 0.0D0 ! Energy Efficiency Ratio at AHRI test conditions for SEER [-]
  REAL(r64) :: PartLoadFactor      = 0.0D0 ! Part load factor, accounts for thermal lag at compressor startup [-]
  REAL(r64) :: EERReduced          = 0.0d0 ! EER at reduced capacity test conditions (100%, 75%, 50%, and 25%)
  REAL(r64) :: ElecPowerReducedCap = 0.0D0 ! Net power consumption (Cond Fan+Compressor) at reduced test condition [W]
  REAL(r64) :: NetCoolingCapReduced= 0.0D0 ! Net Cooling Coil capacity at reduced conditions, accounting for supply fan heat [W]
  REAL(r64) :: LoadFactor          = 0.0D0 ! Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
  REAL(r64) :: DegradationCoeff    = 0.0D0 ! Degradation coeficient, (dimenssionless)
  REAL(r64) :: FanPowerPerEvapAirFlowRate  ! Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
  REAL(r64) :: OutdoorUnitInletAirDrybulbTempReduced  ! Outdoor unit entering air dry-bulb temperature at reduced capacity [C]
  INTEGER   :: RedCapNum                   ! Integer counter for reduced capacity

  IF( FanPowerPerEvapAirFlowRateFromInput <= 0.0D0) THEN
      FanPowerPerEvapAirFlowRate=DefaultFanPowerPerEvapAirFlowRate
  ELSE
      FanPowerPerEvapAirFlowRate=FanPowerPerEvapAirFlowRateFromInput
  ENDIF
  IF (RatedTotalCapacity > 0.0d0) Then

      ! Standard Rating Cooling (net) Capacity calculations:
      TotCapFlowModFac = CurveValue(CapFFlowCurveIndex,AirMassFlowRatioRated)
      TotCapTempModFac = CurveValue(CapFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated,    &
                                    OutdoorUnitInletAirDrybulbTempRated)
      NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac      &
                         - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate

      ! SEER calculations:
      TotCapTempModFac = CurveValue(CapFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated, &
                                    OutdoorUnitInletAirDrybulbTemp)
      TotCoolingCapAHRI = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac
      EIRTempModFac = CurveValue(EIRFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated,     &
                                 OutdoorUnitInletAirDrybulbTemp)
      EIRFlowModFac = CurveValue(EIRFFlowCurveIndex,AirMassFlowRatioRated)
      IF ( RatedCOP > 0.0D0 ) THEN ! RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
           EIR = EIRTempModFac * EIRFlowModFac / RatedCOP
      ELSE
           EIR = 0.0d0
      ENDIF
      ! Calculate net cooling capacity
      NetCoolingCapAHRI = TotCoolingCapAHRI - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
      TotalElecPower = EIR * TotCoolingCapAHRI + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
      ! Calculate SEER value from the Energy Efficiency Ratio (EER) at the AHRI test conditions and the part load factor.
      ! First evaluate the Part Load Factor curve at PLR = 0.5 (AHRI Standard 210/240)
      PartLoadFactor = CurveValue(PLFFPLRCurveIndex,PLRforSEER)
      IF ( TotalElecPower > 0.0D0 ) THEN
           SEER = ( NetCoolingCapAHRI / TotalElecPower ) * PartLoadFactor
      ELSE
           SEER = 0.0d0
      ENDIF

      ! EER calculations:
      ! Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
      TotCapTempModFac = CurveValue(CapFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated, &
                                    OutdoorUnitInletAirDrybulbTempRated)
      NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac   &
                            - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
      ! Calculate Energy Efficiency Ratio (EER) at (19.44C WB and 35.0C DB ), ANSI/AHRI Std. 340/360
      EIRTempModFac = CurveValue(EIRFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated,     &
                      OutdoorUnitInletAirDrybulbTempRated)
      IF ( RatedCOP > 0.0D0 ) THEN
           ! RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
            EIR = EIRTempModFac * EIRFlowModFac / RatedCOP
      ELSE
            EIR = 0.0d0
      ENDIF
      TotalElecPowerRated = EIR * (RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac) &
                          + FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
      IF ( TotalElecPowerRated > 0.0D0 ) THEN
          EER = NetCoolingCapRated / TotalElecPowerRated
      ELSE
          EER = 0.0d0
      ENDIF


      ! IEER calculations:
      IEER =0.0d0
      ! Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
      TotCapTempModFac = CurveValue(CapFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated, &
                                    OutdoorUnitInletAirDrybulbTempRated)
      NetCoolingCapRated = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac   &
                         - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
      DO RedCapNum = 1, NumOfReducedCap
          ! get the outdoor air dry bulb temperature for the reduced capacity test conditions
          IF (ReducedPLR(RedCapNum) > 0.444D0 ) THEN
              OutdoorUnitInletAirDrybulbTempReduced = 5.0D0 + 30.0D0 * ReducedPLR(RedCapNum)
          ELSE
              OutdoorUnitInletAirDrybulbTempReduced = OADBTempLowReducedCapacityTest
          ENDIF
          TotCapTempModFac = CurveValue(CapFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated, &
                                        OutdoorUnitInletAirDrybulbTempReduced)
          NetCoolingCapReduced = RatedTotalCapacity * TotCapTempModFac * TotCapFlowModFac   &
                               - FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate
          EIRTempModFac = CurveValue(EIRFTempCurveIndex,CoolingCoilInletAirWetbulbTempRated,   &
                                     OutdoorUnitInletAirDrybulbTempReduced)
          IF ( RatedCOP > 0.0D0 ) THEN
              EIR = EIRTempModFac * EIRFlowModFac / RatedCOP
          ELSE
              EIR = 0.0d0
          ENDIF
          IF (NetCoolingCapReduced > 0.0d0) THEN
            LoadFactor = ReducedPLR(RedCapNum) * NetCoolingCapRated / NetCoolingCapReduced
          ELSE
            LoadFactor = 1.0d0
          ENDIF
          DegradationCoeff = 1.130D0 - 0.130D0 * LoadFactor
          ElecPowerReducedCap = DegradationCoeff * EIR * (RatedTotalCapacity &
                              * TotCapTempModFac * TotCapFlowModFac)
          EERReduced = ( LoadFactor * NetCoolingCapReduced ) / ( LoadFactor*ElecPowerReducedCap + &
                          FanPowerPerEvapAirFlowRate * RatedAirVolFlowRate)
          IEER = IEER + IEERWeightingFactor(RedCapNum) * EERReduced
      END DO

  ELSE
     CALL ShowSevereError('Standard Ratings: '//TRIM(DXCoilType)//' '//TRIM(DXCoilName)// &
                          ' has zero rated total cooling capacity. Standard ratings cannot be calculated.')
  ENDIF
  RETURN

END SUBROUTINE SingelSpeedDXCoolingCoilStandardRatings

SUBROUTINE MultiSpeedDXCoolingCoilStandardRatings(DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                                  EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, &
                                                  RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, &
                                                  FanPowerPerEvapAirFlowRateFromInput, nsp, &
                                                  NetCoolingCapRatedMaxSpeed, SEER)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Nigusse, FSEC
          !       DATE WRITTEN   December 2012
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the standard ratings net cooling capacity and SEER values for multi speed DX cooling coils
          ! at the AHRI standard test condition(s).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,    ONLY: CurveValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)   :: DXCoilName              ! Name of DX coil for which HSPF is calculated
  CHARACTER(len=*), INTENT(IN)   :: DXCoilType              ! Type of DX coil for which HSPF is calculated
  INTEGER,   INTENT(IN)          :: nsp                     ! Number of compressor speeds
  INTEGER,   INTENT(IN)          :: CapFTempCurveIndex(nsp) ! Index for the capacity as a function of temperature modifier curve
  INTEGER,   INTENT(IN)          :: CapFFlowCurveIndex(nsp) ! Index for the capacity as a function of flow fraction modifier curve
  INTEGER,   INTENT(IN)          :: EIRFTempCurveIndex(nsp) ! Index for the EIR as a function of temperature modifier curve
  INTEGER,   INTENT(IN)          :: EIRFFlowCurveIndex(nsp) ! Index for the EIR as a function of flow fraction modifier curve
  INTEGER,   INTENT(IN)          :: PLFFPLRCurveIndex(nsp)  ! Index for the PLF vs part-load ratio curve
  REAL(r64), INTENT(IN)          :: RatedTotalCapacity(nsp) ! Reference capacity of DX coil [W]
  REAL(r64), INTENT(IN)          :: RatedCOP(nsp)           ! Reference coefficient of performance [W/W]
  REAL(r64), INTENT(IN)          :: RatedAirVolFlowRate(nsp)! Reference air flow rate of DX coil [m3/s]
  REAL(r64), INTENT(IN)          :: FanPowerPerEvapAirFlowRateFromInput(nsp) ! rated fan power per evap air flow rate [W/(m3/s)]
  REAL(r64), INTENT(OUT)         :: NetCoolingCapRatedMaxSpeed ! net cooling capacity at maximum speed
  REAL(r64), INTENT(OUT)         :: SEER                 ! seasonale energy efficiency ratio of multi speed DX cooling coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
  !CHARACTER(len=*), PARAMETER    :: RoutineName='MultiSpeedDXCoolingCoilStandardRatings: ' ! Include trailing blank space
  REAL(r64), PARAMETER           :: SizingFactor = 1.10d0                           ! sizing factor per AHRI Std 210/240-2008
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    ! Intermediate values calculated from the inputs in the idf file
  REAL(r64) :: FanPowerPerEvapAirFlowRate(nsp) ! Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
  REAL(r64) :: TotCoolCapTestA2(nsp)        ! Total cooling capacity at A2 test condition (High speed)
  REAL(r64) :: TotCoolCapTestB2(nsp)        ! Total cooling capacity at B2 test condition (High speed)
  REAL(r64) :: TotCoolCapTestB1(nsp)        ! Total cooling capacity at B1 test condition (Low speed)
  REAL(r64) :: TotCoolCapTestF1(nsp)        ! Total cooling capacity at F1 test condition (Low speed)
  REAL(r64) :: OutdoorUnitPowerTestA2(nsp)  ! Outdoor Unit electric power at A2 test condition (High speed)
  REAL(r64) :: OutdoorUnitPowerTestB2(nsp)  ! Outdoor Unit electric power at B2 test condition (High speed)
  REAL(r64) :: OutdoorUnitPowerTestB1(nsp)  ! Outdoor Unit electric power at B1 test condition (Low speed)
  REAL(r64) :: OutdoorUnitPowerTestF1(nsp)  ! Outdoor Unit electric power at F1 test condition (Low speed)
  REAL(r64) :: NetCoolingCapRated(nsp)      ! net cooling capacity at each speed
  REAL(r64) :: TotCapFlowModFac(nsp)        ! Total capacity modifier f(actual flow vs rated flow) for each speed [-]
  REAL(r64) :: EIRFlowModFac(nsp)           ! EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
  REAL(r64) :: CoolingCapacityLS           = 0.0d0 ! cooling capacity of Mult-speed DX coil at lower speed, [W]
  REAL(r64) :: CoolingCapacityHS           = 0.0d0 ! cooling capacity of Mult-speed DX coil at higher speed, [W]
  REAL(r64) :: CoolingElecPowerLS          = 0.0d0 ! outdoor unit electric power input at low speed, [W]
  REAL(r64) :: CoolingElecPowerHS          = 0.0d0 ! outdoor unit electric power input at high speed, [W]
  REAL(r64) :: CoolingCapacityMax          = 0.0d0 ! cooling capacity of Mult-speed DX coil at max speed, [W]
  REAL(r64) :: CoolingElecPowerMax         = 0.0d0 ! outdoor unit electric power input at Max speed, [W]
  REAL(r64) :: PartLoadRatio               = 0.0d0 ! compressor cycling ratio between successive speeds, [-]
  REAL(r64) :: PartLoadFraction            = 0.0d0 ! part-load fraction that account for the cyclic degradation, [-]
  REAL(r64) :: NetCoolingCapWeighted       = 0.0d0 ! net tot cooling cap weighted by the fraction of the binned cooling hours [W]
  REAL(r64) :: TotCoolingElecPowerWeighted = 0.0d0 ! net total cooling electric power input weighted by the fraction of the
                                                   ! binned cooling hours
  REAL(r64) :: BuildingCoolingLoad         = 0.0d0 ! Building space cooling load corresponding to an outdoor bin temperature [W]
  REAL(r64) :: NetTotCoolCapBinned         = 0.0d0 ! Net tot cooling cap corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotCoolElecPowerBinned      = 0.0d0 ! Total cooling electric power corresponding to an outdoor bin temperature [W]
  REAL(r64) :: LoadFactor                  = 0.0d0 ! "on" time for last stage at the desired reduced capacity, (dimensionless)
  INTEGER   :: BinNum                              ! bin number counter
  INTEGER   :: spnum                               ! compressor speed number

    TotCoolingElecPowerWeighted = 0.0d0
    TotCoolingElecPowerWeighted = 0.0d0
    
    DO spnum = 1, nsp
      FanPowerPerEvapAirFlowRate(spnum)=0.0d0
      IF( FanPowerPerEvapAirFlowRateFromInput(spnum) <= 0.0d0) THEN
          FanPowerPerEvapAirFlowRate(spnum)=DefaultFanPowerPerEvapAirFlowRate
      ELSE
          FanPowerPerEvapAirFlowRate(spnum)=FanPowerPerEvapAirFlowRateFromInput(spnum)
      ENDIF
    END DO

    ! Calculate the capacity and power for each speed
    DO spnum = 1, nsp
      TotCapFlowModFac(spnum) = CurveValue(CapFFlowCurveIndex(spnum),AirMassFlowRatioRated)
      TotCoolCapTestA2(spnum) = RatedTotalCapacity(spnum) &
                              * CurveValue(CapFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                OutdoorCoilInletAirDrybulbTempTestA2) * TotCapFlowModFac(spnum) &
                              - FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      TotCoolCapTestB2(spnum) = RatedTotalCapacity(spnum) &
                              * CurveValue(CapFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                OutdoorCoilInletAirDrybulbTempTestB2) * TotCapFlowModFac(spnum) &
                              - FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      TotCoolCapTestB1(spnum) = RatedTotalCapacity(spnum) &
                              * CurveValue(CapFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                OutdoorCoilInletAirDrybulbTempTestB1) * TotCapFlowModFac(spnum) &
                              - FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      TotCoolCapTestF1(spnum) = RatedTotalCapacity(spnum) &
                              * CurveValue(CapFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                OutdoorCoilInletAirDrybulbTempTestF1) * TotCapFlowModFac(spnum) &
                              - FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)

      EIRFlowModFac(spnum) = CurveValue(EIRFFlowCurveIndex(spnum),AirMassFlowRatioRated)
      IF (RatedCOP(spnum) > 0.0d0) Then
        OutdoorUnitPowerTestA2(spnum) = TotCoolCapTestA2(spnum) * EIRFlowModFac(spnum) &
                                      * CurveValue(EIRFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                        OutdoorCoilInletAirDrybulbTempTestA2) / RatedCOP(spnum)  &
                                      + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
        OutdoorUnitPowerTestB2(spnum) = TotCoolCapTestB2(spnum) * EIRFlowModFac(spnum) &
                                      * CurveValue(EIRFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                        OutdoorCoilInletAirDrybulbTempTestB2) / RatedCOP(spnum)  &
                                      + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
        OutdoorUnitPowerTestB1(spnum) = TotCoolCapTestB1(spnum) * EIRFlowModFac(spnum) &
                                      * CurveValue(EIRFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                        OutdoorCoilInletAirDrybulbTempTestB1) / RatedCOP(spnum)  &
                                      + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
        OutdoorUnitPowerTestF1(spnum) = TotCoolCapTestF1(spnum) * EIRFlowModFac(spnum) &
                                      * CurveValue(EIRFTempCurveIndex(spnum),IndoorCoilInletAirWetbulbTempRated, &
                                        OutdoorCoilInletAirDrybulbTempTestF1) / RatedCOP(spnum)  &
                                      + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      ENDIF
    END DO
    ! Standard Rating cooling (net) capacity calculations:
    NetCoolingCapRated(nsp) = TotCoolCapTestA2(nsp)
    NetCoolingCapRatedMaxSpeed = NetCoolingCapRated(nsp)

    ! Calculate the SEER value based on contribution of each outdoor air bin temperature
    DO BinNum = 1, NumOfOATempBins
       BuildingCoolingLoad = (OutdoorBinTemperatureSEER(BinNum) - 18.3d0) / (35.0d0 - 18.3d0) &
                           * (TotCoolCapTestA2(nsp) / SizingFactor)
      ! determine the speed number
       CoolingCapacityMax = TotCoolCapTestB2(nsp) &
                          + ((TotCoolCapTestA2(nsp) - TotCoolCapTestB2(nsp)) &
                          / (OutdoorCoilInletAirDrybulbTempTestA2 - OutdoorCoilInletAirDrybulbTempTestB2))  &
                          * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestB2)
       CoolingElecPowerMax = OutdoorUnitPowerTestB2(nsp) &
                           + ((OutdoorUnitPowerTestA2(nsp) - OutdoorUnitPowerTestB2(nsp)) &
                           / (OutdoorCoilInletAirDrybulbTempTestA2 - OutdoorCoilInletAirDrybulbTempTestB2))  &
                           * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestB2)

       SpeedLoop: DO spnum = 1, nsp-1
         CoolingCapacityLS = TotCoolCapTestF1(spnum)   &
                           + ((TotCoolCapTestB1(spnum) - TotCoolCapTestF1(spnum))  &
                           / (OutdoorCoilInletAirDrybulbTempTestB1 - OutdoorCoilInletAirDrybulbTempTestF1))  &
                           * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestF1)
         CoolingElecPowerLS = OutdoorUnitPowerTestF1(spnum) &
                            + ((OutdoorUnitPowerTestB1(spnum) - OutdoorUnitPowerTestF1(spnum)) &
                            / (OutdoorCoilInletAirDrybulbTempTestB1 - OutdoorCoilInletAirDrybulbTempTestF1))  &
                            * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestF1)
         CoolingCapacityHS = TotCoolCapTestB2(spnum+1) &
                           + ((TotCoolCapTestA2(spnum+1) - TotCoolCapTestB2(spnum+1)) &
                           / (OutdoorCoilInletAirDrybulbTempTestA2 - OutdoorCoilInletAirDrybulbTempTestB2))  &
                           * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestB2)
         CoolingElecPowerHS = OutdoorUnitPowerTestB2(spnum+1) &
                            + ((OutdoorUnitPowerTestA2(spnum+1) - OutdoorUnitPowerTestB2(spnum+1)) &
                            / (OutdoorCoilInletAirDrybulbTempTestA2 - OutdoorCoilInletAirDrybulbTempTestB2))  &
                            * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestB2)

         IF (BuildingCoolingLoad .LE.  CoolingCapacityLS ) THEN
            PartLoadRatio = MIN(1.0d0, BuildingCoolingLoad / CoolingCapacityLS)
            NetTotCoolCapBinned = PartLoadRatio * CoolingCapacityLS
            PartLoadFraction = 1.0d0 - CyclicDegradationCoeff * (1.0d0 - PartLoadRatio)
            TotCoolElecPowerBinned = (PartLoadRatio/PartLoadFraction) * CoolingElecPowerLS
            Exit SpeedLoop
         ELSEIF ((BuildingCoolingLoad .GT. CoolingCapacityLS) .AND. &
                (BuildingCoolingLoad .LT. CoolingCapacityHS)) THEN
            ! cycle between speed "spnum" and "spnum + 1"
            LoadFactor = MIN(1.0d0, (CoolingCapacityHS - BuildingCoolingLoad)/(CoolingCapacityHS - CoolingCapacityLS))
            LoadFactor = MAX(0.0d0, LoadFactor)
            NetTotCoolCapBinned = LoadFactor*CoolingCapacityLS + (1.0d0-LoadFactor)*CoolingCapacityHS
            TotCoolElecPowerBinned = LoadFactor*CoolingElecPowerLS + (1.0d0-LoadFactor)*CoolingElecPowerHS
            Exit SpeedLoop
         ELSEIF (BuildingCoolingLoad .GE.  CoolingCapacityMax) THEN
            NetTotCoolCapBinned = CoolingCapacityMax
            TotCoolElecPowerBinned = CoolingElecPowerMax
            Exit SpeedLoop
         ENDIF
      END DO SpeedLoop

      NetCoolingCapWeighted = NetCoolingCapWeighted &
                            + NetTotCoolCapBinned * CoolFracBinHoursAtOutdoorBinTemp(BinNum)
      TotCoolingElecPowerWeighted = TotCoolingElecPowerWeighted &
                                  + TotCoolElecPowerBinned * CoolFracBinHoursAtOutdoorBinTemp(BinNum)
    END DO
    IF ( TotCoolingElecPowerWeighted > 0.0d0 ) THEN
         SEER = NetCoolingCapWeighted / TotCoolingElecPowerWeighted
    ELSE
         SEER = 0.0d0
    ENDIF

  RETURN

END SUBROUTINE MultiSpeedDXCoolingCoilStandardRatings

SUBROUTINE MultiSpeedDXHeatingCoilStandardRatings(DXCoilName, DXCoilType, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                                  EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, &
                                                  RatedTotalCapacity, RatedCOP, RatedAirVolFlowRate, &
                                                  FanPowerPerEvapAirFlowRateFromInput, nsp, RegionNum, MinOATCompressor, &
                                                  OATempCompressorOn, OATempCompressorOnOffBlank,DefrostControl, &
                                                  NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp,HSPF)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Nigusse, FSEC
          !       DATE WRITTEN   December 2012
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the standard ratings net heating capacity and HSPF values for multi speed DX heating coils
          ! at the AHRI standard test condition(s).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager,    ONLY: CurveValue, GetCurveType

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: DXCoilName              ! Name of DX coil for which HSPF is calculated
  CHARACTER(len=*), INTENT(IN)    :: DXCoilType              ! Type of DX coil for which HSPF is calculated
  INTEGER,   INTENT(IN)           :: nsp                     ! Number of compressor speeds
  INTEGER,   INTENT(IN)           :: CapFTempCurveIndex(nsp) ! Index for the capacity as a function of temperature modifier curve
  INTEGER,   INTENT(IN)           :: CapFFlowCurveIndex(nsp) ! Index for the capacity as a function of flow fraction modifier curve
  INTEGER,   INTENT(IN)           :: EIRFTempCurveIndex(nsp) ! Index for the EIR as a function of temperature modifier curve
  INTEGER,   INTENT(IN)           :: EIRFFlowCurveIndex(nsp) ! Index for the EIR as a function of flow fraction modifier curve
  INTEGER,   INTENT(IN)           :: PLFFPLRCurveIndex(nsp)  ! Index for the PLF vs part-load ratio curve
  REAL(r64), INTENT(IN)           :: RatedTotalCapacity(nsp) ! Reference capacity of DX coil [W]
  REAL(r64), INTENT(IN)           :: RatedCOP(nsp)           ! Reference coefficient of performance [W/W]
  REAL(r64), INTENT(IN)           :: RatedAirVolFlowRate(nsp)! Reference air flow rate of DX coil [m3/s]
  REAL(r64), INTENT(IN)           :: FanPowerPerEvapAirFlowRateFromInput(nsp) ! rated fan power per evap air flow rate [W/(m3/s)]

  INTEGER,   OPTIONAL, INTENT(IN) :: RegionNum               ! Region number for calculating HSPF of single speed DX heating coil
  INTEGER,   OPTIONAL, INTENT(IN) :: DefrostControl          ! defrost control; 1=timed, 2=on-demand
  REAL(r64), OPTIONAL, INTENT(IN) :: MinOATCompressor        ! Minimum OAT for heat pump compressor operation [C]
  LOGICAL,   OPTIONAL, INTENT(IN) :: OATempCompressorOnOffBlank ! Flag used to determine low temperature cut out factor
  REAL(r64), OPTIONAL, INTENT(IN) :: OATempCompressorOn      ! The outdoor tempearture when the compressor is automatically turned
                                                             ! back on, if applicable, following automatic shut off. This field is
                                                             ! used only for HSPF calculation. [C]
  REAL(r64), INTENT(OUT)          :: NetHeatingCapRatedHighTemp ! net heating capacity at maximum speed and High Temp
  REAL(r64), INTENT(OUT)          :: NetHeatingCapRatedLowTemp  ! net heating capacity at maximum speed and low Temp
  REAL(r64), INTENT(OUT)          :: HSPF                       ! seasonale energy efficiency ratio of multi speed DX cooling coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
  !CHARACTER(len=*), PARAMETER      :: RoutineName='MultiSpeedDXHeatingCoilStandardRatings: ' ! Include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    ! Intermediate values calculated from the inputs in the idf file
  REAL(r64) :: FanPowerPerEvapAirFlowRate(nsp) ! Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]
  REAL(r64) :: TotHeatCapTestH0(nsp)        ! Total cooling capacity at A2 test condition (High speed)
  REAL(r64) :: TotHeatCapTestH1(nsp)        ! Total cooling capacity at B2 test condition (High speed)
  REAL(r64) :: TotHeatCapTestH2(nsp)        ! Total cooling capacity at B1 test condition (Low speed)
  REAL(r64) :: TotHeatCapTestH3(nsp)        ! Total cooling capacity at F1 test condition (Low speed)
  REAL(r64) :: OutdoorUnitPowerTestH0(nsp)  ! Outdoor Unit electric power at A2 test condition (High speed)
  REAL(r64) :: OutdoorUnitPowerTestH1(nsp)  ! Outdoor Unit electric power at B2 test condition (High speed)
  REAL(r64) :: OutdoorUnitPowerTestH2(nsp)  ! Outdoor Unit electric power at B1 test condition (Low speed)
  REAL(r64) :: OutdoorUnitPowerTestH3(nsp)  ! Outdoor Unit electric power at F1 test condition (Low speed)
  REAL(r64) :: HeatingCapacityLS           ! cooling capacity of Mult-speed DX coil at lower speed, [W]
  REAL(r64) :: HeatingCapacityHS           ! cooling capacity of Mult-speed DX coil at higher speed, [W]
  REAL(r64) :: HeatingElecPowerLS          ! outdoor unit electric power input at low speed, [W]
  REAL(r64) :: HeatingElecPowerHS          ! outdoor unit electric power input at high speed, [W]
  REAL(r64) :: HeatingCapacityMax          ! cooling capacity of Mult-speed DX coil at max speed, [W]
  REAL(r64) :: HeatingElecPowerMax         ! outdoor unit electric power input at Max speed, [W]
  REAL(r64) :: TotHeatCapTestH1High(nsp)   ! net heating capacity high speed at H1 test conditon, [W]

! Intermediate values calculated from the inputs in the idf file
  REAL(r64) :: TotCapFlowModFac(nsp)       ! Total capacity modifier f(actual flow vs rated flow) for each speed [-]
  REAL(r64) :: EIRFlowModFac(nsp)          ! EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
  REAL(r64) :: TotCapTempModFac = 0.0D0    ! Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
  REAL(r64) :: EIRTempModFac = 0.0D0       ! EIR modifier (function of entering wetbulb, outside drybulb) [-]

  REAL(r64) :: TotCapTempModFacH0 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H0 Test [-]
  REAL(r64) :: EIRTempModFacH0 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H0 Test[-]
  REAL(r64) :: TotCapTempModFacH1 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H1 Test [-]
  REAL(r64) :: EIRTempModFacH1 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H1 Test[-]
  REAL(r64) :: TotCapTempModFacH2 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H2 Test [-]
  REAL(r64) :: EIRTempModFacH2 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H2 Test[-]
  REAL(r64) :: TotCapTempModFacH3 = 0.0D0  ! Tot capacity modifier (function of entering wetbulb, outside drybulb) at H3 Test [-]
  REAL(r64) :: EIRTempModFacH3 = 0.0D0     ! EIR modifier (function of entering wetbulb, outside drybulb)  at H3 Test[-]

  REAL(r64) :: PartLoadFactor = 0.0D0      ! Part load factor, accounts for thermal lag at compressor startup [-]
  REAL(r64) :: OATempCompressorOff = 0.0D0 ! Minimum outdoor air temperature to turn the commpressor off
  !
  REAL(r64) :: PartLoadRatio               = 0.0d0 ! compressor cycling ratio between successive speeds, [-]
  REAL(r64) :: PartLoadFraction            = 0.0d0 ! part-load fraction that account for the cyclic degradation, [-]

  REAL(r64) :: NetHeatingCapWeighted       = 0.0d0 ! net total heating cap weighted by the fraction of the binned cooling hours [W]
  REAL(r64) :: TotHeatingElecPowerWeighted = 0.0d0 ! net total heat pump and resistance heating electric Energy input weighted by
                                                   ! the fraction of the binned cooling hours
  REAL(r64) :: BuildingHeatingLoad         = 0.0d0 ! Building space heating load corresponding to an outdoor bin temperature [W]
  REAL(r64) :: NetTotHeatCapBinned         = 0.0d0 ! Net tot heatinging cap corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotHeatElecPowerBinnedHP    = 0.0d0 ! Total Heat Pump heating electric power consumption at outdoor bin temp [W]
  REAL(r64) :: TotHeatElecPowerBinnedRH    = 0.0d0 ! Total Resistance heating electric power consumption at outdoor bin temp [W]


  REAL(r64) :: LoadFactor               ! Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
  REAL(r64) :: LowTempCutOutFactor = 0.0d0 ! Factor which corresponds to compressor operation depending on outdoor temperature

  REAL(r64) :: NetHeatingCapRated     = 0.0d0 ! Net Heating Coil capacity at Rated conditions, accounting for supply fan heat [W]
  REAL(r64) :: ElecPowerRated         = 0.0d0 ! Total system power at Rated conditions, accounting for supply fan heat [W]
  REAL(r64) :: NetHeatingCapH2Test    = 0.0d0 ! Net Heating Coil capacity at H2 test conditions, accounting for supply fan heat [W]
  REAL(r64) :: ElecPowerH2Test        = 0.0d0 ! Total system power at H2 test conditions, accounting for supply fan heat [W]
  REAL(r64) :: NetHeatingCapH3Test    = 0.0d0 ! Net Heating Coil capacity at H3 test conditions, accounting for supply fan heat [W]
  REAL(r64) :: ElecPowerH3Test        = 0.0d0 ! Total system power at H3 test conditions, accounting for supply fan heat [W]
  REAL(r64) :: FractionalBinHours     = 0.0d0 ! Fractional bin hours for the heating season  [-]
  REAL(r64) :: BuildingLoad           = 0.0d0 ! Building space conditioning load corresponding to an outdoor bin temperature [W]
  REAL(r64) :: HeatingModeLoadFactor  = 0.0d0 ! Heating mode load factor corresponding to an outdoor bin temperature  [-]
  REAL(r64) :: NetHeatingCapReduced   = 0.0d0 ! Net Heating Coil capacity corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotalBuildingLoad      = 0.0d0 ! Sum of building load over the entire heating season [W]
  REAL(r64) :: TotalElectricalEnergy  = 0.0d0 ! Sum of electrical energy consumed by the heatpump over the heating season [W]
  REAL(r64) :: DemandDeforstCredit    = 1.0d0 ! A factor to adjust HSPF if coil has demand defrost control  [-]
  REAL(r64) :: CheckCOP               = 0.0d0 ! Checking COP at an outdoor bin temperature against unity [-]
  REAL(r64) :: DesignHeatingRequirement      = 0.0d0 ! The amount of heating required to maintain a given indoor temperature
                                                     ! at a particular outdoor design temperature.  [W]
  REAL(r64) :: DesignHeatingRequirementMin   = 0.0d0 ! minimum design heating requirement [W]
  REAL(r64) :: DesignHeatingRequirementMax   = 0.0d0 ! maximum design heating requirement [W]
  REAL(r64) :: ElectricalPowerConsumption    = 0.0d0 ! Electrical power corresponding to an outdoor bin temperature [W]
  REAL(r64) :: HeatPumpElectricalEnergy      = 0.0d0 ! Heatpump electrical energy corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotalHeatPumpElectricalEnergy = 0.0d0 ! Sum of Heatpump electrical energy over the entire heating season [W]
  REAL(r64) :: ResistiveSpaceHeatingElectricalEnergy = 0.0d0      ! resistance heating electrical energy corresponding to an
                                                                  ! outdoor bin temperature [W]
  REAL(r64) :: TotalResistiveSpaceHeatingElectricalEnergy = 0.0d0 ! Sum of resistance heating electrical energy over the
                                                                  ! entire heating season [W]
!
  INTEGER   :: BinNum                 ! bin number counter
  INTEGER   :: spnum                  ! compressor speed number
  INTEGER   :: StandardDHRNum         ! Integer counter for standardized DHRs

  NetHeatingCapWeighted = 0.0d0
  TotHeatingElecPowerWeighted = 0.0d0
  
  DO spnum = 1, nsp
    FanPowerPerEvapAirFlowRate(spnum)=0.0d0
    IF( FanPowerPerEvapAirFlowRateFromInput(spnum) <= 0.0d0) THEN
        FanPowerPerEvapAirFlowRate(spnum)=DefaultFanPowerPerEvapAirFlowRate
    ELSE
        FanPowerPerEvapAirFlowRate(spnum)=FanPowerPerEvapAirFlowRateFromInput(spnum)
    ENDIF
  END DO

  ! Proceed withe HSPF value calculation
  DO spnum = 1, nsp
   TotCapFlowModFac(spnum) = CurveValue(CapFFlowCurveIndex(spnum),AirMassFlowRatioRated)
   SELECT CASE(GetCurveType(CapFTempCurveIndex(spnum)))
     CASE('QUADRATIC', 'CUBIC')
            TotCapTempModFacH0 = CurveValue(CapFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempH0Test)
            TotCapTempModFacH1 = CurveValue(CapFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempRated)
            TotCapTempModFacH2 = CurveValue(CapFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempH2Test)
            TotCapTempModFacH3 = CurveValue(CapFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempH3Test)
     CASE('BIQUADRATIC')
            TotCapTempModFacH0 = CurveValue(CapFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                            HeatingOutdoorCoilInletAirDBTempH0Test)
            TotCapTempModFacH1 = CurveValue(CapFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                            HeatingOutdoorCoilInletAirDBTempRated)
            TotCapTempModFacH2 = CurveValue(CapFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                            HeatingOutdoorCoilInletAirDBTempH2Test)
            TotCapTempModFacH3 = CurveValue(CapFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                            HeatingOutdoorCoilInletAirDBTempH3Test)
   END SELECT

    TotHeatCapTestH0(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH0 * TotCapFlowModFac(spnum) &
                            + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
    TotHeatCapTestH1(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH1 * TotCapFlowModFac(spnum) &
                            + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
    TotHeatCapTestH2(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH2 * TotCapFlowModFac(spnum) &
                            + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
    TotHeatCapTestH3(spnum) = RatedTotalCapacity(spnum) * TotCapTempModFacH3 * TotCapFlowModFac(spnum) &
                            + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)

    EIRFlowModFac(spnum) = CurveValue(EIRFFlowCurveIndex(spnum),AirMassFlowRatioRated)

    SELECT CASE(GetCurveType(EIRFTempCurveIndex(spnum)))
      CASE('QUADRATIC', 'CUBIC')
            EIRTempModFacH0 = CurveValue(EIRFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempH0Test)
            EIRTempModFacH1 = CurveValue(EIRFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempRated)
            EIRTempModFacH2 = CurveValue(EIRFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempH2Test)
            EIRTempModFacH3 = CurveValue(EIRFTempCurveIndex(spnum),HeatingOutdoorCoilInletAirDBTempH3Test)
      CASE('BIQUADRATIC')
            EIRTempModFacH0 = CurveValue(EIRFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH0Test)
            EIRTempModFacH1 = CurveValue(EIRFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempRated)
            EIRTempModFacH2 = CurveValue(EIRFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH2Test)
            EIRTempModFacH3 = CurveValue(EIRFTempCurveIndex(spnum),HeatingIndoorCoilInletAirDBTempRated, &
                                         HeatingOutdoorCoilInletAirDBTempH3Test)
    END SELECT
    IF ( RatedCOP(spnum) > 0.0d0) THEN
      OutdoorUnitPowerTestH0(spnum) = TotHeatCapTestH0(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH0 / RatedCOP(spnum)  &
                                    + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      OutdoorUnitPowerTestH1(spnum) = TotHeatCapTestH1(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH1 / RatedCOP(spnum)  &
                                    + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      OutdoorUnitPowerTestH2(spnum) = TotHeatCapTestH2(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH2 / RatedCOP(spnum)  &
                                    + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
      OutdoorUnitPowerTestH3(spnum) = TotHeatCapTestH3(spnum) * EIRFlowModFac(spnum) * EIRTempModFacH3 / RatedCOP(spnum)  &
                                    + FanPowerPerEvapAirFlowRate(spnum) * RatedAirVolFlowRate(spnum)
    ENDIF
  END DO

  ! determine the HP capacity at the rated condition (AHRI H1 high speed test Condition); and determine the
  ! the building heat requirement for the user specified region
  NetHeatingCapRatedHighTemp = TotHeatCapTestH1(nsp)
  NetHeatingCapRatedLowTemp = TotHeatCapTestH3(nsp)

  IF (RegionNum .EQ. 5) THEN
     DesignHeatingRequirementMin = NetHeatingCapRatedHighTemp
     DesignHeatingRequirementMax = 2.20d0 * NetHeatingCapRatedHighTemp
  ELSE
     DesignHeatingRequirementMin = NetHeatingCapRatedHighTemp*(18.33d0 - OutdoorDesignTemperature(RegionNum)) / (60.0d0/1.80d0)
     DesignHeatingRequirementMax = 2.20d0 * DesignHeatingRequirementMin
  ENDIF
  !
  ! Set the Design Heating Requirement to nearest standard value (From Table 18, AHRI/ANSI Std 210/240)
  DO StandardDHRNum = 1, TotalNumOfStandardDHRs - 1
     IF (DesignHeatingRequirementMin .LT. StandardDesignHeatingRequirement(1)) Then

         DesignHeatingRequirement = MIN(StandardDesignHeatingRequirement(1), DesignHeatingRequirementMax)

     ELSEIF ( DesignHeatingRequirementMin .GE. StandardDesignHeatingRequirement(StandardDHRNum) .AND. &
              DesignHeatingRequirementMin  .LT. StandardDesignHeatingRequirement(StandardDHRNum+1)) THEN
          IF ((DesignHeatingRequirementMin - StandardDesignHeatingRequirement(StandardDHRNum)) .GT. &
              (StandardDesignHeatingRequirement(StandardDHRNum+1) - DesignHeatingRequirementMin)) THEN

              DesignHeatingRequirement = MIN(StandardDesignHeatingRequirement(StandardDHRNum+1), &
                                             DesignHeatingRequirementMax)
          ELSE
              DesignHeatingRequirement = MIN(StandardDesignHeatingRequirement(StandardDHRNum), &
                                             DesignHeatingRequirementMax)
          ENDIF
     ELSEIF(DesignHeatingRequirementMin .GE. StandardDesignHeatingRequirement(TotalNumOfStandardDHRs)) THEN
              DesignHeatingRequirement = MIN(StandardDesignHeatingRequirement(StandardDHRNum), &
                                             DesignHeatingRequirementMax)
    ENDIF
  END DO
  !
  ! The minimum temperature below which the compressor is turned off
  OATempCompressorOff = MinOATCompressor

  DO BinNum = 1, TotalNumOfTemperatureBins(RegionNum)  !NumOfOATempBins

     Select Case (RegionNum)
       Case (1)
         FractionalBinHours = RegionOneFracBinHoursAtOutdoorBinTemp(BinNum)
       Case (2)
         FractionalBinHours = RegionTwoFracBinHoursAtOutdoorBinTemp(BinNum)
       Case (3)
         FractionalBinHours = RegionThreeFracBinHoursAtOutdoorBinTemp(BinNum)
       Case (4)
         FractionalBinHours = RegionFourFracBinHoursAtOutdoorBinTemp(BinNum)
       Case (5)
         FractionalBinHours = RegionFiveFracBinHoursAtOutdoorBinTemp(BinNum)
       Case (6)
         FractionalBinHours = RegionSixFracBinHoursAtOutdoorBinTemp(BinNum)
       Case Default
         FractionalBinHours = RegionFourFracBinHoursAtOutdoorBinTemp(BinNum)
       End Select

     ! Calculate the building heating load
     BuildingHeatingLoad = (18.33d0 - OutdoorBinTemperature(BinNum)) / (18.33d0 - OutdoorDesignTemperature(RegionNum)) &
                         * CorrectionFactor * DesignHeatingRequirement

     IF ( (OutdoorBinTemperature(BinNum) .LE. -8.33d0) .OR. &
          (OutdoorBinTemperature(BinNum) .GE.  7.20d0) ) THEN
         HeatingCapacityMax = TotHeatCapTestH3(nsp) + ((TotHeatCapTestH1(nsp) - TotHeatCapTestH3(nsp)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test))
         HeatingElecPowerMax = OutdoorUnitPowerTestH3(nsp) &
                            + ((OutdoorUnitPowerTestH1(nsp) - OutdoorUnitPowerTestH3(nsp)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test))
     ELSE
         HeatingCapacityMax = TotHeatCapTestH3(nsp) + ((TotHeatCapTestH2(nsp) - TotHeatCapTestH3(nsp))  &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test))
         HeatingElecPowerMax = OutdoorUnitPowerTestH3(nsp) &
                             + ((OutdoorUnitPowerTestH2(nsp) - OutdoorUnitPowerTestH3(nsp)) &
                             * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                             / (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test))
     END IF

     ! determine the speed number
     HeatSpeedLoop: DO spnum = 1, nsp-1
       ! Low Speed
       IF ( OutdoorBinTemperature(BinNum) .LT. -8.33d0) THEN
         HeatingCapacityLS = TotHeatCapTestH3(spnum) + ((TotHeatCapTestH1(spnum) - TotHeatCapTestH3(spnum))  &
                           * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                           / (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test))
         HeatingElecPowerLS = OutdoorUnitPowerTestH3(spnum) + ((OutdoorUnitPowerTestH1(spnum) - OutdoorUnitPowerTestH3(spnum)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test))

       ELSEIF (OutdoorBinTemperature(BinNum) .GE. 4.44d0) THEN
         HeatingCapacityLS = TotHeatCapTestH1(spnum) + ((TotHeatCapTestH0(spnum) - TotHeatCapTestH1(spnum))  &
                           * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempRated) &
                           / (HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated))
         HeatingElecPowerLS = OutdoorUnitPowerTestH1(spnum) + ((OutdoorUnitPowerTestH0(spnum) - OutdoorUnitPowerTestH1(spnum)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempRated) &
                            / (HeatingOutdoorCoilInletAirDBTempH0Test - HeatingOutdoorCoilInletAirDBTempRated))
       ELSE
         HeatingCapacityLS = TotHeatCapTestH3(spnum) + ((TotHeatCapTestH2(spnum) - TotHeatCapTestH3(spnum))  &
                           * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                           / (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test))
         HeatingElecPowerLS = OutdoorUnitPowerTestH3(spnum) + ((OutdoorUnitPowerTestH2(spnum) - OutdoorUnitPowerTestH3(spnum)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test))
       ENDIF
       ! High Speed
       IF ( (OutdoorBinTemperature(BinNum) .LE. -8.33d0) .OR. &
            (OutdoorBinTemperature(BinNum) .GE.  7.20d0) ) THEN
         HeatingCapacityHS = TotHeatCapTestH3(spnum+1) + ((TotHeatCapTestH1(spnum+1) - TotHeatCapTestH3(spnum+1)) &
                           * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                           / (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test))
         HeatingElecPowerHS = OutdoorUnitPowerTestH3(spnum+1) &
                            + ((OutdoorUnitPowerTestH1(spnum+1) - OutdoorUnitPowerTestH3(spnum+1)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempRated - HeatingOutdoorCoilInletAirDBTempH3Test))
       ELSE
         HeatingCapacityHS = TotHeatCapTestH3(spnum+1) + ((TotHeatCapTestH2(spnum+1) - TotHeatCapTestH3(spnum+1))  &
                           * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                           / (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test))
         HeatingElecPowerHS = OutdoorUnitPowerTestH3(spnum+1) &
                            + ((OutdoorUnitPowerTestH2(spnum+1) - OutdoorUnitPowerTestH3(spnum+1)) &
                            * (OutdoorBinTemperature(BinNum) - HeatingOutdoorCoilInletAirDBTempH3Test) &
                            / (HeatingOutdoorCoilInletAirDBTempH2Test - HeatingOutdoorCoilInletAirDBTempH3Test))
       ENDIF
       !
       LowTempCutOutFactor = 0.0D0
       IF(.NOT. OATempCompressorOnOffBlank) THEN
         IF (OutdoorBinTemperature(BinNum) .LE. OATempCompressorOff) THEN
           LowTempCutOutFactor = 0.0D0
         ELSEIF (OutdoorBinTemperature(BinNum) .GT. OATempCompressorOff .and. &
                 OutdoorBinTemperature(BinNum) .LE. OATempCompressorOn) THEN
           LowTempCutOutFactor = 0.5D0
         ELSE
           LowTempCutOutFactor = 1.0D0
         ENDIF
       ELSE
           LowTempCutOutFactor = 1.0D0
       ENDIF

       IF (BuildingHeatingLoad .LE.  HeatingCapacityLS ) THEN
         IF (HeatingCapacityLS > 0.0d0) PartLoadRatio = MIN(1.0d0, BuildingHeatingLoad / HeatingCapacityLS)
         NetTotHeatCapBinned = BuildingHeatingLoad
         PartLoadFraction = 1.0d0 - CyclicDegradationCoeff * (1.0d0 - PartLoadRatio)
         TotHeatElecPowerBinnedHP = (PartLoadRatio/PartLoadFraction) * HeatingElecPowerLS * LowTempCutOutFactor
         TotHeatElecPowerBinnedRH = BuildingHeatingLoad * (1.0d0 - LowTempCutOutFactor)

         Exit HeatSpeedLoop

       ELSEIF ((BuildingHeatingLoad .GT. HeatingCapacityLS) .AND. (BuildingHeatingLoad .LT. HeatingCapacityHS)) THEN
         ! cycle between speed "spnum" and "spnum + 1"
         LoadFactor = MIN(1.0d0, (HeatingCapacityHS - BuildingHeatingLoad)/(HeatingCapacityHS - HeatingCapacityLS))
         LoadFactor = MAX(0.0d0, LoadFactor)

         NetTotHeatCapBinned = BuildingHeatingLoad
         TotHeatElecPowerBinnedHP = LoadFactor*HeatingElecPowerLS + (1.0d0-LoadFactor)*HeatingElecPowerHS
         TotHeatElecPowerBinnedHP = TotHeatElecPowerBinnedHP * LowTempCutOutFactor
         TotHeatElecPowerBinnedRH = BuildingHeatingLoad * (1.0d0 - LowTempCutOutFactor)

         Exit HeatSpeedLoop

       ELSEIF (BuildingHeatingLoad .GE.  HeatingCapacityMax) THEN
         NetTotHeatCapBinned = BuildingHeatingLoad
         IF(.NOT. OATempCompressorOnOffBlank .AND. HeatingElecPowerMax > 0.0d0) THEN
            IF ( (OutdoorBinTemperature(BinNum) .LE. OATempCompressorOff) .OR. &
                 (HeatingCapacityMax/HeatingElecPowerMax .LT. 1.0d0)  )     THEN
               LowTempCutOutFactor = 0.0D0
            ELSEIF ( (OutdoorBinTemperature(BinNum) .GT. OATempCompressorOff .AND. &
                      OutdoorBinTemperature(BinNum) .LE. OATempCompressorOn) .AND. &
                     (HeatingCapacityMax/HeatingElecPowerMax .GT. 1.0d0) )      THEN
               LowTempCutOutFactor = 0.5D0
            ELSEIF ( (OutdoorBinTemperature(BinNum) .GT. OATempCompressorOn) .AND. &
                   (HeatingCapacityMax/HeatingElecPowerMax .GT. 1.0d0)  )     THEN
                LowTempCutOutFactor = 1.0D0
            ENDIF
         ELSE
                LowTempCutOutFactor = 1.0D0
         ENDIF

         TotHeatElecPowerBinnedHP = HeatingElecPowerMax * LowTempCutOutFactor
         TotHeatElecPowerBinnedRH = BuildingHeatingLoad - HeatingCapacityMax * LowTempCutOutFactor

         Exit HeatSpeedLoop
       ENDIF
     END DO HeatSpeedLoop

     NetHeatingCapWeighted = NetHeatingCapWeighted + NetTotHeatCapBinned * FractionalBinHours
     TotHeatingElecPowerWeighted = TotHeatingElecPowerWeighted &
                                 + (TotHeatElecPowerBinnedHP + TotHeatElecPowerBinnedRH )* FractionalBinHours
  END DO

  IF (DefrostControl .EQ. Timed) THEN
    DemandDeforstCredit = 1.0D0   ! Timed defrost control
  ELSE
    DemandDeforstCredit = 1.03D0  ! Demand defrost control
  ENDIF

  IF ( TotHeatingElecPowerWeighted > 0.0d0 ) THEN
      HSPF = NetHeatingCapWeighted * DemandDeforstCredit / TotHeatingElecPowerWeighted
  ELSE
      HSPF = 0.0d0
  ENDIF


  RETURN

END SUBROUTINE MultiSpeedDXHeatingCoilStandardRatings

SUBROUTINE ReportDXCoilRating(CompType,CompName,CompTypeNum, CoolCapVal,SEERValueIP,EERValueSI,EERValueIP, &
                              IEERValueIP,HighHeatingCapVal, LowHeatingCapVal, HSPFValueIP, RegionNum)

    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Bereket Nigusse, Chandan Sharma
    !       DATE WRITTEN   February 2010
    !       MODIFIED       May 2010 (Added EER and IEER entries)
    !                      March 2012 (Added HSPF and High/Low Heating Capacity entries)
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! This subroutine writes the standard rating (net) cooling capacity, SEER, EER and IEER values to
    ! the "eio" and tabular output files for Single Speed compressor DX Cooling Coils.

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY : OutputFileInits
    USE General, ONLY: RoundSigDigits
    USE OutputReportPredefined
    USE DataHVACGlobals, ONLY: CoilDX_CoolingSingleSpeed, CoilDX_HeatingEmpirical, CoilDX_MultiSpeedCooling, &
                               CoilDX_MultiSpeedHeating

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: CompType     ! Type of component
    CHARACTER(len=*), INTENT(IN) :: CompName     ! Name of component
    INTEGER, INTENT(IN)          :: CompTypeNum  ! TypeNum of component
    REAL(r64), INTENT(IN)        :: SEERValueIP  ! SEER value in IP units {Btu/W-h}
    REAL(r64), INTENT(IN)        :: CoolCapVal   ! Standard total (net) cooling capacity for AHRI Std. 210/240 {W}
                                                 !   or ANSI/AHRI Std. 340/360 {W}
    REAL(r64), INTENT(IN)        :: EERValueSI   ! EER value in SI units {W/W}
    REAL(r64), INTENT(IN)        :: EERValueIP   ! EER value in IP units {Btu/W-h}
    REAL(r64), INTENT(IN)        :: IEERValueIP  ! IEER value in IP units {Btu/W-h}

    REAL(r64), INTENT(IN)        :: HighHeatingCapVal   ! High Temperature Heating Standard (Net) Rating Capacity
                                                        ! for AHRI Std. 210/240 {W}
    REAL(r64), INTENT(IN)        :: LowHeatingCapVal    ! Low Temperature Heating Standard (Net) Rating Capacity
                                                        ! for AHRI Std. 210/240 {W}
    REAL(r64), INTENT(IN)        :: HSPFValueIP  ! IEER value in IP units {Btu/W-h}
    INTEGER, INTENT(IN)          :: RegionNum    ! Region Number for which HSPF is calculated

    ! SUBROUTINE PARAMETER DEFINITIONS:


    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    LOGICAL, SAVE :: MyCoolOneTimeFlag = .TRUE.
    LOGICAL, SAVE :: MyHeatOneTimeFlag = .TRUE.

    SELECT CASE(CompTypeNum)

      CASE (CoilDX_CoolingSingleSpeed)
        IF (MyCoolOneTimeFlag) THEN
            WRITE(OutputFileInits, 990)
            MyCoolOneTimeFlag = .FALSE.
        END IF

        WRITE (OutputFileInits, 991) TRIM(CompType), TRIM(CompName),TRIM(RoundSigDigits(CoolCapVal,1)),  &
                                     TRIM(RoundSigDigits(EERValueSI,2)),TRIM(RoundSigDigits(EERValueIP,2)), &
                                     TRIM(RoundSigDigits(SEERValueIP,2)), TRIM(RoundSigDigits(IEERValueIP,2))

        CALL PreDefTableEntry(pdchDXCoolCoilType,    TRIM(CompName),TRIM(CompType))
        CALL PreDefTableEntry(pdchDXCoolCoilNetCapSI,TRIM(CompName),CoolCapVal,1)
        CALL PreDefTableEntry(pdchDXCoolCoilCOP,     TRIM(CompName),TRIM(RoundSigDigits(EERValueSI,2)))
        CALL PreDefTableEntry(pdchDXCoolCoilEERIP,   TRIM(CompName),TRIM(RoundSigDigits(EERValueIP,2)))
        CALL PreDefTableEntry(pdchDXCoolCoilSEERIP,  TRIM(CompName),TRIM(RoundSigDigits(SEERValueIP,2)))
        CALL PreDefTableEntry(pdchDXCoolCoilIEERIP,  TRIM(CompName),TRIM(RoundSigDigits(IEERValueIP,2)))
        CALL addFootNoteSubTable(pdstDXCoolCoil,   'ANSI/AHRI ratings account for supply air fan heat and electric power.')

        990 FORMAT('! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, ',    &
            'Standard Rating (Net) Cooling Capacity {W}, ', 'Standard Rated Net COP {W/W}, ', &
            'EER {Btu/W-h}, ', 'SEER {Btu/W-h}, ', 'IEER {Btu/W-h}')
        991 FORMAT(' DX Cooling Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)

      CASE (CoilDX_HeatingEmpirical, CoilDX_MultiSpeedHeating)
        IF (MyHeatOneTimeFlag) THEN
            WRITE(OutputFileInits, 992)
            MyHeatOneTimeFlag = .FALSE.
        END IF

        WRITE (OutputFileInits, 993) TRIM(CompType), TRIM(CompName), TRIM(RoundSigDigits(HighHeatingCapVal,1)),  &
                                     TRIM(RoundSigDigits(LowHeatingCapVal,1)), &
                                     TRIM(RoundSigDigits(HSPFValueIP,2)), &
                                     TRIM(RoundSigDigits(RegionNum))

        CALL PreDefTableEntry(pdchDXHeatCoilType,TRIM(CompName),TRIM(CompType))
        CALL PreDefTableEntry(pdchDXHeatCoilHighCap,TRIM(CompName),HighHeatingCapVal,1)
        CALL PreDefTableEntry(pdchDXHeatCoilLowCap,TRIM(CompName),LowHeatingCapVal,1)
        CALL PreDefTableEntry(pdchDXHeatCoilHSPFIP,TRIM(CompName),TRIM(RoundSigDigits(HSPFValueIP,2)))
        CALL PreDefTableEntry(pdchDXHeatCoilRegionNum,TRIM(CompName),TRIM(RoundSigDigits(RegionNum)))
        CALL addFootNoteSubTable(pdstDXHeatCoil,   'ANSI/AHRI ratings account for supply air fan heat and electric power.')

        992 FORMAT('! <DX Heating Coil Standard Rating Information>, Component Type, Component Name, ',    &
            'High Temperature Heating (net) Rating Capacity {W}, ', 'Low Temperature Heating (net) Rating Capacity {W}, ', &
            'HSPF {Btu/W-h}, ', 'Region Number')
        993 FORMAT(' DX Heating Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A)

      CASE (CoilDX_MultiSpeedCooling)
        IF (MyCoolOneTimeFlag) THEN
            WRITE(OutputFileInits, 994)
            MyCoolOneTimeFlag = .FALSE.
        END IF

        WRITE (OutputFileInits, 995) TRIM(CompType), TRIM(CompName),TRIM(RoundSigDigits(CoolCapVal,1)),  &
                                     ' ', ' ', TRIM(RoundSigDigits(SEERValueIP,2)), ' '

        CALL PreDefTableEntry(pdchDXCoolCoilType,TRIM(CompName),TRIM(CompType))
        CALL PreDefTableEntry(pdchDXCoolCoilNetCapSI,TRIM(CompName),CoolCapVal,1)
        CALL PreDefTableEntry(pdchDXCoolCoilSEERIP,TRIM(CompName),TRIM(RoundSigDigits(SEERValueIP,2)))
        CALL addFootNoteSubTable(pdstDXCoolCoil,   'ANSI/AHRI ratings account for supply air fan heat and electric power.')        

        994 FORMAT('! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, ',    &
            'Standard Rating (Net) Cooling Capacity {W}, ', 'Standard Rated Net COP {W/W}, ', &
            'EER {Btu/W-h}, ', 'SEER {Btu/W-h}, ', 'IEER {Btu/W-h}')
        995 FORMAT(' DX Cooling Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)

      CASE DEFAULT
    END SELECT
    RETURN

END SUBROUTINE ReportDXCoilRating

SUBROUTINE CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilTypeNum, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                              EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D. Shirey/B. Nigusse, FSEC
          !       DATE WRITTEN   May 2010
          !       MODIFIED       Chandan Sharma, March 2012
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Checks the limits of the various curves used in DXCoil and returns .FALSE. if the limits do not include
          ! the standard test condition(s).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue, GetCurveMinMaxValues, GetCurveIndex, GetCurveType, GetCurveName
  USE DataGlobals,    ONLY: DisplayExtraWarnings
  USE DataHVACGlobals, ONLY: CoilDX_CoolingSingleSpeed, CoilDX_HeatingEmpirical, CoilDX_MultiSpeedCooling, &
                             CoilDX_MultiSpeedHeating

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: DXCoilName          ! Name of DX coil for which HSPF is calculated
  CHARACTER(len=*), INTENT(IN)     :: DXCoilType          ! Type of DX coil - heating or cooling
  INTEGER, INTENT(IN)              :: DXCoilTypeNum       ! Integer type of DX coil - heating or cooling
  INTEGER, INTENT(IN)              :: CapFTempCurveIndex  ! Index for the capacity as a function of temperature modifier curve
  INTEGER, INTENT(IN)              :: CapFFlowCurveIndex  ! Index for the capacity as a function of flow fraction modifier curve
  INTEGER, INTENT(IN)              :: EIRFTempCurveIndex  ! Index for the EIR as a function of temperature modifier curve
  INTEGER, INTENT(IN)              :: EIRFFlowCurveIndex  ! Index for the EIR as a function of flow fraction modifier curve
  INTEGER, INTENT(IN)              :: PLFFPLRCurveIndex   ! Index for the EIR vs part-load ratio curve

          ! SUBROUTINE PARAMETER DEFINITIONS:

  CHARACTER(len=*), PARAMETER :: RoutineName='CheckCurveLimitsForStandardRatings: ' ! Include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

 !  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
  REAL(r64) :: CapacityWBTempMin= 0.0d0  ! Capacity modifier Min value (wet bulb temperature), from the Curve:Biquadratic object
  REAL(r64) :: CapacityWBTempMax= 0.0d0  ! Capacity modifier Max value (wet bulb temperature), from the Curve:Biquadratic object
  REAL(r64) :: CapacityDBTempMin= 0.0d0  ! Capacity modifier Min value (dry bulb temperature), from the Curve:Biquadratic object
  REAL(r64) :: CapacityDBTempMax= 0.0d0  ! Capacity modifier Max value (dry bulb temperature), from the Curve:Biquadratic object

!  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
  REAL(r64) :: EIRWBTempMin= 0.0d0  ! EIR modifier Min value (wet bulb temperature), from the Curve:Biquadratic object
  REAL(r64) :: EIRWBTempMax= 0.0d0  ! EIR modifier Max value (wet bulb temperature), from the Curve:Biquadratic object
  REAL(r64) :: EIRDBTempMin= 0.0d0  ! EIR modifier Min value (dry bulb temperature), from the Curve:Biquadratic object
  REAL(r64) :: EIRDBTempMax= 0.0d0  ! EIR modifier Max value (dry bulb temperature), from the Curve:Biquadratic object

!  Minimum and Maximum independent variable limits from Part Load Fraction Correlation Curve
  REAL(r64) :: PLFFPLRMin = 0.0d0    ! Maximum value for Part Load Ratio, from the corresponding curve object
  REAL(r64) :: PLFFPLRMax = 0.0d0    ! Minimum value for Part Load Ratio, from the corresponding curve object

!  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Flow Fraction Curve
  REAL(r64) :: CapacityFlowRatioMin = 0.0d0 ! Minimum value for flow fraction, from the corresponding curve object
  REAL(r64) :: CapacityFlowRatioMax = 0.0d0 ! Maximum value for flow fraction, from the corresponding curve object

!  Minimum and Maximum independent variable limits from Energy Input Ratio Function of Flow Fraction Curve
  REAL(r64) :: EIRFlowRatioMin = 0.0d0 ! Minimum value for flow fraction, from the corresponding curve object
  REAL(r64) :: EIRFlowRatioMax = 0.0d0 ! Maximum value for flow fraction, from the corresponding curve object

 !  Minimum and Maximum independent variable limits from Total Cooling Capacity Function of Temperature Curve
  REAL(r64) :: HeatingCapODBTempMin= 0.0d0  ! Capacity modifier Min value (outdoor dry bulb temperature)
  REAL(r64) :: HeatingCapODBTempMax= 0.0d0  ! Capacity modifier Max value (outdoor dry bulb temperature)
  REAL(r64) :: HeatingCapIDBTempMin= 0.0d0  ! Capacity modifier Min value (indoor dry bulb temperature)
  REAL(r64) :: HeatingCapIDBTempMax= 0.0d0  ! Capacity modifier Max value (indoor dry bulb temperature)

!  Minimum and Maximum independent variable limits from Energy Input Ratio (EIR) Function of Temperature Curve
  REAL(r64) :: HeatingEIRODBTempMin= 0.0d0  ! EIR modifier Min value (outdoor dry bulb temperature)
  REAL(r64) :: HeatingEIRODBTempMax= 0.0d0  ! EIR modifier Max value (outdoor dry bulb temperature)
  REAL(r64) :: HeatingEIRIDBTempMin= 0.0d0  ! EIR modifier Min value (indoor dry bulb temperature)
  REAL(r64) :: HeatingEIRIDBTempMax= 0.0d0  ! EIR modifier Max value (indoor dry bulb temperature)

  LOGICAL :: CapCurveOATLimitsExceeded = .FALSE. ! Logical for capacity curve OD temp. limits being exceeded (low and High)
  LOGICAL :: CapCurveHighOATLimitsExceeded = .FALSE. ! Logical for capacity curve temperature limits being exceeded (high temp)
  LOGICAL :: CapCurveFlowLimitsExceeded = .FALSE.    ! Logical for capacity curve flow fraction limits being exceeded
  LOGICAL :: EIRCurveHighOATLimitsExceeded = .FALSE. ! Logical for EIR curve temperature limits being exceeded (high temp)
  LOGICAL :: EIRCurveFlowLimitsExceeded = .FALSE.    ! Logical for EIR curve flow fraction limits being exceeded

  LOGICAL :: CapCurveMidOATLimitsExceeded = .FALSE.  ! Logical for capacity curve temperature limits being exceeded (mid temp)
  LOGICAL :: EIRCurveMidOATLimitsExceeded = .FALSE.  ! Logical for EIR curve temperature limits being exceeded (mid temp)
  LOGICAL :: CapCurveLowOATLimitsExceeded = .FALSE.  ! Logical for capacity curve temperature limits being exceeded (low temp)
  LOGICAL :: EIRCurveLowOATLimitsExceeded = .FALSE.  ! Logical for EIR curve temperature limits being exceeded (Low temp)
  LOGICAL :: PLFfPLRforSEERLimitsExceeded = .FALSE.  ! Logical for PLF function of PLR limits being exceeded

  LOGICAL :: CapCurveIEERLimitsExceeded = .FALSE.  ! Logical for capacity curve temperature limits being exceeded (IEER calcs)
  LOGICAL :: EIRCurveIEERLimitsExceeded = .FALSE.  ! Logical for EIR temperature limits being exceeded (IEER calcs)

  LOGICAL :: HeatingCapCurveHSPFLimitsExceeded = .FALSE.  ! Logical for capacity curve temperature limits being exceeded
                                                          ! (HSPF calcs)
  LOGICAL :: HeatingEIRCurveHSPFLimitsExceeded = .FALSE.  ! Logical for EIR curve temperature limits being exceeded
                                                          ! (HSPF calcs)

  SELECT CASE(DXCoilTypeNum)

    CASE (CoilDX_CoolingSingleSpeed)
      CALL GetCurveMinMaxValues(CapFTempCurveIndex,CapacityWBTempMin,CapacityWBTempMax,CapacityDBTempMin,CapacityDBTempMax)
      CALL GetCurveMinMaxValues(EIRFTempCurveIndex,EIRWBTempMin,EIRWBTempMax,EIRDBTempMin,EIRDBTempMax)
      CALL GetCurveMinMaxValues(CapFFlowCurveIndex,CapacityFlowRatioMin,CapacityFlowRatioMax)
      CALL GetCurveMinMaxValues(EIRFFlowCurveIndex,EIRFlowRatioMin,EIRFlowRatioMax)
      CALL GetCurveMinMaxValues(PLFFPLRCurveIndex,PLFFPLRMin,PLFFPLRMax)

      ! Checking the limits of capacity modifying curve for temperatures
      IF ( CapacityDBTempMax < OutdoorCoilInletAirDrybulbTempRated .OR. &
           CapacityDBTempMin > OutdoorCoilInletAirDrybulbTempRated .OR. &
           CapacityWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           CapacityWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           CapCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for flow fraction
      IF ( CapacityFlowRatioMax < AirMassFlowRatioRated .OR. CapacityFlowRatioMin > AirMassFlowRatioRated ) THEN
           CapCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures
      IF ( EIRDBTempMax < OutdoorCoilInletAirDrybulbTempRated .OR. &
           EIRDBTempMin > OutdoorCoilInletAirDrybulbTempRated .OR. &
           EIRWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           EIRWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           EIRCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for flow fraction
      IF ( EIRFlowRatioMax < AirMassFlowRatioRated .OR. EIRFlowRatioMin > AirMassFlowRatioRated ) THEN
           EIRCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for temperatures (SEER calculation)
      IF ( CapacityDBTempMax < OutdoorCoilInletAirDrybulbTempTestB2 .OR. &
           CapacityDBTempMin > OutdoorCoilInletAirDrybulbTempTestB2 .OR. &
           CapacityWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           CapacityWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           CapCurveMidOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures (SEER calculation)
      IF ( EIRDBTempMax < OutdoorCoilInletAirDrybulbTempTestB2 .OR. &
           EIRDBTempMin > OutdoorCoilInletAirDrybulbTempTestB2 .OR. &
           EIRWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           EIRWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           EIRCurveMidOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of Part Load Fraction for PLR (SEER calculation)
      IF (PLFFPLRMax < PLRforSEER .OR. PLFFPLRMin > PLRforSEER )THEN
          PLFfPLRforSEERLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
      IF ( CapacityDBTempMax < OutdoorCoilInletAirDrybulbTempRated .OR. &
           CapacityDBTempMin > OADBTempLowReducedCapacityTest .OR.      &
           CapacityWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           CapacityWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           CapCurveIEERLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures (IEER high and low test conditions)
      IF ( EIRDBTempMax < OutdoorCoilInletAirDrybulbTempRated .OR.  &
           EIRDBTempMin > OADBTempLowReducedCapacityTest .OR.       &
           EIRWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR.  &
           EIRWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           EIRCurveIEERLimitsExceeded = .TRUE.
      END IF

      IF ( CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveHighOATLimitsExceeded .OR.  &
           EIRCurveFlowLimitsExceeded .OR. CapCurveMidOATLimitsExceeded .OR. EIRCurveMidOATLimitsExceeded   .OR.  &
           PLFfPLRforSEERLimitsExceeded .OR. CapCurveIEERLimitsExceeded .OR. EIRCurveIEERLimitsExceeded)     THEN

           CALL ShowWarningError('The Standard Ratings is calculated for '//TRIM(DXCoilType)//' = '// &
                                 TRIM(DXCoilName)//' but not at the AHRI test condition due to curve out of bound.')
           CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.'// &
                                  ' Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance.')

           IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError(RoutineName//'The max and/or min limits specified in the corresponding curve objects')
            CALL ShowContinueError(' do not include the AHRI test conditions required to calculate one or more of'// &
                                   ' the Standard Rating values.')
           END IF

           ! For Standard Rating Cooling Capacity:
           IF (CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                              ' Standard Rating Cooling Capacity calculated is not at the AHRI test condition.')
                   IF (CapCurveHighOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Temperature Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '    &
                            //TRIM(GetCurveName(CapFTempCurveIndex)))
                   END IF
                   IF (CapCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(CapFFlowCurveIndex)))
                   END IF
               END IF
           END IF

           ! For EER:
           IF (CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveHighOATLimitsExceeded .OR.   &
                EIRCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                          ' Energy Efficiency Ratio (EER) calculated is not at the AHRI test condition.')
                   IF (CapCurveHighOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Temperature Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '    &
                            //TRIM(GetCurveName(CapFTempCurveIndex)))
                   ENDIF
                   IF (CapCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(CapFFlowCurveIndex)))
                   END IF
                   IF (EIRCurveHighOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Temperature Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex))     &
                            //', Curve Name = '//TRIM(GetCurveName(EIRFTempCurveIndex)))
                   END IF
                   IF (EIRCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFFlowCurveIndex))//', Curve Name = '   &
                            //TRIM(GetCurveName(EIRFFlowCurveIndex)))
                   END IF
               END IF
           END IF

           ! For SEER:
           IF ( CapCurveMidOATLimitsExceeded .OR. EIRCurveMidOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded  &
            .OR. EIRCurveFlowLimitsExceeded  .OR. PLFfPLRforSEERLimitsExceeded ) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                               ' Seasonal Energy Efficiency Ratio (SEER) calculated is not at the AHRI test condition.')
                   IF (CapCurveMidOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Temperature Curve, '  &
                              //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '  &
                              //TRIM(GetCurveName(CapFTempCurveIndex)))
                   END IF
                   IF (CapCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(CapFFlowCurveIndex)))
                   END IF
                   IF (EIRCurveMidOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Temperature Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex))     &
                            //', Curve Name = '//TRIM(GetCurveName(EIRFTempCurveIndex)))
                   END IF
                   IF (EIRCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFFlowCurveIndex))      &
                            //', Curve Name = '//TRIM(GetCurveName(EIRFFlowCurveIndex)))
                   END IF
                   IF (PLFfPLRforSEERLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Part Load Fraction Correlation Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(PLFFPLRCurveIndex))             &
                            //', Curve Name = '//TRIM(GetCurveName(PLFFPLRCurveIndex)))
                   END IF
               END IF
           END IF

           ! For IEER:
           IF ( CapCurveIEERLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveIEERLimitsExceeded .OR.  &
                EIRCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '//&
                              ' Integrated Energy Efficiency Ratio (IEER) calculated is not at the AHRI test condition.')
                   IF (CapCurveIEERLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Temperature Curve '  &
                       //', Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))      &
                       //', Curve Name = '//TRIM(GetCurveName(CapFTempCurveIndex)))
                   END IF
                   IF (CapCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(CapFFlowCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(CapFFlowCurveIndex)))
                   END IF
                   IF (EIRCurveIEERLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in EIR Function of Temperature Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(EIRFTempCurveIndex)))
                   END IF
                   IF (EIRCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFFlowCurveIndex))//', Curve Name = '   &
                            //TRIM(GetCurveName(EIRFFlowCurveIndex)))
                   END IF
               END IF
           END IF

      END IF  ! End of curve error messages
    CASE (CoilDX_HeatingEmpirical)
      SELECT CASE(GetCurveType(CapFTempCurveIndex))

        CASE('QUADRATIC', 'CUBIC')
          CALL GetCurveMinMaxValues(CapFTempCurveIndex,HeatingCapODBTempMin,HeatingCapODBTempMax)

              ! Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
          IF ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test) THEN
               HeatingCapCurveHSPFLimitsExceeded = .TRUE.
          END IF
        CASE('BIQUADRATIC')
          CALL GetCurveMinMaxValues(CapFTempCurveIndex,HeatingCapIDBTempMin,HeatingCapIDBTempMax, &
                                    HeatingCapODBTempMin,HeatingCapODBTempMax)

              ! Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
          IF ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test .OR. &
               HeatingCapIDBTempMax < HeatingIndoorCoilInletAirDBTempRated .OR. &
               HeatingCapIDBTempMin > HeatingIndoorCoilInletAirDBTempRated ) THEN
               HeatingCapCurveHSPFLimitsExceeded = .TRUE.
          END IF
      END SELECT
      SELECT CASE(GetCurveType(EIRFTempCurveIndex))

        CASE('QUADRATIC', 'CUBIC')
          CALL GetCurveMinMaxValues(EIRFTempCurveIndex,HeatingEIRODBTempMin,HeatingEIRODBTempMax)

          ! Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test) THEN
               HeatingEIRCurveHSPFLimitsExceeded = .TRUE.
          END IF
        CASE('BIQUADRATIC')
          CALL GetCurveMinMaxValues(EIRFTempCurveIndex,HeatingEIRIDBTempMin,HeatingEIRIDBTempMax, &
                                    HeatingEIRODBTempMin,HeatingEIRODBTempMax)

          ! Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test .OR. &
               HeatingEIRIDBTempMax < HeatingIndoorCoilInletAirDBTempRated .OR. &
               HeatingEIRIDBTempMin > HeatingIndoorCoilInletAirDBTempRated ) THEN
               HeatingEIRCurveHSPFLimitsExceeded = .TRUE.
          END IF
      END SELECT
      IF ( HeatingCapCurveHSPFLimitsExceeded .OR. HeatingEIRCurveHSPFLimitsExceeded) THEN
        CALL ShowWarningError('The Standard Ratings is calculated for '//TRIM(DXCoilType)//' = '// &
                               TRIM(DXCoilName)//' but not at the AHRI test condition due to curve out of bound.')
        CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.'// &
                               ' Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance.')
        IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError(RoutineName//'The max and/or min limits specified in the corresponding curve objects')
            CALL ShowContinueError(' do not include the AHRI test conditions required to calculate one or more of'// &
                                   ' the Standard Rating values.')
        END IF
        IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                  ' Heating Seasonal Performance Factor calculated is not at the AHRI test condition.')
            CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.')
            IF (HeatingCapCurveHSPFLimitsExceeded) THEN
                CALL ShowContinueError(' Check limits in Total Heating Capacity Function of Temperature Curve, '  &
                    //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '    &
                    //TRIM(GetCurveName(CapFTempCurveIndex)))
            END IF
            IF (HeatingEIRCurveHSPFLimitsExceeded) THEN
                CALL ShowContinueError(' Check limits in EIR Function of Temperature Curve, ' &
                    //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex))//', Curve Name = '     &
                    //TRIM(GetCurveName(EIRFTempCurveIndex)))
            END IF
        ENDIF
      ENDIF

!   MultiSpeed DX Coil Net Cooling Capacity and SEER:
    CASE (CoilDX_MultiSpeedCooling)
      CALL GetCurveMinMaxValues(CapFTempCurveIndex,CapacityWBTempMin,CapacityWBTempMax,CapacityDBTempMin,CapacityDBTempMax)
      CALL GetCurveMinMaxValues(EIRFTempCurveIndex,EIRWBTempMin,EIRWBTempMax,EIRDBTempMin,EIRDBTempMax)
      CALL GetCurveMinMaxValues(CapFFlowCurveIndex,CapacityFlowRatioMin,CapacityFlowRatioMax)
      CALL GetCurveMinMaxValues(EIRFFlowCurveIndex,EIRFlowRatioMin,EIRFlowRatioMax)
      !CALL GetCurveMinMaxValues(PLFFPLRCurveIndex,PLFFPLRMin,PLFFPLRMax)

      ! Checking the limits of capacity modifying curve for temperatures
      IF ( CapacityDBTempMax < OutdoorCoilInletAirDrybulbTempRated .OR. &
           CapacityDBTempMin > OutdoorCoilInletAirDrybulbTempRated .OR. &
           CapacityWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           CapacityWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           CapCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for flow fraction
      IF ( CapacityFlowRatioMax < AirMassFlowRatioRated .OR. CapacityFlowRatioMin > AirMassFlowRatioRated ) THEN
           CapCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures
      IF ( EIRDBTempMax < OutdoorCoilInletAirDrybulbTempRated .OR. &
           EIRDBTempMin > OutdoorCoilInletAirDrybulbTempRated .OR. &
           EIRWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           EIRWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           EIRCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for flow fraction
      IF ( EIRFlowRatioMax < AirMassFlowRatioRated .OR. EIRFlowRatioMin > AirMassFlowRatioRated ) THEN
           EIRCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for temperatures (SEER calculation)
      IF ( CapacityDBTempMax < OutdoorCoilInletAirDrybulbTempTestF1 .OR. &
           CapacityDBTempMin > OutdoorCoilInletAirDrybulbTempTestF1 .OR. &
           CapacityWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           CapacityWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           CapCurveLowOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures (SEER calculation)
      IF ( EIRDBTempMax < OutdoorCoilInletAirDrybulbTempTestF1 .OR. &
           EIRDBTempMin > OutdoorCoilInletAirDrybulbTempTestF1 .OR. &
           EIRWBTempMax < CoolingCoilInletAirWetbulbTempRated .OR. &
           EIRWBTempMin > CoolingCoilInletAirWetbulbTempRated ) THEN
           EIRCurveLowOATLimitsExceeded = .TRUE.
      END IF

      IF ( CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveHighOATLimitsExceeded .OR.  &
           EIRCurveFlowLimitsExceeded .OR. CapCurveLowOATLimitsExceeded .OR. EIRCurveLowOATLimitsExceeded ) THEN

           CALL ShowWarningError('The Standard Ratings is calculated for '//TRIM(DXCoilType)//' = '// &
                                 TRIM(DXCoilName)//' but not at the AHRI test condition due to curve out of bound.')
           CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.'// &
                                  ' Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance.')

           IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError(RoutineName//'The max and/or min limits specified in the corresponding curve objects')
            CALL ShowContinueError(' do not include the AHRI test conditions required to calculate one or more of'// &
                                   ' the Standard Rating values.')
           END IF

           ! For Standard Rating Cooling Capacity:
           IF (CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                            ' The Standard Rating Cooling Capacity calculated is not at the AHRI test condition.')
                   IF (CapCurveHighOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Temperature Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '    &
                            //TRIM(GetCurveName(CapFTempCurveIndex)))
                   END IF
                   IF (CapCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(CapFFlowCurveIndex)))
                   END IF
               END IF
           END IF

           ! For MultiSpeed DX Coil SEER:

           IF ( CapCurveLowOATLimitsExceeded .OR. EIRCurveLowOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded  &
                .OR. EIRCurveFlowLimitsExceeded ) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                             ' The Seasonal Energy Efficiency Ratio (SEER) calculated is not at the AHRI test condition.')
                   IF (CapCurveLowOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Temperature Curve, '  &
                              //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '  &
                              //TRIM(GetCurveName(CapFTempCurveIndex)))
                   END IF
                   IF (CapCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Total Cooling Capacity Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '     &
                            //TRIM(GetCurveName(CapFFlowCurveIndex)))
                   END IF
                   IF (EIRCurveLowOATLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Temperature Curve, '  &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex))     &
                            //', Curve Name = '//TRIM(GetCurveName(EIRFTempCurveIndex)))
                   END IF
                   IF (EIRCurveFlowLimitsExceeded) THEN
                       CALL ShowContinueError(' Check limits in Energy Input Ratio Function of Flow Fraction Curve, ' &
                            //'Curve Type = '//TRIM(GetCurveType(EIRFFlowCurveIndex))      &
                            //', Curve Name = '//TRIM(GetCurveName(EIRFFlowCurveIndex)))
                   END IF
               END IF
           END IF

      END IF  ! End of curve error messages

    CASE (CoilDX_MultiSpeedHeating)

      SELECT CASE(GetCurveType(CapFTempCurveIndex))

        CASE('QUADRATIC', 'CUBIC')
          CALL GetCurveMinMaxValues(CapFTempCurveIndex,HeatingCapODBTempMin,HeatingCapODBTempMax)

          IF ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test) THEN
               CapCurveOATLimitsExceeded = .TRUE.
          ENDIF
              ! Checking the limits of capacity modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test .OR. &
               HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) THEN
               HeatingCapCurveHSPFLimitsExceeded = .TRUE.
          END IF

        CASE('BIQUADRATIC')
          CALL GetCurveMinMaxValues(CapFTempCurveIndex,HeatingCapIDBTempMin,HeatingCapIDBTempMax, &
                                    HeatingCapODBTempMin,HeatingCapODBTempMax)

              ! Checking the limits of capacity modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test .OR. &
               HeatingCapIDBTempMax < HeatingIndoorCoilInletAirDBTempRated .OR. &
               HeatingCapIDBTempMin > HeatingIndoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) THEN
               HeatingCapCurveHSPFLimitsExceeded = .TRUE.
          END IF
        END SELECT

      SELECT CASE(GetCurveType(EIRFTempCurveIndex))

        CASE('QUADRATIC', 'CUBIC')
          CALL GetCurveMinMaxValues(EIRFTempCurveIndex,HeatingEIRODBTempMin,HeatingEIRODBTempMax)
          ! Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test .OR. &
               HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) THEN
               HeatingEIRCurveHSPFLimitsExceeded = .TRUE.
          END IF
        CASE('BIQUADRATIC')
          CALL GetCurveMinMaxValues(EIRFTempCurveIndex,HeatingEIRIDBTempMin,HeatingEIRIDBTempMax, &
                                    HeatingEIRODBTempMin,HeatingEIRODBTempMax)

          ! Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingEIRODBTempMax < HeatingOutdoorCoilInletAirDBTempRated .OR. &
               HeatingEIRODBTempMin > HeatingOutdoorCoilInletAirDBTempH3Test .OR. &
               HeatingEIRIDBTempMax < HeatingIndoorCoilInletAirDBTempRated .OR. &
               HeatingEIRIDBTempMin > HeatingIndoorCoilInletAirDBTempRated .OR. &
               HeatingCapODBTempMax < HeatingOutdoorCoilInletAirDBTempH0Test) THEN
               HeatingEIRCurveHSPFLimitsExceeded = .TRUE.
          END IF
      END SELECT
      IF ( HeatingCapCurveHSPFLimitsExceeded .OR. HeatingEIRCurveHSPFLimitsExceeded .OR.  &
           CapCurveOATLimitsExceeded) THEN

           CALL ShowWarningError('The Standard Ratings is calculated for '//TRIM(DXCoilType)//' = '// &
                                 TRIM(DXCoilName)//' but not at the AHRI test condition due to curve out of bound.')
           CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.'// &
                                  ' Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance.')

           IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError(RoutineName//'The max and/or min limits specified in the corresponding curve objects')
            CALL ShowContinueError(' do not include the AHRI test conditions required to calculate one or'// &
                                   ' more of the Standard Rating values.')
           END IF
      ENDIF
      IF ( CapCurveOATLimitsExceeded) THEN
        IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                    ' The Net Heating Capacity Calculated is not at the AHRI test condition.')
            CALL ShowContinueError(' Check limits in Total Heating Capacity Function of Temperature Curve, '  &
                    //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '    &
                    //TRIM(GetCurveName(CapFTempCurveIndex)))
        ENDIF
      ENDIF
      IF ( HeatingCapCurveHSPFLimitsExceeded .OR. HeatingEIRCurveHSPFLimitsExceeded) THEN
        IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                    ' The Heating Seasonal Performance Factor calculated is not at the AHRI test condition.')
            IF (HeatingCapCurveHSPFLimitsExceeded) THEN
                CALL ShowContinueError(' Check limits in Total Heating Capacity Function of Temperature Curve, '  &
                    //'Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))//', Curve Name = '    &
                    //TRIM(GetCurveName(CapFTempCurveIndex)))
            END IF
            IF (HeatingEIRCurveHSPFLimitsExceeded) THEN
                CALL ShowContinueError(' Check limits in EIR Function of Temperature Curve, ' &
                    //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex))//', Curve Name = '     &
                    //TRIM(GetCurveName(EIRFTempCurveIndex)))
            END IF
        ENDIF
      ENDIF
    CASE DEFAULT
  END SELECT
  RETURN

END SUBROUTINE CheckCurveLimitsForStandardRatings

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


END MODULE StandardRatings


