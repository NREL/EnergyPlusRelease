MODULE StandardRatings

  ! MODULE INFORMATION:
  !       AUTHOR         Chandan Sharma
  !       DATE WRITTEN   February 2012
  !       MODIFIED       na
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

PUBLIC  CalcChillerIPLV
PRIVATE ReportChillerIPLV
PRIVATE ReformEIRChillerCondInletTempResidual
PRIVATE CheckCurveLimitsForIPLV

PUBLIC  CalcDXCoilStandardRating
PRIVATE CalculateDXHeatingCoilCapacityAndEIR
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
  USE General,         ONLY: SolveRegulaFalsi
  USE DataPlant,  ONLY: PlantLoop,TypeOf_Chiller_ElectricEIR, TypeOf_Chiller_ElectricReformEIR
  USE CurveManager, ONLY: CurveValue

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
  LOGICAL   :: CalcIPLV                  = .FALSE.  ! FALSE if the temp. and flow modifier curves are not inclusive of
                                                    ! AHRI test conditions

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

  CalcIPLV                       = .FALSE.

  CALL CheckCurveLimitsForIPLV(ChillerName, ChillerType, CondenserType, CapFTempCurveIndex, EIRFTempCurveIndex, CalcIPLV)

! IPLV calculations:
  IF ( CalcIPLV ) THEN

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
    END DO

    ! Writes the IPLV value to the EIO file and standard tabular output tables
    CALL ReportChillerIPLV( ChillerName, ChillerType, IPLV,IPLV * ConvFromSIToIP)
  ENDIF

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
    991 FORMAT(' Chiller Standard Rating Information, ',A,', 'A,', ',A,', ',A)

    RETURN

END SUBROUTINE ReportChillerIPLV

SUBROUTINE CheckCurveLimitsForIPLV(ChillerName, ChillerType, CondenserType, CapFTempCurveIndex, EIRFTempCurveIndex, CalcIPLV)

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
  LOGICAL, INTENT(INOUT)       :: CalcIPLV            ! Logical flag set to .FALSE. if IPLV should not be calculated

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
                                ' Integrated Part Load Value (IPLV) cannot be calculated.')
        CASE (TypeOf_Chiller_ElectricReformEIR)

          CALL ShowWarningError('Chiller:Electric:ReformulatedEIR = '// &
                                TRIM(ChillerName)//': '//&
                                ' Integrated Part Load Value (IPLV) cannot be calculated.')
        END SELECT
        IF (CapCurveIPLVLimitsExceeded) THEN
            CALL ShowContinueError(' Check limits in Cooling Capacity Function of Temperature Curve '  &
            //', Curve Type = '//TRIM(GetCurveType(CapFTempCurveIndex))      &
            //', Curve Name = '//TRIM(GetCurveName(CapFTempCurveIndex)))
        END IF
        IF (EIRCurveIPLVLimitsExceeded) THEN
            CALL ShowContinueError(' Check limits in EIR Function of Temperature Curve, ' &
                //'Curve Type = '//TRIM(GetCurveType(EIRFTempCurveIndex)) &
                //', Curve Name = '//TRIM(GetCurveName(EIRFTempCurveIndex)))
        END IF
      END IF
      CalcIPLV = .FALSE.
  ELSE
      CalcIPLV = .TRUE.
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
          !                      May 2010  Added EER and IEER Calculation
          !                      March 2012  Added HSPF Calculation
          !                      August 2012 Added SEER for Multi-speed HP

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
  USE CurveManager,    ONLY: CurveValue, GetCurveMinMaxValues
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

  INTEGER, OPTIONAL, INTENT(IN)    :: RegionNum            ! Region number for calculating HSPF of single speed DX heating coil
  INTEGER, OPTIONAL, INTENT(IN)    :: DefrostControl       ! defrost control; 1=timed, 2=on-demand
  REAL(r64), OPTIONAL, INTENT(IN)  :: MinOATCompressor     ! Minimum OAT for heat pump compressor operation [C]
  REAL(r64), OPTIONAL, INTENT(IN)  :: OATempCompressorOn   ! The outdoor tempearture when the compressor is automatically turned
                                                           ! back on, if applicable, following automatic shut off. This field is
                                                           ! used only for HSPF calculation. [C]
  LOGICAL, OPTIONAL, INTENT(IN)    :: OATempCompressorOnOffBlank  ! Flag used to determine low temperature cut out factor

          ! SUBROUTINE PARAMETER DEFINITIONS:
  ! AHRI Standard 210/240-2008 Performance Test Conditions for Unitary Air-to-Air Air-Conditioning and Heat Pump Equipment
  REAL(r64), PARAMETER ::    CoolingCoilInletAirWetbulbTempRated = 19.44d0  ! 19.44C (67F)  Tests A and B
  REAL(r64), PARAMETER ::    OutdoorUnitInletAirDrybulbTemp = 27.78d0       ! 27.78C (82F)  Test B (for SEER)
  REAL(r64), PARAMETER ::    OutdoorUnitInletAirDrybulbTempRated = 35.0d0   ! 35.00C (95F)  Test A (rated capacity)
  REAL(r64), PARAMETER ::    AirMassFlowRatioRated = 1.0d0   ! AHRI test is at the design flow rate
                                                             ! and hence AirMassFlowRatio is 1.0
  REAL(r64), PARAMETER ::    PLRforSEER = 0.5d0              ! Part-load ratio for SEER calculation (single speed DX cooling coils)
  INTEGER, PARAMETER :: NumOfReducedCap = 4  ! Number of reduced capacity test conditions (100%,75%,50%,and 25%)

  ! Defrost control  (heat pump only)
  INTEGER, PARAMETER :: Timed            = 1 ! defrost cycle is timed
  INTEGER, PARAMETER :: OnDemand         = 2 ! defrost cycle occurs only when required

  INTEGER,   PARAMETER ::    TotalNumOfStandardDHRs = 16     ! Total number of standard design heating requirements
  INTEGER,   PARAMETER, DIMENSION(6) :: TotalNumOfTemperatureBins = (/9, 10, 13, 15, 18, 9/) ! Total number of temperature
                                                                                             ! bins for a region
  REAL(r64),   PARAMETER, DIMENSION(16):: StandardDesignHeatingRequirement = (/1465.36D0, 2930.71D0, 4396.07D0, 5861.42D0, &
                                                                               7326.78D0, 8792.14D0, 10257.49D0, 11722.85D0, &
                                                                               14653.56D0, 17584.27D0, 20514.98D0, 23445.70D0, &
                                                                               26376.41D0, 29307.12D0, 32237.83D0, 38099.26D0/)
                                                                               ! Standardized DHRs from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(4) :: ReducedPLR = (/1.0D0, 0.75d0,0.50d0,0.25d0/)  ! Reduced Capacity part-load conditions
  REAL(r64), PARAMETER, DIMENSION(4) :: IEERWeightingFactor = (/0.020D0, 0.617D0, 0.238D0, 0.125D0/) ! EER Weighting factors (IEER)

  REAL(r64), PARAMETER :: ConvFromSIToIP = 3.412141633D0 ! Conversion from SI to IP [3.412 Btu/hr-W]
  REAL(r64), PARAMETER :: DefaultFanPowerPerEvapAirFlowRate = 773.3D0 ! 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
                                                      ! specifies a nominal/default fan electric power consumption per rated air
                                                      ! volume flow rate to account for indoor fan electric power consumption
                                                      ! when the standard tests are conducted on units that do not have an
                                                      ! indoor air circulting fan. Used if user doesn't enter a specific value.

  REAL(r64), PARAMETER :: CorrectionFactor = 0.77D0   ! A correction factor which tends to improve the agreement between
                                                      ! calculated and measured building loads, dimensionless.

  REAL(r64), PARAMETER :: CyclicDegradationCoeff = 0.25D0

  REAL(r64), PARAMETER, DIMENSION(6)  ::OutdoorDesignTemperature = (/2.78D0, -2.78D0, -8.33D0, -15.0D0, -23.33D0, -1.11D0/)
                                   ! Outdoor design temperature for a region from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::OutdoorBinTemperature    = (/16.67D0, 13.89D0, 11.11D0, 8.33D0, 5.56D0, 2.78D0, 0.00D0, &
                                                                    -2.78D0, -5.56D0, -8.33D0, -11.11D0, -13.89D0, -16.67D0, &
                                                                    -19.44D0, -22.22D0, -25.00D0, -27.78D0, -30.56D0 /)
                                   ! Outdoor bin temperatures from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::RegionOneFracBinHoursAtOutdoorBinTemp = &
                                                             (/0.291D0, 0.239D0, 0.194D0, 0.129D0, 0.081D0, 0.041D0, &
                                                               0.019D0, 0.005D0, 0.001D0, 0.0D0, 0.0D0, 0.0D0, &
                                                               0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
                                   ! Fractional bin hours for different bin temperatures for region one, from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::RegionTwoFracBinHoursAtOutdoorBinTemp = &
                                                             (/0.215D0, 0.189D0, 0.163D0, 0.143D0, 0.112D0, 0.088D0, &
                                                               0.056D0, 0.024D0, 0.008D0, 0.002D0, 0.0D0, 0.0D0, 0.0D0, &
                                                               0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
                                   ! Fractional bin hours for different bin temperatures for region two, from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::RegionThreeFracBinHoursAtOutdoorBinTemp = &
                                                             (/0.153D0, 0.142D0, 0.138D0, 0.137D0, 0.135D0, 0.118D0, &
                                                               0.092D0, 0.042D0, 0.021D0, 0.009D0, 0.005D0, 0.002D0, &
                                                               0.001D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0/)
                                   ! Fractional bin hours for different bin temperatures for region three, from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::RegionFourFracBinHoursAtOutdoorBinTemp = &
                                                             (/0.132D0, 0.111D0, 0.103D0, 0.093D0, 0.1D0, 0.109D0, &
                                                               0.126D0, 0.087D0, 0.055D0, 0.036D0, 0.026D0, 0.013D0, &
                                                               0.006D0, 0.002D0, 0.001D0, 0.0D0, 0.0D0, 0.0D0/)
                                   ! Fractional bin hours for different bin temperatures for region four, from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::RegionFiveFracBinHoursAtOutdoorBinTemp = &
                                                             (/0.106D0, 0.092D0, 0.086D0, 0.076D0, 0.078D0, 0.087D0, &
                                                               0.102D0, 0.094D0, 0.074D0, 0.055D0, 0.047D0, 0.038D0, &
                                                               0.029D0, 0.018D0, 0.01D0, 0.005D0, 0.002D0, 0.001D0/)
                                   ! Fractional bin hours for different bin temperatures for region five, from ANSI/AHRI 210/240
  REAL(r64), PARAMETER, DIMENSION(18) ::RegionSixFracBinHoursAtOutdoorBinTemp = &
                                                             (/0.113D0, 0.206D0, 0.215D0, 0.204D0, 0.141D0, 0.076D0, &
                                                               0.034D0, 0.008D0, 0.003D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
                                                               0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0/)
                                    ! Fractional bin hours for different bin temperatures for region six, from ANSI/AHRI 210/240

!  Multispeed
          ! SUBROUTINE PARAMETER DEFINITIONS:
  ! AHRI Standard 210/240-2008 Performance Test Conditions for Unitary Air-to-Air Air-Conditioning and Heat Pump Equipment
  REAL(r64), PARAMETER ::    IndoorCoilInletAirWetbulbTempRated   = 19.44d0  ! 19.44C (67F)  Tests A2, B2, B1, and F1
  REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempRated  = 35.0d0   ! 35.00C (95F)  Tests A2, B2, B1, and F1
  REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestA2 = 35.0d0   ! 35.00C (95F)  Test A2 (high speed)
  REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestB2 = 27.78d0  ! 27.78C (82F)  Test B2 (high speed)
  REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestB1 = 27.78d0  ! 27.78C (82F)  Test B1 (Low speed)
  REAL(r64), PARAMETER ::    OutdoorCoilInletAirDrybulbTempTestF1 = 19.44d0  ! 19.44C (67F)  Test B1 (Low speed)
  REAL(r64), PARAMETER ::    SizingFactor = 1.10d0                           ! sizing factor per AHRI Std 210/240-2008

  ! The AHRI standard specifies a nominal/default fan electric power consumption per rated air volume flow rate
  ! to account for indoor fan electric power consumption when the standard tests are conducted on units that do
  ! not have an indoor air circulting fan. Used if user doesn't enter a specific value.

  ! Representative cooling season Outdoor air temperature bin from ANSI/AHRI 210/240-2008
  INTEGER, PARAMETER   :: NumOfOATempBins = 8         ! number of outdoor temperature bins for cooling season
  REAL(r64), PARAMETER, DIMENSION(NumOfOATempBins) :: OutdoorBinTemperatureSEER = &
             (/19.44D0, 22.22D0, 25.00D0, 27.78D0, 30.56D0, 33.33D0, 36.11D0, 38.89D0/)
  ! Fractional bin hours for different bin temperatures for cooling, from ANSI/AHRI 210/240 - 2008
  REAL(r64), PARAMETER, DIMENSION(NumOfOATempBins) :: CoolFracBinHoursAtOutdoorBinTemp = &
             (/0.214D0, 0.231D0, 0.216D0, 0.161D0, 0.104D0, 0.052D0, 0.018D0, 0.004D0/)


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: FanPowerPerEvapAirFlowRate(ns) ! Fan power per air volume flow rate through the evaporator coil [W/(m3/s)]

  ! Intermediate values calculated from the inputs in the idf file
  REAL(r64) :: TotCoolCapTestA2(ns)        ! Total cooling capacity at A2 test condition (High speed)
  REAL(r64) :: TotCoolCapTestB2(ns)        ! Total cooling capacity at B2 test condition (High speed)
  REAL(r64) :: TotCoolCapTestB1(ns)        ! Total cooling capacity at B1 test condition (Low speed)
  REAL(r64) :: TotCoolCapTestF1(ns)        ! Total cooling capacity at F1 test condition (Low speed)
  REAL(r64) :: OutdoorUnitPowerTestA2(ns)  ! Outdoor Unit electric power at A2 test condition (High speed)
  REAL(r64) :: OutdoorUnitPowerTestB2(ns)  ! Outdoor Unit electric power at B2 test condition (High speed)
  REAL(r64) :: OutdoorUnitPowerTestB1(ns)  ! Outdoor Unit electric power at B1 test condition (Low speed)
  REAL(r64) :: OutdoorUnitPowerTestF1(ns)  ! Outdoor Unit electric power at F1 test condition (Low speed)

  REAL(r64) :: CoolingCapacityLS           ! cooling capacity of Mult-speed DX coil at lower speed, [W]
  REAL(r64) :: CoolingCapacityHS           ! cooling capacity of Mult-speed DX coil at higher speed, [W]
  REAL(r64) :: CoolingElecPowerLS          ! outdoor unit electric power input at low speed, [W]
  REAL(r64) :: CoolingElecPowerHS          ! outdoor unit electric power input at high speed, [W]
  REAL(r64) :: CoolingCapacityMax          ! cooling capacity of Mult-speed DX coil at max speed, [W]
  REAL(r64) :: CoolingElecPowerMax         ! outdoor unit electric power input at Max speed, [W]

  REAL(r64) ::  OutdoorUnitInletAirDrybulbTempReduced  ! Outdoor unit entering air dry-bulb temperature at reduced capacity [C]

! Inputs to be read from the idf file
  REAL(r64) :: TotCoolingCapAHRI = 0.0D0   ! Total Cooling Coil capacity (gross) at AHRI test conditions [W]


! Intermediate values calculated from the inputs in the idf file
  REAL(r64) :: TotCapFlowModFac(ns)        ! Total capacity modifier f(actual supply air flow vs rated flow) for each speed [-]
  REAL(r64) :: EIRFlowModFac(ns)           ! EIR modifier f(actual supply air flow vs rated flow) for each speed [-]
  REAL(r64) :: TotCapTempModFac = 0.0D0    ! Total capacity modifier (function of entering wetbulb, outside drybulb) [-]
  REAL(r64) :: EIRTempModFac = 0.0D0       ! EIR modifier (function of entering wetbulb, outside drybulb) [-]

  REAL(r64) :: NetCoolingCapAHRI = 0.0D0   ! Net Cooling Coil capacity at AHRI TestB conditions, accounting for supply fan heat [W]
  REAL(r64) :: TotalElecPower = 0.0D0      ! Net power consumption (Cond Fan+Compressor+Indoor Fan) at AHRI test conditions [W]
  REAL(r64) :: TotalElecPowerRated = 0.0D0 ! Net power consumption (Cond Fan+Compressor+Indoor Fan) at Rated test conditions [W]
  REAL(r64) :: EIR= 0.0D0                  ! Energy Efficiency Ratio at AHRI test conditions for SEER [-]
  REAL(r64) :: PartLoadFactor = 0.0D0      ! Part load factor, accounts for thermal lag at compressor startup [-]
  REAL(r64) :: OATempCompressorOff = 0.0D0
  !
  REAL(r64) :: PartLoadRatio               = 0.0d0 ! compressor cycling ratio between successive speeds, [-]
  REAL(r64) :: PartLoadFraction            = 0.0d0 ! part-load fraction that account for the cyclic degradation, [-]
  REAL(r64) :: NetCoolingCapWeighted       = 0.0d0 ! net tot cooling cap weighted by the fraction of the binned cooling hours [W]
  REAL(r64) :: TotCoolingElecPowerWeighted = 0.0d0 ! net total cooling electric power input weighted by the fraction of the
                                                   ! binned cooling hours
  REAL(r64) :: BuildingCoolingLoad         = 0.0d0 ! Building space cooling load corresponding to an outdoor bin temperature [W]
  REAL(r64) :: NetTotCoolCapBinned         = 0.0d0 ! Net tot cooling cap corresponding to an outdoor bin temperature [W]
  REAL(r64) :: TotCoolElecPowerBinned      = 0.0d0 ! Total cooling cap corresponding to an outdoor bin temperature [W]

! Calculated and reported to the EIO file
  REAL(r64) :: SEER = 0.0D0             ! Seasonal Energy Efficiency Ratio in SI [W/W]
  REAL(r64) :: EER = 0.0D0              ! Energy Efficiency Ratio in SI [W/W]
  REAL(r64) :: IEER = 0.0D0             ! Integerated Energy Efficiency Ratio in SI [W/W]
  REAL(r64) :: HSPF = 0.0d0             ! Heating Seasonal Performance Factor in SI [W/W]
  REAL(r64) :: NetCoolingCapRated(ns)   ! Net Cooling Coil capacity at Rated conditions, accounting for supply fan heat [W]

  REAL(r64) :: EERReduced = 0.0d0       ! EER at reduced capacity test conditions (100%, 75%, 50%, and 25%)
  REAL(r64) :: ElecPowerReducedCap      ! Net power consumption (Cond Fan+Compressor) at reduced test condition [W]
  REAL(r64) :: NetCoolingCapReduced     ! Net Cooling Coil capacity at reduced conditions, accounting for supply fan heat [W]
  REAL(r64) :: LoadFactor               ! Fractional "on" time for last stage at the desired reduced capacity, (dimensionless)
  REAL(r64) :: DegradationCoeff         ! Degradation coeficient, (dimenssionless)
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
  INTEGER   :: RedCapNum              ! Integer counter for reduced capacity
  INTEGER   :: StandardDHRNum         ! Integer counter for standardized DHRs
  LOGICAL   :: CalcCapacity = .TRUE.  ! FALSE if temp. and flow modifier curves limits are not inclusive of AHRI test conditions
  LOGICAL   :: CalcSEER = .TRUE.      ! FALSE if the temp. and flow modifier, and PLF curve limits,
                                      ! are not inclusive of AHRI test conditions
  LOGICAL   :: CalcEER = .TRUE.       ! FALSE if the temp. and flow modifier curves are not inclusive of AHRI test conditions
  LOGICAL   :: CalcIEER = .TRUE.      ! FALSE if the temp. and flow modifier curves are not inclusive of AHRI test conditions
  LOGICAL   :: CalcHSPF = .TRUE.      ! FALSE if the temp. and flow modifier curves are not inclusive of AHRI test conditions

SELECT CASE(DXCoilType_Num)

 CASE (CoilDX_CoolingSingleSpeed)

  CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex(1), CapFFlowCurveIndex(1), &
                                          EIRFTempCurveIndex(1), EIRFFlowCurveIndex(1),   &
                                          PLFFPLRCurveIndex(1), CalcCapacity,CalcSEER, &
                                          CalcEER,CalcIEER,CalcHSPF)

  ! Calculate the Indoor fan electric power consumption.  The electric power consumption is estimated
  ! using either user supplied or AHRI default value for fan power per air volume flow rate
  FanPowerPerEvapAirFlowRate=0.0D0
  TotCapTempModFac = 0.0D0
  EIRFlowModFac = 0.0d0
  NetCoolingCapRated = 0.0d0

  IF( FanPowerPerEvapAirFlowRateFromInput(1) <= 0.0D0) THEN
      FanPowerPerEvapAirFlowRate(1)=DefaultFanPowerPerEvapAirFlowRate
  ELSE
      FanPowerPerEvapAirFlowRate(1)=FanPowerPerEvapAirFlowRateFromInput(1)
  ENDIF

  ! Standard Rating Cooling (net) Capacity calculations:
  IF ( CalcCapacity ) THEN
      TotCapFlowModFac(1) = CurveValue(CapFFlowCurveIndex(1),AirMassFlowRatioRated)
      TotCapTempModFac = CurveValue(CapFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated,    &
                                   OutdoorUnitInletAirDrybulbTempRated)
      NetCoolingCapRated(1) = RatedTotalCapacity(1) * TotCapTempModFac * TotCapFlowModFac(1)      &
                            - FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)
  ELSE
      NetCoolingCapRated(1) = 0.0d0
  ENDIF

  ! SEER calculations:
  IF ( CalcSEER ) THEN
      TotCapFlowModFac(1) = CurveValue(CapFFlowCurveIndex(1),AirMassFlowRatioRated)
      TotCapTempModFac = CurveValue(CapFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated, &
                                   OutdoorUnitInletAirDrybulbTemp)
      TotCoolingCapAHRI = RatedTotalCapacity(1) * TotCapTempModFac * TotCapFlowModFac(1)
      EIRTempModFac = CurveValue(EIRFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated,     &
                                   OutdoorUnitInletAirDrybulbTemp)
      EIRFlowModFac(1) = CurveValue(EIRFFlowCurveIndex(1),AirMassFlowRatioRated)
      IF ( RatedCOP(1) > 0.0D0 ) THEN ! RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
           EIR = EIRTempModFac * EIRFlowModFac(1) / RatedCOP(1)
      ELSE
           EIR = 0.0d0
      ENDIF
      ! Calculate net cooling capacity
      NetCoolingCapAHRI = TotCoolingCapAHRI - FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)
      TotalElecPower = EIR * TotCoolingCapAHRI + FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)

      ! Calculate SEER value from the Energy Efficiency Ratio (EER) at the AHRI test conditions and the part load factor.
      ! First evaluate the Part Load Factor curve at PLR = 0.5 (AHRI Standard 210/240)
      PartLoadFactor = CurveValue(PLFFPLRCurveIndex(1),PLRforSEER)
      IF ( TotalElecPower > 0.0D0 ) THEN
           SEER = ( NetCoolingCapAHRI / TotalElecPower ) * PartLoadFactor
      ELSE
           SEER = 0.0d0
      ENDIF
  ELSE
      SEER =0.0D0
  ENDIF

  ! EER calculations:
  IF ( CalcEER ) THEN
      ! Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
      TotCapFlowModFac(1) = CurveValue(CapFFlowCurveIndex(1),AirMassFlowRatioRated)
      TotCapTempModFac = CurveValue(CapFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated, &
                                    OutdoorUnitInletAirDrybulbTempRated)
      NetCoolingCapRated(1) = RatedTotalCapacity(1) * TotCapTempModFac * TotCapFlowModFac(1)   &
                            - FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)

      ! Calculate Energy Efficiency Ratio (EER) at (19.44C WB and 35.0C DB ), ANSI/AHRI Std. 340/360
      EIRTempModFac = CurveValue(EIRFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated,     &
                      OutdoorUnitInletAirDrybulbTempRated)
      EIRFlowModFac(1) = CurveValue(EIRFFlowCurveIndex(1),AirMassFlowRatioRated)
      IF ( RatedCOP(1) > 0.0D0 ) THEN
           ! RatedCOP <= 0.0 is trapped in GetInput, but keep this as "safety"
             EIR = EIRTempModFac * EIRFlowModFac(1) / RatedCOP(1)
      ELSE
             EIR = 0.0d0
      ENDIF
      TotalElecPowerRated = EIR * (RatedTotalCapacity(1) * TotCapTempModFac * TotCapFlowModFac(1)) &
                          + FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)
      IF ( TotalElecPowerRated > 0.0D0 ) THEN
           EER = NetCoolingCapRated(1) / TotalElecPowerRated
      ELSE
           EER = 0.0d0
      ENDIF
  ELSE
      EER = 0.0D0
  ENDIF

  ! IEER calculations:
  IF ( CalcIEER ) THEN
       IEER =0.0d0
       ! Calculate the net cooling capacity at the rated conditions (19.44C WB and 35.0C DB )
       TotCapFlowModFac = CurveValue(CapFFlowCurveIndex(1),AirMassFlowRatioRated)
       TotCapTempModFac = CurveValue(CapFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated, &
                                    OutdoorUnitInletAirDrybulbTempRated)
       NetCoolingCapRated(1) = RatedTotalCapacity(1) * TotCapTempModFac * TotCapFlowModFac(1)      &
                             - FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)

       EIRFlowModFac(1) = CurveValue(EIRFFlowCurveIndex(1),AirMassFlowRatioRated)

       DO RedCapNum = 1, NumOfReducedCap
          ! get the outdoor air dry bulb temperature for the reduced capacity test conditions
          IF (ReducedPLR(RedCapNum) > 0.444D0 ) THEN
              OutdoorUnitInletAirDrybulbTempReduced = 5.0D0 + 30.0D0 * ReducedPLR(RedCapNum)
          ELSE
              OutdoorUnitInletAirDrybulbTempReduced = 18.3D0
          ENDIF
          TotCapTempModFac = CurveValue(CapFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated, &
                                        OutdoorUnitInletAirDrybulbTempReduced)
          NetCoolingCapReduced = RatedTotalCapacity(1) * TotCapTempModFac * TotCapFlowModFac(1)   &
                               - FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1)
          EIRTempModFac = CurveValue(EIRFTempCurveIndex(1),CoolingCoilInletAirWetbulbTempRated,   &
                                     OutdoorUnitInletAirDrybulbTempReduced)
          IF ( RatedCOP(1) > 0.0D0 ) THEN
               EIR = EIRTempModFac * EIRFlowModFac(1) / RatedCOP(1)
          ELSE
               EIR = 0.0d0
          ENDIF
          LoadFactor = ReducedPLR(RedCapNum) * NetCoolingCapRated(1) / NetCoolingCapReduced
          DegradationCoeff = 1.130D0 - 0.130D0 * LoadFactor
          ElecPowerReducedCap = DegradationCoeff * EIR * (RatedTotalCapacity(1) &
                              * TotCapTempModFac * TotCapFlowModFac(1))
          EERReduced = ( LoadFactor * NetCoolingCapReduced ) / ( LoadFactor*ElecPowerReducedCap + &
                         FanPowerPerEvapAirFlowRate(1) * RatedAirVolFlowRate(1))
          IEER = IEER + IEERWeightingFactor(RedCapNum) * EERReduced
       END DO

  ELSE
       IEER = 0.0D0
  ENDIF
    ! Writes the net rated cooling capacity, SEER, EER and IEER values to the EIO file and standard tabular output tables
  CALL ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(1),      &
                           SEER * ConvFromSIToIP,EER,EER * ConvFromSIToIP,IEER * ConvFromSIToIP, &
                           NetHeatingCapRated, NetHeatingCapH3Test, HSPF * ConvFromSIToIP, RegionNum)

 CASE (CoilDX_HeatingEmpirical)

  CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, CapFTempCurveIndex(1), CapFFlowCurveIndex(1), &
                                          EIRFTempCurveIndex(1), EIRFFlowCurveIndex(1), PLFFPLRCurveIndex(1), CalcCapacity,&
                                          CalcSEER,CalcEER,CalcIEER,CalcHSPF)

  IF ( CalcHSPF ) THEN

    HSPF = 0.0d0

    CALL CalculateDXHeatingCoilCapacityAndEIR (RatedTotalCapacity(1), RatedCOP(1), CapFFlowCurveIndex(1), CapFTempCurveIndex(1), &
                                               EIRFFlowCurveIndex(1), EIRFTempCurveIndex(1), RatedAirVolFlowRate(1), &
                                               FanPowerPerEvapAirFlowRateFromInput(1), NetHeatingCapRated, NetHeatingCapH2Test, &
                                               NetHeatingCapH3Test, ElecPowerRated, ElecPowerH2Test, ElecPowerH3Test)

    IF (RegionNum .EQ. 5) THEN
      DesignHeatingRequirementMin = NetHeatingCapRated
    ELSE
      DesignHeatingRequirementMin = NetHeatingCapRated * 1.8D0* (18.33D0 - OutdoorDesignTemperature(RegionNum)) / 60.0D0
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
      DemandDeforstCredit = 1.0D0   ! Timed defrost control
    ELSE
      DemandDeforstCredit = 1.03D0   ! Demand defrost control
    ENDIF

    IF (TotalElectricalEnergy .NE. 0.0D0) THEN
      HSPF = TotalBuildingLoad * DemandDeforstCredit / TotalElectricalEnergy
    ENDIF

  ELSE
    HSPF = 0.0d0
  ENDIF
    ! Writes the HSPF value to the EIO file and standard tabular output tables
  CALL ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(1),      &
                           SEER * ConvFromSIToIP,EER,EER * ConvFromSIToIP,IEER * ConvFromSIToIP, &
                           NetHeatingCapRated, NetHeatingCapH3Test, HSPF * ConvFromSIToIP, RegionNum)


CASE (CoilDX_MultiSpeedCooling)  ! Coil:Cooling:DX:MultiSpeed,

  NetCoolingCapRated = 0.0d0

  DO spnum = 1, ns
      CALL CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilType_Num, &
                                              CapFTempCurveIndex(spnum), CapFFlowCurveIndex(spnum), &
                                              EIRFTempCurveIndex(spnum), EIRFFlowCurveIndex(spnum), &
                                              PLFFPLRCurveIndex(spnum), &
                                              CalcCapacity,CalcSEER,CalcEER,CalcIEER,CalcHSPF)
      FanPowerPerEvapAirFlowRate(spnum)=0.0d0
      IF( FanPowerPerEvapAirFlowRateFromInput(spnum) <= 0.0d0) THEN
          FanPowerPerEvapAirFlowRate(spnum)=DefaultFanPowerPerEvapAirFlowRate
      ELSE
          FanPowerPerEvapAirFlowRate(spnum)=FanPowerPerEvapAirFlowRateFromInput(spnum)
      ENDIF
  END DO

  ! Calculate the capacity and power for each speed
  DO spnum = 1, ns
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
  END DO
  ! Standard Rating cooling (net) capacity calculations:
  NetCoolingCapRated(ns) = TotCoolCapTestA2(ns)

  ! Calculate the SEER value based on contribution of each outdoor air bin temperature
  DO BinNum = 1, NumOfOATempBins
      BuildingCoolingLoad = (OutdoorBinTemperatureSEER(BinNum) - 18.3d0) / (35.0d0 - 18.3d0) &
                          * (TotCoolCapTestA2(ns) / SizingFactor)
      ! determine the speed number
      CoolingCapacityMax = TotCoolCapTestB2(ns) &
                         + ((TotCoolCapTestA2(ns) - TotCoolCapTestB2(ns)) &
                         / (OutdoorCoilInletAirDrybulbTempTestA2 - OutdoorCoilInletAirDrybulbTempTestB2))  &
                         * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestB2)
      CoolingElecPowerMax = OutdoorUnitPowerTestB2(ns) &
                          + ((OutdoorUnitPowerTestA2(ns) - OutdoorUnitPowerTestB2(ns)) &
                          / (OutdoorCoilInletAirDrybulbTempTestA2 - OutdoorCoilInletAirDrybulbTempTestB2))  &
                          * (OutdoorBinTemperatureSEER(BinNum) - OutdoorCoilInletAirDrybulbTempTestB2)
      SpeedLoop: DO spnum = 1, ns-1

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

  ! Writes the SEER value to the EIO file and standard tabular output tables
  CALL ReportDXCoilRating( DXCoilType, DXCoilName, DXCoilType_Num, NetCoolingCapRated(ns),      &
                           SEER * ConvFromSIToIP, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0)

 CASE DEFAULT
    !... other DX Coil types will follow here


END SELECT

RETURN

END SUBROUTINE CalcDXCoilStandardRating

SUBROUTINE CalculateDXHeatingCoilCapacityAndEIR (RatedTotalCapacity, RatedCOP, CapFFlowCurveIndex, CapFTempCurveIndex, &
                                                 EIRFFlowCurveIndex, EIRFTempCurveIndex, RatedAirVolFlowRate, &
                                                 FanPowerPerEvapAirFlowRateFromInput, NetHeatingCapRated, NetHeatingCapH2Test, &
                                                 NetHeatingCapH3Test, ElecPowerRated, ElecPowerH2Test, ElecPowerH3Test)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Chandan Sharma
    !       DATE WRITTEN   February 2012
    !       MODIFIED       na
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
  REAL(r64), INTENT(OUT) :: NetHeatingCapRated  ! Net Heating Coil capacity at Rated conditions,
                                                ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT) :: NetHeatingCapH2Test ! Net Heating Coil capacity at H2 test conditions
                                                ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT) :: NetHeatingCapH3Test ! Net Heating Coil capacity at H3 test conditions
                                                ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT) :: ElecPowerRated      ! Total system power at Rated conditions
                                                ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT) :: ElecPowerH2Test     ! Total system power at H2 test conditions
                                                ! accounting for supply fan heat [W]
  REAL(r64), INTENT(OUT) :: ElecPowerH3Test     ! Total system power at H3 test conditions
                                                ! accounting for supply fan heat [W]

    ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: AirMassFlowRatioRated = 1.0d0   ! AHRI test is at the design flow rate
  REAL(r64), PARAMETER :: DefaultFanPowerPerEvapAirFlowRate = 773.3D0 ! 365 W/1000 scfm or 773.3 W/(m3/s). The AHRI standard
                                                      ! specifies a nominal/default fan electric power consumption per rated air
                                                      ! volume flow rate to account for indoor fan electric power consumption
                                                      ! when the standard tests are conducted on units that do not have an
                                                      ! indoor air circulting fan. Used if user doesn't enter a specific value.

  REAL(r64), PARAMETER :: HeatingCoilInletAirDrybulbTempRated  = 21.11D0  ! Heating coil entering air dry-bulb temperature in
                                                                          ! degrees C (70F) Test H1, H2 and H3 (Std. AHRI 210/240)
  REAL(r64), PARAMETER :: HeatingCoilOutdoorUnitInletAirDrybulbTempRated = 8.33D0  ! Outdoor air dry-bulb temp in degrees C (47F)
                                                                                   ! Test H1 (Std. AHRI 210/240)
  REAL(r64), PARAMETER :: HeatingCoilOutdoorUnitInletAirDBTempH2Test = 1.67D0      ! Outdoor air dry-bulb temp in degrees C (35F)
                                                                                   ! Test H2 (Std. AHRI 210/240)
  REAL(r64), PARAMETER :: HeatingCoilOutdoorUnitInletAirDBTempH3Test = -8.33D0     ! Outdoor air dry-bulb temp in degrees C (17F)
                                                                                   ! Test H3 (Std. AHRI 210/240)

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

  ! Calculate the Indoor fan electric power consumption.  The electric power consumption is estimated
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
        TotCapTempModFacRated = CurveValue(CapFTempCurveIndex,HeatingCoilOutdoorUnitInletAirDrybulbTempRated)

        CapTempModFacH2Test = CurveValue(CapFTempCurveIndex,HeatingCoilOutdoorUnitInletAirDBTempH2Test)

        CapTempModFacH3Test = CurveValue(CapFTempCurveIndex,HeatingCoilOutdoorUnitInletAirDBTempH3Test)
    CASE('BIQUADRATIC')
        TotCapTempModFacRated = CurveValue(CapFTempCurveIndex,HeatingCoilInletAirDrybulbTempRated, &
                                     HeatingCoilOutdoorUnitInletAirDrybulbTempRated)

        CapTempModFacH2Test = CurveValue(CapFTempCurveIndex,HeatingCoilInletAirdrybulbTempRated, &
                                        HeatingCoilOutdoorUnitInletAirDBTempH2Test)

        CapTempModFacH3Test = CurveValue(CapFTempCurveIndex,HeatingCoilInletAirdrybulbTempRated, &
                                         HeatingCoilOutdoorUnitInletAirDBTempH3Test)

  END SELECT

  SELECT CASE(GetCurveType(EIRFTempCurveIndex))

    CASE('QUADRATIC', 'CUBIC')
        EIRTempModFacRated = CurveValue(EIRFTempCurveIndex,HeatingCoilOutdoorUnitInletAirDrybulbTempRated)

        EIRTempModFacH2Test = CurveValue(EIRFTempCurveIndex,HeatingCoilOutdoorUnitInletAirDBTempH2Test)

        EIRTempModFacH3Test = CurveValue(EIRFTempCurveIndex,HeatingCoilOutdoorUnitInletAirDBTempH3Test)
    CASE('BIQUADRATIC')
        EIRTempModFacRated = CurveValue(EIRFTempCurveIndex,HeatingCoilInletAirdrybulbTempRated, &
                                         HeatingCoilOutdoorUnitInletAirDrybulbTempRated)

        EIRTempModFacH2Test = CurveValue(EIRFTempCurveIndex,HeatingCoilInletAirdrybulbTempRated, &
                                         HeatingCoilOutdoorUnitInletAirDBTempH2Test)

        EIRTempModFacH3Test = CurveValue(EIRFTempCurveIndex,HeatingCoilInletAirdrybulbTempRated, &
                                         HeatingCoilOutdoorUnitInletAirDBTempH3Test)
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

RETURN
END SUBROUTINE CalculateDXHeatingCoilCapacityAndEIR

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
    USE DataHVACGlobals, ONLY: CoilDX_CoolingSingleSpeed, CoilDX_HeatingEmpirical, CoilDX_MultiSpeedCooling

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
        CALL addFootNoteSubTable(pdstDXCoolCoil,   'ANSI/AHRI ratings include supply fan')

        990 FORMAT('! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, ',    &
            'Standard Rating (Net) Cooling Capacity {W}, ', 'Standard Rated Net COP {W/W}, ', &
            'EER {Btu/W-h}, ', 'SEER {Btu/W-h}, ', 'IEER {Btu/W-h}')
        991 FORMAT(' DX Cooling Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)

      CASE (CoilDX_HeatingEmpirical)
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
        CALL addFootNoteSubTable(pdstDXHeatCoil,   'ANSI/AHRI ratings include supply fan')

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

        994 FORMAT('! <DX Cooling Coil Standard Rating Information>, Component Type, Component Name, ',    &
            'Standard Rating (Net) Cooling Capacity {W}, ', 'Standard Rated Net COP {W/W}, ', &
            'EER {Btu/W-h}, ', 'SEER {Btu/W-h}, ', 'IEER {Btu/W-h}')
        995 FORMAT(' DX Cooling Coil Standard Rating Information, ',A,', ',A,', ',A,', ',A,', ',A,', ',A,', ',A)

      CASE DEFAULT
    END SELECT
    RETURN

END SUBROUTINE ReportDXCoilRating

SUBROUTINE CheckCurveLimitsForStandardRatings(DXCoilName, DXCoilType, DXCoilTypeNum, CapFTempCurveIndex, CapFFlowCurveIndex, &
                                              EIRFTempCurveIndex, EIRFFlowCurveIndex, PLFFPLRCurveIndex, &
                                              CalcCapacity,CalcSEER,CalcEER,CalcIEER,CalcHSPF)

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
  USE DataHVACGlobals, ONLY: CoilDX_CoolingSingleSpeed, CoilDX_HeatingEmpirical, CoilDX_MultiSpeedCooling

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
  LOGICAL, INTENT(INOUT)       :: CalcCapacity  ! Logical set to .FALSE. if Standard Rating Capacity should not be calculated
  LOGICAL, INTENT(INOUT)       :: CalcSEER      ! Logical set to .FALSE. if SEER should not be calculated
  LOGICAL, INTENT(INOUT)       :: CalcEER       ! Logical set to .FALSE. if EER should not be calculated
  LOGICAL, INTENT(INOUT)       :: CalcIEER      ! Logical set to .FALSE. if IEER should not be calculated
  LOGICAL, INTENT(INOUT)       :: CalcHSPF      ! Logical set to .FALSE. if HSPF should not be calculated

          ! SUBROUTINE PARAMETER DEFINITIONS:

  REAL(r64), PARAMETER :: OAHighDBTemp=35.0d0   ! Outdoor air dry-bulb temp in degrees C (95F) for Test A
                                                !    (Rated Capacity Std. AHRI 210/240 & AHRI 340/360)
  REAL(r64), PARAMETER :: EAWetBulbTemp=19.44d0 ! Cooling coil entering air wet-bulb temperature in degrees C (67F) Tests A and B
                                                !    (Std. AHRI 210/240 & AHRI 340/360)
                                                ! 19.44C (67F)  Tests A2, B2, B1, and F1  (Multi speed DX Coil)
  REAL(r64), PARAMETER :: OAMidDBTemp=27.78d0   ! Outdoor air dry-bulb temp in degrees C (82F) Test B
                                                !    (For SEER Std. AHRI 210/240)
  REAL(r64), PARAMETER :: OALowDBTempMS=19.44d0 ! Outdoor air dry-bulb temp in degrees C (67F) Test F1
                                                !    (For SEER Std. AHRI 210/240)
  REAL(r64), PARAMETER :: PLRforSEER = 0.50d0   ! Cooilng coil part-load ratio (For SEER Std. AHRI 210/240)
  REAL(r64), PARAMETER :: NominalFlowFraction = 1.0d0  ! Cooling coil flow fraction (fraction of rated flow rate)
                                                       !   AHRI test AirMassFlowRatio is 1.0 (Std. AHRI 210/240 & AHRI 340/360)
  REAL(r64), PARAMETER :: OALowDBTemp=18.3d0    ! Outdoor air dry-bulb temp in degrees C (65F)
                                                !    Std. AHRI AHRI 340/360 Dry-bulb Temp at reduced capacity, <= 0.444
  REAL(r64), PARAMETER :: HeatingHighDBTemp = 8.33d0 ! Outdoor air dry-bulb temp in degrees C (47F) Test H1
                                                     !    (Std. AHRI 210/240)
  REAL(r64), PARAMETER :: HeatingLowDBTemp = -8.33d0 ! Outdoor air dry-bulb temp in degrees C (17F) Test H3
                                                     !    (Std. AHRI 210/240)
  REAL(r64), PARAMETER :: HeatingIDTemp = 21.11d0    ! Heating coil entering air dry-bulb temperature in degrees C (70F)
                                                     ! Test H1, H2 and H3   (Std. AHRI 210/240)

  REAL(r64), PARAMETER :: OutdoorCoilInletAirDrybulbTempRated  = 35.0d0   ! 35.00C (95F)  Tests A2, B2, B1, and F1
  REAL(r64), PARAMETER :: OutdoorCoilInletAirDrybulbTempTestA2 = 35.0d0   ! 35.00C (95F)  Test A2 (high speed)
  REAL(r64), PARAMETER :: OutdoorCoilInletAirDrybulbTempTestB2 = 27.78d0  ! 27.78C (82F)  Test B2 (high speed)
  REAL(r64), PARAMETER :: OutdoorCoilInletAirDrybulbTempTestB1 = 27.78d0  ! 27.78C (82F)  Test B1 (Low speed)
  REAL(r64), PARAMETER :: OutdoorCoilInletAirDrybulbTempTestF1 = 19.44d0  ! 19.44C (67F)  Test F1 (Low speed)

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
      IF ( CapacityDBTempMax < OAHighDBTemp .OR. CapacityDBTempMin > OAHighDBTemp  .OR.                    &
           CapacityWBTempMax < EAWetBulbTemp .OR. CapacityWBTempMin > EAWetBulbTemp ) THEN
           CapCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for flow fraction
      IF ( CapacityFlowRatioMax < NominalFlowFraction .OR. CapacityFlowRatioMin > NominalFlowFraction ) THEN
           CapCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures
      IF ( EIRDBTempMax < OAHighDBTemp .OR. EIRDBTempMin > OAHighDBTemp .OR.                               &
           EIRWBTempMax < EAWetBulbTemp .OR. EIRWBTempMin > EAWetBulbTemp ) THEN
           EIRCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for flow fraction
      IF ( EIRFlowRatioMax < NominalFlowFraction .OR. EIRFlowRatioMin > NominalFlowFraction ) THEN
           EIRCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for temperatures (SEER calculation)
      IF ( CapacityDBTempMax < OAMidDBTemp .OR. CapacityDBTempMin > OAMidDBTemp  .OR.                      &
           CapacityWBTempMax < EAWetBulbTemp .OR. CapacityWBTempMin > EAWetBulbTemp ) THEN
           CapCurveMidOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures (SEER calculation)
      IF ( EIRDBTempMax < OAMidDBTemp .OR. EIRDBTempMin > OAMidDBTemp .OR.                                 &
           EIRWBTempMax < EAWetBulbTemp .OR. EIRWBTempMin > EAWetBulbTemp ) THEN
           EIRCurveMidOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of Part Load Fraction for PLR (SEER calculation)
      IF (PLFFPLRMax < PLRforSEER .OR. PLFFPLRMin > PLRforSEER )THEN
          PLFfPLRforSEERLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
      IF ( CapacityDBTempMax < OAHighDBTemp .OR. CapacityDBTempMin > OALowDBTemp .OR.                      &
           CapacityWBTempMax < EAWetBulbTemp .OR. CapacityWBTempMin > EAWetBulbTemp ) THEN
           CapCurveIEERLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures (IEER high and low test conditions)
      IF ( EIRDBTempMax < OAHighDBTemp .OR. EIRDBTempMin > OALowDBTemp .OR.                                &
           EIRWBTempMax < EAWetBulbTemp .OR. EIRWBTempMin > EAWetBulbTemp ) THEN
           EIRCurveIEERLimitsExceeded = .TRUE.
      END IF

      IF ( CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveHighOATLimitsExceeded .OR.  &
           EIRCurveFlowLimitsExceeded .OR. CapCurveMidOATLimitsExceeded .OR. EIRCurveMidOATLimitsExceeded   .OR.  &
           PLFfPLRforSEERLimitsExceeded .OR. CapCurveIEERLimitsExceeded .OR. EIRCurveIEERLimitsExceeded)     THEN

           CALL ShowWarningError('One of the Standard Ratings for '//TRIM(DXCoilType)//' = '// &
                                 TRIM(DXCoilName)//' could not be calculated, so set to zero in output.')
           CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.'// &
                                  ' Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance.')

           IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError(RoutineName//'The max and/or min limits specified in the corresponding curve objects did not')
            CALL ShowContinueError(' allow the curves to be evaluated at the conditions required to calculate one or more of'// &
                                   ' the Standard Rating values.')
           END IF

           ! For Standard Rating Cooling Capacity:
           IF (CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                          ' Standard Rating Cooling Capacity cannot be calculated.')
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
               CalcCapacity = .FALSE.
           ELSE
               CalcCapacity = .TRUE.
           END IF

           ! For EER:
           IF (CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveHighOATLimitsExceeded .OR.   &
                EIRCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                          ' Energy Efficiency Ratio (EER) cannot be calculated.')
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
               CalcEER = .FALSE.
           ELSE
               CalcEER = .TRUE.
           END IF

           ! For SEER:
           IF ( CapCurveMidOATLimitsExceeded .OR. EIRCurveMidOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded  &
            .OR. EIRCurveFlowLimitsExceeded  .OR. PLFfPLRforSEERLimitsExceeded ) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                          ' Seasonal Energy Efficiency Ratio (SEER) cannot be calculated.')
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
               CalcSEER = .FALSE.
           ELSE
               CalcSEER = .TRUE.
           END IF

           ! For IEER:
           IF ( CapCurveIEERLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveIEERLimitsExceeded .OR.  &
                EIRCurveFlowLimitsExceeded) THEN
               IF (DisplayExtraWarnings) THEN
                   CALL ShowContinueError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '//&
                                          ' Integrated Energy Efficiency Ratio (IEER) cannot be calculated.')
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
               CalcIEER = .FALSE.
           ELSE
               CalcIEER = .TRUE.
           END IF

      END IF  ! End of curve error messages
    CASE (CoilDX_HeatingEmpirical)
      SELECT CASE(GetCurveType(CapFTempCurveIndex))

        CASE('QUADRATIC', 'CUBIC')
          CALL GetCurveMinMaxValues(CapFTempCurveIndex,HeatingCapODBTempMin,HeatingCapODBTempMax)

              ! Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
          IF ( HeatingCapODBTempMax < HeatingHighDBTemp .OR. HeatingCapODBTempMin > HeatingLowDBTemp) THEN
               HeatingCapCurveHSPFLimitsExceeded = .TRUE.
          END IF
        CASE('BIQUADRATIC')
          CALL GetCurveMinMaxValues(CapFTempCurveIndex,HeatingCapIDBTempMin,HeatingCapIDBTempMax, &
                                    HeatingCapODBTempMin,HeatingCapODBTempMax)

              ! Checking the limits of capacity modifying curve for temperatures (IEER high and low test conditions)
          IF ( HeatingCapODBTempMax < HeatingHighDBTemp .OR. HeatingCapODBTempMin > HeatingLowDBTemp .OR.                      &
               HeatingCapODBTempMax < HeatingIDTemp .OR. HeatingCapODBTempMin > HeatingIDTemp ) THEN
               HeatingCapCurveHSPFLimitsExceeded = .TRUE.
          END IF
      END SELECT
      SELECT CASE(GetCurveType(EIRFTempCurveIndex))

        CASE('QUADRATIC', 'CUBIC')
          CALL GetCurveMinMaxValues(EIRFTempCurveIndex,HeatingEIRODBTempMin,HeatingEIRODBTempMax)

          ! Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingEIRODBTempMax < HeatingHighDBTemp .OR. HeatingEIRODBTempMin > HeatingLowDBTemp) THEN
               HeatingEIRCurveHSPFLimitsExceeded = .TRUE.
          END IF
        CASE('BIQUADRATIC')
          CALL GetCurveMinMaxValues(EIRFTempCurveIndex,HeatingEIRIDBTempMin,HeatingEIRIDBTempMax, &
                                    HeatingEIRODBTempMin,HeatingEIRODBTempMax)

          ! Checking the limits of EIR modifying curve for temperatures (HSPF high and low test conditions)
          IF ( HeatingEIRODBTempMax < HeatingHighDBTemp .OR. HeatingEIRODBTempMin > HeatingLowDBTemp .OR.                      &
               HeatingEIRODBTempMax < HeatingIDTemp .OR. HeatingEIRODBTempMin > HeatingIDTemp ) THEN
               HeatingEIRCurveHSPFLimitsExceeded = .TRUE.
          END IF
      END SELECT
      IF ( HeatingCapCurveHSPFLimitsExceeded .OR. HeatingEIRCurveHSPFLimitsExceeded) THEN
        IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(TRIM(DXCoilType)//'='//TRIM(DXCoilName)//': '// &
                                    ' Heating Seasonal Performance Factor cannot be calculated.')
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
        CalcHSPF = .FALSE.
      ELSE
        CalcHSPF = .TRUE.
      ENDIF

!   MultiSpeed DX Coil Net Cooling Capacity and SEER:
    CASE (CoilDX_MultiSpeedCooling)
      CALL GetCurveMinMaxValues(CapFTempCurveIndex,CapacityWBTempMin,CapacityWBTempMax,CapacityDBTempMin,CapacityDBTempMax)
      CALL GetCurveMinMaxValues(EIRFTempCurveIndex,EIRWBTempMin,EIRWBTempMax,EIRDBTempMin,EIRDBTempMax)
      CALL GetCurveMinMaxValues(CapFFlowCurveIndex,CapacityFlowRatioMin,CapacityFlowRatioMax)
      CALL GetCurveMinMaxValues(EIRFFlowCurveIndex,EIRFlowRatioMin,EIRFlowRatioMax)
      CALL GetCurveMinMaxValues(PLFFPLRCurveIndex,PLFFPLRMin,PLFFPLRMax)

      ! Checking the limits of capacity modifying curve for temperatures
      IF ( CapacityDBTempMax < OAHighDBTemp .OR. CapacityDBTempMin > OAHighDBTemp  .OR.                    &
           CapacityWBTempMax < EAWetBulbTemp .OR. CapacityWBTempMin > EAWetBulbTemp ) THEN
           CapCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for flow fraction
      IF ( CapacityFlowRatioMax < NominalFlowFraction .OR. CapacityFlowRatioMin > NominalFlowFraction ) THEN
           CapCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures
      IF ( EIRDBTempMax < OAHighDBTemp .OR. EIRDBTempMin > OAHighDBTemp .OR.                               &
           EIRWBTempMax < EAWetBulbTemp .OR. EIRWBTempMin > EAWetBulbTemp ) THEN
           EIRCurveHighOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for flow fraction
      IF ( EIRFlowRatioMax < NominalFlowFraction .OR. EIRFlowRatioMin > NominalFlowFraction ) THEN
           EIRCurveFlowLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of capacity modifying curve for temperatures (SEER calculation)
      IF ( CapacityDBTempMax < OALowDBTempMS .OR. CapacityDBTempMin > OALowDBTempMS  .OR.                      &
           CapacityWBTempMax < EAWetBulbTemp .OR. CapacityWBTempMin > EAWetBulbTemp ) THEN
           CapCurveLowOATLimitsExceeded = .TRUE.
      END IF
      ! Checking the limits of EIR modifying curve for temperatures (SEER calculation)
      IF ( EIRDBTempMax < OALowDBTempMS .OR. EIRDBTempMin > OALowDBTempMS .OR.                                 &
           EIRWBTempMax < EAWetBulbTemp .OR. EIRWBTempMin > EAWetBulbTemp ) THEN
           EIRCurveLowOATLimitsExceeded = .TRUE.
      END IF

      IF ( CapCurveHighOATLimitsExceeded .OR. CapCurveFlowLimitsExceeded .OR. EIRCurveHighOATLimitsExceeded .OR.  &
           EIRCurveFlowLimitsExceeded .OR. CapCurveLowOATLimitsExceeded .OR. EIRCurveLowOATLimitsExceeded ) THEN

           CALL ShowWarningError('The Standard Ratings is calculated for '//TRIM(DXCoilType)//' = '// &
                                 TRIM(DXCoilName)//' but not at the AHRI test condition due to curve out of bound.')
           CALL ShowContinueError(' Review the Standard Ratings calculations in the Engineering Reference for this coil type.'// &
                                  ' Also, use Output:Diagnostics, DisplayExtraWarnings for further guidance.')

           IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError(RoutineName//'The max and/or min limits specified in the corresponding curve objects did not')
            CALL ShowContinueError(' allow the curves to be evaluated at the AHRI test conditions required to calculate one or'// &
                                   ' more of the Standard Rating values.')
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
               CalcCapacity = .FALSE.
           ELSE
               CalcCapacity = .TRUE.
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
               CalcSEER = .FALSE.
           ELSE
               CalcSEER = .TRUE.
           END IF

      END IF  ! End of curve error messages

    CASE DEFAULT
  END SELECT
  RETURN

END SUBROUTINE CheckCurveLimitsForStandardRatings

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


