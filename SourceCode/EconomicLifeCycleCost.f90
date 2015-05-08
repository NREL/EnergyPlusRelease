MODULE EconomicLifeCycleCost

          ! Module containing the routines dealing with the EconomicLifeCycleCost

          ! MODULE INFORMATION:
          !       AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !   To compute life-cycle cost measures such as present value based
          !   on input provided by the user as well as calculated energy costs.

          ! METHODOLOGY EMPLOYED:
          !   Uses NIST Handbook 135 "Life-Cycle Costing Manual for the Federaml
          !   Energy Management Program" for most computations.

          ! REFERENCES:
          !   To compute the net present value for all costs entered in the
          !   LifeCycleCosts objects, the algorithms from NIST Handbook 135
          !   "Life-Cycle Costing Manual for the Federal Energy Management
          !   Program" will be used as the primary source. Supplemental sources
          !   of algorithms will be derived from ASTM E833-09a "Standard
          !   Terminology of Building Economics", ASTM E917-05 "Standard
          !   Practice for Measuring Life-Cycle Cost of Buildings and Building
          !   Systems", and "Engineering Economic Analysis, Ninth Edition", by
          !   Donald Newnan, Ted Eschenback, and Jerome Lavelle.

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataGlobalConstants
USE DataPrecisionGlobals
USE InputProcessor
USE DataCostEstimate
USE DataGlobals ,   ONLY : MaxNameLength
USE DataIPShortCuts
USE DataInterfaces, ONLY: ShowWarningError,ShowSevereError,ShowContinueError

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: disConvBeginOfYear = 1
INTEGER, PARAMETER :: disConvMidYear = 2
INTEGER, PARAMETER :: disConvEndOfYear = 3

INTEGER, PARAMETER :: inflAppConstantDollar = 1
INTEGER, PARAMETER :: inflAppCurrentDollar = 2

! ModifiedAcceleratedCostRecoverySystem or Straight Line
INTEGER, PARAMETER :: depMethMACRS3 = 1
INTEGER, PARAMETER :: depMethMACRS5 = 2
INTEGER, PARAMETER :: depMethMACRS7 = 3
INTEGER, PARAMETER :: depMethMACRS10 = 4
INTEGER, PARAMETER :: depMethMACRS15 = 5
INTEGER, PARAMETER :: depMethMACRS20 = 6
INTEGER, PARAMETER :: depMethStraight27 = 7
INTEGER, PARAMETER :: depMethStraight31 = 8
INTEGER, PARAMETER :: depMethStraight39 = 9
INTEGER, PARAMETER :: depMethStraight40 = 10
INTEGER, PARAMETER :: depMethNone = 11

INTEGER, PARAMETER :: costCatMaintenance = 1
INTEGER, PARAMETER :: costCatRepair = 2
INTEGER, PARAMETER :: costCatOperation = 3
INTEGER, PARAMETER :: costCatReplacement = 4
INTEGER, PARAMETER :: costCatMinorOverhaul = 5
INTEGER, PARAMETER :: costCatMajorOverhaul = 6
INTEGER, PARAMETER :: costCatOtherOperational = 7
INTEGER, PARAMETER :: costCatConstruction = 8
INTEGER, PARAMETER :: costCatSalvage = 9
INTEGER, PARAMETER :: costCatOtherCapital = 10
INTEGER, PARAMETER :: costCatWater = 11
INTEGER, PARAMETER :: costCatEnergy = 12
INTEGER, PARAMETER :: costCatTotEnergy = 13
INTEGER, PARAMETER :: costCatTotOper = 14
INTEGER, PARAMETER :: costCatTotCaptl = 15
INTEGER, PARAMETER :: costCatTotGrand = 16

INTEGER, PARAMETER :: countOfCostCat = 16  !count of the number of cost categories

! The NIST supplement includes UPV* factors for
!   Electricity
!   Natural gas
!   Distillate oil
!   Liquified petroleum gas
!   Residual oil
!   Coal

INTEGER, PARAMETER :: startServicePeriod = 1
INTEGER, PARAMETER :: startBasePeriod = 2


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:

! related to LifeCycleCost:Parameters
LOGICAL :: LCCparamPresent = .FALSE.                  ! If a LifeCycleCost:Parameters object is present
CHARACTER(len=MaxNameLength)   :: LCCname             ! Name
INTEGER :: discountConvension = disConvEndOfYear      ! Discounting Convention
INTEGER :: inflationApproach = inflAppConstantDollar  ! Inflation Approach
REAL(r64) :: realDiscountRate = 0.0d0                   ! Real Discount Rate
REAL(r64) :: nominalDiscountRate = 0.0d0                ! Nominal Discount Rate
REAL(r64) :: inflation = 0.0d0                          ! Inflation
INTEGER ::  baseDateMonth = 0                         ! Base Date Month (1=Jan, 12=Dec)
INTEGER ::  baseDateYear = 0                          ! Base Date Year  1900-2100
INTEGER ::  serviceDateMonth = 0                      ! Service Date Month (1=Jan, 12=Dec)
INTEGER ::  serviceDateYear = 0                       ! Service Date Year 1900-2100
INTEGER ::  lengthStudyYears = 0                      ! Length of Study Period in Years
INTEGER ::  lengthStudyTotalMonths = 0                ! Length of Study expressed in months (years x 12)
REAL(r64) :: taxRate = 0.0d0                            ! Tax rate
INTEGER :: depreciationMethod = depMethNone           ! Depreciation Method
! derived
INTEGER ::  lastDateMonth = 0                         ! Last Date Month (the month before the base date month)
INTEGER ::  lastDateYear = 0                          ! Last Date Year (base date year + length of study period in years)

TYPE RecurringCostsType
  CHARACTER(len=MaxNameLength)   :: name  = ' '      ! Name
  CHARACTER(len=MaxNameLength)   :: lineItem = ' '   ! Line Item
  INTEGER :: category = costCatMaintenance           ! Category
  REAL(r64) :: cost                                  ! Cost
  INTEGER :: startOfCosts = startServicePeriod       ! Start of Costs
  INTEGER :: yearsFromStart = 0                      ! Years from Start 0 - 100
  INTEGER :: monthsFromStart = 0                     ! Months from Start 0 - 11
  INTEGER :: totalMonthsFromStart = 0                ! Total months (12 x years) + months
  INTEGER :: repeatPeriodYears = 0                   ! Repeat Period Years 1 - 100
  INTEGER :: repeatPeriodMonths = 0                  ! Repeat Period Months 0 - 11
  INTEGER :: totalRepeatPeriodMonths = 0             ! Total months (12 x years) + months
  REAL(r64) :: annualEscalationRate = 0.0d0            ! Annual escalation rate
END TYPE
TYPE (RecurringCostsType), ALLOCATABLE, DIMENSION(:) :: RecurringCosts
INTEGER                                              :: numRecurringCosts = 0

TYPE NonrecurringCostType
  CHARACTER(len=MaxNameLength)   :: name  = ' '      ! Name
  CHARACTER(len=MaxNameLength)   :: lineItem = ' '   ! Line Item
  INTEGER :: category = costCatConstruction          ! Category
  REAL(r64) :: cost                                  ! Cost
  INTEGER :: startOfCosts = startServicePeriod       ! Start of Costs
  INTEGER :: yearsFromStart = 0                      ! Years from Start 0 - 100
  INTEGER :: monthsFromStart = 0                     ! Months from Start 0 - 11
  INTEGER :: totalMonthsFromStart = 0                ! Total months (12 x years) + months
END TYPE
TYPE (NonrecurringCostType), ALLOCATABLE, DIMENSION(:) :: NonrecurringCost
INTEGER                                                :: numNonrecurringCost = 0

TYPE UsePriceEscalationType
  CHARACTER(len=MaxNameLength)   :: name  = ' '        ! Name
  INTEGER :: resource                                  ! resource like electricity or natural gas
                                                       ! (uses definitions from DataGlobalConstants)
  INTEGER :: escalationStartYear = 0                   ! Escalation Start Year 1900-2100
  INTEGER :: escalationStartMonth = 0                  ! Escalation Start Month 1 to 12
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Escalation   ! Escalation by year, first year is baseDateYear
                                                       ! last year is baseDateYear + lengthStudyYears - 1
END TYPE
TYPE (UsePriceEscalationType) , ALLOCATABLE, DIMENSION(:) :: UsePriceEscalation
INTEGER                                                   :: numUsePriceEscalation = 0

TYPE UseAdjustmentType
  CHARACTER(len=MaxNameLength)   :: name  = ' '        ! Name
  INTEGER :: resource                                  ! resource like electricity or natural gas
                                                       ! (uses definitions from DataGlobalConstants)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Adjustment   ! Adjustment by year, first year is baseDateYear
                                                       ! last year is baseDateYear + lengthStudyYears - 1
END TYPE
TYPE (UseAdjustmentType), ALLOCATABLE, DIMENSION(:) :: UseAdjustment
INTEGER                                              :: numUseAdjustment = 0

TYPE CashFlowType
  CHARACTER(len=MaxNameLength)   :: name  = ' '        ! Name - just for labeling output - use Category for aggregation
  INTEGER :: SourceKind                                ! 1=recurring, 2=nonrecurring, 3=resource
  INTEGER :: Resource                                  ! resource like electricity or natural gas
                                                       ! (uses definitions from DataGlobalConstants)
  INTEGER :: Category                                  ! uses "costCat" constants above
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: mnAmount     ! cashflow dollar amount by month, first year is baseDateYear
                                                       ! last year is baseDateYear + lengthStudyYears - 1
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: yrAmount     ! cashflow dollar amount by year, first year is baseDateYear
  INTEGER :: pvKind = 0                                ! kind of present value 1=energy, 2=non-energy,3=not computed but summed
  REAL(r64) :: presentValue                            ! total present value for cashflow
  REAL(r64) :: orginalCost                             ! original cost from recurring, non-recurring or energy cost
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: yrPresVal    ! present value by year, first year is baseDateYear
END TYPE
TYPE (CashFlowType), ALLOCATABLE, DIMENSION(:) :: CashFlow
INTEGER :: numCashFlow
INTEGER, PARAMETER :: skRecurring = 1
INTEGER, PARAMETER :: skNonrecurring = 2
INTEGER, PARAMETER :: skResource = 3
INTEGER, PARAMETER :: skSum = 4
INTEGER, PARAMETER :: pvkEnergy = 1
INTEGER, PARAMETER :: pvkNonEnergy = 2
INTEGER, PARAMETER :: pvkNotComputed = 3
INTEGER :: numResourcesUsed

!present value factors
REAL(r64),ALLOCATABLE,DIMENSION(:) :: SPV
REAL(r64),ALLOCATABLE,DIMENSION(:,:) :: energySPV  !yearly equivalent to FEMP UPV* values

!arrays related to computing after tax cashflow and present value
REAL(r64), ALLOCATABLE, DIMENSION(:) :: DepreciatedCapital
REAL(r64), ALLOCATABLE, DIMENSION(:) :: TaxableIncome
REAL(r64), ALLOCATABLE, DIMENSION(:) :: Taxes
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AfterTaxCashFlow
REAL(r64), ALLOCATABLE, DIMENSION(:) :: AfterTaxPresentValue

CHARACTER(len=*), PARAMETER, DIMENSION(12) :: MonthNames = &
     (/"January  ",  &
       "February ",  &
       "March    ",  &
       "April    ",  &
       "May      ",  &
       "June     ",  &
       "July     ",  &
       "August   ",  &
       "September",  &
       "October  ",  &
       "November ",  &
       "December "/)

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

PUBLIC GetInputForLifeCycleCost
PRIVATE GetInputLifeCycleCostParameters
PRIVATE GetInputLifeCycleCostRecurringCosts
PRIVATE GetInputLifeCycleCostNonrecurringCost
PRIVATE GetInputLifeCycleCostUsePriceEscalation
PRIVATE GetInputLifeCycleCostUseAdjustment

PUBLIC ComputeLifeCycleCostAndReport
PRIVATE ExpressAsCashFlows
PRIVATE ComputePresentValue
PRIVATE ComputeTaxAndDepreciation
PRIVATE WriteTabularLifeCycleCostReport

CONTAINS

SUBROUTINE GetInputForLifeCycleCost
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "LifeCycleCost:Parameters" object.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportTabular, ONLY: AddTOCEntry

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
LOGICAL, SAVE :: GetLifeCycleCostInput=.true.

IF (GetLifeCycleCostInput) THEN
  CALL GetInputLifeCycleCostParameters
  CALL GetInputLifeCycleCostRecurringCosts
  CALL GetInputLifeCycleCostNonrecurringCost
  CALL GetInputLifeCycleCostUsePriceEscalation
  CALL GetInputLifeCycleCostUseAdjustment
  IF (LCCparamPresent) THEN
    CALL AddTOCEntry('Life-Cycle Cost Report','Entire Facility')
  END IF
  GetLifeCycleCostInput=.false.
ENDIF
END SUBROUTINE GetInputForLifeCycleCost

SUBROUTINE ComputeLifeCycleCostAndReport
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Perform the life cycle cost computations and write report.

          ! METHODOLOGY EMPLOYED:

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

IF (LCCparamPresent) THEN
  CALL DisplayString('Computing Life Cycle Costs and Reporting')
  CALL ExpressAsCashFlows
  CALL ComputePresentValue
  CALL ComputeTaxAndDepreciation
  CALL WriteTabularLifeCycleCostReport
ENDIF
END SUBROUTINE ComputeLifeCycleCostAndReport


!======================================================================================================================
!======================================================================================================================
!
!
!    GET INPUT ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE GetInputLifeCycleCostParameters
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "LifeCycleCost:Parameters" object.

          ! METHODOLOGY EMPLOYED:

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

INTEGER                     :: jFld
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
REAL(r64),                   DIMENSION(100)  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
INTEGER                     :: NumObj !count of objects

CurrentModuleObject = 'LifeCycleCost:Parameters'
NumObj = GetNumObjectsFound(CurrentModuleObject)

IF (NumObj .EQ. 0) THEN
  LCCparamPresent = .FALSE.
ELSEIF (NumObj .EQ. 1) THEN
  LCCparamPresent = .TRUE.
  CALL GetObjectItem(CurrentModuleObject,1,AlphaArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another life cycle cost object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(AlphaArray(jFld)),'LifeCycleCost:') .GT. 0) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(AlphaArray(1)) //   &
                ' a field was found containing LifeCycleCost: which may indicate a missing comma.')
    END IF
  END DO
  ! start to extract values from input array into appropriate fields
  !  A1,  \field Name
  !       \required-field
  !       \type alpha
  LCCname = AlphaArray(1)
  !  A2, \field Discounting Convention
  !      \type choice
  !      \key EndOfYear
  !      \key MidYear
  !      \key BeginningOfYear
  !      \default EndOfYear
  IF (SameString(AlphaArray(2),'EndOfYear')) THEN
    discountConvension = disConvEndOfYear
  ELSEIF (SameString(AlphaArray(2),'MidYear')) THEN
    discountConvension = disConvMidYear
  ELSEIF (SameString(AlphaArray(2),'BeginningOfYear')) THEN
    discountConvension = disConvBeginOfYear
  ELSE
    discountConvension = disConvEndOfYear
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(2))//'="'//  &
       TRIM(AlphaArray(2))//'". EndOfYear will be used.')
  END IF
  ! A3,  \field Inflation Approach
  !      \type choice
  !      \key ConstantDollar
  !      \key CurrentDollar
  !      \default ConstantDollar
  IF (SameString(AlphaArray(3),'ConstantDollar')) THEN
    inflationApproach = inflAppConstantDollar
  ELSEIF (SameString(AlphaArray(3),'CurrentDollar')) THEN
    inflationApproach = inflAppCurrentDollar
  ELSE
    inflationApproach = inflAppConstantDollar
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(3))//'="'//  &
       TRIM(AlphaArray(3))//'". ConstantDollar will be used.')
  END IF
  ! N1,  \field Real Discount Rate
  !      \type real
  realDiscountRate = NumArray(1)
  IF ((inflationApproach .EQ. inflAppConstantDollar) .AND. lNumericFieldBlanks(1)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid for field '//TRIM(cNumericFieldNames(1))//    &
       ' to be blank when ConstantDollar analysis is be used.')
  END IF
  IF ((realDiscountRate .GT. 0.30d0) .OR. (realDiscountRate .LT. -0.30d0)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(1))//    &
       '.  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ')
  END IF
  ! N2,  \field Nominal Discount Rate
  !      \type real
  nominalDiscountRate = NumArray(2)
  IF ((inflationApproach .EQ. inflAppCurrentDollar) .AND. lNumericFieldBlanks(2)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid for field '//TRIM(cNumericFieldNames(2))//    &
       ' to be blank when CurrentDollar analysis is be used.')
  END IF
  IF ((nominalDiscountRate .GT. 0.30d0) .OR. (nominalDiscountRate .LT. -0.30d0)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(2))//    &
       '.  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ')
  END IF
  ! N3,  \field Inflation
  !      \type real
  inflation = NumArray(3)
  IF ((inflationApproach .EQ. inflAppConstantDollar) .AND. (.NOT. lNumericFieldBlanks(3))) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid for field '//TRIM(cNumericFieldNames(3))//    &
       ' contain a value when ConstantDollar analysis is be used.')
  END IF
  IF ((inflation .GT. 0.30d0) .OR. (inflation .LT. -0.30d0)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(3))//    &
       '.  This value is the decimal value not a percentage so most values are between 0.02 and 0.15. ')
  END IF
  ! A4,  \field Base Date Month
  !      \type choice
  !      \key January
  !      \key February
  !      \key March
  !      \key April
  !      \key May
  !      \key June
  !      \key July
  !      \key August
  !      \key September
  !      \key October
  !      \key November
  !      \key December
  !      \default January
  baseDateMonth = MonthToMonthNumber(AlphaArray(4),1)
  ! N4,  \field Base Date Year
  !      \type integer
  !      \minimum 1900
  !      \maximum 2100
  baseDateYear = INT(NumArray(4))
  IF (baseDateYear .GT. 2100) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(4))//    &
       '.  Value greater than 2100 yet it is representing a year. ')
  END IF
  IF (baseDateYear .LT. 1900) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(4))//    &
       '.  Value less than 1900 yet it is representing a year. ')
  END IF
  ! A5,  \field Service Date Month
  !      \type choice
  !      \key January
  !      \key February
  !      \key March
  !      \key April
  !      \key May
  !      \key June
  !      \key July
  !      \key August
  !      \key September
  !      \key October
  !      \key November
  !      \key December
  !      \default January
  serviceDateMonth = MonthToMonthNumber(AlphaArray(5),1)
  ! N5,  \field Service Date Year
  !      \type integer
  !      \minimum 1900
  !      \maximum 2100
  serviceDateYear = INT(NumArray(5))
  IF (serviceDateYear .GT. 2100) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(5))//    &
       '.  Value greater than 2100 yet it is representing a year. ')
  END IF
  IF (serviceDateYear .LT. 1900) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(5))//    &
       '.  Value less than 1900 yet it is representing a year. ')
  END IF
  ! N6,  \field Length of Study Period in Years
  !      \type integer
  !      \minimum 1
  !      \maximum 100
  lengthStudyYears = INT(NumArray(6))
  IF (lengthStudyYears .GT. 100) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(6))//    &
       '.  A value greater than 100 is not reasonable for an economic evaluation. ')
  END IF
  IF (lengthStudyYears .LT. 1) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(6))//    &
       '.  A value less than 1 is not reasonable for an economic evaluation. ')
  END IF
  lengthStudyTotalMonths = lengthStudyYears * 12
  ! N7, \field Tax rate
  !      \type real
  !      \minimum 0.0
  taxRate = NumArray(7)
  IF (taxRate .LT. 0.0d0 .AND. (.NOT. lNumericFieldBlanks(7))) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(10))//    &
       '.  A value less than 0 is not reasonable for a tax rate. ')
  END IF
  ! A6;  \field Depreciation Method
  !      \type choice
  !      \key ModifiedAcceleratedCostRecoverySystem-3year
  !      \key ModifiedAcceleratedCostRecoverySystem-5year
  !      \key ModifiedAcceleratedCostRecoverySystem-7year
  !      \key ModifiedAcceleratedCostRecoverySystem-10year
  !      \key ModifiedAcceleratedCostRecoverySystem-15year
  !      \key ModifiedAcceleratedCostRecoverySystem-20year
  !      \key StraightLine-27year
  !      \key StraightLine-31year
  !      \key StraightLine-39year
  !      \key StraightLine-40year
  !      \key None
  !      \default None
  IF (SameString(AlphaArray(6),'ModifiedAcceleratedCostRecoverySystem-3year')) THEN
    depreciationMethod = depMethMACRS3
  ELSEIF (SameString(AlphaArray(6),'ModifiedAcceleratedCostRecoverySystem-5year')) THEN
    depreciationMethod = depMethMACRS5
  ELSEIF (SameString(AlphaArray(6),'ModifiedAcceleratedCostRecoverySystem-7year')) THEN
    depreciationMethod = depMethMACRS7
  ELSEIF (SameString(AlphaArray(6),'ModifiedAcceleratedCostRecoverySystem-10year')) THEN
    depreciationMethod = depMethMACRS10
  ELSEIF (SameString(AlphaArray(6),'ModifiedAcceleratedCostRecoverySystem-15year')) THEN
    depreciationMethod = depMethMACRS15
  ELSEIF (SameString(AlphaArray(6),'ModifiedAcceleratedCostRecoverySystem-20year')) THEN
    depreciationMethod = depMethMACRS20
  ELSEIF (SameString(AlphaArray(6),'StraightLine-27year')) THEN
    depreciationMethod = depMethStraight27
  ELSEIF (SameString(AlphaArray(6),'StraightLine-31year')) THEN
    depreciationMethod = depMethStraight31
  ELSEIF (SameString(AlphaArray(6),'StraightLine-39year')) THEN
    depreciationMethod = depMethStraight39
  ELSEIF (SameString(AlphaArray(6),'StraightLine-40year')) THEN
    depreciationMethod = depMethStraight40
  ELSEIF (SameString(AlphaArray(6),'None')) THEN
    depreciationMethod = depMethNone
  ELSEIF (lAlphaFieldBlanks(6)) THEN
    depreciationMethod = depMethNone
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': The input field '//TRIM(cAlphaFieldNames(6)) //  &
       'is blank. "None" will be used.')
  ELSE
    depreciationMethod = depMethNone
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(6))//'="'//  &
       TRIM(AlphaArray(6))//'". "None" will be used.')
  END IF
  ! compute derived variables
  lastDateMonth = baseDateMonth - 1 !same month of the year for first and last month
  IF (lastDateMonth .EQ. 0) lastDateMonth = 12
  lastDateYear = baseDateYear + lengthStudyYears - 1
ELSE
  CALL ShowWarningError(TRIM(CurrentModuleObject)//': Only one instance of this object is allowed. ' //  &
       'No life-cycle cost reports will be generated. ')
  LCCparamPresent = .FALSE.
END IF
END SUBROUTINE GetInputLifeCycleCostParameters

SUBROUTINE GetInputLifeCycleCostRecurringCosts
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "LifeCycleCost:RecurringCosts" object.

          ! METHODOLOGY EMPLOYED:

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

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: jFld
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
REAL(r64),                   DIMENSION(100)  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

IF (.NOT. LCCparamPresent) RETURN
CurrentModuleObject = 'LifeCycleCost:RecurringCosts'
numRecurringCosts = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(RecurringCosts(numRecurringCosts))
DO iInObj = 1 , numRecurringCosts
  CALL GetObjectItem(CurrentModuleObject,iInObj,AlphaArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another life cycle cost object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(AlphaArray(jFld)),'LifeCycleCost:') .GT. 0) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(AlphaArray(1)) //   &
                ' a field was found containing LifeCycleCost: which may indicate a missing comma.')
    END IF
  END DO
  ! start to extract values from input array into appropriate fields
  !   A1,  \field Name
  !        \required-field
  !        \type alpha
  RecurringCosts(iInObj)%Name = AlphaArray(1)
  !   A2,  \field Category
  !        \type choice
  !        \key Maintenance
  !        \key Repair
  !        \key Operation
  !        \key Replacement
  !        \key MinorOverhaul
  !        \key MajorOverhaul
  !        \key OtherOperational
  !        \default Maintenance
  IF (SameString(AlphaArray(2),'Maintenance')) THEN
    RecurringCosts(iInObj)%category = costCatMaintenance
  ELSEIF (SameString(AlphaArray(2),'Repair')) THEN
    RecurringCosts(iInObj)%category = costCatRepair
  ELSEIF (SameString(AlphaArray(2),'Operation')) THEN
    RecurringCosts(iInObj)%category = costCatOperation
  ELSEIF (SameString(AlphaArray(2),'Replacement')) THEN
    RecurringCosts(iInObj)%category = costCatReplacement
  ELSEIF (SameString(AlphaArray(2),'MinorOverhaul')) THEN
    RecurringCosts(iInObj)%category = costCatMinorOverhaul
  ELSEIF (SameString(AlphaArray(2),'MajorOverhaul')) THEN
    RecurringCosts(iInObj)%category = costCatMajorOverhaul
  ELSEIF (SameString(AlphaArray(2),'OtherOperational')) THEN
    RecurringCosts(iInObj)%category = costCatOtherOperational
  ELSE
    RecurringCosts(iInObj)%category = costCatMaintenance
    CALL ShowWarningError(TRIM(CurrentModuleObject) //': Invalid '//TRIM(cAlphaFieldNames(2))//'="'//  &
       TRIM(AlphaArray(2))//'". The category of Maintenance will be used.')
  END IF
  !   N1,  \field Cost
  !        \type real
  RecurringCosts(iInObj)%cost = NumArray(1)
  !   A3,  \field Start of Costs
  !        \type choice
  !        \key ServicePeriod
  !        \key BasePeriod
  !        \default ServicePeriod
  IF (SameString(AlphaArray(3),'ServicePeriod')) THEN
    RecurringCosts(iInObj)%startOfCosts = startServicePeriod
  ELSEIF (SameString(AlphaArray(3),'BasePeriod')) THEN
    RecurringCosts(iInObj)%startOfCosts = startBasePeriod
  ELSE
    RecurringCosts(iInObj)%startOfCosts = startServicePeriod
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(3))//'="'//  &
       TRIM(AlphaArray(3))//'". The start of the service period will be used.')
  END IF
  !   N2,  \field Years from Start
  !        \type integer
  !        \minimum 0
  !        \maximum 100
  RecurringCosts(iInObj)%yearsFromStart = INT(NumArray(2))
  IF (RecurringCosts(iInObj)%yearsFromStart .GT. 100) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(2))//    &
       '.  This value is the number of years from the start so a value greater than 100 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF (RecurringCosts(iInObj)%yearsFromStart .LT. 0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(2))//    &
       '.  This value is the number of years from the start so a value less than 0 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  !   N3,  \field Months from Start
  !        \type integer
  !        \minimum 0
  !        \maximum 1200
  RecurringCosts(iInObj)%monthsFromStart = INT(NumArray(3))
  IF (RecurringCosts(iInObj)%monthsFromStart .GT. 1200) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(3))//    &
       '.  This value is the number of months from the start so a value greater than 1200 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF (RecurringCosts(iInObj)%monthsFromStart .LT. 0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(3))//    &
       '.  This value is the number of months from the start so a value less than 0 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  !   N4,  \field Repeat Period Years
  !        \type integer
  !        \minimum 1
  !        \maximum 100
  RecurringCosts(iInObj)%repeatPeriodYears = INT(NumArray(4))
  IF (RecurringCosts(iInObj)%repeatPeriodYears .GT. 100) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(4))//    &
       '.  This value is the number of years between occurances of the cost so a value greater than 100 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF (RecurringCosts(iInObj)%repeatPeriodYears .LT. 1) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(4))//    &
       '.  This value is the number of years between occurances of the cost so a value less than 1 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  !   N5,  \field Repeat Period Months
  !        \type integer
  !        \minimum 0
  !        \maximum 1200
  RecurringCosts(iInObj)%repeatPeriodMonths = INT(NumArray(5))
  IF (RecurringCosts(iInObj)%repeatPeriodMonths .GT. 1200) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(5))//    &
       '.  This value is the number of months between occurances of the cost so a value greater than 1200 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF (RecurringCosts(iInObj)%repeatPeriodMonths .LT. 0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(5))//    &
       '.  This value is the number of months between occurances of the cost so a value less than 0 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF ((RecurringCosts(iInObj)%repeatPeriodMonths .EQ. 0) .AND. (RecurringCosts(iInObj)%repeatPeriodYears .EQ. 0)) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in fields '//TRIM(cNumericFieldNames(5))//    &
       ' and '// TRIM(cNumericFieldNames(4))//'.  The repeat period must not be zero months and zero years. ')
  END IF
  !   N6;  \field Annual escalation rate
  !        \type real
  RecurringCosts(iInObj)%annualEscalationRate = INT(NumArray(6))
  IF (RecurringCosts(iInObj)%annualEscalationRate .GT. 0.30d0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(6))//    &
       '.  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. ')
  END IF
  IF (RecurringCosts(iInObj)%annualEscalationRate .LT. -0.30d0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(6))//    &
       '.  This value is the decimal value for the annual escalation so most values are between 0.02 and 0.15. ')
  END IF
  ! express the years and months fields in total months
  RecurringCosts(iInObj)%totalMonthsFromStart = RecurringCosts(iInObj)%yearsFromStart * 12 +   &
     RecurringCosts(iInObj)%monthsFromStart
  RecurringCosts(iInObj)%totalRepeatPeriodMonths = RecurringCosts(iInObj)%repeatPeriodYears * 12 +   &
     RecurringCosts(iInObj)%repeatPeriodMonths
END DO
END SUBROUTINE GetInputLifeCycleCostRecurringCosts

SUBROUTINE GetInputLifeCycleCostNonrecurringCost
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "LifeCycleCost:NonrecurringCost" object.

          ! METHODOLOGY EMPLOYED:

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

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: jFld
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
REAL(r64),                   DIMENSION(100)  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
INTEGER                     :: numComponentCostLineItems !number of ComponentCost:LineItem objects

IF (.NOT. LCCparamPresent) RETURN
CurrentModuleObject = 'LifeCycleCost:NonrecurringCost'
numNonrecurringCost = GetNumObjectsFound(CurrentModuleObject)
numComponentCostLineItems = GetNumObjectsFound('ComponentCost:LineItem')
IF (numComponentCostLineItems .GT. 0) THEN !leave room for component cost total
  ALLOCATE(NonrecurringCost(numNonrecurringCost + 1)) !add a place for CostEstimate total
ELSE
  ALLOCATE(NonrecurringCost(numNonrecurringCost))
END IF
DO iInObj = 1 , numNonrecurringCost
  CALL GetObjectItem(CurrentModuleObject,iInObj,AlphaArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another life cycle cost object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(AlphaArray(jFld)),'LifeCycleCost:') .GT. 0) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(AlphaArray(1)) //   &
                ' a field was found containing LifeCycleCost: which may indicate a missing comma.')
    END IF
  END DO
  ! start to extract values from input array into appropriate fields
  ! A1,  \field Name
  !      \required-field
  !      \type alpha
  NonrecurringCost(iInObj)%Name = AlphaArray(1)
  ! A2,  \field Category
  !      \type choice
  !      \key Construction
  !      \key Salvage
  !      \key OtherCapital
  !      \default Construction
  IF (SameString(AlphaArray(2),'Construction')) THEN
    NonrecurringCost(iInObj)%category = costCatConstruction
  ELSEIF (SameString(AlphaArray(2),'Salvage')) THEN
    NonrecurringCost(iInObj)%category = costCatSalvage
  ELSEIF (SameString(AlphaArray(2),'OtherCapital')) THEN
    NonrecurringCost(iInObj)%category = costCatOtherCapital
  ELSE
    NonrecurringCost(iInObj)%category = costCatConstruction
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(2))//'="'//  &
       TRIM(AlphaArray(2))//'". The category of Construction will be used.')
  END IF
  ! N1,  \field Cost
  !      \type real
  NonrecurringCost(iInObj)%cost = NumArray(1)
  ! A3,  \field Start of Costs
  !      \type choice
  !      \key ServicePeriod
  !      \key BasePeriod
  !      \default ServicePeriod
  IF (SameString(AlphaArray(3),'ServicePeriod')) THEN
    NonrecurringCost(iInObj)%startOfCosts = startServicePeriod
  ELSEIF (SameString(AlphaArray(3),'BasePeriod')) THEN
    NonrecurringCost(iInObj)%startOfCosts = startBasePeriod
  ELSE
    NonrecurringCost(iInObj)%startOfCosts = startServicePeriod
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(3))//'="'//  &
       TRIM(AlphaArray(3))//'". The start of the service period will be used.')
  END IF
  ! N2,  \field Years from Start
  !      \type integer
  !      \minimum 0
  !      \maximum 100
  NonrecurringCost(iInObj)%yearsFromStart = INT(NumArray(2))
  IF (NonrecurringCost(iInObj)%yearsFromStart .GT. 100) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(2))//    &
       '.  This value is the number of years from the start so a value greater than 100 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF (NonrecurringCost(iInObj)%yearsFromStart .LT. 0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(2))//    &
       '.  This value is the number of years from the start so a value less than 0 is not reasonable for an economic evaluation. ')
  END IF
  !  N3;  \field Months from Start
  !       \type integer
  !       \minimum 0
  !       \maximum 11
  NonrecurringCost(iInObj)%monthsFromStart = INT(NumArray(3))
  IF (NonrecurringCost(iInObj)%monthsFromStart .GT. 1200) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(3))//    &
       '.  This value is the number of months from the start so a value greater than 1200 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  IF (NonrecurringCost(iInObj)%monthsFromStart .LT. 0) THEN
    CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(3))//    &
       '.  This value is the number of months from the start so a value less than 0 '//  &
          'is not reasonable for an economic evaluation. ')
  END IF
  ! express the years and months fields in total months
  NonrecurringCost(iInObj)%totalMonthsFromStart = NonrecurringCost(iInObj)%yearsFromStart * 12 +   &
     NonrecurringCost(iInObj)%monthsFromStart
END DO
END SUBROUTINE GetInputLifeCycleCostNonrecurringCost

SUBROUTINE GetInputLifeCycleCostUsePriceEscalation
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "LifeCycleCost:UsePriceEscalation" object.

          ! METHODOLOGY EMPLOYED:

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

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: jYear
INTEGER                     :: jFld
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
REAL(r64),                   DIMENSION(100)  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
INTEGER :: escStartYear = 0
INTEGER :: escNumYears = 0
INTEGER :: escEndYear = 0
INTEGER :: earlierEndYear = 0
INTEGER :: laterStartYear = 0
INTEGER :: curEsc = 0
INTEGER :: curFld = 0

IF (.NOT. LCCparamPresent) RETURN
CurrentModuleObject = 'LifeCycleCost:UsePriceEscalation'
numUsePriceEscalation = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(UsePriceEscalation(numUsePriceEscalation))
DO iInObj = 1 , numUsePriceEscalation
  ALLOCATE(UsePriceEscalation(iInObj)%Escalation(lengthStudyYears))
END DO
IF (numUsePriceEscalation .GT. 0) THEN
  DO iInObj = 1 , numUsePriceEscalation
    CALL GetObjectItem(CurrentModuleObject,iInObj,AlphaArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    !check to make sure none of the values are another life cycle cost object
    DO jFld = 1, NumAlphas
      IF (INDEX(MakeUpperCase(AlphaArray(jFld)),'LifeCycleCost:') .GT. 0) THEN
        CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(AlphaArray(1)) //   &
                  ' a field was found containing LifeCycleCost: which may indicate a missing comma.')
      END IF
    END DO
    ! start to extract values from input array into appropriate fields
    ! A1,  \field Name
    !      \required-field
    !      \type alpha
    UsePriceEscalation(iInObj)%Name = TRIM(AlphaArray(1))
    !  A2,  \field Resource
    !       \required-field
    !       \type choice
    !       \key Electricity
    !       \key NaturalGas
    !       \key Steam
    !       \key Gasoline
    !       \key Diesel
    !       \key Coal
    !       \key FuelOil#1
    !       \key FuelOil#2
    !       \key Propane
    !       \key Water
    !       \key OtherFuel1
    !       \key OtherFuel2
    UsePriceEscalation(iInObj)%resource = AssignResourceTypeNum(AlphaArray(2)) !use function from DataGlobalConstants
    IF (NumAlphas .GT. 3) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' contains more alpha fields than expected.')
    END IF
    ! N1,  \field Escalation Start Year
    !      \type integer
    !      \minimum 1900
    !      \maximum 2100
    UsePriceEscalation(iInObj)%escalationStartYear = INT(NumArray(1))
    IF (UsePriceEscalation(iInObj)%escalationStartYear .GT. 2100) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(1))//    &
         '.  Value greater than 2100 yet it is representing a year. ')
    END IF
    IF (UsePriceEscalation(iInObj)%escalationStartYear .LT. 1900) THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//': Invalid value in field '//TRIM(cNumericFieldNames(1))//    &
         '.  Value less than 1900 yet it is representing a year. ')
    END IF
    ! A3,  \field Escalation Start Month
    !      \type choice
    !      \key January
    !      \key February
    !      \key March
    !      \key April
    !      \key May
    !      \key June
    !      \key July
    !      \key August
    !      \key September
    !      \key October
    !      \key November
    !      \key December
    !      \default January
    UsePriceEscalation(iInObj)%escalationStartMonth = MonthToMonthNumber(AlphaArray(3),1)
    ! N2,  \field Year 1 Escalation
    !      \type real
    !      \begin-extensible
    ! The array is from the baseDateYear until baseDateYear + lengthStudyYears
    ! Set the array to default to 1.0
    DO jYear = 1,lengthStudyYears
      UsePriceEscalation(iInObj)%Escalation(jYear) = 1.0d0
    END DO
    ! Since the years in the UsePriceEscalation may not match up with the baseDateYear and
    ! the lenghtStudyYears, need to make adjustments when reading in the values to align
    ! with the baseDateYear (the first item in all yearly arrays)
    escStartYear = UsePriceEscalation(iInObj)%escalationStartYear
    escNumYears = NumNums - 1
    escEndYear = escStartYear + escNumYears - 1
    earlierEndYear = MIN(escEndYear,lastDateYear) ! pick the earlier ending date
    laterStartYear = MAX(escStartYear,baseDateYear) !pick the later starting date
    DO jYear = laterStartYear,earlierEndYear
      curFld = 2 + jYear - escStartYear
      curEsc = 1 + jYear - baseDateYear
      IF ((curFld .LE. numNums) .AND. (curFld .GE. 1)) THEN
        IF ((curEsc .LE. lengthStudyYears) .AND. (curEsc .GE. 1)) THEN
          UsePriceEscalation(iInObj)%Escalation(curEsc) = NumArray(curFld)
        END IF
      END IF
    END DO
  END DO
END IF
END SUBROUTINE GetInputLifeCycleCostUsePriceEscalation

SUBROUTINE GetInputLifeCycleCostUseAdjustment
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "LifeCycleCost:UseAdjustment" object.

          ! METHODOLOGY EMPLOYED:

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

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: jFld
INTEGER                     :: jYear
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
REAL(r64),                   DIMENSION(100)  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
INTEGER                      :: numFldsToUse

IF (.NOT. LCCparamPresent) RETURN
CurrentModuleObject = 'LifeCycleCost:UseAdjustment'
numUseAdjustment = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(UseAdjustment(numUseAdjustment))
DO iInObj = 1 , numUseAdjustment
  ALLOCATE(UseAdjustment(iInObj)%Adjustment(lengthStudyYears))
END DO
IF (numUseAdjustment .GT. 0) THEN
  DO iInObj = 1 , numUseAdjustment
    CALL GetObjectItem(CurrentModuleObject,iInObj,AlphaArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    !check to make sure none of the values are another life cycle cost object
    DO jFld = 1, NumAlphas
      IF (INDEX(MakeUpperCase(AlphaArray(jFld)),'LifeCycleCost:') .GT. 0) THEN
        CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(AlphaArray(1)) //   &
                  ' a field was found containing LifeCycleCost: which may indicate a missing comma.')
      END IF
    END DO
    ! start to extract values from input array into appropriate fields
    !  A1,  \field Name
    !       \required-field
    !       \type alpha
    UseAdjustment(iInObj)%Name = AlphaArray(1)
    !  A2,  \field Resource
    !       \required-field
    !       \type choice
    !       \key Electricity
    !       \key NaturalGas
    !       \key Steam
    !       \key Gasoline
    !       \key Diesel
    !       \key Coal
    !       \key FuelOil#1
    !       \key FuelOil#2
    !       \key Propane
    !       \key Water
    !       \key OtherFuel1
    !       \key OtherFuel2
    UseAdjustment(iInObj)%resource = AssignResourceTypeNum(AlphaArray(2)) !use function from DataGlobalConstants
    IF (NumAlphas .GT. 2) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' contains more alpha fields than expected.')
    END IF
    !  N1,  \field Year 1 Multiplier
    !       \type real
    !       \begin-extensible
    ! Set the array to default to 1.0
    DO jYear = 1,lengthStudyYears
      UseAdjustment(iInObj)%Adjustment(jYear) = 1.0d0
    END DO
    numFldsToUse = MIN(NumNums,lengthStudyYears)
    DO jYear = 1,numFldsToUse
      UseAdjustment(iInObj)%Adjustment(jYear) = numArray(jYear)
    END DO
  END DO
END IF
END SUBROUTINE GetInputLifeCycleCostUseAdjustment

INTEGER FUNCTION MonthToMonthNumber(inMonthString,inDefaultMonth)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Convert the string of the name of the month into numbers 1 to 12

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: inMonthString
INTEGER, INTENT(IN)          :: inDefaultMonth

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

IF (SameString(inMonthString,'January')) THEN
  MonthToMonthNumber = 1
ELSEIF (SameString(inMonthString,'February')) THEN
  MonthToMonthNumber = 2
ELSEIF (SameString(inMonthString,'March')) THEN
  MonthToMonthNumber = 3
ELSEIF (SameString(inMonthString,'April')) THEN
  MonthToMonthNumber = 4
ELSEIF (SameString(inMonthString,'May')) THEN
  MonthToMonthNumber = 5
ELSEIF (SameString(inMonthString,'June')) THEN
  MonthToMonthNumber = 6
ELSEIF (SameString(inMonthString,'July')) THEN
  MonthToMonthNumber = 7
ELSEIF (SameString(inMonthString,'August')) THEN
  MonthToMonthNumber = 8
ELSEIF (SameString(inMonthString,'September')) THEN
  MonthToMonthNumber = 9
ELSEIF (SameString(inMonthString,'October')) THEN
  MonthToMonthNumber = 10
ELSEIF (SameString(inMonthString,'November')) THEN
  MonthToMonthNumber = 11
ELSEIF (SameString(inMonthString,'December')) THEN
  MonthToMonthNumber = 12
ELSE
  MonthToMonthNumber = inDefaultMonth
ENDIF
END FUNCTION MonthToMonthNumber

!======================================================================================================================
!======================================================================================================================
!
!
!    COMPUTATION ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE ExpressAsCashFlows
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Convert all recurring and nonrecurring costs into cash flows
          !    used in calculations and reporting.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

USE EconomicTariff, ONLY: GetMonthlyCostForResource

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
INTEGER :: iCashFlow
INTEGER :: iResource
INTEGER :: jCost
INTEGER :: jMonth
INTEGER :: jAdj
INTEGER :: kYear
INTEGER :: offset
INTEGER :: month !number of months since base date
INTEGER :: firstMonth
INTEGER :: repeatMonths
INTEGER :: baseMonths1900 = 0       ! number of months since 1900 for base period
INTEGER :: serviceMonths1900 = 0    ! number of months since 1900 for service period
INTEGER :: monthsBaseToService
REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: resourceCosts
REAL(r64), DIMENSION(12) :: curResourceCosts
LOGICAL, ALLOCATABLE, DIMENSION(:) :: resourceCostNotZero
REAL(r64), ALLOCATABLE, DIMENSION(:) :: resourceCostAnnual
REAL(r64) :: annualCost
INTEGER :: cashFlowCounter
INTEGER :: found
INTEGER :: curCategory
REAL(r64),ALLOCATABLE,DIMENSION(:) :: monthlyInflationFactor
REAL(r64) :: inflationPerMonth
INTEGER :: iLoop

! compute months from 1900 for base and service period
baseMonths1900 = (baseDateYear - 1900) * 12 + baseDateMonth
serviceMonths1900 = (serviceDateYear - 1900) * 12 + serviceDateMonth
monthsBaseToService = serviceMonths1900 - baseMonths1900
! if ComponentCost:LineItem exist, the grand total of all costs are another non-recurring cost
IF (CurntBldg%GrandTotal .GT. 0.0d0) THEN !from DataCostEstimate and computed in WriteCompCostTable within OutputReportTabular
  numNonrecurringCost = numNonrecurringCost + 1
  NonrecurringCost(numNonrecurringCost)%Name = 'Total of ComponentCost:*'
  NonrecurringCost(numNonrecurringCost)%lineItem = ''
  NonrecurringCost(numNonrecurringCost)%category = costCatConstruction
  NonrecurringCost(numNonrecurringCost)%cost = CurntBldg%GrandTotal
  NonrecurringCost(numNonrecurringCost)%startOfCosts = startBasePeriod
  NonrecurringCost(numNonrecurringCost)%yearsFromStart = 0
  NonrecurringCost(numNonrecurringCost)%monthsFromStart = 0
  NonrecurringCost(numNonrecurringCost)%totalMonthsFromStart = 0
END IF
! gather costs from EconomicTariff for each end use
ALLOCATE(resourceCosts(NumOfResourceTypes,12))
ALLOCATE(resourceCostNotZero(NumOfResourceTypes))
ALLOCATE(resourceCostAnnual(NumOfResourceTypes))
numResourcesUsed = 0
DO iResource = 1 , NumOfResourceTypes
  CALL GetMonthlyCostForResource(iResource + ResourceTypeInitialOffset,curResourceCosts)
  annualCost = 0.0d0
  DO jMonth = 1, 12
    resourceCosts(iResource,jMonth) = curResourceCosts(jMonth)
    annualCost = annualCost + resourceCosts(iResource,jMonth)
  END DO
  IF (annualCost .NE. 0.0d0) THEN
    numResourcesUsed = numResourcesUsed + 1
    resourceCostNotZero(iResource) = .TRUE.
  ELSE
    resourceCostNotZero(iResource) = .FALSE.
  END IF
  resourceCostAnnual(iResource) = annualCost
END DO
! pre-compute the inflation factors for each year
ALLOCATE(monthlyInflationFactor(lengthStudyTotalMonths))
IF (inflationApproach .EQ. inflAppConstantDollar) THEN
  monthlyInflationFactor = 1.0d0 !not really used but just in case
ELSEIF (inflationApproach .EQ. inflAppCurrentDollar) THEN
  ! to allocate an interest rate (in this case inflation) cannot just use 1/12
  ! for the monthly value since it will be slightly wrong. Instead use inverse of
  ! formula from Newnan (4-32) which is r = m x (ia + 1)^(1/m) - 1)
  inflationPerMonth = ((inflation + 1.0d0) ** (1.0d0/12.0d0)) - 1
  DO jMonth = 1, lengthStudyTotalMonths
    monthlyInflationFactor(jMonth) = (1.0d0 + inflationPerMonth) ** (jMonth - 1)
  END DO
END IF

numCashFlow = countOfCostCat + numRecurringCosts + numNonrecurringCost + numResourcesUsed
! Cashflow array order:
!   1 cost categories
!   2 recurring costs
!   3 nonrecurring costs
!   4 resource costs
ALLOCATE(CashFlow(numCashFlow))
DO iCashFlow = 1 , numCashFlow
  ALLOCATE(CashFlow(iCashFlow)%mnAmount(lengthStudyTotalMonths))
  ALLOCATE(CashFlow(iCashFlow)%yrAmount(lengthStudyYears))
  ALLOCATE(CashFlow(iCashFlow)%yrPresVal(lengthStudyYears))
  CashFlow(iCashFlow)%mnAmount = 0.0d0  !zero all cash flow values
  CashFlow(iCashFlow)%yrAmount = 0.0d0  !zero all cash flow values
  CashFlow(iCashFlow)%yrPresVal = 0.0d0  !zero all present values
END DO
! Put nonrecurring costs into cashflows
offset = countOfCostCat + numRecurringCosts
DO jCost = 1, numNonrecurringCost
  CashFlow(offset + jCost)%name = NonrecurringCost(jCost)%name
  CashFlow(offset + jCost)%SourceKind = skNonrecurring
  CashFlow(offset + jCost)%Category = NonrecurringCost(jCost)%category
  CashFlow(offset + jCost)%orginalCost = NonrecurringCost(jCost)%cost
  CashFlow(offset + jCost)%mnAmount = 0.0d0
  IF (NonrecurringCost(jCost)%startOfCosts .EQ. startServicePeriod) THEN
    month = NonrecurringCost(jCost)%totalMonthsFromStart + monthsBaseToService + 1
  ELSEIF (NonrecurringCost(jCost)%startOfCosts .EQ. startBasePeriod) THEN
    month = NonrecurringCost(jCost)%totalMonthsFromStart + 1
  END IF
  IF ((month .GE. 1) .AND. (month .LE. lengthStudyTotalMonths)) THEN
    CashFlow(offset + jCost)%mnAmount(month) = NonrecurringCost(jCost)%cost * monthlyInflationFactor(month)
  ELSE
    CALL ShowWarningError('For life cycle costing a nonrecurring cost named ' // TRIM(NonrecurringCost(jCost)%name) //  &
            ' contains a cost which is not within the study period.')
  END IF
END DO
! Put recurring costs into cashflows
offset = countOfCostCat
DO jCost = 1, numRecurringCosts
  CashFlow(offset + jCost)%name = RecurringCosts(jCost)%name
  CashFlow(offset + jCost)%SourceKind = skRecurring
  CashFlow(offset + jCost)%Category = RecurringCosts(jCost)%category
  CashFlow(offset + jCost)%orginalCost = RecurringCosts(jCost)%cost
  IF (RecurringCosts(jCost)%startOfCosts .EQ. startServicePeriod) THEN
    firstMonth = RecurringCosts(jCost)%totalMonthsFromStart + monthsBaseToService + 1
  ELSEIF (RecurringCosts(jCost)%startOfCosts .EQ. startBasePeriod) THEN
    firstMonth = RecurringCosts(jCost)%totalMonthsFromStart + 1
  END IF
  IF ((firstMonth .GE. 1) .AND. (firstMonth .LE. lengthStudyTotalMonths)) THEN
    month = firstMonth
    IF (RecurringCosts(jCost)%totalRepeatPeriodMonths .GE. 1) THEN
      DO iLoop = 1,10000 !add a limit to the loop to prevent runaway condition
        CashFlow(offset + jCost)%mnAmount(month) = RecurringCosts(jCost)%cost * monthlyInflationFactor(month)
        month = month + RecurringCosts(jCost)%totalRepeatPeriodMonths
        IF (month .GT. lengthStudyTotalMonths) EXIT
      END DO
    END IF
  ELSE
    CALL ShowWarningError('For life cycle costing the recurring cost named ' // TRIM(RecurringCosts(jCost)%name) //   &
                  ' has the first year of the costs that is not within the study period.')
  END IF
END DO
! Put resource costs into cashflows
! the first cash flow for resources should be after the categories, recurring and nonrecurring costs
cashFlowCounter = countOfCostCat + numRecurringCosts + numNonrecurringCost
DO iResource = 1 , NumOfResourceTypes
  IF (resourceCostNotZero(iResource)) THEN
    cashFlowCounter = cashFlowCounter + 1
    CashFlow(cashFlowCounter)%Category = costCatEnergy
    CashFlow(cashFlowCounter)%Resource = iResource + ResourceTypeInitialOffset
    CashFlow(cashFlowCounter)%SourceKind = skResource
    CashFlow(cashFlowCounter)%name = GetResourceTypeChar(iResource + ResourceTypeInitialOffset)
    IF (cashFlowCounter .LE. numCashFlow) THEN
      !put the monthly energy costs into the cashflow prior to adjustments
      !energy costs (a.k.a. resource costs) start at the start of service and repeat
      !until the end of the study total
      DO jMonth = 1, 12
        CashFlow(cashFlowCounter)%mnAmount(monthsBaseToService + jMonth) = resourceCosts(iResource,jMonth)
      END DO
      CashFlow(cashFlowCounter)%orginalCost = resourceCostAnnual(iResource)
      DO jMonth = monthsBaseToService + 13, lengthStudyTotalMonths
        ! use the cost from a year earlier
        CashFlow(cashFlowCounter)%mnAmount(jMonth) = CashFlow(cashFlowCounter)%mnAmount(jMonth - 12)
      END DO
      ! add in the impact of inflation
      DO jMonth = 1, lengthStudyTotalMonths
        CashFlow(cashFlowCounter)%mnAmount(jMonth) = CashFlow(cashFlowCounter)%mnAmount(jMonth) * monthlyInflationFactor(jMonth)
      END DO
      ! now factor in adjustments
      ! need to find the correct adjustment to use for the current resource
      found = 0
      DO jAdj = 1,numUseAdjustment
        IF (UseAdjustment(jAdj)%resource .EQ. iResource + ResourceTypeInitialOffset) THEN
          found = jAdj
          EXIT
        END IF
      END DO
      ! if any adjustments were found for that resource apply the multiplier
      IF (found .NE. 0) THEN
        DO kYear = 1,lengthStudyYears !if service period is later than base period then this will go too far
          DO jMonth = 1,12
            month = (kYear - 1) * 12 + jMonth
            IF (month .GT. lengthStudyTotalMonths) EXIT
            CashFlow(cashFlowCounter)%mnAmount(month) = CashFlow(cashFlowCounter)%mnAmount(month) *   &
               UseAdjustment(found)%Adjustment(kYear)
          END DO
        END DO
      END IF
    END IF
  END IF
END DO
!put cashflows into categories
DO jCost = 1, countOfCostCat
  CashFlow(jCost)%Category = jCost !make each category the type indicated
  CashFlow(jCost)%SourceKind = skSum
END DO
!add the cashflows by category
DO jCost = countOfCostCat + 1, numCashFlow
  curCategory = CashFlow(jCost)%Category
  IF ((curCategory .LE. countOfCostCat) .AND. (curCategory .GE. 1)) THEN
    DO jMonth = 1, lengthStudyTotalMonths
      CashFlow(curCategory)%mnAmount(jMonth) = CashFlow(curCategory)%mnAmount(jMonth) + CashFlow(jCost)%mnAmount(jMonth)
    END DO
  END IF
END DO
!create total categories
DO jMonth = 1, lengthStudyTotalMonths
  CashFlow(costCatTotEnergy)%mnAmount(jMonth) = CashFlow(costCatEnergy)%mnAmount(jMonth)
  CashFlow(costCatTotOper)%mnAmount(jMonth) = CashFlow(costCatMaintenance)%mnAmount(jMonth) + &
                                            CashFlow(costCatRepair)%mnAmount(jMonth) + &
                                            CashFlow(costCatOperation)%mnAmount(jMonth) + &
                                            CashFlow(costCatReplacement)%mnAmount(jMonth) + &
                                            CashFlow(costCatMinorOverhaul)%mnAmount(jMonth) + &
                                            CashFlow(costCatMajorOverhaul)%mnAmount(jMonth) + &
                                            CashFlow(costCatOtherOperational)%mnAmount(jMonth) + &
                                            CashFlow(costCatWater)%mnAmount(jMonth) + &
                                            CashFlow(costCatEnergy)%mnAmount(jMonth)
  CashFlow(costCatTotCaptl)%mnAmount(jMonth) = CashFlow(costCatConstruction)%mnAmount(jMonth) + &
                                            CashFlow(costCatSalvage)%mnAmount(jMonth) + &
                                            CashFlow(costCatOtherCapital)%mnAmount(jMonth)
  CashFlow(costCatTotGrand)%mnAmount(jMonth) = CashFlow(costCatTotOper)%mnAmount(jMonth) + &
                                            CashFlow(costCatTotCaptl)%mnAmount(jMonth)
END DO
!convert all monthly cashflows into yearly cashflows
DO jCost = 1,numCashFlow
  DO kYear = 1,lengthStudyYears
    annualCost = 0.0d0
    DO jMonth = 1, 12
      month = (kYear - 1) * 12 + jMonth
      IF (month .LE. lengthStudyTotalMonths) THEN
        annualCost = annualCost + CashFlow(jCost)%mnAmount(month)
      END IF
    END DO
    CashFlow(jCost)%yrAmount(kYear) = annualCost
  END DO
END DO
END SUBROUTINE ExpressAsCashFlows

SUBROUTINE ComputePresentValue
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   August 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    For each cashflow, compute the present value

          ! METHODOLOGY EMPLOYED:

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
REAL(r64) :: totalPV
INTEGER :: curCategory
INTEGER :: curResource
REAL(r64) :: curDiscountRate
INTEGER :: iCashFlow
INTEGER :: jYear
INTEGER :: kResource
INTEGER :: nUsePriceEsc
REAL(r64) :: effectiveYear

! identify how each cashflow should be treated
DO iCashFlow = 1, numCashFlow
  SELECT CASE (CashFlow(iCashFlow)%SourceKind)
    CASE (skResource)
      !only for real fuels purchased such as electricity, natural gas, etc..
      IF ((CashFlow(iCashFlow)%Resource) .GE. iRT_Electricity .AND. (CashFlow(iCashFlow)%Resource  .LE. iRT_ResidualOil)) THEN
        CashFlow(iCashFlow)%pvKind = pvkEnergy
      ELSE
        CashFlow(iCashFlow)%pvKind = pvkNonEnergy
      END IF
    CASE (skRecurring, skNonrecurring)
      IF (CashFlow(iCashFlow)%Category .EQ. costCatEnergy) THEN
        CashFlow(iCashFlow)%pvKind = pvkEnergy
      ELSE
        CashFlow(iCashFlow)%pvKind = pvkNonEnergy
      END IF
    CASE (skSum)
      CashFlow(iCashFlow)%pvKind = pvkNotComputed
    CASE DEFAULT
      CashFlow(iCashFlow)%pvKind = pvkNotComputed
  END SELECT
END DO
! compute the Single Present Value factors based on the discount rate
ALLOCATE(SPV(lengthStudyYears))
ALLOCATE(energySPV(NumOfResourceTypes,lengthStudyYears))
! Depending if using Constant or Current Dollar analysis
! use the appropriate discount rate
IF (inflationApproach .EQ. inflAppConstantDollar) THEN
  curDiscountRate = realDiscountRate
ELSEIF (inflationApproach .EQ. inflAppCurrentDollar) THEN
  curDiscountRate = nominalDiscountRate
END IF
!compute single present values based on real discount rates
DO jYear = 1, lengthStudyYears
  ! NIST 155 D.2.1.1 - Single Present Value (SPV) formula
  SELECT CASE (discountConvension)
    CASE (disConvBeginOfYear)
      effectiveYear = REAL(jYear,r64) - 1.0d0
    CASE (disConvMidYear)
      effectiveYear = REAL(jYear,r64) - 0.5d0
    CASE (disConvEndOfYear)
      effectiveYear = REAL(jYear,r64)
    CASE DEFAULT
  END SELECT
  SPV(jYear) = 1.0d0 / ((1.0d0 + curDiscountRate) ** effectiveYear)
END DO
!use SPV as default values for all energy types
DO jYear = 1, lengthStudyYears
  DO kResource = 1,NumOfResourceTypes
    energySPV(kResource,jYear) = SPV(jYear)
  END DO
END DO
!loop through the resources and if they match a UseEscalation use those values instead
DO nUsePriceEsc = 1,numUsePriceEscalation
  curResource = UsePriceEscalation(nUsePriceEsc)%resource - ResourceTypeInitialOffset
  IF ((curResource .GE. 1) .AND. (curResource .LT. NumOfResourceTypes)) THEN
    DO jYear = 1, lengthStudyYears
      !the following is based on UPV* formula from NIST 135 supplement but is for a single year
      SELECT CASE (discountConvension)
        CASE (disConvBeginOfYear)
          effectiveYear = REAL(jYear,r64) - 1.0d0
        CASE (disConvMidYear)
          effectiveYear = REAL(jYear,r64) - 0.5d0
        CASE (disConvEndOfYear)
          effectiveYear = REAL(jYear,r64)
        CASE DEFAULT
      END SELECT
      energySPV(curResource,jYear) = UsePriceEscalation(nUsePriceEsc)%Escalation(jYear) /   &
         ((1.0d0 + curDiscountRate) ** effectiveYear)
    END DO
  END IF
END DO
DO iCashFlow = 1, numCashFlow
  SELECT CASE (CashFlow(iCashFlow)%pvKind)
    CASE (pvkNonEnergy)
      totalPV = 0.0d0
      DO jYear = 1,lengthStudyYears
        CashFlow(iCashFlow)%yrPresVal(jYear) = CashFlow(iCashFlow)%yrAmount(jYear) * SPV(jYear)
        totalPV = totalPV + CashFlow(iCashFlow)%yrPresVal(jYear)
      END DO
      CashFlow(iCashFlow)%presentValue = totalPV
    CASE (pvkEnergy)
      curResource = CashFlow(iCashFlow)%Resource - ResourceTypeInitialOffset
      IF ((curResource .GE. 1) .AND. (curResource .LT. NumOfResourceTypes)) THEN
        totalPV = 0.0d0
        DO jYear = 1,lengthStudyYears
          CashFlow(iCashFlow)%yrPresVal(jYear) = CashFlow(iCashFlow)%yrAmount(jYear) * energySPV(curResource,jYear)
          totalPV = totalPV + CashFlow(iCashFlow)%yrPresVal(jYear)
        END DO
        CashFlow(iCashFlow)%presentValue = totalPV
      END IF
    CASE (pvkNotComputed)
      ! do nothing
  END SELECT
END DO
! sum by category
DO iCashFlow = countOfCostCat + 1, numCashFlow
  curCategory = CashFlow(iCashFlow)%Category
  IF ((curCategory .LE. countOfCostCat) .AND. (curCategory .GE. 1)) THEN
    CashFlow(curCategory)%presentValue = CashFlow(curCategory)%presentValue + CashFlow(iCashFlow)%presentValue
    DO jYear = 1,lengthStudyYears
      CashFlow(curCategory)%yrPresVal(jYear) = CashFlow(curCategory)%yrPresVal(jYear) + CashFlow(iCashFlow)%yrPresVal(jYear)
    END DO
  END IF
END DO
!create total categories
CashFlow(costCatTotEnergy)%presentValue = CashFlow(costCatEnergy)%presentValue
CashFlow(costCatTotOper)%presentValue = CashFlow(costCatMaintenance)%presentValue + &
                                            CashFlow(costCatRepair)%presentValue + &
                                            CashFlow(costCatOperation)%presentValue + &
                                            CashFlow(costCatReplacement)%presentValue + &
                                            CashFlow(costCatMinorOverhaul)%presentValue + &
                                            CashFlow(costCatMajorOverhaul)%presentValue + &
                                            CashFlow(costCatOtherOperational)%presentValue + &
                                            CashFlow(costCatWater)%presentValue + &
                                            CashFlow(costCatEnergy)%presentValue
CashFlow(costCatTotCaptl)%presentValue = CashFlow(costCatConstruction)%presentValue + &
                                            CashFlow(costCatSalvage)%presentValue + &
                                            CashFlow(costCatOtherCapital)%presentValue
CashFlow(costCatTotGrand)%presentValue = CashFlow(costCatTotOper)%presentValue + &
                                            CashFlow(costCatTotCaptl)%presentValue
DO jYear = 1,lengthStudyYears
  CashFlow(costCatTotEnergy)%yrPresVal(jYear) = CashFlow(costCatEnergy)%yrPresVal(jYear)
  CashFlow(costCatTotOper)%yrPresVal(jYear) = CashFlow(costCatMaintenance)%yrPresVal(jYear) + &
                                            CashFlow(costCatRepair)%yrPresVal(jYear) + &
                                            CashFlow(costCatOperation)%yrPresVal(jYear) + &
                                            CashFlow(costCatReplacement)%yrPresVal(jYear) + &
                                            CashFlow(costCatMinorOverhaul)%yrPresVal(jYear) + &
                                            CashFlow(costCatMajorOverhaul)%yrPresVal(jYear) + &
                                            CashFlow(costCatOtherOperational)%yrPresVal(jYear) + &
                                            CashFlow(costCatWater)%yrPresVal(jYear) + &
                                            CashFlow(costCatEnergy)%yrPresVal(jYear)
  CashFlow(costCatTotCaptl)%yrPresVal(jYear) = CashFlow(costCatConstruction)%yrPresVal(jYear) + &
                                            CashFlow(costCatSalvage)%yrPresVal(jYear) + &
                                            CashFlow(costCatOtherCapital)%yrPresVal(jYear)
  CashFlow(costCatTotGrand)%yrPresVal(jYear) = CashFlow(costCatTotOper)%yrPresVal(jYear) + &
                                            CashFlow(costCatTotCaptl)%yrPresVal(jYear)
END DO

END SUBROUTINE ComputePresentValue

SUBROUTINE ComputeTaxAndDepreciation
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   August 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Compute the present value after factoring in taxes
          !    and depreciation.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: SizeDepr =  41

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64),DIMENSION(SizeDepr) :: DepreciationPercent !values expressed as percent 5% is 5.0 (based on tables)
REAL(r64) :: curCapital
INTEGER :: curDepYear
INTEGER :: iYear
INTEGER :: jYear

ALLOCATE(DepreciatedCapital(lengthStudyYears))
ALLOCATE(TaxableIncome(lengthStudyYears))
ALLOCATE(Taxes(lengthStudyYears))
ALLOCATE(AfterTaxCashFlow(lengthStudyYears))
ALLOCATE(AfterTaxPresentValue(lengthStudyYears))

! Depreciation factors are based on IRS Publication 946 for 2009 "How to Depreciate Property"
! The MACRS valus are based on Modified Accelerated Cost Recovery System GDS for 3, 5, 7, 10 year
! property are based on 200% depreciation method shown in Appendix A using half year. 15 and 20 are
! based on 150% (Chart 1). For Straight Line depreciation GDS is used for 27 years (actually 27.5)
! 31 years (actually 31.5 years) and 39 years using mid month. For 40 years ADS is used (chart 2)
! Table A-1 is used for 3, 4, 5, 10, 15 and 20 years. Table A-6 is for 27 years. Table A-7 for 31 years.
! Table A-7a for 39 years. Table A-13 for 40 years. These years are a classification of property
! and should not be confused with the length of the study. For 27 years, 31 years, 39 years and 40 years
! the June value was used.
DepreciationPercent = 0.0d0 !default all values to zero
SELECT CASE (depreciationMethod)
  CASE(depMethMACRS3)  ! IRS Publication 946 for 2009 Table A-1
    DepreciationPercent(1) = 33.33d0
    DepreciationPercent(2) = 44.45d0
    DepreciationPercent(3) = 14.81d0
    DepreciationPercent(4) = 7.41d0
  CASE(depMethMACRS5)  ! IRS Publication 946 for 2009 Table A-1
    DepreciationPercent(1) = 20.0d0
    DepreciationPercent(2) = 32.0d0
    DepreciationPercent(3) = 19.2d0
    DepreciationPercent(4) = 11.52d0
    DepreciationPercent(5) = 11.52d0
    DepreciationPercent(6) = 5.76d0
  CASE(depMethMACRS7)  ! IRS Publication 946 for 2009 Table A-1
    DepreciationPercent(1) = 14.29d0
    DepreciationPercent(2) = 24.49d0
    DepreciationPercent(3) = 17.49d0
    DepreciationPercent(4) = 12.49d0
    DepreciationPercent(5) = 8.93d0
    DepreciationPercent(6) = 8.92d0
    DepreciationPercent(7) = 8.93d0
    DepreciationPercent(8) = 4.46d0
  CASE(depMethMACRS10)  ! IRS Publication 946 for 2009 Table A-1
    DepreciationPercent(1) = 10.0d0
    DepreciationPercent(2) = 18.0d0
    DepreciationPercent(3) = 14.4d0
    DepreciationPercent(4) = 11.52d0
    DepreciationPercent(5) = 9.22d0
    DepreciationPercent(6) = 7.37d0
    DepreciationPercent(7) = 6.55d0
    DepreciationPercent(8) = 6.55d0
    DepreciationPercent(9) = 6.56d0
    DepreciationPercent(10) = 6.55d0
    DepreciationPercent(11) = 3.28d0
  CASE(depMethMACRS15)  ! IRS Publication 946 for 2009 Table A-1
    DepreciationPercent(1) = 5.0d0
    DepreciationPercent(2) = 9.5d0
    DepreciationPercent(3) = 8.55d0
    DepreciationPercent(4) = 7.7d0
    DepreciationPercent(5) = 6.93d0
    DepreciationPercent(6) = 6.23d0
    DepreciationPercent(7) = 5.9d0
    DepreciationPercent(8) = 5.9d0
    DepreciationPercent(9) = 5.91d0
    DepreciationPercent(10) = 5.9d0
    DepreciationPercent(11) = 5.91d0
    DepreciationPercent(12) = 5.9d0
    DepreciationPercent(13) = 5.91d0
    DepreciationPercent(14) = 5.9d0
    DepreciationPercent(15) = 5.91d0
    DepreciationPercent(16) = 2.95d0
  CASE(depMethMACRS20)  ! IRS Publication 946 for 2009 Table A-1
    DepreciationPercent(1) = 3.75d0
    DepreciationPercent(2) = 7.219d0
    DepreciationPercent(3) = 6.677d0
    DepreciationPercent(4) = 6.177d0
    DepreciationPercent(5) = 5.713d0
    DepreciationPercent(6) = 5.285d0
    DepreciationPercent(7) = 4.888d0
    DepreciationPercent(8) = 4.522d0
    DepreciationPercent(9) = 4.462d0
    DepreciationPercent(10) = 4.461d0
    DepreciationPercent(11) = 4.462d0
    DepreciationPercent(12) = 4.461d0
    DepreciationPercent(13) = 4.462d0
    DepreciationPercent(14) = 4.461d0
    DepreciationPercent(15) = 4.462d0
    DepreciationPercent(16) = 4.461d0
    DepreciationPercent(17) = 4.462d0
    DepreciationPercent(18) = 4.461d0
    DepreciationPercent(19) = 4.462d0
    DepreciationPercent(20) = 4.461d0
    DepreciationPercent(21) = 2.231d0
  CASE(depMethStraight27)  ! IRS Publication 946 for 2009 Table A-6 (June)
    DepreciationPercent(1) = 1.97d0
    DepreciationPercent(2) = 3.636d0
    DepreciationPercent(3) = 3.636d0
    DepreciationPercent(4) = 3.636d0
    DepreciationPercent(5) = 3.636d0
    DepreciationPercent(6) = 3.636d0
    DepreciationPercent(7) = 3.636d0
    DepreciationPercent(8) = 3.636d0
    DepreciationPercent(9) = 3.636d0
    DepreciationPercent(10) = 3.637d0
    DepreciationPercent(11) = 3.636d0
    DepreciationPercent(12) = 3.637d0
    DepreciationPercent(13) = 3.636d0
    DepreciationPercent(14) = 3.637d0
    DepreciationPercent(15) = 3.636d0
    DepreciationPercent(16) = 3.637d0
    DepreciationPercent(17) = 3.636d0
    DepreciationPercent(18) = 3.637d0
    DepreciationPercent(19) = 3.636d0
    DepreciationPercent(20) = 3.637d0
    DepreciationPercent(21) = 3.636d0
    DepreciationPercent(22) = 3.637d0
    DepreciationPercent(23) = 3.636d0
    DepreciationPercent(24) = 3.637d0
    DepreciationPercent(25) = 3.636d0
    DepreciationPercent(26) = 3.637d0
    DepreciationPercent(27) = 3.636d0
    DepreciationPercent(28) = 3.485d0
  CASE(depMethStraight31)  ! IRS Publication 946 for 2009 Table A-7 (June)
    DepreciationPercent(1) = 1.72d0
    DepreciationPercent(2) = 3.175d0
    DepreciationPercent(3) = 3.175d0
    DepreciationPercent(4) = 3.175d0
    DepreciationPercent(5) = 3.175d0
    DepreciationPercent(6) = 3.175d0
    DepreciationPercent(7) = 3.175d0
    DepreciationPercent(8) = 3.174d0
    DepreciationPercent(9) = 3.175d0
    DepreciationPercent(10) = 3.174d0
    DepreciationPercent(11) = 3.175d0
    DepreciationPercent(12) = 3.174d0
    DepreciationPercent(13) = 3.175d0
    DepreciationPercent(14) = 3.174d0
    DepreciationPercent(15) = 3.175d0
    DepreciationPercent(16) = 3.174d0
    DepreciationPercent(17) = 3.175d0
    DepreciationPercent(18) = 3.174d0
    DepreciationPercent(19) = 3.175d0
    DepreciationPercent(20) = 3.174d0
    DepreciationPercent(21) = 3.175d0
    DepreciationPercent(22) = 3.174d0
    DepreciationPercent(23) = 3.175d0
    DepreciationPercent(24) = 3.174d0
    DepreciationPercent(25) = 3.175d0
    DepreciationPercent(26) = 3.174d0
    DepreciationPercent(27) = 3.175d0
    DepreciationPercent(28) = 3.174d0
    DepreciationPercent(29) = 3.175d0
    DepreciationPercent(30) = 3.174d0
    DepreciationPercent(31) = 3.175d0
    DepreciationPercent(32) = 3.042d0
  CASE(depMethStraight39)  ! IRS Publication 946 for 2009 Table A-7a (June)
    DepreciationPercent(1) = 1.391d0
    DepreciationPercent(2) = 2.564d0
    DepreciationPercent(3) = 2.564d0
    DepreciationPercent(4) = 2.564d0
    DepreciationPercent(5) = 2.564d0
    DepreciationPercent(6) = 2.564d0
    DepreciationPercent(7) = 2.564d0
    DepreciationPercent(8) = 2.564d0
    DepreciationPercent(9) = 2.564d0
    DepreciationPercent(10) = 2.564d0
    DepreciationPercent(11) = 2.564d0
    DepreciationPercent(12) = 2.564d0
    DepreciationPercent(13) = 2.564d0
    DepreciationPercent(14) = 2.564d0
    DepreciationPercent(15) = 2.564d0
    DepreciationPercent(16) = 2.564d0
    DepreciationPercent(17) = 2.564d0
    DepreciationPercent(18) = 2.564d0
    DepreciationPercent(19) = 2.564d0
    DepreciationPercent(20) = 2.564d0
    DepreciationPercent(21) = 2.564d0
    DepreciationPercent(22) = 2.564d0
    DepreciationPercent(23) = 2.564d0
    DepreciationPercent(24) = 2.564d0
    DepreciationPercent(25) = 2.564d0
    DepreciationPercent(26) = 2.564d0
    DepreciationPercent(27) = 2.564d0
    DepreciationPercent(28) = 2.564d0
    DepreciationPercent(29) = 2.564d0
    DepreciationPercent(30) = 2.564d0
    DepreciationPercent(31) = 2.564d0
    DepreciationPercent(32) = 2.564d0
    DepreciationPercent(33) = 2.564d0
    DepreciationPercent(34) = 2.564d0
    DepreciationPercent(35) = 2.564d0
    DepreciationPercent(36) = 2.564d0
    DepreciationPercent(37) = 2.564d0
    DepreciationPercent(38) = 2.564d0
    DepreciationPercent(39) = 2.564d0
    DepreciationPercent(40) = 1.177d0
  CASE(depMethStraight40)  ! IRS Publication 946 for 2009 Table A-13 (June)
    DepreciationPercent(1) = 1.354d0
    DepreciationPercent(2) = 2.5d0
    DepreciationPercent(3) = 2.5d0
    DepreciationPercent(4) = 2.5d0
    DepreciationPercent(5) = 2.5d0
    DepreciationPercent(6) = 2.5d0
    DepreciationPercent(7) = 2.5d0
    DepreciationPercent(8) = 2.5d0
    DepreciationPercent(9) = 2.5d0
    DepreciationPercent(10) = 2.5d0
    DepreciationPercent(11) = 2.5d0
    DepreciationPercent(12) = 2.5d0
    DepreciationPercent(13) = 2.5d0
    DepreciationPercent(14) = 2.5d0
    DepreciationPercent(15) = 2.5d0
    DepreciationPercent(16) = 2.5d0
    DepreciationPercent(17) = 2.5d0
    DepreciationPercent(18) = 2.5d0
    DepreciationPercent(19) = 2.5d0
    DepreciationPercent(20) = 2.5d0
    DepreciationPercent(21) = 2.5d0
    DepreciationPercent(22) = 2.5d0
    DepreciationPercent(23) = 2.5d0
    DepreciationPercent(24) = 2.5d0
    DepreciationPercent(25) = 2.5d0
    DepreciationPercent(26) = 2.5d0
    DepreciationPercent(27) = 2.5d0
    DepreciationPercent(28) = 2.5d0
    DepreciationPercent(29) = 2.5d0
    DepreciationPercent(30) = 2.5d0
    DepreciationPercent(31) = 2.5d0
    DepreciationPercent(32) = 2.5d0
    DepreciationPercent(33) = 2.5d0
    DepreciationPercent(34) = 2.5d0
    DepreciationPercent(35) = 2.5d0
    DepreciationPercent(36) = 2.5d0
    DepreciationPercent(37) = 2.5d0
    DepreciationPercent(38) = 2.5d0
    DepreciationPercent(39) = 2.5d0
    DepreciationPercent(40) = 2.5d0
    DepreciationPercent(41) = 1.146d0
END SELECT
! convert construction costs (not salvage) into depreciation
DepreciatedCapital = 0.0d0 ! set all years to zero
DO iYear = 1, lengthStudyYears
  curCapital = CashFlow(costCatConstruction)%yrAmount(iYear) + CashFlow(costCatOtherCapital)%yrAmount(iYear)
  DO jYear = 1, SizeDepr
    curDepYear = iYear + jYear - 1 !start depreciating with the year that the capital was shown and go to years following
    IF (curDepYear .LE. lengthStudyYears) THEN
      DepreciatedCapital(curDepYear) = DepreciatedCapital(curDepYear) + curCapital * (DepreciationPercent(jYear) / 100)
    END IF
  END DO
END DO
! Using Newnan pg 3880
!   before-tax cash flow
!   depreciation
!   taxable income (before-tax cash flow - depreciation)
!   income taxes (taxable income x incremental tax rate)
!   after-tax cash flow (before-tax cash flow - income taxes)
DO iYear = 1, lengthStudyYears
  TaxableIncome(iYear) = CashFlow(costCatTotGrand)%yrAmount(iYear) - DepreciatedCapital(iYear)
  Taxes(iYear) = TaxableIncome(iYear) * taxRate
  AfterTaxCashFlow(iYear) = CashFlow(costCatTotGrand)%yrAmount(iYear) - Taxes(iYear)
  ! the present value after taxes is pretax present value minus the present value of the taxes
  AfterTaxPresentValue(iYear) = CashFlow(costCatTotGrand)%yrPresVal(iYear) - Taxes(iYear) * SPV(iYear)
END DO
END SUBROUTINE ComputeTaxAndDepreciation


!======================================================================================================================
!======================================================================================================================
!
!
!    OUTPUT ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE WriteTabularLifeCycleCostReport
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   June 2010
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Write the output report related to life-cycle costing
          !    to the tabular output file.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportTabular, ONLY: WriteReportHeaders, WriteSubtitle, WriteTable, RealToStr, IntToStr
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

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
! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody

INTEGER :: month
INTEGER :: numColumns
INTEGER :: iYear
INTEGER :: jObj
INTEGER :: kMonth
INTEGER :: curCashFlow
INTEGER :: numRows
INTEGER :: offset
INTEGER :: numYears
REAL(r64) :: totalPV

IF (LCCparamPresent) THEN
  !---------------------------------
  ! Life-Cycle Cost Verification and Results Report
  !---------------------------------
  CALL WriteReportHeaders('Life-Cycle Cost Report','Entire Facility',1)
  !
  !---- Life-Cycle Cost Parameters
  !
  ALLOCATE(rowHead(11))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  ALLOCATE(tableBody(11,1))
  tableBody = ''
  rowHead(1) = 'Name'
  rowHead(2) = 'Discounting Convention'
  rowHead(3) = 'Inflation Approach'
  rowHead(4) = 'Real Discount Rate'
  rowHead(5) = 'Nominal Discount Rate'
  rowHead(6) = 'Inflation'
  rowHead(7) = 'Base Date'
  rowHead(8) = 'Service Date'
  rowHead(9) = 'Length of Study Period in Years'
  rowHead(10) = 'Tax rate'
  rowHead(11) = 'Depreciation Method'
  columnHead(1) = 'Value'

  tableBody(1,1) = LCCname
  IF (discountConvension .EQ. disConvEndOfYear) THEN
    tableBody(2,1) = 'EndOfYear'
  ELSEIF (discountConvension .EQ. disConvMidYear) THEN
    tableBody(2,1) = 'MidYear'
  ELSEIF (discountConvension .EQ. disConvBeginOfYear) THEN
    tableBody(2,1) = 'BeginningOfYear'
  ENDIF
  IF (inflationApproach .EQ. inflAppConstantDollar) THEN
    tableBody(3,1) = 'ConstantDollar'
  ELSEIF (inflationApproach .EQ. inflAppCurrentDollar) THEN
    tableBody(3,1) = 'CurrentDollar'
  ENDIF
  IF (inflationApproach .EQ. inflAppConstantDollar) THEN
    tableBody(4,1) = TRIM(RealToStr(realDiscountRate, 4))
  ELSE
    tableBody(4,1) = '-- N/A --'
  END IF
  IF (inflationApproach .EQ. inflAppCurrentDollar) THEN
    tableBody(5,1) = TRIM(RealToStr(nominalDiscountRate, 4))
  ELSE
    tableBody(5,1) = '-- N/A --'
  END IF
  IF (inflationApproach .EQ. inflAppCurrentDollar) THEN
    tableBody(6,1) = TRIM(RealToStr(inflation, 4))
  ELSE
    tableBody(6,1) = '-- N/A --'
  END IF
  tableBody(7,1) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear)
  tableBody(8,1) = MonthNames(serviceDateMonth) // ' ' // IntToStr(serviceDateYear)
  tableBody(9,1) = TRIM(IntToStr(lengthStudyYears))
  tableBody(10,1) = TRIM(RealToStr(taxRate, 4))
  SELECT CASE (depreciationMethod)
    CASE(depMethMACRS3)
      tableBody(11,1) = 'ModifiedAcceleratedCostRecoverySystem-3year'
    CASE(depMethMACRS5)
      tableBody(11,1) = 'ModifiedAcceleratedCostRecoverySystem-5year'
    CASE(depMethMACRS7)
      tableBody(11,1) = 'ModifiedAcceleratedCostRecoverySystem-7year'
    CASE(depMethMACRS10)
      tableBody(11,1) = 'ModifiedAcceleratedCostRecoverySystem-10year'
    CASE(depMethMACRS15)
      tableBody(11,1) = 'ModifiedAcceleratedCostRecoverySystem-15year'
    CASE(depMethMACRS20)
      tableBody(11,1) = 'ModifiedAcceleratedCostRecoverySystem-20year'
    CASE(depMethStraight27)
      tableBody(11,1) = 'StraightLine-27year'
    CASE(depMethStraight31)
      tableBody(11,1) = 'StraightLine-31year'
    CASE(depMethStraight39)
      tableBody(11,1) = 'StraightLine-39year'
    CASE(depMethStraight40)
      tableBody(11,1) = 'StraightLine-40year'
    CASE(depMethNone)
      tableBody(11,1) = 'None'
  END SELECT
  columnWidth = 14 !array assignment - same for all columns
  CALL WriteSubtitle('Life-Cycle Cost Parameters')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Life-Cycle Cost Parameters')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Use Price Escalation
  !
  numColumns = MAX(1,numUsePriceEscalation)
  ALLOCATE(rowHead(lengthStudyYears + 2))
  ALLOCATE(columnHead(numColumns))
  ALLOCATE(columnWidth(numColumns))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears + 2,numColumns))
  tableBody = ''
  columnHead = 'none'
  rowHead(1) = 'Resource'
  rowHead(2) = 'Start Date'
  DO iYear = 1,lengthStudyYears
    rowHead(iYear+2) = IntToStr(iYear)
  END DO
  DO jObj = 1,numUsePriceEscalation !loop through objects not columns to add names
    columnHead(jObj) = UsePriceEscalation(jObj)%name
    tableBody(1,jObj) = TRIM(GetResourceTypeChar(UsePriceEscalation(jObj)%resource))
    tableBody(2,jObj) = MonthNames(UsePriceEscalation(jObj)%escalationStartMonth) // ' ' //   &
       IntToStr(UsePriceEscalation(jObj)%escalationStartYear)
  END DO
  DO jObj = 1,numUsePriceEscalation
    DO iYear = 1,lengthStudyYears
      tableBody(iYear+2,jObj) = TRIM(RealToStr(UsePriceEscalation(jObj)%Escalation(iYear),6))
    END DO
  END DO
  CALL WriteSubtitle('Use Price Escalation')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Use Price Escalation')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Use Adjustment
  !
  IF (numUseAdjustment .GE. 1) THEN !only create table if objects used
    numColumns = MAX(1,numUseAdjustment)
    numYears = lengthStudyYears - (serviceDateYear - BaseDateYear)
    ALLOCATE(rowHead(numYears+1))
    ALLOCATE(columnHead(numColumns))
    ALLOCATE(columnWidth(numColumns))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(numYears+1,numColumns))
    tableBody = ''
    columnHead = 'none'
    rowHead(1) = ''
    DO iYear = 1,numYears
      rowHead(iYear+1) = MonthNames(serviceDateMonth) // ' ' // IntToStr(serviceDateYear + iYear - 1)
    END DO
    DO jObj = 1,numUseAdjustment !loop through objects not columns to add names
      columnHead(jObj) = UseAdjustment(jObj)%Name
      tableBody(1,jObj) = TRIM(GetResourceTypeChar(UseAdjustment(jObj)%resource))
    END DO
    DO jObj = 1,numUseAdjustment
      DO iYear = 1,numYears
        tableBody(iYear + 1,jObj) = TRIM(RealToStr(UseAdjustment(jObj)%Adjustment(iYear),6))
      END DO
    END DO
    CALL WriteSubtitle('Use Adjustment')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'Life-Cycle Cost Report',&
                                        'Entire Facility',&
                                        'Use Adjustment')
    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)
  END IF
  !
  !---- Cash Flow for Recurring and Nonrecurring Costs
  !
  numColumns = MAX(1,numRecurringCosts + numNonrecurringCost)
  ALLOCATE(rowHead(lengthStudyYears + 1))
  ALLOCATE(columnHead(numColumns))
  ALLOCATE(columnWidth(numColumns))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears + 1,numColumns))
  tableBody = ''
  rowHead(1) = ''
  DO iYear = 1,lengthStudyYears
    rowHead(iYear+1) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear + iYear - 1)
  END DO
  DO jObj = 1,(numRecurringCosts + numNonrecurringCost)
    curCashFlow = countOfCostCat + jObj
    columnHead(jObj) = CashFlow(curCashFlow)%Name
    SELECT CASE (CashFlow(curCashFlow)%SourceKind)
      CASE (skNonrecurring)
        tableBody(1,jObj) = 'Nonrecurring'
      CASE (skRecurring)
        tableBody(1,jObj) = 'Recurring'
    END SELECT
    DO iYear = 1,lengthStudyYears
       tableBody(iYear + 1,jObj) = TRIM(RealToStr(CashFlow(curCashFlow)%yrAmount(iYear),2))
    END DO
  END DO
  CALL WriteSubtitle('Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Cash Flow for Recurring and Nonrecurring Costs (Without Escalation)')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Energy Cost Cash Flows
  !
  numColumns = MAX(1,numResourcesUsed)
  ALLOCATE(rowHead(lengthStudyYears))
  ALLOCATE(columnHead(numColumns))
  ALLOCATE(columnWidth(numColumns))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears ,numColumns))
  tableBody = ''
  DO iYear = 1,lengthStudyYears
    rowHead(iYear) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear + iYear - 1)
  END DO
  DO jObj = 1,numResourcesUsed
    curCashFlow = countOfCostCat + numRecurringCosts + numNonrecurringCost + jObj
    columnHead(jObj) = CashFlow(curCashFlow)%Name
    DO iYear = 1,lengthStudyYears
      tableBody(iYear,jObj) = TRIM(RealToStr(CashFlow(curCashFlow)%yrAmount(iYear),2))
    END DO
  END DO
  CALL WriteSubtitle('Energy Cost Cash Flows (Without Escalation)')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Energy Cost Cash Flows (Without Escalation)')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Capital Cash Flow by Category
  !
  ALLOCATE(rowHead(lengthStudyYears))
  ALLOCATE(columnHead(4))
  ALLOCATE(columnWidth(4))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears ,4))
  tableBody = ''
  columnHead(1) = 'Construction'
  columnHead(2) = 'Salvage'
  columnHead(3) = 'OtherCapital'
  columnHead(4) = 'Total'
  DO iYear = 1,lengthStudyYears
    rowHead(iYear) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear + iYear - 1)
    tableBody(iYear,1) = TRIM(RealToStr(CashFlow(costCatConstruction)%yrAmount(iYear),2))
    tableBody(iYear,2) = TRIM(RealToStr(CashFlow(costCatSalvage)%yrAmount(iYear),2))
    tableBody(iYear,3) = TRIM(RealToStr(CashFlow(costCatOtherCapital)%yrAmount(iYear),2))
    tableBody(iYear,4) = TRIM(RealToStr(CashFlow(costCatTotCaptl)%yrAmount(iYear),2))
  END DO
  CALL WriteSubtitle('Capital Cash Flow by Category (Without Escalation)')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Capital Cash Flow by Category (Without Escalation)')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Operating Cash Flow by Category
  !
  ALLOCATE(rowHead(lengthStudyYears))
  ALLOCATE(columnHead(10))
  ALLOCATE(columnWidth(10))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears ,10))
  tableBody = ''
  columnHead(1) = 'Energy'
  columnHead(2) = 'Water'
  columnHead(3) = 'Maintenance'
  columnHead(4) = 'Repair'
  columnHead(5) = 'Operation'
  columnHead(6) = 'Replacement'
  columnHead(7) = 'MinorOverhaul'
  columnHead(8) = 'MajorOverhaul'
  columnHead(9) = 'OtherOperational'
  columnHead(10) = 'Total'

  DO iYear = 1,lengthStudyYears
    rowHead(iYear) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear + iYear - 1)
    tableBody(iYear,1) = TRIM(RealToStr(CashFlow(costCatEnergy)%yrAmount(iYear),2))
    tableBody(iYear,2) = TRIM(RealToStr(CashFlow(costCatWater)%yrAmount(iYear),2))
    tableBody(iYear,3) = TRIM(RealToStr(CashFlow(costCatMaintenance)%yrAmount(iYear),2))
    tableBody(iYear,4) = TRIM(RealToStr(CashFlow(costCatRepair)%yrAmount(iYear),2))
    tableBody(iYear,5) = TRIM(RealToStr(CashFlow(costCatOperation)%yrAmount(iYear),2))
    tableBody(iYear,6) = TRIM(RealToStr(CashFlow(costCatReplacement)%yrAmount(iYear),2))
    tableBody(iYear,7) = TRIM(RealToStr(CashFlow(costCatMinorOverhaul)%yrAmount(iYear),2))
    tableBody(iYear,8) = TRIM(RealToStr(CashFlow(costCatMajorOverhaul)%yrAmount(iYear),2))
    tableBody(iYear,9) = TRIM(RealToStr(CashFlow(costCatOtherOperational)%yrAmount(iYear),2))
    tableBody(iYear,10) = TRIM(RealToStr(CashFlow(costCatTotOper)%yrAmount(iYear),2))
  END DO
  CALL WriteSubtitle('Operating Cash Flow by Category (Without Escalation)')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Operating Cash Flow by Category (Without Escalation)')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- DEBUG ONLY - Monthly Cash Flows
  !
  ! This table is not usually produced but was used as a debugging aid. The code
  ! was kept for future debugging efforts related to cashflows but should generally
  ! be commented out.
  !
!  ALLOCATE(rowHead(lengthStudyTotalMonths))
!  ALLOCATE(columnHead(numCashFlow))
!  ALLOCATE(columnWidth(numCashFlow))
!  ALLOCATE(tableBody(lengthStudyTotalMonths,numCashFlow))
!  tableBody = ''
!  columnHead(1) = 'mnt'
!  columnHead(2) = 'rpr'
!  columnHead(3) = 'opr'
!  columnHead(4) = 'repl'
!  columnHead(5) = 'mOvhl'
!  columnHead(6) = 'MOvhl'
!  columnHead(7) = 'oOpr'
!  columnHead(8) = 'cons'
!  columnHead(9) = 'slvg'
!  columnHead(10) = 'oCap'
!  columnHead(11) = 'H20'
!  columnHead(12) = 'ene'
!  columnHead(13) = 'tEne'
!  columnHead(14) = 'tOpr'
!  columnHead(15) = 'tCap'
!  columnHead(16) = 'Totl'
!  DO jObj = countOfCostCat + 1, numCashFlow
!    columnHead(jObj) = CashFlow(jObj)%name
!  END DO
!  DO kMonth = 1,lengthStudyTotalMonths
!    rowHead(kMonth) = MonthNames(1 + MOD((kMonth + baseDateMonth - 2),12)) &
!                      // ' ' // IntToStr(baseDateYear + INT((kMonth - 1) / 12))
!  END DO
!  DO kMonth = 1,lengthStudyTotalMonths
!    DO jObj = 1,numCashFlow
!      tableBody(kMonth,jObj) = TRIM(RealToStr(CashFlow(jObj)%mnAmount(kMonth),2))
!    END DO
!  END DO
!  columnWidth = 14 !array assignment - same for all columns
!  CALL WriteSubtitle('DEBUG ONLY - Monthly Cash Flows')
!  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
!  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
!                                      'Life-Cycle Cost Report',&
!                                      'Entire Facility',&
!                                      'DEBUG ONLY - Monthly Cash Flows')
!  DEALLOCATE(columnHead)
!  DEALLOCATE(rowHead)
!  DEALLOCATE(columnWidth)
!  DEALLOCATE(tableBody)
  !
  !---- Monthly Total Cash Flow
  !
  ALLOCATE(rowHead(lengthStudyYears))
  ALLOCATE(columnHead(12))
  ALLOCATE(columnWidth(12))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears,12))
  tableBody = ''
  DO kMonth = 1,12
    columnHead(kMonth) = MonthNames(kMonth)
  END DO
  DO iYear = 1,lengthStudyYears
    rowHead(iYear) = IntToStr(baseDateYear + iYear - 1)
  END DO
  DO iYear = 1,lengthStudyYears
    DO kMonth = 1,12
      tableBody(iYear,kMonth) = TRIM(RealToStr(CashFlow(costCatTotGrand)%mnAmount((iYear - 1) * 12 + kMonth),2))
    END DO
  END DO
  CALL WriteSubtitle('Monthly Total Cash Flow (Without Escalation)')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Monthly Total Cash Flow (Without Escalation)')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Present Value for Recurring, Nonrecurring and Energy Costs
  !
  numRows = MAX(1,numRecurringCosts + numNonrecurringCost + numResourcesUsed)
  ALLOCATE(rowHead(numRows + 1))
  ALLOCATE(columnHead(5))
  ALLOCATE(columnWidth(5))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(numRows + 1 ,5))
  tableBody = ''
  columnHead(1) = 'Category'
  columnHead(2) = 'Kind'
  columnHead(3) = 'Cost'
  columnHead(4) = 'Present Value'
  columnHead(5) = 'Present Value Factor'
  totalPV = 0.0d0
  rowHead(numRows + 1) = 'TOTAL'
  DO jObj = 1,(numRecurringCosts + numNonrecurringCost + numResourcesUsed)
    offset = countOfCostCat
    rowHead(jObj) = CashFlow(offset + jObj)%name
    SELECT CASE (CashFlow(offset + jObj)%Category)
      CASE (costCatMaintenance)
        tableBody(jObj,1) = 'Maintenance'
      CASE (costCatRepair)
        tableBody(jObj,1) = 'Repair'
      CASE (costCatOperation)
        tableBody(jObj,1) = 'Operation'
      CASE (costCatReplacement)
        tableBody(jObj,1) = 'Replacement'
      CASE (costCatMinorOverhaul)
        tableBody(jObj,1) = 'Minor Overhaul'
      CASE (costCatMajorOverhaul)
        tableBody(jObj,1) = 'Major Overhaul'
      CASE (costCatOtherOperational)
        tableBody(jObj,1) = 'Other Operational'
      CASE (costCatConstruction)
        tableBody(jObj,1) = 'Construction'
      CASE (costCatSalvage)
        tableBody(jObj,1) = 'Salvage'
      CASE (costCatOtherCapital)
        tableBody(jObj,1) = 'Other Capital'
      CASE (costCatWater)
        tableBody(jObj,1) = 'Water'
      CASE (costCatEnergy)
        tableBody(jObj,1) = 'Energy'
      CASE DEFAULT
        tableBody(jObj,1) = '-'
    END SELECT
    SELECT CASE (CashFlow(offset + jObj)%SourceKind)
      CASE (skNonrecurring)
        tableBody(jObj,2) = 'Nonrecurring'
      CASE (skRecurring)
        tableBody(jObj,2) = 'Recurring'
      CASE (skResource)
        tableBody(jObj,2) = 'Energy Cost'
      CASE DEFAULT
        tableBody(jObj,2) = '-'
    END SELECT
    tableBody(jObj,3) = TRIM(RealToStr(CashFlow(offset + jObj)%orginalCost,2))
    tableBody(jObj,4) = TRIM(RealToStr(CashFlow(offset + jObj)%presentValue,2))
    totalPV = totalPV + CashFlow(offset + jObj)%presentValue
    IF (CashFlow(offset + jObj)%orginalCost .NE. 0.0d0) THEN
      tableBody(jObj,5) = TRIM(RealToStr(CashFlow(offset + jObj)%presentValue / CashFlow(offset + jObj)%orginalCost,4))
    ELSE
      tableBody(jObj,5) = '-'
    END IF
  END DO
  tableBody(numRows + 1,4) = TRIM(RealToStr(totalPV,2))
  CALL WriteSubtitle('Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Present Value for Recurring, Nonrecurring and Energy Costs (Before Tax)')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Present Value by Category
  !
  ALLOCATE(rowHead(16))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(16 ,1))
  tableBody = ''
  rowHead(1) = 'Construction'
  rowHead(2) = 'Salvage'
  rowHead(3) = 'Other Capital'
  rowHead(4) = 'Energy'
  rowHead(5) = 'Water'
  rowHead(6) = 'Maintenance'
  rowHead(7) = 'Repair'
  rowHead(8) = 'Operation'
  rowHead(9) = 'Replacement'
  rowHead(10) = 'Minor Overhaul'
  rowHead(11) = 'Major Overhaul'
  rowHead(12) = 'Other Operational'
  rowHead(13) = 'Total Energy'
  rowHead(14) = 'Total Operation'
  rowHead(15) = 'Total Capital'
  rowHead(16) = 'Grand Total'
  columnHead(1) = 'Present Value'

  tableBody(1,1) = TRIM(RealToStr(CashFlow(costCatConstruction)%presentValue,2))
  tableBody(2,1) = TRIM(RealToStr(CashFlow(costCatSalvage)%presentValue,2))
  tableBody(3,1) = TRIM(RealToStr(CashFlow(costCatOtherCapital)%presentValue,2))
  tableBody(4,1) = TRIM(RealToStr(CashFlow(costCatEnergy)%presentValue,2))
  tableBody(5,1) = TRIM(RealToStr(CashFlow(costCatWater)%presentValue,2))
  tableBody(6,1) = TRIM(RealToStr(CashFlow(costCatMaintenance)%presentValue,2))
  tableBody(7,1) = TRIM(RealToStr(CashFlow(costCatRepair)%presentValue,2))
  tableBody(8,1) = TRIM(RealToStr(CashFlow(costCatOperation)%presentValue,2))
  tableBody(9,1) = TRIM(RealToStr(CashFlow(costCatReplacement)%presentValue,2))
  tableBody(10,1) = TRIM(RealToStr(CashFlow(costCatMinorOverhaul)%presentValue,2))
  tableBody(11,1) = TRIM(RealToStr(CashFlow(costCatMajorOverhaul)%presentValue,2))
  tableBody(12,1) = TRIM(RealToStr(CashFlow(costCatOtherOperational)%presentValue,2))
  tableBody(13,1) = TRIM(RealToStr(CashFlow(costCatTotEnergy)%presentValue,2))
  tableBody(14,1) = TRIM(RealToStr(CashFlow(costCatTotOper)%presentValue,2))
  tableBody(15,1) = TRIM(RealToStr(CashFlow(costCatTotCaptl)%presentValue,2))
  tableBody(16,1) = TRIM(RealToStr(CashFlow(costCatTotGrand)%presentValue,2))

  CALL WriteSubtitle('Present Value by Category')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Present Value by Category')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Present Value by Year
  !
  ALLOCATE(rowHead(lengthStudyYears + 1))
  ALLOCATE(columnHead(2))
  ALLOCATE(columnWidth(2))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(lengthStudyYears + 1,2))
  tableBody = ''
  columnHead(1) = 'Total Cost'
  columnHead(2) = 'Present Value of Costs'

  totalPV = 0.0d0
  DO iYear = 1,lengthStudyYears
    rowHead(iYear) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear + iYear - 1)
    tableBody(iYear,1) = TRIM(RealToStr(CashFlow(costCatTotGrand)%yrAmount(iYear),2))
    tableBody(iYear,2) = TRIM(RealToStr(CashFlow(costCatTotGrand)%yrPresVal(iYear),2))
    totalPV = totalPV + CashFlow(costCatTotGrand)%yrPresVal(iYear)
  END DO

  rowHead(lengthStudyYears + 1) = 'TOTAL'
  tableBody(lengthStudyYears + 1,2) = TRIM(RealToStr(totalPV,2))

  CALL WriteSubtitle('Present Value by Year')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Life-Cycle Cost Report',&
                                      'Entire Facility',&
                                      'Present Value by Year')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- After Tax Estimate
  !
  IF (taxRate .NE. 0.0d0) THEN
    ALLOCATE(rowHead(lengthStudyYears + 1))
    ALLOCATE(columnHead(5))
    ALLOCATE(columnWidth(5))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(lengthStudyYears + 1,5))
    tableBody = ''
    columnHead(1) = 'Depreciated Capital'
    columnHead(2) = 'Taxable Income'
    columnHead(3) = 'Income Taxes'
    columnHead(4) = 'After Tax Cash Flow'
    columnHead(5) = 'After Tax Present Value'

    totalPV = 0.0d0
    DO iYear = 1,lengthStudyYears
      rowHead(iYear) = MonthNames(baseDateMonth) // ' ' // IntToStr(baseDateYear + iYear - 1)
      tableBody(iYear,1) = TRIM(RealToStr(DepreciatedCapital(iYear),2))
      tableBody(iYear,2) = TRIM(RealToStr(TaxableIncome(iYear),2))
      tableBody(iYear,3) = TRIM(RealToStr(Taxes(iYear),2))
      tableBody(iYear,4) = TRIM(RealToStr(AfterTaxCashFlow(iYear),2))
      tableBody(iYear,5) = TRIM(RealToStr(AfterTaxPresentValue(iYear),2))
      totalPV = totalPV + AfterTaxPresentValue(iYear)
    END DO

    rowHead(lengthStudyYears + 1) = 'TOTAL'
    tableBody(lengthStudyYears + 1,5) = TRIM(RealToStr(totalPV,2))

    CALL WriteSubtitle('After Tax Estimate')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'Life-Cycle Cost Report',&
                                        'Entire Facility',&
                                        'After Tax Estimate')
    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)
  END IF


END IF
END SUBROUTINE WriteTabularLifeCycleCostReport

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

END MODULE EconomicLifeCycleCost
