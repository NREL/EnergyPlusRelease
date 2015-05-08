MODULE EconomicTariff

! MODULE INFORMATION:
!    AUTHOR         Jason Glazer of GARD Analytics, Inc.
!    DATE WRITTEN   May 2004
!    MODIFIED       na
!    RE-ENGINEERED  na
!
! PURPOSE OF THIS MODULE:
!
!    Compute utility bills for a building based on energy
!    use estimate.
!
! METHODOLOGY EMPLOYED:
!
! REFERENCES:
!    None.
!
! OTHER NOTES:
!
!

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE InputProcessor
USE DataGlobals ,   ONLY : MaxNameLength
USE DataInterfaces, ONLY: ShowSevereError, ShowWarningError, ShowContinueError
USE ScheduleManager, ONLY: GetScheduleIndex

IMPLICIT NONE


PRIVATE

!ECONOMCIS:TARIFF enumerated lists

INTEGER, parameter  ::  kindUnknown               = 0
INTEGER, parameter  ::  kindTariff                = 1
INTEGER, parameter  ::  kindQualify               = 2
INTEGER, parameter  ::  kindChargeSimple          = 3
INTEGER, parameter  ::  kindChargeBlock           = 4
INTEGER, parameter  ::  kindRatchet               = 5
INTEGER, parameter  ::  kindVariable              = 6
INTEGER, parameter  ::  kindComputation           = 7
INTEGER, parameter  ::  kindCategory              = 8
INTEGER, parameter  ::  kindNative                = 9
INTEGER, parameter  ::  kindAssignCompute         = 10

INTEGER, parameter  ::  conversionUSERDEF         = 0
INTEGER, parameter  ::  conversionKWH             = 1
INTEGER, parameter  ::  conversionTHERM           = 2
INTEGER, parameter  ::  conversionMMBTU           = 3  !million btu
INTEGER, parameter  ::  conversionMJ              = 4
INTEGER, parameter  ::  conversionKBTU            = 5
INTEGER, parameter  ::  conversionMCF             = 6  !thousand cubic feet
INTEGER, parameter  ::  conversionCCF             = 7  !hundred cubic feet

CHARACTER(len=*), PARAMETER, DIMENSION(0:7) :: convEneStrings= &
        (/'     ',   &
          'kWh  ',  &
          'Therm',  &
          'MMBtu',  &
          'MJ   ',  &
          'kBtu ',  &
          'MCF  ',  &
          'CCF  '/)
CHARACTER(len=*), PARAMETER, DIMENSION(0:7) :: convDemStrings= &
        (/'     ',  &
          'kW   ',  &
          'Therm',  &
          'MMBtu',  &
          'MJ   ',  &
          'kBtu ',  &
          'MCF  ',  &
          'CCF  '/)

INTEGER, parameter  ::  demandWindowQuarter       = 1
INTEGER, parameter  ::  demandWindowHalf          = 2
INTEGER, parameter  ::  demandWindowHour          = 3
INTEGER, parameter  ::  demandWindowDay           = 4
INTEGER, parameter  ::  demandWindowWeek          = 5

CHARACTER(len=*), PARAMETER, DIMENSION(0:5) :: demWindowStrings= &
        (/'    ',  &
          '/Hr ',  &
          '/Hr ',  &
          '/Hr ',  &
          '/Day',  &
          '/Wk '/)

INTEGER, parameter  ::  buyFromUtility            = 1
INTEGER, parameter  ::  sellToUtility             = 2
INTEGER, parameter  ::  netMetering               = 3


!For several different objects that reference seasons
INTEGER, parameter  ::  seasonWinter             = 1
INTEGER, parameter  ::  seasonSpring             = 2
INTEGER, parameter  ::  seasonSummer             = 3
INTEGER, parameter  ::  seasonFall               = 4
INTEGER, parameter  ::  seasonAnnual             = 5
INTEGER, parameter  ::  seasonMonthly            = 6

!For AssignVariablePt
INTEGER, parameter  :: varIsArgument             = 1  !if used as a value or on right side of expression
INTEGER, parameter  :: varIsAssigned             = 2  !if variable is assigned to or on left side of expression

!For ComputeSteps
! All are negative because all variables are positive
INTEGER, parameter  :: opSUM                     = -1
INTEGER, parameter  :: opMULTIPLY                = -2
INTEGER, parameter  :: opSUBTRACT                = -3
INTEGER, parameter  :: opDIVIDE                  = -4
INTEGER, parameter  :: opABSOLUTE                = -5
INTEGER, parameter  :: opINTEGER                 = -6
INTEGER, parameter  :: opSIGN                    = -7
INTEGER, parameter  :: opROUND                   = -8
INTEGER, parameter  :: opMAXIMUM                 = -9
INTEGER, parameter  :: opMINIMUM                 = -10
INTEGER, parameter  :: opEXCEEDS                 = -11
INTEGER, parameter  :: opANNUALMINIMUM           = -12
INTEGER, parameter  :: opANNUALMAXIMUM           = -13
INTEGER, parameter  :: opANNUALSUM               = -14
INTEGER, parameter  :: opANNUALAVERAGE           = -15
INTEGER, parameter  :: opANNUALOR                = -16
INTEGER, parameter  :: opANNUALAND               = -17
INTEGER, parameter  :: opANNUALMAXIMUMZERO       = -18
INTEGER, parameter  :: opANNUALMINIMUMZERO       = -19
INTEGER, parameter  :: opIF                      = -20
INTEGER, parameter  :: opGREATERTHAN             = -21
INTEGER, parameter  :: opGREATEREQUAL            = -22
INTEGER, parameter  :: opLESSTHAN                = -23
INTEGER, parameter  :: opLESSEQUAL               = -24
INTEGER, parameter  :: opEQUAL                   = -25
INTEGER, parameter  :: opNOTEQUAL                = -26
INTEGER, parameter  :: opAND                     = -27
INTEGER, parameter  :: opOR                      = -28
INTEGER, parameter  :: opNOT                     = -29
INTEGER, parameter  :: opADD                     = -30
INTEGER, parameter  :: opNOOP                    = -31 !no operation - just list the operand variables - shown as FROM

!not predefined variable (user defined name - many variables and all objects)
! used in econvar%specific
INTEGER, parameter  :: varUserDefined            = 1
INTEGER, parameter  :: varNotYetDefined          = 2

!category variables (used in econvar%specific)
INTEGER, parameter  :: catEnergyCharges          = 11
INTEGER, parameter  :: catDemandCharges          = 12
INTEGER, parameter  :: catServiceCharges         = 13
INTEGER, parameter  :: catBasis                  = 14
INTEGER, parameter  :: catAdjustment             = 15
INTEGER, parameter  :: catSurcharge              = 16
INTEGER, parameter  :: catSubtotal               = 17
INTEGER, parameter  :: catTaxes                  = 18
INTEGER, parameter  :: catTotal                  = 19
INTEGER, parameter  :: catNotIncluded            = 20

!native variables (based on energy and demands from the simulation) used in econvar%specific
INTEGER, parameter  :: nativeTotalEnergy         = 101
INTEGER, parameter  :: nativeTotalDemand         = 102
INTEGER, parameter  :: nativePeakEnergy          = 103
INTEGER, parameter  :: nativePeakDemand          = 104
INTEGER, parameter  :: nativeShoulderEnergy      = 105
INTEGER, parameter  :: nativeShoulderDemand      = 106
INTEGER, parameter  :: nativeOffPeakEnergy       = 107
INTEGER, parameter  :: nativeOffPeakDemand       = 108
INTEGER, parameter  :: nativeMidPeakEnergy       = 109
INTEGER, parameter  :: nativeMidPeakDemand       = 110
INTEGER, parameter  :: nativePeakExceedsOffPeak  = 111
INTEGER, parameter  :: nativeOffPeakExceedsPeak  = 112
INTEGER, parameter  :: nativePeakExceedsMidPeak  = 113
INTEGER, parameter  :: nativeMidPeakExceedsPeak  = 114
INTEGER, parameter  :: nativePeakExceedsShoulder = 115
INTEGER, parameter  :: nativeShoulderExceedsPeak = 116
INTEGER, parameter  :: nativeIsWinter            = 117
INTEGER, parameter  :: nativeIsNotWinter         = 118
INTEGER, parameter  :: nativeIsSpring            = 119
INTEGER, parameter  :: nativeIsNotSpring         = 120
INTEGER, parameter  :: nativeIsSummer            = 121
INTEGER, parameter  :: nativeIsNotSummer         = 122
INTEGER, parameter  :: nativeIsAutumn            = 123
INTEGER, parameter  :: nativeIsNotAutumn         = 124

INTEGER, parameter  :: nativePeakAndShoulderEnergy     = 125
INTEGER, parameter  :: nativePeakAndShoulderDemand     = 126
INTEGER, parameter  :: nativePeakAndMidPeakEnergy      = 127
INTEGER, parameter  :: nativePeakAndMidPeakDemand      = 128
INTEGER, parameter  :: nativeShoulderAndOffPeakEnergy  = 129
INTEGER, parameter  :: nativeShoulderAndOffPeakDemand  = 130
INTEGER, parameter  :: nativePeakAndOffPeakEnergy      = 131
INTEGER, parameter  :: nativePeakAndOffPeakDemand      = 132

INTEGER, parameter  :: nativeRealTimePriceCosts        = 133
INTEGER, parameter  :: nativeAboveCustomerBaseCosts    = 134
INTEGER, parameter  :: nativeBelowCustomerBaseCosts    = 135
INTEGER, parameter  :: nativeAboveCustomerBaseEnergy   = 136
INTEGER, parameter  :: nativeBelowCustomerBaseEnergy   = 137

INTEGER, parameter  :: countPeriod               = 4
INTEGER, parameter  :: MaxNumMonths              = 12
INTEGER, parameter  :: maxNumBlk                 = 15

INTEGER, parameter  :: periodPeak                = 1
INTEGER, parameter  :: periodShoulder            = 2
INTEGER, parameter  :: periodOffPeak             = 3
INTEGER, parameter  :: periodMidPeak             = 4

INTEGER, parameter  :: kindMeterNotElectric      = 0 !must be zero because testing of >0 done later.
INTEGER, parameter  :: kindMeterElecSimple       = 1
INTEGER, parameter  :: kindMeterElecProduced     = 2
INTEGER, parameter  :: kindMeterElecPurchased    = 3
INTEGER, parameter  :: kindMeterElecSurplusSold  = 4
INTEGER, parameter  :: kindMeterElecNet          = 5

INTEGER, parameter  :: varUnitTypeEnergy        = 1
INTEGER, parameter  :: varUnitTypeDemand        = 2
INTEGER, parameter  :: varUnitTypeDimensionless = 3
INTEGER, parameter  :: varUnitTypeCurrency      = 4

          !MODULE PARAMETER DEFINITIONS:

TYPE EconVarType
  CHARACTER(len=MaxNameLength)   :: name          = ''  !name of the economics object or variable
  INTEGER                        :: tariffIndx    = 0   !index of the tariff name in the tariff array
  INTEGER                        :: kindOfObj     = 0   !enumerated list for the kind of economics object
  INTEGER                        :: index         = 0   !pointer to item in specific array
  REAL(r64),DIMENSION(MaxNumMonths)   :: values        = 0.0d0   !values
  ! the following items are not part of the object description
  LOGICAL                        :: isArgument    = .FALSE. !flag if the variable is ever used as an argument (value needed)
  LOGICAL                        :: isAssigned    = .FALSE. !flag if the variable is ever assigned to
  INTEGER                        :: specific      = 0   !the specific type of variable - see enumerated lists
  ! the following items are used in determinging the dependency relationship of variables
  ! and consist of an operator and a list of variables.
  INTEGER                        :: cntMeDependOn = 0   !count of items in depend this line depends upon
  INTEGER                        :: operator      = 0   !operator used in equation (usually opSUM or opNOOP)
  INTEGER                        :: firstOperand  = 0   !first item in the operand array
  INTEGER                        :: lastOperand   = 0   !last item in the operand array
  LOGICAL                        :: activeNow     =.FALSE. !flag if the econVar is used in the current tariff
  LOGICAL                        :: isEvaluated   =.FALSE. !flag if the economics object that results in this variable
                                                     !has already been evaulated
  LOGICAL                        :: isReported    =.FALSE. !flag if the econVar has been reported in the output file
  INTEGER                        :: varUnitType   = 0   !variable unit type: energy, demand, dimensionless, currency
END TYPE
TYPE (EconVarType), ALLOCATABLE, DIMENSION(:)    :: econVar
TYPE (EconVarType), ALLOCATABLE, DIMENSION(:)    :: econVarCopy
INTEGER                                          :: numEconVar = 0
INTEGER                                          :: sizeEconVar = 0

! holds the outbound connections for each variable
INTEGER,ALLOCATABLE,DIMENSION(:)                 :: operand   ! sized to sizeOperand
INTEGER,ALLOCATABLE,DIMENSION(:)                 :: operandCopy
INTEGER                                          :: numOperand = 0
INTEGER                                          :: sizeOperand = 0

TYPE TariffType
  CHARACTER(len=MaxNameLength)   :: tariffName      = '' !name of the tariff
  CHARACTER(len=MaxNameLength)   :: reportMeter     = '' !name of the report meter
  INTEGER                        :: reportMeterIndx = 0  !index of the report meter
  INTEGER                        :: kindElectricMtr = 0  !kind of electric meter - see enumerated list above, 0 is not electric
  INTEGER                        :: resourceNum     = 0  !based on list of DataGlobalConstants
  INTEGER                        :: convChoice      = 0  !enumerated choice index of the conversion factor
  REAL(r64)                      :: energyConv      = 0.0d0  !energy conversion factor
  REAL(r64)                      :: demandConv      = 0.0d0  !demand conversion factor
  CHARACTER(len=MaxNameLength)   :: periodSchedule  = '' !name of the period schedule (time of day)
  INTEGER                        :: periodSchIndex  = 0  !index to the period schedule
  CHARACTER(len=MaxNameLength)   :: seasonSchedule  = '' !name of the season schedule (winter/summer)
  INTEGER                        :: seasonSchIndex  = 0  !index to the season schedule
  CHARACTER(len=MaxNameLength)   :: monthSchedule   = '' !name of month schedule (when months end)
  INTEGER                        :: monthSchIndex   = 0  !index to the month schedule
  INTEGER                        :: demandWindow    = 0  !enumerated list of the kind of demand window
  REAL(r64)                      :: demWinTime      = 0.0d0  !length of time for the demand window
  REAL(r64)                      :: monthChgVal     = 0.0d0  !monthly charge value
  INTEGER                        :: monthChgPt      = 0  !pointer to a variable that contains the monthly charge
                                                         !if 0 then use monthChgVal
  REAL(r64)                      :: minMonthChgVal  = 0.0d0  !minimum monthly charge value
  INTEGER                        :: minMonthChgPt   = 0  !pointer to a variable that contains the minimum monthly charge
                                                         !if 0 then use minMonthChgVal
  CHARACTER(len=MaxNameLength)   :: chargeSchedule  = '' !name of the charge schedule (for real time pricing)
  INTEGER                        :: chargeSchIndex  = 0  !index to the charge schedule
  CHARACTER(len=MaxNameLength)   :: baseUseSchedule = '' !name of the baseline use schedule (for real time pricing)
  INTEGER                        :: baseUseSchIndex = 0  !index to the baseline use schedule
  CHARACTER(len=MaxNameLength)   :: groupName       = '' !name of the group
  CHARACTER(len=MaxNameLength)   :: monetaryUnit    = '' !text string representing monetary unit, usually $
  INTEGER                        :: buyOrSell       = 0  !enumerated choice index of the buy or sell options
  ! index to the first and last category variables
  INTEGER                        :: firstCategory   = 0  !first category referenced
  INTEGER                        :: lastCategory    = 0  !last category referenced
  ! pointers to econ variables for categories
  INTEGER                        :: ptEnergyCharges = 0
  INTEGER                        :: ptDemandCharges = 0
  INTEGER                        :: ptServiceCharges= 0
  INTEGER                        :: ptBasis         = 0
  INTEGER                        :: ptAdjustment    = 0
  INTEGER                        :: ptSurcharge     = 0
  INTEGER                        :: ptSubtotal      = 0
  INTEGER                        :: ptTaxes         = 0
  INTEGER                        :: ptTotal         = 0
  INTEGER                        :: ptNotIncluded   = 0
  ! index to the first and last native variables (energies and demands from the simulation)
  INTEGER                        :: firstNative     = 0
  INTEGER                        :: lastNative      = 0
  !native variables (based on energy and demands from the simulation)
  INTEGER                        :: nativeTotalEnergy         = 0
  INTEGER                        :: nativeTotalDemand         = 0
  INTEGER                        :: nativePeakEnergy          = 0
  INTEGER                        :: nativePeakDemand          = 0
  INTEGER                        :: nativeShoulderEnergy      = 0
  INTEGER                        :: nativeShoulderDemand      = 0
  INTEGER                        :: nativeOffPeakEnergy       = 0
  INTEGER                        :: nativeOffPeakDemand       = 0
  INTEGER                        :: nativeMidPeakEnergy       = 0
  INTEGER                        :: nativeMidPeakDemand       = 0
  INTEGER                        :: nativePeakExceedsOffPeak  = 0
  INTEGER                        :: nativeOffPeakExceedsPeak  = 0
  INTEGER                        :: nativePeakExceedsMidPeak  = 0
  INTEGER                        :: nativeMidPeakExceedsPeak  = 0
  INTEGER                        :: nativePeakExceedsShoulder = 0
  INTEGER                        :: nativeShoulderExceedsPeak = 0
  INTEGER                        :: nativeIsWinter            = 0
  INTEGER                        :: nativeIsNotWinter         = 0
  INTEGER                        :: nativeIsSpring            = 0
  INTEGER                        :: nativeIsNotSpring         = 0
  INTEGER                        :: nativeIsSummer            = 0
  INTEGER                        :: nativeIsNotSummer         = 0
  INTEGER                        :: nativeIsAutumn            = 0
  INTEGER                        :: nativeIsNotAutumn         = 0
  INTEGER                        :: nativePeakAndShoulderEnergy    = 0
  INTEGER                        :: nativePeakAndShoulderDemand    = 0
  INTEGER                        :: nativePeakAndMidPeakEnergy     = 0
  INTEGER                        :: nativePeakAndMidPeakDemand     = 0
  INTEGER                        :: nativeShoulderAndOffPeakEnergy = 0
  INTEGER                        :: nativeShoulderAndOffPeakDemand = 0
  INTEGER                        :: nativePeakAndOffPeakEnergy     = 0
  INTEGER                        :: nativePeakAndOffPeakDemand     = 0
  !real time pricing native variable pointers
  INTEGER                        :: nativeRealTimePriceCosts       = 0
  INTEGER                        :: nativeAboveCustomerBaseCosts   = 0
  INTEGER                        :: nativeBelowCustomerBaseCosts   = 0
  INTEGER                        :: nativeAboveCustomerBaseEnergy  = 0
  INTEGER                        :: nativeBelowCustomerBaseEnergy  = 0

  !arrays for holding gathered values
  REAL(r64),DIMENSION(countPeriod,MaxNumMonths)  :: gatherEnergy   = 0.0d0
  REAL(r64),DIMENSION(countPeriod,MaxNumMonths)  :: gatherDemand   = 0.0d0
  REAL(r64)                                 :: collectTime    = 0.0d0
  REAL(r64)                                 :: collectEnergy  = 0.0d0
  !arryas for holding real time pricing gathered values
  REAL(r64),DIMENSION(MaxNumMonths)         :: RTPcost             = 0.0d0
  REAL(r64),DIMENSION(MaxNumMonths)         :: RTPaboveBaseCost    = 0.0d0
  REAL(r64),DIMENSION(MaxNumMonths)         :: RTPbelowBaseCost    = 0.0d0
  REAL(r64),DIMENSION(MaxNumMonths)         :: RTPaboveBaseEnergy  = 0.0d0
  REAL(r64),DIMENSION(MaxNumMonths)         :: RTPbelowBaseEnergy  = 0.0d0

  INTEGER,DIMENSION(MaxNumMonths)           :: seasonForMonth = 0
  !overall qualification of the rate
  LOGICAL                        :: isQualified               = .FALSE.
  INTEGER                        :: ptDisqualifier            = 0
  !overall selection and annual cost
  LOGICAL                        :: isSelected                = .FALSE.
  REAL(r64)                      :: totalAnnualCost           = 0.0d0
  REAL(r64)                      :: totalAnnualEnergy         = 0.0d0
END TYPE
TYPE (TariffType), ALLOCATABLE, DIMENSION(:)     :: tariff
INTEGER                                          :: numTariff = 0

TYPE QualifyType
  INTEGER                        :: namePt         = 0 !index of the name and variable in the variable array
  INTEGER                        :: tariffIndx     = 0 !index of the tariff name in the tariff array
  INTEGER                        :: sourcePt       = 0 !index of the variable in the variable array
  LOGICAL                        :: isMaximum      = .FALSE. !indicator if maximum test otherwise minimum
  REAL(r64)                      :: thresholdVal   = 0.0d0 !value of the threshold
  INTEGER                        :: thresholdPt    = 0 !pointer to the variable holding the values
  INTEGER                        :: season         = 0 !enumerated list of the kind of season
  LOGICAL                        :: isConsecutive  = .FALSE. !indicator if consecutive months otherwise count
  INTEGER                        :: numberOfMonths = 0 !number of months the test must be good for
END TYPE
TYPE (QualifyType), ALLOCATABLE, DIMENSION(:)    :: qualify
INTEGER                                          :: numQualify = 0

TYPE ChargeSimpleType
  INTEGER                        :: namePt         = 0 !index of the name and variable in the variable array
  INTEGER                        :: tariffIndx     = 0 !index of the tariff name in the tariff array
  INTEGER                        :: sourcePt       = 0 !index of the variable in the variable array
  INTEGER                        :: season         = 0 !enumerated list of the kind of season
  INTEGER                        :: categoryPt     = 0 !index of the category in the variable array
  REAL(r64)                      :: costPerVal     = 0.0d0 !cost per unit value
  INTEGER                        :: costPerPt      = 0 !cost per unit index in the variable array (0 is flag for no variable)
END TYPE
TYPE (ChargeSimpleType), ALLOCATABLE, DIMENSION(:) :: chargeSimple
INTEGER                                            :: numChargeSimple = 0

TYPE ChargeBlockType
  INTEGER                        :: namePt         = 0 !index of the name and variable in the variable array
  INTEGER                        :: tariffIndx     = 0 !index of the tariff name in the tariff array
  INTEGER                        :: sourcePt       = 0 !index of the variable in the variable array
  INTEGER                        :: season         = 0 !enumerated list of the kind of season
  INTEGER                        :: categoryPt     = 0 !index of the category in the variable array
  INTEGER                        :: remainingPt    = 0 !index of the remaining into variable in the variable array
  REAL(r64)                      :: blkSzMultVal   = 0.0d0 !block size multiplier value
  INTEGER                        :: blkSzMultPt    = 0 !block size variable in the variable array (0 is flag for no variable)
  INTEGER                        :: numBlk         = 0 !number of blocks used
  REAL(r64),DIMENSION(maxNumBlk)      :: blkSzVal       = 0.0d0 !array of block size values
  INTEGER,DIMENSION(maxNumBlk)   :: blkSzPt        = 0 !block size variables index to the variable array (0 is no variable)
  REAL(r64),DIMENSION(maxNumBlk)      :: blkCostVal     = 0.0d0 !array of block cost values
  INTEGER,DIMENSION(maxNumBlk)   :: blkCostPt      = 0 !block cost variables index to the variable array (0 is no variable)
END TYPE
TYPE (ChargeBlockType), ALLOCATABLE, DIMENSION(:)  :: chargeBlock
INTEGER                                            :: numChargeBlock = 0

TYPE RatchetType
  INTEGER                        :: namePt         = 0 !index of the name and variable in the variable array
  INTEGER                        :: tariffIndx     = 0 !index of the tariff name in the tariff array
  INTEGER                        :: baselinePt     = 0 !index of the baseline variable in the variable array
  INTEGER                        :: adjustmentPt   = 0 !index fo the adjustment variable in the variable array
  INTEGER                        :: seasonFrom     = 0 !enumerated list of the kind of season
  INTEGER                        :: seasonTo       = 0 !enumerated list of the kind of season
  REAL(r64)                      :: multiplierVal  = 0.0d0 !value of the ratchet multiplier
  INTEGER                        :: multiplierPt   = 0 !multiplier variable in the variable array (0 for no variable)
  REAL(r64)                      :: offsetVal      = 0.0d0 !value of the ratchet offset
  INTEGER                        :: offsetPt       = 0 !offset variable in the variable array (0 for no variable)
END TYPE
TYPE (RatchetType), ALLOCATABLE, DIMENSION(:)      :: ratchet
INTEGER                                            :: numRatchet = 0

TYPE ComputationType
  CHARACTER(len=MaxNameLength)   :: computeName    = '' !name of the compute
  INTEGER                        :: firstStep      = 0  !index to steps array for the first step in this compute steps
  INTEGER                        :: lastStep       = 0  !index to steps array for the last step in this compute steps
  LOGICAL                        :: isUserDef      = .FALSE. !if the computation steps were user defined
END TYPE
TYPE (ComputationType), ALLOCATABLE, DIMENSION(:)  :: computation
INTEGER                                            :: numComputation = 0

!list of pointers to variable, 0 end of line, negative indicate operations
INTEGER,ALLOCATABLE,DIMENSION(:)                   :: steps
INTEGER,ALLOCATABLE,DIMENSION(:)                   :: stepsCopy
INTEGER                                            :: numSteps = 0
INTEGER                                            :: sizeSteps = 0

TYPE StackType
  INTEGER                        :: varPt          = 0 !pointer to item in specific array
  REAL(r64),DIMENSION(MaxNumMonths)   :: values         = 0.0d0 !values
END TYPE
TYPE (StackType), ALLOCATABLE, DIMENSION(:)      :: stack
TYPE (StackType), ALLOCATABLE, DIMENSION(:)      :: stackCopy
INTEGER                                          :: topOfStack = 0
INTEGER                                          :: sizeStack  = 0



         !MODULE VARIABLE DECLARATIONS:

! SUBROUTINE SPECIFICATIONS FOR MODULE
PUBLIC     UpdateUtilityBills
PRIVATE    GetInputEconomicsTariff
PRIVATE    GetInputEconomicsQualify
PRIVATE    GetInputEconomicsChargeSimple
PRIVATE    GetInputEconomicsChargeBlock
PRIVATE    GetInputEconomicsRatchet
PRIVATE    GetInputEconomicsVariable
PRIVATE    GetInputEconomicsComputation
PRIVATE      parseComputeLine
PRIVATE      GetLastWord
PRIVATE      LookUpSeason
PRIVATE      FindTariffIndex
PRIVATE      AssignVariablePt
PRIVATE      incrementEconVar
PRIVATE      incrementSteps
PRIVATE      RemoveSpaces
PRIVATE      CreateCategoryNativeVariables
PRIVATE      lookupOperator
PRIVATE    GetInputEconomicsCurrencyType
PRIVATE    CreateDefaultComputation
PRIVATE      addOperand
PRIVATE      addChargesToOperand
PRIVATE    GatherForEconomics
PRIVATE      isWithinRange

PUBLIC     ComputeTariff
PRIVATE      setNativeVariables
PRIVATE      popStack
PRIVATE      pushStack

PUBLIC     WriteTabularTariffReports
PRIVATE      selectTariff

PUBLIC     GetMonthlyCostForResource

CONTAINS

!======================================================================================================================
!======================================================================================================================
!
!
!    MAIN ROUTINE CALLED EACH TIMESTEP
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE UpdateUtilityBills

          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   September 2003
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Single routine used to call all get input
          !    routines for economics.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

USE DataGlobals, ONLY: DoOutputReporting, KindOfSim, ksRunPeriodWeather
USE DataInterfaces, ONLY: ShowFatalError
USE OutputReportTabular, ONLY: AddTOCEntry

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

IMPLICIT NONE

LOGICAL, SAVE     :: GetInput = .TRUE.
LOGICAL           :: ErrorsFound = .false.

IF (GetInput) THEN
  CALL GetInputEconomicsTariff(ErrorsFound)
  ! do rest of GetInput only if at least one tariff is defined.
  CALL GetInputEconomicsCurrencyType(ErrorsFound)
  IF (numTariff .GE. 1) THEN
    IF (.not. ErrorsFound) CALL AddTOCEntry('Economics Results Summary Report','Entire Facility')
    CALL CreateCategoryNativeVariables
    CALL GetInputEconomicsQualify(ErrorsFound)
    CALL GetInputEconomicsChargeSimple(ErrorsFound)
    CALL GetInputEconomicsChargeBlock(ErrorsFound)
    CALL GetInputEconomicsRatchet(ErrorsFound)
    CALL GetInputEconomicsVariable(ErrorsFound)
    CALL GetInputEconomicsComputation(ErrorsFound)
    CALL CreateDefaultComputation
  END IF
  GetInput = .FALSE.
  IF (ErrorsFound) CALL ShowFatalError('UpdateUtilityBills: Preceding errors cause termination.')
END IF
IF (DoOutputReporting .and. (KindOfSim == ksRunPeriodWeather)) THEN
  CALL GatherForEconomics
END IF
END SUBROUTINE UpdateUtilityBills

!======================================================================================================================
!======================================================================================================================
!
!
!    GET INPUT ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE GetInputEconomicsTariff(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Tariff" objects.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataGlobals, ONLY: NumOfTimeStepInHour
USE OutputReportTabular, ONLY: AddTOCEntry
USE DataInterfaces, ONLY:GetVariableKeyCountandType, GetVariableKeys
USE OutputProcessor, ONLY: EnergyMeters, NumEnergyMeters
USE DataGlobalConstants, ONLY: AssignResourceTypeNum
USE DataIPShortCuts
USE General, ONLY: RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsTariff: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: jObj       ! loop index for objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: AlphaArray !character string data
!REAL(r64),                        DIMENSION(100)  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: found
LOGICAL                     :: isNotNumeric
! variables for getting report variable/meter index
INTEGER          :: KeyCount
INTEGER          :: TypeVar
INTEGER          :: AvgSumVar
INTEGER          :: StepTypeVar
CHARACTER(len=MaxNameLength) :: UnitsVar     ! Units sting, may be blank
CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NamesOfKeys      ! Specific key name
INTEGER, DIMENSION(:) , ALLOCATABLE                     :: IndexesForKeyVar ! Array index
INTEGER          :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Tariff'
NumTariff = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(tariff(NumTariff))
DO iInObj = 1 , NumTariff
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
      !  args are always turned to upper case but this is okay...
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  !name of the tariff
  tariff(iInObj)%tariffName =  cAlphaArgs(1)
  !check if tariff name is unique
  found = 0
  DO jObj = 1, iInObj - 1
    IF (tariff(iInObj)%tariffName .EQ. tariff(jObj)%tariffName) THEN
      found = jObj
      EXIT
    END IF
  END DO
  IF (found .GT. 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
    CALL ShowContinueError('...Duplicate name. Name has already been used.')
    ErrorsFound=.true.
  END IF
  !name of the report meter
  tariff(iInObj)%reportMeter =  cAlphaArgs(2)
  ! call the key count function but only need count during this pass
  CALL GetVariableKeyCountandType(tariff(iInObj)%reportMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
  ! if no meters found for that name
  IF (KeyCount .EQ. 0) THEN
    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" missing meter')
    CALL ShowContinueError('Meter referenced is not present due to a lack of equipment that uses that energy source/meter:"'//  &
       trim(tariff(iInObj)%reportMeter)//'".')
    tariff(iInObj)%reportMeterIndx = 0
  ELSE
    ALLOCATE(NamesOfKeys(KeyCount))
    ALLOCATE(IndexesForKeyVar(KeyCount))
    CALL GetVariableKeys(tariff(iInObj)%reportMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
    !although this retrieves all keys for a variable, we only need one so the first one is chosen
    IF (KeyCount .GT. 1) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" multiple keys')
      CALL ShowContinueError('... Multiple keys for variable select. First key will be used.')
    END IF
    !assign the index
    tariff(iInObj)%reportMeterIndx = IndexesForKeyVar(1)
    !get rid of the arrays used to get the variable number
    DEALLOCATE(NamesOfKeys)
    DEALLOCATE(IndexesForKeyVar)
  END IF
  !conversion factor
  IF (SameString(cAlphaArgs(3),'USERDEFINED')) THEN
    tariff(iInObj)%convChoice = conversionUSERDEF
    tariff(iInObj)%energyConv = rNumericArgs(1)      !energy conversion factor
    tariff(iInObj)%demandConv = rNumericArgs(2)      !demand conversion factor
  ELSE IF (SameString(cAlphaArgs(3),'KWH')) THEN
    tariff(iInObj)%convChoice = conversionKWH
    tariff(iInObj)%energyConv = 0.0000002778d0
    tariff(iInObj)%demandConv = 0.001d0
  ELSE IF (SameString(cAlphaArgs(3),'THERM')) THEN
    tariff(iInObj)%convChoice = conversionTHERM
    tariff(iInObj)%energyConv = 9.4781712d-9
    tariff(iInObj)%demandConv = 0.00003412d0
  ELSE IF (SameString(cAlphaArgs(3),'MMBTU')) THEN
    tariff(iInObj)%convChoice = conversionMMBTU
    tariff(iInObj)%energyConv = 9.4781712d-10
    tariff(iInObj)%demandConv = 0.000003412d0
  ELSE IF (SameString(cAlphaArgs(3),'MJ')) THEN
    tariff(iInObj)%convChoice = conversionMJ
    tariff(iInObj)%energyConv = 0.000001d0
    tariff(iInObj)%demandConv = 0.0036d0
  ELSE IF (SameString(cAlphaArgs(3),'KBTU')) THEN
    tariff(iInObj)%convChoice = conversionKBTU
    tariff(iInObj)%energyConv = 9.4781712d-7
    tariff(iInObj)%demandConv = 0.003412d0
  ELSE IF (SameString(cAlphaArgs(3),'MCF')) THEN
    tariff(iInObj)%convChoice = conversionMCF
    tariff(iInObj)%energyConv = 9.4781712d-10
    tariff(iInObj)%demandConv = 0.000003412d0
  ELSE IF (SameString(cAlphaArgs(3),'CCF')) THEN
    tariff(iInObj)%convChoice = conversionCCF
    tariff(iInObj)%energyConv = 9.4781712d-9
    tariff(iInObj)%demandConv = 0.00003412d0
  ELSE
    tariff(iInObj)%convChoice = conversionKWH
    tariff(iInObj)%energyConv = 0.0000002778d0
    tariff(iInObj)%demandConv = 0.001d0
    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
    CALL ShowContinueError(trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'", Defaulting to KWH.')
  END IF
  !schedules
  ! period schedule
  IF (LEN_TRIM(cAlphaArgs(4)) .GT. 0) THEN
    tariff(iInObj)%periodSchedule = cAlphaArgs(4)  !name of the period schedule (time of day)
    tariff(iInObj)%periodSchIndex = GetScheduleIndex(cAlphaArgs(4)) !index to the period schedule
    IF (tariff(iInObj)%periodSchIndex .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
      CALL ShowContinueError(' not found '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4)) //'".')
      ErrorsFound=.true.
    END IF
  ELSE
    tariff(iInObj)%periodSchIndex = 0 !flag value for no schedule used
  END IF
  ! season schedule
  IF (LEN_TRIM(cAlphaArgs(5)) .GT. 0) THEN
    tariff(iInObj)%seasonSchedule = cAlphaArgs(5)  !name of the season schedule (winter/summer)
    tariff(iInObj)%seasonSchIndex = GetScheduleIndex(cAlphaArgs(5))   !index to the season schedule
    IF (tariff(iInObj)%seasonSchIndex .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
      CALL ShowContinueError(' not found '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5)) //'".')
      ErrorsFound=.true.
    END IF
  ELSE
    tariff(iInObj)%seasonSchIndex = 0 !flag value for no schedule used
  END IF
  ! month schedule
  IF (LEN_TRIM(cAlphaArgs(6)) .GT. 0) THEN
    tariff(iInObj)%monthSchedule = cAlphaArgs(6)   !name of month schedule (when months end)
    tariff(iInObj)%monthSchIndex = GetScheduleIndex(cAlphaArgs(6))    !index to the month schedule
    IF (tariff(iInObj)%monthSchIndex .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
      CALL ShowContinueError(' not found '//trim(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6)) //'".')
      ErrorsFound=.true.
    END IF
  ELSE
    tariff(iInObj)%monthSchIndex = 0 !flag value for no schedule used
  END IF
  !type of demand window
  IF (SameString(cAlphaArgs(7),'QuarterHour')) THEN
    ! check to make sure that the demand window and the TIMESTEP IN HOUR are consistant.
    SELECT CASE (NumOfTimeStepInHour)
      CASE (1,3,5,15)
        tariff(iInObj)%demandWindow = demandWindowHour
        tariff(iInObj)%demWinTime = 1.00d0
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
        CALL ShowContinueError('Demand window of QuarterHour is not consistent with number of timesteps per hour ['//  &
           trim(RoundSigDigits(NumOfTimeStepInHour))//'].')
        CALL ShowContinueError('Demand window will be set to FullHour, and the simulation continues.')
      CASE (2,6,10,30)
        tariff(iInObj)%demandWindow = demandWindowHalf
        tariff(iInObj)%demWinTime = 0.50d0
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
        CALL ShowContinueError('Demand window of QuarterHour is not consistent with number of timesteps per hour ['//  &
           trim(RoundSigDigits(NumOfTimeStepInHour))//'].')
        CALL ShowContinueError('Demand window will be set to HalfHour, and the simulation continues.')
      CASE (4,12,20,60)
        tariff(iInObj)%demandWindow = demandWindowQuarter
        tariff(iInObj)%demWinTime = 0.25d0
    END SELECT
  ELSE IF (SameString(cAlphaArgs(7),'HalfHour')) THEN
    SELECT CASE (NumOfTimeStepInHour)
      CASE (1,3,5,15)
        tariff(iInObj)%demandWindow = demandWindowHour
        tariff(iInObj)%demWinTime = 1.00d0
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
        CALL ShowContinueError('Demand window of HalfHour is not consistent with number of timesteps per hour ['//  &
           trim(RoundSigDigits(NumOfTimeStepInHour))//'].')
        CALL ShowContinueError('Demand window will be set to FullHour, and the simulation continues.')
      CASE (2,4,6,10,12,20,30,60)
        tariff(iInObj)%demandWindow = demandWindowHalf
        tariff(iInObj)%demWinTime = 0.50d0
    END SELECT
  ELSE IF (SameString(cAlphaArgs(7),'FullHour')) THEN
    tariff(iInObj)%demandWindow = demandWindowHour
    tariff(iInObj)%demWinTime = 1.00d0
  ELSE IF (SameString(cAlphaArgs(7),'Day')) THEN
    tariff(iInObj)%demandWindow = demandWindowDay
    tariff(iInObj)%demWinTime = 24.00d0
  ELSE IF (SameString(cAlphaArgs(7),'Week')) THEN
    tariff(iInObj)%demandWindow = demandWindowWeek
    tariff(iInObj)%demWinTime = 24.d0 * 7.d0
  ELSE
    ! if not entered default to the same logic as quarter of an hour
    SELECT CASE (NumOfTimeStepInHour)
      CASE (1,3,5,15)
        tariff(iInObj)%demandWindow = demandWindowHour
        tariff(iInObj)%demWinTime = 1.00d0
      CASE (2,6,10,30)
        tariff(iInObj)%demandWindow = demandWindowHalf
        tariff(iInObj)%demWinTime = 0.50d0
      CASE (4,12,20,60)
        tariff(iInObj)%demandWindow = demandWindowQuarter
        tariff(iInObj)%demWinTime = 0.25d0
    END SELECT
  END IF
  !monthly charge
  tariff(iInObj)%monthChgVal = ProcessNumber(cAlphaArgs(8),isNotNumeric)
  tariff(iInObj)%monthChgPt = AssignVariablePt(cAlphaArgs(8),isNotNumeric,varIsArgument,varNotYetDefined,kindUnknown,0,iInObj)
  !minimum monthly charge
  IF (LEN_TRIM(cAlphaArgs(9)) .GT. 0) THEN
    tariff(iInObj)%minMonthChgVal = ProcessNumber(cAlphaArgs(9),isNotNumeric)
  ELSE
    tariff(iInObj)%minMonthChgVal = -HUGE(-1.0d0) !set to a very negative value
  END IF
  tariff(iInObj)%minMonthChgPt = AssignVariablePt(cAlphaArgs(9),isNotNumeric,varIsArgument,varNotYetDefined,kindUnknown,0,iInObj)
  !real time pricing
  tariff(iInObj)%chargeSchedule = cAlphaArgs(10)
  tariff(iInObj)%chargeSchIndex = GetScheduleIndex(cAlphaArgs(10))
  tariff(iInObj)%baseUseSchedule = cAlphaArgs(11)
  tariff(iInObj)%baseUseSchIndex = GetScheduleIndex(cAlphaArgs(11))
  !group name for separate distribution and transmission rates
  tariff(iInObj)%groupName = cAlphaArgs(12)
  !buy or sell option
  IF (SameString(cAlphaArgs(13),'BuyFromUtility')) THEN
    tariff(iInObj)%buyOrSell = buyFromUtility
  ELSE IF (SameString(cAlphaArgs(13),'SellToUtility')) THEN
    tariff(iInObj)%buyOrSell = sellToUtility
  ELSE IF (SameString(cAlphaArgs(13),'NetMetering')) THEN
    tariff(iInObj)%buyOrSell = netMetering
  ELSE
    tariff(iInObj)%buyOrSell = buyFromUtility
  END IF
  ! check if meter is consistent with buy or sell option
  IF ((tariff(iInObj)%buyOrSell .EQ. sellToUtility) .AND.   &
        (.NOT. SameString(tariff(iInObj)%reportMeter,'ELECTRICITYSURPLUSSOLD:FACILITY'))) THEN
    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" atypical meter')
    CALL ShowContinueError('The meter chosen "' // TRIM(tariff(iInObj)%reportMeter) //  &
       '" is not typically used with the sellToUtility option.')
    CALL ShowContinueError('Usually the ElectricitySurplusSold:Facility meter is selected when the sellToUtility option is used.')
  END IF
  IF ((tariff(iInObj)%buyOrSell .EQ. netMetering) .AND.   &
       (.NOT. SameString(tariff(iInObj)%reportMeter,'ELECTRICITYNET:FACILITY'))) THEN
    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" atypical meter')
    CALL ShowContinueError('The meter chosen "' // TRIM(tariff(iInObj)%reportMeter) //  &
       ' is not typically used with the netMetering option.')
    CALL ShowContinueError('Usually the ElectricityNet:Facility meter is selected when the netMetering option is used.')
  END IF
  !also test the buy option for electricity
  IF (tariff(iInObj)%buyOrSell .EQ. buyFromUtility) THEN
    IF (INDEX(MakeUPPERCase(tariff(iInObj)%reportMeter),'ELEC') .GT. 0) THEN !test if electric meter
      IF (.NOT. (SameString(tariff(iInObj)%reportMeter,'Electricity:Facility') .OR.   &
                 SameString(tariff(iInObj)%reportMeter,'ElectricityPurchased:Facility'))) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" atypical meter')
        CALL ShowContinueError('The meter chosen "' // TRIM(tariff(iInObj)%reportMeter) //  &
           ' is not typically used with the buyFromUtility option.')
        CALL ShowContinueError('Usually the Electricity:Facility meter or the '//  &
           'ElectricityPurchased:Facility is selected when the buyFromUtility option is used.')
      ENDIF
    END IF
  END IF
  ! initialize gathering arrays
  tariff(iInObj)%seasonForMonth = 0
  tariff(iInObj)%gatherEnergy = 0.0d0
  tariff(iInObj)%gatherDemand = 0.0d0
  !assume that the tariff is qualified
  tariff(iInObj)%isQualified = .TRUE.
  tariff(iInObj)%ptDisqualifier = 0
  !assume that the tariff is not selected
  tariff(iInObj)%isSelected = .FALSE.
  tariff(iInObj)%totalAnnualCost = 0.0d0
  !now create the Table Of Contents entries for an HTML file
  CALL AddTOCEntry('Tariff Report',tariff(iInObj)%tariffName)
  !associate the resource number with each tariff
  IF (Tariff(iInObj)%reportMeterIndx .GE. 1) THEN
    tariff(iInObj)%resourceNum = AssignResourceTypeNum(EnergyMeters(Tariff(iInObj)%reportMeterIndx)%ResourceType)
  END IF
END DO
END SUBROUTINE GetInputEconomicsTariff

SUBROUTINE GetInputEconomicsQualify(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Qualify" objects.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsQualify: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
!REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
LOGICAL                     :: isNotNumeric
INTEGER          :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Qualify'
NumQualify = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(qualify(NumQualify))
DO iInObj = 1 , NumQualify
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  !index of the tariff name in the tariff array
  qualify(iInObj)%tariffIndx  = FindTariffIndex(cAlphaArgs(2),cAlphaArgs(1),ErrorsFound,CurrentModuleObject)
  CALL warnIfNativeVarname(cAlphaArgs(1),qualify(iInObj)%tariffIndx,ErrorsFound,CurrentModuleObject)
  qualify(iInObj)%namePt = AssignVariablePt(cAlphaArgs(1),.TRUE.,varIsAssigned,&
                                varNotYetDefined,kindQualify,iInObj,qualify(iInObj)%tariffIndx)
  !index of the variable in the variable array
  qualify(iInObj)%sourcePt = AssignVariablePt(cAlphaArgs(3), .TRUE. ,varIsArgument,varNotYetDefined, &
                                kindUnknown,0,qualify(iInObj)%tariffIndx)
  !indicator if maximum test otherwise minimum
  IF (SameString(cAlphaArgs(4),'Minimum')) THEN
    qualify(iInObj)%isMaximum = .FALSE.
  ELSE IF (SameString(cAlphaArgs(4),'Maximum')) THEN
    qualify(iInObj)%isMaximum = .TRUE.
  ELSE
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
    CALL ShowContinueError(trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'".')
    ErrorsFound=.true.
    qualify(iInObj)%isMaximum = .TRUE.
  END IF
  !value of the threshold
  qualify(iInObj)%thresholdVal = ProcessNumber(cAlphaArgs(5),isNotNumeric)
  qualify(iInObj)%thresholdPt = AssignVariablePt(cAlphaArgs(5),isNotNumeric,varIsArgument,&
                                varNotYetDefined,kindUnknown,0,qualify(iInObj)%tariffIndx)
  !enumerated list of the kind of season
  qualify(iInObj)%season = LookUpSeason(cAlphaArgs(6),cAlphaArgs(1))
  !indicator if consecutive months otherwise count
  IF (SameString(cAlphaArgs(7),'Count')) THEN
    qualify(iInObj)%isConsecutive  = .FALSE.
  ELSE IF (SameString(cAlphaArgs(7),'Consecutive')) THEN
    qualify(iInObj)%isConsecutive  = .TRUE.
  ELSE
    CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
    CALL ShowContinueError(trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
    ErrorsFound=.true.
    qualify(iInObj)%isConsecutive  = .TRUE.
  END IF
  !number of months the test must be good for
  qualify(iInObj)%numberOfMonths = rNumericArgs(1)
END DO
END SUBROUTINE GetInputEconomicsQualify

SUBROUTINE GetInputEconomicsChargeSimple(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Charge:Simple" objects.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsChargeSimple: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
!REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
LOGICAL                     :: isNotNumeric
INTEGER          :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Charge:Simple'
numChargeSimple = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(chargeSimple(numChargeSimple))
DO iInObj = 1 , numChargeSimple
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  !index of the tariff name in the tariff array
  chargeSimple(iInObj)%tariffIndx = FindTariffIndex(cAlphaArgs(2),cAlphaArgs(1),ErrorsFound,CurrentModuleObject)
  CALL warnIfNativeVarname(cAlphaArgs(1),chargeSimple(iInObj)%tariffIndx,ErrorsFound,CurrentModuleObject)
  chargeSimple(iInObj)%namePt = AssignVariablePt(cAlphaArgs(1),.TRUE.,varIsAssigned,&
                                varNotYetDefined,kindChargeSimple,iInObj,chargeSimple(iInObj)%tariffIndx)
  !index of the variable in the variable array
  chargeSimple(iInObj)%sourcePt = AssignVariablePt(cAlphaArgs(3),.TRUE.,varIsArgument,&
                                  varNotYetDefined,kindUnknown,0,chargeSimple(iInObj)%tariffIndx)
  !enumerated list of the kind of season
  chargeSimple(iInObj)%season = LookUpSeason(cAlphaArgs(4),cAlphaArgs(1))
  !check to make sure a seasonal schedule is specified if the season is not annual
  IF (chargeSimple(iInObj)%season .NE. seasonAnnual) THEN
    IF (chargeSimple(iInObj)%tariffIndx .NE. 0) THEN
      IF (tariff(chargeSimple(iInObj)%tariffIndx)%seasonSchIndex .EQ. 0) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
        CALL ShowContinueError(trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'".')
        CALL ShowContinueError(' a Season other than Annual is used but no Season Schedule Name is specified'//  &
           ' in the UtilityCost:Tariff.')
      END IF
    END IF
  END IF
  !index of the category in the variable array
  chargeSimple(iInObj)%categoryPt = AssignVariablePt(cAlphaArgs(5),.TRUE.,varIsAssigned,&
                                  varNotYetDefined,kindCategory,iInObj,chargeSimple(iInObj)%tariffIndx)
  !cost per unit value or variable
  chargeSimple(iInObj)%costPerVal = ProcessNumber(cAlphaArgs(6),isNotNumeric)
  chargeSimple(iInObj)%costPerPt = AssignVariablePt(cAlphaArgs(6),isNotNumeric,varIsArgument,&
                                varNotYetDefined,kindUnknown,0,chargeSimple(iInObj)%tariffIndx)
END DO
END SUBROUTINE GetInputEconomicsChargeSimple

SUBROUTINE GetInputEconomicsChargeBlock(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Charge:Block" objects.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsChargeBlock: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
!REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
LOGICAL                     :: isNotNumeric
INTEGER                     :: jBlk       ! loop index for blocks
INTEGER                     :: alphaOffset ! offset used in blocks for alpha array
REAL(r64)                   :: hugeNumber
INTEGER                     :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Charge:Block'
hugeNumber = HUGE(hugeNumber)
numChargeBlock = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(chargeBlock(numChargeBlock))
DO iInObj = 1 , numChargeBlock
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  !index of the tariff name in the tariff array
  chargeBlock(iInObj)%tariffIndx = FindTariffIndex(cAlphaArgs(2),cAlphaArgs(1),ErrorsFound,CurrentModuleObject)
  CALL warnIfNativeVarname(cAlphaArgs(1),chargeBlock(iInObj)%tariffIndx,ErrorsFound,CurrentModuleObject)
  chargeBlock(iInObj)%namePt = AssignVariablePt(cAlphaArgs(1),.TRUE.,varIsAssigned,&
                                varNotYetDefined,kindChargeBlock,iInObj,chargeBlock(iInObj)%tariffIndx)
  !index of the variable in the variable array
  chargeBlock(iInObj)%sourcePt = AssignVariablePt(cAlphaArgs(3),.TRUE.,varIsArgument,&
                                  varNotYetDefined,kindUnknown,0,chargeBlock(iInObj)%tariffIndx)
  !enumerated list of the kind of season
  chargeBlock(iInObj)%season = LookUpSeason(cAlphaArgs(4),cAlphaArgs(1))
  !check to make sure a seasonal schedule is specified if the season is not annual
  IF (chargeBlock(iInObj)%season .NE. seasonAnnual) THEN
    IF (chargeBlock(iInObj)%tariffIndx .NE. 0) THEN
      IF (tariff(chargeBlock(iInObj)%tariffIndx)%seasonSchIndex .EQ. 0) THEN
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
        CALL ShowContinueError(trim(cAlphaFieldNames(4))//'="'//trim(cAlphaArgs(4))//'".')
        CALL ShowContinueError(' a Season other than Annual is used but no Season Schedule Name is specified'//  &
           ' in the UtilityCost:Tariff.')
      END IF
    END IF
  END IF
  !index of the category in the variable array
  chargeBlock(iInObj)%categoryPt = AssignVariablePt(cAlphaArgs(5),.TRUE.,varIsAssigned,&
                                  varNotYetDefined,kindCategory,iInObj,chargeBlock(iInObj)%tariffIndx)
  !index of the remaining into variable in the variable array
  chargeBlock(iInObj)%remainingPt = AssignVariablePt(cAlphaArgs(6),.TRUE.,varIsAssigned,&
                                  varNotYetDefined,kindCategory,iInObj,chargeBlock(iInObj)%tariffIndx)
  !block size multiplier
  IF (LEN_TRIM(cAlphaArgs(7)) .EQ. 0) THEN  !if blank
    chargeBlock(iInObj)%blkSzMultVal = 1.0d0  !default is 1 if left blank
    chargeBlock(iInObj)%blkSzMultPt = 0
  ELSE
    chargeBlock(iInObj)%blkSzMultVal = ProcessNumber(cAlphaArgs(7),isNotNumeric)
    chargeBlock(iInObj)%blkSzMultPt = AssignVariablePt(cAlphaArgs(7),isNotNumeric,varIsArgument, &
                                      varNotYetDefined,kindUnknown,0,chargeBlock(iInObj)%tariffIndx)
  END IF
  !number of blocks used
  chargeBlock(iInObj)%numBlk = (NumAlphas - 7) / 2
  DO jBlk = 1, chargeBlock(iInObj)%numBlk
    alphaOffset = 7 +  (jBlk - 1) * 2
    !catch the "remaining" code word for the block size
    IF (sameString(cAlphaArgs(alphaOffset + 1),"REMAINING")) THEN
      chargeBlock(iInObj)%blkSzVal(jBlk) = hugeNumber / 1000000 !using small portion of largest possible value to prevent overflow
      chargeBlock(iInObj)%blkSzPt(jBlk) = 0
    ELSE
      !array of block size
      chargeBlock(iInObj)%blkSzVal(jBlk) = ProcessNumber(cAlphaArgs(alphaOffset + 1),isNotNumeric)

      chargeBlock(iInObj)%blkSzPt(jBlk) = AssignVariablePt(cAlphaArgs(alphaOffset + 1),isNotNumeric,&
                        varIsArgument,varNotYetDefined,kindUnknown,0,chargeBlock(iInObj)%tariffIndx)
    END IF
    !array of block cost
    chargeBlock(iInObj)%blkCostVal(jBlk) = ProcessNumber(cAlphaArgs(alphaOffset + 2),isNotNumeric)
    chargeBlock(iInObj)%blkCostPt(jBlk) = AssignVariablePt(cAlphaArgs(alphaOffset + 2),isNotNumeric,&
                        varIsArgument,varNotYetDefined,kindUnknown,0,chargeBlock(iInObj)%tariffIndx)
  END DO
END DO
END SUBROUTINE GetInputEconomicsChargeBlock

SUBROUTINE GetInputEconomicsRatchet(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Ratchet" objects.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsRatchet: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
!REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
LOGICAL                     :: isNotNumeric
INTEGER                     :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Ratchet'
numRatchet = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(ratchet(numRatchet))
DO iInObj = 1 , numRatchet
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  !index of the tariff name in the tariff array
  ratchet(iInObj)%tariffIndx = FindTariffIndex(cAlphaArgs(2),cAlphaArgs(1),ErrorsFound,CurrentModuleObject)
  CALL warnIfNativeVarname(cAlphaArgs(1),ratchet(iInObj)%tariffIndx,ErrorsFound,CurrentModuleObject)
  ratchet(iInObj)%namePt = AssignVariablePt(cAlphaArgs(1),.TRUE.,varIsAssigned,&
                                varNotYetDefined,kindRatchet,iInObj,ratchet(iInObj)%tariffIndx)
  !index of the variable in the variable array
  ratchet(iInObj)%baselinePt = AssignVariablePt(cAlphaArgs(3),.TRUE.,varIsArgument,&
                                varNotYetDefined,kindRatchet,iInObj,ratchet(iInObj)%tariffIndx)
  !index of the variable in the variable array
  ratchet(iInObj)%adjustmentPt = AssignVariablePt(cAlphaArgs(4),.TRUE.,varIsArgument,&
                                varNotYetDefined,kindRatchet,iInObj,ratchet(iInObj)%tariffIndx)
  ! seasons to and from
  ratchet(iInObj)%seasonFrom = LookUpSeason(cAlphaArgs(5),cAlphaArgs(1))
  ratchet(iInObj)%seasonTo  = LookUpSeason(cAlphaArgs(6),cAlphaArgs(1))
  !ratchet multiplier
  ratchet(iInObj)%multiplierVal = ProcessNumber(cAlphaArgs(7),isNotNumeric)
  ratchet(iInObj)%multiplierPt = AssignVariablePt(cAlphaArgs(7),isNotNumeric,varIsArgument,varNotYetDefined,&
                                 kindUnknown,0,ratchet(iInObj)%tariffIndx)
  !ratchet offset
  ratchet(iInObj)%offsetVal = ProcessNumber(cAlphaArgs(8),isNotNumeric)
  ratchet(iInObj)%offsetPt = AssignVariablePt(cAlphaArgs(8),isNotNumeric,varIsArgument,varNotYetDefined, &
                                kindUnknown,0,ratchet(iInObj)%tariffIndx)
END DO
END SUBROUTINE GetInputEconomicsRatchet

SUBROUTINE GetInputEconomicsVariable(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Variable" objects.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsVariable: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER                     :: numEconVarObj
INTEGER                     :: tariffPt
INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
!REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: jVal
INTEGER                     :: variablePt
INTEGER                     :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Variable'
numEconVarObj = GetNumObjectsFound(CurrentModuleObject)
DO iInObj = 1 , numEconVarObj
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  tariffPt = FindTariffIndex(cAlphaArgs(2),cAlphaArgs(1),ErrorsFound,CurrentModuleObject)
  variablePt = AssignVariablePt(cAlphaArgs(1),.TRUE.,varIsArgument,&
                                varUserDefined,kindVariable,iInObj,tariffPt)
  CALL warnIfNativeVarname(cAlphaArgs(1),tariffPt,ErrorsFound,CurrentModuleObject)
  !validate the kind of variable - not used internally except for validation
  IF (SameString(cAlphaArgs(3),'ENERGY')) THEN
    econVar(variablePt)%varUnitType = varUnitTypeEnergy
  ELSEIF (SameString(cAlphaArgs(3),'DEMAND')) THEN
    econVar(variablePt)%varUnitType = varUnitTypeDemand
  ELSEIF (SameString(cAlphaArgs(3),'DIMENSIONLESS')) THEN
    econVar(variablePt)%varUnitType = varUnitTypeDimensionless
  ELSEIF (SameString(cAlphaArgs(3),'CURRENCY')) THEN
    econVar(variablePt)%varUnitType = varUnitTypeCurrency
  ELSE
    econVar(variablePt)%varUnitType = varUnitTypeDimensionless
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data')
    CALL ShowContinueError('invalid '//trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
    ErrorsFound=.true.
  END IF
  !move number inputs into econVar
  DO jVal = 1, NumNums
    econVar(variablePt)%values(jVal) = rNumericArgs(jVal)
  END DO
  ! fill the rest of the array with the last value entered
  IF (NumNums .LT. MaxNumMonths) THEN
    DO jVal = NumNums + 1, MaxNumMonths
      econVar(variablePt)%values(jVal) = rNumericArgs(numNums)
    END DO
  END IF
END DO
END SUBROUTINE GetInputEconomicsVariable

SUBROUTINE GetInputEconomicsComputation(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Read the input file for "Economics:Computation" objects.
          !    This object is only used for very complex rates.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsComputation: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER                     :: tariffPt
INTEGER                     :: iInObj     ! loop index variable for reading in objects
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(100)  :: cAlphaArgs !character string data
!REAL(r64),                        DIMENSION(100)  :: rNumericArgs  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: jLine
INTEGER                     :: jFld
CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.

CurrentModuleObject = 'UtilityCost:Computation'
numComputation = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(computation(numTariff))  !not the number of Computations but the number of tariffs
!set default values for computation
computation%computeName = ''
computation%firstStep = 0
computation%lastStep = -1
computation%isUserDef = .FALSE.
DO iInObj = 1 , numComputation
  CALL GetObjectItem(CurrentModuleObject,iInObj,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  !check to make sure none of the values are another economic object
  DO jFld = 1, NumAlphas
    IF (INDEX(MakeUpperCase(cAlphaArgs(jFld)),'UTILITYCOST:') .GT. 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)// '="' // TRIM(cAlphaArgs(1)) //'".')
      CALL ShowContinueError('... a field was found containing UtilityCost: which may indicate a missing comma.')
    END IF
  END Do
  tariffPt = FindTariffIndex(cAlphaArgs(2),cAlphaArgs(1),ErrorsFound,CurrentModuleObject)
  CALL warnIfNativeVarname(cAlphaArgs(1),tariffPt,ErrorsFound,CurrentModuleObject)
  !tariff and computation share the same index, the tariff index
  !so all references are to the tariffPt
  IF (isWithinRange(tariffPt,1,numTariff)) THEN
    computation(tariffPt)%computeName = cAlphaArgs(1)
    computation(tariffPt)%firstStep = numSteps + 1
    DO jLine = 3, NumAlphas
      CALL parseComputeLine(cAlphaArgs(jLine),tariffPt)
    END DO
    computation(tariffPt)%lastStep = numSteps
    ! check to make sure that some steps were defined
    IF (computation(tariffPt)%firstStep .GE. computation(tariffPt)%lastStep) THEN
      computation(tariffPt)%firstStep = 0
      computation(tariffPt)%lastStep = -1
      computation(tariffPt)%isUserDef = .FALSE.
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data.')
      CALL ShowContinueError('... No lines in the computation can be interpreted ')
      ErrorsFound=.true.
    ELSE
      computation(tariffPt)%isUserDef = .TRUE.
    END IF
  ELSE
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data.')
    CALL ShowContinueError('... not found '//trim(cAlphaFieldNames(2))//'="'// TRIM(cAlphaArgs(2))//'".')
    ErrorsFound=.true.
  END IF
END DO
END SUBROUTINE GetInputEconomicsComputation

SUBROUTINE GetInputEconomicsCurrencyType(ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Sets the type of currency (U.S. Dollar, Euro, Yen, etc.. )
          !   This is a "unique" object.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataInterfaces, ONLY :  ShowWarningError
USE DataCostEstimate
USE DataIPShortCuts

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors found during getting input objects.

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='CurrencyType'
CHARACTER(len=*), PARAMETER :: RoutineName='GetInputEconomicsCurrencyType: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER                     :: NumCurrencyType
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
!CHARACTER(len=MaxNameLength),DIMENSION(5) :: cAlphaArgs !character string data - should be 1
!REAL(r64),                   DIMENSION(5) :: rNumericArgs  !numeric data          - should be 0
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: i

CALL initializeMonetaryUnit
NumCurrencyType = GetNumObjectsFound(CurrentModuleObject)
selectedMonetaryUnit = 0 ! invalid
IF (NumCurrencyType .EQ. 0) THEN
  selectedMonetaryUnit = 1 !USD - U.S. Dollar
ELSEIF (NumCurrencyType .EQ. 1) THEN
  CALL GetObjectItem(CurrentModuleObject,1,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  ! Monetary Unit
  DO i = 1, numMonetaryUnit
    IF (SameString(cAlphaArgs(1),monetaryUnit(i)%code)) THEN
      selectedMonetaryUnit = i
      EXIT
    END IF
  END DO
  IF (selectedMonetaryUnit == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid data.')
    CALL ShowContinueError('... invalid '//trim(cAlphaFieldNames(1))//'.')
    ErrorsFound=.true.
  ENDIF
ELSEIF (NumCurrencyType .GT. 1) THEN
  CALL ShowWarningError(RoutineName//CurrentModuleObject//  &
     ' Only one instance of this object is allowed. USD will be used.')
  selectedMonetaryUnit = 1 !USD - U.S. Dollar
ENDIF
END SUBROUTINE GetInputEconomicsCurrencyType

SUBROUTINE parseComputeLine(lineOfCompute,fromTariff)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   June 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Converts a single line in the ECONOMICS:COMPUTE
          !   command into tokens for computation

          ! METHODOLOGY EMPLOYED:
          !   Scan the line from the end of the line to the front of the
          !   line and search for operators and variables. All items
          !   are put into the step array.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=MaxNameLength), INTENT(IN)   :: lineOfCompute
INTEGER, INTENT(IN)                        :: fromTariff
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=MaxNameLength)   :: word
INTEGER                        :: endOfWord
INTEGER                        :: token

endOfWord = LEN_TRIM(lineOfCompute)
DO WHILE (endOfWord .GT. 0)
  ! get a single word (text string delimited by spaces)
  CALL GetLastWord(lineOfCompute,endOfWord,word)
  ! first see if word is an operator
  token = lookupOperator(word)
  ! if not an operator then look for
  IF (token .EQ. 0) THEN
    ! see if argument or assignment (assignment will be first string on line)
    IF (endOfWord .GT. 0) THEN
      token =  AssignVariablePt(word,.TRUE.,varIsArgument,varNotYetDefined,kindUnknown,0,fromTariff)
    ELSE
      token =  AssignVariablePt(word,.TRUE.,varIsAssigned,varNotYetDefined,kindAssignCompute,0,fromTariff)
    ENDIF
  END IF
  ! if a token is found then put it into step array
  IF (token .EQ. 0) THEN
    CALL ShowWarningError('In UtilityCost:Computation line: ' // TRIM(lineOfCompute))
    CALL ShowContinueError('  Do not recognize: ' // TRIM(word) // ' Will skip.')
  ELSE
    CALL incrementSteps
    steps(numSteps) = token
  END IF
END DO
CALL incrementSteps
steps(numSteps) = 0  !at the end of the line show a zero to clear the stack
END SUBROUTINE parseComputeLine

SUBROUTINE GetLastWord(lineOfText,endOfScan,aWord)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   June 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Returns the last substring of the line of text to the
          !   left of the endOfSubStrg pointer. A substring is
          !   delimitted by spaces.  Quotes are not significant
          !   (they are treated just like any other non-space character)

          ! METHODOLOGY EMPLOYED:
          !   Scan the string from the end.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=MaxNameLength),INTENT(IN)    :: lineOfText
INTEGER, INTENT(INOUT)                     :: endOfScan
CHARACTER(len=MaxNameLength),INTENT(OUT)   :: aWord


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iString
INTEGER :: curEndOfScan
LOGICAL :: isInWord
LOGICAL :: isSpace
INTEGER :: beginOfWord
INTEGER :: endOfWord

curEndOfScan = endOfScan
IF (curEndOfScan .GT. 0) THEN
  IF (curEndOfScan .GT. LEN_TRIM(lineOfText)) THEN
    curEndOfScan = LEN_TRIM(lineOfText)
  END IF
  !check if currently on a space or not
  IF (lineOfText(curEndOfScan:curEndOfScan) .EQ. ' ') THEN
    isInWord = .FALSE.
    beginOfWord = 0
    endOfWord = 0
  ELSE
    isInWord = .TRUE.
    beginOfWord = curEndOfScan
    endOfWord = curEndOfScan
  END IF
  !scan backwards from
  DO iString=curEndOfScan,1,-1
    IF (lineOfText(iString:iString) .EQ. ' ') THEN
      isSpace = .TRUE.
    ELSE
      isSpace = .FALSE.
    END IF
    ! all logical conditions of isSpace and isInWord
    IF (isSpace) THEN
      IF (isInWord) THEN
        !found the space in front of the word
        EXIT
      ELSE
        !still have not found the back of the word
        ! do nothing
      END IF
    ELSE
      IF (isInWord) THEN
        !still have not found the space in front of the word
        beginOfWord = iString
      ELSE
        !found the last character of the word
        endOfWord = iString
        beginOfWord = iString
        isInWord = .TRUE.
      END IF
    END IF
  END DO
  aWord = lineOfText(beginOfWord:endOfWord)
  endOfScan = beginOfWord - 1
  IF (endOfScan .LT. 0) THEN
    endOfScan = 0
  END IF
ELSE
  endOfScan = 0
  aWord = ''
END IF
END SUBROUTINE GetLastWord

SUBROUTINE initializeMonetaryUnit
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Sets the type of monetary unit array.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects
          !   The monetaryUnitSymbols.xls spreadsheet helps create the code for this routine

          ! REFERENCES:
          !   www.xe.com/symbols.php

          ! USE STATEMENTS:
USE DataCostEstimate

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
numMonetaryUnit = 111
ALLOCATE(monetaryUnit(numMonetaryUnit))
monetaryUnit(1)%code ='USD'
monetaryUnit(2)%code ='AFN'
monetaryUnit(3)%code ='ALL'
monetaryUnit(4)%code ='ANG'
monetaryUnit(5)%code ='ARS'
monetaryUnit(6)%code ='AUD'
monetaryUnit(7)%code ='AWG'
monetaryUnit(8)%code ='AZN'
monetaryUnit(9)%code ='BAM'
monetaryUnit(10)%code ='BBD'
monetaryUnit(11)%code ='BGN'
monetaryUnit(12)%code ='BMD'
monetaryUnit(13)%code ='BND'
monetaryUnit(14)%code ='BOB'
monetaryUnit(15)%code ='BRL'
monetaryUnit(16)%code ='BSD'
monetaryUnit(17)%code ='BWP'
monetaryUnit(18)%code ='BYR'
monetaryUnit(19)%code ='BZD'
monetaryUnit(20)%code ='CAD'
monetaryUnit(21)%code ='CHF'
monetaryUnit(22)%code ='CLP'
monetaryUnit(23)%code ='CNY'
monetaryUnit(24)%code ='COP'
monetaryUnit(25)%code ='CRC'
monetaryUnit(26)%code ='CUP'
monetaryUnit(27)%code ='CZK'
monetaryUnit(28)%code ='DKK'
monetaryUnit(29)%code ='DOP'
monetaryUnit(30)%code ='EEK'
monetaryUnit(31)%code ='EGP'
monetaryUnit(32)%code ='EUR'
monetaryUnit(33)%code ='FJD'
monetaryUnit(34)%code ='GBP'
monetaryUnit(35)%code ='GHC'
monetaryUnit(36)%code ='GIP'
monetaryUnit(37)%code ='GTQ'
monetaryUnit(38)%code ='GYD'
monetaryUnit(39)%code ='HKD'
monetaryUnit(40)%code ='HNL'
monetaryUnit(41)%code ='HRK'
monetaryUnit(42)%code ='HUF'
monetaryUnit(43)%code ='IDR'
monetaryUnit(44)%code ='ILS'
monetaryUnit(45)%code ='IMP'
monetaryUnit(46)%code ='INR'
monetaryUnit(47)%code ='IRR'
monetaryUnit(48)%code ='ISK'
monetaryUnit(49)%code ='JEP'
monetaryUnit(50)%code ='JMD'
monetaryUnit(51)%code ='JPY'
monetaryUnit(52)%code ='KGS'
monetaryUnit(53)%code ='KHR'
monetaryUnit(54)%code ='KPW'
monetaryUnit(55)%code ='KRW'
monetaryUnit(56)%code ='KYD'
monetaryUnit(57)%code ='KZT'
monetaryUnit(58)%code ='LAK'
monetaryUnit(59)%code ='LBP'
monetaryUnit(60)%code ='LKR'
monetaryUnit(61)%code ='LRD'
monetaryUnit(62)%code ='LTL'
monetaryUnit(63)%code ='LVL'
monetaryUnit(64)%code ='MKD'
monetaryUnit(65)%code ='MNT'
monetaryUnit(66)%code ='MUR'
monetaryUnit(67)%code ='MXN'
monetaryUnit(68)%code ='MYR'
monetaryUnit(69)%code ='MZN'
monetaryUnit(70)%code ='NAD'
monetaryUnit(71)%code ='NGN'
monetaryUnit(72)%code ='NIO'
monetaryUnit(73)%code ='NOK'
monetaryUnit(74)%code ='NPR'
monetaryUnit(75)%code ='NZD'
monetaryUnit(76)%code ='OMR'
monetaryUnit(77)%code ='PAB'
monetaryUnit(78)%code ='PEN'
monetaryUnit(79)%code ='PHP'
monetaryUnit(80)%code ='PKR'
monetaryUnit(81)%code ='PLN'
monetaryUnit(82)%code ='PYG'
monetaryUnit(83)%code ='QAR'
monetaryUnit(84)%code ='RON'
monetaryUnit(85)%code ='RSD'
monetaryUnit(86)%code ='RUB'
monetaryUnit(87)%code ='SAR'
monetaryUnit(88)%code ='SBD'
monetaryUnit(89)%code ='SCR'
monetaryUnit(90)%code ='SEK'
monetaryUnit(91)%code ='SGD'
monetaryUnit(92)%code ='SHP'
monetaryUnit(93)%code ='SOS'
monetaryUnit(94)%code ='SRD'
monetaryUnit(95)%code ='SVC'
monetaryUnit(96)%code ='SYP'
monetaryUnit(97)%code ='THB'
monetaryUnit(98)%code ='TRL'
monetaryUnit(99)%code ='TRY'
monetaryUnit(100)%code ='TTD'
monetaryUnit(101)%code ='TVD'
monetaryUnit(102)%code ='TWD'
monetaryUnit(103)%code ='UAH'
monetaryUnit(104)%code ='UYU'
monetaryUnit(105)%code ='UZS'
monetaryUnit(106)%code ='VEF'
monetaryUnit(107)%code ='VND'
monetaryUnit(108)%code ='XCD'
monetaryUnit(109)%code ='YER'
monetaryUnit(110)%code ='ZAR'
monetaryUnit(111)%code ='ZWD'

monetaryUnit(1)%txt ='$'
monetaryUnit(2)%txt ='AFN'
monetaryUnit(3)%txt ='Lek'
monetaryUnit(4)%txt ='ANG'
monetaryUnit(5)%txt ='$'
monetaryUnit(6)%txt ='$'
monetaryUnit(7)%txt ='AWG'
monetaryUnit(8)%txt ='AZN'
monetaryUnit(9)%txt ='KM'
monetaryUnit(10)%txt ='$'
monetaryUnit(11)%txt ='BGN'
monetaryUnit(12)%txt ='$'
monetaryUnit(13)%txt ='$'
monetaryUnit(14)%txt ='$b'
monetaryUnit(15)%txt ='R$'
monetaryUnit(16)%txt ='$'
monetaryUnit(17)%txt ='P'
monetaryUnit(18)%txt ='p.'
monetaryUnit(19)%txt ='BZ$'
monetaryUnit(20)%txt ='$'
monetaryUnit(21)%txt ='CHF'
monetaryUnit(22)%txt ='$'
monetaryUnit(23)%txt ='CNY'
monetaryUnit(24)%txt ='$'
monetaryUnit(25)%txt ='CRC'
monetaryUnit(26)%txt ='CUP'
monetaryUnit(27)%txt ='CZK'
monetaryUnit(28)%txt ='kr'
monetaryUnit(29)%txt ='RD$'
monetaryUnit(30)%txt ='kr'
monetaryUnit(31)%txt ='£'
monetaryUnit(32)%txt ='EUR'
monetaryUnit(33)%txt ='$'
monetaryUnit(34)%txt ='£'
monetaryUnit(35)%txt ='¢'
monetaryUnit(36)%txt ='£'
monetaryUnit(37)%txt ='Q'
monetaryUnit(38)%txt ='$'
monetaryUnit(39)%txt ='HK$'
monetaryUnit(40)%txt ='L'
monetaryUnit(41)%txt ='kn'
monetaryUnit(42)%txt ='Ft'
monetaryUnit(43)%txt ='Rp'
monetaryUnit(44)%txt ='ILS'
monetaryUnit(45)%txt ='£'
monetaryUnit(46)%txt ='INR'
monetaryUnit(47)%txt ='IRR'
monetaryUnit(48)%txt ='kr'
monetaryUnit(49)%txt ='£'
monetaryUnit(50)%txt ='J$'
monetaryUnit(51)%txt ='¥'
monetaryUnit(52)%txt ='KGS'
monetaryUnit(53)%txt ='KHR'
monetaryUnit(54)%txt ='KPW'
monetaryUnit(55)%txt ='KRW'
monetaryUnit(56)%txt ='$'
monetaryUnit(57)%txt ='KZT'
monetaryUnit(58)%txt ='LAK'
monetaryUnit(59)%txt ='£'
monetaryUnit(60)%txt ='LKR'
monetaryUnit(61)%txt ='$'
monetaryUnit(62)%txt ='Lt'
monetaryUnit(63)%txt ='Ls'
monetaryUnit(64)%txt ='MKD'
monetaryUnit(65)%txt ='MNT'
monetaryUnit(66)%txt ='MUR'
monetaryUnit(67)%txt ='$'
monetaryUnit(68)%txt ='RM'
monetaryUnit(69)%txt ='MT'
monetaryUnit(70)%txt ='$'
monetaryUnit(71)%txt ='NGN'
monetaryUnit(72)%txt ='C$'
monetaryUnit(73)%txt ='kr'
monetaryUnit(74)%txt ='NPR'
monetaryUnit(75)%txt ='$'
monetaryUnit(76)%txt ='OMR'
monetaryUnit(77)%txt ='B/.'
monetaryUnit(78)%txt ='S/.'
monetaryUnit(79)%txt ='Php'
monetaryUnit(80)%txt ='PKR'
monetaryUnit(81)%txt ='PLN'
monetaryUnit(82)%txt ='Gs'
monetaryUnit(83)%txt ='QAR'
monetaryUnit(84)%txt ='lei'
monetaryUnit(85)%txt ='RSD'
monetaryUnit(86)%txt ='RUB'
monetaryUnit(87)%txt ='SAR'
monetaryUnit(88)%txt ='$'
monetaryUnit(89)%txt ='SCR'
monetaryUnit(90)%txt ='kr'
monetaryUnit(91)%txt ='$'
monetaryUnit(92)%txt ='£'
monetaryUnit(93)%txt ='S'
monetaryUnit(94)%txt ='$'
monetaryUnit(95)%txt ='$'
monetaryUnit(96)%txt ='£'
monetaryUnit(97)%txt ='THB'
monetaryUnit(98)%txt ='TRL'
monetaryUnit(99)%txt ='YTL'
monetaryUnit(100)%txt ='TT$'
monetaryUnit(101)%txt ='$'
monetaryUnit(102)%txt ='NT$'
monetaryUnit(103)%txt ='UAH'
monetaryUnit(104)%txt ='$U'
monetaryUnit(105)%txt ='UZS'
monetaryUnit(106)%txt ='Bs'
monetaryUnit(107)%txt ='VND'
monetaryUnit(108)%txt ='$'
monetaryUnit(109)%txt ='YER'
monetaryUnit(110)%txt ='R'
monetaryUnit(111)%txt ='Z$'

monetaryUnit(1)%html ='$'
monetaryUnit(2)%html ='&#x060b;'
monetaryUnit(3)%html ='Lek'
monetaryUnit(4)%html ='&#x0192;'
monetaryUnit(5)%html ='$'
monetaryUnit(6)%html ='$'
monetaryUnit(7)%html ='&#x0192;'
monetaryUnit(8)%html ='&#x043c;&#x0430;&#x043d;'
monetaryUnit(9)%html ='KM'
monetaryUnit(10)%html ='$'
monetaryUnit(11)%html ='&#x043b;&#x0432;'
monetaryUnit(12)%html ='$'
monetaryUnit(13)%html ='$'
monetaryUnit(14)%html ='$b'
monetaryUnit(15)%html ='R$'
monetaryUnit(16)%html ='$'
monetaryUnit(17)%html ='P'
monetaryUnit(18)%html ='p.'
monetaryUnit(19)%html ='BZ$'
monetaryUnit(20)%html ='$'
monetaryUnit(21)%html ='CHF'
monetaryUnit(22)%html ='$'
monetaryUnit(23)%html ='&#x5143;'
monetaryUnit(24)%html ='$'
monetaryUnit(25)%html ='&#x20a1;'
monetaryUnit(26)%html ='&#x20b1;'
monetaryUnit(27)%html ='&#x004b;&#x010d;'
monetaryUnit(28)%html ='kr'
monetaryUnit(29)%html ='RD$'
monetaryUnit(30)%html ='kr'
monetaryUnit(31)%html ='£'
monetaryUnit(32)%html ='&#x20ac;'
monetaryUnit(33)%html ='$'
monetaryUnit(34)%html ='£'
monetaryUnit(35)%html ='¢'
monetaryUnit(36)%html ='£'
monetaryUnit(37)%html ='Q'
monetaryUnit(38)%html ='$'
monetaryUnit(39)%html ='HK$'
monetaryUnit(40)%html ='L'
monetaryUnit(41)%html ='kn'
monetaryUnit(42)%html ='Ft'
monetaryUnit(43)%html ='Rp'
monetaryUnit(44)%html ='&#x20aa;'
monetaryUnit(45)%html ='£'
monetaryUnit(46)%html ='&#x20a8;'
monetaryUnit(47)%html ='&#xfdfc;'
monetaryUnit(48)%html ='kr'
monetaryUnit(49)%html ='£'
monetaryUnit(50)%html ='J$'
monetaryUnit(51)%html ='¥'
monetaryUnit(52)%html ='&#x043b;&#x0432;'
monetaryUnit(53)%html ='&#x17db;'
monetaryUnit(54)%html ='&#x20a9;'
monetaryUnit(55)%html ='&#x20a9;'
monetaryUnit(56)%html ='$'
monetaryUnit(57)%html ='&#x043b;&#x0432;'
monetaryUnit(58)%html ='&#x20ad;'
monetaryUnit(59)%html ='£'
monetaryUnit(60)%html ='&#x20a8;'
monetaryUnit(61)%html ='$'
monetaryUnit(62)%html ='Lt'
monetaryUnit(63)%html ='Ls'
monetaryUnit(64)%html ='&#x0434;&#x0435;&#x043d;'
monetaryUnit(65)%html ='&#x20ae;'
monetaryUnit(66)%html ='&#x20a8;'
monetaryUnit(67)%html ='$'
monetaryUnit(68)%html ='RM'
monetaryUnit(69)%html ='MT'
monetaryUnit(70)%html ='$'
monetaryUnit(71)%html ='&#x20a6;'
monetaryUnit(72)%html ='C$'
monetaryUnit(73)%html ='kr'
monetaryUnit(74)%html ='&#x20a8;'
monetaryUnit(75)%html ='$'
monetaryUnit(76)%html ='&#xfdfc;'
monetaryUnit(77)%html ='B/.'
monetaryUnit(78)%html ='S/.'
monetaryUnit(79)%html ='Php'
monetaryUnit(80)%html ='&#x20a8;'
monetaryUnit(81)%html ='&#x007a;&#x0142;'
monetaryUnit(82)%html ='Gs'
monetaryUnit(83)%html ='&#xfdfc;'
monetaryUnit(84)%html ='lei'
monetaryUnit(85)%html ='&#x0414;&#x0438;&#x043d;&#x002e;'
monetaryUnit(86)%html ='&#x0440;&#x0443;&#x0431;'
monetaryUnit(87)%html ='&#xfdfc;'
monetaryUnit(88)%html ='$'
monetaryUnit(89)%html ='&#x20a8;'
monetaryUnit(90)%html ='kr'
monetaryUnit(91)%html ='$'
monetaryUnit(92)%html ='£'
monetaryUnit(93)%html ='S'
monetaryUnit(94)%html ='$'
monetaryUnit(95)%html ='$'
monetaryUnit(96)%html ='£'
monetaryUnit(97)%html ='&#x0e3f;'
monetaryUnit(98)%html ='&#x20a4;'
monetaryUnit(99)%html ='YTL'
monetaryUnit(100)%html ='TT$'
monetaryUnit(101)%html ='$'
monetaryUnit(102)%html ='NT$'
monetaryUnit(103)%html ='&#x20b4;'
monetaryUnit(104)%html ='$U'
monetaryUnit(105)%html ='&#x043b;&#x0432;'
monetaryUnit(106)%html ='Bs'
monetaryUnit(107)%html ='&#x20ab;'
monetaryUnit(108)%html ='$'
monetaryUnit(109)%html ='&#xfdfc;'
monetaryUnit(110)%html ='R'
monetaryUnit(111)%html ='Z$'
END SUBROUTINE initializeMonetaryUnit


INTEGER FUNCTION LookUpSeason(nameOfSeason,nameOfReferingObj)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Find the index for the season string provided or else
          !    raise a warning.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: nameOfSeason
CHARACTER(len=*), INTENT(IN) :: nameOfReferingObj

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (SameString(nameOfSeason,'Summer')) THEN
  LookUpSeason = seasonSummer
ELSE IF (SameString(nameOfSeason,'Winter')) THEN
  LookUpSeason = seasonWinter
ELSE IF (SameString(nameOfSeason,'Spring')) THEN
  LookUpSeason = seasonSpring
ELSE IF (SameString(nameOfSeason,'Fall')) THEN
  LookUpSeason = seasonFall
ELSE IF (SameString(nameOfSeason,'Annual')) THEN
  LookUpSeason = seasonAnnual
ELSE
  CALL ShowWarningError('UtilityCost: Invalid season name ' // TRIM(nameOfSeason) // ' in: ' // TRIM(nameOfReferingObj))
  CALL ShowContinueError('  Defaulting to Annual')
  LookUpSeason = seasonAnnual
END IF
END FUNCTION LookUpSeason

INTEGER FUNCTION FindTariffIndex(nameOfTariff,nameOfReferingObj,ErrorsFound,nameOfCurObj)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Find the index for the tariff string provided or else
          !    raise a warning.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: nameOfTariff
CHARACTER(len=*), INTENT(IN) :: nameOfReferingObj
LOGICAL, INTENT(INOUT)       :: ErrorsFound
CHARACTER(len=*), INTENT(IN) :: nameOfCurObj

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iTariff
INTEGER :: found

found = 0
DO iTariff = 1, numTariff
  IF (SameString(nameOfTariff, tariff(iTariff)%tariffName)) THEN
    found = iTariff
    EXIT
  END IF
END DO
IF (found .GT. 0) THEN
  FindTariffIndex = found
ELSE
  CALL ShowSevereError(trim(nameOfCurObj)//'="'//trim(nameOfReferingObj)//'" invalid tariff referenced')
  CALL ShowContinueError ('not found UtilityCost:Tariff="'//trim(nameOfTariff)//'".')
  ErrorsFound=.true.
  FindTariffIndex = 0
END IF
END FUNCTION FindTariffIndex

SUBROUTINE warnIfNativeVarname(objName,curTariffIndex,ErrorsFound,curobjName)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   March 2007
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Issue a warning if the variable name (usually the object name) is
          !   one of the names of native variables

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN)  :: objName
INTEGER, INTENT(IN)           :: curTariffIndex
LOGICAL, INTENT(INOUT)        :: ErrorsFound
CHARACTER(len=*), INTENT(IN)  :: curobjName

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL    :: throwError

throwError = .FALSE.
IF (SameString(objName,'TotalEnergy')) throwError = .TRUE.
IF (SameString(objName,'TotalDemand')) throwError = .TRUE.
IF (SameString(objName,'PeakEnergy')) throwError = .TRUE.
IF (SameString(objName,'PeakDemand')) throwError = .TRUE.
IF (SameString(objName,'ShoulderEnergy')) throwError = .TRUE.
IF (SameString(objName,'ShoulderDemand')) throwError = .TRUE.
IF (SameString(objName,'OffPeakEnergy')) throwError = .TRUE.
IF (SameString(objName,'OffPeakDemand')) throwError = .TRUE.
IF (SameString(objName,'MidPeakEnergy')) throwError = .TRUE.
IF (SameString(objName,'MidPeakDemand')) throwError = .TRUE.
IF (SameString(objName,'PeakExceedsOffPeak')) throwError = .TRUE.
IF (SameString(objName,'OffPeakExceedsPeak')) throwError = .TRUE.
IF (SameString(objName,'PeakExceedsMidPeak')) throwError = .TRUE.
IF (SameString(objName,'MidPeakExceedsPeak')) throwError = .TRUE.
IF (SameString(objName,'PeakExceedsShoulder')) throwError = .TRUE.
IF (SameString(objName,'ShoulderExceedsPeak')) throwError = .TRUE.
IF (SameString(objName,'IsWinter')) throwError = .TRUE.
IF (SameString(objName,'IsNotWinter')) throwError = .TRUE.
IF (SameString(objName,'IsSpring')) throwError = .TRUE.
IF (SameString(objName,'IsNotSpring')) throwError = .TRUE.
IF (SameString(objName,'IsSummer')) throwError = .TRUE.
IF (SameString(objName,'IsNotSummer')) throwError = .TRUE.
IF (SameString(objName,'IsAutumn')) throwError = .TRUE.
IF (SameString(objName,'IsNotAutumn')) throwError = .TRUE.
IF (SameString(objName,'PeakAndShoulderEnergy')) throwError = .TRUE.
IF (SameString(objName,'PeakAndShoulderDemand')) throwError = .TRUE.
IF (SameString(objName,'PeakAndMidPeakEnergy')) throwError = .TRUE.
IF (SameString(objName,'PeakAndMidPeakDemand')) throwError = .TRUE.
IF (SameString(objName,'ShoulderAndOffPeakEnergy')) throwError = .TRUE.
IF (SameString(objName,'ShoulderAndOffPeakDemand')) throwError = .TRUE.
IF (SameString(objName,'PeakAndOffPeakEnergy')) throwError = .TRUE.
IF (SameString(objName,'PeakAndOffPeakDemand')) throwError = .TRUE.
IF (SameString(objName,'RealTimePriceCosts')) throwError = .TRUE.
IF (SameString(objName,'AboveCustomerBaseCosts')) throwError = .TRUE.
IF (SameString(objName,'BelowCustomerBaseCosts')) throwError = .TRUE.
IF (SameString(objName,'AboveCustomerBaseEnergy')) throwError = .TRUE.
IF (SameString(objName,'BelowCustomerBaseEnergy')) throwError = .TRUE.
IF (SameString(objName,'EnergyCharges')) throwError = .TRUE.
IF (SameString(objName,'DemandCharges')) throwError = .TRUE.
IF (SameString(objName,'ServiceCharges')) throwError = .TRUE.
IF (SameString(objName,'Basis')) throwError = .TRUE.
IF (SameString(objName,'Surcharges')) throwError = .TRUE.
IF (SameString(objName,'Adjustments')) throwError = .TRUE.
IF (SameString(objName,'Subtotal')) throwError = .TRUE.
IF (SameString(objName,'Taxes')) throwError = .TRUE.
IF (SameString(objName,'Total')) throwError = .TRUE.
IF (throwError) THEN
  ErrorsFound=.true.
  IF (curTariffIndex .GE. 1 .AND. curTariffIndex .LE. numTariff) THEN
    CALL ShowSevereError('UtilityCost:Tariff="'//trim(tariff(curTariffIndex)%tariffName)//'" invalid referenced name')
    CALL ShowContinueError(trim(curobjName)//'="'//trim(objName)//  &
       '" You cannot name an object using the same name as a native variable.')
  ELSE
    CALL ShowSevereError(trim(curobjname)//'="'//trim(objname)//  &
       '" You cannot name an object using the same name as a native variable.')
  END IF
END IF
END SUBROUTINE warnIfNativeVarname

INTEGER FUNCTION AssignVariablePt(stringIn,flagIfNotNumeric,useOfVar,varSpecific,econObjKind,objIndex,tariffPt)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   If the string is not numeric, check if it is a valid string to use as
          !   a variable name. Check if name has been used before and if not create
          !   the variable using the string as its name.
          !   Return the index of the variable.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: stringIn
LOGICAL, INTENT(IN)          :: flagIfNotNumeric
INTEGER, INTENT(IN)          :: useOfVar
INTEGER, INTENT(IN)          :: econObjKind
INTEGER, INTENT(IN)          :: varSpecific
INTEGER, INTENT(IN)          :: objIndex
INTEGER, INTENT(IN)          :: tariffPt
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=200)          :: inNoSpaces
INTEGER                     :: found
INTEGER                     :: iVar

IF (flagIfNotNumeric .AND. (LEN_TRIM(stringIn) .GE. 1)) THEN
  inNoSpaces = RemoveSpaces(stringIn)
  found = 0
  IF (ALLOCATED(econVar)) THEN
    DO iVar = 1, numEconVar
      IF (econVar(iVar)%tariffIndx .EQ. tariffPt) THEN
        IF (SameString(econVar(iVar)%name,inNoSpaces)) THEN
          found = iVar
          EXIT
        END IF
      END IF
    END DO
  END IF
  IF (found .GT. 0) THEN
    AssignVariablePt = found
    IF (econVar(found)%kindOfObj .EQ. 0) THEN
      econVar(found)%kindOfObj = econObjKind
      IF (econVar(found)%index .EQ. 0) econVar(found)%index = objIndex
    END IF
  ELSE
    CALL incrementEconVar
    econVar(numEconVar)%name = inNoSpaces
    econVar(numEconVar)%kindOfObj = econObjKind
    econVar(numEconVar)%index = objIndex
    AssignVariablePt = numEconVar
  END IF
  ! now set the flag for the type of usage the variable has
  IF (useOfVar .EQ. varIsArgument) THEN
    econVar(AssignVariablePt)%isArgument = .TRUE.
  ELSE IF (useOfVar .EQ. varIsAssigned) THEN
    econVar(AssignVariablePt)%isAssigned = .TRUE.
  END IF
  econVar(AssignVariablePt)%tariffIndx = tariffPt
  ! if the user defines the UtilityCost:Computation then this is called when reading the
  ! UtilityCost:Tariff with varNotYetDefined but they are already defined because
  ! the subroutine CreateCategoryNativeVariables has already been called.
  IF (.NOT. ((varSpecific .EQ. varNotYetDefined) .AND. (econVar(AssignVariablePt)%specific .GE. catEnergyCharges))) THEN
    econVar(AssignVariablePt)%specific = varSpecific
  END IF
ELSE !if the string was numeric return a zero
  AssignVariablePt = 0
END IF
END FUNCTION AssignVariablePt

SUBROUTINE incrementEconVar
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Increment the Increase the size of the

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER :: sizeIncrement = 100

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (.NOT. ALLOCATED(econVar)) THEN
  ALLOCATE(econVar(sizeIncrement))
  sizeEconVar = sizeIncrement
  numEconVar = 1
ELSE
  numEconVar = numEconVar + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numEconVar .GT. sizeEconVar) THEN
    ALLOCATE(econVarCopy(sizeEconVar))
    econVarCopy = econVar
    DEALLOCATE(econVar)
    ALLOCATE(econVar(sizeEconVar + sizeIncrement))
    econVar(1:sizeEconVar) = econVarCopy
    DEALLOCATE(econVarCopy)
    sizeEconVar = sizeEconVar + sizeIncrement
  END IF
END IF
! initialize new record)
econVar(numEconVar)%name = ''
econVar(numEconVar)%tariffIndx = 0
econVar(numEconVar)%kindOfObj = 0
econVar(numEconVar)%index = 0
econVar(numEconVar)%values = 0.0d0
econVar(numEconVar)%isArgument = .FALSE.
econVar(numEconVar)%isAssigned = .FALSE.
econVar(numEconVar)%specific = varNotYetDefined
econVar(numEconVar)%values = 0.0d0
econVar(numEconVar)%operator = 0
econVar(numEconVar)%firstOperand = 1
econVar(numEconVar)%lastOperand = 0
econVar(numEconVar)%activeNow = .FALSE.
econVar(numEconVar)%isEvaluated = .FALSE.
END SUBROUTINE incrementEconVar

SUBROUTINE incrementSteps
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   June 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Increment the step array counter and if
          !   necessary increase the size of the array.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER :: sizeIncrement = 100

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (.NOT. ALLOCATED(steps)) THEN
  ALLOCATE(steps(sizeIncrement))
  sizeSteps = sizeIncrement
  numSteps = 1
ELSE
  numSteps = numSteps + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (numSteps .GT. sizeSteps) THEN
    ALLOCATE(stepsCopy(sizeSteps))
    stepsCopy = steps
    DEALLOCATE(steps)
    ALLOCATE(steps(sizeSteps + sizeIncrement))
    steps(1:sizeSteps) = stepsCopy
    DEALLOCATE(stepsCopy)
    sizeSteps = sizeSteps + sizeIncrement
  END IF
END IF
! initialize new record)
steps(numSteps) = 0
END SUBROUTINE incrementSteps

FUNCTION RemoveSpaces(StringIn) RESULT(StringOut)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Return the string with all spaces removed.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: StringIn
CHARACTER(len=200)           :: StringOut

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iString
LOGICAL :: foundSpaces

StringOut = ''
foundSpaces = .FALSE.
DO iString = 1,LEN_TRIM(StringIn)
  IF (StringIn(iString:iString) .NE. ' ') THEN
    StringOut = TRIM(StringOut) // StringIn(iString:iString)
  ELSE
    foundSpaces = .TRUE.
  END IF
END DO
IF (foundSpaces) THEN
  CALL ShowWarningError('UtilityCost: Spaces were removed from the variable="' // TRIM(StringIn) // '".')
  CALL ShowContinueError('...Resultant variable="'//trim(StringOut)//'".')
END IF
END FUNCTION RemoveSpaces

SUBROUTINE CreateCategoryNativeVariables
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    For each tariff create variables that are used for the
          !    categories (i.e., EnergyCharges).

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
INTEGER :: iTariff

DO iTariff = 1, numTariff
! category variables first
  tariff(iTariff)%ptEnergyCharges =  AssignVariablePt("EnergyCharges",.TRUE.,varIsAssigned,&
                                     catEnergyCharges,kindCategory,0,iTariff)
  tariff(iTariff)%firstCategory    = numEconVar
  tariff(iTariff)%ptDemandCharges =  AssignVariablePt("DemandCharges",.TRUE.,varIsAssigned,&
                                     catDemandCharges,kindCategory,0,iTariff)
  tariff(iTariff)%ptServiceCharges = AssignVariablePt("ServiceCharges",.TRUE.,varIsAssigned,&
                                     catServiceCharges,kindCategory,0,iTariff)
  tariff(iTariff)%ptBasis =          AssignVariablePt("Basis",.TRUE.,varIsAssigned,&
                                     catBasis,kindCategory,0,iTariff)
  tariff(iTariff)%ptAdjustment =     AssignVariablePt("Adjustment",.TRUE.,varIsAssigned,&
                                     catAdjustment,kindCategory,0,iTariff)
  tariff(iTariff)%ptSurcharge =      AssignVariablePt("Surcharge",.TRUE.,varIsAssigned,&
                                     catSurcharge,kindCategory,0,iTariff)
  tariff(iTariff)%ptSubtotal =       AssignVariablePt("Subtotal",.TRUE.,varIsAssigned,&
                                     catSubtotal,kindCategory,0,iTariff)
  tariff(iTariff)%ptTaxes =          AssignVariablePt("Taxes",.TRUE.,varIsAssigned,&
                                     catTaxes,kindCategory,0,iTariff)
  tariff(iTariff)%ptTotal =          AssignVariablePt("Total",.TRUE.,varIsAssigned,&
                                     catTotal,kindCategory,0,iTariff)
  tariff(iTariff)%ptNotIncluded =    AssignVariablePt("NotIncluded",.TRUE.,varIsAssigned,&
                                     catNotIncluded,kindCategory,0,iTariff)
  tariff(iTariff)%lastCategory     = numEconVar
! category variables first
  tariff(iTariff)%nativeTotalEnergy    = AssignVariablePt("TotalEnergy",.TRUE.,varIsArgument,&
                                         nativeTotalEnergy,kindNative,0,iTariff)
  tariff(iTariff)%firstNative          = numEconVar
  tariff(iTariff)%nativeTotalDemand    = AssignVariablePt("TotalDemand",.TRUE.,varIsArgument,&
                                         nativeTotalDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakEnergy     = AssignVariablePt("PeakEnergy",.TRUE.,varIsArgument,&
                                         nativePeakEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakDemand     = AssignVariablePt("PeakDemand",.TRUE.,varIsArgument,&
                                         nativePeakDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativeShoulderEnergy = AssignVariablePt("ShoulderEnergy",.TRUE.,varIsArgument,&
                                         nativeShoulderEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativeShoulderDemand = AssignVariablePt("ShoulderDemand",.TRUE.,varIsArgument,&
                                         nativeShoulderDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativeOffPeakEnergy  = AssignVariablePt("OffPeakEnergy",.TRUE.,varIsArgument,&
                                         nativeOffPeakEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativeOffPeakDemand  = AssignVariablePt("OffPeakDemand",.TRUE.,varIsArgument,&
                                         nativeOffPeakDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativeMidPeakEnergy  = AssignVariablePt("MidPeakEnergy",.TRUE.,varIsArgument,&
                                         nativeMidPeakEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativeMidPeakDemand  = AssignVariablePt("MidPeakDemand",.TRUE.,varIsArgument,&
                                         nativeMidPeakDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakExceedsOffPeak = AssignVariablePt("PeakExceedsOffPeak",.TRUE.,varIsArgument,&
                                         nativePeakExceedsOffPeak,kindNative,0,iTariff)
  tariff(iTariff)%nativeOffPeakExceedsPeak = AssignVariablePt("OffPeakExceedsPeak",.TRUE.,varIsArgument,&
                                         nativeOffPeakExceedsPeak,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakExceedsMidPeak = AssignVariablePt("PeakExceedsMidPeak",.TRUE.,varIsArgument,&
                                         nativePeakExceedsMidPeak,kindNative,0,iTariff)
  tariff(iTariff)%nativeMidPeakExceedsPeak = AssignVariablePt("MidPeakExceedsPeak",.TRUE.,varIsArgument,&
                                         nativeMidPeakExceedsPeak,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakExceedsShoulder = AssignVariablePt("PeakExceedsShoulder",.TRUE.,varIsArgument,&
                                         nativePeakExceedsShoulder,kindNative,0,iTariff)
  tariff(iTariff)%nativeShoulderExceedsPeak = AssignVariablePt("ShoulderExceedsPeak",.TRUE.,varIsArgument,&
                                         nativeShoulderExceedsPeak,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsWinter       = AssignVariablePt("IsWinter",.TRUE.,varIsArgument,&
                                         nativeIsWinter,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsNotWinter    = AssignVariablePt("IsNotWinter",.TRUE.,varIsArgument,&
                                         nativeIsNotWinter,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsSpring       = AssignVariablePt("IsSpring",.TRUE.,varIsArgument,&
                                         nativeIsSpring,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsNotSpring    = AssignVariablePt("IsNotSpring",.TRUE.,varIsArgument,&
                                         nativeIsNotSpring,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsSummer       = AssignVariablePt("IsSummer",.TRUE.,varIsArgument,&
                                         nativeIsSummer,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsNotSummer    = AssignVariablePt("IsNotSummer",.TRUE.,varIsArgument,&
                                         nativeIsNotSummer,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsAutumn       = AssignVariablePt("IsAutumn",.TRUE.,varIsArgument,&
                                         nativeIsAutumn,kindNative,0,iTariff)
  tariff(iTariff)%nativeIsNotAutumn    = AssignVariablePt("IsNotAutumn",.TRUE.,varIsArgument,&
                                         nativeIsNotAutumn,kindNative,0,iTariff)

  tariff(iTariff)%nativePeakAndShoulderEnergy    = AssignVariablePt("PeakAndShoulderEnergy",.TRUE.,varIsArgument,&
                                         nativePeakAndShoulderEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakAndShoulderDemand    = AssignVariablePt("PeakAndShoulderDemand",.TRUE.,varIsArgument,&
                                         nativePeakAndShoulderDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakAndMidPeakEnergy    = AssignVariablePt("PeakAndMidPeakEnergy",.TRUE.,varIsArgument,&
                                         nativePeakAndMidPeakEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakAndMidPeakDemand    = AssignVariablePt("PeakAndMidPeakDemand",.TRUE.,varIsArgument,&
                                         nativePeakAndMidPeakDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativeShoulderAndOffPeakEnergy    = AssignVariablePt("ShoulderAndOffPeakEnergy",.TRUE.,varIsArgument,&
                                         nativeShoulderAndOffPeakEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativeShoulderAndOffPeakDemand    = AssignVariablePt("ShoulderAndOffPeakDemand",.TRUE.,varIsArgument,&
                                         nativeShoulderAndOffPeakDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakAndOffPeakEnergy    = AssignVariablePt("PeakAndOffPeakEnergy",.TRUE.,varIsArgument,&
                                         nativePeakAndOffPeakEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativePeakAndOffPeakDemand    = AssignVariablePt("PeakAndOffPeakDemand",.TRUE.,varIsArgument,&
                                         nativePeakAndOffPeakDemand,kindNative,0,iTariff)
  tariff(iTariff)%nativeRealTimePriceCosts    = AssignVariablePt("RealTimePriceCosts",.TRUE.,varIsArgument,&
                                         nativeRealTimePriceCosts,kindNative,0,iTariff)
  tariff(iTariff)%nativeAboveCustomerBaseCosts    = AssignVariablePt("AboveCustomerBaseCosts",.TRUE.,varIsArgument,&
                                         nativeAboveCustomerBaseCosts,kindNative,0,iTariff)
  tariff(iTariff)%nativeBelowCustomerBaseCosts    = AssignVariablePt("BelowCustomerBaseCosts",.TRUE.,varIsArgument,&
                                         nativeBelowCustomerBaseCosts,kindNative,0,iTariff)
  tariff(iTariff)%nativeAboveCustomerBaseEnergy    = AssignVariablePt("AboveCustomerBaseEnergy",.TRUE.,varIsArgument,&
                                         nativeAboveCustomerBaseEnergy,kindNative,0,iTariff)
  tariff(iTariff)%nativeBelowCustomerBaseEnergy    = AssignVariablePt("BelowCustomerBaseEnergy",.TRUE.,varIsArgument,&
                                         nativeBelowCustomerBaseEnergy,kindNative,0,iTariff)
  tariff(iTariff)%lastNative     = numEconVar
END DO
END SUBROUTINE CreateCategoryNativeVariables

INTEGER FUNCTION lookupOperator(opString)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   May 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN)       :: opString
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (SameString(opString,'Sum')) THEN
  lookupOperator = opSum
ELSE IF (SameString(opString,'MULTIPLY')) THEN
  lookupOperator = opMULTIPLY
ELSE IF (SameString(opString,'MULT')) THEN
  lookupOperator = opMULTIPLY
ELSE IF (SameString(opString,'SUBTRACT')) THEN
  lookupOperator = opSUBTRACT
ELSE IF (SameString(opString,'SUBT')) THEN
  lookupOperator = opSUBTRACT
ELSE IF (SameString(opString,'DIVIDE')) THEN
  lookupOperator = opDIVIDE
ELSE IF (SameString(opString,'DIV')) THEN
  lookupOperator = opDIVIDE
ELSE IF (SameString(opString,'ABSOLUTE')) THEN
  lookupOperator = opABSOLUTE
ELSE IF (SameString(opString,'ABS')) THEN
  lookupOperator = opABSOLUTE
ELSE IF (SameString(opString,'INTEGER')) THEN
  lookupOperator = opINTEGER
ELSE IF (SameString(opString,'INT')) THEN
  lookupOperator = opINTEGER
ELSE IF (SameString(opString,'SIGN')) THEN
  lookupOperator = opSIGN
ELSE IF (SameString(opString,'ROUND')) THEN
  lookupOperator = opROUND
ELSE IF (SameString(opString,'Maximum')) THEN
  lookupOperator = opMAXIMUM
ELSE IF (SameString(opString,'MAX')) THEN
  lookupOperator = opMAXIMUM
ELSE IF (SameString(opString,'MINIMUM')) THEN
  lookupOperator = opMINIMUM
ELSE IF (SameString(opString,'MIN')) THEN
  lookupOperator = opMINIMUM
ELSE IF (SameString(opString,'EXCEEDS')) THEN
  lookupOperator = opEXCEEDS
ELSE IF (SameString(opString,'ANNUALMINIMUM')) THEN
  lookupOperator = opANNUALMINIMUM
ELSE IF (SameString(opString,'ANMIN')) THEN
  lookupOperator = opANNUALMINIMUM
ELSE IF (SameString(opString,'ANNUALMAXIMUM')) THEN
  lookupOperator = opANNUALMAXIMUM
ELSE IF (SameString(opString,'ANMAX')) THEN
  lookupOperator = opANNUALMAXIMUM
ELSE IF (SameString(opString,'ANNUALSUM')) THEN
  lookupOperator = opANNUALSUM
ELSE IF (SameString(opString,'ANSUM')) THEN
  lookupOperator = opANNUALSUM
ELSE IF (SameString(opString,'ANNUALAVERAGE')) THEN
  lookupOperator = opANNUALAVERAGE
ELSE IF (SameString(opString,'ANAVG')) THEN
  lookupOperator = opANNUALAVERAGE
ELSE IF (SameString(opString,'ANNUALOR')) THEN
  lookupOperator = opANNUALOR
ELSE IF (SameString(opString,'ANOR')) THEN
  lookupOperator = opANNUALOR
ELSE IF (SameString(opString,'ANNUALAND')) THEN
  lookupOperator = opANNUALAND
ELSE IF (SameString(opString,'ANAND')) THEN
  lookupOperator = opANNUALAND
ELSE IF (SameString(opString,'ANNUALMAXIMUMZERO')) THEN
  lookupOperator = opANNUALMAXIMUMZERO
ELSE IF (SameString(opString,'ANMAXZ')) THEN
  lookupOperator = opANNUALMAXIMUMZERO
ELSE IF (SameString(opString,'ANNUALMINIMUMZERO')) THEN
  lookupOperator = opANNUALMINIMUMZERO
ELSE IF (SameString(opString,'ANMINZ')) THEN
  lookupOperator = opANNUALMINIMUMZERO
ELSE IF (SameString(opString,'IF')) THEN
  lookupOperator = opIF
ELSE IF (SameString(opString,'GREATERTHAN')) THEN
  lookupOperator = opGREATERTHAN
ELSE IF (SameString(opString,'GT')) THEN
  lookupOperator = opGREATERTHAN
ELSE IF (SameString(opString,'GREATEREQUAL')) THEN
  lookupOperator = opGREATEREQUAL
ELSE IF (SameString(opString,'GE')) THEN
  lookupOperator = opGREATEREQUAL
ELSE IF (SameString(opString,'LESSTHAN')) THEN
  lookupOperator = opLESSTHAN
ELSE IF (SameString(opString,'LT')) THEN
  lookupOperator = opLESSTHAN
ELSE IF (SameString(opString,'LESSEQUAL')) THEN
  lookupOperator = opLESSEQUAL
ELSE IF (SameString(opString,'LE')) THEN
  lookupOperator = opLESSEQUAL
ELSE IF (SameString(opString,'EQUAL')) THEN
  lookupOperator = opEQUAL
ELSE IF (SameString(opString,'EQ')) THEN
  lookupOperator = opEQUAL
ELSE IF (SameString(opString,'NOTEQUAL')) THEN
  lookupOperator = opNOTEQUAL
ELSE IF (SameString(opString,'NE')) THEN
  lookupOperator = opNOTEQUAL
ELSE IF (SameString(opString,'AND')) THEN
  lookupOperator = opAND
ELSE IF (SameString(opString,'OR')) THEN
  lookupOperator = opOR
ELSE IF (SameString(opString,'NOT')) THEN
  lookupOperator = opNOT
ELSE IF (SameString(opString,'FROM')) THEN
  lookupOperator = opNOOP
ELSE IF (SameString(opString,'ADD')) THEN
  lookupOperator = opADD
ELSE
  lookupOperator = 0
END IF
END FUNCTION lookupOperator

!======================================================================================================================
!======================================================================================================================
!
!
!    DEFAULT COMPUTATION RELATED ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================


SUBROUTINE CreateDefaultComputation
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   June 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na
          !
          ! PURPOSE OF THIS SUBROUTINE:
          !    For most tariffs defined in EnergyPlus no specific
          !    ECONOMICS:COMPUTATION will be entered. In that case,
          !    a default sequence of computation steps needs to be
          !    created.  This routine creates the default
          !    computation steps.
          !
          !    Object           Fields         Depend On Fields
          !
          !    Qualify          namePt         sourcePt
          !                                    thresholdPt
          !
          !    Charge:Simple    namePt         sourcePt
          !                     categoryPt     costPerPt
          !
          !    Charge:Block     namePt         sourcePt
          !                     categoryPt     blkSzMultPt
          !                     remainingPt    blkSzPt
          !                                    blkCostPt
          !
          !    Ratchet          namePt         baselinePt
          !                                    adjustmentPt
          !                                    multiplierPt
          !                                    offsetPt
          !
          !    These will be formed into expressions that look like
          !
          !      namePt NOOP sourcePt thresholdPt
          !
          !    The different Charges are combined using the SUM operation
          !    into categories.
          !
          !      category SUM chg1Name chg2Name chg3Name
          !
          !    Since the dependency array has one target and multiple
          !    parameters, remainingPt is shown as a seperate equation that
          !    depends on namePt for Charge:Block. The equation will not be
          !    displayed or processed except in the sort.
          !
          !      remainingPt NOOP namePt
          !
          !    Many lines of the computation will include just the name of
          !    a single variable which triggers the calculation for that
          !    charge, ratchet or qualify.
          !
          !      chg1Name
          !
          !    It is also possible that two variables referenced within one
          !    object could include a dependancy relationship also. For
          !    example, the blkSzPt could be calculated using the same sourePt
          !    in Charge:Block.

          ! METHODOLOGY EMPLOYED:
          !    Since some ECONOMCIS:* objects depend on other variables
          !    first must create the order of when to perform the
          !    computations. First a dependancy table is created that
          !    indicates what variables are dependant on other variables.
          !
          !    A directed acyclic graph (DAG) describes the general
          !    problem which is usually solved using a topological
          !    sorting algorithm.
          !
          !    Each line/step is generated and put into the depend
          !    array. Also in the array are counts of how many items it
          !    depends on and a list of entries that are dependant on that
          !    line.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataGlobals, ONLY : OutputFileInits
USE OutputReportTabular, ONLY: IntToStr

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

INTEGER  :: iTariff
INTEGER  :: iVar
INTEGER  :: jVar
INTEGER  :: kObj
INTEGER  :: mBlock
INTEGER  :: kOperand
INTEGER  :: curBasis
INTEGER  :: curSubtotal
INTEGER  :: curTotal
INTEGER  :: curObject
INTEGER  :: numNoDepend
INTEGER  :: referVar
INTEGER  :: loopCount
LOGICAL  :: remainingVarFlag
INTEGER  :: remainPt

! for each tariff that does not have a UtilityCost:Computation object go through the variables
DO iTariff = 1, numTariff
  IF (.NOT. computation(iTariff)%isUserDef) THEN
    ! clear all variables so that they are not active
    DO jVar = 1, numEconVar
      econVar(jVar)%activeNow = .FALSE.
    END DO
    !make all native variables active
    DO jVar = tariff(iTariff)%firstNative, tariff(iTariff)%lastNative
      econVar(jVar)%activeNow = .TRUE.
    END DO
    !"clear" the dependOn array
    numOperand = 0
    !Define the preset equations (category sumation)
    curTotal = tariff(iTariff)%ptTotal
    curSubtotal = tariff(iTariff)%ptSubtotal
    curBasis = tariff(iTariff)%ptBasis
    ! total SUM subtotal taxes
    econVar(curTotal)%operator = opSUM
    econVar(curTotal)%activeNow = .TRUE.
    CALL addOperand(curTotal,curSubtotal)
    CALL addOperand(curTotal,tariff(iTariff)%ptTaxes)
    ! subtotal SUM basis adjustments surcharges
    econVar(curSubtotal)%operator = opSUM
    econVar(curSubtotal)%activeNow = .TRUE.
    CALL addOperand(curSubtotal,curBasis)
    CALL addOperand(curSubtotal,tariff(iTariff)%ptAdjustment)
    CALL addOperand(curSubtotal,tariff(iTariff)%ptSurcharge)
    ! basis SUM EnergyCharges DemandCharges ServiceCharges
    econVar(curBasis)%operator = opSUM
    econVar(curBasis)%activeNow = .TRUE.
    CALL addOperand(curBasis,tariff(iTariff)%ptEnergyCharges)
    CALL addOperand(curBasis,tariff(iTariff)%ptDemandCharges)
    CALL addOperand(curBasis,tariff(iTariff)%ptServiceCharges)
    !set up the equations for other objects
    CALL addChargesToOperand(iTariff,tariff(iTariff)%ptEnergyCharges)
    CALL addChargesToOperand(iTariff,tariff(iTariff)%ptDemandCharges)
    CALL addChargesToOperand(iTariff,tariff(iTariff)%ptServiceCharges)
    CALL addChargesToOperand(iTariff,tariff(iTariff)%ptAdjustment)
    CALL addChargesToOperand(iTariff,tariff(iTariff)%ptSurcharge)
    CALL addChargesToOperand(iTariff,tariff(iTariff)%ptTaxes)
    !add the real time pricing to the energy charges
    IF (tariff(iTariff)%chargeSchIndex .NE. 0) THEN
      CALL addOperand(tariff(iTariff)%ptEnergyCharges, tariff(iTariff)%nativeRealTimePriceCosts)
    END IF
    !now add equations with NOOP to represent each object with its
    !dependancies
    ! Qualify
    DO kObj = 1, numQualify
      IF (qualify(kObj)%tariffIndx .EQ. iTariff) THEN
        curObject = qualify(kObj)%namePt
        econVar(curObject)%operator = opNOOP
        econVar(curObject)%activeNow = .TRUE.
        CALL addOperand(curObject,qualify(kObj)%sourcePt)
        CALL addOperand(curObject,qualify(kObj)%thresholdPt)
      END IF
    END DO
    ! Ratchet
    DO kObj = 1, numRatchet
      IF (ratchet(kObj)%tariffIndx .EQ. iTariff) THEN
        curObject = ratchet(kObj)%namePt
        econVar(curObject)%operator = opNOOP
        econVar(curObject)%activeNow = .TRUE.
        CALL addOperand(curObject,ratchet(kObj)%baselinePt)
        CALL addOperand(curObject,ratchet(kObj)%adjustmentPt)
        CALL addOperand(curObject,ratchet(kObj)%multiplierPt)
        CALL addOperand(curObject,ratchet(kObj)%offsetPt)
      END IF
    END DO
    ! ChargeSimple
    DO kObj = 1, numChargeSimple
      IF (chargeSimple(kObj)%tariffIndx .EQ. iTariff) THEN
        curObject = chargeSimple(kObj)%namePt
        econVar(curObject)%operator = opNOOP
        econVar(curObject)%activeNow = .TRUE.
        CALL addOperand(curObject,chargeSimple(kObj)%sourcePt)
        CALL addOperand(curObject,chargeSimple(kObj)%costPerPt)
      END IF
    END DO
    ! ChargeBlock
    DO kObj = 1, numChargeBlock
      IF (chargeBlock(kObj)%tariffIndx .EQ. iTariff) THEN
        curObject = chargeBlock(kObj)%namePt
        econVar(curObject)%operator = opNOOP
        econVar(curObject)%activeNow = .TRUE.
        CALL addOperand(curObject,chargeBlock(kObj)%sourcePt)
        CALL addOperand(curObject,chargeBlock(kObj)%blkSzMultPt)
        DO mBlock = 1, chargeBlock(kObj)%numBlk
          CALL addOperand(curObject,chargeBlock(kObj)%blkSzPt(mBlock))
          CALL addOperand(curObject,chargeBlock(kObj)%blkCostPt(mBlock))
        END DO
        ! now add a new "equation" for dependency of remainingPt on namePt
        remainPt = chargeBlock(kObj)%remainingPt
        IF (remainPt .GT. 0) THEN
          econVar(remainPt)%operator = opNOOP
          econVar(remainPt)%activeNow = .TRUE.
          CALL addOperand(remainPt,curObject)
        END IF
      END IF
    END DO
    ! Economic:Variable
    !make all of the user defined variables as active
    DO iVar = 1, numEconVar
      IF (econVar(iVar)%tariffIndx .EQ. iTariff) THEN
        IF (econVar(iVar)%kindOfObj .EQ. kindVariable) THEN
          econVar(iVar)%activeNow = .TRUE.
        END IF
      END IF
    END DO
    ! make sure no compuation is already user defined
    IF (computation(iTariff)%firstStep .NE. 0) THEN
      CALL ShowWarningError('In UtilityCost:Tariff: Overwriting user defined tariff ' //TRIM(tariff(iTariff)%tariffName))
    END IF
    !initialize the computation
    computation(iTariff)%computeName = 'Autogenerated - ' // TRIM(tariff(iTariff)%tariffName)
    computation(iTariff)%firstStep = numSteps + 1
    computation(iTariff)%lastStep = -1  !this will be incremented by addStep
    computation(iTariff)%isUserDef = .FALSE.
    ! now all "equations" are defined, treat the variables with the list
    ! of dependancies as a directed acyclic graph and use "count down" algorithm
    ! to do a topological sort of the variables into the order for computation
    !
    ! First, clear the counters
    DO jVar = 1,numEconVar
      econVar(jVar)%cntMeDependOn = 0
    END DO
    ! Second, add up the number of dependancies on each variable
    DO iVar = 1, numEconVar
      IF (econVar(iVar)%activeNow) THEN
        IF (econVar(iVar)%lastOperand .GE. econVar(iVar)%firstOperand) THEN
          econVar(iVar)%cntMeDependOn = 1 + econVar(iVar)%lastOperand - econVar(iVar)%firstOperand
        END IF
      END IF
    END DO
    ! Third, start removing items with zero connections and decrease each
    !   counter.
    numNoDepend = -1
    loopCount = 0
    DO WHILE ((numNoDepend .NE. 0) .OR. (loopCount .GT. 100000))
      numNoDepend = 0
      DO iVar = 1,numEconVar
        IF (econVar(iVar)%activeNow) THEN
          !find a variable that has no more dangling dependancies
          IF (econVar(iVar)%cntMeDependOn .EQ. 0) THEN
            ! If the variable is a native variable then
            !IF (econVar(iVar)%kindOfObj .NE. kindNative) THEN
            IF ((econVar(iVar)%kindOfObj .NE. kindNative) .AND. (econVar(iVar)%kindOfObj .NE. kindVariable)) THEN
              IF (econVar(iVar)%lastOperand .GE. econVar(iVar)%firstOperand) THEN
                !transfer variables and operator to the computation and list of steps
                ! go through the operands backwards (end of line is evaluated first)
                DO kOperand = econVar(iVar)%lastOperand,econVar(iVar)%firstOperand,-1
                  CALL incrementSteps
                  steps(numSteps) = operand(kOperand)
                END DO
                ! append the operator (either SUM or NOOP)
                CALL incrementSteps
                steps(numSteps) = econVar(iVar)%operator
                ! append the variable itself
                CALL incrementSteps
                steps(numSteps) = iVar
                !at the end of the line show a zero to clear the stack
                CALL incrementSteps
                steps(numSteps) = 0
              END IF
            END IF
            ! go through each other variable looking for places where this variable is used
            ! and decrement their counters.
            DO jVar = 1, numEconVar
              IF (econVar(jVar)%activeNow) THEN
                DO kOperand = econVar(jVar)%firstOperand, econVar(jVar)%lastOperand
                  referVar = operand(kOperand)
                  IF (iVar .EQ. referVar) THEN
                    econVar(jVar)%cntMeDependOn = econVar(jVar)%cntMeDependOn - 1
                    ! for each variable that has been decremented to zero increment the counter
                    IF (econVar(jVar)%cntMeDependOn .LE. 0) THEN
                      numNoDepend = numNoDepend + 1
                    END IF
                  END IF
                END DO
              END IF
            END DO
            !make the variable inactive
            econVar(iVar)%activeNow = .FALSE.
          END IF
        END IF
      END DO
      loopCount = loopCount + 1
    END DO
    IF (loopCount .GT. 100000) THEN
      CALL ShowWarningError('UtilityCost:Tariff: Loop count exceeded when counting dependancies in tariff: '// &
         TRIM(tariff(iTariff)%tariffName))
    END IF
    !make sure that all variables associated with the tariff are included
    remainingVarFlag = .FALSE.
    DO iVar = 1, numEconVar
      IF (econVar(iVar)%activeNow) THEN
        remainingVarFlag = .TRUE.
      END IF
    END DO
    IF (remainingVarFlag) THEN
      CALL ShowWarningError('CreateDefaultComputation: In UtilityCost:Computation: '//    &
         'Circular or invalid dependencies found in tariff: ' // &
         TRIM(tariff(iTariff)%tariffName))
      CALL ShowContinueError('  UtilityCost variables that may have invalid dependencies and the variables they are dependant on.')
      DO iVar = 1, numEconVar
        IF (econVar(iVar)%tariffIndx .EQ. iTariff) THEN
          IF (econVar(iVar)%activeNow) THEN
            CALL ShowContinueError('     ' // TRIM(econVar(iVar)%name))
            DO kOperand = econVar(iVar)%firstOperand,econVar(iVar)%lastOperand
              CALL ShowContinueError('        ->  ' // TRIM(econVar(operand(kOperand))%name))
            END DO
          END IF
        END IF
      END DO
    END IF
    !set the end of the computations
    computation(iTariff)%lastStep = numSteps
    IF (computation(iTariff)%firstStep .GE. computation(iTariff)%lastStep) THEN
      computation(iTariff)%firstStep = 0
      computation(iTariff)%lastStep = -1
      CALL ShowWarningError('CreateDefaultComputation: In UtilityCost:Computation: '//  &
                            'No lines in the auto generated computation can be interpreted in tariff: ' // &
                            TRIM(tariff(iTariff)%tariffName))
    END IF
  END IF
END DO
END SUBROUTINE CreateDefaultComputation

SUBROUTINE addOperand(varMe,varOperand)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Used by CreateDefaultComputation to create the dependancy
          !   relationship in the EconVar array

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN) :: varMe
INTEGER,INTENT(IN) :: varOperand

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: sizeIncrement = 100
INTEGER, SAVE     :: prevVarMe = 0

IF (varOperand .NE. 0) THEN
  !increment the numOperand and allocate/reallocate the array
  !if necessary
  IF (.NOT. ALLOCATED(operand)) THEN
    ALLOCATE(operand(sizeIncrement))
    sizeOperand = sizeIncrement
    numOperand = 1
  ELSE
    numOperand = numOperand + 1
    ! if larger then current size then make a temporary array of the same
    ! type and put stuff into it while reallocating the main array
    IF (numOperand .GT. sizeOperand) THEN
      ALLOCATE(operandCopy(sizeOperand))
      operandCopy = operand
      DEALLOCATE(operand)
      ALLOCATE(operand(sizeOperand + sizeIncrement))
      operand(1:sizeOperand) = operandCopy
      DEALLOCATE(operandCopy)
      sizeOperand = sizeOperand + sizeIncrement
    END IF
  END IF
  !now add the dependancy relationship
  operand(numOperand) = varOperand
  econVar(varMe)%lastOperand = numOperand
  !if it is the first time addOperand was called with the varMe value
  !then set the first pointer as well
  IF (varMe .NE. prevVarMe) THEN
    econVar(varMe)%firstOperand = numOperand
    prevVarMe = varMe
  END IF
END IF
END SUBROUTINE addOperand

SUBROUTINE addChargesToOperand(curTariff,curPointer)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Used by CreateDefaultComputation to create the "equation"
          !   for the categories that are summations of ECONOMICS:CHARGES:BLOCK
          !   and ECONOMICS:CHARGES:SIMPLE

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN) :: curTariff
INTEGER,INTENT(IN) :: curPointer
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: kObj

econVar(curPointer)%operator = opSUM
econVar(curPointer)%activeNow = .TRUE.
DO kObj = 1, numChargeSimple
  IF (chargeSimple(kObj)%tariffIndx .EQ. curTariff) THEN
    IF (chargeSimple(kObj)%categoryPt .EQ. curPointer) THEN
      CALL addOperand(curPointer,chargeSimple(kObj)%namePt)
    END IF
  END IF
END DO
DO kObj = 1, numChargeBlock
  IF (chargeBlock(kObj)%tariffIndx .EQ. curTariff) THEN
    IF (chargeBlock(kObj)%categoryPt .EQ. curPointer) THEN
      CALL addOperand(curPointer,chargeBlock(kObj)%namePt)
    END IF
  END IF
END DO
END SUBROUTINE addChargesToOperand

!======================================================================================================================
!======================================================================================================================
!
!
!    GATHER TIMESTEP VALUES ROUTINE
!
!
!======================================================================================================================
!======================================================================================================================


SUBROUTINE GatherForEconomics
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   June 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gathers the data each timestep and updates the arrays
          !   holding the data that will be used by the tariff
          !   calculation.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

USE DataGlobals,     ONLY: HourOfDay,TimeStep,SecInHour,TimeStepZone
USE ScheduleManager, ONLY: GetCurrentScheduleValue
USE DataEnvironment, ONLY: Month

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

REAL(r64), external :: GetCurrentMeterValue

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: iTariff
REAL(r64)    :: curInstantValue
REAL(r64)    :: curDemand
REAL(r64)    :: curEnergy
LOGICAL :: isGood
INTEGER :: curSeason
INTEGER :: curMonth
INTEGER :: curPeriod
REAL(r64)    :: curRTPprice    !real time price
REAL(r64)    :: curRTPbaseline !real time price customer baseline load
REAL(r64)    :: curRTPenergy   !energy applied to real time price
REAL(r64)    :: curRTPcost     !cost for energy for current time

IF (numTariff .GE. 1) THEN
  DO iTariff = 1, numTariff
    isGood = .FALSE.
    !if the meter is defined get the value
    IF (tariff(iTariff)%reportMeterIndx .NE. 0) THEN
      curInstantValue = GetCurrentMeterValue(tariff(iTariff)%reportMeterIndx)
    ELSE
      curInstantValue = 0.0d0
    END IF
    ! remember the demand is still energy over a period of time divided by the
    ! length of time. This gathers the energy also.
    tariff(iTariff)%collectEnergy = tariff(iTariff)%collectEnergy + curInstantValue
    tariff(iTariff)%collectTime   = tariff(iTariff)%collectTime  + (TimeStepZone * SecInHour)
    !added *SecInHour when adding RTP support August 2008
    IF (tariff(iTariff)%collectTime .GE. tariff(iTariff)%demWinTime * SecInHour) THEN
      !get current value that has been converted into desired units
      curDemand = tariff(iTariff)%demandConv * tariff(iTariff)%collectEnergy / tariff(iTariff)%collectTime
      curEnergy = tariff(iTariff)%energyConv * tariff(iTariff)%collectEnergy
      ! get the schedule values
      ! remember no confirmation of schedule values occurs prior to now
      IF (tariff(iTariff)%seasonSchIndex .NE. 0) THEN
        curSeason = GetCurrentScheduleValue(tariff(iTariff)%seasonSchIndex)
      ELSE
        curSeason = 1
      END IF
      IF (tariff(iTariff)%periodSchIndex .NE. 0) THEN
        curPeriod = GetCurrentScheduleValue(tariff(iTariff)%periodSchIndex)
      ELSE
        curPeriod = 1
      END IF
      IF (tariff(iTariff)%monthSchIndex .NE. 0) THEN
        curMonth  = GetCurrentScheduleValue(tariff(iTariff)%monthSchIndex)
      ELSE
        curMonth = Month !from DataEnvironment
      END IF
      IF (isWithinRange(curSeason,1,5)) THEN
        IF (isWithinRange(curPeriod,1,4)) THEN
          IF (isWithinRange(curMonth,1,12)) THEN
            isGood = .TRUE.
          END IF
        END IF
      END IF
      IF (isGood) THEN
        tariff(iTariff)%seasonForMonth(curMonth) = curSeason
        tariff(iTariff)%gatherEnergy(curPeriod,curMonth) = tariff(iTariff)%gatherEnergy(curPeriod,curMonth) + curEnergy
        IF (tariff(iTariff)%gatherDemand(curPeriod,curMonth) .LT. curDemand) THEN
          tariff(iTariff)%gatherDemand(curPeriod,curMonth) = curDemand
        END IF
      ELSE
        CALL ShowWarningError('UtilityCost:Tariff: While gathering for: ' // tariff(iTariff)%tariffName)
        CALL ShowContinueError('Invalid schedule values - outside of range')
      END IF
      ! Real Time Pricing
      IF (tariff(iTariff)%chargeSchIndex .NE. 0) THEN
        curRTPprice = GetCurrentScheduleValue(tariff(iTariff)%chargeSchIndex)
        ! if customer baseline load schedule is used, subtract that off of the
        ! current energy
        IF (tariff(iTariff)%baseUseSchIndex .NE. 0) THEN
          curRTPbaseline = GetCurrentScheduleValue(tariff(iTariff)%baseUseSchIndex)
          curRTPenergy = curEnergy - curRTPbaseline
        ELSE
          curRTPenergy = curEnergy
        END IF
        ! calculate the real time cost for current times energy
        curRTPcost = curRTPenergy * curRTPprice
        tariff(iTariff)%RTPcost(curMonth) = tariff(iTariff)%RTPcost(curMonth) + curRTPcost
        IF (curRTPcost .GT. 0) THEN
          tariff(iTariff)%RTPaboveBaseCost(curMonth) = tariff(iTariff)%RTPaboveBaseCost(curMonth) + curRTPcost
        ELSE
          tariff(iTariff)%RTPbelowBaseCost(curMonth) = tariff(iTariff)%RTPbelowBaseCost(curMonth) + curRTPcost
        END IF
        IF (curRTPenergy .GT. 0) THEN
          tariff(iTariff)%RTPaboveBaseEnergy(curMonth) = tariff(iTariff)%RTPaboveBaseEnergy(curMonth) + curRTPenergy
        ELSE
          tariff(iTariff)%RTPbelowBaseEnergy(curMonth) = tariff(iTariff)%RTPbelowBaseEnergy(curMonth) + curRTPenergy
        END IF
      END IF
      ! reset the counters
      tariff(iTariff)%collectEnergy = 0.0d0
      tariff(iTariff)%collectTime   = 0.0d0
    END IF
  END DO
END IF
END SUBROUTINE GatherForEconomics

LOGICAL FUNCTION isWithinRange(testVal,minThreshold,maxThreshold)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Simple function to check if an integer is equal to or between
          !   two other values.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: testVal
INTEGER, INTENT(IN) :: minThreshold
INTEGER, INTENT(IN) :: maxThreshold

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
IF (maxThreshold .LT. minThreshold) THEN
  CALL ShowWarningError('UtilityCost: Invalid thresholds in IsWithinRange routine.')
END IF
IF ((testVal .LE. maxThreshold) .AND. (testVal .GE. minThreshold)) THEN
  isWithinRange = .TRUE.
ELSE
  isWithinRange = .FALSE.
END IF
END FUNCTION isWithinRange

!======================================================================================================================
!======================================================================================================================
!
!
!    COMPUTE THE UTILITY BILLS AND CREATE REPORTS
!
!
!======================================================================================================================
!======================================================================================================================


SUBROUTINE ComputeTariff
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Perform the calculation steps to compute the monthly
          !    utility bills for the user entered tariffs.
          !
          !    The list of steps for the tariff computation are in order
          !    for stack based computation (reverse polish notation)

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

USE OutputReportTabular, ONLY:  WriteTabularFiles

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS

! values used in specific operations
REAL(r64), DIMENSION(MaxNumMonths) :: a
INTEGER                       :: aPt
REAL(r64), DIMENSION(MaxNumMonths) :: b
INTEGER                       :: bPt
REAL(r64), DIMENSION(MaxNumMonths) :: c
INTEGER                       :: cPt
REAL(r64), DIMENSION(MaxNumMonths) :: d

INTEGER :: iTariff
INTEGER :: jStep
INTEGER :: kStack
INTEGER :: lMonth
INTEGER :: nVar
INTEGER :: curStep
INTEGER, parameter :: noVar = 0

REAL(r64) :: hugeValue
REAL(r64) :: annualAggregate
INTEGER :: annualCnt

hugeValue = HUGE(hugeValue)
!  Clear the isEvaluated flags for all economics variables.
DO nVar = 1, numEconVar
  econVar(nVar)%isEvaluated = .FALSE.
END DO
IF (numTariff .GE. 1) THEN
  WriteTabularFiles = .TRUE.
  CALL setNativeVariables
  DO iTariff = 1, numTariff
    DO jStep = computation(iTariff)%firstStep,computation(iTariff)%lastStep
      curStep = steps(jStep)
      SELECT CASE (curStep)
        CASE (0)  !end of line - assign variable and clear stack
          ! if the stack still has two items on it then assign the values to the
          ! pointer otherwise if it follows a NOOP line it will only have one item
          ! that has already been assigned and no further action is required.
          IF (topOfStack .GE. 2) THEN
            CALL popStack(b,bPt) !pop the variable pointer
            CALL popStack(a,aPt) !pop the values
            IF (isWithinRange(bPt,1,numEconVar)) THEN
              econVar(bPt)%values = a
            END IF
          END IF
          topOfStack = 0
        CASE (1:) !all positive values are a reference to an econVar
          CALL pushStack(econVar(curStep)%values,curStep)
        CASE (opSUM)
          a = 0.0d0
          DO kStack = 1,topOfStack
            CALL popStack(b,bPt)
            a = a + b
          END DO
          CALL pushStack(a,noVar)
        CASE (opMULTIPLY)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          CALL pushStack(a * b,noVar)
        CASE (opSUBTRACT)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          CALL pushStack(b - a,noVar)
        CASE (opDIVIDE)
          CALL popStack(a,aPt)
          CALL popStack(b,bPt)
          DO lMonth = 1,MaxNumMonths
            IF (b(lMonth) .NE. 0) THEN
              c(lMonth) = a(lMonth) / b(lMonth)
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opABSOLUTE)
          CALL popStack(a,aPt)
          CALL pushStack(ABS(a),noVar)
        CASE (opINTEGER)
          CALL popStack(a,aPt)
          CALL pushStack(REAL(INT(a),r64),noVar)
        CASE (opSIGN)
          CALL popStack(a,aPt)
          CALL pushStack(SIGN(1.d0,a),noVar)
!        CASE (opROUND)
!          CALL popStack(b,bPt)
!          CALL popStack(a,aPt)
!          DO lMonth = 1,MaxNumMonths
!            IF ((b(lMonth) .LE. 5) .AND. (b(lMonth) .GE. -5)) THEN
!              c(lMonth) = FLOAT(INT(a(lMonth) / (10 ** b(lMonth))) * (10 ** b(lMonth)))
!            END IF
!          END DO
!          CALL pushStack(c,noVar)
        CASE (opMAXIMUM)
          a = -hugeValue
          DO kStack = 1,topOfStack
            CALL popStack(b,bPt)
            DO lMonth = 1,MaxNumMonths
              IF (b(lMonth) .GT. a(lMonth)) THEN
                a(lMonth) = b(lMonth)
              END IF
            END DO
          END DO
          CALL pushStack(a,noVar)
        CASE (opMINIMUM)
          a = hugeValue
          DO kStack = 1,topOfStack
            CALL popStack(b,bPt)
            DO lMonth = 1,MaxNumMonths
              IF (b(lMonth) .LT. a(lMonth)) THEN
                a(lMonth) = b(lMonth)
              END IF
            END DO
          END DO
          CALL pushStack(a,noVar)
        CASE (opEXCEEDS)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .GT. b(lMonth)) THEN
              c(lMonth) = a(lMonth) - b(lMonth)
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opANNUALMINIMUM)
          !takes the minimum but ignores zeros
          annualAggregate = hugeValue
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. 0) THEN
              IF (a(lMonth) .LT. annualAggregate) THEN
                annualAggregate = a(lMonth)
              END IF
            END IF
          END DO
          ! if all months are zero then hugeValue still in annual but should be zero
          IF (annualAggregate .EQ. hugeValue) THEN
            annualAggregate = 0.0d0
          END IF
          c = annualAggregate
          CALL pushStack(c,noVar)
        CASE (opANNUALMAXIMUM)
          !takes the maximum but ignores zeros
          annualAggregate = -hugeValue
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. 0) THEN
              IF (a(lMonth) .GT. annualAggregate) THEN
                annualAggregate = a(lMonth)
              END IF
            END IF
          END DO
          ! if all months are zero then hugeValue still in annual but should be zero
          IF (annualAggregate .EQ. -hugeValue) THEN
            annualAggregate = 0.0d0
          END IF
          c = annualAggregate
          CALL pushStack(c,noVar)
        CASE (opANNUALSUM)
          !takes the maximum but ignores zeros
          annualAggregate = 0.0d0
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            annualAggregate = annualAggregate + a(lMonth)
          END DO
          c = annualAggregate
          CALL pushStack(c,noVar)
        CASE (opANNUALAVERAGE)
          !takes the annual sum but ignores zeros
          annualAggregate = 0.0d0
          annualCnt = 0
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. 0) THEN
              annualAggregate = annualAggregate + a(lMonth)
              annualCnt = annualCnt + 1
            END IF
          END DO
          ! if all months are zero then return zero
          IF (annualCnt .NE. 0) THEN
            c = annualAggregate / annualCnt
          ELSE
            c = 0.0d0
          END IF
          CALL pushStack(c,noVar)
        CASE (opANNUALOR)
          annualCnt = 0
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. 0) THEN
              annualCnt = annualCnt + 1
            END IF
          END DO
          ! if any months is not zero then "true"
          IF (annualCnt .GE. 1) THEN
            c = 1.0d0
          ELSE
            c = 0.0d0
          END IF
          CALL pushStack(c,noVar)
        CASE (opANNUALAND)
          annualCnt = 0
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. 0) THEN
              annualCnt = annualCnt + 1
            END IF
          END DO
          ! if all months are not zero then "true"
          IF (annualCnt .EQ. MaxNumMonths) THEN
            c = 1.0d0
          ELSE
            c = 0.0d0
          END IF
          CALL pushStack(c,noVar)
        CASE (opANNUALMAXIMUMZERO)
          !takes the maximum including zeros
          annualAggregate = -hugeValue
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .GT. annualAggregate) THEN
              annualAggregate = a(lMonth)
            END IF
          END DO
          c = annualAggregate
          CALL pushStack(c,noVar)
        CASE (opANNUALMINIMUMZERO)
          !takes the maximum including zeros
          annualAggregate = hugeValue
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .LT. annualAggregate) THEN
              annualAggregate = a(lMonth)
            END IF
          END DO
          c = annualAggregate
          CALL pushStack(c,noVar)
        CASE (opIF)
          CALL popStack(c,cPt)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. 0) THEN
              d(lMonth) = b(lMonth)
            ELSE
              d(lMonth) = c(lMonth)
            END IF
          END DO
          CALL pushStack(d,noVar)
        CASE (opGREATERTHAN)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .GT. b(lMonth)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opGREATEREQUAL)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .GE. b(lMonth)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opLESSTHAN)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .LT. b(lMonth)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opLESSEQUAL)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .LE. b(lMonth)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opEQUAL)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .EQ. b(lMonth)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opNOTEQUAL)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .NE. b(lMonth)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opAND)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF ((a(lMonth) .NE. 0) .AND. (b(lMonth) .NE. 0)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opOR)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF ((a(lMonth) .NE. 0) .OR. (b(lMonth) .NE. 0)) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opNOT)
          CALL popStack(a,aPt)
          DO lMonth = 1,MaxNumMonths
            IF (a(lMonth) .EQ. 0) THEN
              c(lMonth) = 1.0d0
            ELSE
              c(lMonth) = 0.0d0
            END IF
          END DO
          CALL pushStack(c,noVar)
        CASE (opADD)
          CALL popStack(b,bPt)
          CALL popStack(a,aPt)
          CALL pushStack(a + b,noVar)
        CASE (opNOOP)
          !do nothing but clear the stack
          topOfStack = 0
          ! No longer pushing a zero to fix bug
          !and push zero
          !a = 0
          !CALL pushStack(a,noVar)
      END SELECT
    END DO
    CALL checkMinimumMonthlyCharge(iTariff)
  END DO
  CALL selectTariff
  CALL LEEDtariffReporting
END IF
END SUBROUTINE ComputeTariff


SUBROUTINE pushStack(monthlyArray,variablePointer)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    A stack is used in the evaluation of the tariff since
          !    the variables and operators are in a reverse polish
          !    notation order. The stack operates on a last-in
          !    first out basis. The stack consists of both a pointer
          !    to the variable and the twelve monthly values.
          !    This routine puts an item on the top of the stack.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportTabular, ONLY: IntToStr

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64), DIMENSION(MaxNumMonths),INTENT(IN) :: monthlyArray
INTEGER,INTENT(IN)                       :: variablePointer

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64), DIMENSION(MaxNumMonths) :: curMonthlyArray
INTEGER :: sizeIncrement = 50

curMonthlyArray = monthlyArray
IF (.NOT. ALLOCATED(stack)) THEN
  ALLOCATE(stack(sizeIncrement))
  sizeStack = sizeIncrement
  topOfStack = 1
ELSE
  topOfStack = topOfStack + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (topOfStack .GT. sizeStack) THEN
    ALLOCATE(stackCopy(sizeStack))
    stackCopy = stack
    DEALLOCATE(stack)
    ALLOCATE(stack(sizeStack + sizeIncrement))
    stack(1:sizeStack) = stackCopy
    DEALLOCATE(stackCopy)
    sizeStack = sizeStack + sizeIncrement
  END IF
END IF
!now push the values on to the stack
stack(topOfStack)%varPt = variablePointer
!check if variable has been evaluated if it is CHARGE:SIMPLE, CHARGE:BLOCK, RATCHET, or QUALIFY
!if it has not overwrite the values for monthlyArray with the evaluated values
IF (variablePointer .NE. 0) THEN
  IF (.NOT. econVar(variablePointer)%isEvaluated) THEN
    SELECT CASE (econVar(variablePointer)%kindOfObj)
      CASE (kindChargeSimple)
        CALL evaluateChargeSimple(variablePointer)
      CASE (kindChargeBlock)
        CALL evaluateChargeBlock(variablePointer)
      CASE (kindRatchet)
        CALL evaluateRatchet(variablePointer)
      CASE (kindQualify)
        CALL evaluateQualify(variablePointer)
      CASE (kindUnknown)
        CALL ShowWarningError('UtilityCost variable not defined: ' // TRIM(econVar(variablePointer)%name))
        CALL ShowContinueError('   In tariff: '  // TRIM(Tariff(econVar(variablePointer)%tariffIndx)%tariffName))
        CALL ShowContinueError('   This may be the result of a mispelled variable name in the UtilityCost:Computation object.')
        CALL ShowContinueError('   All zero values will be assumed for this variable.')
      CASE (kindVariable,kindCategory,kindNative,kindAssignCompute,kindTariff,kindComputation)
        ! do nothing
      CASE DEFAULT
        CALL ShowWarningError('UtilityCost Debugging issue. Invalid kind of variable used (pushStack). ' &
                                // TRIM(IntToStr(econVar(variablePointer)%kindOfObj)) &
                                // ' in tariff: ' // TRIM(Tariff(econVar(variablePointer)%tariffIndx)%tariffName))
    END SELECT
    ! if the serviceCharges are being evaluated add in the monthly charges
    IF (econVar(variablePointer)%specific .EQ. catServiceCharges) CALL addMonthlyCharge(variablePointer)
    !get the results of performing the evaulation - should have been
    !put into the econVar values
    curMonthlyArray = econVar(variablePointer)%values
  END IF
END IF
!now assign
stack(topOfStack)%values = curMonthlyArray
END SUBROUTINE pushStack

SUBROUTINE popStack(monthlyArray,variablePointer)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    A stack is used in the evaluation of the tariff since
          !    the variables and operators are in a reverse polish
          !    notation order. The stack operates on a last-in
          !    first out basis. The stack consists of both a pointer
          !    to the variable and the twelve monthly values.
          !    This routine returns the item on the top of the stack
          !    and removes it from the stack.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL(r64), DIMENSION(MaxNumMonths),INTENT(OUT) :: monthlyArray
INTEGER,INTENT(INOUT)                     :: variablePointer

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

IF (topOfStack .GE. 1) THEN
  variablePointer = stack(topOfStack)%varPt
  monthlyArray = stack(topOfStack)%values
ELSE
  CALL ShowWarningError('UtilityCost:Tariff: stack underflow in calculation of utility bills. On variable: ' &
                        // TRIM(econVar(variablePointer)%name))
  variablePointer = 0
  monthlyArray = 0.0d0
  topOfStack = 0
END IF
topOfStack = topOfStack - 1
END SUBROUTINE popStack

SUBROUTINE evaluateChargeSimple(usingVariable)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: usingVariable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: curTariff
INTEGER :: indexInChg
REAL(r64), DIMENSION(MaxNumMonths) :: sourceVals
REAL(r64), DIMENSION(MaxNumMonths) :: costPer
REAL(r64), DIMENSION(MaxNumMonths) :: resultChg
REAL(r64), DIMENSION(MaxNumMonths) :: seasonMask

curTariff = econVar(usingVariable)%tariffIndx
indexInChg = econVar(usingVariable)%index

!check the tariff - make sure they match
IF (chargeSimple(indexInChg)%namePt .NE. usingVariable) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. ChargeSimple index does not match variable pointer.')
  CALL ShowContinueError('   Between: '  // TRIM(econVar(usingVariable)%name))
  CALL ShowContinueError('       And: '  // TRIM(econVar(chargeSimple(indexInChg)%namePt)%name))
END IF
IF (chargeSimple(indexInChg)%tariffIndx .NE. curTariff) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. ChargeSimple index does not match tariff index.')
  CALL ShowContinueError('   Between: '  // TRIM(tariff(curTariff)%tariffName))
  CALL ShowContinueError('       And: '  // TRIM(tariff(chargeSimple(indexInChg)%tariffIndx)%tariffName))
END IF
! data from the Charge:Simple
sourceVals = econVar(chargeSimple(indexInChg)%sourcePt)%values
! determine if costPer should be based on variable or value
IF (chargeSimple(indexInChg)%costPerPt .NE. 0) THEN
  costPer = econVar(chargeSimple(indexInChg)%costPerPt)%values
ELSE
  costPer = chargeSimple(indexInChg)%costPerVal
END IF
! find proper season mask
SELECT CASE (chargeSimple(indexInChg)%season)
  CASE (seasonSummer)
    seasonMask = econVar(tariff(curTariff)%nativeIsSummer)%values
  CASE (seasonWinter)
    seasonMask = econVar(tariff(curTariff)%nativeIsWinter)%values
  CASE (seasonSpring)
    seasonMask = econVar(tariff(curTariff)%nativeIsSpring)%values
  CASE (seasonFall)
    seasonMask = econVar(tariff(curTariff)%nativeIsAutumn)%values
  CASE (seasonAnnual)
    seasonMask = 1.0d0  !all months are 1
END SELECT
! finally perform calculations
resultChg = sourceVals * costPer * seasonMask
!store the cost in the name of the variable
econVar(usingVariable)%values = resultChg
!set the flag that it has been evaluated so it won't be evaluated multiple times
econVar(usingVariable)%isEvaluated = .TRUE.
END SUBROUTINE evaluateChargeSimple

SUBROUTINE evaluateChargeBlock(usingVariable)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: usingVariable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: curTariff
INTEGER :: indexInChg
INTEGER :: iBlk
INTEGER :: jMonth
REAL(r64), DIMENSION(MaxNumMonths) :: sourceVals
REAL(r64), DIMENSION(MaxNumMonths) :: blkSzMult
REAL(r64), DIMENSION(MaxNumMonths) :: remainVals
REAL(r64), DIMENSION(MaxNumMonths) :: resultChg
REAL(r64), DIMENSION(MaxNumMonths) :: amountForBlk
REAL(r64), DIMENSION(MaxNumMonths) :: curBlkSz
REAL(r64), DIMENSION(MaxNumMonths) :: curBlkCost
REAL(r64), DIMENSION(MaxNumMonths) :: seasonMask
LOGICAL :: flagAllZero

curTariff = econVar(usingVariable)%tariffIndx
indexInChg = econVar(usingVariable)%index

!check the tariff - make sure they match
IF (chargeBlock(indexInChg)%namePt .NE. usingVariable) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. chargeBlock index does not match variable pointer.')
  CALL ShowContinueError('   Between: '  // TRIM(econVar(usingVariable)%name))
  CALL ShowContinueError('       And: '  // TRIM(econVar(chargeBlock(indexInChg)%namePt)%name))
END IF
IF (chargeBlock(indexInChg)%tariffIndx .NE. curTariff) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. chargeBlock index does not match tariff index.')
  CALL ShowContinueError('   Between: '  // TRIM(tariff(curTariff)%tariffName))
  CALL ShowContinueError('       And: '  // TRIM(tariff(chargeBlock(indexInChg)%tariffIndx)%tariffName))
END IF
! data from the chargeBlock
sourceVals = econVar(chargeBlock(indexInChg)%sourcePt)%values
! find proper season mask
SELECT CASE (chargeBlock(indexInChg)%season)
  CASE (seasonSummer)
    seasonMask = econVar(tariff(curTariff)%nativeIsSummer)%values
  CASE (seasonWinter)
    seasonMask = econVar(tariff(curTariff)%nativeIsWinter)%values
  CASE (seasonSpring)
    seasonMask = econVar(tariff(curTariff)%nativeIsSpring)%values
  CASE (seasonFall)
    seasonMask = econVar(tariff(curTariff)%nativeIsAutumn)%values
  CASE (seasonAnnual)
    seasonMask = 1.0d0  !all months are 1
END SELECT
! get block size multiplier
IF (chargeBlock(indexInChg)%blkSzMultPt .NE. 0) THEN
  blkSzMult = econVar(chargeBlock(indexInChg)%blkSzMultPt)%values
ELSE
  blkSzMult = chargeBlock(indexInChg)%blkSzMultVal
END IF
!initially set the remaing energy or demand to the source
remainVals = sourceVals
!initially set the result (cost) to zero
resultChg = 0.0d0
!loop through the blocks performing calculations
DO iBlk = 1, chargeBlock(indexInChg)%numBlk
  IF (chargeBlock(indexInChg)%blkSzPt(iBlk) .NE. 0) THEN
    curBlkSz = econVar(chargeBlock(indexInChg)%blkSzPt(iBlk))%values
  ELSE
    curBlkSz = chargeBlock(indexInChg)%blkSzVal(iBlk)
  END IF
  IF (chargeBlock(indexInChg)%blkCostPt(iBlk) .NE. 0) THEN
    curBlkCost = econVar(chargeBlock(indexInChg)%blkCostPt(iBlk))%values
  ELSE
    curBlkCost = chargeBlock(indexInChg)%blkCostVal(iBlk)
  END IF
  !loop through the months
  DO jMonth = 1, MaxNumMonths
    IF (seasonMask(jMonth) .EQ. 1) THEN
     ! IF ((curBlkSz(jMonth) * blkSzMult(jMonth)) .GT. remainVals(jMonth)) THEN - CR 6547
      IF (blkSzMult(jMonth) .NE. 0) THEN
        IF (curBlkSz(jMonth) .GT. (remainVals(jMonth) / blkSzMult(jMonth)) ) THEN
          amountForBlk(jMonth) = remainVals(jMonth)
        ELSE
          amountForBlk(jMonth) = curBlkSz(jMonth) * blkSzMult(jMonth)
        END IF
      ELSE
        amountForBlk(jMonth) = 0.0d0
      END IF
      resultChg(jMonth) = resultChg(jMonth) + amountForBlk(jMonth) * curBlkCost(jMonth)
      remainVals(jMonth) = remainVals(jMonth) - amountForBlk(jMonth)
    END IF
  END DO
END DO
! store the amount remaining if a variable is specified
IF (chargeBlock(indexInChg)%remainingPt .NE. 0) THEN
  econVar(chargeBlock(indexInChg)%remainingPt)%values = remainVals
ELSE
  flagAllZero = .TRUE.
  DO jMonth = 1, MaxNumMonths
    IF (seasonMask(jMonth) .EQ. 1) THEN
      IF (remainVals(jMonth) .NE. 0) THEN
        flagAllZero = .FALSE.
      END IF
    END IF
  END DO
  IF (.NOT. flagAllZero) THEN
    CALL ShowWarningError('UtilityCost:Tariff Not all energy or demand was assigned in the block charge: ' //   &
                          TRIM(econVar(usingVariable)%name))
  END IF
END IF
!store the cost in the name of the variable
econVar(usingVariable)%values = resultChg
!set the flag that it has been evaluated so it won't be evaluated multiple times
econVar(usingVariable)%isEvaluated = .TRUE.
END SUBROUTINE evaluateChargeBlock

SUBROUTINE evaluateRatchet(usingVariable)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: usingVariable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: curTariff
INTEGER :: indexInChg
REAL(r64), DIMENSION(MaxNumMonths) :: baselineVals
REAL(r64), DIMENSION(MaxNumMonths) :: adjustmentVals
REAL(r64), DIMENSION(MaxNumMonths) :: multiplierVals
REAL(r64), DIMENSION(MaxNumMonths) :: offsetVals
REAL(r64), DIMENSION(MaxNumMonths) :: seasonFromMask
REAL(r64), DIMENSION(MaxNumMonths) :: seasonToMask
LOGICAL                       :: isMonthly
REAL(r64), DIMENSION(MaxNumMonths) :: adjSeasonal
REAL(r64), DIMENSION(MaxNumMonths) :: adjPeak
REAL(r64), DIMENSION(MaxNumMonths) :: maxAdjBase
REAL(r64)                     :: maximumVal
INTEGER                       :: iMonth
REAL(r64), DIMENSION(MaxNumMonths) :: finalResult

curTariff = econVar(usingVariable)%tariffIndx
indexInChg = econVar(usingVariable)%index

!check the tariff - make sure they match
IF (ratchet(indexInChg)%namePt .NE. usingVariable) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. Ratchet index does not match variable pointer.')
  CALL ShowContinueError('   Between: '  // TRIM(econVar(usingVariable)%name))
  CALL ShowContinueError('       And: '  // TRIM(econVar(ratchet(indexInChg)%namePt)%name))
END IF
IF (ratchet(indexInChg)%tariffIndx .NE. curTariff) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. Ratchet index does not match tariff index.')
  CALL ShowContinueError('   Between: '  // TRIM(tariff(curTariff)%tariffName))
  CALL ShowContinueError('       And: '  // TRIM(tariff(ratchet(indexInChg)%tariffIndx)%tariffName))
END IF
! data from the Ratchet
baselineVals = econVar(ratchet(indexInChg)%baselinePt)%values
adjustmentVals = econVar(ratchet(indexInChg)%adjustmentPt)%values
! determine if multiplier should be based on variable or value
IF (ratchet(indexInChg)%multiplierPt .NE. 0) THEN
  multiplierVals = econVar(ratchet(indexInChg)%multiplierPt)%values
ELSE
  multiplierVals = ratchet(indexInChg)%multiplierVal
END IF
! determine if offset should be based on variable or value
IF (ratchet(indexInChg)%offsetPt .NE. 0) THEN
  offsetVals = econVar(ratchet(indexInChg)%offsetPt)%values
ELSE
  offsetVals = ratchet(indexInChg)%offsetVal
END IF
! find proper season from mask
SELECT CASE (ratchet(indexInChg)%seasonFrom)
  CASE (seasonSummer)
    seasonFromMask = econVar(tariff(curTariff)%nativeIsSummer)%values
    isMonthly = .FALSE.
  CASE (seasonWinter)
    seasonFromMask = econVar(tariff(curTariff)%nativeIsWinter)%values
    isMonthly = .FALSE.
  CASE (seasonSpring)
    seasonFromMask = econVar(tariff(curTariff)%nativeIsSpring)%values
    isMonthly = .FALSE.
  CASE (seasonFall)
    seasonFromMask = econVar(tariff(curTariff)%nativeIsAutumn)%values
    isMonthly = .FALSE.
  CASE (seasonAnnual)
    seasonFromMask = 1.0d0  !all months are 1
    isMonthly = .FALSE.
  CASE (seasonMonthly)
    seasonFromMask = 1.0d0  !all months are 1
    isMonthly = .TRUE.
END SELECT
! find proper season to mask
SELECT CASE (ratchet(indexInChg)%seasonTo)
  CASE (seasonSummer)
    seasonToMask = econVar(tariff(curTariff)%nativeIsSummer)%values
  CASE (seasonWinter)
    seasonToMask = econVar(tariff(curTariff)%nativeIsWinter)%values
  CASE (seasonSpring)
    seasonToMask = econVar(tariff(curTariff)%nativeIsSpring)%values
  CASE (seasonFall)
    seasonToMask = econVar(tariff(curTariff)%nativeIsAutumn)%values
  CASE (seasonAnnual)
    seasonToMask = 1.0d0  !all months are 1
END SELECT
! finally perform calculations
IF (isMonthly) THEN
  adjSeasonal = adjustmentVals
ELSE
  maximumVal = -HUGE(maximumVal)
  DO iMonth = 1, MaxNumMonths
    IF (seasonFromMask(iMonth) .EQ. 1) THEN
      IF (adjustmentVals(iMonth) .GT. maximumVal) THEN
        maximumVal = adjustmentVals(iMonth)
      END IF
    END IF
  END DO
  adjSeasonal = maximumVal
END IF
DO iMonth = 1, MaxNumMonths
  !calculate adjusted peak value after offset and multiplier
  adjPeak(iMonth) = (adjSeasonal(iMonth) + offsetVals(iMonth)) * multiplierVals(iMonth)
  !the maximum of the adjustment and the baseline
  IF (adjPeak(iMonth) .GT. baselineVals(iMonth)) THEN
    maxAdjBase(iMonth) = adjPeak(iMonth)
  ELSE
    maxAdjBase(iMonth) = baselineVals(iMonth)
  END IF
END DO
DO iMonth = 1, MaxNumMonths
  IF (seasonToMask(iMonth) .EQ. 1) THEN
    finalResult(iMonth) = maxAdjBase(iMonth)
  ELSE
    finalResult(iMonth) = baselineVals(iMonth)
  END IF
END DO
!store the cost in the name of the variable
econVar(usingVariable)%values = finalResult
!set the flag that it has been evaluated so it won't be evaluated multiple times
econVar(usingVariable)%isEvaluated = .TRUE.
END SUBROUTINE evaluateRatchet

SUBROUTINE evaluateQualify(usingVariable)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: usingVariable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: curTariff
INTEGER :: indexInQual
REAL(r64), DIMENSION(MaxNumMonths) :: sourceVals
REAL(r64), DIMENSION(MaxNumMonths) :: thresholdVals
INTEGER, DIMENSION(MaxNumMonths) :: monthsQualify
REAL(r64), DIMENSION(MaxNumMonths) :: seasonMask
LOGICAL :: curIsMaximum
LOGICAL :: curIsConsecutive
INTEGER :: curNumberOfMonths
INTEGER :: adjNumberOfMonths
INTEGER :: iMonth
LOGICAL :: isQualified
INTEGER :: monthsInSeason
INTEGER :: cntAllQualMonths
INTEGER :: cntConsecQualMonths
INTEGER :: maxConsecQualMonths

curTariff = econVar(usingVariable)%tariffIndx
indexInQual = econVar(usingVariable)%index
!check the tariff - make sure they match
IF (qualify(indexInQual)%namePt .NE. usingVariable) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. Qualify index does not match variable pointer.')
  CALL ShowContinueError('   Between: '  // TRIM(econVar(usingVariable)%name))
  CALL ShowContinueError('       And: '  // TRIM(econVar(qualify(indexInQual)%namePt)%name))
END IF
IF (qualify(indexInQual)%tariffIndx .NE. curTariff) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. Qualify index does not match tariff index.')
  CALL ShowContinueError('   Between: '  // TRIM(tariff(curTariff)%tariffName))
  CALL ShowContinueError('       And: '  // TRIM(tariff(qualify(indexInQual)%tariffIndx)%tariffName))
END IF
! data from the Qualify
sourceVals = econVar(qualify(indexInQual)%sourcePt)%values
curIsMaximum = qualify(indexInQual)%isMaximum
curIsConsecutive = qualify(indexInQual)%isConsecutive
curNumberOfMonths = qualify(indexInQual)%numberOfMonths
! determine if threshold should be based on variable or value
IF (qualify(indexInQual)%thresholdPt .NE. 0) THEN
  thresholdVals = econVar(qualify(indexInQual)%thresholdPt)%values
ELSE
  thresholdVals = qualify(indexInQual)%thresholdVal
END IF
! find proper season mask
SELECT CASE (qualify(indexInQual)%season)
  CASE (seasonSummer)
    seasonMask = econVar(tariff(curTariff)%nativeIsSummer)%values
  CASE (seasonWinter)
    seasonMask = econVar(tariff(curTariff)%nativeIsWinter)%values
  CASE (seasonSpring)
    seasonMask = econVar(tariff(curTariff)%nativeIsSpring)%values
  CASE (seasonFall)
    seasonMask = econVar(tariff(curTariff)%nativeIsAutumn)%values
  CASE (seasonAnnual)
    seasonMask = 1.0d0  !all months are 1
END SELECT
!any months with no energy use are excluded from the qualification process
DO iMonth = 1, MaxNumMonths
  IF (econVar(tariff(curTariff)%nativeTotalEnergy)%values(iMonth) .EQ. 0) THEN
    seasonMask(iMonth) = 0.0d0
  END IF
END DO
! finally perform calculations
!loop through the months
monthsInSeason = 0
DO iMonth = 1, MaxNumMonths
  IF (seasonMask(iMonth) .EQ. 1) THEN
    monthsInSeason = monthsInSeason + 1
    !use threshold as maximum or minimum
    IF (curIsMaximum) THEN
      IF (sourceVals(iMonth) .GT. thresholdVals(iMonth)) THEN
        monthsQualify(iMonth) = 0 !greater than maximum threshold so it is not qualified
      ELSE
        monthsQualify(iMonth) = 1 !less than maximum threshold so it is qualified
      END IF
    ELSE
      IF (sourceVals(iMonth) .LT. thresholdVals(iMonth)) THEN
        monthsQualify(iMonth) = 0 !less than minimum threshold so it is not qualified
      ELSE
        monthsQualify(iMonth) = 1 !greater than minimum threshold so it is qualified
      END IF
    END IF
  ELSE
    monthsQualify(iMonth) = -1 !flag that indicates not part of the season
  END IF
END DO
!see if the number of months is longer then the number of months and adjust
IF (curNumberOfMonths .GT. monthsInSeason) THEN
  adjNumberOfMonths = monthsInSeason
ELSE
  adjNumberOfMonths = curNumberOfMonths
END IF
!now that each month is qualified or not, depending on the type of test see if the entire qualify passe or not
cntAllQualMonths = 0
cntConsecQualMonths = 0
maxConsecQualMonths = 0
DO iMonth = 1,MaxNumMonths
  SELECT CASE (monthsQualify(iMonth))
    CASE (1) !qualified
      cntAllQualMonths = cntAllQualMonths + 1
      cntConsecQualMonths = cntConsecQualMonths + 1
      !see if the count is greater then the previous count and if it is make it the new count
      IF (cntConsecQualMonths .GT. maxConsecQualMonths) THEN
        maxConsecQualMonths = cntConsecQualMonths
      END IF
    CASE (0) !not qualified
      !reset the counter on consecutive months
      cntConsecQualMonths = 0
  END SELECT
END DO
!if test is for consecutive months
IF (curIsConsecutive) THEN
  IF (maxConsecQualMonths .GE. adjNumberOfMonths) THEN
    isQualified = .TRUE.
  ELSE
    isQualified = .FALSE.
  END IF
ELSE !count not consecutive
  IF (cntAllQualMonths .GE. adjNumberOfMonths) THEN
    isQualified = .TRUE.
  ELSE
    isQualified = .FALSE.
  END IF
END IF
!now update the tariff level qualifier - only update if the tariff is still qualified
!and the current qualifer fails.
IF (tariff(curTariff)%isQualified) THEN
  IF (.NOT. isQualified) THEN
    tariff(curTariff)%isQualified = .FALSE.
    tariff(curTariff)%ptDisqualifier = usingVariable
  END IF
END IF
!store the cost in the name of the variable
econVar(usingVariable)%values = REAL(monthsQualify,r64)
!set the flag that it has been evaluated so it won't be evaluated multiple times
econVar(usingVariable)%isEvaluated = .TRUE.
END SUBROUTINE evaluateQualify

SUBROUTINE addMonthlyCharge(usingVariable)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Include the monthly charges in the calculations

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: usingVariable

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: curTariff
!INTEGER :: iMonth
!INTEGER :: curTotalEnergy

curTariff = econVar(usingVariable)%tariffIndx
!check the tariff - make sure they match
IF (tariff(curTariff)%ptServiceCharges .NE. usingVariable) THEN
  CALL ShowWarningError('UtilityCost:Tariff Debugging issue. Tariff index for service charge does not match variable pointer.')
  CALL ShowContinueError('   Between: '  // TRIM(tariff(curTariff)%tariffName))
  CALL ShowContinueError('       And: '  // TRIM(tariff(tariff(curTariff)%ptServiceCharges)%tariffName))
END IF
IF (tariff(curTariff)%monthChgPt .NE. 0) THEN
  econVar(usingVariable)%values = econVar(tariff(curTariff)%monthChgPt)%values + econVar(usingVariable)%values
ELSE
  econVar(usingVariable)%values = tariff(curTariff)%monthChgVal + econVar(usingVariable)%values
END IF
!zero out months with no energy consumption
!curTotalEnergy = tariff(curTariff)%nativeTotalEnergy
!DO iMonth = 1, MaxNumMonths
!  IF (econVar(curTotalEnergy)%values(iMonth) .EQ. 0) THEN
!    econVar(usingVariable)%values(iMonth) = 0
!  END IF
!END DO
END SUBROUTINE addMonthlyCharge

SUBROUTINE checkMinimumMonthlyCharge(curTariff)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   August 2008
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Check if the total is as big as the minimum monthly charge

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: curTariff

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iMonth
INTEGER :: totalVar
INTEGER :: minMonVar

totalVar = tariff(curTariff)%ptTotal
minMonVar = tariff(curTariff)%minMonthChgPt
! if a variable is defined use that
IF (minMonVar .NE. 0) THEN
  DO iMonth = 1, MaxNumMonths
    IF (econVar(totalVar)%values(iMonth) .LT. econVar(minMonVar)%values(iMonth)) THEN
      econVar(totalVar)%values(iMonth) = econVar(minMonVar)%values(iMonth)
    END IF
  END DO
ELSE !use the constant value
  DO iMonth = 1, MaxNumMonths
    IF (econVar(totalVar)%values(iMonth) .LT. tariff(curTariff)%minMonthChgVal) THEN
      econVar(totalVar)%values(iMonth) = tariff(curTariff)%minMonthChgVal
    END IF
  END DO
END IF
END SUBROUTINE checkMinimumMonthlyCharge


SUBROUTINE setNativeVariables
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Set up the "built in" i.e. native variables that hold
          !    the energy and demand from the simulation.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iTariff
INTEGER :: jPeriod
INTEGER :: kMonth
REAL(r64), DIMENSION(MaxNumMonths) :: monthVal
REAL(r64)    :: bigNumber

bigNumber = HUGE(bigNumber)
DO iTariff = 1, numTariff
  !nativeTotalEnergy
  monthVal = 0.0d0
  DO jPeriod = 1, countPeriod
    DO kMonth = 1, MaxNumMonths
      monthVal(kMonth) = monthVal(kMonth) + tariff(iTariff)%gatherEnergy(jPeriod,kMonth)
    END DO
  END DO
  econVar(tariff(iTariff)%nativeTotalEnergy)%values = monthVal
  !nativeTotalDemand
  monthVal = -bigNumber
  DO jPeriod = 1, countPeriod
    DO kMonth = 1, MaxNumMonths
      IF (tariff(iTariff)%gatherDemand(jPeriod,kMonth) .GT. monthVal(kMonth)) THEN
        monthVal(kMonth) = tariff(iTariff)%gatherDemand(jPeriod,kMonth)
      END IF
    END DO
  END DO
  !if no maximum was set just set to zero
  DO kMonth = 1, MaxNumMonths
    IF (monthVal(kMonth) .EQ. -bigNumber) THEN
      monthVal(kMonth) = 0.0d0
    END IF
  END DO
  econVar(tariff(iTariff)%nativeTotalDemand)%values = monthVal
  DO kMonth = 1, MaxNumMonths
    !nativePeakEnergy
    econVar(tariff(iTariff)%nativePeakEnergy)%values(kMonth) = tariff(iTariff)%gatherEnergy(periodPeak,kMonth)
    !nativePeakDemand
    econVar(tariff(iTariff)%nativePeakDemand)%values(kMonth) = tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    !nativeShoulderEnergy
    econVar(tariff(iTariff)%nativeShoulderEnergy)%values(kMonth) = tariff(iTariff)%gatherEnergy(periodShoulder,kMonth)
    !nativeShoulderDemand
    econVar(tariff(iTariff)%nativeShoulderDemand)%values(kMonth) = tariff(iTariff)%gatherDemand(periodShoulder,kMonth)
    !nativeOffPeakEnergy
    econVar(tariff(iTariff)%nativeOffPeakEnergy)%values(kMonth) = tariff(iTariff)%gatherEnergy(periodOffPeak,kMonth)
    !nativeOffPeakDemand
    econVar(tariff(iTariff)%nativeOffPeakDemand)%values(kMonth) = tariff(iTariff)%gatherDemand(periodOffPeak,kMonth)
    !nativeMidPeakEnergy
    econVar(tariff(iTariff)%nativeMidPeakEnergy)%values(kMonth) = tariff(iTariff)%gatherEnergy(periodMidPeak,kMonth)
    !nativeMidPeakDemand
    econVar(tariff(iTariff)%nativeMidPeakDemand)%values(kMonth) = tariff(iTariff)%gatherDemand(periodMidPeak,kMonth)
    !nativePeakExceedsOffPeak
    monthVal(kMonth) = tariff(iTariff)%gatherDemand(periodPeak,kMonth) - tariff(iTariff)%gatherDemand(periodOffPeak,kMonth)
    IF (monthVal(kMonth) .GT. 0) THEN
      econVar(tariff(iTariff)%nativePeakExceedsOffPeak)%values(kMonth) = monthVal(kMonth)
    ELSE
      econVar(tariff(iTariff)%nativePeakExceedsOffPeak)%values(kMonth) = 0.0d0
    ENDIF
    !nativeOffPeakExceedsPeak
    monthVal(kMonth) = tariff(iTariff)%gatherDemand(periodOffPeak,kMonth) - tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    IF (monthVal(kMonth) .GT. 0) THEN
      econVar(tariff(iTariff)%nativeOffPeakExceedsPeak)%values(kMonth) = monthVal(kMonth)
    ELSE
      econVar(tariff(iTariff)%nativeOffPeakExceedsPeak)%values(kMonth) = 0.0d0
    ENDIF
    !nativePeakExceedsMidPeak
    monthVal(kMonth) = tariff(iTariff)%gatherDemand(periodPeak,kMonth) - tariff(iTariff)%gatherDemand(periodMidPeak,kMonth)
    IF (monthVal(kMonth) .GT. 0) THEN
      econVar(tariff(iTariff)%nativePeakExceedsMidPeak)%values(kMonth) = monthVal(kMonth)
    ELSE
      econVar(tariff(iTariff)%nativePeakExceedsOffPeak)%values(kMonth) = 0.0d0
    ENDIF
    !nativeMidPeakExceedsPeak
    monthVal(kMonth) = tariff(iTariff)%gatherDemand(periodMidPeak,kMonth) - tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    IF (monthVal(kMonth) .GT. 0) THEN
      econVar(tariff(iTariff)%nativeMidPeakExceedsPeak)%values(kMonth) = monthVal(kMonth)
    ELSE
      econVar(tariff(iTariff)%nativeMidPeakExceedsPeak)%values(kMonth) = 0.0d0
    ENDIF
    !nativePeakExceedsShoulder
    monthVal(kMonth) = tariff(iTariff)%gatherDemand(periodPeak,kMonth) - tariff(iTariff)%gatherDemand(periodShoulder,kMonth)
    IF (monthVal(kMonth) .GT. 0) THEN
      econVar(tariff(iTariff)%nativePeakExceedsShoulder)%values(kMonth) = monthVal(kMonth)
    ELSE
      econVar(tariff(iTariff)%nativePeakExceedsShoulder)%values(kMonth) = 0.0d0
    ENDIF
    !nativeShoulderExceedsPeak
    monthVal(kMonth) = tariff(iTariff)%gatherDemand(periodShoulder,kMonth) - tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    IF (monthVal(kMonth) .GT. 0) THEN
      econVar(tariff(iTariff)%nativeShoulderExceedsPeak)%values(kMonth) = monthVal(kMonth)
    ELSE
      econVar(tariff(iTariff)%nativeShoulderExceedsPeak)%values(kMonth) = 0.0d0
    ENDIF
    !nativeIsWinter
    !nativeIsNotWinter
    IF (tariff(iTariff)%seasonForMonth(kMonth) .EQ. seasonWinter) THEN
      econVar(tariff(iTariff)%nativeIsWinter)%values(kMonth) = 1.0d0
      econVar(tariff(iTariff)%nativeIsNotWinter)%values(kMonth) = 0.0d0
    ELSE
      econVar(tariff(iTariff)%nativeIsWinter)%values(kMonth) = 0.0d0
      econVar(tariff(iTariff)%nativeIsNotWinter)%values(kMonth) = 1.0d0
    END IF
    !nativeIsSpring
    !nativeIsNotSpring
    IF (tariff(iTariff)%seasonForMonth(kMonth) .EQ. seasonSpring) THEN
      econVar(tariff(iTariff)%nativeIsSpring)%values(kMonth) = 1.0d0
      econVar(tariff(iTariff)%nativeIsNotSpring)%values(kMonth) = 0.0d0
    ELSE
      econVar(tariff(iTariff)%nativeIsSpring)%values(kMonth) = 0.0d0
      econVar(tariff(iTariff)%nativeIsNotSpring)%values(kMonth) = 1.0d0
    END IF
    !nativeIsSummer
    !nativeIsNotSummer
    IF (tariff(iTariff)%seasonForMonth(kMonth) .EQ. seasonSummer) THEN
      econVar(tariff(iTariff)%nativeIsSummer)%values(kMonth) = 1.0d0
      econVar(tariff(iTariff)%nativeIsNotSummer)%values(kMonth) = 0.0d0
    ELSE
      econVar(tariff(iTariff)%nativeIsSummer)%values(kMonth) = 0.0d0
      econVar(tariff(iTariff)%nativeIsNotSummer)%values(kMonth) = 1.0d0
    END IF
    !nativeIsAutumn
    !nativeIsNotAutumn
    IF (tariff(iTariff)%seasonForMonth(kMonth) .EQ. seasonFall) THEN
      econVar(tariff(iTariff)%nativeIsAutumn)%values(kMonth) = 1.0d0
      econVar(tariff(iTariff)%nativeIsNotAutumn)%values(kMonth) = 0.0d0
    ELSE
      econVar(tariff(iTariff)%nativeIsAutumn)%values(kMonth) = 0.0d0
      econVar(tariff(iTariff)%nativeIsNotAutumn)%values(kMonth) = 1.0d0
    END IF
    !nativePeakAndShoulderEnergy
    econVar(tariff(iTariff)%nativePeakAndShoulderEnergy)%values(kMonth) = &
      tariff(iTariff)%gatherEnergy(periodPeak,kMonth) + tariff(iTariff)%gatherEnergy(periodShoulder,kMonth)
    !nativePeakAndShoulderDemand
    IF (tariff(iTariff)%gatherDemand(periodPeak,kMonth) .GT. tariff(iTariff)%gatherDemand(periodShoulder,kMonth)) THEN
      econVar(tariff(iTariff)%nativePeakAndShoulderDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    ELSE
      econVar(tariff(iTariff)%nativePeakAndShoulderDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodShoulder,kMonth)
    END IF
    !nativePeakAndMidPeakEnergy
    econVar(tariff(iTariff)%nativePeakAndMidPeakEnergy)%values(kMonth) = &
      tariff(iTariff)%gatherEnergy(periodPeak,kMonth) + tariff(iTariff)%gatherEnergy(periodMidPeak,kMonth)
    !nativePeakAndMidPeakDemand
    IF (tariff(iTariff)%gatherDemand(periodPeak,kMonth) .GT. tariff(iTariff)%gatherDemand(periodMidPeak,kMonth)) THEN
      econVar(tariff(iTariff)%nativePeakAndMidPeakDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    ELSE
      econVar(tariff(iTariff)%nativePeakAndMidPeakDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodMidPeak,kMonth)
    END IF
    !nativeShoulderAndOffPeakEnergy
    econVar(tariff(iTariff)%nativeShoulderAndOffPeakEnergy)%values(kMonth) = &
      tariff(iTariff)%gatherEnergy(periodShoulder,kMonth) + tariff(iTariff)%gatherEnergy(periodOffPeak,kMonth)
    !nativeShoulderAndOffPeakDemand
    IF (tariff(iTariff)%gatherDemand(periodShoulder,kMonth) .GT. tariff(iTariff)%gatherDemand(periodOffPeak,kMonth)) THEN
      econVar(tariff(iTariff)%nativeShoulderAndOffPeakDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodShoulder,kMonth)
    ELSE
      econVar(tariff(iTariff)%nativeShoulderAndOffPeakDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodOffPeak,kMonth)
    END IF
    !nativePeakAndOffPeakEnergy
    econVar(tariff(iTariff)%nativePeakAndOffPeakEnergy)%values(kMonth) = &
      tariff(iTariff)%gatherEnergy(periodPeak,kMonth) + tariff(iTariff)%gatherEnergy(periodOffPeak,kMonth)
    !nativePeakAndOffPeakDemand
    IF (tariff(iTariff)%gatherDemand(periodPeak,kMonth) .GT. tariff(iTariff)%gatherDemand(periodOffPeak,kMonth)) THEN
      econVar(tariff(iTariff)%nativePeakAndOffPeakDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodPeak,kMonth)
    ELSE
      econVar(tariff(iTariff)%nativePeakAndOffPeakDemand)%values(kMonth) = &
        tariff(iTariff)%gatherDemand(periodOffPeak,kMonth)
    END IF
    !nativeRealTimePriceCosts
    econVar(tariff(iTariff)%nativeRealTimePriceCosts)%values(kMonth) = tariff(iTariff)%RTPcost(kMonth)
    !nativeAboveCustomerBaseCosts
    econVar(tariff(iTariff)%nativeAboveCustomerBaseCosts)%values(kMonth) = tariff(iTariff)%RTPaboveBaseCost(kMonth)
    !nativeBelowCustomerBaseCosts
    econVar(tariff(iTariff)%nativeBelowCustomerBaseCosts)%values(kMonth) = tariff(iTariff)%RTPbelowBaseCost(kMonth)
    !nativeAboveCustomerBaseEnergy
    econVar(tariff(iTariff)%nativeAboveCustomerBaseEnergy)%values(kMonth) = tariff(iTariff)%RTPaboveBaseEnergy(kMonth)
    !nativeBelowCustomerBaseEnergy
    econVar(tariff(iTariff)%nativeBelowCustomerBaseEnergy)%values(kMonth) = tariff(iTariff)%RTPbelowBaseEnergy(kMonth)
  END DO
END DO
END SUBROUTINE setNativeVariables

SUBROUTINE LEEDtariffReporting
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   October 2012
          !    MODIFIED
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Write the economic results for LEED reporting

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportPredefined

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

INTEGER, EXTERNAL                      :: GetMeterIndex  !an exteral subroutine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: elecFacilMeter
INTEGER :: gasFacilMeter
REAL(r64)    :: elecTotalEne
REAL(r64)    :: gasTotalEne
REAL(r64)    :: otherTotalEne
REAL(r64)    :: elecTotalCost
REAL(r64)    :: gasTotalCost
REAL(r64)    :: otherTotalCost
REAL(r64)    :: allTotalCost
CHARACTER(len=MaxNameLength) :: elecTariffNames
CHARACTER(len=MaxNameLength) :: gasTariffNames
CHARACTER(len=MaxNameLength) :: othrTariffNames
INTEGER :: elecUnits
INTEGER :: gasUnits
INTEGER :: othrUnits
INTEGER :: gasDemWindowUnits
INTEGER :: othrDemWindowUnits
INTEGER :: iTariff

IF (numTariff .GT. 0) THEN
  elecFacilMeter = GetMeterIndex('ELECTRICITY:FACILITY')
  gasFacilMeter = GetMeterIndex('GAS:FACILITY')
  elecTotalEne = 0.0d0
  gasTotalEne = 0.0d0
  otherTotalEne = 0.0d0
  elecTotalCost = 0.0d0
  gasTotalCost = 0.0d0
  otherTotalCost = 0.0d0
  allTotalCost = 0.0d0
  elecUnits = 0
  gasUnits = 0
  othrUnits = 0
  gasDemWindowUnits = 0
  othrDemWindowUnits = 0
  elecTariffNames = ''
  gasTariffNames = ''
  othrTariffNames = ''
  DO iTariff = 1, numTariff
    IF (tariff(iTariff)%isSelected) THEN
      allTotalCost = allTotalCost + tariff(iTariff)%totalAnnualCost
      IF (tariff(iTariff)%kindElectricMtr .GE. kindMeterElecSimple) THEN
        IF (tariff(iTariff)%totalAnnualEnergy .GT. elecTotalEne) elecTotalEne = tariff(iTariff)%totalAnnualEnergy
        elecTotalCost = elecTotalCost + tariff(iTariff)%totalAnnualCost
        elecTariffNames = TRIM(elecTariffNames) // ' ' // tariff(iTariff)%tariffName
        elecUnits = tariff(iTariff)%convChoice
      ELSE IF (tariff(iTariff)%reportMeterIndx .EQ. gasFacilMeter) THEN
        IF (tariff(iTariff)%totalAnnualEnergy .GT. gasTotalEne) gasTotalEne = tariff(iTariff)%totalAnnualEnergy
        gasTotalCost = gasTotalCost + tariff(iTariff)%totalAnnualCost
        gasTariffNames = TRIM(gasTariffNames) // ' ' // tariff(iTariff)%tariffName
        gasUnits = tariff(iTariff)%convChoice
        gasDemWindowUnits = tariff(iTariff)%demandWindow
      ELSE
        IF (tariff(iTariff)%totalAnnualEnergy .GT. otherTotalEne) otherTotalEne = tariff(iTariff)%totalAnnualEnergy
        otherTotalCost = otherTotalCost + tariff(iTariff)%totalAnnualCost
        othrTariffNames = TRIM(othrTariffNames) // ' ' // tariff(iTariff)%tariffName
        othrUnits = tariff(iTariff)%convChoice
        othrDemWindowUnits = tariff(iTariff)%demandWindow
      END IF
    END IF
  END DO
  !names of the rates
  CALL PreDefTableEntry(pdchLeedEtsRtNm,'Electricity',elecTariffNames)
  CALL PreDefTableEntry(pdchLeedEtsRtNm,'Natural Gas',gasTariffNames)
  CALL PreDefTableEntry(pdchLeedEtsRtNm,'Other',othrTariffNames)
  !virtual rate
  IF (elecTotalEne .NE. 0) CALL PreDefTableEntry(pdchLeedEtsVirt,'Electricity',elecTotalCost/elecTotalEne,3)
  IF (gasTotalEne .NE. 0) CALL PreDefTableEntry(pdchLeedEtsVirt,'Natural Gas',gasTotalCost/gasTotalEne,3)
  IF (otherTotalEne .NE. 0) CALL PreDefTableEntry(pdchLeedEtsVirt,'Other',otherTotalCost/otherTotalEne,3)
  !units
  CALL PreDefTableEntry(pdchLeedEtsEneUnt,'Electricity',convEneStrings(elecUnits))
  CALL PreDefTableEntry(pdchLeedEtsEneUnt,'Natural Gas',convEneStrings(gasUnits))
  CALL PreDefTableEntry(pdchLeedEtsEneUnt,'Other',convEneStrings(othrUnits))
  CALL PreDefTableEntry(pdchLeedEtsDemUnt,'Electricity',convDemStrings(elecUnits))
  CALL PreDefTableEntry(pdchLeedEtsDemUnt,'Natural Gas',TRIM(convDemStrings(gasUnits)) // TRIM(demWindowStrings(gasDemWindowUnits)))
  CALL PreDefTableEntry(pdchLeedEtsDemUnt,'Other',TRIM(convDemStrings(othrUnits)) // TRIM(demWindowStrings(othrDemWindowUnits)))
  ! total cost
  CALL PreDefTableEntry(pdchLeedEcsTotal,'Electricity',elecTotalCost,2)
  CALL PreDefTableEntry(pdchLeedEcsTotal,'Natural Gas',gasTotalCost,2)
  CALL PreDefTableEntry(pdchLeedEcsTotal,'Other',otherTotalCost,2)
  ! save the total costs for later to compute process fraction
  LEEDelecCostTotal = elecTotalCost
  LEEDgasCostTotal = gasTotalCost
  LEEDothrCostTotal = otherTotalCost
  CALL PreDefTableEntry(pdchLeedEcsTotal,'Total',elecTotalCost + gasTotalCost + otherTotalCost,2)
END IF
END SUBROUTINE LEEDtariffReporting


SUBROUTINE WriteTabularTariffReports
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       January 2010, Kyle Benne
          !                   Added SQLite output
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportTabular, ONLY: WriteReportHeaders, WriteSubtitle, WriteTable, RealToStr, writeTextLine, &
                               buildingGrossFloorArea,  buildingConditionedFloorArea, DetermineBuildingFloorArea, &
                               LookupSItoIP, convertIP, unitsStyle, unitsStyleInchPound
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

INTEGER, EXTERNAL                      :: GetMeterIndex  !an exteral subroutine

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
!other local variables
INTEGER :: elecFacilMeter
INTEGER :: gasFacilMeter
REAL(r64)    :: elecTotalCost
REAL(r64)    :: gasTotalCost
REAL(r64)    :: otherTotalCost
REAL(r64)    :: allTotalCost
CHARACTER(len=2000) :: outString !an arbitarilty long string
INTEGER :: curStep
INTEGER :: indexInChg
INTEGER :: iTariff
INTEGER :: kVar
INTEGER :: lStep
CHARACTER(len=MaxNameLength) :: SIunit = ''
INTEGER :: unitConvIndex = 0
REAL(r64) :: perAreaUnitConv = 0.0d0
CHARACTER(len=MaxNameLength) :: perAreaUnitName = ''


  ! compute floor area if no ABUPS
  IF (buildingConditionedFloorArea == 0.0d0) THEN
    CALL DetermineBuildingFloorArea
  ENDIF

  ! do unit conversions if necessary
  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    SIunit = '[~~$~~/m2]'
    CALL LookupSItoIP(SIunit, unitConvIndex, perAreaUnitName)
    perAreaUnitConv = convertIP(unitConvIndex,1.0d0)
  ELSE
    perAreaUnitName = '[~~$~~/m2]'
    perAreaUnitConv = 1.0d0
  END IF


IF (numTariff .GT. 0) THEN
  CALL DisplayString('Writing Tariff Reports')
  econVar%isReported = .FALSE.
  !CALL selectTariff moved to the end of computeTariff.
  CALL showWarningsBasedOnTotal
  !---------------------------------
  ! Economics Results Summary Report
  !---------------------------------
  CALL WriteReportHeaders('Economics Results Summary Report','Entire Facility',1)
  elecFacilMeter = GetMeterIndex('ELECTRICITY:FACILITY')
  gasFacilMeter = GetMeterIndex('GAS:FACILITY')
  !
  !---- Annual Summary
  !
  ALLOCATE(rowHead(3))
  ALLOCATE(columnHead(4))
  ALLOCATE(columnWidth(4))
  ALLOCATE(tableBody(3,4))
  tableBody = ''
  columnHead(1) = 'Electric'
  columnHead(2) = 'Gas'
  columnHead(3) = 'Other'
  columnHead(4) = 'Total'
  rowHead(1) = 'Cost [~~$~~]'
  rowHead(2) = 'Cost per Total Building Area ' // TRIM(perAreaUnitName)
  rowHead(3) = 'Cost per Net Conditioned Building Area ' // TRIM(perAreaUnitName)
  elecTotalCost = 0.0d0
  gasTotalCost = 0.0d0
  otherTotalCost = 0.0d0
  allTotalCost = 0.0d0
  DO iTariff = 1, numTariff
    IF (tariff(iTariff)%isSelected) THEN
      allTotalCost = allTotalCost + tariff(iTariff)%totalAnnualCost
      IF (tariff(iTariff)%kindElectricMtr .GE. kindMeterElecSimple) THEN
        elecTotalCost = elecTotalCost + tariff(iTariff)%totalAnnualCost
      ELSE IF (tariff(iTariff)%reportMeterIndx .EQ. gasFacilMeter) THEN
        gasTotalCost = gasTotalCost + tariff(iTariff)%totalAnnualCost
      ELSE
        otherTotalCost = otherTotalCost + tariff(iTariff)%totalAnnualCost
! removed because this was confusing        columnHead(3) = tariff(iTariff)%reportMeter
      END IF
    END IF
  END DO
  tableBody(1,1) = TRIM(RealToStr(elecTotalCost,2))
  tableBody(1,2) = TRIM(RealToStr(gasTotalCost,2))
  tableBody(1,3) = TRIM(RealToStr(otherTotalCost,2))
  tableBody(1,4) = TRIM(RealToStr(allTotalCost,2))
  If (buildingGrossFloorArea > 0.0d0) then
   tableBody(2,1) = TRIM(RealToStr((elecTotalCost/buildingGrossFloorArea) * perAreaUnitConv, 2))
   tableBody(2,2) = TRIM(RealToStr((gasTotalCost/buildingGrossFloorArea) * perAreaUnitConv,2))
   tableBody(2,3) = TRIM(RealToStr((otherTotalCost/buildingGrossFloorArea) * perAreaUnitConv,2))
   tableBody(2,4) = TRIM(RealToStr((allTotalCost/buildingGrossFloorArea) * perAreaUnitConv,2))
  endif
  IF (buildingConditionedFloorArea > 0.0d0) THEN
   tableBody(3,1) = TRIM(RealToStr((elecTotalCost/buildingConditionedFloorArea) * perAreaUnitConv, 2))
   tableBody(3,2) = TRIM(RealToStr((gasTotalCost/buildingConditionedFloorArea) * perAreaUnitConv,2))
   tableBody(3,3) = TRIM(RealToStr((otherTotalCost/buildingConditionedFloorArea) * perAreaUnitConv,2))
   tableBody(3,4) = TRIM(RealToStr((allTotalCost/buildingConditionedFloorArea) * perAreaUnitConv,2))
  ENDIF
  columnWidth = 14 !array assignment - same for all columns
  CALL WriteSubtitle('Annual Cost')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Economics Results Summary Report',&
                                      'Entire Facility',&
                                      'Annual Cost')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Tariff Summary
  !
  ALLOCATE(rowHead(numTariff))
  ALLOCATE(columnHead(6))
  ALLOCATE(columnWidth(6))
  ALLOCATE(tableBody(numTariff,6))
  tableBody = ''
  columnHead(1) = 'Selected'
  columnHead(2) = 'Qualified'
  columnHead(3) = 'Meter'
  columnHead(4) = 'Buy or Sell'
  columnHead(5) = 'Group'
  columnHead(6) = 'Annual Cost (~~$~~)'
  DO iTariff = 1, numTariff
    rowHead(iTariff) = tariff(iTariff)%tariffName
    IF (tariff(iTariff)%isSelected) THEN
      tableBody(iTariff,1) = 'Yes'
    ELSE
      tableBody(iTariff,1) = 'No'
    END IF
    IF (tariff(iTariff)%isQualified) THEN
      tableBody(iTariff,2) = 'Yes'
    ELSE
      tableBody(iTariff,2) = 'No'
    END IF
    tableBody(iTariff,3) = tariff(iTariff)%reportMeter
    SELECT CASE (tariff(iTariff)%buyOrSell)
      CASE (buyFromUtility)
        tableBody(iTariff,4) = 'Buy'
      CASE (sellToUtility)
        tableBody(iTariff,4) = 'Sell'
      CASE (netMetering)
        tableBody(iTariff,4) = 'Net'
    END SELECT
    IF (tariff(iTariff)%groupName .EQ. '') THEN
      tableBody(iTariff,5) = '(none)'
    ELSE
      tableBody(iTariff,5) = tariff(iTariff)%groupName
    END IF
    tableBody(iTariff,6) = TRIM(RealToStr(tariff(iTariff)%totalAnnualCost,2))
  END DO
  columnWidth = 14 !array assignment - same for all columns
  CALL WriteSubtitle('Tariff Summary')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Economics Results Summary Report',&
                                      'Entire Facility',&
                                      'Tariff Summary')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !---------------------------------
  ! Tariff Report
  !---------------------------------
  DO iTariff = 1, numTariff
    CALL WriteReportHeaders('Tariff Report',tariff(iTariff)%tariffName,1)
    ALLOCATE(rowHead(7))
    ALLOCATE(columnHead(1))
    ALLOCATE(columnWidth(1))
    ALLOCATE(tableBody(7,1))
    tableBody = ''
    columnHead(1) = 'Parameter'
    rowHead(1) = 'Meter'
    rowHead(2) = 'Selected'
    rowHead(3) = 'Group'
    rowHead(4) = 'Qualified'
    rowHead(5) = 'Disqualifier'
    rowHead(6) = 'Computation'
    rowHead(7) = 'Units'
    tableBody(1,1) = tariff(iTariff)%reportMeter
    IF (tariff(iTariff)%isSelected) THEN
      tableBody(2,1) = 'Yes'
    ELSE
      tableBody(2,1) = 'No'
    END IF
    IF (tariff(iTariff)%groupName .EQ. '') THEN
      tableBody(3,1) = '(none)'
    ELSE
      tableBody(3,1) = tariff(iTariff)%groupName
    END IF
    IF (tariff(iTariff)%isQualified) THEN
      tableBody(4,1) = 'Yes'
    ELSE
      tableBody(4,1) = 'No'
    END IF
    IF (tariff(iTariff)%isQualified) THEN
      tableBody(5,1) = 'n/a'
    ELSE
      tableBody(5,1) = econVar(tariff(iTariff)%ptDisqualifier)%name
    END IF
    IF (computation(iTariff)%isUserDef) THEN
      tableBody(6,1) = computation(iTariff)%computeName
    ELSE
      tableBody(6,1) = 'automatic'
    END IF
    SELECT CASE (tariff(iTariff)%convChoice)
      CASE (conversionUSERDEF)
        tableBody(7,1) = 'User Defined'
      CASE (conversionKWH)
        tableBody(7,1) = 'kWh'
      CASE (conversionTHERM)
        tableBody(7,1) = 'Therm'
      CASE (conversionMMBTU)
        tableBody(7,1) = 'MMBtu'
      CASE (conversionMJ)
        tableBody(7,1) = 'MJ'
      CASE (conversionKBTU)
        tableBody(7,1) = 'kBtu'
      CASE (conversionMCF)
        tableBody(7,1) = 'MCF'
      CASE (conversionCCF)
        tableBody(7,1) = 'CCF'
    END SELECT
    columnWidth = 14 !array assignment - same for all columns
    CALL WriteSubtitle('General')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'Tariff Report',&
                                         tariff(iTariff)%tariffName,&
                                        'General')
    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)
    !
    !---- Categories
    !
    econVar%activeNow = .FALSE.
    econVar(tariff(iTariff)%ptEnergyCharges)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptDemandCharges)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptServiceCharges)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptBasis)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptAdjustment)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptSurcharge)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptSubtotal)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptTaxes)%activeNow = .TRUE.
    econVar(tariff(iTariff)%ptTotal)%activeNow = .TRUE.
    CALL ReportEconomicVariable('Categories',.FALSE.,.TRUE.,tariff(iTariff)%tariffName)
    !
    !---- Charges
    !
    econVar%activeNow = .FALSE.
    DO kVar = 1,numEconVar
      IF (econVar(kVar)%tariffIndx .EQ. iTariff) THEN
        IF ((econVar(kVar)%kindOfObj .EQ. kindChargeSimple) .OR. (econVar(kVar)%kindOfObj .EQ. kindChargeBlock)) THEN
          econVar(kVar)%activeNow = .TRUE.
        END IF
      END IF
    END DO
    CALL ReportEconomicVariable('Charges',.TRUE.,.TRUE.,tariff(iTariff)%tariffName)
    !
    !---- Sources for Charges
    !
    econVar%activeNow = .FALSE.
    DO kVar = 1,numEconVar
      IF (econVar(kVar)%tariffIndx .EQ. iTariff) THEN
        indexInChg = econVar(kVar)%index
        IF (econVar(kVar)%kindOfObj .EQ. kindChargeSimple) THEN
          IF (chargeSimple(indexInChg)%sourcePt .GT. 0) THEN
            econVar(chargeSimple(indexInChg)%sourcePt)%activeNow = .TRUE.
          END IF
        ELSE IF (econVar(kVar)%kindOfObj .EQ. kindChargeBlock) THEN
          IF (chargeBlock(indexInChg)%sourcePt .GT. 0) THEN
            econVar(chargeBlock(indexInChg)%sourcePt)%activeNow = .TRUE.
          END IF
        END IF
      END IF
    END DO
    CALL ReportEconomicVariable('Corresponding Sources for Charges',.FALSE.,.FALSE.,tariff(iTariff)%tariffName)
    !
    !---- Rachets
    !
    econVar%activeNow = .FALSE.
    DO kVar = 1,numEconVar
      IF (econVar(kVar)%tariffIndx .EQ. iTariff) THEN
        IF (econVar(kVar)%kindOfObj .EQ. kindRatchet) THEN
          econVar(kVar)%activeNow = .TRUE.
        END IF
      END IF
    END DO
    CALL ReportEconomicVariable('Ratchets',.FALSE.,.FALSE.,tariff(iTariff)%tariffName)
    !
    !---- Qualifies
    !
    econVar%activeNow = .FALSE.
    DO kVar = 1,numEconVar
      IF (econVar(kVar)%tariffIndx .EQ. iTariff) THEN
        IF (econVar(kVar)%kindOfObj .EQ. kindQualify) THEN
          econVar(kVar)%activeNow = .TRUE.
        END IF
      END IF
    END DO
    CALL ReportEconomicVariable('Qualifies',.FALSE.,.FALSE.,tariff(iTariff)%tariffName)
    !
    !---- Native Variables
    !
    econVar%activeNow = .FALSE.
    DO kVar = tariff(iTariff)%firstNative,tariff(iTariff)%lastNative
      econVar(kVar)%activeNow = .TRUE.
    END DO
    CALL ReportEconomicVariable('Native Variables',.FALSE.,.FALSE.,tariff(iTariff)%tariffName)
    !
    !---- Other Variables
    !
    econVar%activeNow = .FALSE.
    DO kVar = 1,numEconVar
      IF (econVar(kVar)%tariffIndx .EQ. iTariff) THEN
        IF (.NOT. econVar(kVar)%isReported) THEN
          econVar(kVar)%activeNow = .TRUE.
        END IF
      END IF
    END DO
    CALL ReportEconomicVariable('Other Variables',.FALSE.,.FALSE.,tariff(iTariff)%tariffName)
    !
    !---- Computation
    !
    IF (computation(iTariff)%isUserDef) THEN
      CALL writeTextLine('Computation -  User Defined',.TRUE.)
    ELSE
      CALL writeTextLine('Computation -  Automatic',.TRUE.)
    END IF
    outString = ''
    DO lStep = computation(iTariff)%firstStep,computation(iTariff)%lastStep
      curStep = steps(lStep)
      SELECT CASE (curStep)
        CASE (0)  !end of line
          CALL writeTextLine(outString)
          outString = ''
        CASE (1:) !all positive values are a reference to an econVar
          outString = TRIM(econVar(curStep)%name) // ' ' // TRIM(outString)
        CASE (opSUM)
          outString = 'SUM ' // TRIM(outString)
        CASE (opMULTIPLY)
          outString = 'MULTIPLY ' // TRIM(outString)
        CASE (opSUBTRACT)
          outString = 'SUBTRACT ' // TRIM(outString)
        CASE (opDIVIDE)
          outString = 'DIVIDE ' // TRIM(outString)
        CASE (opABSOLUTE)
          outString = 'ABSOLUTE ' // TRIM(outString)
        CASE (opINTEGER)
          outString = 'INTEGER ' // TRIM(outString)
        CASE (opSIGN)
          outString = 'SIGN ' // TRIM(outString)
        CASE (opROUND)
          outString = 'ROUND ' // TRIM(outString)
        CASE (opMAXIMUM)
          outString = 'MAXIMUM ' // TRIM(outString)
        CASE (opMINIMUM)
          outString = 'MINIMUM ' // TRIM(outString)
        CASE (opEXCEEDS)
          outString = 'EXCEEDS ' // TRIM(outString)
        CASE (opANNUALMINIMUM)
          outString = 'ANNUALMINIMUM ' // TRIM(outString)
        CASE (opANNUALMAXIMUM)
          outString = 'ANNUALMAXIMUM ' // TRIM(outString)
        CASE (opANNUALSUM)
          outString = 'ANNUALSUM ' // TRIM(outString)
        CASE (opANNUALAVERAGE)
          outString = 'ANNUALAVERAGE ' // TRIM(outString)
        CASE (opANNUALOR)
          outString = 'ANNUALOR ' // TRIM(outString)
        CASE (opANNUALAND)
          outString = 'ANNUALAND ' // TRIM(outString)
        CASE (opANNUALMAXIMUMZERO)
          outString = 'ANNUALMAXIMUMZERO ' // TRIM(outString)
        CASE (opANNUALMINIMUMZERO)
          outString = 'ANNUALMINIMUMZERO ' // TRIM(outString)
        CASE (opIF)
          outString = 'IF ' // TRIM(outString)
        CASE (opGREATERTHAN)
          outString = 'GREATERTHAN ' // TRIM(outString)
        CASE (opGREATEREQUAL)
          outString = 'GREATEREQUAL ' // TRIM(outString)
        CASE (opLESSTHAN)
          outString = 'LESSTHAN ' // TRIM(outString)
        CASE (opLESSEQUAL)
          outString = 'LESSEQUAL ' // TRIM(outString)
        CASE (opEQUAL)
          outString = 'EQUAL ' // TRIM(outString)
        CASE (opNOTEQUAL)
          outString = 'NOTEQUAL ' // TRIM(outString)
        CASE (opAND)
          outString = 'AND ' // TRIM(outString)
        CASE (opOR)
          outString = 'OR ' // TRIM(outString)
        CASE (opNOT)
          outString = 'NOT ' // TRIM(outString)
        CASE (opADD)
          outString = 'ADD ' // TRIM(outString)
        CASE (opNOOP) !should clear the outString when done debugging
          !outString = ''
          outString = 'FROM ' // TRIM(outString)
      END SELECT
    END DO
  END DO
END IF
END SUBROUTINE WriteTabularTariffReports


SUBROUTINE showWarningsBasedOnTotal
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Get the annual maximum and sum for the econVariable.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iTariff
IF (numTariff .GT. 0) THEN
  DO iTariff = 1, numTariff
    SELECT CASE (tariff(iTariff)%buyOrSell)
      CASE (buyFromUtility)
        IF (tariff(iTariff)%totalAnnualCost .LT. 0) THEN
          CALL ShowWarningError('UtilityCost:Tariff: '//  &
             'A negative annual total cost when buying electricity from a utility is unusual. ')
          CALL ShowContinueError('  In UtilityCost:Tariff named ' // TRIM(tariff(iTariff)%tariffName))
        END IF
      CASE (sellToUtility)
        IF (tariff(iTariff)%totalAnnualCost .GT. 0) THEN
          CALL ShowWarningError('UtilityCost:Tariff: '//  &
             'A positive annual total cost when selling electricity to a utility is unusual. ')
          CALL ShowContinueError('  In UtilityCost:Tariff named ' // TRIM(tariff(iTariff)%tariffName))
        END IF
    END SELECT
  END DO
END IF
END SUBROUTINE showWarningsBasedOnTotal

SUBROUTINE getMaxAndSum(varPointer,sumResult,maxResult)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Get the annual maximum and sum for the econVariable.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: varPointer
REAL(r64),INTENT(OUT) :: sumResult
REAL(r64),INTENT(OUT) :: maxResult

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: sumVal
REAL(r64) :: maximumVal
REAL(r64) :: curVal
INTEGER :: jMonth

sumVal = 0.0d0
maximumVal = -HUGE(maximumVal)
DO jMonth = 1,12 !note not all months get printed out if more than 12 are used.- need to fix this later
  curVal = econVar(varPointer)%values(jMonth)
  sumVal = sumVal + curVal
  IF (curVal .GT. maximumVal) THEN
    maximumVal = curVal
  END IF
END DO
sumResult = sumVal
maxResult = maximumVal
END SUBROUTINE getMaxAndSum

SUBROUTINE ReportEconomicVariable(titleString,includeCategory,showCurrencySymbol,forString)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       January 2010, Kyle Benne
          !                   Added sqlite output
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    Report all econVar that show as activeNow

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportTabular, ONLY: WriteReportHeaders, WriteSubtitle, WriteTable, RealToStr
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: titleString
LOGICAL, INTENT(IN)          :: includeCategory
LOGICAL, INTENT(IN)          :: showCurrencySymbol
CHARACTER(len=*), INTENT(IN) :: forString

! The majority of the input is the econVar array

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
REAL(r64)    :: sumVal
REAL(r64)    :: maximumVal
REAL(r64)    :: curVal
INTEGER :: curIndex
INTEGER :: curCatPt
INTEGER :: curCategory

INTEGER :: iVar
INTEGER :: jMonth
INTEGER :: cntOfVar
INTEGER :: nCntOfVar
cntOfVar = 0
DO iVar = 1, numEconVar
  IF (econVar(iVar)%activeNow) THEN
    cntOfVar = cntOfVar + 1
  END IF
END DO
IF (includeCategory) THEN
  ALLOCATE(rowHead(cntOfVar))
  ALLOCATE(columnHead(15))
  ALLOCATE(columnWidth(15))
  ALLOCATE(tableBody(cntOfVar,15))
ELSE
  ALLOCATE(rowHead(cntOfVar))
  ALLOCATE(columnHead(14))
  ALLOCATE(columnWidth(14))
  ALLOCATE(tableBody(cntOfVar,14))
END IF
! column names
columnHead(1) = 'Jan'
columnHead(2) = 'Feb'
columnHead(3) = 'Mar'
columnHead(4) = 'Apr'
columnHead(5) = 'May'
columnHead(6) = 'Jun'
columnHead(7) = 'Jul'
columnHead(8) = 'Aug'
columnHead(9) = 'Sep'
columnHead(10) = 'Oct'
columnHead(11) = 'Nov'
columnHead(12) = 'Dec'
columnHead(13) = 'Sum'
columnHead(14) = 'Max'
IF (includeCategory) THEN
  columnHead(15) = 'Category'
END IF
nCntOfVar = 0
!row names
DO iVar = 1,numEconVar
  IF (econVar(iVar)%activeNow) THEN
    nCntOfVar = nCntOfVar + 1
    IF (showCurrencySymbol) THEN
      rowHead(nCntOfVar) = TRIM(econVar(iVar)%name) // ' (~~$~~)'
    ELSE
    rowHead(nCntOfVar) = TRIM(econVar(iVar)%name)
  END IF
  END IF
END DO
! fill the body
nCntOfVar = 0
DO iVar = 1,numEconVar
  IF (econVar(iVar)%activeNow) THEN
    nCntOfVar = nCntOfVar + 1
    DO jMonth = 1,12 !note not all months get printed out if more than 12 are used.- need to fix this later
      curVal = econVar(iVar)%values(jMonth)
      IF ((curVal .GT. 0) .AND. (curVal .LT. 1)) THEN
        tableBody(nCntOfVar,jMonth) = TRIM(RealToStr(curVal,4))
      ELSE
        tableBody(nCntOfVar,jMonth) = TRIM(RealToStr(curVal,2))
      END IF
    END DO
    CALL getMaxAndSum(iVar,sumVal,maximumVal)
    tableBody(nCntOfVar,13) = TRIM(RealToStr(sumVal,2))
    tableBody(nCntOfVar,14) = TRIM(RealToStr(maximumVal,2))
    IF (includeCategory) THEN
      !first find category
      curCategory = 0
      curIndex = econVar(iVar)%index
      SELECT CASE (econVar(iVar)%kindOfObj)
        CASE (kindChargeSimple)
          IF ((curIndex .GE. 1) .AND. (curIndex .LE. numChargeSimple)) THEN
            curCatPt = chargeSimple(curIndex)%categoryPt
          END IF
        CASE (kindChargeBlock)
          IF ((curIndex .GE. 1) .AND. (curIndex .LE. numChargeBlock)) THEN
            curCatPt = chargeBlock(curIndex)%categoryPt
          END IF
      END SELECT
      IF ((curCatPt .GE. 1) .AND. (curCatPt .LE. numEconVar)) THEN
        curCategory = econVar(curCatPt)%specific
      ENDIF
      SELECT CASE (curCategory)
        CASE (catEnergyCharges)
          tableBody(nCntOfVar,15) = 'EnergyCharges'
        CASE (catDemandCharges)
          tableBody(nCntOfVar,15) = 'DemandCharges'
        CASE (catServiceCharges)
          tableBody(nCntOfVar,15) = 'ServiceCharges'
        CASE (catBasis)
          tableBody(nCntOfVar,15) = 'Basis'
        CASE (catAdjustment)
          tableBody(nCntOfVar,15) = 'Adjustment'
        CASE (catSurcharge)
          tableBody(nCntOfVar,15) = 'Surcharge'
        CASE (catSubtotal)
          tableBody(nCntOfVar,15) = 'Subtotal'
        CASE (catTaxes)
          tableBody(nCntOfVar,15) = 'Taxes'
        CASE (catTotal)
          tableBody(nCntOfVar,15) = 'Total'
        CASE DEFAULT
          tableBody(nCntOfVar,15) = 'none'
      END SELECT
    END IF
    econVar(iVar)%isReported = .TRUE.
  END IF
END DO
columnWidth = 14 !array assignment - same for all columns
CALL WriteSubtitle(titleString)
CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                    'Tariff Report',&
                                     forString,&
                                     titleString)
DEALLOCATE(columnHead)
DEALLOCATE(rowHead)
DEALLOCATE(columnWidth)
DEALLOCATE(tableBody)

END SUBROUTINE ReportEconomicVariable


SUBROUTINE selectTariff
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   July 2004
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !    To select tariffs for each combination of meter and
          !    group.  If multipler tariffs have the same meter and
          !    group, then select the one with the lowest cost.
          !    For electric tariffs, since they may have buy, sell, or
          !    netmetering, they need to be combined more carefully.
          !    Multiple meters are used but buy + sell might be more or
          !    less expensive than netmeter.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: EnergyMeters, NumEnergyMeters

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
INTEGER :: totalVarPt
INTEGER :: totEneVarPt
REAL(r64)    :: annualTotal
REAL(r64)    :: annEneTotal
INTEGER :: iTariff
INTEGER :: jMonth
INTEGER :: kTariff
INTEGER :: lMin
INTEGER :: mGroup
INTEGER,DIMENSION(:),ALLOCATABLE  :: groupIndex     !index number (in tariff) for the group name
INTEGER,DIMENSION(:),ALLOCATABLE  :: MinTariffIndex !tariff index for the Minimum value
INTEGER :: numMins
INTEGER :: curMinTariffIndex
LOGICAL :: isFound
INTEGER :: groupCount
INTEGER :: lowestSimpleTariff
INTEGER :: lowestPurchaseTariff
INTEGER :: lowestSurplusSoldTariff
INTEGER :: lowestNetMeterTariff

ALLOCATE(groupIndex(numTariff))
groupIndex = 0
groupCount = 0
numMins = 0
ALLOCATE(MinTariffIndex(numTariff))
MinTariffIndex = 0
DO iTariff = 1, numTariff
  !determine if this is meter related to electricity
  IF (Tariff(iTariff)%reportMeterIndx .NE. 0) THEN
    SELECT CASE (TRIM(MakeUpperCase(EnergyMeters(Tariff(iTariff)%reportMeterIndx)%ResourceType)))
      CASE('ELECTRICITY')
        tariff(iTariff)%kindElectricMtr = kindMeterElecSimple
      CASE ('ELECTRICITYPRODUCED')
        tariff(iTariff)%kindElectricMtr = kindMeterElecProduced
      CASE ('ELECTRICITYPURCHASED')
        tariff(iTariff)%kindElectricMtr = kindMeterElecPurchased
      CASE ('ELECTRICITYSURPLUSSOLD')
        tariff(iTariff)%kindElectricMtr = kindMeterElecSurplusSold
      CASE ('ELECTRICITYNET')
       tariff(iTariff)%kindElectricMtr = kindMeterElecNet
      CASE DEFAULT
        tariff(iTariff)%kindElectricMtr = kindMeterNotElectric
    END SELECT
  ELSE
    tariff(iTariff)%kindElectricMtr = kindMeterNotElectric
  END IF
  ! compute the total annual cost of each tariff
  totalVarPt = tariff(iTariff)%ptTotal
  totEneVarPt = tariff(iTariff)%nativeTotalEnergy
  annualTotal = 0.0d0
  annEneTotal = 0.0d0
  DO jMonth = 1,MaxNumMonths
    annualTotal = annualTotal + econVar(totalVarPt)%values(jMonth)
    annEneTotal = annEneTotal + econVar(totEneVarPt)%values(jMonth)
  END DO
  tariff(iTariff)%totalAnnualCost = annualTotal
  tariff(iTariff)%totalAnnualEnergy = annEneTotal
  ! Set the groupIndex
  IF (groupIndex(iTariff) .EQ. 0) THEN
    !set the current item to the tariff index
    groupCount = groupCount + 1
    groupIndex(iTariff) = groupCount
    !set all remaining matching items to the same index
    DO kTariff = iTariff + 1,numTariff
      IF (SameString(tariff(kTariff)%groupName,tariff(iTariff)%groupName)) THEN
        groupIndex(kTariff) = groupCount
      END IF
    END DO
  END IF
END DO
! First process the all tariff and identify the lowest cost for each type of meter and group.
DO iTariff = 1, numTariff
  IF (tariff(iTariff)%isQualified) THEN
    isFound = .FALSE.
    DO lMin = 1,numMins
      curMinTariffIndex = MinTariffIndex(lMin)
      !find matching meter and group
      IF (tariff(iTariff)%reportMeterIndx .EQ. tariff(curMinTariffIndex)%reportMeterIndx) THEN
        IF (groupIndex(iTariff) .EQ. groupIndex(curMinTariffIndex)) THEN
          isFound = .TRUE.
          !found the matching mater and group now test if smaller Min is current tariff
          IF (tariff(iTariff)%totalAnnualCost .LT. tariff(curMinTariffIndex)%totalAnnualCost) THEN
            MinTariffIndex(lMin) = iTariff
            !select the new Minimum tariff and deselect the one that was just exceeded
            tariff(curMinTariffIndex)%isSelected = .FALSE.
            tariff(iTariff)%isSelected = .TRUE.
          END IF
        END IF
      END IF
    END DO
    IF (.NOT. isFound) THEN
      numMins = numMins + 1
      IF (numMins .GT. numTariff) THEN
        CALL ShowWarningError('UtilityCost:Tariff Debugging error numMins greater than numTariff.')
      END IF
      MinTariffIndex(numMins) = iTariff
      ! tariff(numMins)%isSelected = .TRUE.  !original
      tariff(iTariff)%isSelected = .TRUE.  !BTG changed 2/7/2005     CR6573
    END IF
  END IF
END DO
! Now select for the electric meters. If electric buying and selling and netmetering all are going
! on, need to determine which combination should be selected. Within each group select just one set
! of electric results.  The electric results can be either the buy rate only, the buy rate plus the
! sell rate, or the netmetering rate, whichever of these three is the lowest combination.
DO mGroup = 1, groupCount
  lowestSimpleTariff = 0
  lowestPurchaseTariff = 0
  lowestSurplusSoldTariff = 0
  lowestNetMeterTariff = 0
  DO iTariff = 1, numTariff
    IF (tariff(iTariff)%isQualified) THEN
      IF (tariff(iTariff)%isSelected) THEN
        IF (groupIndex(iTariff) .EQ. mGroup) THEN
          SELECT CASE (tariff(iTariff)%kindElectricMtr)
             CASE (kindMeterElecSimple)
               lowestSimpleTariff = iTariff
             CASE (kindMeterElecProduced)
               ! don't show electric produced rates as ever selected since surplus sold is more relevant
               tariff(iTariff)%isSelected = .FALSE.
             CASE (kindMeterElecPurchased)
               lowestPurchaseTariff = iTariff
             CASE (kindMeterElecSurplusSold)
               lowestSurplusSoldTariff = iTariff
             CASE (kindMeterElecNet)
               lowestNetMeterTariff = iTariff
          END SELECT
        END IF
      END IF
    END IF
  END DO
  ! compare the simple and purchased metered tariffs
  IF ((lowestSimpleTariff .GT. 0) .AND. (lowestPurchaseTariff .GT. 0)) THEN
    IF (tariff(lowestSimpleTariff)%totalAnnualCost .LT. tariff(lowestPurchaseTariff)%totalAnnualCost) THEN
      tariff(lowestPurchaseTariff)%isSelected = .FALSE.
      lowestPurchaseTariff = 0
    ELSE
      tariff(lowestSimpleTariff)%isSelected = .FALSE.
      lowestSimpleTariff = 0
    END IF
  END IF
  ! if surplus sold is negative use it otherwise don't
  IF (lowestSurplusSoldTariff .GT. 0) THEN
    IF (tariff(lowestSurplusSoldTariff)%totalAnnualCost .GT. 0) THEN
      tariff(lowestSurplusSoldTariff)%isSelected = .FALSE.
      lowestSurplusSoldTariff = 0
    END IF
  END IF
  ! if netmetering is used compare it to simple plus surplus
  IF (((lowestNetMeterTariff .GT. 0) .AND. (lowestSurplusSoldTariff .GT. 0)) .AND. (lowestSimpleTariff .GT. 0)) THEN
    IF (tariff(lowestNetMeterTariff)%totalAnnualCost .LT.   &
        (tariff(lowestSimpleTariff)%totalAnnualCost + tariff(lowestSurplusSoldTariff)%totalAnnualCost)) THEN
      tariff(lowestSimpleTariff)%isSelected = .FALSE.
      lowestSimpleTariff = 0
      tariff(lowestSurplusSoldTariff)%isSelected = .FALSE.
      lowestSurplusSoldTariff = 0
    ELSE
      tariff(lowestNetMeterTariff)%isSelected = .FALSE.
      lowestNetMeterTariff = 0
    END IF
  END IF
  ! if netmetering is used compare it to purchased plus surplus
  IF (((lowestNetMeterTariff .GT. 0) .AND. (lowestSurplusSoldTariff .GT. 0)) .AND. (lowestPurchaseTariff .GT. 0)) THEN
    IF (tariff(lowestNetMeterTariff)%totalAnnualCost .LT.   &
        (tariff(lowestPurchaseTariff)%totalAnnualCost + tariff(lowestSurplusSoldTariff)%totalAnnualCost)) THEN
      tariff(lowestPurchaseTariff)%isSelected = .FALSE.
      lowestPurchaseTariff = 0
      tariff(lowestSurplusSoldTariff)%isSelected = .FALSE.
      lowestSurplusSoldTariff = 0
    ELSE
      tariff(lowestNetMeterTariff)%isSelected = .FALSE.
      lowestNetMeterTariff = 0
    END IF
  END IF
  ! if netmetering is used compare it to simple only
  IF ((lowestNetMeterTariff .GT. 0) .AND. (lowestSimpleTariff .GT. 0)) THEN
    IF (tariff(lowestNetMeterTariff)%totalAnnualCost .LT. tariff(lowestSimpleTariff)%totalAnnualCost) THEN
      tariff(lowestSimpleTariff)%isSelected = .FALSE.
      lowestSimpleTariff = 0
    ELSE
      tariff(lowestNetMeterTariff)%isSelected = .FALSE.
      lowestNetMeterTariff = 0
    END IF
  END IF
  ! if netmetering is used compare it to purchased only
  IF ((lowestNetMeterTariff .GT. 0) .AND. (lowestPurchaseTariff .GT. 0)) THEN
    IF (tariff(lowestNetMeterTariff)%totalAnnualCost .LT. tariff(lowestPurchaseTariff)%totalAnnualCost) THEN
      tariff(lowestPurchaseTariff)%isSelected = .FALSE.
      lowestPurchaseTariff = 0
    ELSE
      tariff(lowestNetMeterTariff)%isSelected = .FALSE.
      lowestNetMeterTariff = 0
    END IF
  END IF
END DO
DEALLOCATE(groupIndex)
DEALLOCATE(MinTariffIndex)
END SUBROUTINE selectTariff

SUBROUTINE GetMonthlyCostForResource(inResourceNumber,outMonthlyCosts)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !  Return the total annual cost for a given resource number.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)                  :: inResourceNumber
REAL(r64), INTENT(OUT),DIMENSION(12) :: outMonthlyCosts

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
INTEGER :: iTariff
INTEGER :: jMonth
INTEGER :: totalVarPt

outMonthlyCosts = 0.0d0
DO iTariff = 1, numTariff
  IF (Tariff(iTariff)%isSelected) THEN
    IF (Tariff(iTariff)%resourceNum .EQ. inResourceNumber) THEN
      totalVarPt = tariff(iTariff)%ptTotal
      DO jMonth = 1,12 !use 12 because LCC assume 12 months
        outMonthlyCosts(jMonth) = outMonthlyCosts(jMonth) + econVar(totalVarPt)%values(jMonth)
      END DO
    END IF
  END IF
END DO
END SUBROUTINE GetMonthlyCostForResource


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

END MODULE EconomicTariff

