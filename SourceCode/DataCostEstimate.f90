MODULE DataCostEstimate      ! EnergyPlus Data-Only Module
          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for Cost Estimation variables which are considered
          ! to be "global" in nature in EnergyPlus.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! OTHER NOTES:

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
TYPE                   :: CostLineItemStruct
  CHARACTER(len=MaxNameLength) :: LineName = '' ! object name (needed ?)
  CHARACTER(len=MaxNameLength) :: LineType = '' ! Case statement driver?
  CHARACTER(len=MaxNameLength) :: ParentObjType = '' ! parent reference to IDD object type
  CHARACTER(len=MaxNameLength) :: ParentObjName = '' ! parent instance in IDF
  CHARACTER(len=MaxNameLength) :: ParentObjKey  = '' ! end use key for parent object
  INTEGER :: ParentObjIDinList  = 1  !
  REAL(r64)    :: PerSquareMeter     = 0.0d0 ! cost per square meter
  REAL(r64)    :: PerEach            = 0.0d0 ! cost per each
  REAL(r64)    :: PerKiloWattCap     = 0.0d0 ! cost per kW of nominal capacity
  REAL(r64)    :: PerKWCapPerCOP     = 0.0d0 ! cost per kW of nominal capacity per COP
  REAL(r64)    :: PerCubicMeter      = 0.0d0 ! cost per cubic meter
  REAL(r64)    :: PerCubMeterPerSec  = 0.0d0 ! cost per cubic meter per second
  REAL(r64)    :: PerUAinWattperDelK = 0.0d0 ! cost per (UA) in Watt/deltaK
!  REAL(r64)    :: AnnualMaintFract   = 0.0d0 ! cost for annual service and non energy consumables
!  REAL(r64)    :: MinorOverhallFract = 0.0d0 ! cost for minor overhalls
!  INTEGER :: MinorOverhallYears = 0   ! year interval for minor overhalls
!  REAL(r64)    :: MajorOverhallFract = 0.0d0 ! cost for major overhall
!  INTEGER :: MajorOverhallYears = 0   ! year interval for major overhalls
!  INTEGER :: LifeYears          = 0.0 ! expected life in years
!  REAL(r64)    :: ValueAtReplacement = 0.0d0 ! residual value at end of life
  INTEGER :: LineNumber         = -1  ! number of line item in detail list
  REAL(r64)    :: Qty                = 0.0d0 ! quantity in calculations (can be input)
  Character(len=MaxNameLength)  :: Units = ''! Reported units
  REAL(r64)    :: ValuePer           = 0.0d0 ! Cost used in final calculation
  REAL(r64)    :: LineSubTotal       = 0.0d0 ! line item total  Qty * ValuePer
END TYPE CostLineItemStruct

TYPE  :: CostAdjustmentStruct
  REAL(r64)    :: LineItemTot        != 0.0 ! holds total from line item cost calculations
  REAL(r64)    :: MiscCostperSqMeter != 0.0 ! holds user-defined constant cost model
  REAL(r64)    :: DesignFeeFrac      != 0.0 ! holds user-defined fraction for design fees
  REAL(r64)    :: ContractorFeeFrac  != 0.0 ! holds user-defined fraction for contractor fees
  REAL(r64)    :: ContingencyFrac    != 0.0 ! holds user-defined fraction for contingencies
  REAL(r64)    :: BondCostFrac       != 0.0 ! holds user-defined fraction for bonding costs
  REAL(r64)    :: CommissioningFrac  != 0.0 ! holds user-defined fraction for commissioning costs
  REAL(r64)    :: RegionalModifier   != 1.0 ! holds user-defined multiplier to account for regional diffs
  REAL(r64)    :: GrandTotal         != 0.0 ! the Grand Total of all line items plus all other costs
END TYPE CostAdjustmentStruct

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
TYPE (CostLineItemStruct), ALLOCATABLE, DIMENSION(:) :: CostLineItem

 ! CurntBldg holds results for current bldg. cost estimate
TYPE (CostAdjustmentStruct),SAVE :: CurntBldg  =CostAdjustmentStruct  &
   (0.0d0,  &   ! holds total from line item cost calculations
    0.0d0,  &   ! holds user-defined constant cost model
    0.0d0,  &   ! holds user-defined fraction for design fees
    0.0d0,  &   ! holds user-defined fraction for contractor fees
    0.0d0,  &   ! holds user-defined fraction for contingencies
    0.0d0,  &   ! holds user-defined fraction for bonding costs
    0.0d0,  &   ! holds user-defined fraction for commissioning costs
    1.0d0,  &   ! holds user-defined multiplier to account for regional diffs
    0.0d0)      ! the Grand Total of all line items plus all other costs
 ! RefrnceBldg holds user input for comparison.
TYPE (CostAdjustmentStruct),SAVE :: RefrncBldg =CostAdjustmentStruct  &
   (0.0d0,  &   ! holds total from line item cost calculations
    0.0d0,  &   ! holds user-defined constant cost model
    0.0d0,  &   ! holds user-defined fraction for design fees
    0.0d0,  &   ! holds user-defined fraction for contractor fees
    0.0d0,  &   ! holds user-defined fraction for contingencies
    0.0d0,  &   ! holds user-defined fraction for bonding costs
    0.0d0,  &   ! holds user-defined fraction for commissioning costs
    1.0d0,  &   ! holds user-defined multiplier to account for regional diffs
    0.0d0)      ! the Grand Total of all line items plus all other costs

INTEGER  :: NumLineItems=0  ! number of cost estimate line items
LOGICAL  :: DoCostEstimate   = .FALSE. !set to true if any cost estimating needed

TYPE monetaryUnitType
  CHARACTER(len=MaxNameLength)    :: code      = '' !ISO code for currency such as USD or EUR
  CHARACTER(len=MaxNameLength)    :: txt       = '' !text representation of the currency
  CHARACTER(len=MaxNameLength)    :: html      = '' !representation for HTML file - contains unicode references
END TYPE
TYPE (monetaryUnitType), ALLOCATABLE, DIMENSION(:) :: monetaryUnit
INTEGER                           :: numMonetaryUnit=0
INTEGER                           :: selectedMonetaryUnit=0


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

END MODULE DataCostEstimate
