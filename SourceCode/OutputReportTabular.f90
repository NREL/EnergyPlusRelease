MODULE OutputReportTabular

! MODULE INFORMATION:
!    AUTHOR         Jason Glazer of GARD Analytics, Inc.
!    DATE WRITTEN   July 2003
!    MODIFIED       na
!    RE-ENGINEERED  na
!
! PURPOSE OF THIS MODULE:
!    This module allows the user to define several different tabular
!    report that have a specific format.
!
! METHODOLOGY EMPLOYED:
!    Generally aggregation. Specifically, the IDF objects are read into data
!    structures on the first call to update the data.  The data structures
!    include not only ones to hold the IDF data but also that initialize
!    the structure used to gather data each iteration. The report:table:binned
!    object is stored in OutputTableBinned.
!
!    During initialization the TableResults data structure is created which contains
!    all the information needed to perform the aggregation on a timestep basis.
!    After the end of the simulation the original Output data structures
!    are scanned and actual tables are created doing any scaling as necessary
!    and placing all the results into an output table.  The output table
!    is written in the selected format for each of the tables defined.
!
! REFERENCES:
!    None.
!
! OTHER NOTES:.
!
!                                      |--> BinResults
!                                      |
!                                      |--> BinResultsAbove
!   OutputTableBinned ---------------->|
!                                      |--> BinResultsBelow
!                                      |
!                                      |--> BinObjVarID
!
!
!                                      |--> MonthlyFieldSetInput
!   MonthlyInput --------------------->|
!                                      |--> MonthlyTable --> MonthlyColumns
!
!

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE InputProcessor
USE DataGlobals,  ONLY : MaxNameLength, BigNumber, ZoneTSReporting, HVACTSReporting, &
                         KindOfSim, ksDesignDay, ksRunPeriodDesign, ksRunPeriodWeather,   &
                         DoWeathSim, DoOutputReporting, DisplayExtraWarnings, OutputFileInits,   &
                         HourOfDay,TimeStep,SecInHour,TimeStepZone,CurrentTime, NumOfZones, &
                         OutputFileDebug
USE DataInterfaces
USE DataGlobalConstants
USE OutputReportPredefined

IMPLICIT NONE

PRIVATE

          !MODULE PARAMETER DEFINITIONS:

INTEGER, parameter  ::  MaxHeaderLength               = 50
INTEGER, parameter  ::  MaxNoteLength                 = 200

INTEGER, parameter  ::  aggTypeSumOrAvg                     = 1
INTEGER, parameter  ::  aggTypeMaximum                      = 2
INTEGER, parameter  ::  aggTypeMinimum                      = 3
INTEGER, parameter  ::  aggTypeValueWhenMaxMin              = 4
INTEGER, parameter  ::  aggTypeHoursZero                    = 5
INTEGER, parameter  ::  aggTypeHoursNonZero                 = 6
INTEGER, parameter  ::  aggTypeHoursPositive                = 7
INTEGER, parameter  ::  aggTypeHoursNonPositive             = 8
INTEGER, parameter  ::  aggTypeHoursNegative                = 9
INTEGER, parameter  ::  aggTypeHoursNonNegative             = 10
INTEGER, parameter  ::  aggTypeSumOrAverageHoursShown       = 11
INTEGER, parameter  ::  aggTypeMaximumDuringHoursShown      = 12
INTEGER, parameter  ::  aggTypeMinimumDuringHoursShown      = 13

INTEGER, parameter  ::  tableStyleComma               = 1
INTEGER, parameter  ::  tableStyleTab                 = 2
INTEGER, parameter  ::  tableStyleFixed               = 3
INTEGER, parameter  ::  tableStyleHTML                = 4
INTEGER, parameter  ::  tableStyleXML                 = 5

INTEGER, PUBLIC, parameter  ::  unitsStyleNone             = 0 !no change to any units
INTEGER, PUBLIC, parameter  ::  unitsStyleJtoKWH           = 1
INTEGER, PUBLIC, parameter  ::  unitsStyleJtoMJ            = 2
INTEGER, PUBLIC, parameter  ::  unitsStyleJtoGJ            = 3
INTEGER, PUBLIC, parameter  ::  unitsStyleInchPound        = 4

INTEGER, parameter  ::  isAverage                     = 1
INTEGER, parameter  ::  isSum                         = 2

INTEGER, parameter  ::  stepTypeZone               = ZoneTSReporting
INTEGER, parameter  ::  stepTypeHVAC               = HVACTSReporting

! BEPS Report Related Variables
! From Report:Table:Predefined - BEPS
INTEGER, parameter  ::  numResourceTypes = 14
INTEGER, parameter  ::  numSourceTypes   =  12

CHARACTER(LEN=*), parameter :: validChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_:.'

          !MODULE VARIABLE DECLARATIONS:

! The Binned table type is different and only references one variable and its structure is very
! different from the others so it is has its own type.
TYPE OutputTableBinnedType
  CHARACTER(len=MaxNameLength)   :: keyValue      =' ' ! the key value (usually an asterisk to indicate all variables
  CHARACTER(len=MaxNameLength)   :: varOrMeter    =' ' ! the name of the variable or meter
  REAL(r64)                      :: intervalStart =0.0d0 ! The lowest value for the intervals being binned into.
  REAL(r64)                      :: intervalSize  =0.0d0 ! The size of the bins starting with Interval start.
  INTEGER                        :: intervalCount =0   ! The number of bins used. The number of hours below the start of
                                                       ! the lowest bin and above the value of the last bin are also shown.
  INTEGER                        :: resIndex      =0   ! result index - pointer to BinResults array
  INTEGER                        :: numTables     =0
  INTEGER                        :: typeOfVar     =0   ! 0=not found, 1=integer, 2=real, 3=meter
  INTEGER                        :: avgSum        =0   ! Variable  is Averaged=1 or Summed=2
  INTEGER                        :: stepType      =0   ! Variable time step is Zone=1 or HVAC=2
  CHARACTER(len=MaxNameLength)   :: units         =' ' ! the units string, may be blank
  CHARACTER(len=MaxNameLength)   :: scheduleName  =' ' ! the name of the schedule
  INTEGER                        :: scheduleIndex =0   ! index to the schedule specified - if no schedule use zero
END TYPE

TYPE BinResultsType
  REAL(r64),DIMENSION(12)             :: mnth          =0.0d0 ! monthly bins
  REAL(r64),DIMENSION(24)             :: hrly          =0.0d0 ! hourly bins
END TYPE

TYPE BinObjVarIDType
  CHARACTER(len=MaxNameLength)   :: namesOfObj    =' ' ! name of the object
  INTEGER                        :: varMeterNum   =0   ! variable or meter number
END TYPE

TYPE BinStatisticsType
  REAL(r64)                      :: sum           =0.0d0 !sum of the variable
  REAL(r64)                      :: sum2          =0.0d0 !sum of the variable squared
  INTEGER                        :: n             =0   !number of items in sum
  REAL(r64)                      :: minimum       =0.0d0 !minimum value
  REAL(r64)                      :: maximum       =0.0d0 !maximum value
END TYPE

! arrays for time binned results
TYPE (OutputTableBinnedType), ALLOCATABLE, DIMENSION(:)    :: OutputTableBinned
TYPE (BinResultsType),        ALLOCATABLE, DIMENSION(:,:)  :: BinResults       ! table number, number of intervals
TYPE (BinResultsType),        ALLOCATABLE, DIMENSION(:)    :: BinResultsBelow  !time below the lowest defined bin
TYPE (BinResultsType),        ALLOCATABLE, DIMENSION(:)    :: BinResultsAbove  !time above the highest defined bin
TYPE (BinObjVarIDType),       ALLOCATABLE, DIMENSION(:)    :: BinObjVarID
TYPE (BinStatisticsType),     ALLOCATABLE, DIMENSION(:)    :: BinStatistics


INTEGER   ::    OutputTableBinnedCount
INTEGER   ::    BinResultsTableCount
INTEGER   ::    BinResultsIntervalCount

TYPE NamedMonthlyType
  CHARACTER(len=MaxNameLength)    :: title     =' '       ! report title
  LOGICAL                         :: show      =.FALSE.   ! if report should be shown
END TYPE
INTEGER, PARAMETER    :: numNamedMonthly = 62
TYPE (NamedMonthlyType), DIMENSION(:),ALLOCATABLE :: namedMonthly  !for predefined monthly report titles
! These reports are detailed/named in routine InitializePredefinedMonthlyTitles

TYPE MonthlyInputType
  CHARACTER(len=MaxNameLength * 2)   :: name     =' ' ! identifier
  INTEGER                        :: numFieldSet  =0   ! number of monthly field sets
  INTEGER                        :: firstFieldSet=0   ! pointer to the first field set
  INTEGER                        :: numTables    =0   ! number of tables
  INTEGER                        :: firstTable   =0   ! pointer to the first table
  INTEGER                        :: showDigits   =0   ! the number of digits to be shown
END TYPE

TYPE MonthlyFieldSetInputType
  CHARACTER(len=MaxNameLength)   :: variMeter = ''    ! the name of the variable or meter
  CHARACTER(len=MaxNameLength)   :: colHead   = ''    ! the column header to use instead of the variable name (only for predefined)
  INTEGER                        :: aggregate = 0     ! the type of aggregation for the variable (see aggType parameters)
  CHARACTER(len=MaxNameLength)   :: varUnits = ''     ! Units sting, may be blank
  CHARACTER(len=MaxNameLength)   :: variMeterUpper = ''    ! the name of the variable or meter uppercased
  INTEGER                        :: typeOfVar = 0     ! 0=not found, 1=integer, 2=real, 3=meter
  INTEGER                        :: keyCount=0        ! noel
  INTEGER                        :: varAvgSum=1       ! Variable  is Averaged=1 or Summed=2
  INTEGER                        :: varStepType=1     ! Variable time step is Zone=1 or HVAC=2
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: NamesOfKeys  ! keyNames !noel
  INTEGER, ALLOCATABLE, DIMENSION(:)          :: IndexesForKeyVar ! keyVarIndexes !noel
END TYPE

TYPE MonthlyTablesType
  CHARACTER(len=MaxNameLength)   :: keyValue     =' ' ! the key value - the object names that result in the variable
  INTEGER                        :: firstColumn  =0   ! pointer to the monthly column array for the first item
  INTEGER                        :: numColumns   =0   ! number of columns for the table
END TYPE

TYPE MonthlyColumnsType
  CHARACTER(len=MaxNameLength)   :: varName      =' ' ! name of variable
  CHARACTER(len=MaxNameLength)   :: colHead      =''  ! column header (not used for user defined monthly)
  INTEGER                        :: varNum       =0   ! variable or meter number
  INTEGER                        :: typeOfVar    =0   ! 0=not found, 1=integer, 2=real, 3=meter
  INTEGER                        :: avgSum       =0   ! Variable  is Averaged=1 or Summed=2
  INTEGER                        :: stepType     =0   ! Variable time step is Zone=1 or HVAC=2
  CHARACTER(len=MaxNameLength)   :: units        =' ' ! the units string, may be blank
  INTEGER                        :: aggType      =0   ! index to the type of aggregation (see list of parameters)
  REAL(r64),DIMENSION(12)        :: reslt        =0.0d0 ! monthly results
  REAL(r64),DIMENSION(12)        :: duration     =0.0d0 ! the time during which results are summed for use in averages
  INTEGER,DIMENSION(12)          :: timeStamp    =0   ! encoded timestamp of max or min
  REAL(r64)                      :: aggForStep   =0.0d0 ! holds the aggregation for the HVAC time steps when smaller than
                                                      ! the zone timestep
END TYPE

TYPE (MonthlyFieldSetInputType), ALLOCATABLE, DIMENSION(:)  :: MonthlyFieldSetInput
TYPE (MonthlyFieldSetInputType), ALLOCATABLE, DIMENSION(:)  :: MonthlyFieldSetInputCopy
TYPE (MonthlyInputType), ALLOCATABLE, DIMENSION(:)          :: MonthlyInput
TYPE (MonthlyInputType), ALLOCATABLE, DIMENSION(:)          :: MonthlyInputCopy
TYPE (MonthlyTablesType), ALLOCATABLE, DIMENSION(:)         :: MonthlyTables
TYPE (MonthlyColumnsType), ALLOCATABLE, DIMENSION(:)        :: MonthlyColumns

INTEGER   ::    MonthlyInputCount   =0
INTEGER   ::    sizeMonthlyInput    =0
INTEGER,PUBLIC   ::    MonthlyFieldSetInputCount   =0
INTEGER   ::    sizeMonthlyFieldSetInput =0
INTEGER   ::    MonthlyTablesCount  =0
INTEGER   ::    MonthlyColumnsCount =0
LOGICAL,DIMENSION(12) :: IsMonthGathered    =.FALSE. !shown as true for any month used


TYPE TOCEntriesType
  CHARACTER(len=MaxNameLength)   :: reportName    = ' '     ! the name of the individual report
  CHARACTER(len=MaxNameLength)   :: sectionName   = ' '     ! the name of the section containing individual reports
  LOGICAL                        :: isWritten     = .FALSE. !flag if the entry has been written to TOC
END TYPE

TYPE (TOCEntriesType),ALLOCATABLE, DIMENSION(:)          :: TOCEntries
TYPE (TOCEntriesType),ALLOCATABLE, DIMENSION(:)          :: CopyOfTOCEntries
INTEGER :: TOCEntriesCount = 0
INTEGER :: TOCEntriesSize  = 0

TYPE UnitConvType
  CHARACTER(len=20)   :: siName    = ' '     ! the name abbreviation or symbol of the SI units
  CHARACTER(len=20)   :: ipName    = ' '     ! the name abbreviation or symbol of the IP units
  REAL(r64)           :: mult      = 1.d0    ! the multiplier used to convert from SI to IP in IP = (SI * mult) + offset
  REAL(r64)           :: offset    = 0.d0    ! the offset used to convert from SI to IP in IP = (SI * mult) + offset
  CHARACTER(len=20)   :: hint      = ' '     ! the string used when multiple SI units match
  LOGICAL             :: several   = .FALSE. ! several different options for the SI unit to be converted into IP
  LOGICAL             :: default   = .FALSE. ! if part of a set of "several" this should be used as default
END TYPE
TYPE (UnitConvType), ALLOCATABLE, DIMENSION(:) :: UnitConv
INTEGER :: UnitConvSize

LOGICAL, PUBLIC   ::    WriteTabularFiles=.false.

! Allow up to five output files to be created
INTEGER, PARAMETER :: maxNumStyles = 5

! From Report:Table:Style
INTEGER,PUBLIC                          :: unitsStyle        = 0   ! see list of parameters
INTEGER                                 :: numStyles         = 0
INTEGER,DIMENSION(maxNumStyles)         :: TabularOutputFile = 0   ! file number holder for output file
CHARACTER(LEN=1),DIMENSION(maxNumStyles):: del               = ' ' ! the delimiter to use
INTEGER,DIMENSION(maxNumStyles)         :: TableStyle        = 0   ! see list of parameters

REAL(r64)        :: timeInYear      =0.0d0

! Flags for predefined tabular reports
LOGICAL   ::    displayTabularBEPS =.false.
LOGICAL   ::    displayLEEDSummary =.false.
LOGICAL   ::    displayTabularCompCosts =.false. !added BTG 5/6/04 for component cost summary
LOGICAL   ::    displayTabularVeriSum = .false.  !added JG 2006-06-28 for input verification and summary report
LOGICAL   ::    displayComponentSizing = .false.
LOGICAL   ::    displaySurfaceShadowing = .false.
LOGICAL   ::    displayDemandEndUse = .false.
LOGICAL   ::    displayAdaptiveComfort = .false.
LOGICAL   ::    displaySourceEnergyEndUseSummary = .false.
LOGICAL   ::    displayZoneComponentLoadSummary = .false.

! BEPS Report Related Variables
! From Report:Table:Predefined - BEPS
! arrays that hold the meter numbers that are initialized at get input
INTEGER, DIMENSION(numResourceTypes)            :: meterNumTotalsBEPS =0
INTEGER, DIMENSION(numSourceTypes)              :: meterNumTotalsSource =0
LOGICAL, DIMENSION(numSourceTypes)              :: fuelfactorsused=.false.
LOGICAL, DIMENSION(numResourceTypes)            :: ffUsed=.false.
REAL(r64), DIMENSION(numResourceTypes)          :: SourceFactors = 0.0d0
LOGICAL, DIMENSION(numResourceTypes)            :: ffSchedUsed =.false.
INTEGER, DIMENSION(numResourceTypes)            :: ffSchedIndex = 0
INTEGER, DIMENSION(numEndUses,numResourceTypes) :: meterNumEndUseBEPS =0
INTEGER, ALLOCATABLE, DIMENSION(:,:,:)          :: meterNumEndUseSubBEPS
! arrays that hold the names of the resource and end uses
CHARACTER(len=32), DIMENSION(numResourceTypes)  :: resourceTypeNames =' '
CHARACTER(len=32), DIMENSION(numSourceTypes)    :: sourceTypeNames   =' '
CHARACTER(len=32), DIMENSION(numEndUses)        :: endUseNames       =' '
! arrays that hold the actual values for the year
REAL(r64), DIMENSION(numResourceTypes)               :: gatherTotalsBEPS   =0.0d0
REAL(r64), DIMENSION(numResourceTypes)               :: gatherTotalsBySourceBEPS =0.0d0
REAL(r64), DIMENSION(numSourceTypes)                 :: gatherTotalsSource =0.0d0
REAL(r64), DIMENSION(numSourceTypes)                 :: gatherTotalsBySource =0.0d0
REAL(r64), DIMENSION(numEndUses,numResourceTypes)    :: gatherEndUseBEPS   =0.0d0
REAL(r64), DIMENSION(numEndUses,numResourceTypes)    :: gatherEndUseBySourceBEPS   =0.0d0
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)             :: gatherEndUseSubBEPS
! arrays the hold the demand values
REAL(r64), DIMENSION(numResourceTypes)               :: gatherDemandTotal  =0.0d0
REAL(r64), DIMENSION(numEndUses,numResourceTypes)    :: gatherDemandEndUse  =0.0d0
REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)             :: gatherDemandEndUseSub
INTEGER, DIMENSION(numResourceTypes)                 :: gatherDemandTimeStamp = 0
! to keep track of hours for the BEPS report gathering
REAL(r64)                           :: gatherElapsedTimeBEPS =0.0d0
! for normalization of results
REAL(r64)  , PUBLIC                             :: buildingGrossFloorArea       =0.0d0
REAL(r64)  , PUBLIC                             :: buildingConditionedFloorArea =0.0d0
! keep track if schedules are used in fuel factors
LOGICAL  ::  fuelFactorSchedulesUsed = .FALSE.
! for electic load components on BEPS report
INTEGER                                         :: meterNumPowerFuelFireGen       =0
REAL(r64)                                       :: gatherPowerFuelFireGen         =0.0d0
INTEGER                                         :: meterNumPowerPV                =0
REAL(r64)                                       :: gatherPowerPV                  =0.0d0
INTEGER                                         :: meterNumPowerWind              =0
REAL(r64)                                       :: gatherPowerWind                =0.0d0
REAL(r64)                                       :: OverallNetEnergyFromStorage    =0.0d0
INTEGER                                         :: meterNumPowerHTGeothermal      =0
REAL(r64)                                       :: gatherPowerHTGeothermal        =0.0d0
INTEGER                                         :: meterNumElecProduced           =0
REAL(r64)                                       :: gatherElecProduced             =0.0d0
INTEGER                                         :: meterNumElecPurchased          =0
REAL(r64)                                       :: gatherElecPurchased            =0.0d0
INTEGER                                         :: meterNumElecSurplusSold        =0
REAL(r64)                                       :: gatherElecSurplusSold          =0.0d0
! for on site thermal source components on BEPS report
INTEGER                                         :: meterNumWaterHeatRecovery      =0
REAL(r64)                                       :: gatherWaterHeatRecovery        =0.0d0
INTEGER                                         :: meterNumAirHeatRecoveryCool    =0
REAL(r64)                                       :: gatherAirHeatRecoveryCool      =0.0d0
INTEGER                                         :: meterNumAirHeatRecoveryHeat    =0
REAL(r64)                                       :: gatherAirHeatRecoveryHeat      =0.0d0
INTEGER                                         :: meterNumHeatHTGeothermal       =0
REAL(r64)                                       :: gatherHeatHTGeothermal         =0.0d0
INTEGER                                         :: meterNumHeatSolarWater         =0
REAL(r64)                                       :: gatherHeatSolarWater           =0.0d0
INTEGER                                         :: meterNumHeatSolarAir           =0
REAL(r64)                                       :: gatherHeatSolarAir             =0.0d0
! for on site water components on BEPS report
INTEGER                                         :: meterNumRainWater       =0
REAL(r64)                                       :: gatherRainWater         =0.0d0
INTEGER                                         :: meterNumCondensate      =0
REAL(r64)                                       :: gatherCondensate        =0.0d0
INTEGER                                         :: meterNumGroundwater      =0
REAL(r64)                                       :: gatherWellwater        =0.0d0
INTEGER                                         :: meterNumMains         =0
REAL(r64)                                       :: gatherMains           =0.0d0
INTEGER                                         :: meterNumWaterEndUseTotal     =0
REAL(r64)                                       :: gatherWaterEndUseTotal       =0.0d0
! for source energy conversion factors on BEPS report
REAL(r64)                                       :: sourceFactorElectric           =0.0d0
REAL(r64)                                       :: sourceFactorNaturalGas         =0.0d0
REAL(r64)                                       :: efficiencyDistrictCooling     =0.0d0
REAL(r64)                                       :: efficiencyDistrictHeating     =0.0d0
REAL(r64)                                       :: sourceFactorSteam              =0.0d0
REAL(r64)                                       :: sourceFactorGasoline           =0.0d0
REAL(r64)                                       :: sourceFactorDiesel             =0.0d0
REAL(r64)                                       :: sourceFactorCoal               =0.0d0
REAL(r64)                                       :: sourceFactorFuelOil1           =0.0d0
REAL(r64)                                       :: sourceFactorFuelOil2           =0.0d0
REAL(r64)                                       :: sourceFactorPropane            =0.0d0
REAL(r64)                                       :: sourceFactorOtherFuel1         =0.0d0
REAL(r64)                                       :: sourceFactorOtherFuel2         =0.0d0

INTEGER,DIMENSION(8)        :: td
!(1)   Current year
!(2)   Current month
!(3)   Current day
!(4)   Time difference with respect to UTC in minutes (0-59)
!(5)   Hour of the day (0-23)
!(6)   Minutes (0-59)
!(7)   Seconds (0-59)
!(8)   Milliseconds (0-999)


! Design day name storage
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)  :: DesignDayName
INTEGER                                                 :: DesignDayCount=0


!arrays related to pulse and load component reporting
REAL(r64), DIMENSION(:,:),ALLOCATABLE,PUBLIC   :: radiantPulseUsed
INTEGER, DIMENSION(:,:),ALLOCATABLE,PUBLIC   :: radiantPulseTimestep
REAL(r64), DIMENSION(:,:),ALLOCATABLE,PUBLIC   :: radiantPulseReceived
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC   :: loadConvectedNormal
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC   :: loadConvectedWithPulse
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC   :: netSurfRadSeq
REAL(r64), DIMENSION(:,:),ALLOCATABLE,PUBLIC   :: decayCurveCool
REAL(r64), DIMENSION(:,:),ALLOCATABLE,PUBLIC   :: decayCurveHeat
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC   :: ITABSFseq  !used for determining the radiant fraction on each surface
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC   :: TMULTseq   !used for determining the radiant fraction on each surface

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: peopleInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: peopleLatentSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: peopleRadSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE         :: peopleDelaySeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: lightInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: lightRetAirSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: lightLWRadSeq        ! long wave thermal radiation
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: lightSWRadSeq        ! short wave visible radiation
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE         :: lightDelaySeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: equipInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: equipLatentSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: equipRadSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE         :: equipDelaySeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: refrigInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: refrigRetAirSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: refrigLatentSeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: waterUseInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: waterUseLatentSeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: hvacLossInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: hvacLossRadSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE         :: hvacLossDelaySeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: powerGenInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: powerGenRadSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE         :: powerGenDelaySeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: infilInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: infilLatentSeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: zoneVentInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: zoneVentLatentSeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: interZoneMixInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: interZoneMixLatentSeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneCondInstantSeq
!REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneSolarInstantSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneSolarRadSeq
REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: feneSolarDelaySeq

REAL(r64), DIMENSION(:,:,:),ALLOCATABLE,PUBLIC  :: surfDelaySeq

INTEGER,PUBLIC :: maxUniqueKeyCount=0

! for the XML report must keep track fo the active sub-table name and report set by other routines
CHARACTER(len=MaxNameLength) :: activeSubTableName = ''
CHARACTER(len=MaxNameLength) :: activeReportNameNoSpace = ''
CHARACTER(len=MaxNameLength) :: activeReportName = ''
CHARACTER(len=MaxNameLength) :: activeForName = ''
CHARACTER(len=MaxNameLength) :: prevReportName = ''


! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
 PUBLIC       UpdateTabularReports
 PUBLIC       isCompLoadRepReq
 PUBLIC       AllocateLoadComponentArrays
 PUBLIC       DeallocateLoadComponentArrays
 PUBLIC       ComputeLoadComponentDecayCurve
 PRIVATE      GetInputTabularMonthly
 PRIVATE      GetInputTabularTimeBins
 PRIVATE      GetInputTabularStyle
 PRIVATE      GetInputTabularPredefined
 PRIVATE      GetInputFuelAndPollutionFactors
 PRIVATE      GatherBinResultsForTimestep
 PRIVATE      GatherMonthlyResultsForTimestep
 PRIVATE      GatherBEPSResultsForTimestep
 PRIVATE      GatherHeatGainReport
 PRIVATE      GatherSourceEnergyEndUseResultsForTimestep
 PUBLIC       GatherComponentLoadsHVAC
 PUBLIC       GatherComponentLoadsSurface
 PUBLIC       WriteTabularReports
 PRIVATE      WriteMonthlyTables
 PRIVATE      WriteTimeBinTables
 PRIVATE      WritePredefinedTables
 PRIVATE      WriteVeriSumTable
 PRIVATE      WriteBEPSTable
 PRIVATE      WriteSourceEnergyEndUseSummary
 PRIVATE      WriteAdaptiveComfortTable
 PRIVATE      WriteDemandEndUseSummary
 PRIVATE      WriteCompCostTable
 PRIVATE      WriteComponentSizing
 PRIVATE      WriteSurfaceShadowing
 PUBLIC       WriteReportHeaders
 PUBLIC       writeSubtitle
 PUBLIC       WriteTable
 PUBLIC       writeTextLine
 PRIVATE      WriteTableOfContents
 PRIVATE      WriteZoneLoadComponentTable
 PUBLIC      DetermineBuildingFloorArea
 PUBLIC       RealToStr
 PUBLIC       IntToStr
 PUBLIC     OpenOutputTabularFile
 PUBLIC     CloseOutputTabularFile
 PUBLIC     isInQuadrilateral
 PUBLIC     AddTOCEntry
 PRIVATE    FillWeatherPredefinedEntries
 PRIVATE    FillRemainingPredefinedEntries
 PRIVATE    SetupUnitConversions
 PUBLIC     LookupSItoIP
 PUBLIC     ConvertIP
 PUBLIC     GetUnitConversion
 PUBLIC     StrToReal
 PUBLIC     GetColumnUsingTabs
 PUBLIC     DateToString
!PRIVATE      DateToStr

CONTAINS

SUBROUTINE     UpdateTabularReports(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the routine that is called at the end of the time step
          ! loop and updates the arrays of data that will later being put
          ! into the tabular reports.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL, SAVE     :: GetInput = .TRUE.

IF (IndexTypeKey /= ZoneTSReporting .and. IndexTypeKey /= HVACTSReporting) THEN
  CALL ShowFatalError('Invalid reporting requested -- UpdateTabularReports')
ENDIF

IF (GetInput) THEN
  CALL GetInputTabularMonthly
  CALL GetInputTabularTimeBins
  CALL GetInputTabularStyle
  CALL GetInputTabularPredefined
! noel -- noticed this was called once and very slow -- sped up a little by caching keys
  CALL InitializeTabularMonthly
  CALL GetInputFuelAndPollutionFactors
  CALL SetupUnitConversions
  CALL AddTOCZoneLoadComponentTable
  GetInput = .FALSE.
  CALL DATE_AND_TIME(values=td)
END IF
IF (DoOutputReporting .and. WriteTabularFiles .and. (KindOfSim == ksRunPeriodWeather)) THEN
  IF (IndexTypeKey .EQ. stepTypeZone) THEN
    gatherElapsedTimeBEPS = gatherElapsedTimeBEPS + TimeStepZone
  END IF
  CALL GatherMonthlyResultsForTimestep(IndexTypeKey)
  CALL GatherBinResultsForTimestep(IndexTypeKey)
  CALL GatherBEPSResultsForTimestep(IndexTypeKey)
  CALL GatherSourceEnergyEndUseResultsForTimestep(IndexTypeKey)
  CALL GatherPeakDemandForTimestep(IndexTypeKey)
  CALL GatherHeatGainReport(IndexTypeKey)
END IF
END SUBROUTINE UpdateTabularReports


!======================================================================================================================
!======================================================================================================================
!
!
!    GET INPUT ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE  GetInputTabularMonthly
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   The routine assigns the input information for
          !   REPORT:TABLE:MONTHLY also known as tabular monthly
          !   reports that are defined by the user. The input
          !   information is assigned to a data structure that
          !   is used for both user defined monthly reports and
          !   predefined monthly reports.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure and call to build up
          !   data on monthly reports.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='Output:Table:Monthly'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER          :: TabNum     ! index when cycling through each table
INTEGER          :: curTable   ! index of the current table being processed in MonthlyInput
INTEGER          :: curAggType ! kind of aggregation identified (see AggType parameters)
CHARACTER(len=MaxNameLength) :: curAggString     ! Current aggregation sting
INTEGER          :: jField
INTEGER          :: NumParams  ! Number of elements combined
INTEGER          :: NumAlphas  ! Number of elements in the alpha array
INTEGER          :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE  :: AlphArray !character string data
REAL(r64), DIMENSION(:), ALLOCATABLE  :: NumArray  !numeric data
INTEGER          :: IOStat     ! IO Status when calling get input subroutine
LOGICAL, SAVE    :: ErrorsFound=.false.
LOGICAL          :: IsNotOK               ! Flag to verify name
LOGICAL          :: IsBlank               ! Flag for blank name

MonthlyInputCount = GetNumObjectsFound(CurrentModuleObject)
IF (MonthlyInputCount > 0) THEN
  WriteTabularFiles=.true.
  ! if not a run period using weather do not create reports
  IF (.NOT. DoWeathSim) THEN
    CALL ShowWarningError(CurrentModuleObject // &
       ' requested with SimulationControl Run Simulation for Weather File Run Periods set to No so ' &
        // CurrentModuleObject // ' will not be generated')
    RETURN
  END IF
END IF
CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
ALLOCATE(AlphArray(NumAlphas))
AlphArray = ''
ALLOCATE(NumArray(NumNums))
NumArray=0.0d0
DO TabNum = 1 , MonthlyInputCount
  CALL GetObjectItem(CurrentModuleObject,TabNum,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT)
  IsNotOK=.false.
  IsBlank=.false.
  IF (TabNum-1 > 0) THEN
    CALL VerifyName(AlphArray(1),MonthlyInput%name,TabNum-1,IsNotOK,IsBlank,CurrentModuleObject//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='RTMBLANK'
    ENDIF
  ENDIF
  IF (NumAlphas .LT. 2) THEN
    CALL ShowSevereError(CurrentModuleObject//': No fields specified.')
  END IF
  ! add to the data structure
  curTable = AddMonthlyReport(AlphArray(1),INT(NumArray(1)))
  DO jField = 2,numAlphas,2
     curAggString = AlphArray(jField + 1)
     ! set accumulator values to default as appropriate for aggregation type
      IF (SameString(curAggString,'SumOrAverage')) THEN
          curAggType = aggTypeSumOrAvg
      ELSE IF (SameString(curAggString,'Maximum')) THEN
          curAggType = aggTypeMaximum
      ELSE IF (SameString(curAggString,'Minimum')) THEN
          curAggType = aggTypeMinimum
      ELSE IF (SameString(curAggString,'ValueWhenMaximumOrMinimum')) THEN
          curAggType = aggTypeValueWhenMaxMin
      ELSE IF (SameString(curAggString,'HoursZero')) THEN
          curAggType = aggTypeHoursZero
      ELSE IF (SameString(curAggString,'HoursNonzero')) THEN
          curAggType = aggTypeHoursNonZero
      ELSE IF (SameString(curAggString,'HoursPositive')) THEN
          curAggType = aggTypeHoursPositive
      ELSE IF (SameString(curAggString,'HoursNonpositive')) THEN
          curAggType = aggTypeHoursNonPositive
      ELSE IF (SameString(curAggString,'HoursNegative')) THEN
          curAggType = aggTypeHoursNegative
      ELSE IF (SameString(curAggString,'HoursNonnegative')) THEN
          curAggType = aggTypeHoursNonNegative
      ELSE IF (SameString(curAggString,'SumOrAverageDuringHoursShown')) THEN
          curAggType = aggTypeSumOrAverageHoursShown
      ELSE IF (SameString(curAggString,'MaximumDuringHoursShown')) THEN
          curAggType = aggTypeMaximumDuringHoursShown
      ELSE IF (SameString(curAggString,'MinimumDuringHoursShown')) THEN
          curAggType = aggTypeMinimumDuringHoursShown
      ELSE
          curAggType = aggTypeSumOrAvg
          CALL ShowWarningError(CurrentModuleObject//'='//TRIM(MonthlyInput(TabNum)%name)//  &
                ', Variable name='//TRIM(AlphArray(jField)))
          CALL ShowContinueError('Invalid aggregation type="'//TRIM(curAggString)//'"  Defaulting to SumOrAverage.')
      END IF
    CALL AddMonthlyFieldSetInput(curTable,AlphArray(jField),'',curAggType)
  END DO
END DO
DEALLOCATE(AlphArray)
DEALLOCATE(NumArray)

END SUBROUTINE GetInputTabularMonthly

INTEGER FUNCTION AddMonthlyReport(inReportName,inNumDigitsShown)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2008
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates a monthly report

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*),INTENT(IN)  :: inReportName
INTEGER, INTENT(IN)          :: inNumDigitsShown

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER, PARAMETER :: SizeAdder = 25
IF (.NOT. ALLOCATED(MonthlyInput)) THEN
  ALLOCATE(MonthlyInput(SizeAdder))
  sizeMonthlyInput = SizeAdder
  MonthlyInputCount = 1
ELSE
  MonthlyInputCount = MonthlyInputCount + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (MonthlyInputCount .GT. sizeMonthlyInput) THEN
    ALLOCATE(MonthlyInputCopy(sizeMonthlyInput))
    MonthlyInputCopy = MonthlyInput
    DEALLOCATE(MonthlyInput)
    ALLOCATE(MonthlyInput(sizeMonthlyInput + SizeAdder))
    MonthlyInput(1:sizeMonthlyInput) = MonthlyInputCopy
    DEALLOCATE(MonthlyInputCopy)
    sizeMonthlyInput = sizeMonthlyInput + SizeAdder
  END IF
END IF
! initialize new record
MonthlyInput(MonthlyInputCount)%name = inReportName
MonthlyInput(MonthlyInputCount)%showDigits = inNumDigitsShown
AddMonthlyReport = MonthlyInputCount
END FUNCTION AddMonthlyReport

SUBROUTINE AddMonthlyFieldSetInput(inMonthReport, inVariMeter, inColHead, inAggregate)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2008
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Assigns the column information for predefined
          !   monthly reports

          ! METHODOLOGY EMPLOYED:
          !   Simple assignments to public variables.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)           :: inMonthReport
CHARACTER(len=*),INTENT(IN)  :: inVariMeter
CHARACTER(len=*),INTENT(IN)  :: inColHead
INTEGER,INTENT(IN)           :: inAggregate

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: sizeIncrement = 50

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

IF (.NOT. ALLOCATED(MonthlyFieldSetInput)) THEN
  ALLOCATE(MonthlyFieldSetInput(sizeIncrement))
  sizeMonthlyFieldSetInput = sizeIncrement
  MonthlyFieldSetInputCount = 1
ELSE
  MonthlyFieldSetInputCount = MonthlyFieldSetInputCount + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (MonthlyFieldSetInputCount .GT. sizeMonthlyFieldSetInput) THEN
    ALLOCATE(MonthlyFieldSetInputCopy(sizeMonthlyFieldSetInput))
    MonthlyFieldSetInputCopy = MonthlyFieldSetInput
    DEALLOCATE(MonthlyFieldSetInput)
    ALLOCATE(MonthlyFieldSetInput(sizeMonthlyFieldSetInput + sizeIncrement))
    MonthlyFieldSetInput(1:sizeMonthlyFieldSetInput) = MonthlyFieldSetInputCopy
    DEALLOCATE(MonthlyFieldSetInputCopy)
    sizeMonthlyFieldSetInput = sizeMonthlyFieldSetInput + sizeIncrement
  END IF
END IF
! initialize new record)
MonthlyFieldSetInput(MonthlyFieldSetInputCount)%variMeter = inVariMeter
MonthlyFieldSetInput(MonthlyFieldSetInputCount)%colHead = inColHead
MonthlyFieldSetInput(MonthlyFieldSetInputCount)%aggregate = inAggregate
!update the references from the MonthlyInput array
IF ((inMonthReport .GT. 0) .AND. (inMonthReport .LE. MonthlyInputCount)) THEN
  IF (MonthlyInput(inMonthReport)%firstFieldSet .EQ. 0) THEN
    MonthlyInput(inMonthReport)%firstFieldSet = MonthlyFieldSetInputCount
    MonthlyInput(inMonthReport)%numFieldSet = 1
  ELSE
    MonthlyInput(inMonthReport)%numFieldSet = MonthlyInput(inMonthReport)%numFieldSet + 1
  END IF
END IF
END SUBROUTINE AddMonthlyFieldSetInput

SUBROUTINE  InitializeTabularMonthly
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine initializes the data structures based
          !   on input from either the IDF file or from the predefined
          !   monthly reports.  The data structures follow the IDD
          !   closely.  The routine initializes many of the arrays
          !   for monthly tables.

          ! METHODOLOGY EMPLOYED:
          !   Process the data structures that define monthly tabular
          !   reports

          ! NOTE:
          !   The bulk of this routine used to be part of the the
          !   GetInputTabularMonthly routine but when predefined
          !   monthly reports were added this routine was seperated
          !   from input.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER          :: TabNum     ! index when cycling through each table
INTEGER          :: NumColumns     !number of columns specified in the input for an object
INTEGER          :: FirstColumn    !the first column of the monthly input
CHARACTER(len=MaxNameLength) :: curVariMeter     ! current variable or meter
INTEGER          :: colNum         !loop index for columns
INTEGER          :: KeyCount
INTEGER          :: TypeVar
INTEGER          :: AvgSumVar
INTEGER          :: StepTypeVar
CHARACTER(len=MaxNameLength) :: UnitsVar     ! Units sting, may be blank
!CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NamesOfKeys      ! Specific key name
!INTEGER, DIMENSION(:) , ALLOCATABLE                     :: IndexesForKeyVar ! Array index
CHARACTER(len=MaxNameLength),  DIMENSION(:), ALLOCATABLE :: UniqueKeyNames
CHARACTER(len=MaxNameLength),  DIMENSION(:), ALLOCATABLE :: tempUniqueKeyNames
INTEGER          :: UniqueKeyCount
INTEGER          :: iKey
INTEGER          :: jUnique
INTEGER          :: found
INTEGER          :: kUniqueKey
INTEGER          :: lTable
INTEGER          :: mColumn
INTEGER          :: ColumnsRecount
INTEGER          :: TablesRecount
REAL(r64)        :: bigNum=0.0d0
LOGICAL          :: environmentKeyFound
LOGICAL, SAVE :: VarWarning=.true.
INTEGER, SAVE :: ErrCount1=0
INTEGER, SAVE :: ErrCount2=0
!INTEGER       :: maxKeyCount


! if not a running a weather simulation do not create reports
IF (.NOT. DoWeathSim) RETURN
maxUniqueKeyCount=1500
ALLOCATE(UniqueKeyNames(maxUniqueKeyCount))
! First pass through the input objects is to put the name of the report
! into the array and count the number of unique keys found to allocate
! the monthlyTables and monthlyColumns
! This approach seems inefficient but I know of no other way to size
! the arrays prior to filling them and to size the arrays basically
! the same steps must be gone through as with filling the arrays.

!#ifdef ITM_KEYCACHE
! Noel comment:  How about allocating these variables once for the whole routine?
!    Again, if a max value for key count can be agreed upon, we could use it here --
!    otherwise, will have to have re-allocate logic.
!maxKeyCount=1500 ! ?
!ALLOCATE(NamesOfKeys(maxKeyCount))
!ALLOCATE(IndexesForKeyVar(maxKeyCount))
!#endif

MonthlyColumnsCount = 0
MonthlyTablesCount = 0
DO TabNum = 1 , MonthlyInputCount
  ! the number of columns based on number of alpha fields
  NumColumns = MonthlyInput(TabNum)%numFieldSet
  FirstColumn = MonthlyInput(TabNum)%firstFieldSet
  environmentKeyFound = .FALSE.
  UniqueKeyCount = 0
  DO ColNum = 1, NumColumns

!#ifdef ITM_KEYCACHE
    ! Noel comment:  First time in this TabNum/ColNum loop, let's save the results
    !  of GetVariableKeyCountandType & GetVariableKeys.
    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
    ! call the key count function but only need count during this pass
    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
!    IF (KeyCount > maxKeyCount) THEN
!      DEALLOCATE(NamesOfKeys)
!      DEALLOCATE(IndexesForKeyVar)
!      maxKeyCount=KeyCount
!      ALLOCATE(NamesOfKeys(maxKeyCount))
!      ALLOCATE(IndexesForKeyVar(maxKeyCount))
!    ENDIF
    ALLOCATE(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(KeyCount))
    ALLOCATE(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(KeyCount))

    ! fill keys?
    CALL GetVariableKeys(curVariMeter,TypeVar,  &
       MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys,MonthlyFieldSetInput(FirstColumn+ColNum-1)%IndexesForKeyVar)

    ! save these values to use later -- noel
    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeterUpper = curVariMeter
    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%typeOfVar      = TypeVar
    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%keyCount       = KeyCount
    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varAvgSum      = AvgSumVar
    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varStepType    = StepTypeVar
    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varUnits       = UnitsVar
!    DO iKey = 1, KeyCount
!      MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(iKey) = NamesOfKeys(iKey)  !noel
!      MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(iKey) = IndexesForKeyVar(iKey)  !noel
!    ENDDO
!#else
!    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
!    ! call the key count function but only need count during this pass
!    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
!    ALLOCATE(NamesOfKeys(KeyCount))
!    ALLOCATE(IndexesForKeyVar(KeyCount))
!    CALL GetVariableKeys(curVariMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
!#endif

    DO iKey = 1, KeyCount
      found = 0
      ! set a flag if environment variables are found
      IF (sameString(MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey),"ENVIRONMENT")) THEN
        environmentKeyFound = .TRUE.
        found = -1 !so not counted in list of unique keys
      END IF
      DO jUnique = 1, UniqueKeyCount
        IF (sameString(UniqueKeyNames(jUnique),MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey))) THEN
          found = jUnique
          EXIT
        END IF
      END DO
      IF (found .EQ. 0) THEN
        UniqueKeyCount = UniqueKeyCount + 1
        IF (UniqueKeyCount > maxUniqueKeyCount) THEN
          ALLOCATE(tempUniqueKeyNames(maxUniqueKeyCount))
          tempUniqueKeyNames=UniqueKeyNames
          DEALLOCATE(UniqueKeyNames)
          ALLOCATE(UniqueKeyNames(maxUniqueKeyCount+500))
          UniqueKeyNames(1:maxUniqueKeyCount)=tempUniqueKeyNames
          UniqueKeyNames(maxUniqueKeyCount+1:maxUniqueKeyCount+500)=' '
          DEALLOCATE(tempUniqueKeyNames)
          maxUniqueKeyCount=maxUniqueKeyCount+500
        ENDIF
        UniqueKeyNames(UniqueKeyCount) = MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey)
      END IF
    END DO
!#ifdef ITM_KEYCACHE
!    ! Don't deallocate here, only allocating/deallocating once for the whole routine
!#else
!    DEALLOCATE(NamesOfKeys)
!    DEALLOCATE(IndexesForKeyVar)
!#endif
  END DO !colNum
  ! fix for CR8285 - when monthly report is only environmental variables
  IF (environmentKeyFound .AND. UniqueKeyCount .EQ. 0) THEN
    UniqueKeyCount = 1
  END IF
  ! increment the number of tables based on the number of unique keys
  MonthlyTablesCount = MonthlyTablesCount + UniqueKeyCount
  MonthlyColumnsCount = MonthlyColumnsCount + UniqueKeyCount * NumColumns
END DO !TabNum the end of the loop through the inputs objects
! Now that we have the maximum size of the number of tables (each table is
! repeated for the number of keys found) and the number of total columns
! of all of the tables, allocate the arrays to store this information.
ALLOCATE (MonthlyTables(MonthlyTablesCount))
ALLOCATE (MonthlyColumns(MonthlyColumnsCount))
          ! Initialize tables and results
MonthlyTables%keyValue    = ' '
MonthlyTables%firstColumn = 0
MonthlyTables%numColumns  = 0

MonthlyColumns%varName   = ' '
MonthlyColumns%varNum    = 0
MonthlyColumns%typeOfVar = 0
MonthlyColumns%avgSum    = 0
MonthlyColumns%stepType  = 0
MonthlyColumns%units     = ' '
MonthlyColumns%aggType   = 0
DO ColNum = 1, MonthlyColumnsCount
  MonthlyColumns(ColNum)%reslt     = 0.0d0
  MonthlyColumns(ColNum)%timeStamp = 0
  MonthlyColumns(ColNum)%duration  = 0.0d0
END DO

ColumnsRecount = 0
TablesRecount = 0
DO TabNum = 1 , MonthlyInputCount
  ! the number of columns based on number of alpha fields
  NumColumns = MonthlyInput(TabNum)%numFieldSet
  FirstColumn = MonthlyInput(TabNum)%firstFieldSet
  UniqueKeyCount = 0
  environmentKeyFound = .FALSE.
  DO ColNum = 1, NumColumns
!#ifdef ITM_KEYCACHE
    ! Noel comment:  Here is where we could use the saved values
    curVariMeter = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeterUpper
    KeyCount =    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%keyCount
    TypeVar =     MonthlyFieldSetInput(FirstColumn + ColNum - 1)%typeOfVar
    AvgSumVar =   MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varAvgSum
    StepTypeVar = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varStepType
    UnitsVar =    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varUnits
!    DO iKey = 1, KeyCount  !noel
!       NamesOfKeys(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(iKey)  !noel
!       IndexesForKeyVar(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(iKey) !noel
!    ENDDO
!#else
!    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
!    ! call the key count function but only need count during this pass
!    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
!    ALLOCATE(NamesOfKeys(KeyCount))
!    ALLOCATE(IndexesForKeyVar(KeyCount))
!    CALL GetVariableKeys(curVariMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
!#endif

    IF (KeyCount == 0) THEN
      ErrCount1=ErrCount1+1
      IF (ErrCount1 == 1 .and. .not. DisplayExtraWarnings .and. .not. VarWarning .and. KindOfSim == ksRunPeriodWeather) THEN
        CALL ShowWarningError('Processing Monthly Tabular Reports: Variable names not valid for this simulation')
        CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual variables.')
      ENDIF
      !fixing CR5878 removed the showing of the warning once about a specific variable.
      IF (DisplayExtraWarnings .and. KindOfSim == ksRunPeriodWeather) THEN
        CALL ShowWarningError('Processing Monthly Tabular Reports: '//TRIM(MonthlyInput(TabNum)%name))
        CALL ShowContinueError('..Variable name='//TRIM(curVariMeter)//' not valid for this simulation.')
        IF (VarWarning) THEN
          CALL ShowContinueError('..Variables not valid for this simulation will have "[Invalid/Undefined]"'//  &
                 ' in the Units Column of the Table Report.')
          VarWarning=.false.
        ENDIF
      ENDIF
    ENDIF
    DO iKey = 1, KeyCount
      found = 0
      ! set a flag if environment variables are found
      IF (sameString(MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey),"ENVIRONMENT")) THEN
        environmentKeyFound = .TRUE.
        found = -1 !so not counted in list of unique keys
      END IF
      DO jUnique = 1, UniqueKeyCount
        IF (sameString(UniqueKeyNames(jUnique),MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey))) THEN
          found = jUnique
          EXIT
        END IF
      END DO
      IF (found .EQ. 0) THEN
        UniqueKeyCount = UniqueKeyCount + 1
        UniqueKeyNames(UniqueKeyCount) = MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey)
      END IF
    END DO
!#ifdef ITM_KEYCACHE
!    ! Don't deallocate here, only allocating/deallocating once for the whole routine
!#else
!    DEALLOCATE(NamesOfKeys)
!    DEALLOCATE(IndexesForKeyVar)
!#endif
  END DO
  ! fix for CR8285 - when monthly report is only environmental variables
  IF (environmentKeyFound .AND. UniqueKeyCount .EQ. 0) THEN
    UniqueKeyCount = 1
  END IF
  ! increment the number of tables based on the number of unique keys
  MonthlyInput(TabNum)%firstTable = TablesRecount + 1
  MonthlyInput(TabNum)%numTables = UniqueKeyCount
  TablesRecount = TablesRecount + UniqueKeyCount
  ! loop through the different unique keys since each user defined table
  ! has that many instances - one for each unique key.
  ! It is unusual that this loop is about 'keys' and an inner loop is also
  ! about 'keys' but for this loop the keys are really instances of tables.
  DO kUniqueKey = 1, UniqueKeyCount
    lTable = kUniqueKey + MonthlyInput(TabNum)%firstTable - 1
    !use the term 'environment' for identifying the report if
    IF (environmentKeyFound .AND. UniqueKeyCount .EQ. 1) THEN
      MonthlyTables(lTable)%keyValue = 'Environment'
    ELSE  !this is the most common case is to use the unique key for the report
      MonthlyTables(lTable)%keyValue = UniqueKeyNames(kUniqueKey)
    END IF
    MonthlyTables(lTable)%firstColumn = ColumnsRecount + 1
    MonthlyTables(lTable)%numColumns = NumColumns
    ColumnsRecount = ColumnsRecount + NumColumns
    FirstColumn = MonthlyInput(TabNum)%firstFieldSet
    DO ColNum = 1, NumColumns
      environmentKeyFound = .FALSE.
      mColumn = ColNum + MonthlyTables(lTable)%firstColumn - 1
      ! when going through the columns this time, not all columns may have
      ! a EP variable that corresponds to it.  In no variable is found
      ! then set it to 0 to be skipped during data gathering

!#ifdef ITM_KEYCACHE
    ! Noel comment:  Here is where we could use the saved values
    curVariMeter = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeterUpper
    KeyCount =    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%keyCount
    TypeVar =     MonthlyFieldSetInput(FirstColumn + ColNum - 1)%typeOfVar
    AvgSumVar =   MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varAvgSum
    StepTypeVar = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varStepType
    UnitsVar =    MonthlyFieldSetInput(FirstColumn + ColNum - 1)%varUnits
!    DO iKey = 1, KeyCount  !noel
!       NamesOfKeys(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%NamesOfKeys(iKey)  !noel
!       IndexesForKeyVar(iKey) = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%IndexesForKeyVar(iKey) !noel
!    ENDDO
!#else
!    curVariMeter = MakeUPPERCase(MonthlyFieldSetInput(FirstColumn + ColNum - 1)%variMeter)
!    ! call the key count function but only need count during this pass
!    CALL GetVariableKeyCountandType(curVariMeter,KeyCount,TypeVar,AvgSumVar,StepTypeVar,UnitsVar)
!    ALLOCATE(NamesOfKeys(KeyCount))
!    ALLOCATE(IndexesForKeyVar(KeyCount))
!    CALL GetVariableKeys(curVariMeter,TypeVar,NamesOfKeys,IndexesForKeyVar)
!#endif

      IF (KeyCount .EQ. 1) THEN  ! first test if KeyCount is one to avoid referencing a zero element array
        IF (sameString(MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(1),"ENVIRONMENT")) THEN
          environmentKeyFound = .TRUE.
        END IF
      END IF
      ! if this is an environment variable - don't bother searching
      IF (environmentKeyFound) THEN
        found = 1  !only one instance of environment variables so use it.
      ELSE
        ! search through the keys for the currently active key "UniqueKeyNames(kUniqueKey)"
        found = 0
        DO iKey = 1, KeyCount
          IF (sameString(MonthlyFieldSetInput(FirstColumn+ColNum-1)%NamesOfKeys(iKey), UniqueKeyNames(kUniqueKey))) THEN
            found = iKey
            EXIT
          ENDIF
        END DO
      END IF
      IF ((found .GT. 0) .AND. (KeyCount .GE. 1)) THEN
        MonthlyColumns(mColumn)%varName = curVariMeter
        MonthlyColumns(mColumn)%varNum  = MonthlyFieldSetInput(FirstColumn+ColNum-1)%IndexesForKeyVar(found)
        MonthlyColumns(mColumn)%typeOfVar = TypeVar
        MonthlyColumns(mColumn)%avgSum = AvgSumVar
        MonthlyColumns(mColumn)%stepType = StepTypeVar
        MonthlyColumns(mColumn)%units = UnitsVar
        MonthlyColumns(mColumn)%aggType = MonthlyFieldSetInput(FirstColumn + ColNum - 1)%aggregate
       ! set accumulator values to default as appropriate for aggregation type
        SELECT CASE (MonthlyColumns(mColumn)%aggType)
          CASE (aggTypeSumOrAvg)
            MonthlyColumns(mColumn)%reslt = 0.0d0
            MonthlyColumns(mColumn)%duration = 0.0d0
          CASE (aggTypeMaximum)
            MonthlyColumns(mColumn)%reslt = -HUGE(BigNum)
            MonthlyColumns(mColumn)%timeStamp = 0
          CASE (aggTypeMinimum)
            MonthlyColumns(mColumn)%reslt = HUGE(BigNum)
            MonthlyColumns(mColumn)%timeStamp = 0
          CASE (aggTypeValueWhenMaxMin)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeHoursZero)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeHoursNonZero)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeHoursPositive)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeHoursNonPositive)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeHoursNegative)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeHoursNonNegative)
            MonthlyColumns(mColumn)%reslt = 0.0d0
          CASE (aggTypeSumOrAverageHoursShown)
            MonthlyColumns(mColumn)%reslt = 0.0d0
            MonthlyColumns(mColumn)%duration = 0.0d0
          CASE (aggTypeMaximumDuringHoursShown)
            MonthlyColumns(mColumn)%reslt = -HUGE(BigNum)
            MonthlyColumns(mColumn)%timeStamp = 0
          CASE (aggTypeMinimumDuringHoursShown)
            MonthlyColumns(mColumn)%reslt = HUGE(BigNum)
            MonthlyColumns(mColumn)%timeStamp = 0
        END SELECT
      ELSE !if no key corresponds to this instance of the report
        ErrCount2=ErrCount2+1
        IF (ErrCount2 == 1 .and. .not. DisplayExtraWarnings .and. .not. VarWarning .and. KindOfSim == ksRunPeriodWeather) THEN
          CALL ShowWarningError('Processing Monthly Tabular Reports: Variable names not valid for this simulation')
          CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual variables.')
        ENDIF
        !fixing CR5878 removed the showing of the warning once about a specific variable.
        IF (DisplayExtraWarnings .and. KindOfSim == ksRunPeriodWeather) THEN
          CALL ShowWarningError('Processing Monthly Tabular Reports: '//TRIM(MonthlyInput(TabNum)%name))
          CALL ShowContinueError('..Variable name='//TRIM(curVariMeter)//' not valid for this simulation.')
          CALL ShowContinueError('..i.e., Variable name='//TRIM(UniqueKeyNames(kUniqueKey))//':'//  &
                                    TRIM(curVariMeter)//' not valid for this simulation.')
          IF (VarWarning) THEN
            CALL ShowContinueError('..Variables not valid for this simulation will have "[Invalid/Undefined]"'//  &
                   ' in the Units Column of the Table Report.')
            VarWarning=.false.
          ENDIF
        ENDIF
        MonthlyColumns(mColumn)%varName = curVariMeter
        MonthlyColumns(mColumn)%varNum  = 0
        MonthlyColumns(mColumn)%typeOfVar = 0
        MonthlyColumns(mColumn)%avgSum = 0
        MonthlyColumns(mColumn)%stepType = 0
        MonthlyColumns(mColumn)%units = 'Invalid/Undefined'
        MonthlyColumns(mColumn)%aggType = aggTypeSumOrAvg
      ENDIF
!#ifdef ITM_KEYCACHE
!#else
!    DEALLOCATE(NamesOfKeys)
!    DEALLOCATE(IndexesForKeyVar)
!#endif
    END DO !ColNum
  END DO !kUniqueKey
END DO !TabNum the end of the loop through the inputs objects

!#ifdef ITM_KEYCACHE
!DEALLOCATE(NamesOfKeys)
!DEALLOCATE(IndexesForKeyVar)
!#endif

IF (ALLOCATED(UniqueKeynames)) DEALLOCATE(UniqueKeyNames)
END SUBROUTINE InitializeTabularMonthly

SUBROUTINE GetInputTabularTimeBins
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine initializes the data structures based
          !   on input from in the IDF file.  The data structures
          !   follow the IDD closely.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts
USE ScheduleManager, ONLY: GetScheduleIndex

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='Output:Table:TimeBins'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER                     :: iInObj     ! index when cycling through each idf input object
INTEGER                     :: NumParams  ! Number of elements combined
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE  :: AlphArray !character string data
REAL(r64), DIMENSION(:), ALLOCATABLE  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: iTable
INTEGER                     :: firstReport
INTEGER                     :: repIndex
INTEGER                     :: indx
INTEGER                     :: found
REAL(r64)                   :: bigVal !used with HUGE

CHARACTER(len=MaxNameLength),ALLOCATABLE,DIMENSION(:)  :: objNames
INTEGER,ALLOCATABLE,DIMENSION(:)                       :: objVarIDs

CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
ALLOCATE(AlphArray(NumAlphas))
AlphArray=' '
ALLOCATE(NumArray(NumNums))
NumArray=0.0d0

timeInYear=0.0d0 !intialize the time in year counter
! determine size of array that holds the IDF description
OutputTableBinnedCount = GetNumObjectsFound(CurrentModuleObject)
ALLOCATE(OutputTableBinned(OutputTableBinnedCount))
IF (OutputTableBinnedCount > 0) THEN
  WriteTabularFiles=.true.
  ! if not a run period using weather do not create reports
  IF (.NOT. DoWeathSim) THEN
    CALL ShowWarningError(CurrentModuleObject // &
       ' requested with SimulationControl Run Simulation for Weather File Run Periods set to No so ' &
        // CurrentModuleObject // ' will not be generated')
    RETURN
  END IF
END IF
! looking for maximum number of intervals for sizing
BinResultsIntervalCount = 0
BinResultsTableCount = 0
DO iInObj = 1 , OutputTableBinnedCount
  CALL GetObjectItem(CurrentModuleObject,iInObj,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  OutputTableBinned(iInObj)%keyValue      = AlphArray(1)
  OutputTableBinned(iInObj)%varOrMeter    = AlphArray(2)
  !if a schedule has been specified assign
  IF (LEN_TRIM(AlphArray(3)) .GT. 0) THEN
    OutputTableBinned(iInObj)%scheduleName  = AlphArray(3)
    OutputTableBinned(iInObj)%scheduleIndex = GetScheduleIndex(AlphArray(3))
    IF (OutputTableBinned(iInObj)%scheduleIndex .EQ. 0) THEN
      CALL ShowWarningError(CurrentModuleObject//': invalid '//TRIM(cAlphaFieldNames(3))//'="' //   &
         TRIM(AlphArray(3))//'" - not found.')
    END IF
  ELSE
    OutputTableBinned(iInObj)%scheduleIndex = 0 !flag value for no schedule used
  END IF
  !validate the kind of variable - not used internally except for validation
  IF (LEN_TRIM(AlphArray(4)) .GT. 0) THEN
    IF (.NOT. (SameString(AlphArray(4),'ENERGY') .OR. SameString(AlphArray(4),'DEMAND') .OR. &
      SameString(AlphArray(4),'TEMPERATURE') .OR. SameString(AlphArray(4),'FLOWRATE'))) THEN
      CALL ShowWarningError('In '//TRIM(CurrentModuleObject)//' named ' // TRIM(AlphArray(1)) //   &
                            ' the Variable Type was not energy, demand, temperature, or flowrate.')
    END IF
  END IF
  OutputTableBinned(iInObj)%intervalStart = NumArray(1)
  OutputTableBinned(iInObj)%intervalSize  = NumArray(2)
  OutputTableBinned(iInObj)%intervalCount = INT(NumArray(3))
  ! valid range checking on inputs
  IF (OutputTableBinned(iInObj)%intervalCount .LT. 1) THEN
    OutputTableBinned(iInObj)%intervalCount = 1
  END IF
  IF (OutputTableBinned(iInObj)%intervalCount .GT. 20) THEN
    OutputTableBinned(iInObj)%intervalCount = 20
  END IF
  IF (OutputTableBinned(iInObj)%intervalSize .LT. 0) THEN
    OutputTableBinned(iInObj)%intervalSize = 1000.0d0
  END IF
  OutputTableBinned(iInObj)%resIndex = BinResultsTableCount + 1  !the next results report
  ! find maximum number of intervals
  IF (OutputTableBinned(iInObj)%intervalCount .GT. BinResultsIntervalCount) THEN
    BinResultsIntervalCount = OutputTableBinned(iInObj)%intervalCount
  END IF
  CALL GetVariableKeyCountandType(OutputTableBinned(iInObj)%varOrMeter, OutputTableBinned(iInObj)%numTables, &
                                  OutputTableBinned(iInObj)%typeOfVar, OutputTableBinned(iInObj)%avgSum, &
                                  OutputTableBinned(iInObj)%stepType, OutputTableBinned(iInObj)%units)
  IF (OutputTableBinned(iInObj)%typeOfVar .EQ. 0) THEN
    CALL ShowWarningError(CurrentModuleObject//': User specified meter or variable not found: ' //   &
                            TRIM(OutputTableBinned(iInObj)%varOrMeter))
  END IF
  ! If only a single table key is requested than only one should be counted
  ! later will reset the numTables array pointer but for now use it to know
  ! how many items to scan through
  IF (OutputTableBinned(iInObj)%keyValue .EQ. '*') THEN
    BinResultsTableCount = BinResultsTableCount + OutputTableBinned(iInObj)%numTables
  ELSE
    BinResultsTableCount = BinResultsTableCount + 1  !if a particular key is requested then only one more report
  ENDIF
END DO
! size the arrays that holds the bin results
ALLOCATE(BinResults(BinResultsTableCount,BinResultsIntervalCount))
ALLOCATE(BinResultsBelow(BinResultsTableCount))
ALLOCATE(BinResultsAbove(BinResultsTableCount))
ALLOCATE(BinStatistics(BinResultsTableCount))
ALLOCATE(BinObjVarID(BinResultsTableCount))
! now that the arrays are sized go back and fill in
! what ID numbers are used for each table
DO iInObj = 1 , OutputTableBinnedCount
  firstReport = OutputTableBinned(iInObj)%resIndex
  ! allocate the arrays to the number of objects
  ALLOCATE(objNames(OutputTableBinned(iInObj)%numTables))
  ALLOCATE(objVarIDs(OutputTableBinned(iInObj)%numTables))
  CALL GetVariableKeys(OutputTableBinned(iInObj)%varOrMeter, OutputTableBinned(iInObj)%typeOfVar, objNames, objVarIDs)
  IF (OutputTableBinned(iInObj)%keyValue .EQ. '*') THEN
    DO iTable = 1, OutputTableBinned(iInObj)%numTables
      repIndex = firstReport + (iTable - 1)
      BinObjVarID(repIndex)%namesOfObj  = objNames(iTable)
      BinObjVarID(repIndex)%varMeterNum = objVarIDs(iTable)
      ! check if valid meter or number
      IF (objVarIDs(iTable) .EQ. 0) THEN
        CALL ShowWarningError(CurrentModuleObject//': Specified variable or meter not found: ' //   &
                            TRIM(objNames(iTable)))
      END IF
    END DO
  ELSE
    ! scan through the keys and look for the user specified key
    found = 0
    DO iTable = 1, OutputTableBinned(iInObj)%numTables
      IF (sameString(objNames(iTable),OutputTableBinned(iInObj)%keyValue)) THEN
        found = iTable
        EXIT
      END IF
    END DO
    ! the first and only report is assigned to the found object name
    IF (found .NE. 0) THEN
      BinObjVarID(firstReport)%namesOfObj  = objNames(found)
      BinObjVarID(firstReport)%varMeterNum = objVarIDs(found)
    ELSE
      CALL ShowWarningError(CurrentModuleObject//': Specified key not found, the first key will be used: ' //   &
                            TRIM(OutputTableBinned(iInObj)%keyValue))
      BinObjVarID(firstReport)%namesOfObj  = objNames(1)
      BinObjVarID(firstReport)%varMeterNum = objVarIDs(1)
      IF (objVarIDs(1) .EQ. 0) THEN
        CALL ShowWarningError(CurrentModuleObject//': Specified meter or variable not found: ' //   &
                            TRIM(objNames(1)))
      END IF
    END IF
    ! reset the number of tables to one
    OutputTableBinned(iInObj)%numTables = 1
  END IF
  ! release the arrays if they are already allocated
  DEALLOCATE(objNames)
  DEALLOCATE(objVarIDs)
END DO
! clear the binning arrays to zeros
! - not completely sure this approach will work
DO indx = 1, 12
  BinResults(1:BinResultsTableCount,1:BinResultsIntervalCount)%mnth(indx) = 0.0d0
  BinResultsBelow(1:BinResultsTableCount)%mnth(indx) = 0.0d0
  BinResultsAbove(1:BinResultsTableCount)%mnth(indx) = 0.0d0
END DO
DO indx = 1, 24
  BinResults(1:BinResultsTableCount,1:BinResultsIntervalCount)%hrly(indx) = 0.0d0
  BinResultsBelow(1:BinResultsTableCount)%hrly(indx) = 0.0d0
  BinResultsAbove(1:BinResultsTableCount)%hrly(indx) = 0.0d0
END DO
! initialize statistics counters
BinStatistics%minimum = HUGE(bigVal)
BinStatistics%maximum = -HUGE(bigVal)
BinStatistics%n       = 0
BinStatistics%sum     = 0.0d0
BinStatistics%sum2    = 0.0d0

DEALLOCATE(AlphArray)
DEALLOCATE(NumArray)

END SUBROUTINE GetInputTabularTimeBins

SUBROUTINE GetInputTabularStyle
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine set a flag for the output format for
          !   all tabular reports. This is a "unique" object.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts
USE DataStringGlobals, ONLY: CharComma, CharTab, CharSpace
USE SQLiteProcedures, ONLY: WriteTabularDataToSQLite, WriteOutputToSQLite

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='OutputControl:Table:Style'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER                     :: NumTabularStyle
INTEGER                     :: NumParams  ! Number of elements combined
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE  :: AlphArray !character string data
REAL(r64), DIMENSION(:), ALLOCATABLE  :: NumArray  !numeric data
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine

CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
ALLOCATE(AlphArray(NumAlphas))
AlphArray=' '
ALLOCATE(NumArray(NumNums))
NumArray=0.0d0

NumTabularStyle = GetNumObjectsFound(CurrentModuleObject)

IF (NumTabularStyle .EQ. 0) THEN
  AlphArray(1)='COMMA'
  numStyles = 1
  TableStyle(1) = tableStyleComma
  del(1) = CharComma !comma
  unitsStyle = unitsStyleNone
ELSEIF (NumTabularStyle .EQ. 1) THEN
  CALL GetObjectItem(CurrentModuleObject,1,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
  ! ColumnSeparator
  IF (SameString(AlphArray(1),'Comma')) THEN
    numStyles = 1
    TableStyle(1) = tableStyleComma
    del(1) = CharComma !comma
  ELSEIF (SameString(AlphArray(1),'Tab')) THEN
    numStyles = 1
    TableStyle(1) = tableStyleTab
    del(1) = CharTab  !tab
  ELSEIF (SameString(AlphArray(1),'Fixed')) THEN
    numStyles = 1
    TableStyle(1) = tableStyleFixed
    del(1) = CharSpace ! space
  ELSEIF (SameString(AlphArray(1),'HTML')) THEN
    numStyles = 1
    TableStyle(1) = tableStyleHTML
    del(1) = CharSpace !space - this is not used much for HTML output
  ELSEIF (SameString(AlphArray(1),'XML')) THEN
    numStyles = 1
    TableStyle(1) = tableStyleXML
    del(1) = CharSpace !space - this is not used much for XML output
  ELSEIF (SameString(AlphArray(1),'CommaAndHTML')) THEN
    numStyles = 2
    TableStyle(1) = tableStyleComma
    del(1) = CharComma !comma
    TableStyle(2) = tableStyleHTML
    del(2) = CharSpace !space - this is not used much for HTML output
  ELSEIF (SameString(AlphArray(1),'CommaAndXML')) THEN
    numStyles = 2
    TableStyle(1) = tableStyleComma
    del(1) = CharComma !comma
    TableStyle(2) = tableStyleXML
    del(2) = CharSpace !space - this is not used much for XML output
  ELSEIF (SameString(AlphArray(1),'TabAndHTML')) THEN
    numStyles = 2
    TableStyle(1) = tableStyleTab
    del(1) = CharTab  !tab
    TableStyle(2) = tableStyleHTML
    del(2) = CharSpace !space - this is not used much for HTML output
  ELSEIF (SameString(AlphArray(1),'XMLandHTML')) THEN
    numStyles = 2
    TableStyle(1) = tableStyleXML
    del(1) = CharSpace !space - this is not used much for XML output
    TableStyle(2) = tableStyleHTML
    del(2) = CharSpace !space - this is not used much for HTML output
  ELSEIF (SameString(AlphArray(1),'All')) THEN
    numStyles = 5
    TableStyle(1) = tableStyleComma
    del(1) = CharComma !comma
    TableStyle(2) = tableStyleTab
    del(2) = CharTab  !tab
    TableStyle(3) = tableStyleFixed
    del(3) = CharSpace ! space
    TableStyle(4) = tableStyleHTML
    del(4) = CharSpace !space - this is not used much for HTML output
    TableStyle(5) = tableStyleXML
    del(5) = CharSpace !space - this is not used much for XML output
  ELSE
    CALL ShowWarningError(CurrentModuleObject//': Invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
       TRIM(AlphArray(1))//'". Commas will be used.')
    numStyles = 1
    TableStyle(1) = tableStyleComma
    del(1) = CharComma !comma
    AlphArray(1)='COMMA'
  ENDIF
  !MonthlyUnitConversion
  IF (NumAlphas .GE. 2) THEN
    IF (SameString(AlphArray(2),'None')) THEN
      unitsStyle = unitsStyleNone
    ELSEIF (SameString(AlphArray(2),'JTOKWH')) THEN
      unitsStyle = unitsStyleJtoKWH
    ELSEIF (SameString(AlphArray(2),'JTOMJ')) THEN
      unitsStyle = unitsStyleJtoMJ
    ELSEIF (SameString(AlphArray(2),'JTOGJ')) THEN
      unitsStyle = unitsStyleJtoGJ
    ELSEIF (SameString(AlphArray(2),'INCHPOUND')) THEN
      unitsStyle = unitsStyleInchPound
    ELSE
      unitsStyle = unitsStyleNone
      CALL ShowWarningError(CurrentModuleObject//': Invalid '//TRIM(cAlphaFieldNames(2))//'="'//  &
         TRIM(AlphArray(2))//'". No unit conversion will be performed. Normal SI units will be shown.')
    ENDIF
  ELSE
    unitsStyle = unitsStyleNone
    AlphArray(2)='None'
  END IF
ELSEIF (NumTabularStyle .GT. 1) THEN
  CALL ShowWarningError(CurrentModuleObject//': Only one instance of this object is allowed. Commas will be used.')
  TableStyle = tableStyleComma
  del = CharComma !comma
  AlphArray(1)='COMMA'
  unitsStyle = unitsStyleNone
  AlphArray(2)='None'
ENDIF

IF (WriteTabularFiles) THEN
  Write(OutputFileInits,fmta) '! <Tabular Report>,Style,Unit Conversion'
  IF (AlphArray(1) /= 'HTML') THEN
    CALL ConvertCaseToLower(AlphArray(1),AlphArray(2))
    AlphArray(1)(2:)=AlphArray(2)(2:)
  ENDIF
  WRITE(OutputFileInits,"('Tabular Report,',A,',',A)") TRIM(AlphArray(1)),TRIM(AlphArray(2))
ENDIF

DEALLOCATE(AlphArray)
DEALLOCATE(NumArray)

END SUBROUTINE GetInputTabularStyle

SUBROUTINE GetInputTabularPredefined
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine flags if any of the predefined reports
          !   are requested by the user

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataIPShortCuts
USE OutputProcessor, ONLY: EndUseCategory, MaxNumSubcategories
USE DataStringGlobals, ONLY: CharComma, CharTab, CharSpace
USE OutputReportPredefined, ONLY: reportName, numReportName
USE General, ONLY: RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='Output:Table:SummaryReports'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER, EXTERNAL                      :: GetMeterIndex  !an external subroutine

INTEGER                     :: NumTabularPredefined
INTEGER                     :: NumParams
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)  :: AlphArray
REAL(r64),ALLOCATABLE,                         DIMENSION(:)  :: NumArray
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: iReport
CHARACTER(len=MaxNameLength) :: meterName
INTEGER                      :: meterNumber
INTEGER :: iResource
INTEGER :: jEndUse
INTEGER :: kEndUseSub
INTEGER :: jReport
INTEGER :: lenAlpha
INTEGER :: lenReport
LOGICAL :: nameFound
LOGICAL :: ErrorsFound

ErrorsFound=.false.
NumTabularPredefined = GetNumObjectsFound(CurrentModuleObject)
IF (NumTabularPredefined .EQ. 1) THEN
  ! find out how many fields since the object is extensible
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
  ! allocate the temporary arrays for the call to get the filed
  ALLOCATE(AlphArray(NumAlphas))
  AlphArray = ''
  ! don't really need the NumArray since not expecting any numbers but the call requires it
  ALLOCATE(NumArray(NumNums))
  NumArray = 0.0d0
  ! get the object
  CALL GetObjectItem(CurrentModuleObject,1,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT)
  ! default all report flags to false (do not get produced)
  displayTabularBEPS = .FALSE.
  ! initialize the names of the predefined monthly report titles
  CALL InitializePredefinedMonthlyTitles
  ! loop through the fields looking for matching report titles
  DO iReport = 1, NumAlphas
    nameFound=.false.
    IF (SameString(AlphArray(iReport),'ABUPS')) THEN
      displayTabularBEPS = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'AnnualBuildingUtilityPerformanceSummary')) THEN
      displayTabularBEPS = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'BEPS')) THEN
      displayTabularBEPS = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'ComponentCostEconomicsSummary')) then
      displayTabularCompCosts = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'InputVerificationandResultsSummary')) then
      displayTabularVeriSum = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'IVRS')) then
      displayTabularVeriSum = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'ComponentSizingSummary')) then
      displayComponentSizing = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'CSS')) then
      displayComponentSizing = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'SurfaceShadowingSummary')) then
      displaySurfaceShadowing = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'SHAD')) then
      displaySurfaceShadowing = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'DemandEndUseComponentsSummary')) then
      displayDemandEndUse = .true.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'AdaptiveComfortSummary')) then
      displayAdaptiveComfort = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'SourceEnergyEndUseComponentsSummary')) then
      displaySourceEnergyEndUseSummary = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'ZoneComponentLoadSummary')) then
      displayZoneComponentLoadSummary = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'LEEDSummary')) then
      displayLEEDSummary = .TRUE.
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'EnergyMeters')) then
      WriteTabularFiles=.true.
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'AllSummary')) then
      WriteTabularFiles=.true.
      displayTabularBEPS = .TRUE.
      displayTabularVeriSum = .TRUE.
      displayTabularCompCosts = .TRUE.
      displaySurfaceShadowing = .TRUE.
      displayComponentSizing = .TRUE.
      displayDemandEndUse = .true.
      displayAdaptiveComfort = .TRUE.
      displaySourceEnergyEndUseSummary = .TRUE.
      nameFound=.true.
      DO jReport = 1, numReportName
        reportName(jReport)%show = .TRUE.
      END DO
    ELSEIF (SameString(AlphArray(iReport),'AllSummaryAndSizingPeriod')) then
      WriteTabularFiles=.true.
      displayTabularBEPS = .TRUE.
      displayTabularVeriSum = .TRUE.
      displayTabularCompCosts = .TRUE.
      displaySurfaceShadowing = .TRUE.
      displayComponentSizing = .TRUE.
      displayDemandEndUse = .true.
      displayAdaptiveComfort = .TRUE.
      displaySourceEnergyEndUseSummary = .TRUE.
      nameFound=.true.
      DO jReport = 1, numReportName
        reportName(jReport)%show = .TRUE.
      END DO
      !the sizing period reports
      displayZoneComponentLoadSummary = .TRUE.
    ELSEIF (SameString(AlphArray(iReport),'AllMonthly')) then
      WriteTabularFiles=.true.
      DO jReport = 1, numNamedMonthly
        namedMonthly(jReport)%show = .TRUE.
      END DO
      nameFound=.true.
    ELSEIF (SameString(AlphArray(iReport),'AllSummaryAndMonthly')) then
      WriteTabularFiles=.true.
      displayTabularBEPS = .TRUE.
      displayTabularVeriSum = .TRUE.
      displayTabularCompCosts = .TRUE.
      displaySurfaceShadowing = .TRUE.
      displayComponentSizing = .TRUE.
      displayDemandEndUse = .true.
      displayAdaptiveComfort = .TRUE.
      displaySourceEnergyEndUseSummary = .TRUE.
      nameFound=.true.
      DO jReport = 1, numReportName
        reportName(jReport)%show = .TRUE.
      END DO
      DO jReport = 1, numNamedMonthly
        namedMonthly(jReport)%show = .TRUE.
      END DO
    ELSEIF (SameString(AlphArray(iReport),'AllSummaryMonthlyAndSizingPeriod')) then
      WriteTabularFiles=.true.
      displayTabularBEPS = .TRUE.
      displayTabularVeriSum = .TRUE.
      displayTabularCompCosts = .TRUE.
      displaySurfaceShadowing = .TRUE.
      displayComponentSizing = .TRUE.
      displayDemandEndUse = .true.
      displayAdaptiveComfort = .TRUE.
      displaySourceEnergyEndUseSummary = .TRUE.
      nameFound=.true.
      DO jReport = 1, numReportName
        reportName(jReport)%show = .TRUE.
      END DO
      DO jReport = 1, numNamedMonthly
        namedMonthly(jReport)%show = .TRUE.
      END DO
      !the sizing period reports
      displayZoneComponentLoadSummary = .TRUE.
    ENDIF
    !check the reports that are predefined and are created by outputreportpredefined.f90
    DO jReport = 1, numReportName
      lenAlpha = LEN_TRIM(AlphArray(iReport))
      lenReport = LEN_TRIM(reportName(jReport)%name)
      IF (SameString(AlphArray(iReport),reportName(jReport)%name)) THEN
        WriteTabularFiles=.true.
        reportName(jReport)%show = .TRUE.
        nameFound=.true.
      END IF
      IF (SameString(AlphArray(iReport),reportName(jReport)%abrev)) THEN
        WriteTabularFiles=.true.
        reportName(jReport)%show = .TRUE.
        nameFound=.true.
      END IF
    END DO
   ! check if the predefined monthly reports are used
    DO jReport = 1, numNamedMonthly
      IF (SameString(AlphArray(iReport),namedMonthly(jReport)%title)) THEN
        namedMonthly(jReport)%show = .TRUE.
        WriteTabularFiles=.TRUE.
        nameFound=.true.
      END IF
    END DO
    IF (.not. nameFound) THEN
      CALL ShowSevereError(CurrentModuleObject//' Field['//trim(RoundSigDigits(iReport))//']="'//  &
         trim(AlphArray(iReport))//'", invalid report name -- will not be reported.')
!      ErrorsFound=.true.
    ENDIF
  END DO
  CALL CreatePredefinedMonthlyReports
  DEALLOCATE(AlphArray)
  DEALLOCATE(NumArray)
ELSEIF (NumTabularPredefined > 1) THEN
  CALL ShowSevereError(CurrentModuleObject//': Only one instance of this object is allowed.')
  ErrorsFound=.true.
END IF
IF (ErrorsFound) THEN
  CALL ShowFatalError(CurrentModuleObject//': Preceding errors cause termination.')
ENDIF
! if the BEPS report has been called for than initialize its arrays
IF (displayTabularBEPS .OR. displayDemandEndUse .OR. displaySourceEnergyEndUseSummary .or. displayLEEDSummary) THEN
! initialize the resource type names
  resourceTypeNames(1)  = 'Electricity'
  resourceTypeNames(2)  = 'Gas'
  resourceTypeNames(3)  = 'DistrictCooling'
  resourceTypeNames(4)  = 'DistrictHeating'
  resourceTypeNames(5)  = 'Steam'
  resourceTypeNames(6)  = 'Gasoline'
  resourceTypeNames(7)  = 'Water'
  resourceTypeNames(8)  = 'Diesel'
  resourceTypeNames(9)  = 'Coal'
  resourceTypeNames(10) = 'FuelOil#1'
  resourceTypeNames(11) = 'FuelOil#2'
  resourceTypeNames(12) = 'Propane'
  resourceTypeNames(13) = 'OtherFuel1'
  resourceTypeNames(14) = 'OtherFuel2'

  sourceTypeNames(1)='Electric'
  sourceTypeNames(2)='NaturalGas'
  sourceTypeNames(3)='Gasoline'
  sourceTypeNames(4)='Diesel'
  sourceTypeNames(5)='Coal'
  sourceTypeNames(6)='FuelOil#1'
  sourceTypeNames(7)='FuelOil#2'
  sourceTypeNames(8)='Propane'
  sourceTypeNames(9)='PurchasedElectric'
  sourceTypeNames(10)='SoldElectric'
  sourceTypeNames(11)='OtherFuel1'
  sourceTypeNames(12)='OtherFuel2'

  ! initialize the end use names
  endUseNames(endUseHeating) = 'Heating'
  endUseNames(endUseCooling) = 'Cooling'
  endUseNames(endUseInteriorLights) = 'InteriorLights'
  endUseNames(endUseExteriorLights) = 'ExteriorLights'
  endUseNames(endUseInteriorEquipment) = 'InteriorEquipment'
  endUseNames(endUseExteriorEquipment) = 'ExteriorEquipment'
  endUseNames(endUseFans) = 'Fans'
  endUseNames(endUsePumps) = 'Pumps'
  endUseNames(endUseHeatRejection) = 'HeatRejection'
  endUseNames(endUseHumidification) = 'Humidifier'
  endUseNames(endUseHeatRecovery) = 'HeatRecovery'
  endUseNames(endUseWaterSystem) = 'WaterSystems'
  endUseNames(endUseRefrigeration) = 'Refrigeration'
  endUseNames(endUseCogeneration) = 'Cogeneration'

  ! End use subs must be dynamically allocated to accomodate the end use with the most subcategories
  ALLOCATE(meterNumEndUseSubBEPS(numResourceTypes,numEndUses,MaxNumSubcategories))
  meterNumEndUseSubBEPS = 0

  ! loop through all of the resources and end uses and sub end uses for the entire facility
  DO iResource = 1, numResourceTypes
    meterName = TRIM(resourceTypeNames(iResource)) // ':FACILITY'
    meterNumber = GetMeterIndex(meterName)
    meterNumTotalsBEPS(iResource) = meterNumber

    DO jEndUse = 1, numEndUses
      meterName = TRIM(endUseNames(jEndUse)) // ':' //  TRIM(resourceTypeNames(iResource)) !// ':FACILITY'
      meterNumber = GetMeterIndex(meterName)
      meterNumEndUseBEPS(jEndUse,iResource) = meterNumber

      DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
        meterName = TRIM(EndUseCategory(jEndUse)%SubcategoryName(kEndUseSub)) &
          //':'//TRIM(endUseNames(jEndUse))//':'//TRIM(resourceTypeNames(iResource))
        meterNumber = GetMeterIndex(meterName)
        meterNumEndUseSubBEPS(iResource,jEndUse,kEndUseSub) = meterNumber
      END DO
    END DO
  END DO

  DO iResource = 1, numSourceTypes
    meterNumber = GetMeterIndex(trim(sourceTypeNames(iResource))//'Emissions:Source')
    meterNumTotalsSource(iResource) = meterNumber
  END DO

  ! initialize the gathering arrays to zero
  gatherTotalsBEPS   = 0.0d0
  gatherTotalsBySourceBEPS   = 0.0d0
  gatherTotalsSource = 0.0d0
  gatherTotalsBySource = 0.0d0
  gatherEndUseBEPS   = 0.0d0
  gatherEndUseBySourceBEPS   = 0.0d0
  ! End use subs must be dynamically allocated to accomodate the end use with the most subcategories
  ALLOCATE(gatherEndUseSubBEPS(numResourceTypes,numEndUses,MaxNumSubcategories))
  gatherEndUseSubBEPS = 0.0d0
  ALLOCATE(gatherDemandEndUseSub(numResourceTypes,numEndUses,MaxNumSubcategories))
  gatherDemandEndUseSub = 0.0d0

  ! get meter numbers for other meters relating to electric load components
  meterNumPowerFuelFireGen =   GetMeterIndex('Cogeneration:ElectricityProduced')
  meterNumPowerPV =            GetMeterIndex('Photovoltaic:ElectricityProduced')
  meterNumPowerWind =          GetMeterIndex('WindTurbine:ElectricityProduced')
  meterNumPowerHTGeothermal =  GetMeterIndex('HTGeothermal:ElectricityProduced')
  meterNumElecProduced =       GetMeterIndex('ElectricityProduced:Facility')
  meterNumElecPurchased =      GetMeterIndex('ElectricityPurchased:Facility')
  meterNumElecSurplusSold =    GetMeterIndex('ElectricitySurplusSold:Facility')
  ! if no ElectricityPurchased:Facility meter is defined then no electric load center
  ! was created by the user and no power generation will occur in the plant. The amount
  ! purchased would be the total end use.
  IF (meterNumElecPurchased .EQ. 0) THEN
    meterNumElecPurchased = GetMeterIndex('Electricity:Facility')
  END IF

  ! initialize the gathering variables for the electric load components
  gatherPowerFuelFireGen = 0.0d0
  gatherPowerPV = 0.0d0
  gatherPowerWind = 0.0d0
  gatherPowerHTGeothermal = 0.0d0
  gatherElecProduced = 0.0d0
  gatherElecPurchased = 0.0d0
  gatherElecSurplusSold = 0.0d0

  ! get meter numbers for onsite thermal components on BEPS report
  meterNumWaterHeatRecovery =    GetMeterIndex('HeatRecovery:EnergyTransfer')
  meterNumAirHeatRecoveryCool =  GetMeterIndex('HeatRecoveryForCooling:EnergyTransfer')
  meterNumAirHeatRecoveryHeat =  GetMeterIndex('HeatRecoveryForHeating:EnergyTransfer')
  meterNumHeatHTGeothermal =     GetMeterIndex('HTGeothermal:HeatProduced')
  meterNumHeatSolarWater =       GetMeterIndex('SolarWater:Facility')
  meterNumHeatSolarAir =         GetMeterIndex('HeatProduced:SolarAir')
  ! initialize the gathering variables for onsite thermal components on BEPS report
  gatherWaterHeatRecovery = 0.0d0
  gatherAirHeatRecoveryCool = 0.0d0
  gatherAirHeatRecoveryHeat = 0.0d0
  gatherHeatHTGeothermal = 0.0d0
  gatherHeatSolarWater = 0.0d0
  gatherHeatSolarAir = 0.0d0

  ! get meter numbers for water components on BEPS report
  meterNumRainWater =     GetMeterIndex('Rainwater:OnSiteWater')
  meterNumCondensate =    GetMeterIndex('Condensate:OnSiteWater')
  meterNumGroundwater =   GetMeterIndex('Wellwater:OnSiteWater')
  meterNumMains =         GetMeterIndex('MainsWater:Facility')
  meterNumWaterEndUseTotal =   GetMeterIndex('Water:Facility')

  ! initialize the gathering variables for water components on BEPS report
  gatherRainWater        = 0.0d0
  gatherCondensate       = 0.0d0
  gatherWellwater        = 0.0d0
  gatherMains            = 0.0d0
  gatherWaterEndUseTotal = 0.0d0

END IF
END SUBROUTINE GetInputTabularPredefined

LOGICAL FUNCTION isCompLoadRepReq()
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Determine if the ZoneComponentLoadSummary or
          !   ZoneComponentLoadDetail reports are requested.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: CurrentModuleObject='Output:Table:SummaryReports'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER                     :: NumTabularPredefined
INTEGER                     :: NumParams
INTEGER                     :: NumAlphas  ! Number of elements in the alpha array
INTEGER                     :: NumNums    ! Number of elements in the numeric array
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)  :: AlphArray
REAL(r64),ALLOCATABLE,                         DIMENSION(:)  :: NumArray
INTEGER                     :: IOStat     ! IO Status when calling get input subroutine
INTEGER                     :: iReport
LOGICAL :: isFound

isFound = .FALSE.
NumTabularPredefined = GetNumObjectsFound(CurrentModuleObject)
IF (NumTabularPredefined .EQ. 1) THEN
  ! find out how many fields since the object is extensible
  CALL GetObjectDefMaxArgs(CurrentModuleObject,NumParams,NumAlphas,NumNums)
  ! allocate the temporary arrays for the call to get the filed
  ALLOCATE(AlphArray(NumAlphas))
  AlphArray = ''
  ! don't really need the NumArray since not expecting any numbers but the call requires it
  ALLOCATE(NumArray(NumNums))
  NumArray = 0.0d0
  ! get the object
  CALL GetObjectItem(CurrentModuleObject,1,AlphArray,NumAlphas, &
                    NumArray,NumNums,IOSTAT)
  ! loop through the fields looking for matching report titles
  DO iReport = 1, NumAlphas
    IF (SameString(AlphArray(iReport),'ZoneComponentLoadSummary')) then
      isFound = .TRUE.
    END IF
    IF (SameString(AlphArray(iReport),'AllSummaryAndSizingPeriod')) then
      isFound = .TRUE.
    END IF
    IF (SameString(AlphArray(iReport),'AllSummaryMonthlyAndSizingPeriod')) then
      isFound = .TRUE.
    END IF
  END DO
  DEALLOCATE(AlphArray)
  DEALLOCATE(NumArray)
END IF
isCompLoadRepReq = isFound !return true if either report was found
END FUNCTION


SUBROUTINE InitializePredefinedMonthlyTitles
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Initialize the NamedMonthly array for the titles
          !   of the monthly predefined reports

          ! METHODOLOGY EMPLOYED:
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataOutputs
USE General, ONLY: RoundSigDigits

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: xcount

ALLOCATE(namedMonthly(numNamedMonthly))
namedMonthly(1)%title = 'ZoneCoolingSummaryMonthly'
namedMonthly(2)%title = 'ZoneHeatingSummaryMonthly'
namedMonthly(3)%title = 'ZoneElectricSummaryMonthly'
namedMonthly(4)%title = 'SpaceGainsMonthly'
namedMonthly(5)%title = 'PeakSpaceGainsMonthly'
namedMonthly(6)%title = 'SpaceGainComponentsAtCoolingPeakMonthly'
namedMonthly(7)%title = 'EnergyConsumptionElectricityNaturalGasMonthly'
namedMonthly(8)%title = 'EnergyConsumptionElectricityGeneratedPropaneMonthly'
namedMonthly(9)%title = 'EnergyConsumptionDieselFuelOilMonthly'
namedMonthly(10)%title = 'EnergyConsumptionDistrictHeatingCoolingMonthly'
namedMonthly(11)%title = 'EnergyConsumptionCoalGasolineMonthly'
namedMonthly(12)%title = 'EnergyConsumptionOtherFuelsMonthly'
namedMonthly(13)%title = 'EndUseEnergyConsumptionElectricityMonthly'
namedMonthly(14)%title = 'EndUseEnergyConsumptionNaturalGasMonthly'
namedMonthly(15)%title = 'EndUseEnergyConsumptionDieselMonthly'
namedMonthly(16)%title = 'EndUseEnergyConsumptionFuelOilMonthly'
namedMonthly(17)%title = 'EndUseEnergyConsumptionCoalMonthly'
namedMonthly(18)%title = 'EndUseEnergyConsumptionPropaneMonthly'
namedMonthly(19)%title = 'EndUseEnergyConsumptionGasolineMonthly'
namedMonthly(20)%title = 'EndUseEnergyConsumptionOtherFuelsMonthly'
namedMonthly(21)%title = 'PeakEnergyEndUseElectricityPart1Monthly'
namedMonthly(22)%title = 'PeakEnergyEndUseElectricityPart2Monthly'
namedMonthly(23)%title = 'ElectricComponentsOfPeakDemandMonthly'
namedMonthly(24)%title = 'PeakEnergyEndUseNaturalGasMonthly'
namedMonthly(25)%title = 'PeakEnergyEndUseDieselMonthly'
namedMonthly(26)%title = 'PeakEnergyEndUseFuelOilMonthly'
namedMonthly(27)%title = 'PeakEnergyEndUseCoalMonthly'
namedMonthly(28)%title = 'PeakEnergyEndUsePropaneMonthly'
namedMonthly(29)%title = 'PeakEnergyEndUseGasolineMonthly'
namedMonthly(30)%title = 'PeakEnergyEndUseOtherFuelsMonthly'
namedMonthly(31)%title = 'SetpointsNotMetWithTemperaturesMonthly'
namedMonthly(32)%title = 'ComfortReportSimple55Monthly'
namedMonthly(33)%title = 'UnglazedTranspiredSolarCollectorSummaryMonthly'
namedMonthly(34)%title = 'OccupantComfortDataSummaryMonthly'
namedMonthly(35)%title = 'ChillerReportMonthly'
namedMonthly(36)%title = 'TowerReportMonthly'
namedMonthly(37)%title = 'BoilerReportMonthly'
namedMonthly(38)%title = 'DXReportMonthly'
namedMonthly(39)%title = 'WindowReportMonthly'
namedMonthly(40)%title = 'WindowEnergyReportMonthly'
namedMonthly(41)%title = 'WindowZoneSummaryMonthly'
namedMonthly(42)%title = 'WindowEnergyZoneSummaryMonthly'
namedMonthly(43)%title = 'AverageOutdoorConditionsMonthly'
namedMonthly(44)%title = 'OutdoorConditionsMaximumDryBulbMonthly'
namedMonthly(45)%title = 'OutdoorConditionsMinimumDryBulbMonthly'
namedMonthly(46)%title = 'OutdoorConditionsMaximumWetBulbMonthly'
namedMonthly(47)%title = 'OutdoorConditionsMaximumDewPointMonthly'
namedMonthly(48)%title = 'OutdoorGroundConditionsMonthly'
namedMonthly(49)%title = 'WindowACReportMonthly'
namedMonthly(50)%title = 'WaterHeaterReportMonthly'
namedMonthly(51)%title = 'GeneratorReportMonthly'
namedMonthly(52)%title = 'DaylightingReportMonthly'
namedMonthly(53)%title = 'CoilReportMonthly'
namedMonthly(54)%title = 'PlantLoopDemandReportMonthly'
namedMonthly(55)%title = 'FanReportMonthly'
namedMonthly(56)%title = 'PumpReportMonthly'
namedMonthly(57)%title = 'CondLoopDemandReportMonthly'
namedMonthly(58)%title = 'ZoneTemperatureOscillationReportMonthly'
namedMonthly(59)%title = 'AirLoopSystemEnergyAndWaterUseMonthly'
namedMonthly(60)%title = 'AirLoopSystemComponentLoadsMonthly'
namedMonthly(61)%title = 'AirLoopSystemComponentEnergyUseMonthly'
namedMonthly(62)%title = 'MechanicalVentilationLoadsMonthly'

IF (numNamedMonthly /= NumMonthlyReports) THEN
  CALL ShowFatalError('InitializePredefinedMonthlyTitles: Number of Monthly Reports in OutputReportTabular=['//  &
    trim(RoundSigDigits(numNamedMonthly))//'] does not match number in DataOutputs=['//  &
    trim(RoundSigDigits(NumMonthlyReports))//'].')
ELSE
  DO xcount=1,NumNamedMonthly
    IF (.not. SameString(MonthlyNamedReports(xcount),namedMonthly(xcount)%title)) THEN
      CALL ShowSevereError('InitializePredefinedMonthlyTitles: Monthly Report Titles in OutputReportTabular do not match'//  &
         ' titles in DataOutput.')
      CALL ShowContinueError('first mismatch at ORT ['//trim(RoundSigDigits(NumNamedMonthly))//'] ="'//  &
         trim(namedMonthly(xcount)%Title)//'".')
      CALL ShowContinueError('same location in DO ="'//trim(MonthlyNamedReports(xcount))//'".')
      CALL ShowFatalError('Preceding condition causes termination.')
    ENDIF
  ENDDO
ENDIF
END SUBROUTINE InitializePredefinedMonthlyTitles

SUBROUTINE CreatePredefinedMonthlyReports
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   For any predefined monthly reports that have been
          !   called out, define the individual columns.

          ! METHODOLOGY EMPLOYED:
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: curReport

! ----------------------------------------------------------------------------------------
! If any variable are added to these reports they also need to be added to the
! AddVariablesForMonthlyReport routine in InputProcessor.
! ----------------------------------------------------------------------------------------

IF (NamedMonthly(1)%show) THEN
  curReport = AddMonthlyReport('ZoneCoolingSummaryMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Air System Sensible Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Air System Sensible Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Total Internal Latent Gain Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Total Internal Latent Gain Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(2)%show) THEN
  curReport = AddMonthlyReport('ZoneHeatingSummaryMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Air System Sensible Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Air System Sensible Heating Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(3)%show) THEN
  curReport = AddMonthlyReport('ZoneElectricSummaryMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Lights Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Lights Electric Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Electric Equipment Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Electric Equipment Electric Energy','',aggTypeMaximum)
END IF
IF (NamedMonthly(4)%show) THEN
  curReport = AddMonthlyReport('SpaceGainsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone People Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Lights Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Electric Equipment Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Gas Equipment Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Hot Water Equipment Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Steam Equipment Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Other Equipment Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Infiltration Sensible Heat Gain Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Infiltration Sensible Heat Loss Energy','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(5)%show) THEN
  curReport = AddMonthlyReport('PeakSpaceGainsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone People Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Lights Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Electric Equipment Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Gas Equipment Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Hot Water Equipment Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Steam Equipment Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Other Equipment Total Heating Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Infiltration Sensible Heat Gain Energy','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Infiltration Sensible Heat Loss Energy','',aggTypeMaximum)
END IF
IF (NamedMonthly(6)%show) THEN
  curReport = AddMonthlyReport('SpaceGainComponentsAtCoolingPeakMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Air System Sensible Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone People Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Lights Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Electric Equipment Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Gas Equipment Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Hot Water Equipment Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Steam Equipment Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Other Equipment Total Heating Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Infiltration Sensible Heat Gain Energy','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Infiltration Sensible Heat Loss Energy','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(7)%show) THEN
  curReport = AddMonthlyReport('EnergyConsumptionElectricityNaturalGasMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Electricity:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Electricity:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Gas:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Gas:Facility','',aggTypeMaximum)
END IF
IF (NamedMonthly(8)%show) THEN
  curReport = AddMonthlyReport('EnergyConsumptionElectricityGeneratedPropaneMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ElectricityProduced:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'ElectricityProduced:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Propane:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Propane:Facility','',aggTypeMaximum)
END IF
IF (NamedMonthly(9)%show) THEN
  curReport = AddMonthlyReport('EnergyConsumptionDieselFuelOilMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Diesel:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Diesel:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'FuelOil#1:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'FuelOil#1:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'FuelOil#2:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'FuelOil#2:Facility','',aggTypeMaximum)
END IF
IF (NamedMonthly(10)%show) THEN
  curReport = AddMonthlyReport('EnergyConsumptionDistrictHeatingCoolingMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'DistrictCooling:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'DistrictCooling:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'DistrictHeating:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'DistrictHeating:Facility','',aggTypeMaximum)
END IF
IF (NamedMonthly(11)%show) THEN
  curReport = AddMonthlyReport('EnergyConsumptionCoalGasolineMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Coal:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Coal:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Gasoline:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Gasoline:Facility','',aggTypeMaximum)
END IF
IF (NamedMonthly(12)%show) THEN
  curReport = AddMonthlyReport('EnergyConsumptionOtherFuelsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'OtherFuel1:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'OtherFuel1:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'OtherFuel2:Facility','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'OtherFuel2:Facility','',aggTypeMaximum)
END IF
IF (NamedMonthly(13)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionElectricityMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorLights:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorLights:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorEquipment:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Fans:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Pumps:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'HeatRejection:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Humidifier:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'HeatRecovery:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Electricity','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Electricity','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(14)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionNaturalGasMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorEquipment:Gas','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Gas','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Gas','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Gas','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Gas','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Gas','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(15)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionDieselMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Diesel','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Diesel','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Diesel','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Diesel','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Diesel','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(16)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionFuelOilMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:FuelOil#1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:FuelOil#1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:FuelOil#1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:FuelOil#1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:FuelOil#1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:FuelOil#2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:FuelOil#2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:FuelOil#2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:FuelOil#2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:FuelOil#2','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(17)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionCoalMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Coal','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Coal','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Coal','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(18)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionPropaneMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Propane','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Propane','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Propane','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Propane','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Propane','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(19)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionGasolineMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Gasoline','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Gasoline','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Gasoline','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Gasoline','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Gasoline','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(20)%show) THEN
  curReport = AddMonthlyReport('EndUseEnergyConsumptionOtherFuelsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:OtherFuel1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:OtherFuel1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:OtherFuel1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:OtherFuel1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:OtherFuel1','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:OtherFuel2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:OtherFuel2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:OtherFuel2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:OtherFuel2','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:OtherFuel2','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(21)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseElectricityPart1Monthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorLights:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorLights:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorEquipment:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Fans:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Pumps:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Electricity','',aggTypeMaximum)
END IF
IF (NamedMonthly(22)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseElectricityPart2Monthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'HeatRejection:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Humidifier:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'HeatRecovery:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Electricity','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Electricity','',aggTypeMaximum)
END IF
IF (NamedMonthly(23)%show) THEN
  curReport = AddMonthlyReport('ElectricComponentsOfPeakDemandMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Electricity:Facility','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorLights:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorEquipment:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorLights:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Fans:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Pumps:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Electricity','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'HeatRejection:Electricity','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(24)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseNaturalGasMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'InteriorEquipment:Gas','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Gas','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Gas','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Gas','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Gas','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Gas','',aggTypeMaximum)
END IF
IF (NamedMonthly(25)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseDieselMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Diesel','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Diesel','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Diesel','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Diesel','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Diesel','',aggTypeMaximum)
END IF
IF (NamedMonthly(26)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseFuelOilMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:FuelOil#1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:FuelOil#1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:FuelOil#1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:FuelOil#1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:FuelOil#1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:FuelOil#2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:FuelOil#2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:FuelOil#2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:FuelOil#2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:FuelOil#2','',aggTypeMaximum)
END IF
IF (NamedMonthly(27)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseCoalMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Coal','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Coal','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Coal','',aggTypeMaximum)
END IF
IF (NamedMonthly(28)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUsePropaneMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Propane','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Propane','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Propane','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Propane','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Propane','',aggTypeMaximum)
END IF
IF (NamedMonthly(29)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseGasolineMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:Gasoline','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:Gasoline','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:Gasoline','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:Gasoline','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:Gasoline','',aggTypeMaximum)
END IF
IF (NamedMonthly(30)%show) THEN
  curReport = AddMonthlyReport('PeakEnergyEndUseOtherFuelsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:OtherFuel1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:OtherFuel1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:OtherFuel1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:OtherFuel1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:OtherFuel1','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'ExteriorEquipment:OtherFuel2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling:OtherFuel2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Heating:OtherFuel2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'WaterSystems:OtherFuel2','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cogeneration:OtherFuel2','',aggTypeMaximum)
END IF
IF (NamedMonthly(31)%show) THEN
  curReport = AddMonthlyReport('SetpointsNotMetWithTemperaturesMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Heating Setpoint Not Met Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Heating Setpoint Not Met While Occupied Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Cooling Setpoint Not Met Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Cooling Setpoint Not Met While Occupied Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
END IF
IF (NamedMonthly(32)%show) THEN
  curReport = AddMonthlyReport('ComfortReportSimple55Monthly',2)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mean Air Temperature','',aggTypeSumOrAverageHoursShown)
END IF
IF (NamedMonthly(33)%show) THEN
  curReport = AddMonthlyReport('UnglazedTranspiredSolarCollectorSummaryMonthly',5)
  CALL AddMonthlyFieldSetInput(curReport,'Solar Collector System Efficiency','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Solar Collector System Efficiency','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Solar Collector Outside Face Suction Velocity','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Solar Collector Sensible Heating Rate','',aggTypeSumOrAverageHoursShown)
END IF
IF (NamedMonthly(34)%show) THEN
  curReport = AddMonthlyReport('OccupantComfortDataSummaryMonthly',5)
  CALL AddMonthlyFieldSetInput(curReport,'People Occupant Count','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'People Air Temperature','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'People Air Relative Humidity','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Thermal Comfort Fanger Model PMV','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Thermal Comfort Fanger Model PPD','',aggTypeSumOrAverageHoursShown)
END IF
IF (NamedMonthly(35)%show) THEN
  curReport = AddMonthlyReport('ChillerReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Electric Power','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Electric Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Evaporator Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Evaporator Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Condenser Heat Transfer Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller COP','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller COP','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Part Load Ratio','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Chiller Part Load Ratio','',aggTypeMaximum)
END IF
IF (NamedMonthly(36)%show) THEN
  curReport = AddMonthlyReport('TowerReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Fan Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Fan Electric Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Fan Electric Power','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Heat Transfer Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Inlet Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Outlet Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Tower Mass Flow Rate','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(37)%show) THEN
  curReport = AddMonthlyReport('BoilerReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Gas Consumption','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Heating Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Heating Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Gas Consumption Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Inlet Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Outlet Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Mass Flow Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Ancillary Electric Power','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Part Load Ratio','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Boiler Part Load Ratio','',aggTypeMaximum)
END IF
IF (NamedMonthly(38)%show) THEN
  curReport = AddMonthlyReport('DXReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Total Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Total Cooling Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Sensible Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Latent Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Crankcase Heater Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Runtime Fraction','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Runtime Fraction','',aggTypeMinimum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Total Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Sensible Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Latent Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Electric Power','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Crankcase Heater Electric Power','',aggTypeMaximum)
END IF
IF (NamedMonthly(39)%show) THEN
  curReport = AddMonthlyReport('WindowReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Transmitted Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Transmitted Beam Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Transmitted Diffuse Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Heat Gain Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Heat Loss Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Inside Face Glazing Condensation Status','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Shading Device Is On Time Fraction','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Storm Window On Off Status','',aggTypeHoursNonZero)
END IF
IF (NamedMonthly(40)%show) THEN
  curReport = AddMonthlyReport('WindowEnergyReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Transmitted Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Transmitted Beam Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Transmitted Diffuse Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Heat Gain Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Surface Window Heat Loss Energy','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(41)%show) THEN
  curReport = AddMonthlyReport('WindowZoneSummaryMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Windows Total Heat Gain Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Windows Total Heat Loss Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Windows Total Transmitted Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Exterior Windows Total Transmitted Beam Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Interior Windows Total Transmitted Diffuse Solar Radiation Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Interior Windows Total Transmitted Beam Solar Radiation Rate','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(42)%show) THEN
  curReport = AddMonthlyReport('WindowEnergyZoneSummaryMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Windows Total Heat Gain Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Windows Total Heat Loss Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Windows Total Transmitted Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Exterior Windows Total Transmitted Beam Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Interior Windows Total Transmitted Diffuse Solar Radiation Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Interior Windows Total Transmitted Beam Solar Radiation Energy','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(43)%show) THEN
  curReport = AddMonthlyReport('AverageOutdoorConditionsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Dewpoint Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Wind Speed','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Sky Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Diffuse Solar Radiation Rate per Area','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Direct Solar Radiation Rate per Area','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Rain Status','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(44)%show) THEN
  curReport = AddMonthlyReport('OutdoorConditionsMaximumDryBulbMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Dewpoint Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Wind Speed','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Sky Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Diffuse Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Direct Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(45)%show) THEN
  curReport = AddMonthlyReport('OutdoorConditionsMinimumDryBulbMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeMinimum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Dewpoint Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Wind Speed','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Sky Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Diffuse Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Direct Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(46)%show) THEN
  curReport = AddMonthlyReport('OutdoorConditionsMaximumWetBulbMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Dewpoint Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Wind Speed','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Sky Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Diffuse Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Direct Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(47)%show) THEN
  curReport = AddMonthlyReport('OutdoorConditionsMaximumDewPointMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Dewpoint Temperature','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Drybulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Outdoor Air Wetbulb Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Wind Speed','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Sky Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Diffuse Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Site Direct Solar Radiation Rate per Area','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(48)%show) THEN
  curReport = AddMonthlyReport('OutdoorGroundConditionsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Ground Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Surface Ground Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Deep Ground Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Mains Water Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Ground Reflected Solar Radiation Rate per Area','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Site Snow on Ground Status','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(49)%show) THEN
  curReport = AddMonthlyReport('WindowACReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Total Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Total Cooling Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Sensible Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Latent Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Total Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Sensible Cooling Rate','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Latent Cooling Rate','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Window Air Conditioner Electric Power','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(50)%show) THEN
  curReport = AddMonthlyReport('WaterHeaterReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Total Demand Heat Transfer Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Use Side Heat Transfer Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Burner Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Gas Consumption','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Total Demand Heat Transfer Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Loss Demand Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Heat Loss Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Tank Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Heat Recovery Supply Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Water Heater Source Energy','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(51)%show) THEN
  curReport = AddMonthlyReport('GeneratorReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Produced Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Diesel Consumption','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Gas Consumption','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Produced Electric Energy','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Total Heat Recovery','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Jacket Heat Recovery Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Lube Heat Recovery','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Exhaust Heat Recovery Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Generator Exhaust Air Temperature','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(52)%show) THEN
  curReport = AddMonthlyReport('DaylightingReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Site Exterior Beam Normal Illuminance','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Lighting Power Multiplier','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Lighting Power Multiplier','',aggTypeMinimumDuringHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Reference Point 1 Illuminance','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Reference Point 1 Glare Index','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time','', aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time','', aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Reference Point 2 Illuminance','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Reference Point 2 Glare Index','',aggTypeSumOrAverageHoursShown)
  CALL AddMonthlyFieldSetInput(curReport,'Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time','', aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time','', aggTypeSumOrAvg)
END IF
IF (NamedMonthly(53)%show) THEN
  curReport = AddMonthlyReport('CoilReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Heating Coil Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Heating Coil Heating Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Total Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Sensible Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Total Cooling Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Sensible Cooling Rate','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Cooling Coil Wetted Area Fraction','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(54)%show) THEN
  curReport = AddMonthlyReport('PlantLoopDemandReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Cooling Demand Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Cooling Demand Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Heating Demand Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Heating Demand Rate','',aggTypeMaximum)
END IF
IF (NamedMonthly(55)%show) THEN
  curReport = AddMonthlyReport('FanReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Fan Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Fan Rise in Air Temperature','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Fan Electric Power','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Fan Rise in Air Temperature','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(56)%show) THEN
  curReport = AddMonthlyReport('PumpReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Fluid Heat Gain Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Electric Power','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Shaft Power','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Fluid Heat Gain Rate','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Outlet Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Pump Mass Flow Rate','',aggTypeValueWhenMaxMin)
END IF
IF (NamedMonthly(57)%show) THEN
  curReport = AddMonthlyReport('CondLoopDemandReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Cooling Demand Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Cooling Demand Rate','',aggTypeMaximum)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Inlet Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Outlet Temperature','',aggTypeValueWhenMaxMin)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Heating Demand Rate','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Plant Supply Side Heating Demand Rate','',aggTypeMaximum)
END IF
IF (NamedMonthly(58)%show) THEN
  curReport = AddMonthlyReport('ZoneTemperatureOscillationReportMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Oscillating Temperatures Time','',aggTypeHoursNonZero)
  CALL AddMonthlyFieldSetInput(curReport,'Zone People Occupant Count','',aggTypeSumOrAverageHoursShown)
END IF
IF (NamedMonthly(59)%show) THEN
  curReport = AddMonthlyReport('AirLoopSystemEnergyAndWaterUseMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Hot Water Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Steam Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Chilled Water Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Gas Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Water Volume','',aggTypeSumOrAvg)
END IF

IF (NamedMonthly(60)%show) THEN
  curReport = AddMonthlyReport('AirLoopSystemComponentLoadsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Fan Air Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Cooling Coil Total Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heating Coil Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heat Exchanger Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heat Exchanger Total Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Humidifier Total Heating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Evaporative Cooler Total Cooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Desiccant Dehumidifier Total Cooling Energy','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(61)%show) THEN
  curReport = AddMonthlyReport('AirLoopSystemComponentEnergyUseMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Fan Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heating Coil Hot Water Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Cooling Coil Chilled Water Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System DX Heating Coil Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System DX Cooling Coil Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heating Coil Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heating Coil Gas Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Heating Coil Steam Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Humidifier Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Evaporative Cooler Electric Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Air System Desiccant Dehumidifier Electric Energy','',aggTypeSumOrAvg)
END IF
IF (NamedMonthly(62)%show) THEN
  curReport = AddMonthlyReport('MechanicalVentilationLoadsMonthly',2)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation No Load Heat Removal Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation Cooling Load Increase Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Mechanical Ventilation Cooling Load Increase Due to Overheating Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation Cooling Load Decrease Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation No Load Heat Addition Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation Heating Load Increase Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,  &
     'Zone Mechanical Ventilation Heating Load Increase Due to Overcooling Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation Heating Load Decrease Energy','',aggTypeSumOrAvg)
  CALL AddMonthlyFieldSetInput(curReport,'Zone Mechanical Ventilation Air Changes per Hour','',aggTypeSumOrAvg)
END IF
END SUBROUTINE CreatePredefinedMonthlyReports

SUBROUTINE GetInputFuelAndPollutionFactors
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   January 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Read the Fuel Factor inputs by the user to
          !   get the source energy conversion factors
          !   Also reads PolutionCalculationFactors to
          !   get information on district cooling and heating

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE PollutionModule, ONLY: GetFuelFactorInfo, GetEnvironmentalImpactFactorInfo

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)   :: curSourceFactor
LOGICAL     :: fuelFactorUsed
LOGICAL     :: fFScheduleUsed
INTEGER     :: ffScheduleIndex

!set the default factors for source energy - they will be overwritten if the user sets any values
sourceFactorElectric         = 3.167d0
sourceFactorNaturalGas       = 1.084d0
sourceFactorSteam            = 0.3d0
sourceFactorGasoline         = 1.05d0
sourceFactorDiesel           = 1.05d0
sourceFactorCoal             = 1.05d0
sourceFactorFuelOil1         = 1.05d0
sourceFactorFuelOil2         = 1.05d0
sourceFactorPropane          = 1.05d0
sourceFactorOtherFuel1       = 1.0d0
sourceFactorOtherFuel2       = 1.0d0
! the following should be kept consistent with the assumptions in the pollution calculation routines
efficiencyDistrictCooling   = 3.0d0
efficiencyDistrictHeating   = 0.3d0


!  TotalSourceEnergyUse = (gatherTotalsSource(1) & !total source from electricity
!                  +  gatherTotalsSource(2)   & !natural gas
!                  + gatherTotalsSource(3)    & !gasoline
!                  + gatherTotalsSource(4)    & !diesel
!                  + gatherTotalsSource(5)    & !coal
!                  + gatherTotalsSource(6)    & !fuel oil #1
!                  + gatherTotalsSource(7)    & !fuel oil #2
!                  + gatherTotalsSource(8)    &  !propane
!                  + gatherTotalsBEPS(3)*sourceFactorElectric/efficiencyDistrictCooling  & !district cooling
!                  + gatherTotalsBEPS(4)*sourceFactorNaturalGas/efficiencyDistrictHeating  & !district heating
!                  + gatherTotalsBEPS(5)*sourceFactorSteam  & !steam
!                                          ) / largeConversionFactor

    CALL GetFuelFactorInfo('NaturalGas',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorNaturalGas = curSourceFactor
      fuelfactorsused(2)=.true.
      ffUsed(2)=.true.
    ENDIF
    SourceFactors(2) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(2) = .TRUE.
      ffSchedIndex(2) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('ResidualOil',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorFuelOil2 = curSourceFactor
      fuelfactorsused(7)=.true.
      ffUsed(11)=.true.
    ENDIF
    SourceFactors(11) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(11) = .TRUE.
      ffSchedIndex(11) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('DistillateOil',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorFuelOil1 = curSourceFactor
      fuelfactorsused(6)=.true.
      ffUsed(10)=.true.
    ENDIF
    SourceFactors(10) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(10) = .TRUE.
      ffSchedIndex(10) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('Coal',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorCoal = curSourceFactor
      fuelfactorsused(5)=.true.
      ffUsed(9)=.true.
    ENDIF
    SourceFactors(9) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(9) = .TRUE.
      ffSchedIndex(9) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('Electricity',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorElectric = curSourceFactor
      fuelfactorsused(1)=.true.
      ffUsed(1)=.true.
    ENDIF
    SourceFactors(1) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(1) = .TRUE.
      ffSchedIndex(1) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('Gasoline',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorGasoline = curSourceFactor
      fuelfactorsused(3)=.true.
      ffUsed(6)=.true.
    ENDIF
    SourceFactors(6) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(6) = .TRUE.
      ffSchedIndex(6) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('Propane',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorPropane = curSourceFactor
      fuelfactorsused(8)=.true.
      ffUsed(12)=.true.
    ENDIF
    SourceFactors(12) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(12) = .TRUE.
      ffSchedIndex(12) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('Diesel',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorDiesel = curSourceFactor
      fuelfactorsused(4)=.true.
      ffUsed(8)=.true.
    ENDIF
    SourceFactors(8) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(8) = .TRUE.
      ffSchedIndex(8) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('DistrictCooling',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      ffUsed(3)=.true.
    ENDIF
    SourceFactors(3) = curSourceFactor
    IF (fFScheduleUsed) THEN
      ffSchedUsed(3) = .TRUE.
      ffSchedIndex(3) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('DistrictHeating',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      ffUsed(4)=.true.
    ENDIF
    SourceFactors(4) = curSourceFactor
    IF (fFScheduleUsed) THEN
      ffSchedUsed(4) = .TRUE.
      ffSchedIndex(4) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('Steam',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      ffUsed(5)=.true.
    ENDIF
    SourceFactors(5) = curSourceFactor
    IF (fFScheduleUsed) THEN
      ffSchedUsed(5) = .TRUE.
      ffSchedIndex(5) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('OtherFuel1',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorOtherFuel1 = curSourceFactor
      fuelfactorsused(11)=.true.  ! should be source number
      ffUsed(13)=.true.
    ENDIF
    SourceFactors(13) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(13) = .TRUE.
      ffSchedIndex(13) = ffScheduleIndex
    ENDIF

    CALL GetFuelFactorInfo('OtherFuel2',fuelFactorUsed,curSourceFactor,fFScheduleUsed,ffScheduleIndex)
    IF (fuelFactorUsed) THEN
      sourceFactorOtherFuel2 = curSourceFactor
      fuelfactorsused(12)=.true.  ! should be source number
      ffUsed(14)=.true.
    ENDIF
    SourceFactors(14) = curSourceFactor
    IF (fFScheduleUsed) THEN
      fuelFactorSchedulesUsed=.true.
      ffSchedUsed(14) = .TRUE.
      ffSchedIndex(14) = ffScheduleIndex
    ENDIF

    CALL GetEnvironmentalImpactFactorInfo(efficiencyDistrictHeating,efficiencyDistrictCooling,sourceFactorSteam)

END SUBROUTINE GetInputFuelAndPollutionFactors


!======================================================================================================================
!======================================================================================================================
!
!
!    OTHER INITIALIZATION ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE OpenOutputTabularFile
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Create a file that holds the output from the tabular reports
          !   the output is in a CSV file if it is comma delimited otherwise
          !   it is in a TXT file.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataStringGlobals, ONLY : VerString
USE DataEnvironment,   ONLY : EnvironmentName, WeatherFileLocationTitle
USE DataHeatBalance,   ONLY : BuildingName

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"
CHARACTER(len=*), PARAMETER :: TimeStampFmt1="(A,I4,A,I2.2,A,I2.2)"
CHARACTER(len=*), PARAMETER :: TimeStampFmt2="(A,I2.2,A,I2.2,A,I2.2,A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER,EXTERNAL :: GetNewUnitNumber  ! Function to call if file not opened
INTEGER :: iStyle
INTEGER :: curFH  !current file handle
CHARACTER(LEN=1) :: curDel
INTEGER :: write_stat

! get a new file unit number
! create a file to hold the results
! Use a CSV file if comma seperated but otherwise use TXT file
! extension.
IF (WriteTabularFiles) THEN
  DO iStyle = 1, numStyles
    TabularOutputFile(iStyle) = GetNewUnitNumber()
    curFH = TabularOutputFile(iStyle)
    curDel = del(iStyle)
    IF (tableStyle(iStyle) .eq. tableStyleComma) THEN
      CALL DisplayString('Writing tabular output file results using comma format.')
      OPEN(UNIT=curFH,FILE='eplustbl.csv',action='WRITE',iostat=write_stat)
      IF (write_stat /= 0) THEN
       CALL ShowFatalError('OpenOutputTabularFile: Could not open file "eplustbl.csv" for output (write).')
      ENDIF
      WRITE(curFH,fmta) 'Program Version:'// curDel //TRIM(VerString)
      WRITE(curFH,*) 'Tabular Output Report in Format: '// curDel // 'Comma'
      WRITE(curFH,fmta) ''
      WRITE(curFH,fmta) 'Building:'    // curDel //TRIM(BuildingName)
      IF (EnvironmentName == WeatherFileLocationTitle) THEN
        WRITE(curFH,fmta) 'Environment:' // curDel //TRIM(EnvironmentName)
      ELSE
        WRITE(curFH,fmta) 'Environment:' // curDel //TRIM(EnvironmentName)//' ** '//TRIM(WeatherFileLocationTitle)
      ENDIF
      WRITE(curFH,fmta) ''
    ELSEIF (tableStyle(iStyle) .eq. tableStyleTab) THEN
      CALL DisplayString('Writing tabular output file results using tab format.')
      OPEN(curFH,FILE='eplustbl.tab',action='WRITE',iostat=write_stat)
      IF (write_stat /= 0) THEN
       CALL ShowFatalError('OpenOutputTabularFile: Could not open file "eplustbl.tab" for output (write).')
      ENDIF
      WRITE(curFH,fmta) 'Program Version'// curDel //TRIM(VerString)
      WRITE(curFH,fmta) 'Tabular Output Report in Format: '// curDel // 'Tab'
      WRITE(curFH,fmta) ''
      WRITE(curFH,fmta) 'Building:'    // curDel //TRIM(BuildingName)
      IF (EnvironmentName == WeatherFileLocationTitle) THEN
        WRITE(curFH,fmta) 'Environment:' // curDel //TRIM(EnvironmentName)
      ELSE
        WRITE(curFH,fmta) 'Environment:' // curDel //TRIM(EnvironmentName)//' ** '//TRIM(WeatherFileLocationTitle)
      ENDIF
      WRITE(curFH,fmta) ''
    ELSEIF (tableStyle(iStyle) .eq. tableStyleHTML) THEN
      CALL DisplayString('Writing tabular output file results using HTML format.')
      OPEN(curFH,FILE='eplustbl.htm',action='WRITE',iostat=write_stat)
      IF (write_stat /= 0) THEN
       CALL ShowFatalError('OpenOutputTabularFile: Could not open file "eplustbl.htm" for output (write).')
      ENDIF
      WRITE(curFH,fmta) '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"' // &
                                  '"http://www.w3.org/TR/html4/loose.dtd">'
      WRITE(curFH,fmta) '<html>'
      WRITE(curFH,fmta) '<head>'
      IF (EnvironmentName == WeatherFileLocationTitle) THEN
        WRITE(curFH,fmta) '<title> ' // TRIM(BuildingName) // ' ' // TRIM(EnvironmentName)
      ELSE
        WRITE(curFH,fmta) '<title> ' // TRIM(BuildingName) // ' ' // TRIM(EnvironmentName)//' ** '//TRIM(WeatherFileLocationTitle)
      ENDIF
      WRITE(curFH,TimeStampFmt1) '  ',td(1),'-', td(2),'-',td(3)
      WRITE(curFH,TimeStampFmt2) '  ',td(5),':', td(6),':',td(7),' '
      WRITE(curFH,fmta) ' - EnergyPlus</title>'
      WRITE(curFH,fmta) '</head>'
      WRITE(curFH,fmta) '<body>'
      WRITE(curFH,fmta) '<p><a href="#toc" style="float: right">Table of Contents</a></p>'
      WRITE(curFH,fmta) '<a name=top></a>'
      WRITE(curFH,fmta) '<p>Program Version:<b>'// TRIM(VerString) // '</b></p>'
      WRITE(curFH,fmta) '<p>Tabular Output Report in Format: <b>HTML</b></p>'
      WRITE(curFH,fmta) '<p>Building: <b>' //TRIM(BuildingName) // '</b></p>'
      IF (EnvironmentName == WeatherFileLocationTitle) THEN
        WRITE(curFH,fmta) '<p>Environment: <b>' //TRIM(EnvironmentName) // '</b></p>'
      ELSE
        WRITE(curFH,fmta) '<p>Environment: <b>' //TRIM(EnvironmentName)//' ** '//TRIM(WeatherFileLocationTitle)// '</b></p>'
      ENDIF
      WRITE(curFH,TimeStampFmt1) "<p>Simulation Timestamp: <b>", td(1),'-', td(2),'-',td(3)
      WRITE(curFH,TimeStampFmt2) '  ',td(5),':', td(6),':',td(7),'</b></p>'
    ELSEIF (tableStyle(iStyle) .eq. tableStyleXML) THEN
      CALL DisplayString('Writing tabular output file results using XML format.')
      OPEN(curFH,FILE='eplustbl.xml',action='WRITE',iostat=write_stat)
      IF (write_stat /= 0) THEN
       CALL ShowFatalError('OpenOutputTabularFile: Could not open file "eplustbl.xml" for output (write).')
      ENDIF
      WRITE(curFH,fmta) '<?xml version="1.0"?>'
      WRITE(curFH,fmta) '<EnergyPlusTabularReports>'
      WRITE(curFH,fmta) '  <BuildingName>' // TRIM(BuildingName) // '</BuildingName>'
      WRITE(curFH,fmta) '  <EnvironmentName>' // TRIM(EnvironmentName) // '</EnvironmentName>'
      WRITE(curFH,fmta) '  <WeatherFileLocationTitle>' //TRIM(WeatherFileLocationTitle) // '</WeatherFileLocationTitle>'
      WRITE(curFH,fmta) '  <ProgramVersion>'// TRIM(VerString) // '</ProgramVersion>'
      WRITE(curFH,fmta) '  <SimulationTimestamp>'
      WRITE(curFH,fmta) '    <Date>'
      WRITE(curFH,TimeStampFmt1) '      ', td(1),'-', td(2),'-',td(3)
      WRITE(curFH,fmta) '    </Date>'
      WRITE(curFH,fmta) '    <Time>'
      WRITE(curFH,TimeStampFmt2) '      ',td(5),':', td(6),':',td(7),' '
      WRITE(curFH,fmta) '    </Time>'
      WRITE(curFH,fmta) '  </SimulationTimestamp>'
      WRITE(curFH,fmta) ' '
    ELSE
      CALL DisplayString('Writing tabular output file results using text format.')
      OPEN(curFH,File='eplustbl.txt', Action='write',iostat=write_stat)
      IF (write_stat /= 0) THEN
       CALL ShowFatalError('OpenOutputTabularFile: Could not open file "eplustbl.txt" for output (write).')
      ENDIF
      WRITE(curFH,fmta) 'Program Version: ' //TRIM(VerString)
      WRITE(curFH,fmta) 'Tabular Output Report in Format: '// curDel // 'Fixed'
      WRITE(curFH,fmta) ''
      WRITE(curFH,fmta) 'Building:        ' //TRIM(BuildingName)
      IF (EnvironmentName == WeatherFileLocationTitle) THEN
        WRITE(curFH,fmta) 'Environment:     ' //TRIM(EnvironmentName)
      ELSE
        WRITE(curFH,fmta) 'Environment:     ' //TRIM(EnvironmentName)//' ** '//TRIM(WeatherFileLocationTitle)
      ENDIF
      WRITE(curFH,fmta) ''
    END IF
  END DO
END IF
END SUBROUTINE OpenOutputTabularFile

SUBROUTINE CloseOutputTabularFile
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Create a file that holds the output from the tabular reports
          !   the output is in a CSV file if it is comma delimited otherwise
          !   it is in a TXT file.

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iStyle

IF (WriteTabularFiles) THEN
  DO iStyle = 1, numStyles
    ! if HTML file put ending info
    IF (tableStyle(iStyle) .eq. tableStyleHTML) THEN
      WRITE(TabularOutputFile(iStyle),fmta) '</body>'
      WRITE(TabularOutputFile(iStyle),fmta) '</html>'
    ELSEIF (tableStyle(iStyle) .eq. tableStyleXML) THEN
      IF (LEN_TRIM(prevReportName) .NE. 0) THEN
        WRITE(TabularOutputFile(iStyle),fmta) '</' // TRIM(prevReportName) //'>'  !close the last element if it was used.
      END IF
      WRITE(TabularOutputFile(iStyle),fmta) '</EnergyPlusTabularReports>'
    END IF
    CLOSE(TabularOutputFile(iStyle))
  END DO
END IF
END SUBROUTINE CloseOutputTabularFile


SUBROUTINE WriteTableOfContents
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   June 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Creates hyperlinks for table of contents

          ! METHODOLOGY EMPLOYED:
          !   Go through the reports and create links

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportPredefined, ONLY: reportName, numReportName
USE DataCostEstimate, ONLY: DoCostEstimate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER    :: iInput
INTEGER    :: jTable
INTEGER    :: curTable
INTEGER    :: iEntry
INTEGER    :: jEntry
INTEGER    :: kReport
CHARACTER(len=MaxNameLength)   :: curSection
INTEGER    :: iStyle
INTEGER    :: curFH
CHARACTER(len=MaxNameLength)   :: origName
CHARACTER(len=MaxNameLength)   :: curName
INTEGER    :: indexUnitConv

DO iStyle = 1, numStyles
  IF (tableStyle(iStyle) .eq. tableStyleHTML) THEN
    curFH = TabularOutputFile(iStyle)
    WRITE(curFH,fmta) '<hr>'
    WRITE(curFH,fmta) '<a name=toc></a>'
    WRITE(curFH,fmta) '<p><b>Table of Contents</b></p>'
    WRITE(curFH,fmta) '<a href="#top">Top</a>'
    IF (displayTabularBEPS) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Annual Building Utility Performance Summary', &
                          'Entire Facility')) // '">Annual Building Utility Performance Summary</a>'
    ENDIF
    IF (displayTabularVeriSum) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Input Verification and Results Summary', &
                          'Entire Facility')) // '">Input Verification and Results Summary</a>'
    ENDIF
    IF (displayDemandEndUse) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Demand End Use Components Summary', &
                          'Entire Facility')) // '">Demand End Use Components Summary</a>'
    ENDIF
    IF (displaySourceEnergyEndUseSummary) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Source Energy End Use Components Summary', &
                          'Entire Facility')) // '">Source Energy End Use Components Summary</a>'
    ENDIF
    IF (DoCostEstimate) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Component Cost Economics Summary', &
                          'Entire Facility')) // '">Component Cost Economics Summary</a>'
    ENDIF
    IF (displayComponentSizing) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Component Sizing Summary', &
                          'Entire Facility')) // '">Component Sizing Summary</a>'
    END IF
    IF (displaySurfaceShadowing) THEN
      WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName('Surface Shadowing Summary', &
                          'Entire Facility')) // '">Surface Shadowing Summary</a>'
    END IF
    DO kReport = 1, numReportName
      IF (reportName(kReport)%show) THEN
        WRITE(curFH,fmta) '<br><a href="#' // TRIM(MakeAnchorName(TRIM(reportName(kReport)%namewithSpaces), &
                          'Entire Facility')) // '">'//TRIM(reportName(kReport)%namewithSpaces)//'</a>'
      END IF
    END DO
    IF (DoWeathSim) THEN
      DO iInput = 1, MonthlyInputCount
        IF (MonthlyInput(iInput)%numTables .GT. 0) THEN
          WRITE(curFH,fmta) '<p><b>' // TRIM(MonthlyInput(iInput)%name) // '</b></p> |'
          DO jTable = 1 , MonthlyInput(iInput)%numTables
            curTable =jTable + MonthlyInput(iInput)%firstTable - 1
            WRITE(curFH,fmta) '<a href="#' // TRIM(MakeAnchorName(MonthlyInput(iInput)%name, &
                            MonthlyTables(curTable)%keyValue)) // '">' // TRIM(MonthlyTables(curTable)%keyValue) //  '</a>    |   '
          END DO
        END IF
      END DO
      DO iInput = 1 , OutputTableBinnedCount
        IF (OutputTableBinned(iInput)%numTables .GT. 0) THEN
          IF (OutputTableBinned(iInput)%scheduleIndex == 0) THEN
            WRITE(curFH,fmta) '<p><b>' // TRIM(OutputTableBinned(iInput)%varOrMeter) // '</b></p> |'
          ELSE
            WRITE(curFH,fmta) '<p><b>' // TRIM(OutputTableBinned(iInput)%varOrMeter) //  &
                                    ' [' // TRIM(OutputTableBinned(iInput)%ScheduleName) // ']' // '</b></p> |'
          ENDIF
          DO jTable = 1, OutputTableBinned(iInput)%numTables
            curTable = OutputTableBinned(iInput)%resIndex + (jTable - 1)
            curName = ''
            IF (unitsStyle .EQ. unitsStyleInchPound) THEN
              origName = TRIM(OutputTableBinned(iInput)%varOrMeter)//' ['//TRIM(OutputTableBinned(iInput)%units) //']'
              CALL LookupSItoIP(origName, indexUnitConv, curName)
            ELSE
              curName = TRIM(OutputTableBinned(iInput)%varOrMeter)//' ['//TRIM(OutputTableBinned(iInput)%units) //']'
            END IF
            IF (OutputTableBinned(iInput)%scheduleIndex == 0) THEN
              WRITE(curFH,fmta) '<a href="#' // TRIM(MakeAnchorName(TRIM(curName), BinObjVarID(curTable)%namesOfObj)) // '">' // &
                              TRIM(BinObjVarID(curTable)%namesOfObj) //  '</a>   |  '
            ELSE
              WRITE(curFH,fmta) '<a href="#' // TRIM(MakeAnchorName(TRIM(curName)  //   &
                              TRIM(OutputTableBinned(iInput)%ScheduleName),   &
                              BinObjVarID(curTable)%namesOfObj)) // '">' // &
                              TRIM(BinObjVarID(curTable)%namesOfObj) //  '</a>   |  '
            ENDIF
          END DO
        END IF
      END DO
    END IF
    !add entries specifically added using AddTOCEntry
    DO iEntry = 1, TOCEntriesCount
      IF (.NOT. TOCEntries(iEntry)%isWritten) THEN
        curSection = TOCEntries(iEntry)%sectionName
        WRITE(curFH,fmta) '<p><b>' // TRIM(curSection) // '</b></p> |'
        DO jEntry = iEntry, TOCEntriesCount
          IF (.NOT. TOCEntries(jEntry)%isWritten) THEN
            IF (TOCEntries(jEntry)%sectionName .EQ. curSection) THEN
              WRITE(curFH,fmta) '<a href="#' // TRIM(MakeAnchorName(TOCEntries(jEntry)%sectionName, &
                          TOCEntries(jEntry)%reportName)) // '">' // &
                          TRIM(TOCEntries(jEntry)%reportName) //  '</a>   |  '
              TOCEntries(jEntry)%isWritten = .TRUE.
            END IF
          END IF
        END DO
      END IF
    END DO
  END IF
END DO
END SUBROUTINE WriteTableOfContents


!======================================================================================================================
!======================================================================================================================
!
!
!    GATHER DATA EACH TIME STEP ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE GatherBinResultsForTimestep(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gathers the data each timesetp and adds the length of the
          !   timestep to the appropriate bin.

          ! USE STATEMENTS:
USE DataHVACGlobals, ONLY: TimeStepSys
USE DataEnvironment, ONLY: Month
USE ScheduleManager, ONLY: GetCurrentScheduleValue

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER  :: iInObj
INTEGER  :: jTable
REAL(r64)     :: curValue
! values of OutputTableBinned array for current index
REAL(r64)     :: curIntervalStart
REAL(r64)     :: curIntervalSize
INTEGER  :: curIntervalCount
INTEGER  :: curResIndex
INTEGER  :: curNumTables
INTEGER  :: curTypeOfVar
INTEGER  :: curScheduleIndex
REAL(r64)     :: elapsedTime
LOGICAL  :: gatherThisTime
!
REAL(r64)     :: topValue
INTEGER  :: binNum
INTEGER  :: repIndex
INTEGER  :: curStepType

!REAL(r64), external :: GetInternalVariableValue

IF (.NOT. DoWeathSim) RETURN
elapsedTime = TimeStepSys
timeInYear = timeInYear + elapsedTime
DO iInObj = 1, OutputTableBinnedCount
  ! get values of array for current object being referenced
  curIntervalStart = OutputTableBinned(iInObj)%intervalStart
  curIntervalSize = OutputTableBinned(iInObj)%intervalSize
  curIntervalCount = OutputTableBinned(iInObj)%intervalCount
  curResIndex = OutputTableBinned(iInObj)%resIndex
  curNumTables = OutputTableBinned(iInObj)%numTables
  topValue = curIntervalStart + curIntervalSize * curIntervalCount
  curTypeOfVar = OutputTableBinned(iInObj)%typeOfVar
  curStepType = OutputTableBinned(iInObj)%stepType
  curScheduleIndex = OutputTableBinned(iInObj)%scheduleIndex
  !if a schedule was used, check if it was non-zero value
  IF (curScheduleIndex .NE. 0) THEN
    IF (GetCurrentScheduleValue(curScheduleIndex) .NE. 0.0d0) THEN
      gatherThisTime = .TRUE.
    ELSE
      gatherThisTime = .FALSE.
    END IF
  ELSE
    gatherThisTime = .TRUE.
  END IF
  IF (gatherThisTime) THEN
    DO jTable = 1, curNumTables
      repIndex = curResIndex + (jTable - 1)
      IF (((curStepType .EQ. stepTypeZone) .AND. (IndexTypeKey .EQ. ZoneTSReporting)) .OR.   &
        ((curStepType .EQ. stepTypeHVAC) .AND. (IndexTypeKey .EQ. HVACTSReporting))) THEN
        ! put actual value from OutputProcesser arrays
        curValue = GetInternalVariableValue(curTypeOfVar,BinObjVarID(repIndex)%varMeterNum)
        ! per MJW when a summed variable is used divide it by the length of the time step
        IF (IndexTypeKey .EQ. HVACTSReporting) THEN
          elapsedTime = TimeStepSys
        ELSE
          elapsedTime = TimeStepZone
        ENDIF
        IF (OutputTableBinned(iInObj)%avgSum .EQ. isSum) THEN ! if it is a summed variable
          curValue = curValue / (elapsedTime * SecInHour)
        END IF
        ! check if the value is above the maximum or below the minimum value
        ! first before binning the value within the range.
        IF (curValue .LT. curIntervalStart) THEN
          BinResultsBelow(repIndex)%mnth(month) = BinResultsBelow(repIndex)%mnth(month) + elapsedTime
          BinResultsBelow(repIndex)%hrly(HourOfDay) = BinResultsBelow(repIndex)%hrly(HourOfDay) + elapsedTime
        ELSE IF (curValue .GE. topValue) THEN
          BinResultsAbove(repIndex)%mnth(month) = BinResultsAbove(repIndex)%mnth(month) + elapsedTime
          BinResultsAbove(repIndex)%hrly(HourOfDay) = BinResultsAbove(repIndex)%hrly(HourOfDay) + elapsedTime
        ELSE
          ! determine which bin the results are in
          binNum = INT((curValue - curIntervalStart) / curIntervalSize) + 1
          BinResults(repIndex,binNum)%mnth(month) = BinResults(repIndex,binNum)%mnth(month) + elapsedTime
          BinResults(repIndex,binNum)%hrly(HourOfDay) = BinResults(repIndex,binNum)%hrly(HourOfDay) + elapsedTime
        END IF
        ! add to statistics array
        BinStatistics(repIndex)%n = BinStatistics(repIndex)%n + 1
        BinStatistics(repIndex)%sum = BinStatistics(repIndex)%sum + curValue
        BinStatistics(repIndex)%sum2 = BinStatistics(repIndex)%sum2 + curValue * curValue
        IF (curValue .LT. BinStatistics(repIndex)%minimum) THEN
          BinStatistics(repIndex)%minimum = curValue
        ENDIF
        IF (curValue .GT. BinStatistics(repIndex)%maximum) THEN
          BinStatistics(repIndex)%maximum = curValue
        ENDIF
      END IF
    END DO
  END IF
END DO
END SUBROUTINE GatherBinResultsForTimestep

SUBROUTINE GatherMonthlyResultsForTimestep(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gathers the data each timestep and updates the arrays
          !   holding the data that will be reported later.

          ! USE STATEMENTS:
USE DataHVACGlobals, ONLY: TimeStepSys,SysTimeElapsed
USE DataEnvironment, ONLY: Month, DayOfMonth
USE General, ONLY: EncodeMonDayHrMin,DetermineMinuteForReporting

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER  :: iTable    ! loop variable for monthlyTables
INTEGER  :: jColumn   ! loop variable for monthlyColumns
INTEGER  :: curCol
REAL(r64)     :: curValue
INTEGER  :: curTypeOfVar
INTEGER  :: curVarNum
REAL(r64)     :: elapsedTime
REAL(r64)     :: oldResultValue
INTEGER  :: oldTimeStamp
REAL(r64)     :: oldDuration
REAL(r64)     :: newResultValue
INTEGER  :: newTimeStamp
REAL(r64)     :: newDuration
INTEGER  :: timestepTimeStamp
LOGICAL  :: activeMinMax
!LOGICAL,SAVE  :: activeHoursShown=.false.  !fix by LKL addressing CR6482
LOGICAL  :: activeHoursShown
LOGICAL  :: activeNewValue
INTEGER  :: curStepType
INTEGER  :: minuteCalculated
INTEGER  :: kOtherColumn   ! variable used in loop to scan through additional columns
INTEGER  :: scanColumn
REAL(r64)     :: scanValue
INTEGER  :: scanTypeOfVar
INTEGER  :: scanVarNum
REAL(r64)     :: oldScanValue
! local copies of some of the MonthlyColumns array references since
! profiling showed that they were slow.
LOGICAL, SAVE     :: RunOnce = .TRUE.
INTEGER,ALLOCATABLE,DIMENSION(:), SAVE :: MonthlyColumnsTypeOfVar
INTEGER,ALLOCATABLE,DIMENSION(:), SAVE :: MonthlyColumnsStepType
INTEGER,ALLOCATABLE,DIMENSION(:), SAVE :: MonthlyColumnsAggType
INTEGER,ALLOCATABLE,DIMENSION(:), SAVE :: MonthlyColumnsVarNum
INTEGER,ALLOCATABLE,DIMENSION(:), SAVE :: MonthlyTablesNumColumns
INTEGER :: curFirstColumn = 0

IF (.NOT. DoWeathSim) RETURN

!create temporary arrays to speed processing of these arrays
IF (RunOnce) THEN
  !MonthlyColumns
  ALLOCATE (MonthlyColumnsTypeOfVar(MonthlyColumnsCount))
  MonthlyColumnsTypeOfVar = MonthlyColumns%typeOfVar
  ALLOCATE (MonthlyColumnsStepType(MonthlyColumnsCount))
  MonthlyColumnsStepType = MonthlyColumns%stepType
  ALLOCATE (MonthlyColumnsAggType(MonthlyColumnsCount))
  MonthlyColumnsAggType = MonthlyColumns%aggType
  ALLOCATE (MonthlyColumnsVarNum(MonthlyColumnsCount))
  MonthlyColumnsVarNum = MonthlyColumns%varNum
  !MonthlyTables
  ALLOCATE (MonthlyTablesNumColumns(MonthlyTablesCount))
  MonthlyTablesNumColumns = MonthlyTables%numColumns

  !set flag so this block is only executed once
  RunOnce = .FALSE.
END IF

elapsedTime = TimeStepSys
IF (IndexTypeKey .EQ. HVACTSReporting) THEN
  elapsedTime = TimeStepSys
ELSE
  elapsedTime = TimeStepZone
ENDIF
IsMonthGathered(Month) = .TRUE.
DO iTable = 1, MonthlyTablesCount
  activeMinMax = .FALSE.  !at the beginning of the new timestep
  activeHoursShown = .FALSE.     !fix by JG addressing CR6482
  curFirstColumn = MonthlyTables(iTable)%firstColumn
  DO jColumn = 1, MonthlyTablesNumColumns(iTable)
    curCol = jColumn + curFirstColumn - 1
    curTypeOfVar = MonthlyColumnsTypeOfVar(curCol)
    curStepType = MonthlyColumnsStepType(curCol)
    IF (((curStepType .EQ. stepTypeZone) .AND. (IndexTypeKey .EQ. ZoneTSReporting)) .OR.   &
       ((curStepType .EQ. stepTypeHVAC) .AND. (IndexTypeKey .EQ. HVACTSReporting))) THEN
       !  the above condition used to include the following prior to new scan method
       !  (MonthlyColumns(curCol)%aggType .EQ. aggTypeValueWhenMaxMin)
      curVarNum = MonthlyColumnsVarNum(curCol)
      curValue = GetInternalVariableValue(curTypeOfVar,curVarNum)
      ! Get the value from the result array
      oldResultValue = MonthlyColumns(curCol)%reslt(Month)
      oldTimeStamp = MonthlyColumns(curCol)%timeStamp(Month)
      oldDuration = MonthlyColumns(curCol)%duration(Month)
      ! Zero the revised values (as default if not set later in SELECT)
      newResultValue = 0.0d0
      newTimeStamp = 0
      newDuration = 0.0d0
      activeNewValue = .FALSE.
      ! the current timestamp
      minuteCalculated = DetermineMinuteForReporting(IndexTypeKey)
!      minuteCalculated = (CurrentTime - INT(CurrentTime))*60
!      IF (IndexTypeKey .EQ. stepTypeHVAC) minuteCalculated = minuteCalculated + SysTimeElapsed * 60
!      minuteCalculated = INT((TimeStep-1) * TimeStepZone * 60) + INT((SysTimeElapsed + TimeStepSys) * 60)
      CALL EncodeMonDayHrMin(timestepTimeStamp,Month,DayOfMonth,HourOfDay,minuteCalculated)
      ! perform the selected aggregation type
      ! use next lines since it is faster was: SELECT CASE (MonthlyColumns(curCol)%aggType)
      SELECT CASE (MonthlyColumnsAggType(curCol))
        CASE (aggTypeSumOrAvg)
          IF (MonthlyColumns(curCol)%avgSum .EQ. isSum) THEN ! if it is a summed variable
            newResultValue = oldResultValue + curValue
          ELSE
            newResultValue = oldResultValue + curValue * elapsedTime  !for averaging - weight by elapsed time
          END IF
          newDuration = oldDuration + elapsedTime
          activeNewValue = .TRUE.
        CASE (aggTypeMaximum)
          ! per MJW when a summed variable is used divide it by the length of the time step
          IF (MonthlyColumns(curCol)%avgSum .EQ. isSum) THEN ! if it is a summed variable
            IF (IndexTypeKey .EQ. HVACTSReporting) THEN
              curValue = curValue / (TimeStepSys * SecInHour)
            ELSE
              curValue = curValue / (TimeStepZone * SecInHour)
            ENDIF
          END IF
          IF (curValue .GT. oldResultValue) THEN
            newResultValue = curValue
            newTimeStamp = timestepTimeStamp
            activeMinMax = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeMinMax = .FALSE.  !reset this
          END IF
        CASE (aggTypeMinimum)
          ! per MJW when a summed variable is used divide it by the length of the time step
          IF (MonthlyColumns(curCol)%avgSum .EQ. isSum) THEN ! if it is a summed variable
            IF (IndexTypeKey .EQ. HVACTSReporting) THEN
              curValue = curValue / (TimeStepSys * SecInHour)
            ELSE
              curValue = curValue / (TimeStepZone * SecInHour)
            ENDIF
          END IF
          IF (curValue .LT. oldResultValue) THEN
            newResultValue = curValue
            newTimeStamp = timestepTimeStamp
            activeMinMax = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeMinMax = .FALSE.  !reset this
          END IF
        CASE (aggTypeHoursZero)
          IF (curValue .EQ. 0) THEN
            newResultValue = oldResultValue + elapsedTime
            activeHoursShown = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeHoursShown = .FALSE.
          END IF
        CASE (aggTypeHoursNonZero)
          IF (curValue .NE. 0) THEN
            newResultValue = oldResultValue + elapsedTime
            activeHoursShown = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeHoursShown = .FALSE.
          END IF
        CASE (aggTypeHoursPositive)
          IF (curValue .GT. 0) THEN
            newResultValue = oldResultValue + elapsedTime
            activeHoursShown = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeHoursShown = .FALSE.
          END IF
        CASE (aggTypeHoursNonPositive)
          IF (curValue .LE. 0) THEN
            newResultValue = oldResultValue + elapsedTime
            activeHoursShown = .TRUE.
            activeNewValue = .TRUE.
         ELSE
            activeHoursShown = .FALSE.
          END IF
        CASE (aggTypeHoursNegative)
          IF (curValue .LT. 0) THEN
            newResultValue = oldResultValue + elapsedTime
            activeHoursShown = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeHoursShown = .FALSE.
          END IF
        CASE (aggTypeHoursNonNegative)
          IF (curValue .GE. 0) THEN
            newResultValue = oldResultValue + elapsedTime
            activeHoursShown = .TRUE.
            activeNewValue = .TRUE.
          ELSE
            activeHoursShown = .FALSE.
          END IF
        ! The valueWhenMaxMin is picked up now during the activeMinMax if block below.
        !CASE (aggTypeValueWhenMaxMin)
        !CASE (aggTypeSumOrAverageHoursShown)
        !CASE (aggTypeMaximumDuringHoursShown)
        !CASE (aggTypeMinimumDuringHoursShown)
      END SELECT
      ! if the new value has been set then set the monthly values to the
      ! new columns. This skips the aggregation types that don't even get
      ! triggered now such as valueWhenMinMax and all the agg*HoursShown
      IF (activeNewValue) THEN
        MonthlyColumns(curCol)%reslt(Month) = newResultValue
        MonthlyColumns(curCol)%timeStamp(Month) = newTimeStamp
        MonthlyColumns(curCol)%duration(Month) = newDuration
      END IF
      ! if a minimum or maximum value was set this timeStep then
      ! scan the remaining columns of the table looking for values
      ! that are aggregation type "ValueWhenMaxMin" and set their values
      ! if another minimum or maximum column is found then end
      ! the scan (it will be taken care of when that column is done)
      IF (activeMinMax) THEN
        DO kOtherColumn = jColumn + 1, MonthlyTables(iTable)%numColumns
          scanColumn = kOtherColumn + MonthlyTables(iTable)%firstColumn - 1
          SELECT CASE (MonthlyColumns(scanColumn)%aggType)
            CASE (aggTypeMaximum,aggTypeMinimum)
              ! end scanning since these might reset
              EXIT !do
            CASE (aggTypeValueWhenMaxMin)
              ! this case is when the value should be set
              scanTypeOfVar = MonthlyColumns(scanColumn)%typeOfVar
              scanVarNum = MonthlyColumns(scanColumn)%varNum
              scanValue = GetInternalVariableValue(scanTypeOfVar,scanVarNum)
              ! When a summed variable is used divide it by the length of the time step
              IF (MonthlyColumns(scanColumn)%avgSum .EQ. isSum) THEN ! if it is a summed variable
                IF (IndexTypeKey .EQ. HVACTSReporting) THEN
                  scanValue = scanValue / (TimeStepSys * SecInHour)
                ELSE
                  scanValue = scanValue / (TimeStepZone * SecInHour)
                ENDIF
              END IF
              MonthlyColumns(scanColumn)%reslt(Month) = scanValue
            CASE DEFAULT
              ! do nothing
          END SELECT
        END DO
      END IF
      ! If the hours variable is active then scan through the rest of the variables
      ! and accumulate
      IF (activeHoursShown) THEN
        DO kOtherColumn = jColumn + 1, MonthlyTables(iTable)%numColumns
          scanColumn = kOtherColumn + MonthlyTables(iTable)%firstColumn - 1
          scanTypeOfVar = MonthlyColumns(scanColumn)%typeOfVar
          scanVarNum = MonthlyColumns(scanColumn)%varNum
          scanValue = GetInternalVariableValue(scanTypeOfVar,scanVarNum)
          oldScanValue = MonthlyColumns(scanColumn)%reslt(Month)
          SELECT CASE (MonthlyColumns(scanColumn)%aggType)
            CASE (aggTypeHoursZero,aggTypeHoursNonZero)
              ! end scanning since these might reset
              EXIT !do
            CASE (aggTypeHoursPositive,aggTypeHoursNonPositive)
              ! end scanning since these might reset
              EXIT !do
            CASE (aggTypeHoursNegative,aggTypeHoursNonNegative)
              ! end scanning since these might reset
              EXIT !do
            CASE (aggTypeSumOrAverageHoursShown)
              ! this case is when the value should be set
              IF (MonthlyColumns(scanColumn)%avgSum .EQ. isSum) THEN ! if it is a summed variable
                MonthlyColumns(scanColumn)%reslt(Month) = oldScanValue + scanValue
              ELSE
                !for averaging - weight by elapsed time
                MonthlyColumns(scanColumn)%reslt(Month) = oldScanValue + scanValue * elapsedTime
              END IF
              MonthlyColumns(scanColumn)%duration(Month) = MonthlyColumns(scanColumn)%duration(Month) + elapsedTime
            CASE (aggTypeMaximumDuringHoursShown)
              IF (MonthlyColumns(scanColumn)%avgSum .EQ. isSum) THEN ! if it is a summed variable
                IF (IndexTypeKey .EQ. HVACTSReporting) THEN
                  scanValue = scanValue / (TimeStepSys * SecInHour)
                ELSE
                  scanValue = scanValue / (TimeStepZone * SecInHour)
                ENDIF
              END IF
              IF (scanValue .GT. oldScanValue) THEN
                MonthlyColumns(scanColumn)%reslt(Month) = scanValue
                MonthlyColumns(scanColumn)%timeStamp(Month) = timestepTimeStamp
              END IF
            CASE (aggTypeMinimumDuringHoursShown)
              IF (MonthlyColumns(scanColumn)%avgSum .EQ. isSum) THEN ! if it is a summed variable
                IF (IndexTypeKey .EQ. HVACTSReporting) THEN
                  scanValue = scanValue / (TimeStepSys * SecInHour)
                ELSE
                  scanValue = scanValue / (TimeStepZone * SecInHour)
                ENDIF
              END IF
              IF (scanValue .LT. oldScanValue) THEN
                MonthlyColumns(scanColumn)%reslt(Month) = scanValue
                MonthlyColumns(scanColumn)%timeStamp(Month) = timestepTimeStamp
              END IF
            CASE DEFAULT
              ! do nothing
          END SELECT
          activeHoursShown = .FALSE. !fixed CR8317
        END DO
      END IF
    END IF
  END DO
END DO
END SUBROUTINE GatherMonthlyResultsForTimestep

SUBROUTINE GatherBEPSResultsForTimestep(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine gathers data for producing the BEPS report

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects
          !   Meter names are of two forms:
          !
          !         <ResourceType>:<name>
          !   or
          !
          !         <EndUseType>:<ResourceType>
          !
          !   For the purposes of this routine, only the facility <name>
          !   is used.  Remember that 'Building' is actually the sum of
          !   the zones only without system,plant and exterior. The only
          !   way to get them all is to use 'facility'
          !
          !   The <EndUseType> are:
          !          Heating
          !          Cooling
          !          InteriorLights
          !          ExteriorLights
          !          InteriorEquipment
          !          ExteriorEquipment
          !          Fans
          !          Pumps
          !          HeatRejection
          !          Humidifier
          !          HeatRecovery
          !          DHW
          !          Refrigeration
          !          Cogeneration
          !
          !   The <ResourceType> are:
          !          Electricity
          !          Gas
          !          Gasoline
          !          Diesel
          !          Coal
          !          FuelOil#1
          !          FuelOil#2
          !          Propane
          !          Water
          !          Steam
          !          DistrictCooling
          !          DistrictHeating

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: EndUseCategory
USE DataStringGlobals, ONLY: CharComma, CharTab, CharSpace


IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER  :: iResource
INTEGER  :: jEndUse
INTEGER  :: kEndUseSub
REAL(r64)     :: curMeterValue
INTEGER  :: curMeterNumber

REAL(r64), external :: GetCurrentMeterValue


! if no beps report is called then skip

IF ((displayTabularBEPS .or. displayLEEDSummary) .AND. (IndexTypeKey .EQ. stepTypeZone)) THEN
  ! add the current time to the total elapsed time
  !FOLLOWING LINE MOVED TO UPDATETABULARREPORTS because used even when beps is not called
  !gatherElapsedTimeBEPS = gatherElapsedTimeBEPS + TimeStepZone
  ! loop through all of the resource types for the entire facility
!  DO iResource = 1, numResourceTypes
!    curMeterNumber = meterNumTotalsBEPS(iResource)
!    IF (curMeterNumber .GT. 0) THEN
!      curMeterValue = GetCurrentMeterValue(curMeterNumber)
!      gatherTotalsBEPS(iResource) = gatherTotalsBEPS(iResource) + curMeterValue
!    END IF
!  END DO

  ! loop through all of the resources and end uses for the entire facility
  DO iResource = 1, numResourceTypes
    curMeterNumber = meterNumTotalsBEPS(iResource)
    IF (curMeterNumber .GT. 0) THEN
      curMeterValue = GetCurrentMeterValue(curMeterNumber)
      gatherTotalsBEPS(iResource) = gatherTotalsBEPS(iResource) + curMeterValue
    END IF

    DO jEndUse = 1, numEndUses
      curMeterNumber = meterNumEndUseBEPS(jEndUse, iResource)
      IF (curMeterNumber .GT. 0) THEN
        curMeterValue = GetCurrentMeterValue(curMeterNumber)
        gatherEndUseBEPS(jEndUse, iResource) = gatherEndUseBEPS(jEndUse, iResource) + curMeterValue

        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          curMeterNumber = meterNumEndUseSubBEPS(iResource,jEndUse,kEndUseSub)
          IF (curMeterNumber .GT. 0) THEN
            curMeterValue = GetCurrentMeterValue(curMeterNumber)
            gatherEndUseSubBEPS(iResource,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(iResource,jEndUse,kEndUseSub) + curMeterValue
          END IF
        END DO
      END IF
    END DO
  END DO

  DO iResource = 1, numSourceTypes
    curMeterNumber = meterNumTotalsSource(iResource)
    IF (curMeterNumber .GT. 0) THEN
      curMeterValue = GetCurrentMeterValue(curMeterNumber)
      gatherTotalsSource(iResource) = gatherTotalsSource(iResource) + curMeterValue
    END IF
  END DO

  ! gather the electric load components
  gatherPowerFuelFireGen =  gatherPowerFuelFireGen  + GetCurrentMeterValue(meterNumPowerFuelFireGen)
  gatherPowerPV =           gatherPowerPV           + GetCurrentMeterValue(meterNumPowerPV)
  gatherPowerWind =         gatherPowerWind         + GetCurrentMeterValue(meterNumPowerWind)
  gatherPowerHTGeothermal = gatherPowerHTGeothermal + GetCurrentMeterValue(meterNumPowerHTGeothermal)
  gatherElecProduced =      gatherElecProduced      + GetCurrentMeterValue(meterNumElecProduced)
  gatherElecPurchased =     gatherElecPurchased     + GetCurrentMeterValue(meterNumElecPurchased)
  gatherElecSurplusSold =   gatherElecSurplusSold   + GetCurrentMeterValue(meterNumElecSurplusSold)
  ! gather the onsite thermal components
  gatherWaterHeatRecovery   = gatherWaterHeatRecovery     + GetCurrentMeterValue(meterNumWaterHeatRecovery)
  gatherAirHeatRecoveryCool = gatherAirHeatRecoveryCool   + GetCurrentMeterValue(meterNumAirHeatRecoveryCool)
  gatherAirHeatRecoveryHeat = gatherAirHeatRecoveryHeat   + GetCurrentMeterValue(meterNumAirHeatRecoveryHeat)
  gatherHeatHTGeothermal    = gatherHeatHTGeothermal      + GetCurrentMeterValue(meterNumHeatHTGeothermal)
  gatherHeatSolarWater      = gatherHeatSolarWater        + GetCurrentMeterValue(meterNumHeatSolarWater)
  gatherHeatSolarAir        = gatherHeatSolarAir          + GetCurrentMeterValue(meterNumHeatSolarAir)
  ! gather the water supply components
  gatherRainWater           = gatherRainWater        + GetCurrentMeterValue(meterNumRainWater)
  gatherCondensate          = gatherCondensate       + GetCurrentMeterValue(meterNumCondensate)
  gatherWellwater           = gatherWellwater        + GetCurrentMeterValue(meterNumGroundwater)
  gatherMains               = gatherMains            + GetCurrentMeterValue(meterNumMains)
  gatherWaterEndUseTotal    = gatherWaterEndUseTotal + GetCurrentMeterValue(meterNumWaterEndUseTotal)

END IF
END SUBROUTINE GatherBEPSResultsForTimestep

SUBROUTINE GatherSourceEnergyEndUseResultsForTimestep(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar
          !       DATE WRITTEN   September 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine gathers data for producing the end uses report in source energy

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects
          !   Meter names are of two forms:
          !
          !         <ResourceType>:<name>
          !   or
          !
          !         <EndUseType>:<ResourceType>
          !
          !   The <EndUseType> are:
          !          Heating
          !          Cooling
          !          InteriorLights
          !          ExteriorLights
          !          InteriorEquipment
          !          ExteriorEquipment
          !          Fans
          !          Pumps
          !          HeatRejection
          !          Humidifier
          !          HeatRecovery
          !          DHW
          !          Refrigeration
          !          Cogeneration
          !
          !   The <ResourceType> are:
          !          Electricity 1
          !          Gas 2
          !          Gasoline 6
          !          Diesel 8
          !          Coal 9
          !          FuelOil#1 10
          !          FuelOil#2 11
          !          Propane 12
          !          Water 7
          !          Steam 5
          !          DistrictCooling 3
          !          DistrictHeating 4

          !          sourceTypeNames(1)='Electric'
          !          sourceTypeNames(2)='NaturalGas'
          !          sourceTypeNames(3)='Gasoline'
          !          sourceTypeNames(4)='Diesel'
          !          sourceTypeNames(5)='Coal'
          !          sourceTypeNames(6)='FuelOil#1'
          !          sourceTypeNames(7)='FuelOil#2'
          !          sourceTypeNames(8)='Propane'
          !          sourceTypeNames(9)='PurchasedElectric'
          !          sourceTypeNames(10)='SoldElectric'
          !          sourceTypeNames(11)='OtherFuel1'
          !          sourceTypeNames(12)='OtherFuel2'

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: EndUseCategory
USE DataStringGlobals, ONLY: CharComma, CharTab, CharSpace
USE ScheduleManager, ONLY: GetCurrentScheduleValue

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER   :: iResource
INTEGER   :: jEndUse
INTEGER   :: kEndUseSub
REAL(r64) :: curMeterValue
INTEGER   :: curMeterNumber

REAL(r64), external :: GetCurrentMeterValue

! if no beps by source report is called then skip

IF ((displaySourceEnergyEndUseSummary) .AND. (IndexTypeKey .EQ. stepTypeZone)) THEN
  ! loop through all of the resources and end uses for the entire facility
  DO iResource = 1, numResourceTypes

    IF (fFSchedUsed(iResource)) THEN
      curMeterNumber = meterNumTotalsBEPS(iResource)
      IF (curMeterNumber .GT. 0) THEN
        curMeterValue = GetCurrentMeterValue(curMeterNumber) *   &
           GetCurrentScheduleValue(ffSchedIndex(iResource)) * SourceFactors(iResource)
        gatherTotalsBySourceBEPS(iResource) = gatherTotalsBySourceBEPS(iResource) + curMeterValue
      END IF
    ELSE
      curMeterNumber = meterNumTotalsBEPS(iResource)
      IF (curMeterNumber .GT. 0) THEN
        curMeterValue = GetCurrentMeterValue(curMeterNumber) * SourceFactors(iResource)
        gatherTotalsBySourceBEPS(iResource) = gatherTotalsBySourceBEPS(iResource) + curMeterValue
      END IF
    END IF

    DO jEndUse = 1, numEndUses
      IF (fFSchedUsed(iResource)) THEN
        curMeterNumber = meterNumEndUseBEPS(jEndUse, iResource)
        IF (curMeterNumber .GT. 0) THEN
          curMeterValue = GetCurrentMeterValue(curMeterNumber) *   &
             GetCurrentScheduleValue(ffSchedIndex(iResource)) * SourceFactors(iResource)
          gatherEndUseBySourceBEPS(jEndUse, iResource) = gatherEndUseBySourceBEPS(jEndUse, iResource) + curMeterValue
        END IF
      ELSE
        curMeterNumber = meterNumEndUseBEPS(jEndUse, iResource)
        IF (curMeterNumber .GT. 0) THEN
          curMeterValue = GetCurrentMeterValue(curMeterNumber) * SourceFactors(iResource)
          gatherEndUseBySourceBEPS(jEndUse, iResource) = gatherEndUseBySourceBEPS(jEndUse, iResource) + curMeterValue
        END IF
      END IF
    END DO
  END DO

END IF
END SUBROUTINE GatherSourceEnergyEndUseResultsForTimestep

SUBROUTINE GatherPeakDemandForTimestep(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   January 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine gathers data for producing the Peak Demand
          !   by end-use report

          ! METHODOLOGY EMPLOYED:
          !   Uses get input structure similar to other objects
          !   Meter names are of two forms:
          !
          !         <ResourceType>:<name>
          !   or
          !
          !         <EndUseType>:<ResourceType>
          !
          !   For the purposes of this routine, only the facility <name>
          !   is used.  Remember that 'Building' is actually the sum of
          !   the zones only without system,plant and exterior. The only
          !   way to get them all is to use 'facility'
          !
          !   The <EndUseType> are:
          !          Heating
          !          Cooling
          !          InteriorLights
          !          ExteriorLights
          !          InteriorEquipment
          !          ExteriorEquipment
          !          Fans
          !          Pumps
          !          HeatRejection
          !          Humidifier
          !          HeatRecovery
          !          DHW
          !          Refrigeration
          !          Cogeneration
          !
          !   The <ResourceType> are:
          !          Electricity
          !          Gas
          !          Gasoline
          !          Diesel
          !          Coal
          !          FuelOil#1
          !          FuelOil#2
          !          Propane
          !          Water
          !          Steam
          !          DistrictCooling
          !          DistrictHeating

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: EndUseCategory
USE DataStringGlobals, ONLY: CharComma, CharTab, CharSpace
USE DataEnvironment, ONLY: Month, DayOfMonth
USE General, ONLY: EncodeMonDayHrMin,DetermineMinuteForReporting

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER  :: iResource
INTEGER  :: jEndUse
INTEGER  :: kEndUseSub
REAL(r64)  :: curDemandValue
INTEGER  :: curMeterNumber
INTEGER  :: minuteCalculated
INTEGER  :: timestepTimeStamp
REAL(r64), external :: GetCurrentMeterValue

IF ((displayDemandEndUse) .AND. (IndexTypeKey .EQ. stepTypeZone)) THEN
  ! loop through all of the resources and end uses for the entire facility
  DO iResource = 1, numResourceTypes
    curMeterNumber = meterNumTotalsBEPS(iResource)
    IF (curMeterNumber .GT. 0) THEN
      curDemandValue = GetCurrentMeterValue(curMeterNumber) / (TimeStepZone * SecInHour)
      ! check if current value is greater than existing peak demand value
      IF (curDemandValue .GT. gatherDemandTotal(iResource)) THEN
        gatherDemandTotal(iResource) = curDemandValue
        ! save the time that the peak demand occured
!        minuteCalculated = (CurrentTime - INT(CurrentTime))*60
        minuteCalculated = DetermineMinuteForReporting(IndexTypeKey)
        CALL EncodeMonDayHrMin(timestepTimeStamp,Month,DayOfMonth,HourOfDay,minuteCalculated)
        gatherDemandTimeStamp(iResource)= timestepTimeStamp
        ! if new peak demand is set, then gather all of the end use values at this particular
        ! time to find the components of the peak demand
        DO jEndUse = 1, numEndUses
          curMeterNumber = meterNumEndUseBEPS(jEndUse, iResource)
          IF (curMeterNumber .GT. 0) THEN
            curDemandValue = GetCurrentMeterValue(curMeterNumber) / (TimeStepZone * SecInHour)
            gatherDemandEndUse(jEndUse, iResource) = curDemandValue
            DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
              curMeterNumber = meterNumEndUseSubBEPS(iResource,jEndUse,kEndUseSub)
              IF (curMeterNumber .GT. 0) THEN
                curDemandValue = GetCurrentMeterValue(curMeterNumber) / (TimeStepZone * SecInHour)
                gatherDemandEndUseSub(iResource,jEndUse,kEndUseSub) = curDemandValue
              END IF
            END DO
          END IF
        END DO
      END IF
    END IF
  END DO
END IF
END SUBROUTINE GatherPeakDemandForTimestep


SUBROUTINE GatherHeatGainReport(IndexTypeKey)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

!PURPOSE OF THIS SUBROUTINE:
!   Gathers the data each zone timestep for the heat gain report.
!
! The routine generates an annual table with the following columns which correspond to
! the output variables and data structures shown:
!
! Column                               Output Variable                                Internal Data Structure      Timestep Type
! ------                               ---------------                                -----------------------      -------- -----
! HVAC Input Sensible Air Heating      Zone Air Heat Balance System Air Transfer Rate ZnAirRpt()%SumMCpDTsystem    HVAC     Rate
!                                   Zone Air Heat Balance System Convective Heat Gain Rate ZnAirRpt()%SumNonAirSystem HVAC   Rate
!
! HVAC Input Sensible Air Cooling      Zone Air Heat Balance System Air Transfer Rate ZnAirRpt()%SumMCpDTsystem    HVAC     Rate
!                                    Zone Air Heat Balance System Convective Heat Gain Rate ZnAirRpt()%SumNonAirSystem HVAC  Rate
!
! HVAC Input Heated Surface Heating    Electric Low Temp Radiant Heating Energy       ElecRadSys()%HeatEnergy      HVAC     Energy
!                                      Zone Ventilated Slab Radiant Heating Energy    VentSlab()%RadHeatingEnergy  HVAC     Energy
!                                      Hydronic Low Temp Radiant Heating Energy       HydrRadSys()%HeatEnergy      HVAC     Energy
!                                      Constant Flow Low Temp Radiant Heating Energy  CFloRadSys()%HeatEnergy      HVAC     Energy
!
! HVAC Input Cooled Surface Cooling    Zone Ventilated Slab Radiant Cooling Energy    -VentSlab()%RadCoolingEnergy HVAC     Energy
!                                      Hydronic Low Temp Radiant Cooling Energy       -HydrRadSys()%CoolEnergy     HVAC     Energy
!                                      Constant Flow Low Temp Radiant Cooling Energy  -CFloRadSys()%CoolEnergy     HVAC     Energy
!
! People Sensible Heat Addition        Zone People Sensible Heating Energy            ZnRpt()%PeopleSenGain        Zone     Energy
!
! Lights Sensible Heat Addition        Zone Lights Total Heating Energy               ZnRpt()%LtsTotGain           Zone     Energy
!
! Equipment Sensible Heat Addition     Zone Electric Equipment Radiant Heating Energy ZnRpt()%ElecRadGain          Zone     Energy
!                                      Zone Gas Equipment Radiant Heating Energy      ZnRpt()%GasRadGain           Zone     Energy
!                                      Zone Steam Equipment Radiant Heating Energy    ZnRpt()%SteamRadGain         Zone     Energy
!                                      Zone Hot Water Equipment Radiant Heating Energy ZnRpt()%HWRadGain           Zone     Energy
!                                      Zone Other Equipment Radiant Heating Energy    ZnRpt()%OtherRadGain         Zone     Energy
!                                   Zone Electric Equipment Convective Heating Energy ZnRpt()%ElecConGain          Zone     Energy
!                                      Zone Gas Equipment Convective Heating Energy   ZnRpt()%GasConGain           Zone     Energy
!                                      Zone Steam Equipment Convective Heating Energy ZnRpt()%SteamConGain         Zone     Energy
!                                    Zone Hot Water Equipment Convective Heating Energy ZnRpt()%HWConGain          Zone     Energy
!                                      Zone Other Equipment Convective Heating Energy ZnRpt()%OtherConGain         Zone     Energy
!
! Window Heat Addition                 Zone Windows Total Heat Gain Energy            ZoneWinHeatGainRepEnergy()   Zone     Energy
!
! Interzone Air Transfer Heat Addition Zone Air Heat Balance Interzone Air Transfer Rate  ZnAirRpt()%SumMCpDTzones HVAC     Rate
!
! Infiltration Heat Addition           Zone Air Heat Balance Outdoor Air Transfer Rate ZnAirRpt()%SumMCpDtInfil     HVAC     Rate
!
! Equipment Sensible Heat Removal      Zone Electric Equipment Radiant Heating Energy ZnRpt()%ElecRadGain          Zone     Energy
!                                      Zone Gas Equipment Radiant Heating Energy      ZnRpt()%GasRadGain           Zone     Energy
!                                      Zone Steam Equipment Radiant Heating Energy    ZnRpt()%SteamRadGain         Zone     Energy
!                                      Zone Hot Water Equipment Radiant Heating Energy ZnRpt()%HWRadGain           Zone     Energy
!                                      Zone Other Equipment Radiant Heating Energy    ZnRpt()%OtherRadGain         Zone     Energy
!                                   Zone Electric Equipment Convective Heating Energy ZnRpt()%ElecConGain          Zone     Energy
!                                      Zone Gas Equipment Convective Heating Energy   ZnRpt()%GasConGain           Zone     Energy
!                                      Zone Steam Equipment Convective Heating Energy ZnRpt()%SteamConGain         Zone     Energy
!                                     Zone Hot Water Equipment Convective Heating Energy ZnRpt()%HWConGain         Zone     Energy
!                                      Zone Other Equipment Convective Heating Energy ZnRpt()%OtherConGain         Zone     Energy
!
! Window Heat Removal                  Zone Windows Total Heat Loss Energy            -ZoneWinHeatLossRepEnergy()  Zone     Energy
!
! Interzone Air Transfer Heat Removal  Zone Air Heat Balance Interzone Air Transfer Rate ZnAirRpt()%SumMCpDTzones  HVAC     Rate
!
! Infiltration Heat Removal            Zone Air Heat Balance Outdoor Air Transfer Rate ZnAirRpt()%SumMCpDtInfil     HVAC     Rate
!
! The following two columns are derived based on the values of the other columns and need to be computed on every HVAC timestep.
!   Opaque Surface Conduction and Other Heat Addition
!   Opaque Surface Conduction and Other Heat Removal
!
! For variables that are updated on a zone timestep basis, the values are used on the HVAC timestep but are ratioed by the
! timestep lengths.
!
! The peak reports follow a similar example.


          ! USE STATEMENTS:
USE DataHeatBalance, ONLY: ZonePreDefRep, ZnAirRpt, ZnRpt, &
                           ZoneWinHeatGainRepEnergy, ZoneWinHeatLossRepEnergy,  &
                           ZoneWinHeatGainRep,ZoneWinHeatLossRep, &
                           BuildingPreDefRep
USE VentilatedSlab, ONLY: VentSlab, NumOfVentSlabs
USE LowTempRadiantSystem, ONLY: HydrRadSys, NumOfHydrLowTempRadSys, &
                                CFloRadSys, NumOfCFloLowTempRadSys, &
                                ElecRadSys, NumOfElecLowTempRadSys
USE DataEnvironment, ONLY: Month, DayOfMonth
USE OutputReportPredefined, ONLY: pdrSensibleGain, reportName
USE DataHVACGlobals, ONLY: TimeStepSys,SysTimeElapsed
USE General, ONLY: EncodeMonDayHrMin,DetermineMinuteForReporting


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) :: IndexTypeKey  ! What kind of data to update (Zone, HVAC)

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64),PARAMETER :: FracToMin=60.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iZone = 0
INTEGER :: iRadiant = 0
INTEGER :: curZone = 0
REAL(r64) :: eqpSens = 0.0d0
REAL(r64) :: total = 0.0d0
! the following arrays store the radiant total for each timestep
REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: radiantHeat
REAL(r64), ALLOCATABLE, DIMENSION(:),SAVE :: radiantCool
INTEGER  :: timestepTimeStamp = 0
REAL(r64) :: bldgHtPk = 0.0d0
REAL(r64) :: bldgClPk = 0.0d0
REAL(r64) :: timeStepRatio = 0.0d0
LOGICAL, SAVE :: firstTime=.true.

integer ActualTimeMin


IF (.NOT. DoWeathSim) RETURN

IF (.not. reportName(pdrSensibleGain)%show) RETURN !don't gather data if report isn't requested

IF (IndexTypeKey .EQ. stepTypeZone) RETURN !only add values over the HVAC timestep basis

IF (firstTime) THEN
  ALLOCATE(radiantHeat(NumOfZones))
  ALLOCATE(radiantCool(NumOfZones))
  firstTime=.false.
END IF
  !clear the radiant surface accumulation variables
radiantHeat=0.0d0
radiantCool=0.0d0
!--------------------
!     ANNUAL
!--------------------
timeStepRatio = TimeStepSys/TimeStepZone !the fraction of the zone time step used by the system timestep
DO iZone = 1, NumOfZones
  !People Sensible Heat Addition
  ZonePreDefRep(iZone)%SHGSAnPeoplAdd =  ZonePreDefRep(iZone)%SHGSAnPeoplAdd + ZnRpt(iZone)%PeopleSenGain * timeStepRatio
  !Lights Sensible Heat Addition
  ZonePreDefRep(iZone)%SHGSAnLiteAdd =  ZonePreDefRep(iZone)%SHGSAnLiteAdd + ZnRpt(iZone)%LtsTotGain * timeStepRatio
  !HVAC Input Sensible Air Heating
  !HVAC Input Sensible Air Cooling
  IF ((ZnAirRpt(iZone)%SumMCpDTsystem + ZnAirRpt(iZone)%SumNonAirSystem) .GT. 0.0d0) THEN
    ZonePreDefRep(iZone)%SHGSAnHvacHt =  ZonePreDefRep(iZone)%SHGSAnHvacHt +   &
       ZnAirRpt(iZone)%SumMCpDTsystem * TimeStepSys * SecInHour &
       + ZnAirRpt(iZone)%SumNonAirSystem * TimeStepSys * SecInHour
  ELSE
    ZonePreDefRep(iZone)%SHGSAnHvacCl =  ZonePreDefRep(iZone)%SHGSAnHvacCl +   &
       ZnAirRpt(iZone)%SumMCpDTsystem * TimeStepSys * SecInHour &
       + ZnAirRpt(iZone)%SumNonAirSystem * TimeStepSys * SecInHour
  END IF
  !Interzone Air Transfer Heat Addition
  !Interzone Air Transfer Heat Removal
  IF (ZnAirRpt(iZone)%SumMCpDTzones .GT. 0.0d0) THEN
    ZonePreDefRep(iZone)%SHGSAnIzaAdd =  ZonePreDefRep(iZone)%SHGSAnIzaAdd +   &
       ZnAirRpt(iZone)%SumMCpDTzones * TimeStepSys * SecInHour
  ELSE
    ZonePreDefRep(iZone)%SHGSAnIzaRem =  ZonePreDefRep(iZone)%SHGSAnIzaRem +   &
       ZnAirRpt(iZone)%SumMCpDTzones * TimeStepSys * SecInHour
  END IF
  !Window Heat Addition
  !Window Heat Removal
  ZonePreDefRep(iZone)%SHGSAnWindAdd = ZonePreDefRep(iZone)%SHGSAnWindAdd + ZoneWinHeatGainRepEnergy(iZone) * timeStepRatio
  ZonePreDefRep(iZone)%SHGSAnWindRem = ZonePreDefRep(iZone)%SHGSAnWindRem - ZoneWinHeatLossRepEnergy(iZone) * timeStepRatio
  !Infiltration Heat Addition
  !Infiltration Heat Removal
  IF (ZnAirRpt(iZone)%SumMCpDtInfil .GT. 0.0d0) THEN
    ZonePreDefRep(iZone)%SHGSAnInfilAdd = ZonePreDefRep(iZone)%SHGSAnInfilAdd +   &
       ZnAirRpt(iZone)%SumMCpDtInfil * TimeStepSys * SecInHour
  ELSE
    ZonePreDefRep(iZone)%SHGSAnInfilRem = ZonePreDefRep(iZone)%SHGSAnInfilRem +   &
       ZnAirRpt(iZone)%SumMCpDtInfil * TimeStepSys * SecInHour
  END IF
  !Equipment Sensible Heat Addition
  !Equipment Sensible Heat Removal
  ! the following variables are already gains so they do not need to be converted by multiplying by time.
  eqpSens = (ZnRpt(iZone)%ElecRadGain + ZnRpt(iZone)%GasRadGain + ZnRpt(iZone)%HWRadGain +   &
     ZnRpt(iZone)%SteamRadGain + ZnRpt(iZone)%OtherRadGain + ZnRpt(iZone)%ElecConGain +   &
     ZnRpt(iZone)%GasConGain + ZnRpt(iZone)%HWConGain + ZnRpt(iZone)%SteamConGain +   &
     ZnRpt(iZone)%OtherConGain) &
          * timeStepRatio
  IF (eqpSens .GT. 0.0d0) THEN
    ZonePreDefRep(iZone)%SHGSAnEquipAdd = ZonePreDefRep(iZone)%SHGSAnEquipAdd + eqpSens
  ELSE
    ZonePreDefRep(iZone)%SHGSAnEquipRem = ZonePreDefRep(iZone)%SHGSAnEquipRem + eqpSens
  ENDIF
END DO
! HVAC Input Heated Surface Heating
! HVAC Input Cooled Surface Cooling
DO iRadiant = 1, NumOfVentSlabs
  curZone = VentSlab(iRadiant)%ZonePtr
  IF ((curZone .GT. 0) .AND. (curZone .LE. NumOfZones)) THEN
    ZonePreDefRep(curZone)%SHGSAnSurfHt = ZonePreDefRep(curZone)%SHGSAnSurfHt + VentSlab(iRadiant)%RadHeatingEnergy
    ZonePreDefRep(curZone)%SHGSAnSurfCl = ZonePreDefRep(curZone)%SHGSAnSurfCl - VentSlab(iRadiant)%RadCoolingEnergy
    radiantHeat(curZone) = VentSlab(iRadiant)%RadHeatingPower
    radiantCool(curZone) = -VentSlab(iRadiant)%RadCoolingPower
  END IF
END DO
DO iRadiant = 1, NumOfHydrLowTempRadSys
  curZone = HydrRadSys(iRadiant)%ZonePtr
  IF ((curZone .GT. 0) .AND. (curZone .LE. NumOfZones)) THEN
    ZonePreDefRep(curZone)%SHGSAnSurfHt = ZonePreDefRep(curZone)%SHGSAnSurfHt + HydrRadSys(iRadiant)%HeatEnergy
    ZonePreDefRep(curZone)%SHGSAnSurfCl = ZonePreDefRep(curZone)%SHGSAnSurfCl - HydrRadSys(iRadiant)%CoolEnergy
    radiantHeat(curZone) = radiantHeat(curZone) + HydrRadSys(iRadiant)%HeatPower
    radiantCool(curZone) = radiantCool(curZone) - HydrRadSys(iRadiant)%CoolPower
  END IF
END DO
DO iRadiant = 1, NumOfCFloLowTempRadSys
  curZone = CFloRadSys(iRadiant)%ZonePtr
  IF ((curZone .GT. 0) .AND. (curZone .LE. NumOfZones)) THEN
    ZonePreDefRep(curZone)%SHGSAnSurfHt = ZonePreDefRep(curZone)%SHGSAnSurfHt + CFloRadSys(iRadiant)%HeatEnergy
    ZonePreDefRep(curZone)%SHGSAnSurfCl = ZonePreDefRep(curZone)%SHGSAnSurfCl - CFloRadSys(iRadiant)%CoolEnergy
    radiantHeat(curZone) = radiantHeat(curZone) + CFloRadSys(iRadiant)%HeatPower
    radiantCool(curZone) = radiantCool(curZone) - CFloRadSys(iRadiant)%CoolPower
  END IF
END DO
DO iRadiant = 1, NumOfElecLowTempRadSys
  curZone = ElecRadSys(iRadiant)%ZonePtr
  IF ((curZone .GT. 0) .AND. (curZone .LE. NumOfZones)) THEN
    ZonePreDefRep(curZone)%SHGSAnSurfHt = ZonePreDefRep(curZone)%SHGSAnSurfHt + ElecRadSys(iRadiant)%HeatEnergy
    radiantHeat(curZone) = radiantHeat(curZone) + ElecRadSys(iRadiant)%HeatPower
  END IF
END DO
! Opaque Surface Conduction and Other Heat Addition
! Opaque Surface Conduction and Other Heat Removal
DO iZone = 1, NumOfZones
  total =  ZonePreDefRep(iZone)%SHGSAnPeoplAdd    &
         + ZonePreDefRep(iZone)%SHGSAnLiteAdd     &
         + ZonePreDefRep(iZone)%SHGSAnHvacHt      &
         + ZonePreDefRep(iZone)%SHGSAnHvacCl      &
         + ZonePreDefRep(iZone)%SHGSAnIzaAdd      &
         + ZonePreDefRep(iZone)%SHGSAnIzaRem      &
         + ZonePreDefRep(iZone)%SHGSAnWindAdd     &
         + ZonePreDefRep(iZone)%SHGSAnWindRem     &
         + ZonePreDefRep(iZone)%SHGSAnInfilAdd    &
         + ZonePreDefRep(iZone)%SHGSAnInfilRem    &
         + ZonePreDefRep(iZone)%SHGSAnEquipAdd    &
         + ZonePreDefRep(iZone)%SHGSAnEquipRem    &
         + ZonePreDefRep(iZone)%SHGSAnSurfHt      &
         + ZonePreDefRep(iZone)%SHGSAnSurfCl
  total = -total !want to know the negative value of the sum since the row should add up to zero
  IF (total .GT. 0) THEN
    ZonePreDefRep(iZone)%SHGSAnOtherAdd = total
  ELSE
    ZonePreDefRep(iZone)%SHGSAnOtherRem = total
  ENDIF
END DO
!--------------------------------
! ZONE PEAK COOLING AND HEATING
!--------------------------------
DO iZone = 1, NumOfZones
  IF ((ZnAirRpt(iZone)%SumMCpDTsystem + radiantHeat(iZone)+ ZnAirRpt(iZone)%SumNonAirSystem) .GT. 0) THEN
    IF ((ZnAirRpt(iZone)%SumMCpDTsystem + radiantHeat(iZone)+ ZnAirRpt(iZone)%SumNonAirSystem) &
          .GT. ZonePreDefRep(iZone)%htPeak) THEN
      ZonePreDefRep(iZone)%htPeak = ZnAirRpt(iZone)%SumMCpDTsystem + radiantHeat(iZone) + ZnAirRpt(iZone)%SumNonAirSystem
      !determine timestamp
!      ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
!      ActualtimeE = ActualTimeS+TimeStepSys
!      ActualTimeHrS=INT(ActualTimeS)
!      ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
      ActualTimeMin=DetermineMinuteForReporting(IndexTypeKey)
      CALL EncodeMonDayHrMin(timestepTimeStamp,Month,DayOfMonth,HourOfDay,ActualTimeMin)
      ZonePreDefRep(iZone)%htPtTimeStamp = timestepTimeStamp
      !HVAC Input Sensible Air Heating
      !HVAC Input Sensible Air Cooling
      ZonePreDefRep(iZone)%SHGSHtHvacHt = ZnAirRpt(iZone)%SumMCpDTsystem + ZnAirRpt(iZone)%SumNonAirSystem
      ZonePreDefRep(iZone)%SHGSHtHvacCl = 0.0d0
      ! HVAC Input Heated Surface Heating
      ! HVAC Input Cooled Surface Cooling
      ZonePreDefRep(iZone)%SHGSHtSurfHt = radiantHeat(iZone)
      ZonePreDefRep(iZone)%SHGSHtSurfCl = radiantCool(iZone)
      !People Sensible Heat Addition
      ZonePreDefRep(iZone)%SHGSHtPeoplAdd = ZnRpt(iZone)%PeopleSenGainRate
      !Lights Sensible Heat Addition
      ZonePreDefRep(iZone)%SHGSHtLiteAdd = ZnRpt(iZone)%LtsTotGainRate
      !Equipment Sensible Heat Addition
      !Equipment Sensible Heat Removal
      eqpSens = ZnRpt(iZone)%ElecRadGainRate + ZnRpt(iZone)%GasRadGainRate &
                  + ZnRpt(iZone)%HWRadGainRate + ZnRpt(iZone)%SteamRadGainRate + ZnRpt(iZone)%OtherRadGainRate &
                  + ZnRpt(iZone)%ElecConGainRate + ZnRpt(iZone)%GasConGainRate + ZnRpt(iZone)%HWConGainRate &
                  + ZnRpt(iZone)%SteamConGainRate + ZnRpt(iZone)%OtherConGainRate
      IF (eqpSens .GT. 0.0d0) THEN
        ZonePreDefRep(iZone)%SHGSHtEquipAdd = eqpSens
        ZonePreDefRep(iZone)%SHGSHtEquipRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSHtEquipAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSHtEquipRem = eqpSens
      END IF
      !Window Heat Addition
      !Window Heat Removal
      ZonePreDefRep(iZone)%SHGSHtWindAdd = ZoneWinHeatGainRep(iZone)
      ZonePreDefRep(iZone)%SHGSHtWindRem = -ZoneWinHeatLossRep(iZone)
      IF (ZnAirRpt(iZone)%SumMCpDTzones .GT. 0.0d0) THEN
        ZonePreDefRep(iZone)%SHGSHtIzaAdd = ZnAirRpt(iZone)%SumMCpDTzones
        ZonePreDefRep(iZone)%SHGSHtIzaRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSHtIzaAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSHtIzaRem = ZnAirRpt(iZone)%SumMCpDTzones
      ENDIF
      !Infiltration Heat Addition
      !Infiltration Heat Removal
      IF (ZnAirRpt(iZone)%SumMCpDtInfil .GT. 0.0d0) THEN
        ZonePreDefRep(iZone)%SHGSHtInfilAdd = ZnAirRpt(iZone)%SumMCpDtInfil
        ZonePreDefRep(iZone)%SHGSHtInfilRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSHtInfilAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSHtInfilRem = ZnAirRpt(iZone)%SumMCpDtInfil
      ENDIF
      ! Opaque Surface Conduction and Other Heat Addition
      ! Opaque Surface Conduction and Other Heat Removal
      total =  ZonePreDefRep(iZone)%SHGSHtPeoplAdd    &
             + ZonePreDefRep(iZone)%SHGSHtLiteAdd     &
             + ZonePreDefRep(iZone)%SHGSHtHvacHt      &
             + ZonePreDefRep(iZone)%SHGSHtHvacCl      &
             + ZonePreDefRep(iZone)%SHGSHtIzaAdd      &
             + ZonePreDefRep(iZone)%SHGSHtIzaRem      &
             + ZonePreDefRep(iZone)%SHGSHtWindAdd     &
             + ZonePreDefRep(iZone)%SHGSHtWindRem     &
             + ZonePreDefRep(iZone)%SHGSHtInfilAdd    &
             + ZonePreDefRep(iZone)%SHGSHtInfilRem    &
             + ZonePreDefRep(iZone)%SHGSHtEquipAdd    &
             + ZonePreDefRep(iZone)%SHGSHtEquipRem    &
             + ZonePreDefRep(iZone)%SHGSHtSurfHt      &
             + ZonePreDefRep(iZone)%SHGSHtSurfCl
      total = -total !want to know the negative value of the sum since the row should add up to zero
      IF (total .GT. 0) THEN
        ZonePreDefRep(iZone)%SHGSHtOtherAdd = total
        ZonePreDefRep(iZone)%SHGSHtOtherRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSHtOtherAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSHtOtherRem = total
      ENDIF
    END IF
  ELSE
    IF ((ZnAirRpt(iZone)%SumMCpDTsystem + radiantCool(iZone) + ZnAirRpt(iZone)%SumNonAirSystem) &
          .LT. ZonePreDefRep(iZone)%clPeak) THEN
      ZonePreDefRep(iZone)%clPeak = ZnAirRpt(iZone)%SumMCpDTsystem + radiantCool(iZone) + ZnAirRpt(iZone)%SumNonAirSystem
      !determine timestamp
!      ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
!      ActualtimeE = ActualTimeS+TimeStepSys
!      ActualTimeHrS=INT(ActualTimeS)
!      ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
      ActualTimeMin=DetermineMinuteForReporting(IndexTypeKey)
      CALL EncodeMonDayHrMin(timestepTimeStamp,Month,DayOfMonth,HourOfDay,ActualTimeMin)
      ZonePreDefRep(iZone)%clPtTimeStamp = timestepTimeStamp
      !HVAC Input Sensible Air Heating
      !HVAC Input Sensible Air Cooling
      ZonePreDefRep(iZone)%SHGSClHvacHt = 0.0d0
      ZonePreDefRep(iZone)%SHGSClHvacCl = ZnAirRpt(iZone)%SumMCpDTsystem + ZnAirRpt(iZone)%SumNonAirSystem
      ! HVAC Input Heated Surface Heating
      ! HVAC Input Cooled Surface Cooling
      ZonePreDefRep(iZone)%SHGSClSurfHt = radiantHeat(iZone)
      ZonePreDefRep(iZone)%SHGSClSurfCl = radiantCool(iZone)
      !People Sensible Heat Addition
      ZonePreDefRep(iZone)%SHGSClPeoplAdd = ZnRpt(iZone)%PeopleSenGainRate
      !Lights Sensible Heat Addition
      ZonePreDefRep(iZone)%SHGSClLiteAdd = ZnRpt(iZone)%LtsTotGainRate
      !Equipment Sensible Heat Addition
      !Equipment Sensible Heat Removal
      eqpSens = ZnRpt(iZone)%ElecRadGainRate + ZnRpt(iZone)%GasRadGainRate &
                  + ZnRpt(iZone)%HWRadGainRate + ZnRpt(iZone)%SteamRadGainRate + ZnRpt(iZone)%OtherRadGainRate &
                  + ZnRpt(iZone)%ElecConGainRate + ZnRpt(iZone)%GasConGainRate + ZnRpt(iZone)%HWConGainRate &
                  + ZnRpt(iZone)%SteamConGainRate + ZnRpt(iZone)%OtherConGainRate
      IF (eqpSens .GT. 0.0d0) THEN
        ZonePreDefRep(iZone)%SHGSClEquipAdd = eqpSens
        ZonePreDefRep(iZone)%SHGSClEquipRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSClEquipAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSClEquipRem = eqpSens
      END IF
      !Window Heat Addition
      !Window Heat Removal
      ZonePreDefRep(iZone)%SHGSClWindAdd = ZoneWinHeatGainRep(iZone)
      ZonePreDefRep(iZone)%SHGSClWindRem = -ZoneWinHeatLossRep(iZone)
      IF (ZnAirRpt(iZone)%SumMCpDTzones .GT. 0.0d0) THEN
        ZonePreDefRep(iZone)%SHGSClIzaAdd = ZnAirRpt(iZone)%SumMCpDTzones
        ZonePreDefRep(iZone)%SHGSClIzaRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSClIzaAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSClIzaRem = ZnAirRpt(iZone)%SumMCpDTzones
      ENDIF
      !Infiltration Heat Addition
      !Infiltration Heat Removal
      IF (ZnAirRpt(iZone)%SumMCpDtInfil .GT. 0.0d0) THEN
        ZonePreDefRep(iZone)%SHGSClInfilAdd = ZnAirRpt(iZone)%SumMCpDtInfil
        ZonePreDefRep(iZone)%SHGSClInfilRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSClInfilAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSClInfilRem = ZnAirRpt(iZone)%SumMCpDtInfil
      ENDIF
      ! Opaque Surface Conduction and Other Heat Addition
      ! Opaque Surface Conduction and Other Heat Removal
      total =  ZonePreDefRep(iZone)%SHGSClPeoplAdd    &
             + ZonePreDefRep(iZone)%SHGSClLiteAdd     &
             + ZonePreDefRep(iZone)%SHGSClHvacHt      &
             + ZonePreDefRep(iZone)%SHGSClHvacCl      &
             + ZonePreDefRep(iZone)%SHGSClIzaAdd      &
             + ZonePreDefRep(iZone)%SHGSClIzaRem      &
             + ZonePreDefRep(iZone)%SHGSClWindAdd     &
             + ZonePreDefRep(iZone)%SHGSClWindRem     &
             + ZonePreDefRep(iZone)%SHGSClInfilAdd    &
             + ZonePreDefRep(iZone)%SHGSClInfilRem    &
             + ZonePreDefRep(iZone)%SHGSClEquipAdd    &
             + ZonePreDefRep(iZone)%SHGSClEquipRem    &
             + ZonePreDefRep(iZone)%SHGSClSurfHt      &
             + ZonePreDefRep(iZone)%SHGSClSurfCl
      total = -total !want to know the negative value of the sum since the row should add up to zero
      IF (total .GT. 0) THEN
        ZonePreDefRep(iZone)%SHGSClOtherAdd = total
        ZonePreDefRep(iZone)%SHGSClOtherRem = 0.0d0
      ELSE
        ZonePreDefRep(iZone)%SHGSClOtherAdd = 0.0d0
        ZonePreDefRep(iZone)%SHGSClOtherRem = total
      ENDIF
    END IF
  END IF
END DO
!------------------------------------
! BUILDING PEAK COOLING AND HEATING
!------------------------------------
bldgHtPk = 0.0d0
bldgClPk = 0.0d0
DO iZone = 1, NumOfZones
  IF ((ZnAirRpt(iZone)%SumMCpDTsystem  + radiantHeat(iZone) + ZnAirRpt(iZone)%SumNonAirSystem) .GT. 0) THEN
    bldgHtPk = bldgHtPk + ZnAirRpt(iZone)%SumMCpDTsystem + radiantHeat(iZone) + ZnAirRpt(iZone)%SumNonAirSystem
  ELSE
    bldgClPk = bldgClPk + ZnAirRpt(iZone)%SumMCpDTsystem + radiantCool(iZone) + ZnAirRpt(iZone)%SumNonAirSystem
  END IF
END DO
IF (bldgHtPk .GT. BuildingPreDefRep%htPeak) THEN
  BuildingPreDefRep%htPeak =  bldgHtPk
  !determine timestamp
!  ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
!  ActualtimeE = ActualTimeS+TimeStepSys
!  ActualTimeHrS=INT(ActualTimeS)
!  ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
  ActualTimeMin=DetermineMinuteForReporting(IndexTypeKey)
  CALL EncodeMonDayHrMin(timestepTimeStamp,Month,DayOfMonth,HourOfDay,ActualTimeMin)
  BuildingPreDefRep%htPtTimeStamp = timestepTimeStamp
  !reset building level results to zero prior to accumulating across zones
  BuildingPreDefRep%SHGSHtHvacHt = 0.0d0
  BuildingPreDefRep%SHGSHtHvacCl = 0.0d0
  BuildingPreDefRep%SHGSHtSurfHt = 0.0d0
  BuildingPreDefRep%SHGSHtSurfCl = 0.0d0
  BuildingPreDefRep%SHGSHtPeoplAdd = 0.0d0
  BuildingPreDefRep%SHGSHtLiteAdd = 0.0d0
  BuildingPreDefRep%SHGSHtEquipAdd = 0.0d0
  BuildingPreDefRep%SHGSHtWindAdd = 0.0d0
  BuildingPreDefRep%SHGSHtIzaAdd = 0.0d0
  BuildingPreDefRep%SHGSHtInfilAdd = 0.0d0
  BuildingPreDefRep%SHGSHtOtherAdd = 0.0d0
  BuildingPreDefRep%SHGSHtEquipRem = 0.0d0
  BuildingPreDefRep%SHGSHtWindRem = 0.0d0
  BuildingPreDefRep%SHGSHtIzaRem = 0.0d0
  BuildingPreDefRep%SHGSHtInfilRem = 0.0d0
  BuildingPreDefRep%SHGSHtOtherRem = 0.0d0
  DO iZone = 1, NumOfZones
    !HVAC Input Sensible Air Heating
    !HVAC Input Sensible Air Cooling
    BuildingPreDefRep%SHGSHtHvacHt = BuildingPreDefRep%SHGSHtHvacHt + ZnAirRpt(iZone)%SumMCpDTsystem &
                                      + ZnAirRpt(iZone)%SumNonAirSystem
    ! HVAC Input Heated Surface Heating
    ! HVAC Input Cooled Surface Cooling
    BuildingPreDefRep%SHGSHtSurfHt = BuildingPreDefRep%SHGSHtSurfHt + radiantHeat(iZone)
    BuildingPreDefRep%SHGSHtSurfCl = BuildingPreDefRep%SHGSHtSurfCl + radiantCool(iZone)
    !People Sensible Heat Addition
    BuildingPreDefRep%SHGSHtPeoplAdd = BuildingPreDefRep%SHGSHtPeoplAdd + ZnRpt(iZone)%PeopleSenGainRate
    !Lights Sensible Heat Addition
    BuildingPreDefRep%SHGSHtLiteAdd = BuildingPreDefRep%SHGSHtLiteAdd + ZnRpt(iZone)%LtsTotGainRate
    !Equipment Sensible Heat Addition
    !Equipment Sensible Heat Removal
    eqpSens = ZnRpt(iZone)%ElecRadGainRate + ZnRpt(iZone)%GasRadGainRate &
                  + ZnRpt(iZone)%HWRadGainRate + ZnRpt(iZone)%SteamRadGainRate + ZnRpt(iZone)%OtherRadGainRate &
                  + ZnRpt(iZone)%ElecConGainRate + ZnRpt(iZone)%GasConGainRate + ZnRpt(iZone)%HWConGainRate &
                  + ZnRpt(iZone)%SteamConGainRate + ZnRpt(iZone)%OtherConGainRate
    IF (eqpSens .GT. 0.0d0) THEN
      BuildingPreDefRep%SHGSHtEquipAdd = BuildingPreDefRep%SHGSHtEquipAdd + eqpSens
    ELSE
      BuildingPreDefRep%SHGSHtEquipRem = BuildingPreDefRep%SHGSHtEquipRem + eqpSens
    END IF
    !Window Heat Addition
    !Window Heat Removal
    BuildingPreDefRep%SHGSHtWindAdd = BuildingPreDefRep%SHGSHtWindAdd + ZoneWinHeatGainRep(iZone)
    BuildingPreDefRep%SHGSHtWindRem = BuildingPreDefRep%SHGSHtWindRem -ZoneWinHeatLossRep(iZone)
    IF (ZnAirRpt(iZone)%SumMCpDTzones .GT. 0.0d0) THEN
      BuildingPreDefRep%SHGSHtIzaAdd = BuildingPreDefRep%SHGSHtIzaAdd + ZnAirRpt(iZone)%SumMCpDTzones
    ELSE
      BuildingPreDefRep%SHGSHtIzaRem = BuildingPreDefRep%SHGSHtIzaRem + ZnAirRpt(iZone)%SumMCpDTzones
    ENDIF
    !Infiltration Heat Addition
    !Infiltration Heat Removal
    IF (ZnAirRpt(iZone)%SumMCpDtInfil .GT. 00) THEN
      BuildingPreDefRep%SHGSHtInfilAdd = BuildingPreDefRep%SHGSHtInfilAdd + ZnAirRpt(iZone)%SumMCpDtInfil
    ELSE
      BuildingPreDefRep%SHGSHtInfilRem = BuildingPreDefRep%SHGSHtInfilRem + ZnAirRpt(iZone)%SumMCpDtInfil
    ENDIF
  END DO
  ! Opaque Surface Conduction and Other Heat Addition
  ! Opaque Surface Conduction and Other Heat Removal
  total =  BuildingPreDefRep%SHGSHtPeoplAdd    &
         + BuildingPreDefRep%SHGSHtLiteAdd     &
         + BuildingPreDefRep%SHGSHtHvacHt      &
         + BuildingPreDefRep%SHGSHtHvacCl      &
         + BuildingPreDefRep%SHGSHtIzaAdd      &
         + BuildingPreDefRep%SHGSHtIzaRem      &
         + BuildingPreDefRep%SHGSHtWindAdd     &
         + BuildingPreDefRep%SHGSHtWindRem     &
         + BuildingPreDefRep%SHGSHtInfilAdd    &
         + BuildingPreDefRep%SHGSHtInfilRem    &
         + BuildingPreDefRep%SHGSHtEquipAdd    &
         + BuildingPreDefRep%SHGSHtEquipRem    &
         + BuildingPreDefRep%SHGSHtSurfHt      &
         + BuildingPreDefRep%SHGSHtSurfCl
  total = -total !want to know the negative value of the sum since the row should add up to zero
  IF (total .GT. 0) THEN
    BuildingPreDefRep%SHGSHtOtherAdd = BuildingPreDefRep%SHGSHtOtherAdd + total
  ELSE
    BuildingPreDefRep%SHGSHtOtherRem = BuildingPreDefRep%SHGSHtOtherRem + total
  ENDIF
END IF
IF (bldgClPk .LT. BuildingPreDefRep%clPeak) THEN
  BuildingPreDefRep%clPeak =  bldgClPk
  !determine timestamp
!  ActualTimeS = CurrentTime-TimeStepZone+SysTimeElapsed
!  ActualtimeE = ActualTimeS+TimeStepSys
!  ActualTimeHrS=INT(ActualTimeS)
!  ActualTimeMin=NINT((ActualtimeE - ActualTimeHrS)*FracToMin)
  ActualTimeMin=DetermineMinuteForReporting(IndexTypeKey)
  CALL EncodeMonDayHrMin(timestepTimeStamp,Month,DayOfMonth,HourOfDay,ActualTimeMin)
  BuildingPreDefRep%clPtTimeStamp = timestepTimeStamp
  !reset building level results to zero prior to accumulating across zones
  BuildingPreDefRep%SHGSClHvacHt = 0.0d0
  BuildingPreDefRep%SHGSClHvacCl = 0.0d0
  BuildingPreDefRep%SHGSClSurfHt = 0.0d0
  BuildingPreDefRep%SHGSClSurfCl = 0.0d0
  BuildingPreDefRep%SHGSClPeoplAdd = 0.0d0
  BuildingPreDefRep%SHGSClLiteAdd = 0.0d0
  BuildingPreDefRep%SHGSClEquipAdd = 0.0d0
  BuildingPreDefRep%SHGSClWindAdd = 0.0d0
  BuildingPreDefRep%SHGSClIzaAdd = 0.0d0
  BuildingPreDefRep%SHGSClInfilAdd = 0.0d0
  BuildingPreDefRep%SHGSClOtherAdd = 0.0d0
  BuildingPreDefRep%SHGSClEquipRem = 0.0d0
  BuildingPreDefRep%SHGSClWindRem = 0.0d0
  BuildingPreDefRep%SHGSClIzaRem = 0.0d0
  BuildingPreDefRep%SHGSClInfilRem = 0.0d0
  BuildingPreDefRep%SHGSClOtherRem = 0.0d0
  DO iZone = 1, NumOfZones
    !HVAC Input Sensible Air Heating
    !HVAC Input Sensible Air Cooling
    BuildingPreDefRep%SHGSClHvacCl = BuildingPreDefRep%SHGSClHvacCl + ZnAirRpt(iZone)%SumMCpDTsystem &
                                      + ZnAirRpt(iZone)%SumNonAirSystem
    ! HVAC Input Heated Surface Heating
    ! HVAC Input Cooled Surface Cooling
    BuildingPreDefRep%SHGSClSurfHt = BuildingPreDefRep%SHGSClSurfHt + radiantHeat(iZone)
    BuildingPreDefRep%SHGSClSurfCl = BuildingPreDefRep%SHGSClSurfCl + radiantCool(iZone)
    !People Sensible Heat Addition
    BuildingPreDefRep%SHGSClPeoplAdd = BuildingPreDefRep%SHGSClPeoplAdd + ZnRpt(iZone)%PeopleSenGainRate
    !Lights Sensible Heat Addition
    BuildingPreDefRep%SHGSClLiteAdd = BuildingPreDefRep%SHGSClLiteAdd + ZnRpt(iZone)%LtsTotGainRate
    !Equipment Sensible Heat Addition
    !Equipment Sensible Heat Removal
    eqpSens = ZnRpt(iZone)%ElecRadGainRate + ZnRpt(iZone)%GasRadGainRate &
                  + ZnRpt(iZone)%HWRadGainRate + ZnRpt(iZone)%SteamRadGainRate + ZnRpt(iZone)%OtherRadGainRate &
                  + ZnRpt(iZone)%ElecConGainRate + ZnRpt(iZone)%GasConGainRate + ZnRpt(iZone)%HWConGainRate &
                  + ZnRpt(iZone)%SteamConGainRate + ZnRpt(iZone)%OtherConGainRate
    IF (eqpSens .GT. 0.0d0) THEN
      BuildingPreDefRep%SHGSClEquipAdd = BuildingPreDefRep%SHGSClEquipAdd + eqpSens
    ELSE
      BuildingPreDefRep%SHGSClEquipRem = BuildingPreDefRep%SHGSClEquipRem + eqpSens
    END IF
    !Window Heat Addition
    !Window Heat Removal
    BuildingPreDefRep%SHGSClWindAdd = BuildingPreDefRep%SHGSClWindAdd + ZoneWinHeatGainRep(iZone)
    BuildingPreDefRep%SHGSClWindRem = BuildingPreDefRep%SHGSClWindRem - ZoneWinHeatLossRep(iZone)
    IF (ZnAirRpt(iZone)%SumMCpDTzones .GT. 0.0d0) THEN
      BuildingPreDefRep%SHGSClIzaAdd = BuildingPreDefRep%SHGSClIzaAdd + ZnAirRpt(iZone)%SumMCpDTzones
    ELSE
      BuildingPreDefRep%SHGSClIzaRem = BuildingPreDefRep%SHGSClIzaRem + ZnAirRpt(iZone)%SumMCpDTzones
    ENDIF
    !Infiltration Heat Addition
    !Infiltration Heat Removal
    IF (ZnAirRpt(iZone)%SumMCpDtInfil .GT. 00) THEN
      BuildingPreDefRep%SHGSClInfilAdd = BuildingPreDefRep%SHGSClInfilAdd + ZnAirRpt(iZone)%SumMCpDtInfil
    ELSE
      BuildingPreDefRep%SHGSClInfilRem = BuildingPreDefRep%SHGSClInfilRem + ZnAirRpt(iZone)%SumMCpDtInfil
    ENDIF
  END DO
  ! Opaque Surface Conduction and Other Heat Addition
  ! Opaque Surface Conduction and Other Heat Removal
  total =  BuildingPreDefRep%SHGSClPeoplAdd    &
         + BuildingPreDefRep%SHGSClLiteAdd     &
         + BuildingPreDefRep%SHGSClHvacHt      &
         + BuildingPreDefRep%SHGSClHvacCl      &
         + BuildingPreDefRep%SHGSClIzaAdd      &
         + BuildingPreDefRep%SHGSClIzaRem      &
         + BuildingPreDefRep%SHGSClWindAdd     &
         + BuildingPreDefRep%SHGSClWindRem     &
         + BuildingPreDefRep%SHGSClInfilAdd    &
         + BuildingPreDefRep%SHGSClInfilRem    &
         + BuildingPreDefRep%SHGSClEquipAdd    &
         + BuildingPreDefRep%SHGSClEquipRem    &
         + BuildingPreDefRep%SHGSClSurfHt      &
         + BuildingPreDefRep%SHGSClSurfCl
  total = -total !want to know the negative value of the sum since the row should add up to zero
  IF (total .GT. 0) THEN
    BuildingPreDefRep%SHGSClOtherAdd = BuildingPreDefRep%SHGSClOtherAdd + total
  ELSE
    BuildingPreDefRep%SHGSClOtherRem = BuildingPreDefRep%SHGSClOtherRem + total
  ENDIF
END IF
END SUBROUTINE GatherHeatGainReport

!======================================================================================================================
!======================================================================================================================
!
!
!    WRITE OUTPUT FILE ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================

SUBROUTINE     WriteTabularReports
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   This routine hides from the main simulation that four specific
          !   types of tabular reports are each created. If another type of
          !   report is added it can be added to the list here.
IMPLICIT NONE

  INTEGER EchoInputFile  ! found unit number for 'eplusout.audit'
INTEGER, EXTERNAL :: FindUnitNumber

CALL FillWeatherPredefinedEntries
CALL FillRemainingPredefinedEntries
IF (WriteTabularFiles) THEN
  ! call each type of report in turn
  CALL WriteBEPSTable
  CALL WriteTableOfContents
  CALL WriteVeriSumTable
  CALL WriteDemandEndUseSummary
  CALL WriteSourceEnergyEndUseSummary
  CALL WritePredefinedTables
  CALL WriteComponentSizing
  CALL WriteSurfaceShadowing
  CALL WriteCompCostTable
  CALL WriteAdaptiveComfortTable
  CALL WriteZoneLoadComponentTable
  IF (DoWeathSim) THEN
    CALL WriteMonthlyTables
    CALL WriteTimeBinTables
  END IF
ENDIF
  EchoInputFile=FindUnitNumber('eplusout.audit')
  Write(EchoInputFile,*) 'MonthlyInputCount=',MonthlyInputCount
  Write(EchoInputFile,*) 'sizeMonthlyInput=',sizeMonthlyInput
  Write(EchoInputFile,*) 'MonthlyFieldSetInputCount=',MonthlyFieldSetInputCount
  Write(EchoInputFile,*) 'sizeMonthlyFieldSetInput=',sizeMonthlyFieldSetInput
  Write(EchoInputFile,*) 'MonthlyTablesCount=',MonthlyTablesCount
  Write(EchoInputFile,*) 'MonthlyColumnsCount=',MonthlyColumnsCount
  Write(EchoInputFile,*) 'sizeReportName=',sizeReportName
  Write(EchoInputFile,*) 'numReportName=',numReportName
  Write(EchoInputFile,*) 'sizeSubTable=',sizeSubTable
  Write(EchoInputFile,*) 'numSubTable=',numSubTable
  Write(EchoInputFile,*) 'sizeColumnTag=',sizeColumnTag
  Write(EchoInputFile,*) 'numColumnTag=',numColumnTag
  Write(EchoInputFile,*) 'sizeTableEntry=',sizeTableEntry
  Write(EchoInputFile,*) 'numTableEntry=',numTableEntry
  Write(EchoInputFile,*) 'sizeCompSizeTableEntry=',sizeCompSizeTableEntry
  Write(EchoInputFile,*) 'numCompSizeTableEntry=',numCompSizeTableEntry

RETURN
END SUBROUTINE WriteTabularReports

SUBROUTINE FillWeatherPredefinedEntries
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   Feb 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Read the STAT file for the active weather file and summarize in a predefined report.
          !   The stat file that is attached may have several formats -- from evolution of the
          !   stat file from the weather converter (or others that produce a similar stat file).

          ! METHODOLOGY EMPLOYED:
          !   na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputReportPredefined

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=1), PARAMETER :: degChar='°'

! LineTypes for reading the stat file
INTEGER, PARAMETER :: StatisticsLine=1
INTEGER, PARAMETER :: LocationLine=2
INTEGER, PARAMETER :: LatLongLine=3
INTEGER, PARAMETER :: ElevationLine=4
INTEGER, PARAMETER :: StdPressureLine=5
INTEGER, PARAMETER :: DataSourceLine=6
INTEGER, PARAMETER :: WMOStationLine=7
INTEGER, PARAMETER :: DesignConditionsLine=8
INTEGER, PARAMETER :: heatingConditionsLine=9
INTEGER, PARAMETER :: coolingConditionsLine=10
INTEGER, PARAMETER :: stdHDDLine=11
INTEGER, PARAMETER :: stdCDDLine=12
INTEGER, PARAMETER :: maxDryBulbLine=13
INTEGER, PARAMETER :: minDryBulbLine=14
INTEGER, PARAMETER :: maxDewPointLine=15
INTEGER, PARAMETER :: minDewPointLine=16
INTEGER, PARAMETER :: wthHDDLine=17
INTEGER, PARAMETER :: wthCDDLine=18
INTEGER, PARAMETER :: KoppenLine=19
INTEGER, PARAMETER :: KoppenDes1Line=20
INTEGER, PARAMETER :: KoppenDes2Line=21
INTEGER, PARAMETER :: AshStdLine=22
INTEGER, PARAMETER :: AshStdDes1Line=23
INTEGER, PARAMETER :: AshStdDes2Line=24
INTEGER, PARAMETER :: AshStdDes3Line=25

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER, EXTERNAL :: GetNewUnitNumber  ! External  function to "get" a unit number

CHARACTER(len=200) :: lineIn
INTEGER :: statFile
LOGICAL :: fileExists
INTEGER :: lineType = 0
INTEGER :: lineTypeinterim = 0
INTEGER :: readStat
LOGICAL :: isASHRAE
LOGICAL :: iscalc
LOGICAL :: isKoppen
INTEGER :: ashPtr
INTEGER :: lnPtr
INTEGER :: col1
INTEGER :: col2
INTEGER :: col3
INTEGER :: sposlt
INTEGER :: eposlt
INTEGER :: sposlg
INTEGER :: eposlg
INTEGER :: spostz
INTEGER :: epostz
character(len=5) ashDesYear
CHARACTER(2) :: ashZone   !ashrae climate zone
CHARACTER(len=MaxNameLength) :: curNameWithSIUnits
CHARACTER(len=MaxNameLength) :: curNameAndUnits
INTEGER :: indexUnitConv
CHARACTER(len=10) :: storeASHRAEHDD
CHARACTER(len=10) :: storeASHRAECDD
LOGICAL :: heatingDesignlinepassed
LOGICAL :: coolingDesignlinepassed
LOGICAL :: desConditionlinepassed

INQUIRE(file='in.stat',EXIST=fileExists)
readStat=0
isASHRAE=.false.
iscalc=.false.
isKoppen=.false.
heatingDesignlinepassed=.false.
coolingDesignlinepassed=.false.
desConditionlinepassed=.false.
storeASHRAEHDD=' '
storeASHRAECDD=' '
lineTypeinterim=0
IF (fileExists) THEN
  statFile = GetNewUnitNumber()
  OPEN (unit=statFile, file='in.stat', action='READ', iostat=readStat)
  IF (readStat /= 0) THEN
    CALL ShowFatalError('FillWeatherPredefinedEntries: Could not open file "in.stat" for input (read).')
  ENDIF
  DO WHILE (readStat == 0) !end of file, or error
    lineType=lineTypeinterim
    READ(UNIT=statFile,FMT='(A)',IOSTAT=readStat) lineIn
    ! reconcile line with different versions of stat file
    ! v7.1 added version as first line.
    lineIn=ADJUSTL(lineIn)
    if (lineIn(1:10) == 'Statistics') then
      lineType=StatisticsLine
    elseif (lineIn(1:8) == 'Location') then
      lineType=LocationLine
    elseif (lineIn(1:1) == '{') then
      lineType=LatLongLine
    elseif (lineIn(1:9) == 'Elevation') then
      lineType=ElevationLine
    elseif (lineIn(1:17) == 'Standard Pressure') then
      lineType=StdPressureLine
    elseif (lineIn(1:11) == 'Data Source') then
      lineType=DataSourceLine
    elseif (lineIn(1:11) == 'WMO Station') then
      lineType=WMOStationLine
    elseif (INDEX(lineIn,'Design Conditions') > 0) then
      if (.not. desConditionlinepassed) then
        desConditionlinepassed=.true.
        lineType=DesignConditionsLine
      endif
    elseif (lineIn(2:8) == 'Heating') then
      if (.not. heatingDesignlinepassed) then
        heatingDesignlinepassed=.true.
        lineType=heatingConditionsLine
      endif
    elseif (lineIn(2:8) == 'Cooling') then
      if (.not. coolingDesignlinepassed) then
        coolingDesignlinepassed=.true.
        lineType=coolingConditionsLine
      endif
    elseif (INDEX(lineIn,'(standard) heating degree-days (10°C baseline)') > 0) then
      lineType=stdHDDLine
    elseif (INDEX(lineIn,'(standard) cooling degree-days (18.3°C baseline)') > 0) then
      lineType=stdCDDLine
    elseif (INDEX(lineIn,'Maximum Dry Bulb') > 0) then
      lineType=maxDryBulbLine
    elseif (INDEX(lineIn,'Minimum Dry Bulb') > 0) then
      lineType=minDryBulbLine
    elseif (INDEX(lineIn,'Maximum Dew Point') > 0) then
      lineType=maxDewPointLine
    elseif (INDEX(lineIn,'Minimum Dew Point') > 0) then
      lineType=minDewPointLine
    elseif (INDEX(lineIn,'(wthr file) heating degree-days (10°C baseline)') > 0 .or. &
            INDEX(lineIn,'heating degree-days (10°C baseline)') > 0) then
      lineType=wthHDDLine
    elseif (INDEX(lineIn,'(wthr file) cooling degree-days (18°C baseline)') > 0 .or. &
            INDEX(lineIn,'cooling degree-days (18°C baseline)') > 0) then
      lineType=wthCDDLine
    endif
    ! these not part of big if/else because sequential
    if (lineType == KoppenDes1Line .and. isKoppen) lineType=KoppenDes2Line
    if (lineType == KoppenLine .and. isKoppen) lineType=KoppenDes1Line
    if (INDEX(lineIn,'(Köppen classification)') > 0) lineType=KoppenLine
    if (lineType == AshStdDes2Line) lineType=AshStdDes3Line
    if (lineType == AshStdDes1Line) lineType=AshStdDes2Line
    if (lineType == AshStdLine) lineType=AshStdDes1Line
    if (INDEX(lineIn,'ASHRAE Standards') > 0) lineType=AshStdLine

    SELECT CASE (lineType)
      CASE (StatisticsLine) ! Statistics for USA_CA_San.Francisco_TMY2
        CALL PreDefTableEntry(pdchWthrVal, 'Reference', lineIn(16:))
      CASE (LocationLine) ! Location -- SAN_FRANCISCO CA USA
        CALL PreDefTableEntry(pdchWthrVal, 'Site:Location', lineIn(12:))
      CASE (LatLongLine) !      {N 37° 37'} {W 122° 22'} {GMT -8.0 Hours}
        ! find the {}
        sposlt=INDEX(lineIn,'{')
        eposlt=INDEX(lineIn,'}')
        IF (sposlt > 0 .and. eposlt > 0) THEN
          CALL PreDefTableEntry(pdchWthrVal, 'Latitude', lineIn(sposlt:eposlt))
          ! redefine so next scan can go with {}
          lineIn(sposlt:sposlt)='['
          lineIn(eposlt:eposlt)=']'
        ELSE
          CALL PreDefTableEntry(pdchWthrVal, 'Latitude', 'not found')
        ENDIF
        sposlg=INDEX(lineIn,'{')
        eposlg=INDEX(lineIn,'}')
        IF (sposlg > 0 .and. eposlg > 0) THEN
          CALL PreDefTableEntry(pdchWthrVal, 'Longitude', lineIn(sposlg:eposlg))
          ! redefine so next scan can go with {}
          lineIn(sposlg:sposlg)='['
          lineIn(eposlg:eposlg)=']'
        ELSE
          CALL PreDefTableEntry(pdchWthrVal, 'Longitude', 'not found')
        ENDIF
        spostz=INDEX(lineIn,'{')
        epostz=INDEX(lineIn,'}')
        IF (spostz > 0 .and. epostz > 0) THEN
          CALL PreDefTableEntry(pdchWthrVal, 'Time Zone', lineIn(spostz:epostz))
          ! redefine so next scan can go with {}
          lineIn(spostz:spostz)='['
          lineIn(epostz:epostz)=']'
        ELSE
          CALL PreDefTableEntry(pdchWthrVal, 'Time Zone', 'not found')
        ENDIF
      CASE (ElevationLine) ! Elevation --     5m above sea level
        lnPtr=index(lineIn(13:),'m')
        if (lnPtr > 0) then
          curNameWithSIUnits = 'Elevation (m) '//lineIn(13+lnPtr+1:)
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(lineIn(13:13+lnPtr-2))),1)))
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameWithSIUnits), lineIn(13:13+lnPtr-2))
          ENDIF
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Elevation', 'not found')
        ENDIF
      CASE (StdPressureLine) ! Standard Pressure at Elevation -- 101265Pa
        CALL PreDefTableEntry(pdchWthrVal, 'Standard Pressure at Elevation', lineIn(35:))
      CASE (DataSourceLine) ! Data Source -- TMY2-23234
        CALL PreDefTableEntry(pdchWthrVal, 'Data Source', lineIn(16:))
      CASE (WMOStationLine) ! WMO Station 724940
        CALL PreDefTableEntry(pdchWthrVal, 'WMO Station', lineIn(13:))
      CASE (DesignConditionsLine) !  - Using Design Conditions from "Climate Design Data 2005 ASHRAE Handbook"
        ashPtr=INDEX(lineIn,'ASHRAE')
        IF (ashPtr .GT. 0) THEN
          isASHRAE = .TRUE.
          iscalc = .true.
          IF (ashPtr > 5) THEN !Objexx:BoundsViolation IF block added to protect against ashPtr<=5
             ashDesYear=lineIn(ashPtr-5:ashPtr-1)
          ELSE
             ashDesYear=''
          ENDIF
          CALL PreDefTableEntry(pdchWthrVal, 'Weather File Design Conditions ', 'Climate Design Data '//  &
             ashDesYear//'ASHRAE Handbook')
        ELSEIF (INDEX(lineIn,'not calculated') > 0 .or. lineIn == ' ') THEN
          iscalc = .false.
          CALL PreDefTableEntry(pdchWthrVal, 'Weather File Design Conditions ', 'not calculated, Number of days < 1 year')
        ELSE
          isASHRAE = .FALSE.
          iscalc = .true.
          CALL PreDefTableEntry(pdchWthrVal, 'Weather File Design Conditions ', 'Calculated from the weather file')
        END IF
      CASE (heatingConditionsLine) !  winter/heating design conditions
        IF (iscalc) THEN
          IF (isASHRAE) THEN
            IF (ashDesYear == '2001') THEN
              IF (unitsStyle .EQ. unitsStyleInchPound) THEN
                curNameWithSIUnits = 'Heating Design Temperature 99.6% (C)'
                CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
                CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,2))),1))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99% (F)',   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,3))),1))//degChar)
              ELSE
                CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99.6% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,2))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,3))//degChar)
              ENDIF
            ELSE  ! 2005 and 2009 are the same
              IF (unitsStyle .EQ. unitsStyleInchPound) THEN
                curNameWithSIUnits = 'Heating Design Temperature 99.6% (C)'
                CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
                CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,4))),1))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99% (F)',   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,5))),1))//degChar)
              ELSE
                CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99.6% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,4))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,5))//degChar)
              ENDIF
            ENDIF
          ELSE  ! from weather file
            IF (GetColumnUsingTabs(lineIn,5) == '  ') THEN
              col1=3
              col2=4
            ELSE
              col1=4
              col2=5
            ENDIF
            IF (unitsStyle .EQ. unitsStyleInchPound) THEN
              curNameWithSIUnits = 'Heating Design Temperature 99.6% (C)'
              CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
              CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
                 trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,col1))),1))//degChar)
              CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99% (F)',   &
                 trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,col2))),1))//degChar)
            ELSE
              CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99.6% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,col1))//degChar)
              CALL PreDefTableEntry(pdchWthrVal, 'Heating Design Temperature 99% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,col2))//degChar)
            ENDIF
          ENDIF
        ENDIF
      CASE (coolingConditionsLine) !  summer/cooling design conditions
        IF (iscalc) THEN
          IF (isASHRAE) THEN
            IF (ashDesYear == '2001') THEN
              IF (unitsStyle .EQ. unitsStyleInchPound) THEN
                curNameWithSIUnits = 'Cooling Design Temperature 0.4% (C)'
                CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
                CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,2))),1))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 1% (F)',   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,4))),1))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 2% (F)',   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,6))),1))//degChar)
              ELSE
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 0.4% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,2))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 1% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,4))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 2% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,6))//degChar)
              ENDIF
            ELSE  ! 2005 and 2009 are the same
              IF (unitsStyle .EQ. unitsStyleInchPound) THEN
                curNameWithSIUnits = 'Cooling Design Temperature 0.4% (C)'
                CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
                CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,5))),1))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 1% (F)',   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,7))),1))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 2% (F)',   &
                   trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,9))),1))//degChar)
              ELSE
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 0.4% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,5))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 1% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,7))//degChar)
                CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 2% (C)',   &
                   trim(GetColumnUsingTabs(lineIn,9))//degChar)
              ENDIF
            ENDIF
          ELSE ! from weather file
            IF (GetColumnUsingTabs(lineIn,6) == '  ') THEN
              col1=3
              col2=4
              col3=5
            ELSE
              col1=4
              col2=5
              col3=6
            ENDIF
            IF (unitsStyle .EQ. unitsStyleInchPound) THEN
              curNameWithSIUnits = 'Cooling Design Temperature 0.4% (C)'
              CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
              CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
                 trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,col1))),1))//degChar)
              CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 1% (F)',   &
                 trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,col2))),1))//degChar)
              CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 2% (F)',   &
                 trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(GetColumnUsingTabs(lineIn,col3))),1))//degChar)
            ELSE
              CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 0.4% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,col1))//degChar)
              CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 1% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,col2))//degChar)
              CALL PreDefTableEntry(pdchWthrVal, 'Cooling Design Temperature 2% (C)',   &
                 trim(GetColumnUsingTabs(lineIn,col3))//degChar)
            ENDIF
          END IF
        ENDIF
      CASE (stdHDDLine) !  - 1745 annual (standard) heating degree-days (10°C baseline)
        storeASHRAEHDD=lineIn(3:6)
      CASE (stdCDDLine) !  -  464 annual (standard) cooling degree-days (18.3°C baseline)
        storeASHRAECDD=lineIn(3:6)
      CASE (maxDryBulbLine) !   - Maximum Dry Bulb temperature of  35.6°C on Jul  9
        sposlt=INDEX(lineIn,'of')
        eposlt=INDEX(lineIn,'C')
        sposlt=sposlt+2
        eposlt=eposlt-2
        if (sposlt > 0 .and. eposlt > 0) then
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            curNameWithSIUnits = 'Maximum Dry Bulb Temperature (C)'
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(lineIn(sposlt:eposlt))),1))//degchar)
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dry Bulb Temperature (C)', lineIn(sposlt:eposlt)//degchar)
          ENDIF
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dry Bulb Temperature','not found')
        endif
        sposlt=INDEX(lineIn,'on')
        sposlt=sposlt+2
        if (sposlt > 0) then
          CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dry Bulb Occurs on', lineIn(sposlt:))
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dry Bulb Occurs on', 'not found')
        endif
      CASE (minDryBulbLine) !   - Minimum Dry Bulb temperature of -22.8°C on Jan  7
        sposlt=INDEX(lineIn,'of')
        eposlt=INDEX(lineIn,'C')
        sposlt=sposlt+2
        eposlt=eposlt-2
        if (sposlt > 0 .and. eposlt > 0) then
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            curNameWithSIUnits = 'Minimum Dry Bulb Temperature (C)'
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(lineIn(sposlt:eposlt))),1))//degchar)
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dry Bulb Temperature (C)', lineIn(sposlt:eposlt)//degchar)
          ENDIF
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dry Bulb Temperature','not found')
        endif
        sposlt=INDEX(lineIn,'on')
        sposlt=sposlt+2
        if (sposlt > 0) then
          CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dry Bulb Occurs on', lineIn(sposlt:))
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dry Bulb Occurs on', 'not found')
        endif
      CASE (maxDewPointLine) !   - Maximum Dew Point temperature of  25.6°C on Aug  4
        sposlt=INDEX(lineIn,'of')
        eposlt=INDEX(lineIn,'C')
        sposlt=sposlt+2
        eposlt=eposlt-2
        if (sposlt > 0 .and. eposlt > 0) then
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            curNameWithSIUnits = 'Maximum Dew Point Temperature (C)'
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(lineIn(sposlt:eposlt))),1))//degchar)
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dew Point Temperature (C)', lineIn(sposlt:eposlt)//degchar)
          ENDIF
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dew Point Temperature','not found')
        endif
        sposlt=INDEX(lineIn,'on')
        sposlt=sposlt+2
        if (sposlt > 0) then
          CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dew Point Occurs on', lineIn(sposlt:))
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Maximum Dew Point Occurs on', 'not found')
        endif
      CASE (minDewPointLine) !   - Minimum Dew Point temperature of -28.9°C on Dec 31
        sposlt=INDEX(lineIn,'of')
        eposlt=INDEX(lineIn,'C')
        sposlt=sposlt+2
        eposlt=eposlt-2
        if (sposlt > 0 .and. eposlt > 0) then
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            curNameWithSIUnits = 'Minimum Dew Point Temperature (C)'
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIP(indexUnitConv,StrToReal(lineIn(sposlt:eposlt))),1))//degchar)
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dew Point Temperature (C)', lineIn(sposlt:eposlt)//degchar)
          ENDIF
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dew Point Temperature','not found')
        endif
        sposlt=INDEX(lineIn,'on')
        sposlt=sposlt+2
        if (sposlt > 0) then
          CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dew Point Occurs on', lineIn(sposlt:))
        else
          CALL PreDefTableEntry(pdchWthrVal, 'Minimum Dew Point Occurs on', 'not found')
        endif
      CASE (wthHDDLine) !  - 1745 (wthr file) annual heating degree-days (10°C baseline)
        IF (storeASHRAEHDD /= ' ') THEN
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            curNameWithSIUnits = 'Standard Heating Degree-Days - base 50°(C)'
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIPDelta(indexUnitConv,StrToReal(storeASHRAEHDD)),1)))
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Standard Heating Degree-Days (base 10°C)', storeASHRAEHDD)
          ENDIF
        ELSE
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            CALL PreDefTableEntry(pdchWthrVal, 'Standard Heating Degree-Days (base 50°F)', 'not found')
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Standard Heating Degree-Days (base 10°C)', 'not found')
          ENDIF
        ENDIF
        IF (unitsStyle .EQ. unitsStyleInchPound) THEN
          curNameWithSIUnits = 'Weather File Heating Degree-Days - base 50°(C)'
          CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
          CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
             trim(RealToStr(ConvertIPDelta(indexUnitConv,StrToReal(lineIn(3:6))),1)))
          CALL PreDefTableEntry(pdchLeedGenData, 'Heating Degree Days',   &
             trim(RealToStr(ConvertIPDelta(indexUnitConv,StrToReal(lineIn(3:6))),1)))
        ELSE
          CALL PreDefTableEntry(pdchWthrVal, 'Weather File Heating Degree-Days (base 10°C)', lineIn(3:6))
          CALL PreDefTableEntry(pdchLeedGenData, 'Heating Degree Days', lineIn(3:6))
        ENDIF
      CASE (wthCDDLine) !  -  464 (wthr file) annual cooling degree-days (18°C baseline)
        IF (storeASHRAECDD /= ' ') THEN
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            curNameWithSIUnits = 'Standard Cooling Degree-Days - base 65°(C)'
            CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
            CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
               trim(RealToStr(ConvertIPDelta(indexUnitConv,StrToReal(storeASHRAECDD)),1)))
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Standard Cooling Degree-Days (base 18.3°C)', storeASHRAECDD)
          ENDIF
        ELSE
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            CALL PreDefTableEntry(pdchWthrVal, 'Standard Cooling Degree-Days (base 65°F)', 'not found')
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Standard Cooling Degree-Days (base 18.3°C)', 'not found')
          ENDIF
        ENDIF
        IF (unitsStyle .EQ. unitsStyleInchPound) THEN
          curNameWithSIUnits = 'Weather File Cooling Degree-Days - base 64.4°(C)'
          CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
          CALL PreDefTableEntry(pdchWthrVal, trim(curNameAndUnits),   &
             trim(RealToStr(ConvertIPDelta(indexUnitConv,StrToReal(lineIn(3:6))),1)))
          CALL PreDefTableEntry(pdchLeedGenData, 'Cooling Degree Days',   &
             trim(RealToStr(ConvertIPDelta(indexUnitConv,StrToReal(lineIn(3:6))),1)))
        ELSE
          CALL PreDefTableEntry(pdchWthrVal, 'Weather File Cooling Degree-Days (base 18°C)', lineIn(3:6))
          CALL PreDefTableEntry(pdchLeedGenData, 'Cooling Degree Days',lineIn(3:6))
        ENDIF
      CASE (KoppenLine) ! - Climate type "BSk" (Köppen classification)
        IF (INDEX(lineIn,'not shown') == 0) THEN
          isKoppen=.true.
          IF (lineIn(19:19) .EQ. '"') THEN  ! two character classification
            CALL PreDefTableEntry(pdchWthrVal, 'Köppen Classification', lineIn(17:18))
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Köppen Classification', lineIn(17:19))
          END IF
        ELSE
          isKoppen=.false.
          CALL PreDefTableEntry(pdchWthrVal, 'Köppen Recommendation', lineIn(3:))
        ENDIF
      CASE (KoppenDes1Line) ! - Tropical monsoonal or tradewind-coastal (short dry season, lat. 5-25°)
        IF (isKoppen) THEN
          CALL PreDefTableEntry(pdchWthrVal, 'Köppen Description', lineIn(3:))
        ENDIF
      CASE (KoppenDes2Line) ! - Unbearably humid periods in summer, but passive cooling is possible
        IF (isKoppen) THEN
          IF (LEN_TRIM(lineIn) .GT. 3) THEN ! avoid blank lines
            IF (lineIn(3:4) .NE. '**') THEN  ! avoid line with warning
              CALL PreDefTableEntry(pdchWthrVal, 'Köppen Recommendation', lineIn(3:))
            ELSE
              CALL PreDefTableEntry(pdchWthrVal, 'Köppen Recommendation', '')
            END IF
          ELSE
            CALL PreDefTableEntry(pdchWthrVal, 'Köppen Recommendation', '')
          END IF
        ENDIF
      CASE (AshStdLine,AshStdDes1Line,AshStdDes2Line,AshStdDes3Line)
                  !  - Climate type "1A" (ASHRAE Standards 90.1-2004 and 90.2-2004 Climate Zone)**
        IF (INDEX(lineIn,'Standards') .GT. 0) THEN
          ashZone = lineIn(17:18)
          if (ashZone(2:2) == '"') ashZone(2:2)=' '
          CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Climate Zone', ashZone)
          CALL PreDefTableEntry(pdchLeedGenData,'Climate Zone',ashZone)
          IF (ashZone .EQ. '1A') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Very Hot-Humid')
          ELSEIF (ashZone .EQ. '1B') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Very Hot-Dry')
          ELSEIF (ashZone .EQ. '2A') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Hot-Humid')
          ELSEIF (ashZone .EQ. '2B') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Hot-Dry')
          ELSEIF (ashZone .EQ. '3A') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Warm-Humid')
          ELSEIF (ashZone .EQ. '3B') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Warm-Dry')
          ELSEIF (ashZone .EQ. '3C') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Warm-Marine')
          ELSEIF (ashZone .EQ. '4A') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Mixed-Humid')
          ELSEIF (ashZone .EQ. '4B') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Mixed-Dry')
          ELSEIF (ashZone .EQ. '4C') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Mixed-Marine')
          ELSEIF (ashZone .EQ. '5A') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Cool-Humid')
          ELSEIF (ashZone .EQ. '5B') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Cool-Dry')
          ELSEIF (ashZone .EQ. '5C') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Cool-Marine')
          ELSEIF (ashZone .EQ. '6A') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Cold-Humid')
          ELSEIF (ashZone .EQ. '6B') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Cold-Dry')
          ELSEIF (ashZone .EQ. '7 ') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Very Cold')
          ELSEIF (ashZone .EQ. '8 ') THEN
            CALL PreDefTableEntry(pdchWthrVal, 'ASHRAE Description', 'Subarctic')
          END IF
        END IF
    END SELECT
    LineIn = ''
    lineTypeinterim=0
    if (lineType == AshStdDes3Line) lineTypeinterim=0
    if (lineType == AshStdDes2Line) lineTypeinterim=AshStdDes2Line
    if (lineType == AshStdDes1Line) lineTypeinterim=AshStdDes1Line
    if (lineType == AshStdLine) lineTypeinterim=AshStdLine
    if (lineType == KoppenDes2Line) lineTypeinterim=0
    if (lineType == KoppenDes1Line) lineTypeinterim=KoppenDes1Line
    if (lineType == KoppenLine) lineTypeinterim=KoppenLine
  END DO
  CLOSE(UNIT=statFile)
ENDIF
END SUBROUTINE FillWeatherPredefinedEntries

FUNCTION GetColumnUsingTabs(inString,colNum) RESULT (resultString)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Assumes that the input string contains tabs that mark the
          !   separation between columns. Returns the string that appears
          !   in the column specified.

          ! METHODOLOGY EMPLOYED:
          !   na

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: inString     ! Input String
INTEGER, INTENT(IN) :: colNum                   ! Column number
CHARACTER(len=LEN(inString)) :: resultString ! Result String

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(1),PARAMETER :: tb=CHAR(9) !tab character

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

CHARACTER(len=LEN(inString)) :: procIn ! processed input string
INTEGER :: startTab
INTEGER :: endTab
INTEGER :: inLen
INTEGER :: i

procIn = inString
inLen = LEN(procIn)
startTab = 0
endTab = INDEX(procIn,tb)
IF (endTab .GT. 0) THEN
  procIn(endTab:endTab) = ' ' !replace tab with space so next search doesn't find this tab again
ELSE
  endTab = inLen + 1 !one character past the end of string since substract one when extracting
END IF
DO i = 2,colNum !already have first column identified so do loop only if for column 2 or greater.
  startTab = endTab
  endTab = INDEX(procIn,tb)
  IF (endTab .GT. 0) THEN
    procIn(endTab:endTab) = ' ' !replace tab with space so next search doesn't find this tab again
  ELSE
    endTab = inLen + 1 !one character past the end of string since substract one when extracting
  END IF
END DO
IF (startTab .LT. endTab) THEN
  resultString = procIn(startTab+1:endTab-1) !extract but leave tab characters out
ELSE
  resultString = ''
END IF
END FUNCTION GetColumnUsingTabs

SUBROUTINE FillRemainingPredefinedEntries
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   May 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Just before writing the output reports, will gather up
          !   any additional report entries for the predefined reports.

          ! METHODOLOGY EMPLOYED:
          !   na

USE DataHeatBalance,   ONLY: Zone, TotLights, Lights, ZonePreDefRep, ZnAirRpt, BuildingPreDefRep, People, NumPeopleStatements
USE ExteriorEnergyUse, ONLY: ExteriorLights, NumExteriorLights, ScheduleOnly, AstroClockOverride
USE ScheduleManager,   ONLY: ScheduleAverageHoursPerWeek, GetScheduleName
USE DataEnvironment,   ONLY: RunPeriodStartDayOfWeek,CurrentYearIsLeapYear
USE DataHeatBalance,   ONLY: ZoneIntGain
USE DataHVACGlobals,   ONLY: NumPrimaryAirSys
USE DataOutputs,       ONLY: iNumberOfRecords,iNumberOfDefaultedFields,iTotalFieldsWithDefaults,  &
       iNumberOfAutosizedFields,iTotalAutoSizableFields,iNumberOfAutoCalcedFields,iTotalAutoCalculatableFields
USE ZonePlenum,        ONLY: NumZoneReturnPlenums, NumZoneSupplyPlenums
USE DataEnvironment,   ONLY : EnvironmentName, WeatherFileLocationTitle
USE DataErrorTracking, ONLY: TotalSevereErrors, TotalWarningErrors
USE General, ONLY: RoundSigDigits
USE DataAirflowNetwork,  ONLY : SimulateAirflowNetwork,AirflowNetworkControlMultizone,AirflowNetworkControlMultiADS
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iLight
INTEGER :: zonePt
INTEGER :: iZone
INTEGER :: jPeople
REAL(r64) :: totalVolume = 0.0d0
INTEGER :: numUncondZones = 0
INTEGER :: numCondZones = 0
INTEGER :: StartOfWeek
REAL(r64) :: HrsPerWeek = 0.0d0
REAL(r64) :: consumptionTotal
REAL(r64) :: convertJtoGJ
! sensible heat gain report totals
REAL(r64) :: totalHvacHt = 0.0d0
REAL(r64) :: totalHvacCl = 0.0d0
REAL(r64) :: totalSurfHt = 0.0d0
REAL(r64) :: totalSurfCl = 0.0d0
REAL(r64) :: totalPeoplAdd = 0.0d0
REAL(r64) :: totalLiteAdd = 0.0d0
REAL(r64) :: totalEquipAdd = 0.0d0
REAL(r64) :: totalWindAdd = 0.0d0
REAL(r64) :: totalIzaAdd = 0.0d0
REAL(r64) :: totalInfilAdd = 0.0d0
REAL(r64) :: totalOtherAdd = 0.0d0
REAL(r64) :: totalEquipRem = 0.0d0
REAL(r64) :: totalWindRem = 0.0d0
REAL(r64) :: totalIzaRem = 0.0d0
REAL(r64) :: totalInfilRem = 0.0d0
REAL(r64) :: totalOtherRem = 0.0d0

convertJtoGJ = 1.0d0/1000000000.0d0
StartOfWeek = RunPeriodStartDayOfWeek
IF (StartOfWeek .EQ. 0) StartOfWeek = 2 !if the first day of the week has not been set yet, assume monday

!Interior Connected Lighting Power
consumptionTotal = 0.0d0
DO iLight = 1, TotLights
  zonePt = Lights(iLight)%ZonePtr
  IF (Zone(zonePt)%SystemZoneNodeNumber .GT. 0) THEN                                           !conditioned y/n
    CALL PreDefTableEntry(pdchInLtCond,Lights(iLight)%Name,'Y')
  ELSE
    CALL PreDefTableEntry(pdchInLtCond,Lights(iLight)%Name,'N')
  END IF
  CALL PreDefTableEntry(pdchInLtAvgHrSchd,Lights(iLight)%Name,ScheduleAverageHoursPerWeek(Lights(iLight)%SchedPtr, &
                                                                          StartOfWeek,CurrentYearIsLeapYear))
  ! average operating hours per week
  IF (gatherElapsedTimeBEPS .GT. 0) THEN
    HrsPerWeek = 24 * 7 * Lights(iLight)%SumTimeNotZeroCons/gatherElapsedTimeBEPS
    CALL PreDefTableEntry(pdchInLtAvgHrOper,Lights(iLight)%Name,HrsPerWeek)
  END IF
  ! full load hours per week
  IF ((Lights(iLight)%DesignLevel * gatherElapsedTimeBEPS) .GT. 0) THEN
    HrsPerWeek = 24 * 7 * Lights(iLight)%SumConsumption/(Lights(iLight)%DesignLevel * gatherElapsedTimeBEPS * SecInHour)
    CALL PreDefTableEntry(pdchInLtFullLoadHrs,Lights(iLight)%Name,HrsPerWeek)
  END IF
  CALL PreDefTableEntry(pdchInLtConsump,Lights(iLight)%Name,Lights(iLight)%SumConsumption/1000000000.d0)
  consumptionTotal = consumptionTotal + Lights(iLight)%SumConsumption/1000000000.d0
END DO
CALL PreDefTableEntry(pdchInLtConsump,'Interior Lighting Total',consumptionTotal)

!Exterior Lighting
consumptionTotal = 0.0d0
DO iLight = 1, NumExteriorLights
  IF (ExteriorLights(iLight)%ControlMode .EQ. 1) THEN                          !photocell/schedule
    CALL PreDefTableEntry(pdchExLtAvgHrSchd,ExteriorLights(iLight)%Name,ScheduleAverageHoursPerWeek( &
                           ExteriorLights(iLight)%SchedPtr, StartOfWeek,CurrentYearIsLeapYear))
  END IF
  ! average operating hours per week
  IF (gatherElapsedTimeBEPS .GT. 0) THEN
    HrsPerWeek = 24 * 7 * ExteriorLights(iLight)%SumTimeNotZeroCons/gatherElapsedTimeBEPS
    CALL PreDefTableEntry(pdchExLtAvgHrOper,ExteriorLights(iLight)%Name,HrsPerWeek)
  END IF
  ! full load hours per week
  IF ((ExteriorLights(iLight)%DesignLevel * gatherElapsedTimeBEPS) .GT. 0) THEN
    HrsPerWeek = 24 * 7 * ExteriorLights(iLight)%SumConsumption/  &
       (ExteriorLights(iLight)%DesignLevel * gatherElapsedTimeBEPS * SecInHour)
    CALL PreDefTableEntry(pdchExLtFullLoadHrs,ExteriorLights(iLight)%Name,HrsPerWeek)
  END IF
  CALL PreDefTableEntry(pdchExLtConsump,ExteriorLights(iLight)%Name,ExteriorLights(iLight)%SumConsumption/1000000000.d0)
  consumptionTotal = consumptionTotal + ExteriorLights(iLight)%SumConsumption/1000000000.d0
END DO
CALL PreDefTableEntry(pdchExLtConsump,'Exterior Lighting Total',consumptionTotal)

!outside air ventilation
DO iZone = 1, NumOfZones
  IF (Zone(iZone)%SystemZoneNodeNumber .GE. 0) THEN !conditioned zones only
    IF (Zone(iZone)%isNominalOccupied) THEN
      !occupants
      IF (ZonePreDefRep(iZone)%NumOccAccumTime .GT. 0) THEN
        CALL PreDefTableEntry(pdchOaoAvgNumOcc1, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%NumOccAccum / ZonePreDefRep(iZone)%NumOccAccumTime)
        CALL PreDefTableEntry(pdchOaoAvgNumOcc2, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%NumOccAccum / ZonePreDefRep(iZone)%NumOccAccumTime)
      END IF
      !Mechanical ventilation
      IF (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0) THEN
        CALL PreDefTableEntry(pdchOaoAvgMechVent, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%MechVentVolTotal / (ZonePreDefRep(iZone)%TotTimeOcc * Zone(iZone)%volume &
                      * Zone(iZone)%Multiplier * Zone(iZone)%ListMultiplier),3)
      END IF
      IF ((Zone(iZone)%volume .GT. 0) .AND. (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0)) THEN
        CALL PreDefTableEntry(pdchOaoMinMechVent, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%MechVentVolMin / Zone(iZone)%volume * Zone(iZone)%Multiplier * Zone(iZone)%ListMultiplier,3)
      END IF
      !infiltration
      IF (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0) THEN
        CALL PreDefTableEntry(pdchOaoAvgInfil, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%InfilVolTotal / (ZonePreDefRep(iZone)%TotTimeOcc * Zone(iZone)%volume),3)
      END IF
      IF ((Zone(iZone)%volume .GT. 0) .AND. (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0)) THEN
        CALL PreDefTableEntry(pdchOaoMinInfil, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%InfilVolMin / Zone(iZone)%volume,3)
      END IF
      !AFN infiltration -- check that afn sim is being done.
      if (SimulateAirflowNetwork .lt. AirflowNetworkControlMultizone) then
        ZonePreDefRep(iZone)%AFNInfilVolMin =0.0d0
        ZonePreDefRep(iZone)%AFNInfilVolTotal = 0.0d0
        If (.NOT. (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. &
                   SimulateAirflowNetwork == AirflowNetworkControlMultiADS)) THEN
          ZonePreDefRep(iZone)%AFNInfilVolMin =0.0d0
          ZonePreDefRep(iZone)%AFNInfilVolTotal = 0.0d0
        endif
      endif
      IF (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0) THEN
        CALL PreDefTableEntry(pdchOaoAvgAFNInfil, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%AFNInfilVolTotal / (ZonePreDefRep(iZone)%TotTimeOcc * Zone(iZone)%volume),3)
      END IF
      IF ((Zone(iZone)%volume .GT. 0) .AND. (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0)) THEN
        CALL PreDefTableEntry(pdchOaoMinAFNInfil, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%AFNInfilVolMin / Zone(iZone)%volume,3)
      END IF
      !simple 'ZoneVentilation'
      IF (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0) THEN
        CALL PreDefTableEntry(pdchOaoAvgSimpVent, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%SimpVentVolTotal / (ZonePreDefRep(iZone)%TotTimeOcc * Zone(iZone)%volume),3)
      END IF
      IF ((Zone(iZone)%volume .GT. 0) .AND. (ZonePreDefRep(iZone)%TotTimeOcc .GT. 0)) THEN
        CALL PreDefTableEntry(pdchOaoMinSimpVent, Zone(iZone)%Name, &
          ZonePreDefRep(iZone)%SimpVentVolMin / Zone(iZone)%volume,3)
      END IF


      !Zone volume
      CALL PreDefTableEntry(pdchOaoZoneVol1, Zone(iZone)%Name, Zone(iZone)%Volume)
      CALL PreDefTableEntry(pdchOaoZoneVol2, Zone(iZone)%Name, Zone(iZone)%Volume)
      totalVolume = totalVolume + Zone(iZone)%Volume
    END IF
  END IF
END DO

! Add the number of central air distributions system to the count report
CALL PreDefTableEntry(pdchHVACcntVal, 'HVAC Air Loops',NumPrimaryAirSys)
! Add the number of conditioned and unconditioned zones to the count report
DO iZone = 1, NumOfZones
  IF (Zone(iZone)%SystemZoneNodeNumber .GT. 0) THEN !conditioned zones only
    numCondZones = numCondZones + 1
  ELSE
    numUncondZones = numUncondZones + 1
  END IF
END DO
CALL PreDefTableEntry(pdchHVACcntVal, 'Conditioned Zones',numCondZones)
CALL PreDefTableEntry(pdchHVACcntVal, 'Unconditioned Zones',numUncondZones)
!add the number of plenums to the count report
CALL PreDefTableEntry(pdchHVACcntVal, 'Supply Plenums',NumZoneSupplyPlenums)
CALL PreDefTableEntry(pdchHVACcntVal, 'Return Plenums',NumZoneReturnPlenums)

! Started to create a total row but did not fully implement
!CALL PreDefTableEntry(pdchOaoZoneVol1,'Total OA Avg', totalVolume)
!CALL PreDefTableEntry(pdchOaoZoneVol2,'Total OA Min', totalVolume)

! Add footnote saying if it is a design day or other kind of environment

! Field counts
CALL PreDefTableEntry(pdchFieldCntVal,'IDF Objects',iNumberOfRecords)
CALL PreDefTableEntry(pdchFieldCntVal,'Defaulted Fields',iNumberOfDefaultedFields)
CALL PreDefTableEntry(pdchFieldCntVal,'Fields with Defaults',iTotalFieldsWithDefaults)
CALL PreDefTableEntry(pdchFieldCntVal,'Autosized Fields',iNumberOfAutosizedFields)
CALL PreDefTableEntry(pdchFieldCntVal,'Autosizable Fields',iTotalAutoSizableFields)
CALL PreDefTableEntry(pdchFieldCntVal,'Autocalculated Fields',iNumberOfAutoCalcedFields)
CALL PreDefTableEntry(pdchFieldCntVal,'Autocalculatable Fields',iTotalAutoCalculatableFields)

DO iZone = 1, NumOfZones
  !annual
  CALL PreDefTableEntry(pdchSHGSAnHvacHt,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnHvacHt * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnHvacCl,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnHvacCl * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnSurfHt,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnSurfHt * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnSurfCl,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnSurfCl * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnPeoplAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnPeoplAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnLiteAdd,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnLiteAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnEquipAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnEquipAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnWindAdd,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnWindAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnIzaAdd,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnIzaAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnInfilAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnInfilAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnOtherAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnOtherAdd * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnEquipRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnEquipRem * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnWindRem,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnWindRem * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnIzaRem,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnIzaRem * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnInfilRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnInfilRem * convertJtoGJ,3)
  CALL PreDefTableEntry(pdchSHGSAnOtherRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSAnOtherRem * convertJtoGJ,3)
  !peak cooling
  CALL PreDefTableEntry(pdchSHGSClTimePeak, Zone(iZone)%Name, DateToString(ZonePreDefRep(iZone)%clPtTimeStamp))
  CALL PreDefTableEntry(pdchSHGSClHvacHt,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClHvacHt)
  CALL PreDefTableEntry(pdchSHGSClHvacCl,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClHvacCl)
  CALL PreDefTableEntry(pdchSHGSClSurfHt,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClSurfHt)
  CALL PreDefTableEntry(pdchSHGSClSurfCl,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClSurfCl)
  CALL PreDefTableEntry(pdchSHGSClPeoplAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClPeoplAdd)
  CALL PreDefTableEntry(pdchSHGSClLiteAdd,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClLiteAdd)
  CALL PreDefTableEntry(pdchSHGSClEquipAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClEquipAdd)
  CALL PreDefTableEntry(pdchSHGSClWindAdd,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClWindAdd)
  CALL PreDefTableEntry(pdchSHGSClIzaAdd,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClIzaAdd)
  CALL PreDefTableEntry(pdchSHGSClInfilAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClInfilAdd)
  CALL PreDefTableEntry(pdchSHGSClOtherAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClOtherAdd)
  CALL PreDefTableEntry(pdchSHGSClEquipRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClEquipRem)
  CALL PreDefTableEntry(pdchSHGSClWindRem,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClWindRem)
  CALL PreDefTableEntry(pdchSHGSClIzaRem,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClIzaRem)
  CALL PreDefTableEntry(pdchSHGSClInfilRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClInfilRem)
  CALL PreDefTableEntry(pdchSHGSClOtherRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSClOtherRem)
  !peak heating
  CALL PreDefTableEntry(pdchSHGSHtTimePeak, Zone(iZone)%Name, DateToString(ZonePreDefRep(iZone)%htPtTimeStamp))
  CALL PreDefTableEntry(pdchSHGSHtHvacHt,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtHvacHt)
  CALL PreDefTableEntry(pdchSHGSHtHvacCl,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtHvacCl)
  CALL PreDefTableEntry(pdchSHGSHtSurfHt,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtSurfHt)
  CALL PreDefTableEntry(pdchSHGSHtSurfCl,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtSurfCl)
  CALL PreDefTableEntry(pdchSHGSHtPeoplAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtPeoplAdd)
  CALL PreDefTableEntry(pdchSHGSHtLiteAdd,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtLiteAdd)
  CALL PreDefTableEntry(pdchSHGSHtEquipAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtEquipAdd)
  CALL PreDefTableEntry(pdchSHGSHtWindAdd,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtWindAdd)
  CALL PreDefTableEntry(pdchSHGSHtIzaAdd,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtIzaAdd)
  CALL PreDefTableEntry(pdchSHGSHtInfilAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtInfilAdd)
  CALL PreDefTableEntry(pdchSHGSHtOtherAdd, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtOtherAdd)
  CALL PreDefTableEntry(pdchSHGSHtEquipRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtEquipRem)
  CALL PreDefTableEntry(pdchSHGSHtWindRem,  Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtWindRem)
  CALL PreDefTableEntry(pdchSHGSHtIzaRem,   Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtIzaRem)
  CALL PreDefTableEntry(pdchSHGSHtInfilRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtInfilRem)
  CALL PreDefTableEntry(pdchSHGSHtOtherRem, Zone(iZone)%Name, ZonePreDefRep(iZone)%SHGSHtOtherRem)
END DO
!totals for annual report
DO iZone = 1, NumOfZones
  totalHvacHt = totalHvacHt + ZonePreDefRep(iZone)%SHGSAnHvacHt
  totalHvacCl = totalHvacCl + ZonePreDefRep(iZone)%SHGSAnHvacCl
  totalSurfHt = totalSurfHt + ZonePreDefRep(iZone)%SHGSAnSurfHt
  totalSurfCl = totalSurfCl + ZonePreDefRep(iZone)%SHGSAnSurfCl
  totalPeoplAdd = totalPeoplAdd + ZonePreDefRep(iZone)%SHGSAnPeoplAdd
  totalLiteAdd = totalLiteAdd + ZonePreDefRep(iZone)%SHGSAnLiteAdd
  totalEquipAdd = totalEquipAdd + ZonePreDefRep(iZone)%SHGSAnEquipAdd
  totalWindAdd = totalWindAdd + ZonePreDefRep(iZone)%SHGSAnWindAdd
  totalIzaAdd = totalIzaAdd + ZonePreDefRep(iZone)%SHGSAnIzaAdd
  totalInfilAdd = totalInfilAdd + ZonePreDefRep(iZone)%SHGSAnInfilAdd
  totalOtherAdd = totalOtherAdd + ZonePreDefRep(iZone)%SHGSAnOtherAdd
  totalEquipRem = totalEquipRem + ZonePreDefRep(iZone)%SHGSAnEquipRem
  totalWindRem = totalWindRem + ZonePreDefRep(iZone)%SHGSAnWindRem
  totalIzaRem = totalIzaRem + ZonePreDefRep(iZone)%SHGSAnIzaRem
  totalInfilRem = totalInfilRem + ZonePreDefRep(iZone)%SHGSAnInfilRem
  totalOtherRem = totalOtherRem + ZonePreDefRep(iZone)%SHGSAnOtherRem
END DO
CALL PreDefTableEntry(pdchSHGSAnHvacHt,   'Total Facility', totalHvacHt * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnHvacCl,   'Total Facility', totalHvacCl * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnSurfHt,   'Total Facility', totalSurfHt * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnSurfCl,   'Total Facility', totalSurfCl * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnPeoplAdd, 'Total Facility', totalPeoplAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnLiteAdd,  'Total Facility', totalLiteAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnEquipAdd, 'Total Facility', totalEquipAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnWindAdd,  'Total Facility', totalWindAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnIzaAdd,   'Total Facility', totalIzaAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnInfilAdd, 'Total Facility', totalInfilAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnOtherAdd, 'Total Facility', totalOtherAdd * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnEquipRem, 'Total Facility', totalEquipRem * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnWindRem,  'Total Facility', totalWindRem * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnIzaRem,   'Total Facility', totalIzaRem * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnInfilRem, 'Total Facility', totalInfilRem * convertJtoGJ,3)
CALL PreDefTableEntry(pdchSHGSAnOtherRem, 'Total Facility', totalOtherRem * convertJtoGJ,3)
! building level results for peak cooling
CALL PreDefTableEntry(pdchSHGSClTimePeak, 'Total Facility', DateToString(BuildingPreDefRep%clPtTimeStamp))
CALL PreDefTableEntry(pdchSHGSClHvacHt,   'Total Facility', BuildingPreDefRep%SHGSClHvacHt)
CALL PreDefTableEntry(pdchSHGSClHvacCl,   'Total Facility', BuildingPreDefRep%SHGSClHvacCl)
CALL PreDefTableEntry(pdchSHGSClSurfHt,   'Total Facility', BuildingPreDefRep%SHGSClSurfHt)
CALL PreDefTableEntry(pdchSHGSClSurfCl,   'Total Facility', BuildingPreDefRep%SHGSClSurfCl)
CALL PreDefTableEntry(pdchSHGSClPeoplAdd, 'Total Facility', BuildingPreDefRep%SHGSClPeoplAdd)
CALL PreDefTableEntry(pdchSHGSClLiteAdd,  'Total Facility', BuildingPreDefRep%SHGSClLiteAdd)
CALL PreDefTableEntry(pdchSHGSClEquipAdd, 'Total Facility', BuildingPreDefRep%SHGSClEquipAdd)
CALL PreDefTableEntry(pdchSHGSClWindAdd,  'Total Facility', BuildingPreDefRep%SHGSClWindAdd)
CALL PreDefTableEntry(pdchSHGSClIzaAdd,   'Total Facility', BuildingPreDefRep%SHGSClIzaAdd)
CALL PreDefTableEntry(pdchSHGSClInfilAdd, 'Total Facility', BuildingPreDefRep%SHGSClInfilAdd)
CALL PreDefTableEntry(pdchSHGSClOtherAdd, 'Total Facility', BuildingPreDefRep%SHGSClOtherAdd)
CALL PreDefTableEntry(pdchSHGSClEquipRem, 'Total Facility', BuildingPreDefRep%SHGSClEquipRem)
CALL PreDefTableEntry(pdchSHGSClWindRem,  'Total Facility', BuildingPreDefRep%SHGSClWindRem)
CALL PreDefTableEntry(pdchSHGSClIzaRem,   'Total Facility', BuildingPreDefRep%SHGSClIzaRem)
CALL PreDefTableEntry(pdchSHGSClInfilRem, 'Total Facility', BuildingPreDefRep%SHGSClInfilRem)
CALL PreDefTableEntry(pdchSHGSClOtherRem, 'Total Facility', BuildingPreDefRep%SHGSClOtherRem)
! building level results for peak heating
CALL PreDefTableEntry(pdchSHGSHtTimePeak, 'Total Facility', DateToString(BuildingPreDefRep%htPtTimeStamp))
CALL PreDefTableEntry(pdchSHGSHtHvacHt,   'Total Facility', BuildingPreDefRep%SHGSHtHvacHt)
CALL PreDefTableEntry(pdchSHGSHtHvacCl,   'Total Facility', BuildingPreDefRep%SHGSHtHvacCl)
CALL PreDefTableEntry(pdchSHGSHtSurfHt,   'Total Facility', BuildingPreDefRep%SHGSHtSurfHt)
CALL PreDefTableEntry(pdchSHGSHtSurfCl,   'Total Facility', BuildingPreDefRep%SHGSHtSurfCl)
CALL PreDefTableEntry(pdchSHGSHtPeoplAdd, 'Total Facility', BuildingPreDefRep%SHGSHtPeoplAdd)
CALL PreDefTableEntry(pdchSHGSHtLiteAdd,  'Total Facility', BuildingPreDefRep%SHGSHtLiteAdd)
CALL PreDefTableEntry(pdchSHGSHtEquipAdd, 'Total Facility', BuildingPreDefRep%SHGSHtEquipAdd)
CALL PreDefTableEntry(pdchSHGSHtWindAdd,  'Total Facility', BuildingPreDefRep%SHGSHtWindAdd)
CALL PreDefTableEntry(pdchSHGSHtIzaAdd,   'Total Facility', BuildingPreDefRep%SHGSHtIzaAdd)
CALL PreDefTableEntry(pdchSHGSHtInfilAdd, 'Total Facility', BuildingPreDefRep%SHGSHtInfilAdd)
CALL PreDefTableEntry(pdchSHGSHtOtherAdd, 'Total Facility', BuildingPreDefRep%SHGSHtOtherAdd)
CALL PreDefTableEntry(pdchSHGSHtEquipRem, 'Total Facility', BuildingPreDefRep%SHGSHtEquipRem)
CALL PreDefTableEntry(pdchSHGSHtWindRem,  'Total Facility', BuildingPreDefRep%SHGSHtWindRem)
CALL PreDefTableEntry(pdchSHGSHtIzaRem,   'Total Facility', BuildingPreDefRep%SHGSHtIzaRem)
CALL PreDefTableEntry(pdchSHGSHtInfilRem, 'Total Facility', BuildingPreDefRep%SHGSHtInfilRem)
CALL PreDefTableEntry(pdchSHGSHtOtherRem, 'Total Facility', BuildingPreDefRep%SHGSHtOtherRem)

! LEED Report
!
! 1.1A-General Information
!CALL PreDefTableEntry(pdchLeedGenData,'Principal Heating Source','-')
IF (EnvironmentName == WeatherFileLocationTitle) THEN
  CALL PreDefTableEntry(pdchLeedGenData,'Weather File',TRIM(EnvironmentName))
ELSE
  CALL PreDefTableEntry(pdchLeedGenData,'Weather File',TRIM(EnvironmentName)//' ** '//TRIM(WeatherFileLocationTitle))
ENDIF

!CALL PreDefTableEntry(pdchLeedGenData,'Climate Zone','-')
!CALL PreDefTableEntry(pdchLeedGenData,'Heating Degree Days','-')
!CALL PreDefTableEntry(pdchLeedGenData,'Cooling Degree Days','-')
CALL PreDefTableEntry(pdchLeedGenData,'HDD and CDD data source','Weather File Stat')
CALL PreDefTableEntry(pdchLeedGenData,'Total gross floor area [m2]','-')

END SUBROUTINE FillRemainingPredefinedEntries

SUBROUTINE WriteMonthlyTables
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Set up the monthly tabular report results

          ! METHODOLOGY EMPLOYED:
          !   Creates several arrays that are passed to the writeTable
          !   routine.  All arrays are strings so numbers need to be
          !   converted prior to calling writeTable.

USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=MaxNameLength*2), ALLOCATABLE, DIMENSION(:)   :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                          :: columnWidth
CHARACTER(len=MaxNameLength), DIMENSION(16)               :: rowHead
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:,:) :: tableBody
CHARACTER(len=MaxNameLength), DIMENSION(13)               :: aggString
CHARACTER(len=MaxNameLength)                              :: curAggString
INTEGER    :: iInput
INTEGER    :: jTable
INTEGER    :: kColumn
INTEGER    :: lMonth
INTEGER    :: curTable
INTEGER    :: curCol
REAL(r64)  :: curVal
REAL(r64)  :: curConversionFactor
REAL(r64)  :: curConversionOffset = 0.0d0
INTEGER    :: columnUsedCount
INTEGER    :: columnRecount
INTEGER    :: digitsShown
REAL(r64)  :: minVal,maxVal,sumVal,sumDuration
CHARACTER(len=MaxNameLength) :: curUnits
CHARACTER(len=MaxNameLength) :: energyUnitsString
REAL(r64)  :: energyUnitsConversionFactor
INTEGER    :: indexUnitConv
CHARACTER(len=MaxNameLength) :: varNameWithUnits
REAL(r64)  :: veryLarge
REAL(r64)  :: verySmall

rowHead(1)  =  'January'
rowHead(2)  =  'February'
rowHead(3)  =  'March'
rowHead(4)  =  'April'
rowHead(5)  =  'May'
rowHead(6)  =  'June'
rowHead(7)  =  'July'
rowHead(8)  =  'August'
rowHead(9)  =  'September'
rowHead(10) =  'October'
rowHead(11) =  'November'
rowHead(12) =  'December'
rowHead(13) =  ''
rowHead(14) =  'Annual Sum or Average'
rowHead(15) =  'Minimum of Months'
rowHead(16) =  'Maximum of Months'

aggString(aggTypeSumOrAvg)                     = ''
aggString(aggTypeMaximum)                      = ' Maximum '
aggString(aggTypeMinimum)                      = ' MINIMUM '
aggString(aggTypeValueWhenMaxMin)              = ' AT MAX/MIN '
aggString(aggTypeHoursZero)                    = ' HOURS ZERO '
aggString(aggTypeHoursNonZero)                 = ' HOURS NON-ZERO '
aggString(aggTypeHoursPositive)                = ' HOURS POSITIVE '
aggString(aggTypeHoursNonPositive)             = ' HOURS NON-POSITIVE '
aggString(aggTypeHoursNegative)                = ' HOURS NEGATIVE '
aggString(aggTypeHoursNonNegative)             = ' HOURS NON-NEGATIVE '
aggString(aggTypeSumOrAverageHoursShown)       = ' FOR HOURS SHOWN '
aggString(aggTypeMaximumDuringHoursShown)      = ' MAX FOR HOURS SHOWN '
aggString(aggTypeMinimumDuringHoursShown)      = ' MIN FOR HOURS SHOWN '

veryLarge = 1.0D280
verySmall = -1.0D280

! set the unit conversion
SELECT CASE (unitsStyle)
  CASE (unitsStyleNone)
    energyUnitsString = 'J  '
    energyUnitsConversionFactor = 1.0d0
  CASE (unitsStyleJtoKWH)
    energyUnitsString = 'kWh'
    energyUnitsConversionFactor = 1.0d0/3600000.0d0
  CASE (unitsStyleJtoMJ)
    energyUnitsString = 'MJ '
    energyUnitsConversionFactor = 1.0d0/1000000.0d0
  CASE (unitsStyleJtoGJ)
    energyUnitsString = 'GJ '
    energyUnitsConversionFactor = 1.0d0/1000000000.0d0
END SELECT

! loop through each input to get the name of the tables
DO iInput = 1, MonthlyInputCount
  ! loop through each report and
  digitsShown = MonthlyInput(iInput)%showDigits
  DO jTable = 1 , MonthlyInput(iInput)%numTables
    curTable =jTable + MonthlyInput(iInput)%firstTable - 1
    ! first loop through and count how many 'columns' are defined
    ! since max and min actually define two columns (the value
    ! and the timestamp).
    columnUsedCount = 0
    DO kColumn = 1, MonthlyTables(curTable)%numColumns
      curCol = kColumn + MonthlyTables(curTable)%firstColumn - 1
      SELECT CASE (MonthlyColumns(curCol)%aggType)
        CASE (aggTypeSumOrAvg, aggTypeValueWhenMaxMin, &
              aggTypeHoursZero, aggTypeHoursNonZero, &
              aggTypeHoursPositive, aggTypeHoursNonPositive, &
              aggTypeHoursNegative, aggTypeHoursNonNegative, &
              aggTypeSumOrAverageHoursShown)
          columnUsedCount = columnUsedCount + 1
        CASE (aggTypeMaximum,aggTypeMinimum, &
              aggTypeMaximumDuringHoursShown, aggTypeMinimumDuringHoursShown)
          columnUsedCount = columnUsedCount + 2
      END SELECT
    END DO !jColumn
    ALLOCATE (columnHead(columnUsedCount))
    ALLOCATE (columnWidth(columnUsedCount))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE (tableBody(16,columnUsedCount))
    tableBody = "" !set entire table to blank as default
    columnRecount = 0
    DO kColumn = 1, MonthlyTables(curTable)%numColumns
      curCol = kColumn + MonthlyTables(curTable)%firstColumn - 1
      curAggString = aggString(MonthlyColumns(curCol)%aggType)
      IF (LEN_TRIM(curAggString) .GT. 0) THEN
        curAggString = ' {' // TRIM(ADJUSTL(curAggString)) // '}'
      END IF
      !do the unit conversions
      IF (unitsStyle .EQ. unitsStyleInchPound) THEN
        varNameWithUnits = TRIM(MonthlyColumns(curCol)%varName) // '[' // &
                TRIM(MonthlyColumns(curCol)%units)  // ']'
        CALL LookupSItoIP(varNameWithUnits, indexUnitConv, curUnits)
        CALL GetUnitConversion(indexUnitConv, curConversionFactor, curConversionOffset, curUnits)
      ELSE !just do the Joule conversion
        !if units is in Joules, convert if specified
        IF (sameString(MonthlyColumns(curCol)%units,'J')) THEN
          curUnits = energyUnitsString
          curConversionFactor = energyUnitsConversionFactor
          curConversionOffset = 0.0d0
        ELSE !if not joules don't perform conversion
          curUnits = MonthlyColumns(curCol)%units
          curConversionFactor = 1.0d0
          curConversionOffset = 0.0d0
        END IF
      END IF
      SELECT CASE (MonthlyColumns(curCol)%aggType)
        CASE (aggTypeSumOrAvg,aggTypeSumOrAverageHoursShown)
          columnRecount = columnRecount + 1
          ! put in the name of the variable for the column
          columnHead(columnRecount) = TRIM(MonthlyColumns(curCol)%varName) // TRIM(curAggString) // &
                                      ' [' // TRIM(curUnits)  // ']'
          sumVal = 0.0d0
          sumDuration = 0.0d0
          maxVal = -HUGE(maxVal)
          minVal = HUGE(maxVal)
          DO lMonth = 1, 12
            IF (MonthlyColumns(curCol)%avgSum .EQ. isAverage) THEN ! if it is a average variable divide by duration
              IF (MonthlyColumns(curCol)%duration(lMonth) .NE. 0) THEN
                curVal = ((MonthlyColumns(curCol)%reslt(lMonth) / MonthlyColumns(curCol)%duration(lMonth)) &
                         * curConversionFactor) + curConversionOffset
              ELSE
                curVal = 0.0d0
              END IF
              sumVal = sumVal + (MonthlyColumns(curCol)%reslt(lMonth) * curConversionFactor) + curConversionOffset
              sumDuration = sumDuration + MonthlyColumns(curCol)%duration(lMonth)
            ELSE
              curVal = (MonthlyColumns(curCol)%reslt(lMonth) * curConversionFactor) + curConversionOffset
              sumVal = sumVal + curVal
            END IF
            IF (IsMonthGathered(lMonth)) THEN
              tableBody(lMonth,columnRecount) = TRIM(RealToStr(curVal,digitsShown))
              IF (curVal .GT. maxVal) maxVal = curVal
              IF (curVal .LT. minVal) minVal = curVal
            ELSE
              tableBody(lMonth,columnRecount) = '-'
            END IF
          END DO !lMonth
          ! add the summary to bottom
          IF (MonthlyColumns(curCol)%avgSum .EQ. isAverage) THEN ! if it is a average variable divide by duration
            IF (sumDuration .GT. 0) THEN
              tableBody(14,columnRecount) = TRIM(RealToStr(sumVal/sumDuration,digitsShown))
            ELSE
              tableBody(14,columnRecount) = ''
            END IF
          ELSE
            tableBody(14,columnRecount) = TRIM(RealToStr(sumVal,digitsShown))
          ENDIF
          IF (minVal .NE. HUGE(maxVal)) THEN
            tableBody(15,columnRecount) = TRIM(RealToStr(minVal,digitsShown))
          END IF
          IF (maxVal .NE. -HUGE(maxVal)) THEN
            tableBody(16,columnRecount) = TRIM(RealToStr(maxVal,digitsShown))
          END IF
        CASE (aggTypeHoursZero, aggTypeHoursNonZero, &
              aggTypeHoursPositive, aggTypeHoursNonPositive, &
              aggTypeHoursNegative, aggTypeHoursNonNegative)

          columnRecount = columnRecount + 1
          ! put in the name of the variable for the column
          columnHead(columnRecount) = TRIM(MonthlyColumns(curCol)%varName) // TRIM(curAggString) // ' [HOURS]'
          sumVal = 0.0d0
          maxVal = -HUGE(maxVal)
          minVal = HUGE(maxVal)
          DO lMonth = 1, 12
            curVal = MonthlyColumns(curCol)%reslt(lMonth)
            IF (IsMonthGathered(lMonth)) THEN
              tableBody(lMonth,columnRecount) = TRIM(RealToStr(curVal,digitsShown))
              sumVal = sumVal + curVal
              IF (curVal .GT. maxVal) maxVal = curVal
              IF (curVal .LT. minVal) minVal = curVal
            ELSE
              tableBody(lMonth,columnRecount) = '-'
            END IF
          END DO !lMonth
          ! add the summary to bottom
          tableBody(14,columnRecount) = TRIM(RealToStr(sumVal,digitsShown))
          IF (minVal .NE. HUGE(maxVal)) THEN
            tableBody(15,columnRecount) = TRIM(RealToStr(minVal,digitsShown))
          END IF
          IF (maxVal .NE. -HUGE(maxVal)) THEN
            tableBody(16,columnRecount) = TRIM(RealToStr(maxVal,digitsShown))
          END IF
        CASE (aggTypeValueWhenMaxMin)
          columnRecount = columnRecount + 1
          IF (MonthlyColumns(curCol)%avgSum .EQ. isSum) THEN
            curUnits = TRIM(curUnits) // '/s'
          END IF
          IF (sameString(curUnits,'J/s')) THEN
            curUnits = 'W'
          END IF
          !CR7783 fix
          IF (sameString(curUnits,'kWh/s')) THEN
            curUnits = 'W'
            curConversionFactor = curConversionFactor * 3600000.0d0
          END IF
          IF (sameString(curUnits,'GJ/s')) THEN
            curUnits = 'kW'
            curConversionFactor = curConversionFactor * 1000000.0d0
          END IF
          IF (sameString(curUnits,'MJ/s')) THEN
            curUnits = 'kW'
            curConversionFactor = curConversionFactor * 1000.0d0
          END IF
          IF (sameString(curUnits,'therm/s')) THEN
            curUnits = 'kBtu/h'
            curConversionFactor = curConversionFactor * 360000.0d0
          END IF
          IF (sameString(curUnits,'kBtu/s')) THEN
            curUnits = 'kBtu/h'
            curConversionFactor = curConversionFactor * 3600.0d0
          END IF
          IF (sameString(curUnits,'ton-hrs/s')) THEN
            curUnits = 'ton'
            curConversionFactor = curConversionFactor * 3600.0d0
          END IF
          columnHead(columnRecount) = TRIM(MonthlyColumns(curCol)%varName) // TRIM(curAggString) // ' [' &
                                       // TRIM(curUnits) //']'
          maxVal = -HUGE(maxVal)
          minVal = HUGE(maxVal)
          DO lMonth = 1, 12
            curVal = MonthlyColumns(curCol)%reslt(lMonth) * curConversionFactor + curConversionOffset
            IF (IsMonthGathered(lMonth)) THEN
              tableBody(lMonth,columnRecount) = TRIM(RealToStr(curVal,digitsShown))
              IF (curVal .GT. maxVal) maxVal = curVal
              IF (curVal .LT. minVal) minVal = curVal
            ELSE
              tableBody(lMonth,columnRecount) = '-'
            END IF
          END DO !lMonth
          ! add the summary to bottom
          IF (minVal .NE. HUGE(maxVal)) THEN
            tableBody(15,columnRecount) = TRIM(RealToStr(minVal,digitsShown))
          END IF
          IF (maxVal .NE. -HUGE(maxVal)) THEN
            tableBody(16,columnRecount) = TRIM(RealToStr(maxVal,digitsShown))
          END IF
        CASE (aggTypeMaximum,aggTypeMinimum,aggTypeMaximumDuringHoursShown,aggTypeMinimumDuringHoursShown)
          columnRecount = columnRecount + 2
          ! put in the name of the variable for the column
          IF (MonthlyColumns(curCol)%avgSum .EQ. isSum) THEN ! if it is a summed variable
            curUnits = TRIM(curUnits) // '/s'
          ENDIF
          IF (sameString(curUnits,'J/s')) THEN
            curUnits = 'W'
          END IF
          !CR7783 fix
          IF (sameString(curUnits,'kWh/s')) THEN
            curUnits = 'W'
            curConversionFactor = curConversionFactor * 3600000.0d0
          END IF
          IF (sameString(curUnits,'GJ/s')) THEN
            curUnits = 'kW'
            curConversionFactor = curConversionFactor * 1000000.0d0
          END IF
          IF (sameString(curUnits,'MJ/s')) THEN
            curUnits = 'kW'
            curConversionFactor = curConversionFactor * 1000.0d0
          END IF
          IF (sameString(curUnits,'therm/s')) THEN
            curUnits = 'kBtu/h'
            curConversionFactor = curConversionFactor * 360000.0d0
          END IF
          IF (sameString(curUnits,'kBtu/s')) THEN
            curUnits = 'kBtu/h'
            curConversionFactor = curConversionFactor * 3600.0d0
          END IF
          IF (sameString(curUnits,'ton-hrs/s')) THEN
            curUnits = 'ton'
            curConversionFactor = curConversionFactor * 3600.0d0
          END IF
          columnHead(columnRecount - 1) = TRIM(MonthlyColumns(curCol)%varName) // TRIM(curAggString) //   &
                                              '[' // TRIM(curUnits) // ']'
          columnHead(columnRecount) = TRIM(MonthlyColumns(curCol)%varName) // ' {TIMESTAMP} '
          maxVal = -HUGE(maxVal)
          minVal = HUGE(maxVal)
          DO lMonth = 1, 12
            IF (IsMonthGathered(lMonth)) THEN
              curVal = MonthlyColumns(curCol)%reslt(lMonth)
              !CR7788 the conversion factors were causing an overflow for the InchPound case since the
              !value was very small
              !restructured the following lines to hide showing HUGE and -HUGE values in output table CR8154 Glazer
              IF ((curVal .LT. veryLarge) .AND. (curVal .GT. verySmall)) THEN
                curVal = curVal * curConversionFactor + curConversionOffset
                IF (curVal .GT. maxVal) maxVal = curVal
                IF (curVal .LT. minVal) minVal = curVal
                IF (curVal .LT. veryLarge .AND. curVal .GT. verySmall) THEN
                  tableBody(lMonth,columnRecount - 1) = TRIM(RealToStr(curVal,digitsShown))
                ELSE
                  tableBody(lMonth,columnRecount - 1) = '-'
                END IF
                tableBody(lMonth,columnRecount) = TRIM(DateToString(MonthlyColumns(curCol)%timeStamp(lMonth)))
              ELSE
                tableBody(lMonth,columnRecount - 1) = '-'
                tableBody(lMonth,columnRecount) = '-'
              END IF
            ELSE
              tableBody(lMonth,columnRecount - 1) = '-'
              tableBody(lMonth,columnRecount) = '-'
            END IF
          END DO !lMonth
          ! add the summary to bottom
          ! Don't include if the original min and max values are still present
          IF (minVal .LT. veryLarge) THEN
            tableBody(15,columnRecount - 1) = TRIM(RealToStr(minVal,digitsShown))
          ELSE
            tableBody(15,columnRecount - 1) = '-'
          END IF
          IF (maxVal .GT. verySmall) THEN
            tableBody(16,columnRecount - 1) = TRIM(RealToStr(maxVal,digitsShown))
          ELSE
            tableBody(15,columnRecount - 1) = '-'
          END IF
      END SELECT
    END DO !KColumn
    CALL WriteReportHeaders(MonthlyInput(iInput)%name,MonthlyTables(curTable)%keyValue,isAverage)
    CALL WriteSubtitle("Custom Monthly Report")
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.TRUE.) !transpose monthly XML tables.
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        MonthlyInput(iInput)%name,&
                                        MonthlyTables(curTable)%keyValue,&
                                        'Custom Monthly Report')
    DEALLOCATE (columnHead)
    DEALLOCATE (columnWidth)
    DEALLOCATE (tableBody)
  END DO !jTables
END DO ! iInput
END SUBROUTINE WriteMonthlyTables

SUBROUTINE WriteTimeBinTables
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Set up the time bin tabular report results

          ! METHODOLOGY EMPLOYED:
          !   Creates several arrays that are passed to the writeTable
          !   routine.  All arrays are strings so numbers need to be
          !   converted prior to calling writeTable.
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER    :: iInObj
INTEGER    :: iTable
INTEGER    :: kHour
INTEGER    :: kMonth
INTEGER    :: nCol
!main table
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)   :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                          :: columnWidth
CHARACTER(len=MaxNameLength), DIMENSION(39)               :: rowHead
CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:,:) :: tableBody
!stat table
CHARACTER(len=MaxNameLength), DIMENSION(1)                :: columnHeadStat
INTEGER,DIMENSION(1)                                      :: columnWidthStat
CHARACTER(len=MaxNameLength), DIMENSION(6)                :: rowHeadStat
CHARACTER(len=MaxNameLength), DIMENSION(6,1)              :: tableBodyStat

REAL(r64)     :: curIntervalStart
REAL(r64)     :: curIntervalSize
INTEGER  :: curIntervalCount
INTEGER  :: curResIndex
INTEGER  :: curNumTables
INTEGER  :: numIntervalDigits
INTEGER  :: firstReport
REAL(r64)     :: topValue
INTEGER  :: repIndex
REAL(r64)     :: rowTotal
REAL(r64)     :: colTotal
REAL(r64)     :: aboveTotal
REAL(r64)     :: belowTotal
REAL(r64)     :: tableTotal
!CHARACTER(len=MaxNameLength):: repNameWithUnits ! For time bin reports, varible name with units
CHARACTER(len=MaxNameLength*2+15) :: repNameWithUnitsandscheduleName
REAL(r64)     :: repStDev                            ! standard deviation
REAL(r64)     :: repMean
CHARACTER(len=MaxNameLength) :: curNameWithSIUnits
CHARACTER(len=MaxNameLength) :: curNameAndUnits
INTEGER :: indexUnitConv

rowHead(1)  =  'Interval Start'
rowHead(2)  =  'Interval End'
rowHead(3)  =  'January'
rowHead(4)  =  'February'
rowHead(5)  =  'March'
rowHead(6)  =  'April'
rowHead(7)  =  'May'
rowHead(8)  =  'June'
rowHead(9)  =  'July'
rowHead(10) =  'August'
rowHead(11) =  'September'
rowHead(12) =  'October'
rowHead(13) =  'November'
rowHead(14) =  'December'
rowHead(15) =  '12:01 to  1:00 am'
rowHead(16) =  ' 1:01 to  2:00 am'
rowHead(17) =  ' 2:01 to  3:00 am'
rowHead(18) =  ' 3:01 to  4:00 am'
rowHead(19) =  ' 4:01 to  5:00 am'
rowHead(20) =  ' 5:01 to  6:00 am'
rowHead(21) =  ' 6:01 to  7:00 am'
rowHead(22) =  ' 7:01 to  8:00 am'
rowHead(23) =  ' 8:01 to  9:00 am'
rowHead(24) =  ' 9:01 to 10:00 am'
rowHead(25) =  '10:01 to 11:00 am'
rowHead(26) =  '11:01 to 12:00 pm'
rowHead(27) =  '12:01 to  1:00 pm'
rowHead(28) =  ' 1:01 to  2:00 pm'
rowHead(29) =  ' 2:01 to  3:00 pm'
rowHead(30) =  ' 3:01 to  4:00 pm'
rowHead(31) =  ' 4:01 to  5:00 pm'
rowHead(32) =  ' 5:01 to  6:00 pm'
rowHead(33) =  ' 6:01 to  7:00 pm'
rowHead(34) =  ' 7:01 to  8:00 pm'
rowHead(35) =  ' 8:01 to  9:00 pm'
rowHead(36) =  ' 9:01 to 10:00 pm'
rowHead(37) =  '10:01 to 11:00 pm'
rowHead(38) =  '11:01 to 12:00 am'
rowHead(39) =  'Total'
DO iInObj = 1 , OutputTableBinnedCount
  firstReport = OutputTableBinned(iInObj)%resIndex
  curNameWithSIUnits = TRIM(OutputTableBinned(iInObj)%varOrMeter)//' ['//TRIM(OutputTableBinned(iInObj)%units) //']'
  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
    curIntervalStart = ConvertIP(indexUnitConv,OutputTableBinned(iInObj)%intervalStart)
    curIntervalSize = ConvertIPdelta(indexUnitConv,OutputTableBinned(iInObj)%intervalSize)
  ELSE
    curNameAndUnits = curNameWithSIUnits
    curIntervalStart = OutputTableBinned(iInObj)%intervalStart
    curIntervalSize = OutputTableBinned(iInObj)%intervalSize
  END IF
  curIntervalCount = OutputTableBinned(iInObj)%intervalCount
  curResIndex = OutputTableBinned(iInObj)%resIndex
  curNumTables = OutputTableBinned(iInObj)%numTables
  topValue = curIntervalStart + curIntervalSize * curIntervalCount
  IF (curIntervalSize .LT. 1) THEN
    numIntervalDigits = 4
  ELSEIF (curIntervalSize .GE. 10) THEN
    numIntervalDigits = 0
  ELSE
    numIntervalDigits = 2
  END IF
  ! make arrays two columns wider for below and above bin range
  ALLOCATE(columnHead(curIntervalCount + 3))
  ALLOCATE(columnWidth(curIntervalCount + 3))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(39,curIntervalCount + 3))
  tableBody = ' '
  columnHead = '-'
  tableBody(1,1) = 'less than'
  TableBody(2,1) = RealToStr(curIntervalStart,numIntervalDigits)
  DO nCol = 1, curIntervalCount
    columnHead(nCol + 1) = IntToStr(nCol)
    !beginning of interval
    tableBody(1,nCol + 1) = TRIM(RealToStr(curIntervalStart + (nCol-1)*curIntervalSize ,numIntervalDigits)) // '<='
    !end of interval
    tableBody(2,nCol + 1) = TRIM(RealToStr(curIntervalStart + nCol*curIntervalSize ,numIntervalDigits)) // '>'
  END DO
  TableBody(1, curIntervalCount + 2) = 'equal to or more than'
  TableBody(2, curIntervalCount + 2) = RealToStr(topValue,numIntervalDigits)
  TableBody(1, curIntervalCount + 3) = 'Row'
  TableBody(2, curIntervalCount + 3) = 'Total'
  DO iTable = 1, curNumTables
    repIndex = firstReport + (iTable - 1)
    IF (OutputTableBinned(iInObj)%scheduleIndex == 0) THEN
      repNameWithUnitsandscheduleName = curNameAndUnits
    ELSE
      repNameWithUnitsandscheduleName = TRIM(curNameAndUnits) // &
                               ' [' // TRIM(OutputTableBinned(iInObj)%ScheduleName) // ']'
    ENDIF
    CALL WriteReportHeaders(repNameWithUnitsandscheduleName, BinObjVarID(repIndex)%namesOfObj, OutputTableBinned(iInObj)%avgSum)
    DO kHour = 1, 24
      tableBody(14+kHour,1) = TRIM(RealToStr(BinResultsBelow(repIndex)%hrly(kHour),2))
      tableBody(14+kHour,curIntervalCount+2) = RealToStr(BinResultsAbove(repIndex)%hrly(kHour),2)
      rowTotal = BinResultsBelow(repIndex)%hrly(kHour) + BinResultsAbove(repIndex)%hrly(kHour)
      DO nCol = 1, curIntervalCount
        tableBody(14+kHour,nCol+1) = TRIM(RealToStr(BinResults(repIndex,nCol)%hrly(kHour),2))
        ! sum the total for all columns
        rowTotal = rowTotal + BinResults(repIndex,nCol)%hrly(kHour)
      END DO
      tableBody(14+kHour, nCol+2) = TRIM(RealToStr(rowTotal,2))
    END DO
    tableTotal = 0.0d0
    DO kMonth = 1, 12
      tableBody(2+kMonth,1) = RealToStr(BinResultsBelow(repIndex)%mnth(kMonth),2)
      tableBody(2+kMonth,curIntervalCount+2) = RealToStr(BinResultsAbove(repIndex)%mnth(kMonth),2)
      rowTotal = BinResultsBelow(repIndex)%mnth(kMonth) + BinResultsAbove(repIndex)%mnth(kMonth)
      DO nCol = 1, curIntervalCount
        tableBody(2+kMonth,nCol+1) = TRIM(RealToStr(BinResults(repIndex,nCol)%mnth(kMonth),2))
        ! sum the total for all columns
        rowTotal = rowTotal + BinResults(repIndex,nCol)%mnth(kMonth)
      END DO
      tableBody(2+kMonth, nCol+2) = TRIM(RealToStr(rowTotal,2))
      tableTotal = tableTotal + rowTotal
    END DO
    ! compute total row
    DO nCol = 1, curIntervalCount
      colTotal=0.0d0
      DO kMonth = 1, 12
        colTotal = colTotal + BinResults(repIndex,nCol)%mnth(kMonth)
      END DO
      tableBody(39,nCol+1) = TRIM(RealToStr(colTotal,2))
    END DO
    aboveTotal = 0.0d0
    belowTotal = 0.0d0
    DO kMonth = 1, 12
      aboveTotal = aboveTotal + BinResultsAbove(repIndex)%mnth(kMonth)
      belowTotal = belowTotal + BinResultsBelow(repIndex)%mnth(kMonth)
    END DO
    tableBody(39,1) = TRIM(RealToStr(belowTotal,2))
    tableBody(39,curIntervalCount+2) = TRIM(RealToStr(aboveTotal,2))
    tableBody(39,curIntervalCount+3) = TRIM(RealToStr(tableTotal,2))
    CALL writeTextLine('Values in table are in hours.')
    CALL writeTextLine(' ')
    CALL WriteSubtitle("Time Bin Results")
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.TRUE.) !transpose XML tables
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        repNameWithUnitsandscheduleName,&
                                        BinObjVarID(repIndex)%namesOfObj,&
                                        'Time Bin Results')
    !create statistics table
    rowHeadStat(1) = 'Minimum'
    rowHeadStat(2) = 'Mean minus two standard deviations'
    rowHeadStat(3) = 'Mean'
    rowHeadStat(4) = 'Mean plus two standard deviations'
    rowHeadStat(5) = 'Maximum'
    rowHeadStat(6) = 'Standard deviation'
    columnHeadStat(1) = 'Statistic'
    columnWidthStat(1) = 14
    !per Applied Regression Analysis and Other Multivariate Methods, Kleinburger/Kupper, 1978
    !first check if very large constant number has caused the second part to be larger than the first
    IF (BinStatistics(repIndex)%n .GT. 1) THEN
      IF (BinStatistics(repIndex)%sum2 .GT. ((BinStatistics(repIndex)%sum ** 2)/BinStatistics(repIndex)%n)) THEN
        repStDev = SQRT((BinStatistics(repIndex)%sum2 - ((BinStatistics(repIndex)%sum ** 2)/BinStatistics(repIndex)%n)) &
                   / (BinStatistics(repIndex)%n - 1))
      ELSE
        repStDev = 0.0d0
      END IF
      repMean = BinStatistics(repIndex)%sum / BinStatistics(repIndex)%n
    ELSE
      repStDev = 0.0d0
      repMean = 0.0d0
    END IF
    IF (unitsStyle .EQ. unitsStyleInchPound) THEN
      tableBodyStat(1,1) = RealToStr(ConvertIP(indexUnitConv,BinStatistics(repIndex)%minimum),2)
      tableBodyStat(2,1) = RealToStr(ConvertIP(indexUnitConv,repMean -  2 * repStDev),2)
      tableBodyStat(3,1) = RealToStr(ConvertIP(indexUnitConv,repMean),2)
      tableBodyStat(4,1) = RealToStr(ConvertIP(indexUnitConv,repMean +  2 * repStDev),2)
      tableBodyStat(5,1) = RealToStr(ConvertIP(indexUnitConv,BinStatistics(repIndex)%Maximum),2)
      tableBodyStat(6,1) = RealToStr(ConvertIPdelta(indexUnitConv,repStDev),2)
    ELSE
      tableBodyStat(1,1) = RealToStr(BinStatistics(repIndex)%minimum,2)
      tableBodyStat(2,1) = RealToStr(repMean -  2 * repStDev,2)
      tableBodyStat(3,1) = RealToStr(repMean,2)
      tableBodyStat(4,1) = RealToStr(repMean +  2 * repStDev,2)
      tableBodyStat(5,1) = RealToStr(BinStatistics(repIndex)%Maximum,2)
      tableBodyStat(6,1) = RealToStr(repStDev,2)
    END IF
    CALL writeSubtitle('Statistics')
    CALL writeTable(tableBodyStat,rowHeadStat,columnHeadStat,columnWidthStat,.TRUE.) !transpose XML table
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        repNameWithUnitsandscheduleName,&
                                        BinObjVarID(repIndex)%namesOfObj,&
                                        'Statistics')
  END DO
  DEALLOCATE(columnHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
END DO
END SUBROUTINE WriteTimeBinTables

SUBROUTINE WriteBEPSTable
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   November 2003
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Take the gathered total and enduse meter data and structure
          !   the results into a tabular report for output.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   This report actually consists of many sub-tables each with
          !   its own call to writeTable.  Anytime that column headings are
          !   desired they are done in a new table because the only place
          !   that will split up very long header lines for the fixed width
          !   table is the header rows.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: MaxNumSubcategories, EndUseCategory
USE DataWater,       ONlY: WaterStorage
USE ManageElectricPower , ONLY: ElecStorage, NumElecStorageDevices
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords
USE DataHVACGlobals, ONLY: deviationFromSetPtThresholdHtg,deviationFromSetPtThresholdClg
USE ScheduleManager, ONLY: GetScheduleName

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, parameter  ::  enduseLine            = 1
INTEGER, parameter  ::  detailLine            = 16
INTEGER, parameter  ::  normalizedLine        = 23
INTEGER, parameter  ::  elecSatisLine         = 36
INTEGER, parameter  ::  thermSatisLine        = 50
INTEGER, parameter  ::  waterSatisLine        = 59
INTEGER, parameter  ::  sourceSiteLine        = 69
INTEGER, parameter  ::  areaLine              = 73
INTEGER, parameter  ::  controlLine           = 77
INTEGER, parameter  ::  notesLine             = 83

INTEGER, parameter  ::  colElectricity        = 1
INTEGER, parameter  ::  colGas                = 2
INTEGER, parameter  ::  colAdditionalFuel     = 3
INTEGER, parameter  ::  colPurchCool          = 4
INTEGER, parameter  ::  colPurchHeat          = 5
INTEGER, parameter  ::  colWater              = 6

REAL(r64), parameter :: SmallValue = 1.d-14

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody

! all arrays are in the format: (row, columnm)
REAL(r64),DIMENSION(15,6)                           :: useVal
REAL(r64),DIMENSION(4,6)                            :: normalVal
REAL(r64),DIMENSION(6)                              :: collapsedTotal
REAL(r64),DIMENSION(numEndUses,6)                   :: collapsedEndUse
REAL(r64),DIMENSION(6,numEndUses,MaxNumSubcategories) :: collapsedEndUseSub
REAL(r64),DIMENSION(numEndUses,6)                     :: endUseSubOther
LOGICAL,DIMENSION(numEndUses)                         :: needOtherRow
REAL(r64)                                      :: totalOnsiteHeat
REAL(r64)                                      :: totalOnsiteWater
REAL(r64)                                      :: totalWater
REAL(r64)                                      :: netElecPurchasedSold
REAL(r64)                                      :: totalSiteEnergyUse
REAL(r64)                                      :: netSiteEnergyUse
REAL(r64)                                      :: totalSourceEnergyUse
REAL(r64)                                      :: netSourceEnergyUse
REAL(r64)                                      :: netSourceElecPurchasedSold
INTEGER                                        :: iResource
INTEGER                                        :: jEndUse
INTEGER                                        :: kEndUseSub
INTEGER                                        :: i
REAL(r64)                                      :: largeConversionFactor
REAL(r64)                                      :: kConversionFactor
INTEGER                                        :: numRows
REAL(r64)                                      :: initialStorage
REAL(r64)                                      :: finalStorage
REAL(r64)                                      :: StorageChange
INTEGER                                        :: resourcePrimaryHeating
REAL(r64)                                      :: heatingMaximum
CHARACTER(len=100)                             :: footnote
REAL(r64)                                      :: waterConversionFactor
REAL(r64)                                      :: areaConversionFactor
REAL(r64)                                      :: convBldgGrossFloorArea
REAL(r64)                                      :: convBldgCondFloorArea
CHARACTER(len=MaxNameLength) :: curNameWithSIUnits
CHARACTER(len=MaxNameLength) :: curNameAndUnits
INTEGER :: indexUnitConv
CHARACTER(len=52)                              :: tableString
REAL(r64)                                      :: processFraction
REAL(r64)                                      :: processElecCost
REAL(r64)                                      :: processGasCost
REAL(r64)                                      :: processOthrCost

REAL(r64),DIMENSION(6)                         :: leedFansParkFromFan
REAL(r64),DIMENSION(6)                         :: leedFansParkFromExtFuelEquip
REAL(r64),DIMENSION(6)                         :: leedIntLightProc
REAL(r64),DIMENSION(6)                         :: leedCook
REAL(r64),DIMENSION(6)                         :: leedIndProc
REAL(r64),DIMENSION(6)                         :: leedElevEsc
CHARACTER(len=MaxNameLength)                   :: subCatName
REAL(r64)                                      :: nonMisc
REAL(r64)                                      :: leedSiteIntLite = 0.0d0
REAL(r64)                                      :: leedSiteSpHeat = 0.0d0
REAL(r64)                                      :: leedSiteSpCool = 0.0d0
REAL(r64)                                      :: leedSiteFanInt = 0.0d0
REAL(r64)                                      :: leedSiteSrvWatr = 0.0d0
REAL(r64)                                      :: leedSiteRecept = 0.0d0
REAL(r64)                                      :: leedSiteMisc = 0.0d0
REAL(r64)                                      :: leedSiteTotal = 0.0d0
REAL(r64)                                      :: unconvert

IF (displayTabularBEPS .or. displayLEEDSummary) THEN
  ! show the headers of the report
  IF (displayTabularBEPS) THEN
    CALL WriteReportHeaders('Annual Building Utility Performance Summary','Entire Facility',isAverage)
    ! show the number of hours that the table applies to
    CALL writeTextLine('Values gathered over ' // RealToStr(gatherElapsedTimeBEPS,2) // ' hours',.TRUE.)
    IF (gatherElapsedTimeBEPS .LT. 8759.0d0) THEN  ! might not add up to 8760 exactly but can't be more than 1 hour diff.
      CALL writeTextLine('WARNING: THE REPORT DOES NOT REPRESENT A FULL ANNUAL SIMULATION.',.TRUE.)
    END IF
    CALL writeTextLine('',.TRUE.)
  ENDIF
  ! determine building floor areas
  CALL DetermineBuildingFloorArea
  ! collapse the gatherEndUseBEPS array to the resource groups displayed
  DO jEndUse=1,numEndUses
    collapsedEndUse(jEndUse,1) = gatherEndUseBEPS(jEndUse,1)    !electricity
    collapsedEndUse(jEndUse,2) = gatherEndUseBEPS(jEndUse,2)    !natural gas
    collapsedEndUse(jEndUse,3) = gatherEndUseBEPS(jEndUse,6)  & !additional fuel  <- gasoline
                               + gatherEndUseBEPS(jEndUse,8)  & !                 <- diesel
                               + gatherEndUseBEPS(jEndUse,9)  & !                 <- coal
                               + gatherEndUseBEPS(jEndUse,10) & !                 <- fuel oil #1
                               + gatherEndUseBEPS(jEndUse,11) & !                 <- fuel oil #2
                               + gatherEndUseBEPS(jEndUse,12) & !                 <- propane
                               + gatherEndUseBEPS(jEndUse,13) & !                 <- otherfuel1
                               + gatherEndUseBEPS(jEndUse,14)   !                 <- otherfuel2
    collapsedEndUse(jEndUse,4) = gatherEndUseBEPS(jEndUse,3)    !district cooling <- purchased cooling
    collapsedEndUse(jEndUse,5) = gatherEndUseBEPS(jEndUse,4)  & !district heating <- purchased heating
                               + gatherEndUseBEPS(jEndUse,5)    !                 <- steam
    collapsedEndUse(jEndUse,6) = gatherEndUseBEPS(jEndUse,7)    !water
  END DO
  ! repeat with totals
  collapsedTotal(1) = gatherTotalsBEPS(1)    !electricity
  collapsedTotal(2) = gatherTotalsBEPS(2)    !natural gas
  collapsedTotal(3) = gatherTotalsBEPS(6)  & !additional fuel  <- gasoline
                    + gatherTotalsBEPS(8)  & !                 <- diesel
                    + gatherTotalsBEPS(9)  & !                 <- coal
                    + gatherTotalsBEPS(10) & !                 <- fuel oil #1
                    + gatherTotalsBEPS(11) & !                 <- fuel oil #2
                    + gatherTotalsBEPS(12) & !                 <- propane
                    + gatherTotalsBEPS(13) & !                 <- otherfuel1
                    + gatherTotalsBEPS(14)   !                 <- otherfuel2
  collapsedTotal(4) = gatherTotalsBEPS(3)    !district cooling <- purchased cooling
  collapsedTotal(5) = gatherTotalsBEPS(4)  & !district heating <- purchased heating
                    + gatherTotalsBEPS(5)    !                 <- steam
  collapsedTotal(6) = gatherTotalsBEPS(7)    !water

  DO jEndUse=1,numEndUses
    DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
      collapsedEndUseSub(1,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(1,jEndUse,kEndUseSub)    !electricity
      collapsedEndUseSub(2,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(2,jEndUse,kEndUseSub)    !natural gas
      collapsedEndUseSub(3,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(6,jEndUse,kEndUseSub) &  !additional fuel  <- gasoline
                                               + gatherEndUseSubBEPS(8,jEndUse,kEndUseSub) &  !                 <- diesel
                                               + gatherEndUseSubBEPS(9,jEndUse,kEndUseSub) &  !                 <- coal
                                               + gatherEndUseSubBEPS(10,jEndUse,kEndUseSub) & !                 <- fuel oil #1
                                               + gatherEndUseSubBEPS(11,jEndUse,kEndUseSub) & !                 <- fuel oil #2
                                               + gatherEndUseSubBEPS(12,jEndUse,kEndUseSub) & !                 <- propane
                                               + gatherEndUseSubBEPS(13,jEndUse,kEndUseSub) & !                 <- otherfuel1
                                               + gatherEndUseSubBEPS(14,jEndUse,kEndUseSub)   !                 <- otherfuel2
      collapsedEndUseSub(4,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(3,jEndUse,kEndUseSub)    !district cooling <- purch cooling
      collapsedEndUseSub(5,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(4,jEndUse,kEndUseSub) &  !district heating <- purch heating
                                               + gatherEndUseSubBEPS(5,jEndUse,kEndUseSub)    !                 <- steam
      collapsedEndUseSub(6,jEndUse,kEndUseSub) = gatherEndUseSubBEPS(7,jEndUse,kEndUseSub)    !water
    END DO
  END DO

  ! unit conversion - all values are used as divisors
  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      largeConversionFactor = 3600000.d0
      kConversionFactor = 1.0d0
      waterConversionFactor = 1.0d0
      areaConversionFactor = 1.0d0
    CASE (unitsStyleInchPound)
      largeConversionFactor =  getSpecificUnitDivider('J','kBtu')    !1054351.84 J to kBtu
      kConversionFactor = 1.0d0
      waterConversionFactor = getSpecificUnitDivider('m3','gal')     !0.003785413 m3 to gal
      areaConversionFactor = getSpecificUnitDivider('m2','ft2')      !0.092893973 m2 to ft2
    CASE DEFAULT
      largeConversionFactor = 1000000000.d0
      kConversionFactor =  1000.0d0
      waterConversionFactor = 1.0d0
      areaConversionFactor = 1.0d0
  END SELECT

  ! convert floor areas
  convBldgGrossFloorArea = buildingGrossFloorArea / areaConversionFactor
  convBldgCondFloorArea = buildingConditionedFloorArea / areaConversionFactor

  !convert units into GJ (divide by 1,000,000,000) if J otherwise kWh
  DO iResource= 1,5 !don't do water
    DO jEndUse=1,numEndUses
      collapsedEndUse(jEndUse,iResource) = collapsedEndUse(jEndUse,iResource) / largeConversionFactor
        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          collapsedEndUseSub(iResource,jEndUse,kEndUseSub) = collapsedEndUseSub(iResource,jEndUse,kEndUseSub) &
            / largeConversionFactor
        END DO
    END DO
    collapsedTotal(iResource) = collapsedTotal(iResource) / largeConversionFactor
  END DO
  !do water
  DO jEndUse=1,numEndUses
    collapsedEndUse(jEndUse,6) = collapsedEndUse(jEndUse,6) / waterConversionFactor
      DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
        collapsedEndUseSub(6,jEndUse,kEndUseSub) = collapsedEndUseSub(6,jEndUse,kEndUseSub) &
          / waterConversionFactor
      END DO
  END DO

  ! convert to GJ
  gatherPowerFuelFireGen =  gatherPowerFuelFireGen  / largeConversionFactor
  gatherPowerPV =           gatherPowerPV           / largeConversionFactor
  gatherPowerWind =         gatherPowerWind         / largeConversionFactor
  gatherPowerHTGeothermal = gatherPowerHTGeothermal / largeConversionFactor
  gatherElecProduced =      gatherElecProduced      / largeConversionFactor
  gatherElecPurchased =     gatherElecPurchased     / largeConversionFactor
  gatherElecSurplusSold =   gatherElecSurplusSold   / largeConversionFactor

  ! get change in overall state of charge for electrical storage devices.
  IF (NumElecStorageDevices >0) THEN
    OverallNetEnergyFromStorage = (Sum(ElecStorage%StartingEnergyStored) - Sum(ElecStorage%ThisTimeStepStateOfCharge))
    OverallNetEnergyFromStorage = OverallNetEnergyFromStorage  / largeConversionFactor
  ELSE
    OverallNetEnergyFromStorage = 0.0D0
  ENDIF
  ! determine which resource is the primary heating resourse
  resourcePrimaryHeating = 0
  heatingMaximum = 0.0d0
  DO iResource = 1, 5 !don't do water
    IF (collapsedEndUse(endUseHeating, iResource) .GT. heatingMaximum) THEN
      heatingMaximum = collapsedEndUse(endUseHeating, iResource)
      resourcePrimaryHeating = iResource
    END IF
  END DO


  !
  !---- Source and Site Energy Sub-Table
  !
  ALLOCATE(rowHead(4))
  ALLOCATE(columnHead(3))
  ALLOCATE(columnWidth(3))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(4,3))

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Total Energy [kWh]'
      columnHead(2) = 'Energy Per Total Building Area [kWh/m2]'
      columnHead(3) = 'Energy Per Conditioned Building Area [kWh/m2]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Total Energy [kBtu]'
      columnHead(2) = 'Energy Per Total Building Area [kBtu/ft2]'
      columnHead(3) = 'Energy Per Conditioned Building Area [kBtu/ft2]'
    CASE DEFAULT
      columnHead(1) = 'Total Energy [GJ]'
      columnHead(2) = 'Energy Per Total Building Area [MJ/m2]'
      columnHead(3) = 'Energy Per Conditioned Building Area [MJ/m2]'
  END SELECT

  rowHead(1)  = 'Total Site Energy'
  rowHead(2)  = 'Net Site Energy'
  rowHead(3)  = 'Total Source Energy'
  rowHead(4)  = 'Net Source Energy'

  tableBody = ''

  ! compute the net amount of electricity received from the utility which
  ! is the amount purchased less the amount sold to the utility. Note that
  ! previously these variables were converted into GJ so now we don't need
  ! to do any conversion
    totalSiteEnergyUse = (gatherTotalsBEPS(1)  & !electricity
                        + gatherTotalsBEPS(2)  & !natural gas
                        + gatherTotalsBEPS(3)  & !district cooling
                        + gatherTotalsBEPS(4)  & !district heating
                        + gatherTotalsBEPS(5)  & !steam
                        + gatherTotalsBEPS(6)  & !gasoline
  ! water is not included   gatherTotalsBEPS(7)    !water
                        + gatherTotalsBEPS(8)  & !diesel
                        + gatherTotalsBEPS(9)  & !coal
                        + gatherTotalsBEPS(10) & !fuel oil #1
                        + gatherTotalsBEPS(11) & !fuel oil #2
                        + gatherTotalsBEPS(12) & !propane
                        + gatherTotalsBEPS(13) & !otherfuel1
                        + gatherTotalsBEPS(14) & !otherfuel2
                                              ) / largeConversionFactor

  netElecPurchasedSold = gatherElecPurchased  - gatherElecSurplusSold

  netSiteEnergyUse = netElecPurchasedSold & !electricity (already in GJ)
              + ( gatherTotalsBEPS(2)  & !natural gas
                + gatherTotalsBEPS(3)  & !district cooling
                + gatherTotalsBEPS(4)  & !district heating
                + gatherTotalsBEPS(5)  & !steam
                + gatherTotalsBEPS(6)  & !gasoline
  ! water is not included   gatherTotalsBEPS(7)    !water
                + gatherTotalsBEPS(8)  & !diesel
                + gatherTotalsBEPS(9)  & !coal
                + gatherTotalsBEPS(10) & !fuel oil #1
                + gatherTotalsBEPS(11) & !fuel oil #2
                + gatherTotalsBEPS(12) & !propane
                + gatherTotalsBEPS(13) & !otherfuel1
                + gatherTotalsBEPS(14) & !otherfuel2
                                      ) / largeConversionFactor

  IF (efficiencyDistrictCooling .EQ. 0)  efficiencyDistrictCooling = 1.0d0
  IF (efficiencyDistrictHeating .EQ. 0)  efficiencyDistrictHeating = 1.0d0


  ! source emissions already have the source factors included in the calcs.
  TotalSourceEnergyUse=0.0d0
  !  electricity
  if (fuelfactorsused(1)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(1)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(1)*sourceFactorElectric
  endif
  !  natural gas
  if (fuelfactorsused(2)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(2)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(2)*sourceFactorNaturalGas
  endif
  ! gasoline
  if (fuelfactorsused(3)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(3)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(6)*sourceFactorGasoline
  endif
  ! diesel
  if (fuelfactorsused(4)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(4)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(8)*sourceFactorDiesel
  endif
  ! coal
  if (fuelfactorsused(5)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(5)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(9)*sourceFactorCoal
  endif
  ! fuel oil #1
  if (fuelfactorsused(6)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(6)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(10)*sourceFactorFuelOil1
  endif
  ! fuel oil #2
  if (fuelfactorsused(7)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(7)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(11)*sourceFactorFuelOil2
  endif
  ! propane
  if (fuelfactorsused(8)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(8)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(12)*sourceFactorPropane
  endif
  !otherfuel1
  if (fuelfactorsused(11)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(11)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(13)*sourceFactorOtherFuel1
  endif
  !otherfuel2
  if (fuelfactorsused(12)) then
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsSource(12)
  else
    TotalSourceEnergyUse=TotalSourceEnergyUse+gatherTotalsBEPS(14)*sourceFactorOtherFuel2
  endif


  TotalSourceEnergyUse = (TotalSourceEnergyuse   &
                  + gatherTotalsBEPS(3)*sourceFactorElectric/efficiencyDistrictCooling  & !district cooling
                  + gatherTotalsBEPS(4)*sourceFactorNaturalGas/efficiencyDistrictHeating  & !district heating
                  + gatherTotalsBEPS(5)*sourceFactorSteam  & !steam
                                          ) / largeConversionFactor


  ! now determine "net" source from purchased and surplus sold (still in J)

  if (fuelfactorsused(1)) then
    netSourceElecPurchasedSold = gatherTotalsSource(9) - gatherTotalsSource(10)
  else
    netSourceElecPurchasedSold = netElecPurchasedSold*sourceFactorElectric*largeConversionFactor  ! back to J
  endif

  netSourceEnergyUse=0.0d0
  !  natural gas
  if (fuelfactorsused(2)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(2)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(2)*sourceFactorNaturalGas
  endif
  ! gasoline
  if (fuelfactorsused(3)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(3)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(6)*sourceFactorGasoline
  endif
  ! diesel
  if (fuelfactorsused(4)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(4)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(8)*sourceFactorDiesel
  endif
  ! coal
  if (fuelfactorsused(5)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(5)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(9)*sourceFactorCoal
  endif
  ! fuel oil #1
  if (fuelfactorsused(6)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(6)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(10)*sourceFactorFuelOil1
  endif
  ! fuel oil #2
  if (fuelfactorsused(7)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(7)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(11)*sourceFactorFuelOil2
  endif
  ! propane
  if (fuelfactorsused(8)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(8)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(12)*sourceFactorPropane
  endif
  ! otherfuel1
  if (fuelfactorsused(11)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(11)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(13)*sourceFactorOtherFuel1
  endif
  ! otherfuel2
  if (fuelfactorsused(12)) then
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsSource(12)
  else
    netSourceEnergyUse=netSourceEnergyUse+gatherTotalsBEPS(14)*sourceFactorOtherFuel2
  endif

  netSourceEnergyUse = (netSourceEnergyUse   &          ! from other fuels
                  + netSourceElecPurchasedSold        & !net source from electricity
                  + gatherTotalsBEPS(3)*sourceFactorElectric/efficiencyDistrictCooling  & !district cooling
                  + gatherTotalsBEPS(4)*sourceFactorNaturalGas/efficiencyDistrictHeating  & !district heating
                  + gatherTotalsBEPS(5)*sourceFactorSteam  & !steam
                                         ) / largeConversionFactor


  ! show annual values
  tableBody(1,1)  = TRIM(RealToStr(totalSiteEnergyUse,2))
  tableBody(2,1)  = TRIM(RealToStr(netSiteEnergyUse, 2))
  tableBody(3,1)  = TRIM(RealToStr(totalSourceEnergyUse,2))
  tableBody(4,1)  = TRIM(RealToStr(netSourceEnergyUse,2))
  ! show  per building area
  IF (convBldgGrossFloorArea .GT. 0) THEN
    tableBody(1,2)  = TRIM(RealToStr(totalSiteEnergyUse * kConversionFactor &
                           / convBldgGrossFloorArea,2))
    tableBody(2,2)  = TRIM(RealToStr(netSiteEnergyUse * kConversionFactor &
                           / convBldgGrossFloorArea,2))
    tableBody(3,2)  = TRIM(RealToStr(totalSourceEnergyUse * kConversionFactor &
                           / convBldgGrossFloorArea, 2))
    tableBody(4,2)  = TRIM(RealToStr(netSourceEnergyUse * kConversionFactor &
                           / convBldgGrossFloorArea, 2))
   END IF
  ! show  per conditioned building area
  IF (convBldgCondFloorArea .GT. 0) THEN
    tableBody(1,3)  = TRIM(RealToStr(TotalSiteEnergyUse * kConversionFactor &
                           / convBldgCondFloorArea,2))
    tableBody(2,3)  = TRIM(RealToStr(NetSiteEnergyUse * kConversionFactor &
                           / convBldgCondFloorArea,2))
    tableBody(3,3)  = TRIM(RealToStr(TotalSourceEnergyUse * kConversionFactor &
                           / convBldgCondFloorArea, 2))
    tableBody(4,3)  = TRIM(RealToStr(NetSourceEnergyUse * kConversionFactor &
                           / convBldgCondFloorArea, 2))
  END IF

  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Site and Source Energy')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Site and Source Energy')
  ENDIF

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- Source and Site Energy Sub-Table
  !
  ALLOCATE(rowHead(13))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  columnWidth = 50 !array assignment
  ALLOCATE(tableBody(13,1))

  columnHead(1) = 'Site=>Source Conversion Factor'

  rowHead(1)  = 'Electricity'
  rowHead(2)  = 'Natural Gas'
  rowHead(3)  = 'District Cooling'
  rowHead(4)  = 'District Heating'
  rowHead(5)  = 'Steam'
  rowHead(6)  = 'Gasoline'
  rowHead(7)  = 'Diesel'
  rowHead(8)  = 'Coal'
  rowHead(9)  = 'Fuel Oil #1'
  rowHead(10)  = 'Fuel Oil #2'
  rowHead(11)  = 'Propane'
  rowHead(12)  = 'Other Fuel 1'
  rowHead(13)  = 'Other Fuel 2'

  tableBody = ''

  ! set columns to conversion factors
  ! show values
!  tableBody(1,1)  = TRIM(RealToStr(sourceFactorElectric,3))
!  tableBody(2,1)  = TRIM(RealToStr(sourceFactorNaturalGas, 3))
!  tableBody(3,1)  = TRIM(RealToStr(sourceFactorElectric/ efficiencyDistrictCooling,3))
!  tableBody(4,1)  = TRIM(RealToStr(sourceFactorNaturalGas/ efficiencyDistrictHeating ,3))
!  tableBody(5,1)  = TRIM(RealToStr(sourceFactorSteam ,3))
!  tableBody(6,1)  = TRIM(RealToStr(sourceFactorGasoline ,3))
!  tableBody(7,1)  = TRIM(RealToStr(sourceFactorDiesel ,3))
!  tableBody(8,1)  = TRIM(RealToStr(sourceFactorCoal ,3))
!  tableBody(9,1)  = TRIM(RealToStr(sourceFactorFuelOil1 ,3))
!  tableBody(10,1) = TRIM(RealToStr(sourceFactorFuelOil2 ,3))
!  tableBody(11,1) = TRIM(RealToStr(sourceFactorPropane ,3))

  IF (.not. ffSchedUsed(1)) THEN
    tableBody(1,1)  = TRIM(RealToStr(sourceFactorElectric,3))
  ELSEIF (gatherTotalsBEPS(1) > SmallValue) THEN
    tableBody(1,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(1)/gatherTotalsBEPS(1),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(1))) // '")'
  ELSE
    tableBody(1,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(2)) THEN
    tableBody(2,1)  = TRIM(RealToStr(sourceFactorNaturalGas, 3))
  ELSEIF (gatherTotalsBEPS(2) > SmallValue) THEN
    tableBody(2,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(2)/gatherTotalsBEPS(2),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(2))) // '")'
  ELSE
    tableBody(2,1)  = 'N/A'
  END IF

  tableBody(3,1)  = TRIM(RealToStr(sourceFactorElectric/ efficiencyDistrictCooling,3)) ! District Cooling

  tableBody(4,1)  = TRIM(RealToStr(sourceFactorNaturalGas/ efficiencyDistrictHeating ,3)) ! Disctrict Heating

  tableBody(5,1)  = TRIM(RealToStr(sourceFactorSteam ,3)) ! Steam

  IF (.not. ffSchedUsed(6)) THEN
    tableBody(6,1)  = TRIM(RealToStr(sourceFactorGasoline ,3))
  ELSEIF (gatherTotalsBEPS(6) > SmallValue) THEN
    tableBody(6,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(6)/gatherTotalsBEPS(6),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(6))) // '")'
  ELSE
    tableBody(6,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(8)) THEN
    tableBody(7,1)  = TRIM(RealToStr(sourceFactorDiesel ,3))
  ELSEIF (gatherTotalsBEPS(8) > SmallValue) THEN
    tableBody(7,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(8)/gatherTotalsBEPS(8),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(8))) // '")'
  ELSE
    tableBody(7,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(9)) THEN
    tableBody(8,1)  = TRIM(RealToStr(sourceFactorCoal ,3))
  ELSEIF (gatherTotalsBEPS(9) > SmallValue) THEN
    tableBody(8,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(9)/gatherTotalsBEPS(9),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(9))) //'")'
  ELSE
    tableBody(8,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(10)) THEN
    tableBody(9,1)  = TRIM(RealToStr(sourceFactorFuelOil1 ,3))
  ELSEIF (gatherTotalsBEPS(10) > SmallValue) THEN
    tableBody(9,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(10)/gatherTotalsBEPS(10),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(10))) //'")'
  ELSE
    tableBody(9,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(11)) THEN
    tableBody(10,1)  = TRIM(RealToStr(sourceFactorFuelOil2 ,3))
  ELSEIF (gatherTotalsBEPS(11) > SmallValue) THEN
    tableBody(10,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(11)/gatherTotalsBEPS(11),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(11))) //'")'
  ELSE
    tableBody(10,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(12)) THEN
    tableBody(11,1)  = TRIM(RealToStr(sourceFactorPropane ,3))
  ELSEIF (gatherTotalsBEPS(12) > SmallValue) THEN
    tableBody(11,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(12)/gatherTotalsBEPS(12),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(12))) //'")'
  ELSE
    tableBody(11,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(13)) THEN
    tableBody(12,1)  = TRIM(RealToStr(sourceFactorOtherFuel1 ,3))
  ELSEIF (gatherTotalsBEPS(13) > SmallValue) THEN
    tableBody(12,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(13)/gatherTotalsBEPS(13),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(13))) //'")'
  ELSE
    tableBody(12,1)  = 'N/A'
  END IF

  IF (.not. ffSchedUsed(14)) THEN
    tableBody(13,1)  = TRIM(RealToStr(sourceFactorOtherFuel2 ,3))
  ELSEIF (gatherTotalsBEPS(14) > SmallValue) THEN
    tableBody(13,1)  = 'Effective Factor = ' // TRIM(RealToStr(gatherTotalsBySourceBEPS(14)/gatherTotalsBEPS(14),3)) // &
                         ' (calculated using schedule "' // TRIM(GetScheduleName(ffSchedIndex(14))) //'")'
  ELSE
    tableBody(13,1)  = 'N/A'
  END IF

  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Site to Source Energy Conversion Factors')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Site to Source Energy Conversion Factors')
  ENDIF

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- Building Area Sub-Table
  !
  ALLOCATE(rowHead(3))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(3,1))

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Area [m2]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Area [ft2]'
    CASE DEFAULT
      columnHead(1) = 'Area [m2]'
  END SELECT

  rowHead(1)  = 'Total Building Area'
  rowHead(2)  = 'Net Conditioned Building Area'
  rowHead(3)  = 'Unconditioned Building Area'

  tableBody = ''
  tableBody(1,1)  = TRIM(RealToStr(convBldgGrossFloorArea,2))
  CALL PreDefTableEntry(pdchLeedGenData,'Total gross floor area [m2]',TRIM(RealToStr(convBldgGrossFloorArea,2)))
  tableBody(2,1)  = TRIM(RealToStr(convBldgCondFloorArea,2))
  tableBody(3,1)  = TRIM(RealToStr(convBldgGrossFloorArea - convBldgCondFloorArea,2))

  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Building Area')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Building Area')
  ENDIF

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- End Use Sub-Table
  !
  ALLOCATE(rowHead(16))
  ALLOCATE(columnHead(6))
  ALLOCATE(columnWidth(6))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(16,6))
  DO iResource= 1,6
    useVal(1,iResource)  = collapsedEndUse(endUseHeating,iResource)
    useVal(2,iResource)  = collapsedEndUse(endUseCooling,iResource)
    useVal(3,iResource)  = collapsedEndUse(endUseInteriorLights,iResource)
    useVal(4,iResource)  = collapsedEndUse(endUseExteriorLights,iResource)
    useVal(5,iResource)  = collapsedEndUse(endUseInteriorEquipment,iResource)
    useVal(6,iResource)  = collapsedEndUse(endUseExteriorEquipment,iResource)
    useVal(7,iResource)  = collapsedEndUse(endUseFans,iResource)
    useVal(8,iResource)  = collapsedEndUse(endUsePumps,iResource)
    useVal(9,iResource)  = collapsedEndUse(endUseHeatRejection,iResource)
    useVal(10,iResource) = collapsedEndUse(endUseHumidification,iResource)
    useVal(11,iResource) = collapsedEndUse(endUseHeatRecovery,iResource)
    useVal(12,iResource) = collapsedEndUse(endUseWaterSystem,iResource)
    useVal(13,iResource) = collapsedEndUse(endUseRefrigeration,iResource)
    useVal(14,iResource) = collapsedEndUse(endUseCogeneration,iResource)

    useVal(15,iResource) = collapsedTotal(iResource)  ! totals
  END DO

  rowHead(1)  = 'Heating'
  rowHead(2)  = 'Cooling'
  rowHead(3)  = 'Interior Lighting'
  rowHead(4)  = 'Exterior Lighting'
  rowHead(5)  = 'Interior Equipment'
  rowHead(6)  = 'Exterior Equipment'
  rowHead(7)  = 'Fans'
  rowHead(8)  = 'Pumps'
  rowHead(9)  = 'Heat Rejection'
  rowHead(10) = 'Humidification'
  rowHead(11) = 'Heat Recovery'
  rowHead(12) = 'Water Systems'
  rowHead(13) = 'Refrigeration'
  rowHead(14) = 'Generators'
  rowHead(15) = ''
  rowHead(16) = 'Total End Uses'

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Electricity [kWh]'
      columnHead(2) = 'Natural Gas [kWh]'
      columnHead(3) = 'Additional Fuel [kWh]'
      columnHead(4) = 'District Cooling [kWh]'
      columnHead(5) = 'District Heating [kWh]'
      columnHead(6) = 'Water [m3]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Electricity [kBtu]'
      columnHead(2) = 'Natural Gas [kBtu]'
      columnHead(3) = 'Additional Fuel [kBtu]'
      columnHead(4) = 'District Cooling [kBtu]'
      columnHead(5) = 'District Heating [kBtu]'
      columnHead(6) = 'Water [gal]'
    CASE DEFAULT
      columnHead(1) = 'Electricity [GJ]'
      columnHead(2) = 'Natural Gas [GJ]'
      columnHead(3) = 'Additional Fuel [GJ]'
      columnHead(4) = 'District Cooling [GJ]'
      columnHead(5) = 'District Heating [GJ]'
      columnHead(6) = 'Water [m3]'
  END SELECT

  tableBody = ''
  DO iResource= 1,6
    DO jEndUse=1,14
      tableBody(jEndUse,iResource) = TRIM(RealToStr(useVal(jEndUse,iResource),2))
    END DO
    tableBody(16,iResource) = TRIM(RealToStr(useVal(15,iResource),2))
  END DO
  !complete the LEED end use table using the same values
  ! for certain rows in the LEED table the subcategories are necessary so first compute those values
  leedFansParkFromFan = 0.0d0
  leedFansParkFromExtFuelEquip = 0.0d0
  leedIntLightProc = 0.0d0
  leedCook = 0.0d0
  leedIndProc = 0.0d0
  leedElevEsc = 0.0d0

  DO iResource = 1, 5    ! don't bother with water
    DO jEndUse = 1, NumEndUses
      IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          subCatName = EndUseCategory(jEndUse)%SubcategoryName(kEndUseSub)
          IF (SameString(subCatName,'Fans - Parking Garage')) THEN
            IF (jEndUse .EQ. 7) THEN  !fans
              leedFansParkFromFan(iResource) = leedFansParkFromFan(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
            ELSE
              leedFansParkFromExtFuelEquip(iResource) = leedFansParkFromExtFuelEquip(iResource) &
                   + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
            END IF
          ELSEIF (SameString(subCatName,'Interior Lighting - Process')) THEN
            leedIntLightProc(iResource) = leedIntLightProc(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          ELSEIF (SameString(subCatName,'Cooking')) THEN
            leedCook(iResource) = leedCook(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          ELSEIF (SameString(subCatName,'Industrial Process')) THEN
            leedIndProc(iResource) = leedIndProc(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          ELSEIF (SameString(subCatName,'Elevators and Escalators')) THEN
            leedElevEsc(iResource) = leedElevEsc(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          END IF
        END DO
      END IF
    END DO
  END DO

  unconvert = largeConversionFactor / 1000000000.d0  !to avoid double converting, the values for the LEED report should be in GJ
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Interior Lighting',unconvert * (useVal(3,colElectricity) &
                    - leedIntLightProc(colElectricity)),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Exterior Lighting',unconvert * useVal(4,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Space Heating',unconvert * useVal(1,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Space Cooling',unconvert * useVal(2,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Pumps',unconvert * useVal(8,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Heat Rejection',unconvert * useVal(9,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Fans-Interior',unconvert * (useVal(7,colElectricity) &
                     - leedFansParkFromFan(colElectricity)),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Fans-Parking Garage',unconvert * (leedFansParkFromFan(colElectricity) &
                     + leedFansParkFromExtFuelEquip(colElectricity)),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Service Water Heating',unconvert * useVal(12,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Receptacle Equipment',unconvert * useVal(5,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Interior Lighting (process)',unconvert * leedIntLightProc(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Refrigeration Equipment',unconvert * useVal(13,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Cooking',unconvert * leedCook(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Industrial Process',unconvert * leedIndProc(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Elevators and Escalators',unconvert * leedElevEsc(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElEneUse,'Total Line',unconvert * useVal(15,colElectricity),2)
  !  Energy Use Intensities
  IF (buildingGrossFloorArea .GT. 0) THEN
    CALL PreDefTableEntry(pdchLeedEuiElec,'Interior Lighting',unconvert * 1000 * (useVal(3,colElectricity) &
                      - leedIntLightProc(colElectricity))/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Space Heating',unconvert * 1000 * useVal(1,colElectricity)/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Space Cooling',unconvert * 1000 * useVal(2,colElectricity)/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Fans-Interior',unconvert * 1000 * (useVal(7,colElectricity) &
                      - leedFansParkFromFan(colElectricity))/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Service Water Heating',unconvert * 1000 &
                      * useVal(12,colElectricity)/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Receptacle Equipment',unconvert * 1000 &
                      * useVal(5,colElectricity)/buildingGrossFloorArea,2)
    nonMisc = useVal(3,colElectricity) - leedIntLightProc(colElectricity) &
              + useVal(1,colElectricity) +  useVal(2,colElectricity) &
              + useVal(7,colElectricity) - leedFansParkFromFan(colElectricity) &
              + useVal(12,colElectricity) + useVal(5,colElectricity)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Miscellaneous',unconvert * 1000 * (useVal(15,colElectricity) - nonMisc) &
              / buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiElec,'Subtotal',unconvert * 1000 * useVal(15,colElectricity)/buildingGrossFloorArea,2)
  END IF

  CALL PreDefTableEntry(pdchLeedEusTotal,'Electricity',unconvert * useVal(15,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedEusProc,'Electricity',unconvert * (useVal(5,colElectricity) + useVal(13,colElectricity)),2)
  IF (useVal(15,colElectricity) .NE. 0) THEN
    processFraction = (useVal(5,colElectricity) + useVal(13,colElectricity))/useVal(15,colElectricity)
    processElecCost = LEEDelecCostTotal * processFraction
  ELSE
    processElecCost = 0.0d0
  END IF
  CALL PreDefTableEntry(pdchLeedEcsProc,'Electricity',processElecCost,2)
  CALL addFootNoteSubTable(pdstLeedEneCostSum,'Process energy cost based on ratio of process to total energy.')

  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Interior Lighting',unconvert * (useVal(3,colGas) - leedIntLightProc(colGas)),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Exterior Lighting',unconvert * useVal(4,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Space Heating',unconvert * useVal(1,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Space Cooling',unconvert * useVal(2,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Pumps',unconvert * useVal(8,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Heat Rejection',unconvert * useVal(9,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Fans-Interior',unconvert * (useVal(7,colGas)- leedFansParkFromFan(colGas)),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Fans-Parking Garage',unconvert * (leedFansParkFromFan(colGas) &
                    + leedFansParkFromExtFuelEquip(colGas)),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Service Water Heating',unconvert * useVal(12,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Receptacle Equipment',unconvert * useVal(5,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Interior Lighting (process)',unconvert * leedIntLightProc(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Refrigeration Equipment',unconvert * useVal(13,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Cooking',unconvert * leedCook(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Industrial Process',unconvert * leedIndProc(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Elevators and Escalators',unconvert * leedElevEsc(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasEneUse,'Total Line',unconvert * useVal(15,colGas),2)
  !  Energy Use Intensities
  IF (buildingGrossFloorArea .GT. 0) THEN
    CALL PreDefTableEntry(pdchLeedEuiNatG,'Space Heating',unconvert * 1000 * useVal(1,colGas)/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiNatG,'Service Water Heating',unconvert * 1000 * useVal(12,colGas)/buildingGrossFloorArea,2)
    nonMisc = useVal(1,colGas) + useVal(12,colGas)
    CALL PreDefTableEntry(pdchLeedEuiNatG,'Miscellaneous',unconvert * 1000 * (useVal(15,colGas) - nonMisc) &
           / buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiNatG,'Subtotal',unconvert * 1000 * useVal(15,colGas)/buildingGrossFloorArea,2)
  END IF
  CALL PreDefTableEntry(pdchLeedEusTotal,'Natural Gas',unconvert * useVal(15,colGas),2)
  CALL PreDefTableEntry(pdchLeedEusProc,'Natural Gas',unconvert * (useVal(5,colGas) + useVal(13,colGas)),2)
  IF (useVal(15,colGas) .NE. 0) THEN
    processFraction = (useVal(5,colGas) + useVal(13,colGas))/useVal(15,colGas)
    processGasCost = LEEDgasCostTotal * processFraction
  ELSE
    processGasCost = 0.0d0
  END IF
  CALL PreDefTableEntry(pdchLeedEcsProc,'Natural Gas',processGasCost,2)

  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Interior Lighting',unconvert *   &
                                    (useVal(3,colAdditionalFuel) + useVal(3,colPurchCool) &
                                       + useVal(3,colPurchHeat) - (leedIntLightProc(colAdditionalFuel) &
                                       + leedIntLightProc(colPurchCool) + leedIntLightProc(colPurchHeat))) ,2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Exterior Lighting',unconvert *   &
                                    (useVal(4,colAdditionalFuel) + useVal(4,colPurchCool) &
                                       + useVal(4,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Space Heating',unconvert * (useVal(1,colAdditionalFuel) + useVal(1,colPurchCool) &
                                       + useVal(1,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Space Cooling',unconvert * (useVal(2,colAdditionalFuel) + useVal(2,colPurchCool) &
                                       + useVal(2,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Pumps',unconvert * (useVal(8,colAdditionalFuel) + useVal(8,colPurchCool) &
                                       + useVal(8,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Heat Rejection',unconvert * (useVal(9,colAdditionalFuel) + useVal(9,colPurchCool) &
                                       + useVal(9,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Fans-Interior',unconvert * (useVal(7,colAdditionalFuel) +  useVal(7,colPurchCool) &
                                       + useVal(7,colPurchHeat) - (leedFansParkFromFan(colAdditionalFuel) &
                                       + leedFansParkFromFan(colPurchCool) + leedFansParkFromFan(colPurchHeat))),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Fans-Parking Garage',unconvert * (leedFansParkFromFan(colAdditionalFuel) &
                                       + leedFansParkFromFan(colPurchCool) + leedFansParkFromFan(colPurchHeat) &
                                       + leedFansParkFromExtFuelEquip(colAdditionalFuel)   &
                                       + leedFansParkFromExtFuelEquip(colPurchCool) &
                                       + leedFansParkFromExtFuelEquip(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Service Water Heating',unconvert * (useVal(12,colAdditionalFuel) &
                                       + useVal(12,colPurchCool) + useVal(12,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Receptacle Equipment',unconvert * (useVal(5,colAdditionalFuel) &
                                       + useVal(5,colPurchCool) + useVal(5,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Interior Lighting (process)',unconvert * (leedIntLightProc(colAdditionalFuel) &
                                      + leedIntLightProc(colPurchCool) + leedIntLightProc(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Refrigeration Equipment',unconvert * (useVal(13,colAdditionalFuel) &
                                      + useVal(13,colPurchCool) + useVal(13,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Cooking',unconvert * (leedCook(colAdditionalFuel) + leedCook(colPurchCool) &
                                      + leedCook(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Industrial Process',unconvert * (leedIndProc(colAdditionalFuel) &
                                      + leedIndProc(colPurchCool) + leedIndProc(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Elevators and Escalators',unconvert * (leedElevEsc(colAdditionalFuel) &
                                      + leedElevEsc(colPurchCool) + leedElevEsc(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthEneUse,'Total Line',unconvert * (useVal(15,colAdditionalFuel) + useVal(15,colPurchCool) &
                                      + useVal(15,colPurchHeat)),2)
  !  Energy Use Intensities
  IF (buildingGrossFloorArea .GT. 0) THEN
    CALL PreDefTableEntry(pdchLeedEuiOthr,'Miscellaneous',unconvert * 1000 * useVal(15,colAdditionalFuel)/buildingGrossFloorArea,2)
    CALL PreDefTableEntry(pdchLeedEuiOthr,'Subtotal',unconvert * 1000 * useVal(15,colAdditionalFuel)/buildingGrossFloorArea,2)
  END IF
  CALL PreDefTableEntry(pdchLeedEusTotal,'Additional',unconvert * (useVal(15,colAdditionalFuel) + useVal(15,colPurchCool) &
                                                             + useVal(15,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedEusProc,'Additional',unconvert * (useVal(5,colAdditionalFuel) + useVal(13,colAdditionalFuel) &
                                                             + useVal(5,colPurchCool) + useVal(13,colPurchCool) &
                                                             + useVal(5,colPurchHeat) + useVal(13,colPurchHeat)) ,2)
  IF ((useVal(15,colAdditionalFuel) + useVal(15,colPurchCool) + useVal(15,colPurchHeat)) .GT. 0.001d0) THEN
              processFraction = (useVal(5,colAdditionalFuel) + useVal(13,colAdditionalFuel) &
                              + useVal(5,colPurchCool) + useVal(13,colPurchCool) &
                              + useVal(5,colPurchHeat) + useVal(13,colPurchHeat))&
                              /(useVal(15,colAdditionalFuel) + useVal(15,colPurchCool) + useVal(15,colPurchHeat))
  ELSE
     processFraction = 0.0d0
  END IF
  processOthrCost = LEEDothrCostTotal * processFraction
  CALL PreDefTableEntry(pdchLeedEcsProc,'Additional',processOthrCost,2)
  CALL PreDefTableEntry(pdchLeedEcsProc,'Total',processElecCost + processGasCost + processOthrCost,2)
  ! accumulate for percentage table
  leedSiteIntLite = 0.0d0
  leedSiteSpHeat = 0.0d0
  leedSiteSpCool = 0.0d0
  leedSiteFanInt = 0.0d0
  leedSiteSrvWatr = 0.0d0
  leedSiteRecept = 0.0d0
  leedSiteTotal  = 0.0d0
  DO iResource = 1, 5    ! don't bother with water
    leedSiteIntLite = leedSiteIntLite + useVal(3,iResource) - leedIntLightProc(iResource)
    leedSiteSpHeat = leedSiteSpHeat + useVal(1,iResource)
    leedSiteSpCool = leedSiteSpCool + useVal(2,iResource)
    leedSiteFanInt = leedSiteFanInt + useVal(7,iResource) - leedFansParkFromFan(iResource)
    leedSiteSrvWatr = leedSiteSrvWatr + useVal(12,iResource)
    leedSiteRecept = leedSiteRecept + useVal(5,iResource)
    leedSiteTotal  = leedSiteTotal + useVal(15,iResource)
  END DO
  IF (leedSiteTotal .NE. 0) THEN
    CALL PreDefTableEntry(pdchLeedEupPerc,'Interior Lighting',100 * leedSiteIntLite / leedSiteTotal,2)
    CALL PreDefTableEntry(pdchLeedEupPerc,'Space Heating',100 * leedSiteSpHeat / leedSiteTotal,2)
    CALL PreDefTableEntry(pdchLeedEupPerc,'Space Cooling',100 * leedSiteSpCool / leedSiteTotal,2)
    CALL PreDefTableEntry(pdchLeedEupPerc,'Fans-Interior',100 * leedSiteFanInt / leedSiteTotal,2)
    CALL PreDefTableEntry(pdchLeedEupPerc,'Service Water Heating',100 * leedSiteSrvWatr / leedSiteTotal,2)
    CALL PreDefTableEntry(pdchLeedEupPerc,'Receptacle Equipment',100 * leedSiteRecept / leedSiteTotal,2)
    CALL PreDefTableEntry(pdchLeedEupPerc,'Miscellaneous',100 * (leedSiteTotal &
      - (leedSiteIntLite + leedSiteSpHeat + leedSiteSpCool + leedSiteFanInt + leedSiteSrvWatr + leedSiteRecept)) / leedSiteTotal,2)
  END IF
  !totals across energy source
  CALL PreDefTableEntry(pdchLeedEusTotal,'Total',unconvert * (useVal(15,colAdditionalFuel) + useVal(15,colPurchCool)  &
                                         + useVal(15,colPurchHeat) + useVal(15,colElectricity) + useVal(15,colGas)),2)
  CALL PreDefTableEntry(pdchLeedEusProc,'Total',unconvert * (useVal(5,colAdditionalFuel) + useVal(13,colAdditionalFuel) &
                                                             + useVal(5,colPurchCool) + useVal(13,colPurchCool) &
                                                             + useVal(5,colPurchHeat) + useVal(13,colPurchHeat) &
                                                             + useVal(5,colElectricity) + useVal(13,colElectricity) &
                                                             + useVal(5,colGas) + useVal(13,colGas)),2)

  footnote = ''
  SELECT CASE (resourcePrimaryHeating)
    CASE (colElectricity)
      footnote = 'Note: Electricity appears to be the principal heating source based on energy usage. '
      CALL PreDefTableEntry(pdchLeedGenData,'Principal Heating Source','Electricity')
    CASE (colGas)
      footnote = 'Note: Natural gas appears to be the principal heating source based on energy usage. '
      CALL PreDefTableEntry(pdchLeedGenData,'Principal Heating Source','Natural Gas')
    CASE (colAdditionalFuel)
      footnote = 'Note: Additional fuel appears to be the principal heating source based on energy usage. '
      CALL PreDefTableEntry(pdchLeedGenData,'Principal Heating Source','Additional Fuel')
    CASE (colPurchHeat)
      footnote = 'Note: District heat appears to be the principal heating source based on energy usage. '
      CALL PreDefTableEntry(pdchLeedGenData,'Principal Heating Source','District Heat')
  END SELECT
  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('End Uses')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.FALSE., footnote)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'End Uses')
  ENDIF
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)


  !
  !---- End Uses By Subcategory Sub-Table
  !

  !determine if subcategories add up to the total and
  !if not, determine the difference for the 'other' row
  needOtherRow = .FALSE. !set array to all false assuming no other rows are needed
  DO iResource = 1, 6
    DO jEndUse = 1, NumEndUses
      IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
        !set the value to the total for the end use
        endUseSubOther(jEndUse,iResource) = collapsedEndUse(jEndUse,iResource)
        ! subtract off each sub end use category value
        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          endUseSubOther(jEndUse,iResource) = endUseSubOther(jEndUse,iResource) &
            - collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
        END DO
        !if just a small value remains set it to zero
        IF (ABS(endUseSubOther(jEndUse,iResource)) .GT. 0.01d0) THEN
          needOtherRow(jEndUse) = .TRUE.
        ELSE
          endUseSubOther(jEndUse,iResource) = 0.0d0
        END IF
      ELSE
        endUseSubOther(jEndUse,iResource) = 0.0d0
      END IF
    END DO
  END DO

  !determine the number of rows needed for sub-table
  numRows = 0
  DO jEndUse = 1, NumEndUses
    IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
      DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
        numRows = numRows + 1
      END DO
      ! check if an 'other' row is needed
      IF (needOtherRow(jEndUse)) THEN
        numRows = numRows + 1
      END IF
    ELSE
      numRows = numRows + 1
    END IF
  END DO

  ALLOCATE(rowHead(numRows))
  ALLOCATE(columnHead(7))
  ALLOCATE(columnWidth(7))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(numRows,7))

  rowHead = ''
  tableBody = ''

  ! Build row head and subcategories columns
  i = 1
  DO jEndUse = 1, NumEndUses
    rowHead(i) = EndUseCategory(jEndUse)%DisplayName
    IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
      DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
        tableBody(i,1) = EndUseCategory(jEndUse)%SubcategoryName(kEndUseSub)
        i = i + 1
      END DO
      ! check if an 'other' row is needed
      IF (needOtherRow(jEndUse)) THEN
        tableBody(i,1) = 'Other'
        i = i + 1
      END IF
    ELSE
      tableBody(i,1) = 'General'
      i = i + 1
    END IF
  END DO

  columnHead(1) = 'Subcategory'

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(2) = 'Electricity [kWh]'
      columnHead(3) = 'Natural Gas [kWh]'
      columnHead(4) = 'Additional Fuel [kWh]'
      columnHead(5) = 'District Cooling [kWh]'
      columnHead(6) = 'District Heating [kWh]'
      columnHead(7) = 'Water [m3]'
    CASE (unitsStyleInchPound)
      columnHead(2) = 'Electricity [kBtu]'
      columnHead(3) = 'Natural Gas [kBtu]'
      columnHead(4) = 'Additional Fuel [kBtu]'
      columnHead(5) = 'District Cooling [kBtu]'
      columnHead(6) = 'District Heating [kBtu]'
      columnHead(7) = 'Water [gal]'
    CASE DEFAULT
      columnHead(2) = 'Electricity [GJ]'
      columnHead(3) = 'Natural Gas [GJ]'
      columnHead(4) = 'Additional Fuel [GJ]'
      columnHead(5) = 'District Cooling [GJ]'
      columnHead(6) = 'District Heating [GJ]'
      columnHead(7) = 'Water [m3]'
  END SELECT

  DO iResource = 1, 6
    i = 1
    DO jEndUse = 1, NumEndUses
      IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          tableBody(i,iResource+1) = TRIM(RealToStr(collapsedEndUseSub(iResource,jEndUse,kEndUseSub),2))
          i = i + 1
        END DO
        !put other
        IF (needOtherRow(jEndUse)) THEN
          tableBody(i,iResource+1) = TRIM(RealToStr(endUseSubOther(jEndUse,iResource),2))
          i = i + 1
        END IF
      ELSE
        tableBody(i,iResource+1) = TRIM(RealToStr(collapsedEndUse(jEndUse,iResource),2))
        i = i + 1
      END IF
    END DO
  END DO

  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('End Uses By Subcategory')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'End Uses By Subcategory')
  ENDIF
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Normalized by Conditioned Area Sub-Table
  !
  ! Calculations for both normalized tables are first
  ALLOCATE(rowHead(4))
  ALLOCATE(columnHead(6))
  ALLOCATE(columnWidth(6))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(4,6))
  DO iResource= 1,6
    normalVal(1,iResource)  = collapsedEndUse(endUseInteriorLights,iResource) &     !Lights     <- InteriorLights
                            + collapsedEndUse(endUseExteriorLights,iResource)       !           <- ExteriorLights

    normalVal(2,iResource)  = collapsedEndUse(endUseFans,iResource)  &              !HVAC       <- fans
                            + collapsedEndUse(endUsePumps,iResource)  &             !           <- pumps
                            + collapsedEndUse(endUseHeating,iResource)  &           !           <- heating
                            + collapsedEndUse(endUseCooling,iResource)  &           !           <- cooling
                            + collapsedEndUse(endUseHeatRejection,iResource) &      !           <- heat rejection
                            + collapsedEndUse(endUseHumidification,iResource) &     !           <- humidification
                            + collapsedEndUse(endUseWaterSystem,iResource)          !           <- water system domestic hot water

    normalVal(3,iResource)  = collapsedEndUse(endUseInteriorEquipment,iResource)  & !Other      <- InteriorEquipment
                            + collapsedEndUse(endUseExteriorEquipment,iResource)  & !           <- ExteriorEquipment
                            + collapsedEndUse(endUseCogeneration,iResource) &       !           <- generator fuel
                            + collapsedEndUse(endUseHeatRecovery,iResource) &       !           <- Heat Recovery (parasitics)
                            + collapsedEndUse(endUseRefrigeration,iResource)        !           <- Refrigeration

    normalVal(4,iResource) = collapsedTotal(iResource)           ! totals
  END DO
  ! convert the normalized end use values to MJ from GJ if using J
  DO iResource= 1,5   !not including resource=6 water
    DO jEndUse=1,4
      normalVal(jEndUse,iResource) = normalVal(jEndUse,iResource) * kConversionFactor
    END DO
  END DO

  rowHead(1)  = 'Lighting'   !typo fixed 5-17-04 BTG
  rowHead(2)  = 'HVAC'
  rowHead(3)  = 'Other'
  rowHead(4)  = 'Total'

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Electricity Intensity [kWh/m2]'
      columnHead(2) = 'Natural Gas Intensity [kWh/m2]'
      columnHead(3) = 'Additional Fuel Intensity [kWh/m2]'
      columnHead(4) = 'District Cooling Intensity [kWh/m2]'
      columnHead(5) = 'District Heating Intensity [kWh/m2]'
      columnHead(6) = 'Water Intensity [m3/m2]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Electricity Intensity [kBtu/ft2]'
      columnHead(2) = 'Natural Gas Intensity [kBtu/ft2]'
      columnHead(3) = 'Additional Fuel Intensity [kBtu/ft2]'
      columnHead(4) = 'District Cooling Intensity [kBtu/ft2]'
      columnHead(5) = 'District Heating Intensity [kBtu/ft2]'
      columnHead(6) = 'Water Intensity [gal/ft2]'
    CASE DEFAULT
      columnHead(1) = 'Electricity Intensity [MJ/m2]'
      columnHead(2) = 'Natural Gas Intensity [MJ/m2]'
      columnHead(3) = 'Additional Fuel Intensity [MJ/m2]'
      columnHead(4) = 'District Cooling Intensity [MJ/m2]'
      columnHead(5) = 'District Heating Intensity [MJ/m2]'
      columnHead(6) = 'Water Intensity [m3/m2]'
  END SELECT

  CALL writeTextLine('Normalized Metrics',.TRUE.)

  ! write the conditioned area based table
  tableBody = ''
  IF (convBldgCondFloorArea .GT. 0) THEN
    DO iResource= 1,6
      DO jEndUse=1,4
        tableBody(jEndUse,iResource) = TRIM(RealToStr(normalVal(jEndUse,iResource) / convBldgCondFloorArea,2))
      END DO
    END DO
  END IF
  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Utility Use Per Conditioned Floor Area')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Utility Use Per Conditioned Floor Area')
  ENDIF
  !
  !---- Normalized by Total Area Sub-Table
  !
  tableBody = ''
  IF (convBldgGrossFloorArea .GT. 0) THEN
    DO iResource= 1,6
      DO jEndUse=1,4
        tableBody(jEndUse,iResource) = TRIM(RealToStr(normalVal(jEndUse,iResource) / convBldgGrossFloorArea,2))
      END DO
    END DO
  END IF
  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Utility Use Per Total Floor Area')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Utility Use Per Total Floor Area')
  ENDIF

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Electric Loads Satisfied Sub-Table
  !
  ALLOCATE(rowHead(13))
  ALLOCATE(columnHead(2))
  ALLOCATE(columnWidth(2))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(13,2))

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Electricity [kWh]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Electricity [kBtu]'
    CASE DEFAULT
      columnHead(1) = 'Electricity [GJ]'
  END SELECT
  columnHead(2) = 'Percent Electricity [%]'

  rowHead(1)  = 'Fuel-Fired Power Generation'
  rowHead(2)  = 'High Temperature Geothermal*'
  rowHead(3)  = 'Photovoltaic Power'
  rowHead(4)  = 'Wind Power'
  rowHead(5)  = 'Net Decrease in On-Site Storage'
  rowHead(6)  = 'Total On-Site Electric Sources'
  rowHead(7)  = ''
  rowHead(8)  = 'Electricity Coming From Utility'
  rowHead(9)  = 'Surplus Electricity Going To Utility'
  rowHead(10)  = 'Net Electricity From Utility'
  rowHead(11) = ''
  rowHead(12) = 'Total On-Site and Utility Electric Sources'
  rowHead(13) = 'Total Electricity End Uses'

  tableBody = ''

  ! show annual values
  unconvert = largeConversionFactor / 1000000000.d0  !to avoid double converting, the values for the LEED report should be in GJ

  tableBody(1,1)  = TRIM(RealToStr(gatherPowerFuelFireGen,2))
  tableBody(2,1)  = TRIM(RealToStr(gatherPowerHTGeothermal,2))
  tableBody(3,1)  = TRIM(RealToStr(gatherPowerPV,2))
  CALL PreDefTableEntry(pdchLeedRenAnGen,'Photovoltaic',unconvert * gatherPowerPV,2)
  tableBody(4,1)  = TRIM(RealToStr(gatherPowerWind,2))
  CALL PreDefTableEntry(pdchLeedRenAnGen,'Wind',unconvert * gatherPowerWind,2)
  tableBody(5,1)  = TRIM(RealToStr(OverallNetEnergyFromStorage, 2 ))
  tableBody(6,1)  = TRIM(RealToStr(gatherElecProduced,2))
  tableBody(8,1)  = TRIM(RealToStr(gatherElecPurchased,2))
  tableBody(9,1)  = TRIM(RealToStr(gatherElecSurplusSold,2))
  tableBody(10,1)  = TRIM(RealToStr(gatherElecPurchased - gatherElecSurplusSold,2))
  tableBody(12,1) = TRIM(RealToStr(gatherElecProduced + (gatherElecPurchased - gatherElecSurplusSold),2))
  tableBody(13,1) = TRIM(RealToStr(collapsedTotal(1),2))

  ! show annual percentages
  IF (collapsedTotal(1) .GT. 0) THEN
    tableBody(1,2)  = TRIM(RealToStr(100.0d0 * gatherPowerFuelFireGen / collapsedTotal(1),2))
    tableBody(2,2)  = TRIM(RealToStr(100.0d0 * gatherPowerHTGeothermal / collapsedTotal(1),2))
    tableBody(3,2)  = TRIM(RealToStr(100.0d0 * gatherPowerPV / collapsedTotal(1),2))
    tableBody(4,2)  = TRIM(RealToStr(100.0d0 * gatherPowerWind / collapsedTotal(1),2))
    tableBody(5,2)  = TRIM(RealToStr(100.0d0 * OverallNetEnergyFromStorage / collapsedTotal(1),2))
    tableBody(6,2)  = TRIM(RealToStr(100.0d0 * gatherElecProduced / collapsedTotal(1),2))
    tableBody(8,2)  = TRIM(RealToStr(100.0d0 * gatherElecPurchased / collapsedTotal(1),2))
    tableBody(9,2)  = TRIM(RealToStr(100.0d0 * gatherElecSurplusSold / collapsedTotal(1),2))
    tableBody(10,2) = TRIM(RealToStr(100.0d0 * (gatherElecPurchased - gatherElecSurplusSold) / collapsedTotal(1),2))
    tableBody(12,2) = TRIM(RealToStr(100.0d0 * (gatherElecProduced + (gatherElecPurchased - gatherElecSurplusSold)) /   &
                                               collapsedTotal(1),2))
    tableBody(13,2) = TRIM(RealToStr(100.0d0,2))
  END IF

  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Electric Loads Satisfied')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Electric Loads Satisfied')
  ENDIF

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- On-Site Thermal Sources Sub-Table
  !
  ALLOCATE(rowHead(7))
  ALLOCATE(columnHead(2))
  ALLOCATE(columnWidth(2))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(7,2))

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Heat [kWh]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Heat [kBtu]'
    CASE DEFAULT
      columnHead(1) = 'Heat [GJ]'
  END SELECT

  columnHead(2) = 'Percent Heat [%]'

  rowHead(1)  = 'Water-Side Heat Recovery'
  rowHead(2)  = 'Air to Air Heat Recovery for Cooling'
  rowHead(3)  = 'Air to Air Heat Recovery for Heating'
  rowHead(4)  = 'High-Temperature Geothermal*'
  rowHead(5)  = 'Solar Water Thermal'
  rowHead(6)  = 'Solar Air Thermal'
  rowHead(7)  = 'Total On-Site Thermal Sources'

  tableBody = ''

  ! convert to GJ

  gatherWaterHeatRecovery   =  gatherWaterHeatRecovery    / largeConversionFactor
  gatherAirHeatRecoveryCool =  gatherAirHeatRecoveryCool  / largeConversionFactor
  gatherAirHeatRecoveryHeat =  gatherAirHeatRecoveryHeat  / largeConversionFactor
  gatherHeatHTGeothermal    =  gatherHeatHTGeothermal     / largeConversionFactor
  gatherHeatSolarWater      =  gatherHeatSolarWater       / largeConversionFactor
  gatherHeatSolarAir        =  gatherHeatSolarAir         / largeConversionFactor

  ! determine total on site heat
  totalOnsiteHeat = gatherWaterHeatRecovery &
                  + gatherAirHeatRecoveryCool &
                  + gatherAirHeatRecoveryHeat &
                  + gatherHeatHTGeothermal &
                  + gatherHeatSolarWater &
                  + gatherHeatSolarAir

  ! show annual values
  tableBody(1,1)  = TRIM(RealToStr(gatherWaterHeatRecovery,2))
  tableBody(2,1)  = TRIM(RealToStr(gatherAirHeatRecoveryCool,2))
  tableBody(3,1)  = TRIM(RealToStr(gatherAirHeatRecoveryHeat,2))
  tableBody(4,1)  = TRIM(RealToStr(gatherHeatHTGeothermal,2))
  tableBody(5,1)  = TRIM(RealToStr(gatherHeatSolarWater,2))
  tableBody(6,1)  = TRIM(RealToStr(gatherHeatSolarAir,2))
  tableBody(7,1)  = TRIM(RealToStr(totalOnsiteHeat,2))

  IF (totalOnsiteHeat .GT. 0) THEN
    tableBody(1,2)  = TRIM(RealToStr(100.0d0 * gatherWaterHeatRecovery / totalOnsiteHeat,2))
    tableBody(2,2)  = TRIM(RealToStr(100.0d0 * gatherAirHeatRecoveryCool / totalOnsiteHeat,2))
    tableBody(3,2)  = TRIM(RealToStr(100.0d0 * gatherAirHeatRecoveryHeat / totalOnsiteHeat,2))
    tableBody(4,2)  = TRIM(RealToStr(100.0d0 * gatherHeatHTGeothermal / totalOnsiteHeat,2))
    tableBody(5,2)  = TRIM(RealToStr(100.0d0 * gatherHeatSolarWater / totalOnsiteHeat,2))
    tableBody(6,2)  = TRIM(RealToStr(100.0d0 * gatherHeatSolarAir / totalOnsiteHeat,2))
    tableBody(7,2)  = TRIM(RealToStr(100.0d0,2))
  END IF

  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('On-Site Thermal Sources')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'On-Site Thermal Sources')
  ENDIF

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- Water Loads Sub-Table
  ! As of 12/8/2003 decided to not include this sub-table to wait
  ! until water use is implemented in EnergyPlus before displaying
  ! the table. Implementing water end-uses makes sense for EnergyPlus
  ! but since they are not really implemented as of December 2003 the
  ! table would be all zeros.  Recommendation to exclude this table
  ! for now made by Glazer and Crawley.
  !
  !Aug 2006, adding table in with implementation of water system, BGriffith
  !
  !
  ALLOCATE(rowHead(13))
  ALLOCATE(columnHead(2))
  ALLOCATE(columnWidth(2))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(13,2))
!
  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Water [m3]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Water [gal]'
    CASE DEFAULT
      columnHead(1) = 'Water [m3]'
  END SELECT
  columnHead(2) = 'Percent Water [%]'
!
  rowHead(1)  = 'Rainwater Collection'
  rowHead(2)  = 'Condensate Collection'
  rowHead(3)  = 'Groundwater Well'
  rowHead(4)  = 'Total On Site Water Sources'
  rowHead(5)  = '-'
  rowHead(6)  = 'Initial Storage'
  rowHead(7)  = 'Final Storage'
  rowHead(8)  = 'Change in Storage'
  rowHead(9)  = '-'
  rowHead(10)  = 'Water Supplied by Utility'
  rowHead(11)  = '-'
  rowHead(12)  = 'Total On Site, Change in Storage, and Utility Water Sources'
  rowHead(13)  = 'Total Water End Uses'
!
  tableBody = '-'
!

  totalOnsiteWater   = gatherRainWater + gatherCondensate  + gatherWellwater


!  ! show annual values
  tableBody(1,1)  = TRIM(RealToStr(gatherRainWater / waterConversionFactor ,2))
  tableBody(2,1)  = TRIM(RealToStr(gatherCondensate / waterConversionFactor,2))
  tableBody(3,1)  = TRIM(RealToStr(gatherWellwater / waterConversionFactor,2))
  tableBody(4,1)  = TRIM(RealToStr(totalOnsiteWater / waterConversionFactor,2))


  IF (allocated(WaterStorage)) Then
    initialStorage  = sum(waterStorage%InitialVolume)
    finalStorage    = SUM(WaterSTorage%ThisTimeStepVolume)
    StorageChange   = initialStorage-finalStorage
  else
    initialStorage  = 0.0d0
    finalStorage    = 0.0d0
    StorageChange   = 0.0d0
  endif
  tableBody(6,1)  = TRIM(RealToStr(initialStorage / waterConversionFactor,2))
  tableBody(7,1)  = TRIM(RealToStr(finalStorage / waterConversionFactor,2))
  tableBody(8,1)  = TRIM(RealToStr(StorageChange / waterConversionFactor,2))

  totalWater        = totalOnsiteWater + gatherMains + StorageChange

  tableBody(10,1)  = TRIM(RealToStr(gatherMains / waterConversionFactor,2))
  tableBody(12,1)  = TRIM(RealToStr(totalWater / waterConversionFactor,2))
  tableBody(13,1)  = TRIM(RealToStr(gatherWaterEndUseTotal / waterConversionFactor,2))
!

  IF (gatherWaterEndUseTotal .GT. 0) THEN
    tableBody(1,2)  = TRIM(RealToStr(100.0d0 * gatherRainWater / gatherWaterEndUseTotal,2))
    tableBody(2,2)  = TRIM(RealToStr(100.0d0 * gatherCondensate / gatherWaterEndUseTotal,2))
    tableBody(3,2)  = TRIM(RealToStr(100.0d0 * gatherWellwater / gatherWaterEndUseTotal,2))
    tableBody(4,2)  = TRIM(RealToStr(100.0d0 * totalOnsiteWater / gatherWaterEndUseTotal,2))
    tableBody(6,2)  = TRIM(RealToStr(100.0d0 * initialStorage/ gatherWaterEndUseTotal,2))
    tableBody(7,2)  = TRIM(RealToStr(100.0d0 * finalStorage / gatherWaterEndUseTotal,2))
    tableBody(8,2)  = TRIM(RealToStr(100.0d0 * StorageChange/ gatherWaterEndUseTotal,2))

    tableBody(10,2)  = TRIM(RealToStr(100.0d0 * gatherMains / gatherWaterEndUseTotal,2))

    tableBody(12,2)  = TRIM(RealToStr(100.0d0 * totalWater / gatherWaterEndUseTotal,2))
    tableBody(13,2)  = TRIM(RealToStr(100.0d0,2))
  END IF
!

!  ! heading for the entire sub-table
  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Water Source Summary')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Water Source Summary')
  ENDIF

!
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- Comfort and Setpoint Not Met Sub-Table
  !
  IF (displayTabularBEPS) THEN
    ALLOCATE(rowHead(2))
    ALLOCATE(columnHead(1))
    ALLOCATE(columnWidth(1))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(2,1))

    CALL writeSubtitle('Setpoint Not Met Criteria')

    curNameWithSIUnits = 'Degrees [deltaC]'
    curNameAndUnits = curNameWithSIUnits
    IF (unitsStyle .EQ. unitsStyleInchPound) THEN
      CALL LookupSItoIP(curNameWithSIUnits, indexUnitConv, curNameAndUnits)
    ENDIF
    columnHead(1)=curNameAndUnits

    rowHead(1)  = 'Tolerance for Zone Heating Setpoint Not Met Time'
    rowHead(2)  = 'Tolerance for Zone Cooling Setpoint Not Met Time'

    IF (unitsStyle .NE. unitsStyleInchPound) THEN
      tableBody(1,1)  = TRIM(RealToStr(abs(deviationFromSetPtThresholdHtg),2))
      tableBody(2,1)  = TRIM(RealToStr(deviationFromSetPtThresholdClg,2))
    ELSE
      tableBody(1,1)  = TRIM(RealToStr(ConvertIPDelta(indexUnitConv,abs(deviationFromSetPtThresholdHtg)),2))
      tableBody(2,1)  = TRIM(RealToStr(ConvertIPDelta(indexUnitConv,deviationFromSetPtThresholdClg),2))
    ENDIF

    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'AnnualBuildingUtilityPerformanceSummary',&
                                        'Entire Facility',&
                                        'Setpoint Not Met Criteria')

    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)
  ENDIF

  ALLOCATE(rowHead(3))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(3,1))

  IF (displayTabularBEPS) THEN
    CALL writeSubtitle('Comfort and Setpoint Not Met Summary')
  ENDIF

  columnHead(1) = 'Facility [Hours]'

  rowHead(1)  = 'Time Setpoint Not Met During Occupied Heating'
  rowHead(2)  = 'Time Setpoint Not Met During Occupied Cooling'
  rowHead(3)  = 'Time Not Comfortable Based on Simple ASHRAE 55-2004'

  tableBody(1,1)  = TRIM(RealToStr(TotalNotMetHeatingOccupiedForABUPS,2))
  tableBody(2,1)  = TRIM(RealToStr(TotalNotMetCoolingOccupiedForABUPS,2))
  CALL PreDefTableEntry(pdchLeedAmData,'Number of hours heating loads not met', &
          TRIM(RealToStr(TotalNotMetHeatingOccupiedForABUPS,2)))
  CALL PreDefTableEntry(pdchLeedAmData,'Number of hours cooling loads not met', &
          TRIM(RealToStr(TotalNotMetCoolingOccupiedForABUPS,2)))
  CALL PreDefTableEntry(pdchLeedAmData,'Number of hours not met',TRIM(RealToStr(TotalNotMetOccupiedForABUPS,2)))
  tableBody(3,1)  = TRIM(RealToStr(TotalTimeNotSimpleASH55EitherForABUPS,2))

  IF (displayTabularBEPS) THEN
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'AnnualBuildingUtilityPerformanceSummary',&
                                      'Entire Facility',&
                                      'Comfort and Setpoint Not Met Summary')
  ENDIF


  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)


  !
  !---- Control Summary Sub-Table
  !

  !
  !---- End Notes
  !
  IF (displayTabularBEPS) THEN
    CALL writeTextLine('Note 1: An asterisk (*) indicates that the feature is not yet implemented.')
  ENDIF
  !CALL writeTextLine('Note 2: The source energy conversion factors used are: ')
  !CALL writeTextLine('        1.05 for all fuels, 1 for district, and 3 for electricity.')
END IF
END SUBROUTINE WriteBEPSTable

SUBROUTINE WriteSourceEnergyEndUseSummary
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar
          !       DATE WRITTEN   September 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Take the gathered total and end use source energy meter data and structure
          !   the results into a tabular report for output.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: MaxNumSubcategories, EndUseCategory
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody

! all arrays are in the format: (row, columnm)
REAL(r64),DIMENSION(15,6)                           :: useVal
REAL(r64),DIMENSION(6)                              :: collapsedTotal
REAL(r64),DIMENSION(numEndUses,6)                   :: collapsedEndUse
REAL(r64),DIMENSION(6,numEndUses,MaxNumSubcategories) :: collapsedEndUseSub
REAL(r64)                                      :: totalSourceEnergyUse
INTEGER                                        :: iResource
INTEGER                                        :: jEndUse
INTEGER                                        :: kEndUseSub
INTEGER                                        :: i
REAL(r64)                                      :: largeConversionFactor
INTEGER                                        :: numRows
CHARACTER(len=100)                             :: footnote = ' '
REAL(r64)                                      :: areaConversionFactor
REAL(r64)                                      :: convBldgGrossFloorArea
REAL(r64)                                      :: convBldgCondFloorArea
CHARACTER(len=MaxNameLength) :: curNameWithSIUnits
CHARACTER(len=MaxNameLength) :: curNameAndUnits
INTEGER :: indexUnitConv

IF (displaySourceEnergyEndUseSummary) THEN
  ! show the headers of the report
  CALL WriteReportHeaders('Source Energy End Use Components Summary','Entire Facility',isAverage)
  ! show the number of hours that the table applies to
  CALL writeTextLine('Values gathered over ' // RealToStr(gatherElapsedTimeBEPS,2) // ' hours',.TRUE.)
  IF (gatherElapsedTimeBEPS .LT. 8759.0d0) THEN  ! might not add up to 8760 exactly but can't be more than 1 hour diff.
    CALL writeTextLine('WARNING: THE REPORT DOES NOT REPRESENT A FULL ANNUAL SIMULATION.',.TRUE.)
  END IF
  CALL writeTextLine('',.TRUE.)
  ! determine building floor areas
  CALL DetermineBuildingFloorArea
  ! collapse the gatherEndUseBEPS array to the resource groups displayed
  DO jEndUse=1,numEndUses
    collapsedEndUse(jEndUse,1) = gatherEndUseBySourceBEPS(jEndUse,1)    !electricity
    collapsedEndUse(jEndUse,2) = gatherEndUseBySourceBEPS(jEndUse,2)    !natural gas
    collapsedEndUse(jEndUse,3) = gatherEndUseBySourceBEPS(jEndUse,6)  & !Additional fuel  <- gasoline
                               + gatherEndUseBySourceBEPS(jEndUse,8)  & !                 <- diesel
                               + gatherEndUseBySourceBEPS(jEndUse,9)  & !                 <- coal
                               + gatherEndUseBySourceBEPS(jEndUse,10) & !                 <- fuel oil #1
                               + gatherEndUseBySourceBEPS(jEndUse,11) & !                 <- fuel oil #2
                               + gatherEndUseBySourceBEPS(jEndUse,12) & !                 <- propane
                               + gatherEndUseBySourceBEPS(jEndUse,13) & !                 <- otherfuel1
                               + gatherEndUseBySourceBEPS(jEndUse,14)   !                 <- otherfuel2
    collapsedEndUse(jEndUse,4) = gatherEndUseBySourceBEPS(jEndUse,3)    !district cooling <- purchased cooling
    collapsedEndUse(jEndUse,5) = gatherEndUseBySourceBEPS(jEndUse,4)  & !district heating <- purchased heating
                               + gatherEndUseBySourceBEPS(jEndUse,5)    !                 <- steam
    collapsedEndUse(jEndUse,6) = gatherEndUseBySourceBEPS(jEndUse,7)    !water
  END DO
  ! repeat with totals
  collapsedTotal(1) = gatherTotalsBySourceBEPS(1)    !electricity
  collapsedTotal(2) = gatherTotalsBySourceBEPS(2)    !natural gas
  collapsedTotal(3) = gatherTotalsBySourceBEPS(6)  & !Additional fuel  <- gasoline
                    + gatherTotalsBySourceBEPS(8)  & !                 <- diesel
                    + gatherTotalsBySourceBEPS(9)  & !                 <- coal
                    + gatherTotalsBySourceBEPS(10) & !                 <- fuel oil #1
                    + gatherTotalsBySourceBEPS(11) & !                 <- fuel oil #2
                    + gatherTotalsBySourceBEPS(12) & !                 <- propane
                    + gatherTotalsBySourceBEPS(13) & !                 <- otherfuel1
                    + gatherTotalsBySourceBEPS(14)   !                 <- otherfuel2
  collapsedTotal(4) = gatherTotalsBySourceBEPS(3)    !district cooling <- purchased cooling
  collapsedTotal(5) = gatherTotalsBySourceBEPS(4)  & !district heating <- purchased heating
                    + gatherTotalsBySourceBEPS(5)    !                 <- steam
  collapsedTotal(6) = gatherTotalsBySourceBEPS(7)    !water

  ! unit conversion - all values are used as divisors

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      largeConversionFactor = 3600000.d0
      areaConversionFactor = 1.0d0
    CASE (unitsStyleInchPound)
      largeConversionFactor =  getSpecificUnitDivider('J','kBtu')    !1054351.84 J to kBtu
      areaConversionFactor = getSpecificUnitDivider('m2','ft2')      !0.092893973 m2 to ft2
    CASE DEFAULT
      largeConversionFactor = 1000000.d0 ! to MJ
      areaConversionFactor = 1.0d0
  END SELECT

  ! convert floor areas
  convBldgCondFloorArea = buildingConditionedFloorArea / areaConversionFactor

  !convert units into MJ (divide by 1,000,000) if J otherwise kWh
  DO iResource= 1,5 !don't do water
    DO jEndUse=1,numEndUses
      collapsedEndUse(jEndUse,iResource) = collapsedEndUse(jEndUse,iResource) / largeConversionFactor
    END DO
    collapsedTotal(iResource) = collapsedTotal(iResource) / largeConversionFactor
  END DO

  ALLOCATE(rowHead(16))
  ALLOCATE(columnHead(5))
  ALLOCATE(columnWidth(5))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(16,5))
  DO iResource= 1,6
    useVal(1,iResource)  = collapsedEndUse(endUseHeating,iResource)
    useVal(2,iResource)  = collapsedEndUse(endUseCooling,iResource)
    useVal(3,iResource)  = collapsedEndUse(endUseInteriorLights,iResource)
    useVal(4,iResource)  = collapsedEndUse(endUseExteriorLights,iResource)
    useVal(5,iResource)  = collapsedEndUse(endUseInteriorEquipment,iResource)
    useVal(6,iResource)  = collapsedEndUse(endUseExteriorEquipment,iResource)
    useVal(7,iResource)  = collapsedEndUse(endUseFans,iResource)
    useVal(8,iResource)  = collapsedEndUse(endUsePumps,iResource)
    useVal(9,iResource)  = collapsedEndUse(endUseHeatRejection,iResource)
    useVal(10,iResource) = collapsedEndUse(endUseHumidification,iResource)
    useVal(11,iResource) = collapsedEndUse(endUseHeatRecovery,iResource)
    useVal(12,iResource) = collapsedEndUse(endUseWaterSystem,iResource)
    useVal(13,iResource) = collapsedEndUse(endUseRefrigeration,iResource)
    useVal(14,iResource) = collapsedEndUse(endUseCogeneration,iResource)

    useVal(15,iResource) = collapsedTotal(iResource)  ! totals
  END DO

  rowHead(1)  = 'Heating'
  rowHead(2)  = 'Cooling'
  rowHead(3)  = 'Interior Lighting'
  rowHead(4)  = 'Exterior Lighting'
  rowHead(5)  = 'Interior Equipment'
  rowHead(6)  = 'Exterior Equipment'
  rowHead(7)  = 'Fans'
  rowHead(8)  = 'Pumps'
  rowHead(9)  = 'Heat Rejection'
  rowHead(10) = 'Humidification'
  rowHead(11) = 'Heat Recovery'
  rowHead(12) = 'Water Systems'
  rowHead(13) = 'Refrigeration'
  rowHead(14) = 'Generators'
  rowHead(15) = ''
  rowHead(16) = 'Total Source Energy End Use Components'

  largeConversionFactor = 1.0d0

  SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Source Electricity [kWh]'
      columnHead(2) = 'Source Natural Gas [kWh]'
      columnHead(3) = 'Source Additional Fuel [kWh]'
      columnHead(4) = 'Source District Cooling [kWh]'
      columnHead(5) = 'Source District Heating [kWh]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Source Electricity [kBtu]'
      columnHead(2) = 'Source Natural Gas [kBtu]'
      columnHead(3) = 'Source Additional Fuel [kBtu]'
      columnHead(4) = 'Source District Cooling [kBtu]'
      columnHead(5) = 'Source District Heating [kBtu]'
    CASE DEFAULT
      columnHead(1) = 'Source Electricity [GJ]'
      columnHead(2) = 'Source Natural Gas [GJ]'
      columnHead(3) = 'Source Additional Fuel [GJ]'
      columnHead(4) = 'Source District Cooling [GJ]'
      columnHead(5) = 'Source District Heating [GJ]'
      largeConversionFactor = 1000.d0 ! for converting MJ to GJ
  END SELECT

  !
  !---- End Uses by Source Energy Sub-Table
  !

  tableBody = ''
  DO iResource= 1,5
      DO jEndUse=1,14
        tableBody(jEndUse,iResource) = TRIM(RealToStr(useVal(jEndUse,iResource) / largeConversionFactor,2))
      END DO
      tableBody(16,iResource) = TRIM(RealToStr(useVal(15,iResource) / largeConversionFactor,2))
  END DO

  ! heading for the entire sub-table
  CALL writeSubtitle('Source Energy End Use Components Summary')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                    'SourceEnergyEndUseComponentsSummary',&
                                    'Entire Facility',&
                                    'Source Energy End Use Components Summary')


  !
  !---- Normalized by Conditioned Area Sub-Table
  !

   SELECT CASE (unitsStyle)
    CASE (unitsStyleJtoKWH)
      columnHead(1) = 'Source Electricity [kWh/m2]'
      columnHead(2) = 'Source Natural Gas [kWh/m2]'
      columnHead(3) = 'Source Additional Fuel [kWh/m2]'
      columnHead(4) = 'Source District Cooling [kWh/m2]'
      columnHead(5) = 'Source District Heating [kWh/m2]'
    CASE (unitsStyleInchPound)
      columnHead(1) = 'Source Electricity [kBtu/ft2]'
      columnHead(2) = 'Source Natural Gas [kBtu/ft2]'
      columnHead(3) = 'Source Additional Fuel [kBtu/ft2]'
      columnHead(4) = 'Source District Cooling [kBtu/ft2]'
      columnHead(5) = 'Source District Heating [kBtu/ft2]'
    CASE DEFAULT
      columnHead(1) = 'Source Electricity [MJ/m2]'
      columnHead(2) = 'Source Natural Gas [MJ/m2]'
      columnHead(3) = 'Source Additional Fuel [MJ/m2]'
      columnHead(4) = 'Source District Cooling [MJ/m2]'
      columnHead(5) = 'Source District Heating [MJ/m2]'
  END SELECT

  tableBody = ''
  IF (convBldgCondFloorArea .GT. 0) THEN
    DO iResource= 1,5
      DO jEndUse=1,14
        tableBody(jEndUse,iResource) = TRIM(RealToStr(useVal(jEndUse,iResource) / convBldgCondFloorArea,2))
      END DO
      tableBody(16,iResource) = TRIM(RealToStr(useVal(15,iResource) / convBldgCondFloorArea,2))
    END DO
  END IF

  CALL writeTextLine('Normalized Metrics',.TRUE.)

  ! heading for the entire sub-table
  CALL writeSubtitle('Source Energy End Use Components Per Conditioned Floor Area')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                    'SourceEnergyEndUseComponentsSummary',&
                                    'Entire Facility',&
                                    'Source Energy End Use Component Per Conditioned Floor Area')

  !
  !---- Normalized by Total Area Sub-Table
  !
  tableBody = ''
  IF (convBldgCondFloorArea .GT. 0) THEN
    DO iResource= 1,5
      DO jEndUse=1,14
        tableBody(jEndUse,iResource) = TRIM(RealToStr(useVal(jEndUse,iResource) / convBldgCondFloorArea,2))
      END DO
      tableBody(16,iResource) = TRIM(RealToStr(useVal(15,iResource) / convBldgCondFloorArea,2))
    END DO
  END IF

  ! heading for the entire sub-table
  CALL writeSubtitle('Source Energy End Use Components Per Total Floor Area')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                    'SourceEnergyEndUseComponentsSummary',&
                                    'Entire Facility',&
                                    'Source Energy End Use Components Per Total Floor Area')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)


END IF
END SUBROUTINE WriteSourceEnergyEndUseSummary

SUBROUTINE WriteDemandEndUseSummary
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   January 2009
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Take the gathered total and enduse meter data and structure
          !   the results into a tabular report for output.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   This report actually consists of many sub-tables each with
          !   its own call to writeTable.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE OutputProcessor, ONLY: MaxNumSubcategories, EndUseCategory
USE DataWater       , ONlY: WaterStorage
USE ManageElectricPower , ONLY: ElecStorage, NumElecStorageDevices
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, parameter  ::  colElectricity        = 1
INTEGER, parameter  ::  colGas                = 2
INTEGER, parameter  ::  colAdditionalFuel          = 3
INTEGER, parameter  ::  colPurchCool          = 4
INTEGER, parameter  ::  colPurchHeat          = 5
INTEGER, parameter  ::  colWater              = 6

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody

! all arrays are in the format: (row, columnm)
REAL(r64),DIMENSION(15,6)                           :: useVal
REAL(r64),DIMENSION(6)                              :: collapsedTotal
REAL(r64),DIMENSION(numEndUses,6)                   :: collapsedEndUse
INTEGER, DIMENSION(6)                               :: collapsedTimeStep
REAL(r64),DIMENSION(6,numEndUses,MaxNumSubcategories) :: collapsedEndUseSub
INTEGER                                        :: iResource
INTEGER                                        :: jEndUse
INTEGER                                        :: kEndUseSub
INTEGER                                        :: i
INTEGER                                        :: numRows
CHARACTER(len=100)                             :: footnote = ''
REAL(r64)                                      :: additionalFuelMax
INTEGER                                        :: additionalFuelSelected
INTEGER                                        :: additionalFuelNonZeroCount
INTEGER                                        :: distrHeatSelected
LOGICAL                                        :: bothDistrHeatNonZero
REAL(r64)                                      :: powerConversion
REAL(r64)                                      :: flowConversion

REAL(r64),DIMENSION(6)                         :: leedFansParkFromFan
REAL(r64),DIMENSION(6)                         :: leedFansParkFromExtFuelEquip
REAL(r64),DIMENSION(6)                         :: leedIntLightProc
REAL(r64),DIMENSION(6)                         :: leedCook
REAL(r64),DIMENSION(6)                         :: leedIndProc
REAL(r64),DIMENSION(6)                         :: leedElevEsc
REAL(r64)                                      :: unconvert
CHARACTER(len=MaxNameLength)                   :: subCatName

IF (displayDemandEndUse) THEN
  ! show the headers of the report
  CALL WriteReportHeaders('Demand End Use Components Summary','Entire Facility',isAverage)
  ! totals - select which additional fuel to display and which other district heating
  collapsedTotal=0.0d0
  collapsedTotal(1) = gatherDemandTotal(1)    !electricity
  collapsedTimeStep(1) = gatherDemandTimeStamp(1)
  collapsedTotal(2) = gatherDemandTotal(2)    !natural gas
  collapsedTimeStep(2) = gatherDemandTimeStamp(2)
  collapsedTotal(4) = gatherDemandTotal(3)    !district cooling <- purchased cooling
  collapsedTimeStep(4) = gatherDemandTimeStamp(3)
  collapsedTotal(6) = gatherDemandTotal(7)    !water
  collapsedTimeStep(6) = gatherDemandTimeStamp(7)
  ! select which of the additional fuels should be displayed based on which has the highest
  ! demand. This is usually likely to be the only additional fuel that is actually being used.
  ! If an additional fuel is non-zero, a footnote to the table is added.
  ! First step is to see if any additional fuels are non-zero
  additionalFuelNonZeroCount = 0
  IF (gatherDemandTotal(6) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(8) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(9) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(10) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(11) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(12) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(13) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (gatherDemandTotal(14) .GT. 0.0d0) additionalFuelNonZeroCount = additionalFuelNonZeroCount + 1
  IF (additionalFuelNonZeroCount .GT. 1) THEN
    footnote = 'Additional fuels have non-zero demand but are not shown on this report.'
  END IF
  !assuming that at least one of these is non-zero
  additionalFuelSelected = 12 !default is propane if no other given
  additionalFuelMax = gatherDemandTotal(12)
  IF (additionalFuelNonZeroCount .GT. 0) THEN
    IF (gatherDemandTotal(6) .GT. additionalFuelMax) THEN ! gasoline
      additionalFuelSelected = 6
      additionalFuelMax = gatherDemandTotal(6)
    END IF
    IF (gatherDemandTotal(8) .GT. additionalFuelMax) THEN ! diesel
      additionalFuelSelected = 8
      additionalFuelMax = gatherDemandTotal(8)
    END IF
    IF (gatherDemandTotal(9) .GT. additionalFuelMax) THEN ! coal
      additionalFuelSelected = 9
      additionalFuelMax = gatherDemandTotal(9)
    END IF
    IF (gatherDemandTotal(10) .GT. additionalFuelMax) THEN ! fuel oil #1
      additionalFuelSelected = 10
      additionalFuelMax = gatherDemandTotal(10)
    END IF
    IF (gatherDemandTotal(11) .GT. additionalFuelMax) THEN ! fuel oil #2
      additionalFuelSelected = 11
      additionalFuelMax = gatherDemandTotal(11)
    END IF
    IF (gatherDemandTotal(12) .GT. additionalFuelMax) THEN ! propane
      additionalFuelSelected = 12
      additionalFuelMax = gatherDemandTotal(12)
    END IF
    IF (gatherDemandTotal(13) .GT. additionalFuelMax) THEN ! otherfuel1
      additionalFuelSelected = 13
      additionalFuelMax = gatherDemandTotal(13)
    END IF
    IF (gatherDemandTotal(14) .GT. additionalFuelMax) THEN ! otherfuel2
      additionalFuelSelected = 14
      additionalFuelMax = gatherDemandTotal(14)
    END IF
  END IF
  !set the time of peak demand and total demand for the additinoal fuel selected
  collapsedTimeStep(3) = gatherDemandTimeStamp(additionalFuelSelected)
  collapsedTotal(3) = gatherDemandTotal(additionalFuelSelected)
  !set flag if both puchased heating and steam both have positive demand
  bothDistrHeatNonZero = (gatherDemandTotal(4) .GT. 0.0d0) .AND. (gatherDemandTotal(5) .GT. 0.0d0)
  !select the district heating source that has a larger demand
  IF (gatherDemandTotal(4) .GT. gatherDemandTotal(5)) THEN
    distrHeatSelected = 4 ! purchased heating
    IF (bothDistrHeatNonZero) THEN
      footnote = TRIM(footnote) // ' Steam has non-zero demand but is not shown on this report.'
    END IF
  ELSE
    distrHeatSelected = 5 ! steam
    IF (bothDistrHeatNonZero) THEN
      footnote = TRIM(footnote) // ' District heating has non-zero demand but is not shown on this report.'
    END IF
  END IF
  !set the time of peak demand and total demand for the purchased heating/steam
  collapsedTimeStep(5) = gatherDemandTimeStamp(distrHeatSelected)
  collapsedTotal(5) = gatherDemandTotal(distrHeatSelected)

  !establish unit conversion factors
  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    powerConversion = getSpecificUnitMultiplier('W','kBtuh')
    flowConversion = getSpecificUnitMultiplier('m3/s','gal/min')
  ELSE
    powerConversion = 1.0d0
    flowConversion = 1.0d0
  END IF

  ! collapse the gatherEndUseBEPS array to the resource groups displayed
  collapsedEndUse=0.0d0
  DO jEndUse=1,numEndUses
    collapsedEndUse(jEndUse,1) = gatherDemandEndUse(jEndUse,1) * powerConversion    !electricity
    collapsedEndUse(jEndUse,2) = gatherDemandEndUse(jEndUse,2) * powerConversion    !natural gas
    collapsedEndUse(jEndUse,3) = gatherDemandEndUse(jEndUse,additionalFuelSelected) * powerConversion  !additional fuel
    collapsedEndUse(jEndUse,4) = gatherDemandEndUse(jEndUse,3) * powerConversion    ! purchased cooling
    collapsedEndUse(jEndUse,5) = gatherDemandEndUse(jEndUse,distrHeatSelected) * powerConversion !district heating
    collapsedEndUse(jEndUse,6) = gatherDemandEndUse(jEndUse,7) * flowConversion    !water
  END DO
  DO jEndUse=1,numEndUses
    DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
      collapsedEndUseSub(1,jEndUse,kEndUseSub) =   &
         gatherDemandEndUseSub(1,jEndUse,kEndUseSub) * powerConversion    !electricity
      collapsedEndUseSub(2,jEndUse,kEndUseSub) =   &
         gatherDemandEndUseSub(2,jEndUse,kEndUseSub) * powerConversion    !natural gas
      collapsedEndUseSub(3,jEndUse,kEndUseSub) =   &
         gatherDemandEndUseSub(additionalFuelSelected,jEndUse,kEndUseSub) * powerConversion !additional fuel
      collapsedEndUseSub(4,jEndUse,kEndUseSub) =   &
         gatherDemandEndUseSub(3,jEndUse,kEndUseSub) * powerConversion    !purch cooling
      collapsedEndUseSub(5,jEndUse,kEndUseSub) =   &
         gatherDemandEndUseSub(distrHeatSelected,jEndUse,kEndUseSub) * powerConversion    !district heating
      collapsedEndUseSub(6,jEndUse,kEndUseSub) =   &
         gatherDemandEndUseSub(7,jEndUse,kEndUseSub) * flowConversion    !water
    END DO
  END DO
  !convert totals
  collapsedTotal(1) = collapsedTotal(1) * powerConversion !electricity
  collapsedTotal(2) = collapsedTotal(2) * powerConversion !natural gas
  collapsedTotal(3) = collapsedTotal(3) * powerConversion !additional fuel
  collapsedTotal(4) = collapsedTotal(4) * powerConversion !purchased cooling
  collapsedTotal(5) = collapsedTotal(5) * powerConversion !district heating
  collapsedTotal(6) = collapsedTotal(6) * flowConversion  !water
  !
  !---- End Use Sub-Table
  !
  ALLOCATE(rowHead(17))
  ALLOCATE(columnHead(6))
  ALLOCATE(columnWidth(6))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(17,6))
  DO iResource= 1,6
    useVal(1,iResource)  = collapsedEndUse(endUseHeating,iResource)
    useVal(2,iResource)  = collapsedEndUse(endUseCooling,iResource)
    useVal(3,iResource)  = collapsedEndUse(endUseInteriorLights,iResource)
    useVal(4,iResource)  = collapsedEndUse(endUseExteriorLights,iResource)
    useVal(5,iResource)  = collapsedEndUse(endUseInteriorEquipment,iResource)
    useVal(6,iResource)  = collapsedEndUse(endUseExteriorEquipment,iResource)
    useVal(7,iResource)  = collapsedEndUse(endUseFans,iResource)
    useVal(8,iResource)  = collapsedEndUse(endUsePumps,iResource)
    useVal(9,iResource)  = collapsedEndUse(endUseHeatRejection,iResource)
    useVal(10,iResource) = collapsedEndUse(endUseHumidification,iResource)
    useVal(11,iResource) = collapsedEndUse(endUseHeatRecovery,iResource)
    useVal(12,iResource) = collapsedEndUse(endUseWaterSystem,iResource)
    useVal(13,iResource) = collapsedEndUse(endUseRefrigeration,iResource)
    useVal(14,iResource) = collapsedEndUse(endUseCogeneration,iResource)
    useVal(15,iResource) = collapsedTotal(iResource)  ! totals
  END DO

  rowHead(1)  = 'Time of Peak'
  rowHead(2)  = 'Heating'
  rowHead(3)  = 'Cooling'
  rowHead(4)  = 'Interior Lighting'
  rowHead(5)  = 'Exterior Lighting'
  rowHead(6)  = 'Interior Equipment'
  rowHead(7)  = 'Exterior Equipment'
  rowHead(8)  = 'Fans'
  rowHead(9)  = 'Pumps'
  rowHead(10) = 'Heat Rejection'
  rowHead(11) = 'Humidification'
  rowHead(12) = 'Heat Recovery'
  rowHead(13) = 'Water Systems'
  rowHead(14) = 'Refrigeration'
  rowHead(15) = 'Generators'
  rowHead(16) = ''
  rowHead(17) = 'Total End Uses'

  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    columnHead(1) = 'Electricity [kBtuh]'
    columnHead(2) = 'Natural Gas [kBtuh]'
    SELECT CASE (additionalFuelSelected)
      CASE(6) ! gasoline
        columnHead(3) = 'Gasoline [kBtuh]'
      CASE(8) ! Diesel
        columnHead(3) = 'Diesel [kBtuh]'
      CASE(9) ! Coal
        columnHead(3) = 'Coal [kBtuh]'
      CASE(10) ! Fuel Oil #1
        columnHead(3) = 'Fuel Oil #1 [kBtuh]'
      CASE(11) ! Fuel Oil #2
        columnHead(3) = 'Fuel Oil #2 [kBtuh]'
      CASE(12) ! Propane
        columnHead(3) = 'Propane [kBtuh]'
      CASE(13) ! OtherFuel1
        columnHead(3) = 'Other Fuel 1 [kBtuh]'
      CASE(14) ! OtherFuel2
        columnHead(3) = 'Other Fuel 2 [kBtuh]'
    END SELECT
    columnHead(4) = 'District Cooling [kBtuh]'
    SELECT CASE (distrHeatSelected)
      CASE (4)
        columnHead(5) = 'District Heating [kBtuh]'
      CASE (5)
        columnHead(5) = 'Steam [kBtuh]'
    END SELECT
    columnHead(6) = 'Water [gal/min]'
  ELSE
    columnHead(1) = 'Electricity [W]'
    columnHead(2) = 'Natural Gas [W]'
    SELECT CASE (additionalFuelSelected)
      CASE(6) ! gasoline
        columnHead(3) = 'Gasoline [W]'
      CASE(8) ! Diesel
        columnHead(3) = 'Diesel [W]'
      CASE(9) ! Coal
        columnHead(3) = 'Coal [W]'
      CASE(10) ! Fuel Oil #1
        columnHead(3) = 'Fuel Oil #1 [W]'
      CASE(11) ! Fuel Oil #2
        columnHead(3) = 'Fuel Oil #2 [W]'
      CASE(12) ! Propane
        columnHead(3) = 'Propane [W]'
      CASE(13) ! OtherFuel1
        columnHead(3) = 'Other Fuel 1 [W]'
      CASE(14) ! OtherFuel2
        columnHead(3) = 'Other Fuel 2 [W]'
    END SELECT
    columnHead(4) = 'District Cooling [W]'
    SELECT CASE (distrHeatSelected)
      CASE (4)
        columnHead(5) = 'District Heating [W]'
      CASE (5)
        columnHead(5) = 'Steam [W]'
    END SELECT
    columnHead(6) = 'Water [m3/s]'
  END IF

  tableBody = ''
  DO iResource= 1,6
    DO jEndUse=1,14
      tableBody(1 + jEndUse,iResource) = TRIM(RealToStr(useVal(jEndUse,iResource),2))
    END DO
    tableBody(1,iResource) = TRIM(DateToString(collapsedTimeStep(iResource)))
    tableBody(17,iResource) = TRIM(RealToStr(collapsedTotal(iResource),2))
  END DO

    !complete the LEED end use table using the same values
  ! for certain rows in the LEED table the subcategories are necessary so first compute those values
  leedFansParkFromFan = 0.0d0
  leedFansParkFromExtFuelEquip = 0.0d0
  leedIntLightProc = 0.0d0
  leedCook = 0.0d0
  leedIndProc = 0.0d0
  leedElevEsc = 0.0d0
  DO iResource = 1, 5    ! don't bother with water
    DO jEndUse = 1, NumEndUses
      IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          subCatName = EndUseCategory(jEndUse)%SubcategoryName(kEndUseSub)
          IF (SameString(subCatName,'Fans - Parking Garage')) THEN
            IF (jEndUse .EQ. 7) THEN  !fans
              leedFansParkFromFan(iResource) = leedFansParkFromFan(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
            ELSE
              leedFansParkFromExtFuelEquip(iResource) = leedFansParkFromExtFuelEquip(iResource) &
                  + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
            END IF
          ELSEIF (SameString(subCatName,'Interior Lighting - Process')) THEN
            leedIntLightProc(iResource) = leedIntLightProc(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          ELSEIF (SameString(subCatName,'Cooking')) THEN
            leedCook(iResource) = leedCook(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          ELSEIF (SameString(subCatName,'Industrial Process')) THEN
            leedIndProc(iResource) = leedIndProc(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          ELSEIF (SameString(subCatName,'Elevators and Escalators')) THEN
            leedElevEsc(iResource) = leedElevEsc(iResource) + collapsedEndUseSub(iResource,jEndUse,kEndUseSub)
          END IF
        END DO
      END IF
    END DO
  END DO

    !complete the LEED end use table using the same values
  unconvert = 1/powerConversion
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Interior Lighting',unconvert * (useVal(3,colElectricity) &
                - leedIntLightProc(colElectricity)),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Exterior Lighting',unconvert * useVal(4,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Space Heating',unconvert * useVal(1,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Space Cooling',unconvert * useVal(2,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Pumps',unconvert * useVal(8,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Heat Rejection',unconvert * useVal(9,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Fans-Interior',unconvert * (useVal(7,colElectricity) &
                - leedFansParkFromFan(colElectricity)),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Fans-Parking Garage',unconvert * (leedFansParkFromFan(colElectricity) &
                + leedFansParkFromExtFuelEquip(colElectricity)),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Service Water Heating',unconvert * useVal(12,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Receptacle Equipment',unconvert * useVal(5,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Interior Lighting (process)',unconvert * leedIntLightProc(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Refrigeration Equipment',unconvert * useVal(13,colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Cooking',unconvert * leedCook(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Industrial Process',unconvert * leedIndProc(colElectricity),2)
  CALL PreDefTableEntry(pdchLeedPerfElDem,'Elevators and Escalators',unconvert * leedElevEsc(colElectricity),2)
 !CALL PreDefTableEntry(pdchLeedPerfElDem,'Total',useVal(15,colElectricity),2)

  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Interior Lighting',unconvert * (useVal(3,colGas) - leedIntLightProc(colGas)),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Exterior Lighting',unconvert * useVal(4,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Space Heating',unconvert * useVal(1,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Space Cooling',unconvert * useVal(2,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Pumps',unconvert * useVal(8,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Heat Rejection',unconvert * useVal(9,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Fans-Interior',unconvert * (useVal(7,colGas)- leedFansParkFromFan(colGas)),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Fans-Parking Garage',unconvert * (leedFansParkFromFan(colGas) &
               + leedFansParkFromExtFuelEquip(colGas)),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Service Water Heating',unconvert * useVal(12,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Receptacle Equipment',unconvert * useVal(5,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Interior Lighting (process)',unconvert * leedIntLightProc(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Refrigeration Equipment',unconvert * useVal(13,colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Cooking',unconvert * leedCook(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Industrial Process',unconvert * leedIndProc(colGas),2)
  CALL PreDefTableEntry(pdchLeedPerfGasDem,'Elevators and Escalators',unconvert * leedElevEsc(colGas),2)
  !CALL PreDefTableEntry(pdchLeedPerfGasDem,'Total',useVal(15,colGas),2)

  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Interior Lighting',unconvert * (useVal(3,colAdditionalFuel) + useVal(3,colPurchCool)  &
                                 + useVal(3,colPurchHeat) - (leedIntLightProc(colAdditionalFuel) + leedIntLightProc(colPurchCool) &
                                 + leedIntLightProc(colPurchHeat))) ,2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Exterior Lighting',unconvert * (useVal(4,colAdditionalFuel) + useVal(4,colPurchCool) &
                                 + useVal(4,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Space Heating',unconvert * (useVal(1,colAdditionalFuel) + useVal(1,colPurchCool) &
                                 + useVal(1,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Space Cooling',unconvert * (useVal(2,colAdditionalFuel) + useVal(2,colPurchCool) &
                                 + useVal(2,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Pumps',unconvert * (useVal(8,colAdditionalFuel) + useVal(8,colPurchCool) &
                                 + useVal(8,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Heat Rejection',unconvert * (useVal(9,colAdditionalFuel) + useVal(9,colPurchCool) &
                                 + useVal(9,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Fans-Interior',unconvert * (useVal(7,colAdditionalFuel) +  useVal(7,colPurchCool) &
                             + useVal(7,colPurchHeat) - (leedFansParkFromFan(colAdditionalFuel)   &
                             + leedFansParkFromFan(colPurchCool) &
                             + leedFansParkFromFan(colPurchHeat))),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Fans-Parking Garage',unconvert * (leedFansParkFromFan(colAdditionalFuel) &
                             + leedFansParkFromFan(colPurchCool) + leedFansParkFromFan(colPurchHeat) &
                             + leedFansParkFromExtFuelEquip(colAdditionalFuel) + leedFansParkFromExtFuelEquip(colPurchCool) &
                             + leedFansParkFromExtFuelEquip(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Service Water Heating',unconvert * (useVal(12,colAdditionalFuel) &
                             + useVal(12,colPurchCool) + useVal(12,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Receptacle Equipment',unconvert *   &
                           (useVal(5,colAdditionalFuel) + useVal(5,colPurchCool) &
                             + useVal(5,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Interior Lighting (process)',unconvert * (leedIntLightProc(colAdditionalFuel) &
                             + leedIntLightProc(colPurchCool) + leedIntLightProc(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Refrigeration Equipment',unconvert * (useVal(13,colAdditionalFuel) &
                             + useVal(13,colPurchCool) + useVal(13,colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Cooking',unconvert * (leedCook(colAdditionalFuel) + leedCook(colPurchCool) &
                             + leedCook(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Industrial Process',unconvert * (leedIndProc(colAdditionalFuel) &
                             + leedIndProc(colPurchCool) + leedIndProc(colPurchHeat)),2)
  CALL PreDefTableEntry(pdchLeedPerfOthDem,'Elevators and Escalators',unconvert * (leedElevEsc(colAdditionalFuel) &
                             + leedElevEsc(colPurchCool) + leedElevEsc(colPurchHeat)),2)
  !CALL PreDefTableEntry(pdchLeedPerfOthDem,'Total',useVal(15,colAdditionalFuel) + useVal(15,colPurchCool) + useVal(15,colPurchHeat),2)


  CALL writeSubtitle('End Uses')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.false.,footnote)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'DemandEndUseComponentsSummary',&
                                      'Entire Facility',&
                                      'End Uses')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- End Uses By Subcategory Sub-Table
  !
  numRows = 0
  DO jEndUse = 1, NumEndUses
    IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
      DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
        numRows = numRows + 1
      END DO
    ELSE
      numRows = numRows + 1
    END IF
  END DO

  ALLOCATE(rowHead(numRows))
  ALLOCATE(columnHead(7))
  ALLOCATE(columnWidth(7))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(numRows,7))

  rowHead = ''
  tableBody = ''

  ! Build row head and subcategories columns
  i = 1
  DO jEndUse = 1, NumEndUses
    rowHead(i) = EndUseCategory(jEndUse)%DisplayName
    IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
      DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
        tableBody(i,1) = EndUseCategory(jEndUse)%SubcategoryName(kEndUseSub)
        i = i + 1
      END DO
    ELSE
      tableBody(i,1) = 'General'
      i = i + 1
    END IF
  END DO

  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    columnHead(1) = 'Subcategory'
    columnHead(2) = 'Electricity [kBtuh]'
    columnHead(3) = 'Natural Gas [kBtuh]'
    SELECT CASE (additionalFuelSelected)
      CASE(6) ! gasoline
        columnHead(4) = 'Gasoline [kBtuh]'
      CASE(8) ! Diesel
        columnHead(4) = 'Diesel [kBtuh]'
      CASE(9) ! Coal
        columnHead(4) = 'Coal [kBtuh]'
      CASE(10) ! Fuel Oil #1
        columnHead(4) = 'Fuel Oil #1 [kBtuh]'
      CASE(11) ! Fuel Oil #2
        columnHead(4) = 'Fuel Oil #2 [kBtuh]'
      CASE(12) ! Propane
        columnHead(4) = 'Propane [kBtuh]'
      CASE(13) ! OtherFuel1
        columnHead(4) = 'Other Fuel 1 [kBtuh]'
      CASE(14) ! OtherFuel2
        columnHead(4) = 'Other Fuel 2 [kBtuh]'
    END SELECT
    columnHead(5) = 'District Cooling [kBtuh]'
    SELECT CASE (distrHeatSelected)
      CASE (4)
        columnHead(6) = 'District Heating [kBtuh]'
      CASE (5)
        columnHead(6) = 'Steam [kBtuh]'
    END SELECT
    columnHead(7) = 'Water [gal/min]'
  ELSE
    columnHead(1) = 'Subcategory'
    columnHead(2) = 'Electricity [W]'
    columnHead(3) = 'Natural Gas [W]'
    SELECT CASE (additionalFuelSelected)
      CASE(6) ! gasoline
        columnHead(4) = 'Gasoline [W]'
      CASE(8) ! Diesel
        columnHead(4) = 'Diesel [W]'
      CASE(9) ! Coal
        columnHead(4) = 'Coal [W]'
      CASE(10) ! Fuel Oil #1
        columnHead(4) = 'Fuel Oil #1 [W]'
      CASE(11) ! Fuel Oil #2
        columnHead(4) = 'Fuel Oil #2 [W]'
      CASE(12) ! Propane
        columnHead(4) = 'Propane [W]'
      CASE(13) ! OtherFuel1
        columnHead(4) = 'Other Fuel 1 [W]'
      CASE(14) ! OtherFuel2
        columnHead(4) = 'Other Fuel 2 [W]'
    END SELECT
    columnHead(5) = 'District Cooling [W]'
    SELECT CASE (distrHeatSelected)
      CASE (4)
        columnHead(6) = 'District Heating [W]'
      CASE (5)
        columnHead(6) = 'Steam [W]'
    END SELECT
    columnHead(7) = 'Water [m3/s]'
  END IF

  DO iResource = 1, 6
    i = 1
    DO jEndUse = 1, NumEndUses
      IF (EndUseCategory(jEndUse)%NumSubcategories > 0) THEN
        DO kEndUseSub = 1, EndUseCategory(jEndUse)%NumSubcategories
          tableBody(i,iResource+1) = TRIM(RealToStr(collapsedEndUseSub(iResource,jEndUse,kEndUseSub),2))
          i = i + 1
        END DO
      ELSE
        tableBody(i,iResource+1) = TRIM(RealToStr(collapsedEndUse(jEndUse,iResource),2))
        i = i + 1
      END IF
    END DO
  END DO

  ! heading for the entire sub-table
  CALL writeSubtitle('End Uses By Subcategory')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.false.,footnote)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'DemandEndUseComponentsSummary',&
                                      'Entire Facility',&
                                      'End Uses By Subcategory')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
END IF
END SUBROUTINE WriteDemandEndUseSummary


SUBROUTINE WriteCompCostTable

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   April/May 2004
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! produce a results table from Cost Estimate Calculations

          ! METHODOLOGY EMPLOYED:
          ! USE data from CostEstimateManager, call JGlazer's subroutines

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataCostEstimate
  USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

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
REAL(r64), DIMENSION(10,3):: TableBodyData
REAL(r64)  :: RefBldgConstCost ! holds interim value for construction component costs: reference bldg.
REAL(r64)  :: CurntBldgConstCost ! holds interim value for construction component costs: current bldg.
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody
INTEGER    :: Item  ! do-loop counter for line items
INTEGER    :: NumRows ! number of rows in report table excluding table header
INTEGER    :: NumCols ! number of columns in report table
CHARACTER(len=MaxNameLength) :: SIunit = ''
CHARACTER(len=MaxNameLength) :: m2_unitName = ''
REAL(r64) :: m2_unitConv = 0.0d0
INTEGER :: unitConvIndex = 0
CHARACTER(len=MaxNameLength) :: IPunitName = ''
REAL(r64) :: IPqty
REAL(r64) :: IPsingleValue
REAL(r64) :: IPvaluePer

  If (.not. DoCostEstimate)  RETURN

  CALL WriteReportHeaders('Component Cost Economics Summary','Entire Facility',isAverage)

  ! compute floor area if no ABUPS
  IF (buildingConditionedFloorArea == 0.0d0) THEN
    CALL DetermineBuildingFloorArea
  ENDIF

  ! 1st sub-table with total Costs and normalized with area
  ALLOCATE(rowHead(10))
  ALLOCATE(columnHead(3))
  ALLOCATE(columnWidth(3))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(10,3))

  columnHead(1) = 'Reference Bldg.'
  columnHead(2) = 'Current Bldg. Model'
  columnHead(3) = 'Difference'

  rowHead(1)    = 'Line Item SubTotal (~~$~~)'
  rowHead(2)    = 'Misc. Costs (~~$~~)'
  rowHead(3)    = 'Regional Adjustment (~~$~~)'
  rowHead(4)    = 'Design Fee (~~$~~)'
  rowHead(5)    = 'Contractor Fee (~~$~~)'
  rowHead(6)    = 'Contingency (~~$~~)'
  rowHead(7)    = 'Permits, Bonds, Insurance (~~$~~)'
  rowHead(8)    = 'Commissioning (~~$~~)'
  rowHead(9)    = 'Cost Estimate Total (~~$~~)'
  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    SIunit = '[m2]'
    CALL LookupSItoIP(SIunit, unitConvIndex, m2_unitName)
    m2_unitConv = convertIP(unitConvIndex,1.0d0)
    rowHead(10)   = 'Cost Per Conditioned Building Area (~~$~~/ft2)'
  ELSE
    rowHead(10)   = 'Cost Per Conditioned Building Area (~~$~~/m2)'
    m2_unitConv = 1.0d0
  END IF
  TableBodyData = 0.0d0
  TableBody     = ''

  TableBodyData(1,1) = RefrncBldg%LineItemTot
  TableBody(1,1)     = TRIM(RealToStr(TableBodyData(1,1),2))
  TableBodyData(2,1) = RefrncBldg%MiscCostperSqMeter * buildingConditionedFloorArea
  TableBody(2,1)     = TRIM(RealToStr(TableBodyData(2,1),2))

  IF (RefrncBldg%RegionalModifier /= 1.0d0) THEN
    TableBodyData(3,1) = (RefrncBldg%LineItemTot + RefrncBldg%MiscCostperSqMeter * buildingConditionedFloorArea) &
                         *(RefrncBldg%RegionalModifier - 1.0d0)
  ELSE
    TableBodyData(3,1) = 0.0d0
  ENDIF

  RefBldgConstCost   = SUM(TableBodyData(1:3,1))

  TableBody(3,1)     = TRIM(RealToStr(TableBodyData(3,1),2))
  TableBodyData(4,1) = RefBldgConstCost * RefrncBldg%DesignFeeFrac
  TableBody(4,1)     = TRIM(RealToStr(TableBodyData(4,1),2))
  TableBodyData(5,1) = RefBldgConstCost * RefrncBldg%ContractorFeeFrac
  TableBody(5,1)     = TRIM(RealToStr(TableBodyData(5,1),2))
  TableBodyData(6,1) = RefBldgConstCost * RefrncBldg%ContingencyFrac
  TableBody(6,1)     = TRIM(RealToStr(TableBodyData(6,1),2))
  TableBodyData(7,1) = RefBldgConstCost * RefrncBldg%BondCostFrac
  TableBody(7,1)     = TRIM(RealToStr(TableBodyData(7,1),2))
  TableBodyData(8,1) = RefBldgConstCost * RefrncBldg%CommissioningFrac
  TableBody(8,1)     = TRIM(RealToStr(TableBodyData(8,1),2))
  RefrncBldg%GrandTotal = SUM(TableBodyData(1:8,1))
  TableBodyData(9,1) = RefrncBldg%GrandTotal
  TableBody(9,1)     = TRIM(RealToStr(TableBodyData(9,1),2))
  IF (buildingConditionedFloorArea .GT. 0.0d0) THEN
    TableBodyData(10,1) = TableBodyData(9,1) / (buildingConditionedFloorArea * m2_unitConv)
  endif
  TableBody(10,1)     = TRIM(RealToStr(TableBodyData(10,1),2))

  TableBodyData(1,2) = CurntBldg%LineItemTot
  TableBody(1,2)     = trim(RealToStr(TableBodyData(1,2),2))
  TableBodyData(2,2) = CurntBldg%MiscCostperSqMeter * buildingConditionedFloorArea
  TableBody(2,2)     = trim(RealToStr(TableBodyData(2,2),2))
  IF (CurntBldg%RegionalModifier /= 1.0d0) THEN
    TableBodyData(3,2) = (CurntBldg%LineItemTot + CurntBldg%MiscCostperSqMeter * buildingConditionedFloorArea) &
                         *(CurntBldg%RegionalModifier - 1.0d0)
  ELSE
    TableBodyData(3,2) = 0.0d0
  ENDIF
  TableBody(3,2)     = trim(RealToStr(TableBodyData(3,2),2))

  CurntBldgConstCost = SUM(TableBodyData(1:3,2))

  TableBodyData(4,2) = CurntBldgConstCost * CurntBldg%DesignFeeFrac
  TableBody(4,2)     = TRIM(RealToStr(TableBodyData(4,2),2))

  TableBodyData(5,2) = CurntBldgConstCost * CurntBldg%ContractorFeeFrac
  TableBody(5,2)     = trim(RealToStr(TableBodyData(5,2),2))
  TableBodyData(6,2) = CurntBldgConstCost * CurntBldg%ContingencyFrac
  TableBody(6,2)     = trim(RealToStr(TableBodyData(6,2),2))
  TableBodyData(7,2) = CurntBldgConstCost * CurntBldg%BondCostFrac
  TableBody(7,2)     = trim(RealToStr(TableBodyData(7,2),2))
  TableBodyData(8,2) = CurntBldgConstCost * CurntBldg%CommissioningFrac
  TableBody(8,2)     = TRIM(RealToStr(TableBodyData(8,2),2))

  CurntBldg%GrandTotal = sum(TableBodyData(1:8,2))
  TableBodyData(9,2) = CurntBldg%GrandTotal
  TableBody(9,2)     = trim(RealToStr(TableBodyData(9,2),2))
  IF (buildingConditionedFloorArea .GT. 0) THEN
    TableBodyData(10,2) = TableBodyData(9,2) / (buildingConditionedFloorArea * m2_unitConv)
  endif
  TableBody(10,2)     = trim(RealToStr(TableBodyData(10,2),2))

  TableBodyData(1:10,3) = TableBodyData(1:10,2) - TableBodyData(1:10,1)
  TableBody(1,3)     = trim(RealToStr(TableBodyData(1,3),2))
  TableBody(2,3)     = trim(RealToStr(TableBodyData(2,3),2))
  TableBody(3,3)     = trim(RealToStr(TableBodyData(3,3),2))
  TableBody(4,3)     = trim(RealToStr(TableBodyData(4,3),2))
  TableBody(5,3)     = trim(RealToStr(TableBodyData(5,3),2))
  TableBody(6,3)     = trim(RealToStr(TableBodyData(6,3),2))
  TableBody(7,3)     = trim(RealToStr(TableBodyData(7,3),2))
  TableBody(8,3)     = trim(RealToStr(TableBodyData(8,3),2))
  TableBody(9,3)     = trim(RealToStr(TableBodyData(9,3),2))
  TableBody(10,3)     = trim(RealToStr(TableBodyData(10,3),2))

  CALL writeSubtitle('Construction Cost Estimate Summary' )
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Construction Cost Estimate Summary',&
                                      'Entire Facility',&
                                      'Construction Cost Estimate Summary')

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  NumRows = NumLineItems +1  !body will have the total and line items
  NumCols = 6  ! Line no., Line name, Qty, Units, ValperQty, Subtotal
  ALLOCATE(rowHead(NumRows))
  ALLOCATE(columnHead(NumCols))
  ALLOCATE(columnWidth(NumCols))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(NumRows,NumCols))
  tableBody = '--' ! array init
  rowHead = '--'   ! array init
  rowHead(NumRows) = 'Line Item SubTotal'  !last line in table will be a total
  ! setup up column headers
  columnHead(1) = 'Line No.'
  columnHead(2) = 'Item Name'
  columnHead(3) = 'Quantity.'
  ColumnHead(4) = 'Units'
  columnHead(5) = '~~$~~ per Qty.'
  columnHead(6) = 'SubTotal ~~$~~'

  columnWidth = (/7,30,16,10,16,16/) !array assignment - for all columns

  DO Item=1,NumLineItems
    tableBody(item, 1) = trim(IntToStr(CostLineItem(item)%LineNumber))
    tableBody(item, 2) = trim(CostLineItem(item)%LineName)
    IF (unitsStyle .EQ. unitsStyleInchPound) THEN
      CALL LookupSItoIP(CostLineItem(item)%Units, unitConvIndex, IPunitName)
      IF (unitConvIndex .NE. 0) THEN
        IPqty = convertIP(unitConvIndex,CostLineItem(item)%Qty)
        tableBody(item, 3) = trim(RealToStr(IPqty, 2))
        tableBody(item, 4) = trim(IPunitName)
        IPsingleValue = convertIP(unitConvIndex,1.0d0)
        IF (IPsingleValue .NE. 0.0d0) THEN
          IPvaluePer = CostLineItem(item)%ValuePer / IPsingleValue
          tableBody(item, 5) = trim(RealToStr(IPvaluePer, 2))
        END IF
      ELSE
        tableBody(item, 3) = trim(RealToStr(CostLineItem(item)%Qty, 2))
        tableBody(item, 4) = trim(CostLineItem(item)%Units)
        tableBody(item, 5) = trim(RealToStr(CostLineItem(item)%ValuePer, 2))
      END IF
    ELSE
      tableBody(item, 3) = trim(RealToStr(CostLineItem(item)%Qty, 2))
      tableBody(item, 4) = trim(CostLineItem(item)%Units)
      tableBody(item, 5) = trim(RealToStr(CostLineItem(item)%ValuePer, 2))
    END IF
    tableBody(item, 6) = trim(RealToStr(CostLineItem(item)%LineSubTotal, 2))
  ENDDO
  tableBody(NumRows, 6) = trim(RealToStr(CurntBldg%LineItemTot, 2))
  CALL writeSubtitle('Cost Line Item Details') !: '//trim(RealToStr(CostEstimateTotal, 2)))
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'Construction Cost Estimate Summary',&
                                      'Entire Facility',&
                                      'Cost Line Item Details')
  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)


  RETURN

END SUBROUTINE WriteCompCostTable

SUBROUTINE WriteVeriSumTable
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   June 2006
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Summarize inputs and results for use with code and beyond-code
          !   compliance into a tabular report for output.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   This report actually consists of many sub-tables each with
          !   its own call to writeTable.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataStringGlobals, ONLY: VerString
USE DataEnvironment,   ONLY: EnvironmentName,Latitude,Longitude,Elevation,TimeZoneNumber, &
                             RunPeriodStartDayOfWeek,WeatherFileLocationTitle
USE DataHeatBalance,   ONLY: Zone, BuildingAzimuth, Construct, TotLights, Lights, ZoneIntGain, &
                             People, TotPeople, ZoneElectric, TotElecEquip, ZoneGas, TotGasEquip, &
                             ZoneOtherEq, TotOthEquip, ZoneHWEq, TotHWEquip, BuildingRotationAppendixG
USE DataSurfaces,      ONLY: Surface, TotSurfaces,SurfaceClass_Wall,SurfaceClass_Floor,SurfaceClass_Roof, &
                             SurfaceClass_Window,SurfaceClass_TDD_Dome,FrameDivider,ExternalEnvironment,Ground, &
                             OtherSideCondModeledExt,GroundFCfactorMethod
USE ScheduleManager,   ONLY: ScheduleAverageHoursPerWeek, GetScheduleName
USE ExteriorEnergyUse, ONLY: ExteriorLights, NumExteriorLights, ScheduleOnly, AstroClockOverride
USE General,           ONLY: SafeDivide,RoundSigDigits
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER, parameter :: wwrcTotal = 1
INTEGER, parameter :: wwrcNorth = 2
INTEGER, parameter :: wwrcEast = 3
INTEGER, parameter :: wwrcSouth = 4
INTEGER, parameter :: wwrcWest = 5
INTEGER, parameter :: wwrrWall = 1
INTEGER, parameter :: wwrrAbvGndWall = 2
INTEGER, parameter :: wwrrWindow = 3
INTEGER, parameter :: wwrrWWR = 4
INTEGER, parameter :: wwrrAbvGndWWR = 5

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody

INTEGER :: iSurf
INTEGER :: kOpaque
INTEGER :: zonePt
INTEGER :: iLight
INTEGER :: iZone
INTEGER :: iPeople
INTEGER :: iPlugProc
REAL(r64) :: mult
REAL(r64) :: curAzimuth
REAL(r64) :: curArea
REAL(r64) :: wallAreaN
REAL(r64) :: wallAreaS
REAL(r64) :: wallAreaE
REAL(r64) :: wallAreaW
REAL(r64) :: aboveGroundWallAreaN
REAL(r64) :: aboveGroundWallAreaS
REAL(r64) :: aboveGroundWallAreaE
REAL(r64) :: aboveGroundWallAreaW
REAL(r64) :: windowAreaN
REAL(r64) :: windowAreaS
REAL(r64) :: windowAreaE
REAL(r64) :: windowAreaW
!wall and window areas attached to conditioned zones
REAL(r64) :: wallAreaNcond
REAL(r64) :: wallAreaScond
REAL(r64) :: wallAreaEcond
REAL(r64) :: wallAreaWcond
REAL(r64) :: aboveGroundWallAreaNcond
REAL(r64) :: aboveGroundWallAreaScond
REAL(r64) :: aboveGroundWallAreaEcond
REAL(r64) :: aboveGroundWallAreaWcond
REAL(r64) :: windowAreaNcond
REAL(r64) :: windowAreaScond
REAL(r64) :: windowAreaEcond
REAL(r64) :: windowAreaWcond
LOGICAL :: isConditioned
LOGICAL :: isAboveGround

REAL(r64) :: roofArea
REAL(r64) :: skylightArea
REAL(r64) :: totLightPower
REAL(r64) :: totNumPeople
REAL(r64) :: totPlugProcess
REAL(r64) :: frameWidth
REAL(r64) :: frameArea

LOGICAL :: zoneIsCond
LOGICAL :: usezoneFloorArea

INTEGER :: grandTotal = 1
INTEGER :: condTotal = 2
INTEGER :: uncondTotal = 3
INTEGER :: notpartTotal = 4
INTEGER :: iTotal
CHARACTER(len=MaxNameLength) :: SIunit = ''
INTEGER :: unitConvIndex = 0
REAL(r64) :: m_unitConv = 0.0d0
REAL(r64) :: m2_unitConv = 0.0d0
REAL(r64) :: m3_unitConv = 0.0d0
REAL(r64) :: Wm2_unitConv = 0.0d0
CHARACTER(len=MaxNameLength) :: m_unitName = ''
CHARACTER(len=MaxNameLength) :: m2_unitName = ''
CHARACTER(len=MaxNameLength) :: m3_unitName = ''
CHARACTER(len=MaxNameLength) :: Wm2_unitName = ''

!zone summary total
REAL(r64), DIMENSION(4) :: zstArea = 0.0d0
REAL(r64), DIMENSION(4) :: zstVolume = 0.0d0
REAL(r64), DIMENSION(4) :: zstWallArea = 0.0d0
REAL(r64), DIMENSION(4) :: zstWindowArea = 0.0d0
REAL(r64), DIMENSION(4) :: zstLight = 0.0d0
REAL(r64), DIMENSION(4) :: zstPeople = 0.0d0
REAL(r64), DIMENSION(4) :: zstPlug = 0.0d0

! misc
REAL(r64) :: pdiff
LOGICAL :: DetailedWWR
REAL(r64) :: TotalWallArea
REAL(r64) :: TotalWindowArea
REAL(r64) :: TotalAboveGroundWallArea

! all arrays are in the format: (row, columnm)
IF (displayTabularVeriSum) THEN
  ! show the headers of the report
  CALL WriteReportHeaders('Input Verification and Results Summary','Entire Facility',isAverage)

  ! do unit conversions if necessary
  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    SIunit = '[m]'
    CALL LookupSItoIP(SIunit, unitConvIndex, m_unitName)
    m_unitConv = convertIP(unitConvIndex,1.0d0)
    SIunit = '[m2]'
    CALL LookupSItoIP(SIunit, unitConvIndex, m2_unitName)
    m2_unitConv = convertIP(unitConvIndex,1.0d0)
    SIunit = '[m3]'
    CALL LookupSItoIP(SIunit, unitConvIndex, m3_unitName)
    m3_unitConv = convertIP(unitConvIndex,1.0d0)
    SIunit = '[W/m2]'
    CALL LookupSItoIP(SIunit, unitConvIndex, Wm2_unitName)
    Wm2_unitConv = convertIP(unitConvIndex,1.0d0)
  ELSE
    m_unitName = '[m]'
    m_unitConv = 1.0d0
    m2_unitName = '[m2]'
    m2_unitConv = 1.0d0
    m3_unitName = '[m3]'
    m3_unitConv = 1.0d0
    Wm2_unitName = '[W/m2]'
    Wm2_unitConv = 1.0d0
  END IF
  !
  !---- General Sub-Table
  !

  ! since a variable number of design days is possible, first read them before sizing the arrays
  ALLOCATE(rowHead(10))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(10,1))

  columnHead(1) = 'Value'
  rowHead(1)  = 'Program Version and Build'
  rowHead(2)  = 'RunPeriod'
  rowHead(3)  = 'Weather File'
  rowHead(4)  = 'Latitude [deg]'
  rowHead(5)  = 'Longitude [deg]'

  rowHead(6)  = 'Elevation ' // TRIM(m_unitName)
  rowHead(7)  = 'Time Zone'
  rowHead(8)  = 'North Axis Angle [deg]'
  rowHead(9)  = 'Rotation for Appendix G [deg]'
  rowHead(10) = 'Hours Simulated [hrs]'
!  rowHead(9)  = 'Num Table Entries' !used for debugging

  tableBody = ''

  tableBody(1,1) = TRIM(VerString) !program
  tableBody(2,1) = TRIM(EnvironmentName) !runperiod name
  tableBody(3,1) = TRIM(WeatherFileLocationTitle) !weather
  tableBody(4,1) = TRIM(RealToStr(Latitude,2)) !latitude
  tableBody(5,1) = TRIM(RealToStr(Longitude,2)) !longitude
  tableBody(6,1) = TRIM(RealToStr(Elevation * m_unitConv,2)) !Elevation
  tableBody(7,1) = TRIM(RealToStr(TimeZoneNumber,2)) !Time Zone
  tableBody(8,1) = TRIM(RealToStr(BuildingAzimuth,2)) !north axis angle
  tableBody(9,1) = TRIM(RealToStr(BuildingRotationAppendixG,2)) !Rotation for Appendix G
  tableBody(10,1) = TRIM(RealToStr(gatherElapsedTimeBEPS,2)) !hours simulated
!  tableBody(9,1) = TRIM(IntToStr(numTableEntry)) !number of table entries for predefined tables

  CALL writeSubtitle('General')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'InputVerificationandResultsSummary',&
                                      'Entire Facility',&
                                      'General')

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
  !
  !---- Window Wall Ratio Sub-Table
  !
  CALL writeTextLine('ENVELOPE',.TRUE.)

  ALLOCATE(rowHead(5))
  ALLOCATE(columnHead(5))
  ALLOCATE(columnWidth(5))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(5,5))

  columnHead(wwrcTotal) = 'Total'
  columnHead(wwrcNorth) = 'North (315 to 45 deg)'
  columnHead(wwrcEast) = 'East (45 to 135 deg)'
  columnHead(wwrcSouth) = 'South (135 to 225 deg)'
  columnHead(wwrcWest) = 'West (225 to 315 deg)'

  rowHead(wwrrWall)  = 'Gross Wall Area ' // TRIM(m2_unitName)
  rowHead(wwrrAbvGndWall)  = 'Above Ground Wall Area ' // TRIM(m2_unitName)
  rowHead(wwrrWindow)  = 'Window Opening Area ' // TRIM(m2_unitName)
  rowHead(wwrrWWR)  = 'Gross Window-Wall Ratio [%]'
  rowHead(wwrrAbvGndWWR)  = 'Above Ground Window-Wall Ratio [%]'

  wallAreaN = 0.0d0
  wallAreaS = 0.0d0
  wallAreaE = 0.0d0
  wallAreaW = 0.0d0
  aboveGroundWallAreaN = 0.0d0
  aboveGroundWallAreaS = 0.0d0
  aboveGroundWallAreaE = 0.0d0
  aboveGroundWallAreaW = 0.0d0
  windowAreaN = 0.0d0
  windowAreaS = 0.0d0
  windowAreaE = 0.0d0
  windowAreaW = 0.0d0
  wallAreaNcond = 0.0d0
  wallAreaScond = 0.0d0
  wallAreaEcond = 0.0d0
  wallAreaWcond = 0.0d0
  aboveGroundWallAreaNcond = 0.0d0
  aboveGroundWallAreaScond = 0.0d0
  aboveGroundWallAreaEcond = 0.0d0
  aboveGroundWallAreaWcond = 0.0d0
  windowAreaNcond = 0.0d0
  windowAreaScond = 0.0d0
  windowAreaEcond = 0.0d0
  windowAreaWcond = 0.0d0
  roofArea = 0.0d0
  skylightArea = 0.0d0
  totLightPower = 0.0d0
  totNumPeople = 0.0d0
  totPlugProcess = 0.0d0
  kOpaque = 0

  DetailedWWR=(GetNumSectionsFound('DETAILEDWWR_DEBUG') > 0)

  IF (DetailedWWR) THEN
    WRITE(OutputFileDebug,'(A)') '======90.1 Classification [>=60 & <=120] tilt = wall=================='
    WRITE(OutputFileDebug,'(A)') 'SurfName,Class,Area,Tilt'
  ENDIF

  DO iSurf = 1, TotSurfaces
    !only exterior surfaces including underground
    IF (.not. Surface(iSurf)%HeatTransSurf) CYCLE
    isAboveGround = (Surface(iSurf)%ExtBoundCond == ExternalEnvironment) .or.  &
        (Surface(iSurf)%ExtBoundCond == OtherSideCondModeledExt)
    IF (isAboveGround  .or. (Surface(iSurf)%ExtBoundCond == Ground)  .or.   &
        (Surface(iSurf)%ExtBoundCond == GroundFCfactorMethod)) THEN
      curAzimuth = Surface(iSurf)%Azimuth
      curArea = Surface(iSurf)%GrossArea
      IF  (Surface(iSurf)%FrameDivider .NE. 0) THEN
        frameWidth = FrameDivider(Surface(iSurf)%FrameDivider)%FrameWidth
        frameArea = (Surface(iSurf)%Height + 2.0d0*frameWidth)*(Surface(iSurf)%Width + 2.0d0*frameWidth) &
           - (Surface(iSurf)%Height * Surface(iSurf)%Width)
        curArea = curArea + frameArea
      END IF
      zonePt = Surface(iSurf)%Zone
      isConditioned = .FALSE.
      IF (zonePt .GT. 0) THEN
        IF (Zone(zonePt)%SystemZoneNodeNumber .GT. 0) THEN
          isConditioned = .TRUE.
        ENDIF
      END IF
      IF ((Surface(iSurf)%Tilt >= 60.d0) .AND. (Surface(iSurf)%Tilt <= 120.d0)) THEN
        !vertical walls and windows
        SELECT CASE (Surface(iSurf)%Class)
          CASE (SurfaceClass_Wall,SurfaceClass_Floor,SurfaceClass_Roof)
            mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier
            IF ((curAzimuth .GE. 315.d0) .OR. (curAzimuth .LT. 45.d0)) THEN
              wallAreaN = wallAreaN + curArea * mult
              IF (isConditioned) wallAreaNcond = wallAreaNcond + curArea * mult
              IF (isAboveGround) THEN
                aboveGroundWallAreaN = aboveGroundWallAreaN + curArea * mult
                IF (isConditioned) aboveGroundWallAreaNcond = aboveGroundWallAreaNcond + curArea * mult
              ENDIF
            ELSE IF ((curAzimuth .GE. 45.d0) .AND. (curAzimuth .LT. 135.d0)) THEN
              wallAreaE = wallAreaE + curArea * mult
              IF (isConditioned) wallAreaEcond = wallAreaEcond + curArea * mult
              IF (isAboveGround) THEN
                aboveGroundWallAreaE = aboveGroundWallAreaE + curArea * mult
                IF (isConditioned) aboveGroundWallAreaEcond = aboveGroundWallAreaEcond + curArea * mult
              ENDIF
            ELSE IF ((curAzimuth .GE. 135.d0) .AND. (curAzimuth .LT. 225.d0)) THEN
              wallAreaS = wallAreaS + curArea * mult
              IF (isConditioned) wallAreaScond = wallAreaScond + curArea * mult
              IF (isAboveGround) THEN
                aboveGroundWallAreaS = aboveGroundWallAreaS + curArea * mult
                IF (isConditioned) aboveGroundWallAreaScond = aboveGroundWallAreaScond + curArea * mult
              ENDIF
            ELSE IF ((curAzimuth .GE. 225.d0) .AND. (curAzimuth .LT. 315.d0)) THEN
              wallAreaW = wallAreaW + curArea * mult
              IF (isConditioned) wallAreaWcond = wallAreaWcond + curArea * mult
              IF (isAboveGround) THEN
                aboveGroundWallAreaW = aboveGroundWallAreaW + curArea * mult
                IF (isConditioned) aboveGroundWallAreaWcond = aboveGroundWallAreaWcond + curArea * mult
              ENDIF
            ENDIF
            IF (DetailedWWR) THEN
              WRITE(OutputFileDebug,'(A)') trim(Surface(iSurf)%Name)//',Wall,'//trim(RoundSigDigits(curArea*mult,1))//  &
                 ','//trim(RoundSigDigits(Surface(iSurf)%Tilt,1))
            ENDIF
          CASE (SurfaceClass_Window,SurfaceClass_TDD_Dome)
            mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier * Surface(iSurf)%Multiplier
            IF ((curAzimuth .GE. 315.d0) .OR. (curAzimuth .LT. 45.d0)) THEN
              windowAreaN = windowAreaN + curArea * mult
              IF (isConditioned) windowAreaNcond = windowAreaNcond + curArea * mult
            ELSE IF ((curAzimuth .GE. 45.d0) .AND. (curAzimuth .LT. 135.d0)) THEN
              windowAreaE = windowAreaE + curArea * mult
              IF (isConditioned) windowAreaEcond = windowAreaEcond + curArea * mult
            ELSE IF ((curAzimuth .GE. 135.d0) .AND. (curAzimuth .LT. 225.d0)) THEN
              windowAreaS = windowAreaS + curArea * mult
              IF (isConditioned) windowAreaScond = windowAreaScond + curArea * mult
            ELSE IF ((curAzimuth .GE. 225.d0) .AND. (curAzimuth .LT. 315.d0)) THEN
              windowAreaW = windowAreaW + curArea * mult
              IF (isConditioned) windowAreaWcond = windowAreaWcond + curArea * mult
            ENDIF
            IF (DetailedWWR) THEN
              WRITE(OutputFileDebug,'(A)') trim(Surface(iSurf)%Name)//',Window,'//trim(RoundSigDigits(curArea*mult,1))//  &
                 ','//trim(RoundSigDigits(Surface(iSurf)%Tilt,1))
            ENDIF
        END SELECT
      ELSE IF (Surface(iSurf)%Tilt < 60.d0) THEN !roof and skylights
        SELECT CASE (Surface(iSurf)%Class)
          CASE (SurfaceClass_Wall,SurfaceClass_Floor,SurfaceClass_Roof)
              mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier
              roofArea = roofArea + curArea * mult
              IF (DetailedWWR) THEN
                WRITE(OutputFileDebug,'(A)') trim(Surface(iSurf)%Name)//',Roof,'//trim(RoundSigDigits(curArea*mult,1))//  &
                    ','//trim(RoundSigDigits(Surface(iSurf)%Tilt,1))
              ENDIF
          CASE (SurfaceClass_Window,SurfaceClass_TDD_Dome)
              mult = Zone(zonePt)%Multiplier * Zone(zonePt)%ListMultiplier * Surface(iSurf)%Multiplier
              skylightArea = skylightArea + curArea * mult
              IF (DetailedWWR) THEN
                WRITE(OutputFileDebug,'(A)') trim(Surface(iSurf)%Name)//',Skylight,'//trim(RoundSigDigits(curArea*mult,1))// &
                     ','//trim(RoundSigDigits(Surface(iSurf)%Tilt,1))
              ENDIF
        END SELECT
      ELSE !floors
        !ignored
      END IF
    END IF
  END DO

  TotalWallArea=wallAreaN + wallAreaS + wallAreaE + wallAreaW
  TotalAboveGroundWallArea=aboveGroundWallAreaN+aboveGroundWallAreaS+aboveGroundWallAreaE+aboveGroundWallAreaW
  TotalWindowArea=windowAreaN + windowAreaS + windowAreaE + windowAreaW
  IF (DetailedWWR) THEN
    WRITE(OutputFileDebug,'(A)') '========================'
    WRITE(OutputFileDebug,'(A)') 'TotalWallArea,WallAreaN,WallAreaS,WallAreaE,WallAreaW'
    WRITE(OutputFileDebug,'(A)') 'TotalWindowArea,WindowAreaN,WindowAreaS,WindowAreaE,WindowAreaW'
    WRITE(OutputFileDebug,'(A)') trim(RoundSigDigits(TotalWallArea,2))//','//  &
       trim(RoundSigDigits(WallAreaN,2))//','//trim(RoundSigDigits(WallAreaS,2))//','//   &
       trim(RoundSigDigits(WallAreaE,2))//','//trim(RoundSigDigits(WallAreaW,2))
    WRITE(OutputFileDebug,'(A)') trim(RoundSigDigits(TotalWindowArea,2))//','//  &
       trim(RoundSigDigits(WindowAreaN,2))//','//trim(RoundSigDigits(WindowAreaS,2))//','//   &
       trim(RoundSigDigits(WindowAreaE,2))//','//trim(RoundSigDigits(WindowAreaW,2))
  ENDIF

  tableBody = ''

  tableBody(wwrrWall,wwrcNorth) = TRIM(RealToStr(wallAreaN * m2_unitConv,2))
  tableBody(wwrrWall,wwrcSouth) = TRIM(RealToStr(wallAreaS * m2_unitConv,2))
  tableBody(wwrrWall,wwrcEast) =  TRIM(RealToStr(wallAreaE * m2_unitConv,2))
  tableBody(wwrrWall,wwrcWest) =  TRIM(RealToStr(wallAreaW * m2_unitConv,2))
  tableBody(wwrrWall,wwrcTotal) =  TRIM(RealToStr(TotalWallArea * m2_unitConv,2))

  tableBody(wwrrAbvGndWall,wwrcNorth) = TRIM(RealToStr(aboveGroundWallAreaN * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcSouth) = TRIM(RealToStr(aboveGroundWallAreaS * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcEast) =  TRIM(RealToStr(aboveGroundWallAreaE * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcWest) =  TRIM(RealToStr(aboveGroundWallAreaW * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcTotal) =  TRIM(RealToStr(TotalAboveGroundWallArea * m2_unitConv,2))

  tableBody(wwrrWindow,wwrcNorth) = TRIM(RealToStr(windowAreaN * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcSouth) = TRIM(RealToStr(windowAreaS * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcEast) =  TRIM(RealToStr(windowAreaE * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcWest) =  TRIM(RealToStr(windowAreaW * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcTotal) =  TRIM(RealToStr(TotalWindowArea * m2_unitConv,2))

  tableBody(wwrrWWR,wwrcNorth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaN , wallAreaN),2))
  tableBody(wwrrWWR,wwrcSouth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaS , wallAreaS),2))
  tableBody(wwrrWWR,wwrcEast) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaE , wallAreaE),2))
  tableBody(wwrrWWR,wwrcWest) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaW , wallAreaW),2))
  tableBody(wwrrWWR,wwrcTotal) =  TRIM(RealToStr(100.d0 * SafeDivide(TotalWindowArea , TotalWallArea),2))

  tableBody(wwrrAbvGndWWR,wwrcNorth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaN , aboveGroundWallAreaN),2))
  tableBody(wwrrAbvGndWWR,wwrcSouth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaS , aboveGroundWallAreaS),2))
  tableBody(wwrrAbvGndWWR,wwrcEast) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaE , aboveGroundWallAreaE),2))
  tableBody(wwrrAbvGndWWR,wwrcWest) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaW , aboveGroundWallAreaW),2))
  tableBody(wwrrAbvGndWWR,wwrcTotal)=TRIM(RealToStr(100.d0*SafeDivide(TotalWindowArea,TotalAboveGroundWallArea),2))

  CALL writeSubtitle('Window-Wall Ratio')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'InputVerificationandResultsSummary',&
                                      'Entire Facility',&
                                      'Window-Wall Ratio')


  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- Conditioned Window Wall Ratio Sub-Table
  !
  ALLOCATE(rowHead(5))
  ALLOCATE(columnHead(5))
  ALLOCATE(columnWidth(5))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(5,5))

 columnHead(wwrcTotal) = 'Total'
  columnHead(wwrcNorth) = 'North (315 to 45 deg)'
  columnHead(wwrcEast) = 'East (45 to 135 deg)'
  columnHead(wwrcSouth) = 'South (135 to 225 deg)'
  columnHead(wwrcWest) = 'West (225 to 315 deg)'

  rowHead(wwrrWall)  = 'Gross Wall Area ' // TRIM(m2_unitName)
  rowHead(wwrrAbvGndWall)  = 'Above Ground Wall Area ' // TRIM(m2_unitName)
  rowHead(wwrrWindow)  = 'Window Opening Area ' // TRIM(m2_unitName)
  rowHead(wwrrWWR)  = 'Gross Window-Wall Ratio [%]'
  rowHead(wwrrAbvGndWWR)  = 'Above Ground Window-Wall Ratio [%]'

!calculations appear in last block with normal window-wall ratio table

  TotalWallArea=wallAreaNcond + wallAreaScond + wallAreaEcond + wallAreaWcond
  TotalAboveGroundWallArea=aboveGroundWallAreaNcond+aboveGroundWallAreaScond+aboveGroundWallAreaEcond+aboveGroundWallAreaWcond
  TotalWindowArea=windowAreaNcond + windowAreaScond + windowAreaEcond + windowAreaWcond

  tableBody = ''

  tableBody(wwrrWall,wwrcNorth) = TRIM(RealToStr(wallAreaNcond * m2_unitConv,2))
  tableBody(wwrrWall,wwrcSouth) = TRIM(RealToStr(wallAreaScond * m2_unitConv,2))
  tableBody(wwrrWall,wwrcEast) =  TRIM(RealToStr(wallAreaEcond * m2_unitConv,2))
  tableBody(wwrrWall,wwrcWest) =  TRIM(RealToStr(wallAreaWcond * m2_unitConv,2))
  tableBody(wwrrWall,wwrcTotal) =  TRIM(RealToStr(TotalWallArea * m2_unitConv,2))

  tableBody(wwrrAbvGndWall,wwrcNorth) = TRIM(RealToStr(aboveGroundWallAreaNcond * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcSouth) = TRIM(RealToStr(aboveGroundWallAreaScond * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcEast) =  TRIM(RealToStr(aboveGroundWallAreaEcond * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcWest) =  TRIM(RealToStr(aboveGroundWallAreaWcond * m2_unitConv,2))
  tableBody(wwrrAbvGndWall,wwrcTotal) =  TRIM(RealToStr(TotalAboveGroundWallArea * m2_unitConv,2))

  tableBody(wwrrWindow,wwrcNorth) = TRIM(RealToStr(windowAreaNcond * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcSouth) = TRIM(RealToStr(windowAreaScond * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcEast) =  TRIM(RealToStr(windowAreaEcond * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcWest) =  TRIM(RealToStr(windowAreaWcond * m2_unitConv,2))
  tableBody(wwrrWindow,wwrcTotal) =  TRIM(RealToStr(TotalWindowArea * m2_unitConv,2))

  tableBody(wwrrWWR,wwrcNorth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaNcond , wallAreaNcond),2))
  tableBody(wwrrWWR,wwrcSouth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaScond , wallAreaScond),2))
  tableBody(wwrrWWR,wwrcEast) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaEcond , wallAreaEcond),2))
  tableBody(wwrrWWR,wwrcWest) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaWcond , wallAreaWcond),2))
  tableBody(wwrrWWR,wwrcTotal) =  TRIM(RealToStr(100.d0 * SafeDivide(TotalWindowArea,TotalWallArea),2))

  tableBody(wwrrAbvGndWWR,wwrcNorth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaNcond , aboveGroundWallAreaNcond),2))
  tableBody(wwrrAbvGndWWR,wwrcSouth) = TRIM(RealToStr(100.d0 * SafeDivide(windowAreaScond , aboveGroundWallAreaScond),2))
  tableBody(wwrrAbvGndWWR,wwrcEast) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaEcond , aboveGroundWallAreaEcond),2))
  tableBody(wwrrAbvGndWWR,wwrcWest) =  TRIM(RealToStr(100.d0 * SafeDivide(windowAreaWcond , aboveGroundWallAreaWcond),2))
  tableBody(wwrrAbvGndWWR,wwrcTotal)=TRIM(RealToStr(100.d0*SafeDivide(TotalWindowArea,TotalAboveGroundWallArea),2))

  CALL writeSubtitle('Conditioned Window-Wall Ratio')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'InputVerificationandResultsSummary',&
                                      'Entire Facility',&
                                      'Conditioned Window-Wall Ratio')


  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  !
  !---- Skylight Roof Ratio Sub-Table
  !
  ALLOCATE(rowHead(3))
  ALLOCATE(columnHead(1))
  ALLOCATE(columnWidth(1))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(3,1))

  columnHead(1) = 'Total'

  rowHead(1)  = 'Gross Roof Area ' // TRIM(m2_unitName)
  rowHead(2)  = 'Skylight Area ' // TRIM(m2_unitName)
  rowHead(3)  = 'Skylight-Roof Ratio [%]'

  IF (DetailedWWR) THEN
    WRITE(OutputFileDebug,'(A)') '========================'
    WRITE(OutputFileDebug,'(A)') 'TotalRoofArea,SkylightArea'
    WRITE(OutputFileDebug,'(A)') trim(RoundSigDigits(roofArea,2))//','//  &
       trim(RoundSigDigits(skylightArea,2))
  ENDIF

  tableBody(1,1) = TRIM(RealToStr(roofArea * m2_unitConv,2))
  tableBody(2,1) = TRIM(RealToStr(skylightArea * m2_unitConv,2))
  tableBody(3,1) = TRIM(RealToStr(100.d0 * SafeDivide(skylightArea , roofArea),2))

  CALL writeSubtitle('Skylight-Roof Ratio')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'InputVerificationandResultsSummary',&
                                      'Entire Facility',&
                                      'Skylight-Roof Ratio')


  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)

  IF (SUM(Zone(1:NumOfZones)%ExtGrossWallArea_Multiplied) > 0.0d0 .or.   &
      SUM(Zone(1:NumOfZones)%ExtGrossGroundWallArea_Multiplied) > 0.0d0) THEN
    pdiff=ABS((wallAreaN + wallAreaS + wallAreaE + wallAreaW)-  &
       (SUM(Zone(1:NumOfZones)%ExtGrossWallArea_Multiplied)+SUM(Zone(1:NumOfZones)%ExtGrossGroundWallArea_Multiplied)))/  &
       (SUM(Zone(1:NumOfZones)%ExtGrossWallArea_Multiplied)+SUM(Zone(1:NumOfZones)%ExtGrossGroundWallArea_Multiplied))
    IF (pdiff > .019d0) THEN
      CALL ShowWarningError('WriteVeriSumTable: InputVerificationsAndResultsSummary: '//  &
         'Wall area based on [>=60,<=120] degrees (tilt) as walls ')
      CALL ShowContinueError('differs ~'//trim(RoundSigDigits(pdiff*100.d0,1))//  &
         '% from user entered Wall class surfaces. '//  &
         'Degree calculation based on ASHRAE 90.1 wall definitions.')
!      CALL ShowContinueError('Calculated based on degrees=['//  &
!         trim(adjustl(RealToStr((wallAreaN + wallAreaS + wallAreaE + wallAreaW),3)))//  &
!         '] m2, Calculated from user entered Wall class surfaces=['//  &
!         trim(adjustl(RealToStr(SUM(Zone(1:NumOfZones)%ExtGrossWallArea_Multiplied),3)))//' m2.')
      CALL ShowContinueError('Check classes of surfaces and tilts for discrepancies.')
      CALL ShowContinueError('Total wall area by ASHRAE 90.1 definition='//  &
         trim(adjustl(RealToStr((wallAreaN + wallAreaS + wallAreaE + wallAreaW),3)))//  &
         ' m2.')
      CALL ShowContinueError('Total exterior wall area from user entered classes='//  &
         trim(adjustl(RealToStr(SUM(Zone(1:NumOfZones)%ExtGrossWallArea_Multiplied),3)))//' m2.')
      CALL ShowContinueError('Total ground contact wall area from user entered classes='//  &
         trim(adjustl(RealToStr(SUM(Zone(1:NumOfZones)%ExtGrossGroundWallArea_Multiplied),3)))//' m2.')
    ENDIF
  ENDIF
  !
  !---- Space Summary Sub-Table
  !
  CALL writeTextLine('PERFORMANCE',.TRUE.)

  ALLOCATE(rowHead(NumOfZones + 4))
  ALLOCATE(columnHead(10))
  ALLOCATE(columnWidth(10))
  columnWidth = 14 !array assignment - same for all columns
  ALLOCATE(tableBody(NumOfZones + 4,10))

  columnHead(1) = 'Area ' // TRIM(m2_unitName)
  columnHead(2) = 'Conditioned (Y/N)'
  columnHead(3) = 'Part of Total Floor Area (Y/N)'
  columnHead(4) = 'Volume ' // TRIM(m3_unitName)
  columnHead(5) = 'Multipliers'
  columnHead(6) = 'Gross Wall Area ' // TRIM(m2_unitName)
  columnHead(7) = 'Window Glass Area ' // TRIM(m2_unitName)
  columnHead(8) = 'Lighting ' // TRIM(Wm2_unitName)
  columnHead(9) = 'People '  // TRIM(m2_unitName(1:Len_Trim(m2_unitName)-1)) //  &
     ' per person'//m2_unitName(Len_Trim(m2_unitName):Len_Trim(m2_unitName))
  columnHead(10) = 'Plug and Process ' // TRIM(Wm2_unitName)

  rowHead  = ''

  rowHead(NumOfZones + grandTotal) = 'Total'
  rowHead(NumOfZones + condTotal) = 'Conditioned Total'
  rowHead(NumOfZones + uncondTotal) = 'Unconditioned Total'
  rowHead(NumOfZones + notpartTotal) = 'Not Part of Total'

  tableBody = ''

  DO iZone = 1, NumOfZones
    mult = Zone(iZone)%Multiplier * Zone(iZone)%ListMultiplier
    rowHead(iZone) = TRIM(Zone(iZone)%Name)
    IF (Zone(iZone)%SystemZoneNodeNumber .GT. 0)   THEN
      tableBody(iZone,2) = 'Yes'
      zoneIsCond = .TRUE.
    ELSE
      tableBody(iZone,2) = 'No'
      zoneIsCond = .FALSE.
    END IF
    IF (Zone(iZone)%isPartOfTotalArea)   THEN
      tableBody(iZone,3) = 'Yes'
      usezoneFloorArea = .TRUE.
    ELSE
      tableBody(iZone,3) = 'No'
      usezoneFloorArea = .FALSE.
    END IF
    tableBody(iZone,1) = TRIM(RealToStr(Zone(iZone)%FloorArea * m2_unitConv,2))
    tableBody(iZone,4) = TRIM(RealToStr(Zone(iZone)%Volume * m3_unitConv,2))
    !no unit conversion necessary since done automatically
    CALL PreDefTableEntry(pdchLeedSutSpArea,Zone(iZone)%Name,Zone(iZone)%FloorArea,2)
    IF (zoneIsCOnd) THEN
      CALL PreDefTableEntry(pdchLeedSutOcArea,Zone(iZone)%Name,Zone(iZone)%FloorArea,2)
      CALL PreDefTableEntry(pdchLeedSutUnArea,Zone(iZone)%Name,'0.00')
    ELSE
      CALL PreDefTableEntry(pdchLeedSutOcArea,Zone(iZone)%Name,'0.00')
      CALL PreDefTableEntry(pdchLeedSutUnArea,Zone(iZone)%Name,Zone(iZone)%FloorArea,2)
    ENDIF
    tableBody(iZone,5) = TRIM(RealToStr(mult,2))
    tableBody(iZone,6) = TRIM(RealToStr(Zone(iZone)%ExtGrossWallArea * m2_unitConv,2))
    tableBody(iZone,7) = TRIM(RealToStr(Zone(iZone)%ExtWindowArea * m2_unitConv,2))
    ! lighting density
    totLightPower = 0.0d0
    DO iLight = 1, TotLights
      IF (iZone .EQ. Lights(iLight)%ZonePtr) THEN
        totLightPower = totLightPower + Lights(iLight)%DesignLevel
      END IF
    END DO
    IF (Zone(iZone)%FloorArea .GT. 0 .and. usezoneFloorArea) THEN
      tableBody(iZone,8) = TRIM(RealToStr(Wm2_unitConv * totLightPower / Zone(iZone)%FloorArea,4))
    END IF
    ! people density
    totNumPeople = 0.0d0
    DO iPeople = 1, TotPeople
      IF (iZone .EQ. People(iPeople)%ZonePtr) THEN
        totNumPeople = totNumPeople + People(iPeople)%NumberOfPeople
      END IF
    END DO
    IF (totNumPeople .GT. 0) THEN
      tableBody(iZone,9) = TRIM(RealToStr(Zone(iZone)%FloorArea * m2_unitConv / totNumPeople,2))
    END IF
    ! plug and process density
    totPlugProcess =  0.0d0
    DO iPlugProc = 1, TotElecEquip
      IF (iZone .EQ. ZoneElectric(iPlugProc)%ZonePtr) THEN
        totPlugProcess = totPlugProcess + ZoneElectric(iPlugProc)%DesignLevel
      END IF
    END DO
    DO iPlugProc = 1, TotGasEquip
      IF (iZone .EQ. ZoneGas(iPlugProc)%ZonePtr) THEN
        totPlugProcess = totPlugProcess + ZoneGas(iPlugProc)%DesignLevel
      END IF
    END DO
    DO iPlugProc = 1, TotOthEquip
      IF (iZone .EQ. ZoneOtherEq(iPlugProc)%ZonePtr) THEN
        totPlugProcess = totPlugProcess + ZoneOtherEq(iPlugProc)%DesignLevel
      END IF
    END DO
    DO iPlugProc = 1, TotHWEquip
      IF (iZone .EQ. ZoneHWEq(iPlugProc)%ZonePtr) THEN
        totPlugProcess = totPlugProcess + ZoneHWEq(iPlugProc)%DesignLevel
      END IF
    END DO
    IF (Zone(iZone)%FloorArea .GT. 0 .and. useZoneFloorArea) THEN
      tableBody(iZone,10) = TRIM(RealToStr(totPlugProcess * Wm2_unitConv / Zone(iZone)%FloorArea,4))
    END IF
    !total rows for conditioned, unconditioned, and total
    IF (usezoneFloorArea) THEN
      zstArea(grandTotal) = zstArea(grandTotal) + mult * Zone(iZone)%FloorArea
      zstVolume(grandTotal) = zstVolume(grandTotal) + mult * Zone(iZone)%Volume
      zstWallArea(grandTotal) = zstWallArea(grandTotal) + mult * Zone(iZone)%ExtGrossWallArea
      zstWindowArea(grandTotal) = zstWindowArea(grandTotal) + mult * Zone(iZone)%ExtWindowArea
      zstLight(grandTotal) = zstLight(grandTotal) + mult * totLightPower
      zstPeople(grandTotal) = zstPeople(grandTotal) + mult * totNumPeople
      zstPlug(grandTotal) = zstPlug(grandTotal) + mult * totPlugProcess
    ELSE
      zstArea(notpartTotal) = zstArea(notpartTotal) + mult * Zone(iZone)%FloorArea
      zstVolume(notpartTotal) = zstVolume(notpartTotal) + mult * Zone(iZone)%Volume
      zstWallArea(notpartTotal) = zstWallArea(notpartTotal) + mult * Zone(iZone)%ExtGrossWallArea
      zstWindowArea(notpartTotal) = zstWindowArea(notpartTotal) + mult * Zone(iZone)%ExtWindowArea
      zstLight(notpartTotal) = zstLight(notpartTotal) + mult * totLightPower
      zstPeople(notpartTotal) = zstPeople(notpartTotal) + mult * totNumPeople
      zstPlug(notpartTotal) = zstPlug(notpartTotal) + mult * totPlugProcess
    ENDIF
    IF (zoneIsCond .and. usezoneFloorArea) THEN
      zstArea(condTotal) = zstArea(condTotal) + mult * Zone(iZone)%FloorArea
      zstVolume(condTotal) = zstVolume(condTotal) + mult * Zone(iZone)%Volume
      zstWallArea(condTotal) = zstWallArea(condTotal) + mult * Zone(iZone)%ExtGrossWallArea
      zstWindowArea(condTotal) = zstWindowArea(condTotal) + mult * Zone(iZone)%ExtWindowArea
      zstLight(condTotal) = zstLight(condTotal) + mult * totLightPower
      zstPeople(condTotal) = zstPeople(condTotal) + mult * totNumPeople
      zstPlug(condTotal) = zstPlug(condTotal) + mult * totPlugProcess
   ELSEIF (.not. zoneIsCond) THEN
      zstArea(uncondTotal) = zstArea(uncondTotal) + mult * Zone(iZone)%FloorArea
      zstVolume(uncondTotal) = zstVolume(uncondTotal) + mult * Zone(iZone)%Volume
      zstWallArea(uncondTotal) = zstWallArea(uncondTotal) + mult * Zone(iZone)%ExtGrossWallArea
      zstWindowArea(uncondTotal) = zstWindowArea(uncondTotal) + mult * Zone(iZone)%ExtWindowArea
      zstLight(uncondTotal) = zstLight(uncondTotal) + mult * totLightPower
      zstPeople(uncondTotal) = zstPeople(uncondTotal) + mult * totNumPeople
      zstPlug(uncondTotal) = zstPlug(uncondTotal) + mult * totPlugProcess
    ELSE
      zstArea(notpartTotal) = zstArea(notpartTotal) + mult * Zone(iZone)%FloorArea
      zstVolume(notpartTotal) = zstVolume(notpartTotal) + mult * Zone(iZone)%Volume
      zstWallArea(notpartTotal) = zstWallArea(notpartTotal) + mult * Zone(iZone)%ExtGrossWallArea
      zstWindowArea(notpartTotal) = zstWindowArea(notpartTotal) + mult * Zone(iZone)%ExtWindowArea
      zstLight(notpartTotal) = zstLight(notpartTotal) + mult * totLightPower
      zstPeople(notpartTotal) = zstPeople(notpartTotal) + mult * totNumPeople
      zstPlug(notpartTotal) = zstPlug(notpartTotal) + mult * totPlugProcess
    END IF
  END DO
  DO iTotal = 1, 4
    tableBody(NumOfZones + iTotal,1) = TRIM(RealToStr(zstArea(iTotal) * m2_unitConv,2))
    tableBody(NumOfZones + iTotal,4) = TRIM(RealToStr(zstVolume(iTotal) * m3_unitConv,2))
    tableBody(NumOfZones + iTotal,6) = TRIM(RealToStr(zstWallArea(iTotal) * m2_unitConv,2))
    tableBody(NumOfZones + iTotal,7) = TRIM(RealToStr(zstWindowArea(iTotal) * m2_unitConv,2))
    IF (zstArea(iTotal) .NE. 0) THEN
      tableBody(NumOfZones + iTotal,8) = TRIM(RealToStr(zstLight(iTotal) * Wm2_unitConv / zstArea(iTotal),4))
      tableBody(NumOfZones + iTotal,10) = TRIM(RealToStr(zstPlug(iTotal) * Wm2_unitConv / zstArea(iTotal),4))
    END IF
    IF (zstPeople(iTotal) .NE. 0) THEN
      tableBody(NumOfZones + iTotal,9) = TRIM(RealToStr(zstArea(iTotal) * m2_unitConv / zstPeople(iTotal),2))
    END IF
  END DO
  CALL PreDefTableEntry(pdchLeedSutSpArea,'Totals',zstArea(grandTotal),2)
  CALL PreDefTableEntry(pdchLeedSutOcArea,'Totals',zstArea(condTotal),2)
  CALL PreDefTableEntry(pdchLeedSutUnArea,'Totals',zstArea(uncondTotal),2)

  CALL writeSubtitle('Zone Summary')
  CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
  CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                      'InputVerificationandResultsSummary',&
                                      'Entire Facility',&
                                      'Zone Summary')

  DEALLOCATE(columnHead)
  DEALLOCATE(rowHead)
  DEALLOCATE(columnWidth)
  DEALLOCATE(tableBody)
END IF
END SUBROUTINE WriteVeriSumTable

SUBROUTINE WriteAdaptiveComfortTable

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Tyler Hoyt
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Writes summary table for adaptive comfort models. Tabulates
          ! occupied hours not meeting comfort bounds for ASHRAE-55 and
          ! CEN-15251 adaptive models.

          ! METHODOLOGY EMPLOYED:
          !


          ! REFERENCES:
          !

          ! USE STATEMENTS:
  USE DataHeatBalance, ONLY: People, TotPeople
  USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  CHARACTER(len=MaxNameLength), DIMENSION(5)     :: columnHead
  INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:)     :: rowHead
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:,:)   :: tableBody
  INTEGER :: numPeopleAdaptive = 0
  INTEGER :: i
  INTEGER, ALLOCATABLE, DIMENSION(:) :: peopleInd ! Index the relevant people

! Should deallocate after writing table. - LKL

  IF (displayAdaptiveComfort .AND. TotPeople > 0 ) THEN
    ALLOCATE(peopleInd(TotPeople))

    DO i=1,TotPeople
      IF (People(i)%AdaptiveASH55 .or. People(i)%AdaptiveCEN15251) THEN
        numPeopleAdaptive = numPeopleAdaptive + 1
        peopleInd(numPeopleAdaptive) = i
      END IF
    END DO

    ALLOCATE(rowHead(numPeopleAdaptive))
    ALLOCATE(tableBody(numPeopleAdaptive,5))

    CALL WriteReportHeaders('Adaptive Comfort Summary','Entire Facility',0)
    CALL writeSubtitle('Time Not Meeting the Adaptive Comfort Models during Occupied Hours')

    ALLOCATE(columnWidth(5))
    columnWidth = 10
    columnHead(1) = 'ASHRAE55 90% Acceptability Limits [Hours]'
    columnHead(2) = 'ASHRAE55 80% Acceptability Limits  [Hours]'
    columnHead(3) = 'CEN15251 Category I Acceptability Limits [Hours]'
    columnHead(4) = 'CEN15251 Category II Acceptability Limits [Hours]'
    columnHead(5) = 'CEN15251 Category III Acceptability Limits [Hours]'

    tableBody = ''
    DO i=1,numPeopleAdaptive
      rowHead(i) = People(i)%Name
      IF (People(i)%AdaptiveASH55) THEN
        tableBody(i,1)  = TRIM(RealToStr(People(peopleInd(i))%TimeNotMetASH5590,2))
        tableBody(i,2)  = TRIM(RealToStr(People(peopleInd(i))%TimeNotMetASH5580,2))
      END IF
      IF (People(i)%AdaptiveCEN15251) THEN
        tableBody(i,3)  = TRIM(RealToStr(People(peopleInd(i))%TimeNotMetCEN15251CatI,2))
        tableBody(i,4)  = TRIM(RealToStr(People(peopleInd(i))%TimeNotMetCEN15251CatII,2))
        tableBody(i,5)  = TRIM(RealToStr(People(peopleInd(i))%TimeNotMetCEN15251CatIII,2))
      END IF
    END DO

    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                            'AdaptiveComfortReport',&
                                            'Entire Facility',&
                                            'People Summary')
  END IF

END SUBROUTINE WriteAdaptiveComfortTable


SUBROUTINE WritePredefinedTables
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2006
          !       MODIFIED       January 2010, Kyle Benne; Added SQLite output
          !                      March 2010, Linda Lawrie; Modify SizingPeriod:DesignDay to convert column/humidity types
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Write out tables that have been predefined with data gathered
          !   throughout the program code.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   This is a generic routine to write a report with multiple
          !   subtables. The structure of the report are created in
          !   OutputReportPredefined which also includes a routine that
          !   builds up a tableEntry array which holds the data for the
          !   predefined reports.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: rowToUnqObjName
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: colHeadToColTag
INTEGER :: curNumColumns
INTEGER :: curNumRows
INTEGER :: curColumn
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: uniqueObjectName
LOGICAL, ALLOCATABLE, DIMENSION(:)  ::   useUniqueObjectName
INTEGER :: numUnqObjName
CHARACTER(len=MaxNameLength) :: curObjectName
INTEGER :: countRow
INTEGER :: countColumn
INTEGER :: found
INTEGER :: curColTagIndex
INTEGER :: curRowUnqObjIndex
INTEGER :: colCurrent
INTEGER :: rowCurrent
INTEGER :: iReportName
INTEGER :: jSubTable
INTEGER :: kColumnTag
INTEGER :: lTableEntry
INTEGER :: mUnqObjNames
INTEGER :: nColHead
INTEGER :: oRowHead
CHARACTER(len=MaxNameLength) :: colTagWithSI
CHARACTER(len=MaxNameLength) :: curColTag
INTEGER, ALLOCATABLE, DIMENSION(:)  ::   colUnitConv
INTEGER :: indexUnitConv
INTEGER :: columnUnitConv
CHARACTER(len=MaxNameLength) :: repTableTag
REAL(r64) :: IPvalue

! loop through the entries and associate them with the subtable and create
! list of unique object names
! Much of this code is to allow for integer compares instead of string
! compares that are nested three levels in a loop.
ALLOCATE(uniqueObjectName(numTableEntry))
ALLOCATE(useUniqueObjectName(numTableEntry))
numUnqObjName=0
DO lTableEntry = 1,numTableEntry
  !associate the subtable with each column
  curColumn = tableEntry(lTableEntry)%indexColumn
  IF ((curColumn .GE. 1) .AND. (curColumn .LE. numColumnTag)) THEN
    tableEntry(lTableEntry)%subTableIndex = columnTag(curColumn)%indexSubTable
  END IF
  !make a list of unique object names
  curObjectName = tableEntry(lTableEntry)%objectName
  found = 0
  DO mUnqObjNames = 1,numUnqObjName
    IF (SameString(curObjectName,uniqueObjectName(mUnqObjNames))) THEN
      found = mUnqObjNames
    END IF
  END DO
  ! if found then point to the unique object
  IF (found .GT. 0) THEN
    tableEntry(lTableEntry)%uniqueObjName = found
  ! if not found add to the unique object list
  ELSE
    numUnqObjName = numUnqObjName + 1
    uniqueObjectName(numUnqObjName) = curObjectName
    tableEntry(lTableEntry)%uniqueObjName = numUnqObjName
  END IF
END DO
! loop through all reports and include those that have been flagged as 'show'
DO iReportName = 1, numReportName
  IF (reportName(iReportName)%show) THEN
    CALL WriteReportHeaders(reportName(iReportname)%namewithSpaces,'Entire Facility',isAverage)
    ! loop through the subtables and include those that are associated with this report
    DO jSubTable = 1, numSubTable
      IF (subTable(jSubTable)%indexReportName .EQ. iReportName) THEN
        !determine how many columns
        curNumColumns = 0
        DO kColumnTag = 1,numColumnTag
          IF (columnTag(kColumnTag)%indexSubTable .EQ. jSubTable) THEN
            curNumColumns = curNumColumns + 1
          END IF
        END DO
        !determine how many rows by going through table entries and setting
        !flag in useUniqueObjectName to true, then count number of true's.
        useUniqueObjectName = .FALSE. !array assignment
        DO lTableEntry = 1,numTableEntry
          IF (tableEntry(lTableEntry)%subTableIndex .EQ. jSubTable) THEN
            useUniqueObjectName(tableEntry(lTableEntry)%uniqueObjName) = .TRUE.
          END IF
        END DO
        curNumRows = 0
        DO mUnqObjNames = 1,numUnqObjName
          IF (useUniqueObjectName(mUnqObjNames)) THEN
            curNumRows = curNumRows + 1
          END IF
        END DO
        IF (curNumRows .EQ. 0) curNumRows = 1
        ! now create the arrays that are filled with values
        ALLOCATE(rowHead(curNumRows))
        ALLOCATE(columnHead(curNumColumns))
        ALLOCATE(columnWidth(curNumColumns))
        columnWidth = 14 !array assignment - same for all columns
        ALLOCATE(tableBody(curNumRows,curNumColumns))
        rowHead = ''
        columnHead = ''
        tableBody = ''
        ! this array stores the unique object name index for each row
        ALLOCATE(rowToUnqObjName(curNumRows))
        ! this array stores the columnHead index for each column
        ALLOCATE(colHeadToColTag(curNumColumns))
        ALLOCATE(colUnitConv(curNumColumns))
        ! set row headings
        countRow = 0
        rowHead(1) = 'None'
        DO mUnqObjNames = 1,numUnqObjName
          IF (useUniqueObjectName(mUnqObjNames)) THEN
            countRow = countRow + 1
            rowHead(countRow) = uniqueObjectName(mUnqObjNames)
                  rowToUnqObjName(countRow) = mUnqObjNames
          END IF
        END DO
        ! set column headings
        countColumn = 0
        DO kColumnTag = 1,numColumnTag
          IF (columnTag(kColumnTag)%indexSubTable .EQ. jSubTable) THEN
            countColumn = countColumn + 1
            !do the unit conversions
            colTagWithSI = columnTag(kColumnTag)%heading
            IF (unitsStyle .EQ. unitsStyleInchPound) THEN
              CALL LookupSItoIP(colTagWithSI, indexUnitConv, curColTag)
              colUnitConv(countColumn) = indexUnitConv
            ELSE
              curColTag = colTagWithSI
              colUnitConv(countColumn) = 0
            END IF
            columnHead(countColumn) = curColTag
            colHeadToColTag(countColumn) = kColumnTag
          END IF
        END DO
        ! fill the body of the table from the entries
        ! find the entries associated with the current subtable
        DO lTableEntry = 1,numTableEntry
          IF (tableEntry(lTableEntry)%subTableIndex .EQ. jSubTable) THEN
            !determine what column the current entry is in
             curColTagIndex = tableEntry(lTableEntry)%indexColumn
             DO nColHead = 1,curNumColumns
               IF (curColTagIndex .EQ. colHeadToColTag(nColHead)) THEN
                 colCurrent = nColHead
                 EXIT
               END IF
             END DO
            !determine what row the current entry is in
            curRowUnqObjIndex = tableEntry(lTableEntry)%uniqueObjName
            DO oRowHead = 1, curNumRows
              IF (curRowUnqObjIndex .EQ. rowToUnqObjName(oRowHead)) THEN
                rowCurrent = oRowHead
                EXIT
              END IF
            END DO
            !finally assign the entry to the place in the table body
            IF (unitsStyle .EQ. unitsStyleInchPound) THEN
              columnUnitConv=colUnitConv(colCurrent)
              IF (SameString(subTable(jSubTable)%Name,'SizingPeriod:DesignDay')) THEN
                IF (SameString(columnHead(colCurrent),'Humidity Value')) THEN
                  CALL LookupSItoIP(tableEntry(lTableEntry+1)%charEntry, columnUnitConv, repTableTag)
                  tableEntry(lTableEntry+1)%charEntry=repTableTag
                ENDIF
              ENDIF
              IF (tableEntry(lTableEntry)%origEntryIsReal .AND. (columnUnitConv .NE.0)) THEN
                IPvalue = convertIP(columnUnitConv, tableEntry(lTableEntry)%origRealEntry)
                tableBody(rowCurrent, colCurrent) = TRIM(RealToStr(IPvalue,tableEntry(lTableEntry)%significantDigits))
              ELSE
                tableBody(rowCurrent, colCurrent) = tableEntry(lTableEntry)%charEntry
              END IF
            ELSE
              tableBody(rowCurrent, colCurrent) = tableEntry(lTableEntry)%charEntry
            END IF
          END IF
        END DO
        !create the actual output table
        CALL writeSubtitle(subTable(jSubTable)%name)
        CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.false.,subTable(jSubTable)%footnote)
        CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                            reportName(iReportname)%name,&
                                            'Entire Facility',&
                                            subTable(jSubTable)%name)
        !clean up the temporary arrays used
        DEALLOCATE(columnHead)
        DEALLOCATE(rowHead)
        DEALLOCATE(columnWidth)
        DEALLOCATE(tableBody)
        !clean up the pointer arrays
        DEALLOCATE(rowToUnqObjName)
        DEALLOCATE(colHeadToColTag)
        DEALLOCATE(colUnitConv)
      END IF
    END DO
  END IF
END DO
DEALLOCATE(uniqueObjectName)
END SUBROUTINE WritePredefinedTables

SUBROUTINE WriteComponentSizing
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2007
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Write out tables based on component sizing data originally
          !   found in the EIO report.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   The tables created do not have known headers for rows or
          !   columns so those are determined based on what calls have
          !   been made to the ReportSizingOutput routine.  A table
          !   is created for each type of component. Columns are created
          !   for each description within that table. Rows are created
          !   for each named object.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: colUnitConv
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: uniqueDesc
INTEGER :: numUniqueDesc
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: uniqueObj
INTEGER :: numUniqueObj
CHARACTER(len=MaxNameLength) :: curDesc
CHARACTER(len=MaxNameLength) :: curObj
INTEGER :: foundEntry
INTEGER :: foundDesc
INTEGER :: foundObj
INTEGER :: loopLimit
INTEGER :: iTableEntry
INTEGER :: jUnique
CHARACTER(len=MaxNameLength) :: curColHeadWithSI = ''
CHARACTER(len=MaxNameLength) :: curColHead = ''
INTEGER :: indexUnitConv = 0
REAL(r64) :: curValueSI = 0.0d0
REAL(r64) :: curValue = 0.0d0

IF (displayComponentSizing) THEN
  CALL WriteReportHeaders('Component Sizing Summary','Entire Facility',isAverage)
  !The arrays that look for unique headers are dimensioned in the
  !running program since the size of the number of entries is
  !not previouslly known. Use the size of all entries since that
  !is the maximum possible.
  ALLOCATE(uniqueDesc(numCompSizeTableEntry))
  ALLOCATE(uniqueObj(numCompSizeTableEntry))
  !initially clear the written flags for entire array
  ! The following line is not really necessary and it is possible that the array has
  ! not been allocated when this is first called.
  !  CompSizeTableEntry%written = .false.
  ! repeat the following loop until everything in array has been
  ! written into a table
  loopLimit = 0
  DO WHILE (loopLimit .LE. 100)  !put a maximum count since complex loop that could run indefinitely if error
    foundEntry = 0
    loopLimit = loopLimit + 1
    DO iTableEntry = 1, numCompSizeTableEntry
      IF (.NOT. CompSizeTableEntry(iTableEntry)%written) THEN
        foundEntry = iTableEntry
        EXIT
      END IF
    END DO
    IF (foundEntry .EQ. 0) EXIT !leave main loop - all items put into tables
    !clear active items
    CompSizeTableEntry%active = .false.
    !make an unwritten item that is of the same type active - these will be the
    !entries for the particular subtable.
    DO iTableEntry = 1, numCompSizeTableEntry
      IF (.NOT. CompSizeTableEntry(iTableEntry)%written) THEN
        IF (SameString(CompSizeTableEntry(iTableEntry)%typeField,CompSizeTableEntry(foundEntry)%typeField)) THEN
          CompSizeTableEntry(iTableEntry)%active = .true.
        END IF
      END IF
    END DO
    !identify unique descriptions and objects (columns and rows) in order
    !to size the table arrays properly.
    !reset the counters for the arrays looking for unique rows and columns
    numUniqueDesc = 0
    numUniqueObj = 0
    DO iTableEntry = 1, numCompSizeTableEntry
      !search for descriptions
      foundDesc = 0
      IF (CompSizeTableEntry(iTableEntry)%active) THEN
        curDesc = CompSizeTableEntry(iTableEntry)%description
        !look through the list of unique items to see if it matches
        DO jUnique = 1, numUniqueDesc
          IF (SameString(curDesc,uniqueDesc(jUnique))) THEN
            foundDesc = jUnique
            EXIT
          ENDIF
        END DO
        !if not found add to the list
        IF (foundDesc .EQ. 0) THEN
          numUniqueDesc = numUniqueDesc + 1
          uniqueDesc(numUniqueDesc) = curDesc
        END IF
        !search for objects
        foundObj = 0
        curObj = CompSizeTableEntry(iTableEntry)%nameField
        DO jUnique = 1, numUniqueObj
          IF (SameString(curObj,uniqueObj(jUnique))) THEN
            foundObj = jUnique
            EXIT
          ENDIF
        END DO
        !if not found add to the list
        IF (foundObj .EQ. 0) THEN
          numUniqueObj = numUniqueObj + 1
          uniqueObj(numUniqueObj) = curObj
        END IF
      END IF
    END DO
    !make sure the table has at least one row and columns
    IF (numUniqueDesc .EQ. 0) numUniqueDesc = 1
    IF (numUniqueObj .EQ. 0) numUniqueObj = 1
    !now that the unique row and column headers are known the array
    !sizes can be set for the table arrays
    ALLOCATE(rowHead(numUniqueObj))
    ALLOCATE(columnHead(numUniqueDesc))
    ALLOCATE(columnWidth(numUniqueDesc))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(colUnitConv(numUniqueDesc))
    ALLOCATE(tableBody(numUniqueObj,numUniqueDesc))
    ! initialize table body to blanks (in case entries are incomplete)
    tableBody = ' '
    !transfer the row and column headings first
    DO jUnique = 1, numUniqueDesc
      !do the unit conversions
      curColHeadWithSI = uniqueDesc(jUnique)
      IF (unitsStyle .EQ. unitsStyleInchPound) THEN
        CALL LookupSItoIP(curColHeadWithSI, indexUnitConv, curColHead)
        colUnitConv(jUnique) = indexUnitConv
      ELSE
        curColHead = curColHeadWithSI
        colUnitConv(jUnique) = 0
      END IF
      columnHead(jUnique) = curColHead
    END DO
    DO jUnique = 1, numUniqueObj
      rowHead(jUnique) = uniqueObj(jUnique)
    END DO
    !fill the table
    DO iTableEntry = 1, numCompSizeTableEntry
      !find the row and column for the specific entry
      IF (CompSizeTableEntry(iTableEntry)%active) THEN
        curDesc = CompSizeTableEntry(iTableEntry)%description
        foundDesc = 0
        DO jUnique = 1, numUniqueDesc
          IF (SameString(uniqueDesc(jUnique),curDesc)) THEN
            foundDesc = jUnique
            EXIT
          END IF
        END DO
        curObj = CompSizeTableEntry(iTableEntry)%nameField
        foundObj = 0
        DO jUnique = 1, numUniqueObj
          IF (SameString(rowHead(jUnique),curObj)) THEN
            foundObj = jUnique
            EXIT
          END IF
        END DO
        IF ((foundDesc .GE. 1) .AND. (foundObj .GE. 1)) THEN
          curValueSI = CompSizeTableEntry(iTableEntry)%valField
          IF (unitsStyle .EQ. unitsStyleInchPound) THEN
            IF (colUnitConv(foundDesc) .NE. 0) THEN
              curValue = convertIP(colUnitConv(foundDesc),curValueSI)
            ELSE
              curValue = curValueSI
            END IF
          ELSE
            curValue = curValueSI
          END IF
          IF (ABS(curValue) .GE. 1.0d0) THEN
            tableBody(foundObj,foundDesc) = TRIM(RealToStr(curValue,2))
          ELSE
            tableBody(foundObj,foundDesc) = TRIM(RealToStr(curValue,6))
          END IF
          CompSizeTableEntry(iTableEntry)%written = .true.
        END IF
      END IF
    END DO
    !write the table
    CALL writeSubtitle(CompSizeTableEntry(foundEntry)%typeField)
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth,.FALSE.,'User-Specified values were used. '// &
                                            'Design Size values were used if no User-Specified values were provided.')
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'ComponentSizingSummary',&
                                        'Entire Facility',&
                                        CompSizeTableEntry(foundEntry)%typeField)
    !deallocate these arrays since they are used to create the next
    !table
    DEALLOCATE(rowHead)
    DEALLOCATE(columnHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(colUnitConv)
    DEALLOCATE(tableBody)
  END DO
  !free the memory of these arrays that are only needed in this routine
  DEALLOCATE(uniqueDesc)
  DEALLOCATE(uniqueObj)
END IF
END SUBROUTINE WriteComponentSizing


SUBROUTINE WriteSurfaceShadowing
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2007
          !       MODIFIED       January 2010, Kyle Benne
          !                      Added SQLite output
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Write out tables based on which surfaces shade subsurfaces.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   Use <br> tag to put multiple rows into a single cell.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataSurfaces, ONLY: Surface, TotSurfaces
USE DataShadowingCombinations
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=2000),ALLOCATABLE, DIMENSION(:,:)            :: tableBody
!CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: unique
INTEGER,ALLOCATABLE, DIMENSION(:)     :: unique
INTEGER                                                    :: numUnique
!CHARACTER(len=MaxNameLength)                               :: curRecSurf
INTEGER                               :: curRecSurf
CHARACTER(len=2000)                                        :: listOfSurf
INTEGER :: found
INTEGER :: iShadRel
INTEGER :: jUnique
INTEGER :: iKindRec
INTEGER :: numreceivingfields
INTEGER :: HTS
INTEGER :: NGSS

!displaySurfaceShadowing=.false.  for debugging
IF (displaySurfaceShadowing) THEN
   numreceivingfields=0
   DO HTS=1,TotSurfaces
    DO NGSS=1,ShadowComb(HTS)%NumGenSurf
      numreceivingfields=numreceivingfields+1
    ENDDO
    DO NGSS=1,ShadowComb(HTS)%NumSubSurf
      numreceivingfields=numreceivingfields+1
    ENDDO
  ENDDO

  ALLOCATE(ShadowRelate(numreceivingfields))
  numShadowRelate=0
  DO HTS=1,TotSurfaces
    DO NGSS=1,ShadowComb(HTS)%NumGenSurf
      numShadowRelate=numShadowRelate+1
      ShadowRelate(numShadowRelate)%castSurf =   ShadowComb(HTS)%GenSurf(NGSS)
      ShadowRelate(numShadowRelate)%recSurf =    HTS
      ShadowRelate(numShadowRelate)%recKind =    recKindSurface
    ENDDO
    DO NGSS=1,ShadowComb(HTS)%NumSubSurf
      numShadowRelate=numShadowRelate+1
      ShadowRelate(numShadowRelate)%castSurf =   ShadowComb(HTS)%SubSurf(NGSS)
      ShadowRelate(numShadowRelate)%recSurf =    HTS
      ShadowRelate(numShadowRelate)%recKind =    recKindSubsurface
    ENDDO
  ENDDO

  CALL WriteReportHeaders('Surface Shadowing Summary','Entire Facility',isAverage)
  ALLOCATE(unique(numShadowRelate))
  !do entire process twice, once with surfaces receiving, once with subsurfaces receiving
  DO iKindRec = recKindSurface,recKindSubsurface
    numUnique = 0
    !first find the number of unique
    DO iShadRel = 1, numShadowRelate
      IF (ShadowRelate(iShadRel)%recKind .EQ. iKindRec) THEN
        curRecSurf = ShadowRelate(iShadRel)%recSurf
        found = 0
        DO jUnique = 1, numUnique
          IF (curRecSurf .EQ. unique(jUnique)) THEN
            found = jUnique
            EXIT
          END IF
        END DO
        IF (found .EQ. 0) THEN
          numUnique = numUnique + 1
          unique(numUnique) = curRecSurf
        END IF
      END IF
    END DO
    ALLOCATE(rowHead(numUnique))
    ALLOCATE(columnHead(1))
    ALLOCATE(columnWidth(1))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(numUnique,1))
    columnHead(1) = 'Possible Shadow Receivers'
    IF (numUnique == 0) columnHead(1) = 'None'
    DO jUnique = 1,numUnique
      curRecSurf = unique(jUnique)
      rowHead(jUnique) = Surface(curRecSurf)%Name
      listOfSurf = ''
      DO iShadRel = 1, numShadowRelate
        IF (ShadowRelate(iShadRel)%recKind .EQ. iKindRec) THEN
          IF (curRecSurf .EQ. ShadowRelate(iShadRel)%recSurf) THEN
            listOfSurf = TRIM(listOfSurf) // TRIM(Surface(ShadowRelate(iShadRel)%castSurf)%Name) // ' | ' !'<br>'
          END IF
        END IF
      END DO
      tableBody(jUnique,1) = listOfSurf
    END DO
    !write the table
    SELECT CASE (iKindRec)
      CASE (recKindSurface)
        CALL writeSubtitle('Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces ')
        CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                            'SurfaceShadowingSummary',&
                                            'Entire Facility',&
                                            'Surfaces (Walls, Roofs, etc) that may be Shadowed by Other Surfaces ')
      CASE (recKindSubsurface)
        CALL writeSubtitle('Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces ')
        CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                            'SurfaceShadowingSummary',&
                                            'Entire Facility',&
                                            'Subsurfaces (Windows and Doors) that may be Shadowed by Surfaces ')
    END SELECT
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    !deallocate these arrays since they are used to create the next
    !table
    DEALLOCATE(rowHead)
    DEALLOCATE(columnHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)
  END DO
  DEALLOCATE(unique)
END IF
END SUBROUTINE WriteSurfaceShadowing


SUBROUTINE AddTOCZoneLoadComponentTable
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Add the table of contents entries for the Zone heat transfer
          !   summary report.

          ! METHODOLOGY EMPLOYED:
          !   Call the AddTOCEntry routine for each zone.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

USE DataHeatBalance,   ONLY: Zone
USE DataZoneEquipment, ONLY: ZoneEquipConfig
USE DataGlobals, ONLY: CompLoadReportIsReq

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: iZone

IF (displayZoneComponentLoadSummary .AND. CompLoadReportIsReq) THEN
  DO iZone = 1, NumOfZones
    IF (.not. ZoneEquipConfig(iZone)%IsControlled) CYCLE
    CALL AddTOCEntry('Zone Component Load Summary',Zone(iZone)%Name)
  END DO
END IF
END SUBROUTINE AddTOCZoneLoadComponentTable

SUBROUTINE AllocateLoadComponentArrays
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Allocate the arrays related to the load component report

          ! METHODOLOGY EMPLOYED:
          !   Use the ALLOCATE command

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE DataSurfaces,      ONLY: TotSurfaces
USE DataEnvironment, ONLY: TotDesDays, TotRunDesPersDays
USE DataGlobals, ONLY: NumOfTimeStepInHour

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL, SAVE     :: DoAllocate = .TRUE.

IF (DoAllocate) THEN
    !For many of the following arrays the last dimension is the number of environments and is same as sizing arrays
  ALLOCATE(radiantPulseUsed(NumOfZones,0:TotDesDays+TotRunDesPersDays))
  radiantPulseUsed = 0.0d0
  ALLOCATE(radiantPulseTimestep(NumOfZones,0:TotDesDays+TotRunDesPersDays))
  radiantPulseTimestep = 0
  ALLOCATE(radiantPulseReceived(TotSurfaces,0:TotDesDays+TotRunDesPersDays))
  radiantPulseReceived = 0.0d0
  ALLOCATE(loadConvectedNormal(TotSurfaces,0:NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  loadConvectedNormal = 0.0d0
  ALLOCATE(loadConvectedWithPulse(TotSurfaces,0:NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  loadConvectedWithPulse = 0.0d0
  ALLOCATE(netSurfRadSeq(TotSurfaces,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  netSurfRadSeq = 0.0d0
  ALLOCATE(decayCurveCool(TotSurfaces,NumOfTimeStepInHour*24))
  decayCurveCool = 0.0d0
  ALLOCATE(decayCurveHeat(TotSurfaces,NumOfTimeStepInHour*24))
  decayCurveHeat = 0.0d0
  ALLOCATE(ITABSFseq(TotSurfaces,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  ITABSFseq = 0.0d0
  ALLOCATE(TMULTseq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  TMULTseq = 0.0d0
  ALLOCATE(peopleInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  peopleInstantSeq = 0.0d0
  ALLOCATE(peopleLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  peopleLatentSeq = 0.0d0
  ALLOCATE(peopleRadSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  peopleRadSeq = 0.0d0
  ALLOCATE(peopleDelaySeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  peopleDelaySeq = 0.0d0
  ALLOCATE(lightInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  lightInstantSeq = 0.0d0
  ALLOCATE(lightRetAirSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  lightRetAirSeq = 0.0d0
  ALLOCATE(lightLWRadSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  lightLWRadSeq = 0.0d0
  ALLOCATE(lightSWRadSeq(TotSurfaces,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  lightSWRadSeq = 0.0d0
  ALLOCATE(lightDelaySeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  lightLWRadSeq = 0.0d0
  ALLOCATE(equipInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  equipInstantSeq = 0.0d0
  ALLOCATE(equipLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  equipLatentSeq = 0.0d0
  ALLOCATE(equipRadSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  equipRadSeq = 0.0d0
  ALLOCATE(equipDelaySeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  equipDelaySeq = 0.0d0
  ALLOCATE(refrigInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  refrigInstantSeq = 0.0d0
  ALLOCATE(refrigRetAirSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  refrigRetAirSeq = 0.0d0
  ALLOCATE(refrigLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  refrigLatentSeq = 0.0d0
  ALLOCATE(waterUseInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  waterUseInstantSeq = 0.0d0
  ALLOCATE(waterUseLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  waterUseLatentSeq = 0.0d0
  ALLOCATE(hvacLossInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  hvacLossInstantSeq = 0.0d0
  ALLOCATE(hvacLossRadSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  hvacLossRadSeq = 0.0d0
  ALLOCATE(hvacLossDelaySeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  hvacLossDelaySeq = 0.0d0
  ALLOCATE(powerGenInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  powerGenInstantSeq = 0.0d0
  ALLOCATE(powerGenRadSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  powerGenRadSeq = 0.0d0
  ALLOCATE(powerGenDelaySeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  powerGenDelaySeq = 0.0d0
  ALLOCATE(infilInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  infilInstantSeq = 0.0d0
  ALLOCATE(infilLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  infilLatentSeq = 0.0d0
  ALLOCATE(zoneVentInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  zoneVentInstantSeq = 0.0d0
  ALLOCATE(zoneVentLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  zoneVentLatentSeq = 0.0d0
  ALLOCATE(interZoneMixInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  interZoneMixInstantSeq = 0.0d0
  ALLOCATE(interZoneMixLatentSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  interZoneMixLatentSeq = 0.0d0
  ALLOCATE(feneCondInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  feneCondInstantSeq = 0.0d0
!  ALLOCATE(feneSolarInstantSeq(NumOfZones,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
!  feneSolarInstantSeq = 0.0d0
  ALLOCATE(feneSolarRadSeq(TotSurfaces,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  feneSolarRadSeq = 0.0d0
  ALLOCATE(feneSolarDelaySeq(TotSurfaces,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  feneSolarDelaySeq = 0.0d0
  ALLOCATE(surfDelaySeq(TotSurfaces,NumOfTimeStepInHour*24,TotDesDays+TotRunDesPersDays))
  surfDelaySeq = 0.0d0
  DoAllocate = .FALSE.
END IF
END SUBROUTINE AllocateLoadComponentArrays

SUBROUTINE DeallocateLoadComponentArrays
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Deallocate the arrays related to the load component report that will not
          !   be needed in the reporting.

          ! METHODOLOGY EMPLOYED:
          !   Use the DEALLOCATE command

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE DataSurfaces,      ONLY: TotSurfaces
USE DataEnvironment, ONLY: TotDesDays, TotRunDesPersDays
USE DataGlobals, ONLY: NumOfTimeStepInHour

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

DEALLOCATE(radiantPulseUsed)
DEALLOCATE(radiantPulseTimestep)
DEALLOCATE(radiantPulseReceived)
!need for reporting  DEALLOCATE(loadConvectedNormal)
DEALLOCATE(loadConvectedWithPulse)
!need for reporting  DEALLOCATE(decayCurveCool)
!need for reporting  DEALLOCATE(decayCurveHeat)
END SUBROUTINE DeallocateLoadComponentArrays

SUBROUTINE ComputeLoadComponentDecayCurve

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines the load component decay curve based on normal and pulse results from zone sizing.

          ! METHODOLOGY EMPLOYED:
          ! Decay curve is the fraction of the heat convected from a surface over the initial radiant heat
          ! absorbed by the surface.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing, ONLY: CalcFinalZoneSizing
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataSurfaces, ONLY: Surface, TotSurfaces
  USE DataGlobals, ONLY: NumOfTimeStepInHour


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
INTEGER :: ZoneNum = 0
INTEGER :: SurfNum = 0
INTEGER :: TimeStep = 0
INTEGER :: TimeOfPulse  = 0
INTEGER :: CoolDesSelected = 0  !design day selected for cooling
INTEGER :: HeatDesSelected = 0  !design day selected for heating
INTEGER :: i
REAL(r64) :: diff

DO SurfNum = 1, TotSurfaces
  ZoneNum = Surface(SurfNum)%Zone
  IF (ZoneNum .EQ. 0) CYCLE
  IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
  CoolDesSelected = CalcFinalZoneSizing(ZoneNum)%CoolDDNum
  !loop over timesteps after pulse occured
  IF (CoolDesSelected .NE. 0) THEN
    TimeOfPulse = radiantPulseTimestep(ZoneNum,CoolDesSelected)
    ! if the CoolDesSelected time is on a different day than
    ! when the pulse occurred, need to scan back and find when
    ! the pulse occurred.
    IF (TimeOfPulse .EQ. 0) THEN
      DO i = CoolDesSelected, 1, -1
        TimeOfPulse = radiantPulseTimestep(ZoneNum,i)
        IF (TimeOfPulse .NE. 0) EXIT
      END DO
    END IF
    IF (TimeOfPulse == 0) TimeOfPulse=1
    DO TimeStep = TimeOfPulse, NumOfTimeStepInHour* 24
      IF (radiantPulseReceived(surfNum,CoolDesSelected) .NE. 0.0d0) THEN
        diff = loadConvectedWithPulse(surfNum,TimeStep,CoolDesSelected) &
                                  - loadConvectedNormal(surfNum,TimeStep,CoolDesSelected)
        decayCurveCool(surfNum, TimeStep - TimeOfPulse + 1) = -diff / radiantPulseReceived(surfNum,CoolDesSelected)
      ELSE
        decayCurveCool(surfNum, TimeStep - TimeOfPulse + 1) = 0.0d0
      END IF
    END DO
  END IF
  HeatDesSelected = CalcFinalZoneSizing(ZoneNum)%HeatDDNum
  IF (HeatDesSelected .NE. 0) THEN
    TimeOfPulse = radiantPulseTimestep(ZoneNum,HeatDesSelected)
    ! scan back to the day that the heating pulse occurs, if necessary
    IF (TimeOfPulse .EQ. 0) THEN
      DO i = HeatDesSelected, 1, -1
        TimeOfPulse = radiantPulseTimestep(ZoneNum,i)
        IF (TimeOfPulse .NE. 0) EXIT
      END DO
    END IF
    IF (TimeOfPulse == 0) TimeOfPulse=1
    DO TimeStep = TimeOfPulse, NumOfTimeStepInHour* 24
      IF (radiantPulseReceived(surfNum,HeatDesSelected) .NE. 0.0d0) THEN
        diff = loadConvectedWithPulse(surfNum,TimeStep,HeatDesSelected) &
                                  - loadConvectedNormal(surfNum,TimeStep,HeatDesSelected)
        decayCurveHeat(surfNum, TimeStep - TimeOfPulse + 1) = -diff  / radiantPulseReceived(surfNum,HeatDesSelected)
      ELSE
        decayCurveHeat(surfNum, TimeStep - TimeOfPulse + 1) = 0.0d0
      END IF
    END DO
  END IF
END DO
END SUBROUTINE ComputeLoadComponentDecayCurve

SUBROUTINE GatherComponentLoadsSurface
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gather values during sizing used for loads component report.

          ! METHODOLOGY EMPLOYED:
          !   Save sequence of values for report during sizing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE DataGlobals, ONLY: NumOfTimeStepInHour, CompLoadReportIsReq, isPulseZoneSizing
USE DataSizing, ONLY: CurOverallSimDay
USE DataSurfaces, ONLY: Surface, TotSurfaces, WinGainConvGlazToZoneRep,WinGainConvGlazShadGapToZoneRep, &
                            WinGainConvShadeToZoneRep, WinGainFrameDividerToZoneRep,SurfaceClass_Window
USE DataZoneEquipment, ONLY: ZoneEquipConfig


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iSurf = 0
INTEGER :: ZoneNum = 0
INTEGER :: TimeStepInDay = 0

IF (CompLoadReportIsReq .AND. .NOT. isPulseZoneSizing) THEN
  TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
  feneCondInstantSeq(:,TimeStepInDay,CurOverallSimDay) = 0.0d0
  DO iSurf = 1, TotSurfaces
    ZoneNum = Surface(iSurf)%Zone
    IF (ZoneNum .EQ. 0) CYCLE
    IF (Surface(iSurf)%Class /= SurfaceClass_Window) CYCLE
   ! IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
    feneCondInstantSeq(ZoneNum,TimeStepInDay,CurOverallSimDay) = feneCondInstantSeq(ZoneNum,TimeStepInDay,CurOverallSimDay) &
                                                + WinGainConvGlazToZoneRep(iSurf) &
                                                + WinGainConvGlazShadGapToZoneRep(iSurf) &
                                                + WinGainConvShadeToZoneRep(iSurf) &
                                                + WinGainFrameDividerToZoneRep(iSurf)
    ! for now assume zero instant solar - may change related
    ! to how blinds and shades absorb solar radiation and
    ! convect that heat that timestep.
    !feneSolarInstantSeq(ZoneNum,TimeStepInDay,CurOverallSimDay) = 0
  END DO
END IF
END SUBROUTINE GatherComponentLoadsSurface

SUBROUTINE GatherComponentLoadsHVAC
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Gather values during sizing used for loads component report.

          ! METHODOLOGY EMPLOYED:
          !   Save sequence of values for report during sizing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
USE DataGlobals, ONLY: NumOfTimeStepInHour, CompLoadReportIsReq, isPulseZoneSizing
USE DataSizing, ONLY: CurOverallSimDay
USE DataHeatBalance, ONLY: ZnAirRpt, RefrigCaseCredit
USE DataHVACGlobals, ONLY: TimeStepSys
USE DataAirflowNetwork,  ONLY : SimulateAirflowNetwork,AirflowNetworkControlSimple,AirflowNEtworkReportData


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iZone = 0
INTEGER :: TimeStepInDay = 0

IF (CompLoadReportIsReq .AND. .NOT. isPulseZoneSizing) THEN
  TimeStepInDay = (HourOfDay-1)*NumOfTimeStepInHour + TimeStep
  DO iZone = 1, NumOfZones
    infilInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) = ((ZnAirRpt(iZone)%InfilHeatGain &  !zone infiltration
                         - ZnAirRpt(iZone)%InfilHeatLoss) /(TimeStepSys * SecInHour))
    IF (SimulateAirflowNetwork .GT. AirflowNetworkControlSimple) THEN
      infilInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) = infilInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) &
                         + (AirflowNetworkReportData(iZone)%MultiZoneInfiSenGainW &                   !air flow network
                             -  AirflowNEtworkReportData(iZone)%MultiZoneInfiSenLossW)
    END IF
    infilLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) = ((ZnAirRpt(iZone)%InfilLatentGain &  !zone infiltration
                         - ZnAirRpt(iZone)%InfilLatentLoss) /(TimeStepSys * SecInHour))
    IF (SimulateAirflowNetwork .GT. AirflowNetworkControlSimple) THEN
      infilLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) = infilLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) &
                         + (AirflowNetworkReportData(iZone)%MultiZoneInfiLatGainW &                   !air flow network
                             -  AirflowNEtworkReportData(iZone)%MultiZoneInfiLatLossW)
    END IF

    zoneVentInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) = ((ZnAirRpt(iZone)%VentilHeatGain &  !zone ventilation
                         - ZnAirRpt(iZone)%VentilHeatLoss) /(TimeStepSys * SecInHour))
    zoneVentLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) = ((ZnAirRpt(iZone)%VentilLatentGain &  !zone ventilation
                         - ZnAirRpt(iZone)%VentilLatentLoss) /(TimeStepSys * SecInHour))

    interZoneMixInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) = ((ZnAirRpt(iZone)%MixHeatGain &  !zone mixing
                         - ZnAirRpt(iZone)%MixHeatLoss) /(TimeStepSys * SecInHour))
    IF (SimulateAirflowNetwork .GT. AirflowNetworkControlSimple) THEN
      interZoneMixInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) = interZoneMixInstantSeq(iZone,TimeStepInDay,CurOverallSimDay) &
                         + (AirflowNetworkReportData(iZone)%MultiZoneMixSenGainW &                   !air flow network
                             -  AirflowNEtworkReportData(iZone)%MultiZoneMixSenLossW)
    END IF
    interZoneMixLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) = ((ZnAirRpt(iZone)%MixLatentGain &  !zone mixing
                         - ZnAirRpt(iZone)%MixLatentLoss) /(TimeStepSys * SecInHour))
    IF (SimulateAirflowNetwork .GT. AirflowNetworkControlSimple) THEN
      interZoneMixLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) = interZoneMixLatentSeq(iZone,TimeStepInDay,CurOverallSimDay) &
                         + (AirflowNetworkReportData(iZone)%MultiZoneMixLatGainW &                   !air flow network
                             -  AirflowNEtworkReportData(iZone)%MultiZoneMixLatLossW)
    END IF
  END DO
END IF
END SUBROUTINE GatherComponentLoadsHVAC

SUBROUTINE ComputeDelayedComponents
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   September 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   For load component report, convert the sequence of radiant gains
          !   for people and equipment and other internal loads into convective
          !   gains based on the decay curves.

          ! METHODOLOGY EMPLOYED:
          !   For each step of sequence from each design day, compute the
          !   contributations from previous timesteps multiplied by the decay
          !   curve. Rather than store every internal load's radiant contribution
          !   to each surface, the TMULT and ITABSF sequences were also stored
          !   which allocates the total radiant to each surface in the zone. The
          !   formula used is:
          !       QRadThermInAbs(SurfNum) = QL(NZ) * TMULT(NZ) * ITABSF(SurfNum)


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataSurfaces,      ONLY: Surface, TotSurfaces,SurfaceClass_Window
USE DataEnvironment, ONLY: TotDesDays, TotRunDesPersDays
USE DataGlobals, ONLY: NumOfTimeStepInHour
USE DataSizing, ONLY: CalcFinalZoneSizing,NumTimeStepsInAvg
USE DataZoneEquipment, ONLY: ZoneEquipConfig

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

INTEGER :: iZone
INTEGER :: jSurf
INTEGER :: kTimeStep
INTEGER :: lDesHtCl
INTEGER :: mStepBack
INTEGER :: desSelected
LOGICAL :: isCooling
REAL(r64) :: QRadThermInAbsMult
REAL(r64) :: peopleConvFromSurf
REAL(r64) :: peopleConvIntoZone
REAL(r64),DIMENSION(:),ALLOCATABLE :: peopleRadIntoSurf
REAL(r64) :: equipConvFromSurf
REAL(r64) :: equipConvIntoZone
REAL(r64),DIMENSION(:),ALLOCATABLE :: equipRadIntoSurf
REAL(r64) :: hvacLossConvFromSurf
REAL(r64) :: hvacLossConvIntoZone
REAL(r64),DIMENSION(:),ALLOCATABLE :: hvacLossRadIntoSurf
REAL(r64) :: powerGenConvFromSurf
REAL(r64) :: powerGenConvIntoZone
REAL(r64),DIMENSION(:),ALLOCATABLE :: powerGenRadIntoSurf
REAL(r64) :: lightLWConvFromSurf
REAL(r64) :: lightLWConvIntoZone
REAL(r64),DIMENSION(:),ALLOCATABLE :: lightLWRadIntoSurf
REAL(r64) :: lightSWConvFromSurf
REAL(r64) :: lightSWConvIntoZone
REAL(r64) :: feneSolarConvFromSurf
REAL(r64) :: feneSolarConvIntoZone
REAL(r64) :: adjFeneSurfNetRadSeq

ALLOCATE(peopleRadIntoSurf(NumOfTimeStepInHour*24))
ALLOCATE(equipRadIntoSurf(NumOfTimeStepInHour*24))
ALLOCATE(hvacLossRadIntoSurf(NumOfTimeStepInHour*24))
ALLOCATE(powerGenRadIntoSurf(NumOfTimeStepInHour*24))
ALLOCATE(lightLWRadIntoSurf(NumOfTimeStepInHour*24))
! deallocate after writing?  LKL
IF (ALLOCATED(CalcFinalZoneSizing)) THEN
  DO lDesHtCl = 1,2 !iterates between heating and cooling design day
    isCooling = lDesHtCl .EQ. 2  !flag for when cooling design day otherwise heating design day
    DO iZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(iZone)%IsControlled) CYCLE
      IF (isCooling) THEN
        desSelected = CalcFinalZoneSizing(iZone)%CoolDDNum
      ELSE
        desSelected = CalcFinalZoneSizing(iZone)%HeatDDNum
      END IF
      IF (desSelected .EQ. 0)  CYCLE
      DO kTimeStep = 1, NumOfTimeStepInHour*24
        peopleConvIntoZone = 0.0d0
        equipConvIntoZone = 0.0d0
        hvacLossConvIntoZone = 0.0d0
        powerGenConvIntoZone = 0.0d0
        lightLWConvIntoZone = 0.0d0
        lightSWConvIntoZone = 0.0d0
        feneSolarConvIntoZone = 0.0d0
        adjFeneSurfNetRadSeq = 0.0d0
        DO jSurf = 1,TotSurfaces
          IF (.NOT. Surface(jSurf)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces
          IF (Surface(jSurf)%Zone .EQ. iZone) THEN
            !determine for each timestep the amount of radiant heat for each end use absorbed in each surface
            QRadThermInAbsMult = TMULTseq(iZone,kTimeStep,desSelected) * ITABSFseq(jSurf,kTimeStep,desSelected)  &
                                 * Surface(jSurf)%area
            peopleRadIntoSurf(kTimeStep) = peopleRadSeq(iZone,kTimeStep,DesSelected) * QRadThermInAbsMult
            equipRadIntoSurf(kTimeStep) = equipRadSeq(iZone,kTimeStep,DesSelected) * QRadThermInAbsMult
            hvacLossRadIntoSurf(kTimeStep) = hvacLossRadSeq(iZone,kTimeStep,DesSelected) * QRadThermInAbsMult
            powerGenRadIntoSurf(kTimeStep) = powerGenRadSeq(iZone,kTimeStep,DesSelected) * QRadThermInAbsMult
            lightLWRadIntoSurf(kTimeStep) = lightLWRadSeq(iZone,kTimeStep,DesSelected) * QRadThermInAbsMult
            !for each time step, step back through time and apply decay curve
            peopleConvFromSurf = 0.0d0
            equipConvFromSurf = 0.0d0
            hvacLossConvFromSurf = 0.0d0
            powerGenConvFromSurf = 0.0d0
            lightLWConvFromSurf = 0.0d0
            lightSWConvFromSurf = 0.0d0
            feneSolarConvFromSurf = 0.0d0
            DO mStepBack = 1,kTimeStep
              IF (isCooling) THEN
                peopleConvFromSurf = peopleConvFromSurf + peopleRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveCool(jSurf,mStepBack)
                equipConvFromSurf = equipConvFromSurf + equipRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveCool(jSurf,mStepBack)
                hvacLossConvFromSurf = hvacLossConvFromSurf + hvacLossRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveCool(jSurf,mStepBack)
                powerGenConvFromSurf = powerGenConvFromSurf + powerGenRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveCool(jSurf,mStepBack)
                lightLWConvFromSurf = lightLWConvFromSurf + lightLWRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveCool(jSurf,mStepBack)
                ! short wave is already accumulated by surface
                lightSWConvFromSurf = lightSWConvFromSurf + lightSWRadSeq(jSurf,kTimeStep - mStepBack + 1,DesSelected) &
                                       * decayCurveCool(jSurf,mStepBack)
                feneSolarConvFromSurf = feneSolarConvFromSurf + feneSolarRadSeq(jSurf,kTimeStep - mStepBack + 1,DesSelected) &
                                       * decayCurveCool(jSurf,mStepBack)
              ELSE
                peopleConvFromSurf = peopleConvFromSurf + peopleRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveHeat(jSurf,mStepBack)
                equipConvFromSurf = equipConvFromSurf + equipRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveHeat(jSurf,mStepBack)
                hvacLossConvFromSurf = hvacLossConvFromSurf + hvacLossRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveHeat(jSurf,mStepBack)
                powerGenConvFromSurf = powerGenConvFromSurf + powerGenRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveHeat(jSurf,mStepBack)
                lightLWConvFromSurf = lightLWConvFromSurf + lightLWRadIntoSurf(kTimeStep - mStepBack + 1) &
                                       * decayCurveHeat(jSurf,mStepBack)
                ! short wave is already accumulated by surface
                lightSWConvFromSurf = lightSWConvFromSurf + lightSWRadSeq(jSurf, kTimeStep - mStepBack + 1,DesSelected) &
                                       * decayCurveHeat(jSurf,mStepBack)
                feneSolarConvFromSurf = feneSolarConvFromSurf + feneSolarRadSeq(jSurf,kTimeStep - mStepBack + 1,DesSelected) &
                                       * decayCurveHeat(jSurf,mStepBack)
              ENDIF
            END DO
            peopleConvIntoZone = peopleConvIntoZone + peopleConvFromSurf
            equipConvIntoZone = equipConvIntoZone + equipConvFromSurf
            hvacLossConvIntoZone = hvacLossConvIntoZone + hvacLossConvFromSurf
            powerGenConvIntoZone = powerGenConvIntoZone + powerGenConvFromSurf
            lightLWConvIntoZone = lightLWConvIntoZone + lightLWConvFromSurf
            lightSWConvIntoZone = lightSWConvIntoZone + lightSWConvFromSurf
            feneSolarConvIntoZone = feneSolarConvIntoZone + feneSolarConvFromSurf
            ! determine the remaining convective heat from the surfaces that are not based
            ! on any of these other loads
            !negative because heat from surface should be positive
            surfDelaySeq(jSurf,kTimeStep,desSelected) = -loadConvectedNormal(jSurf,kTimeStep,desSelected) &
                 - netSurfRadSeq(jSurf,kTimeStep,desSelected) &         !remove net radiant for the surface
                 - (peopleConvFromSurf + equipConvFromSurf + hvacLossConvFromSurf + powerGenConvFromSurf &
                     + lightLWConvFromSurf + lightSWConvFromSurf + feneSolarConvFromSurf)
            ! also remove the net radiant component on the instanteous conduction for fenestration
            IF (surface(jSurf)%class .EQ. SurfaceClass_Window) THEN
              adjFeneSurfNetRadSeq = adjFeneSurfNetRadSeq + netSurfRadSeq(jSurf,kTimeStep,desSelected)
            END IF
          END IF
        END DO
        peopleDelaySeq(iZone,kTimeStep,desSelected) = peopleConvIntoZone
        equipDelaySeq(iZone,kTimeStep,desSelected) = equipConvIntoZone
        hvacLossDelaySeq(iZone,kTimeStep,desSelected) = hvacLossConvIntoZone
        powerGenDelaySeq(iZone,kTimeStep,desSelected) = powerGenConvIntoZone
        !combine short wave (visible) and long wave (thermal) impacts
        lightDelaySeq(iZone,kTimeStep,desSelected) = lightLWConvIntoZone + lightSWConvIntoZone
        feneSolarDelaySeq(iZone,kTimeStep,desSelected) = feneSolarConvIntoZone
        ! also remove the net radiant component on the instanteous conduction for fenestration
        feneCondInstantSeq(iZone,kTimeStep,desSelected) = feneCondInstantSeq(iZone,kTimeStep,desSelected) - adjFeneSurfNetRadSeq
      END DO
    END DO
  END DO
ENDIF
END SUBROUTINE ComputeDelayedComponents

SUBROUTINE WriteZoneLoadComponentTable
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Write the tables for the ZoneLoadComponentSummary and
          !   ZoneLoadComponentDetail reports which summarize the major
          !   load components for each zone in the building.

          ! METHODOLOGY EMPLOYED:
          !   Create arrays for the call to writeTable and then call it.
          !   This report actually consists of many sub-tables each with
          !   its own call to writeTable.
!
! The overall methodology is explained below:
!
! Determine decay curve - Pulse of radiant heat which is about 5% of lighting and
! equipment input [radiantPulseUsed(iZone)] for a single timestep a few hours after
! cooling or heat is scheduled on for each zone [radiantPulseTimestep(iZone)].
! The radiant heat received on each wall is stored [radiantPulseReceived(jSurface)].
! The load convected in the normal case [loadConvectedNormal(jSurface, kTime, mode)]
! and in the case with the pulse [loadConvectedWithPulse(jSurface, kTime, mode)].
! The difference divided by the pulse received by each surface
! [radiantPulseReceived(jSurface)] is stored in [decayCurve(jSurface,kTime,mode)].
!
! Determine delayed loads - From the last timestep of the peak load on the zone
! working backwards any radiant heat that was absorbed by the wall from an internal gain
! or solar gain is multiplied by the appropriate timesteps in the decay curve
! [decayCurve(jSurface,kTime,mode)] for timesteps that make up
! the number of averaged timesteps are used to determine the peak load
! [NumTimeStepsInAvg]. The sum for all surfaces in the zone are added together to
! determine the delayed load.
!
! Determine instant loads - Average the convective portion of the internal gains
! for the timesteps made up of the peak load period. Average those across the peak
! load period.
!
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

USE DataHeatBalance,   ONLY: Zone
USE SQLiteProcedures, ONLY: CreateSQLiteTabularDataRecords
USE DataZoneEquipment, ONLY: ZoneEquipConfig
USE DataSurfaces, ONLY: Surface, TotSurfaces,ExternalEnvironment,Ground,GroundFCfactorMethod, &
                        OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt, &
                        SurfaceClass_Wall,SurfaceClass_Floor,SurfaceClass_Roof, &
                        SurfaceClass_Door,OSC
USE DataSizing, ONLY: CalcFinalZoneSizing,NumTimeStepsInAvg,CoolPeakDateHrMin,HeatPeakDateHrMin
USE DataZoneEquipment, ONLY: ZoneEquipConfig
USE DataGlobals, ONLY: NumOfTimeStepInHour, CompLoadReportIsReq,ShowDecayCurvesInEIO
USE General, ONLY: MovingAvg
USE Psychrometrics,  ONLY: PsyTwbFnTdbWPb,PsyRhFnTdbWPb


IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:

! These correspond to the columns in the load component table
INTEGER, PARAMETER :: cSensInst = 1
INTEGER, PARAMETER :: cSensDelay = 2
INTEGER, PARAMETER :: cSensRA = 3
INTEGER, PARAMETER :: cLatent = 4
INTEGER, PARAMETER :: cTotal = 5
INTEGER, PARAMETER :: cPerc = 6

!internal gains
INTEGER, PARAMETER :: rPeople =     1
INTEGER, PARAMETER :: rLights =     2
INTEGER, PARAMETER :: rEquip =      3
INTEGER, PARAMETER :: rRefrig =     4
INTEGER, PARAMETER :: rWaterUse =   5
INTEGER, PARAMETER :: rHvacLoss =   6
INTEGER, PARAMETER :: rPowerGen =   7
!misc
INTEGER, PARAMETER :: rInfil =      8
INTEGER, PARAMETER :: rZoneVent =   9
INTEGER, PARAMETER :: rIntZonMix =  10
!opaque surfaces
INTEGER, PARAMETER :: rRoof =       11
INTEGER, PARAMETER :: rIntZonCeil = 12
INTEGER, PARAMETER :: rOtherRoof =  13
INTEGER, PARAMETER :: rExtWall =    14
INTEGER, PARAMETER :: rIntZonWall = 15
INTEGER, PARAMETER :: rGrdWall =    16
INTEGER, PARAMETER :: rOtherWall =  17
INTEGER, PARAMETER :: rExtFlr =     18
INTEGER, PARAMETER :: rIntZonFlr =  19
INTEGER, PARAMETER :: rGrdFlr =     20
INTEGER, PARAMETER :: rOtherFlr =   21
!subsurfaces
INTEGER, PARAMETER :: rFeneCond =   22
INTEGER, PARAMETER :: rFeneSolr =   23
INTEGER, PARAMETER :: rOpqDoor =    24
!total
INTEGER, PARAMETER :: rGrdTot =     25

          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: CoolDesSelected = 0  !design day selected for cooling
INTEGER :: HeatDesSelected = 0  !design day selected for heating
INTEGER :: timeCoolMax = 0 !Time Step at Cool max
INTEGER :: timeHeatMax = 0 !Time Step at Heat Max
INTEGER :: iZone = 0
INTEGER :: jTime = 0
INTEGER :: k = 0
INTEGER :: kSurf = 0
INTEGER :: numObj = 0
INTEGER :: objCount = 0
INTEGER :: ZoneNum = 0
INTEGER :: tempUnitConvIndex = 0
REAL(r64),ALLOCATABLE, DIMENSION(:) :: seqData  !raw data sequence that has not been averaged yet
REAL(r64),ALLOCATABLE, DIMENSION(:) :: avgData  !sequence data after averaging
INTEGER :: NumOfTimeStepInDay
REAL(r64),ALLOCATABLE, DIMENSION(:) :: delayOpaque !hold values for report for delayed opaque
REAL(r64) :: singleSurfDelay
REAL(r64),ALLOCATABLE, DIMENSION(:) :: totalColumn
REAL(r64),ALLOCATABLE, DIMENSION(:) :: percentColumn
REAL(r64),ALLOCATABLE, DIMENSION(:) :: grandTotalRow
REAL(r64) :: totalGrandTotal
REAL(r64) :: powerConversion
INTEGER :: tempConvIndx !temperature conversion index
CHARACTER(len=MaxNameLength) :: stringWithTemp
INTEGER :: curExtBoundCond
REAL(r64) :: mult !zone multiplier

! all arrays are in the format: (row, column)
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: columnHead
INTEGER,ALLOCATABLE,DIMENSION(:)                           :: columnWidth
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:)     :: rowHead
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:,:)   :: tableBody

IF (displayZoneComponentLoadSummary .AND. CompLoadReportIsReq) THEN
  CALL ComputeDelayedComponents
  NumOfTimeStepInDay = NumOfTimeStepInHour*24
  ALLOCATE(seqData(NumOfTimeStepInDay))
  ALLOCATE(avgData(NumOfTimeStepInDay))
  ALLOCATE(delayOpaque(rGrdTot))
  ALLOCATE(totalColumn(rGrdTot))
  ALLOCATE(percentColumn(rGrdTot))
  ALLOCATE(grandTotalRow(cPerc))

  !establish unit conversion factors
  IF (unitsStyle .EQ. unitsStyleInchPound) THEN
    powerConversion = getSpecificUnitMultiplier('W','Btu/h') !or kBtuh?
    tempConvIndx = getSpecificUnitIndex('C','F')
  ELSE
    powerConversion = 1.0d0
    tempConvIndx = 0 !when zero is used with ConvertIP the value is returned unconverted
  END IF

  ! show the line definition for the decay curves
  IF (ShowDecayCurvesInEIO) THEN
    WRITE (OutputFileInits, '(A)') '! <Radiant to Convective Decay Curves for Cooling>,Zone Name, Surface Name, Time ' &
                  // '1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36'
    WRITE (OutputFileInits, '(A)') '! <Radiant to Convective Decay Curves for Heating>,Zone Name, Surface Name, Time ' &
                  // '1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36'
  END IF

  DO iZone = 1, NumOfZones
    IF (.not. ZoneEquipConfig(iZone)%IsControlled) CYCLE
    mult = Zone(iZone)%Multiplier * Zone(iZone)%ListMultiplier
    IF (mult .EQ. 0.0) mult = 1.0

    !
    !---- Cooling Peak Load Components Sub-Table
    !
    CALL WriteReportHeaders('Zone Component Load Summary',TRIM(Zone(iZone)%Name),isAverage)

    ALLOCATE(rowHead(rGrdTot))
    ALLOCATE(columnHead(cPerc))
    ALLOCATE(columnWidth(cPerc))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(rGrdTot,cPerc))

    IF (unitsStyle .NE. unitsStyleInchPound) THEN
      columnHead(cSensInst) = 'Sensible - Instant [W]'
      columnHead(cSensDelay) = 'Sensible - Delayed [W]'
      columnHead(cSensRA) = 'Sensible - Return Air [W]'
      columnHead(cLatent) = 'Latent [W]'
      columnHead(cTotal) = 'Total [W]'
      columnHead(cPerc) = '%Grand Total'
    ELSE
      columnHead(cSensInst) = 'Sensible - Instant [Btu/h]'
      columnHead(cSensDelay) = 'Sensible - Delayed [Btu/h]'
      columnHead(cSensRA) = 'Sensible - Return Air [Btu/h]'
      columnHead(cLatent) = 'Latent [Btu/h]'
      columnHead(cTotal) = 'Total [Btu/h]'
      columnHead(cPerc) = '%Grand Total'
    END IF

    !internal gains
    rowHead(rPeople) = 'People'
    rowHead(rLights) = 'Lights'
    rowHead(rEquip) = 'Equipment'
    rowHead(rRefrig) = 'Refrigeration Equipment'
    rowHead(rWaterUse) = 'Water Use Equipment'
    rowHead(rPowerGen) = 'Power Generation Equipment'
    rowHead(rHvacLoss) = 'HVAC Equipment Losses'
    rowHead(rRefrig) = 'Refrigeration'
    !misc
    rowHead(rInfil) = 'Infiltration'
    rowHead(rZoneVent) = 'Zone Ventilation'
    rowHead(rIntZonMix) = 'Interzone Mixing'
    !opaque surfaces
    rowHead(rRoof) = 'Roof'
    rowHead(rIntZonCeil) = 'Interzone Ceiling'
    rowHead(rOtherRoof) = 'Other Roof'
    rowHead(rExtWall) = 'Exterior Wall'
    rowHead(rIntZonWall) = 'Interzone Wall'
    rowHead(rGrdWall) = 'Ground Contact Wall'
    rowHead(rOtherWall) = 'Other Wall'
    rowHead(rExtFlr) = 'Exterior Floor'
    rowHead(rIntZonFlr) = 'Interzone Floor'
    rowHead(rGrdFlr) = 'Ground Contact Floor'
    rowHead(rOtherFlr) = 'Other Floor'
    !subsurfaces
    rowHead(rFeneCond) = 'Fenestration Conduction'
    rowHead(rFeneSolr) = 'Fenestration Solar'
    rowHead(rOpqDoor) = 'Opaque Door'
    rowHead(rGrdTot) = 'Grand Total'

    tableBody = ''
    totalColumn = 0.0d0
    percentColumn = 0.0d0
    grandTotalRow = 0.0d0

    CoolDesSelected = CalcFinalZoneSizing(iZone)%CoolDDNum
    timeCoolMax = CalcFinalZoneSizing(iZone)%TimeStepNumAtCoolMax
    IF (CoolDesSelected .NE. 0 .AND. timeCoolMax .NE. 0) THEN

      !PEOPLE
      seqData = peopleInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPeople,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rPeople) = totalColumn(rPeople) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = peopleLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPeople,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rPeople) = totalColumn(rPeople) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      seqData = peopleDelaySeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPeople,cSensDelay)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rPeople) = totalColumn(rPeople) + AvgData(timeCoolMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeCoolMax)

      !LIGHTS
      seqData = lightInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rLights,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rLights) = totalColumn(rLights) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = lightRetAirSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rLights,cSensRA)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rLights) = totalColumn(rLights) + AvgData(timeCoolMax)
      grandTotalRow(cSensRA) = grandTotalRow(cSensRA) + AvgData(timeCoolMax)

      seqData = lightDelaySeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rLights,cSensDelay)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rLights) = totalColumn(rLights) + AvgData(timeCoolMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeCoolMax)

      !EQUIPMENT
      seqData = equipInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rEquip,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rEquip) = totalColumn(rEquip) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = equipLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rEquip,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rEquip) = totalColumn(rEquip) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      seqData = equipDelaySeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rEquip,cSensDelay)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rEquip) = totalColumn(rEquip) + AvgData(timeCoolMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeCoolMax)

      !REFRIGERATION EQUIPMENT
      seqData = refrigInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rRefrig,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rRefrig) = totalColumn(rRefrig) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = refrigRetAirSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rRefrig,cSensRA)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rRefrig) = totalColumn(rRefrig) + AvgData(timeCoolMax)
      grandTotalRow(cSensRA) = grandTotalRow(cSensRA) + AvgData(timeCoolMax)

      seqData = refrigLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rRefrig,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rRefrig) = totalColumn(rRefrig) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      !WATER USE EQUIPMENT
      seqData = waterUseInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rWaterUse,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rWaterUse) = totalColumn(rWaterUse) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = waterUseLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rWaterUse,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rWaterUse) = totalColumn(rWaterUse) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      !HVAC EQUIPMENT LOSSES
      seqData = hvacLossInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rHvacLoss,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rHvacLoss) = totalColumn(rHvacLoss) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = hvacLossDelaySeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rHvacLoss,cSensDelay)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rHvacLoss) = totalColumn(rHvacLoss) + AvgData(timeCoolMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeCoolMax)

      !POWER GENERATION EQUIPMENT
      seqData = powerGenInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPowerGen,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rPowerGen) = totalColumn(rPowerGen) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = powerGenDelaySeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPowerGen,cSensDelay)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rPowerGen) = totalColumn(rPowerGen) + AvgData(timeCoolMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeCoolMax)

      !INFILTRATION
      seqData = infilInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rInfil,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rInfil) = totalColumn(rInfil) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = infilLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rInfil,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rInfil) = totalColumn(rInfil) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      !ZONE VENTILATION
      seqData = zoneVentInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rZoneVent,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rZoneVent) = totalColumn(rZoneVent) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = zoneVentLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rZoneVent,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rZoneVent) = totalColumn(rZoneVent) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      !INTERZONE MIXING
      seqData = interZoneMixInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rIntZonMix,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rIntZonMix) = totalColumn(rIntZonMix) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = interZoneMixLatentSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rIntZonMix,cLatent)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rIntZonMix) = totalColumn(rIntZonMix) + AvgData(timeCoolMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeCoolMax)

      !FENESTRATION CONDUCTION
      seqData = feneCondInstantSeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rFeneCond,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rFeneCond) = totalColumn(rFeneCond) + AvgData(timeCoolMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      !FENESTRATION SOLAR
!      seqData = feneSolarInstantSeq(iZone,:,CoolDesSelected) * powerConversion
!      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
!      tableBody(rFeneSolr,cSensInst)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
!      totalColumn(rFeneSolr) = totalColumn(rFeneSolr) + AvgData(timeCoolMax)
!      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeCoolMax)

      seqData = feneSolarDelaySeq(iZone,:,CoolDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rFeneSolr,cSensDelay)  = TRIM(RealToStr(AvgData(timeCoolMax),2))
      totalColumn(rFeneSolr) = totalColumn(rFeneSolr) + AvgData(timeCoolMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeCoolMax)

      !opaque surfaces - must combine individual surfaces by class and other side conditions
      delayOpaque = 0.0d0
      DO kSurf = 1,TotSurfaces
        IF (.NOT. Surface(kSurf)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces
        IF (Surface(kSurf)%Zone .EQ. iZone) THEN
          curExtBoundCond = surface(kSurf)%ExtBoundCond
          !if exterior is other side coefficients using ground preprocessor terms then
          !set it to ground instead of other side coefficients
          IF (curExtBoundCond .EQ. OtherSideCoefNoCalcExt .OR. curExtBoundCond .EQ. OtherSideCoefCalcExt) THEN
            IF (SameString(OSC(Surface(kSurf)%OSCPtr)%Name(1:17), 'surfPropOthSdCoef')) THEN
              curExtBoundCond = Ground
            END IF
          END IF
          seqData = surfDelaySeq(kSurf,:,CoolDesSelected)
          CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
          singleSurfDelay = AvgData(timeCoolMax) * powerConversion
          SELECT CASE (surface(kSurf)%class)
            CASE (SurfaceClass_Wall)
              SELECT CASE (curExtBoundCond)
                CASE (ExternalEnvironment)
                  delayOpaque(rExtWall) = delayOpaque(rExtWall) + singleSurfDelay
                CASE (Ground,GroundFCfactorMethod)
                  delayOpaque(rGrdWall) = delayOpaque(rGrdWall) + singleSurfDelay
                CASE (OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt)
                  delayOpaque(rOtherWall) = delayOpaque(rOtherWall) + singleSurfDelay
                CASE DEFAULT !interzone
                  delayOpaque(rIntZonWall) = delayOpaque(rIntZonWall) + singleSurfDelay
              END SELECT
            CASE (SurfaceClass_Floor)
              SELECT CASE (curExtBoundCond)
                CASE (ExternalEnvironment)
                  delayOpaque(rExtFlr) = delayOpaque(rExtFlr) + singleSurfDelay
                CASE (Ground,GroundFCfactorMethod)
                  delayOpaque(rGrdFlr) = delayOpaque(rGrdFlr) + singleSurfDelay
                CASE (OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt)
                  delayOpaque(rOtherFlr) = delayOpaque(rOtherFlr) + singleSurfDelay
                CASE DEFAULT !interzone
                  delayOpaque(rIntZonFlr) = delayOpaque(rIntZonFlr) + singleSurfDelay
              END SELECT
            CASE (SurfaceClass_Roof)
              SELECT CASE (curExtBoundCond)
                CASE (ExternalEnvironment)
                  delayOpaque(rRoof) = delayOpaque(rRoof) + singleSurfDelay
                CASE (Ground,GroundFCfactorMethod,OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt)
                  delayOpaque(rOtherRoof) = delayOpaque(rOtherRoof) + singleSurfDelay
                CASE DEFAULT !interzone
                  delayOpaque(rIntZonCeil) = delayOpaque(rIntZonCeil) + singleSurfDelay
              END SELECT
            CASE (SurfaceClass_Door)
              delayOpaque(rOpqDoor) = delayOpaque(rOpqDoor) + singleSurfDelay
          END SELECT
        END IF
      END DO
      DO k = rRoof,rOtherFlr
        tableBody(k,cSensDelay)  = TRIM(RealToStr(delayOpaque(k),2))
        totalColumn(k) = totalColumn(k) + delayOpaque(k)
        grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + delayOpaque(k)
      END DO
      tableBody(rOpqDoor,cSensDelay) = TRIM(RealToStr(delayOpaque(rOpqDoor),2))
      totalColumn(rOpqDoor) = totalColumn(rOpqDoor) + delayOpaque(rOpqDoor)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + delayOpaque(rOpqDoor)
    END IF

    !GRAND TOTAL ROW
    totalGrandTotal = 0.0d0
    DO k = 1,cLatent
      tableBody(rGrdTot,k)  = TRIM(RealToStr(grandTotalRow(k),2))
      totalGrandTotal = totalGrandTotal + grandTotalRow(k)
    END DO
    tableBody(rGrdTot,cTotal)  = TRIM(RealToStr(totalGrandTotal,2))

    !TOTAL COLUMN AND PERCENT COLUMN
    DO k = 1,rOpqDoor !to last row before total
      tableBody(k,cTotal) = TRIM(RealToStr(totalColumn(k),2))
      IF (totalGrandTotal .NE. 0.0d0) THEN
        tableBody(k,cPerc) = TRIM(RealToStr(100 * totalColumn(k)/totalGrandTotal,2))
      END IF
    END DO

    CALL writeSubtitle('Estimated Cooling Peak Load Components')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'ZoneComponentLoadSummary',&
                                        TRIM(Zone(iZone)%Name),&
                                        'Estimated Cooling Peak Load Components')

    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)

    !
    !---- Cooling Peak Conditions
    !

    ALLOCATE(rowHead(10))
    ALLOCATE(columnHead(1))
    ALLOCATE(columnWidth(1))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(10,1))

    columnHead(1) = 'Value'
    IF (unitsStyle .NE. unitsStyleInchPound) THEN
      rowHead(1) = 'Time of Peak Load'
      rowHead(2) = 'Outside  Dry Bulb Temperature [C]'
      rowHead(3) = 'Outside  Wet Bulb Temperature [C]'
      rowHead(4) = 'Outside Humidity Ratio at Peak [kgWater/kgAir]'
      rowHead(5) = 'Zone Dry Bulb Temperature [C]'
      rowHead(6) = 'Zone Relative Humdity [%]'
      rowHead(7) = 'Zone Humidity Ratio at Peak [kgWater/kgAir]'
      rowHead(8) = 'Peak Design Sensible Load [W]'
      rowHead(9) = 'Estimated Instant + Delayed Sensible Load [W]'
      rowHead(10) = 'Difference [W]'
    ELSE
      rowHead(1) = 'Time of Peak Load'
      rowHead(2) = 'Outside  Dry Bulb Temperature [F]'
      rowHead(3) = 'Outside  Wet Bulb Temperature [F]'
      rowHead(4) = 'Outside Humidity Ratio at Peak [lbWater/lbAir]'
      rowHead(5) = 'Zone Dry Bulb Temperature [F]'
      rowHead(6) = 'Zone Relative Humdity [%]'
      rowHead(7) = 'Zone Humidity Ratio at Peak [lbWater/lbAir]'
      rowHead(8) = 'Peak Design Sensible Load [Btu/h]'
      rowHead(9) = 'Estimated Instant + Delayed Sensible Load [Btu/h]'
      rowHead(10) = 'Difference [Btu/h]'
    END IF

    tableBody = ''

    IF (timeCoolMax .NE. 0) THEN

      !Time of Peak Load
      tableBody(1,1) = TRIM(CoolPeakDateHrMin(iZone))

      !Outside  Dry Bulb Temperature
      tableBody(2,1) = TRIM(RealToStr(convertIP(tempConvIndx,CalcFinalZoneSizing(iZone)%CoolOutTempSeq(timeCoolMax)),2))

      !Outside  Wet Bulb Temperature
      !use standard sea level air pressure because air pressure is not tracked with sizing data
      IF (CalcFinalZoneSizing(iZone)%CoolOutHumRatSeq(timeCoolMax) .LT. 1.0d0 .AND.     &
          CalcFinalZoneSizing(iZone)%CoolOutHumRatSeq(timeCoolMax) .GT. 0.0d0) THEN
        tableBody(3,1) = TRIM(RealToStr(convertIP(tempConvIndx, &
                                     PsyTwbFnTdbWPb(CalcFinalZoneSizing(iZone)%CoolOutTempSeq(timeCoolMax), &
                                     CalcFinalZoneSizing(iZone)%CoolOutHumRatSeq(timeCoolMax), &
                                     101325.0d0))  ,2))
      END IF

      !Outside Humidity Ratio at Peak
      tableBody(4,1) = TRIM(RealToStr(CalcFinalZoneSizing(iZone)%CoolOutHumRatSeq(timeCoolMax),5))

      !Zone Dry Bulb Temperature
      tableBody(5,1) = TRIM(RealToStr(convertIP(tempConvIndx,CalcFinalZoneSizing(iZone)%CoolZoneTempSeq(timeCoolMax)),2))

      !Zone Relative Humdity
      !use standard sea level air pressure because air pressure is not tracked with sizing data
      tableBody(6,1) = TRIM(RealToStr(100 * PsyRhFnTdbWPb(CalcFinalZoneSizing(iZone)%CoolZoneTempSeq(timeCoolMax), &
                                                     CalcFinalZoneSizing(iZone)%CoolZoneHumRatSeq(timeCoolMax), &
                                                     101325.0d0)  ,2))

      !Zone Humidity Ratio at Peak
      tableBody(7,1) = TRIM(RealToStr(CalcFinalZoneSizing(iZone)%CoolZoneHumRatSeq(timeCoolMax),5))

    END IF

    !Peak Design Sensible Load
    tableBody(8,1) = TRIM(RealToStr((CalcFinalZoneSizing(iZone)%DesCoolLoad / mult) * powerConversion,2))

    !Estimated Instant + Delayed Sensible Load
    tableBody(9,1) = TRIM(RealToStr(grandTotalRow(cSensInst) + grandTotalRow(cSensDelay),2))

    !Difference
    tableBody(10,1) = TRIM(RealToStr((CalcFinalZoneSizing(iZone)%DesCoolLoad / mult) * powerConversion &
                         - (grandTotalRow(cSensInst) + grandTotalRow(cSensDelay)),2))

    CALL writeSubtitle('Cooling Peak Conditions')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'ZoneComponentLoadSummary',&
                                        TRIM(Zone(iZone)%Name),&
                                        'Cooling Peak Conditions')

    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)


!    !
!    !---- Radiant to Convective Decay Curves for Cooling
!    !
!    numObj = 0
!    !determine the number of surfaces to include
!    DO kSurf = 1, TotSurfaces
!      ZoneNum = Surface(kSurf)%Zone
!      IF (ZoneNum .NE. iZone) CYCLE
!      IF (ZoneNum .EQ. 0) CYCLE
!      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
!      numObj = numObj + 1
!    END DO
!
!    ALLOCATE(rowHead(numObj))
!    ALLOCATE(columnHead(16))
!    ALLOCATE(columnWidth(16))
!    columnWidth = 14 !array assignment - same for all columns
!    ALLOCATE(tableBody(numObj,16))
!
!    columnHead(1) = 'Time 1'
!    columnHead(2) = 'Time 2'
!    columnHead(3) = 'Time 3'
!    columnHead(4) = 'Time 4'
!    columnHead(5) = 'Time 5'
!    columnHead(6) = 'Time 6'
!    columnHead(7) = 'Time 7'
!    columnHead(8) = 'Time 8'
!    columnHead(9) = 'Time 9'
!    columnHead(10) = 'Time 10'
!    columnHead(11) = 'Time 11'
!    columnHead(12) = 'Time 12'
!    columnHead(13) = 'Time 13'
!    columnHead(14) = 'Time 14'
!    columnHead(15) = 'Time 15'
!    columnHead(16) = 'Time 16'
!
!    tableBody = ''
!    objCount = 0
!    DO kSurf = 1, TotSurfaces
!      ZoneNum = Surface(kSurf)%Zone
!      IF (ZoneNum .NE. iZone) CYCLE
!      IF (ZoneNum .EQ. 0) CYCLE
!      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
!      objCount = objCount + 1
!      rowHead(objCount) = TRIM(Surface(kSurf)%Name)
!      DO jTime = 1, 16
!        tableBody(objCount,jTime) = TRIM(RealToStr(decayCurveCool(kSurf,jTime),3))
!      END DO
!    END DO
!
!    CALL writeSubtitle('Radiant to Convective Decay Curves for Cooling')
!    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
!    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
!                                        'ZoneComponentLoadDetail',&
!                                        TRIM(Zone(iZone)%Name),&
!                                        'Radiant to Convective Decay Curves for Cooling')
!
!    DEALLOCATE(columnHead)
!    DEALLOCATE(rowHead)
!    DEALLOCATE(columnWidth)
!    DEALLOCATE(tableBody)

    ! Put the decay curve into the EIO file
    IF (ShowDecayCurvesInEIO) THEN
      DO kSurf = 1, TotSurfaces
        ZoneNum = Surface(kSurf)%Zone
        IF (ZoneNum .NE. iZone) CYCLE
        IF (ZoneNum .EQ. 0) CYCLE
        IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
        WRITE (OutputFileInits, '(4A)',ADVANCE='NO')  'Radiant to Convective Decay Curves for Cooling,' &
                            ,TRIM(Zone(iZone)%Name),',',TRIM(Surface(kSurf)%Name)
        DO jTime = 1, MIN(NumOfTimeStepInHour*24,36)
          WRITE(OutputFileInits,'(A,F6.3)',ADVANCE='NO') ',',decayCurveCool(kSurf,jTime)
        END DO
        WRITE(OutputFileInits,'()',ADVANCE='YES') !put a line feed at the end of the line
      END DO
    END IF

    !
    !---- Heating Peak Load Components Sub-Table
    !
    ALLOCATE(rowHead(rGrdTot))
    ALLOCATE(columnHead(cPerc))
    ALLOCATE(columnWidth(cPerc))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(rGrdTot,cPerc))

    IF (unitsStyle .NE. unitsStyleInchPound) THEN
      columnHead(cSensInst) = 'Sensible - Instant [W]'
      columnHead(cSensDelay) = 'Sensible - Delayed [W]'
      columnHead(cSensRA) = 'Sensible - Return Air [W]'
      columnHead(cLatent) = 'Latent [W]'
      columnHead(cTotal) = 'Total [W]'
      columnHead(cPerc) = '%Grand Total'
    ELSE
      columnHead(cSensInst) = 'Sensible - Instant [Btu/h]'
      columnHead(cSensDelay) = 'Sensible - Delayed [Btu/h]'
      columnHead(cSensRA) = 'Sensible - Return Air [Btu/h]'
      columnHead(cLatent) = 'Latent [Btu/h]'
      columnHead(cTotal) = 'Total [Btu/h]'
      columnHead(cPerc) = '%Grand Total'
    END IF

    !internal gains
    rowHead(rPeople) = 'People'
    rowHead(rLights) = 'Lights'
    rowHead(rEquip) = 'Equipment'
    rowHead(rRefrig) = 'Refrigeration Equipment'
    rowHead(rWaterUse) = 'Water Use Equipment'
    rowHead(rPowerGen) = 'Power Generation Equipment'
    rowHead(rHvacLoss) = 'HVAC Equipment Losses'
    rowHead(rRefrig) = 'Refrigeration'
    !misc
    rowHead(rInfil) = 'Infiltration'
    rowHead(rZoneVent) = 'Zone Ventilation'
    rowHead(rIntZonMix) = 'Interzone Mixing'
    !opaque surfaces
    rowHead(rRoof) = 'Roof'
    rowHead(rIntZonCeil) = 'Interzone Ceiling'
    rowHead(rOtherRoof) = 'Other Roof'
    rowHead(rExtWall) = 'Exterior Wall'
    rowHead(rIntZonWall) = 'Interzone Wall'
    rowHead(rGrdWall) = 'Ground Contact Wall'
    rowHead(rOtherWall) = 'Other Wall'
    rowHead(rExtFlr) = 'Exterior Floor'
    rowHead(rIntZonFlr) = 'Interzone Floor'
    rowHead(rGrdFlr) = 'Ground Contact Floor'
    rowHead(rOtherFlr) = 'Other Floor'
    !subsurfaces
    rowHead(rFeneCond) = 'Fenestration Conduction'
    rowHead(rFeneSolr) = 'Fenestration Solar'
    rowHead(rOpqDoor) = 'Opaque Door'
    rowHead(rGrdTot) = 'Grand Total'

    tableBody = ''
    totalColumn = 0.0d0
    percentColumn = 0.0d0
    grandTotalRow = 0.0d0

    HeatDesSelected = CalcFinalZoneSizing(iZone)%HeatDDNum
    timeHeatMax = CalcFinalZoneSizing(iZone)%TimeStepNumAtHeatMax
    IF (HeatDesSelected .NE. 0 .AND. timeHeatMax .NE. 0) THEN

      !PEOPLE
      seqData = peopleInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPeople,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rPeople) = totalColumn(rPeople) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = peopleLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPeople,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rPeople) = totalColumn(rPeople) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      seqData = peopleDelaySeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPeople,cSensDelay)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rPeople) = totalColumn(rPeople) + AvgData(timeHeatMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeHeatMax)

      !LIGHTS
      seqData = lightInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rLights,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rLights) = totalColumn(rLights) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = lightRetAirSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rLights,cSensRA)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rLights) = totalColumn(rLights) + AvgData(timeHeatMax)
      grandTotalRow(cSensRA) = grandTotalRow(cSensRA) + AvgData(timeHeatMax)

      seqData = lightDelaySeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rLights,cSensDelay)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rLights) = totalColumn(rLights) + AvgData(timeHeatMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeHeatMax)

      !EQUIPMENT
      seqData = equipInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rEquip,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rEquip) = totalColumn(rEquip) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = equipLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rEquip,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rEquip) = totalColumn(rEquip) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      seqData = equipDelaySeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rEquip,cSensDelay)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rEquip) = totalColumn(rEquip) + AvgData(timeHeatMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeHeatMax)

      !REFRIGERATION EQUIPMENT
      seqData = refrigInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rRefrig,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rRefrig) = totalColumn(rRefrig) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = refrigRetAirSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rRefrig,cSensRA)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rRefrig) = totalColumn(rRefrig) + AvgData(timeHeatMax)
      grandTotalRow(cSensRA) = grandTotalRow(cSensRA) + AvgData(timeHeatMax)

      seqData = refrigLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rRefrig,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rRefrig) = totalColumn(rRefrig) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      !WATER USE EQUIPMENT
      seqData = waterUseInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rWaterUse,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rWaterUse) = totalColumn(rWaterUse) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = waterUseLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rWaterUse,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rWaterUse) = totalColumn(rWaterUse) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      !HVAC EQUIPMENT LOSSES
      seqData = hvacLossInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rHvacLoss,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rHvacLoss) = totalColumn(rHvacLoss) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = hvacLossDelaySeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rHvacLoss,cSensDelay)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rHvacLoss) = totalColumn(rHvacLoss) + AvgData(timeHeatMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeHeatMax)

      !POWER GENERATION EQUIPMENT
      seqData = powerGenInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPowerGen,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rPowerGen) = totalColumn(rPowerGen) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = powerGenDelaySeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rPowerGen,cSensDelay)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rPowerGen) = totalColumn(rPowerGen) + AvgData(timeHeatMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeHeatMax)

      !INFILTRATION
      seqData = infilInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rInfil,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rInfil) = totalColumn(rInfil) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = infilLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rInfil,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rInfil) = totalColumn(rInfil) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      !ZONE VENTILATION
      seqData = zoneVentInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rZoneVent,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rZoneVent) = totalColumn(rZoneVent) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = zoneVentLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rZoneVent,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rZoneVent) = totalColumn(rZoneVent) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      !INTERZONE MIXING
      seqData = interZoneMixInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rIntZonMix,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rIntZonMix) = totalColumn(rIntZonMix) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = interZoneMixLatentSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rIntZonMix,cLatent)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rIntZonMix) = totalColumn(rIntZonMix) + AvgData(timeHeatMax)
      grandTotalRow(cLatent) = grandTotalRow(cLatent) + AvgData(timeHeatMax)

      !FENESTRATION CONDUCTION
      seqData = feneCondInstantSeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rFeneCond,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rFeneCond) = totalColumn(rFeneCond) + AvgData(timeHeatMax)
      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      !FENESTRATION SOLAR
!      seqData = feneSolarInstantSeq(iZone,:,HeatDesSelected) * powerConversion
!      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
!      tableBody(rFeneSolr,cSensInst)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
!      totalColumn(rFeneSolr) = totalColumn(rFeneSolr) + AvgData(timeHeatMax)
!      grandTotalRow(cSensInst) = grandTotalRow(cSensInst) + AvgData(timeHeatMax)

      seqData = feneSolarDelaySeq(iZone,:,HeatDesSelected) * powerConversion
      CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
      tableBody(rFeneSolr,cSensDelay)  = TRIM(RealToStr(AvgData(timeHeatMax),2))
      totalColumn(rFeneSolr) = totalColumn(rFeneSolr) + AvgData(timeHeatMax)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + AvgData(timeHeatMax)

      !opaque surfaces - must combine individual surfaces by class and other side conditions
      delayOpaque = 0.0d0
      DO kSurf = 1,TotSurfaces
        IF (.NOT. Surface(kSurf)%HeatTransSurf) CYCLE ! Skip non-heat transfer surfaces
        IF (Surface(kSurf)%Zone .EQ. iZone) THEN
          curExtBoundCond = surface(kSurf)%ExtBoundCond
          !if exterior is other side coefficients using ground preprocessor terms then
          !set it to ground instead of other side coefficients
          IF (curExtBoundCond .EQ. OtherSideCoefNoCalcExt .OR. curExtBoundCond .EQ. OtherSideCoefCalcExt) THEN
            IF (SameString(OSC(Surface(kSurf)%OSCPtr)%Name(1:17), 'surfPropOthSdCoef')) THEN
              curExtBoundCond = Ground
            END IF
          END IF
          seqData = surfDelaySeq(kSurf,:,HeatDesSelected)
          CALL MovingAvg(seqData,NumOfTimeStepInDay,NumTimeStepsInAvg,AvgData)
          singleSurfDelay = AvgData(timeHeatMax) * powerConversion
          SELECT CASE (surface(kSurf)%class)
            CASE (SurfaceClass_Wall)
              SELECT CASE (curExtBoundCond)
                CASE (ExternalEnvironment)
                  delayOpaque(rExtWall) = delayOpaque(rExtWall) + singleSurfDelay
                CASE (Ground,GroundFCfactorMethod)
                  delayOpaque(rGrdWall) = delayOpaque(rGrdWall) + singleSurfDelay
                CASE (OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt)
                  delayOpaque(rOtherWall) = delayOpaque(rOtherWall) + singleSurfDelay
                CASE DEFAULT !interzone
                  delayOpaque(rIntZonWall) = delayOpaque(rIntZonWall) + singleSurfDelay
              END SELECT
            CASE (SurfaceClass_Floor)
              SELECT CASE (curExtBoundCond)
                CASE (ExternalEnvironment)
                  delayOpaque(rExtFlr) = delayOpaque(rExtFlr) + singleSurfDelay
                CASE (Ground,GroundFCfactorMethod)
                  delayOpaque(rGrdFlr) = delayOpaque(rGrdFlr) + singleSurfDelay
                CASE (OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt)
                  delayOpaque(rOtherFlr) = delayOpaque(rOtherFlr) + singleSurfDelay
                CASE DEFAULT !interzone
                  delayOpaque(rIntZonFlr) = delayOpaque(rIntZonFlr) + singleSurfDelay
              END SELECT
            CASE (SurfaceClass_Roof)
              SELECT CASE (curExtBoundCond)
                CASE (ExternalEnvironment)
                  delayOpaque(rRoof) = delayOpaque(rRoof) + singleSurfDelay
                CASE (Ground,GroundFCfactorMethod,OtherSideCoefNoCalcExt,OtherSideCoefCalcExt,OtherSideCondModeledExt)
                  delayOpaque(rOtherRoof) = delayOpaque(rOtherRoof) + singleSurfDelay
                CASE DEFAULT !interzone
                  delayOpaque(rIntZonCeil) = delayOpaque(rIntZonCeil) + singleSurfDelay
              END SELECT
            CASE (SurfaceClass_Door)
              delayOpaque(rOpqDoor) = delayOpaque(rOpqDoor) + singleSurfDelay
          END SELECT
        END IF
      END DO
      DO k = rRoof,rOtherFlr
        tableBody(k,cSensDelay)  = TRIM(RealToStr(delayOpaque(k),2))
        totalColumn(k) = totalColumn(k) + delayOpaque(k)
        grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + delayOpaque(k)
      END DO
      tableBody(rOpqDoor,cSensDelay) = TRIM(RealToStr(delayOpaque(rOpqDoor),2))
      totalColumn(rOpqDoor) = totalColumn(rOpqDoor) + delayOpaque(rOpqDoor)
      grandTotalRow(cSensDelay) = grandTotalRow(cSensDelay) + delayOpaque(rOpqDoor)
    END IF

    !GRAND TOTAL ROW
    totalGrandTotal = 0.0d0
    DO k = 1,cLatent
      tableBody(rGrdTot,k)  = TRIM(RealToStr(grandTotalRow(k),2))
      totalGrandTotal = totalGrandTotal + grandTotalRow(k)
    END DO
    tableBody(rGrdTot,cTotal)  = TRIM(RealToStr(totalGrandTotal,2))

    !TOTAL COLUMN AND PERCENT COLUMN
    DO k = 1,rOpqDoor !to last row before total
      tableBody(k,cTotal) = TRIM(RealToStr(totalColumn(k),2))
      IF (totalGrandTotal .NE. 0.0d0) THEN
        tableBody(k,cPerc) = TRIM(RealToStr(100 * totalColumn(k)/totalGrandTotal,2))
      END IF
    END DO

    CALL writeSubtitle('Estimated Heating Peak Load Components')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'ZoneComponentLoadSummary',&
                                        TRIM(Zone(iZone)%Name),&
                                        'Estimated Heating Peak Load Components')

    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)

    !
    !---- Heating Peak Conditions Sub-Table
    !


    ALLOCATE(rowHead(10))
    ALLOCATE(columnHead(1))
    ALLOCATE(columnWidth(1))
    columnWidth = 14 !array assignment - same for all columns
    ALLOCATE(tableBody(10,1))

    columnHead(1) = 'Value'
    IF (unitsStyle .NE. unitsStyleInchPound) THEN
      rowHead(1) = 'Time of Peak Load'
      rowHead(2) = 'Outside  Dry Bulb Temperature [C]'
      rowHead(3) = 'Outside  Wet Bulb Temperature [C]'
      rowHead(4) = 'Outside Humidity Ratio at Peak [kgWater/kgAir]'
      rowHead(5) = 'Zone Dry Bulb Temperature [C]'
      rowHead(6) = 'Zone Relative Humdity [%]'
      rowHead(7) = 'Zone Humidity Ratio at Peak [kgWater/kgAir]'
      rowHead(8) = 'Peak Design Sensible Load [W]'
      rowHead(9) = 'Estimated Instant + Delayed Sensible Load [W]'
      rowHead(10) = 'Difference [W]'
    ELSE
      rowHead(1) = 'Time of Peak Load'
      rowHead(2) = 'Outside  Dry Bulb Temperature [F]'
      rowHead(3) = 'Outside  Wet Bulb Temperature [F]'
      rowHead(4) = 'Outside Humidity Ratio at Peak [lbWater/lbAir]'
      rowHead(5) = 'Zone Dry Bulb Temperature [F]'
      rowHead(6) = 'Zone Relative Humdity [%]'
      rowHead(7) = 'Zone Humidity Ratio at Peak [lbWater/lbAir]'
      rowHead(8) = 'Peak Design Sensible Load [Btu/h]'
      rowHead(9) = 'Estimated Instant + Delayed Sensible Load [Btu/h]'
      rowHead(10) = 'Difference [Btu/h]'
    END IF

    tableBody = ''

    IF (timeHeatMax .NE. 0) THEN
       !Time of Peak Load
      tableBody(1,1) = TRIM(HeatPeakDateHrMin(iZone))

      !Outside  Dry Bulb Temperature
      tableBody(2,1) = TRIM(RealToStr(convertIP(tempConvIndx,CalcFinalZoneSizing(iZone)%HeatOutTempSeq(timeHeatMax)),2))

      !Outside  Wet Bulb Temperature
      !use standard sea level air pressure because air pressure is not tracked with sizing data
      IF (CalcFinalZoneSizing(iZone)%HeatOutHumRatSeq(timeHeatMax) .LT. 1.0d0 .AND.       &
          CalcFinalZoneSizing(iZone)%HeatOutHumRatSeq(timeHeatMax) .GT. 0.0d0) THEN
        tableBody(3,1) = TRIM(RealToStr(convertIP(tempConvIndx, &
                                       PsyTwbFnTdbWPb(CalcFinalZoneSizing(iZone)%HeatOutTempSeq(timeHeatMax), &
                                       CalcFinalZoneSizing(iZone)%HeatOutHumRatSeq(timeHeatMax), &
                                       101325.0d0))  ,2))
      END IF

      !Humidity Ratio at Peak
      tableBody(4,1) = TRIM(RealToStr(CalcFinalZoneSizing(iZone)%HeatOutHumRatSeq(timeHeatMax),5))

      !Zone Dry Bulb Temperature
      tableBody(5,1) = TRIM(RealToStr(convertIP(tempConvIndx,CalcFinalZoneSizing(iZone)%HeatZoneTempSeq(timeHeatMax)),2))

      !Zone Relative Temperature
      !use standard sea level air pressure because air pressure is not tracked with sizing data
      tableBody(6,1) = TRIM(RealToStr(100 * PsyRhFnTdbWPb(CalcFinalZoneSizing(iZone)%HeatZoneTempSeq(timeHeatMax), &
                                                     CalcFinalZoneSizing(iZone)%HeatZoneHumRatSeq(timeHeatMax), &
                                                     101325.0d0)  ,2))

      !Zone Relative Humdity
      tableBody(7,1) = TRIM(RealToStr(CalcFinalZoneSizing(iZone)%HeatZoneHumRatSeq(timeHeatMax),5))

    END IF

    !Peak Design Sensible Load
    tableBody(8,1) = TRIM(RealToStr((-CalcFinalZoneSizing(iZone)%DesHeatLoad / mult) * powerConversion,2))    !change sign

    !Estimated Instant + Delayed Sensible Load
    tableBody(9,1) = TRIM(RealToStr(grandTotalRow(cSensInst) + grandTotalRow(cSensDelay),2))

    !Difference
    tableBody(10,1) = TRIM(RealToStr((-CalcFinalZoneSizing(iZone)%DesHeatLoad /mult) * powerConversion &
                             - (grandTotalRow(cSensInst) + grandTotalRow(cSensDelay)),2))

    CALL writeSubtitle('Heating Peak Conditions')
    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
                                        'ZoneComponentLoadSummary',&
                                        TRIM(Zone(iZone)%Name),&
                                        'Heating Peak Conditions')

    DEALLOCATE(columnHead)
    DEALLOCATE(rowHead)
    DEALLOCATE(columnWidth)
    DEALLOCATE(tableBody)

!    !
!    !---- Radiant to Convective Decay Curves for Heating
!    !
!    numObj = 0
!    !determine the number of surfaces to include
!    DO kSurf = 1, TotSurfaces
!      ZoneNum = Surface(kSurf)%Zone
!      IF (ZoneNum .NE. iZone) CYCLE
!      IF (ZoneNum .EQ. 0) CYCLE
!      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
!      numObj = numObj + 1
!    END DO
!
!    ALLOCATE(rowHead(numObj))
!    ALLOCATE(columnHead(16))
!    ALLOCATE(columnWidth(16))
!    columnWidth = 14 !array assignment - same for all columns
!    ALLOCATE(tableBody(numObj,16))
!
!    columnHead(1) = 'Time 1'
!    columnHead(2) = 'Time 2'
!    columnHead(3) = 'Time 3'
!    columnHead(4) = 'Time 4'
!    columnHead(5) = 'Time 5'
!    columnHead(6) = 'Time 6'
!    columnHead(7) = 'Time 7'
!    columnHead(8) = 'Time 8'
!    columnHead(9) = 'Time 9'
!    columnHead(10) = 'Time 10'
!    columnHead(11) = 'Time 11'
!    columnHead(12) = 'Time 12'
!    columnHead(13) = 'Time 13'
!    columnHead(14) = 'Time 14'
!    columnHead(15) = 'Time 15'
!    columnHead(16) = 'Time 16'
!
!    tableBody = ''
!    objCount = 0
!    DO kSurf = 1, TotSurfaces
!      ZoneNum = Surface(kSurf)%Zone
!      IF (ZoneNum .NE. iZone) CYCLE
!      IF (ZoneNum .EQ. 0) CYCLE
!      IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
!      objCount = objCount + 1
!      rowHead(objCount) = TRIM(Surface(kSurf)%Name)
!      DO jTime = 1, 16
!        tableBody(objCount,jTime) = TRIM(RealToStr(decayCurveHeat(kSurf,jTime),3))
!      END DO
!    END DO
!
!    CALL writeSubtitle('Radiant to Convective Decay Curves for Heating')
!    CALL writeTable(tableBody,rowHead,columnHead,columnWidth)
!    CALL CreateSQLiteTabularDataRecords(tableBody,rowHead,columnHead,&
!                                        'ZoneComponentLoadDetail',&
!                                        TRIM(Zone(iZone)%Name),&
!                                        'Radiant to Convective Decay Curves for Heating')
!
!    DEALLOCATE(columnHead)
!    DEALLOCATE(rowHead)
!    DEALLOCATE(columnWidth)
!    DEALLOCATE(tableBody)

    ! Put the decay curve into the EIO file
    IF (ShowDecayCurvesInEIO) THEN
      DO kSurf = 1, TotSurfaces
        ZoneNum = Surface(kSurf)%Zone
        IF (ZoneNum .NE. iZone) CYCLE
        IF (ZoneNum .EQ. 0) CYCLE
        IF (.not. ZoneEquipConfig(ZoneNum)%IsControlled) CYCLE
        WRITE (OutputFileInits, '(4A)',ADVANCE='NO')  'Radiant to Convective Decay Curves for Heating,', &
                                       TRIM(Zone(iZone)%Name),',',TRIM(Surface(kSurf)%Name)
        DO jTime = 1, MIN(NumOfTimeStepInHour*24,36)
          WRITE(OutputFileInits,'(A,F6.3)',ADVANCE='NO') ',', decayCurveHeat(kSurf,jTime)
        END DO
        WRITE(OutputFileInits,'()',ADVANCE='YES') !put a line feed at the end of the line
      END DO
    END IF

  END DO
END IF

END SUBROUTINE WriteZoneLoadComponentTable


SUBROUTINE WriteReportHeaders(reportName,objectName,averageOrSum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Write the first few lines of each report with headers to the output
          !   file for tabular reports.
USE DataStringGlobals, ONLY : VerString
USE DataHeatBalance,   ONLY : BuildingName

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),INTENT(IN) :: reportName
CHARACTER(len=*),INTENT(IN) :: objectName
INTEGER, INTENT(IN)         :: averageOrSum

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"
CHARACTER(len=*), PARAMETER :: TimeStampFmt1="(A,I4,A,I2.2,A,I2.2)"
CHARACTER(len=*), PARAMETER :: TimeStampFmt2="(A,I4.2,A,I2.2,A,I2.2,A)"


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=MaxNameLength * 2)       :: modifiedReportName
INTEGER                                :: iStyle
INTEGER                                :: curFH
CHARACTER(len=1)                       :: curDel

IF (averageOrSum .EQ. isSum) THEN ! if it is a summed variable CR5959
  modifiedReportName = TRIM(reportName) // ' per second'
ELSE
  modifiedReportName = reportName
ENDIF
DO iStyle = 1, numStyles
  curFH = TabularOutputFile(iStyle)
  curDel = del(iStyle)
  SELECT CASE (TableStyle(iStyle))
    CASE (tableStyleComma,tableStyleTab)
      WRITE(curFH,fmta) '--------------------------------------------------' // &
                                    '--------------------------------------------------'
      WRITE(curFH,fmta) 'REPORT:'      // curDel //TRIM(modifiedReportName)
      WRITE(curFH,fmta) 'FOR:'         // curDel //TRIM(objectName)
    CASE (tableStyleFixed)
      WRITE(curFH,fmta) '--------------------------------------------------' // &
                                    '--------------------------------------------------'
      WRITE(curFH,fmta) 'REPORT:      '// curDel //TRIM(modifiedReportName)
      WRITE(curFH,fmta) 'FOR:         '// curDel //TRIM(objectName)
    CASE (tableStyleHTML)
      WRITE(curFH,fmta) '<hr>'
      WRITE(curFH,fmta) '<p><a href="#toc" style="float: right">Table of Contents</a></p>'
      WRITE(curFH,fmta) '<a name=' // TRIM(MakeAnchorName(reportName,objectName)) // '></a>'
      WRITE(curFH,fmta) '<p>Report:<b>'   // curDel //TRIM(modifiedReportName) //'</b></p>'
      WRITE(curFH,fmta) '<p>For:<b>'         // curDel //TRIM(objectName) //'</b></p>'
      WRITE(curFH,TimeStampFmt1) "<p>Timestamp: <b>", td(1),'-', td(2),'-',td(3)
      WRITE(curFH,TimeStampFmt2) '  ',td(5),':', td(6),':',td(7),'</b></p>'
    CASE (tableStyleXML)
      IF (LEN_TRIM(prevReportName) .NE. 0) THEN
        WRITE(curFH,fmta) '</' // TRIM(prevReportName) //'>'  !close the last element if it was used.
      END IF
      WRITE(curFH,fmta) '<' // TRIM(ConvertToElementTag(modifiedReportName)) //'>'
      WRITE(curFH,fmta) '  <for>' //TRIM(objectName) //'</for>'
      prevReportName = ConvertToElementTag(modifiedReportName) !save the name for next time
  END SELECT
END DO
!clear the active subtable name for the XML reporting
activeSubTableName = ''
!save the report name if the subtable name is not available during XML processing
activeReportName = modifiedReportName
!save the "for" which is the object name in the report for HTML comment that contains the report, for, and subtable
activeForName = objectName
END SUBROUTINE WriteReportHeaders

SUBROUTINE writeSubtitle(subtitle)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Insert a subtitle into the current report
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),INTENT(IN) :: subtitle

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iStyle

DO iStyle = 1, numStyles
  SELECT CASE (TableStyle(iStyle))
    CASE (tableStyleComma,tableStyleTab,tableStyleFixed)
      WRITE(TabularOutputFile(iStyle),fmta) TRIM(subtitle)
      WRITE(TabularOutputFile(iStyle),fmta) ''
    CASE (tableStyleHTML)
      WRITE(TabularOutputFile(iStyle),fmta) '<b>'// TRIM(subtitle) // '</b><br><br>'
      WRITE(TabularOutputFile(iStyle),fmta) '<!-- FullName:' // TRIM(activeReportName) //'_' // &
                                            TRIM(activeForName) //'_' // TRIM(subtitle) // '-->'
    CASE (tableStyleXML)
      !save the active subtable name for the XML reporting
      activeSubTableName = subtitle
      !no other output is needed since writeTable uses the subtable name for each record.
  END SELECT
END DO
END SUBROUTINE WriteSubtitle

SUBROUTINE writeTextLine(lineOfText,isBold)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   April 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Insert a subtitle into the current report
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),INTENT(IN) :: lineOfText
LOGICAL,INTENT(IN),OPTIONAL :: isBold

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iStyle
LOGICAL :: useBold

IF (Present(isBold)) THEN
  useBold = isBold
ELSE
  useBold = .FALSE.
END IF

DO iStyle = 1, numStyles
  SELECT CASE (TableStyle(iStyle))
    CASE (tableStyleComma,tableStyleTab,tableStyleFixed)
      WRITE(TabularOutputFile(iStyle),fmta) TRIM(lineOfText)
    CASE (tableStyleHTML)
      IF (useBold) THEN
        WRITE(TabularOutputFile(iStyle),fmta) '<b>'// TRIM(lineOfText) // '</b><br><br>'
      ELSE
        WRITE(TabularOutputFile(iStyle),fmta) TRIM(lineOfText) // '<br>'
      END IF
    CASE (tableStyleXML)
      IF (LEN_TRIM(lineOfText) .NE. 0) THEN
        WRITE(TabularOutputFile(iStyle),fmta) '<note>' // TRIM(lineOfText) // '</note>'
      END IF
  END SELECT
END DO
END SUBROUTINE writeTextLine

SUBROUTINE WriteTable(body,rowLabels,columnLabels,widthColumn,transposeXML,footnoteText)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Output a table to the tabular output file in the selected
          !   style (comma, tab, space, html, xml).
          !
          !   The widthColumn array is only used for fixed space formatted reports
          !   if columnLables contain a vertical bar '|', they are broken into multiple
          !   rows.  If they exceed the column width even after that and the format is
          !   fixed, they are further shortened.
          !
          !   To include the currency symbol ($ by default but other symbols if the user
          !   has input it with Economics:CurrencyType) use the string ~~$~~ in the row
          !   headers, column headers, and body. For HTML files, the ASCII or UNICODE
          !   symbol for the currency will be included. For TXT files, the ASCII symbol
          !   will be used.

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),INTENT(IN),DIMENSION(:,:)    :: body  ! row,column
CHARACTER(len=*),INTENT(IN),DIMENSION(:)      :: rowLabels
CHARACTER(len=*),INTENT(IN),DIMENSION(:)      :: columnLabels
INTEGER,INTENT(INOUT),DIMENSION(:)               :: widthColumn
LOGICAL,INTENT(IN),OPTIONAL                   :: transposeXML
CHARACTER(len=*),INTENT(IN),OPTIONAL          :: footnoteText

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: fmta="(A)"
CHARACTER(len=*), PARAMETER :: blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=MaxNameLength*2),DIMENSION(:,:), ALLOCATABLE  :: colLabelMulti
CHARACTER(len=1000)                                         :: workColumn
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE       :: rowLabelTags
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE       :: columnLabelTags
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE       :: rowUnitStrings
CHARACTER(len=MaxNameLength),DIMENSION(:),ALLOCATABLE       :: columnUnitStrings
CHARACTER(len=MaxNameLength),DIMENSION(:,:),ALLOCATABLE       :: bodyEsc

INTEGER :: numColLabelRows
INTEGER :: maxNumColLabelRows
INTEGER :: widthRowLabel
INTEGER :: maxWidthRowLabel

INTEGER :: rowsBody
INTEGER :: colsBody
INTEGER :: colsColumnLabels
INTEGER :: colsWidthColumn
INTEGER :: rowsRowLabels

INTEGER :: iCol
INTEGER :: jRow
INTEGER :: colWidthLimit
INTEGER :: barLoc

CHARACTER(LEN=1000) :: outputLine
CHARACTER(LEN=1000) :: spaces
INTEGER :: iStyle
INTEGER :: curFH
CHARACTER(LEN=1)    :: curDel
CHARACTER(len=MaxNameLength) :: tagWithAttrib
integer :: col1start
LOGICAL :: doTransposeXML
LOGICAL :: isTableBlank
LOGICAL :: isRecordBlank

IF (Present(transposeXML)) THEN
  doTransposeXML = transposeXML
ELSE
  doTransposeXML = .FALSE. !if not present assume that the XML table should not be transposed
END IF
! create blank string
spaces = blank ! REPEAT(' ',1000)
! get sizes of arrays
rowsBody = SIZE(Body,1)
colsBody = SIZE(Body,2)
rowsRowLabels = SIZE(rowLabels)
colsColumnLabels = SIZE(columnLabels)
colsWidthColumn = SIZE(widthColumn)
! check size of arrays for consistancy and if inconsistent use smaller value
! and display warning
IF (rowsBody .NE. rowsRowLabels) THEN
  CALL ShowWarningError('REPORT:TABLE Inconsistant number of rows.')
  rowsBody = MIN(rowsBody,rowsRowLabels)
  rowsRowLabels = rowsBody
ENDIF
IF ((colsBody .NE. colsColumnLabels) .OR. (colsBody .NE.colsWidthColumn))  THEN
  CALL ShowWarningError('REPORT:TABLE Inconsistant number of columns.')
  colsBody = MIN(colsBody,MIN(colsColumnLabels,colsWidthColumn))
  colsWidthColumn = colsBody
  colsColumnLabels = colsBody
ENDIF
! create arrays to hold the XML tags
ALLOCATE(rowLabelTags(rowsBody))
ALLOCATE(columnLabelTags(colsBody))
ALLOCATE(rowUnitStrings(rowsBody))
ALLOCATE(columnUnitStrings(colsBody))
ALLOCATE(bodyEsc(rowsBody,colsBody))
! create new array to hold multiple line column lables
ALLOCATE(colLabelMulti(50,colsColumnLabels))
colLabelMulti = blank  !set array to blank
numColLabelRows = 0 !default value
maxNumColLabelRows = 0
DO iStyle = 1, numStyles
  curFH = TabularOutputFile(iStyle)
  curDel = del(iStyle)
  ! go through the columns and break them into multiple lines
  ! if bar '|' is found in a row then break into two lines
  ! if longer than the column width break into two lines for fixed style only
  DO iCol = 1, colsColumnLabels
    numColLabelRows = 0
    workColumn = columnLabels(iCol)
    widthcolumn(icol)=MAX(widthcolumn(icol),len_trim(columnLabels(iCol)))
    DO
      barLoc = INDEX(workColumn,'|')
      IF (barLoc .GT. 0) THEN
        numColLabelRows = numColLabelRows + 1
        colLabelMulti(numColLabelRows,iCol) = workColumn(:barLoc-1)
        workColumn = workColumn(barLoc+1:)
      ELSE
        numColLabelRows = numColLabelRows + 1
        colLabelMulti(numColLabelRows,iCol) = workColumn
        EXIT !inner do loop
      END IF
    END DO
    IF (numColLabelRows .GT. maxNumColLabelRows) THEN
      maxNumColLabelRows = numColLabelRows
    END IF
  END DO
  ! extra preprocessing for fixed style reports
  IF (TableStyle(iStyle) .EQ. tableStyleFixed) THEN
    ! break column headings into multiple rows if long (for fixed) or contain two spaces in a row.
    DO iCol = 1, colsColumnLabels
      colWidthLimit = widthColumn(iCol)
      DO jRow = 1, maxNumColLabelRows
        colLabelMulti(jRow,iCol) = colLabelMulti(jRow,iCol)(1:colWidthLimit)
      END DO
    END DO
    maxWidthRowLabel = 0
    DO jRow = 1, rowsRowLabels
      widthRowLabel = LEN_TRIM(rowLabels(jRow))
      IF (widthRowLabel .GT. maxWidthRowLabel) THEN
        maxWidthRowLabel = widthRowLabel
      END IF
    END DO
  END IF
  ! output depending on style of format
  SELECT CASE (TableStyle(iStyle))

    CASE (tableStyleComma,tableStyleTab)
      ! column headers
      DO jRow = 1, maxNumColLabelRows
        outputLine = curDel  ! one leading delimiters on column header lines
        DO iCol = 1, colsColumnLabels
          outputLine = TRIM(outputLine) // curDel // TRIM(ADJUSTL(colLabelMulti(jRow, iCol)))
        END DO
        WRITE(curFH,fmta) TRIM(InsertCurrencySymbol(outputLine,.FALSE.))
      END DO
      ! body with row headers
      DO jRow = 1, rowsBody
        outputLine = curDel // TRIM(rowLabels(jRow))   !one leading delimiters on table body lines
        DO iCol = 1, colsBody
          outputLine = TRIM(outputLine) // curDel // TRIM(ADJUSTL(body(jRow, iCol)))
        END DO
        WRITE(curFH,fmta) TRIM(InsertCurrencySymbol(outputLine,.FALSE.))
      END DO
      IF (PRESENT(footnoteText)) THEN
        IF (LEN_TRIM(footnoteText) .GT. 0) THEN
          WRITE(curFH, fmta) TRIM(footnoteText)
        END IF
      END IF
      WRITE(curFH,fmta) ''
      WRITE(curFH,fmta) ''

    CASE (tableStyleFixed)
      ! column headers
      DO jRow = 1, maxNumColLabelRows
        outputLine = blank ! spaces(:maxWidthRowLabel+2)  !two extra spaces and leave blank area for row labels
        col1start=max(maxWidthRowLabel+2,3)
        DO iCol = 1, colsColumnLabels
          if (iCol /= 1) then
            outputLine = TRIM(outputLine) // '  '// ADJUSTR(colLabelMulti(jRow, iCol)(1:widthColumn(iCol)))
          else
            outputLine(col1start:) = '  '// ADJUSTR(colLabelMulti(jRow, iCol)(1:widthColumn(iCol)))
          endif
        END DO
        WRITE(curFH,fmta) TRIM(InsertCurrencySymbol(outputLine,.FALSE.))
      END DO
      ! body with row headers
      DO jRow = 1, rowsBody
        outputLine = '  ' // ADJUSTR(rowLabels(jRow)(:maxWidthRowLabel))   !two blank spaces on table body lines
        col1start=max(len_trim(outputLine)+2,maxWidthRowLabel+2)
        DO iCol = 1, colsBody
          if (iCol /= 1) then
            outputLine = TRIM(outputLine) // '  '// ADJUSTR(body(jRow, iCol)(1:widthColumn(iCol)))
          else
            outputLine(col1start:)= '  '// ADJUSTR(body(jRow, iCol)(1:widthColumn(iCol)))
          endif
        END DO
        WRITE(curFH,fmta) TRIM(InsertCurrencySymbol(outputLine,.FALSE.))
      END DO
      IF (PRESENT(footnoteText)) THEN
        IF (LEN_TRIM(footnoteText) .GT. 0) THEN
          WRITE(curFH, fmta) TRIM(footnoteText)
        END IF
      END IF
      WRITE(curFH,fmta) ''
      WRITE(curFH,fmta) ''

    CASE (tableStyleHTML)
      ! set up it being a table
      WRITE(curFH,fmta) '<table border="1" cellpadding="4" cellspacing="0">'
      ! column headers
      WRITE(curFH,fmta) '  <tr><td></td>'  !start new row and leave empty cell
      DO iCol = 1, colsColumnLabels
        outputLine = '    <td align="right">'
        DO jRow = 1, maxNumColLabelRows
          outputLine = TRIM(outputLine) // TRIM(colLabelMulti(jRow, iCol))
          IF (jRow .LT. maxNumColLabelRows) THEN
            outputLine = TRIM(outputLine) // '<br>'
          END IF
        END DO
        WRITE(curFH,fmta) TRIM(InsertCurrencySymbol(outputLine,.TRUE.)) // '</td>'
      END DO
      WRITE(curFH,fmta) '  </tr>'
      ! body with row headers
      DO jRow = 1, rowsBody
        WRITE(curFH,fmta) '  <tr>'
        IF (TRIM(rowLabels(jRow)) .NE. '') THEN
          WRITE(curFH,fmta) '    <td align="right">' // TRIM(InsertCurrencySymbol(rowLabels(jRow),.TRUE.)) // '</td>'
        ELSE
          WRITE(curFH,fmta) '    <td align="right">&nbsp;</td>'
        ENDIF
        DO iCol = 1, colsBody
          IF (TRIM(body(jRow,iCol)) .NE. '') THEN
            WRITE(curFH,fmta) '    <td align="right">' // TRIM(InsertCurrencySymbol(body(jRow, iCol),.TRUE.)) // '</td>'
          ELSE
            WRITE(curFH,fmta) '    <td align="right">&nbsp;</td>'
          ENDIF
        END DO
        WRITE(curFH,fmta) '  </tr>'
      END DO
      ! end the table
      WRITE(curFH,fmta) '</table>'
      IF (PRESENT(footnoteText)) THEN
        IF (LEN_TRIM(footnoteText) .GT. 0) THEN
          WRITE(curFH, fmta) '<i>' // TRIM(footnoteText) // '</i>'
        END IF
      END IF
      WRITE(curFH,fmta) '<br><br>'
    CASE (tableStyleXML)
      !check if entire table is blank and it if is skip generating anything
      isTableBlank = .TRUE.
      DO jRow = 1, rowsBody
        DO iCol = 1, colsBody
          IF (LEN_TRIM(body(jRow, iCol)) .GT. 0) THEN
            isTableBlank = .FALSE.
            EXIT
          END IF
        END DO
        IF (.NOT. isTableBlank) EXIT
      END DO
      ! if non-blank cells in the table body were found create the table.
      IF (.NOT. isTableBlank) THEN
        !if report name and subtable name the same add "record" to the end
        activeSubTableName = ConvertToElementTag(activeSubTableName)
        activeReportNameNoSpace = ConvertToElementTag(activeReportName)
        IF (SameString(activeSubTableName,activeReportNameNoSpace)) THEN
          activeSubTableName = TRIM(activeSubTableName) // 'Record'
        END IF
        !if no subtable name use the report name and add "record" to the end
        IF (LEN_TRIM(activeSubTableName) .EQ. 0) THEN
          activeSubTableName = TRIM(activeReportNameNoSpace) // 'Record'
        END IF
        ! if a single column table, transpose it automatically
        IF ((colsBody .EQ. 1) .AND. (rowsBody .GT. 1)) THEN
          doTransposeXML = .TRUE.
        END IF
        ! first convert all row and column headers into tags compatible with XML strings
        DO jRow = 1, rowsBody
          rowLabelTags(jRow) = ConvertToElementTag(rowLabels(jRow))
          IF (LEN_TRIM(rowLabelTags(jRow)) .EQ. 0) THEN
            rowLabelTags(jRow) = "none"
          END IF
          rowUnitStrings(jRow) = GetUnitSubString(rowLabels(jRow))
          IF (SameString(rowUnitStrings(jRow),'Invalid/Undefined')) THEN
            rowUnitStrings(jRow) = ''
          END IF
        END DO
        DO iCol = 1, colsBody
          columnLabelTags(iCol) = ConvertToElementTag(columnLabels(iCol))
          IF (LEN_TRIM(columnLabelTags(iCol)) .EQ. 0) THEN
            columnLabelTags(iCol) = "none"
          END IF
          columnUnitStrings(iCol) = GetUnitSubString(columnLabels(iCol))
          IF (SameString(columnUnitStrings(iCol),'Invalid/Undefined')) THEN
            columnUnitStrings(iCol) = ''
          END IF
        END DO
        ! convert entire table body to one with escape characters (no " ' < > &)
        DO jRow = 1, rowsBody
          DO iCol = 1, colsBody
            bodyEsc(jRow,iCol) = ConvertToEscaped(body(jRow,iCol))
          END DO
        END DO
        IF (.NOT. doTransposeXML) THEN
          ! body with row headers
          DO jRow = 1, rowsBody
            !check if record is blank and it if is skip generating anything
            isRecordBlank = .TRUE.
            DO iCol = 1, colsBody
              IF (LEN_TRIM(bodyEsc(jRow, iCol)) .GT. 0) THEN
                isRecordBlank = .FALSE.
                EXIT
              END IF
            END DO
            IF (.NOT. isRecordBlank) THEN
              WRITE(curFH,fmta) '  <' // TRIM(activeSubTableName) // '>'
              IF (LEN_TRIM(rowLabelTags(jRow)) .GT. 0) THEN
                WRITE(curFH, fmta) '    <name>' // TRIM(rowLabelTags(jRow)) // '</name>'
              ENDIF
              DO iCol = 1, colsBody
                IF (LEN_TRIM(ADJUSTL(bodyEsc(jRow, iCol))) .GT. 0) THEN !skip blank cells
                  tagWithAttrib = '<' // TRIM(columnLabelTags(iCol))
                  IF (LEN_TRIM(columnUnitStrings(iCol)) .GT. 0) THEN
                    tagWithAttrib = TRIM(tagWithAttrib) // &
                                    ' units=' // CHAR(34) // TRIM(columnUnitStrings(iCol)) // CHAR(34) &
                                    // '>' !if units are present add them as an attribute
                  ELSE
                    tagWithAttrib = TRIM(tagWithAttrib) //  '>'
                  ENDIF
                  WRITE(curFH, fmta) '    ' // TRIM(tagWithAttrib) // &
                                      TRIM(ADJUSTL(bodyEsc(jRow, iCol))) // &
                                      '</' // TRIM(columnLabelTags(iCol)) // '>'
                END IF
              END DO
              WRITE(curFH,fmta) '  </' // TRIM(activeSubTableName) // '>'
            END IF
          END DO
        ELSE !transpose XML table
          ! body with row headers
          DO iCol = 1, colsBody
            !check if record is blank and it if is skip generating anything
            isRecordBlank = .TRUE.
            DO jRow = 1, rowsBody
              IF (LEN_TRIM(bodyEsc(jRow, iCol)) .GT. 0) THEN
                isRecordBlank = .FALSE.
                EXIT
              END IF
            END DO
            IF (.NOT. isRecordBlank) THEN
              WRITE(curFH,fmta) '  <' // TRIM(activeSubTableName) // '>'
              ! if the column has units put them into the name tag
              IF (LEN_TRIM(columnLabelTags(iCol)) .GT. 0) THEN
                IF (LEN_TRIM(columnUnitStrings(iCol)) .GT. 0) THEN
                  WRITE(curFH, fmta) '    <name units=' // CHAR(34) // TRIM(columnUnitStrings(iCol)) // CHAR(34) &
                                      // '>' // TRIM(columnLabelTags(iCol)) // '</name>'
                ELSE
                  WRITE(curFH, fmta) '    <name>' // TRIM(columnLabelTags(iCol)) // '</name>'
                END IF
              ENDIF
              DO jRow = 1, rowsBody
                IF (LEN_TRIM(bodyEsc(jRow, iCol)) .GT. 0) THEN !skip blank cells
                  tagWithAttrib = '<' // TRIM(rowLabelTags(jRow))
                  IF (LEN_TRIM(rowUnitStrings(jRow)) .GT. 0) THEN
                    tagWithAttrib = TRIM(tagWithAttrib) // &
                                    ' units=' // CHAR(34) // TRIM(rowUnitStrings(jRow)) // CHAR(34) &
                                    // '>' !if units are present add them as an attribute
                  ELSE
                    tagWithAttrib = TRIM(tagWithAttrib) //  '>'
                  ENDIF
                  WRITE(curFH, fmta) '    ' // TRIM(tagWithAttrib) // &
                                      TRIM(ADJUSTL(bodyEsc(jRow, iCol))) // &
                                      '</' // TRIM(rowLabelTags(jRow)) // '>'
                END IF
              END DO
              WRITE(curFH,fmta) '  </' // TRIM(activeSubTableName) // '>'
            END IF
          END DO
        END IF
        IF (PRESENT(footnoteText)) THEN
          IF (LEN_TRIM(footnoteText) .GT. 0) THEN
            WRITE(curFH, fmta) '  <footnote>' // TRIM(footnoteText) // '</footnote>'
          END IF
        END IF
      END IF
    CASE DEFAULT

  END SELECT
END DO
DEALLOCATE(colLabelMulti)
DEALLOCATE(rowLabelTags)
DEALLOCATE(columnLabelTags)
DEALLOCATE(rowUnitStrings)
DEALLOCATE(columnUnitStrings)
DEALLOCATE(bodyEsc)
RETURN
END SUBROUTINE WriteTable

FUNCTION MakeAnchorName(reportString,objectString) RESULT (stringOut)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   June 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Use the name of the report and object be used to create and HTML anchor

          ! METHODOLOGY EMPLOYED:
          !   Remove spaces and put double colon between names

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: reportString
CHARACTER(len=*), INTENT(IN) :: objectString
CHARACTER(len=MaxNameLength * 2)           :: StringOut

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: i

StringOut = ''
DO i = 1,LEN_TRIM(reportString)
  IF (INDEX(validChars,reportString(i:i)) .GT. 0) THEN
    StringOut = TRIM(StringOut) // reportString(i:i)
  END IF
END DO
StringOut = TRIM(StringOut) // '::'
DO i = 1,LEN_TRIM(objectString)
  IF (INDEX(validChars,objectString(i:i)) .GT. 0) THEN
    StringOut = TRIM(StringOut) // objectString(i:i)
  END IF
END DO
END FUNCTION

FUNCTION InsertCurrencySymbol(inString,isHTML) RESULT (outSt)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Looks for the ~~$~~

          ! METHODOLOGY EMPLOYED:
          !   na
USE DataCostEstimate
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: inString     ! Input String
LOGICAL, INTENT(IN) :: isHTML                ! True if an HTML string
CHARACTER(len=LEN(inString)) :: outSt ! Result String

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: loc !location of the ~$~ symbol
outSt = inString
loc = INDEX(outSt,'~~$~~')
DO WHILE (loc .GT. 0)
  IF (isHTML) THEN
    outSt = inString(:loc-1) // TRIM(monetaryUnit(selectedMonetaryUnit)%html) // outSt(loc+5:)
  ELSE
    outSt = inString(:loc-1) // TRIM(monetaryUnit(selectedMonetaryUnit)%txt) // outSt(loc+5:)
  END IF
  loc = INDEX(outSt,'~~$~~')
END DO
END FUNCTION InsertCurrencySymbol

FUNCTION ConvertToElementTag(inString) RESULT (outString)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Convert report column or row header into a tag string
          !   that just has A-Z, a-z, or 0-1 characters and is
          !   shown in camelCase.

          ! METHODOLOGY EMPLOYED:
          !   na
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: inString             ! Input String
CHARACTER(len=LEN(inString)) :: outString            ! Result String

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iIn   !index through the string
INTEGER :: jOut  !index of the output string
INTEGER :: curCharVal !ascii value of current character
LOGICAL :: foundOther !flag if character found besides A-Z, a-z, 0-9

outString = " "
jOut = 0
foundOther = .TRUE.
DO iIn = 1,LEN_TRIM(inString)
  curCharVal = ICHAR(inString(iIn:iIn))
  SELECT CASE (curCharVal)
    CASE (65:90)  !A-Z upper case
      jOut = jOut + 1
      IF (foundOther) THEN
        outString(jOut:jOut) = CHAR(curCharVal) !keep as upper case after finding a space or another character
      ELSE
        outString(jOut:jOut) = CHAR(curCharVal + 32) !convert to lower case
      ENDIF
      foundOther = .FALSE.
    CASE (97:122) !A-Z lower case
      jOut = jOut + 1
      IF (foundOther) THEN
        outString(jOut:jOut) = CHAR(curCharVal - 32) !convert to upper case
      ELSE
        outString(jOut:jOut) = CHAR(curCharVal) !leave as lower case
      ENDIF
      foundOther = .FALSE.
    CASE (48:57)   !0-9 numbers
      jOut = jOut + 1
      ! if first character is a number then prepend with the letter "t"
      IF (jOut .EQ. 1) THEN
        outString(1:1) = 't'
        jOut = 2
      END IF
      outString(jOut:jOut) = CHAR(curCharVal)
      foundOther = .FALSE.
    CASE (91) ! [ bracket
      EXIT !stop parsing because unit string was found
    CASE DEFAULT
      foundOther = .TRUE.
  END SELECT
END DO
END FUNCTION ConvertToElementTag

FUNCTION ConvertToEscaped(inString) RESULT (outString)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Convert to XML safe escaped character string
          !   so it excludes:
          !               " ' < > &

          ! METHODOLOGY EMPLOYED:
          !   na
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: inString             ! Input String
CHARACTER(len=LEN(inString)) :: outString            ! Result String

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: iIn   !index through the string
INTEGER :: jOut  !index of the output string
INTEGER :: curCharVal !ascii value of current character

outString = " "
jOut = 0
DO iIn = 1,LEN_TRIM(inString)
  curCharVal = ICHAR(inString(iIn:iIn))
  SELECT CASE (curCharVal)
    CASE (34)  !  "
      jOut = jOut + 6
      outString(jOut-5:jOut) = '&quot;'
    CASE (38) !   &
      jOut = jOut + 5
      outString(jOut-4:jOut) = '&amp;'
    CASE (39) !   '
      jOut = jOut + 6
      outString(jOut-6:jOut) = '&apos;'
    CASE (60) !   <
      jOut = jOut + 4
      outString(jOut-3:jOut) = '&lt;'
    CASE (62) !   >
      jOut = jOut + 4
      outString(jOut-3:jOut) = '&gt;'
    CASE (176) !   degree
      jOut = jOut + 1
      outString(jOut:jOut) = '*' !replace degree symbol with asterisk to avoid errors from various XML editors
    CASE DEFAULT !most characters are fine
      jOut = jOut + 1
      outString(jOut:jOut) = CHAR(curCharVal)
  END SELECT
END DO
END FUNCTION ConvertToEscaped


SUBROUTINE DetermineBuildingFloorArea

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   November 2003
          !       MODIFIED       BTG added checks for plenums. Feb2004
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   To determine the total floor area of the building and the
          !   conditioned floor area of the building

          ! METHODOLOGY EMPLOYED:
          !   Use the Zone array and sum the areas for all zones

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE DataHeatBalance, ONLY: Zone, StandardZone
USE ZonePlenum, ONLY: ZoneRetPlenCond, ZoneSupPlenCond, NumZoneReturnPlenums, NumZoneSupplyPlenums

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)     :: curZoneArea
INTEGER  :: iZone
!INTEGER  :: found

buildingGrossFloorArea = 0.0d0
buildingConditionedFloorArea = 0.0d0
DO iZone=1,NumOfZones
  curZoneArea = Zone(iZone)%FloorArea * Zone(iZone)%Multiplier * Zone(iZone)%ListMultiplier

  ! OLD CHECK IF PLENUM SHOULD BE EXCLUDED AUTOMATICALLY
  !check if this zone is also a return plenum or a supply plenum
  !found = 0
  !if (NumZoneReturnPlenums > 0) THEN
  !  found = FindItemInList(Zone(iZone)%Name, ZoneRetPlenCond%ZoneName, NumZoneReturnPlenums)
  !endif
  !IF (found /= 0)  curZoneArea = 0.0d0
  !found = 0
  !if (NumZoneSupplyPlenums > 0) THEN
  !  found = FindItemInList(Zone(iZone)%Name, ZoneSupPlenCond%ZoneName, NumZoneSupplyPlenums)
  !endif
  !IF (found /= 0)  curZoneArea = 0.0d0

  IF (Zone(iZone)%isPartOfTotalArea) THEN
    buildingGrossFloorArea = buildingGrossFloorArea + curZoneArea
    ! If a ZoneHVAC:EquipmentConnections is used for a zone then
    ! it is considered conditioned. Also ZONE SUPPLY PLENUM and ZONE RETURN PLENUM are
    ! also is considered conditioned.
    IF (Zone(iZone)%SystemZoneNodeNumber .GT. 0)   THEN
      buildingConditionedFloorArea = buildingConditionedFloorArea + curZoneArea
    END IF
  END IF
END DO
END SUBROUTINE DetermineBuildingFloorArea

!======================================================================================================================
!======================================================================================================================
!
!
!    ROUTINES RELATED TO IF VALUE IS IN A RANGE
!
!
!======================================================================================================================
!======================================================================================================================

LOGICAL FUNCTION isInTriangle(qx,qy,x1,y1,x2,y2,x3,y3)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   June 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Determine if point q is in triangle defined by points a,b,c

          ! METHODOLOGY EMPLOYED:
          !   The function used three times is positive the point is on the "right"
          !   side and negative if on "left" side. By checking to make sure the signs
          !   are always the same. it determines that the point is inside of the
          !   triangle.

          ! REFERENCES:
          !   http://mcraefamily.com/MathHelp/GeometryPointAndTriangle2.htm

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), INTENT(IN) :: qx
REAL(r64), INTENT(IN) :: qy
REAL(r64), INTENT(IN) :: x1
REAL(r64), INTENT(IN) :: y1
REAL(r64), INTENT(IN) :: x2
REAL(r64), INTENT(IN) :: y2
REAL(r64), INTENT(IN) :: x3
REAL(r64), INTENT(IN) :: y3

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: fAB
REAL(r64) :: fCA
REAL(r64) :: fBC

fAB = (qy - y1) * (x2 - x1) - (qx - x1) * (y2 - y1)
fCA = (qy - y3) * (x1 - x3) - (qx - x3) * (y1 - y3)
fBC = (qy - y2) * (x3 - x2) - (qx - x2) * (y3 - y2)
IF ((fAB * fBC) .GE. 0.0d0 .AND. (fBC * fCA) .GE. 0.0d0) THEN
  isInTriangle = .TRUE.
ELSE
  isInTriangle = .FALSE.
END IF
END FUNCTION isInTriangle


LOGICAL FUNCTION isInQuadrilateral(qx,qy,ax,ay,bx,by,cx,cy,dx,dy)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   June 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Determine if point q is in a quadrilateral defined by points a,b,c,d
          !   Those points should express a quadrilateral in order of the points going
          !   around the outside of the polygon. They should not describe an "hourglass"
          !   shape where the lines cross in the middle of the figure.

          ! METHODOLOGY EMPLOYED:
          !   Check if the point is in triangle a,b,c or in triangle c,d,a

          ! REFERENCES:
          !   http://mcraefamily.com/MathHelp/GeometryPointAndTriangle4.htm

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), INTENT(IN) :: qx
REAL(r64), INTENT(IN) :: qy
REAL(r64), INTENT(IN) :: ax
REAL(r64), INTENT(IN) :: ay
REAL(r64), INTENT(IN) :: bx
REAL(r64), INTENT(IN) :: by
REAL(r64), INTENT(IN) :: cx
REAL(r64), INTENT(IN) :: cy
REAL(r64), INTENT(IN) :: dx
REAL(r64), INTENT(IN) :: dy

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL :: inABC
LOGICAL :: inCDA

inABC = isInTriangle(qx,qy,ax,ay,bx,by,cx,cy)
inCDA = isInTriangle(qx,qy,cx,cy,dx,dy,ax,ay)
IF (inABC .OR. inCDA) THEN
  isInQuadrilateral = .TRUE.
ELSE
  isInQuadrilateral = .FALSE.
END IF
END FUNCTION isInQuadrilateral

!======================================================================================================================
!======================================================================================================================
!
!
!    SUPPORT ROUTINES
!
!
!======================================================================================================================
!======================================================================================================================


FUNCTION RealToStr(RealIn,numDigits) RESULT (stringOut)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       November 2008; LKL - prevent errors
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          !   Abstract away the internal write concept

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)            :: RealIn
  INTEGER, INTENT(IN)              :: numDigits

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(LEN=*), PARAMETER, DIMENSION(0:9) :: formDigits= &
    (/'(F12.0)',   &  ! formDigits(0)
      '(F12.1)',   &  ! formDigits(1)
      '(F12.2)',   &  ! formDigits(2)
      '(F12.3)',   &  ! formDigits(3)
      '(F12.4)',   &  ! formDigits(4)
      '(F12.5)',   &  ! formDigits(5)
      '(F12.6)',   &  ! formDigits(6)
      '(F12.7)',   &  ! formDigits(7)
      '(F12.8)',   &  ! formDigits(8)
      '(F12.9)'/)     ! formDigits(9)
   REAL(r64), PARAMETER, DIMENSION(0:9) :: maxvalDigits= &
     (/9999999999.d0,  &  ! maxvalDigits(0)
        999999999.d0,  &  ! maxvalDigits(1)
         99999999.d0,  &  ! maxvalDigits(2)
          9999999.d0,  &  ! maxvalDigits(3)
           999999.d0,  &  ! maxvalDigits(4)
            99999.d0,  &  ! maxvalDigits(5)
             9999.d0,  &  ! maxvalDigits(6)
              999.d0,  &  ! maxvalDigits(7)
               99.d0,  &  ! maxvalDigits(8)
                9.d0/)    ! maxvalDigits(9)

   CHARACTER(len=*), PARAMETER :: fmtd='(E12.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  CHARACTER(LEN=12) :: stringOut
  INTEGER           :: nDigits

  nDigits = numDigits
  if (RealIn < 0.0d0) nDigits=nDigits-1
  IF (nDigits .GT. 9) nDigits = 9
  IF (nDigits .LT. 0) nDigits = 0

  if (ABS(RealIn) > maxvalDigits(nDigits)) then
    write(FMT=fmtd, UNIT=stringOut) RealIn
  else
    write(FMT=formDigits(nDigits), UNIT=stringOut) RealIn
  endif
!  WRITE(FMT=, UNIT=stringOut) RealIn
  ! check if it did not fit
!  IF (stringOut(1:1) .EQ. "*") THEN
!    WRITE(FMT='(E12.6)', UNIT=stringOut) RealIn
!  END IF

!WRITE(FMT="(F10.4)", UNIT=stringOut, IOSTAT=status ) RealIn
END FUNCTION

FUNCTION IntToStr(intIn) RESULT (stringOut)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Abstract away the internal write concept

IMPLICIT NONE

INTEGER, INTENT(IN)    :: intIn
CHARACTER(LEN=12)      :: stringOut
WRITE(FMT=*, UNIT=stringOut) intIn
END FUNCTION

FUNCTION StrToReal(stringIn) RESULT (realValue)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Abstract away the internal read concept

IMPLICIT NONE

CHARACTER(len=*), INTENT(IN)    :: stringIn
REAL(R64)                       :: realValue
READ(FMT=*, UNIT=stringIn, Err=900) realValue
RETURN
900 realValue=-99999.d0
RETURN
END FUNCTION

FUNCTION DateToString(codedDate) RESULT (stringOut)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Convert the coded date format into a usable
          !   string

USE General, ONLY: DecodeMonDayHrMin

IMPLICIT NONE

INTEGER, INTENT(IN)  :: codedDate   ! word containing encoded month, day, hour, minute
                                    ! ((month*100 + day)*100 + hour)*100 + minute
CHARACTER(len=*), PARAMETER :: DateFmt="(I2.2,'-',A3,'-',I2.2,':',I2.2)"

CHARACTER(len=12) :: stringOut
INTEGER :: Month  ! month in integer format (1-12)
INTEGER :: Day    ! day in integer format (1-31)
INTEGER :: Hour   ! hour in integer format (1-24)
INTEGER :: Minute ! minute in integer format (0:59)
CHARACTER(LEN=3) :: monthName

IF (codedDate /= 0) THEN
  monthName =''
  CALL DecodeMonDayHrMin(codedDate,Month,Day,Hour,Minute)
  Hour = Hour - 1
  IF (Minute .EQ. 60) THEN
    Hour = Hour + 1
    Minute = 0
  END IF
  SELECT CASE (MONTH)
    CASE (1)
      monthName = 'JAN'
    CASE (2)
      monthName = 'FEB'
    CASE (3)
      monthName = 'MAR'
    CASE (4)
      monthName = 'APR'
    CASE (5)
      monthName = 'MAY'
    CASE (6)
      monthName = 'JUN'
    CASE (7)
      monthName = 'JUL'
    CASE (8)
      monthName = 'AUG'
    CASE (9)
      monthName = 'SEP'
    CASE (10)
      monthName = 'OCT'
    CASE (11)
      monthName = 'NOV'
    CASE (12)
      monthName = 'DEC'
    CASE DEFAULT
      monthName = '***'
  END SELECT
  WRITE(FMT=DateFmt, UNIT=stringOut) Day,MonthName,Hour,Minute
  IF (INDEX(stringOut,'*') .GT. 0) THEN
    stringOut = '-'
  END IF
ELSE  ! codeddate = 0
    stringOut = '-'
ENDIF

END FUNCTION DateToString

SUBROUTINE AddTOCEntry(nameSection,nameReport)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   September 2005
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Adds an entry for the TOC so that it can be created
          !   prior to the actual reports being generated. Note that
          !   the arguments must match what is used in
          !   "WriteReportHeaders" for the HTML anchors to work
          !   correctly.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*),INTENT(IN) :: nameReport
CHARACTER(len=*),INTENT(IN) :: nameSection

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na

IF (.NOT. ALLOCATED(TOCEntries)) THEN
  TOCEntriesSize = 20
  ALLOCATE(TOCEntries(TOCEntriesSize))
  TOCEntriesCount = 1
ELSE
  TOCEntriesCount = TOCEntriesCount + 1
  ! if larger then current size then make a temporary array of the same
  ! type and put stuff into it while reallocating the main array
  IF (TOCEntriesCount .GT. TOCEntriesSize) THEN
    ALLOCATE(CopyOfTOCEntries(TOCEntriesSize))
    CopyOfTOCEntries = TOCEntries
    DEALLOCATE(TOCEntries)
    ! double the size of the array
    ALLOCATE(TOCEntries(TOCEntriesSize + 20))
    TOCEntries(1:TOCEntriesSize) = CopyOfTOCEntries
    DEALLOCATE(CopyOfTOCEntries)
    TOCEntriesSize = TOCEntriesSize + 20
  END IF
END IF
TOCEntries(TOCEntriesCount)%reportName = nameReport
TOCEntries(TOCEntriesCount)%sectionName = nameSection
END SUBROUTINE

SUBROUTINE SetupUnitConversions
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 12, 2009
          !    MODIFIED       March 2010; Linda Lawrie; Add deltaC and KJ/KG
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Initialize the array that contains the unit conversion
          !   information. The code is based on code generated
          !   in a spreadsheet titled UnitConversion.xls

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
UnitConvSize = 94
ALLOCATE(UnitConv(UnitConvSize))
UnitConv(1)%siName = '%'
UnitConv(2)%siName = '°C'
UnitConv(3)%siName = '0=OFF 1=ON'
UnitConv(4)%siName = '0-NO  1-YES'
UnitConv(5)%siName = '1-YES 0-NO'
UnitConv(6)%siName = 'A'
UnitConv(7)%siName = 'ACH'
UnitConv(8)%siName = 'ACH'
UnitConv(9)%siName = 'BASE 10C'
UnitConv(10)%siName = 'BASE 18C'
UnitConv(11)%siName = 'C'
UnitConv(12)%siName = 'CD/M2'
UnitConv(13)%siName = 'DEG'
UnitConv(14)%siName = 'FRAC'
UnitConv(15)%siName = 'HOUR'
UnitConv(16)%siName = 'HOURS'
UnitConv(17)%siName = 'HR'
UnitConv(18)%siName = 'HRS'
UnitConv(19)%siName = 'J'
UnitConv(20)%siName = 'J'
UnitConv(21)%siName = 'J'
UnitConv(22)%siName = 'J'
UnitConv(23)%siName = 'J'
UnitConv(24)%siName = 'J'
UnitConv(25)%siName = 'J/KG'
UnitConv(26)%siName = 'J/KG H2O'
UnitConv(27)%siName = 'J/M2'
UnitConv(28)%siName = 'K/M'
UnitConv(29)%siName = 'KG'
UnitConv(30)%siName = 'KG/KG'
UnitConv(31)%siName = 'KG/M3'
UnitConv(32)%siName = 'KG/S'
UnitConv(33)%siName = 'KGWATER/KGAIR'
UnitConv(34)%siName = 'KGWATER/SEC'
UnitConv(35)%siName = 'KMOL/S'
UnitConv(36)%siName = 'KMOL/SEC'
UnitConv(37)%siName = 'KWH'
UnitConv(38)%siName = 'L'
UnitConv(39)%siName = 'L'
UnitConv(40)%siName = 'LUM/W'
UnitConv(41)%siName = 'LUX'
UnitConv(42)%siName = 'M'
UnitConv(43)%siName = 'M'
UnitConv(44)%siName = 'M/S'
UnitConv(45)%siName = 'M/S'
UnitConv(46)%siName = 'M2'
UnitConv(47)%siName = 'M2/PERSON'
UnitConv(48)%siName = 'M3'
UnitConv(49)%siName = 'M3'
UnitConv(50)%siName = 'M3/M2'
UnitConv(51)%siName = 'M3/S'
UnitConv(52)%siName = 'M3/S'
UnitConv(53)%siName = 'M3/S-M2'
UnitConv(54)%siName = 'M3/S-PERSON'
UnitConv(55)%siName = 'M3/S-PERSON'
UnitConv(56)%siName = 'PA'
UnitConv(57)%siName = 'PA'
UnitConv(58)%siName = 'PA'
UnitConv(59)%siName = 'PA'
UnitConv(60)%siName = 'PA'
UnitConv(61)%siName = 'PA'
UnitConv(62)%siName = 'PA'
UnitConv(63)%siName = 'PA'
UnitConv(64)%siName = 'S'
UnitConv(65)%siName = 'V'
UnitConv(66)%siName = 'W'
UnitConv(67)%siName = 'W'
UnitConv(68)%siName = 'W'
UnitConv(69)%siName = 'W'
UnitConv(70)%siName = 'W'
UnitConv(71)%siName = 'W/KG'
UnitConv(72)%siName = 'W/KG H2O'
UnitConv(73)%siName = 'W/K'
UnitConv(74)%siName = 'W/M2'
UnitConv(75)%siName = 'W/M2'
UnitConv(76)%siName = 'W/M2-C'
UnitConv(77)%siName = 'W/M2-K'
UnitConv(78)%siName = 'W/W'
UnitConv(79)%siName = 'deltaC'
UnitConv(80)%siName = 'KJ/KG'
UnitConv(81)%siName = 'W-S/M3'
UnitConv(82)%siName = 'W-S/M3'
UnitConv(83)%siName = '~~$~~/m2'
UnitConv(84)%siName = 'GJ'
UnitConv(85)%siName = 'GJ'
UnitConv(86)%siName = 'GJ'
UnitConv(87)%siName = 'GJ'
UnitConv(88)%siName = 'GJ'
UnitConv(89)%siName = 'GJ'
UnitConv(90)%siName = 'GJ'
UnitConv(91)%siName = 'MJ/m2'
UnitConv(92)%siName = 'MJ/m2'
UnitConv(93)%siName = 'MJ/m2'
UnitConv(94)%siName = 'Invalid/Undefined'

UnitConv(1)%ipName = '%'
UnitConv(2)%ipName = 'F'
UnitConv(3)%ipName = '0=Off 1=On'
UnitConv(4)%ipName = '0-No  1-Yes'
UnitConv(5)%ipName = '1-Yes 0-No'
UnitConv(6)%ipName = 'A'
UnitConv(7)%ipName = 'ACH'
UnitConv(8)%ipName = 'ach'
UnitConv(9)%ipName = 'base 50F'
UnitConv(10)%ipName = 'base 65F'
UnitConv(11)%ipName = 'F'
UnitConv(12)%ipName = 'cd/in2'
UnitConv(13)%ipName = 'deg'
UnitConv(14)%ipName = 'Frac'
UnitConv(15)%ipName = 'Hour'
UnitConv(16)%ipName = 'Hours'
UnitConv(17)%ipName = 'hr'
UnitConv(18)%ipName = 'hrs'
UnitConv(19)%ipName = 'kBtu'
UnitConv(20)%ipName = 'kWh'
UnitConv(21)%ipName = 'therm'
UnitConv(22)%ipName = 'MMBtu'
UnitConv(23)%ipName = 'Wh'
UnitConv(24)%ipName = 'ton-hrs'
UnitConv(25)%ipName = 'Btu/lb'
UnitConv(26)%ipName = 'Btu/lbWater'
UnitConv(27)%ipName = 'kBtu/sqft'
UnitConv(28)%ipName = 'F/ft'
UnitConv(29)%ipName = 'lb'
UnitConv(30)%ipName = 'lb/lb'
UnitConv(31)%ipName = 'lb/ft3'
UnitConv(32)%ipName = 'lb/s'
UnitConv(33)%ipName = 'lbWater/lbAir'
UnitConv(34)%ipName = 'lbWater/s'
UnitConv(35)%ipName = 'kmol/s'
UnitConv(36)%ipName = 'kmol/sec'
UnitConv(37)%ipName = 'kWh'
UnitConv(38)%ipName = 'gal'
UnitConv(39)%ipName = 'ft3'
UnitConv(40)%ipName = 'lum/W'
UnitConv(41)%ipName = 'foot-candles'
UnitConv(42)%ipName = 'ft'
UnitConv(43)%ipName = 'in'
UnitConv(44)%ipName = 'ft/min'
UnitConv(45)%ipName = 'miles/hr'
UnitConv(46)%ipName = 'ft2'
UnitConv(47)%ipName = 'ft2/person'
UnitConv(48)%ipName = 'ft3'
UnitConv(49)%ipName = 'gal'
UnitConv(50)%ipName = 'f3/f2'
UnitConv(51)%ipName = 'ft3/min'
UnitConv(52)%ipName = 'gal/min'
UnitConv(53)%ipName = 'ft3/min-ft2'
UnitConv(54)%ipName = 'ft3/min-person'
UnitConv(55)%ipName = 'gal/min-person'
UnitConv(56)%ipName = 'psi'
UnitConv(57)%ipName = 'inHg'
UnitConv(58)%ipName = 'inH2O'
UnitConv(59)%ipName = 'ftH2O'
UnitConv(60)%ipName = 'psi'
UnitConv(61)%ipName = 'inHg'
UnitConv(62)%ipName = 'inH2O'
UnitConv(63)%ipName = 'ftH2O'
UnitConv(64)%ipName = 's'
UnitConv(65)%ipName = 'V'
UnitConv(66)%ipName = 'Btu/h'
UnitConv(67)%ipName = 'W'
UnitConv(68)%ipName = 'kW'
UnitConv(69)%ipName = 'kBtuh'
UnitConv(70)%ipName = 'ton'
UnitConv(71)%ipName = 'kBtuh/lb'
UnitConv(72)%ipName = 'kBtuh/lb'
UnitConv(73)%ipName = 'Btu/h-F'
UnitConv(74)%ipName = 'Btu/h-ft2'
UnitConv(75)%ipName = 'kBtuh/ft2'
UnitConv(76)%ipName = 'Btu/h-ft2-F'
UnitConv(77)%ipName = 'Btu/h-ft2-F'
UnitConv(78)%ipName = 'Btuh/Btuh'
UnitConv(79)%ipName = 'deltaF'
UnitConv(80)%ipName = 'Btu/lb'
UnitConv(81)%ipName = 'W-min/ft3'
UnitConv(82)%ipName = 'W-min/gal'
UnitConv(83)%ipName = '~~$~~/ft2'
UnitConv(84)%ipName = 'kBtu'
UnitConv(85)%ipName = 'kWh'
UnitConv(86)%ipName = 'kWh'
UnitConv(87)%ipName = 'therm'
UnitConv(88)%ipName = 'MMBtu'
UnitConv(89)%ipName = 'Wh'
UnitConv(90)%ipName = 'ton-hrs'
UnitConv(91)%ipName = 'kWh/ft2'
UnitConv(92)%ipName = 'kBtu/ft2'
UnitConv(93)%ipName = 'kBtu/ft2'
UnitConv(94)%ipName = 'Invalid/Undefined'

UnitConv(1)%mult = 1.d0
UnitConv(2)%mult = 1.8d0
UnitConv(3)%mult = 1.d0
UnitConv(4)%mult = 1.d0
UnitConv(5)%mult = 1.d0
UnitConv(6)%mult = 1.d0
UnitConv(7)%mult = 1.d0
UnitConv(8)%mult = 1.d0
UnitConv(9)%mult = 1.8d0
UnitConv(10)%mult = 1.8d0
UnitConv(11)%mult = 1.8d0
UnitConv(12)%mult = 0.000645160041625726d0
UnitConv(13)%mult = 1.d0
UnitConv(14)%mult = 1.d0
UnitConv(15)%mult = 1.d0
UnitConv(16)%mult = 1.d0
UnitConv(17)%mult = 1.d0
UnitConv(18)%mult = 1.d0
UnitConv(19)%mult = 0.00000094845d0
UnitConv(20)%mult = 0.000000277778d0
UnitConv(21)%mult = 0.0000000094845d0
UnitConv(22)%mult = 0.00000000094845d0
UnitConv(23)%mult = 0.000277777777777778d0
UnitConv(24)%mult = 0.0000000789847d0
UnitConv(25)%mult = 0.00042956d0
UnitConv(26)%mult = 0.0000004302105d0
UnitConv(27)%mult = 0.00000008811404d0
UnitConv(28)%mult = 0.54861322767449d0
UnitConv(29)%mult = 2.2046d0
UnitConv(30)%mult = 1.d0
UnitConv(31)%mult = 0.062428d0
UnitConv(32)%mult = 2.2046d0
UnitConv(33)%mult = 1.d0
UnitConv(34)%mult = 2.2046d0
UnitConv(35)%mult = 1.d0
UnitConv(36)%mult = 1.d0
UnitConv(37)%mult = 1.d0
UnitConv(38)%mult = 0.264172037284185d0
UnitConv(39)%mult = 0.0353146624712848d0
UnitConv(40)%mult = 1.d0
UnitConv(41)%mult = 0.092902267d0
UnitConv(42)%mult = 3.281d0
UnitConv(43)%mult = 39.37d0
UnitConv(44)%mult = 196.86d0
UnitConv(45)%mult = 2.2369d0
UnitConv(46)%mult = 10.764961d0
UnitConv(47)%mult = 10.764961d0
UnitConv(48)%mult = 35.319837041d0
UnitConv(49)%mult = 264.172d0
UnitConv(50)%mult = 3.281d0
UnitConv(51)%mult = 2118.6438d0
UnitConv(52)%mult = 15852.d0
UnitConv(53)%mult = 196.85d0
UnitConv(54)%mult = 2118.6438d0
UnitConv(55)%mult = 15852.d0
UnitConv(56)%mult = 0.0001450377d0
UnitConv(57)%mult = 0.00029613d0
UnitConv(58)%mult = 0.00401463d0
UnitConv(59)%mult = 0.00033455d0
UnitConv(60)%mult = 0.0001450377d0
UnitConv(61)%mult = 0.00029613d0
UnitConv(62)%mult = 0.00401463d0
UnitConv(63)%mult = 0.00033455d0
UnitConv(64)%mult = 1.d0
UnitConv(65)%mult = 1.d0
UnitConv(66)%mult = 3.412d0
UnitConv(67)%mult = 1.d0
UnitConv(68)%mult = 0.001d0
UnitConv(69)%mult = 0.00341442d0
UnitConv(70)%mult = 0.0002843333d0
UnitConv(71)%mult = 0.001547673d0
UnitConv(72)%mult = 0.001547673d0
UnitConv(73)%mult = 1.8987d0
UnitConv(74)%mult = 0.316954237d0
UnitConv(75)%mult = 0.000316954237d0
UnitConv(76)%mult = 0.176085687d0
UnitConv(77)%mult = 0.176085687d0
UnitConv(78)%mult = 1.d0
UnitConv(79)%mult = 1.8d0
UnitConv(80)%mult = 0.42956d0
UnitConv(81)%mult = 1.0d0/2118.6438d0
UnitConv(82)%mult = 1.0d0/15852d0
UnitConv(83)%mult = 1.0d0/10.764961d0
UnitConv(84)%mult = 0.00000094845d0 * 1000000000d0
UnitConv(85)%mult = 0.000000277778d0 * 1000000000d0
UnitConv(86)%mult = 0.000000277778d0 * 1000000000d0
UnitConv(87)%mult = 0.0000000094845d0 * 1000000000d0
UnitConv(88)%mult = 0.00000000094845d0 * 1000000000d0
UnitConv(89)%mult = 0.000277777777777778d0 * 1000000000d0
UnitConv(90)%mult = 0.0000000789847d0 * 1000000000d0
UnitConv(91)%mult = 0.277777777777778d0/10.764961d0
UnitConv(92)%mult = 0.94708628903179d0/10.764961d0
UnitConv(93)%mult = 0.94708628903179d0/10.764961d0
UnitConv(94)%mult = 1.0d0


UnitConv(2)%offset = 32.d0
UnitConv(11)%offset = 32.d0
UnitConv(25)%offset = 7.6736d0
UnitConv(80)%offset = 7.6736d0  ! 80 is KJ/KG -- should this be multiplied by 1000?

UnitConv(20)%hint = 'ELEC'
UnitConv(21)%hint = 'GAS'
UnitConv(24)%hint = 'COOL'
UnitConv(38)%hint = 'WATER'
UnitConv(49)%hint = 'WATER'
UnitConv(52)%hint = 'WATER'
UnitConv(67)%hint = 'ELEC'
UnitConv(70)%hint = 'COOL'
UnitConv(82)%hint = 'WATER'
UnitConv(85)%hint = 'CONSUMP'
UnitConv(86)%hint = 'ELEC'
UnitConv(87)%hint = 'GAS'
UnitConv(90)%hint = 'COOL'
UnitConv(91)%hint = 'ELEC'
UnitConv(92)%hint = 'GAS'
UnitConv(92)%hint = 'ADDITIONAL'

UnitConv(19)%several = .TRUE.
UnitConv(20)%several = .TRUE.
UnitConv(21)%several = .TRUE.
UnitConv(22)%several = .TRUE.
UnitConv(23)%several = .TRUE.
UnitConv(24)%several = .TRUE.
UnitConv(38)%several = .TRUE.
UnitConv(39)%several = .TRUE.
UnitConv(42)%several = .TRUE.
UnitConv(43)%several = .TRUE.
UnitConv(44)%several = .TRUE.
UnitConv(45)%several = .TRUE.
UnitConv(48)%several = .TRUE.
UnitConv(49)%several = .TRUE.
UnitConv(51)%several = .TRUE.
UnitConv(52)%several = .TRUE.
UnitConv(54)%several = .TRUE.
UnitConv(55)%several = .TRUE.
UnitConv(56)%several = .TRUE.
UnitConv(57)%several = .TRUE.
UnitConv(58)%several = .TRUE.
UnitConv(59)%several = .TRUE.
UnitConv(60)%several = .TRUE.
UnitConv(61)%several = .TRUE.
UnitConv(62)%several = .TRUE.
UnitConv(63)%several = .TRUE.
UnitConv(66)%several = .TRUE.
UnitConv(67)%several = .TRUE.
UnitConv(68)%several = .TRUE.
UnitConv(69)%several = .TRUE.
UnitConv(70)%several = .TRUE.
UnitConv(74)%several = .TRUE.
UnitConv(75)%several = .TRUE.
UnitConv(81)%several = .TRUE.
UnitConv(82)%several = .TRUE.
UnitConv(84)%several = .TRUE.
UnitConv(85)%several = .TRUE.
UnitConv(86)%several = .TRUE.
UnitConv(87)%several = .TRUE.
UnitConv(88)%several = .TRUE.
UnitConv(89)%several = .TRUE.
UnitConv(90)%several = .TRUE.
UnitConv(91)%several = .TRUE.
UnitConv(92)%several = .TRUE.
END SUBROUTINE SetupUnitConversions

FUNCTION GetUnitSubString(inString) RESULT (outUnit)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   return the substring contained in brackets []
          !   that contains the units.

          ! METHODOLOGY EMPLOYED:
          !   na
IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: inString             ! Input String
CHARACTER(len=LEN(inString)) :: outUnit             ! Result String

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: posLBrac
INTEGER :: posRBrac

outUnit = " "
!check if string has brackets or parentheses
posLBrac = INDEX(inString, '[')  ! left bracket
posRBrac = INDEX(inString, ']')  ! right bracket
!extract the substring with the units
IF ((posLBrac .GT. 0) .AND. (posRBrac .GT. 0) .AND. ((posRBrac - posLBrac) .GE. 2)) THEN
  outUnit = inString(posLBrac+1:posRBrac-1)
ELSE
  outUnit = " "
END IF
END FUNCTION GetUnitSubString

SUBROUTINE LookupSItoIP(stringInWithSI,unitConvIndex,stringOutWithIP)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 12, 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   The input string to this subroutine can either contain
          !   a unit that should be looked up or it can contain
          !   but the unit and other text (such as the column heading)
          !   that includes a hint for when the unit may have multiple
          !   possible conversions. If the input string includes
          !   just the unit it does not have either brackets or
          !   parenthesis. If the string includes text with a possible
          !   hint the units themselves will be in either brackets
          !   or parentheses. The index to the unitConv array is returned
          !   which can be used with the convertIP function. Also the
          !   string with the IP units substituted is returned.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
          !    na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN)  :: stringInWithSI
INTEGER,INTENT(OUT)           :: unitConvIndex
CHARACTER(len=*), INTENT(OUT) :: stringOutWithIP

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
CHARACTER(len=MaxNameLength) :: unitSIOnly = ''
INTEGER :: posLBrac = 0
INTEGER :: posRBrac = 0
INTEGER :: posLParen = 0
INTEGER :: posRParen = 0
INTEGER :: modeInString
INTEGER,PARAMETER :: misBrac = 1
INTEGER,PARAMETER :: misParen = 2
INTEGER,PARAMETER :: misNoHint = 3
INTEGER :: iUnit = 0
INTEGER :: defaultConv = 0
INTEGER :: foundConv = 0
INTEGER :: firstOfSeveral = 0
CHARACTER(len=MaxNameLength*2) :: stringInUpper  ! *2 to take care of >100 characters in string.
INTEGER :: selectedConv = 0

stringOutWithIP = ''
stringInUpper = MakeUPPERCase(stringInWithSI)
!check if string has brackets or parentheses
posLBrac = INDEX(stringInUpper, '[')  ! left bracket
posRBrac = INDEX(stringInUpper, ']')  ! right bracket
posLParen = INDEX(stringInUpper, '(')  ! left parenthesis
posRParen = INDEX(stringInUpper, ')')  ! right parenthesis
!extract the substring with the units
IF ((posLBrac .GT. 0) .AND. (posRBrac .GT. 0) .AND. ((posRBrac - posLBrac) .GE. 2)) THEN
  unitSIOnly = stringInUpper(posLBrac+1:posRBrac-1)
  modeInString = misBrac
ELSEIF ((posLParen .GT. 0) .AND. (posRParen .GT. 0) .AND. ((posRParen - posLParen) .GE. 2)) THEN
  unitSIOnly = stringInUpper(posLParen+1:posRParen-1)
  modeInString = misParen
ELSE
  unitSIOnly = stringInUpper
  modeInString = misNoHint
END IF
defaultConv = 0
foundConv = 0
firstOfSeveral = 0
DO iUnit = 1,UnitConvSize
  IF (SameString(UnitConv(iUnit)%siName,unitSIOnly)) THEN
    IF (UnitConv(iUnit)%several) THEN
      IF (firstofSeveral .EQ. 0) firstOfSeveral = iUnit
      IF (UnitConv(iUnit)%default) defaultConv = iUnit
      ! look for the hint string
      IF (LEN_TRIM(UnitConv(iUnit)%hint) .GT. 0) THEN
        IF (INDEX(stringInUpper, TRIM(UnitConv(iUnit)%hint)) .GT. 0) THEN
          foundConv = iUnit
          EXIT
        END IF
      END IF
    ELSE !not several possibilities so don't bother with rest of array
      foundConv = iUnit
      EXIT
    END IF
  END IF
END DO
! if it is found set the selected value to what was found. if not found,
! directly set it to the default and if no default set it to the first item
! in group.  Return zero if not found.
IF (foundConv .GT. 0) THEN
  selectedConv = foundConv
ELSE
  ! not found - see if in a group it should be default or first.
  IF (firstOfSeveral .EQ. 0) THEN
    selectedConv = 0
  ELSE
    IF (defaultConv .NE. 0) THEN
      selectedConv = defaultConv
    ELSE
      selectedConv = firstOfSeveral
    END IF
  END IF
END IF
! if one was selected substitute the units into the output string
IF (selectedConv .GT. 0) THEN
  SELECT CASE (modeInString)
    CASE (misBrac)
      stringOutWithIP = stringInWithSI(1:posLBrac) // &
         TRIM(UnitConv(selectedConv)%ipName) // stringInWithSI(posRBrac:)
    CASE (misParen)
      stringOutWithIP = stringInWithSI(1:posLParen) // &
         TRIM(UnitConv(selectedConv)%ipName) // stringInWithSI(posRParen:)
    CASE (misNoHint)
      stringOutWithIP = TRIM(UnitConv(selectedConv)%ipName)
  END SELECT
ELSE
  ! if no conversion just output the input string
  stringOutWithIP = stringInWithSI
END IF
! For debugging only
!CALL  ShowWarningError('LookupSItoIP in: ' // TRIM(stringInWithSI) // ' out: ' // TRIM(stringOutWithIP))
!IF (foundConv .NE. 0) CALL  ShowWarningError('   Hint ' // TRIM(UnitConv(foundConv)%hint) // IntToStr(foundConv) )
unitConvIndex = selectedConv
END SUBROUTINE

REAL(r64) FUNCTION ConvertIP(unitConvIndex,SIvalue)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 13, 2009
          !    MODIFIED       September 2012
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Apply the selected unit conversion to the input value
          !   expressed in SI units to result in IP units.
          !   If zero is provided as unit index, return the original
          !   value (no conversion)

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)                      :: unitConvIndex
REAL(r64), INTENT(IN)                   :: SIvalue

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
IF (unitConvIndex .EQ. 0) THEN
  ConvertIP = SIvalue
ELSEIF ((unitConvIndex .GT. 0) .AND. (unitConvIndex .LE. UnitConvSize)) THEN
  ConvertIP = (SIvalue * UnitConv(unitConvIndex)%mult) + UnitConv(unitConvIndex)%offset
ELSE
  ConvertIP = 0.0d0
END IF
END FUNCTION ConvertIP

REAL(r64) FUNCTION ConvertIPdelta(unitConvIndex,SIvalue)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 18, 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Apply the selected unit conversion to the input value
          !   expressed in SI units to result in IP units. This routine
          !   only uses the mulitplier and NOT the offset and is appropriate
          !   when the number being converted is a difference or delta
          !   between values (such as a temperature difference).

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)                      :: unitConvIndex
REAL(r64), INTENT(IN)                   :: SIvalue

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na

IF ((unitConvIndex .GT. 0) .AND. (unitConvIndex .LE. UnitConvSize)) THEN
  ConvertIPdelta = SIvalue * UnitConv(unitConvIndex)%mult
ELSE
  ConvertIPdelta = 0.0d0
END IF
END FUNCTION ConvertIPdelta


SUBROUTINE GetUnitConversion(unitConvIndex,multiplier,offset,IPunit)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 13, 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Return of the multiplier and adder for the given
          !   SI to IP unit conversion.
          !
          !     SI = (IP * multipier) + offset
          !
          !  This function could be replaced by referencing the
          !  array directly but does include some checking of the
          !  bounds of the array.


          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN)                        :: unitConvIndex
REAL(r64), INTENT(OUT)                    :: multiplier
REAL(r64), INTENT(OUT)                    :: offset
CHARACTER(len=MaxNameLength), INTENT(OUT) :: IPunit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !    na
IF ((unitConvIndex .GT. 0) .AND. (unitConvIndex .LE. UnitConvSize)) THEN
  multiplier = UnitConv(unitConvIndex)%mult
  offset = UnitConv(unitConvIndex)%offset
  IPunit = TRIM(UnitConv(unitConvIndex)%ipName)
ELSE
  multiplier = 0.0d0
  offset = 0.0d0
  IPunit = ''
END IF
END SUBROUTINE GetUnitConversion

REAL(r64) FUNCTION getSpecificUnitMultiplier(SIunit,IPunit)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 13, 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Return of the multiplier for a specific
          !   SI to IP unit conversion. No offset is provided so
          !   it cannot be used to convert units such as temperatures
          !   that include an offset.
          !
          !     SI = (IP * multipier) + offset
          !
          !   Unlike LookupSItoIP, this function does not expect more
          !   the units in the two input parameters. No hints or
          !   defaults are used since both the SI and IP units are
          !   input by the user.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: SIunit
CHARACTER(len=*), INTENT(IN) :: IPunit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: found = 0
INTEGER :: iUnit = 0

DO iUnit = 1,UnitConvSize
  IF (SameString(UnitConv(iUnit)%siName,SIunit)) THEN
    IF (SameString(UnitConv(iUnit)%ipName,IPunit)) THEN
      found = iUnit
      EXIT
    END IF
  END IF
END DO
IF (found .NE. 0) THEN
  getSpecificUnitMultiplier = UnitConv(found)%mult
ELSE
  getSpecificUnitMultiplier = 0.0d0
END IF
END FUNCTION getSpecificUnitMultiplier


REAL(r64) FUNCTION getSpecificUnitDivider(SIunit,IPunit)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   February 13, 2009
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Returns the divider (1/multiplier) for a specific
          !   SI to IP unit conversion. No offset is provided so
          !   it cannot be used to convert units such as temperatures
          !   that include an offset.
          !
          !     SI = (IP * multipier) + offset
          !
          !   Unlike LookupSItoIP, this function does not expect more
          !   the units in the two input parameters. No hints or
          !   defaults are used since both the SI and IP units are
          !   input by the user.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: SIunit
CHARACTER(len=*), INTENT(IN) :: IPunit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: mult

mult = getSpecificUnitMultiplier(SIunit,IPunit)
IF (mult .NE. 0) THEN
  getSpecificUnitDivider = 1/mult
ELSE
  getSpecificUnitDivider = 0.0d0
END IF
END FUNCTION getSpecificUnitDivider

REAL(r64) FUNCTION getSpecificUnitIndex(SIunit,IPunit)
          ! SUBROUTINE INFORMATION:
          !    AUTHOR         Jason Glazer of GARD Analytics, Inc.
          !    DATE WRITTEN   September 21, 2012
          !    MODIFIED       na
          !    RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Return of the unit conversion index for a specific
          !   SI to IP unit conversion.
          !
          !   Unlike LookupSItoIP, this function does not expect more
          !   the units in the two input parameters. No hints or
          !   defaults are used since both the SI and IP units are
          !   input by the user.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          !    na

          ! USE STATEMENTS:
IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
CHARACTER(len=*), INTENT(IN) :: SIunit
CHARACTER(len=*), INTENT(IN) :: IPunit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !    na

          ! INTERFACE BLOCK SPECIFICATIONS:
          !    na

          ! DERIVED TYPE DEFINITIONS:
          !    na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: found = 0
INTEGER :: iUnit = 0

DO iUnit = 1,UnitConvSize
  IF (SameString(UnitConv(iUnit)%siName,SIunit)) THEN
    IF (SameString(UnitConv(iUnit)%ipName,IPunit)) THEN
      found = iUnit
      EXIT
    END IF
  END IF
END DO
IF (found .NE. 0) THEN
  getSpecificUnitIndex = found
ELSE
  getSpecificUnitIndex = 0.0d0
END IF
END FUNCTION getSpecificUnitIndex

!     NOTICE
!
!     Copyright © 1996-2013 The Board of Trustees of the University of Illinois
!     and The Regents of the University of
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

END MODULE OutputReportTabular

