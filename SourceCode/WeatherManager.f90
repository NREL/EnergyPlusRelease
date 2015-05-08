MODULE WeatherManager          ! EnergyPlus Simulation Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 1997
          !       MODIFIED       December 1998, FW; December 1999, LKL.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contains all of the weather handling routines for
          ! EnergyPlus.  That includes getting user input, defining design day
          ! weather, retrieving data from weather files, and supplying the
          ! outdoor environment for each time step.

          ! METHODOLOGY EMPLOYED:
          ! Setting up the design days is similar to BLAST/IBLAST.  Reading the
          ! BLAST weather files is similar to that code in BLAST/IBLAST.  The EnergyPlus
          ! Weather file (EPW) is new code.

          ! REFERENCES:
          ! (I)BLAST legacy code, internal Reverse Engineering documentation,
          ! and internal Evolutionary Engineering documentation.

          ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataReportingFlags
USE DataInterfaces
USE DataSystemVariables, ONLY: iASCII_CR, iUnicode_end

USE General, ONLY: ProcessDateString, RoundSigDigits !, ValidateMonthDay
USE Psychrometrics


IMPLICIT NONE    ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
    ! Following are Date Types read in from EPW file or IDF
INTEGER, PARAMETER :: InvalidDate=-1
INTEGER, PARAMETER :: MonthDay=1
INTEGER, PARAMETER :: NthDayInMonth=2
INTEGER, PARAMETER :: LastDayInMonth=3

INTEGER, PARAMETER :: ScheduleMethod = 1     ! Constant for water mains temperatures calculation methods
INTEGER, PARAMETER :: CorrelationMethod = 2  ! Constant for water mains temperatures calculation methods

INTEGER, PARAMETER :: InvalidWeatherFile = 0
INTEGER, PARAMETER :: EPlusWeatherFile = 1

INTEGER, PARAMETER :: ASHRAE_ClearSky       =0  ! Design Day solar model ASHRAE ClearSky (default)
INTEGER, PARAMETER :: Zhang_Huang           =1  ! Design Day solar model Zhang Huang
INTEGER, PARAMETER :: SolarModel_Schedule   =2  ! Design Day solar model (beam and diffuse) from user entered schedule
INTEGER, PARAMETER :: ASHRAE_Tau            =3  ! Design Day solar model ASHRAE tau (per 2009 HOF)

INTEGER, PARAMETER :: DDHumIndType_Wetbulb   =0 ! Design Day Humidity Indicating Type = Wetbulb (default)
INTEGER, PARAMETER :: DDHumIndType_Dewpoint  =1 ! Design Day Humidity Indicating Type = Dewpoint
INTEGER, PARAMETER :: DDHumIndType_Enthalpy  =2 ! Design Day Humidity Indicating Type = Enthalpy
INTEGER, PARAMETER :: DDHumIndType_HumRatio  =3 ! Design Day Humidity Indicating Type = Humidity Ratio
INTEGER, PARAMETER :: DDHumIndType_RelHumSch =4 ! Design Day Humidity Indicating Type = relhum schedule
INTEGER, PARAMETER :: DDHumIndType_WBProfDef =5 ! Design Day Humidity Indicating Type = Wetbulb default profile
INTEGER, PARAMETER :: DDHumIndType_WBProfDif =6 ! Design Day Humidity Indicating Type = Wetbulb difference profile
INTEGER, PARAMETER :: DDHumIndType_WBProfMul =7 ! Design Day Humidity Indicating Type = Wetbulb multiplier profile
INTEGER, PARAMETER :: DDHumIndType_Count = 8    ! # of DDHumIndTypes


INTEGER, PARAMETER :: DDDBRangeType_Default    = 0 ! Design Day DryBulb Range Type = Default Multipliers
INTEGER, PARAMETER :: DDDBRangeType_Multiplier = 1 ! Design Day DryBulb Range Type = Multiplier Schedule
INTEGER, PARAMETER :: DDDBRangeType_Difference = 2 ! Design Day DryBulb Range Type = Difference Schedule
INTEGER, PARAMETER :: DDDBRangeType_Profile    = 3 ! Design Day DryBulb Range Type = Temperature Profile

INTEGER, PARAMETER :: WP_ScheduleValue         = 1 ! User entered Schedule value for Weather Property
INTEGER, PARAMETER :: WP_DryBulbDelta          = 2 ! User entered DryBulb difference Schedule value for Weather Property
INTEGER, PARAMETER :: WP_DewPointDelta         = 3 ! User entered Dewpoint difference Schedule value for Weather Property
INTEGER, PARAMETER :: WP_SkyTAlgorithmA        = 4 ! place holder

INTEGER, PARAMETER :: GregorianToJulian        = 1 ! JGDate argument for Gregorian to Julian Date conversion
INTEGER, PARAMETER :: JulianToGregorian        = 2 ! JGDate argument for Julian to Gregorian Date conversion

REAL(r64),  PARAMETER :: Sigma=5.6697d-8 ! Stefan-Boltzmann constant
REAL(r64),  PARAMETER :: TKelvin=KelvinConv  ! conversion from Kelvin to Celsius

CHARACTER(len=*),  PARAMETER :: Blank=' '
CHARACTER(len=*),  PARAMETER, DIMENSION(7) :: DaysOfWeek=(/"SUNDAY   ","MONDAY   ","TUESDAY  ", &
                                                           "WEDNESDAY","THURSDAY ","FRIDAY   ","SATURDAY "/)

LOGICAL :: Debugout=.false.

          ! DERIVED TYPE DEFINITIONS:

TYPE EnvironmentData
  CHARACTER(len=100) :: Title           = Blank   ! Environment name
  CHARACTER(len=100) :: cKindOfEnvrn    = Blank   ! kind of environment
  INTEGER :: KindOfEnvrn                = 0       ! Type of environment (see Parameters for KindOfSim in DataGlobals)
  INTEGER :: TotalDays                  = 0       ! Number of days in environment
  INTEGER :: StartJDay                  = 0       ! Day of year of first day of environment
  INTEGER :: StartMonth                 = 0
  INTEGER :: StartDay                   = 0
  INTEGER :: StartYear                  = 0
  INTEGER :: StartDate                  = 0
  INTEGER :: EndMonth                   = 0
  INTEGER :: EndDay                     = 0
  INTEGER :: EndJDay                    = 0
  INTEGER :: EndYear                    = 0
  INTEGER :: EndDate                    = 0
  INTEGER :: DayOfWeek                  = 0       ! Starting Day of Week for the (Weather) RunPeriod (User Input)
  LOGICAL :: UseDST                     = .false. ! True if DaylightSavingTime is used for this RunPeriod
  LOGICAL :: UseHolidays                = .false. ! True if Holidays are used for this RunPeriod (from WeatherFile)
  LOGICAL :: ApplyWeekendRule           = .false. ! True if "Weekend Rule" is to be applied to RunPeriod
  LOGICAL :: UseRain                    = .true.  ! True if Rain from weather file should be used (set rain to true)
  LOGICAL :: UseSnow                    = .true.  ! True if Snow from weather file should be used (set Snow to true)
  INTEGER, DIMENSION(12) :: MonWeekDay  = 0
  LOGICAL :: SetWeekDays                =.false.  ! true when weekdays will be reset (after first year or on repeat)
  INTEGER :: NumSimYears                = 1 ! Total Number of times this period to be performed
  INTEGER :: CurrentCycle               = 0 ! Current cycle through weather file in NumSimYears repeats
  INTEGER :: WP_Type1                   = 0 ! WeatherProperties SkyTemperature Pointer
  INTEGER :: CurrentYear                = 0       ! Current year
  LOGICAL :: IsLeapYear                 =.false.  ! True if current year is leap year.
  LOGICAL :: RollDayTypeOnRepeat        =.true.   ! If repeating run period, increment day type on repeat.
  LOGICAL :: TreatYearsAsConsecutive    =.true.   ! When year rolls over, increment year and recalculate Leap Year
  LOGICAL :: MatchYear                  = .false. ! for actual weather will be true
  LOGICAL :: ActualWeather              = .false. ! true when using actual weather data
  INTEGER :: RawSimDays                 = 0       ! number of basic sim days.
END TYPE EnvironmentData

TYPE DesignDayData
  CHARACTER(len=MaxNameLength) :: Title = Blank     ! Environment name
  REAL(r64) :: MaxDryBulb               = 0.0d0     ! Maximum Dry-Bulb Temperature (C)
  REAL(r64) :: DailyDBRange             = 0.0d0     ! Daily Temperature Range (deltaC)
  REAL(r64) :: HumIndValue              = 0.0d0     ! Humidity Indicating Value at Max Dry-bulb Temperature
  INTEGER   :: HumIndType               = 0       ! Humidity Indicating type  (see Parameters)
  REAL(r64) :: PressBarom               = 0.0d0     ! Atmospheric/Barometric Pressure (Pascals)
  REAL(r64) :: WindSpeed                = 0.0d0     ! Wind Speed (m/s)
  REAL(r64) :: WindDir                  = 0.0d0     ! Wind Direction (degrees clockwise from North, N=0, E=90, S=180, W=270)
  REAL(r64) :: SkyClear                 = 0.0d0     ! Sky Clearness (0 to 1)
  INTEGER :: RainInd                    = 0       ! Rain Indicator (1 = raining and surfaces are wet, else 0)
  INTEGER :: SnowInd                    = 0       ! Snow Indicator (1 = snow on ground, else  0)
  INTEGER :: DayOfMonth                 = 0       ! Day of Month ( 1 - 31 )
  INTEGER :: Month                      = 0       ! Month of Year ( 1 - 12 )
  INTEGER :: DayType                    = 0       ! Day Type Sunday = 1 - Saturday = 7
  INTEGER :: DSTIndicator               = 0       ! Daylight Saving Time Period Indicator (1=yes, 0=no) for this DesignDay
  INTEGER :: SolarModel                 = 0       ! Solar Model for creating solar values for design day.
  INTEGER :: DBTempRangeType            = 0       ! Drybulb Range Type (see Parameters)
  INTEGER :: TempRangeSchPtr            = 0       ! Schedule pointer to a day schedule for dry-bulb temperature range multipliers
  INTEGER :: HumIndSchPtr               = 0       ! Schedule pointer to a day schedule that specifies
                                                  !    relative humidity (%) or wet-bulb range multipliers per HumIndType
  INTEGER :: BeamSolarSchPtr            = 0       ! Schedule pointer to a day schedule for beam solar
  INTEGER :: DiffuseSolarSchPtr         = 0       ! Schedule pointer to a day schedule for diffuse solar
  REAL(r64) :: TauB                     = 0.0d0     ! beam pseudo optical depth for ASHRAE tau model
  REAL(r64) :: TauD                     = 0.0d0     ! diffuse pseudo optical depth for ASHRAE tau model
  REAL(r64) :: DailyWBRange             = 0.0d0     ! daily range of wetbulb (deltaC)
  LOGICAL   :: PressureEntered          =.false.  ! true if a pressure was entered in design day data
  LOGICAL   :: DewPointNeedsSet         =.false.  ! true if the Dewpoint humidicating value needs to be set (after location determined)
END TYPE DesignDayData

TYPE RunPeriodData
  CHARACTER(len=MaxNameLength) :: Title = Blank
  CHARACTER(len=MaxNameLength) :: PeriodType = Blank
  INTEGER :: TotalDays                  = 0       ! total number of days in requested period
  INTEGER :: StartMonth                 = 1
  INTEGER :: StartDay                   = 1
  INTEGER :: StartDate                  = 0       ! Calculated start date (Julian) for a weather file run period
  INTEGER :: StartYear                  = 0       ! entered in "consecutive"/real runperiod object
  INTEGER :: EndMonth                   = 12
  INTEGER :: EndDay                     = 31
  INTEGER :: EndDate                    = 0       ! Calculated end date (Julian) for a weather file run period
  INTEGER :: EndYear                    = 0       ! entered in "consecutive"/real runperiod object
  INTEGER :: DayOfWeek                  = 0       ! Day of Week that the RunPeriod will start on (User Input)
  LOGICAL :: UseDST                     = .false. ! True if DaylightSavingTime is used for this RunPeriod
  LOGICAL :: UseHolidays                = .false. ! True if Holidays are used for this RunPeriod (from WeatherFile)
  LOGICAL :: ApplyWeekendRule           = .false. ! True if "Weekend Rule" is to be applied to RunPeriod
  LOGICAL :: UseRain                    = .true.  ! True if Rain from weather file should be used (set rain to true)
  LOGICAL :: UseSnow                    = .true.  ! True if Snow from weather file should be used (set Snow to true)
  INTEGER, DIMENSION(12) :: MonWeekDay  = 0
  INTEGER :: NumSimYears                = 1       ! Total Number of years of simulation to be performed
  INTEGER :: BeginYear                  = 0       ! Start year entered in regular RunPeriod object
  LOGICAL :: IsLeapYear                 =.false.  ! True if Begin Year is leap year.
  LOGICAL :: RollDayTypeOnRepeat        =.true.   ! If repeating run period, increment day type on repeat.
  LOGICAL :: TreatYearsAsConsecutive    =.true.   ! When year rolls over, increment year and recalculate Leap Year
  LOGICAL :: ActualWeather              = .false. ! true when using actual weather data
END TYPE RunPeriodData

TYPE DayWeatherVariables                ! Derived Type for Storing Weather "Header" Data
  INTEGER :: DayOfYear                  = 0       ! Day of year for weather data
  INTEGER :: Year                       = 0       ! Year of weather data
  INTEGER :: Month                      = 0       ! Month of weather data
  INTEGER :: DayOfMonth                 = 0       ! Day of month for weather data
  INTEGER :: DayOfWeek                  = 0       ! Day of week for weather data
  INTEGER :: DaylightSavingIndex        = 0       ! Daylight Saving Time Period indicator (0=no,1=yes)
  INTEGER :: HolidayIndex               = 0       ! Holiday indicator (0=no holiday, non-zero=holiday type)
  REAL(r64)    :: SinSolarDeclinAngle        = 0.0d0     ! Sine of the solar declination angle
  REAL(r64)    :: CosSolarDeclinAngle        = 0.0d0     ! Cosine of the solar declination angle
  REAL(r64)    :: EquationOfTime             = 0.0d0     ! Value of the equation of time formula
END TYPE DayWeatherVariables

TYPE SpecialDayData
  CHARACTER(len=MaxNameLength) :: Name  = Blank     ! Name
  INTEGER :: DateType                   = 0       ! Date type as read in from IDF
  INTEGER :: Month                      = 0       ! Start Month
  INTEGER :: Day                        = 0       ! Start Day of month or Count for DateTypes=NthDayOfMonth
  INTEGER :: WeekDay                    = 0       ! For Date types=NthDayOfMonth and LastDayOfMonth
  INTEGER :: CompDate                   = 0       ! Start Date in "compressed date" format, only if Month/Day
  LOGICAL :: WthrFile                   = .false. ! True if this Special Day came from weather file (EPW)
  INTEGER :: Duration                   = 0       ! Number of days this special Day is used for
  INTEGER :: DayType                    = 0       ! Day Type desigation for this Special Day period
  INTEGER :: ActStMon                   = 0
  INTEGER :: ActStDay                   = 0
  LOGICAL :: Used                       = .false. ! Set to true in a run period after use (NthDayOfMonth and LastDayOfMonth only)
END TYPE

TYPE DataPeriodData
  CHARACTER(len=MaxNameLength) :: Name  = Blank     ! DataPeriod Title
  CHARACTER(len=10) :: DayOfWeek        = Blank     ! Start Day of Week for DataPeriod
  INTEGER :: NumYearsData               = 1       ! Number of years for which data is present in EPW.
  INTEGER :: WeekDay                    = 0
  INTEGER :: StMon                      = 0
  INTEGER :: StDay                      = 0
  INTEGER :: StYear                     = 0
  INTEGER :: EnMon                      = 0
  INTEGER :: EnDay                      = 0
  INTEGER :: EnYear                     = 0
  INTEGER :: NumDays                    = 0
  INTEGER, DIMENSION(12) :: MonWeekDay  = 0
  INTEGER :: DataStJDay                 = 0
  INTEGER :: DataEnJDay                 = 0
  LOGICAL :: HasYearData                = .false.
END TYPE

TYPE DaylightSavingPeriodData
  INTEGER :: StDateType                 = 0       ! Start Date type as from EPW or IDF
  INTEGER :: StWeekDay                  = 0       ! For DateTypes=NthDayOfMonth or LastDayOfMonth
  INTEGER :: StMon                      = 0       ! DaylightSavingTime (DST) Start Month
  INTEGER :: StDay                      = 0       ! DaylightSavingTime (DST) Start Day
  INTEGER :: EnDateType                 = 0       ! End Date type as from EPW or IDF
  INTEGER :: EnMon                      = 0       ! DaylightSavingTime (DST) End Month
  INTEGER :: EnDay                      = 0       ! DaylightSavingTime (DST) End Day
  INTEGER :: EnWeekDay                  = 0       ! For DateTypes=NthDayOfMonth or LastDayOfMonth
END TYPE

TYPE MissingData          ! This Derived type carries the default missing data
                          ! for those data elements that would be best replaced
                          ! with the previous hour's data for missing data.
  REAL(r64) :: DryBulb      =0.0d0 ! Dry Bulb Temperature (C)
  REAL(r64) :: DewPoint     =0.0d0 ! Dew Point Temperature (C)
  INTEGER   :: RelHumid     =0   ! Relative Humidity (%)
  REAL(r64) :: StnPres      =0.0d0 ! Atmospheric Pressure (Pa)
  INTEGER   :: WindDir      =0   ! Wind Direction (deg)
  REAL(r64) :: WindSpd      =0.0d0 ! Wind Speed/Velocity (m/s)
  INTEGER   :: TotSkyCvr    =0   ! Total Sky Cover (tenths)
  INTEGER   :: OpaqSkyCvr   =0   ! Opaque Sky Cover (tenths)
  REAL(r64) :: Visibility   =0.0d0 ! Visibility (km)
  INTEGER   :: Ceiling      =0   ! Ceiling Height (m)
  INTEGER   :: PrecipWater  =0   ! Precipitable Water (mm)
  REAL(r64) :: AerOptDepth  =0.0d0 ! Aerosol Optical Depth
  INTEGER   :: SnowDepth    =0   ! Snow Depth (cm)
  INTEGER   :: DaysLastSnow =0   ! Number of Days since last snow
  REAL(r64) :: Albedo       =0.0d0 ! Albedo
  REAL(r64) :: LiquidPrecip =0.0d0 ! Rain/Liquid Precipitation (mm)
END TYPE

TYPE MissingDataCounts    ! This Derived type carries the counts of missing data
                          ! items in the weather reading process.  It will count
                          ! only items that are on the source file -- not those that
                          ! are derived from data on the source file.
                          ! Comments below illustrate the data that is being counted:
  INTEGER :: DryBulb      =0 ! Dry Bulb Temperature (C)
  INTEGER :: DewPoint     =0 ! Dew Point Temperature (C)
  INTEGER :: RelHumid     =0 ! Relative Humidity (%)
  INTEGER :: StnPres      =0 ! Atmospheric Pressure (Pa)
  INTEGER :: WindDir      =0 ! Wind Direction (deg)
  INTEGER :: WindSpd      =0 ! Wind Speed/Velocity (m/s)
  INTEGER :: DirectRad    =0 ! Direct Radiation (wh/m2)
  INTEGER :: DiffuseRad   =0 ! Diffuse Radiation (wh/m2)
  INTEGER :: TotSkyCvr    =0 ! Total Sky Cover (tenths)
  INTEGER :: OpaqSkyCvr   =0 ! Opaque Sky Cover (tenths)
  INTEGER :: Visibility   =0 ! Visibility (km)
  INTEGER :: Ceiling      =0 ! Ceiling Height (m)
  INTEGER :: PrecipWater  =0 ! Precipitable Water (mm)
  INTEGER :: AerOptDepth  =0 ! Aerosol Optical Depth
  INTEGER :: SnowDepth    =0 ! Snow Depth (cm)
  INTEGER :: DaysLastSnow =0 ! Number of Days since last snow
  INTEGER :: WeathCodes   =0 ! Weather codes invalid
  INTEGER :: Albedo       =0 ! Albedo
  INTEGER :: LiquidPrecip =0 ! Liquid Precip Depth
END TYPE

TYPE RangeDataCounts      ! This Derived type carries the counts of out of range
                          ! items in the weather reading process.  It will count
                          ! only items that are on the source file -- not those that
                          ! are derived from data on the source file.
                          ! Comments below illustrate the data that is being counted:
  INTEGER :: DryBulb      =0 ! Dry Bulb Temperature (C)
  INTEGER :: DewPoint     =0 ! Dew Point Temperature (C)
  INTEGER :: RelHumid     =0 ! Relative Humidity (%)
  INTEGER :: StnPres      =0 ! Atmospheric Pressure (Pa)
  INTEGER :: WindDir      =0 ! Wind Direction (deg)
  INTEGER :: WindSpd      =0 ! Wind Speed/Velocity (m/s)
  INTEGER :: DirectRad    =0 ! Direct Radiation (wh/m2)
  INTEGER :: DiffuseRad   =0 ! Diffuse Radiation (wh/m2)
END TYPE

TYPE TypicalExtremeData
  CHARACTER(len=MaxNameLength) :: Title           = Blank     ! Environment name
  CHARACTER(len=20)  :: ShortTitle      = Blank     ! Environment name
  CHARACTER(len=20)  :: MatchValue      = Blank     ! String to be matched for input/running these periods for design.
  CHARACTER(len=20)  :: MatchValue1     = Blank     ! String to be also matched (synonym)
  CHARACTER(len=20)  :: MatchValue2     = Blank     ! String to be also matched (synonym)
  CHARACTER(len=20)  :: TEType          = Blank     ! Typical or Extreme
  INTEGER :: TotalDays                  = 0       ! Number of days in environment
  INTEGER :: StartJDay                  = 0       ! Day of year of first day of environment
  INTEGER :: StartMonth                 = 0
  INTEGER :: StartDay                   = 0
  INTEGER :: EndMonth                   = 0
  INTEGER :: EndDay                     = 0
  INTEGER :: EndJDay                    = 0
END TYPE TypicalExtremeData

TYPE WeatherProperties
  CHARACTER(len=MaxNameLength) :: Name  = Blank     ! Reference Name
  CHARACTER(len=MaxNameLength) :: ScheduleName        = Blank    ! Schedule Name or Algorithm Name
  LOGICAL                      :: IsSchedule          = .true.   ! Default is using Schedule
  INTEGER                      :: CalculationType     = 0        !
  INTEGER                      :: SchedulePtr         = 0        ! pointer to schedule when used
  LOGICAL                      :: UsedForEnvrn        =.false.
END TYPE WeatherProperties

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:

INTEGER :: YearofSim = 1                   ! The Present year of Simulation.
INTEGER, PARAMETER :: NumDaysInYear = 365
INTEGER          :: EnvironmentReportNbr   =0   ! Report number for the environment stamp
CHARACTER(len=3) :: EnvironmentReportChr   =Blank ! Report number for the environment stamp (character -- for printing)
INTEGER          :: TimeStampReportNbr     =0   ! Report number for the time stamp
CHARACTER(len=3) :: TimeStampReportChr     =Blank ! Report number for the time stamp (character -- for printing)
INTEGER :: WeatherDataReport               =0   ! Report number for the weather data
LOGICAL :: WeatherFileExists=.FALSE.       ! Set to true if a weather file exists
Character(len=100) :: LocationTitle=Blank   ! Location Title from input File
LOGICAL :: LocationGathered=.false.        ! flag to show if Location exists on Input File (we assume one is there and
                                           ! correct on weather file)

REAL(r64)  :: WeatherFileLatitude  = 0.0d0
REAL(r64)  :: WeatherFileLongitude = 0.0d0
REAL(r64)  :: WeatherFileTimeZone  = 0.0d0
REAL(r64)  :: WeatherFileElevation = 0.0d0
INTEGER :: WeatherFileUnitNumber           ! File unit number for the weather file
REAL(r64), DIMENSION(12) :: GroundTemps=(/18.d0,18.d0,18.d0,18.d0,18.d0,18.d0,18.d0,18.d0,18.d0,18.d0,18.d0,18.d0/) ! Bldg Surface
REAL(r64), DIMENSION(12) :: GroundTempsFC=(/0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0/) ! F or C factor method
REAL(r64), DIMENSION(12) :: SurfaceGroundTemps=(/13.d0,13.d0,13.d0,13.d0,13.d0,13.d0,13.d0,13.d0,13.d0,13.d0,13.d0,13.d0/) ! Surface
REAL(r64), DIMENSION(12) :: DeepGroundTemps=(/16.d0,16.d0,16.d0,16.d0,16.d0,16.d0,16.d0,16.d0,16.d0,16.d0,16.d0,16.d0/)   ! Deep
REAL(r64), DIMENSION(12) :: GroundReflectances=(/.2d0,.2d0,.2d0,.2d0,.2d0,.2d0,.2d0,.2d0,.2d0,.2d0,.2d0,.2d0/)   !User Specified Ground Reflectances
!EPTeam above line replaces (big diffs) REAL(r64), DIMENSION(12) :: GroundReflectances=(/.2,.2,.2,.2,.2,.2,.2,.2,.2,.2,.2,.2/)   !User Specified Ground Reflectances
REAL(r64) :: SnowGndRefModifier=1.0d0           ! Modifier to ground reflectance during snow
REAL(r64) :: SnowGndRefModifierForDayltg=1.0d0  ! Modifier to ground reflectance during snow for daylighting
INTEGER :: WaterMainsTempsMethod = 0          ! Water mains temperature calculation method
INTEGER :: WaterMainsTempsSchedule = 0        ! Water mains temperature schedule
REAL(r64) :: WaterMainsTempsAnnualAvgAirTemp = 0.0d0 ! Annual average outdoor air temperature (C)
REAL(r64) :: WaterMainsTempsMaxDiffAirTemp = 0.0d0   ! Maximum difference in monthly average outdoor air temperatures (deltaC)
LOGICAL :: wthFCGroundTemps=.false.
REAL(r64) :: RainAmount=0.0d0
REAL(r64) :: SnowAmount=0.0d0

TYPE (DayWeatherVariables),SAVE :: TodayVariables=       &  ! Today's daily weather variables
    DayWeatherVariables(               & ! Derived Type for Storing Weather "Header" Data
     0, &     ! Day of year for weather data
     0, &     ! Year of weather data
     0, &     ! Month of weather data
     0, &     ! Day of month for weather data
     0, &     ! Day of week for weather data
     0, &     ! Daylight Saving Time Period indicator (0=no,1=yes)
     0, &     ! Holiday indicator (0=no holiday, non-zero=holiday type)
   0.0d0, &     ! Sine of the solar declination angle
   0.0d0, &     ! Cosine of the solar declination angle
   0.0d0)       ! Value of the equation of time formula
TYPE (DayWeatherVariables),SAVE ::  TomorrowVariables= &     ! Tomorrow's daily weather variables
    DayWeatherVariables(               & ! Derived Type for Storing Weather "Header" Data
     0, &     ! Day of year for weather data
     0, &     ! Year of weather data
     0, &     ! Month of weather data
     0, &     ! Day of month for weather data
     0, &     ! Day of week for weather data
     0, &     ! Daylight Saving Time Period indicator (0=no,1=yes)
     0, &     ! Holiday indicator (0=no holiday, non-zero=holiday type)
   0.0d0, &     ! Sine of the solar declination angle
   0.0d0, &     ! Cosine of the solar declination angle
   0.0d0)       ! Value of the equation of time formula
TYPE (DayWeatherVariables), ALLOCATABLE, DIMENSION(:) :: DesignDay  ! Design day environments
TYPE (MissingData),SAVE :: Missing  = MissingData  &
      (0.0d0,  & ! Dry Bulb Temperature (C)
       0.0d0,  & ! Dew Point Temperature (C)
       0,      & ! Relative Humidity (%)
       0.0d0,  & ! Atmospheric Pressure (Pa)
       0,      & ! Wind Direction (deg)
       0.0d0,  & ! Wind Speed/Velocity (m/s)
       0,      & ! Total Sky Cover (tenths)
       0,      & ! Opaque Sky Cover (tenths)
       0.0d0,  & ! Visibility (km)
       0,      & ! Ceiling Height (m)
       0,      & ! Precipitable Water (mm)
       0.0d0,  & ! Aerosol Optical Depth
       0,      & ! Snow Depth (cm)
       0,      & ! Number of Days since last snow
       0.0d0,  & ! Albedo
       0.0d0)    ! Rain/Liquid Precipitation (mm)

TYPE (MissingDataCounts),SAVE :: Missed = MissingDataCounts(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
TYPE (RangeDataCounts),SAVE :: OutOfRange = RangeDataCounts(0,0,0,0,0,0,0,0)

TYPE (DesignDayData), ALLOCATABLE, DIMENSION(:) :: DesDayInput ! Design day Input Data
TYPE (EnvironmentData), ALLOCATABLE, DIMENSION(:) :: Environment ! Environment data
TYPE (RunPeriodData), ALLOCATABLE, DIMENSION(:) :: RunPeriodInput
TYPE (RunPeriodData), ALLOCATABLE, DIMENSION(:) :: RunPeriodDesignInput
TYPE (TypicalExtremeData), ALLOCATABLE, DIMENSION(:) :: TypicalExtremePeriods
TYPE (DaylightSavingPeriodData),SAVE :: EPWDST = DaylightSavingPeriodData(0,0,0,0,0,0,0,0) ! Daylight Saving Period Data from EPW file
TYPE (DaylightSavingPeriodData),SAVE :: IDFDST = DaylightSavingPeriodData(0,0,0,0,0,0,0,0) ! Daylight Saving Period Data from IDF file
TYPE (DaylightSavingPeriodData),SAVE :: DST    = DaylightSavingPeriodData(0,0,0,0,0,0,0,0) ! Daylight Saving Period Data, if active
TYPE (WeatherProperties), ALLOCATABLE, DIMENSION(:) :: WPSkyTemperature
Integer :: TotRunPers  =0  ! Total number of Run Periods (Weather data) to Setup
Integer :: TotRunDesPers  =0   ! Total number of Run Design Periods (Weather data) to Setup

INTEGER :: NumSpecialDays=0
TYPE (SpecialDayData), ALLOCATABLE, DIMENSION(:) :: SpecialDays
INTEGER, DIMENSION(366) :: SpecialDayTypes  =0  ! To hold holiday types given in input file
INTEGER, DIMENSION(366) :: WeekDayTypes     =0  ! To hold Week day types using specified first day
INTEGER, DIMENSION(366) :: DSTIndex         =0  ! To hold DST Index based on weather file or input

INTEGER :: NumDataPeriods=0
TYPE (DataPeriodData), ALLOCATABLE, DIMENSION(:) :: DataPeriods

INTEGER :: NumIntervalsPerHour=1

LOGICAL :: UseDaylightSaving=.true.   ! True if user says to use Weather File specified DaylightSaving Period
LOGICAL :: UseSpecialDays=.true.      ! True if user says to use Weather File specified Special Days for current RunPeriod
LOGICAL :: UseRainValues=.true.       ! True if rain values from weather file are to be used
LOGICAL :: UseSnowValues=.true.       ! True if snow values from weather file are to be used
LOGICAL :: EPWDaylightSaving=.false.  ! True if a DaylightSaving Time Period is input (EPW files)
LOGICAL :: IDFDaylightSaving=.false.  ! True if a DaylightSaving Time Period is input (IDF files)
LOGICAL :: DaylightSavingIsActive=.false.     ! True if a DaylightSavingPeriod should be used for Environment
LOGICAL :: WFAllowsLeapYears=.false.  ! True if the Weather File (WF) header has "Yes" for Leap Years
INTEGER :: WFLeapYearInd=0 ! Indicator for current Weather file "Leap Year", used in DayOfYear calculations and others.
INTEGER :: curSimDayforEndofRunPeriod=0  ! normal=number days in sim, but different when repeating runperiods or multi-year files
INTEGER :: Envrn=0                ! Counter for environments
INTEGER :: NumOfEnvrn=0           ! Number of environments to be simulated
INTEGER :: NumEPWTypExtSets=0       ! Number of Typical/Extreme on weather file.
INTEGER :: NumWPSkyTemperatures=0   ! Number of WeatherProperty:SkyTemperature items in input file

LOGICAL,   ALLOCATABLE, DIMENSION (:,:) :: TodayIsRain         ! Rain indicator, true=rain
LOGICAL,   ALLOCATABLE, DIMENSION (:,:) :: TodayIsSnow         ! Snow indicator, true=snow
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayRainAmount     ! ficitious indicator of Rain
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodaySnowAmount     ! ficitious indicator of Snow
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayOutDryBulbTemp   ! Dry bulb temperature of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayOutWetBulbTemp   ! Wet bulb temperature of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayOutDewPointTemp  ! Dew Point Temperature of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayOutBaroPress     ! Barometric pressure of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayOutHumRat        ! Humidity ratio of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayOutRelHum        ! Relative Humidity of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayWindSpeed        ! Wind speed of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayWindDir          ! Wind direction of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodaySkyTemp          ! Sky temperature
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayHorizIRSky       ! Horizontal IR from Sky
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayBeamSolarRad     ! Direct normal solar irradiance
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayDifSolarRad      ! Sky diffuse horizontal solar irradiance
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayAlbedo           ! Albedo
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TodayLiquidPrecip     ! Liquid Precipitation Depth (mm)

LOGICAL,   ALLOCATABLE, DIMENSION (:,:) :: TomorrowIsRain         ! Rain indicator, true=rain
LOGICAL,   ALLOCATABLE, DIMENSION (:,:) :: TomorrowIsSnow         ! Snow indicator, true=snow
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowRainAmount     ! ficitious indicator of Rain
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowSnowAmount     ! ficitious indicator of Snow
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowOutDryBulbTemp   ! Dry bulb temperature of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowOutDewPointTemp  ! Dew Point Temperature of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowOutBaroPress     ! Barometric pressure of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowOutRelHum        ! Relative Humidity of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowWindSpeed        ! Wind speed of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowWindDir          ! Wind direction of outside air
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowSkyTemp          ! Sky temperature
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowHorizIRSky       ! Horizontal IR from Sky
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowBeamSolarRad     ! Direct normal solar irradiance
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowDifSolarRad      ! Sky diffuse horizontal solar irradiance
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowAlbedo           ! Albedo
REAL(r64), ALLOCATABLE, DIMENSION (:,:) :: TomorrowLiquidPrecip     ! Liquid Precipitation Depth

REAL(r64), ALLOCATABLE, DIMENSION (:,:,:) :: DDDBRngModifier        ! Design Day Dry-bulb Temperature Range Modifier
REAL(r64), ALLOCATABLE, DIMENSION (:,:,:) :: DDHumIndModifier       ! Design Day relative humidity values
                                                                    !   or wet-bulb modifiers (per HumIndType)
REAL(r64), ALLOCATABLE, DIMENSION (:,:,:) :: DDBeamSolarValues       ! Design Day Beam Solar Values
REAL(r64), ALLOCATABLE, DIMENSION (:,:,:) :: DDDiffuseSolarValues    ! Design Day Relative Humidity Values

REAL(r64), ALLOCATABLE, DIMENSION (:,:,:) :: DDSkyTempScheduleValues ! Sky temperature - DesignDay input

INTEGER :: RptIsRain=0            ! Rain Report Value
INTEGER :: RptIsSnow=0            ! Snow Report Value
INTEGER :: RptDayType=0           ! DayType Report Value


REAL(r64) :: HrAngle      =0.0d0 ! Current Hour Angle
REAL(r64) :: SolarAltitudeAngle =0.0d0     ! Angle of Solar Altitude (degrees)
REAL(r64) :: SolarAzimuthAngle  =0.0d0     ! Angle of Solar Azimuth (degrees)
REAL(r64) :: HorizIRSky         =0.0d0     ! Horizontal Infrared Radiation Intensity (W/m2)
REAL(r64) :: TimeStepFraction =0.0d0 ! Fraction of hour each time step represents
REAL(r64),ALLOCATABLE,DIMENSION(:) :: SPSiteDryBulbRangeModScheduleValue    ! reporting Drybulb Temperature Range Modifier Schedule Value
REAL(r64),ALLOCATABLE,DIMENSION(:) :: SPSiteHumidityConditionScheduleValue  ! reporting Humidity Condition Schedule Value
REAL(r64),ALLOCATABLE,DIMENSION(:) :: SPSiteBeamSolarScheduleValue          ! reporting Beam Solar Schedule Value
REAL(r64),ALLOCATABLE,DIMENSION(:) :: SPSiteDiffuseSolarScheduleValue       ! reporting Diffuse Solar Schedule Value
REAL(r64),ALLOCATABLE,DIMENSION(:) :: SPSiteSkyTemperatureScheduleValue     ! reporting SkyTemperature Modifier Schedule Value
INTEGER,ALLOCATABLE,DIMENSION(:)   :: SPSiteScheduleNamePtr ! SP Site Schedule Name Ptrs
CHARACTER(len=16),ALLOCATABLE,DIMENSION(:) :: SPSiteScheduleUnits ! SP Site Schedule Units
INTEGER :: NumSPSiteScheduleNamePtrs=0 ! Number of SP Site Schedules (DesignDay only)
INTEGER :: NumMissing=0  ! Number of hours of missing data
LOGICAL :: StripCR=.false.  ! If true, strip last character (<cr> off each EPW line)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: Interpolation       ! Interpolation values based on Number of Time Steps in Hour
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SolarInterpolation  ! Solar Interpolation values based on
                                                                   !      Number of Time Steps in Hour
INTEGER, DIMENSION(12) :: EndDayOfMonth=(/31,28,31,30,31,30,31,31,30,31,30,31/)
LOGICAL :: ErrorInWeatherFile = .false.   ! Set to TRUE when there is a problem with dates
INTEGER :: LeapYearAdd=0                  ! Set during environment if leap year is active (adds 1 to number days in Feb)
LOGICAL :: DatesShouldBeReset = .false.   ! True when weekdays should be reset
LOGICAL :: StartDatesCycleShouldBeReset = .false.  ! True when start dates on repeat should be reset
LOGICAL :: Jan1DatesShouldBeReset = .false.    ! True if Jan 1 should signal reset of dates

          ! SUBROUTINE SPECIFICATIONS FOR MODULE WeatherManager
PUBLIC  ManageWeather
PUBLIC  GetNextEnvironment
PUBLIC  ResetEnvironmentCounter
PRIVATE InitializeWeather
PRIVATE UpdateWeatherData
PRIVATE SetCurrentWeather
PRIVATE ReadWeatherForDay
PRIVATE ReadEPlusWeatherForDay
PRIVATE InterpretWeatherDataLine
Private SetUpDesignDay
PRIVATE CalculateDailySolarCoeffs
PRIVATE CalculateSunDirectionCosines
PRIVATE OpenWeatherFile
PRIVATE CloseWeatherFile
PRIVATE OpenEPlusWeatherFile
PRIVATE ResolveLocationInformation
PRIVATE CheckLocationValidity
PRIVATE CheckWeatherFileValidity
PRIVATE ReportOutputFileHeaders
PRIVATE ReportWeatherAndTimeInformation
!PUBLIC  ProcessDateString
! Get Input from Input File
PRIVATE ReadUserWeatherInput
PRIVATE GetRunPeriodData
PRIVATE GetRunPeriodDesignData
PRIVATE GetDesignDayData
PRIVATE GetLocationInfo
PRIVATE GetWeatherProperties
PRIVATE GetSTM
PRIVATE GetWaterMainsTemperatures
PRIVATE CalcWaterMainsTemp
PRIVATE GetWeatherStation
PRIVATE SetupEnvironmentTypes

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE ManageWeather        ! Main driver routine for this module

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 1997
          !       MODIFIED       June 1997 (general clean-up)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver of the weather manager module.
          ! It controls the assignment of weather related global variables as
          ! well as the reads and writes for weather information.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus "manager" methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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

  LOGICAL, SAVE   :: PrintEnvrnStamp = .FALSE.    ! Set to true when the environment header should be printed

          ! FLOW:

  CALL InitializeWeather(PrintEnvrnStamp)

  CALL SetCurrentWeather

  CALL ReportWeatherAndTimeInformation(PrintEnvrnStamp)

  RETURN

END SUBROUTINE ManageWeather

SUBROUTINE ResetEnvironmentCounter

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine provides an easy method to assure that the environment
          ! counter (used by GetNextEnvironment) is reset before SetupSimulation or
          ! Simulating.  May not be necessary, but just in case.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

  Envrn=0

  RETURN

END SUBROUTINE ResetEnvironmentCounter

SUBROUTINE GetNextEnvironment(Available,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is called from the outer simulation manager and determines
          ! if another environment is available in the "run list" or if the end has been
          ! reached.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: InvJulianDay, JulianDay, BetweenDates
  USE DataSystemVariables

  USE DataInterfaces, ONLY: SetupEMSActuator
  USE DataHeatBalance, ONLY: AdaptiveComfortRequested_ASH55, AdaptiveComfortRequested_CEN15251
  USE ThermalComfort, ONLY: CalcThermalComfortAdaptiveASH55,CalcThermalComfortAdaptiveCEN15251

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(OUT)   :: Available    ! true if there is another environment, false if the end
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! will be set to true if severe errors are found in inputs

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetNextEnvironment: '
  CHARACTER(len=*), PARAMETER :: EnvironFormat="('! <Environment>,Environment Name,Environment Type, Start Date, End Date,',   &
                                                       & ' Start DayOfWeek, Duration {#days}, Source:Start DayOfWeek, ',       &
                                                       & ' Use Daylight Saving, Use Holidays, Apply Weekend Holiday Rule, ',   &
                                                       & ' Use Rain Values, Use Snow Values',/,                                &
                                               & '! <Environment:Special Days>, Special Day Name, Special Day Type, Source, ', &
                                                       & 'Start Date, Duration {#days}',/,                                     &
                                               & '! <Environment:Daylight Saving>, Daylight Saving Indicator, Source,',        &
                                                       &   ' Start Date, End Date',/,                                          &
                                               & '! <Environment:WarmupDays>, NumberofWarmupDays')"
  CHARACTER(len=*), PARAMETER :: EnvNameFormat="('Environment',12(',',A))"
  CHARACTER(len=*), PARAMETER :: EnvDSTNFormat="('Environment:Daylight Saving,No,',A)"
  CHARACTER(len=*), PARAMETER :: EnvDSTYFormat="('Environment:Daylight Saving,Yes',3(',',A))"
  CHARACTER(len=*), PARAMETER :: EnvSpDyFormat="('Environment:Special Days',4(',',A),',',I3)"
  CHARACTER(len=*), PARAMETER :: DateFormat="(I2.2,'/',I2.2)"
  CHARACTER(len=*), PARAMETER :: DateFormatwithYear="(I2.2,'/',I2.2,'/',I4.4)"
  CHARACTER(len=*), PARAMETER, DIMENSION(5) :: SpecialDayNames=(/"Holiday        ","SummerDesignDay",  &
                                                                 "WinterDesignDay","CustomDay1     ","CustomDay2     "/)
  CHARACTER(len=*), PARAMETER, DIMENSION(12) :: ValidDayNames=(/"Sunday         ","Monday         ","Tuesday        ", &
                                                                "Wednesday      ","Thursday       ","Friday         ",  &
                                                                "Saturday       ","Holiday        ","SummerDesignDay",  &
                                                                "WinterDesignDay","CustomDay1     ","CustomDay2     "/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE   :: GetInputFlag=.TRUE.   ! Set to true before execution starts
  LOGICAL, SAVE   :: FirstCall=.true.
  LOGICAL, SAVE   :: PrntEnvHeaders=.true.
  INTEGER :: Loop
  INTEGER :: Loop1
  CHARACTER(len=20) :: StDate
  CHARACTER(len=20) :: EnDate
  CHARACTER(len=10) :: string
  CHARACTER(len=10) :: cTotalEnvDays
  INTEGER :: NumDays
  INTEGER :: DSTActStMon
  INTEGER :: DSTActStDay
  INTEGER :: DSTActEnMon
  INTEGER :: DSTActEnDay
  INTEGER :: RunStJDay
  INTEGER :: RunEnJDay
  LOGICAL :: OkRun
  INTEGER :: ThisWeekDay
  INTEGER :: TWeekDay
  INTEGER, DIMENSION(12) :: MonWeekDay
  INTEGER, DIMENSION(12) :: ActEndDayOfMonth
  INTEGER :: ThisDay
  INTEGER :: JDay
  INTEGER :: JDay1
  INTEGER :: JDay5Start
  INTEGER :: JDay5End
  CHARACTER(len=20) :: Source
  CHARACTER(len=3) :: ApWkRule
  CHARACTER(len=3) :: AlpUseDST
  CHARACTER(len=3) :: AlpUseSpec
  CHARACTER(len=3) :: AlpUseRain
  CHARACTER(len=3) :: AlpUseSnow
  CHARACTER(len=100) :: kindOfRunPeriod
  REAL(r64) :: GrossApproxAvgDryBulb

  IF (BeginSimFlag .and. FirstCall) THEN

    PrintEndDataDictionary = .TRUE.

    CALL ReportOutputFileHeaders    ! Write the output file header information

    ! SetupOutputVariables, CurrentModuleObject='All Simulations'

    CALL SetupOutputVariable('Site Outdoor Air Drybulb Temperature [C]',OutDryBulbTemp,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Dewpoint Temperature [C]',OutDewPointTemp,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Wetbulb Temperature [C]',OutWetBulbTemp,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Humidity Ratio [kgWater/kgDryAir]',OutHumRat,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Relative Humidity [%]',OutRelHum,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Barometric Pressure [Pa]',OutBaroPress,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Wind Speed [m/s]',WindSpeed,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Wind Direction [deg]',WindDir,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Sky Temperature [C]',SkyTemp,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Horizontal Infrared Radiation Rate per Area [W/m2]',HorizIRSky,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Diffuse Solar Radiation Rate per Area [W/m2]',DifSolarRad,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Direct Solar Radiation Rate per Area [W/m2]',BeamSolarRad,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Precipitation Depth [m]',LiquidPrecipitation,'Zone','Sum','Environment')
    CALL SetupOutputVariable('Site Ground Reflected Solar Radiation Rate per Area [W/m2]', &
                                 GndSolarRad,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Ground Temperature [C]',GroundTemp,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Surface Ground Temperature [C]',GroundTemp_Surface,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Deep Ground Temperature [C]',GroundTemp_Deep,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Simple Factor Model Ground Temperature [C]',GroundTempFC,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Enthalpy [J/kg]',OutEnthalpy,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Outdoor Air Density [kg/m3]',OutAirDensity,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Solar Azimuth Angle [deg]',SolarAzimuthAngle,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Solar Altitude Angle [deg]',SolarAltitudeAngle,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Solar Hour Angle [deg]',HrAngle,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Rain Status []',RptIsRain,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Snow on Ground Status []',RptIsSnow,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Exterior Horizontal Sky Illuminance [lux]',HISKF,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Exterior Horizontal Beam Illuminance [lux]',HISUNF,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Exterior Beam Normal Illuminance [lux]',HISUNFnorm,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Sky Diffuse Solar Radiation Luminous Efficacy [lum/W]',PDIFLW,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Beam Solar Radiation Luminous Efficacy [lum/W]',PDIRLW,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Daylighting Model Sky Clearness []',SkyClearness,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Daylighting Model Sky Brightness []',SkyBrightness,'Zone','Average','Environment')
    CALL SetupOutputVariable('Site Daylight Saving Time Status []',DSTIndicator,'Zone','State','Environment')
    CALL SetupOutputVariable('Site Day Type Index []',RptDayType,'Zone','State','Environment')
    CALL SetupOutputVariable('Site Mains Water Temperature [C]',WaterMainsTemp,'Zone','Average','Environment')

    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Outdoor Dry Bulb', '[C]',  &
                               EMSOutDryBulbOverrideOn  , EMSOutDryBulbOverrideValue )
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Outdoor Dew Point', '[C]',  &
                               EMSOutDewPointTempOverrideOn  , EMSOutDewPointTempOverrideValue )
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Outdoor Relative Humidity', '[%]',  &
                               EMSOutRelHumOverrideOn  , EMSOutRelHumOverrideValue )
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Diffuse Solar', '[W/m2]',  &
                               EMSDifSolarRadOverrideOn  , EMSDifSolarRadOverrideValue )
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Direct Solar', '[W/m2]',  &
                               EMSBeamSolarRadOverrideOn  , EMSBeamSolarRadOverrideValue )
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Wind Speed', '[m/s]',  &
                               EMSWindSpeedOverrideOn  , EMSWindSpeedOverrideValue )
      CALL SetupEMSActuator('Weather Data', 'Environment', 'Wind Direction', '[deg]',  &
                               EMSWindDirOverrideOn  , EMSWindDirOverrideValue )
    ENDIF

    FirstCall=.false.

  END IF    ! ... end of BeginSimFlag IF-THEN block.

  IF (GetInputFlag) THEN

    CALL SetUpInterpolationValues
    TimeStepFraction=1.d0/REAL(NumOfTimeStepInHour,r64)

    CALL OpenWeatherFile(ErrorsFound)   ! moved here because of possibility of special days on EPW file
    CALL CloseWeatherFile
    CALL ReadUserWeatherInput
    CALL AllocateWeatherData
    IF (NumIntervalsPerHour /= 1) THEN
      IF (NumIntervalsPerHour /= NumOfTimeStepInHour) THEN
        CALL ShowSevereError(RoutineName//  &
           'Number of intervals per hour on Weather file does not match specified number of Time Steps Per Hour')
        ErrorsFound=.true.
      ENDIF
    ENDIF
    GetInputFlag=.false.
    Envrn=0
    IF (NumOfEnvrn > 0) THEN
      CALL ResolveLocationInformation(ErrorsFound) ! Obtain weather related info from input file
      CALL CheckLocationValidity
      IF (Environment(NumOfEnvrn)%KindOfEnvrn /= ksDesignDay) THEN
        CALL CheckWeatherFileValidity
      ENDIF
      IF (ErrorsFound) THEN
        CALL ShowSevereError(RoutineName//'No location specified, program will terminate.')
      ENDIF
    ELSE
      ErrorsFound=.true.
      CALL ShowSevereError(RoutineName//'No Design Days or Run Period(s) specified, program will terminate.')
    ENDIF
    IF (DDOnly .and. TotDesDays == 0) THEN
      ErrorsFound=.true.
      CALL ShowSevereError(RoutineName//  &
         'Requested Design Days only (DDOnly) but no Design Days specified, program will terminate.')
    ENDIF
    IF (ReverseDD .and. TotDesDays == 1) THEN
      ErrorsFound=.true.
      CALL ShowSevereError(RoutineName//  &
         'Requested Reverse Design Days (ReverseDD) but only 1 Design Day specified, program will terminate.')
    ENDIF
    CurrentOverallSimDay=0
    TotalOverallSimDays=0
    MaxNumberSimYears=1
    DO Loop=1,NumOfEnvrn
      TotalOverallSimDays=TotalOverallSimDays+Environment(Loop)%TotalDays
      IF (Environment(Loop)%KindOfEnvrn == ksRunPeriodWeather) THEN
        MaxNumberSimYears=MAX(MaxNumberSimYears,Environment(Loop)%NumSimYears)
      ENDIF
    ENDDO
    CALL DisplaySimDaysProgress(CurrentOverallSimDay,TotalOverallSimDays)
  ENDIF

  CALL CloseWeatherFile  ! will only close if opened.
  Envrn=Envrn+1
  DatesShouldBeReset=.false.
  IF (Envrn > NumOfEnvrn) THEN
    Available=.false.
    Envrn = 0
    CurEnvirNum = 0
  ELSE
    KindOfSim = Environment(Envrn)%KindOfEnvrn
    DayOfYear=Environment(Envrn)%StartJDay
    DayOfMonth=Environment(Envrn)%StartDay
    Month=Environment(Envrn)%StartMonth
    NumOfDayInEnvrn = Environment(Envrn)%TotalDays  ! Set day loop maximum from DataGlobals
    IF (.not. DoingSizing .and. .not. KickOffSimulation) THEN
      IF (AdaptiveComfortRequested_ASH55 .or. AdaptiveComfortRequested_CEN15251) THEN
        IF (KindOfSim == ksDesignDay) THEN
          IF (DoDesDaySim) THEN
            CALL ShowWarningError(RoutineName//'Adaptive Comfort being reported during design day.')
            GrossApproxAvgDryBulb=(DesDayInput(Envrn)%MaxDryBulb+  &
               (DesDayInput(Envrn)%MaxDryBulb-DesDayInput(Envrn)%DailyDBRange))/2.0d0
            IF (AdaptiveComfortRequested_ASH55) CALL CalcThermalComfortAdaptiveASH55(.true.,.false.,GrossApproxAvgDryBulb)
            IF (AdaptiveComfortRequested_CEN15251) CALL CalcThermalComfortAdaptiveCEN15251(.true.,.false.,GrossApproxAvgDryBulb)
          ENDIF
        ELSE
          IF (DoWeathSim .or. DoDesDaySim) THEN
            IF (AdaptiveComfortRequested_ASH55) CALL CalcThermalComfortAdaptiveASH55(.true.,.true.,0.0d0)
            IF (AdaptiveComfortRequested_CEN15251) CALL CalcThermalComfortAdaptiveCEN15251(.true.,.true.,0.0d0)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF (Envrn > TotDesDays .and. WeatherFileExists) THEN
      CALL OpenEPlusWeatherFile(ErrorsFound,.false.)
    ENDIF
    Available=.true.
    IF ((KindOfSim == ksRunPeriodWeather) .and. (.not. WeatherFileExists .and. DoWeathSim)) THEN
      IF (.not. DoingSizing .and. .not. KickOffSimulation) THEN
        CALL ShowSevereError('Weather Simulation requested, but no weather file attached.')
        ErrorsFound=.true.
      ENDIF
      Envrn = 0
      Available=.false.
    ELSEIF ((KindOfSim == ksRunPeriodWeather) .and. (.not. WeatherFileExists .and. .not. DoWeathSim)) THEN
      Available=.false.
      Envrn = 0
    ELSEIF ((KindOfSim == ksRunPeriodWeather) .and. DoingSizing) THEN
      Available=.false.
      Envrn = 0
    ENDIF

    IF (.not. ErrorsFound .and. Available .and. Envrn > 0) THEN
      EnvironmentName = Environment(Envrn)%Title
      CurEnvirNum = Envrn
      RunPeriodStartDayOfWeek=0
      IF ( (DoDesDaySim .and. (KindOfSim /= ksRunPeriodWeather)) .or. ((KindOfSim == ksRunPeriodWeather) .and. DoWeathSim) ) THEN
        IF (PrntEnvHeaders .AND. DoWeatherInitReporting) THEN
          WRITE(OutputFileInits,EnvironFormat)
          PrntEnvHeaders=.false.
        ENDIF

        SELECT CASE(KindOfSim)

        CASE (ksRunPeriodWeather,ksRunPeriodDesign)
          kindOfRunPeriod = Environment(Envrn)%cKindOfEnvrn
          IF (KindOfSim == ksRunPeriodWeather) THEN
            RunPeriodEnvironment=.true.
          ELSE
            RunPeriodEnvironment=.false.
          ENDIF
          ActEndDayOfMonth=EndDayOfMonth
          CurrentYearIsLeapYear=Environment(Envrn)%IsLeapYear
          IF (CurrentYearIsLeapYear .and. WFAllowsLeapYears) THEN
            LeapYearAdd=1
          ELSE
            LeapYearAdd=0
          ENDIF
          IF (CurrentYearIsLeapYear) THEN
            ActEndDayOfMonth(2)=EndDayOfMonth(2)+LeapYearAdd
          ENDIF
          UseDaylightSaving=Environment(Envrn)%UseDST
          UseSpecialDays=Environment(Envrn)%UseHolidays
          UseRainValues=Environment(Envrn)%UseRain
          UseSnowValues=Environment(Envrn)%UseSnow

          OkRun=.false.
          ThisWeekDay=0
          DO Loop=1,NumDataPeriods
            IF (.not. Environment(Envrn)%ActualWeather) THEN
              RunStJDay=JulianDay(DataPeriods(Loop)%StMon,DataPeriods(Loop)%StDay,LeapYearAdd)
              RunEnJDay=JulianDay(DataPeriods(Loop)%EnMon,DataPeriods(Loop)%EnDay,LeapYearAdd)
              IF (.not. BetweenDates(Environment(Envrn)%StartJDay,RunStJDay,RunEnJDay)) CYCLE
              IF (.not. BetweenDates(Environment(Envrn)%EndJDay,RunStJDay,RunEnJDay)) CYCLE
              OkRun=.true.
              IF (RunStJDay > Environment(Envrn)%StartJDay) THEN
                NumDays=RunStJDay-Environment(Envrn)%StartJDay
              ELSE
                NumDays=Environment(Envrn)%StartJDay-RunStJDay
              ENDIF
              ThisWeekDay=MOD(DataPeriods(Loop)%WeekDay+NumDays-1,7)+1
              EXIT
            ELSE  ! Actual Weather
              RunStJDay=DataPeriods(Loop)%DataStJDay
              RunEnJDay=DataPeriods(Loop)%DataEnJDay
              IF (.not. DataPeriods(Loop)%HasYearData) THEN
                CALL ShowSevereError('GetNextEnvironment: Runperiod:CustomRange has been entered but weatherfile '//  &
                   ' DATA PERIOD does not have year included in start/end date.')
                CALL ShowContinueError('...to match the RunPeriod, the DATA PERIOD should be mm/dd/yyyy for both.')
              ENDIF
              IF (.not. BetweenDates(Environment(Envrn)%StartDate,RunStJDay,RunEnJDay)) CYCLE
              IF (.not. BetweenDates(Environment(Envrn)%EndDate,RunStJDay,RunEnJDay)) CYCLE
              OkRun=.true.
              IF (RunStJDay > Environment(Envrn)%StartDate) THEN
                NumDays=RunStJDay-Environment(Envrn)%StartDate
              ELSE
                NumDays=Environment(Envrn)%StartDate-RunStJDay
              ENDIF
              ThisWeekDay=MOD(DataPeriods(Loop)%WeekDay+NumDays-1,7)+1
              EXIT
            ENDIF
          ENDDO

          IF (.not. OkRun) THEN
            IF (.not. Environment(Envrn)%ActualWeather) THEN
              WRITE(StDate,DateFormat) Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay
              WRITE(EnDate,DateFormat) Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay
              CALL ShowSevereError(RoutineName//'Runperiod [mm/dd] (Start='//TRIM(StDate)//',End='//TRIM(EnDate)//  &
                                      ') requested not within Data Period(s) from Weather File')
            ELSE
              WRITE(StDate,DateFormatwithYear) Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,  &
                 Environment(Envrn)%StartYear
              WRITE(EnDate,DateFormatwithYear) Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,  &
                 Environment(Envrn)%EndYear
              CALL ShowSevereError(RoutineName//'Runperiod [mm/dd/yyyy] (Start='//TRIM(StDate)//',End='//TRIM(EnDate)//  &
                                      ') requested not within Data Period(s) from Weather File')
            ENDIF
            WRITE(StDate,DateFormat) DataPeriods(1)%StMon,DataPeriods(1)%StDay
            WRITE(EnDate,DateFormat) DataPeriods(1)%EnMon,DataPeriods(1)%EnDay
            IF (DataPeriods(1)%StYear > 0) THEN
              string=RoundSigDigits(DataPeriods(1)%StYear)
              StDate=trim(StDate)//'/'//string
            ELSE
              StDate=trim(StDate)//'/<noyear>'
            ENDIF
            IF (DataPeriods(1)%EnYear > 0) THEN
              string=RoundSigDigits(DataPeriods(1)%EnYear)
              EnDate=trim(EnDate)//'/'//string
            ELSE
              EnDate=trim(EnDate)//'/<noyear>'
            ENDIF
            IF (NumDataPeriods == 1) THEN
              CALL ShowContinueError('Weather Data Period (Start='//TRIM(StDate)//',End='//TRIM(EnDate))
            ELSE
              CALL ShowContinueError('Multiple Weather Data Periods 1st (Start='//TRIM(StDate)//',End='//TRIM(EnDate)//')')
            ENDIF
            CALL ShowFatalError(RoutineName//'Program terminates due to preceding condition.')
          ENDIF

          ! Following builds Environment start/end for ASHRAE 55 warnings
          WRITE(StDate,DateFormat) Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay
          WRITE(EnDate,DateFormat) Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay
          IF (Environment(Envrn)%ActualWeather) THEN
            string=RoundSigDigits(Environment(Envrn)%StartYear)
            StDate=trim(StDate)//'/'//string
            string=RoundSigDigits(Environment(Envrn)%EndYear)
            EnDate=trim(EnDate)//'/'//string
          ELSEIF (Environment(Envrn)%CurrentYear > 0 .and. Environment(Envrn)%TreatYearsAsConsecutive) THEN
            string=RoundSigDigits(Environment(Envrn)%CurrentYear)
            StDate=trim(StDate)//'/'//string
            string=RoundSigDigits(Environment(Envrn)%CurrentYear+Environment(Envrn)%NumSimYears)
            EnDate=trim(EnDate)//'/'//string
          ENDIF
          EnvironmentStartEnd=TRIM(StDate)//' - '//TRIM(EnDate)

          IF (DoWeatherInitReporting) THEN
            IF (Environment(Envrn)%UseDST) THEN
              AlpUseDST='Yes'
            ELSE
              AlpUseDST='No'
            ENDIF
            IF (Environment(Envrn)%UseHolidays) THEN
              AlpUseSpec='Yes'
            ELSE
              AlpUseSpec='No'
            ENDIF
            IF (Environment(Envrn)%ApplyWeekendRule) THEN
              ApWkRule='Yes'
            ELSE
              ApWkRule='No'
            ENDIF
            IF (Environment(Envrn)%UseRain) THEN
              AlpUseRain='Yes'
            ELSE
              AlpUseRain='No'
            ENDIF
            IF (Environment(Envrn)%UseSnow) THEN
              AlpUseSnow='Yes'
            ELSE
              AlpUseSnow='No'
            ENDIF
            cTotalEnvDays=RoundSigDigits(Environment(Envrn)%TotalDays)
            IF (Environment(Envrn)%DayOfWeek == 0) THEN  ! Uses Weather file start
              WRITE(OutputFileInits,EnvNameFormat) TRIM(Environment(Envrn)%Title),TRIM(kindOfRunPeriod),  &
                                         TRIM(StDate),TRIM(EnDate),TRIM(ValidDayNames(ThisWeekDay)),  &
                                         trim(cTotalEnvDays),'UseWeatherFile',          &
                                         AlpUseDST,AlpUseSpec,ApWkRule,AlpUseRain,AlpUseSnow
              TWeekDay=ThisWeekDay
              MonWeekDay=DataPeriods(Loop)%MonWeekDay
            ELSE
              WRITE(OutputFileInits,EnvNameFormat) TRIM(Environment(Envrn)%Title),TRIM(kindOfRunPeriod),  &
                                         TRIM(StDate),TRIM(EnDate),TRIM(ValidDayNames(Environment(Envrn)%DayOfWeek)),  &
                                         trim(cTotalEnvDays),'Use RunPeriod Specified Day',   &
                                         AlpUseDST,AlpUseSpec,ApWkRule,AlpUseRain,AlpUseSnow
              TWeekDay=Environment(Envrn)%DayOfWeek
              MonWeekDay=Environment(Envrn)%MonWeekDay
            ENDIF
          ELSE  ! just in case
            IF (Environment(Envrn)%DayOfWeek == 0) THEN  ! Uses Weather file start
              TWeekDay=ThisWeekDay
              MonWeekDay=DataPeriods(Loop)%MonWeekDay
            ELSE
              TWeekDay=Environment(Envrn)%DayOfWeek
              MonWeekDay=Environment(Envrn)%MonWeekDay
            ENDIF
          ENDIF

          IF (.not. DoingSizing .and. .not. KickOffSimulation) THEN
            IF ( (KindOfSim == ksRunPeriodWeather .and. DoWeathSim) ) THEN
              IF (AdaptiveComfortRequested_ASH55 .or. AdaptiveComfortRequested_CEN15251) THEN
                IF (WFAllowsLeapYears) THEN
                  CALL ShowSevereError(RoutineName//  &
                     'AdaptiveComfort Reporting does not work correctly with leap years in weather files.')
                  ErrorsFound=.true.
                ENDIF
                IF (NumDataPeriods /= 1) THEN
                  CALL ShowSevereError(RoutineName//  &
                     'AdaptiveComfort Reporting does not work correctly with multiple dataperiods in weather files.')
                  ErrorsFound=.true.
                ENDIF
                IF (DataPeriods(1)%StMon == 1 .and. DataPeriods(1)%StDay == 1) THEN
                  RunStJDay=JulianDay(DataPeriods(1)%StMon,DataPeriods(1)%StDay,LeapYearAdd)
                  RunEnJDay=JulianDay(DataPeriods(1)%EnMon,DataPeriods(1)%EnDay,LeapYearAdd)
                  IF (RunEnJDay-RunStJDay+1 /= 365) THEN
                    CALL ShowSevereError(RoutineName//  &
                       'AdaptiveComfort Reporting does not work correctly with weather files that do not contain 365 days.')
                    ErrorsFound=.true.
                  ENDIF
                ELSE
                  CALL ShowSevereError(RoutineName//  &
                         'AdaptiveComfort Reporting does not work correctly with weather files that do not start on 1 January.')
                  ErrorsFound=.true.
                ENDIF
                IF (NumIntervalsPerHour /= 1) THEN
                  CALL ShowSevereError(RoutineName//  &
                     'AdaptiveComfort Reporting does not work correctly with weather files '//  &
                     'that have multiple interval records per hour.')
                  ErrorsFound=.true.
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          ! Only need to set Week days for Run Days
          RunPeriodStartDayOfWeek=TWeekDay
          WeekDayTypes=0
          JDay5Start=JulianDay(Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,LeapYearAdd)
          JDay5End=JulianDay(Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,LeapYearAdd)
          IF (JDay5End >= JDay5Start) THEN
            curSimDayforEndofRunPeriod=DayOfSim+(JDay5End-JDay5Start)+LeapYearAdd
          ELSE
            curSimDayforEndofRunPeriod=DayOfSim+JulianDay(12,31,LeapYearAdd)-JDay5Start+JDay5End
          ENDIF
          Loop=JDay5Start
          DO
            WeekDayTypes(Loop)=TWeekDay
            TWeekDay=MOD(TWeekDay,7)+1
            Loop=Loop+1
            IF (Loop > 366) Loop=1
            IF (Loop == JDay5End) EXIT
          ENDDO

          IF (UseDaylightSaving) THEN
            IF (EPWDaylightSaving) THEN
              DaylightSavingIsActive=.true.
            ENDIF
          ELSE
            DaylightSavingIsActive=.false.
          ENDIF
          IF (IDFDaylightSaving) THEN
            DaylightSavingIsActive=.true.
          ENDIF
          Environment(Envrn)%SetWeekDays=.false.
          IF (Environment(Envrn)%ActualWeather) THEN
            curSimDayforEndofRunPeriod=Environment(Envrn)%TotalDays
          ENDIF
          IF (DaylightSavingIsActive) THEN
            CALL SetDSTDateRanges(MonWeekDay,DSTIndex,DSTActStMon,DSTActStDay,DSTActEnMon,DSTActEnDay)
          ENDIF

          CALL SetSpecialDayDates(MonWeekDay)

          IF (Environment(Envrn)%StartMonth /= 1 .or. Environment(Envrn)%StartDay /= 1) THEN
            StartDatesCycleShouldBeReset=.true.
            Jan1DatesShouldBeReset=.true.
          ENDIF

          IF (Environment(Envrn)%StartMonth == 1 .and. Environment(Envrn)%StartDay == 1) THEN
            StartDatesCycleShouldBeReset=.false.
            Jan1DatesShouldBeReset=.true.
          ENDIF

          IF (Environment(Envrn)%ActualWeather) THEN
            StartDatesCycleShouldBeReset=.false.
            Jan1DatesShouldBeReset=.true.
          ENDIF

          ! Report Actual Dates for Daylight Saving and Special Days
          IF (.not. KickoffSimulation) THEN
            Source=Blank
            IF (UseDaylightSaving) THEN
              IF (EPWDaylightSaving) THEN
                Source='WeatherFile'
              ENDIF
            ELSE
              Source='RunPeriod Object'
            ENDIF
            IF (IDFDaylightSaving) THEN
              Source='InputFile'
            ENDIF
            IF (DaylightSavingIsActive .AND. DoWeatherInitReporting) THEN
              WRITE(StDate,DateFormat) DSTActStMon,DSTActStDay
              WRITE(EnDate,DateFormat) DSTActEnMon,DSTActEnDay
              WRITE(OutputFileInits,EnvDSTYFormat) TRIM(Source),TRIM(StDate),TRIM(EnDate)
            ELSE IF (DoOutputReporting) THEN
              WRITE(OutputFileInits,EnvDSTNFormat) TRIM(Source)
            ENDIF
            DO Loop=1,NumSpecialDays
              IF (SpecialDays(Loop)%WthrFile .and. UseSpecialDays .and. DoWeatherInitReporting) THEN
                WRITE(StDate,DateFormat) SpecialDays(Loop)%ActStMon,SpecialDays(Loop)%ActStDay
                WRITE(OutputFileInits,EnvSpDyFormat) TRIM(SpecialDays(Loop)%Name),    &
                                                     TRIM(SpecialDayNames(SpecialDays(Loop)%DayType)),  &
                                                     'WeatherFile',TRIM(StDate),SpecialDays(Loop)%Duration
              ENDIF
              IF (.not. SpecialDays(Loop)%WthrFile .and. DoWeatherInitReporting) THEN
                WRITE(StDate,DateFormat) SpecialDays(Loop)%ActStMon,SpecialDays(Loop)%ActStDay
                WRITE(OutputFileInits,EnvSpDyFormat) TRIM(SpecialDays(Loop)%Name),    &
                                                     TRIM(SpecialDayNames(SpecialDays(Loop)%DayType)),  &
                                                     'InputFile',TRIM(StDate),SpecialDays(Loop)%Duration
              ENDIF
            ENDDO
          ENDIF

        CASE (ksDesignDay) ! Design Day
          RunPeriodEnvironment=.false.
          WRITE(StDate,DateFormat) DesDayInput(Envrn)%Month,DesDayInput(Envrn)%DayOfMonth
          EnDate=StDate
          IF (DesDayInput(Envrn)%DayType <= 7 .and. DoWeatherInitReporting) THEN
            WRITE(OutputFileInits,EnvNameFormat) TRIM(Environment(Envrn)%Title),'SizingPeriod:DesignDay',TRIM(StDate),  &
               TRIM(EnDate),TRIM(DaysOfWeek(DesDayInput(Envrn)%DayType)),'1','N/A','N/A','N/A','N/A','N/A','N/A'
          ELSE IF (DoWeatherInitReporting) THEN
            WRITE(OutputFileInits,EnvNameFormat) TRIM(Environment(Envrn)%Title),'SizingPeriod:DesignDay',TRIM(StDate),  &
               TRIM(EnDate),TRIM(SpecialDayNames(DesDayInput(Envrn)%DayType - 7)),'1','N/A','N/A','N/A','N/A','N/A','N/A'
          ENDIF
          IF (DesDayInput(Envrn)%DSTIndicator == 0 .and. DoWeatherInitReporting) THEN
            WRITE(OutputFileInits,EnvDSTNFormat) 'SizingPeriod:DesignDay'
          ELSE IF (DoWeatherInitReporting) THEN
            WRITE(OutputFileInits,EnvDSTYFormat) 'SizingPeriod:DesignDay',TRIM(StDate),TRIM(EnDate)
          ENDIF

        END SELECT

      ENDIF
    ENDIF  ! ErrorsFound
  ENDIF

  IF (ErrorsFound .and. .not. DoingSizing .and. .not. KickOffSimulation) THEN
    CALL ShowSevereError(RoutineName//'Errors found in getting a new environment')
    Available=.false.
  ELSEIF (ErrorsFound) THEN
    Available=.false.
  ENDIF

  RETURN

END SUBROUTINE GetNextEnvironment

SUBROUTINE SetupWeekDaysByMonth(StMon,StDay,StWeekDay,WeekDays)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the weekday for each month based on the start date and
          ! weekday specified for that date.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: StMon
  INTEGER, INTENT(IN) :: StDay
  INTEGER, INTENT(IN) :: StWeekDay
  INTEGER, INTENT(INOUT), DIMENSION(12) :: Weekdays

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER CurWeekDay

  ! Set 1st day of Start Month
  CurWeekDay=StWeekDay
  DO Loop=1,StDay-1
    CurWeekDay=CurWeekDay-1
    IF (CurWeekDay == 0) CurWeekDay=7
  ENDDO

  WeekDays(StMon)=CurWeekDay
  DO Loop=StMon+1,12

    SELECT CASE(Loop)
    CASE(2)
      CurWeekDay=CurWeekDay+EndDayOfMonth(1)
      DO WHILE (CurWeekDay > 7)
        CurWeekDay=CurWeekDay-7
      ENDDO
      WeekDays(Loop)=CurWeekDay

    CASE(3)
      CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)+LeapYearAdd
      DO WHILE (CurWeekDay > 7)
        CurWeekDay=CurWeekDay-7
      ENDDO
      WeekDays(Loop)=CurWeekDay

    CASE(4:12)
      CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)
      DO WHILE (CurWeekDay > 7)
        CurWeekDay=CurWeekDay-7
      ENDDO
      WeekDays(Loop)=CurWeekDay

    END SELECT
  ENDDO

  IF (ANY(WeekDays == 0)) THEN
    ! need to start at StMon and go backwards.
    ! EndDayOfMonth is also "days" in month.  (without leapyear day in February)
    CurWeekDay=StWeekDay
    DO Loop=1,StDay-1
      CurWeekDay=CurWeekDay-1
      IF (CurWeekDay == 0) CurWeekDay=7
    ENDDO

    DO Loop=StMon-1,1,-1

      SELECT CASE(Loop)

      CASE(1)
        CurWeekDay=CurWeekDay-EndDayOfMonth(1)
        DO WHILE (CurWeekDay <= 0)
          CurWeekDay=CurWeekDay+7
        ENDDO
        WeekDays(Loop)=CurWeekDay

      CASE(2)
        CurWeekDay=CurWeekDay-EndDayOfMonth(2)+LeapYearAdd
        DO WHILE (CurWeekDay <= 0)
          CurWeekDay=CurWeekDay+7
        ENDDO
        WeekDays(Loop)=CurWeekDay

      CASE(3:12)
        CurWeekDay=CurWeekDay-EndDayOfMonth(Loop)
        DO WHILE (CurWeekDay <= 0)
          CurWeekDay=CurWeekDay+7
        ENDDO
        WeekDays(Loop)=CurWeekDay
      END SELECT

    ENDDO

  ENDIF

  RETURN

END SUBROUTINE SetupWeekDaysByMonth

SUBROUTINE ResetWeekDaysByMonth(WeekDays,LeapYearAdd,StartMonth,StartMonthDay,EndMonth,EndMonthDay,Rollover,MidSimReset)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine resets the weekday for each month based on the current weekday
          ! and previous weekdays per month.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT), DIMENSION(12) :: Weekdays
  INTEGER, INTENT(IN) :: LeapYearAdd
  INTEGER, INTENT(IN) :: StartMonth
  INTEGER, INTENT(IN) :: StartMonthDay
  INTEGER, INTENT(IN) :: EndMonth
  INTEGER, INTENT(IN) :: EndMonthDay
  LOGICAL, INTENT(IN) :: Rollover
  LOGICAL, INTENT(IN),OPTIONAL :: MidSimReset

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(12) :: WeekdaysCopy
  INTEGER Loop
  INTEGER CurWeekDay
  LOGICAL ResetMidSimulation

  ResetMidSimulation=.false.
  IF (PRESENT(MidSimReset)) ResetMidSimulation=MidSimReset


  WeekdaysCopy=WeekDays
  IF (.not. ResetMidSimulation) THEN
    IF (Rollover) THEN
      IF (StartMonth == 1) THEN
        CurWeekDay=Weekdays(12)+EndDayOfMonth(12)+StartMonthDay-1
      ELSE
        CurWeekDay=WeekDays(EndMonth)+EndMonthDay
      ENDIF
    ELSE  ! restart at same as before
      CurWeekDay=Weekdays(StartMonth)
    ENDIF
    DO WHILE (CurWeekDay > 7)
      CurWeekDay=CurWeekDay-7
    ENDDO

    Weekdays=0
    Weekdays(StartMonth)=CurWeekDay
    DO Loop=StartMonth+1,12
      SELECT CASE(Loop)
      CASE(2)
        CurWeekDay=CurWeekDay+EndDayOfMonth(1)
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(Loop)=CurWeekDay

      CASE(3)
        CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)+LeapYearAdd
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(Loop)=CurWeekDay

      CASE(4:12)
        CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(Loop)=CurWeekDay
      END SELECT
    ENDDO

    IF (ANY(WeekDays == 0)) THEN
      ! need to start at StMon and go backwards.
      ! EndDayOfMonth is also "days" in month.  (without leapyear day in February)
      CurWeekDay=WeekDays(StartMonth)
      DO Loop=1,StartMonthDay-1
        CurWeekDay=CurWeekDay-1
        IF (CurWeekDay == 0) CurWeekDay=7
      ENDDO

      DO Loop=StartMonth-1,1,-1

        SELECT CASE(Loop)

        CASE(1)
          CurWeekDay=CurWeekDay-EndDayOfMonth(1)
          DO WHILE (CurWeekDay <= 0)
            CurWeekDay=CurWeekDay+7
          ENDDO
          WeekDays(Loop)=CurWeekDay

        CASE(2)
          CurWeekDay=CurWeekDay-EndDayOfMonth(2)+LeapYearAdd
          DO WHILE (CurWeekDay <= 0)
            CurWeekDay=CurWeekDay+7
          ENDDO
          WeekDays(Loop)=CurWeekDay

        CASE(3:12)
          CurWeekDay=CurWeekDay-EndDayOfMonth(Loop)
          DO WHILE (CurWeekDay <= 0)
            CurWeekDay=CurWeekDay+7
          ENDDO
          WeekDays(Loop)=CurWeekDay
        END SELECT

      ENDDO

    ENDIF

  ELSE
    IF (Rollover) THEN
      IF (StartMonth == 1) THEN
        CurWeekDay=Weekdays(12)+EndDayOfMonth(12)+StartMonthDay-1
      ELSE
        CurWeekDay=WeekDays(EndMonth)+EndMonthDay
      ENDIF
    ELSE  ! restart at same as before
      CurWeekDay=Weekdays(StartMonth)
    ENDIF
    DO WHILE (CurWeekDay > 7)
      CurWeekDay=CurWeekDay-7
    ENDDO
    WeekDays=0
    IF (StartMonth /= 1) THEN
      CurWeekDay=WeekDaysCopy(12)+EndDayOfMonth(12)
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(1)=CurWeekDay
      CurWeekDay=CurWeekDay+EndDayOfMonth(1)
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(2)=CurWeekDay
        CurWeekDay=CurWeekDay+EndDayOfMonth(2)+LeapYearAdd
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(3)=CurWeekDay
      DO Loop=4,12
        CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)
        DO WHILE (CurWeekDay > 7)
          CurWeekDay=CurWeekDay-7
        ENDDO
        WeekDays(Loop)=CurWeekDay
      ENDDO
    ELSE
      Weekdays=0
      Weekdays(StartMonth)=CurWeekDay
      DO Loop=StartMonth+1,12
        SELECT CASE(Loop)
        CASE(2)
          CurWeekDay=CurWeekDay+EndDayOfMonth(1)
          DO WHILE (CurWeekDay > 7)
            CurWeekDay=CurWeekDay-7
          ENDDO
          WeekDays(Loop)=CurWeekDay

        CASE(3)
          CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)+LeapYearAdd
          DO WHILE (CurWeekDay > 7)
            CurWeekDay=CurWeekDay-7
          ENDDO
          WeekDays(Loop)=CurWeekDay

        CASE(4:12)
          CurWeekDay=CurWeekDay+EndDayOfMonth(Loop-1)
          DO WHILE (CurWeekDay > 7)
            CurWeekDay=CurWeekDay-7
          ENDDO
          WeekDays(Loop)=CurWeekDay
        END SELECT
      ENDDO

      IF (ANY(WeekDays == 0)) THEN
        ! need to start at StMon and go backwards.
        ! EndDayOfMonth is also "days" in month.  (without leapyear day in February)
        CurWeekDay=WeekDays(StartMonth)
        DO Loop=1,StartMonthDay-1
          CurWeekDay=CurWeekDay-1
          IF (CurWeekDay == 0) CurWeekDay=7
        ENDDO

        DO Loop=StartMonth-1,1,-1

          SELECT CASE(Loop)

          CASE(1)
            CurWeekDay=CurWeekDay-EndDayOfMonth(1)
            DO WHILE (CurWeekDay <= 0)
              CurWeekDay=CurWeekDay+7
            ENDDO
            WeekDays(Loop)=CurWeekDay

          CASE(2)
            CurWeekDay=CurWeekDay-EndDayOfMonth(2)+LeapYearAdd
            DO WHILE (CurWeekDay <= 0)
              CurWeekDay=CurWeekDay+7
            ENDDO
            WeekDays(Loop)=CurWeekDay

          CASE(3:12)
            CurWeekDay=CurWeekDay-EndDayOfMonth(Loop)
            DO WHILE (CurWeekDay <= 0)
              CurWeekDay=CurWeekDay+7
            ENDDO
            WeekDays(Loop)=CurWeekDay
          END SELECT

        ENDDO

      ENDIF
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE ResetWeekDaysByMonth

SUBROUTINE SetDSTDateRanges(MonWeekDay,DSTIndex,DSTActStMon,DSTActStDay,DSTActEnMon,DSTActEnDay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! With multiple year weather files (or repeating weather files that rollover day),
          ! need to set DST (Daylight Saving Time) dates at start of environment or year.
          ! DST is only projected for one year.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: JulianDay

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT),DIMENSION(:) :: MonWeekDay  ! Weekday of each day 1 of month
  INTEGER, INTENT(INOUT),DIMENSION(:) :: DSTIndex    ! DST Index for each julian day (1:366)
  INTEGER, OPTIONAL :: DSTActStMon
  INTEGER, OPTIONAL :: DSTActStDay
  INTEGER, OPTIONAL :: DSTActEnMon
  INTEGER, OPTIONAL :: DSTActEnDay

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='SetDSTDateRanges: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ActStartMonth  ! Actual Start Month
  INTEGER :: ActStartDay    ! Actual Start Day of Month
  INTEGER :: ActEndMonth    ! Actual End Month
  INTEGER :: ActEndDay      ! Actual End Day of Month
  INTEGER :: ThisDay        ! Day of Month
  INTEGER :: JDay
  INTEGER :: JDay1
  LOGICAL :: ErrorsFound
  INTEGER, DIMENSION(12) :: ActEndDayOfMonth

  ErrorsFound=.false.
  ActEndDayOfMonth=EndDayOfMonth
  ActEndDayOfMonth(2)=EndDayOfMonth(2)+LeapYearAdd
  IF (DST%StDateType == MonthDay) THEN
    ActStartMonth =DST%StMon
    ActStartDay   =DST%StDay
  ELSEIF (DST%StDateType == NthDayInMonth) THEN
    ThisDay=DST%StWeekday-MonWeekDay(DST%StMon)+1
    DO WHILE (ThisDay <= 0)
      ThisDay=ThisDay+7
    ENDDO
    ThisDay=ThisDay+7*(DST%StDay-1)
    IF (ThisDay > ActEndDayOfMonth(DST%StMon)) THEN
      CALL ShowSevereError(RoutineName//'Determining DST: DST Start Date, Nth Day of Month, not enough Nths')
      ErrorsFound=.true.
    ELSE
      ActStartMonth =DST%StMon
      ActStartDay   =ThisDay
    ENDIF
  ELSE ! LastWeekDayInMonth
    ThisDay=DST%StWeekday-MonWeekDay(DST%StMon)+1
    DO WHILE (ThisDay+7 <= ActEndDayOfMonth(DST%StMon))
      ThisDay=ThisDay+7
    ENDDO
    ActStartMonth =DST%StMon
    ActStartDay   =ThisDay
  ENDIF

  IF (DST%EnDateType == MonthDay) THEN
    ActEndMonth =DST%EnMon
    ActEndDay   =DST%EnDay
  ELSEIF (DST%EnDateType == NthDayInMonth) THEN
    ThisDay=DST%EnWeekday-MonWeekDay(DST%EnMon)+1
    DO WHILE (ThisDay <= 0)
      ThisDay=ThisDay+7
    ENDDO
    ThisDay=ThisDay+7*(DST%EnDay-1)
    IF (ThisDay > ActEndDayOfMonth(DST%EnMon)) THEN
      CALL ShowSevereError(RoutineName//'Determining DST: DST End Date, Nth Day of Month, not enough Nths')
      ErrorsFound=.true.
    ELSE
      ActEndMonth =DST%EnMon
      ActEndDay   =ThisDay
    ENDIF
  ELSE ! LastWeekDayInMonth
    ThisDay=DST%EnWeekday-MonWeekDay(DST%EnMon)+1
    DO WHILE (ThisDay+7 <= ActEndDayOfMonth(DST%EnMon))
      ThisDay=ThisDay+7
    ENDDO
    ActEndMonth =DST%EnMon
    ActEndDay   =ThisDay
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Program terminates due to preceding condition(s).')
  ENDIF

  IF (PRESENT(DSTActStMon)) THEN
    DSTActStMon=ActStartMonth
    DSTActStDay=ActStartDay
    DSTActEnMon=ActEndMonth
    DSTActEnDay=ActEndDay
  ENDIF

  DSTIndex=0
  JDay =JulianDay(ActStartMonth,ActStartDay,LeapYearAdd)
  JDay1=JulianDay(ActEndMonth,ActEndDay,LeapYearAdd)
  IF (JDay1 >= JDay) THEN
    DSTIndex(JDay:JDay1)=1
  ELSE
    DSTIndex(JDay:366)=1
    DSTIndex(1:JDay1)=1
  ENDIF

  RETURN

END SUBROUTINE SetDSTDateRanges

SUBROUTINE SetSpecialDayDates(MonWeekDay)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! With multiple year weather files (or repeating weather files that rollover day),
          ! need to set Special Day dates at start of environment or year.
          ! Special Days are only projected for one year.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: InvJulianDay,JulianDay

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT),DIMENSION(:) :: MonWeekDay  ! Weekday of each day 1 of month

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='SetSpecialDayDates: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop
  INTEGER :: ThisDay
  INTEGER :: JDay
  INTEGER :: JDay1
  INTEGER :: Loop1
  LOGICAL :: ErrorsFound
  INTEGER, DIMENSION(12) :: ActEndDayOfMonth

  ErrorsFound=.false.
  ActEndDayOfMonth=EndDayOfMonth
  ActEndDayOfMonth(2)=EndDayOfMonth(2)+LeapYearAdd
  SpecialDayTypes=0
  DO Loop=1,NumSpecialDays
    IF (SpecialDays(Loop)%WthrFile .and. .not. UseSpecialDays) CYCLE
    IF (SpecialDays(Loop)%DateType <= MonthDay) THEN
      JDay=JulianDay(SpecialDays(Loop)%Month,SpecialDays(Loop)%Day,LeapYearAdd)
      IF (SpecialDays(Loop)%Duration == 1 .and. Environment(Envrn)%ApplyWeekendRule) THEN
        IF (WeekDayTypes(JDay) == 1) THEN
          ! Sunday, must go to Monday
          JDay=JDay+1
          IF (JDay == 366 .and. LeapYearAdd == 0) JDay=1
        ELSEIF (WeekDayTypes(JDay) == 7) THEN
          JDay=JDay+1
          IF (JDay == 366 .and. LeapYearAdd == 0) JDay=1
          JDay=JDay+1
          IF (JDay == 366 .and. LeapYearAdd == 0) JDay=1
        ENDIF
      ENDIF
      CALL InvJulianDay(JDay,SpecialDays(Loop)%ActStMon,SpecialDays(Loop)%ActStDay,LeapYearAdd)
    ELSEIF (SpecialDays(Loop)%DateType == NthDayInMonth) THEN
      IF (SpecialDays(Loop)%WeekDay >= MonWeekDay(SpecialDays(Loop)%Month)) THEN
        ThisDay=SpecialDays(Loop)%Weekday-MonWeekDay(SpecialDays(Loop)%Month)+1
      ELSE
        ThisDay=SpecialDays(Loop)%Weekday-MonWeekDay(SpecialDays(Loop)%Month)+1+7
      ENDIF
      ThisDay=ThisDay+7*(SpecialDays(Loop)%Day-1)
      IF (ThisDay > ActEndDayOfMonth(SpecialDays(Loop)%Month)) THEN
        CALL ShowSevereError(RoutineName//'Special Day Date, Nth Day of Month, not enough Nths, for SpecialDay='//  &
                             TRIM(SpecialDays(Loop)%Name))
        ErrorsFound=.true.
        CYCLE
      ENDIF
      SpecialDays(Loop)%ActStMon=SpecialDays(Loop)%Month
      SpecialDays(Loop)%ActStDay=ThisDay
      JDay=JulianDay(SpecialDays(Loop)%Month,ThisDay,LeapYearAdd)
    ELSE ! LastWeekDayInMonth
      ThisDay=SpecialDays(Loop)%Weekday-MonWeekDay(SpecialDays(Loop)%Month)+1
      DO WHILE (ThisDay+7 <= ActEndDayOfMonth(SpecialDays(Loop)%Month))
        ThisDay=ThisDay+7
      ENDDO
      SpecialDays(Loop)%ActStMon=SpecialDays(Loop)%Month
      SpecialDays(Loop)%ActStDay=ThisDay
      JDay=JulianDay(SpecialDays(Loop)%Month,ThisDay,LeapYearAdd)
    ENDIF
    IF (SpecialDayTypes(JDay) /= 0) THEN
       CALL ShowWarningError(RoutineName//'Special Day definition ('//TRIM(SpecialDays(Loop)%Name)//  &
                                  ') is overwriting previously entered special day period')
       IF (UseSpecialDays) THEN
         CALL ShowContinueError('...This could be caused by definitions on the Weather File.')
       ENDIF
       CALL ShowContinueError('...This could be caused by duplicate definitions in the Input File.')
    ENDIF
    JDay1=JDay-1
    DO Loop1=0,SpecialDays(Loop)%Duration-1
      JDay1=JDay1+1
      IF (JDay1 == 366 .and. LeapYearAdd == 0) JDay1=1
      IF (JDay1 == 367) JDay1=1
      SpecialDayTypes(JDay1)=SpecialDays(Loop)%DayType
    ENDDO
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Program terminates due to preceding condition(s).')
  ENDIF

  RETURN

END SUBROUTINE SetSpecialDayDates

SUBROUTINE InitializeWeather(PrintEnvrnStamp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver of the weather initializations.
          ! Most of the weather handling can be described as "initializations"
          ! so most of the work is done via this subroutine.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE General, ONLY: InvJulianDay, JulianDay

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: PrintEnvrnStamp   ! Set to true when the environment header should be printed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop
  INTEGER FirstSimDayofYear          ! Variable which tells when to skip the day in a multi year simulation.

  LOGICAL,SAVE :: FirstCall=.true.  ! Some things should only be done once
!  LOGICAL, SAVE :: SetYear=.true.
  INTEGER :: JDay5Start
  INTEGER :: JDay5End
  INTEGER :: TWeekDay

          ! FLOW:

  IF (BeginSimFlag .and. FirstCall) THEN

    FirstCall=.false.
    EndMonthFlag=.false.

  END IF    ! ... end of BeginSimFlag IF-THEN block.

  IF (BeginEnvrnFlag) THEN

    !Call and setup the Design Day environment
    IF (Environment(Envrn)%KindOfEnvrn /= ksRunPeriodWeather) THEN
      IF (Envrn <= TotDesDays) THEN
        Call SetupDesignDay(Envrn)
      ENDIF
    ENDIF

    NumMissing=0   ! Only used in Weather file environments
                   ! Start over missing values with each environment
    Missing%StnPres      = StdBaroPress  ! Initial "missing" value
    Missing%DryBulb      = 6.d0       ! Initial "missing" value
    Missing%DewPoint     = 3.d0       ! Initial "missing" value
    Missing%RelHumid     = 50.0d0       ! Initial "missing" value
    Missing%WindSpd      = 2.5d0      ! Initial "missing" value
    Missing%WindDir      = 180        ! Initial "missing" value
    Missing%TotSkyCvr    = 5          ! Initial "missing" value
    Missing%OpaqSkyCvr   = 5          ! Initial "missing" value
    Missing%Visibility   = 777.7d0    ! Initial "missing" value
    Missing%Ceiling      = 77777      ! Initial "missing" value
    Missing%PrecipWater  = 0          ! Initial "missing" value
    Missing%AerOptDepth  = 0.0d0      ! Initial "missing" value
    Missing%SnowDepth    = 0          ! Initial "missing" value
    Missing%DaysLastSnow = 88         ! Initial "missing" value
    Missing%Albedo       = 0.0d0      ! Initial "missing" value
    Missing%LiquidPrecip = 0.0d0      ! Initial "missing" value
                                    ! Counts set to 0 for each environment
    Missed%StnPres      = 0
    Missed%DryBulb      = 0
    Missed%DewPoint     = 0
    Missed%RelHumid     = 0
    Missed%WindSpd      = 0
    Missed%WindDir      = 0
    Missed%TotSkyCvr    = 0
    Missed%OpaqSkyCvr   = 0
    Missed%Visibility   = 0
    Missed%Ceiling      = 0
    Missed%PrecipWater  = 0
    Missed%AerOptDepth  = 0
    Missed%SnowDepth    = 0
    Missed%DaysLastSnow = 0
    Missed%Albedo       = 0
    Missed%LiquidPrecip = 0
    Missed%WeathCodes   = 0
    Missed%DirectRad    = 0
    Missed%DiffuseRad   = 0
                                    ! Counts set to 0 for each environment
    OutOfRange%StnPres      = 0
    OutOfRange%DryBulb      = 0
    OutOfRange%DewPoint     = 0
    OutOfRange%RelHumid     = 0
    OutOfRange%WindSpd      = 0
    OutOfRange%WindDir      = 0
    OutOfRange%DirectRad    = 0
    OutOfRange%DiffuseRad   = 0


    PrintEnvrnStamp=.TRUE. ! Set this to true so that on first non-warmup day (only) the environment header will print out

!    WeekDayCount=0  ! Reset weekday count (weather periods only)
    DO Loop=1,NumSpecialDays
      SpecialDays(Loop)%Used=.false.
    ENDDO

    IF (KindOfSim /= ksDesignDay) THEN
      CALL ReadWeatherForDay(1,Envrn,.false.)     ! Read first day's weather
    ELSE
      TomorrowVariables = DesignDay(Envrn)
    END IF

  END IF    ! ... end of BeginEnvrnFlag IF-THEN block.

  IF (BeginDayFlag) THEN

    ! Check Holidays, Daylight Saving Time, Ground Temperatures, etc.

    CALL UpdateWeatherData  ! Update daily weather info

          ! Read tomorrow's weather only if necessary.  This means that the
          ! simulation is out of warmup, is using a weather tape for this
          ! environment, and is not on the last day (day after last day is
          ! assumed to be equal to last day).

! Following code checks whether the present day of simulation matches the start month and start day.
! In a multi year simulation with run period less than 365, we need to position the weather line
! appropriately.

    IF ( (.NOT.WarmupFlag) .AND. (Environment(Envrn)%KindOfEnvrn /= ksDesignDay) ) THEN
      IF (DayOfSim < NumOfDayInEnvrn) THEN
        IF (DayOfSim == curSimDayforEndofRunPeriod) THEN
          curSimDayforEndofRunPeriod=curSimDayforEndofRunPeriod+Environment(Envrn)%RawSimDays
          IF (StartDatesCycleShouldBeReset) THEN
            CALL ResetWeekDaysByMonth(Environment(Envrn)%MonWeekDay,LeapYearAdd,  &
                 Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,  &
                 Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,Environment(Envrn)%RollDayTypeOnRepeat)
            IF (DaylightSavingIsActive) THEN
              CALL SetDSTDateRanges(Environment(Envrn)%MonWeekDay,DSTIndex)
            ENDIF
            CALL SetSpecialDayDates(Environment(Envrn)%MonWeekDay)
          ENDIF
          YearofSim = YearofSim + 1
          FirstSimDayofYear = 1
          CALL ReadWeatherForDay(FirstSimDayofYear,Envrn,.false.)  ! Read tomorrow's weather
        ELSE
          CALL ReadWeatherForDay(DayOfSim+1,Envrn,.false.)  ! Read tomorrow's weather
        ENDIF
      END IF
    END IF


    IF (DayOfMonth == EndDayOfMonth(Month)) THEN
      EndMonthFlag=.true.
    ENDIF

    ! Set Tomorrow's date data
    MonthTomorrow=TomorrowVariables%Month
    DayOfMonthTomorrow=TomorrowVariables%DayOfMonth
    DayOfWeekTomorrow=TomorrowVariables%DayOfWeek
    HolidayIndexTomorrow=TomorrowVariables%HolidayIndex
    YearTomorrow=TomorrowVariables%Year

    IF (Environment(Envrn)%KindOfEnvrn == ksRunPeriodWeather) THEN
      IF (Month == 1 .and. DayOfMonth == 1 .and. Environment(Envrn)%ActualWeather) THEN
        IF (DatesShouldBeReset) THEN
          IF (Environment(Envrn)%TreatYearsAsConsecutive) THEN
            Environment(Envrn)%CurrentYear=Environment(Envrn)%CurrentYear + 1
            Environment(Envrn)%IsLeapYear=IsLeapYear(Environment(Envrn)%CurrentYear)
            CurrentYearIsLeapYear=Environment(Envrn)%IsLeapYear
            IF (CurrentYearIsLeapYear) THEN
              IF (WFAllowsLeapYears) THEN
                LeapYearAdd=1
              ELSE
                LeapYearAdd=0
              ENDIF
            ELSE
              LeapYearAdd=0
            ENDIF
! need to reset MonWeekDay and WeekDayTypes
            IF (.not. CurrentYearIsLeapYear) THEN
              JDay5Start=JulianDay(Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,0)
              JDay5End=JulianDay(Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,0)
            ELSE
              JDay5Start=JulianDay(Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,LeapYearAdd)
              JDay5End=JulianDay(Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,LeapYearAdd)
            ENDIF
            IF (.not. Environment(Envrn)%ActualWeather) &
               curSimDayforEndofRunPeriod=DayOfSim+Environment(Envrn)%RawSimDays+LeapYearAdd-1

            Loop=JDay5Start
            TWeekDay=DayOfWeek
            DO
              WeekDayTypes(Loop)=TWeekDay
              TWeekDay=MOD(TWeekDay,7)+1
              Loop=Loop+1
              IF (Loop > 366) Loop=1
              IF (Loop == JDay5End) EXIT
            ENDDO
            CALL ResetWeekDaysByMonth(Environment(Envrn)%MonWeekDay,LeapYearAdd,  &
                 Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,  &
                 Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,Environment(Envrn)%RollDayTypeOnRepeat)
            IF (DaylightSavingIsActive) THEN
              CALL SetDSTDateRanges(Environment(Envrn)%MonWeekDay,DSTIndex)
            ENDIF
            CALL SetSpecialDayDates(Environment(Envrn)%MonWeekDay)
          ENDIF
        ENDIF
      ELSEIF ((Month == 1 .and. DayOfMonth == 1) .and.  DatesShouldBeReset .and.  &
              (Jan1DatesShouldBeReset) ) THEN
        IF (Environment(Envrn)%TreatYearsAsConsecutive) THEN
          Environment(Envrn)%CurrentYear=Environment(Envrn)%CurrentYear + 1
          Environment(Envrn)%IsLeapYear=IsLeapYear(Environment(Envrn)%CurrentYear)
          CurrentYearIsLeapYear=Environment(Envrn)%IsLeapYear
          IF (CurrentYearIsLeapYear .and. .not. WFAllowsLeapYears) CurrentYearIsLeapYear=.false.
          IF (DayOfSim < curSimDayForEndOfRunPeriod .and. CurrentYearIsLeapYear)   &
             curSimDayforEndofRunPeriod=curSimDayforEndofRunPeriod+1
        ENDIF
        IF (CurrentYearIsLeapYear) THEN
          IF (WFAllowsLeapYears) THEN
            LeapYearAdd=1
          ELSE
            LeapYearAdd=0
          ENDIF
        ELSE
          LeapYearAdd=0
        ENDIF

        IF (DayOfSim < curSimDayForEndOfRunPeriod) THEN
          IF (Environment(Envrn)%RollDayTypeOnRepeat .or. CurrentYearIsLeapYear) THEN
            CALL ResetWeekDaysByMonth(Environment(Envrn)%MonWeekDay,LeapYearAdd,  &
               Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,  &
               Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,Environment(Envrn)%RollDayTypeOnRepeat,.true.)
          ELSE
            CALL ResetWeekDaysByMonth(Environment(Envrn)%MonWeekDay,LeapYearAdd,  &
               Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay,  &
               Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay,Environment(Envrn)%RollDayTypeOnRepeat,.false.)
          ENDIF
          IF (DaylightSavingIsActive) THEN
            CALL SetDSTDateRanges(Environment(Envrn)%MonWeekDay,DSTIndex)
          ENDIF
          CALL SetSpecialDayDates(Environment(Envrn)%MonWeekDay)
        ENDIF
      ENDIF
!      SetYear=.false.
    ENDIF
  END IF    ! ... end of BeginDayFlag IF-THEN block.

  IF (.not. BeginDayFlag .and. .not. WarmupFlag .and.   &
      (Month /= Environment(Envrn)%StartMonth .or. DayOfMonth /= Environment(Envrn)%StartDay)   &
       .and. .not. DatesShouldBeReset .and.  &
       Environment(Envrn)%KindOfEnvrn == ksRunPeriodWeather) THEN
!    SetYear=.true.
    DatesShouldBeReset=.true.
  ENDIF

  IF (EndEnvrnFlag .and. (Environment(Envrn)%KindOfEnvrn /= ksDesignDay) ) THEN
    REWIND(WeatherFileUnitNumber)
    CALL SkipEPlusWFHeader
    CALL ReportMissing_RangeData
  ENDIF

  ! set the EndDesignDayEnvrnsFlag (dataGlobals)
  ! True at the end of the last design day environment (last time step of last hour of last day of environ which is a design day)
  ! added to address CR7562
  EndDesignDayEnvrnsFlag = .FALSE.
  IF (EndEnvrnFlag) THEN
    IF (Envrn .LT. NumOfEnvrn) THEN
      IF (environment(Envrn)%KindOfEnvrn .NE. environment(Envrn + 1)%KindOfEnvrn) THEN
        EndDesignDayEnvrnsFlag = .TRUE.
      END IF
    ELSE
      ! if the last environment set the flag to true.
      EndDesignDayEnvrnsFlag = .TRUE.
    END IF
  END IF

  RETURN

END SUBROUTINE InitializeWeather

SUBROUTINE UpdateWeatherData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates all of the daily weather data in the local
          ! module level variables and the global variables.
          ! This subroutine will temporarily transfer the weather data for the
          ! current day to the old data structure contained in envdat.inc until
          ! enough reengineering has taken place to eliminate the need for this
          ! include.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

          ! FLOW:
!unused          integer :: myhr

  TodayVariables=TomorrowVariables          ! Transfer Tomorrow's Daily Weather Variables to Today

  IF (BeginEnvrnFlag) THEN
    PreviousHour=24
  ENDIF

  TodayIsRain = TomorrowIsRain
  TodayIsSnow = TomorrowIsSnow
  TodayOutDryBulbTemp = TomorrowOutDryBulbTemp
  TodayOutDewPointTemp = TomorrowOutDewPointTemp
  TodayOutBaroPress = TomorrowOutBaroPress
  TodayOutRelHum = TomorrowOutRelHum
  TodayWindSpeed = TomorrowWindSpeed
  TodayWindDir = TomorrowWindDir
  TodaySkyTemp = TomorrowSkyTemp
  TodayHorizIRSky = TomorrowHorizIRSky
  TodayBeamSolarRad = TomorrowBeamSolarRad
  TodayDifSolarRad = TomorrowDifSolarRad
  TodayLiquidPrecip = TomorrowLiquidPrecip

          ! Update Global Data

  DayOfYear = TodayVariables%DayOfYear
  Year = TodayVariables%Year
  Month = TodayVariables%Month
  DayOfMonth = TodayVariables%DayOfMonth
  DayOfWeek = TodayVariables%DayOfWeek
!  WeekDayCount(DayOfWeek)=WeekDayCount(DayOfWeek)+1
  HolidayIndex = TodayVariables%HolidayIndex
  IF (HolidayIndex > 0) THEN
    RptDayType=7+HolidayIndex
  ELSE
    RptDayType=DayOfWeek
  ENDIF
  DSTIndicator = TodayVariables%DaylightSavingIndex
  EquationOfTime=TodayVariables%EquationOfTime
  CosSolarDeclinAngle=TodayVariables%CosSolarDeclinAngle
  SinSolarDeclinAngle=TodayVariables%SinSolarDeclinAngle

  RETURN

END SUBROUTINE UpdateWeatherData

SUBROUTINE SetCurrentWeather

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   March 1990
          !       MODIFIED       Aug94 (LKL) Fixed improper weighting
          !                      Nov98 (FCW) Added call to get exterior illuminances
          !                      Jan02 (FCW) Changed how ground reflectance for daylighting is set
          !                      Mar12 (LKL) Changed settings for leap years/ current years.
          !       RE-ENGINEERED  Apr97,May97 (RKS)

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to interpolate the hourly
          ! environment data for the sub-hourly time steps in EnergyPlus.  In
          ! other words, this subroutine puts the current weather conditions
          ! into the proper variables.  Rather than using the same data for
          ! each time step, environment data is interpolated as a continuum
          ! throughout the day.

          ! METHODOLOGY EMPLOYED:
          ! The current hour (HourOfDay) as well as the next hour are used
          ! to come up with environment data per time step interval.  Method
          ! used is to assign a weighting for the current hour's data and
          ! (1-that weighting) to the next hour's data.  Actual method is:  if
          ! the current time step is 15 minutes into hour, the interpolated dry
          ! bulb temperature should be 3/4*dry bulb temperature of current hour
          ! and 1/4*dry bulb temperature of next environment hourly data.  At
          ! day boundary (current hour = 24), the next hour is hour 1 of next
          ! weather data day (Tomorrow%).

          ! REFERENCES:
          ! INTERPOL(IBLAST) legacy code.

          ! USE STATEMENTS:
  USE General, ONLY: JulianDay
  USE ScheduleManager, ONLY: UpdateScheduleValues
  use inputprocessor, only:samestring

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: TimeStmpFmt="(I2.2,'/',I2.2,' ',I2.2,':')"
  CHARACTER(len=*), PARAMETER :: MnDyFmt="(I2.2,'/',I2.2)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER,SAVE :: NextHour
  REAL(r64) TempVal
  REAL(r64) TempDPVal

          ! FLOW:


  NextHour = HourOfDay+1

  IF (HourOfDay .EQ. 24) THEN
    NextHour = 1
  END IF

  IF (HourOfDay == 1) THEN
    DayOfYear_Schedule=JulianDay(Month,DayOfMonth,1)
  ENDIF

  CALL UpdateScheduleValues

  WRITE(CurMnDyHr,TimeStmpFmt) Month,DayOfMonth,HourOfDay-1
  WRITE(CurMnDy,MnDyFmt) Month,DayOfMonth

  WeightNow=Interpolation(TimeStep)
  WeightPreviousHour = 1.0d0-WeightNow

  CurrentTime=(HourOfDay-1)+TimeStep*(TimeStepFraction)
  SimTimeSteps = (DayOfSim-1)*24*NumOfTimeStepInHour + (HourOfDay-1)*NumOfTimeStepInHour + TimeStep

  GroundTemp=GroundTemps(Month)
  GroundTempKelvin=GroundTemp+KelvinConv
  GroundTempFC=GroundTempsFC(Month)
  GroundTemp_Surface=SurfaceGroundTemps(Month)
  GroundTemp_Deep=DeepGroundTemps(Month)
  GndReflectance=GroundReflectances(Month)
  GndReflectanceForDayltg=GndReflectance

  CALL CalcWaterMainsTemp

  ! Determine if Sun is up or down, set Solar Cosine values for time step.
  CALL DetermineSunUpDown(SOLCOS)
  IF (SunIsUp .and. SolarAltitudeAngle < 0.0d0) THEN
    CALL ShowFatalError('SetCurrentWeather: At '//TRIM(CurMnDyHr)//' Sun is Up but Solar Altitude Angle is < 0.0')
  ENDIF

  OutDryBulbTemp = TodayOutDryBulbTemp(HourOfDay,TimeStep)
  IF (EMSOutDryBulbOverrideOn) OutDryBulbTemp = EMSOutDryBulbOverrideValue
  OutBaroPress   = TodayOutBaroPress(HourOfDay,TimeStep)
  OutDewPointTemp= TodayOutDewPointTemp(HourOfDay,TimeStep)
  IF (EMSOutDewPointTempOverrideOn) OutDewPointTemp = EMSOutDewPointTempOverrideValue
  OutRelHum      = TodayOutRelHum(HourOfDay,TimeStep)
  OutRelHumValue = OutRelHum/100.d0
  IF (EMSOutRelHumOverrideOn) THEN
    OutRelHumValue = EMSOutRelHumOverrideValue/ 100.d0
    OutRelHum = EMSOutRelHumOverrideValue
  ENDIF

  ! Humidity Ratio and Wet Bulb are derived
  OutHumRat      = PsyWFnTdbRhPb(OutDryBulbTemp,OutRelHumValue,OutBaroPress, 'SetCurrentWeather')
  OutWetBulbTemp = PsyTwbFnTdbWPb(OutDryBulbTemp,OutHumRat,OutBaroPress)
  IF (OutDryBulbTemp < OutWetBulbTemp)  THEN
    OutWetBulbTemp=OutDryBulbTemp
    TempVal=PsyWFnTdbTwbPb(OutDryBulbTemp,OutWetBulbTemp,OutBaroPress)
    TempDPVal=PsyTdpFnWPb(TempVal,OutBaroPress)
    OutDewPointTemp=TempDPVal
  ENDIF

  IF (OutDewPointTemp > OutWetBulbTemp) THEN
    OutDewPointTemp=OutWetBulbTemp
  ENDIF

  IF (KindOfSim == ksDesignDay) THEN
    SPSiteDryBulbRangeModScheduleValue = -999.0d0    ! N/A Drybulb Temperature Range Modifier Schedule Value
    SPSiteHumidityConditionScheduleValue = -999.0d0  ! N/A Humidity Condition Schedule Value
    SPSiteBeamSolarScheduleValue = -999.0d0          ! N/A Beam Solar Schedule Value
    SPSiteDiffuseSolarScheduleValue = -999.0d0       ! N/A Diffuse Solar Schedule Value
    SPSiteSkyTemperatureScheduleValue = -999.0d0     ! N/A SkyTemperature Modifier Schedule Value

    IF (DesDayInput(Envrn)%DBTempRangeType /= DDDBRangeType_Default) THEN
      SPSiteDryBulbRangeModScheduleValue(Envrn) = DDDBRngModifier(Envrn,HourOfDay,TimeStep)
    ENDIF
    IF (DesDayInput(Envrn)%HumIndType == DDHumIndType_WBProfDef         &
        .or. DesDayInput(Envrn)%HumIndType == DDHumIndType_WBProfDif       &
        .or. DesDayInput(Envrn)%HumIndType == DDHumIndType_WBProfMul) THEN
      SPSiteHumidityConditionScheduleValue(Envrn) = DDHumIndModifier(Envrn,HourOfDay,TimeStep)
    ELSEIF (DesDayInput(Envrn)%HumIndType ==  DDHumIndType_RelHumSch) THEN
      SPSiteHumidityConditionScheduleValue(Envrn) = DDHumIndModifier(Envrn,HourOfDay,TimeStep)
    ENDIF
    IF (DesDayInput(Envrn)%SolarModel == SolarModel_Schedule) THEN
      SPSiteBeamSolarScheduleValue(Envrn) = DDBeamSolarValues(Envrn,HourOfDay,TimeStep)
      SPSiteDiffuseSolarScheduleValue(Envrn) = DDDiffuseSolarValues(Envrn,HourOfDay,TimeStep)
    ENDIF
    IF (Environment(Envrn)%WP_Type1 /= 0) THEN
      SPSiteSkyTemperatureScheduleValue(Envrn) = DDSkyTempScheduleValues(Envrn,HourOfDay,TimeStep)
    ENDIF
  ELSEIF (TotDesDays > 0) THEN
    SPSiteDryBulbRangeModScheduleValue = -999.0d0    ! N/A Drybulb Temperature Range Modifier Schedule Value
    SPSiteHumidityConditionScheduleValue = -999.0d0  ! N/A Humidity Condition Schedule Value
    SPSiteBeamSolarScheduleValue = -999.0d0          ! N/A Beam Solar Schedule Value
    SPSiteDiffuseSolarScheduleValue = -999.0d0       ! N/A Diffuse Solar Schedule Value
    SPSiteSkyTemperatureScheduleValue = -999.0d0     ! N/A SkyTemperature Modifier Schedule Value
  ENDIF

  WindSpeed      = TodayWindSpeed(HourOfDay,TimeStep)
  IF (EMSWindSpeedOverrideOn) WindSpeed   = EMSWindSpeedOverrideValue
  WindDir        = TodayWindDir(HourOfDay,TimeStep)
  IF (EMSWindDirOverrideOn) WindDir = EMSWindDirOverrideValue
  HorizIRSky     = TodayHorizIRSky(HourOfDay,TimeStep)
  SkyTemp        = TodaySkyTemp(HourOfDay,TimeStep)
  SkyTempKelvin  = SkyTemp + KelvinConv
  DifSolarRad    = TodayDifSolarRad(HourOfDay,TimeStep)
  IF (EMSDifSolarRadOverrideOn) DifSolarRad = EMSDifSolarRadOverrideValue
  BeamSolarRad   = TodayBeamSolarRad(HourOfDay,TimeStep)
  IF (EMSBeamSolarRadOverrideOn)  BeamSolarRad = EMSBeamSolarRadOverrideValue
  LiquidPrecipitation = TodayLiquidPrecip(HourOfDay,TimeStep)/1000.d0 ! convert from mm to m

  IF (UseRainValues) THEN
    IsRain=TodayIsRain(HourOfDay,TimeStep) !.or. LiquidPrecipitation >= .8d0)  ! > .8 mm
  ELSE
    IsRain=.false.
  ENDIF
  IF (UseSnowValues) THEN
    IsSnow=TodayIsSnow(HourOfDay,TimeStep)
  ELSE
    IsSnow=.false.
  ENDIF

  IF (IsSnow) THEN
    GndReflectance = MAX(MIN(GndReflectance*SnowGndRefModifier,1.0d0),0.0d0)
    GndReflectanceForDayltg = MAX(MIN(GndReflectanceForDayltg*SnowGndRefModifierForDayltg,1.0d0),0.0d0)
  ENDIF

  GndSolarRad=MAX((BeamSolarRad*SOLCOS(3) + DifSolarRad)*GndReflectance,0.0D0)

  IF (.not. SunIsUp) THEN
    DifSolarRad    = 0.0d0
    BeamSolarRad   = 0.0d0
    GndSolarRad    = 0.0d0
  ENDIF

  ! Calc some values
  OutEnthalpy=PsyHFnTdbW(OutDryBulbTemp,OutHumRat)
  OutAirDensity=PsyRhoAirFnPbTdbW(OutBaroPress,OutDryBulbTemp,OutHumRat)

  ! Make sure outwetbulbtemp is valid.  And that no error occurs here.
  IF (OutDryBulbTemp < OutWetBulbTemp) OutWetBulbTemp=OutDryBulbTemp

!                                      VALIDITY TEST.
  IF (OutDewPointTemp > OutWetBulbTemp) THEN
    OutDewPointTemp=OutWetBulbTemp
  ENDIF
          ! Get exterior daylight illuminance for daylighting calculation

  CALL DayltgCurrentExtHorizIllum

  IF (.not. IsRain) THEN
    RptIsRain=0
  ELSE
    RptIsRain=1
  ENDIF

  IF (.not. IsSnow) THEN
    RptIsSnow=0
  ELSE
    RptIsSnow=1
  ENDIF

  RETURN

END SUBROUTINE SetCurrentWeather

SUBROUTINE ReadWeatherForDay(DayToRead,Environ,BackSpaceAfterRead)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   April 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the driving routine behind reading the weather data.
          ! Theoretically, several kinds of weather files could be read here.  As
          ! distributed only EPW files are allowed.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER DayToRead   ! =1 when starting out, otherwise signifies next day
  INTEGER Environ     ! Environment being simulated
  LOGICAL BackSpaceAfterRead ! True if weather file is to be backspaced after read

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ReadEPlusWeatherForDay(DayToRead,Environ,BackSpaceAfterRead)

  RETURN

END SUBROUTINE ReadWeatherForDay

SUBROUTINE ReadEPlusWeatherForDay(DayToRead,Environ,BackSpaceAfterRead)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   April 1999
          !       MODIFIED       March 2012; add actual weather read.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the appropriate day of EPW weather data.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: RangeCheck
  USE General, ONLY: JulianDay,RoundSigDigits
  USE ScheduleManager, ONLY: GetScheduleValuesForDay

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DayToRead   ! =1 when starting out, otherwise signifies next day
  INTEGER, INTENT(IN) :: Environ     ! Environment being simulated
  LOGICAL, INTENT(IN) :: BackSpaceAfterRead ! True if weather file is to be backspaced after read

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: YMDHFmt="(I4.4,2('/',I2.2),1X,I2.2,':',I2.2)"
  CHARACTER(len=*), PARAMETER :: YMDHFmt1="(I4.4,2('/',I2.2),1X,'hour=',I2.2,' - expected hour=',I2.2)"
  CHARACTER(len=*), PARAMETER :: DataFmt="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE HourlyWeatherData
    LOGICAL, DIMENSION(24) :: IsRain      = .false. ! Rain indicator, true=rain
    LOGICAL, DIMENSION(24) :: IsSnow      = .false. ! Snow indicator, true=snow
    REAL(r64), DIMENSION(24) :: OutDryBulbTemp = 0.0d0     ! Hourly dry bulb temperature of outside air
    REAL(r64), DIMENSION(24) :: OutDewPointTemp= 0.0d0     ! Hourly Dew Point Temperature of outside air
    REAL(r64), DIMENSION(24) :: OutBaroPress   = 0.0d0     ! Hourly barometric pressure of outside air
    REAL(r64), DIMENSION(24) :: OutRelHum      = 0.0d0     ! Hourly relative humidity
    REAL(r64), DIMENSION(24) :: WindSpeed      = 0.0d0     ! Hourly wind speed of outside air
    REAL(r64), DIMENSION(24) :: WindDir        = 0.0d0     ! Hourly wind direction of outside air
    REAL(r64), DIMENSION(24) :: SkyTemp        = 0.0d0     ! Hourly sky temperature
    REAL(r64), DIMENSION(24) :: HorizIRSky     = 0.0d0     ! Hourly Horizontal Infrared Radiation Intensity
    REAL(r64), DIMENSION(24) :: BeamSolarRad   = 0.0d0     ! Hourly direct normal solar irradiance
    REAL(r64), DIMENSION(24) :: DifSolarRad    = 0.0d0     ! Hourly sky diffuse horizontal solar irradiance
    REAL(r64), DIMENSION(24) :: Albedo         = 0.0d0     ! Albedo
    REAL(r64), DIMENSION(24) :: LiquidPrecip   = 0.0d0     ! Liquid Precipitation
  END TYPE
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Hour
  INTEGER TS
  INTEGER WYear,WMonth,WDay,WHour,WMinute
  REAL(r64) DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,  &
       DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,WindDir,WindSpeed,  &
       TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,PrecipWater,    &
       AerosolOptDepth,SnowDepth,DaysSinceLastSnow,Albedo,LiquidPrecip
  INTEGER PresWeathObs
  INTEGER, DIMENSION(9) ::  PresWeathConds
  CHARACTER(len=500) WeatherDataLine
  LOGICAL Ready
  INTEGER CurTimeStep
  INTEGER Item

  TYPE (HourlyWeatherData) Wthr
  REAL(r64) A,B,C,AVSC
  REAL(r64) SkyTemp
  INTEGER, SAVE :: CurDayOfWeek
  LOGICAL, SAVE :: UseDayOfWeek
  LOGICAL SkipThisDay  ! Used when LeapYear is/is not in effect
  LOGICAL TryAgain
  INTEGER ReadStatus
  INTEGER NumRewinds
  CHARACTER(len=40) BadRecord
  LOGICAL ErrorsFound
  REAL(r64),SAVE :: CurTime
  REAL(r64) HourRep
  INTEGER OSky
  REAL(r64) TDewK
  REAL(r64) ESky
  LOGICAL ErrorFound
  CHARACTER(len=20) ErrOut
  LOGICAL,SAVE :: LastHourSet  ! for Interpolation
  INTEGER NxtHour
  REAL(r64) WtNow
  REAL(r64) WtPrevHour
  REAL(r64) WgtHourNow
  REAL(r64) WgtPrevHour
  REAL(r64) WgtNextHour
  REAL(r64), SAVE :: LastHrOutDryBulbTemp
  REAL(r64), SAVE :: LastHrOutDewPointTemp
  REAL(r64), SAVE :: LastHrOutBaroPress
  REAL(r64), SAVE :: LastHrOutRelHum
  REAL(r64), SAVE :: LastHrWindSpeed
  REAL(r64), SAVE :: LastHrWindDir
  REAL(r64), SAVE :: LastHrSkyTemp
  REAL(r64), SAVE :: LastHrHorizIRSky
  REAL(r64), SAVE :: LastHrBeamSolarRad
  REAL(r64), SAVE :: LastHrDifSolarRad
  REAL(r64), SAVE :: LastHrAlbedo
  REAL(r64), SAVE :: LastHrLiquidPrecip
  REAL(r64), SAVE :: NextHrBeamSolarRad
  REAL(r64), SAVE :: NextHrDifSolarRad
  REAL(r64), SAVE :: NextHrLiquidPrecip
  LOGICAL :: RecordDateMatch
  INTEGER :: JDay5Start,JDay5End,Loop,TWeekDay

  IF (DayToRead == 1) THEN

! Checks whether Weather file contains just one year of data. If yes then rewind and position to first
! day of weather file. The rest of code appropriately positions to the start day.

    Ready=.false.
    NumRewinds=0
!     Must position file to proper day
!     File already position to first data record
!          Set Current Day of Week to "start of Data Period"
    CurTime=1.d0/REAL(NumIntervalsPerHour,r64)
    CurDayOfWeek=DataPeriods(1)%WeekDay-1
    WYear=0
    WMonth=0
    WDay=0
    Whour=0
    WMinute=0
    LastHourSet=.false.
    DO WHILE (.not. Ready)
      READ(WeatherFileUnitNumber,DataFmt,IOSTAT=ReadStatus) WeatherDataLine
         IF(ReadStatus== 0) THEN
                ! Reduce ugly code
            CALL InterpretWeatherDataLine(WeatherDataLine,ErrorFound,WYear,WMonth,WDay,WHour,WMinute,  &
                  DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,            &
                  DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,    &
                  WindDir,WindSpeed,TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,          &
                  PresWeathObs,PresWeathConds,PrecipWater,AerosolOptDepth,SnowDepth,             &
                  DaysSinceLastSnow,Albedo,LiquidPrecip)
         ELSEIF(ReadStatus < 0) THEN
           IF (NumRewinds >0) THEN
             CALL ShowSevereError('Multiple rewinds on EPW while searching for first day')
           ELSE
             REWIND(WeatherFileUnitNumber)
             NumRewinds=NumRewinds+1
             CALL SkipEPlusWFHeader
             READ(WeatherFileUnitNumber,DataFmt,IOSTAT=ReadStatus) WeatherDataLine
             CALL InterpretWeatherDataLine(WeatherDataLine,ErrorFound,WYear,WMonth,WDay,WHour,WMinute,  &
                    DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,            &
                    DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,    &
                    WindDir,WindSpeed,TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,          &
                    PresWeathObs,PresWeathConds,PrecipWater,AerosolOptDepth,SnowDepth,             &
                    DaysSinceLastSnow,Albedo,LiquidPrecip)

           ENDIF
         ENDIF
      IF (ReadStatus /= 0) THEN
        BadRecord=TRIM(RoundSigDigits(WYear))//'/'//TRIM(RoundSigDigits(WMonth))//'/'//TRIM(RoundSigDigits(WDay))//   &
           Blank//TRIM(RoundSigDigits(WHour))//':'//TRIM(RoundSigDigits(WMinute))
        WRITE(ErrOut,*) ReadStatus
        ErrOut=ADJUSTL(ErrOut)
        CALL ShowFatalError('Error occured on EPW while searching for first day, stopped at '//TRIM(BadRecord)//  &
                            ' IO Error='//TRIM(RoundSigDigits(ReadStatus)),OutputFileStandard)
      ENDIF
      IF (CurDayOfWeek <= 7) THEN
        CurDayOfWeek=MOD(CurDayOfWeek,7)+1
      ENDIF
      IF (WMonth == Environment(Environ)%StartMonth .and. WDay == Environment(Environ)%StartDay .and.   &
          .not. Environment(Environ)%MatchYear) THEN
        RecordDateMatch=.true.
      ELSEIF (WMonth == Environment(Environ)%StartMonth .and. WDay == Environment(Environ)%StartDay .and.   &
          Environment(Environ)%MatchYear .and. WYear == Environment(Environ)%StartYear) THEN
        RecordDateMatch=.true.
      ELSE
        RecordDateMatch=.false.
      ENDIF
      IF (RecordDateMatch) THEN
        BACKSPACE(WeatherFileUnitNumber)
        Ready=.true.
        IF (CurDayOfWeek <= 7) THEN
          CurDayOfWeek=CurDayOfWeek-1
        ENDIF
        ! Do the range checks on the first set of fields -- no others.
        ErrorsFound=.false.
        IF (DryBulb >= 99.9d0) &
          CALL RangeCheck(ErrorsFound,'DryBulb Temperature','WeatherFile','Severe','>= -90',(Drybulb>=-90.0d0), &
                        '<= 70',(DryBulb <=70.0d0),RoundSigDigits(DryBulb,2),WhatObjectName=WeatherFileLocationTitle)
        IF (Dewpoint < 99.9d0) &
          CALL RangeCheck(ErrorsFound,'DewPoint Temperature','WeatherFile','Severe','>= -90',(Dewpoint>=-90.0d0), &
                        '<= 70',(Dewpoint <=70.0d0),RoundSigDigits(Dewpoint,2),WhatObjectName=WeatherFileLocationTitle)
        IF (RelHum < 999.d0) &
          CALL RangeCheck(ErrorsFound,'Relative Humidity','WeatherFile','Severe','> 0',(RelHum>=0.0d0), &
                        '<= 110',(RelHum<=110.0d0),RoundSigDigits(RelHum,0),WhatObjectName=WeatherFileLocationTitle)
        IF (AtmPress < 999999.d0) &
          CALL RangeCheck(ErrorsFound,'Atmospheric Pressure','WeatherFile','Severe','> 31000',(AtmPress>31000.0d0), &
                          '<=120000',(AtmPress<=120000.0d0),RoundSigDigits(AtmPress,0),WhatObjectName=WeatherFileLocationTitle)
        IF (DirectRad < 9999.d0) &
          CALL RangeCheck(ErrorsFound,'Direct Radiation','WeatherFile','Severe','>= 0',(DirectRad>=0.0d0),  &
             WhatObjectName=WeatherFileLocationTitle)
        IF (DiffuseRad < 9999.d0) &
          CALL RangeCheck(ErrorsFound,'Diffuse Radiation','WeatherFile','Severe','>= 0',(DiffuseRad>=0.0d0),  &
             WhatObjectName=WeatherFileLocationTitle)
        IF (WindDir < 999.d0) &
          CALL RangeCheck(ErrorsFound,'Wind Direction','WeatherFile','Severe','>=0',(WindDir>=0.0d0), &
                        '<=360',(WindDir<=360.0d0),RoundSigDigits(WindDir,0),WhatObjectName=WeatherFileLocationTitle)
        IF (WindSpeed < 999.d0) &
          CALL RangeCheck(ErrorsFound,'Wind Speed','WeatherFile','Severe','>=0',(WindSpeed>=0.0d0), &
                        '<=40',(WindSpeed<=40.0d0),RoundSigDigits(WindSpeed,2),WhatObjectName=WeatherFileLocationTitle)
        IF (ErrorsFound) THEN
          CALL ShowSevereError('Out of Range errors found with initial day of WeatherFile')
        ENDIF
      ELSE
      !  Must skip this day
        DO Item=2,NumIntervalsPerHour
          READ(WeatherFileUnitNumber,DataFmt,IOSTAT=ReadStatus) WeatherDataLine
          IF (ReadStatus /= 0) THEN
            READ(WeatherDataLine,*) WYear,WMonth,WDay,WHour,WMinute
            BadRecord=TRIM(RoundSigDigits(WYear))//'/'//TRIM(RoundSigDigits(WMonth))//'/'//TRIM(RoundSigDigits(WDay))//   &
                      Blank//TRIM(RoundSigDigits(WHour))//':'//TRIM(RoundSigDigits(WMinute))
            CALL ShowFatalError('Error occured on EPW while searching for first day, stopped at '//TRIM(BadRecord)//  &
                                ' IO Error='//TRIM(RoundSigDigits(ReadStatus)),OutputFileStandard)
          ENDIF
        ENDDO
        DO Item=1,23*NumIntervalsPerHour
          READ(WeatherFileUnitNumber,DataFmt,IOSTAT=ReadStatus) WeatherDataLine
          IF (ReadStatus /= 0) THEN
            READ(WeatherDataLine,*) WYear,WMonth,WDay,WHour,WMinute
            BadRecord=TRIM(RoundSigDigits(WYear))//'/'//TRIM(RoundSigDigits(WMonth))//'/'//TRIM(RoundSigDigits(WDay))//   &
                      Blank//TRIM(RoundSigDigits(WHour))//':'//TRIM(RoundSigDigits(WMinute))
            CALL ShowFatalError('Error occured on EPW while searching for first day, stopped at '//TRIM(BadRecord)//  &
                                ' IO Error='//TRIM(RoundSigDigits(ReadStatus)),OutputFileStandard)
          ENDIF
        ENDDO
      ENDIF
    ENDDO

   ! Positioned to proper day
    IF (.not. KickOffSimulation .and. .not. DoingSizing .and. Environment(Environ)%KindOfEnvrn == ksRunPeriodWeather) THEN
      Environment(Environ)%CurrentCycle=Environment(Environ)%CurrentCycle+1
      IF (.not. Environment(Environ)%RollDayTypeOnRepeat) THEN
        CALL SetDayOfWeekInitialValues(Environment(Environ)%DayOfWeek,CurDayOfWeek,UseDayOfWeek)
        IF (DaylightSavingIsActive) THEN
          CALL SetDSTDateRanges(Environment(Envrn)%MonWeekDay,DSTIndex)
        ENDIF
          CALL SetSpecialDayDates(Environment(Envrn)%MonWeekDay)
      ELSEIF (Environment(Environ)%CurrentCycle == 1) THEN
        CALL SetDayOfWeekInitialValues(Environment(Environ)%DayOfWeek,CurDayOfWeek,UseDayOfWeek)
        Environment(Environ)%SetWeekDays=.true.
        IF (DaylightSavingIsActive) THEN
          CALL SetDSTDateRanges(Environment(Envrn)%MonWeekDay,DSTIndex)
        ENDIF
          CALL SetSpecialDayDates(Environment(Envrn)%MonWeekDay)
      ELSE
        CurDayOfWeek=DayOfWeekTomorrow
      ENDIF
    ELSE
      CALL SetDayOfWeekInitialValues(Environment(Environ)%DayOfWeek,CurDayOfWeek,UseDayOfWeek)
    ENDIF
  ENDIF

  TryAgain=.true.
  SkipThisDay=.false.

  DO WHILE (TryAgain)

    TryAgain=.false.

    TomorrowOutDryBulbTemp=0.0d0
    TomorrowOutDewPointTemp=0.0d0
    TomorrowOutBaroPress=0.0d0
    TomorrowOutRelHum=0.0d0
    TomorrowWindSpeed=0.0d0
    TomorrowWindDir=0.0d0
    TomorrowSkyTemp=0.0d0
    TomorrowHorizIRSky=0.0d0
    TomorrowBeamSolarRad=0.0d0
    TomorrowDifSolarRad=0.0d0
    TomorrowAlbedo=0.0d0
    TomorrowLiquidPrecip=0.0d0
    TomorrowIsRain=.false.
    TomorrowIsSnow=.false.

    DO Hour=1,24
      Do CurTimeStep=1,NumIntervalsPerHour
        HourRep=REAL(Hour-1,r64)+(CurTime*REAL(CurTimeStep,r64))
        READ(WeatherFileUnitNumber,DataFmt,IOSTAT=ReadStatus) WeatherDataLine
        IF (ReadStatus /= 0) WeatherDataLine=Blank
        IF (WeatherDataLine == Blank) THEN
          IF (Hour == 1) THEN
            ReadStatus=-1
          ELSE
            ReadStatus=99
          ENDIF
        ENDIF
        IF (ReadStatus == 0) THEN
          CALL InterpretWeatherDataLine(WeatherDataLine,ErrorFound,WYear,WMonth,WDay,WHour,WMinute,  &
                    DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,            &
                    DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,    &
                    WindDir,WindSpeed,TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,          &
                    PresWeathObs,PresWeathConds,PrecipWater,AerosolOptDepth,SnowDepth,             &
                    DaysSinceLastSnow,Albedo,LiquidPrecip)
        ELSE  ! ReadStatus /=0
          IF (ReadStatus < 0 .and. NumDataPeriods == 1) THEN  ! Standard End-of-file, rewind and position to first day...
              IF (DataPeriods(1)%NumDays >= NumDaysInYear) THEN
              REWIND(WeatherFileUnitNumber)
              CALL SkipEPlusWFHeader
              READ(WeatherFileUnitNumber,DataFmt,IOSTAT=ReadStatus) WeatherDataLine

              CALL InterpretWeatherDataLine(WeatherDataLine,ErrorFound,WYear,WMonth,WDay,WHour,WMinute,  &
                        DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,            &
                        DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,    &
                        WindDir,WindSpeed,TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,          &
                        PresWeathObs,PresWeathConds,PrecipWater,AerosolOptDepth,SnowDepth,             &
                        DaysSinceLastSnow,Albedo,LiquidPrecip)
            ELSE
              BadRecord=TRIM(RoundSigDigits(WYear))//'/'//TRIM(RoundSigDigits(WMonth))//'/'//TRIM(RoundSigDigits(WDay))//   &
                      Blank//TRIM(RoundSigDigits(WHour))//':'//TRIM(RoundSigDigits(WMinute))
              CALL ShowFatalError('End-of-File encountered after '//TRIM(BadRecord)//', starting from first day of '// &
                                  'Weather File would not be "next day"')
            ENDIF
          ELSE
            BadRecord=TRIM(RoundSigDigits(WYear))//'/'//TRIM(RoundSigDigits(WMonth))//'/'//TRIM(RoundSigDigits(WDay))//   &
                      Blank//TRIM(RoundSigDigits(WHour))//':'//TRIM(RoundSigDigits(WMinute))
            CALL ShowFatalError('Unexpected error condition in middle of reading EPW file, stopped at '//TRIM(BadRecord), &
                                 OutputFileStandard)
          ENDIF
        ENDIF

        IF (Hour /= Whour) THEN
            BadRecord=TRIM(RoundSigDigits(WYear))//'/'//TRIM(RoundSigDigits(WMonth))//'/'//TRIM(RoundSigDigits(WDay))//   &
                      Blank//TRIM(RoundSigDigits(WHour))//':'//TRIM(RoundSigDigits(WMinute))
            CALL ShowFatalError('Unexpected error condition in middle of reading EPW file, '//TRIM(BadRecord), &
                                 OutputFileStandard)
        ENDIF

!         Set possible missing values
        IF (ETHoriz < 0.0d0) ETHoriz=9999.d0
        IF (ETDirect < 0.0d0) ETDirect=9999.d0
        IF (IRHoriz <= 0.0d0) IRHoriz=9999.d0
        IF (GLBHoriz < 0.0d0) GLBHoriz=9999.d0
        IF (DisplayWeatherMissingDataWarnings) THEN
          IF (DirectRad >= 9999.d0) THEN
            Missed%DirectRad=Missed%DirectRad+1
          ENDIF
          IF (DiffuseRad >= 9999.d0) THEN
            Missed%DiffuseRad=Missed%DirectRad+1
          ENDIF
          IF (DirectRad < 0.0d0) THEN
            DirectRad=9999.d0
            OutOfRange%DirectRad=OutOfRange%DirectRad+1
          ENDIF
          IF (DiffuseRad < 0.0d0) THEN
            DiffuseRad=9999.d0
            OutOfRange%DiffuseRad=OutOfRange%DiffuseRad+1
          ENDIF
        ENDIF
        IF (GLBHorizIllum < 0.0d0) GLBHorizIllum=999999.d0
        IF (DirectNrmIllum < 0.0d0) DirectNrmIllum=999999.d0
        IF (DiffuseHorizIllum < 0.0d0) DiffuseHorizIllum=999999.d0
        IF (ZenLum < 0.0d0) ZenLum=99999.d0
        IF (AtmPress < 0.0d0) AtmPress=999999.d0
        IF (WindSpeed < 0.0d0) WindSpeed=999.d0
        IF (WindDir < -360.d0 .or. WindDir > 360.d0) WindDir=999.d0
        IF (TotalSkyCover < 0.0d0) TotalSkyCover=99.d0
        IF (RelHum < 0.0d0) RelHum=999.d0
        IF (OpaqueSkyCover < 0.0d0) OpaqueSkyCover=99.d0
        IF (Visibility < 0.0d0) Visibility=9999.d0
        IF (CeilHeight < 0.0d0) CeilHeight=9999.d0
        IF (PresWeathObs < 0) PresWeathObs=9.0d0
        IF (PrecipWater < 0.0d0) PrecipWater=999.d0
        IF (AerosolOptDepth < 0.0d0) AerosolOptDepth=999.d0
        IF (SnowDepth < 0.0d0) SnowDepth=999.d0
        IF (DaysSinceLastSnow < 0.0d0) DaysSinceLastSnow=99.d0
        IF (Albedo < 0.0d0) Albedo=999.d0
        IF (LiquidPrecip < 0.0d0) LiquidPrecip=999.d0

        IF (Hour == 1 .and. CurTimeStep == 1) THEN
          IF (WMonth == 2 .and. WDay == 29 .and. (.not. CurrentYearIsLeapYear .or. .not. WFAllowsLeapYears)) THEN
            EndDayOfMonth(2)=28
            SkipThisDay=.true.
            TryAgain=.true.
            CALL ShowWarningError('ReadEPlusWeatherForDay: Feb29 data encountered but will not be processed.')
            IF (.not. WFAllowsLeapYears) THEN
              CALL ShowContinueError('...WeatherFile does not allow Leap Years. '//  &
                 'HOLIDAYS/DAYLIGHT SAVINGS header must indicate "Yes".')
            ENDIF
            CYCLE
          ELSEIF (WMonth == 2 .and. WDay == 29 .and. CurrentYearIsLeapYear .and. WFAllowsLeapYears) THEN
            TryAgain=.false.
            SkipThisDay=.false.
          ELSE
            TryAgain=.false.
            SkipThisDay=.false.
          ENDIF

          TomorrowVariables%Year=WYear
          TomorrowVariables%Month=WMonth
          TomorrowVariables%DayOfMonth=WDay
          TomorrowVariables%DayOfYear=JulianDay(WMonth,Wday,LeapYearAdd)
          CALL CalculateDailySolarCoeffs(TomorrowVariables%DayOfYear,A,B,C,AVSC,TomorrowVariables%EquationOfTime, &
                                         TomorrowVariables%SinSolarDeclinAngle,TomorrowVariables%CosSolarDeclinAngle)
          IF (CurDayOfWeek <= 7) THEN
            CurDayOfWeek=MOD(CurDayOfWeek,7)+1
          ENDIF
          TomorrowVariables%DayOfWeek=CurDayOfWeek
          TomorrowVariables%DaylightSavingIndex=DSTIndex(TomorrowVariables%DayOfYear)
          TomorrowVariables%HolidayIndex=SpecialDayTypes(TomorrowVariables%DayOfYear)
        ENDIF

        IF (SkipThisDay) CYCLE

        ! Check out missing values

        IF (DryBulb >= 99.9d0) THEN
          DryBulb=Missing%DryBulb
          Missed%DryBulb=Missed%DryBulb+1
        ENDIF
        IF (DryBulb < -90.d0 .or. DryBulb > 70.d0) THEN
          OutOfRange%DryBulb=OutOfRange%DryBulb+1
        ENDIF

        IF (DewPoint >= 99.9d0) THEN
          DewPoint=Missing%DewPoint
          Missed%DewPoint=Missed%DewPoint+1
        ENDIF
        IF (DewPoint < -90.d0 .or. DewPoint > 70.d0) THEN
          OutOfRange%DewPoint=OutOfRange%DewPoint+1
        ENDIF

        IF (RelHum >= 999.d0) THEN
          RelHum=Missing%RelHumid
          Missed%RelHumid=Missed%RelHumid+1
        ENDIF
        IF (RelHum < 0.d0 .or. RelHum > 110.d0) THEN
          OutOfRange%RelHumid=OutOfRange%RelHumid+1
        ENDIF

        IF (AtmPress >= 999999.d0) THEN
          AtmPress=Missing%StnPres
          Missed%StnPres=Missed%StnPres+1
        ENDIF
        IF (AtmPress <= 31000.d0 .or. AtmPress > 120000.d0) THEN
          OutOfRange%StnPres=OutOfRange%StnPres+1
          AtmPress=Missing%StnPres
        ENDIF

        IF (WindDir >= 999.d0) THEN
          WindDir=Missing%WindDir
          Missed%WindDir=Missed%WindDir+1
        ENDIF
        IF (WindDir < 0.d0 .or. WindDir > 360.d0) THEN
          OutOfRange%WindDir=OutOfRange%WindDir+1
        ENDIF

        IF (WindSpeed >= 999.d0) THEN
          WindSpeed=Missing%WindSpd
          Missed%WindSpd=Missed%WindSpd+1
        ENDIF
        IF (WindSpeed < 0.d0 .or. WindSpeed > 40.d0) THEN
          OutOfRange%WindSpd=OutOfRange%WindSpd+1
        ENDIF

        IF (TotalSkyCover >= 99.d0) THEN
          TotalSkyCover=Missing%TotSkyCvr
          Missed%TotSkyCvr=Missed%TotSkyCvr+1
        ENDIF

        IF (OpaqueSkyCover >= 99.d0) THEN
          OpaqueSkyCover=Missing%OpaqSkyCvr
          Missed%OpaqSkyCvr=Missed%OpaqSkyCvr+1
        ENDIF

        ! Some values are not used within EnergyPlus, don't keep stats on their missing data points.

!        IF (Visibility >= 9999.) THEN
!          Visibility=Missing%Visibility
!          Missed%Visibility=Missed%Visibility+1
!        ENDIF

!        IF (CeilHeight >= 99999.) THEN
!          CeilHeight=Missing%Ceiling
!         Missed%Ceiling=Missed%Ceiling+1
!        ENDIF

!        IF (PrecipWater >= 999.) THEN
!          PrecipWater=Missing%PrecipWater
!          Missed%PrecipWater=Missed%PrecipWater+1
!        ENDIF

!        IF (AerosolOptDepth >= .999) THEN
!          AerosolOptDepth=Missing%AerOptDepth
!         Missed%AerOptDepth=Missed%AerOptDepth+1
!        ENDIF

        IF (SnowDepth >= 999.d0) THEN
          SnowDepth=Missing%SnowDepth
          Missed%SnowDepth=Missed%SnowDepth+1
        ENDIF

        IF (Albedo >= 999.d0) THEN
          Albedo=Missing%Albedo
          Missed%Albedo=Missed%Albedo+1
        ENDIF

        IF (LiquidPrecip >= 999.d0) THEN
          LiquidPrecip=Missing%LiquidPrecip
          Missed%LiquidPrecip=Missed%LiquidPrecip+1
          LiquidPrecip=0.0d0
        ENDIF


!        IF (DaysSinceLastSnow >= 99) THEN
!          DaysSinceLastSnow=Missing%DaysLastSnow
!          Missed%DaysLastSnow=Missed%DaysLastSnow+1
!        ENDIF

        TomorrowOutDryBulbTemp(Hour,CurTimeStep)=DryBulb
        TomorrowOutDewPointTemp(Hour,CurTimeStep)=DewPoint
        TomorrowOutBaroPress(Hour,CurTimeStep)=AtmPress
        TomorrowOutRelHum(Hour,CurTimeStep)=RelHum
        RelHum=RelHum*.01d0
        TomorrowWindSpeed(Hour,CurTimeStep)=WindSpeed
        TomorrowWindDir(Hour,CurTimeStep)=WindDir
        TomorrowLiquidPrecip(Hour,CurTimeStep)=LiquidPrecip
        TomorrowHorizIRSky(Hour,CurTimeStep)=IRHoriz

        IF (Environment(Envrn)%WP_Type1 == 0) THEN
          ! Calculate sky temperature, use IRHoriz if not missing
          IF (IRHoriz >= 9999.d0) THEN
            ! Missing, use sky cover
            OSky=OpaqueSkyCover
            TDewK=MIN(DryBulb,DewPoint)+TKelvin
            ESky= (.787d0 +.764d0*LOG((TDewK)/TKelvin))*(1.d0 + .0224d0*OSky - 0.0035d0*(OSky**2) + .00028d0*(OSky**3))
            SkyTemp=(DryBulb+TKelvin)*(ESky**.25d0)-TKelvin
          ELSE  ! Valid IR from Sky
            SkyTemp=(IRHoriz/Sigma)**.25d0 -TKelvin
          ENDIF
        ELSE
          SkyTemp=0.0d0  ! dealt with later
        ENDIF

        TomorrowSkyTemp(Hour,CurTimeStep)=SkyTemp

        IF (ETHoriz >= 9999.d0) ETHoriz=0.0d0
        IF (ETDirect >= 9999.d0) ETDirect=0.0d0
        IF (GLBHoriz >= 9999.d0) GLBHoriz=0.0d0
        IF (DirectRad >= 9999.d0) DirectRad=0.0d0
        IF (DiffuseRad >= 9999.d0) DiffuseRad=0.0d0
        IF (GLBHorizIllum >= 999900.d0) GLBHorizIllum=0.0d0
        IF (DirectNrmIllum >= 999900.d0) DirectNrmIllum=0.0d0
        IF (DiffuseHorizIllum >= 999900.d0) DiffuseHorizIllum=0.0d0
        IF (ZenLum >= 99990.d0) ZenLum=0.0d0
        IF (IgnoreSolarRadiation) THEN
          GLBHoriz=0.0d0
          DirectRad=0.0d0
          DiffuseRad=0.0d0
        ENDIF
        IF (IgnoreBeamRadiation) THEN
          DirectRad=0.0d0
        ENDIF
        IF (IgnoreDiffuseRadiation) THEN
          DiffuseRad=0.0d0
        ENDIF

        TomorrowBeamSolarRad(Hour,CurTimeStep)=DirectRad
        TomorrowDifSolarRad(Hour,CurTimeStep)=DiffuseRad

        TomorrowIsRain(Hour,CurTimeStep) = .false.
        IF (PresWeathObs == 0) THEN
          IF (PresWeathConds(1) .lt. 9 .or. &
              PresWeathConds(2) .lt. 9 .or. &
              PresWeathConds(3) .lt. 9)  &
                TomorrowIsRain(Hour,CurTimeStep)=.true.
        ELSE
          TomorrowIsRain(Hour,CurTimeStep) = .false.
        ENDIF
        TomorrowIsSnow(Hour,CurTimeStep) = (SnowDepth > 0.0d0)

         ! default if rain but none on weather file
        IF (TomorrowIsRain(Hour,CurTimeStep) .and.   &
            TomorrowLiquidPrecip(Hour,CurTimeStep) == 0.0d0)   &
            TomorrowLiquidPrecip(Hour,CurTimeStep)=2.0d0 ! 2mm in an hour ~ .08 inch

        Missing%DryBulb=DryBulb
        Missing%DewPoint=DewPoint
        Missing%RelHumid=RelHum*100.d0
        Missing%StnPres=AtmPress
        Missing%WindDir=WindDir
        Missing%WindSpd=WindSpeed
        Missing%TotSkyCvr=TotalSkyCover
        Missing%OpaqSkyCvr=OpaqueSkyCover
        Missing%Visibility=Visibility
        Missing%Ceiling=CeilHeight
        Missing%PrecipWater=PrecipWater
        Missing%AerOptDepth=AerosolOptDepth
        Missing%SnowDepth=SnowDepth
        Missing%DaysLastSnow=DaysSinceLastSnow
        Missing%Albedo=Albedo
!        Missing%LiquidPrecip=LiquidPrecip

      ENDDO  ! CurTimeStep Loop

    ENDDO  ! Hour Loop

  ENDDO  ! Try Again While Loop

  IF (BackSpaceAfterRead) THEN
    BACKSPACE(WeatherFileUnitNumber)
  ENDIF

  IF (NumIntervalsPerHour == 1 .and. NumOfTimeStepInHour > 1) THEN
    ! Create interpolated weather for timestep orientation
    ! First copy ts=1 (hourly) from data arrays to Wthr structure
    DO Hour=1,24
      Wthr%OutDryBulbTemp(Hour)=TomorrowOutDryBulbTemp(Hour,1)
      Wthr%OutDewPointTemp(Hour)=TomorrowOutDewPointTemp(Hour,1)
      Wthr%OutBaroPress(Hour)=TomorrowOutBaroPress(Hour,1)
      Wthr%OutRelHum(Hour)=TomorrowOutRelHum(Hour,1)
      Wthr%WindSpeed(Hour)=TomorrowWindSpeed(Hour,1)
      Wthr%WindDir(Hour)=TomorrowWindDir(Hour,1)
      Wthr%SkyTemp(Hour)=TomorrowSkyTemp(Hour,1)
      Wthr%HorizIRSky(Hour)=TomorrowHorizIRSky(Hour,1)
      Wthr%BeamSolarRad(Hour)=TomorrowBeamSolarRad(Hour,1)
      Wthr%DifSolarRad(Hour)=TomorrowDifSolarRad(Hour,1)
      Wthr%IsRain(Hour)=TomorrowIsRain(Hour,1)
      Wthr%IsSnow(Hour)=TomorrowIsSnow(Hour,1)
      Wthr%Albedo(Hour)=TomorrowAlbedo(Hour,1)
      Wthr%LiquidPrecip(Hour)=TomorrowLiquidPrecip(Hour,1)
    ENDDO

    IF (.not. LastHourSet) THEN
    ! For first day of weather, all time steps of the first hour will be
    ! equal to the first hour's value.
      LastHrOutDryBulbTemp=Wthr%OutDryBulbTemp(24)
      LastHrOutDewPointTemp=Wthr%OutDewPointTemp(24)
      LastHrOutBaroPress=Wthr%OutBaroPress(24)
      LastHrOutRelHum=Wthr%OutRelHum(24)
      LastHrWindSpeed=Wthr%WindSpeed(24)
      LastHrWindDir=Wthr%WindDir(24)
      LastHrSkyTemp=Wthr%SkyTemp(24)
      LastHrHorizIRSky=Wthr%HorizIRSky(24)
      LastHrBeamSolarRad=Wthr%BeamSolarRad(24)
      LastHrDifSolarRad=Wthr%DifSolarRad(24)
      LastHrAlbedo=Wthr%Albedo(24)
      LastHrLiquidPrecip=Wthr%LiquidPrecip(24)
      LastHourSet=.true.
    ENDIF

    DO Hour=1,24

      NxtHour = Hour+1
      IF (Hour == 24) THEN
        NxtHour = 1
      END IF
      NextHrBeamSolarRad=Wthr%BeamSolarRad(NxtHour)
      NextHrDifSolarRad=Wthr%DifSolarRad(NxtHour)
      NextHrLiquidPrecip=Wthr%LiquidPrecip(NxtHour)

      DO TS=1,NumOfTimeStepInHour

        WtNow=Interpolation(TS)
        WtPrevHour = 1.0d0-WtNow

        ! Do Solar "weighting"

        WgtHourNow=SolarInterpolation(TS)

        IF (NumOfTimeStepInHour == 1) THEN
          WgtNextHour=1.0d0-WgtHourNow
          WgtPrevHour=0.0d0
        ELSE
          IF (WgtHourNow == 1.0d0) THEN
            !  It's at the half hour
            WgtNextHour=0.0d0
            WgtPrevHour=0.0d0
          ELSEIF (TS*TimeStepFraction < .5d0) THEN
            WgtNextHour=0.0d0
            WgtPrevHour=1.0d0-WgtHourNow
          ELSE  ! After the half hour
            WgtPrevHour=0.0d0
            WgtNextHour=1.0d0-WgtHourNow
          ENDIF
        ENDIF

        TomorrowOutDryBulbTemp(Hour,TS) = LastHrOutDryBulbTemp*WtPrevHour &
                                          + Wthr%OutDryBulbTemp(Hour)*WtNow
        TomorrowOutBaroPress(Hour,TS)   = LastHrOutBaroPress*WtPrevHour   &
                                          + Wthr%OutBaroPress(Hour)*WtNow
        TomorrowOutDewPointTemp(Hour,TS)= LastHrOutDewPointTemp*WtPrevHour &
                                          + Wthr%OutDewPointTemp(Hour)*WtNow
        TomorrowOutRelHum(Hour,TS)      = LastHrOutRelHum*WtPrevHour      &
                                          + Wthr%OutRelHum(Hour)*WtNow
        TomorrowWindSpeed(Hour,TS)      = LastHrWindSpeed*WtPrevHour      &
                                          + Wthr%WindSpeed(Hour)*WtNow
        TomorrowWindDir(Hour,TS)        = LastHrWindDir*WtPrevHour        &
                                          + Wthr%WindDir(Hour)*WtNow
        TomorrowHorizIRSky(Hour,TS)     = LastHrHorizIRSky*WtPrevHour        &
                                          + Wthr%HorizIRSky(Hour)*WtNow
        IF (Environment(Environ)%WP_Type1 == 0) THEN
          TomorrowSkyTemp(Hour,TS)        = LastHrSkyTemp*WtPrevHour        &
                                            + Wthr%SkyTemp(Hour)*WtNow
        ENDIF
        TomorrowDifSolarRad(Hour,TS)    = LastHrDifSolarRad*WgtPrevHour    &
                                          + Wthr%DifSolarRad(Hour)*WgtHourNow  &
                                          + NextHrDifSolarRad*WgtNextHour
        TomorrowBeamSolarRad(Hour,TS)   = LastHrBeamSolarRad*WgtPrevHour   &
                                          + Wthr%BeamSolarRad(Hour)*WgtHourNow &
                                          + NextHrBeamSolarRad*WgtNextHour

        TomorrowLiquidPrecip(Hour,TS)   = LastHrLiquidPrecip*WtPrevHour        &
                                          + Wthr%LiquidPrecip(Hour)*WtNow
        TomorrowLiquidPrecip(Hour,TS)   = TomorrowLiquidPrecip(Hour,TS)/REAL(NumOfTimeStepInHour,r64)

        TomorrowIsRain(Hour,TS)         = (TomorrowLiquidPrecip(Hour,TS) >= .8d0/REAL(NumOfTimeStepInHour,r64))  !Wthr%IsRain(Hour)
        TomorrowIsSnow(Hour,TS)         = Wthr%IsSnow(Hour)
      ENDDO  ! End of TS Loop

      LastHrOutDryBulbTemp=Wthr%OutDryBulbTemp(Hour)
      LastHrOutDewPointTemp=Wthr%OutDewPointTemp(Hour)
      LastHrOutBaroPress=Wthr%OutBaroPress(Hour)
      LastHrOutRelHum=Wthr%OutRelHum(Hour)
      LastHrWindSpeed=Wthr%WindSpeed(Hour)
      LastHrWindDir=Wthr%WindDir(Hour)
      LastHrSkyTemp=Wthr%SkyTemp(Hour)
      LastHrBeamSolarRad=Wthr%BeamSolarRad(Hour)
      LastHrDifSolarRad=Wthr%DifSolarRad(Hour)
      LastHrAlbedo=Wthr%Albedo(Hour)
      LastHrLiquidPrecip=Wthr%LiquidPrecip(Hour)

    ENDDO  ! End of Hour Loop

    IF (Environment(Environ)%WP_Type1 /= 0) THEN
      SELECT CASE(WPSkyTemperature(Environment(Environ)%WP_Type1)%CalculationType)

        CASE (WP_ScheduleValue)
          CALL GetScheduleValuesForDay(WPSkyTemperature(Environment(Environ)%WP_Type1)%SchedulePtr,  &
                              TomorrowSkyTemp,TomorrowVariables%DayOfYear,CurDayOfWeek)
        CASE (WP_DryBulbDelta)
          CALL GetScheduleValuesForDay(WPSkyTemperature(Environment(Environ)%WP_Type1)%SchedulePtr,  &
                              TomorrowSkyTemp,TomorrowVariables%DayOfYear,CurDayOfWeek)
          DO Hour=1,24
            DO TS=1,NumOfTimeStepInHour
              TomorrowSkyTemp(Hour,TS)=TomorrowOutDryBulbTemp(Hour,TS)-TomorrowSkyTemp(Hour,TS)
            ENDDO
          ENDDO

        CASE (WP_DewPointDelta)
          CALL GetScheduleValuesForDay(WPSkyTemperature(Environment(Environ)%WP_Type1)%SchedulePtr,  &
                              TomorrowSkyTemp,TomorrowVariables%DayOfYear,CurDayOfWeek)
          DO Hour=1,24
            DO TS=1,NumOfTimeStepInHour
              TomorrowSkyTemp(Hour,TS)=TomorrowOutDewPointTemp(Hour,TS)-TomorrowSkyTemp(Hour,TS)
            ENDDO
          ENDDO

        CASE DEFAULT

      END SELECT

    ENDIF
  ENDIF

  RETURN

CONTAINS
  SUBROUTINE SetDayOfWeekInitialValues(EnvironDayOfWeek,CurDayOfWeek,UseDayOfWeek)

            ! SUBROUTINE INFORMATION:
            !       AUTHOR         Linda Lawrie
            !       DATE WRITTEN   March 2012
            !       MODIFIED       na
            !       RE-ENGINEERED  na

            ! PURPOSE OF THIS SUBROUTINE:
            ! Set of begin day of week for an environment.  Similar sets but slightly different
            ! conditions.  Improve code readability by having three routine calls instead of three
            ! IF blocks.

            ! METHODOLOGY EMPLOYED:
            ! na

            ! REFERENCES:
            ! na

            ! USE STATEMENTS:
            ! na

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

            ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)    :: EnvironDayOfWeek ! Starting Day of Week for the (Weather) RunPeriod (User Input)
    INTEGER, INTENT(INOUT) :: CurDayOfWeek     ! Current Day of Week
    LOGICAL, INTENT(INOUT) :: UseDayOfWeek     ! hmmm does not appear to be used anywhere.

            ! SUBROUTINE PARAMETER DEFINITIONS:
            ! na

            ! INTERFACE BLOCK SPECIFICATIONS:
            ! na

            ! DERIVED TYPE DEFINITIONS:
            ! na

            ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
            ! na

    IF (EnvironDayOfWeek /= 0) THEN
      IF (EnvironDayOfWeek <= 7) THEN
        CurDayOfWeek=EnvironDayOfWeek-1
      ELSE
        CurDayOfWeek=EnvironDayOfWeek
      ENDIF
      UseDayOfWeek=.false.
    ELSE
      UseDayOfWeek=.true.
    ENDIF

  RETURN

  END SUBROUTINE SetDayOfWeekInitialValues

END SUBROUTINE ReadEPlusWeatherForDay

SUBROUTINE InterpretWeatherDataLine(Line,ErrorFound,WYear,WMonth,WDay,Whour,WMinute,  &
              RField1,RField2,RField3,RField4,RField5,RField6,RField7,RField8,RField9,  &
              RField10,RField11,RField12,RField13,RField14,RField15,RField16,RField17,  &
              RField18,RField19,RField20,WObs,WCodesArr,RField22,RField23,RField24,RField25,  &
              RField26,RField27)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine interprets the EPW weather data line because comma delimited fields
          ! may cause problems with some compilers.  (Particularly character variables in
          ! comma delimited lines.

          ! METHODOLOGY EMPLOYED:
          ! Field by field interpretation, eliminating the "data source field" which is also
          ! likely to contain blanks.  Note that the "Weatherconditions" must be a 9 character
          ! alpha field with no intervening blanks.

          ! REFERENCES:
          ! CALL InterpretWeatherDataLine(WeatherDataLine,ErrorFound,WYear,WMonth,WDay,WHour,WMinute,  &
          !       DryBulb,DewPoint,RelHum,AtmPress,ETHoriz,ETDirect,IRHoriz,GLBHoriz,            &
          !       DirectRad,DiffuseRad,GLBHorizIllum,DirectNrmIllum,DiffuseHorizIllum,ZenLum,    &
          !       WindDir,WindSpeed,TotalSkyCover,OpaqueSkyCover,Visibility,CeilHeight,          &
          !       PresWeathObs,PresWeathConds,PrecipWater,AerosolOptDepth,SnowDepth,DaysSinceLastSnow,
          !       Albedo,LiquidPrecipDepth)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(INOUT) :: Line
  LOGICAL, INTENT(OUT)   :: ErrorFound
  INTEGER, INTENT(OUT)   :: WYear
  INTEGER, INTENT(OUT)   :: WMonth
  INTEGER, INTENT(OUT)   :: WDay
  INTEGER, INTENT(OUT)   :: Whour
  INTEGER, INTENT(OUT)   :: WMinute
  REAL(r64), INTENT(OUT) :: RField1            !       DryBulb
  REAL(r64), INTENT(OUT) :: RField2            !       DewPoint
  REAL(r64), INTENT(OUT) :: RField3            !       RelHum
  REAL(r64), INTENT(OUT) :: RField4            !       AtmPress
  REAL(r64), INTENT(OUT) :: RField5            !       ETHoriz
  REAL(r64), INTENT(OUT) :: RField6            !       ETDirect
  REAL(r64), INTENT(OUT) :: RField7            !       IRHoriz
  REAL(r64), INTENT(OUT) :: RField8            !       GLBHoriz
  REAL(r64), INTENT(OUT) :: RField9            !       DirectRad
  REAL(r64), INTENT(OUT) :: RField10           !       DiffuseRad
  REAL(r64), INTENT(OUT) :: RField11           !       GLBHorizIllum
  REAL(r64), INTENT(OUT) :: RField12           !       DirectNrmIllum
  REAL(r64), INTENT(OUT) :: RField13           !       DiffuseHorizIllum
  REAL(r64), INTENT(OUT) :: RField14           !       ZenLum
  REAL(r64), INTENT(OUT) :: RField15           !       WindDir
  REAL(r64), INTENT(OUT) :: RField16           !       WindSpeed
  REAL(r64), INTENT(OUT) :: RField17           !       TotalSkyCover
  REAL(r64), INTENT(OUT) :: RField18           !       OpaqueSkyCover
  REAL(r64), INTENT(OUT) :: RField19           !       Visibility
  REAL(r64), INTENT(OUT) :: RField20           !       CeilHeight
  INTEGER, INTENT(OUT)   :: WObs               !       PresWeathObs
  INTEGER, DIMENSION(9), INTENT(OUT) ::WCodesArr       !       PresWeathConds
  REAL(r64), INTENT(OUT) :: RField22           !       PrecipWater
  REAL(r64), INTENT(OUT) :: RField23           !       AerosolOptDepth
  REAL(r64), INTENT(OUT) :: RField24           !       SnowDepth
  REAL(r64), INTENT(OUT) :: RField25           !       DaysSinceLastSnow
  REAL(r64), INTENT(OUT) :: RField26           !       Albedo
  REAL(r64), INTENT(OUT) :: RField27           !       LiquidPrecip

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=10), PARAMETER :: ValidDigits='0123456789'
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=LEN(Line)) :: SaveLine
  INTEGER Pos
  CHARACTER(len=20) PresWeathCodes
  REAL(r64) RYear
  REAL(r64) RMonth
  REAL(r64) RDay
  REAL(r64) RHour
  REAL(r64) RMinute
  CHARACTER(len=32) DateError
  REAL(r64)       :: RField21
  INTEGER Count
  INTEGER, SAVE :: LCount=0
  LOGICAL :: DateInError

  LCount=LCount+1
  IF (StripCR) THEN
    Pos=LEN_TRIM(Line)
    IF (ICHAR(Line(Pos:Pos)) == iASCII_CR) Line(Pos:Pos)=Blank
  ENDIF
  ErrorFound=.false.
  SaveLine=Line   ! in case of errors

  ! Do the first five.  (To get to the DataSource field)
  READ(Line,*,ERR=900) RYear,RMonth,RDay,RHour,RMinute
  WYear=NINT(RYear)
  WMonth=NINT(RMonth)
  WDay=NINT(RDay)
  WHour=NINT(RHour)
  WMinute=NINT(RMinute)

  DateInError=.false.
  IF (WMonth >=1 .and. WMonth <=12) THEN
    ! Month number is valid
    IF (WMonth /= 2) THEN
      IF (WDay > EndDayOfMonth(WMonth)) THEN
        DateInError=.true.
      ENDIF
    ELSEIF (WDay > EndDayOfMonth(WMonth)+1) THEN  ! Whether actually used is determined by calling routine.
      DateInError=.true.
    ENDIF
  ELSE
    DateInError=.true.
  ENDIF

  IF (DateInError) THEN
    CALL ShowSevereError('Reading Weather Data Line, Invalid Date, Year='//TRIM(RoundSigDigits(WYear))//  &
        ', Month='//TRIM(RoundSigDigits(WMonth))//', Day='//TRIM(RoundSigDigits(WDay)))
    CALL ShowFatalError('Program terminates due to previous condition.')
  ENDIF

  Pos=INDEX(Line,',')  ! WYear
  IF (Pos == 0) THEN
    GOTO 902
  ENDIF
  Line=Line(Pos+1:)
  Pos=INDEX(Line,',')  ! WMonth
  Line=Line(Pos+1:)
  Pos=INDEX(Line,',')  ! WDay
  Line=Line(Pos+1:)
  Pos=INDEX(Line,',')  ! WHour
  Line=Line(Pos+1:)
  Pos=INDEX(Line,',')  ! WMinute
  Line=Line(Pos+1:)

  ! Data Source/Integrity field -- ignore
  Pos=INDEX(Line,',')
  Line=Line(Pos+1:)

  ! Now read more numerics with List Directed I/O (note there is another "character" field lurking)
  READ(Line,*,err=901) RField1,RField2,RField3,RField4,RField5,RField6,RField7,RField8,RField9,           &
               RField10,RField11,RField12,RField13,RField14,RField15,RField16,RField17,RField18,  &
               RField19,RField20,RField21
  DO Count=1,21
    Pos=INDEX(Line,',')
    Line=Line(Pos+1:)
  ENDDO
  Pos=INDEX(Line,',')
  IF (Pos > 0 .and. Pos /= 1) THEN
    PresWeathCodes=Line(1:Pos-1)
  ELSE
    PresWeathCodes='999999999'
  ENDIF
  Line=Line(Pos+1:)
  Pos=INDEX(Line,',')
  IF (Pos /= 0) THEN
    IF (Pos /= 1) THEN
      READ(Line(1:Pos-1),*,err=901) RField22
    ELSE
      RField22=999.0d0
    ENDIF
    Line=Line(Pos+1:)
    Pos=INDEX(Line,',')
    IF (Pos /= 0) THEN
      IF (Pos /= 1) THEN
        READ(Line(1:Pos-1),*,err=901) RField23
      ELSE
        RField23=999.0d0
      ENDIF
      Line=Line(Pos+1:)
      Pos=INDEX(Line,',')
      IF (Pos /= 0) THEN
        IF (Pos /= 1) THEN
          READ(Line(1:Pos-1),*,err=901) RField24
        ELSE
          RField24=999.0d0
        ENDIF
        Line=Line(Pos+1:)
        Pos=INDEX(Line,',')
        IF (Pos /= 0) THEN
          IF (Pos /= 1) THEN
            READ(Line(1:Pos-1),*,err=901) RField25
          ELSE
            RField25=999.0d0
          ENDIF
          Line=Line(Pos+1:)
          Pos=INDEX(Line,',')
          IF (Pos /= 0) THEN
            IF (Pos /= 1) THEN
              READ(Line(1:Pos-1),*,err=901) RField26
            ELSE
              RField26=999.0d0
            ENDIF
            Line=Line(Pos+1:)
            Pos=INDEX(Line,',')
            IF (Pos /= 0) THEN
              IF (Pos /= 1) THEN
                READ(Line(1:Pos-1),*,err=901) RField27
              ELSE
                RField27=999.0d0
              ENDIF
              Line=Line(Pos+1:)
              Pos=INDEX(Line,',')
            ELSE
              RField27=999.0d0
            ENDIF
          ELSE
            RField26=999.0d0
            RField27=999.0d0
          ENDIF
        ELSE
          READ(Line,*,err=901) RField25
          RField26=999.0d0
          RField27=999.0d0
        ENDIF
      ELSE
        READ(Line,*,err=901) RField24
        RField25=999.0d0
        RField26=999.0d0
        RField27=999.0d0
      ENDIF
    ELSE
      READ(Line,*,err=901) RField23
      RField24=999.0d0
      RField25=999.0d0
      RField26=999.0d0
      RField27=999.0d0
    ENDIF
  ELSE
    READ(Line,*,err=901) RField22
    RField23=999.0d0
    RField24=999.0d0
    RField25=999.0d0
    RField26=999.0d0
    RField27=999.0d0
  ENDIF
!  READ(Line,*,err=903,end=903) RField22,RField23,RField24,RField25

  WObs=NINT(RField21)
  IF (WObs == 0) THEN  ! Obs Indicator indicates Weather Codes valid
    ! Check for miscellaneous characters
    Pos=INDEX(PresWeathCodes,'''')
    DO WHILE (Pos > 0)
      PresWeathCodes(Pos:Pos)=Blank
      Pos=INDEX(PresWeathCodes,'''')
    ENDDO
    Pos=INDEX(PresWeathCodes,'"')
    DO WHILE (Pos > 0)
      PresWeathCodes(Pos:Pos)=Blank
      Pos=INDEX(PresWeathCodes,'"')
    ENDDO
    PresWeathCodes=ADJUSTL(PresWeathCodes)
    IF (LEN_TRIM(PresWeathCodes) == 9) THEN
      DO Pos=1,9
        IF (INDEX(ValidDigits,PresWeathCodes(Pos:Pos)) == 0) PresWeathCodes(Pos:Pos)='9'
      ENDDO
      READ(PresWeathCodes,'(9I1)') WCodesArr
    ELSE
      Missed%WeathCodes=Missed%WeathCodes+1
      WCodesArr=9
    ENDIF
  ELSE
    WCodesArr=9
  ENDIF


  RETURN

  900 CALL ShowSevereError('Invalid Date info in Weather Line')
      CALL ShowContinueError('Entire Data Line='//TRIM(SaveLine))
      CALL ShowFatalError('Error in Reading Weather Data')

  901 WRITE(DateError,"(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)") WYear,WMonth,WDay,WHour,WMinute
      CALL ShowSevereError('Invalid Weather Line at date='//TRIM(DateError))
      CALL ShowContinueError('Full Data Line='//trim(SaveLine))
      CALL ShowContinueError('Remainder of line='//TRIM(Line))
      CALL ShowFatalError('Error in Reading Weather Data')

  902 WRITE(DateError,"(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)") WYear,WMonth,WDay,WHour,WMinute
      CALL ShowSevereError('Invalid Weather Line (no commas) at date='//TRIM(DateError))
      CALL ShowContinueError('Full Data Line='//trim(SaveLine))
      CALL ShowContinueError('Remainder of line='//TRIM(Line))
      CALL ShowFatalError('Error in Reading Weather Data')

  903 WRITE(DateError,"(I4,'/',I2,'/',I2,' Hour#=',I2,' Min#=',I2)") WYear,WMonth,WDay,WHour,WMinute
      CALL ShowSevereError('Invalid Weather Line at date='//TRIM(DateError))
      CALL ShowContinueError('Full Data Line='//trim(SaveLine))
      CALL ShowContinueError('Partial line read; Remainder of line='//TRIM(Line))
      CALL ShowFatalError('Error in Reading Weather Data')

END SUBROUTINE InterpretWeatherDataLine

SUBROUTINE SetUpDesignDay(EnvrnNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 1977
          !       MODIFIED       June 1997 (RKS); May 2013 (LKL) add temperature profile for drybulb.
          !       RE-ENGINEERED  August 2003;LKL -- to generate timestep weather for design days.

          ! PURPOSE OF THIS SUBROUTINE:
          ! This purpose of this subroutine is to convert the user supplied input
          ! values for the design day parameters into an entire weather day
          ! record.  This now bypasses any file I/O by keeping all of the
          ! weather day record information in the local module level derived type
          ! called DesignDay.

          ! METHODOLOGY EMPLOYED:
          ! Methodology incorporates the design day setup from Tarp as appropriate.

          ! REFERENCES:
          ! ASHRAE Handbook of Fundamentals?

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits, JulianDay
  USE InputProcessor, ONLY: SameString, MakeUPPERcase
  USE ScheduleManager, ONLY:GetSingleDayScheduleValues

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: EnvrnNum  ! Environment number passed into the routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: GlobalSolarConstant=1367.d0
  REAL(r64), PARAMETER :: ZHGlobalSolarConstant=1355.d0
  CHARACTER(len=*), PARAMETER :: EnvDDHdFormat="('! <Environment:Design Day Data>, Max Dry-Bulb Temp {C}, ',  &
                                                      & 'Temp Range {dC}, Temp Range Ind Type, ',  &
                                                      & 'Hum Ind Value at Max Temp, Hum Ind Type,Pressure {Pa}, ',  &
                                                      & 'Wind Direction {deg CW from N}, ',   &
                                                      & 'Wind Speed {m/s}, Clearness, Rain, Snow')"
  CHARACTER(len=*), PARAMETER :: EnvDDayFormat="('Environment:Design Day Data,')"
  CHARACTER(len=*), PARAMETER :: DDayMiscHdFormat="('! <Environment:Design_Day_Misc>,DayOfYear,ASHRAE A Coeff,',  &
                                                      & 'ASHRAE B Coeff,ASHRAE C Coeff,Solar Constant-Annual Variation,',  &
                                                      & 'Eq of Time {minutes}, Solar Declination Angle {deg}, Solar Model')"
  CHARACTER(len=*), PARAMETER :: DDayMiscFormat="('Environment:Design_Day_Misc,',I3,',')"
  CHARACTER(len=*), PARAMETER :: fmta='(A)'
  CHARACTER(len=*), PARAMETER :: MnDyFmt="(I2.2,'/',I2.2)"
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_C0=.5598d0  !37.6865d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_C1=.4982d0  !13.9263d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_C2=-.6762d0 !-20.2354d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_C3=.02842d0 !0.9695d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_C4=-.00317d0 !-0.2046d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_C5=.014d0    !-0.0980d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_D=-17.853d0  !-10.8568d0
  REAL(r64), PARAMETER :: ZhangHuangModCoeff_K=.843d0  !49.3112d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE HourlyWeatherData
    REAL(r64), DIMENSION(24) :: BeamSolarRad   = 0.0d0     ! Hourly direct normal solar irradiance
    REAL(r64), DIMENSION(24) :: DifSolarRad    = 0.0d0     ! Hourly sky diffuse horizontal solar irradiance
  END TYPE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  Hour
  INTEGER  TS
  REAL(r64)     A             ! Apparent solar irradiation at air mass = 0
  REAL(r64)     AVSC          ! Annual variation in the solar constant
  REAL(r64)     B             ! Atmospheric extinction coefficient
  REAL(r64)     C             ! ASHRAE diffuse radiation factor
  REAL(r64)     ETR           ! radiation of an extraterrestrial normal surface, W/m2
  REAL(r64)     HO            ! Radiation on an extraterrestial horizontal surface
  REAL(r64)     KT            ! Radiation ratio
  REAL(r64) SUNCOS(3)         ! Sun direction cosines
  INTEGER CurrentYear
  INTEGER OSky                ! Opaque Sky Cover (tenths)
  REAL(r64) HumidityRatio     ! Humidity Ratio -- when constant for day
  REAL(r64) TDewK             ! Dewpoint in Kelvin
  REAL(r64) ESky              ! Emissivitity of Sky
  REAL(r64) CosZenith         ! Cosine of Zenith Angle of Sun
  REAL(r64) TotHoriz          ! Total Radiation on Horizontal Surface
  REAL(r64) GndReflet         ! Ground Reflectivity
  REAL(r64) CurTime           ! For Solar Calcs
  REAL(r64) WetBulb           ! For calculating
  REAL(r64) DBRange           ! working copy of dry-bulb daily range, C (or 1 if input is difference)
  REAL(r64) WBRange           ! working copy of wet-bulb daily range. C (or 1 if input is difference)

  INTEGER, DIMENSION(8) :: Date0
  LOGICAL, SAVE :: PrintDDHeader
  CHARACTER(len=3) AlpUseRain
  CHARACTER(len=3) AlpUseSnow
  REAL(r64) ::  LastHrBeamSolarRad     ! Direct normal solar irradiance
  REAL(r64) ::  LastHrDifSolarRad      ! Sky diffuse horizontal solar irradiance
  REAL(r64) ::  NextHrBeamSolarRad     ! Direct normal solar irradiance
  REAL(r64) ::  NextHrDifSolarRad      ! Sky diffuse horizontal solar irradiance
  LOGICAL :: ConstantHumidityRatio
  REAL(r64) OutHumRat
  REAL(r64) WgtHourNow
  REAL(r64) WgtPrevHour
  REAL(r64) WgtNextHour
  CHARACTER(len=75) :: StringOut
  TYPE (HourlyWeatherData) :: Wthr
  LOGICAL :: SaveWarmupFlag
  REAL(r64) :: GloHorzRad
  REAL(r64) :: ClearnessIndex_kt
  REAL(r64) :: ClearnessIndex_ktc
  REAL(r64) :: ClearnessIndex_kds
  REAL(r64) :: SinSolarAltitude
  REAL(r64) :: TotSkyCover
  INTEGER :: Hour1Ago,Hour3Ago
  REAL(r64) :: BeamRad, DiffRad     ! working calculated beam and diffuse rad, W/m2
  REAL(r64) :: testval
!     For reporting purposes, set year to current system year
   SaveWarmupFlag=WarmupFlag
   WarmupFlag=.true.

   CALL DATE_AND_TIME(Values=Date0)
   CurrentYear=Date0(1)

   IF (BeginSimFlag) THEN
     PrintDDHeader=.true.
   ENDIF

   DesignDay(EnvrnNum)%Year = CurrentYear ! f90 date_and_time implemented. full 4 digit year !+ 1900
   DesignDay(EnvrnNum)%Month = DesDayInput(EnvrnNum)%Month
   DesignDay(EnvrnNum)%DayOfMonth = DesDayInput(EnvrnNum)%DayOfMonth
   DesignDay(EnvrnNum)%DayOfYear = JulianDay(DesignDay(EnvrnNum)%Month,DesignDay(EnvrnNum)%DayOfMonth,0)
   WRITE(CurMnDy,MnDyFmt) DesDayInput(EnvrnNum)%Month,DesDayInput(EnvrnNum)%DayOfMonth
   EnvironmentName=DesDayInput(EnvrnNum)%Title
   RunPeriodEnvironment=.false.
          ! Following builds Environment start/end for ASHRAE 55 warnings
   EnvironmentStartEnd=TRIM(CurMnDy)//' - '//TRIM(CurMnDy)

   ! Check that barometric pressure is within range
   IF (DesDayInput(EnvrnNum)%PressureEntered) THEN
     IF (ABS((DesDayInput(EnvrnNum)%PressBarom-StdBaroPress)/StdBaroPress) > .1d0) THEN  ! 10% off
       CALL ShowWarningError('SetUpDesignDay: Entered DesignDay Barometric Pressure='//  &
                             TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%PressBarom,0))//   &
                             ' differs by more than 10% from Standard Barometric Pressure='//  &
                             TRIM(RoundSigDigits(StdBaroPress,0))//'.')
       CALL ShowContinueError('...occurs in DesignDay='//TRIM(EnvironmentName)//  &
                              ', Standard Pressure (based on elevation) will be used.')
       DesDayInput(EnvrnNum)%PressBarom=StdBaroPress
     ENDIF
   ELSE
     DesDayInput(EnvrnNum)%PressBarom=StdBaroPress
   ENDIF

   ! verify that design WB or DP <= design DB
   IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_Dewpoint .and. DesDayInput(EnvrnNum)%DewpointNeedsSet) THEN
     ! dew-point
     testval=PsyWFnTdbRhPb(DesDayInput(EnvrnNum)%MaxDryBulb,1.0d0,DesDayInput(EnvrnNum)%PressBarom)
     DesDayInput(EnvrnNum)%HumIndValue=PsyTdpFnWPb(testval,DesDayInput(EnvrnNum)%PressBarom)
   ENDIF

   ! Day of week defaults to Monday, if day type specified, then that is used.
   DesignDay(EnvrnNum)%DayOfWeek = 2
   IF (DesDayInput(EnvrnNum)%DayType <= 7) DesignDay(EnvrnNum)%DayOfWeek = DesDayInput(EnvrnNum)%DayType

   ! set Holiday as indicated by user input
   DesignDay(EnvrnNum)%HolidayIndex = 0
   IF (DesDayInput(EnvrnNum)%DayType > 7) DesignDay(EnvrnNum)%HolidayIndex = DesDayInput(EnvrnNum)%DayType-7

   DesignDay(EnvrnNum)%DaylightSavingIndex = DesDayInput(EnvrnNum)%DSTIndicator

   !  Set up Solar parameters for day
   CALL CalculateDailySolarCoeffs(DesignDay(EnvrnNum)%DayOfYear,A,B,C,AVSC,DesignDay(EnvrnNum)%EquationOfTime, &
                                 DesignDay(EnvrnNum)%SinSolarDeclinAngle,DesignDay(EnvrnNum)%CosSolarDeclinAngle)

   IF (PrintDDHeader .and. DoWeatherInitReporting) THEN
     WRITE(OutputFileInits,EnvDDHdFormat)
     WRITE(OutputFileInits,DDayMiscHdFormat)
     PrintDDHeader=.false.
   ENDIF
   IF (DoWeatherInitReporting) THEN
     IF (DesDayInput(Envrn)%RainInd == 1) THEN
       AlpUseRain='Yes'
     ELSE
       AlpUseRain='No'
     ENDIF
     IF (DesDayInput(Envrn)%SnowInd == 1) THEN
       AlpUseSnow='Yes'
     ELSE
       AlpUseSnow='No'
     ENDIF
     WRITE(OutputFileInits,EnvDDayFormat,advance='No')
     StringOut=RoundSigDigits(DesDayInput(Envrn)%MaxDryBulb,2)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(DesDayInput(Envrn)%DailyDBRange,2)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=','
     IF (DesDayInput(Envrn)%DBTempRangeType == DDDBRangeType_Default) THEN
       StringOut='DefaultMultipliers,'
     ELSEIF (DesDayInput(Envrn)%DBTempRangeType == DDDBRangeType_Multiplier) THEN
       StringOut='MultiplierSchedule,'
     ELSEIF (DesDayInput(Envrn)%DBTempRangeType == DDDBRangeType_Profile) THEN
       StringOut='TemperatureProfile,'
     ELSEIF (DesDayInput(Envrn)%DBTempRangeType == DDDBRangeType_Difference) THEN
       StringOut='DifferenceSchedule,'
     ENDIF
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)
     IF (DesDayInput(Envrn)%HumIndType == DDHumIndType_Wetbulb) THEN
       StringOut='Wetbulb,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,2))//' {C},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_Dewpoint) THEN
       StringOut='Dewpoint,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,2))//' {C},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_Enthalpy) THEN
       StringOut='Enthalpy,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,2))//' {kJ/kg},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_HumRatio) THEN
       StringOut='HumidityRatio,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,4))//' {},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_RelHumSch) THEN
       StringOut='Schedule,<schedule values from 0.0 to 100.0 {percent},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_WBProfDef) THEN
       StringOut='WetBulbProfileDefaultMultipliers,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,2))//' {C},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_WBProfDif) THEN
       StringOut='WetBulbProfileDifferenceSchedule,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,2))//' {C},'
     ELSEIF (DesDayInput(Envrn)%HumIndType == DDHumIndType_WBProfMul) THEN
       StringOut='WetBulbProfileMultiplierSchedule,'//trim(RoundSigDigits(DesDayInput(Envrn)%HumIndValue,2))//' {C},'
     ENDIF
     StringOut=RoundSigDigits(DesDayInput(Envrn)%PressBarom,0)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(DesDayInput(Envrn)%WindDir,0)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(DesDayInput(Envrn)%WindSpeed,1)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(DesDayInput(Envrn)%SkyClear,2)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     WRITE(OutputFileInits,fmta) AlpUseRain//','//AlpUseSnow

     WRITE(OutputFileInits,DDayMiscFormat,advance='No') DesignDay(EnvrnNum)%DayOfYear
     StringOut=RoundSigDigits(A,1)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(B,4)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(C,4)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(AVSC,1)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(DesignDay(EnvrnNum)%EquationOfTime*60.0d0,2)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     StringOut=RoundSigDigits(ASIN(DesignDay(EnvrnNum)%SinSolarDeclinAngle)/DegToRadians,1)
     WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
     IF (DesDayInput(EnvrnNum)%SolarModel == ASHRAE_ClearSky) THEN
       StringOut='ASHRAEClearSky'
     ELSEIF (DesDayInput(EnvrnNum)%SolarModel == Zhang_Huang) THEN
       StringOut='ZhangHuang'
     ELSEIF (DesDayInput(EnvrnNum)%SolarModel == SolarModel_Schedule) THEN
       StringOut='User supplied beam/diffuse from schedules'
     ELSEIF (DesDayInput(EnvrnNum)%SolarModel == ASHRAE_Tau) THEN
       StringOut='ASHRAETau'
     ELSE
       StringOut='unknown'
     ENDIF
     WRITE(OutputFileInits,fmta) TRIM(StringOut)
   ENDIF


   ! Must set up weather values for Design Day.  User can specify the "humidity indicator" as
   ! Wetbulb, DewPoint or input the relative humidity schedule.  For both wetbulb and dewpoint indicators, the
   ! humidity for the day will be constant, using the drybulb (max) and humidity indicator temperature to
   ! set the values.  For the scheduled values, these are already set in the DDxxx array.

   CurrentTime=25.0d0

   SELECT CASE(DesDayInput(Envrn)%HumIndType)

   CASE (DDHumIndType_Wetbulb)
     HumidityRatio=PsyWFnTdbTwbPb(DesDayInput(EnvrnNum)%MaxDryBulb,          &
                          DesDayInput(EnvrnNum)%HumIndValue,          &
                          DesDayInput(EnvrnNum)%PressBarom,'SetUpDesignDay:PsyWFnTdbTwbPb')
     ConstantHumidityRatio=.true.

   CASE (DDHumIndType_Dewpoint)
     HumidityRatio=PsyWFnTdpPb(DesDayInput(EnvrnNum)%HumIndValue,          &
                          DesDayInput(EnvrnNum)%PressBarom,'SetUpDesignDay:PsyWFnTdpPb')
     ConstantHumidityRatio=.true.

   CASE (DDHumIndType_HumRatio)
     HumidityRatio=DesDayInput(EnvrnNum)%HumIndValue
     ConstantHumidityRatio=.true.

   CASE (DDHumIndType_Enthalpy)
     HumidityRatio=PsyWFnTdbH(DesDayInput(EnvrnNum)%MaxDryBulb,DesDayInput(EnvrnNum)%HumIndValue*1000.0d0,  &
        'SetUpDesignDay:PsyWFnTdbH')
     ConstantHumidityRatio=.true.

   CASE (DDHumIndType_RelHumSch)
     ! nothing to do -- DDHumIndModifier already contains the scheduled Relative Humidity
     ConstantHumidityRatio=.false.
     TomorrowOutRelHum=DDHumIndModifier( EnvrnNum,:,:)

   CASE (DDHumIndType_WBProfDef, DDHumIndType_WBProfDif, DDHumIndType_WBProfMul)
     ConstantHumidityRatio = .false.

   CASE DEFAULT
     CALL ShowSevereError('SetUpDesignDay: Invalid Humidity Indicator type')
     CALL ShowContinueError('Occurred in Design Day='//TRIM(DesDayInput(Envrn)%Title))

   END SELECT

   IF (DesDayInput(EnvrnNum)%RainInd /= 0) THEN
     TomorrowIsRain(:,:)=.true.
     OSky=10
     TomorrowLiquidPrecip=3.0d0
   ELSE
     TomorrowIsRain(:,:)=.false.
     OSky=0
     TomorrowLiquidPrecip=0.0d0
   ENDIF

   IF (DesDayInput(EnvrnNum)%SnowInd == 0) THEN
     TomorrowIsSnow(:,:)=.false.
     GndReflet=.2d0
   ELSE  ! Snow
     TomorrowIsSnow(:,:)=.true.
     GndReflet = .7d0
   ENDIF

   ! Some values are constant

   TomorrowOutBaroPress(:,:) = DesDayInput(EnvrnNum)%PressBarom
   TomorrowWindSpeed(:,:)    = DesDayInput(EnvrnNum)%WindSpeed
   TomorrowWindDir(:,:)      = DesDayInput(EnvrnNum)%WindDir
   TomorrowAlbedo=0.0d0

   ! resolve daily ranges
   IF (DesDayInput(EnvrnNum)%DBTempRangeType == DDDBRangeType_Difference) THEN
      DBRange = 1.d0      ! use unscaled multiplier values if difference
   ELSEIF (DesDayInput(EnvrnNum)%DBTempRangeType == DDDBRangeType_Profile) THEN
      DBRange = 0.0d0
   ELSE
      DBRange = DesDayInput(EnvrnNum)%DailyDBRange
   ENDIF
   IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDif) THEN
      WBRange = 1.d0      ! use unscaled multiplier values if difference
   ELSE
      WBRange = DesDayInput(EnvrnNum)%DailyWBRange
   ENDIF

   DO Hour = 1,24
     DO TS=1,NumOfTimeStepInHour

       IF (DesDayInput(EnvrnNum)%DBTempRangeType /= DDDBRangeType_Profile) THEN
         ! dry-bulb profile
         TomorrowOutDryBulbTemp(Hour,TS)=DesDayInput(EnvrnNum)%MaxDryBulb - DDDBRngModifier(EnvrnNum,Hour,TS)*DBRange
       ELSE ! DesDayInput(EnvrnNum)%DBTempRangeType == DDDBRangeType_Profile
         TomorrowOutDryBulbTemp(Hour,TS)=DDDBRngModifier(EnvrnNum,Hour,TS)
       ENDIF


       ! wet-bulb - generate from profile, humidity ratio, or dew point
       IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDef         &
        .or. DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDif       &
        .or. DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfMul) THEN
         WetBulb = DesDayInput(EnvrnNum)%HumIndValue - DDHumIndModifier(EnvrnNum,Hour,TS)*WBRange
         WetBulb = MIN( WetBulb, TomorrowOutDryBulbTemp(Hour,TS))   ! WB must be <= DB
         OutHumRat = PsyWFnTdbTwbPb(TomorrowOutDryBulbTemp(Hour,TS), &
                              WetBulb, DesDayInput(EnvrnNum)%PressBarom)
         TomorrowOutDewPointTemp(Hour,TS)=PsyTdpFnWPb(OutHumRat,DesDayInput(EnvrnNum)%PressBarom)
         TomorrowOutRelHum(Hour,TS)=PsyRhFnTdbWPb(TomorrowOutDryBulbTemp(Hour,TS),OutHumRat, &
                                               DesDayInput(EnvrnNum)%PressBarom,'WeatherManager')*100.0d0
       ELSE IF (ConstantHumidityRatio) THEN
       !  Need Dew Point Temperature.  Use Relative Humidity to get Humidity Ratio, unless Humidity Ratio is constant
        !BG 9-26-07  moved following inside this IF statment; when HumIndType is 'Schedule' HumidityRatio wasn't being initialized
         WetBulb = PsyTwbFnTdbWPb(TomorrowOutDryBulbTemp(Hour,TS),HumidityRatio,DesDayInput(EnvrnNum)%PressBarom, &
         'WeatherManager.f90 subroutine SetUpDesignDay')

         OutHumRat = PsyWFnTdpPb(TomorrowOutDryBulbTemp(Hour,TS),DesDayInput(EnvrnNum)%PressBarom)
         IF (HumidityRatio > OutHumRat) THEN
           WetBulb=TomorrowOutDryBulbTemp(Hour,TS)
         ELSE
           OutHumRat = PsyWFnTdbTwbPb(TomorrowOutDryBulbTemp(Hour,TS), &
                              WetBulb,DesDayInput(EnvrnNum)%PressBarom)
         ENDIF
         TomorrowOutDewPointTemp(Hour,TS)=PsyTdpFnWPb(OutHumRat,DesDayInput(EnvrnNum)%PressBarom)
         TomorrowOutRelHum(Hour,TS)=PsyRhFnTdbWPb(TomorrowOutDryBulbTemp(Hour,TS),OutHumRat, &
                                               DesDayInput(EnvrnNum)%PressBarom,'WeatherManager')*100.0d0
       ELSE
         HumidityRatio=PsyWFnTdbRhPb(TomorrowOutDryBulbTemp(Hour,TS),DDHumIndModifier(EnvrnNum,Hour,TS)/100.0d0,  &
                                               DesDayInput(EnvrnNum)%PressBarom)
         ! TomorrowOutRelHum values set earlier
         TomorrowOutDewPointTemp(Hour,TS)=PsyTdpFnWPb(HumidityRatio,DesDayInput(EnvrnNum)%PressBarom)
       ENDIF

       ! Determine Sky Temp ==>
       ! Function of DryBulb, DewPoint, OpaqueSkyCover
       ! Calculate Sky IR
       !HIR = ESKY * SIGMA * (TOUT**4)
       !
       !where
       !
       !HIR = horizontal IR intensity (W/m2)
       !ESKY = sky emissivity
       !SIGMA = Stefan-Boltzmann constant = 5.6697e-8 W/m2-K4
       !TOUT = drybulb temperature (K)
       !
       !The sky emissivity is given by
       !
       !ESKY = [0.787 + 0.764*ln(TDEW/273)]*[1 + 0.0224*N - 0.0035*(N**2) + 0.00028*(N**3)]
       !
       !where
       !
       !TDEW = dewpoint temperature (K)
       !N = opaque sky cover (tenths)
       !
       !Example: Clear sky (N=0), TOUT = 273+20=293K, TDEW = 273+10=283K:
       !
       !ESKY = 0.787 + 0.764*0.036 = 0.815
       !HIR = 0.815*5.6697e-8*(293**4) = 340.6 W/m2

       !References:
       !
       !George N. Walton, "Thermal Analysis Research Program Reference Manual,"
       !NBSIR 83-2655, March 1983, p. 21.
       !
       !G. Clark and C. Allen, "The Estimation of Atmospheric Radiation for Clear and
       !Cloudy Skies," Proc. 2nd National Passive Solar Conference (AS/ISES), 1978, pp. 675-678.

       IF (Environment(EnvrnNum)%WP_Type1 == 0) THEN
         TDewK=MIN(TomorrowOutDryBulbTemp(Hour,TS),TomorrowOutDewPointTemp(Hour,TS))+TKelvin
         ESky= (.787d0 +.764d0*LOG((TDewK)/TKelvin))*(1.d0 + .0224d0*OSky - 0.0035d0*(OSky**2) + .00028d0*(OSky**3))
         TomorrowHorizIRSky(Hour,TS)=Esky*Sigma*(TomorrowOutDryBulbTemp(Hour,TS)+TKelvin)**4
         TomorrowSkyTemp(Hour,TS)=(TomorrowOutDryBulbTemp(Hour,TS)+TKelvin)*(ESky**.25d0)-TKelvin
       ELSE
         TDewK=MIN(TomorrowOutDryBulbTemp(Hour,TS),TomorrowOutDewPointTemp(Hour,TS))+TKelvin
         ESky= (.787d0 +.764d0*LOG((TDewK)/TKelvin))*(1.d0 + .0224d0*OSky - 0.0035d0*(OSky**2) + .00028d0*(OSky**3))
         TomorrowHorizIRSky(Hour,TS)=Esky*Sigma*(TomorrowOutDryBulbTemp(Hour,TS)+TKelvin)**4
       ENDIF

       ! Generate solar values for timestep
       !    working results = BeamRad and DiffRad
       !    stored to program globals at end of loop
       IF (DesDayInput(EnvrnNum)%SolarModel == SolarModel_Schedule) THEN
         ! scheduled: set value unconditionally (whether sun up or not)
         BeamRad=DDBeamSolarValues(EnvrnNum,Hour,TS)
         DiffRad=DDDiffuseSolarValues(EnvrnNum,Hour,TS)
       ELSE

       ! calc time = fractional hour of day
         IF (NumOfTimeStepInHour /= 1) THEN
           CurTime = REAL(Hour-1,r64) + REAL(TS,r64)*TimeStepFraction
         ELSE
           CurTime = REAL(Hour,r64)+TS1TimeOffset
         ENDIF

         CALL CalculateSunDirectionCosines(CurTime,DesignDay(EnvrnNum)%EquationOfTime,DesignDay(EnvrnNum)%SinSolarDeclinAngle,  &
                                         DesignDay(EnvrnNum)%CosSolarDeclinAngle,SUNCOS)
         CosZenith=SUNCOS(3)
         IF (CosZenith < SunIsUpValue) THEN
           BeamRad = 0.d0
           DiffRad = 0.d0
         ELSE
           SinSolarAltitude=SUNCOS(3)

           SELECT CASE (DesDayInput(EnvrnNum)%SolarModel)

           CASE (Ashrae_ClearSky)
             TotHoriz = DesDayInput(EnvrnNum)%SkyClear * A * (C + CosZenith) * EXP( -B / CosZenith)
             HO=GlobalSolarConstant*AVSC*CosZenith
             KT=TotHoriz/HO
             KT=MIN(KT,.75d0)
             DiffRad = TotHoriz * (1.0045d0 + KT * (.04349d0 + KT * (-3.5227d0 + 2.6313d0 * KT)))
             IF (DesDayInput(EnvrnNum)%SkyClear .GT. 0.70d0) DiffRad = TotHoriz*C/(C+CosZenith)
             BeamRad = (TotHoriz-DiffRad)/CosZenith
             DiffRad = MAX(0.0d0,DiffRad)
             BeamRad = MAX(0.0d0,BeamRad)

           CASE (ASHRAE_Tau)
             ETR = GlobalSolarConstant*AVSC      ! extraterrestrial normal irrad, W/m2
             CALL ASHRAETauModel( ETR, CosZenith, DesDayInput(EnvrnNum)%TauB, DesDayInput(EnvrnNum)%TauD, &
                  BeamRad, DiffRad, GloHorzRad)

           CASE (Zhang_Huang)
             Hour3Ago = MOD( Hour+20, 24)+1       ! hour 3 hours before
             TotSkyCover=MAX( 1.0d0-DesDayInput(EnvrnNum)%SkyClear, 0.0d0)
             GloHorzRad = ( ZHGlobalSolarConstant * SinSolarAltitude * (ZhangHuangModCoeff_C0 &
                 + ZhangHuangModCoeff_C1 * TotSkyCover &
                 + ZhangHuangModCoeff_C2 * (TotSkyCover)**2 &
                 + ZhangHuangModCoeff_C3 * (TomorrowOutDryBulbTemp(Hour,TS) - TomorrowOutDryBulbTemp(Hour3Ago,TS)) &
                 + ZhangHuangModCoeff_C4 * TomorrowOutRelHum(Hour,TS) &
                 + ZhangHuangModCoeff_C5 * TomorrowWindSpeed(Hour,TS)) + ZhangHuangModCoeff_D ) &
                 / ZhangHuangModCoeff_K
             GloHorzRad = MAX( GloHorzRad,0.0d0)
             ClearnessIndex_kt=GloHorzRad/(GlobalSolarConstant * SinSolarAltitude)
  !          ClearnessIndex_kt=DesDayInput(EnvrnNum)%SkyClear
             ClearnessIndex_Ktc = 0.4268d0 + 0.1934d0 * SinSolarAltitude
             IF (ClearnessIndex_Kt < ClearnessIndex_Ktc) THEN
               ClearnessIndex_Kds = (3.996d0 -3.862d0*SinSolarAltitude +1.54d0*SinSolarAltitude**2)* &
                     ClearnessIndex_Kt**3
             ELSE
               ClearnessIndex_Kds = ClearnessIndex_Kt - (1.107d0 + 0.03569d0 * SinSolarAltitude + 1.681d0 * &
                     SinSolarAltitude**2) * (1.d0-ClearnessIndex_Kt)**3
             ENDIF
             ! Calculate direct normal radiation, W/m2
             BeamRad = ZHGlobalSolarConstant * SinSolarAltitude * ClearnessIndex_Kds * &
                  ((1.d0 - ClearnessIndex_Kt) / (1.d0 - ClearnessIndex_Kds))
             ! Calculation diffuse horizontal radiation, W/m2
             DiffRad = ZHGlobalSolarConstant * SinSolarAltitude * &
                  ((ClearnessIndex_Kt - ClearnessIndex_Kds) / (1.d0 - ClearnessIndex_Kds))

           CASE DEFAULT
         END SELECT
       END IF
     END IF

     ! override result to 0 per environment var (for testing)
     IF (IgnoreSolarRadiation .or. IgnoreBeamRadiation) BeamRad = 0.0d0
     IF (IgnoreSolarRadiation .or. IgnoreDiffuseRadiation) DiffRad = 0.0d0;

     TomorrowBeamSolarRad( Hour,TS) = BeamRad
     TomorrowDifSolarRad( Hour,TS)  = DiffRad

     END DO   ! Timestep (TS) Loop
   ENDDO  ! Hour Loop

   ! back-fill hour values from timesteps
   ! hour values = integrated over hour ending at time of hour
   ! insurance: hourly values not known to be needed
   DO Hour = 1,24
     Hour1Ago = MOD( Hour+22, 24)+1
     BeamRad = (TomorrowBeamSolarRad( Hour1Ago, NumOfTimeStepInHour) + TomorrowBeamSolarRad( Hour, NumOfTimeStepInHour)) / 2.0d0
     DiffRad = (TomorrowDifSolarRad(  Hour1Ago, NumOfTimeStepInHour) + TomorrowDifSolarRad(  Hour, NumOfTimeStepInHour)) / 2.0d0
     IF (NumOfTimeStepInHour > 1) THEN
        BeamRad = BeamRad + SUM( TomorrowBeamSolarRad( Hour, 1:NumOfTimeStepInHour-1))
        DiffRad = DiffRad + SUM( TomorrowDifSolarRad(  Hour, 1:NumOfTimeStepInHour-1))
     ENDIF
     Wthr%BeamSolarRad( Hour) = BeamRad / NumOfTimeStepInHour
     Wthr%DifSolarRad( Hour)  = DiffRad / NumOfTimeStepInHour
   END DO

   IF (Environment(EnvrnNum)%WP_Type1 /= 0) THEN

     SELECT CASE(WPSkyTemperature(Environment(EnvrnNum)%WP_Type1)%CalculationType)

       CASE (WP_ScheduleValue)
         CALL GetSingleDayScheduleValues(WPSkyTemperature(Environment(EnvrnNum)%WP_Type1)%SchedulePtr,  &
                             TomorrowSkyTemp)
         DDSkyTempScheduleValues(EnvrnNum,:,:)=TomorrowSkyTemp
       CASE (WP_DryBulbDelta)
         CALL GetSingleDayScheduleValues(WPSkyTemperature(Environment(EnvrnNum)%WP_Type1)%SchedulePtr,  &
                             TomorrowSkyTemp)
         DDSkyTempScheduleValues(EnvrnNum,:,:)=TomorrowSkyTemp
         DO Hour=1,24
           DO TS=1,NumOfTimeStepInHour
             TomorrowSkyTemp(Hour,TS)=TomorrowOutDryBulbTemp(Hour,TS)-TomorrowSkyTemp(Hour,TS)
           ENDDO
         ENDDO

       CASE (WP_DewPointDelta)
         CALL GetSingleDayScheduleValues(WPSkyTemperature(Environment(EnvrnNum)%WP_Type1)%SchedulePtr,  &
                             TomorrowSkyTemp)
         DDSkyTempScheduleValues(EnvrnNum,:,:)=TomorrowSkyTemp
         DO Hour=1,24
           DO TS=1,NumOfTimeStepInHour
             TomorrowSkyTemp(Hour,TS)=TomorrowOutDewPointTemp(Hour,TS)-TomorrowSkyTemp(Hour,TS)
           ENDDO
         ENDDO

       CASE DEFAULT

     END SELECT

   ENDIF



   WarmupFlag=SaveWarmupFlag

   RETURN

END SUBROUTINE SetUpDesignDay

!------------------------------------------------------------------------------
REAL(r64) FUNCTION AirMass(CosZen)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         C Barnaby
          !       DATE WRITTEN   Nov 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate relative air mass using Kasten and Young approximation

          ! METHODOLOGY EMPLOYED:
          ! Eqn (16), ASHRAE HOF 2009, p. 14.9

          ! REFERENCES:
          ! ASHRAE HOF 2009 Chapter 14
          ! Kasten, F and T. Young.  1989.  Revised optical air mass tables
          !   and approximating formula.  Applied Optics 28:4735-4738.

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN) :: CosZen    ! COS( solar zenith), 0 - 1


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: SunAltD

    IF (CosZen <= 0.001d0) THEN
        AirMass = 37.07837343d0   ! limit value calc'd with Excel
                                !  value increases little as CosZen -> 0
    ELSE IF (CosZen >= 1.d0) THEN
        AirMass = 1.d0
    ELSE
        ! note: COS( Zen) = SIN( Alt)
        SunAltD = ASIN( CosZen) / DegToRadians      ! altitude, degrees
        AirMass = 1.d0/(CosZen + 0.50572d0 * (6.07995d0 + SunAltD)**(-1.6364d0))
    END IF
END FUNCTION AirMass
!------------------------------------------------------------------------------
SUBROUTINE ASHRAETauModel( ETR, CosZen, TauB, TauD, IDirN, IDifH, IGlbH)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         C Barnaby
          !       DATE WRITTEN   Nov 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate clear-sky direct and diffuse irradiance using ASHRAE "tau" model

          ! METHODOLOGY EMPLOYED:
          ! Eqns (17-18), ASHRAE HOF 2009, p. 14.9

          ! REFERENCES:
          ! ASHRAE HOF 2009 Chapter 14

          ! USE STATEMENTS:
          ! na

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT( IN) :: ETR       ! extraterrestrial normal irradiance, W/m2
    REAL(r64), INTENT( IN) :: CosZen    ! COS( solar zenith angle), 0 - 1
    REAL(r64), INTENT( IN) :: TauB      ! beam tau factor
    REAL(r64), INTENT( IN) :: TauD      ! dif tau factor
    REAL(r64), INTENT(OUT) :: IDirN     ! returned: direct (beam) irradiance on normal surface, W/m2
    REAL(r64), INTENT(OUT) :: IDifH     ! returned: diffuse irradiance on horiz surface, W/m2
    REAL(r64), INTENT(OUT) :: IGlbH     ! returned: global irradiance on horiz surface, W/m2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    REAL(r64) AB, AD    ! air mass exponents
    REAL(r64) M         ! air mass

    IF (CosZen < SunIsUpValue .OR. TauB <= 0.0d0 .OR. TauD <= 0.0d0) THEN
        IDirN = 0.0d0
        IDifH = 0.0d0
        IGlbH = 0.0d0
    ELSE
        AB = 1.219d0 - 0.043d0*TauB - 0.151d0*TauD - 0.204d0*TauB*TauD
        AD = 0.202d0 + 0.852d0*TauB - 0.007d0*Taud - 0.357d0*TauB*TauD
        M = AirMass( CosZen)
        IDirN = ETR * EXP( -TauB * M**AB)
        IDifH = ETR * EXP( -TauD * M**AD)
        IGlbH = IDirN * CosZen + IDifH
    END IF

END SUBROUTINE ASHRAETauModel

SUBROUTINE AllocateWeatherData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine allocates the weather data structures (Today, Tomorrow,
          ! Design Day) to the proper number of "time steps in hour" requested by the user.
          ! Interpolation of data is done later after either setting up the design day (hourly
          ! data) or reading in hourly weather data.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

  ALLOCATE(TodayIsRain(24,NumOfTimeStepInHour))
  TodayIsRain=.false.
  ALLOCATE(TodayIsSnow(24,NumOfTimeStepInHour))
  TodayIsSnow=.false.
  ALLOCATE(TodayOutDryBulbTemp(24,NumOfTimeStepInHour))
  TodayOutDryBulbTemp=0.0d0
  ALLOCATE(TodayOutDewPointTemp(24,NumOfTimeStepInHour))
  TodayOutDewPointTemp=0.0d0
  ALLOCATE(TodayOutBaroPress(24,NumOfTimeStepInHour))
  TodayOutBaroPress=0.0d0
  ALLOCATE(TodayOutRelHum(24,NumOfTimeStepInHour))
  TodayOutRelHum=0.0d0
  ALLOCATE(TodayWindSpeed(24,NumOfTimeStepInHour))
  TodayWindSpeed=0.0d0
  ALLOCATE(TodayWindDir(24,NumOfTimeStepInHour))
  TodayWindDir=0.0d0
  ALLOCATE(TodaySkyTemp(24,NumOfTimeStepInHour))
  TodaySkyTemp=0.0d0
  ALLOCATE(TodayHorizIRSky(24,NumOfTimeStepInHour))
  TodayHorizIRSky=0.0d0
  ALLOCATE(TodayBeamSolarRad(24,NumOfTimeStepInHour))
  TodayBeamSolarRad=0.0d0
  ALLOCATE(TodayDifSolarRad(24,NumOfTimeStepInHour))
  TodayDifSolarRad=0.0d0
  ALLOCATE(TodayAlbedo(24,NumOfTimeStepInHour))
  TodayAlbedo=0.0d0
  ALLOCATE(TodayLiquidPrecip(24,NumOfTimeStepInHour))
  TodayLiquidPrecip=0.0d0

  ALLOCATE(TomorrowIsRain(24,NumOfTimeStepInHour))
  TomorrowIsRain=.false.
  ALLOCATE(TomorrowIsSnow(24,NumOfTimeStepInHour))
  TomorrowIsSnow=.false.
  ALLOCATE(TomorrowOutDryBulbTemp(24,NumOfTimeStepInHour))
  TomorrowOutDryBulbTemp=0.0d0
  ALLOCATE(TomorrowOutDewPointTemp(24,NumOfTimeStepInHour))
  TomorrowOutDewPointTemp=0.0d0
  ALLOCATE(TomorrowOutBaroPress(24,NumOfTimeStepInHour))
  TomorrowOutBaroPress=0.0d0
  ALLOCATE(TomorrowOutRelHum(24,NumOfTimeStepInHour))
  TomorrowOutRelHum=0.0d0
  ALLOCATE(TomorrowWindSpeed(24,NumOfTimeStepInHour))
  TomorrowWindSpeed=0.0d0
  ALLOCATE(TomorrowWindDir(24,NumOfTimeStepInHour))
  TomorrowWindDir=0.0d0
  ALLOCATE(TomorrowSkyTemp(24,NumOfTimeStepInHour))
  TomorrowSkyTemp=0.0d0
  ALLOCATE(TomorrowHorizIRSky(24,NumOfTimeStepInHour))
  TomorrowHorizIRSky=0.0d0
  ALLOCATE(TomorrowBeamSolarRad(24,NumOfTimeStepInHour))
  TomorrowBeamSolarRad=0.0d0
  ALLOCATE(TomorrowDifSolarRad(24,NumOfTimeStepInHour))
  TomorrowDifSolarRad=0.0d0
  ALLOCATE(TomorrowAlbedo(24,NumOfTimeStepInHour))
  TomorrowAlbedo=0.0d0
  ALLOCATE(TomorrowLiquidPrecip(24,NumOfTimeStepInHour))
  TomorrowLiquidPrecip=0.0d0

  RETURN

END SUBROUTINE AllocateWeatherData

SUBROUTINE CalculateDailySolarCoeffs(DayOfYear,A,B,C,AnnVarSolConstant,EquationOfTime,SineSolarDeclination,CosineSolarDeclination)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   May 1985
          !       MODIFIED       1999 for EnergyPlus
          !       RE-ENGINEERED  2001; LKL; Remove need for English -> SI conversion
          !                      Implement Tarp "fix" for Southern Hemisphere

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the daily solar coefficients used in other
          ! calculations.  Specifically, this routine computes values of the solar declination, equation
          ! of time, and ashrae sky coefficients a, b, and c for a given
          ! day of the year.


          ! METHODOLOGY EMPLOYED:
          ! The method is the same as that recommended in the ASHRAE loads
          ! algorithms manual, except that the fourier series expressions
          ! have been extended by two terms for greater accuracy.
          ! coefficients for the new expressions were determined at USACERL
          ! using data from the cited references.

          ! REFERENCES:
          ! J. L. Threlkeld, "Thermal Environmental Engineering", 1970,
          ! p.316, for declination and equation of time.
          ! "ASHRAE Handbook of Fundamentals", 1972, p.387 for sky
          ! coefficients a, b, and c.
          ! See SUN3 in SolarShading. See SUN2 in BLAST.  See SUN3 in Tarp.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DayOfYear              ! Day of year (1 - 366)
  REAL(r64), INTENT(OUT)   :: A                      ! ASHRAE "A" - Apparent solar irradiation at air mass = 0 [W/M**2]
  REAL(r64), INTENT(OUT)   :: B                      ! ASHRAE "B" - Atmospheric extinction coefficient
  REAL(r64), INTENT(OUT)   :: C                      ! ASHRAE "C" - Diffuse radiation factor
  REAL(r64), INTENT(OUT)   :: AnnVarSolConstant      ! Annual variation in the solar constant
  REAL(r64), INTENT(OUT)   :: EquationOfTime         ! Equation of Time
  REAL(r64), INTENT(OUT)   :: SineSolarDeclination   ! Sine of Solar Declination
  REAL(r64), INTENT(OUT)   :: CosineSolarDeclination ! Cosine of Solar Declination

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: DayCorrection=PI*2.d0/366.d0
  REAL(r64), PARAMETER, DIMENSION(9) :: SineSolDeclCoef = &  !Fitted coefficients of Fourier series
        (/ .00561800d0, .0657911d0, -.392779d0,   .00064440d0,-.00618495d0, & ! Sine of declination coefficients
          -.00010101d0,-.00007951d0,-.00011691d0, .00002096d0 /)
  REAL(r64), PARAMETER, DIMENSION(9) :: EqOfTimeCoef = &  !Fitted coefficients of Fourier Series
        (/ .00021971d0,-.122649d0,   .00762856d0,-.156308d0,  -.0530028d0,  & ! Equation of Time coefficients
          -.00388702d0,-.00123978d0,-.00270502d0,-.00167992d0 /)
  REAL(r64), PARAMETER, DIMENSION(9) :: ASHRAE_A_Coef = &  !Fitted coefficients of Fourier Series
        (/  1161.6685d0, 1.1554d0, 77.3575d0, -0.5359d0, -3.7622d0,         & ! ASHRAE A Factor coefficients
            0.9875d0, -3.3924d0, -1.7445d0, 1.1198d0 /)
! English (original) units:
!              368.49341,.366502,24.538624,-.169983,-1.193417,            &
!              .313261,-1.076093,-.543376,.355197 ,                       &

  REAL(r64), PARAMETER, DIMENSION(9) :: ASHRAE_B_Coef = &  !Fitted coefficients of Fourier Series
        (/ .171631d0,-.00400448d0,-.0344923d0,.00000209d0,.00325428d0,         & ! ASHRAE B Factor coefficients
          -.00085429d0,.00229562d0,.0009034d0,-.0011867d0 /)
  REAL(r64), PARAMETER, DIMENSION(9) :: ASHRAE_C_Coef = &  !Fitted coefficients of Fourier Series
        (/ .0905151d0,-.00322522d0,-.0407966d0,.000104164d0,.00745899d0,        & ! ASHRAE C Factor coefficients
          -.00086461d0,.0013111d0,.000808275d0,-.00170515d0 /)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    X     ! Day of Year in Radians (Computed from Input DayOfYear)
  REAL(r64)    CosX  ! COS(X)
  REAL(r64)    SinX ! SIN(X)


  X=DayCorrection*DayOfYear   ! Convert Julian date (Day of Year) to angle X

          ! Calculate sines and cosines of X
  SinX = SIN(X)
  CosX = COS(X)

  SineSolarDeclination = SineSolDeclCoef(1) + &
                           SineSolDeclCoef(2)*SinX +  &
                           SineSolDeclCoef(3)*CosX + &
                           SineSolDeclCoef(4)*(SinX*CosX*2.0d0) + &
                           SineSolDeclCoef(5)*(CosX**2 - SinX**2) + &
                           SineSolDeclCoef(6)*(SinX*(CosX**2 - SinX**2) + CosX*(SinX*CosX*2.0d0)) + &
                           SineSolDeclCoef(7)*(CosX*(CosX**2 - SinX**2) - SinX*(SinX*CosX*2.0d0)) + &
                           SineSolDeclCoef(8)*(2.0d0*(SinX*CosX*2.0d0)*(CosX**2 - SinX**2)) + &
                           SineSolDeclCoef(9)*((CosX**2 - SinX**2)**2 - (SinX*CosX*2.0d0)**2)
  CosineSolarDeclination=SQRT(1.0-SineSolarDeclination**2)

  EquationOfTime = EqOfTimeCoef(1) + &
                   EqOfTimeCoef(2)*SinX +  &
                   EqOfTimeCoef(3)*CosX + &
                   EqOfTimeCoef(4)*(SinX*CosX*2.0d0) + &
                   EqOfTimeCoef(5)*(CosX**2 - SinX**2) + &
                   EqOfTimeCoef(6)*(SinX*(CosX**2 - SinX**2) + CosX*(SinX*CosX*2.0d0)) + &
                   EqOfTimeCoef(7)*(CosX*(CosX**2 - SinX**2) - SinX*(SinX*CosX*2.0d0)) + &
                   EqOfTimeCoef(8)*(2.0d0*(SinX*CosX*2.0d0)*(CosX**2 - SinX**2)) + &
                   EqOfTimeCoef(9)*((CosX**2 - SinX**2)**2 - (SinX*CosX*2.0d0)**2)

  AnnVarSolConstant=1.000047d0 + .000352615d0*SinX + .0334454d0*CosX

  A = ASHRAE_A_Coef(1) + &
                   ASHRAE_A_Coef(2)*SinX +  &
                   ASHRAE_A_Coef(3)*CosX + &
                   ASHRAE_A_Coef(4)*(SinX*CosX*2.0d0) + &
                   ASHRAE_A_Coef(5)*(CosX**2 - SinX**2) + &
                   ASHRAE_A_Coef(6)*(SinX*(CosX**2 - SinX**2) + CosX*(SinX*CosX*2.0d0)) + &
                   ASHRAE_A_Coef(7)*(CosX*(CosX**2 - SinX**2) - SinX*(SinX*CosX*2.0d0)) + &
                   ASHRAE_A_Coef(8)*(2.0d0*(SinX*CosX*2.0d0)*(CosX**2 - SinX**2)) + &
                   ASHRAE_A_Coef(9)*((CosX**2 - SinX**2)**2 - (SinX*CosX*2.0d0)**2)

!                        Compute B and C coefficients

  IF (Latitude < 0.0d0) THEN
!                            If in southern hemisphere, compute B and C with a six month time shift.
    X = X - PI
    SinX = SIN(X)
    CosX = COS(X)
  ENDIF

  B = ASHRAE_B_Coef(1) + &
                   ASHRAE_B_Coef(2)*SinX +  &
                   ASHRAE_B_Coef(3)*CosX + &
                   ASHRAE_B_Coef(4)*(SinX*CosX*2.0d0) + &
                   ASHRAE_B_Coef(5)*(CosX**2 - SinX**2) + &
                   ASHRAE_B_Coef(6)*(SinX*(CosX**2 - SinX**2) + CosX*(SinX*CosX*2.0d0)) + &
                   ASHRAE_B_Coef(7)*(CosX*(CosX**2 - SinX**2) - SinX*(SinX*CosX*2.0d0)) + &
                   ASHRAE_B_Coef(8)*(2.0d0*(SinX*CosX*2.0d0)*(CosX**2 - SinX**2)) + &
                   ASHRAE_B_Coef(9)*((CosX**2 - SinX**2)**2 - (SinX*CosX*2.0d0)**2)

  C = ASHRAE_C_Coef(1) + &
                   ASHRAE_C_Coef(2)*SinX +  &
                   ASHRAE_C_Coef(3)*CosX + &
                   ASHRAE_C_Coef(4)*(SinX*CosX*2.0d0) + &
                   ASHRAE_C_Coef(5)*(CosX**2 - SinX**2) + &
                   ASHRAE_C_Coef(6)*(SinX*(CosX**2 - SinX**2) + CosX*(SinX*CosX*2.0d0)) + &
                   ASHRAE_C_Coef(7)*(CosX*(CosX**2 - SinX**2) - SinX*(SinX*CosX*2.0d0)) + &
                   ASHRAE_C_Coef(8)*(2.0d0*(SinX*CosX*2.0d0)*(CosX**2 - SinX**2)) + &
                   ASHRAE_C_Coef(9)*((CosX**2 - SinX**2)**2 - (SinX*CosX*2.0d0)**2)

      RETURN

END SUBROUTINE CalculateDailySolarCoeffs

SUBROUTINE CalculateSunDirectionCosines(TimeValue,EqOfTime,SinSolDeclin,CosSolDeclin,SUNCOS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   May 1975
          !       MODIFIED       1999 for EnergyPlus
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine computes the solar direction cosines for hourly
          ! radiation calculations.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! "NECAP Engineering Manual", 1974, p.3-117

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)  :: TimeValue       ! Current Time of Day
  REAL(r64), INTENT(IN)  :: EqOfTime        ! Equation of Time
  REAL(r64), INTENT(IN)  :: SinSolDeclin    ! Sine of Solar Declination
  REAL(r64), INTENT(IN)  :: CosSolDeclin    ! Cosine of Solar Declination
  REAL(r64), INTENT(OUT) :: SUNCOS(3)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         REAL(r64)  COSH  ! Cosine of hour angle
         REAL(r64)  H     ! Hour angle (before noon = +)

!                                      COMPUTE THE HOUR ANGLE
      H=(15.d0*(12.d0-(TimeValue+EqOfTime))+(TimeZoneMeridian-Longitude))*DegToRadians
      COSH=COS(H)
!                                      COMPUTE THE COSINE OF THE
!                                      SOLAR ZENITH ANGLE.
!                                      This is also the Sine of the Solar Altitude Angle

      SUNCOS(3)=SinSolDeclin*SinLatitude+CosSolDeclin*CosLatitude*COSH

      IF(SUNCOS(3) >= SunIsUpValue) THEN     ! If Sun above horizon, compute other direction cosines
        SUNCOS(2)=SinSolDeclin*CosLatitude-CosSolDeclin*SinLatitude*COSH
        SUNCOS(1)=CosSolDeclin*SIN(H)
      ELSE                             ! Sun is down, set to 0.0
        SUNCOS(1)=0.0d0
        SUNCOS(2)=0.0d0
      ENDIF

  RETURN

END SUBROUTINE CalculateSunDirectionCosines

SUBROUTINE DetermineSunUpDown(SunDirectionCosines)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines if the sun is up or down for the current
          ! hour/timestep.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Sun routines from IBLAST, authored by Walton.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(OUT), DIMENSION(3) :: SunDirectionCosines

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) H       ! Hour angle (before noon = +)
  REAL(r64) SinAltitude,SolarAltitude,SolarAzimuth,SolarZenith
  REAL(r64) CosAzimuth,CosZenith
!  REAL(r64) HAngle

          ! COMPUTE THE HOUR ANGLE

  IF (NumOfTimeStepInHour /= 1) THEN
    HrAngle = (15.d0*(12.d0-(CurrentTime+TodayVariables%EquationOfTime))+(TimeZoneMeridian-Longitude))
  ELSE
    HrAngle = (15.d0*(12.d0-((CurrentTime+TS1TimeOffset)+TodayVariables%EquationOfTime))+(TimeZoneMeridian-Longitude))
  ENDIF
  H=HrAngle*DegToRadians

          ! Compute the Cosine of the Solar Zenith (Altitude) Angle.
  CosZenith=SinLatitude*TodayVariables%SinSolarDeclinAngle+CosLatitude*TodayVariables%CosSolarDeclinAngle*COS(H)

  SolarZenith=ACOS(CosZenith)
  SinAltitude=CosLatitude*TodayVariables%CosSolarDeclinAngle*COS(H)+SinLatitude*TodayVariables%SinSolarDeclinAngle
  SolarAltitude=ASIN(SinAltitude)
  CosAzimuth=-(SinLatitude*CosZenith-TodayVariables%SinSolarDeclinAngle)/(CosLatitude*SIN(SolarZenith))
  ! Following because above can yield invalid cos value.  (e.g. at south pole)
  CosAzimuth=MAX(CosAzimuth,-1.0d0)
  CosAzimuth=MIN(1.0d0,CosAzimuth)
  SolarAzimuth=ACOS(CosAzimuth)

  SolarAltitudeAngle=SolarAltitude/DegToRadians
  SolarAzimuthAngle=SolarAzimuth/DegToRadians
  IF (HrAngle < 0.0d0) THEN
    SolarAzimuthAngle=360.d0-SolarAzimuthAngle
  ENDIF

  SunDirectionCosines(3) = CosZenith
  IF (CosZenith < SunIsUpValue) THEN
    SunIsUp=.false.
    SunDirectionCosines(2)=0.0d0
    SunDirectionCosines(1)=0.0d0
  ELSE
    SunIsUp=.true.
    SunDirectionCosines(2) = TodayVariables%SinSolarDeclinAngle*CosLatitude   &
                             - TodayVariables%CosSolarDeclinAngle*SinLatitude*COS(H)
    SunDirectionCosines(1) = TodayVariables%CosSolarDeclinAngle*SIN(H)
  ENDIF

  RETURN

END SUBROUTINE DetermineSunUpDown


SUBROUTINE OpenWeatherFile(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 1999
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks to see if a weather file and what kind of weather file
          ! exists in the working directory and calls appropriate routines to
          ! open the files and set up for use.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:


  INQUIRE(FILE='in.epw',EXIST=WeatherFileExists)

  IF (WeatherFileExists) THEN
    CALL OpenEPlusWeatherFile(ErrorsFound,.true.)
  ENDIF

  RETURN

END SUBROUTINE OpenWeatherFile

SUBROUTINE OpenEPlusWeatherFile(ErrorsFound,ProcessHeader)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   June 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine opens the EnergyPlus Weather File (in.epw) and processes
          ! the initial header records.

          ! METHODOLOGY EMPLOYED:
          ! List directed reads, as possible.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound     ! Will be set to true if errors found
  LOGICAL, INTENT(IN)    :: ProcessHeader   ! Set to true when headers should be processed (rather than just read)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: AFormat="(A)"
  CHARACTER(len=*), PARAMETER, DIMENSION(8) :: Header=(/"LOCATION                ","DESIGN CONDITIONS       ",  &
                                                        "TYPICAL/EXTREME PERIODS ","GROUND TEMPERATURES     ",  &
                                                        "HOLIDAYS/DAYLIGHT SAVING","COMMENTS 1              ",  &
                                                        "COMMENTS 2              ","DATA PERIODS            "/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER GetNewUnitNumber
  INTEGER Pos
  CHARACTER(len=800) Line
  integer HdPos,HdLine
  logical StillLooking
  integer endcol
  integer, external :: FindNonSpace
  LOGICAL EPWOpen
  integer :: unitnumber

  INQUIRE(FILE='in.epw',NUMBER=unitnumber,OPENED=EPWOpen)
  IF (EPWOpen) CLOSE(unitnumber)

  WeatherFileUnitNumber=GetNewUnitNumber()
  OPEN(WeatherFileUnitNumber,File='in.epw',ERR=9999,action='read')

  IF (ProcessHeader) THEN
  ! Read in Header Information

  ! Headers should come in order
    HdLine=1   ! Look for first Header
    StillLooking=.true.
    DO WHILE (StillLooking)
      READ(WeatherFileUnitNumber,AFormat,END=9998) line
      endcol=LEN_TRIM(line)
      IF (endcol > 0) THEN
        IF (ICHAR(line(endcol:endcol)) == iASCII_CR) THEN
          StripCR=.true.
          line(endcol:endcol)=Blank
        ENDIF
        IF (ICHAR(line(endcol:endcol)) == iUnicode_end) THEN
          GOTO 9997
        ENDIF
      ENDIF
      Pos=FindNonSpace(line)
      HdPos=INDEX(line,TRIM(Header(HdLine)))
      IF (Pos /= HdPos) CYCLE
!      line=MakeUPPERCase(line)
      CALL ProcessEPWHeader(Header(HdLine),line,ErrorsFound)
      HdLine=HdLine+1
      IF (HdLine == 9) StillLooking=.false.
    ENDDO
  ELSE  ! Header already processed, just read
    CALL SkipEPLusWFHeader
  ENDIF

  RETURN

  9997 CALL ShowSevereError('OpenWeatherFile: EPW Weather File appears to be a Unicode or binary file.',OutputFileStandard)
       CALL ShowContinueError('...This file cannot be read by this program. Please save as PC or Unix file and try again')
       CALL ShowFatalError('Program terminates due to previous condition.')

  9998 CALL ShowFatalError('OpenWeatherFile: Unexpected End-of-File on EPW Weather file, '//  &
                           'while reading header information, looking for header='//          &
                            TRIM(Header(HdLine)),OutputFileStandard)

  9999 CALL ShowFatalError('OpenWeatherFile: Could not OPEN EPW Weather File',OutputFileStandard)

  RETURN

END SUBROUTINE OpenEPlusWeatherFile

SUBROUTINE CloseWeatherFile

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine closes the open weather file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  LOGICAL EPWOpen
  integer :: unitnumber

  !  Make sure it's open

  INQUIRE(FILE='in.epw',NUMBER=unitnumber,OPENED=EPWOpen)
  IF (EPWOpen) CLOSE(unitnumber)

  RETURN

END SUBROUTINE CloseWeatherFile

SUBROUTINE ResolveLocationInformation(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is currently the main interface between the old data
          ! structure on the BLAST Weather file and the new data structure contained
          ! in this module.  At some point, this subroutine will be converted
          ! to read information directly from the new input file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound   ! Set to true if no location evident

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: LocHdFormat="('! <Site:Location>, Location Name, Latitude {N+/S- Deg}, Longitude {E+/W- Deg}, ',  &
                                                & ' Time Zone Number {GMT+/-}, Elevation {m}, ',  &
                                                & ' Standard Pressure at Elevation {Pa}, Standard RhoAir at Elevation')"
  CHARACTER(len=*), PARAMETER :: LocFormat="('Site:Location',7(',',A))"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

  IF (Environment(NumOfEnvrn)%KindOfEnvrn == ksRunPeriodWeather .and. WeatherFileExists) THEN
    IF (LocationGathered) THEN
      ! See if "matching" location
      IF (ABS(Latitude-WeatherFileLatitude)    > 1.d0   .or.  &
          ABS(Longitude-WeatherFileLongitude)    > 1.d0 .or.  &
          ABS(TimeZoneNumber-WeatherFileTimeZone) > 0.0d0 .or.  &
          ABS(Elevation-WeatherFileElevation)/Max(Elevation,1.d0) > .10d0) THEN
        CALL ShowWarningError('Weather file location will be used rather than entered (IDF) Location object.')
        CALL ShowContinueError('..Location object='//TRIM(LocationTitle))
        CALL ShowContinueError('..Weather File Location='//TRIM(WeatherFileLocationTitle))
        CALL ShowContinueError('..due to location differences, Latitude difference=['//  &
           trim(RoundSigDigits(ABS(Latitude-WeatherFileLatitude),2))//'] degrees, Longitude difference=['//   &
           trim(RoundSigDigits(ABS(Longitude-WeatherFileLongitude),2))//'] degrees.')
        CALL ShowContinueError('..Time Zone difference=['//  &
           trim(RoundSigDigits(ABS(TimeZoneNumber-WeatherFileTimeZone),1))//'] hour(s), Elevation difference=['//   &
           trim(RoundSigDigits(ABS((Elevation-WeatherFileElevation)/Max(Elevation,1.d0))*100.0,2))//'] percent,'//  &
              ' ['//trim(RoundSigDigits(ABS(Elevation-WeatherFileElevation),2))//'] meters.')
      ENDIF
    ENDIF

    LocationTitle=WeatherFileLocationTitle
    Latitude = WeatherFileLatitude
    Longitude = WeatherFileLongitude
    TimeZoneNumber = WeatherFileTimeZone
    Elevation=WeatherFileElevation
  ELSEIF (.not. LocationGathered) THEN
    LocationTitle='Not Entered'
    CALL ShowSevereError('No Location given. Must have location information for simulation.')
    ErrorsFound=.true.
  END IF

  IF (.not. ErrorsFound) THEN
    StdBaroPress=101.325d0*(1.d0-2.25577d-05*Elevation)**5.2559d0
    StdBaroPress=StdBaroPress*1000.d0
    StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,constant_twenty,constant_zero)
    ! Write Final Location Information to the initialization output file
    Write(OutputFileInits,LocHdFormat)
    Write(OutputFileInits,LocFormat) Trim(LocationTitle),TRIM(RoundSigDigits(Latitude,2)),  &
                                     TRIM(RoundSigDigits(Longitude,2)),  &
                                     TRIM(RoundSigDigits(TimeZoneNumber,2)),  &
                                     TRIM(RoundSigDigits(Elevation,2)),  &
                                     TRIM(RoundSigDigits(StdBaroPress,0)),  &
                                     TRIM(RoundSigDigits(StdRhoAir,4))
  ENDIF

  RETURN

END SUBROUTINE ResolveLocationInformation

SUBROUTINE CheckLocationValidity

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is checks to see whether the user specified location
          ! or the weather file location (if one exists) is valid.  The standard
          ! time meridian is also calculated and compared to the user supplied
          ! or weather file time zone number.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Legacy subroutine CKBLDE.

          ! USE STATEMENTS:
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

  LOGICAL :: LocationError  ! Set to true if there is a problem detected
  REAL(r64)    :: StdTimeMerid   ! Standard time meridian
  REAL(r64)    :: Diffcalc       ! Difference between Standard Time Meridian and TimeZone

          ! FLOW:

  LocationError = .FALSE.

  IF ( (Latitude .EQ. -999.d0) .AND. (Longitude .EQ. -999.d0) &
                             .AND. (TimeZoneNumber .NE. -999.d0) ) THEN
    CALL ShowSevereError('No location specified')
    LocationError=.TRUE.
  END IF

  IF ( (Latitude .LT. -90.d0) .OR. (Latitude .GT. 90.d0) ) THEN
    CALL ShowSevereError('Latitude must be between -90 and 90; Entered='//TRIM(RoundSigDigits(Latitude,2)))
    LocationError=.TRUE.
  END IF

  IF ( (Longitude .LT. -180.d0) .OR. (Longitude .GT. 180.d0) ) THEN
    CALL ShowSevereError('Longitude must be between -180 and 180; Entered='//TRIM(RoundSigDigits(Longitude,2)))
    LocationError=.TRUE.
  END IF

  IF ( (TimeZoneNumber < -12.00d0) .OR. (TimeZoneNumber > 14.00d0) ) THEN
    CALL ShowSevereError('Time Zone must be between -12 and +14; Entered='//TRIM(RoundSigDigits(TimeZoneNumber,2)))
    LocationError=.TRUE.
  END IF

  StdTimeMerid=GetSTM(Longitude)    ! Obtain the standard time meridian.

  ! Bias at +/- 12 for StdTimeMerid
!  IF (StdTimeMerid == -12.0 .and. TimeZoneNumber > 0) THEN
!    StdTimeMerid=12.0
!  ELSEIF (StdTimeMerid == 12.0 .and. TimeZoneNumber < 0) THEN
!    StdTimeMerid=-12.0
!  ENDIF

          ! Compare the standard time meridian with the time zone number.  If
          ! different, notify the user.  If StdTimeMerid couldn't be calculated,
          ! produce an error message.

  IF (StdTimeMerid >= -12.0d0 .and. StdTimeMerid <= 12.0d0) THEN
    IF (TimeZoneNumber .NE. StdTimeMerid) THEN
      DiffCalc=ABS(TimeZoneNumber-StdTimeMerid)
      IF (DiffCalc > 1.d0 .and. DiffCalc < 24.d0) THEN
        IF (DiffCalc < 3.d0) THEN
          CALL ShowWarningError('Standard Time Meridian and Time Zone differ by more than 1, '// &
             'Difference="'//TRIM(RoundSigDigits(DiffCalc,1))//'"')
          CALL ShowContinueError('Solar Positions may be incorrect')
        ELSE
          CALL ShowSevereError('Standard Time Meridian and Time Zone differ by more than 2, '// &
             'Difference="'//TRIM(RoundSigDigits(DiffCalc,1))//'"')
          CALL ShowContinueError('Solar Positions will be incorrect')
!          LocationError=.true.
        ENDIF
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError('Unable to calculate the standard time meridian')
    LocationError=.TRUE.
  ENDIF

          ! Error handling:  if there are any errors in the location information
          ! the simulation must be terminated

  IF (LocationError) THEN
    CALL ShowFatalError('Due to previous error condition, simulation terminated')
  ENDIF

  IF (TimeZoneNumber <= 12.00d0) THEN
    TimeZoneMeridian=TimeZoneNumber*15.d0
  ELSE
    TimeZoneMeridian=TimeZoneNumber*15.d0-360.d0
  ENDIF
  SinLatitude=SIN(DegToRadians*Latitude)
  CosLatitude=COS(DegToRadians*Latitude)

  IF (Latitude == 0.0d0 .and. Longitude == 0.0d0 .and. TimeZoneNumber == 0.0d0) THEN
    CALL ShowWarningError('Did you realize that you have Latitude=0.0, Longitude=0.0 and TimeZone=0.0?'//  &
                          '  Your building site is in the middle of the Atlantic Ocean.')
  ENDIF

  RETURN

END SUBROUTINE CheckLocationValidity

SUBROUTINE CheckWeatherFileValidity

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 1977
          !       MODIFIED       June 1997 (RKS)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine contains a portion of the legacy subroutine CKBLDE.
          ! The main purpose of this routine is to check the validity of the
          ! weather dates provided by the user and the attached weather file.
          ! These functions may eventually be pushed to an interface.  This
          ! routine also sends the weather file header information at the
          ! Environment derived type.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Legacy subroutine CKBLDE.

          ! USE STATEMENTS:
  USE General, ONLY: JulianDay

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
          ! na

          ! FLOW:

  ErrorInWeatherFile=.FALSE.
  IF (.not. WeatherFileExists) THEN ! No weather file exists but the user requested one--print error message

    IF (DoWeathSim) THEN
      CALL ShowWarningError('Weather Environment(s) requested, but no weather file found')
      ErrorInWeatherFile=.TRUE.
    ENDIF

  END IF  ! ... end of WeatherFileExists IF-THEN

  RETURN

END SUBROUTINE CheckWeatherFileValidity

SUBROUTINE ReportOutputFileHeaders

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine prints out the necessary header information required
          ! by the EnergyPlus output file format.  This subroutine can be
          ! replicated in any other modules which must send data to the output
          ! file.

          ! METHODOLOGY EMPLOYED:
          ! For each report, the report flag integer must be saved from the
          ! global report number counter.  Then, the report counter must be
          ! incremented.  Finally, the header information for the report must
          ! be sent to the output file.

          ! REFERENCES:
          ! EnergyPlus Output Description document.

          ! USE STATEMENTS:
  USE OutputProcessor, ONLY: TimeStepStampReportNbr,DailyStampReportNbr,MonthlyStampReportNbr,RunPeriodStampReportNbr,  &
                             TimeStepStampReportChr,DailyStampReportChr,MonthlyStampReportChr,RunPeriodStampReportChr

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: IntFmt="(I3)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

                                       ! Format descriptor for the environment title
  CHARACTER(len=*), PARAMETER :: EnvironmentFormat = "(a,',5,Environment Title[],Latitude[deg],"//  &
                                 "Longitude[deg],Time Zone[],Elevation[m]')"
  CHARACTER(len=*),  PARAMETER :: TimeStepFormat        = "(a,',6,Day of Simulation[],Month[],Day of Month[],"// &
                                 "DST Indicator[1=yes 0=no],Hour[],StartMinute[],EndMinute[],DayType')"
  CHARACTER(len=*),  PARAMETER :: DailyFormat        = "(a,',3,Cumulative Day of Simulation[],Month[],Day of Month[]," // &
                                 "DST Indicator[1=yes 0=no],DayType  ! When Daily ',A,' Requested')"
  CHARACTER(len=*),  PARAMETER :: MonthlyFormat        = "(a,',2,Cumulative Days of Simulation[],Month[]" // &
                                 "  ! When Monthly ',A,' Requested')"
  CHARACTER(len=*),  PARAMETER :: RunPeriodFormat        = "(a,',1,Cumulative Days of Simulation[]" // &
                                 " ! When Run Period ',A,' Requested')"

          ! FLOW:

  CALL AssignReportNumber(EnvironmentReportNbr)
  IF (EnvironmentReportNbr /= 1) THEN  !  problem
    CALL ShowFatalError('ReportOutputFileHeaders: Assigned report number for Environment title is not 1.  Contact Support.')
  ENDIF
  WRITE(EnvironmentReportChr,IntFmt) EnvironmentReportNbr
  EnvironmentReportChr=ADJUSTL(EnvironmentReportChr)
  WRITE (OutputFileStandard,EnvironmentFormat) TRIM(EnvironmentReportChr)
  WRITE (OutputFileMeters,EnvironmentFormat) TRIM(EnvironmentReportChr)

  CALL AssignReportNumber(TimeStepStampReportNbr)
  WRITE(TimeStepStampReportChr,IntFmt) TimeStepStampReportNbr
  TimeStepStampReportChr=ADJUSTL(TimeStepStampReportChr)
  WRITE (OutputFileStandard,TimeStepFormat) TRIM(TimeStepStampReportChr)
  WRITE (OutputFileMeters,TimeStepFormat) TRIM(TimeStepStampReportChr)

  CALL AssignReportNumber(DailyStampReportNbr)
  WRITE(DailyStampReportChr,IntFmt) DailyStampReportNbr
  DailyStampReportChr=ADJUSTL(DailyStampReportChr)
  WRITE (OutputFileStandard,DailyFormat) TRIM(DailyStampReportChr),'Report Variables'
  WRITE (OutputFileMeters,DailyFormat) TRIM(DailyStampReportChr),'Meters'

  CALL AssignReportNumber(MonthlyStampReportNbr)
  WRITE(MonthlyStampReportChr,IntFmt) MonthlyStampReportNbr
  MonthlyStampReportChr=ADJUSTL(MonthlyStampReportChr)
  WRITE (OutputFileStandard,MonthlyFormat) TRIM(MonthlyStampReportChr),'Report Variables'
  WRITE (OutputFileMeters,MonthlyFormat) TRIM(MonthlyStampReportChr),'Meters'

  CALL AssignReportNumber(RunPeriodStampReportNbr)
  WRITE(RunPeriodStampReportChr,IntFmt) RunPeriodStampReportNbr
  RunPeriodStampReportChr=ADJUSTL(RunPeriodStampReportChr)
  WRITE (OutputFileStandard,RunPeriodFormat) TRIM(RunPeriodStampReportChr),'Report Variables'
  WRITE (OutputFileMeters,RunPeriodFormat) TRIM(RunPeriodStampReportChr),'Meters'

  RETURN

END SUBROUTINE ReportOutputFileHeaders

SUBROUTINE ReportWeatherAndTimeInformation(PrintEnvrnStamp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver of the weather reporting.  This
          ! routine is also responsible for printing the time and environment
          ! stamps.

          ! METHODOLOGY EMPLOYED:
          ! Reporting is only done for non-warmup days.  The environment stamp
          ! is only reported at the beginning of an environment, but after the
          ! warmup days (to allow all modules to print the report headers to the
          ! output file.  This is controlled by the PrintEnvrnStamp variable
          ! which is passed in and reset if necessary.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!unused0909  USE DataSystemVariables, ONLY: ReportDuringWarmup

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)     :: PrintEnvrnStamp   ! Set to true when the environment header should be printed

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: EndOfHeaderFormat = "('End of Data Dictionary')"    ! End of data dictionary marker
  CHARACTER(len=*), PARAMETER :: EnvironmentStampFormat = "(a,',',a,3(',',f7.2),',',f7.2)" ! Format descriptor for environ stamp
!  CHARACTER(len=*), PARAMETER :: TimeStampFormat = "(i3,',',i4,',',i2,',',i2,',',i2)" ! Format descriptor for the date/time stamp

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=100) :: Title

          ! FLOW:

          ! Report the time stamp and the current weather to the output file

  Title = Environment(Envrn)%Title
  IF(.NOT.WarmupFlag) THEN  ! Write the required output information

          ! The first time through in a non-warmup day, the environment header
          ! must be printed.  This must be done here and not in the generic
          ! BeginEnvrnFlag block above because other modules in the simulation
          ! must also print out header information.  This can be done during
          ! the simulation warmup if the environment stamp printing is delayed
          ! until the warmup is completed.  The stamp should only be printed once
          ! per environment (set/reset of PrintEnvrnStamp).  In addition, before
          ! the first environment, the end of the header block flag must also be
          ! sent to the output file.

    IF (PrintEnvrnStamp) THEN

      IF (PrintEndDataDictionary .AND. DoOutputReporting) THEN
        WRITE (OutputFileStandard,EndOfHeaderFormat)
        WRITE (OutputFileMeters,EndOfHeaderFormat)
        PrintEndDataDictionary = .FALSE.
      ENDIF
      IF (DoOutputReporting) THEN
        WRITE (OutputFileStandard,EnvironmentStampFormat) TRIM(EnvironmentReportChr), &
                  Trim(Title),Latitude,Longitude,TimeZoneNumber,Elevation
        WRITE (OutputFileMeters,EnvironmentStampFormat) TRIM(EnvironmentReportChr), &
                  Trim(Title),Latitude,Longitude,TimeZoneNumber,Elevation
        PrintEnvrnStamp=.FALSE.
      END IF

    END IF
  END IF    ! ... end of .NOT.WarmupFlag IF-THEN block.

  RETURN

END SUBROUTINE ReportWeatherAndTimeInformation


SUBROUTINE ReadUserWeatherInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   September 1997
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver of the weather manager module.
          ! It controls the assignment of weather related global variables as
          ! well as the reads and writes for retrieving weather information.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
  USE DataSystemVariables

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
   Integer :: Env           ! Environment Loop Counter
   LOGICAL :: ErrorsFound=.false.
   Integer :: RPD1
   Integer :: RPD2
   Integer :: RP    ! number of run periods
   Integer :: RPAW  ! number of run periods, actual weather


          ! FLOW:

      !Get the number of design days and annual runs from user inpout
      TotDesDays=GetNumObjectsFound('SizingPeriod:DesignDay')
      RPD1=GetNumObjectsFound('SizingPeriod:WeatherFileDays')
      RPD2=GetNumObjectsFound('SizingPeriod:WeatherFileConditionType')
      RP=GetNumObjectsFound('RunPeriod')
      RPAW=GetNumObjectsFound('RunPeriod:CustomRange')
      TotRunPers=RP+RPAW
      NumOfEnvrn=TotDesDays+TotRunPers+RPD1+RPD2
      IF (TotRunPers > 0) THEN
        WeathSimReq=.true.
      ELSE
        WeathSimReq=.false.
      ENDIF

      ALLOCATE(SPSiteScheduleNamePtr(TotDesDays*5))
      ALLOCATE(SPSiteScheduleUnits(TotDesDays*5))

      SPSiteScheduleNamePtr=0
      SPSiteScheduleUnits=Blank

      !Allocate the Design Day and Environment array to the # of DD's or/and
      ! Annual runs on input file
      ALLOCATE (DesignDay(TotDesDays))
      ALLOCATE (Environment(NumOfEnvrn))

      ! Set all Environments to False and then the weather environment will be set
      !  in the get annual run data subroutine
      Do Env = 1, TotDesDays
          Environment(Env)%KindOfEnvrn = ksDesignDay
      End Do
      Do Env = 1, RPD1+RPD2
          IF (.not. DDOnly) THEN
            Environment(TotDesDays+Env)%KindOfEnvrn = ksRunPeriodDesign
          ELSE
            Environment(TotDesDays+Env)%KindOfEnvrn = ksRunPeriodWeather
          ENDIF
      End Do
      Do Env = 1, TotRunPers
          Environment(TotDesDays+RPD1+RPD2+Env)%KindOfEnvrn = ksRunPeriodWeather
      End Do

      IF (TotDesDays >= 1) THEN
        CALL GetDesignDayData(TotDesDays,ErrorsFound)
      ENDIF

      IF (RPD1 >=1 .or. RPD2 >= 1) THEN
        CALL GetRunPeriodDesignData(ErrorsFound)
      ENDIF

      !the last environment(s) is designated the weather environment if an annual run
      ! is selected.  All of the design systems is done from the design day info
      ! which will have to be completed to run the annual run.
      IF (TotRunPers >= 1 .or. FullAnnualRun) THEN
        CALL GetRunPeriodData(TotRunPers,ErrorsFound)
      ENDIF

      IF (RPD1 >=1 .or. RPD2 >= 1 .or. TotRunPers >= 1 .or. FullAnnualRun) THEN
        CALL GetSpecialDayPeriodData(ErrorsFound)
        CALL GetDSTData(ErrorsFound)
        IF (IDFDaylightSaving) THEN
          DST=IDFDST
        ENDIF
      ENDIF

      CALL GetLocationInfo(ErrorsFound)

      CALL GetGroundTemps(ErrorsFound)

      CALL GetGroundReflectances(ErrorsFound)

      CALL GetSnowGroundRefModifiers(ErrorsFound)

      CALL GetWaterMainsTemperatures(ErrorsFound)

      CALL GetWeatherStation(ErrorsFound)

      CALL SetupEnvironmentTypes

      CALL GetWeatherProperties(ErrorsFound)

      ! Deallocate ones used for schedule pointers
      DEALLOCATE(SPSiteScheduleNamePtr)
      DEALLOCATE(SPSiteScheduleUnits)

     IF (ErrorsFound) THEN
       CALL ShowFatalError('GetWeatherInput: Above errors cause termination')
     ENDIF

  RETURN

END SUBROUTINE ReadUserWeatherInput

SUBROUTINE GetRunPeriodData(TotRunPers,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   October 1997
          !       MODIFIED       February 1999, Add multiple run periods, Change name.
          !                      March 2012, LKL, Add features to object; New "actual weather" object;
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the run period info from User input and the
          !  simulation dates

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, GetObjectItem, SameString, VerifyName, GetNumObjectsFound
  USE General, ONLY: JulianDay,TrimSigDigits
  USE DataSystemVariables
  USE DataIPShortCuts

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
 Integer :: TotRunPers   ! Total number of Run Periods requested
 LOGICAL, INTENT(INOUT) :: ErrorsFound

         ! SUBROUTINE PARAMETER DEFINITIONS:
         ! na

         ! INTERFACE BLOCK SPECIFICATIONS:
         ! na

         ! DERIVED TYPE DEFINITIONS:
         ! na

         ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlpha ! Number of alphas being input
  INTEGER :: NumNumeric  ! Number of numbers being input
  INTEGER :: IOStat           ! IO Status when calling get input subroutine
  INTEGER :: Loop
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
  INTEGER :: Count
  TYPE (EnvironmentData), ALLOCATABLE, DIMENSION(:) :: TempEnvironment ! Environment data
  Integer :: RP    ! number of run periods
  Integer :: RPAW  ! number of run periods, actual weather
  Integer :: Ptr
  Integer :: LocalLeapYearAdd

         ! FLOW:
  RP=GetNumObjectsFound('RunPeriod')
  RPAW=GetNumObjectsFound('RunPeriod:CustomRange')

   !Call Input Get routine to retrieve annual run data
  ALLOCATE (RunPeriodInput(TotRunPers))

  cCurrentModuleObject='RunPeriod'
  count=0
  IF(.not. WFAllowsLeapYears) THEN
    LocalLeapYearAdd=0
  ELSE
    LocalLeapYearAdd=1
  ENDIF
  DO Loop=1,RP
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumeric,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IF (.not. lAlphaFieldBlanks(1)) THEN
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),RunPeriodInput%Title,Count,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
    ENDIF

    Count=Count+1
    RunPeriodInput(Loop)%Title=cAlphaArgs(1)

   !set the start and end day of month from user input
   ! N1 , \field Begin Month
   ! N2 , \field Begin Day of Month
   ! N3 , \field End Month
   ! N4 , \field End Day of Month
    RunPeriodInput(Loop)%StartMonth = Int(rNumericArgs(1))
    RunPeriodInput(Loop)%StartDay = Int(rNumericArgs(2))
    RunPeriodInput(Loop)%EndMonth   = Int(rNumericArgs(3))
    RunPeriodInput(Loop)%EndDay   = Int(rNumericArgs(4))

    !  N5,  \field Number of Times Runperiod to be Repeated
    IF (INT(rNumericArgs(5))==0) THEN
      RunPeriodInput(Loop)%NumSimYears = 1
    ELSE
      RunPeriodInput(Loop)%NumSimYears = INT(rNumericArgs(5))
    ENDIF

    !  N6;  \field Start Year
    IF (INT(rNumericArgs(6))==0) THEN
      RunPeriodInput(Loop)%BeginYear = autocalculate
      RunPeriodInput(Loop)%TreatYearsAsConsecutive = .false.
    ELSE
      RunPeriodInput(Loop)%BeginYear = INT(rNumericArgs(6))
      RunPeriodInput(Loop)%TreatYearsAsConsecutive = .true.
    ENDIF

    IF (FullAnnualRun .and. Loop == 1) THEN
      RunPeriodInput(Loop)%StartMonth = 1
      RunPeriodInput(Loop)%StartDay = 1
      RunPeriodInput(Loop)%EndMonth   = 12
      RunPeriodInput(Loop)%EndDay   = 31
    ENDIF


    SELECT CASE (RunPeriodInput(Loop)%StartMonth)

      CASE (1,3,5,7,8,10,12)
        IF (RunPeriodInput(Loop)%StartDay > 31) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(1))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (4,6,9,11)
        IF (RunPeriodInput(Loop)%StartDay > 30) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(1))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (2)
        IF (RunPeriodInput(Loop)%StartDay > 28+LocalLeapYearAdd) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(1))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//  &
             TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//']')
        ErrorsFound=.true.
    END SELECT

    SELECT CASE (RunPeriodInput(Loop)%EndMonth)

      CASE (1,3,5,7,8,10,12)
        IF (RunPeriodInput(Loop)%EndDay > 31) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(4))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(3))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (4,6,9,11)
        IF (RunPeriodInput(Loop)%EndDay > 30) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(4))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(3))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (2)
        IF (RunPeriodInput(Loop)%EndDay > 28+LocalLeapYearAdd) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(4))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(3))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
             TRIM(cNumericFieldNames(3))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//']')
        ErrorsFound=.true.
    END SELECT

    ! A2 , \field Day of Week for Start Day
    IF (lAlphaFieldBlanks(2) .or. cAlphaArgs(2) == 'USEWEATHERFILE') THEN
      RunPeriodInput(Loop)%DayOfWeek=0 ! Defaults to Day of Week from Weather File
    ELSE
      RunPeriodInput(Loop)%DayOfWeek=FindItemInList(cAlphaArgs(2),DaysOfWeek,7)
      IF (RunPeriodInput(Loop)%DayOfWeek == 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(2))//' invalid (Day of Week) ['//  &
            TRIM(cAlphaArgs(2))//' for Start is not Valid, DayofWeek from WeatherFile will be used.')
      ENDIF
    ENDIF

    ! A3,  \field Use Weather File Holidays and Special Days
    IF (lAlphaFieldBlanks(3) .or. SameString(cAlphaArgs(3),'YES')) THEN
      RunPeriodInput(Loop)%UseHolidays=.true.
    ELSEIF (SameString(cAlphaArgs(3),'NO')) THEN
      RunPeriodInput(Loop)%UseHolidays=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(3))//' invalid ['//TRIM(cAlphaArgs(3))//']')
      ErrorsFound=.true.
    ENDIF

    ! A4,  \field Use Weather File Daylight Saving Period
    IF (lAlphaFieldBlanks(4) .or. SameString(cAlphaArgs(4),'YES')) THEN
      RunPeriodInput(Loop)%UseDST=.true.
    ELSEIF (SameString(cAlphaArgs(4),'NO')) THEN
      RunPeriodInput(Loop)%UseDST=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(4))//' invalid ['//TRIM(cAlphaArgs(4))//']')
      ErrorsFound=.true.
    ENDIF

    ! A5,  \field Apply Weekend Holiday Rule
    IF (lAlphaFieldBlanks(5) .or. SameString(cAlphaArgs(5),'YES')) THEN
      RunPeriodInput(Loop)%ApplyWeekendRule=.true.
    ELSEIF (SameString(cAlphaArgs(5),'NO')) THEN
      RunPeriodInput(Loop)%ApplyWeekendRule=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(5))//' invalid ['//TRIM(cAlphaArgs(5))//']')
      ErrorsFound=.true.
    ENDIF

    ! A6,  \field Use Weather File Rain Indicators
    IF (lAlphaFieldBlanks(6) .or. SameString(cAlphaArgs(6),'YES')) THEN
      RunPeriodInput(Loop)%UseRain=.true.
    ELSEIF (SameString(cAlphaArgs(6),'NO')) THEN
      RunPeriodInput(Loop)%UseRain=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(6))//' invalid ['//TRIM(cAlphaArgs(6))//']')
      ErrorsFound=.true.
    ENDIF

    ! A7,  \field Use Weather File Snow Indicators
    IF (lAlphaFieldBlanks(7) .or. SameString(cAlphaArgs(7),'YES')) THEN
      RunPeriodInput(Loop)%UseSnow=.true.
    ELSEIF (SameString(cAlphaArgs(7),'NO')) THEN
      RunPeriodInput(Loop)%UseSnow=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(7))//' invalid ['//TRIM(cAlphaArgs(7))//']')
      ErrorsFound=.true.
    ENDIF

   ! A8,  \field Increment Day of Week on repeat
   IF (lAlphaFieldBlanks(8) .or. SameString(cAlphaArgs(8),'YES')) THEN
      RunPeriodInput(Loop)%RollDayTypeOnRepeat=.true.
    ELSEIF (SameString(cAlphaArgs(8),'NO')) THEN
      RunPeriodInput(Loop)%RollDayTypeOnRepeat=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//' '//  &
            TRIM(cAlphaFieldNames(8))//' invalid ['//TRIM(cAlphaArgs(8))//']')
      ErrorsFound=.true.
    ENDIF

    !calculate the annual start and end dates from the user inputted month and day
    RunPeriodInput(Loop)%StartDate = JulianDay(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,LeapYearAdd)
    RunPeriodInput(Loop)%EndDate = JulianDay(RunPeriodInput(Loop)%EndMonth,RunPeriodInput(Loop)%EndDay,LeapYearAdd)
    RunPeriodInput(Loop)%MonWeekDay=0
    IF (RunPeriodInput(Loop)%DayOfWeek /= 0 .and. .not. ErrorsFound) THEN
      CALL SetupWeekDaysByMonth(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,  &
                                RunPeriodInput(Loop)%DayOfWeek,RunPeriodInput(Loop)%MonWeekDay)
    ENDIF
  ENDDO

  cCurrentModuleObject='RunPeriod:CustomRange'
  count=0
  DO Ptr=1,RPAW
    CALL GetObjectItem(cCurrentModuleObject,Ptr,cAlphaArgs,NumAlpha,rNumericArgs,NumNumeric,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IF (.not. lAlphaFieldBlanks(1)) THEN
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),RunPeriodInput%Title,Count,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
    ENDIF

    Count=Count+1
    Loop=RP+Ptr
    RunPeriodInput(Loop)%Title=cAlphaArgs(1)

   !set the start and end day of month from user input
   ! N1 , \field Begin Month
   ! N2 , \field Begin Day of Month
   ! N3,  \field Start Year
   ! N4 , \field End Month
   ! N5 , \field End Day of Month
   ! N6,  \field End Year
    RunPeriodInput(Loop)%StartMonth = Int(rNumericArgs(1))
    RunPeriodInput(Loop)%StartDay   = Int(rNumericArgs(2))
    RunPeriodInput(Loop)%StartYear  = Int(rNumericArgs(3))
    RunPeriodInput(Loop)%EndMonth   = Int(rNumericArgs(4))
    RunPeriodInput(Loop)%EndDay     = Int(rNumericArgs(5))
    RunPeriodInput(Loop)%EndYear    = Int(rNumericArgs(6))
    RunPeriodInput(Loop)%TreatYearsAsConsecutive = .true.

    IF (FullAnnualRun .and. Loop == 1) THEN
      RunPeriodInput(Loop)%StartMonth = 1
      RunPeriodInput(Loop)%StartDay = 1
      RunPeriodInput(Loop)%EndMonth   = 12
      RunPeriodInput(Loop)%EndDay   = 31
    ENDIF


    SELECT CASE (RunPeriodInput(Loop)%StartMonth)

      CASE (1,3,5,7,8,10,12)
        IF (RunPeriodInput(Loop)%StartDay > 31) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(1))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (4,6,9,11)
        IF (RunPeriodInput(Loop)%StartDay > 30) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(1))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (2)
        IF (RunPeriodInput(Loop)%StartDay > 28+LeapYearAdd) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(1))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//' '//  &
             TRIM(cNumericFieldNames(2))//' invalid=['//  &
             TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//']')
        ErrorsFound=.true.
    END SELECT

    SELECT CASE (RunPeriodInput(Loop)%EndMonth)

      CASE (1,3,5,7,8,10,12)
        IF (RunPeriodInput(Loop)%EndDay > 31) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(4))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(3))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (4,6,9,11)
        IF (RunPeriodInput(Loop)%EndDay > 30) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(4))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(3))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE (2)
        IF (RunPeriodInput(Loop)%EndDay > 28+LeapYearAdd) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//', '//  &
             TRIM(cNumericFieldNames(4))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndDay))//']')
          CALL ShowContinueError('Indicated '//trim(cNumericFieldNames(3))//'=['//   &
               trim(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//'].')
          ErrorsFound=.true.
        ENDIF
      CASE DEFAULT
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
             TRIM(cNumericFieldNames(3))//' invalid=['//TRIM(TrimSigDigits(RunPeriodInput(Loop)%EndMonth))//']')
        ErrorsFound=.true.
    END SELECT

    ! A2 , \field Day of Week for Start Day
    IF (lAlphaFieldBlanks(2) .or. cAlphaArgs(2) == 'USEWEATHERFILE') THEN
      RunPeriodInput(Loop)%DayOfWeek=0 ! Defaults to Day of Week from Weather File
    ELSE
      RunPeriodInput(Loop)%DayOfWeek=FindItemInList(cAlphaArgs(2),DaysOfWeek,7)
      IF (RunPeriodInput(Loop)%DayOfWeek == 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(2))//' invalid (Day of Week) ['//  &
            TRIM(cAlphaArgs(2))//' for Start is not Valid, DayofWeek from WeatherFile will be used.')
      ENDIF
    ENDIF

    ! A3,  \field Use Weather File Holidays and Special Days
    IF (lAlphaFieldBlanks(3) .or. SameString(cAlphaArgs(3),'YES')) THEN
      RunPeriodInput(Loop)%UseHolidays=.true.
    ELSEIF (SameString(cAlphaArgs(3),'NO')) THEN
      RunPeriodInput(Loop)%UseHolidays=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(3))//' invalid ['//TRIM(cAlphaArgs(3))//']')
      ErrorsFound=.true.
    ENDIF

    ! A4,  \field Use Weather File Daylight Saving Period
    IF (lAlphaFieldBlanks(4) .or. SameString(cAlphaArgs(4),'YES')) THEN
      RunPeriodInput(Loop)%UseDST=.true.
    ELSEIF (SameString(cAlphaArgs(4),'NO')) THEN
      RunPeriodInput(Loop)%UseDST=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(4))//' invalid ['//TRIM(cAlphaArgs(4))//']')
      ErrorsFound=.true.
    ENDIF

    ! A5,  \field Apply Weekend Holiday Rule
    IF (lAlphaFieldBlanks(5) .or. SameString(cAlphaArgs(5),'YES')) THEN
      RunPeriodInput(Loop)%ApplyWeekendRule=.true.
    ELSEIF (SameString(cAlphaArgs(5),'NO')) THEN
      RunPeriodInput(Loop)%ApplyWeekendRule=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(5))//' invalid ['//TRIM(cAlphaArgs(5))//']')
      ErrorsFound=.true.
    ENDIF

    ! A6,  \field Use Weather File Rain Indicators
    IF (lAlphaFieldBlanks(6) .or. SameString(cAlphaArgs(6),'YES')) THEN
      RunPeriodInput(Loop)%UseRain=.true.
    ELSEIF (SameString(cAlphaArgs(6),'NO')) THEN
      RunPeriodInput(Loop)%UseRain=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(6))//' invalid ['//TRIM(cAlphaArgs(6))//']')
      ErrorsFound=.true.
    ENDIF

    ! A7,  \field Use Weather File Snow Indicators
    IF (lAlphaFieldBlanks(7) .or. SameString(cAlphaArgs(7),'YES')) THEN
      RunPeriodInput(Loop)%UseSnow=.true.
    ELSEIF (SameString(cAlphaArgs(7),'NO')) THEN
      RunPeriodInput(Loop)%UseSnow=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(7))//' invalid ['//TRIM(cAlphaArgs(7))//']')
      ErrorsFound=.true.
    ENDIF

    !calculate the annual start and end days from the user inputted month and day
    RunPeriodInput(Loop)%ActualWeather=.true.
    CALL JGDate(GregorianToJulian,RunPeriodInput(Loop)%StartDate,  &
       RunPeriodInput(Loop)%StartYear,RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay)
    CALL JGDate(GregorianToJulian,RunPeriodInput(Loop)%EndDate,  &
       RunPeriodInput(Loop)%EndYear,RunPeriodInput(Loop)%EndMonth,RunPeriodInput(Loop)%EndDay)
    RunPeriodInput(Loop)%MonWeekDay=0
    IF (RunPeriodInput(Loop)%DayOfWeek /= 0 .and. .not. ErrorsFound) THEN
      CALL SetupWeekDaysByMonth(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,  &
                                RunPeriodInput(Loop)%DayOfWeek,RunPeriodInput(Loop)%MonWeekDay)
    ENDIF
  ENDDO

  IF (TotRunPers == 0 .and. FullAnnualRun) THEN
    DEALLOCATE(RunPeriodInput)
    CALL ShowWarningError('No Run Periods input but Full Annual Simulation selected.  Adding Run Period to 1/1 through 12/31.')
    NumOfEnvrn=NumOfEnvrn+1
    ALLOCATE(TempEnvironment(NumOfEnvrn))
    IF (NumOfEnvrn > 1) THEN
      TempEnvironment(1:NumOfEnvrn-1)=Environment(1:NumOfEnvrn-1)
    ENDIF
    DEALLOCATE(Environment)
    ALLOCATE(Environment(NumOfEnvrn))
    Environment=TempEnvironment
    DEALLOCATE(TempEnvironment)
    Environment(NumOfEnvrn)%KindOfEnvrn = ksRunPeriodWeather
    TotRunPers=1
    WeathSimReq=.true.
    ALLOCATE(RunPeriodInput(TotRunPers))
    RunPeriodInput(1)%StartDate = JulianDay(RunPeriodInput(1)%StartMonth,RunPeriodInput(1)%StartDay,LeapYearAdd)
    RunPeriodInput(1)%EndDate = JulianDay(RunPeriodInput(1)%EndMonth,RunPeriodInput(1)%EndDay,LeapYearAdd)
    RunPeriodInput(1)%MonWeekDay=0
    IF (RunPeriodInput(1)%DayOfWeek /= 0 .and. .not. ErrorsFound) THEN
      CALL SetupWeekDaysByMonth(RunPeriodInput(1)%StartMonth,RunPeriodInput(1)%StartDay,  &
                               RunPeriodInput(1)%DayOfWeek,RunPeriodInput(1)%MonWeekDay)
    ENDIF
  ENDIF

Return

END SUBROUTINE GetRunPeriodData

SUBROUTINE GetRunPeriodDesignData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the run period design info from User input and the
          !  simulation dates

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, GetObjectItem, FindItem, GetNumObjectsFound, VerifyName, SameString
  USE General, ONLY: JulianDay,TrimSigDigits
  USE DataSystemVariables
  USE DataIPShortCuts

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
 LOGICAL, INTENT(INOUT) :: ErrorsFound

         ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(12) :: ValidNames=(/"SUNDAY         ","MONDAY         ","TUESDAY        ",  &
                                                             "WEDNESDAY      ","THURSDAY       ","FRIDAY         ",  &
                                                             "SATURDAY       ","HOLIDAY        ","SUMMERDESIGNDAY",  &
                                                             "WINTERDESIGNDAY","CUSTOMDAY1     ","CUSTOMDAY2     "/)

         ! INTERFACE BLOCK SPECIFICATIONS:
         ! na

         ! DERIVED TYPE DEFINITIONS:
         ! na

         ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlphas   ! Number of alphas being input
  INTEGER :: NumNumerics  ! Number of Numerics being input
  INTEGER :: IOStat           ! IO Status when calling get input subroutine
  INTEGER :: Loop
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
  INTEGER :: RPD1
  INTEGER :: RPD2
  INTEGER :: Count
  INTEGER :: WhichPeriod
!unused1208  CHARACTER(len=MaxNameLength) :: ThisObject


         ! FLOW:
   !Call Input Get routine to retrieve annual run data
  RPD1=GetNumObjectsFound('SizingPeriod:WeatherFileDays')
  RPD2=GetNumObjectsFound('SizingPeriod:WeatherFileConditionType')
  TotRunDesPers=RPD1+RPD2

  ALLOCATE (RunPeriodDesignInput(RPD1+RPD2))

  Count=0
  cCurrentModuleObject='SizingPeriod:WeatherFileDays'
  DO Loop=1,RPD1
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumerics,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),RunPeriodDesignInput%Title,Count,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    Count=Count+1
    RunPeriodDesignInput(Count)%Title=cAlphaArgs(1)
    RunPeriodDesignInput(Count)%PeriodType='User Selected WeatherFile RunPeriod (Design)'

   !set the start and end day of month from user input
     RunPeriodDesignInput(Count)%StartMonth = Int(rNumericArgs(1))
     RunPeriodDesignInput(Count)%StartDay   = Int(rNumericArgs(2))
     RunPeriodDesignInput(Count)%EndMonth   = Int(rNumericArgs(3))
     RunPeriodDesignInput(Count)%EndDay     = Int(rNumericArgs(4))

   SELECT CASE (RunPeriodDesignInput(Count)%StartMonth)

    CASE (1,3,5,7,8,10,12)
      IF (RunPeriodDesignInput(Count)%StartDay > 31) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
           ' '//TRIM(cNumericFieldNames(2))//' invalid (Day of Month) ['//  &
           TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
        ErrorsFound=.true.
      ENDIF
    CASE (4,6,9,11)
      IF (RunPeriodDesignInput(Count)%StartDay > 30) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
           ' '//TRIM(cNumericFieldNames(2))//' invalid (Day of Month) ['//  &
           TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
        ErrorsFound=.true.
      ENDIF
    CASE (2)
      IF (RunPeriodDesignInput(Count)%StartDay > 28+LeapYearAdd) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
           ' '//TRIM(cNumericFieldNames(2))//' invalid (Day of Month) ['//  &
           TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartDay))//']')
        ErrorsFound=.true.
      ENDIF
    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
         ' '//TRIM(cNumericFieldNames(1))//' invalid (Month) ['//  &
         TRIM(TrimSigDigits(RunPeriodInput(Loop)%StartMonth))//']')
      ErrorsFound=.true.
   END SELECT

   IF (lAlphaFieldBlanks(2)) THEN
     RunPeriodDesignInput(Count)%DayOfWeek=2 ! Defaults to Monday
   ELSE
     RunPeriodDesignInput(Count)%DayOfWeek=FindItemInList(cAlphaArgs(2),ValidNames,12)
     IF (RunPeriodDesignInput(Count)%DayOfWeek == 0 .or. RunPeriodDesignInput(Count)%DayOfWeek == 8) THEN
       CALL ShowWarningError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
           ' '//TRIM(cAlphaFieldNames(1))//' invalid (Day of Week) ['//  &
           TRIM(cAlphaArgs(1))//' for Start is not Valid, Monday will be Used.')
       RunPeriodDesignInput(Count)%DayOfWeek=2 ! Defaults to Monday
     ENDIF
   ENDIF

   IF (lAlphaFieldBlanks(3) .or. SameString(cAlphaArgs(3),'YES')) THEN
     RunPeriodDesignInput(Count)%UseDST=.true.
   ELSEIF (SameString(cAlphaArgs(3),'NO')) THEN
     RunPeriodDesignInput(Count)%UseDST=.false.
   ELSE
     CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
           TRIM(cAlphaFieldNames(3))//' invalid ['//TRIM(cAlphaArgs(3))//']')
     ErrorsFound=.true.
   ENDIF

   IF (lAlphaFieldBlanks(4) .or. SameString(cAlphaArgs(4),'YES')) THEN
     RunPeriodDesignInput(Count)%UseRain=.true.
     RunPeriodDesignInput(Count)%UseSnow=.true.
   ELSEIF (SameString(cAlphaArgs(4),'NO')) THEN
     RunPeriodDesignInput(Count)%UseRain=.false.
     RunPeriodDesignInput(Count)%UseSnow=.false.
   ELSE
     CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
           TRIM(cAlphaFieldNames(4))//' invalid ['//TRIM(cAlphaArgs(4))//']')
     ErrorsFound=.true.
   ENDIF

   !calculate the annual start and end days from the user inputted month and day
   RunPeriodDesignInput(Count)%StartDate = JulianDay(RunPeriodDesignInput(Count)%StartMonth,  &
                                                      RunPeriodDesignInput(Count)%StartDay,LeapYearAdd)
   RunPeriodDesignInput(Count)%EndDate   = JulianDay(RunPeriodDesignInput(Count)%EndMonth,  &
                                                      RunPeriodDesignInput(Count)%EndDay,LeapYearAdd)
   IF (RunPeriodDesignInput(Count)%StartDate <= RunPeriodDesignInput(Count)%EndDate) THEN
     RunPeriodDesignInput(Count)%TotalDays=(RunPeriodDesignInput(Count)%EndDate-RunPeriodDesignInput(Count)%StartDate+1)   &
                                   * RunPeriodDesignInput(Count)%NumSimYears
   ELSE
     RunPeriodDesignInput(Count)%TotalDays=(JulianDay(12,31,LeapYearAdd) -   &
                  RunPeriodDesignInput(Count)%StartDate+1+RunPeriodDesignInput(Count)%EndDate) &
                                   * RunPeriodDesignInput(Count)%NumSimYears
   ENDIF
   RunPeriodDesignInput(Count)%MonWeekDay=0
    IF (RunPeriodDesignInput(1)%DayOfWeek /= 0 .and. .not. ErrorsFound) THEN
      CALL SetupWeekDaysByMonth(RunPeriodDesignInput(1)%StartMonth,RunPeriodDesignInput(1)%StartDay,  &
                                RunPeriodDesignInput(1)%DayOfWeek,RunPeriodDesignInput(1)%MonWeekDay)
    ENDIF
  ENDDO

  cCurrentModuleObject='SizingPeriod:WeatherFileConditionType'
  DO Loop=1,RPD2
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumerics,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),RunPeriodDesignInput%Title,Count,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Title')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    Count=Count+1
    RunPeriodDesignInput(Count)%Title=cAlphaArgs(1)
    RunPeriodDesignInput(Count)%PeriodType='User Selected WeatherFile Typical/Extreme Period (Design)='//TRIM(cAlphaArgs(2))

    ! Period Selection
    IF (.not. lAlphaFieldBlanks(2)) THEN
      WhichPeriod=FindItem(cAlphaArgs(2),TypicalExtremePeriods%MatchValue,NumEPWTypExtSets)
      IF (WhichPeriod /= 0) THEN
        RunPeriodDesignInput(Count)%StartDay=TypicalExtremePeriods(WhichPeriod)%StartDay
        RunPeriodDesignInput(Count)%StartMonth=TypicalExtremePeriods(WhichPeriod)%StartMonth
        RunPeriodDesignInput(Count)%StartDate=TypicalExtremePeriods(WhichPeriod)%StartJDay
        RunPeriodDesignInput(Count)%EndDay=TypicalExtremePeriods(WhichPeriod)%EndDay
        RunPeriodDesignInput(Count)%EndMonth=TypicalExtremePeriods(WhichPeriod)%EndMonth
        RunPeriodDesignInput(Count)%EndDate=TypicalExtremePeriods(WhichPeriod)%EndJDay
        RunPeriodDesignInput(Count)%TotalDays=TypicalExtremePeriods(WhichPeriod)%TotalDays
      ELSE
        WhichPeriod=FindItem(cAlphaArgs(2),TypicalExtremePeriods%MatchValue1,NumEPWTypExtSets)
        IF (WhichPeriod /= 0) THEN
          RunPeriodDesignInput(Count)%StartDay=TypicalExtremePeriods(WhichPeriod)%StartDay
          RunPeriodDesignInput(Count)%StartMonth=TypicalExtremePeriods(WhichPeriod)%StartMonth
          RunPeriodDesignInput(Count)%StartDate=TypicalExtremePeriods(WhichPeriod)%StartJDay
          RunPeriodDesignInput(Count)%EndDay=TypicalExtremePeriods(WhichPeriod)%EndDay
          RunPeriodDesignInput(Count)%EndMonth=TypicalExtremePeriods(WhichPeriod)%EndMonth
          RunPeriodDesignInput(Count)%EndDate=TypicalExtremePeriods(WhichPeriod)%EndJDay
          RunPeriodDesignInput(Count)%TotalDays=TypicalExtremePeriods(WhichPeriod)%TotalDays
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
               ' '//TRIM(cAlphaFieldnames(2))//'='//TRIM(cAlphaArgs(2))//' matched to '//  &
               trim(TypicalExtremePeriods(WhichPeriod)%MatchValue))
        ELSE
          WhichPeriod=FindItem(cAlphaArgs(2),TypicalExtremePeriods%MatchValue2,NumEPWTypExtSets)
          IF (WhichPeriod /= 0) THEN
            RunPeriodDesignInput(Count)%StartDay=TypicalExtremePeriods(WhichPeriod)%StartDay
            RunPeriodDesignInput(Count)%StartMonth=TypicalExtremePeriods(WhichPeriod)%StartMonth
            RunPeriodDesignInput(Count)%StartDate=TypicalExtremePeriods(WhichPeriod)%StartJDay
            RunPeriodDesignInput(Count)%EndDay=TypicalExtremePeriods(WhichPeriod)%EndDay
            RunPeriodDesignInput(Count)%EndMonth=TypicalExtremePeriods(WhichPeriod)%EndMonth
            RunPeriodDesignInput(Count)%EndDate=TypicalExtremePeriods(WhichPeriod)%EndJDay
            RunPeriodDesignInput(Count)%TotalDays=TypicalExtremePeriods(WhichPeriod)%TotalDays
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
                 ' '//TRIM(cAlphaFieldnames(2))//'='//TRIM(cAlphaArgs(2))//' matched to '//  &
                 trim(TypicalExtremePeriods(WhichPeriod)%MatchValue))
          ELSE
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
                 ' '//TRIM(cAlphaFieldnames(2))//' invalid (not on Weather File)='//TRIM(cAlphaArgs(2)))
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ENDIF
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
           ' '//TRIM(cAlphaFieldnames(2))//' invalid (blank).')
      ErrorsFound=.true.
    ENDIF

    IF (lAlphaFieldBlanks(3)) THEN
      RunPeriodDesignInput(Count)%DayOfWeek=2 ! Defaults to Monday
    ELSE
      RunPeriodDesignInput(Count)%DayOfWeek=FindItemInList(cAlphaArgs(3),ValidNames,12)
      IF (RunPeriodDesignInput(Count)%DayOfWeek == 0 .or. RunPeriodDesignInput(Count)%DayOfWeek == 8) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': object='//TRIM(RunPeriodDesignInput(Count)%Title)//  &
            ' '//TRIM(cAlphaFieldNames(3))//' invalid (Day of Week) ['//  &
            TRIM(cAlphaArgs(3))//' for Start is not Valid, Monday will be Used.')
        RunPeriodDesignInput(Count)%DayOfWeek=2 ! Defaults to Monday
      ENDIF
    ENDIF

    IF (lAlphaFieldBlanks(4) .or. SameString(cAlphaArgs(4),'YES')) THEN
      RunPeriodDesignInput(Count)%UseDST=.true.
    ELSEIF (SameString(cAlphaArgs(4),'NO')) THEN
      RunPeriodDesignInput(Count)%UseDST=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
           TRIM(cAlphaFieldNames(4))//' invalid ['//TRIM(cAlphaArgs(4))//']')
      ErrorsFound=.true.
    ENDIF

    IF (lAlphaFieldBlanks(5) .or. SameString(cAlphaArgs(5),'YES')) THEN
      RunPeriodDesignInput(Count)%UseRain=.true.
      RunPeriodDesignInput(Count)%UseSnow=.true.
    ELSEIF (SameString(cAlphaArgs(5),'NO')) THEN
      RunPeriodDesignInput(Count)%UseRain=.false.
      RunPeriodDesignInput(Count)%UseSnow=.false.
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': object #'//TRIM(TrimSigDigits(Loop))//  &
            TRIM(cAlphaFieldNames(5))//' invalid ['//TRIM(cAlphaArgs(5))//']')
      ErrorsFound=.true.
    ENDIF
    RunPeriodDesignInput(1)%MonWeekDay=0
    IF (RunPeriodDesignInput(1)%DayOfWeek /= 0 .and. .not. ErrorsFound) THEN
      CALL SetupWeekDaysByMonth(RunPeriodDesignInput(1)%StartMonth,RunPeriodDesignInput(1)%StartDay,  &
                                RunPeriodDesignInput(1)%DayOfWeek,RunPeriodDesignInput(1)%MonWeekDay)
    ENDIF

  ENDDO
Return

END SUBROUTINE GetRunPeriodDesignData

SUBROUTINE GetSpecialDayPeriodData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads any special day period data from the IDF and
          ! processes it into the data structure that will drive the values
          ! in the SpecialDayTypes array.

          ! METHODOLOGY EMPLOYED:
          ! Processes the following IDD definition:
          !
          ! SpecialDayPeriod,
          !      \memo This object sets up holidays/special days to be used during weather file
          !      \memo run periods.  (These are not used with DesignDay objects.)
          !      \memo Depending on the value in the run period, days on the weather file may also
          !      \memo be used.  However, the weather file specification will take precedence over
          !      \memo any specification shown here.  (No error message on duplicate days or overlapping
          !      \memo days).
          !  A1, \field Holiday Name
          !  A2, \field StartDate
          !      \memo  Dates can be several formats:
          !      \memo  <number>/<number>  (month/day)
          !      \memo  <number> Month
          !      \memo  Month <number>
          !      \memo Months are January, February, March, April, May, June, July, August, September, October, November, December
          !      \memo Months can be the first 3 letters of the month
          !        \note will eventually allow: 3 Monday April (meaning 3rd Monday in April)
          !  N1, \field duration (number of days)
          !  A3; \field SpecialDayType
          !        \note SpecialDayType selects the schedules appropriate for each day so labeled
          !        \type choice
          !        \key Holiday
          !        \key SummerDesignDay
          !        \key WinterDesignDay
          !        \key CustomDay1
          !        \key CustomDay2

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, FindItemInList, GetObjectItem, VerifyName
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! will be set to true if severe errors are found in inputs

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(5) :: ValidDayTypes=(/"HOLIDAY        ","SUMMERDESIGNDAY",  &
                                                               "WINTERDESIGNDAY","CUSTOMDAY1     ","CUSTOMDAY2     "/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), DIMENSION(3) :: AlphArray
  INTEGER NumAlphas
  REAL(r64), DIMENSION(1) :: Duration
  INTEGER NumNumbers
  INTEGER NumSpecDays
  INTEGER Count
  INTEGER Loop
  INTEGER PMonth
  INTEGER PDay
  INTEGER PWeekDay
  INTEGER DateType
  INTEGER IOStat
  INTEGER DayType
  LOGICAL :: IsNotOK=.false.               ! Flag to verify name
  LOGICAL :: IsBlank=.false.               ! Flag for blank name

  cCurrentModuleObject='RunPeriodControl:SpecialDays'
  NumSpecDays=GetNumObjectsFound(cCurrentModuleObject)
  IF (ALLOCATED(SpecialDays)) THEN   ! EPW already allocated the array
    Count=NumSpecialDays-NumSpecDays+1
  ELSE
    ALLOCATE(SpecialDays(NumSpecDays))
    NumSpecialDays=NumSpecDays
    Count=1
  ENDIF

  DO Loop=1,NumSpecDays

    CALL GetObjectItem(cCurrentModuleObject,Loop,AlphArray,NumAlphas,Duration,NumNumbers,IOSTAT)

    CALL VerifyName(AlphArray(1),SpecialDays%Name,Count-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) AlphArray(1)='xxxxx'
    ENDIF

    SpecialDays(Count)%Name=AlphArray(1)

    CALL ProcessDateString(AlphArray(2),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
    IF (DateType == MonthDay) THEN
      SpecialDays(Count)%DateType=DateType
      SpecialDays(Count)%Month=PMonth
      SpecialDays(Count)%Day=PDay
      SpecialDays(Count)%WeekDay=0
      SpecialDays(Count)%CompDate=PMonth*32+PDay
      SpecialDays(Count)%WthrFile=.false.
    ELSEIF (DateType /= InvalidDate) THEN
      SpecialDays(Count)%DateType=DateType
      SpecialDays(Count)%Month=PMonth
      SpecialDays(Count)%Day=PDay
      SpecialDays(Count)%WeekDay=PWeekDay
      SpecialDays(Count)%CompDate=0
      SpecialDays(Count)%WthrFile=.false.
    ELSEIF (DateType == InvalidDate) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(AlphArray(1))//' Invalid '//  &
         TRIM(cAlphaFieldNames(2))//'='//TRIM(AlphArray(2)))
      ErrorsFound=.true.
    ENDIF


    IF (Duration(1) > 0) THEN
      SpecialDays(Count)%Duration=INT(Duration(1))
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(AlphArray(1))//' Invalid '//  &
         TRIM(cNumericFieldNames(1))//'='//TRIM(TrimSigDigits(Duration(1),0)))
      ErrorsFound=.true.
    ENDIF

    DayType=FindItemInList(AlphArray(3),ValidDayTypes,5)
    IF (DayType == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': '//TRIM(AlphArray(1))//' Invalid '//  &
         TRIM(cAlphaFieldNames(3))//'='//TRIM(AlphArray(3)))
      ErrorsFound=.true.
    ELSE
      SpecialDays(Count)%DayType=DayType
    ENDIF
    Count=Count+1
  ENDDO

  !CALL CalcSpecialDayTypes

  RETURN

END SUBROUTINE GetSpecialDayPeriodData

SUBROUTINE CalcSpecialDayTypes

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates the array of Special Day types used during
          ! the simulation.

          ! METHODOLOGY EMPLOYED:
          ! Sets up the SpecialDayTypes array that then is used during simulation.
          ! Uses WFLeapYearInd to indicate Leap Year simulation runs.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: JulianDay

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
  INTEGER Loop
  INTEGER Loop1
  INTEGER JDay
  INTEGER Warn

  SpecialDayTypes=0  ! Initialize/Reset Special Day Types array

  DO Loop=1,NumSpecialDays

    IF (SpecialDays(Loop)%WthrFile) CYCLE

    Warn=0

    JDay=JulianDay(SpecialDays(Loop)%Month,SpecialDays(Loop)%Day,LeapYearAdd)-1

    DO Loop1=1,SpecialDays(Loop)%Duration
      JDay=JDay+1
      IF (JDay > 366) THEN
        CALL ShowWarningError('SpecialDay='//TRIM(SpecialDays(Loop)%Name)//  &
                              ' causes index of more than 366, ignoring those beyond 366')
      ELSE
        IF (SpecialDayTypes(JDay) /= 0 .and. Warn == 0) THEN
          CALL ShowWarningError('SpecialDay='//TRIM(SpecialDays(Loop)%Name)// &
                                ' attempted overwrite of previous set special day')
          Warn=1
        ELSEIF (SpecialDayTypes(JDay) == 0) THEN
          SpecialDayTypes(JDay)=SpecialDays(Loop)%DayType
        ENDIF
      ENDIF
    ENDDO
  ENDDO

  RETURN

END SUBROUTINE CalcSpecialDayTypes

SUBROUTINE GetDSTData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets a possible "Daylight Saving Period" from the IDF.  Using this
          ! will overwrite any prior DST data.

          ! METHODOLOGY EMPLOYED:
          ! Processes the following IDD definition:
          ! DaylightSavingPeriod,
          !      \memo This object sets up the Daylight Saving period for any RunPeriod.
          !      \memo Ignores any DaylightSavingperiod values on the weather file and uses this definition.
          !      \memo (These are not used with DesignDay objects.)
          !  A1, \field StartDate
          !  A2, \field EndDate
          !      \memo  Dates can be several formats:
          !      \memo  <number>/<number>  (month/day)
          !      \memo  <number> <Month>
          !      \memo  <Month> <number>
          !      \memo <Nth> <Weekday> in <Month)
          !      \memo Last <WeekDay> in <Month>
          !      \memo <Month> can be January, February, March, April, May, June, July, August, September,
                                      ! October, November, December
          !      \memo Months can be the first 3 letters of the month
          !      \memo <Weekday> can be Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday
          !      \memo <Nth> can be 1 or 1st, 2 or 2nd, etc. up to 5(?)


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! will be set to true if severe errors are found in inputs

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumFound
  INTEGER NumAlphas
  INTEGER IOSTAT
  INTEGER NumNumbers

  cCurrentModuleObject='RunPeriodControl:DaylightSavingTime'
  NumFound=GetNumObjectsFound(cCurrentModuleObject)

  IF (NumFound == 1 ) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IF (NumAlphas /= 2) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Insufficient fields, must have Start AND End Dates')
      ErrorsFound=.true.
    ELSE  ! Correct number of arguments
      CALL ProcessDateString(cAlphaArgs(1),IDFDST%StMon,IDFDST%StDay,IDFDST%StWeekDay,IDFDST%StDateType,ErrorsFound)
      IF (IDFDST%StDateType == InvalidDate) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      ENDIF
      CALL ProcessDateString(cAlphaArgs(2),IDFDST%EnMon,IDFDST%EnDay,IDFDST%EnWeekDay,IDFDST%EnDateType,ErrorsFound)
      IF (IDFDST%EnDateType == InvalidDate) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        ErrorsFound=.true.
      ENDIF
      IDFDaylightSaving=.true.
    ENDIF
  ELSEIF (NumFound > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects in Input File, only one allowed.')
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE GetDSTData

SUBROUTINE GetDesignDayData(TotDesDays,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   September 1997
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine retrieves the design day info from user input file
          !  which is later to be used in the Setup Design Day Routine.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! SizingPeriod:DesignDay,
          !   A1, \field Name
          !   N1,  \field Month
          !   N2,  \field Day of Month
          !   A2,  \field Day Type
          !   N3,  \field Maximum Dry-Bulb Temperature
          !   N4,  \field Daily Dry-Bulb Temperature Range
          !   A3,  \field Dry-Bulb Temperature Range Modifier Type
          !   A4,  \field Dry-Bulb Temperature Range Modifier Day Schedule Name
          !   A5,  \field Humidity Condition Type
          !   N5,  \field Wetbulb or DewPoint at Maximum Dry-Bulb
          !   A6,  \field Humidity Condition Day Schedule Name
          !   N6,  \field Humidity Ratio at Maximum Dry-Bulb
          !   N7,  \field Enthalpy at Maximum Dry-Bulb  !will require units transition.
          !   N8,  \field Daily Wet-Bulb Temperature Range
          !   N9,  \field Barometric Pressure
          !   N10, \field Wind Speed
          !   N11, \field Wind Direction
          !   A7,  \field Rain Indicator
          !   A8,  \field Snow Indicator
          !   A9,  \field Daylight Saving Time Indicator
          !   A10, \field Solar Model Indicator
          !   A11, \field Beam Solar Day Schedule Name
          !   A12, \field Diffuse Solar Day Schedule Name
          !   N12, \field ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
          !   N13, \field ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
          !   N14; \field Sky Clearness

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: FindItemInList, GetObjectItem, VerifyName, RangeCheck, SameString
  USE General, ONLY: RoundSigDigits, FindNumberInList
  USE ScheduleManager, ONLY: GetDayScheduleIndex, GetSingleDayScheduleValues, CheckDayScheduleValueMinMax
  USE DataSystemVariables
  USE OutputReportPredefined

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer :: TotDesDays    ! Total number of Design days to Setup
  LOGICAL, INTENT(INOUT) :: ErrorsFound

         ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(12) :: ValidNames=(/'SUNDAY         ','MONDAY         ','TUESDAY        ',  &
                                                             'WEDNESDAY      ','THURSDAY       ','FRIDAY         ',  &
                                                             'SATURDAY       ','HOLIDAY        ','SUMMERDESIGNDAY',  &
                                                             'WINTERDESIGNDAY','CUSTOMDAY1     ','CUSTOMDAY2     '/)
  CHARACTER(len=*), PARAMETER, DIMENSION(0:DDHumIndType_Count-1) :: HumidityIndicatingType=  &
         (/'Wetbulb [C]                        ',  &
           'Dewpoint [C]                       ',  &
           'Enthalpy [J/kg]                    ',  &
           'Humidity Ratio []                  ',  &
           'Schedule []                        ',  &
           'WetBulbProfileDefaultMultipliers []', &
           'WetBulbProfileDifferenceSchedule []', &
           'WetBulbProfileMultiplierSchedule []'/)


!  REAL(r64), PARAMETER, DIMENSION(24) :: DefaultTempRangeMult=(/ .87d0,.92d0,.96d0,.99d0,1.0d0,.98d0,.93d0,  &
!                   .84d0,.71d0,.56d0,.39d0,.23d0, .11d0,.03d0,.00d0,.03d0,.10d0,.21d0,.34d0,.47d0,.58d0,.68d0,.76d0,.82d0 /)
  ! Below are the 2009 fractions, HOF, Chap 14, Table 6
  REAL(r64), PARAMETER, DIMENSION(24) :: DefaultTempRangeMult=(/ .88d0,.92d0,.95d0,.98d0,1.0d0,.98d0,.91d0,  &
                   .74d0,.55d0,.38d0,.23d0,.13d0, .05d0,0.00d0,0.00d0,.06d0,.14d0,.24d0,.39d0,.50d0,.59d0,.68d0,.75d0,.82d0 /)

         ! INTERFACE BLOCK SPECIFICATIONS:
         ! na

         ! DERIVED TYPE DEFINITIONS:
         ! na

         ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer :: EnvrnNum      ! Environment Loop to pass to Design Day Setup Routine
  INTEGER :: NumAlpha ! Number of material alpha names being passed
  INTEGER :: NumNumerics  ! Number of material properties being passed
  INTEGER :: IOStat           ! IO Status when calling get input subroutine
  LOGICAL :: IsNotOK               ! Flag to verify name
  LOGICAL :: IsBlank               ! Flag for blank name
  INTEGER :: HrLoop
  INTEGER :: TSLoop
  REAL(r64) LastHrValue
  REAL(r64) WNow
  REAL(r64) WPrev
  REAL(r64) testval
  LOGICAL errflag
  INTEGER DDLoop
  CHARACTER(len=MaxNameLength) :: envTitle
  CHARACTER(len=15) :: units
  INTEGER :: schPtr
  LOGICAL :: MaxDryBulbEntered
  LOGICAL :: PressureEntered

         ! FLOW:

   ALLOCATE (DesDayInput(TotDesDays))! Allocate the array to the # of DD's
   ALLOCATE (DDDBRngModifier(TotDesDays,24,NumOfTimeStepInHour))
   DDDBRngModifier=0.0d0
   ALLOCATE (DDHumIndModifier(TotDesDays,24,NumOfTimeStepInHour))
   DDHumIndModifier=0.0d0
   ALLOCATE (DDBeamSolarValues(TotDesDays,24,NumOfTimeStepInHour))
   DDBeamSolarValues=0.0d0
   ALLOCATE (DDDiffuseSolarValues(TotDesDays,24,NumOfTimeStepInHour))
   DDDiffuseSolarValues=0.0d0
   ALLOCATE (DDSkyTempScheduleValues(TotDesDays,24,NumOfTimeStepInHour))
   DDSkyTempScheduleValues=0.0d0

   ALLOCATE(SPSiteDryBulbRangeModScheduleValue(TotDesDays))
   SPSiteDryBulbRangeModScheduleValue = 0.0d0
   ALLOCATE(SPSiteHumidityConditionScheduleValue(TotDesDays))
   SPSiteHumidityConditionScheduleValue = 0.0d0
   ALLOCATE(SPSiteBeamSolarScheduleValue(TotDesDays))
   SPSiteBeamSolarScheduleValue = 0.0d0
   ALLOCATE(SPSiteDiffuseSolarScheduleValue(TotDesDays))
   SPSiteDiffuseSolarScheduleValue = 0.0d0
   ALLOCATE(SPSiteSkyTemperatureScheduleValue(TotDesDays))
   SPSiteSkyTemperatureScheduleValue = 0.0d0

   IF (ReverseDD .and. TotDesDays <=1) THEN
     CALL ShowSevereError('GetDesignDayData: Reverse Design Day requested but # Design Days <=1')
   ENDIF

   cCurrentModuleObject='SizingPeriod:DesignDay'
   Do DDLoop = 1,TotDesDays

     IF (ReverseDD) THEN
       IF (DDLoop == 1 .and. TotDesDays > 1) THEN
         EnvrnNum=2
       ELSEIF (DDLoop == 2) THEN
         EnvrnNum=1
       ELSE
         EnvrnNum=DDLoop
       ENDIF
     ELSE
       EnvrnNum=DDLoop
     ENDIF


  !Call Input Get routine to retrieve design day data
    MaxDryBulbEntered=.false.
    PressureEntered=.false.
    CALL GetObjectItem(cCurrentModuleObject,DDLoop,cAlphaArgs,NumAlpha,rNumericArgs,NumNumerics,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

          !   A1, \field Name
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),DesDayInput%Title,EnvrnNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    DesDayInput(EnvrnNum)%Title = cAlphaArgs(1)            ! Environment name
    Environment(EnvrnNum)%Title = DesDayInput(EnvrnNum)%Title

          !   N3,  \field Maximum Dry-Bulb Temperature
          !   N4,  \field Daily Dry-Bulb Temperature Range
          !   N9,  \field Barometric Pressure
          !   N10, \field Wind Speed
          !   N11, \field Wind Direction
    DesDayInput(EnvrnNum)%MaxDryBulb = rNumericArgs(3)       ! Maximum Dry-Bulb Temperature (C)
    IF (.not. lNumericFieldBlanks(3)) MaxDryBulbEntered=.true.
    DesDayInput(EnvrnNum)%DailyDBRange = rNumericArgs(4)     ! Daily dry-bulb temperature range (deltaC)
    DesDayInput(EnvrnNum)%PressBarom = rNumericArgs(9)       ! Atmospheric/Barometric Pressure (Pascals)
    IF (.not. lNumericFieldBlanks(9)) PressureEntered=.true.
    DesDayInput(EnvrnNum)%PressureEntered=PressureEntered
    DesDayInput(EnvrnNum)%WindSpeed = rNumericArgs(10)        ! Wind Speed (m/s)
    DesDayInput(EnvrnNum)%WindDir = MOD(rNumericArgs(11),360.d0)! Wind Direction
                                                               ! (degrees clockwise from North, N=0, E=90, S=180, W=270)
          !   N1,  \field Month
          !   N2,  \field Day of Month
          !   N12, \field ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub)
          !   N13, \field ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud)
          !   N8,  \field Daily Wet-Bulb Temperature Range
    DesDayInput(EnvrnNum)%Month=Int(rNumericArgs(1))        ! Month of Year ( 1 - 12 )
    DesDayInput(EnvrnNum)%DayOfMonth=Int(rNumericArgs(2))   ! Day of Month ( 1 - 31 )
    DesDayInput(EnvrnNum)%TauB=rNumericArgs(12)              ! beam tau >= 0
    DesDayInput(EnvrnNum)%TauD=rNumericArgs(13)              ! diffuse tau >= 0
    DesDayInput(EnvrnNum)%DailyWBRange=rNumericArgs(8)      ! Daily wet-bulb temperature range (deltaC)

          !   N14; \field Sky Clearness
    DesDayInput(EnvrnNum)%SkyClear = rNumericArgs(14)         ! Sky Clearness (0 to 1)

          !   A7,  \field Rain Indicator
    IF (SameString(cAlphaArgs(7),'Yes') .or. SameString(cAlphaArgs(7),'1') ) THEN
      DesDayInput(EnvrnNum)%RainInd=1
    ELSEIF (SameString(cAlphaArgs(7),'No') .or. SameString(cAlphaArgs(7),'0') .or. lAlphaFieldBlanks(7)) THEN
      DesDayInput(EnvrnNum)%RainInd=0
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//  &
         '", invalid field: '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
      CALL ShowContinueError('"No" will be used.')
      DesDayInput(EnvrnNum)%RainInd=0
    ENDIF

          !   A8,  \field Snow Indicator
    IF (SameString(cAlphaArgs(8),'Yes') .or. SameString(cAlphaArgs(8),'1') ) THEN
      DesDayInput(EnvrnNum)%SnowInd=1
    ELSEIF (SameString(cAlphaArgs(8),'No') .or. SameString(cAlphaArgs(8),'0') .or. lAlphaFieldBlanks(8)) THEN
      DesDayInput(EnvrnNum)%SnowInd=0
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//  &
         '", invalid field: '//TRIM(cAlphaFieldNames(8))//'="'//TRIM(cAlphaArgs(8))//'".')
      CALL ShowContinueError('"No" will be used.')
      DesDayInput(EnvrnNum)%SnowInd=0
    ENDIF

          !   A3,  \field Dry-Bulb Temperature Range Modifier Type
    ! check DB profile input
    IF (lAlphaFieldBlanks(3)) THEN
      cAlphaArgs(3)='DefaultMultipliers'
      DesDayInput(EnvrnNum)%DBTempRangeType = DDDBRangeType_Default
    ELSEIF (SameString(cAlphaArgs(3),'Multiplier') .or. SameString(cAlphaArgs(3),'MultiplierSchedule')) THEN
      cAlphaArgs(3)='MultiplierSchedule'
      DesDayInput(EnvrnNum)%DBTempRangeType = DDDBRangeType_Multiplier
      units='[]'
    ELSEIF (SameString(cAlphaArgs(3),'Difference') .or. SameString(cAlphaArgs(3),'Delta') .or.   &
            SameString(cAlphaArgs(3),'DifferenceSchedule') .or. SameString(cAlphaArgs(3),'DeltaSchedule')) THEN
      cAlphaArgs(3)='DifferenceSchedule'
      DesDayInput(EnvrnNum)%DBTempRangeType = DDDBRangeType_Difference
      units='[deltaC]'
    ELSEIF (SameString(cAlphaArgs(3),'DefaultMultipliers')) THEN
      cAlphaArgs(3)='DefaultMultipliers'
      DesDayInput(EnvrnNum)%DBTempRangeType = DDDBRangeType_Default
      ! Validate Temperature - Daily range
    ELSEIF (SameString(cAlphaArgs(3),'TemperatureProfileSchedule')) THEN
      cAlphaArgs(3)='TemperatureProfileSchedule'
      DesDayInput(EnvrnNum)%DBTempRangeType = DDDBRangeType_Profile
      units='[C]'
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
      ErrorsFound=.true.
      cAlphaArgs(3)='invalid field'
      DesDayInput(EnvrnNum)%DBTempRangeType = DDDBRangeType_Default
    ENDIF

    IF (DesDayInput(EnvrnNum)%DBTempRangeType /= DDDBRangeType_Profile .and. .not. MaxDryBulbEntered .and.   &
        cAlphaArgs(3) /= 'invalid field') THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('..invalid blank field: '//TRIM(cNumericFieldNames(3)))
      CALL ShowContinueError('..this field is required when '//trim(cAlphaFieldNames(3))//'="'//  &
         trim(cAlphaArgs(3))//'".')
      ErrorsFound=.true.
    ENDIF

    ! Assume either "multiplier" option will make full use of range...
    IF (DesDayInput(EnvrnNum)%DBTempRangeType /= DDDBRangeType_Difference .and.   &
        DesDayInput(EnvrnNum)%DBTempRangeTYpe /= DDDBRangeType_Profile) THEN
      testval=DesDayInput(EnvrnNum)%MaxDryBulb-DesDayInput(EnvrnNum)%DailyDBRange
      errflag=.false.
      CALL RangeCheck(errflag,cAlphaFieldNames(3),cCurrentModuleObject,'Severe','>= -90',(testval>=-90.d0), &
                        '<= 70',(testval <=70.d0),WhatObjectName=DesDayInput(EnvrnNum)%Title)
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF
    ENDIF

          !   A4,  \field Dry-Bulb Temperature Range Modifier Day Schedule Name
    IF (DesDayInput(EnvrnNum)%DBTempRangeType /= DDDBRangeType_Default) THEN
      IF (.not. lAlphaFieldBlanks(4)) THEN
        DesDayInput(EnvrnNum)%TempRangeSchPtr=GetDayScheduleIndex(cAlphaArgs(4))
        IF (DesDayInput(EnvrnNum)%TempRangeSchPtr == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
          CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
          ErrorsFound=.true.
        ELSE
          CALL GetSingleDayScheduleValues(DesDayInput(EnvrnNum)%TempRangeSchPtr,DDDBRngModifier(EnvrnNum,:,:))
          schPtr=FindNumberInList(DesDayInput(EnvrnNum)%TempRangeSchPtr,SPSiteScheduleNamePtr,NumSPSiteScheduleNamePtrs)
          IF (schPtr == 0) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%TempRangeSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Drybulb Temperature Range Modifier Schedule Value '//units,  &
               SPSiteDryBulbRangeModScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(4))
          ELSEIF (SPSiteScheduleUnits(schPtr)/= units) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%TempRangeSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Drybulb Temperature Range Modifier Schedule Value '//units,  &
               SPSiteDryBulbRangeModScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(4))
          ENDIF
          IF (cAlphaArgs(3) == 'MultiplierSchedule') THEN
            IF ( .not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%TempRangeSchPtr,0.0d0,'>=',1.0d0,'<=')) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
              CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
              CALL ShowContinueError('..Specified [Schedule] Dry-bulb Range Multiplier Values are not within [0.0, 1.0]')
              ErrorsFound=.true.
            ENDIF
          ELSEIF (cAlphaArgs(3) == 'DifferenceSchedule') THEN  ! delta, must be > 0.0
            IF (.not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%TempRangeSchPtr,0.0d0,'>=')) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
              CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
              CALL ShowSevereError('Some [Schedule] Dry-bulb Range Difference Values are < 0.0 [would make max larger].')
              ErrorsFound=.true.
            ENDIF
          ENDIF
          IF (cAlphaArgs(3) == 'TemperatureProfileSchedule') THEN
            testval=MAXVAL(DDDBRngModifier(EnvrnNum,:,:))
            IF (MaxDryBulbEntered) THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", data override.')
              CALL ShowContinueError('..'//trim(cNumericFieldNames(3))//'=['//  &
                 trim(RoundSigDigits(DesDayInput(EnvrnNum)%MaxDryBulb,2))//'] will be overwritten.')
              CALL ShowContinueError('..'//trim(cAlphaFieldNames(3))//'="'//trim(cAlphaArgs(3))//'".')
              CALL ShowContinueError('..with max value=['//trim(RoundSigDigits(testval,2))//'].')
            ENDIF
            DesDayInput(EnvrnNum)%MaxDryBulb=testval
          ENDIF
          testval=MAXVAL(DDDBRngModifier(EnvrnNum,:,:))
          testval=DesDayInput(EnvrnNum)%MaxDryBulb-testval
          errflag=.false.
          CALL RangeCheck(errflag,TRIM(cAlphaFieldNames(4)),TRIM(cCurrentModuleObject),'Severe','>= -90',(testval>=-90.d0), &
                           '<= 70',(testval <=70.d0),WhatObjectName=DesDayInput(EnvrnNum)%Title)
          IF (errflag) THEN
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(4))//' is blank.')
        CALL ShowContinueError('..required when '//TRIM(cAlphaFieldNames(3))//' indicates "SCHEDULE".')
        ErrorsFound=.true.
      ENDIF
    ELSE
      ! Default dry-bulb temperature Range
      LastHrValue=DefaultTempRangeMult(24)
      DO HrLoop=1,24
        DO TSLoop=1,NumOfTimeStepInHour
          WNow=Interpolation(TSLoop)
          WPrev=1.0-WNow
          DDDBRngModifier(EnvrnNum,HrLoop,TSLoop)=LastHrValue*WPrev+DefaultTempRangeMult(HrLoop)*WNow
        ENDDO
        LastHrValue=DefaultTempRangeMult(HrLoop)
      ENDDO
    ENDIF

          !   A5,  \field Humidity Condition Type
    IF (SameString(cAlphaArgs(5),'WetBulb')) THEN
      cAlphaArgs(5)='WetBulb'
          !   N5,  \field Wetbulb or DewPoint at Maximum Dry-Bulb
      IF (.not. lNumericFieldBlanks(5)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(5)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(5))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
      errflag=.false.
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_Wetbulb
      CALL RangeCheck(errflag,TRIM(cAlphaFieldNames(5))//' - Wet-Bulb',cCurrentModuleObject,'Severe','>= -90',  &
                     (DesDayInput(EnvrnNum)%HumIndValue>=-90.d0),'<= 70',(DesDayInput(EnvrnNum)%HumIndValue <=70.d0),  &
                     WhatObjectName=DesDayInput(EnvrnNum)%Title)
      IF (errflag) THEN
!        CALL ShowContinueError(TRIM(cCurrentModuleObject)//': Occured in '//TRIM(DesDayInput(EnvrnNum)%Title))
        ErrorsFound=.true.
      ENDIF
    ELSEIF (SameString(cAlphaArgs(5),'DewPoint')) THEN
      cAlphaArgs(5)='DewPoint'
      IF (.not. lNumericFieldBlanks(5)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(5)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(5))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
      errflag=.false.
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_Dewpoint
      CALL RangeCheck(errflag,TRIM(cAlphaFieldNames(5))//' - Dew-Point',TRIM(cCurrentModuleObject),'Severe','>= -90',  &
                     (DesDayInput(EnvrnNum)%HumIndValue>=-90.d0),'<= 70',(DesDayInput(EnvrnNum)%HumIndValue <=70.d0),  &
                      WhatObjectName=DesDayInput(EnvrnNum)%Title)
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF
    ELSEIF (SameString(cAlphaArgs(5),'HumidityRatio')) THEN
      cAlphaArgs(5)='HumidityRatio'
          !   N6,  \field Humidity Ratio at Maximum Dry-Bulb
      IF (.not. lNumericFieldBlanks(6)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(6)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(6))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
      errflag=.false.
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_HumRatio
      CALL RangeCheck(errflag,TRIM(cAlphaFieldNames(5))//' - Humidity-Ratio',TRIM(cCurrentModuleObject),'Severe','>= 0',  &
                     (DesDayInput(EnvrnNum)%HumIndValue>=0.d0),'<= .03',(DesDayInput(EnvrnNum)%HumIndValue <=.03d0),  &
                     WhatObjectName=DesDayInput(EnvrnNum)%Title)
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF
    ELSEIF (SameString(cAlphaArgs(5),'Enthalpy')) THEN
      cAlphaArgs(5)='Enthalpy'
          !   N7,  \field Enthalpy at Maximum Dry-Bulb  !will require units transition.
      IF (.not. lNumericFieldBlanks(7)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(7)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(7))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
      errflag=.false.
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_Enthalpy
      CALL RangeCheck(errflag,TRIM(cAlphaFieldNames(5))//' - Enthalpy','SizingPeriod:DesignDay','Severe','>= 0.0',  &
                     (DesDayInput(EnvrnNum)%HumIndValue>=0.d0),'<= 130000',(DesDayInput(EnvrnNum)%HumIndValue <=130000.d0),  &
                      WhatObjectName=DesDayInput(EnvrnNum)%Title)
      IF (errflag) THEN
        ErrorsFound=.true.
      ENDIF
    ELSEIF (SameString(cAlphaArgs(5),'RelativeHumiditySchedule')) THEN
      cAlphaArgs(5)='RelativeHumiditySchedule'
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_RelHumSch
      units='[%]'
    ELSEIF (SameString(cAlphaArgs(5),'WetBulbProfileMultiplierSchedule')) THEN
      cAlphaArgs(5)='WetBulbProfileMultiplierSchedule'
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_WBProfMul
      units='[]'
      IF (.not. lNumericFieldBlanks(5)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(5)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(5))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSEIF (SameString(cAlphaArgs(5),'WetBulbProfileDifferenceSchedule')) THEN
      cAlphaArgs(5)='WetBulbProfileDifferenceSchedule'
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_WBProfDif
      units='[]'
      IF (.not. lNumericFieldBlanks(5)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(5)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(5))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSEIF (SameString(cAlphaArgs(5),'WetBulbProfileDefaultMultipliers')) THEN
      cAlphaArgs(5)='WetBulbProfileDefaultMultipliers'
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_WBProfDef
      IF (.not. lNumericFieldBlanks(5)) THEN
        DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(5)       ! Humidity Indicating Conditions at Max Dry-Bulb
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(5))//' is blank.')
        CALL ShowContinueError('..field is required when '//trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'".')
        ErrorsFound=.true.
      ENDIF
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
      CALL ShowContinueError('WetBulb will be used. Maximum Dry Bulb will be used as WetBulb at Maximum Dry Bulb.')
      cAlphaArgs(5)='WetBulb'
      DesDayInput(EnvrnNum)%HumIndType=DDHumIndType_Wetbulb
      DesDayInput(EnvrnNum)%HumIndValue = rNumericArgs(3)
    ENDIF

    ! resolve humidity schedule if needed
          !   A6,  \field Humidity Condition Day Schedule Name
    IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_RelHumSch .or. &
        DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfMul .or. &
        DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDif) THEN
      IF (lAlphaFieldBlanks(6)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(6))//' is blank.')
        CALL ShowContinueError('..field is required when '//TRIM(cAlphaFieldNames(3))//'="'// TRIM(cAlphaArgs(3)) // '".')
        ErrorsFound=.true.
      ELSE
        DesDayInput(EnvrnNum)%HumIndSchPtr=GetDayScheduleIndex(cAlphaArgs(6))
        IF (DesDayInput(EnvrnNum)%HumIndSchPtr == 0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
          CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
          CALL ShowContinueError('Default Humidity will be used (constant for day using Humidity Indicator Temp).')
          ! reset HumIndType ?
        ELSE

          CALL GetSingleDayScheduleValues( DesDayInput(EnvrnNum)%HumIndSchPtr, DDHumIndModifier(EnvrnNum,:,:))

          schPtr=FindNumberInList(DesDayInput(EnvrnNum)%HumIndSchPtr,SPSiteScheduleNamePtr,NumSPSiteScheduleNamePtrs)
          IF (schPtr == 0) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%HumIndSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Humidity Condition Schedule Value '//units,  &
               SPSiteHumidityConditionScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(6))
          ELSEIF (SPSiteScheduleUnits(schPtr)/= units) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%HumIndSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Humidity Condition Schedule Value '//units,  &
               SPSiteHumidityConditionScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(6))
          ENDIF


          SELECT CASE (DesDayInput(EnvrnNum)%HumIndType)

           CASE (DDHumIndType_RelHumSch)
             IF ( .not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%HumIndSchPtr,0.0,'>=',100.0,'<=')) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
               CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
               CALL ShowContinueError('Specified [Scheduled] Relative Humidity Values are not within [0.0, 100.0]')
               ErrorsFound=.true.
             ENDIF

           CASE (DDHumIndType_WBProfMul)
             ! multiplier: use schedule value, check 0 <= v <= 1
             IF ( .not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%HumIndSchPtr,0.0d0,'>=',1.0d0,'<=')) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
               CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
               CALL ShowContinueError('..Specified [Schedule] Wet-bulb Profile Range Multiplier Values are not within [0.0, 1.0]')
               ErrorsFound=.true.
             ENDIF

           CASE (DDHumIndType_WBProfDif)
             IF (.not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%HumIndSchPtr,0.0d0,'>=')) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
               CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
               CALL ShowSevereError('Some [Schedule] Wet-bulb Profile Difference Values are < 0.0 [would make max larger].')
               ErrorsFound=.true.
             ENDIF
          END SELECT
        END IF
     END IF

    ELSE IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDef) THEN
      ! re WetBulbProfileDefaultMultipliers
      LastHrValue=DefaultTempRangeMult(24)
      DO HrLoop=1,24
        DO TSLoop=1,NumOfTimeStepInHour
          WNow=Interpolation(TSLoop)
          WPrev=1.0-WNow
          DDHumIndModifier(EnvrnNum,HrLoop,TSLoop)=LastHrValue*WPrev+DefaultTempRangeMult(HrLoop)*WNow
        ENDDO
        LastHrValue=DefaultTempRangeMult(HrLoop)
      ENDDO
    ! ELSE missing case?
    ENDIF


    ! verify that design WB or DP <= design DB
    IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_Dewpoint .or.   &
        DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_Wetbulb .or. &
        DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfMul .or. &
        DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDef .or. &
        DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_WBProfDif) THEN
      IF (DesDayInput(EnvrnNum)%HumIndValue > DesDayInput(EnvrnNum)%MaxDryBulb) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", range check data.')
        CALL ShowContinueError('..Humidity Indicator Temperature at Max Temperature='//  &
                   TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%HumIndValue,1))//  &
                   ' > Max DryBulb='//TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%MaxDryBulb,1)))
        CALL ShowContinueError('..'//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
        CALL ShowContinueError('..Conditions for day will be set to Relative Humidity = 100%')
        IF (DesDayInput(EnvrnNum)%HumIndType == DDHumIndType_Dewpoint) THEN
          DesDayInput(EnvrnNum)%DewpointNeedsSet = .true.
        ELSE
          ! wet-bulb
          DesDayInput(EnvrnNum)%HumIndValue = DesDayInput(EnvrnNum)%MaxDryBulb
        ENDIF
      ENDIF
    ENDIF

          !   A10, \field Solar Model Indicator
    IF (lAlphaFieldBlanks(10)) THEN
      DesDayInput(EnvrnNum)%SolarModel = ASHRAE_ClearSky
    ELSEIF (SameString(cAlphaArgs(10),'ASHRAEClearSky') .or. SameString(cAlphaArgs(10),'CLEARSKY')) THEN
      DesDayInput(EnvrnNum)%SolarModel = ASHRAE_ClearSky
    ELSEIF (SameString(cAlphaArgs(10),'ZhangHuang')) THEN
      DesDayInput(EnvrnNum)%SolarModel = Zhang_Huang
    ELSEIF (SameString(cAlphaArgs(10), 'ASHRAETau')) THEN
      DesDayInput(EnvrnNum)%SolarModel = ASHRAE_Tau
    ELSEIF (SameString(cAlphaArgs(10),'Schedule')) THEN
      DesDayInput(EnvrnNum)%SolarModel = SolarModel_Schedule
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(10))//'="'//TRIM(cAlphaArgs(10))//'".')
      CALL ShowContinueError('Model used will be ASHRAE ClearSky')
      DesDayInput(EnvrnNum)%SolarModel = ASHRAE_ClearSky
    ENDIF

    IF (DesDayInput(EnvrnNum)%SolarModel == SolarModel_Schedule) THEN
          !   A11, \field Beam Solar Day Schedule Name
      IF (.not. lAlphaFieldBlanks(11)) THEN
        DesDayInput(EnvrnNum)%BeamSolarSchPtr=GetDayScheduleIndex(cAlphaArgs(11))
        IF (DesDayInput(EnvrnNum)%BeamSolarSchPtr == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
          CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(11))//'="'//TRIM(cAlphaArgs(11))//'".')
          CALL ShowContinueError('..Required when '//TRIM(cAlphaFieldNames(10))//' indicates "Schedule".')
          ErrorsFound=.true.
        ELSE
          CALL GetSingleDayScheduleValues(DesDayInput(EnvrnNum)%BeamSolarSchPtr,DDBeamSolarValues(EnvrnNum,:,:))
          schPtr=FindNumberInList(DesDayInput(EnvrnNum)%BeamSolarSchPtr,SPSiteScheduleNamePtr,NumSPSiteScheduleNamePtrs)
          units='[W/m2]'
          IF (schPtr == 0) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%BeamSolarSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Beam Solar Schedule Value '//units,  &
               SPSiteBeamSolarScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(11))
          ELSEIF (SPSiteScheduleUnits(schPtr)/= units) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%BeamSolarSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Beam Solar Schedule Value '//units,  &
               SPSiteBeamSolarScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(11))
          ENDIF
          IF ( .not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%BeamSolarSchPtr,0.0,'>=')) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
            CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(11))//'="'//TRIM(cAlphaArgs(11))//'".')
            CALL ShowContinueError('..Specified [Schedule] Values are not >= 0.0')
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ELSE  ! should have entered beam schedule
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(11))//' is blank.')
        ErrorsFound=.true.
      ENDIF
          !   A12, \field Diffuse Solar Day Schedule Name
      IF (.not. lAlphaFieldBlanks(12)) THEN
        DesDayInput(EnvrnNum)%DiffuseSolarSchPtr=GetDayScheduleIndex(cAlphaArgs(12))
        IF (DesDayInput(EnvrnNum)%DiffuseSolarSchPtr == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
          CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(12))//'="'//TRIM(cAlphaArgs(12))//'".')
          CALL ShowContinueError('..Required when '//TRIM(cAlphaFieldNames(10))//' indicates "Schedule".')
          ErrorsFound=.true.
        ELSE
          CALL GetSingleDayScheduleValues(DesDayInput(EnvrnNum)%DiffuseSolarSchPtr,DDDiffuseSolarValues(EnvrnNum,:,:))
          schPtr=FindNumberInList(DesDayInput(EnvrnNum)%DiffuseSolarSchPtr,SPSiteScheduleNamePtr,NumSPSiteScheduleNamePtrs)
          units='[W/m2]'
          IF (schPtr == 0) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%DiffuseSolarSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Diffuse Solar Schedule Value '//units,  &
               SPSiteDiffuseSolarScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(12))
          ELSEIF (SPSiteScheduleUnits(schPtr)/= units) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=DesDayInput(EnvrnNum)%DiffuseSolarSchPtr
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Diffuse Solar Schedule Value '//units,  &
               SPSiteDiffuseSolarScheduleValue(EnvrnNum),'Zone','Average',cAlphaArgs(12))
          ENDIF
          IF ( .not. CheckDayScheduleValueMinMax(DesDayInput(EnvrnNum)%DiffuseSolarSchPtr,0.0,'>=')) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
            CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(12))//'="'//TRIM(cAlphaArgs(12))//'".')
            CALL ShowContinueError('..Specified [Schedule] Values are not >= 0.0')
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ELSE  ! should have entered diffuse schedule
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(12))//' is blank.')
        ErrorsFound=.true.
      ENDIF
    ENDIF

    IF (DesDayInput(EnvrnNum)%SolarModel == ASHRAE_ClearSky) THEN
      IF (lNumericFieldBlanks(14)) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('..invalid field: '//TRIM(cNumericFieldNames(14))//' is blank.')
        CALL ShowContinueError('..Zero clear sky (no solar) will be used.')
      ENDIF
    ENDIF

    ! Validate Design Day Month

    SELECT CASE (DesDayInput(EnvrnNum)%Month)

    CASE (1,3,5,7,8,10,12)
      IF (DesDayInput(EnvrnNum)%DayOfMonth > 31) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('.. invalid field: '//TRIM(cNumericFieldNames(2))//'=['//  &
           TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%DayOfMonth))//  &
           '], Month=['//trim(RoundSigDigits(DesDayInput(EnvrnNum)%Month))//'].')
        ErrorsFound=.true.
      ENDIF
    CASE (4,6,9,11)
      IF (DesDayInput(EnvrnNum)%DayOfMonth > 30) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('.. invalid '//TRIM(cNumericFieldNames(2))//'=['//  &
           TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%DayOfMonth))//  &
           '], Month=['//trim(RoundSigDigits(DesDayInput(EnvrnNum)%Month))//'].')
        ErrorsFound=.true.
      ENDIF
    CASE (2)
      IF (DesDayInput(EnvrnNum)%DayOfMonth > 28) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
        CALL ShowContinueError('.. invalid '//TRIM(cNumericFieldNames(2))//'=['//  &
           TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%DayOfMonth))//  &
           '], Month=['//trim(RoundSigDigits(DesDayInput(EnvrnNum)%Month))//'].')
        ErrorsFound=.true.
      ENDIF
    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('.. invalid '//TRIM(cNumericFieldNames(1))//' invalid (Month) ['//  &
         TRIM(RoundSigDigits(DesDayInput(EnvrnNum)%Month))//'].')
      ErrorsFound=.true.
    END SELECT

          !   A9,  \field Daylight Saving Time Indicator
    IF (SameString(cAlphaArgs(9),'Yes') .or. SameString(cAlphaArgs(9),'1') ) THEN
      DesDayInput(EnvrnNum)%DSTIndicator=1
    ELSEIF (SameString(cAlphaArgs(9),'No') .or. SameString(cAlphaArgs(9),'0') .or. lAlphaFieldBlanks(9)) THEN
      DesDayInput(EnvrnNum)%DSTIndicator=0
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(9))//'="'//TRIM(cAlphaArgs(9))//'". "No" will be used.')
      DesDayInput(EnvrnNum)%DSTIndicator=0
    ENDIF

          !   A2,  \field Day Type
    DesDayInput(EnvrnNum)%DayType=FindItemInList(cAlphaArgs(2),ValidNames,12)
    IF (DesDayInput(EnvrnNum)%DayType == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(DesDayInput(EnvrnNum)%Title)//'", invalid data.')
      CALL ShowContinueError('..invalid field: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      CALL ShowContinueError('Valid values are Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,'//  &
          'Holiday,SummerDesignDay,WinterDesignDay,CustomDay1,CustomDay2.')
      ErrorsFound=.true.
    ENDIF

    Environment(EnvrnNum)%Title=DesDayInput(EnvrnNum)%Title
    Environment(EnvrnNum)%KindOfEnvrn = ksDesignDay
    Environment(EnvrnNum)%TotalDays=1
    Environment(EnvrnNum)%StartMonth=DesDayInput(EnvrnNum)%Month
    Environment(EnvrnNum)%StartDay=DesDayInput(EnvrnNum)%DayOfMonth
    Environment(EnvrnNum)%EndMonth=Environment(EnvrnNum)%StartMonth
    Environment(EnvrnNum)%EndDay=Environment(EnvrnNum)%StartDay
    Environment(EnvrnNum)%DayOfWeek=0
    Environment(EnvrnNum)%UseDST=.false.
    Environment(EnvrnNum)%UseHolidays=.false.
    Environment(EnvrnNum)%StartJDay = DesignDay(EnvrnNum)%DayOfYear
    Environment(EnvrnNum)%EndJDay=Environment(EnvrnNum)%StartJDay

    !create predefined report on design day
    envTitle = DesDayInput(EnvrnNum)%Title
    CALL PreDefTableEntry(pdchDDmaxDB,envTitle,DesDayInput(EnvrnNum)%MaxDryBulb)
    CALL PreDefTableEntry(pdchDDrange,envTitle,DesDayInput(EnvrnNum)%DailyDBRange)
    IF (DesDayInput(EnvrnNum)%HumIndType /= DDHumIndType_RelHumSch) THEN
      CALL PreDefTableEntry(pdchDDhumid,envTitle,DesDayInput(EnvrnNum)%HumIndValue)
    ELSE
      CALL PreDefTableEntry(pdchDDhumid,envTitle,'N/A')
    ENDIF
    CALL PreDefTableEntry(pdchDDhumTyp,envTitle,HumidityIndicatingType(DesDayInput(EnvrnNum)%HumIndType))
    CALL PreDefTableEntry(pdchDDwindSp,envTitle,DesDayInput(EnvrnNum)%WindSpeed)
    CALL PreDefTableEntry(pdchDDwindDr,envTitle,DesDayInput(EnvrnNum)%WindDir)
  End Do
Return

END SUBROUTINE GetDesignDayData


SUBROUTINE GetLocationInfo(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   October 1997
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the location info from the IDF file; latitude,
          !  longitude and time zone number.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
  USE DataIPShortCuts

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

         ! SUBROUTINE PARAMETER DEFINITIONS:
         ! na

         ! INTERFACE BLOCK SPECIFICATIONS:
         ! na

         ! DERIVED TYPE DEFINITIONS:
         ! na

         ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: LocNumAlpha ! Number of alpha names being passed
  INTEGER :: LocNumProp  ! Number of properties being passed
  INTEGER :: IOStat           ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(1) :: LocNames ! Temp Array to transfer location info
  REAL(r64), DIMENSION(4) :: LocProps !Temporary array to transfer location info
  INTEGER NumLocations

         ! FLOW:
  cCurrentModuleObject='Site:Location'
  NumLocations=GetNumObjectsFound(cCurrentModuleObject)

  IF (NumLocations > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound=.true.
  ENDIF

  IF (NumLocations == 1) THEN
      !Call Input Get routine to retrieve Location information
    CALL GetObjectItem(cCurrentModuleObject,1,LocNames,LocNumAlpha, &
                       LocProps,LocNumProp,IOSTAT)

   !set latitude, longitude, and time zone number variables
   LocationTitle = LocNames(1)
   Latitude = LocProps(1)
   Longitude= LocProps(2)
   TimeZoneNumber = LocProps(3)
   Elevation=LocProps(4)
   LocationGathered=.true.
  ENDIF

Return

END SUBROUTINE GetLocationInfo

SUBROUTINE GetWeatherProperties(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Weather properties are an advanced concept for simulation.  Primarily, these properties are
          ! used in the test suite runs that have specific requirements for certain properties (such as
          ! sky temperature).

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! WeatherProperty:SkyTemperature,
          !        \memo This object is used to override internal sky temperature calculations.
          !   A1,  \field Name
          !        \reference DesignDays
          !        \note leave blank for RunPeriods (until we name them)
          !        \note This field references the applicable design day or runperiod(s) if left blank.
          !   A2,  \field Calculation Type
          !        \type choice
          !        \key ScheduleValue
          !        \key DifferenceScheduleDryBulbValue
          !        \key DifferenceScheduleDewPointValue
          !        \key AlgorithmA
          !   A3;  \field Schedule Name
          !        \type object-list
          !        \object-list DayScheduleNames
          !        \object-list ScheduleNames

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, SameString, FindItemInList, VerifyName
  USE ScheduleManager, ONLY: GetScheduleIndex,GetDayScheduleIndex
  USE DataIPShortCuts
  USE General, ONLY: FindNumberInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetWeatherProperties:'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Item
  INTEGER :: IOSTAT
  INTEGER :: NumAlpha
  INTEGER :: NumNumerics
  LOGICAL :: IsNotOk
  LOGICAL :: IsBlank
  INTEGER :: Found
  INTEGER :: envFound
  INTEGER :: Count
  INTEGER :: schPtr
  LOGICAL :: MultipleEnvironments
  CHARACTER(len=15) :: units

  cCurrentModuleObject='WeatherProperty:SkyTemperature'
  NumWPSkyTemperatures=GetNumObjectsFound(cCurrentModuleObject)

  ALLOCATE(WPSkyTemperature(NumWPSkyTemperatures))  ! by default, not used.

  DO Item=1,NumWPSkyTemperatures
    MultipleEnvironments=.false.
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlpha,rNumericArgs,NumNumerics,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    SELECT CASE (cAlphaArgs(1))
      CASE (' ')
        Found=0
        DO Count=1,NumOfEnvrn
          IF (Environment(Count)%KindOfEnvrn /= ksRunPeriodWeather) CYCLE
          IF (Environment(Count)%WP_Type1 /= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
               '", indicated Environment Name already assigned.')
            IF (Environment(Count)%Title /= Blank) THEN
              CALL ShowContinueError('...Environment="'//trim(Environment(Count)%Title)//  &
                  '", already using '//trim(cCurrentModuleObject)//'="'//  &
                  trim(WPSkyTemperature(Environment(Count)%WP_Type1)%Name)//'".')
            ELSE
              CALL ShowContinueError('... Runperiod Environment, already using '//trim(cCurrentModuleObject)//'="'//  &
                  trim(WPSkyTemperature(Environment(Count)%WP_Type1)%Name)//'".')
            ENDIF
            ErrorsFound=.true.
          ELSE
            Environment(Count)%WP_Type1=Item
            Found=Count
          ENDIF
        ENDDO
        MultipleEnvironments=.true.
        IF (Found == 0) THEN
          CALL ShowWarningError('GetWeatherProperties: WeatherProperty:SkyTemperature=blank, no run periods found.')
          CALL ShowContinueError('...SkyTemperature will not be applied.')
          CYCLE
        ENDIF
      CASE DEFAULT ! really a name
        Found=FindItemInList(cAlphaArgs(1),Environment%Title,NumOfEnvrn)
        envFound=Found
        IF (Found == 0) THEN
          CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
             '", invalid Environment Name referenced.')
          CALL ShowContinueError('...remainder of object not processed.')
          ErrorsFound=.true.
          CYCLE
        ELSE
          IF (Environment(Found)%WP_Type1 /= 0) THEN
            CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
               '", indicated Environment Name already assigned.')
            CALL ShowContinueError('...Environment="'//trim(Environment(Found)%Title)//  &
                '", already using '//trim(cCurrentModuleObject)//'="'//  &
                trim(WPSkyTemperature(Environment(Found)%WP_Type1)%Name)//'".')
            ErrorsFound=.true.
          ELSE
            Environment(Found)%WP_Type1=Item
          ENDIF
        ENDIF
    END SELECT

    IF (.not. lAlphaFieldBlanks(1)) THEN
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),WPSkyTemperature%Name,Item-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      WPSkyTemperature(Item)%Name = cAlphaArgs(1)            ! Name
    ELSE
      WPSkyTemperature(Item)%Name = 'All RunPeriods'
    ENDIF
    ! Validate Calculation Type.
    IF (SameString(cAlphaArgs(2),'ScheduleValue')) THEN
      WPSkyTemperature(Item)%CalculationType=WP_ScheduleValue
      WPSkyTemperature(Item)%IsSchedule=.true.
      units='[C]'
    ELSEIF (SameString(cAlphaArgs(2),'DifferenceScheduleDryBulbValue')) THEN
      WPSkyTemperature(Item)%CalculationType=WP_DryBulbDelta
      WPSkyTemperature(Item)%IsSchedule=.true.
      units='[deltaC]'
    ELSEIF (SameString(cAlphaArgs(2),'DifferenceScheduleDewPointValue')) THEN
      WPSkyTemperature(Item)%CalculationType=WP_DewPointDelta
      WPSkyTemperature(Item)%IsSchedule=.true.
      units='[deltaC]'
    ELSE
      CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
        '", invalid '//trim(cAlphaFieldNames(2))//'.')
      CALL ShowContinueError('...entered value="'//trim(cAlphaArgs(2))//'", should be one of: '//  &
        'ScheduleValue, DifferenceScheduleDryBulbValue, DifferenceScheduleDewPointValue.')
      ErrorsFound=.true.
    ENDIF

    WPSkyTemperature(Item)%ScheduleName=cAlphaArgs(3)
    IF (Environment(Found)%KindOfEnvrn == ksRunPeriodWeather .or.   &
        Environment(Found)%KindOfEnvrn == ksRunPeriodDesign) THEN
      WPSkyTemperature(Item)%ScheduleName=cAlphaArgs(3)
      ! See if it's a schedule.
      Found=GetScheduleIndex(cAlphaArgs(3))
      IF (Found == 0) THEN
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
          '", invalid '//trim(cAlphaFieldNames(3))//'.')
        CALL ShowContinueError('...Entered name="'//trim(cAlphaArgs(3))//'".')
        CALL ShowContinueError('...Should be a full year schedule ("Schedule:Year", "Schedule:Compact",'//  &
           ' "Schedule:File", or "Schedule:Constant" objects.')
        ErrorsFound=.true.
      ELSE
        WPSkyTemperature(Item)%IsSchedule=.true.
        WPSkyTemperature(Item)%SchedulePtr=Found
      ENDIF
    ELSE      ! See if it's a valid schedule.
      Found=GetDayScheduleIndex(cAlphaArgs(3))
      IF (Found == 0) THEN
        CALL ShowSevereError(RoutineName//trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//  &
          '", invalid '//trim(cAlphaFieldNames(3))//'.')
        CALL ShowContinueError('...Entered name="'//trim(cAlphaArgs(3))//'".')
        CALL ShowContinueError('...Should be a single day schedule ("Schedule:Day:Hourly",'//  &
           ' "Schedule:Day:Interval", or "Schedule:Day:List" objects.')
        ErrorsFound=.true.
      ELSE
        IF (envFound /= 0) THEN
          schPtr=FindNumberInList(Found,SPSiteScheduleNamePtr,NumSPSiteScheduleNamePtrs)
          IF (schPtr == 0) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=Found
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Sky Temperature Schedule Value '//units,  &
                 SPSiteSkyTemperatureScheduleValue(envFound),'Zone','Average',cAlphaArgs(3))
          ELSEIF (SPSiteScheduleUnits(schPtr)/= units) THEN
            NumSPSiteScheduleNamePtrs=NumSPSiteScheduleNamePtrs+1
            SPSiteScheduleNamePtr(NumSPSiteScheduleNamePtrs)=Found
            SPSiteScheduleUnits(NumSPSiteScheduleNamePtrs)=units
            CALL SetupOutputVariable('Sizing Period Site Sky Temperature Schedule Value '//units,  &
                 SPSiteSkyTemperatureScheduleValue(envFound),'Zone','Average',cAlphaArgs(3))
          ENDIF
          WPSkyTemperature(Item)%IsSchedule=.true.
          WPSkyTemperature(Item)%SchedulePtr=Found
        ENDIF
      ENDIF
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE GetWeatherProperties

SUBROUTINE GetGroundTemps(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   October 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This file reads the Ground Temps from the input file and puts them
          !  in a new variable.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: GndNumAlpha ! Number of construction alpha names being passed
  INTEGER :: GndNumProp  ! dummy variable for properties being passed
  INTEGER :: IOStat      ! IO Status when calling get input subroutine
  Integer :: I           ! Loop counter variable
  CHARACTER(len=MaxNameLength),DIMENSION(1) :: GndAlphas ! Construction Alpha names defined
  REAL(r64), DIMENSION(12) :: GndProps !Temporary array to transfer ground temperatures
  LOGICAL :: GenErrorMessage=.false.

     ! FLOW:
  cCurrentModuleObject='Site:GroundTemperature:BuildingSurface'
  I=GetNumObjectsFound(cCurrentModuleObject)
  IF (I == 1) THEN
     !Get the object names for each construction from the input processor
     CALL GetObjectItem(cCurrentModuleObject,1,GndAlphas,GndNumAlpha, &
                      GndProps,GndNumProp,IOSTAT)

    IF (GndNumProp < 12) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Less than 12 values entered.')
      ErrorsFound=.true.
    ENDIF

    !Assign the ground temps to the variable
    Do I=1,12
      GroundTemps(I) = GndProps(I)
      IF (GroundTemps(I) < 15.d0 .or. GroundTemps(I) > 25.d0) GenErrorMessage=.true.
    End Do

    GroundTempObjInput=.true.

  ELSEIF (I > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound=.true.
  ELSE
    GroundTemps=18.d0
  ENDIF

  IF (GenErrorMessage) THEN
    CALL ShowWarningError(TRIM(cCurrentModuleObject)//': Some values fall outside the range of 15-25C.')
    CALL ShowContinueError('These values may be inappropriate.  Please consult the Input Output Reference for more details.')
  ENDIF

  ! Write Final Ground Temp Information to the initialization output file
  Write(OutputFileInits,'(A)') '! <Site:GroundTemperature:BuildingSurface>, Months From Jan to Dec {C}'
  Write(OutputFileInits,720) 'Site:GroundTemperature:BuildingSurface',(GroundTemps(I),I=1,12)
  720 Format(' ',A,12(', ',F6.2))


  !Added for ground temperatures for F and C factor defined surfaces
  cCurrentModuleObject='Site:GroundTemperature:FCfactorMethod'
  I=GetNumObjectsFound(cCurrentModuleObject)
  IF (I == 1) THEN
     CALL GetObjectItem(cCurrentModuleObject,1,GndAlphas,GndNumAlpha,GndProps,GndNumProp,IOSTAT)

    IF (GndNumProp < 12) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Less than 12 values entered.')
      ErrorsFound=.true.
    ENDIF

    FCGroundTemps=.true.
    ! overwrite values read from weather file for the 0.5m set ground temperatures
    Do I=1,12
      GroundTempsFC(I) = GndProps(I)
    End Do

  ELSEIF (I > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound=.true.

  ELSEIF (wthFCGroundTemps) THEN
    FCGroundTemps=.true.
  ENDIF

  IF (FCGroundTemps) THEN  ! Write Ground Temp Information to the initialization output file
    Write(OutputFileInits,'(A)') '! <Site:GroundTemperature:FCfactorMethod>, Months From Jan to Dec {C}'
    Write(OutputFileInits,720) 'Site:GroundTemperature:FCfactorMethod',(GroundTempsFC(I),I=1,12)
  ENDIF


  PubGroundTempSurfFlag=.FALSE.
  cCurrentModuleObject='Site:GroundTemperature:Shallow'
  I=GetNumObjectsFound(cCurrentModuleObject)
  IF (I == 1) THEN
     !Get the object names for each construction from the input processor
     CALL GetObjectItem(cCurrentModuleObject,1,GndAlphas,GndNumAlpha, &
                      GndProps,GndNumProp,IOSTAT)

    IF (GndNumProp < 12) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Less than 12 values entered.')
      ErrorsFound=.true.
    ENDIF

    !Assign the ground temps to the variable
    Do I=1,12
      SurfaceGroundTemps(I) = GndProps(I)
    End Do

    GroundTemp_SurfaceObjInput=.true.

  ELSEIF (I > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound=.true.
  ELSE
    SurfaceGroundTemps=13.d0
  ENDIF

  ! Write Final Ground Temp Information to the initialization output file
  Write(OutputFileInits,'(A)') '! <Site:GroundTemperature:Shallow>, Months From Jan to Dec {C}'
  Write(OutputFileInits,720) 'Site:GroundTemperature:Shallow',(SurfaceGroundTemps(I),I=1,12)

  cCurrentModuleObject='Site:GroundTemperature:Deep'
  I=GetNumObjectsFound(cCurrentModuleObject)
  IF (I == 1) THEN
     !Get the object names for each construction from the input processor
     CALL GetObjectItem(cCurrentModuleObject,1,GndAlphas,GndNumAlpha, &
                      GndProps,GndNumProp,IOSTAT)

    IF (GndNumProp < 12) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Less than 12 values entered.')
      ErrorsFound=.true.
    ENDIF

    !Assign the ground temps to the variable
    Do I=1,12
      DeepGroundTemps(I) = GndProps(I)
    End Do

    GroundTemp_DeepObjInput=.true.

  ELSEIF (I > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound=.true.
  ELSE
    DeepGroundTemps=16.d0
  ENDIF

  ! Write Final Ground Temp Information to the initialization output file
  Write(OutputFileInits,'(A)') '! <Site:GroundTemperature:Deep>, Months From Jan to Dec {C}'
  Write(OutputFileInits,720) 'Site:GroundTemperature:Deep',(DeepGroundTemps(I),I=1,12)

  !Assigning the ground temperature array to a public array for use in other subroutines
  !Main use is for PlantPipeHeatTransfer, where the buried pipe model needs to average
  ! a full year's worth of data at the beginning of the simulation
  IF(GroundTemp_SurfaceObjInput) THEN
    PubGroundTempSurfFlag=.TRUE.
    DO I = 1, 12
      PubGroundTempSurface(I) = SurfaceGroundTemps(I)
    END DO
  END IF

  RETURN

END SUBROUTINE GetGroundTemps

SUBROUTINE GetGroundReflectances(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This file reads the Ground Reflectances from the input file (optional) and
          ! places them in the monthly array.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: GndNumAlpha ! Number of construction alpha names being passed
  INTEGER :: GndNumProp  ! dummy variable for properties being passed
  INTEGER :: IOStat      ! IO Status when calling get input subroutine
  Integer :: I           ! Loop counter variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: GndAlphas ! Construction Alpha names defined
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: GndProps                          !Temporary array to transfer ground reflectances

     ! FLOW:
  cCurrentModuleObject='Site:GroundReflectance'
  I=GetNumObjectsFound(cCurrentModuleObject)
  IF (I /=0) THEN
    ALLOCATE(GndProps(12))
    ALLOCATE(GndAlphas(1))
    IF (I == 1) THEN
       !Get the object names for each construction from the input processor
       CALL GetObjectItem(cCurrentModuleObject,1,GndAlphas,GndNumAlpha, &
                        GndProps,GndNumProp,IOSTAT)

      IF (GndNumProp < 12) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Less than 12 values entered.')
        ErrorsFound=.true.
      ENDIF

      !Assign the ground reflectances to the variable
      GroundReflectances(1:12) = GndProps(1:12)

    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
      ErrorsFound=.true.
    ENDIF
    DEALLOCATE(GndProps)
    DEALLOCATE(GndAlphas)
  ENDIF

  ! Write Final Ground Reflectance Information to the initialization output file
  Write(OutputFileInits,'(A)') '! <Site:GroundReflectance>, Months From Jan to Dec {dimensionless}'
  Write(OutputFileInits,720) (GroundReflectances(I),I=1,12)
  720 Format(' Site:GroundReflectance',12(', ',F5.2))

  RETURN

END SUBROUTINE GetGroundReflectances

SUBROUTINE GetSnowGroundRefModifiers(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This file reads the Snow Ground Reflectance Modifiers from the input file (optional) and
          ! places them in the variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: GndNumAlpha ! Number of construction alpha names being passed
  INTEGER :: GndNumProp  ! dummy variable for properties being passed
  INTEGER :: IOStat      ! IO Status when calling get input subroutine
  Integer :: I           ! Loop counter variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: GndAlphas ! Construction Alpha names defined
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: GndProps                          !Temporary array to transfer ground reflectances

     ! FLOW:
  cCurrentModuleObject='Site:GroundReflectance:SnowModifier'
  I=GetNumObjectsFound(cCurrentModuleObject)
  IF (I /=0) THEN
    ALLOCATE(GndProps(2))
    ALLOCATE(GndAlphas(1))
    IF (I == 1) THEN
       !Get the object names for each construction from the input processor
       CALL GetObjectItem(cCurrentModuleObject,1,GndAlphas,GndNumAlpha, &
                        GndProps,GndNumProp,IOSTAT)

      !Assign the ground reflectances to the variable
      SnowGndRefModifier=GndProps(1)
      SnowGndRefModifierForDayltg=GndProps(2)

    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
      ErrorsFound=.true.
    ENDIF
    DEALLOCATE(GndProps)
    DEALLOCATE(GndAlphas)
  ENDIF

  ! Write Final Ground Reflectance Modifier Information to the initialization output file
  Write(OutputFileInits,'(A)') '! <Site:GroundReflectance:SnowModifier>, Normal, Daylighting {dimensionless}'
  Write(OutputFileInits,720) SnowGndRefModifier,SnowGndRefModifierForDayltg
  720 Format(' Site:GroundReflectance:SnowModifier',2(', ',F7.3))

  Write(OutputFileInits,'(A)') '! <Site:GroundReflectance:Snow>, Months From Jan to Dec {dimensionless}'
  Write(OutputFileInits,721) ' Site:GroundReflectance:Snow',(MAX(MIN(GroundReflectances(I)*SnowGndRefModifier,1.0d0),0.0d0),I=1,12)
  Write(OutputFileInits,'(A)') '! <Site:GroundReflectance:Snow:Daylighting>, Months From Jan to Dec {dimensionless}'
  Write(OutputFileInits,721) ' Site:GroundReflectance:Snow:Daylighting',  &
                              (MAX(MIN(GroundReflectances(I)*SnowGndRefModifierForDayltg,1.0d0),0.0d0),I=1,12)
  721 Format(A,12(', ',F5.2))
  RETURN

END SUBROUTINE GetSnowGroundRefModifiers


SUBROUTINE GetWaterMainsTemperatures(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reads the input data for the WATER MAINS TEMPERATURES object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, SameString
  USE ScheduleManager, ONLY: GetScheduleIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)                    :: ErrorsFound

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                   :: NumObjects
  INTEGER                                   :: NumAlphas ! Number of elements in the alpha array
  INTEGER                                   :: NumNums   ! Number of elements in the numeric array
  INTEGER                                   :: IOStat    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(2) :: AlphArray ! Character string data
  REAL(r64), DIMENSION(2)                        :: NumArray  ! Numeric data

     ! FLOW:
  cCurrentModuleObject='Site:WaterMainsTemperature'
  NumObjects = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumObjects == 1) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,AlphArray,NumAlphas,NumArray,NumNums,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IF (SameString(AlphArray(1),'Schedule')) THEN
      WaterMainsTempsMethod = ScheduleMethod

      WaterMainsTempsSchedule = GetScheduleIndex(AlphArray(2))
      IF (WaterMainsTempsSchedule .EQ. 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           '='//TRIM(AlphArray(2)))
        ErrorsFound = .TRUE.
      END IF

    ELSE IF (SameString(AlphArray(1),'Correlation')) THEN
      WaterMainsTempsMethod = CorrelationMethod

      IF (NumNums == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Missing Annual Average and Maximum Difference fields.')
        ErrorsFound = .TRUE.
      ELSE IF (NumNums == 1) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Missing Maximum Difference field.')
        ErrorsFound = .TRUE.
      ELSE
        WaterMainsTempsAnnualAvgAirTemp = NumArray(1)
        WaterMainsTempsMaxDiffAirTemp = NumArray(2)
      END IF

    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
           '='//TRIM(AlphArray(1)))
      ErrorsFound = .TRUE.
    END IF

  ELSE IF (NumObjects > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound = .TRUE.
  END IF

  RETURN

END SUBROUTINE GetWaterMainsTemperatures


SUBROUTINE CalcWaterMainsTemp()

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the daily water mains temperature based on input data from the WATER MAINS TEMPERATURES object.

          ! METHODOLOGY EMPLOYED:
          ! Water mains temperature is either taken from a schedule or calculated by a correlation.  The correlation
          ! is fit to Fahrenheit units, so the air temperature values are first convert to F, then mains temperature
          ! is calculated and converted back to C.

          ! REFERENCES:
          ! Correlation developed by Jay Burch and Craig Christensen at NREL, described in:
          ! Hendron, R., Anderson, R., Christensen, C., Eastment, M., and Reeves, P.  2004.  "Development of an Energy
          ! Savings Benchmark for All Residential End-Uses", Proceedings of SimBuild 2004, IBPSA-USA National Conference,
          ! Boulder, CO, August 4 - 6, 2004.

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tavg    ! Annual Average Outdoor Air Temperature (F)
  REAL(r64) :: Tdiff   ! Maximum difference in monthly average outdoor air temperatures (deltaF)
  REAL(r64) :: Ratio   ! Value used in correlation
  REAL(r64) :: Lag     ! Value used in correlation
  REAL(r64) :: Offset  ! Value used in correlation

     ! FLOW:
  SELECT CASE (WaterMainsTempsMethod)

    CASE (ScheduleMethod)
      WaterMainsTemp = GetCurrentScheduleValue(WaterMainsTempsSchedule)

    CASE (CorrelationMethod)
      ! Convert C to F
      Tavg = WaterMainsTempsAnnualAvgAirTemp * (9.0d0 / 5.0d0) + 32.0d0
      Tdiff = WaterMainsTempsMaxDiffAirTemp * (9.0d0 / 5.0d0)

      Ratio = 0.4d0 + 0.01d0 * (Tavg - 44.0d0)
      Lag = 35.0d0 - 1.0d0 * (Tavg - 44.0d0)
      Offset = 6.0d0

      WaterMainsTemp = Tavg + Offset + Ratio * (Tdiff / 2.0d0) *   &
         SIN((0.986d0 * (DayOfYear - 15.0d0 - Lag) - 90.0d0) * DegToRadians)

      IF (WaterMainsTemp < 32.0d0) WaterMainsTemp = 32.0d0

      ! Convert F to C
      WaterMainsTemp = (WaterMainsTemp - 32.0d0) * (5.0d0 / 9.0d0)

    CASE DEFAULT
      WaterMainsTemp = 10.0d0  ! 50 F

  END SELECT

  RETURN

END SUBROUTINE CalcWaterMainsTemp

SUBROUTINE GetWeatherStation(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reads the input data for the WEATHER STATION object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)                    :: ErrorsFound

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                   :: NumObjects
  INTEGER                                   :: NumAlphas ! Number of elements in the alpha array
  INTEGER                                   :: NumNums   ! Number of elements in the numeric array
  INTEGER                                   :: IOStat    ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(1) :: AlphArray ! Character string data
  REAL(r64), DIMENSION(4)                        :: NumArray  ! Numeric data
  REAL(r64) :: WeatherFileWindSensorHeight  ! Height of the wind sensor at the weather station, i.e., weather file
  REAL(r64) :: WeatherFileWindExp           ! Exponent for the wind velocity profile at the weather station
  REAL(r64) :: WeatherFileWindBLHeight      ! Boundary layer height for the wind velocity profile at the weather station (m)
  REAL(r64) :: WeatherFileTempSensorHeight  ! Height of the air temperature sensor at the weather station (m)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

     ! FLOW:
  cCurrentModuleObject='Site:WeatherStation'
  NumObjects = GetNumObjectsFound(cCurrentModuleObject)

  ! Default conditions for a weather station in an open field at a height of 10 m. (These should match the IDD defaults.)
  WeatherFileWindSensorHeight = 10.0d0
  WeatherFileWindExp = 0.14d0
  WeatherFileWindBLHeight = 270.0d0
  WeatherFileTempSensorHeight = 1.5d0

  IF (NumObjects == 1) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,AlphArray,NumAlphas,NumArray,NumNums,IOStat)

    IF (NumNums > 0) WeatherFileWindSensorHeight = NumArray(1)
    IF (NumNums > 1) WeatherFileWindExp = NumArray(2)
    IF (NumNums > 2) WeatherFileWindBLHeight = NumArray(3)
    IF (NumNums > 3) WeatherFileTempSensorHeight = NumArray(4)

  ELSE IF (NumObjects > 1) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered. Only one allowed.')
    ErrorsFound = .TRUE.
  END IF

  WeatherFileWindModCoeff = (WeatherFileWindBLHeight / WeatherFileWindSensorHeight) ** WeatherFileWindExp
  WeatherFileTempModCoeff = AtmosphericTempGradient * EarthRadius * WeatherFileTempSensorHeight /   &
                                  (EarthRadius + WeatherFileTempSensorHeight)

  ! Write to the initialization output file
  WRITE(OutputFileInits,'(A)') '! <Environment:Weather Station>,Wind Sensor Height Above Ground {m},'// &
    'Wind Speed Profile Exponent {},Wind Speed Profile Boundary Layer Thickness {m},'// &
    'Air Temperature Sensor Height Above Ground {m},Wind Speed Modifier Coefficient [Internal],'//  &
    'Temperature Modifier Coefficient [Internal]'

  WRITE(OutputFileInits,720) TRIM(RoundSigDigits(WeatherFileWindSensorHeight,3)), TRIM(RoundSigDigits(WeatherFileWindExp,3)),   &
     TRIM(RoundSigDigits(WeatherFileWindBLHeight,3)),TRIM(RoundSigDigits(WeatherFileTempSensorHeight,3)),  &
     TRIM(RoundSigDigits(WeatherFileWindModCoeff,3)),TRIM(RoundSigDigits(WeatherFileTempModCoeff,3))

720 FORMAT('Environment:Weather Station',6(',',A))

  RETURN

END SUBROUTINE GetWeatherStation


SUBROUTINE DayltgCurrentExtHorizIllum

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       Nov98 (FW); Nov 2000 (FW)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! CALCULATES EXTERIOR DAYLIGHT ILLUMINANCE AND LUMINOUS EFFICACY

          ! METHODOLOGY EMPLOYED:
          ! CALLED by SetCurrentWeather.
          ! CALCULATES THE CURRENT-TIME-STEP
          ! ILLUMINANCE ON AN UNOBSTRUCTED HORIZONTAL SURFACE FROM THE
          ! THE SKY AND FROM DIRECT SUN.

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DEXTIL.

          ! USE STATEMENTS:
          ! na

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

REAL(r64)   :: SDIRH                   ! Exterior horizontal beam irradiance (W/m2)
REAL(r64)   :: SDIFH                   ! Exterior horizontal sky diffuse irradiance (W/m2)
!REAL(r64)   :: PDIRLW                  ! Luminous efficacy (lum/W) of beam solar radiation
!REAL(r64)   :: PDIFLW                  ! Luminous efficacy (lum/W) of sky diffuse solar radiation

!              DIRECT AND DIFFUSE HORIZONTAL SOLAR IRRADIANCE (W/M2).
!              SOLCOS(3), below, is the cosine of the solar zenith angle.
!
      IF(SunIsUp) THEN
        SDIRH  = BeamSolarRad * SOLCOS(3)
        SDIFH  = DifSolarRad
!              Fraction of sky covered by clouds
        CloudFraction = (SDIFH/(SDIRH+SDIFH+0.0001d0))**2
!
!              Luminous efficacy of sky diffuse solar and beam solar (lumens/W);
!              Horizontal illuminance from sky and horizontal beam illuminance (lux)
!              obtained from solar quantities on weather file and luminous efficacy.

        CALL DayltgLuminousEfficacy (PDIFLW,PDIRLW)
        HISKF = SDIFH * PDIFLW
        HISUNF = SDIRH * PDIRLW
        HISUNFnorm = BeamSolarRad * PDIRLW
      ELSE
        SDIRH = 0.d0
        SDIFH = 0.d0
        CloudFraction = 0.d0
        PDIFLW = 0.d0
        PDIRLW = 0.d0
        HISKF = 0.d0
        HISUNF = 0.d0
        HISUNFnorm = 0.d0
        SkyClearness = 0.d0
        SkyBrightness = 0.d0
      END IF

      RETURN
END SUBROUTINE DayltgCurrentExtHorizIllum

SUBROUTINE DayltgLuminousEfficacy (DiffLumEff,DirLumEff)
!
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       August 2009, BG fixed upper bound for sky clearness bin 7
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Uses diffuse horizontal solar irradiance, direct normal solar
          ! irradiance, atmospheric moisture and sun position
          ! to determine the luminous efficacy in lumens/watt
          ! of sky diffuse solar radiation and direct normal solar radiation.
          ! Based on an empirical method described in
          ! R. Perez, P. Ineichen, R. Seals, J. Michalsky and R. Stewart,
          ! "Modeling daylight availability and irradiance components from direct
          ! global irradiance components from direct and global irradiance,"
          ! Solar Energy 44 (1990) 271-289.

          ! Called by DayltgCurrentExtHorizIllum.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER, DIMENSION(8) :: ADiffLumEff=  &         ! Diffuse luminous efficacy coefficients
      (/97.24d0, 107.22d0, 104.97d0, 102.39d0, 100.71d0, 106.42d0, 141.88d0, 152.23d0/)
REAL(r64), PARAMETER, DIMENSION(8) :: BDiffLumEff=  &
      (/-0.46d0, 1.15d0,   2.96d0,   5.59d0,   5.94d0,   3.83d0,   1.90d0,   0.35d0  /)
REAL(r64), PARAMETER, DIMENSION(8) :: CDiffLumEff=  &
      (/12.00d0, 0.59d0,   -5.53d0,  -13.95d0, -22.75d0, -36.15d0, -53.24d0, -45.27d0/)
REAL(r64), PARAMETER, DIMENSION(8) :: DDiffLumEff=  &
      (/-8.91d0, -3.95d0,  -8.77d0,  -13.90d0, -23.74d0, -28.83d0, -14.03d0, -7.98d0 /)
REAL(r64), PARAMETER, DIMENSION(8) :: ADirLumEff=   &         ! Direct luminous efficacy coefficients
      (/57.20d0, 98.99d0,  109.83d0, 110.34d0, 106.36d0, 107.19d0, 105.75d0, 101.18d0/)
REAL(r64), PARAMETER, DIMENSION(8) :: BDirLumEff=   &
      (/-4.55d0, -3.46d0,  -4.90d0,  -5.84d0,  -3.97d0,  -1.25d0,  0.77d0,   1.58d0  /)
REAL(r64), PARAMETER, DIMENSION(8) :: CDirLumEff=   &
      (/-2.98d0, -1.21d0,  -1.71d0,  -1.99d0,  -1.75d0,  -1.51d0,  -1.26d0,  -1.10d0 /)
REAL(r64), PARAMETER, DIMENSION(8) :: DDirLumEff=   &
      (/117.12d0,12.38d0,  -8.81d0,  -4.56d0,  -6.16d0,  -26.73d0, -34.44d0, -8.29d0 /)
REAL(r64), PARAMETER, DIMENSION(12) :: ExtraDirNormIll=  &     ! Monthly exterrestrial direct normal illuminance (lum/m2)
      (/131153.d0,130613.d0,128992.d0,126816.d0,124731.d0,123240.d0, &
        122652.d0,123120.d0,124576.d0,126658.d0,128814.d0,130471.d0/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)   :: DirLumEff               ! Luminous efficacy of beam solar radiation (lum/W)
REAL(r64)   :: DiffLumEff              ! Luminous efficacy of sky diffuse solar radiation (lum/W)
REAL(r64)   :: SunZenith               ! Solar zenith angle (radians)
REAL(r64)   :: SunAltitude             ! Solar altitude angle (radians)
REAL(r64)   :: SinSunAltitude          ! Sine of the solar altitude angle
REAL(r64)   :: Zeta
INTEGER     :: ISkyClearness           ! Sky clearness bin
REAL(r64)   :: AirMass                 ! Relative optical air mass
REAL(r64)   :: AtmosMoisture           ! Atmospheric moisture (cm of precipitable water)


          ! FLOW:
!


!
     SunZenith = ACOS (SOLCOS(3))
     SunAltitude = PiOvr2 - SunZenith
     SinSunAltitude = SIN(SunAltitude)
!
!              Clearness of sky. SkyClearness close to 1.0 corresponds to an overcast sky.
!              SkyClearness > 6 is a clear sky.
!              DifSolarRad is the diffuse horizontal irradiance.
!              BeamSolarRad is the direct normal irradiance.
!
      Zeta = 1.041d0*SunZenith**3
      SkyClearness = ( (DifSolarRad + BeamSolarRad)/(DifSolarRad + 0.0001d0) + Zeta )/(1.0d0+Zeta)
      AirMass = (1.d0-0.1d0*Elevation/1000.d0) / (SinSunAltitude + 0.15d0/(SunAltitude/DegToRadians + 3.885d0)**1.253d0)
!
!              In the following, 93.73 is the extraterrestrial luminous efficacy
!
      SkyBrightness = (DifSolarRad * 93.73d0)* AirMass / ExtraDirNormIll(Month)
!
      IF(SkyClearness.LE.1.065d0) THEN
         ISkyClearness = 1
      ELSE IF(SkyClearness.GT.1.065d0.AND.SkyClearness.LE.1.23d0) THEN
         ISkyClearness = 2
      ELSE IF(SkyClearness.GT.1.23d0.AND.SkyClearness.LE.1.50d0) THEN
         ISkyClearness = 3
      ELSE IF(SkyClearness.GT.1.50d0.AND.SkyClearness.LE.1.95d0) THEN
         ISkyClearness = 4
      ELSE IF(SkyClearness.GT.1.95d0.AND.SkyClearness.LE.2.80d0) THEN
         ISkyClearness = 5
      ELSE IF(SkyClearness.GT.2.80d0.AND.SkyClearness.LE.4.50d0) THEN
         ISkyClearness = 6
      ELSE IF(SkyClearness.GT.4.50d0.AND.SkyClearness.LE.6.20d0) THEN
         ISkyClearness = 7
      ELSE
         ISkyClearness = 8
      END IF
!
      AtmosMoisture = EXP (0.07d0*OutDewPointTemp - 0.075d0)
!
!              Sky diffuse luminous efficacy
!
      IF(SkyBrightness.LE.0.0d0) THEN
         DiffLumEff = 0.d0
      ELSE
         DiffLumEff = ADiffLumEff(ISkyClearness) + BDiffLumEff(ISkyClearness)*AtmosMoisture + &
                      CDiffLumEff(ISkyClearness)*SOLCOS(3) + &
                      DDiffLumEff(ISkyClearness)*LOG(SkyBrightness)
      ENDIF
!
!              Direct normal luminous efficacy
!
      IF(SkyBrightness.LE.0.d0) THEN
         DirLumEff = 0.d0
      ELSE
         DirLumEff = MAX(0.d0,ADirLumEff(ISkyClearness) + BDirLumEff(ISkyClearness)*AtmosMoisture + &
                       CDirLumEff(ISkyClearness)*EXP(5.73d0*SunZenith-5.d0) + &
                       DDirLumEff(ISkyClearness)*SkyBrightness)
      ENDIF

      RETURN
END SUBROUTINE DayltgLuminousEfficacy

REAL(r64) Function GetSTM(Longitude)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function determines the "standard time meridian" from the input
          ! longitude. Calculates the proper Meridian from Longitude.  This
          ! value is needed for weather calculations so that the sun comes
          ! up and goes down at the right times.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: Longitude  ! Longitude from user input

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      REAL(r64) longl(-12:12)  ! Lower Longitude value for a Time Zone
      REAL(r64) longh(-12:12)  ! Upper Longitude value for a Time Zone
      INTEGER i  ! Loop variable
      REAL(r64) temp  ! temporary value used to determine time zone
      REAL(r64) tz    ! resultant tz meridian

      GetSTM=0.0d0
!
      longl(0)=-7.5d0
      longh(0)=7.5d0
      do i=1,12
        longl(i)=longl(i-1)+15.d0
        longh(i)=longh(i-1)+15.d0
      enddo
      do i=1,12
        longl(-i)=longl(-i+1)-15.d0
        longh(-i)=longh(-i+1)-15.d0
      enddo
      temp=Longitude
      temp=mod(temp,360.d0)

      if (temp > 180.d0) temp=temp-180.d0
      do i=-12,12
        if (temp > longl(i) .and. temp <= longh(i)) then
          tz=i
          tz=mod(tz,24.d0)
          GetSTM=tz
          exit
        endif
      enddo

      RETURN
!
END FUNCTION GetSTM

SUBROUTINE ProcessEPWHeader(HeaderString,Line,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes each header line in the EPW weather file.

          ! METHODOLOGY EMPLOYED:
          ! File is positioned to the correct line, then backspaced.  This routine
          ! reads in the line and processes as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: ProcessNumber, FindItemInList, MakeUPPERCase, GetNumObjectsFound, SameString
  USE General, ONLY: JulianDay

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: HeaderString
  CHARACTER(len=*), INTENT(INOUT) :: Line
  LOGICAL, INTENT(INOUT)          :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: AFormat="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=100) :: Title=Blank
  INTEGER :: Count
  CHARACTER(len=20) WMO
  INTEGER Pos
  REAL(r64) Number
  LOGICAL IOStatus
  INTEGER PMonth,Pday,PWeekDay,PYear,DateType
  INTEGER NumHdArgs
  LOGICAL ErrFlag
  CHARACTER(len=20) ErrNum
  INTEGER CurCount
  INTEGER CurOne
  INTEGER NumEPWHolidays
  INTEGER NumGrndTemps
  INTEGER endcol
  INTEGER TropExtremeCount  ! because these can show up as "no dry" need to count and separate.
  INTEGER actcount
  LOGICAL errflag1


  ! Strip off Header value from Line
  Pos=INDEX(Line,',')
  IF (Pos == 0 .and. MakeUPPERCase(HeaderString(1:8)) /= 'COMMENTS') THEN
    CALL ShowSevereError('Invalid Header line in in.epw -- no commas')
    CALL ShowContinueError('Line='//TRIM(Line))
    CALL ShowFatalError('Previous conditions cause termination.')
  ENDIF
  Line=Line(Pos+1:)

  SELECT CASE(MakeUPPERCase(HeaderString))

    CASE ('LOCATION')
  ! LOCATION, A1 [City], A2 [State/Province/Region], A3 [Country],
  ! A4 [Source], N1 [WMO], N2 [Latitude],
  ! N3 [Longitude], N4 [Time Zone], N5 [Elevation {m}]

      NumHdArgs=9
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              IF (StripCR) THEN
                endcol=LEN_TRIM(Line)
                IF (endcol > 0) THEN
                  IF (ICHAR(Line(endcol:endcol)) == iASCII_CR) Line(endcol:endcol)=Blank
                ENDIF
              ENDIF
              Line=ADJUSTL(Line)
              Line=MakeUPPERCase(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

          CASE (1)
            Title=TRIM(Line(1:Pos-1))

          CASE (2,3,4)
            Title=TRIM(Title)//Blank//TRIM(Line(1:Pos-1))

          CASE (5)
            WMO=TRIM(Line(1:Pos-1))
            Title=TRIM(Title)//' WMO#='//TRIM(WMO)

          CASE (6,7,8,9)
            Number=ProcessNumber(Line(1:Pos-1),ErrFlag)
            IF (.not. ErrFlag) THEN
              SELECT CASE (Count)
                CASE (6)
                  WeatherFileLatitude=Number
                CASE (7)
                  WeatherFileLongitude=Number
                CASE (8)
                  WeatherFileTimeZone=Number
                CASE (9)
                  WeatherFileElevation=Number
              END SELECT
            ELSE
              CALL ShowSevereError('GetEPWHeader:LOCATION, invalid numeric='//Line(1:Pos-1))
              ErrorsFound=.true.
            ENDIF
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
      ENDDO
      WeatherFileLocationTitle=ADJUSTL(Title)

    CASE ('DESIGN CONDITIONS')
     ! No action

    CASE ('TYPICAL/EXTREME PERIODS')
      TropExtremeCount=0
      Line=ADJUSTL(Line)
      Pos=INDEX(Line,',')
      IF (Pos == 0) THEN
        IF (LEN_TRIM(Line) == 0) THEN
          DO WHILE (Pos == 0 .and. LEN_TRIM(Line) == 0)
            READ(WeatherFileUnitNumber,AFormat) Line
            Line=ADJUSTL(Line)
            Pos=INDEX(Line,',')
          ENDDO
        ELSE
          Pos=LEN_TRIM(Line)+1
        ENDIF
      ENDIF
      NumEPWTypExtSets=ProcessNumber(Line(1:Pos-1),IOStatus)
      Line=Line(Pos+1:)
      ALLOCATE(TypicalExtremePeriods(NumEPWTypExtSets))
      TropExtremeCount=0
      Count=1
      DO WHILE (Count <= NumEPWTypExtSets)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos /= 0) THEN
          TypicalExtremePeriods(Count)%Title=TRIM(Line(1:Pos-1))
          Line=Line(Pos+1:)
        ELSE
          CALL ShowWarningError('ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)='//  &
                        TRIM(Line(1:Pos-1)))
          CALL ShowContinueError('...on processing Typical/Extreme period #'//TRIM(RoundSigDigits(Count)))
          NumEPWTypExtSets=Count-1
          EXIT
        ENDIF
        Pos=INDEX(Line,',')
        IF (Pos /= 0) THEN
          TypicalExtremePeriods(Count)%TEType=TRIM(Line(1:Pos-1))
          Line=Line(Pos+1:)
          IF (SameString(TypicalExtremePeriods(Count)%TEType,'EXTREME')) THEN
            IF (SameString(TypicalExtremePeriods(Count)%Title(1:36),  &
                           'NO DRY SEASON - WEEK NEAR ANNUAL MAX')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='NoDrySeasonMax'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:36),  &
                              'NO DRY SEASON - WEEK NEAR ANNUAL MIN')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='NoDrySeasonMin'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:36),  &
                           'NO WET SEASON - WEEK NEAR ANNUAL MAX')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='NoWetSeasonMax'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:36),  &
                              'NO WET SEASON - WEEK NEAR ANNUAL MIN')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='NoWetSeasonMin'
            ! to account for problems earlier in weather files:
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'NO DRY')) THEN
              IF (TropExtremeCount == 0) THEN
                TypicalExtremePeriods(Count)%Title='No Dry Season - Week Near Annual Max'
                TypicalExtremePeriods(Count)%ShortTitle='NoDrySeasonMax'
                TropExtremeCount=TropExtremeCount+1
              ELSEIF (TropExtremeCount == 1) THEN
                TypicalExtremePeriods(Count)%Title='No Dry Season - Week Near Annual Min'
                TypicalExtremePeriods(Count)%ShortTitle='NoDrySeasonMin'
                TropExtremeCount=TropExtremeCount+1
              ENDIF
            ELSE  ! make new short titles
              IF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'SUMMER')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='Summer'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'WINTER')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='Winter'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:12),'TROPICAL HOT')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='TropicalHot'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:13),'TROPICAL COLD')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='TropicalCold'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'AUTUMN')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='Autumn'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'NO DRY')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='NoDrySeason'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'NO WET')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='NoWetSeason'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:4),'WET ')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='WetSeason'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:4),'DRY ')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='DrySeason'
              ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:5),'SPRING')) THEN
                TypicalExtremePeriods(Count)%ShortTitle='Spring'
              ENDIF
            ENDIF
          ELSE ! not extreme
            IF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'SUMMER')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='Summer'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'WINTER')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='Winter'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:12),'TROPICAL HOT')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='TropicalHot'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:13),'TROPICAL COLD')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='TropicalCold'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'AUTUMN')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='Autumn'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'NO DRY')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='NoDrySeason'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:6),'NO WET')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='NoWetSeason'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:4),'WET ')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='WetSeason'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:4),'DRY ')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='DrySeason'
            ELSEIF (SameString(TypicalExtremePeriods(Count)%Title(1:5),'SPRING')) THEN
              TypicalExtremePeriods(Count)%ShortTitle='Spring'
            ENDIF
          ENDIF
        ELSE
          CALL ShowWarningError('ProcessEPWHeader: Invalid Typical/Extreme Periods Header(WeatherFile)='//  &
                        TRIM(TypicalExtremePeriods(Count)%Title)//Blank//TRIM(Line(1:Pos-1)))
          CALL ShowContinueError('...on processing Typical/Extreme period #'//TRIM(RoundSigDigits(Count)))
          NumEPWTypExtSets=Count-1
          EXIT
        ENDIF
        Pos=INDEX(Line,',')
        IF (Pos /= 0) THEN
          CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
          IF (DateType /= InvalidDate) THEN
            IF (PMonth /= 0 .and. PDay /= 0) THEN
              TypicalExtremePeriods(Count)%StartMonth=PMonth
              TypicalExtremePeriods(Count)%StartDay=PDay
            ENDIF
          ELSE
            CALL ShowSevereError('ProcessEPWHeader: Invalid Typical/Extreme Periods Start Date Field(WeatherFile)='//  &
                        TRIM(Line(1:Pos-1)))
            CALL ShowContinueError('...on processing Typical/Extreme period #'//TRIM(RoundSigDigits(Count)))
            ErrorsFound=.true.
          ENDIF
          Line=Line(Pos+1:)
        ENDIF
        Pos=INDEX(Line,',')
        IF (Pos /= 0) THEN
          CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
          IF (DateType /= InvalidDate) THEN
            IF (PMonth /= 0 .and. PDay /= 0) THEN
              TypicalExtremePeriods(Count)%EndMonth=PMonth
              TypicalExtremePeriods(Count)%EndDay=PDay
            ENDIF
          ELSE
            CALL ShowSevereError('ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)='//  &
                TRIM(Line(1:Pos-1)))
            CALL ShowContinueError('...on processing Typical/Extreme period #'//TRIM(RoundSigDigits(Count)))
            ErrorsFound=.true.
          ENDIF
          Line=Line(Pos+1:)
        ELSE ! Pos=0, probably last one
          CALL ProcessDateString(Line(1:LEN_TRIM(Line)),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
          IF (DateType /= InvalidDate) THEN
            IF (PMonth /= 0 .and. PDay /= 0) THEN
              TypicalExtremePeriods(Count)%EndMonth=PMonth
              TypicalExtremePeriods(Count)%EndDay=PDay
            ENDIF
          ELSE
            CALL ShowSevereError('ProcessEPWHeader: Invalid Typical/Extreme Periods End Date Field(WeatherFile)='//  &
                TRIM(Line(1:Pos-1)))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Count=Count+1
      ENDDO
      ! Process periods to set up other values.
      DO Count=1,NumEPWTypExtSets
        ! JulianDay (Month,Day,LeapYearValue)
        SELECT CASE(MakeUPPERCase(TypicalExtremePeriods(Count)%ShortTitle))
          CASE ('SUMMER')
            IF (SameString(TypicalExtremePeriods(Count)%TEType,'EXTREME')) THEN
              TypicalExtremePeriods(Count)%MatchValue = 'SummerExtreme'
              TypicalExtremePeriods(Count)%MatchValue1= 'TropicalHot'
              TypicalExtremePeriods(Count)%MatchValue2= 'NoDrySeasonMax'
            ELSE
              TypicalExtremePeriods(Count)%MatchValue = 'SummerTypical'
            ENDIF

          CASE ('WINTER')
            IF (SameString(TypicalExtremePeriods(Count)%TEType,'EXTREME')) THEN
              TypicalExtremePeriods(Count)%MatchValue = 'WinterExtreme'
              TypicalExtremePeriods(Count)%MatchValue1= 'TropicalCold'
              TypicalExtremePeriods(Count)%MatchValue2= 'NoDrySeasonMin'
            ELSE
              TypicalExtremePeriods(Count)%MatchValue = 'WinterTypical'
            ENDIF

          CASE ('AUTUMN')
            TypicalExtremePeriods(Count)%MatchValue = 'AutumnTypical'

          CASE ('SPRING')
            TypicalExtremePeriods(Count)%MatchValue = 'SpringTypical'

          CASE ('WETSEASON')
            TypicalExtremePeriods(Count)%MatchValue = 'WetSeason'

          CASE ('DRYSEASON')
            TypicalExtremePeriods(Count)%MatchValue = 'DrySeason'

          CASE ('NOWETSEASON')
            TypicalExtremePeriods(Count)%MatchValue = 'NoWetSeason'

          CASE ('NODRYSEASON')
            TypicalExtremePeriods(Count)%MatchValue = 'NoDrySeason'

          CASE ('NODRYSEASONMAX','NOWETSEASONMAX')
            TypicalExtremePeriods(Count)%MatchValue = TypicalExtremePeriods(Count)%ShortTitle
            TypicalExtremePeriods(Count)%MatchValue1= 'TropicalHot'
            TypicalExtremePeriods(Count)%MatchValue2= 'SummerExtreme'

          CASE ('NODRYSEASONMIN','NOWETSEASONMIN')
            TypicalExtremePeriods(Count)%MatchValue = TypicalExtremePeriods(Count)%ShortTitle
            TypicalExtremePeriods(Count)%MatchValue1= 'TropicalCold'
            TypicalExtremePeriods(Count)%MatchValue2= 'WinterExtreme'

          CASE ('TROPICALHOT')
            TypicalExtremePeriods(Count)%MatchValue = 'TropicalHot'
            TypicalExtremePeriods(Count)%MatchValue1= 'SummerExtreme'
            TypicalExtremePeriods(Count)%MatchValue2= 'NoDrySeasonMax'

          CASE ('TROPICALCOLD')
            TypicalExtremePeriods(Count)%MatchValue = 'TropicalCold'
            TypicalExtremePeriods(Count)%MatchValue1= 'WinterExtreme'
            TypicalExtremePeriods(Count)%MatchValue2= 'NoDrySeasonMin'

          CASE DEFAULT
            TypicalExtremePeriods(Count)%MatchValue = 'Invalid - no match'

        END SELECT
        TypicalExtremePeriods(Count)%StartJDay=JulianDay(TypicalExtremePeriods(Count)%StartMonth,   &
                                                         TypicalExtremePeriods(Count)%StartDay,0)
        TypicalExtremePeriods(Count)%EndJDay=JulianDay(TypicalExtremePeriods(Count)%EndMonth,   &
                                                         TypicalExtremePeriods(Count)%EndDay,0)
        IF (TypicalExtremePeriods(Count)%StartJDay <= TypicalExtremePeriods(Count)%EndJDay) THEN
          TypicalExtremePeriods(Count)%TotalDays=TypicalExtremePeriods(Count)%EndJDay-TypicalExtremePeriods(Count)%StartJDay+1
        ELSE
          TypicalExtremePeriods(Count)%TotalDays=JulianDay(12,31,LeapYearAdd)-  &
                            TypicalExtremePeriods(Count)%StartJDay+1+TypicalExtremePeriods(Count)%EndJDay
        ENDIF
      ENDDO

    CASE ('GROUND TEMPERATURES')
      ! Added for ground surfaces defined with F or c factor method. TH 7/2009
      ! Assume the 0.5 m set of ground temperatures
      ! or first set on a weather file, if any.
      Pos=INDEX(Line,',')
      IF (Pos /= 0) THEN
        NumGrndTemps=ProcessNumber(Line(1:Pos-1),ErrFlag)
        IF (.not. ErrFlag .AND. NumGrndTemps >=1 ) THEN
          Line=Line(Pos+1:)
          ! skip depth, soil conductivity, soil density, soil specific heat
          do count=1,4
            Pos=INDEX(Line,',')
            if (Pos == 0) then
              Line=Blank
              exit
            endif
            Line=Line(Pos+1:)
          enddo
          GroundTempsFC=0.0d0
          actcount=0
          do count=1,12 ! take the first set of ground temperatures.
            Pos=INDEX(Line,',')
            IF (Pos /= 0) THEN
              Number=ProcessNumber(Line(1:Pos-1),ErrFlag)
              GroundTempsFC(Count) = Number
              actcount=actcount+1
            ELSE
              IF (Len_Trim(Line) > 0) THEN
                Number=ProcessNumber(Line(1:Pos-1),ErrFlag)
                GroundTempsFC(Count) = Number
                actcount=actcount+1
              ENDIF
              EXIT
            ENDIF
            Line=Line(Pos+1:)
          ENDDO
          if (actcount == 12) wthFCGroundTemps=.true.
        ENDIF
      ENDIF

    CASE ('HOLIDAYS/DAYLIGHT SAVING')
     !A1, \field LeapYear Observed
     ! \type choice
     ! \key Yes
     ! \key No
     ! \note Yes if Leap Year will be observed for this file
     ! \note No if Leap Year days (29 Feb) should be ignored in this file
     !A2, \field Daylight Saving Start Day
     !A3, \field Daylight Saving End Day
     !N1, \field Number of Holidays
     !A4, \field Holiday 1 Name
     !A5, \field Holiday 1 Day
     ! etc.
     ! Start with Minimum number of NumHdArgs
      Line=MakeUPPERCase(Line)
      NumHdArgs=4
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              IF (StripCR) THEN
                endcol=LEN_TRIM(Line)
                IF (endcol > 0) THEN
                  IF (ICHAR(Line(endcol:endcol)) == iASCII_CR) Line(endcol:endcol)=Blank
                ENDIF
              ENDIF
              Line=ADJUSTL(Line)
              Line=MakeUPPERCase(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

          CASE(1)
            IF (Line(1:1) == 'Y') THEN
!              LeapYear=.true.
              WFAllowsLeapYears=.true.
              WFLeapYearInd=0 !1
            ELSE
!              LeapYear=.false.
              WFAllowsLeapYears=.false.
              WFLeapYearInd=0
            ENDIF

          CASE(2)
            errflag1=ErrorsFound
            ErrorsFound=.false.
            CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
            IF (DateType /= InvalidDate) THEN
              IF (PMonth == 0 .and. PDay == 0) THEN
                EPWDaylightSaving=.false.
              ELSE
                EPWDaylightSaving=.true.
                EPWDST%StDateType=DateType
                EPWDST%StMon=PMonth
                EPWDST%StDay=PDay
                EPWDST%STWeekDay=PWeekDay
              ENDIF
            ELSE
              ErrorsFound=errflag1
              CALL ShowContinueError('ProcessEPWHeader: Invalid Daylight Saving Period Start Date Field(WeatherFile)='//  &
                                     TRIM(Line(1:Pos-1)))
              CALL ShowContinueError('...invalid header='//trim(HeaderString))
              CALL ShowContinueError('...Setting Weather File DST to false.')
              EPWDaylightSaving=.false.
            ENDIF

          CASE(3)
            CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
            IF (EPWDaylightSaving) THEN
              IF (DateType /= InvalidDate) THEN
                EPWDST%EnDateType=DateType
                EPWDST%EnMon=PMonth
                EPWDST%EnDay=PDay
                EPWDST%EnWeekDay=PWeekDay
              ELSE
                CALL ShowWarningError('ProcessEPWHeader: Invalid Daylight Saving Period End Date Field(WeatherFile)='//  &
                                        TRIM(Line(1:Pos-1)))
                CALL ShowContinueError('...Setting Weather File DST to false.')
                EPWDaylightSaving=.false.
              ENDIF
              DST=EPWDST
            ENDIF

          CASE(4)
            NumEPWHolidays=ProcessNumber(Line(1:Pos-1),IOStatus)
            NumSpecialDays=NumEPWHolidays+GetNumObjectsFound('RunPeriodControl:SpecialDays')
            ALLOCATE(SpecialDays(NumSpecialDays))
            NumHdArgs=4+NumEPWHolidays*2
            CurCount=0

          CASE(5:)
          IF (MOD(Count,2) /= 0) THEN
            CurCount=CurCount+1
            IF (CurCount > NumSpecialDays) THEN
              CALL ShowSevereError('Too many SpecialDays')
              ErrorsFound=.true.
            ELSE
              SpecialDays(CurCount)%Name=Line(1:Pos-1)
            ENDIF
            ! Process name
          ELSE
            IF (CurCount <= NumSpecialDays) THEN
            ! Process date
              CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound)
              IF (DateType == MonthDay) THEN
                SpecialDays(CurCount)%DateType=DateType
                SpecialDays(CurCount)%Month=PMonth
                SpecialDays(CurCount)%Day=PDay
                SpecialDays(CurCount)%WeekDay=0
                SpecialDays(CurCount)%CompDate=Pmonth*32+Pday
                SpecialDays(CurCount)%Duration=1
                SpecialDays(CurCount)%DayType=1
                SpecialDays(CurCount)%WthrFile=.true.
              ELSEIF (DateType /= InvalidDate) THEN
                SpecialDays(CurCount)%DateType=DateType
                SpecialDays(CurCount)%Month=PMonth
                SpecialDays(CurCount)%Day=PDay
                SpecialDays(CurCount)%WeekDay=PWeekDay
                SpecialDays(CurCount)%CompDate=0
                SpecialDays(CurCount)%Duration=1
                SpecialDays(CurCount)%DayType=1
                SpecialDays(CurCount)%WthrFile=.true.
              ELSEIF (DateType == InvalidDate) THEN
                CALL ShowSevereError('Invalid SpecialDay Date Field(WeatherFile)='//TRIM(Line(1:Pos-1)))
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ENDIF
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
      ENDDO
      DO Count=1,NumEPWTypExtSets
        ! JulianDay (Month,Day,LeapYearValue)
        TypicalExtremePeriods(Count)%StartJDay=JulianDay(TypicalExtremePeriods(Count)%StartMonth,   &
                                                         TypicalExtremePeriods(Count)%StartDay,LeapYearAdd)
        TypicalExtremePeriods(Count)%EndJDay=JulianDay(TypicalExtremePeriods(Count)%EndMonth,   &
                                                         TypicalExtremePeriods(Count)%EndDay,LeapYearAdd)
        IF (TypicalExtremePeriods(Count)%StartJDay <= TypicalExtremePeriods(Count)%EndJDay) THEN
          TypicalExtremePeriods(Count)%TotalDays=TypicalExtremePeriods(Count)%EndJDay-TypicalExtremePeriods(Count)%StartJDay+1
        ELSE
          TypicalExtremePeriods(Count)%TotalDays=JulianDay(12,31,LeapYearAdd)-  &
                            TypicalExtremePeriods(Count)%StartJDay+1+TypicalExtremePeriods(Count)%EndJDay
        ENDIF
      ENDDO

  CASE ('COMMENTS 1','COMMENTS 2')

  CASE ('DATA PERIODS')
!     N1, \field Number of Data Periods
!     N2, \field Number of Records per hour
!     A1, \field Data Period 1 Name/Description
!     A2, \field Data Period 1 Start Day of Week
!       \type choice
!       \key  Sunday
!       \key  Monday
!       \key  Tuesday
!       \key  Wednesday
!       \key  Thursday
!       \key  Friday
!       \key  Saturday
!     A3, \field Data Period 1 Start Day
!     A4, \field Data Period 1 End Day
      Line=MakeUPPERCase(Line)
      NumHdArgs=2
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              IF (StripCR) THEN
                endcol=LEN_TRIM(Line)
                IF (endcol > 0) THEN
                  IF (ICHAR(Line(endcol:endcol)) == iASCII_CR) Line(endcol:endcol)=Blank
                ENDIF
              ENDIF
              Line=ADJUSTL(Line)
              Line=MakeUPPERCase(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

        CASE(1)
          NumDataPeriods=ProcessNumber(Line(1:Pos-1),IOStatus)
          ALLOCATE(DataPeriods(NumDataPeriods))
          NumHdArgs=NumHdArgs+4*NumDataPeriods
          IF (NumDataPeriods > 0) THEN
            DataPeriods(1:NumDataPeriods)%NumDays=0
          ENDIF
          CurCount=0

        CASE(2)
          NumIntervalsPerHour=ProcessNumber(Line(1:Pos-1),IOStatus)
!          IF (NumIntervalsPerHour /= 1) THEN
!            CALL ShowSevereError('Process EPW: Not ready for more than one interval per hour')
!            ErrorsFound=.true.
!          ENDIF

        CASE(3:)
          CurOne=MOD(Count-3,4)

          SELECT CASE(CurOne)

            CASE(0)
              ! Description of Data Period
              CurCount=CurCount+1
              IF (CurCount > NumDataPeriods) THEN
                CALL ShowSevereError('Too many data periods')
                ErrorsFound=.true.
              ELSE
                DataPeriods(CurCount)%Name=Line(1:Pos-1)
              ENDIF

            CASE(1)
              ! Start Day of Week
              IF (CurCount <= NumDataPeriods) THEN
                DataPeriods(CurCount)%DayOfWeek=Line(1:Pos-1)
                DataPeriods(CurCount)%WeekDay=FindItemInList(DataPeriods(CurCount)%DayOfWeek,DaysOfWeek,7)
                IF (DataPeriods(CurCount)%WeekDay == 0) THEN
                  WRITE(ErrNum,*) CurCount
                  ErrNum=ADJUSTL(ErrNum)
                  CALL ShowSevereError('Weather File -- Invalid Start Day of Week for Data Period #'//TRIM(ErrNum)// &
                                       ', Invalid day='//TRIM(DataPeriods(CurCount)%DayOfWeek))
                  ErrorsFound=.true.
                ENDIF
              ENDIF

            CASE(2)
              ! DataPeriod Start Day
              IF (CurCount <= NumDataPeriods) THEN
                CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound,PYear)
                IF (DateType == MonthDay) THEN
                  DataPeriods(CurCount)%StMon=PMonth
                  DataPeriods(CurCount)%StDay=PDay
                  DataPeriods(CurCount)%StYear=PYear
                  IF (PYear /= 0) DataPeriods(CurCount)%HasYearData=.true.
                ELSE
                  CALL ShowSevereError('Data Periods must be of the form <DayOfYear> or <Month Day> (WeatherFile), found='  &
                                       //TRIM(Line(1:Pos-1)))
                  ErrorsFound=.true.
                ENDIF
              ENDIF

            CASE(3)
              IF (CurCount <= NumDataPeriods) THEN
                CALL ProcessDateString(Line(1:Pos-1),PMonth,PDay,PWeekDay,DateType,ErrorsFound,PYear)
                IF (DateType == MonthDay) THEN
                  DataPeriods(CurCount)%EnMon=PMonth
                  DataPeriods(CurCount)%EnDay=PDay
                  DataPeriods(CurCount)%EnYear=PYear
                  IF (PYear == 0 .and. DataPeriods(CurCount)%HasYearData) THEN
                    CALL ShowWarningError('Data Period (WeatherFile) - Start Date contains year. End Date does not.')
                    CALL ShowContinueError('...Assuming same year as Start Date for this data.')
                    DataPeriods(CurCount)%EnYear=DataPeriods(CurCount)%StYear
                  ENDIF
                ELSE
                  CALL ShowSevereError('Data Periods must be of the form <DayOfYear> or <Month Day>, (WeatherFile) found='  &
                                       //TRIM(Line(1:Pos-1)))
                  ErrorsFound=.true.
                ENDIF
              ENDIF
              IF (DataPeriods(CurCount)%StYear == 0 .or. DataPeriods(CurCount)%EnYear == 0) THEN
                DataPeriods(CurCount)%DataStJDay=JulianDay(DataPeriods(CurCount)%StMon,DataPeriods(CurCount)%StDay,LeapYearAdd)
                DataPeriods(CurCount)%DataEnJDay=JulianDay(DataPeriods(CurCount)%EnMon,DataPeriods(CurCount)%EnDay,LeapYearAdd)
                IF (DataPeriods(CurCount)%DataStJDay <= DataPeriods(CurCount)%DataEnJDay) THEN
                  DataPeriods(CurCount)%NumDays=DataPeriods(CurCount)%DataEnJDay-DataPeriods(CurCount)%DataStJDay+1
                ELSE
                  DataPeriods(CurCount)%NumDays=(365-DataPeriods(CurCount)%DataStJDay+1)+(DataPeriods(CurCount)%DataEnJDay-1+1)
                ENDIF
              ELSE  ! weather file has actual year(s)
                CALL jgDate(GregorianToJulian,DataPeriods(CurCount)%DataStJDay,  &
                   DataPeriods(CurCount)%StYear,DataPeriods(CurCount)%StMon,DataPeriods(CurCount)%StDay)
                CALL jgDate(GregorianToJulian,DataPeriods(CurCount)%DataEnJDay,  &
                   DataPeriods(CurCount)%EnYear,DataPeriods(CurCount)%EnMon,DataPeriods(CurCount)%EnDay)
                DataPeriods(CurCount)%NumDays=DataPeriods(CurCount)%DataEnJDay-DataPeriods(CurCount)%DataStJDay+1
              ENDIF
              ! Have processed the last item for this, can set up Weekdays for months
              DataPeriods(CurCount)%MonWeekDay=0
              IF (.not. ErrorsFound) THEN
                CALL SetupWeekDaysByMonth(DataPeriods(CurCount)%StMon,DataPeriods(CurCount)%StDay,  &
                                          DataPeriods(CurCount)%WeekDay,DataPeriods(CurCount)%MonWeekDay)
              ENDIF

          END SELECT
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
       ENDDO

    CASE DEFAULT
      CALL ShowFatalError('Invalid EPW Header designation found='//TRIM(HeaderString))

  END SELECT
  RETURN

END SUBROUTINE ProcessEPWHeader


SUBROUTINE SkipEPlusWFHeader

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine skips the initial header records on the EnergyPlus Weather File (in.epw).

          ! METHODOLOGY EMPLOYED:
          ! List directed reads, as possible.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: MakeUPPERCase, ProcessNumber

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: AFormat="(A)"
  CHARACTER(len=*), PARAMETER :: Header="DATA PERIODS"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Pos
  CHARACTER(len=255) Line
  integer HdPos
  logical StillLooking
  integer, external :: FindNonSpace
  INTEGER NumHdArgs
  INTEGER Count
  INTEGER CurCount
  INTEGER CurOne
  INTEGER NumPeriods
  LOGICAL IOStatus
  INTEGER endcol

  ! Read in Header Information

  ! Headers should come in order
  StillLooking=.true.
  DO WHILE (StillLooking)
    READ(WeatherFileUnitNumber,AFormat,END=9998) line
    Pos=FindNonSpace(line)
    line=MakeUPPERCase(line)
    HdPos=INDEX(line,TRIM(Header))
    IF (HdPos /= 0) EXIT
  ENDDO

! Dummy process Data Periods line
!  'DATA PERIODS'
!     N1, \field Number of Data Periods
!     N2, \field Number of Records per hour
!     A1, \field Data Period 1 Name/Description
!     A2, \field Data Period 1 Start Day of Week
!       \type choice
!       \key  Sunday
!       \key  Monday
!       \key  Tuesday
!       \key  Wednesday
!       \key  Thursday
!       \key  Friday
!       \key  Saturday
!     A3, \field Data Period 1 Start Day
!     A4, \field Data Period 1 End Day
      NumHdArgs=2
      Count=1
      DO WHILE (Count <= NumHdArgs)
        Line=ADJUSTL(Line)
        Pos=INDEX(Line,',')
        IF (Pos == 0) THEN
          IF (LEN_TRIM(Line) == 0) THEN
            DO WHILE (Pos == 0)
              READ(WeatherFileUnitNumber,AFormat) Line
              IF (StripCR) THEN
                endcol=LEN_TRIM(Line)
                IF (endcol > 0) THEN
                  IF (ICHAR(Line(endcol:endcol)) == iASCII_CR) Line(endcol:endcol)=Blank
                ENDIF
              ENDIF
              Line=ADJUSTL(Line)
              Line=MakeUPPERCase(Line)
              Pos=INDEX(Line,',')
            ENDDO
          ELSE
            Pos=LEN_TRIM(Line)+1
          ENDIF
        ENDIF

        SELECT CASE(Count)

        CASE(1)
          NumPeriods=ProcessNumber(Line(1:Pos-1),IOStatus)
          NumHdArgs=NumHdArgs+4*NumPeriods
          CurCount=0

        CASE(2)

        CASE(3:)
          CurOne=MOD(Count-3,4)

          SELECT CASE(CurOne)

            CASE(0)
              ! Description of Data Period
              CurCount=CurCount+1

            CASE(1:3)

          END SELECT
        END SELECT
        Line=Line(Pos+1:)
        Count=Count+1
       ENDDO

  RETURN

  9998 CALL ShowFatalError('Unexpected End-of-File on EPW Weather file, while reading header information, looking for header='// &
                            TRIM(Header),OutputFileStandard)

  RETURN

END SUBROUTINE SkipEPlusWFHeader

SUBROUTINE ReportMissing_RangeData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports the counts of missing/out of range data
          ! for weather file environments.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: MissString='Missing Data Found on Weather Data File'
  CHARACTER(len=*), PARAMETER :: msFmt="('Missing ',A,', Number of items=',I5)"
  CHARACTER(len=*), PARAMETER :: InvString='Invalid Data Found on Weather Data File'
  CHARACTER(len=*), PARAMETER :: ivFmt="('Invalid ',A,', Number of items=',I5)"
  CHARACTER(len=*), PARAMETER :: RangeString='Out of Range Data Found on Weather Data File'
  CHARACTER(len=*), PARAMETER :: rgFmt="('Out of Range ',A,' [',A,',',A,'], Number of items=',I5)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL MissedHeader
  LOGICAL OutOfRangeHeader
  CHARACTER(len=120) ErrString

  IF (.not. DisplayWeatherMissingDataWarnings) RETURN

  MissedHeader=.false.
  IF (Missed%DryBulb>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Dry Bulb Temperatures"',Missed%DryBulb
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%StnPres>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Atmospheric Pressure"',Missed%StnPres
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%RelHumid>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Relative Humidity"',Missed%RelHumid
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%DewPoint>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Dew Point Temperatures"',Missed%DewPoint
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%WindSpd>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Wind Speed"',Missed%WindSpd
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%WindDir>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Wind Direction"',Missed%WindDir
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%DirectRad>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Direct Radiation"',Missed%DirectRad
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%DiffuseRad>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Diffuse Radiation"',Missed%DiffuseRad
    CALL ShowMessage(ErrString)
  ENDIF
!  IF (Missed%Visibility>0) THEN
!    IF (.not. MissedHeader) THEN
!      CALL ShowWarningError(MissString)
!      MissedHeader=.true.
!    ENDIF
!    WRITE(ErrString,msFMT) 'Visibility',Missed%Visibility
!    CALL ShowMessage(ErrString)
!  ENDIF
!  IF (Missed%AerOptDepth>0) THEN
!    IF (.not. MissedHeader) THEN
!      CALL ShowWarningError(MissString)
!      MissedHeader=.true.
!    ENDIF
!    WRITE(ErrString,msFMT) 'Aerosol Optical Depth',Missed%AerOptDepth
!    CALL ShowMessage(ErrString)
!  ENDIF
  IF (Missed%TotSkyCvr>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Total Sky Cover"',Missed%TotSkyCvr
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%OpaqSkyCvr>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Opaque Sky Cover"',Missed%OpaqSkyCvr
    CALL ShowMessage(ErrString)
  ENDIF
!  IF (Missed%Ceiling>0) THEN
!    IF (.not. MissedHeader) THEN
!      CALL ShowWarningError(MissString)
!      MissedHeader=.true.
!    ENDIF
!    WRITE(ErrString,msFMT) 'Ceiling Height',Missed%Ceiling
!    CALL ShowMessage(ErrString)
!  ENDIF
!  IF (Missed%PrecipWater>0) THEN
!    IF (.not. MissedHeader) THEN
!      CALL ShowWarningError(MissString)
!      MissedHeader=.true.
!    ENDIF
!    WRITE(ErrString,msFMT) 'Water Precipitation',Missed%PrecipWater
!    CALL ShowMessage(ErrString)
!  ENDIF
  IF (Missed%SnowDepth>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Snow Depth"',Missed%SnowDepth
    CALL ShowMessage(ErrString)
  ENDIF
  IF (Missed%WeathCodes>0) THEN
    CALL ShowWarningError(InvString)
    WRITE(ErrString,ivFMT) '"Weather Codes" (not equal 9 digits)',Missed%WeathCodes
    CALL ShowMessage(ErrString)
  ENDIF
!  IF (Missed%Albedo>0) THEN
!    IF (.not. MissedHeader) THEN
!      CALL ShowWarningError(MissString)
!      MissedHeader=.true.
!    ENDIF
!    WRITE(ErrString,msFMT) '"Albedo"',Missed%Albedo
!    CALL ShowMessage(ErrString)
!  ENDIF
  IF (Missed%LiquidPrecip>0) THEN
    IF (.not. MissedHeader) THEN
      CALL ShowWarningError(MissString)
      MissedHeader=.true.
    ENDIF
    WRITE(ErrString,msFMT) '"Liquid Precipitation Depth"',Missed%LiquidPrecip
    CALL ShowMessage(ErrString)
  ENDIF
!  IF (Missed%DaysLastSnow>0) THEN
!    IF (.not. MissedHeader) THEN
!      CALL ShowWarningError(MissString)
!      MissedHeader=.true.
!    ENDIF
!    WRITE(ErrString,msFMT) 'Days Since Last Snow',Missed%DaysLastSnow
!    CALL ShowMessage(ErrString)
!  ENDIF

  OutOfRangeHeader=.false.
  IF (OutOfRange%DryBulb>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Dry Bulb Temperatures','>=-90','<=70',OutOfRange%DryBulb
    CALL ShowMessage(ErrString)
  ENDIF
  IF (OutOfRange%StnPres>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Atmospheric Pressure','>31000','<=120000',OutOfRange%StnPres
    CALL ShowMessage(ErrString)
    CALL ShowMessage('Out of Range values set to last good value')
  ENDIF
  IF (OutOfRange%RelHumid>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Relative Humidity','>=0','<=110',OutOfRange%RelHumid
    CALL ShowMessage(ErrString)
  ENDIF
  IF (OutOfRange%DewPoint>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Dew Point Temperatures','>=-90','<=70',OutOfRange%DewPoint
    CALL ShowMessage(ErrString)
  ENDIF
  IF (OutOfRange%WindSpd>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Wind Speed','>=0','<=40',OutOfRange%WindSpd
    CALL ShowMessage(ErrString)
  ENDIF
  IF (OutOfRange%WindDir>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Wind Direction','>=0','<=360',OutOfRange%WindDir
    CALL ShowMessage(ErrString)
  ENDIF
  IF (OutOfRange%DirectRad>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Direct Radiation','>=0','NoLimit',OutOfRange%DirectRad
    CALL ShowMessage(ErrString)
  ENDIF
  IF (OutOfRange%DiffuseRad>0) THEN
    IF (.not. OutOfRangeHeader) THEN
      CALL ShowWarningError(RangeString)
      OutOfRangeHeader=.true.
    ENDIF
    WRITE(ErrString,rgFmt) 'Diffuse Radiation','>=0','NoLimit',OutOfRange%DiffuseRad
    CALL ShowMessage(ErrString)
  ENDIF

  RETURN

END SUBROUTINE ReportMissing_RangeData

SUBROUTINE SetupInterpolationValues

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates the "interpolation" values / weights that are used for
          ! interpolating weather data from hourly down to the time step level.

          ! METHODOLOGY EMPLOYED:
          ! Create arrays (InterpolationValues, SolarInterpolationValues) dependent on
          ! Number of Time Steps in Hour.  This will be used in the "SetCurrentWeather" procedure.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  INTEGER halfpoint
  INTEGER hpoint
  INTEGER tloop
  REAL(r64) tweight
  REAL(r64) tweight1

  ALLOCATE(Interpolation(NumOfTimeStepInHour))
  ALLOCATE(SolarInterpolation(NumOfTimeStepInHour))
  Interpolation=0.0d0
  SolarInterpolation=0.0d0
  halfpoint=0

  do tloop=1,NumOfTimeStepInHour
    IF (NumOfTimeStepInHour == 1) THEN
      tweight=1.0d0
    ELSE
      tweight = MIN(1.0d0,(REAL(tloop,r64)/REAL(NumOfTimeStepInHour,r64)))
    END IF

    Interpolation(tloop)=tweight

  enddo

  if (mod(NumOfTimeStepInHour,2) == 0) then
    ! even number of time steps.
    halfpoint=NumOfTimeStepInHour/2
    SolarInterpolation(halfpoint)=1.0d0
    tweight=1.d0/REAL(NumOfTimeStepInHour,r64)
    hpoint=1
    do tloop=halfpoint+1,NumOfTimeStepInHour
      SolarInterpolation(tloop)=1.0-hpoint*tweight
      hpoint=hpoint+1
    enddo
    hpoint=1
    do tloop=halfpoint-1,1,-1
      SolarInterpolation(tloop)=1.0-hpoint*tweight
      hpoint=hpoint+1
    enddo
  else  ! odd number of time steps
    if (NumOfTimeStepInHour == 1) then
      SolarInterpolation(1)=.5d0
    elseif (NumOfTimeStepInHour == 3) then
      tweight=1.d0/real(NumOfTimeStepInHour,r64)
      SolarInterpolation(1)=5.0d0/6.0d0
      SolarInterpolation(2)=5.0d0/6.0d0
      SolarInterpolation(3)=.5d0
    else
      tweight=1.d0/real(NumOfTimeStepInHour,r64)
      halfpoint=NumOfTimeStepInHour/2
      tweight1=1.0d0-tweight/2.d0
      SolarInterpolation(halfpoint)=tweight1
      SolarInterpolation(halfpoint+1)=tweight1
      hpoint=1
      do tloop=halfpoint+2,NumOfTimeStepInHour
        SolarInterpolation(tloop)=tweight1-hpoint*tweight
        hpoint=hpoint+1
      enddo
      hpoint=1
      do tloop=halfpoint-1,1,-1
        SolarInterpolation(tloop)=tweight1-hpoint*tweight
        hpoint=hpoint+1
      enddo
    endif
  endif

  RETURN

END SUBROUTINE SetupInterpolationValues

SUBROUTINE SetupEnvironmentTypes

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Make sure Environment derived type is set prior to getting
          ! Weather Properties

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: InvJulianDay, JulianDay, BetweenDates

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
  INTEGER :: Loop
  INTEGER :: Loop1
  INTEGER :: JDay1
  INTEGER :: JDay2
  INTEGER :: LocalLeapYearAdd

          ! Transfer weather file information to the Environment derived type
      Envrn=TotDesDays+1
      ! Sizing Periods from Weather File
      DO Loop=1,TotRunDesPers
        Environment(Envrn)%StartMonth=RunPeriodDesignInput(Loop)%StartMonth
        Environment(Envrn)%StartDay=RunPeriodDesignInput(Loop)%StartDay
        Environment(Envrn)%StartJDay=JulianDay(RunPeriodDesignInput(Loop)%StartMonth,  &
                                                  RunPeriodDesignInput(Loop)%StartDay,LeapYearAdd)
        Environment(Envrn)%TotalDays=RunPeriodDesignInput(Loop)%TotalDays
        Environment(Envrn)%EndMonth=RunPeriodDesignInput(Loop)%EndMonth
        Environment(Envrn)%EndDay=RunPeriodDesignInput(Loop)%EndDay
        Environment(Envrn)%EndJDay=JulianDay(RunPeriodDesignInput(Loop)%EndMonth,  &
                                                RunPeriodDesignInput(Loop)%EndDay,LeapYearAdd)
        Environment(Envrn)%NumSimYears=RunPeriodDesignInput(Loop)%NumSimYears
        IF (Environment(Envrn)%StartJDay <= Environment(Envrn)%EndJDay) THEN
          Environment(Envrn)%TotalDays=(Environment(Envrn)%EndJDay-Environment(Envrn)%StartJDay+1)   &
                                        * Environment(Envrn)%NumSimYears
        ELSE
          Environment(Envrn)%TotalDays=(JulianDay(12,31,LeapYearAdd)  &
                                -Environment(Envrn)%StartJDay+1+Environment(Envrn)%EndJDay) * Environment(Envrn)%NumSimYears
        ENDIF
        TotRunDesPersDays=TotRunDesPersDays+Environment(Envrn)%TotalDays
        Environment(Envrn)%UseDST=RunPeriodDesignInput(Loop)%UseDST
        Environment(Envrn)%UseHolidays=RunPeriodDesignInput(Loop)%UseHolidays
        Environment(Envrn)%Title=RunPeriodDesignInput(Loop)%Title
        Environment(Envrn)%cKindOfEnvrn = RunPeriodDesignInput(Loop)%PeriodType
        Environment(Envrn)%KindOfEnvrn = ksRunPeriodDesign
        Environment(Envrn)%DayOfWeek=RunPeriodDesignInput(Loop)%DayOfWeek
        Environment(Envrn)%MonWeekDay=RunPeriodDesignInput(Loop)%MonWeekDay
        Environment(Envrn)%SetWeekDays=.false.
        Environment(Envrn)%ApplyWeekendRule=RunPeriodDesignInput(Loop)%ApplyWeekendRule
        Environment(Envrn)%UseRain=RunPeriodDesignInput(Loop)%UseRain
        Environment(Envrn)%UseSnow=RunPeriodDesignInput(Loop)%UseSnow
        Envrn=Envrn+1
      ENDDO

      ! RunPeriods from weather file
      DO Loop=1,TotRunPers  ! Run Periods.
        Environment(Envrn)%StartMonth=RunPeriodInput(Loop)%StartMonth
        Environment(Envrn)%StartDay=RunPeriodInput(Loop)%StartDay
        Environment(Envrn)%EndMonth=RunPeriodInput(Loop)%EndMonth
        Environment(Envrn)%EndDay=RunPeriodInput(Loop)%EndDay
        Environment(Envrn)%NumSimYears=RunPeriodInput(Loop)%NumSimYears
        IF (RunPeriodInput(Loop)%ActualWeather) THEN
          Environment(Envrn)%CurrentYear=RunPeriodInput(Loop)%StartYear
          Environment(Envrn)%IsLeapYear=IsLeapYear(RunPeriodInput(Loop)%StartYear)
          Environment(Envrn)%TreatYearsAsConsecutive=.true.
          Environment(Envrn)%StartYear=RunPeriodInput(Loop)%StartYear
          Environment(Envrn)%EndYear=RunPeriodInput(Loop)%EndYear
          CALL jgDate(GregorianToJulian,Environment(Envrn)%StartDate,  &
             Environment(Envrn)%StartYear,Environment(Envrn)%StartMonth,Environment(Envrn)%StartDay)
          CALL jgDate(GregorianToJulian,Environment(Envrn)%EndDate,  &
             Environment(Envrn)%EndYear,Environment(Envrn)%EndMonth,Environment(Envrn)%EndDay)
          Environment(Envrn)%StartJDay=Environment(Envrn)%StartDate
          Environment(Envrn)%EndJDay=Environment(Envrn)%EndDate
          Environment(Envrn)%TotalDays=Environment(Envrn)%EndDate-Environment(Envrn)%StartDate + 1
          Environment(Envrn)%RawSimDays=Environment(Envrn)%EndDate-Environment(Envrn)%StartDate + 1
          Environment(Envrn)%MatchYear=.true.
          Environment(Envrn)%ActualWeather=.true.
        ELSEIF (RunPeriodInput(Loop)%BeginYear < 100) THEN ! std RunPeriod
          Environment(Envrn)%CurrentYear=0
          IF (.not. WFAllowsLeapYears) THEN
            Environment(Envrn)%IsLeapYear=.false.  ! explicit set
            LocalLeapYearAdd=0
          ELSE
            Environment(Envrn)%IsLeapYear=.true.  ! explicit set
            LocalLeapYearAdd=1
          ENDIF
          Environment(Envrn)%TreatYearsAsConsecutive=.false.
          Environment(Envrn)%RollDayTypeOnRepeat=RunPeriodInput(Loop)%RollDayTypeOnRepeat
          Environment(Envrn)%StartJDay=JulianDay(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,LocalLeapYearAdd)
          Environment(Envrn)%EndJDay=JulianDay(RunPeriodInput(Loop)%EndMonth,RunPeriodInput(Loop)%EndDay,LocalLeapYearAdd)
        ! need message if isleapyear and wfleapyearind=0
          IF (Environment(Envrn)%StartJDay <= Environment(Envrn)%EndJDay) THEN
            Environment(Envrn)%RawSimDays=(Environment(Envrn)%EndJDay-Environment(Envrn)%StartJDay+1)
            Environment(Envrn)%TotalDays=(Environment(Envrn)%EndJDay-Environment(Envrn)%StartJDay+1)   &
                                          * Environment(Envrn)%NumSimYears
          ELSE
            Environment(Envrn)%RawSimDays=(JulianDay(12,31,LeapYearAdd)-Environment(Envrn)%StartJDay+1+  &
                                                    Environment(Envrn)%EndJDay)
            Environment(Envrn)%TotalDays=(JulianDay(12,31,LeapYearAdd)-Environment(Envrn)%StartJDay+1+  &
                                                    Environment(Envrn)%EndJDay) &
                                          * Environment(Envrn)%NumSimYears
          ENDIF

        ELSE ! Using Runperiod and StartYear option.
          Environment(Envrn)%CurrentYear=RunPeriodInput(Loop)%BeginYear
          Environment(Envrn)%IsLeapYear=IsLeapYear(Environment(Envrn)%CurrentYear)
          Environment(Envrn)%TreatYearsAsConsecutive=.true.
          Environment(Envrn)%RollDayTypeOnRepeat=RunPeriodInput(Loop)%RollDayTypeOnRepeat
          Environment(Envrn)%StartJDay=JulianDay(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,LeapYearAdd)
          Environment(Envrn)%EndJDay=JulianDay(RunPeriodInput(Loop)%EndMonth,RunPeriodInput(Loop)%EndDay,LeapYearAdd)
          Environment(Envrn)%TotalDays=0
          DO Loop1=1,Environment(Envrn)%NumSimYears
            IF (.not. IsLeapYear(RunPeriodInput(Loop)%BeginYear-1+Loop1) .or. .not. WFAllowsLeapYears) THEN
              JDay1=JulianDay(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,0)
              JDay2=JulianDay(RunPeriodInput(Loop)%EndMonth,RunPeriodInput(Loop)%EndDay,0)
              IF (JDay1 <= JDay2) THEN
                IF (Loop1 == 1) &
                  Environment(Envrn)%RawSimDays=(JDay2-Jday1+1)
                Environment(Envrn)%TotalDays=Environment(Envrn)%TotalDays+(JDay2-Jday1+1)
              ELSE
                IF (Loop1 == 1) &
                   Environment(Envrn)%RawSimDays=JulianDay(12,31,0)-JDay1+1+JDay2
                Environment(Envrn)%TotalDays=Environment(Envrn)%TotalDays+  &
                   JulianDay(12,31,0)-JDay1+1+JDay2
              ENDIF
            ELSE  ! Leap Year
              JDay1=JulianDay(RunPeriodInput(Loop)%StartMonth,RunPeriodInput(Loop)%StartDay,1)
              JDay2=JulianDay(RunPeriodInput(Loop)%EndMonth,RunPeriodInput(Loop)%EndDay,1)
              IF (JDay1 <= JDay2) THEN
                Environment(Envrn)%TotalDays=Environment(Envrn)%TotalDays+(JDay2-Jday1+1)
              ELSE
                Environment(Envrn)%TotalDays=Environment(Envrn)%TotalDays+  &
                   JulianDay(12,31,1)-JDay1+1+JDay2
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        Environment(Envrn)%UseDST=RunPeriodInput(Loop)%UseDST
        Environment(Envrn)%UseHolidays=RunPeriodInput(Loop)%UseHolidays
        IF (RunPeriodInput(Loop)%Title == Blank) THEN
          Environment(Envrn)%Title=WeatherFileLocationTitle
        ELSE
          Environment(Envrn)%Title=RunPeriodInput(Loop)%Title
        ENDIF
        Environment(Envrn)%cKindOfEnvrn = 'WeatherFileRunPeriod'
        Environment(Envrn)%KindOfEnvrn = ksRunPeriodWeather
        Environment(Envrn)%DayOfWeek=RunPeriodInput(Loop)%DayOfWeek
        Environment(Envrn)%MonWeekDay=RunPeriodInput(Loop)%MonWeekDay
        Environment(Envrn)%SetWeekDays=.false.
        Environment(Envrn)%ApplyWeekendRule=RunPeriodInput(Loop)%ApplyWeekendRule
        Environment(Envrn)%UseRain=RunPeriodInput(Loop)%UseRain
        Environment(Envrn)%UseSnow=RunPeriodInput(Loop)%UseSnow
        Envrn=Envrn+1
      ENDDO

  RETURN

END SUBROUTINE SetupEnvironmentTypes

FUNCTION IsLeapYear(Year) RESULT(YesNo)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! From entered year returns true (Yes) if it's a leap year, false (no) if not.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Year
  LOGICAL :: YesNo

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

    YesNo=.false.
    IF (MOD(Year,4) == 0) THEN  ! Potential Leap Year
      IF (.not. (MOD(Year,100) == 0 .and. MOD(Year,400) /= 0)) THEN
        YesNo=.true.
      ENDIF
    ENDIF
  RETURN

END FUNCTION IsLeapYear

SUBROUTINE JGDate(jflag,jdate,gyyyy,gmm,gdd)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Subroutine JGDate is a gregorian date to actual julian date
          ! converter.  the advantage of storing a julian date in the
          ! jdate format rather than a 5 digit format is that any
          ! number of days can be add or subtracted to jdate and
          ! that result is a proper julian date.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! for discussion of this algorithm,
          ! see cacm, vol 11, no 10, oct 1968, page 657

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: jflag   ! indicates direction of conversion,
                     ! 1 --> gregorian (dd/mm/yyyy) to julian
                     ! 2 --> julian to gregorian.

  INTEGER :: jdate   ! input/output julian date, typically a 7 or 8 digit integer
  INTEGER :: gyyyy   ! input/output gregorian year, should be specified as 4 digits
  INTEGER :: gmm     ! input/output gregorian month
  INTEGER :: gdd     ! input/output gregorian day

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: tdate   ! integer*4 variable needed for double precision arithmetic
  INTEGER :: tyyyy   ! integer*4 variable needed for double precision arithmetic
  INTEGER :: tmm     ! integer*4 variable needed for double precision arithmetic
  INTEGER :: tdd     ! integer*4 variable needed for double precision arithmetic
  INTEGER :: l       ! temporary variable used in conversion.
  INTEGER :: n       ! temporary variable used in conversion.

!
!                                       gregorian to julian
!
      if (jflag == 1) then
        tyyyy=gyyyy
        tmm=gmm
        tdd=gdd
        l= (tmm - 14) / 12
        jdate = tdd - 32075 + 1461 * (tyyyy + 4800 + l)/4 + 367 * (tmm - 2 - l*12)/12 - 3 * ((tyyyy + 4900 + l)/100)/4

      elseif (jflag == 2) then
!
!                                       julian to gregorian
!
        tdate = jdate
        l = tdate + 68569
        n = 4*l / 146097
        l = l - (146097*n + 3) / 4
        tyyyy = 4000*(l+1) / 1461001
        l = l- 1461*tyyyy / 4 + 31
        tmm = 80*l / 2447
        tdd = l - 2447*tmm / 80
        l = tmm/11
        tmm = tmm + 2 - 12*l
        tyyyy = 100 * (n - 49) + tyyyy + l
!c
        gyyyy = tyyyy
        gdd   = tdd
        gmm   = tmm

      endif
!c
      return

END SUBROUTINE JGDate

FUNCTION CalculateDayOfWeek(JulianDate) RESULT (dayOfWeek)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Using Julian date (from jgdate calc), calculate the correct day of week.

          ! METHODOLOGY EMPLOYED:
          ! Zeller's algorithm.

          ! REFERENCES:
          ! http://en.wikipedia.org/wiki/Zeller%27s_congruence
          ! and other references around the web.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: JulianDate   ! from JGDate calculation
  INTEGER                :: dayOfWeek    ! EnergyPlus convention (1=Sunday, 2=Monday, etc)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: JulDate ! Julian date copy
  INTEGER :: Gyyyy  ! Gregorian yyyy
  INTEGER :: Gmm    ! Gregorian mm
  INTEGER :: Gdd    ! Gregorian dd

  JulDate = JulianDate
  CALL JGDate(JulianToGregorian,JulDate,Gyyyy,Gmm,Gdd)

  ! Jan, Feb are 13, 14 months of previous year
  IF (Gmm < 3) THEN
    Gmm=Gmm+12
    Gyyyy=Gyyyy-1
  ENDIF

  dayOfWeek = MOD(Gdd + (13*(Gmm+1)/5) + Gyyyy + (Gyyyy/4) + 6*(Gyyyy/100) + (Gyyyy/400) , 7)
  IF (dayOfWeek == 0) dayOfWeek=7

  RETURN

END FUNCTION CalculateDayOfWeek

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

END MODULE WeatherManager
