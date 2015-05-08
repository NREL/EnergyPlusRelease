MODULE DataSizing    ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module contains type definitions and variables
          ! associated with HVAC system design flow rates, temperatures and
          ! capacities. This data is available to the HVAC component modules
          ! for their self sizing calculations.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

! parameters for outside air flow method
INTEGER, PARAMETER :: NumOAFlowMethods      = 6

INTEGER, PARAMETER :: OAFlowNone            = 0
INTEGER, PARAMETER :: OAFlowPPer            = 1
INTEGER, PARAMETER :: OAFlow                = 2
INTEGER, PARAMETER :: OAFlowPerArea         = 3
INTEGER, PARAMETER :: OAFlowACH             = 4
INTEGER, PARAMETER :: OAFlowSum             = 5
INTEGER, PARAMETER :: OAFlowMax             = 6

CHARACTER(len=*), PARAMETER, DIMENSION(NumOAFlowMethods) :: cOAFlowMethodTypes=  &
         (/'Flow/Person    ',  &
           'Flow/Zone      ',  &
           'Flow/Area      ',  &
           'AirChanges/Hour',  &
           'Sum            ',  &
           'Maximum        '/)

! parameters for outside air
INTEGER, PARAMETER :: AllOA                 = 1
INTEGER, PARAMETER :: MinOA                 = 2

! parameters for loop fluid type
INTEGER, PARAMETER :: HeatingLoop           = 1
INTEGER, PARAMETER :: CoolingLoop           = 2
INTEGER, PARAMETER :: CondenserLoop         = 3
INTEGER, PARAMETER :: SteamLoop             = 4

! paramters for sizing
INTEGER, PARAMETER :: NonCoincident         = 1
INTEGER, PARAMETER :: Coincident            = 2

! paramters for supply air flow rate method
INTEGER, PARAMETER :: SupplyAirTemperature  = 1
INTEGER, PARAMETER :: TemperatureDifference = 2

! paramters for sizing
INTEGER, PARAMETER :: FromDDCalc            = 1
INTEGER, PARAMETER :: InpDesAirFlow         = 2
INTEGER, PARAMETER :: DesAirFlowWithLim     = 3

! parameters for Type of Load to Size On
INTEGER, PARAMETER :: Sensible              = 0
INTEGER, PARAMETER :: Latent                = 1
INTEGER, PARAMETER :: Total                 = 2
INTEGER, PARAMETER :: Ventilation           = 3

! parameter for autosize
REAL(r64), PARAMETER :: AutoSize            = -99999.0d0

! parameter for (time-of-peak) sizing format
CHARACTER(len=*), PARAMETER :: PeakHrMinFmt = "(I2.2,':',I2.2,':00')"

!Zone Outdoor Air Method
INTEGER, PARAMETER :: ZOAM_FlowPerPerson = 1  ! set the outdoor air flow rate based on number of people in the zone
INTEGER, PARAMETER :: ZOAM_FlowPerZone = 2    ! sum the outdoor air flow rate per zone based on user input
INTEGER, PARAMETER :: ZOAM_FlowPerArea = 3    ! sum the outdoor air flow rate based on zone area
INTEGER, PARAMETER :: ZOAM_FlowPerACH = 4     ! sum the outdoor air flow rate based on number of air changes for the zone
INTEGER, PARAMETER :: ZOAM_Sum = 5            ! sum the outdoor air flow rate of the people component and the space floor area component
INTEGER, PARAMETER :: ZOAM_Max = 6            ! use the maximum of the outdoor air flow rate of the people component and
                                              ! the space floor area component

!System Outdoor Air Method
INTEGER, PARAMETER :: SOAM_ZoneSum = 1  ! Sum the outdoor air flow rates of all zones
INTEGER, PARAMETER :: SOAM_VRP = 2      ! Use ASHRAE Standard 62.1-2007 to calculate the system level outdoor air flow rates
                                        !  considering the zone air distribution effectiveness and the system ventilation efficiency
INTEGER, PARAMETER :: SOAM_IAQP = 3     ! Use ASHRAE Standard 62.1-2007 IAQP to calculate the system level outdoor air flow rates
                                        ! based on the CO2 setpoint
INTEGER, PARAMETER :: SOAM_ProportionalControl = 4     ! Use ASHRAE Standard 62.1-2004 or Trane Engineer's newsletter (volume 34-5)
                                                       ! to calculate the system level outdoor air flow rates
INTEGER, PARAMETER :: SOAM_IAQPGC = 5   ! Use ASHRAE Standard 62.1-2004 IAQP to calculate the system level outdoor air flow rates
                                        ! based on the generic contaminant setpoint
INTEGER, PARAMETER :: SOAM_IAQPCOM = 6  ! Take the maximum outdoor air rate from both CO2 and generic contaminant controls
                                        ! based on the generic contaminant setpoint

          ! DERIVED TYPE DEFINITIONS:
TYPE ZoneSizingInputData
  CHARACTER &
    (len=MaxNameLength) :: ZoneName                 = ' '     ! name of a zone
  INTEGER               :: ZoneNum                  = 0       ! index of the zone
  INTEGER               :: ZnCoolDgnSAMethod        = 0       ! choice of how to get zone cooling design air temperature;
                                                              !  1 = specify supply air temperature,
                                                              !  2 = calculate from the temperature difference
  INTEGER               :: ZnHeatDgnSAMethod        = 0       ! choice of how to get zone heating design air temperature;
                                                              !  1 = specify supply air temperature,
                                                              !  2 = calculate from the temperature difference
  REAL(r64)             :: CoolDesTemp              = 0.0d0   ! zone design cooling supply air temperature [C]
  REAL(r64)             :: HeatDesTemp              = 0.0d0   ! zone design heating supply air temperature [C]
  REAL(r64)             :: CoolDesTempDiff          = 0.0d0   ! zone design cooling supply air temperature difference [deltaC]
  REAL(r64)             :: HeatDesTempDiff          = 0.0d0   ! zone design heating supply air temperature difference [deltaC]
  REAL(r64)             :: CoolDesHumRat            = 0.0d0   ! zone design cooling supply air humidity ratio [kg-H2O/kg-air]
  REAL(r64)             :: HeatDesHumRat            = 0.0d0   ! zone design heating supply air humidity ratio [kg-H2O/kg-air]
  CHARACTER &
    (len=MaxNameLength) :: DesignSpecOAObjName      = ' '     ! name of the design specification outdoor air object
  INTEGER               :: OADesMethod              = 0       ! choice of how to calculate minimum outside air;
                                                              !  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
                                                              !  4 = sum of flow from 3 OA input fields;
                                                              !  5 = max of flow from 3 OA input fields
  REAL(r64)             :: DesOAFlowPPer            = 0.0d0   ! design outside air flow per person in zone [m3/s]
  REAL(r64)             :: DesOAFlowPerArea         = 0.0d0   ! design outside air flow per zone area [m3/s / m2]
  REAL(r64)             :: DesOAFlow                = 0.0d0   ! design outside air flow for the zone [m3/s]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get zone cooling design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL(r64)             :: DesCoolAirFlow           = 0.0d0   ! design zone supply air flow rate [m3/s]
  REAL(r64)             :: DesCoolMinAirFlowPerArea = 0.0d0   ! design cooling minimum air flow rate per zone area [m3/s / m2]
  REAL(r64)             :: DesCoolMinAirFlow        = 0.0d0   ! design cooling minimum air flow rate [m3/s]
  REAL(r64)             :: DesCoolMinAirFlowFrac    = 0.0d0   ! design cooling minimum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get zone heating design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL(r64)             :: DesHeatAirFlow           = 0.0d0   ! design zone heating supply air flow rate [m3/s]
  REAL(r64)             :: DesHeatMaxAirFlowPerArea = 0.0d0   ! design heating maximum air flow rate per zone area [m3/s / m2]
  REAL(r64)             :: DesHeatMaxAirFlow        = 0.0d0   ! design heating maximum air flow rate [m3/s]
  REAL(r64)             :: DesHeatMaxAirFlowFrac    = 0.0d0   ! design heating maximum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  REAL(r64)             :: HeatSizingFactor             = 0.0d0   ! the zone heating sizing ratio
  REAL(r64)             :: CoolSizingFactor             = 0.0d0   ! the zone cooling sizing ratio
  REAL(r64)             :: ZoneADEffCooling         = 1.0d0
  REAL(r64)             :: ZoneADEffHeating         = 1.0d0
  CHARACTER &
    (len=MaxNameLength) :: ZoneAirDistEffObjName      = ' '     ! name of the zone air distribution effectiveness object name
  INTEGER               :: ZoneAirDistributionIndex     = 0   ! index to the zone air distribution object
  INTEGER               :: ZoneDesignSpecOAIndex        = 0   ! index to the zone design spec OA object
  REAL(r64)             :: ZoneSecondaryRecirculation   = 0.0d0   ! the zone secondary air recirculation fraction
END TYPE ZoneSizingInputData

TYPE ZoneSizingData
  CHARACTER &
    (len=MaxNameLength) :: ZoneName                 = ' '     ! name of a zone
  CHARACTER &
    (len=MaxNameLength) :: CoolDesDay               = ' '     ! name of a cooling design day
  CHARACTER &
    (len=MaxNameLength) :: HeatDesDay               = ' '     ! name of a heating design day
  INTEGER               :: ZnCoolDgnSAMethod        = 0       ! choice of how to get zone cooling design air temperature;
                                                              !  1 = specify supply air temperature,
                                                              !  2 = calculate from the temperature difference
  INTEGER               :: ZnHeatDgnSAMethod        = 0       ! choice of how to get zone heating design air temperature;
                                                              !  1 = specify supply air temperature,
                                                              !  2 = calculate from the temperature difference
  REAL(r64)             :: CoolDesTemp              = 0.0d0   ! zone design cooling supply air temperature [C]
  REAL(r64)             :: HeatDesTemp              = 0.0d0   ! zone design heating supply air temperature [C]
  REAL(r64)             :: CoolDesTempDiff          = 0.0d0   ! zone design cooling supply air temperature difference [deltaC]
  REAL(r64)             :: HeatDesTempDiff          = 0.0d0   ! zone design heating supply air temperature difference [deltaC]
  REAL(r64)             :: CoolDesHumRat            = 0.0d0   ! zone design cooling supply air humidity ratio [kg-H2O/kg-air]
  REAL(r64)             :: HeatDesHumRat            = 0.0d0   ! zone design heating supply air humidity ratio [kg-H2O/kg-air]
  INTEGER               :: ZoneDesignSpecOAIndex    = 0       ! index to DesignSpecification:OutdoorAir object
  INTEGER               :: OADesMethod              = 0       ! choice of how to calculate minimum outside air;
                                                              !  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
                                                              !  4 = sum of flow from 3 OA input fields;
                                                              !  5 = max of flow from 3 OA input fields
  REAL(r64)             :: DesOAFlowPPer            = 0.0d0   ! design outside air flow per person in zone [m3/s]
  REAL(r64)             :: DesOAFlowPerArea         = 0.0d0   ! design outside air flow per zone area [m3/s / m2]
  REAL(r64)             :: DesOAFlow                = 0.0d0   ! design outside air flow for the zone [m3/s]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get zone cooling design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL(r64)             :: InpDesCoolAirFlow        = 0.0d0   ! design zone supply air flow rate [m3/s]
  REAL(r64)             :: DesCoolMinAirFlowPerArea = 0.0d0   ! design cooling minimum air flow rate per zone area [m3/s / m2]
  REAL(r64)             :: DesCoolMinAirFlow        = 0.0d0   ! design cooling minimum air flow rate [m3/s]
  REAL(r64)             :: DesCoolMinAirFlowFrac    = 0.0d0   ! design cooling minimum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get zone heating design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL(r64)             :: InpDesHeatAirFlow        = 0.0d0   ! design zone heating supply air flow rate [m3/s]
  REAL(r64)             :: DesHeatMaxAirFlowPerArea = 0.0d0   ! design heating maximum air flow rate per zone area [m3/s / m2]
  REAL(r64)             :: DesHeatMaxAirFlow        = 0.0d0   ! design heating maximum air flow rate [m3/s]
  REAL(r64)             :: DesHeatMaxAirFlowFrac    = 0.0d0   ! design heating maximum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  REAL(r64)             :: HeatSizingFactor             = 0.0d0   ! the zone heating sizing ratio
  REAL(r64)             :: CoolSizingFactor             = 0.0d0   ! the zone cooling sizing ratio
  INTEGER               :: ActualZoneNum            = 0       ! index into the Zone data array (in DataHeatBalance)
  INTEGER               :: SupplyAirNode            = 0       ! node number of supply air node

  REAL(r64)             :: DesHeatMassFlow          = 0.0d0   ! zone design heating air mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideDesHeatMassOn = .FALSE. ! true if EMS is acting on this structure
  REAL(r64)             :: EMSValueDesHeatMassFlow  = 0.0D0   ! Value EMS directing to use for Design Heating air mass flow [kg/s]

  REAL(r64)             :: DesCoolMassFlow          = 0.0d0   ! zone design cooling air mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideDesCoolMassOn = .FALSE. ! true if EMS is acting on this structure
  REAL(r64)             :: EMSValueDesCoolMassFlow  = 0.0D0   ! Value EMS directing to use for Design Cooling air mass flow [kg/s]

  REAL(r64)             :: DesHeatLoad              = 0.0d0   ! zone design heating load [W]
  LOGICAL               :: EMSOverrideDesHeatLoadOn = .FALSE. ! true if EMS is acting on this structure
  REAL(r64)             :: EMSValueDesHeatLoad      = 0.0D0   ! Value EMS directing to use for zone design heating load  [W]

  REAL(r64)             :: DesCoolLoad              = 0.0d0   ! zone design cooling load [W]
  LOGICAL               :: EMSOverrideDesCoolLoadOn = .FALSE. ! true if EMS is acting on this structure
  REAL(r64)             :: EMSValueDesCoolLoad      = 0.0D0   ! Value EMS directing to use for zone design cooling load  [W]

  REAL(r64)             :: DesHeatDens              = 0.0d0   ! zone design heating air density [kg/m3]
  REAL(r64)             :: DesCoolDens              = 0.0d0   ! zone design cooling air density [kg/m3]

  REAL(r64)             :: DesHeatVolFlow           = 0.0d0   ! zone design heating air volume flow rate [m3/s]
  LOGICAL               :: EMSOverrideDesHeatVolOn  = .FALSE. ! true if EMS is acting on this structure
  REAL(r64)             :: EMSValueDesHeatVolFlow   = 0.0D0   ! Value EMS directing to use for Design Heating air volume flow [m3/s]

  REAL(r64)             :: DesCoolVolFlow           = 0.0d0   ! zone design cooling air volume flow rate [m3/s]
  LOGICAL               :: EMSOverrideDesCoolVolOn  = .FALSE. ! true if EMS is acting on this structure
  REAL(r64)             :: EMSValueDesCoolVolFlow   = 0.0D0   ! Value EMS directing to use for Design cooling air volume flow [m3/s]

  REAL(r64)             :: DesHeatVolFlowMax        = 0.0d0   ! zone design heating maximum air volume flow rate [m3/s]
  REAL(r64)             :: DesCoolVolFlowMin        = 0.0d0   ! zone design cooling minimum air volume flow rate [m3/s]
  REAL(r64)             :: DesHeatCoilInTemp        = 0.0d0   ! zone heating coil design air inlet temperature [C]
  REAL(r64)             :: DesCoolCoilInTemp        = 0.0d0   ! zone cooling coil design air inlet temperature [C]
  REAL(r64)             :: DesHeatCoilInHumRat      = 0.0d0   ! zone heating coil design air inlet humidity ratio [kg/kg]
  REAL(r64)             :: DesCoolCoilInHumRat      = 0.0d0   ! zone cooling coil design air inlet humidity ratio [kg/kg]
  REAL(r64)             :: DesHeatCoilInTempTU      = 0.0d0   ! zone heating coil design air inlet temperature (supply air)([C]
  REAL(r64)             :: DesCoolCoilInTempTU      = 0.0d0   ! zone cooling coil design air inlet temperature (supply air)[C]
  REAL(r64)             :: DesHeatCoilInHumRatTU    = 0.0d0   ! zone heating coil design air inlet humidity ratio
                                                              !  (supply air) [kg/kg]
  REAL(r64)             :: DesCoolCoilInHumRatTU    = 0.0d0   ! zone cooling coil design air inlet humidity ratio
                                                              !  (supply air) [kg/kg]
  REAL(r64)             :: HeatMassFlow             = 0.0d0   ! current zone heating air mass flow rate (HVAC time step)
  REAL(r64)             :: CoolMassFlow             = 0.0d0   ! current zone cooling air mass flow rate (HVAC time step)
  REAL(r64)             :: HeatLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
  REAL(r64)             :: CoolLoad                 = 0.0d0   ! current zone heating load (HVAC time step)
  REAL(r64)             :: HeatZoneTemp             = 0.0d0   ! current zone temperature (heating, time step)
  REAL(r64)             :: HeatOutTemp              = 0.0d0   ! current outdoor temperature (heating, time step)
  REAL(r64)             :: HeatZoneRetTemp          = 0.0d0   ! current zone return temperature (heating, time step)
  REAL(r64)             :: HeatTstatTemp            = 0.0d0   ! current zone thermostat temperature (heating, time step)
  REAL(r64)             :: CoolZoneTemp             = 0.0d0   ! current zone temperature (cooling, time step)
  REAL(r64)             :: CoolOutTemp              = 0.0d0   ! current Outdoor temperature (cooling, time step)
  REAL(r64)             :: CoolZoneRetTemp          = 0.0d0   ! current zone return temperature (cooling, time step)
  REAL(r64)             :: CoolTstatTemp            = 0.0d0   ! current zone thermostat temperature (cooling, time step)
  REAL(r64)             :: HeatZoneHumRat           = 0.0d0   ! current zone humidity ratio (heating, time step)
  REAL(r64)             :: CoolZoneHumRat           = 0.0d0   ! current zone humidity ratio (cooling, time step)
  REAL(r64)             :: HeatOutHumRat            = 0.0d0   ! current outdoor humidity ratio (heating, time step)
  REAL(r64)             :: CoolOutHumRat            = 0.0d0   ! current outdoor humidity ratio (cooling, time step)
  REAL(r64)             :: ZoneTempAtHeatPeak       = 0.0d0   ! zone temp at max heating [C]
  REAL(r64)             :: ZoneRetTempAtHeatPeak    = 0.0d0   ! zone return temp at max heating [C]
  REAL(r64)             :: OutTempAtHeatPeak        = 0.0d0   ! outdoor temperature at max heating [C]
  REAL(r64)             :: ZoneTempAtCoolPeak       = 0.0d0   ! zone temp at max cooling [C]
  REAL(r64)             :: ZoneRetTempAtCoolPeak    = 0.0d0   ! zone return temp at max cooling [C]
  REAL(r64)             :: OutTempAtCoolPeak        = 0.0d0   ! outdoor temperature at max cooling [C]
  REAL(r64)             :: ZoneHumRatAtHeatPeak     = 0.0d0   ! zone humidity ratio at max heating [kg/kg]
  REAL(r64)             :: ZoneHumRatAtCoolPeak     = 0.0d0   ! zone humidity ratio at max cooling [kg/kg]
  REAL(r64)             :: OutHumRatAtHeatPeak      = 0.0d0   ! outdoor humidity at max heating [kg/kg]
  REAL(r64)             :: OutHumRatAtCoolPeak      = 0.0d0   ! outdoor humidity at max cooling [kg/kg]
  INTEGER               :: TimeStepNumAtHeatMax     = 0       ! time step number (in day) at Heating peak
  INTEGER               :: TimeStepNumAtCoolMax     = 0       ! time step number (in day) at cooling peak
  INTEGER               :: HeatDDNum                = 0       ! design day index of design day causing heating peak
  INTEGER               :: CoolDDNum                = 0       ! design day index of design day causing heating peak
  CHARACTER(len=8)      :: cHeatDDDate              = ' '     ! date of design day causing heating peak
  CHARACTER(len=8)      :: cCoolDDDate              = ' '     ! date of design day causing cooling peak
  REAL(r64)             :: MinOA                    = 0.0d0   ! design minimum outside air in m3/s
  REAL(r64)             :: DesCoolMinAirFlow2       = 0.0d0   ! design cooling minimum air flow rate [m3/s] derived from
                                                              !  DesCoolMinAirFlowPerArea
  REAL(r64)             :: DesHeatMaxAirFlow2       = 0.0d0   ! design heating maximum air flow rate [m3/s] derived from
                                                              !  DesHeatMaxAirFlowPerArea
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatFlowSeq        ! daily sequence of zone heating air mass flow rate
                                                              !  (zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolFlowSeq        ! daily sequence of zone cooling air mass flow rate
                                                              !  (zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatLoadSeq        ! daily sequence of zone heating load zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolLoadSeq        ! daily sequence of zone cooling load zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatZoneTempSeq    ! daily sequence of zone temperatures (heating, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatOutTempSeq     ! daily sequence of outdoor temperatures (heating, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatZoneRetTempSeq ! daily sequence of zone return temperatures (heating,
                                                              !  zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatTstatTempSeq   ! daily sequence of zone thermostat temperatures (heating,
                                                              !  zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: DesHeatSetPtSeq    ! daily sequence of indoor set point temperatures (zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolZoneTempSeq    ! daily sequence of zone temperatures (cooling, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolOutTempSeq     ! daily sequence of outdoor temperatures (cooling, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolZoneRetTempSeq ! daily sequence of zone return temperatures (cooling,
                                                              !  zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolTstatTempSeq   ! daily sequence of zone thermostat temperatures (cooling,
                                                              !  zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: DesCoolSetPtSeq    ! daily sequence of indoor set point temperatures (zone time step)

  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatZoneHumRatSeq  ! daily sequence of zone humidity ratios (heating, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolZoneHumRatSeq  ! daily sequence of zone humidity ratios (cooling, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatOutHumRatSeq   ! daily sequence of outdoor humidity ratios (heating, zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolOutHumRatSeq   ! daily sequence of outdoor humidity ratios (cooling, zone time step)
  REAL(r64)             :: ZoneADEffCooling         = 1.0d0   ! the zone air distribution effectiveness in cooling mode
  REAL(r64)             :: ZoneADEffHeating         = 1.0d0   ! the zone air distribution effectiveness in heating mode
  REAL(r64)             :: ZoneSecondaryRecirculation   = 0.0d0   ! the zone secondary air recirculation fraction
  REAL(r64)             :: ZonePrimaryAirFraction       = 0.0d0   ! the zone primary air fraction for cooling based calculations
  REAL(r64)             :: ZonePrimaryAirFractionHtg    = 0.0d0   ! the zone primary air fraction for heating based calculations
  REAL(r64)             :: ZoneOAFracCooling        = 0.0d0   ! OA fraction in cooling mode
  REAL(r64)             :: ZoneOAFracHeating        = 0.0d0   ! OA fraction in heating mode
  REAL(r64)             :: TotalOAFromPeople        = 0.0d0   ! Zone OA required due to people
  REAL(r64)             :: TotalOAFromArea          = 0.0d0   ! Zone OA required based on floor area
  REAL(r64)             :: TotPeopleInZone          = 0.0d0   ! total number of people in the zone
  REAL(r64)             :: TotalZoneFloorArea       = 0.0d0   ! total zone floor area
  REAL(r64)             :: ZonePeakOccupancy        = 0.0d0   ! zone peak occupancy based on max schedule value
  REAL(r64)             :: SupplyAirAdjustFactor    = 1.0d0   ! supply air adjustment factor for next time step if OA is capped
  REAL(r64)             :: ZpzClgByZone             = 0.0d0   ! OA Std 62.1 required fraction in cooling mode
  REAL(r64)             :: ZpzHtgByZone             = 0.0d0   ! OA Std 62.1 required fraction in heating mode
  REAL(r64)             :: VozClgByZone             = 0.0d0   ! value of required cooling vent to zone, used in 62.1 tabular report
  REAL(r64)             :: VozHtgByZone             = 0.0d0   ! value of required heating vent to zone, used in 62.1 tabular report

END TYPE ZoneSizingData

TYPE TermUnitSizingData
  REAL(r64)             :: AirVolFlow               = 0.0d0 ! design air vol flow rate for single duct terminal unit [m3/s]
  REAL(r64)             :: MaxHWVolFlow             = 0.0d0 ! design Hot Water vol flow for single duct terminal unit [m3/s]
  REAL(r64)             :: MaxSTVolFlow             = 0.0d0 ! design Steam vol flow rate for single duct terminal unit [m3/s]
  REAL(r64)             :: MaxCWVolFlow             = 0.0d0 ! design Cold Water vol flow for single duct terminal unit [m3/s]
  REAL(r64)             :: MinFlowFrac              = 0.0d0 ! design minimum flow fraction for a terminal unit
  REAL(r64)             :: InducRat                 = 0.0d0 ! design induction ratio for a terminal unit
  LOGICAL               :: InducesPlenumAir         =.FALSE.! True if secondary air comes from the plenum
  REAL(r64)             :: ReheatAirFlowMult        = 1.0d0 ! multiplier for air flow in reheat coil UA calculation
  REAL(r64)             :: ReheatLoadMult           = 1.0d0 ! multiplier for load in reheat coil UA calculation
  REAL(r64)             :: DesCoolingLoad           = 0.0d0 ! design cooling load used for zone equipment [W]
  REAL(r64)             :: DesHeatingLoad           = 0.0d0 ! design heating load used for zone equipment [W]
END TYPE TermUnitSizingData

TYPE ZoneEqSizingData                                       ! data saved from zone eq component sizing and passed to subcomponents
  REAL(r64)             :: AirVolFlow               = 0.0d0 ! design air vol flow rate for zone equipment unit [m3/s]
  REAL(r64)             :: MaxHWVolFlow             = 0.0d0 ! design Hot Water vol flow for zone equipment unit [m3/s]
  REAL(r64)             :: MaxCWVolFlow             = 0.0d0 ! design Cold Water vol flow for zone equipment unit [m3/s]
  REAL(r64)             :: OAVolFlow                = 0.0d0 ! design outside air flow for zone equipment unit [m3/s]
  REAL(r64)             :: DesCoolingLoad           = 0.0d0 ! design cooling load used for zone equipment [W]
  REAL(r64)             :: DesHeatingLoad           = 0.0d0 ! design heating load used for zone equipment [W]
  LOGICAL               :: AirFlow  = .FALSE.   ! TRUE if AirloopHVAC system air flow rate is calcualted
  LOGICAL               :: Capacity = .FALSE.  ! TRUE if AirloopHVAC system capacity is calculated
END TYPE ZoneEqSizingData

TYPE SystemSizingInputData
  CHARACTER &
    (len=MaxNameLength) :: AirPriLoopName           = ' '     ! name of an AirLoopHVAC object
  INTEGER               :: AirLoopNum               = 0       ! index number of air loop
  INTEGER               :: LoadSizeType             = 0       ! type of load to size on;
                                                              ! 0=sensible, 1=latent, 2=total, 3=ventilation
  INTEGER               :: SizingOption             = 0       ! 1 = noncoincident, 2 = coincident
  INTEGER               :: CoolOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for cooling sizing
  INTEGER               :: HeatOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for heating sizing
  REAL(r64)             :: DesOutAirVolFlow         = 0.0d0   ! design (minimum) outside air flow rate [m3/s]
  REAL(r64)             :: SysAirMinFlowRat         = 0.0d0   ! minimum system air flow ratio
  REAL(r64)             :: PreheatTemp              = 0.0d0   ! preheat design set temperature [C]
  REAL(r64)             :: PrecoolTemp              = 0.0d0   ! precool design set temperature [C]
  REAL(r64)             :: PreheatHumRat            = 0.0d0   ! preheat design humidity ratio [kg water/kg dry air]
  REAL(r64)             :: PrecoolHumRat            = 0.0d0   ! precool design humidity ratio [kg water/kg dry air]
  REAL(r64)             :: CoolSupTemp              = 0.0d0   ! cooling design supply air temperature [C]
  REAL(r64)             :: HeatSupTemp              = 0.0d0   ! heating design supply air temperature [C]
  REAL(r64)             :: CoolSupHumRat            = 0.0d0   ! cooling design supply air humidity ratio [kg water/kg dry air]
  REAL(r64)             :: HeatSupHumRat            = 0.0d0   ! heating design supply air humidity ratio [kg water/kg dry air]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get system cooling design air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per system, user input
  REAL(r64)             :: DesCoolAirFlow           = 0.0d0   ! design system supply air flow rate for cooling[m3/s]
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get system heating design air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per zone, user input
  REAL(r64)             :: DesHeatAirFlow           = 0.0d0   ! design system heating supply air flow rate [m3/s]
  INTEGER               :: SystemOAMethod           = 0       ! System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
  REAL(r64)             :: MaxZoneOAFraction        = 0.0d0   ! maximum value of min OA for zones served by system
  LOGICAL               :: OAAutosized              = .FALSE.  ! Set to true if design OA vol flow is set to 'autosize'
                                                               ! in Sizing:System
END TYPE SystemSizingInputData

TYPE SystemSizingData             ! Contains data for system sizing
  CHARACTER &
    (len=MaxNameLength) :: AirPriLoopName           = ' '     ! name of an AirLoopHVAC object
  CHARACTER &
    (len=MaxNameLength) :: CoolDesDay               = ' '     ! name of a cooling design day
  CHARACTER &
    (len=MaxNameLength) :: HeatDesDay               = ' '     ! name of a heating design day
  INTEGER               :: LoadSizeType             = 0       ! type of load to size on;
                                                              ! 0=sensible, 1=latent, 2=total, 3=ventilation
  INTEGER               :: SizingOption             = 0       ! 1 = noncoincident, 2 = coincident.
  INTEGER               :: CoolOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for cooling sizing
  INTEGER               :: HeatOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for heating sizing
  REAL(r64)             :: DesOutAirVolFlow         = 0.0d0   ! design (minimum) outside air flow rate [m3/s]
  REAL(r64)             :: SysAirMinFlowRat         = 0.0d0   ! minimum system air flow ratio
  REAL(r64)             :: PreheatTemp              = 0.0d0   ! preheat design set temperature
  REAL(r64)             :: PrecoolTemp              = 0.0d0   ! precool design set temperature [C]
  REAL(r64)             :: PreheatHumRat            = 0.0d0   ! preheat design humidity ratio [kg water/kg dry air]
  REAL(r64)             :: PrecoolHumRat            = 0.0d0   ! precool design humidity ratio [kg water/kg dry air]
  REAL(r64)             :: CoolSupTemp              = 0.0d0   ! cooling design supply air temperature [C]
  REAL(r64)             :: HeatSupTemp              = 0.0d0   ! heating design supply air temperature[C]
  REAL(r64)             :: CoolSupHumRat            = 0.0d0   ! cooling design supply air humidity ratio [kg water/kg dry air]
  REAL(r64)             :: HeatSupHumRat            = 0.0d0   ! heating design supply air humidity ratio [kg water/kg dry air]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get system design cooling air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per system, user input
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get system design heating air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per system, user input
  REAL(r64)             :: InpDesCoolAirFlow        = 0.0d0   ! input design system supply air flow rate [m3/s]
  REAL(r64)             :: InpDesHeatAirFlow        = 0.0d0   ! input design system heating supply air flow rate [m3/s]
  REAL(r64)             :: CoinCoolMassFlow         = 0.0d0   ! coincident peak cooling mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideCoinCoolMassFlowOn = .FALSE. ! If true, EMS to change coincident peak cooling mass flow rate
  REAL(r64)             :: EMSValueCoinCoolMassFlow   = 0.0D0 ! Value EMS wants for coincident peak cooling mass flow rate [kg/s]

  REAL(r64)             :: CoinHeatMassFlow         = 0.0d0   ! coincident peak heating mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideCoinHeatMassFlowOn = .FALSE. ! If true, EMS to set coincident peak heating mass flow rate
  REAL(r64)             :: EMSValueCoinHeatMassFlow   = 0.0D0 ! Value EMS wants for coincident peak heating mass flow rate [kg/s]

  REAL(r64)             :: NonCoinCoolMassFlow      = 0.0d0   ! noncoincident peak cooling mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideNonCoinCoolMassFlowOn = .FALSE. ! true, EMS to set noncoincident peak cooling mass flow rate
  REAL(r64)             :: EMSValueNonCoinCoolMassFlow   = 0.0D0 ! Value EMS for noncoincident peak cooling mass flow rate [kg/s]

  REAL(r64)             :: NonCoinHeatMassFlow      = 0.0d0   ! noncoincident peak heating mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideNonCoinHeatMassFlowOn = .FALSE. ! true, EMS to set noncoincident peak heating mass flow rate
  REAL(r64)             :: EMSValueNonCoinHeatMassFlow   = 0.0D0 ! Value EMS for noncoincident peak heating mass flow rate [kg/s]

  REAL(r64)             :: DesMainVolFlow           = 0.0d0   ! design main supply duct volume flow [m3/s]
  LOGICAL               :: EMSOverrideDesMainVolFlowOn = .FALSE. ! If true, EMS is acting to change DesMainVolFlow
  REAL(r64)             :: EMSValueDesMainVolFlow   = 0.0D0 ! Value EMS providing for design main supply duct volume flow [m3/s]

  REAL(r64)             :: DesHeatVolFlow           = 0.0d0   ! design heat supply duct volume flow [m3/s]
  LOGICAL               :: EMSOverrideDesHeatVolFlowOn = .FALSE. ! If true, EMS is acting to change DesCoolVolFlow
  REAL(r64)             :: EMSValueDesHeatVolFlow   = 0.0D0 ! Value EMS providing for design cool  supply duct volume flow [m3/s]

  REAL(r64)             :: DesCoolVolFlow           = 0.0d0   ! design cool  supply duct volume flow [m3/s]
  LOGICAL               :: EMSOverrideDesCoolVolFlowOn = .FALSE. ! If true, EMS is acting to change DesCoolVolFlow
  REAL(r64)             :: EMSValueDesCoolVolFlow   = 0.0D0 ! Value EMS providing for design cool  supply duct volume flow [m3/s]

  REAL(r64)             :: SensCoolCap              = 0.0d0   ! design sensible cooling capacity [W]
  REAL(r64)             :: HeatCap                  = 0.0d0   ! design heating capacity [W]
  REAL(r64)             :: PreheatCap               = 0.0d0   ! design preheat capacity [W]
  REAL(r64)             :: CoolMixTemp              = 0.0d0   ! design mixed air temperature for cooling [C]
  REAL(r64)             :: CoolMixHumRat            = 0.0d0   ! design mixed air hum ratio for cooling [kg water/kg dry air]
  REAL(r64)             :: CoolRetTemp              = 0.0d0   ! design return air temperature for cooling [C]
  REAL(r64)             :: CoolRetHumRat            = 0.0d0   ! design return air hum ratio for cooling [kg water/kg dry air]
  REAL(r64)             :: CoolOutTemp              = 0.0d0   ! design outside air temperature for cooling [C]
  REAL(r64)             :: CoolOutHumRat            = 0.0d0   ! design outside air hum ratio for cooling [kg water/kg dry air]
  REAL(r64)             :: HeatMixTemp              = 0.0d0   ! design mixed air temperature for heating [C]
  REAL(r64)             :: HeatMixHumRat            = 0.0d0   ! design mixed air hum ratio for heating [kg water/kg dry air]
  REAL(r64)             :: HeatRetTemp              = 0.0d0   ! design return air temperature for heating [C]
  REAL(r64)             :: HeatRetHumRat            = 0.0d0   ! design return air hum ratio for heating [kg water/kg dry air]
  REAL(r64)             :: HeatOutTemp              = 0.0d0   ! design outside air temperature for heating [C]
  REAL(r64)             :: HeatOutHumRat            = 0.0d0   ! design outside air hum ratio for Heating [kg water/kg dry air]
  REAL(r64)             :: DesCoolVolFlowMin        = 0.0d0   ! design minimum system cooling flow rate [m3/s]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatFlowSeq         ! daily sequence of system heating air mass flow rate
                                                               !  (zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: CoolFlowSeq         ! daily sequence of system cooling air mass flow rate
                                                               !  (zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SensCoolCapSeq      ! daily sequence of system sensible cooling capacity
                                                               !  (zone time step)
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HeatCapSeq          ! daily sequence of system heating capacity [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: PreHeatCapSeq       ! daily sequence of system preheat capacity [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysCoolRetTempSeq   ! daily sequence of system cooling return temperatures [C]
                                                               !  [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysCoolRetHumRatSeq ! daily sequence of system cooling return humidity ratios
                                                               !  [kg water/kg dry air] [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysHeatRetTempSeq   ! daily sequence of system heating return temperatures [C]
                                                               !   [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysHeatRetHumRatSeq ! daily sequence of system heating return humidity ratios
                                                               !  [kg water/kg dry air] [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysCoolOutTempSeq   ! daily sequence of system cooling outside temperatures [C]
                                                               !  [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysCoolOutHumRatSeq ! daily sequence of system cooling outside humidity ratios
                                                               !  [kg water/kg dry air] [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysHeatOutTempSeq   ! daily sequence of system heating outside temperatures [C]
                                                               !  [zone time step]
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: SysHeatOutHumRatSeq ! daily sequence of system heating outside humidity ratios
                                                               !   [kg water/kg dry air] [zone time step]
  INTEGER               :: SystemOAMethod           = 0        ! System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
  REAL(r64)             :: MaxZoneOAFraction        = 0.0d0    ! maximum value of min OA for zones served by system
  REAL(r64)             :: SysUncOA                 = 0.0d0    ! uncorrected system outdoor air flow based on zone people and
                                                               ! zone area
  LOGICAL               :: OAAutosized              = .FALSE.  ! Set to true if design OA vol flow is set to 'autosize'
                                                               ! in Sizing:System
END TYPE SystemSizingData

TYPE PlantSizingData
  CHARACTER &
    (len=MaxNameLength) :: PlantLoopName            = ' '     ! name of PLANT LOOP or CONDENSER LOOP object
  INTEGER               :: LoopType                 = 0       ! type of loop: 1=heating, 2=cooling, 3=condenser
  REAL(r64)             :: ExitTemp                 = 0.0d0   ! loop design exit (supply) temperature [C]
  REAL(r64)             :: DeltaT                   = 0.0d0   ! loop design temperature drop (or rise) [DelK]
  ! Calculated
  REAL(r64)             :: DesVolFlowRate           = 0.0d0   ! loop design flow rate in m3/s
  LOGICAL               :: VolFlowSizingDone        = .FALSE. ! flag to indicate when this loop has finished sizing flow rate
END TYPE PlantSizingData

TYPE DesDayWeathData
  CHARACTER(len=8)      :: DateString               = ' '     ! date of design day weather values
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: Temp               ! design day temperatures at the major time step
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: HumRat             ! design day humidity ratios at the major time step
  REAL(r64), ALLOCATABLE, DIMENSION(:)  :: Press              ! design day braometric pressure at the major time step
END TYPE DesDayWeathData

TYPE CompDesWaterFlowData                                     ! design water flow rate for components that use water as an
                                                              !  energy source or sink
  INTEGER               :: SupNode                  = 0       ! water inlet node number (condenser side for water / water)
  REAL(r64)             :: DesVolFlowRate           = 0.0d0   ! water design flow rate [m3/s]
END TYPE CompDesWaterFlowData

TYPE OARequirementsData
  CHARACTER(len=MaxNameLength) :: Name   = ' '
  INTEGER      :: OAFlowMethod           = 0        !- Method for OA flow calculation
                                                    !- (Flow/Person, Flow/Zone, Flow/Area, FlowACH, Sum, Maximum)
  REAL(r64)    :: OAFlowPerPerson        = 0.0D0    !- OA requirement per person
  REAL(r64)    :: OAFlowPerArea          = 0.0D0    !- OA requirement per zone area
  REAL(r64)    :: OAFlowPerZone          = 0.0D0    !- OA requirement per zone
  REAL(r64)    :: OAFlowACH              = 0.0D0    !- OA requirement per zone per hour
  INTEGER      :: OAFlowFracSchPtr       = 0        !- Fraction schedule applied to total OA requirement
  REAL(r64)    :: MaxOAFractionSchValue  = 0.0D0    !- Maximum value from OAFlow fraction schedule (used for sizing)
END TYPE OARequirementsData

TYPE ZoneAirDistributionData
  CHARACTER(len=MaxNameLength) :: Name       = ' '
  CHARACTER(len=MaxNameLength) :: ZoneADEffSchName = ' '!- Zone air distribution effectiveness schedule name
  REAL(r64)    :: ZoneADEffCooling           = 1.0D0    !- Zone air distribution effectiveness in cooling mode
  REAL(r64)    :: ZoneADEffHeating           = 1.0D0    !- Zone air distribution effectiveness in heating mode
  REAL(r64)    :: ZoneSecondaryRecirculation = 0.0D0    !- Zone air secondary recirculation ratio
  INTEGER      :: ZoneADEffSchPtr       = 0             !- Zone air distribution effectiveness schedule index
END TYPE ZoneAirDistributionData

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:

TYPE (OARequirementsData), ALLOCATABLE, DIMENSION(:) :: OARequirements

TYPE (ZoneAirDistributionData), ALLOCATABLE, DIMENSION(:) :: ZoneAirDistribution

TYPE (ZoneSizingInputData),   ALLOCATABLE, DIMENSION(:)   :: ZoneSizingInput      ! Input data for zone sizing
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:,:) :: ZoneSizing           ! Data for zone sizing (all data, all design
                                                                                  !  days; includes effects of user multiplier
                                                                                  !  and user set flows)
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:)   :: FinalZoneSizing      ! Final data for zone sizing including effects
                                                                                  !  of user input multiplier and flows
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:,:) :: CalcZoneSizing       ! Data for zone sizing (all data,
                                                                                  !  all design days, calculated only)
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:)   :: CalcFinalZoneSizing  ! Final data for zone sizing (calculated only)
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:)   :: TermUnitFinalZoneSizing   ! Final data for sizing terminal units
TYPE (SystemSizingInputData), ALLOCATABLE, DIMENSION(:)   :: SysSizInput          ! Input data array for system sizing object
TYPE (SystemSizingData),      ALLOCATABLE, DIMENSION(:,:) :: SysSizing            ! Data array for system sizing (all data)
TYPE (SystemSizingData),      ALLOCATABLE, DIMENSION(:)   :: FinalSysSizing       ! Data array for system sizing (max heat/cool)
                                                                                  !  using user input system flow rates.
TYPE (SystemSizingData),      ALLOCATABLE, DIMENSION(:)   :: CalcSysSizing        ! Data array for system sizing (max heat/cool)
                                                                                  !  before applying user input sys flow rates.
TYPE (TermUnitSizingData),    ALLOCATABLE, DIMENSION(:)   :: TermUnitSizing       ! Data added in sizing routines
TYPE (ZoneEqSizingData),      ALLOCATABLE, DIMENSION(:)   :: ZoneEqSizing         ! Data added in zone eq component sizing routines
TYPE (ZoneEqSizingData),      ALLOCATABLE, DIMENSION(:)   :: UnitarySysEqSizing   ! Data added in unitary system sizing routines
TYPE (ZoneEqSizingData),      ALLOCATABLE, DIMENSION(:)   :: OASysEqSizing        ! Data added in unitary system sizing routines
TYPE (PlantSizingData),       ALLOCATABLE, DIMENSION(:)   :: PlantSizData         ! Input data array for plant sizing
TYPE (DesDayWeathData),       ALLOCATABLE, DIMENSION(:)   :: DesDayWeath          ! design day weather saved at major time step
TYPE (CompDesWaterFlowData),  ALLOCATABLE, DIMENSION(:)   :: CompDesWaterFlow     ! array to store components' design water flow

INTEGER   :: NumOARequirements  = 0       ! Number of OA Requirements objects
INTEGER   :: NumZoneAirDistribution = 0   ! Number of zone air distribution objects
INTEGER   :: NumZoneSizingInput = 0       ! Number of Zone Sizing objects
INTEGER   :: NumSysSizInput     = 0       ! Number of System Sizing objects
INTEGER   :: NumPltSizInput     = 0       ! Number of Plant Sizing objects
INTEGER   :: CurSysNum          = 0       ! Current Air System index (0 if not in air loop)
INTEGER   :: CurOASysNum        = 0       ! Current outside air system index (0 if not in OA Sys)
INTEGER   :: CurZoneEqNum       = 0       ! Current Zone Equipment index (0 if not simulating ZoneEq)
INTEGER   :: CurBranchNum       = 0       ! Index of branch being simulated (or 0 if not air loop)
INTEGER   :: CurDuctType        = 0       ! Duct type of current branch
INTEGER   :: CurLoopNum         = 0       ! the current plant loop index
INTEGER   :: CurCondLoopNum     = 0       ! the current condenser loop number
INTEGER   :: CurEnvirNumSimDay  = 0
INTEGER   :: CurOverallSimDay   = 0
INTEGER   :: NumTimeStepsInAvg  = 0       ! number of time steps in the averaging window for the design flow and load sequences
INTEGER   :: SaveNumPlantComps      = 0       ! Number of components using water as an energy source or sink (e.g. water coils)
LOGICAL   :: TermUnitSingDuct   = .FALSE. ! TRUE if a non-induction single duct terminal unit
LOGICAL   :: TermUnitPIU        = .FALSE. ! TRUE if a powered induction terminal unit
LOGICAL   :: TermUnitIU         = .FALSE. ! TRUE if an unpowered induction terminal unit
LOGICAL   :: ZoneEqFanCoil      = .FALSE. ! TRUE if a 4 pipe fan coil unit is being simulated
LOGICAL   :: ZoneEqDXCoil       = .FALSE. ! TRUE if a ZoneHVAC DX coil is being simulated
LOGICAL   :: ZoneCoolingOnlyFan = .FALSE. ! TRUE if a ZoneHVAC DX cooling coil is only coil in parent
LOGICAL   :: ZoneHeatingOnlyFan = .FALSE. ! TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
LOGICAL   :: SysSizingRunDone   = .FALSE. ! True if a system sizing run is successfully completed.
LOGICAL   :: ZoneSizingRunDone  = .FALSE. ! True if a zone sizing run has been successfully completed.
REAL(r64) :: AutoVsHardSizingThreshold = 0.1d0 ! criteria threshold used to determine if user hard size and autosize disagree 10%
REAL(r64) :: AutoVsHardSizingDeltaTempThreshold = 1.5d0 ! temperature criteria threshold for autosize versus hard size [C]
REAL(r64) :: DXCoolCap          = 0.0d0   ! The ARI cooling capacity of a DX unit.
REAL(r64) :: UnitaryHeatCap     = 0.0d0   ! the heating capacity of a unitary system
REAL(r64) :: SuppHeatCap        = 0.0d0   ! the heating capacity of the supplemental heater in a unitary system
REAL(r64) :: GlobalHeatSizingFactor = 0.0d0   ! the global heating sizing ratio
REAL(r64) :: GlobalCoolSizingFactor = 0.0d0   ! the global cooling sizing ratio
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneSizThermSetPtHi ! highest zone thermostat setpoint during zone sizing calcs
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneSizThermSetPtLo ! lowest zone thermostat setpoint during zone sizing calcs
CHARACTER(len=15), ALLOCATABLE, DIMENSION(:) :: CoolPeakDateHrMin
CHARACTER(len=15), ALLOCATABLE, DIMENSION(:) :: HeatPeakDateHrMin
CHARACTER(len=1) :: SizingFileColSep=' '  ! Character to separate columns in sizing outputs


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

END MODULE DataSizing
