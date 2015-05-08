MODULE DataPhotovoltaics      ! Data-Only Module for native EnergyPlus Photovoltaics variables

          ! MODULE INFORMATION:
          !       AUTHOR         D. Bradley
          !       DATE WRITTEN   May 2003
          !       MODIFIED       B. Griffith, Dec. 2003, heavy changes, moved derived types here from Photovoltaics.f90
          !                      B. Griffith, Feb 2008, added BIPV and inverter to one-diode model
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for the variables that relate specifically
          ! to the native EnergyPlus photovoltaics simulation.

          ! METHODOLOGY EMPLOYED:

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
  CHARACTER(len=*), PARAMETER :: cPVGeneratorObjectName       = 'Generator:Photovoltaic'
  CHARACTER(len=*), PARAMETER :: cPVSimplePerfObjectName      = 'PhotovoltaicPerformance:Simple'
  CHARACTER(len=*), PARAMETER :: cPVEquiv1DiodePerfObjectName = 'PhotovoltaicPerformance:EquivalentOne-Diode'
  CHARACTER(len=*), PARAMETER :: cPVSandiaPerfObjectName      = 'PhotovoltaicPerformance:Sandia'

  INTEGER, PARAMETER :: iNotYetSetPVModel = 0
  INTEGER, PARAMETER :: iSimplePVModel = 1001
  INTEGER, PARAMETER :: iTRNSYSPVModel = 1002
  INTEGER, PARAMETER :: iSandiaPVModel = 1003

  INTEGER, PARAMETER :: iNotYetSetCellIntegration                = 0 ! cell temp method not set
  INTEGER, PARAMETER :: iDecoupledCellIntegration                = 1 ! cell temp method based on energy balance
  INTEGER, PARAMETER :: iDecoupledUllebergDynamicCellIntegration = 2 ! cell temp method based on energy bal with capacity
  INTEGER, PARAMETER :: iSurfaceOutsideFaceCellIntegration       = 3 ! cell temp method based on coupling to E+'s heat balance
  INTEGER, PARAMETER :: iTranspiredCollectorCellIntegration      = 4 ! cell temp method based on coupling to unglazed transpired co
  INTEGER, PARAMETER :: iExteriorVentedCavityCellIntegration     = 5 ! cell temp method based on coupling to nat vent exterior cavi
  INTEGER, PARAMETER :: iPVTSolarCollectorCellIntegration        = 6 ! cell temp method based on coupling to PVT model

  INTEGER, PARAMETER :: FixedEfficiency         = 10 ! simple PV, constant efficiency
  INTEGER, PARAMETER :: ScheduledEfficiency     = 11 ! simpel PV, scheduled efficiency

  INTEGER, PARAMETER :: CrystallineSiPVCells    = 1
  INTEGER, PARAMETER :: AmorphousSiPVCells      = 2

  REAL(r64),    PARAMETER :: MinIrradiance = 0.3d0  ![W/m2] Assume no operation if Ic is below this number (W/m2)
        ! DERIVED TYPE DEFINITIONS
  TYPE SimplePVParamsStruct
      CHARACTER(len=MaxNameLength) :: Name=' ' ! name as identified in Sandia database
      REAL(r64)                    :: AreaCol = 0.0D0 ! effective area of solar collection
      REAL(r64)                    :: ActiveFraction =0.0D0 ! fraction of parent surface that has active solar cells
      INTEGER                      :: EfficencyInputMode = 0 ! to schedule or not
      INTEGER                      :: EffSchedPtr    =0   ! index pointer for efficiency schedule
      REAL(r64)                    :: PVEfficiency   =0.0D0 ! fixed or current PV efficiency
  END TYPE SimplePVParamsStruct

  TYPE TRNSYSPVModuleParamsStruct    !  for  GENERATOR:PV:Equivalent One-Diode Model
    CHARACTER(len=MaxNameLength) :: Name=' '
    INTEGER   :: CellsInSeries        = 0    !cells in series [-]
    INTEGER   :: CellType             = 0    ! type of PV cell (crystalline, amorphous )
    REAL(r64) :: Area                 =0.0D0 !module area [m2]
    REAL(r64) :: TauAlpha             =0.0D0 !tau alpha product at normal incidence [-]
    REAL(r64) :: SemiConductorBandgap =0.0D0 !electron bandgap [eV]
    REAL(r64) :: ShuntResistance      =0.0D0 !shunt resistance [ohms]
    REAL(r64) :: RefIsc               =0.0D0 !short circuit current at reference conditions [A/K]
    REAL(r64) :: RefVoc               =0.0D0 !open circuit voltage at reference conditions [V/K]
    REAL(r64) :: RefTemperature       =0.0D0 !temperature at reference conditions
    REAL(r64) :: RefInsolation        =0.0D0 !radiation at reference conditions [W/m2]
    REAL(r64) :: Imp                  =0.0D0 !current at max power [A]
    REAL(r64) :: Vmp                  =0.0D0 !voltage at max power [V]
    REAL(r64) :: TempCoefIsc          =0.0D0 !temperature coefficient of short circuit current
    REAL(r64) :: TempCoefVoc          =0.0D0 !temperature coefficient of open circuit voltage
    REAL(r64) :: NOCTAmbTemp          =0.0D0 !ambient temperature at NOCT [C]
    REAL(r64) :: NOCTCellTemp         =0.0D0 !cell temperature at NOCT [C]
    REAL(r64) :: NOCTInsolation       =0.0D0 !radiation at NOCT [W/m2]
    REAL(r64) :: HeatLossCoef         =0.0D0 !heat loss coefficient [W/m2.K]
    REAL(r64) :: HeatCapacity         =0.0D0 !total heat capacity (only used in TC mode 1)
  END TYPE TRNSYSPVModuleParamsStruct

  TYPE TRNSYSPVCalcStruct
    REAL(r64) :: Insolation           =0.0D0 !radiation [W/m2]
    REAL(r64) :: ArrayCurrent         =0.0D0 !array current at current conditions [A]
    REAL(r64) :: ArrayVoltage         =0.0D0 !array voltage at current conditions [V]
    REAL(r64) :: ArrayPower           =0.0D0 !array power at current conditions [W]
    REAL(r64) :: ArrayEfficiency      =0.0D0 !array efficiency at current conditions [0..1]
    REAL(r64) :: CellTemp             =0.0D0 !array cell temperature at current conditions [C]
    REAL(r64) :: CellTempK            =0.0D0 !array cell temperature (for setting last cell temp) [K]
    REAL(r64) :: TimeElapsed          =0.0D0 !time previous update of last cell temp
    REAL(r64) :: LastCellTempK        =0.0D0 !array cell temperature at previous conditions [K]
    REAL(r64) :: ArrayIsc             =0.0D0 !array short circuit current at current conditions [A]
    REAL(r64) :: ArrayVoc             =0.0D0 !array open circuit voltage at current conditions [V]
  END TYPE TRNSYSPVCalcStruct

  TYPE SNLModuleParamsStuct  ! for PV MODULE:SANDIA PARAMETERS
    CHARACTER(len=MaxNameLength) :: name=' ' ! name as identified in Sandia database
    REAL(r64)    :: Acoll           =0.0D0 ! Active collector area (m2, single module)
    REAL(r64)    :: NcellSer        =0.0D0 ! Number of cells in series in a module's cell-string (unitless)
    REAL(r64)    :: NparSerCells    =0.0D0 ! Number of cell-strings in parallel in module (unitless)
    REAL(r64)    :: Isc0            =0.0D0 ! Short circuit current at reference conditions (Amps)
    REAL(r64)    :: Voc0            =0.0D0 ! Open circuit voltage at reference conditions (Volts)
    REAL(r64)    :: Imp0            =0.0D0 ! Max power point current at reference conditions (Amps)
    REAL(r64)    :: Vmp0            =0.0D0 ! Voltage at max power at reference conditions (Volts)
    REAL(r64)    :: aIsc            =0.0D0 ! Normalized temperature coefficient for Isc (Amps/degC) Isc temperature coeff
    REAL(r64)    :: aImp            =0.0D0 ! Normalized temperature coefficient for Imp (1/degC) Imp temperature coeff
    REAL(r64)    :: c_0             =0.0D0 ! Empirical coefficients relating Imp to Ee (unitless)
                                         !   coefficient relating Imp to irradiance
    REAL(r64)    :: c_1             =0.0D0 ! Empirical coefficients relating Imp to Ee (unitless)
                                         !   coefficient relating Voc to irradiance
    REAL(r64)    :: BVoc0           =0.0D0 ! Temperature coefficient for module open-circuit-voltage at reference conditions
                                         !   (Volts/degC)
    REAL(r64)    :: mBVoc           =0.0D0 ! Coefficient for irradiance dependence of open-circuit-voltage-temperature
                                         !  coefficient  (V/°C)
    REAL(r64)    :: BVmp0           =0.0D0 ! Temperature coefficient for module maximum-power-voltage at reference conditions
                                         !   (V/°C)
    REAL(r64)    :: mBVmp           =0.0D0 ! Cofficient for irradiance dependence of maximum-power-voltage-temperature
                                         !   coefficient (V/°C)
    REAL(r64)    :: DiodeFactor     =0.0D0 ! Empirically determined 'diode factor' for individual cells (unitless)
    REAL(r64)    :: c_2             =0.0D0 ! Empirical coefficients relating Vmp to Ee (unitless)
                                         !   (coefficient relating Vmp to irradiance)
    REAL(r64)    :: c_3             =0.0D0 ! Empirical coefficients relating Vmp to Ee (unitless)
                                         !   (coefficient relating Vmp to irradiance)
    REAL(r64)    :: a_0             =0.0D0 ! Empirical coefficients for f1(AMa) polynomial (unitless)
    REAL(r64)    :: a_1             =0.0D0 ! Empirical coefficients for f1(AMa) polynomial (unitless)
    REAL(r64)    :: a_2             =0.0D0 ! Empirical coefficients for f1(AMa) polynomial (unitless)
    REAL(r64)    :: a_3             =0.0D0 ! Empirical coefficients for f1(AMa) polynomial (unitless)
    REAL(r64)    :: a_4             =0.0D0 ! Empirical coefficients for f1(AMa) polynomial (unitless)
    REAL(r64)    :: b_0             =0.0D0 ! Empirical coefficients for f1(AOI) polynomial (unitless)
    REAL(r64)    :: b_1             =0.0D0 ! Empirical coefficients for f1(AOI) polynomial (unitless)
    REAL(r64)    :: b_2             =0.0D0 ! Empirical coefficients for f1(AOI) polynomial (unitless)
    REAL(r64)    :: b_3             =0.0D0 ! Empirical coefficients for f1(AOI) polynomial (unitless)
    REAL(r64)    :: b_4             =0.0D0 ! Empirical coefficients for f1(AOI) polynomial (unitless)
    REAL(r64)    :: b_5             =0.0D0 ! Empirical coefficients for f1(AOI) polynomial (unitless)
    REAL(r64)    :: DT0             =0.0D0 ! Temperature difference between Tc and Tm at Eo (°C),
                                         ! (This is d(Tc) in Sandia database)
    REAL(r64)    :: fd              =0.0D0 ! Fraction of diffuse irradiance used by module (unitless)
    REAL(r64)    :: a               =0.0D0 ! Empirical coefficient for module temp.at low wind,
                                         ! high solar irradiance (unitless)
    REAL(r64)    :: b               =0.0D0 ! Empirical coefficient relating module temp.
                                         ! decrease with increasing wind speed (unitless)
    REAL(r64)    :: c_4             =0.0D0 ! Empirical coefficients relating Ix to Ee (unitless)
    REAL(r64)    :: c_5             =0.0D0 ! Empirical coefficients relating Ix to Ee (unitless)
    REAL(r64)    :: Ix0             =0.0D0 ! Current at V = 0.5 Voc and at reference conditions (Amps)
    REAL(r64)    :: Ixx0            =0.0D0 ! Current at V = 0.5 (Vmp + Voc) and at reference conditions (Amps)
    REAL(r64)    :: c_6             =0.0D0 ! Empirical coefficients relating Ixx to Ee (unitless)
    REAL(r64)    :: c_7             =0.0D0 ! Empirical coefficients relating Ixx to Ee (unitless)
  END TYPE SNLModuleParamsStuct

  TYPE SNLPVInputStruct         ! for data obtained elsewhere in EnergyPlus
    REAL(r64)    :: IcBeam         =0.0D0 !incident beam solar (W/m2)
    REAL(r64)    :: IcDiffuse      =0.0D0 ! incident diffuse solar (W/m2)
    REAL(r64)    :: IncidenceAngle =0.0D0 ! angle from normal for beam (deg)
    REAL(r64)    :: ZenithAngle    =0.0D0 !solar zenith angle (deg)
    REAL(r64)    :: Tamb           =0.0D0 ! outdoor drybulb temperature (C)
    REAL(r64)    :: WindSpeed      =0.0D0 ! outdoor windspeed. (m/s)
    REAL(r64)    :: Altitude       =0.0D0 ! elevation above sea level. (m)
  END TYPE SNLPVInputStruct

  TYPE SNLPVCalcStruct ! hold calculated results from PV modeling.
    REAL(r64) :: Vmp              =0.0D0 !(Volts) maximum power voltage
    REAL(r64) :: Imp              =0.0D0 !(Amps) maximum power current
    REAL(r64) :: Pmp              =0.0D0 !(W) (was kJ/hr) maximum power point power
    REAL(r64) :: EffMax           =0.0D0 !(unitless) conversion efficiency at max power point
    REAL(r64) :: Isc              =0.0D0 !(Amps) short circuit current
    REAL(r64) :: Voc              =0.0D0 !(Volts) open circuit voltage
    REAL(r64) :: Tcell            =0.0D0 !(deg C) solar cell operating temperature
    REAL(r64) :: Tback            =0.0D0 !(deg C) solar module operation temp, at back of module
    REAL(r64) :: AMa              =0.0D0 !(unitless) Absolute Air mass
    REAL(r64) :: F1               =0.0D0 !(unitless) holds result of "AMa-Function" for solar spectrum influence
    REAL(r64) :: F2               =0.0D0 !(unitless) holds result of AOI-Function for angle-of-incidence
    REAL(r64) :: Ix               =0.0D0 !(Amps) Current at V = 0.5 Voc
    REAL(r64) :: Vx               =0.0D0 !(Volts) Voltage at 0.5 Voc
    REAL(r64) :: Ixx              =0.0D0 !(Amps) current at V = 0.5(Vmpp + Voc)
    REAL(r64) :: Vxx              =0.0D0 !(Volts) voltage at 0.5(Vmpp + Voc)
    REAL(r64) :: SurfaceSink      =0.0D0 ! (Watts) energy balance term to account for electricity leaving
  END TYPE SNLPVCalcStruct

  TYPE PVReportVariables              !   for  GENERATOR:PV:EQUIVALENT ONE-DIODE MODEL
    REAL(r64) :: DCPower               =0.0D0 ! Direct Current power from PV array
    REAL(r64) :: DCEnergy              =0.0D0 ! Direct Current energy from PV array
    REAL(r64) :: ArrayEfficiency       =0.0D0 !array efficiency at current conditions [0..1]
    REAL(r64) :: CellTemp              =0.0D0 !array cell temperature at current conditions [C]
    REAL(r64) :: ArrayIsc              =0.0D0 !array short circuit current at current conditions [A]
    REAL(r64) :: ArrayVoc              =0.0D0 !array open circuit voltage at current conditions [V]
    REAL(r64) :: ArrayCurrent          =0.0D0
    REAL(r64) :: ArrayVoltage          =0.0D0
  END TYPE PVReportVariables

  TYPE PVArrayStruct
    CHARACTER(len=MaxNameLength) :: Name = ' '
    CHARACTER(len=MaxNameLength) :: SurfaceName = ' ' ! named surface in heat balance domain
    CHARACTER(len=MaxNameLength) :: PerfObjName = ' '
    INTEGER   :: SurfacePtr  = 0 ! index for named surface
    INTEGER   :: PVModelType = 0 ! type of performance modeling, Simple, TRNSYS or Equivalent 1-diode, or Sandia/King model
    INTEGER   :: CellIntegrationMode = 0     ! how are PV cells integrated with other E+ modeling
    REAL(r64) :: NumModNSeries       = 1.0D0 ! number of modules in series in one string
    REAL(r64) :: NumSeriesNParall    = 1.0D0 ! number of series strings in parallel

    INTEGER   :: UTSCPtr       = 0 ! pointer to UTSC number for INTEGRATED TRANSPIRED COLLECTOR mode
    INTEGER   :: ExtVentCavPtr = 0 ! pointer to Exterior Vented Cavity EXTERIOR VENTED CAVITY
    INTEGER   :: PVTPtr        = 0 ! pointer to PVT model
    REAL(r64) :: SurfaceSink   = 0.0D0 ! PV power "sink" for integration
    TYPE(PVReportVariables)           :: Report  ! report variables
    ! nested structs for user input parameters
    TYPE(SimplePVParamsStruct)        :: SimplePVModule ! simple model input params
    TYPE(TRNSYSPVModuleParamsStruct)  :: TRNSYSPVModule ! equivalent one-diode input params
    TYPE(SNLModuleParamsStuct)        :: SNLPVModule ! Sandia/King model input parameter data

    !nested structs for model input from elsewhere and calculations
    TYPE(TRNSYSPVCalcStruct)          :: TRNSYSPVcalc
    TYPE(SNLPVInputStruct)            :: SNLPVinto    ! model input from elsewhere in EnergyPlus
    TYPE(SNLPVCalcStruct)             :: SNLPVCalc      ! calc'd data for GENERATOR:PV:Sandia model

  END TYPE PVArrayStruct
        ! INTERFACE BLOCK SPECIFICATIONS
        ! na

        ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumPVs        =0 ! count of number of PV generators
  INTEGER :: Num1DiodePVModuleTypes  = 0 ! count for Equivalent one-diode model
  INTEGER :: NumSimplePVModuleTypes  =0 ! count of number of input objs for simple model
  INTEGER :: NumSNLPVModuleTypes     =0 ! count of number of input objs for Sandia model

  TYPE (PVArrayStruct  ), ALLOCATABLE, DIMENSION(:) :: PVarray

  REAL(r64)    :: ShuntResistance=0.0D0  ! old "RSH" in common block of trnsys code

! ___________________________________________________________________________

!     EnergyPlus V1.2 and beyond include models for photovoltaic calculations called
!     Generator:Photovoltaic:Simple and Generator:PV:Sandia implemented by the Center for
!     Buildings and Thermal Systems, National Renewable Energy Laboratory, 1617 Cole Blvd
!     MS 2722, Golden, CO, 80401
!
!
!     EnergyPlus v1.1.1 and beyond includes model for Photovoltaic calculations, now
!     referred to as the Generator:PV:Equivalent One-Diode model developed by Thermal Energy
!     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719;
!     Tel: (608) 274-2577

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
END MODULE DataPhotovoltaics
